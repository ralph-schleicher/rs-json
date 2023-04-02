;;; tests.lisp --- test procedure

;; Copyright (C) 2023 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage #:de.ralph-schleicher.json-tests
  (:nicknames :rs-json-tests)
  (:use :common-lisp :iterate :lisp-unit :rs-json)
  (:export
   #:main))

(in-package :rs-json-tests)

(defparameter *test-directory* (merge-pathnames
				(make-pathname :directory '(:relative "t"))
				(asdf:system-source-directory "rs-json")))

(defvar oref (gensym)
  "A unique symbol.")

(defun oref (object key)
  "Return the value of an object member, or ‘nil’ if it does not exist.
Secondary value is true if the object member exists."
  (cond ((hash-table-p object)
	 (gethash key object))
	((null object)
	 (values nil nil))
	((consp (first object)) ;an alist
	 (let ((cell (assoc key object :test #'equal)))
	   (values (cdr cell) (not (null cell)))))
	(t
	 (let ((value (getf object key oref)))
	   (if (eq value oref)
	       (values nil nil)
	     (values value t))))))

(defun okey (name)
  "Return the object key symbol for the string NAME."
  (intern name :rs-json-tests))

(defun rtt (string)
  "Do a round-trip test."
  (assert-equality #'string= string (serialize nil (parse string)) string))

(define-test objects
  ;; Check ‘*object-as*’ and ‘*object-key-decoder*’.
  (let ((source "{\"foo\" : 42, \"bar\" : \"baz\"}"))
    (let* ((*object-as* :hash-table)
	   (data (parse source)))
      (assert-true (hash-table-p data) data)
      (assert-equal 2 (hash-table-count data) data)
      (assert-equal 42 (oref data "foo") data)
      (assert-equal "baz" (oref data "bar") data)
      (assert-equal nil (oref data "hack") data))
    (let* ((*object-as* :alist)
	   (data (parse source)))
      (assert-true (consp (first data)) data)
      (assert-equal 2 (length data) data)
      (assert-equal 42 (oref data "foo") data)
      (assert-equal "baz" (oref data "bar") data)
      (assert-equal nil (oref data "hack") data))
    (let* ((*object-as* :plist)
	   (*object-key-decoder* #'okey)
	   (*encode-symbol-hook* :preserve)
	   (data (parse source)))
      (assert-true (symbolp (first data)) data)
      (assert-equal 4 (length data) data)
      (assert-equal 42 (oref data '|foo|) data)
      (assert-equal "baz" (oref data '|bar|) data)
      (assert-equal nil (oref data '|hack|) data))
    ()))

(define-test arrays
  ;; Check ‘*array-as*’.
  (let ((source "[42, \"baz\"]"))
    (let* ((*array-as* :vector)
	   (data (parse source)))
      (assert-true (vectorp data) data)
      (assert-equal 2 (length data) data)
      (assert-equal 42 (aref data 0) data)
      (assert-equal "baz" (aref data 1) data))
    (let* ((*array-as* :list)
	   (data (parse source)))
      (assert-true (listp data) data)
      (assert-equal 2 (length data) data)
      (assert-equal 42 (nth 0 data) data)
      (assert-equal "baz" (nth 1 data) data))
    ()))

(define-test literals
  ;; Decoding.
  (assert-eq :true (parse "true"))
  (assert-eq :false (parse "false"))
  (assert-eq :null (parse "null"))
  ;; Encoding.
  (assert-equal "true" (serialize nil :true))
  (assert-equal "false" (serialize nil :false))
  (assert-equal "null" (serialize nil :null))
  ;; Encoding of ‘t’ and ‘nil’.
  (assert-equal "true" (serialize nil t))
  (let ((*nil-encoder* #'encode-false))
    (assert-equal "false" (serialize nil nil)))
  (let ((*nil-encoder* #'encode-null))
    (assert-equal "null" (serialize nil nil)))
  ;; Check ‘*true*’, ‘*false*’, and ‘*null*’.
  (iter (for value :in (list :true 'true t))
	(let ((*true* value))
	  (assert-eq value (parse "true") value)
	  (assert-equal "true" (serialize nil value) value)))
  (iter (for value :in (list :false 'false nil))
	(let ((*nil-encoder* #'encode-false)
	      (*false* value))
	  (assert-eq value (parse "false") value)
	  (assert-equal "false" (serialize nil value) value)))
  (iter (for value :in (list :null 'null nil))
	(let ((*nil-encoder* #'encode-null)
	      (*null* value))
	  (assert-eq value (parse "null") value)
	  (assert-equal "null" (serialize nil value) value)))
  ())

(define-test duplicate-object-keys
  ;; Check ‘*allow-duplicate-object-keys*’.
  (let ((source "{\"a\" : 1, \"a\" : 2, \"a\" : 3}"))
    (let ((*object-as* :hash-table))
      (let* ((*allow-duplicate-object-keys* t)
	     (data (parse source)))
	(assert-equal 1 (hash-table-count data) data)
	(assert-equal 3 (oref data "a") data))
      (let* ((*allow-duplicate-object-keys* :ignore)
	     (data (parse source)))
	(assert-equal 1 (hash-table-count data) data)
	(assert-equal 1 (oref data "a") data))
      (let ((*allow-duplicate-object-keys* :append))
	(assert-error 'program-error (parse source) source))
      (let ((*allow-duplicate-object-keys* nil))
	(assert-error 'syntax-error (parse source) source)))
    (let ((*object-as* :alist))
      (let* ((*allow-duplicate-object-keys* t)
	     (data (parse source)))
	(assert-equal '(("a" . 3)) data data))
      (let* ((*allow-duplicate-object-keys* :ignore)
	     (data (parse source)))
	(assert-equal '(("a" . 1)) data data))
      (let* ((*allow-duplicate-object-keys* :append)
	     (data (parse source)))
	(assert-equal '(("a" . 1) ("a" . 2) ("a" . 3)) data data))
      (let ((*allow-duplicate-object-keys* nil))
	(assert-error 'syntax-error (parse source) source)))
    (let ((*object-as* :plist))
      (let* ((*allow-duplicate-object-keys* t)
	     (data (parse source)))
	(assert-equal '("a" 3) data data))
      (let* ((*allow-duplicate-object-keys* :ignore)
	     (data (parse source)))
	(assert-equal '("a" 1) data data))
      (let* ((*allow-duplicate-object-keys* :append)
	     (data (parse source)))
	(assert-equal '("a" 1 "a" 2 "a" 3) data data))
      (let ((*allow-duplicate-object-keys* nil))
	(assert-error 'syntax-error (parse source) source)))
    ()))

(define-test round-trip
  ;; Empty compound structures.
  (iter (for elem :in '(:hash-table :alist :plist))
	(let ((*list-encoder* #'encode-object)
	      (*object-as* elem))
	  (rtt "{}")))
  (iter (for elem :in '(:vector :list))
	(let ((*list-encoder* #'encode-array)
	      (*array-as* elem))
	  (rtt "[]")))
  ;; Literals.
  (iter (for elem :in '(:true true t))
	(let ((*true* elem))
	  (rtt "true")))
  (iter (for elem :in '(:false false nil))
	(let ((*nil-encoder* #'encode-false)
	      (*false* elem))
	  (rtt "false")))
  (iter (for elem :in '(:null null nil))
	(let ((*nil-encoder* #'encode-null)
	      (*null* elem))
	  (rtt "null")))
  ())

(defun save-parse (source &rest options &key &allow-other-keys)
  (let ((status :fail) data)
    (ignore-errors
     (setf data (apply #'parse source options)
	   status :pass))
    (values status data)))

(define-test json-checker
  (let ((input-directory (merge-pathnames
			  (make-pathname :directory '(:relative "json-checker"))
			  *test-directory*)))
    (labels ((parse (file-name)
	       (save-parse (merge-pathnames file-name input-directory))))
      (with-default-values ()
	(assert-eq :pass (parse "pass1.json"))
	(assert-eq :pass (parse "pass2.json"))
	(assert-eq :pass (parse "pass3.json"))
	(assert-eq :pass (parse "fail1.json")) ;to be documented
	(assert-eq :fail (parse "fail2.json"))
	(assert-eq :fail (parse "fail3.json"))
	(assert-eq :fail (parse "fail4.json"))
	(assert-eq :fail (parse "fail5.json"))
	(assert-eq :fail (parse "fail6.json"))
	(assert-eq :fail (parse "fail7.json"))
	(assert-eq :fail (parse "fail8.json"))
	(assert-eq :fail (parse "fail9.json"))
	(assert-eq :fail (parse "fail10.json"))
	(assert-eq :fail (parse "fail11.json"))
	(assert-eq :fail (parse "fail12.json"))
	(assert-eq :fail (parse "fail13.json"))
	(assert-eq :fail (parse "fail14.json"))
	(assert-eq :fail (parse "fail15.json"))
	(assert-eq :fail (parse "fail16.json"))
	(assert-eq :fail (parse "fail17.json"))
	(let ((*maximum-nesting-depth* 19))
	  (assert-eq :fail (parse "fail18.json")))
	(let ((*maximum-nesting-depth* 20))
	  (assert-eq :pass (parse "fail18.json")))
	(assert-eq :fail (parse "fail19.json"))
	(assert-eq :fail (parse "fail20.json"))
	(assert-eq :fail (parse "fail21.json"))
	(assert-eq :fail (parse "fail22.json"))
	(assert-eq :fail (parse "fail23.json"))
	(assert-eq :fail (parse "fail24.json"))
	(assert-eq :fail (parse "fail25.json"))
	(assert-eq :fail (parse "fail26.json"))
	(assert-eq :fail (parse "fail27.json"))
	(assert-eq :fail (parse "fail28.json"))
	(assert-eq :fail (parse "fail29.json"))
	(assert-eq :fail (parse "fail30.json"))
	(assert-eq :fail (parse "fail31.json"))
	(assert-eq :fail (parse "fail32.json"))
	(assert-eq :fail (parse "fail33.json"))
	()))))

(define-test json-test-suite
  (let ((input-directory (merge-pathnames
			  (make-pathname :directory '(:relative "json-test-suite" "test_parsing"))
			  *test-directory*)))
    (labels ((parse (file-name)
	       (save-parse (merge-pathnames file-name input-directory))))
      (with-default-values ()
	(setf *allow-duplicate-object-keys* :append)
	(iter (for pathname :in (uiop:directory-files input-directory))
	      (for file-name = (file-namestring pathname))
	      (ecase (char file-name 0)
		(#\y
		 (assert-eq :pass (parse file-name) file-name))
		(#\n
		 (assert-eq :fail (parse file-name) file-name))
		(#\i
		 (parse file-name)
		 (assert-true t)))))))
  ())

(defun main (&optional (tests :all))
  (let ((lisp-unit:*print-errors* t)
	(lisp-unit:*print-failures* t)
	(lisp-unit:*print-summary* t))
    (run-tests tests :rs-json-tests)))

;; tests.lisp ends here
