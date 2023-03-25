;;; bench.lisp --- measure performance of JSON libraries

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

(defpackage #:de.ralph-schleicher.json-benchmark
  (:nicknames :rs-json-bench)
  (:use :common-lisp :iterate)
  (:export
   #:pass1
   #:citm_catalog
   #:large))

(in-package :rs-json-bench)

(defparameter *libraries*
  '(:cl-json
    #-clisp
    :jonathan
    :json-streams
    :jsown
    #-clozure
    :jzon
    :rs-json
    :shasht
    :st-json
    :yason)
  "List of JSON libraries.")

(define-symbol-macro dev-null (uiop:null-device-pathname))
(define-symbol-macro utf-8 (uiop:encoding-external-format :utf-8))

(defgeneric %read (library source))
(defgeneric %write (library data))

(progn
  (defmethod %read ((library (eql :cl-json)) (source string))
    (let ((*read-default-float-format* 'double-float))
      (json:decode-json-from-string source)))

  (defmethod %read ((library (eql :cl-json)) (source stream))
    (let ((*read-default-float-format* 'double-float))
      (json:decode-json source)))

  (defmethod %read ((library (eql :cl-json)) (source pathname))
    (with-open-file (stream source :external-format utf-8)
      (%read library stream)))

  (defmethod %write ((library (eql :cl-json)) data)
    (json:encode-json data *standard-output*))
  ())

#-clisp
(progn
  (defmethod %read ((library (eql :jonathan)) (source string))
    (jonathan:parse source))

  (defmethod %read ((library (eql :jonathan)) (source pathname))
    (%read library (alexandria:read-file-into-string source :external-format utf-8)))

  (defmethod %write ((library (eql :jonathan)) data)
    (jonathan:to-json data))
  ())

(progn
  (defun %read-json-streams (source)
    (json-streams:json-parse source :duplicate-key-check nil))

  (defmethod %read ((library (eql :json-streams)) (source string))
    (%read-json-streams source))

  (defmethod %read ((library (eql :json-streams)) (source stream))
    (%read-json-streams source))

  (defmethod %read ((library (eql :json-streams)) (source pathname))
    (with-open-file (stream source :external-format utf-8)
      (%read-json-streams stream)))

  (defmethod %write ((library (eql :json-streams)) data)
    (json-streams:json-stringify data))
  ())

(progn
  (defmethod %read ((library (eql :jsown)) (source string))
    (jsown:parse source))

  (defmethod %read ((library (eql :jsown)) (source pathname))
    (%read library (alexandria:read-file-into-string source :external-format utf-8)))

  (defmethod %write ((library (eql :jsown)) data)
    (jsown:to-json data))
  ())

#-clozure
(progn
  (defun %read-jzon (source)
    (com.inuoe.jzon:parse source :max-depth 1000))

  (defmethod %read ((library (eql :jzon)) (source string))
    (%read-jzon source))

  (defmethod %read ((library (eql :jzon)) (source stream))
    (%read-jzon source))

  (defmethod %read ((library (eql :jzon)) (source pathname))
    (with-open-file (stream source :external-format utf-8)
      (%read-jzon stream)))

  (defmethod %write ((library (eql :jzon)) data)
    (com.inuoe.jzon:stringify data :stream *standard-output*))
  ())

(progn
  (defun %read-rs-json (source)
    (let ((rs-json:*allow-duplicate-object-keys* t))
      (rs-json:parse source)))

  (defmethod %read ((library (eql :rs-json)) (source string))
    (%read-rs-json source))

  (defmethod %read ((library (eql :rs-json)) (source stream))
    (%read-rs-json source))

  (defmethod %read ((library (eql :rs-json)) (source pathname))
    (%read-rs-json source))

  (defmethod %write ((library (eql :rs-json)) data)
    (rs-json:serialize *standard-output* data))
  ())

(progn
  (defun %read-shasht (source)
    (let ((*read-default-float-format* 'double-float))
      (shasht:read-json source)))

  (defmethod %read ((library (eql :shasht)) (source string))
    (%read-shasht source))

  (defmethod %read ((library (eql :shasht)) (source stream))
    (%read-shasht source))

  (defmethod %read ((library (eql :shasht)) (source pathname))
    (with-open-file (stream source :external-format utf-8)
      (%read-shasht stream)))

  (defmethod %write ((library (eql :shasht)) data)
    (shasht:write-json data *standard-output*))
  ())

(progn
  (defun %read-st-json (source)
    (let ((*read-default-float-format* 'double-float))
      (st-json:read-json source)))

  (defmethod %read ((library (eql :st-json)) (source string))
    (%read-st-json source))

  (defmethod %read ((library (eql :st-json)) (source stream))
    (%read-st-json source))

  (defmethod %read ((library (eql :st-json)) (source pathname))
    (with-open-file (stream source :external-format utf-8)
      (%read-shasht stream)))

  (defmethod %write ((library (eql :st-json)) data)
    (st-json:write-json data *standard-output*))
  ())

(progn
  (defun %read-yason (source)
    (yason:parse source))

  (defmethod %read ((library (eql :yason)) (source string))
    (%read-yason source))

  (defmethod %read ((library (eql :yason)) (source stream))
    (%read-yason source))

  (defmethod %read ((library (eql :yason)) (source pathname))
    (with-open-file (stream source :external-format utf-8)
      (%read-yason stream)))

  (defmethod %write ((library (eql :yason)) data)
    (yason:encode data *standard-output*))
  ())

(defun bench (pathname &key (libraries *libraries*) (repeat-count 1) stream dump)
  (let ((file-name (file-namestring pathname))
	(source (unless stream (alexandria:read-file-into-string pathname))))
    (iter (for lib :in libraries)
	  (terpri *trace-output*)
	  (format *trace-output* "========================================~%")
	  (format *trace-output* "~A~%" lib)
	  (format *trace-output* "========================================~%")
	  (trivial-garbage:gc)
	  (handler-case
	      (if (not stream)
		  (let ((data (%read lib source)))
		    (when dump
		      (format *trace-output* "~:W~%" data))
		    (iter (repeat repeat-count)
			  (format *trace-output* "~A;READ;~A~%" lib file-name)
			  (trivial-garbage:gc)
			  (time (%read lib source)))
		    (when dump
		      (let ((*standard-output* *trace-output*))
			(let ((tem (%write lib data)))
			  (if (stringp tem)
			      (format *trace-output* "~A~%" tem)
			    (terpri *trace-output*)))))
		    (with-open-file (stream dev-null :direction :output :if-exists :overwrite :external-format utf-8)
		      (let ((*standard-output* stream))
			(iter (repeat repeat-count)
			      (format *trace-output* "~A;WRITE;~A~%" lib file-name)
			      (trivial-garbage:gc)
		      	      (time (%write lib data))))))
		;; Stream I/O.
		(progn
		  (iter (repeat repeat-count)
			(let ((data (with-open-file (stream pathname :external-format utf-8)
				      (format *trace-output* "~A;READ;~A~%" lib file-name)
				      (trivial-garbage:gc)
				      (time (%read lib stream)))))
			  (with-open-file (stream dev-null :direction :output :if-exists :overwrite :external-format utf-8)
			    (let ((*standard-output* stream))
			      (format *trace-output* "~A;WRITE;~A~%" lib file-name)
			      (trivial-garbage:gc)
		      	      (time (%write lib data))))))))
	    (error (condition)
	      (format *trace-output*
		      "~A: error (~A)~%~A~&"
		      (namestring pathname) (type-of condition) condition)))
	  ())))

;;; Program entry points.

(defun pass1 ()
  (let ((pathname (merge-pathnames
		   #P"t/json-checker/pass1.json"
		   (asdf:system-source-directory "rs-json"))))
    (bench pathname :repeat-count 1 :dump t)))

(defun citm_catalog ()
  (let ((pathname (merge-pathnames
		   #P"t/data/citm_catalog.json"
		   (asdf:system-source-directory "rs-json"))))
    (bench pathname :repeat-count 9)))

(defun large ()
  (let ((pathname (merge-pathnames
		   #P"t/large.json"
		   (asdf:system-source-directory "rs-json"))))
    (bench pathname :repeat-count 1 :stream t)))

;;; bench.lisp ends here