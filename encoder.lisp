;;; encoder.lisp --- JSON encoder

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

(in-package :rs-json)

(defun encoding-error (&optional (datum nil datum-supplied-p) &rest arguments)
  "Signal an encoding error."
  (cond ((stringp datum)
	 (error 'encoding-error
		:stream *standard-output*
		:format-control datum
		:format-arguments arguments))
	(datum-supplied-p
	 (apply #'error (or datum 'encoding-error)
		:stream *standard-output*
		arguments))
	(t
	 (error 'encoding-error
		:stream *standard-output*))))

(defun serialize (destination object)
  "Print a Lisp data structure as a JSON value.

First argument DESTINATION is the output object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal to
 ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument OBJECT is the Lisp object to be serialized.

The actual serialization of a Lisp object as a JSON value is performed
by the ‘encode’ methods (which see).

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output.

Exceptional situations:

   * Signals an ‘encoding-error’ if a Lisp object can not be
     encoded as a JSON value.

   * May signal an ‘arithmetic-error’ if a rational number is
     converted to a floating-point number."
  (flet ((%print (stream)
	   (let ((*standard-output* stream))
	     (encode object))))
    (etypecase destination
      (stream
       (%print destination) nil)
      (string
       (with-output-to-string (stream destination)
	 (%print stream) nil))
      (pathname
       (with-open-file (stream destination :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (%print stream) nil))
      ((member t)
       (%print *standard-output*) nil)
      (null
       (with-output-to-string (stream)
	 (%print stream))))))

(defsubst output (object)
  (etypecase object
    (character
     (write-char object))
    (string
     (write-string object))
    (null)))

(defmacro with-delimiters (start end &body forms)
  "Print the start delimiter START, evaluate FORMS,
then print the end delimiter END."
  `(progn
     (output ,start)
     ,@forms
     (output ,end)
     ()))

(defgeneric encode (object)
  (:documentation "Encode a Lisp object as a JSON value.

Argument OBJECT is the Lisp object to be serialized.

The JSON output is written to the ‘*standard-output*’ stream.
The return value of an ‘encode’ method is ignored."))

(defmethod encode (object)
  "The default encoding method.
Signals an ‘encoding-error’."
  (encoding-error "The type of object is ‘~S’." (type-of object)))

;;; Objects

(defun object-member (key value firstp)
  "Encode a JSON object member."
  (when (not firstp)
    (write-string ", "))
  (encode key)
  (write-string " : ")
  (encode value))

(defmacro with-object (&body body)
  "Encode a JSON object.
The BODY calls ‘(object-member KEY VALUE)’ to print an object member."
  (alexandria:once-only ((firstp t))
    `(flet ((object-member (key value)
	      (object-member key value ,firstp)
	      (setf ,firstp nil)))
       (with-delimiters #\{ #\}
	 ,@body))))

(defun object-from-hash-table (hash-table)
  "Encode hash table HASH-TABLE as a JSON object."
  (declare (type hash-table hash-table))
  (with-object
    (maphash #'object-member hash-table)))

(defun object-from-alist (alist)
  "Encode associated list ALIST as a JSON object."
  (declare (type list alist))
  (with-object
    (iter (for (key . value) :in alist)
	  (object-member key value))))

(defun object-from-plist (plist)
  "Encode property list PLIST as a JSON object."
  (declare (type list plist))
  (with-object
    (iter (for (key value) :on plist :by #'cddr)
	  (object-member key value))))

(defun encode-object (object)
  "Encode a Lisp object as a JSON object.

Argument OBJECT is the Lisp object to be serialized as a JSON object.
 Value has to be a hash table, an associated list, or a property list.
If OBJECT is a list, the value of the ‘*object-as*’ special variable
 affects the interpretation of OBJECT.

Mostly useful for binding the ‘*list-encoder*’ special variable."
  (check-type object (or list hash-table))
  (etypecase object
    (null
     (with-object))
    (list
     (case *object-as*
       (:alist
	(object-from-alist object))
       (:plist
	(object-from-plist object))
       (t
	;; Use some heuristics.
	(if (consp (first object))
	    (object-from-alist object)
	  (object-from-plist object)))))
    (hash-table
     (object-from-hash-table object))))

(defmethod encode ((object hash-table))
  "Encode a hash table as a JSON object."
  (object-from-hash-table object))

;;; Arrays

(defun array-element (value firstp)
  "Encode a JSON array element."
  (when (not firstp)
    (write-string ", "))
  (encode value))

(defmacro with-array (&body body)
  "Encode a JSON array.
The BODY calls ‘(array-element VALUE)’ to print an array element."
  (alexandria:once-only ((firstp t))
    `(flet ((array-element (value)
	      (array-element value ,firstp)
	      (setf ,firstp nil)))
       (with-delimiters #\[ #\]
	 ,@body))))

(defun array-from-sequence (sequence)
  "Encode sequence SEQUENCE as a JSON array."
  (declare (type sequence sequence))
  (with-array
    (map nil #'array-element sequence)))

(defun encode-array (object)
  "Encode a Lisp object as a JSON array.

Argument OBJECT is the Lisp object to be serialized as a JSON array.
 Value has to be a sequence, i.e. a vector or a list.

Mostly useful for binding ‘*list-encoder*’."
  (check-type object sequence)
  (array-from-sequence object))

(defmethod encode ((object vector))
  "Encode a vector as a JSON array."
  (array-from-sequence object))

;;; Strings

(defun string-char (char)
  "Encode the character CHAR as part of a JSON string."
  (declare (type character char))
  (cond ((char= char #\Backspace)
	 (write-string "\\b"))
	((char= char #\Page)
	 (write-string "\\f"))
	((or (char= char #\Linefeed)
	     (char= char #\Newline))
	 (write-string "\\n"))
	((char= char #\Return)
	 (write-string "\\r"))
	((char= char #\Tab)
	 (write-string "\\t"))
	((or (char= char #\")
	     (char= char #\\)
	     ;; TODO: Consider adding an option to allow non-escaped
	     ;; slash characters.
	     (char= char #\/))
	 (write-char #\\)
	 (write-char char))
	((standard-char-p char)
	 (write-char char))
	((let ((code (char-code char)))
	   (cond ((<= code #xFFFF)
		  (format t "\\u~4,'0X" code))
		 ((<= code #x10FFFF)
		  (multiple-value-bind (high low)
		      (truncate (- code #x10000) #x400)
		    (format t "\\u~4,'0X\\u~4,'0X" (+ high #xD800) (+ low #xDC00))))
		 ((encoding-error "Invalid Unicode character ‘~A’." char)))))))

(defun string-from-string (string)
  "Encode string STRING as a JSON string."
  (declare (type string string))
  (with-delimiters #\" #\"
    (iter (for char :in-string string)
	  (string-char char))))

(defmethod encode ((object string))
  "Encode a string as a JSON string."
  (string-from-string object))

(defmethod encode ((object symbol))
  "Encode a symbol as a JSON string.
Affected by ‘*print-case*’."
  (let ((string (copy-seq (symbol-name object))))
    (ecase *print-case*
      (:upcase (nstring-upcase string))
      (:downcase (nstring-downcase string))
      (:capitalize (nstring-capitalize string)))
    (string-from-string string)))

(defmethod encode ((object character))
  "Encode a character as a JSON string."
  (with-delimiters #\" #\"
    (string-char object)))

;;; Numbers

(defmethod encode ((object integer))
  "Encode an integer as a JSON number."
  (princ object))

(defmethod encode ((object rational))
  "Encode a rational number as a JSON number.
Affected by ‘*read-default-float-format*’."
  (princ (coerce object *read-default-float-format*)))

(defmethod encode ((object real))
  "Encode a floating-point number as a JSON number."
  (let ((*read-default-float-format* (type-of object)))
    (princ object)))

;;; Literals

(defsubst encode-true (object)
  "Constantly print a JSON ‘true’ value."
  (declare (ignore object))
  (write-string "true"))

(defsubst encode-false (object)
  "Constantly print a JSON ‘false’ value.

Mostly useful for binding ‘*nil-encoder*’."
  (declare (ignore object))
  (write-string "false"))

(defsubst encode-null (object)
  "Constantly print a JSON ‘null’ value.

Mostly useful for binding ‘*nil-encoder*’."
  (declare (ignore object))
  (write-string "null"))

(defmethod encode ((object (eql :true)))
  "Encode ‘:true’ as a JSON ‘true’ value."
  (encode-true object))

(defmethod encode ((object (eql :false)))
  "Encode ‘:false’ as a JSON ‘false’ value."
  (encode-false object))

(defmethod encode ((object (eql :null)))
  "Encode ‘:null’ as a JSON ‘null’ value."
  (encode-null object))

(defmethod encode ((object (eql t)))
  "Encode ‘t’ as a JSON ‘true’ value."
  (encode-true object))

(defmethod encode ((object (eql nil)))
  "Encode ‘nil’ by calling ‘*nil-encoder*’."
  (funcall *nil-encoder* object))

;;; Miscellaneous

(defsubst encode-list (object)
  "Encode a list by calling ‘*list-encoder*’.

Mostly useful for binding ‘*nil-encoder*’."
  (declare (type list object))
  (funcall *list-encoder* object))

(defmethod encode ((object list))
  "Encode a list by calling ‘*list-encoder*’."
  (encode-list object))

;;; encoder.lisp ends here
