;;; common.lisp --- common definitions

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

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(defsubst whitespace-char-p (char)
  "Return true if CHAR is a whitespace character.
Argument CHAR has to be a character object."
  (declare (type character char))
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Linefeed)
      (char= char #\Return)
      (char= char #\Newline)
      #+cl-unicode
      (when *allow-unicode-whitespace*
	(cl-unicode:has-binary-property char (quote #.(cl-unicode:property-symbol "White_Space"))))))

(define-condition json-error (stream-error simple-condition)
  ()
  (:documentation "Base class for all JSON errors."))

(define-condition syntax-error (json-error)
  ()
  (:documentation "Condition type for a syntax error.")
  (:report (lambda (condition stream)
	     (format stream "Invalid JSON syntax")
	     (alexandria:when-let ((input (stream-error-stream condition)))
	       (format stream " in ~S" input)
	       (alexandria:when-let (position (file-position input))
		 (format stream " at ~S" position)))
	     (format stream ".")
	     (when (stringp (simple-condition-format-control condition))
	       (terpri stream)
	       (apply #'format stream
		      (simple-condition-format-control condition)
		      (simple-condition-format-arguments condition))))))

(define-condition encoding-error (json-error)
  ()
  (:documentation "Condition type for an encoding error.")
  (:report (lambda (condition stream)
	     (format stream "Failed to encode a Lisp object as JSON")
	     (alexandria:when-let ((output (stream-error-stream condition)))
	       (format stream " in ~S" output)
	       (alexandria:when-let (position (file-position output))
		 (format stream " at ~S" position)))
	     (format stream ".")
	     (when (stringp (simple-condition-format-control condition))
	       (terpri stream)
	       (apply #'format stream
		      (simple-condition-format-control condition)
		      (simple-condition-format-arguments condition))))))

;;; common.lisp ends here
