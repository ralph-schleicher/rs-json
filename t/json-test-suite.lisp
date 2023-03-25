;;; json-test-suite.lisp --- JSON Test Suite parser

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

(defpackage :json-test-suite
  (:use :common-lisp)
  (:export #:main))

(in-package :json-test-suite)

(defvar program-name)

(defun standalone-debugger (condition hook)
  "Terminate the program indicating a failure condition."
  (declare (ignore hook))
  (format *error-output*
	  "~&~A: internal error (~A)~%~A~&"
	  program-name (type-of condition) condition)
  (uiop:quit 2))

(defun main ()
  "Program entry point."
  (setf *debugger-hook* #'standalone-debugger)
  (setf program-name (file-namestring (uiop:argv0)))
  (let ((arg (uiop:command-line-arguments)))
    (unless (= (length arg) 2)
      (uiop:die 2 "~A: wrong number of arguments~%" program-name))
    (let ((library (intern (string-upcase (first arg)) (find-package :keyword)))
	  (file-name (second arg)))
      (unless (member library rs-json-bench::*libraries*)
	(uiop:die 2 "~A: unknown JSON library ‘~(~A~)’~%" program-name library))
      (unless (probe-file file-name)
	(uiop:die 2 "~A:~A: no such file~%" program-name file-name))
      ;; Run the actual program.
      (handler-case
	  (rs-json-bench::%read library (parse-namestring file-name))
	(error (condition)
	  (format *error-output*
		  "~&~A:~A: error (~A)~%~A~&"
		  program-name file-name (type-of condition) condition)
	  ;; Failure.
	  (uiop:quit 1)))
      ;; Success.
      (uiop:quit 0))))

;;; json-test-suite.lisp ends here
