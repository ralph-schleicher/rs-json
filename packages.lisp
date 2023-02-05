;;; packages.lisp --- package definitions

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

(defpackage #:de.ralph-schleicher.restless/json
  (:nicknames :rs-json)
  (:use :common-lisp :iterate)
  (:export
   ;; specials.lisp
   #:*object-key-decoder*
   #:*object-as*
   #:*array-as*
   #:*true*
   #:*false*
   #:*null*
   #:*maximum-nesting-depth*
   #:*allow-unicode-whitespace*
   #:*allow-trailing-comma*
   #:*allow-literal-object-keys*
   #:*allow-duplicate-object-keys*
   #:*allow-lax-numbers*
   #:*list-encoder*
   #:*nil-encoder*
   #:with-default-values
   ;; common.lisp
   #:json-error
   #:syntax-error
   #:encoding-error
   ;; decoder.lisp
   #:parse
   ;; encoder.lisp
   #:serialize
   #:encode-object
   #:encode-array
   #:encode-false
   #:encode-null
   #:encode-list
   #:encode)
  (:documentation
   "Yet another JSON decoder/encoder.

If you can't wait until YASON is fixed, then this library is for you.
The main differences are listed below.

   * The parser is strictly RFC 8259 compliant whenever it makes
     sense.  However, you can tweak the behavior of the parser to
     suite your needs.

   * The printer does not support indentation.

   * JSON objects can be represented as hash-tables, associated lists,
     or property lists.  The default is to use alists.

   * JSON arrays can be represented as vectors or lists.  The default
     is to use vectors.

   * The JSON values ‘true’, ‘false’, and ‘null’ are represented by
     the keywords ‘:true’, ‘:false’, and ‘:null’ respectively.  But
     you can change that to suite your needs.

   * The default configuration is round-trip save, i.e. you can read
     a JSON value and write it back without loss of information.  This
     is a strict requirement when updating a Web resource via an HTTP
     GET/PUT cycle.

   * Performance is competitive to other “fast” JSON libraries out
     there."))

;;; packages.lisp ends here
