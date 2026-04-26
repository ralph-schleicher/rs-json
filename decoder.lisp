;;; decoder.lisp --- JSON decoder

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

(defvar next-char nil
  "The last character read by the ‘next-char’ function.")
(declaim (type (or null character) next-char))

(defsubst next-char (&optional (eof-error-p t))
  "Read the next character from ‘*standard-input*’."
  (setf next-char (read-char *standard-input* eof-error-p nil)))

(defsubst next-char* (&optional (eof-error-p t))
  "Like the ‘next-char’ function but skip over whitespace characters."
  (loop
    (next-char eof-error-p)
    (unless (and next-char (whitespace-char-p next-char))
      (return)))
  next-char)

(defvar nesting-depth 0
  "The current number of nested structures.")
(declaim (type integer nesting-depth))

(defsubst incr-nesting ()
  "Increase the nesting depth."
  (incf nesting-depth)
  (when (and *maximum-nesting-depth* (> nesting-depth *maximum-nesting-depth*))
    (%syntax-error "Too many nested structures, current limit is ~A." *maximum-nesting-depth*)))

(defsubst decr-nesting ()
  "Decrease the nesting depth."
  (decf nesting-depth))

(defun %syntax-error (&optional (datum nil datum-supplied-p) &rest arguments)
  "Signal a syntax error."
  (when next-char
    (unread-char next-char *standard-input*))
  (cond ((stringp datum)
         (error 'syntax-error
                :stream *standard-input*
                :position (file-position *standard-input*)
                :format-control datum
                :format-arguments arguments))
        (datum-supplied-p
         (apply #'error (or datum 'syntax-error)
                :stream *standard-input*
                :position (file-position *standard-input*)
                arguments))
        (next-char
         (error 'syntax-error
                :stream *standard-input*
                :position (file-position *standard-input*)
                :format-control "Unexpected character ‘~A’."
                :format-arguments (list next-char)))
        (t
         (error 'syntax-error
                :stream *standard-input*
                :position (file-position *standard-input*)
                :format-control "Premature end of file."
                :format-arguments ()))))

(defsubst %plist-from-alist-reverse (alist)
  "Destructively rearrange an alist into a plist.
The order of the pairs is reversed."
  (let (head cell tail)
    (loop
      (when (null alist)
        (return))
      (setf head alist
            cell (first alist)
            alist (rest alist))
      (setf (first head) (rest cell)
            (rest head) tail
            (rest cell) head)
      (setf tail cell))
    tail))

(defun parse-object ()
  "Parse a JSON object."
  (let (object (emptyp t) key-string key value dup)
    ;; Discard opening brace.
    (next-char*)
    (incr-nesting)
    ;; Parse object members.
    (loop
      (case next-char
        (#\}
         (return))
        (#\,
         (when emptyp
           (%syntax-error "Leading comma before object member."))
         ;; Discard comma.
         (next-char*)
         ;; Check for trailing comma.
         (when (and *allow-trailing-comma* (char= next-char #\}))
           (return)))
        (t
         (when (not emptyp)
           (%syntax-error "Missing comma after object member."))))
      ;; Read the key.
      (setf key-string (if (char= next-char #\")
                           (parse-string)
                         (progn
                           (when (not *allow-literal-object-keys*)
                             (%syntax-error "Object key must be a quoted string."))
                           (parse-literal t)))
            key (funcall *object-key-decoder* key-string))
      ;; Check if the key already exists.
      (setf dup (assoc key object :test #'equal))
      (when dup
        (cond ((not *allow-duplicate-object-keys*)
               (%syntax-error "Duplicate object key ‘~A’." key-string))
              ((and (eq *object-as* :hash-table)
                    (eq *allow-duplicate-object-keys* :append))
               (error 'program-error))))
      ;; Read the key/value separator.
      (when (null next-char)
        (error 'end-of-file :stream *standard-input*))
      (unless (char= next-char #\:)
        (%syntax-error "Expect a colon between object key and value."))
      (next-char*)
      ;; Read the value.
      (setf value (parse-value))
      (cond ((or (not dup) (eq *allow-duplicate-object-keys* :append))
             ;; First occurrence of the key.
             (setf object (acons key value object)))
            ((not (eq *allow-duplicate-object-keys* :ignore))
             ;; Successive occurrence of the same key.
             ;; Replace existing value.
             (rplacd dup value)))
      ;; Object is not empty.
      (setf emptyp nil))
    ;; Discard closing brace and skip trailing whitespace.
    (decr-nesting)
    (next-char* nil)
    ;; Return value.
    (ecase *object-as*
      (:alist
       (setf object (nreverse object)))
      (:plist
       (setf object (%plist-from-alist-reverse object)))
      (:hash-table
       (setf object (alexandria:alist-hash-table object :test #'equal))))
    (when *decode-object-hook*
      (setf object (funcall *decode-object-hook* object)))
    object))

(defun parse-array ()
  "Parse a JSON array."
  (let (array (length 0))
    ;; Discard opening bracket.
    (next-char*)
    (incr-nesting)
    ;; Parse array elements.
    (loop
      (case next-char
        (#\]
         (return))
        (#\,
         (when (= length 0)
           (%syntax-error "Leading comma before array element."))
         ;; Discard comma.
         (next-char*)
         ;; Check for trailing comma.
         (when (and *allow-trailing-comma* (char= next-char #\]))
           (return)))
        (t
         (when (> length 0)
           (%syntax-error "Missing comma after array element."))))
      ;; Read the array element.
      (push (parse-value) array)
      ;; Array is not empty.
      (incf length))
    ;; Discard closing bracket and skip trailing whitespace.
    (decr-nesting)
    (next-char* nil)
    ;; Return value.
    (setf array (nreverse array))
    (when (eq *array-as* :vector)
      (setf array (make-array length :initial-contents array)))
    (when *decode-array-hook*
      (setf array (funcall *decode-array-hook* array)))
    array))

(defun %unicode-escape ()
  "Helper function for ‘parse-string’."
  (flet ((parse-hex ()
           "Read four hexadecimal digits and return the corresponding numerical value."
           (logior (ash (or (digit-char-p (next-char) 16) (%syntax-error)) 12)
                   (ash (or (digit-char-p (next-char) 16) (%syntax-error))  8)
                   (ash (or (digit-char-p (next-char) 16) (%syntax-error))  4)
                   (or (digit-char-p (next-char) 16) (%syntax-error)))))
    (let ((high (parse-hex)))
      (if (not (<= #xD800 high #xDFFF))
          ;; A regular character.
          (code-char high)
        ;; A surrogate pair.
        (progn
          (unless (and (char= (next-char) #\\)
                       (char= (next-char) #\u))
            (%syntax-error))
          (let ((low (parse-hex)))
            (unless (and (<= #xD800 high #xDBFF)
                         (<= #xDC00 low #xDFFF))
              (%syntax-error "Invalid UTF-16 surrogate pair U+~4,'0X and U+~4,'0X in string." high low))
            #-cmucl
            (code-char (+ (ash (- high #xD800) 10)
                          (- low #xDC00)
                          #x10000))
            ;; CMUCL strings use UTF-16 encoding.  Just return the
            ;; surrogate pair as is.
            #+cmucl
            (values (code-char high) (code-char low))))))))

(macrolet ((body ()
             ;; Parse quoted string.
             `(loop
                ;; Initially, this call discards the
                ;; opening quote character.
                (next-char)
                (case next-char
                  (#\"
                   ;; Discard closing quote character
                   ;; and skip trailing whitespace.
                   (next-char* nil)
                   (return))
                  (#\\
                   ;; Escape sequence.
                   (next-char)
                   (case next-char
                     (#\" (outc #\"))
                     (#\\ (outc #\\))
                     (#\/ (outc #\/))
                     (#\b (outc #\Backspace))
                     (#\f (outc #\Page))
                     (#\n (outc #\Linefeed))
                     (#\r (outc #\Return))
                     (#\t (outc #\Tab))
                     (#\u
                      #-cmucl
                      (outc (%unicode-escape))
                      #+cmucl
                      (multiple-value-bind (high low)
                          (%unicode-escape)
                        (outc high)
                        (when low
                          (outc low))))
                     (t
                      (%syntax-error "Unknown escape sequence ‘\\~A’ in string." next-char))))
                  (t
                   ;; Any other character.
                   ;;
                   ;; “All code points may be placed within the
                   ;; quotation marks except for the code points
                   ;; that must be escaped: quotation mark (U+0022),
                   ;; reverse solidus (U+005C), and the control
                   ;; characters U+0000 to U+001F.”
                   (when (<= 0 (char-code next-char) #x1F)
                     (%syntax-error "Raw control character ‘~A’ in string." next-char))
                   (outc next-char)))))

           (define-string-parser (name () doc form)
             `(defun ,name ()
                ,@(when doc (list doc))
                ,@(when form (list form)))))

  (define-string-parser %string-as-string ()
    "Parse a JSON string as a ‘string’."
    (with-output-to-string (buffer)
      (flet ((outc (char)
               "Append a character to the output buffer."
               (write-char char buffer)))
        (declare (inline outc))
        (body))))

  (define-string-parser %string-as-base-string ()
    "Attempt to parse a JSON string as a ‘base-string’."
    (with-scratch-buffer ()
      (let ((base-string-p t))
        (flet ((outc (char)
                 "Append a character to the output buffer."
                 (unless (typep char 'base-char)
                   (setf base-string-p nil))
                 (outc char)))
          (declare (inline outc))
          (body))
        (if (not base-string-p)
            (buffer-string)
          (let ((string (make-array (- (point-max) (point-min)) :element-type 'base-char)))
            (iter (for i :from 0)
                  (for j :from (point-min) :below (point-max))
                  (setf (char string i) (char (current-buffer) j)))
            string))))))

(defvar parse-string-function #'%string-as-string
  "The function to parse a JSON string.")

(defsubst parse-string ()
  "Parse a JSON string."
  (funcall parse-string-function))

(defun %make-float (n q d)
  "Poor man's floating-point number conversion of significand N and
decimal exponent Q.  Third argument D is the number of significant
digits."
  (declare (type (integer 0) n d)
           (type integer q))
  (coerce
   (if (zerop n)
       0
     (let ((e (+ q d)))
       (cond ((> e 350)
              (error 'floating-point-overflow
                     :operation '%make-float
                     :operands (list n q d)))
             ((< e -400)
              0)
             ((minusp q)
              (/ n (expt 10 (- q))))
             (t ;q ≥ 0
              (* n (expt 10 q))))))
   'double-float))

(defun parse-number ()
  "Parse a JSON number."
  (let ((sign #\+)
        (int 0) ;significand
        (bias 0) ;exponent of the significand
        (exp 0) ;explicit exponent
        (digits 0) ;number of significant digits
        intp fracp ;true if a digit is processed
        floatp ;true if the result is a floating-point number
        (strictp (not *allow-lax-numbers*)))
    (block nil
      ;; Optional number sign.
      (cond ((char= next-char #\-)
             (setf sign #\-)
             (next-char))
            ((char= next-char #\+)
             (when strictp
               (%syntax-error "Number starts with an explicit plus sign."))
             (next-char)))
      ;; Integer part.
      ;;
      ;; To restrict the significand to 19 significant digits, replace
      ;; any occurrence of the form
      ;;
      ;;      (setf int (+ (* int 10) ...))
      ;; by
      ;;      (if (< digits 19)
      ;;          (setf int (+ (* int 10) ...))
      ;;        (incf bias))
      (cond ((char= next-char #\0)
             (setf intp t)
             (next-char nil))
            (t
             ;; If NEXT-CHAR is actually a decimal digit, then it is
             ;; non-zero and all following digits are significant.
             (iter (for digit = (and next-char (decimal-digit-char-p next-char)))
                   (while digit)
                   (setf int (+ (* int 10) digit))
                   (incf digits)
                   (setf intp t)
                   (next-char nil))
             (when (and strictp (not intp))
               (%syntax-error "Integer part of a number must not be empty."))))
      (when (null next-char)
        (return))
      ;; Optional fractional part.
      (when (char= next-char #\.)
        (setf floatp t)
        ;; Skip decimal point.  If the integer part is empty, the
        ;; fractional part must be not empty.
        (next-char (or strictp (not intp)))
        (when (null next-char)
          (return))
        ;; Fractional part.
        (let ((fractional-digits 0)
              (trailing-zeros 0))
          (iter (for digit = (and next-char (decimal-digit-char-p next-char)))
                (while digit)
                (if (zerop digit)
                    (incf trailing-zeros)
                  (progn
                    (incf fractional-digits (1+ trailing-zeros))
                    ;; Process intermediate zeros.
                    (if (zerop int)
                        ;; Don't have a significant digit up to here.
                        (setf trailing-zeros 0)
                      (iter (while (plusp trailing-zeros))
                            (setf int (* int 10))
                            (incf digits)
                            (decf trailing-zeros)))
                    ;; Process the significant digit.
                    (setf int (+ (* int 10) digit))
                    (incf digits)))
                (setf fracp t)
                (next-char nil))
          (when (and strictp (not fracp))
            (%syntax-error "Fractional part of a number must not be empty."))
          (decf bias fractional-digits)
          (when (null next-char)
            (return))))
      ;; Need at least one digit.
      (unless (or intp fracp)
        (%syntax-error "Significand of a number must consist of at least one digit."))
      ;; Optional exponent part.
      (when (or (char= next-char #\E)
                (char= next-char #\e))
        (setf floatp t)
        ;; Skip exponent marker.
        (next-char)
        ;; Exponent.
        (let ((sign #\+) digitp)
          (cond ((char= next-char #\-)
                 (setf sign #\-)
                 (next-char))
                ((char= next-char #\+)
                 (next-char)))
          (iter (for digit = (and next-char (decimal-digit-char-p next-char)))
                (while digit)
                (setf exp (+ (* exp 10) digit))
                (setf digitp t)
                (next-char nil))
          (when (not digitp)
            (%syntax-error "Exponent of a number must not be empty."))
          (when (char= sign #\-)
            (setf exp (- exp))))))
    ;; Create the unsigned number.
    (let ((val (if floatp (%make-float int (+ bias exp) digits) int)))
      ;; Skip trailing whitespace.
      (when (and next-char (whitespace-char-p next-char))
        (next-char* nil))
      ;; Return value.
      (if (char= sign #\-) (- val) val))))

(defun parse-literal (&optional identifierp)
  "Parse a JSON literal name token, i.e. ‘true’, ‘false’, or ‘null’.

If optional argument IDENTIFIERP is true, accept any valid JavaScript
 identifier.

Return either the Lisp value of the literal name token or the identifier
name (a string)."
  ;; The idea is to parse a JavaScript identifier name and then check
  ;; whether or not it is a literal name token.
  (with-scratch-buffer ()
    ;; Identifier names do not start with a digit.
    (unless (or (alpha-char-p next-char)
                (char= next-char #\$)
                (char= next-char #\_))
      (%syntax-error))
    (loop
      (outc next-char)
      (next-char nil)
      (unless (and next-char
                   (or (alpha-char-p next-char)
                       (digit-char-p next-char)
                       (char= next-char #\$)
                       (char= next-char #\_)))
        (return)))
    (let ((buffer (current-buffer))
          (start (point-min))
          (end (point-max)))
      (prog1
          (if (not identifierp)
              ;; Expect a literal name token.
              (cond ((string= buffer "true"  :start1 start :end1 end)
                     *true*)
                    ((string= buffer "false" :start1 start :end1 end)
                     *false*)
                    ((string= buffer "null"  :start1 start :end1 end)
                     *null*)
                    ((%syntax-error "Unknown literal name token ‘~A’." (buffer-string))))
            ;; Accept any identifier name.
            (let ((name (buffer-string)))
              (if (or (string= name "true")
                      (string= name "false")
                      (string= name "null"))
                  (%syntax-error "Literal name token ‘~A’ is not a valid identifier name." name)
                name)))
        ;; Skip trailing whitespace.
        (when (and next-char (whitespace-char-p next-char))
          (next-char* nil))))))

(defun parse-value ()
  "Parse any JSON value."
  (case next-char
    (#\{
     (parse-object))
    (#\[
     (parse-array))
    (#\"
     (parse-string))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\.)
     (parse-number))
    (t
     (parse-literal))))

(defun %read (stream &optional junk-allowed)
  "Common entry point for all read functions."
  ;; Using a non-volatile scratch buffer for parsing numbers
  ;; and literals reduces running time by approximately 10 %
  ;; and memory requirements by 20 % on file ‘large.json’.
  (let ((*scratch* (make-scratch-buffer))
        (*standard-input* stream)
        (next-char nil)
        (nesting-depth 0)
        (parse-string-function (if *string-as-base-string* #'%string-as-base-string #'%string-as-string)))
    ;; Read first character.
    (next-char*)
    ;; Parse the JSON value.
    (let ((data (parse-value)))
      ;; Check for end of file.
      (when next-char
        (unless junk-allowed
          (%syntax-error))
        (unread-char next-char stream))
      ;; Return values.
      (values data (file-position stream)))))

(defun parse (source &key junk-allowed)
  "Read a JSON value.

First argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is
 equal to ‘*standard-input*’
If keyword argument JUNK-ALLOWED is true, do not signal an error
 of type ‘syntax-error’ if a non-whitespace character occurs after
 the JSON value.  Default value is false.

The ‘parse’ function expects exactly one JSON value.  Any value
is accepted, not only an object or array.  Optional leading and
trailing whitespace is ignored.

Return value is the Lisp representation of the JSON value.
Secondary value is the position where the parsing ends, or
‘nil’ if the position can not be determined.

Exceptional Situations:

   * Signals an ‘end-of-file’ error if the input ends in the
     middle of a JSON value.

   * Signals a ‘syntax-error’ if the input contains an invalid
     JSON structure.

   * May signal an ‘arithmetic-error’ if a JSON number can not
     be represented as a Lisp number.

   * Signals a ‘program-error’ if JSON objects are parsed as
     hash tables, ‘*allow-duplicate-object-keys*’ is bound to
     ‘:append’, and a duplicate object member occurs."
  (etypecase source
    (stream
     (%read source junk-allowed))
    (string
     (with-input-from-string (stream source)
       (%read stream junk-allowed)))
    (pathname
     (with-open-file (stream source :external-format (uiop:encoding-external-format :utf-8))
       (%read stream junk-allowed)))
    ((member t)
     (%read *standard-input* junk-allowed))))

(defun %oref (data key)
  "Return the value of an object member.

Signal an error of type ‘parse-error’ if the object member
does not exist."
  (declare (type string key))
  (let ((needle (funcall *object-key-decoder* key)))
    (typecase data
      (list
       (cond ((null data)
              ;; No key.
              ())
             ((consp (first data))
              ;; Object as alist.
              (alexandria:when-let ((cell (assoc needle data :test #'equal)))
                (return-from %oref (cdr cell))))
             (t
              ;; Object as plist.
              (iter (for tail :on data :by #'cddr)
                    (when (equal (first tail) needle)
                      (when (endp (rest tail))
                        (error 'simple-type-error
                               :format-control "Lisp data structure is not a property list.~%Data: ~S"
                               :format-arguments (list data)))
                      (return-from %oref (second tail)))))))
      (hash-table
       (multiple-value-bind (value exists)
           (gethash needle data)
         (when exists
           (return-from %oref value))))))
  (error 'parse-error))

(defun %oset (data key value &optional create)
  "Store a value into an object member.

Signal an error of type ‘parse-error’ if the object member
does not exist."
  (declare (type string key))
  (let ((needle (funcall *object-key-decoder* key)))
    (typecase data
      (cons
       (cond ((consp (first data))
              ;; Object as alist.
              (alexandria:when-let ((cell (assoc needle data :test #'equal)))
                (setf (cdr cell) value)
                (return-from %oset))
              (when create
                (nconc data (acons needle value ()))
                (return-from %oset)))
             (t
              ;; Object as plist.
              (iter (for tail :on data :by #'cddr)
                    (when (equal (first tail) needle)
                      (when (endp (rest tail))
                        (error 'simple-type-error
                               :format-control "Lisp data structure is not a property list.~%Data: ~S"
                               :format-arguments (list data)))
                      (setf (second tail) value)
                      (return-from %oset)))
              (when create
                (nconc data (list needle value))
                (return-from %oset)))))
      (hash-table
       (when (or create (nth-value 1 (gethash needle data)))
         (setf (gethash needle data) value)
         (return-from %oset)))))
  (error 'parse-error))

(defun %aref (data index)
  "Return the value of an array element.

Signal an error of type ‘parse-error’ if the array element
does not exist."
  (declare (type (integer 0) index))
  (typecase data
    (vector
     ;; Array as vector.
     (when (< index (length data))
       (return-from %aref (aref data index))))
    (list
     ;; Array as list.
     (alexandria:when-let ((tail (nthcdr index data)))
       (return-from %aref (first tail)))))
  (error 'parse-error))

(defun %aset (data index value &optional append)
  "Store a value into an array element.

Signal an error of type ‘parse-error’ if the array element
does not exist."
  (declare (type (integer 0) index))
  (if (not append)
      (typecase data
        (vector
         ;; Array as vector.
         (when (< index (length data))
           (setf (aref data index) value)
           (return-from %aset)))
        (cons
         ;; Array as list.
         (alexandria:when-let ((tail (nthcdr index data)))
           (setf (first tail) value)
           (return-from %aset))))
    ;; Append an array element.
    (typecase data
      (vector
       (vector-push-extend value data)
       (return-from %aset))
      (cons
       (nconc data (list value))
       (return-from %aset))))
  (error 'parse-error))

(defun jref (data &rest keys)
  "Access an element of a JSON value.

First argument DATA is the Lisp representation of a JSON value.
Remaining arguments are element selectors, keys for short.  A key is
 either a string or a non-negative integer selecting an object member
 or array element respectively.  If the key is a string, it is
 filtered by calling ‘*object-key-decoder*’ before looking up the
 object member.  A key can also be the symbol ‘-’ to refer to the
 array element after the last array element.

Return the value of the selected element.

The ‘setf’ macro can be used with ‘jref’ to modify the value
associated with the selected element.

Exceptional Situations:

   * Signals a ‘type-error’ if the key is not a string,
     a non-negative integer, or the symbol ‘-’.

   * Signals a ‘parse-error’ if the key does not exist.

Notes:

The ‘jref’ function implements the JSON Pointer standard, see
RFC 6901, but with a Lisp interface instead of a string syntax
to identify a specific element in a JSON value."
  (dolist (key keys)
    (etypecase key
      (string
       ;; Access an object member.
       (setf data (%oref data key)))
      ((integer 0)
       ;; Access an array element.
       (setf data (%aref data key)))
      ((eql -)
       (error 'parse-error))))
  data)

(defun (setf jref) (value data key &rest keys)
  (loop
    (when (endp keys)
      (return))
    ;; Descend into the data structure.
    (setf data (jref data key)
          key (first keys)
          keys (rest keys)))
  ;; Assign new value.
  (etypecase key
    (string
     (%oset data key value))
    ((integer 0)
     (%aset data key value))
    ((eql -)
     (error 'parse-error)))
  ;; Satisfy the setf protocol.
  value)

;;; decoder.lisp ends here
