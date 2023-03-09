;;; specials.lisp --- special variables

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

(defvar *object-key-decoder* #'identity
  "The function to convert the key string of an object member.
Default is the ‘identity’ function.

The function is called with one argument, the key string.  The value
of the function is used as the key for the data structure produced.
See also the ‘*object-as*’ special variable.")

(defvar *object-as* :alist
  "The Lisp data structure used to represent a JSON object.
Value is either ‘:hash-table’, ‘:alist’, or ‘:plist’.  The default
is to use alists.

For hash tables and alists, object keys are compared with ‘equal’,
i.e. using strings as object keys works as expected.  For plists,
object keys are compared with ‘eql’, i.e. you should also set
‘*object-key-decoder*’ to a function returning a symbol when
representing objects as plists.")
(declaim (type (member :hash-table :alist :plist) *object-as*))

(defvar *decode-object-hook* nil
  "Hook to run after a JSON object has been decoded.
Value has to be a function designator.  The function is called with
one argument, the Lisp data structure representing the JSON object;
see the ‘*object-as*’ special variable.  The return value of the
function is used as the actual object value.")

(defvar *array-as* :vector
  "The Lisp data structure used to represent a JSON array.
Value is either ‘:vector’ or ‘:list’.  The default is to use
vectors.")
(declaim (type (member :vector :list) *array-as*))

(defvar *decode-array-hook* nil
  "Hook to run after a JSON array has been decoded.
Value has to be a function designator.  The function is called with
one argument, the Lisp data structure representing the JSON array;
see the ‘*array-as*’ special variable.  The return value of the
function is used as the actual array value.")

(defvar *true* :true
  "The symbol to represent the JSON value ‘true’.
Default is ‘:true’, but ‘t’ may be appropriate, too.")
(declaim (type symbol *true*))

(defvar *false* :false
  "The symbol to represent the JSON value ‘false’.
Default is ‘:false’, but ‘nil’ may be appropriate, too.  For the
later, please take care of ambiguities since ‘nil’ also represents
the empty list.")
(declaim (type symbol *false*))

(defvar *null* :null
  "The symbol to represent the JSON value ‘null’.
Default is ‘:null’, but ‘nil’ may be appropriate, too.  For the
later, please take care of ambiguities since ‘nil’ also represents
falsity and the empty list.")
(declaim (type symbol *null*))

(defvar *maximum-nesting-depth* 1000
  "The maximum number of nested JSON structures.
Value must be a positive number and should be at least 20.  A value
of ‘nil’ means to not limit the depth of nesting.  The default value
is 1000.  This option only has an effect when reading JSON values.")
(declaim (type (or (integer (0)) null) *maximum-nesting-depth*))

(defvar *allow-unicode-whitespace* nil
  "Whether or not to accept any Unicode whitespace character.
If true, any character with the Unicode whitespace property is
considered a whitespace character.  Otherwise, only the space,
horizontal tab, line feed, and carriage return character are
considered a whitespace character.  Default is false.")

(defvar *allow-trailing-comma* nil
  "Whether or not to accept a comma after the last object member
or after the last array element.  Default is false.")

(defvar *allow-literal-object-keys* nil
  "Whether or not to accept literal names as keys of object members.
Default is false.")

(defvar *allow-duplicate-object-keys* nil
  "Whether or not to accept duplicate keys in JSON objects.
If true, the value of an existing object member is replaced by a
successive object member with the same key.  Special value ‘:ignore’
means to ignore successive duplicate object members, i.e. the value
of an existing object member will not be replaced.  Special value
‘:append’ means to add a duplicate object member to the Lisp data
structure – this feature is only available for alists and plists.
If false, signal a ‘syntax-error’.  This is the default.

ECMA-404 says nothing about duplicate object keys.  RFC 8259 says
that object keys should be unique.  Anyway, with this user option,
you have the choice.")

(defvar *allow-lax-numbers* nil
  "Whether or not to accept non-standard syntax of numbers.
If true, numbers may have an explicit plus sign and digits before or
after the decimal point may be omitted.  Default is false.")

(defvar *list-encoder* 'encode-object
  "The actual function used to encode a list.

Suitable values are ‘encode-object’ or ‘encode-array’ to encode
a list as a JSON object or as a JSON array respectively.  Default
is ‘encode-object’ in accordance with the default value of the
‘*object-as*’ special variable.

The function is called with one argument, the list.  The return
value of the function is ignored.

See also the ‘*nil-encoder*’ special variable.")

(defvar *nil-encoder* 'encode-list
  "The actual function used to encode ‘nil’.

Suitable values are ‘encode-list’, ‘encode-false’, or ‘encode-null’
to encode ‘nil’ as a list, as the JSON ‘false’ value, or as the JSON
‘null’ value respectively.  Default is ‘encode-list’ in accordance
with the default value of the ‘*object-as*’ special variable.")

(defvar *pretty-printer* nil
  "Whether or not to enable the pretty printer for JSON output.
Default is false.

If the pretty printer is disabled, JSON output is just a flat
sequence of characters.  For example:

     [{\"foo\" : 42, \"bar\" : [\"baz\", \"hack\"]}, null]

There is no explicit or implicit line break since all control
characters are escaped.  While this is fast and machine readable,
it's difficult for humans to reveal the structure of the data.

If the pretty printer is enabled, JSON output is more visually
appearing.  Here is the same example as above but pretty printed:

     [{\"foo\" : 42,
       \"bar\" : [\"baz\",
                \"hack\"]},
      null]

Explicit line breaks occur after object members and array elements
and the items of these compound structures are lined up nicely.")

(defmacro with-default-values ((&rest ignored) &body body)
  "Establish a lexical environment with all special variables bound
to their default values."
  (declare (ignore ignored))
  `(let ((*object-key-decoder* #'identity)
	 (*object-as* :alist)
	 (*array-as* :vector)
	 (*true* :true)
	 (*false* :false)
	 (*null* :null)
	 (*maximum-nesting-depth* 1000)
	 (*allow-unicode-whitespace* nil)
	 (*allow-trailing-comma* nil)
	 (*allow-literal-object-keys* nil)
	 (*allow-duplicate-object-keys* nil)
	 (*allow-lax-numbers* nil)
	 (*list-encoder* 'encode-object)
	 (*nil-encoder* 'encode-list)
	 (*pretty-printer* nil))
     ,@body))

;;; specials.lisp ends here
