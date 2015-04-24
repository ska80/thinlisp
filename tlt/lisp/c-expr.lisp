(in-package "TLI")

;;;; Module C-EXPR

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1995 Gensym Corporation.
;;; All rights reserved.

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>

;;; Author: Jim Allard






;;;; C Expression Structures




;;; This module implements structures and operations for representing and
;;; manipulating C expressions during Lisp to C translations.

;;; The structure type `c-expr' contains two slots, one used to hold a
;;; pointer to an emitter function and the other containing a precedence
;;; integer.  The precedence value should come from the table in CaRM4, p. 181,
;;; with the exception that the value should be negative if the operator is
;;; right-associative.  This structure is included into different types of
;;; substructures that implement the different types of C expressions.

(defstruct (c-expr)
  emitter
  precedence-slot)




;;; The macro `c-expr-precedence' returns the absolute value of the stored
;;; precedence value.

(defmacro c-expr-precedence (c-expr)
  `(abs (c-expr-precedence-slot ,c-expr)))




;;; The macros `c-expr-right-associative-p' and
;;; `c-expr-left-associative-p' return whether or not the operator for the
;;; given expression binds right or left.  Taken from the table in CaRM3,
;;; p. 167.

(defmacro c-expr-right-associative-p (c-expr)
  `(> (the fixnum (c-expr-precedence-slot ,c-expr)) 0))

(defmacro c-expr-left-associative-p (c-expr)
  `(< (the fixnum (c-expr-precedence-slot ,c-expr)) 0))





;;; The macro `emit-expression-to-c-file' takes a C expression structure, a C
;;; file structure, and the indentation level for the enclosing C statement.  If
;;; there are any line breaks emitted into the C file within this expression,
;;; they should be indented at the current indentation level plus two.

(defmacro emit-expression-to-c-file (c-expr c-file indent)
  (let ((expr (gensym)))
    `(let ((,expr ,c-expr))
       (unless (c-expr-p ,expr)
	 (error "~s is being emitted as a c-expr, but it isn't one." ,expr))
       (funcall (c-expr-emitter ,expr) ,expr ,c-file ,indent))))






;;;; C Expression Formatting Utilities




;;; The macro `def-c-expr' defines a structure and emitting function for the
;;; given C expression name.  It takes a structure name, an arglist of the
;;; precedence-slot value for this expression and the slots of the structure,
;;; and a body of forms to define the emitter function for this C expression
;;; type.  The variables expr, c-file, and indent are provided within the
;;; defined emitter function.

;;; The set of slots for the structure can either be a symbol naming a slot, or
;;; it can be a list whose car is the slot name and the rest are keyword
;;; arguments of slot specs.  The only slot spec currently supported is
;;; :validity-test which takes an unevaluated symbol naming a function or macro
;;; of one argument that will be used to check if the value of that slot is
;;; valid on creation of the c-expr.  This is being used to detect bugs in
;;; translation functions which might not provide the correct data type to a
;;; slot.

(defmacro def-c-expr (c-expr-name (precedence-slot &rest slots) &body forms)
  (let* ((emitter-name (intern (format nil "EMIT-~a" c-expr-name)))
	 (slot-names (loop for slot in slots
			   collect (if (consp slot) (cons-car slot) slot)))
	 (slot-checkers? (loop for slot in slots thereis (not (symbolp slot))))
	 (constructor-name (intern (format nil "MAKE-~a" c-expr-name)))
	 (internal-constructor
	   (if slot-checkers?
	       (intern (format nil "~a-INTERNAL" constructor-name))
	       constructor-name)))
    `(progn
       (defstruct (,c-expr-name
		    (:include c-expr
			      (emitter (function ,emitter-name))
			      (precedence-slot ,precedence-slot))
		    (:constructor ,internal-constructor ,slot-names))
	 ,@slot-names)
       ,@(when slot-checkers?
	   `((defun ,constructor-name ,slot-names
	       (let ((new (,internal-constructor ,@slot-names)))
		 ,@(loop for slot in slots
			 when (consp slot)
			   collect
			   (tl:destructuring-bind-strict
			     (name &key (test nil))
			     slot
			     (when test
			       (let ((accessor
				       (intern
					 (format nil "~a-~a" c-expr-name name))))
				 `(unless (,test (,accessor new))
				    (error
				      ,(format
					 nil "Bad ~a argument to MAKE-~a: ~~s"
					 name c-expr-name)
				      (,accessor new)))))))
		 new))))
       (defun ,emitter-name (expr c-file indent)
	 ,@forms))))



;;; The macro `format-c-expr' takes a constant control string, a surrounding
;;; expression, a C file, an indentation level, and the rest of the args are
;;; values formatted within the control string.  Within the control string, the
;;; dispatching character is %, with the following formatting options available:

;;;   % = (emit-character-to-c-file #\% c-file)
;;;   S = (emit-string-to-c-file arg c-file)
;;;   C = (emit-character-to-c-file arg c-file)
;;;   E = (emit-expression-to-c-file arg c-file indent)
;;;   L = (emit-left-expression-to-c-file expr arg c-file indent)
;;;   R = (emit-right-expression-to-c-file expr arg c-file indent)
;;;   A = (emit-arglist-to-c-file arg c-file indent)
;;;   I = (emit-indentation-to-c-file-if-necessary c-file arg indent)
;;;   N = (emit-newline-to-c-file c-file)

(defmacro format-c-expr
    (constant-control-string surrounding-expr c-file indent
			     &rest formatted-args)
  (let ((forms nil)
	(constant-chars nil)
	(s-expr (make-symbol "EXPR"))
	(cf (make-symbol "C-FILE"))
	(ind (make-symbol "INDENT"))
	(args formatted-args)
	(arg-count (length formatted-args)))
    (loop with used-args = 0
	  for index from 0 below (length constant-control-string)
	  for char = (schar constant-control-string index)
	  do
      (cond ((not (char= char #\%))
	     (push char constant-chars))
	    (t
	     (when constant-chars
	       (if (null (cdr constant-chars))
		   (push `(emit-character-to-c-file ,(car constant-chars) ,cf)
			 forms)
		   (push `(emit-string-to-c-file
			   ,(make-string-array
			     (length constant-chars)
			     :initial-contents (reverse constant-chars))
			   ,cf)
			 forms))
	       (setq constant-chars nil))
	     (incf index)
	     (setq char (schar constant-control-string index))
	     (ecase char
	       ((#\%)
		(push #\% constant-chars))
	       ((#\S #\s)
		(incf used-args)
		(push `(emit-string-to-c-file ,(pop args) ,cf) forms))
	       ((#\C #\c)
		(incf used-args)
		(push `(emit-character-to-c-file ,(pop args) ,cf) forms))
	       ((#\E #\e)
		(incf used-args)
		(push `(emit-expression-to-c-file ,(pop args) ,cf ,ind) forms))
	       ((#\L #\l)
		(incf used-args)
		(push `(emit-left-expression-to-c-file
			 ,s-expr ,(pop args) ,cf ,ind)
		      forms))
	       ((#\R #\r)
		(incf used-args)
		(push `(emit-right-expression-to-c-file
			 ,s-expr ,(pop args) ,cf ,ind)
		      forms))
	       ((#\A #\a)
		(incf used-args)
		(push `(emit-arglist-to-c-file ,(pop args) ,cf ,ind)
		      forms))
	       ((#\I #\i)
		(incf used-args)
		(push `(emit-indentation-to-c-file-if-necessary
			 ,cf ,(pop args) ,ind)
		      forms))
	       ((#\N #\n)
		(push `(emit-newline-to-c-file ,cf) forms)))))
	  finally
	    (progn
	      (when constant-chars
		(if (null (cdr constant-chars))
		    (push `(emit-character-to-c-file ,(car constant-chars) ,cf)
			  forms)
		    (push `(emit-string-to-c-file
			    ,(make-string-array
			      (length constant-chars)
			      :initial-contents (reverse constant-chars))
			    ,cf)
			  forms))
		(setq constant-chars nil))
	      (unless (= used-args arg-count)
		(warn "Format-c-expr used ~a args, but was given ~a"
		      used-args arg-count))
	      (return
		`(let ((,s-expr ,surrounding-expr)
		       (,cf ,c-file)
		       (,ind ,indent))
		   (format-c-expr-suppress-not-used-warnings ,s-expr ,cf ,ind)
		   ,@(reverse forms)))))))

(defun format-c-expr-suppress-not-used-warnings (a b c)
  (declare (ignore a b c))
  nil)




;;; The function `emit-arglist-to-c-file' takes a list of C expressions to emit,
;;; a C file to emit into, and the current indentation level.  This function
;;; will emit a comma separated list of the given arguments to the C file.  Note
;;; that this does not wrap any parens or other formatting around the written
;;; list.  If given NIL, this function does nothing.

(defun emit-arglist-to-c-file (expressions-list c-file indent)
  (loop for first = t then nil
	for arg in expressions-list do
    (unless first
      (emit-character-to-c-file #\, c-file)
      (emit-indentation-to-c-file-if-necessary c-file arg indent))
    (emit-expression-to-c-file arg c-file indent)))




;;; The function `emit-infix-expr-to-c-file' takes a surrounding expression, a
;;; c-file, and indentation level, a left argument, an operation character or
;;; string, and a right argument.  This function emits the infix operation to
;;; the given C file.

(defun emit-infix-expr-to-c-file (expr c-file indent left op right)
  (emit-left-expression-to-c-file expr left c-file indent)
  (emit-indentation-to-c-file-if-necessary c-file right indent)
  (if (characterp op)
      (emit-character-to-c-file op c-file)
      (emit-string-to-c-file op c-file))
  (emit-right-expression-to-c-file expr right c-file indent))
	  





;;;; C Expression Structures




;;; The structure `c-name-expression' represents a reference to a C identifier,
;;; either local or global.

(def-c-expr c-name-expr (17 (name :test stringp))
  (declare (ignore indent))
  (emit-string-to-c-file (c-name-expr-name expr) c-file))




;;; The macro `unquoted-c-string-char-p' takes a character and returns whether
;;; that character can be included within a C constant string without special
;;; quoting.

(defmacro unquoted-c-string-char-p (character)
  `(char-bit-on-p standard-c-string-char-bit-vector ,character))

(defparameter standard-c-string-char-bit-vector
  (make-char-bit-vector
    '((#\a #\z)
      (#\A #\Z)
      (#\0 #\9)
      " !#%^&*()-_=+~[]|;:{},.<>/?$@`")))




;;; The macro `c-escape-char-for-char?' takes a character and returns the escape
;;; character that will produce the argument within a C constant string when the
;;; escape character follows a backslash.  It returns NIL if no such escape
;;; character exists.

;;; These characters were taken from the table in CaRM4, p. 34.  The alert (a),
;;; form feed (f), horizontal tab (t), vertical tab (v), and question mark (?)
;;; were not included since they were rare or had questionable translations
;;; between Lisp and C -jra 8/28/95

(defmacro c-escape-char-for-char? (char)
  `(cdr (assoc ,char
	       '((#\"         . #\")
		 (#\newline   . #\n)
		 (#\return    . #\r)
		 (#\\         . #\\)
		 (#\'         . #\')
		 (#\backspace . #\b)))))




;;; The function `transform-to-c-string-constant' takes a string and returns the
;;; equivalent string with all needed C character quoting.  This function will
;;; either return the given string if no quoting is needed or return a new
;;; string containing the appropriate additional quoting.

(defun transform-to-c-string-constant (string)
  (let ((length (length string)))
    (if (and (simple-string-p string)
	     (loop for index fixnum from 0 below length
		   always (unquoted-c-string-char-p (schar string index))))
	string
	(with-output-to-string (output)
	  (loop for index fixnum from 0 below length
		for char = (char string index)
		do
	    (if (unquoted-c-string-char-p char)
		(write-char char output)
		(let ((escape-code? (c-escape-char-for-char? char)))
		  (write-char #\\ output)
		  (if escape-code?
		      (write-char escape-code? output)
		      (format output "~3,'0o" (char-code char))))))))))




;;; The structure `c-literal-expr' represents a reference to a constant, either
;;; an integer, double, character, string, or a vector holding integers or
;;; doubles.

(defmacro ok-as-c-literal-expr-p (literal-value)
  (let ((value (gensym)))
    `(let ((,value ,literal-value))
       (typecase ,value
	 (fixnum t)
	 (double-float t)
	 (string t)
	 (character t)
	 (integer (<= (integer-length ,value) 31))
	 (t
	  (and (consp ,value) (eq (cons-cdr ,value) 'long)))))))

(def-c-expr c-literal-expr (17 (literal-value :test ok-as-c-literal-expr-p))
  (declare (ignore indent))
  (let ((literal-value (c-literal-expr-literal-value expr)))
    (typecase literal-value
      (fixnum
       (emit-string-to-c-file
	 (write-integer-to-string literal-value)
	 c-file))
      (double-float
       (emit-string-to-c-file
	 (format nil "~a" literal-value)
	 c-file))
      (string
       (emit-character-to-c-file #\" c-file)
       (emit-string-to-c-file 
	 (transform-to-c-string-constant literal-value)
	 c-file)
       (emit-character-to-c-file #\" c-file))
      (character
       (emit-character-to-c-file #\' c-file)
       (if (unquoted-c-string-char-p literal-value)
	   (emit-character-to-c-file literal-value c-file)
	   (emit-string-to-c-file 
	     (transform-to-c-string-constant
	       (make-string 1 :initial-element literal-value))
	     c-file))
       (emit-character-to-c-file #\' c-file))
      (integer
       ;; The bignum case for fixnum constants exceeding 28 bits in size.
       (if (<= (integer-length literal-value) 31)
	   (emit-string-to-c-file
	     (write-integer-to-string literal-value)
	     c-file)
	   (translation-error "Can't emit ~s as a C literal, it's too large."
			      literal-value)))
      (t
       (cond
	 ((and (consp literal-value)
	       (eq (cons-cdr literal-value) 'long))
	  (emit-string-to-c-file
	    (write-integer-to-string (cons-car literal-value))
	    c-file)
	  (emit-character-to-c-file #\L c-file))
	 (t
	  (translation-error
	    "Can't emit ~s as a C literal." literal-value)))))))




;;; The structure `c-str-init-expr' represents an initializer for a C Str_X
;;; (constant string) type.  These are emitted from c-var-decls.

(defconstant maximum-inline-c-constant-string-length 120)

(def-c-expr c-str-init-expr (17 string)
  (let* ((string (c-str-init-expr-string expr))
	 (length (length string))
	 (printed-length (write-integer-to-string length)))
    (format-c-expr "{ %S, %S, %S, " expr c-file indent
		   (write-integer-to-string (c-type-tag 'str))
		   printed-length
		   printed-length)
    (emit-indentation-to-c-file-if-necessary c-file string indent)
    (cond
      ((< length maximum-inline-c-constant-string-length)
       (emit-character-to-c-file #\" c-file)
       (emit-string-to-c-file
	 (transform-to-c-string-constant string)
	 c-file)
       (emit-character-to-c-file #\" c-file))
      (t
       (loop for index fixnum from 0 below length
	     for char = (schar string index)
	     initially (emit-string-to-c-file "{ " c-file)
	     do
	 (when (/= index 0)
	   (emit-character-to-c-file #\, c-file)
	   (emit-indentation-to-c-file-if-necessary
	     c-file nil indent))
	 (emit-character-to-c-file #\' c-file)
	 (if (and (unquoted-c-string-char-p char)
		  (not (char= char #\')))
	     (emit-character-to-c-file char c-file)
	     (let ((escape-code? (c-escape-char-for-char? char)))
	       (emit-character-to-c-file #\\ c-file)
	       (if escape-code?
		   (emit-character-to-c-file escape-code? c-file)
		   (emit-string-to-c-file
		     (format nil "~3,'0o" (char-code char))
		     c-file))))
	 (emit-character-to-c-file #\' c-file)
	     finally
	       (emit-string-to-c-file ", '\\000' }" c-file))))
    (emit-string-to-c-file " }" c-file)))




;;; The structures for `c-uint8-init-expr', `c-uint16-init-expr',
;;; `c-sint16-init-expr' represent initializers for arrays of unsigned byte 8,
;;; unsigned byte 16, and signed byte 16 values.

(def-c-expr c-uint8-init-expr (17 array)
  (emit-integer-vector-to-c-file
    expr (c-uint8-init-expr-array expr) 'sa-uint8 c-file indent))

(def-c-expr c-sint16-init-expr (17 array)
  (emit-integer-vector-to-c-file
    expr (c-sint16-init-expr-array expr) 'sa-sint16 c-file indent))

(def-c-expr c-uint16-init-expr (17 array)
  (emit-integer-vector-to-c-file
    expr (c-uint16-init-expr-array expr) 'sa-uint16 c-file indent))

(defun emit-integer-vector-to-c-file (expr array c-type c-file indent)
  (let* ((length (length array))
	 (length-string (write-integer-to-string length)))
    (format-c-expr "{%S, %S, " expr c-file indent
		   (write-integer-to-string (c-type-tag c-type))
		   length-string)
    ;; Both of the unsigned-byte array types have fill pointers, initialize it
    ;; to the same value as the length.
    (emit-string-to-c-file length-string c-file)
    (emit-string-to-c-file ", " c-file)
    (emit-character-to-c-file #\{ c-file)
    ;; ANSI C requires you have at least one element in the curly braces.
    (if (= length 0)
	(emit-string-to-c-file "0" c-file)
	(loop for index from 0 below length do
	  (when (/= index 0)
	    (emit-string-to-c-file ", " c-file)
	    (emit-indentation-to-c-file-if-necessary c-file nil indent))
	  (emit-string-to-c-file
	    (write-integer-to-string (aref array index))
	    c-file)))
    (emit-string-to-c-file "}}" c-file)))




;;; The `c-double-init-expr' represents an initialization form for a constant
;;; array of double-floats, which is a Sa_double array in C.

(def-c-expr c-double-init-expr (17 array)
  (let* ((array (c-double-init-expr-array expr))
	 (length (length array)))
    (format-c-expr "{%S, %S, {" expr c-file indent
		   (write-integer-to-string (c-type-tag 'sa-double))
		   (write-integer-to-string length))
    (if (= length 0)
	(emit-string-to-c-file "0.0" c-file)
	(loop for index from 0 below length do
	  (when (/= index 0)
	    (emit-string-to-c-file ", " c-file)
	    (emit-indentation-to-c-file-if-necessary c-file nil indent))
	  (emit-string-to-c-file
	    (format nil "~a" (aref array index))
	    c-file)))
    (emit-string-to-c-file "}}" c-file)))




;;; The `c-sv-init-expr' represents an initialization form for a constant
;;; simple-vector.  It contains the length and a c-array-init-expr that will
;;; initialize the body of the simple-vector.

(def-c-expr c-sv-init-expr (17 length init-expr)
  (format-c-expr "{%S, %S, %E}" expr c-file indent
		 (write-integer-to-string (c-type-tag 'sv))
		 (write-integer-to-string (c-sv-init-expr-length expr))
		 (c-sv-init-expr-init-expr expr)))



;;; The `c-array-init-expr' represents an initialization form for an array of
;;; type Obj.  The argument to this form is a simple vector containing c-exprs
;;; that will initialize the contents of this array.  This is used to initialize
;;; both cons trees and to initialize the body of simple-vectors.

(def-c-expr c-array-init-expr (17 c-expr-array)
  (let* ((array (c-array-init-expr-c-expr-array expr))
	 (length (length array)))
    (emit-character-to-c-file #\{ c-file)
    (if (= length 0)
	;; ANSI C requires at least one element between the braces.
	(emit-string-to-c-file "(Obj)NULL" c-file)
	(loop for index fixnum from 0 below length
	      for init-expr = (svref array index)
	      do
	  (unless (zerop index)
	    (emit-string-to-c-file ", " c-file)
	    (emit-indentation-to-c-file-if-necessary c-file init-expr indent))
	  (emit-expression-to-c-file init-expr c-file indent)))
    (emit-character-to-c-file #\} c-file)))




;;; The `c-ldouble-init-expr' structure represents an initialization form for an
;;; Ldouble structure.  It holds one extra value, the double-float needed.

(def-c-expr c-ldouble-init-expr (17 double-float)
  (let ((double (c-ldouble-init-expr-double-float expr)))
    (format-c-expr "{%S, %S}" expr c-file indent
		   (write-integer-to-string (c-type-tag 'ldouble))
		   (format nil "~A" double))))



;;; The `c-paren-expr' represents an explicitly parenthesized expression.  Since
;;; the C expression emitter will automatically insert enough parentheses to
;;; correctly group operands, this should almost never be necessary, but it is
;;; included anyway.

(def-c-expr c-paren-expr (17 sub-expr)
  (format-c-expr "%I(%E)" expr c-file indent
		 (c-paren-expr-sub-expr expr)
		 (c-paren-expr-sub-expr expr)))




;;; The `c-subscript-expr' structure represents a reference to an element of an
;;; array.

(def-c-expr c-subscript-expr (-17 array-expr index-expr)
  (format-c-expr "%I%L[%E]" expr c-file indent
		 (c-subscript-expr-array-expr expr)
		 (c-subscript-expr-array-expr expr)
		 (c-subscript-expr-index-expr expr)))




;;; The `c-direct-selection-expr' represents a reference to an element of a
;;; struct, where the struct reference is to the struct itself, not to a pointer
;;; to the struct.

(def-c-expr c-direct-selection-expr (-17 struct-expr slot-name-string)
  (format-c-expr "%I%L.%S" expr c-file indent
		 (c-direct-selection-expr-struct-expr expr)
		 (c-direct-selection-expr-struct-expr expr)
		 (c-direct-selection-expr-slot-name-string expr)))




;;; The `c-indirect-selection-expr' represents a reference to an element of a
;;; struct, where the struct reference is to a pointer to the struct.

(def-c-expr c-indirect-selection-expr (-17 struct-ptr-expr slot-name-string)
  (format-c-expr "%I%L->%S" expr c-file indent
		 (c-indirect-selection-expr-struct-ptr-expr expr)
		 (c-indirect-selection-expr-struct-ptr-expr expr)
		 (c-indirect-selection-expr-slot-name-string expr)))




;;; The `c-function-call-expr' represents a call to a C function.  It is also
;;; used to represent calls to C macros, since they have the same emitting
;;; characteristics.

(defun list-of-c-exprs (arglist-exprs)
  (loop for arg-cons = arglist-exprs then (cons-cdr arg-cons)
	while arg-cons
	always (and (consp arg-cons) (c-expr-p (cons-car arg-cons)))))

(def-c-expr c-function-call-expr
    (-17 function-expr (arglist-exprs :test list-of-c-exprs))
  (format-c-expr "%I%L(%A)" expr c-file indent
		 (c-function-call-expr-function-expr expr)
		 (c-function-call-expr-function-expr expr)
		 (c-function-call-expr-arglist-exprs expr)))




;;; The `c-postfix-inc-expr' and `c-postfix-dec-expr' represent postfix
;;; increment or decrement expressions.  As a reminder, the postfix versions
;;; increment or decrement the lvalue given as an argument and return the
;;; original value of the argument before modification.

(def-c-expr c-postfix-inc-expr (-17 location-expr)
  (format-c-expr "%L++" expr c-file indent
		 (c-postfix-inc-expr-location-expr expr)))

(def-c-expr c-postfix-dec-expr (-17 location-expr)
  (format-c-expr "%L--" expr c-file indent
		 (c-postfix-dec-expr-location-expr expr)))

(defmacro make-c-postfix-expr (location-expr op-string)
  (if (constantp op-string)
      (cond ((string= op-string "++")
	     `(make-c-postfix-inc-expr ,location-expr))
	    ((string= op-string "--")
	     `(make-c-postfix-dec-expr ,location-expr))
	    (t
	     (error "~s is not a valid C postfix operator, need ++ or --"
		    op-string)))
      (error "Make-c-postfix-expr needs a constant op-string, not ~s"
	     op-string)))




;;; The `c-sizeof-expr' represents a call to sizeof.  This looks like a function
;;; call, but CaRM4 says that its precedence is different.  Also, the argument
;;; is a type string, not an expression argument list.

(def-c-expr c-sizeof-expr (15 type-string)
  (format-c-expr "%Isizeof(%S)" expr c-file indent
		 (c-sizeof-expr-type-string expr)
		 (c-sizeof-expr-type-string expr)))




;;; The `c-unary-expr' represents all one character unary expressions in C.
;;; This stucture holds a character that is the unary operator and an argument
;;; expression.  The operations that can be emitted into this structure are:

;;;   ~  bitwise not,
;;;   !  logical not,
;;;   -  arithmetic negation,
;;;   +  arithmetic plus (a no-op useless operator),
;;;   &  address of,
;;;   *  indirection, sometimes called value of,

(def-c-expr c-unary-expr (15 (op-char :test characterp) arg-expr)
  (format-c-expr "%I%C%R" expr c-file indent
		 (c-unary-expr-arg-expr expr)
		 (c-unary-expr-op-char expr)
		 (c-unary-expr-arg-expr expr)))




;;; The `c-prefix-inc-expr' and `c-prefix-dec-expr' structures represent prefix
;;; increment and decrement expressions, where the return value is the argument
;;; value after it is has been incremented or decremented.  These are the
;;; natural implementations of incff and decff.

(def-c-expr c-prefix-inc-expr (15 location-expr)
  (format-c-expr "++%R" expr c-file indent
		 (c-prefix-inc-expr-location-expr expr)))

(def-c-expr c-prefix-dec-expr (15 location-expr)
  (format-c-expr "--%R" expr c-file indent
		 (c-prefix-dec-expr-location-expr expr)))

(defmacro make-c-prefix-expr (op-string location-expr)
  (if (constantp op-string)
      (cond ((string= op-string "++")
	     `(make-c-prefix-inc-expr ,location-expr))
	    ((string= op-string "--")
	     `(make-c-prefix-dec-expr ,location-expr))
	    (t
	     (error "~s is not a valid C prefix operator, need ++ or --"
		    op-string)))
      (error "Make-c-prefix-expr needs a constant op-string, not ~s"
	     op-string)))




;;; The `c-cast-expr' represents a type cast operation in C.  It holds a C type
;;; and an argument to be casted.

(def-c-expr c-cast-expr (14 c-type arg-expr)
  (let ((type-string (c-type-string (c-cast-expr-c-type expr))))
    (when (null type-string)
      (error "Undefined C type ~s in c-cast-expr." (c-cast-expr-c-type expr)))
    (format-c-expr "%I(%S)%R" expr c-file indent
		   type-string
		   type-string
		   (c-cast-expr-arg-expr expr))))




;;; The `c-mult-expr' represents all of the multiplier and divisor operations.
;;; Note that this can take a character or a string argument.  The operations
;;; are:

;;;   *  multiplication,
;;;   /  division, and
;;;   %  remainder.

(def-c-expr c-mult-expr (13 left-arg op-char right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-mult-expr-left-arg expr)
    (c-mult-expr-op-char expr)
    (c-mult-expr-right-arg expr)))




;;; The `c-add-expr' represents all of the additive expressions.  There are, of
;;; course, only two + and -.  Note that this can takes either a character or a
;;; string as the op-char argument.

(def-c-expr c-add-expr (12 left-arg op-char right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-add-expr-left-arg expr)
    (c-add-expr-op-char expr)
    (c-add-expr-right-arg expr)))




;;; The `c-shift-expr' structure represents the two shifting operations.  They
;;; are left-shift << and right-shift >>.

(def-c-expr c-shift-expr (11 left-arg (op-string :test stringp) right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-shift-expr-left-arg expr)
    (c-shift-expr-op-string expr)
    (c-shift-expr-right-arg expr)))




;;; The `c-relational-expr' structure represents non-equality numeric comparitor
;;; operations.  They are <, >, <=, and >=.

(def-c-expr c-relational-expr (10 left-arg (op-string :test stringp) right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-relational-expr-left-arg expr)
    (c-relational-expr-op-string expr)
    (c-relational-expr-right-arg expr)))




;;; The `c-equality-expr' structure represents equal and not equal numeric
;;; comparitors, == and !=.

(def-c-expr c-equality-expr (9 left-arg (op-string :test stringp) right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-equality-expr-left-arg expr)
    (c-equality-expr-op-string expr)
    (c-equality-expr-right-arg expr)))




;;; The bitwise logical operators all have different precedence, but are
;;; otherwise identical.  The `c-bitwise-and-expr' structure represents the &
;;; op.  The `c-bitwise-xor-expr' represents the ^ op.  The `c-bitwise-or-expr'
;;; represents the | op.

(def-c-expr c-bitwise-and-expr (8 left-arg right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-bitwise-and-expr-left-arg expr)
    #\&
    (c-bitwise-and-expr-right-arg expr)))

(def-c-expr c-bitwise-xor-expr (7 left-arg right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-bitwise-xor-expr-left-arg expr)
    #\^
    (c-bitwise-xor-expr-right-arg expr)))

(def-c-expr c-bitwise-or-expr (6 left-arg right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-bitwise-or-expr-left-arg expr)
    #\|
    (c-bitwise-or-expr-right-arg expr)))




;;; The `c-logical-and-expr' and `c-logical-or-expr' structures represent the &&
;;; and || operators

(def-c-expr c-logical-and-expr (5 left-arg right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-logical-and-expr-left-arg expr)
    " && "
    (c-logical-and-expr-right-arg expr)))

(def-c-expr c-logical-or-expr (4 left-arg right-arg)
  (emit-infix-expr-to-c-file
    expr c-file indent
    (c-logical-or-expr-left-arg expr)
    " || "
    (c-logical-or-expr-right-arg expr)))




;;; The `c-conditional-expr' represents an embedded if-then-else expression
;;; within a statement.

(def-c-expr c-conditional-expr (-3 logical-expr then-expr else-expr)
  (format-c-expr "%I%L %I? %L %I: %R" expr c-file indent
		 (c-conditional-expr-logical-expr expr)
		 (c-conditional-expr-logical-expr expr)
		 (c-conditional-expr-then-expr expr)
		 (c-conditional-expr-then-expr expr)
		 (c-conditional-expr-else-expr expr)
		 (c-conditional-expr-else-expr expr)))




;;; The `c-assignment-expr' structure represents all of the assignment operators
;;; in C.  They are =, +=, -=, *=, /= %=, <<=, >>=, &=, ^=, and |=.

(def-c-expr c-assignment-expr (-2 left-arg op-string right-arg)
  (format-c-expr "%I%L %I%S %R" expr c-file indent
		 (c-assignment-expr-left-arg expr)
		 (c-assignment-expr-left-arg expr)
		 (c-assignment-expr-right-arg expr)
		 (c-assignment-expr-op-string expr)
		 (c-assignment-expr-right-arg expr)))




;;; The `c-comma-expr' structure represents comma expressions, or sequential
;;; expression evaluation.  When emitting comma expressions, we always emit a
;;; set of parens around them to avoid any possible confusion with the commas
;;; separating function arguments.  Hopefully this will be used seldom enough
;;; that it won't possibly be an issue.

(def-c-expr c-comma-expr (1 arg-list)
  (format-c-expr "(%A)" expr c-file indent (c-comma-expr-arg-list expr)))




;;; The `c-line-comment-expr' structure is a fake C expression that really holds
;;; a subexpression and a comment that should be placed onto the same line as
;;; the end of the emitted held expression.  This is used in situations such as
;;; to comment slot names where only an index into an SVREF remains.

(def-c-expr c-line-comment-expr (1 held-expr comment-string)
  (emit-expression-to-c-file
    (c-line-comment-expr-held-expr expr) c-file indent)
  (emit-line-comment-to-c-file
    (c-line-comment-expr-comment-string expr) c-file))




;;; The `c-#if-expr' structure holds a set of alternating ifdef expressions and
;;; c-expressions.  Each of the ifdef expressions gets translated as a "#if x"
;;; on a new line where x is the expression, then the C expression is emitted at
;;; the current indentation level, then the next ifdef expression gets
;;; translated on a new line as "#else x", followed by the next c-expression.
;;; If the last ifdef expression is null, then there will be no condition on the
;;; final else clause.

(def-c-expr c-#if-expr (1 if-and-c-exprs)
  (format-c-expr "%N#if %E%N" expr c-file indent
		 (car (c-#if-expr-if-and-c-exprs expr)))
  (emit-indentation-to-c-file c-file indent)
  (format-c-expr "%E%N" expr c-file indent
		 (second (c-#if-expr-if-and-c-exprs expr)))
  (loop for (else-if? next-expr) on (cddr (c-#if-expr-if-and-c-exprs expr))
				 by #'cddr
	do
    (cond
      (else-if?
       (emit-string-to-c-file "#elif" c-file)
       (emit-newline-to-c-file c-file)
       (emit-indentation-to-c-file c-file indent)
       (format-c-expr "%E%N" expr c-file indent else-if?))
      (t
       (emit-string-to-c-file "#else" c-file)
       (emit-newline-to-c-file c-file)))
    (emit-indentation-to-c-file c-file indent)
    (format-c-expr "%E%N" expr c-file indent next-expr))
  (emit-string-to-c-file "#endif" c-file)
  (emit-newline-to-c-file c-file)
  (emit-indentation-to-c-file c-file indent))





;;;; C Infix Expressions




;;; As a convenience, all of the different types of infix C expression given
;;; above can be created using the macro `make-c-infix-expr'.  It takes a left
;;; arg, an infix operator string, and a right arg.

(defconstant c-expr-infix-operator-types
  ;; Op strings          C-expr type        Requires op string arg?
  '((("*" "/" "%")       c-mult-expr        t)
    (("+" "-")           c-add-expr         t)
    (("<<" ">>")         c-shift-expr       t)
    (("<" ">" "<=" ">=") c-relational-expr  t)
    (("==" "!=")         c-equality-expr    t)
    (("&")               c-bitwise-and-expr nil)
    (("^")               c-bitwise-xor-expr nil)
    (("|")               c-bitwise-or-expr  nil)
    (("&&")              c-logical-and-expr nil)
    (("||")              c-logical-or-expr  nil)
    (("=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|=")
                         c-assignment-expr  t)))

(defmacro make-c-infix-expr (left-arg op-string right-arg)
  (if (constantp op-string)
      (loop for type in c-expr-infix-operator-types do
	(when (member op-string (car type) :test #'string=)
	  (return
	    (let ((make-func (intern (format nil "MAKE-~a" (second type))))
		  (new-left-arg
		    (cond
		      ((fixnump left-arg)
		       `(make-c-literal-expr ,left-arg))
		      ((stringp left-arg)
		       `(make-c-name-expr ,left-arg))
		      ((and (consp left-arg)
			    (memq (cons-car left-arg)
				  '(make-c-name-expr make-c-literal-expr
				    make-c-infix-expr make-c-add-expr)))
		       left-arg)
		      (t
		       `(make-c-name-or-literal-expr-if-necessary ,left-arg))))
		  (new-right-arg
		    (cond
		      ((fixnump right-arg)
		       `(make-c-literal-expr ,right-arg))
		      ((stringp right-arg)
		       `(make-c-name-expr ,right-arg))
		      ((and (consp right-arg)
			    (memq (cons-car right-arg)
				  '(make-c-name-expr make-c-literal-expr
				    make-c-infix-expr make-c-add-expr)))
		       right-arg)
		      (t
		       `(make-c-name-or-literal-expr-if-necessary ,right-arg)))))
	      (if (third type)
		  `(,make-func ,new-left-arg ,op-string ,new-right-arg)
		  `(,make-func ,new-left-arg ,new-right-arg)))))
	    finally
	      (error "~s is not a valid infix op-string" op-string))
      (error "Only constant op-strings can be given to make-c-infix-expr, not ~s"
	     op-string)))

(defun make-c-name-or-literal-expr-if-necessary (value)
  (cond ((fixnump value)
	 (make-c-literal-expr value))
	((stringp value)
	 (make-c-name-expr value))
	((c-expr-p value)
	 value)
	(t
	 (translation-error
	   "Can't transform ~s into a c-expr." value))))
       








;;;; C Expression Nesting



;;; The macros `left-arg-needs-parens-p' and `right-arg-needs-parens-p' are used
;;; to determine whether a surrounding expression needs to wrap a subexpression
;;; within parens to force appropriate operator grouping.  For example,
;;; "(a*b)+c" doesn't really need the parentheses to enforce a and b being
;;; combined using * before c is added to the result because multiplication has
;;; a higher precedence than addition.  However, "a*(b+c)" does require
;;; parentheses.

;;; In the first example, the + expression would be the surrounding expression
;;; and the * would be the subexpression when calling left-arg-needs-parens-p.
;;; In general, left-arg-needs-parens-p will return false when the subexpression
;;; is a simple token or when its precedence is higher or equal to the
;;; precedence of the surrounding expression.  It is implemented conservatively
;;; so that it never yeilds a false negative.

;;; In the second example, * would be the surrounding expression and + would be
;;; the subexpression when calling right-arg-needs-parens-p.  In general,
;;; right-arg-needs-parens-p will return false when the subexpression is a
;;; simple token or when its precedence is lower than the precedence of the
;;; surrounding expression.  This function is also implemented conservatively so
;;; that it never yeilds a false negative.

(defmacro left-arg-needs-parens-p (expression sub-expression)
  (let ((expr (gensym))
	(sub-expr (gensym)))
    `(let ((,expr ,expression)
	   (,sub-expr ,sub-expression))
       (if (c-line-comment-expr-p ,sub-expr)
	   (left-arg-needs-parens-p-1
	     ,expr (c-line-comment-expr-held-expr ,sub-expr))
	   (and (not (c-name-expr-p ,sub-expr))
		(not (c-literal-expr-p ,sub-expr))
		(not (c-function-call-expr-p ,sub-expr))
		(not (c-paren-expr-p ,sub-expr))
		(not (c-direct-selection-expr-p ,sub-expr))
		(not (c-indirect-selection-expr-p ,sub-expr))
		;; the following isn't working correctly, I'll go with the
		;; add-hoc approach for now.  -jra 1/16/96
;                (not (c-expr-left-associative-p ,sub-expr))
;                (not (>= (the fixnum (c-expr-precedence ,sub-expr))
;                         (the fixnum (c-expr-precedence ,expr))))
		)))))

(defun left-arg-needs-parens-p-1 (expr sub-expr)
  (left-arg-needs-parens-p expr sub-expr))

(defmacro right-arg-needs-parens-p (expression sub-expression)
  (let ((expr (gensym))
	(sub-expr (gensym)))
    `(let ((,expr ,expression)
	   (,sub-expr ,sub-expression))
       (if (c-line-comment-expr-p ,sub-expr)
	   (right-arg-needs-parens-p-1
	     ,expr (c-line-comment-expr-held-expr ,sub-expr))
	   (not (or (c-name-expr-p ,sub-expr)
		    (c-function-call-expr-p ,sub-expr)
		    (and (c-literal-expr-p ,sub-expr)
			 (or (not (fixnump (c-literal-expr-literal-value ,sub-expr)))
			     (>= (the fixnum (c-literal-expr-literal-value
					       ,sub-expr))
				 0)))
		    (c-cast-expr-p ,sub-expr)
		    (c-paren-expr-p ,sub-expr)))))))

;; The following isn't working correctly, bag it and go with the add-hoc approach.  -jra 1/16/96
;;                (not (c-expr-right-associative-p ,sub-expr))
;;                (not (< (the fixnum (c-expr-precedence ,sub-expr))
;;                        (the fixnum (c-expr-precedence ,expr))))


(defun right-arg-needs-parens-p-1 (expr sub-expr)
  (right-arg-needs-parens-p expr sub-expr))




;;; The function `emit-left-expression-to-c-file' takes a surrounding C
;;; expression, a C expression to emit which is the left-hand argument to an
;;; infix binary operator or the argument to a postfix operator, a C file, and
;;; an indentation level.  This function will emit the C expression, wrapping it
;;; within parentheses if necessary to ensure proper grouping within it versus
;;; the surrounding expression.

(defun emit-left-expression-to-c-file
    (surrounding-expression sub-expression c-file indent)
  (let ((parens?
	  (left-arg-needs-parens-p surrounding-expression sub-expression)))
    (when parens?
      (emit-character-to-c-file #\( c-file))
    (emit-expression-to-c-file sub-expression c-file indent)
    (when parens?
      (emit-character-to-c-file #\) c-file))))




;;; The function `emit-right-expression-to-c-file' takes a surrounding C
;;; expression, a C expression to emit which is the right-hand argument to an
;;; infix binary operator or the argument to a prefix operator, a C file, and an
;;; indentation level.  This function will emit the C expression, wrapping it
;;; within parentheses if necessary to ensure proper grouping within it versus
;;; the surrounding expression.

(defun emit-right-expression-to-c-file
    (surrounding-expression sub-expression c-file indent)
  (let ((parens?
	  (right-arg-needs-parens-p surrounding-expression sub-expression)))
    (when parens?
      (emit-character-to-c-file #\( c-file))
    (emit-expression-to-c-file sub-expression c-file indent)
    (when parens?
      (emit-character-to-c-file #\) c-file))))




;;; The function `side-effect-free-c-expr-p' takes a C-expr and returns a best
;;; guess about whether or not that c-expr produces any side-effects.  If this
;;; function returns true, then you can be assured that no side-effects can
;;; arise.  If it returns NIL, then either it returns side-effects or was too
;;; complex to determine.

(defun side-effect-free-c-expr-p (c-expr)
  (or (c-name-expr-p c-expr)
      (c-literal-expr-p c-expr)
      (and (c-cast-expr-p c-expr)
	   (let ((sub-expr (c-cast-expr-arg-expr c-expr)))
	     (if (eq (c-cast-expr-c-type c-expr) 'void)
		 (if (c-name-expr-p sub-expr)
		     (string= (c-name-expr-name sub-expr) "NULL")
		     (side-effect-free-c-expr-p sub-expr))
		 (side-effect-free-c-expr-p sub-expr))))
      (and (c-indirect-selection-expr-p c-expr)
	   (side-effect-free-c-expr-p
	     (c-indirect-selection-expr-struct-ptr-expr c-expr)))
      (and (c-unary-expr-p c-expr)
	   (side-effect-free-c-expr-p (c-unary-expr-arg-expr c-expr)))
      (and (c-subscript-expr-p c-expr)
	   (side-effect-free-c-expr-p (c-subscript-expr-array-expr c-expr))
	   (side-effect-free-c-expr-p (c-subscript-expr-index-expr c-expr)))
      (and (c-line-comment-expr-p c-expr)
	   (side-effect-free-c-expr-p (c-line-comment-expr-held-expr c-expr)))))





;;; The function `c-unbound-value-expr' creates and returns a C-expr that when
;;; evaluated will return the value that unbound-variables are initialized to,
;;; i.e. the unbound value.  This is used for variables and for the return
;;; values of void functions that are funcalled.

(defun c-unbound-value-expr ()
  (make-c-cast-expr
    'obj (make-c-unary-expr #\& (make-c-name-expr "Unbound"))))
