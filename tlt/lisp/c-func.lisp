(in-package "TLI")

;;;; Module C-FUNC

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






;;;; C Functions




;;; This module implements structures and operations on C functions.

;;; The structure `c-func' represents C functions during a translation.  C
;;; functions have a list of storage class specifier strings, a C return type,
;;; a C identifier name, a Lisp symbol name of the function they are a
;;; translation of, a list of parameter name strings, a corresponding list of
;;; parameter C types, and C compound statement as a body.  The operations on
;;; these structures are make-c-func, emit-function-to-c-file,
;;; c-decl-for-function.

(defstruct (c-func
	     (:constructor
	      make-c-func
	      (storage-classes c-return-type identifier lisp-func-spec?
			       parameter-names c-parameter-types
			       c-parameter-storage-classes
			       body-statement namespace c-file)))
  storage-classes
  c-return-type
  identifier
  lisp-func-spec?
  parameter-names
  c-parameter-types
  c-parameter-storage-classes
  body-statement
  namespace
  c-file
  (reusable-c-variables nil)
  (c-variables-scheduled-for-reuse nil))




;;; The function `emit-function-to-c-file' takes a C-func structure and writes
;;; it to a C file.

(defun emit-function-to-c-file (c-func c-file)
  ;; Blank line before.
  (emit-newline-to-c-file c-file)
  ;; Comment of which Lisp function it came from, if any.
  (when (c-func-lisp-func-spec? c-func)
    (emit-string-to-c-file
     (let ((*print-pretty* nil))
       (format nil "/* Translated from ~a~a = ~a */"
	       (first (c-func-lisp-func-spec? c-func))
	       (or (second (c-func-lisp-func-spec? c-func)) "()")
	       (third (c-func-lisp-func-spec? c-func))))
      c-file)
    (emit-newline-to-c-file c-file)
    (emit-newline-to-c-file c-file))
  (loop for string in (c-func-storage-classes c-func) do
    (emit-string-to-c-file string c-file)
    (emit-character-to-c-file #\space c-file))
  (let ((function-type-string (c-type-string (c-func-c-return-type c-func))))
    (emit-string-to-c-file function-type-string c-file)
    (unless (char= (char function-type-string (1- (length function-type-string)))
		   #\*)
      (emit-character-to-c-file #\space c-file)))
  (emit-string-to-c-file (c-func-identifier c-func) c-file)
  (emit-string-to-c-file " (" c-file)
  (if (c-func-parameter-names c-func)
      (loop for first? = t then nil
	    for name in (c-func-parameter-names c-func)
	    for c-type in (c-func-c-parameter-types c-func)
	    for c-type-string = (c-type-string c-type)
	    for storage-classes in (c-func-c-parameter-storage-classes c-func)
	    do
	(unless first?
	  (emit-string-to-c-file ", " c-file)
	  (emit-indentation-to-c-file-if-necessary c-file nil 2))
	(loop for storage-class in storage-classes do
	  (emit-string-to-c-file storage-class c-file)
	  (emit-character-to-c-file #\space c-file))
	(emit-string-to-c-file c-type-string c-file)
	(unless (char= (char c-type-string (1- (length c-type-string))) #\*)
	  (emit-character-to-c-file #\space c-file))
	(emit-string-to-c-file name c-file))
      (emit-string-to-c-file "void" c-file))
  (emit-character-to-c-file #\) c-file)
  (emit-newline-to-c-file c-file)
  (emit-statement-to-c-file (c-func-body-statement c-func) c-file 1))






;;;; C Local Variables




;;; The function `lexical-c-variable-identifier' is used to get a C variable for
;;; use within the translation of a function.  The arguments to this function
;;; are a Lisp symbol from which the name of the C variable should be derived,
;;; the c-func within which the variable should be allocated, the C type of the
;;; variable, and a list of storage-classes for the variable (typically either
;;; NIL or '("volatile").

(defun lexical-c-variable-identifier (lisp-symbol c-func c-type storage-classes)
  (let* ((base-name (c-base-string lisp-symbol))
	 (base-name-length (length base-name))
	 (new-identifier? nil))
    (declare (fixnum base-name-length))
    (when (null c-type)
      (translation-error "Bad c-type given for variable ~s" lisp-symbol))
    (unless storage-classes
      (loop for entry in (c-func-reusable-c-variables c-func)
	    for identifier = (cons-car entry)
	    for type = (cons-cdr entry)
	    do
	(when (and (equal type c-type)
		   (>= (length identifier) base-name-length)
		   (loop for index fixnum from 0 below base-name-length
			 always (char= (schar base-name index)
				       (schar identifier index)))
		   (loop for index fixnum from base-name-length
				   below (length identifier)
			 always (digit-char-p (schar identifier index))))
	  (setq new-identifier? identifier)
	  (setf (c-func-reusable-c-variables c-func)
		(delq entry (c-func-reusable-c-variables c-func)))
	  (return nil))))
    (unless new-identifier?
      (setq new-identifier?
	    (c-identifier-for-string
	      base-name '(variable)
	      (c-func-namespace c-func) (c-func-namespace c-func)))
      (emit-declaration-to-compound-statement
	(make-c-var-decl storage-classes c-type new-identifier? nil)
	(c-func-body-statement c-func)))
    new-identifier?))




;;; The macro `reclaim-reusable-c-variables' takes a list of C variable
;;; specifications, where the entries in the list are conses whose car is the C
;;; identifier string for the variable and whose cdr is the C type for the
;;; variable.  These variables are not put into the list of reusable C variables
;;; immediately, since the translating function that allocated these variables
;;; generally returns a C expression that uses their values.  These variables
;;; can be reused as soon as the next C expression translation that discards its
;;; results is executed.  At these points, the macro
;;; `return-reusable-c-variables-to-c-func-pool' should be called.  This can
;;; occur, for example, after each statement in a progn is translated, except
;;; for the last.

(defmacro reclaim-reusable-c-variables (c-func list-of-id-type-pairs)
  (let ((c-function (gensym)))
    `(let ((,c-function ,c-func))
       (setf (c-func-c-variables-scheduled-for-reuse ,c-function)
	     (nconc (c-func-c-variables-scheduled-for-reuse ,c-function)
		    ,list-of-id-type-pairs)))))

(defmacro return-reusable-c-variables-to-c-func-pool (c-func)
  (if (symbolp c-func)
      `(when (c-func-c-variables-scheduled-for-reuse ,c-func)
	 (setf (c-func-reusable-c-variables ,c-func)
	       (nconc (c-func-reusable-c-variables ,c-func)
		      (c-func-c-variables-scheduled-for-reuse ,c-func)))
	 (setf (c-func-c-variables-scheduled-for-reuse ,c-func) nil))
      (let ((c-function (gensym)))
	`(let ((,c-function ,c-func))
	   (return-reusable-c-variables-to-c-func-pool ,c-function)))))




;;; The function `reusable-c-variable-identifier' can be used instead of
;;; lexical-c-variable-identifier in cases where the reclaimation of the
;;; allocated identifier will happen with the same c-variables-scope as the call
;;; to the allocator.  What this means is that it is generally safe (and
;;; convenient) to use this interface instead of lexical-c-variables-identifier
;;; within def-c-translation forms or within forms that do not themselves call
;;; with-reusable-c-variables-scope.

(defun reusable-c-variable-identifier (lisp-symbol c-func c-type env)
  (let ((new-var (lexical-c-variable-identifier
		   lisp-symbol c-func c-type
		   (storage-classes-for-env env))))
    (reclaim-reusable-c-variables c-func (list (cons new-var c-type)))
    new-var))



;;; The macro `with-reusable-c-variables-scope' takes a c-function and a body of
;;; code.  It executes the body of code in a scope where any C variables
;;; scheduled for reuse are placed back into the pool of reusable C variables.
;;; It returns all values from the last form in its body.  This macro should be
;;; wrapped around scopes in which code is being executed for side-effect and
;;; all result values from the code will be discarded.

(defmacro with-reusable-c-variables-scope ((c-func) &body body)
  (let ((c-function (gensym))
	(previous-scheduled-variables (gensym)))
    `(let* ((,c-function ,c-func)
	    (,previous-scheduled-variables
	       (c-func-c-variables-scheduled-for-reuse ,c-function)))
       (setf (c-func-c-variables-scheduled-for-reuse ,c-function) nil)
       (multiple-value-prog1
	   (progn ,@body)
	 (return-reusable-c-variables-to-c-func-pool ,c-function)
	 (setf (c-func-c-variables-scheduled-for-reuse ,c-function)
	       ,previous-scheduled-variables)))))






;;;; C Statement Labels




;;; The function `lexical-c-statement-label' takes a symbol and a c-func that the
;;; label will be used within.  This macro returns a string in the function's
;;; namespace that can be used as a unique statement label within the function.

(defmacro lexical-c-statement-label (symbol c-func)
  (if (and (or (constantp symbol) (symbolp symbol))
	   (symbolp c-func))
      `(c-identifier-for-symbol
	 ,symbol '(label) (c-func-namespace ,c-func) (c-func-namespace ,c-func))
      (let ((sym (gensym))
	    (func (gensym)))
	`(let ((,sym ,symbol)
	       (,func ,c-func))
	   (c-identifier-for-symbol
	     ,sym '(label) (c-func-namespace ,func) (c-func-namespace ,func))))))
