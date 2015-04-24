(in-package "TLI")

;;;; Module C-TYPE-UTIL

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






;;;; C Type Definition Utilities




;;; This module implements defining forms for C type definitions, most of which
;;; are found in C-COERCE.

;;; The macro `c-type-coercion-function' takes a C type name symbol and returns
;;; a compiled function that can coerce a C-expr into the given type.  It is
;;; setfable.  The macro `c-pointer-type-coersion-function' takes a C type name
;;; symbol and returns a compiled function that can coerce a C-expr into a
;;; pointer to the given type.  It is setfable.

(defmacro c-type-coercion-function (c-type)
  `(get ,c-type :c-type-coercion-function))

(defmacro c-type-coercion-predicate-function (c-type)
  `(get ,c-type :c-type-coercion-predicate-function))

(defmacro c-pointer-type-coercion-function (c-type)
  `(get ,c-type :c-type-coersion-function))

(defmacro c-pointer-type-coercion-predicate-function (c-type)
  `(get ,c-type :c-pointer-type-coercion-predicate-function))




;;; The macro `def-c-type-coercion' implements C translations for coercions of C
;;; expression results to this type.  It takes a C type name, an argument list
;;; of one element for a C expression structure, and contains a body which looks
;;; like the body of a case statement.  The case keys should be C types returned
;;; by the expression given to this function.  The case clauses should return a
;;; C expression that contains the given C expression argument and coerces that
;;; argument from the case clause type into the target type.

(defmacro def-c-type-coercion (c-type (c-expr-var &optional c-type-var)
				      &body case-clauses)
  (let ((coercion-function-name (intern (format nil "COERCE-~a" c-type)))
	(predicate-function-name (intern (format nil "COERCE-~a-P" c-type)))
	(starting-type (or c-type-var (make-symbol "STARTING-C-TYPE"))))
    `(progn
       (defun ,coercion-function-name (,c-expr-var ,starting-type env)
	 (identity env)
	 (cond
	   ,@(loop for case-clause in case-clauses
		   collect
		   `(,(if (cdr (cons-car case-clause))
			  `(or ,@(loop for c-type in (cons-car case-clause)
				       collect `(satisfies-c-required-type-p
						  ,starting-type
						  ',c-type)))
			  `(satisfies-c-required-type-p
			     ,starting-type
			     ',(cons-car (cons-car case-clause))))
		      ,@(cons-cdr case-clause)))
	   ,@(if (not (memq 'void (caar (last case-clauses))))
		 `((t
		    (translation-error
		     "Can't coerce C type ~s into ~a" ,starting-type ',c-type)))
	       nil)))
       (setf (c-type-coercion-function ',c-type) #',coercion-function-name)
       (defun ,predicate-function-name (starting-type)
	 (loop for coercable-type in ',(loop for case-clause in case-clauses
					     append (cons-car case-clause))
	       thereis (satisfies-c-required-type-p
			 starting-type coercable-type)))
       (setf (c-type-coercion-predicate-function ',c-type)
	     #',predicate-function-name)
       ',c-type)))




;;; The macro `def-c-pointer-type-coercion' implements C translations for
;;; coercions of C expression results into pointers to this type.  It takes a C
;;; type name, an argument list of one element for a C expression, and contains
;;; a body which looks like the body of a case statement.  The case keys should
;;; be C types returned by the expression given to this function.  The case
;;; clauses should return a C expression that contains the given C expression
;;; argument and coerces that argument from the case clause type into a pointer
;;; to the target type.

(defmacro def-c-pointer-type-coercion (c-type (c-expr-var &optional c-type-var)
					      &body case-clauses)
  (let ((coercion-function-name (intern (format nil "COERCE-~a-POINTER" c-type)))
	(predicate-function-name (intern (format nil "COERCE-~a-PTR-P" c-type)))
	(starting-type (or c-type-var (make-symbol "STARTING-C-TYPE"))))
    `(progn
       (defun ,coercion-function-name (,c-expr-var ,starting-type env)
	 (identity env)
	 (cond
	   ,@(loop for case-clause in case-clauses
		   collect
		   `(,(if (cdr (cons-car case-clause))
			  `(or ,@(loop for c-type in (cons-car case-clause)
				       collect `(satisfies-c-required-type-p
						  ,starting-type
						  ',c-type)))
			  `(satisfies-c-required-type-p
			     ,starting-type
			     ',(cons-car (cons-car case-clause))))
		      ,@(cons-cdr case-clause)))
	   ,@(if (not (memq 'void (caar (last case-clauses))))
		 `((t
		    (translation-error
		     "Can't coerce C type ~s into a ~a pointer."
		     ,starting-type ',c-type)))
	       nil)))
       (setf (c-pointer-type-coercion-function ',c-type)
	     #',coercion-function-name)
       (defun ,predicate-function-name (starting-type)
	 (loop for coercable-type in ',(loop for case-clause in case-clauses
					     append (cons-car case-clause))
	       thereis (satisfies-c-required-type-p
			 starting-type coercable-type)))
       (setf (c-pointer-type-coercion-predicate-function ',c-type)
	     #',predicate-function-name)
       '(pointer ,c-type))))




;;; The function `coerce-c-expr-result-to-type' takes a c-expr, the C-type of
;;; its result, and a target C-type.  This function will surround the given
;;; c-expr with whatever coercion operations are needed to coerce from the
;;; current result type into the target result type.  If this cannot be done, a
;;; warning is issued and a call to error is returned.

(defun coerce-c-expr-result-to-type (c-expr c-type target-c-type env)
  (cond
    ((satisfies-c-required-type-p c-type target-c-type) c-expr)
    ((and (consp target-c-type)
	  (memqp (cons-car target-c-type) '(pointer array)))
     (let ((coerce-function
	     (c-pointer-type-coercion-function (cons-second target-c-type))))
       (cond (coerce-function
	      (funcall coerce-function c-expr c-type env))
	     (t
	      (translation-error
		"Can't coerce C type ~s into a ~s pointer."
		c-type target-c-type)
	      c-expr))))
    ((symbolp target-c-type)
     (let ((coerce-function (c-type-coercion-function target-c-type)))
       (cond (coerce-function
	      (funcall coerce-function c-expr c-type env))
	     (t
	      (translation-error
		"Can't coerce C type ~s into a ~s." c-type target-c-type)
	      c-expr))))
    (t
     (translation-error "Invalid target-c-type ~s." target-c-type))))

(defun c-type-can-be-coerced-p (current-type target-type)
  (or (satisfies-c-required-type-p current-type target-type)
      (and (consp target-type)
	   (memqp (cons-car target-type) '(pointer array))
	   (let ((predicate-function
		   (c-pointer-type-coercion-predicate-function
		     (cons-second target-type))))
	     (and predicate-function
		 (funcall predicate-function current-type))))
      (and (symbolp target-type)
	   (let ((predicate-function
		   (c-type-coercion-predicate-function target-type)))
	     (and predicate-function
		  (funcall predicate-function current-type))))))




;;; The function `most-specific-common-c-supertype' takes two C types and
;;; returns a C type that both argument types can be coerced to that is the most
;;; specific type in the intersection between them.  Of course the degenerate
;;; case is that they are equal and so one of them is returned.  If they are not
;;; equal or one of them is void, then a coercion operation to find a common
;;; type, therefore the return type at this point would be void.

(defun most-specific-common-c-supertype (c-type-1 c-type-2)
  (cond ((and (null c-type-1) (null c-type-2))
	 'void)
	((null c-type-1)
	 c-type-2)
	((null c-type-2)
	 c-type-1)
	((satisfies-c-required-type-p c-type-1 c-type-2)
	 c-type-2)
	(t
	 'void)))

(defmacro c-integer-types ()
  `'(sint32 uint8 uint16 sint16 unsigned-char))




;;; The function `default-value-for-c-type' takes a C type and returns a C expr
;;; that returns a default value for the given type.  This is generally used in
;;; situations where a stand-in expression is needed for an expression that
;;; could not supply the desired type.

(defun default-value-for-c-type (c-type)
  (cond ((loop for type in (c-integer-types)
	       thereis (satisfies-c-required-type-p c-type type))
	 (make-c-literal-expr 0))
	((satisfies-c-required-type-p c-type 'double)
	 (make-c-literal-expr 0.0))
	(t
	 (make-c-cast-expr c-type (make-c-name-expr "NULL")))))
