(in-package "TLI")

;;;; Module L-CONST

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






;;;; Constant Folding, Bit Setting, and Type Propagation




;;; This file contains l-expr walkers and methods for constant folding, setting
;;; constant, functional, and side-effect-free bits, and type propagation in
;;; l-exprs.

;;; The macro `set-l-expr-bits' is used during prepare-l-expr-for-translation
;;; (an l-expr walker) to set the l-expr bits.  It sets the functional,
;;; constant, and side-effect-free bits to their default values, then calls the
;;; set-specific-l-expr-bits method on the l-expr, returning the result of that
;;; method (so that constant folding can work).

(defmacro set-l-expr-bits (l-expr)
  (if (symbolp l-expr)
      `(progn
	 (clear-l-expr-bits ,l-expr)
	 (set-specific-l-expr-bits ,l-expr))
      (let ((l-expr-var (gensym)))
	`(let ((,l-expr-var ,l-expr))
	   (clear-l-expr-bits ,l-expr)
	   (set-specific-l-expr-bits ,l-expr)))))




;;; The generic l-expr method `set-specific-l-expr-bits' is used to perform two
;;; operations just after expressions have been transformed into l-exprs and
;;; just before type propagation is performed.  These operations are setting the
;;; bits on l-exprs that represent whether l-expr expressions are constants,
;;; functional, or side-effect-free.  L-exprs that are constants are the
;;; quote-l-expr and other operations whose values do not change over time and
;;; are not affected by other side-effecting operations.  The functional bit is
;;; set on l-exprs whose operations are side-effect free and values depend
;;; entirely on its arguments.  Note that being functional is entirely a
;;; property of the operation, and not of the expression as a whole.  Functional
;;; l-exprs with only constant argument l-exprs may be constant folded into a
;;; quote l-expr.  Side-effect-free l-exprs are those whose operations and
;;; argument l-exprs perform no mutations to any data.  They differ from
;;; functional l-exprs, in that the side-effect-free bit describes this l-expr
;;; and its argument l-exprs, and that side-effect free operations may use the
;;; values of global variables in computing their result.

(def-l-expr-generic-method set-specific-l-expr-bits (l-expr)
  l-expr)

(defun make-quoted-constant-l-expr (value env aug-env)
  (let ((quote-l-expr (make-quote-l-expr value env aug-env)))
    (initialize-l-expr quote-l-expr)
    (set-l-expr-bits quote-l-expr)))




;;; The eval-when-l-expr and constant fold into either a quoted NIL if the
;;; situation does not include :execute, into its last form if it has only one
;;; body form, or staying as itself if there are multiple forms to execute.

(def-l-expr-method set-specific-l-expr-bits (eval-when-l-expr)
  (let* ((form (l-expr-form eval-when-l-expr))
	 (situations (cons-second form))
	 (subforms (cons-cddr form)))
    (cond ((and (not (memq 'eval situations))
		(not (memq :execute situations)))
	   (make-quoted-constant-l-expr
	     nil (l-expr-env eval-when-l-expr)
	     (l-expr-aug-env eval-when-l-expr)))
	  ((null (cons-cdr subforms))
	   (cons-car subforms))
	  (t
	   eval-when-l-expr))))




;;; The quote-l-expr is constant, functional, and side-effect-free.

(def-l-expr-method set-specific-l-expr-bits (quote-l-expr)
  (setf (l-expr-constant-p quote-l-expr) t)
  (setf (l-expr-functional-p quote-l-expr) t)
  (setf (l-expr-side-effect-free-p quote-l-expr) t)
  quote-l-expr)




;;; If-l-expr is functional, can constant fold out given a constant conditional
;;; argument, and can be side-effect free if all of its arguments are
;;; side-effect-free.

(def-l-expr-method set-specific-l-expr-bits (if-l-expr)
  (let ((form (l-expr-form if-l-expr)))
    (setf (l-expr-functional-p if-l-expr) t)
    (cond ((l-expr-constant-p (cons-second form))
	   (if (l-expr-constant-value (cons-second form))
	       (cons-third form)
	       (cons-fourth form)))
	  ((loop for l-expr in (cons-cdr form)
		 always (l-expr-side-effect-free-p l-expr))
	   (setf (l-expr-side-effect-free-p if-l-expr) t)
	   if-l-expr)
	  (t
	   if-l-expr))))




;;; Progn-l-expr is functional, and is side-effect free if all of its arguments
;;; are side-effect-free.  Note that side-effect-free arguments that are not the
;;; last argument can be discarded, though this will be handled at translation
;;; time.

(def-l-expr-method set-specific-l-expr-bits (progn-l-expr)
  (let ((form (l-expr-form progn-l-expr)))
    (setf (l-expr-functional-p progn-l-expr) t)
    (cond ((null (cons-cdr form))
	   (make-quoted-constant-l-expr
	     nil (l-expr-env progn-l-expr)
	     (l-expr-aug-env progn-l-expr)))
	  ((null (cons-cddr form))
	   (cons-second form))
	  (t
	   (when (loop for l-expr in (cons-cdr (l-expr-form progn-l-expr))
		       always (l-expr-side-effect-free-p l-expr))
	     (setf (l-expr-side-effect-free-p progn-l-expr) t))
	   progn-l-expr))))




;;; The-l-expr can be snipped out if the argument is a constant and its value is
;;; of the type declared within this form.  It is side-effect-free if its
;;; argument is.

(def-l-expr-method set-specific-l-expr-bits (the-l-expr)
  (tl:destructuring-bind-strict (nil type l-expr) (l-expr-form the-l-expr)
    (setf (l-expr-functional-p the-l-expr) t)
    (setf (l-expr-side-effect-free-p the-l-expr)
	  (l-expr-side-effect-free-p l-expr))
    (if (and (l-expr-constant-p l-expr)
	     (tl-typep (l-expr-constant-value l-expr) type))
	l-expr
	the-l-expr)))




;;; Locally-l-expr is functional and is side-effect-free if all of its argument
;;; l-exprs are.

(def-l-expr-method set-specific-l-expr-bits (locally-l-expr)
  (setf (l-expr-functional-p locally-l-expr) t)
  (setf (l-expr-side-effect-free-p locally-l-expr)
	(loop for l-expr in (cons-cdr (l-expr-form locally-l-expr))
	      always (l-expr-side-effect-free-p l-expr)))
  locally-l-expr)




;;; Function-call depends on whether the function to be called is functional
;;; and/or side-effect free.  If the function is functional and all of the
;;; arguments are constants and those constants match the required argument
;;; types, then this can be constant folded.  If the operation is
;;; side-effect-free and all of the arguments are also side-effect-free, then it
;;; is side-effect-free.

(def-l-expr-method set-specific-l-expr-bits (function-call-l-expr)
  (let* ((form (l-expr-form function-call-l-expr))
	 (args (cons-cdr form))
	 (decls (function-call-l-expr-decls function-call-l-expr))
	 (functional? (cdr (assq 'tl:functional decls)))
	 (side-effect-free? (cdr (assq 'tl:side-effect-free decls))))
    (setf (l-expr-functional-p function-call-l-expr) functional?)
    (cond ((and functional?
		(loop for l-expr in args always (l-expr-constant-p l-expr))
		(let* ((ftype (or (cdr (assq 'ftype decls))
				  (cdr (assq 'computed-ftype decls))))
		       (argtypes (second ftype)))
		  (and ftype
		       (loop with optional = nil
			     with arg-cons = args
			     for argtype in argtypes
			     do
			 (cond ((eq argtype '&optional)
				(setq optional t))
			       ((null arg-cons)
				(return optional))
			       ((tl-typep (l-expr-constant-value (car arg-cons))
					  argtype)
				(setq arg-cons (cons-cdr arg-cons)))
			       (t (return nil)))
			     finally (return (null arg-cons))))))
	   (make-quoted-constant-l-expr
	     (cons (cons-car form)
		   (loop for l-expr in args
			 collect `(tl:quote ,(l-expr-constant-value l-expr))))
	     (l-expr-env function-call-l-expr)
	     (l-expr-aug-env function-call-l-expr)))
	  (t
	   (setf (l-expr-side-effect-free-p function-call-l-expr)
		 (and side-effect-free?
		      (loop for l-expr in args
			    always (l-expr-side-effect-free-p l-expr))))
	   function-call-l-expr))))




;;; Implicit-symbol-value-l-expr can be constant folded if the variable is a
;;; constant.  It is always side-effect-free and is functional.

(def-l-expr-method set-specific-l-expr-bits (implicit-symbol-value-l-expr)
  (let* ((binding-type (implicit-symbol-value-l-expr-binding-type
			 implicit-symbol-value-l-expr))
	 (constant-value
	   (if (eq binding-type :constant)
	       (symbol-value (l-expr-form implicit-symbol-value-l-expr)))))
    (cond ((and (eq binding-type :constant)
		(or (fixnump constant-value)
		    (tl-typep constant-value 'double-float)
		    (symbolp constant-value)))
	   (make-quoted-constant-l-expr
	     (list 'tl:quote constant-value)
	     (l-expr-env implicit-symbol-value-l-expr)
	     (l-expr-aug-env implicit-symbol-value-l-expr)))
	  (t
	   (when (eq binding-type :constant)
	     (setf (l-expr-constant-p implicit-symbol-value-l-expr) t))
	   (setf (l-expr-functional-p implicit-symbol-value-l-expr) t)
	   (setf (l-expr-side-effect-free-p implicit-symbol-value-l-expr) t)
	   implicit-symbol-value-l-expr))))




;;; Coerce-to-type-l-expr is a functional operator, so it takes on the bits of
;;; its argument l-expr.  Note that if an l-expr tree is evaluated twice, then
;;; two coerce-to-type l-exprs can be stacked on top of each other.  In that
;;; case, this method should eliminate itself and return its argument.

(def-l-expr-method set-specific-l-expr-bits (coerce-to-type-l-expr)
  (let ((held-l-expr (cons-second (l-expr-form coerce-to-type-l-expr))))
    (cond ((coerce-to-type-l-expr-p held-l-expr)
	   held-l-expr)
	  (t
	   (setf (l-expr-functional-p coerce-to-type-l-expr) t)
	   (setf (l-expr-side-effect-free-p coerce-to-type-l-expr)
		 (l-expr-side-effect-free-p held-l-expr))
	   (setf (l-expr-constant-p coerce-to-type-l-expr)
		 (l-expr-constant-p held-l-expr))
	   coerce-to-type-l-expr))))




;;; Values is a functional operator, since mutations to the Values_count are not
;;; semantically side-effects, and because the translator itself ensures that
;;; Values_count setting always happens on its own statement line, thus we can't
;;; have left-to-right argument evaluation semantics problems.  Though this is a
;;; functional operator, we will never set the constantp bit because the
;;; constant folding mechanism hasn't been designed to work with multiple
;;; values.  -jra 2/1/96

(def-l-expr-method set-specific-l-expr-bits (values-l-expr)
  (setf (l-expr-functional-p values-l-expr) t)
  (when (loop for arg in (cons-cdr (l-expr-form values-l-expr))
	      always (l-expr-side-effect-free-p arg))
    (setf (l-expr-side-effect-free-p values-l-expr) t))
  values-l-expr)




;;; Typep is functional, and can constant fold given pairs of constant
;;; arguments.

(def-l-expr-method set-specific-l-expr-bits (inlined-typep-l-expr)
  (let* ((form (l-expr-form inlined-typep-l-expr))
	 (object-l-expr (cons-second form))
	 (type (cons-third form)))
    (setf (l-expr-functional-p inlined-typep-l-expr) t)
    (cond ((l-expr-constant-p object-l-expr)
	   (make-quoted-constant-l-expr
	     (tl-typep (l-expr-constant-value object-l-expr) type)
	     (l-expr-env inlined-typep-l-expr)
	     (l-expr-aug-env inlined-typep-l-expr)))
	  (t
	   (when (l-expr-side-effect-free-p object-l-expr)
	     (setf (l-expr-side-effect-free-p inlined-typep-l-expr) t))
	   inlined-typep-l-expr))))




;;; AND is functional, and can constant fold (or optimize) in three cases:
;;;    1. A single (or no) argument(s)
;;;    2. There is a constant nil argument
;;;          and all previous args are side-effect-free.
;;;    3. There is a constant nil argument
;;;           (but not all previous args are side-effect-free),
;;;         it is still possible to discard (snip out) any arguments
;;;            after the last (possibly)side-effecting arg and before
;;;            the constant nil arg.
;;; Here is a recap of the logic implemented for AND expressions:
;;;    1. loop through the args, stopping if hit a constant arg with value NIL
;;;              (discard any remaining args as irrelevant)
;;;    2. snip out any constant arg with non-NIL value
;;;              (unless its the last arg, since we may need its value(s))
;;;    3. if any arg is NOT side-effect-free
;;;         then set the all-args-side-effect-free? flag to NIL
;;;              and save this arg-cons for possibly snipping out
;;;                       intervening side-effect-free args

(def-l-expr-method set-specific-l-expr-bits (and-l-expr)
  (setf (l-expr-functional-p and-l-expr) t)
  (loop with form = (l-expr-form and-l-expr)
	with last-cons = form
	with all-args-side-effect-free? = t
	with last-maybe-side-effecting-arg-cons = form
	with arg = nil
	for arg-cons = (cons-cdr last-cons)
	while arg-cons
	do
    (setq arg (cons-car arg-cons))
    (cond ((l-expr-constant-p arg)	
	   (if (l-expr-constant-value arg)  ; value is non-NIL
	       (if (null (cons-cdr arg-cons)) ; If this is last arg of form
		   (setq last-cons arg-cons)  ;     don't snip, just advance
		   (setf (cdr last-cons)
			 (cons-cdr arg-cons))) ; else snip non-NIL constant 
	       (cond (all-args-side-effect-free?
		      (return (make-quoted-constant-l-expr
				nil
				(l-expr-env and-l-expr)
				(l-expr-aug-env and-l-expr))))
		     (t
		      (setf (cdr last-maybe-side-effecting-arg-cons)
			    arg-cons) ; snip out intervening side-effect-free args
		      (setf (cdr arg-cons) nil  ; discard any remaining args
			    last-cons arg-cons))))); advance last-cons
	  (t (unless (l-expr-side-effect-free-p arg)
	       (setq all-args-side-effect-free? nil)
	       (setf last-maybe-side-effecting-arg-cons arg-cons))
	     (setq last-cons arg-cons))) ; advance last-cons
	finally
	  (return
	    (cond
	      ((null (cons-cdr form))
	       (make-quoted-constant-l-expr
		 t (l-expr-env and-l-expr)
		 (l-expr-aug-env and-l-expr)))
	      ((null (cons-cddr form))
	       (cons-second form))
	      (t
	       (setf (l-expr-side-effect-free-p arg)
		     all-args-side-effect-free?)
	       and-l-expr)))))




;;; OR is functional, but can only constant fold when the number of args is 0 or
;;; 1 (Is this still called constant-folding if the arg is not itself constant?)
;;; This is because the actual non-NIL value returned by OR is relevant -- it
;;; matters which arg produces the first non-NIL value.

;;; Here is a recap of the logic implemented for OR expressions:

;;;    1. loop through the args, stopping if hit a constant arg with value
;;;    non-NIL (discard any remaining args as irrelevant)

;;;    2. snip out any constant arg with NIL value (it's OK to snip out last
;;;    value, since a NIL is a NIL, and even we snip all args, the default is
;;;    NIL)

(def-l-expr-method set-specific-l-expr-bits (or-l-expr)
  (setf (l-expr-functional-p or-l-expr) t)
  (loop with form = (l-expr-form or-l-expr)
	with last-cons = form
	with arg = nil
	for arg-cons = (cons-cdr last-cons)
	while arg-cons
	do
    (setq arg (cons-car arg-cons))
    (if (l-expr-constant-p arg)
	(if (l-expr-constant-value arg)	; constant value is non-NIL
	    (setf (cdr arg-cons) nil	; discard any remaining args
		  last-cons arg-cons) ; advance last-cons
	    (setf (cdr last-cons) (cons-cdr arg-cons)))	;snip constant arg with NIL value
	(setq last-cons arg-cons))
	finally
	  (return
	    (cond
	      ((null (cons-cdr form))
	       (make-quoted-constant-l-expr
		 nil (l-expr-env or-l-expr)
		 (l-expr-aug-env or-l-expr)))
	      ((null (cons-cddr form))
	       (cons-second form))
	      (t
	       (when (loop for arg in (cons-cdr form)
			   always (l-expr-side-effect-free-p arg))
		 (setf (l-expr-side-effect-free-p arg) t))
	       or-l-expr)))))

(def-l-expr-method set-specific-l-expr-bits (c-comment-form-l-expr)
  (let* ((l-expr c-comment-form-l-expr)
	 (arg-l-expr (third (l-expr-form l-expr))))
    (setf (l-expr-side-effect-free-p l-expr)
	  (l-expr-side-effect-free-p arg-l-expr))
    (setf (l-expr-constant-p l-expr)
	  (l-expr-constant-p arg-l-expr))
    l-expr))





;;;; Type Propagation




;;; After constant-folding and bit-setting has happened on the l-exprs, then a
;;; type propagation pass is made over the l-exprs.  In this pass, both Lisp
;;; types and C types are determined.  When called, types will have already been
;;; chosen for each l-expr argument of this l-expr.  This method should set the
;;; l-expr-lisp-return-type and l-expr-c-return-types appropriately.  If the
;;; result types do not match the required types, then the caller of this method
;;; will attempt to perform appropriate type coercion.  The required types are
;;; given to these methods purely to aid in selecting the appropriate
;;; implementation of this l-expr, and may be ignored.

;;; The l-expr method `choose-l-expr-types' takes an l-expr whose arguments have
;;; already had type choosing performed upon them.  This method should set its
;;; own lisp-return-type and c-return-type attributes.

(def-l-expr-generic-method choose-l-expr-types
    (l-expr required-lisp-type required-c-type)
  (translation-warning "No choose-l-expr-types method for ~s" l-expr)
  (setf (l-expr-lisp-return-type l-expr) required-lisp-type)
  (setf (l-expr-c-return-type l-expr) required-c-type)
  l-expr)




;;; The `trivial-l-expr-lisp-result-type' l-expr method is used during initial
;;; l-expr-izing of translations to get a determination of the Lisp return type
;;; of this expression, if one can be quickly computed.  If one cannot be
;;; determined, then NIL should be returned.  This is called by the
;;; expression-result-type function within the Lisp special forms walkers.

(def-l-expr-generic-method trivial-l-expr-lisp-result-type (l-expr)
  (declare (ignore l-expr))
  nil)




;;; The choose-l-expr-types method for `block-l-expr' combines the type
;;; specifications returned from the last form in the body and the types in the
;;; block structure for this block-name.  These types will have been updated by
;;; the type chooser for the return-from l-expr.

(def-l-expr-method choose-l-expr-types (block-l-expr lisp-type c-type)
  (declare (ignore c-type))
  (let* ((form (l-expr-form block-l-expr))
	 (values-required? (type-includes-values-p lisp-type))
	 (exit-scope (tl:declaration-information
		       'exit-scope (l-expr-aug-env block-l-expr)))
	 (block-name (cons-second form))
	 (block-struct
	   (loop for entry in exit-scope
		 when (and (eq (cons-car entry) 'block)
			   (eq (cons-second entry) block-name))
		   return (cons-third entry)))
	 (last-l-expr (cons-car (last (cons-cddr form))))
	 (last-l-expr-lisp-type (l-expr-lisp-return-type last-l-expr)))
    (when (not values-required?)
      (setq last-l-expr-lisp-type (type-of-first-value last-l-expr-lisp-type)))
    (setf (block-scope-lisp-return-type block-struct)
	  (if (block-scope-lisp-return-type block-struct)
	      (most-specific-common-supertype
		last-l-expr-lisp-type
		(block-scope-lisp-return-type block-struct))
	      last-l-expr-lisp-type))
    (setf (block-scope-c-return-type block-struct)
	  (if (block-scope-c-return-type block-struct)
	      (most-specific-common-c-supertype
		(l-expr-c-return-type last-l-expr)
		(block-scope-c-return-type block-struct))
	      (l-expr-c-return-type last-l-expr)))
    (setf (l-expr-lisp-return-type block-l-expr)
	  (block-scope-lisp-return-type block-struct))
    (setf (l-expr-c-return-type block-l-expr)
	  (block-scope-c-return-type block-struct))
    block-l-expr))




;;; The choose-l-expr-types method for `catch-l-expr' always returns types * and
;;; Obj, since that is all that can come from the catch buffer.

(def-l-expr-method choose-l-expr-types (catch-l-expr lisp-type c-type)
  (setf (l-expr-lisp-return-type catch-l-expr)
	(if (eq lisp-type 'void) 'void '*))
  (setf (l-expr-c-return-type catch-l-expr)
	(if (eq c-type 'void) 'void 'obj))
  catch-l-expr)




;;; The function `choose-l-expr-types-ala-progn' takes an l-expr and sets its
;;; return types to the return type of the last l-expr in its form.

(defun choose-l-expr-types-ala-progn (l-expr)
  (let ((last-l-expr (cons-car (last (cons-cdr (l-expr-form l-expr))))))
    (setf (l-expr-lisp-return-type l-expr)
	  (l-expr-lisp-return-type last-l-expr))
    (setf (l-expr-c-return-type l-expr)
	  (l-expr-c-return-type last-l-expr))
    l-expr))




;;; The function `trivial-l-expr-lisp-result-type-ala-progn' takes an l-expr and
;;; returns the trivial type of the last l-expr in its body.

(defun trivial-l-expr-lisp-result-type-ala-progn (l-expr)
  (let ((last-form-cons? (last (cons-cdr (l-expr-form l-expr)))))
    (if last-form-cons?
	(trivial-l-expr-lisp-result-type
	  (cons-car last-form-cons?))
	'null)))





;;; The choose-l-expr-types method for `eval-when-l-expr' is processed like a
;;; progn.

(def-l-expr-method choose-l-expr-types (eval-when-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn eval-when-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (eval-when-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn eval-when-l-expr))

(def-l-expr-method choose-l-expr-types (flet-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn flet-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (flet-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn flet-l-expr))

(def-l-expr-method choose-l-expr-types (named-lambda-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  ;; Named-lambdas are only allowed at top level.  So they don't return anything.
  (setf (l-expr-lisp-return-type named-lambda-l-expr) 'void)
  (setf (l-expr-c-return-type named-lambda-l-expr) 'void)
  nil)

(def-l-expr-method choose-l-expr-types (function-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (setf (l-expr-lisp-return-type function-l-expr) 'compiled-function)
  (setf (l-expr-c-return-type function-l-expr) 'obj)
  function-l-expr)

(def-l-expr-method trivial-l-expr-lisp-result-type (function-l-expr)
  (declare (ignore function-l-expr))
  'compiled-function)

(def-l-expr-method choose-l-expr-types (go-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (setf (l-expr-lisp-return-type go-l-expr) 'void)
  (setf (l-expr-c-return-type go-l-expr) 'void)
  go-l-expr)

(def-l-expr-method choose-l-expr-types (if-l-expr lisp-type c-type)
  (declare (ignore c-type))
  (let* ((form (l-expr-form if-l-expr))
	 (then-l-expr (cons-third form))
	 (else-l-expr? (fourth form))
	 (values-required? (type-includes-values-p lisp-type))
	 (then-lisp-type (l-expr-lisp-return-type then-l-expr))
	 (else-lisp-type (if else-l-expr?
			     (l-expr-lisp-return-type else-l-expr?)
			     'null)))
    (when (not values-required?)
      (setq then-lisp-type (type-of-first-value then-lisp-type))
      (setq else-lisp-type (type-of-first-value else-lisp-type)))
    (setf (l-expr-lisp-return-type if-l-expr)
	  (most-specific-common-supertype then-lisp-type else-lisp-type))
    (setf (l-expr-c-return-type if-l-expr)
	  (most-specific-common-c-supertype
	    (l-expr-c-return-type then-l-expr)
	    (if else-l-expr?
		(l-expr-c-return-type else-l-expr?)
		'obj)))
    if-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (if-l-expr)
  (let* ((form (l-expr-form if-l-expr))
	 (then-type? (trivial-l-expr-lisp-result-type (cons-third form)))
	 (else-type? (trivial-l-expr-lisp-result-type (cons-fourth form))))
    (if (and then-type? else-type?)
	(most-specific-common-supertype then-type? else-type? t)
	nil)))

(def-l-expr-method choose-l-expr-types (labels-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn labels-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (labels-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn labels-l-expr))

(def-l-expr-method choose-l-expr-types (let-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn let-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (let-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn let-l-expr))

(def-l-expr-method choose-l-expr-types (let*-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn let*-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (let*-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn let*-l-expr))

(def-l-expr-method choose-l-expr-types (macrolet-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn macrolet-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (macrolet-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn macrolet-l-expr))

(def-l-expr-method choose-l-expr-types
    (multiple-value-bind-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn multiple-value-bind-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (multiple-value-bind-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn multiple-value-bind-l-expr))

(def-l-expr-method choose-l-expr-types
    (multiple-value-prog1-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (let ((first-l-expr (cons-second (l-expr-form multiple-value-prog1-l-expr))))
    (setf (l-expr-lisp-return-type multiple-value-prog1-l-expr)
	  (l-expr-lisp-return-type first-l-expr))
    (setf (l-expr-c-return-type multiple-value-prog1-l-expr)
	  (l-expr-c-return-type first-l-expr))
    multiple-value-prog1-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (multiple-value-prog1-l-expr)
  (trivial-l-expr-lisp-result-type
    (cons-second (l-expr-form multiple-value-prog1-l-expr))))

(def-l-expr-method choose-l-expr-types (progn-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn progn-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (progn-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn progn-l-expr))

(def-l-expr-method choose-l-expr-types (quote-l-expr lisp-type c-type)
  (let ((value (l-expr-form quote-l-expr)))
    (multiple-value-bind (lisp-return c-return)
	(if (satisfies-c-required-type-p c-type 'boolean)
	    (values 't 'boolean)
	    (typecase value
	      (null
	       (values 'null 'obj))
	      (symbol
	       (values 'symbol 'obj))
	      (integer
	       ;; Note that the underlying Lisp fixnums and TL fixnums may have
	       ;; different sizes (for example in CLISP).  Use fixnump, which
	       ;; returns whether it would be a TL fixnum.  -jallard 5/28/01
	       (when (not (fixnump value))
		 (translation-warning
		  "Constant ~a isn't a fixnum in TL, TL doesn't support bignums."
		  value))
	       (if (tl-subtypep lisp-type '(c-type "long"))
		   (values '(c-type "long") 'long)
		   (values 'fixnum
			   (if (loop for type in '(sint32 uint32 sint16 uint16 
							  uint8)
				     thereis (satisfies-c-required-type-p
					       c-type type))
			       c-type
			       'obj))))
	      (double-float
	       (values 'double-float 'double))
	      (string
	       (if (and (tl-subtypep lisp-type '(c-type (pointer "char")))
			(<= (length value)
			    maximum-inline-c-constant-string-length))
		   (values '(c-type (pointer "char")) '(pointer char))
		   (values 'simple-string
			   (if (satisfies-c-required-type-p
				 c-type '(pointer unsigned-char))
			       '(pointer unsigned-char)
			       'obj))))
;              (managed-float
;               (values 'managed-float 'obj))
	      (character
	       ;; Note that translate-quoted-value-into-c does know how to emit
	       ;; a character as C-type Obj, but in normal circumstances we want
	       ;; to let type coercion surrounding the translated constant do
	       ;; that transform, in case there are further optimizations that
	       ;; can be had from knowing this value is naturally an integer,
	       ;; unsigned-char.  The code to emit characters as objects is
	       ;; there for constant structure initializing, which does not go
	       ;; through type choosing.  -jra 12/18/96
	       (values 'character 'unsigned-char))
	      ((array (unsigned-byte 8))
	       (values '(array (unsigned-byte 8)) 'obj))
	      ((array (unsigned-byte 16))
	       (values '(array (unsigned-byte 16)) 'obj))
	      ((array double-float)
	       (values '(array double-float) 'obj))
	      (simple-vector
	       (values 'simple-vector 'obj))
	      (t
	       (values 't 'obj))))
      (setf (l-expr-lisp-return-type quote-l-expr) lisp-return)
      (setf (l-expr-c-return-type quote-l-expr) c-return)
      quote-l-expr)))

(def-l-expr-method trivial-l-expr-lisp-result-type (quote-l-expr)
  (tl-type-of (l-expr-form quote-l-expr)))

(def-l-expr-method choose-l-expr-types (return-from-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (let* ((form (l-expr-form return-from-l-expr))
	 (name (cons-second form))
	 (value-l-expr (cons-third form))
	 (value-lisp-return-type (l-expr-lisp-return-type value-l-expr))
	 (block-struct
	   (block-scope-struct-for-name name (l-expr-env return-from-l-expr)))
	 (block-requires-values?
	   (type-includes-values-p (block-scope-lisp-required-type 
				     block-struct))))
    (when (not block-requires-values?)
      (setq value-lisp-return-type (type-of-first-value value-lisp-return-type)))
    (setf (block-scope-lisp-return-type block-struct)
	  (if (block-scope-lisp-return-type block-struct)
	      (most-specific-common-supertype
		value-lisp-return-type
		(block-scope-lisp-return-type block-struct))
	      value-lisp-return-type))
    (setf (block-scope-c-return-type block-struct)
	  (if (block-scope-c-return-type block-struct)
	      (most-specific-common-c-supertype
		(l-expr-c-return-type value-l-expr)
		(block-scope-c-return-type block-struct))
	      (l-expr-c-return-type value-l-expr)))
    (setf (l-expr-lisp-return-type return-from-l-expr) 'void)
    (setf (l-expr-c-return-type return-from-l-expr) 'void)
    return-from-l-expr))

(def-l-expr-method choose-l-expr-types (setq-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (let* ((form (l-expr-form setq-l-expr))
	 (symbol (cons-second form))
	 (decls
	   (multiple-value-bind (bind-type? local? declarations)
	       (tl:variable-information symbol (l-expr-env setq-l-expr))
	     (declare (ignore bind-type? local?))
	     declarations))
	 (binding? (cdr (assq 'variable-binding-structure decls)))
	 (binding-type? (and binding? (variable-binding-lisp-type binding?)))
	 (declared-type? (cdr (assq 'type decls)))
	 (lisp-type
	   (if binding-type?
	       (if declared-type?
		   (duplicate-type-declaration binding-type? declared-type?)
		   binding-type?)
	       (or declared-type? t)))
	 (c-type (or (and binding? (variable-binding-c-type binding?))
		     'obj)))
    (setf (l-expr-lisp-return-type setq-l-expr) lisp-type)
    (setf (l-expr-c-return-type setq-l-expr) c-type)
    setq-l-expr))

(def-l-expr-method choose-l-expr-types (tagbody-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  ;; Tagbody always returns NIL.
  (setf (l-expr-lisp-return-type tagbody-l-expr) 'null)
  (setf (l-expr-c-return-type tagbody-l-expr) 'obj)
  tagbody-l-expr)

(def-l-expr-method trivial-l-expr-lisp-result-type (tagbody-l-expr)
  (declare (ignore tagbody-l-expr))
  'null)

(def-l-expr-method choose-l-expr-types (the-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (let ((value-form (cons-third (l-expr-form the-l-expr))))
    (setf (l-expr-lisp-return-type the-l-expr)
	  (l-expr-lisp-return-type value-form))
    (setf (l-expr-c-return-type the-l-expr) (l-expr-c-return-type value-form))
    the-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (the-l-expr)
  (cons-second (l-expr-form the-l-expr)))

(def-l-expr-method choose-l-expr-types (throw-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  ;; Throw never returns.
  (setf (l-expr-lisp-return-type throw-l-expr) 'void)
  (setf (l-expr-c-return-type throw-l-expr) 'void)
  throw-l-expr)

(def-l-expr-method choose-l-expr-types (unwind-protect-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (let ((protected-l-expr (cons-second (l-expr-form unwind-protect-l-expr))))
    (setf (l-expr-lisp-return-type unwind-protect-l-expr)
	  (l-expr-lisp-return-type protected-l-expr))
    (setf (l-expr-c-return-type unwind-protect-l-expr)
	  (l-expr-c-return-type protected-l-expr))
    unwind-protect-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (unwind-protect-l-expr)
  (trivial-l-expr-lisp-result-type
    (cons-second (l-expr-form unwind-protect-l-expr))))

(def-l-expr-method choose-l-expr-types (symbol-macrolet-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn symbol-macrolet-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (symbol-macrolet-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn symbol-macrolet-l-expr))

(def-l-expr-method choose-l-expr-types (locally-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (choose-l-expr-types-ala-progn locally-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (locally-l-expr)
  (trivial-l-expr-lisp-result-type-ala-progn locally-l-expr))

(def-l-expr-method choose-l-expr-types (fixnum-case-l-expr lisp-type c-type)
  (declare (ignore c-type))
  (let* ((form (l-expr-form fixnum-case-l-expr))
	 (values-required? (type-includes-values-p lisp-type))
	 (clauses (cons-cddr form))
	 ;; The special form walker guarantees that there is at least one
	 ;; clause, and at least one form in each clause.
	 (first-clause (cons-car clauses))
	 (first-value-l-expr (cons-car (last (cons-cdr first-clause))))
	 (lisp-return-type
	   (if values-required?
	       (l-expr-lisp-return-type first-value-l-expr)
	       (type-of-first-value
		 (l-expr-lisp-return-type first-value-l-expr))))
	 (c-return-type (l-expr-c-return-type first-value-l-expr)))

    (loop for clause in (cons-cdr clauses)
	  for result-l-expr = (cons-car (last (cons-cdr clause)))
	  for lisp-type-of-clause = (l-expr-lisp-return-type result-l-expr)
	  do
      (setq lisp-return-type
	    (most-specific-common-supertype
	      lisp-return-type
	      (if values-required?
		  lisp-type-of-clause
		  (type-of-first-value lisp-type-of-clause))))
      (setq c-return-type
	    (most-specific-common-c-supertype
	      c-return-type (l-expr-c-return-type result-l-expr))))
    
    (setf (l-expr-lisp-return-type fixnum-case-l-expr) lisp-return-type)
    (setf (l-expr-c-return-type fixnum-case-l-expr) c-return-type)
    fixnum-case-l-expr))

(def-l-expr-method choose-l-expr-types (while-loop-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  ;; While loop always returns NIL.
  (setf (l-expr-lisp-return-type while-loop-l-expr) 'null)
  (setf (l-expr-c-return-type while-loop-l-expr) 'obj)
  while-loop-l-expr)

(def-l-expr-method choose-l-expr-types (for-loop-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  ;; For loop always returns NIL.
  (setf (l-expr-lisp-return-type for-loop-l-expr) 'null)
  (setf (l-expr-c-return-type for-loop-l-expr) 'obj)
  for-loop-l-expr)

(def-l-expr-method choose-l-expr-types (function-call-l-expr lisp-type c-type)
  (let* ((decls (function-call-l-expr-decls function-call-l-expr))
	 (c-translator? (cdr (assq 'c-translator decls))))
    (if c-translator?
	(let ((chosen-translation?
		(funcall (cons-second c-translator?)
			 function-call-l-expr lisp-type c-type)))
	  (cond (chosen-translation?
		 (setf (function-call-l-expr-translator? function-call-l-expr)
		       chosen-translation?)
		 (setf (l-expr-lisp-return-type function-call-l-expr)
		       (cons-second
			 (chosen-translation-lisp-type chosen-translation?)))
		 (setf (l-expr-c-return-type function-call-l-expr)
		       (cons-second
			 (chosen-translation-c-type chosen-translation?))))
		(t
		 (translation-error
		   "No def-c-translation clause chosen for form ~s, arg-types = ~s"
		   (l-expr-pretty-form function-call-l-expr)
		   (loop for l-expr in (cdr (l-expr-form function-call-l-expr))
			 collect (list (l-expr-lisp-return-type l-expr)
				       (l-expr-c-return-type l-expr)))))))
	(let* ((ftype (or (cdr (assq 'ftype decls))
			  (cdr (assq 'computed-ftype decls))))
	       (lisp-return-type (cons-third ftype)))
	  (setf (l-expr-lisp-return-type function-call-l-expr)
		lisp-return-type)
	  (setf (l-expr-c-return-type function-call-l-expr)
		(c-func-type-for-lisp-func-return-type-spec lisp-return-type))))
    function-call-l-expr))




;;; The trivial-l-expr-lisp-result-type method for function-call-l-expr is only
;;; allowed to return type specs for functions that have explicit ftype
;;; declarations.  This prevents computed-ftypes from affecting the
;;; computed-ftypes of other functions, which we want to avoid because of its
;;; inherent instability.

(def-l-expr-method trivial-l-expr-lisp-result-type (function-call-l-expr)
  (third (cdr (assq 'ftype (function-call-l-expr-decls
			     function-call-l-expr)))))




;;; When the required-type is void for an implicit-symbol-value call, it could
;;; be that this reference is there to prevent an unused variable warning.  In
;;; these cases, this operation will return type void, and the translation will
;;; simply be a reference to the variable with the result type cast to void,
;;; which is the standard way to prevent unused variable warnings in C.

(def-l-expr-method choose-l-expr-types
    (implicit-symbol-value-l-expr required-lisp-type required-c-type)
  (declare (ignore required-c-type))
  (let* ((isv implicit-symbol-value-l-expr)
	 (decls (implicit-symbol-value-l-expr-decls isv))
	 (binding? (cdr (assq 'variable-binding-structure decls)))
	 (binding-type? (and binding? (variable-binding-lisp-type binding?)))
	 (declared-type? (cdr (assq 'type decls)))
	 (lisp-type
	   (or (and (eq required-lisp-type 'void)
		    'void)
	       (and binding-type?
		    (if declared-type?
			(duplicate-type-declaration binding-type? declared-type?)
			binding-type?))
	       declared-type?
	       (and (eq (implicit-symbol-value-l-expr-binding-type isv)
			:constant)
		    (loop with value = (symbol-value (l-expr-form isv))
			  for type in tl-significant-types
			  when (tl-typep value type)
			    return type))
	       t))
	 (c-type (or (and (eq required-lisp-type 'void)
			  'void)
		     (and binding? (variable-binding-c-type binding?))
		     'obj)))
    (setf (l-expr-lisp-return-type isv) lisp-type)
    (setf (l-expr-c-return-type isv) c-type)
    isv))

(def-l-expr-method trivial-l-expr-lisp-result-type
    (implicit-symbol-value-l-expr)
  (or (cdr (assq 'type (implicit-symbol-value-l-expr-decls
			 implicit-symbol-value-l-expr)))
      t))

(def-l-expr-method choose-l-expr-types (coerce-to-type-l-expr lisp-type c-type)
  (let* ((held-l-expr (cons-second (l-expr-form coerce-to-type-l-expr)))
	 (held-lisp-type (l-expr-lisp-return-type held-l-expr)))
    (setf (coerce-to-type-l-expr-lisp-required-type coerce-to-type-l-expr)
	  lisp-type)
    (setf (coerce-to-type-l-expr-c-required-type coerce-to-type-l-expr)
	  c-type)
    ;; Handle the case where the required type wants a values-count set, but the
    ;; held-type has not set the values count.
    (cond ((type-includes-values-p lisp-type)
	   (when (not (type-includes-values-p held-lisp-type))
	     (setq held-lisp-type (list 'values held-lisp-type))))
	  ;; Handle the case where the required type does not want a
	  ;; values-count set, but the held-type does in fact set the values
	  ;; count.
	  ((type-includes-values-p held-lisp-type)
	   (setq held-lisp-type (type-of-first-value held-lisp-type))))
    (setf (l-expr-lisp-return-type coerce-to-type-l-expr)
	  (cond
	    ((tl-subtypep held-lisp-type lisp-type) held-lisp-type)
	    ((tl-subtypep lisp-type held-lisp-type) lisp-type)
	    ((and (tl-subtypep held-lisp-type 'fixnum)
		  (tl-subtypep lisp-type 'double-float))
	     'double-float)
	    ;; When attempting to coerce to a C-type, check that the Lisp type
	    ;; we can coerce from is compatible with the type we know how to
	    ;; coerce from.  For example, to coerce to long, the type fixnum
	    ;; must be a possible subtype of the argument type.
	    ((and (explicit-lisp-to-c-type-p lisp-type)
		  (or (and (tl-subtypep lisp-type '(c-type "long"))
			   (tl-subtypep 'fixnum held-lisp-type))
		      (and (tl-subtypep lisp-type '(c-type (pointer "char")))
			   (tl-subtypep 'string held-lisp-type))
		      (tl-subtypep lisp-type '(c-type (pointer "void")))))
	     lisp-type)

	    ;; When coercing from a Long, the required type must allow fixnums.
	    ((and (tl-subtypep held-lisp-type '(c-type "long"))
		  (tl-subtypep 'fixnum lisp-type))
	     'fixnum)
	    ;; When coercing from type NULL, allow initializations of fixnums to zero.
	    ((and (tl-subtypep held-lisp-type 'null)
		  (tl-subtypep lisp-type 'fixnum))
	     'fixnum)
	    (t
	     ;; Else the coercion can't work, return 'void
	     'void)))
    (setf (l-expr-c-return-type coerce-to-type-l-expr) c-type)))




;;; Within the type chooser for values, I'll take one optimization.  In the case
;;; where this form is producing more values than are required, I'll discard
;;; those extra values within the form so that the unused portions can be
;;; translated with a return directive of :discard.  In cases where more values
;;; are desired than are provided, the receiving location (always a
;;; multiple-value-setq or multiple-value-bind) will have to go ahead and
;;; provide the extra NIL values.  That seems better than adding extra code here
;;; to give them NILs that can be generated in that location anyway.  The
;;; special form walker and the l-expr-walker both perform their required-type
;;; computations in agreement with this policy.

(def-l-expr-method choose-l-expr-types (values-l-expr lisp-type c-type)
  (declare (ignore c-type))
  (let ((args (cons-cdr (l-expr-form values-l-expr))))
    (cond
      ((and (consp lisp-type) (eq (cons-car lisp-type) 'values))
       (setf (l-expr-lisp-return-type values-l-expr)
	     (cons 'values
		   ;; The repeat statement limits us to returning at most the
		   ;; number of values that are required.
		   (loop repeat (length (cons-cdr lisp-type))
			 for arg in args
			 collect (type-of-first-value
				   (l-expr-lisp-return-type arg))))))
      ((eq lisp-type '*)
       (setf (l-expr-lisp-return-type values-l-expr)
	     (cons 'values
		   (loop for arg in args
			 collect (type-of-first-value
				   (l-expr-lisp-return-type arg))))))
      (t
       ;; The caller doesn't care about secondary values, so we won't provide
       ;; them and we won't set the values_count on translation.  Note that the
       ;; Lisp form walker in SPECIAL guarantees that we always have at least
       ;; one argument.
       (setf (l-expr-lisp-return-type values-l-expr)
	     (type-of-first-value (l-expr-lisp-return-type (cons-car args))))))
    (setf (l-expr-c-return-type values-l-expr)
	  (l-expr-c-return-type (cons-car args)))))




;;; The type chooser for inlined-typep wants T and Obj from its arguments, and
;;; it returns T and boolean.

(def-l-expr-method choose-l-expr-types (inlined-typep-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (setf (l-expr-lisp-return-type inlined-typep-l-expr) 't)
  (setf (l-expr-c-return-type inlined-typep-l-expr) 'boolean)
  inlined-typep-l-expr)




;;; The type choosers for and-l-expr and or-l-expr prefer to return T and
;;; boolean, but if the argument wants a non-boolean return value, then they
;;; return T or * (depending on if the required-type needs the values-count set)
;;; and Obj.

(def-l-expr-method choose-l-expr-types (and-l-expr lisp-type c-type)
  (choose-logical-l-expr-types and-l-expr lisp-type c-type))

(def-l-expr-method choose-l-expr-types (or-l-expr lisp-type c-type)
  (choose-logical-l-expr-types or-l-expr lisp-type c-type))

(defun choose-logical-l-expr-types (l-expr lisp-type c-type)
  (cond
    ((satisfies-c-required-type-p c-type 'boolean)
     (setf (l-expr-lisp-return-type l-expr) t)
     (setf (l-expr-c-return-type l-expr) 'boolean))
    (t
     (setf (l-expr-lisp-return-type l-expr)
	   (if (type-includes-values-p lisp-type) '* t))
     (setf (l-expr-c-return-type l-expr) 'obj))))

(def-l-expr-method choose-l-expr-types
    (list-dynamic-extent-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (setf (l-expr-lisp-return-type list-dynamic-extent-l-expr) 'cons)
  (setf (l-expr-c-return-type list-dynamic-extent-l-expr) 'obj))

(def-l-expr-method choose-l-expr-types
    (funcall-internal-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (setf (l-expr-lisp-return-type funcall-internal-l-expr)
	(if (cons-second (l-expr-form funcall-internal-l-expr))
	    '*
	    't))
  (setf (l-expr-c-return-type funcall-internal-l-expr) 'obj))

(def-l-expr-method choose-l-expr-types
    (c-comment-form-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (let* ((l-expr c-comment-form-l-expr)
	 (arg-l-expr (cons-third (l-expr-form l-expr))))
    (setf (l-expr-lisp-return-type l-expr)
	  (l-expr-lisp-return-type arg-l-expr))
    (setf (l-expr-c-return-type l-expr)
	  (l-expr-c-return-type arg-l-expr))))




;;; `Malloc-class-instance' takes an unquoted symbol that names a class or
;;; structure type.  It will malloc and return an instance of that type, with a
;;; Lisp type of the named class and a C type of obj.

(def-l-expr-method choose-l-expr-types (malloc-class-instance-l-expr lt ct)
  (declare (ignore lt ct))
  (let* ((class (cons-second (l-expr-form malloc-class-instance-l-expr))))
    (setf (l-expr-lisp-return-type malloc-class-instance-l-expr) class)
    (setf (l-expr-c-return-type malloc-class-instance-l-expr) 'obj)
    malloc-class-instance-l-expr))

(def-l-expr-method trivial-l-expr-lisp-result-type (malloc-class-instance-l-expr)
  (cons-second (l-expr-form malloc-class-instance-l-expr)))




;;; The `get-slot' special form is used to translate accessors of slots of C
;;; structures.  Note that if the argument type (i.e. the structure's required C
;;; type) is either obj or a C pointer type, then this operation will translate
;;; into a C indirect structure access.  For any other C type, this will
;;; translate into a direct structure element access.  Note that if the argument
;;; type is typedef'ed as a pointer, this function cannot find that out, and so
;;; it will incorrectly access the slot.

;;; In order to make this operation usable as a primitive for general C
;;; structure access, the Lisp and C types of both the structure and the held
;;; values, as well as the string naming the slot name are all given as explicit
;;; arguments.  The exception is when used as an accessor for defstruct, the
;;; symbol naming the slot is given instead of the C string, and the C type of
;;; the value is given as nil.  In this case, the C accessor string and C type
;;; of the slot value should be looked up during translation.  Only the first
;;; argument, the structure, is evaluated.  With this style of arguments we can
;;; make definitions for accessing C structures (for example threads or X
;;; windows handed to us from C) and manipulate these within Lisp.  This
;;; primitive is initially used by defstruct and def-c-struct.

;;; The `set-slot' special form is the same as get-slot, except that the new
;;; slot value is given as a final, additional argument.

;;;   (get-slot <struct> <slot-name-or-string> <arg-lisp-type> <arg-c-type>
;;;      <held-lisp-type> <held-c-type>)
;;;   (set-slot <struct> <slot-name-or-string> <arg-lisp-type> <arg-c-type>
;;;      <held-lisp-type> <held-c-type> <new-value>)

(def-l-expr-method choose-l-expr-types (get-slot-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (destructuring-bind (slot-name arg-lisp-type arg-c-type 
				 result-lisp-type result-c-type)
		      (cons-cddr (l-expr-form get-slot-l-expr))
    (declare (ignore arg-c-type))
    (let ((slot-c-type (or result-c-type
			   (and (symbolp slot-name)
				(class-type-p arg-lisp-type)
				(get-c-type-for-class-and-slot 
				   arg-lisp-type slot-name))
			   'obj)))
      (setf (l-expr-lisp-return-type get-slot-l-expr) result-lisp-type)
      (setf (l-expr-c-return-type get-slot-l-expr) slot-c-type)
      get-slot-l-expr)))

(def-l-expr-method trivial-l-expr-lisp-result-type (get-slot-l-expr)
  (nth 5 (l-expr-form get-slot-l-expr)))

(tl:defsetf get-slot set-slot)

(def-l-expr-method choose-l-expr-types (set-slot-l-expr lisp-type c-type)
  (declare (ignore lisp-type c-type))
  (destructuring-bind (slot-name arg-lisp arg-c held-lisp held-c new-value)
		      (cons-cddr (l-expr-form set-slot-l-expr))
    (declare (ignore arg-c))
    (let ((slot-c-type (or held-c
			   (and (symbolp slot-name)
				(class-type-p arg-lisp)
				(get-c-type-for-class-and-slot
				 arg-lisp slot-name))
			   'obj)))
      (setf (l-expr-lisp-return-type set-slot-l-expr)
	    (duplicate-type-declaration 
	     held-lisp (l-expr-lisp-return-type new-value)))
      (setf (l-expr-c-return-type set-slot-l-expr) 
	    (expand-c-type slot-c-type))
      set-slot-l-expr)))

(def-l-expr-method trivial-l-expr-lisp-result-type (set-slot-l-expr)
  (nth 5 (l-expr-form set-slot-l-expr)))










;;;; Walkers for Preparing L-exprs for Translation




;;; The function `prepare-l-expr-for-translation' is used to walk l-exprs before
;;; the translation pass.  This walker performs two tasks.  The first is to
;;; constant fold and set constant, side-effect-free, and functional bits on
;;; l-exprs.  The second is to perform type inference and function choosing.
;;; The function set-l-expr-bits does the first.  The l-expr method
;;; choose-l-expr-types does the second.

;;; Note that this walker can be called multiple times as optimizable operators
;;; adjust the required types of their arguments.

(defun prepare-l-expr-for-translation
    (l-expr lisp-required-type c-required-type)
  (setq l-expr (set-l-expr-bits l-expr))
  (when (and (l-expr-side-effect-free-p l-expr)
	     (eq lisp-required-type 'void)
	     ;; Leave in references to otherwise unused variables.
	     (not (and (implicit-symbol-value-l-expr-p l-expr)
		       (eq (implicit-symbol-value-l-expr-binding-type l-expr)
			   :lexical))))
    (return-from prepare-l-expr-for-translation
      (let ((quote-l-expr 
	      (make-quote-l-expr nil (l-expr-env l-expr) (l-expr-env l-expr))))
	(initialize-l-expr quote-l-expr)
	(set-l-expr-bits quote-l-expr)
	(choose-l-expr-types
	  quote-l-expr lisp-required-type c-required-type))))
  (choose-l-expr-types l-expr lisp-required-type c-required-type)
  (when (null (l-expr-lisp-return-type l-expr))
    (translation-error "No type chosen for ~s" l-expr))
  (cond ((coerce-to-type-l-expr-p l-expr)
	 (let ((held-l-expr (cons-second (l-expr-form l-expr))))
	   (cond
	     ((and (satisfies-lisp-required-type-p
		     (l-expr-lisp-return-type held-l-expr)
		     lisp-required-type)
		   (satisfies-c-required-type-p
		     (l-expr-c-return-type held-l-expr) c-required-type))
	      held-l-expr)
	     (t
	      l-expr))))
	((and (satisfies-lisp-required-type-p
		(l-expr-lisp-return-type l-expr) lisp-required-type)
	      (satisfies-c-required-type-p
		(l-expr-c-return-type l-expr) c-required-type))
	 l-expr)
	(t
	 (let ((new-l-expr
		 (make-coerce-to-type-l-expr
		   (list 'coerce-to-type l-expr)
		   (l-expr-env l-expr)
		   (l-expr-aug-env l-expr))))
	   (initialize-l-expr new-l-expr)
	   (setq new-l-expr (set-l-expr-bits new-l-expr))
	   (choose-l-expr-types new-l-expr lisp-required-type c-required-type)
	   new-l-expr))))






;;;; L-expr Argument Type Checking and Coercing




;;; The functions `l-expr-matches-lisp-type-p' and `l-expr-matches-c-type-p' are
;;; in def-c-translation to determine if a translation is appropriate for a
;;; particular l-expr.  These functions are used in the expansions of calls to
;;; the def-c-translation macro, but are included here to pick up forward
;;; references.

(defun l-expr-matches-lisp-type-p
    (argument-types result-type l-expr required-type)
  (declare (ignore result-type required-type))
  (let ((args (cons-cdr (l-expr-form l-expr))))
    (and (equal-list-lengths-p args argument-types)
	 (loop for l-expr in args
	       for type in argument-types
	       for l-expr-type = (l-expr-lisp-return-type l-expr)
	       always (satisfies-lisp-required-type-p l-expr-type type)))))

(defun l-expr-matches-c-type-p
    (c-argument-types lisp-argument-types c-result-type l-expr
		      required-c-type default-type?)
  (declare (ignore c-result-type required-c-type))
  (let ((args (cons-cdr (l-expr-form l-expr))))
    (and (equal-list-lengths-p args c-argument-types)
	 (loop for l-expr-cons on args
	       for l-expr = (cons-car l-expr-cons)
	       for c-arg-type in c-argument-types
	       for lisp-arg-type in lisp-argument-types
	       for l-expr-type = (l-expr-c-return-type l-expr)
	       always (or (satisfies-c-required-type-p l-expr-type c-arg-type)
			  (and (not default-type?)
			       (coerce-l-expr-to-return-c-type
				 l-expr-cons c-arg-type lisp-arg-type)))))))

(defun coerce-l-expr-to-return-c-type (l-expr-cons c-type lisp-type)
  (let* ((l-expr (cons-car l-expr-cons)))
    ;; Note that walking the l-expr would strip the coerce-to-type-l-expr as the
    ;; first clause to the OR below, but the clause used does so without
    ;; traversing the entire l-expr subexpression.
    (or (and (coerce-to-type-l-expr-p l-expr)
	     (let ((held-l-expr (cons-second (l-expr-form l-expr))))
	       (cond
		 ((and (satisfies-lisp-required-type-p
			 (l-expr-lisp-return-type held-l-expr) lisp-type)
		       (satisfies-c-required-type-p
			 (l-expr-c-return-type held-l-expr)
			 c-type))
		  (setf (car l-expr-cons) held-l-expr)
		  t)
		 (t nil))))
	(let ((new-l-expr
		(walk-l-expr
		  l-expr #'prepare-l-expr-for-translation
		  lisp-type c-type)))
	  (setf (car l-expr-cons) new-l-expr)
	  (and (satisfies-lisp-required-type-p
		 (l-expr-lisp-return-type new-l-expr) lisp-type)
	       (satisfies-c-required-type-p
		 (l-expr-c-return-type new-l-expr) c-type))))))




;;; The function `uncoerced-l-expr-c-return-type' takes an l-expr and returns
;;; either the c-return-type of the argument, or if the argument is either a
;;; coerce-to-type-l-expr or a the-l-expr it returns the c-return-type of the
;;; argument to the "coerce" or "the" operator.  This function is used when
;;; determining what type an l-expr optimally returns before coercion.

;;; This function is necessary since on an initial pass through
;;; choose-l-expr-types, coercion operators will be added at the deepest leaves
;;; of expressions.  If we are to determine which type would naturally come out
;;; of a translation without coercions, we have to dive deep into the l-expr to
;;; find any coercion points and return the type of the operator that feeds that
;;; coercion.  The effect of this operator is to allow sint32 values to be
;;; passed through integer expressions.  Any expression formats not handled by
;;; this function will get their fixnum results coerced into Obj format, perhaps
;;; then requiring transforms back into sint32 format later on in the expression
;;; evaluation.

(defun uncoerced-l-expr-c-return-type (l-expr)
  (cond ((the-l-expr-p l-expr)
	 (uncoerced-l-expr-c-return-type (cons-third (l-expr-form l-expr))))
	((coerce-to-type-l-expr-p l-expr)
	 (l-expr-c-return-type (cons-second (l-expr-form l-expr))))
	((or (progn-l-expr-p l-expr)
	     (let-l-expr-p l-expr)
	     (let*-l-expr-p l-expr)
	     (locally-l-expr-p l-expr))
	 (uncoerced-l-expr-c-return-type (cons-car (last (l-expr-form l-expr)))))
	((if-l-expr-p l-expr)
	 (let* ((form (l-expr-form l-expr))
		(then-type (uncoerced-l-expr-c-return-type (cons-second form)))
		(else-type (uncoerced-l-expr-c-return-type (cons-third form))))
	   (if (c-types-equal-p then-type else-type)
	       then-type
	       (l-expr-c-return-type l-expr))))
	(t
	 (l-expr-c-return-type l-expr))))






;;;; L-Expr Quick Result Type




;;; The function `trivial-lisp-result-type-if-l-expr' takes a Lisp constant, and
;;; if it is an l-expr returns an expression result type for the evaluation of
;;; that l-expr.  Otherwise, it returns NIL.  This function is called during
;;; code walking to get a fast approximation of Lisp type.  Note that this
;;; function must be conservative, in that the type returned here must be a
;;; superior type of the whatever type is eventually actually determined by
;;; calling the choose-l-expr-types method.

;;; This method is implemented by calling the `trivial-l-expr-lisp-result-type'
;;; method if the argument is an l-expr.  The default implementation of this
;;; method returns NIL.  The default implementation of this method is found at
;;; the head of this file, and methods for specific types are found next to the
;;; choose-l-expr-types method for each type of l-expr.

(defun trivial-lisp-result-type-if-l-expr (lisp-constant)
  (if (l-expr-p lisp-constant)
      (trivial-l-expr-lisp-result-type lisp-constant)
      nil))

(setq trivial-lisp-result-type-function #'trivial-lisp-result-type-if-l-expr)
