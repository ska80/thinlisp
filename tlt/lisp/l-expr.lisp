(in-package "TLI")

;;;; Module L-EXPR

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






;;;; Lisp Expression Structures




;;; This file contains definitions of structures for translating Lisp forms.
;;; All structures representing Lisp forms during the middle layer of
;;; translation will be stored in subtypes of the struct type `l-expr.  This is
;;; the type manipulated by the translator as it prepares to translate into
;;; c-file, c-func, c-state, and c-expr structures.  There is a method system on
;;; l-exprs, defined with def-l-expr-generic-method and def-l-expr-method forms.
;;; With the definition of each l-expr form in this file, there is a walker
;;; method defined.

(defstruct (l-expr (:print-function print-l-expr))
  number
  form
  env
  aug-env
  (lisp-return-type nil)
  (c-return-type nil)
  (bit-flags 0))

(defun print-l-expr (l-expr stream level)
  (with-printing-wrapper (l-expr stream)
    (prin1 (tl-type-of l-expr) stream)
    (if (and (numberp *print-level*) (> level *print-level*))
	(write-string " ..." stream)
	(format stream " ~s" (l-expr-form l-expr)))))

(defmacro l-expr-pretty-form (l-expr)
  `(de-l-exprize-form (l-expr-form ,l-expr)))

(defun de-l-exprize-form (form)
  (cond ((consp form)
	 (cons (de-l-exprize-form (cons-car form))
	       (de-l-exprize-form (cons-cdr form))))
	((l-expr-p form)
	 (de-l-exprize-form (l-expr-form form)))
	(t form)))




;;; The macros `test-l-expr-bit-flag' and `set-l-expr-bit-flag' are used to
;;; implement small, efficient true false flags on l-exprs.  They take a l-expr,
;;; an integer bit number, and the set version takes a true/false argument.  The
;;; first available bit is 0, the last is 28.  Note that set-l-expr-bit-flag is
;;; non-standard with respect to the bit argument, it should always be given as
;;; a constant.

(defmacro test-l-expr-bit-flag (l-expr bit)
  (let ((mask (expt 2 bit)))
    `(/= (the fixnum (logand (the fixnum (l-expr-bit-flags ,l-expr)) ,mask))
	 0)))

(defmacro set-l-expr-bit-flag (l-expr bit new-value)
  (let* ((l-expr-value (gensym))
	 (mask (expt 2 bit))
	 (set-value-code 
	  `(progn
	     (setf (l-expr-bit-flags ,l-expr-value)
	       (logior
		(the fixnum (l-expr-bit-flags ,l-expr-value))
		,mask))
	     t))
	 (clear-value-code 
	  `(progn
	     (setf (l-expr-bit-flags ,l-expr-value)
	       (logand
		(the fixnum (l-expr-bit-flags ,l-expr-value))
		(the fixnum ,(lognot mask))))
	     nil)))
    `(let ((,l-expr-value ,l-expr))
       ,(if (constantp new-value)
	    (if (eval new-value)
		set-value-code
	      clear-value-code)
	  `(if ,new-value
	       ,set-value-code
	     ,clear-value-code)))))

(defmacro def-l-expr-bit-flag (name bit)
  (let ((setter (intern (format nil "SET-~a" name))))
    `(progn
       (defmacro ,name (l-expr)
	 (list 'test-l-expr-bit-flag l-expr ,bit))
       (defmacro ,setter (l-expr true-p)
	 (list 'set-l-expr-bit-flag l-expr ,bit true-p))
       (defsetf ,name ,setter))))




;;; The macro `clear-l-expr-bits' takes an l-expr and sets all of its bit flags
;;; to NIL.

(defmacro clear-l-expr-bits (l-expr)
  `(progn
     (setf (l-expr-bit-flags ,l-expr) 0)
     nil))



;;; The macro `l-expr-constant-p' takes a l-expr and returns whether or not it
;;; evaluates to a constant.  It is setfable.

(def-l-expr-bit-flag l-expr-constant-p 0)




;;; The macro `l-expr-functional-p' takes a l-expr and returns whether or not it
;;; has been marked as a functional form, i.e. that it's value is determined
;;; completely by its arguments (i.e. it refers to no globals) and it is
;;; side-effect free.

(def-l-expr-bit-flag l-expr-functional-p 1)




;;; The macro `l-expr-side-effect-free-p' takes a l-expr and returns whether or
;;; not it has been marked as side-effect free.

(def-l-expr-bit-flag l-expr-side-effect-free-p 2)






;;;; Making L-exprs




;;; The macro `l-expr-constructor' takes a symbol naming a Lisp operation and
;;; returns either a compiled function that creates l-expr structs for that Lisp
;;; form, or NIL if there is no special constructor for that form type.  If the
;;; given symbol names a function, then the l-expr struct to represent that
;;; function call can be constructed by calling make-function-call-l-expr.  The
;;; l-expr constructors take as arguments the form, the environment surrounding
;;; the form, the augmented environment in place within the body of this form,
;;; and the required type for an evaluation of this form.

(defmacro l-expr-constructor (symbol)
  `(get ,symbol :l-expr-constructor))




;;; The macro `l-expr-struct' takes a symbol naming a Lisp operation and returns
;;; the l-expr struct name for that operation or NIL if none exists.

(defmacro l-expr-struct (symbol)
  `(get ,symbol :l-expr-struct))




;;; The macro `l-expr-struct-number' takes a symbol naming a l-expr struct and
;;; returns a unique fixnum for that struct.

(defconstant maximum-l-expr-struct-number 50)

(defmacro l-expr-struct-number (l-expr-struct-name)
  `(get ,l-expr-struct-name :l-expr-struct-number))






;;;; L-expr Methods




;;; The macro `def-l-expr-generic-method' takes a method name, an argument list,
;;; and a default implementation of that method.  It defines a macro of the
;;; given method name that can very efficiently dispatch to the appropriate
;;; method implementation given the first argument.

(defmacro def-l-expr-generic-method (name arglist &body body)
  (let ((default-method-name (intern (format nil "~a-DEFAULT-METHOD" name)))
	(method-table-var (l-expr-method-table-var-name name)))
    (unless (eq (car arglist) 'l-expr)
      (error "New l-expr generic methods must take at least one arg, and it must be l-expr"))
    `(progn
       (defun ,default-method-name ,arglist ,@body)
       (defvar ,method-table-var
	 (make-array maximum-l-expr-struct-number :initial-element nil))
       (defmacro ,name ,arglist
	 (let* ((struct-var (if (symbolp ,(car arglist))
				,(car arglist)
				(gensym)))
		(method? (gensym))
		(inits (if (not (eq struct-var ,(car arglist)))
			   `((,struct-var ,,(car arglist))))))
	   `(let* (,@inits
		   (,method?
		      (svref ,',method-table-var (l-expr-number ,struct-var))))
	      (if ,method?
		  (funcall ,method? ,struct-var ,,@(cdr arglist))
		  (,',default-method-name ,struct-var ,,@(cdr arglist)))))))))




;;; The macro `def-l-expr-method' takes a method name, an argument list, and a
;;; body of forms to implement that method, and defines an implementation of the
;;; named method for a type of l-expr struct.unsete

(defmacro def-l-expr-method (name arglist &body body)
  (let* ((method-table-var (l-expr-method-table-var-name name))
	 (l-expr-type (car arglist))
	 (method-function-name
	   (intern (format nil "~a-FOR-~a" name l-expr-type))))
    `(progn
       (defun ,method-function-name ,arglist ,@body)
       (cond ((l-expr-struct-number ',l-expr-type)
	      (setf (svref ,method-table-var
			   (l-expr-struct-number ',l-expr-type))
		    #',method-function-name))
	     (t (warn "Def-l-expr-method ~s named ~s, which is not an l-expr type."
		      ',name ',l-expr-type)))
       ',method-function-name)))






;;;; L-Expr Types and Walkers




;;; For each kind of Lisp form that needs a unique handling in translations,
;;; there will be an l-expr type.  The macro def-l-expr defines new l-expr types
;;; and a walk-l-expr method for that type.

;;; The macro `def-l-expr' defines a structure and a walker function for Lisp
;;; expression structures.  The constructor takes a form, an environment, an
;;; augmented environment, and a required type.  All these structures will
;;; inherit from l-expr.  The body forms should either be the definition of the
;;; walker for this structure, or the keyword :default, which means to use the
;;; default walker from the generic walk-l-expr method.

(defmacro def-l-expr (name (l-expr-number slots) &body walker-forms)
  (let* ((struct-name (intern (format nil "~a-L-EXPR" (symbol-name name))))
	 (constructor-name (intern (format nil "MAKE-~a" struct-name))))
    `(progn
       (defstruct (,struct-name
		    (:include l-expr (number ,l-expr-number))
		    (:constructor ,constructor-name (form env aug-env)))
	 ,@slots)
       (setf (l-expr-constructor ',name) #',constructor-name)
       (setf (l-expr-struct ',name) ',struct-name)
       (setf (l-expr-struct-number ',struct-name) ,l-expr-number)
       ,(when (not (equal walker-forms '(:default)))
	  `(def-l-expr-method walk-l-expr
	       (,struct-name function lisp-required-type c-required-type)
	     ,@walker-forms)))))




;;; The l-expr method `walk-l-expr' takes a l-expr, a function, the
;;; lisp-required-type for this l-expr and the c-required-type for this l-expr.
;;; This method will walk through the given l-expr and its subforms calling the
;;; given walker function in a bottom up manner.  For each walked l-expr, the
;;; value returned by the walker function will replace the given l-expr within
;;; its superior form.

(def-l-expr-generic-method walk-l-expr
    (l-expr function lisp-required-type c-required-type)
  (walk-l-expr-subforms
    (cons-cdr (l-expr-form l-expr)) function lisp-required-type c-required-type)
  (funcall function l-expr lisp-required-type c-required-type))




;;; The function `walk-l-expr-subforms' takes a list of l-exprs, a function, and
;;; the extra walker arguments.  This function walks each l-expr in the list,
;;; replacing it with the result of the walker function.  This is intended to be
;;; used as a utility function for those walkers that need to walk a progn body.

(defun walk-l-expr-subforms (subforms function lisp-required-type c-required-type)
  (loop for subform-cons on subforms
	for next-subform-cons = (cons-cdr subform-cons)
	for subform = (cons-car subform-cons)
	for last-form? = (null next-subform-cons)
	for walked-subform
	    = (walk-l-expr
		subform function
		(if last-form? lisp-required-type 'void)
		(if last-form? c-required-type 'void))
	do
    (when (not (eq subform walked-subform))
      (setf (car subform-cons) walked-subform)))
  nil)




;;; The function `walk-block-like-l-expr' takes an l-expr, a function, and the
;;; two extra args for walkers, and it walks the given l-expr as you would for
;;; block, in other words ignoring the first argument to the form.

(defun walk-block-like-l-expr (l-expr function lisp-required-type c-required-type)
  (walk-l-expr-subforms
    (cons-cddr (l-expr-form l-expr)) function lisp-required-type c-required-type)
  (funcall function l-expr lisp-required-type c-required-type))




;;; The generic l-expr method `initialize' is used to initialize new l-expr
;;; instances.  By default there is no initialization needed.  This method
;;; should side effect the l-expr with whatever initializations are necessary.
;;; Currently, the initializations are to remove declares or cache environment
;;; information.

(def-l-expr-generic-method initialize-l-expr (l-expr)
  (declare (ignore l-expr))
  nil)

(defun remove-declares-from-progn-like-form (form)
  (multiple-value-bind (decls body)
      (split-declarations-and-body (cons-cdr form))
    (setf (cdr form) body)
    decls))

(defmacro remove-declares-from-let-like-form (form)
  `(remove-declares-from-progn-like-form (cons-cdr ,form)))

(defmacro remove-declares-from-defun-like-form (form)
  `(remove-declares-from-progn-like-form (cons-cddr ,form)))


  





;;; The following section implements the l-expr types and walker functions.
;;; Comments are only given where the l-expr is not obvious.

(def-l-expr tl:block (1 nil)
  (let* ((form (l-expr-form block-l-expr))
	 (exit-scope (tl:declaration-information
		       'exit-scope (l-expr-aug-env block-l-expr)))
	 (block-name (cons-second form))
	 (block-struct?
	   (loop for entry in exit-scope
		 when (and (eq (cons-car entry) 'block)
			   (eq (cons-second entry) block-name))
		   return (cons-third entry))))
    (cond (block-struct?
	   (setf (block-scope-lisp-required-type block-struct?)
		 lisp-required-type)
	   (setf (block-scope-c-required-type block-struct?) c-required-type)
	   (setf (block-scope-lisp-return-type block-struct?) nil)
	   (setf (block-scope-c-return-type block-struct?) nil)
	   (setf (block-scope-l-expr-return-from-count block-struct?) 0))
	  (t
	   (translation-error
	     "Block ~s had no corresponding block structure." block-name)))
    (walk-block-like-l-expr
      block-l-expr function lisp-required-type c-required-type)))




;;; Since the throw calls can return type *, then the body of a catch form will
;;; also be compiled with type * and Obj, unless the values from this catch form
;;; are ignored.  If the type is void, then the body is compiled requiring that
;;; type.

(def-l-expr tl:catch (2 nil)
  (let ((form (l-expr-form catch-l-expr))
	(l-type (if (eq lisp-required-type 'void) 'void '*))
	(c-type (if (eq c-required-type 'void) 'void 'obj)))
    (setf (second form) (walk-l-expr (cons-second form) function 't 'obj))
    (walk-l-expr-subforms (cons-cddr form) function l-type c-type)
    (funcall function catch-l-expr l-type c-type)))


(def-l-expr tl:declare (3 nil)
  (declare (ignore function lisp-required-type c-required-type))
  (translation-error
    "A declare form ~s became structurized as an l-expr.  Either the ~
     declare was misplaced or the surrounding form has mishandled it."
    (l-expr-form declare-l-expr)))

(def-l-expr tl:eval-when (4 nil)
  (walk-block-like-l-expr
    eval-when-l-expr function lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (eval-when-l-expr)
  (let ((form (l-expr-form eval-when-l-expr)))
    (setf (second form)
	  (loop for situation in (second form)
		collect (or (cdr (assq situation 
				       #+lucid
				       '((tl:compile . compile)
					 (tl:load . load)
					 (tl:eval . eval))
				       #-lucid
				       '((tl:compile . :compile-toplevel)
					 (tl:load    . :load-toplevel)
					 (tl:eval    . :execute)
					 (compile    . :compile-toplevel)
					 (load       . :load-toplevel)
					 (eval       . :execute))))
			    situation)))))




;;; The `flet-l-expr' struct is walked like block, since the function forms have
;;; already been split off into separate top-level named-lambdas which will be
;;; walked on their own.

(def-l-expr tl:flet (5 nil)
  (walk-block-like-l-expr
    flet-l-expr function lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (flet-l-expr)
  (remove-declares-from-let-like-form (l-expr-form flet-l-expr)))

(def-l-expr named-lambda (6 (decls))
  (let* ((form (l-expr-form named-lambda-l-expr))
	 (decls (named-lambda-l-expr-decls named-lambda-l-expr))
	 (ftype (or (cdr (assq 'ftype decls))
		    (cdr (assq 'computed-ftype decls))))
	 (ftype-return-type (cons-third ftype))
	 (c-return-type (c-func-type-for-lisp-func-return-type-spec
			  ftype-return-type)))
    (walk-l-expr-subforms
      (cons-cdddr form) function ftype-return-type c-return-type)
    (funcall function named-lambda-l-expr lisp-required-type c-required-type)))

(def-l-expr-method initialize-l-expr (named-lambda-l-expr)
  (let* ((form (l-expr-form named-lambda-l-expr))
	 (name (cons-second form))
	 (env (l-expr-env named-lambda-l-expr)))
    (multiple-value-bind (func-type? local? decls)
	(tl:function-information name env)
      (declare (ignore local?))
      (unless (eq func-type? :function)
	(translation-error
	  "Can't translate function ~s, no ftype or computed ftype declaration.  ~
           Perhaps this function wasn't loaded into this Lisp environment."
	  name))
      (setf (named-lambda-l-expr-decls named-lambda-l-expr) decls)
      (remove-declares-from-defun-like-form form))))

(def-l-expr tl:function (7 nil)
  (funcall function function-l-expr lisp-required-type c-required-type))

(def-l-expr tl:go (8 nil)
  (funcall function go-l-expr lisp-required-type c-required-type))

(def-l-expr tl:if (9 nil)
  (let* ((form (l-expr-form if-l-expr))
	 (test (cons-cdr form))
	 (then (cons-cdr test))
	 (else? (cons-cdr then)))
    (setf (car test) (walk-l-expr (cons-car test) function 't 'boolean))
    (setf (car then)
	  (walk-l-expr (cons-car then) function
		       lisp-required-type c-required-type))
    (when else?
      (setf (car else?)
	    (walk-l-expr (cons-car else?) function
			 lisp-required-type c-required-type)))
    (funcall function if-l-expr lisp-required-type c-required-type)))

(def-l-expr tl:labels (10 nil)
  (walk-block-like-l-expr labels-l-expr function lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (labels-l-expr)
  (remove-declares-from-let-like-form (l-expr-form labels-l-expr)))

(defun walk-let-like-l-expr (l-expr function lisp-required-type c-required-type)
  (loop with aug-env = (l-expr-aug-env l-expr)
	for binding in (cons-second (l-expr-form l-expr)) do
    (when (and (consp binding) (second binding))
      (let* ((var (cons-car binding))
	     (struct (variable-decl var 'variable-binding-structure aug-env))
	     (lisp-type (variable-binding-lisp-type struct))
	     (c-type (variable-binding-c-type struct)))
	(when (env-requires-volatile-p aug-env)
	  (setf (variable-binding-volatile struct) t))
	(setf (second binding)
	      (walk-l-expr (cons-second binding) function lisp-type c-type)))))
  (walk-block-like-l-expr l-expr function lisp-required-type c-required-type))

(def-l-expr tl:let (11 nil)
  (walk-let-like-l-expr let-l-expr function lisp-required-type c-required-type))

(defun initialize-let-like-l-expr (let-like-l-expr)
  (let ((form (l-expr-form let-like-l-expr))
	(aug-env (l-expr-aug-env let-like-l-expr))
	lisp-type c-type)
    (remove-declares-from-let-like-form form)
    (loop for (var) in (cons-second form)
	  for binding = (variable-decl var 'variable-binding-structure aug-env)
	  do
      (multiple-value-setq (lisp-type c-type)
	(choose-variable-type-given-walked-env var aug-env))
      (setf (variable-binding-lisp-type binding) lisp-type)
      (setf (variable-binding-c-type binding) c-type))))

(def-l-expr-method initialize-l-expr (let-l-expr)
  (initialize-let-like-l-expr let-l-expr))

(def-l-expr tl:let* (12 nil)
  (walk-let-like-l-expr let*-l-expr function lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (let*-l-expr)
  (initialize-let-like-l-expr let*-l-expr))

(def-l-expr tl:macrolet (13 nil)
  (walk-block-like-l-expr
    macrolet-l-expr function lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (macrolet-l-expr)
  (remove-declares-from-let-like-form (l-expr-form macrolet-l-expr)))

(def-l-expr tl:multiple-value-bind (14 nil)
  (let* ((form (l-expr-form multiple-value-bind-l-expr))
	 (aug-env (l-expr-aug-env multiple-value-bind-l-expr))
	 (volatile? (env-requires-volatile-p aug-env))
	 (var-types
	   (loop for var in (cons-second form)
		 for struct = (variable-decl
				var 'variable-binding-structure aug-env)
		 do
	     (when volatile?
	       (setf (variable-binding-volatile struct) t))
		 collect (variable-binding-lisp-type struct)))
	 (lisp-type (cond ((null var-types)
			   'void)
			  ((null (cons-cdr var-types))
			   (cons-car var-types))
			  (t
			   (cons 'values var-types))))
	 (c-type (c-type-for-lisp-type (or (car var-types) 'void))))
    (setf (third form)
	  (walk-l-expr (cons-third form) function lisp-type c-type))
    (walk-l-expr-subforms
      (cons-cdddr form) function lisp-required-type c-required-type)
    (funcall function multiple-value-bind-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr-method initialize-l-expr (multiple-value-bind-l-expr)
  (let ((form (l-expr-form multiple-value-bind-l-expr))
	(aug-env (l-expr-aug-env multiple-value-bind-l-expr))
	lisp-type c-type)
    (remove-declares-from-defun-like-form form)
    ;; For multiple-value-bind, do not attempt to infer the types of the
    ;; variables, just go straight with the declarations given, if any.  The
    ;; reason is that for initialization forms we are getting a lot of
    ;; translation failures when secondary variables are getting inferred types
    ;; that are too aggressive.
    (loop for var in (cons-second form)
	  for binding = (variable-decl var 'variable-binding-structure aug-env)
	  do
      (multiple-value-setq (lisp-type c-type)
	(choose-variable-type-from-declarations var aug-env))
      (setf (variable-binding-lisp-type binding) lisp-type)
      (setf (variable-binding-c-type binding) c-type))))

(def-l-expr tl:multiple-value-prog1 (15 nil)
  (let* ((form (l-expr-form multiple-value-prog1-l-expr))
	 (value-cons (cons-cdr form)))
    (setf (car value-cons)
	  (walk-l-expr
	    (cons-car value-cons) function lisp-required-type c-required-type))
    (loop for form-cons on (cons-cdr value-cons)
	  do
      (setf (car form-cons)
	    (walk-l-expr (cons-car form-cons) function 'void 'void)))
    (funcall function multiple-value-prog1-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr tl:progn (16 nil) :default)

(def-l-expr tl:quote (17 nil)
  (funcall function quote-l-expr lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (quote-l-expr)
  (let ((constant-value (eval (l-expr-form quote-l-expr))))
    ;; If the constant is a string with a fill-pointer (which would be true if
    ;; this were a computed constant from a macro expansion, replace it with a
    ;; simple-string.  There is no change in semantics since TL string all
    ;; logically have fill pointers, but the TL C emitters use simple strings.
    (when (and (stringp constant-value)
	       (not (simple-string-p constant-value)))
      (let ((new-string (make-string (length constant-value))))
	(replace new-string constant-value)
	(setq constant-value new-string)))
    (setf (l-expr-form quote-l-expr) constant-value)

    ;; The following error can occur when a special form walker returns the result
    ;; of walking the form to be returned instead of returning a form whose
    ;; subparts have been walked.
    (when (l-expr-p (l-expr-form quote-l-expr))
      (translation-error
	"Internal translator bug: the special form surrounding ~s returned a ~
        walked form instead of a form with walked subparts."
	(l-expr-form quote-l-expr)))))

(def-l-expr tl:return-from (18 nil)
  (let* ((form (l-expr-form return-from-l-expr))
	 (env (l-expr-env return-from-l-expr))
	 (exit-scope (tl:declaration-information 'exit-scope env))
	 (block-name (cons-second form))
	 (intervening-unwind-protect? nil)
	 (block-struct?
	   (loop for entry in exit-scope
		 for type = (cons-car entry)
		 do
	     (cond ((eq type 'block)
		    (when (eq (cons-second entry) block-name)
		      (return (cons-third entry))))
		   ((eq type 'unwind-protect)
		    (setq intervening-unwind-protect? t))))))
    (when block-struct?
      (incf (block-scope-l-expr-return-from-count block-struct?))
      (when (env-requires-volatile-p env)
	(setf (block-scope-volatile-return-from block-struct?) t)))
    (setf (third form)
	  (walk-l-expr
	    (cons-third form)
	    function
	    (if block-struct?
		(block-scope-lisp-required-type block-struct?)
		'*)
	    (if (and block-struct? (not intervening-unwind-protect?))
		(block-scope-c-required-type block-struct?)
		'obj)))
    (funcall function return-from-l-expr lisp-required-type c-required-type)))

(def-l-expr tl:setq (19 nil)
  (let* ((form (l-expr-form setq-l-expr))
	 (symbol (cons-second form))
	 (env (l-expr-env setq-l-expr))
	 (decls (multiple-value-bind (bind-type? local? declarations)
		    (tl:variable-information symbol env)
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
	       (or declared-type? 't))))
    ;; When setting a variable within a scope that allows unwind-protects, mark
    ;; the variable as needing a volatile storage class.
    (when (and binding? (env-requires-volatile-p env))
      (setf (variable-binding-volatile binding?) t))

    (setf (third form)
	  (walk-l-expr
	    (cons-third form)
	    function
	    lisp-type
	    (if binding? (variable-binding-c-type binding?) 'obj)))
    (funcall function setq-l-expr lisp-required-type c-required-type)))

(def-l-expr tl:tagbody (20 nil)
  (let ((form (l-expr-form tagbody-l-expr)))
    ;; First, walk the body eliminating all dead code between unconditional go
    ;; statements and the next tag where processing could start again.
    (loop with form-cons = (cons-cdr form)
	  while form-cons do
      (let ((subform (cons-car form-cons)))
	(if (and (or (go-l-expr-p subform)
		     (return-from-l-expr-p subform))
		 (l-expr-p (car (cons-cdr form-cons))))
	    (setf (cdr form-cons) (cons-cddr form-cons))
	  (setq form-cons (cons-cdr form-cons)))))
    ;; Next, walk all the non-tag subforms.
    (loop for form-cons on (cons-cdr form)
	  for subform = (cons-car form-cons)
	  do
      (when (l-expr-p subform)
	(let ((walked-subform (walk-l-expr subform function 'void 'void)))
	  (when (not (eq subform walked-subform))
	    (setf (car form-cons) walked-subform)))))
    (funcall function tagbody-l-expr lisp-required-type c-required-type)))

(defun combine-required-and-declared-type (required-type declared-type)
  (let* ((values-count-required? (type-includes-values-p required-type))
	 (values-count-declared? (type-includes-values-p declared-type)))
    (if values-count-required?
	(if values-count-declared?
	    (duplicate-type-declaration required-type declared-type)
	    (list 'values (duplicate-type-declaration
			    (type-of-first-value required-type)
			    declared-type)))
	(if values-count-declared?
	    (duplicate-type-declaration
	      required-type (type-of-first-value declared-type))
	    (duplicate-type-declaration required-type declared-type)))))

(def-l-expr tl:the (21 nil)
  (walk-block-like-l-expr
    the-l-expr
    function
    (combine-required-and-declared-type lisp-required-type (cons-second (l-expr-form the-l-expr)))
    c-required-type))

(def-l-expr tl:throw (22 nil)
  (let ((form (l-expr-form throw-l-expr)))
    (setf (second form)
	  (walk-l-expr (cons-second form) function 't 'obj))
    (setf (third form)
	  (walk-l-expr (cons-third form) function '* 'obj))
    (funcall function throw-l-expr lisp-required-type c-required-type)))

(def-l-expr tl:unwind-protect (23 nil)
  (let ((form (l-expr-form unwind-protect-l-expr)))
    (setf (second form)
	  (walk-l-expr
	    (cons-second form) function lisp-required-type c-required-type))
    (walk-l-expr-subforms (cons-cddr form) function 'void 'void)
    (funcall function unwind-protect-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr tl:symbol-macrolet (24 nil)
  (walk-block-like-l-expr symbol-macrolet-l-expr function
			  lisp-required-type c-required-type))

(def-l-expr tl:locally (25 nil) :default)

(def-l-expr-method initialize-l-expr (locally-l-expr)
  (remove-declares-from-progn-like-form (l-expr-form locally-l-expr)))

(def-l-expr fixnum-case (26 nil)
  (let ((form (l-expr-form fixnum-case-l-expr)))
    (setf (second form)
	  (walk-l-expr (cons-second form) function 'fixnum 'sint32))
    (loop for clause in (cons-cddr form) do
      (walk-l-expr-subforms (cons-cdr clause) function
			    lisp-required-type c-required-type))
    (funcall function fixnum-case-l-expr lisp-required-type c-required-type)))

(def-l-expr while-loop (27 nil)
  (let ((form (l-expr-form while-loop-l-expr)))
    (setf (second form)
	  (walk-l-expr (cons-second form) function 't 'boolean))
    (walk-l-expr-subforms (cons-cddr form) function 'void 'void)
    (funcall function while-loop-l-expr lisp-required-type c-required-type)))

(def-l-expr for-loop (28 nil)
  (let* ((form (l-expr-form for-loop-l-expr))
	 (inits (cons-second form)))
    (when (cons-car inits)
      (setf (car inits) (walk-l-expr (cons-car inits) function 'void 'void)))
    (when (cons-second inits)
      (setf (second inits)
	    (walk-l-expr (cons-second inits) function 't 'boolean)))
    (when (cons-third inits)
      (setf (third inits)
	    (walk-l-expr (cons-third inits) function 'void 'void)))
    (walk-l-expr-subforms (cons-cddr form) function 'void 'void)
    (funcall function for-loop-l-expr lisp-required-type c-required-type)))




;;; The `function-call-l-expr' structure represents function calls.  It contains
;;; extra slots for the function information for the called function.

(def-l-expr function-call (29 (local-p decls local-function translator?))
  (let* ((form (l-expr-form function-call-l-expr))
	 (name (cons-car form))
	 (decls (function-call-l-expr-decls function-call-l-expr))
	 (ftype (or (cdr (assq 'ftype decls))
		    (cdr (assq 'computed-ftype decls)))))
    (unless ftype
      (cond
	((fboundp name)
	 (translation-error
	   "Can't translate a call to ~s, even though it is defined as a ~
              function in Lisp."
	   name))
	((macro-function name)
	 (translation-error
	   "Can't translate a call to ~s, it is a macro." name))
	((lisp-special-operator-p name)
	 (translation-error
	   "Can't translate a call to ~s, which is a Lisp special form."
	   name))
	(t
	 (translation-error
	   "Can't translate a call to ~s, it is undefined." name))))
    (loop for arg-cons on (cons-cdr form)
	  for arg-type in (remove '&optional (cons-second ftype) :test #'eq)
	  for arg = (cons-car arg-cons)
	  for walked-arg
	      = (walk-l-expr arg function arg-type
			     (c-type-for-lisp-type arg-type))
	  do
      (when (not (eq arg walked-arg))
	(setf (car arg-cons) walked-arg)))
    (funcall function function-call-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr-method initialize-l-expr (function-call-l-expr)
  (let* ((form (l-expr-form function-call-l-expr))
	 (name (cons-car form))
	 (arg-count (length (cons-cdr form)))
	 (env (l-expr-env function-call-l-expr)))
    (declare (fixnum arg-count))
    (multiple-value-bind (func-type? local? decls local-func?)
	(tl:function-information name env)
      (unless (eq func-type? :function)
	(translation-error
	  "Calling ~s, which does not name a defined function."
	  name))
      (when (cdr (assq 'tl:conser decls))
	(check-area-for-conser-call name env))
      (let* ((ftype (or (cdr (assq 'ftype decls))
			(cdr (assq 'computed-ftype decls))))
	     (arg-spec (second ftype))
	     (needed-arg-count (length arg-spec))
	     (optionals? (memq '&optional arg-spec)))
	(declare (fixnum needed-arg-count))
	(cond
	  (optionals?
	   (decf needed-arg-count)
	   (cond
	     ((< arg-count needed-arg-count)
	      (let* ((optional-types (cons-cdr optionals?))
		     (optional-arg-count (length optional-types))
		     (default-optional-args
			 (cdr (assq 'optional-arg-default-values decls)))
		     (minimum-arg-count
		       (- needed-arg-count optional-arg-count)))
		(declare (fixnum optional-arg-count minimum-arg-count))
		(cond
		  ((>= arg-count minimum-arg-count)
		   (setf
		     (l-expr-form function-call-l-expr)
		     (append
		       form
		       (loop for default-form in default-optional-args
			     for arg-cons
				 = (nthcdr minimum-arg-count (cdr form))
				 then (cdr arg-cons)
			     when (null arg-cons)
			       collect
			       ;; Using funcall to avoid the forward reference.
			       (tl:walk
				 default-form (l-expr-env function-call-l-expr)
				 'walk-form-into-l-expr
				 't)))))
		  (t
		   (translation-warning
		     "Calling ~s with too few arguments in the form ~s"
		     name (l-expr-pretty-form function-call-l-expr))))))
	     ((> arg-count needed-arg-count)
	      (translation-warning
		"Calling ~s with too many arguments in the form ~s"
		name (l-expr-pretty-form function-call-l-expr)))))
	  ((/= arg-count needed-arg-count)
	   (translation-warning
	     "Calling ~s with too ~a arguments in the form ~s"
	     name
	     (if (> arg-count needed-arg-count) "many" "few")
	     (l-expr-pretty-form function-call-l-expr))))
	(setf (function-call-l-expr-local-p function-call-l-expr) local?)
	(setf (function-call-l-expr-decls function-call-l-expr) decls)
	(setf (function-call-l-expr-local-function function-call-l-expr)
	      local-func?)
	(setf (function-call-l-expr-translator? function-call-l-expr) nil)))))




;;; The `implicit-symbol-value-l-expr' structure represents references to the
;;; value of a variable.  The form is the symbol being referenced.  It contains
;;; extra slots which are the variable-information for the referenced variable.

(def-l-expr implicit-symbol-value (30 (binding-type local-binding decls))
  (funcall function implicit-symbol-value-l-expr lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (implicit-symbol-value-l-expr)
  (let ((symbol (l-expr-form implicit-symbol-value-l-expr)))
    (multiple-value-bind (binding-type? local-binding decl-alist)
	(tl:variable-information
	  symbol (l-expr-env implicit-symbol-value-l-expr))
      (unless binding-type?
	(setq binding-type? :special)
	(translation-warning
	  "Referencing variable ~s, which is undefined in this scope, assuming it's special."
	  symbol))
      (setf (implicit-symbol-value-l-expr-binding-type
	      implicit-symbol-value-l-expr)
	    binding-type?)
      (setf (implicit-symbol-value-l-expr-local-binding
	      implicit-symbol-value-l-expr)
	    local-binding)
      (setf (implicit-symbol-value-l-expr-decls
	      implicit-symbol-value-l-expr)
	    decl-alist))))




;;; The `coerce-to-type' l-expr structure is inserted into l-expr trees during
;;; type propagation and preparation for translations when the Lisp or C return
;;; types of an l-expr don't satisfy the required types.  The translation of
;;; this l-expr then needs to either emit the appropriate operations to coerce
;;; the type or signal a translation error.

(def-l-expr coerce-to-type (31 (lisp-required-type c-required-type))
  (let ((form (l-expr-form coerce-to-type-l-expr)))
    (setf (second form)
	  (walk-l-expr (cons-second form) function
		       lisp-required-type c-required-type))
    (funcall function coerce-to-type-l-expr
	     lisp-required-type c-required-type)))




;;; The `tl:values' l-expr structure represents a call to values.  Note that
;;; when the required type does not need the values count to be returned, then
;;; this should behave just like a prog1, passing through the first value,
;;; discarded the remainder, and ignoring the values count since the caller of
;;; this operation intends to ignore the values count anyway.

(def-l-expr tl:values (32 nil)
  (let ((args (cons-cdr (l-expr-form values-l-expr))))
    (cond
      ((and (consp lisp-required-type)
	    (eq (cons-car lisp-required-type) 'values))
       (loop for arg-cons on args
	     for lisp-type-cons = (cons-cdr lisp-required-type)
				then (cdr lisp-type-cons)
	     for lisp-type = (or (car lisp-type-cons) 'void)
	     for c-type = c-required-type
			then (if (eq lisp-type 'void) 'void 'obj)
	     do
	 (setf (car arg-cons)
	       (walk-l-expr (cons-car arg-cons) function
			    (type-of-first-value lisp-type) c-type))))
      ((eq lisp-required-type '*)
       (loop for arg-cons on args
	     for c-type = c-required-type then 'obj
	     do
	 (setf (car arg-cons)
	       (walk-l-expr (cons-car arg-cons) function 't c-type))))
      (t
       (loop for arg-cons on args
	     for lisp-type = lisp-required-type then 'void
	     for c-type = c-required-type then 'void
	     do
	 (setf (car arg-cons)
	       (walk-l-expr (cons-car arg-cons) function lisp-type c-type)))))
    (funcall function values-l-expr lisp-required-type c-required-type)))




;;; The `def-named-variable' l-expr structure is used to define variables,
;;; parameters, and constants.

(def-l-expr def-named-variable (33 nil)
  (let* ((form (l-expr-form def-named-variable-l-expr))
	 (name (cons-second form))
	 (init-cons (cons-cdddr form))
	 (init (cons-car form))
	 (env (l-expr-env def-named-variable-l-expr))
	 (lisp-type (or (variable-decl name 'type env))))
    (unless (eq init no-initial-value)
      (setf (car init-cons)
	    (walk-l-expr init function lisp-type 'obj)))
    (funcall function def-named-variable-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr inlined-typep (34 nil)
  (let ((object-cons (cons-cdr (l-expr-form inlined-typep-l-expr))))
    (setf (car object-cons)
	  (walk-l-expr (cons-car object-cons) function 't 'obj)))
  (funcall function inlined-typep-l-expr
	   lisp-required-type c-required-type))

(def-l-expr-method initialize-l-expr (inlined-typep-l-expr)
  (let ((type-cons (cons-cddr (l-expr-form inlined-typep-l-expr))))
    (setf (car type-cons)
	  (eval (car type-cons)))))

(def-l-expr tl:and (35 nil)
  (loop with test-only? = (satisfies-c-required-type-p c-required-type 'boolean)
	for arg-cons on (cons-cdr (l-expr-form and-l-expr))
	for test-arg? = (or test-only? (cons-cdr arg-cons))
	do
    (setf (car arg-cons)
	  (walk-l-expr (cons-car arg-cons) function
		       (if test-arg? 't lisp-required-type)
		       (if test-arg? 'boolean 'obj))))
  (funcall function and-l-expr lisp-required-type c-required-type))

(def-l-expr tl:or (36 nil)
  (loop with test-only? = (satisfies-c-required-type-p c-required-type 'boolean)
	for arg-cons on (cons-cdr (l-expr-form or-l-expr))
	for test-arg? = (or test-only? (cons-cdr arg-cons))
	do
    (setf (car arg-cons)
	  (walk-l-expr (cons-car arg-cons) function
		       (if test-arg? t lisp-required-type)
		       (if test-only? 'boolean 'obj))))
  (funcall function or-l-expr lisp-required-type c-required-type))

(def-l-expr list-dynamic-extent (37 nil)
  (loop for arg-cons on (cons-cdr (l-expr-form list-dynamic-extent-l-expr))
	do
    (setf (car arg-cons)
	  (walk-l-expr (cons-car arg-cons) function t 'obj)))
  (funcall function list-dynamic-extent-l-expr
	   lisp-required-type c-required-type))

(def-l-expr funcall-internal (38 nil)
  (let* ((form (l-expr-form funcall-internal-l-expr))
	 (compiled-function-cons (cons-cddr form)))
    (setf (car compiled-function-cons)
	  (walk-l-expr (cons-car compiled-function-cons) function
		       'compiled-function 'obj))
    (loop for arg-cons on (cons-cdr compiled-function-cons) do
      (setf (car arg-cons)
	    (walk-l-expr (cons-car arg-cons) function t 'obj)))
    (funcall function funcall-internal-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr tl:c-comment-form (39 nil)
  (let ((arg-cons (cons-cddr (l-expr-form c-comment-form-l-expr))))
    (setf (car arg-cons)
	  (walk-l-expr (cons-car arg-cons) function
		       lisp-required-type c-required-type))
    (funcall function c-comment-form-l-expr
	     lisp-required-type c-required-type)))

(def-l-expr malloc-class-instance (40 nil)
  (funcall function malloc-class-instance-l-expr 
	   lisp-required-type c-required-type))

(def-l-expr get-slot (41 nil)
  (let ((arg-cons (cons-cdr (l-expr-form get-slot-l-expr))))
    (destructuring-bind (struct name arg-lisp-type arg-c-type 
				value-type value-c-type)
			arg-cons
      (declare (ignore name value-type value-c-type))
      (setf (car arg-cons)
	    (walk-l-expr struct function arg-lisp-type arg-c-type))
      (funcall function get-slot-l-expr 
	       lisp-required-type c-required-type))))

(def-l-expr set-slot (42 nil)
  (let ((arg-cons (cons-cdr (l-expr-form set-slot-l-expr))))
    (destructuring-bind (struct name arg-lisp-type arg-c-type 
				held-lisp-type held-c-type new-value)
			arg-cons
      (let ((slot-c-type (or held-c-type
			     (and (symbolp name) 
				  (class-type-p arg-lisp-type)
				  (get-c-type-for-class-and-slot 
				   arg-lisp-type name))
			     'obj)))
	(setf (car arg-cons)
	      (walk-l-expr struct function arg-lisp-type arg-c-type))
	(setf (car (last arg-cons))
	      (walk-l-expr new-value function held-lisp-type slot-c-type))
	(funcall function set-slot-l-expr
		 lisp-required-type c-required-type)))))






;;;; L-expr Constant Values




;;; The macro `l-expr-constant-value' returns the constant stored in one of
;;; these l-exprs.  All l-exprs that can set the l-expr-constant-p bit in
;;; L-CONST must be listed in this function.

(defun l-expr-constant-value (l-expr)
  (cond ((quote-l-expr-p l-expr)
	 (l-expr-form l-expr))
	((the-l-expr-p l-expr)
	 (l-expr-constant-value (cons-third (l-expr-form l-expr))))
	((coerce-to-type-l-expr-p l-expr)
	 (l-expr-constant-value (cons-second (l-expr-form l-expr))))
	((implicit-symbol-value-l-expr-p l-expr)
	 (symbol-value (l-expr-form l-expr)))
	((c-comment-form-l-expr-p l-expr)
	 (l-expr-constant-value (cons-third (l-expr-form l-expr))))
	(t
	 (translation-error "~s doesn't have a l-expr-constant-value."
			    l-expr))))
