(in-package "TLI")

;;;; Module SPECIAL

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






;;;; Special Forms




;;; This module contains definitions of handling functions for special forms.
;;; There are several purposes that must be served by the handling forms
;;; presented here.  The first is propagation down into the subforms for
;;; mexp-all.  The second purpose is for propagation down into subforms during
;;; structurize-all, and the last purpose is a generalized walking form for
;;; calling a function on all macroexpanded subforms of a given form.

;;; Each TL special form will have a macroexpander that expands into the Lisp
;;; version of the same form.  This allows us to use the underlying Lisp
;;; compiler on forms that have been macroexpanded to functions and TL special
;;; forms.  The function tl:macro-function will decline to return the
;;; macro-function for any TL special form, thus preventing any of our
;;; macroexpanders from expanding it.

;;; The macro `declare-tl-special-forms' takes an &rest argument of symbols and
;;; declaims that these name special forms, and it also defines macros that will
;;; expand calls to this forms in underlying Lisp systems into the special forms
;;; with the same name in the TLI package, which uses the underlying Lisp.

(defmacro declare-tl-special-forms (&rest special-form-names)
  `(progn
     (tl:declaim (special-form ,@(loop for arg in special-form-names
				     collect (if (consp arg) (car arg) arg))))
     ,@(loop for name-or-form in special-form-names
	   for name = (if (consp name-or-form) (car name-or-form) name-or-form)
	   for lisp-name = (intern (symbol-name name) *tli-package*)
	   when (not (eq name lisp-name)) ; some symbols are imported into TL
	   collect
	     (if (consp name-or-form)
		 (cons 'defmacro name-or-form)
	       `(defmacro ,name (&rest forms)
		  (cons ',lisp-name forms))))))

(declare-tl-special-forms
  tl:block tl:catch tl:declare tl:go tl:if
  tl:flet tl:labels
  tl:let tl:let* tl:macrolet
  ;; multiple-value-call not implemented
  tl:multiple-value-bind tl:values
  tl:multiple-value-prog1 tl:progn
  ;; progv not implemented
  tl:quote tl:return-from tl:setq tl:throw tl:unwind-protect
  ;; generic-flet, generic-labels not implemented
  ;; with-added-methods not implemented
  tl:locally
  ;; load-time-eval not implemented
  tl:and tl:or
  )




;;; The function `special-form-p' is a non-translatable TL function that returns
;;; whether or not a symbol is a TL special-form.

(defun tl:special-form-p (symbol)
  (eq (tl:function-information symbol) :special-form))




;;; The macro `def-special-form-walker' is used to define functions that perform
;;; code walking across special forms.  It takes a name, a lambda-list of three
;;; arguments, and a body of forms.  The function defined will be called with a
;;; form, an environment, and a walker-function.  This function should augment
;;; the environment in whatever way is required by this special form, call
;;; tl:walk on all subforms, and return two values.  The first value is an
;;; s-expression within orginal subforms replaced by the result of calling
;;; tl:walk on them.  The second value is the augmented environment in effect
;;; for walking the subforms, or NIL if the subforms were walked with the env
;;; given to the walker function.  (If there are several environments that could
;;; be returned, the most augmented should generally be chosen.)  The
;;; walker-function is only passed on to calls to tl:walk from this function,
;;; the caller of this function is reponsible for calling the walker function on
;;; the result of this function.

(defmacro def-special-form-walker (name lambda-list &body body)
  (let ((function-name (intern (format nil "WALK-~a" name) *tli-package*)))
    `(progn
       (tl:declaim (special-form-walker ,name ,function-name))
       (defun ,function-name ,lambda-list ,@body))))




;;; The bogus variable `trivial-lisp-result-type-function' is used to hold
;;; the compiled function for this forwardly referenced facility.  This
;;; approach avoids forward reference warnings for all Lisp
;;; implementations.

(defvar trivial-lisp-result-type-function 'trivial-lisp-result-type-if-l-expr)



;;; The function `expression-result-type' takes an s-expression and attempts to
;;; determine the type of the values that it can return.  If it cannot make a
;;; determination, then it returns NIL.

(defun expression-result-type (expr env)
  (cond
    ((null expr)
     'null)
    ((symbolp expr)
     (let ((type? (variable-decl expr 'type env)))
       (if type?
	   (upgraded-optimizable-type type?)
	   t)))
    ((atom expr)
     ;; While we are translating, l-exprs can be given as "constants", since
     ;; they are produced while walking translatable forms.  In the case where
     ;; it is an l-expr, attempt to determine the result-type.  If this can't be
     ;; done, then assume this is a true Common Lisp constant and get its type.
     (let ((l-expr-type? (funcall trivial-lisp-result-type-function expr)))
       (upgraded-optimizable-type
	 (or l-expr-type?
	     (tl-type-of (eval expr))))))
    ((and (consp expr)
	  (symbolp (car expr)))
     (let ((operator (car expr)))
       (cond ((eq operator 'tl:the)
	      (upgraded-optimizable-type (second expr)))
	     ((eq operator 'tl:if)
	      (let* ((then-cons (cons-cddr expr))
		     (else-cons? (cons-cdr then-cons)))
		(most-specific-common-supertype
		  (expression-result-type (cons-car then-cons) env)
		  (if else-cons?
		      (expression-result-type (cons-car else-cons?) env)
		      'null)
 		  t)))
	     ((memqp operator '(tl:progn tl:let tl:let* tl:multiple-value-bind))
	      (expression-result-type (car (last expr)) env))
	     ((eq operator 'tl:multiple-value-prog1)
	      (expression-result-type (cons-second expr) env))
	     (t
	      (let ((ftype? (function-decl operator 'ftype env)))
		(if ftype?
		    (cons-third ftype?)
		    '*))))))
    (t '*)))






;;;; Special Form Walkers




;;; This section implements the walker functions for all TL special forms.

;;; The function `walk-progn-body' is used by many of the special form walkers
;;; to walk a list of forms where the result of the last of them must satisfy
;;; the type given by the required type.  The results all forms but the last are
;;; discarded, so they are walked with a required type of void.

(defun walk-progn-body (forms env walker required-type)
  (when (null forms)
    (setq forms (list nil)))
  (loop for form-cons = forms then next-form-cons?
	for next-form-cons? = (cdr form-cons)
	for sub-form = (car form-cons)
	while form-cons
	collect (tl:walk sub-form env walker
			 (if next-form-cons?
			     'void
			     required-type))))




;;; The walker for `tl:block' adds information to the environment about the
;;; availability of the block with the given name and a given block-struct, and
;;; then it walks all its subforms.

(def-special-form-walker tl:block (form env walker type)
  (let* ((name (cons-second form))
	 (block-struct (make-block-scope type))
	 (new-env (tl:augment-environment
		    env
		    :declare `((block ,name ,block-struct))))
	 (walked-forms
	   (walk-progn-body (or (cons-cddr form) '(nil)) new-env walker type)))
    (unless (symbolp name)
      (error "Block names must be symbols, not ~s" name))
    (if (= (block-scope-return-from-count block-struct) 0)
	(cons 'tl:progn walked-forms)
	(values `(tl:block ,name ,@walked-forms) new-env))))




;;; The walker for `tl:catch' treats it like a function.  It established no new
;;; environment and walks all subforms, including the evaluated catch tag form.

(def-special-form-walker tl:catch (form env walker required-type)
  (let ((inner-env (tl:augment-environment env :declare '((catch)))))
    (unless (tl:declaration-information 'tl:allow-unwind-protect env)
      (translation-warning
	"Calling catch without an allow-unwind-protect declaration."))
    (values
      `(tl:catch
	   ,(tl:walk (cons-second form) env walker 't)
	 ,@(walk-progn-body
	     (or (cons-cddr form) '(nil))
	     inner-env walker required-type))
      inner-env)))

;; Consider adding catch tags to the exit scope.  This would enable us to
;; optimize throws to catch tags within a single function and it would further
;; enable us to detect within-function throws that violated require-local-exit
;; declarations.  -jra 5/4/95




;;; The walker for `tl:declare' should never be called.  If it is, then it means
;;; that a surrounding form that could include declarations did not check for
;;; and previously process these declarations, or it means that a user placed a
;;; declaration in an inappropriate place in their code.

(def-special-form-walker tl:declare (form env walker type)
  (declare (ignore walker env type))
  (warn "The declare form ~s was placed in an inappropriate place or the ~
         surrounding form improperly handled it.  Ignoring it."
	form)
  nil)




;;; The walker for `tl:eval-when' walks the body forms and returns them as for
;;; progn.  The translator performs any needed compile-time evaluations.  Note
;;; that the macro defining tl:eval-when is defined in ENV to resolve forward
;;; referencing problems.

(tl:declaim (special-form tl:eval-when))

(def-special-form-walker tl:eval-when (form env walker required-type)
  (let ((situations (cons-second form)))
    `(tl:eval-when ,situations
       ,@(walk-progn-body
	   (or (cons-cddr form) '(nil)) env walker required-type))))




;;; The global variable `local-functions-queue' is used to collect functions
;;; being defined locally within an enclosing function.

(defvar local-functions-queue nil)




;;; The function `generate-local-function-name' takes a prefix, a local name,
;;; and an environment and returns a new symbol to be used as a globally defined
;;; function name for the implementation of the given function.

(defun generate-local-function-name (prefix name env)
  (intern (format nil "~a~a-IN-~a-~a"
		  prefix name
		  (tl:declaration-information 'scope-name env)
		  (length (if (boundp 'local-functions-queue)
			      local-functions-queue
			      nil)))))




;;; The walker for `tl:flet' defines new global functions to implement the local
;;; functions, walks them, then augments the current environment and walks the
;;; body of the flet.  The FLET defined functions are walked within the current
;;; environment to pick up any lexical function definitions surrounding this
;;; one, however any lexical variable references won't work out.  We haven't
;;; implemented true lexical closures in TL, so these functions just become new
;;; global functions that are aliased to within the scope of the flet form.

(def-special-form-walker tl:flet (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cddr form))
    (let* ((func-forms (cons-second form))
	   (decls (loop for form in decl-forms append (cons-cdr form)))
	   (new-func-name-decls nil)
	   (func-names
	     (loop for func-form in func-forms
		   for func-name = (cons-car func-form)
		   for new-name = (generate-local-function-name
				    "FLET-" func-name env)
		   for named-form
		       = (multiple-value-bind (decls body)
			     (split-declarations-and-body (cons-cddr func-form))
			   `(tl:defun ,new-name ,(cons-second func-form)
			      ,@decls
			      (tl:block ,func-name
				,@body)))
		   do
	       (when (boundp 'local-functions-queue)
		 (setq local-functions-queue
		       (nconc local-functions-queue
			      (list (tl:walk named-form env walker t))))
		 (push `(local-function-global-name ,func-name ,new-name)
		       new-func-name-decls))
		   collect func-name))
	   (new-env
	     (tl:augment-environment
	       env :function func-names
	       :declare (append decls new-func-name-decls))))
      (values
	`(tl:flet ,(cond ((boundp 'local-functions-queue)
			  nil)
			 (t
			  (when func-forms
			    (warn "Passing through functions ~s in FLET."
				  (loop for func-form in func-forms
					collect (car func-form))))
			  func-forms))
	   ,@decl-forms
	   ,@(walk-progn-body (or body '(nil)) new-env walker required-type))
	new-env))))




;;; The special form `named-lambda' is used in the expansion of the tl:defun
;;; macro.  Since this macro is expanded for every function during the compile
;;; of a system, this is the place where we register which system and module
;;; this symbol is being defined within.

(tl:declaim (special-form named-lambda))

(defmacro named-lambda (name lambda-list &body decls-and-body)
  `(defun ,name ,lambda-list ,@decls-and-body))




;;; The constant `lambda-parameters-limit' is used in the implementation of
;;; apply to determine how many cases are needed for dispatching to
;;; funcall-internal.  Changed this to 20.  There is some overhead within
;;; tl:apply for each possible argument, regardless of whether it is actually
;;; passed.  CL says that this should be at least 50, which is fine for
;;; non-funcalled functions, but this variable is used to limit funcalled
;;; functions, and 20 works out just fine.  -jallard 7/17/97

(defconstant tl:lambda-parameters-limit 20)




;;; The value multiple-values-limit is reflected in the size of Values_buffer,
;;; defined in tlt/c/tlt.c.  If this value changes here, it must be updated into
;;; the definition of Values_buffer.

(defconstant tl:multiple-values-limit 20)




;;; The function `rewrite-lambda-list-for-named-lambda' takes a lambda list and
;;; returns three values values.  The first is a possibly rewritten replacement
;;; for the given list.  The second value is a list of the argument variables in
;;; the lambda-list.  The third value is a list of special-variable and argument
;;; variable pairs, where the special variables were part of the original lambda
;;; list and the argument variables are their replacements in the new lambda
;;; list.  The first form in the named lambda should rebind the argument values
;;; into the special variables.

(defun lambda-list-names-and-specials (lambda-list)
  (when (loop for elt in lambda-list
	      always (and (symbolp elt)
			  (not (memqp elt tl:lambda-list-keywords))
			  (not (eq (tl:variable-information elt) :special))))
    (return-from lambda-list-names-and-specials
      (values lambda-list lambda-list nil)))
  (loop with new-lambda-list = nil
	with variables = nil
	with specials-and-inits = nil
	for lambda-elt in lambda-list
	for var = (cond ((symbolp lambda-elt) lambda-elt)
			((symbolp (cons-car lambda-elt)) (cons-car lambda-elt))
			(t (cons-second (cons-car lambda-elt))))
	do
    (cond ((memqp lambda-elt tl:lambda-list-keywords)
	   (push lambda-elt new-lambda-list))
	  ((eq (tl:variable-information var) :special)
	   (let ((new-arg (intern (format nil "~a-INIT" var) *tli-package*))
		 (new-lambda-elt
		   (if (consp lambda-elt) (copy-list lambda-elt) lambda-elt)))
	     (push new-arg variables)
	     (push (list var new-arg) specials-and-inits)
	     (cond ((eq var new-lambda-elt)
		    (setq new-lambda-elt new-arg))
		   ((eq var (cons-car new-lambda-elt))
		    (setf (car new-lambda-elt) new-arg))
		   (t
		    (setf (second (car new-lambda-elt)) new-arg)))
	     (push new-lambda-elt new-lambda-list)))
	  (t
	   (push lambda-elt new-lambda-list)
	   (push var variables)))
	finally
	  (return (values (nreverse new-lambda-list)
			  (nreverse variables)
			  (nreverse specials-and-inits)))))

(def-special-form-walker named-lambda (form env walker required-type)
  (declare (ignore required-type))
  (tl:destructuring-bind-strict (nil name original-lambda-list . decls-and-body)
    form
    (multiple-value-bind (decl-forms body)
	(split-declarations-and-body decls-and-body)
      (multiple-value-bind (lambda-list variables specials-and-inits)
	  (lambda-list-names-and-specials original-lambda-list)
	(let* ((current-translation-context name)
	       (declares
		 (nconc
		   (list `(scope-name ,name)
			 ;; Mask the top-level permanent area.
			 '(tl:consing-area nil))
		   (loop for decl in decl-forms appending (cons-cdr decl))
		   (loop for var in variables
			 collect
			 (list 'variable-binding-structure
			       var (make-variable-binding nil)))))
	       (declared-env (tl:augment-environment
			       env :variable variables :declare declares))
	       (return-type?
		 (or (tl:declaration-information 'tl:return-type declared-env)
		     (third (function-decl name 'ftype declared-env))
		     '*)))
	  (values
	    `(named-lambda
	       ,name
	       ,(loop with optional? = nil
		      for lambda-elt in lambda-list
		      collect
		      (cond
			(optional?
			 (let* ((var (if (consp lambda-elt)
					 (cons-first lambda-elt)
					 lambda-elt))
				(type
				  (or (variable-decl var 'type declared-env)
				      t))
				(init (if (consp lambda-elt)
					  (cons-second lambda-elt)
					  nil))
				(walked-init (tl:walk init env walker type)))
			   (additional-variable-setter-type
			     var declared-env
			     (expression-result-type walked-init env))
			   `(,var ,walked-init)))
			(t
			 (when (eq lambda-elt '&optional)
			   (setq optional? t))
			 lambda-elt)))
	       ,@decl-forms
	       ,(tl:walk
		  (if specials-and-inits
		      `(tl:let* ,specials-and-inits
			 (tl:block ,name ,@body))
		      `(tl:block ,name ,@body))
		  declared-env walker return-type?))
	    declared-env))))))




;;; The walker for `tl:function' transforms embedded lambdas into functions
;;; bound on the local-functions-queue, and then returns a function form
;;; referencing that new function.  If the embedded reference is a symbol naming
;;; a local function within the current environment, the function form is
;;; transformed into a reference to the global function name of that local
;;; function.  Note that a macro for this is not needed, since tl:function is EQ
;;; to lisp:function.  This is required unless we attempt to change the
;;; readtable to have #' turn into tl:function, which is unneccessary.

(tl:declaim (special-form tl:function))

(def-special-form-walker tl:function (form env walker required-type)
  (declare (ignore required-type))
  (let ((func (cons-second form)))
    (cond
      ((and (consp func) (eq (car func) 'lambda))
       (if (boundp 'local-functions-queue)
	   (let ((new-name (generate-local-function-name
			     "LAMBDA-" 'lambda env)))
	     (setq local-functions-queue
		   (nconc
		     local-functions-queue
		     (list
		       (tl:walk `(tl:defun ,new-name ,@(cons-cdr func))
				env walker t))))
	     `(tl:function ,new-name))
	   form))
      ((symbolp func)
       (multiple-value-bind (type? local? decls?)
	   (tl:function-information func env)
	 (if (and local?
		  (eq type? :function)
		  (assq 'local-function-global-name decls?))
	     `(tl:function ,(cdr (assq 'local-function-global-name decls?)))
	     form)))
      (t
       ;; Can we get this case?
       (cerror "continue"
	       "Special form walker for FUNCTION got a non-lambda, non-symbol, ~s"
	       func)
       form))))



;;; The walker for `tl:go' attempts to find an enter in the env for the given
;;; tag and if found enters itself as a user of that tag (the user of tag stuff
;;; isn't ready yet  -jra 4/12/95).

(def-special-form-walker tl:go (form env walker required-type)
  (declare (ignore walker required-type))
  (let* ((tag-name (cons-second form))
	 (exit-scope (tl:declaration-information 'exit-scope env))
	 (found-tag? (loop for entry in exit-scope
			   thereis (and (eq (cons-car entry) 'tagbody-tags)
					(assq tag-name (cons-cdr entry))))))
    ;; Issue a warning when the given argument is not the global environment,
    ;; but let things proceed.  It seems that there is a code-walker in the INT
    ;; files that causes the macroexpansion of GO forms without having
    ;; appropriately propagated the environment.  Let it go by, but we will have
    ;; to revisit this code walker later.  -jallard, 5/8/97

    (cond (found-tag?
	   (incf (tagbody-scope-reference-count (cons-second found-tag?))))
	  ((not (global-environment-p env))
	   (warn "GO was unable to find the tag ~s in its environment."
		 (cons-second form))))
    ;; Place more code here to modify the found tag structure.
    form))




;;; The walker for `tl:if' walks its subforms and returns them.  If there is no
;;; else clause, a NIL clause is given.

(def-special-form-walker tl:if (form env walker required-type)
  (when (cddddr form)
    (error "Too many forms in IF, only one else clause allowed: ~s"
	   form))
  `(tl:if ,(tl:walk (cons-second form) env walker t)
	  ,(tl:walk (cons-third form) env walker required-type)
	  ,(tl:walk (fourth form) env walker required-type)))




;;; The walker for `tl:labels' defines a set of local potentially mutually
;;; recursive functions, walking them in an environment that defines each of
;;; them, and then it walks its subforms.

(def-special-form-walker tl:labels (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cddr form))
    (let* ((func-forms (cons-second form))
	   (decls (loop for form in decl-forms append (cons-cdr form)))
	   (new-func-name-decls
	     (loop for (func-name) in func-forms
		   for new-name = (generate-local-function-name
				    "LABELS-" func-name env)
		   collect `(local-function-global-name ,func-name ,new-name)))
	   (labeled-functions-env
	     (tl:augment-environment
	       env
	       :function (loop for (func-name) in func-forms collect func-name)
	       :declare (append
			  (loop for form in decl-forms 
				append
				(loop for decl in (cons-cdr form)
				      when (and (consp decl)
						(memq (car decl)
						      '(tl:inline tl:notinline tl:ftype)))
					collect decl))
			  new-func-name-decls)))
	   (new-env
	     (tl:augment-environment
	       env
	       :function (loop for (func-name) in func-forms collect func-name)
	       :declare (append decls new-func-name-decls))))
      (loop for func-form in func-forms
	    for (nil nil new-name) in new-func-name-decls
	    do
	(when (boundp 'local-functions-queue)
	  (setq local-functions-queue
		(nconc local-functions-queue
		       (list
			 (tl:walk
			   (multiple-value-bind (decls body)
			       (split-declarations-and-body (cons-cddr func-form))
			     `(tl:defun ,new-name ,(cons-second func-form)
				,@decls
				(block ,(cons-first func-form)
				  ,@body)))
			   labeled-functions-env walker t))))))
      (values
	`(tl:labels ,(if (boundp 'local-functions-queue)
			 nil
			 func-forms)
	   ,@decl-forms
	   ,@(walk-progn-body body new-env walker required-type))
	new-env))))




;;; The function `transform-init-for-dynamic-extent' is called on initialization
;;; forms for variables which have been declared dynamic-extent.  This function
;;; should either transform the given initialization form into a conser for a
;;; dynamic-extent data structure (such as list-dynamic-extent), verify that the
;;; form is already a conser for a dynamic-extent data structure (for the
;;; recursive walk case), or else issue a warning and return the original form.
;;; The given form must directly call a transformable conser, or else
;;; macroexpand into a direct call to a transformable conser.  This is to say
;;; that this function will not attempt to look inside progn or let forms to
;;; find and transform the value returning clause of that form.

;;; The following forms can be transformed into dynamic-extent consing.

;;;   tl:list  => list-dynamic-extent
;;;   tl:cons  => cons-dynamic-extent

;;; This function is given the variable symbol being initialized, the init-form
;;; given by the user, the environment in which the init-form will be evaluated,
;;; and the environment in which the variable has been defined with its
;;; initializations.  This function may call itself recursively as it attempts
;;; to slowly macroexapnd the init-form, searching for a transformable
;;; initializer.

(defun transform-init-for-dynamic-extent (var given-init env new-env)
  (let ((transformed-form? nil))
    ;; Attempt a transform.
    (when (consp given-init)
      (case (cons-car given-init)
	((tl:list)
	 (setq transformed-form?
	       (cons 'list-dynamic-extent (cons-cdr given-init))))
	((tl:cons)
	 (setq transformed-form?
	       (cons 'cons-dynamic-extent (cons-cdr given-init))))))
    ;; If a transform was found, return it.  Else, perform one macroexpansion
    ;; and try again.  If no expansion can occur, then warn and return the form
    ;; as expanded so far.
    (if transformed-form?
	transformed-form?
	(multiple-value-bind (expanded-form any-expansion?)
	    (tl:macroexpand-1 given-init env)
	  (cond (any-expansion?
		 (transform-init-for-dynamic-extent
		   var expanded-form env new-env))
		((and (consp expanded-form)
		      (memqp (cons-car expanded-form)
			     '(list-dynamic-extent cons-dynamic-extent)))
		 expanded-form)
		(t
		 (translation-warning
		   "~s was declared dynamic-extent, but the initial value form~@
                    couldn't be transformed into a dynamic-extent initial value.~@
                    Expanded initial value form = ~s"
		   var expanded-form)
		 expanded-form))))))

;; Note that new-env is not really used here, but I included them in the
;; argument list in case in the future we wanted to perform type-declaration
;; optimizations during this transform.  -jra 1/17/97
	
  




;;; The special form walker for `tl:let' walks each variable initialization form
;;; in the surrounding environment of the entire let form, then walks the body
;;; in the scope of the new variable bindings and the declarations in the body.

(def-special-form-walker tl:let (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cddr form))
    (let* ((bindings (cons-second form))
	   (variables
	     (loop for bind in bindings
		   collect (if (consp bind) (car bind) bind)))
	   (env-with-decls
	     (tl:augment-environment
	       env :variable variables
	       :declare
	       (append
		 (loop for declare in decl-forms append (cons-cdr declare))
		 (loop for bind in bindings
		       for var = (if (consp bind) (car bind) bind)
		       for var-init? = (and (consp bind) (symbolp (second bind))
					    (second bind))
		       for var-init-struct?
			   = (if var-init?
				 (multiple-value-bind (b l d)
				     (tl:variable-information var-init? env)
				   (declare (ignore l))
				   (if (eq b :lexical)
				       (cdr (assq 'variable-binding-structure d)))))
		       for struct = (make-variable-binding nil)
		       for init-info?
			   = (if var-init-struct?
				 (list var-init? var-init-struct?
				       (list 0) (list 0)))
		       do
		   (when init-info?
		     (setf (variable-binding-init-info? struct) init-info?))
		       nconc
		       (if init-info?
			   (list
			     `(variable-binding-structure ,var ,struct)
			     `(setq-counter ,var ,(third init-info?))
			     `(setq-counter ,var-init? ,(fourth init-info?)))
			   (list
			     `(variable-binding-structure ,var ,struct)))))))
	   ;; The check for special variables must happen within the scope of
	   ;; the given declarations so that special declarations can affect
	   ;; this loop.
	   (special-variables
	     (loop for var in variables
		   when (eq (tl:variable-information var env-with-decls)
			    :special)
		     collect var))
	   (new-env
	     (if special-variables
		 (tl:augment-environment
		   env-with-decls
		   :declare `((global-variable-binding ,@special-variables)))
		 env-with-decls)))
      (values
	(cond
	  ((and (null bindings)
		(null decl-forms))
	   `(tl:progn
	      ,@(walk-progn-body body new-env walker required-type)))
	  ((null bindings)
	   `(tl:locally ,@decl-forms
	      ,@(walk-progn-body body new-env walker required-type)))
	  (t
	   `(tl:let ,(loop for bind in bindings
			   for var = (if (consp bind) (car bind) bind)
			   for var-type = (or (variable-decl var 'type new-env)
					      t)
			   for given-init
			       = (if (and (consp bind)
					  (or (not (null (second bind)))
					      (eq var-type t)
					      (tl-typep nil var-type)))
				     (second bind)
				     (default-init-for-lisp-type var-type))
			   for transformed-init
			       = (if (variable-decl var 'dynamic-extent new-env)
				     (transform-init-for-dynamic-extent
				       var given-init env new-env)
				     given-init)
			   for walked-init
			       = (tl:walk transformed-init env walker var-type)
			   do
		       (additional-variable-setter-type
			 var new-env
			 (expression-result-type walked-init env))
			   collect (list var walked-init))
	      ,@decl-forms
	      ,@(walk-progn-body body new-env walker required-type))))
	new-env))))




;;; The function `decls-for-variable' takes a list of declarations and a
;;; variable name, and returns the subset of the given declarations that apply
;;; to that variable.  In cases where the declaration mentioning this variable
;;; also mentions other variables, then the returned declaration will be
;;; rewritten to only mention the desired variable.

(defun decls-for-variable (var decls env)
  (let ((variable-declarations
	  (tl:declaration-information 'tl:variable-declaration env))
	(decls-to-return nil))
    (loop for decl in decls
	  for decl-type = (cons-car decl)
	  do
      (cond ((eq decl-type 'type)
	     (when (memq var (cons-cddr decl))
	       (push `(type ,(cons-second decl) ,var) decls-to-return)))
	    ((memq decl-type variable-declarations)
	     (when (memq var (cons-cdr decl))
	       (push `(,decl-type ,var) decls-to-return)))))
    (nreverse decls-to-return)))




;;; The special form walker for `tl:let*' walks each variable init form in turn,
;;; incrementally building up the environment with the declarations that apply
;;; to the variables being bound.

(def-special-form-walker tl:let* (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cddr form))
    (loop with decls = (loop for form in decl-forms append (cons-cdr form))
	  with no-init-given = (cons 'unsupplied #\!)
	  for final-env = env then next-env
	  for binding in (cons-second form)
	  for var = (if (consp binding) (car binding) binding)
	  for given-init = (if (consp binding)
			       (second binding)
			       no-init-given)
	  for init-env = env then next-env
	  for var-init? = (if (symbolp given-init) given-init)
	  for var-init-struct?
	      = (if var-init?
		    (multiple-value-bind (b l d)
			(tl:variable-information var-init? init-env)
		      (declare (ignore l))
		      (if (eq b :lexical)
			  (cdr (assq 'variable-binding-structure d)))))
	  for init-info?
	      = (if var-init-struct?
		    (list var-init? var-init-struct? (list 0) (list 0)))
	  for struct = (make-variable-binding nil)
	  for init-env-with-decls
	      = (tl:augment-environment
		  init-env :variable (list var)
		  :declare
		  `(,@(decls-for-variable var decls init-env)
		      (variable-binding-structure ,var ,struct)
		      ,@(if init-info?
			    `((setq-counter ,var ,(third init-info?))
			      (setq-counter ,var-init? ,(fourth init-info?))))))
	  for next-env
	      = (if (eq (tl:variable-information var init-env-with-decls)
			:special)
		    (tl:augment-environment
		      init-env-with-decls
		      :declare `((global-variable-binding ,var)))
		    init-env-with-decls)
	  for var-type = (or (variable-decl var 'type next-env) t)
	  for init = (if (eq given-init no-init-given)
			 (default-init-for-lisp-type var-type)
			 given-init)
	  for transformed-init
	      = (if (variable-decl var 'dynamic-extent next-env)
		    (transform-init-for-dynamic-extent
		      var init init-env next-env)
		    init)
	  for walked-init
	      = (tl:walk transformed-init init-env walker var-type)
	  do
      (when init-info?
	(setf (variable-binding-init-info? struct) init-info?))
      (additional-variable-setter-type
	var env
	(expression-result-type walked-init init-env))
	  collect (list var walked-init) into walked-bindings
	  finally
	    (setq final-env (tl:augment-environment final-env :declare decls))
	    (return
	      (values
		(cond
		  ((and (null walked-bindings)
			(null decl-forms))
		   `(tl:progn
		      ,@(walk-progn-body body final-env walker required-type)))
		  ((null walked-bindings)
		   `(tl:locally
			,@decl-forms
		      ,@(walk-progn-body body final-env walker required-type)))
		  (t
		   `(tl:let* ,walked-bindings
		      ,@decl-forms
		      ,@(walk-progn-body body final-env walker required-type))))
		final-env)))))



;;; The special form walker for macrolet augments the environment to contain the
;;; new macro definitions, then walks its body.

(def-special-form-walker tl:macrolet (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cddr form))
    (loop with macro-forms = (cons-second form)
	  with decls = (loop for form in decl-forms append (cons-cdr form))
	  for macro-form in macro-forms
	  for macro-name = (cons-car macro-form)
	  for parsed-defn = (tl:parse-macro macro-name (cons-second macro-form)
					    (cons-cddr macro-form) env)
	  for macro-defn = (eval `(function ,parsed-defn))
	  collect (list macro-name macro-defn) into new-macros
	  finally
	    (let ((new-env (tl:augment-environment
			     env :macro new-macros :declare decls)))
	      (return
		(values
		  `(tl:macrolet ,macro-forms
		     ,@decl-forms
		     ,@(walk-progn-body body new-env walker required-type))
		  new-env))))))

;; Note that the newly defined macros are defined within the global environment,
;; not the local lexical one, as is called for in CLtL 2, near the bottom of
;; p. 153.  This loses the use of local macros and symbol-macros within the body
;; of the new local macros, though local macros and symbols-macros are applied
;; to the expansion results of the macros.  If we feel compelled to fix this,
;; then tl:parse-macro should be expanded to perform a walk of the body of the
;; macro within the scope of the given environment.  -jra 5/4/95




;;; The walker for `tl:multiple-value-bind' walks the value initialization form
;;; in the surrounding environment and then walks the body in the scope of the
;;; new variable bindings and declarations in the body.  Note that
;;; multiple-value-bind is not specifically a special form, though a comment at
;;; the bottom of CLtL2, p. 72 indicates that it is generally a good idea for
;;; compilers to treat it that way.  I agree.  -jra 11/9/95

(def-special-form-walker tl:multiple-value-bind (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cdddr form))
    (let* ((variables (cons-second form))
	   (decls (loop for form in decl-forms
			append (cons-cdr form)))
	   (env-with-decls
	     (tl:augment-environment
	       env :variable variables
	       :declare
	       (append decls (loop for var in variables
				   collect
				   (list 'variable-binding-structure var
					 (make-variable-binding nil))))))
	   ;; The check for special variables must happen within the scope of
	   ;; the given declarations so that special declarations can affect
	   ;; this loop.
	   (special-variables
	     (loop for var in variables
		   when (eq (tl:variable-information var env-with-decls)
			    :special)
		     collect var
		   do
	       (when (variable-decl var 'dynamic-extent env-with-decls)
		 (translation-warning
		   "~s was declared dynamic-extent, but dynamic-extent is not ~@
                    supported by multiple-value-bind."
		   var))))
	   (new-env
	     (if special-variables
		 (tl:augment-environment
		   env-with-decls
		   :declare `((global-variable-binding ,@special-variables)))
		 env-with-decls))
	   (init-required-type
	     (cond
	       ((cddr variables)
		(cons 'values
		      (loop for var in variables
			    collect (or (variable-decl var 'type new-env) t))))
	       ((cdr variables)
		(or (variable-decl (car variables) 'type new-env) t))
	       (t 'void)))
	   (walked-init-form
	     (tl:walk (cons-third form) env walker init-required-type))
	   (walked-init-result-type (expression-result-type walked-init-form env)))
      (cond ((and (consp walked-init-result-type)
		  (eq (cons-car walked-init-result-type) 'values))
	     (loop for type in (cons-cdr walked-init-result-type)
		   for var in variables
		   do
	       (additional-variable-setter-type var new-env type)))
	    ((car variables)		; if at least one variable
	     (additional-variable-setter-type
	       (car variables) new-env walked-init-result-type)))
      (values
	`(tl:multiple-value-bind ,variables ,walked-init-form
	   ,@decl-forms
	   ,@(walk-progn-body body new-env walker required-type))
	new-env))))




;;; The walker for `tl:multiple-value-prog1' walks the first form given the required
;;; type of the walker as a whole.  The remaining forms are then walked with a
;;; void required type.

(def-special-form-walker tl:multiple-value-prog1 (form env walker required-type)
  `(tl:multiple-value-prog1
       ,(tl:walk (cons-second form) env walker required-type)
     ,@(walk-progn-body (cons-cddr form) env walker 'void)))




;;; The walker for `tl:progn' is a simple as they get (though I got it wrong the
;;; first time by expanding into a let, not a tl:let  -jra 11/9/95).

(def-special-form-walker tl:progn (form env walker required-type)
  `(tl:progn ,@(walk-progn-body (cons-cdr form) env walker required-type)))




;;; The walker for `tl:quote' returns the given form with no walking done.

(def-special-form-walker tl:quote (form env walker required-type)
  (declare (ignore env walker))
  (tl:destructuring-bind-strict (nil value) form
    (unless (or (explicit-lisp-to-c-type-p required-type)
		(tl-typep value required-type))
      (warn "~s does not return a value of type ~s." form required-type))
    form))




;;; The walker for `tl:return-from' finds the surrounding block structure that
;;; matches the named block, fetches its required type, and walks the value
;;; returning form giving that required type.  Note that the value form is
;;; optional, defaulting to NIL.

(def-special-form-walker tl:return-from (form env walker required-type)
  (declare (ignore required-type))
  (let* ((tag (cons-second form))
	 (exit-scope (tl:declaration-information 'exit-scope env))
	 (block-struct? (loop for decl in exit-scope
			      when (and (eq (cons-car decl) 'block)
					(eq (cons-second decl) tag))
				return (cons-third decl))))
      (cond
	(block-struct?
	 (incf (block-scope-return-from-count block-struct?))
	 `(tl:return-from ,tag
	    ,(tl:walk
	       (third form) env walker
	       (block-scope-lisp-required-type block-struct?))))
	(t
	 ;; Some existing code walkers in G2 are bad about passing environments.
	 ;; If the global environment has been given, then don't even bother to
	 ;; complain and just return the walked form.
	 (unless (global-environment-p env)
	   (warn "~s unable to find block for ~s." form tag))
	 `(tl:return-from ,tag
	    ,(tl:walk (third form) env walker '*))))))




;;; The walker for `tl:setq' first checks if there are multiple setting pairs.
;;; If so, it then recurses into a walk of a progn with one call to setq for
;;; each pair.  If there is only one pair, it then checks if the variable to be
;;; set is defined as a symbol-macro.  If so, then this call it transformed into
;;; a call to setf.  If the variable is not a symbol-macro, is either a local
;;; lexical, or global variable (this is the standard case), then new value form
;;; is walked and the binding structure for the variable is updated with the
;;; result type of the new value form.

(def-special-form-walker tl:setq (form env walker required-type)
  (let ((forms-count (length (cons-cdr form))))
    (cond ((oddp forms-count)
	   (error "~s contains an odd number of arguments." form))
	  ((/= forms-count 2)
	   `(tl:progn
	      ,@(loop for (var value) on (cons-cdr form) by #'cddr
		      collect (tl:walk `(tl:setq ,var ,value)
				       env walker required-type))))
	  (t
	   (let ((var (cons-second form))
		 (value (cons-third form)))
	     (multiple-value-bind (type? local? decls?)
		 (tl:variable-information var env)
	       (declare (ignore local?))
	       (ecase type?
		 ((:lexical :special)
		  (let ((walked-new-value
			  (tl:walk value env walker
				   (duplicate-type-declaration
				     (or (cdr (assq 'type decls?)) t)
				     required-type))))
		    (loop for counter-cons in (cdr (assq 'setq-counter decls?))
			  do
		      (incf (car counter-cons)))
		    (additional-variable-setter-type
		      var env
		      (expression-result-type walked-new-value env))
		    `(tl:setq ,var ,walked-new-value)))
		 ((:symbol-macro)
		  `(tl:progn ,(tl:walk `(tl:setf ,var ,value)
				       env walker required-type)))
		 ((nil)
		  ;; Give a warning for variables that seem clearly undefined.
		  ;; Eliminate those that are bound, since they are often
		  ;; underlying Lisp environment variables being set in
		  ;; development only code.
		  (unless (boundp var)
		    (warn "~s setting an undefined variable." form))
		  `(tl:setq ,var ,(tl:walk value env walker required-type)))
		 ((:constant)
		  (warn "~s setting constant variable." form)
		  `(tl:setq ,var
			    ,(tl:walk value env walker required-type))))))))))




;;; The walker for `tl:tagbody' registers as tags all symbols and fixnums found
;;; at its top level, then walks the remaining forms inside the augmented
;;; environment.  It also loops for tag and go signatures that indicate looping
;;; statements.  If found, these are rewritten to use the for-loop or while-loop
;;; special forms.

(tl:declaim (special-form tl:tagbody))

(defmacro tl:tagbody (&rest tags-and-forms)
  `(tagbody
      ,@(loop for tag-or-form in tags-and-forms
	      when tag-or-form collect tag-or-form)))

(def-special-form-walker tl:tagbody (form env walker required-type)
  (declare (ignore required-type))
  ;; Eliminate NILs at top level, Lucid complains about them.  Also eliminate
  ;; any go statements to tags that immediately follow the go.
  (setq form
	(cons (cons-car form)
	      (loop for elt-cons on (cons-cdr form)
		    for tagbody-elt = (cons-car elt-cons)
		    when (and tagbody-elt
			      (not (and (consp tagbody-elt)
					(eq (car tagbody-elt) 'tl:go)
					(consp (cdr tagbody-elt))
					(eq (second tagbody-elt)
					    (second elt-cons)))))
		      collect tagbody-elt)))
  ;; Pattern recognize the signature of LOOP, DO, and friends, and rewrite their
  ;; bodies into appropriate while-loop and for-loop forms.
  (when (loop for subform-cons = (cons-cdr form) then (cons-cdr subform-cons)
	      while subform-cons
	      do
	  (when (eq (cons-car subform-cons) 'tl::next-loop)
	    (setq subform-cons (cons-cdr subform-cons))
	    (return (loop for subform = (car subform-cons)
			  while subform-cons
			  never (atom subform)
			  thereis (equal subform '(tl:go tl::next-loop))
			  do
		      (setf subform-cons (cons-cdr subform-cons))))))
    (setq form (rewrite-tagbody-for-loops form env)))
  (let* ((tag-decls (loop for item in (cons-cdr form)
			  when (atom item)
			    collect (list item (make-tagbody-scope item))))
	 (new-env (tl:augment-environment
		    env :declare (list (cons 'tagbody-tags tag-decls)))))
    (values
      `(tl:tagbody
	  ,@(loop for item in (cons-cdr form)
		  collect
		  (if (atom item)
		      item
		      (tl:walk item new-env walker 'void))))
      new-env)))

(defun rewritable-when-end-test-p (form env)
  (and (consp form)
       (eq (cons-car form) 'tl:when)
       (= (length form) 3)
       (tl:destructuring-bind-strict (nil test body)
	 form
	 (and (equal body '(tl:go tl::end-loop))
	      (translates-as-c-expr-p test env)))))

(defun rewritable-unless-end-test-p (form env)
  (and (consp form)
       (eq (cons-car form) 'tl:unless)
       (= (length form) 3)
       (tl:destructuring-bind-strict (nil test body)
	 form
	 (and (equal body '(tl:go tl::end-loop))
	      (translates-as-c-expr-p test env)))))

(defun translates-as-c-expr-p (form env)
  (or (symbolp form)
      (constantp form)
      (and (consp form)
	   (let ((operator (cons-car form)))
	     (and (or (memqp operator
			     '(tl:null tl:atom tl:consp tl:eq tl:eql tl:equal
			       eq-trans eql-trans equal-trans
			       tl:and tl:or tl:not not-unbound-value-p))
		      ;; For the following operators, type restrictions on
		      ;; arguments could cause type check statements when we are
		      ;; translating safe.  Only allow these when unsafe.  -jra
		      ;; 2/23/96
		      (and (/= (tl:optimize-information 'safety env) 3)
			   (memqp operator
				  '(tl:< tl:<= tl:= tl:>= tl:> tl:+ tl:- tl:1+
				    tl:1- tl:car-of-cons tl:cdr-of-cons
				    tl:plusp))))
		  (loop for subform in (cons-cdr form)
			always (translates-as-c-expr-p subform env)))))))

(defun end-loop-used-p (form-or-forms)
  (do* ((forms-cons form-or-forms (cons-cdr forms-cons)))
       ((atom forms-cons)
	nil)
    (let ((form (cons-car forms-cons)))
      (when (consp form)
	(let ((form-car (cons-car form)))
	  (cond ((eq form-car 'tl:loop-finish)
		 (return t))
		((eq form-car 'tl:go)
		 (return (and (consp (cons-cdr form))
			      (eq (cons-second form) 'tl::end-loop))))
		(t
		 (when (end-loop-used-p form)
		   (return t)))))))))

(defun rewrite-tagbody-for-loops (form env)
  (let* ((loop-body nil)
	 (new-loop-form (list 'while-loop))
	 ;; Use 1 instead of T as the default, it has a simpler translation.
	 (end-test 1)
	 (iteration-form nil)
	 (new-tagbody-body
	   (loop with in-body? = nil
		 with past-body? = nil
		 for subform in (cons-cdr form)
		 when (not in-body?)
		   collect (if (and (not past-body?)
				    (eq subform 'tl::next-loop))
			       new-loop-form
			       subform)
		 do
	     (cond (in-body?
		    (if (equal subform '(tl:go tl::next-loop))
			(setq in-body? nil
			      past-body? t)
			(push subform loop-body)))
		   ((and (not past-body?)
			 (eq subform 'tl::next-loop))
		    (setq in-body? t))))))
    (setq loop-body (nreverse loop-body))
    (cond ((rewritable-when-end-test-p (car loop-body) env)
	   (let* ((end-test-form (cons-car loop-body))
		  (test (cons-second end-test-form)))
	     (setq end-test
		   (if (and (consp test)
			    (memqp (cons-car test) '(tl:null tl:not)))
		       (cons-second test)
		       `(tl:not ,test)))
	     (setq loop-body (cons-cdr loop-body))))
	  ((rewritable-unless-end-test-p (car loop-body) env)
	   (setq end-test (cons-second (cons-car loop-body)))
	   (setq loop-body (cons-cdr loop-body))))
    ;; Attempt to find a final TL:SETQ in the body, and rotate that into the
    ;; iteration step form to make a for-loop instead of a while-loop
    (let ((last-form (car (last loop-body))))
      (when (and (consp last-form)
		 (eq (cons-car last-form) 'tl:setq)
		 (cons-cdr last-form)
		 (translates-as-c-expr-p (car (last last-form)) env))
	(setq iteration-form
	      `(tl:setq ,@(nthcdr (- (length last-form) 2) last-form)))
	(setq loop-body
	      (if (= (length last-form) 3)
		  (butlast loop-body)
		  (nconc (butlast loop-body)
			 `((tl:setq ,@(butlast (cons-cdr last-form) 2))))))))
    (cond (iteration-form
	   (setf (car new-loop-form) 'for-loop)
	   (setf (cdr new-loop-form)
		 (cons (list nil end-test iteration-form)
		       (list (cons 'tl:tagbody loop-body)))))
	  (t
	   (setf (cdr new-loop-form)
		 (cons end-test
		       (list (cons 'tl:tagbody loop-body))))))
    (cons 'tl:tagbody 
	  (if (end-loop-used-p new-tagbody-body)
	      new-tagbody-body
	    (delq 'tl::end-loop new-tagbody-body)))))




;;; The special form walker for `tl:the' walks its subform given the
;;; intersection of the required-type given this form as a whole and the type
;;; given as the first argument to this form.

(tl:declaim (special-form tl:the))

(defmacro tl:the (type form)
  `(the ,(expand-type type) ,form))

(def-special-form-walker tl:the (form env walker required-type)
  (tl:destructuring-bind-strict (nil type subform) form
    `(tl:the ,(expand-type type)
	     ,(tl:walk subform env walker
		       (duplicate-type-declaration type required-type)))))




;;; The special form walker for `tl:throw' walks all subforms, ignoring the
;;; required type, since this expression will never return to its direct caller.

(def-special-form-walker tl:throw (form env walker required-type)
  (declare (ignore required-type))
  (tl:destructuring-bind-strict (nil tag result) form
    `(tl:throw
	 ,(tl:walk tag env walker 't)
       ,(tl:walk result env walker '*))))




;;; The special form walker for `tl:unwind-protect' declares an unwind-protect
;;; scope around the protected form, walks it giving the passed in required
;;; type, then walks the cleanup forms with a required-type of void and in the
;;; environment scope that was passed in to this function.

(def-special-form-walker tl:unwind-protect (form env walker required-type)
  (let ((new-env (tl:augment-environment
		   env :declare (list (list 'unwind-protect (make-protect))))))
    (unless (tl:declaration-information 'tl:allow-unwind-protect env)
      (translation-warning
	"Calling unwind-protect without an allow-unwind-protect declaration."))
    (values
      `(tl:unwind-protect
	    ,(tl:walk (cons-second form) new-env walker required-type)
	 ,(tl:walk
	    `(tl:locally
		 (tl:declare (tl:require-local-exit
			       "Returning through an unwind protect cleanup scope."))
	       ,@(cons-cddr form))
	    env walker 'void))
      new-env)))




;;; The special form walker for `tl:symbol-macrolet' declares the given symbol
;;; macros into the environment, augments the environment with any declarations
;;; given on the body, and then processed the forms of the body.

(tl:declaim (special-form tl:symbol-macrolet))

(defmacro tl:symbol-macrolet (symbol-decls &body decls-and-body-forms)
  `(#+lucid lcl::symbol-macrolet #-lucid symbol-macrolet
	    ,symbol-decls
	    ,@decls-and-body-forms))
	    

(def-special-form-walker tl:symbol-macrolet (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cddr form))
    (let* ((decls (loop for form in decl-forms append (cons-cdr form)))
	   (new-env (tl:augment-environment
		      env :symbol-macro (cons-second form)
		      :declare decls)))
      (values
	`(tl:symbol-macrolet
	   ,(cons-second form)
	   ,@decl-forms
	   ,@(walk-progn-body body new-env walker required-type))
	new-env))))




;;; The special form walker for `tl:locally' asserts any declarations in the
;;; body into the environment, then walks all further subforms giving the passed
;;; in required-type.

(def-special-form-walker tl:locally (form env walker required-type)
  (multiple-value-bind (decl-forms body)
      (split-declarations-and-body (cons-cdr form))
    (let* ((decls (loop for form in decl-forms append (cons-cdr form)))
	   (new-env (tl:augment-environment env :declare decls)))
      (values
	`(tl:locally
	     ,@decl-forms
	   ,@(walk-progn-body body new-env walker required-type))
	new-env))))




;;; The special form walker for `fixnum-case' walks the switching value with a
;;; required type of fixnum, and then each of the case tags and forms requiring
;;; the passed in required type.

(tl:declaim (special-form fixnum-case))

(defmacro fixnum-case (fixnum-keyform &body clauses)
  `(case ,fixnum-keyform ,@clauses))

(def-special-form-walker fixnum-case (form env walker required-type)
  `(fixnum-case ,(tl:walk (cons-second form) env walker 'fixnum)
     ,@(loop for (keys . forms) in (cons-cddr form)
	     collect
	     `(,(cond ((fixnump keys)
		       (list keys))
		      ((memqp keys '(t tl:otherwise))
		       t)
		      ((and (consp keys)
			    (loop for x in keys always (fixnump x)))
		       keys)
		      (t
		       (error "FIXNUM-CASE given invalid keys: ~s" keys)))
		 ,@(walk-progn-body
		     (or forms '(nil)) env walker required-type)))
     ,@(let ((last-clause? (car (last (cons-cddr form)))))
	 (when (or (null last-clause?)
		   (and (not (eq (car last-clause?) 't))
			(not (eq (car last-clause?) 'tl:otherwise))))
	   `((t ,(tl:walk nil env walker required-type)))))))




;;; The special form walker for `while-loop' takes a continue-looping predicate
;;; and a body of loop forms.  It walks both of these in the given environment.
;;; Note that this macro always returns NIL

(tl:declaim (special-form while-loop))

(defmacro while-loop (continue-looping-form &body body)
  ;; Using tagbody to avoid the extra (block nil ...) that loop gives you.
  `(tagbody
      next-loop
      (unless ,continue-looping-form (go end-loop))
      ,@body
      (go next-loop)
      end-loop))

(def-special-form-walker while-loop (form env walker required-type)
  (declare (ignore required-type))
  `(while-loop
     ,(tl:walk (cons-second form) env walker 't)
     ,@(walk-progn-body (cons-cddr form) env walker 'void)))




;;; The special form walker for `for-loop' takes a list of an init expression, a
;;; control expression, and a stepping expression, and then it takes a body
;;; forms that are the interior of the loop.  The init expression is evaluated
;;; once on entry to this macro and its value is discarded.  Next, the control
;;; expression is evaluated, and if non-null, we execute the body forms.  If the
;;; control expression returns NIL, then we terminate the loop, returning NIL.
;;; After the body forms have been executed, the stepping expression is
;;; evaluated and its value is discarded, and then we go back up to the point
;;; where we evaluate the control expression.

(tl:declaim (special-form for-loop))

(defmacro for-loop ((init control step) &body forms)
  ;; Using tagbody to avoid the extra (block nil ...) that loop gives you.
  `(tagbody
      ,@(if init `(,init) nil)
      next-loop
      (unless ,control (go end-loop))
      ,@forms
      ,@(if step `(,step) nil)
      (go next-loop)
      end-loop))

(def-special-form-walker for-loop (form env walker required-type)
  (declare (ignore required-type))
  (tl:destructuring-bind-strict
      (nil (init control step) &body body)
    form
    `(for-loop (,(if init (tl:walk init env walker 'void) nil)
		 ,(tl:walk control env walker 't)
		 ,(if step (tl:walk step env walker 'void) nil))
	       ,@(walk-progn-body body env walker 'void))))




;;; The `tl:values' operation is defined as a function in Common Lisp, but it
;;; needs to be a special form.  Note that in the case of a values call with no
;;; arguments, this will constant fold into a NIL.  We do not implement a
;;; multiple-value-list, so these are semantically equivalent.  -jra 2/1/96

(def-special-form-walker tl:values (form env walker required-type)
  (if (null (cons-cdr form))
      nil
      `(tl:values
	 ,@(cond
	     ((and (consp required-type) (eq (cons-car required-type) 'values))
	      (loop for arg in (cons-cdr form)
		    for type-cons = (cons-cdr required-type)
				  then (cdr type-cons)
		    for type = (or (car type-cons) 'void)
		    collect (tl:walk arg env walker type)))
	     ((eq required-type '*)
	      (loop for arg in (cons-cdr form)
		    collect (tl:walk arg env walker 't)))
	     (t
	      (loop for arg in (cons-cdr form)
		    for type = required-type then 'void
		    collect (tl:walk arg env walker type)))))))




;;; The special-form `def-named-variable' is used to define variables and
;;; parameters in TL.  It takes the name of the new variable; a keyword
;;; indicating whether it is a variable, parameter, constant, or
;;; underlying-lisp-variable; and either an initialization form or the value of
;;; the no-initial-value parameter.

;;; The :underlying-lisp-variable case is where the symbol names a variable
;;; already defined in the Lisp implementation which we want to use, but need a
;;; translated C variable to take its place.  Examples are *package* and
;;; *features*.  The behavior is for the Lisp compile time to register is as a
;;; special variable but not actually expand to a defvar, and at translation
;;; time it behaves exactly as the :variable case would.

(defparameter no-initial-value (make-symbol "UNBOUND"))

(tl:declaim (special-form def-named-variable))

(defmacro def-named-variable (name var-type init)
  `(progn
     ,@(when (and *current-system-name* *current-module-name*)
	 `((tl:declaim (variable-home ,(cons *current-system-name*
					     *current-module-name*)
				      ,name))))
     ,(ecase var-type
	((:variable)
	 (if (eq init no-initial-value)
	     `(defvar ,name)
	     `(defvar ,name ,init)))
	((:parameter)
	 `(defparameter ,name ,init))
	((:constant)
	 `(defconstant ,name ,init))
	((:underlying-lisp-variable :underlying-lisp-constant)
	 nil))))

(def-special-form-walker def-named-variable (form env walker required-type)
  (declare (ignore required-type))
  (tl:destructuring-bind-strict
      (nil name var-type init)
    form
    (let ((type (or (variable-decl name 'type env) t)))
      `(def-named-variable ,name ,var-type
	 ,(if (eq init no-initial-value)
	      init
	      (tl:walk init env walker type))))))




;;; The special-form `inlined-typep' takes an object and a quoted Lisp type.
;;; This form emits code to check if the object is an instance of that type.
;;; Note that tl:typep will already have expanded the type and will have
;;; stripped off any combinations using AND, OR, or NOT.  Note that for certain
;;; types we optimize the Lisp runtime speed of this operation by macroexpanding
;;; into calls to type predicate functions that have been compiled for inlined
;;; speed.

(tl:declaim (special-form inlined-typep))

#+lucid
(proclaim '(optimize (compilation-speed 0))) ; cause TYPEP to be inlined

(defun inline-fixnum-p (x) (typep x 'fixnum))

(defun inline-double-float-p (x) (typep x 'double-float))

(defun inline-float-vector-p (x) (typep x '(simple-array double-float (*))))

(defun inline-consp (x) (consp x))

(defun inline-uint16-array-p (x) (typep x '(array (unsigned-byte 16) (*))))

(defun inline-sint16-array-p (x) (typep x '(array (signed-byte 16) (*))))

(defun inline-uint8-array-p (x) (typep x '(array (unsigned-byte 8) (*))))

(defun inline-stringp (x) (typep x 'string))

(defun inline-simple-vector-p (x) (simple-vector-p x))

#+lucid
(proclaim '(optimize (compilation-speed 3)))

(defmacro inlined-typep (object type-arg)
  ;; In this macro we are guaranteed that type is a constant.
  (let ((type (eval type-arg)))
    (cond ((tl-subtypep type 'fixnum)
	   `(inline-fixnum-p ,object))
	  ((tl-subtypep type 'double-float)
	   `(inline-double-float-p ,object))
	  ((tl-subtypep type '(array double-float))
	   `(inline-float-vector-p ,object))
	  ((tl-subtypep type 'cons)
	   `(inline-consp ,object))
	  ((tl-subtypep type '(array (unsigned-byte 16)))
	   `(inline-uint16-array-p ,object))
	  ((tl-subtypep type '(array (signed-byte 16)))
	   `(inline-sint16-array-p ,object))
	  ((tl-subtypep type '(array (unsigned-byte 8)))
	   `(inline-uint8-array-p ,object))
	  ((tl-subtypep type 'string)
	   `(inline-stringp ,object))
	  ((tl-subtypep type 'simple-vector)
	   `(inline-simple-vector-p ,object))
	  (t
	   `(tl-typep ,object ,type-arg)))))

(def-special-form-walker inlined-typep (form env walker required-type)
  (declare (ignore required-type))
  (tl:destructuring-bind-strict
      (nil object type)
    form
    (unless (symbolp object)
      (error "Type checking can only handle variable object references, not ~s"
	     object))
    (unless (tl:constantp type)
      (error "Inlined type checking can only handle constant type forms, not ~s."
	     type))
    `(inlined-typep ,(tl:walk object env walker 't) ,type)))




;;; The `tl:and' and `tl:or' special form walkers are implemented to get
;;; appropriate type propagation when these expressions are evaluated only for
;;; boolean results.

(def-special-form-walker tl:and (form env walker required-type)
  (cond ((null (cons-cdr form))
	 't)
	((null (cons-cddr form))
	 `(tl:progn ,(tl:walk (cons-second form) env walker required-type)))
	(t
	 `(tl:and
	    ,@(loop for arg-cons = (cons-cdr form) then next-cons?
		    for next-cons? = (cdr arg-cons)
		    for arg = (car arg-cons)
		    while arg-cons
		    collect (tl:walk arg env walker
				     (if next-cons? 't required-type)))))))

(def-special-form-walker tl:or (form env walker required-type)
  (cond ((null (cons-cdr form))
	 nil)
	((null (cons-cddr form))
	 `(tl:progn ,(tl:walk (cons-second form) env walker required-type)))
	(t
	 `(tl:or
	    ,@(loop for arg-cons = (cons-cdr form) then next-cons?
		    for next-cons? = (cdr arg-cons)
		    for arg = (car arg-cons)
		    while arg-cons
		    collect (tl:walk arg env walker
				     (if next-cons? 't required-type)))))))




;;; The `list-dynamic-extent' special form makes and returns a list containing
;;; the given arguments.  The lifetime of this list spans the function this
;;; special form is compiled within.  In the future, this scope may be tightened
;;; to the binding initialized by the evaluation of this form, but for now the
;;; scope continues through the entire function.  The first use of this form is
;;; to provide &rest arguments to functions and to implement initializations for
;;; variables declared dynamic-extent.  This is also used as the primitive for
;;; cons-dynamic-extent, which depends on the scope expanding to the entire
;;; surrounding function.

(tl:declaim (special-form list-dynamic-extent))

(defmacro list-dynamic-extent (&rest list-elements)
  `(list ,@list-elements))

(def-special-form-walker list-dynamic-extent (form env walker required-type)
  (declare (ignore required-type))
  (if (cons-cdr form)
      `(list-dynamic-extent
	 ,@(loop for arg in (cons-cdr form)
		 collect (tl:walk arg env walker 't)))
      nil))




;;; The macro `cons-dynamic-extent' expands into a call to list-dynamic-extent,
;;; supplying the car and the initial element of the list, and setfing the cdr
;;; of the list to the given cdr.  This results in the cdr being set twice, but
;;; hey, that's cheap compared to the cost be making yet another special-form to
;;; build a special translation just for this.  This macro depends on the scope
;;; of list-dynamic-extent extending through the entire surrounding
;;; function. -jra 1/17/97

(def-tl-macro cons-dynamic-extent (car cdr)
  (let ((temp (gensym)))
    `(tl:let ((,temp (list-dynamic-extent ,car)))
       (tl:setf (tl:cdr ,temp) ,cdr)
       ,temp)))




;;; The `funcall-internal' special form is used to perform inlined function call
;;; dispatches to the C functions within compiled-functions.  The first argument
;;; is a constant T or NIL that indicates whether the function being called is
;;; known to set the Values_count.  If NIL, then it is presumed that the called
;;; function does not set it, and this call will be compiled to receive only one
;;; value from the dispatched-to function, even if it actually does set the
;;; values count and returns more values.

(tl:declaim (special-form funcall-internal))

(defmacro funcall-internal
    (sets-values-count? compiled-function &rest function-args)
  (if sets-values-count?
      `(funcall ,compiled-function ,@function-args)
      `(values (funcall ,compiled-function ,@function-args))))

(def-special-form-walker funcall-internal (form env walker required-type)
  (declare (ignore required-type))
  (tl:destructuring-bind-strict
      (nil sets-values-count? compiled-function &rest function-args)
    form
    `(funcall-internal
       ,sets-values-count?
       ,(tl:walk compiled-function env walker 'compiled-function)
       ,@(loop for arg in function-args
	       collect (tl:walk arg env walker 't)))))



;;; The numeric operations `+' and `*' have to be done as speical forms in TL in
;;; order to allow the use of lisp:+ and lisp:* within the read-eval-print loop
;;; of the Lisp development environment.  The implementations of the walkers for
;;; these special forms will always expand into the TLI package symbols that
;;; implement these operations for us.  Note that spurious but harmless tl:progn
;;; forms get wrapped around the result so that these special form walkers are
;;; always returning a form with walked contents rather than just the walked
;;; thing itself.

(tl:declaim (special-form + *))

(def-special-form-walker + (form env walker required-type)
  `(tl:progn
     ,(tl:walk (cons 'plus (cons-cdr form)) env walker required-type)))

(def-special-form-walker * (form env walker required-type)
  `(tl:progn
     ,(tl:walk (cons 'multiply (cons-cdr form)) env walker required-type)))






;;;; C Comments




;;; The special-form `c-comment-form' takes a constant string to be emitted as a
;;; C comment and an argument expression.  This form will return the value of
;;; the argument expression, but cause a C comment to be placed onto the same
;;; line as the translation of the given form.

(tl:declaim (special-form tl:c-comment-form))

(defmacro tl:c-comment-form (comment-string form)
  (declare (ignore comment-string))
  form)

(def-special-form-walker tl:c-comment-form (form env walker required-type)
  (unless (stringp (second form))
    (error "C-comment-form requires a string as its first argument, not ~s."
	   (second form)))
  `(tl:c-comment-form
     ,(second form)
     ,(tl:walk (third form) env walker required-type)))






;;;; Defstruct forms




;;; The special forms `malloc-class-instance', `get-slot', and `set-slot' are
;;; primitives of defstruct for creating, accessing, and modifying structures.
;;; During Lisp development, we use the underlying implementation of defstruct.

(tl:declaim (special-form malloc-class-instance get-slot set-slot))

(defmacro malloc-class-instance (class-or-struct-name raw-constructor)
  (declare (ignore class-or-struct-name))
  `(,raw-constructor))

(def-special-form-walker malloc-class-instance (form env walker required-type)
  (declare (ignore env walker required-type))
  form)

(defmacro get-slot (struct slot-name lisp-arg c-arg lisp-val c-val)
  (if (and (symbolp slot-name) (class-type-p lisp-arg))
      `(the ,lisp-val (,(struct-slot-dev-reader 
			 (get-slot-for-class-and-slot-name 
			  lisp-arg slot-name))
		       ,struct))
    `(error "Attempted to evaluate translation-time get-slot for ~s"
	    (list 'get-slot ,struct ',slot-name ',lisp-arg ',c-arg 
		  ',lisp-val ',c-val))))

(def-special-form-walker get-slot (form env walker required-type)
  (declare (ignore required-type))
  `(get-slot ,(tl:walk (cons-second form) env walker (cons-fourth form))
	     ,@(cons-cddr form)))

(defmacro set-slot (struct slot-name struct-type struct-c-type
		    value-type value-c-type new-value)
  (if (and (symbolp slot-name) (class-type-p struct-type))
      `(tl:setf (,(struct-slot-dev-reader
		   (get-slot-for-class-and-slot-name struct-type slot-name))
		 ,struct)
	 (the ,value-type ,new-value))
    `(error "Attempted to evaluate translation-time set-slot for ~s"
	    (list 'set-slot ,struct ,slot-name ',struct-type ',struct-c-type
		  ',value-type ',value-c-type ,new-value))))

(def-special-form-walker set-slot (form env walker required-type)
  (declare (ignore required-type))
  (destructuring-bind (struct accessor-string struct-type struct-c-type
			      value-type value-c-type new-value)
		      (cons-cdr form)
    `(set-slot ,(tl:walk struct env walker struct-type)
	       ,accessor-string ,struct-type ,struct-c-type
	       ,value-type ,value-c-type
	       ,(tl:walk new-value env walker value-type))))




;;; Special forms to do someday over the rainbow.  -jra 11/9/95

;;; ;; multiple-value-call not implemented
;;; ;; progv not implemented
;;; ;; generic-flet, generic-labels not implemented
;;; ;; with-added-methods not implemented
;;; ;; load-time-eval not implemented
