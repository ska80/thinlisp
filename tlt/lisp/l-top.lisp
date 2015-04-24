(in-package "TLI")

;;;; Module L-TOP

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






;;;; Top Level L-exprs




;;; This module contains l-expr methods and functions for translating top-level
;;; l-expr structures into c-expr structures, which are emitted into c-funcs and
;;; then c-files (whew).

;;; The l-expr method `translate-top-level-l-expr' takes a l-expr and a c-file,
;;; and performs top level processing on that l-expr.  Top level processing for
;;; definitional forms means emitting a new function, variable, or whatever into
;;; the file.  For all other types of forms, it means emitting the l-expr as a
;;; top level form into the initialization C function for this file.  That
;;; fall-over is implemented in the default method, so all definitional l-exprs
;;; need to have specific methods that perform their correct operation.

(def-l-expr-generic-method translate-top-level-l-expr (l-expr c-file)
  (let ((top-level-function (c-file-top-level-function c-file)))
    (with-reusable-c-variables-scope (top-level-function)
      (translate-l-expr-into-c
	(walk-l-expr l-expr #'prepare-l-expr-for-translation 'void 'void)
	top-level-function
	(c-file-top-level-compound-statement c-file)
	:discard))))




;;; The translate-top-level-l-expr method for `named-lambda' creates a new
;;; c-func, a new c-namespace, translates the body into the C-func, and then
;;; emits it into the C file.

(def-l-expr-method translate-top-level-l-expr (named-lambda-l-expr c-file)
  (let* ((form (l-expr-form named-lambda-l-expr))
	 (aug-env (l-expr-aug-env named-lambda-l-expr))
	 (name (cons-second form))
	 (current-translation-context name)
	 (arglist (cons-third form))
	 (body (cons-cdddr form))
	 (decls (multiple-value-bind (type local f-decls)
		    (tl:function-information name)
		  (declare (ignore type local))
		  f-decls))
	 (ftype
	   (or (cdr (assq 'ftype decls))
	       (cdr (assq 'computed-ftype decls))
	       (translation-error
		 "Function ~s lacked an FTYPE or COMPUTED-FTYPE declaration."
		 name)))
	 (extra-storage-declarations
	   (cdr (assq 'function-storage-declarations decls)))
	 (c-return-type (c-func-type-for-lisp-func-return-type-spec
			  (cons-third ftype)))
	 (c-file-namespace (c-file-namespace c-file))
	 (c-func-namespace (make-c-namespace c-file-namespace))
	 (c-identifier (c-identifier-for-function
			 name *global-c-namespace* c-func-namespace))
	 (lisp-parameter-info
	   (loop with optional? = nil
		 for arg in arglist
		 for arg-type in (cons-second ftype)
		 if (eq arg '&optional)
		   do (setq optional? t)
		 else
		   collect
		   ;; Note that the special-form walker for named-lambda
		   ;; provides an init-form for every optional argument.
		   (let* ((arg-symbol (if optional? (cons-car arg) arg))
			  (c-identifier
			    (c-identifier-for-symbol
			      arg-symbol '(variable)
			      c-func-namespace c-func-namespace))
			  (optional-init-form?
			    (if optional? (cons-second arg) nil)))
		     (multiple-value-bind (binding-type local? decls)
			 (tl:variable-information arg-symbol aug-env)
		       (declare (ignore local?))
		       (let ((binding (cdr (assq 'variable-binding-structure
						 decls))))
			 (setf (variable-binding-c-identifier binding)
			       c-identifier)
			 (setf (variable-binding-c-type binding)
			       (c-type-for-lisp-type arg-type))
			 (setf (variable-binding-lisp-type binding)
			       arg-type)
			 (list arg-symbol optional-init-form? binding
			       binding-type decls))))))
	 (c-parameter-names
	   (loop for info in lisp-parameter-info
		 collect (variable-binding-c-identifier (cons-third info))))
	 (c-parameter-types
	   (loop for info in lisp-parameter-info
		 collect (variable-binding-c-type (cons-third info))))
	 (c-parameter-storage-classes
	   ;; This list will be overridden after we have walked and translated
	   ;; the body, determining if any of the arguments need to be declared
	   ;; volatile.
	   (loop repeat (length lisp-parameter-info) collect nil))
	 (c-body (make-c-compound-statement nil nil nil nil))
	 (c-func (make-c-func
		   extra-storage-declarations
		   c-return-type c-identifier (cons name (cons-cdr ftype))
		   c-parameter-names c-parameter-types
		   c-parameter-storage-classes
		   c-body c-func-namespace
		   c-file)))
    ;; Push this function name and identifier onto the list of defined functions
    ;; of this c-file.
    (register-defined-function c-file name c-identifier)

    ;; Set the bits and types on this form and all subforms.
    (walk-l-expr named-lambda-l-expr #'prepare-l-expr-for-translation
		 'void 'void)

    ;; Emit statements to prevent unused variable warnings from the C compiler
    ;; for ignored arguments.

    (loop with *print-pretty* = nil
	  for info in lisp-parameter-info do
      (when (cdr (assq 'ignore (cons-fifth info)))
	(emit-expr-to-compound-statement
	  (make-c-line-comment-expr
	    (make-c-cast-expr
	      'void (make-c-name-expr
		      (variable-binding-c-identifier (cons-third info))))
	    (format nil "~a was declared ignore" (cons-car info)))
	  c-body)))

    ;; Emit the l-exprs of the named-lambda into body of the C function.
    (translate-progn-body body c-func c-body :return)

    ;; Overwrite the storage-classes for the arguments using the now-modified
    ;; bindings which reflect whether or not any arguments need to be declared
    ;; volatile.
    (setf (c-func-c-parameter-storage-classes c-func)
	  (loop for info in lisp-parameter-info
		collect (if (variable-binding-volatile (cons-third info))
			    '("volatile")
			    nil)))

    ;; Emit the C function into the C file.
    (emit-function-to-c-file c-func c-file)))




;;; The translate-top-level-l-expr method for `progn-l-expr' calls the same
;;; method on each of its argument l-exprs.

(def-l-expr-method translate-top-level-l-expr (progn-l-expr c-file)
  (loop for l-expr in (cons-cdr (l-expr-form progn-l-expr)) do
    (translate-top-level-l-expr l-expr c-file)))




;;; The translate-top-level-l-expr method for `eval-when-l-expr' currently just
;;; recurses on its argument l-exprs.

(def-l-expr-method translate-top-level-l-expr (eval-when-l-expr c-file)
  (loop for l-expr in (cons-cddr (l-expr-form eval-when-l-expr)) do
    (translate-top-level-l-expr l-expr c-file)))




;;; The translate-top-level-l-expr method for `def-named-variable' defines
;;; variables, parameters, and constants.

(def-l-expr-method translate-top-level-l-expr
    (def-named-variable-l-expr c-file)
  (let* ((dnv def-named-variable-l-expr)
	 (form (l-expr-form dnv))
	 (var-name (cons-second form))
	 (var-kind (cons-third form))
	 (init (cons-fourth form))
	 (c-name (c-identifier-for-variable
		   var-name *global-c-namespace* *global-c-namespace*))
	 (lisp-type (or (variable-decl var-name 'type (l-expr-env dnv))
			t))
	 (c-type 'obj)
	 (assignment-statement (make-c-compound-statement nil nil nil nil))
	 (top-level-function (c-file-top-level-function c-file))
	 (top-level-c-body (c-file-top-level-compound-statement c-file)))
    (register-defined-variable c-file var-name c-name)
    (unless (eq init no-initial-value)
      (setq init
	    (walk-l-expr init #'prepare-l-expr-for-translation
			 lisp-type c-type)))
    (emit-declaration-to-c-file
      (make-c-var-decl
	nil c-type c-name
	(if (and (eq var-kind :constant)
		 (l-expr-constant-p init)
		 (or (tl-subtypep (l-expr-lisp-return-type init) 'fixnum)
		     (tl-subtypep (l-expr-lisp-return-type init) 'symbol)))
	    (let ((c-expr
		    (translate-l-expr-into-c
		      init top-level-function assignment-statement :c-expr)))
	      (unless (c-compound-statement-empty-p assignment-statement)
		(translation-error
		  "Translator goofed on defconstant init for ~s." var-name))
	      (setq init no-initial-value)
	      c-expr)
	    (make-c-cast-expr
	      'obj (make-c-unary-expr #\& (make-c-name-expr "Unbound")))))
      c-file 0)
    (unless (eq init no-initial-value)
      (emit-expr-to-compound-statement
	(make-c-infix-expr
	  (make-c-name-expr c-name) "="
	  (translate-l-expr-into-c 
	    init top-level-function assignment-statement :c-expr))
	assignment-statement)
      (if (memqp var-kind '(:variable :underlying-lisp-variable))
	  (emit-statement-to-compound-statement
	    (make-c-conditional-statement
	      (list
		(make-c-infix-expr
		  (make-c-name-expr c-name) "=="
		  (make-c-cast-expr
		    'obj
		    (make-c-unary-expr #\& (make-c-name-expr "Unbound"))))
		assignment-statement))
	    top-level-c-body)
	  (emit-statement-to-compound-statement
	    assignment-statement top-level-c-body)))))







;;;; Translating Top Level Forms




;;; The function `walk-form-into-l-expr' is used as a walker function to turn
;;; Lisp s-expressions into l-expr structures.  It takes a form, an environment,
;;; an augumented environment which surrounds the subforms of this form, and a
;;; required type for the result of this form.

(defun walk-form-into-l-expr (form env aug-env type)
  (declare (ignore type))
  (let ((l-expr
	  (typecase form
	    (cons
	     (let* ((name (cons-car form))
		    (constructor-function? (l-expr-constructor name)))
	       (if constructor-function?
		   (funcall constructor-function? form env aug-env)
		   (make-function-call-l-expr form env aug-env))))
	    (symbol
	     (if (or (keywordp form)
		     (null form)
		     (eq form t))
		 (make-quote-l-expr form env aug-env)
		 (make-implicit-symbol-value-l-expr form env aug-env)))
	    (t
	     (make-quote-l-expr form env aug-env)))))
    (initialize-l-expr l-expr)
    l-expr))




;;; The variable `compile-time-too-mode' is used to represent the
;;; "compile-time-too" mode described in CLtL2, pp. 88-94.

(defvar compile-time-too-mode nil)




;;; The function `translate-top-level-lisp-form' processes top level forms per
;;; the description in CLtL2, pp. 88-94.  The exceptions are that it does not
;;; treat the contents of locally, compiler-let, macrolet, or symbol-macrolet as
;;; top level forms.

(defun translate-top-level-lisp-form (form c-file)
  ;; Top level atoms are no-ops.
  (when (consp form)
    (let ((operator (cons-car form)))
      (cond
	((eq operator 'tl:progn)
	 (loop for subform in (cons-cdr form) do
	   (translate-top-level-lisp-form subform c-file)))
	((eq operator 'tl:eval-when)
	 (let* ((situations (cons-second form))
		(lt? (or (memq 'tl:load situations)
			 (memq :load-toplevel situations)))
		(ct? (or (memq 'tl:compile situations)
			 (memq :compile-toplevel situations)))
		(ex? (or (memq 'tl:eval situations)
			 (memq :execute situations))))
	   (cond
	     ((or (and lt? ct?)
		  (and lt? (not ct?) ex? compile-time-too-mode))
	      (loop with compile-time-too-mode = t
		    for subform in (cons-cddr form) do
		(translate-top-level-lisp-form subform c-file)))
	     ((or (and lt? (not ct?) (not compile-time-too-mode))
		  (and lt? (not ct?) (not ex?)))
	      (loop with compile-time-too-mode = nil
		    for subform in (cons-cddr form) do
		(translate-top-level-lisp-form subform c-file)))
	     ((or (and (not lt?) ct?)
		  (and (not lt?) (not ct?) ex? compile-time-too-mode))
	      ;; Must we?  It might be possible to skip these at translation
	      ;; time.  -jra 11/9/95
	      (eval (cons 'progn (cons-cddr form))))
	     (t
	      (error "Eval when doesn't know what to do!")))))
	((eq operator 'in-package)
	 (let ((new-package (find-package (second form))))
	   (if new-package
	       (setq *package* new-package)
	       (translation-error
		 "Error in ~s, no package named ~s exists."
		 form (second form)))
	   (let* ((top-level-function (c-file-top-level-function c-file))
		  (top-level-c-body
		    (c-file-top-level-compound-statement c-file)))
	     (with-reusable-c-variables-scope (top-level-function)
	       (when (register-used-variable c-file '*package* "SpackageS" 'obj)
		 (register-needed-variable-extern
		   c-file '("extern") 'obj "SpackageS"))
	       (when (register-used-function
		       c-file 'tl::find-package-1 "find_package_1" '(function (t) t))
		(register-needed-function-extern
		  c-file '("extern") 'obj "find_package_1" '(obj)))
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-name-expr "SpackageS")
		   "="
		   (make-c-function-call-expr
		     (make-c-name-expr "find_package_1")
		     (list
		       (translate-l-expr-into-c
			 (walk-l-expr
			   (tl:walk (second form) nil #'walk-form-into-l-expr t)
			   #'prepare-l-expr-for-translation t 'obj)
			 top-level-function
			 top-level-c-body
			 :c-expr))))
		 top-level-c-body)))))
	(t
	 (multiple-value-bind (new-form expanded?)
	     (tl:macroexpand-1 form nil)
	   (if expanded?
	       (translate-top-level-lisp-form new-form c-file)
	       (let ((l-expr (tl:walk new-form nil #'walk-form-into-l-expr 'void)))
		 (translate-top-level-l-expr l-expr c-file)))))))))
