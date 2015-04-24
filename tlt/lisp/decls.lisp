(in-package "TLI")

;;;; Module DECLS

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






;;;; Declaration Definitions




;;; This module contains definitions for all TL supported declarations (except
;;; variable-declaration, defined in ENV).  The tl:declaration is embedded in
;;; the environment implementation.  All other declarations are defined here.
;;; The declarations are taken from CLtL 2, Sec. 9.2, pp. 223-236.




;;; The `special' declaration takes a list of variable names and declares
;;; bindings and references in scope to be to a special variable, not a local
;;; lexical one.  CLtL 2, p. 223.

(tl:declaim (variable-declaration special))

(define-declaration special (decl-spec env)
  (values
    :variable
    (loop for var in (cons-cdr decl-spec)
	  unless (eq (tl:variable-information var env) :special)
	    collect (list var 'special t))))




;;; The `constant' declaration takes a list of constant names (from defconstant
;;; forms) and declares that references to this name are to a constant, not a
;;; local lexical variable.

(tl:declaim (variable-declaration constant))

(define-declaration constant (decl-spec env)
  (declare (ignore env))
  (values
    :variable
    (loop for var in (cons-cdr decl-spec)
	  collect (list var 'constant t))))



;;; The `type' declaration takes a list of variable names and declares that
;;; bindings will receive and references will return values of the given type.
;;; If a binding has this declaration in its innermost environment, then storage
;;; for the binding may be optimized (e.g. double-float to C double).  CLtL 2,
;;; p. 224.

;;; Note that type does apply to variable bindings, but it does not have the
;;; format required by variable-declaration.  Type declarations are handled
;;; specially in the code that handles variable-declarations.

(define-declaration type (decl-spec env)
  (process-type-declaration decl-spec env))

(defun process-type-declaration (decl-spec env)
  (values
    :variable
    (loop with type = (expand-type (cons-second decl-spec))
	  for var in (cons-cddr decl-spec)
	  collect
	  (list var 'type
		(let* ((previous-type? (variable-decl var 'type env)))
		  (if previous-type?
		      (duplicate-type-declaration previous-type? type)
		      type))))))




;;; The following declarations are for type symbols declared in CLtL 2, Table
;;; 4-1, p. 50.  This set of declarations is called for in the middle of p. 227.

(defmacro define-type-declarations (&rest types)
  (let ((tl-types
	  (loop for type in types
		for tl-type = (intern (symbol-name type) *tl-package*)
		unless (eq type tl-type)
		  collect (cons tl-type type))))
    `(progn
       (tl:declaim
	 (variable-declaration
	   ,@(loop for (tl-type) in tl-types collect tl-type)
	   ,@types))
       ,@(loop for (tl-type . type) in tl-types
	       collect
	       `(define-declaration ,tl-type (decl-spec env)
		  (process-type-declaration
		    (cons 'type (cons ',type (cons-cdr decl-spec))) env)))
       ,@(loop for type in types
	       collect
	       `(define-declaration ,type (decl-spec env)
		  (process-type-declaration (cons 'type decl-spec) env))))))

(define-type-declarations
    array atom bignum bit bit-vector character compiled-function complex cons
    double-float file-stream fixnum float function hash-table integer keyword
    list long-float nil null number package pathname random-state ratio rational
    readtable sequence short-float signed-byte simple-array simple-bit-vector
    simple-string simple-vector single-float standard-char stream string
    string-char symbol t unsigned-byte vector base-character
    extended-character real)




;;; The type `string-stream' must be handled specially, since I am not using the
;;; underlying Lisp's implementation of this type in development-time TL
;;; programs.

(tl:declaim (variable-declaration tl:string-stream tl-string-stream))

(define-declaration tl:string-stream (decl-spec env)
  (process-type-declaration
   (cons 'type (cons 'tl-string-stream (cons-cdr decl-spec))) env))

(define-declaration tl-string-stream (decl-spec env)
  (process-type-declaration (cons 'type decl-spec) env))




;;; The declaration `ftype' declares that functions are of a specific type.
;;; CLtL 2, p. 227.  The form of this declaration is (ftype <type> name ...).
;;; The type (function (<arg-type> ...) <value-type>) is required here.

;;; Note that CLtL 2 does not specify a means of declaring the argument types
;;; of a function without describing the value types.  For example, the type
;;; (function (fixnum) t) declares that the function receives one fixnum
;;; argument and returns one value of arbitrary type.  TL extends the function
;;; type to include an asterisk in the values location when no declaration is
;;; being made about the number or type of values returned from a funtion.  So
;;; the type for a function taking a fixnum and returns an arbitrary number of
;;; values is (function (fixnum) *)

(define-declaration ftype (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop with type = (expand-type (cons-second decl-spec))
	  with home = (if (and *current-system-name* *current-module-name*)
			  (cons *current-system-name* *current-module-name*)
			nil)
	  for func in (cons-cddr decl-spec)
	  for ftype-spec = (list (list func 'ftype type))
	  nconc (if home
		    (cons (list func 'function-home home) ftype-spec)
		  ftype-spec))))




;;; The declarations `inline' and `notinline' ask that the implementation prefer
;;; to inline calls or be prevented from inlining calls to the named functions.
;;; CLtL 2, pp. 229-230.  The implementation of this declaration is in tl:defun.
;;; If a function is inlineable, then we will define a compiler macro for that
;;; function which will inline calls to it.

(define-declaration inline (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop for func in (cons-cdr decl-spec)
	  collect (list func 'inline 'inline))))

(define-declaration notinline (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop for func in (cons-cdr decl-spec)
	  collect (list func 'inline 'notinline))))




;;; The declaration `ignore' informs the compiler that the named variables will
;;; not be referenced or set after the initial binding.  CLtL 2, p. 230.

(define-declaration ignore (decl-spec env)
  (declare (ignore env))
  (values
    :variable
    (loop for var in (cons-cdr decl-spec)
	  collect (list var 'ignore t))))




;;; The declaration `optimize' informs the Lisp system about how to weigh
;;; different factors when determining how to compile a given bit of code.  CLtL
;;; 2, p. 231.  Note that the TL translator currently ignores these settings,
;;; though I've given some thought to safety three turning on type checking
;;; where I can't prove the correctness of type declarations and requirements.
;;; -jra 4/6/95

(define-declaration optimize (decl-spec env)
  (let ((old-optimize (tl:declaration-information 'optimize env)))
    (values
      :declare
      (list
	'optimize
	(list 'speed (or (second (assq 'speed (cdr decl-spec)))
			 (second (assq 'speed old-optimize))
			 3))
	(list 'space (or (second (assq 'space (cdr decl-spec)))
			 (second (assq 'space old-optimize))
			 0))
	(list 'safety (or (second (assq 'safety (cdr decl-spec)))
			  (second (assq 'safety old-optimize))
			  0))
	(list 'compilation-speed
	      (or (second (assq 'compilation-speed (cdr decl-spec)))
		  (second (assq 'compilation-speed old-optimize))
		  0))
	(list 'raise-for-jim
	      (or (second (assq 'raise-for-jim (cdr decl-spec)))
		  (second (assq 'raise-for-jim old-optimize))
		  3))
	(list 'debug (or (second (assq 'debug (cdr decl-spec)))
			 (second (assq 'debug old-optimize))
			 0))))))




;;; The following proclaim establishes the default optimize characteristics in
;;; the global environment.

(tl:proclaim '(optimize))




;;; The declaration `dynamic-extent' is used to declare that variables or
;;; functions refer to data objects that will not returned from the scope of the
;;; binding of the variable that is declared.  Variables are declared as their
;;; symbol names.  Functions are declared as (function <name>).  There is a TL
;;; specific restriction that variables and functions may not be mixed in the
;;; same decl-spec, you must have two seperate uses of dynamic-extent to declare
;;; both variables and functions as dynamic-extent.  Note that this is not the
;;; same as WITH-TEMPORARY-AREA, and in fact TL currently ignores this
;;; declaration.  See me for a long and boring story about how Gensym's proposal
;;; for temporary areas in Common Lisp got turned into this declaration.  -jra
;;; 4/7/95

(tl:declaim (variable-declaration dynamic-extent))

(define-declaration dynamic-extent (decl-spec env)
  (declare (ignore env))
  (if (every #'symbolp (cons-cdr decl-spec))
      (values
	:variable
	(loop for var in (cons-cdr decl-spec)
	      collect (list var 'dynamic-extent t)))
      (values
	:function
	(loop for func-spec in (cons-cdr decl-spec)
	      for func = (progn
			   (unless (eq (cons-car func-spec) 'function)
			     (error "Spec ~s in dynamic-extent was not in the ~
                                     form (function <name>)"
				    func-spec))
			   (cons-second func-spec))
	      collect (list func 'dynamic-extent t)))))




;;; The following declarations are specific to TL.

;;; The `require-local-exit' declaration is used to declare that the surrounded
;;; body of code should not perform any non-local exits through the scope of
;;; this declaration.  This is needed for forms like protected-let, which set up
;;; reclaiming forms for bindings without requiring an unwind-protect, or
;;; writing-icp-message-group-recklessly.  This declaration will cause the
;;; compiler to issues warnings for code that contains return-froms that pass
;;; exit the scope of this declaration.  Note that this will not protect against
;;; throws.  A true unwind-protect is required for that.

;;; It takes the following form: (require-local-exit {<string or t>}).  If
;;; supplied, the string will be used as the warning message when the compiler
;;; detects a non-local exit through this scope.  If unsupplied, it defaults to
;;; T.

;;; This declaration affects the `exit-scope' key in the declaration-information
;;; space.

(defun add-to-exit-scope (scope-element env)
  (values
    :declare
    (cons 'exit-scope
	  (cons scope-element (tl:declaration-information 'exit-scope env)))))

(define-declaration tl:require-local-exit (decl-spec env)
  (add-to-exit-scope
    (list 'require-local-exit (or (car (cons-cdr decl-spec)) t))
    env))




;;; The `block-scope' structure is used to hold information about a block scope
;;; within an environment.  Whenever there is a new block defined, a new
;;; instance of this structure is added to the exit-scope.  The function
;;; `block-scope-struct-for-name' searches for and returns the block structure within
;;; the given environment that corresponds to the given name.  Note that this
;;; function may print a warning if there is a require-local-exit call in the
;;; exit-scope before a block is found.

(defstruct (block-scope (:constructor make-block-scope (lisp-required-type)))
  lisp-required-type
  (c-required-type nil)
  (lisp-return-type nil)
  (c-return-type nil)
  (return-from-count 0)
  (l-expr-return-from-count 0)
  (return-directive nil)
  (target-label nil)
  (value-cache-c-variable nil)
  (volatile-return-from nil))

(defun block-scope-struct-for-name (block-name env)
  (let ((exit-scope (tl:declaration-information 'exit-scope env)))
    (loop for scope-entry in exit-scope
	  for scope-type = (cons-car scope-entry) do
      (when (eq scope-type 'require-local-exit)
	(issue-local-exit-warning block-name (cons-second scope-entry)))
      (when (and (eq scope-type 'block)
		 (eq (cons-second scope-entry) block-name))
	(return (cons-third scope-entry))))))

(defun issue-local-exit-warning (block-name specific-warning)
  (cond ((stringp specific-warning)
	 (translation-warning
	   "Bad return-from or go through require-local-exit for ~s: ~a"
	   block-name specific-warning))
	(t
	 (translation-warning
	   "Bad return-from or go for ~s: returning through scope that ~
              requires a normal, local exit."
	   block-name))))




;;; The `block' declaration is used to register the fact that a named block is
;;; available within the scope of an environment.  The format of this
;;; declaration is (BLOCK <name> <block-struct>).  This declaration also affects
;;; the exit-scope key in the declaration-information space.

(define-declaration block (decl-spec env)
  (add-to-exit-scope decl-spec env))




;;; The `catch' declaration is used to register the fact that a catch form
;;; surrounds the current scope.  This is needed when unwinding the the
;;; exit-scope for a return-from.  The format of this declaration is (CATCH).
;;; This declaration affects the exit-scope key in the declaration information
;;; space.

(define-declaration catch (decl-spec env)
  (add-to-exit-scope decl-spec env))




;;; The `global-variable-binding' declaration is used to register the fact that
;;; a global variable is bound at a particular layer within the exit-scoping.
;;; The format of this declaration is (GLOBAL-VARIABLE-BINDING <var> ...).  When
;;; a return-from is executed that passes through this layer, code must be
;;; executed to unbind this variable, and that code analysis uses information
;;; from this declaration to do so.  Variables should be listed in this
;;; declaration in binding order (e.g. the same order as they are listed in a
;;; let or let*), since that affects the ways that unbinding will occur.  This
;;; declaration affects the exit-scope key in the declaration-information
;;; space.  Note that this declaration does not declare the named variables
;;; special, but it is a constraint violation if the named variables are not in
;;; fact special.

(tl:declaim (variable-declaration global-variable-binding))

(define-declaration global-variable-binding (decl-spec env)
  (add-to-exit-scope decl-spec env))




;;; The declaration `variable-binding-structure' is used during compilation to
;;; hold a mutable structure containing information about each setter and getter
;;; of a variable's value.  This declaration should only be applied by the
;;; compiler and only to variables variables at thier outermost binding scope.
;;; The format of this declaration is (variable-binding-structure <name>
;;; <struc>).

(define-declaration variable-binding-structure (decl-spec env)
  (declare (ignore env))
  (let ((var-name (cons-second decl-spec)))
    (values
      :variable
      `((,var-name variable-binding-structure ,(cons-third decl-spec))))))




;;; The structure `variable-binding' is used to track setting and getting of the
;;; value of a binding of a variable.  This is the structure that is bound into
;;; the variable-binding-structure declaration.

(defstruct (variable-binding
	     (:constructor make-variable-binding (setter-type)))
  setter-type
  (getter-type nil)
  (c-identifier nil)
  (c-type nil)
  (lisp-type nil)
  (init-info? nil)
  (volatile nil))




;;; The functions `additional-variable-binding-setter-type' and
;;; `additional-variable-setter-type' are used to register the expression result
;;; type of any expressions that modify the value of a binding of a variable.
;;; Any special form that either binds or setq's a variable must call this with
;;; the most specific type determinable from the initialization or new value
;;; expression.

(defun additional-variable-binding-setter-type (binding type)
  (let ((old-type? (variable-binding-setter-type binding))
	(new-type (cond ((eq type '*) t)
			((and (consp type) (eq (car type) 'values))
			 (or (second type) 'null))
			(t type))))
    ;; If we currently know nothing about the variable and we are setting it to
    ;; type T, then ignore this information, it doesn't really help us much.
    ;; Note that this could allow us to use getter information to pick a type
    ;; tighter than type T, but that is the point of the getter info
    ;; optimization.  -jallard 7/25/97
    (unless (and (null old-type?) (eq new-type t))
      (setf (variable-binding-setter-type binding)
	    (if old-type?
		(most-specific-common-supertype old-type? new-type)
		new-type)))))

(defun additional-variable-setter-type (var env type)
  (let ((binding? (variable-decl var 'variable-binding-structure env)))
    (when binding?
      (additional-variable-binding-setter-type binding? type))))




;;; The functions `additional-variable-binding-getter-type' and
;;; `additional-variable-getter-type' are used to register the required types
;;; for any references to the values of a variable binding.  The code walker
;;; should call this function for any walks that happen across a variable
;;; reference.

(defun additional-variable-binding-getter-type (binding type)
  (let ((old-type? (variable-binding-getter-type binding))
	(new-type (cond ((eq type '*) t)
			((and (consp type) (eq (car type) 'values))
			 (or (second type) 'null))
			(t type))))
    (setf (variable-binding-getter-type binding)
	  (if old-type?
	      (most-specific-common-supertype old-type? new-type)
	      new-type))))

(defun additional-variable-getter-type (var env type)
  (unless (eq type 'void)
    (let ((binding? (variable-decl var 'variable-binding-structure env)))
      (when binding?
	(additional-variable-binding-getter-type binding? type)))))




;;; The `protect' struct is used to represent information about a
;;; protected body of forms.

(defstruct (protect (:constructor make-protect ()))
  (exit-path-identifier nil)
  (value-cache-identifier nil)
  (values-count-identifier nil)
  (values-buffer-identifier nil)
  (exit-descriptions nil)
  (cleanup-statement-label nil))



;;; The `unwind-protect' declaration is used to register the fact that an
;;; unwind-protect protected form surrounds the scope of the environment.  The
;;; format of this declaration is (UNWIND-PROTECT <unwind-protect-struct>).
;;; This declaration affects the exit-scope key in the declaration-information
;;; space.

(define-declaration unwind-protect (decl-spec env)
  (add-to-exit-scope decl-spec env))




;;; The `tl:allow-unwind-protect' declaration is used to declare that within a
;;; scope calls to unwind-protect or catch will not cause warnings.  Though it
;;; originally had a different purpose, this declaration is required so that
;;; programmers are required to acknowledge that these very slow operations may
;;; be included in their code.  The format is (ALLOW-UNWIND-PROTECT).

(define-declaration tl:allow-unwind-protect (decl-spec env)
  (declare (ignore decl-spec env))
  (values
    :declare 
    (list 'tl:allow-unwind-protect t)))




;;; The functions `env-requires-volatile-p' and `storage-classes-for-env' are
;;; both used to determine the storage classes for variables defined within
;;; scopes.  When allow-unwind-protect is true within a scope, then the volatile
;;; storage class should be used for any declared or modified variables.

(defun env-requires-volatile-p (env)
  (tl:declaration-information 'tl:allow-unwind-protect env))

(defun storage-classes-for-env (env)
  (if (env-requires-volatile-p env)
      '("volatile")
      nil))




;;; The `special-form' declaration is used to register the fact that a symbol
;;; names a TL special-form.  The format is (SPECIAL-FORM <name> ...).  This
;;; affects both the type of this name in the tl:function-information space and
;;; affects the special-form key in the declarations for this name.

(define-declaration special-form (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop for name in (cons-cdr decl-spec)
	  collect (list name 'special-form nil))))




;;; The `special-form-walker' declaration is used to associate a form walker
;;; function with a special form.  The format is (SPECIAL-FORM-WALKER <name>
;;; <function>).  This affects the special-form-walker key in the
;;; function-information declarations for the given name.  The function should
;;; be an object acceptible to funcall.

(define-declaration special-form-walker (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (list (list (cons-second decl-spec)
		'special-form-walker
		(cons-third decl-spec)))))




;;; The `tagbody-tags' declaration is used to register the existance of a set of
;;; available tags for go special forms, along with a structure for storing
;;; information about each tag.  This declaration affects the exit-scope key in
;;; the declaration space.  The format is (tagbody-tags (<tag> <struct>)...).

(define-declaration tagbody-tags (decl-spec env)
  (add-to-exit-scope decl-spec env))

(defstruct (tagbody-scope (:constructor make-tagbody-scope (name)))
  name
  (reference-count 0)
  (c-identifier nil))
  




;;; The `tl:macros-only' declaration is a TL specific extension to CL.  It says
;;; that the given function names are functions for available only for the macro
;;; expansion pass of a compilation.  The format of this declaration is
;;; (MACROS-ONLY <func-name>...) or (MACROS-ONLY).  The second form should only
;;; be used at top level of a given function.  The first form should be used at
;;; top level.

(define-declaration tl:macros-only (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (if (cons-cdr decl-spec)
	(loop for func in (cons-cdr decl-spec)
	      collect (list func 'tl:macros-only t))
	(list (list t 'tl:macros-only t)))))




;;; The declaration `tl:return-type' is a TL specific extension to CL.  When
;;; declared at the top level of a function, it is equivalent to an ftype
;;; declaration for the current function at top level of the file using the
;;; given type as a return type, and whatever types are declared for the
;;; argument variables as the argument types (in fact, defun macroexpands into a
;;; declaim ftype for this function when return-type is supplied).  This
;;; declaration provides no new functionality beyond ftype, but it may be more
;;; convenient to use.

(define-declaration tl:return-type (decl-spec env)
  (declare (ignore env))
  (values
    :declare
    (cons 'tl:return-type (expand-type (cons-second decl-spec)))))




;;; The declaration `local-function-global-name' is used during compilation to
;;; hold the global names of functions declared by flet and labels.  All such
;;; functions are defined as global functions, and references to the local names
;;; are transformed into calls to the global names found in this declaration.
;;; The format of this declaration is (local-function-global-name <local-name>
;;; <global-name>).

(define-declaration local-function-global-name (decl-spec env)
  (declare (ignore env))
  (values
    :function
    `((,(cons-second decl-spec)
	local-function-global-name ,(cons-third decl-spec)))))




;;; The declaration `scope-name' is used to hold a symbol that names the current
;;; scope.  In most cases (actaully all of them that I can think of now) this
;;; will be the name of the enclosing function.  The format of this declaration
;;; is (scope-name <symbol>), and it affects the declaration-information space.

(define-declaration scope-name (decl-spec env)
  (declare (ignore env))
  (values
    :declare
    (cons 'scope-name (cons-second decl-spec))))




;;; The declaration `computed-ftype' is used to hold a function type
;;; specification as determined by compile-time analysis of the named function.
;;; This specification will be used to translate the function and to translate
;;; calls to the function.  This type spec may be overridden by an explict ftype
;;; declaration by the user.  The format of this declaration is (computed-ftype
;;; <type> <name>...), where type is (function (<arg-types>...) <result-type>),
;;; where result-type may be <type>, (values <type>...), or an asterisc (i.e. *)
;;; if the function returns an unknown number of values of unknown types.

(define-declaration computed-ftype (decl-spec env)
  (declare (ignore env))
  (tl:destructuring-bind-strict (nil decl-type . names) decl-spec
    (values
      :function
      (loop with type = (expand-type decl-type)
	    with home = (if (and *current-system-name* *current-module-name*)
			    (cons *current-system-name* *current-module-name*)
			  nil)
	    for name in names
	    for ftype-spec = (list (list name 'computed-ftype type))
	    nconc (if home
		      (cons (list name 'function-home home) ftype-spec)
		    ftype-spec)))))




;;; The declaration `optional-arg-default-values' is used to hold the initforms
;;; for uninitialized optional arguments to the named function.  This list will
;;; be used to augment calls to this function if an adequate number of arguments
;;; have not been supplied.  The format of this declaration is
;;; (optional-arg-default-values <list-of-init-forms> <function-name>...).

(define-declaration optional-arg-default-values (decl-spec env)
  (declare (ignore env))
  (tl:destructuring-bind-strict (nil init-list . names) decl-spec
    (values
      :function
      (loop for name in names
	    collect (list name 'optional-arg-default-values init-list)))))




;;; The declaration `foreign-c-identifier' is used to hold the C function names
;;; given as the name for foreign function declarations.  This is used when
;;; computing the function-c-identifier for a Lisp symbol.

(define-declaration foreign-c-identifier (decl-spec env)
  (declare (ignore env))
  (tl:destructuring-bind-strict (nil symbol string) decl-spec
    (values
      :function
      (list (list symbol 'foreign-c-identifier string)))))




;;; The declaration `function-c-identifier' is used to hold the string that
;;; represents the C identifier for a function.  It is used at translation time
;;; to record and then determine the appropriate C name for a Lisp function
;;; name.  The format of this declaration is (function-c-identifier
;;; <lisp-symbolic-name> <c-identifier-string>).

(define-declaration function-c-identifier (decl-spec env)
  (declare (ignore env))
  (tl:destructuring-bind-strict (nil symbol string) decl-spec
    (values
      :function
      (list (list symbol 'function-c-identifier string)))))




;;; The declaration `variable-c-identifier' is used to hold the string that
;;; represents the C identifier for a Lisp global variable.  (The identifiers
;;; for lexical variables are held in the variable-binding-structure.)  The
;;; format of this declaration is (variable-c-identifier <lisp-symbol>
;;; <c-identifier-string>).

(tl:declaim (variable-declaration variable-c-identifier))

(define-declaration variable-c-identifier (decl-spec env)
  (declare (ignore env))
  (tl:destructuring-bind-strict (nil symbol string) decl-spec
    (values
      :variable
      (list (list symbol 'variable-c-identifier string)))))




;;; The declaration `tl:functional' is used on functions to declare that their
;;; value is completely determined by their arguments (i.e. they refer to no
;;; globals) and that they perform no side-effects.  This is a stronger
;;; declaration than side-effect free, and so this declaration has the
;;; side-effect of also declaring the function side-effect free.  Funtional
;;; operations have the distinction of being constant-foldable given constant
;;; arguments.  The functions + and SVREF are prototypical examples of
;;; functional operations.

(define-declaration tl:functional (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop for function in (cons-cdr decl-spec)
	  nconc (list (list function 'tl:functional t)
		      (list function 'tl:side-effect-free t)))))




;;; The declaration `tl:side-effect-free' is used to declare that a function
;;; performs no side-effects.  Calls to side-effect free functions may be
;;; translated in-line in C without harming the left-to-right evaluation
;;; semantics called for by Common Lisp (and TL).

(define-declaration tl:side-effect-free (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop for function in (cons-cdr decl-spec)
	  collect (list function 'tl:side-effect-free t))))




;;; The declaration `c-translator' is a TL internal function declaration to
;;; associate a set of functions for translating function forms with a function.
;;; This declaration is only issued by the macroexpansion of the form
;;; def-c-translation.  The format of the declaration is (c-translator
;;; <function-set> <function-name>...).

(define-declaration c-translator (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop for function in (cons-cddr decl-spec)
	  collect (list function 'c-translator (cons-second decl-spec)))))




;;; The declaration `tl:fat-and-slow' is required around code scopes containing
;;; generic arithmetic.  This declaration is intentionally pejorative to
;;; encourage programmers to fully type declare their arithmetic and achieve
;;; inline expansions.  The format of the declaration is (fat-and-slow).

(define-declaration tl:fat-and-slow (decl-spec env)
  (declare (ignore decl-spec env))
  (values :declare (list 'tl:fat-and-slow t)))




;;; The macro `fat-and-slow-warning' takes an environment, an operation name
;;; and a form, and will issue a translation-warning if the fat-and-slow
;;; declaration is not in effect for the given environment.  Call this in
;;; translation code that you wish the user would avoid by offering more type
;;; declarations, and this code will appropriately scold them.  Note that this
;;; is a non-standard macro, in that the operator and form arguments will not be
;;; evaluated unless a warning is actually given.

(defun fat-and-slow-warning (env operation form)
  (unless (tl:declaration-information 'tl:fat-and-slow env)
    (translation-warning
      "~s called without type declarations or fat-and-slow declaration.
   The source was:  ~s~%"
      operation form)))

(defun fat-and-slow-warning-with-description (env string form)
  (unless (tl:declaration-information 'tl:fat-and-slow env)
    (translation-warning
      "~a
  The source was :  ~s~%" 
      string form)))




;;; The declaration `tl:consing-area' is used to declare the expected consing
;;; area within a scope.  Note that this declaration does not change the consing
;;; area, it only makes it apparent within a lexical scope what the expected
;;; dynamic consing scope will be.  Either this declaration or an actaul
;;; with-permanent-area or with-temporary-area declaration is required around
;;; all translated consing operations.  The format of this declaration is
;;; (tl:consing-area {tl:permanent | tl:temporary | tl:either).  The value
;;; tl:either should be declared within defun forms that define a function that
;;; can be called from either a permanent or temporary consing scope.  This
;;; declaration will prevent compile-time warnings from the body of the declared
;;; function, and it will declare this function as a tl:conser, implying that
;;; the call to this function must itself be wrapped in a permanent, temporary,
;;; or either consing scope.  Forms compiled at top level by default are in the
;;; permanent consing area.  The translation of DEFUN sets the consing area to
;;; NIL so that forms within DEFUN must perform their own declaration, unless
;;; the function is declared a tl:conser, in which case the consing area
;;; defaults to tl:either.

(define-declaration tl:consing-area (decl-spec env)
  (declare (ignore env))
  (tl:destructuring-bind-strict (nil type)
    decl-spec
    (unless (memqp type '(tl:permanent tl:temporary tl:either nil))
      (error "The consing-area declaration received the type ~s.~
              The type must be either permanent, temporary, or either."
	     type))
    (values :declare (cons 'tl:consing-area type))))

; (tl:proclaim '(tl:consing-area tl:permanent))

;; If the consing complaints get to be too much for you, comment back in the
;; line above to set the global default.  -jallard 8/28/99




;;; The declaration `tl:conser' is used declare that a named function will
;;; perform consing, and so must be called from a scope that has a lexically
;;; apparent tl:consing-area declaration.  Note that declaring the consing-area
;;; of either at the outermost scope of a function implies that the function is
;;; a conser.  The format for this declaration is (tl:conser <function-name>...).

(define-declaration tl:conser (decl-spec env)
  (declare (ignore env))
  (values
   :function
   (loop for function in (cons-cdr decl-spec)
       collect `(,function tl:conser t))))




;;; The declaration `setq-counter' for variables is used monitor how many
;;; mutations happen to a variable within a particular scope.  This information
;;; is used for various optimizations such as determining whether or not a
;;; variable may be eliminated and replaced with the variable that initialized
;;; it.  The format for this declaration is (setq-counter <variable-name> (0)).

(define-declaration setq-counter (decl-spec env)
  (tl:destructuring-bind-strict
      (nil variable-name counter-cons)
    decl-spec
    (values
      :variable
      (list (list variable-name
		  'setq-counter
		  (cons counter-cons
			(variable-decl variable-name 'setq-counter env)))))))




;;; The declaration `tl:dll-exported-function' for functions is used to declare
;;; that the translation of a function will be exported from the DLL built from
;;; the translation.  This only has an effect on Windows machines, in all other
;;; cases the declaration is ignored.  The format for this declaration is
;;; (dll-exported <function-name>...).

(define-declaration tl:dll-exported-function (decl-spec env)
  (values
    :function
    (loop for function in (cdr decl-spec)
	  for storage-decls
	      = (function-decl function 'function-storage-declarations env)
	  collect (list
		    function 'function-storage-declarations
		    (if (member "DLLEXPORT" storage-decls :test #'string=)
			storage-decls
			(append storage-decls (list "DLLEXPORT")))))))




;;; The declaration `tl:static-function' for functions is used to declare that
;;; the translation of the function will only be used from within the same file
;;; that defines the function.  The format for this declaration is
;;; (static-function <function-name>...).

(define-declaration tl:static-function (decl-spec env)
  (values
    :function
    (loop for function in (cdr decl-spec)
	  for storage-decls
	      = (function-decl function 'function-storage-declarations env)
	  collect (list
		    function 'function-storage-declarations
		    (if (member "static" storage-decls :test #'string=)
			storage-decls
			(append storage-decls (list "static")))))))




;;; The declaration `class-name' is declared on every class and structure name.
;;; It's expansion will also include class home information, if the current
;;; global variable bindings for system names and modules names allow it.

(define-declaration class-name (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop with home = (if (and *current-system-name* *current-module-name*)
			  (cons *current-system-name* *current-module-name*)
			nil)
	  for class-name in (cons-cdr decl-spec)
	  for class-spec = (list (list class-name 'class-name t))
	  nconc (if home
		    (cons (list class-name 'class-home home) class-spec)
		  class-spec))))





;;; The declaration `function-home' for functions is used to register which
;;; system and module a symbol is defined within.  This is used when reserving C
;;; name identifiers for systems during translations.  The format for this
;;; declaration is (function-home (<system> . <module>) <function-name>...).
;;; The system and modules names are symbols in the tl-user package.  The
;;; declaration `variable-home' is the same thing, but for global variables.
;;; The declaration `class-home' is the same for names of defined classes and
;;; structures.  The class-home declaration is stored in the
;;; declaration space, not with variables or functions.

(define-declaration function-home (decl-spec env)
  (declare (ignore env))
  (values
    :function
    (loop with home = (second decl-spec)
	  for function in (cddr decl-spec)
	  collect (list function 'function-home home))))

(define-declaration variable-home (decl-spec env)
  (declare (ignore env))
  (values
    :variable
    (loop with home = (second decl-spec)
	  for variable in (cddr decl-spec)
	  collect (list variable 'variable-home home))))

(define-declaration class-home (decl-spec env)
  (declare (ignore env))
  (values
   :function
   (loop for home = (second decl-spec)
	 for class in (cddr decl-spec)
	 collect (list class 'class-home home))))
