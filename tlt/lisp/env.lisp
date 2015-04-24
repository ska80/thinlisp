(in-package "TLI")

;;;; Module ENV

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






;;;; Environment Operations




;;; This module implements support for environments and declarations that affect
;;; environments.  It contains the operations called for in CLtL 2 Chapter 9,
;;; Declarations, and the environment support needed in Chapter 8, Macros.




;;; This section implements operations that manipulate environments.  The
;;; `environment' structure is used to hold environment information for
;;; macroexpansion.  It contains local-env slots for each of the variable,
;;; function, and declaration namespaces, and a next-env? slot.  The local-env
;;; slots will contain either an alist or a hash table of local environment
;;; information.  The local-env will be a hash-table only when the set of data
;;; within the local-env is large enough to justify it.  The next-env? slot
;;; points to the environment that this environment was based upon.  If the
;;; information being sought is not found in the local-env, then access
;;; operations should continue the search into the next-env.  The hash tables
;;; and alists all use symbols as keys and have arbitrary values.  The
;;; comparisons of keys are by EQ.  The macros `search-variable-environment',
;;; `search-function-environment', and `search-declaration-environment' take a
;;; symbol, an environment, and optionally a default value and return the value
;;; associated with the given symbol key.  New enviroments are made by
;;; augment-environment.

(defstruct (environment
	    (:constructor
	     make-environment-1
	     (local-variable-env local-function-env local-declaration-env 
				 next-env?))
      (:print-function print-environment))
  local-variable-env
  local-function-env
  local-declaration-env
  next-env?)

(defparameter hash-optimization-point 20)

(defun add-entries-to-environment-hash (env-hash-table env-alist)
  (loop for (key . value) in env-alist do
    (setf (gethash key env-hash-table) value))
  env-hash-table)

(defun hash-env-alist-if-necessary (env-alist inherited-env)
  (if (and (consp env-alist)
	   (>= (length env-alist) hash-optimization-point))
      (add-entries-to-environment-hash
	(make-hash-table :test #'eq)
	env-alist)
      (if env-alist
	  env-alist
	  inherited-env)))




;;; The function `make-environment' is used to create a new environment,
;;; inheriting from the given environment, but newly containing the given
;;; variable, function, and declaration environment members.  If any of these
;;; environment namespaces contains sufficient members to justify the memory
;;; required for a faster lookup, that set of environment additions will be
;;; stored in hash table.

(defun make-environment (var-env func-env decl-env env)
  (make-environment-1
     (hash-env-alist-if-necessary
       var-env
       (environment-local-variable-env env))
     (hash-env-alist-if-necessary
       func-env
       (environment-local-function-env env))
     (hash-env-alist-if-necessary
       decl-env
       (environment-local-declaration-env env))
     env))




;;; The function `make-new-global-env' should be used to make the first non-null
;;; global environment.  All namespaces within it will be stored in hash tables,
;;; since it is presumed that the set of data eventually stored here will be
;;; large.  Note that this environment is initialized to contain one decl-spec,
;;; so that the declaration that defines new declarations is already set up.

(defun make-new-global-env ()
  (make-environment-1
    (make-hash-table :test #'eq)
    (make-hash-table :test #'eq)
    (add-entries-to-environment-hash
      (make-hash-table :test #'eq)
      '((declaration declaration tl:variable-declaration variable-declaration)))
    nil))




;;; The global variable `global-tlt-environment' contains an environment holding
;;; globally proclaimed (or declaimed) declarations.  The macro `env-or-default'
;;; is used to take environment arguments to functions that receive them, and to
;;; either return any valid environments given or to return the global
;;; environment.  Note that this function can receive environment arguments from
;;; the underlying Lisp implementation, in which case it should return the
;;; default, global TLT environment.

(defvar global-tlt-environment (make-new-global-env))

(defmacro env-or-default (environment?)
  (let ((env? (gensym)))
    `(let ((,env? ,environment?))
       ;; Lucid uses conses for its environments, CMULisp uses structures.  Test
       ;; for our specific structure type here.
       (if (environment-p ,env?)
	   ,env?
	 global-tlt-environment))))




;;; The function `global-environment-p' takes an env argument and returns
;;; whether or not it is the global one, or is an environment arg that would
;;; default to the global one, i.e. NIL or an underlying Lisp environment.

(defun global-environment-p (environment?)
  (or (null environment?)
      (consp environment?)
      (eq environment? global-tlt-environment)))




;;; The variable `verbose-environment-printing' is used to control the printer
;;; for environment structures.  By define, an extremely short print is given of
;;; environments.  However, if this variable is set to true, then the printer
;;; for environments will display the variable, function, and declaration
;;; contents for printed environments.  The function `describe-environment'
;;; binds this variable to true and then prints the environment.

(defvar verbose-environment-printing nil)

(defun describe-environment (env)
  (let ((verbose-environment-printing t))
    (prin1 env)
    env))




;;; The function `print-environment' is the structure printer for environments.
;;; Note that it will not print the global environment, since it is always too
;;; large.  To print the global environment, call print-global-environment.

(defun print-environment (env stream level)
  (with-printing-wrapper (env stream)
    (write-string "ENV" stream)
    (cond ((and (numberp *print-level*) (> level *print-level*))
	   (write-string " ..." stream))
	  ((eq env global-tlt-environment)
	   (write-string " GLOBAL-ENV" stream))
	  (verbose-environment-printing
	   (let* ((next-env (environment-next-env? env))
		  (var-env (environment-local-variable-env env))
		  (next-var-env (and next-env
				     (environment-local-variable-env next-env)))
		  (func-env (environment-local-function-env env))
		  (next-func-env (and next-env
				      (environment-local-function-env next-env)))
		  (decl-env (environment-local-declaration-env env))
		  (next-decl-env (and next-env
				      (environment-local-declaration-env next-env))))
	     (when (and var-env (not (eq var-env next-var-env)))
	       (print-env-contents
		 "Vars" (environment-local-variable-env env) stream))
	     (when (and func-env (not (eq func-env next-func-env)))
	       (print-env-contents
		 "Funcs" (environment-local-function-env env) stream))
	     (when (and decl-env (not (eq decl-env next-decl-env)))
	       (print-env-contents
		 "Decls" (environment-local-declaration-env env) stream))
	     (format stream " Next-env: ~a" next-env))))))

(defun print-env-contents (env-name alist-or-hash-table stream)
  (when alist-or-hash-table
    (format stream "~%~a:" env-name)
    (if (hash-table-p alist-or-hash-table)
	(let ((max-length (or *print-length* most-positive-fixnum))
	      (count 0))
	  (write-string " Hash" stream)
	  (maphash
	    #'(lambda (key value)
		(cond ((< count max-length)
		       (when (> count 0) (write-char #\, stream))
		       (format stream "~% ~s => ~s" key value)
		       (incf count))
		      ((= count max-length)
		       (write-string "..." stream)
		       (incf count))
		      (t ; do nothing
		       nil)))
	    alist-or-hash-table))
	(loop with max-length = (or *print-length* most-positive-fixnum)
	      for count from 0 to max-length
	      for alist-entry in alist-or-hash-table
	      do
	  (cond ((< count max-length)
		 (when (> count 0) (write-char #\, stream))
		 (format stream "~% ~s => ~s"
			 (car alist-entry) (cdr alist-entry)))
		((= count max-length)
		 (write-string "..." stream)))))
    (write-char #\; stream)))      

(defun print-global-environment ()
  (let* ((global-env global-tlt-environment)
	 (global-tlt-environment nil)
	 (verbose-environment-printing t))
    (print-environment global-env *standard-output* 0)))
    



;;; The macro search-environment should be used to perform all searches within
;;; environment namespaces.  It should only be called by
;;; `search-function-environment', `search-variable-environment', and
;;; `search-declaration-environment', which pass in the appropriate environment
;;; accessor symbol as the first argument.

(defmacro search-variable-environment (symbol env &optional default)
  `(search-environment environment-local-variable-env ,symbol ,env ,default))

(defmacro search-function-environment (symbol env &optional default)
  `(search-environment environment-local-function-env ,symbol ,env ,default))

(defmacro search-declaration-environment (symbol env &optional default)
  `(search-environment environment-local-declaration-env ,symbol ,env ,default))

(defmacro search-environment (env-accessor symbol env default)
  (let ((key (gensym))
	(this-env (gensym))
	(local-env (gensym))
	(default-value (gensym))
	(found-value (gensym)))
    `(loop with ,key = ,symbol
	   with ,this-env = ,env
	   with ,default-value = ,default
	   with ,local-env
	   with ,found-value
	   while ,this-env
	   do
       (setq ,local-env (,env-accessor ,this-env))
       (setq ,found-value (if (hash-table-p ,local-env)
			      (gethash ,key ,local-env ,default-value)
			    (or (cdr (assq ,symbol ,local-env))
				,default-value)))
       (cond ((eq ,found-value ,default-value)
	      (setq ,this-env (environment-next-env? ,this-env)))
	     (t
	      (return ,found-value)))
	   finally (return ,default-value))))




;;; The macro `tl:proclaim' takes a declaration specifier and puts it into
;;; effect globally within the Lisp environment.  See CLtL 2, Sec. 9,1, p. 222
;;; for details.

(defmacro tl:proclaim (decl-spec)
  (unless (memq :translator *features*)
    `(progn
       ,@(when (and (constantp decl-spec)
		    (let ((spec (eval decl-spec)))
		      (and (consp spec)
			   (eq (car spec) 'special))))
	   ;; Let the underlying Lisp see special declarations.
	   `((proclaim ,decl-spec)))
       (proclaim-decl-list (list ,decl-spec)))))

(defun proclaim-decl-list (decl-specs)
  (multiple-value-bind (variable-env function-env declaration-env)
      (collect-environment-augmentations
	global-tlt-environment nil nil nil nil decl-specs)
    (when variable-env
      (add-entries-to-environment-hash
	(environment-local-variable-env global-tlt-environment)
	variable-env))
    (when function-env
      (add-entries-to-environment-hash
	(environment-local-function-env global-tlt-environment)
	function-env))
    (when declaration-env
      (add-entries-to-environment-hash
	(environment-local-declaration-env global-tlt-environment)
	declaration-env))
    nil))




;;; The macro `tl:declaim' takes a set of declaration specs and enters them into
;;; the global environment.  They are not evaluated.

(defmacro tl:declaim (&rest decl-specs)
  (unless (memq :translator *features*)
    `(progn
       ,@(loop for decl in decl-specs
	       when (memqp (car decl) '(special declaration))
		 collect `(lisp-declaim ,decl))
       (tl:eval-when (:compile-toplevel :load-toplevel :execute)
	 (proclaim-decl-list ',decl-specs)))))




;;; The macro definition for `tl:eval-when' normally would be in SPECIAL, but
;;; has been moved forward here to support its use by declaim.

(defmacro tl:eval-when (situations &body forms)
  (let ((lisp-situations
	  (loop for situation in situations
		collect (or (cdr (assq situation 
				       #+lucid
				       '((tl:compile . compile)
					 (tl:load    . load)
					 (tl:eval    . eval))
				       #-lucid
				       '((tl:compile . :compile-toplevel)
					 (tl:load    . :load-toplevel)
					 (tl:eval    . :execute)
					 (compile    . :compile-toplevel)
					 (load       . :load-toplevel)
					 (eval       . :execute))))
			    situation))))
    `(eval-when ,lisp-situations ,@forms)))




;;; The macro `tl:define-declaration' is used to extend the set of declarations
;;; handled by the environment management code.  See CLtL 2, Sec. 8.5, p. 213
;;; for its specification.

(defmacro tl:define-declaration (decl-name lambda-list &body forms)
  (unless (eval-feature :translator)
    (let* ((function-name
	     (intern (format nil "~a-~a-DECL-DEFN"
			     (package-name (symbol-package decl-name))
			     decl-name)
		     *tli-package*)))
      `(progn
	 (defun ,function-name ,lambda-list ,@forms)
	 (setf (get ',decl-name :declaration-definition) #',function-name)
	 (tl:declaim (declaration ,decl-name))
	 ',decl-name))))

(defmacro define-declaration (decl-name lambda-list &body forms)
  `(tl:define-declaration ,decl-name ,lambda-list ,@forms))







;;; The `declaration declaration' introduces new declarations needed by an
;;; application.  The `variable-declaration' declaration specifies that a
;;; previously declared declaration applies to variable bindings.  It must be
;;; bootstrapped into the global environment via special code in augment
;;; environment, but it is redefined here.

;;; The definition of the `declaration' kind of declaration must be installed
;;; by hand, since the expansion of the tl:define-declaration macro depends on
;;; declaration already being available.

(defun declaration-decl-defn (decl-spec env)
  (let ((current-declarations (tl:declaration-information 'declaration env)))
    (loop for new-declaration in (cons-cdr decl-spec) do
      (pushnew new-declaration current-declarations))
    (values :declare (cons 'declaration current-declarations))))

(setf (get 'declaration :declaration-definition) #'declaration-decl-defn)




;;; The `variable-declaration' declaration is a TL declaration extension that
;;; registers the fact that a particular declaration applies to variable
;;; bindings, and that it has the form (<decl-name> <var-name>...).  This
;;; declaration is used when finding the subset of a block of declarations that
;;; apply to variable bindings for sequential binding forms like let*.  Let*
;;; needs to be able to incrementally augment the environment for each variable
;;; in turn, applying declarations that are needed for that environment one by
;;; one as state is built up.  This requires knowing which declarations apply to
;;; variable bindings, and being able to rewrite those declarations to pluck out
;;; the information needed for a single variable in that set and generating a
;;; new declaration that applies to that one variable.  If you define new
;;; declarations with the `declaration' declaration, then you should also
;;; declare them using `variable-declaration' if they apply to variable
;;; bindings.  Otherwise, their handling in let* may not be accurate, in that it
;;; won't be applied within the scope of initialization expressions for bindings
;;; following the one that should have been affected.

;;; The format is (variable-declaration <decl-name>...).

(defun variable-declaration-decl-defn (decl-spec env)
  (values
    :declare
    (cons 'tl:variable-declaration
	  (union (cons-cdr decl-spec)
		 (tl:declaration-information 'tl:variable-declaration env)))))

(setf (get 'tl:variable-declaration :declaration-definition)
      #'variable-declaration-decl-defn)

(setf (get 'variable-declaration :declaration-definition)
      #'variable-declaration-decl-defn)




;;; The function `tl:augment-environment' is used to add information to a given
;;; environment, returning the new environment.  The given environment is not
;;; mutated.  See CLtL 2, Sec. 8.5, p. 211 for specification.

(defun tl:augment-environment
    (env &key variable symbol-macro function macro declare)
  (setq env (env-or-default env))
  (multiple-value-bind (variable-env function-env declaration-env)
      (collect-environment-augmentations
	env variable symbol-macro function macro declare)
    (if (and (null variable-env) (null function-env) (null declaration-env))
	(if (eq env global-tlt-environment)
	    nil
	    env)
	(make-environment variable-env function-env declaration-env env))))
    
(defun collect-environment-augmentations
    (env variable symbol-macro function macro declare)
  (let ((variable-env nil)
	(function-env nil)
	(declaration-env nil))
    (loop for var in variable
	  for kind? = (tl:variable-information var nil)
          do
      (push (list var (if (memqp kind? '(:special :constant)) kind? :lexical)
		  t nil)
	    variable-env))
    (loop for defn in symbol-macro do
      (push (list (cons-car defn) :symbol-macro (cons-second defn) nil)
	    variable-env))
    (loop for func in function do
      (push (list func :function t nil) function-env))
    (loop for defn in macro do
      (push (list (cons-car defn) :macro (cons-second defn) nil) function-env))
    (loop for decl-spec in declare
	  for decl-name = (cons-car decl-spec)
	  for decl-defn = (get decl-name :declaration-definition)
	  do
      (when (null decl-defn)
	(translation-error "Unknown declaration type :~s" decl-name))
      (multiple-value-bind (decl-kind decl-info)
	  (funcall decl-defn decl-spec env)
	(ecase decl-kind
	  ((:variable)
	   (loop for (var-name key value) in decl-info
		 for local-binding =
		 (or (assq var-name variable-env)
		     (multiple-value-bind (type? local? decls?)
			 (tl:variable-information var-name env)
		       (let ((new-binding (list var-name type? local? decls?)))
			 (push new-binding variable-env)
			 new-binding)))
		 for bindings-cons = (cons-cdddr local-binding)
		 do
	     (cond ((eq key 'special)
		    (setf (second local-binding) :special))
		   ((eq key 'constant)
		    (setf (second local-binding) :constant)))
	     (setf (car bindings-cons)
		   (if (eq env global-tlt-environment)
		       (add-to-alist-without-duplication
			 key value (cons-car bindings-cons))
		       (cons (cons key value) (cons-car bindings-cons))))))
	  ((:function)
	   (loop for (func-name key value) in decl-info
		 for binding-type = (cond ((or (eq decl-name 'ftype)
					       (eq decl-name 'computed-ftype))
					   :function)
					  ((eq decl-name 'special-form)
					   :special-form)
					  (t nil))
		 for local-binding =
		 (or (assq func-name function-env)
		     (multiple-value-bind (type? local? decls?)
			 (tl:function-information func-name env)
		       (let ((new-binding (list func-name type? local? decls?)))
			 (push new-binding function-env)
			 new-binding)))
		 for bindings-cons = (cons-cdddr local-binding)
		 do
	     (when binding-type
	       ;; When the delcaration type implies a binding type
	       ;; (i.e. binding-type is non-null), and when the previous binding
	       ;; had a type that was explicitly determined, and the type is now
	       ;; changing, issue a warning.  A previous binding can only be
	       ;; explicitly determined if there are previous declarations in
	       ;; effect.  -jallard 11/21/99
	       (when (and (cons-second local-binding) 
			  (not (eq binding-type (cons-second local-binding)))
			  (or (cons-third local-binding)
			      (cons-fourth local-binding)))
		 (translation-warning 
		  "Changing ~s from a ~s to a ~s."
		  func-name (cons-second local-binding) binding-type))
	       (setf (second local-binding) binding-type))
	     (setf (car bindings-cons)
		   (if (eq env global-tlt-environment)
		       (add-to-alist-without-duplication
			 key value (cons-car bindings-cons))
		       (cons (cons key value) (cons-car bindings-cons))))))
	  ((:declare)
	   (push decl-info declaration-env)))))
    (values variable-env function-env declaration-env)))






;;;; Environment Accessors




;;; The function `tl:variable-information' takes a symbol and an environment and
;;; returns three values, binding-type (nil, :special, :lexical, :symbol-macro,
;;; or :constant), local-binding (t, nil, or a symbol-macro expansion), and a
;;; declaration alist.  See CLtL 2, Sec. 8.5, p. 208 for details, but note that
;;; the symbol-macro expansion in the local-binding return value is a TL
;;; specific characteristic.

(defun tl:variable-information (symbol &optional env)
  (let ((entry? (search-variable-environment
		  symbol
		  (env-or-default env))))
    (cond (entry?
	   (values-list entry?))
	  ((or (null symbol) (keywordp symbol) (eq symbol 't))
	   (values :constant nil nil))
	  (t
	   (values nil nil nil)))))




;;; The function `variable-decl' takes a symbol naming a variable, a symbol
;;; naming a key in the declarations for that symbol and an optional environment
;;; argument.  This function will look up the variable information for the given
;;; variable in the given environment, then search for and return the value
;;; associated with the given declaration key for that variable.

(defun variable-decl (symbol key &optional env)
  (multiple-value-bind (type? local? decls)
      (tl:variable-information symbol env)
    (declare (ignore type? local?))
    (cdr (assq key decls))))




;;; The function `function-information' takes a symbol and an environment and
;;; returns up to four values; function-type (nil, :function, :macro, or
;;; :special-form), local-binding (t or nil), a declaration alist, and a either
;;; the name of a local function or the expansion function for a local macro.
;;; This fourth value is TL specific and not part of CLtL 2.  See CLtL 2,
;;; Sec. 8.5, p. 209 for details.

(defun tl:function-information (symbol &optional env)
  (let ((entry? (search-function-environment
		  symbol
		  (env-or-default env))))
    (if entry?
	(values-list entry?)
	(values
	  (cond ((not (fboundp symbol)) nil)
		((macro-function symbol) :macro)
		(t :function))
	  nil nil))))




;;; The function `function-decl' takes a symbol naming a function, a symbol
;;; naming a key in the declarations for that symbol and an optional environment
;;; argument.  This function will look up the function information for the given
;;; function in the given environment, then search for and return the value
;;; associated with the given declaration key for that variable.

(defun function-decl (symbol key &optional env)
  (multiple-value-bind (type? local? decls)
      (tl:function-information symbol env)
    (declare (ignore type? local?))
    (cdr (assq key decls))))




;;; The functiion `declaration-information' takes a symbol naming a declaration
;;; type and an optional environment, and returns a single value describing the
;;; state of that declaration within the given environment.  See CLtL 2,
;;; Sec. 8.5, pp. 210-211 for details.

(defun tl:declaration-information (symbol &optional env)
  (search-declaration-environment symbol (env-or-default env)))




;;; The function `tl:optimize-information' is just like declaration-information,
;;; but it fetches the different optimization parameters out of the optimize
;;; declaration.  Those parameters are speed, safety, size, and
;;; compilation-speed.

(defun tl:optimize-information (symbol &optional env)
  (second (assq symbol (tl:declaration-information 'optimize env))))




;;; The function `map-over-declarations' takes three arguments: a function of
;;; two arguments, one of the keywords :function or :variable, and a declaration
;;; name.  This function will map over the contents of the global environment
;;; and call the given function once for each symbol having that declaration,
;;; passing the symbol and the value of the declartion.

(defun map-over-declarations (function decls-space declaration-name)
  (maphash
    #'(lambda (symbol env-entry)
	(let ((decl-entry (assq declaration-name (third env-entry))))
	  (when decl-entry
	    (funcall function symbol (cdr decl-entry)))))
    (cond ((eq decls-space :function)
	   (environment-local-function-env global-tlt-environment))
	  ((eq decls-space :variable)
	   (environment-local-variable-env global-tlt-environment))
	  (t
	   (error
	     "Decl-space should have been one of :variable or :function, was ~s"
	     decls-space)))))
