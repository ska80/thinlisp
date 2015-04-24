(in-package "TLI")

;;;; Module DEFUN

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






;;;; Function Definitions




;;; This module implements the macros `tl:defun' and `tl:def-c-translation'.  These
;;; define translatable functions.

;;; The function `choose-variable-type-given-walked-env' is used to determine
;;; the Lisp type that should be chosen for lexical variables.  If a type has
;;; been explicitly declared for the variable, then it uses that.  However, if
;;; no type has been declared, it then looks for type information that can be
;;; determined from the uses of the variable during the walk over the code
;;; within the scope of this variable binding.  For example, if a variable is
;;; always used in locations that require simple-vectors, then we can infer that
;;; the variable should be given the type simple-vector.  If neither a type
;;; declaration nor clear usage pattern emerges, then the default type T is
;;; used.

(defun choose-variable-type-given-walked-env (variable inner-env)
  (multiple-value-bind (type? local? decls)
      (tl:variable-information variable inner-env)
    (declare (ignore local?))
    (let* ((declared-type? (cdr (assq 'type decls)))
	   (special-variable? (eq type? :special))
	   (binding (cdr (assq 'variable-binding-structure decls)))
	   (get-type? (variable-binding-getter-type binding))
	   (set-type? (variable-binding-setter-type binding))
	   (lisp-type
	     (or declared-type?
		 (if special-variable?
		     t)
		 (if (and get-type? set-type?)
		     (if (and (not (eq get-type? t))
			      (tl-subtypep set-type? get-type?))
			 set-type?
			 (most-specific-common-supertype get-type? set-type?))
		     (or get-type? set-type?))
		 t)))
      ;; (format t "~%Var = ~s Decl = ~a Get = ~a Set = ~a Lisp = ~a~%"
      ;;   variable declared-type? get-type? set-type? lisp-type)
      ;; Don't allow chosen types of NULL.  Generally this happens because of a
      ;; default initialization of NIL, but no more specific setter or getter
      ;; type can be found.  When the type is chosen as NULL, put it back to T.
      (cond ((eq lisp-type 'null)
	     (setq lisp-type t))
	    ((eq lisp-type 'void)
	     (translation-error "Varible ~s had void as the chosen type."
				variable)))
      ;; (format t "~%Var = ~s Decl = ~a Get = ~a Set = ~a Lisp = ~a and end~%"
      ;;   variable declared-type? get-type? set-type? lisp-type)
      (values
	lisp-type
	(if special-variable?
	    'obj
	    (c-type-for-lisp-type lisp-type))))))

;; When choosing the C type, it will generally pick the optimized type for the
;; determined Lisp type, with the exception that if the variable is special,
;; then it always uses type Obj.  We had wanted another exception, but it isn't
;; working out.  If there is a determined getter type, and if that type is not a
;; subtype of the chosen Lisp type, then we tried choosing Obj as well.  This
;; case is intended to avoid the pathological problem of a variable being type
;; declared to force a non-obj storage type, but then used repeatedly requiring
;; type Obj.  However, since all numeric operations require number, not
;; necessarily fixnum or float, then we lost all numeric storage
;; optimizations. -jallard 7/24/97




;;; The function `choose-variable-type-from-declarations' is like
;;; choose-variable-type-given-walked-env, but it only used the actual type
;;; declarations for varaibles, rather than allowing inference to tighten the
;;; types beyond declarations.  This is used by multiple-value-bind, where
;;; aggressive type tightening was failing due to analysis troubles deep within
;;; values forms.

(defun choose-variable-type-from-declarations (variable inner-env)
  (multiple-value-bind (type? local? decls)
      (tl:variable-information variable inner-env)
    (declare (ignore local?))
    (let* ((declared-type? (cdr (assq 'type decls)))
	   (lisp-type (or declared-type? t))
	   (special-variable? (eq type? :special)))
      (when (eq lisp-type 'void)
	(translation-error "Variable ~s had void as the declared type." variable))
      (values
	lisp-type
	(if special-variable?
	    'obj
	    (c-type-for-lisp-type lisp-type))))))



;;; The function `collect-computed-ftypes' determines the ftype of the function
;;; just walked by defun.  It is a subfunction of the defun macro.

(defun collect-computed-ftypes (form env inner-env required-type)
  (declare (ignore env required-type))
  (cond
    ((and (consp form)
	  (eq (car form) 'named-lambda)
	  (boundp 'local-functions-queue))
     (tl:destructuring-bind-strict (nil name lambda-list . doc-decls-body) form
      (push
	`(tl:declaim
	       (,(if (tl:declaration-information 'tl:return-type inner-env)
		     'ftype
		     'computed-ftype)
		 (function
		   ,(loop for lambda-elt in lambda-list
			  collect
			  (if (eq lambda-elt '&optional)
			      lambda-elt
			      (let ((arg-var
				      (cond ((symbolp lambda-elt)
					     lambda-elt)
					    ((symbolp (cons-car lambda-elt))
					     (cons-car lambda-elt))
					    (t
					     (cons-second (cons-car lambda-elt))))))
				(choose-variable-type-given-walked-env
				  arg-var inner-env))))
		   ,(or (tl:declaration-information 'tl:return-type inner-env)
			(expression-result-type (car (last doc-decls-body))
						inner-env)))
		 ,name)
	       ,@(when (memq '&optional lambda-list)
		   `((optional-arg-default-values
		       ,(loop for arg in (cdr (memq '&optional lambda-list))
			      collect (if (symbolp arg) nil (second arg)))
		       ,name)))
	       ,@(when (eq (tl:declaration-information 
			    'tl:consing-area inner-env)
			   'tl:either)
		   `((tl:conser ,name))))
	local-functions-queue)
      form))
    (t form)))




;;; The macro `tl:defun' defines functions.  It differs from CL defun in that
;;; some of the argument list features of CL are not available in TL.  In
;;; particular, TL does not directly support &key or &rest arguments in
;;; functions, but uses macros to provide those facilities.

;;; If a "function" is defined with &key or &rest args, then actually tl:defun
;;; will define a macro which performs keyword and rest argument changes at
;;; compile time, and then calling a function with the word "-FUNCTION"
;;; postpended to the original name.  The list generated by the &rest has
;;; dynamic extent and so should not be returned or incorporated into data
;;; structures whose lifetimes extend past the return of the function.

;;; When not translating, `tl:defun' expands into a declaim of the
;;; computed-ftype of the defined function and a definition of the function.
;;; The computed ftype can be overridden by an 
;;; explicit ftype declaration in the source code.  The computed-ftype is
;;; determined by type declarations and type optimizations of the argument
;;; variables, and by either a return-type declaration for this function or by
;;; type proofs about the value.  When we are translating, defun expands into a
;;; call to a function contained in a named lambda.

;;; When translating, the translate-file function will be walking the top level
;;; forms of the file, effectively macroexpanding the body of the function.
;;; When not translating, we are either in an explicit call to tl:walk for some
;;; debugging purpose, or we are in the first pass compile.  In these cases, the
;;; defun macro will fully macroexpand its body so that, for example, we can
;;; compute the return type of the function and so that we can explode out the
;;; local functions into top level defuns.

(def-tl-macro tl:defun (name lambda-list &body doc-decls-body)
  (when (memq '&aux lambda-list)
    (error "In ~s, DEFUN doesn't support &aux." name))
  (when (eq (symbol-package name) *lisp-package*)
    (error "Redefining ~s, a symbol in the Lisp package." name))
  (if (or (memq '&key lambda-list) (memq '&rest lambda-list))
      (let ((func-name (intern (format nil "~a-FUNCTION" name)))
	    (macro-lambda-list
	      (loop with keyword-args? = nil
		    for arg in lambda-list
		    collect (cond (keyword-args?
				   (cond ((and (consp arg) (second arg))
					  (list* (car arg)
						 (list 'quote (second arg))
						 (cddr arg)))
					 (t
					  (when (memq arg '(&optional &rest))
					    (setq keyword-args? nil))
					  arg)))
				  (t
				   (when (eq arg '&key)
				     (setq keyword-args? t))
				   arg))))
	    (rest-arg? (second (memq '&rest lambda-list)))
	    (func-lambda-list
	      (loop for arg in lambda-list
		    unless (memqp arg '(&optional &key &rest))
		      collect (if (consp arg)
				  (cons-car arg)
				  arg))))
	(multiple-value-bind (decls body)
	    (split-declarations-and-body doc-decls-body)
	  `(tl:progn
	     (tl:defmacro ,name ,macro-lambda-list
	       ,(if rest-arg?
		    `(list ',func-name
			   ,@(butlast func-lambda-list)
			   (cons 'list-dynamic-extent ,rest-arg?))
		    `(list ',func-name ,@func-lambda-list)))
	     ,@(when (eq (function-decl name 'inline) 'inline)
		 `((tl:declaim (inline ,func-name))))
	     ,@(when (function-decl name 'tl:conser)
		 `((tl:declaim (tl:conser ,func-name))))
	     (tl:defun ,func-name ,func-lambda-list
	       ,@decls
	       (tl:block ,name
		 ,@body)))))
      (let* ((local-functions-queue nil)
	     (translating? (memq :translator *features*))
	     (walked-named-lambda
	       (tl:walk
		 `(named-lambda ,name ,lambda-list ,@doc-decls-body)
		 nil
		 (if (not translating?) #'collect-computed-ftypes nil)
		 t)))
	`(tl:progn
	   ,@local-functions-queue
	   ,@(when (and (eq (function-decl name 'inline) 'inline)
			(not translating?))
	       (multiple-value-bind (decls body)
		   (split-declarations-and-body doc-decls-body)
		 (let ((var-list (loop for arg in lambda-list
				       unless (eq arg '&optional)
					 collect (if (consp arg)
						     (car arg)
						     arg))))
		   `((tl:define-compiler-macro ,name ,lambda-list
		       ,(cond
			  ;; Simplest inlining case.
			  ((and (null decls)
				(null lambda-list))
			   ``(tl:block ,',name ,@',body))
			  ;; If the body is a single form taking all of the
			  ;; arguments in order, don't bother to rebind them,
			  ;; just expand them inline.  Wrap the body with a THE
			  ;; form if there is a declared return type.
			  ((and (or (null decls)
				    (and (null (cdr decls))
					 (null (cddar decls))
					 (eq (caadar decls) 'tl:return-type)))
				(= (length body) 1)
				(or (and (consp (car body))
					 (symbolp (caar body))
					 (equal (cdar body) var-list))
				    (and (symbolp (car body))
					 (consp var-list)
					 (null (cons-cdr var-list))
					 (eq (car body) (car var-list)))))
			   (if (symbolp (car body))
			       ``(,@',(if decls
					  `(tl:the ,(cadr (cadar decls)))
					  '(tl:progn))
				    ,,(car body))
			       ``(,@',(if decls
					  `(tl:the ,(cadr (cadar decls)))
					  '(tl:progn))
				    (tl:block ,',name
				      (,',(caar body) ,,@var-list)))))
			  (t
			   (let ((declared-result-type?
				   (loop named type-search
					 for declare in decls do
				     (loop for decl in (cdr declare) do
				       (when (eq (car decl) 'tl:return-type)
					 (return-from type-search 
						      (second decl)))))))
			     ``(tl:let ,(list ,@(loop for var in var-list
						      collect (list 'list (list 'quote var) var)))
				 ,@',decls
				 (,@',(if declared-result-type?
					  `(tl:the ,declared-result-type?)
					  '(tl:progn))
				    (tl:block ,',name
				      ,@',body)))))))))))
	,walked-named-lambda))))






;;;; Translatable Functions




;;; The macro `def-c-translation' is used to define Lisp operations and their
;;; translations into C code.  This macro takes a Lisp macro of the operation to
;;; be used within calls to this operation in the Lisp environment, and it takes
;;; translation clauses giving the C argument and result types and a translation
;;; form for each translation.  Calls to this operation are translated by
;;; attempting to find the first listed operation where the C argument types and
;;; and result type are acceptible.  If no translation can be found, a second
;;; attempt is made but selecting operations where the types can be coerced to
;;; the required types for the given translation.  If neither approach finds an
;;; applicable translation function, an error is signalled during translation.

;;; The syntax I've adopted for this is kind of random, but the addition of the
;;; "specs" no-op tokens made the indenting and keyword handling work out.  -jra
;;; 1/11/96

;;; A translation defines the function type for the operation, a macro expansion
;;; for the operation to be used in non-translated compiles, and then several
;;; possible C translations, each with further information used to determine if
;;; the translation is applicable.  The syntax is as follows:

;;;   (def-c-translation <name> (<arg-name> ...)
;;;     ((lisp-specs :ftype ((<lisp-arg-type> ...) <lisp-result-type>))
;;;      <macro-expander-s-expression-body>)
;;;     ((trans-specs
;;;        :c-type ((<c-arg-type> ...) <c-result-type>)
;;;        :lisp-type ((<lisp-arg-type> ...) <lisp-result-type>)
;;;        :test <additional-applicability-predicate>)
;;;      <c-translation-s-expression-body>)
;;;     ... )

;;; <name> = the operation name, e.g. svref
;;; <arg-name> = the names of the arguments, used in the expander and
;;;   translation s-expressions

;;; The first form after the arglist defines the most general type specification
;;; for this operation and a macroexpander to be used in non-translation
;;; compiles.  The "lisp-specs" portion must contain an :ftype keyword argument.
;;; The remaining elements of this form are the s-experssions that will be
;;; expanded into a macro for this operation.  These forms may refer to the
;;; argument symbols given in the arglist.

;;; The translation forms follow.  There can be multiple possible translations
;;; for an operation.  The first list in a translation form contains the
;;; "trans-specs".  These are used to determine when this translation is
;;; applicable.  The :c-type keyword is required, the :lisp-type and :test
;;; keywords are optional.  When a translation is considered for use, it uses
;;; the Lisp type arguments (if present), then the C type arguments, then the
;;; additional test (if present) to determine if this translation is applicable.
;;; Note that the result Lisp and C types are not used to determine if a
;;; translation is applicable, but the result types are used to determine the
;;; result type of this operation for use by its caller.  Once a translation is
;;; determined to be applicable, then the body forms of the translation are
;;; evaluated.  They should return a C-expr that holds the value of this
;;; expression.  Within the translation forms, the argument name symbols will
;;; hold C-exprs already translated to ensure left-to-right evaluation.  The
;;; following variables are also available, but should be needed less often:
;;; <arg-name>-l-expr holds the l-expr structures for the argument expressions;
;;; function-call-l-expr holds the l-expr for this operation; c-func holds the C
;;; function being translated into; and c-compound-statement holds the
;;; c-compound-statement currently being emitted into.

;;; The expression in the test argument should only rarely be needed.  Within it
;;; the following variables are availabe: <arg-name>-l-expr holds the l-expr
;;; structure for each argument; function-call-l-expr holds the l-expr for this
;;; operation; lisp-type holds the rquired Lisp type for this operation; and
;;; c-type holds the required C type for this operation.  Note that type
;;; coercions will automatically be added for translated C results whose type
;;; differs from the required type.

;;; The following is an example translation for the fixnum portions of a two
;;; argument addition operator.

;;;   (def-c-translation +-two-arg (number1 number2)
;;;     ((lisp-specs :ftype ((number number) number))
;;;      `(lisp:+ ,number1 ,number2))
;;;     
;;;     ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
;;;   		:c-type ((sint32 sint32) sint32))
;;;      (make-c-infix-expr number1 "+" number2))
;;;     
;;;     ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
;;;   		:c-type ((sint32 obj) sint32)
;;;   		:test (l-expr-constant-p number1-l-expr))
;;;      (make-c-infix-expr
;;;        (make-c-literal-expr (ash (l-expr-constant-value number1-l-expr) 2))
;;;        "+"
;;;        number2)))

(defmacro def-c-translation (name function-arglist ftype-and-macro-decls
				  &rest specs-and-trans)
  (tl:destructuring-bind-strict (&key ftype)
    (cons-cdr (cons-car ftype-and-macro-decls))
    (unless ftype
      (error "~A: Def-c-translation requires :ftype and :macro args" name))
    (let* ((macro (cons-cdr ftype-and-macro-decls))
	   (chooser-name (intern (format nil "~a-CHOOSE-C-TRANS" name)))
	   (l-expr-args (loop for arg in function-arglist
			      collect (intern (format nil "~a-L-EXPR" arg))))
	   (translator-name (intern (format nil "~a-C-TRANS" name)))
	   (uses-tests? (loop for (spec) in specs-and-trans
			      thereis (memq :test spec)))
	   (uses-lisp-type? (loop for (spec) in specs-and-trans
				  thereis (memq :lisp-type spec)))
	   (home-system (or *current-system-name* 'tl-user::tl))
	   (home-module (or *current-module-name* 'tl-user::tlt))
	   (c-arg-types
	     (loop for lisp-type in (cons-car ftype)
		   collect (c-type-for-lisp-type lisp-type))))
			      
      `(progn
	 (tl:declaim (ftype ,(cons 'function ftype) ,name)
		     (c-translator
		       (,translator-name ,chooser-name
					 ,(not (null (cdr specs-and-trans))))
		       ,name)
		     ;; In the following line, lie about the home location of this 
		     (function-home (,home-system . ,home-module) ,name))
	 (lisp:defmacro ,name ,function-arglist ,@macro)
	 (defun ,translator-name
	     (function-call-l-expr c-expr-args c-func
				   c-compound-statement return-directive)
	   (tl:destructuring-bind-strict
	       ,l-expr-args
	     (cons-cdr (l-expr-form function-call-l-expr))
	     ;; Suppress unused variable warnings with the following.
	     ,@(loop for l-expr in l-expr-args collect `(identity ,l-expr))
	     (tl:destructuring-bind-strict
		 ,function-arglist
	       c-expr-args
	       (emit-c-expr-as-directed 
		 (case (the fixnum
			    (cons-car (function-call-l-expr-translator?
					function-call-l-expr)))
		   ,@(loop for index from 0
			   for spec-and-trans in specs-and-trans
			   collect `((,index) ,@(cons-cdr spec-and-trans)))
		   (t (error "Bad translator number!")))
		 function-call-l-expr c-func c-compound-statement
		 return-directive))))
	 (defun ,chooser-name (function-call-l-expr lisp-type c-type)
	   ,(unless uses-lisp-type?
	      (if uses-tests?
		  '(identity lisp-type)
		  '(declare (ignore lisp-type))))
	   (,@(if uses-tests?
		  `(tl:destructuring-bind-strict
		       ,l-expr-args
		     (cons-cdr (l-expr-form function-call-l-expr))
		     ,@(loop for arg in l-expr-args collect `(identity ,arg)))
		  '(progn))
	     (cond
	       ,@(loop for index from 0
		       for (spec) in specs-and-trans
		       collect
		       (tl:destructuring-bind-strict
			   (&key (lisp-type nil) (c-type nil) (test nil))
			 (cons-cdr spec)
			 `((and ,@(nconc
				    (if lisp-type
					`((l-expr-matches-lisp-type-p
					    ',(car lisp-type) ',(second lisp-type)
					    function-call-l-expr lisp-type))
					nil)
				    (if test `(,test) nil)
				    `((l-expr-matches-c-type-p
					',(car c-type)
					',(car (or lisp-type ftype))
					',(second c-type)
					function-call-l-expr c-type
					,(and (eq spec (caar specs-and-trans))
					      (or (null c-type)
						  (equal c-arg-types
							 (cons-car c-type))))))))
			   '(,index ,(or lisp-type ftype) ,c-type))))
	       (t nil))))))))

(defmacro chosen-translation-index (chosen-translation)
  `(cons-car ,chosen-translation))

(defmacro chosen-translation-lisp-type (chosen-translation)
  `(cons-second ,chosen-translation))

(defmacro chosen-translation-c-type (chosen-translation)
  `(cons-third ,chosen-translation))
