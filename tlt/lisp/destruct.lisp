(in-package "TLI")

;;;; Module DESTRUCT

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






;;;; Destructuring




;;; The macro `tl:destructuring-bind-strict' takes a (possibly dotted) cons tree
;;; of symbols, a value argument, and a body, and performs an efficient
;;; destructuring bind of the given symbols to the values in the corresponding
;;; positions of the list returned by the value argument.  If any of the symbols
;;; in the binding list are NIL, then that corresponding value will not be bound
;;; to any symbol.  Note that this is more efficient than evaluating setqs of
;;; the first, second, etc. of the value list, in that the spine conses of the
;;; list are only traversed once.

;;; This macro supports the lambda-list keywords &optional, &rest, &key, &aux,
;;; &body, and &whole.  Note that it supports &allow-other-keys, but only if
;;; that lambda-list keyword follows &keys.  The keyword argument
;;; :allow-other-keys is not supported.  This also supports the &environment
;;; lambda-list keyword, which it expands into a reference to tli::env.  The
;;; &environment argument is only intended for use by tl:parse-macro and other
;;; users may find that the details of implementation have changed at some point
;;; in the future.

;; See CLtL 2, Sec. 8.3, p. 204 for details.  Note that this version of
;; destructuring-bind-strict is strict about the given value matching the given
;; pattern.  This is in conformance with the standard, but is a change from
;; ThinLisp's previous destructuring bind, which supplied NILs for missing parts
;; of the pattern.  In tl/lisp/tl-extension.lisp there is a forgiving version of
;; tl:destructuring-bind that is exported for the interim.  When we can make
;; significant edits to the lisp directory, we will change the name of the
;; non-conforming destructuring-bind to destructuring-bind-forgiving and
;; re-adopt the Common Lisp version on the standard name.  -jra 1/10/97

;; Today's the day.  Tl:destructuring-bind has been changed to expand into
;; tl:destructuring-bind-strict.  -jallard 9/25/99.

(defvar destruct-action-queue nil)

(defvar destruct-binding-list nil)

(defconstant tl:lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment))

(defmacro let*-destruct (bindings &body body)
  `(,(if (eval-feature :translator) 'tl:let* 'let*) ,bindings ,@body))

(defmacro when-destruct (test &body body)
  `(,(if (eval-feature :translator) 'tl:when 'when) ,test ,@body))

(defmacro setq-destruct (&rest vars-and-values)
  `(,(if (eval-feature :translator) 'tl:setq 'setq) ,@vars-and-values))

(defmacro progn-destruct (&body body)
  `(,(if (eval-feature :translator) 'tl:progn 'progn) ,@body))

(defmacro cons-car-destruct (cons)
  `(,(if (eval-feature :translator) 'tl:car-of-cons 'cons-car) ,cons))

(defmacro cons-cdr-destruct (cons)
  `(,(if (eval-feature :translator) 'tl:cdr-of-cons 'cons-cdr) ,cons))

(defmacro tl:destructuring-bind-strict (pattern value &body decls-and-body)
  (let* ((destruct-action-queue nil)
	 (destruct-binding-list nil)
	 (cons-error-message
	   "Invalid pattern for destructuring-bind-strict, ~A where a cons was expected.")
	 (value-var (gensym)))
    (collect-destructure-bindings pattern value-var nil)
    (multiple-value-bind (decls body)
	(split-declarations-and-body decls-and-body)
      `(let*-destruct
	 ((,value-var ,value)
	  ,@(reverse destruct-binding-list))
	 ,@decls
	 ,@(loop with check-found? = nil
		 with forms = nil
		 for action in destruct-action-queue
		 do
	     (when (and (not check-found?)
			(eq (cons-car action) 'check))
	       (setq check-found? t))
	     (when check-found?
	       (push (cons-cdr action) forms))
		 finally
		   (return forms))
	 ,@body))))

(defun add-destruct-binding (var value)
  (push (list var
	      (if destruct-action-queue
		  `(progn-destruct
		     ,@(reverse (mapcar #'cdr destruct-action-queue))
		     ,value)
		  value))
	destruct-binding-list)
  (setq destruct-action-queue nil))

(defun add-destruct-action (form)
  (push (cons nil form) destruct-action-queue)
  nil)

(defun clear-destruct-actions ()
  (setq destruct-action-queue nil))

(defun add-destruct-check (form)
  (push (cons 'check form) destruct-action-queue)
  nil)

(defmacro not-null-destructuring-error (shoulda-been-nil)
  `(,(if (eval-feature :translator)
	 'tl::not-null-destructuring-error-1
	 'not-null-destructuring-error-1)
     ,shoulda-been-nil))

(defun not-null-destructuring-error-1 (shoulda-been-nil)
  (error "The extra value ~s ran off the end of a destructuring-bind-strict pattern."
	 shoulda-been-nil))

(defun single-var-lambda-key-p (key lambda-list)
  (loop with next-cons
	for sublist = lambda-list then (cdr next-cons)
	while (and (consp sublist)
		   (consp (setq next-cons (cons-cdr sublist))))
	do
    (when (eq (cons-car sublist) key)
      (return (cons-car next-cons)))))

(defun remove-lambda-key (key list)
  (setq list (copy-list list))
  (loop for trailer = nil then cons
      for cons on list
      do
    (when (eq (cons-car cons) key)
      (if trailer
	  (setf (cdr trailer) (cons-cddr cons))
	(setq list (cons-cddr cons)))
      (loop-finish)))
  list)

(defun collect-destructure-bindings (pattern value-var null-value-ok?)
  (cond
   ((symbolp pattern)
    (if (null pattern)
	(unless null-value-ok?
	  (add-destruct-check
	   `(when-destruct
	     ,value-var (not-null-destructuring-error ,value-var))))
      (add-destruct-binding pattern value-var)))
   ((atom pattern)
    nil)
   ((single-var-lambda-key-p '&whole pattern)
    (add-destruct-binding (single-var-lambda-key-p '&whole pattern) value-var)
    (collect-destructure-bindings
     (remove-lambda-key '&whole pattern) value-var t))
   ((single-var-lambda-key-p '&environment pattern)
    (add-destruct-binding (single-var-lambda-key-p '&environment pattern) 'env)
    (collect-destructure-bindings
     (remove-lambda-key '&environment pattern) value-var t))
   ((consp pattern)
    (etypecase (cons-car pattern)
      (symbol
       (case (cons-car pattern)
	 ((&optional)
	  (collect-optional-destructure-bindings
	   (cons-cdr pattern) value-var))
	 ((&rest &body)
	  (add-destruct-binding (cons-second pattern) value-var)
	  (when (cddr pattern)
	    (unless (memq (third pattern) tl:lambda-list-keywords)
	      (error "&rest may only be followed by other lambda-list keywords, not ~s"
		     (cddr pattern)))
	    (collect-destructure-bindings (cddr pattern) value-var t)))
	 ((&key)
	  (collect-keyword-destructure-bindings (cons-cdr pattern) value-var))
	 ((&aux)
	  (loop for binding in (cons-cdr pattern) do
		(cond ((symbolp binding)
		       (add-destruct-binding binding nil))
		      ((consp binding)
		       (add-destruct-binding
			(cons-car binding) (car (cons-cdr binding))))
		      (t
		       (error "Invalid &aux binding ~s" binding)))))
	 ((nil)
	  (add-destruct-action
	   `(setq-destruct ,value-var (cons-cdr-destruct ,value-var)))
	  (collect-destructure-bindings
	   (cons-cdr pattern) value-var null-value-ok?))
	 (otherwise
	  (add-destruct-binding
	   (cons-car pattern) `(cons-car-destruct ,value-var))
	  (add-destruct-action
	   `(setq-destruct ,value-var (cons-cdr-destruct ,value-var)))
	  (collect-destructure-bindings
	   (cons-cdr pattern) value-var null-value-ok?))))
      (cons
       (let ((new-value-var (gensym)))
	 (add-destruct-binding new-value-var `(cons-car-destruct ,value-var))
	 (collect-destructure-bindings (cons-car pattern) new-value-var nil)
	 (add-destruct-action
	  `(setq-destruct ,value-var (cons-cdr-destruct ,value-var)))
	 (collect-destructure-bindings
	  (cons-cdr pattern) value-var null-value-ok?)))))))

(defun collect-keyword-destructure-bindings (key-list value-var)
  (unless (memq '&allow-other-keys key-list)
    (add-destruct-action
      (let ((key-to-check (gensym))
	    (valid-keys
	      (loop for key in key-list
		    collect
		    (cond ((symbolp key)
			   (intern (symbol-name key) "KEYWORD"))
			  ((symbolp (cons-car key))
			   (intern (symbol-name (cons-car key)) "KEYWORD"))
			  (t (cons-car (cons-car key)))))))
	(if (eval-feature :translator)
	    `(tl:unless (tl:do ((,key-to-check ,value-var (tl:cddr ,key-to-check)))
			       ((tl:null ,key-to-check) nil)
			  (tl:when (tl:eq (tl:car-of-cons ,key-to-check) 
					  :allow-other-keys)
			    (return (tl:cadr ,key-to-check))))
	      (tl:do ((,key-to-check ,value-var (tl:cddr ,key-to-check)))
		     ((tl:null ,key-to-check)
		      nil)
	        (tl:unless (tl:memq (tl:car-of-cons ,key-to-check) ',valid-keys)
		  (tl:error
		   "The key ~A does not match any of the keyword arguments."
		   (tl:car-of-cons ,key-to-check)))))
	    `(unless (loop named allow-other-keys-keyword
			   for ,key-to-check on ,value-var by #'cddr do
		       (when (eq ,key-to-check :allow-other-keys)
			 (return-from allow-other-keys-keyword 
				      (cons-cadr ,key-to-check)))
		           finally (return-from allow-other-keys-keyword nil))
	       (loop for ,key-to-check on ,value-var by #'cddr do
		 (unless (memq (cons-car ,key-to-check) ',valid-keys)
		   (error "The key ~A does not match any of the keyword arguments."
			  (cons-car ,key-to-check)))))))))
  (loop with search-var = (gensym)
	for key in key-list
	do
    (cond ((eq key '&allow-other-keys)
	   nil)
	  ((symbolp key)
	   (add-destruct-binding
	     key
	     (if (eval-feature :translator)
		 `(tl:do ((,search-var ,value-var (tl:cddr ,search-var)))
			 ((tl:eq (tl:car-of-cons ,search-var)
				 ,(intern (symbol-name key) "KEYWORD"))
			  (tl:car-of-cons (tl:cdr-of-cons ,search-var))))
		 `(loop for ,search-var on ,value-var by #'cddr do
		    (when (eq (cons-car ,search-var)
			      ,(intern (symbol-name key) "KEYWORD"))
		      (return (cons-car (cons-cdr ,search-var))))))))
	  (t
	   (let* ((init (car (cons-cdr key)))
		  (fancy-key? (consp (cons-car key)))
		  (keyword
		    (if fancy-key?
			(cons-car (cons-car key))
			(intern (symbol-name (cons-car key)) "KEYWORD")))
		  (variable
		    (if fancy-key?
			(cons-car (cons-cdr (cons-car key)))
			(cons-car key)))
		  (supplied-p-var? (caddr key)))
	     (when supplied-p-var?
	       (add-destruct-binding supplied-p-var? nil))
	     (add-destruct-binding
	       variable
	       (if (eval-feature :translator)
		   `(tl:do ((,search-var ,value-var (tl:cddr ,search-var)))
			   ((tl:or (tl:null ,search-var)
				   (tl:eq (tl:car-of-cons ,search-var)
					  ',keyword))
			    (tl:if ,search-var
				   (tl:progn
				    ,@(when supplied-p-var?
					`((tl:setq ,supplied-p-var? t)))
				    (tl:car-of-cons (tl:cdr-of-cons ,search-var)))
				   ,init)))
		   `(loop for ,search-var on ,value-var by #'cddr do
		      (when (eq (cons-car ,search-var) ',keyword)
			,@(when supplied-p-var?
			    `((setq ,supplied-p-var? t)))
			(return (cons-car (cons-cdr ,search-var))))
			  finally (return ,init))))))))
  (clear-destruct-actions))

(defun collect-optional-destructure-bindings (opt-list value-var)
  (loop for opt-binding in opt-list
	for var = (if (symbolp opt-binding)
		      opt-binding
		      (cons-car opt-binding))
	for init = (if (consp opt-binding)
		       (cons-car (cons-cdr opt-binding))
		       nil)
	for init-p-var? = (if (consp opt-binding)
			     (third opt-binding)
			     nil)
	do
    (add-destruct-binding
      var
      (if (eval-feature :translator)
	  `(tl:if ,value-var (tl:car-of-cons ,value-var) ,init)
	  `(if ,value-var (cons-car ,value-var) ,init)))
    (when init-p-var?
      (add-destruct-binding
	init-p-var?
	(if (eval-feature :translator)
	    `(tl:not (tl:null ,value-var))
	    `(not (null ,value-var)))))
    (add-destruct-action
      (if (eval-feature :translator)
	  `(tl:setq ,value-var
		    (tl:if ,value-var (tl:cdr-of-cons ,value-var) nil))
	  `(setq ,value-var (if ,value-var (cons-cdr ,value-var) nil)))))
  (clear-destruct-actions))
