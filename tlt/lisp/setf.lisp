(in-package "TLI")

;;;; Module SETF

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






;;;; Generalized Variable Setting




;;; This module implements setf and its setter function defining buddies.  Note
;;; that we have departed from CLtL II here in that setter functions are not
;;; named using a list whose car is setf.  In fact, there is no concept of
;;; setter functions here.  What we do have is a way of associating an operation
;;; with a setter-operation (the simple form of defsetf), a way of associating a
;;; setf-method with an operation (the complex form of defsetf), and we have
;;; location mutating macros that perform macro expansions of these defined
;;; getter-to-setter relationships.  This is the optimization that most Lisp
;;; implementations provide anyway, we are only declining to add the function
;;; interface to this batch of information.

;;; The macro `tl:defsetf' associates a

(defmacro simple-setf-update-fn (access-fn)
  `(get ,access-fn :simple-setf-update-fn))

(defmacro setf-method (access-fn)
  `(get ,access-fn :setf-method))

(defmacro tl:define-modify-macro (name lambda-list function)
  (let* ((env (gensym))
	 (access-function-returning-form
	   `(append
	      '(,function)
	      (list place)
	      ,@(loop with within-keyword-args? = nil
		      for arg-cons = lambda-list then (cdr arg-cons)
		      for arg = (car arg-cons)
		      for arg-symbol = (if (consp arg) (cons-car arg) arg)
		      while arg-cons
		      nconcing
		      (cond ((eq arg '&rest)
			     (setq arg-cons (cdr arg-cons))
			     (list (car arg-cons)))
			    ((eq arg '&optional)
			     nil)
			    ((eq arg '&key)
			     (setq within-keyword-args? t)
			     nil)
			    (within-keyword-args?
			     (list `(list
				      ,(intern (symbol-name arg-symbol)
					       *keyword-package*))
				   `(list ,arg-symbol)))
			    (t
			     (list `(list ,arg-symbol))))))))
    `(tl:defmacro ,name ,(append `(&environment ,env place) lambda-list)
       (cond
	 ((and (symbolp place)
	       (not (eq (tl:variable-information place ,env)
			:symbol-macro)))
	  `(tl:setf ,place ,,access-function-returning-form))
	 ((and (consp place)
	       (eq (cons-car place) 'tl:the))
	  (multiple-value-bind (vars vals stores store-form access-form)
	      (tl:get-setf-expansion (cons-third place) ,env)
	    `(tl:let* ,(loop for var in (append vars stores)
			     for value
				 in (append
				      vals
				      (let* ((type (cons-second place))
					     (place `(tl:the ,type ,access-form)))
					`((tl:the
					    ,type
					    ,,access-function-returning-form))))
			     collect `(,var ,value))
	       (tl:declare (tl:type ,(cons-second place) ,(car stores)))
	       ,store-form)))
	 (t
	  (multiple-value-bind (vars vals stores store-form access-form)
	      (tl:get-setf-expansion place ,env)
	    `(tl:let* ,(loop for var in (append vars stores)
			     for value
				 in (append
				      vals
				      (let ((place access-form))
					`(,,access-function-returning-form)))
			     collect `(,var ,value))
	       ,store-form)))))))

(def-tl-macro tl:define-setf-method (name lambda-list &body body)
  (when (eval-feature :translator)
    (return-from tl:define-setf-method nil))
  (let ((setf-method-name (intern (format nil "~a-SETF-METHOD" name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,setf-method-name
	     ,@(cons-cdr (tl:parse-macro name lambda-list body)))
	 (setf (setf-method ',name)
	       (function ,setf-method-name)))
       ',name)))


(def-tl-macro tl:defsetf (access-fn update-fn-or-lambda-list
				    &rest store-var-and-body)
  (when (eval-feature :translator)
    (return-from tl:defsetf nil))
  `(tl:eval-when (:compile-toplevel :load-toplevel :execute)
     ,(cond
	((null store-var-and-body)
	 `(setf (simple-setf-update-fn ',access-fn)
		',update-fn-or-lambda-list))
	(t
	 (let ((store-var (cons-caar store-var-and-body))
	       (body (cons-cdr store-var-and-body))
	       (arg-vars (loop for arg-cons on update-fn-or-lambda-list
			       for arg = (cons-car arg-cons)
			       do
			   (when (eq arg '&environment)
			     (setq arg-cons (cons-cddr arg-cons))
			     (setq arg (cons-car arg-cons)))
			       unless (memqp arg '(&optional &rest &key))
				 collect (if (consp arg) (car arg) arg))))
	   `(tl:define-setf-method ,access-fn ,update-fn-or-lambda-list
	      (let ((temps (list ,@(loop repeat (length arg-vars)
					 collect '(gensym))))
		    (value-var (gensym)))
		(values temps
			(list ,@arg-vars)
			(list value-var)
			(tl:destructuring-bind-strict
			  ,(cons store-var arg-vars)
			  (cons value-var temps)
			  ,@body)
			`(,',access-fn ,@temps)))))))))


(def-tl-macro tl:setf (&environment env &rest places-and-values)
  (cond
    ((null places-and-values)
     nil)
    ((/= (length places-and-values) 2)
     `(tl:progn
	,@(loop for pair-cons on places-and-values by #'cddr
		for place = (cons-car pair-cons)
		for value = (if (consp (cons-cdr pair-cons))
				(cons-second pair-cons)
				(error "Uneven number of arguments to setf ~s"
				       places-and-values))
		collect `(tl:setf ,place ,value))))
    (t
     (let ((place (cons-car places-and-values))
	   (value (cons-second places-and-values)))
       (cond
	 ((symbolp place)
	  (multiple-value-bind (binding-type local-binding)
	      (tl:variable-information place env)
	    (cond ((not (eq binding-type :symbol-macro))
		   `(tl:setq ,place ,value))
		  (t
		   `(tl:setf ,local-binding ,value)))))
	 ((or (not (consp place)) (not (symbolp (cons-car place))))
	  (error "SETF place ~s was not a valid form." place))
	 ((eq (cons-car place) 'tl:the)
	  `(tl:setf ,(cons-third place) (tl:the ,(cons-second place) ,value)))
	 (t
	  (let* ((op (cons-car place))
		 (simple-update? (simple-setf-update-fn op))
		 (setf-method? (if (null simple-update?) (setf-method op))))
	    (cond
	      (simple-update?
	       `(,simple-update? ,@(cons-cdr place) ,value))
	      (setf-method?
	       (multiple-value-bind (vars vals stores store-form)
		   (funcall setf-method? place env)
		 `(tl:let* ,(loop for var in (append vars stores)
				  for value in (append vals (list value))
				  collect (list var value))
		    ,store-form)))
	      (t
	       (multiple-value-bind (new-place expanded?)
		   (tl:macroexpand-1 place env)
		 (cond
		   (expanded?
		     `(tl:setf ,new-place ,value))
		   ((not (eval-feature :translator))
		    ;; Give the underlying Lisp level a shot at setfing this form.
		    `(setf ,place ,value))
		   (t
		    (error "SETF cannot find a setter for ~s" place)))))))))))))




;;; See setf and get-setf-expansion documentation in CLtL2, p. 140.

(defun tl:get-setf-method (place &optional env)
  (tl:get-setf-expansion place env))

(defun tl:get-setf-expansion (place &optional env)
  (cond
    ((symbolp place)
     (multiple-value-bind (binding-type local-binding)
	 (tl:variable-information place env)
       (cond ((eq binding-type :symbol-macro)
	      (tl:get-setf-expansion local-binding env))
	     (t
	      (let ((new-value (gensym)))
		(values nil nil (list new-value)
			`(tl:setq ,place ,new-value)
			place))))))
    ((or (not (consp place)) (not (symbolp (cons-car place))))
     (error "GET-SETF-EXPANSION place ~s was not a valid form." place))
    (t
     (let* ((op (cons-car place))
	    (simple-update? (simple-setf-update-fn op))
	    (setf-method? (if (null simple-update?) (setf-method op))))
       (cond
	 (simple-update?
	  (let ((temporaries (loop repeat (length (cons-cdr place))
				   collect (gensym)))
		(store-var (gensym)))
	    (values
	      temporaries
	      (cons-cdr place)
	      (list store-var)
	      `(,simple-update? ,@temporaries ,store-var)
	      `(,op ,@temporaries))))
	 (setf-method?
	  (funcall setf-method? place env))
	 (t
	  (multiple-value-bind (new-place expanded?)
	      (tl:macroexpand-1 place env)
	    (cond
	      (expanded?
	       (tl:get-setf-expansion new-place env))
	      ((not (eval-feature :translator))
	       ;; When we are not translating, let the underlying Lisp
	       ;; implementation have a shot at the expansion.
	       (get-setf-expansion place))
	      (t
	       (error "GET-SETF-EXPANSION cannot find a setter for ~s" place))))))))))

