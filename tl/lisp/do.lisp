(in-package "TL")

;;;; Module DO

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1996 Gensym Corporation.
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






;;;; Simple Looping




;;; This module implements the simple looping macros for TL.  Some of this code
;;; has been pulled from CMULISP.

;;; Note that in TL:DOTIMES, the termination count must always be a fixnum.

(defmacro dotimes ((var count &optional (result nil)) &body body)
  (multiple-value-bind (decls forms)
      (split-declarations-and-body body) 
    (cond
      ((fixnump count)
       `(let ((,var 0))
	  (declare (type fixnum ,var))
	  ,@decls
	  (block nil
	    (tli::for-loop
	      (nil (< ,var ,count) (setq ,var (+ ,var 1)))
	      ,@forms)
	    ,result)))
      (t
       (let ((end-value (gensym)))
	 `(let ((,end-value ,count)
		(,var 0))
	    (declare (type fixnum ,end-value ,var))
	    ,@decls
	    (block nil
	      (tli::for-loop
		(nil (< ,var ,end-value) (setq ,var (+ ,var 1)))
		,@forms)
	      ,result)))))))




;;; We repeatedly bind the var instead of setting it so that we never give the
;;; var a random value such as NIL (which might conflict with a declaration).
;;; If there is a result form, we introduce a gratitous binding of the variable
;;; to NIL w/o the declarations, then evaluate the result form in that
;;; environment.  We spuriously reference the gratuitous variable, since we
;;; don't want to use IGNORABLE on what might be a special var.

(defmacro dolist ((var list &optional (result nil)) &body body)
  (let ((the-list (gensym)))
    `(let ((,the-list ,list))
       (block nil
	 (tli::for-loop
	   (nil ,the-list (setq ,the-list (cdr (the cons ,the-list))))
	   (let ((,var (car (the cons ,the-list))))
	     ,@body))
	 ,(if result
	      `(let ((,var nil))
		 ,var
		 ,result)
	      nil)))))

(defun-for-macro do-do-body (varlist endlist code decl bind step name block)
  (let* ((inits ())
	 (steps ())
	 (endtest (car endlist)))
    ;; Check for illegal old-style do.
    (when (or (not (listp varlist)) (atom endlist))
      (lisp:error "Ill-formed ~S -- possibly illegal old style DO?" name))
    ;; Parse the varlist to get inits and steps.
    (dolist (v varlist)
      (cond ((symbolp v) (push v inits))
	    ((listp v)
	     (unless (symbolp (first v))
	       (lisp:error "~S step variable is not a symbol: ~S" name (first v)))
	     (case (tli::length-trans v)
	       (1 (push (first v) inits))
	       (2 (push v inits))
	       (3 (push (list (first v) (second v)) inits)
		  (setq steps (list* (third v) (first v) steps)))
	       (t (lisp:error "~S is an illegal form for a ~S varlist." v name))))
	    (t (lisp:error "~S is an illegal form for a ~S varlist." v name))))
    ;; And finally construct the new form.
    `(block ,block
       (,bind ,(nreverse inits)
         ,@decl
	 (tagbody
	  next-loop
	   ,@(when endtest `((when ,endtest (go end-loop))))
	   ,@code
	   ,(if (cddr steps)
		`(,step ,@(nreverse steps))
	      `(setq ,@(nreverse steps)))
	   (go next-loop)
	   ,@(when endtest '(end-loop))
	   (return-from ,block (progn ,@(cdr endlist))))))))




;;; DO ({(Var [Init] [Step])}*) (Exit-Test Exit-Form*) Declaration* Form*
;;; Iteration construct.  Each Var is initialized in parallel to the value of
;;; the specified Init form.  On subsequent iterations, the Vars are assigned
;;; the value of the Step form (if any) in paralell.  The Test is evaluated
;;; before each evaluation of the body Forms.  When the Test is true, the the
;;; Exit-Forms are evaluated as a PROGN, with the result being the value of the
;;; DO.  A block named NIL is established around the entire expansion, allowing
;;; RETURN to be used as an laternate exit mechanism."

(defmacro do (varlist endlist &body decls-and-forms)
  (multiple-value-bind (decls body)
      (split-declarations-and-body decls-and-forms)
    (do-do-body varlist endlist body decls 'let 'psetq 'do nil)))




;;; DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*

;;; Iteration construct.  Each Var is initialized sequentially (like LET*) to the
;;; value of the specified Init form.  On subsequent iterations, the Vars are
;;; sequentially assigned the value of the Step form (if any).  The Test is
;;; evaluated before each evaluation of the body Forms.  When the Test is true,
;;; the the Exit-Forms are evaluated as a PROGN, with the result being the value
;;; of the DO.  A block named NIL is established around the entire expansion,
;;; allowing RETURN to be used as an laternate exit mechanism."

(defmacro do* (varlist endlist &body decls-and-forms)
  (multiple-value-bind (decls body)
      (split-declarations-and-body decls-and-forms)
    (do-do-body varlist endlist body decls 'let* 'setq 'do* nil)))
