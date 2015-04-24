;; -*- Mode: Lisp; Package: AB; Base: 10; Syntax: Common-Lisp -*-
(in-package "TL")

;;;; Module TL-EXTENSION

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1990, 1991, 1992, 1997 Gensym Corporation.
;;; All Rights Reserved.

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>

;;; Author: Lowell B. Hawkinson
;;; Author: Jim Allard




;;;; Basic Facilities for Both G2 and TLT


(declare-forward-reference gsi-magic-number variable)

(declare-forward-reference c-exit function) ;PRIMITIVES
(declare-forward-reference tl-probe-file function) ;PATHNAMES






;;;; Concepts
 
 
;;; The macro `def-concept' expands to nothing in production code.  In
;;; development, it records NAME as a definition and makes it visible to etags
;;; in emacs.  The Body may include a documentation string which is also
;;; recorded.  Name-and-type is either a symbol, or a list of (name type).
;;; The default definition type is Concept.
 
(defmacro def-concept (name-and-type &body body)
  #-development (declare (ignore name-and-type body))
  #+development (let* ((name (if (atom name-and-type) name-and-type (car name-and-type)))
 		       (type (if (atom name-and-type) 'concept (cadr name-and-type)))
 		       (doc (if (stringp (car body))
 				(car body))))
 		  `(progn
		     ;; This just annoys you when a concept is defined in several ways.
		     ;;		     (lcl:record-source-file ',name ',type)
 		     ,@(when doc
 			 ;; It is CL that documentation is setf-able, but our
 			 ;; ab:documentation is not defined until SITE.
 			 #+Lucid
 			 `((setf (lisp:documentation ',name ',type) ',doc)))
 		     ',name)))
 
 
;; (def-concept concept
;;   "What a concept!")




;;; The macro `dwarn' is a development-only call to WARN.

(defmacro dwarn (format &rest args)
  #+development `(warn ,format ,@args)
  #-development (declare (ignore format args)))


;;; The macro `dmesg' is a development-only call to FORMAT.

(defmacro dmesg (format &rest args)
  #+development `(format t ,format ,@args)
  #-development (declare (ignore format args)))



;;; The macro `with-tracing' is a development-only wrapper for trace print
;;; statements.  The real macro, with-tracing-1, is in TLDEBUG.

(defmacro with-tracing ((kind &key block) &body body)
  #-development (declare (ignore kind block body))
  #+development `(with-tracing-1 (,kind :block ,block) ,@body))



;;;; Variables for Chestnut Debugging

;;; Incremental compiling in Chestnut does not allow the creation of global
;;; variables. Hence some spares for such use.

(defvar debug-1 nil)
(defvar debug-2 nil)
(defvar debug-3 nil)
(defvar debug-4 nil)
(defvar debug-5 nil)
(defvar debug-6 nil)
(defvar debug-7 nil)
(defvar debug-8 nil)
(defvar debug-9 nil)
(defvar debug-10 nil)
(defvar debug-11 nil)
(defvar debug-12 nil)






;;;; Built-In Packages




;;; `Lisp-package-1' is bound to the lisp package.  The lisp package
;;; contains all TL (most Common Lisp) "primitives": user-visible
;;; functions, special forms, macros, and global variables.

(defvar lisp-package-1 (find-package "LISP"))



;;; `Keyword-package-1' is bound to the keyword package, which contains
;;; all `keywords': symbols prefixed by : which evaluate to themselves.

(defvar keyword-package-1 (find-package "KEYWORD"))



;;; `Symbol-is-keyword-p' is true if symbol is a keyword.

(defmacro symbol-is-keyword-p (symbol)
  `(eq (symbol-package ,symbol) keyword-package-1))



;;; `Keyword-symbol-p' returns non-nil if the object it is given is a
;;; symbol and it is in the KEYWORD package and nil otherwise.

(defun-for-macro keyword-symbol-p (object)
  (and (symbolp object)
       (string= "KEYWORD" (package-name (symbol-package object)))))






;;;; Storage Allocation Primitives



;;; `Within-managed-object-scope' indicates if we are in the scope of
;;; the with-managed-objects wrapper.

(defvar within-managed-object-scope nil)

;; With-managed-objects is defined in PRIMITIVES.  -pto 03sep90





;;;   with-dynamic-creation {form}*  [macro]

;;; The macro `with-dynamic-creation' will cause any consing which occurs within
;;; the dynamic scope of this form to be created within a dynamic area of
;;; memory, i.e.  one that is elegible for garbage collection.  It returns all
;;; values returned for the last form in its body.  This form should be used
;;; around areas of the code which we do not yet have under consing control.

;;; On the Lisp machine this will lambda bind the default consing area to be the
;;; dynamic unmanaged area that is the default consing area for this system.  In
;;; Lucid 2.x, we will be wrapping the body with the lucid::with-dynamic-area
;;; macro.  In Lucid 3.0, we will be wrapping the body with
;;; lucid::with-normal-consing-area.



(defmacro with-dynamic-creation (&rest forms)
  (feature-case
    (:vanilla-areas
      `(progn
	 ,@forms))
    (:tl
      `(with-permanent-area ,@forms))
    (:translator
      (warn "This translator has no WITH-DYNAMIC-CREATION code.")
      `(progn ,@forms))))






;;;; Macro Definition Aids




;;; `split-declarations-and-documentation-from-statements' takes a list known
;;; here as a body and returns two lists.  One is the any doc or declarations
;;; found on the front of the list body.  For example
;;;  '("Documentation" (declare foo) "Result") -- becomes -->
;;;  ("Documentation" (DECLARE FOO))
;;;  ("Result")

(defun-for-macro split-declarations-and-documentation-from-statements (body)
  (multiple-value-bind (decls body docs)
      (split-declarations-and-body body)
    (values (append docs decls) body)))




;;; `Tl-make-symbol' is a compile time function similar to make-symbol that
;;; returns an uninterned symbol.  It differs in that the print name will be
;;; made unique.  Note that for gensyms produced during translations into C, the
;;; gensym function will not return unique print names.  The translator itself
;;; generates unique print names in a consistent way on translation.

(defun-for-macro tl-make-symbol (print-name-prefix)
  (let* ((prefix-string
	   (cond
	     ((stringp print-name-prefix) print-name-prefix)
	     ((symbolp print-name-prefix) (symbol-name print-name-prefix))
	     (t (cerror "Continue."
			"to-make-symbol should be called with a symbol or string.")
		"G"))))
    (prog1
      (gensym prefix-string)
      (gensym "G"))))






;;; `Def-substitution-macro' is like defun except that (1) it defines a macro,
;;; (2) the lambda list must be just a list of variables, and (3) it produces
;;; the macro expansion using expand-by-substituting-args-if-all-trivial (see
;;; below).

;;; `Def-substitution-macro-with-computed-body-form' is like
;;; def-substitution-macro except that the "body", after any declarations and
;;; documentation strings, must consist of a single form that will be evaluated
;;; to be the form to substitute into.

;;; `Substitution macros' are macros that work, in the "trivial" case where all
;;; args are atoms or quote forms, by substituting args for corresponding macro
;;; variables wherever they occur in a form, or else by constructing a let form
;;; that binds each of the macro variables to its corresponding arg.
;;; Therefore, for maximal efficiency, substitution macros should be invoked
;;; with only atomic and quoted args.  Substitution macros are typically
;;; defined by means of def-substitution-macro and
;;; def-substitution-macro-with-computed-body-form; see below.

;;; Expand-by-substituting-args-if-all-trivial produces a macro expansion as
;;; follows:  if all the args are trivial, it substitutes args for the
;;; corresponding macro variables wherever they occur in form; otherwise, it
;;; constructs a let form, with form as the body, that binds each of the
;;; macro-variables to its corresponding arg.  Substitution is pervasive in
;;; form except within quote forms.

;;; An argument is trivial if it is either an atom or a quote form.  The macro
;;; variables should not be &rest or &body variables (or the like).  Note that
;;; the macro-variables argument of expand-by-substituting-args-if-all-trivial
;;; is not evaluated.

;;;   `Legal-substitution-macro' is a utility function used by the substitution
;;;   macros.  It checks that the body is absolutly safe given our extremely
;;;   stupid code walker.  For example: (def-substitution-macro listify (list)
;;;   (list list)) will do (listify x) to expand to (x x).  To avoid this we
;;;   preclude any of the arguments appearing as the first symbol in a list.
;;;   That of course causes preempts a lot of useful constructs. "Safety first."

#+safer-substitution-macros
(defun-for-macro legal-substitution-macro-p (variables body)
  (loop for form in body
	always (legal-substitution-macro-p-1 variables form)))

#+safer-substitution-macros
(defun-for-macro legal-substitution-macro-p-1 (variables body)
  (typecase body
    (atom t)
    (cons
     (or (eql (car body) 'quote)
	 (if (member (car body) variables)
	     nil
	     (loop for tail on (rest body)
		   when (atom tail) do (return t)
		   unless (legal-substitution-macro-p-1 variables (car tail))
		     do (return nil)
		   finally (return t)))))))

(defmacro def-substitution-macro (name variables &body body)
  #+safer-substitution-macros
  (unless (legal-substitution-macro-p variables body)
    (warn "Illegal body (see legal-substitution-macro) in def-substitution-macro ~S"
	  name))
  (loop for rest-of-body on body
	while
	(or (if (not (atom (car rest-of-body)))
		(eq (caar rest-of-body) 'declare))
	    (stringp (car rest-of-body)))
	collect (car rest-of-body) into declarations-and-documentation
	finally
	  (return
	    `(progn
	      #+safer-substitution-macros
	      (defun-for-macro
		  ,(intern
		    (format
		     nil
		     "UNUSED-FUNCTION-TO-VALIDATE-SUBSTITUTION-MACRO-IS-FUNCTION-LIKE-FOR-~S"
		     name))
		  (,@variables)
		,@body)
	      ,@(unless (eval-feature :no-macros)
		  `((eval-when (compile load eval)
		      (setf (substitution-function-p ',name) t))))
	      (defmacro ,name ,variables
		,@declarations-and-documentation
		(expand-by-substituting-args-if-all-trivial
		 ,variables
		 ',(if (cdr rest-of-body)
		       `(progn . ,rest-of-body)
		       (car rest-of-body))))))))

#+obsolete
(defmacro def-substitution-macro-with-computed-body-form
    (name variables &body body)
  (loop for rest-of-body on body
	while
	  (or (if (not (atom (car rest-of-body)))
		  (eq (caar rest-of-body) 'declare))
	      (stringp (car rest-of-body)))
	collect (car rest-of-body) into declarations-and-documentation
	finally
	  (return
	    `(defmacro ,name ,variables
	       ,@declarations-and-documentation
	       (expand-by-substituting-args-if-all-trivial
		 ,variables
		 . ,rest-of-body)))))

;; The macro def-substitution-macro-with-computed-body-form is not actually
;; used, and so we'll consider discontinuing it.  For now, comment it out.
;; -jallard 2/4/97

(defmacro expand-by-substituting-args-if-all-trivial (macro-variables form)
  `(substitute-args-if-all-trivial
     ',macro-variables
     (list . ,macro-variables) ,form))
  
(defun-for-macro substitute-args-if-all-trivial (variables args form)
  (if (loop for arg in args
	    always (or (atom arg) (eq (car arg) 'quote)))
      (substitute-args form variables args nil)
      `(let ,(loop for variable in variables
		   as arg in args
		   collect `(,variable ,arg))
	 ,form)))
  
(defun-for-macro substitute-args (form-or-tail variables args tail-case?)
  (cond
    ((atom form-or-tail)
     (if (or tail-case? (not (symbolp form-or-tail)))
	 form-or-tail
	 (let ((arg-index? (position-list form-or-tail variables)))
	   (if arg-index?
	       (nth arg-index? args)
	       form-or-tail))))
    ((and (not tail-case?) (eq (car form-or-tail) 'quote))
     form-or-tail)
    (t (let ((result-for-car
	      (substitute-args (car form-or-tail) variables args nil))
	     (result-for-cdr
	      (substitute-args (cdr form-or-tail) variables args t)))
	 (if (and (eq result-for-car (car form-or-tail))
		  (eq result-for-cdr (cdr form-or-tail)))
	     form-or-tail
	     (cons result-for-car result-for-cdr))))))




;;; The macro `defun-keyword-macro' defines a macro which allows constant
;;; keyword arguments, and expands into a call to an internal function which
;;; takes only required arguments.

;;; **Note**: Default values for optional and keyword arguments may not depend
;;; on the values of earlier arguments.  Defaults are evaluated in the CALLER's
;;; not the callee's environment.  Supplied-p and alternate keyword names are
;;; NOT handled.

;;; **Note**: This is no longer necessary.  This functionality is now defaulted
;;; in TL.  -jallard 2/4/97

(defmacro defun-allowing-keywords (name lambda-list &body body)
  `(defun ,name ,lambda-list ,@body))

;; Chestnut should do (a better version of) this for us.  -fmw, 9/21/93

;; TL did.  -jallard 2/4/97


  
;;; The macro `defun-substitution-macro' expands to a defmacro which references
;;; a function (which has a different name) in development and to a
;;; def-substitution-macro for non-development compiles.

;;; Expands to a defmacro and a defun if :development is in *features*.  The
;;; defmacro expands to a function call to the defun, which is named differently
;;; (<name>-FOR-DEVELOPMENT) from the defmacro.  Otherwise it expands to a
;;; def-substitution-macro.  You should ensure that this macro/function is
;;; called only once, and that it is defined before it is called.

(defmacro defun-substitution-macro (name variables &body body)
  (cond
    ((eval-feature :development)
     (let* ((development-function-name
	      (intern
		(format nil "~a-FOR-DEVELOPMENT" name))))
       `(progn
	  ,@(unless (eval-feature :no-macros)
	      `((eval-when (compile load eval)
		  (setf (substitution-function-p ',name) t))))
	  (defmacro ,name ,variables
	    (list ',development-function-name ,@variables))
	  (defun ,development-function-name ,variables
	    ,@body))))
    (t
     `(def-substitution-macro ,name ,variables
	,@body))))



;;; The macro `defconstant-for-macro' expands to a defparameter in
;;; development compiles, defconstant in macro compiles, and
;;; nil otherwise.

(defmacro defconstant-for-macro (variable initial-value)
  (cond
    ((eval-feature :development)
     `(eval-when (compile load eval)
	(defparameter ,variable ,initial-value)))
    ((not (eval-feature :no-macros))
     `(defconstant ,variable ,initial-value))
    (t nil)))

;; Changed defconstant-for-macro to place its binding in the compilation
;; environment even when it defines a parameter and not a constant.  jh,
;; 3/29/91.

;; Consider wrapping eval-when (compile load eval) around the defconstant
;; expansion in the second cond-clause too.  This would enable us to define
;; constants for macros which could be used in #.  contexts without the
;; ubiquitous and annoying eval-when (compile load eval) wrapper around each
;; group of such constants.  We won't do this now because I seem to recall that
;; Symbolics has a strange model for the defconstant compilation environment
;; which would make this not work.  Although Symbolics is not supported anymore,
;; there is a chance that some Lisp will come along with the same strange model,
;; so using an eval-when around the defconstant needs more thought.  jh,
;; 7/30/91.

;;; The macro `defmacro-for-constant' defines a defconstant-for-macro
;;; and a macro (both with the same name).  The macro expands to
;;; a reference to the constant/parameter during development and macro
;;; compiles (so its value is not hardwired into compiled code) and
;;; to the eval'ed value of the constant/parameter in distribution
;;; compiles.

(defmacro defmacro-for-constant (variable-and-macro-name initial-value)
  `(progn
     (eval-when (compile eval load)
       (defconstant-for-macro ,variable-and-macro-name ,initial-value))
     (defmacro ,variable-and-macro-name ()
       (if (eval-feature :no-macros)
	   ,(if (consp initial-value)
		`(list 'quote ,variable-and-macro-name)
		`,variable-and-macro-name)
	   ',variable-and-macro-name))))




;;; `Temporary-progn' is used to insert temporary code.  The macro's arguments
;;; are:

;;;   (expiration-month expiration-day expiration-year
;;;     &optional message-string)
;;;     &body body

;;; After this date the compiler on all platforms will start complaining that
;;; the form has "expired." The date and message string should be constants.

;; - ben 6/28/91

(defmacro temporary-progn
    ((expiration-month expiration-day expiration-year &optional message-string)
     &body body)
  (when (< (encode-universal-time
	     0 0 0 expiration-day expiration-month expiration-year) 
	   (get-universal-time))
    (if message-string
	(warn "Temporary-progn has expired, please remove. ~%~
               ~2tIt is labeled: ~S"
	      message-string)
	(warn "A temporary-progn has expired, please remove")))
  `(progn
     ,@body))



;;; `lambda-lists-conform-p' ...

(defun-for-macro lambda-lists-conform-p
		 (lambda-list-1 lambda-list-2)
  ;; jh, 9/12/91.  Eventually we should do a more elaborate, CLOS-like
  ;; conformity test here.
  (and (not (intersection lambda-list-1 lambda-list-keywords))
       (not (intersection lambda-list-2 lambda-list-keywords))
       (= (length lambda-list-1) (length lambda-list-2))))



;;; `Congruent-lambda-list-p' is used to check if a method's lambda list is well
;;; formed given the lambda list specified in a generic method declaration.
;;; This currently means that the lambda lists are the same length and make no
;;; use of lambda list keywords.  That is a subset of the definition of
;;; congurent used in CLOS.

(defun-for-macro congruent-lambda-list-p
    (specified-lambda-list implementation-lambda-list)
  (block this-predicate
    
    (unless (= (length specified-lambda-list)
	       (length implementation-lambda-list))
      (return-from this-predicate nil))
    
    (loop for specified-argument in specified-lambda-list 
	  as implemented-argument in implementation-lambda-list
	  do
      (cond
	((memq specified-argument lambda-list-keywords)
	 (warn "Congruent-lambda-list-p does not support lambda list keywords other than &optional")
	 (return-from this-predicate nil))
	((member implemented-argument lambda-list-keywords)
	 (warn "Congruent-lambda-list-p can not check lambda list keywords other ")
	 (return-from this-predicate nil))
	)
	  finally (return-from this-predicate t))))

;; The current definition of a congruent lambda list happens to additionally
;; ensure that the functions are "simple," i.e. that in chestnut you can use
;; funcall-simple-compile-function, which is significantly faster than funcall.
;; - ben Aug/25/93







;;;; Format-symbol




;;; Format-symbol takes a format control-string and args and returns the result
;;; as a symbol interned in the current package (the value *package*).  This
;;; function is primarily to be used in the development compile-time
;;; environment, especially for macro expansion.  Runtime uses of this function
;;; will leak.

(defmacro format-symbol (control-string &rest args)
  `(intern (format nil ,control-string ,@args)))






;;;; Type Declaration Macros




;;; The following macros are used to implement the fixnum arithmetic and
;;; floating point arithmetic type declared operations.

;;; The function `expand-type-preserving-numeric-operation' takes a type, an
;;; operation, and an argument list and will emit code with all arguments to
;;; the form and all intermediate results of the form type declared using the
;;; THE special form.  This macro is for declaring numeric operations like +.
;;; Note that it will not work for operations like =, whose intermediate
;;; results are not of the same type as the arguments.  Also note that this
;;; macro will signal an error if passed a null argument list, since it does
;;; not know what the identities are of the set of operations that may be
;;; passed in.  If there is one argument, it will type declare the argument and
;;; the result.  If there are two arguments, it will type declare those
;;; arguments, combine them with the operator, and declare the result.  With 3
;;; or more arguments, it will expand into a series of type declared calls to
;;; the operator, maintaining left to right argument evaluation and left to
;;; right combination, combining only two values at a time.  These combinations
;;; will not bottom out into any one argument calls to the operation.


(defun-for-macro declare-type-preserving-numeric-operation-1
                 (type operation args)
  (cond 
    ((null (cdr args))
     `(the ,type (,operation ,(declare-result type (first args)))))
    ((null (cddr args))
     `(the ,type (,operation
		    ,(declare-result type (first args))
		    ,(declare-result type (second args)))))
    (t
     `(the ,type
	   (,operation
	      ,(declare-type-preserving-numeric-operation-1
		 type operation (butlast args))
	      ,(declare-result type (car (last args))))))))

(defun-for-macro declare-type-preserving-numeric-operation
		 (type operation args &optional env)
  (declare (ignore env))
  (cond
    ((null args)
     (error "Numeric type wrapper called with no arguments."))
    ;; If the type is T, just split it up into binary operations.
    ((eq type t)
     (if (null (cddr args))
	 `(,operation ,@args)
	 `(,operation
	     ,(declare-type-preserving-numeric-operation
		type operation (butlast args))
	     ,(car (last args)))))
    (t
     (declare-type-preserving-numeric-operation-1 type operation args))))





;;; The functions `declare-argument-types', `declare-argument-and-result-types',
;;; and `declare-result' all takes a type and a form and perform the described
;;; type declarations on the form.

(defun-for-macro declare-argument-types (type form &optional env)
  (declare (ignore env))
  (cons (car form)
	(loop for argument in (cdr form)
	      collect (declare-result type argument))))

(defun-for-macro declare-argument-and-result-types (type form &optional env)
  (declare (ignore env))
  (let* ((operator (car form))
	 (args (cdr form)))
    `(the ,type
	  (,operator ,@(loop for arg in args
			     collect (declare-result type arg))))))

(defvar-for-macro fixnum-returning-operations
  '(+f -f *f minf maxf absf halff twicef logandf logandc2f logxorf
    logiorf ashf modf
    +w -w *w minw maxw absw
    +r -r *r minr maxr absr
    +i -i *i mini maxi absi))

(defvar-for-macro double-float-returning-operations
  '(1+e 1-e +e -e *e /e mine maxe abse loge atane cose expe expte sine
    sqrte tane))




;;; The function for macro `declare-result' wraps a "the type" form around the
;;; given form.  It does one optimization, that if the form is already declared,
;;; or if the form is one of a few known forms which declare the type, it will
;;; decline to wrap its form argument.  This is a compile time optimization to
;;; avoid redundant type wrappers.

(defun-for-macro declare-result (type form)
  (cond ((and (consp form)
	      (or (and (eq (car form) 'the)
		       (consp (cdr form))
		       (eq (cadr form) type))
		  (and (eq type 'fixnum)
		       (member (car form) fixnum-returning-operations))
		  (and (eq type 'double-float)
		       (member (car form) double-float-returning-operations))))
	 form)
	((and (consp form)
	      (eq (car form) 'setq)
	      (= (length form) 3))
	 `(setq ,(second form) (the ,type ,(third form))))
	(t (list 'the type form))))

;; jh, 3/12/93.  Due to a bug in Lucid, we must evert the variable from inside a
;; setq form handed to declare-result.  Otherwise, we get a strange warning
;; complaining that the variable FIXNUM is assumed special.  Here is a simple
;; example:
;;(defun unjustifiable-complaint (exponent most-righthand-float-zeros)
;;  (let (cond-temporary-1)
;;    (cond ((> (the fixnum (setq cond-temporary-1 exponent))
;;		most-righthand-float-zeros)
;;	     (values 'underflow nil))
;;	    ((> cond-temporary-1 12) 
;;           (values 'overflow t)))))

;;; The function for macro `fixnum-returning-expression-p' takes an expression
;;; and returns whether or not it can prove that the expression returns fixnums.

(defun-for-macro fixnum-returning-expression-p (expression)
  (or (and (constantp expression)
	   (typep (eval expression) 'fixnum))
      (and (consp expression)
	   (symbolp (car expression))
	   (or (member (car expression)
		       fixnum-returning-operations
		       :test #'eq)
	       (and (eq (car expression) 'the)
		    (consp (cdr expression))
		    (eq (second expression) 'fixnum))))))




;;; The function for macro `declare-comparitor' is used to do type declarations
;;; for numeric comparison operations.

(defun-for-macro declare-comparitor
		 (comparitor type arguments &optional env)
  (declare-argument-types type `(,comparitor ,@arguments) env))




;;; The function for macro `typed-operator-for-type' takes a symbol naming a
;;; Common Lisp numeric operation and a symbol naming a numeric type.  The
;;; result is a symbol naming a type declared version of that operator for the
;;; given type.  This currently can return optimized operators for fixnums and
;;; double-floats.

(defun-for-macro typed-operator-for-type (operator type)
  (cond ((eq type 'fixnum)
	 (fixnum-operator-for-operation operator))
	((eq type 'double-float)
	 (double-float-operator-for-operation operator))
	(t
	 operator)))






;;;; Fixnum Arithmetic




;;; The function for macro `fixnum-operator-for-operation' takes a symbol naming
;;; a generic Common Lisp numeric operator and returns a symbol naming a fixnum
;;; optimized version of the same operator.

(defparameter-for-macro fixnum-operator-alist
  `((1+ . 1+f)
    (1- . 1-f)
    (+ . +f)
    (- . -f)
    (* . *f)
    (= . =f)
    (/= . /=f)
    (< . <f)
    (> . >f)
    (<= . <=f)
    (>= . >=f)
    (minusp . minuspf)
    (plusp . pluspf)
    (min . minf)
    (max . maxf)
    (abs . absf)
    (floor . floorf)
    (ceiling . ceilingf)
    (truncate . truncatef)
    (round . roundf)
    (half . halff)
    (twice . twicef)
    (logand . logandf)
    (logandc2 . logandc2f)
    (logxor . logxorf)
    (logior . logiorf)
    (logbitp . logbitpf)
    (ash . ashf)
    (mod . modf)
    (incf . incff)
    (decf . decff)))
  
(defun-for-macro fixnum-operator-for-operation (operator)
  (or (cdr (assoc operator fixnum-operator-alist))
      operator))




;;; A `fixnum arithmetic suite' includes the set of functions 1+f, 1-f, +f, -f,
;;; *f, =f, /=f, <f, >f, <=f, >=f, minuspf, pluspf, minf, maxf, absf, ashf,
;;; modf, logandf, logand2cf, logxorf, logiorf, logbitpf, incff, and decff,
;;; which are like 1+, 1-, +, -, =, /=, <, >, <=, >=, minusp, plusp, min, max,
;;; abs, ash, mod, logand, logand2c, logxor, logior, logbitp, incf, and decf,
;;; except that they take fixnum arguments and, in the case of all but the
;;; comparisons, produce fixnum values.  Note that the arguments of +f and -f
;;; must be such as to allow them to have fixnum values.  Halff computes the
;;; floor of half of fixnum.  Twicef twice the fixnum argument.

;;; Each fixnum arithmetic suite has a "suffix".  The `standard fixnum
;;; arithmetic suite' has the suffix "F".  Def-fixnum-arithmetic-system, below,
;;; takes a suffix and defines the suite for that suffix.

;;; Def-fixnum-arithmetic-system defines the fixnum arithmetic suite with same
;;; given above, exactly as with the normal fixnum arithmetic system, but with a
;;; different suffix than "F".  Suffix-for-arithmetic-system, which is a quoted
;;; arg, should be a symbol or a string.  If a string is provided, the string is
;;; handled case sensitively.  If a symbol is provided, the arg is turned into
;;; the symbol's print name and then handled the same as if it were a string.

;;; Fixnum arithmetic may not be used within forms that are evaluated at read-time.
;;; Instead, use generic arithmetic.  For example, #.(+ x y) should be written
;;; instead of #.(+f x y).

(defmacro def-fixnum-arithmetic-system (suffix-for-arithmetic-system
					&key lower-bound upper-bound)
  `(progn
     ,@(loop for (op . fixnum-op) in fixnum-operator-alist
	     as rest-arglist-var = (format-symbol "ARGS-FOR-~a" op)
	     as system-op = (format-symbol "~a~a" op suffix-for-arithmetic-system)
	     as body = `(cons ',fixnum-op ,rest-arglist-var)

	     ;; This defines the nullary version of min and max for system. See the
	     ;; comment following "(def-fixnum-arithmetic-system w ...)" in primitives
	     ;; for motivation as to why this is natural.
	     do (cond ((and lower-bound (eq op 'max))
		       (setq body `(if (null ,rest-arglist-var) ',lower-bound ,body)))
		      ((and upper-bound (eq op 'min))
		       (setq body `(if (null ,rest-arglist-var) ',upper-bound ,body))))
	     collect
	       `(defmacro ,system-op (&rest ,rest-arglist-var)
		  ,body))))

(defmacro halff (fixnum)
  `(right-shiftf ,fixnum 1))

(defmacro twicef (fixnum)
  `(left-shiftf ,fixnum 1))

(defmacro 1+f (fixnum &environment env)
  (declare-argument-and-result-types 'fixnum `(1+ ,fixnum) env))

(defmacro 1-f (fixnum &environment env)
  (declare-argument-and-result-types 'fixnum `(1- ,fixnum) env))

(defmacro +f (&rest fixnums &environment env)
  (cond ((null fixnums)
	 0)
	((null (cdr fixnums))
	 (declare-result 'fixnum (car fixnums)))
	(t
	 (declare-type-preserving-numeric-operation 'fixnum '+ fixnums env))))



;;; The substitution macro `fixnum-vector-distance' takes two fixnums
;;; and returns the square root of the sum of the squares of the
;;; fixnums.

(def-substitution-macro fixnum-vector-distance (d1 d2)
  (the fixnum (isqrt (+f (*f d1 d1) (*f d2 d2)))))

(defmacro -f (fixnum &rest more-fixnums &environment env)
  (declare-type-preserving-numeric-operation
    'fixnum '- (cons fixnum more-fixnums) env))

(defmacro *f (&rest fixnums &environment env)
  (if (null fixnums)
      1
      (declare-type-preserving-numeric-operation 'fixnum '* fixnums env)))

(defmacro =f (&rest fixnums &environment env)
  (declare-argument-types 'fixnum (cons '= fixnums) env))

(defmacro /=f (&rest fixnums &environment env)
  (declare-argument-types 'fixnum (cons '/= fixnums) env))

(defmacro <f (&rest fixnums &environment env)
  (declare-argument-types 'fixnum (cons '< fixnums) env))

(defmacro >f (&rest fixnums &environment env)
  (declare-argument-types 'fixnum (cons '> fixnums) env))

(defmacro <=f (&rest fixnums &environment env)
  (declare-argument-types 'fixnum (cons '<= fixnums) env))

(defmacro >=f (&rest fixnums &environment env)
  (declare-argument-types 'fixnum (cons '>= fixnums) env))

(defmacro minuspf (fixnum &environment env)
  (declare-argument-types 'fixnum `(minusp ,fixnum) env))

(defmacro pluspf (fixnum &environment env)
  (declare-argument-types 'fixnum `(plusp ,fixnum) env))




;;; The first clause defines (minf) => most-positive-fixnum and (maxf) =>
;;; most-negative-fixnum. See the comment following
;;; "(def-fixnum-arithmetic-system w ...)" in primitives.lisp for motivation as
;;; to why this is natural.

(defmacro minf (&rest fixnums &environment env)
  (if (null fixnums)
      'most-positive-fixnum
      (declare-type-preserving-numeric-operation 'fixnum 'min fixnums env)))

(defmacro maxf (&rest fixnums &environment env)
  (if (null fixnums)
      'most-negative-fixnum
      (declare-type-preserving-numeric-operation 'fixnum 'max fixnums env)))




;;; `pin-in-rangef' does just that and is much more readable and dependable
;;; than the little tangle of max and min.

(defmacro pin-in-rangef (lower-bound x upper-bound)
  `(maxf ,lower-bound (minf ,x ,upper-bound)))


(defmacro absf (fixnum &environment env)
  (declare-argument-and-result-types 'fixnum `(abs ,fixnum) env))

(defmacro floorf-positive-2 (positive-fixnum positive-divisor-fixnum)
  (if (and (simple-argument-p positive-fixnum)
	   (simple-argument-p positive-divisor-fixnum))
      `(values (floorf-positive ,positive-fixnum ,positive-divisor-fixnum)
	       (modf-positive ,positive-fixnum ,positive-divisor-fixnum))
      (let ((f1 (gensym))
	    (f2 (gensym)))
	`(let ((,f1 ,positive-fixnum)
	       (,f2 ,positive-divisor-fixnum))
	   (declare (type fixnum ,f1 ,f2))
	   (values (floorf-positive ,f1 ,f2)
		   (modf-positive ,f1 ,f2))))))

(defmacro floorf (fixnum &optional divisor-fixnum &environment env)
  `(the (values fixnum fixnum)
	,(declare-argument-types
	   'fixnum
	   `(floor ,fixnum
		   ,@(if divisor-fixnum
			 `(,divisor-fixnum)
			 nil))
	   env)))

(defmacro ceilingf (fixnum &optional divisor-fixnum &environment env)
  `(the (values fixnum fixnum)
	,(declare-argument-types
	   'fixnum
	   `(ceiling ,fixnum
		     ,@(if divisor-fixnum
			   `(,divisor-fixnum)
			   nil))
	   env)))

(defmacro truncatef (fixnum &optional divisor-fixnum &environment env)
  `(the (values fixnum fixnum)
	,(declare-argument-types
	   'fixnum
	   `(truncate ,fixnum
		      ,@(if divisor-fixnum
			    `(,divisor-fixnum)
			    nil))
	   env)))

(defun truncatef-first (fixnum divisor-fixnum)
  (declare (type fixnum fixnum divisor-fixnum) )
;;  (eliminate-for-gsi))
  (if (pluspf fixnum)
      (if (pluspf divisor-fixnum)
	  (floorf-positive fixnum divisor-fixnum)
	  (-f (floorf-positive fixnum (-f divisor-fixnum))))
      (if (pluspf divisor-fixnum)
	  (-f (floorf-positive (-f fixnum) divisor-fixnum))
	  (floorf-positive (-f fixnum) (-f divisor-fixnum)))))

(defmacro roundf (fixnum &optional divisor-fixnum &environment env)
  `(the (values fixnum fixnum)
	,(declare-argument-types
	   'fixnum
	   `(round ,fixnum
		   ,@(if divisor-fixnum
			 `(,divisor-fixnum)
			 nil))
	   env)))


(defmacro logandf (&rest fixnums &environment env)
  (cond ((null fixnums)
	 -1)
	((null (cdr fixnums))
	 (declare-result 'fixnum (car fixnums)))
	;; The following line works around an IntelNT C compiler bug, where
	;; (IFIX(x) & 128L) > 0L will fail, due to some special handling
	;; afforded to the value 128.  We suspect that this problem occurs for
	;; all fixnums from 128 through 255, since the 7th bit is on in all
	;; these values, under the theory that this has something to do with the
	;; compiler trying to use a signed char type for the result.  This can
	;; be worked around shuffling the result of the bitwise and through an
	;; assignment statement, accomplished in the translator via the LET
	;; statement.  -jra 9/30/94
	((loop for fixnum-form in fixnums
	       thereis (and (fixnump fixnum-form)
			    (<= 128 fixnum-form 255)))
	 (let ((logand-result (gensym)))
	   `(let ((,logand-result
		     ,(declare-type-preserving-numeric-operation
			'fixnum 'logand fixnums env)))
	      (declare (type fixnum ,logand-result))
	      ,logand-result)))
	(t
	 (declare-type-preserving-numeric-operation
	   'fixnum 'logand fixnums env))))

(defmacro logandc1f (fixnum1 fixnum2 &environment env)
  (declare-argument-and-result-types 'fixnum `(logandc1 ,fixnum1 ,fixnum2) env))

(defmacro logandc2f (fixnum1 fixnum2 &environment env)
  (declare-argument-and-result-types 'fixnum `(logandc2 ,fixnum1 ,fixnum2) env))

(defmacro logxorf (&rest fixnums &environment env)
  (cond ((null fixnums)
	 0)
	((null (cdr fixnums))
	 (declare-result 'fixnum (first fixnums)))
	(t
	 (declare-type-preserving-numeric-operation 'fixnum 'logxor fixnums env))))

(defmacro logiorf (&rest fixnums &environment env)
  (cond ((null fixnums)
	 0)
	((null (cdr fixnums))
	 (declare-result 'fixnum (first fixnums)))
	(t
	 (declare-type-preserving-numeric-operation 'fixnum 'logior fixnums env))))

(defmacro lognotf (fixnum &environment env)
  (declare-argument-and-result-types 'fixnum `(lognot ,fixnum) env))

(defmacro ashf (fixnum n &environment env)
  (cond ((and (constantp n) (= (eval n) 0))
	 fixnum)
	((and (constantp fixnum) (constantp n))
	 (ash (eval fixnum) (eval n)))
	;; -jra & jh 7/21/93 Note that there are warnings in Harbison and Steele
	;; "C: A Reference Manual Edition 3", page 191 that say that the C >>
	;; operator (right shift) may or may not sign extend a negative left
	;; hand argument.  The assumption about right shift extending the sign
	;; bit is validated below in validate-fixnum-assumptions.
	((constantp n)
	 (let ((shift (eval n)))
	   `(,(if (minusp shift)
		    'right-shiftf
		    'left-shiftf)
	       ,fixnum ,(abs shift))))
	(t
	 (declare-argument-and-result-types 'fixnum `(ash ,fixnum ,n) env))))

(defmacro right-shiftf (fixnum non-negative-shift-distance)
  `(the fixnum (tli::fixnum-right-shift
		 (the fixnum ,fixnum)
		 (the fixnum ,non-negative-shift-distance))))

(defmacro left-shiftf (fixnum non-negative-shift-distance)
  `(the fixnum (tli::fixnum-left-shift
		 (the fixnum ,fixnum)
		 (the fixnum ,non-negative-shift-distance))))

(defmacro logbitpf (fixnum-index fixnum)
  `(logbitp (the fixnum ,fixnum-index) (the fixnum ,fixnum)))

(defmacro modf (fixnum fixnum-divisor &environment env)
  (if (eval-feature :chestnut-trans)
      `(chestnut-modf-function ,fixnum ,fixnum-divisor)
      (declare-argument-and-result-types
	'fixnum `(mod ,fixnum ,fixnum-divisor) env)))




;;; The function `rem-fixnums' takes two fixnums and returns the remainder of
;;; the two.  This is a faster interface to the second of truncatef.

(declaim (functional rem-fixnums))

(defun rem-fixnums (fixnum fixnum-divisor)
  (declare (type fixnum fixnum fixnum-divisor)
	   (return-type fixnum))
  ;;  (eliminate-for-gsi))
  (if (minuspf fixnum)
      (if (minuspf fixnum-divisor)
	  (-f (modf-positive (-f fixnum) (-f fixnum-divisor)))
	  (-f (modf-positive (-f fixnum) fixnum-divisor)))
      (if (minuspf fixnum-divisor)
	  (modf-positive fixnum (-f fixnum-divisor))
	  (modf-positive fixnum fixnum-divisor))))

(define-modify-macro incff (&optional (delta 1)) +f)

(define-modify-macro decff (&optional (delta 1)) -f)




;;; `Two-arg-lcmf' is LCM for two fixnums, returning one fixnum.

(declaim (functional two-arg-lcmf))

(defun two-arg-lcmf (n m)
  (declare (type fixnum n m)
	   (return-type fixnum))
;;  (eliminate-for-gsi))
  (*f (truncatef-first
	(maxf n m)
	(two-arg-gcdf n m))
      (minf n m)))




;;; `Two-arg-gcdf' is GCD of two fixnum arguments returning a fixnum.  With
;;; fixnum arguments, we use the binary GCD algorithm from Knuth's seminumerical
;;; algorithms.

;; Derived from CMU Lisp's code.

(declaim (functional two-arg-gcdf))

(defun two-arg-gcdf (u v)
  (declare (type fixnum u v)
	   (return-type fixnum))
;;  (eliminate-for-gsi))
  (cond
    ((=f u 0) v)
    ((=f v 0) u)
    (t
     (loop for k from 0
	   for u fixnum = (absf u) then (ashf u -1)
	   for v fixnum = (absf v) then (ashf v -1)
	   until (oddp (logiorf u v))
	   finally
	     (loop for temp fixnum = (if (oddp u) (-f v) (ashf u -1))
			    then (ashf temp -1)
		   when (oddp temp)
		     do
		       (if (pluspf temp)
			   (setq u temp)
			   (setq v (-f temp)))
		       (setq temp (-f u v))
		       (when (=f 0 temp)
			 (return-from two-arg-gcdf
			   (left-shiftf u k))))))))





;;;; System-specific Compilation




;;; The macro `current-system-case' is intended to eliminate unnecessary runtime
;;; branches based on the current system, when that system is running in a
;;; distribution image.  Each clause is a list whose head is an unevaluated
;;; symbol naming a system and whose tail is a set of actions to be executed in
;;; an implicit progn if the current-system-case form is evaluated in the given
;;; system.  Currently (jh, 3/18/94), system names are: ab (an old name for g2),
;;; gsi, and telewindows.

;;; In distribution, current-system-case only expands into the actions given by
;;; the clause for the current system and ignores the other clauses.  In
;;; development, this macro expands into a case statement that dispatches at
;;; runtime on current-system-name and uses all clauses.  The fact that the
;;; development expansion uses all clauses means that extra functions must be
;;; stubbed out there.  Worse, in general, any difference between distribution
;;; and development compiles is abhorrent, since it leads to obscure bugs.  But
;;; unless development does a runtime check, there will be problems loading
;;; different systems in the same Lisp.

(defun-for-macro get-appropriate-current-system-case-clause
    (clauses current-system)
  (dolist (system-and-actions clauses nil)
    (let ((system-or-systems (first system-and-actions)))
      (when (or (member system-or-systems '(t otherwise))
		(if (consp system-or-systems)
		    (member current-system system-or-systems)
		    (eq current-system system-or-systems)))
	(return `(progn ,@(rest system-and-actions)))))))

(defmacro current-system-case (&body clauses)
  (let ((systems-mentioned nil))
    (dolist (clause clauses)
      (let ((system-or-systems (first clause)))
	(cond
	  ((symbolp system-or-systems)
	   (push (if (memq system-or-systems '(t otherwise))
		     system-or-systems
		     (normalize-system-name system-or-systems))
		 systems-mentioned))
	  (t (setq systems-mentioned
		   (append (loop for sys in system-or-systems
				 collect (normalize-system-name sys))
			   systems-mentioned))))))
    (let ((errant-systems?
	    (set-difference
	      systems-mentioned
	      '(t otherwise tl-user::ab tl-user::telewindows tl-user::tw
		tl-user::ntw tl-user::gsi tl-user::tl tl-user::g2))))
      (when errant-systems?
	(error "CURRENT-SYSTEM-CASE: unknown system(s): ~s"
	       errant-systems?)))
    (cond
      ((eval-feature :development)
       `(cond
	  ,@(loop for clause in clauses
		  for system-or-systems = (car clause)
		  collect
		  (cons
		    (cond
		      ((memq system-or-systems '(t otherwise))
		       t)
		      ((symbolp system-or-systems)
		       `(memq ',(normalize-system-name system-or-systems)
			      tli::current-systems))
                      ((null (cdr system-or-systems))
                       `(memq ',(normalize-system-name (car system-or-systems))
                              tli::current-systems))
		      (t
		       `(loop for sys in ',(loop for sys in system-or-systems
						 collect
						 (normalize-system-name sys))
			      thereis (memq sys tli::current-systems))))
		    (cdr clause)))))
      (t
       (loop for clause in clauses
	     for system-or-systems = (car clause)
	     when (or (memq system-or-systems '(t otherwise))
		      (if (symbolp system-or-systems)
			  (memq (normalize-system-name system-or-systems)
				tli::current-systems)
			  (loop for sys in system-or-systems
				thereis (memq (normalize-system-name sys)
					      tli::current-systems))))
	       return (cons 'progn (cdr clause))
	     finally
	       ;; The following attempt at rigor was before its time.  -jallard
	       ;; 6/23/97
	       ; (error "Current-system-case found no applicable clause in ~s"
	       ; systems-mentioned)
	       (return nil))))))

(defmacro if-chestnut (chestnut-form otherwise-form)
  chestnut-form
  #+development
  otherwise-form
  #-development
  (if (and (eval-feature :chestnut-3)
	   (eval-feature :chestnut-trans))
      chestnut-form
      otherwise-form))

(defmacro if-chestnut-gsi (chestnut-gsi-form otherwise-form)
  chestnut-gsi-form
  #+development
  otherwise-form
  #-development
  `(current-system-case
     (gsi
       (if-chestnut
	 ,chestnut-gsi-form
	 ,otherwise-form))
     (t
       ,otherwise-form)))






;;;; Forward Reference Masking in Scopes




;;; The macro `with-ignored-forward-references' takes a list of function names
;;; that may be called within the body of this macro.  The compilation of this
;;; in development should mask any forward reference warnings by calling the
;;; function in a way that the compiler won't notice it as a source of a
;;; reference that might require warnings.  In translation, this macro is a no-op.

(defmacro with-ignored-forward-references (function-names &body forms)
  (if (eval-feature :development)
      `(macrolet
	   ,(loop for name in function-names
		  collect `(,name (&rest args)
				  (let ((func (gensym)))
				    `(let ((,func ',',name))
				       (funcall ,func ,@args)))))
	 ,@forms)
      `(progn ,@forms)))

;; Note that in TL, funcall macroexpands into a direct call to the given
;; function, if that function is a constant.  So, we must bind the function
;; object first, before calling funcall.  -jallard 5/7/97






;;;; Fixnum Arithmetic Assumptions




;;; The following code is run at launch time for any ThinLisp package that
;;; includes tl-extension.  It runs a function that will call error if any of the
;;; assumptions that we have made about arithmetic do not hold true.  Add more
;;; assumption checking code to the function validate-fixnum-assumptions.

(defmacro bad-assumption (error-string &rest error-format-args)
  (unless (stringp error-string)
    (warn "Cannot compile call to bad-assumption: ~a ~a"
	  error-string error-format-args))
  (let ((control-string
	  (concatenate
	    'string
	    "The following problem has been detected:  "
	    error-string)))
    `(error ,control-string ,@error-format-args)))
       

(defparameter negative-fifty-million -50000000)

(defparameter compile-time-most-positive-fixnum #.most-positive-fixnum)

(defparameter float-test-value 13.4)
(defparameter fixnum-test-value 11)

(defun validate-fixnum-assumptions ()
  (unless (=f most-positive-fixnum compile-time-most-positive-fixnum)
    (bad-assumption
      "Compile time most-positive-fixnum is different from run-time most-positive-fixnum."))
  (unless (=f (ashf negative-fifty-million -8) -195313)
    (bad-assumption
      "Right shift doesn't sign extend."))
  (unless (and (=f (tli::truncate-test-by-dividing-with-slash
		     (the double-float float-test-value) 1.0)
		   13)
	       (=f (tli::truncate-test-by-dividing-with-slash
		     (the double-float (- (the double-float float-test-value)))
		     1.0)
		   -13)
	       (=f (tli::truncate-test-by-dividing-with-slash
		     (the double-float float-test-value) -1.0)
		   -13)
	       (=f (tli::truncate-test-by-dividing-with-slash
		     (the double-float (- (the double-float float-test-value)))
		     -1.0)
		   13))
    (bad-assumption
      "Casting floats to integers cannot be relied on to perform truncation"))
;  #+cmu
;  (warn "Disabled truncation testing.")
  #-cmu
  (unless (and (=f (tli::truncate-test-by-dividing-with-slash
		     (the fixnum (- (the fixnum fixnum-test-value))) 2)
		   -5)
	       (=f (tli::truncate-test-by-dividing-with-slash
		     (the fixnum fixnum-test-value) -2)
		   -5)
	       (=f (tli::truncate-test-by-dividing-with-slash
		     (the fixnum (- (the fixnum fixnum-test-value))) -2)
		   5))
    (bad-assumption
      "Division of fixnums does not reliably perform truncation with negative args"))
  nil)

(validate-fixnum-assumptions)






;;;; Floating Point Arithmetic




;;; The function for macro `double-float-operator-for-operation' takes a symbol
;;; naming a Common Lisp numeric operation and returns the symbol naming the
;;; type declared equivalent for double-floats.

(defparameter-for-macro double-float-operator-alist
  '((1+ . 1+e)
    (1- . 1-e)
    (+ . +e)
    (- . -e)
    (* . *e)
    (/ . /e)
    (= . =e)
    (/= . /=e)
    (< . <e)
    (> . >e)
    (<= . <=e)
    (>= . >=e)
    (min . mine)
    (max . maxe)
    (abs . abse)
    (ffloor . ffloore)
    (ffloor-first . ffloore-first)
    (floor-first . floore-first)
    (ceiling-first . ceilinge-first)
    (fceiling . fceilinge)
    (ftruncate . ftruncatee)
    (fround . frounde)
    (log . loge)
    (atan . atane)
    (cos . cose)
    (exp . expe)
    (expt . expte)
    (minusp . minuspe)
    (plusp . pluspe)
    (sin . sine)
    (sqrt . sqrte)
    (tan . tane)))

(defun-for-macro double-float-operator-for-operation (operator)
  (or (cdr (assoc operator double-float-operator-alist))
      operator))


;;; Invokation of `assert-that-within-temporary-float-scope' provides a
;;; runtime assertion that we are dynamically inside of a temporary float
;;; scope.  This has no effect, or cost, if we are not  #+development.

;; - ben 2/14/91

#+development
(defvar within-temporary-float-scope? nil)

#+development
(defvar perform-float-scope-checking? nil)

(defmacro assert-that-within-temporary-float-scope ()
  #+development
  `(unless within-temporary-float-scope? 
     (cerror "Continue." "Call this function only inside a temporary-float-scope!"))
  #-development
  `nil)




(defmacro potentially-rigorous-operation (form)
  #+(and development rigorous-float-checking)
  `(progn
     (when perform-float-scope-checking?
       (assert-that-within-temporary-float-scope))
     ,form)
  #-(and development rigorous-float-checking)
  form)

(defmacro with-floating-point-checking-off (&body forms)
  `(let ((perform-float-scope-checking? nil))
     ,@forms))

;;; Chestnut has supplied us with a set of operations which take and produce raw
;;; double floats as values.  These double floats can only exist on the stack
;;; while being passed from one raw-double operation to another, and the final
;;; result must eventually be coerced back into a double-float.  The macro
;;; `coerce-double-float-to-raw-double' takes an argument which should be
;;; coerced into a raw-double.  If the argument to this macro is an expression,
;;; an attempt will be made to find an operator for that expression which
;;; already produces a raw-double as its value.  If one can be found, the
;;; operator of the given expression is replaced, and all of its arguments are
;;; themselves coerced to raw-doubles.  If no such operator can be found, the
;;; Chestnut primitve unboxing operation is emitted.

(defmacro coerce-double-float-to-raw-double (double-float-expression)
  #+chestnut
  (if (not (eval-feature :chestnut-trans))
      double-float-expression
      (let* ((expression
	       (loop for exp = double-float-expression
			     then (third exp)
		     while (and (consp exp) (eq (car exp) 'the))
		     finally (return exp)))
	     (operator?
	       (if (consp expression)
		   (car expression)))
	     (raw-operator?
	       (if operator?
		   (raw-double-returning-operator operator?))))
	(cond ((null raw-operator?)
	       (if (eq operator? 'if)
		   `(if ,(second expression)
			(coerce-double-float-to-raw-double
			  ,(third expression))
			(coerce-double-float-to-raw-double
			  ,(fourth expression)))
		   `(trun:unbox%d ,expression)))
	      ((raw-double-receiving-operator operator?)
	       `(,raw-operator?
		 ,@(loop for arg in (cdr expression)
			 collect `(coerce-double-float-to-raw-double ,arg))))
	      (t
	       `(,raw-operator? ,@(cdr expression))))))
  #-chestnut
  double-float-expression)




;;; The macro `coerce-raw-double-to-double-float' takes a raw-double produced by
;;; the Chestnut optimized floating point operations, and boxes it up again
;;; inside of a Common Lisp double float, using the Chestnut supplied primitive.

(defmacro coerce-raw-double-to-double-float (raw-double)
  #+chestnut
  (if (eval-feature :chestnut-trans)
      `(trun:box%d ,raw-double)
      raw-double)
  #-chestnut
  raw-double)




;;; The floating point arithmetic suite is a set of macros which are defined to
;;; act upon and (with the exception of predicates and the rounding type
;;; functions) return double floating point numbers.  These functions include
;;; type declarations for the operators and results so that compilers may
;;; optimize calls to these numeric operations into appropriate floating point
;;; machine instructions.  The following macros are defined: 1+e, 1-e, +e, -e,
;;; =e, /=e, <e, >e, <=e, >=e, mine, maxe, *e, /e, rounde, floore, ceilinge, and
;;; truncatee.  Note that it is also useful to declare the types of local
;;; variables to double-float.  Both Lucid and Ibuki [an obsolete platform] use
;;; this information to limit the boxing of a raw floating point number into a
;;; Lisp, consed float.

;; See the comment in the section about fixnum arithmetic above which explains
;; why we ARE wrapping numeric type definers around forms for the Lispms.

(defmacro 1+e (double-float &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types 'double-float `(1+ ,double-float) env)))

(defmacro 1+e-raw (raw-double &environment env)
  (declare-argument-and-result-types 'raw-double `(1+ ,raw-double) env))

(defmacro 1-e (double-float &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types 'double-float `(1- ,double-float) env)))

(defmacro 1-e-raw (raw-double &environment env)
  (declare-argument-and-result-types 'raw-double `(1- ,raw-double) env))

(defmacro +e (&rest double-floats &environment env)
  `(potentially-rigorous-operation
    ,(if (null double-floats)
	0.0
	(declare-type-preserving-numeric-operation
	  'double-float '+ double-floats env))))

(defmacro +e-raw (&rest raw-doubles &environment env)
  (declare-type-preserving-numeric-operation 'raw-double '+ raw-doubles env))

(defmacro -e (double-float &rest double-floats &environment env)
  `(potentially-rigorous-operation
    ,(declare-type-preserving-numeric-operation
      'double-float '- (cons double-float double-floats) env)))

(defmacro -e-raw (raw-double &rest raw-doubles &environment env)
  (declare-type-preserving-numeric-operation
    'raw-double '- (cons raw-double raw-doubles) env))

(defmacro *e (&rest double-floats &environment env)
  `(potentially-rigorous-operation
    ,(if (null double-floats)
	1.0
	(declare-type-preserving-numeric-operation
	  'double-float '* double-floats env))))

(defmacro *e-raw (&rest raw-doubles &environment env)
  (declare-type-preserving-numeric-operation 'raw-double '* raw-doubles env))

(defmacro /e (&rest double-floats &environment env)
  `(potentially-rigorous-operation
    ,(declare-type-preserving-numeric-operation 'double-float '/ double-floats env)))

(defmacro /e-raw (&rest raw-doubles &environment env)
  (declare-type-preserving-numeric-operation 'raw-double '/ raw-doubles env))

(defmacro =e (&rest double-floats &environment env)
  (declare-comparitor '= 'double-float double-floats env))

(defmacro =e-raw (&rest raw-doubles &environment env)
  (declare-comparitor '= 'raw-double raw-doubles env))

(defmacro /=e (&rest double-floats &environment env)
  (declare-comparitor '/= 'double-float double-floats env))

(defmacro /=e-raw (&rest raw-doubles &environment env)
  (declare-comparitor '/= 'raw-double raw-doubles env))

(defmacro <e (&rest double-floats &environment env)
  (declare-comparitor '< 'double-float double-floats env))

(defmacro <e-raw (&rest raw-doubles &environment env)
  (declare-comparitor '< 'raw-double raw-doubles env))

(defmacro >e (&rest double-floats &environment env)
  (declare-comparitor '> 'double-float double-floats env))

(defmacro >e-raw (&rest raw-doubles &environment env)
  (declare-comparitor '> 'raw-double raw-doubles env))

(defmacro <=e (&rest double-floats &environment env)
  (declare-comparitor '<= 'double-float double-floats env))

(defmacro <=e-raw (&rest raw-doubles &environment env)
  (declare-comparitor '<= 'raw-double raw-doubles env))

(defmacro >=e (&rest double-floats &environment env)
  (declare-comparitor '>= 'double-float double-floats env))

(defmacro >=e-raw (&rest raw-doubles &environment env)
  (declare-comparitor '>= 'raw-double raw-doubles env))

(defmacro mine (&rest double-floats)
  `(potentially-rigorous-operation
    ,(declare-type-preserving-numeric-operation
       'double-float 'min double-floats)))

(defmacro mine-raw (&rest raw-doubles &environment env)
  (declare-type-preserving-numeric-operation
    'raw-double 'min raw-doubles env))

(defmacro maxe (&rest double-floats &environment env)
  `(potentially-rigorous-operation
    ,(cond
      ((loop for argument in double-floats
	     always (simple-argument-p argument))
       (declare-type-preserving-numeric-operation
	 'double-float 'max double-floats env))
      (t
       (loop for argument in double-floats
	     for bound-argument = (gensym)
	     collect `(,bound-argument ,argument) into bindings
	     collect bound-argument into new-arguments
	     finally
	       (return
		 `(let ,bindings
		    ,(declare-type-preserving-numeric-operation
		       'double-float 'max new-arguments env))))))))

(defmacro maxe-raw (&rest raw-doubles &environment env)
  (declare-type-preserving-numeric-operation
    'raw-double 'max raw-doubles env))

(defmacro abse (double-float &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	`(abs ,double-float)
	env)))

(defmacro abse-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(abs ,raw-double)
    env))

(defmacro ffloore (double-float &optional double-float-divisor &environment env)
  `(potentially-rigorous-operation
     (the (values ,'double-float ,'double-float)
	  ,(declare-argument-types
	     'double-float
	     `(potentially-rigorous-operation
		,(if double-float-divisor
		     `(ffloor ,double-float ,double-float-divisor)
		     `(ffloor ,double-float)))
	     env))))

(defmacro fceilinge (double-float &optional double-float-divisor &environment env)
  `(potentially-rigorous-operation
     (the (values ,'double-float ,'double-float)
	,(declare-argument-types
	   'double-float
	   `(potentially-rigorous-operation
	     ,(if double-float-divisor
		 `(fceiling ,double-float ,double-float-divisor)
		 `(fceiling ,double-float)))
	   env))))




;;; The macro `ffloore-first' is like Common Lisp ffloor, but only the first
;;; value is returned and only one argument is received.  In C translation
;;; implementations there is a direct translation for this operator into the C
;;; floor function.

(defmacro ffloore-first (double-float &optional divisor-double-float?)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	(if divisor-double-float?
	    `(ffloor ,double-float ,divisor-double-float?)
	    `(ffloor ,double-float)))))




;;; The macro `floore-first' is like Common Lisp floor, but it accepts only
;;; double-floats, returns only the first value of floor, and the result must be
;;; able to lie within the range of fixnums.  The behavior is undefined if any
;;; of these constraints are not adhered to.

(defmacro floore-first (double-float &optional divisor-double-float?)
  `(potentially-rigorous-operation
     (the fixnum
	  ,(declare-argument-types
	     'double-float
	     (if divisor-double-float?
		 `(floor ,double-float ,divisor-double-float?)
		 `(floor ,double-float))))))




;;; The macro `ceilinge-first' takes a double-float and an optional double-float
;;; divisor, divides the two, takes the ceiling of the result, and returns the
;;; fixnum result.  Note that if the result is not within fixnum bounds, then
;;; the result of this operation is undefined and it may signal an error.  The
;;; macro `fceilinge-first' is the optimized version of fceiling.

(defmacro ceilinge-first (double-float &optional divisor-double-float?)
  `(potentially-rigorous-operation
     (the fixnum
	  ,(declare-argument-types
	     'double-float
	     (if divisor-double-float?
		 `(ceiling ,double-float ,divisor-double-float?)
		 `(ceiling ,double-float))))))

(defmacro fceilinge-first (double-float &optional divisor-double-float?)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	(if divisor-double-float?
	    `(fceiling ,double-float ,divisor-double-float?)
	    `(fceiling ,double-float)))))

(defmacro ftruncatee (double-float &optional double-float-divisor &environment env)
  `(potentially-rigorous-operation
    (the (values ,'double-float ,'double-float)
	 ,(declare-argument-types
	    'double-float
	    (if double-float-divisor
		`(ftruncate ,double-float ,double-float-divisor)
		`(ftruncate ,double-float))
	    env))))



;;; `ftruncatee-up' truncates up away from 0 (i.e. does the opposite of ftruncatee)

(defun ftruncatee-up (float)
;;  (declare (eliminate-for-gsi))
  (if (>e float 0.0)
      (fceilinge float)
      (ffloore   float)))


(defmacro frounde (double-float &optional double-float-divisor &environment env)
  `(potentially-rigorous-operation
    (the (values ,'double-float ,'double-float)
	 ,(declare-argument-types
	    'double-float
	    (if double-float-divisor
		`(fround ,double-float ,double-float-divisor)
		`(fround ,double-float))
	    env))))

(defmacro loge (double-float &optional double-float-base &environment env)
  `(potentially-rigorous-operation 
     ,(declare-argument-and-result-types
	'double-float
	(if double-float-base
	    `(log ,double-float ,double-float-base)
	    `(log ,double-float))
	env)))

(defmacro loge-raw (raw-double &optional raw-double-base &environment env)
  (declare-argument-and-result-types
    'raw-double
    (if raw-double-base
	`(log ,raw-double ,raw-double-base)
	`(log ,raw-double))
    env))

(defmacro atane (double-float &optional double-float-2 &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
       'double-float
       (if double-float-2
	   `(atan ,double-float ,double-float-2)
	   `(atan ,double-float))
       env)))

(defmacro atane-raw (raw-double &optional raw-double-2 &environment env)
  (declare-argument-and-result-types
    'raw-double
    (if raw-double-2
	`(atan ,raw-double ,raw-double-2)
	`(atan ,raw-double))
    env))

(defmacro cose (double-float &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	`(cos ,double-float)
	env)))

(defmacro cose-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(cos ,raw-double)
    env))

(defmacro expe (double-float &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	`(exp ,double-float)
	 env)))

(defmacro expe-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(exp ,raw-double)
    env))

(defmacro expte (double-float double-float-power &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	`(expt ,double-float ,double-float-power)
	env)))

(defmacro expte-raw (raw-double raw-double-power &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(expt ,raw-double ,raw-double-power)
    env))

(defmacro minuspe (double-float &environment env)
  (declare-argument-and-result-types
    'double-float
    `(minusp ,double-float)
    env))

(defmacro minuspe-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(minusp ,raw-double)
    env))

(defmacro pluspe (double-float &environment env)
  (declare-argument-and-result-types
    'double-float
    `(plusp ,double-float)
    env))

(defmacro pluspe-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(plusp ,raw-double)
    env))

(defmacro sine (double-float &environment env)
  (declare-argument-and-result-types
    'double-float
    `(sin ,double-float)
    env))

(defmacro sine-raw (raw-double &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'raw-double
	`(sin ,raw-double)
	env)))

(defmacro sqrte (double-float &environment env)
  `(potentially-rigorous-operation
    ,(declare-argument-and-result-types
      'double-float
      `(sqrt ,double-float)
      env)))

(defmacro sqrte-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(sqrt ,raw-double)
    env))

(defmacro tane (double-float &environment env)
  `(potentially-rigorous-operation
     ,(declare-argument-and-result-types
	'double-float
	`(tan ,double-float)
	env)))

(defmacro tane-raw (raw-double &environment env)
  (declare-argument-and-result-types
    'raw-double
    `(tan ,raw-double)
    env))

;;




;;; The macro `coerce-to-double-float' takes a non-complex number and converts
;;; it into a double-float.

(defmacro coerce-to-double-float (number)  
  `(potentially-rigorous-operation
     ,(cond ((constantp number)
	     (float (eval number) 0.0))
	    (t
	     `(float ,number 0.0)))))




;;; The macro `coerce-fixnum-to-double-float' takes a fixnum and converts it
;;; into a double-float.

(defmacro coerce-fixnum-to-double-float (fixnum)
  `(potentially-rigorous-operation
     ,(cond ((constantp fixnum)
	     (float (eval fixnum) 0.0))
	    (t
	     `(float (the fixnum ,fixnum) 0.0)))))

(defun coerce-to-double-float-function (x)
  (declare (consing-area either))
  (coerce-to-double-float x))


;;; The macro `double-float-p' is a predicate which takes a thing and determines
;;; whether it is a double-float.

(defmacro double-float-p (thing)
  (if (eval-feature :chestnut-trans)
      `(floatp ,thing)
      `(typep ,thing 'double-float)))




;;; The following feature is used by LOOP to determine whether or not it should
;;; attempt to use +F etc. operators.  This is being put in because of a nagging
;;; bug with some of the defun-for-macros (namedly typed-operator-for-type) are
;;; undefined when compiling forms in the bottom of this file.  This is odd,
;;; since all of them are wrapped with eval-when (compile load eval).

#-no-macros
(pushnew :thinlisp-typed-arithmetic *features*)





;;;; Fast Operations On List Structure




(defmacro first-of-long-enough-list (conses)
  `(first-of-conses ,conses))

(defmacro second-of-long-enough-list (conses)
  `(second-of-conses ,conses))

(defmacro third-of-long-enough-list (conses)
  `(third-of-conses ,conses))

(defmacro fourth-of-long-enough-list (conses)
  `(fourth-of-conses ,conses))

(defmacro fifth-of-long-enough-list (conses)
  `(fifth-of-conses ,conses))

(defmacro sixth-of-long-enough-list (conses)
  `(sixth-of-conses ,conses))

(defmacro seventh-of-long-enough-list (conses)
  `(seventh-of-conses ,conses))

(defmacro eighth-of-long-enough-list (conses)
  `(eighth-of-conses ,conses))

(defmacro ninth-of-long-enough-list (conses)
  `(ninth-of-conses ,conses))

(defmacro tenth-of-long-enough-list (conses)
  `(tenth-of-conses ,conses))






;;;; Preserve Symbol Function and Symbol Value



;;; `Declare-funcallable-symbol' is used to declare that symbol has a
;;; symbol-function that is funcall'ed else where.  This declaration is
;;; necessary to allow translation.

;;; In the Chestnut translator, Lisp symbols which name functions and global
;;; variables are eliminated from the run-time image, and references to the
;;; functions and values represented by this symbols are in-lined at reference
;;; points.  If a Lisp symbol is really needed at run-time, because
;;; symbol-value, symbol-function, or whatever is going to be called on it, and
;;; if this symbol is not already referenced elsewhere in the code as a quoted
;;; constant, the following declarations need to be issued to ensure that the
;;; symbol-value and symbol-function cells of the symbol are preserved into
;;; run-time.

(defvar destination-for-symbol-with-preserved-cells nil)

(defmacro declare-funcallable-symbol (unquoted-symbol)
  (unless (symbolp unquoted-symbol)
    (error "Funcallable symbol ~s should have been an unquoted symbol"
	   unquoted-symbol))
  `(setq destination-for-symbol-with-preserved-cells ',unquoted-symbol))



;;; `Declare-symbol-value-preservation' is used to declare that symbol will have
;;; its symbol-value accessed via the symbol-value function.  This declaration is
;;; necessary to allow translation.

(defmacro declare-symbol-value-preservation (unquoted-symbol)
  (assert
    (symbolp unquoted-symbol) nil
    "~a should have been an unquoted symbol" unquoted-symbol)

  `(setq destination-for-symbol-with-preserved-cells ',unquoted-symbol))






;;;; Side-Effect Free Functions




;;; The macro `declare-side-effect-free-function' is given an unquoted symbol
;;; which is the name of a function.  This declaration is used to tell the
;;; compiler that this function can't change the behavior or returned values of
;;; any other operation.  This information allows the compiler to relax the
;;; restriction that arguments to functions in Common Lisp are called in left to
;;; right order.  This relaxation can allow for some significant optimizations.
;;; The macro `declare-side-effect-free-functions' takes an &rest list of
;;; unquoted symbols, and does the same thing.

;;; It is obviously true that a function is side-effect free if the function
;;; doesn't do any SETQ, SETF, or other data modifying operation--it only
;;; extracts portions of a given data structure.

;;; This will also be true for any function which doesn't change anything in a
;;; way which could cause another function (or this one for that matter) to
;;; return another value.  For example, if a property list fetching operation
;;; puts a fetched operation at the front of a property list (assuming this
;;; property will be fetched again soon), and no operation in the system could
;;; return a different value given the order of properties in a property list,
;;; then the property list fetching operation can be declared side-effect free.

;;; There is a further, less obvious case, where a function could be considered
;;; side-effect free.  Say that function A causes side-effects, those side
;;; effects cannot affect the behavior of function A, but they can affect the
;;; behavior of other operations in set B.  Function A can be declared
;;; side-effect free if none of the functions in set B are declared side-effect
;;; free.  This can be done since any argument list containing calls to the
;;; operations in set B will be executed left to right, giving correct behaviors
;;; to argument lists containing calls to both function A and calls to
;;; operations in the set B.  However, argument lists containing only calls to
;;; function A and other side-effect free functions can be optimized.  This
;;; special case is very important, since it can justify side-effect free
;;; declarations for structure allocation functions, like thinlisp-cons, as long
;;; as the memory metering functions are not declared side-effect free.  If
;;; there are more calls that are performance critical to thinlisp-cons than there
;;; are to the memory metering functions, then this trade-off is a win.

(defmacro declare-side-effect-free-function (function-name)
  `(declaim (side-effect-free ,function-name)))

(defmacro declare-side-effect-free-functions (&rest function-names)
  `(declaim (side-effect-free ,@function-names)))






;;;; Funcalling




;;; We have shadowed funcall in G2 to be able to expand it into more efficient
;;; machine specific code for some implementations of Lisp.  In particular,
;;; there is a fast C encoded version for Chestnut.  For all other
;;; implementations of Lisp, this macro expands into a normal Lisp funcall.
;;; There are also versions of funcall which are optimized for funcalling
;;; symbols and compiled functions.

;;; The macro `thinlisp-funcall' is like the Common Lisp version of funcall,
;;; except that it can only accept symbols and compiled function objects as the
;;; function argument.  In some systems (like ohh say Chestnut) it expands into
;;; C code which is much faster than the default Common Lisp implementation.
;;; Note that right now, the symbol funcall is itself shadowed and redefined as
;;; a macro that calls thinlisp-funcall.

;;; The comments above about faster funcalls no longer apply to the generic
;;; funcall operation.  We are putting in an optimization for funcall-simple-
;;; compiled-function, but the generic funcall still must go through Chestnut's
;;; implementation to handle things like optional and keyword arguments.  So,
;;; the thinlisp-funcall macro will only expand into ab-lisp::funcall, though we will
;;; retain this macro in case we want to do something interesting with it in the
;;; future.  -jra 4/24/91

(defmacro thinlisp-funcall (symbol-or-compiled-function &rest arguments)
  `(funcall ,symbol-or-compiled-function ,@arguments))




;;; The `funcall-symbol' macro should be used in all cases where a funcall is
;;; performed whose first argument will be a symbol naming a compiled function
;;; (i.e.  the vast majority of our funcall dispatches).  This macro will
;;; produce an inline funcall on those systems which can produce it.  See also
;;; funcall-simple-function-symbol, defined above.

(defmacro funcall-symbol (symbol &rest arguments)
  `(funcall-compiled-function (symbol-function ,symbol) ,@arguments))



;;; The macro `funcall-compiled-function' should be used in all cases where a
;;; funcall is performed whose first argument will be a compiled function
;;; object.  This macro produces inline dispatching to the funcalled function on
;;; those system which can support it.

(defmacro funcall-compiled-function (compiled-function &rest arguments)
  `(funcall (the compiled-function ,compiled-function) ,@arguments))






;;;; Function Declarations




;;; The macro `declare-function-type' takes a function name, a list of the
;;; argument types accepted by a function, and a list of the value types
;;; returned by the function.  If the number of returned values is not known,
;;; then supply an asterisk for the return values list.  If the function has no
;;; return value, then supply the TL-specific type VOID.

;;; It will register the function declaration so that it can be used by the Lisp
;;; compiler to produce more efficient function calls.  In development it also
;;; sets a property on the function name symbol reflecting the declaration.

;;; In TLT, we pay lots of attention to the function type declarations, and this
;;; macro will now expand out directly to a call to declaim of an ftype.

(defmacro declare-function-type
	  (function-name argument-type-list return-type-list-or-asterisk)
  `(declaim (ftype
	      (function
		,argument-type-list
		,(cond ((eq return-type-list-or-asterisk '*)
			'*)
		       ((memq return-type-list-or-asterisk
			      '(void tli::void))
			'void)
		       ((and (consp return-type-list-or-asterisk)
			     (null (cdr return-type-list-or-asterisk)))
			(car return-type-list-or-asterisk))
		       ((listp return-type-list-or-asterisk)
			(cons 'values return-type-list-or-asterisk))
		       (t
			(error "Bad return-type ~s to declare-function-type."
			       return-type-list-or-asterisk))))
	      ,function-name)))





;;; The macro `declare-simple-functions' produces function-type declarations for
;;; a set of simple functions, i.e. function which take a fixed number of
;;; arguments of type T and return a single value of type T.

(defmacro declare-simple-functions (&rest functions-and-arg-list-lengths)
  `(progn
     ,@(loop for (function arg-list-length) on functions-and-arg-list-lengths
					    by #'cddr
	     for arg-types = (make-list arg-list-length)
	     do (fill arg-types t)
	     collect
	     `(declare-function-type ,function ,arg-types (t)))))


#+development
(defparameter enable-simple-compiled-function-type-checking nil)

#+development
(defparameter simple-compiled-function-blacklist  nil)

#+development
(defun simple-compiled-function-p (thing)
  (and (compiled-function-p thing)
       #+lucid (let* ((name (sys:procedure-ref thing 1))
		      (ftype (if (symbolp name)
				 (get name 'function-type-declaration))))
		 (and (consp ftype)
		      (consp (first ftype))
		      (every #'(lambda (x) (eq x t)) (first ftype))
		      (or (equal (second ftype) '(t))
			  (equal (second ftype) '*)	    ; Methods
			  (equal (second ftype) '(t t))
			  (equal (second ftype) '(t t t))
			  (equal (second ftype) '(t t t t)))))))

#+development
(defun check-type-of-simple-compiled-function (function)
  (unless (simple-compiled-function-p function)
    (pushnew function simple-compiled-function-blacklist)
    (when enable-simple-compiled-function-type-checking
      (cerror "Ignore it" "~s is not a simple-compiled-function" function))))

(defun-for-macro split-defun-body (body)
  (do* ((collected-declaration-and-documentation nil)
	(rest-of-body body (cdr rest-of-body))
	(this-form (car body) (car rest-of-body)))
       ((or (not (consp rest-of-body))
		     (not (or (and (consp this-form)
				   (eq (car this-form) 'declare))
			      (stringp this-form))))

	;; jh, 3/4/93.  CLtL/2e, p 85: "If doc-string is not followed by a
	;; declaration, it may be present only if at least one form is also
	;; specified, as it is otherwise taken to be a form."
	(let ((last-decl-or-doc-form
		(first collected-declaration-and-documentation)))
	  (multiple-value-bind (doc-and-decl-forms defun-body)
	      (if (and (null rest-of-body)
		       (stringp last-decl-or-doc-form))
		  (values (nreverse 
			    (cdr collected-declaration-and-documentation))
			  (list last-decl-or-doc-form))
		  (values (nreverse collected-declaration-and-documentation)
			  rest-of-body))

	    ;; jh, 3/4/93.  CLtL/2e, p 85: "It is an error if more than one
	    ;; doc-string is present."
	    (when (> (count-if #'stringp doc-and-decl-forms) 1)
	      (warn "SPLIT-DEFUN-BODY: more than one doc string in~%~s~%"
		    body))

	    (values doc-and-decl-forms defun-body))))

    (push this-form collected-declaration-and-documentation)))




;;; The function `expand-defun-simple' builds the definition of a simple
;;; function, using the given defun type ('defun, etc).

(defun-for-macro expand-defun-simple (defun-type name lambda-list body)
  (if (loop for arg in lambda-list
	    thereis (or (not (symbolp arg))
			(eq '&optional arg)
			(eq '&key arg)
			(eq '&rest arg)))
      (error "Argument lists for simple functions cannot contain &optional, ~
              &key, &rest, or non-symbol arguments.")
      `(,defun-type ,name ,lambda-list
	 (declare (return-type t))
	 ,@body)))




;;; The macro `defun-simple' is used to define functions which are simple
;;; functions.  Simple functions are those which take a fixed
;;; number of arguments and return one value.  These functions can be compiled
;;; directly into C functions which take their arguments and receive their
;;; values on the C stack, which is more efficient than passing them on the Lisp
;;; stack data structure.

(defmacro defun-simple (name arguments &body declarations-and-forms)
  (expand-defun-simple 'defun name arguments declarations-and-forms))




;;; The macro `defun-simple-allowing-unwind' is a variation on a theme.

(defmacro defun-simple-allowing-unwind (name arguments &body declarations-and-forms)
  (expand-defun-simple 'defun-allowing-unwind name arguments declarations-and-forms))

(defmacro defun-allowing-unwind (name lambda-list &rest decls-and-body)
  `(defun ,name ,lambda-list
     (declare (allow-unwind-protect))
     ,@decls-and-body))




;;; The macro `defun-void' is used to define functions which return
;;; no values.  This is the moral equivalent of the C return type VOID.
;;; Otherwise all the rules about defun-simple apply to this guy.

#+development
(defparameter bogus-value 'bogus-defun-void-value)

(defmacro defun-void (name arguments &body declarations-and-forms)
  (if (loop for arg in arguments
	    thereis (or (not (symbolp arg))
			(eq '&optional arg)
			(eq '&key arg)
			(eq '&rest arg)))
      (error "Argument lists for simple or void functions cannot contain ~
              &optional, &key, &rest, or non-symbol arguments.")
      `(defun ,name ,arguments
	 (declare (return-type void))
	 ,@declarations-and-forms
	 #+development
	 bogus-value)))




;;; The macro `defun-funcallable' is used to define functions which are funcalled
;;; (lisp functions called by system procedures for instance).

(defmacro defun-funcallable (name arguments &body declarations-and-forms)
  `(progn
     (declare-funcallable-symbol ,name)
     (defun ,name ,arguments ,@declarations-and-forms)))




;;; The macro `defun-into-place' is for defining anonymous functions that will
;;; have a compiled function object that will be placed into some location.
;;; Though in a distribution compile these functions will not have a Lisp symbol
;;; associated with them, in development they will be defined under the symbol
;;; given in the first argument.

;;; The compiled function objects for these functions should be fetched from the
;;; place using the macro `compiled-function-in-place'.  In development, the
;;; symbol naming the function will be kept in the place so that trace can work,
;;; but in distribution the compiled function will be kept there, and then
;;; compiled-function-in-place will simply return its argument.

;;; The following variants of defun-into-place provide extra functionality
;;; similar to their defun counterparts: `defun-simple-into-place',
;;; `defun-simple-multi-valued-into-place', and `defun-void-into-place'.

;; Note that the first attempt at this utility failed, since our initial use of
;; flet to define the functions did not allow for the tx:nonrelocating
;; declaration to be applied to the function.  Rick@chestnut assures me now that
;; using labels will make this work out.  Here's hoping.  -jra 12/7/93

(defmacro defun-into-place ((name place) arguments &body body)
  #+chestnut-3
  `(labels ((,name ,arguments ,@body))
     ,@(when (eval-feature :chestnut-trans)
         `((declare (tx:nonrelocating ,name))))
     (setf ,place #',name)
     nil)
  #-chestnut-3
  `(progn
    (defun ,name ,arguments ,@body)
     #+development (setf ,place ',name)
     #-development (setf ,place #',name)
    nil))

(defmacro compiled-function-in-place (defun-into-place-location)
  (if (eval-feature '(or :chestnut-3 (not :development)))
      defun-into-place-location
      `(symbol-function ,defun-into-place-location)))

(defmacro defun-simple-into-place ((name place) arguments &body body)
  #+chestnut-3
  (multiple-value-bind (declaration-and-documentation inner-body)
      (split-defun-body body)
    `(labels ((,name ,arguments
		,@declaration-and-documentation
		,@(if (and (eval-feature :chestnut-trans)
			   (not (eval-feature :chestnut-3-2-7)))
		      `((gensym-return-one (block ,name ,@inner-body))
			nil)
		      inner-body)))
       ,@(when (eval-feature :chestnut-trans)
           `((declare (tx:nonrelocating ,name))))
       #+chestnut-3-2-7
       ,@(when (eval-feature :chestnut-trans)
	   `((declare (tx:returns-one-value ,name))))
       (setf ,place #',name)
       nil))
  #-chestnut-3
  `(progn
    (defun-simple ,name ,arguments ,@body)
     #+development (setf ,place ',name)
     #-development (setf ,place #',name)
    nil))

(defmacro defun-void-into-place ((name place) arguments &body body)
  #+chestnut-3
  (multiple-value-bind (declaration-and-documentation inner-body)
      (split-defun-body body)
    `(labels ((,name ,arguments
		,@declaration-and-documentation
		,@inner-body
		,@(when (and (eval-feature :chestnut-trans)
			     (not (eval-feature :chestnut-3-2-7)))
		    `((gensym-return-one nil)))
		nil))
       ,@(when (eval-feature :chestnut-trans)
           `((declare (tx:nonrelocating ,name))))
       #+chestnut-3-2-7
       ,@(when (eval-feature :chestnut-trans)
	   `((declare (tx:returns-one-value ,name))))
       (setf ,place #',name)
       nil))
  #-chestnut-3
  `(progn
     (defun-void ,name ,arguments ,@body)
     #+development (setf ,place ',name)
     #-development (setf ,place #',name)
     nil))

(defmacro defun-simple-multi-valued-into-place
    ((name place) arguments &body body)
  #+chestnut-3
  `(labels ((,name ,arguments ,@body))
     ,@(when (eval-feature :chestnut-trans)
         `((declare (tx:nonrelocating ,name))))
     (setf ,place #',name)
     nil)
  #-chestnut-3
  `(progn
    (defun ,name ,arguments ,@body)
     #+development (setf ,place ',name)
     #-development (setf ,place #',name)
    nil))

;; Customers:
;;  def-goto-tag-recorder
;;  def-special-form-converter
;;  def-preferred-instruction-chooser
;;  

;; Potential customers:
;;  define-chart-data-point-indicator
;;  def-class-method

;;;






;;;; Basic Macros




;;; `Memq', `assq', and `delq' are Lisp classics, but were foolishly omitted
;;; from Common Lisp.  (Delq should be used only where reclaiming is not an
;;; issue.)

;;; `Memq-macro' and `assq-macro' are macro versions of memq and assq.

;;; `Getfq' is an eq version of getf.  It can not be used with setf.

;; Note that within the macros below we used to use the Lisp machine primitives
;; for memq and assq.  However, this could make the disassemblies of some of
;; our facilities be deceiving, in that we would have function calls in some
;; implementations, but machine instructions on the Lisp machine.  Similarly, in
;; macro expansions, we would get a couple of instructions on the Lisp machine,
;; but a big macro expansion in the delivered product.  Now that we are not
;; shipping G2 on Lisp machines, it makes sense to have the Lisp machine do what
;; everything else does, instead of taking pains to make the Lisp machines run
;; as fast as possible.  -jra 5/8/91

(defmacro default-memq-macro (key list)
  (let ((key-var?
	  (if (not (or (constantp key) (symbolp key)))
	      (gensym)))
	(current-cons (gensym)))
    `(loop ,@(if key-var? `(with ,key-var? = ,key))
	   for ,current-cons = ,list then (cdr-of-cons ,current-cons)
	   while ,current-cons
	   do
	     (when (eq (car-of-cons ,current-cons) ,(or key-var? key))
	       (return ,current-cons)))))



;;; `memq-p-macro' is similar to memq, except that in certain cases where the
;;; value of memq would be a list, its value is car of that list. Memq-p-macro
;;; optimizes the case where the second argument is a quoted list by converting
;;; the form to a disjunction of eq tests. E.g. (memq-p-macro x '(a b c) becomes
;;; (or (eq x 'a) (eq x 'b) (eq x 'c)). 

(defmacro memq-p-macro (key list)
  (let ((key-var?
	  (if (not (simple-argument-p key))
	      (gensym)))
	(current-cons (gensym)))
    (if (and (consp list) (eq (car list) 'quote))

	(if key-var?
	    `(let ((,key-var? ,key))
	       (or . ,(loop for elt in (second list) collect `(eq ,key-var? ',elt))))	
	    `(or . ,(loop for elt in (second list) collect `(eq ,key ',elt))))
	
	`(loop ,@(if key-var? `(with ,key-var? = ,key))
	       for ,current-cons = ,list then (cdr-of-cons ,current-cons)
	       while ,current-cons
	       do
		 (when (eq (car-of-cons ,current-cons) ,(or key-var? key))
		   (return ,current-cons))))))




(defmacro memq-macro (key list)
  `(default-memq-macro ,key ,list))

;; As a note for possible (though unlikely) future reference, both Lucid and
;; Zetalisp have memq operations, in the symbols lucid::memq and zeta::memq.  We
;; used to use those operations for Lucid and Lispm implementations, but have
;; switched to keep this Lisps, now only used during development, consistent
;; with the delivered software in Chestnut.  -jra 5/30/91

(defun eq-or-memq (x y)
;;  (declare (eliminate-for-gsi))
  (if (atom y) (eq x y) (memq x y)))


(defmacro default-assq-macro (key a-list)
  (let ((key-var?
	  (if (not (simple-argument-p key))
	      (gensym)))
	(alist-entry? (gensym)))
    `(loop ,@(if key-var? `(with ,key-var? = ,key))
	   for ,alist-entry? in ,a-list
	   do
	     (when (and ,alist-entry?
			(eq (car-of-cons ,alist-entry?)
			    ,(or key-var? key)))
	       (return ,alist-entry?)))))

(defmacro assq-macro (key a-list)
  `(default-assq-macro ,key ,a-list))


(defmacro getfq-macro (plist indicator &optional default)	; can't be used in setf
  (let ((indicator-var?
	  (if (not (simple-argument-p indicator))
	      (gensym)))
	(default-var?
	  (if (not (simple-argument-p default))
	      (gensym)))
	(l (gensym)))
    `(loop with ,l = ,plist
	 ,@(if indicator-var? `(with ,indicator-var? = ,indicator))
	 ,@(if default-var? `(with ,default-var? = ,default))
	 while ,l
	 do
      (cond ((eq (car-of-cons ,l) ,(or indicator-var? indicator))
	     (setq ,l (car-of-cons (cdr-of-cons ,l)))
	     (return ,l))
	    (t
	     (setq ,l (cdr-of-cons (cdr-of-cons ,l) ))))
	 finally
	   (return ,(or default-var? default)))))

(declare-side-effect-free-function getfq-function-no-default)

(defun-simple getfq-function-no-default (plist indicator)
  (getfq-macro plist indicator))

(declare-side-effect-free-function getfq-function)

(defun-simple getfq-function (plist indicator default)
  (getfq-macro plist indicator default))

(defmacro getfq (plist indicator &optional default)
  (if default
      `(getfq-function ,plist ,indicator ,default)
      `(getfq-function-no-default ,plist ,indicator)))



  
;; Note that Zetalisp has a delq operation in zeta::delq, but we don't use it
;; for reasons of staying consistent with the delivered systems in Chestnut.
;; -jra 5/30

;; A hole in Common Lisp is the absence of a pop-like macro that will delete a
;; specific item from anywhere in the list found at place.  But one could
;; probably define it by means of define-modify-macro!




;;; The macro `memberp' is a version of member which returns true (non-nil)
;;; or false (nil), depending on membership.  It takes the same arguments as
;;; CLtL member.  However, it may or may not return the tail of the list
;;; containing the first arg; i.e., it might just return T in the true case.

(defmacro memberp (&rest member-args)
  `(and (member ,@member-args) t))

;; This macro is primarily here to take advantage of an optimization in
;; Chestnut which inlines constant lists in member calls within an "and", when
;; those calls are not the last form of the and.  Also, Chestnut puts out pesky
;; warnings when it thinks the optimization should be used.  -jra 5/30/91

;; That should be done, then, for that implementation -- in other
;; implementations, it would likely just add extra code, since CLtL's AND has
;; to return its last value.  So I took out of the spec that this returns T in
;; the true case; such a spec would almost never be preferred to the new one.
;; (I will leave it to some other industrious sole to decide to actually fork
;; the implementation -- it's nicer in some ways to to have it be the same on
;; all platforms, and the only one that matters for now is Chestnut.)
;; (MHD 5/31/91)




;;; `Neq' is here, too.

(defmacro neq (x y)
  `(not (eq ,x ,y)))



;;; `True-non-empty-list-p' is true if its arg is a non-null list that terminates
;;; with nil.

(def-substitution-macro true-non-empty-list-p (x)
  (and (not (atom x)) (null (cdr (last x)))))



;;; `Multiple-values' are used widely in our code, but there are constructs
;;; around which their use is difficult.  

;;; In production code we do not use `multiple-value-prog1', `values-list',
;;; `multiple-value-call', setf of values, or `multiple-value-list'

;;; For example `multiple-value-prog1' is not known to work in Chestnut, it was
;;; known to leak in most Lisp implmentations.  It is very difficult to prove
;;; that it works in a given implementation since testing all combinations with
;;; all contour crossing operations is difficult.

;;; Particularly notable are the macros that do unwind-protect in development
;;; only.  They expand to prog1 in production, and hence never return
;;; multiple-values.  Bummer.

;;; Multiple-value-prog1 was trusted on the lispm so there is code, now
;;; obsolete, which uses it.

;; ben 2/16/94

;; Today we are starting an experiment to see if multiple-value-prog1 can be
;; trusted in Chestnut.  In doing so we are converting a very few cases of prog1
;; to this in critical macros where it's absence is deeply felt. - ben 2/17/94

#+forward-reference
(def-concept multiple-values)


;;; `Without-multiple-values' evaluates it's body and returns the
;;; first value of it's last form.  In development it will emit a
;;; warning if the last form had additional values.

(defmacro without-multiple-values (&body body)
  #+development
  (let ((result (gensym))
	(a (gensym))
	(b (gensym))
	(c (gensym))
	(d (gensym))
	(e (gensym))
	(f (gensym)))
    `(multiple-value-bind (,result ,a ,b ,c ,d ,e ,f)
	 (progn ,@body)
       (when (or ,a ,b ,c ,d ,e ,f)
	 (development-warning-about-multiple-values))
       ,result))
  #-development
  (let ((result (gensym)))
    `(let ((,result (progn ,@body)))
       ,result)))

#+development
(defun-for-macro development-warning-about-multiple-values ()
  (warn
    "Discarding additional multiple-values within a without-multiple-values form."))


;;; `Nth-value' returns the nth value, counting from 0, of multiple-value-form.
;;; If multiple-value-form has fewer than n values, nil is returned.

;; Note that this macro requires a constant integer as its first argument.

(defmacro nth-value (n multiple-value-form)
  (loop with variable = (gensym)
	as i from 0 below n
	collect (gensym) into other-variables
	finally
	  (return
	    `(multiple-value-bind (,@other-variables ,variable)
		 ,multiple-value-form
	       (declare (ignore . ,other-variables))
	       ,variable))))

;; Forgotten in Common Lisp.

;; It's been added in CLtL II, but is not yet available in most Lisps.  -jra
;; 11/26/90



;;; `Multiple-value-setq-some' is just like multiple-value-setq, except that
;;; it accepts nil in place of a var whose value should be ignored.

(defmacro multiple-value-setq-some (variables form)
  (if (loop for variable? in variables
	    always variable?)
      ;; then:
      `(multiple-value-setq ,variables ,form)
      ;; else:
      (loop repeat (length variables)
	    collect (gensym) into gensyms
	    finally
	      (return
		`(multiple-value-bind ,gensyms
		     ,form
		   ,@(loop for variable? in variables
			   as gensym in gensyms
			   as first-time? = t then nil
			   when (and (null variable?) (not first-time?))
			     collect gensym into gensyms-to-ignore
			   finally
			     (return
			       (if gensyms-to-ignore
				   `((declare (ignore . ,gensyms-to-ignore))))))
		   ,@(loop with rest-of-body-so-far = nil
			   for variable? in variables
			   as gensym in gensyms
			   as first-time? = t then nil
			   do (cond
				(variable?
				 (setq rest-of-body-so-far
				       (cons `(setq ,variable? ,gensym)
					     rest-of-body-so-far)))
				(first-time?
				 (setq rest-of-body-so-far
				       (cons gensym
					     rest-of-body-so-far))))
			   finally
			     (return rest-of-body-so-far)))))))

;; This spec should have been Common Lisp's spec for multiple-value-setq.
;; In Lisp Machine Lisp, multiple-value-setq-some is called multiple-value.  CLtL
;; falsely claims that multiple-value-setq is called multiple-value.  The differences
;; between multiple-value-setq-some and multiple-value-setq are exactly the same
;; as between Lisp Machine Lisp's multiple-value and multiple-value-setq.
;; (MHD 12/19/90)

;; It's been added in CLtL II, but is not yet available in most Lisps.  -jra
;; 11/26/90



;;; The macro `pretend-to-use-value' simply tells compilers not to give a warning
;;; that "the value of X is not used".  X should be a symbol.  This can be relied
;;; on to be a very efficient method of accomplishing its "trick", and should be
;;; standardly used for such purposes.  See also multiple-value-setq-some, which
;;; can often be useful in eliminating the variables warned about.

(defvar special-variable-for-use-value-macro nil)	; always nil

(defmacro pretend-to-use-value (x)
  `(if special-variable-for-use-value-macro
       (setq special-variable-for-use-value-macro ,x)))

;; If faster ways to do this in a given implementation are discovered, they should
;; be employed.  The speed of this is that of a nil check on a special variable,
;; which should be quite fast.

;; I originally thought of the name "use-value", but that led me to discover that
;; Lucid already defined a function of the same name, although I am not sure of
;; its function. (MHD 1/9/91)



;;; The macro `nthcdr-macro' is used inside the defmacro for defstruct-g2
;;; accessor forms.  It expands into N calls to cdr on the result of the form
;;; passed in.

(defmacro nthcdr-macro (n form)
  (cond ((and (constantp n)
	      (< (eval n) 20))
	 (let ((n-evaled (eval n)))
	   (loop with result-form = form
		 repeat n-evaled
		 do
		   (setq result-form (list 'cdr result-form))
		 finally
		   (return result-form))))
	(t
	 (let ((result (gensym)))
	   `(loop repeat ,n
		  with ,result = ,form
		  do
		    (setq ,result (cdr ,result))
		  finally
		    (return ,result))))))



;;; The macro `set-list-contents' will modify a list such that it contains as
;;; elements the new contents passed as the rest argument.  It is more efficient
;;; than calling setf on the first, then second, etc. in that it will not
;;; traverse the conses of a list more than once.  It returns the passed list.

(defmacro set-list-contents (list &rest new-contents)
  (cond ((null new-contents)
	 list)
	((null (cdr new-contents))
	 (if (symbolp list)
	     `(progn
		(setf (car ,list) ,(car new-contents))
		,list)
	     (let ((list-evaled (gensym)))
	       `(let ((,list-evaled ,list))
		  (setf (car ,list-evaled) ,(car new-contents))
		  ,list-evaled))))
	(t
	 (let* ((eval-needed? (not (symbolp list)))
		(new-list (if eval-needed? (gensym) list))
		(current-cons (gensym)))
	   `(let* (,@(if eval-needed? `((,new-list ,list)))
		   (,current-cons ,new-list))
	      (setf (car ,current-cons) ,(car new-contents))
	      ,@(loop with lines = nil
		      for element in (cdr new-contents)
		      do
			(push `(setq ,current-cons (cdr-of-cons ,current-cons))
			      lines)
			(push `(setf (car ,current-cons) ,element) lines)
		      finally
			(return (nreverse lines)))
	      ,new-list)))))



(defmacro destructuring-bind ((destructure-pattern value) &body forms)
  `(destructuring-bind-strict (,destructure-pattern ,value) ,@forms))




;;; The macro `destructuring-bind-forgiving' takes a (possibly dotted) cons tree
;;; of symbols, a value argument, and a body, and performs an efficient
;;; destructuring bind of the given symbols to the values in the corresponding
;;; positions of the list returned by the value argument.  If any of the symbols
;;; in the binding list are NIL, then that corresponding value will not be bound
;;; to any symbol.  Note that this is more efficient than evaluating setqs of
;;; the first, second, etc. of the value list, in that the spine conses of the
;;; list are only traversed once.

;; Note that at the present time this expands into a set of nested LET forms.
;; This could be improved upon if all bindings could be made in a single LET*.
;; This would allow the first form of the body to be declarations about the
;; generated variable bindings.  -jra 7/27/89

(defmacro destructuring-bind-forgiving ((destructure-pattern value) &body forms)
  (if destructure-pattern
      (let ((list-var (gensym)))
	`(let ((,list-var ,value))
	   ,@(destructure-expand
	       (list list-var) (list destructure-pattern) forms)))
      `(progn ,value ,@forms)))

(defun-for-macro destructure-expand (list-var-stack pattern-stack body-forms)
  (let* ((list-var (car list-var-stack))
	 (pattern (car pattern-stack))
	 (pattern-element (if (consp pattern) (car pattern))))
    (cond ((null pattern-stack)
	   body-forms)
	  ((null pattern)
	   (destructure-expand
	     (cdr list-var-stack) (cdr pattern-stack) body-forms))
	  ((symbolp pattern)
	   `((let ((,pattern ,list-var))
	       ,@(destructure-expand
		   (cdr list-var-stack) (cdr pattern-stack) body-forms))))
	  ((null pattern-element)
	   (setf (car pattern-stack) (cdr pattern))
	   (cons `(setq ,list-var (cdr ,list-var))
		 (destructure-expand
		   list-var-stack pattern-stack body-forms)))
	  ((symbolp pattern-element)
	   (setf (car pattern-stack) (cdr pattern))
	   (if (and (cdr pattern) (symbolp (cdr pattern)))
	       `((let ((,pattern-element (car ,list-var))
		       (,(cdr pattern) (cdr ,list-var)))
		   ,@(destructure-expand
		       (cdr list-var-stack) (cdr pattern-stack) body-forms)))
	       `((let ((,pattern-element (car ,list-var)))
		   ,@(if (cdr pattern)
			 `((setq ,list-var (cdr ,list-var))))
		   ,@(destructure-expand
		       list-var-stack pattern-stack body-forms)))))
	  ((consp pattern-element)
	   (let ((sub-list-var (gensym)))
	     (setf (car pattern-stack) (cdr pattern))
	     `((let ((,sub-list-var (car ,list-var)))
		 ,@(if (cdr pattern)
		       `((setq ,list-var (cdr ,list-var))))
		 ,@(destructure-expand (cons sub-list-var list-var-stack)
				       (cons pattern-element pattern-stack)
				       body-forms))))))))


;;; The macro `destructuring-setq' takes a (possibly dotted) cons tree of symbols,
;;; a value argument, and a body, and performs an efficient setq of the given
;;; symbols to the values in the corresponding positions of the list returned by
;;; the value argument.  If any of the symbols in the binding list are NIL, then
;;; that corresponding value will not be setq to any symbol.  Note that this is
;;; more efficient than evaluating setqs of the first, second, etc.  of the value
;;; list, in that the spine conses of the list are only traversed once.  The value
;;; returned by destructuring-setq is undefined.  Note that the syntax is closer
;;; to Common Lisp than destructuring-bind (cf. CLtL/2e p205).

;; jh per bah, 8/13/91.  Destructuring-setq is an adaptation of Jim's
;; destructuring-bind.

(defmacro destructuring-setq (destructure-pattern value)
  (if destructure-pattern
      (let ((list-var (gensym)))
	`(let ((,list-var ,value))
	   ,@(destructuring-setq-expand
	       (list list-var)
	       (list destructure-pattern))))
      nil))

(defun-for-macro destructuring-setq-expand (list-var-stack pattern-stack)
  (let* ((list-var (car list-var-stack))
	 (pattern (car pattern-stack))
	 (pattern-element (if (consp pattern) (car pattern))))
    (cond ((null pattern-stack)
	   nil) 
	  ((null pattern)
	   (destructuring-setq-expand
	     (cdr list-var-stack) (cdr pattern-stack)))
	  ((symbolp pattern)
	   `((setq ,pattern ,list-var)
	     ,@(destructuring-setq-expand
		 (cdr list-var-stack) (cdr pattern-stack))))
	  ((null pattern-element)
	   (setf (car pattern-stack) (cdr pattern))
	   (cons `(setq ,list-var (cdr ,list-var))
		 (destructuring-setq-expand
		   list-var-stack pattern-stack)))
	  ((symbolp pattern-element)
	   (setf (car pattern-stack) (cdr pattern))
	   (if (and (cdr pattern) (symbolp (cdr pattern)))
	       `((setq ,pattern-element (car ,list-var))
		 (setq ,(cdr pattern) (cdr ,list-var))
		 ,@(destructuring-setq-expand
		     (cdr list-var-stack) (cdr pattern-stack)))
	       `((setq ,pattern-element (car ,list-var))
		 ,@(if (cdr pattern)
		       `((setq ,list-var (cdr ,list-var))))
		 ,@(destructuring-setq-expand
		     list-var-stack pattern-stack))))
	  ((consp pattern-element)
	   (let ((sub-list-var (gensym)))
	     (setf (car pattern-stack) (cdr pattern))
	     `((let ((,sub-list-var (car ,list-var)))
		 ,@(if (cdr pattern)
		       `((setq ,list-var (cdr ,list-var))))
		 ,@(destructuring-setq-expand 
		     (cons sub-list-var list-var-stack)
		     (cons pattern-element pattern-stack)))))))))






;;; `Build-ab-symbol' is a defun-for-macro which provides a short building
;;; symbols in macros.  Constructs like (intern (format nil ...)) are replaced
;;; with (build-symbol ...).  For example
;;;   (let ((name 'foo)
;;;         (n 12)))
;;;     (build-symbol "fast-Stuff" name :bar N))
;;;    --> FAST-STUFF-FOO-12
;;; Symbols are interned in package AB, and everything is put in uppercase.

;; This must be defined after funcall is finished.

(defun-for-macro build-ab-symbol (&rest components)
  (intern
    (with-output-to-string (output)
      (loop for first = t then nil
	    for component in components
	    do
	(unless first
	  (write-char #\- output))
	;; Uppercase each component.
	(format output "~@:(~a~)" component)))
    (find-package "AB")))

(defun-for-macro build-ab-symbol-no-hyphens (&rest components)
  (intern
    (with-output-to-string (output)
      (loop for component in components do
	;; Uppercase each component.
	(format output "~@:(~a~)" component)))
    (find-package "AB")))


;;; `inline-tree-eq' is a predicate that encapsulates many varieties
;;; of cons-tree comparison without introducing a function-call and
;;; improving readablility of complex tests.  It also supports
;;; wildcarding through use of the symbol '*.

(defun-for-macro expand-inline-tree-eq (var tree)
  (cond ((consp tree)
	 `(and (consp ,var)
	       (let ((the-cons ,var))
		 (and
		   (progn
		     (setq ,var (car-of-cons the-cons))
		     ,(expand-inline-tree-eq var (car tree)))
		   (progn
		     (setq ,var (cdr-of-cons the-cons))
		     ,(expand-inline-tree-eq var (cdr tree)))))))
	((null tree)
	 `(null ,var))
	((eq tree '*)
	 t)
	(t                   ; this includes the fixnum case - see JRA
	 `(eq ,var ',tree))))
  
(defmacro inline-tree-eq (thing tree)
  `(let ((thing ,thing))
     ,(expand-inline-tree-eq 'thing tree)))








;;;; Atomic Operations



;;; As-atomic-operation guarantees that the body will be executed atomically.
;;; That is, so long as all possibly conflicting code that has side affects
;;; on shared global data structures is performed within an as-atomic-operation
;;; form, there should be no inconsistent results.  As-atomic-operation forms
;;; may be nested.  

;;; Any operations that alter plists of symbols (i.e. "setf of get" operations),
;;; or any other global structures for that matter, should be performed as
;;; atomic operations.

;;; For Lisps without multiple processes (the majority), this form does nothing.
;;; On Lisps with multiple processes (within the same address space), this form
;;; should guarantee that no interruption of body will happen once it has
;;; started executing.

(defmacro as-atomic-operation (&body body)
  `(progn ,@body))



(defmacro make-hash-table (&key (test nil) (size nil)
				(rehash-size nil) (rehash-threshold nil))
  `(ab-lisp::make-hash-table
     ,@(if test
	   `(:test ,(cond ((member test '('eq #'eq) :test #'equal)
			   '#'ab-lisp::eq)
			  ((member test '('eql #'eql) :test #'equal)
			   '#'ab-lisp::eql)
			  ((member test '('equal #'equal) :test #'equal)
			   '#'ab-lisp::equal)
			  ((member test '('equalp #'equalp) :test #'equal)
			   '#'ab-lisp::equalp)
			  (t
			   (error "Unrecognized hash-table test ~s" test))))
	   nil)
     ,@(if size `(:size ,size) nil)
     ,@(if rehash-size `(:rehash-size ,rehash-size) nil)
     ,@(if rehash-threshold `(:rehash-threshold ,rehash-threshold) nil)
     ))



;;;; Output recording (all #+development).


#+development
(progn
(defvar output-recording-id-table (make-hash-table) "Table of object->id")
(defvar output-recording-object-table (make-hash-table) "Table of id->object"))
;; Break out to eliminate a TLT cascade.

#+development
(progn
(defvar output-recording-enabled t)
(defvar output-recording-next-id 1000)
(defvar output-recording-minimum-id output-recording-next-id)


;;; "Exported" functions.

(defun enable-output-recording ()
  "Remember complex lisp objects whenever they are printed."
  (setq output-recording-enabled t))
  
(defun disable-output-recording ()
  "Stop remembering lisp objects as they are printed."
  (setq output-recording-enabled nil))

(defun clear-output-history ()
  "Clear all output histories, so that their object may be GCed.  Notice that
 this means that if an object is printed again, it will acquire a new ID."
  ;; We notice invalidated ID's by keeping the counter going over clear's.
  (setf output-recording-minimum-id output-recording-next-id)
  (lisp:clrhash output-recording-id-table)
  (lisp:clrhash output-recording-object-table))

(defun show-output-history (&optional (how-many 20))
  (loop for id from (- output-recording-next-id 1) by -1
	repeat how-many
	for object = (output-recording-get-object id)
	when object
	  do (format t " [~d] ~s~%" id object))
  (values))

    
;;; "Internal" functions.

(defun output-recording-get-object (id)
  (values (gethash id output-recording-object-table)))

(defun output-recording-get-id (object)
  (or
    (gethash object output-recording-id-table)
    (output-recording-store-object object)))

(defun output-recording-store-object (object)
  (let ((id output-recording-next-id))
    (setf (gethash id output-recording-object-table) object
	  (gethash object output-recording-id-table) id)
    (incf output-recording-next-id)
    id))

(defun abbrev-of-internal-number (frame)
  (modf (output-recording-get-id frame) 100))

)




;;;; Printing random objects.



;;; The macro `printing-random-object' ensures that random objects printed
;;; follow the #< ... @id> notation while recording output, and as #< ...
;;; pointer> otherwise.

(defmacro printing-random-object ((object stream) &body body)
  `(let ((.object. ,object)
	 (.stream. ,stream))
     (print-random-object-prefix .object. .stream.)
     ,@body
     (print-random-object-suffix .object. .stream.)))





;;; The function `%pointer' returns the address of the header of the given lisp
;;; object, as an integer, possibly a bignum.

(defmacro %pointer (lisp-object)
  (cond ((eval-feature :translator)
	 `(tli::pointer-as-fixnum ,lisp-object))
	((eval-feature :lucid)
	 (list (intern "%POINTER" "SYS") lisp-object))
	(t
	 `(progn ,lisp-object 0))))

(defun print-random-object-prefix (object stream)
  (declare (ignore object))
  (write-string "#<" stream))

(defun print-random-object-suffix (object stream)
  #+Development
  (if output-recording-enabled
      (format stream " @~d>" (output-recording-get-id object))
      (format stream " ~x>" (%pointer object)))
  #-Development
  (progn
    (write-char #\space stream)
    (write-fixnum (%pointer object) 16 0 stream)
    (write-char #\> stream)))
