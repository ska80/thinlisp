(in-package "TL")

;;;; Module FORWARD

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






;;;; Forward Reference Declarations




;;; This module implements utilities for declaring that a variable or function
;;; used in this file will be defined at a later time.






;;;; Declare-Forward-Reference




;;; `Declare-forward-reference' causes the compiler not to worry about forward
;;; references in the current module.
;;;
;;; The syntax is
;;;
;;;    (declare-forward-reference . forward-reference-spec)
;;;
;;; where forward-reference-spec is
;;;
;;;    (symbol {type-of-reference}+ [module])
;;;
;;; Type-of-reference is either VARIABLE or FUNCTION. If forward-reference is
;;; used as a variable in the current module, and variable is among the elements
;;; of types-of-references, then declare-forward-reference will cause the
;;; compiler to consider that symbol to be special.  If forward-reference is
;;; used as a function in the current module, and function is among the
;;; types-of-references then that symbol will be given a dummy functional
;;; binding (if it does not already have one).  None of the arguments are
;;; evaluated, and they should all be symbols.
;;;
;;; Module is a symbol that is neither FUNCTION nor VARIABLE.
;;; Declare-forward-reference makes some checks for inconsistent module
;;; declarations.  It is optional.

;;; `Declare-forward-references' allows a list of forward reference specs.  If
;;; the first form in a declare-forward-references is a list whose car is
;;; :suppress-module-checks-for, and the system being loaded is a member of the
;;; cdr of that list, then module checking will not occur.  This is useful when
;;; you only want to stub out a function that a system never intends to call.
;;; In the future (jh, 10/21/93), this might replace artifacts like TW-PATCHES.

;;; `Declare-forward-references-to-module' lets you gang things together that
;;; are all for one module.  It also defaults type-of-reference to FUNCTION,
;;; exploiting the fact that these make up the vast majority of cases.

;; jh, 10/21/93.  The standin functions created by
;; stand-in-for-forwardly-referenced-function all call
;; signal-forward-reference so that we only have a single code constant for the
;; cerror string, rather than as many as there are forward references.
  
(defun-for-macro signal-forward-reference (name-of-function?)
  (if name-of-function?
      (cerror "Ignore this problem and proceed."
	      "The forwardly referenced function ~a is still undefined!"
	      name-of-function?)
      (cerror "Ignore this problem and proceed."
	      "This forwardly referenced function is still undefined!")))

(defun-for-macro stand-in-for-forwardly-referenced-function
    (&optional arg1 arg2 arg3 arg4 arg5	; add any number as needed
	       arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 
	       arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23)
  (declare
    (ignore arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12
	    arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23))
  (signal-forward-reference nil))

(defun-for-macro create-standin-for-forwardly-referenced-function (name-of-function)
  (if (member :development *features*)
      #'(#-lucid lambda
	 #+lucid lcl:named-lambda #+lucid stand-in-for-forwardly-referenced-function
	    (&optional arg1 arg2 arg3 arg4 arg5	; add any number as needed
		       arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 
		       arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23)
	  (declare
	    (ignore arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12
		    arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23))
	  (signal-forward-reference name-of-function))
      (symbol-function 'stand-in-for-forwardly-referenced-function)))

(defun-for-macro declare-forwardly-referenced-function (forward-reference)
  (when (not (fboundp forward-reference))
    (setf (symbol-function forward-reference)
	  (create-standin-for-forwardly-referenced-function
	    forward-reference))))

(defvar-for-macro allowed-declare-forward-reference-types
  '(variable function))




;;; The macro `declare-forward-reference-simple' is an internal abstraction
;;; designed to ease the task of adding functionality to
;;; declare-forward-references.  Its arguments are the result of parsing the
;;; arguments to a forward declaration spec.  Declare-forward-reference-simple
;;; should not be used outside of this module (currently BOOTSTRAP).

;; jh, 10/21/93.  Modified declare-forward-reference-simple to generate a
;; standin function for forwardly referenced functions that reports the name of
;; the function.  This involves creating closures, but only in development.

(defmacro declare-forward-reference-simple
    (forward-reference
      variable-reference-p
      function-reference-p
      declared-module?)
  (when declared-module?

    ;; We don't have the following now in TLT.  -jra 6/27/96
    ;(check-for-an-actually-existing-module declared-module?)

    ;; Check that, if the declaration mentions a module, previous declarations
    ;; for the same symbol did not mention a different one.
    (flet ((check-module-consistency (property type-present-p type-name)
	     (when type-present-p
	       (let ((previously-declared-module?
		       (get forward-reference property)))
		 (when previously-declared-module?
		   (unless (eq declared-module?
			       previously-declared-module?)
		     (warn "DECLARE-FORWARD-REFERENCE: ~(~S~) ~S,~%~
                            previously declared to be in module ~S,~%~
                            now declared to be in module ~S"
			   type-name
			   forward-reference
			   previously-declared-module?
			   declared-module?)))))))
      (check-module-consistency
	'module-defining-forwardly-referenced-variable?
	variable-reference-p
	'variable)
      (check-module-consistency
	'module-defining-forwardly-referenced-function?
	function-reference-p
	'function)))

  `(progn
     
     ;; Handle forwardly-referenced variables if necessary.
     ,@(if (and variable-reference-p
		(not (eval-feature :translator)))
	   `((declaim (special ,forward-reference))
	     (eval-when (compile load eval)
	       (ab-lisp::proclaim '(ab-lisp::special ,forward-reference))))
	   nil)
     ,@(if (and variable-reference-p declared-module?)
	   `((eval-when (compile)
	       (setf (get ',forward-reference
			  'module-defining-forwardly-referenced-variable?)
		     ',declared-module?)))
	   nil)

     ;; Handle forwardly-referenced functions if necessary.
     ,@(cond
	 ((eval-feature :translator)
	  nil)
	 (t (if function-reference-p
		`((eval-when (compile load eval)
		    (declare-forwardly-referenced-function ',forward-reference)))
		nil)))
     ,@(if (and function-reference-p declared-module?)
	   `((eval-when (compile)
	       (setf (get ',forward-reference
			  'module-defining-forwardly-referenced-function?)
		     ',declared-module?)))
	   nil)))



(defmacro declare-forward-reference
	  (forward-reference &rest types-of-references-and-possibly-module)

  ;; Destructure the arguments.
  (let* ((final-arg (car (last types-of-references-and-possibly-module)))
	 (declared-module?
	   (if (memq final-arg allowed-declare-forward-reference-types)
	       nil
	       final-arg))
	 (types-of-references?
	   (if declared-module?
	       (butlast types-of-references-and-possibly-module)
	       types-of-references-and-possibly-module))
	 (variable-reference-p (and (memq 'variable types-of-references?) t))
	 (function-reference-p (and (memq 'function types-of-references?) t)))

    ;; Check that the declaration contains at least one permissible
    ;; reference-type.
    (unless (and types-of-references?
		 (null (set-difference
			 types-of-references?
			 allowed-declare-forward-reference-types)))
      (warn "DECLARE-FORWARD-REFERENCE: ~
             ~S has a malformed set of reference types."
	    forward-reference))

    `(declare-forward-reference-simple
       ,forward-reference
       ,variable-reference-p
       ,function-reference-p
       ,declared-module?)))

(defmacro declare-forward-references (&rest forward-reference-specs)
  (unless (eval-feature :translator)
    (let* ((possible-suppression-spec (first forward-reference-specs))
	   (suppression-spec-p
	     (and (listp possible-suppression-spec)
		  (eq (first possible-suppression-spec)
		      :suppress-module-checks-for)))
	   (systems-to-suppress-module-checking-for?
	     (and suppression-spec-p
		  (rest possible-suppression-spec)))
	   (suppress-module-checking-p
	     systems-to-suppress-module-checking-for?
;           (or (not (boundp             ; usu. in-buffer compile case
;                      'current-system-being-loaded))
;               (member current-system-being-loaded
;                       systems-to-suppress-module-checking-for?))
	     )
	   (declarations nil))
      (do* ((specs-list
	      (if suppression-spec-p
		  (rest forward-reference-specs)
		  forward-reference-specs)
	      (cdr specs-list))
	    (spec (car specs-list) (car specs-list)))
	   ((null specs-list) `(progn ,@(nreverse declarations)))
	(if suppress-module-checking-p
	    (push `(declare-forward-reference-simple
		     ,(first spec)
		     ,(and (member 'variable spec) t)
		     ,(and (member 'function spec) t)
		     nil)
		  declarations)
	    (push `(declare-forward-reference ,@spec) declarations))))))




(defmacro declare-forward-references-to-module (module &rest references)
  `(declare-forward-references
     . ,(let ((result nil))
	  (dolist (reference references)
	    (push (if (atom reference)
		      `(,reference function ,module)
		      `(,@reference ,module))
		  result))
	  result)))

;; The somewhat byzantine syntax of declare-forward-reference is due in some
;; part to forward compatability problems.  In particular, MODULE has not been
;; required because it was not even possible to specify for the first 7 years
;; ('86-'93) of this macro's life.  The forms
;; declare-forward-references-for-modules and declare-forward-references are
;; newer (10/93), but are quickly gaining popularity.  (MHD 10/15/93)

;; TO DO: (now that we have module information)
;;
;; After loading each module, for each forward reference to that module, check
;; to make sure that the reference has been resolved by loading that module.
;; Complain if unresolved.  For functions, use fdefinedp.  There may not be a
;; portable "specialp" for variables -- just abstract from that and do the best
;; you can for each implementation.  (MHD 10/15/93)

;;   Changed to defvar. -pto 10sep90

;;   WHY?! -- Changed back.  Defvar initial bindings
;;     get made again when compiling in the buffer in
;;     Zmacs.  This could be dangerous.  The above
;;     is perfectly valid and exactly what we want as
;;     far as I can see. (MHD 1/7/91)

;; 'Round and 'round we go.  Chestnut cannot handle double defvars of variables.
;; If you have more than one defvar, it turns into more than one definition of
;; the same variable in the C files, which is an error in C.  Also, since
;; Chestnut does multiple passes over the source code, forward references are
;; never required.  This is why declare-forward-reference should be a no-op for
;; Chestnut, always.  The reason that Peng changed the proclaim special into a
;; defvar is that Chestnut ignores proclaim specials.  Peng must not have
;; realized that this would cause a bug because of double defvars.  -jra 2/1/91


;; Moved here from primitives.  -pto 10mar89


;;; `Find-dead-forward-references' warns of all symbols function bound to the
;;; forward reference stand in function.  It is called in G2-FINAL during the
;;; macro pass to assure (at least) that forward references to development only
;;; functions are guarded by #+development.

(defun-for-macro find-dead-forward-references ()
  #-lucid
  nil
  #+lucid
  (do-symbols (symbol (find-package 'ab))
    (let (symbol-function)
      (when (fboundp symbol)
	(when (and (typep (setf symbol-function (symbol-function symbol))
			  'system:procedure)
		   (eq (system:procedure-name symbol-function)
		       'stand-in-for-forwardly-referenced-function))
	  (unless (eq symbol 'stand-in-for-forwardly-referenced-function)
	    (warn "~S introduced in a forward referenced was never defined."
		  symbol)))))))

;; This works only in LUCID.  It is designed to be robust in spite of boostrap
;; loading multiple times, and hence redefining the symbol-function of
;; stand-in-for-forwardly-referenced-function.  See g2-final for call site.
;; - ben 3/9/94
