(in-package "TLI")

;;;; Module TLT-FOREIGN

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1997 Gensym Corporation.
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






;;;; Primitive Operations for TL Foreign Function Call Out and Call In




;;; This module implements the macros for defining interfaces to hand-written C
;;; functions.  There are three facilities here, first one for defining call-out
;;; interfaces from Lisp to C foreign functions.  The second for defining
;;; call-out interfaces from Lisp to C macros (providing a Lisp macro to use
;;; instead during Lisp development).  The third is a function for linking
;;; libraries against the set of foreign functions that have been defined, but
;;; not yet bound to C functions within the Lisp environment.

;;; The macro `def-tl-foreign-function' defines a Lisp function that can be
;;; called in order to call a C function.  When expanding during translation
;;; this macro has the side-effect of registering the given C name as the
;;; identifier for the Lisp symbol.

(defmacro tl:def-tl-foreign-function (lisp-name-and-key-forms
				      &rest arg-names-and-types)
  (let* ((lisp-name (first lisp-name-and-key-forms))
	 (key-forms (rest lisp-name-and-key-forms))
	 (language-form (assq :language key-forms))
	 (name-form (assq :name key-forms))
	 (return-type-form (assq :return-type key-forms)))
    `(def-tl-foreign-function-required-args
	 (,lisp-name
	    (:language ,(if language-form
			    (second language-form)
			    :c))
	    (:name ,(if name-form
			(second name-form)
			nil))
	    (:return-type ,(if return-type-form
			       (second return-type-form)
			       nil)))
	 ,@arg-names-and-types)))

(defmacro def-tl-foreign-function-required-args ((lisp-name
						   (&key (language :c))
						   (&key name)
						   (&key return-type))
						 &rest arg-names-and-types)
  (unless (eq language :c)
    (error "TL can only translate foreign calls to :C, not ~s" language))
  (let ((expansion
	  `(tl:progn
	     (tl:declaim (tl:ftype
			   (tl:function
			     ,(loop for (nil type) in arg-names-and-types
				    collect (rewrite-foreign-keyword-type-names
					      type))
			     ,(rewrite-foreign-keyword-type-names return-type))
			   ,lisp-name))
	     (tl:declaim (foreign-c-identifier ,lisp-name ,name)))))
;    (format t "~%~%Made ~s~%" expansion)
    expansion))

(defparameter keyword-to-lisp-type-alist
  '((:fixnum                 . (c-type "long"))
    (:fixnum-long            . (c-type "long"))
    (:fixnum-int             . (c-type "long"))
    (:long                   . (c-type "long"))
    (:signed-32bit           . fixnum)
    (:unsigned-32bit-pointer . (c-type (pointer "uint32")))
    (:char-pointer           . (c-type (pointer "char")))
    (:string                 . (c-type (pointer "char")))
    (:simple-string          . (c-type (pointer "char")))
    (:double-float           . double-float)
    (:8-bit-unsigned-array   . (array (unsigned-byte 8)))
    (:16-bit-unsigned-array  . (array (unsigned-byte 16)))
    (:16-bit-signed-array    . (array (signed-byte 16)))
    (:object                 . t)
    (:void                   . void)))

(defun rewrite-foreign-keyword-type-names (type)
  (or (and (keywordp type)
	   (cdr (assq type keyword-to-lisp-type-alist)))
      type))




;;; The variable `underlying-def-foreign-callable' holds the symbol naming this
;;; port's implementation of def-foreign-callable.  If this variable
;;; returnscontains NIL, then no attempt will be made to enable this feature in
;;; Lisp development.

(defvar underlying-def-foreign-callable
  #+lucid
  'lcl:def-foreign-callable
  #+(and cmu verify)
  'alien:def-alien-routine
  #-(or lucid (and cmu verify))
  nil)

(defun underlying-lisp-callable-type (type)
  #+lucid
  (or (cdr (assq type '((:fixnum-long            . :fixnum)
			(:fixnum-int             . :fixnum)
			(:object                 . :pointer)
			(:string                 . :simple-string)
			(:unsigned-32bit-pointer . (:pointer :unsigned-32bit))
			(:char-pointer           . (:pointer :char))
			)))
      type)
  #+(and cmu verify)
  (or (cdr (assq type '((:fixnum-long            . '(alien (signed 64)))
			(:fixnum-int             . '(alian (signed 32)))
			(:object                 . '(alian '(* void)))
			(:string                 . '(alien '(* (unsigned 16))))
			(:unisgned-32bit-pointer . '(alien '(* (unsigned 32))))
			(:char-pointer           . '(alien '(* char))))))
      type)
  #-lucid
  type)




;;; The macro `tli::def-foreign-callable' is written to be compatible with the
;;; Lucid and Chestnut implementations.  It defines the function in question,
;;; using the same type transforms as are used for foreign-functions.  Note that
;;; this is NOT being made into an exported symbol of TL until the Lisp code has
;;; been rewritten to be able to handle it.

(defmacro tli::def-foreign-callable
    ((lisp-name &rest key-value-pairs)
     args
     &body decls-and-forms)
  (let* ((return-type
	   (or (second (assq :return-type key-value-pairs))
	       (error ":return-type unspecified in def-foreign-callable")))
	 (name (second (assq :name key-value-pairs)))
	 (transformed-return-type
	   (rewrite-foreign-keyword-type-names return-type))
	 (transformed-args
	   (loop for (arg-name arg-type) in args
		 collect (list arg-name
			       (rewrite-foreign-keyword-type-names arg-type))))
	 (translating? (eval-feature :translator))
	 (lisp-implementation? underlying-def-foreign-callable))
    `(tl:progn
       ,@(unless translating?
	   `((tl:declaim
	       (tl:ftype
		 (tl:function ,(loop for arg-spec in transformed-args
				     collect (second arg-spec))
			      ,transformed-return-type)
		 ,lisp-name))))
       ,@(unless (or translating? (null name))
	   `((tl:declaim
	       (function-c-identifier ,lisp-name ,name))))
       ,(cond
	   ((and (not translating?) lisp-implementation?)
	    `(,lisp-implementation?
		(,lisp-name
		   (:return-type ,(underlying-lisp-callable-type return-type))
		   ,@(if name `((:name ,name))))
		,(loop for (arg-name arg-type) in args
		       collect `(,arg-name ,(underlying-lisp-callable-type arg-type)))
		,@decls-and-forms))
	   (t
	    `(tl:defun ,lisp-name ,(loop for (arg-name) in args collect arg-name)
	       (tl:declare
		 (tl:return-type ,transformed-return-type)
		 ,@(loop for (arg-name arg-type) in transformed-args
			 collect `(tl:type ,arg-type ,arg-name)))
	       ,@decls-and-forms))))))






;;;; Inlineable Pseudo Functions


(def-tl-macro tl:def-inlined-pseudo-function-with-side-effects
    (lisp-name args &body lisp-body)
  (unless (eval-feature :translator)
    (let* ((specs? (and (consp lisp-name) lisp-name))
	   (given-return-type (if specs? (second specs?) :object))
	   (lisp-return-type
	     (rewrite-foreign-keyword-type-names given-return-type))
	   (lisp-name (if specs? (car specs?) lisp-name))
	   (c-name
	     (or (and specs? (third specs?))
		 (string-downcase (substitute #\_ #\- (symbol-name lisp-name)))))
	   (given-arg-types
	     (loop for arg in args collect (if (atom arg) :object (second arg))))
	   (lisp-arg-types
	     (loop for type in given-arg-types
		   collect (rewrite-foreign-keyword-type-names type)))
	   (arg-names
	     (loop for arg in args collect (if (atom arg) arg (car arg))))
	   )
      `(def-c-translation ,lisp-name ,arg-names
	 ((lisp-specs :ftype (,lisp-arg-types ,lisp-return-type))
	  (list* 'let
		 (list ,@(loop for name in arg-names
			       collect `(list ',name ,name)))
		 ',lisp-body))
	 ((trans-specs :c-type (,(loop for type in lisp-arg-types
				       collect (c-type-for-lisp-type type))
				 ,(c-type-for-lisp-type lisp-return-type)))
	  (make-c-function-call-expr
	    (make-c-name-expr ,c-name)
	    (list ,@arg-names)))))))

(def-tl-macro tl:def-inlined-pseudo-function (lisp-name args &body lisp-body)
  `(tl:progn
     (tl:declaim (tl:side-effect-free
		   ,(if (consp lisp-name) (car lisp-name) lisp-name)))
     (tl:def-inlined-pseudo-function-with-side-effects ,lisp-name ,args
       ,@lisp-body)))


;;; The macro `def-c' is the simplest interface to defining an interface to
;;; calling a C function from Lisp.  This macro takes the name of the new Lisp
;;; macro to be defined, and then it takes the elements of a C extern statement
;;; in the order, return type, C name string, and the rest being argument types.

;;; For example, to define an interface to the C ceil() function, you would use
;;; the following form.
;;;   (def-c ceil double "ceil" double)
;;;

