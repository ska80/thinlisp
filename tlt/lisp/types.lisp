(in-package "TLI")

;;;; Module TYPES

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






;;;; Lisp Type Manipulation




;;; The macro `tl:deftype' defines an expander from one type specification to
;;; another, where it is presumed that the type expanded into is a "simpler"
;;; type closer to the what is natively supported within the TL language.  Types
;;; declared with tl:deftype can be used anywhere that natively supported types
;;; can be used.

(defmacro declared-type-expansion (name)
  `(get ,name :declared-type-expander))

(defmacro tl:deftype (name arglist &rest body)
  (let ((type-expander (intern (format nil "~a-TYPE-EXPANDER" name))))
    (unless (eval-feature :translator)
      (multiple-value-bind (decls forms)
	  (split-declarations-and-body body)
	`(progn
	   (deftype ,name ,arglist ,@body)
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (defun ,type-expander ,arglist
	       ,@decls
	       (block ,name
		 ,@forms))
	     (setf (declared-type-expansion ',name) ',type-expander)))))))

(defun expand-type (type)
  (let* ((cons-type? (consp type))
	 (type-name (if cons-type? (cons-car type) type))
	 (expander? (declared-type-expansion type-name)))
    (setq type 
	  (if expander?
	      (expand-type
		(if cons-type?
		    (apply expander? (cons-cdr type))
		    (funcall expander?)))
	      type))
    (cond
      ((and (consp type)
	    (memqp (cons-car type) '(array simple-array)))
       (if (cons-cdr type)
	   `(,(cons-car type) ,(expand-type (cons-cadr type)) ,@(cddr type))
	 type))
      ((and (consp type)
	    (memqp (cons-car type) '(and or not unsigned-byte)))
       (cons (cons-car type)
	     (loop for subtype in (cons-cdr type)
		 collect (if (integerp subtype)
			     subtype
			   (expand-type subtype)))))
      ((and (consp type)
	    (eq (cons-car type) 'function))
       (list 'function
	     (loop for arg-type in (cons-second type)
		   collect (expand-type arg-type))
	     (expand-type (cons-third type))))
      (t type))))




;;; The macro `explicit-lisp-to-c-type-p' returns whether or not a Lisp type is
;;; of the form (c-type x), which is an explicit interface within the Lisp type
;;; system to the C types.

(defmacro explicit-lisp-to-c-type-p (lisp-type)
  (let ((type (gensym)))
    `(let ((,type ,lisp-type))
       (and (consp ,type)
	    (eq (cons-car ,type) 'c-type)))))




;;; This module contains operations for manipulating types within TLT.  In many
;;; cases the operations used here will be based on the underlying Lisp's type
;;; system.  Note that these operations are not the TL supported operations for
;;; use in runtime systems.  Those will be supplied in the TLRTL system under
;;; the TL package, not the TLI package.

;;; The function `tl-subtypep' should be used instead of subtypep so we can fix
;;; problems in underlying Lisp implementations as needed.  Similarly, we should
;;; use `tl-typep' instead of typep and `tl-type-of' instead of type-of.

(defun tl-subtypep (subtype superior-type)
  (setq subtype (expand-type subtype))
  (setq superior-type (expand-type superior-type))
  (cond ((eq superior-type 'void)
	 (values t t))
	((eq superior-type '*)
	 (values
	   (type-includes-values-p subtype)
	   t))
	;; Types of the form (c-type "long") or (c-type (pointer "char")) are
	;; subtypes if the C type specifiers are equal.
	((or (explicit-lisp-to-c-type-p superior-type)
	     (explicit-lisp-to-c-type-p subtype))
	 (values
	   (and (consp superior-type)
		(consp subtype)
		(eq (cons-car superior-type) (cons-car subtype))
		(equal (cons-cadr superior-type) (cons-cadr subtype)))
	   t))
	((and (consp superior-type)
	      (eq (cons-car superior-type) 'values))
	 (values
	   (or (and (eq subtype '*)
		    (loop for value-type in (cons-cdr superior-type)
			  always (tl-subtypep 't value-type)))
	       (and (consp subtype)
		    (eq (cons-car subtype) 'values)
		    (loop for superior-value-type in (cons-cdr superior-type)
			  for subtype-value-type-cons?
			      = (cons-cdr subtype)
			      then (cdr subtype-value-type-cons?)
			  always (tl-subtypep (or (car subtype-value-type-cons?)
						  'null)
					      superior-value-type))))
	   t))
	((or (memqp superior-type
		    '(unbound managed-float tl-string-stream file-stream))
	     (memqp subtype '(unbound)))
	 (values (eq subtype superior-type) t))
	;; Since all of our arrays are one-dimensional, all array types are
	;; sequences.  In CL, only vectors and simple-arrays are subtypes of
	;; sequence.
	((eq superior-type 'sequence)
	 (multiple-value-bind (result certainp)
	     (tl-subtypep subtype 'array)
	   (if result
	       (values result certainp)
	     (tl-subtypep subtype 'list))))
	((eq subtype 'void)
	 (values nil t))
	((eq subtype '*)
	 (tl-subtypep 't superior-type))
	((and (consp subtype)
	      (eq (cons-car subtype) 'values))
	 (tl-subtypep (or (second subtype) 'null) superior-type))
	((eq subtype 'managed-float)
	 (values (eq superior-type 't) t))
	((memqp subtype '(tl-string-stream file-stream))
	 (values (memqp superior-type '(stream t)) t))
	((and (consp superior-type)
	      (consp subtype)
	      (eq (cons-car superior-type) 'array)
	      (eq (cons-car subtype) 'array)
	      (consp (cons-cdr superior-type))
	      (consp (cons-cdr subtype)))
	 (values (equal (upgraded-tl-array-element-type (second superior-type))
			(upgraded-tl-array-element-type (second subtype)))
		 t))
	(t
	 (subtypep subtype superior-type))))

(defconstant most-positive-tl-fixnum 536870911)

(defconstant most-negative-tl-fixnum -536870912)

(defstruct (tl-string-stream)
  (strings nil)
  (input-string)
  (input-index)
  (input-index-bounds))

(defun tl-typep (object type)
  (setq type (expand-type type))
  (cond ((memqp type '(void * t))
	 t)
	((eq type 'unbound)
	 nil)
	;; Lucid doesn't implement the types string-stream or file-stream.  -jra
	;; 3/4/96 However, CMU Lisp and other ANSI CL implementations do, and on
	;; those Lisps the interpretation of the type string-stream was messing
	;; us up.  Instead, switch over to tl-string-stream, which is the
	;; structure type that will actually implement this functionality in TL.
	;; -jallard 5/27/99
	((eq type 'tl-string-stream)
	 (typep object 'tl-string-stream))
	((eq type 'managed-float)
	 (and (typep object '(array double-float))
	      (= (length object) 1)))
	#+lucid
	((eq type 'file-stream)
	 (and (typep object 'stream)
	      (not (typep object 'lcl:string-output-stream))))
	;; If we run on a non-30-bit fixnum implementation, comment this in.
	;; -jra 3/4/96
	;; CLISP is one such implementation.  -jallard 5/28/01
	#+clisp
	((eq type 'fixnum)
	 (or (typep object 'fixnum)
	     (and (typep object 'integer)
		  (<= object most-positive-tl-fixnum)
		  (>= object most-negative-tl-fixnum))))
	((and (consp type)
	      (eq (cons-car type) 'values)
	      (= (length type) 2))
	 (tl-typep object (cons-second type)))
	((explicit-lisp-to-c-type-p type)
	 nil)
	(t
	 (typep object type))))




;;; The macro `fixnump' checks if the given value is a fixnum in TL translated
;;; systems.

(defmacro fixnump (x)
  `(tl-typep ,x 'fixnum))

(defmacro tl-type-of (object)
  (let ((ob (gensym))
	(type (gensym)))
    `(let* ((,ob ,object)
	    (,type (type-of ,ob)))
       (if (and (or (eq ,type 'integer) (eq ,type 'bignum))
		(fixnump ,ob))
	   'fixnum
	 ,type))))




;;; The parameter `tl-optimizable-types' holds an alist of Lisp types to C type
;;; implementations to optimize the given type when that value is stored in a
;;; lexcial variable or appears as a result of an expression evaluation.

(defparameter tl-optimizable-types
  '(((unsigned-byte 8) . uint8)
    ((unsigned-byte 16) . uint16)
    ((signed-byte 16) . sint16)
    (fixnum . sint32)
    (double-float . double)
    (character . unsigned-char)
    ((array (unsigned-byte 8)) . (array uint8))
    ((array (unsigned-byte 16)) . (array uint16))
    ((array (signed-byte 16)) . (array sint16))
    ((array double-float) . (array double))
    (simple-string . (array unsigned-char))
    (string . (array unsigned-char))
    (simple-vector . (array Obj))))




;;; The parameter `tl-significant-types' is a list of Lisp types that are
;;; significant to the optimizations possible in TL.  This list can be added to
;;; arbitrarily, but it is included here so that the translator can collapse
;;; pathological combinations of ands, ors, and nots into a simple enough type
;;; spec that it can be handled easily.  Effectively this list is used to limit
;;; the type inference insanity possible within Common Lisp.

;;; Note that all types in this list must either have a single type tag or must
;;; have a special case within translate-type-check-predicate.

(defparameter tl-significant-types
  (nconc
    (loop for (type) in tl-optimizable-types
	  collect type)
    '(null
      cons
      list
      managed-float
      symbol
      compiled-function
      package
      tl-string-stream
      file-stream)))
      
      



;;; The function `most-specific-common-supertype' takes two types and returns a
;;; type that the most specific type that is superior to both given types.  It
;;; first attempts to use one or the other of the given types, and then searches
;;; through a set of types that TL knows how to optimize in runtime systems.  If
;;; all of this fails, this function returns T, the supertype of all types.
;;; This function is useful when combining the types from multiple expressions
;;; to determine the most specific type that could result from the evaluation of
;;; one or the other expression.

;;; When this function is used during the type combinations during a
;;; translation, there is no way to find a common supertype for types that
;;; differ in whether or not they set the values count.  In those cases this
;;; function will signal an error.  However, this function is also used before
;;; actual translations to try to find the best guess at Lisp function return
;;; types.  When used for this purpose, you can allow this function to upgrade
;;; from types that don't set the values count up to values count setting types
;;; by passing the optional ignore-values-mismatch argument.

(defun most-specific-common-supertype
    (type1 type2 &optional ignore-values-mismatch?)
  (cond ((equal type1 type2)
	 type1)
	((or (type-includes-values-p type1)
	     (type-includes-values-p type2))
	 (cond
	   ((or (not (type-includes-values-p type1))
		(not (type-includes-values-p type2)))
	    (cond
	      (ignore-values-mismatch? '*)
	      (t (error "Values setting mismatch attempting to combine ~s and ~s."
			type1 type2))))
	   ((or (eq type1 '*)
		(eq type2 '*)
		(/= (length type1) (length type2)))
	    '*)
	   (t
	    (cons 'values
		  (loop for subtype1 in (cons-cdr type1)
			for subtype2 in (cons-cdr type2)
			collect (most-specific-common-supertype
				  subtype1 subtype2))))))
	((tl-subtypep type1 type2)
	 type2)
	((tl-subtypep type2 type1)
	 type1)
	(t
	 (loop for type in tl-significant-types
	       when (and (tl-subtypep type1 type) (tl-subtypep type2 type))
	         return type
	       finally (return t)))))




;;; The function `duplicate-type-declaration' takes two types that have both
;;; been declared on the same variable, and returns a type sufficient for use in
;;; declaring this variable as the intersection of of the two types.  The
;;; trivial implementation of this operation would return the two types within
;;; an and, but that is not sufficient for optimization purposes.

(defun duplicate-type-declaration (type1 type2)
  (setq type1 (expand-type type1))
  (setq type2 (expand-type type2))
  (cond ((eq type1 '*)
	 type2)
	((eq type2 '*)
	 type1)
	((or (eq type2 'void) (tl-subtypep type1 type2))
	 type1)
	((or (eq type1 'void) (tl-subtypep type2 type1))
	 type2)
	(t
	 (loop for type in tl-significant-types
	       when (or (tl-subtypep type1 type) (tl-subtypep type2 type))
	         return type
	       finally (return t)))))




;;; The function `optimizable-type-of' takes a Lisp object and returns the most
;;; specific optimizable type that includes the given object.

(defun optimizable-type-of (object)
  (loop for type in tl-significant-types
	when (tl-typep object type)
	  return type
	finally (return t)))




;;; The function `upgraded-optimizable-type' takes a type and returns the most
;;; specific super type that is optimizable in TL.

(defun upgraded-optimizable-type (base-type)
  (loop with expanded-base-type = (expand-type base-type)
	initially (when (explicit-lisp-to-c-type-p expanded-base-type)
		    (return expanded-base-type))
	for type in tl-significant-types
	when (tl-subtypep expanded-base-type type)
	  return type
	finally (return t)))




;;; The function `element-type-of-vector-type' takes a Lisp vector type and
;;; returns the type of the elements of that vector.  If no specific type has
;;; been declared, then the element type is T.

(defun element-type-of-vector-type (lisp-vector-type)
  (if (consp lisp-vector-type)
      (second lisp-vector-type)
    t))




;;; The parameter `upgraded-array-element-type-alist' holds the alist of Lisp
;;; types array element types to their equivalent TL package Lisp types and
;;; optimized C array element types, representing all array type optimizations
;;; supported within TL.

(defparameter upgraded-array-element-type-alist
  `((character          tl:character          unsigned-char)
    ((unsigned-byte 8)  (tl:unsigned-byte 8)  uint8)
    ((unsigned-byte 16) (tl:unsigned-byte 16) uint16)
    ((signed-byte 16)   (tl:signed-byte 16)   sint16)
    (double-float       tl:double-float       double)
    (t                  tl:t                  obj)))




;;; The function `upgraded-tl-array-element-type' takes an element type for an
;;; array and returns the specific superior type that has a specialized array
;;; implementation.  Note that the macro tl:upgraded-array-element-type uses
;;; this function.

(defun upgraded-tl-array-element-type (element-type)
  (second (assoc (expand-type element-type) upgraded-array-element-type-alist
		 :test #'tl-subtypep)))




;;; The function `upgraded-struct-element-type' takes a Lisp type that is the
;;; declared element type for a structure or class, and returns the most
;;; specific superior Lisp type that has a specialized struct implementation.
;;; Note that this function does not handle bit-field conversions for
;;; unsigned-byte or signed-byte Lisp types.  These must be handled specially.

(defun upgraded-struct-element-type (element-type)
  (let ((expanded-type (expand-type element-type)))
    (if (and (consp expanded-type) (eq (cons-car expanded-type) 'integer))
	expanded-type
      (upgraded-tl-array-element-type element-type))))




;;; The function `struct-slot-c-type-for-lisp-type' is like c-type-for-lisp-type
;;; except that it will return integer and unsigned integer bit-fielded C types
;;; when the Lisp type indicates an appropriate integer subrange.

(defun struct-slot-c-type-for-lisp-type (lisp-type)
  (setq lisp-type (upgraded-struct-element-type lisp-type))
  (if (or (atom lisp-type)
	  (not (eq (car lisp-type) 'integer)))
      (c-type-for-lisp-type lisp-type)
    (let ((low-bound (second lisp-type))
	  (high-bound (third lisp-type)))
      (declare (fixnum low-bound high-bound))
      (if (>= low-bound 0)
	  `(uint ,(integer-length high-bound))
	`(int ,(1+ (max (integer-length low-bound) 
			(integer-length high-bound))))))))




;;; The function `c-type-for-lisp-type' takes a Lisp type and returns the C type
;;; that should be used in translations to represent it.  (See C-TYPES for a 
;;; description of C types.)

(defun c-type-for-lisp-type (lisp-type)
  ;; The cond clauses optimize the most commonly used cases.
  (setq lisp-type (expand-type lisp-type))
  (cond
    ((eq lisp-type 't) 'obj)
    ((eq lisp-type 'fixnum) 'sint32)
    ((eq lisp-type 'simple-vector) '(array obj))
    ((explicit-lisp-to-c-type-p lisp-type)
     ;; The C type for Lisp types of the form (c-type *) are EQ to that Lisp
     ;; type.  (Got you confused yet?  -jallard 7/7/97)
     (cond ((tl-subtypep lisp-type '(c-type "long"))
	    'long)
	   ((tl-subtypep lisp-type '(c-type (pointer "char")))
	    '(pointer char))
	   ((tl-subtypep lisp-type '(c-type (pointer "void")))
	    '(pointer void))
	   ((tl-subtypep lisp-type '(c-type (pointer "uint32")))
	    '(pointer uint32))
	   (t
	    lisp-type)))
    (t
     (loop for (optimizable-type . c-type) in tl-optimizable-types
	   do
       (when (tl-subtypep lisp-type optimizable-type)
	 (return c-type))
	   finally (return 'obj)))))



    
;;; The function `c-func-type-for-lisp-func-return-type-spec' takes a Lisp
;;; function return type specification as would be found in the ftype or
;;; computed-ftype specification, and returns the appropriate C function return
;;; type for a translation of that Lisp function.  Note that the returned C type
;;; is the type of only the first value returned by the C function, since all
;;; secondary values are returned as type Obj within the values-buffer global
;;; array.

(defun c-func-type-for-lisp-func-return-type-spec (lisp-type-spec)
  (cond
    ;; First deal with the non-Lisp types we use to describe return values not
    ;; expressible within the Lisp type system.

    ;; The return spec * is used for functions that return an unpredictable
    ;; number of values.
    ((eq lisp-type-spec '*)
     'obj)
    ;; The return spec void is used for functions that return no useful value.
    ((eq lisp-type-spec 'void)
     'void)
    ;; The return spec (values <type> ...) describes a specific number of
    ;; multiple values of given Lisp types, always implemented as C Obj types.
    ((and (consp lisp-type-spec) (eq (cons-car lisp-type-spec) 'values))
     'obj)
    ;; Otherwise it is a specific, potentially optimizable type.
    (t
     (c-type-for-lisp-type lisp-type-spec))))




;;; The function `satisfies-lisp-required-type-p' takes a Lisp return type and a
;;; Lisp required type.  This function returns whether or not the return type
;;; satisfies the required type or would need further type checking or coercion
;;; to provide the required type.  Note that this function is strict, that the
;;; return type must fully satisfy the type requirements of the required type.
;;; Also note that the requirement of multiple values means that the return type
;;; must either be * or a (values ...) type.  This function is used to determine
;;; whether or not further operations need to be wrapped around a value
;;; producing form to set the values count.

(defun satisfies-lisp-required-type-p (result-type required-type)
  (or (eq required-type 'void)
      (and (eq required-type 't) (not (eq result-type 'void)))
      (equal required-type result-type)
      (and (eq required-type '*)
	   (type-includes-values-p result-type))
      (and (consp required-type)
	   (eq (cons-car required-type) 'values)
	   (if (and (consp result-type)
		    (eq (cons-car result-type) 'values))
	       (loop for subtype in (cons-cdr required-type)
		     for result-type-cons = (cons-cdr result-type)
					  then (cdr result-type-cons)
		     for result-subtype = (or (car result-type-cons) 'null)
		     always (tl-subtypep subtype result-subtype))
	       (and (eq result-type '*)
		    (loop for subtype in (cons-cdr required-type)
			  always (tl-subtypep 't subtype)))))
      (and (not (values-of-t-type-p required-type))
	   (tl-subtypep (type-of-first-value result-type) required-type))))




;;; The function `values-of-t-type-p' takes a type specification and returns
;;; true if it is a (values t ...) type.  The function `type-of-first-value'
;;; takes a type and returns either the type, or if the given type returns
;;; multiple values, it returns the type of the first returned value.

(defun values-of-t-type-p (type)
  (and (consp type)
       (eq (cons-car type) 'values)
       (loop for subtype in (cons-cdr type)
	     always (eq subtype 't))))

(defun type-of-first-value (type)
  (cond ((atom type)
	 (if (eq type '*)
	     t
	     type))
	((eq (cons-car type) 'values)
	 (or (second type) 'null))
	(t type)))




;;; The function `type-includes-values-p' takes a Lisp type and returns whether
;;; or not the type specifies its multiple values requirements.

(defun type-includes-values-p (type)
  (or (eq type '*)
      (and (consp type)
	   (eq (cons-car type) 'values))))





;;; The function `default-init-for-lisp-type' takes a Lisp type and returns the
;;; default initial value for variables of the given type, where they have been
;;; given no explicit initial value.

(defun default-init-for-lisp-type (lisp-type)
  (cond ((tl-subtypep lisp-type 'fixnum) 0)
	((tl-subtypep lisp-type 'double-float) 0.0)
	((tl-subtypep lisp-type 'character) #\null)
	(t nil)))
