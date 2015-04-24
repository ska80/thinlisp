(in-package "TL")

;;;; Module TL-TYPES

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






;;;; Type Operations for TL




;;; This module implements type predicates and declarations.  There are also
;;; two variables needed by the memory allocation primitives.

(defvar tli::current-region 0)

(defvar tli::temporary-area-top nil)

(deftype void () 'tli::void)

(deftype fixnum () 'tli::fixnum)

(defconstant fixnum-signed-byte-width 30)

(defconstant most-positive-fixnum 536870911)

(defconstant most-negative-fixnum -536870912)

(defmacro fixnump (object)
  `(typep ,object 'tli::fixnum))

(deftype null () 'tli::null)

(defmacro null (object)
  `(not ,object))

(deftype symbol () 'tli::symbol)

(defmacro symbolp (object)
  `(typep ,object 'tli::symbol))

(deftype atom () 'tli::atom)

(defmacro atom (object)
  `(not (consp ,object)))

(deftype cons () 'tli::cons)

(defmacro consp (object)
  `(typep ,object 'tli::cons))

(deftype list () 'tli::list)

(defmacro listp (object)
  `(typep ,object 'list))

(deftype number () 'tli::number)

(defmacro numberp (object)
  `(typep ,object 'tli::number))

(deftype integer (&optional low-bound high-bound)
  (tli::cond ((tli::or (tli::null low-bound) (tli::eq low-bound '*))
	      (tli::setq low-bound most-negative-fixnum))
	     ((tli::consp low-bound)
	      (tli::setq low-bound (tli::1+ (tli::car low-bound)))))
  (tli::cond ((tli::or (tli::null high-bound) (tli::eq high-bound '*))
	      (tli::setq high-bound most-positive-fixnum))
	     ((tli::consp high-bound)
	      (tli::setq high-bound (tli::1- (tli::car high-bound)))))
  `(tli::integer ,low-bound ,high-bound))

(defmacro integerp (object)
  `(typep ,object 'tli::fixnum))

(deftype unsigned-byte (bit-length)
  (tli::when (tli::>= bit-length fixnum-signed-byte-width)
    (tli::error "Type (unsigned-byte ~a) exceeds ThinLisp's fixnum range."
		bit-length))
  `(tli::integer 0 ,(tli::1- (tli::ash 1 bit-length))))

(deftype signed-byte (bit-length)
  (tli::when (tli::> bit-length fixnum-signed-byte-width)
    (tli::error "Type (signed-byte ~a) exceeds ThinLisp's fixnum range."
		bit-length))
  `(tli::integer ,(tli::- (tli::ash 1 (tli::1- bit-length)))
		 ,(tli::1- (tli::ash 1 (tli::1- bit-length)))))

(deftype float () 'tli::double-float)

(deftype single-float () 'tli::double-float)

(deftype short-float () 'tli::double-float)

(deftype long-float () 'tli::double-float)

; (deftype managed-float () 'tli::managed-float)

(deftype double-float () 'tli::double-float)

(defmacro floatp (object)
  `(typep ,object 'tli::double-float))

(deftype character () 'tli::character)

(deftype string-char () 'tli::character)

(defmacro characterp (object)
  `(typep ,object 'tli::character))

(deftype simple-vector () 'tli::simple-vector)

(defmacro simple-vector-p (object)
  `(typep ,object 'tli::simple-vector))

(deftype structure () 'tli::structure)

(defmacro structurep (object)
  `(typep ,object 'tli::structure))

(deftype string () 'tli::string)

(deftype simple-string () 'tli::string)

(defmacro stringp (object)
  `(typep ,object 'tli::string))

(defmacro simple-string-p (object)
  `(typep ,object 'tli::string))

(deftype package () 'tli::package)

(defmacro packagep (object)
  `(typep ,object 'tli::package))

(deftype compiled-function () 'tli::compiled-function)

(defmacro compiled-function-p (object)
  `(typep ,object 'tli::compiled-function))

;; ThinLisp does not have interpreted functions, so all functions are
;; compiled-functions.  Having a deftype for function is problematic since
;; tl:function is eq to cl:function.  The fact that the readtable generates this
;; symbol is a partial cause of this problem, since TL doesn't redefine the
;; readtable.  Punt for now.  -jallard 6/7/01

;; (deftype function () 'tli::compiled-function)

(defmacro functionp (object)
  `(typep ,object 'tli::compiled-function))

(deftype stream () 'tli::stream)

(defmacro streamp (object)
  `(typep ,object 'tli::stream))

(deftype string-stream () 'tli::tl-string-stream)

(deftype file-stream () 'tli::file-stream)

(deftype and (&rest types)
  `(tli::and ,@types))

(deftype or (&rest types)
  `(tli::or ,@types))

(deftype not (type)
  `(tli::not ,type))

(deftype values (&rest types)
  `(tli::values ,@types))

;;; All TL arrays are vectors and are considered simple-arrays.

(defun-for-macro expand-array-type (original-array-type array-specs)
  (if array-specs
      (let ((elt-type (car array-specs))
	    (dimensions (car (cdr array-specs))))
	(when (cdr (cdr array-specs))
	  (ab-lisp::error
	    "Malformed array type spec ~s"
	    (cons original-array-type array-specs)))
	(cond ((or (null dimensions)
		   (lisp:eql dimensions 1)
		   (lisp:equal dimensions '(*))
		   (lisp:equal dimensions '(tli::*)))
	       `(tli::array ,elt-type))
	      ((and (consp dimensions)
		    (null (cdr dimensions))
		    (fixnump (car dimensions)))
	       `(tli::array ,elt-type ,dimensions))
	      (t
	       (ab-lisp::error
		 "TL doesn't support multi-dimensional arrays in type ~s."
		 (cons original-array-type array-specs)))))
      'tli::array))

(deftype vector (&optional (elt-type t) (length '*))
  (expand-array-type 'vector `(,elt-type (,length))))

(deftype simple-array (&optional (elt-type t) (dimensions '(*)))
  (expand-array-type 'simple-array `(,elt-type ,dimensions)))

(deftype array (&optional (elt-type t) (dimensions '(*)))
  (expand-array-type 'array `(,elt-type ,dimensions)))

(deftype c-type (held-c-type)
  `(tli::c-type 
     ,(cond ((stringp held-c-type)
	     held-c-type)
	    ((and (consp held-c-type)
		  (consp (cdr held-c-type))
		  (stringp (car (cdr held-c-type))))
	     (let ((car (car (the cons held-c-type))))
	       (if (or (eq car 'pointer)
		       (eq car 'tli::pointer))
		   `(tli::pointer ,(car (cdr held-c-type)))
		   (ab-lisp::error
		     "Badly formed C-TYPE, ~s"
		     `(c-type ,held-c-type)))))
	    (t
	     (ab-lisp::error
	       "Badly formed C-TYPE, ~s"
	       `(c-type ,held-c-type))))))






;;;; Macro-time Subtyping Operations




;;; The following operations will only be available during macro-expand and
;;; compile time, and are not translatable runtime functions.

(defun-for-macro upgraded-array-element-type (type)
  (tli::upgraded-tl-array-element-type type))

(defun-for-macro subtypep (subtype superior-type &optional environment)
  (declare (ignore environment))
  (tli::tl-subtypep subtype superior-type))
