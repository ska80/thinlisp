(in-package "TLI")

;;;; Module REGIONS

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






;;;; Memory Regions




;;; This module contains operations for specifying and manipulating memory
;;; regions within TL applications.

;;; The function `region-number-for-type-and-area' takes a Lisp type and an area
;;; name, one of either tl:permannent or tl:temporary.  This function returns an
;;; integer that is the region number for creating the given type in the given
;;; area.  Note that this number is only used for error checking at runtime to
;;; verify that the consing area is what the declaration said it should be.

(defun region-number-for-type-and-area (lisp-type area)
  (cond ((eq area 'tl:temporary)
	 (unless (eq lisp-type 'double-float)
	   (translation-warning
	     "Attempting to allocate a ~s in a temporary area.  Only floats can go there."
	     lisp-type))
	 2)
	((eq area 'tl:permanent)
	 (if (eq lisp-type 'symbol)
	     1
	   0))
	((eq area 'tl:either)
	 -1)
	(t
	 (translation-error "Bad area name ~s." area))))




;;; The function `declared-area-name' takes an environment and a Lisp type and
;;; returns the symbol naming the current area declared within the environment.
;;; If none is visible, this function issues a translation warning and returns
;;; tl:permanent.

(defun declared-area-name (env type-to-allocate)
  (let ((area? (tl:declaration-information 'tl:consing-area env)))
    (if area?
	area?
      (let ((enclosing-function? (tl:declaration-information 'scope-name env)))
	(cond ((null enclosing-function?)
	       'tl:permanent)
	      ((function-decl enclosing-function? 'tl:conser)
	       'tl:either)
	      (t
	       (translation-warning
		"Consing a ~s with no surrounding consing-area declaration."
		type-to-allocate)
	       'tl:permanent))))))




;;; The function `check-area-for-conser-call' is called when translating a call
;;; to a function that conses into the current area (i.e. a conser).  If there
;;; is no current area declaration, this will issue a warning.

(defun check-area-for-conser-call (conser-function-name env)
  (let ((area? (tl:declaration-information 'tl:consing-area env)))
    (unless (or area?
		(null (tl:declaration-information 'scope-name env)))
      (translation-warning
       "Calling a consing function, ~a, without a surrounding consing-area declaration."
       conser-function-name))
    nil))




;;; The macros `with-temporary-area' and `with-permanent-area' are implemented
;;; by rebinding the variables Current-region and Temporary-area-top.  These
;;; variables are defined in TL.

(def-tl-macro tl:with-temporary-area (&body forms)
  `(tl:let* ((current-region
	       ,(region-number-for-type-and-area 'double-float 'tl:temporary))
	     (temporary-area-top temporary-area-top))
     (tl:declare (tl:consing-area tl:temporary))
     ,@forms))

(def-tl-macro tl:with-permanent-area (&body forms)
  `(tl:let* ((current-region
	       ,(region-number-for-type-and-area 'double-float 'tl:permanent)))
     (tl:declare (tl:consing-area tl:permanent))
     ,@forms))
