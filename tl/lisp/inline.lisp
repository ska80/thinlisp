(in-package "TL")

;;;; Module INLINE

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
;;; Author: Glenn Iba






;;;; Functions that are to be inlined via compiler macros




;;; This module contains basic function definitions that are needed early on,
;;; and that are normally to be inlined via compiler macros.





;;; Note that the implementation of `eql' can rely primarily on EQ tests,
;;; but only needs to compare values of numbers in the case where they are both
;;; double floats.  Since fixnums and characters are immediate, EQ is an
;;; accurate test for these types.

(declaim (functional eql))

(defun eql (a b)
  (declare (return-type t))
  ;; The silly if wrapper here puts the translation of this function into a
  ;; required type of boolean, which translates better than the general value
  ;; returning translations of AND and OR.  -jra 2/23/96
  (if (or (eq a b)
	  (and (typep a 'double-float)
	       (typep b 'double-float)
	       (= (the double-float a) (the double-float b))))
      t
      nil))




;;; The variable `tli::symbol-plist-of-nil' is used by the translations for
;;; symbol-plist.

(defvar tli::symbol-plist-of-nil nil)




;;; The function make-gensymed-symbol is defined in packages, since it needs to
;;; be after format.

(defmacro gensym (&optional string-or-number)
  `(the symbol 
	,(if (tli::eval-feature :translator)
	     `(make-gensymed-symbol ,string-or-number)
	     (if string-or-number
		 `(lisp:gensym ,string-or-number)
		 `(lisp:gensym)))))




;;; The following operations implement the built-in list searching facilities,
;;; including optimizations for the standard EQ, EQL, and EQUAL tests.

(defmacro my-identity (x)
  x)




;;; The macro `substitution-function-p' is a predicate used to declare that a
;;; symbol was defined using the def-substitution mechinism.  This knowledge is
;;; useful to code walkers which then know they may treat the forms as function
;;; like.

(defmacro substitution-function-p (name)
  `(get ,name 'substitution-function-p))
