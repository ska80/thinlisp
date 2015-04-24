(in-package "TLI")

;;;; Module DEFVAR

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






;;;; Variable Definitions




;;; This module implements the macros `tl:defvar', `tl:defparameter', and
;;; `tl:defconstant'.  Defvar and defparameter define global variables that are
;;; translated into C variables.  Defconstant defines pointers to variable
;;; values that are always inlined into the referencing locations.

(defmacro tl:defvar (name &optional (initial-value no-initial-value)
			  (documentation nil))
  (declare (ignore documentation))
  `(tl:progn
     (tl:declaim (special ,name))
     (def-named-variable ,name :variable ,initial-value)))
			  
(defmacro tl:defparameter (name initial-value)
  `(tl:progn
     (tl:declaim (special ,name))
     (def-named-variable ,name :parameter ,initial-value)))

(defmacro tl:defconstant (name initial-value)
  `(tl:progn
     (tl:declaim (constant ,name))
     (def-named-variable ,name :constant ,initial-value)))




;;; The macro `def-translatable-lisp-var' is used to define a variable for TL on
;;; a symbol that is an underlying Lisp implementation variable.  An example is
;;; *features*.  We must use the Lisp package version so that the reader can
;;; interpret #+ reader macros appropriately, but we want to distinguish a use
;;; of this variable from a use of some other Lisp variable that we do not have
;;; a definition or translation for.  This macro does the trick, standing in for
;;; a defvar, as far as the translator is concerned, but while still using the
;;; previously defined defvar while in development.

(defmacro tl:def-translatable-lisp-var
    (name &optional (initial-value no-initial-value))
  `(tl:progn
     (tl:declaim (special ,name))
     (def-named-variable ,name :underlying-lisp-variable ,initial-value)))

(defmacro def-translatable-lisp-constant
    (name initial-value)
  `(tl:progn
     (tl:declaim (constant ,name))
     (def-named-variable ,name :underlying-lisp-constant ,initial-value)))
