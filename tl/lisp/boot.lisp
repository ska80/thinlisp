(in-package "TL")

;;;; Module BOOT

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






;;;; Bootstrapping TL




;;; Loading this module will define the TL system, which implements the
;;; translated primitives in TL, ThinLisp.  TL stands upon TLT, the ThinLisp
;;; Translator, which should already have been loaded when this file is loaded.

(declare-system (tl :library t :used-systems nil
		    :extra-c-files ("tl")
		    :extra-h-files ("tl"))
  boot
  stubs
  tl-types
  inline
  tl-prim
  do
  format
  input
  tl-basics
  loop
  apply
  generic-math
  generic-prim
  packages
  tl-util
  versions
  forward
  tl-extension
  tl-time)
