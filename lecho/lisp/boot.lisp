(in-package "TL-USER")

;;;; Module BOOT

;;; Copyright (c) 1999 The ThinLisp Group
;;; Copyright (c) 1999 Jim Allard
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






;;;; Bootstrapping Lecho




;;; The `lecho' system is a simple, Lisp-based echo program that is primarily
;;; here to demonstrate the typical use of declare-system, package creation, and
;;; main functions with the TL translator.

;;; Typically, your systems should have a boot.lisp file in this position.  If
;;; TL is attempting to find information about a system and currently has no
;;; information about it, it will load a filename of the form
;;; <sys-name>/lisp/boot.lisp.  If that doesn't exist or doesn't contain the
;;; desired declare-system, then it will attempt to load
;;; lisp/<sys-name>-boot.lisp.

;;; Typically this file should be in the TL-USER package, and you can create any
;;; further packages you might want here.  For example's sake, I'm going to make
;;; a LECHO package that will be used for the remaining file(s) in this system.

(unless (find-package "LECHO")
  (make-package "LECHO"))

(declare-system (lecho :main-function tl-user::main)
  boot
  echo)
