(in-package "TL")

;;;; Module TL-TIME

;;; Copyright (c) 2000-2001 The ThinLisp Group
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






;;;; Time Functions



;;; This module contains support for the time related functions of Common Lisp.
;;; For now I'll start with the internal real time and sleeping functions.

(declaim (inline get-internal-real-time
		 get-internal-run-time))

(def-inlined-pseudo-function-with-side-effects (init-cronometer :void) ()
  nil)

(def-inlined-pseudo-function-with-side-effects (cronometer :fixnum) ()
  (tli::get-internal-real-time))

(init-cronometer)

(defun get-internal-real-time ()
  (declare (return-type fixnum))
  (cronometer))

(def-inlined-pseudo-function (cpu-run-time :fixnum) ()
  (tli::get-internal-run-time))

(defun get-internal-run-time ()
  (declare (return-type fixnum))
  (cpu-run-time))

(def-inlined-pseudo-function (ticks-per-second :fixnum) ()
  tli::internal-time-units-per-second)

(declaim (type fixnum internal-time-units-per-second))

(defparameter internal-time-units-per-second (ticks-per-second))

(def-inlined-pseudo-function-with-side-effects (sleep-ticks :void) ((sleep-seconds :fixnum))
  (tli::sleep (/e (coerce-to-double-float sleep-seconds)
		  (coerce-to-double-float tli::internal-time-units-per-second))))

(defun sleep (seconds)
  (declare (return-type t)
	   (type double-float seconds))
  (sleep-ticks (floore-first (*e seconds
				 (coerce-to-double-float 
				  internal-time-units-per-second))))
  nil)
