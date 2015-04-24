(in-package "CL-USER")

;;;; Module BOOT

;;; Copyright (c) 1999-2001 The ThinLisp Group
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




;;; Bootstrapping TL



;;; To bootstrap a ThinLisp translation environment, load this file into your
;;; favorite Lisp environment.  Uncomment and specialize the
;;; DEF-CONVENIENCE-FORMS code below, and then add any needed calls to
;;; translate-system that are needed to even the odds.

;;; ThinLisp consists of an extensible set of systems, each rooted in the src
;;; directory and organized into a directory tree similar to the following.

;;;   src/tlt/lisp/
;;;           dev/
;;;       tl/lisp/
;;;          dev/
;;;          macro/
;;;          c/
;;;          bin/
;;;       lecho/lisp/
;;;             dev/
;;;             macro/
;;;             c/
;;;             bin/

;;; The "tlt" system is the ThinLisp Translator, which is the base module of the
;;; entire system.  This contains a lisp directory and the directories for the
;;; Lisp binaries.  The "tl" system is the first translated system, and it
;;; contains Lisp implementations of Common Lisp facilities in its Lisp
;;; directory.  These are compiled into the dev or macro directories, and
;;; translations of these Lisp files are placed into the C directory.  Note that
;;; the c directory also contains the only handwritten C files in this sytem,
;;; tl.c and tl.h.  The compiled binaries for this handwritten and translated C
;;; files go into the bin directory.  This system is an example of a TL library
;;; system -- it translates into a C library.  The "lecho" system is shipped as
;;; a small example of a system that is translated and compiled into an
;;; executable.  This system will read all of its command line arguments, and
;;; echo them back to standard-output, much lish the Bourne shell "echo"
;;; command.

;;; Every system should have a "boot.lisp" file in its lisp directory.  Loading
;;; this file will define all characteristics required to run tl:compile-system
;;; and tl:translate-system.  The macro `def-system-convenience-forms' will
;;; define `load-xxx', `compile-xxx', and `translate-xxx' forms for any system
;;; name "xxx", which expand into calls to the basic needed operations.  In the
;;; forms below, add your system names to the list to generated all needed
;;; shortcuts.  Once you have bootstrapped ThinLisp into your Lisp development
;;; environment, then calling load-system, compile-system, or translate-system
;;; will attempt to find and load the <system>/lisp/boot.lisp file to bootstrap
;;; information about the system to be loaded.  This recurses through any needed
;;; systems of your system.

;;; The code below will load up the ThinLisp translator, then compile the TL
;;; system.  Feel free to add further def-convenience-forms calls or to add
;;; further code to compile/load/translate your system.

;;; Load the translator's boot file.

(load
 (merge-pathnames (make-pathname :directory '(:relative "tlt" "lisp") :name "boot")
		  (or *load-pathname* *default-pathname-defaults*)))

;; a less diverse world this would have read (load "tlt/lisp/boot"))



;;; Compile (and load) the translator.

(compile-tlt)



;;; Define convenience forms for all systems (add your own to this list).

(def-system-convenience-forms lecho)

;;; Translate your system (modify to compile or translate your system).

(translate-lecho)
