(in-package "TL")

;;;; Module VERSIONS

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






;;;; Platform Type and System Versions Information




;;; This module contains utilities for determining the current machine type that
;;; is running and for representing ThinLisp package version number information
;;; and it contains utilities for finding out information about loaded systems.






;;;; Systems Information




;;; This section contains functions that fetch information about systems.  This
;;; information has been set up by the translation time expansion of
;;; declare-system.

(defvar tl-user-package (find-package "TL-USER"))

(defun normalize-system-name (symbol)
  (declare (type symbol symbol)
	   (return-type symbol))
  (let ((tl-package-name (if (not (eq (symbol-package symbol) tl-user-package))
			     (intern (symbol-name symbol) tl-user-package)
			     symbol)))
    (or (get tl-package-name :nicknames-to)
	tl-package-name)))

(defun normalize-module-name (symbol)
  (declare (type symbol symbol)
	   (return-type symbol))
  (if (not (eq (symbol-package symbol) tl-user-package))
      (intern (symbol-name symbol) tl-user-package)
      symbol))




;;; The function `system-modules' takes a symbol naming a system and returns the
;;; load-ordered list of symbols naming modules within that system.  The
;;; function `system-nicknames' returns a list of nickname symbols.  The
;;; function `system-alias' returns a symbol alias for the given system if any
;;; was defined, else it returns the system name.  The function
;;; `system-used-systems' returns a list of the systems that are directly used
;;; by this system.  The function `system-all-used-systems' returns a
;;; load-ordered list of all systems that make up the given system.  Note that
;;; the given system name is always included as the last element of this list.

(defun system-modules (system-name)
  (declare (type symbol system-name)
	   (return-type list))
  (get (normalize-system-name system-name) :system-modules))

(defun system-alias (system-name)
  (declare (type symbol system-name)
	   (return-type symbol))
  (let ((name (normalize-system-name system-name)))
    (or (get name :alias)
	name)))

(defun system-nicknames (system-name)
  (declare (type symbol system-name)
	   (return-type list))
  (get (normalize-system-name system-name) :system-nicknames))

(defun system-used-systems (system-name)
  (declare (type symbol system-name)
	   (return-type list))
  (get (normalize-system-name system-name) :system-used-systems))

(defvar collected-systems nil)

(defun system-all-used-systems (system-name)
  (declare (type symbol system-name)
	   (return-type list))

  (let* ((name (normalize-system-name system-name))
	 (cached-list? (get name :system-all-used-systems)))
    (or cached-list?
	(with-permanent-area
	 (let ((collected-systems nil))
	   (collect-all-used-systems name)
	   (setq collected-systems (nreverse collected-systems))
	   (setf (get name :system-all-used-systems) collected-systems)
	   collected-systems)))))

(defun collect-all-used-systems (name)
  (declare (consing-area either)
	   (return-type void))
  (loop for sub-system in (get name :system-used-systems) do
    (collect-all-used-systems sub-system))
  (pushnew name collected-systems :test #'eq))









;;;; Platform and Operating System Type




;;; The following constants identify all of the ports of TL translated software.
;;; The numbers in these constants must match up with the numbers found in the
;;; file ext/c/cprim.c, as defined just above the function
;;; cc_get_platform_code().

(defconstant i386-code         1)
(defconstant dos-code          2)
(defconstant aviion-code       3)
(defconstant sgi-code          4)
(defconstant sequent-code      5)
(defconstant next-code         6)
(defconstant decstation-code   7)
(defconstant masscomp-code     8)
(defconstant hp9000s300-code   9)
(defconstant hp9000s400-code  10)
(defconstant hp9000s700-code  11)
(defconstant hp9000s800-code  12)
(defconstant rs6000-code      13)
(defconstant sun3-code        14)
(defconstant sun4-code        15)
(defconstant sparcsol-code    16)
(defconstant alphavms-code    17)
(defconstant motorola-code    18)
(defconstant vms-code         19)
(defconstant stratus-code     20)
(defconstant harris-code      21)
(defconstant nec-code         22)
(defconstant alphaosf-code    23)
(defconstant alphant-code     24)
(defconstant intelnt-code     25)
(defconstant ncr-code         26)
(defconstant windows95-code   27)
(defconstant freebsd-code     28)
(defconstant linux386-code    29)
(defconstant macosx-code      30)




;;; The variable `g2-operating-system' holds a symbol naming the operating
;;; system of the computer this software is currently running on.  It will have
;;; one for four values: WIN32, UNIX, VMS, or DOS.  (Note that we currently have
;;; no DOS port, so it can actually only have one of the first three values.
;;; -jra 12/9/96)

(defvar g2-operating-system
  (let ((platform-code (tli::get-platform-code)))
    (declare (type fixnum platform-code))
    (cond
      ((= platform-code i386-code)         'unix)
      ((= platform-code dos-code)          'dos)
      ((= platform-code aviion-code)       'unix)
      ((= platform-code sgi-code)          'unix)
      ((= platform-code sequent-code)      'unix)
      ((= platform-code next-code)         'unix)
      ((= platform-code decstation-code)   'unix)
      ((= platform-code masscomp-code)     'unix)
      ((= platform-code hp9000s300-code)   'unix)
      ((= platform-code hp9000s400-code)   'unix)
      ((= platform-code hp9000s700-code)   'unix)
      ((= platform-code hp9000s800-code)   'unix)
      ((= platform-code rs6000-code)       'unix)
      ((= platform-code sun3-code)         'unix)
      ((= platform-code sun4-code)         'unix)
      ((= platform-code sparcsol-code)     'unix)
      ((= platform-code alphavms-code)     'vms)
      ((= platform-code motorola-code)     'unix)
      ((= platform-code vms-code)          'vms)
      ((= platform-code stratus-code)      'unix)
      ((= platform-code harris-code)       'unix)
      ((= platform-code nec-code)          'unix)
      ((= platform-code alphaosf-code)     'unix)
      ((= platform-code alphant-code)      'win32)
      ((= platform-code intelnt-code)      'win32)
      ((= platform-code ncr-code)          'unix)
      ((= platform-code windows95-code)    'win32)
      ((= platform-code freebsd-code)      'unix)
      ((= platform-code linux386-code)     'unix)
      ((= platform-code macosx-code)       'unix)

      ;; jh per jra, 8/26/93.  Made the "otherwise" clause more forgiving for
      ;; new ports.  Instead of an error, we assume that all new ports will be
      ;; to UNIX boxes.
      (t
       (format t "Unknown platform code ~a, assuming UNIX o/s" platform-code)
       'unix))))




;;; The global variable `g2-machine-type' holds a symbol naming the type of
;;; computer that this software is currently running on.

(defvar g2-machine-type
  (let ((platform-code (tli::get-platform-code)))
    (declare (type fixnum platform-code))
    (cond
      ((= platform-code i386-code)         'i386)
      ((= platform-code dos-code)          'dos)
      ((= platform-code aviion-code)       'aviion)
      ((= platform-code sgi-code)          'sgi)
      ((= platform-code sequent-code)      'sequent)
      ((= platform-code next-code)         'next)
      ((= platform-code decstation-code)   'decstation)
      ((= platform-code masscomp-code)     'masscomp)
      ((= platform-code hp9000s300-code)   'hp9000s300)
      ((= platform-code hp9000s400-code)   'hp9000s400)
      ((= platform-code hp9000s700-code)   'hp9000s700)
      ((= platform-code hp9000s800-code)   'hp9000s800)
      ((= platform-code rs6000-code)       'rs6000)
      ((= platform-code sun3-code)         'sun3)
      ((= platform-code sun4-code)         'sun4)
      ((= platform-code sparcsol-code)     'sparcsol)
      ((= platform-code alphavms-code)     'alphavms)
      ((= platform-code motorola-code)     'motorola)
      ((= platform-code vms-code)          'vms)
      ((= platform-code stratus-code)      'stratus)
      ((= platform-code harris-code)       'harris)
      ((= platform-code nec-code)          'nec)
      ((= platform-code alphaosf-code)     'alphaosf)
      ((= platform-code alphant-code)      'alphant)
      ((= platform-code intelnt-code)      'intelnt)
      ((= platform-code ncr-code)          'ncr)
      ((= platform-code windows95-code)    'windows95)
      ((= platform-code freebsd-code)      'freebsd)
      ((= platform-code linux386-code)     'linux)
      ((= platform-code macosx-code)       'macosx)
      ;; jh per jra, 8/26/93.  Made the "otherwise" clause more forgiving for
      ;; new ports.  Instead of an error, we return a special symbol.
      (t                                   'experimental-port))))




;;; The function `machine-model' returns a CL string containing a
;;; user-presentable ASCII name for the type of computer that this software is
;;; currently running on.  Note that the string returned contains a leading
;;; space.

(defvar machine-model-var nil)

(defun machine-model ()
  (if machine-model-var
      machine-model-var
      (setq machine-model-var
	    (case g2-machine-type
	      (aviion " Data General AViiON")
	      (motorola " Motorola 88000")
	      (next " NeXT")
	      (sgi " Silicon Graphics")
	      (sequent " Sequent")
	      (decstation " DECstation")
	      (masscomp " Concurrent 6000s")
	      (hp9000s300 " HP9000/300s")
	      (hp9000s400 " HP9000/400s")
	      (hp9000s700 " HP9000/700s")
	      (hp9000s800 " HP9000/800s")
	      ((i386 compaq) " '386") ;jh, 12/18/92.  Added i386.
	      (rs6000 " IBM POWERstation")
	      (sun3 " Sun 3")
	      (vms " Vax VMS")
	      (alphavms " DEC Alpha AXP/OpenVMS")
	      (sun4 " Sun 4/SPARCstation")
	      (sparcsol " Sun 4/SPARCstation Solaris")
	      (harris " Harris Night Hawk")
	      (stratus " Stratus FTX")
	      (nec " NEC EWS4800 Series")
	      (alphaosf " DEC OSF/1 AXP")
	      (alphant  " Alpha NT") ;jh, 1/25/94.  Needs official name.
	      (intelnt  " Intel NT") ;jh, 1/25/94.  Needs official name.
	      ;; jh per mpc, 5/27/94.  The following clause will have to be
	      ;; modified as our DOS-based PC ports proliferate.
	      (dos "MS Windows 3.1")
	      (ncr " NCR 386/486") ;jh per mes.  Needs official name.
	      (windows95 " Windows 95") ;jh per mpc, 3/28/95.
	      (macosx " PowerPC MacOSX")
	      ;; jh per jra & mes, 8/26/93.  Made the "otherwise" clause more
	      ;; articulate for new ports.  The wording may be changed, but
	      ;; should produce a title block that will discourage illicit use
	      ;; of unreleased G2 ports.
	      (otherwise " Experimental Port")))))
