(in-package "CL-USER")

;;;; Module BOOT

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






;;;; Bootstrapping the Gensym Language Translator




;;; Loading this module will load bootstrapping functions for TLT, including
;;; compile-tlt, which you should call to complete the compiling and loading of
;;; the TLT system..  This file sets implementation specific switches, creates
;;; all needed packages, and defines compile-tlt.  Note that this does not occur
;;; through tlt:def-system, but through minimal code that is made and used only
;;; within this file.

;;; You should bootstrap your Lisp environment with the following code (which
;;; requries that the base level of your sandbox is the current working
;;; directory, e.g. /bt/jra/).

;;; (progn (load "tlt/lisp/boot") (compile-tlt) (compile-system <system>))






;;;; TLT System Support




;;; For bootstrapping purposes within this file, the parameter `tlt-modules',
;;; and the functions `compile-tlt' and `load-tlt' are defined.
;;; They provide the most minimal system defining support I can get away with to
;;; load TLT itself and to perform "make-like" minimal compiles and loads of
;;; this system.  Note that all files are expected to be within the same
;;; directory and that this will be the default directory when these functions
;;; are executed.

;;; The parameter `tlt-modules' contains a list of all lisp file modules in the
;;; TLT system, with the notable exception of this file BOOT.

(defparameter tlt-modules
  '(exports
    tli-util
    system
    destruct
    env
    types
    decls
    macros
    special
    defun
    defvar
    regions
    setf
    ; bit-pack
    ; clos
    defstruct
    backquote
    c-names
    c-files
    c-types
    c-expr
    c-decls
    c-state
    c-func
    c-type-util
    c-coerce
    trandata
    l-expr
    l-const
    l-stack
    l-trans
    symbols
    l-top
    tlt-foreign
    tlt-prim
    tlt-math
    tlt-out
    makefiles
    trans
    ))




;;; The function `compile-tlt' will compile and load all modules in TLT as
;;; necessary.  A module is compiled if the corresponding binary file for it
;;; does not exist or if the binary file isn't newer than the lisp file.  A
;;; module is loaded if the :tlt-load-date property of the module symbol is
;;; absent or if the file write date within that property is less than the file
;;; write date of the binary file.  Note that this file, BOOT, is handled
;;; specially, and that the value of the parameter tlt-modules is not read until
;;; after any new versions of BOOT have already been compiled and loaded.

(defun compile-tlt (&key recompile from (safe t))
  ;; If BOOT is not compiled, or the compile is out of date, compile and load
  ;; it.  Otherwise don't bother to compile or load it, since we are already
  ;; running within a function within that file, and so can assume that it has
  ;; been loaded.
  (write-line "Compiling and loading TLT...")
  (unwind-protect
       (progn
	 (if safe
	     (safest-compilations)
	   (fastest-compilations))
	 (loop with delete-from-preventer = from
	       for module in tlt-modules 
	       while recompile
	       do
	   (when (and delete-from-preventer
		      (string= (symbol-name delete-from-preventer) 
			       (symbol-name module)))
	     (setq delete-from-preventer nil))
	   (unless delete-from-preventer
	     (delete-tlt-module-binary module)))
	 (compile-load-tlt-module 'boot (and recompile (null from)) 
				  1 (1+ (length tlt-modules)))
	 ;; After loading BOOT, call this function to get into the
	 ;; newest compiled form.
	 (compile-tlt-modules recompile from))
    (safest-compilations)))

(defmacro with-deferred-warnings (&body forms) 
  #+clisp
  `(progn ,@forms)
  #-clisp
  `(with-compilation-unit () ,@forms))

(defun compile-tlt-modules (recompile from)
  (with-deferred-warnings
    (loop with *readtable* = (copy-readtable nil)
	with recompile-module? = (and recompile (null from))
	with total-modules = (1+ (length tlt-modules))
	for module in tlt-modules 
	for module-count from 2 do
      (when (and recompile
		 (null recompile-module?)
		 (string= (symbol-name from) (symbol-name module)))
	(setq recompile-module? t))
      (compile-load-tlt-module module recompile-module? 
			       module-count total-modules)))
  (let ((tli-compile-tlt (intern "COMPILE-TLT" "TLI"))
	(tl-compile-tlt (intern "COMPILE-TLT" "TL")))
    (unless (fboundp tli-compile-tlt)
      (setf (symbol-function tli-compile-tlt)
	(symbol-function 'compile-tlt)))
    (unless (fboundp tl-compile-tlt)
      (setf (symbol-function tl-compile-tlt)
	(symbol-function 'compile-tlt)))))

(defconstant lisp-file-type 
   #-aclpc "lisp"
   #+aclpc "lsp")

(defconstant binary-file-type 
    #+lucid                    "sbin"
    #+aclpc                    "acl"
    #+allegro                  "fasl"
    #+cmu                      "x86f"
    #+mcl                      "pfsl"
    #+clisp                    "fas"
    #-(or lucid aclpc allegro cmu mcl clisp) "bin")




;;; The function `fastest-compilations' will set the optimize flags to get the
;;; fastest code out of compilations.  Typically this will be the case for TLT
;;; itself.  The function `safest-compilations' sets the optimize flags for
;;; safest code, at the expense of speed.  This will typically be used for
;;; systems defined within TL.

(defun fastest-compilations ()
  (pushnew :fastest-tlt *features*)
  (proclaim
;    '(optimize
;      (compilation-speed 3)
;      (speed 1)
;      (safety 3))
    '(optimize
;      #+cmu (c::brevity 1)
      (compilation-speed 0)
      (speed 3)
      (safety 0)
      )
    ))

(defun safest-compilations ()
  (setq *features* (delete :fastest-tlt (the list *features*)))
  (proclaim '(optimize
	      #+cmu (debug 3)
;	      #+cmu (c::brevity 3)
	      (compilation-speed #-cmu 3 #+cmu 2)
	      (speed #-cmu 1 #+cmu 0)
	      (safety 3)
	      )))





;;; The macro `finalize-pathname' takes a pathname and performs any work on
;;; that pathname which might be required by particular Lisp
;;; implementations.  For example, LOAD in Allegro CL for Windows doesn't
;;; work well with relative directory paths, so all pathnames are best
;;; merged with the defaults BEFORE being used.

(defun finalize-pathname (pathname)
;  #+allegro
;  (merge-pathnames pathname)
;  #-allegro
  pathname)




;;; The variable `exports-file-write-date' contains the file write date of the
;;; file tlt/lisp/exports.lisp.  Any Lisp, C, or TLT file that is not compiled
;;; up to date with this file will be recompiled.  Since exports of the TL
;;; package are included here, this is a reasonable precaution and is a nice
;;; feature -- it gives us a way to force full recompiles of everything.

(defvar exports-file-write-date nil)

(defun compile-load-tlt-module (module force-recompile? count total)
  (let* ((file-name (string-downcase (symbol-name module)))
	 (lisp-file 
	   (finalize-pathname
	     (make-pathname :directory '(:relative "tlt" "lisp")
			    :name file-name
			    :type lisp-file-type)))
	 (bin-file 
	  (finalize-pathname (make-pathname
			      :directory 
			      #-clisp-old '(:relative "tlt" "dev")
			      #+clisp-old '(:relative "tlt" "lisp")
			      :name file-name
			      :type binary-file-type)))
	 (relative-bin-file 
	  #+lucid
	  (finalize-pathname (make-pathname
			      :directory '(:relative :up "dev")
			      :name file-name
			      :type binary-file-type))
	  #-lucid
	  bin-file)
	 (lisp-date (and (probe-file lisp-file)
			 (file-write-date lisp-file)))
	 (bin-date? (and (probe-file bin-file)
			 (file-write-date bin-file)))
	 (load-date? (get module :tlt-load-date)))
    #+clisp-old
    (declare (ignore relative-bin-file))
    (when (null lisp-date)
      (warn "Module ~a does not have a corresponding lisp file ~a."
	    module lisp-file))
    (ensure-directories-exist bin-file :verbose nil)
    (when (eq module 'exports)
      (setq exports-file-write-date lisp-date))
    (when (or force-recompile?
	      (null bin-date?)
	      (and lisp-date
		   (<= bin-date? lisp-date))
	      (and exports-file-write-date
		   (<= bin-date? exports-file-write-date)))
      ;; The following weird construction forces line output buffering.
      (write-string (format nil "Compiling   ~40a    [~3d/~3d] ~%" lisp-file count total))
      (force-output)
      (compile-file lisp-file #-clisp-old :output-file #-clisp-old relative-bin-file
		    :verbose nil :print nil)
      (setq bin-date? (file-write-date bin-file)))
    (when (or (null load-date?)
	      (/= load-date? bin-date?))
      ;; The following weird construction forces line output buffering.
      (write-string (format nil "Loading     ~40a    [~3d/~3d] ~%" bin-file count total))
      (force-output)
      (load bin-file :verbose nil)
      (setf (get module :tlt-load-date) bin-date?))))




;;; The function `delete-tlt-module-binary' takes a symbol naming a TLT module.
;;; If the binary file for that module exists, it will be deleted.  This is used
;;; when recompiling, so that a failed recompile can't play gotcha with old
;;; binary files when you attempt to continue compiling after fixing a bug.

(defun delete-tlt-module-binary (module)
  (let* ((file-name (string-downcase (symbol-name module)))
	 (bin-file 
	  (finalize-pathname 
	   (make-pathname :directory '(:relative "tlt" "lisp" "dev")
			  :name file-name
			  :type binary-file-type))))
    (when (probe-file bin-file)
      (format t "Deleting ~a~%" bin-file)
      (delete-file bin-file))))

			  
	  






;;;; Lisp Implementation Specific Switches




;;; Set the default float format to doubles.

(setq *read-default-float-format* 'double-float)




;;; Set the print-case to :upcase.  This is supposed to be the default value,
;;; but ACL Win 3.02 defaults it to :downcase, which breaks many of my symbol
;;; generating macros.

(setq *print-case* :upcase)




 ;;; For Lucid, we suppress messages about every file being read or created with
;;; the :file-messages option.  We suppress the messages about which compiler is
;;; being used with the :optimize-message option.  All other options are left at
;;; their defaults as documented in the Lucid 4.0 User's Guide, pp. 6-13 through
;;; 6-15.

#+lucid
(lcl:compiler-options :file-messages nil :optimize-message nil)




;;; The variable lcl::*redefinition-action* controls whether or not warnings are
;;; issued for redefined functions.  We will suppress these until we can find a
;;; way for our forward reference declarations to stop causing these bogus
;;; warnings.  -jallard, 6/4/97

#+lucid
(setq lcl::*redefinition-action* nil)




;;; The following features control the temporary area implementations in the
;;; Lisp directories.

#+lucid
(pushnew :using-egc *features*)
#+lucid
(pushnew :no-lucid-temporary-areas *features*)




;;; The Lucid global variable `*load-verbose*' controls whether the load
;;; function issues messages by default every time it loads a file (LCL 4.0
;;; Advanced Users Guide, p. 7-55.  By default it does give messages, here we
;;; turn it off.  Note that all of the calls to load in bootstrap currently
;;; override the default with the :verbose keyword, though calls to load from
;;; this file do not.

;;; The Allegro global variable lisp:*load-verbose* performs the same function.

#+lucid
(setq lcl::*load-verbose* nil)

#+(or aclpc allegro cmu)
(setq *load-verbose* nil)

#+(or allegro cmu)
(setq *compile-verbose* nil
      *compile-print* nil)

#+cmu
(setq *GC-VERBOSE* nil)

#+allegro
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)




;;; The following prevents a CLISP features of compile-time evaluating calls to
;;; package operations with constant arguments.  I.e., the default setting of
;;; this variable would create a package at compile time for the following code.
;;;
;;;   (defun never-call-me ()
;;;     (when (hell-freezes-over)
;;;        (make-package "PACKAGE-OF-THE-APOCALYPSE")))
;;;
;;; I'll turn off this switch.  -jallard 5/28/01

#+clisp
(setq custom:*package-tasks-treat-specially* nil)





;;; The following sets up the standard printer debugger settings for Lucid.

#+lucid
(setq *print-level* nil
      *print-length* nil
      lcl:*debug-print-level* 5
      lcl:*debug-print-length* 20)

#-lucid
(setq *print-level* nil
      *print-length* nil)




;;; The following grows the memory in one chunk so that incremental growth
;;; doesn't have to happen as much.  The number of bytes given should be split
;;; up amongt the various allocation pools for different Lisps so that the total
;;; process size approaches the number given.

(defvar memory-expanded-limit 0)

(defun expand-memory-to-limit (bytes)
  (unless (>= memory-expanded-limit bytes)
    ;; Within Lucid, use two-thirds of the memory for reserved space, and then use
    ;; the other third split amongst the two hemispheres.  -jallard 10/30/97
    #+lucid
    (let ((target-blocks (ceiling bytes 65536))
	  (lcl:*gc-silence* t))
      (lcl:change-memory-management
	;; Kill Lisp if larger than 384 Meg.
	:growth-limit 8000
	;; Grow 3 Meg at a time (2 hemispheres * 25 seqments * 64Kbytes).
	:growth-rate 25
	;; Add one sixth of the expansion to the default in each hemisphere.
	:expand (floor target-blocks 6)
	;; Only expand if there is less than 30% free after a GC, default is 33%
	:reclamation-ratio 0.3
	;; Grow the reserved area by 25 segments at a time, i.e. 1.2 Meg.
	:reserved-growth-rate 25
	;; Add 10.2 Meg reserve to the 7 Meg already reserved.
	:expand-reserved (floor (* target-blocks 2) 3))
      ;; Set the ephemeral level sizes to 1 Meg, 1.2 Meg, and 1.2 Meg to
      ;; attempt to get lots of garbage reclaimed here.  This is double the
      ;; default sizes.  -jra 1/6/92
      (lcl:egc-options :level-sizes '(16 20 20)))

    #+cmu
    (progn 
      (setq ext:*bytes-consed-between-gcs* bytes)
      (setq ext:*gc-verbose* t))

    (setq memory-expanded-limit bytes)))

(expand-memory-to-limit 7500000)




;;; The following form suppresses warnings in ACL about differences between
;;; CLtL1 and CLtL2 in eval-when handling of some kinds of top level
;;; forms.  Doc can be found on pp. 7-33 to 7-34 of the ACL 4.3 Volume 1
;;; User Manual.

;;; Turning these warnings back on for now.  -jallard 5/10/99

#+(and allegro ignore)
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)






;;;; Default Directories and File Types




;;; The function `change-default-directory' is used to set the default
;;; directory.  In some systems this is other than just setq'ing
;;; *default-pathname-defaults*.  The function `default-directory' returns
;;; what the system thinks is the default directory, when that it is
;;; different from *default-pathname-defaults*.

(defun change-default-directory (string-or-pathname)
  (setq *default-pathname-defaults* (probe-file (pathname string-or-pathname)))
  #+lucid
  (lcl:cd *default-pathname-defaults*)
  #+allegro
  (excl:chdir *default-pathname-defaults*)
  #+cmu
  (unix:unix-chdir *default-pathname-defaults*)
  #+clisp
  (cd *default-pathname-defaults*)

  *default-pathname-defaults*)

#-clisp
(defun default-directory ()
  #+lucid
  (lcl::pwd)
  #+allegro
  (excl:current-directory)
  #+cmu
  (multiple-value-bind (a b)
      (unix:unix-current-directory)
    (declare (ignore a))
    b)
  #-(or lucid allegro cmu)
  *default-pathname-defaults*
  )

#-clisp
(defun cd (string-or-pathname)
  (change-default-directory string-or-pathname))

(defun pwd ()
  (default-directory))





;;;; Features




;;; The :tl feature is pushed onto features to represent that this translator
;;; has been loaded into the environment.

(pushnew :tl *features*)




;;; Set up features that describe the Lisp version and machine that we are
;;; currently running on.  These are largely inherited from the old features set
;;; up in lisp/load.lisp.

#+(and sun sparc)
(pushnew :sun4 *features*)

#+(and lucid :lcl3.0)
(pushnew :lucid-3 *features*)

#+(and lucid :lcl4.0)
(pushnew :lucid-4 *features*)






;;;; Packages




;;; The TLI (ThinLisp Internals) package is used to implement the translator.
;;; Users of the translator can find all of its interfacing functions within the
;;; TLT (ThinLisp Translator) package.  The language implemented by this
;;; translator is found in the TL (ThinLisp) package.  All symbols
;;; exported from these packages are found in the module EXPORTS.

(defun make-package-if-necessary (name use-list)
  (unless (find-package name)
    (make-package name :use use-list)))

(make-package-if-necessary "TLI"     '("LISP"))
(make-package-if-necessary "TLT"     nil)
(make-package-if-necessary "TL"      nil)
(make-package-if-necessary "AB-LISP" '("LISP"))
(make-package-if-necessary "TL-USER" '("TL"))

(unless (fboundp (intern "COMPILE-TLT" (find-package "TLI")))
  (setf (symbol-function (intern "COMPILE-TLT" (find-package "TLI")))
	(symbol-function 'compile-tlt)))




;;; The symbol `defmacro' must be shadowed for Allegro, since they don't
;;; seem to get the eval-when semantics right.  Sigh, here we go again.

(defmacro defmacro-replacement (name arglist &body decls-and-forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmacro ,name ,arglist ,@decls-and-forms)))

(defun install-replacement-defmacro ()
  (let ((tli-package (find-package "TLI"))
	(ab-lisp-package (find-package "AB-LISP"))
	(replacement (macro-function 'defmacro-replacement)))
    (shadow '(DEFMACRO) tli-package)
    (shadow '(DEFMACRO) ab-lisp-package)
    (setf (macro-function (find-symbol "DEFMACRO" tli-package))
          replacement)
    (setf (macro-function (find-symbol "DEFMACRO" ab-lisp-package))
          replacement)
    nil))

#+(and allegro ignore)
(install-replacement-defmacro)
