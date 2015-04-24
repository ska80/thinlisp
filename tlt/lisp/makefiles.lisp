(in-package "TLI")

;;;; Module MAKEFILES

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






;;;; Makefile Generation




;;; This module implements a simple makefile generator to bootstrap compilation
;;; of TL translations.

;;; The variable `makefile-element-alist' is an alist of symbols naming
;;; variables to be bound in a makefile, associated with the strings that should
;;; be used to initialize those variables.  The default settings here were made
;;; for compiling using the Cygnus tools on Windows with the environment
;;; variable MAKE_MODE set to UNIX.

(defparameter makefile-element-alist
    '((cc           . "gcc -o")
      (opt-flags    . "-O2 -fomit-frame-pointer")
      (debug-flags  . "-ggdb3")
      (thread-flags . "-DPTHREAD")
      (cc-flags     . "-pipe -ansi -pedantic -W -Wall -c")
      (wild         . "%")
      (stem         . "$*")
      (target       . "$@")
      (first-dep    . "$<")
      
      (archive      . "ar rsc")
      (link         . "gcc -o")
      (opt-link     . "-O2")
      (debug-link   . "-g")
      (system-libs  . "")

      (lib-prefix   . "lib")
      (lib-postfix  . ".a")
      (exe-prefix   . "")
      (exe-postfix  . ".exe")
      (obj-postfix  . ".o")
      ))




;;; The parameter `makefile-ports' is used to override settings of
;;; makefile-element-alist for various ports.  For each element of the
;;; makefile-ports list, a distinct makefile will be generated.  Each element of
;;; makefile-ports is a list with the folowing format.

;;;   ( port-names-list . makefile-element-alist-overrides )

;;; The port-names-list portion is a list of the name of this port, and the
;;; names of other defined ports which should provide defaults for this port.
;;; The ports later in the list will win out over overrides earlier in the list.

;;; For example, the element (("freebsd" "linux") (cc . "cc -o")) would specify
;;; that the makefile for FreeBSD should be like the makefile for Linux, except
;;; that "cc -o" should be used as the compile command, rather than gcc.

(defparameter makefile-ports
  '((("cygnus"))
    
    (("linux")
     (exe-postfix . "")
     (system-libs . "-lm"))

    (("freebsd" "linux"))

    (("macosx" "freebsd")
     (exe-postfix . "")
     (cc . "cc -o")
     (link . "$(CC)")
     (archive . "ar -r -c -u"))

    (("config")
     (cc          . "@CC@")
     (cc-flags    . "@CFLAGS@ -pipe -ansi -pedantic -W -Wall -c")
     )

    ))

(defun port-requires-ranlib-p (port-name)
  (string= "macosx" port-name))

(defvar makefile-elements nil)

(defun makeup (key)
  (or (cdr (assoc key makefile-elements))
      ""))

(defun generate-makefiles (system verbose)
  (loop for ((port-name . subports) . port-alist) in makefile-ports
	for makefile-elements 
	= (loop named portalist
		with alist = makefile-element-alist
		for name in subports
		for subalist = (loop named subportalist
				     for subport in makefile-ports
				     do
				 (when (string= name (cons-caar subport))
				   (return-from subportalist (cons-cdr subport))))
		do (setq alist (append subalist alist))
		finally (return-from portalist (append port-alist alist)))
	do
    (generate-makefile system verbose port-name)))




;;; The function generate-makefiles takes a system, and uses information from
;;; the system and from the makefile-ports file to generate a set of makefiles
;;; for the current ports.

(defun generate-makefile (system verbose port-name)
  (let ((path (system-makefile system port-name))
	(temp-path (system-temporary-makefile system))
	(bin-dir (system-bin-dir system))
	(optimized-bin-dir (system-optimized-bin-dir system))
	(current-year
	 (sixth (multiple-value-list
		 (decode-universal-time (get-universal-time)))))
	(target (if (system-is-library-p system)
		    (format nil "~a~(~a~)~a"
			    (makeup 'lib-prefix)
			    (system-name system)
			    (makeup 'lib-postfix))
		  (format nil "~a~(~a~)~a"
			  (makeup 'exe-prefix)
			  (system-name system)
			  (makeup 'exe-postfix))))
	(obj (makeup 'obj-postfix))
	(pattern (makeup 'wild))
	(files-per-line 5))
    (with-open-file (output temp-path :direction :output :if-exists :supersede)
      (format output "#~%# ~a ~a Makefile~%#~%# Copyright (c) ~a The ThinLisp Group~%~%"
	      (system-name system) (string-capitalize port-name) current-year)
      (format output "CC = ~a~%" (makeup 'cc))
      (format output "~%CFLAGS =~%")
      (format output "~%ifdef THREAD~%")
      (format output "CFLAGS += ~a~%" (makeup 'thread-flags))
      (format output "endif~%")
      (format output "~%ifdef OPT~%")
      (format output "CFLAGS += ~a~%" (makeup 'opt-flags))
      (format output "else~%")
      (format output "CFLAGS += ~a~%" (makeup 'debug-flags))
      (format output "endif~%")
      (format output "~%CFLAGS += ~a~2%" (makeup 'cc-flags))
      (cond ((system-is-library-p system)
	     (format output "ARCHIVE = ~a~%" (makeup 'archive)))
	    (t
	     (format output "LINK = ~a~%" (makeup 'link))
	     (format output "~%ifdef OPT~%")
	     (format output "LINKFLAGS = ~a~%" (makeup 'opt-link))
	     (format output "LIBS =")
	     (loop for subsystem in (butlast (system-all-used-systems system)) do
	       (tlt-write-char #\space output)
	       (relative-path-to-directory
		optimized-bin-dir (system-optimized-bin-dir (tl:find-system subsystem))
		output)
	       (format output "lib~(~a~).a" subsystem))	    
	     (format output "~%else~%")
	     (format output "LINKFLAGS = ~a~%" (makeup 'debug-link))
	     (format output "LIBS =")
	     (loop for subsystem in (butlast (system-all-used-systems system)) do
	       (tlt-write-char #\space output)
	       (relative-path-to-directory
		bin-dir (system-bin-dir (tl:find-system subsystem))
		output)
	       (format output "lib~(~a~).a" subsystem))
	     (format output "~%endif~2%")
	     (format output "SYSLIBS = ~a~%" (makeup 'system-libs))))
      (tlt-write-string "OBJECTS = " output)
      (loop for file-count = 1 then (1+ file-count)
	    for file-name in (system-extra-c-files system) do
	(when (= (mod file-count files-per-line) 0)
	  (format output " \\~%        "))
	(format output " ~a~a" file-name obj))
      (loop with file-count = (length (system-extra-c-files system))
	    for module in (system-modules system)
	    do
	(when (system-module-included-p system module)
	  (incf file-count)
	  (when (= (mod file-count files-per-line) 0)
	    (format output " \\~%       "))
	  (format output " ~a~a" (module-file-name-string module) obj)))
            
      (format output "~%~%all : ~a~%~%clean :~%" target)
      (tlt-write-char #\tab output)
      (format output "-rm *~a~%" obj)
      (tlt-write-char #\tab output)
      (format output "-( if [ -f ~a ] ; then rm ~a ; fi )~%~%" target target)
      
      (format output "~a : ~a $(OBJECTS) $(LIBS)~%" target (pathname-name path))
      (tlt-write-char #\tab output)
      (format output "-( if [ -f ~a ] ; then rm ~a ; fi )~%" target target)
      (tlt-write-char #\tab output)
      (cond ((system-is-library-p system)
	     (format output "$(ARCHIVE) ~a $(OBJECTS)~%" target)
	     (when (port-requires-ranlib-p port-name)
	       (tlt-write-char #\tab output)
	       (format output "ranlib ~a~%" target))
	     (format output "~%"))
	    (t
	     (format output "$(LINK) ~a $(LINKFLAGS) $(OBJECTS) " target)
	     (format output "$(LIBS) $(SYSLIBS)~%~%")))
      
      (format output "~a~a : ../c/~a.c ../c/~a.h ~a"
	      pattern obj pattern pattern (pathname-name path))
      (loop for file in (system-extra-h-files system) do
	(format output " ../c/~a.h" file))
      (tlt-write-char #\newline output)
      (tlt-write-char #\tab output)
      (format output "$(CC) ~a $(CFLAGS) -I ../c"
	      (makeup 'target))
      (unless (eq system (tl:find-system 'tl))
	(format output " -I")
	(relative-path-to-directory
	 bin-dir (system-c-dir (tl:find-system 'tl)) output))
      (format output " ~a~%" (makeup 'first-dep)))
    
    ;; If the newly created makefile is different from the existing one,
    ;; overwrite the existing one with the newly created temporary.
    (when (or (not (probe-file path))
	      (not (file-contents-equal temp-path path)))
      (when verbose
	(format t "~%Installing new C dir   makefile for ~a ~a" 
		(system-name system) (string-capitalize port-name)))
      (with-open-file (input temp-path)
	(with-open-file (output path :direction 
			 :output :if-exists :supersede)
	  (loop for line = (read-line input nil :eof)
		until (eq line :eof) do
	    (write-line line output)))))
    (delete-file temp-path)
    
    ;; If the makefile is different from the one in the binary directory, push
    ;; it into the binary directory.
    (loop for binary-makefile in (list (system-binary-makefile system port-name)
				       (system-optimized-binary-makefile
					system port-name))
	  do
      (ensure-directories-exist binary-makefile)
      (when (or (not (probe-file binary-makefile))
		(not (file-contents-equal path binary-makefile)))
	(when verbose
	  (format t "~%Installing new bin dir makefile for ~a ~a"
		  (system-name system) (string-capitalize port-name)))
	(with-open-file (input path)
	  (with-open-file (output binary-makefile 
			   :direction :output :if-exists :supersede)
	    (loop for line = (read-line input nil :eof)
		until (eq line :eof)
		do
	      (write-line line output))))))))
