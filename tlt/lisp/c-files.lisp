(in-package "TLI")

;;;; Module C-FILES

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






;;;; C File Structures




;;; This module implements structures and operations for C files.  A C file
;;; structure actually holds more than just a pointer to a stream into a C
;;; file.  It holds all information relevant to the translation of an individual
;;; Lisp module.

;;; The `c-file' structure contains an output stream to a C file, an output
;;; stream to an H file, a namespace, a top-level function, a list of needed
;;; variable and function externs in the H file, and a either a C file structure
;;; to hold the top level forms and constants for this file, or NIL if the
;;; current file will hold its own top level forms.

(defstruct (c-file (:print-function print-c-file))
  (system nil)
  (module nil)
  (temporary-pathname nil)
  (final-pathname nil)
  (c-stream nil)
  (h-file nil)
  (trans-data-stream nil)
  (trans-data-temporary-pathname nil)
  (trans-data-final-pathname nil)
  (namespace nil)
  (top-level-function nil)
  (top-level-function-name nil)
  (top-level-compound-statement nil)
  (top-level-symbols-function nil)
  (top-level-symbols-function-name nil)
  (top-level-symbols-compound-statement nil)
  (defined-functions nil)
  (defined-variables nil)
  (defined-c-types nil)
  (used-functions nil)
  (used-variables nil)
  (used-constants nil)
  (used-symbols nil)
  (used-compiled-functions nil)
  (used-class-typedefs nil)
  (last-symbol-definition? nil)
  (symbols-defined nil)
  (last-compiled-function-definition? nil)
  (compiled-functions-defined nil)
  (needed-function-externs nil)
  (needed-class-typedefs nil)
  (needed-variable-externs nil)
  (top-level-c-file? nil)
  (line-cache-length 0)
  (line-comments-cache (make-array '(256)))
  (line-comments-cache-index 0)
  (line-comments-cache-length 0))

(defun print-c-file (c-file stream level)
  (declare (ignore level))
  (with-printing-wrapper (c-file stream)
    (format stream "C-File ~a" (c-file-c-stream c-file))))
    




;;; The variable `*system-top-level-c-file*' holds a C file to store top level
;;; functions for the current system, or NIL if each C file should store its own
;;; top level functions.

(defvar *system-top-level-c-file* nil)




;;; The macro `with-open-c-file' is used to open up a new C file structure while
;;; translating a module.

(defmacro with-open-c-file ((c-file-var system module) &body forms)
  (let ((abort? (gensym)))
    `(let ((,c-file-var (make-c-file))
	   (,abort? t))
       (unwind-protect
	    (progn
	      (setf (c-file-system ,c-file-var) ,system)
	      (setf (c-file-module ,c-file-var) ,module)
	      (init-newly-opened-c-file ,c-file-var)
	      ,@forms
	      (setq ,abort? nil))
	 (close-c-file ,c-file-var :abort ,abort?)))))




;;; The function `make-top-level-c-file' takes a system and returns a C file
;;; structure ready to be used as a holder for the top level functions for a
;;; whole system.

(defun make-top-level-c-file (system)
  (make-c-file
    :c-stream (open (system-c-file system 'top) :direction :output)))






;;;; Registering Defined and Needed Functions




;;; The function `register-defined-function' takes a C file, a Lisp symbol, and
;;; a C identifier string and registers it in the list of defined functions for
;;; this file.  Note that if there have already been calls to this function from
;;; within this file, then it will be listed in the hash table of needed
;;; functions.  This function should remove it from there.  The given
;;; information is stored to get stable C identifier choices in future
;;; incremental translations.

(defun register-defined-function (c-file function-symbol c-identifier-string)
  (setf (gethash function-symbol (c-file-defined-functions c-file))
	c-identifier-string)
  (remhash function-symbol (c-file-used-functions c-file))
  nil)




;;; The function `register-defined-variable' takes a C file, a Lisp symbol, and
;;; a C identifier string for the variable and registers it in the list of
;;; defined variables for this file.  Note that if there has been a forward
;;; reference to this variable from within this file, then the variable will be
;;; in the list of used-variables for the file.  If so, this function removes
;;; it.  The given information is stored to get stable C identifier choices in
;;; future incremental translations.

(defun register-defined-variable (c-file variable-symbol c-identifier-string)
  (setf (gethash variable-symbol (c-file-defined-variables c-file))
	c-identifier-string)
  (remhash variable-symbol (c-file-used-variables c-file))
  nil)




;;; The function `register-used-function' takes a C file, a Lisp symbol naming a
;;; function, a C identifier used in the translation for a call to that symbol,
;;; and the ftype used for the translation of a call to that function.  This is
;;; stored so that in future incremental translations it can be determined
;;; whether or not this file needs to be retranslated due to a change in ftype
;;; or identifier for functions called from this file.

;;; This function returns T when the named identifier needs an extern within
;;; this C file (i.e. the function hasn't already been defined in this C file
;;; and it hasn't already been registered as a used function).

(defun register-used-function (c-file lisp-function c-identifier ftype)
  (unless (or (gethash lisp-function (c-file-defined-functions c-file))
	      (gethash lisp-function (c-file-used-functions c-file)))
    (setf (gethash lisp-function (c-file-used-functions c-file))
	  (cons c-identifier ftype))
    t))




;;; The function `register-used-class' takes a C file, a Lisp symbol naming a
;;; class, and a C identifier used in the translation of references to instances
;;; of that class.  This is stored so that in future incremental translations it
;;; can be determined whether or not this file needs to be retranslated due to a
;;; change in identifiers for classes referenced from this file.

;;; This function returns T when the named identifier needs a typedef within
;;; this C file.

(defun register-used-class (c-file lisp-class c-identifier c-struct-type)
  (unless (gethash lisp-class (c-file-used-class-typedefs c-file))
    (setf (gethash lisp-class (c-file-used-class-typedefs c-file))
	  (cons c-identifier c-struct-type))
    t))




;;; The function `register-used-variable' takes a C file, a Lisp symbol naming a
;;; variable, a C identifier used in the translation of a reference to that
;;; function, and the type used in the translation.  This is stored so that in
;;; future incremental translations it can be determined whether or not this
;;; file needs to be retranslated due to a change in type or identifier for
;;; global variables referenced from this file.

(defun register-used-variable (c-file lisp-variable c-identifier type)
  (unless (or (gethash lisp-variable (c-file-defined-variables c-file))
	      (gethash lisp-variable (c-file-used-variables c-file)))
    (setf (gethash lisp-variable (c-file-used-variables c-file))
	  (cons c-identifier type))
    t))




;;;; Emitting Functions




;;; The macro `emit-string-to-c-file' takes a string and a c-file structure, and
;;; emits the string into the C file stream for that C file structure.  Note
;;; that it will perform some caching within lines.

(defmacro emit-string-to-c-file (string c-file)
  (let ((string-var (gensym))
	(c-file-var (gensym)))
    `(let ((,string-var ,string)
	   (,c-file-var ,c-file))
       (setf (c-file-line-cache-length ,c-file-var)
	     (the fixnum
		  (+ (the fixnum (c-file-line-cache-length ,c-file-var))
		     (the fixnum (length ,string-var)))))
       (tlt-write-string ,string-var (c-file-c-stream ,c-file-var))
       nil)))




;;; The macro `emit-string-with-newlines-to-file' takes a string and a c-file
;;; structure, and emits the string into the C file stream for the C file
;;; structure.  When this happens, we compute the new appropriate
;;; line-cache-length by iterating backwards through the string looking for a
;;; newline.  Note that the comments cache is not flushed by this operation (it
;;; would have to emit into the middle of the given string), but instead the
;;; comments cache is deferred to the end of the new line emitted within this
;;; string.  This should be a seldom used operation.

(defmacro emit-string-with-newlines-to-c-file (string c-file)
  (let ((string-var (gensym))
	(c-file-var (gensym))
	(length (gensym))
	(index (gensym)))
    `(let* ((,string-var ,string)
	    (,c-file-var ,c-file)
	    (,length (length ,string-var)))
       (declare (fixnum ,length))
       (loop for ,index fixnum from (1- ,length) downto 0
	     until (char= (schar ,string-var ,index) #\newline)
	     finally
	       (if (<= ,index 0)
		   (incf (c-file-line-cache-length ,c-file-var) 
			 (- ,length ,index))
		   (setf (c-file-line-cache-length ,c-file-var)
			 (max 1 (- ,length ,index)))))
       (tlt-write-string ,string-var (c-file-c-stream ,c-file-var))
       nil)))




;;; The macro `emit-character-to-c-file' takes a character and a c-file
;;; structure, and emits the character into the C file stream for that C file
;;; structure.  Note that it will perform some caching within lines.

(defmacro emit-character-to-c-file (character c-file)
  (let* ((needs-rebind? (not (symbolp c-file)))
	 (char (if needs-rebind? (gensym) character))
	 (c-file-var (if needs-rebind? (gensym) c-file))
	 (prefix (if needs-rebind?
		     `(let ((,char ,character) (,c-file-var ,c-file)))
		     '(progn))))
    `(,@prefix
	(setf (c-file-line-cache-length ,c-file-var)
	      (the fixnum (+ (the fixnum (c-file-line-cache-length ,c-file-var))
			     1)))
	(tlt-write-char ,char (c-file-c-stream ,c-file-var))
	nil)))




;;; The macro `emit-freshline-to-c-file' takes a C file structure, flushes any
;;; cached line information, and then emits a newline to the C file structure if
;;; any line information needed to be flushed.

(defmacro emit-freshline-to-c-file (c-file)
  (if (symbolp c-file)
      `(when (or (plusp (the fixnum (c-file-line-cache-length ,c-file)))
		 (plusp (the fixnum (c-file-line-comments-cache-index ,c-file))))
	 (emit-newline-to-c-file ,c-file))
      (let ((c-file-var (gensym)))
	`(let ((,c-file-var ,c-file))
	   (emit-freshline-to-c-file ,c-file-var)))))




;;; The constant `line-comments-target-column ' is the leftmost place within a
;;; line that we'll start a line comment.

(defconstant line-comments-target-column 48)




;;; The function `emit-newline-to-c-file' takes a C file structure, flushes any
;;; cache, and then emits a newline to the C stream for the C file.

(defun emit-newline-to-c-file (c-file)
  (let ((c-stream (c-file-c-stream c-file)))
    (when (plusp (the fixnum (c-file-line-comments-cache-index c-file)))
      ;; Emit enough spaces to put us at the target column, or aligned to the
      ;; next multiple of 4.  Always emit at least 2 spaces.
      (let* ((line-so-far (+ (c-file-line-cache-length c-file) 2))
	     (spaces (+ (if (<= line-so-far line-comments-target-column)
			    (- line-comments-target-column line-so-far)
			    (logand (- line-so-far) 3))
			2)))
	(tlt-write-string
	  "                                                "
	  c-stream
	  :end spaces))
      ;; Emit comment opening.
      (tlt-write-string "/* " c-stream)
      (loop with comments-cache = (c-file-line-comments-cache c-file)
	    for comments-index fixnum from 0
			       below (c-file-line-comments-cache-index c-file)
	    do
	(tlt-write-string (svref comments-cache comments-index) c-stream))
      (tlt-write-string " */" c-stream)
      (setf (c-file-line-comments-cache-index c-file) 0)
      (setf (c-file-line-comments-cache-length c-file) 0))

    (setf (c-file-line-cache-length c-file) 0)
    (tlt-write-char #\newline c-stream)
    nil))




;;; The function `emit-indentation-to-c-file' takes a number of indentation
;;; levels to emit and a c file to emit into.  This function will emit a
;;; freshline to the file then emit two spaces of indentation per level, except
;;; if the number of levels exceeds 14 (both of these values will be parameters
;;; in the implementation).  In order to prevent excessively long lines,
;;; indentation levels over 14 will have a comment at the beginning of the line
;;; saying how many levels of indentation, and then further spaces will be
;;; emitted to give an effective indentation of 14 levels.  The indentation
;;; level will also be stored into the c-file structure, so that line
;;; continuations of expressions can be indented to this level plus one or two
;;; more extra levels.

(defmacro indent-string-given-level (indent-level)
  (let ((spaces-per-indent 2)
	(maximum-indents-allowed 14)
	(indent (gensym)))
    `(let ((,indent ,indent-level))
       (declare (fixnum ,indent))
       (case ,indent
	 ,@(loop for level from 0 to maximum-indents-allowed
		 collect
		 `((,level) ,(make-string (* level spaces-per-indent)
					  :initial-element #\space)))
	 (otherwise
	  (format nil "/* Indented ~a levels */    " ,indent))))))

(defun emit-indentation-to-c-file (c-file indent)
  (declare (fixnum indent))
  (emit-freshline-to-c-file c-file)
  (when (plusp indent)
    (emit-string-to-c-file
      (indent-string-given-level indent)
      c-file)))




;;; The function `emit-line-comment-to-c-file' is used to emit comment strings
;;; to a C file that will be printed to the right of the C statements included
;;; on this line.  When emitting these to a file, the beginning of the comment
;;; will be aligned onto the nearest 4 character boundary following either 2
;;; spaces to the right of the line contents or the 48th column in a line,
;;; whichever is greater.  For line length computation purposes, this means that
;;; the first line comment on a line makes the cached-line contents at least 48
;;; characters long, plus at most 3 alignment chars, plus 6 characters for the
;;; comment delimiters and spaces, plus the actual comment length itself.

(defun emit-line-comment-to-c-file (comment-string c-file)
  (unless (zerop (c-file-line-comments-cache-index c-file))
    (setf (svref (c-file-line-comments-cache c-file)
		 (c-file-line-comments-cache-index c-file))
	  ", ")
    (incf (c-file-line-comments-cache-index c-file))
    (incf (c-file-line-comments-cache-length c-file) 2))
  (setf (svref (c-file-line-comments-cache c-file)
	       (c-file-line-comments-cache-index c-file))
	comment-string)
  (incf (c-file-line-comments-cache-index c-file))
  (incf (c-file-line-comments-cache-length c-file)
	(length comment-string))
  nil)




;;; The function `c-file-line-length' takes a C file and returns the number of
;;; characters emitted to the current line so far.  When line comments have been
;;; emitted to the file, then the line length must be extended to include
;;; padding, the comment deliminters, etc.

(defun c-file-line-length (c-file)
  (if (zerop (c-file-line-comments-cache-length c-file))
      (c-file-line-cache-length c-file)
      (let* ((line-length
	       (max (- line-comments-target-column 2)
		    (c-file-line-cache-length c-file)))
	     (aligned-length (+ line-length (logand (- line-length) 3)))
	     (comment-length (+ (c-file-line-comments-cache-length c-file) 6)))
	(+ aligned-length comment-length))))




;;; The macro `emit-indentation-to-c-file-if-necessary' takes a C-file and a
;;; c-expr and emits a newline and indentation if the current line is too long
;;; or if further emitting of the given c-expr would make the line too long.

;; For now, this function ignores the given c-expr, but in the future this
;; function could be made to check that expression to see if the next set of
;; text to be printed from this expression before a newline would overflow the
;; line, and then we would kick out a newline.  Ahh, dreams.  -jra 8/28/95

;; If we did add the c-expr, it would have to allow a NIL argument as well,
;; since sometimes we are emitting things other than c-exprs, such as c-decls.
;; -jra 1/22/96

(defmacro emit-indentation-to-c-file-if-necessary (c-file c-expr? indent)
  (declare (ignore c-expr?))
  (let* ((break-point 70)
	 (cf (gensym))
	 (ind (gensym)))
    `(let ((,cf ,c-file)
	   (,ind ,indent))
       (when (> (if (zerop (c-file-line-comments-cache-length ,cf))
		    (c-file-line-cache-length ,cf)
		    (+ (c-file-line-comments-cache-length ,cf)
		       (c-file-line-cache-length ,cf)
		       8))
		,break-point)
	 (emit-indentation-to-c-file ,cf (+ ,ind 2))))))
