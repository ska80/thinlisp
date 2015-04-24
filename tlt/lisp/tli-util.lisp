(in-package "TLI")

;;;; Module TLI-UTIL

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






;;;; Miscellaneous Utilities




;;; This file contains miscellaneous utilities needed within the TLI package,
;;; but which are not large enough or sophisticated enough to deserve their own
;;; module.






;;;; Forward References




;;; The ubiquitous `declare-forward-function-references' and
;;; `declare-forward-variable-references' macros are defined here.  They each
;;; take &rest arguments of symbols that will name functions or variables in
;;; modules later in the load order.

(defun stand-in (&rest args)
  (declare (ignore args))
  (error "You have called the stand-in for a forward referenced TLI function."))

(defmacro declare-forward-function-references (&rest functions)
  `(eval-when (:compile-toplevel)
     ,@(loop for func in functions
	     collect `(unless (fboundp ',func)
			(setf (symbol-function ',func) #'stand-in)))))

(defmacro declare-forward-variable-references (&rest variables)
  `(eval-when (:compile-toplevel)
     (proclaim '(special ,@variables))))
	       
  





;;;; Package Variables




;;; The global parameters `*tli-package*', `*tlt-package*', and `*tl-package*'
;;; contain pointers to the package objects defined by the TLT.

(defparameter *tli-package* (find-package "TLI"))

(defparameter *tlt-package* (find-package "TLT"))

(defparameter *tl-package* (find-package "TL"))

(defparameter *tl-user-package* (find-package "TL-USER"))

(defparameter *lisp-package* (find-package #+lucid "LISP" #-lucid "COMMON-LISP"))

(defparameter *cl-user-package* (find-package "CL-USER"))

(defparameter *keyword-package* (find-package "KEYWORD"))




;;; The constants `lisp-file-type', `lisp-binary-file-type', `c-file-type',
;;; `h-file-type', and `trans-data-file-type' hold strings containing the types
;;; for the different types of files manipulated by the translator.  The
;;; constants `lisp-dev-binary-directory-name' and
;;; `lisp-macro-binary-directory-name' contain the names of the subdirectories
;;; of the Lisp directories that will contain binaries for these two diffent
;;; types of compiles.

(defconstant lisp-file-type "lisp")

(defconstant lisp-binary-file-type
    #+lucid                      "sbin"
    #+allegro                    "fasl"
    #+cmu                        "x86f"
    #+mcl                        "pfsl"
    #-(or lucid allegro cmu mcl) "bin")

(defconstant c-file-type "c")		; duh

(defconstant h-file-type "h")

(defconstant trans-data-file-type "tlt")

(defconstant temporary-c-file-type "tmc")

(defconstant temporary-h-file-type "tmh")

(defconstant temporary-trans-data-file-type "tmg")

(defconstant lisp-dev-binary-directory-name "dev")

(defconstant lisp-macro-binary-directory-name "macro")






;;;; Fast Stream Writing




;;; The macros `tlt-write-char' and `tlt-write-string' should be used for all
;;; output to Lisp file streams.  In implementations where optimizations can be
;;; found, these macros will expand to the optimized versions.

(defmacro tlt-write-char (char stream)
  #+lucid
  `(lcl:fast-write-char ,char ,stream)
  #-lucid
  `(write-char ,char ,stream))

(defmacro tlt-write-string (string stream &rest keyword-args)
  #+lucid
  `(lcl:fast-write-string ,string ,stream ,@keyword-args)
  #-lucid
  `(write-string ,string ,stream ,@keyword-args))


(defun pathname-print-string (path case escape)
  (with-output-to-string (temp-stream)
    (write path :stream temp-stream :case case :escape escape)))





;;;; File Comparison




;;; The function `file-contents-equal' takes two pathnames and returns whether
;;; or not the ASCII contents of these two pathnames are the same.  Both files
;;; are assumed to exist.

(defun file-contents-equal (file1 file2)
  (with-open-file (file1-in file1)
    (with-open-file (file2-in file2)
      (loop for file1-line = (read-line file1-in nil nil)
	    for file2-line = (read-line file2-in nil nil)
	    always (or (eq file1-line file2-line)
		       (and file1-line file2-line
			    (string= file1-line file2-line)))
	  while (and file1-line file2-line)))))






;;;; Home-Grown Method Dispatch




;;; Several different operations will be implemented for l-expr structures, so
;;; this section implements a means of defining methods for specific types of
;;; l-expr structures (and no, I still don't want to use CLOS for this).

;;; The macro `l-expr-method-table-var-name' takes a l-expr method name and
;;; returns the symbol naming the global variable that holds the dispatch table
;;; for that method.

(defmacro l-expr-method-table-var-name (method-name)
  `(intern (format nil "~a-METHOD-TABLE" ,method-name)))








;;;; String Building




;;; Since there are differences between CLtL1 and CLtL2 about the data type of
;;; elements of strings, you cannot portably write a call to make-array given an
;;; element type that will construct a string.  Make-string does this, but it
;;; doesn't have the same level of support as make-array, for example
;;; initial-contents.  The macro `make-string-array' takes a dimensions argument
;;; and &rest arguments of other make-array options.  It expands into a call to
;;; make-array that supplies the correct element type to get a string.

(defmacro make-string-array (length &rest make-array-args)
  `(make-array ,length
	       :element-type ',(array-element-type (make-string 1))
	       ,@make-array-args))



;;; The function `write-integer-to-string' takes an integer and returns a string
;;; containing that integer printed in decimal radix.  This is faster than
;;; calling format.

(defmacro write-digit-into-string (fill-pointered-string digit)
  (let ((string (gensym)))
    `(let ((,string ,fill-pointered-string))
       (declare (string ,string))
       (let ((index (fill-pointer ,string)))
	 (declare (fixnum index))
	 (setf (fill-pointer ,string) (the fixnum (+ index 1)))
	 (setf (char ,string index)
	       (code-char (the fixnum (+ ,digit #.(char-code #\0))))))
       nil)))

(defun write-positive-fixnum-into-string (string fixnum)
  (declare (string string)
	   (fixnum fixnum))
  (cond
    ((< fixnum 10)
     (write-digit-into-string string fixnum))
    (t
     (multiple-value-bind (value remainder)
	 (truncate fixnum 10)
       (declare (fixnum value remainder))
       (write-positive-fixnum-into-string string value)
       (write-digit-into-string string remainder))))
  nil)

(defun write-positive-bignum-into-string (string integer)
  (multiple-value-bind (value remainder)
      (truncate integer 10)
    (if (typep value 'fixnum)
	(write-positive-fixnum-into-string string value)
	(write-positive-bignum-into-string string value))
    (write-digit-into-string string remainder)))

(defun write-integer-to-string (integer)
  (case integer
    (-1 "-1")
    (0   "0")
    (1   "1")
    (2   "2")
    (3   "3")
    (4   "4")
    (5   "5")
    (6   "6")
    (7   "7")
    (8   "8")
    (9   "9")
    (10 "10")
    (11 "11")
    (12 "12")
    (13 "13")
    (14 "14")
    (15 "15")
    (16 "16")
    (17 "17")
    (18 "18")
    (19 "19")
    (20 "20")
    (t
     (let ((result-string (make-string-array 12 :fill-pointer t))
	   (index 0))
       (declare (fixnum index))
       (setf (fill-pointer result-string) 0)
       (when (< integer 0)
	 (setf (fill-pointer result-string) 1)
	 (setf (char result-string index) #\-)
	 (setq integer (- integer))
	 (incf index))
       (if (typep integer 'fixnum)
	   (write-positive-fixnum-into-string result-string integer)
	   (write-positive-bignum-into-string result-string integer))
       result-string))))






;;;; Type Specific Accessors




;;; The following section provides type specific versions of functions supplied
;;; in Common Lisp.

;;; The macros `cons-car' and `cons-cdr' are type checking versons of car and
;;; cdr that require the argument to be a cons.  These operations are guaranteed
;;; to signal an error if the argument is not a cons.  The variable
;;; cons-error-message may be bound to a string which will be used as the error
;;; message.

(defmacro cons-car (cons)
  #+fastest-tlt
  `(car (the cons ,cons))
  #+(and (not fastest-tlt) cmu)
  `(safe-car ,cons)
  #+(and (not fastest-glt) (not cmu))
  (cond ((and (constantp cons)
	      (not (consp (eval cons))))
	 `(cons-error ,cons))
	((symbolp cons)
	 `(if (consp ,cons)
	      (car (the cons ,cons))
	      (cons-error ,cons)))
	(t
	 (let ((arg (gensym)))
	   `(let ((,arg ,cons))
	      (if (consp ,arg)
		  (car (the cons ,arg))
		  (cons-error ,arg)))))))

(defun safe-car (arg)
  (if (consp arg)
      (car (the cons arg))
    (cons-error arg)))

(defmacro cons-first (cons)
  `(cons-car ,cons))

(defmacro cons-cdr (cons)
  #+fastest-tlt
  `(cdr (the cons ,cons))
  #+(and (not fastest-tlt) cmu)
  `(safe-cdr ,cons)
  #+(and (not fastest-tlt) (not cmu))
  (cond ((and (constantp cons)
	      (not (consp (eval cons))))
	 `(cons-error ,cons))
	((symbolp cons)
	 `(if (consp ,cons)
	      (cdr (the cons ,cons))
	      (cons-error ,cons)))
	(t
	 (let ((arg (gensym)))
	   `(let ((,arg ,cons))
	      (if (consp ,arg)
		  (cdr (the cons ,arg))
		  (cons-error ,arg)))))))

(defun safe-cdr (arg)
  (if (consp arg)
      (cdr (the cons arg))
    (cons-error arg)))

(defun cons-caar (conses)
  (cons-car (cons-car conses)))

(defun cons-cadr (conses)
  (cons-car (cons-cdr conses)))

(defun cons-cddr (conses)
  (cons-cdr (cons-cdr conses)))

(defun cons-second (conses)
  (cons-car (cons-cdr conses)))

(defun cons-third (conses)
  (cons-car (cons-cddr conses)))

(defun cons-cdddr (conses)
  (cons-cdr (cons-cddr conses)))

(defun cons-fourth (conses)
  (cons-car (cons-cdddr conses)))

(defun cons-fifth (conses)
  (cons-car (cons-cdr (cons-cdddr conses))))

(defconstant default-cons-error-message
  "An argument to cons-car or cons-cdr, ~s, was not a cons.")

(defvar cons-error-message default-cons-error-message)

(defun cons-error (non-cons)
  (cerror "Continue, returning NIL." cons-error-message non-cons)
  nil)






;;;; List Searchers




;;; The following macros provide old Zetalisp list operations.

(defmacro assq (key a-list)
  (let ((key-var?
	  (if (not (or (constantp key) (symbolp key)))
	      (gensym)))
	(alist-entry? (gensym)))
    `(loop ,@(if key-var? `(with ,key-var? = ,key))
	   for ,alist-entry? in ,a-list
	   do
       (when (and ,alist-entry?
		  (eq (cons-car ,alist-entry?)
		      ,(or key-var? key)))
	 (return ,alist-entry?)))))

(defmacro memq (key list)
  (let ((key-var?
	  (if (not (or (constantp key) (symbolp key)))
	      (gensym)))
	(current-cons (gensym)))
    `(loop ,@(if key-var? `(with ,key-var? = ,key))
	   for ,current-cons = ,list then (cons-cdr ,current-cons)
	   while ,current-cons
	   do
       (when (eq (cons-car ,current-cons) ,(or key-var? key))
	 (return ,current-cons)))))

(defmacro memqp (key list)
  (if (constantp list)
      (if (symbolp key)
	  `(or ,@(loop for value in (eval list)
		       collect `(eq ,key ',value)))
	  (let ((key-var (make-symbol "KEY")))
	    `(let ((,key-var ,key))
	       (memqp ,key-var ,list))))
      `(memq ,key ,list)))
      

(defmacro delq (element containing-list)
  (let ((elt (gensym))
	(list (gensym)))
    `(let ((,elt ,element)
	   (,list ,containing-list))
       (cond ((null ,list) nil)
	     ((eq (cons-car ,list) ,elt)
	      (cons-cdr ,list))
	     (t
	      (loop with trailer = ,list
		    for next-cons = (cons-cdr trailer)
		    while next-cons
		    do
		(when (eq (cons-car next-cons) ,elt)
		  (setf (cdr trailer) (cons-cdr next-cons))
		  (return nil))
		(setq trailer next-cons))
	      ,list)))))




;;; The macro `equal-list-lengths-p' takes two lists and returns whether or not
;;; they are the same length.

(defmacro equal-list-lengths-p (list1 list2)
  (let ((list1-cons (gensym))
	(list2-cons (gensym)))
    `(loop for ,list1-cons = ,list1 then (cons-cdr ,list1-cons)
	   for ,list2-cons = ,list2 then (cons-cdr ,list2-cons)
	   while (and ,list1-cons ,list2-cons)
	   finally (return (and (null ,list1-cons) (null ,list2-cons))))))




;;; The function `add-to-alist-without-duplication' non-destructively adds the
;;; given key and value to the given alist, removing any existing entries
;;; already existing in the given alist.

;;; This function should be used when the resulting alist is going to be stored
;;; a long time (as opposed to being garbage collected soon) and you want to
;;; make sure that the resulting alist consumes as little memory as possible.

(defun add-to-alist-without-duplication (key value old-alist)
  (cons (cons key value)
	(if (assq key old-alist)
	    (if (eq key (cons-caar old-alist))
		(cons-cdr old-alist)
		(loop for old-element-cons on old-alist
		      for old-alist-entry = (cons-car old-element-cons)
		      until (eq (cons-car old-alist-entry) key)
		      collect old-alist-entry into new-alist-head
		      finally (return (nconc new-alist-head
					     (cons-cdr old-element-cons)))))
	    old-alist)))







;;;; Compiling Environment Utilities




;;; The function `eval-feature' takes a features testing form similar to what
;;; you might find after a pound-sign plus in source code.  This function
;;; evaluates the given form against the current binding of *features*.  The
;;; function `well-formed-eval-feature-clause' is a predicate determining
;;; whether a particular Lisp object is a valid argument to eval-feature,
;;; i.e. an s-expression of AND, OR, NOT and keyword terminals.

(defun eval-feature (feature-form)
  (if (symbolp feature-form)
      (memq feature-form *features*)
      (ecase (cons-car feature-form)
	((and tl:and)
	 (loop for form in (cons-cdr feature-form)
	       always (eval-feature form)))
	((or tl:or)
	 (loop for form in (cons-cdr feature-form)
	       thereis (eval-feature form)))
	((not tl:not)
	 (not (eval-feature (cons-second feature-form)))))))

(defun well-formed-eval-feature-clause (object)
  (cond ((consp object)
	 (let ((car (cons-car object)))
	   (and (memqp car '(and tl:and or tl:or not tl:not))
		(loop for cdr = (cons-cdr object) then (cons-cdr cdr)
		      while cdr
		      always (and (consp cdr)
				  (well-formed-eval-feature-clause
				    (cons-car cdr)))))))
	((keywordp object)
	 t)
	(t nil)))




;;; The function `tl:constantp' works like Common Lisp constantp, but also
;;; recognizes tl:quote.

(defun tl:constantp (form)
  (or (constantp form)
      (and (consp form)
	   (memqp (cons-car form) '(tl:quote tl:function)))))




;;; The variables `*current-system-name*' and `*current-module-name*' are bound
;;; to the appropriate symbols during translations.

(defvar *current-system-name* nil)

(defvar *current-module-name* nil)




;;; The macro `tl:expand-development-memory' takes a number of bytes of
;;; expansion and will extend the size of the Lisp development environment to
;;; that limit.

(defmacro tl:expand-development-memory (bytes)
  (unless (eval-feature :translator)
    `(user::expand-memory-to-limit ,bytes)))






;;;; Debug Printing




;;; This section should be filled out with routines from Fred's work in
;;; lisp/tl-extension.lisp to make printed representations of items be mouseable in
;;; Emacs.  For now, it does something lame and quick.

;;; The macro `with-printing-wrapper' takes an object, a stream, and a body of
;;; code.  It will print interesting header and trailer characters to identify
;;; the object being printed.  This macro returns the values returned from the
;;; body.  This is to be used for debugging in Lisp environments.

(defmacro with-printing-wrapper ((object stream) &body forms)
  (let ((object-var (gensym))
	(stream-var (gensym)))
    `(let ((,object-var ,object)
	   (,stream-var ,stream))
       (write-string "#<" ,stream-var)
       (multiple-value-prog1
	   ,@forms
	 (format stream " ~X>"
		 #+lucid (sys:%pointer ,object-var)
		 #-lucid (progn ,object-var 0))))))




;;; The macro `with-faster-standard-output' somewhat improves the speed of
;;; printing that goes to standard output by enabling output buffering on those
;;; systems that have it.  For example, this helps the interaction between Lucid
;;; and an Emacs displaying the output of that Lucid by limiting the process
;;; switching between the two.  This helps with the "stuck emacs" problem, when
;;; lots of little updates keep making emacs defer updating the screen, instead
;;; of having fewer large updates that emacs can actually catch up with.  By
;;; default in Lucid, every function that writes to *standard-output*
;;; immediately flushes it out to the user.

(defmacro with-faster-standard-output (&body forms)
  (if (eval-feature :translator)
      `(progn ,@forms)
      #+lucid
      `(lcl:with-buffered-terminal-output (*standard-output*)
	 ,@forms)
      #+(or allegro cmu)
      `(multiple-value-prog1
	   (progn ,@forms)
	 (force-output))
      #-(or lucid allegro cmu)
      `(progn ,@forms)))






;;;; Translation Error Handling




;;; The macro `translation-error' takes arguments like error, and performs a
;;; non-local exit of the current form translation, reporting the error to the
;;; user.  This expands into a call to format to *error-output*, then a throw to
;;; :translation-error.  Note that the throw is done through a separate
;;; function, `throw-translation-error', so that a breakpoint can be made on it
;;; to debug incorrect translation-error throws.

(defmacro translation-error (format-string &rest format-args)
  `(translation-error-1 (format nil ,format-string ,@format-args)))

(defun translation-error-1 (message)
  (describe-translation-context t)
  (write-string message *error-output*)
  (write-char #\space *error-output*)
  (handle-translation-problem :translation-error))

(defvar within-translation-catcher nil)

(defvar cerror-on-translations-errors?
  nil   ;; the default
  ;t     ;; use for debugging to enable backtrace
  )

(defun handle-translation-problem (throw-tag?)
  (when (and throw-tag? cerror-on-translations-errors?)
    (cerror "continue" "Handling translation problem."))
  (when throw-tag?
    (if within-translation-catcher
	(throw throw-tag? :error)
	(error "Uncontinuable TL Error, described just above."))))




;;; The macro `translation-warning' takes a control string and value arguments
;;; ala format, and warns the user about the problem within the translation.

(defmacro translation-warning (control-string &rest args)
  `(let ((*print-level* 3)
	 (*print-length* 5))
     (translation-warning-1 (format nil ,control-string ,@args))))

(defun translation-warning-1 (message)
  (force-output *standard-output*)
  (describe-translation-context nil)
  (write-string message *error-output*)
  (write-char #\space *error-output*)
  (force-output *error-output*)
  (handle-translation-problem nil))
  




;;; The variable `current-translation-context' should be bound to a symbol or
;;; form that somehow identifies the current thing being translated.  If bound
;;; to a symbol, then this should name the function currently being translated.
;;; If bound to a form, it should be the top level form currently being
;;; translated.  The binding of this variable is used when generating messages
;;; from translation-error and translation-warning.

(defvar current-translation-context nil)

(defun describe-translation-context (error?)
  (when current-translation-context
    (if (symbolp current-translation-context)
	(format *error-output* "~%Within function ~s:"
		current-translation-context)
	(let ((message (format nil "~a" current-translation-context)))
	  (when (> (length message) 80)
	    (setq message (format nil "~a..." (subseq message 0 80))))
	  (format *error-output* "~%Within ~a:" message)))
    (setq current-translation-context nil))
  (terpri *error-output*)
  (if error?
      (write-string "   Error: " *error-output*)
      (write-string "   Warning: " *error-output*))
  (force-output *error-output*))



;;; The variable `nonnil-variable-of-unknowable-value-and-type' is used
;;; in contexts were we want to supress all knowledge the compiler might
;;; infer about the result type.  This is useful in CMU lisp to supress
;;; dead code warnings due to nonlocal exits or constant return values.
;;; Such cases presumably arise only in code that is platform specific.

(defvar nonnil-variable-of-unknowable-value-and-type t)




;;; The function `lisp-translation-error' is used in the translation
;;; for the Lisp when we want to error at runtime.  It is obfuscated
;;; enough to assure that the compiler does not infer things about
;;; the down stream code.  This includes obfuscating the return type
;;; and the nonlocal exit, both of which generate dead code notes in
;;; CMU Lisp.

(defun lisp-translation-error (format-string &rest args)
  (when nonnil-variable-of-unknowable-value-and-type
    (apply #'error format-string args))
  nonnil-variable-of-unknowable-value-and-type)





;;;; Char Bit Vectors




;;; Within several portions of TLT there is a need for a fast predicates on
;;; characters that determine whether or not a given character is a member of a
;;; set.  Char bit vectors are used to represent a set of characters.
;;; Make-char-bit-vector is used to create one, and char-bit-on-p is used to
;;; test if a particular character is a member of the set.

(defmacro char-bit-on-p (bit-vector character)
  `(= (sbit ,bit-vector (char-code ,character)) 1))

(defun set-char-bit-range (bit-vector start through)
  (loop for index fixnum from (char-code start) to (char-code through) do
    (setf (sbit bit-vector index) 1)))

(defun set-char-bits-from-string (bit-vector string-of-chars-to-set)
  (loop for index fixnum from 0 below (length string-of-chars-to-set)
	for bit-index fixnum = (char-code (schar string-of-chars-to-set index))
	do
    (setf (sbit bit-vector bit-index) 1)))

(defun make-char-bit-vector (chars-ranges-and-strings)
  (loop with bit-vector = (make-array '(256) :element-type 'bit :initial-element 0)
	for entry in chars-ranges-and-strings do
    (cond ((characterp entry)
	   ;; When the following bit is changed to sbit, it causes a bug in the
	   ;; Lucid optimizer.  This operation is called rarely enough that a
	   ;; slightly slower access is really OK.  -jallard 11/13/97
	   (setf (bit bit-vector (char-code entry)) 1))
	  ((consp entry)
	   (set-char-bit-range bit-vector (car entry) (second entry)))
	  ((stringp entry)
	   (set-char-bits-from-string bit-vector entry))
	  (t
	   (error "Bad spec ~s in make-char-bit-vector" entry)))
	finally (return bit-vector)))






;;;; Rounding




;;; The function `round-up' takes a positive fixnum and rounds it up to the next
;;; highest multiple of the second argument fixnum.

(defun round-up (value alignment)
  (declare (fixnum value alignment))
  (setq value (- value))
  (setq alignment (- alignment))
  (setq value (logand value alignment))
  (the fixnum (- value)))






;;;; Selective Multiple-Value-Setq




;;; Some Lisps don't support having NIL in the settings list for cases where you
;;; want only a few of the values from a form.  The macro
;;; `multiple-value-setq-some' is just like multiple-value-setq, except it
;;; supports NIL in the setters list, which is interpreted as an ignored value.

(defmacro multiple-value-setq-some (settings value-form)
  (let ((bindings nil)
	(setq-forms nil)
	(ignorables nil))
    (loop for first = t then nil
	  for var in settings
	  for bind-var = (gensym)
	  do
      (push bind-var bindings)
      (cond (var
	     (push (list 'setq var bind-var) setq-forms))
	    ((not first)
	     ;; Don't ignore the first, we're going to return it.
	     (push bind-var ignorables))))
    (setq bindings (nreverse bindings))
    (setq setq-forms (nreverse setq-forms))
    (setq ignorables (nreverse ignorables))
    `(multiple-value-bind ,bindings
	 ,value-form
       (declare (ignore ,@ignorables))
       ,@setq-forms
       ,(car bindings))))




;;; The function `split-declarations-and-body' takes a list of forms that may
;;; have documentation and declarations at their head.  It splits the decls,
;;; body, and documentation and returns these three values in the order just
;;; mentioned.

(defun split-declarations-and-body (decl-and-body)
  (if (consp decl-and-body)
      (loop with decls = nil
	    with docs = nil
	    with first-form = nil
	    for subbody = decl-and-body then (cons-cdr subbody)
	    until (not (consp subbody))
	    do
	(setq first-form (cons-car subbody))
	(cond ((and (consp first-form)
		    (eq (car first-form) 'declare))
	       (push first-form decls))
	      ((and (stringp first-form)
		    (not (null (cons-cdr subbody)))) ; Not the last form
	       (push first-form docs))
	      (t
	       (return (values (nreverse decls) subbody (nreverse docs)))))
	    finally
	      (return (values (nreverse decls) subbody (nreverse docs))))
      (values nil decl-and-body nil)))

(defun tl:split-declarations-and-body (decl-and-body)
  (split-declarations-and-body decl-and-body))




;;;; Declaiming




;;; The Lucid we are currently using does not support declaim, so we have a
;;; macro that abstracts this with TLT.  Note that TL supports declaim, and so
;;; only the TLT implementation need worry about this fixup.

(defmacro lisp-declaim (&rest decls)
  #+lucid
  `(eval-when (compile load eval)
     ,@(loop for decl in decls
	     collect `(proclaim ',decl)))
  #-lucid
  `(declaim ,@decls))




;;;; Special Forms




;;; The special-form-p function has been removed from ANSI Common Lisp.
;;; Define it for those Lisp systems that haven't given us a replacement.

(defun lisp-special-operator-p (symbol)
  #+(or ansi-cl clisp)
  (special-operator-p symbol)
  #-(or ansi-cl clisp)
  (special-form-p symbol))




;;; The function `simple-argument-p' takes a form and returns whether or not it
;;; is simple enough to not require rebinding if we want to multiply expand it
;;; into the result of a macro expansion.

(defun simple-argument-p (form)
  (or (tl:constantp form)
      (symbolp form)
      (and (consp form)
	   (memq (car form) '(tl:the the))
	   (consp (cddr form))
	   (simple-argument-p (third form)))))






;;;; Memory Management




;;; The function `gc-a-little' will invoke the ephemeral garbage collector on
;;; the most transient levels of garbage.  This should be called at top levels
;;; of large processes when it is expected that most of the recent created data
;;; will be garbage, for example inbetween module translations.

(defun gc-a-little ()
  #+lucid
  (lcl:ephemeral-gc)
  #+allegro
  (excl:gc :tenure)
  #+cmu
  (ext:gc)
  nil)

(defun gc-a-lot ()
  #+lucid
  (lcl:full-gc)
  #+allegro
  (excl:gc t)
  ;; Unreleased versions of CMUCL have an incremental garbage collector.  When
  ;; that is released, then the arguments :full t should be added to the
  ;; following call.  -jallard 2/22/01
  #+cmu
  (ext:gc)
  nil)
