(in-package "TL")

;;;; Module PACKAGES

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






;;;; Package Functions and Macros




;;; This module implements operations for symbols and packages.

;;; The following form allows us to reference lisp:*package* in our sources, yet
;;; still have a C translation for it in the emitted images.

(def-translatable-lisp-var *package*)




;;; The function `sxhash-string' takes a string and computes the hash number of
;;; that string.  The hash value is guaranteed to be of type (unsigned-byte 16).

;;; Note that the hash code for a one character string is equal to the char-code
;;; of the character.  This fact is used when initializing the symbol T in
;;; tli::dump-makefile-information.  -jallard 12/5/97

(declaim (functional sxhash-string))

(defun sxhash-string (text-string)
  (declare (type string text-string)
	   (return-type fixnum))
  (loop with hash fixnum = 0
	for index from 0 below (length text-string)
	do
    (setq hash (logxor (+ (logand (ash hash 1) 65535)
			  (ash hash -15))
		       (char-code (char text-string index))))
	finally (return hash)))




;;; The function tli::init-symbol takes a symbol with only the type tag
;;; initialized, and assigns into it the name an hash number of the symbol.  It
;;; returns the symbol.  The function tli::init-symbol-into-package is a
;;; combination of init-symbol and insert-symbol-into-package, this is called
;;; from the top level initializations of symbols in translated C code.

(defun tli::init-symbol (symbol string string-hash)
  (declare (type symbol symbol)
	   (type t string)
	   (type fixnum string-hash)
	   (return-type symbol))
  (tli::set-symbol-type-tag symbol)
  (setf (tli::symbol-local-value symbol) t)
  (setf (tli::symbol-external symbol) nil)
  (setf (tli::symbol-balance symbol) 0)
  (setf (tli::symbol-imported symbol) nil)
  (setf (tli::symbol-name-hash symbol) string-hash)
  (tli::set-symbol-name symbol string)
  (tli::set-symbol-value-pointer symbol (tli::the-unbound-value))
  (tli::set-non-null-symbol-plist symbol nil)
  (tli::set-symbol-package symbol nil)
  (tli::set-symbol-function symbol (tli::the-unbound-value))
  (setf (tli::symbol-left-branch symbol) (tli::the-unbound-value))
  (setf (tli::symbol-right-branch symbol) (tli::the-unbound-value))
  symbol)

(defun tli::init-symbol-into-package (symbol string string-hash package)
  (declare (type symbol symbol)
	   (type t string)
	   (type fixnum string-hash)
	   (type package package)
	   (return-type symbol))
  (tli::init-symbol symbol string string-hash)
  (when package
    (insert-symbol-into-package symbol package))
  symbol)




;;; The function `insert-symbol-into-package' takes a package and a symbol to be
;;; interned directly into that package.  This function returns no values.

(defun insert-symbol-into-package (symbol package)
  (declare (type symbol symbol)
	   (type package package)
	   (return-type void))
  (if (tli::not-unbound-value-p (tli::package-root-symbol package))
      ;; Insert into balanced binary tree later, just a binary tree now.  -jra
      ;; 4/8/96
      (let ((hash-number (tli::symbol-name-hash symbol))
	    (name (tli::non-null-symbol-name symbol)))
	(declare (type fixnum hash-number))
	(loop with current-symbol = (tli::package-root-symbol package)
	      for last-symbol = current-symbol
	      for current-hash fixnum = (tli::symbol-name-hash current-symbol)
	      do
	  (cond ((< hash-number current-hash)
		 (setq current-symbol (tli::symbol-left-branch current-symbol))
		 (unless (tli::not-unbound-value-p current-symbol)
		   (setf (tli::symbol-left-branch last-symbol) symbol)
		   (tli::set-symbol-package symbol package)
		   (return nil)))
		((> hash-number current-hash)
		 (setq current-symbol (tli::symbol-right-branch current-symbol))
		 (unless (tli::not-unbound-value-p current-symbol)
		   (setf (tli::symbol-right-branch last-symbol) symbol)
		   (tli::set-symbol-package symbol package)
		   (return nil)))
		(t
		 (let ((compare-result
			 (tli::string-compare
			   name (tli::non-null-symbol-name current-symbol))))
		   (declare (type fixnum compare-result))
		   (cond
		     ((< compare-result 0)
		      (setq current-symbol (tli::symbol-left-branch current-symbol))
		      (unless (tli::not-unbound-value-p current-symbol)
			(setf (tli::symbol-left-branch last-symbol) symbol)
			(tli::set-symbol-package symbol package)
			(return nil)))
		     ((> compare-result 0)
		      (setq current-symbol (tli::symbol-right-branch current-symbol))
		      (unless (tli::not-unbound-value-p current-symbol)
			(setf (tli::symbol-right-branch last-symbol) symbol)
			(tli::set-symbol-package symbol package)
			(return nil)))
		     (t
		      (error "Can't insert ~a in ~a, a symbol with that name already exists."
			     symbol package))))))))
      (progn
	(tli::set-symbol-package symbol package)
	(setf (tli::package-root-symbol package) symbol))))




;;; The function `find-symbol-in-single-package' takes a string, the hash for
;;; that string, and a package.  If there is a symbol with the given name
;;; directly interned in this package, then this function will return that
;;; symbol.  If no such symbol exists, then this function returns 0.  Note that
;;; NIL is not the default return value, since that could be the symbol that is
;;; returned from this function.  Also note that this function is not searching
;;; through the used packages of the given package, but only searches the one
;;; given package.  Even further, note that this function returns the symbol as
;;; found in the package data structure.  The caller must check whether or not
;;; the symbol is an imported pointer to another symbol or if the symbol is
;;; external.

(defun find-symbol-in-single-package (string string-hash package)
  (declare (type string string)
	   (type fixnum string-hash)
	   (type package package)
	   (return-type t))
  (loop with current-symbol = (tli::package-root-symbol package)
	initially (unless (tli::not-unbound-value-p current-symbol)
		    (return 0))
	while (tli::not-unbound-value-p current-symbol)
	for current-hash fixnum = (tli::symbol-name-hash current-symbol)
	do
    (cond ((< string-hash current-hash)
	   (setq current-symbol (tli::symbol-left-branch current-symbol)))
	  ((> string-hash current-hash)
	   (setq current-symbol (tli::symbol-right-branch current-symbol)))
	  (t
	   (let ((compare-result (tli::string-compare
				   string
				   (tli::non-null-symbol-name current-symbol))))
	     (declare (type fixnum compare-result))
	     (cond
	       ((< compare-result 0)
		(setq current-symbol (tli::symbol-left-branch current-symbol)))
	       ((> compare-result 0)
		(setq current-symbol (tli::symbol-right-branch current-symbol)))
	       (t
		(return current-symbol))))))
	finally (return 0)))




;;; The variable `all-packages' contains a list of all packages in the current
;;; environment.  The function `list-all-packages' is the CLtL2 interface to
;;; fetching that list.

(defvar all-packages)

(declaim (functional list-all-packages))

(defun list-all-packages ()
  all-packages)

(defun find-package-1 (string-or-symbol-or-package)
  (declare (return-type t))
  (typecase string-or-symbol-or-package
    (string
     (let ((name string-or-symbol-or-package))
       (declare (type string name))
       (loop for package in all-packages do
	 (when (string= (package-name package) name)
	   (return package)))))
    (symbol
     (find-package-1 (symbol-name string-or-symbol-or-package)))
    (package
     string-or-symbol-or-package)
    (t
     (error "FIND-PACKAGE given bad argument ~a."
	    string-or-symbol-or-package))))




;;; The macro `find-package-or-error' takes a string, symbol, or package and
;;; returns the package named by the argument.  Note that this differs from
;;; find-package, which returns NIL if no package can be found.  This macro
;;; attempts to optimize cases where the argument can be proven to be a package.

(defmacro find-package-or-error (&environment env package-or-name)
  (if (tli::tl-subtypep (tli::expression-result-type package-or-name env)
			'package)
      package-or-name
      `(find-package-or-error-1 ,package-or-name)))

(defun find-package-or-error-1 (name)
  (declare (return-type package))
  (let ((find-result (find-package name)))
    (if find-result
	find-result
	(error "No package with name ~a could be found." name))))




;;; The macro `package-use-list' fetches the use-list from a package after
;;; coercing it to a package.

(defmacro package-use-list (package-or-package-name)
  `(tli::package-use-list-internal
     (find-package-or-error ,package-or-package-name)))

(defun make-package-1 (name use)
  (declare (type string name)
	   (return-type t))
  (with-permanent-area
    (let* ((name-to-use (string-upcase name))
	   (use-list (loop for used in use
			   for used-package = (find-package-or-error used)
			   collect used-package))
	   (found-package? (find-package-1 name-to-use)))
      (cond
	(found-package?
	 #-translator
	 (error "Cannot make-package ~s, that package already exists."
		name-to-use)
	 ;; When in translation, check that the given use list matches the
	 ;; already existing use list, else signal an error.  This relaxing of
	 ;; the error case is done since we pre-create all packages that have
	 ;; constant folded symbol interned in them.  -jallard 12/4/97
	 (unless (loop for used-package-cons = (package-use-list found-package?)
					     then (cdr-of-cons used-package-cons)
		       for new-package-cons = use-list
					    then (cdr-of-cons new-package-cons)
		       while (and used-package-cons new-package-cons)
		       always (eq (car-of-cons used-package-cons)
				  (car-of-cons new-package-cons))
		       finally
			 (when (or used-package-cons new-package-cons)
			   (return nil)))
	   (error "Use list for ~a differs from compile-time list: ~@
                   compile-time = ~s, new = ~s."
		  name-to-use (tli::package-use-list-internal found-package?)
		  use))
	 found-package?)
	(t
	 (let ((new-package (tli::make-new-package name-to-use use-list)))
	   (setq all-packages (cons new-package all-packages))
	   new-package))))))




;;; The macro `find-symbol' expands into a call to find-symbol-in-package.  This
;;; function requires an actual package argument and is given the already
;;; computed hash of the string.

(defmacro find-symbol (string &optional package)
  (let ((package-arg (if package
			 `(find-package-or-error ,package)
			 '*package*)))
    (if (or (constantp string) (symbolp string))
	`(find-symbol-in-package
	   ,string (sxhash-string ,string) ,package-arg)
	(let ((string-var (gensym)))
	  `(let ((,string-var ,string))
	     (declare (type string ,string-var))
	     (find-symbol-in-package
	       ,string-var (sxhash-string ,string-var) ,package-arg))))))

(defun find-symbol-in-package (string string-hash package)
  (declare (type string string)
	   (type fixnum string-hash)
	   (type package package))
  #-translator
  (declare (ignore string-hash))
  #-translator
  (return-from find-symbol-in-package
    (ab-lisp::find-symbol string package))
  #+translator
  (let ((found-symbol 
	  (find-symbol-in-single-package string string-hash package)))
    (if (eql found-symbol 0)
	(loop for used-package in (package-use-list package)
	      for found-inherited-symbol
		  = (find-symbol-in-single-package
		      string string-hash used-package)
	      do
	  (unless (eql found-inherited-symbol 0)
	    (return (values
		      (if (tli::symbol-imported found-inherited-symbol)
			  (tli::symbol-value-pointer found-inherited-symbol)
			  found-inherited-symbol)
		      :inherited)))
	      finally
		(return (values nil nil)))
	(values (if (tli::symbol-imported found-symbol)
		    (tli::symbol-value-pointer found-symbol)
		    found-symbol)
		(if (tli::symbol-external found-symbol)
		    :external
		    :internal)))))




;;; The function `intern-string-in-package' implements intern.  It takes the
;;; string, an already computed hash number for the string, and the package
;;; object to intern any newly created symbol into.

(defun intern-string-in-package (string hash-number package)
  (declare (type string string)
	   (type fixnum hash-number)
	   (type package package))
  (multiple-value-bind (symbol found?)
      (find-symbol-in-package string hash-number package)
    (if found?
	(values symbol found?)
      (with-permanent-area 
	(let ((new-symbol (tli::make-empty-symbol)))
	  (tli::init-symbol new-symbol string hash-number)
	  (tli::set-symbol-package new-symbol package)
	  (insert-symbol-into-package new-symbol package)
	  (values new-symbol :internal))))))




;;; The function `import' takes a symbol or list of symbols, and an optional
;;; package.  It performs the usual CLtL import operation.  Note that this is a
;;; consing operation.

;;; The way that an imported symbol is represented in the package is that a new
;;; symbol structure is allocated, and the symbol-imported bit is set on that
;;; symbol.  The symbol-value of this newly created "indirection symbol" is then
;;; set to the symbol being imported.  Within find-symbol-in-package, when a
;;; symbol is found by find-symbol-in-single-package, find-symbol-in-package
;;; then checks if the returned symbol structure has the symbol-imported bit
;;; set.  If so, find-symbol-in-package returns the symbol-value of the symbol
;;; structure returned from find-symbol-in-single-package.  In this manner,
;;; symbol structures are used to indirect from the binary tree of one package
;;; to imported symbols brought in from other packages.  Note that if the
;;; imported symbol is then exported, this happens by setting the exported bit
;;; on the indirection symbol, not the originally imported symbol. -jra 5/7/96

(defun import (symbol-or-symbol-list &optional (package-arg *package*))
  (declare (return-type t))
  (with-permanent-area ()
    (let ((symbol-list (if (listp symbol-or-symbol-list)
			   symbol-or-symbol-list
			   (tli::list-dynamic-extent symbol-or-symbol-list)))
	  (package (find-package-or-error package-arg)))
      (loop for symbol in symbol-list
	    for symbol-name = (symbol-name symbol)
	    for symbol-hash fixnum = (tli::symbol-name-hash symbol)
	    do
	(multiple-value-bind (found-symbol found?)
	    (find-symbol-in-package symbol-name symbol-hash package)
	  (when (and found? (not (eq found-symbol symbol)))
	    (error "The symbol ~a cannot be imported into ~a, a symbol with ~
                    that name is already accessible."
		   symbol package)))
	(cond ((null (symbol-package symbol))
	       (insert-symbol-into-package symbol package))
	      (t
	       (let ((import-symbol (tli::make-empty-symbol)))
		 (tli::init-symbol import-symbol symbol-name symbol-hash)
		 (setf (tli::symbol-imported import-symbol) t)
		 (setf (tli::symbol-value-pointer import-symbol) symbol)
		 (insert-symbol-into-package import-symbol package)))))
      t)))




;;; The function `export' takes a symbol or list of symbols, and an optional
;;; package.  This version of export does not perform all of the name conflict
;;; checks called for in CLtL2, Section 11.5.  It is presumed that the
;;; development Lisp environment has weeded these out.

(defun export (symbol-or-symbol-list &optional (package-arg *package*))
  (declare (return-type t))
  (with-permanent-area ()
    (let ((symbol-list (if (listp symbol-or-symbol-list)
			   symbol-or-symbol-list
			   (tli::list-dynamic-extent symbol-or-symbol-list)))
	  (package (find-package-or-error package-arg)))
      (loop for symbol in symbol-list
	    for symbol-name = (symbol-name symbol)
	    for symbol-hash fixnum = (tli::symbol-name-hash symbol)
	    do
	(multiple-value-bind (found-symbol found?)
	    (find-symbol-in-package symbol-name symbol-hash package)
	  (unless (and found? (eq found-symbol symbol))
	    (error "The symbol ~a cannot be exported from ~a, it is not ~
                    accessible from that package."
		   symbol package))
	  (case found?
	    ((:external)
	     ;; Already exported, do nothing.
	     nil)
	    ((:internal)
	     ;; Already found in this package's binary tree, merely the external
	     ;; bit.
	     (setf (tli::symbol-external symbol) t))
	    ((:inherited)
	     ;; It is accessible, but not in this package's data strucutures.
	     ;; Import an indirection symbol, and set the external bit on it.
	     (let ((import-symbol (tli::make-empty-symbol)))
	       (tli::init-symbol import-symbol symbol-name symbol-hash)
	       (setf (tli::symbol-imported import-symbol) t)
	       (setf (tli::symbol-value-pointer import-symbol) symbol)
	       (insert-symbol-into-package import-symbol package)
	       (setf (tli::symbol-external import-symbol) t)))
	    (t
	     (error "Bad second value ~a from find-symbol received by export."
		    found?)))))
      t)))




;;; The predicate `keywordp' tests if a given object is a keyword.

(defvar *keyword-package* (find-package "KEYWORD"))

(defmacro keywordp (object)
  (if (symbolp object)
      `(and (symbolp ,object)
	    ,object
	    (eq (symbol-package ,object) *keyword-package*))
      (let ((symbol-evaled (gensym)))
	`(let ((,symbol-evaled ,object))
	   (keywordp ,symbol-evaled)))))




;;; The function make-gensymed-symbol implements translated calls to gensym.

(defvar *gensym-counter* 1)

(declaim (type fixnum *gensym-counter*))

(defun make-gensymed-symbol (string-or-counter?)
  (declare (return-type symbol))
  (let ((prefix "G")
	(counter *gensym-counter*))
    (declare (type fixnum counter))
    (cond
      ((fixnump string-or-counter?)
       (setq counter string-or-counter?))
      ((stringp string-or-counter?)
       (setq prefix string-or-counter?)
       (incf *gensym-counter*))
      (t
       ;; Else, ignore the argument, do the default.
       (incf *gensym-counter*)))
    (with-permanent-area
      (let* ((counter-length
	      (1+ (truncate (the double-float
				 (log (float counter 1.0) 10)))))
	     (new-name (make-string
			(+ counter-length (length (the string prefix))))))
	(declare (type fixnum counter-length))
	(setf (fill-pointer new-name) 0)
	(format new-name "~a~v,'0d" prefix counter-length counter)
	(make-symbol new-name)))))




;;; The TL internal function `write-symbol' is called from tl:write to output
;;; symbols.

(defun write-symbol (symbol case stream?)
  (declare (type symbol symbol)
	   (return-type void))
  (let ((name-string (symbol-name symbol)))
    (when *print-escape*
      (if (keywordp symbol)
	  (write-string ":" stream?)
	  (let ((home-package (symbol-package symbol)))
	    (when (and (not (eq home-package *package*))
		       (not (eq (find-symbol name-string *package*) symbol)))
	      (write-string (package-name home-package) stream?)
	      (write-string
		(if (tli::symbol-external symbol) ":" "::")
		stream?)))))
    (let* ((name name-string)
	   (length (length (the string name-string)))
	   (stream (get-string-or-file-stream-for-output-macro
		     stream? length))
	   (string? (stringp stream)))
      (declare (type string name)
	       (type fixnum length))
      ;; Do the best optimization for the default case, :upcase.
      (cond
	((eq case :upcase)
	 (if string?
	     (dotimes (index length)
	       (write-char-to-string (char-upcase (char name index)) stream))
	   (dotimes (index length)
	     (write-char-to-file-stream
	      (char-upcase (char name index)) stream))))
	((eq case :downcase)
	 (dotimes (index length)
	   (write-char-to-string-or-file-stream
	     (char-downcase (char name index)) stream string?)))
	;; The :capitalize case.
	(t
	 (do* ((first t)
	       (index 0 (+ index 1)))
	      ((>= index length))
	   (declare (type fixnum index))
	   (let* ((char (char name index))
		  (alpha-char? (alpha-char-p char)))
	     (cond (first
		    (when alpha-char?
		      (setq char (char-upcase char))
		      (setq first nil)))
		   ((not alpha-char?)
		    (setq first t))
		   (t
		    (setq char (char-downcase char))))
	     (write-char-to-string-or-file-stream char stream string?))))))))






;;;; Package Specific Symbols




;; (jh, 9/27/90)

;;; with-package-specific-symbols 
;;;     (package-specs) &rest body
;;;
;;; On occasion we need to mention a package-qualified symbol even though we
;;; aren't sure that the package exists.  This has happened recently in our
;;; translator efforts, where the translator's host Lisp has a package that the
;;; destination Lisp lacks.  For example, suppose a Lucid-specific memory
;;; allocation form contains the symbol lucid::change-memory-management.  Although
;;; this form need not be present in the resulting Chestnut Lisp image, the symbol
;;; is read anyway, since the reader has to read in enough of a form to be able
;;; to ignore it.  This causes an error in Chestnut when the reader reaches the
;;; package qualifier for the unknown "LUCID" package.  
;;;
;;; Note that this is only a problem in macros, since any defuns with
;;; platform-specific symbols are not read in the host Lisp and never make it to
;;; the macro phase as long as the conditionalization #-translator is used.
;;;
;;; The solution is to refer to a package-qualified symbol by its printname and
;;; package name and thus bypass the symbol-reader entirely.  The macro
;;; with-package-specific-symbols accomplishes this by interning and substituting
;;; the package-qualified symbol into a macroexpansion only when it finds that the
;;; package exists.  When the package doesn't exist, with-package-specific-symbol
;;; leaves the symbol alone, since the code will never be executed anyway.  This
;;; arrangement gives the reader something to skip over, does the right thing when
;;; we actually intend to use the code, and makes the code more perspicuous than
;;; sprinkling it throughout with (intern "SYMBOL" "PACKAGE") forms.
;;;
;;; The form package-specs is of the format
;;;   (package1 |::| symbol1
;;;    package2 |::| symbol2
;;;    ...)
;;; and is intended to resemble the package-qualified symbols visually.  Note that
;;; the middle item, the symbol whose printname consists of two colons, is
;;; syntactic sugar.
;;;
;;; This macro also has the advantage of documenting in one place the
;;; platform-specific calls we make.

(defmacro with-package-specific-symbols (package-specs &body body)
  (let (platform-specific-symbol-alist)
    (do ((these-package-specs package-specs (cdddr these-package-specs)))
	((null these-package-specs))
      (let ((the-package (first these-package-specs))
	    (symbol (third these-package-specs)))
	(when (find-package the-package)
	  (push
	    (cons symbol
		  (intern (symbol-name symbol) (symbol-name the-package)))
	    platform-specific-symbol-alist))))
    (if (= (length body) 1)
	(setq body (car body))
	(setq body `(progn ,@body)))
    (if platform-specific-symbol-alist
	`(sublis ',platform-specific-symbol-alist
		 ,body)
	body)))
