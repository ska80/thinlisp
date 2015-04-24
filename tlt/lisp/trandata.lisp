(in-package "TLI")

;;;; Module TRANDATA

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




;;;; Copyright Handling

;;; The function unix-style-namestring is like namestring, but
;;; it converts DOS style backslashs to unix style.  This keeps
;;; the generated file text stable for source control.

(defun unix-style-namestring (pathname)
  (substitute #\/ #\\ (namestring pathname)))

;; Where does this function belong?

(defun generate-default-c-copyright-prolog (stream file-kind system-name module-name c-namestring lisp-namestring)
  (declare (type (member :lisp-file :c-file) file-kind))
  (declare (ignore system-name module-name))
  (let ((unix-c-namestring (unix-style-namestring c-namestring))
	(unix-lisp-namestring (unix-style-namestring lisp-namestring))
	(organization "The Thinlisp Group")  ;; Ought to be in the system data.
	 (current-year
	   (sixth (multiple-value-list
		      (decode-universal-time (get-universal-time))))))
  (case file-kind
    (:lisp-file
       (format stream
	  ";;;; Module ~a

;;; Copyright (c) ~a ~a All Rights Reserved.

;;; Translation data for ~a.
"
	  unix-c-namestring
	  current-year
	  organization
	  unix-lisp-namestring))
    (:c-file
     (format stream
	     "/***
 *
 * Module:      ~a
 *
 * Copyright (c) ~a ~a All Rights Reserved.
 *
 * Description: Translation of ~a.
 *    by ThinLisp http://www.thinlisp.org
 *
 */~%~%"
	     
	     unix-c-namestring
	     current-year
	     organization
	     unix-lisp-namestring)))))

(defun emit-copyright (stream file-kind system module-name c-namestring lisp-namestring)
  (let* ((system-name (system-name system))
	 (copyright-generator
	 (getf (system-properties system)
	       :copyright-generator
	       #'generate-default-c-copyright-prolog)))
    (funcall copyright-generator
	     stream file-kind system-name module-name
	     c-namestring lisp-namestring)))
  
  
;;;; Translation Data Files






;;; In order to implement incremental translations, certain data must be kept
;;; about the translations that have occurred.  This information is stored in
;;; translation data files that are kept in the same directories as the
;;; translated C files.  They must be kept under source code control just as the
;;; C files themselves are kept, since their information is about a particular C
;;; translation of a file.  The format of these files will be Lisp forms that
;;; are read and evalled.  They will be output via pretty printing, and any
;;; lists in them will be sorted so as to keep differences between versions of
;;; the files localized to small sections of the files.

;;; There are three classes of information stored in the files: function type
;;; signatures, since a change in function type requires a retranslate of its
;;; callers; constant vector contents and locations, since global constant
;;; folding requires retranslates if the location of a constant in a vector
;;; changes; and externed C identifiers of the file, since the set of externs
;;; in one file can affect the availability and therefore the use of externs in
;;; a secondary file.

;;; Inside the c/<system-name>/<module-name>.tlt files, there will be two forms.
;;; The first is just a fixnum that represents the version of the TLT files that
;;; we are will to attempt to reload.  If that fixnum is not equal to the value
;;; of `trans-data-tlt-version', then we will not attempt to read the file.  If
;;; the version numbers match, then the seconf form in the file, when evaluated,
;;; will return a `trans-data' structure.

;; Changed the version to 2 when class typedefs were added.  -jallard 11/3/99

(defparameter trans-data-tlt-version 2)

(defstruct (trans-data
	     (:constructor
	      make-trans-data (used-symbols
				symbol-array-name
				defined-symbols
				used-compiled-functions
				compiled-function-array-name
				defined-compiled-functions
				used-functions
				defined-functions
				used-variables
				defined-variables
				used-class-typedefs)))
  used-symbols
  symbol-array-name
  defined-symbols
  used-compiled-functions
  compiled-function-array-name
  defined-compiled-functions
  used-functions
  defined-functions
  used-variables
  defined-variables
  used-class-typedefs)




;;; The global variables `*global-symbol-registry*' and
;;; `*global-compiled-function-registry*' are used to store hash tables of
;;; symbol keys that point to lists of c-var-identifiers and indices.  These
;;; lookups are used to store the locations of symbol structures and compiled-
;;; function structures that have already been interned in forms translated
;;; prior to this one.  The identifiers are strings that name arrays of symbol
;;; or compiled function structures, and the index is the location of that array
;;; of the given symbol.  The address of this location is then used by
;;; references to this symbol within expressions.

;;; The variable `*global-package-registry*' is used to hold a list of the home
;;; packages of all symbols in the global symbol registry.  These packages must
;;; be made before we begin generating symbol constants.

(defvar *global-symbol-registry* nil)

(defvar *global-compiled-function-registry* nil)

(defvar *global-package-registry* nil)




;;; The variable `debugging-translate' is bound to T during translations for
;;; debugging purposes.  This can be used to suppress extraneous file contents
;;; like verbose headers.

(defvar debugging-translate nil)






;;; The function `write-trans-data-file' is called when closing a C file that
;;; has just been translated.  It will write out a file that contains all the
;;; data needed during incremental translates to verify that none of the data
;;; generated during a translation has changed, and all the data that the
;;; translation of this file would generate, so the effect of the translation
;;; of this file can be established during an incremental translate that skips
;;; this file.

(defun write-trans-data-file (c-file)
  (with-open-file (stream (c-file-trans-data-final-pathname c-file)
		   :direction :output
		   :if-exists :supersede)
    (when debugging-translate
      (return-from write-trans-data-file nil))
    (let ((system (c-file-system c-file))
	  (module (c-file-module c-file))
	  (*package* *tli-package*)
	  (*print-pretty* nil)
	  (symbol-array-name (car (c-file-last-symbol-definition? c-file)))
	  (compiled-function-array-name
	    (car (c-file-last-compiled-function-definition? c-file))))
      (emit-copyright
       stream
       :lisp-file
       system
       module
       (namestring (system-c-file system module))
       (namestring (system-lisp-file system module)))

      (format stream
	      ";;; The following is the value of the trans-data-tlt-version ~
               parameter.~%~a~%~%"
	      trans-data-tlt-version)

      (write-line "(make-trans-data" stream)
      (write-trans-data-hash-table
	stream
	"Used quoted symbols = (symbol symbol-array . index)."
	(c-file-used-symbols c-file))
      (format stream
	      "  ;; Name for this file's array of quoted symbols.~%  ~s~%  ~
                         ;; Quoted symbols defined in this file.~%"
	      symbol-array-name)
      (write-trans-data-list-of-symbols
	stream (reverse (c-file-symbols-defined c-file)))
      (write-trans-data-hash-table
	stream
	"Used compiled-functions = (name func-array . index)."
	(c-file-used-compiled-functions c-file))
      (format stream
	      "  ;; Name for this file's array of compiled-functions.~%  ~s~%  ~
                   ;; Compiled-function objects defined in this file.~%"
	      compiled-function-array-name)
      (write-trans-data-list-of-symbols
	stream
	(reverse (c-file-compiled-functions-defined c-file)))
      (write-trans-data-hash-table
	stream
	"Used function type signatures." (c-file-used-functions c-file))
      (write-trans-data-hash-table
	stream
	"Defined functions."
	(c-file-defined-functions c-file))
      (write-trans-data-hash-table
	stream
	"Used variables = (symbol c-name . type)."
	(c-file-used-variables c-file))
      (write-trans-data-hash-table
	stream
	"Defined variables" (c-file-defined-variables c-file))
      (write-trans-data-hash-table
	stream
	"Used class typedefs." (c-file-used-class-typedefs c-file))
      (write-line "  )" stream))))

(defvar trans-data-hash-table-entries nil)

(defun collect-trans-data-hash-table-entries (symbol value)
  (push (cons symbol value) trans-data-hash-table-entries))

(defun trans-data-hash-table-entry-key (entry)
  (symbol-name (cons-car entry)))

(defun write-trans-data-hash-table (stream title hash-table)
  (format stream "  ;; ~a~%" title)
  (let ((trans-data-hash-table-entries nil))
    (maphash #'collect-trans-data-hash-table-entries hash-table)
    (setq trans-data-hash-table-entries
	  (sort trans-data-hash-table-entries
		#'string<
		:key #'trans-data-hash-table-entry-key))
    (if (null trans-data-hash-table-entries)
	(write-line "  NIL" stream)
	(loop for first = t then nil
	      for entry in trans-data-hash-table-entries do
	  (if first
	      (format stream "  '(~s" entry)
	      (format stream "~%    ~s" entry))
	      finally (write-line ")" stream)))))

(defun write-trans-data-list-of-symbols (stream symbols)
  (if symbols
      (loop for first = t then nil
	    for symbol in symbols
	    do
	(if first
	    (format stream "  `(~s" symbol)
	    (format stream "~%    ~s" symbol))
	    finally (write-line ")" stream))
      (write-line "  NIL" stream)))


(defmacro retranslate-warning (verbose module format-string &rest args)
  `(when ,verbose
     (format t "~%  ~a will be retranslated.  " ,module)
     (format t ,format-string ,@args)))




;;; The function `trans-data-indicates-retranslate-p' takes a system and a
;;; module, and it views the referenced constants and the referenced functions
;;; and determines if the constants are still located in the same location
;;; within the same constant-vectors, and if the referenced functions still
;;; provide the same argument and return value signature.  This function is
;;; called only when it is known that there is a TLT data file that is up to
;;; date with the source Lisp file, so this function need not be concerned about
;;; file existence or dates.  If this function returns NIL, it will also modify
;;; the global symbol registries and the global C namespaces to register the
;;; names defined in a normal translation of this file.

(defun trans-data-indicates-retranslate-p (system module verbose)
  (let* ((data-file (system-trans-data-file system module))
	 (trans-data-version nil)
	 (trans-data nil))
    ;; Read in the version number.  If it does not match the one in the
    ;; trans-data-tlt-version parameter, then return T.  Else, we can read the
    ;; trans-data file.
    (when (with-open-file (input data-file)
	    (setq trans-data-version (read input nil :eof))
	    (cond ((eql trans-data-version trans-data-tlt-version)
		   (let ((*package* *tli-package*))
		     (setq trans-data (eval (read input)))
		     nil))
		  (t t)))
      (retranslate-warning verbose module "Obsolete format in TLT file.")
      (return-from trans-data-indicates-retranslate-p t))

    ;; Check that the symbol constants we used are still in the same locations.
    (loop for (symbol . array-name-and-index)
	      in (trans-data-used-symbols trans-data)
	  do
      (unless (equal array-name-and-index
		     (gethash symbol *global-symbol-registry*))
	(retranslate-warning
	  verbose module "Quoted symbol ~s has moved." symbol)
	(return-from trans-data-indicates-retranslate-p t)))
    ;; Check that the symbols constants we define are not already defined.
    (loop for symbol in (trans-data-defined-symbols trans-data) do
      (when (gethash symbol *global-symbol-registry*)
	(retranslate-warning
	  verbose module "~s is now quoted by an earlier file." symbol)
	(return-from trans-data-indicates-retranslate-p t)))
    ;; Check that the compiled-function constants we used are stil in the same
    ;; locations.
    (loop for (func-name . array-name-and-index)
	      in (trans-data-used-compiled-functions trans-data)
	  do
      (unless (equal array-name-and-index
		     (gethash func-name *global-compiled-function-registry*))
	(retranslate-warning
	  verbose module "The compiled-function ~s has moved."
	  func-name)
	(return-from trans-data-indicates-retranslate-p t)))
    ;; Check that the compiled-function constants we define are not already
    ;; defined.
    (loop for symbol in (trans-data-defined-compiled-functions trans-data) do
      (when (gethash symbol *global-compiled-function-registry*)
	(retranslate-warning
	  verbose module
	  "The compiled-function ~s is in an earlier file."
	  symbol)
	(return-from trans-data-indicates-retranslate-p t)))

    ;; Check that the functions we used have the same C names and function
    ;; signatures.
    (loop for (function-name c-name . ftype)
	      in (trans-data-used-functions trans-data)
	  for current-c-name = (c-identifier-for-function
				 function-name *global-c-namespace*
				 *global-c-namespace*)
	  for current-ftype = (or (function-decl function-name 'ftype)
				  (function-decl
				    function-name 'computed-ftype))
	  do
      (when (not (string= c-name current-c-name))
	(retranslate-warning
	  verbose module
	  "A C function name has changed:~%    ~a changed to ~a."
	  function-name c-name current-c-name)
	(return-from trans-data-indicates-retranslate-p t))
      (when (not (equal ftype current-ftype))
	(retranslate-warning
	  verbose module
	  "The ftype for ~s changed:~%    ~s changed to ~s"
	  function-name ftype current-ftype)
	(return-from trans-data-indicates-retranslate-p t)))

    ;; Check that the variables we used have the same C names and types.
    (loop for (variable-name c-name . c-type)
	      in (trans-data-used-variables trans-data)
	  for current-c-name = (c-identifier-for-variable
				 variable-name *global-c-namespace*
				 *global-c-namespace*)
	  for struct?
	      = (variable-decl variable-name 'variable-binding-structure)
	  for current-c-type
	      = (if struct? (variable-binding-c-type struct?) 'obj)
	  do
      (when (not (string= c-name current-c-name))
	(retranslate-warning
	  verbose module
	  "A C variable name has changed:~%    ~a changed to ~a."
	  variable-name c-name current-c-name)
	(return-from trans-data-indicates-retranslate-p t))
      (when (not (equal c-type current-c-type))
	(retranslate-warning
	  verbose module
	  "The C variable type for ~s has changed:~%    ~a changed to ~a."
	  variable-name c-type current-c-type)
	(return-from trans-data-indicates-retranslate-p t)))

    ;; Check that the classes we used have the same identifiers and structure
    ;; types.
    (loop for (class-name c-name . c-struct-type)
	      in (trans-data-used-class-typedefs trans-data)
	  for current-c-name = (c-identifier-for-class
				 class-name *global-c-namespace*
				 *global-c-namespace*) 
	  for class-info? = (class-info class-name)
	  for current-c-struct-type 
	      = (if class-info? (struct-c-type class-info?) nil)
	  do
      (when (not (string= c-name current-c-name))
	(retranslate-warning
	  verbose module
	  "A C typedef name for a Lisp structure or class has changed:~%    ~a changed to ~a"
	  class-name c-name current-c-name)
	(return-from trans-data-indicates-retranslate-p t))
      (when (not (c-types-equal-p c-struct-type current-c-struct-type))
	(retranslate-warning
	 verbose module
	 "A C struct type for a Lisp structure or class has changed:~%    ~a changed to ~a"
	 class-name c-struct-type current-c-struct-type)
	(return-from trans-data-indicates-retranslate-p t)))

    ;; Check that the functions we define can get the same C names.  Note that
    ;; this attempt to look up the c-identifier-for-function will cause the
    ;; returned identifier to be established for that symbol.
    (loop for (function-name . c-name)
	      in (trans-data-defined-functions trans-data)
	  for current-c-name = (c-identifier-for-function
				 function-name *global-c-namespace*
				 *global-c-namespace*)
	  do
      (when (not (string= c-name current-c-name))
	(retranslate-warning
	  verbose module
	  "The C function name for ~s has changed:~%    ~a changed to ~a."
	  function-name c-name current-c-name)
	(return-from trans-data-indicates-retranslate-p t)))

    ;; Check that the variables we defined can get the same C names.  Note that
    ;; this attempt to look up the c-identifier-for-variable will cause the
    ;; returned identifier to be established for that symbol.
    (loop for (variable-name . c-name)
	      in (trans-data-defined-variables trans-data)
	  for current-c-name = (c-identifier-for-variable
				 variable-name *global-c-namespace*
				 *global-c-namespace*)
	  do
      (when (not (string= c-name current-c-name))
	(retranslate-warning
	  verbose module
	  "The C variable name for ~s has changed:~%    ~a changed to ~a."
	  variable-name c-name current-c-name)
	(return-from trans-data-indicates-retranslate-p t)))

    ;; If we get here, we will be returning NIL.  This code installs the defined
    ;; information for symbol constants and compiled-function constants within
    ;; this file, as recorded in the trans-data structure.  The names for
    ;; defined functions and variables were installed in the C namespace with
    ;; the last two loops above.
    (loop with array-name = (trans-data-symbol-array-name trans-data)
	  with last-package = nil
	  for index fixnum from 0
	  for symbol in (trans-data-defined-symbols trans-data)
	  for package = (symbol-package symbol)
	  do
      (setf (gethash symbol *global-symbol-registry*)
	    (cons array-name index))
      (when (and package
		 (not (eq package last-package)) ; minor optimization
		 (not (memq package *global-package-registry*)))
	(push package *global-package-registry*)))
    (loop with array-name = (trans-data-compiled-function-array-name trans-data)
	  for index fixnum from 0
	  for symbol in (trans-data-defined-compiled-functions trans-data)
	  do
      (setf (gethash symbol *global-compiled-function-registry*)
	    (cons array-name index)))
    nil))




;;; The function `similar-as-constants-p' takes two constants and returns
;;; whether or not these two contants may be folded within compilations.  See
;;; CLtL II, pp. 691-694 for details.

;; Currently unused.  -jallard 10/24/97

#+unused
(defun similar-as-constants-p (const1 const2)
  ;; The EQ test covers symbols, packages, 
  (or (and (eq const1 const2)
	   (or (numberp const1)
	       (consp const1)
	       (simple-vector-p const1)
	       (stringp const1))) 
      (typecase const1
	(number
	 (and (numberp const2)
	      (= const1 const2)))
	(cons
	 (loop for cons1 = const1 then (cdr cons1)
	       for cons2 = const2 then (cdr cons2)
	       while (and (consp cons1)
			  (consp cons2))
	       always (similar-as-constants-p (car cons1) (car cons2))
	       finally
	       (return (or (eq cons1 cons2)
			   (and (atom cons1)
				(atom cons2)
				(similar-as-constants-p cons1 cons2))))))
	(simple-vector
	 (and (simple-vector-p const2)
	      (= (length const1) (length const2))
	      (loop for index from 0 below (length const1)
		    always (similar-as-constants-p
			     (svref const1 index)
			     (svref const2 index)))))
	(string
	 (and (stringp const2)
	      (= (length const1) (length const2))
	      (loop for index fixnum from 0 below (length const1)
		    always (char= (char const1 index) (char const2 index)))))
	(otherwise nil))))


;; The following two functions are unused for now.  Keeping them in case I
;; change my mind about leaving the trans-data structures cached in memory,
;; rather than the current behavior of re-reading them each time we consider
;; translating a file, then allowing them to be garbage collected.  -jallard
;; 10/22/97

#+unused
(defun set-trans-data-structure (system module structure)
  (let* ((system-name (normalize-system-name system))
	 (module-name (normalize-module-name module))
	 (module-alist (get module-name :trans-data-structure))
	 (system-entry? (assq system-name module-alist)))
    (if system-entry?
	(setf (cdr system-entry?) structure)
	(setf (get module-name :trans-data-structure)
	      (cons system-name structure)))
    structure))

#+unused
(defun trans-data-structure (system module)
  (cdr (assq (normalize-system-name system)
	     (get (normalize-module-name module) :trans-data-structure))))
