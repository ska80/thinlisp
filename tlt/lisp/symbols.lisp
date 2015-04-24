(in-package "TLI")

;;;; Module SYMBOLS

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






;;;; Symbols in Runtime Images







(defun emit-symbol-array-initialization (c-file array-name symbol-list initial-index)
  (let* ((symbols-func (c-file-top-level-symbols-function c-file))
	 (symbols-body (c-func-body-statement symbols-func))
	 (package-to-c-var-alist nil))
    (loop for index from initial-index
	  for symbol in symbol-list
	  for symbol-name = (symbol-name symbol)
	  for home-package? = (symbol-package symbol)
	  for package?
	      = (if (eq home-package? *lisp-package*)
		    (if (find-symbol symbol-name *tl-package*)
			*tl-package*
			(translation-error
			  "Attempting to make a pointer to ~s, a LISP symbol ~
                               not accessible from the TL package."
			  symbol))
		    home-package?)
	  for package-var? = (cdr (assq package? package-to-c-var-alist))
	  for symbol-expr = (make-c-subscript-expr (make-c-name-expr array-name)
						   (make-c-literal-expr index))
	  do
      (when (and package? (null package-var?))
	(setq package-var?
	      (lexical-c-variable-identifier
		(intern (format nil "CACHED-~a-PACKAGE" (package-name package?)))
		symbols-func 'obj nil))
	(setq package-to-c-var-alist
	      (cons (cons package? package-var?) package-to-c-var-alist))
	(emit-expr-to-compound-statement
	  (make-c-infix-expr
	    (make-c-name-expr package-var?)
	    "=" (make-c-function-call-expr
		  (make-c-name-expr "find_package_1")
		  (list (translate-string-constant-into-c
			  (package-name package?) 'obj c-file
			  nil symbols-func symbols-body :c-expr))))
	  symbols-body))
      (emit-expr-to-compound-statement
	(make-c-function-call-expr
	  (make-c-name-expr "init_symbol_into_package")
	  (list
	    (make-c-cast-expr 'obj (make-c-unary-expr #\& symbol-expr))
	    (translate-string-constant-into-c
	      symbol-name 'obj c-file nil symbols-func symbols-body :c-expr)
	    (make-c-literal-expr (funcall 'tl:sxhash-string symbol-name))
	    (make-c-name-expr (if package? package-var? "NULL"))))
	symbols-body)

      ;; Mark the external bit on symbols that are external to their package.
      (when package?
	(multiple-value-bind (new-sym internal)
	    (find-symbol symbol-name package?)
	  (declare (ignore new-sym))
	  (when (eq internal :external)
	    (emit-expr-to-compound-statement
	      (make-c-infix-expr
		(make-c-direct-selection-expr symbol-expr "external")
		"=" (make-c-literal-expr 1))
	      symbols-body))))
      
      (cond ((or (eq package? *keyword-package*) (eq symbol 't))
	     ;; Hook up symbol-value to point to self.
	     (emit-expr-to-compound-statement
	       (make-c-infix-expr
		 (make-c-direct-selection-expr symbol-expr "symbol_value")
		 "=" (make-c-cast-expr 'obj (make-c-unary-expr #\& symbol-expr)))
	       symbols-body))
	    ((memqp (tl:variable-information symbol) '(:special :constant))
	     (let ((c-value-var-name (c-identifier-for-variable
				       symbol *global-c-namespace*
				       (c-func-namespace symbols-func))))
	       (when (register-used-variable
		       c-file symbol c-value-var-name 'obj)
		 (register-needed-variable-extern
		   c-file '("extern") 'obj c-value-var-name))
	       ;; Hook up non-local-value pointer.
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-direct-selection-expr symbol-expr "local_value")
		   "=" (make-c-literal-expr 0))
		 symbols-body)
	     
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-direct-selection-expr symbol-expr "symbol_value")
		   "=" (make-c-cast-expr
			 'obj (make-c-unary-expr
				#\& (make-c-name-expr
				      c-value-var-name))))
		 symbols-body))))
      ;; Hook up a compiled-function constant to this symbol when the
      ;; function-type of the symbol is :function, and when it does not have a
      ;; direct C translation function.
      (multiple-value-bind (function-type? local? decls)
	  (tl:function-information symbol)
	(declare (ignore local?))
	(when (and (eq function-type? :function)
		   (or (assq 'computed-ftype decls)
		       (assq 'ftype decls))
		   (not (assq 'c-translator decls)))
	  ;; Hook up symbol-function pointer to a compiled-function.
	  (emit-expr-to-compound-statement
	    (make-c-infix-expr
	      (make-c-direct-selection-expr symbol-expr "symbol_function")
	      "=" (translate-compiled-function-constant-into-c
		    symbol decls c-file nil symbols-func symbols-body :c-expr))
	    symbols-body))))))
