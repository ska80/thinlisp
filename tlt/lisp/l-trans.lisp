(in-package "TLI")

;;;; Module L-TRANS

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1995-1997 Gensym Corporation.
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






;;;; Translating L-Exprs




;;; This module contains l-expr methods and functions for translating l-expr
;;; structures into c-expr structures, which are emitted into c-funcs and then
;;; c-files (whew).  This file deals with translating l-exprs within functions.

;;; The l-expr method `translate-l-expr-into-c' takes a l-expr, the surrounding
;;; c-function into which the translation is being performed, a C compound
;;; statement into which C statements can be emitted, and a return values
;;; directive.  The return values directive can have one of the following
;;; values:

;;; :discard - means this l-expr is being evaluated for side-effect only, its
;;; values are not used;

;;; :c-expr - means that the translation should return a C-expr which when
;;; evaluated will return the one value from the translation of this expression;

;;; :return - means that the translation of this l-expr should emit a
;;; c-return-statement into the given compound statement to return its values
;;; (or void) from this function; or

;;; (<var-binding-struct> ...) - a list of variable binding structures meaning
;;; that the values of the translation of the l-expr should be setq'ed into
;;; these variables.

(def-l-expr-generic-method translate-l-expr-into-c
    (l-expr c-func c-compound-statement return-directive)
  (format *error-output* "No translator for ~s~%" l-expr)
  (emit-c-expr-as-directed
    (make-c-function-call-expr
      (make-c-name-expr "error")
      (list (make-c-literal-expr "Unable to translate.")))
    l-expr c-func c-compound-statement return-directive))




;;; The function `return-directive-discards-values-p' takes a return directive
;;; and a c-func and returns whether or not the translation of an l-expr with
;;; the given return-directive within the given c-func will discard its values.
;;; This happens in three cases;

;;;   the return-directive is :discard,
;;;   the return directive is NIL (i.e. an empty list of variables to assign),
;;;   the return-directive is :return and the c-func type is 'void.

(defun return-directive-discards-values-p (return-directive c-func)
  (or (eq return-directive :discard)
      (null return-directive)
      (and (eq return-directive :return)
	   (eq (c-func-c-return-type c-func) 'void))))




;;; The function `translate-progn-body' is a convenience function for
;;; translating a list of l-exprs in a progn body.

(defun translate-progn-body
    (l-exprs c-func c-compound-statement return-directive)
  (loop for l-expr-cons = l-exprs then next-cons?
	for next-cons? = (cons-cdr l-expr-cons)
	while next-cons?
	do
    (with-reusable-c-variables-scope (c-func)
      (translate-l-expr-into-c
	(cons-car l-expr-cons) c-func c-compound-statement :discard))
	finally
	  (return
	    (if (eq return-directive :c-expr)
		(translate-l-expr-into-c
		  (cons-car l-expr-cons) c-func c-compound-statement
		  return-directive)
		(with-reusable-c-variables-scope (c-func)
		  (translate-l-expr-into-c
		    (cons-car l-expr-cons) c-func c-compound-statement
		    return-directive))))))
	




;;; The function `emit-c-expr-as-directed' takes a c-expr, the l-expr it is the
;;; return value of, the c-func enclosing it, a c-compound-statment, and a
;;; return-directive.  This is used as utility function for those l-expr
;;; translators that require no special handling of their return values.  It
;;; will emit c-statements as necessary into the given compound statement or
;;; return the given c-expr as directed by the return-directive.  The value of
;;; this function should be returned from the l-expr translator.

(defun emit-c-expr-as-directed
    (c-expr l-expr c-func c-body return-directive)
  (case return-directive
    ((:c-expr) c-expr)
    ((:discard nil)
     ;; If we are discarding the value, emit side-effecting C expressions as an
     ;; expression statement.  Add more side-effect-free checks both here and in
     ;; the 'void return case below.  -jra 1/11/96
     (unless (side-effect-free-c-expr-p c-expr)
       (emit-expr-to-compound-statement c-expr c-body))
     nil)
    ((:return)
     ;; Any differences in type requirements have already been handled by the
     ;; translation of any coerce-to-type l-exprs, so just emit a return
     ;; statement.
     (cond ((eq (c-func-c-return-type c-func) 'void)
	    (emit-c-expr-as-directed c-expr l-expr c-func c-body :discard)
	    (emit-statement-to-compound-statement
	      (make-c-return-statement nil) c-body))
	   (t
	    (emit-statement-to-compound-statement
	      (make-c-return-statement c-expr) c-body)))
     nil)
    (t
     (emit-expr-to-compound-statement
       (make-c-infix-expr
	 (make-c-name-expr (cons-car return-directive)) "=" c-expr)
       c-body)
     (loop for binding in (cons-cdr return-directive)
	   for value-count fixnum from 1
	   do
       (emit-expr-to-compound-statement
	 (make-c-infix-expr
	   (make-c-name-expr binding)
	   "="
	   (make-c-conditional-expr
	     (make-c-infix-expr
	       "Values_count" ">" (make-c-literal-expr value-count))
	     (make-c-subscript-expr
	       (make-c-name-expr "Values_buffer")
	       (make-c-literal-expr (the fixnum (- value-count 1))))
	     (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))
	 c-body)))))




;;; The variable `block-temp-var' is used for temporary variables when
;;; tranlating block statements.

(defvar block-temp-var (make-symbol "BLOCK-TEMP"))




;;; The C translation for `block-l-expr' caches the return-directive into the
;;; block struct for this block, caches a GOTO tag for this block into the
;;; struct, and then translates its body, discarding the results of all forms
;;; except the last.  If the return directive for this translation is :c-expr,
;;; then a new C variable will be made, it will be cached into the block
;;; structure, and all return-from calls should cache their results into this
;;; variable before performing a goto to the desired branch point.

(def-l-expr-method translate-l-expr-into-c
    (block-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form block-l-expr))
	 (name (cons-second form))
	 (block-struct
	   (block-scope-struct-for-name name (l-expr-aug-env block-l-expr)))
	 (return-from-count
	   (block-scope-l-expr-return-from-count block-struct)))
    (declare (fixnum return-from-count))

    ;; If there are no return-froms that mention this block (which will happen
    ;; often), then translate this like a progn.
    (cond
      ((= return-from-count 0)
       ;; Note that the block walker in SPECIAL ensures the block contains
       ;; at least one form.
       (translate-progn-body
	 (cons-cddr form) c-func c-compound-statement return-directive))
      (t
       (cond ((eq return-directive :c-expr)
	      (setf (block-scope-value-cache-c-variable block-struct)
		    (lexical-c-variable-identifier
		      block-temp-var c-func
		      (l-expr-c-return-type block-l-expr)
		      (if (block-scope-volatile-return-from block-struct)
			  '("volatile")
			  nil)))
	      (setf (block-scope-return-directive block-struct)
		    (list (block-scope-value-cache-c-variable block-struct))))
	     (t
	      (setf (block-scope-value-cache-c-variable block-struct) nil)
	      (setf (block-scope-return-directive block-struct) return-directive)))
       (when (not (eq return-directive :return))
	 (setf (block-scope-target-label block-struct)
	       (lexical-c-statement-label
		 (intern (format nil "EXIT_~a" name)) c-func)))
       (loop for form-cons = (cons-cddr form) then next-cons?
	     for next-cons? = (cons-cdr form-cons)
	     while next-cons?
	     do
	 (with-reusable-c-variables-scope (c-func)
	   (translate-l-expr-into-c
	     (cons-car form-cons) c-func c-compound-statement :discard))
	     finally
	       (cond
		 ((eq return-directive :c-expr)
		  (emit-expr-to-compound-statement
		    (make-c-infix-expr
		      (make-c-name-expr
			(block-scope-value-cache-c-variable block-struct))
		      "="
		      (translate-l-expr-into-c
			(cons-car form-cons) c-func c-compound-statement
			:c-expr))
		    c-compound-statement))
		 (t
		  (with-reusable-c-variables-scope (c-func)
		    (translate-l-expr-into-c
		      (cons-car form-cons) c-func c-compound-statement
		      return-directive)))))
       (when (block-scope-target-label block-struct)
	 (emit-statement-to-compound-statement
	   (make-c-label-statement (block-scope-target-label block-struct))
	   c-compound-statement))
       (when (eq return-directive :c-expr)
	 (reclaim-reusable-c-variables
	   c-func
	   (list (cons (block-scope-value-cache-c-variable block-struct)
		       (l-expr-c-return-type block-l-expr))))
	 (make-c-name-expr (block-scope-value-cache-c-variable 
			     block-struct)))))))




;;; The translation method for `catch-l-expr' establishes a local variable
;;; holding a jump buffer, pushes a pointer to the jmp_buf, the catch tag and a
;;; catch stack marker onto the stack, then translates into a call to setjmp,
;;; branching on whether it is an establishment or a longjmp.

(def-l-expr-method translate-l-expr-into-c
    (catch-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form catch-l-expr))
	 (jump-buffer (lexical-c-variable-identifier
			(make-symbol "CATCH-LOCATION") c-func 'jmp-buf nil))
	 (result? (if (not (return-directive-discards-values-p
			     return-directive c-func))
		      (lexical-c-variable-identifier
			'catch-result c-func 'obj '("volatile"))
		      nil))
	 (env (l-expr-env catch-l-expr))
	 ;; Change the following to 3 later.  -jallard 8/4/97 It's
	 ;; later.  -jallard 3/4/01
	 (safety? (>= (tl:optimize-information 'safety env) 3))
	 (expected-tos
	   (when safety?
	     (lexical-c-variable-identifier
	       'expected-top-of-stack c-func 'sint32 '("volatile")))))
    ;; Cache the old top-of-stack for debugging purposes.
    (when safety?
      (emit-expr-to-compound-statement
	(make-c-infix-expr (make-c-name-expr expected-tos) "=" "Throw_stack_top")
	c-compound-statement))
    ;; Emit the statement to evaluate the catch tag, putting it into the
    ;; Throw_stack.
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-subscript-expr
	  (make-c-name-expr "Throw_stack")
	  (make-c-infix-expr "Throw_stack_top" "+" 2))
	"="
	(translate-l-expr-into-c
	  (cons-second form) c-func c-compound-statement :c-expr))
      c-compound-statement)
    ;; Put a pointer to the jump buffer into the Throw_stack.
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-subscript-expr
	  (make-c-name-expr "Throw_stack")
	  (make-c-infix-expr "Throw_stack_top" "+" 1))
	"="
	(make-c-cast-expr
	  'obj (make-c-unary-expr #\& (make-c-name-expr jump-buffer))))
      c-compound-statement)
    ;; Increment Throw_stack_top and mark this portion of the stack as a catch
    ;; frame (marker number 4).
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	"Throw_stack_top" "=" (make-c-infix-expr "Throw_stack_top" "+" 3))
      c-compound-statement)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-subscript-expr (make-c-name-expr "Throw_stack")
			       (make-c-name-expr "Throw_stack_top"))
	"="
	(make-c-cast-expr 'obj (make-c-literal-expr 4)))
      c-compound-statement)
    ;; Emit a conditional statement that setjmp's the jump buffer, then
    ;; evaluates either the protected body or evaluates the code that retrieves
    ;; the cached thrown values from the stack.
    (emit-statement-to-compound-statement
      (make-c-conditional-statement
	(list
	  ;; Test
	  (make-c-infix-expr
	    (make-c-function-call-expr (make-c-name-expr "setjmp")
				       (list (make-c-name-expr jump-buffer)))
	    "!=" 0)
	  ;; Then statement
	  (make-c-expr-statement
	    (let ((retrieve (make-c-function-call-expr
			      (make-c-name-expr "retrieve_values_from_stack")
			      nil)))
	      (if result?
		  (make-c-infix-expr result? "=" retrieve)
		  retrieve)))
	  ;; No second test
	  nil
	  ;; Else part
	  (loop with protected-code
		  = (make-c-compound-statement nil nil nil nil)
		for l-expr-cons on (cons-cddr form)
		for l-expr = (cons-car l-expr-cons)
		do
	    (if (or (null result?) (cons-cdr l-expr-cons))
		(with-reusable-c-variables-scope (c-func)
		  (translate-l-expr-into-c
		    l-expr c-func protected-code :discard))
		(translate-l-expr-into-c
		  l-expr c-func protected-code (list result?)))
		finally
		  (emit-expr-to-compound-statement
		    (make-c-infix-expr
		      "Throw_stack_top" "="
		      (make-c-infix-expr "Throw_stack_top" "-" 3))
		    protected-code)
		  (return protected-code))))
      c-compound-statement)
    ;; Emit code to check that the top-of-stack has been restored to the
    ;; expected value.
    (when safety?
      (emit-statement-to-compound-statement
	(make-c-conditional-statement
	  (list (make-c-infix-expr (make-c-name-expr expected-tos)
				   "!=" (make-c-name-expr "Throw_stack_top"))
		(make-c-expr-statement
		  (make-c-function-call-expr
		    (make-c-name-expr "error")
		    (list (make-c-literal-expr
			    "Corrupted Throw_stack_top at catch."))))))
	c-compound-statement))
	
    ;; If the return directive called for a result, emit the code for it.
    (if result?
	(prog1
	    (emit-c-expr-as-directed
	      (make-c-name-expr result?) catch-l-expr c-func
	      c-compound-statement return-directive)
	  (reclaim-reusable-c-variables
	    c-func (list (cons jump-buffer 'jmp-buf) (cons result? 'obj))))
	(reclaim-reusable-c-variables
	  c-func (list (cons jump-buffer 'jmp-buf))))))




;;; The translation method for `eval-when-l-expr' operates like progn, since any
;;; eliminating of these forms from the body would have occurred earlier during
;;; constant folding.

(def-l-expr-method translate-l-expr-into-c
    (eval-when-l-expr c-func c-compound-statement return-directive)
  (translate-progn-body
    (cons-cddr (l-expr-form eval-when-l-expr))
    c-func c-compound-statement return-directive))

(def-l-expr-method translate-l-expr-into-c
    (flet-l-expr c-func c-compound-statement return-directive)
  (translate-progn-body
    (cons-cddr (l-expr-form flet-l-expr))
    c-func c-compound-statement return-directive))

(def-l-expr-method translate-l-expr-into-c
    (function-l-expr c-func c-body return-directive)
  (let ((name (cons-second (l-expr-form function-l-expr))))
    (multiple-value-bind (function-type local? decls local-function?)
	(tl:function-information name (l-expr-env function-l-expr))
      (cond
	((and (eq function-type :function)
	      (or (cdr (assq 'ftype decls))
		  (cdr (assq 'computed-ftype decls))))
	 (translate-compiled-function-constant-into-c
	   (if local? local-function? name) decls
	   (c-func-c-file c-func)
	   function-l-expr c-func c-body return-directive))
	(t
	 (translation-warning
	   "Can't translate #'~s: ~a."
	   name
	   (cond
	     ((fboundp name)
	      "it is Lisp function, but it has no C translation")
	     ((or (eq function-type :macro) (macro-function name))
	      "it is a macro, not a function")
	     ((or (eq function-type :special-form) 
		  (lisp-special-operator-p name))
	      "it is a special-form, not a function")
	     (t "it is not a defined function")))
	 (emit-c-expr-as-directed
	   (make-c-function-call-expr
	     (make-c-name-expr "error")
	     (list (make-c-literal-expr
		     (format nil "#'~s is not a defined function." name))))
	   function-l-expr c-func c-body return-directive))))))

(def-l-expr-method translate-l-expr-into-c
    (go-l-expr c-func c-body return-directive)
  (let* ((target-tag (cons-second (l-expr-form go-l-expr)))
	 (exit-scope
	   (tl:declaration-information 'exit-scope (l-expr-env go-l-expr)))
	 (found-tag? nil)
	 (current-c-body c-body)
	 (pre-exit-cleanups nil))
    (loop for entry in exit-scope
	  for type = (cons-car entry) do
      (cond
	((eq type 'tagbody-tags)
	 (setq found-tag? (assq target-tag (cons-cdr entry)))
	 (when found-tag?
	   (let ((struct (cons-second found-tag?)))
	     (when (null (tagbody-scope-c-identifier struct))
	       (setf (tagbody-scope-c-identifier struct)
		     (lexical-c-statement-label
		       (if (symbolp target-tag)
			   target-tag
			   (intern (format nil "TAG-~a" target-tag)))
		       c-func)))
	     (when pre-exit-cleanups
	       (emit-scope-exit-cleanups
		 (reverse pre-exit-cleanups) current-c-body c-func))
	     (emit-statement-to-compound-statement
	       (make-c-goto-statement (tagbody-scope-c-identifier struct))
	       current-c-body)
	     ;; Generally any references to the NIL returned here are eliminated.
	     ;; See the return point in return-from for a more complete
	     ;; explanation.  -jra 1/27/96
	     (return (if (eq return-directive :c-expr)
			 (default-value-for-c-type 'obj))))))

	((eq type 'block)
	 ;; Ignore block scopes.
	 nil)

	((eq type 'require-local-exit)
	 (issue-local-exit-warning target-tag (cons-second entry)))

	((eq type 'unwind-protect)
	 (let* ((current-protect (cons-second entry))
		(exit-path
		  (+ (length (protect-exit-descriptions current-protect))
		     2))
		(exit-statement (make-c-compound-statement nil nil nil nil))
		(new-exit (list 'go exit-path exit-statement)))
	   (push new-exit (protect-exit-descriptions current-protect))
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr
	       (make-c-name-expr (protect-values-count-identifier
				   current-protect))
	       "=" 0)
	     current-c-body)

	   (when pre-exit-cleanups
	     (emit-scope-exit-cleanups
	       (reverse pre-exit-cleanups) current-c-body c-func)
	     (setq pre-exit-cleanups nil))
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr
	       (make-c-name-expr (protect-exit-path-identifier current-protect))
	       "=" exit-path)
	     current-c-body)
	   (emit-statement-to-compound-statement
	     (make-c-goto-statement (protect-cleanup-statement-label
				      current-protect))
	     current-c-body)
	   (setq current-c-body exit-statement)))
	((memqp type '(global-variable-binding catch))
	 (push entry pre-exit-cleanups))
	(t (translation-error
	     "Unknown exit-type type ~s when seen by go ~s."
	     type target-tag))))))



;;; The translation for `if-l-expr' can translate either as an inline
;;; conditional expression, or it can translate as a conditional statement.  If
;;; the result is to be discarded, then it will always translate as a statement.
;;; If the result is used, then it goes to great pains in an attempt to
;;; translate into a C expression, but falls back to a statement if any of the
;;; argument l-exprs require substatements themselves.

(def-l-expr-method translate-l-expr-into-c
    (if-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form if-l-expr))
	 (test-l-expr (cons-second form))
	 (temp-storage-classes (storage-classes-for-env (l-expr-env if-l-expr)))
	 (c-return-type (l-expr-c-return-type if-l-expr))
	 (test-statement (make-c-compound-statement nil nil nil nil))
	 (test-c-expr
	   (translate-l-expr-into-c test-l-expr c-func test-statement :c-expr))
	 (test-required-statements?
	   (not (c-compound-statement-empty-p test-statement)))
	 (then-l-expr (cons-third form))
	 (else-l-expr (cons-fourth form))
	 (then-statement (make-c-compound-statement nil nil nil nil))
	 (else-statement (make-c-compound-statement nil nil nil nil)))
    (when test-required-statements?
      (emit-statement-to-compound-statement test-statement c-compound-statement))
    (cond
      ((not (eq return-directive :c-expr))
       (cond
	 ;; Drop the else clause if possible
	 ((and (return-directive-discards-values-p return-directive c-func)
	       (not (eq return-directive :return))
	       (l-expr-side-effect-free-p else-l-expr))
	  (with-reusable-c-variables-scope (c-func)
	    (translate-l-expr-into-c
	      then-l-expr c-func then-statement :discard))
	  (emit-statement-to-compound-statement
	    (make-c-conditional-statement (list test-c-expr then-statement))
	    c-compound-statement))
	 ;; Rotate the "else" clause to the "then" clause if we have a no-op
	 ;; "then" clause.
	 ((and (return-directive-discards-values-p return-directive c-func)
	       (not (eq return-directive :return))
	       (l-expr-side-effect-free-p then-l-expr))
	  (with-reusable-c-variables-scope (c-func)
	    (translate-l-expr-into-c
	      else-l-expr c-func then-statement :discard))
	  (emit-statement-to-compound-statement
	    (make-c-conditional-statement
	      (list (make-c-unary-expr #\! test-c-expr) then-statement))
	    c-compound-statement))
	 (t
	  (translate-l-expr-into-c
	    then-l-expr c-func then-statement return-directive)
	  (translate-l-expr-into-c
	    else-l-expr c-func else-statement return-directive)
	  (emit-statement-to-compound-statement
	    (make-c-conditional-statement
	      (list test-c-expr then-statement nil else-statement))
	    c-compound-statement))))
      ;; Attempt expanding into a C conditonal expr if the test-l-expr emitted
      ;; no statements and if this l-expr is side-effect free.  As an aesthetic
      ;; tweak, in order to prevent overly deep nesting of c-if-exprs, we will
      ;; prevent using an expression in the case where the else clause of this
      ;; if-l-expr is itself an if-l-expr.
      ((and (not test-required-statements?)
	    (l-expr-side-effect-free-p if-l-expr)
	    (or (not (if-l-expr-p else-l-expr))
		(not (if-l-expr-p (cons-fourth (l-expr-form else-l-expr))))))
       (let ((then-c-expr (translate-l-expr-into-c
			    then-l-expr c-func then-statement :c-expr))
	     (else-c-expr nil))
	 (cond
	   ((c-compound-statement-empty-p then-statement)
	    (setq else-c-expr
		  (translate-l-expr-into-c
		    else-l-expr c-func else-statement :c-expr))
	    (if (c-compound-statement-empty-p else-statement)
		(make-c-conditional-expr test-c-expr then-c-expr else-c-expr)
		(let ((if-result (lexical-c-variable-identifier
				   'if-result-temp c-func c-return-type
				   temp-storage-classes)))
		  (emit-expr-to-compound-statement
		    (make-c-infix-expr if-result "=" then-c-expr)
		    then-statement)
		  (emit-expr-to-compound-statement
		    (make-c-infix-expr if-result "=" else-c-expr)
		    else-statement)
		  (emit-statement-to-compound-statement
		    (make-c-conditional-statement
		      (list test-c-expr then-statement nil else-statement))
		    c-compound-statement)
		  (reclaim-reusable-c-variables
		    c-func (list (cons if-result c-return-type)))
		  (make-c-name-expr if-result))))
	   (t
	    (let ((if-result (lexical-c-variable-identifier
			       'if-result-temp c-func c-return-type
			       temp-storage-classes)))
	      (emit-expr-to-compound-statement
		(make-c-infix-expr if-result "=" then-c-expr)
		then-statement)
	      (translate-l-expr-into-c
		else-l-expr c-func else-statement (list if-result))
	      (emit-statement-to-compound-statement
		(make-c-conditional-statement
		  (list test-c-expr then-statement nil else-statement))
		c-compound-statement)
	      (reclaim-reusable-c-variables
		c-func (list (cons if-result c-return-type)))
	      (make-c-name-expr if-result))))))
      ;; Else, translate into a C conditional statement, caching the result into
      ;; a new temporary variable and returning that as the result.
      (t
       (let ((if-result (list (lexical-c-variable-identifier
				'if-result-temp c-func c-return-type
				temp-storage-classes))))
	 (translate-l-expr-into-c then-l-expr c-func then-statement if-result)
	 (translate-l-expr-into-c else-l-expr c-func else-statement if-result)
	 (emit-statement-to-compound-statement
	   (make-c-conditional-statement
	     (list test-c-expr then-statement nil else-statement))
	   c-compound-statement)
	 (setf (cdr if-result) c-return-type)
	 (reclaim-reusable-c-variables c-func (list if-result))
	 (make-c-name-expr (cons-car if-result)))))))

(def-l-expr-method translate-l-expr-into-c
    (labels-l-expr c-func c-compound-statement return-directive)
  (translate-progn-body
    (cons-cddr (l-expr-form labels-l-expr))
    c-func c-compound-statement return-directive))




;;; The translation for `let-l-expr' allocates C identifiers for the variables
;;; it binds, it emits statements to initialize the bound variables, and it then
;;; translates and emits the body forms.

;;; There are two special cases.  The first is that when a variable's binding
;;; structure contains init information, and the setq counters within it are
;;; both still zero, then it can replace the variable being bound with
;;; references to the variable in the init information.  This eliminates
;;; unneeded rebinding of variables values, effectively splicing out unneeded
;;; intermediate variables.

;;; The second case is for special variables.  When there are special variables,
;;; the previous values of those special variables need to be cached onto the
;;; Throw_stack before being initialized, and the old values need to be restored
;;; on exit.

(def-l-expr-method translate-l-expr-into-c
    (let-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form let-l-expr))
	 (inner-env (l-expr-aug-env let-l-expr))
	 (temp-storage-classes (storage-classes-for-env inner-env))
	 (bindings (cons-second form))
	 (body (cons-cddr form))
	 (c-file (c-func-c-file c-func))
	 (specials-to-rebind nil)
	 (temp-var-pairs nil))
    (loop for pair in bindings
	  for var = (cons-car pair)
	  for init-l-expr = (cons-second pair)
	  do
      (multiple-value-bind (binding-variety local? decls)
	  (tl:variable-information var inner-env)
	(declare (ignore local?))
	(let* ((struct (cons-cdr (assq 'variable-binding-structure decls)))
	       (lisp-type (variable-binding-lisp-type struct))
	       (c-type (variable-binding-c-type struct))
	       (init-info? (variable-binding-init-info? struct))
	       (volatile?
		 (or (variable-binding-volatile struct)
		     (env-requires-volatile-p inner-env))))
	  (cond
	    ((eq binding-variety :lexical)
	     (if (substitute-variables-p init-info? lisp-type c-type)
		 (setf (variable-binding-c-identifier struct)
		       (variable-binding-c-identifier
			 (cons-second init-info?)))
		 (let ((c-identifier
			 (lexical-c-variable-identifier
			   var c-func c-type
			   (if volatile? '("volatile") nil))))
		   (push (cons c-identifier c-type) temp-var-pairs)
		   (setf (variable-binding-c-identifier struct) c-identifier)
		   (translate-l-expr-into-c
		     init-l-expr c-func c-compound-statement
		     (list c-identifier)))))
	    ((eq binding-variety :special)
	     (let ((new-value
		     (lexical-c-variable-identifier
		       (intern (format nil "NEW-~a" var)) c-func c-type
		       (if volatile? '("volatile") nil)))
		   (c-name
		     (c-identifier-for-variable
		       var *global-c-namespace* (c-func-namespace c-func))))
	       (when (register-used-variable c-file var c-name c-type)
		 (register-needed-variable-extern
		   c-file '("extern") c-type c-name))
	       (push (cons new-value c-type) temp-var-pairs)
	       (setf (variable-binding-c-identifier struct) c-name)
	       (push (cons c-name new-value) specials-to-rebind)
	       (translate-l-expr-into-c
		 init-l-expr c-func c-compound-statement (list new-value))))
	    (t
	     (translation-error
	       "Unknown binding type ~s for ~s" binding-variety var))))))
    (cond
      (specials-to-rebind
       (let* ((c-type-of-let (l-expr-c-return-type let-l-expr))
	      (let-result?
		(unless (return-directive-discards-values-p
			  return-directive c-func)
		  (lexical-c-variable-identifier
		    'temp c-func c-type-of-let temp-storage-classes)))
	      ;; Change this to 3 some day when we trust that stack handling has
	      ;; settled down again.  -jallard 6/14/00
	      (safety? (>= (tl:optimize-information 'safety inner-env)
			   3))
	      (thread-state-var (lexical-c-variable-identifier
				 'ts c-func '(pointer thread-state) nil))
	      (expected-tos
		(when safety?
		  (reusable-c-variable-identifier
		    'expected-top-of-stack c-func 'sint32 inner-env))))
	 (when safety?
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr (make-c-name-expr expected-tos) "=" "Throw_stack_top")
	     c-compound-statement))
	 (emit-expr-to-compound-statement
	  (make-c-infix-expr
	   (make-c-name-expr thread-state-var) "=" "THREAD_STATE")
	  c-compound-statement)
	 (rebind-globals-for-let 
	  specials-to-rebind c-compound-statement thread-state-var)
	 (when let-result?
	   (push (cons let-result? (l-expr-c-return-type let-l-expr))
		 temp-var-pairs))
	 (translate-progn-body
	   body c-func c-compound-statement
	   (if let-result? (list let-result?) :discard))
	 (unbind-globals-for-let
	   (loop for (global-var) in specials-to-rebind collect global-var)
	   c-compound-statement thread-state-var)
	 (reclaim-reusable-c-variables c-func temp-var-pairs)
	 (when safety?
	   (emit-statement-to-compound-statement
	     (make-c-conditional-statement
	       (list (make-c-infix-expr (make-c-name-expr expected-tos)
					"!=" "Throw_stack_top")
		     (make-c-expr-statement
		       (make-c-function-call-expr
			 (make-c-name-expr "error")
			 (list (make-c-literal-expr
				 "Corrupted Throw_stack_top in let"))))))
	     c-compound-statement))

	 (emit-c-expr-as-directed
	   (if let-result?
	       (make-c-name-expr let-result?)
	       (default-value-for-c-type c-type-of-let))
	   let-l-expr c-func
	   c-compound-statement return-directive)))
      (t
       (prog1
	   (translate-progn-body
	     body c-func c-compound-statement return-directive)
	 (reclaim-reusable-c-variables c-func temp-var-pairs))))))

(defun substitute-variables-p (init-info? lisp-type c-type)
  (and init-info?
       (= (the fixnum (cons-car (cons-third init-info?))) 0)
       (= (the fixnum (cons-car (cons-fourth init-info?))) 0)
       (satisfies-c-required-type-p
	 (variable-binding-c-type (cons-second init-info?))
	 c-type)
       (satisfies-lisp-required-type-p
	 (variable-binding-lisp-type (cons-second init-info?))
	 lisp-type)))

(def-l-expr-method translate-l-expr-into-c
    (let*-l-expr c-func c-compound-statement return-directive)
  (loop with form = (l-expr-form let*-l-expr)
	with inner-env = (l-expr-aug-env let*-l-expr)
	with temp-storage-classes = (storage-classes-for-env inner-env)
	with safety? = (>= (tl:optimize-information 'safety inner-env) 3)
	with expected-tos = nil
	with thread-state-var = nil
	with bindings = (cons-second form)
	with body = (cons-cddr form)
	with c-file = (c-func-c-file c-func)
	with specials-to-unbind = nil
	with temp-var-pairs = nil
	with binding-variety = nil
	with decls = nil
	with struct = nil
	with lisp-type = nil
	with c-type = nil
	with init-info? = nil
	with volatile? = nil
	for pair in bindings
	for var = (cons-car pair)
	for init-l-expr = (cons-second pair)
	do
    (multiple-value-setq-some (binding-variety nil decls)
      (tl:variable-information var inner-env))
    (setq struct (cons-cdr (assq 'variable-binding-structure decls)))
    (setq lisp-type (variable-binding-lisp-type struct))
    (setq c-type (variable-binding-c-type struct))
    (setq init-info? (variable-binding-init-info? struct))
    (setq volatile? (variable-binding-volatile struct))

    (cond
      ((eq binding-variety :lexical)
       (if (substitute-variables-p init-info? lisp-type c-type)
	   (setf (variable-binding-c-identifier struct)
		 (variable-binding-c-identifier (cons-second init-info?)))
	   (let ((c-identifier
		   (lexical-c-variable-identifier
		     var c-func c-type
		     (if volatile? '("volatile") nil))))
	     (push (cons c-identifier c-type) temp-var-pairs)
	     (setf (variable-binding-c-identifier struct) c-identifier)
	     (translate-l-expr-into-c
	       init-l-expr c-func c-compound-statement (list c-identifier)))))
      ((eq binding-variety :special)
       (let ((new-value (lexical-c-variable-identifier
			  'temp c-func c-type
			  (storage-classes-for-env inner-env)))
	     (global-var
	       (c-identifier-for-variable
		 var *global-c-namespace* (c-func-namespace c-func))))
	 (when (register-used-variable c-file var global-var c-type)
	   (register-needed-variable-extern c-file '("extern") c-type
					    global-var))
	 (push (cons new-value c-type) temp-var-pairs)
	 (setf (variable-binding-c-identifier struct) global-var)
	 (setq specials-to-unbind (nconc specials-to-unbind (list global-var)))
	 (when (null thread-state-var)
	   (setq thread-state-var (lexical-c-variable-identifier
				   'ts c-func '(pointer thread-state) nil))
	   (emit-expr-to-compound-statement
	    (make-c-infix-expr (make-c-name-expr thread-state-var)
			       "=" "THREAD_STATE")
	    c-compound-statement))
	 (when (and safety? (null expected-tos))
	   (setq expected-tos
		 (reusable-c-variable-identifier
		   'expected-top-of-stack c-func 'sint32 inner-env))
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr
	       (make-c-name-expr expected-tos) "=" 
	       (make-c-indirect-selection-expr 
		(make-c-name-expr thread-state-var) "throw_stack_top"))
	     c-compound-statement))
	 (translate-l-expr-into-c
	   init-l-expr c-func c-compound-statement (list new-value))
	 (bind-global-for-let global-var new-value c-compound-statement 
			      thread-state-var)))
      (t
       (translation-error
	 "Unknown binding type ~s for ~s." binding-variety var)))
	finally
	  (if specials-to-unbind
	      (let ((let*-result?
		      (unless (return-directive-discards-values-p
				return-directive c-func)
			(lexical-c-variable-identifier
			  'temp c-func (l-expr-c-return-type let*-l-expr)
			  temp-storage-classes))))
		(when let*-result?
		  (push (cons let*-result? (l-expr-c-return-type let*-l-expr))
			temp-var-pairs))
		(translate-progn-body
		  body c-func c-compound-statement
		  (if let*-result? (list let*-result?) :discard))
		(unbind-globals-for-let specials-to-unbind
					c-compound-statement thread-state-var)

		;; Emit code to check that the top-of-stack has been restored to
		;; the expected value.
		(when safety?
		  (emit-statement-to-compound-statement
		    (make-c-conditional-statement
		      (list (make-c-infix-expr
			      (make-c-name-expr expected-tos)
			      "!=" (make-c-indirect-selection-expr 
				    (make-c-name-expr thread-state-var) 
				    "throw_stack_top"))
			    (make-c-expr-statement
			      (make-c-function-call-expr
				(make-c-name-expr "error")
				(list
				  (make-c-literal-expr
				    "Corrupted Throw_stack_top at let*."))))))
		    c-compound-statement))
		
		(reclaim-reusable-c-variables c-func temp-var-pairs)
		(when let*-result?
		  (return
		    (emit-c-expr-as-directed
		      (make-c-name-expr let*-result?) let*-l-expr c-func
		      c-compound-statement return-directive))))
	      (return
		(prog1
		    (translate-progn-body
		      body c-func c-compound-statement return-directive)
		  (reclaim-reusable-c-variables
		    c-func temp-var-pairs))))))

(def-l-expr-method translate-l-expr-into-c
    (macrolet-l-expr c-func c-compound-statement return-directive)
  (translate-progn-body
    (cons-cddr (l-expr-form macrolet-l-expr))
    c-func c-compound-statement return-directive))

(def-l-expr-method translate-l-expr-into-c
    (multiple-value-bind-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form multiple-value-bind-l-expr))
	 (lisp-vars (cons-second form))
	 (init-l-expr (cons-third form))
	 (body (cons-cdddr form))
	 (inner-env (l-expr-aug-env multiple-value-bind-l-expr))
	 (temp-storage-classes (storage-classes-for-env inner-env))
	 (c-return-type (l-expr-c-return-type multiple-value-bind-l-expr))
	 (c-file (c-func-c-file c-func))
	 (specials-and-inits nil)
	 (thread-state-var nil)
	 (non-object-vars-and-inits nil)
	 (temp-var-pairs nil)
	 (c-vars nil)
	 binding-variety decls struct c-type volatile?)
    (loop for var in lisp-vars
	  for first? = t then nil
	  do
      (multiple-value-setq-some (binding-variety nil decls)
	(tl:variable-information var inner-env))
      (setq struct (cons-cdr (assq 'variable-binding-structure decls)))
      (setq c-type (variable-binding-c-type struct))
      (setq volatile? (variable-binding-volatile struct))
      (cond
	((and (eq binding-variety :lexical)
	      (not first?)
	      (not (satisfies-c-required-type-p c-type 'obj)))
	 (let ((new-ident (lexical-c-variable-identifier
			    var c-func c-type
			    (if volatile? '("volatile") nil)))
	       (temp-ident (lexical-c-variable-identifier
			     'temp c-func 'obj temp-storage-classes)))
	   (push (cons new-ident c-type) temp-var-pairs)
	   (push (cons temp-ident 'obj) temp-var-pairs)
	   (push (list c-type new-ident temp-ident) non-object-vars-and-inits)
	   (push temp-ident c-vars)
	   (setf (variable-binding-c-identifier struct) new-ident)))
	((eq binding-variety :lexical)
	 (let ((new-ident (lexical-c-variable-identifier
			    var c-func c-type
			    (if volatile? '("volatile") nil))))
	   (push (cons new-ident c-type) temp-var-pairs)
	   (push new-ident c-vars)
	   (setf (variable-binding-c-identifier struct) new-ident)))
	;; We only have globals of type 'obj now.  If we change that, then we
	;; will need to fix this up to do some type casting like we did for the
	;; non-object lexicals.  -jra 1/22/96
	((and (eq binding-variety :special) (eq c-type 'obj))
	 (let ((new-ident (lexical-c-variable-identifier
			    'temp c-func 'obj temp-storage-classes))
	       (global-ident
		 (c-identifier-for-variable
		   var *global-c-namespace* (c-func-namespace c-func))))
	   (when (register-used-variable c-file var global-ident c-type)
	     (register-needed-variable-extern c-file '("extern") c-type
					      global-ident))
	   (push (cons new-ident 'obj) temp-var-pairs)
	   (push new-ident c-vars)
	   (push (list global-ident new-ident) specials-and-inits)
	   (setf (variable-binding-c-identifier struct) global-ident)))
	(t
	 (translation-error
	   "Unknown binding variety or type for ~s: ~s and ~s."
	   var binding-variety c-type))))
    (setq c-vars (nreverse c-vars))
    (setq specials-and-inits (nreverse specials-and-inits))
    (setq non-object-vars-and-inits (nreverse non-object-vars-and-inits))

    ;; Evaluate the initalization into the c-vars.
    (translate-l-expr-into-c init-l-expr c-func c-compound-statement c-vars)

    ;; Fix up non-object secondary values.
    (loop for (c-type new-var temp-var) in non-object-vars-and-inits do
      (emit-expr-to-compound-statement
	(make-c-infix-expr
	  (make-c-name-expr new-var)
	  "=" (coerce-c-expr-result-to-type
		(make-c-name-expr temp-var) 'obj c-type inner-env))
	c-compound-statement))

    ;; Bind the global variables.
    (when specials-and-inits
      (setq thread-state-var (lexical-c-variable-identifier
			      'ts c-func '(pointer thread-state) nil))
      (emit-expr-to-compound-statement
       (make-c-infix-expr (make-c-name-expr thread-state-var)
			  "=" "THREAD_STATE")
       c-compound-statement)
      (rebind-globals-for-let 
       (loop for (global-var temp-var) in specials-and-inits 
	     collect (cons global-var temp-var))
       c-compound-statement thread-state-var))

    ;; Evaluate the body, unbinding the globals we have any.
    (cond
      ((and (or (eq return-directive :c-expr)
		(and (eq return-directive :return)
		     (not (eq c-return-type 'void))))
	    specials-and-inits)
       (let ((result (lexical-c-variable-identifier
		       'temp c-func c-return-type temp-storage-classes)))
	 (push (cons result c-return-type) temp-var-pairs)
	 (translate-progn-body body c-func c-compound-statement (list result))
	 (unbind-globals-for-let
	   (loop for (global) in specials-and-inits collect global)
	   c-compound-statement thread-state-var)
	 (reclaim-reusable-c-variables c-func temp-var-pairs)
	 (emit-c-expr-as-directed 
	   (make-c-name-expr result)
	   multiple-value-bind-l-expr c-func c-compound-statement
	   return-directive)))
      (t
       (prog1
	   (translate-progn-body
	     body c-func c-compound-statement return-directive)
	 (when specials-and-inits
	   (unbind-globals-for-let
	     (loop for (global) in specials-and-inits collect global)
	     c-compound-statement thread-state-var)))))))

(def-l-expr-method translate-l-expr-into-c
    (multiple-value-prog1-l-expr c-func c-body return-directive)
  (let* ((mvp1 multiple-value-prog1-l-expr)
	 (temp-storage-classes (storage-classes-for-env (l-expr-env mvp1)))
	 (form (l-expr-form mvp1))
	 (values-l-expr (cons-second form))
	 (l-expr-body (cons-cddr form))
	 (lisp-type (l-expr-lisp-return-type mvp1))
	 (c-type (l-expr-c-return-type mvp1)))
    (cond
      ((return-directive-discards-values-p return-directive c-func)
       (translate-progn-body (cons-cdr form) c-func c-body return-directive))
      ((not (type-includes-values-p lisp-type))
       (let ((result-var (lexical-c-variable-identifier
			   'temp c-func c-type temp-storage-classes)))
	 (translate-l-expr-into-c values-l-expr c-func c-body (list result-var))
	 (translate-progn-body l-expr-body c-func c-body :discard)
	 (reclaim-reusable-c-variables c-func (list (cons result-var c-type)))
	 (emit-c-expr-as-directed
	   (make-c-name-expr result-var) mvp1 c-func c-body return-directive)))
      (t
       ;; In the type propagation in the walker for mvp1, if the lisp required
       ;; type includes multiple values, then the c-required-type for the values
       ;; form is always forced to Obj.  This ensures that we can always cache
       ;; the values onto the stack in this translation here.  -jra 1/23/96
       (let ((result-var (lexical-c-variable-identifier
			   'temp c-func c-type temp-storage-classes))
	     (kept-values-count
	       (lexical-c-variable-identifier
		 'temp-values-count c-func 'sint32 temp-storage-classes))
	     (kept-values-array
	       (lexical-c-variable-identifier
		 'temp-values-array c-func
		 (list 'array 'obj tl:multiple-values-limit)
		 nil)))
	 (translate-l-expr-into-c values-l-expr c-func c-body (list result-var))
	 (emit-copy-value-buffer-statements
	   kept-values-array "Values_buffer" kept-values-count "Values_count"
	   c-body)
	 (translate-progn-body l-expr-body c-func c-body :discard)
	 (emit-copy-value-buffer-statements
	   "Values_buffer" kept-values-array "Values_count" kept-values-count
	   c-body)
	 (emit-c-expr-as-directed
	   (make-c-name-expr result-var) mvp1 c-func c-body
	   return-directive))))))

(def-l-expr-method translate-l-expr-into-c
    (progn-l-expr c-func c-body return-directive)
  (translate-progn-body
    (cons-cdr (l-expr-form progn-l-expr)) c-func c-body return-directive))

(def-l-expr-method translate-l-expr-into-c
    (quote-l-expr c-func c-body return-directive)
  (translate-quoted-value-into-c
    (l-expr-form quote-l-expr)
    (l-expr-c-return-type quote-l-expr)
    (c-func-c-file c-func)
    quote-l-expr c-func c-body return-directive))

(defun translate-quoted-value-into-c
    (value c-type c-file quote-l-expr c-func c-body return-directive)
  (cond
    ((satisfies-c-required-type-p c-type 'boolean)
     (emit-c-expr-as-directed (make-c-literal-expr (if value 1 0))
			      quote-l-expr c-func c-body return-directive))
    ((or (and (fixnump value)
	      (loop for type in '(sint32 uint32 sint32 uint16 uint8)
		    thereis (satisfies-c-required-type-p c-type type)))
	 (and (tl-typep value 'double-float)
	      (satisfies-c-required-type-p c-type 'double)))
     (emit-c-expr-as-directed (make-c-literal-expr value)
			      quote-l-expr c-func c-body return-directive))
    ((and (fixnump value)
	  (satisfies-c-required-type-p c-type 'long))
     (emit-c-expr-as-directed (make-c-literal-expr (cons value 'long))
			      quote-l-expr c-func c-body return-directive))
    ((and (fixnump value)
	  (satisfies-c-required-type-p c-type 'obj))
     (emit-c-expr-as-directed
      (make-c-function-call-expr (make-c-name-expr "BOXFIX") 
				 (list (make-c-literal-expr value)))
      quote-l-expr c-func c-body return-directive))
    ((tl-typep value 'character)
     (let ((c-char-expr (make-c-literal-expr value)))
       (emit-c-expr-as-directed
	 (cond
	   ((satisfies-c-required-type-p c-type 'obj)
	    (coerce-c-expr-result-to-type
	      c-char-expr 'unsigned-char c-type (l-expr-env quote-l-expr)))
	   ((satisfies-c-required-type-p c-type 'unsigned-char)
	    (if (<= 0 (char-code value) 127)
		c-char-expr
		(make-c-cast-expr 'unsigned-char c-char-expr)))
	   ((satisfies-c-required-type-p c-type 'char)
	    c-char-expr)
	   (t
	    (translation-error "Can't emit ~s as c-type ~s." value c-type)))
	 quote-l-expr c-func c-body return-directive)))
    ((tl-typep value 'double-float)
     (translate-double-float-constant-into-c
       value c-type c-file quote-l-expr c-func c-body return-directive))
    ((stringp value)
     (translate-string-constant-into-c
       value c-type c-file quote-l-expr c-func c-body return-directive))
;    ((managed-float-p value)
;     (translate-managed-float-constant-into-c
;       value c-file quote-l-expr c-func c-body return-directive))
    ((or (tl-typep value '(array (unsigned-byte 8)))
	 (tl-typep value '(array (unsigned-byte 16))))
     (translate-byte-array-constants-into-c
       value c-type c-file quote-l-expr c-func c-body return-directive))
    ((tl-typep value '(array double-float))
     (translate-float-array-constants-into-c
       value c-type c-file quote-l-expr c-func c-body return-directive))
    ((simple-vector-p value)
     (translate-simple-vector-constants-into-c
       value c-type c-file quote-l-expr c-func c-body return-directive))
    ((consp value)
     (translate-cons-tree-constant-into-c
       value c-file quote-l-expr c-func c-body return-directive))
    ((symbolp value)
     (translate-symbol-constant-into-c
       value c-file quote-l-expr c-func c-body return-directive))
    ((compiled-function-p value)
     (translation-error
       "Can't translate a reference to the constant ~s.  Try an unevaluated ~
        reference to this compiled-function through the FUNCTION special form ~
        (which the reader macro #' expands into)."
       value))
    (t
     (translation-error "Quote-l-expr doesn't have a case for ~s" value))))

(defun declared-const-array-type (c-array-type length c-file)
  (let ((const-array-type (c-type-for-const-array c-array-type length)))
    (unless (member const-array-type (c-file-defined-c-types c-file)
		    :test #'equal)
      (emit-declaration-to-c-file
	(make-c-typedef-decl const-array-type (c-type-string const-array-type))
	(or (c-file-h-file c-file) c-file)
	0)
      (push const-array-type (c-file-defined-c-types c-file)))
    const-array-type))

(defun translate-string-constant-into-c
    (value c-type c-file quote-l-expr c-func c-body return-directive)
  (cond
    ((satisfies-c-required-type-p c-type '(pointer char))
     (emit-c-expr-as-directed
       (make-c-literal-expr value)
       quote-l-expr c-func c-body return-directive))
    (t
     (let ((comment-string-max-length 50)
	   (string-var (gethash value (c-file-used-constants c-file)))
	   (const-array-type
	     (declared-const-array-type 'str (length value) c-file)))
       (declare (fixnum comment-string-max-length))
       (when (null string-var)
	 (setq string-var
	       (c-identifier-for-string
		 "str_const" '(variable) (c-file-namespace c-file)
		 (c-func-namespace c-func)))
	 (setf (gethash value (c-file-used-constants c-file))
	       string-var)
	 (emit-declaration-to-c-file
	   (make-c-var-decl
	     '("static" "const") const-array-type string-var
	     (make-c-str-init-expr value))
	   c-file 0))
       (emit-c-expr-as-directed
	(make-c-line-comment-expr
	 (cond 
	  ((satisfies-c-required-type-p c-type '(pointer unsigned-char))
	   (make-c-indirect-selection-expr
	    (make-c-cast-expr 
	     '(pointer str)
	     (make-c-unary-expr #\& (make-c-name-expr string-var)))
	    "body"))
	  ((satisfies-c-required-type-p c-type '(pointer str))
	   (make-c-cast-expr
	    '(pointer str)
	    (make-c-unary-expr #\& (make-c-name-expr string-var))))
	  (t
	   (make-c-cast-expr
	    'obj (make-c-unary-expr #\& (make-c-name-expr string-var)))))
	 (let ((*print-pretty* nil)
	       (length (length value)))
	   (declare (fixnum length))
	   (if (> length comment-string-max-length)
	       (format nil "\"~a...\"" (subseq value 0 comment-string-max-length))
	     (let ((new-string (make-string (+ length 2))))
	       (setf (schar new-string 0) #\")
	       (loop for index fixnum from 0 below length
		   for new-index fixnum = (+ index 1) do
		     (setf (schar new-string new-index)
		       (char value index)))
	       (setf (schar new-string (+ length 1)) #\")
	       new-string))))
	quote-l-expr c-func c-body return-directive)))))

;(defun translate-managed-float-constant-into-c
;    (value c-file quote-l-expr c-func c-body return-directive)
;  (let* ((float-value (aref (the (array double-float)
;                                 (managed-float-array-value value))
;                            0))
;         (c-identifier (c-identifier-for-string
;                         "managed_float" '(variable) (c-file-namespace c-file)
;                         (c-func-namespace c-func)))
;         (top-level-c-body (c-file-top-level-compound-statement c-file)))
;    (emit-declaration-to-c-file
;      (make-c-var-decl '("static") 'obj c-identifier nil)
;      c-file 0)
;    (emit-expr-to-compound-statement
;      (make-c-infix-expr
;        (make-c-name-expr c-identifier) "="
;        (make-c-function-call-expr
;          (make-c-name-expr "alloc_mdouble")
;          (list (make-c-literal-expr float-value)
;                (make-c-literal-expr
;                  (region-number-for-type-and-area
;                    'managed-float
;                    (declared-area-name
;                      (l-expr-env quote-l-expr) 'managed-float)))
;                (make-c-literal-expr (c-type-tag 'mdouble)))))
;      top-level-c-body)
;    (emit-c-expr-as-directed
;      (make-c-name-expr c-identifier) quote-l-expr c-func c-body
;      return-directive)))

(defun translate-byte-array-constants-into-c
    (value c-type c-file quote-l-expr c-func c-body return-directive)
  (let* ((one-byte? (tl-typep value '(array (unsigned-byte 8))))
	 (array-var
	   (c-identifier-for-string
	     (if one-byte? "sa_uint8_const" "sa_uint16_const")
	     '(variable) (c-file-namespace c-file)
	     (c-func-namespace c-func)))
	 (const-array-type (declared-const-array-type
			     (if one-byte? 'sa-uint8 'sa-uint16)
			     (length value) c-file)))
    (emit-declaration-to-c-file
      (make-c-var-decl '("static") const-array-type array-var
		       (if one-byte?
			   (make-c-uint8-init-expr value)
			   (make-c-uint16-init-expr value)))
      c-file 0)
    (emit-c-expr-as-directed
      (if (satisfies-c-required-type-p
	    c-type (if one-byte? '(pointer uint8) '(pointer uint16)))
	  (make-c-direct-selection-expr
	    (make-c-name-expr array-var) "body")
	  (make-c-cast-expr
	    'obj (make-c-unary-expr #\& (make-c-name-expr array-var))))
      quote-l-expr c-func c-body return-directive)))

(defun translate-float-array-constants-into-c
    (value c-type c-file quote-l-expr c-func c-body return-directive)
  (let* ((array-var
	   (c-identifier-for-string
	     "sa_double_const"
	     '(variable) (c-file-namespace c-file)
	     (c-func-namespace c-func)))
	 (const-array-type (declared-const-array-type
			     'sa-double (length value) c-file)))
    (emit-declaration-to-c-file
      (make-c-var-decl '("static") const-array-type array-var
		       (make-c-double-init-expr value))
      c-file 0)
    (emit-c-expr-as-directed
      (if (satisfies-c-required-type-p c-type '(pointer double))
	  (make-c-direct-selection-expr
	    (make-c-name-expr array-var) "body")
	  (make-c-cast-expr
	    'obj (make-c-unary-expr #\& (make-c-name-expr array-var))))
      quote-l-expr c-func c-body return-directive)))

(defun translate-simple-vector-constants-into-c
    (value c-type c-file quote-l-expr c-func c-body return-directive)
  (let* ((array-var (c-identifier-for-string
		      "sv_const" '(variable)
		      (c-file-namespace c-file) (c-func-namespace c-func)))
	 (length (length value))
	 (top-level-c-func (c-file-top-level-function c-file))
	 (top-level-c-body (c-file-top-level-compound-statement c-file))
	 (expressionized-array (make-array length))
	 (simplified-array (make-array length))
	 (sv-init-body (make-c-compound-statement nil nil nil nil))
	 (array-expr (make-c-direct-selection-expr
		       (make-c-name-expr array-var) "body"))
	 (null-expr (translate-quoted-value-into-c
		      nil 'obj c-file quote-l-expr
		      top-level-c-func top-level-c-body :c-expr)))
    (declare (fixnum length))
    (loop for index fixnum from 0 below length
	  for old-elt = (svref value index)
	  for elt-expression = (translate-quoted-value-into-c
				 old-elt 'obj c-file quote-l-expr
				 top-level-c-func top-level-c-body :c-expr)
	  do
      (setf (svref expressionized-array index) elt-expression)
      (cond ((or (null old-elt) (fixnump old-elt) (characterp old-elt))
	     (setf (svref simplified-array index) elt-expression))
	    (t
	     (setf (svref simplified-array index) null-expr)
	     (emit-expr-to-compound-statement
	       (make-c-infix-expr
		 (make-c-subscript-expr array-expr
					(make-c-literal-expr index))
		 "=" elt-expression)
	       sv-init-body))))

    (unless (c-compound-statement-empty-p sv-init-body)
      (emit-statement-to-compound-statement
	(make-c-#if-statement
	  (list (make-c-function-call-expr
		  (make-c-name-expr "defined")
		  (list (make-c-name-expr "NO_ADDRESS_CONSTANTS")))
		sv-init-body))
	top-level-c-body))

    (emit-declaration-to-c-file
      (make-c-var-decl
	'("static") (declared-const-array-type 'sv length c-file) array-var
	(make-c-sv-init-expr
	  length
	  (if (c-compound-statement-empty-p sv-init-body)
	      (make-c-array-init-expr simplified-array)
	      (make-c-#if-expr
		(list
		  (make-c-function-call-expr
		    (make-c-name-expr "defined")
		    (list (make-c-name-expr "NO_ADDRESS_CONSTANTS")))
		  (make-c-array-init-expr simplified-array)
		  nil
		  (make-c-array-init-expr expressionized-array))))))
      c-file 0)

    (emit-c-expr-as-directed
      (if (satisfies-c-required-type-p c-type '(pointer obj))
	  (make-c-direct-selection-expr (make-c-name-expr array-var) "body")
	  (make-c-cast-expr
	    'obj (make-c-unary-expr #\& (make-c-name-expr array-var))))
      quote-l-expr c-func c-body return-directive)))

(defun translate-double-float-constant-into-c
    (value c-type c-file quote-l-expr c-func c-body return-directive)
  (if (satisfies-c-required-type-p c-type 'double)
      (emit-c-expr-as-directed
	(make-c-literal-expr value)
	quote-l-expr c-func c-body return-directive)
      (let ((ldouble-var (gethash value (c-file-used-constants c-file))))
	(when (null ldouble-var)
	  (setq ldouble-var
		(c-identifier-for-string
		  "ldouble_const" '(variable) (c-file-namespace c-file)
		  (c-func-namespace c-func)))
	  (setf (gethash value (c-file-used-constants c-file))
		ldouble-var)
	  (emit-declaration-to-c-file
	    (make-c-var-decl '("static") 'ldouble ldouble-var
			     (make-c-ldouble-init-expr value))
	    c-file 0))
	(emit-c-expr-as-directed
	  (make-c-cast-expr
	    'obj (make-c-unary-expr #\& (make-c-name-expr ldouble-var)))
	  quote-l-expr c-func c-body return-directive))))

(defun translate-cons-tree-constant-into-c
    (value c-file quote-l-expr c-func c-body return-directive)
  (let* ((cons-hash (make-hash-table :test #'eq))
	 (cons-count 0)
	 (init-vector nil)
	 (simple-init-vector nil)
	 (uninitialized-marker (gensym))
	 (tree-var
	   (c-identifier-for-string
	     "cons_const" '(variable) (c-file-namespace c-file)
	     (c-func-namespace c-func)))
	 (tree-var-expr (make-c-name-expr tree-var))
	 (init-statements (make-c-compound-statement nil nil nil nil))
	 (top-c-func (c-file-top-level-function c-file))
	 (top-c-body (c-file-top-level-compound-statement c-file)))
    (declare (fixnum cons-count))
    (labels ((assign-index (cons)
	       (unless (gethash cons cons-hash)
		 (setf (gethash cons cons-hash) cons-count)
		 (incf cons-count)
		 (loop for next-cons? = (cdr cons) then (cons-cdr next-cons?)
		       while (consp next-cons?)
		       do
		   (assign-index next-cons?))
		 (loop with car
		       for next-cons? = cons then (cons-cdr next-cons?)
		       while (consp next-cons?)
		       do
		   (setq car (cons-car next-cons?))
		   (when (consp car)
		     (assign-index car)))))

	     (cons-expr (cons)
	       (let ((index (make-c-literal-expr
			      (* (gethash cons cons-hash) 2))))
		 (make-c-cast-expr
		   'obj (make-c-infix-expr
			  (make-c-cast-expr
			    'uint32 (make-c-unary-expr
				      #\& (make-c-subscript-expr
					    tree-var-expr index)))
			  "+" 2))))

	     (constant-expr (lisp-object)
	       (if (consp lisp-object)
		   (cons-expr lisp-object)
		   (translate-quoted-value-into-c
		     lisp-object 'obj c-file quote-l-expr
		     top-c-func top-c-body :c-expr)))

	     (init-cons (cons)
	       (let ((car-index (* (gethash cons cons-hash) 2)))
		 (declare (fixnum car-index))
		 (when (eq (svref init-vector car-index)
			   uninitialized-marker)
		   (let ((length
			   (loop for last-index fixnum = (- car-index 2) then next-index
				 for next-cons = cons then (cons-cdr next-cons)
				 for next-index fixnum = car-index
						then (* (if (consp next-cons)
							    (gethash next-cons cons-hash)
							  0)
							2)
				 while (and (consp next-cons)
					    (= (the fixnum (+ last-index 2)) next-index))
				 count t)))
		     (declare (fixnum length))
		     (cond
		       ;; The function call to initialize the cdrs isn't worth
		       ;; it unless we can get 3 or more cdrs initialized in
		       ;; that one call.
		       ((> length 2)
			(emit-expr-to-compound-statement
			  (make-c-function-call-expr
			    (make-c-name-expr "hook_up_cdrs")
			    (list
			      (if (= car-index 0)
				  tree-var-expr
				  (make-c-unary-expr
				    #\& (make-c-subscript-expr
					  tree-var-expr
					  (make-c-literal-expr car-index))))
			      (make-c-literal-expr length)
			      (constant-expr (cdr (last cons)))))
			  init-statements)
			;; Then, initialize the cars and put NILs into the
			;; simple initialization vector for the cdrs.
			(loop with next-cons?
			      for index fixnum from car-index by 2
			      for this-cons = cons then next-cons?
			      while (consp this-cons)
			      do
			  (setq next-cons? (cons-cdr this-cons))
			  (init-car-or-cdr-at-index
			    (cons-car this-cons) index nil)
			  (setf (svref simple-init-vector (+ index 1))
				(constant-expr nil))
			  (init-car-or-cdr-at-index
			    next-cons? (1+ index) t)))
		       (t
			(init-car-or-cdr-at-index (car cons) car-index nil)
			(init-car-or-cdr-at-index
			  (cdr cons) (1+ car-index) nil)))))))

	     (init-car-or-cdr-at-index
		 (car-or-cdr-value car-or-cdr-index complex-init-only?)
	       (let ((expr (constant-expr car-or-cdr-value)))
		 (setf (svref init-vector car-or-cdr-index) expr)
		 (cond
		   (complex-init-only?
		    nil)
		   ((or (fixnump car-or-cdr-value) (null car-or-cdr-value)
			(characterp car-or-cdr-value))
		    (setf (svref simple-init-vector car-or-cdr-index) expr))
		   (t
		    (setf (svref simple-init-vector car-or-cdr-index)
			  (constant-expr nil))
		    (emit-expr-to-compound-statement
		      (make-c-infix-expr
			(make-c-subscript-expr
			  tree-var-expr (make-c-literal-expr car-or-cdr-index))
			"=" expr)
		      init-statements)
		    (when (and (not complex-init-only?)
			       (consp car-or-cdr-value))
		      (init-cons car-or-cdr-value)))))))
      (assign-index value)
      (setq init-vector (make-array (* cons-count 2)
				    :initial-element uninitialized-marker))
      (setq simple-init-vector (make-array (* cons-count 2)))
      (init-cons value)
      (unless (c-compound-statement-empty-p init-statements)
	(emit-statement-to-compound-statement
	  (make-c-#if-statement
	    (list (make-c-function-call-expr
		    (make-c-name-expr "defined")
		    (list (make-c-name-expr "NO_ADDRESS_CONSTANTS")))
		  init-statements))
	  top-c-body))

      (emit-declaration-to-c-file
	(make-c-var-decl
	  '("static") (list 'array 'obj (* cons-count 2)) tree-var
	  (if (c-compound-statement-empty-p init-statements)
	      (make-c-array-init-expr simple-init-vector)
	      (make-c-#if-expr
		(list (make-c-function-call-expr
			(make-c-name-expr "defined")
			(list (make-c-name-expr "NO_ADDRESS_CONSTANTS")))
		      (make-c-array-init-expr simple-init-vector)
		      nil
		      (make-c-array-init-expr init-vector)))))
	c-file 0)
      (emit-c-expr-as-directed
	(cons-expr value) quote-l-expr c-func c-body return-directive))))

(defun translate-symbol-constant-into-c
    (value c-file quote-l-expr c-func c-body return-directive)
  (cond
    
    ((null value)
     (emit-c-expr-as-directed
       (make-c-cast-expr 'obj (make-c-name-expr "NULL"))
       quote-l-expr c-func c-body return-directive))
    ((eq value t)
     (emit-c-expr-as-directed
       (make-c-cast-expr 'obj (make-c-unary-expr #\& (make-c-name-expr "T")))
       quote-l-expr c-func c-body return-directive))
    (t
     (let ((symbol-reference? (gethash value *global-symbol-registry*))
	   (last-definition? (c-file-last-symbol-definition? c-file)))
       (cond
	 (symbol-reference?
	  ;; Don't register an extern to the symbol array for the current file.
	  (unless (and last-definition?
		       (string= (cons-car last-definition?)
				(cons-car symbol-reference?)))
	    (register-needed-variable-extern
	      c-file '("extern") '(array sym) (cons-car symbol-reference?))
	    (setf (gethash value (c-file-used-symbols c-file))
		  symbol-reference?)))
	 (t
	  (let ((new-definition 
		  (if last-definition?
		      (cons (cons-first last-definition?)
			    (1+ (cons-cdr last-definition?)))
		      (cons
			(c-identifier-for-symbol
			  (intern (format nil "~(~a_~a~)_symbols"
					  *current-system-name*
					  *current-module-name*))
			  '(variable) *global-c-namespace*
			  (c-func-namespace c-func))
			0))))
	    (setf (gethash value *global-symbol-registry*) new-definition)
	    (setf (c-file-last-symbol-definition? c-file) new-definition)
	    (push value (c-file-symbols-defined c-file))
	    (when (and (not (memq (symbol-package value) *global-package-registry*))
		       (not (null (symbol-package value))))
	      (push (symbol-package value) *global-package-registry*))
	    (setq symbol-reference? new-definition))))
       (emit-c-expr-as-directed
	 (make-c-line-comment-expr 
	   (make-c-cast-expr
	     'obj (make-c-infix-expr
		    (make-c-name-expr (cons-car symbol-reference?))
		    "+" (make-c-literal-expr (cons-cdr symbol-reference?))))
	   (symbol-name value))
	 quote-l-expr c-func c-body return-directive)))))

(defun translate-compiled-function-constant-into-c
    (compiled-function-name decls c-file l-expr? c-func c-body return-directive)
  (let ((compiled-func-reference?
	  (or (gethash compiled-function-name *global-compiled-function-registry*)
	      (make-and-initialize-new-compiled-function-reference
	        compiled-function-name decls c-file c-func l-expr?))))
    (register-needed-variable-extern
      c-file '("extern") '(array func)
      (cons-car compiled-func-reference?))
    (setf (gethash compiled-function-name
		   (c-file-used-compiled-functions c-file))
	  compiled-func-reference?)
    (emit-c-expr-as-directed
      (make-c-cast-expr
	'obj (make-c-unary-expr
	       #\& (make-c-subscript-expr
		     (make-c-name-expr (cons-car compiled-func-reference?))
		     (make-c-line-comment-expr
		       (make-c-literal-expr (cons-cdr compiled-func-reference?))
		       (let ((*print-pretty* nil))
			 (format nil "#'~a" compiled-function-name))))))
      l-expr? c-func c-body return-directive)))

(defun make-global-compiled-function-location (compiled-function-name c-file c-func)
  (let* ((last-definition? (c-file-last-compiled-function-definition? c-file))
	 (new-definition 
	   (if last-definition?
	       (cons (cons-first last-definition?)
		     (1+ (cons-cdr last-definition?)))
	       (cons
		 (c-identifier-for-symbol
		   (intern (format nil "~(~a_~a~)_funcs"
				   *current-system-name* *current-module-name*))
		   '(variable) *global-c-namespace* (c-func-namespace c-func))
		 0))))
    (setf (gethash compiled-function-name *global-compiled-function-registry*)
	  new-definition)
    (setf (c-file-last-compiled-function-definition? c-file)
	  new-definition)
    (push compiled-function-name (c-file-compiled-functions-defined c-file))
    new-definition))

(defun make-and-initialize-new-compiled-function-reference
    (compiled-function-name decls c-file c-func l-expr?)
  (let* ((new-definition (make-global-compiled-function-location
			   compiled-function-name c-file c-func))
	 (ftype (or (cdr (assq 'ftype decls))
		    (cdr (assq 'computed-ftype decls))))
	 (c-return-type
	   (if ftype
	       (c-func-type-for-lisp-func-return-type-spec (cons-third ftype))
	       (translation-error
		 "Compiling (FUNCTION ~s), but that function is not defined."
		 compiled-function-name)))
	 (c-argument-types
	   (loop for arg-lisp-type in (cons-second ftype)
		 unless (eq arg-lisp-type '&optional)
		   collect (c-type-for-lisp-type arg-lisp-type)))
	 (arg-count (length c-argument-types))
	 (optional-argument-defaults
	   (loop for default in (cdr (assq 'optional-arg-default-values decls))
		 collect (cond
			   ((constantp default)
			    (eval default))
			   (t
			    (translation-warning
			      "A default value, ~s, of an optional argument of ~
                               ~s was not a constant and it might be funcalled.  ~
                               TL can't handle this, using NIL instead."
			      default
			      compiled-function-name)
			    nil))))
	 (top-c-func (c-file-top-level-function c-file))
	 (top-c-body (c-func-body-statement top-c-func))
	 (c-func-identifier (c-identifier-for-function
			      compiled-function-name *global-c-namespace*
			      (c-func-namespace c-func)))

	 (compiled-function
	   (make-c-subscript-expr
	     (make-c-name-expr (cons-car new-definition))
	     (make-c-literal-expr (cons-cdr new-definition))))
	 (env (if l-expr? (l-expr-env l-expr?) nil))
	 (safe? (>= (tl:optimize-information 'safety env) 3)))    
    (when (register-used-function
	    c-file compiled-function-name c-func-identifier ftype)
      (register-needed-function-extern
	c-file '("extern") c-return-type c-func-identifier c-argument-types))
    ;; If needed, emit a wrapper function that takes obj arguments and returns
    ;; an obj value.  Then a pointer to the wrapper function will be emitted
    ;; into the compiled-function object instead of the actual implementation of
    ;; this compiled-function.
    (unless (and (or (satisfies-c-required-type-p c-return-type 'obj)
		     (and (satisfies-c-required-type-p 'void c-return-type)
			  (not safe?)))
		 (loop for c-arg-type in c-argument-types
		       always (satisfies-c-required-type-p c-arg-type 'obj)))
      (setq c-func-identifier
	    (make-and-emit-obj-wrapper-function
	      c-return-type c-func-identifier c-argument-types
	      compiled-function-name ftype safe? env top-c-func c-file)))

    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "type")
	"=" (make-c-literal-expr (c-type-tag 'func)))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "arg_count")
	"=" (make-c-literal-expr arg-count))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "optional_arguments")
	"=" (make-c-literal-expr (length optional-argument-defaults)))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "sets_values_count")
	"=" (make-c-literal-expr
	      (if (type-includes-values-p (cons-third ftype)) 1 0)))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "default_arguments")
	"=" (translate-l-expr-into-c
	      (prepare-l-expr-for-translation 
		(make-quoted-constant-l-expr
		  (list 'quote optional-argument-defaults)
		  env env)
		't 'obj)
	      top-c-func top-c-body :c-expr))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "closure_environment")
	"=" (translate-l-expr-into-c
	      (prepare-l-expr-for-translation 
		(make-quoted-constant-l-expr nil env env) 't 'obj)
	      top-c-func top-c-body :c-expr))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "name")
	"=" (translate-l-expr-into-c
	      (prepare-l-expr-for-translation 
		(make-quoted-constant-l-expr
		  (symbol-name compiled-function-name)
		  env env)
		't 'obj)
	      top-c-func top-c-body :c-expr))
      top-c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-direct-selection-expr compiled-function "c_function")
	"=" (make-c-cast-expr '(function obj (obj))
			      (make-c-name-expr c-func-identifier)))
      top-c-body)
    new-definition))




;;; The function `make-and-emit-obj-wrapper-function' will create and emit into
;;; a C file a new wrapper function that takes all arguments as type Obj and
;;; returns all values as type Obj.  It wraps a function which does not take and
;;; return all obj types.  If we are compiling safely, then lots of type
;;; checking will get emitted here, and if the return type of the called
;;; function is void, then we will return the Unbound-value as our bogus return.
;;; This function returns the string that is the C identifier for the new
;;; wrapper function.

(defun make-and-emit-obj-wrapper-function
    (c-return-type c-func-identifier c-argument-types original-lisp-name ftype
		   safe? env top-c-func c-file)
  (let* ((wrapper-name
	   (c-identifier-for-function
	     (intern (format nil "~a-WRAPPER" original-lisp-name))
	    (c-file-namespace c-file)
	    (c-func-namespace top-c-func)))
	 (c-arg-names (loop for index from 1 to (length c-argument-types)
			    collect (format nil "arg_~a" index)))
	 (c-body (make-c-compound-statement nil nil nil nil))
	 (c-func (make-c-func
		   '("static") 'obj wrapper-name nil c-arg-names
		   (loop repeat (length c-argument-types)
			 collect 'obj)
		   (loop repeat (length c-argument-types)
			 collect nil)
		   c-body (c-file-namespace c-file) c-file))
	 (wrapped-call
	   (make-c-function-call-expr
	     (make-c-name-expr c-func-identifier)
	     (loop for c-arg-name in c-arg-names
		   for c-arg-type in c-argument-types
		   for lisp-type in (cons-second ftype)
		   for c-name-expr = (make-c-name-expr c-arg-name)
		   collect
		   (progn
		     (when (and safe? (not (tl-subtypep t lisp-type)))
		       (emit-safety-type-check-to-compound-statement
			 c-name-expr lisp-type t c-body))
		     (if (satisfies-c-required-type-p 'obj c-arg-type)
			 c-name-expr
			 (coerce-c-expr-result-to-type
			   c-name-expr 'obj c-arg-type env)))))))
    (cond
      ((eq c-return-type 'void)
       (emit-expr-to-compound-statement wrapped-call c-body)
       (emit-expr-to-compound-statement
	 (make-c-infix-expr "Values_count" "=" 1)
	 c-body)
       (emit-statement-to-compound-statement
	 (make-c-return-statement (c-unbound-value-expr))
	 c-body))
      (t
       (unless (type-includes-values-p (cons-third ftype))
	 (let ((temp-c-name-expr
		 ;; Pass the global environment to
		 ;; reusable-c-variable-identifier, since this new static
		 ;; function is not embedded within any environments.
		 (make-c-name-expr (reusable-c-variable-identifier
				     'result c-func c-return-type nil))))
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr temp-c-name-expr "=" wrapped-call)
	     c-body)
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr "Values_count" "=" 1)
	     c-body)
	   (setq wrapped-call temp-c-name-expr)))
       (emit-statement-to-compound-statement
	 (make-c-return-statement
	   (if (satisfies-c-required-type-p c-return-type 'obj)
	       wrapped-call
	       (coerce-c-expr-result-to-type
		 wrapped-call c-return-type 'obj env)))
	 c-body)))
    (emit-function-to-c-file c-func c-file)
    wrapper-name))
    
	
	
	    
				
    


(def-l-expr-method translate-l-expr-into-c
      (return-from-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form return-from-l-expr))
	 (target-name (cons-second form))
	 (value-l-expr (cons-third form))
	 (env (l-expr-env return-from-l-expr))
	 (exit-scope (tl:declaration-information 'exit-scope env))
	 (temp-storage-classes (storage-classes-for-env env))
	 (previous-unwind-protect nil)
	 (current-c-body c-body)
	 (target-block (loop for block in exit-scope
			     when (and (eq (cons-car block) 'block)
				       (eq (cons-second block) target-name))
			       return (cons-third block)))
	 (block-scope-return-directive
	   (block-scope-return-directive target-block))
	 (c-type (block-scope-c-return-type target-block))
	 (lisp-type (block-scope-lisp-return-type target-block))
	 (local-result-var? nil)
	 (target-label? (block-scope-target-label target-block))
	 (pre-exit-cleanups nil))
    (loop for entry in exit-scope
	  for type = (cons-car entry) do
      (cond
	((eq type 'block)
	 (when (eq target-name (cons-second entry))
	   (cond
	     (previous-unwind-protect
	      (emit-scope-exit-cleanups
		(reverse pre-exit-cleanups) current-c-body c-func)
	      (emit-c-expr-as-directed
		(make-c-name-expr (protect-value-cache-identifier
				    previous-unwind-protect))
		return-from-l-expr c-func current-c-body
		block-scope-return-directive)
	      (when target-label?
		(emit-statement-to-compound-statement
		  (make-c-goto-statement target-label?) current-c-body)))
	     ((and (not (eq c-type 'void))
		   pre-exit-cleanups
		   (eq block-scope-return-directive :return))
	      (setq local-result-var?
		    (lexical-c-variable-identifier
		      'temp c-func c-type temp-storage-classes))
	      (translate-l-expr-into-c
		value-l-expr c-func current-c-body (list local-result-var?))
	      (emit-scope-exit-cleanups
		(reverse pre-exit-cleanups) current-c-body c-func)
	      ;; Note, no goto is needed here to the target-label since the next
	      ;; statement is guaranteed to emit a return statement, since we
	      ;; verified that the return-directive is :return up above.
	      ;; -jallard 7/31/97
	      (emit-c-expr-as-directed
		(make-c-name-expr local-result-var?)
		return-from-l-expr c-func current-c-body 
		block-scope-return-directive))
	     ((and pre-exit-cleanups
		   (eq block-scope-return-directive :return))
	      (translate-l-expr-into-c
		value-l-expr c-func current-c-body :discard)
	      (emit-scope-exit-cleanups
		(reverse pre-exit-cleanups) current-c-body c-func)
	      (emit-statement-to-compound-statement
		(make-c-return-statement nil) current-c-body))
	     (t
	      (translate-l-expr-into-c
		value-l-expr c-func current-c-body block-scope-return-directive)
	      (when pre-exit-cleanups
		(emit-scope-exit-cleanups
		  (reverse pre-exit-cleanups) current-c-body c-func))
	      (when target-label?
		(emit-statement-to-compound-statement
		  (make-c-goto-statement target-label?) current-c-body))))

	   ;; If the calling l-expr insists on a return value, give them a NIL.
	   ;; Since the c-result-type of this l-expr is 'void, we will end up in
	   ;; coerce-c-expr-result-to-type.  All type coercions from void emit a
	   ;; runtime error if this code is ever executed.  Just for neatness
	   ;; sake, in most cases this code follows the previous return or goto
	   ;; statement, and the dead-code eliminator in the emitter for
	   ;; c-compound-statement declines to emit the whole hulabaloo.  -jra
	   ;; 1/27/96
	   (return (if (eq return-directive :c-expr)
		       (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))))

	((eq type 'require-local-exit)
	 (issue-local-exit-warning target-name (cons-second entry)))

	((eq type 'tagbody-tags)
	 ;; Ignore tagbody statements.
	 nil)

	((eq type 'unwind-protect)
	 (let* ((current-protect (cons-second entry))
		(exit-path
		  (+ (length (protect-exit-descriptions current-protect))
		     2))
		(exit-statement (make-c-compound-statement nil nil nil nil))
		(new-exit (list 'return-from exit-path exit-statement)))
	   (push new-exit (protect-exit-descriptions current-protect))
	   (if (null previous-unwind-protect)
	       (translate-l-expr-into-c
		 value-l-expr c-func current-c-body
		 (list (protect-value-cache-identifier current-protect)))
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-name-expr
		     (protect-value-cache-identifier current-protect))
		   "="
		   (make-c-name-expr
		     (protect-value-cache-identifier previous-unwind-protect)))
		 current-c-body))
	   (if (type-includes-values-p lisp-type)
	       (emit-copy-value-buffer-statements
		 (protect-values-buffer-identifier current-protect)
		 "Values_buffer"
		 (protect-values-count-identifier current-protect)
		 "Values_count"
		 current-c-body)
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-name-expr
		     (protect-values-count-identifier current-protect))
		   "=" 1)
		 current-c-body))
	   (when pre-exit-cleanups
	     (emit-scope-exit-cleanups
	       (reverse pre-exit-cleanups) current-c-body c-func)
	     (setq pre-exit-cleanups nil))
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr
	       (make-c-name-expr (protect-exit-path-identifier current-protect))
	       "=" exit-path)
	     current-c-body)
	   (emit-statement-to-compound-statement
	     (make-c-goto-statement (protect-cleanup-statement-label
				      current-protect))
	     current-c-body)
	   (setq previous-unwind-protect current-protect)
	   (setq current-c-body exit-statement)))
	((memqp type '(global-variable-binding catch))
	 (push entry pre-exit-cleanups))
	(t (translation-error
	     "Unknown exit-type type ~s when seen by return-from ~s."
	     type target-name))))))

(defun emit-copy-value-buffer-statements
    (target-buffer source-buffer values-target values-source c-body)
  (emit-expr-to-compound-statement
    (make-c-infix-expr values-target "=" values-source)
    c-body)
  (emit-statement-to-compound-statement
    (make-c-conditional-statement
      (list
	(make-c-infix-expr values-target ">" 1)
	(make-c-expr-statement 
	  (make-c-function-call-expr
	    (make-c-name-expr "memcpy")
	    (list
	      (make-c-cast-expr
		'(pointer void) (make-c-name-expr target-buffer))
	      (make-c-cast-expr
		'(pointer void) (make-c-name-expr source-buffer))
	      (make-c-infix-expr
		(make-c-infix-expr values-target "-" 1)
		"*" (make-c-sizeof-expr (c-type-string 'obj))))))))
    c-body))

(defun emit-scope-exit-cleanups (cleanups c-compound-statement c-func)
  (loop with c-file = (c-func-c-file c-func)
	with thread-state-var = nil
	for cleanup-cons on cleanups
	for cleanup = (cons-car cleanup-cons)
	for cleanup-type = (cons-car cleanup)
	do
    (when (null thread-state-var)
      (setq thread-state-var (lexical-c-variable-identifier
			      'ts c-func '(pointer thread-state) nil))
      (emit-expr-to-compound-statement
       (make-c-infix-expr 
	(make-c-name-expr thread-state-var) "=" "THREAD_STATE")
       c-compound-statement))
    (cond ((eq cleanup-type 'catch)
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr
	      (make-c-indirect-selection-expr
	       (make-c-name-expr thread-state-var) "throw_stack_top") 
	      "-=" (loop for count fixnum from 3 by 3
		       while (eq (car (second cleanup-cons)) 'catch)
		       do
		   (setq cleanup-cons (cons-cdr cleanup-cons))
		       finally (return count)))
	     c-compound-statement))
	  ((eq cleanup-type 'global-variable-binding)
	   (unbind-globals-for-let
	     (loop for var in (cons-cdr cleanup)
		   for c-name
		       = (c-identifier-for-variable
			   var *global-c-namespace*
			   (c-func-namespace c-func))
		   do
	       (when (register-used-variable c-file var c-name 'obj)
		 (register-needed-variable-extern
		   c-file '("extern") 'obj c-name))
		   collect c-name)
	     c-compound-statement thread-state-var))
	  (t
	   (translation-error "Bad cleanup type ~s" cleanup-type)))))

(def-l-expr-method translate-l-expr-into-c
    (setq-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form setq-l-expr))
	 (symbol (cons-second form))
	 (value-l-expr (cons-third form)))
    (multiple-value-bind (bind-type? local? decls)
	(tl:variable-information symbol (l-expr-env setq-l-expr))
      (declare (ignore local?))
      (let* ((binding? (cdr (assq 'variable-binding-structure decls)))
	     (c-name
	       (if binding?
		   (variable-binding-c-identifier binding?)
		   (c-identifier-for-variable
		     symbol *global-c-namespace* (c-func-namespace c-func))))
	     (c-type (if binding?
			 (variable-binding-c-type binding?)
			 'obj))
	     (c-file (c-func-c-file c-func)))
	(when (and (null binding?)
		   (not (memqp bind-type? '(:special :constant))))
	  (setq bind-type? :special)
	  (translation-warning "Free variable ~s assumed to be special."
			       symbol))
	(unless binding?
	  (when (register-used-variable c-file symbol c-name c-type)
	    (register-needed-variable-extern c-file '("extern") c-type c-name)))
	(emit-c-expr-as-directed
	 (if (eq bind-type? :special)
	     (make-c-function-call-expr
	      (make-c-name-expr "SET_GLOBAL")
	      (list (make-c-name-expr c-name)
		    (translate-l-expr-into-c value-l-expr c-func c-body :c-expr)))
	   (make-c-infix-expr
	    (make-c-name-expr c-name)
	    "=" (translate-l-expr-into-c value-l-expr c-func c-body :c-expr)))
	 setq-l-expr c-func c-body return-directive)))))

(def-l-expr-method translate-l-expr-into-c
    (tagbody-l-expr c-func c-body return-directive)
  ;; The tags for this tagbody should be the first thing on the exit scope.
  (loop with tagbody-tags
	  = (cons-car (tl:declaration-information
			'exit-scope (l-expr-aug-env tagbody-l-expr)))
	with tag-list = (if (eq (car tagbody-tags) 'tagbody-tags)
			    (cons-cdr tagbody-tags)
			    (error "Can't find tagbody-tags!"))
	for l-expr-or-tag in (cons-cdr (l-expr-form tagbody-l-expr))
	do
    (if (l-expr-p l-expr-or-tag)
	(with-reusable-c-variables-scope (c-func)
	  (translate-l-expr-into-c l-expr-or-tag c-func c-body :discard))
	(let ((tag-struct (second (assq l-expr-or-tag tag-list))))
	  (when (null tag-struct)
	    (translation-error
	      "Can't find tag entry for ~s within tagbody!" l-expr-or-tag))
	  (unless (= (the fixnum (tagbody-scope-reference-count tag-struct)) 0)
	    (when (null (tagbody-scope-c-identifier tag-struct))
	      (setf (tagbody-scope-c-identifier tag-struct)
		    (lexical-c-statement-label
		      (if (symbolp l-expr-or-tag)
			  l-expr-or-tag
			  (intern (format nil "TAG-~a" l-expr-or-tag)))
		      c-func)))
	    (emit-statement-to-compound-statement
	      (make-c-label-statement (tagbody-scope-c-identifier tag-struct))
	      c-body)))))
  (unless (memqp return-directive '(:discard nil))
    (emit-c-expr-as-directed
      (make-c-cast-expr 'obj (make-c-name-expr "NULL"))
      tagbody-l-expr c-func c-body return-directive)))




;;; The translation method for `the-l-expr' merely translates its third
;;; argument, since all of the type inferencing has already been done.

(def-l-expr-method translate-l-expr-into-c
    (the-l-expr c-func c-compound-statement return-directive)
  (translate-l-expr-into-c
    (cons-third (l-expr-form the-l-expr))
    c-func c-compound-statement return-directive))

(def-l-expr-method translate-l-expr-into-c
    (throw-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form throw-l-expr))
	 (tag-l-expr (cons-second form))
	 (value-l-expr (cons-third form))
	 (tag-temp
	   (lexical-c-variable-identifier
	     'tag c-func 'obj
	     (storage-classes-for-env (l-expr-env throw-l-expr)))))
    (translate-l-expr-into-c
      tag-l-expr c-func c-body (list tag-temp))
    (emit-expr-to-compound-statement
      (make-c-function-call-expr
	(make-c-name-expr "throw_towards_catch_tag")
	(list (make-c-name-expr tag-temp)
	      (translate-l-expr-into-c
		value-l-expr c-func c-body :c-expr)))
      c-body)
    (reclaim-reusable-c-variables c-func (list (cons tag-temp 'obj)))
    (when (eq return-directive :c-expr)
      (make-c-cast-expr 'obj (make-c-name-expr "NULL")))))

(def-l-expr-method translate-l-expr-into-c
    (unwind-protect-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form unwind-protect-l-expr))
	 (protected-l-expr (cons-second form))
	 (cleanup-l-exprs (cons-cddr form))
	 (c-type (l-expr-c-return-type unwind-protect-l-expr))
	 (lisp-type (l-expr-lisp-return-type unwind-protect-l-expr))
	 (inner-env (l-expr-aug-env unwind-protect-l-expr))
	 (exit-scope (tl:declaration-information 'exit-scope inner-env))
	 (protect-struct
	   (if (eq (caar exit-scope) 'unwind-protect)
	       (cadar exit-scope)
	       (translation-error "Can't find unwind-protect scoping struct.")))
	 (value-cache (lexical-c-variable-identifier
			'pass-through-value c-func 'obj '("volatile")))
	 (values-count
	   (lexical-c-variable-identifier
	     'pass-through-value-count c-func 'sint32 '("volatile")))
	 (values-buffer (lexical-c-variable-identifier
			  'pass-through-value-buffer c-func
			  (list 'array 'obj tl:multiple-values-limit)
			  ;; Array variables do not need to be declared
			  ;; volatile.  -jallard 7/31/97
			  nil))
	 (protected-value
	   (if (or (eq c-type 'obj) (eq c-type 'void))
	       value-cache
	       (lexical-c-variable-identifier
		 'protected-value c-func c-type '("volatile"))))
	 (exit-path (lexical-c-variable-identifier
		      'exit-path c-func 'sint32 '("volatile")))
	 (jump-buf (lexical-c-variable-identifier
		     'unwind-buffer c-func 'jmp-buf nil))
	 (throw-tag (lexical-c-variable-identifier
		      'throw-tag c-func 'obj '("volatile")))
	 (cleanup-label (lexical-c-statement-label 'cleanup c-func))
	 (throw-handler-statement (make-c-compound-statement nil nil nil nil))
	 (protected-statement (make-c-compound-statement nil nil nil nil))
	 (exit-statement (make-c-compound-statement nil nil nil nil))
	 ;; Change the following to 3 later.  -jallard 8/4/97
	 (safety? (>= (tl:optimize-information 'safety inner-env) 3))
	 (expected-tos
	   (when safety?
	     (lexical-c-variable-identifier
	       'expected-top-of-stack c-func 'sint32 '("volatile")))))
    (setf (protect-exit-path-identifier protect-struct) exit-path)
    (setf (protect-value-cache-identifier protect-struct) value-cache)
    (setf (protect-values-count-identifier protect-struct) values-count)
    (setf (protect-values-buffer-identifier protect-struct) values-buffer)
    (setf (protect-cleanup-statement-label protect-struct) cleanup-label)

    ;; Cache the old top-of-stack for debugging purposes.
    (when safety?
      (emit-expr-to-compound-statement
	(make-c-infix-expr (make-c-name-expr expected-tos) "=" "Throw_stack_top")
	c-body))

    ;; First push a pointer to the jump-buffer onto the Throw-stack.
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	"Throw_stack_top" "=" (make-c-infix-expr "Throw_stack_top" "+" 2))
      c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-subscript-expr
	  (make-c-name-expr "Throw_stack") (make-c-name-expr "Throw_stack_top"))
	"=" (make-c-cast-expr 'obj (make-c-literal-expr 3)))
      c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-subscript-expr
	  (make-c-name-expr "Throw_stack")
	  (make-c-infix-expr "Throw_stack_top" "-" 1))
	"=" (make-c-cast-expr
	      'obj (make-c-unary-expr #\& (make-c-name-expr jump-buf))))
      c-body)

    ;; Though the following initializations are not strictly necessasry,
    ;; initializing these three variables keep the C compilers from complaining
    ;; that they are being used uninitialized.
    (emit-expr-to-compound-statement
      (make-c-infix-expr (make-c-name-expr exit-path) "=" 0)
      c-body)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	(make-c-name-expr throw-tag)
	"=" (make-c-cast-expr 'obj (make-c-name-expr "NULL")))
      c-body)
    (when (not (eq protected-value value-cache))
      (emit-expr-to-compound-statement
	(make-c-infix-expr
	  (make-c-name-expr protected-value)
	  "=" (default-value-for-c-type c-type))
	c-body))

    ;; Generate the statement used as the handler for throws.  It will set the
    ;; exit-path to 1 and cache the current throw tag.
    (emit-expr-to-compound-statement
      (make-c-infix-expr exit-path "=" 1) throw-handler-statement)
    (emit-expr-to-compound-statement
      (make-c-infix-expr throw-tag "=" "Current_throw") throw-handler-statement)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	value-cache "="
	(make-c-function-call-expr
	  (make-c-name-expr "retrieve_values_from_stack") nil))
      throw-handler-statement)
    (emit-copy-value-buffer-statements
      values-buffer "Values_buffer" values-count "Values_count"
      throw-handler-statement)
    
    ;; Translate the protected body, storing the first value into the
    ;; protected-value.
    (translate-l-expr-into-c protected-l-expr c-func protected-statement
			     (if (eq return-directive :discard)
				 :discard
				 (list protected-value)))
    (cond
      ((type-includes-values-p lisp-type)
       (emit-copy-value-buffer-statements
	 values-buffer "Values_buffer" values-count "Values_count"
	 protected-statement))
      (t
       (emit-expr-to-compound-statement
	 (make-c-infix-expr values-count "=" 1)
	 protected-statement)))
    ;; Pop the unwind-protect frame off the stack, then if this l-expr returns
    ;; multiple values, cache the protected value onto the stack before
    ;; executing the cleanup forms.  The exit-path code for normal exits will
    ;; always be 0.
    (emit-expr-to-compound-statement
      (make-c-infix-expr exit-path "=" 0) protected-statement)
    (emit-expr-to-compound-statement
      (make-c-infix-expr
	"Throw_stack_top" "=" (make-c-infix-expr "Throw_stack_top" "-" 2))
      protected-statement)

    ;; Emit the conditional statement that checks the result of setjmp.
    (emit-statement-to-compound-statement
      (make-c-conditional-statement
	(list (make-c-infix-expr
		(make-c-function-call-expr
		  (make-c-name-expr "setjmp")
		  (list (make-c-name-expr jump-buf)))
		"!=" 0)
	      throw-handler-statement
	      nil			; no else test
	      protected-statement))
      c-body)

    ;; If there are any exit-descriptions (make from local exits that pass
    ;; through this unwind-protect), then emit a label for them to go to.
    (when (protect-exit-descriptions protect-struct)
      (emit-statement-to-compound-statement
	(make-c-label-statement cleanup-label)
	c-body))

    ;; Now translate the cleanup forms.
    (translate-progn-body cleanup-l-exprs c-func c-body :discard)

    ;; Restore the values, if any, to the global values buffer.
    (emit-copy-value-buffer-statements
      "Values_buffer" values-buffer "Values_count" values-count
      c-body)

    ;; Make a body for a switch statement that will take the proper exit-path.
    ;; Always emit the default path 0 and the throw path 1 first, then emit any
    ;; paths for the other exit-descriptions.
    (emit-statement-to-compound-statement
      (make-c-label-statement "case 0") exit-statement)
    (emit-statement-to-compound-statement
      (make-c-break-statement) exit-statement)
    ;; Emit the case where we continue a throw.
    (emit-statement-to-compound-statement
      (make-c-label-statement "case 1") exit-statement)
    (emit-expr-to-compound-statement
      (make-c-function-call-expr
	(make-c-name-expr "throw_towards_catch_tag")
	(list (make-c-name-expr throw-tag) (make-c-name-expr value-cache)))
      exit-statement)
    ;; Next emit all the go and return-from cases.
    (loop for exit-case fixnum from 2
	  for exit in (protect-exit-descriptions protect-struct)
	  for exit-type = (cons-car exit)
	  do
      (cond
	((memqp exit-type '(return-from go))
	 (emit-statement-to-compound-statement
	   (make-c-label-statement (format nil "case ~a" (cons-second exit)))
	   exit-statement)
	 (emit-statement-to-compound-statement
	   (cons-third exit)
	   exit-statement))
	(t
	 (translation-error
	   "Unknown exit-type ~A in unwind-protect." exit-type))))

    ;; Emit code to check that the top-of-stack has been restored to the
    ;; expected value.
    (when safety?
      (emit-statement-to-compound-statement
	(make-c-conditional-statement
	  (list (make-c-infix-expr (make-c-name-expr expected-tos)
				   "!=" (make-c-name-expr "Throw_stack_top"))
		(make-c-expr-statement
		  (make-c-function-call-expr
		    (make-c-name-expr "error")
		    (list (make-c-literal-expr
			    "Corrupted Throw_stack_top at unwind-protect."))))))
	c-body))

    ;; Emit the switch statement.
    (emit-statement-to-compound-statement
      (make-c-switch-statement (make-c-name-expr exit-path) exit-statement)
      c-body)

    ;; Finally, emit code for the return value, if any.
    (emit-c-expr-as-directed
      (make-c-name-expr protected-value)
      unwind-protect-l-expr c-func c-body return-directive)))

(def-l-expr-method translate-l-expr-into-c
    (symbol-macrolet-l-expr c-func c-body return-directive)
  (translate-progn-body
    (cons-cddr (l-expr-form symbol-macrolet-l-expr))
    c-func c-body return-directive))

(def-l-expr-method translate-l-expr-into-c
    (locally-l-expr c-func c-body return-directive)
  (translate-progn-body
    (cons-cdr (l-expr-form locally-l-expr)) c-func c-body return-directive))

(def-l-expr-method translate-l-expr-into-c
    (fixnum-case-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form fixnum-case-l-expr))
	 (control-l-expr (cons-second form))
	 (clauses (cons-cddr form))
	 (switch-body (make-c-compound-statement nil nil nil nil))
	 (control-c-expr
	   (translate-l-expr-into-c control-l-expr c-func c-body :c-expr))
	 (c-type (l-expr-c-return-type fixnum-case-l-expr))
	 (result-temp?
	   (if (eq return-directive :c-expr)
	       (lexical-c-variable-identifier
		 'temp c-func c-type
		 (storage-classes-for-env
		   (l-expr-env fixnum-case-l-expr)))))
	 (break-needed? (not (eq return-directive :return)))
	 (clause-return
	   (if result-temp?
	       (list result-temp?)
	       return-directive)))
    (loop for clause in clauses
	  for keys = (cons-car clause)
	  do
      (if (eq keys t)
	  (emit-statement-to-compound-statement
	    (make-c-label-statement "default") switch-body)
	  (loop for key in (cons-car clause) do
	    (emit-statement-to-compound-statement
	      (make-c-label-statement (format nil "case ~a" key))
	      switch-body)))
      (translate-progn-body 
	(cons-cdr clause) c-func switch-body clause-return)
      (when break-needed?
	(emit-statement-to-compound-statement
	  (make-c-break-statement)
	  switch-body)))
    (emit-statement-to-compound-statement
      (make-c-switch-statement control-c-expr switch-body)
      c-body)
    (when result-temp?
      (reclaim-reusable-c-variables c-func (list (cons result-temp? c-type)))
      (make-c-name-expr result-temp?))))

(def-l-expr-method translate-l-expr-into-c
    (while-loop-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form while-loop-l-expr))
	 (control-l-expr (cons-second form))
	 (body-l-exprs (cons-cddr form))
	 (while-body (make-c-compound-statement nil nil nil nil))
	 (control-c-expr (translate-l-expr-into-c
			   control-l-expr c-func while-body :c-expr)))
    (unless (c-compound-statement-empty-p while-body)
      (translation-error
	"~s cannot be used as a while-loop control statement."
	(l-expr-pretty-form control-l-expr)))
    (translate-progn-body body-l-exprs c-func while-body :discard)
    (emit-statement-to-compound-statement
      (make-c-while-statement control-c-expr while-body)
      c-body)
    (emit-c-expr-as-directed
      (make-c-cast-expr 'obj (make-c-name-expr "NULL"))
      while-loop-l-expr c-func c-body return-directive)))

(def-l-expr-method translate-l-expr-into-c
    (for-loop-l-expr c-func c-body return-directive)
  (tl:destructuring-bind-strict
      (nil (init? control step?) . l-expr-body)
    (l-expr-form for-loop-l-expr)
    (let* ((init-c-expr? (if init?
			     (translate-l-expr-into-c
			       init? c-func c-body :c-expr)))
	   (loop-c-body (make-c-compound-statement nil nil nil nil))
	   (control-c-expr
	     (prog1
		 (translate-l-expr-into-c control c-func loop-c-body :c-expr)
	       (unless (c-compound-statement-empty-p loop-c-body)
		 (translation-error
		   "~s is too complicated to use as a FOR-LOOP control expression."
		   (l-expr-pretty-form control)))))
	   (step-c-expr?
	     (if step?
		 (prog1
		     (translate-l-expr-into-c step? c-func loop-c-body :c-expr)
		   (unless (c-compound-statement-empty-p loop-c-body)
		     "~s is too complicated to use as a FOR-LOOP step expression."
		     (l-expr-pretty-form step?))))))
      (translate-progn-body l-expr-body c-func loop-c-body :discard)
      (emit-statement-to-compound-statement
	(make-c-for-statement
	  init-c-expr? control-c-expr step-c-expr? loop-c-body)
	c-body)
      (emit-c-expr-as-directed
	(make-c-cast-expr 'obj (make-c-name-expr "NULL"))
	for-loop-l-expr c-func c-body
	return-directive))))
	   




;;; The variable `arg-temp-var' is used as a temporary variable name when
;;; translating function calls that require some argument values to be cached
;;; due to side-effecting arguments.

(defvar arg-temp-var (make-symbol "ARG-TEMP"))




;;; The translation method for `function-call-l-expr' determines whether the
;;; arguments need to be translated in separate statements or whether then can
;;; be emitted in line.  If there is more than one argument, or if there is at
;;; least one non-side-effect-free argument and the remaining arguments are not
;;; functional (basically constants), then we need to allocate new variables to
;;; hold the function arguments, evaluate each in turn in a separate statement,
;;; then return the c-expr for calling this function on the given arguments.

(def-l-expr-method translate-l-expr-into-c
    (function-call-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form function-call-l-expr))
	 (argument-list (cons-cdr form))
	 (decls (function-call-l-expr-decls function-call-l-expr))
	 (ftype (or (cdr (assq 'ftype decls))
		    (cdr (assq 'computed-ftype decls))))
	 (c-expr-arg-list nil)
	 (c-temporary-variables nil)
	 (translator? (function-call-l-expr-translator? function-call-l-expr)))
    ;; Convert the arguments into c-exprs.
    (multiple-value-setq (c-expr-arg-list c-temporary-variables)
      (translate-function-arglist-into-c
	argument-list c-func c-compound-statement
	(l-expr-env function-call-l-expr)))

    ;; Emit a call to the function, and then register the temporary variables as
    ;; reusable.
    (prog1
	(if translator?
	    (funcall (cons-second (assq 'c-translator decls))
		     function-call-l-expr c-expr-arg-list c-func
		     c-compound-statement return-directive)
	    (let* ((function-name (cons-car form))
		   (c-identifier (c-identifier-for-function
				   function-name *global-c-namespace*
				   (c-func-namespace c-func))))
	      (when (register-used-function
		      (c-func-c-file c-func) function-name c-identifier ftype)
		(register-needed-function-extern
		  (c-func-c-file c-func) '("extern")
		  (c-func-type-for-lisp-func-return-type-spec
		    (cons-third ftype))
		  c-identifier
		  (loop for arg-lisp-type in (cons-second ftype)
			unless (eq arg-lisp-type '&optional)
			  collect (c-type-for-lisp-type arg-lisp-type))))
	      (emit-c-expr-as-directed
		(make-c-function-call-expr
		  (make-c-name-expr c-identifier) c-expr-arg-list)
		function-call-l-expr c-func c-compound-statement return-directive)))
      (when c-temporary-variables
	(reclaim-reusable-c-variables c-func c-temporary-variables)))))

(defun translate-function-arglist-into-c (argument-list c-func c-body env)
  (let ((c-expr-arg-list nil)
	(c-temporary-variables nil)
	(argument-count (length argument-list)))
    (cond
      ((or (= argument-count 1)
	   (loop for l-expr in argument-list
		 count (not (l-expr-side-effect-free-p l-expr))
		   into side-effect-count
		 finally
		   (return
		     (or (zerop side-effect-count)
			 (and (= side-effect-count 1)
			      (loop for l-expr in argument-list
				    always
				    (or (not (l-expr-side-effect-free-p l-expr))
					(l-expr-constant-p l-expr))))))))
       (setq c-expr-arg-list
	     (loop for l-expr in argument-list
		   collect (translate-l-expr-into-c
			     l-expr c-func c-body :c-expr))))
      (t
       (setq c-expr-arg-list (copy-list argument-list))
       (loop with temp-storage-classes = (storage-classes-for-env env)
	     with temp-var = arg-temp-var
	     for arg-cons on c-expr-arg-list
	     for arg = (cons-car arg-cons)
	     do
	 (if (and (not (l-expr-constant-p arg))
		  (cons-cdr arg-cons)
		  (loop for arg in arg-cons
			thereis (not (l-expr-side-effect-free-p arg))))
	     ;; If this argument is not a constant, this argument is not the
	     ;; last one, and either this or a following expression is not
	     ;; side-effect-free, then cache the value of this argument into a C
	     ;; temporary variable.
	     (let* ((type (l-expr-c-return-type arg))
		    (new-var (lexical-c-variable-identifier
			       temp-var c-func type temp-storage-classes)))
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-name-expr new-var) "="
		   (translate-l-expr-into-c
		     arg c-func c-body :c-expr))
		 c-body)
	       (setf (car arg-cons) (make-c-name-expr new-var))
	       (push (cons new-var type) c-temporary-variables))
	     ;; Otherwise, evaluate this argument in place.
	     (setf (car arg-cons)
		   (translate-l-expr-into-c
		     arg c-func c-body :c-expr))))))
    (values c-expr-arg-list c-temporary-variables)))




;;; The translate-l-expr-into-c method for `implicit-symbol-value-l-expr'
;;; translates into a C name expr for the c-identifier for the binding structure
;;; for the given variables.

(def-l-expr-method translate-l-expr-into-c
    (implicit-symbol-value-l-expr c-func c-body return-directive)
  (let* ((isv implicit-symbol-value-l-expr)
	 (name (l-expr-form isv))
	 (binding-type (implicit-symbol-value-l-expr-binding-type isv))
	 (specl-const? (memqp binding-type '(:special :constant)))
	 (decls (implicit-symbol-value-l-expr-decls isv))
	 (binding? (cdr (assq 'variable-binding-structure decls)))
	 (c-name (if binding?
		     (variable-binding-c-identifier binding?)
		     (c-identifier-for-variable
		       name *global-c-namespace* (c-func-namespace c-func))))
	 (c-name-expr (make-c-name-expr c-name))
	 (c-type (if binding?
		     (variable-binding-c-type binding?)
		     'obj))
	 (c-file (c-func-c-file c-func))
	 (chosen-c-type-for-this-reference (l-expr-c-return-type isv)))
    (when specl-const?
      (when (register-used-variable c-file name c-name c-type)
	(register-needed-variable-extern c-file '("extern") c-type c-name)))
    (when (and (null binding?) (not specl-const?))
      (setq binding-type :special)
      (translation-warning "Free variable ~s assumed to be special." name))
    (when (eq binding-type :special)
      (setq c-name-expr (make-c-function-call-expr
			 (make-c-name-expr "GET_GLOBAL") (list c-name-expr))))
    ;; When the chosen C type is void, then this reference is being emitted only
    ;; to inhibit warnings from the C compiler that this variable is unused.
    ;; This arises in the case were Lisp warnings have been inhibited by making
    ;; a reference at top level of a progn, rather than doing a declare ignore.
    (emit-c-expr-as-directed
      (if (eq chosen-c-type-for-this-reference 'void)
	  (make-c-cast-expr 'void c-name-expr)
	  c-name-expr)
      isv c-func c-body return-directive)))




;;; The function `translate-type-check-predicate' takes a c-expr (which may be
;;; repeatedly evaluated) holding an Obj, a required Lisp type, and the known
;;; value type.  This function will return a c-expr predicate which returns
;;; boolean true when the value in the c-name-expr is an instance of the given
;;; Lisp type, and boolean false otherwise.

(defun translate-type-check-predicate
    (c-expr required-lisp-type result-lisp-type)
  (cond
    ((and (consp required-lisp-type) (eq (cons-car required-lisp-type) 'values))
     (let* ((needed-types
	      (loop with initial-types = (cons-cdr required-lisp-type)
		    for last-type? = (car (last initial-types))
		    while (and initial-types 
			       last-type? (tl-subtypep t last-type?))
		    do
		(setq initial-types (butlast initial-types))
		    finally (return initial-types)))
	    (needed-value-count (length needed-types))
	    (known-types (if (and (consp result-lisp-type)
				  (eq (cons-car result-lisp-type) 'values))
			     (cons-cdr result-lisp-type)))
	    (count-test (make-c-infix-expr
			  "Value_count" ">="
			  (make-c-literal-expr needed-value-count)))
	    (type-tests nil))
       (when (and needed-types
		  (not (tl-subtypep (or (car known-types) t)
				    (car needed-types))))
	 (push (translate-type-check-predicate
		 c-expr (car needed-types) (or (car known-types) t))
	       type-tests))
       (loop for index fixnum from 0
	     for type in (cdr needed-types)
	     for known-cons = (cdr known-types) then (cdr known-cons)
	     for known-type = (or (car known-cons) (if known-types 'null t))
	     do
	 (unless (tl-subtypep known-type type)
	   (push (translate-type-check-predicate
		   (make-c-subscript-expr (make-c-name-expr "Values_buffer")
					  (make-c-literal-expr index))
		   type known-type)
		 type-tests)))
       (if type-tests
	   (make-c-logical-and-expr
	     count-test
	     (if (cons-cdr type-tests)
		 (loop for test in (cons-cdr type-tests)
		       for combined-test
			   = (make-c-logical-and-expr test (car type-tests))
			   then (make-c-logical-and-expr test combined-test)
		       finally (return combined-test))
		 (cons-car type-tests)))
	   count-test)))
    ((and (consp required-lisp-type) (eq (cons-car required-lisp-type) 'and))
     (cond ((null (cons-cddr required-lisp-type))
	    (translate-type-check-predicate
	      c-expr (cons-second required-lisp-type) result-lisp-type))
	   ((null (cons-cdddr required-lisp-type))
	    (make-c-logical-and-expr
	      (translate-type-check-predicate
		c-expr (cons-second required-lisp-type) result-lisp-type)
	      (translate-type-check-predicate
		c-expr (cons-third required-lisp-type) result-lisp-type)))
	   (t
	    (make-c-logical-and-expr
	      (translate-type-check-predicate
		c-expr (cons-second required-lisp-type) result-lisp-type)
	      (translate-type-check-predicate
		c-expr (cons 'and (cons-cddr required-lisp-type))
		result-lisp-type)))))

    ((and (consp required-lisp-type) (eq (cons-car required-lisp-type) 'or))
     (cond ((null (cons-cddr required-lisp-type))
	    (translate-type-check-predicate
	      c-expr (cons-second required-lisp-type) result-lisp-type))
	   ((null (cons-cdddr required-lisp-type))
	    (make-c-logical-or-expr
	      (translate-type-check-predicate
		c-expr (cons-second required-lisp-type) result-lisp-type)
	      (translate-type-check-predicate
		c-expr (cons-third required-lisp-type) result-lisp-type)))
	   (t
	    (make-c-logical-or-expr
	      (translate-type-check-predicate
		c-expr (cons-second required-lisp-type) result-lisp-type)
	      (translate-type-check-predicate
		c-expr (cons 'or (cons-cddr required-lisp-type))
		result-lisp-type)))))
    ((and (consp required-lisp-type) (eq (cons-car required-lisp-type) 'not))
     (make-c-unary-expr
       #\! (translate-type-check-predicate
	     c-expr (cons-second required-lisp-type) result-lisp-type)))
    ((tl-subtypep required-lisp-type 'null)
     (make-c-infix-expr c-expr "==" (make-c-name-expr "NULL")))
    ((tl-subtypep required-lisp-type 'fixnum)
     (make-c-line-comment-expr
       (translate-immediate-tag-test c-expr 1) "Fixnump"))
    ((tl-subtypep required-lisp-type 'cons)
     (make-c-line-comment-expr
       (translate-immediate-tag-test c-expr 2) "Consp"))
    ((eq required-lisp-type 'atom)
     (make-c-line-comment-expr
       (make-c-infix-expr
	 (make-c-infix-expr (make-c-cast-expr 'uint32 c-expr) "&" 3)
	 "!=" 2)
       "Atomp"))
    ((tl-subtypep required-lisp-type 'character)
     (translate-immediate-tag-test c-expr 3))
    (t
     (let ((type-tag? (lisp-type-tag required-lisp-type))
	   (*print-pretty* nil))
       (cond (type-tag?
	      (make-c-line-comment-expr
	        (translate-tag-test c-expr type-tag?)
		(format nil "~a-P" required-lisp-type)))
	     ((class-type-p required-lisp-type)
	      (let* ((info (class-info required-lisp-type))		
		     (min-type-tag (struct-type-tag info))
		     (max-type-tag (struct-maximum-subtype-tag info)))
		(declare (fixnum min-type-tag max-type-tag))
		(make-c-line-comment-expr
		  (translate-extended-tag-test c-expr min-type-tag max-type-tag)
		  (format nil "~a-P" required-lisp-type))))

	     ((tl-subtypep required-lisp-type 'number)
	      (make-c-infix-expr
		(translate-type-check-predicate
		  c-expr 'fixnum result-lisp-type)
		"||"
		(translate-type-check-predicate
		  c-expr 'double-float result-lisp-type)))
	     ((tl-subtypep required-lisp-type 'list)
	      (make-c-infix-expr
		(translate-type-check-predicate
		  c-expr 'null result-lisp-type)
		"||"
		(translate-type-check-predicate
		  c-expr 'cons result-lisp-type)))
	     (t
	      (translation-error
		"Can't type check Lisp type ~s" required-lisp-type)
	      (make-c-literal-expr 1)))))))

(defun translate-immediate-tag-test (c-expr tag)
  (make-c-infix-expr
    (make-c-function-call-expr (make-c-name-expr "IMMED_TAG") (list c-expr))
    "=="
    (make-c-literal-expr tag)))

(defun translate-tag-test (c-expr tag)
  (make-c-infix-expr
    (make-c-infix-expr c-expr "!=" "NULL")
    "&&"
    (make-c-infix-expr
      (translate-immediate-tag-test c-expr 0)
      "&&"
      (make-c-infix-expr
        (make-c-function-call-expr (make-c-name-expr "STD_TAG") (list c-expr))
	"=="
	(make-c-literal-expr tag)))))

(defun translate-extended-tag-test (c-expr min-type-tag max-type-tag)
  (make-c-infix-expr
    (translate-tag-test c-expr (c-type-tag 'class-hdr))
    "&&"
    (if (= min-type-tag max-type-tag)
	(make-c-infix-expr
	  (make-c-function-call-expr 
	    (make-c-name-expr "EXTENDED_TAG") (list c-expr))
	  "=="
	  (make-c-literal-expr min-type-tag))
      (make-c-infix-expr
        (make-c-infix-expr 
	  (make-c-literal-expr min-type-tag)
	  "<="
	  (make-c-function-call-expr
	    (make-c-name-expr "EXTENDED_TAG") (list c-expr)))
	"&&"
	(make-c-infix-expr 
	  (make-c-function-call-expr
	    (make-c-name-expr "EXTENDED_TAG") (list c-expr))
	  "<="
	  (make-c-literal-expr max-type-tag))))))




;;; The translate-l-expr-into-c method for `coecere-to-type-l-expr' performs
;;; Lisp type checks to ensure correct type casting and them performs C type
;;; casting.

(def-l-expr-method translate-l-expr-into-c
    (coerce-to-type-l-expr c-func c-compound-statement return-directive)
  (let* ((form (l-expr-form coerce-to-type-l-expr))
	 (held-l-expr (cons-second form))
	 (held-lisp-type (l-expr-lisp-return-type held-l-expr))
	 (held-c-type (l-expr-c-return-type held-l-expr))
	 (lisp-type
	   (coerce-to-type-l-expr-lisp-required-type coerce-to-type-l-expr))
	 (c-type (coerce-to-type-l-expr-c-required-type coerce-to-type-l-expr))
	 (env (l-expr-env coerce-to-type-l-expr))
	 (insert-values-count? nil)
	 (safety-value
	   (second
	     (assq 'safety (tl:declaration-information
			     'optimize (l-expr-env coerce-to-type-l-expr))))))
    (cond ((type-includes-values-p lisp-type)
	   (when (not (type-includes-values-p held-lisp-type))
	     (setq insert-values-count? t)
	     (setq held-lisp-type (list 'values held-lisp-type))))
	  ((type-includes-values-p held-lisp-type)
	   (setq held-lisp-type (type-of-first-value held-lisp-type))))

    ;; When we are attempting to coerce into or out of a C type, the we will
    ;; fool the mechanisms below by slamming the held-lisp-type, lisp-type, and
    ;; c-type variables of this function to cause the behavior we want.  For
    ;; example, when coercing into a (c-type long), we really want the argument
    ;; type to be a fixnum and the resulting c-type to be long.  By setqing
    ;; lisp-type to fixnum and c-type to long, the following code will do
    ;; exactly what we want.  All coercions in and out of types of the form
    ;; (c-type X) are handled this way.  Also, in the special case of coercing a
    ;; NULL to fixnum, we'll do the same.  This case arises in some
    ;; circumstances where fixnum variables have blank initializations.
    (cond
      ((and (explicit-lisp-to-c-type-p lisp-type)
	    (not (explicit-lisp-to-c-type-p held-lisp-type)))
       (cond ((tl-subtypep lisp-type '(c-type "long"))
	      (setq lisp-type 'fixnum))
	     ((tl-subtypep lisp-type '(c-type (pointer "char")))
	      (setq lisp-type 'string))
	     ((or (tl-subtypep lisp-type '(c-type (pointer "void")))
		  (tl-subtypep lisp-type '(c-type (pointer "uint32"))))
	      (setq lisp-type t))
	     (t
	      (translation-error
		"Can't coerce ~s into ~s." held-lisp-type lisp-type))))
      ((and (explicit-lisp-to-c-type-p held-lisp-type)
	    (not (explicit-lisp-to-c-type-p lisp-type)))
       (cond ((tl-subtypep held-lisp-type '(c-type "long"))
	      (setq held-lisp-type 'fixnum)
	      (setq held-c-type 'long))
	     ((tl-subtypep held-lisp-type '(c-type (pointer "uint32")))
	      (setq held-lisp-type t)
	      (setq held-c-type '(pointer uint32)))
	     (t
	      (translation-error
		"Can't coerce ~s into ~s." held-lisp-type lisp-type))))
      ((and (tl-subtypep lisp-type 'fixnum)
	    (tl-subtypep held-lisp-type 'null))
       (setq held-lisp-type 'fixnum)
       (setq held-c-type 'int)))

    (cond
      ((or (return-directive-discards-values-p return-directive c-func)
	   (and (null insert-values-count?)
		(tl-subtypep held-lisp-type lisp-type)
		(satisfies-c-required-type-p held-c-type c-type)))
       (translate-l-expr-into-c
	 held-l-expr c-func c-compound-statement return-directive))
      ;; Give an error for disjoint types.
      ((or (and (not (tl-subtypep held-lisp-type lisp-type))
		(not (tl-subtypep lisp-type held-lisp-type)))
	   (and (l-expr-constant-p held-l-expr)
		(not (tl-typep (l-expr-constant-value held-l-expr) lisp-type))))
       (cond
	 ;; When the value being returned is a NIL and the required C type is
	 ;; Obj, just translate it through and see what happens.  NIL is the
	 ;; default initial value, and for some type declared variables, such as
	 ;; types package, there is nothing you can really do.

	 ;; Eeek!  Review this logic once G2 is being translated.  It seems we
	 ;; could do better within LET et ali so that this case did not arise.
	 ;; There could easily be type declarations bugs in user code that could
	 ;; be hidden by this case.  -jallard 4/22/97
	 ((or (tl-subtypep held-lisp-type 'null)
	      (and (l-expr-constant-p held-l-expr)
		   (null (l-expr-constant-value held-l-expr))))
	  (cond
	    ((satisfies-c-required-type-p c-type 'obj)
	     (translate-l-expr-into-c
	       held-l-expr c-func c-compound-statement return-directive))
	    ((satisfies-c-required-type-p c-type 'double)
	     (emit-c-expr-as-directed
	       (make-c-literal-expr 0.0) coerce-to-type-l-expr
	       c-func c-compound-statement return-directive))
	    (t
	     (emit-c-expr-as-directed
	       (make-c-cast-expr
		 c-type (translate-l-expr-into-c
			  held-l-expr c-func c-compound-statement :c-expr))
	       coerce-to-type-l-expr c-func c-compound-statement
	       return-directive))))
	 ((and (tl-subtypep lisp-type 'double-float)
	       (tl-subtypep held-lisp-type 'fixnum))
	  (let* ((held-translated-expr
		   (translate-l-expr-into-c
		     held-l-expr c-func c-compound-statement :c-expr))
		 (double-value-expr
		  (make-c-cast-expr
		    'double
		    (if (satisfies-c-required-type-p held-c-type 'sint32)
			held-translated-expr
			(coerce-c-expr-result-to-type
			  held-translated-expr held-c-type 'sint32 env)))))
	    (emit-c-expr-as-directed
	      (if (satisfies-c-required-type-p 'double c-type)
		  double-value-expr
		  (coerce-c-expr-result-to-type
		    double-value-expr 'double c-type env))
	      coerce-to-type-l-expr c-func c-compound-statement
	      return-directive)))
	 (t
	  ;; Emit the code, emit a call to error, and return the default
	  ;; constant for the given C type.
	  (unless (l-expr-side-effect-free-p held-l-expr)
	    (translate-l-expr-into-c
	      held-l-expr c-func c-compound-statement :discard))
	  (emit-expr-to-compound-statement
	    (make-c-function-call-expr
	      (make-c-name-expr "error")
	      (list (make-c-literal-expr
		      (format nil "Unable to coerce type ~a to ~a"
			      held-lisp-type lisp-type))))
	    c-compound-statement)
	  (emit-c-expr-as-directed
	    (default-value-for-c-type c-type)
	    coerce-to-type-l-expr c-func
	    c-compound-statement return-directive))))
;       (translation-error
;	 "The type ~s returned from ~s cannot satisfy the required type ~s."
;	 held-lisp-type (l-expr-pretty-form held-l-expr) lisp-type)

;      ((and (l-expr-constant-p held-l-expr)
;	    (not (tl-typep (l-expr-constant-value held-l-expr) lisp-type)))
;       (translation-error
;	 "The constant ~s cannot satisfy the required-type ~s."
;	 (l-expr-constant-value held-l-expr)
;	 lisp-type))
      (t
       (let ((held-c-expr (translate-l-expr-into-c
			    held-l-expr c-func c-compound-statement :c-expr))
	     (temp-var? nil))
	 (when (and (not (tl-subtypep held-lisp-type lisp-type))
		    (eq held-c-type 'obj)
		    (>= safety-value 3))
	   ;; Emit a Lisp type check if we are safe.
	   (unless (c-name-expr-p held-c-expr)
	     (setq temp-var? (lexical-c-variable-identifier
			       (make-symbol "TYPE-CHECK-TEMP") c-func 'obj
			       (storage-classes-for-env env)))
	     (emit-expr-to-compound-statement
	       (make-c-infix-expr (make-c-name-expr temp-var?) "=" held-c-expr)
	       c-compound-statement)
	     (setq held-c-expr (make-c-name-expr temp-var?)))

	   (emit-safety-type-check-to-compound-statement
	     held-c-expr lisp-type held-lisp-type
	     c-compound-statement))
	 ;; Wrap the held-c-expr with a type cast if necessary.
	 (unless (satisfies-c-required-type-p held-c-type c-type)
	   (setq held-c-expr
		 (coerce-c-expr-result-to-type
		   held-c-expr held-c-type c-type env)))
	 (when insert-values-count?
	   (unless (side-effect-free-c-expr-p held-c-expr)
	     (let ((values-temp (lexical-c-variable-identifier
				  'temp c-func c-type
				  (storage-classes-for-env env))))
	       (emit-expr-to-compound-statement
		 (make-c-infix-expr
		   (make-c-name-expr values-temp) "=" held-c-expr)
		 c-compound-statement)
	       (reclaim-reusable-c-variables
		 c-func (list (cons values-temp c-type)))
	       (setq held-c-expr (make-c-name-expr values-temp))))
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr "Values_count" "=" 1)
	     c-compound-statement))
	 (when temp-var?
	   (reclaim-reusable-c-variables c-func (list (cons temp-var? 'obj))))
	 (emit-c-expr-as-directed
	   held-c-expr coerce-to-type-l-expr c-func c-compound-statement
	   return-directive))))))




;;; The function `emit-safety-type-check-to-compound-statement' takes a
;;; c-name-expr of type Obj, a Lisp required-type, the currently known Lisp type
;;; of the value, and a c-compound-statement.  This will emit a statement that
;;; will call type-error if the value in the c-name-expr is not of the
;;; required-type.  This is used to implement safety checks.

(defun emit-safety-type-check-to-compound-statement
    (c-name-expr required-lisp-type held-lisp-type c-compound-statement)
  (emit-statement-to-compound-statement
    (make-c-conditional-statement
      (list (make-c-unary-expr
	      #\! (translate-type-check-predicate
		    c-name-expr required-lisp-type held-lisp-type))
	    (make-c-expr-statement 
	      (make-c-function-call-expr
		(make-c-name-expr "type_error")
		(list c-name-expr
		      (make-c-literal-expr
			(format nil "~a" required-lisp-type)))))))
    c-compound-statement))




;;; The translator for `values-l-expr' takes care to only set the values count
;;; in cases where the caller needs it and then if we know how many values the
;;; caller needs, we do not attempt to cache into the value buffer any extra
;;; values beyond those desired.

(def-l-expr-method translate-l-expr-into-c
    (values-l-expr c-func c-body return-directive)
  (let* ((lisp-type (l-expr-lisp-return-type values-l-expr))
	 (c-type (l-expr-c-return-type values-l-expr))
	 (args (cons-cdr (l-expr-form values-l-expr)))
	 (temp-storage-classes
	   (storage-classes-for-env (l-expr-env values-l-expr)))
	 (first-value? nil))
    (cond
      ;; Directly set the listed variables in the return directive only when
      ;; there are multiple locations to set or when the Lisp type does not
      ;; include returning multiple values.  The problem is that often a single
      ;; temporary variable may be passed in, when the caller expected that the
      ;; values count would be set and secondary values would be put into the
      ;; values buffer.  We can only use the direct setting case when we have
      ;; good reason to believe that the caller does not expect the values count
      ;; to be set.  -jra 6/27/96
      ((and (consp return-directive)
	    (or (cons-cdr return-directive)
		(not (type-includes-values-p lisp-type))))
       (loop for arg-cons = args then (cdr arg-cons)
	     for var-cons = return-directive then (cdr var-cons)
	     while (or arg-cons var-cons)
	     do
	 (cond ((and arg-cons var-cons)
		(translate-l-expr-into-c
		  (cons-car arg-cons) c-func c-body (list (cons-car var-cons))))
	       (var-cons
		(emit-expr-to-compound-statement
		  (make-c-infix-expr
		    (make-c-name-expr (cons-car var-cons))
		    "=" (make-c-cast-expr
			  'obj (make-c-name-expr "NULL")))
		  c-body))
	       (arg-cons
		(translate-l-expr-into-c
		  (cons-car arg-cons) c-func c-body :discard))
	       (t (error "This else clause can't happen!")))))
      ((type-includes-values-p lisp-type)
       (cond
	 ((or (cons-cdr args) (eq return-directive :return))
	  (setq first-value?
		(lexical-c-variable-identifier
		  'temp c-func c-type
		  temp-storage-classes))
	  (translate-l-expr-into-c
	    (cons-car args) c-func c-body (list first-value?))
	  (loop with desired-values fixnum = (if (eq lisp-type '*)
						 tl:multiple-values-limit
						 (length (cons-cdr lisp-type)))
		for index fixnum from 0
		for arg in (cons-cdr args)
		do
	    (if (< index desired-values)
		;; This means the result of this value is needed.
		(emit-expr-to-compound-statement
		  (make-c-infix-expr
		    (make-c-subscript-expr (make-c-name-expr "Values_buffer")
					   (make-c-literal-expr index))
		    "="
		    (translate-l-expr-into-c arg c-func c-body :c-expr))
		  c-body)
		;; Else, this value is being discarded.
		(translate-l-expr-into-c arg c-func c-body :discard))))
	 (t
	  (translate-l-expr-into-c
	    (cons-car args) c-func c-body return-directive)))
       (emit-expr-to-compound-statement
	 (make-c-infix-expr
	   "Values_count" "="
	   (make-c-literal-expr (if (eq lisp-type '*)
				    (length args)
				    (length (cons-cdr lisp-type)))))
	 c-body)
       (when first-value?
	 (reclaim-reusable-c-variables c-func (list (cons first-value? c-type)))
	 (emit-c-expr-as-directed
	   (make-c-name-expr first-value?)
	   values-l-expr c-func c-body return-directive)))
      ;; Else, the caller doesn't want the values count set.
      ((null (cons-cdr args))
       (translate-l-expr-into-c (car args) c-func c-body return-directive))
      ((eq return-directive :discard)
       (loop for arg in args do
	 (translate-l-expr-into-c arg c-func c-body return-directive)))

      (t
       (unless (return-directive-discards-values-p return-directive c-func)
	 (setq first-value? (lexical-c-variable-identifier
			      'temp c-func c-type temp-storage-classes)))
       (translate-l-expr-into-c
	 (cons-car args) c-func c-body (list first-value?))
       (loop for arg in (cons-cdr args) do
	 (translate-l-expr-into-c arg c-func c-body :discard))
       (when first-value?
	 (reclaim-reusable-c-variables c-func (list (cons first-value? c-type))))
       (emit-c-expr-as-directed
	 (if first-value?
	     (make-c-name-expr first-value?)
	     (make-c-cast-expr 'obj (make-c-name-expr "NULL")))
	 values-l-expr c-func c-body return-directive)))))




;;; The translator for `inlined-typep-l-expr' emits inlined code to typetest its
;;; first argument.  The walker and tl:typep have ensured that the first
;;; argument is always a symbol and so may be evaluated multiple times.  The
;;; type argument is a constant naming a primitive Lisp type suitable for
;;; immediate translation into a C type predicate.

(def-l-expr-method translate-l-expr-into-c
    (inlined-typep-l-expr c-func c-body return-directive)
  (let* ((form (l-expr-form inlined-typep-l-expr))
	 (object-l-expr (cons-second form))
	 (type (cons-third form)))
    (emit-c-expr-as-directed
      (translate-type-check-predicate
	(translate-l-expr-into-c object-l-expr c-func c-body :c-expr)
	type
	(l-expr-lisp-return-type object-l-expr))
      inlined-typep-l-expr c-func c-body return-directive)))




;;; The translator for `and-l-expr' emits as a c-logical-and-expr when the
;;; result type is boolean.  Otherwise, it emits all but the last as a
;;; c-logical-and-expr and uses that as a test predicate to determine if it
;;; should return NIL or the values of the last argument.

(def-l-expr-method translate-l-expr-into-c
    (and-l-expr c-func c-body return-directive)
  (let ((args (cons-cdr (l-expr-form and-l-expr)))
	(needs-values?
	  (type-includes-values-p (l-expr-lisp-return-type and-l-expr))))
    (cond
      ((satisfies-c-required-type-p (l-expr-c-return-type and-l-expr) 'boolean)
       (emit-c-expr-as-directed
	 (collect-logical-and
	   (translate-l-expr-into-c (cons-car args) c-func c-body :c-expr)
	   (cons-cdr args) c-func c-body (l-expr-env and-l-expr))
	 and-l-expr c-func c-body return-directive))
      (t
       (let* ((first-expr (translate-l-expr-into-c
			    (cons-car args) c-func c-body :c-expr))
	      (rest-boolean-args (butlast (cons-cdr args)))
	      (test-expr 
		(if rest-boolean-args
		    (collect-logical-and
		      first-expr rest-boolean-args c-func c-body
		      (l-expr-env and-l-expr))
		    first-expr))
	      (then-statement (make-c-compound-statement nil nil nil nil))
	      (else-statement (make-c-compound-statement nil nil nil nil))
	      (then-result 
		(translate-l-expr-into-c
		  (cons-car (last args)) c-func then-statement
		  return-directive))
	      (result-var?
		(if (and (eq return-directive :c-expr)
			 (or (not (c-compound-statement-empty-p then-statement))
			     needs-values?))
		    (lexical-c-variable-identifier
		      'temp c-func 'obj
		      (storage-classes-for-env (l-expr-env and-l-expr))))))
	 (cond
	   ((and (eq return-directive :c-expr)
		 (null result-var?))
	    (make-c-conditional-expr
	      test-expr then-result
	      (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))
	   (t
	    (when result-var?
	      (emit-expr-to-compound-statement
		(make-c-infix-expr
		  (make-c-name-expr result-var?) "=" then-result)
		then-statement))
	    (when needs-values?
	      (emit-expr-to-compound-statement
		(make-c-infix-expr "Values_count" "=" 1)
		else-statement))
	    (emit-c-expr-as-directed
	      (make-c-cast-expr 'obj (make-c-name-expr "NULL"))
	      and-l-expr c-func else-statement
	      (if result-var? (list result-var?) return-directive))
	    (emit-statement-to-compound-statement
	      (make-c-conditional-statement
		(list test-expr then-statement nil else-statement))
	      c-body)
	    (when result-var?
	      (reclaim-reusable-c-variables
		c-func (list (cons result-var? 'obj)))
	      (make-c-name-expr result-var?)))))))))

(defun collect-logical-and (c-expr remaining-arg-list c-func c-body env)
  (if (null remaining-arg-list)
      c-expr
      (let* ((next-arg-statement (make-c-compound-statement nil nil nil nil))
	     (next-c-expr (translate-l-expr-into-c
			    (cons-car remaining-arg-list) c-func
			    next-arg-statement :c-expr)))
	(cond
	  ((c-compound-statement-empty-p next-arg-statement)
	   (collect-logical-and
	     (make-c-logical-and-expr c-expr next-c-expr)
	     (cons-cdr remaining-arg-list) c-func c-body env))
	  (t
	   (let ((result (lexical-c-variable-identifier
			   'temp c-func 'boolean
			   (storage-classes-for-env env))))
	     (emit-expr-to-compound-statement
	       (make-c-infix-expr result "=" next-c-expr)
	       next-arg-statement)
	     (emit-statement-to-compound-statement
	       (make-c-conditional-statement
		 (list
		   c-expr
		   next-arg-statement
		   nil
		   (make-c-expr-statement (make-c-infix-expr result "=" 0))))
	       c-body)
	     (prog1
		 (collect-logical-and
		   (make-c-name-expr result)
		   (cons-cdr remaining-arg-list) c-func c-body env)
	       (reclaim-reusable-c-variables
		 c-func (list (cons result 'boolean))))))))))




;;; The translator for `or-l-expr' attempts to use C logical or statements to
;;; implement itself.  However, if the result-type is Obj, or if the clauses in
;;; the or emit C statements, then this expands into nested sets of if
;;; statements.

(def-l-expr-method translate-l-expr-into-c
    (or-l-expr c-func c-body return-directive)
  (let ((args (cons-cdr (l-expr-form or-l-expr)))
	(needs-values? (type-includes-values-p
			 (l-expr-lisp-return-type or-l-expr))))
    (cond
      ((satisfies-c-required-type-p (l-expr-c-return-type or-l-expr) 'boolean)
       (emit-c-expr-as-directed
	 (collect-logical-or
	   (translate-l-expr-into-c (cons-car args) c-func c-body :c-expr)
	   (cons-cdr args) c-func c-body (l-expr-env or-l-expr))
	 or-l-expr c-func c-body return-directive))
      (t
       (let ((result (lexical-c-variable-identifier
		       'temp c-func 'obj
		       (storage-classes-for-env (l-expr-env or-l-expr)))))
	 (emit-object-result-or
	   result needs-values? args c-func c-body)
	 (prog1
	     (emit-c-expr-as-directed
	       (make-c-name-expr result)
	       or-l-expr c-func c-body return-directive)
	   (reclaim-reusable-c-variables
	     c-func (list (cons result 'obj)))))))))

(defun emit-object-result-or (var values? args c-func c-body)
  (let* ((arg (cons-car args))
	 (more-args (cons-cdr args))
	 (arg-type (l-expr-lisp-return-type arg))
	 (set-values-count?
	   (and values?
		more-args
		(not (and (consp arg-type)
			  (eq (cons-car arg-type) 'values)
			  (= (length (cons-cdr arg-type)) 1))))))
    (translate-l-expr-into-c arg c-func c-body (list var))
    (when set-values-count?
      (emit-expr-to-compound-statement
	(make-c-infix-expr "Values_count" "=" 1)
	c-body))
    (when more-args
      (let ((next-statement (make-c-compound-statement nil nil nil nil)))
	(emit-object-result-or var values? more-args c-func next-statement)
	(emit-statement-to-compound-statement
	  (make-c-conditional-statement
	    (list (make-c-infix-expr var "==" "NULL")
		  next-statement))
	  c-body)))))
	  

(defun collect-logical-or (c-expr l-expr-args c-func c-body env)
  (if (null l-expr-args)
      c-expr
      (let* ((c-arg-body (make-c-compound-statement nil nil nil nil))
	     (c-arg-expr (translate-l-expr-into-c
			   (cons-car l-expr-args) c-func c-arg-body :c-expr)))
	(cond
	  ((c-compound-statement-empty-p c-arg-body)
	   (collect-logical-or
	     (make-c-logical-or-expr c-expr c-arg-expr)
	     (cons-cdr l-expr-args) c-func c-body env))
	  (t
	   (let ((result (lexical-c-variable-identifier
			   'temp c-func 'boolean
			   (storage-classes-for-env env))))
	     (emit-expr-to-compound-statement
	       (make-c-infix-expr (make-c-name-expr result) "=" c-expr)
	       c-body)
	     (emit-expr-to-compound-statement
	       (make-c-infix-expr
		 (make-c-name-expr result) "="
		 (collect-logical-or
		   c-arg-expr (cons-cdr l-expr-args) c-func c-arg-body env))
	       c-arg-body)
	     (emit-statement-to-compound-statement
	       (make-c-conditional-statement
		 (list (make-c-unary-expr #\! (make-c-name-expr result))
		       c-arg-body))
	       c-body)
	     (reclaim-reusable-c-variables c-func (list (cons result 'boolean)))
	     (make-c-name-expr result)))))))




;;; The list-dynamic-extent special form should translate into C code that
;;; allocates an Obj array, the successive elemnts of the array should be
;;; initialized to the values of the given expressions and pointers to the next
;;; elements of the list, and the result of this expression should be the
;;; pointer (plus 2) to the zeroth element of the array.  Note that there should
;;; be no attempt to recycle the array variable, since the value of this
;;; expression can get used outside of the C expression which will receive this
;;; value.  Because of that, there is no dynamic extent visible to this
;;; translation function within which a reclaimation could occur.  By not
;;; reclaiming the variable, that gives the created list an extent that lasts
;;; until the end of the function calling this special form.

(def-l-expr-method translate-l-expr-into-c
    (list-dynamic-extent-l-expr c-func c-body return-directive)
  (let* ((args (cons-cdr (l-expr-form list-dynamic-extent-l-expr)))
	 (length (length args))
	 (array-var (lexical-c-variable-identifier
		      'temp-list c-func (list 'array 'Obj (* length 2)) nil)))
    (declare (fixnum length))
    (loop for arg in args
	  for car-index fixnum from 0 by 2
	  do
      (emit-expr-to-compound-statement
	(make-c-infix-expr
	  (make-c-subscript-expr
	    (make-c-name-expr array-var)
	    (make-c-literal-expr car-index))
	  "="
	  (or (translate-l-expr-into-c arg c-func c-body :c-expr)
	      (error "Didn't get a c-expr")))
	c-body))
    ;; If there is only one cons, use inline code to initialize the CDR and to
    ;; return the Obj reference to the cons.  If there are more than one, then
    ;; call the function hook_up_cdrs, which will return the Obj reference to
    ;; the initial cons.
    (cond ((= length 1)
	   (emit-expr-to-compound-statement
	     (make-c-infix-expr
	       (make-c-subscript-expr
		 (make-c-name-expr array-var)
		 (make-c-literal-expr 1))
	       "=" (make-c-cast-expr 'obj (make-c-name-expr "NULL")))
	     c-body)
	   (emit-c-expr-as-directed
	     (make-c-cast-expr
	       'obj (make-c-infix-expr
		      (make-c-cast-expr 'uint32 (make-c-name-expr array-var))
		      "+"
		      (make-c-literal-expr 2)))
	     list-dynamic-extent-l-expr c-func c-body return-directive))
	  (t
	   (emit-c-expr-as-directed
	     (make-c-function-call-expr
	       (make-c-name-expr "hook_up_cdrs")
	       (list (make-c-name-expr array-var)
		     (make-c-literal-expr length)
		     (make-c-name-expr "NULL")))
	     list-dynamic-extent-l-expr c-func c-body return-directive)))))




;;; The funcall-internal special form is used to funcall a compiled function
;;; arguments, giving exactly the correct number of arguments.  Note that the
;;; first argument to this form, which indicates whether or not the called
;;; function sets the values count, has already been taken into account during
;;; the type choosing pass, and so may be ignored here.

(def-l-expr-method translate-l-expr-into-c
    (funcall-internal-l-expr c-func c-body return-directive)
  (tl:destructuring-bind-strict (nil nil func &rest args)
    (l-expr-form funcall-internal-l-expr)
    (let ((target-func
	    (make-c-indirect-selection-expr
	      (make-c-cast-expr
		(c-func-type-holding-function-type
		  `(function obj ,(loop repeat (length args) collect 'obj)))
		(translate-l-expr-into-c func c-func c-body :c-expr))
	      "c_function")))
      (multiple-value-bind (c-args c-temporary-variables)
	  (translate-function-arglist-into-c
	    args c-func c-body (l-expr-env funcall-internal-l-expr))
	(prog1
	    (emit-c-expr-as-directed
	      (make-c-function-call-expr target-func c-args)
	      funcall-internal-l-expr c-func c-body return-directive)
	  (when c-temporary-variables
	    (reclaim-reusable-c-variables c-func c-temporary-variables)))))))

;; Note that the appropriate C function type is taken into account by
;; incorporating those types into the typedef definitions of the various
;; "Func_N" types.  The original implementation of funcall-internal fetched a
;; function pointer to a void(*)(void) function from Func and then cast it to
;; the right function type.  Some compilers, such as the Decstation, refused to
;; allow this cast.  Since all compilers allow casting to arbitrary structure
;; pointers, we can incorporate the correct C function types with a set of these
;; typedefs and side-step any queasiness about function casting in C compilers.
;; -jra 12/15/96




;;; The default expansion of `tl:funcall' calls apply, which knows how to supply
;;; optional arguments and knows how to call the internal version of funcall.
;;; In cases where the argument can be seen to be a function (or macro,
;;; actually) this will open code into a call to the given operation.

(def-tl-macro tl:funcall (&environment env function &rest args)
  (cond 
    ((and (consp function)
	  (memqp (car function) '(quote function))
	  (symbolp (cons-second function))
	  (tl:function-information (second function) env))
     (cons (second function) args))
    ((and (consp function)
	  (eq (cons-car function) 'function)
	  (let ((func (cons-second function)))
	    (and (consp func)
		 (eq (cons-car func) 'tl:lambda))))
     (let* ((lambda-expr (cons-second function))
	    (arglist (cons-second lambda-expr))
	    (body (cons-cddr lambda-expr)))
       (multiple-value-bind (decls forms) (split-declarations-and-body body)
         (if (and (loop for arg in arglist 
			never (memqp arg '(&rest &key &aux &optional)))
		  (= (length args) (length arglist)))
	     `(tl:let ,(loop for var in arglist
			     for arg in args
			     collect `(,var ,arg))
	        ,@decls
		,@forms)
	   ;; When funcalling #'(lambda ...) forms, the translator can turn the call
	   ;; into a direct C function call if the function is defined via an FLET.
	   ;; So, all funcalls of lambdas will be made into flets instead.
	   (let ((new-name (make-symbol "FUNCALLED-LAMBDA"))
		 (lambda (cons-second function)))
	     `(tl:flet ((,new-name ,(cons-second lambda)
			  ,@(cons-cddr lambda)))
	        (,new-name ,@args)))))))
    (t
     (fat-and-slow-warning-with-description
      env 
      "FUNCALL can be further optimized using FUNCALL-SIMPLE-COMPILED-FUNCTION~@
       or this warning can be suppressed with a FAT-AND-SLOW declaration."
      `(tl:funcall ,function ,@args))
     `(tl:apply ,function (list-dynamic-extent ,@args)))))




;;; The translation for the `tl:c-comment-form' special form translates its
;;; argument as a :c-expr, wraps a line-comment c-expr around it and emits the
;;; result as directed.

(def-l-expr-method translate-l-expr-into-c
    (c-comment-form-l-expr c-func c-body return-directive)
  (let ((form (l-expr-form c-comment-form-l-expr)))
    (emit-c-expr-as-directed
      (make-c-line-comment-expr
	(translate-l-expr-into-c
	  (cons-third form) c-func c-body :c-expr)
	(cons-second form))
      c-comment-form-l-expr c-func c-body return-directive)))




;;; The translation for `malloc-class-instance' ensures that the typedef for the
;;; needed type is visible in the H file for this translated C file.  Then it
;;; calls the malloc_class_instance function and returns the result.

(def-l-expr-method translate-l-expr-into-c 
  (malloc-class-instance-l-expr c-func c-body return-directive)
  (let* ((l-expr malloc-class-instance-l-expr)
	 (form (l-expr-form l-expr))
	 (type (cons-second form))
	 (info (structure-info type))
	 (c-type-name (struct-c-type-name info))
	 (c-type (struct-c-type info))
	 (alignment (struct-c-alignment info)))
    (when (register-used-class
	   (c-func-c-file c-func) type c-type-name c-type)
      (register-needed-class-typedef
        (c-func-c-file c-func)
	c-type-name
	(struct-c-type info)))
    (emit-c-expr-as-directed
      (make-c-function-call-expr
        (make-c-name-expr "alloc_struct")
	(list (make-c-sizeof-expr c-type-name)
	      (make-c-literal-expr alignment)
	      (make-c-literal-expr
	        (region-number-for-type-and-area
		  type
		  (declared-area-name
		    (l-expr-env l-expr)
		    type)))
	      (make-c-literal-expr (c-type-tag 'class-hdr))))
      l-expr c-func c-body return-directive)))




;;; The translation for `get-slot' will emit either an indirect selection expr,
;;; or a direct selection expr, depending on the C type of the structure.  If
;;; the structure C type is obj or an obvious pointer type (i.e. a list whose
;;; car is 'pointer), then it will emit an indirect selection.  Since we don't
;;; have knowledge of all C types, we'll assume that all other types are direct
;;; structure types, and we'll emit a direct selection expression for those.

;;; Also, this translator ensures that the typedef is visible for structure and
;;; class slot references.

;;; The translator for set-slot is nearly identical, except that the structure
;;; access is also being assigned into.

(def-l-expr-method translate-l-expr-into-c (get-slot-l-expr c-func c-body 
							    return-spec)
  (destructuring-bind (struct slot-name lisp-type c-type &rest ignored)
		      (cons-cdr (l-expr-form get-slot-l-expr))
    (declare (ignore ignored))
    (let ((class-info? (and (class-type-p lisp-type)
			    (class-info lisp-type)))
	  (c-accessor (or (and (stringp slot-name)
			       slot-name)
			  (and (symbolp slot-name)
			       (class-type-p lisp-type)
			       (get-c-name-for-class-and-slot 
				lisp-type slot-name))
			  (translation-error 
			   "Cannot find C accessor: ~s"
			   (l-expr-form get-slot-l-expr))))
	  (c-struct (translate-l-expr-into-c struct c-func c-body :c-expr)))
      (when (and class-info?
		 (register-used-class
		   (c-func-c-file c-func) lisp-type 
		   (struct-c-type-name class-info?)
		   (struct-c-type class-info?)))
	(register-needed-class-typedef
	  (c-func-c-file c-func)
	  (struct-c-type-name class-info?)
	  (struct-c-type class-info?)))
      (emit-c-expr-as-directed
       (cond ((c-types-equal-p c-type 'obj)
	      (make-c-indirect-selection-expr
	        (make-c-cast-expr 
		 (list 'pointer (struct-c-type-name class-info?)) c-struct)
		c-accessor))
	     ((c-pointer-type-p c-type)
	      (make-c-indirect-selection-expr c-struct c-accessor))
	     (t
	      (make-c-direct-selection-expr c-struct c-accessor)))
       get-slot-l-expr c-func c-body return-spec))))

(def-l-expr-method translate-l-expr-into-c (set-slot-l-expr c-func c-body 
							    return-spec)
  (destructuring-bind (struct slot-name lisp-type c-type held-lisp held-c 
		       new-value)
		      (cons-cdr (l-expr-form set-slot-l-expr))
    (declare (ignore held-lisp held-c))
    (let ((class-info? (and (class-type-p lisp-type)
			    (class-info lisp-type)))
	  (c-accessor (or (and (stringp slot-name)
			       slot-name)
			  (and (symbolp slot-name)
			       (class-type-p lisp-type)
			       (get-c-name-for-class-and-slot 
				lisp-type slot-name))
			  (translation-error 
			   "Cannot find C accessor: ~s"
			   (l-expr-form set-slot-l-expr))))
	  (c-struct (translate-l-expr-into-c struct c-func c-body :c-expr)))
      (when (and class-info?
		 (register-used-class
		   (c-func-c-file c-func) lisp-type 
		   (struct-c-type-name class-info?)
		   (struct-c-type class-info?)))
	(register-needed-class-typedef
	  (c-func-c-file c-func)
	  (struct-c-type-name class-info?)
	  (struct-c-type class-info?)))
      (emit-c-expr-as-directed
       (make-c-infix-expr
	 (cond ((c-types-equal-p c-type 'obj)
		(make-c-indirect-selection-expr
		  (make-c-cast-expr 
		    (list 'pointer (struct-c-type-name class-info?)) c-struct)
		  c-accessor))
	       ((c-pointer-type-p c-type)
		(make-c-indirect-selection-expr c-struct c-accessor))
	       (t
		(make-c-direct-selection-expr c-struct c-accessor)))
	 "="
	 (translate-l-expr-into-c new-value c-func c-body :c-expr))
       set-slot-l-expr c-func c-body return-spec))))
