(in-package "TLI")

;;;; Module C-STATE

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






;;;; C Statements




;;; This module contains structures and functions needed to implement data
;;; structures and emitters for C statements within the translator.  In order to
;;; optimize dispatching to emitters for C statements, the c-statement structure
;;; type will contain a single slot, which should hold the compiled function for
;;; the emitter for each type.

(defstruct c-statement
  emitter)




;;; The macro `emit-statement-to-c-file' takes a c-statement structure, a
;;; c-file structure, and an integer representing an indentation level (1 is the
;;; lowest that will be seen) and it dispatches to the appropriate emitter
;;; function for the particular kind of c-statement given.

(defmacro emit-statement-to-c-file (c-statement c-file indentation-level)
  (let ((statement (gensym)))
    `(let ((,statement ,c-statement))
       (unless (c-statement-p ,statement)
	 (error "Emitting ~s as a c-statement, but it isn't one." ,statement))
       (funcall (c-statement-emitter ,statement)
		,statement ,c-file ,indentation-level))))




;;; The macro `def-c-state' is used to define a new type of C statement, with
;;; its emitter It takes a type name, a list of the slots for this type, and a
;;; body of forms used to implement the emitter for this type.

(defmacro def-c-state (name (&rest slots) &body forms)
  (let ((emitter-name (intern (format nil "EMIT-~a" name)))
	(constructor-name (intern (format nil "MAKE-~a" name))))
    `(progn
       (defstruct (,name
		    (:include c-statement (emitter (function ,emitter-name)))
		    (:constructor ,constructor-name ,slots)
		    (:print-function print-c-expr))
	 ,@slots)

       (defun ,emitter-name (statement c-file indent)
	 (declare (fixnum indent))
	 ,@forms))))


(defun print-c-expr (c-expr stream level)
  (declare (ignore level))
  (with-printing-wrapper (c-expr stream)
    (prin1 (tl-type-of c-expr) stream)))




;;; The macro `format-c-state' takes a constant control string, a C file, and
;;; the rest of the args are values formatted within the control string.  Within
;;; the control string, the dispatching character is %, with the following
;;; formatting options available:

;;;   % = (emit-character-to-c-file #\% c-file)
;;;   S = (emit-string-to-c-file arg c-file)
;;;   C = (emit-character-to-c-file arg c-file)
;;;   E = (emit-expression-to-c-file arg c-file arg)
;;;   I = (emit-indentation-to-c-file c-file arg)
;;;   N = (emit-newline-to-c-file c-file)
;;;   T = (emit-statement-to-c-file arg c-file arg)

(defmacro format-c-state
    (constant-control-string c-file &rest formatted-args)
  (let ((forms nil)
	(constant-chars nil)
	(cf (make-symbol "C-FILE"))
	(args formatted-args)
	(arg-count (length formatted-args)))
    (loop with used-args = 0
	  for index from 0 below (length constant-control-string)
	  for char = (schar constant-control-string index)
	  do
      (cond ((not (char= char #\%))
	     (push char constant-chars))
	    (t
	     (when constant-chars
	       (if (null (cdr constant-chars))
		   (push `(emit-character-to-c-file ,(car constant-chars) ,cf)
			 forms)
		   (push `(emit-string-to-c-file
			   ,(make-string-array
			     (length constant-chars)
			     :initial-contents (reverse constant-chars))
			   ,cf)
			 forms))
	       (setq constant-chars nil))
	     (incf index)
	     (setq char (schar constant-control-string index))
	     (ecase char
	       ((#\%)
		(push #\% constant-chars))
	       ((#\S #\s)
		(incf used-args)
		(push `(emit-string-to-c-file ,(pop args) ,cf) forms))
	       ((#\C #\c)
		(incf used-args)
		(push `(emit-character-to-c-file ,(pop args) ,cf) forms))
	       ((#\E #\e)
		(incf used-args 2)
		(push `(emit-expression-to-c-file ,(pop args) ,cf ,(pop args)) forms))
	       ((#\I #\i)
		(incf used-args)
		(push `(emit-indentation-to-c-file ,cf ,(pop args))
		      forms))
	       ((#\N #\n)
		(push `(emit-newline-to-c-file ,cf) forms))
	       ((#\T #\t)
		(incf used-args 2)
		(push `(emit-statement-to-c-file ,(pop args) ,cf ,(pop args)) forms)))))
	  finally
	    (progn
	      (when constant-chars
		(if (null (cdr constant-chars))
		    (push `(emit-character-to-c-file ,(car constant-chars) ,cf)
			  forms)
		    (push `(emit-string-to-c-file
			    ,(make-string-array
			      (length constant-chars)
			      :initial-contents (reverse constant-chars))
			    ,cf)
			  forms))
		(setq constant-chars nil))
	      (unless (= used-args arg-count)
		(warn "Format-c-state used ~a args, but was given ~a"
		      used-args arg-count))
	      (return
		`(let ((,cf ,c-file))
		   ,@(reverse forms)))))))




;;; C labels for goto tags, switch case values, and switch default branch points
;;; will be represented via a c-label-statement.  The label slot of this
;;; structure will contain a string, which be either a goto tag string, the
;;; string "case <x>" where x is a switch dispatch value, or the string
;;; "default".  The emitter for a label will emit the given indentation level
;;; minus one, emit the held string, a colon, and a newline.  This indentation
;;; level sets labels even with the opening curly braces of their containing
;;; compound statements, and sets switch case values and default tags at a level
;;; even with the opening switch statement.

(def-c-state c-label-statement (label-string)
  (format-c-state "%I %S:%N" c-file
		  (1- indent)
		  (c-label-statement-label-string statement)))




;;; This statement contains a list of alternating c-expressions and
;;; c-statements.  Each expression is a condition for the following statement in
;;; a multiway conditional statement (H&S 3rd, p. 218).  If the final statement
;;; for the multiway conditional is a simple else (i.e. always perform the last
;;; statement if no preceeding clause was selected) then a NIL should be found
;;; in place of the c-expression in the last two elements of the list.  A c-
;;; conditional-statement emits the given indentation, emits the string "if (",
;;; emits the first c-expression, emits the string ") ", then emits the
;;; corresponding c-statement at the given indentation level plus one.  For all
;;; subsequent pairs, it should first emit the given indentation level and the
;;; string "else ".  If the c-expression is non-null then emit "if (", emit the
;;; c-expression, and emit ") ".  Then emit the c-statement at the indentation
;;; level plus one.

;;; Note that if the C statements within the conditional are compound
;;; statements, then their opening curly braces will be emitted on the same line
;;; as the expression.  If it is any other kind of statement or is a compound
;;; statement holding a single statement, the act of emitting its indentation
;;; will force a fresh line, placing the statement alone on the next line, one
;;; indentation level further in from the if statement.

;;; The proof that we will not experience the dangling else problem is only
;;; valid if all statements held within conditional statements are emitted as
;;; compound statements.  This emitter should verify that by checking for held
;;; conditional statements and wrapping them in a compound statement if any are
;;; found.

(def-c-state c-conditional-statement (expressions-and-statements)
  (emit-indentation-to-c-file c-file indent)
  (emit-c-conditional-expressions-and-statements
    (c-conditional-statement-expressions-and-statements statement)
    c-file indent))




;;; C compound statements will be used to hold all emitted C statements,
;;; statement labels, and lexically scoped variable declarations.  When a
;;; translation is occurring on a Lisp function, the function that performs a
;;; translation of a Lisp expression will be given a Lisp s-expression, a
;;; required type spec for the result of the translation, a prolog C compound
;;; statement, and an epilog C compound statement.  The translation function
;;; will return a C expression structure that returns the values of the
;;; translated Lisp expression, but it will also emit statements into the prolog
;;; C compound statement which must be run before the C expression is evaluated,
;;; and it will emit cleanup statements into the epilog C compound statement
;;; which must be run after the C expression is evaluated.

;;; The following functions will be available for C-compound-statements:
;;; make-c-compound- statement, emit-statement-to-compound-statement, and
;;; emit-label-to-compound-statement.  Make-c-compound-statment is an
;;; argumentless constructor.  Emit-statement-to-compound- statement is used to
;;; add a new statement to the end of the sequence of statements currently held
;;; in the compound-statement.  Note that if the statement being added is itself
;;; a compound statement, then the effect is to concatenate its sequence of
;;; statements to the end of the containing compound statement.  This eliminates
;;; unneccessary nested curly braces, and is a needed memory optimization given
;;; how heavily I intend to use compound statments when translating.

;;; If the compound statement contains no variable declaractions or statements,
;;; then it will be emitted as a single semicolon and a newline.  If the
;;; compound statement contains no variable declarations and a single contained
;;; statement that is neither a c-conditional-statement nor a c-label-statement,
;;; then it will emit a newline and its one statement at the given indentation.
;;; Otherwise, it will emit a curly brace, a newline, emit all variable
;;; declarations at the given indentation level, emit all its held statements
;;; and labels at the given indentation level, emit a null statement if the last
;;; thing emitted was a label, then emit its final curly brace at the given
;;; indentation level minus one.

;;; Note that statement labels will be emitted into compound statments just as C
;;; statements are, except that they will be preceeded by one fewer level of
;;; indentation, and if the last thing within a compound statement is a label, a
;;; null statement (i.e. a line containing nothing but a semi-colon) is emitted
;;; after the label, but before the final curly brace of the compound statement.

;;; Also note that conditional statements held within compound statements will
;;; always be enclosed within curly braces to protect them from inappropriately
;;; acquiring dangling else clauses from potentially surrounding conditional
;;; expressions.

(def-c-state c-compound-statement
    (declarations declarations-tail statements statements-tail)
  (let ((declarations (c-compound-statement-declarations statement))
	(statements (c-compound-statement-statements statement)))
    (cond ((and (null statements)
		(/= indent 1))		; must have braces at top level
	   (emit-indentation-to-c-file c-file indent)
	   (emit-character-to-c-file #\; c-file)
	   (emit-newline-to-c-file c-file))
	  ((and (null declarations)
		statements
		(null (cons-cdr statements))
		(/= indent 1)		; must have braces at top level
		(not (c-conditional-statement-p (car statements)))
		(not (c-label-statement-p (car statements))))
	   (emit-statement-to-c-file (car statements) c-file indent))
	  (t
	   (when (zerop (c-file-line-length c-file))
	     (emit-indentation-to-c-file c-file (1- indent)))
	   (emit-character-to-c-file #\{ c-file)
	   (emit-newline-to-c-file c-file)
	   (when declarations
	     (emit-c-decl-list-to-c-file declarations c-file indent)
	     (emit-newline-to-c-file c-file)) ; one blank line after declarations
	   (loop with dead-code = nil
		 for last-statement = nil then substatement
		 for substatement in statements
		 do
	     ;; After seeing a go or return statement, decline to emit any
	     ;; statments that cannot cause execution to reenter, such as expr
	     ;; statements or other gos.  Label statements and anything that can
	     ;; contain a compound statement (which could contain a label) could
	     ;; cause execution to resume.
	     (unless (and dead-code
			  (label-excluded-statement-p substatement))
	       (setq dead-code nil)
	       (when (and (c-label-statement-p substatement)
			  (c-label-statement-p last-statement)
			  (not (eql (search "case "
					    (c-label-statement-label-string
					      last-statement))
				    0)))
		 (emit-character-to-c-file #\; c-file)
		 (emit-newline-to-c-file c-file))
	       (emit-statement-to-c-file substatement c-file indent)
	       (when (unconditional-branch-statement-p substatement)
		 (setq dead-code t)))
		
		 finally
		   (when (and last-statement
			      (c-label-statement-p last-statement))
 		     (emit-character-to-c-file #\; c-file)
		     (emit-newline-to-c-file c-file)))
	   (emit-indentation-to-c-file c-file (1- indent))
	   (emit-character-to-c-file #\} c-file)
	   (emit-newline-to-c-file c-file)))))




;;; The function `emit-statement-to-compound-statement' is used when translating
;;; to add another statement to the end of a compound statement.

(defun emit-statement-to-compound-statement (statement compound-statement)
  (unless (c-statement-p statement)
    (error "~A is not a C-statement."))
  (if (c-compound-statement-p statement)
      (let ((substatements (c-compound-statement-statements statement)))
	(if (null (c-compound-statement-declarations statement))
	    (if substatements
		(let ((statements-tail (c-compound-statement-statements-tail
					 compound-statement)))
		  (cond (statements-tail
			 (setf (cdr statements-tail) substatements)
			 (setf (c-compound-statement-statements-tail
				 compound-statement)
			       (c-compound-statement-statements-tail
				 statement)))
			(t
			 (setf (c-compound-statement-statements
				 compound-statement)
			       substatements)
			 (setf (c-compound-statement-statements-tail
				 compound-statement)
			       (c-compound-statement-statements-tail
				 statement)))))
		;; Do nothing
		nil)
	    (error "Can't nest declarations within compound statements.")))
      (let ((statements-tail (c-compound-statement-statements-tail
			       compound-statement))
	    (new-cons (cons statement nil)))
	(if statements-tail
	    (setf (cdr statements-tail) new-cons)
	    (setf (c-compound-statement-statements compound-statement)
		  new-cons))
	(setf (c-compound-statement-statements-tail compound-statement)
	      new-cons))))

(defmacro emit-expr-to-compound-statement (c-expr c-compound-statement)
  `(emit-statement-to-compound-statement
     (make-c-expr-statement ,c-expr) ,c-compound-statement))




;;; The function `emit-declaration-to-compound-statement' takes a declaration
;;; and a C compound statement and adds the declaration to the end of the set
;;; for this compound statement.

(defun emit-declaration-to-compound-statement (decl compound-statement)
  (let ((decl-tail?
	  (c-compound-statement-declarations-tail compound-statement))
	(new-tail (cons decl nil)))
    (if decl-tail?
	(setf (cdr decl-tail?) new-tail)
	(setf (c-compound-statement-declarations compound-statement) new-tail))
    (setf (c-compound-statement-declarations-tail compound-statement) new-tail)
    nil))




;;; The macro `c-compound-statement-length' takes a compound-statement and
;;; returns the number of held statements within it.  The macro
;;; `c-compound-statement-declarations-length' takes a compound statement and
;;; returns the number of declarations it will emit at its head.  The macro
;;; `c-compound-statement-empty-p' takes a compound-statement and returns
;;; whether or not it contains any statements or declarations.

(defmacro c-compound-statement-length (compound)
  `(the fixnum (length (c-compound-statement-statements ,compound))))

(defmacro c-compound-statement-declarations-length (compound)
  `(the fixnum (length (c-compound-statement-declarations ,compound))))

(defmacro c-compound-statement-empty-p (compound)
  (if (symbolp compound)
      `(and (null (c-compound-statement-statements ,compound))
	    (null (c-compound-statement-declarations ,compound)))
      (let ((c (gensym)))
	`(let ((,c ,compound))
	   (c-compound-statement-empty-p ,c)))))




;;; The function `emit-c-conditional-expressions-statements' is used by the
;;; emitter for C conditional statements.  This function can recurse onto itself
;;; when it finds a C conditional statement in its last statement.  The function
;;; has been moved here from further up in the file near c-conditional-
;;; statements to pick up the accessors and macros for compound-statments.

(defun emit-c-conditional-expressions-and-statements
    (expressions-and-statements c-file indent)
  (loop for clause on expressions-and-statements by #'cddr
	for expression? = (cons-car clause)
	for substatement = (cons-second clause)
	;; Stop early if the final else contains an empty statement.
	until (and (null expression?)
		   (null (cddr clause))
		   (c-compound-statement-p substatement)
		   (c-compound-statement-empty-p substatement))
	do
    (unless (eq clause expressions-and-statements)
      (emit-indentation-to-c-file c-file indent)
      (emit-string-to-c-file "else " c-file))
    (when expression?
      (emit-string-to-c-file "if (" c-file)
      (emit-expression-to-c-file expression? c-file indent)
      (emit-string-to-c-file ") " c-file))
    ;; The first two clauses of this emitter attempt to collapse nested
    ;; c-conditional-statements in the else clause into this one.
    (cond ((and (null expression?)
		(null (cons-cddr clause))
		(c-conditional-statement-p substatement))
	   (emit-c-conditional-expressions-and-statements
	     (c-conditional-statement-expressions-and-statements
	       substatement)
	     c-file indent))
	  ((and (null expression?)
		(null (cons-cddr clause))
		(c-compound-statement-p substatement)
		(= (c-compound-statement-declarations-length substatement) 0)
		(= (c-compound-statement-length substatement) 1)
		(c-conditional-statement-p
		  (cons-car
		    (c-compound-statement-statements substatement))))
	   (emit-c-conditional-expressions-and-statements
	     (c-conditional-statement-expressions-and-statements
	       (cons-car
		 (c-compound-statement-statements substatement)))
	     c-file indent))
	  ((c-conditional-statement-p substatement)
	   (let ((wrapper (make-c-compound-statement nil nil nil nil)))
	     (emit-statement-to-compound-statement substatement wrapper)
	     (emit-statement-to-c-file wrapper c-file (1+ indent))))
	  (t
	   (emit-statement-to-c-file substatement c-file (1+ indent))))))




;;; The `c-expr-statement' merely evaluates its argument expression.

(def-c-state c-expr-statement (expr)
  (format-c-state "%I%E;%N" c-file
		  indent
		  (c-expr-statement-expr statement)
		  indent))



;;; The `c-while-statement' will contain a test expression and a body statement.

(def-c-state c-while-statement (test-expr body-statement)
  (format-c-state "%Iwhile (%E) %T%N" c-file
		  indent
		  (c-while-statement-test-expr statement)
		  indent
		  (c-while-statement-body-statement statement)
		  (1+ indent)))




;;; The `c-do-statement' will contain a body statement and a test expression.

(def-c-state c-do-statement (body-statement test-expr)
  (format-c-state "%Ido %T%Iwhile (%E);%N" c-file
		  indent
		  (c-do-statement-body-statement statement)
		  (1+ indent)
		  indent
		  (c-do-statement-test-expr statement)
		  indent))



;;; The `c-for-statement' contains an init-expr?, a test-expr?, a step-expr? and
;;; a body-statement.

(def-c-state c-for-statement (init-expr? test-expr? step-expr? body-statement)
  (emit-indentation-to-c-file c-file indent)
  (emit-string-to-c-file "for (" c-file)
  (when (c-for-statement-init-expr? statement)
    (emit-expression-to-c-file
      (c-for-statement-init-expr? statement) c-file indent))
  (emit-character-to-c-file #\; c-file)
  (when (c-for-statement-test-expr? statement)
    (emit-expression-to-c-file
      (c-for-statement-test-expr? statement) c-file indent))
  (emit-character-to-c-file #\; c-file)
  (when (c-for-statement-step-expr? statement)
    (emit-expression-to-c-file
      (c-for-statement-step-expr? statement) c-file indent))
  (emit-string-to-c-file ") " c-file)
  (emit-statement-to-c-file
    (c-for-statement-body-statement statement) c-file (1+ indent)))




;;; The `c-switch-statement' contains a control-expr and a body statement.
;;; Within the body statement there should be label and break statements that
;;; affect where control flows through the switch statement.

(def-c-state c-switch-statement (control-expr body-statement)
  (format-c-state "%Iswitch (%E) %T" c-file
		  indent
		  (c-switch-statement-control-expr statement)
		  indent
		  (c-switch-statement-body-statement statement)
		  (1+ indent)))




;;; The `c-break-statement'  `c-continue-statement' take no arguments and simply
;;; emit indentation, their names, a semicolon, and a newline.

(def-c-state c-break-statement ()
  (declare (ignore statement))
  (format-c-state "%Ibreak;%N" c-file indent))

(def-c-state c-continue-statement ()
  (declare (ignore statement))
  (format-c-state "%Icontinue;%N" c-file indent))




;;; The `c-return-statement' takes an optional return value expression.

(def-c-state c-return-statement (value-expr?)
  (format-c-state "%Ireturn" c-file indent)
  (when (c-return-statement-value-expr? statement)
    (format-c-state " %E" c-file
		    (c-return-statement-value-expr? statement)
		    indent))
  (format-c-state ";%N" c-file))




;;; The `c-goto-statement' takes a target label and emits a goto to that
;;; location.

(def-c-state c-goto-statement (label-string)
  (format-c-state "%Igoto %S;%N" c-file
		  indent
		  (c-goto-statement-label-string statement)))




;;; The `c-null-statement' takes no arguments and emits indentation, a singleton
;;; semicolon, and a newline.

(def-c-state c-null-statement ()
  (declare (ignore statement))
  (format-c-state "%I;%N" c-file indent))




;;; The `c-#if-statement' takes a list of alternating C-exprs and C statements,
;;; and will emit them in a #if-#elif-#else-#endif wrapper, where the C-exprs
;;; are the predicates to #if and #elif statements, and the C statements are
;;; emitted between them.

(def-c-state c-#if-statement (exprs-and-statements)
  (let ((exprs-and-states (c-#if-statement-exprs-and-statements statement)))
    (emit-freshline-to-c-file c-file)
    (format-c-state "#if %E%N%T" c-file
		    (car exprs-and-states) indent
		    (second exprs-and-states) (1+ indent))
    (loop for (expr? statement) on (cddr exprs-and-states) by #'cddr do
      (emit-freshline-to-c-file c-file)
      (if expr?
	  (format-c-state "#elif %E%N%T" c-file
			  expr? indent
			  statement (1+ indent))
	  (format-c-state "#else%N%T" c-file
			  statement (1+ indent))))
    (emit-freshline-to-c-file c-file)
    (format-c-state "#endif%N" c-file)))




;;; These functions were moved down here to pick up the predicates for
;;; c-statements.

(defun label-excluded-statement-p (statement)
  (or (c-expr-statement-p statement)
      (c-goto-statement-p statement)
      (c-return-statement-p statement)
      (c-break-statement-p statement)))

(defun unconditional-branch-statement-p (statement)
  (or (c-goto-statement-p statement)
      (c-return-statement-p statement)
      (c-break-statement-p statement)))
