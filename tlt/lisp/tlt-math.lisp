(in-package "TLI")

;;;; Module TLT-MATH

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






;;;; Primitive Operations for TL Arithmetic




;;; This module implements arithmetic operations that have direct translations
;;; into C.

;;; Note that because the Lisp symbol + has special meaning as a variable in the
;;; read-eval-print loop, we have to import that symbol into TL and export it.
;;; To give it OUR implementation within the TL environment, + is now a special
;;; form that expands into calls to tli::plus.

;;; The macro `plus' always expands into its two argument variety.  Note that
;;; plus only supports fixnums and double-floats, so no integer overflow is
;;; allowed.

(def-tl-macro plus (&rest numbers)
  (cond ((null numbers)
	 0)
	((null (cons-cdr numbers))
	 (cons-car numbers))
	((null (cons-cddr numbers))
	 `(+-two-arg ,(cons-car numbers) ,(cons-second numbers)))
	(t
	 `(plus (+-two-arg ,(cons-car numbers) ,(cons-second numbers))
		,@(cons-cddr numbers)))))

(def-tl-macro tl:1+ (number)
  `(plus ,number 1))

(defun l-expr-wants-sint32-type-p (l-expr)
  (and (not (l-expr-constant-p l-expr))
       (loop with type = (uncoerced-l-expr-c-return-type l-expr)
	     for c-int-type in (c-integer-types)
	     thereis (satisfies-c-required-type-p type c-int-type))))

(tl:declaim (tl:functional +-two-arg))

(def-c-translation +-two-arg (number1 number2)
  ((lisp-specs :ftype ((number number) number))
   `(+ ,number1 ,number2))
  ;; Use sint32 version when at least one of the arguments is not a constant and
  ;; has an uncoerced type that matches one of the built-in C integer types.
  ;; Note that both args cannot be constants, since that would already have been
  ;; constant folded away.
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:test (or (l-expr-wants-sint32-type-p number1-l-expr)
			  (l-expr-wants-sint32-type-p number2-l-expr))
		:c-type ((sint32 sint32) sint32))
   (make-c-infix-expr number1 "+" number2))
  ;; If neither argument is already an sint32, it is most efficient to use the
  ;; following fixnum-add version.
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((obj obj) obj))
   (make-c-cast-expr
     'obj
     (make-c-line-comment-expr
       (make-c-infix-expr
	 (make-c-infix-expr (make-c-cast-expr 'sint32 number1)
			    "+" (make-c-cast-expr 'sint32 number2))
	 "-" 1)
       "Fixnum add")))
  ((trans-specs :lisp-type ((fixnum double-float) double-float)
		:c-type ((sint32 double) double))
   (make-c-infix-expr
     (coerce-c-expr-result-to-type
       number1 'sint32 'double (l-expr-env function-call-l-expr))
     "+" number2))
  ((trans-specs :lisp-type ((double-float fixnum) double-float)
		:c-type ((double sint32) double))
   (make-c-infix-expr
     number1 "+"
     (coerce-c-expr-result-to-type
       number2 'sint32 'double (l-expr-env function-call-l-expr))))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (make-c-infix-expr number1 "+" number2))
  ;; If we can do no better than generic arithmetic, require the fat-and-slow
  ;; declaration.
  ((trans-specs :lisp-type ((number number) number)
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) '+
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_plus" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_plus") (list number1 number2))))

(def-tl-macro tl:- (&rest numbers)
  (cond ((null numbers)
	 0)
	((null (cons-cdr numbers))
	 `(negate ,(cons-car numbers)))
	((null (cons-cddr numbers))
	 `(minus-two-arg ,(cons-car numbers) ,(cons-second numbers)))
	(t
	 `(tl:- (minus-two-arg ,(cons-car numbers) ,(cons-second numbers))
		,@(cons-cddr numbers)))))

(def-tl-macro tl:1- (number)
  `(tl:- ,number 1))

(tl:declaim (tl:functional negate))

(def-c-translation negate (number)
  ((lisp-specs :ftype ((number) number))
   `(- ,number))
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:test (l-expr-wants-sint32-type-p number-l-expr)
		:c-type ((sint32) sint32))
   (make-c-unary-expr #\- number))
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((obj) obj))
   (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-literal-expr 2) "-" (make-c-cast-expr 'sint32 number))))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double))
   (make-c-unary-expr #\- number))
  ((trans-specs :lisp-type ((number) number)
		:c-type ((obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:-
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_negate" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_negate") (list number))))

(tl:declaim (tl:functional minus-two-arg))

(def-c-translation minus-two-arg (number1 number2)
  ((lisp-specs :ftype ((number number) number))
   `(- ,number1 ,number2))
  ;; Use sint32 version when at least one of the arguments is not a constant and
  ;; has an uncoerced type that matches one of the built-in C integer types.
  ;; Note that both args cannot be constants, since that would already have been
  ;; constant folded away.
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:test (or (l-expr-wants-sint32-type-p number1-l-expr)
			  (l-expr-wants-sint32-type-p number2-l-expr))
		:c-type ((sint32 sint32) sint32))
   (make-c-infix-expr number1 "-" number2))
  ;; If neither argument is already an sint32, it is most efficient to use the
  ;; following fixnum-add version.
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((obj obj) obj))
   (make-c-cast-expr
     'obj
     (make-c-line-comment-expr
       (make-c-infix-expr
	 (make-c-infix-expr (make-c-cast-expr 'sint32 number1)
			    "-" (make-c-cast-expr 'sint32 number2))
	 "+" 1)
       "Fixnum subtract")))
  ((trans-specs :lisp-type ((fixnum double-float) double-float)
		:c-type ((sint32 double) double))
   (make-c-infix-expr
     (coerce-c-expr-result-to-type
       number1 'sint32 'double (l-expr-env function-call-l-expr))
     "-" number2))
  ((trans-specs :lisp-type ((double-float fixnum) double-float)
		:c-type ((double sint32) double))
   (make-c-infix-expr
     number1 "-"
     (coerce-c-expr-result-to-type
       number2 'sint32 'double (l-expr-env function-call-l-expr))))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (make-c-infix-expr number1 "-" number2))
  ;; If we can do no better than generic arithmetic, require the fat-and-slow
  ;; declaration.
  ((trans-specs :lisp-type ((number number) number)
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:-
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_minus" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_minus") (list number1 number2))))

(def-tl-macro multiply (&rest numbers)
  (cond ((null numbers)
	 1)
	((null (cons-cdr numbers))
	 (cons-car numbers))
	((null (cons-cddr numbers))
	 `(multiply-two-arg ,(cons-car numbers) ,(cons-second numbers)))
	(t
	 `(multiply
	    (multiply-two-arg ,(cons-car numbers) ,(cons-second numbers))
	    ,@(cons-cddr numbers)))))

(def-c-translation multiply-two-arg (number1 number2)
  ((lisp-specs :ftype ((number number) number))
   `(* ,number1 ,number2))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32))
   (make-c-mult-expr number1 #\* number2))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (make-c-mult-expr number1 #\* number2))
  ((trans-specs :lisp-type ((number number) number)
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) '*
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_multiply" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_multiply") (list number1 number2))))

(def-tl-macro tl:/ (&rest numbers)
  (cond ((null numbers)
	 (error "/ called with no arguments"))
	((null (cons-cdr numbers))
	 `(tl:/ 1 ,(cons-car numbers)))
	((null (cons-cddr numbers))
	 `(divide-two-arg ,(cons-car numbers) ,(cons-second numbers)))
	(t
	 `(tl:/ (divide-two-arg ,(cons-car numbers) ,(cons-second numbers))
		,@(cons-cddr numbers)))))

(def-c-translation divide-two-arg (number1 number2)
  ((lisp-specs :ftype ((number number) number))
   `(/ ,number1 ,number2))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((obj obj) obj))
   (translation-error
     "/ cannot be called on two fixnums in TL: (/ ~s ~s).  Use floor or ~
       another truncating divide."
     (l-expr-pretty-form number1-l-expr)
     (l-expr-pretty-form number2-l-expr)))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (make-c-mult-expr number1 #\/ number2))
  ((trans-specs :lisp-type ((number number) number)
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:/
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_divide" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_divide") (list number1 number2))))

(defmacro def-numeric-comparitors (&rest symbol-op-and-name-sets)
  `(progn
     ,@(loop for (symbol op-string name) in symbol-op-and-name-sets
	     for two-arg-op = (intern (format nil "~a-TWO-ARG" symbol)
				      *tl-package*)
	     for lisp-symbol = (intern (symbol-name symbol) *lisp-package*)
	     for generic-op = (intern (format nil "GENERIC-~a" name)
				      *tl-package*)
	     for generic-string = (c-base-string generic-op)
	     for c-expr-maker
		 = (intern
		     (format
		       nil "MAKE-~a"
		       (second (assoc op-string c-expr-infix-operator-types
				      :test #'(lambda (x y)
						(when (stringp y)
						  (setq x (prog1 y (setq y x))))
						(member x y :test #'string=))))))
	     
	     nconc
	     `((def-tl-macro ,symbol (number &rest more-numbers)
		 (cond
		   ((null more-numbers)
		    `(tl:progn ,number t))
		   ((null (cons-cdr more-numbers))
		    `(,',two-arg-op ,number ,@more-numbers))
		   ((and (simple-argument-p number)
			 (loop for arg in more-numbers
			       always (simple-argument-p arg)))
		    `(tl:and ,@(loop for arg-cons on (cons number more-numbers)
				     while (cdr arg-cons)
				     collect `(,',two-arg-op
						   ,(first arg-cons)
						   ,(second arg-cons)))))

		   (t
		    (let* ((arglist (cons number more-numbers))
			   (varlist (loop repeat (length arglist)
					  collect (gensym))))
		      `(tl:let ,(loop for var in varlist
				      for arg in arglist
				      collect (list var arg))
			 (tl:and ,@(loop for arg-cons on varlist
					 while (cdr arg-cons)
					 collect `(,',two-arg-op
						       ,(first arg-cons)
						       ,(second arg-cons)))))))))
	       (tl:declaim (tl:functional ,two-arg-op))
	       (def-c-translation ,two-arg-op (number1 number2)
		 ((lisp-specs :ftype ((number number) t))
		  `(,',lisp-symbol ,number1 ,number2))
		 ((trans-specs
		    :lisp-type ((fixnum fixnum) t)
		    :test (or (l-expr-wants-sint32-type-p number1-l-expr)
			      (l-expr-wants-sint32-type-p number2-l-expr))
		    :c-type ((sint32 sint32) boolean))
		  (,c-expr-maker number1 ,op-string number2))
		 ((trans-specs :lisp-type ((fixnum fixnum) t)
			       :c-type ((obj obj) boolean))
		  (,c-expr-maker (make-c-cast-expr 'sint32 number1)
				 ,op-string
				 (make-c-cast-expr 'sint32 number2)))
		 ((trans-specs :lisp-type ((double-float double-float) t)
			       :c-type ((double double) boolean))
		  (,c-expr-maker number1 ,op-string number2))
		 ((trans-specs :lisp-type ((number number) t)
			       :c-type ((obj obj) obj))
		  (fat-and-slow-warning
		    (l-expr-env function-call-l-expr)
		    ',symbol (l-expr-pretty-form function-call-l-expr))
		  (register-needed-function-extern
		    (c-func-c-file c-func)
		    '("extern") 'obj ,generic-string '(obj obj))
		  (make-c-function-call-expr
		    (make-c-name-expr ,generic-string)
		    (list number1 number2))))))))


(def-numeric-comparitors (tl:< "<" "LESS-THAN")
    (tl:> ">" "GREATER-THAN")
  (tl:<= "<=" "LESS-THAN-OR-EQUAL")
  (tl:>= ">=" "GREATER-THAN-OR-EQUAL")
  (tl:=  "==" "NUMERIC-EQUAL")
  (tl:/= "!=" "NUMERIC-NOT-EQUAL"))

(def-tl-macro tl:zerop (&environment env value)
  (if (tl-subtypep (expression-result-type value env) 'double-float)
      `(tl:= ,value 0.0)
      `(tl:= ,value 0)))

(def-tl-macro tl:plusp (&environment env value)
  (if (tl-subtypep (expression-result-type value env) 'double-float)
      `(tl:> ,value 0.0)
      `(tl:> ,value 0)))

(def-tl-macro tl:minusp (&environment env value)
  (if (tl-subtypep (expression-result-type value env) 'double-float)
      `(tl:< ,value 0.0)
      `(tl:< ,value 0)))

(def-tl-macro tl:float (value &optional float-type-arg)
  (if (and float-type-arg
	   (not (constantp float-type-arg)))
      (error "The second argument to float is discarded in TL, so please pass a ~
              constant instead of ~s."
	     float-type-arg)
      `(coerce-to-float ,value)))

(tl:declaim (tl:functional coerce-to-float))

(def-c-translation coerce-to-float (number)
  ((lisp-specs :ftype ((number) double-float))
   `(float ,number 0.0d0))
  ((trans-specs :lisp-type ((fixnum) double-float)
		:c-type ((sint32) double))
   (make-c-cast-expr 'double number))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double))
   number)
  ((trans-specs :lisp-type ((number) double-float)
		:c-type ((obj) double))
   (let ((temp-var nil)
	 (expr number))
     (unless (c-name-expr-p number)
       (setq temp-var (reusable-c-variable-identifier
			'temp c-func 'obj (l-expr-env function-call-l-expr)))
       (emit-expr-to-compound-statement
	 (make-c-infix-expr temp-var "=" number)
	 c-compound-statement)
       (setq expr (make-c-name-expr temp-var)))
     (make-c-conditional-expr
       (translate-type-check-predicate expr 'fixnum 'number)
       (make-c-cast-expr
	 'double
	 (make-c-infix-expr (make-c-cast-expr 'sint32 expr) ">>" 2))
       ;; Since we already know its a number, if it is not a fixnum in the test
       ;; above, then it must be a double-float, and we don't need to perform
       ;; that test here, since any safety tests will have already verified that
       ;; it is a number.
       (make-c-indirect-selection-expr
	 (make-c-cast-expr '(pointer ldouble) expr)
	 "body")))))

(def-c-translation tl:sxhash (object)
  ((lisp-specs :ftype ((t) fixnum))
   `(sxhash ,object))
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((sint32) sint32))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "abs")
       (list (coerce-c-expr-result-to-type
	       object 'sint32 'int (l-expr-env function-call-l-expr))))
     'int 'sint32 (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((double-float) fixnum)
		:c-type ((double) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "sxhash_double_float" '(double))
   (make-c-function-call-expr
     (make-c-name-expr "sxhash_double_float")
     (list object)))
  ((trans-specs :lisp-type ((string) fixnum)
		:c-type (((array unsigned-char)) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'sint32 "sxhash_string" '((array unsigned-char)))
   (make-c-function-call-expr
     (make-c-name-expr "sxhash_string")
     (list object)))
  ((trans-specs :lisp-type ((symbol) fixnum)
		:c-type ((Obj) sint32))
   (make-c-cast-expr
     'sint32
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 object)
       ">>" (make-c-literal-expr 3))))
  ((trans-specs :lisp-type ((compiled-function) fixnum)
		:c-type ((Obj) sint32))
   (make-c-cast-expr
     'sint32
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 object)
       ">>" (make-c-literal-expr 3))))
  ((trans-specs :lisp-type (((array (unsigned-byte 16))) fixnum)
		:c-type (((array uint16)) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'sint32 "sxhash_array_16" '((array uint16)))
   (make-c-function-call-expr
     (make-c-name-expr "sxhash_array_16")
     (list object)))
  ((trans-specs :c-type ((obj) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'sint32 "generic_sxhash" '(Obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_sxhash")
     (list object))))

(def-c-translation decompose-float (float uint16-array)
  ((lisp-specs :ftype ((double-float (array (unsigned-byte 16)))
		       void))
   `(error "No LISP implementation of (DECOMPOSE-FLOAT ~s ~s)" 
	   ,float ,uint16-array))
  ((trans-specs :c-type ((double (array uint16)) void))
   (make-c-function-call-expr
     (make-c-name-expr "memcpy")  ;; arg order is  dest, src
     (list uint16-array
	   (make-c-unary-expr #\& float)
	   (make-c-sizeof-expr (c-type-string 'double))))))

(def-tl-macro tl:max (arg &rest args)
  (cond ((null args)
	 arg)
	(t
	 (let ((x (gensym))
	       (y (gensym)))
	   `(tl:let ((,x ,arg)
		     (,y (tl:max ,@args)))
	      (tl:if (tl:> ,x ,y)
		     ,x
		     ,y))))))

(def-tl-macro tl:min (arg &rest args)
  (cond ((null args)
	 arg)
	(t
	 (let ((x (gensym))
	       (y (gensym)))
	   `(tl:let ((,x ,arg)
		     (,y (tl:min ,@args)))
	      (tl:if (tl:< ,x ,y)
		     ,x
		     ,y))))))

(defmacro def-log-operators (name-op-identity-tuples)
  (cons
    'progn
    (loop for (name op-string identity) in name-op-identity-tuples
	  for tl-name = (intern (symbol-name name) *tl-package*)
	  for lisp-name = (intern (symbol-name name) *lisp-package*)
	  for c-trans-name = (intern (format nil "~a-TWO-ARG" lisp-name))
	  append
	  `((def-tl-macro ,tl-name (&rest numbers)
	      (cond ((loop for arg in numbers
			   always (constantp arg))
		     (apply ',lisp-name
			    (loop for arg in numbers collect (eval arg))))
		    ((null numbers)
		     ,identity)
		    ((null (cons-cdr numbers))
		     (cons-car numbers))
		    ((null (cons-cddr numbers))
		     `(,',c-trans-name
			   ,(cons-car numbers) ,(cons-second numbers)))
		    (t
		     `(,',tl-name (,',c-trans-name ,(cons-car numbers)
						   ,(cons-second numbers))
				  ,@(cons-cddr numbers)))))
	    (tl:declaim (tl:functional ,c-trans-name))
	    (def-c-translation ,c-trans-name (fix1 fix2)
	      ((lisp-specs :ftype ((fixnum fixnum) fixnum))
	       `(the fixnum (,',lisp-name (the fixnum ,fix1) (the fixnum ,fix2))))
	      ((trans-specs :c-type ((sint32 sint32) sint32))
	       (make-c-infix-expr fix1 ,op-string fix2)))))))

;; Here are the (implicit) definitions for LOGAND, LOGXOR, and LOGIOR:

(def-log-operators ((logior "|" 0)
		    (logxor "^" 0)
		    (logand "&" -1)))

(tl:declaim (tl:functional tl:lognot))

(def-c-translation tl:lognot (fix)
  ((lisp-specs :ftype ((fixnum) fixnum))
   `(the fixnum (lognot (the fixnum ,fix))))
  ((trans-specs :c-type ((sint32) sint32))
   (make-c-unary-expr #\~ fix)))

(def-tl-macro tl:ash (number left-shift)
  (cond ((and (constantp number) (constantp left-shift))
	 (ash (eval number) (eval left-shift)))
	((constantp left-shift)
	 (let ((shift (eval left-shift)))
	   (cond ((> shift 0)
		  `(fixnum-left-shift ,number ,shift))
		 ((< shift 0)
		  `(fixnum-right-shift ,number ,(- shift)))
		 (t
		  number))))
	(t
	 (let ((num (gensym))
	       (shift (gensym)))
	   `(tl:let ((,num ,number)
		     (,shift ,left-shift))
	      (tl:declare (fixnum ,num ,shift))
	      (tl:cond ((tl:> ,shift 0)
			(fixnum-left-shift ,num ,shift))
		       ((tl:< ,shift 0)
			(fixnum-right-shift ,num (tl:- ,shift)))
		       (t
			,num)))))))

(tl:declaim (tl:functional fixnum-left-shift fixnum-right-shift))

(def-c-translation fixnum-left-shift (number shift)
  ((lisp-specs :ftype ((fixnum fixnum) fixnum))
   `(the fixnum (ash (the fixnum ,number) (the fixnum ,shift))))
  ((trans-specs :c-type ((sint32 sint32) sint32))
   (make-c-infix-expr number "<<" shift)))

(def-c-translation fixnum-right-shift (number shift)
  ((lisp-specs :ftype ((fixnum fixnum) fixnum))
   `(the fixnum (ash ,number (the fixnum (- (the fixnum ,shift))))))
  ((trans-specs :c-type ((sint32 sint32) sint32))
   (make-c-infix-expr number ">>" shift)))

(def-tl-macro tl:logbitp (number bit)
  `(tl:/= (tl:logand ,number (fixnum-left-shift 1 ,bit)) 0))

(tl:declaim (tl:functional tl:floorf-positive floor-two-arg floor-one-arg))

(def-c-translation tl:floorf-positive (positive-fixnum positive-fixnum-divisor)
  ((lisp-specs :ftype ((fixnum fixnum) fixnum))
   `(the fixnum (values (floor (the fixnum ,positive-fixnum)
			       (the fixnum ,positive-fixnum-divisor)))))
  ((trans-specs :c-type ((sint32 sint32) sint32))
   (make-c-infix-expr positive-fixnum "/" positive-fixnum-divisor)))

(def-tl-macro tl:floor (number &optional divisor)
  (if divisor
      `(floor-two-arg ,number ,divisor)
      `(floor-one-arg ,number)))

(def-c-translation floor-two-arg (number divisor)
  ((lisp-specs :ftype ((number number) (values fixnum number)))
   `(floor ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'sint32 "fixnum_floor_first" '(sint32 sint32))
   (make-c-function-call-expr
     (make-c-name-expr "fixnum_floor_first") (list number divisor)))
  ((trans-specs :lisp-type ((fixnum fixnum) (values fixnum fixnum))
		:c-type ((sint32 sint32) obj))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'obj "fixnum_floor" '(sint32 sint32))
   (make-c-function-call-expr
     (make-c-name-expr "fixnum_floor") (list number divisor)))
  ((trans-specs :lisp-type ((double-float double-float) fixnum)
		:c-type ((double double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-cast-expr 'sint32 
		     (make-c-function-call-expr
		       (make-c-name-expr "floor")
		       (list (make-c-infix-expr number "/" divisor)))))
  ((trans-specs :lisp-type ((double-float double-float) (values fixnum number))
		:c-type ((obj obj) obj))
   ;; No Fat-and-slow warning if args are properly declared.
   ;; Assuming 2 values returned are REALLY what you want.
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_floor" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_floor") (list number divisor)))
  ((trans-specs :lisp-type ((number number) (values fixnum number))
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:floor
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_floor" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_floor") (list number divisor))))

(def-c-translation floor-one-arg (number)
  ((lisp-specs :ftype ((number) (values fixnum number)))
   `(floor ,number))
  ;; The following case just passes the value through.  I doubt it occurs, but
  ;; is here for completeness.  -jra 6/17/96
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((sint32) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   number)
  ((trans-specs :lisp-type ((double-float) fixnum)
		:c-type ((double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-cast-expr 'sint32
		     (make-c-function-call-expr
		       (make-c-name-expr "floor")
		       (list number))))
  ((trans-specs :lisp-type ((number) (values fixnum number))
		:c-type ((obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_floor_one" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_floor_one") (list number))))

(tl:declaim (tl:functional tl:modf-positive tl:mod tl:mod-float-positive tl:rem))

(def-c-translation tl:modf-positive (positive-fixnum positive-fixnum-divisor)
  ((lisp-specs :ftype ((fixnum fixnum) fixnum))
   `(the fixnum (mod (the fixnum ,positive-fixnum)
		     (the fixnum ,positive-fixnum-divisor))))
  ((trans-specs :c-type ((sint32 sint32) sint32))
   (make-c-infix-expr positive-fixnum "%" positive-fixnum-divisor)))

(def-c-translation tl:mod (number divisor)
  ((lisp-specs :ftype ((number number) number))
   `(mod ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "mod_fixnums" '(sint32 sint32))
   (make-c-function-call-expr
     (make-c-name-expr "mod_fixnums")
     (list number divisor)))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'double "mod_float" '(double double))
   (make-c-function-call-expr
     (make-c-name-expr "mod_float")
     (list number divisor)))
  ((trans-specs :lisp-type ((number number) number)
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:mod
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_mod" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_mod") (list number divisor))))

(def-c-translation tl:mod-float-positive (float float-divisor)
  ((lisp-specs :ftype ((double-float double-float) double-float))
   `(the double-float (mod ,float ,float-divisor)))
  ((trans-specs :c-type ((double double) double))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'double "fmod" '(double double))
   (make-c-function-call-expr
     (make-c-name-expr "fmod")
     (list float float-divisor))))

(def-c-translation tl:rem (number divisor)
  ((lisp-specs :ftype ((number number) number))
   `(rem ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32))
   (make-c-infix-expr number "%" divisor))
  ((trans-specs :lisp-type ((fixnum double-float) double-float)
		:c-type ((sint32 double) double))
   (make-c-function-call-expr
     (make-c-name-expr "fmod")
     (list (coerce-c-expr-result-to-type number 'sint32 'double
					 (l-expr-env function-call-l-expr))
	   divisor)))
  ((trans-specs :lisp-type ((double-float fixnum) double-float)
		:c-type ((double sint32) double))
   (make-c-function-call-expr
     (make-c-name-expr "fmod")
     (list number
	   (coerce-c-expr-result-to-type divisor 'sint32 'double
					 (l-expr-env function-call-l-expr)))))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (make-c-function-call-expr
     (make-c-name-expr "fmod")
     (list number divisor)))
  ((trans-specs :lisp-type ((number number) number)
		:c-type ((obj obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:rem
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_rem" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_rem") (list number divisor))))



(tl:declaim (tl:functional tl:oddp tl:evenp tl:abs call-sqrt))

(def-c-translation tl:oddp (fixnum)
  ((lisp-specs :ftype ((fixnum) t))
   `(ab-lisp::oddp ,fixnum))
  ((trans-specs :lisp-type ((fixnum) t)
		:test (l-expr-wants-sint32-type-p fixnum-l-expr)
		:c-type ((sint32) boolean))
   (make-c-infix-expr (make-c-infix-expr fixnum "&" 1) "!=" 0))
  ((trans-specs :lisp-type ((fixnum) t)
		:c-type ((obj) boolean))
   (make-c-infix-expr
     (make-c-infix-expr (make-c-cast-expr 'sint32 fixnum) "&" 4)
     "!=" 0)))

(def-c-translation tl:evenp (fixnum)
  ((lisp-specs :ftype ((fixnum) t))
   `(ab-lisp::evenp ,fixnum))
  ((trans-specs :lisp-type ((fixnum) t)
		:test (l-expr-wants-sint32-type-p fixnum-l-expr)
		:c-type ((sint32) boolean))
   (make-c-infix-expr (make-c-infix-expr fixnum "&" 1) "==" 0))
  ((trans-specs :lisp-type ((fixnum) t)
		:c-type ((obj) boolean))
   (make-c-infix-expr
     (make-c-infix-expr (make-c-cast-expr 'sint32 fixnum) "&" 4)
     "==" 0)))

(def-tl-macro tl:sqrt (number)
  `(call-sqrt (tl:float ,number)))

(def-c-translation call-sqrt (float)
  ((lisp-specs :ftype ((double-float) double-float))
   `(ab-lisp::sqrt ,float))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "sqrt")
     (list float))))

(def-c-translation tl:abs (number)
  ((lisp-specs :ftype ((number) number))
   `(ab-lisp::abs ,number))
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((sint32) sint32))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "abs")
       (list (coerce-c-expr-result-to-type number 'sint32 'int
					   (l-expr-env function-call-l-expr))))
     'int 'sint32 (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "fabs")
     (list number)))
  ((trans-specs :lisp-type ((number) number)
		:c-type ((obj) obj))
   (fat-and-slow-warning
     (l-expr-env function-call-l-expr) 'tl:abs
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_abs" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_abs") (list number))))

(def-tl-macro tl:log (number &optional base)
  (if base
      (if (and (or (constantp base)
		   (and (consp base)
			(eq (car base) 'tl:the)
			(constantp (third base))))
	       (= (eval base) 10))
	  `(tl:the tl:double-float (log-10 ,number))
	  `(tl:the tl:double-float (tl:/ (log-e ,number) (log-e ,base))))
      `(tl:the tl:double-float (log-e ,number))))

(tl:declaim (tl:functional log-e log-10 call-exp tl:expt))

(def-c-translation log-e (double)
  ((lisp-specs :ftype ((double-float) double-float))
   `(the double-float (log ,double)))
  ((trans-specs :c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "log")
     (list double))))

(def-c-translation log-10 (double)
  ((lisp-specs :ftype ((double-float) double-float))
   `(the double-float (log ,double 10.0)))
  ((trans-specs :c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "log10")
     (list double))))

(def-tl-macro tl:exp (power)
  `(call-exp (tl:float ,power)))

(def-c-translation  call-exp (power)
  ((lisp-specs :ftype ((double-float) double-float))
   `(float (exp ,power)))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "exp")
     (list power))))

(def-c-translation tl:expt (base power)
  ((lisp-specs :ftype ((number number) number))
   `(expt ,base ,power))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "expt_fixnum" '(sint32 sint32))
   (make-c-function-call-expr
     (make-c-name-expr "expt_fixnum") (list base power)))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double))
   (make-c-function-call-expr
     (make-c-name-expr "pow")
     (list base power)))
  ((trans-specs :c-type ((obj obj) obj))
   (fat-and-slow-warning (l-expr-env function-call-l-expr) 'tl:expt
			 (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_expt" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_expt") (list base power))))

(def-c-translation tl:scale-float (float exponent)
  ((lisp-specs :ftype ((double-float integer) double-float))
   `(scale-float ,float ,exponent))
  ((trans-specs :c-type ((double sint32) double))
   (make-c-function-call-expr
     (make-c-name-expr "ldexp")
     (list float
	   (coerce-c-expr-result-to-type
	     exponent 'sint32 'int (l-expr-env function-call-l-expr))))))

(def-tl-macro tl:ceiling (number &optional divisor)
  (if divisor
      `(ceiling-two-arg ,number ,divisor)
      `(ceiling-one-arg ,number)))

(def-c-translation ceiling-two-arg (number divisor)
  ((lisp-specs :ftype ((number number) (values fixnum number)))
   `(ceiling ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-cast-expr
     'sint32
     (make-c-function-call-expr
       (make-c-name-expr "ceil")
       (list (make-c-infix-expr
	       (coerce-c-expr-result-to-type
		 number 'sint32 'double (l-expr-env function-call-l-expr))
	       "/"
	       (coerce-c-expr-result-to-type
		 divisor 'sint32 'double (l-expr-env function-call-l-expr)))))))
  ((trans-specs :lisp-type ((double-float double-float) fixnum)
		:c-type ((double double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-cast-expr 'sint32
		     (make-c-function-call-expr
		       (make-c-name-expr "ceil")
		       (list (make-c-infix-expr number "/" divisor)))))
  ((trans-specs :lisp-type ((number number) (values fixnum number))
		:c-type ((obj obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_ceiling" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_ceiling") (list number divisor))))

(def-c-translation ceiling-one-arg (number)
  ((lisp-specs :ftype ((number) (values fixnum number)))
   `(ceiling ,number))
  ;; The following case just passes the value through.  I doubt it occurs, but
  ;; is here for completeness.  -jra 6/17/96
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((sint32) sint32)
		;; Check that only one return value is required.
		:test (tl-subtypep lisp-type 'fixnum))
   number)
  ((trans-specs :lisp-type ((double-float) fixnum)
		:c-type ((double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-cast-expr 'sint32
		     (make-c-function-call-expr
		       (make-c-name-expr "ceil")
		       (list number))))
  ((trans-specs :lisp-type ((number) (values fixnum number))
		:c-type ((obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_ceiling_one" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_ceiling_one") (list number))))

(def-tl-macro tl:ffloor (number &optional divisor)
  (if divisor
      `(ffloor-two-arg ,number ,divisor)
      `(ffloor-one-arg ,number)))

(def-c-translation ffloor-two-arg (number divisor)
  ((lisp-specs :ftype ((number number) (values double-float number)))
   `(ffloor ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) double-float)
		:c-type ((sint32 sint32) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'sint32 "fixnum_floor_first" '(sint32 sint32))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "fixnum_floor_first") (list number divisor))
     'sint32 'double (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-function-call-expr
     (make-c-name-expr "floor")
     (list (make-c-infix-expr number "/" divisor))))
  ((trans-specs :lisp-type ((number number) (values double-float number))
		:c-type ((obj obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_ffloor" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_ffloor") (list number divisor))))

(def-c-translation ffloor-one-arg (number)
  ((lisp-specs :ftype ((number) (values double-float number)))
   `(ffloor ,number))
  ((trans-specs :lisp-type ((fixnum) double-float)
		:c-type ((sint32) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (coerce-c-expr-result-to-type
     number 'sint32 'double (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-function-call-expr
     (make-c-name-expr "floor")
     (list number)))
  ((trans-specs :lisp-type ((number) (values double-float number))
		:c-type ((obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_ffloor_one" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_ffloor_one") (list number))))

(def-tl-macro tl:fceiling (number &optional divisor)
  (if divisor
      `(fceiling-two-arg ,number ,divisor)
      `(fceiling-one-arg ,number)))

(def-c-translation fceiling-two-arg (number divisor)
  ((lisp-specs :ftype ((number number) (values double-float number)))
   `(fceiling ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) double-float)
		:c-type ((sint32 sint32) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-function-call-expr
     (make-c-name-expr "ceil")
     (list (make-c-infix-expr
	     (make-c-cast-expr 'double number)
	     "/"
	     (make-c-cast-expr 'double divisor)))))
  ((trans-specs :lisp-type ((double-float double-float) double-float)
		:c-type ((double double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-function-call-expr
     (make-c-name-expr "ceil")
     (list (make-c-infix-expr number "/" divisor))))
  ((trans-specs :lisp-type ((number number) (values double-float number))
		:c-type ((obj obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_fceiling" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_fceiling") (list number divisor))))

(def-c-translation fceiling-one-arg (number)
  ((lisp-specs :ftype ((number) (values double-float number)))
   `(fceiling ,number))
  ((trans-specs :lisp-type ((fixnum) double-float)
		:c-type ((sint32) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-cast-expr 'double number))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-function-call-expr
     (make-c-name-expr "ceil")
     (list number)))
  ((trans-specs :lisp-type ((number) (values double-float number))
		:c-type ((obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_fceiling_one" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_fceiling_one") (list number))))


(tl:declaim (tl:functional truncate-one-arg truncate-two-arg
			   truncate-test-by-dividing-with-slash
			   round-one-arg round-two-arg tl:fround))

(def-tl-macro tl:truncate (number &optional divisor)
  (if divisor
      `(truncate-two-arg ,number ,divisor)
      `(truncate-one-arg ,number)))


;; The following is used by validate-fixnum-tests in tl-extension.lisp
;;   It forces a divide by "/" to produce a fixnum (in the float
;;   case it coerces the result to a sint32).  The result can be compared
;;   to the known results of truncate to determine whether or not it is safe
;;  to use "/"  and integer casting to do truncation.  NOTE: the only cases
;;  covered are 2 fixnums or 2 double-floats -- there is no generic case!

(def-c-translation truncate-test-by-dividing-with-slash (number divisor)
  ((lisp-specs :ftype ((number number) fixnum))
   `(the fixnum (values (truncate ,number ,divisor))))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32))
   (make-c-infix-expr number "/" divisor))
  ((trans-specs :lisp-type ((double-float double-float) fixnum)
		:c-type ((double double) sint32))
   (make-c-cast-expr 'sint32
		     (make-c-infix-expr number "/" divisor))))


(def-c-translation truncate-one-arg (number)
  ((lisp-specs :ftype ((number) (values fixnum number)))
   `(truncate ,number))
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((sint32) sint32)
   		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   number)
  ((trans-specs :lisp-type ((double-float) fixnum)
		:c-type ((double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (coerce-c-expr-result-to-type
     number 'double 'sint32 (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((number) fixnum)
		:c-type ((obj) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "generic_truncate_one_first" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_truncate_one_first") (list number)))
  ((trans-specs :lisp-type ((number) (values fixnum number))
		:c-type ((obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_truncate_one" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_truncate_one") (list number))))

(def-c-translation truncate-two-arg (number divisor)
  ((lisp-specs :ftype ((number number) (values fixnum number)))
   `(truncate ,number ,divisor))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-infix-expr number "/" divisor))
  ((trans-specs :lisp-type ((fixnum double-float) fixnum)
		:c-type ((sint32 double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (coerce-c-expr-result-to-type
     (make-c-infix-expr (coerce-c-expr-result-to-type
			  number 'sint32 'double
			  (l-expr-env function-call-l-expr))
			"/" divisor)
     'double 'sint32 (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((double-float fixnum) fixnum)
		:c-type ((double sint32) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (coerce-c-expr-result-to-type
     (make-c-infix-expr number "/" (coerce-c-expr-result-to-type
				     divisor 'sint32 'double
				     (l-expr-env function-call-l-expr)))
     'double 'sint32 (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((double-float double-float) fixnum)
		:c-type ((double double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (coerce-c-expr-result-to-type
     (make-c-infix-expr number "/" divisor)
     'double 'sint32 (l-expr-env function-call-l-expr)))
  ((trans-specs :lisp-type ((number number) fixnum)
		:c-type ((obj obj) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (fat-and-slow-warning-with-description
     (l-expr-env function-call-l-expr)
     "TRUNCATE can be further optimized by declaring its argument types.~%~
               This warning can be suppressed with a (fat-and-slow) declaration."
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "generic_truncate_two_first" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_truncate_two_first") (list number divisor)))
  ((trans-specs :c-type ((obj obj) obj))
   (fat-and-slow-warning-with-description
     (l-expr-env function-call-l-expr)
     "Unless both values of TRUNCATE are needed here, declare it to return a single value.~%~
               This warning can be suppressed with a (fat-and-slow) declaration."
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_truncate_two" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_truncate_two") (list number divisor))))


(def-tl-macro tl:round (number &optional divisor)
  (if divisor
      `(round-two-arg ,number ,divisor)
      (if (or (symbolp number)
	      (constantp number))
	  `(round-one-arg ,number)
	  (let ((arg (gensym)))
	    `(tl:let ((,arg ,number))
	       (round-one-arg ,arg))))))

;;; Note that this translator expands its argument more than once.

(def-c-translation round-one-arg (number)
  ((lisp-specs :ftype ((number) (values fixnum number)))
   ;; NOTE - lisp rounds exact halves to nearest EVEN integer
   ;; The following contortions are to make the lisp expansion of round conform
   ;; to the c-translated version, which rounds halves away from 0.
   `(let* ((number-value ,number)
	   (rounded-value (if (>= number-value 0)
			      (truncate (+ number-value 1/2))
			      (truncate (- number-value 1/2)))))
      (values rounded-value (- number-value rounded-value))))
  ((trans-specs :lisp-type ((fixnum) fixnum)
		:c-type ((sint32) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   number)
  ((trans-specs :lisp-type ((double-float) fixnum)
		:c-type ((double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (make-c-conditional-expr
     (make-c-infix-expr number "<" (make-c-literal-expr 0.0))
     (coerce-c-expr-result-to-type
       (make-c-infix-expr number "-" (make-c-literal-expr 0.5))
       'double 'sint32 (l-expr-env function-call-l-expr))
     (coerce-c-expr-result-to-type
       (make-c-infix-expr number "+" (make-c-literal-expr 0.5))
       'double 'sint32 (l-expr-env function-call-l-expr))))
  ((trans-specs :lisp-type ((number) fixnum)
		:c-type ((obj) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'sint32 "generic_round_one_first" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_round_one_first") (list number)))
  ((trans-specs :c-type ((obj) obj))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_round_one" '(obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_round_one") (list number))))

(def-c-translation round-two-arg (number divisor)
  ((lisp-specs :ftype ((number number) (values fixnum number)))
   ;; NOTE - lisp rounds exact halves to nearest EVEN integer
   ;; The following contortions are to make the lisp expansion of round conform
   ;; to the c-translated version, which rounds halves away from 0.
   `(let* ((number-value ,number)
	   (divisor-value ,divisor)
	   (quotient (/ number-value divisor-value))
	   (rounded-value (if (>= quotient 0)
			      (truncate (+ quotient 1/2))
			      (truncate (- quotient 1/2)))))
      (values rounded-value (- number-value (* rounded-value divisor-value)))))
  ((trans-specs :lisp-type ((fixnum fixnum) fixnum)
		:c-type ((sint32 sint32) sint32)
   		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (round-two-arg-code-emitter (coerce-c-expr-result-to-type
				 number 'sint32 'double
				 (l-expr-env function-call-l-expr))
			       (coerce-c-expr-result-to-type
				 divisor 'sint32 'double
				 (l-expr-env function-call-l-expr))
			       c-compound-statement
			       c-func function-call-l-expr))
  ((trans-specs :lisp-type ((double-float fixnum) fixnum)
		:c-type ((double sint32) sint32)
   		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (round-two-arg-code-emitter number (coerce-c-expr-result-to-type
					divisor 'sint32 'double
					(l-expr-env function-call-l-expr))
			       c-compound-statement
			       c-func function-call-l-expr))
  ((trans-specs :lisp-type ((fixnum double-float) fixnum)
		:c-type ((sint32 double) sint32)
   		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (round-two-arg-code-emitter
     (coerce-c-expr-result-to-type
       number 'sint32 'double (l-expr-env function-call-l-expr))
     divisor c-compound-statement c-func function-call-l-expr))
  ((trans-specs :lisp-type ((double-float double-float) fixnum)
		:c-type ((double double) sint32)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (round-two-arg-code-emitter
     number divisor
     c-compound-statement c-func function-call-l-expr))
  ((trans-specs :lisp-type ((number number) fixnum)
		:c-type ((obj obj) sint32)
   		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (fat-and-slow-warning-with-description
     (l-expr-env function-call-l-expr)
     "ROUND can be further optimized by declaring its argument types.~%~
               This warning can be suppressed with a (fat-and-slow) declaration."
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'sint32 "generic_round_two_first" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_round_two_first") (list number divisor)))
  ((trans-specs :c-type ((obj obj) obj))
   (fat-and-slow-warning-with-description
     (l-expr-env function-call-l-expr)
     "Unless both values of ROUND are needed here, declare it to return a single value.~%~
               This warning can be suppressed with a (fat-and-slow) declaration."
     (l-expr-pretty-form function-call-l-expr))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern") 'obj "generic_round_two" '(obj obj))
   (make-c-function-call-expr
     (make-c-name-expr "generic_round_two") (list number divisor))))

(defun round-two-arg-code-emitter
    (number-expr divisor-expr c-compound-statement c-func function-call-l-expr)
  (let* ((env (l-expr-env function-call-l-expr))
	 (temp-var (reusable-c-variable-identifier 'temp c-func 'double env)))
    (emit-expr-to-compound-statement
      (make-c-assignment-expr (make-c-name-expr temp-var)
			      "=" (make-c-infix-expr
				    number-expr "/" divisor-expr))
      c-compound-statement)
    (make-c-conditional-expr
      (make-c-infix-expr
	(make-c-name-expr temp-var)
	"<"
	(make-c-literal-expr 0.0))
      (coerce-c-expr-result-to-type
	(make-c-infix-expr (make-c-name-expr temp-var)
			   "-" (make-c-literal-expr 0.5))
	'double 'sint32 (l-expr-env function-call-l-expr))
      (coerce-c-expr-result-to-type
	(make-c-infix-expr (make-c-name-expr temp-var)
			   "+" (make-c-literal-expr 0.5))
	'double 'sint32 (l-expr-env function-call-l-expr)))))


;;; Just doing basic case of one float arg, returning a single value

(def-c-translation tl:fround (double)
  ((lisp-specs :ftype ((double-float) double-float))
   ;; NOTE - lisp rounds exact halves to nearest EVEN integer
   ;; The following contortions are to make the lisp expansion of round conform
   ;; to the c-translated version, which rounds halves away from 0.
   `(let ((number-value ,double))
      (if (>= number-value 0)
	  (ftruncate (+ number-value 0.5))
	  (ftruncate (- number-value 0.5)))))
  ((trans-specs :lisp-type ((double-float) double-float)
		:c-type ((double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (let ((temp (reusable-c-variable-identifier
		 'temp c-func 'double (l-expr-env function-call-l-expr))))
     (make-c-conditional-expr
       (make-c-infix-expr
	 (make-c-infix-expr (make-c-name-expr temp)
			    "=" double) ; eval just once
	 "<"
	 (make-c-literal-expr 0.0))
       (make-c-function-call-expr
	 (make-c-name-expr "ceil")
	 (list (make-c-infix-expr (make-c-name-expr temp)
				  "-"
				  (make-c-literal-expr 0.5))))
       (make-c-function-call-expr
	 (make-c-name-expr "floor")
	 (list (make-c-infix-expr (make-c-name-expr temp)
				  "+"
				  (make-c-literal-expr 0.5))))))))


(tl:declaim (tl:functional ftruncate-two-arg ftruncate-one-arg))

;;; We're taking shortcuts with ftruncate, in particular converting args to
;;; floats before performing the operation.  This reduces the number of cases
;;; that need to be handled, but also may cause some loss of efficiency and
;;; accuracy in the case where both args are fixnums (eg floating divide where
;;; an integer divide could have been done, and the "remainder" second value
;;; may only be approximate because of floating point roundoff errors).

(def-tl-macro tl:ftruncate (number &optional divisor)
  (if divisor
      `(ftruncate-two-arg (tl:float ,number)
			  (tl:float ,divisor))
      `(ftruncate-one-arg (tl:float ,number))))

(def-c-translation ftruncate-two-arg (number divisor)
  ((lisp-specs :ftype ((double-float double-float)
		       (values double-float double-float)))
   `(ftruncate ,number ,divisor))
  ((trans-specs :c-type ((double double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (let ((temp (reusable-c-variable-identifier
		 'temp c-func 'double (l-expr-env function-call-l-expr))))
     (make-c-conditional-expr
       (make-c-infix-expr
	 (make-c-infix-expr
	   (make-c-name-expr temp)
	   "="
	   (make-c-infix-expr number "/" divisor))
	 ">="
	 (make-c-literal-expr 0))
       (make-c-function-call-expr (make-c-name-expr "floor")
				  (list (make-c-name-expr temp)))
       (make-c-function-call-expr (make-c-name-expr "ceil")
				  (list (make-c-name-expr temp))))))
  ((trans-specs :c-type ((double double) obj))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'obj "ftruncate-two-arg-mult-value" '(double double))
   (make-c-function-call-expr
     (make-c-name-expr "ftruncate-two-arg-mult-value") (list number divisor))))

(def-c-translation ftruncate-one-arg (number)
  ((lisp-specs :ftype ((double-float)
		       (values double-float double-float)))
   `(ftruncate ,number))
  ((trans-specs :c-type ((double) double)
		;; Check that only one return value is required.
		:test (not (type-includes-values-p lisp-type)))
   (let ((temp (reusable-c-variable-identifier
		 'temp c-func 'double (l-expr-env function-call-l-expr))))
     (make-c-conditional-expr
       (make-c-infix-expr
	 (make-c-infix-expr (make-c-name-expr temp)
			    "=" number) ; eval just once
	 ">="
	 (make-c-literal-expr 0.0))
       (make-c-function-call-expr (make-c-name-expr "floor")
				  (list (make-c-name-expr temp)))
       (make-c-function-call-expr (make-c-name-expr "ceil")
				  (list (make-c-name-expr temp))))))
  ((trans-specs :c-type ((double double) obj))
   (register-needed-function-extern
     (c-func-c-file c-func)
     '("extern") 'obj "ftruncate-one-arg-mult-value" '(double))
   (make-c-function-call-expr
     (make-c-name-expr "ftruncate-one-arg-mult-value")
     (list number))))




;;; Trigonometric functions

(tl:declaim (tl:functional call-sin))

(def-tl-macro tl:sin (number)
  `(tl:the tl:double-float (call-sin (tl:float ,number))))

(def-c-translation call-sin (double)
  ((lisp-specs :ftype ((double-float) double-float))
   `(the double-float (sin ,double)))
  ((trans-specs :c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "sin")
     (list double))))

(tl:declaim (tl:functional call-cos))

(def-tl-macro tl:cos (number)
  `(tl:the tl:double-float (call-cos (tl:float ,number))))

(def-c-translation call-cos (double)
  ((lisp-specs :ftype ((double-float) double-float))
   `(the double-float (cos ,double)))
  ((trans-specs :c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "cos")
     (list double))))

(tl:declaim (tl:functional call-atan call-atan2 call-tan))

(def-tl-macro tl:atan (number1 &optional number2)
  (if number2
      `(tl:the tl:double-float (call-atan2 (tl:float ,number1)
					   (tl:float ,number2)))
      `(tl:the tl:double-float (call-atan (tl:float ,number1)))))

(def-c-translation call-atan (double)
  ((lisp-specs :ftype ((double-float) double-float))
   `(the double-float (atan ,double)))
  ((trans-specs :c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "atan")
     (list double))))

(def-c-translation call-atan2 (double1 double2)
  ((lisp-specs :ftype ((double-float double-float) double-float))
   `(the double-float (atan ,double1 ,double2)))
  ((trans-specs :c-type ((double double) double))
   (make-c-function-call-expr
     (make-c-name-expr "atan2")
     (list double1
	   double2))))

(def-tl-macro tl:tan (number)
  `(tl:the tl:double-float (call-atan (tl:float ,number))))

(def-c-translation call-tan (double)
  ((lisp-specs :ftype ((double-float) double-float))
   `(the double-float (atan ,double)))
  ((trans-specs :c-type ((double) double))
   (make-c-function-call-expr
     (make-c-name-expr "tan")
     (list double))))
