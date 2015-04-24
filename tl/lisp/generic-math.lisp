(in-package "TL")

;;;; Module GENERIC-MATH

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






;;;; Primitive Operations for TL Arithmetic




;;; This module implements the translated portions of math operations in TL.

;;; The function `math-type-error' should be called when a two-argument numeric
;;; operator is given non-numeric arguments.  It takes a string naming the
;;; operator and the two arguments.  The function `fixnum-overflow-error' should
;;; be called when it is determined that a safe fixnum operation has overflowed.

(defun math-type-error (op-string number1 number2)
  (declare (return-type void))
  (error "Non-number argument to ~a: arg1 = ~a, arg2 = ~a"
	 op-string number1 number2))

(defun math-one-arg-type-error (op-string number)
  (declare (return-type void))
  (error "Non-number argument to ~a: ~a" op-string number))

(defun fixnum-overflow-error (op-string number1 number2)
  (declare (return-type void))
  (error "Overflowed the range of integers in the following: ~a ~a ~a"
	 number1 op-string number2))





;;; The macro `def-generic-math-comparitor' is used to define functions that
;;; perform comparisons on generic numeric arguments.

(defmacro def-generic-math-comparitor (op-symbol op-name)
  (let ((function-name
	  (intern (ab-lisp::format nil "GENERIC-~a" op-name)
		  tli::*tli-package*)))
    `(progn
       (declaim (functional ,function-name))
       (defun ,function-name (arg1 arg2)
	 (declare (type number arg1 arg2)
		  (return-type t))
	 (if (fixnump arg1)
	     (if (fixnump arg2)
		 (,op-symbol (the fixnum arg1) (the fixnum arg2))
		 (,op-symbol (float (the fixnum arg1) 0.0)
			     (the double-float arg2)))
	     (if (fixnump arg2)
		 (,op-symbol (the double-float arg1)
			     (float (the fixnum arg2) 0.0))
		 (,op-symbol (the double-float arg1)
			     (the double-float arg2))))))))

(def-generic-math-comparitor < "LESS-THAN")

(def-generic-math-comparitor > "GREATER-THAN")

(def-generic-math-comparitor <= "LESS-THAN-OR-EQUAL")

(def-generic-math-comparitor >= "GREATER-THAN-OR-EQUAL")

(def-generic-math-comparitor = "NUMERIC-EQUAL")

(def-generic-math-comparitor /= "NUMERIC-NOT-EQUAL")








;;; The macro `def-generic-math-operator' is used to define the math operators
;;; that combine in the normal way with type contagion.  It defines operations
;;; named generic-<op-name>.  They are +, -, and *.  Note that these operations
;;; are never called directly, but are expanded to using def-c-translation
;;; forms.

(defmacro def-generic-math-operator (op-symbol op-name)
  (let ((function-name
	  (intern (ab-lisp::format nil "GENERIC-~A" op-name)
		  tli::*tli-package*))
	(op-string (string-downcase (symbol-name op-symbol))))
    `(progn
       (declaim (functional ,function-name)
		(conser ,function-name))
       (defun ,function-name (number1 number2)
	 (declare (type number number1 number2)
		  (return-type number))
	 (typecase number1
	   (fixnum
	    (typecase number2
	      (fixnum
	       (perform-two-arg-fixnum-op-testing-for-overflow
		 ,op-symbol ,op-string number1 number2))
	      (double-float
	       (the double-float
		    (,op-symbol (float (the fixnum number1) 0.0)
				(the double-float number2))))
	      (t
	       (math-type-error ,op-string number1 number2)
	       0)))
	   (double-float
	    (typecase number2
	      (fixnum
	       (the double-float
		    (,op-symbol (the double-float number1)
				(float (the fixnum number2) 0.0))))
	      (double-float
	       (the double-float
		    (,op-symbol (the double-float number1)
				(the double-float number2))))
	      (t
	       (math-type-error ,op-string number1 number2)
	       0)))
	   (t
	    (math-type-error ,op-string number1 number2)
	  0))))))

(defmacro perform-two-arg-fixnum-op-testing-for-overflow
    (op-symbol op-string number1 number2 &environment env)
  (let ((safety (optimize-information 'safety env)))
    (if (lisp:>= safety 3)
	`(let* ((fix1 ,number1)
		(fix2 ,number2)
		(result (,op-symbol fix1 fix2)))
	   (declare (type fixnum fix1 fix2 result))
	   (if (and (<= result most-positive-fixnum)
		    (>= result most-negative-fixnum))
	       result
	       (fixnum-overflow-error ,op-string number1 number2)))
	`(the fixnum (,op-symbol (the fixnum ,number1)
				 (the fixnum ,number2))))))

(def-generic-math-operator + plus)

(def-generic-math-operator - minus)

(def-generic-math-operator * multiply)

(declaim (functional generic-divide))

(defun generic-divide (number1 number2)
  (declare (type number number1 number2)
	   (consing-area either)
	   (return-type number))
  (typecase number1
    (fixnum
     (typecase number2
       (fixnum
	(integer-divide-error number1 number2)
	0)
       (double-float
        (the double-float (/ (float (the fixnum number1) 0.0)
			     (the double-float number2))))
       (t
	(math-type-error "/" number1 number2)
	0)))
    (double-float
     (typecase number2
       (fixnum
        (the double-float (/ (the double-float number1)
			     (float (the fixnum number2) 0.0))))
       (double-float
        (the double-float
             (/ (the double-float number1) (the double-float number2))))
       (t
	(math-type-error "/" number1 number2)
	0)))
    (t
     (math-type-error "/" number1 number2)
     0)))

(defun integer-divide-error (number1 number2)
  (declare (return-type void))
  (error "Integer /, use floor or another truncating operator: arg1 = ~a, arg2 = ~a"
	 number1 number2))

(declaim (functional generic-negate)
	 (conser generic-negate))

(defun generic-negate (number)
  (declare (return-type number))
  (typecase number
    (fixnum
     (the fixnum (- (the fixnum number))))
    (double-float
     (the double-float (- (the double-float number))))
    (t
     (math-one-arg-type-error "-" number))))




;;; The macro `ceilingf-positive' is a version of ceiling that takes two
;;; non-negative fixnums and returns a single fixnum result.  This operation can
;;; be optimized better in C translated versions because the negative range of
;;; integers don't have to be dealt with.

(defmacro ceilingf-positive (fixnum divisor-fixnum)
  (if (or (symbolp divisor-fixnum) (constantp divisor-fixnum))
      `(floorf-positive
	 (+ (the fixnum ,fixnum)
	    (the fixnum (- (the fixnum ,divisor-fixnum) 1)))
	 ,divisor-fixnum)
      (let ((numerator (gensym))
	    (divisor (gensym)))
	`(let ((,numerator ,fixnum)
	       (,divisor ,divisor-fixnum))
	   (declare (type fixnum ,numerator ,divisor))
	   (ceilingf-positive ,numerator ,divisor)))))

(declaim (functional fixnum-floor fixnum-floor-first generic-floor
		     generic-floor-one generic-ffloor generic-ffloor-one
		     generic-truncate-one generic-truncate-two
		     generic-truncate-one-first
		     generic-round-one generic-round-two
		     generic-round-one-first generic-round-two-first))

(defun fixnum-floor (fixnum divisor-fixnum)
  (declare (type fixnum fixnum divisor-fixnum)
	   (return-type (values fixnum fixnum)))
  (let ((floor-result 0)
	(remainder-result 0))
    (declare (type fixnum floor-result remainder-result))
    (if (> divisor-fixnum 0)
	(cond ((>= fixnum 0)
	       (setq floor-result 
		     (floorf-positive fixnum divisor-fixnum))
	       (setq remainder-result
		     (modf-positive fixnum divisor-fixnum)))
	      (t
	       (let ((positive-fixnum (- fixnum)))
		 (declare (type fixnum positive-fixnum))
		 (setq floor-result
		       ;; The following is the negation of ceiling.
		       (- (ceilingf-positive
			    positive-fixnum divisor-fixnum)))
		 (setq remainder-result
		       (modf-positive positive-fixnum divisor-fixnum))
		 (unless (= remainder-result 0)
		   (setq remainder-result
			 (- divisor-fixnum remainder-result))))))
	(let ((positive-divisor (- divisor-fixnum)))
	  (declare (type fixnum positive-divisor))
	  (cond ((>= fixnum 0)
		 (setq floor-result
		       (- (ceilingf-positive fixnum positive-divisor)))
		 (setq remainder-result
		       (modf-positive fixnum positive-divisor))
		 (unless (= remainder-result 0)
		   ;; Note that divisor-fixnum is negative, so the statement
		   ;; below works.
		   (setq remainder-result
			 (+ divisor-fixnum remainder-result))))
		(t
		 (let ((positive-fixnum (- fixnum)))
		   (declare (type fixnum positive-fixnum))
		   (setq floor-result
			 (floorf-positive positive-fixnum positive-divisor))
		   (setq remainder-result
			 (- (modf-positive positive-fixnum
					   positive-divisor))))))))
    (values floor-result remainder-result)))

(defun fixnum-floor-first (fixnum divisor-fixnum)
  (declare (type fixnum fixnum divisor-fixnum)
	   (return-type fixnum))
  (if (> divisor-fixnum 0)
      (cond ((>= fixnum 0)
	     (floorf-positive fixnum divisor-fixnum))
	    (t
	     (let ((positive-fixnum (- fixnum)))
	       (declare (type fixnum positive-fixnum))
	       ;; The following is the negation of ceiling.
	       (- (floorf-positive
		    (+ positive-fixnum (- divisor-fixnum 1))
		    divisor-fixnum)))))
      (cond ((>= fixnum 0)
	     (let ((positive-divisor (- divisor-fixnum)))
	       (declare (type fixnum positive-divisor))
	       (- (ceilingf-positive fixnum positive-divisor))))
	    (t
	     ;; since floor = truncate when both args have same sign
	     (truncate fixnum divisor-fixnum)))))

(defun generic-floor (number divisor)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (typecase number
    (fixnum
     (typecase divisor
       (fixnum
	(fixnum-floor (the fixnum number) (the fixnum divisor)))
       (double-float
	(let ((float-number (float (the fixnum number) 1.0)))
	  (declare (type double-float float-number))
	  (values (the fixnum (floor float-number
				     (the double-float divisor)))
		  (mod float-number (the double-float divisor)))))
       (t
	(math-type-error "floor" number divisor)
	;; The following is unreachable, but it simplifies type inference.
	(values 0 0))))
    (double-float
     (typecase divisor
       (fixnum
	(let ((float-divisor (float (the fixnum divisor) 1.0)))
	  (declare (type double-float float-divisor))
	  (values (the fixnum (floor (the double-float number) float-divisor))
		  (mod (the double-float number) float-divisor))))
       (double-float
	(values (the fixnum (floor (the double-float number)
				   (the double-float divisor)))
		(mod (the double-float number)
		     (the double-float divisor))))
       (t
	(math-type-error "floor" number divisor)
	(values 0 0))))
    (t
     (math-type-error "floor" number divisor)
     (values 0 0))))

(defun generic-floor-one (number)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (typecase number
    (fixnum
     (values number 0))
    (double-float
     (let ((floored-value (the fixnum (floor (the double-float number)))))
       (declare (type fixnum floored-value))
       (values floored-value
	       (the double-float (- (the double-float number)
				    (float floored-value 1.0))))))
    (t
     (math-one-arg-type-error "floor" number)
     (values 0 0))))

(defun generic-ffloor (number divisor)
  (declare (consing-area either)
	   (return-type (values double-float number)))
  (typecase number
    (fixnum
     (typecase divisor
       (fixnum
	(values (the double-float (ffloor (the fixnum number)
					  (the fixnum divisor)))
		(mod (the fixnum number) (the fixnum divisor))))
       (double-float
	(let ((float-number (float (the fixnum number) 1.0)))
	  (declare (type double-float float-number))
	  (values (ffloor float-number (the double-float divisor))
		  (mod float-number (the double-float divisor)))))
       (t
	(math-type-error "ffloor" number divisor)
	;; The following is unreachable, but it simplifies type inference.
	(values 0.0 0))))
    (double-float
     (typecase divisor
       (fixnum
	(let ((float-divisor (float (the fixnum divisor) 1.0)))
	  (declare (type double-float float-divisor))
	  (values (ffloor (the double-float number) float-divisor)
		  (mod (the double-float number) float-divisor))))
       (double-float
	(values (ffloor (the double-float number)
		       (the double-float divisor))
		(mod (the double-float number)
		     (the double-float divisor))))
       (t
	(math-type-error "ffloor" number divisor)
	(values 0.0 0))))
    (t
     (math-type-error "ffloor" number divisor)
     (values 0.0 0))))

(defun generic-ffloor-one (number)
  (declare (consing-area either)
	   (return-type (values double-float number)))
  (typecase number
    (fixnum
     (values (float (the fixnum number) 1.0) 0))
    (double-float
     (let ((floored-value (the double-float
			       (ffloor (the double-float number)))))
       (declare (type double-float floored-value))
       (values floored-value
	       (the double-float (- (the double-float number)
				    (the double-float floored-value))))))
    (t
     (math-one-arg-type-error "ffloor" number)
     (values 0.0 0))))

(defun generic-ceiling (number divisor)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (let ((ceiling-value 0))
    (declare (type fixnum ceiling-value))
    (typecase number
      (fixnum
       (typecase divisor
	 (fixnum
	  (setf ceiling-value
		(the fixnum (ceiling (the fixnum number)
				     (the fixnum divisor))))
	  (values
	    ceiling-value
	    (the fixnum (- (the fixnum number)
			   (* (the fixnum divisor)
			      ceiling-value)))))
	 (double-float
	  (let ((float-number (float (the fixnum number) 1.0)))
	    (declare (type double-float float-number))
	    (setf ceiling-value
		  (the fixnum (ceiling float-number
				       (the double-float divisor))))
	    (values ceiling-value
		    (the double-float
			 (- (the double-float float-number)
			    (the double-float
				 (* (the double-float divisor)
				    (the double-float
					 (float ceiling-value 1.0)))))))))
	 (t
	  (math-type-error "ceiling" number divisor)
	  ;; The following is unreachable, but it simplifies type inference.
	  (values 0 0))))
      (double-float
       (typecase divisor
	 (fixnum
	  (let ((float-divisor (float (the fixnum divisor) 1.0)))
	    (declare (type double-float float-divisor))
	    (setf ceiling-value
		  (the fixnum (ceiling (the double-float number)
				       float-divisor)))
	    (values ceiling-value
		    (the double-float
			 (- (the double-float number)
			    (the double-float
				 (* float-divisor
				    (the double-float
					 (float ceiling-value 1.0)))))))))
	 (double-float
	  (setf ceiling-value
		(the fixnum (ceiling (the double-float number)
				     (the double-float divisor))))
	  (values ceiling-value
		  (the double-float
		       (- (the double-float number)
			  (the double-float
			       (* (the double-float divisor)
				  (the double-float
				       (float ceiling-value 1.0))))))))
	 (t
	  (math-type-error "ceiling" number divisor)
	  (values 0 0))))
      (t
       (math-type-error "ceiling" number divisor)
       (values 0 0)))))

(defun generic-ceiling-one (number)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (typecase number
    (fixnum
     (values number 0))
    (double-float
     (let ((ceiled-value (the fixnum (ceiling (the double-float number)))))
       (declare (type fixnum ceiled-value))
       (values ceiled-value
	       (the double-float (- (the double-float number)
				    (float ceiled-value 1.0))))))
    (t
     (math-one-arg-type-error "ceiling" number)
     (values 0 0))))

(defun generic-fceiling (number divisor)
  (declare (consing-area either)
	   (return-type (values double-float number)))
  (let ((ceiling-value 0.0))
    (declare (type double-float ceiling-value))
    (typecase number
      (fixnum
       (typecase divisor
	 (fixnum
	  (let ((fixnum-ceiling-value
		  (ceiling (the fixnum number) (the fixnum divisor))))
	    (declare (type fixnum fixnum-ceiling-value))
	    (setf ceiling-value
		  (the double-float (float fixnum-ceiling-value 1.0)))
	    (values ceiling-value
		    (the fixnum
			 (- (the fixnum number)
			    (the fixnum
				 (* fixnum-ceiling-value
				    (the fixnum divisor))))))))
	 (double-float
	  (let ((float-number (float (the fixnum number) 1.0)))
	    (declare (type double-float float-number))
	    (setf ceiling-value
		  (fceiling float-number (the double-float divisor)))
	    (values ceiling-value
		    (the double-float
			 (- float-number
			    (the double-float
				 (* ceiling-value
				    (the double-float divisor))))))))
	 (t
	  (math-type-error "fceiling" number divisor)
	  ;; The following is unreachable, but it simplifies type inference.
	  (values 0.0 0))))
      (double-float
       (typecase divisor
	 (fixnum
	  (let ((float-divisor (float (the fixnum divisor) 1.0)))
	    (declare (type double-float float-divisor))
	    (setf ceiling-value (fceiling (the double-float number)
					  float-divisor))
	    (values ceiling-value
		    (the double-float
			 (- (the double-float number)
			    (the double-float
				 (* ceiling-value
				    float-divisor)))))))
	 (double-float
	  (setf ceiling-value (fceiling (the double-float number)
					(the double-float divisor)))
	  (values ceiling-value
		  (the double-float
		       (- (the double-float number)
			  (the double-float
			       (* ceiling-value
				  (the double-float divisor)))))))

	 (t
	  (math-type-error "fceiling" number divisor)
	  (values 0.0 0))))
      (t
       (math-type-error "fceiling" number divisor)
       (values 0.0 0)))))

(defun generic-fceiling-one (number)
  (declare (consing-area either)
	   (return-type (values double-float number)))
  (typecase number
    (fixnum
     (values (the double-float (float (the fixnum number) 1.0)) 0))
    (double-float
     (let ((ceiled-value (fceiling (the double-float number))))
       (declare (type double-float ceiled-value))
       (values ceiled-value
	       (the double-float (- (the double-float number)
				    ceiled-value)))))
    (t
     (math-one-arg-type-error "fceiling" number)
     (values 0.0 0))))

(defun generic-truncate-one-first (number)
  (declare (return-type fixnum))
  (typecase number
    (fixnum number)
    (double-float
     (truncate (the double-float number)))
    (t (math-one-arg-type-error "truncate" number)
       0)))

(defun generic-truncate-one (number)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (typecase number
    (fixnum (values (the fixnum number) 0))
    (double-float
     (let ((truncate-value (truncate (the double-float number))))
       (declare (type fixnum truncate-value))
       (values truncate-value
	       (the double-float (- (the double-float number)
				    (float truncate-value 1.0))))))
    (t (math-one-arg-type-error "truncate" number)
       (values 0 0))))

(defun generic-truncate-two-first (number divisor)
  (declare (return-type fixnum))
  (typecase number
    (fixnum
     (typecase divisor
       (fixnum
	(the fixnum (truncate (the fixnum number)
			      (the fixnum divisor))))
       (double-float
	(the fixnum (truncate (the fixnum number)
			      (the double-float divisor))))
       (t (math-type-error "truncate" number divisor)
	  ;; The following is unreachable, but it simplifies type inference.
	  0)))
    (double-float
     (typecase divisor
       (fixnum
	(the fixnum (truncate (the double-float number)
			      (the fixnum divisor))))
       (double-float
	(the fixnum (truncate (the double-float number)
			      (the double-float divisor))))
       (t (math-type-error "truncate" number divisor)
	  0)))
    (t
     (math-type-error "truncate" number divisor)
     0)))

(defun generic-truncate-two (number divisor)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (typecase number
    (fixnum
     (typecase divisor
       (fixnum
	(values (the fixnum (truncate (the fixnum number)
				      (the fixnum divisor)))
		(rem (the fixnum number) (the fixnum divisor))))
       (double-float
	(values (the fixnum (truncate (the fixnum number)
				      (the double-float divisor)))
		(rem (the fixnum number) (the double-float divisor))))
       (t
	(math-type-error "truncate" number divisor)
	;; The following is unreachable, but it simplifies type inference.
	(values 0 0))))
    (double-float
     (typecase divisor
       (fixnum
	(values (the fixnum (truncate (the double-float number)
				      (the fixnum divisor)))
		(rem (the double-float number) (the fixnum divisor))))
       (double-float
	(values (the fixnum (truncate (the double-float number)
				      (the double-float divisor)))
		(rem (the double-float number)
		     (the double-float divisor))))
       (t
	(math-type-error "truncate" number divisor)
	(values 0 0))))
    (t
     (math-type-error "truncate" number divisor)
     (values 0 0))))

(defun ftruncate-two-arg-mult-value (number divisor)
  (declare (type double-float number divisor)
	   (consing-area either)
	   (return-type (values double-float double-float)))
  (let* ((quotient (/ number divisor))
	 (integer-quotient (if (>= quotient 0.0)
			       (the double-float
				    (tli::ffloor-one-arg quotient))
			       (the double-float
				    (tli::fceiling-one-arg quotient)))))
    (declare (type double-float quotient integer-quotient))
    (values integer-quotient
	    (the double-float
		 (- number (the double-float
				(* divisor integer-quotient)))))))

(defun ftruncate-one-arg-mult-value (number)
  (declare (type double-float number)
	   (consing-area either)
	   (return-type (values double-float double-float)))
  (let ((quotient (if (>= number 0.0)
		      (the double-float
			   (tli::ffloor-one-arg
			     (the double-float number)))
		      (the double-float
			   (tli::fceiling-one-arg
			     (the double-float number))))))
    (declare (type double-float quotient))
    (values quotient
	    (the double-float
		 (- number quotient)))))

(defun generic-round-one-first (number)
  (declare (return-type fixnum))
  (typecase number
    (fixnum number)
    (double-float
     (round (the double-float number)))
    (t (math-one-arg-type-error "round" number)
       0)))

(defun generic-round-one (number)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (typecase number
    (fixnum (values (the fixnum number) 0))
    (double-float
     (let ((round-value (the fixnum	; is this needed to force single return val?
			     (round (the double-float number)))))
       (declare (type fixnum round-value))
       (values round-value
	       (the double-float (- (the double-float number)
				    (float round-value 1.0))))))
    (t (math-one-arg-type-error "round" number)
       (values 0 0))))

(defun generic-round-two-first (number divisor)
  (declare (return-type fixnum))
  ;; This typecase only checks that both args are numeric,
  ;;    and gives an error if not
  (typecase number
    (fixnum)
    (double-float)
    (t (math-type-error "round" number divisor)))
  (typecase divisor
    (fixnum)
    (double-float)
    (t (math-type-error "round" number divisor)))
  ;; Note that since all two argument rounding cases, including the fixnum case,
  ;; perform a floating point divide, it is an efficient implementation to go
  ;; ahead and coerce both arguments to floats and only invoke round in the two
  ;; argument double-float case.
  (the fixnum (round (the double-float (float number 0.0))
		     (the double-float (float divisor 0.0)))))

(defun generic-round-two (number divisor)
  (declare (consing-area either)
	   (return-type (values fixnum number)))
  (let ((round-value 0))
    (declare (type fixnum round-value))
    (typecase number
      (fixnum
       (typecase divisor
	 (fixnum
	  (setf round-value (the fixnum (round (the fixnum number)
					       (the fixnum divisor))))
	  (values round-value
		  (- (the fixnum number)
		     (the fixnum (* (the fixnum round-value)
				    (the fixnum divisor))))))
	 (double-float
	  (setf round-value (the fixnum (round (the fixnum number)
					       (the double-float divisor))))
	  (values round-value
		  (- (float (the fixnum number) 1.0)
		     (the double-float (* (float (the fixnum round-value) 1.0)
					  (the double-float divisor))))))
	 (t (math-type-error "round" number divisor)
	    ;; The following is unreachable, but it simplifies type inference.
	    (values 0 0))))
      (double-float
       (typecase divisor
	 (fixnum
	  (setf round-value (the fixnum (round (the double-float number)
					       (the fixnum divisor))))
	  (values round-value
		  (- (the double-float number)
		     (float (the fixnum (* (the fixnum round-value)
					   (the fixnum divisor)))
			    1.0))))
	 (double-float
	  (setf round-value (the fixnum (round (the double-float number)
					       (the double-float divisor))))
	  (values round-value
		  (- (the double-float number)
		     (the double-float (* (float (the fixnum round-value) 1.0)
					  (the double-float divisor))))))
	 (t (math-type-error "round" number divisor)
	    (values 0 0))))
      (t (math-type-error "round" number divisor)
	 (values 0 0)))))


(declaim (functional mod-fixnums mod-float
		     generic-mod generic-rem))

(defun mod-fixnums (fixnum divisor-fixnum)
  (declare (type fixnum fixnum divisor-fixnum)
	   (return-type fixnum))
  (let ((remainder-result 0))
    (declare (type fixnum remainder-result))
    (if (> divisor-fixnum 0)
	(cond ((>= fixnum 0)
	       (setq remainder-result
		  (modf-positive fixnum divisor-fixnum)))
	      (t
	       (let ((positive-fixnum (- fixnum)))
		 (declare (type fixnum positive-fixnum))
		 (setq remainder-result
		       (modf-positive positive-fixnum divisor-fixnum))
		 (unless (= remainder-result 0)
		   (setq remainder-result
			 (- divisor-fixnum remainder-result))))))
	(cond ((>= fixnum 0)
	       (let ((positive-divisor (- divisor-fixnum)))
		 (declare (type fixnum positive-divisor))
		 (setq remainder-result
		       (modf-positive fixnum positive-divisor))
		 (unless (= remainder-result 0)
		   ;; Note that divisor-fixnum is negative, so the statement
		   ;; below works.
		   (setq remainder-result
			 (+ divisor-fixnum remainder-result)))))
	      (t
	       ;; since rem is same as mod when both signs are same
	       (setq remainder-result (rem fixnum divisor-fixnum)))))
    remainder-result))

(defun mod-float (float divisor-float)
  (declare (type double-float float divisor-float)
	   (return-type double-float))
  (let ((remainder-result 0.0))
    (declare (type double-float remainder-result))
    (if (> divisor-float 0.0)
	(cond ((>= float 0.0)
	       (setq remainder-result
		     (mod-float-positive float divisor-float)))
	      (t
	       (setq remainder-result
		     (mod-float-positive (- float) divisor-float))
	       (unless (= remainder-result 0.0)
		 (setq remainder-result
		       (- divisor-float remainder-result)))))
	(cond ((>= float 0.0)
	       (setq remainder-result
		     (mod-float-positive float (- divisor-float)))
	       (unless (= remainder-result 0.0)
		 ;; Note that divisor-float is negative, so the statement
		 ;; below works.
		 (setq remainder-result
		       (+ divisor-float remainder-result))))
	      (t
	       (setq remainder-result
		     ;; since rem =  mod when both args have same sign
		     (rem float divisor-float)))))
    remainder-result))

(defun generic-mod (number divisor)
  (declare (consing-area either)
	   (return-type number))
  (typecase number
    (fixnum
     (typecase divisor
       (fixnum
	(the fixnum (mod (the fixnum number) (the fixnum divisor))))
       (double-float
	(the double-float (mod (float (the fixnum number) 1.0)
			       (the double-float divisor))))
       (t
	(math-type-error "mod" number divisor)
	0)))
    (double-float
     (typecase divisor
       (fixnum
	(the double-float (mod (the double-float number)
			       (float (the fixnum divisor) 1.0))))
       (double-float
	(the double-float (mod (the double-float number)
			       (the double-float divisor))))
       (t
	(math-type-error "mod" number divisor)
	0)))
    (t
     (math-type-error "mod" number divisor)
     0)))

(defun generic-rem (number divisor)
  (declare (consing-area either)
	   (return-type number))
    (typecase number
    (fixnum
     (typecase divisor
       (fixnum
	(the fixnum (rem (the fixnum number) (the fixnum divisor))))
       (double-float
	(the double-float (rem (float (the fixnum number) 1.0)
			       (the double-float divisor))))
       (t
	(math-type-error "rem" number divisor)
	0)))
    (double-float
     (typecase divisor
       (fixnum
	(the double-float (rem (the double-float number)
			       (float (the fixnum divisor) 1.0))))
       (double-float
	(the double-float (rem (the double-float number)
			       (the double-float divisor))))
       (t
	(math-type-error "rem" number divisor)
	0)))
    (t
     (math-type-error "rem" number divisor)
     0)))


(declaim (functional generic-abs integer-length logcount))

(defun generic-abs (number)
  (declare (consing-area either)
	   (return-type number))
  (typecase number
    (fixnum
     (the fixnum (abs (the fixnum number))))
    (double-float
     (the double-float (abs (the double-float number))))
    (t
     (math-one-arg-type-error "abs" number)
     number)))

(defun integer-length (fixnum)
  (declare (type fixnum fixnum)
	   (return-type fixnum))
  (when (< fixnum 0)
    (setq fixnum (lognot fixnum)))
  (let ((count 0))
    (declare (type fixnum count))
    (cond ((>= fixnum (expt 2 24))
	   (incf count 24)
	   (setq fixnum (ash fixnum -24)))
	  ((>= fixnum (expt 2 16))
	   (incf count 16)
	   (setq fixnum (ash fixnum -16)))
	  ((>= fixnum (expt 2 8))
	   (incf count 8)
	   (setq fixnum (ash fixnum -8))))
    (loop while (> fixnum 0)
	  do
      (setq fixnum (ash fixnum -1))
      (incf count))
    count))

(defun logcount (fixnum)
  (declare (type fixnum fixnum)
	   (return-type fixnum))
  (when (< fixnum 0)
    (setq fixnum (lognot fixnum)))
  (loop for count from 0
	while (> fixnum 0)
	do
    ;; The following line uses the trick that a bitwise AND of an integer and
    ;; the negation of itself will return the least significant bit turned on in
    ;; that integer.  -jallard, 4/29/97
    (setq fixnum (- fixnum (logand fixnum (- fixnum))))
	finally (return count)))

(defmacro logtest (integer1 integer2)
  `(not (zerop (the fixnum (logand ,integer1 ,integer2)))))

(defmacro logandc2 (integer1 integer2)
  `(logand ,integer1 (lognot ,integer2)))

(defmacro logandc1 (integer1 integer2)
  `(logand (lognot ,integer1) ,integer2))




;;;  The function `isqrt' Returns the root of the nearest integer less than or
;;;  equal to n which is a perfect square.  This code was shamelessly cribbed
;;;  from CMU Lisp, and then changed to actually make it inline all of the
;;;  arithmetic.  -jallard, 4/29/97

(declaim (ftype (function (fixnum) fixnum) isqrt)
	 (functional isqrt))

(defun isqrt (n)
  (declare (type fixnum n)
	   (return-type fixnum))
  ;; theoretically (> n 7) ,i.e., n-len-quarter > 0
  (if (<= n 24)
      (cond ((> n 15) 4)
	    ((> n  8) 3)
	    ((> n  3) 2)
	    ((> n  0) 1)
	    (t 0))
      (let* ((n-len-quarter (tli::fixnum-right-shift (integer-length n) 2))
	     (n-half (tli::fixnum-right-shift
		       n (tli::fixnum-left-shift n-len-quarter 1)))
	     (n-half-isqrt (isqrt n-half))
	     (init-value
	       (tli::fixnum-left-shift (1+ n-half-isqrt) n-len-quarter)))
	(declare (type fixnum n-len-quarter n-half n-half-isqrt init-value))
	(loop do
	  (let ((iterated-value
		  (tli::fixnum-right-shift
		    (+ init-value (floorf-positive n init-value)) 1)))
	    (declare (type fixnum iterated-value))
	    (unless (< iterated-value init-value)
	      (return init-value))
	    (setq init-value iterated-value))))))

(defun expt-fixnum (base power)
  (declare (type fixnum base power)
	   (return-type fixnum))
  (loop with accumulator fixnum = 1
	repeat power
	do
    (setq accumulator (* accumulator base))
	finally (return accumulator)))

(defun generic-expt (base power)
  (declare (type number base power)
	   (consing-area either)
	   (return-type number))
  (if (fixnump base)
      (if (fixnump power)
	  (expt-fixnum (the fixnum base) (the fixnum power))
	  (the double-float
	       (expt (float (the fixnum base) 1.0)
		     (the double-float power))))
      (the double-float
	   (expt (the double-float base)
		 (the double-float
		      (if (fixnump power)
			  (float power 1.0)
			  power))))))
