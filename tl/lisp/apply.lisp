(in-package "TL")

;;;; Module APPLY

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1997 Gensym Corporation.
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






;;;; Apply Implementation




;;; This file contains the function that the macro tl:apply expands into when we
;;; are translating.  It can handle optional arguments, but not keyword or rest
;;; arguments, which are only availabe in macros within TL.

;;; Note that this can only handle tl:lambda-parameters-limit (currently 20)
;;; arguments in its dispatch.  If we want to increase this number, we must also
;;; edit tl/c/tlt.h to define these function types.

(defun apply-1 (function args)
  (let* ((rest-length (length args))
	 (function-args
	   (cond ((> rest-length 1)
		  (let ((next-to-last-cons (nthcdr (- rest-length 2) args)))
		    (setf (cdr next-to-last-cons)
			  (cadr-of-conses next-to-last-cons))
		    args))
		 (t
		  (car (the cons args)))))
	 (given-arg-count (length function-args))
	 (compiled-function
	   (typecase function
	     (null
	      (error "NIL given as the function argument to apply"))
	     (symbol
	      (if (fboundp function)
		  (symbol-function function)
		  (error "Cannot apply ~s, it does not name a function."
			 function)))
	     (compiled-function
	      function)
	     (t
	      (error "~s given as the function argument to apply."
		     function))))
	 (actual-arg-count
	   (tli::compiled-function-arg-count compiled-function)))
    (declare (type fixnum rest-length given-arg-count actual-arg-count))
    (when (/= given-arg-count actual-arg-count)
      (if (and (< given-arg-count actual-arg-count)
	       (>= (+ given-arg-count
		      (tli::compiled-function-optional-arguments
			compiled-function))
		   actual-arg-count))
	  (cond
	    (function-args
	     ;; Note that this implementation depends on the extent of dynamic
	     ;; extent conses extending through the body of the containing
	     ;; function.
	     (macrolet ((optional-argument-conses ()
			  `(tli::list-dynamic-extent
			     ,@(loop repeat lambda-parameters-limit
				     collect nil))))
	       (let ((conses (optional-argument-conses)))
		 (loop for cons = conses then (cdr-of-cons cons)
		       for arg-cons = function-args then next-arg-cons?
		       for next-arg-cons? = (cdr-of-cons arg-cons)
		       while next-arg-cons? do
		   (setf (car cons)
			 (car (the cons arg-cons)))
		       finally
			 (setf (car cons) (car (the cons arg-cons)))
			 (setf (cdr cons)
			       (nthcdr
				 (- (tli::compiled-function-optional-arguments
				      compiled-function)
				    (- actual-arg-count given-arg-count))
				 (tli::compiled-function-default-arguments
				   compiled-function))))
		 (setq function-args conses))))
	    (t
	     (setq function-args
		   (tli::compiled-function-default-arguments
		     compiled-function))))
	  (error "Argument count mismatch in APPLY ~s on ~s"
		 compiled-function function-args)))
    (macrolet ((dispatch-to-apply-primitive
		   (arglist-var actual-arg-count-var compiled-function-var)
		 (let ((arg-list (gensym))
		       (arg-vars (loop repeat lambda-parameters-limit
				       collect (gensym))))
		   `(let ((,arg-list ,arglist-var)
			  ,@arg-vars)
		      (block set-vars
			,@(loop for arg in arg-vars
				nconc
				`((unless ,arg-list
				    (return-from set-vars nil))
				  (setq ,arg (car-of-cons ,arg-list))
				  (setq ,arg-list (cdr-of-cons ,arg-list)))))
		      (case (the fixnum ,actual-arg-count-var)
			,@(loop for arg-count from 0
					      to tl:lambda-parameters-limit
				collect
				`((,arg-count)
				  (if (/= (tli::compiled-function-sets-values-count
					    ,compiled-function-var)
					  0)
				      (tli::funcall-internal
					t ,compiled-function-var
					,@(loop repeat arg-count
						for arg in arg-vars
						collect arg))
				      (tli::funcall-internal
					nil ,compiled-function-var
					,@(loop repeat arg-count
						for arg in arg-vars
						collect arg)))))
			(t
			 (error "Calling APPLY on ~a with ~a args, it can only handle ~a."
				,compiled-function-var ,actual-arg-count-var
				,lambda-parameters-limit)))))))
      (tli::set-thread-closure-env 
        (tli::compiled-function-closure-environment compiled-function))
      (dispatch-to-apply-primitive
	function-args actual-arg-count compiled-function))))
