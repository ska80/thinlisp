(in-package "TL")

;;;; Module TL-UTIL

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






;;;; Miscellaneous Utilities




;;; This module implements utilities for TL.  It differs from TL-PRIM and
;;; TL-BASICS only in that this file comes after LOOP and PACKAGES in the load
;;; order, and so can use those facilities.






;;;; Pushnew




;;; Pushnew is implemented here instead of in TLT because it refers to MEMBER,
;;; which is defined in TL-BASICS.

(defmacro pushnew (&environment env item list-holding-place
				&key (test '#'eql) (key nil))
  (if (and (symbolp list-holding-place)
	   (not (eq (variable-information list-holding-place env)
		    :symbol-macro))
	   (or (symbolp item) (constantp item)))
      `(setq ,list-holding-place
	     (if (member ,item ,list-holding-place
			 :test ,test ,@(if key `(:key ,key)))
		 ,list-holding-place
		 (cons ,item ,list-holding-place)))
      (multiple-value-bind (temps vals stores store-form access-form)
	  (tl:get-setf-expansion list-holding-place env)
	(let ((item-var (gensym)))
	  `(tl:let*
	       ,(cons (list item-var item)
		      (loop for var in (append temps stores)
			    for val in (append
					 vals
					 `((if (member ,item-var ,access-form
						       :test ,test
						       ,@(if key `(:key ,key)))
					       ,access-form
					       (cons ,item-var ,access-form))))
			    collect (list var val)))
	     ,store-form)))))




;;; The function `pairlis' conforms to ANSI Common Lisp specifications, using
;;; the out given there at allows the new elements to be reversed as they are
;;; added onto the alist.

(defun pairlis (new-keys new-data &optional (alist nil))
  (declare (type list new-keys new-data alist)
	   (return-type list)
	   (consing-area either))
  (loop for key in new-keys
	for data in new-data
	do
    (setq alist (cons (cons key data) alist)))
  alist)

(defun compute-new-plist (plist property new-value)
  (declare (return-type t))
  (with-permanent-area
   (loop for cons = plist then (cddr-of-conses cons)
	 while cons do
     (when (eq (car-of-cons cons) property)
       (setf (car (cdr-of-cons cons)) new-value)
       (return nil))
     finally
     (setq plist
	   (cons property (cons new-value plist)))))
  plist)


(defmacro remf (&environment env plist property)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion plist env)
    (when (cdr stores)
      (ab-lisp::error "REMF doesn't support multiple store variables: ~s"
		      plist))
    (let ((plist-var (gensym))
	  (property-var (gensym)))
      `(let* (,@(loop for temp in temps
		      for val in vals
		      collect
		      `(,temp ,val))
		(,plist-var ,access-form)
		(,property-var ,property)
		(,(car stores) nil))
	 (loop for last-pairlist = nil then pairlist
	       for pairlist = ,plist-var then (cddr-of-conses pairlist)
	       while pairlist
	       do
	   (when (eq (car-of-cons pairlist) ,property-var)
	     (cond ((null last-pairlist)	; pair appears first in list
		    (setf ,(car stores) (cddr-of-conses pairlist))
		    ,store-form)
		   (t (setf (cddr-of-conses last-pairlist)
			    (cddr-of-conses pairlist))))
	     (return t))
	       finally
	       (return nil))))))

(defmacro remprop (symbol property)
  `(remf (symbol-plist ,symbol) ,property))

(define-setf-method getf
    (&environment env property-list property &optional default?)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion property-list env)
    ;; Prop is the incoming property-name, new-prop-value is the incoming new
    ;; property-value we are storing, and new-plist is the outgoing new
    ;; property-list to be stored back into the original location.
    (when (cdr stores)
      (ab-lisp::error "GETF doesn't support multiple store variables: ~s"
		      property-list))
    (let ((prop (gensym))
	  (new-prop-value (gensym))
	  (new-plist (car stores)))
      (values
	;; Return a list of temporary variables.
	(append temps (list prop))
	;; Return a list of forms to bind into the temporary variables.
	(append vals (list property))
	;; Return the store variable that setf should place the new
	;; property-value into.
	(list new-prop-value)
	;; Return the storing form when evaluated will modify bind the new-plist
	;; variable to the new property-list, store that plist into its calling
	;; location, and return the new property-value
	`(let ((,new-plist (compute-new-plist ,access-form ,prop ,new-prop-value)))
	   ,store-form
	   ,new-prop-value)
	;; An access form for the current property value, in case the
	;; surrounding setf needs to fetch it.
	`(getf ,access-form ,prop ,@(if default? `(,default?) nil))))))

(defmacro shiftf (&environment env &rest places-and-new-value)
  (loop with first-value-var = (gensym)
	with previous-store-vars
	with temps and vals and stores and store-form and access-form
	for place-cons = places-and-new-value then next-place-cons
	for next-place-cons = (cdr-of-cons place-cons)
	while next-place-cons
	do
    (multiple-value-setq (temps vals stores store-form access-form)
      (get-setf-expansion (car-of-cons place-cons) env))
    (when (cdr stores)
      (ab-lisp::error "SHIFTF doesn't support multiple store values : ~s"
		      (car-of-cons place-cons)))
	nconc (nconc (loop for temp in temps
			   for val in vals
			   collect `(,temp ,val))
		     (list `(,(if (eq place-cons places-and-new-value)
				  first-value-var
				  (car previous-store-vars))
			      ,access-form)))
	  into let*-bindings
	collect store-form into forms
	do
    (setq previous-store-vars stores)
	finally
	  (return `(let* (,@let*-bindings
			    (,(car previous-store-vars)
			      ,(car place-cons)))
		     ,@forms
		     ,first-value-var))))

(defmacro rotatef (&environment env &rest places)
  (loop with first-value = (gensym)
	with previous-store-vars
	with temps and vals and stores and store-form and access-form
	for place-cons on places
	do
    (multiple-value-setq (temps vals stores store-form access-form)
      (get-setf-expansion (car-of-cons place-cons) env))
    (when (cdr stores)
      (ab-lisp::error "ROTATEF doesn't support multiple store variables: ~s"
		      (car-of-cons place-cons)))
	nconc (nconc (loop for temp in temps
			   for val in vals
			   collect `(,temp ,val))
		     (list `(,(if (eq place-cons places)
				  first-value
				  (car previous-store-vars))
			      ,access-form)))
	  into let*-bindings
	collect store-form into forms
	do
    (setq previous-store-vars stores)
	finally
	  (return
	    `(let* (,@let*-bindings
		      (,(car previous-store-vars)
			,first-value))
	       ,@forms
	       nil))))

(defmacro psetf (&whole whole &environment env &rest places-and-values)
  (loop with temps and vals and stores and store-form
	for place-cons on places-and-values by #'cddr
	do
    (multiple-value-setq (temps vals stores store-form)
      (get-setf-expansion (car-of-cons place-cons) env))
    (when (null (cdr-of-cons place-cons))
      (ab-lisp::error "Odd number of forms in PSETF:  ~s" whole))
    (when (cdr stores)
      (ab-lisp::warn
	"PSETF doesn't support multiple store variables,~%~
                        (only the first value will be stored): ~s"
	(car-of-cons place-cons)))
    	nconc (nconc (loop for temp in temps
			   for val in vals
			   collect `(,temp ,val))
		     (list `(,(car stores)
			      ,(cadr-of-conses place-cons))))
	  into let*-bindings
	collect store-form into store-forms
	finally
	  (return
	    `(let* (,@let*-bindings)
	       ,@store-forms
	       nil))))




;;; The macro `feature-case' is like eval-feature, but in a case statement form.

(defmacro feature-case (&body case-clauses)
  (let* ((cond-body nil)
	 test)
    (dolist (case-clause case-clauses)
      (setq test (car case-clause))
      (cond
	((and (not (symbolp test))
	      (not (and (listp test) (or (eq (car test) 'not)
					 (eq (car test) 'and)
					 (eq (car test) 'or)))))
	 (error "Feature-case test ~s can only have symbols or and, or, and not in the case clauses."
		test))
	((eq test t) (push case-clause cond-body))
	(t (push `((eval-feature ',test) ,@(rest case-clause))
		 cond-body))))
    `(cond
       ,@(nreverse cond-body))))






;;;; Counting




(defmacro count (item sequence &key (test '#'eql) (key '#'identity))
  (let ((elt (gensym))
	(test-item (if (or (symbolp item) (constantp item))
		       item
		       (gensym))))
    `(loop ,@(if (not (eq test-item item)) `(with ,test-item = ,item))
	      for ,elt in ,sequence
	      count (funcall ,test ,item (funcall ,key ,elt)))))

(defmacro count-if (predicate list)
  (let ((pred (if (or (constantp predicate)
		      (symbolp predicate))
		  predicate
		  (gensym)))
	(elt (gensym)))
    `(loop ,@(if (not (eq pred predicate)) `(with ,pred = ,predicate))
	   for ,elt in ,list
	   count (funcall ,pred ,elt))))






;;;; Sequence Predicates




;;; The following operations implement the sequence predicates in CLtL2 p. 396.
;;; Note that we have written them as macros, not functions, and so calls to
;;; these will result is large code sizes.  Also note that if the references to
;;; the sequences cannot be proven to be particular sequence types, then you
;;; will get fat-and-slow warnings.

(defmacro min-lengths (seq &rest seqs)
  (if seqs
      `(the fixnum (min (the fixnum (length ,seq)) (min-lengths ,@seqs)))
      `(the fixnum (length ,seq))))

(defmacro list-predicate-loop (predicate loop-key &rest seq-vars)
  (let ((elt-var (gensym))
	(index (gensym)))
    `(loop ,@(when (cdr seq-vars)
	       `(for ,index from 0 below (min-lengths ,@seq-vars)))
	      for ,elt-var in ,(car seq-vars)
	      ,loop-key (funcall ,predicate
				 ,elt-var
				 ,@(loop for seq-var in (cdr seq-vars)
					 collect `(elt ,seq-var ,index))))))

(defmacro array-predicate-loop (predicate loop-key &rest seq-vars)
  (let ((index (gensym)))
    `(loop for ,index from 0 below (min-lengths ,@seq-vars)
	   ,loop-key (funcall ,predicate
			      (aref ,(car seq-vars) ,index)
			      ,@(loop for seq-var in (cdr seq-vars)
					 collect `(elt ,seq-var ,index))))))

(defmacro some (predicate sequence &rest more-seqs)
  (let ((map (loop for seq in (cons sequence more-seqs)
		   collect (list (gensym) seq))))
    `(let ,map
       (if (listp ,(caar map))
	   (list-predicate-loop
	     ,predicate thereis ,@(loop for (var) in map collect var))
	   (array-predicate-loop
	     ,predicate thereis ,@(loop for (var) in map collect var))))))

(defmacro every (predicate sequence &rest more-seqs)
  (let ((map (loop for seq in (cons sequence more-seqs)
		   collect (list (gensym) seq))))
    `(let ,map
       (if (listp ,(caar map))
	   (list-predicate-loop
	     ,predicate always ,@(loop for (var) in map collect var))
	   (array-predicate-loop
	     ,predicate always ,@(loop for (var) in map collect var))))))

(defmacro notany (predicate sequence &rest more-seqs)
  (let ((map (loop for seq in (cons sequence more-seqs)
		   collect (list (gensym) seq))))
    `(let ,map
       (if (listp ,(caar map))
	   (list-predicate-loop
	     ,predicate never ,@(loop for (var) in map collect var))
	   (array-predicate-loop
	     ,predicate never ,@(loop for (var) in map collect var))))))

(defmacro notevery (predicate seq &rest more-seqs)
  (let ((local-predicate (make-symbol "NOTEVERY-TEST"))
	(args (loop repeat (1+ (length more-seqs))
		    collect (gensym))))
    `(flet ((,local-predicate ,args
	      (declare (return-type t))
	      (not (funcall ,predicate ,@args))))
       (some #',local-predicate ,seq ,@more-seqs))))




;;;; Mapping




(defmacro mapcar (function list &rest more-lists)
  (let* ((all-lists (cons list more-lists))
	 (function-var (if (or (symbolp function)
			       (constantp function)
			       (and (consp function)
				    (eq (car function) 'function)))
			   function
			 (gensym)))
	 (vars (loop repeat (length all-lists)
		     collect (gensym))))
    `(loop ,@(if (not (eq function function-var))
		 `(with ,function-var = ,function)
	       nil)
	   ,@(loop for list-expr in all-lists
		   for var in vars
		   nconc `(for ,var in ,list-expr))
	   collect (funcall ,function ,@vars))))

(defmacro mapc (function list &rest more-lists)
  (let ((loop-name (gensym))
	(function-var (if (or (symbolp function)
			      (constantp function)
			      (and (consp function)
				   (eq (car function) 'function)))
			  function
			(gensym)))
	(first-list-var (gensym))
	(more-vars (loop repeat (1+ (length more-lists))
			 collect (gensym))))
    `(loop named ,loop-name
	   ,@(if (not (eq function-var function))
		 `(with ,function-var = ,function)
	       nil)
	   with ,first-list-var = ,list
	   ,@(loop for list-expr in (cons list more-lists)
		   for var in more-vars
		   nconc `(for ,var in ,list-expr))
	   do (funcall ,function ,@more-vars)
	   finally (return-from ,loop-name ,first-list-var))))






;;;; Fill Functions and Macros




;; Note that key args :start and :end are not handled.
;; At present there are no calls requiring these key words.

(defun fill-list (sequence elt)
  (declare (type list sequence)
	   (return-type list))
  (loop for cons on sequence
	do
    (setf (car cons) elt))
  sequence)


(defun fill-simple-vector (sequence elt)
  (declare (type simple-vector sequence)
	   (return-type simple-vector))
  (loop for index from 0 below (length sequence)
	do
    (setf (aref sequence index) elt))
  sequence)


; Do not need to define fill-string here, since it is
;   already defined in tlt-prim.lisp


(defun fill-array-unsigned-byte-8 (sequence elt)
  (declare (type (simple-array (unsigned-byte 8)) sequence)
	   (type (unsigned-byte 8) elt)
	   (return-type (simple-array (unsigned-byte 8))))
  (loop for index from 0 below (length sequence)
	do
    (setf (aref sequence index) elt))
  sequence)

(defun fill-array-unsigned-byte-16 (sequence elt)
  (declare (type (simple-array (unsigned-byte 16)) sequence)
	   (type (unsigned-byte 16) elt)
	   (return-type (simple-array (unsigned-byte 16))))
  (loop for index from 0 below (length sequence)
	do
    (setf (aref sequence index) elt))
  sequence)

(defun fill-array-signed-byte-16 (sequence elt)
  (declare (type (simple-array (signed-byte 16)) sequence)
	   (type (signed-byte 16) elt)
	   (return-type (simple-array (signed-byte 16))))
  (loop for index from 0 below (length sequence)
	do
    (setf (aref sequence index) elt))
  sequence)

(defun fill-array-double-float (sequence elt)
  (declare (type (simple-array double-float) sequence)
	   (type double-float elt)
	   (return-type (simple-array double-float)))
  (loop for index from 0 below (length sequence)
	do
    (setf (aref sequence index) elt))
  sequence)



;;; Append-type-to-name should only be called at compile-time.
;;; In particular, it should never be translated.

(defun-for-macro append-type-to-name (name type)
  (intern (string-upcase (format nil "~a~a" name type)) "TL"))


;;; Generic fill

(defun fill (sequence elt)
  (declare (return-type t))
  (macrolet ((fill-typecase (array-var elt)
               `(typecase ,array-var
		  (null
		   nil)
                  (list
		   (fill-list ,array-var ,elt))
                  ,@(loop for type-triple in tli::primitive-array-types
			  for type = (first type-triple)
			  for type-name = (third type-triple)
                          collect
                          `(,type
			      (,(append-type-to-name "FILL-" type-name)
				,array-var ,elt)))
                  (t
                   (error "Unrecognized sequence-type of ~s for fill."
                          ,array-var)))))
    (fill-typecase sequence elt)
    sequence))






;;;; Search




(defmacro search (&environment env pattern source &key (test '#'eql)
			       (start1 0) (end1 nil) (start2 0) (end2 nil))
  (unless (constantp test)
    (error "Search must be called with a constant test function, got the form ~s"
	  test))
  ;; To make the expression-result-type calls work out more often, macroexpand
  ;; the pattern and source arguments now.
  (setq pattern (macroexpand pattern env))
  (setq source (macroexpand source env))
  (let ((pattern-type (tli::expression-result-type pattern env))
	(source-type (tli::expression-result-type source env))
	(local-test (make-symbol "SEARCH-PREDICATE")))
    (cond
      ((and (subtypep pattern-type 'string)
	    (subtypep source-type 'string))
       `(search-string ,pattern ,source :test ,test
		       :start1 ,start1 :end1 ,end1
		       :start2 ,start2 :end2 ,end2))
      ((and (subtypep pattern-type 'list)
	    (subtypep source-type 'list))
       `(flet ((,local-test (x y)
		 (declare (return-type t))
		 (funcall ,test x y)))
	  (search-list ,pattern ,source #',local-test
		       :start1 ,start1 :end1 ,end1
		       :start2 ,start2 :end2 ,end2)))
      (t
       (tli::fat-and-slow-warning
	 env 'search
	 `(search ,pattern ,source :test ,test
		  :start1 ,start1 :end1 ,end1
		  :start2 ,start2 :end2 ,end2))
       `(flet ((,local-test (x y)
		 (declare (return-type t))
		 (funcall ,test x y)))
	  (generic-search ,pattern ,source #',local-test
			  ,start1 ,end1 ,start2 ,end2))))))

(defmacro search-string
    (pattern-string searched-string &key (test '#'eql)
		    (start1 0) (end1 nil) (start2 0) (end2 nil))
  (let ((pattern (gensym))
	(searched (gensym))
	(pattern-length (gensym))
	(searched-length (gensym)))
    `(let ((,pattern ,pattern-string)
	   (,searched ,searched-string))
       (declare (type string ,pattern ,searched))
       ,(if (and (or (equal test '#'eql)
		     (equal test '#'char=))
		 (null end1)
		 (null end2))
	    `(tli::search-string-1 ,pattern ,searched ,start1 ,start2)
	    `(loop with ,pattern-length fixnum = (or ,end1 (length ,pattern))
		   with ,searched-length fixnum = (or ,end2 (length ,searched))
		   for index from ,start2 to (- ,searched-length ,pattern-length)
		   do
	       (when (loop for index2 from ,start1 below ,pattern-length
			   always (funcall ,test
					   (char ,searched (+ index index2))
					   (char ,pattern index2)))
		 (return index)))))))

(defun search-list (pattern-list source-list test-func
				 &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (declare (return-type t)
	   (type fixnum start1 start2))
  (loop with pattern = (nthcdr start1 pattern-list)
	for index from start2 to (- (the fixnum (or end2 (length source-list)))
				    (the fixnum (or end1 (length pattern-list))))
	for source-cons on (nthcdr start2 source-list)
	do
    (when (loop for src-elt in source-cons
		for pat-elt in pattern
		always (funcall-simple-compiled-function
			 test-func src-elt pat-elt))
      (return-from search-list index))))

(defun generic-search (pattern source test-func start1 end1 start2 end2)
  (declare (return-type t)
	   (fat-and-slow)
	   (type fixnum start1 start2))
  (if (and (listp pattern) (listp source))
      (search-list pattern source test-func
		   :start1 start1 :end1 end1 :start2 start2 :end2 end2)
      (loop with pattern-length fixnum = (or end1 (length pattern))
	    with source-length fixnum = (or end2 (length source))
	    for index from start2 to (- source-length pattern-length)
	    do
	(when (loop for index2 from start1 below pattern-length
		    always (funcall-simple-compiled-function
			     test-func
			     (aref pattern index2)
			     (aref source (+ index index2))))
	  (return index)))))

(defun search-test (some-seq some-big-seq)
  (declare (fat-and-slow))
  (search some-seq some-big-seq))






;;;; Sorting




;;; This section implements sorting algorithms.  There is a problem in
;;; implementing sorts in TL, since most of the comparison operations have been
;;; implemented as macros (e.g. <, string<) and so the predicate argument to
;;; sort will often be a macro instead of a function.  We work around this
;;; problem by open-coding the calls to the predicate and key operations.  Note
;;; that these sorts (quick sort and sinking rock) are stable, so stable-sort
;;; "just" macroexpands to sort.

(defmacro stable-sort (sequence less-than-predicate &key (key '#'identity))
  `(sort ,sequence ,less-than-predicate :key ,key))

(defmacro sort (&environment env sequence less-than-predicate
			     &key (key '#'identity))
  (setq sequence (macroexpand sequence env))
  (cond
    ((subtypep (tli::expression-result-type sequence env) 'list)
     `(sort-list ,sequence ,less-than-predicate ,key))
    (t
     (tli::fat-and-slow-warning
       env 'sort `(sort ,sequence ,less-than-predicate :key ,key))
     (let ((local-predicate (make-symbol "SORT-PREDICATE"))
	   (seq (gensym)))
       `(flet ((,local-predicate (x y)
		 (declare (return-type t))
		 (funcall ,less-than-predicate
			  (funcall ,key x)
			  (funcall ,key y))))
	  (let ((,seq ,sequence))
	    (if (listp ,seq)
		(quick-sort-list ,seq (length ,seq) #',local-predicate)
		(generic-sort-vector ,seq #',local-predicate))))))))

(defun generic-sort-vector (vector less-than-predicate)
  (declare (return-type t)
	   (fat-and-slow))
  (loop for end-point from (- (length vector) 1) downto 1
	do
    (loop for lesser-index from 0 below end-point
	  for lesser-elt = (aref vector lesser-index)
	  for greater-index fixnum = (+ lesser-index 1)
	  for greater-elt = (aref vector greater-index)
	  do
      (when (funcall-simple-compiled-function
	      less-than-predicate greater-elt lesser-elt)
	(setf (aref vector greater-index) lesser-elt
	      (aref vector lesser-index) greater-elt))))
  vector)

(defmacro sort-list (list predicate key)
  (let ((list-var (gensym)))
    `(flet ((local-sort-predicate (x y)
	      (declare (return-type t))
	      (funcall ,predicate
		       ,(if key
			    `(funcall ,key x)
			    'x)
		       ,(if key
			    `(funcall ,key y)
			    'y))))
       (let ((,list-var ,list))
	 (quick-sort-list ,list-var (length ,list-var) #'local-sort-predicate)))))


(defun quick-sort-list (l n predicate)
  (declare (type fixnum n)
	   (return-type t))
  (case n
    ((0 1)
     nil)
    ((2)
     (let* ((first (car-of-cons l))
	    (cdr (cdr-of-cons l))
	    (second (car-of-cons cdr)))
       (unless (funcall-simple-compiled-function predicate first second)
	 (setf (car l) second)
	 (setf (car cdr) first))))
    (t
     (let* ((half-n (tli::fixnum-right-shift n 1)) ; split list and sort each half
	    (l-tail (nthcdr (- half-n 1) l))
	    (l2 (quick-sort-list (cdr-of-cons l-tail) (- n half-n) predicate))
	    (l1 (progn
		  (setf (cdr l-tail) nil)
		  (quick-sort-list l half-n predicate))))
       (declare (type fixnum half-n))
       (setq l nil)
       (loop until			; merge the sorted halves
	     (cond
	       ((not (funcall-simple-compiled-function
		       predicate (car-of-cons l2) (car-of-cons l1)))
		(setq l-tail
		      (if (null l)
			  (setq l l1)
			  (setf (cdr l-tail) l1)))
		(if (null (setq l1 (cdr l1)))
		    (setf (cdr l-tail) l2)))
	       (t
		(setq l-tail (if (null l)
				 (setq l l2)
				 (setf (cdr l-tail) l2)))
		(if (null (setq l2 (cdr l2)))
		    (setf (cdr l-tail) l1))))))))
  l)






;;;; Position




(defmacro position
    (item sequence &key (start 0) (end nil) (test '#'eql)(key '#'identity)
	  &environment env)
  (unless (constantp test)
    (error "Position needs a constant :test argument, not ~s" test))
  (unless (constantp key)
    (error "Position needs a constant :key argument, not ~s" key))
  (setq item (macroexpand item env))
  (setq sequence (macroexpand sequence env))
  (let ((item-type (tli::expression-result-type item env))
	(sequence-type (tli::expression-result-type sequence env)))
    (cond ((subtypep sequence-type 'string)
	   (if (subtypep item-type 'character)
	       `(position-string ,item ,sequence
				 :start ,start
				 :end ,end
				 :test ,test
				 :key ,key)
	       `(generic-element-position-in-string ,item ,sequence
						    :start ,start
						    :end ,end
						    :test ,test
						    :key ,key)))
	  ((subtypep sequence-type 'list)
	   `(position-list ,item ,sequence
			   :start ,start
			   :end ,end
			   :test ,test
			   :key ,key))
	  ((subtypep sequence-type 'array)
	   `(position-array ,item ,sequence
			    :start ,start
			    :end ,end
			    :test ,test
			    :key ,key))
	  (t
	   (let ((local-test (make-symbol "SEARCH-PREDICATE")))
	     (tli::fat-and-slow-warning
	       env 'position
	       `(position ,item ,sequence
			  :start ,start
			  :end ,end
			  :test ,test
			  :key ,key))
	     `(flet ((,local-test (x y)
		       (declare (return-type t))
		       (funcall ,test x (funcall ,key y))))
		(generic-position ,item ,sequence ,start ,end
				  #',local-test)))))))

(defmacro generic-element-position-in-string
    (element string &key (start 0) (end nil) (test '#'eql) (key '#'identity))
  (unless (constantp test)
    (error "~s needs a constant :test argument, not ~s"
	   'generic-element-position-in-string
	   test))
  (unless (constantp key)
    (error "~s needs a constant :key argument, not ~s"
	    'generic-element-position-in-string
	    key))
  (let ((elt (if (or (constantp element)
		     (symbolp element))
		 element
		 (gensym)))
	(str (gensym))
	(index (gensym)))
    `(let (,@(when (not (eq elt element))
	       `((,elt ,element)))
	     (,str ,string))
       (declare (type string ,str))
       (loop for ,index from ,start below ,(or end `(length ,str)) 
	     when (funcall ,test ,elt (funcall ,key
					       (char ,str ,index)))
	       return ,index))))




;;; The macro `position-string' takes a character and a string and searches for
;;; that character.  This macro supports the usual :start, :end, :test, and :key
;;; arguments of position.

(defmacro position-string
    (character string &key (start 0) (end nil) (test '#'char=) (key '#'identity))
  (unless (constantp test)
    (error "Position-string needs a constant :test argument, not ~s" test))
  (unless (constantp key)
    (error "Position-string needs a constant :key argument, not ~s" key))
  (let ((char (if (constantp character) character (gensym)))
	(str (gensym))
	(index (gensym)))
    `(let (,@(when (not (eq char character))
	       `((,char ,character)))
	     (,str ,string))
       (declare ,@(when (not (eq char character))
		    `((type character ,char)))
		(type string ,str))
       ,(if (and (or (equal test '#'char=)
		     (equal test '#'eql))
		 (equal key '#'identity)
		 (null end))
	    `(tli::position-in-string-1 ,char ,str ,start)
	    `(loop for ,index from ,start below ,(or end `(length ,str)) do
	       (when (funcall ,test ,char (funcall ,key
						   (char ,str ,index)))
		 (return ,index)))))))

(defmacro position-list
    (element list-to-search &key (start 0) (end nil) (test '#'eql) (key '#'identity))
  (unless (constantp test)
    (error "Position-list needs a constant :test argument, not ~s" test))
  (unless (constantp key)
    (error "Position-list needs a constant :key argument, not ~s" key))
  (let ((elt (if (or (constantp element)
		     (symbolp element))
		 element
		 (gensym)))
	(list-elt (gensym))
	(index (gensym)))
    `(let (,@(unless (eq elt element)
	       `((,elt ,element))))
       (loop for ,index from ,start ,@(if end `(below ,end) nil)
	     for ,list-elt in ,(if (eql start 0)
				   list-to-search
				   `(nthcdr ,index ,list-to-search))
	     when (funcall ,test ,elt (funcall ,key ,list-elt))
	       return ,index))))

(defmacro position-array
    (element array-to-search &key (start 0) (end nil) (test '#'eql) (key '#'identity))
  (unless (constantp test)
    (error "Position-array needs a constant :test argument, not ~s" test))
  (unless (constantp key)
    (error "Position-array needs a constant :key argument, not ~s" key))
  (let ((elt (if (or (constantp element)
		     (symbolp element))
		 element
		 (gensym)))
	(array-elt (gensym))
	(index (gensym)))
    `(let (,@(unless (eq elt element)
	       `((,elt ,element))))
       (loop for ,index from ,start below ,@(if end
						`(,end)
						`((length ,array-to-search)))
	     for ,array-elt = (aref ,array-to-search ,index)
	     when (funcall ,test ,elt (funcall ,key ,array-elt))
	       return ,index))))

(declaim (functional generic-position))

(defun generic-position (item sequence start end test-func)
  (declare (return-type t)
	   (fat-and-slow))
  (if (listp sequence)
      (loop for elt-cons = (nthcdr start sequence) then (cdr-of-cons elt-cons)
	    for index from start below (if end end (length sequence))
	    for elt = (car-of-cons elt-cons)
	    do
	(when (funcall-simple-compiled-function
		test-func item elt)
	  (return index)))
      (loop for index from start below (if end end (length sequence))
	    do
	(when (funcall-simple-compiled-function
		test-func
		item
		(aref sequence index))
	  (return index)))))






;;;; Find




(defmacro find (item sequence
		     &key (start 0) (end nil) (test '#'eql)
		     (key '#'identity))
  (let ((thing (gensym))
	(seq (gensym))
	(elt (gensym))
	(start-index (gensym))
	(end-index? (gensym))
	(index (gensym)))
    `(let ((,thing ,item)
	   (,seq ,sequence)
	   (,start-index ,start)
	   (,end-index? ,end))
       (declare (type fixnum ,start-index))
       (if (listp ,seq)
	   (loop for ,elt in (nthcdr ,start-index ,seq)
		 for ,index from ,start-index
		 while (or (null ,end-index?)
			   (< ,index (the fixnum ,end-index?)))
		 do
	     (when (funcall ,test ,thing (funcall ,key ,elt))
	       (return ,elt)))
	   (loop for ,index from ,start-index
			    below (or ,end-index? (length ,seq))
		 for ,elt = (funcall ,key (aref ,seq ,index))
		 do
	     (when (funcall ,test ,thing ,elt)
	       (return ,elt)))))))






;;;; Remove and its Family




;;; The remove family of functions are implemented here only in the list cases
;;; of sequences.  Others will signal an error.

(defun non-list-remove-error (sequence)
  (declare (return-type void))
  (error "REMOVE only handles lists, not this sequence: ~s"
	 sequence))

(defmacro remove (item list &key (test '#'eql) (key '#'identity))
  (let ((thing (gensym))
	(sequence (gensym))
	(elt (gensym)))
    `(let ((,thing ,item)
	   (,sequence ,list))
       (if (listp ,sequence)
	   (loop for ,elt in ,sequence
		 when (not (funcall ,test ,thing
				    (funcall ,key ,elt)))
		   collect ,elt)
	   (non-list-remove-error ,sequence)))))

(defmacro remove-if (test list &key (key '#'identity))
  (let ((sequence (gensym))
	(elt (gensym)))
    `(let ((,sequence ,list))
       (if (listp ,sequence)
	   (loop for ,elt in ,sequence
		 when (not (funcall ,test (funcall ,key ,elt)))
		   collect ,elt)
	   (non-list-remove-error ,sequence)))))






;;;; Intersection and set-difference




(defmacro intersection (list1 list2 &key (test '#'eql) (key '#'identity))
  (let ((test-set (gensym))
	(elt2 (gensym)))
    `(loop with ,test-set = ,list1
	   for ,elt2 in ,list2
	   when (member ,elt2 ,test-set :test ,test :key ,key)
	     collect ,elt2)))

(defmacro set-difference (list1 list2 &key (test '#'eql) (key '#'identity))
  (let ((elt1 (gensym))
	(test-set (gensym)))
    `(loop for ,elt1 in ,list1
	   with ,test-set = ,list2
	   unless (member ,elt1 ,test-set :test ,test :key ,key)
	     collect ,elt1)))






;;;; Simple Macro Arguments



;;; The function `simple-argument-p' determines when an argument is simple
;;; enough that it does not need to be rebound in macro arguments.

(defun-for-macro simple-argument-p (form)
  (or (symbolp form)
      (constantp form)
      (and (consp form)
	   (eq (car form) 'the)
	   (simple-argument-p (third form)))))



;;; The macro `special-variable-p' is used within our macros which.  It
;;; takes symbols and returns whether they have been globally declared special.

(defmacro special-variable-p (symbol)
  (let ((sym (gensym)))
    `(let ((,sym ,symbol))
       (or (eq (variable-information ,sym) :special)
	   #+lucid
	   (lucid::proclaimed-special-p ,sym)))))






;;;; Miscellaneous definitions which were formerly "Stubs"
;;;;    and don't naturally fit elsewhere




(declaim (functional list-length))

(defun list-length (lis)
  (declare (type list lis)
	   (return-type t))
  (loop for n from 0 by 2
	for fast = lis then (cdr (the cons fast))  ;; first cdr is done below
	for slow = lis then (cdr (the cons slow))
	do
    (when (endp fast)
      (return n))
    (when (endp (setf fast (cdr (the cons fast))))
      (return (+ n 1)))
    (when (and (eq fast slow)
	       (> n 0))
      (return nil))))



(declaim (functional tree-equal-test))

(defmacro tree-equal (a b &key (test '#'eql) &environment env)
  (unless (constantp test)
    (error "tree-equal needs a constant :test argument, not ~s" test))
  (setq a (macroexpand a env))
  (setq b (macroexpand b env))
  (let ((local-test (make-symbol "TREE-EQUAL-PREDICATE")))
    `(flet ((,local-test (x y)
	      (declare (t x y)
		       (return-type t))
	      (funcall ,test x y)))
       (tree-equal-test ,a ,b #',local-test))))

(defun tree-equal-test (a b test)
  ;; test must already be a simple-compiled-function
  (declare (return-type t))
  (if (and (consp a)(consp b))
      (and (tree-equal-test (car-of-cons a) (car-of-cons b) test)
	   (tree-equal-test (cdr-of-cons a) (cdr-of-cons b) test))
      (funcall-simple-compiled-function test a b)))

(defun copy-tree (tree)
  (declare (consing-area either)
	   (return-type t))
  (if (consp tree)
      (cons (copy-tree (car-of-cons tree))
	    (copy-tree (cdr-of-cons tree)))
      tree))

(defmacro nsubst (&whole whole new old tree &rest keyargs)
  (when keyargs
    (warn "NSUBST does not yet support :test or :key keyword args, source was ~s"
	  whole))
  `(nsubst-eql-ident ,new ,old ,tree))

(defun nsubst-eql-ident (new old tree)
  (declare (return-type t))
  (if (eql old tree)
      new
      (nsubst-eql-ident-aux new old tree)))

(defun nsubst-eql-ident-aux (new old tree)
  (when (consp tree)
    (if (eql old (car-of-cons tree))
	(setf (car tree) new)
	(nsubst-eql-ident-aux new old (car-of-cons tree)))
    (if (eql old (cdr-of-cons tree))
	(setf (cdr tree) new)
	(nsubst-eql-ident-aux new old (cdr-of-cons tree)))))


(defun copy-seq (sequence)
  (declare (consing-area either)
	   (return-type t))
  (if (listp sequence)
      (loop for item in sequence collect item)
      (typecase sequence
	(simple-vector  (make-array (length sequence) :element-type t))
	(string (make-string (length sequence)))
	((array double-float)
	 (make-array (length sequence)
		     :element-type 'double-float))
	((array (unsigned-byte 8))
	 (make-array (length sequence)
		     :element-type '(unsigned-byte 8)))
	((array (unsigned-byte 16))
	 (make-array (length sequence)
		     :element-type '(unsigned-byte 16)))
	(t (error "unrecognized sequence type for sequence ~s" sequence)))))

(defun substitute (new-item old-item sequence)
  (declare (consing-area either))
  (if (listp sequence)
      (loop for item in sequence
	    collect
	    (if (eql item old-item)
		new-item
		item))
      (let ((new-sequence (copy-seq sequence)))
	(declare (fat-and-slow))
	(loop for index from 0 below (length sequence)
	      for item = (aref sequence index)
	      do
	  (setf (aref new-sequence index)
		(if (eql item old-item)
		    new-item
		    item)))
	new-sequence)))

(declaim (functional vectorp))

(defmacro arrayp (object)
  `(vectorp ,object))

(defmacro array-rank (array)
  `(progn ,array  ;; eval arg
	  1))

(defun vectorp (object)
  (typecase object
    (simple-vector t)
    (string t)
    ((array double-float) t)
    ((array (unsigned-byte 8)) t)
    ((array (unsigned-byte 16)) t)
    (t nil)))

(declaim (functional equalp))

(defun equalp (a b)
  (declare (return-type t)
	   (consing-area either)
	   (fat-and-slow))
  (typecase a
    (fixnum (typecase b
	      (fixnum (= (the fixnum a)
			 (the fixnum b)))
	      (double-float (= (the double-float
				    (float (the fixnum a) 1.0))
			       (the double-float b)))
	      (t nil)))
    (double-float (typecase b
		    (fixnum (= (the double-float a)
			       (the double-float
				    (float (the fixnum b) 1.0))))
		    (double-float (= (the double-float a)
				     (the double-float b)))
		    (t nil)))
    (character (typecase b
		 (character (char= (the character a)
				   (the character b)))
		 (t nil)))
    (cons (and (consp b)
	       (equalp (car-of-cons a) (car-of-cons b))
	       (equalp (cdr-of-cons a) (cdr-of-cons b))))
    (string (typecase b
	      (string (string-equal (the string a)(the string b)))
	      (t nil)))
    (simple-vector
     (and (arrayp b)
	  (let ((a-length (length (the simple-vector a))))
	    (declare (type fixnum a-length))
	    (and (= a-length (length b))
		 (loop for index from 0 below a-length
		       always
		       (equalp (aref (the simple-vector a) index)
			       (aref b index)))))))
    ((array (unsigned-byte 8))
     (and (arrayp b)
	  (let ((a-length (length (the (array (unsigned-byte 8)) a))))
	    (declare (type fixnum a-length))
	    (and (= a-length (the fixnum (length b)))
		 (loop for index from 0 below a-length
		       always
		       (equalp (aref (the (array (unsigned-byte 8)) a) index)
			       (aref b index)))))))
    ((array (unsigned-byte 16))
     (and (arrayp b)
	  (let ((a-length (length (the (array (unsigned-byte 16)) a))))
	    (declare (type fixnum a-length))
	    (and (= a-length (the fixnum (length b)))
		 (loop for index from 0 below a-length
		       always
		       (equalp (aref (the (array (unsigned-byte 16)) a) index)
			       (aref b index)))))))
    ((array double-float)
     (and (arrayp b)
	  (let ((a-length (length (the (array double-float) a))))
	    (declare (type fixnum a-length))
	    (and (= a-length (the fixnum (length b)))
		 (loop for index from 0 below a-length
		       always
		       (equalp (the double-float
				    (aref (the (array double-float) a) index))
			       (aref b index)))))))
    (t (eq a b))))






;;;; Optimized-Constants




;;; Within the old Lisp to C translators, we had made our own system for storing
;;; large code constants, since none of them had done a half decent job.  TL
;;; does a half decent job, so these had become no-ops.  However, there was a
;;; macro called copy-optimized-constant which was semantically guaranteed to
;;; return a mutable copy of the constant, but not the constant itself.  We now
;;; must re-implement this.

(defmacro optimize-constant (arg)
  arg)

(defmacro recursive-copy-optimized-constant (arg)
  (if (symbolp arg)
      `(typecase ,arg
	 (null
	  ,arg)
	 (fixnum
	  ,arg)
	 (character
	  ,arg)
	 (symbol
	  ,arg)
	 (t
	  (copy-optimized-constant ,arg)))
      (let ((arg-evaled (gensym)))
	`(let ((,arg-evaled ,arg))
	   (recursive-copy-optimized-constant ,arg-evaled)))))

(defun copy-optimized-constant (arg)
  (declare (return-type t))
  (with-permanent-area
   (typecase arg
     (null
      arg)
     (fixnum
      arg)
     (character
      arg)
     (symbol
      arg)
     (cons
      (loop with new-list = (copy-list arg)
	    for copied-cons = new-list then next-cons?
	    for next-cons? = (cdr (the cons copied-cons))
	    until (atom next-cons?)
	    do
	(setf (car copied-cons)
	      (recursive-copy-optimized-constant
	       (car (the cons copied-cons))))
	    finally
	    (setf (cdr copied-cons)
		  (recursive-copy-optimized-constant
		   next-cons?))
	    (return new-list)))
     (simple-vector
      (let* ((length (length (the simple-vector arg)))
	     (sv arg)
	     (new-sv (tli::make-simple-vector length)))
	(declare (type fixnum length)
		 (type simple-vector sv new-sv))
	(loop for index from 0 below length do
	  (setf (svref new-sv index)
		(recursive-copy-optimized-constant
		 (svref sv index))))
	new-sv))
     (string
      (let* ((length (length (the string arg)))
	     (new-string (tli::make-string-1 length)))
	(replace-strings new-string arg :end2 length)
	new-string))
     (t
      (error "Unhandled type in copy-optmized-constant: ~s" arg)))))






;;;; Memory Management Utilities




(defun region-number-of-name (region-name)
  (declare (return-type fixnum))
  (case region-name
    ((:dynamic)   0)
    ((:static)    1)
    ((:temporary) 2)
    (t (error "Bad memory region name ~s.  Expected :dynamic, :static, or ~@
               :temporary."
	      region-name))))




;;; The function `realloc-region-up-to-limit' takes a keyword naming an area and
;;; a target number of bytes.  If the number of bytes already allocated for use
;;; in that region is lower than the number given, then more memory should be
;;; malloced into that region.  Note that unlike C realloc, no previously
;;; allocated memory is ever given back, new memory is incrementally added.

(defun realloc-region-up-to-limit (region-name target-size)
  (declare (type fixnum target-size)
	   (return-type void))
  (let* ((region-number (region-number-of-name region-name))
	 (size (tli::internal-region-bytes-size region-number)))
    (declare (type fixnum region-number size))
    (when (< size target-size)
      (tli::malloc-block-into-region
	region-number (the fixnum (- target-size size)) 1))))


(defun region-bytes-size (region-name)
  (declare (return-type fixnum))
  (tli::internal-region-bytes-size (region-number-of-name region-name)))

(defun region-bytes-used (region-name)
  (declare (return-type fixnum))
  (tli::internal-region-bytes-used (region-number-of-name region-name)))

(defun region-bytes-available (region-name)
  (declare (return-type fixnum))
  (tli::internal-region-bytes-available (region-number-of-name region-name)))






;;;; Hashing functions




(defun generic-sxhash (object)
  (declare (return-type fixnum))
  (typecase object
    (fixnum (sxhash (the fixnum object)))
    (double-float (sxhash (the double-float object)))
    (string (sxhash (the string object)))
    (symbol (sxhash (the symbol object)))
    (compiled-function (sxhash (the compiled-function object)))
    (character (char-code (the character object)))
    (cons (sxhash-cons-tree (the cons object)))
    ((array (unsigned-byte 16))
     (sxhash (the (array (unsigned-byte 16)) object)))
    (t 0)))				; otherwsie punt

(defmacro new-hash (old-hash new-value &optional (bit-width 16) (rotate-left 1))
  ;; Assuming that old-hash, width,  and rotate are all constants
  ;; New-value will only be evaluated once (thus order of eval doesn't matter)
  (setf bit-width (eval bit-width))
  (setf rotate-left (eval rotate-left))
  (unless (> bit-width 0)
    (warn "NEW-HASH requires bit-width to be positive, value was ~s"
	  bit-width))
  (unless (>= rotate-left 0)
    (warn "NEW-HASH requires rotate-left to be non-negative, value was ~s"
	  rotate-left))
  (unless (< rotate-left bit-width)
    (warn "NEW-HASH requires that rotate-left be less than bit-width,~
                                  ~%     values were ~s and ~s respectively"
	  rotate-left bit-width))
  `(logxor (+ (logand (ash ,old-hash ,rotate-left) ,(1- (expt 2 bit-width)))
	      (ash ,old-hash ,(- rotate-left bit-width)))
	   ,new-value))

(defun sxhash-array-16 (array-16)
  (declare (type (array (unsigned-byte 16)) array-16)
	   (return-type fixnum))
  (loop with hash fixnum = 0
	for index from 0 below (length array-16)
	do
    (setq hash (new-hash hash (aref array-16 index)))
	finally (return hash)))

(defvar *decompose-float-buffer* (make-array 4 :element-type '(unsigned-byte 16)))

(declaim (type (array (unsigned-byte 16)) *decompose-float-buffer*))

(defun sxhash-double-float (double-float)
  (declare (type double-float double-float)
	   (return-type fixnum))
  #-translator
  (ab-lisp::sxhash double-float)
  #+translator
  (progn
    (tli::decompose-float double-float
			  *decompose-float-buffer*)
    (loop with hash fixnum = 0
	  for index from 0 to 3
	  do
      (setf hash (new-hash hash (aref *decompose-float-buffer* index)
			   28 4))
	  finally
	    (return hash))))

(defun sxhash-cons-tree (cons-tree)
  (declare (type cons cons-tree)
	   (return-type fixnum))
  (loop with hash = 0
	for next-cons on cons-tree
	do
    (setf hash (new-hash hash (generic-sxhash (car-of-cons next-cons)) 28 5))
    (when (not (consp (cdr-of-cons next-cons)))
      (return (new-hash hash (generic-sxhash (cdr-of-cons next-cons)) 28 5)))
	finally
	  (return hash)))		; should never get here






;;;; BREAK




(defmacro break (&optional format-string &rest args)
  (if (eval-feature :translator)
      `(progn
	 (format t "~%BREAK: ")
	 (format t
		 ,@(if format-string `(,format-string) `(""))
		 ,@args)
	 (format t "~%Press <Return> to continue...")
	 (read-char))
      `(lisp:break
	 ,@(if format-string `(,format-string) nil)
	 ,@args)))






;;;; PPRINT




(defmacro pprint (object &optional stream)
  (if (eval-feature :translator)
      `(print ,object ,@(if stream `(,stream) nil))
      `(lisp::pprint ,object ,@(if stream `(,stream) nil))))






;;;; Assert




;;; Common Lisp assert signals a continuable error, with the continuation
;;; restart handler assigning values into the generalized variables given in the
;;; places argument.  ThinLisp does not have restartable errors, so assert
;;; simply calls error when the test fails.

(defmacro assert (test places format-string &rest format-args)
  (declare (ignore places))
  `(unless ,test
     (error ,format-string ,@format-args)))
