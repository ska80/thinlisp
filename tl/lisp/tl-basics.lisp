(in-package "TL")

;;;; Module TL-BASICS

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






;;;; Miscellaneous Basic Level Functions




;;; This module implements more basic level operations for TL.  At this point
;;; DO, DOTIMES, and DOLIST are available, but LOOP is not.






;;;; Basic Sequence Operations


(declaim (functional length))

(defun length (sequence)
  (declare (type t sequence)
	   (return-type fixnum))
  (typecase sequence
    (null 0)
    (cons
     (do* ((count 1 (1+ count))
	   (cons? (cdr-of-cons sequence) (cdr-of-cons cons?)))
	  ((null cons?)
	   count)
       (declare (type fixnum count))))
    (simple-vector
     (tli::length-trans (the simple-vector sequence)))
    (string
     (tli::length-trans (the string sequence)))
    ((array (unsigned-byte 8))
     (tli::length-trans (the (array (unsigned-byte 8)) sequence)))
    ((array (unsigned-byte 16))
     (tli::length-trans (the (array (unsigned-byte 16)) sequence)))
    ((array double-float)
     (tli::length-trans (the (array double-float) sequence)))
    (t
     (error "Non-sequence object ~a given to length." sequence)
     0)))

(declaim (functional equal))

(defun equal (a b)
  (declare (return-type t))
  (if (or (eq a b)
	  (typecase a
	    (double-float
	     (and (typep b 'double-float)
		  (= (the double-float a) (the double-float b))))
	    (string
	     (and (stringp b)
		  ;; String= is not defined until further down in this file.
		  (= (tli::string-compare a b) 0)))
	    (cons
	     (do* ((a-cons a (cdr-of-cons a-cons))
		   (b-cons b (cdr-of-cons b-cons)))
		  ((not (and (consp a-cons) (consp b-cons)))
		   (or (eq a-cons b-cons)
		       (and (not (consp a-cons))
			    (not (consp b-cons))
			    (equal a-cons b-cons))))
	       (unless (equal (car-of-cons a-cons)
			      (car-of-cons b-cons))
		 (return nil))))
	    (t nil)))
      t
      nil))


;;; This section implements some of the primitive sequence operations.

(defun copy-list (list)
  (declare (type list list)
	   (consing-area either)
	   (return-type list))
  (if (null list)
      nil
      (let* ((length (length list))
	     (new-list (make-list length)))
	(do* ((old-cons list old-cdr)
	      (old-cdr (cdr old-cons) (cdr old-cons))
	      (new-cons new-list (cdr new-cons)))
	    ((atom old-cdr)
	     (setf (car new-cons) (car old-cons))
	     (setf (cdr new-cons) old-cdr))
	  (declare (type cons old-cons new-cons))
	  (setf (car new-cons) (car old-cons)))
	new-list)))

(defun append (&rest lists)
  (declare (consing-area either))
  (cond
    ((null lists)
     nil)
    (t
     (let ((new-list nil)
	   (cons-in-new-list? nil))
       (do* ((list-holder lists next-list-holder)
	     (next-list-holder (cdr (the cons lists)) (cdr list-holder))
	     (old-list (car list-holder) (car list-holder)))
	    ((atom next-list-holder)
	     (cond (cons-in-new-list?
		    (setf (cdr (last cons-in-new-list?)) old-list))
		   (t
		    (setq new-list old-list))))
	 (declare (type cons list-holder))
	 (when old-list
	   (let ((new-copy (copy-list old-list)))
	     (cond (cons-in-new-list?
		    (setf (cdr (last cons-in-new-list?)) new-copy)
		    (setq cons-in-new-list? new-copy))
		   (t
		    (setq new-list new-copy)
		    (setq cons-in-new-list? new-copy))))))
       new-list))))

		 
	    






;;;; List Searching




(declaim (inline identity)
	 (functional identity))

(defun identity (x)
  ;; Since this function will generally be inlined, the only calls to it will be
  ;; through funcall, and that is helped by functions that set the values count.
  (declare (return-type (values t)))
  x)

(defmacro assoc (item alist &key (test '#'eql) (key '#'identity))
  (let ((test-name (if (and (consp test)
			    (or (eq (car test) 'function)
				(eq (car test) 'quote)))
		       (second test)))
	(key-name (if (and (consp key)
			    (or (eq (car key) 'function)
				(eq (car key) 'quote)))
		      (second key))))
    (cond ((and (eq key-name 'identity) (eq test-name 'eq))
	   `(assq ,item ,alist))
	  ((and (eq key-name 'identity)
		(eq test-name 'eql))
	   `(assoc-eql ,item ,alist))
	  ((and (eq key-name 'identity) (eq test-name 'equal))
	   `(assoc-equal ,item ,alist))
	  (t
	   (let ((item-var (gensym))
		 (list-var (gensym))
		 (alist-entry (gensym)))
	     `(do ((,item-var ,item)
		   (,list-var ,alist (cdr-of-cons ,list-var)))
		  ((null ,list-var)
		   nil)
		(let ((,alist-entry (car-of-cons ,list-var)))
		  (when (funcall ,test
				 (funcall ,key (car-of-cons ,alist-entry))
				 ,item-var)
		    (return ,alist-entry)))))))))

(defun assq (item alist)
  (declare (type list alist)
	   (return-type t))
  (assoc item alist :test #'eq :key #'my-identity))

(defun assoc-eql (item alist)
  (declare (type list alist)
	   (return-type t))
  (assoc item alist :test #'eql :key #'my-identity))

(defun assoc-equal (item alist)
  (declare (type list alist)
	   (return-type t))
  (assoc item alist :test #'equal :key #'my-identity))

(defmacro member (item list &key (test '#'eql) (key '#'identity))
  (let ((test-name (if (and (consp test)
			    (or (eq (car test) 'function)
				(eq (car test) 'quote)))
		       (second test)))
	(key-name (if (and (consp key)
			    (or (eq (car key) 'function)
				(eq (car key) 'quote)))
		      (second key))))
    (cond ((and (eq key-name 'identity) (eq test-name 'eq))
	   `(memq ,item ,list))
	  ((and (eq key-name 'identity)
		(eq test-name 'eql))
	   `(member-eql ,item ,list))
	  ((and (eq key-name 'identity) (eq test-name 'equal))
	   `(member-equal ,item ,list))
	  (t
	   (let ((item-var (gensym))
		 (list-var (gensym)))
	     `(do ((,item-var ,item)
		   (,list-var ,list (cdr-of-cons ,list-var)))
		  ((null ,list-var)
		   nil)
		(when (funcall ,test
			       ,item-var
			       (funcall ,key (car-of-cons ,list-var)))
		  (return ,list-var))))))))

(defun memq (item list)
  (declare (type list list)
	   (return-type t))
  (member item list :test #'eq :key #'my-identity))

(defun member-eql (item list)
  (declare (type list list)
	   (return-type t))
  (member item list :test #'eql :key #'my-identity))

(defun member-equal (item list)
  (declare (type list list)
	   (return-type t))
  (member item list :test #'equal :key #'my-identity))

(defmacro delete (item list &key (test '#'eql) (key '#'identity) (count nil))
  (unless (and (consp test) (eq (car test) 'function))
    (error "The test argument to member must be a constant of the form #'<name>, it was ~a."
	   test))
  (unless (and (consp key) (eq (car key) 'function))
    (error "The key argument to member must be a constant of the form #'<name>, it was ~a."
	   key))
  (let ((test-name (second test))
	(key-name (second key)))
    (cond
      ((and (eq key-name 'identity) (eq test-name 'eq))
       `(delq ,item ,list ,count))
      ((and (eq key-name 'identity)
	    (eq test-name 'eql))
       `(delete-eql ,item ,list ,count))
      ((and (eq key-name 'identity) (eq test-name 'equal))
       `(delete-equal ,item ,list ,count))
      (t
       (let ((item-var (gensym))
	     (list-var (gensym))
	     (max? (gensym))
	     (deleted-so-far (gensym))
	     (trailer (gensym))
	     (next-cons (gensym))
	     (current-cons (gensym)))
	 `(do* ((,item-var ,item)
		(,list-var ,list)
		(,max? ,count)
		(,deleted-so-far 0)
		(,trailer nil)
		(,next-cons nil)
		(,current-cons ,list-var))
	       ((null ,current-cons)
		,list-var)
	    (declare (type fixnum ,deleted-so-far))
	    (setq ,next-cons (cdr-of-cons ,current-cons))
	    (cond
	      ((funcall ,test (funcall ,key (car-of-cons ,current-cons))
			,item-var)
	       (if ,trailer
		   (setf (cdr ,trailer) ,next-cons)
		   (setq ,list-var ,next-cons))
	       (when ,max?
		 (setq ,deleted-so-far (+ ,deleted-so-far 1))
		 (when (>= ,deleted-so-far (the fixnum ,max?))
		   (return ,list-var))))
	      (t
	       (setq ,trailer ,current-cons)))
	    (setq ,current-cons ,next-cons)))))))

(defun delq (item list &optional count)
  (declare (type list list)
	   (return-type t))
  (delete item list :test #'eq :key #'my-identity :count count))

(defun delete-eql (item list &optional count)
  (declare (type list list)
	   (return-type t))
  (delete item list :test #'eql :key #'my-identity :count count))

(defun delete-equal (item list &optional count)
  (declare (type list list)
	   (return-type t))
  (delete item list :test #'equal :key #'my-identity :count count))

(declaim (functional nthcdr nth))

(defun nthcdr (n list)
  (declare (type fixnum n)
	   (type list list)
	   (return-type t))
  (do ((count 0 (+ count 1)))
      ((or (>= count n)
	   (null list))
       list)
    (declare (type fixnum count))
    (setq list (cdr-of-cons list))))

(defun nth (n list)
  (declare (type fixnum n)
	   (type list list)
	   (return-type t))
  (do ((count 0 (+ count 1)))
      ((null list)
       nil)
    (declare (type fixnum count))
    (cond ((>= count n)
	   (return (car-of-cons list)))
	  (t
	   (setq list (cdr-of-cons list))))))

(defsetf nth set-nth)

(defmacro set-nth (n list new-value)
  `(setf (car (nthcdr ,n ,list)) ,new-value))

(defmacro endp (list &environment env)
  (if (lisp:>= (optimize-information 'safety env) 3)
      (let ((list-var (gensym)))
	`(let ((,list-var ,list))
	   (if (null ,list-var)
	       t
	       (if (consp ,list-var)
		   nil
		   (endp-error-function ,list-var)))))
      `(null ,list)))

(defun endp-error-function (arg)
  (declare (return-type void))
  (error "ENDP requires a list argument, received ~s" arg))

(defun tli::generic-array-dimension (array)
  (declare (type t array)
	   (return-type fixnum))
  (typecase array
    (simple-vector
     (array-dimension (the simple-vector array) 0))
    (string
     (array-dimension (the string array) 0))
    ((array (unsigned-byte 8))
     (array-dimension (the (array (unsigned-byte 8)) array) 0))
    ((array (unsigned-byte 16))
     (array-dimension (the (array (unsigned-byte 16)) array) 0))
    ((array double-float)
     (array-dimension (the (array double-float) array) 0))
    (t
     (error "Non-array object ~a given to array-dimension." array)
     0)))

(defun tli::generic-fill-pointer (vector)
  (declare (type t vector)
	   (return-type fixnum))
  (typecase vector
    (string
     (fill-pointer (the string vector)))
    ((array (unsigned-byte 8))
     (fill-pointer (the (array (unsigned-byte 8)) vector)))
    ((array (unsigned-byte 16))
     (fill-pointer (the (array (unsigned-byte 16)) vector)))
    (t
     (error "~a does not have a fill-pointer." vector)
     0)))

(defun tli::generic-set-fill-pointer (vector new-fill-pointer)
  (declare (type t vector)
	   (type fixnum new-fill-pointer)
	   (return-type fixnum))
  (typecase vector
    (string
     ;; Note that the expansion of setting the fill pointer on a string also
     ;; sets a null byte into the new "end" of the string.
     (setf (fill-pointer (the string vector)) new-fill-pointer))
    ((array (unsigned-byte 8))
     (setf (fill-pointer (the (array (unsigned-byte 8)) vector))
	   new-fill-pointer))
    ((array (unsigned-byte 16))
     (setf (fill-pointer (the (array (unsigned-byte 16)) vector))
	   new-fill-pointer))
    (t
     (error "~a does not have a fill-pointer, cannot set it." vector)
     0)))

(declaim (functional array-has-fill-pointer))

(defun array-has-fill-pointer-p (vector)
  (declare (type t vector)
	  (return-type t))
  (typecase vector
    (string t)
    ((array (unsigned-byte 8)) t)
    ((array (unsigned-byte 16)) t)
    (t nil)))

       


;;; The macro `string' takes a symbol or a string and returns a string.  The
;;; CLtL2 behavior of consing a string around a single character is not
;;; supported.

(defun string-arg-invalid (x)
  (declare (return-type t))
  (error "STRING given ~s, which was not a string or symbol." x)
  nil)

(defun coerce-to-string (thing)
  (declare (type t thing)
	   (return-type string))
  (let ((coerced-string nil))
    (typecase thing
      (symbol
       (setq coerced-string (symbol-name (the symbol thing))))
      (string
       (setq coerced-string thing))
      (t
       (string-arg-invalid thing)))
    coerced-string))

(defmacro string (&environment env string-or-symbol)
  (if (tli::tl-subtypep
	(tli::expression-result-type string-or-symbol env)
	'string)
      string-or-symbol
      `(coerce-to-string ,string-or-symbol)))

(defun bounded-string-compare (a b start1 end1 start2 end2)
  (declare (type string a b)
	   (type fixnum start1 start2)
	   (return-type fixnum))
  (let ((e1 (if (null end1)
		(tli::length-trans a)
	      end1))
	(e2 (if (null end2)
		(tli::length-trans b)
	      end2)))
    (declare (type fixnum e1 e2))
    (do ()
	((or (>= start1 e1)
	     (>= start2 e2))
	 0)
      (let ((c1 (char a start1))
	    (c2 (char b start2)))
	(declare (type character c1 c2))
	(unless (char= c1 c2)
	  (return-from bounded-string-compare
	    (if (char< c1 c2) -1 1)))))))




;;; Note that these string comparison functions do not implement the
;;; keyword arguments :start1 :end1 :start2 :end2.  Note also that
;;; the "inequality" comparitors only return booleans, not the "mismatch
;;; index" specified in CommonLisp for the the "true" case.

(defmacro def-string-comparitor (op-name operation)
  `(defmacro ,op-name (string-or-symbol-a string-or-symbol-b &key
					  (start1 0) (end1 nil)
					  (start2 0) (end2 nil))
     (let ((a (gensym))
	   (b (gensym)))
       (if (and (equal start1 0) (equal start2 0)
		(null end1) (null end2))
	   `(let ((,a (string ,string-or-symbol-a))
		  (,b (string ,string-or-symbol-b)))
	      (,',operation (tli::string-compare ,a ,b) 0))
	 `(bounded-string-compare
	    (string ,string-or-symbol-a)
	    (string ,string-or-symbol-b)
	    ,start1 ,end1 ,start2 ,end2)))))

(def-string-comparitor string= =)

(def-string-comparitor string< <)

(def-string-comparitor string<= <=)

(def-string-comparitor string> >)

(def-string-comparitor string>= >=)

(def-string-comparitor string/= /=)

(declaim (functional string-equal string-lessp string-greaterp))

(defun string-equal (string-or-symbol-1 string-or-symbol-2)
  (declare (return-type t))
  (let* ((string1 (string string-or-symbol-1))
	 (string2 (string string-or-symbol-2))
	 (length1 (length string1))
	 (length2 (length string2)))
    (declare (type string string1 string2)
	     (type fixnum length1 length2))
    (and (= length1 length2)
	 (dotimes (index length1 t)
	   (unless (char-equal (char string1 index) (char string2 index))
	     (return nil))))))

(defun string-lessp (string-or-symbol-1 string-or-symbol-2)
  (declare (return-type t))
  (let* ((string1 (string string-or-symbol-1))
	 (string2 (string string-or-symbol-2))
	 (length1 (length string1))
	 (length2 (length string2))
	 (min-length (if (< length1 length2) length1 length2)))
    (declare (type string string1 string2)
	     (type fixnum length1 length2 min-length))
    (dotimes (index min-length (< length1 length2))
      (let ((char1 (char-upcase (char string1 index)))
	    (char2 (char-upcase (char string2 index))))
	(declare (type character char1 char2))
	(cond ((char< char1 char2)
	       (return t))
	      ((char> char1 char2)
	       (return nil)))))))

(defun string-greaterp (string-or-symbol-1 string-or-symbol-2)
  (declare (return-type t))
  (let* ((string1 (string string-or-symbol-1))
	 (string2 (string string-or-symbol-2))
	 (length1 (length string1))
	 (length2 (length string2))
	 (min-length (if (< length1 length2) length1 length2)))
    (declare (type string string1 string2)
	     (type fixnum length1 length2 min-length))
    (dotimes (index min-length (> length1 length2))
      (let ((char1 (char-upcase (char string1 index)))
	    (char2 (char-upcase (char string2 index))))
	(declare (type character char1 char2))
	(cond ((char> char1 char2)
	       (return t))
	      ((char< char1 char2)
	       (return nil)))))))

(defmacro nconc (&rest lists)
  (cond ((null lists)
	 nil)
	((null (cdr-of-cons lists))
	 (car lists))
	((null (cdr-of-cons (cdr-of-cons lists)))
	 `(nconc-1 ,@lists))
	(t
	 `(nconc ,@(butlast lists 2)
		 (nconc-1 ,@(nthcdr (- (length lists) 2) lists))))))

(defun nconc-1 (list1 list2)
  (declare (return-type t))
  (if (null list1)
      list2
      (do* ((cons list1 next-cdr)
	    (next-cdr (cdr-of-cons cons) (cdr-of-cons cons)))
	   ((null next-cdr)
	    (setf (cdr cons) list2)
	    list1))))

(defun nreconc (list1 list2)
  (declare (return-type t))
  (if (null list1)
      list2
      (let ((current-cdr (cdr-of-cons list1)))
	(setf (cdr list1) list2)
	(do ()
	    ((null current-cdr)
	     list1)
	  (let ((next-cdr (cdr-of-cons current-cdr)))
	    (setf (cdr current-cdr) list1)
	    (setq list1 current-cdr)
	    (setq current-cdr next-cdr))))))






;;;; Printing Primitives




(defun write-string-into-string (string-to-append output-string)
  (declare (type string string-to-append output-string)
	   (return-type string))
  (let* ((append-fill (fill-pointer string-to-append))
	 (output-fill (fill-pointer output-string))
	 (new-length (+ append-fill output-fill)))
    (declare (type fixnum append-fill output-fill new-length))
    (when (> new-length (array-dimension output-string 0))
      (error "Write-string-into-string overflow while appending ~s."
	     string-to-append))
    (replace-strings output-string string-to-append
		     :start1 output-fill :start2 0 :end2 append-fill)
    (setf (fill-pointer output-string) new-length)
    output-string))

(defun write-char-into-string (char output-string)
  (declare (type character char)
	   (type string output-string)
	   (return-type string))
  (let ((current-fill (fill-pointer output-string)))
    (declare (type fixnum current-fill))
    (unless (> (array-dimension output-string 0) current-fill)
      (error "Write-char-into-string overflow to string ~s." output-string))
    (setf (fill-pointer output-string) (+ current-fill 1))
    (setf (char output-string current-fill) char)
    output-string))






;;;; WARN




(defmacro warn (format-string &rest args)
  (if (eval-feature :translator)
      `(progn
	 (format t "~%;;; Warning: ")
	 (format t
		 ,format-string
		 ,@args))
      `(lisp:warn
	 ,format-string
	 ,@args)))






;;;; Apply






;;; The function `apply' implements all generic dispatching in TL, including
;;; dispathing through funcall.  These are all shuffled through apply since only
;;; apply implements optional arguments.

;;; Note that in non-translations, a compiler-macro will turn this into a call
;;; to the Lisp version of apply.  This is because optional argument providing
;;; in the underlying implementation can only be handled by the built in apply.

(defmacro apply (function &rest args)
  (if (eval-feature :translator)
      `(apply-1 ,function (tli::list-dynamic-extent ,@args))
      `(ab-lisp::apply ,function ,@args)))






;;;; Backquote Reader Macros




;;; TL has its own reader macros for backquote and comma, to produce
;;; translatable Lisp forms.  These get installed into the readtable during Lisp
;;; load time for this file, but are not translated.


#-translator
(lisp:set-macro-character #\` #'tli::read-backquote)

#-translator
(lisp:set-macro-character #\, #'tli::read-comma)






;;;; Environment




(defun lisp-implementation-version ()
  (declare (return-type string))
  "1.0.1")

(defun lisp-implementation-type ()
  (declare (return-type string))
  "ThinLisp")
