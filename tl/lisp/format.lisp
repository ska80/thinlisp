(in-package "TL")

;;;; Module FORMAT

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






;;;; Format and Supporting Stream Operations




;;; This module implements format and the more basic character, string, and
;;; stream operations needed to support it.






;;;; Characters




;;; The following character operations may have assumptions that our Lisp
;;; environment runs within an ASCII character encoding.

(defmacro digit-char-p (character &optional (radix 10))
  (if (and (constantp radix) (<= (eval radix) 10))
      (let ((weight (gensym)))
	`(let ((,weight (- (char-code ,character) (char-code #\0))))
	   (declare (type fixnum ,weight))
	   (if (and (>= ,weight 0)
		    (< ,weight ,radix))
	       ,weight
	       nil)))
      (let ((char-code (gensym))
	    (rad (gensym)))
	`(let ((,char-code (char-code ,character))
	       (,rad ,radix))
	   (declare (type fixnum ,char-code ,rad))
	   (cond ((<= ,rad 10)
		  (setq ,char-code (- ,char-code (char-code #\0)))
		  (if (and (>= ,char-code 0)
			   (< ,char-code ,rad))
		      ,char-code
		      nil))
		 ((and (>= ,char-code (char-code #\0))
		       (<= ,char-code (char-code #\9)))
		  (- ,char-code (char-code #\0)))
		 ((and (>= ,char-code (char-code #\A))
		       (<= ,char-code (char-code #\Z)))
		  (setq ,char-code
			(+ (the fixnum (- ,char-code (char-code #\A))) 10))
		  (if (< ,char-code ,rad)
		      ,char-code
		      nil))
		 ((and (>= ,char-code (char-code #\a))
		       (<= ,char-code (char-code #\z)))
		  (setq ,char-code
			(+ (the fixnum (- ,char-code (char-code #\a))) 10))
		  (if (< ,char-code ,rad)
		      ,char-code
		      nil)))))))

(defmacro digit-char (weight &optional (radix 10))
  (let ((char-number (gensym))
	(radix-value (gensym)))
    (if (and (constantp radix)
	     (<= (eval radix) 10))
	(if (or (constantp weight) (symbolp weight))
	    `(if (< (the fixnum ,weight) ,radix)
		 (code-char (+ (the fixnum ,weight) (char-code #\0)))
		 nil)
	    `(let ((,char-number ,weight))
	       (declare (type fixnum ,char-number))
	       (if (< ,char-number ,radix)
		   (code-char (+ ,char-number (char-code #\0)))
		   nil)))
	`(let ((,char-number ,weight)
	       (,radix-value ,radix))
	   (declare (type fixnum ,char-number ,radix-value))
	   (if (< ,char-number ,radix-value)
	       (code-char
		 (if (< ,char-number 10)
		     (+ ,char-number (char-code #\0))
		     (+ ,char-number (- (char-code #\A) 10))))
	       nil)))))

(defmacro digit-char-no-nils (weight)
  (let ((fixnum (gensym)))
    `(let ((,fixnum ,weight))
       (declare (type fixnum ,fixnum))
       (code-char (+ (if (< ,fixnum 10)
			 (char-code #\0)
			 (- (char-code #\A) 10))
		     ,fixnum)))))

(defmacro def-multi-arg-char-comparitor (op two-arg-op)
  `(defmacro ,op (char &rest chars)
     (cond ((null chars)
	    ;; Default of true, the given char arg will suffice.
	    char)
	   ((null (cdr chars))
	    `(,',two-arg-op ,char ,@chars))
	   (t
	    (let ((bindings (lisp:mapcar #'(lambda (char) (list (gensym) char))
					 (cons char chars))))
	      `(let ,bindings
		 (declare (type character ,@(lisp:mapcar #'lisp:car bindings)))
		 (and ,@(do ((tests nil)
			     (bind-cons bindings (cdr bind-cons)))
			    ((null (cdr bind-cons))
			     (reverse tests))
			  (push `(,',two-arg-op ,(caar bind-cons)
						,(caadr bind-cons))
				tests)))))))))

(def-multi-arg-char-comparitor char= tli::two-arg-char=)

(def-multi-arg-char-comparitor char/= tli::two-arg-char/=)

(def-multi-arg-char-comparitor char< tli::two-arg-char<)

(def-multi-arg-char-comparitor char<= tli::two-arg-char<=)

(def-multi-arg-char-comparitor char> tli::two-arg-char>)

(def-multi-arg-char-comparitor char>= tli::two-arg-char>=)

(defmacro lower-case-p (character)
  (let ((char (gensym)))
    `(let ((,char ,character))
       (declare (type character ,char))
       (and (char<= #\a ,char)
	    (char<= ,char #\z)))))

(defmacro upper-case-p (character)
  (let ((char (gensym)))
    `(let ((,char ,character))
       (declare (type character ,char))
       (and (char<= #\A ,char)
	    (char<= ,char #\Z)))))

(defmacro alpha-char-p (character)
  (let ((char (gensym)))
    `(let ((,char ,character))
       (declare (type character ,char))
       (or (and (char<= #\a ,char)
		(char<= ,char #\z))
	   (and (char<= #\A ,char)
		(char<= ,char #\Z))))))

(defmacro both-case-p (character)
  `(alpha-char-p ,character))

(defmacro alphanumericp (character)
  (let ((char (gensym)))
    `(let ((,char ,character))
       (or (digit-char-p ,char)
	   (alpha-char-p ,char)))))

(defmacro char-upcase (character)
  (let ((char (gensym)))
    `(let ((,char ,character))
       (declare (type character ,char))
       (if (lower-case-p ,char)
	   ;; ASCII assumption here!
	   (code-char (- (char-code ,char)
			 (- (char-code #\a) (char-code #\A))))
	   ,char))))

(defmacro char-downcase (character)
  (let ((char (gensym)))
    `(let ((,char ,character))
       (declare (type character ,char))
       (if (upper-case-p ,char)
	   ;; ASCII assumption here!
	   (code-char (+ (char-code ,char)
			 (- (char-code #\a) (char-code #\A))))
	   ,char))))

(defmacro def-case-insensitive-char-comparitor (new-op base-op)
  `(defmacro ,new-op (char1 char2)
     `(,',base-op (char-upcase ,char1) (char-upcase ,char2))))

(def-case-insensitive-char-comparitor char-equal char=)

(def-case-insensitive-char-comparitor char-not-equal char/=)

(def-case-insensitive-char-comparitor char-lessp char<)

(def-case-insensitive-char-comparitor char-greaterp char>)

(def-case-insensitive-char-comparitor char-not-greaterp char<=)

(def-case-insensitive-char-comparitor char-not-lessp char>=)

(defmacro int-char (integer)
  `(code-char ,integer))

(defmacro char-int (character)
  `(char-code ,character))

(defun char-name (character)
  (declare (type character character)
	   (return-type t))
  (case (char-code character)
    (#.(char-code #\newline) "newline")
    (#.(char-code #\space) "space")
    (#.(char-code #\rubout) "rubout")
    (#.(char-code #\page) "page")
    (#.(char-code #\tab) "tab")
    (#.(char-code #\backspace) "backspace")
    (#.(char-code #\return) "return")
    (#.(char-code #\null) "null")
    ;; Linefeed is the same as newline.  -jra 12/30/96
    ;; (#.(char-code #\linefeed) "linefeed")
    (t nil)))





;;;; String Case Convesions

(defun copy-string (string length)
  (declare (type string string)
	   (type fixnum length)
	   (consing-area either))
  (let ((new-string (make-string length)))
    (replace-strings new-string string :end2 length)
    new-string))

(defun nstring-upcase (string &key (start 0) end)
  (declare (type string string)
	   (type fixnum start)
	   (return-type string))
  (do ((limit (if end end (tli::length-trans string)))
       (index start (+ index 1)))
      ((>= index limit) string)
    (declare (type fixnum limit index))
    (setf (char string index)
	  (char-upcase (char string index)))))

(defun string-upcase (string &key (start 0) end)
  (declare (type fixnum start)
	   (type string string)
	   (return-type string)
	   (consing-area either))
  (do* ((length (tli::length-trans string))
	(limit (if end end length))
	(index start (+ index 1)))
       ((>= index limit) string)
    (declare (type fixnum length limit index))
    (when (lower-case-p (char string index))
      (return (nstring-upcase
		(copy-string string length) :start index :end limit)))))

(defun nstring-downcase (string &key (start 0) end)
  (declare (type string string)
	   (type fixnum start)
	   (return-type string))
  (do ((limit (if end end (tli::length-trans string)))
       (index start (+ index 1)))
      ((>= index limit) string)
    (declare (type fixnum limit index))
    (setf (char string index)
	  (char-downcase (char string index)))))

(defun string-downcase (string &key (start 0) end)
  (declare (type fixnum start)
	   (type string string)
	   (return-type string)
	   (consing-area either))
  (do* ((length (tli::length-trans string))
	(limit (if end end length))
	(index start (+ index 1)))
       ((>= index limit) string)
    (declare (type fixnum length limit index))
    (when (upper-case-p (char string index))
      (return (nstring-downcase
		(copy-string string length) :start index :end limit)))))

(defun nstring-capitalize (string &key (start 0) end)
  (declare (type string string)
	   (type fixnum start)
	   (return-type string))
  (do ((word-start t)
       (limit (if end end (tli::length-trans string)))
       (index start (+ index 1)))
      ((>= index limit) string)
    (declare (type fixnum limit index))
    (let ((char (char string index)))
      (declare (type character char))
      (setf (char string index)
	    (cond ((alphanumericp char)
		   (cond (word-start
			  (setq word-start nil)
			  (char-upcase char))
			 (t
			  (char-downcase char))))
		  (t
		   (setq word-start t)
		   char))))))

(defmacro string-capitalize (string &key (start 0) end)
  (let ((original-string (gensym))
	(length (gensym)))
   `(let* ((,original-string ,string)
	   (,length (tli::length-trans ,original-string)))
      (nstring-capitalize
	(copy-string ,original-string ,length)
	:start ,start :end ,end))))
	




;;;; String Pools




;;; The macro `reclaim-string' is a no-op stub to place hold for when we have a
;;; built-in pool for 8-bit strings within TL.

(defmacro reclaim-string (string)
  `(progn ,string nil))




;;;; Stream Pools




;;; The macro `reclaim-string-stream' is a no-op stub to place hold for when we
;;; have pools of string streams.

(defmacro reclaim-string-stream (string-stream)
  `(progn ,string-stream nil))






;;;; Printing




(def-translatable-lisp-var *terminal-io* (tli::make-terminal-io-file-stream))

(def-translatable-lisp-var *standard-input* *terminal-io*)

(def-translatable-lisp-var *standard-output* *terminal-io*)

(def-translatable-lisp-var *error-output* (tli::make-error-output-file-stream))

(def-translatable-lisp-var *query-io* *terminal-io*)

(def-translatable-lisp-var *debug-io* *terminal-io*)

(def-translatable-lisp-var *trace-output* *error-output*)

(def-translatable-lisp-var *print-case* :upcase)

(def-translatable-lisp-var *print-base* 10)

(def-translatable-lisp-var *print-escape* t)

(def-translatable-lisp-var *print-pretty* nil)

(def-translatable-lisp-var *print-length* nil)

(def-translatable-lisp-var *print-circle* nil)

(def-translatable-lisp-var *print-level* nil)

(defconstant default-string-stream-size 128)

(defmacro get-string-or-file-stream-for-output-macro (stream-arg needed-space)
  (let ((stream (gensym))
	(needed-chars (gensym)))
    `(let ((,stream ,stream-arg)
	   (,needed-chars ,needed-space))
       (declare (type fixnum ,needed-chars))
       (typecase ,stream
	 (symbol
	  (cond
	    ((null ,stream) *standard-output*)
	    ((eq ,stream t) *terminal-io*)
	    (t (get-string-or-file-stream-for-output ,stream ,needed-chars))))
	 (file-stream ,stream)
	 (string ,stream)
	 (t
	  (get-string-or-file-stream-for-output ,stream ,needed-chars))))))

(defun get-string-or-file-stream-for-output (stream-arg needed-space)
  (declare (type fixnum needed-space)
	   (return-type t))
  (typecase stream-arg
    (symbol
     (cond ((null stream-arg) *standard-output*)
	   ((eq stream-arg t) *terminal-io*)
	   (t (bad-stream-error stream-arg)
	      nil)))
    ((or file-stream string)
     stream-arg)
    (string-stream
     (let* ((string-list (tli::string-stream-strings stream-arg))
	    (first-string (car string-list)))
       (when (or (null first-string)
		 (< (the fixnum
			 (- (array-dimension (the string first-string) 0)
			    (fill-pointer (the string first-string))))
		    needed-space))
	 (locally (declare (consing-area permanent))
	   (let ((new-string
		   (make-string
		     (if (> needed-space default-string-stream-size)
			 needed-space
			 default-string-stream-size))))
	     (declare (type string new-string))
	     (setf (fill-pointer new-string) 0)
	     (setf (tli::string-stream-strings stream-arg)
		   (cons new-string string-list))
	     (setq first-string new-string))))
       first-string))
    (t
     ;; When not in translation, we have to be able to handle arbitrary Lisp
     ;; streams, so just return them here.
     #-translator
     (when (ab-lisp::typep stream-arg 'ab-lisp::stream)
       (return-from get-string-or-file-stream-for-output stream-arg))
     (bad-stream-error stream-arg))))

(defun last-string-char? (string)
  (declare (type string string)
	   (return-type t))
  (let ((length (tli::length-trans string)))
    (declare (type fixnum length))
    (if (> length 0)
	(char string (1- length))
	nil)))

(defun get-last-string-stream-character (string-stream)
  (declare (type string-stream string-stream)
	   (return-type t))
  (let ((first-string? (car (tli::string-stream-strings string-stream))))
    (if first-string?
	(last-string-char? first-string?)
	nil)))

(defun last-stream-char? (stream)
  (declare (return-type t))
  (cond ((stringp stream)
	 (last-string-char? stream))
	((typep stream 'string-stream)
	 (get-last-string-stream-character stream))
	(t nil)))

(defun get-output-stream-string (string-stream)
  (declare (type string-stream string-stream)
	   (return-type t))
  (let ((string-list (tli::string-stream-strings string-stream))
	(length 0))
    (declare (type fixnum length))
    (dolist (string string-list)
      (declare (type t string))
      (incf length (tli::length-trans (the string string))))
    (let ((result-string (locally (declare (consing-area permanent))
			   (make-string length)))
	  (current-end length))
      (declare (type string result-string)
	       (type fixnum current-end))
      (dolist (string string-list)
	(declare (type t string))
	(let ((this-length (tli::length-trans (the string string))))
	  (declare (type fixnum this-length))
	  (decf current-end this-length)
	  (replace-strings result-string
			   (the string string)
			   :start1 current-end
			   :start2 0
			   :end2 this-length)))
      result-string)))

(defmacro with-output-to-string ((var &optional string) &body forms)
  `(let ((,var ,(or string `(make-string-output-stream))))
     ,(if string
	  `(progn ,@forms)
	  `(locally
	       (declare (require-local-exit
			  "You cannot return-from through a with-output-to-string, it will leak!"))
	     (prog1
		 (progn
		   ,@forms
		   (get-output-stream-string ,var))
	       (reclaim-string-stream ,var))))))

(defun force-output (&optional output-stream)
  (declare (return-type null))
  (let ((stream (get-string-or-file-stream-for-output output-stream 0)))
    (cond
      ((typep stream 'file-stream)
       (tli::force-output-to-file stream))
      #-translator
      ((ab-lisp::typep stream 'ab-lisp::stream)
       (ab-lisp::force-output stream)))
    nil))

(defun write-string
    (string &optional (stream? nil) &key (start 0) (end nil))
  (declare (type string string)
	   (type fixnum start)
	   (return-type string))
  (let* ((end-index (if (null end)
		  (tli::length-trans string)
		  end))
	 (stream (get-string-or-file-stream-for-output stream?
						       (- end-index start))))
    (declare (type fixnum end-index))
    (cond ((stringp stream)
	   (let ((current-fill (fill-pointer (the string stream))))
	     (declare (type fixnum current-fill))
	     (setf (fill-pointer (the string stream))
		   (+ current-fill (the fixnum (- end-index start))))
	     (replace-strings
	       stream string
	       :start1 current-fill
	       :start2 start
	       :end2 end-index)))
	  ((or end (/= start 0))
	   (do ((index start (+ index 1)))
	       ((>= index end-index)
		nil)
	     (declare (type fixnum index))
	     (write-char-to-file-stream (char string index) stream)))
	  (t
	   (write-string-to-file-stream string stream)))
    string))

(defmacro write-char-to-string (character string)
  (let ((char (gensym))
	(str (gensym))
	(fill (gensym)))
    (if (and (or (constantp character) (symbolp character))
	     (symbolp string))
	`(let ((,fill (fill-pointer (the string ,string))))
	   (declare (type fixnum ,fill))
	   (setf (fill-pointer (the string ,string)) (+ ,fill 1))
	   (setf (char (the string ,string) ,fill) ,character))
      `(let ((,char ,character)
	     (,str ,string))
	 (declare (type character ,char))
	 (write-char-to-string ,char ,str)))))

(defmacro write-char-to-string-or-file-stream (character string-or-file-stream string?)
  (if (and (or (symbolp character)
	       (constantp character))
	   (symbolp string-or-file-stream))
      `(progn 
	 (if ,string?
	     (write-char-to-string ,character ,string-or-file-stream)
	   (write-char-to-file-stream ,character ,string-or-file-stream))
	 ,character)
    (let ((char (gensym))
	  (stream (gensym)))
      `(let ((,char ,character)
	     (,stream ,string-or-file-stream))
	 (declare (type character ,char))
	 (write-char-to-string-or-file-stream ,char ,stream ,string?)))))

(defun write-char (char &optional (stream? nil))
  (declare (type character char)
	   (return-type character))
  (let ((string? nil))
    (typecase stream?
      (string
       (setq string? t))
      (file-stream
       nil)
      (t
       (setq stream? (get-string-or-file-stream-for-output stream? 1))
       (setq string? (stringp stream?))))
    (write-char-to-string-or-file-stream char stream? string?)
    char))

(defun write-fixnum-in-hex (fixnum stream?)
  (declare (type fixnum fixnum)
	   (return-type void))
  (when (< fixnum 0)
    (write-char #\- stream?)
    (setq fixnum (- fixnum)))
  (when (> fixnum 15)
    (write-fixnum-in-hex (ash fixnum -4) stream?)
    (setq fixnum (logand fixnum 15)))
  (write-char (digit-char-no-nils fixnum)
	      stream?))

(defun write-fixnum-in-arbitrary-base (fixnum base stream)
  (declare (type fixnum fixnum base)
	   (return-type void))
  (cond ((< fixnum 0)
	 (write-char #\- stream)
	 ;; Note that the most-negative-fixnum case works here because
	 ;; type-declared fixnum locations in TL have 32 bits of storage, not
	 ;; 30.  So, while technically negating most-negative-fixnum will
	 ;; overflow most-positive-fixnum, in practice we have two extra bits of
	 ;; room and it works out OK.  -jra 1/5/97
	 (write-fixnum-in-arbitrary-base (- fixnum) base stream))
	((>= fixnum base)
	 (write-fixnum-in-arbitrary-base (floor fixnum base) base stream)
	 (write-char (digit-char-no-nils (mod fixnum base)) stream))
	(t
	 (write-char (digit-char-no-nils fixnum) stream))))

(defparameter reversed-fixnum-with-commas-string (make-string 64))

(defun write-fixnum-with-commas (fixnum base comma-interval comma-char stream)
  (declare (type fixnum fixnum base comma-interval)
	   (type character comma-char)
	   (return-type void))
  (let ((reversed-fixnum-with-commas reversed-fixnum-with-commas-string))
    (declare (type string reversed-fixnum-with-commas))
    (when (< fixnum 0)
      (write-char #\- stream)
      (setq fixnum (- fixnum)))
    (setf (fill-pointer reversed-fixnum-with-commas) 0)
    (do ((char-count 0 (+ char-count 1))
	 (fill 0 (+ fill 1)))
	((and (<= fixnum 0) (> fill 0)))
      (declare (type fixnum char-count fill))
      (when (>= char-count comma-interval)
	(setq char-count 0)
	(setf (fill-pointer reversed-fixnum-with-commas) (+ fill 1))
	(setf (char reversed-fixnum-with-commas fill) comma-char)
	(incf fill))
      (setf (fill-pointer reversed-fixnum-with-commas) (+ fill 1))
      (setf (char reversed-fixnum-with-commas fill)
	    (digit-char-no-nils (mod fixnum base)))
      (setq fixnum
	    (if (< fixnum base)
		(- fixnum base)
		(floor fixnum base))))
    (do ((index (1- (tli::length-trans reversed-fixnum-with-commas))
		(- index 1)))
	((< index 0))
      (declare (type fixnum index))
      (write-char (char reversed-fixnum-with-commas index) stream))))

(defun write-fixnum (fixnum base width stream?)
  (declare (type fixnum fixnum base width)
	   (return-type fixnum))
  (let ((stream (get-string-or-file-stream-for-output-macro
		  stream?
		  (if (= width 0) 11 width))))
    (declare (type fixnum base))
    (cond
      ((= base 10)
       (if (stringp stream)
	   (tli::write-fixnum-to-string fixnum width stream)
	   (tli::princ-fixnum-to-file fixnum width stream)))
      ((= base 16)
       (write-fixnum-in-hex fixnum stream))
      (t
       (write-fixnum-in-arbitrary-base fixnum base stream)))
    fixnum))

(defmacro write-double-float (double-float width &optional (stream? nil))
  (let ((double (gensym))
	(width-value (gensym))
	(stream (gensym)))
    `(let* ((,double ,double-float)
	    (,width-value ,width)
	    (,stream (get-string-or-file-stream-for-output
		       ,stream?
		       (if (= ,width-value 0) 16 ,width-value))))
       (declare (type double-float ,double)
		(type fixnum ,width-value))
       (if (stringp ,stream)
	   (tli::write-double-to-string ,double ,width-value ,stream)
	   (tli::princ-double-to-file ,double ,width-value ,stream))
       ,double)))


(defmacro write-object-pointer-in-hex (object stream?)
  `(write-fixnum-in-hex
     (tli::pointer-as-fixnum ,object)
     ,stream?))

(defun print-random-object-with-type-name (object name extra-info? stream?)
  (declare (type string name)
	   (return-type void))
  (write-string "#<" stream?)
  (write-string name stream?)
  (write-char #\space stream?)
  (when extra-info?
    (princ extra-info? stream?)
    (write-char #\space stream?))
  (write-string "0x" stream?)
  (write-object-pointer-in-hex object stream?)
  (write-char #\> stream?))

(defun write (object &key
		     (stream *standard-output*) (case *print-case*)
		     (escape *print-escape*) (base *print-base*)
		     (pretty *print-pretty*) (level *print-level*)
		     (length *print-length*) (circle *print-circle*))
  (declare (return-type t)
	   (ignore pretty level length circle))
  (typecase object
    (null
     (write-string
       (cond ((eq case :downcase) "nil")
	     ((eq case :capitalize) "Nil")
	     ;; The :upcase or default behavior
	     (t "NIL"))
       stream))
    (fixnum
     (write-fixnum object base 0 stream))
    (cons
     (write-list object stream))
    (character
     (locally (declare (type character object))
       (if escape
	   (let ((name? (char-name object)))
	     (write-string "#\\" stream)
	     (if name?
		 (write-string name? stream)
		 (write-char object stream)))
	   (write-char object stream))))
;    (managed-float
;     (with-temporary-area
;       (print-random-object-with-type-name
;         object "Managed-Float" (managed-float-value object) stream)))
    (double-float
     (write-double-float object 0 stream))
    (simple-vector
     (print-random-object-with-type-name
       object "Simple-Vector" (tli::length-trans (the simple-vector object)) stream))
    (string
     (locally (declare (type string object))
       (cond (escape
	      (write-char #\" stream)
	      (dotimes (index (tli::length-trans object))
		(let ((char (char object index)))
		  (when (or (char= char #\") (char= char #\\))
		    (write-char #\\ stream))
		  (write-char char stream)))
	      (write-char #\" stream))
	     (t
	      (write-string object stream)))))
    ((array (unsigned-byte 8))
     (print-random-object-with-type-name
       object "Unsigned-Byte-8-Vector"
       (tli::length-trans (the (array (unsigned-byte 8)) object))
       stream))
    ((array (unsigned-byte 16))
     (print-random-object-with-type-name
       object "Unsigned-Byte-16-Vector"
       (tli::length-trans (the (array (unsigned-byte 16)) object))
       stream))
    ((array (signed-byte 16))
     (print-random-object-with-type-name
       object "Signed-Byte-16-Vector"
       (tli::length-trans (the (array (signed-byte 16)) object))
       stream))
    ((array double-float)
     (print-random-object-with-type-name
       object "Double-Float-Vector"
       (tli::length-trans (the (array double-float) object))
       stream))
    (symbol
     (funcall-simple-compiled-function #'write-symbol object case stream))
    (compiled-function
     (print-random-object-with-type-name
       object "Compiled-Function"
       (tli::compiled-function-name object)
       stream))
    (package
     (print-random-object-with-type-name
       object "Package" (package-name object) stream))
    (tli::unbound
     (print-random-object-with-type-name
       object "The-Unbound-Value" nil stream))
    (string-stream
     (print-random-object-with-type-name
       object "String-Stream" nil stream))
    (file-stream
     (print-random-object-with-type-name
       object "File-Stream" nil stream))
    ;; During Lisp development, we will need to be able to print Lisp pathnames.
    #-translator
    (ab-lisp::pathname
     (write-string (tli::pathname-print-string object case escape) stream))
    (t
     (print-random-object-with-type-name
       object "Unknown-Type" nil stream)))
  object)

(defun prin1 (object &optional (stream? nil))
  (declare (return-type t))
  (let ((*print-escape* t))
    (write object :stream stream?)))

(defun princ (object &optional (stream? nil))
  (declare (return-type t))
  (let ((*print-escape* nil))
    (write object :stream stream?)))

(defun terpri (&optional (stream? nil))
  (write-char #\newline stream?)
  nil)

(defmacro fresh-line (&optional (stream? nil))
  `(progn
     (terpri ,stream?)
     t))

(defun write-line (string &optional (stream? nil) &key (start 0) (end nil))
  (write-string string stream? :start start :end end)
  (write-char #\newline stream?)
  string)

(defun print (object &optional (stream? nil))
  (terpri stream?)
  (prin1 object stream?)
  (write-char #\space stream?)
  object)

(defmacro write-byte (byte file-stream)
  (let ((value (gensym)))
    `(let ((,value ,byte))
       (declare (type fixnum ,value))
       (tli::write-byte-to-file ,value ,file-stream)
       ,value)))

(defun write-list (cons stream?)
  (declare (return-type t))
  (write-char #\( stream?)
  (do* ((current-cons cons next-cons)
	(current-car (car current-cons) (car current-cons))
	(next-cons (cdr current-cons) (cdr current-cons)))
       ((atom next-cons)
	(write current-car :stream stream?)
	(cond ((null next-cons)
	       (write-char #\) stream?))
	      (t
	       (write-string " . " stream?)
	       (write next-cons :stream stream?)
	       (write-char #\) stream?))))
    (declare (type cons current-cons))
    (write current-car :stream stream?)
    (write-char #\space stream?))
  cons)




;;; Within format directives that require a certain string width, temporary
;;; strings will be used out of the field-width-string-list variable.  Within
;;; format, this variable will be bound to itself.  If a string is needed within
;;; format, then the string at the CAR of this variable should be used, and if
;;; there is another listed string in the CDR of this variable, it should be
;;; SETQed to that next cons.  If the CDR is NIL, a new string should be
;;; allocated, it should be added to the end of the list, and then the newly
;;; created cons placed into this variable.  The strings should be
;;; field-width-string-length characters long, which is currently set to 256.

(defconstant field-width-string-length 256)

(defvar field-width-string-list (list (make-string field-width-string-length)))

(defun alloc-field-width-string ()
  (declare (consing-area permanent))
  (let* ((this-cons field-width-string-list)
	 (this-string (car this-cons))
	 (next-cons? (cdr this-cons)))
    (declare (type cons this-cons))
    (unless next-cons?
      (setq next-cons?
	    (cons (make-string field-width-string-length) nil))
      (setf (cdr this-cons) next-cons?))
    (setq field-width-string-list next-cons?)
    (setf (fill-pointer (the string this-string)) 0)
    this-string))

(defun write-with-arglist
    (stream object arglist atsign-modifier? colon-modifier?)
  (declare (return-type t))
  (let ((current-stream (if arglist (alloc-field-width-string) stream)))
    (if (null object)
	(write-string (if colon-modifier? "()" "nil") current-stream)
	(write object :stream current-stream))
    (when arglist
      (destructuring-bind-strict
	  (mincol-arg &optional colinc-arg minpad-arg padchar-arg)
	arglist
      ;; Note that defaulting is done in the LET below instead of the
      ;; destructuring bind above, since users are allowed to explicitly supply
      ;; NIL for an argument, and that is supposed to be used as an indication
      ;; that the variable should take on the default value.
	(let ((mincol (or mincol-arg 0))
	      (colinc (or colinc-arg 1))
	      (minpad (or minpad-arg 0))
	      (padchar (or padchar-arg #\space)))
	  (declare (type fixnum mincol colinc minpad)
		   (type character padchar)
		   (type string current-stream))
	  (unless atsign-modifier?
	    (write-string current-stream stream))
	  (do ((length (tli::length-trans current-stream))
	       (pad 0 (+ pad colinc)))
	      ((and (>= (+ length pad) mincol)
		    (>= pad minpad)))
	    (declare (type fixnum length pad))
	    (dotimes (x colinc)
	      (write-char padchar stream)))
	  (when atsign-modifier?
	    (write-string current-stream stream)))))
    object))




;;; In the fixnum writer, we ignore the colon-modifer and the arguments about
;;; comma insertion into the number.  -jra 1/3/97

(defun write-fixnum-with-arglist
    (stream fixnum arglist atsign-modifier? colon-modifier?)
  (declare (type fixnum fixnum)
	   (return-type void))
  (let ((current-stream (if arglist (alloc-field-width-string) stream)))
    (when (and atsign-modifier? (>= fixnum 0))
      (write-char #\+ current-stream))
    (destructuring-bind-strict
	(&optional mincol-arg padchar-arg comma-char-arg comma-interval-arg)
      arglist
      ;; Note that defaulting is done in the LET below instead of the
      ;; destructuring bind above, since users are allowed to explicitly supply
      ;; NIL for an argument, and that is supposed to be used as an indication
      ;; that the variable should take on the default value.
      (let ((mincol (or mincol-arg 0))
	    (padchar (or padchar-arg #\space))
	    (comma-char (or comma-char-arg #\,))
	    (comma-interval (or comma-interval-arg 3)))
	(declare (type fixnum mincol comma-interval)
		 (type character padchar comma-char))
	(if colon-modifier?
	    (write-fixnum-with-commas
	      fixnum *print-base* comma-interval comma-char current-stream)
	    (write-fixnum fixnum *print-base* 0 current-stream))
	(when mincol-arg
	  (locally (declare (type string current-stream))
	    (dotimes (x (- mincol (tli::length-trans current-stream)))
	      (write-char padchar stream))
	    (write-string current-stream stream)))))))




;;; The TL implementation of `format' supports a compatible subset of the
;;; control directives and argument list features of those directives.
;;; TL:Format supports the reading of arguments to control directives as
;;; described in the CL documentation of format, including up to four args, with
;;; numeric args, character args, "V" style argument indirection, "#" style
;;; remaining argument counting, and the atsign and colon modifier flags.
;;; TL:Write and TL:format support *print-escape*, *print-base*, and
;;; *print-case* in the standard way.  The following are the supported control
;;; directives and argument patterns.  See CLtL2 for a more verbose description
;;; of the same thing.

;;; Char Name         Arguments         @ Meaning      : Meaning
;;; ---- ---------    ----------------- -------------- ---------------
;;; ~A   Aesthetic    mincol, colinc,   pad left       () for nil
;;;                    minpad, padchar
;;; ~C   Character    none              add escapes    <unsupported>
;;; ~S   Standard     mincol, colinc,   pad on left    () for nil
;;;                    minpad,padchar
;;; ~D   Decimal      mincol,padchar    + or - always  <unsupported>
;;; ~H   Hex          mincol,padchar    + or - always  <unsupported>
;;; ~O   Octal        mincol,padchar    + or - always  <unsupported>
;;; ~B   Binary       mincol,padchar    + or - always  <unsupported>

;;; ~(   Downcase     none              @ = cap-first  : = capitalize
;;;                                            @: = upcase
;;; ~)   End-of-case-change

;;; ~{   Iterate      max-iterations    use remaining  use sublists
;;; ~}   End-of-iteration
;;; ~^   Premature-up-and-out

;;; ~[   Conditional  select-nth-clause do-if-true     select-false-then-true
;;; ~;   Separator    none              <unsupported>  default-clause
;;; ~]   End-of-conditional

(defun format (stream-arg control-string &rest args)
  (declare (type string control-string))
  (let ((stream (or stream-arg
		    ;; The following is a leak.  We need to make these streams
		    ;; in a pool.  -jra 12/30/96
		    (locally (declare (consing-area permanent))
		      (make-string-output-stream))))
	;; When we install a string to stream upcase and downcase caching into,
	;; the value of stream will be cached here.
	(cached-stream? nil)
	(current-case-conversion nil)
	(index 0)
	(iteration-start-index? nil)
	(iteration-maximum-loops? nil)
	(iteration-end-index nil)
	(iteration-cached-args nil)
	(iteration-uses-sublists? nil)
	(iteration-sublists nil)
	(control-string-length (tli::length-trans control-string))
	(field-width-string-list field-width-string-list))
    (declare (type fixnum index control-string-length))
    (do ()
	((>= index control-string-length))
      (let ((next-char (char control-string index)))
	(declare (type character next-char))
	(incf index)
	(cond
	  ((char= next-char #\~)
	   (when (>= index control-string-length)
	     (bad-control-directive-error control-string))
	   (setq next-char (char control-string index))
	   (incf index)
	   (let ((arglist-pool (list nil nil nil nil nil nil nil))
		 (arglist nil)
		 (current-arg? nil)
		 (colon-modifier? nil)
		 (atsign-modifier? nil))
	     (declare (dynamic-extent arglist-pool))
	     (do ((numeric? (digit-char-p next-char)
			    (digit-char-p next-char)))
		 (nil)
	       (if numeric?
		   (setq current-arg?
			 (cond ((fixnump current-arg?)
				(+ (* (the fixnum current-arg?) 10)
				   (the fixnum numeric?)))
			       ((null current-arg?)
				numeric?)
			       (t
				(bad-control-directive-error control-string))))
		   (case (char-code next-char)
		     ((#.(char-code #\,))
		      (let ((new-cons arglist-pool))
			(declare (type cons new-cons))
			(setq arglist-pool (cdr new-cons))
			(setf (car new-cons) current-arg?)
			(setf (cdr new-cons) nil)
			(if arglist
			    (setf (cdr (last arglist)) new-cons)
			    (setq arglist new-cons))
			(setq current-arg? nil)))
		     ((#.(char-code #\v) #.(char-code #\V))
		      (setq current-arg? (pop args)))
		     ((#.(char-code #\#))
		      (setq current-arg? (tli::length-trans args)))
		     ((#.(char-code #\'))
		      (setq current-arg? (char control-string index))
		      (incf index))
		     ((#.(char-code #\@))
		      (setq atsign-modifier? t))
		     ((#.(char-code #\:))
		      (setq colon-modifier? t))
		     (t
		      (when current-arg?
			(let ((new-cons arglist-pool))
			  (declare (type cons new-cons))
			  (setq arglist-pool (cdr new-cons))
			  (setf (car new-cons) current-arg?)
			  (setf (cdr new-cons) nil)
			  (if arglist
			      (setf (cdr (last arglist)) new-cons)
			      (setq arglist new-cons))))
		      (return nil))))
	       (when (>= index control-string-length)
		 (bad-control-directive-error control-string))
	       (setq next-char (char control-string index))
	       (incf index))
	     (let ((down-char (char-downcase next-char)))
	       (declare (type character down-char))
	       (case down-char
		 ((#\a #\s)
		  (let ((*print-escape* (char= down-char #\s)))
		    (write-with-arglist stream (pop args) arglist
					atsign-modifier? colon-modifier?)))
		 ((#\c)
		  (write (pop args) :stream stream :escape atsign-modifier?))
		 ((#\d #\x #\o #\b)
		  (let ((object (pop args))
			(*print-escape* nil)
			(*print-base* (case down-char
					(#\x 16) (#\o 8) (#\b 2) (t 10))))
		    (cond ((fixnump object)
			   (write-fixnum-with-arglist
			     stream object arglist atsign-modifier?
			     colon-modifier?))
			  (t
			   ;; Note that the arglist format is differnt for
			   ;; write-with-arglist, convert the fixnum style to
			   ;; the write style.
			   (when (cdr arglist)
			     (let ((new-cdr arglist))
			       (setq arglist (cdddr new-cdr))
			       (setf (cdddr new-cdr) nil)
			       (setf (car new-cdr) nil)
			       (setf (cadr new-cdr) nil)
			       (setf (caddr new-cdr) (cadr arglist))
			       (setf (cdr arglist) new-cdr)))
			   (write-with-arglist
			     stream object arglist atsign-modifier?
			     colon-modifier?)))))
		 ((#\&)
		  (unless (eql #\newline (last-stream-char? stream))
		    (write-char #\newline stream))
		  (when arglist
		    (dotimes (x (1- (the fixnum (car (the cons arglist)))))
		      (write-char #\newline stream))))
		 ((#\t)
		  ;; The spec allows a stupid implementation of tabulate to at
		  ;; worst emit 2 spaces.  I'm all over that.  -jallard, 5/1/97
		  (write-string "  " stream))
		 ((#\%)
		  (dotimes (x (or (car arglist) 1))
		    (write-char #\newline stream)))
		 ((#\~)
		  (write-char #\~ stream))
		 ((#\newline)
		  (when atsign-modifier?
		    (write-char #\newline stream))
		  (unless colon-modifier?
		    (do ()
			((and (< index control-string-length)
			      (let ((white-char (char control-string index)))
				(and (not (char= white-char #\space))
				     (not (char= white-char #\tab)))))
			 nil)
		      (incf index))))
		 ((#\()
		  (when (or current-case-conversion cached-stream?)
		    (bad-control-directive-error control-string))
		  (setq cached-stream? stream)
		  (setq stream (alloc-field-width-string))
		  (setq current-case-conversion
			(if atsign-modifier?
			    (if colon-modifier?
				:upcase
				:cap-first-lower-rest)
			    (if colon-modifier?
				:capitalize
				:downcase))))
		 ((#\))
		  (unless (and current-case-conversion cached-stream?)
		    (bad-control-directive-error control-string))
		  (let ((field-width-string stream))
		    (declare (type string field-width-string))
		    (case current-case-conversion
		      (:upcase
		       (nstring-upcase field-width-string))
		      (:downcase
		       (nstring-downcase field-width-string))
		      (:capitalize
		       (nstring-capitalize field-width-string))
		      (:cap-first-lower-rest
		       (when (> (tli::length-trans field-width-string) 0)
			 (setf (char field-width-string 0)
			       (char-upcase (char field-width-string 0)))
			 (nstring-downcase field-width-string :start 1))))
		    (write-string field-width-string cached-stream?))
		  (setq stream cached-stream?)
		  (setq current-case-conversion nil)
		  (setq cached-stream? nil))
		 ((#\{)
		  (when iteration-start-index?
		    (format-error
		      "TL:FORMAT doesn't support nested ~{ iterations."
		      control-string))
		  (setq iteration-start-index? index)
		  (setq iteration-maximum-loops? (car arglist))
		  (cond
		    (atsign-modifier?
		     (cond (colon-modifier?
			    (setq iteration-uses-sublists? t)
			    (setq iteration-sublists args))
			   (t
			    (setq iteration-uses-sublists? nil)))
		     (setq iteration-cached-args nil))
		    (t
		     (cond (colon-modifier?
			    (setq iteration-uses-sublists? t)
			    (setq iteration-sublists (car args)))
			   (t
			    (setq args (car args))
			    (setq iteration-uses-sublists? nil)))
		     (setq iteration-cached-args (cdr args))))
		  (let ((at-least-once? nil))
		    (setq iteration-end-index
			  (do ((end-index index (+ end-index 1)))
			      ((>= end-index control-string-length)
			       nil)
			    (declare (type fixnum end-index))
			    (let ((char (char control-string end-index)))
			      (declare (type character char))
			      (when (char= char #\~)
				(let ((next-char
					(char control-string (+ end-index 1))))
				  (declare (type character next-char))
				  (cond ((char= next-char #\})
					 (return (+ end-index 2)))
					((and (char= next-char #\:)
					      (char= (char control-string
							   (+ end-index 2))
						     #\}))
					 (setq at-least-once? t)
					 (return (+ end-index 3)))))))))
		    (when (null iteration-end-index)
		      (format-error
			"In FORMAT, a ~{ iteration had no closing ~}"
			control-string))
		    (cond
		      ((or (and iteration-maximum-loops?
				(<= (the fixnum iteration-maximum-loops?) 0))
			   (and (if iteration-uses-sublists?
				    (null iteration-sublists)
				    (null args))
				(not at-least-once?)))
		       (setq args iteration-cached-args)
		       (setq iteration-start-index? nil)
		       (setq index iteration-end-index))
		      (iteration-uses-sublists?
		       (setq args (pop iteration-sublists))))))
		 ((#\^)
		  (when (null iteration-start-index?)
		    (format-error "In FORMAT, ~^ found outside of any ~{ iteration."
				  control-string))
		  (setq iteration-start-index? nil)
		  (setq index iteration-end-index))
		 ((#\})
		  (when (null iteration-start-index?)
		    (format-error "In FORMAT, unmatched ~}."
				  control-string))
		  (cond
		    ((or (and iteration-maximum-loops?
			      (<= (the fixnum
				       (decf (the fixnum
						  iteration-maximum-loops?)))
				  0))
			 (if iteration-uses-sublists?
			     (null iteration-sublists)
			     (null args)))
		     ;; Exit the iteration.  Since index has already read past
		     ;; the close curly brace, we don't need to reset it.
		     (setq iteration-start-index? nil)
		     (setq args iteration-cached-args))
		    (t
		     ;; Loop back to the top of the iteration again.  If we are
		     ;; using sublists, reset args to the next sublist.
		     (setq index iteration-start-index?)
		     (when iteration-uses-sublists?
		       (setq args (pop iteration-sublists))))))
		 ((#\[)
		  (let ((clause-number
			  (if atsign-modifier?
			      (cond ((car args) 0)
				    (t (pop args)
				       most-positive-fixnum))
			      (if colon-modifier?
				  (if (pop args) 1 0)
				  (if arglist
				      (car arglist)
				      (pop args))))))
		    (declare (type fixnum clause-number))
		    (when (< clause-number 0)
		      (setq clause-number most-positive-fixnum))
		    (do ((clause-index index (1+ clause-index)))
			((>= clause-index control-string-length)
			 (format-error "No ~] found for ~[" control-string))
		      (declare (type fixnum clause-index))
		      (when (zerop clause-number)
			(setq index clause-index)
			(return nil))
		      (let ((char (char control-string clause-index)))
			(declare (type character char))
			(when (char= char #\~)
			  (incf clause-index)
			  (let* ((directive-index
				   (discard-format-arglist
				     control-string clause-index
				     control-string-length))
				 (next-char
				   (char control-string directive-index)))
			    (declare (type fixnum directive-index)
				     (type character next-char))
			    (case next-char
			      ((#\;)
			       (cond
				 ((char= (char control-string clause-index) #\:)
				  (setq index (+ directive-index 1))
				  (return nil))
				 ((/= clause-index directive-index)
				  (format-error "Only : args to ~; allowed."
						control-string))
				 (t
				  (decf clause-number))))
			      ((#\])
			       (setq index (+ directive-index 1))
			       (return nil))
			      ((#\[)
			       (setq clause-index
				     (find-end-of-conditional
				       control-string (+ directive-index 1)
				       control-string-length)))
			      (t
			       (setq clause-index
				     directive-index)))))))))
		 ((#\;)
		  (setq index (1+ (find-end-of-conditional
				    control-string index
				    control-string-length))))
		 ((#\])
		  ;; Index has been pre-incremented, do nothing.
		  nil)			      
		 (t
		  (unsupported-control-char-error next-char))))))
	  (t
	   (write-char next-char stream)))))
    (if (null stream-arg)
	(prog1 (get-output-stream-string stream)
	  (reclaim-string-stream stream))
	nil)))

(defun discard-format-arglist (control-string index length)
  (declare (type string control-string)
	   (type fixnum index length)
	   (return-type fixnum))
  (do ((char (char control-string index)
	     (char control-string index)))
      ((>= index length)
       (format-error "Twiddle did not have corresponding directive."
		     control-string)
       0)
    (declare (type character char))
    (case char
      ((#\, #\v #\V #\# #\@ #\: #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       ;; Skip it
       (incf index))
      ((#\')
       ;; Skip this and the next char.
       (incf index 2))
      (t
       ;; It must be a directive, return it.
       (return index)))))

(defun find-end-of-conditional (control-string conditional-body-index length)
  (declare (type string control-string)
	   (type fixnum conditional-body-index length)
	   (return-type fixnum))
  (do* ((nesting 1)
	(index conditional-body-index (+ index 1)))
       ((>= index length)
	(format-error "Unmatched ~[." control-string))
    (declare (type fixnum nesting index))
    (let ((char (char control-string index)))
      (declare (type character char))
      (when (char= char #\~)
	(setq index (discard-format-arglist
		      control-string (+ index 1) length))
	(case (char control-string index)
	  ((#\])
	   (decf nesting)
	   (when (zerop nesting)
	     (return index)))
	  ((#\[)
	   (incf nesting)))))))






;;;; Error Signalling




;;; The macro `error' takes the CLtL I version, with arguments similar to
;;; format.  Note that the evaluation of the arguments and the generation of the
;;; error string (using format) are wrapped within a with-permanent-area.  If we
;;; are actually signalling an error, I'd prefer that it be safe but possibly
;;; leaky.  Also note that this macro returns NIL to keep the value returning
;;; mechanism from going nutso, attempting the cast the void result of
;;; simple-error into whatever the required type is of this scope.  -jra 3/19/96

(defmacro error (control-string &rest args)
  (let ((arg-count (tli::length-trans args)))
    (case arg-count
      ((0)
       `(progn
	  (tli::tli-simple-error ,control-string)
	  nil))
      ((1)
       `(error-one-arg ,control-string ,@args))
      ((2)
       `(error-two-args ,control-string ,@args))
      ((3)
       `(error-three-args ,control-string ,@args))
      (t
       `(progn
	  (with-permanent-area ()
	    (tli::tli-simple-error
	      (format nil ,control-string ,@args)))
	  nil)))))

(defmacro cerror (continue-string control-string &rest args)
  (if (eval-feature :translator)
      `(error ,control-string ,@args)
      `(lisp:cerror ,continue-string ,control-string ,@args)))

(defun error-one-arg (control-string arg)
  (declare (return-type null))
  (with-permanent-area ()
    (tli::tli-simple-error
      (format nil control-string arg)))
  nil)

(defun error-two-args (control-string arg1 arg2)
  (declare (return-type null))
  (with-permanent-area ()
    (tli::tli-simple-error
      (format nil control-string arg1 arg2)))
  nil)

(defun error-three-args (control-string arg1 arg2 arg3)
  (declare (return-type null))
  (with-permanent-area ()
    (tli::tli-simple-error
      (format nil control-string arg1 arg2 arg3)))
  nil)




;;; The following functions signal errors for formatting and printing functions.
;;; They are here to pick up the macro definitions of error and format.

(defun bad-control-directive-error (control-string)
  (declare (return-type void))
  (error "This control string contained an ill-formed format directive: ~s"
	 control-string))

(defun unsupported-control-char-error (bad-char)
  (declare (return-type void))
  (error "The character ~s is not a supported format control character."
	 bad-char))

(defun bad-stream-error (stream-arg)
  (declare (return-type void))
  (error "The object ~s was not a valid stream argument." stream-arg))

(defun format-error (description control-string)
  (error "~a  The control string was ~s" description control-string))




;;; The function not-null-destructuring-error-1 is needed by the macroexpansion
;;; of tl:destructuring-bind-strict.

(defun not-null-destructuring-error-1 (shoulda-been-nil)
  (declare (return-type void))
  (error "The extra value ~a ran off the end of a destructuring-bind-strict pattern."
	 shoulda-been-nil))




;;; The function `check-make-array-dimensions' is needed by the macroexpansion
;;; of tl:make-array.

(defun check-make-array-dimensions (dimensions)
  (declare (return-type fixnum))
  (if (consp dimensions)
      (if (cdr (the cons dimensions))
	  (error "TL make-array does not support multiple-dimensions: ~a"
		 dimensions)
	  (setq dimensions (car (the cons dimensions)))))
  (if (not (fixnump dimensions))
      (error "TL make-array dimension argument was not an integer: ~a"
	     dimensions))
  dimensions)






;;;; Poor Man's Debugging Tools




;;; The following functions are designed for aiding use of C debuggers.

;;; The function `pm-print' takes a Lisp object and prin1's it to
;;; standard-output (i.e. printing escape characters), forcing output on the
;;; stream afterwards.

(defun pm-print (object)
  (declare (return-type t))
  (prin1 object)
  (terpri)
  (force-output)
  nil)
