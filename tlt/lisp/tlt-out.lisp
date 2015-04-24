(in-package "TLI")

;;;; Module TL-OUT

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






;;;; Primitive Operations for Output to Standard Output




;;; This module implements printing operations that have direct translations
;;; into C.

(def-c-translation princ-fixnum-to-file (fixnum width file-stream)
  ((lisp-specs :ftype ((fixnum fixnum file-stream) void))
   (let ((fix (gensym))
	 (wid (gensym))
	 (stream (gensym)))
     `(let ((,fix ,fixnum)
	    (,wid ,width)
	    (,stream ,file-stream))
	(format ,stream "~Vd" ,wid ,fix))))
  ((trans-specs :c-type ((sint32 sint32 obj) void))
   (make-c-function-call-expr
     (make-c-name-expr "fprintf")
     (list (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer file-strm) file-stream)
	     "output")
	   (make-c-literal-expr "%*ld")
	   (make-c-cast-expr 'int width)
	   (make-c-cast-expr 'long fixnum)))))

(def-c-translation write-fixnum-to-string (fixnum width string)
  ((lisp-specs :ftype ((fixnum fixnum string) void))
   (let ((fix (gensym))
	 (wid (gensym))
	 (str (gensym)))
     `(let ((,fix ,fixnum)
	    (,wid ,width)
	    (,str ,string))
	(format ,str "~Vd" ,wid ,fix))))
  ((trans-specs :c-type ((sint32 sint32 (pointer str)) void))
   (make-c-function-call-expr
     (make-c-name-expr "write_fixnum_into_str")
     (list fixnum width string))))

(def-c-translation princ-double-to-file (double width file-stream)
  ((lisp-specs :ftype ((double-float fixnum file-stream) void))
   (let ((float (gensym))
	 (wid (gensym))
	 (stream (gensym)))
     `(let ((,float ,double)
	    (,wid ,width)
	    (,stream ,file-stream))
	(format ,stream "~Vg" ,wid ,float))))
  ((trans-specs :c-type ((double sint32 obj) void))
   (make-c-function-call-expr
     (make-c-name-expr "fprintf")
     (list (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer file-strm) file-stream)
	     "output")
	   (make-c-literal-expr "%*g")
	   (make-c-cast-expr 'int width)
	   double))))

(def-c-translation write-double-to-string (double width string)
  ((lisp-specs :ftype ((double-float fixnum string) void))
   (let ((float (gensym))
	 (wid (gensym))
	 (str (gensym)))
     `(let ((,float ,double)
	    (,wid ,width)
	    (,str ,string))
	(format ,str "~Vg" ,wid ,float))))
  ((trans-specs :c-type ((double sint32 (pointer str)) void))
   (make-c-function-call-expr
     (make-c-name-expr "write_double_into_str")
     (list double width string))))

(def-c-translation tl:write-char-to-file-stream (character file-stream)
  ((lisp-specs :ftype ((character file-stream) void))
   `(write-char ,character ,file-stream))
  ((trans-specs :c-type ((unsigned-char obj) void))
   (make-c-function-call-expr
     (make-c-name-expr "putc")
     (list (make-c-cast-expr 'char character)
	   (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer file-strm) file-stream)
	     "output")))))

(def-c-translation write-byte-to-file (byte file-stream)
  ((lisp-specs :ftype ((fixnum file-stream) void))
   `(write-byte ,byte ,file-stream))
  ((trans-specs :c-type ((unsigned-char obj) void))
   (make-c-function-call-expr
     (make-c-name-expr "putc")
     (list (make-c-cast-expr 'char byte)
	   (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer file-strm) file-stream)
	     "output")))))

(def-c-translation tl:write-string-to-file-stream (string file-stream)
  ((lisp-specs :ftype ((string file-stream) void))
   `(write-string ,string ,file-stream))
  ((trans-specs :c-type (((pointer unsigned-char) obj) void))
   (make-c-function-call-expr
     (make-c-name-expr "fputs")
     (list (make-c-cast-expr '(pointer char) string)
	   (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer file-strm) file-stream)
	     "output")))))

(def-c-translation force-output-to-file (file-stream)
  ((lisp-specs :ftype ((file-stream) void))
   `(force-output ,file-stream))
  ((trans-specs :c-type ((obj) void))
   (make-c-function-call-expr
     (make-c-name-expr "fflush")
     (list (make-c-indirect-selection-expr
	     (make-c-cast-expr '(pointer file-strm) file-stream)
	     "output")))))




;;; The CMU Lisp compiler continually finds it necessary to brag when it finds
;;; and eliminates dead code.  To inhibit these noise warnings, I've added this
;;; variable which makes it uncertain whether or not ERROR will actually get
;;; called.  This leaves enough doubt for the compiler to be quiet.  -jallard
;;; 5/28/99

;;; The macro derror is used in places were we want to invoke error, but
;;; we want the following code to remain alive when compiling in lisp
;;; because that code is alive in C and we want the lisp compiler's opinion
;;; on it.  There are two tricks in this macro, one is to guard the call
;;; to error so the CMU lisp compiler can't decern that it is always
;;; called, and the other is to return a datum of unknown type so that
;;; the compiler will proceed to investigate all alternatives down stream
;;; from this call. - ben 6/2/99

(defmacro derror (&rest args)
  `(lisp-translation-error ,@args))

(def-c-translation tli-simple-error (string)
  ((lisp-specs :ftype ((string) void))
   `(derror "~A" ,string))
  ((trans-specs :c-type (((pointer unsigned-char)) void))
   (make-c-function-call-expr
     (make-c-name-expr "error")
     (list (make-c-cast-expr '(pointer char) string)))))








;;;; Primitive Operations for Input from File-Streams




(tl:declaim (tl:functional c-eof-value))

(def-c-translation c-eof-value ()
  ((lisp-specs :ftype (() fixnum))
   -1)
  ((trans-specs :c-type (() sint32))
   (coerce-c-expr-result-to-type
     (make-c-name-expr "EOF")
     'int 'sint32 (l-expr-env function-call-l-expr))))

(def-c-translation file-stream-eof-p (file-stream)
  ((lisp-specs :ftype ((file-stream) t))
   `(not (peek-char nil ,file-stream nil nil)))
  ((trans-specs :c-type ((obj) boolean))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "feof")
       (list (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer file-strm) file-stream)
	       "input")))
     'int 'boolean (l-expr-env function-call-l-expr))))

;; The EOF/ERROR result may differ between LISP and C versions
;;    Lisp returns -1, Translated C version returns EOF
;;   This is OK if always check for EOF by (= returned-value (c-eof-value))
;;       since in LISP this will be -1, and in translated C it will be EOF.

(def-c-translation call-getc-on-file-stream (file-stream)
  ((lisp-specs :ftype ((file-stream) fixnum))
   `(let ((file-stream ,file-stream)
	  read-result)
      (cond ((equal (stream-element-type file-stream)
		    '(unsigned-byte 8))	; binary mode
	     (setf read-result
		    (read-byte file-stream nil nil))
	     (if read-result
		 read-result
		 -1))
	    (t				; character mode
	     (setf read-result
		   (read-char file-stream nil nil))
	     (if read-result
		 (char-code read-result)
		 -1)))))
  ((trans-specs :c-type ((obj) sint32))
   (coerce-c-expr-result-to-type 
     (make-c-function-call-expr
       (make-c-name-expr "getc")
       (list (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer file-strm) file-stream)
	       "input")))
     'int 'sint32 (l-expr-env function-call-l-expr))))






;;;  Low level block input from file-streams
;;;    WARNING: these routines DO NOT CHECK bounds for writing
;;;       These checks must be done by calling routines!!
;;;    `fread-from-file-stream-into-byte-array' and
;;;    `fread-from-file-stream-into-string' both call fread
;;;    on a file-stream.  They read a block of bytes (chars) into
;;;    a byte-array (string), respectively, starting at a specified
;;;    index.  The returned result is just the number of bytes (chars)
;;;    that were read (a count of 0 means error or EOF -- call feof
;;;    to determine which).  NOTE that the returned value IS NOT the
;;;    bounding index of the byte-array (string), rather it is the count
;;;    of bytes read (or 0 an if error was encountered).

(def-c-translation fread-from-file-stream-into-byte-array
    (byte-array array-start count file-stream)
  ((lisp-specs :ftype (((array (unsigned-byte 8)) fixnum fixnum file-stream)
		       fixnum))
   ;; Would use ANSI-CL  READ-SEQUENCE, but is not defined in Lucid
   `(loop with byte-array = ,byte-array
	  with array-start = ,array-start
	  with count = ,count
	  with file-stream = ,file-stream
	  with binary-mode? = (equal (stream-element-type file-stream)
				     '(unsigned-byte 8))
	  for array-index from array-start
	  for byte-count from 0 below count
	  for read-result = (if binary-mode?
				(read-byte file-stream nil nil)
				(read-char file-stream nil nil))
	  while read-result
	  do
      (setf (aref byte-array array-index)
	    (if binary-mode?
		read-result
		(char-code read-result)))
	  finally
	    (return byte-count)))
  ((trans-specs :c-type (((array uint8) sint32 sint32 obj) sint32))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "fread")
       (list (make-c-cast-expr '(pointer void)
			       (make-c-infix-expr byte-array
						  "+"
						  array-start))
	     (make-c-cast-expr 'size-t (make-c-literal-expr 1))
	     (make-c-cast-expr 'size-t count)
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer file-strm) file-stream)
	       "input")))
     'size-t 'sint32 (l-expr-env function-call-l-expr))))

(def-c-translation fread-from-file-stream-into-string
    (string string-start count file-stream)
  ((lisp-specs :ftype ((string fixnum fixnum file-stream)
		       fixnum))
   ;; Would use ANSI-CL  READ-SEQUENCE, but is not defined in Lucid
   `(loop with string = ,string
	  with string-start = ,string-start
	  with count = ,count
	  with file-stream = ,file-stream
	  with binary-mode? = (equal (stream-element-type file-stream)
				     '(unsigned-byte 8))
	  for string-index from string-start
	  for byte-count from 0 below count
	  for read-result = (if binary-mode?
				(read-byte file-stream nil nil)
				(read-char file-stream nil nil))
	  while read-result
	  do
      (setf (aref string string-index)
	    (if binary-mode?
		(code-char read-result)
		read-result))
	  finally
	    (return byte-count)))
  ((trans-specs :c-type (((pointer unsigned-char) sint32 sint32 obj) sint32))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "fread")
       (list (make-c-cast-expr '(pointer void)
			       (make-c-infix-expr string
						  "+"
						  string-start))
	     (make-c-cast-expr 'size-t (make-c-literal-expr 1))
	     (make-c-cast-expr 'size-t count)
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer file-strm) file-stream)
	       "input")))
     'size-t 'sint32 (l-expr-env function-call-l-expr))))


(def-c-translation call-strlen (string)
  ((lisp-specs :ftype ((string) fixnum))
   `(loop with string = ,string
	  for char-index from 0 below (array-dimension string 0)
	  when (char= (char string char-index) #\null)
	    return char-index
	  finally
	    (error "In call-strlen, no #\null character found in string ~s"
		   string)))
  ((trans-specs :c-type (((pointer unsigned-char)) sint32))
   (coerce-c-expr-result-to-type
     (make-c-function-call-expr
       (make-c-name-expr "strlen")
       (list (coerce-c-expr-result-to-type
	       string '(pointer unsigned-char) '(pointer char)
	       (l-expr-env function-call-l-expr))))
     'size-t 'sint32 (l-expr-env function-call-l-expr))))

(def-c-translation call-fgets-on-file-stream (string-buffer count file-stream)
  ((lisp-specs :ftype ((string fixnum file-stream) string))
   (let ((string-buffer-var (gensym))
	 (count-var (gensym))
	 (file-stream-var (gensym)))
     `(loop with ,string-buffer-var = ,string-buffer
	    with ,count-var = ,count
	    with ,file-stream-var = ,file-stream
	    for index from 0 below (1- ,count-var)
	    for next-char = (read-char ,file-stream-var nil nil)
	    do
	(cond ((null next-char)
	       (return (cond ((zerop index)
			      nil)
			     ((setf (char ,string-buffer-var index) #\null)
			      ,string-buffer-var))))
	      ((char= next-char #\newline)
	       (setf (char ,string-buffer-var index)
		     next-char)
	       (setf (char ,string-buffer-var (1+ index))
		     #\null)
	       (return ,string-buffer-var))
	      (t
	       (setf (char ,string-buffer-var index)
		     next-char)))
	    finally
	      (setf (char ,string-buffer-var (1+ index))
		     #\null)
	      (return ,string-buffer-var))))
  ((trans-specs :c-type (((pointer unsigned-char) sint32 obj)
			 (pointer unsigned-char)))
   (coerce-c-expr-result-to-type 
     (make-c-function-call-expr
       (make-c-name-expr "fgets")
       (list (coerce-c-expr-result-to-type
	       string-buffer '(pointer unsigned-char) '(pointer char)
	       (l-expr-env function-call-l-expr))
	     (coerce-c-expr-result-to-type count 'sint32 'int
					   (l-expr-env function-call-l-expr))
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer file-strm) file-stream)
	       "input")))
     '(pointer char) '(pointer unsigned-char) (l-expr-env function-call-l-expr))))

(def-c-translation null-pointer? (pointer-obj)
  ((lisp-specs :ftype ((t) t))
   `(null ,pointer-obj))
  ((trans-specs :c-type (((pointer unsigned-char)) boolean))
   (make-c-infix-expr
     pointer-obj
     "=="
     (make-c-name-expr "NULL"))))





;;; Opening Files


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Mapping decode-mode uses to get lisp open keywords to mimic fopen
;;      (r, w, a must be first char, but b and + are optional/interchangeable)
;;
;;                 direction       if-exists       if-does-not-exist
;;       r           :input             ----                   NIL
;;      w           :output       :supersede          :create
;;      a            :output       :append             :create
;;      r+         :output        :overwrite         NIL
;;
;;      b  (binary)   -->  :element-type '(unsigned-byte 8)
;;     no b (text)     -->  :element-type 'character
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun decode-mode (mode-string)
  (let* ((mode-string-length (length mode-string))
	 (first-char (char mode-string 0))
	 plus?
	 binary?)
    (when (> mode-string-length 1)
      (case (char mode-string 1)
	(#\b (setf binary? t))
	(#\+ (setf plus? t))))
    (when (> mode-string-length 2)
      (case (char mode-string 2)
	(#\b (setf binary? t))
	(#\+ (setf plus? t))))
    (append
      (case first-char
	(#\r
	 (if plus?
	     '(:direction :output :if-exists :overwrite :if-does-not-exist nil)
	     '(:direction :input :if-does-not-exist nil)))
	(#\w
	 '(:direction :output :if-exists :supersede :if-does-not-exist :create))
	(#\a
	 '(:direction :output :if-exists :append :if-does-not-exist :create)))
      (if binary?
	  '(:element-type '(unsigned-byte 8))
	  '(:element-type 'string-char)))))

(def-c-translation call-fopen (filename mode)
  ((lisp-specs :ftype ((string string) file-stream))
   `(open ,filename ,@(decode-mode mode)))
  ((trans-specs :c-type (((pointer unsigned-char)(pointer unsigned-char))
			 obj))
   (let ((temp (reusable-c-variable-identifier
		 'temp c-func '(pointer file)
		 (l-expr-env function-call-l-expr))))
     (emit-expr-to-compound-statement
       (make-c-infix-expr
	 (make-c-name-expr temp)
	 "="
	 (make-c-function-call-expr
	   (make-c-name-expr "fopen")
	   (list (coerce-c-expr-result-to-type
		   filename '(pointer unsigned-char) '(pointer char)
		   (l-expr-env function-call-l-expr))
		 (coerce-c-expr-result-to-type
		   mode '(pointer unsigned-char) '(pointer char)
		   (l-expr-env function-call-l-expr)))))
       c-compound-statement)
     (make-c-conditional-expr 
       (make-c-infix-expr temp
			  "=="
			  "NULL")
       (make-c-cast-expr 'obj
			 (make-c-name-expr "NULL"))
       (make-c-function-call-expr
	 (make-c-name-expr "alloc_file_strm")
	 (list (make-c-name-expr temp)
	       (make-c-name-expr temp)
	       (coerce-c-expr-result-to-type
		 filename '(pointer unsigned-char) '(pointer char)
		 (l-expr-env function-call-l-expr))
	       (coerce-c-expr-result-to-type
		 mode '(pointer unsigned-char) '(pointer char)
		 (l-expr-env function-call-l-expr))
	       (make-c-literal-expr
		 (region-number-for-type-and-area
		   'file-stream
		   (declared-area-name
		     (l-expr-env function-call-l-expr)
		     'file-stream)))
	       (make-c-literal-expr (c-type-tag 'file-strm))))))))

(def-c-translation call-fclose (file-stream abort)
  ((lisp-specs :ftype ((file-stream t) fixnum))
   (let ((file-stream-var (gensym))
	 (abort-var (gensym)))
     `(let* ((,file-stream-var ,file-stream)
	     (,abort-var ,abort))
	(close ,file-stream-var :abort ,abort-var)
	(when ,abort-var
	  (delete-file (truename ,file-stream-var)))
	0)))
  ((trans-specs :c-type ((obj boolean) sint32))
   ;;; should this try to recycle / reclaim the file-stream ??
   (let ((temp (reusable-c-variable-identifier
		 'temp c-func 'sint32 (l-expr-env function-call-l-expr))))
     (emit-expr-to-compound-statement
       (make-c-infix-expr
	 (make-c-name-expr temp)
	 "="
	 (coerce-c-expr-result-to-type
	   (make-c-function-call-expr
	     (make-c-name-expr "fclose")
	     (list (make-c-indirect-selection-expr
		     (make-c-cast-expr '(pointer file-strm)
				       file-stream)
		     "input")))  ; input and output are always the same
	   'int 'sint32 (l-expr-env function-call-l-expr)))
       c-compound-statement)
     (register-needed-function-extern
       (c-func-c-file c-func) '("extern")
       'sint32 "delete_named_file" '((pointer char)))
     (emit-statement-to-compound-statement
       (make-c-conditional-statement
	 (list 
	   abort 
	   (make-c-expr-statement
	     (make-c-function-call-expr
	       (make-c-name-expr "delete_named_file")
	       (list (make-c-indirect-selection-expr
		       (make-c-cast-expr '(pointer file-strm)
					 file-stream)
		       "filename"))))))
       c-compound-statement)
     (make-c-name-expr temp))))

(def-c-translation delete-named-file (filename)
  ((lisp-specs :ftype ((string) fixnum))
   `(progn (ab-lisp::delete-file ,filename) 0))
  ((trans-specs :c-type (((pointer unsigned-char)) sint32))
   (register-needed-function-extern
     (c-func-c-file c-func) '("extern")
     'sint32 "delete_named_file" '((pointer char)))
   (make-c-function-call-expr
       (make-c-name-expr "delete_named_file")
       (list (coerce-c-expr-result-to-type
	       filename '(pointer unsigned-char)'(pointer char)
	       (l-expr-env function-call-l-expr))))))
