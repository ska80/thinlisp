(in-package "TL")

;;;; Module INPUT

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

;;; Author: Glenn Iba






;;;; Input and Supporting Stream Operations




;;; This module implements input functions for file-streams and string-streams.




(defmacro eof-value? (value)
  `(= ,value (tli::c-eof-value)))

(defun analyze-file-stream-error (file-stream eof-error-p eof-value)
  (declare (type file-stream file-stream)
	   (return-type t))
  (if (tli::file-stream-eof-p file-stream)
      (error-or-value file-stream eof-error-p eof-value)
      (error "Non-EOF error while reading from stream ~s" file-stream)))

(defun error-or-value (stream eof-error-p eof-value)
  (declare (return-type t))
  (if eof-error-p
      (error "End of File  error while reading from stream ~s"
	     stream)
      eof-value))

(defmacro read-char-from-file-stream (&optional (file-stream '*standard-input*)
						(eof-error-p t)
						eof-value)
  (let ((stream-var (gensym))
	(getc-val-var (gensym))
	(eof-error-p-var (gensym))
	(eof-value-var (gensym)))
    `(let* ((,stream-var ,file-stream)
	    (,getc-val-var (tli::call-getc-on-file-stream ,stream-var))
	    (,eof-error-p-var ,eof-error-p)
	    (,eof-value-var ,eof-value))
       (declare (type file-stream ,stream-var)
		(type fixnum ,getc-val-var))
       (if (eof-value? ,getc-val-var)
	   (analyze-file-stream-error ,stream-var
				      ,eof-error-p-var
				      ,eof-value-var)
	   (code-char ,getc-val-var)))))

(defmacro read-byte-from-file-stream (&optional (file-stream '*standard-input*)
						(eof-error-p t)
						eof-value)
  (let ((stream-var (gensym))
	(getc-val-var (gensym))
	(eof-error-p-var (gensym))
	(eof-value-var (gensym)))
    `(let* ((,stream-var ,file-stream)

	    (,getc-val-var (tli::call-getc-on-file-stream ,stream-var))
	    (,eof-error-p-var ,eof-error-p)
	    (,eof-value-var ,eof-value))
       (declare (type file-stream ,stream-var)
		(type fixnum ,getc-val-var))
       (if (eof-value? ,getc-val-var)
	   (analyze-file-stream-error ,stream-var
				      ,eof-error-p-var
				      ,eof-value-var)
	   ,getc-val-var))))

(defmacro read-char-from-string-stream (&optional
					 (string-stream '*standard-input*)
					 (eof-error-p t)
					 eof-value)
  (let ((stream-var (gensym))
	(index-var (gensym))
	(eof-error-p-var (gensym))
	(eof-value-var (gensym)))
    `(let* ((,stream-var ,string-stream)
	    (,index-var (tli::string-stream-input-index ,stream-var))
	    (,eof-error-p-var ,eof-error-p)
	    (,eof-value-var ,eof-value))
       (declare (type string-stream ,stream-var)
		(type fixnum ,index-var))
       (cond ((< ,index-var (tli::string-stream-input-index-bounds ,stream-var))
	      (setf (tli::string-stream-input-index ,stream-var)
		    (the fixnum (1+ ,index-var)))
	      (char (tli::string-stream-input-string ,stream-var)
		    ,index-var))
	     (t ;; end of file
	      ;; can try to simplify if can prove eof-error-p is nil (then just plug in eof-value)
	      (error-or-value ,stream-var
			      ,eof-error-p-var
			      ,eof-value-var))))))

(defmacro read-byte (&optional (file-stream '*standard-input*)
			       (eof-error-p t)
			       eof-value
			       recursive-p)
  (when (and recursive-p
	     (not (or (constantp recursive-p) (symbolp recursive-p))))
    (error "READ-BYTE doesn't support non-null recursive arguments: ~s"
	   recursive-p))
  `(read-byte-from-file-stream ,file-stream ,eof-error-p ,eof-value))

(defmacro read-char (&optional (stream '*standard-input*)
			       (eof-error-p t)
			       eof-value
			       recursive-p
			       &environment env)
  (when (and recursive-p
	     (not (or (constantp recursive-p) (symbolp recursive-p))))
    (error "READ-CHAR doesn't support non-null recursive arguments: ~s"
	   recursive-p))
  (setf stream (macroexpand stream env))
  (let ((stream-type (tli::expression-result-type stream env)))
    (cond
      ((subtypep stream-type 'file-stream)
       `(read-char-from-file-stream ,stream ,eof-error-p ,eof-value))
      ((subtypep stream-type 'string-stream)
       `(read-char-from-string-stream ,stream ,eof-error-p ,eof-value))
      (t `(generic-read-char ,stream ,eof-error-p ,eof-value)))))

(defun generic-read-char (stream eof-error-p eof-value)
  (declare (return-type t))
  (typecase stream
    (file-stream
     (read-char-from-file-stream stream eof-error-p eof-value))
    (string-stream
     (read-char-from-string-stream stream eof-error-p eof-value))
    (t
     (error "Stream ~s was neither a file-stream ~
                              nor a string-stream in generic-read-char"
	    stream))))


(defparameter *input-string-buffer*
  (make-string (expt 2 14)))

(defparameter *input-string-buffer-size*
  ;; Ensure last character always stays Null, so can safely call strlen().
  (1- (expt 2 14)))


(defun read-line-from-file-stream (&optional (file-stream *standard-input*)
					     (eof-error-p t)
					     eof-value)
  (declare (type file-stream file-stream))
  (let ((fgets-result (tli::call-fgets-on-file-stream
			(the string *input-string-buffer*)
			(the fixnum *input-string-buffer-size*)
			file-stream)))
    (declare (type (or null string) fgets-result))
    (cond ((tli::null-pointer? fgets-result)
	   (values (analyze-file-stream-error file-stream
					      eof-error-p
					      eof-value)
		   t))			; yes, we're missing a new-line
	  (t
	   (let ((strlen-value (tli::call-strlen *input-string-buffer*))
		 (missing-new-line? t))
	     (declare (type fixnum strlen-value))
	     (cond ((> strlen-value
		       (the fixnum *input-string-buffer-size*))
		    (error "Input buffer overflow reading from ~s"
			   file-stream))
		   ((and (> strlen-value 0)
			 (char= (char *input-string-buffer*
				      (the fixnum (1- strlen-value)))
				#\newline))
		    (setf missing-new-line? nil)
		    (setf strlen-value (the fixnum (1- strlen-value)))
		    ;(setf (char *input-string-buffer* strlen-value) #\null)
		    ;   Setting the fill-pointer should now take care of this
		    ))
	     (setf (fill-pointer (the string *input-string-buffer*))
		   strlen-value)
	     (values *input-string-buffer* missing-new-line?))))))


(defun read-line-from-string-stream (&optional (string-stream *standard-input*)
					       (eof-error-p t)
					       eof-value)
  (declare (type string-stream string-stream))
  (let ((input-string (tli::string-stream-input-string string-stream))
	(final-result *input-string-buffer*)
	(input-index-bounds (tli::string-stream-input-index-bounds
			      string-stream))
	(next-char #\null)
	(missing-newline? t))
    (declare (type string final-result)
	     (type character next-char)
	     (type fixnum input-index-bounds))
    (do ((input-index (tli::string-stream-input-index string-stream)
		      (1+ input-index))
	 (buffer-index 0 (1+ buffer-index)))
	((not (and (< input-index input-index-bounds)
		   (< buffer-index (the fixnum *input-string-buffer-size*))
		   missing-newline?))
	 (cond ((not missing-newline?)
		(setf buffer-index
		      (1- buffer-index)))  ; backup past newline
	       ((zerop buffer-index)
		;; No characters read, so EOF must be first thing read
		(setf final-result
		      (error-or-value string-stream
				      eof-error-p
				      eof-value)))
	       ((and (= buffer-index
			(the fixnum *input-string-buffer-size*))
		     (not (= input-index input-index-bounds)))
		(error "Input buffer overflow reading from ~s"
		       string-stream)))
	 (setf (tli::string-stream-input-index string-stream) input-index
	       ; Setting the fill-pointer should now take care of this
	       ; (char *input-string-buffer* buffer-index) #\null
	       (fill-pointer (the string *input-string-buffer*)) buffer-index)
	 (values final-result missing-newline?))
      (declare (type fixnum input-index buffer-index))
      (setf next-char (char input-string input-index))
      (setf (char *input-string-buffer* buffer-index) next-char
	    missing-newline? (char/= next-char #\newline)))))

(defmacro read-line (&optional (stream '*standard-input*)
			       (eof-error-p t)
			       eof-value
			       &environment env)
  (setf stream (macroexpand stream env))
  (let ((stream-type (tli::expression-result-type stream env)))
    (cond
      ((subtypep stream-type 'file-stream)
       `(read-line-from-file-stream ,stream ,eof-error-p ,eof-value))
      ((subtypep stream-type 'string-stream)
       `(read-line-from-string-stream ,stream ,eof-error-p ,eof-value))
      (t `(generic-read-line ,stream ,eof-error-p ,eof-value)))))

(defun generic-read-line (stream eof-error-p eof-value)
  (typecase stream
    (file-stream
     (read-line-from-file-stream stream eof-error-p eof-value))
    (string-stream
     (read-line-from-string-stream stream eof-error-p eof-value))
    (t
     (error "Stream ~s was neither a file-stream ~
                              nor a string-stream in generic-read-line"
	    stream))))







;;;; FILE Input and Output




;;; This implementation of `probe-file' tests whether or not a file can be opened
;;;  for input.  The returned value should be considered a boolean.  In particular,
;;;  it does not return the truename of the file.  A nil result does not guarantee
;;;  that the file does not exist, only that it couldn't be opened for some reason.

(defmacro probe-file (filename)
  (let ((filename-var (gensym)))
    `(let* ((,filename-var ,filename)
	    (file-stream (tli::call-fopen ,filename-var "r")))
       (when file-stream
	 (tli::call-fclose file-stream nil))
       (if file-stream
	   ,filename-var
	   nil))))

(defmacro close-file-stream (file-stream &key abort)
  (let ((file-stream-var (gensym))
	(abort-var (gensym)))
    `(let ((,file-stream-var ,file-stream)
	   (,abort-var ,abort))
       (unless (or (null ,file-stream-var)
		   (eq ,file-stream-var *terminal-io*))
	 (tli::call-fclose ,file-stream-var ,abort-var))
       t)))

(defmacro delete-file (filename)
  `(progn (tli::delete-named-file ,filename)
	  t))

(defun create-file (filename &optional binary?)
  (declare (type string filename)
	   (consing-area either))
  (if binary?
      (close-file-stream (tli::call-fopen filename "ab"))
      (close-file-stream (tli::call-fopen filename "a"))))

(defun-for-macro binary-mode? (element-type)
  (cond ((subtypep element-type 'character)
	 nil)
	((subtypep element-type '(unsigned-byte 8))
	 t)
	(t (error "Element-type ~s was neither a subtype of character nor (unsigned-byte 8)"
		  element-type))))

(defmacro open (&whole form
		       filename &key
		       (direction :input)
		       (element-type ''character)
		       (if-exists :error)
		       (if-does-not-exist :unsupplied))
  (unless (and (constantp direction)
	       (constantp element-type)
	       (constantp if-exists)
	       (constantp if-does-not-exist))
    (error "OPEN needs constant valued arguments to direction, element-type, ~
                           if-exists, and if-does-not-exist. ~%    Form = ~s"
	   form))
  (setf direction (eval direction)
	element-type (eval element-type)
	if-exists (eval if-exists)
	if-does-not-exist (eval if-does-not-exist))
  (when (eq if-does-not-exist :unsupplied)
    (setf if-does-not-exist
	  (case direction
	    (:input :error)
	    ((:output :io)  ; we don't support :io yet, but in case we ever do
	     (case if-exists
	       ((:append :overwrite) :error)
	       (t :create)))
	    (t nil))))
  (case direction
    (:input
     (if (binary-mode? element-type)
	 `(open-for-binary-input ,filename ,if-does-not-exist)
	 `(open-for-text-input ,filename ,if-does-not-exist)))
    (:output
     (if (binary-mode? element-type)
	 `(open-for-binary-output ,filename ,if-exists ,if-does-not-exist)
	 `(open-for-text-output ,filename ,if-exists ,if-does-not-exist)))
    (t (error "Invalid value for :DIRECTION keyword.  Form = ~s" form))))

(defun open-for-binary-input (filename if-does-not-exist)
  (declare (type string filename)
	   (consing-area either)
	   (return-type t))
  (case if-does-not-exist
    ((nil)
     (tli::call-fopen filename "rb"))
    (:error
     (let ((file-stream (tli::call-fopen filename "rb")))
       (if file-stream
	   file-stream
	   (error "File named ~s could not be opened for input, perhaps does not exist"
		  filename))))
    (:create
     (let ((file-stream (tli::call-fopen filename "rb")))
       (cond (file-stream file-stream)
	     (t (create-file filename t)
		(tli::call-fopen filename "rb")))))))

(defun open-for-text-input (filename if-does-not-exist)
  (declare (type string filename)
	   (consing-area either)
	   (return-type t))
  (case if-does-not-exist
    ((nil)
     (tli::call-fopen filename "r"))
    (:error
     (let ((file-stream (tli::call-fopen filename "r")))
       (if file-stream
	   file-stream
	   (error "File named ~s could not be opened for input, perhaps does not exist"
		  filename))))
    (:create
     (let ((file-stream (tli::call-fopen filename "r")))
       (cond (file-stream file-stream)
	     (t (create-file filename nil)
		(tli::call-fopen filename "r")))))
    (t (error "Unrecognized value ~s for keyword :IF-DOES-NOT-EXIST"
	      if-does-not-exist))))

(defun open-for-binary-output (filename if-exists if-does-not-exist)
  (declare (type string filename)
	   (consing-area either)
	   (return-type t))
  (case if-does-not-exist
    (:create   ; don't need to check existence for this (except for :overwrite)
     (case if-exists
       ((:error nil)			; will have to check after all
	(let ((file-stream (probe-file filename)))
	  (cond (file-stream
		 (case if-exists
		   (:error (error "File ~s already exists" filename))
		   (t nil)))
		(t (tli::call-fopen filename "ab")))))
       (:overwrite
	;; Mode "rb+" opens for "update" without truncating
	;;  (An alternative might be to open for append "a" and reset
	;;      file-pointer to start of file)
	(let ((file-stream (tli::call-fopen filename "rb+")))
	  (cond (file-stream)
		(t (create-file filename t)
		   (tli::call-fopen filename "rb+")))))
       (:supersede
	(tli::call-fopen filename "wb"))
       (:append 
	(tli::call-fopen filename "ab"))
       (t (error "While attempting to open file ~s for output~%~
                                  unrecognized :IF-EXISTS keyword value ~s"
		 filename
		 if-exists))))
    ((:error nil)			; will need to check if exists for these
     (let ((file-stream (probe-file filename)))
       (cond (file-stream		; file exists
	      (case if-exists
		(:error
		 (error "File ~s already exists" filename))
		((nil) nil)
		(:overwrite
		 (tli::call-fopen filename "rb+"))
		(:supersede
		 (tli::call-fopen filename "wb"))
		(:append
		 (tli::call-fopen filename "ab"))
		(t (error "While attempting to open file ~s for output~%~
                                    unrecognized :IF-EXISTS keyword value ~s"
			  filename
			  if-exists))))
	     (t			;file does not exist
	      (case if-does-not-exist
		(:error
		 (error "File ~s does not exist" filename))
		(t nil))))))))

(defun open-for-text-output (filename if-exists if-does-not-exist)
  (declare (type string filename)
	   (consing-area either)
	   (return-type t))
  (case if-does-not-exist
    (:create				; don't need to check existence for this
     (case if-exists
       ((:error nil)			; will have to check after all
	(let ((file-stream (probe-file filename)))
	  (cond (file-stream
		 (case if-exists
		   (:error (error "File ~s already exists" filename))
		   (t nil)))
		(t (tli::call-fopen filename "a")))))
       (:overwrite
	(let ((file-stream (tli::call-fopen filename "r+")))
	  (cond (file-stream)
		(t (create-file filename nil)
		   (tli::call-fopen filename "r+")))))
       (:supersede
	(tli::call-fopen filename "w"))
       (:append 
	(tli::call-fopen filename "a"))
       (t (error "While attempting to open file ~s for output~%~
                                  unrecognized :IF-EXISTS keyword value ~s"
		 filename
		 if-exists))))
    ((:error nil)			; will need to check if exists for these
     (let ((file-stream (probe-file filename)))
       (cond (file-stream		; file exists
	      (case if-exists
		(:error
		 (error "File ~s already exists" filename))
		((nil) nil)
		(:overwrite
		 (tli::call-fopen filename "r+"))
		(:supersede
		 (tli::call-fopen filename "w"))
		(:append
		 (tli::call-fopen filename "a"))
		(t (error "While attempting to open file ~s for output~%~
                                    unrecognized :IF-EXISTS keyword value ~s"
			  filename
			  if-exists))))
	     (t			;file does not exist
	      (case if-does-not-exist
		(:error
		 (error "File ~s does not exist" filename))
		(t nil))))))))







;;;; Stream operations




(defmacro close-string-stream (stream &key abort)
  `(progn
     ,stream  ;; eval arg
     ,abort   ;; eval arg
     t))

(defmacro close (stream &key abort &environment env)
  (setf stream (macroexpand stream env))
  (let ((stream-type (tli::expression-result-type stream env)))
    (cond
      ((subtypep stream-type 'file-stream)
       `(close-file-stream ,stream :abort ,abort))
      ((subtypep stream-type 'string-stream)
       `(close-string-stream ,stream :abort ,abort))
      (t `(generic-close ,stream ,abort)))))

(defun generic-close (stream abort)
  (declare (return-type t))
  (typecase stream
    (null)
    (file-stream
     (close-file-stream stream :abort abort))
    (string-stream
     (close-string-stream stream :abort abort))
    (t
     (error "Stream ~s was neither a file-stream ~
                              nor a string-stream in generic-close"
	    stream)))
  t)

(defmacro with-open-stream ((var stream) &rest decls-and-body)
  (multiple-value-bind (decls body)
      (split-declarations-and-body decls-and-body)
    (let ((normal-exit? (gensym)))
      `(let ((,normal-exit? nil)
	     (,var ,stream))
	 (declare (allow-unwind-protect))
	 ,@decls
	 (unwind-protect
	      (multiple-value-prog1 (progn ,@body)
		(setq ,normal-exit? t))
	   (close ,var :abort (not ,normal-exit?)))))))

(defmacro with-open-file ((var filespec &rest options) &rest decls-and-body)
  `(with-open-stream (,var (open ,filespec ,@options))
     ,@decls-and-body))






;;;; Read




;;; TL does not currently have a full read system with readtables and the like.
;;; Instead we are trying to get by on the simplest stuff possible.  The
;;; following function reads fixnums, double-floats, symbols, and strings but no
;;; other types of input.

(defmacro read-from-string (string &optional (eof-error-p t) eof-value
				   &key (start 0) (end nil))
  (if (eval-feature :translator)
      `(read-from-string-1 ,string ,eof-error-p ,eof-value ,start ,end)
      `(lisp:read-from-string ,string ,eof-error-p ,eof-value :start ,start :end ,end)))

(defun read-from-string-1 (string eof-error-p eof-value start end)
  (declare (ignore string eof-error-p start end)
	   (return-type (values t fixnum)))
  (values eof-value 0))

