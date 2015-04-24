(in-package "TLI")

;;;; Module C-DECL

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






;;;; C Declarations




;;; This module implements structures and operations on C declarations.

;;; The `c-decl' structure represents a C declaration.  There are three subtypes
;;; of declarations: c-typedef-decl, c-function-decl, and c-var-decl.  The only
;;; operation on c-decls is emit-declaration-to-c-file, declared later in this
;;; file.

(defstruct (c-decl)
  )




;;; The macro `def-c-decl' defines new subytpes of c-decl.  It takes a name of a
;;; new structure type, a list of the slots for this type, and a body of forms
;;; that implement an emitter function for the declaration type.

(defmacro def-c-decl (name slots &body forms)
  (let ((constructor-name (intern (format nil "MAKE-~a" name)))
	(emitter-name (intern (format nil "EMIT-~a-TO-C-FILE" name))))
    `(progn
       (defstruct (,name
		    (:include c-decl)
		    (:constructor ,constructor-name ,slots))
	 ,@slots)
       (defun ,emitter-name (c-decl c-file indent)
	 ,@forms))))


       

;;; The `c-var-decl' structure represents a declaration of a global or local
;;; variable identifier.  It takes a list of storage class specifier strings, a
;;; C type, an identifier string, and either an expression to emit as the
;;; initializer or NIL.

(def-c-decl c-var-decl (storage-classes c-type identifier init-expr?)
  ;; Put a blank line before top level variable declarations.
  (if (zerop indent)
      (emit-newline-to-c-file c-file)
      (emit-indentation-to-c-file c-file indent))
  (loop for string in (c-var-decl-storage-classes c-decl) do
    (emit-string-to-c-file string c-file)
    (emit-character-to-c-file #\space c-file))
  (let* ((c-type (c-var-decl-c-type c-decl))
	 (type-is-cons? (consp c-type))
	 (identifier (c-var-decl-identifier c-decl)))
    (cond ((and type-is-cons?
		(eq (cons-car c-type) 'pointer))
	   (emit-string-to-c-file (c-type-string (second c-type)) c-file)
	   (emit-string-to-c-file " *" c-file)
	   (emit-string-to-c-file identifier c-file))
	  ((and type-is-cons?
		(eq (cons-car c-type) 'array))
	   (emit-string-to-c-file (c-type-string (second c-type)) c-file)
	   (emit-character-to-c-file #\space c-file)
	   ;; For some reason, for the gcc compiler, externs for arrays must use
	   ;; array syntax, not pointer syntax, but declarations of variables
	   ;; within functions must use the pointer syntax.  Therefore, if we
	   ;; have a zero indent (i.e. this is a top-level declaration) always
	   ;; use array format.  -jra 5/31/96
	   (cond ((or (third c-type)
		      (zerop indent))
		  (emit-string-to-c-file identifier c-file)
		  (emit-character-to-c-file #\[ c-file)
		  (when (third c-type)
		    (emit-string-to-c-file (format nil "~a" (third c-type)) c-file))
		  (emit-character-to-c-file #\] c-file))
		 (t
		  (emit-character-to-c-file #\* c-file)
		  (emit-string-to-c-file identifier c-file)))) 
	  (t
	   (let ((type-string (c-type-string c-type)))
	     (when (null type-string)
	       (translation-error "Don't know how to emit c-type ~s" c-type))
	     (emit-string-to-c-file type-string c-file)
	     (emit-character-to-c-file #\space c-file)
	     (emit-string-to-c-file identifier c-file))))
    (when (c-var-decl-init-expr? c-decl)
      (if (not (eq c-type 'obj))
	  (emit-indentation-to-c-file c-file (1+ indent))
	  (emit-character-to-c-file #\space c-file))
      (emit-string-to-c-file "= " c-file)
      (emit-expression-to-c-file
	(c-var-decl-init-expr? c-decl) c-file (1+ indent)))
    (emit-character-to-c-file #\; c-file)
    (emit-newline-to-c-file c-file)))




;;; The function `emit-c-var-decl-list-to-c-file' is used to emit a set of c
;;; variable declarations where they all share a common type and none of them
;;; have initializations.  This should only be called from
;;; emit-c-decl-list-to-c-file.

(defun emit-c-var-decl-list-to-c-file (c-decl identifier-list c-file indent)
  (emit-indentation-to-c-file c-file indent)
  (loop for string in (c-var-decl-storage-classes c-decl) do
    (emit-string-to-c-file string c-file)
    (emit-character-to-c-file #\space c-file))
  (emit-string-to-c-file (c-type-string (c-var-decl-c-type c-decl)) c-file)
  (emit-character-to-c-file #\space c-file)
  (loop for first = t then nil
	for ident in identifier-list do
    (unless first
      (emit-string-to-c-file ", " c-file))
    (emit-indentation-to-c-file-if-necessary c-file nil (1+ indent))
    (emit-string-to-c-file ident c-file))
  (emit-character-to-c-file #\; c-file)
  (emit-newline-to-c-file c-file))




;;; The `c-function-decl' structure represents a function declaration.  It takes
;;; a list of storage-classes for the declaration (typically '("extern") or
;;; '("static")), a C type for the return value of the function, a string which
;;; is the identifier for the function, and a list of argument C types.  The
;;; declarations are always emitted as ANSI C prototypes.

(def-c-decl c-function-decl
    (storage-classes c-type identifier arg-c-types)
  (emit-indentation-to-c-file c-file indent)
  (loop for string in (c-function-decl-storage-classes c-decl) do
    (emit-string-to-c-file string c-file)
    (emit-character-to-c-file #\space c-file))
  (emit-string-to-c-file
    (c-type-string (c-function-decl-c-type c-decl)) c-file)
  (emit-character-to-c-file #\space c-file)
  (emit-string-to-c-file (c-function-decl-identifier c-decl) c-file)
  (emit-character-to-c-file #\( c-file)
  (if (c-function-decl-arg-c-types c-decl)
      (loop for first? = t then nil
	    for c-type in (c-function-decl-arg-c-types c-decl)
	    do
	(unless first?
	  (emit-string-to-c-file ", " c-file)
	  (emit-indentation-to-c-file-if-necessary c-file nil indent))
	(emit-string-to-c-file (c-type-string c-type) c-file))
      (emit-string-to-c-file "void" c-file))
  (emit-string-to-c-file ");" c-file)
  (emit-newline-to-c-file c-file))




;;; The `c-typedef-decl' structure represents a typedef declaration.  It takes a
;;; C type and a string identifier for that type.  The only C types that this is
;;; currently capable of emitting are new structures for holding constant
;;; arrays.  There is a special list representation for these types of arrays,
;;; (const-array <struct-type> <length>).  All other types used by the
;;; translator are defined beforehand in the tlt/tlt.h file.

;; In the future, we may wish to have specific C types defined for translated
;; def-structures.  When that occurs, C types and this declaration operation
;; should be extended to support emitting new structure types.  -jra 9/1/95

;; Done.  -jallard 11/3/99

(def-c-decl c-typedef-decl (c-type identifier)
  (let ((c-type (c-typedef-decl-c-type c-decl))
	(identifier (c-typedef-decl-identifier c-decl))
	(type-format-string nil)
	(type-string nil))
    (unless (and (consp c-type)
		 (or (eq (car c-type) 'struct)
		     (and (eq (car c-type) 'const-array)
			  (fixnump (third c-type)))))
      (error "Can only define new types of structs and explicitly sized arrays, not ~s"
	     c-type))
    (cond ((eq (car c-type) 'struct)
	   (setq type-string (c-type-string c-type)))
	  (t
	   (ecase (second c-type)
	     ((sv)
	      (setq type-format-string
		    "struct {
  unsigned int type  :  8;
  unsigned int length: 24;
  Obj body[~a];
}"
		    ))
	     ((str)
	      (setq type-format-string
		    "struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  unsigned char body[~a];
}"
		    ))
	     ((sa-uint8)
	      (setq type-format-string
		    "struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  uint8 body[~a];
}"
		    ))
	     ((sa-sint16)
	      (setq type-format-string
		    "struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  sint16 body[~a];
}"
		    ))
	     ((sa-uint16)
	      (setq type-format-string
		    "struct {
  unsigned int type       :  8;
  unsigned int length     : 24;
  unsigned int fill_length: 24;
  uint16 body[~a];
}"
		    ))
	     ((sa-double)
	      (setq type-format-string
		    "struct {
  unsigned int type:    8;
  unsigned int length: 24;
  double body[~a];
}"
		    )))
	   (setq type-string (format nil type-format-string (third c-type)))))
    ;; Now format the typedef.  Emit a blank line before top level typedefs.
    (if (zerop indent)
	(emit-newline-to-c-file c-file)
      (emit-indentation-to-c-file c-file indent))
    (emit-string-to-c-file "typedef " c-file)
    ;; Note that the following string has newlines in it, which messes up the
    ;; line length unless you use the connect emitter.
    (emit-string-with-newlines-to-c-file type-string c-file)
    (emit-character-to-c-file #\space c-file)
    (emit-string-to-c-file identifier c-file)
    (emit-character-to-c-file #\; c-file)
    (emit-newline-to-c-file c-file)))




;;; The function `emit-c-decl-list-to-c-file' takes a list of C declarations and
;;; emits them to the given C file at the given indentation level.  If there are
;;; multiple variable declarations that may be collapsed onto a single line,
;;; this function will attempt to do so.  Otherwise, it is a simple iteration
;;; over the list, repeatedly calling emit-declaration-to-c-file.

(defun emit-c-decl-list-to-c-file (c-decl-list c-file indent)
  (loop with c-decl
	for c-decl-cons = c-decl-list then (cons-cdr c-decl-cons)
	while c-decl-cons
	do
    (setq c-decl (cons-car c-decl-cons))
    (if (and (c-var-decl-p c-decl)
	     (symbolp (c-var-decl-c-type c-decl))
	     (null (c-var-decl-init-expr? c-decl)))
	(loop with vars = (list (c-var-decl-identifier c-decl))
	      while (and (consp (cons-cdr c-decl-cons))
			 (let ((next-decl (cons-second c-decl-cons)))
			   (and (c-var-decl-p next-decl)
				(eq (c-var-decl-c-type c-decl)
				    (c-var-decl-c-type next-decl))
				(equalp (c-var-decl-storage-classes c-decl)
					(c-var-decl-storage-classes next-decl))
				(null (c-var-decl-init-expr? c-decl)))))
	      do
	  (setq c-decl-cons (cons-cdr c-decl-cons))
	  (setq vars (nconc vars (list (c-var-decl-identifier
					 (cons-car c-decl-cons)))))
	      finally
		(emit-c-var-decl-list-to-c-file c-decl vars c-file indent))
	(emit-declaration-to-c-file c-decl c-file indent))))




;;; The function `emit-declaration-to-c-file' takes a C decl and emits it at the
;;; proper indentation within the file.  This declaration is reponsible for
;;; emitting indentation and the newline following the declaration.

(defun emit-declaration-to-c-file (c-decl c-file indent)
  (cond ((c-var-decl-p c-decl)
	 (emit-c-var-decl-to-c-file c-decl c-file indent))
	((c-function-decl-p c-decl)
	 (emit-c-function-decl-to-c-file c-decl c-file indent))
	((c-typedef-decl-p c-decl)
	 (emit-c-typedef-decl-to-c-file c-decl c-file indent))
	(t
	 (error "Can't emit ~s as a c-decl." c-decl))))






;;;; Registering Needed Externs




;;; The function `register-needed-function-extern' is used to register the need for an
;;; extern statement with the given characteristics.  If this function is called
;;; more than once with the same identifier, then only the first registered
;;; extern will be used, the second is redundant.  (It is possible that the
;;; second could be a different, thus conflicting extern.  There is other code
;;; that ensures this will not happen.  -jra 11/15/95)

(defun register-needed-function-extern
    (c-file storage-classes c-return-type c-identifier c-parameter-types)
  (unless (gethash c-identifier (c-file-needed-function-externs c-file))
    (setf (gethash c-identifier (c-file-needed-function-externs c-file))
	  (make-c-function-decl
	    storage-classes c-return-type c-identifier c-parameter-types))))




;;; The function `register-needed-class-typedef' is used to register the need
;;; for a typedef statement with the given characteristics.  The first time this
;;; is called for a file, a typedef will be emitted into the include file.
;;; Second and subsequent calls to this function for a particular file have no
;;; effect.

(defun register-needed-class-typedef (c-file c-type-name c-struct-type)
  (unless (gethash c-type-name (c-file-needed-class-typedefs c-file))
    (setf (gethash c-type-name (c-file-needed-class-typedefs c-file))
	  (make-c-typedef-decl c-struct-type c-type-name))))




;;; The function `register-needed-variable-extern' is used to register the need for an
;;; extern statement with the given characteristics.  If this variable is used
;;; more than once in this c-file, then only the first call for the identifier
;;; is used.

(defun register-needed-variable-extern (c-file storage-classes c-type c-identifier)
  (unless (gethash c-identifier (c-file-needed-variable-externs c-file))
    (setf (gethash c-identifier (c-file-needed-variable-externs c-file))
	  (make-c-var-decl storage-classes c-type c-identifier nil))))
