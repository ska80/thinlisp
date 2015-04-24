(in-package "TLI")

;;;; Module C-COERCE

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






;;;; C Type Coercion




;;; This module implements coercions between C types.  Most of the types used in
;;; TLT can be coerced into type Obj and back.  This module implements the
;;; functions and declarations needed for that.






;;;; C Type Definitions




;;; The following are the declarations and coercion functions for all the C
;;; types.

;; When passing "pointers" straight through to C, the argument type will be
;; pointer to void.

(def-c-type void "void" "void *" nil nil)

(def-c-pointer-type-coercion void (c-expr starting-c-type)
  ((obj)
   (make-c-cast-expr '(pointer void) c-expr))
  ((void)
   (make-c-cast-expr
     '(pointer void)
     (coerce-c-expr-result-to-type
       c-expr starting-c-type 'obj env))))

(def-c-type obj "Obj" "Obj *" nil nil)

(def-c-type-coercion obj (c-expr original-type)
  (((pointer cons))
   (make-c-cast-expr
     'obj
     (make-c-infix-expr (make-c-cast-expr 'uint32 c-expr) "+" 2)))
  ((sint32 uint32 sint16 uint16 uint8 int uint long ulong size-t)
   (make-c-function-call-expr (make-c-name-expr "BOXFIX") (list c-expr)))
  ((unsigned-char)
   (make-c-function-call-expr (make-c-name-expr "BOXCHAR") (list c-expr)))
  (((pointer obj))
   (make-c-function-call-expr (make-c-name-expr "ObjSvHDR") (list c-expr)))
  (((pointer unsigned-char))
   (make-c-function-call-expr (make-c-name-expr "ObjStrHDR") (list c-expr)))
   (((pointer uint8))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint8) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
   (((pointer sint16))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-sint16) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
   (((pointer uint16))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-uint16) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
   (((pointer double))
    (make-c-cast-expr
     'obj
     (make-c-infix-expr
       (make-c-cast-expr 'uint32 c-expr) "-"
       (make-c-cast-expr
	 'uint32
	 (make-c-unary-expr
	   #\&
	   (make-c-subscript-expr
	     (make-c-indirect-selection-expr
	       (make-c-cast-expr '(pointer sa-double) (make-c-name-expr "NULL"))
	       "body")
	     (make-c-literal-expr 0)))))))
  ((boolean)
   (make-c-conditional-expr
     c-expr
     (make-c-cast-expr 'obj (make-c-unary-expr #\& (make-c-name-expr "T")))
     (make-c-cast-expr 'obj (make-c-name-expr "NULL"))))
  ((double)
   (make-c-function-call-expr
     (make-c-name-expr "alloc_ldouble")
     (list c-expr
	   (make-c-literal-expr
	     (region-number-for-type-and-area
	       'double-float (declared-area-name env 'double-float)))
	   (make-c-literal-expr (c-type-tag 'ldouble)))))
  (((pointer uint32))
    ;; Objects masquerading as something else coming in from C sometimes are
    ;; passed as pointer to uint32.  Just cast them to Obj.
    (make-c-cast-expr 'obj c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'obj (default-value-for-c-type 'obj)))
  )

(defun make-void-cast-stand-in-expr (c-expr original-type target-type default)
  (let ((exprs (list default)))
    (unless (eq original-type 'void)
      (push (make-c-function-call-expr
	      (make-c-name-expr "type_cast_error")
	      (list (make-c-literal-expr (c-type-string 'void))
		    (make-c-literal-expr (c-type-string target-type))))
	exprs))
    (unless (side-effect-free-c-expr-p c-expr)
      (push c-expr exprs))
    (if (null (cdr exprs))
	(car exprs)
	(make-c-comma-expr exprs))))

(def-c-pointer-type-coercion obj (c-expr original-type)
  (((pointer sv))
   (make-c-indirect-selection-expr c-expr "body"))
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sv) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer obj)
     (default-value-for-c-type '(pointer obj)))))

(def-c-type cons "Cons" "Cons *" nil nil)

(def-c-pointer-type-coercion cons (c-expr original-expr)
  ((obj)
   (make-c-cast-expr
     '(pointer cons)
     (make-c-infix-expr (make-c-cast-expr 'uint32 c-expr) "-" 2)))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer cons)
     (default-value-for-c-type '(pointer cons)))))
		     

(def-c-type hdr "Hdr" "Hdr *" nil nil)

(def-c-type sint32 "sint32" "sint32 *" nil nil)

(def-c-type-coercion sint32 (c-expr original-type)
  ((obj)
   (make-c-function-call-expr 
    (make-c-name-expr "UNBOXFIX") (list c-expr)))
  ((uint32 uint8 sint16 uint16 unsigned-char int long size-t)
   (make-c-cast-expr 'sint32 c-expr))
  ((double)
   ;; This is used by rounding and truncation code.
   (make-c-cast-expr 'sint32 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'sint32 (make-c-literal-expr 0))))

(def-c-type uint8 "uint8" "uint8 *" nil nil)

(def-c-type-coercion uint8 (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'uint8
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint16 uint16 sint32 unsigned-char)
   (make-c-cast-expr 'uint8 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'uint8 (make-c-literal-expr 0))))

(def-c-pointer-type-coercion uint8 (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-uint8) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer uint8)
     (default-value-for-c-type '(pointer uint8)))))

(def-c-type sint16 "sint16" "sint16 *" nil nil)

(def-c-type-coercion sint16 (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'sint16 (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((uint8 uint16 sint32 unsigned-char)
   (make-c-cast-expr 'sint16 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'sint16 (make-c-literal-expr 0))))

(def-c-pointer-type-coercion sint16 (c-expr original-expr)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-sint16) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer sint16)
     (default-value-for-c-type '(pointer sint16)))))

(def-c-type uint16 "uint16" "uint16 *" nil nil)

(def-c-type-coercion uint16 (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'uint16 (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((uint8 sint16 sint32 unsigned-char)
   (make-c-cast-expr 'uint16 c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'uint16 (make-c-literal-expr 0))))

(def-c-pointer-type-coercion uint16 (c-expr original-expr)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-uint16) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer uint16)
     (default-value-for-c-type '(pointer uint16)))))

(def-c-type uint32 "uint32" "uint32 *" nil nil)

(def-c-pointer-type-coercion uint32 (c-expr original-expr)
  ;; In the OLE code, there is a cheater function that needs this type
  ;; transform.  -jallard 11/3/97
  ((obj)
   (make-c-cast-expr '(pointer uint32) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-expr '(pointer uint32)
     (default-value-for-c-type '(pointer uint32)))))

(def-c-type char "char" "char *" nil nil)

(def-c-type unsigned-char "unsigned char" "unsigned char *" nil nil)




;;; The types long and int are only used when casting values for passing to
;;; built-in routines, or casting back the type of returned values.

(def-c-type long "long" "long *" nil nil)

(def-c-type ulong "unsigned long" "unsigned long *" nil nil)

(def-c-type size-t "size_t" "size_t *" nil nil)

(def-c-type int "int" "int *" nil nil)

(def-c-type uint "unsigned int" "unsigned int *" nil nil)

(def-c-type-coercion long (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'long
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 sint16 uint16 char unsigned-char int size-t)
   (make-c-cast-expr 'long c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'long (make-c-literal-expr 0))))

(def-c-type-coercion ulong (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'ulong
     (coerce-c-expr-result-to-type c-expr 'obj 'uint32 env)))
  ((sint32 uint8 sint16 uint16 char unsigned-char int size-t)
   (make-c-cast-expr 'ulong c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'ulong (make-c-literal-expr 0))))

(def-c-type-coercion size-t (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'size-t
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 sint16 uint16 char unsigned-char int)
   (make-c-cast-expr 'size-t c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'size-t (make-c-literal-expr 0))))

(def-c-type-coercion int (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'int
     (coerce-c-expr-result-to-type c-expr 'obj 'sint32 env)))
  ((sint32 uint8 sint16 uint16 char unsigned-char long size-t)
   (make-c-cast-expr 'int c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'int (make-c-literal-expr 0))))

(def-c-type-coercion uint (c-expr original-type)
  ((obj)
   (make-c-cast-expr
     'uint
     (coerce-c-expr-result-to-type c-expr 'obj 'uint32 env)))
  ((sint32 uint8 sint16 uint16 char unsigned-char long size-t)
   (make-c-cast-expr 'uint c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'uint (make-c-literal-expr 0))))

(def-c-type-coercion unsigned-char (c-expr original-type)
  ((obj)
   (make-c-function-call-expr (make-c-name-expr "UNBOXCHAR") (list c-expr)))
  ((sint32 uint8 sint16 uint16 char)
   (make-c-cast-expr 'unsigned-char c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'unsigned-char (make-c-literal-expr 0))))

(def-c-pointer-type-coercion unsigned-char (c-expr original-type)
  (((pointer str))
   (make-c-indirect-selection-expr c-expr "body"))
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer str) c-expr)
     "body"))
  (((pointer char))
   (make-c-cast-expr '(pointer unsigned-char) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer unsigned-char)
     (default-value-for-c-type '(pointer unsigned-char)))))

(def-c-type-coercion char (c-expr original-type)
  ((obj)
   (make-c-cast-expr
    'char 
    (make-c-function-call-expr (make-c-name-expr "UNBOXCHAR") (list c-expr))))
  ((sint32 uint8 sint16 uint16)
   (make-c-cast-expr 'char c-expr))
  ((unsigned-char)
   (make-c-cast-expr 'char c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'char (make-c-literal-expr 0))))

(def-c-pointer-type-coercion char (c-expr original-type)
  (((pointer str))
   (make-c-cast-expr
    '(pointer char)
    (make-c-indirect-selection-expr c-expr "body")))
  ((obj)
   (make-c-cast-expr
     '(pointer char)
     (make-c-indirect-selection-expr
       (make-c-cast-expr '(pointer str) c-expr)
       "body")))
  (((pointer unsigned-char))
   (make-c-cast-expr '(pointer char) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer char)
     (default-value-for-c-type '(pointer char)))))

(def-c-type boolean "int" "int *" nil nil)

(def-c-type-coercion boolean (c-expr original-c-type)
  ((obj)
   (make-c-equality-expr c-expr "!=" (make-c-name-expr "NULL")))
  ;; I'm not really sure that the next line is well founded.  Lisp FALSE is
  ;; explicitly only NIL, and integers with zero values are actually equivalent
  ;; to TRUE.  So, for the types that are native C types (as opposed to types
  ;; that optimize Lisp types), provide the convenience of comparing them
  ;; against zero.  -jallard 7/23/97
  ((int long char short size-t unsigned-short)
   (make-c-equality-expr c-expr "!=" (make-c-literal-expr 0)))
  ((void)
   ;; Since all types satisfy the void type, this is the effective else clause
   ;; of this form.  Attempt to cast the value back to object and then compare
   ;; it against NULL.
   (make-c-equality-expr
     (coerce-c-expr-result-to-type
       c-expr original-c-type 'obj env)
     "!=" (make-c-name-expr "NULL"))))

(def-c-type double "double" "double *" nil nil)

(def-c-type-coercion double (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer ldouble) c-expr)
     "body"))
  ((sint32 uint32 sint16 uint16 uint8 long int size-t)
   (make-c-cast-expr 'double c-expr))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type 'double (make-c-literal-expr 0.0))))

(def-c-pointer-type-coercion double (c-expr original-type)
  ((obj)
   (make-c-indirect-selection-expr
     (make-c-cast-expr '(pointer sa-double) c-expr)
     "body"))
  ((void)
   (make-void-cast-stand-in-expr
     c-expr original-type '(pointer double)
     (default-value-for-c-type '(pointer double)))))

(def-c-type mdouble "Mdouble" "Mdouble *" managed-float 4)

(def-c-type ldouble "Ldouble" "Ldouble *" double-float 5)

(def-c-type sv "Sv" "Sv *" simple-vector 6)

(def-c-pointer-type-coercion sv (c-expr original-type)
  (((pointer obj))
   (make-c-function-call-expr (make-c-name-expr "SvHDR") (list c-expr)))
  ((obj)
   (make-c-cast-expr '(pointer sv) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
    c-expr original-type '(pointer sv) 
    (default-value-for-c-type '(pointer sv)))))
  

(def-c-type str "Str" "Str *" string 7)

(def-c-pointer-type-coercion str (c-expr original-type)
  (((pointer unsigned-char))
   (make-c-function-call-expr (make-c-name-expr "StrHDR") (list c-expr)))
  ((obj)
   (make-c-cast-expr '(pointer str) c-expr))
  ((void)
   (make-void-cast-stand-in-expr
    c-expr original-type '(pointer str) 
    (default-value-for-c-type '(pointer str)))))

(def-c-type sa-uint8 "Sa_uint8" "Sa_uint8 *" (array (unsigned-byte 8)) 8)

(def-c-type sa-uint16 "Sa_uint16" "Sa_uint16 *" (array (unsigned-byte 16)) 9)

(def-c-type sa-double "Sa_double" "Sa_double *" (array double-float) 10)

(def-c-type sym "Sym" "Sym *" symbol 11)

(def-c-type func "Func" "Func *" compiled-function 12)

(defmacro def-c-function-types-for-funcall ()
  (cons 'progn
	(loop for args from 0 to tl:lambda-parameters-limit
	      for c-type-symbol
		  = (intern (format nil "FUNC-~a" args) *tli-package*)
	      for c-type-string = (format nil "Func_~a" args)
	      collect
	      `(def-c-type ,c-type-symbol
		   ,c-type-string
		 ,(format nil "~a *" c-type-string)
		 nil nil))))

(def-c-function-types-for-funcall)

(def-c-type pkg "Pkg" "Pkg *" package 13)

(def-c-type unbound "Hdr" "Hdr *" unbound 14)

(def-c-type string-strm "String_strm" "String_strm *" tl-string-stream 15)

(def-c-type file-strm "File_strm" "File_strm *" file-stream 16)

(def-c-type jmp-buf "jmp_buf" "jmp_buf *" nil nil)

(def-c-type thread-state "Thread_state" "Thread_state *" nil nil)

(def-c-type c-function "ERROR" "Obj (*)(Obj)" nil nil)

(def-c-type char-pointer "char *" "char **" nil nil)

(def-c-type class-hdr "Class_hdr" "Class_hdr *" nil 17)

(def-c-type sa-sint16 "Sa_sint16" "Sa_sint16 *" (array (signed-byte 16)) 18)



;;; Note that you never manipulate files in C, only pointers to them.

(def-c-type file "FILE" "FILE *" nil nil)
