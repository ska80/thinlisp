(in-package "TLI")

;;;; Module L-STACK

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






;;;; Lisp Stacks




;;; Within the TL runtime environment, each thread of execution gets a stack for
;;; storing global variable bindings, unwind-protect cleanups, sets of values
;;; from multiple-value-prog1 forms, and catch scopes.  This file contains
;;; functions that emit code that manages the stack.  These are generally called
;;; from the l-expr translations in L-TRANS.






;;;; Stack Layout




;;; The Lisp stack is constructed as follows.  Within the Thread_state struct
;;; for a thread, there is a member called throw_stack and another called
;;; throw_stack_top.  These names came from an early point in the implementation
;;; when only catch and throw had been implemented, and these stacks had not yet
;;; been used for global variable binding or unwind-protect scopes.  The
;;; throw_stack is an array of Obj elements, and the throw_stack_top is an
;;; integer pointing to the highest occupied element of the stack.  In other
;;; words, when the stack is empty, throw_stack_top equals -1.  (Yes, it's
;;; strange, and I can't remember why in the world I did it that way, but lots
;;; of code depends on that now, so we're somewhat stuck.

;;; The stack can be walked from the top down towards the bottom.  The things
;;; held on the stack are stack frames.  The topmost element of every stack
;;; frame is an integer denote the stack frame type.  The number of stack
;;; elements and their interpretation are different per type of stack frame.
;;; The Stack_frames enumeration in tl/c/tl.h defines the constants used to mark
;;; the different stack frame types.






;;;; Global Variable Bindings




;;; The binding style used for TL depends on whether or not TL is compiled for
;;; multi-threading (i.e. has the THREAD variable set when invoking the
;;; makefiles).  The C macros GET_GLOBAL and SET_GLOBAL implement global
;;; variable getting and setting of the current bound value within the current
;;; thread.  In both situations, references to special variables are translated
;;; as calls to GET_GLOBAL and SET_GLOBAL, and given as their first argument a
;;; reference to a C global variable that represents the value cell of the
;;; symbol naming the special variable.

;;; If TL is compiled without multi-threading, then shallow binding will be
;;; used.  The C global variable will always contain the value of the current
;;; binding, and the stack will contain previous binding values which must be
;;; restored as binding scopes are exited.  In shallow binding, calls to
;;; GET_GLOBAL and SET_GLOBAL get and set the value of the global C variable
;;; directly without any intervening function calls or further processing.
;;; Binding a special means storing the current value of the C global into the
;;; binding frame on the stack, and then overwriting the C global with the new
;;; bound value.  Unbinding a special means restoring the value from the binding
;;; frame by overwriting the value of the C global.

;;; When compiled with multi-threading, then deep binding is used.  The current
;;; binding varies per thread, since the dynamic scope of a binding only applies
;;; to the thread in which the binding was made.  The value of the C global
;;; variable contains the value of the default binding, i.e. the value shared
;;; between threads that have not further bound that variable.  The stack
;;; contains new bindings which themselvs hold the value for new bindings.  In
;;; deep binding, calls to GET_GLOBAL and SET_GLOBAL must first search the stack
;;; of the currently running thread for any bindings of the special variable.
;;; If a thread specific binding is found, then the value of the special
;;; variable is fetched or set into the memory location within the topmost
;;; binding frame for that variable on the stack.  If no binding frame for that
;;; variable is found, then the C global variable is used.  Each binding frame
;;; on the stack is identified with the address of the C global variable.

;;; The function `bind-global-for-let' emits code to bind a global variable.
;;; The stack frame for a binding contains the following elements, from the top
;;; of the frame down towards the bottom:
;;;   1.  BINDING_FRAME, i.e. the stack frame type marker
;;;   2.  The address of the C global variable
;;;   3.  The value of the binding.

;;; When using shallow binding (i.e. non-threaded compilation) the value will
;;; actually be the value of the previous binding and will be used to restore
;;; the previous binding.  When using deep binding, the value element of this
;;; stack frame is actually the memory location of the new binding, and all gets
;;; and sets of the global variable will occur into this location.

(defun bind-global-for-let (global-identifier new-value-identifier 
					      c-compound-statement
					      &optional thread-state-identifier)
  (emit-expr-to-compound-statement
   (make-c-function-call-expr 
    (make-c-name-expr "bind_global")
    (list (make-c-unary-expr #\& (make-c-name-expr global-identifier))
	  (make-c-name-expr (or thread-state-identifier "THREAD_STATE"))
	  (make-c-name-expr new-value-identifier)))
   c-compound-statement))  

(defun rebind-globals-for-let (specials-to-rebind c-compound-statement 
						  &optional thread-state-var)
  (loop for (global-var . new-value) in specials-to-rebind do
    (bind-global-for-let global-var new-value c-compound-statement 
			 thread-state-var)))

(defun unbind-globals-for-let (globals c-compound-statement 
				       &optional thread-state-var)
  (loop for index fixnum from 0
	for global in (reverse globals)
	do
    (when (not (stringp global))
      (translation-error "Unbinding global ~s, which was not a string" global))
    (emit-expr-to-compound-statement
     (make-c-function-call-expr
      (make-c-name-expr "unbind_global")
      (list (make-c-unary-expr #\& (make-c-name-expr global))
	    (make-c-name-expr (or thread-state-var "THREAD_STATE"))))
     c-compound-statement)))
