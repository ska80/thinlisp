(in-package "TLI")

;;;; Module CLOS

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






;;;; CLOS




;;; This module implements a subset of CLOS.  In particular, this implementation
;;; assumes that all classes that will be defined have already been defined, and
;;; therefore it makes optimizations that would be incorrect if further
;;; subclasses could be defined.

;;; There are some cases where this will not be appropriate.  In particular, if
;;; there are systems that are distributed as binary libraries (i.e. the Lisp
;;; source is not included), and so no retranslation of that system could be
;;; done after it had been specialized by a user, then we need to be able to
;;; declare that the classes in that library may not be optimized as strongly as
;;; the default case.  A future feature of TL could be a `tl:subclassable'
;;; declaration as an extension to Common Lisp that informs TL that it would not
;;; make assumptions about the subclass tree of a class while translating.
;;; Hopefully this feature will be rarely used, since it has a devastating
;;; effect on the performance of many operations.  For those Dylan afficianados
;;; out there, it is as if every class were declared sealed by default, and you
;;; had to explicitly unseal them to get the more dynamic behavior.

(defmacro def-metaclasses (&rest names)
  `(tl:progn
    ,@(loop for name in names
	    collect
	    `(tl:setf (tl:get ','name 'metaclass) t))
    nil))

(def-metaclasses tl:standard-class tl:built-in-class tl:structure-class)

;(defclass standard-object)

;(defclass standard-class)

; Update-instance-for-redefined-class

; shared-initialize

; slot-value

; change-class

; defmethod

; defgeneric

; class type, instances describe classes

; method - an implementation for a particular op/class

; standard-method - subclass of method

; method-combination - indirect instance of the method-combination class that
; represents the type of method combination used for a particular generic
; function.

; coerce function - no effect

; type-of returns the symbol naming the class

; type boolean => (member t nil)

; generic-function, standard-generic-function type - dispatches to methods


(defmacro tl:function-keywords (method)
  (error "tl:function-keywords unsupported : ~a" method))


(defun ensure-generic-function (function-name &key argument-precedence-order
					      declare documentation 
					      environment generic-function-class
					      lambda-list method-class
					      method-combination 
					      generic-function )
  (error "stub"))

(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys)
  )

(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys)
  )

(defgeneric shared-initialize (instance slot-names &rest initargs &key &allow-other-keys)
  )

(defgeneric update-instance-for-different-class 
  (previous current &rest initargs &key &allow-other-keys)
  )

(defmethod update-instance-for-redefined-class 
  (instance added-slots discarded-slots property-list &rest initargs 
	    &key &allow-other-keys)
  )

(defgeneric change-class (instance new-class &rest initargs)
  )


(defun slot-boundp (instance slot-name)
  )

(defun slot-exists-p (object slot-name)
  nil)

(defun slot-makunbound (instance slot-name)
  instance)

(defgeneric slot-missing (class object slot-name operation &optional new-value)
   )

(defgeneric slot-unbound (class instance slot-name)
  )

(defun slot-value (object name)
  nil)

(defgeneric no-applicable-method ()
  nil)

(defun determine-effective-method ()
  ; 1. Select the applicable methods.

  ; 2. Sort the applicable methods by precedence order, putting the most
  ; specific method first.

  ; 3. Apply method combination, to the sorted list of applicable methods,
  ; producing the effective method.

  nil)

(defun precedence-order-sort ()
  ; 1. Examine parameter specializers in left to right order, or in the order
  ; provided by the :argument-precedence-order option to defgeneric or to any of
  ; the other operations that specify generic function options.

  ; 2. Compare parameter specializers from each method.  When they argree, the
  ; next pair are compared fro agreement.  If all agree, then the two methods
  ; must have different qualifiers, so relative order between them is
  ; unimportant.  When they do not agree, the most specific superior class of
  ; the actual argument is more specific and so is sorted closer to the front of
  ; the list.

  ; 3. If just one pair of corresponding parameter specializers is (eql object),
  ; the method with that parameter specializer precedes the other method.  If
  ; both parameter specializers are eql expressions, the specializers must
  ; agree.  The resulting list has the most specific first, and the least
  ; specific last.

  nil)

(defun apply-method-combination ()
  ; 1. Before methods are run in most specific-first order, and after methods
  ; are run in least-specific-first order.

  ; 2.  All of the around methods are run before any other methods, so for
  ; example a less specific around method could run before a more specific
  ; primary method.

  ; 3.  If only primary methods are used and if call-next-method is not used,
  ; only the most specific method is invoked, that is more specific primary
  ; methods shadow more general ones.

  ; 4. If any around methods exist, they provide the values returned from the
  ; generic function.  If not, then the primary method supplies the values.  If
  ; no around or primary methods exist, then the values returned by
  ; no-applicable-method are returned.

  nil)

(defgeneric call-next-method ()
  ; Can be called within around methods or primary methods to invoke the next
  ; around method or primary method, respectively.  If no other around method or
  ; primary method exists, then no-next-method is called.
  nil)

(defgeneric no-next-method ()
  ; This is called with call-next-method comes up empty.
  nil)

(defun next-method-p ()
  ; Returns whether or not a next method exists.
  nil)







;;; Def-class, make-instance
