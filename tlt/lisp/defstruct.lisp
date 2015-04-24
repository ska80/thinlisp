(in-package "TLI")

;;;; Module DEFSTRUCT

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






;;;; Defstruct




;;; This module implements the `tl:defstruct' macro.  During Lisp development
;;; time it expands into the underlying Lisp's defstruct.  Translated, each
;;; structure type has a unique type number.  These numbers are produced via a
;;; depth first walk of the structure type hierarchy.  Subtype checks are
;;; preformed by determining that the object is a structure, and that the
;;; structure type number is greater than or equal to the structure number, and
;;; less than or equal to the highest numbered type in the type subtree below
;;; this structure.  In translation, the type slot of the Hdr structure will
;;; contain the type number for all structures, and the fill slot will contain
;;; the unique number for this type.

;;; The `get-defstruct-option' function is used to help decipher the unusual
;;; options format for defstruct.

(defun get-defstruct-option (options name default 
				     &optional (singleton-default nil))
  (loop for elt in options do
    (cond ((eq elt name)
	   (return singleton-default))
	  ((and (consp elt) (eq (car elt) name))
	   (return (cadr elt))))
        finally (return default))) 



;;; The accessors `structure-info' and `class-info' take a symbol and return the
;;; struct that holds its information, or else NIL if it is not a defined class
;;; or structure type.  Thare are setfable.  Currently they are synonyms.

(defmacro structure-info (name)
  `(get ,name :structure-info))

(defmacro class-info (name)
  `(structure-info ,name))




;;; The function `class-type-p' takes a Lisp type and returns whether or this
;;; type directly names a structure-class or standard-class.  That is, this
;;; returns non-null if the type is a symbol and names a structure or class.

(defun class-type-p (lisp-type)
  (and (symbolp lisp-type)
       (structure-info lisp-type)))




;;; The variable `*all-classes*' contains a list of all symbols naming classes.

(defvar *all-classes* nil)




;;; The `struct' structure type is used to hold structure type
;;; information during Lisp development time.

(defstruct (struct)
  name
  doc
  local-slot-descriptions
  slot-descriptions
  conc-name
  copier
  include
  (initial-offset 0)
  predicate
  print-object
  print-function
  type
  reclaimer
  (type-tag nil)
  (maximum-subtype-tag nil)
  (c-type nil)
  (c-type-name nil)
  (c-alignment nil))

(defstruct (struct-slot (:copier copy-struct-slot))
  name
  (initial-form nil)
  (original-type t)
  (type t)
  (read-only nil)
  (reclaimer nil)
  (reader nil)
  (dev-reader nil)
  (setter nil)
  (c-accessor nil)
  (c-type nil)
  (copier nil))

(defun construct-struct-slot (slot-description &optional conc-name overridden-slot)
  (let ((new-slot
	 (destructuring-bind (name &optional init 
				   &key (type t) (read-only nil)
				   (reclaimer nil) 
				   (copier nil))
			     slot-description
	   (make-struct-slot :name name 
			     :initial-form init :type type 
			     :original-type type :read-only read-only
			     :reclaimer reclaimer :copier copier))))
    (cond (overridden-slot
	   (setf (struct-slot-original-type new-slot)
	     (struct-slot-original-type overridden-slot))
	   (setf (struct-slot-read-only new-slot)
	     (or (struct-slot-read-only overridden-slot)
		 (struct-slot-read-only new-slot)))
	   (setf (struct-slot-reader new-slot)
	     (struct-slot-reader overridden-slot))
	   (setf (struct-slot-dev-reader new-slot)
	     (struct-slot-dev-reader overridden-slot))
	   (setf (struct-slot-setter new-slot)
	     (struct-slot-setter overridden-slot)))
	  (t
	   (let ((reader 
		  (if conc-name
		      (intern (format nil "~a~a" conc-name
				      (struct-slot-name new-slot)))
		    (struct-slot-name new-slot))))
	     (setf (struct-slot-reader new-slot) reader)
	     (setf (struct-slot-dev-reader new-slot)
	       (intern (format nil "TL-DEV-~a" reader)))
	     (setf (struct-slot-setter new-slot)
		   (intern (format nil "SETF-~a" reader))))))
    new-slot))




;;; The function `assign-structure-type-tags' assigns a new set of integer type
;;; tags to all structure and class types currently defined.  This should be
;;; done immediately before a translation begins.  If it is called twice during
;;; a translation, then the second call has no effect.  It is an error to define
;;; a new structure type while type tags are assigned.  The function
;;; `clear-structure-type-tags' removes those tags.  The macro
;;; `with-structure-tag-assignments' will assign type tags and then clear them
;;; on exit, unless this is a dynamically nested call to
;;; with-structure-tag-assignments, in which case it is a no-op.

(defvar structure-type-tags-assigned nil)

(defparameter base-structure-type-tag 32)

(defun assign-structure-type-tag-tree (type initial-tag)
  (declare (fixnum initial-tag))
  (let ((info (structure-info type)))
    (setf (struct-type-tag info) initial-tag)
    (incf initial-tag)
    (loop for subtype in (struct-subtypes type) do
      (setq initial-tag (assign-structure-type-tag-tree subtype initial-tag)))
    (setf (struct-maximum-subtype-tag info) (1- initial-tag))
    initial-tag))

(defun assign-structure-type-tags ()
  (unless structure-type-tags-assigned
    (setq structure-type-tags-assigned t)
    (assign-structure-type-tag-tree 'structure base-structure-type-tag)))

(defun clear-structure-type-tags ()
  (loop for type in *all-classes* do
    (setf (struct-type-tag (structure-info type)) nil))
  (setq structure-type-tags-assigned nil))

(defmacro with-structure-tag-assignments (&body forms)
  (let ((previously-assigned (gensym)))
    `(let ((,previously-assigned structure-type-tags-assigned))
       (unwind-protect
	   (progn
	     (unless ,previously-assigned
	       (assign-structure-type-tags))
	     ,@forms)
	 (unless ,previously-assigned
	   (clear-structure-type-tags))))))




;;; The function `class-type-tag' takes a Lisp type naming a class and returns
;;; its integer type tag.  Note that this function will only work for Lisp types
;;; that are symbols naming strutuure or class types.

(defun class-type-tag (lisp-type)
  (if (symbolp lisp-type)
      (let ((info (structure-info lisp-type)))
	(if info
	    (struct-type-tag info)
	  nil))
    nil))




;;; The function `struct-subtypes' takes a symbol naming a class or structure
;;; type, and returns a list of the direct subtypes of that type.  The function
;;; `struct-add-subtype' takes a superior type and a subtype, and adds the
;;; subtype to the superior type's list.

(defun struct-subtypes (type-symbol)
  (get type-symbol :subtypes))

(defun struct-add-subtype (superior-type subtype)
  (pushnew subtype (get superior-type :subtypes))
  superior-type)




;;; The function `type-tags-for-class-type' takes a symbol naming a structure
;;; or class and returns a list of the integer type tags for that type and all
;;; of its subtypes.

(defun type-tags-for-class-type (type-symbol)
  (let* ((struct-info (structure-info type-symbol))
	 (tag (struct-type-tag struct-info)))
    (cond 
      (tag 
       (loop for next-tag from tag to (struct-maximum-subtype-tag struct-info) 
	     collect next-tag))
      (t
       (error "Type ~s did not yet have a type tag assigned." type-symbol)))))





;;; The function `get-c-name-for-class-and-slot' takes a symbol naming a class
;;; and a symbol naming a slot in that class.  It returns a string naming the C
;;; accessor for that slot.  Note that this function can only be called during
;;; translation, and not during normal compilation.  The function
;;; `get-c-type-for-class-and-slot' is similar, except that it returns the C
;;; type for values stored in this slot.

(defun get-slot-for-class-and-slot-name (class-name slot-name)
  (loop with class-info = (class-info class-name)
	for slot in (struct-slot-descriptions class-info)
	do
    (when (eq (struct-slot-name slot) slot-name)
      (return slot))))

(defun get-c-name-for-class-and-slot (class-name slot-name)
  (struct-slot-c-accessor (get-slot-for-class-and-slot-name class-name slot-name)))

(defun get-c-type-for-class-and-slot (class-name slot-name)
  (struct-slot-c-type (get-slot-for-class-and-slot-name class-name slot-name)))




;;; The function `collect-struct-slot-descriptions' takes struct that has the
;;; local-slot-descriptions, include, and initial-offset slots filled in, and it
;;; computes a new full set of slot descriptions for this struct type and stores
;;; it into the slot-descriptions element of the given struct.

(defun collect-slot-descriptions (struct conc-name)
  (let* ((include-spec (struct-include struct))
	 (include-name (car include-spec))
	 (include-overrides (loop for elt in (cdr include-spec)
				  collect (if (symbolp elt)
					      (list elt)
					    elt)))
	 (superior-struct? (if include-name (structure-info include-name) nil))
	 (superior-descriptions
	  (cond ((null include-name)
		 nil)
		(superior-struct?
		 (struct-slot-descriptions superior-struct?))
		(t
		 (error "Included structure ~s is not defined." 
			include-name))))
	 (new-descriptions-list
	  (append
	   (loop for desc in superior-descriptions
		 for override = (member (struct-slot-name desc) 
					include-overrides
					:key #'car :test #'eq)
		 collect (if override
			     (construct-struct-slot 
			      (car override) nil desc)
			   (copy-struct-slot desc)))
	   (if (struct-type struct)
	       (loop repeat (struct-initial-offset struct)
		     collect (construct-struct-slot nil))
	     nil)
	   (loop for desc in (struct-local-slot-descriptions struct)
		 collect (construct-struct-slot desc conc-name)))))
    new-descriptions-list))

	  
    

;;; The macro `tl:defstruct' implements ANSI defstruct, with the following
;;; extensions.

;;; The option (:reclaimer <name>) can be given to indicate that a reclaimer
;;; function should be defined which will call all slot reclaimer functions, and
;;; then store the structure in a resource pool which will be used to fetch a
;;; new structure instance the next time one is needed.  If this option is
;;; given, but no name is provided, then the name defaults to "RECLAIM-"
;;; prepended to the structure name and interned in the default package where
;;; the defstruct is defined.

;;; The slot description option :reclaimer should be given an argument naming a
;;; function or macro which should be called to reclaim the value of this slot
;;; when the structure reclaimer is called.  Note that this option is only
;;; useful when the structure as a whole has been given the :reclaimer option,
;;; which will force definition of a reclaimer function from which this
;;; reclaimer will be called.

(defmacro tl:defstruct (name-and-options &rest doc-and-slots)
  (let* ((name (if (consp name-and-options) 
		   (cons-car name-and-options)
		 name-and-options))
	 (options (if (consp name-and-options)
		      (cdr name-and-options)
		    nil))
	 (doc (if (stringp (car doc-and-slots))
		  (car doc-and-slots)
		nil))
	 (type (get-defstruct-option options :type nil))
	 (reclaimer (get-defstruct-option options :reclaimer nil))
	 (reclaimer-slot-name (if reclaimer
				  (intern (format nil "~a-RECYCLE-CHAIN" name))
				nil))
	 (pool (if reclaimer
		   (intern (format nil "~a-POOL" name) *tli-package*)
		 nil))
	 (local-slot-descriptions 
	  (append
	   (loop for desc in (if doc (cdr doc-and-slots) doc-and-slots)
		 for slot-name = (if (symbolp desc) desc (car desc))
		 collect (if (symbolp desc) (list slot-name) desc))
	   (if reclaimer-slot-name
	       (list (list reclaimer-slot-name))
	     nil)))
	 (conc-name 
	  (string (get-defstruct-option
		   options :conc-name (format nil "~a-" name))))
	 (copier 
	  (get-defstruct-option
	   options :copier (intern (format nil "COPY-~a" name))))
	 (include (get-defstruct-option options :include nil))
	 (initial-offset (get-defstruct-option options :initial-offset 0))
	 (named-option (get-defstruct-option options :named nil t))
	 (predicate
	  (let ((default-predicate (intern (format nil "~a-P" name))))
	    (get-defstruct-option
	     options :predicate default-predicate default-predicate)))
	 (print-object (get-defstruct-option options :print-object nil))
	 (print-function (get-defstruct-option options :print-function nil))
	 (new-struct (make-struct
		      :name name :doc doc
		      :local-slot-descriptions local-slot-descriptions
		      :copier copier :include include
		      :initial-offset initial-offset :predicate predicate
		      :print-object print-object :print-function print-function
		      :type type :reclaimer reclaimer))
	 (slots (collect-slot-descriptions new-struct conc-name))
	 (local-slots
	  (nthcdr (- (length slots) (length local-slot-descriptions)) slots))
	 (raw-constructor (intern (format nil "MAKE-~a" name) *tli-package*))
	 (constructors (collect-constructors name options slots)))
    `(tl:progn
      (tl:declaim (class-name ,name))
      ,@(if pool
	    `((tl:defvar ,pool nil))
	  nil)
      ,@(if (not (eval-feature :translator))
	    `((tl:eval-when (:compile-toplevel :load-toplevel :execute)
	        (install-structure
		  ',name ,doc ',local-slot-descriptions ',copier ',include 
		  ,initial-offset ',predicate ',print-object 
		  ',print-function ',type ',reclaimer ',conc-name))
	      (defstruct (,name (:conc-name nil)
			  (:constructor ,raw-constructor ())
			  (:copier nil) (:predicate ,predicate))
		,@(loop for local-slot in local-slots
		      collect (struct-slot-dev-reader local-slot))))
	  nil)
      ,@(expand-readers-and-writers 
	 name type named-option slots local-slot-descriptions)
      ,@(expand-constructors name type named-option slots 
			     raw-constructor pool constructors)
      ,@(if predicate
	    `((tl:declaim (tl:ftype (tl:function (t) t) ,predicate))
	      (tl:define-compiler-macro ,predicate (x)
	        `(tl:typep ,x ',',name))
	      ,@(if (eval-feature :translator)
		    `((tl:defun ,predicate (x)
			(tl:declare (tl:return-type t))
			(,predicate x)))
		  nil))
	  nil)
      ,@(if copier
	    (expand-copier name copier pool type raw-constructor slots)
	  nil)
      ,@(if reclaimer
	    (expand-reclaimer name reclaimer pool type slots)
	  nil)
      ',name)))

(defun expand-reclaimer (name reclaimer pool type slots)
  (let ((recycled-chain-slot (car (last slots))))
    `((tl:defun ,reclaimer (,name)
	(tl:declare (tl:return-type tl:void)
		    (tl:type ,(or type name) ,name))
	,@(loop for slot in slots
		for reader = (struct-slot-reader slot)
		for reclaimer = (struct-slot-reclaimer slot)
		when (and reclaimer reader (struct-slot-name slot))
		collect `(,reclaimer (,reader ,name)))
	(,(struct-slot-setter recycled-chain-slot) ,name ,pool)
	(tl:setq ,pool ,name)
	nil))))

(defun expand-copier (name copier pool type raw-constructor slots)
  (let* ((malloc-form
	  (cond ((null type)
		 `(malloc-class-instance ,name ,raw-constructor))
		((tl-subtypep type 'list)
		 `(tl:copy-list old))
		(t
		 `(tl:make-array
		   (tl:length old)
		   :element-type 
		   ',(element-type-of-vector-type type)))))
	 (new-form
	  (if pool
	      (let* ((chain-slot (car (last slots)))
		     (recycled (gensym)))
		`(tl:let ((,recycled ,pool))
		   (tl:cond (,recycled
			     (tl:setq ,pool (,(struct-slot-reader chain-slot)
					     ,recycled))
			     (,(struct-slot-setter chain-slot) ,recycled nil)
			     ,recycled)
			    (t
			     ,malloc-form))))
	    malloc-form))
	 (element-copy-forms
	  (cond ((null type)
		 (loop for slot in slots
		       for reader = (struct-slot-reader slot)
		       for setter = (struct-slot-setter slot)
		       for copier = (struct-slot-copier slot)
		       when (and (struct-slot-name slot) reader)
		       collect (if copier
				   `(,setter new (,copier (,reader old)))
				 `(,setter new (,reader old)))))
		((tl-subtypep type 'list)
		 nil)
		((tl-subtypep type 'simple-vector)
		 `((tl:replace-simple-vectors new old)))
		(t 
		 `((tl:dotimes (index (tl:length old))
	             (tl:setf (tl:aref new index)
			      (tl:aref old index))))))))
    `((tl:define-compiler-macro ,copier (x)
	`(tl:let* ((old ,x)
		   (new ,',new-form))
	   (tl:declare (tl:type ,',(or type name) old new))
	   ,@',element-copy-forms
	   new))
      (tl:defun ,copier (,name)
	(tl:declare (tl:return-type ,(or type name))
		    (tl:type ,(or type name) ,name)
		    (tl:consing-area tl:either))
	(,copier ,name)))))
  
(defun expand-readers-and-writers (name type named slots local-slots)
  (loop with local-start = (- (length slots) (length local-slots))
      for slot in (nthcdr local-start slots)
      for slot-offset from (+ local-start (if named 1 0))
      for slot-name = (struct-slot-name slot)
      for slot-reader = (struct-slot-reader slot)
      for slot-type = (struct-slot-type slot)
      for setter = (struct-slot-setter slot)
      for struct-var = name
      for new-value-var = 'tl::new-value
      nconc `((tl:declaim (tl:functional ,slot-reader))
	      (tl:define-compiler-macro ,slot-reader (,struct-var)
		,(cond ((null type)
			``(get-slot ,,struct-var ,',slot-name ,',name 
				    obj ,',slot-type nil))
		       ((tl-subtypep type 'list)
			``(tl:nth ,,slot-offset ,,struct-var))
		       (t
			``(tl:aref (tl:the ,',type ,,struct-var) 
				   ,,slot-offset))))
	      ;; Since the reader and setter functions will only be called when
	      ;; the compiler macro cannot be expanded, i.e. when there is a
	      ;; funcalled invocation, declare the argument and return types to
	      ;; be T to simplify funcall's argument handling job.  If the type
	      ;; is double-float, then this will entail consing up a new one.
	      ;; Declare this an "either" consing area to stop the compiler from
	      ;; complaining.
	      (tl:defun ,slot-reader (,struct-var)
		(tl:declare (tl:return-type t)
			    (tl:consing-area tl:either)
			    (tl:type t ,struct-var))
		(,slot-reader ,struct-var))
	      ,@(if (not (struct-slot-read-only slot))
		    `((tl:defsetf ,slot-reader ,setter)))
	      (tl:define-compiler-macro ,setter (,struct-var 
						 ,new-value-var)
	        `(set-slot ,,struct-var ,',slot-name ,',name obj
			   ,',slot-type nil ,,new-value-var))
	      (tl:defun ,setter (,struct-var ,new-value-var)
			(tl:declare (tl:return-type t)
				    (tl:type t ,struct-var ,new-value-var))
	        (,setter ,struct-var ,new-value-var)
		,new-value-var))))

(defun collect-constructors (name options slots) 
  (loop with collected = nil
	with slot-names = (loop for slot in slots
				for slot-name = (struct-slot-name slot)
				when slot-name collect slot-name)
	for elt in options
	do
    (cond ((eq elt :constructor)
	   (push (list nil) collected))
	  ((atom elt)
	   nil)
	  ((eq (cons-car elt) :constructor)
	   (cond ((null (cons-cdr elt))
		  (push (list nil) collected))
		 ((null (cons-cddr elt))
		  (push (list (cons-second elt) (cons 'tl:&key slot-names))
			collected))
		 (t
		  (push (cons-cdr elt) collected)))))
	finally 
	(when (null collected)
	  (setq collected 
		(list (list (intern (format nil "MAKE-~a" name))
			    (cons 'tl:&key slot-names)))))
	(return collected)))

(defun find-slot (slot-name slots)
  (loop for slot in slots
	do
    (when (eq (struct-slot-name slot) slot-name)
      (return slot))))

(defun supply-default-initial-binding (slot-name-or-binding
				       supply-default slots
				       constructor args)
  
  (let* ((slot-name (cond ((consp slot-name-or-binding)
			   (setq supply-default nil)
			   (car slot-name-or-binding))
			  (t slot-name-or-binding)))
	 (slot (find-slot slot-name slots)))
    (cond ((null slot)
	   (error "No slot named ~s in ~s" slot-name
		  (append `(:constructor ,constructor) 
			  (if args (list args) nil))))
	  (supply-default
	   (list slot-name (struct-slot-initial-form slot)))
	  (t slot-name-or-binding))))

(defconstant boa-lambda-list-keywords 
  '(tl:&optional tl:&rest tl:&aux tl:&key tl:&allow-other-keys))

(defun expand-constructors (name type named slots 
			    raw-constructor pool constructors)
  (loop with slot-count = (+ (length slots) (if named 1 0))
      for (constructor args) in constructors
      when constructor
      collect
	(loop with arglist = 
	      (loop with optional-or-key = nil
		  for arg in args
		  until (eq arg 'tl:&aux)
		  collect 
		    (cond ((memqp arg boa-lambda-list-keywords)
			   (setq optional-or-key (not (eq arg 'tl:&rest)))
			   arg)
			  (t
			   (supply-default-initial-binding 
			    arg optional-or-key slots
			    constructor args))))
	    with aux-vars = (loop for arg in (cdr (memq 'tl:&aux args))
				collect (supply-default-initial-binding 
					 arg t slots constructor args))
	    with boa-slot-names = (append
				   (loop for arg in arglist
				       unless (memqp arg boa-lambda-list-keywords)
				       collect (if (consp arg) (car arg) arg))
				   (loop for arg in aux-vars
				       collect (car arg)))
	    with body = nil
	    with struct-var = (gensym)
	    with struct-from-pool = (gensym)
	    with malloc-form = (cond ((null type) 
				      `(malloc-class-instance ,name ,raw-constructor))
				     ((tl-subtypep type 'list)
				      `(tl:make-list ,slot-count))
				     (t
				      `(tl:make-array 
					,slot-count 
					:element-type 
					',(element-type-of-vector-type
					   type))))
	    for slot-index from (if named 1 0)
	    for slot in slots
	    for slot-name = (struct-slot-name slot)
	    for value-form = (if (memqp slot-name boa-slot-names) 
				 slot-name
			       (struct-slot-initial-form slot))
	    for set-form 
	    = (cond ((null type)
		     `(set-slot 
		       ,struct-var ,slot-name ,name obj
		       ,(struct-slot-type slot) nil
		       ,value-form))
		    ((tl-subtypep type 'list)
		     value-form)
		    (t
		     `(tl:setf (tl:aref (tl:the ,type ,struct-var) ,slot-index)
			,value-form)))
	    do
	      (when slot-name
		(push set-form body))
	    finally
	      (setq body (reverse body))
	      (return 
		`(tl:defun ,constructor ,arglist
		   (tl:declare 
		    (tl:return-type ,(or type name))
		    ,@(loop for arg in arglist 
			  for arg-name = (if (consp arg) (car arg) arg)
			  unless (memqp arg-name boa-lambda-list-keywords)
			  collect
			    `(tl:type
			      ,(struct-slot-type 
				(find-slot arg-name slots))
			      ,arg-name))
		    (tl:consing-area tl:either))
		   (tl:let* (,@aux-vars
			     ,@(if pool
				   `((,struct-from-pool ,pool))
				 nil)
			     (,struct-var 
			      ,(if pool 
				   `(tl:cond (,struct-from-pool
					      (tl:setq ,pool 
						       (,(struct-slot-reader
							  (cons-car (last slots)))
							,struct-from-pool))
					      (,(struct-slot-setter
						 (cons-car (last slots)))
					       ,struct-from-pool nil)
					      ,struct-from-pool)
					     (t
					      ,malloc-form))
				   malloc-form)))
		     (tl:declare (tl:type ,(or type name) ,struct-var))
		     ,@(if type
			   (if (tl-subtypep type 'list)
			       `((tl:set-list-contents
				  ,@(if named
					(cons (list 'tl:quote name) body)
				      body)))
			     (if named
				 (cons `(tl:setf (tl:aref (tl:the ,type ,struct-var) 0)
					  ',name)
				       body)
			       body))
			 body)
		     ,struct-var))))))




;;; The function `compute-c-type-for-class' is called after all structures and
;;; functions have been defined, and just before a translation is begun
;;; (i.e. from reserve-global-identifiers).  The reason that C types for
;;; structures are defined so late is that the slot names must be determined
;;; only within the context of a translation, and the slot names are central to
;;; the definition of the C type.  In order to have repeatable translations
;;; (i.e. there are no diffs between two translations of the same sources), then
;;; the order in which global C identifiers are reserved must be deterministic,
;;; and not affected by incrementals Lisp compilations, as would occur if we
;;; defined C types when defstructs were defined.

(defun compute-c-type-for-class (class)
  (declare (special *global-c-namespace*))
  (let ((info (class-info class))
	(struct nil)
	(align 4)
	(c-namespace *global-c-namespace*))
    (push (list '(uint 24) (c-identifier-for-struct-slot
			    'type c-namespace c-namespace))
	  struct)
    (push (list '(uint 8) (c-identifier-for-struct-slot
			   'extended-type c-namespace c-namespace))
	  struct)
    (c-identifier-for-class class c-namespace c-namespace)
    (loop for slot in (struct-slot-descriptions info)
	  for c-slot-name = (c-identifier-for-struct-slot
			     (struct-slot-reader slot)
			     c-namespace c-namespace)
	  for lisp-slot-type = (struct-slot-original-type slot)
	  for c-slot-type = (struct-slot-c-type-for-lisp-type lisp-slot-type)
	  do
      (setf (struct-slot-c-accessor slot) c-slot-name)
      (setf (struct-slot-c-type slot) c-slot-type)
      (when (c-types-equal-p c-slot-type 'double)
	(setq align 8))
      (push (list c-slot-type c-slot-name) struct))
    (setf (struct-c-type info) (cons 'struct (nreverse struct)))
    (setf (struct-c-alignment info) align)))

(defun install-structure (name doc local-slot-descriptions copier include 
			       initial-offset predicate print-object 
			       print-function type reclaimer conc-name)
  (let ((struct (make-struct
		  :name name
		  :doc doc
		  :local-slot-descriptions local-slot-descriptions
		  :copier copier
		  :include include
		  :initial-offset initial-offset
		  :predicate predicate
		  :print-object print-object
		  :print-function print-function
		  :type type
		  :reclaimer reclaimer)))
    (setf (structure-info name) struct)
    (setf (struct-slot-descriptions struct) 
	  (collect-slot-descriptions struct conc-name))
    (cond (include
	   (struct-add-subtype include name))
	  ((not (eq name 'structure))
	   (struct-add-subtype 'structure name)))
    (pushnew name *all-classes*)
    name))

(install-structure
  'structure nil nil nil nil nil nil nil nil nil nil nil)
