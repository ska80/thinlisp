(in-package "TLI")

;;; Module: Bit-pack

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright (c) 1998 Artificial Creatures, Inc.
;;; Copyright (c) 1998 IS Robotics, Inc.

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>

;;; Author: David Sotkowitz

;;;; Type Info for ThinLisp CLOS.

;;;
;;; This is the Bit-Packed Encoding Algorithm from:
;;;   A. Krall, J. Vitek, and R.N. Horspool. Efficienyt Type Inclusion Tests. 
;;;   Proceedings of the Object-Oriented Programming Languages, Systems, and
;;;   Applications, OOPSALA '97, Atlanta, October 1997.
;;; A postscript version of the paper can be found at:
;;;   http://www.complang.tuwien.ac.at/andi/typecheck/
;;;

;;; Stub

(defun bucket-elements (bucket) 
  (declare (ignore bucket))
  nil)

(defun set-bucket-elements (bucket new)
  (declare (ignore bucket))
  new)

(defsetf bucket-elements set-bucket-elements)

(defun bucket-joins (bucket)
  (declare (ignore bucket))
  nil)

(defun set-bucket-joins (bucket new)
  (declare (ignore bucket))
  new)

(defsetf bucket-joins set-bucket-joins)




;;; Its value should be something like (integer-length most-positive-fixnum).

(defconstant bpe-word-size (integer-length most-positive-fixnum))



;;; The following should be labels within type-level when and if L has them:

(defun cardinality-of-ancestors (cardinality inner-type)
  (mapcar #'(lambda (x) 
              (when x
                (incf cardinality)
                (setf cardinality (cardinality-of-ancestors cardinality x))))
          (type-bpe-info-parents inner-type))
  cardinality)




;;; The level of a type is the maximum cardinality of its ancestors. The point
;;; is that a descendent will always have a greater type-level than one of its
;;; ancestors.

;;; The exact values are irrelevant, they only need to be monotonically
;;; increasing as you traverse down the inheritance hierarchy. This is just a
;;; helper for reverse-level-order and level-order immediately below.

;; could use class-precedence list

(defun type-level (typ)
  (cardinality-of-ancestors 0 typ))




;;; Generates an ordered list, higher type level (largest # of ancestors) first

(defun reverse-level-order (types)
  (stable-sort (copy-list types) #'(lambda (x y) (>= (type-level x) (type-level y)))))




;;; Generates an ordered list, lower type level (least # of ancestors) first

(defun level-order (types)
  (stable-sort (copy-list types) #'(lambda (x y) (<= (type-level x) (type-level y)))))




;;; The following should be labels within calculate-sets-of-join-and-spine-types
;;; when and if L has them:

(defun descendents-not-multi-p (typ)
  (dolist (x (type-bpe-info-children typ) 't)
    (when (and (not (null x))
               (or (> (length (type-bpe-info-parents x)) 1) (not (descendents-not-multi-p x))))
      (return nil))))




;;; The following should be labels within calculate-sets-of-join-and-spine-types
;;; when and if L has them:

(defun collect-parents (child-type result-list)
  (dolist (parent-type (type-bpe-info-parents child-type) result-list)
    (unless (or (null parent-type) 
                (member parent-type result-list :test #'eq))
      (setf result-list 
            (collect-parents parent-type 
                             (cons parent-type result-list))))))




;;; The following should all be flets within assign-buckets when and if L has them:

;;; A join type is a type with multiple parents which has only single subtyping descendents.
;;; A spine type is any ancestor of a join type

(defun calculate-sets-of-join-and-spine-types (set-of-types)
  (let ((set-of-join-types nil)
        ;; A spine type is any ancestor of a join type
        (set-of-spine-types))
    ;;; multi-p is 't if the cardinality of the type's set of its parents is > 1.
    (macrolet ((multi-p (typ) `(> (length (type-bpe-info-parents ,typ)) 1)))
      (dolist (x set-of-types)
        (when (and (multi-p x) (descendents-not-multi-p x))
	  (setf set-of-spine-types (collect-parents x set-of-spine-types))
	  (push x set-of-join-types))))
    (values set-of-join-types set-of-spine-types)))




;;; A plain type is a member of the set-of-types which is neither a join type
;;; nor a spine type

(defun calculate-set-of-plain-types (set-of-types set-of-join-types 
						  set-of-spine-types)
  (let ((result-set nil))
    (dolist (x set-of-types)
      (unless (or (memqp x set-of-join-types)
		  (memqp x set-of-spine-types))
	(push x result-set)))
    result-set))




;;; The following should be a labels within assign-buckets when and if L has them:

(defun set-descendents-buckets-used (types)
  (let (children)
    (dolist (x types)
      (setf children (type-bpe-info-children x))
      (dolist (y children)
        (setf (type-bpe-info-buckets-used y) 
              (union (type-bpe-info-buckets-used y)
                     (type-bpe-info-buckets-used x))))
      (set-descendents-buckets-used children))))




;;; Given a set of types, allocate and assign buckets for them. No two types in the same
;;; bucket may have common descendents, thus type identifiers need only be unique within
;;; a bucket.

(defun assign-buckets (set-of-types &optional (verbose nil))
  ;;
  ;; Assumption: all types have their parents and children previously calculated.
  ;;
  
  ;; initialize type-entries
  ;;
  ;; foreach (x in the set T) 
  ;;    x.joins := { }
  ;;    x.used  := { }
  ;;
  (dolist (x set-of-types)
    (setf (type-bpe-info-joins x) nil) 
    (setf (type-bpe-info-buckets-used x) nil))
  
  (multiple-value-bind (set-of-join-types set-of-spine-types)
                       (calculate-sets-of-join-and-spine-types set-of-types)
    
    (let ((buckets 		nil)
          (set-of-plain-types	(calculate-set-of-plain-types 
                                 set-of-types set-of-join-types set-of-spine-types)))
      
      ;; update parents joins
      ;;
      ;; foreach (x in the set join(T))
      ;;    foreach (y in the set parents(x)) 
      ;;       y.joins := y.joins U {x}
      ;;
      (dolist (x set-of-join-types)
        (dolist (y (type-bpe-info-parents x))
	  (when y
	    (setf (type-bpe-info-joins y) (adjoin x (type-bpe-info-joins y))))))
      
      ;; foreach (x in the set reverse-level-order( spine(T) ))
      ;;    found := false
      ;;    foreach (b in the set buckets)
      ;;       if( card (b.elements) < 255 & x.joins ^ b.joins = {} )
      ;;          found := true
      ;;		b.elements := b.elements U {x}
      ;;		b.joins := b.joins U x.joins
      ;;		break;
      ;;	  if(found = false)
      ;;	      b := new Bucket
      ;;	      buckets := buckets U {b}
      ;;	      b.elements := b.elements U {x}
      ;;	      b.joins := x.joins
      ;;    foreach (y in the set parents(x))
      ;;	      y.joins := y.joins U x.joins
       
      ;; Bucket assignment starts with spine types since other types depend on
      ;; them. Spine types are visited in reverse topological order since the
      ;; lower types are less likely to conflict with one another. The reverse
      ;; order also allows us to build join sets while assigning buckets.
      
      (dolist (x (reverse-level-order set-of-spine-types))
        (let ((found nil))
	  (macrolet ((add-type-description-to-bucket (entry bucket)
		       `(progn 
			  (setf (bucket-elements ,bucket) 
				(adjoin ,entry (bucket-elements ,bucket)))
			  (setf (bucket-joins ,bucket)
				(union (type-bpe-info-joins ,entry) (bucket-joins ,bucket)))
			  (setf (type-bpe-info-buckets-used ,entry) 
				(adjoin ,bucket (type-bpe-info-buckets-used ,entry)))))
		     (intersect-p (list1 list2)
		       `(dolist (item ,list1 nil)
			  (when (member item ,list2)
			    (return 't)))))
            (dolist (b buckets)
	      ;; When bucket is not full, and no 2 spine types have no join
	      ;; type in common,the spine type may be added to the bucket.
	      (when (and (< (length (bucket-elements b)) 255) 
			 (not (intersect-p (type-bpe-info-joins x) (bucket-joins b))))
		(setf found 't)
		(add-type-description-to-bucket x b)
		(return)))
	    (unless found
	      (let ((b (make-bucket)))
		(push b buckets)
		(add-type-description-to-bucket x b))))
	  ;; After assigning a bucket to a type, the join sets of the parents are
	  ;; updated with the join sets of the current type.
	  (dolist (y (type-bpe-info-parents x))
	    (setf (type-bpe-info-joins y)
		  (union (type-bpe-info-joins y) (type-bpe-info-joins x))))))

      
      (when verbose
        (format t "~%assign-buckets: Done assigning buckets to splines"))
      
      ;; For each type compute set of buckets already used by its ancestors
      ;; Pass in the subset of the set-of-spine-types which have no parents

      (let ((set-of-root-spine-types nil))
        (dolist (x set-of-spine-types)
          (unless (type-bpe-info-parents x)
            (setf set-of-root-spine-types (cons x set-of-root-spine-types))))
        (when verbose
          (format t "~%assign-buckets:parentless splines ~A" set-of-root-spine-types))
        (set-descendents-buckets-used set-of-root-spine-types))
      
      (when verbose
        (format t "~%assign-buckets: Done assigning buckets to splines descendents"))
      
      ;;  foreach (x in the set level-order( plain(T) U join (T)))
      ;;      found := false
      ;;      foreach (b in the set buckets)
      ;;	 if ( card (b.elements) < 255 & not(b S x.used))
      ;;            found := true
      ;;	    b.elements := b.elements U {x}
      ;;            x.used := x.used U {b}
      ;;	    break;
      ;;      if (found = false)
      ;;         b := new Bucket
      ;;         buckets := buckets U {b}
      ;;	 b.elements := b.elements U {x}
      ;;	 x.used := x.used U {b}
      ;;      foreach (y in the set children(x))
      ;;         y.used := y.used U x.used

      ;; Visit in level-order to ensure that buckets are assigned
      ;; to parents before children.

      (dolist (x (level-order (union set-of-plain-types set-of-join-types)))
        (let ((found nil))
	  ;; If the bucket is not full and is not already used by any of a
	  ;; type's ancestors, the bucket may be used for the type.
	  (dolist (b buckets)
	    (when (and (< (length (bucket-elements b)) 255) 
		       (null (member b (type-bpe-info-buckets-used x) :test #'eq)))
	      (setf found 't)
	      (setf (bucket-elements b) (adjoin x (bucket-elements b)))
	      (setf (type-bpe-info-buckets-used x) 
		    (adjoin b (type-bpe-info-buckets-used x)))
	      (return)))
	  (unless found
	    (let ((b (make-bucket)))
	      (push b buckets)
	      (setf (bucket-elements b) (adjoin x (bucket-elements b)))
	      (setf (type-bpe-info-buckets-used x) 
		    (adjoin b (type-bpe-info-buckets-used x)))))
	  (dolist (y (type-bpe-info-children x))
	    (setf (type-bpe-info-buckets-used y) 
		  (union (type-bpe-info-buckets-used y)
			 (type-bpe-info-buckets-used x))))))

      (when verbose
        (format t "~%Exiting assign-buckets"))
      buckets)))

(defun test-type-representation-buckets (number-of-bucket-words y x)
  (let ((empty (make-type-description
                :row (make-array number-of-bucket-words :initial-element 0)
                :bucket-position (type-description-bucket-position x)
                :bucket-word (type-description-bucket-word x)
                :bucket-mask (type-description-bucket-mask x))))
    
    (unless (and (eq (type-description-bucket-word x) (type-description-bucket-word y))
                 (zerop (logand (svref (type-description-row y) (type-description-bucket-word x))
                                (type-description-bucket-mask x))))
      
      (unless (type-subtypep y empty)
        (unless (or (type-subtypep x empty) 
                    (type-subtypep y x))
          (error "Bucket conflict: Child ~A has different non-empty value #x~X for parent ~A."
                 (type-description-name y)
                 (logand (svref (type-description-row x) (type-description-bucket-word x))
                         (type-description-bucket-mask x))
                 (type-description-name x)))
        
        (dolist (parent (type-description-parents x))
          #+L (setf parent (%find-type-description parent))
          (setf (type-description-bucket-word empty) (type-description-bucket-word parent))
          (setf (type-description-bucket-mask empty) (type-description-bucket-mask parent))
          (setf (type-description-bucket-value empty) (type-description-bucket-value parent))
          (setf (type-description-bucket-position empty) (type-description-bucket-position parent))
          (unless (or (and (eq (type-description-bucket-word parent) (type-description-bucket-word y))
                           (zerop (logand (svref (type-description-row y) (type-description-bucket-word parent))
                                          (type-description-bucket-mask parent))))
                      (type-subtypep parent empty)
                      (type-subtypep y empty)
                      (type-subtypep y parent))
            (error "Bucket conflict: Child ~A has different non-empty value #x~X for ancestor ~A."
                   (type-description-name y)
                   (logand (aref (type-description-row parent) (type-description-bucket-word parent))
                           (type-description-bucket-mask parent))
                   (type-description-name parent))))))))
                                        
;;;
;;; Building type representations (runtime data structures)
;;;
(defun build-type-representations (set-of-types buckets &optional (testp nil))
  (macrolet ((integer-len (value)
               ;; In Common Lisp could be `(integer-length ,value)
               ;; In assign-buckets only allow 255 elements in a bucket, hence max is 8
               `(if (>= 0 (- ,value 1)) 1
                       (if (>= 0 (- ,value 3)) 2
                           (if (>= 0 (- ,value 7)) 3
                               (if (>= 0 (- ,value 15)) 4
                                   (if (>= 0 (- ,value 31)) 5
                                       (if (>= 0 (- ,value 63)) 6
                                           (if (>= 0 (- ,value 127)) 7 8))))))))
             )
    (let ((bucket-masks (make-array 8))
          (number-of-bucket-words
           (let ((current-bucket-word 1)
                 (position 0)
                 (bucket-cardinality 0))
             (dolist (b buckets current-bucket-word)
               ;; increment by prior bucket-cardinality
               (setf position (+ position bucket-cardinality))
               (setf bucket-cardinality (integer-len (length (bucket-elements b))))
               
               (when (> (+ bucket-cardinality position) bpe-word-size)
                 (incf current-bucket-word)
                 (setf position 0))))))

          ;; initialize bucket-masks
          (progn
            (setf (svref bucket-masks 0) 1)
            (setf (svref bucket-masks 1) 3)
            (setf (svref bucket-masks 2) 7)
            (setf (svref bucket-masks 3) 15)
            (setf (svref bucket-masks 4) 31)
            (setf (svref bucket-masks 5) 63)
            (setf (svref bucket-masks 6) 127)
            (setf (svref bucket-masks 7) 255))
      
      ;; foreach (b in the set buckets)
      ;;    c := 0		- intra-bucket counter, used as a type identifier
      ;;    n := n + 1
      ;;    foreach (x in the set b.elements)
      ;;       c := c + 1
      ;;	     x.type := new type-description
      ;;       x.type.bucket := n
      ;;	     x.type.row := new Array[1...P] of int8
      ;;                     for Bit-Packed Encoding, we use Array[1...B] of bpe-word-size
      ;;	     foreach (i in the set [1...P])
      ;;          x.type.row[i] := 0
      ;;	     x.type.row[x.type.bucket] := c
      ;;
      (macrolet
        ((calculate-bucket-mask (bucket-cardinality)
           ;; Could be `(1- (expt 2 ,bucket-cardinality)) in CommonLisp
           `(svref bucket-masks (1- ,bucket-cardinality)))
         (init-bucket-value (value row bucket-word bucket-shift-value) 
           `(setf (aref ,row ,bucket-word) (ash ,value ,bucket-shift-value)))
         )
        
        ;; Note: buckets can be visited in any order when creating the type-representation
        ;; The only constraint is that it match the order used by the number-of-bucket-words
        ;; calculation.
        (let ((current-bucket-word 0)
              (position 0)
              (bucket-cardinality 0)
              bucket-elements
              bucket-mask
              bucket-shift-value)
          (dolist (b buckets)
            (setf bucket-elements (bucket-elements b))
            ;; increment by prior bucket-cardinality
            (setf position (+ position bucket-cardinality))
            (setf bucket-cardinality (integer-len (length bucket-elements)))
            (setf bucket-mask (calculate-bucket-mask bucket-cardinality))
            
            ;; Does the current bucket fit within the space remaining in the word?
            (when (> (+ position bucket-cardinality) bpe-word-size)
              (incf current-bucket-word)
              (setf position 0))
            (setf bucket-shift-value (- bpe-word-size (integer-len bucket-mask) position))

            (let ((c 0))
              (dolist (bpe-typ bucket-elements)
                ;; c is the type-identifier. It is unique within a bucket. 
                ;; 0 means no type.
                (incf c)
                (let ((typ (type-bpe-info-rtinfo bpe-typ))
                      (new-row (make-array number-of-bucket-words :initial-element 0)))
                  (setf (type-description-row typ) new-row)
                  (setf (type-description-bucket-word typ) current-bucket-word)
                  (setf (type-description-bucket-position typ) position)
                  (setf (type-description-bucket-mask typ) (ash bucket-mask bucket-shift-value))
                  (init-bucket-value c new-row current-bucket-word bucket-shift-value)
                  (setf (type-description-bucket-value typ) (svref new-row current-bucket-word))))))))
      
      ;; foreach (x in the set level-order(types)
      ;;    foreach (y in the set children(x))
      ;;       foreach (i in the set [1...P])  * Note [1...P] is the PE, not the BPE description
      ;;          y.type.row[i] := y.type.row[i] | x.type.row[i]	
      ;; Note: | is the logical-or operator
      ;;
      (let (parent-value child-value)
        (dolist (x (level-order (copy-list set-of-types)))
          (dolist (y (type-bpe-info-children x))
            (let ((x-desc (type-bpe-info-rtinfo x))
                  (y-desc (type-bpe-info-rtinfo y)))
              (when testp 
                (test-type-representation-buckets number-of-bucket-words y-desc x-desc))
              (do ((i 0 (incf i)))
                  ((> i (- number-of-bucket-words 1)))
                (setf parent-value (aref (type-description-row x-desc) i))
                (setf child-value (aref (type-description-row y-desc) i))
                (setf (aref (type-description-row y-desc) i)
                      (logior child-value parent-value))))))))))

;;; If a child type is already defined, then the type descriptor of
;;; the child refers to the old parent type descriptor.
(defun update-child-type-descriptions (old-type-description new-type-description)
  (maphash #'(lambda (type-name type-description-value)
               #-L(declare (ignore type-name))
               (let ((parents (type-description-parents 
                               #+L (cdr type-description-value)
                               #-L type-description-value)))
                 (do ((r parents (rest r)))
                     ((or (null r) 
                          (eq #+L (%find-type-description (car r)) #-L (type-definition (car r))
                              old-type-description))
                      (when r (setf (car r) new-type-description)) r))))
           #+L *type-table*
           #-L (global-type-definition-table)))

#+L
(defun bpe-update-type-description (type-description-container)
 (let ((type-description (%type-container-description type-description-container)))
   ;; In L, just return the type-description to be updated
   type-description))

#-L
(defun bpe-update-type-description (type-description)
  ;; In the cold load, create a new type-description
  (let* ((type-description-name (type-description-name type-description))
         (new-type-description
          (make-type-description :name 		type-description-name 
                                 :parents 	(type-description-parents type-description)
                                 :dataslots 	(type-description-dataslots type-description))))
    ;; No need to update-child-type-descriptions here since we have just built the table.
    (record-type-definition type-description-name new-type-description)
    (setf (gethash type-description-name (global-type-definition-table)) new-type-description)
    new-type-description))

;;;
;;; Given the set of types and a hash table keyed by a type-description with new type-bpe-info
;;; for the type as the value, fill in the children and parents fields for all the types.
;;; 
(defun bpe-set-parents-and-children (set-of-types type-description-to-type-bpe-info-table)
  (dolist (typ set-of-types)
    (let ((type-description (type-bpe-info-rtinfo typ)))
      (dolist (desc-parent (type-description-parents type-description))
        (let* ((parent-description #+L (%find-type-description desc-parent)
                                   #-L (type-definition desc-parent))
               (parent (gethash parent-description type-description-to-type-bpe-info-table)))
          (setf (type-bpe-info-parents typ)
                (nconc (type-bpe-info-parents typ) (cons parent nil)))
          (when parent
            (setf (type-bpe-info-children parent)
                  (nconc (type-bpe-info-children parent) (cons typ nil)))))))))

;;
;; Top-level function called by the coldloader and the fasloader
(defun build-type-table ()
  (#+L with-no-suspension #-L progn
   (let* ((type-description-to-type-bpe-info-table (make-hash-table :test #'equal))
          (verbose	nil)
          (set-of-types	
           (let ((result nil))
             (maphash 
              #'(lambda (k v)
                  #-L(declare (ignore k))
                  (let* ((type-description  (bpe-update-type-description v))
                         (type-bpe-info	    (make-type-bpe-info :rtinfo type-description)))
                    (setf (gethash type-description type-description-to-type-bpe-info-table) type-bpe-info)
                    (setf result (nconc result (list type-bpe-info)))))
              #-L(global-type-definition-table)
              #+L *type-table*
              )           
             (bpe-set-parents-and-children result type-description-to-type-bpe-info-table)
             result))
          (buckets	(assign-buckets set-of-types verbose))
          (testp 	't))
     (build-type-representations set-of-types buckets testp))))

#+L
(defun out-of-date-type-object-on-heap-p (changed-types)
  (let ((function #'(lambda (object) 
                      (when (structurep object)        ; quickly prune uninteresting objects
                        (dolist (type changed-types nil) 
                          (when (structurep object type)
                            (throw 'rebuild-image-for-type-table-p 't)))))))
    
    (when (catch 'rebuild-image-for-type-table-p
            (map-heap-objects function :gc nil)
            nil)                        ; none found
      ;; found an affected object.  retry the search after gc.
      (catch 'rebuild-image-for-type-table-p
        (map-heap-objects function :gc t)
        nil))))                         ; none found

;;;
;;; Incremental Updating Notes : should we need to make the tables incrementally updateable
;;; in the future.
;;;
;;; Destructive changes: modify type graph by adding/removing edges between existing vertices
;;; Additive changes: Only add new vertices and edges to the type graph.
;;;
;;; Compute new graph at subtype call to first "uninitialized" type.
;;; 
;;; No destructive changes : For a new type, each row has to be extended by an entry and a new
;;; row must be added.  
;;; Pre-allocate rows?
;;; 
;;; Destructive changes : Recomputing only necessary if we add new join types to previously
;;; existing types, otherwise update can be performed by extending rows providing number of
;;; bits required to represent a bucket is unchanged.
;;; 

#+ignore (progn

;;; The following is usd for testing only.

(defun init-type-graph ()
  (let* ((a-node (make-type-description :name 'a))
         (b-node (make-type-description :name 'b :parents (list a-node)))
         (c-node (make-type-description :name 'c :parents (list a-node)))
         (e-node (make-type-description :name 'e :parents (list a-node)))
         (g-node (make-type-description :name 'g :parents (list a-node)))
         (d-node (make-type-description :name 'd :parents (list c-node e-node)))
         (f-node (make-type-description :name 'f :parents (list e-node g-node)))
         (a2-node (make-type-description :name 'a2))
         (b2-node (make-type-description :name 'b2 :parents (list a2-node)))
         (c2-node (make-type-description :name 'c2 :parents (list a2-node)))
         (d2-node (make-type-description :name 'd2 :parents (list a2-node)))
         (e2-node (make-type-description :name 'e2 :parents (list a2-node)))
         (f2-node (make-type-description :name 'f2 :parents (list b2-node c2-node)))
         (g2-node (make-type-description :name 'g2 :parents (list c2-node d2-node)))
         (h2-node (make-type-description :name 'h2 :parents (list d2-node e2-node)))
         (a3-node (make-type-description :name 'a3))
         (b3-node (make-type-description :name 'b3 :parents (list a3-node)))
         (c3-node (make-type-description :name 'c3 :parents (list a3-node)))
         (d3-node (make-type-description :name 'd3 :parents (list a3-node)))
         (e3-node (make-type-description :name 'e3 :parents (list a3-node)))
         (f3-node (make-type-description :name 'f3 :parents (list b3-node c3-node)))
         (g3-node (make-type-description :name 'g3 :parents (list c3-node d3-node)))
         (h3-node (make-type-description :name 'h3 :parents (list d3-node e3-node)))
         (a4-node (make-type-description :name 'a4 :parents (list a3-node)))
         (b4-node (make-type-description :name 'b4 :parents (list a4-node f3-node)))
         (c4-node (make-type-description :name 'c4 :parents (list a4-node)))
         (d4-node (make-type-description :name 'd4 :parents (list a4-node)))
         (e4-node (make-type-description :name 'e4 :parents (list a4-node)))
         (f4-node (make-type-description :name 'f4 :parents (list b4-node c4-node)))
         (g4-node (make-type-description :name 'g4 :parents (list c4-node d4-node)))
         (h4-node (make-type-description :name 'h4 :parents (list d4-node e4-node)))
         (a0-node (make-type-description :name 'a0))
         (b0-node (make-type-description :name 'b0))
         (c0-node (make-type-description :name 'c0))
         (d0-node (make-type-description :name 'd0))
         (e0-node (make-type-description :name 'e0))
         (f0-node (make-type-description :name 'f0))
         (g0-node (make-type-description :name 'g0))
         (h0-node (make-type-description :name 'h0))
         (i0-node (make-type-description :name 'i0))
         (j0-node (make-type-description :name 'j0))
         (k0-node (make-type-description :name 'k0))
         (l0-node (make-type-description :name 'l0))
         (m0-node (make-type-description :name 'm0))
         (n0-node (make-type-description :name 'n0))
         (o0-node (make-type-description :name 'o0))
         (p0-node (make-type-description :name 'p0))
         (q0-node (make-type-description :name 'q0))
         (r0-node (make-type-description :name 'r0))
         (s0-node (make-type-description :name 's0))
         (t0-node (make-type-description :name 't0))
         (u0-node (make-type-description :name 'u0))
         (v0-node (make-type-description :name 'v0))
         (w0-node (make-type-description :name 'w0))
         (x0-node (make-type-description :name 'x0))
         (y0-node (make-type-description :name 'y0))
         (z0-node (make-type-description :name 'z0))
         (aa0-node (make-type-description :name 'aa0))

         (set-of-types (list a-node b-node c-node e-node g-node d-node f-node
                             a2-node b2-node c2-node e2-node g2-node d2-node f2-node h2-node
                             a3-node b3-node c3-node e3-node g3-node d3-node f3-node h3-node
                             a4-node b4-node c4-node e4-node g4-node d4-node f4-node h4-node
                             a0-node b0-node c0-node d0-node e0-node f0-node g0-node h0-node i0-node 
                             j0-node k0-node l0-node m0-node n0-node o0-node p0-node q0-node r0-node 
                             s0-node t0-node u0-node v0-node w0-node x0-node y0-node z0-node 
                             aa0-node)))
    set-of-types))

;;; The following should all be labels within test-typecheck when and if L has them:
(defun collect-set-of-subtypes (type set-of-subtypes)
  (setf set-of-subtypes (adjoin type set-of-subtypes))
  (dolist (parent (type-bpe-info-parents type))
    (setf set-of-subtypes 
          (collect-set-of-subtypes parent set-of-subtypes)))
  set-of-subtypes)
                     
(defun member-parents (type match-type)
  (let ((parents (type-bpe-info-parents type)))
    (when parents
      (or (member match-type parents :test #'eq)
          (dolist (parent parents nil)
            (when (member-parents parent match-type)
              (return 't)))))))

(defun l-user::test-typecheck (&optional (verbose nil) 
                                         (set-of-types (init-type-graph))
                                         (test-buckets 't))
  (when verbose
    (format t "~%Testing ~d types in set:~%" (length set-of-types)))
  (let* ((type-description-to-type-bpe-info-table (make-hash-table))
         (error-found-p nil)
         (set-of-bpe-info	
          (let ((result nil))
            (dolist (type-description set-of-types) 
              (let ((type-bpe-info (make-type-bpe-info :rtinfo type-description)))
                (setf (gethash type-description type-description-to-type-bpe-info-table) type-bpe-info)
                (setf result (nconc result (list type-bpe-info)))))
            (bpe-set-parents-and-children result type-description-to-type-bpe-info-table)
            result)))
    
    (macrolet ((get-type-name (x)
                 `(dolist (entry set-of-types)
                    (when (eq (type-bpe-info-rtinfo entry) ,x)
                      (return (type-description-name (type-bpe-info-rtinfo entry)))))))
      
      (build-type-representations 
       set-of-bpe-info (assign-buckets set-of-bpe-info verbose) test-buckets)
      
      (when verbose
        (format t "~%***** Description of Type Set *****~%"))
      
      ;;; The same macrolet get-bucket-value as in type-subtypep. 
      ;;; Here only for testing purposes.
      (macrolet ((get-bucket-value (sub-type-description type-description)
                   `(logand (aref (type-description-row ,sub-type-description) 
                                  (type-description-bucket-word ,type-description))
                            (type-description-bucket-mask ,type-description))))
        (let (desc parents)
          (dolist (entry set-of-bpe-info)
            (setf desc (type-bpe-info-rtinfo entry))
            (setf parents (type-description-parents entry))
            
            (when verbose
              (let (subtypep)
                (format
                 t "~%Class ~A id ~D word ~D position ~D mask ~D~%   parents: "
                 (type-description-name desc)
                 (get-bucket-value desc desc)
                 (type-description-bucket-word desc)
                 (type-description-bucket-position desc)
                 (type-description-bucket-mask desc))
                (if (type-description-parents desc)
                  (dolist (parent parents)
                    #+L(setf parent (%find-type-description parent))
                    (format t "~A " (type-description-name (type-bpe-info-rtinfo parent))))
                  (format t "<Root Node!>"))
                (format t "~%   subtypep: ")
                (dolist (inner-desc set-of-types)
                  (setf subtypep 
                        (type-subtypep desc inner-desc))           
                  (when subtypep
                    (format t " ~A" (type-description-name inner-desc))))
                (format t "~%   children: ")
                (if (type-bpe-info-children entry)
                  (dolist (child (type-bpe-info-children entry))
                    (format t "~A " (type-description-name (type-bpe-info-rtinfo child))))
                  (format t "<Childless!>"))))
            
            (let ((non-subtypes
                   ;; is there a type which is subtypep but not a subtype ?
                   (let (subtypep
                         (matches nil))
                     (dolist (match-desc set-of-types matches)
                       (setf subtypep 
                             (type-subtypep desc match-desc))               
                       (when (and subtypep 
                                  (not (member-parents desc match-desc))
                                  (not (eq match-desc desc)))
                         (setf matches (cons match-desc matches))))))
                  (missing-subtypes
                   ;; is there a subtype which is not subtypep?
                   (let ((matches nil))
                     (dolist (match-entry (collect-set-of-subtypes entry nil) matches)
                       (let ((match-desc (type-bpe-info-rtinfo match-entry)))
                         (unless (type-subtypep desc match-desc)
                           (setf matches (cons match-desc matches))))))))
              (when non-subtypes
                (error "Non-ancestors found which are subtypep of ~A~%   "
                        (type-description-name desc))
                (dolist (match-desc non-subtypes)
                  (format t " ~A" (type-description-name match-desc))))
              (when missing-subtypes
                (error "Ancestors found which are not subtypep of ~A~%   "
                        (type-description-name desc))
                (dolist (match-desc missing-subtypes)
                  (format t " ~A" (type-description-name match-desc))))
              (setf error-found-p (or error-found-p non-subtypes missing-subtypes)))))))
    (unless error-found-p
      (format t "~&~%No errors found."))
    (values)))

(defvar *set-of-classes* nil)
(defvar *alist-of-classes* nil)
(defvar *synonym-classes* nil)

#-L
(defmacro get-class-direct-superclasses (class)
  #+MCL `(ccl:class-direct-superclasses ,class)
  #+ALLEGRO `(clos:class-direct-superclasses ,class))

#-L
(defmacro get-class-direct-subclasses (class)
  #+MCL `(ccl:class-direct-subclasses ,class)
  #+ALLEGRO `(clos:class-direct-subclasses ,class))

#-L
(defun test-classes-body 
       (classes synonym-classes verbose-p savep test-buckets-p)
  (let* ((alist-of-classes
          ;; create an alist of class object to type-description
          (let ((classes-alist nil))
            (dolist (entry classes classes-alist)
              (setf classes-alist (cons (cons (cdr entry) (make-type-description :name (car entry))) classes-alist)))))
         (set-of-classes
          ;; Now that all class objects have type-description's, create the list of type-description's
          ;; and assign in the parent field.
          (let (incomplete-hierarchy 
                class entry parent-found
                (entries nil))
            (dolist (alist-entry alist-of-classes (progn (format t "~%") entries))
              (setf class (car alist-entry))
              (setf entry (cdr alist-entry))
              (setf incomplete-hierarchy nil)
              (unless (and class entry)
                (error "Unexpected NULL class or entry: ~A ~A" class entry))
              (when (and class entry)
                (let ((parents
                       (let ((parent-list nil))
                         (dolist (parent (get-class-direct-superclasses class) parent-list)
                           (setf parent-found (assoc parent alist-of-classes :test #'eq))
                           (unless parent-found
                             (setf incomplete-hierarchy 't)
                             (warn "Missing parent of ~A: ~A" class parent))
                           (when parent-found
                             (setf parent-list (cons (cdr (assoc parent alist-of-classes :test #'eq)) parent-list)))))))
                  (setf (type-description-parents entry) parents)))
              (unless incomplete-hierarchy		;; some builtin classes are just missing 
                (setf entries (cons entry entries)))))))
    (when savep
      (setf *synonym-classes* synonym-classes)
      (setf *set-of-classes* set-of-classes)
      (setf *alist-of-classes* alist-of-classes))
    (l-user::test-typecheck verbose-p set-of-classes test-buckets-p)))
  
#-L
(progn

#+MCL
(defun test-MCL-classes 
       (&optional (verbose-p nil) (savep 't) (test-buckets-p 't))
  (declare (special *set-of-classes* *alist-of-classes* *synonym-classes*))
  ;; Get class hierarchies for the classes:
  ;;   inspector::usual-inspector, inspector::formatting-inspector,
  ;;   ccl::select-dialog, and ccl::scroll-bar-dialog-item.
  (let ((ccl::*warn-if-redefine-kernel* nil))
    (load "ccl:inspector;inspector-class.lisp")
    (load "ccl:library;pop-up-menu.lisp")
    (load "ccl:lib;ccl-menus.lisp")
    (load "ccl:Library;scroll-bar-dialog-items.lisp"))

  (let ((classes nil)
        (synonym-classes nil))
    ;; Map over all classes in the hash table collecting the name and object
    (maphash #'(lambda (name class) 
                 (when (cl:find-class name nil)
                   (let ((found-class-name (class-name (if (consp class) (cdr class) class))))
                     (if (eq name found-class-name)
                       (push class classes)
                       ;; classes without their own class object; e.g. (find-class 'single-float) -> double-float
                       (push class synonym-classes)))))
             ccl::%find-classes%)
    (test-classes-body classes synonym-classes verbose-p savep test-buckets-p))
  (values))

#+ALLEGRO
(defun test-ALLEGRO-classes (&optional (verbose-p nil) (savep 't) (test-buckets-p 't))
  (let* ((root-class (find-class 't))
         (classes `( ,(cons 't root-class))))
    (labels ((add-descendents (parent)
               (loop for child in (get-class-direct-subclasses parent)
                     unless (or (rassoc child classes :test #'eq)
                                (null child))
                     do (progn
                          (setf classes (cons (cons (class-name child) child) classes))
                          (add-descendents child)))))
      (add-descendents root-class))
    (test-classes-body classes nil verbose-p savep test-buckets-p))
  (values))

)

)
