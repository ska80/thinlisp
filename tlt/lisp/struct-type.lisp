(in-package "L-SYSTEM")

;;; Module STRUCT-TYPE

;;; Copyright (c) 1999-2001 The ThinLisp Group
;;; Copyright, Artificial Creatures, Inc., 1998
;;; Copyright, IS Robotics, Inc., 1998

;;; This file is part of ThinLisp.

;;; ThinLisp is open source; you can redistribute it and/or modify it
;;; under the terms of the ThinLisp License as published by the ThinLisp
;;; Group; either version 1 or (at your option) any later version.

;;; ThinLisp is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; For additional information see <http://www.thinlisp.org/>


;;;; Type Info for the L implementation

;;;
;;; type-description : 
;;;   Runtime data structure encapsulating subtype relations of an object's type. A type
;;;   is a subtype of itself. This structure is placed as the beginning of a structure object
;;;   by %make-structure.
;;;
(defstruct type-description
  ;; #-L :type #-L 'cl:symbol
  (name nil)
  
  ;; parents are the direct ancestors of a type
  ;; #-L :type #-L 'cl:list
  (parents nil)
    
  ;; the ordered list of dataslot names in the structure
  ;; #-L :type #-L 'cl:list
  (dataslots nil)
  
  ;;;
  ;;; The rest of this data structure (which for simplicity, will be called a type-rep)
  ;;; is the functional equivalent of a matrix of types by types whose
  ;;; values are type identifiers. Type identifiers are small because they only need to be
  ;;; unique within a bucket. One dimension of the matrix is that each type has a type-rep.
  ;;; Within the type-rep itself, the other dimension is packed into buckets which are
  ;;; themselves packed into words. In actual usage, many buckets have only one identifier (i.e. 
  ;;; type) within them and therefore require only 1 bit in type-rep-row.
  ;;;
    
  ;; dimensions [1 ... B]. B is the number of words needed for the packing of all buckets.
  ;; B is obtained by packing the Packed-Encoding of the subtype relations.
  ;; #-L :type #-L '(cl:vector `(cl:unsigned-byte ,bpe-word-size))
  (row nil)
  
  ;; the word within row containing the type's bucket
  ;; #-L :type #-L '(cl:unsigned-byte 8)
  (bucket-word 0)
  
  ;; Mask for the type identifiers within a bucket. Implicitly tells us the bucket size.
  ;; Based on the largest identifier value in the bucket.
  ;; #-L :type #-L '(cl:unsigned-byte 8)
  (bucket-mask 0)
  
  ;; location within the word of the bucket
  ;; #-L :type #-L '(cl:unsigned-byte 5)
  (bucket-position 0)
  
  ;; The value for this type in the type comparison. The result of
  ;;    (svref (type-description-row this-type-description) 
  ;;           (type-description-bucket-word this-type-description))
  (bucket-value 0)

  ;; Needed by inheritance, so that the lexical context of the initialization is correct.
  (init-function nil)
  )

;;; runtime L hashtable Key : type name Entry: (type-name . type-description)
#+L
(defvar *type-table*)

#+L
(defun set-container-type-description (container type-description) 
  (setf (cdr container) type-description))

;; Needed by faslin.lisp

#+L
(defmacro make-type-container (typename type-description)
  `(cons ,typename ,type-description))

#+L
(defun %record-type-container (typename type-description)
  (setf (gethash typename *type-table*) (make-type-container typename type-description)))

;;;
;;; The name in the generated type-description will be NIL. A bucket-mask of -1
;;; will not match the type of any existing object.
;;;
#+L
(defun make-forward-reference-type-container (typename)
  (make-type-container typename (make-type-description :bucket-mask -1)))

;;;
;;; The value in the type table is a forward reference.
;;;
(defun forward-reference-type-p (type-description)
  (and (null (type-description-name type-description))
       (= (type-description-bucket-mask type-description) -1)))

#+L
(defun %find-type-container (typename)
  (or (gethash typename *type-table*)
      ;; Create a forward type reference. It does not match any object's type.
      ;; To avoid unwanted warnings do not use %record-type-container.
      (setf (gethash typename *type-table*) (make-forward-reference-type-container typename))))

(defmacro lsi:%type-container-description (container) `(cdr ,container))

#-L
(defmethod print-object ((object type-description) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (type-description-name object) (prin1 (type-description-name object) stream))))

(defun compare-type-description (type-description parents dataslots)
  (if (or (null type-description)
          (forward-reference-type-p type-description))
    (values nil t t)
    (let ((parents-unchanged-p   (equal parents   (type-description-parents   type-description)))
          (dataslots-unchanged-p (equal dataslots (type-description-dataslots type-description))))
      (values (and parents-unchanged-p dataslots-unchanged-p) parents-unchanged-p dataslots-unchanged-p))))


(defstruct type-bpe-info

  (rtinfo nil)

  ;; parents are the direct ancestors of a type
  ;; #-L :type #-L 'cl:list
  (parents nil)
  
  ;; children are the direct descendents of a type
  ;; #-L :type #-L 'cl:list
  (children nil)
    
  ;; A join type is a type with multiple parents which has only single
  ;; subtyping descendents. This is the set of all children (direct 
  ;; descendents) of this type which are join types. Used to calculate
  ;; the subtype hierarchy by the Bit-Packed Encoding Algorithm.
  ;; #-L :type #-L 'cl:list
  (joins nil)
  
  ;; The set of buckets used by this type and all of its ancestors. Used
  ;; to calculate the subtype hierarchy by the Bit-Packed Encoding Algorithm.
  ;; #-L :type #-L 'cl:list
  (buckets-used nil)
    
  )

#-L
(defmethod print-object ((object type-bpe-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (type-bpe-info-rtinfo object) (prin1 (type-description-name (type-bpe-info-rtinfo object)) stream))))

;;;
;;; Build bpe-time data structure. A bucket has 2 constraints:
;;; 1. No 2 bucket elements may share a common descendent.
;;; 2. The maximum size of a bucket. The algorithm in the paper allows
;;;    for 255 elements, or 1 byte at runtime.
;;;
(defstruct bucket
  ;; The set of all types (type-description's) in the bucket
  (elements nil)
  ;; The set of all join types of the bucket's bucket-elements
  (joins nil)
  )

;; used by structurep whixh needs to refer to the container so that it is 
;; not bound to a particular version of a type, but rather always compares against the most recent.
(defun type-container-subtypep (subtype-description type-container)
  (type-subtypep subtype-description (lsi:%type-container-description type-container)))

;;;
;;; The type inclusion test is (# precedes a compile time constant):
;;;
;;;   type-description type := object.type-description
;;;   int32 word := type.row[#bucket-word]
;;;   word := word >> #bucket-pos
;;;   word := word & #bucket-mask
;;;   if ( word = #tid )
;;;
;;; Return a boolean value indicating the relationship between type1 and type2:
;;; T: type1 is a subtype of type2.
;;; NIL: type1 is not a subtype of type2.
;;;
(defun type-subtypep (subtype-description type-description)
  ;;;
  ;;; There is a related Bit-packing macrolet init-bucket-value in defun 
  ;;; build-type-representations above, and also in the commented out test code below.
  ;;;
  (zerop (logand (logxor (svref (type-description-row subtype-description) 
                                (type-description-bucket-word type-description))
                         (type-description-bucket-value type-description))
                 (type-description-bucket-mask type-description))))

;;; this needs to be there as a runtime for L -- it shadows a fastcall
;;; --- kab, 5/13/99: Is this even used anymore?
#+L 
(defun %make-structure (type-header &rest args)
  (let ((struct (%make-filled-object (ltarget *lptr-structure-header*)
                                     (+ (length args) 1)
                                     type-header)))
    (do ((i 1 (1+ i))
         (argls args (rest argls)))
        ((null argls))
      (setf (%structure-ref struct i) (first argls)))
    struct))

#+L
(defun copy-structure (structure)
  (check-structure structure)
  (let* ((length (%vector-length structure))
         (copy (%make-filled-object (ltarget *lptr-structure-header*)
                                    length
                                    (%structure-header structure))))
    (do ((index 1 (1+ index)))
        ((<= length index) copy)
      (setf (%structure-ref copy index) (%structure-ref structure index)))))

;;;
;;; display-all-types is #+ignore'd because it is for debugging only and doesn't need to exist
;;; in a real image. 
;;; The l-system qualifiers are unnecessary within this file, but make it easier to cut and
;;; paste the function into an L listener while debugging.
;;; When type descriptions contain slot names, the L version can be data driven.
;;;
#+ignore
(defun display-all-types ( )
  (maphash #'(lambda (k v)
               (format t "~S ~A~%" k v)
               #-L(describe v)
               #+L(if (structurep (cdr v))
                    (progn
                      (format t "   name : ~S~%" (l-system:type-description-name (cdr v)))
                      (format t "   parents : ~A~%" (l-system:type-description-parents (cdr v)))
                      (format t "   dataslots : ~S~%" (l-system:type-description-dataslots (cdr v)))
                      (format t "   row : ~A~%" (l-system:type-description-row (cdr v)))
                      (format t "   bucket-word : ~A~%" (l-system:type-description-bucket-word (cdr v)))
                      (format t "   bucket-mask : ~A~%" (l-system:type-description-bucket-mask (cdr v)))
                      (format t "   bucket-position : ~A~%" (l-system:type-description-bucket-position (cdr v)))
                      (format t "   bucket-value : ~A~%" (l-system:type-description-bucket-value (cdr v)))
                      (format t "   init-function : ~A~%" (l-system:type-description-init-function (cdr v))))
                    (format t "Not structurep!"))
               (format t "~%~%"))
           #+L l-system:*type-table*
           #-L (l-system::global-type-definition-table)
           ))
