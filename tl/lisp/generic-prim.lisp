(in-package "TL")

;;;; Module GENERIC-PRIM

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






;;;; Generic Functions Mirroring TLT-PRIM




(defun generic-aref (array index)
  (declare (type fixnum index)
	   (consing-area either)
	   (return-type t))
  (macrolet ((aref-typecase (array-var array-index)
               `(typecase ,array-var
                  ,@(loop for type-triple in tli::primitive-array-types
			  for type = (first type-triple)
                          collect
                          `(,type
			      (aref (the ,type ,array-var) ,array-index)))
                  (t
                   (error "Unrecognized array-type of ~s for AREF."
                          ,array-var)))))
    (aref-typecase array index)))


(defun generic-set-aref (array index value)
  (declare (type fixnum index)
	   (return-type t))
  (macrolet ((set-aref-typecase (array-var array-index new-value)
               `(typecase ,array-var
                  ,@(loop for type-triple in tli::primitive-array-types
			  for type = (first type-triple)
			  for elt-type = (second type-triple)
                          collect
                          `(,type
			      ;; should this call SET-AREF instead of (SETF-ing (AREF ...)) ???
			      (setf (aref (the ,type ,array-var) ,array-index)
				    (the ,elt-type ,new-value))))
                  (t
                   (error "Unrecognized array-type of ~s for SET-AREF."
                          ,array-var)))))
    (set-aref-typecase array index value)
    value))



(defun generic-elt (sequence index)
  (declare (type fixnum index)
	   (consing-area either)
	   (return-type t))
  (macrolet ((elt-typecase (sequence-var sequence-index)
               `(typecase ,sequence-var
		  (list (nth ,sequence-index (the list ,sequence-var)))
                  ,@(loop for type-triple in tli::primitive-array-types
			  for type = (first type-triple)
                          collect
                          `(,type
			      (aref (the ,type ,sequence-var) ,sequence-index)))
                  (t
                   (error "Unrecognized sequence type of ~s for ELT."
                          ,sequence-var)))))
    (elt-typecase sequence index)))

(defun generic-set-elt (sequence index value)
  (declare (type fixnum index)
	   (return-type t))
  (macrolet ((set-elt-typecase (sequence-var sequence-index new-value)
               `(typecase ,sequence-var
		  (list (setf (nth ,sequence-index (the list ,sequence-var))
			      ,new-value))
                  ,@(loop for type-triple in tli::primitive-array-types
			  for type = (first type-triple)
			  for elt-type = (second type-triple)
                          collect
                          `(,type
			      (setf (elt (the ,type ,sequence-var) ,sequence-index)
				    (the ,elt-type ,new-value))))
                  (t
                   (error "Unrecognized sequence type of ~s for SET-ELT."
                          ,sequence-var)))))
    (set-elt-typecase sequence index value)
    value))

