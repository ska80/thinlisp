(in-package "TL")

(defun svref (a b)
  (declare (simple-vector a)
	   (fixnum b)
	   (return-type t))
  (aref a b))

(defun uffda (x y)
  (svref x y))

(defun frob (x y)
  (declare (fixnum x y))
  (+ x y))

(defun frob-2 (x y)
  (declare (return-type fixnum)
	   (fixnum x))
  (+ x (the fixnum y)))

(defun frob-3 (x y)
  (declare (return-type fixnum) (fixnum x))
  (+ x y))

(defun frob-4 (x y)
  (declare (return-type fixnum) (fixnum x) (type t y))
  (+ x y))

(defun frob-5 (x y)
  (+ x y))

(defun uffda (x y z)
  (declare (fixnum x y z)
	   (return-type fixnum))
  (let ((a (+ x y))
	(b (- y z)))
    (declare (fixnum a b))
    (let ((c b))
      (declare (fixnum c))
      (+ a c))))

(defun uffda2 (x y z)
  (declare (fixnum x y z)
	   (return-type fixnum))
  (let* ((a (+ x y))
	 (b (- y z))
	 (d (+ a b)))
    (declare (fixnum a b d))
    (let ((c b))
      (declare (fixnum c))
      (+ a c))))

(defun lotsa-values ()
  (declare (return-type *))
  23)

(defun some-values ()
  (values 1 2 3))

(defun mvp1-test ()
  (multiple-value-prog1 (lotsa-values)
    (lotsa-values)))

(defun mvp1-test2 ()
  (declare (return-type fixnum))
  (multiple-value-prog1 (uffda2 1 2 3)
    (lotsa-values)))

(defun multiple-value-test ()
  (declare (return-type t))
  (multiple-value-bind (a b) (lotsa-values)
    (if b
	(+ a b)
	(if a
	    (+ a a)
	    nil))))

(defun mvb-2 ()
  (multiple-value-bind (a b) (values 1 2)
    (declare (number a b)
	     (fat-and-slow))
    (+ a b)))

(defun mvb-3 ()
  (declare (optimize (safety 3)))
  (multiple-value-bind (a b)
      (lotsa-values)
    (declare (fixnum a b))
    (> a b)))


(defun simple-return-from (a b)
  (declare (fixnum a b))
  (if (> a 1)
      (return-from simple-return-from a)
      (setq a (- a)))
  (+ a b))

(defun simple-return-from1 (a b)
  (declare (fixnum a b) (return-type (values fixnum)))
  (if (> a 1)
      (return-from simple-return-from a)
      (setq a (- a)))
  (+ a b))

(defun simple-unwind-protect (a b)
  (declare (fixnum a b))
  (unwind-protect
       (if (> a 23)
	   (+ a b)
	   (return-from simple-unwind-protect b))
    (simple-return-from a b)))

(defun unwind-protect-test (a b)
  (declare (fixnum a b))
  (tagbody
     (return-from unwind-protect-test
       (unwind-protect
	    (if (> a 23)
		(if (< b 12)
		    (+ a b)
		    (go other-exit))
		(return-from unwind-protect-test b))
	 (simple-return-from a b)))
     other-exit
     (return-from unwind-protect-test 23)))

(defun throw-test (a b)
  (throw a b))


(progn

(defvar thingee 1)

(defparameter thingee2 2)

(defconstant thingee3 3)

(defun thingee-all ()
  (declare (fat-and-slow))
  (+ thingee thingee2 thingee3)))

(defun a-string ()
  (declare (return-type t))
  "Yowsa")

(progn
  (defconstant byte-array
    #.(lisp:make-array 10 :element-type '(unsigned-byte 8) :initial-element 9))
  (defconstant double-byte-array
    #.(lisp:make-array 15 :element-type '(unsigned-byte 16) :initial-element 132)))

(progn
  (defconstant list-1 '(1 2 3 4 "Uffda" (5 nil . 8) 23))
  (defconstant list-2 (quote #.(let ((a (lisp:list 1 2 3))) (lisp:setf (lisp:cdr (lisp:last a)) a) a)))
  (defconstant list-3 (quote #.(let ((a (lisp:list nil 2 3))) (lisp:setf (lisp:car a) a) a))))

(defun and-test1 (a b)
  (and a (< a b)))

(defun and-test2 (a b)
  (declare (return-type t))
  (let ((result (and a b (< a b))))
    result))

(defun and-test3 (a b)
  (declare (return-type t))
  (if (and a b (not (< a b)))
      1
      2))

(defun or-test1 (a b c)
  (or a b c))

(defun or-test2 (a b c)
  (if (or a (progn (setq c 23) b) c)
      1 2))

(defun or-test3 (a b c)
  (declare (return-type t))
  (or a b c))

(defun set-second (a b)
  (setf (cdr (car (the cons a))) b))


(declare-system (gsi :library t :lisp-dir "lisp/")
  load
  bootstrap
  systems
  delta
  gsi-patches
  loop
  lisp-fixes
  tl-extension
  (tldebug :include-test :development)
  os-foreign
  basics
  os-time
  os-settings
  os-memory
  primitives
  characters
  utilities0
  utilities1
  utilities2
  utilities3
  os-error
  launch
  networks
  int1
  int2
  int3
  int4
  gsi-common
  rpc-common1
  rpc-common2
  gsi
  gsi-rpc1
  gsi-rpc2
  translate)
