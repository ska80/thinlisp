(in-package "TL")

;;;
;;;
;;;   **********************************************************************
;;;   ****** Common Lisp ******** LOOP Iteration Macro *********************
;;;   **********************************************************************
;;;   **** (C) COPYRIGHT 1980, 1981 MASSACHUSETTS INSTITUTE OF TECHNOLOGY **
;;;   ******** THIS IS A READ-ONLY FILE! (ALL WRITES RESERVED) *************
;;;   **********************************************************************




;;;; LOOP Iteration Macro

;;; This is the "officially sanctioned" version of LOOP for running in
;;; Common Lisp.  It is a conversion of LOOP 829, which is fairly close to
;;; that released with Symbolics Release 6.1 (803).  This conversion was
;;; made by Glenn Burke (one of the original author/maintainers);  the
;;; work was performed at Palladian Software, in Cambridge MA, April 1986.
;;; 
;;; The current version of this file will be maintained at MIT, available
;;; for anonymous FTP on MC.LCS.MIT.EDU from the file "LSB1;CLLOOP >".  This
;;; location will no doubt change sometime in the future.
;;; 
;;; This file, like the LOOP it is derived from, has unrestricted
;;; distribution -- anyone may take it and use it.  But for the sake of
;;; consistency, bug reporting, compatibility, and users' sanity, PLEASE
;;; PLEASE PLEASE don't go overboard with fixes or changes.  Remember that
;;; this version is supposed to be compatible with the Maclisp/Zetalisp/NIL
;;; LOOP;  it is NOT intended to be "different" or "better" or "redesigned".
;;; Report bugs and propose fixes to BUG-LOOP@MC.LCS.MIT.EDU;
;;; announcements about LOOP will be made to the mailing list
;;; INFO-LOOP@MC.LCS.MIT.EDU.  Mail concerning those lists (such as requests
;;; to be added) should be sent to the BUG-LOOP-REQUEST and
;;; INFO-LOOP-REQUEST lists respectively.  Note the Change History page
;;; below...
;;; 
;;; LOOP documentation is still probably available from the MIT Laboratory
;;; for Computer Science publications office:
;;; 	LCS Publications
;;; 	545 Technology Square
;;; 	Cambridge, MA 02139
;;; It is Technical Memo 169, "LOOP Iteration Macro", and is very old.  The
;;; most up-to-date documentation on this version of LOOP is that in the NIL
;;; Reference Manual (TR-311 from LCS Publications);  while you wouldn't
;;; want to get that (it costs nearly $15) just for LOOP documentation,
;;; those with access to a NIL manual might photocopy the chapter on LOOP.
;;; That revised documentation can be reissued as a revised technical memo
;;; if there is sufficient demand.
;;; 


;;;; Change History

;;; [mhd@gensym] 30-Oct-86 20:30
;;;
;;;   This is now only meant to work with system AB.  It has been modified in the following
;;;   ways:
;;;
;;;   -Added CommonLisp syntax directive to mode line (for symbolics 7.0 sake).
;;;   -Commented out cond-like clause misplaced in a case statement in loop-do-collect.
;;;   -Made "AB-" be a prefix to loop generated gensyms in loop-gentemp.
;;;   -changed the package from loop to AB, and we shadow loop, defined differently as a
;;;    special form in CLtL, from package LISP and also shadow define-loop-macro, loop-
;;;    finish, define-loop-path, and define-loop-sequence-path if they exist (like on
;;;    a Lisp Machine).  The shadowing is not done here, by the way, but in the load.lisp
;;;    file, where all of that sort of package stuff is performed.
;;;    The idea is to use our own loop and shadow the system's version of loop, even if we
;;;    are on, say, a lisp machine, which tries to support both the official "dumb loop" in
;;;    Steele as well as "Loop -- the iteration macro" compatibly through hand-checking of
;;;    args a macro expansion time.  (This is ok to do, Steele seems to say.)  But why bother
;;;    using the native loop if you can get by perfectly well with this one version forever?
;;;    So that's what this does.  (This feature is what makes it "only meant to work with
;;;    system AB".)
;;;   -Commented out define-loop-macro macro definer both because it is of no use and because
;;;    it uses some little used, though not unneeded, defmacro features that TI has not put
;;;    in.
;;;    (See the code for further information).  Added an explicit, though trivial, defmacro
;;;    for loop itself which is common lisp and works on TI.
;;;
;;;   -Fixed 2nd arg to find-symbol to be a package instead of a symbol (the name of one).
;;;    (Some lisps are soft on this.)

;;; [mhd@gensym] 5/11/87
;;;   -added "using" feature of COLLECT x [INTO y] [USING z], where z is the name of a function
;;;    that is used like and behaves like Cons to be used to cons up the collection.

;;; [mhd@gensym] 8/11/87
;;;   -generate a (LET* ...) expression wherever it is equivalent to a ((lambda ...) ...)
;;;    expression.  This fixes a problem with at least one vendor's Common Lisp compiler.

;;; [mhd@gensym] 12/28/87
;;;   -removed old GCLISP remedial support
;;;   -changed all occurances of defun to defun-for-macro
;;;   -commented out (provide 'loop) at top-level -- inappropriate in our software

;;; [mhd@gensym] 12/29/87
;;;   -make define-loop-path be a noop when :no-macros is in *features*.
;;;   -none of the predefined loop sequences are useful, so I commented them out.

;;; [gsb@palladian] 30-apr-86 00:26  File Created from NIL's LOOP version 829
;;;------------------------------------------------------------------------
;;;------- End of official change history -- note local fixes below -------
;;;------------------------------------------------------------------------



;;;; Package setup


;(provide 'loop)


;;;The following symbols are documented as being available via SI:.  Far be
;;;it for us to define a package by that name, however we can do the
;;;following.  We will create a "loop-si-kludge" package (sounds like a
;;;fairly safe name), import the SI: symbols from there into LOOP, export
;;;them, define that people (use-package 'loop), and if they want to
;;;maintain source compatibility they can add the SI nickname the
;;;loop-si-kludge package.  How's that?

;(in-package 'loop-si-kludge)

;(export '(loop-tequal loop-tassoc loop-tmember loop-use-system-destructuring?
;	  loop-named-variable loop-simplep loop-simplep-1
;	  loop-sequencer loop-sequence-elements-path))

;<--mhd (in-package 'loop)

;(use-package '(loop-si-kludge))

;shadow?

;<--mhd (export '(loop loop-finish define-loop-macro define-loop-path
;<--mhd 	       define-loop-sequence-path))

;<--mhd  (export '(loop-tequal loop-tassoc loop-tmember loop-use-system-destructuring?
;<--mhd  	  loop-named-variable loop-simplep loop-simplep-1
;<--mhd  	  loop-sequencer loop-sequence-elements-path))

;require?


;;;; Macro Environment Setup


;;<--- Data-type?, the macro, removed by mhd.  It was not doing anything and
;;  prevented us from eliminating macros in run-time versions.


;;; The uses of this macro are retained in the CL version of loop, in case they are
;;; needed in a particular implementation.  Originally dating from the use of the
;;; Zetalisp COPYLIST* function, this is used in situations where, were cdr-coding
;;; in use, having cdr-NIL at the end of the list might be suboptimal because the
;;; end of the list will probably be RPLACDed and so cdr-normal should be used instead.
(defmacro loop-copylist* (l)
  `(copy-list ,l))


;;;; Random Macros

(defmacro loop-simple-error (unquoted-message &optional (datum nil datump))
  `(error ,(if datump "LOOP:  ~S ~A" "LOOP:  ~A")
	  ',unquoted-message ,@(and datump (list datum))))


(defmacro loop-warn (unquoted-message &optional (datum nil datump))
  (if datump
      `(warn ,(concatenate 'string "LOOP: " unquoted-message " -- ~{~S~^ ~}")
	     ,datum)
      `(warn ',(concatenate 'string "LOOP: " unquoted-message))))


(defmacro loop-pop-source () '(pop loop-source-code))


(defvar-for-macro loop-gentemp-counter 0)


(defmacro loop-gentemp (&optional (pref ''loopvar-))
  `(make-symbol
     (format
       nil
       (if (eval-feature :translator)
	   "TL-~a"
	   "TL-~a~d")
       (symbol-name ,pref)
       (prog1 loop-gentemp-counter		; unnec. arg in some cases
	      (incf loop-gentemp-counter)))))

	   




;;;; Setq Hackery

; Note:  LOOP-MAKE-PSETQ is NOT flushable depending on the existence
; of PSETQ, unless PSETQ handles destructuring.  Even then it is
; preferable for the code LOOP produces to not contain intermediate
; macros, especially in the PDP10 version.

(defun-for-macro loop-make-psetq (frobs)
    (and frobs
	 (loop-make-setq
	    (list (car frobs)
		  (if (null (cddr frobs)) (cadr frobs)
		      `(prog1 ,(cadr frobs)
			      ,(loop-make-psetq (cddr frobs))))))))


(defvar-for-macro loop-use-system-destructuring?
    nil)

(defvar-for-macro loop-desetq-temporary)

; Do we want this???  It is, admittedly, useful...
;(defmacro loop-desetq (&rest x)
;  (let ((loop-desetq-temporary nil))
;     (let ((setq-form (loop-make-desetq x)))
;	(if loop-desetq-temporary
;	    `((lambda (,loop-desetq-temporary) ,setq-form) nil)
;	    setq-form))))


(defun-for-macro loop-make-desetq (x)
   (if loop-use-system-destructuring?
       (cons (do ((l x (cddr l))) ((null l) 'setq)
	       (or (and (not (null (car l))) (symbolp (car l)))
		   (return 'desetq)))
	     x)
       (do ((x x (cddr x)) (r nil) (var) (val))
	   ((null x) (and r (cons 'setq r)))
	 (setq var (car x) val (cadr x))
	 (cond ((and (not (atom var))
		     (not (atom val))
		     (not (and (member (car val) '(car cdr cadr cddr caar cdar))
			       (atom (cadr val)))))
		  (setq x (list* (or loop-desetq-temporary
				     (setq loop-desetq-temporary
					   (loop-gentemp 'loop-desetq-)))
				 val var loop-desetq-temporary (cddr x)))))
	 (setq r (nconc r (loop-desetq-internal (car x) (cadr x)))))))


(defun-for-macro loop-desetq-internal (var val)
  (cond ((null var) nil)
	((atom var) (list var val))
	(t (nconc (loop-desetq-internal (car var) `(car ,val))
		  (loop-desetq-internal (cdr var) `(cdr ,val))))))


(defun-for-macro loop-make-setq (pairs)
    (and pairs (loop-make-desetq pairs)))


(defparameter-for-macro loop-keyword-alist			;clause introducers
     '(	(named loop-do-named)
	(initially loop-do-initially)
	(finally loop-do-finally)
	(nodeclare loop-nodeclare)
	(do loop-do-do)
	(doing loop-do-do)
	(return loop-do-return)
	(collect loop-do-collect list)
	(collecting loop-do-collect list)
	(append loop-do-collect append)
	(appending loop-do-collect append)
	(nconc loop-do-collect nconc)
	(nconcing loop-do-collect nconc)
	(count loop-do-collect count)
	(counting loop-do-collect count)
	(sum loop-do-collect sum)
	(summing loop-do-collect sum)
	(maximize loop-do-collect max)
	(minimize loop-do-collect min)
	(always loop-do-always nil) ;Normal, do always
	(never loop-do-always t)    ; Negate the test on always.
	(thereis loop-do-thereis)
	(while loop-do-while nil while)	    ; Normal, do while
	(until loop-do-while t until)	    ; Negate the test on while
	(when loop-do-when nil when)	    ; Normal, do when
	(if loop-do-when nil if)    ; synonymous
 	(unless loop-do-when t unless)	    ; Negate the test on when
	(with loop-do-with)))


(defparameter-for-macro loop-iteration-keyword-alist
    `((for loop-do-for)
      (as loop-do-for)
      (repeat loop-do-repeat)))




;;; I changed the CAR in the clause for IN to be CAR-OF-CONS for better speed.
;;; -jra 5/1/91

(defparameter-for-macro loop-for-keyword-alist			;Types of FOR
     '( (= loop-for-equals)
        (first loop-for-first)
	(in loop-list-stepper car-of-cons)
	(on loop-list-stepper nil)
	(from loop-for-arithmetic from)
	(downfrom loop-for-arithmetic downfrom)
	(upfrom loop-for-arithmetic upfrom)
	(below loop-for-arithmetic below)
	(to loop-for-arithmetic to)
	(being loop-for-being)))

(defvar-for-macro loop-prog-names)


(defvar-for-macro loop-macro-environment)	;Second arg to macro functions,
					;passed to macroexpand.

(defvar-for-macro loop-path-keyword-alist nil)		; PATH functions
(defvar-for-macro loop-named-variables)			; see LOOP-NAMED-VARIABLE
(defvar-for-macro loop-variables)			;Variables local to the loop
(defvar-for-macro loop-declarations)			; Local dcls for above
(defvar-for-macro loop-nodeclare)			; but don't declare these
(defvar-for-macro loop-variable-stack)
(defvar-for-macro loop-declaration-stack)
(defvar-for-macro loop-desetq-crocks)			; see loop-make-variable
(defvar-for-macro loop-desetq-stack)			; and loop-translate-1
(defvar-for-macro loop-prologue)				;List of forms in reverse order
(defvar-for-macro loop-wrappers)				;List of wrapping forms, innermost first
(defvar-for-macro loop-before-loop)
(defvar-for-macro loop-body)				;..
(defvar-for-macro loop-after-body)			;.. for FOR steppers
(defvar-for-macro loop-epilogue)				;..
(defvar-for-macro loop-after-epilogue)			;So COLLECT's RETURN comes after FINALLY
(defvar-for-macro loop-conditionals)			;If non-NIL, condition for next form in body
  ;The above is actually a list of entries of the form
  ;(cond (condition forms...))
  ;When it is output, each successive condition will get
  ;nested inside the previous one, but it is not built up
  ;that way because you wouldn't be able to tell a WHEN-generated
  ;COND from a user-generated COND.
  ;When ELSE is used, each cond can get a second clause

(defvar-for-macro loop-when-it-variable)			;See LOOP-DO-WHEN
(defvar-for-macro loop-never-stepped-variable)		; see LOOP-FOR-FIRST
(defvar-for-macro loop-emitted-body?)			; see LOOP-EMIT-BODY,
						; and LOOP-DO-FOR
(defvar-for-macro loop-iteration-variables)		; LOOP-MAKE-ITERATION-VARIABLE
(defvar-for-macro loop-iteration-variablep)		; ditto
(defvar-for-macro loop-collect-cruft)			; for multiple COLLECTs (etc)
(defvar-for-macro loop-source-code)
(defvar-for-macro loop-duplicate-code nil)  ; see LOOP-OPTIMIZE-DUPLICATED-CODE-ETC


;;;; Construct a value return


(defun-for-macro loop-construct-return (form)
  (if loop-prog-names
      `(return-from ,(car loop-prog-names) ,form)
      `(return ,form)))

;;;; Token Hackery

;Compare two "tokens".  The first is the frob out of LOOP-SOURCE-CODE,
;the second a symbol to check against.

(defun-for-macro loop-tequal (x1 x2)
  (and (symbolp x1) (string= x1 x2)))


(defun-for-macro loop-tassoc (kwd alist)
  (and (symbolp kwd) (assoc kwd alist :test #'string=)))


(defun-for-macro loop-tmember (kwd list)
  (and (symbolp kwd) (member kwd list :test #'string=)))



;(defmacro define-loop-macro (keyword)
;  "Makes KEYWORD, which is a LOOP keyword, into a Lisp macro that may
;introduce a LOOP form.  This facility exists mostly for diehard users of
;a predecessor of LOOP.  Unconstrained use is not advised, as it tends to
;decrease the transportability of the code and needlessly uses up a
;function name."
;  (or (eq keyword 'loop)
;      (loop-tassoc keyword loop-keyword-alist)
;      (loop-tassoc keyword loop-iteration-keyword-alist)
;      (loop-simple-error "not a loop keyword - define-loop-macro" keyword))
;  `(defmacro ,keyword (&whole whole-form &rest keywords-and-forms &environment env)
;     (declare (ignore keywords-and-forms))
;     (loop-translate whole-form env)))
;<--mhd (Don't need this facility -- doesn't work on TI)


;(define-loop-macro loop)
;<--mhd (Don't need this facility -- doesn't work on TI)





(defmacro loop (&environment env &rest keywords-and-forms)
  (loop-translate keywords-and-forms env))


(defmacro loop-finish () 
  "Causes the iteration to terminate \"normally\", the same as implicit
termination by an iteration driving clause, or by use of WHILE or
UNTIL -- the epilogue code (if any) will be run, and any implicitly
collected result will be returned as the value of the LOOP."
  '(go end-loop))

(defun-for-macro loop-translate (x loop-macro-environment)
  (loop-translate-1 x))


(defun-for-macro loop-end-testify (list-of-forms)
    (if (null list-of-forms) nil
	`(when ,(if (null (cdr (setq list-of-forms (nreverse list-of-forms))))
		    (car list-of-forms)
		    (cons 'or list-of-forms))
	   (go end-loop))))


(defun-for-macro loop-optimize-duplicated-code-etc (&aux before after groupa groupb a b
					       lastdiff)
    (do ((l1 (nreverse loop-before-loop) (cdr l1))
	 (l2 (nreverse loop-after-body) (cdr l2)))
	((equal l1 l2)
	   (setq loop-body (nconc (delete nil l1) (nreverse loop-body))))
      (push (car l1) before) (push (car l2) after))
    (cond ((not (null loop-duplicate-code))
	     (setq loop-before-loop (nreverse (delete nil before))
		   loop-after-body (nreverse (delete nil after))))
	  (t (setq loop-before-loop nil loop-after-body nil
		   before (nreverse before) after (nreverse after))
	     (do ((bb before (cdr bb)) (aa after (cdr aa)))
		 ((null aa))
	       (cond ((not (equal (car aa) (car bb))) (setq lastdiff aa))
		     ((not (loop-simplep (car aa)))	;Mustn't duplicate
		      (return nil))))
	     (cond (lastdiff  ;Down through lastdiff should be duplicated
		    (do nil (nil)
		      (and (car before) (push (car before) loop-before-loop))
		      (and (car after) (push (car after) loop-after-body))
		      (setq before (cdr before) after (cdr after))
		      (and (eq after (cdr lastdiff)) (return nil)))
		    (setq loop-before-loop (nreverse loop-before-loop)
			  loop-after-body (nreverse loop-after-body))))
	     (do ((bb (nreverse before) (cdr bb))
		  (aa (nreverse after) (cdr aa)))
		 ((null aa))
	       (setq a (car aa) b (car bb))
	       (cond ((and (null a) (null b)))
		     ((equal a b)
			(loop-output-group groupb groupa)
			(push a loop-body)
			(setq groupb nil groupa nil))
		     (t (and a (push a groupa)) (and b (push b groupb)))))
	     (loop-output-group groupb groupa)))
    (and loop-never-stepped-variable
	 (push `(setq ,loop-never-stepped-variable nil) loop-after-body))
    nil)


(defun-for-macro loop-output-group (before after)
    (and (or after before)
	 (let ((v (or loop-never-stepped-variable
		      (setq loop-never-stepped-variable
			    (loop-make-variable
			      (loop-gentemp 'loop-iter-flag-) t nil)))))
	    (push (cond ((not before)
			  `(unless ,v (progn ,@after)))
			((not after)
			  `(when ,v (progn ,@before)))
			(t `(cond (,v ,@before) (t ,@after))))
		  loop-body))))


(defun-for-macro loop-translate-1 (loop-source-code)
  (and (eq (car loop-source-code) 'loop)
       (setq loop-source-code (cdr loop-source-code)))
  (do ((loop-iteration-variables nil)
       (loop-iteration-variablep nil)
       (loop-variables nil)
       (loop-nodeclare nil)
       (loop-named-variables nil)
       (loop-declarations nil)
       (loop-desetq-crocks nil)
       (loop-variable-stack nil)
       (loop-declaration-stack nil)
       (loop-desetq-stack nil)
       (loop-prologue nil)
       (loop-wrappers nil)
       (loop-before-loop nil)
       (loop-body nil)
       (loop-emitted-body? nil)
       (loop-after-body nil)
       (loop-epilogue nil)
       (loop-after-epilogue nil)
       (loop-conditionals nil)
       (loop-when-it-variable nil)
       (loop-never-stepped-variable nil)
       (loop-desetq-temporary nil)
       (loop-prog-names nil)
       (loop-collect-cruft nil)
       (keyword)
       (tem)
       (progvars))
      ((null loop-source-code)
       (and loop-conditionals
	    (loop-simple-error "Hanging conditional in loop macro"
			       (caadar loop-conditionals)))
       (loop-optimize-duplicated-code-etc)
       (loop-bind-block)
       (and loop-desetq-temporary (push loop-desetq-temporary progvars))
       (setq tem `(block ,(car loop-prog-names)
		    (let ,progvars
		      (tagbody
			,@(nreverse loop-prologue)
			,@loop-before-loop
		     next-loop
			,@loop-body
			,@loop-after-body
			(go next-loop)
		     end-loop
			,@(nreverse loop-epilogue)
			,@(nreverse loop-after-epilogue)))))
       (do ((vars) (dcls) (crocks))
	   ((null loop-variable-stack))
	 (setq vars (car loop-variable-stack)
	       loop-variable-stack (cdr loop-variable-stack)
	       dcls (car loop-declaration-stack)
	       loop-declaration-stack (cdr loop-declaration-stack)
	       tem (list tem))
	 (and (setq crocks (pop loop-desetq-stack))
	      (push (loop-make-desetq crocks) tem))
	 (and dcls (push (cons 'declare dcls) tem))
	 (cond ((do ((l vars (cdr l))) ((null l) nil)
		  (and (not (atom (car l)))
		       (or (null (caar l)) (not (symbolp (caar l))))
		       (return t)))
		  (setq tem `(let ,(nreverse vars) ,@tem)))
	       (t (let ((lambda-vars nil) (lambda-vals nil))
		    (do ((l vars (cdr l)) (v)) ((null l))
		      (cond ((atom (setq v (car l)))
			       (push v lambda-vars)
			       (push nil lambda-vals))
			    (t (push (car v) lambda-vars)
			       (push (cadr v) lambda-vals))))

		    ;; Always use (let ((x y) ...) ...) instead of ((lambda (x ...) ...) y ...)
		    ;;   from now on.  The lambda way was fine, but might confuse some of the
		    ;;   newer common lisp compilers.  (MHD 8/13/87)
		    (setq tem `(let ,(do* ((result nil))
					  ((null lambda-vars)
					   (nreverse result))
				       (push (list (pop lambda-vars)
						   (if lambda-vals
						       (pop lambda-vals)))
					     result))
				 ,@tem))))))
       (do ((l loop-wrappers (cdr l))) ((null l))
	 (setq tem (append (car l) (list tem))))
       tem)
    ;;The following commented-out code is what comes from the newest source
    ;; code in use in NIL.  The code in use following it comes from about version
    ;; 803, that in use in symbolics release 6.1, for instance.  To turn on the
    ;; implicit DO feature, switch them and fix loop-get-form to just pop the source.
    (if (symbolp (setq keyword (car loop-source-code)))
    	(loop-pop-source)
      (setq keyword 'do))
    (if (setq tem (loop-tassoc keyword loop-keyword-alist))
    	(lisp:apply (cadr tem) (cddr tem))
    	(if (setq tem (loop-tassoc
    			 keyword loop-iteration-keyword-alist))
    	    (loop-hack-iteration tem)
    	    (if (loop-tmember keyword '(and else))
    		; Alternative is to ignore it, ie let it go around to the
    		; next keyword...
    		(loop-simple-error
    		   "secondary clause misplaced at top level in LOOP macro"
    		   (list keyword (car loop-source-code)
    			 (cadr loop-source-code)))
    		(loop-simple-error
    		   "unknown keyword in LOOP macro" keyword))))
    ;;    (if (symbolp (setq keyword (loop-pop-source)))
    ;;	(if (setq tem (loop-tassoc keyword loop-keyword-alist))
    ;;	    (apply (cadr tem) (cddr tem))
    ;;	    (if (setq tem (loop-tassoc
    ;;			     keyword loop-iteration-keyword-alist))
    ;;		(loop-hack-iteration tem)
    ;;		(if (loop-tmember keyword '(and else))
    ;;		    ; Alternative is to ignore it, ie let it go around to the
    ;;		    ; next keyword...
    ;;		    (loop-simple-error
    ;;		       "secondary clause misplaced at top level in LOOP macro"
    ;;		       (list keyword (car loop-source-code)
    ;;			     (cadr loop-source-code)))
    ;;		    (loop-simple-error
    ;;		       "unknown keyword in LOOP macro" keyword))))
    ;;	(loop-simple-error
    ;;	   "found where keyword expected in LOOP macro" keyword))
))


(defun-for-macro loop-bind-block ()
   (cond ((not (null loop-variables))
	    (push loop-variables loop-variable-stack)
	    (push loop-declarations loop-declaration-stack)
	    (setq loop-variables nil loop-declarations nil)
	    (push loop-desetq-crocks loop-desetq-stack)
	    (setq loop-desetq-crocks nil))))


;Get FORM argument to a keyword.  Read up to atom.  PROGNify if necessary.
(defun-for-macro loop-get-progn-1 ()
  (do ((forms (list (loop-pop-source)) (cons (loop-pop-source) forms))
       (nextform (car loop-source-code) (car loop-source-code)))
      ((atom nextform) (nreverse forms))))

(defun-for-macro loop-get-progn ()
  (let ((forms (loop-get-progn-1)))
    (if (null (cdr forms)) (car forms) (cons 'progn forms))))

(defun-for-macro loop-get-form (for)
  ;; Until implicit DO is installed, use the following.  Then, replace it with
  ;; just loop-pop-source.
  (let ((forms (loop-get-progn-1)))
    (cond ((null (cdr forms)) (car forms))
	  (t (loop-warn 
"The use of multiple forms with an implicit PROGN in this context
is considered obsolete, but is still supported for the time being.
If you did not intend to use multiple forms here, you probably omitted a DO.
If the use of multiple forms was intentional, put a PROGN in your code.
The offending clause"
		(if (atom for) (cons for forms) (append for forms)))
	     (cons 'progn forms)))))


;;;This function takes a substitutable expression containing generic arithmetic
;;; of some form or another, and a data type name, and substitutes for the function
;;; any type-specific functions for that type in the implementation.

;;; Note that the documentation above is incorrect for the actual usage of
;;; loop-typed-arith.  This function is always given a symbol naming a numeric
;;; operation.  Today, I changed this to use the type declared arithmetic
;;; operations.  Note that this function is a no-op until the arithmetic macros
;;; are loaded.  -jra 2/4/91

(defun-for-macro loop-typed-arith (substitutable-expression data-type)
  (declare (ignore data-type))
  substitutable-expression)

;(declare-forward-reference typed-operator-for-type function)

;(defvar-for-macro loop-typed-ops nil)

;(defun-for-macro loop-typed-arith (substitutable-operation data-type)
;  (when (not (member substitutable-operation loop-typed-ops :test #'equal))
;    (push substitutable-operation loop-typed-ops))
;  (cond ((not (eval-feature :tl-typed-arithmetic))
;         substitutable-operation)
;        ((symbolp substitutable-operation)
;         (typed-operator-for-type substitutable-operation data-type))
;        ((and (consp substitutable-operation)
;              (symbolp (car substitutable-operation))
;              (symbolp (cdr substitutable-operation)))
;         (cons (typed-operator-for-type (car substitutable-operation) data-type)
;               (cdr substitutable-operation)))
;        (t
;         (warn "Loop-Typed-Arith saw ~s, but didn't know how to handle it.")
;         substitutable-operation)))

(defvar-for-macro loop-floating-point-types
	'(flonum float short-float single-float double-float long-float))

(defun-for-macro loop-typed-init (data-type)
  (let ((tem nil))
    (cond ((loop-tmember data-type '(fixnum integer number)) 0)
	  ((setq tem (car (loop-tmember
			    data-type loop-floating-point-types)))
	   (cond ((member tem '(flonum float)) 0.0)
		 (t (lisp:coerce 0 tem)))))))


(defun-for-macro loop-make-variable (name initialization dtype)
  (cond ((null name)
	   (cond ((not (null initialization))
		    (push (list (setq name (loop-gentemp 'loop-ignore-))
				initialization)
			  loop-variables)
		      (push `(ignore ,name) loop-declarations))))
	((atom name)
	   (cond (loop-iteration-variablep
		    (if (member name loop-iteration-variables)
			(loop-simple-error
			   "Duplicated iteration variable somewhere in LOOP"
			   name)
			(push name loop-iteration-variables)))
		 ((assoc name loop-variables)
		    (loop-simple-error
		       "Duplicated var in LOOP bind block" name)))
	   (or (symbolp name)
	       (loop-simple-error "Bad variable somewhere in LOOP" name))
	   (loop-declare-variable name dtype)
	   ; We use ASSOC on this list to check for duplications (above),
	   ; so don't optimize out this list:
	   (push (list name (or initialization (loop-typed-init dtype)))
		 loop-variables))
	(initialization
	   (cond (loop-use-system-destructuring?
		    (loop-declare-variable name dtype)
		    (push (list name initialization) loop-variables))
		 (t (let ((newvar (loop-gentemp 'loop-destructure-)))
		      (push (list newvar initialization) loop-variables)
		      ; LOOP-DESETQ-CROCKS gathered in reverse order.
		      (setq loop-desetq-crocks
			    (list* name newvar loop-desetq-crocks))
		      (loop-make-variable name nil dtype)))))
	(t (let ((tcar nil) (tcdr nil))
	     (if (atom dtype) (setq tcar (setq tcdr dtype))
	       (setq tcar (car dtype) tcdr (cdr dtype)))
	     (loop-make-variable (car name) nil tcar)
	     (loop-make-variable (cdr name) nil tcdr))))
  name)


(defun-for-macro loop-make-iteration-variable (name initialization dtype)
    (let ((loop-iteration-variablep t))
       (loop-make-variable name initialization dtype)))


(defun-for-macro loop-declare-variable (name dtype)
    (cond ((or (null name) (null dtype)) nil)
	  ((symbolp name)
	     (cond ((member name loop-nodeclare))
		   (t (push `(type ,(if (loop-tequal dtype 'notype) 't dtype)	;<--mhd (per GSB)
				   ,name) loop-declarations))))
	  ((consp name)
	      (cond ((consp dtype)
		       (loop-declare-variable (car name) (car dtype))
		       (loop-declare-variable (cdr name) (cdr dtype)))
		    (t (loop-declare-variable (car name) dtype)
		       (loop-declare-variable (cdr name) dtype))))
	  (t (loop-simple-error "can't hack this"
				(list 'loop-declare-variable name dtype)))))


(defun-for-macro loop-constantp (form)
  (constantp form))

(defun-for-macro loop-maybe-bind-form (form data-type?)
    ; Consider implementations which will not keep EQ quoted constants
    ; EQ after compilation & loading.
    ; Note FUNCTION is not hacked, multiple occurences might cause the
    ; compiler to break the function off multiple times!
    ; Hacking it probably isn't too important here anyway.  The ones that
    ; matter are the ones that use it as a stepper (or whatever), which
    ; handle it specially.
    (if (loop-constantp form) form
	(loop-make-variable (loop-gentemp 'loop-bind-) form data-type?)))


(defun-for-macro loop-optional-type ()
    (let ((token (car loop-source-code)))
	(and (not (null token))
	     (or (not (atom token))
		 (loop-tmember token '(fixnum integer number notype))
		 (loop-tmember token loop-floating-point-types))
	     (loop-pop-source))))


;Incorporates conditional if necessary
(defun-for-macro loop-make-conditionalization (form)
  (cond ((not (null loop-conditionals))
	   (rplacd (last (car (last (car (last loop-conditionals)))))
		   (list form))
	   (cond ((loop-tequal (car loop-source-code) 'and)
		    (loop-pop-source)
		    nil)
		 ((loop-tequal (car loop-source-code) 'else)
		    (loop-pop-source)
		    ;; If we are already inside an else clause, close it off
		    ;; and nest it inside the containing when clause
		    (let ((innermost (car (last loop-conditionals))))
		      (cond ((null (cddr innermost)))	;Now in a WHEN clause, OK
			    ((null (cdr loop-conditionals))
			     (loop-simple-error "More ELSEs than WHENs"
						(list 'else (car loop-source-code)
						      (cadr loop-source-code))))
			    (t (setq loop-conditionals (cdr (nreverse loop-conditionals)))
			       (rplacd (last (car (last (car loop-conditionals))))
				       (list innermost))
			       (setq loop-conditionals (nreverse loop-conditionals)))))
		    ;; Start a new else clause
		    (rplacd (last (car (last loop-conditionals)))
			    (list (list 't)))
		    nil)
		 (t ;Nest up the conditionals and output them
		     (do ((prev (car loop-conditionals) (car l))
			  (l (cdr loop-conditionals) (cdr l)))
			 ((null l))
		       (rplacd (last (car (last prev))) (list (car l))))
		     (prog1 (car loop-conditionals)
			    (setq loop-conditionals nil)))))
	(t form)))

(defun-for-macro loop-pseudo-body (form &aux (z (loop-make-conditionalization form)))
   (cond ((not (null z))
	    (cond (loop-emitted-body? (push z loop-body))
		  (t (push z loop-before-loop) (push z loop-after-body))))))

(defun-for-macro loop-emit-body (form)
  (setq loop-emitted-body? t)
  (loop-pseudo-body form))


(defun-for-macro loop-do-named ()
  (let ((name (loop-pop-source)))
    (unless (and name (symbolp name))
      (loop-simple-error "Bad name for your loop construct" name))
    ;If this don't come first, LOOP will be confused about how to return
    ; from the prog when it tries to generate such code
    (when (or loop-before-loop loop-body loop-after-epilogue)
      (loop-simple-error "NAMED clause occurs too late" name))
    (when (cdr (setq loop-prog-names (cons name loop-prog-names)))
      (loop-simple-error "Too many names for your loop construct"
			 loop-prog-names))))

(defun-for-macro loop-do-initially ()
  (push (loop-get-progn) loop-prologue))

(defun-for-macro loop-nodeclare (&aux (varlist (loop-pop-source)))
    (or (null varlist)
	(consp varlist)
	(loop-simple-error "Bad varlist to nodeclare loop clause" varlist))
    (setq loop-nodeclare (append varlist loop-nodeclare)))

(defun-for-macro loop-do-finally ()
  (push (loop-get-progn) loop-epilogue))

(defun-for-macro loop-do-do ()
  (loop-emit-body (loop-get-progn)))

(defun-for-macro loop-do-return ()
   (loop-pseudo-body (loop-construct-return (loop-get-form 'return))))




;;; Loop-do-collect ...
;;; ... If the subclause "USING quoted-name-of-cons-function" appended to a collect
;;; clause is provided, the function named by quoted-name-of-cons-function is used
;;; instead of CONS for consing up the collection.  Thus
;;;
;;;     (loop for x in '(a b c) collect x [into ...] using cons)
;;;
;;; has exactly the same semantics as
;;;
;;;     (loop for x in '(a b c) collect x [into ...])

;; The above feature and documentation were added by mhd@gensym, 5/11/87.


(defun-for-macro loop-do-collect (type)
  (let ((var nil) (form nil) (tem nil) (tail nil) (dtype nil) (cruft nil) (rvar nil)
	(ctype (case type
		 ((max min) 'maxmin)
		 ((nconc list append) 'list)
		 ((count sum) 'sum)
;<--mhd		 ((member type '(max min)) 'maxmin)
		 (t (error "LOOP internal error:  ~S is an unknown collecting keyword."
			   type))))
	(cons-function? nil))
    (setq form (loop-get-form type) dtype (loop-optional-type))
    (cond ((loop-tequal (car loop-source-code) 'into)
	   (loop-pop-source)
	   (setq rvar (setq var (loop-pop-source)))))
    (cond ((loop-tequal (car loop-source-code) 'using)
	   (loop-pop-source)
	   (setq cons-function? (loop-pop-source))))
    ; CRUFT will be (varname ctype dtype var tail (optional tem))
    (cond ((setq cruft (assoc var loop-collect-cruft))
	     (cond ((not (eq ctype (car (setq cruft (cdr cruft)))))
		      (loop-simple-error
		         "incompatible LOOP collection types"
			 (list ctype (car cruft))))
		   ((and dtype (not (eq dtype (cadr cruft))))
		      ;Conditional should be on data-type reality
		    (error "~A and ~A Unequal data types into ~A"
			   dtype (cadr cruft) (car cruft))))
	     (setq dtype (car (setq cruft (cdr cruft)))
		   var (car (setq cruft (cdr cruft)))
		   tail (car (setq cruft (cdr cruft)))
		   tem (cadr cruft))
	     (and (eq ctype 'maxmin)
		  (not (atom form)) (null tem)
		  (rplaca (cdr cruft)
			  (setq tem (loop-make-variable
				       (loop-gentemp 'loop-maxmin-)
				       nil dtype)))))
	  (t (unless dtype
	       (setq dtype (case type
			     (count 'fixnum)
			     ((min max sum) 'number))))
	     (unless var
	       (push (loop-construct-return (setq var (loop-gentemp)))
		     loop-after-epilogue))
	     (loop-make-iteration-variable var nil dtype)
	     (cond ((eq ctype 'maxmin)
		      ;Make a temporary.
		      (unless (atom form)
			(setq tem (loop-make-variable
				    (loop-gentemp) nil dtype)))
		      ;Use the tail slot of the collect database to hold a
		      ; flag which says we have been around once already.
		      (setq tail (loop-make-variable
				   (loop-gentemp 'loop-maxmin-fl-) t nil)))
		   ((eq ctype 'list)
		    ;For dumb collection, we need both a tail and a flag var
		    ; to tell us whether we have iterated.
		    (setq tail (loop-make-variable (loop-gentemp) nil nil)
			  tem (loop-make-variable (loop-gentemp) nil nil))))
	     (push (list rvar ctype dtype var tail tem)
		   loop-collect-cruft)))
    (loop-emit-body
	(case type
	  (count (setq tem `(setq ,var (,(loop-typed-arith '1+ dtype)
					,var)))
		 (if (or (eq form t) (equal form ''t))
		     tem
		     `(when ,form ,tem)))
	  (sum
	   ;; Allow "... summing x using +f ..."
	   `(setq ,var (,(or cons-function? (loop-typed-arith '+ dtype))
			 ,form ,var)))
	  ((max min)
	   ;; Allow "... maximizing x using maxf ..."
	     (let ((forms nil) (arglist nil))
		; TEM is temporary, properly typed.
		(and tem (setq forms `((setq ,tem ,form)) form tem))
		(setq arglist (list var form))
		(push (if (and (loop-tmember dtype '(fixnum flonum))
			       (null cons-function?))
			  ; no contagious arithmetic
			  `(when (or ,tail
				     (,(loop-typed-arith
				         (if (eq type 'max) '< '>)
					 dtype)
				      ,@arglist))
			     (setq ,tail nil ,@arglist))
			  ; potentially contagious arithmetic -- must use
			  ; MAX or MIN so that var will be contaminated
			  (let ((func (or cons-function?
					  (loop-typed-arith type dtype))))
			    `(setq ,var (cond (,tail (setq ,tail nil) ,form)
					      (t (,func ,@arglist))))))
		      forms)
		(if (cdr forms) (cons 'progn (nreverse forms)) (car forms))))
	  (t
;; before "using":
;	   (case type
;	      (list (setq form (list 'list form)))
;	      (append (or (and (not (atom form)) (eq (car form) 'list))
;			    (setq form `(copy-list ,form)))))
;	   (let ((q `(if ,tail (cdr (rplacd ,tail ,tem))
;			 (setq ,var ,tem))))
;	     (if (and (not (atom form)) (eq (car form) 'list) (cdr form))
;		 `(setq ,tem ,form ,tail ,(loop-cdrify (cddr form) q))
;		 `(when (setq ,tem ,form) (setq ,tail (last ,q)))))
;; after "using":
	   (if cons-function?
	       (setq form (list cons-function? form nil))
	       (case type
		 (list (setq form (list 'list form)))
		 (append (or (and (not (atom form)) (eq (car form) 'list))
			     (setq form `(copy-list ,form))))))
	   (let ((q `(progn
		       (if ,tail
			   (rplacd ,tail ,tem)
			   (setq ,var ,tem))
		       ,tem)))
	     (if cons-function?
		 `(setq ,tem ,form ,tail ,q)
		 (if (and (not (atom form)) (eq (car form) 'list) (cdr form))
		     `(setq ,tem ,form ,tail ,(loop-cdrify (cddr form) q))
		     `(when (setq ,tem ,form) (setq ,tail (last ,q)))))))))))

;; The above generates a useless LAST and a useless WHEN test given loop COLLECT
;; clauses such as (loop for x in '(a b c) collect x).
;; FIX!!! (MHD/PTO@Gensym 1/24/90)    -- FIXED BY MHD (caused by a dumb mistake
;; when I transformed it a long time ago to handle "using"! (MHD 2/12/90)

;; The "fixed" version contained an unneeded call to CDR in the case where tail
;; already existed.  Fixed.  -jra 6/4/96



(defun-for-macro loop-cdrify (arglist form)
    (do ((size (length arglist) (- size 4)))
	((< size 4)
	 (if (zerop size) form
	     (list (cond ((= size 1) 'cdr) ((= size 2) 'cddr) (t 'cdddr))
		   form)))
      (declare (type fixnum size))
      (setq form (list 'cddddr form))))



(defun-for-macro loop-do-while (negate? kwd &aux (form (loop-get-form kwd)))
  (and loop-conditionals (loop-simple-error
			   "not allowed inside LOOP conditional"
			   (list kwd form)))
  (loop-pseudo-body `(,(if negate? 'when 'unless)
		      ,form (go end-loop))))


(defun-for-macro loop-do-when (negate? kwd)
  (let ((form (loop-get-form kwd)) (cond nil))
    (cond ((loop-tequal (cadr loop-source-code) 'it)
	     ;WHEN foo RETURN IT and the like
	     (setq cond `(setq ,(loop-when-it-variable) ,form))
	     (setq loop-source-code		;Plug in variable for IT
		   (list* (car loop-source-code)
			  loop-when-it-variable
			  (cddr loop-source-code))))
	  (t (setq cond form)))
    (and negate? (setq cond `(not ,cond)))
    (setq loop-conditionals (nconc loop-conditionals `((cond (,cond)))))))

(defun-for-macro loop-do-with ()
  (do ((var) (equals) (val) (dtype)) (nil)
    (setq var (loop-pop-source) equals (car loop-source-code))
    (cond ((loop-tequal equals '=)
	     (loop-pop-source)
	     (setq val (loop-get-form (list 'with var '=)) dtype nil))
	  ((or (loop-tequal equals 'and)
	       (loop-tassoc equals loop-keyword-alist)
	       (loop-tassoc equals loop-iteration-keyword-alist))
	     (setq val nil dtype nil))
	  (t (setq dtype (loop-optional-type) equals (car loop-source-code))
	     (cond ((loop-tequal equals '=)
		      (loop-pop-source)
		      (setq val (loop-get-form (list 'with var dtype '=))))
		   ((and (not (null loop-source-code))
			 (not (loop-tassoc equals loop-keyword-alist))
			 (not (loop-tassoc
				 equals loop-iteration-keyword-alist))
			 (not (loop-tequal equals 'and)))
		      (loop-simple-error "Garbage where = expected" equals))
		   (t (setq val nil)))))
    (loop-make-variable var val dtype)
    (if (not (loop-tequal (car loop-source-code) 'and)) (return nil)
	(loop-pop-source)))
  (loop-bind-block))

(defun-for-macro loop-do-always (negate?)
  (let ((form (loop-get-form 'always)))
    (loop-emit-body `(,(if negate? 'when 'unless) ,form
		      ,(loop-construct-return nil)))
    (push (loop-construct-return t) loop-after-epilogue)))

;THEREIS expression
;If expression evaluates non-nil, return that value.
(defun-for-macro loop-do-thereis ()
   (loop-emit-body `(when (setq ,(loop-when-it-variable)
				,(loop-get-form 'thereis))
		      ,(loop-construct-return loop-when-it-variable))))


;;;; Hacks

(defun-for-macro loop-simplep (expr)
    (if (null expr) 0
      (catch 'loop-simplep
	(let ((ans (loop-simplep-1 expr)))
	  (declare (type fixnum ans))
	  (and (< ans 20.) ans)))))

(defvar-for-macro loop-simplep
	'(> < <= >= /= + - 1+ 1- ash equal atom setq prog1 prog2 and or = aref char schar sbit svref))

(defun-for-macro loop-simplep-1 (x)
  (let ((z 0))
    (declare (type fixnum z))
    (cond ((loop-constantp x) 0)
	  ((atom x) 1)
	  ((eq (car x) 'cond)
	     (do ((cl (cdr x) (cdr cl))) ((null cl))
	       (do ((f (car cl) (cdr f))) ((null f))
		 (setq z (+ (loop-simplep-1 (car f)) z 1))))
	     z)
	  ((symbolp (car x))
	     (let ((fn (car x)) (tem nil))
	       (cond ((setq tem (get fn 'loop-simplep))
		        (if (typep tem 'fixnum) (setq z tem)
			    (setq z (lisp:funcall tem x) x nil)))
		     ((member fn '(null not eq go return progn)))
		     ((member fn '(car cdr)) (setq z 1))
		     ((member fn '(caar cadr cdar cddr)) (setq z 2))
		     ((member fn '(caaar caadr cadar caddr
				   cdaar cdadr cddar cdddr))
		        (setq z 3))
		     ((member fn '(caaaar caaadr caadar caaddr
				   cadaar cadadr caddar cadddr
				   cdaaar cdaadr cdadar cdaddr
				   cddaar cddadr cdddar cddddr))
		        (setq z 4))
		     ((member fn loop-simplep) (setq z 2))
		     (t (multiple-value-bind (new-form expanded-p)
			    (macroexpand-1 x loop-macro-environment)
			  (if expanded-p
			      (setq z (loop-simplep-1 new-form) x nil)
			      (throw 'loop-simplep nil)))))
	       (do ((l (cdr x) (cdr l))) ((null l))
		 (setq z (+ (loop-simplep-1 (car l)) 1 z)))
	       z))
	  (t (throw 'loop-simplep nil)))))


;;;; The iteration driver
(defun-for-macro loop-hack-iteration (entry)
  (do ((last-entry entry)
       (source loop-source-code loop-source-code)
       (pre-step-tests nil)
       (steps nil)
       (post-step-tests nil)
       (pseudo-steps nil)
       (pre-loop-pre-step-tests nil)
       (pre-loop-steps nil)
       (pre-loop-post-step-tests nil)
       (pre-loop-pseudo-steps nil)
       (tem) (data) (foo) (bar))
      (nil)
    ; Note we collect endtests in reverse order, but steps in correct
    ; order.  LOOP-END-TESTIFY does the nreverse for us.
    (setq tem (setq data (lisp:apply (cadr entry) (cddr entry))))
    (and (car tem) (push (car tem) pre-step-tests))
    (setq steps (nconc steps (loop-copylist* (car (setq tem (cdr tem))))))
    (and (car (setq tem (cdr tem))) (push (car tem) post-step-tests))
    (setq pseudo-steps
	  (nconc pseudo-steps (loop-copylist* (car (setq tem (cdr tem))))))
    (setq tem (cdr tem))
    (and (or loop-conditionals loop-emitted-body?)
	 (or tem pre-step-tests post-step-tests pseudo-steps)
	 (let ((cruft (list (car entry) (car source)
			    (cadr source) (caddr source))))
	    (if loop-emitted-body?
		(loop-simple-error
		   "Iteration is not allowed to follow body code" cruft)
		(loop-simple-error
		   "Iteration starting inside of conditional in LOOP"
		   cruft))))
    (or tem (setq tem data))
    (and (car tem) (push (car tem) pre-loop-pre-step-tests))
    (setq pre-loop-steps
	  (nconc pre-loop-steps (loop-copylist* (car (setq tem (cdr tem))))))
    (and (car (setq tem (cdr tem))) (push (car tem) pre-loop-post-step-tests))
    (setq pre-loop-pseudo-steps
	  (nconc pre-loop-pseudo-steps (loop-copylist* (cadr tem))))
    (cond ((or (not (loop-tequal (car loop-source-code) 'and))
	       (and loop-conditionals
		    (not (loop-tassoc (cadr loop-source-code)
					 loop-iteration-keyword-alist))))
	     (setq foo (list (loop-end-testify pre-loop-pre-step-tests)
			     (loop-make-psetq pre-loop-steps)
			     (loop-end-testify pre-loop-post-step-tests)
			     (loop-make-setq pre-loop-pseudo-steps))
		   bar (list (loop-end-testify pre-step-tests)
			     (loop-make-psetq steps)
			     (loop-end-testify post-step-tests)
			     (loop-make-setq pseudo-steps)))
	     (cond ((not loop-conditionals)
		      (setq loop-before-loop (nreconc foo loop-before-loop)
			    loop-after-body (nreconc bar loop-after-body)))
		   (t ((lambda (loop-conditionals)
			  (push (loop-make-conditionalization
				   (cons 'progn (delete nil foo)))
				loop-before-loop))
		       (lisp:mapcar #'(lambda (x)	;Copy parts that will get rplacd'ed
					(cons (car x)
					      (lisp:mapcar #'(lambda (x) (loop-copylist* x)) (cdr x))))
				    loop-conditionals))
		      (push (loop-make-conditionalization
			       (cons 'progn (delete nil bar)))
			    loop-after-body)))
	     (loop-bind-block)
	     (return nil)))
    (loop-pop-source) ; flush the "AND"
    (setq entry (cond ((setq tem (loop-tassoc
				    (car loop-source-code)
				    loop-iteration-keyword-alist))
		         (loop-pop-source)
			 (setq last-entry tem))
		      (t last-entry)))))


;FOR variable keyword ..args..
(defun-for-macro loop-do-for ()
  (let ((var (loop-pop-source))
	(data-type? (loop-optional-type))
	(keyword (loop-pop-source))
	(first-arg nil)
	(tem nil))
    (setq first-arg (loop-get-form (list 'for var keyword)))
    (or (setq tem (loop-tassoc keyword loop-for-keyword-alist))
	(loop-simple-error
	   "Unknown keyword in FOR or AS clause in LOOP"
	   (list 'for var keyword)))
    (lisp:apply (cadr tem) var first-arg data-type? (cddr tem))))


(defun-for-macro loop-do-repeat ()
    (let ((var (loop-make-variable
		  (loop-gentemp 'loop-repeat-)
		  (loop-get-form 'repeat) 'fixnum)))
       `((not (,(loop-typed-arith 'plusp 'fixnum) ,var))
         () ()
         (,var (,(loop-typed-arith '1- 'fixnum) ,var)))))


; Kludge the First
(defun-for-macro loop-when-it-variable ()
    (or loop-when-it-variable
	(setq loop-when-it-variable
	      (loop-make-variable (loop-gentemp 'loop-it-) nil nil))))



(defun-for-macro loop-for-equals (var val data-type?)
  (cond ((loop-tequal (car loop-source-code) 'then)
	   ;FOR var = first THEN next
	   (loop-pop-source)
	   (loop-make-iteration-variable var val data-type?)
	   `(() (,var ,(loop-get-form (list 'for var '= val 'then))) () ()
	     () () () ()))
	(t (loop-make-iteration-variable var nil data-type?)
	   (let ((varval (list var val)))
	     (cond (loop-emitted-body?
		    (loop-emit-body (loop-make-setq varval))
		    '(() () () ()))
		   (`(() ,varval () ())))))))

(defun-for-macro loop-for-first (var val data-type?)
    (or (loop-tequal (car loop-source-code) 'then)
	(loop-simple-error "found where THEN expected in FOR ... FIRST"
			   (car loop-source-code)))
    (loop-pop-source)
    (loop-make-iteration-variable var nil data-type?)
    `(() (,var ,(loop-get-form (list 'for var 'first val 'then))) () ()
      () (,var ,val) () ()))




;;; Replaced CAR and CDR in the following function with CAR-OF-CONS and
;;; CDR-OF-CONS to gain an optimization during for x on y and for x in y
;;; iterations.  However, I take no responsibility for the formatting of the
;;; code below.  -jra 5/1/91

(defun-for-macro loop-list-stepper (var val data-type? fn)
    (let ((stepper (cond ((loop-tequal (car loop-source-code) 'by)
			    (loop-pop-source)
			    (loop-get-form (list 'for var
						 (if (eq fn 'car-of-cons) 'in 'on)
						 val 'by)))
			 (t '(function cdr-of-cons))))
	  (var1 nil) (stepvar nil) (step nil) (et nil) (pseudo nil))
       (setq step (if (or (atom stepper)
			  (not (member (car stepper) '(quote function))))
		      `(lisp:funcall ,(setq stepvar (loop-gentemp 'loop-fn-)))
		      (list (cadr stepper))))
       (cond ((and (atom var)
		   ;; (eq (car step) 'cdr)
		   (not fn))
	        (setq var1 (loop-make-iteration-variable var val data-type?)))
	     (t (loop-make-iteration-variable var nil data-type?)
		(setq var1 (loop-make-variable
			     (loop-gentemp 'loop-list-) val nil))
		(setq pseudo (list var (if fn (list fn var1) var1)))))
       (rplacd (last step) (list var1))
       (and stepvar (loop-make-variable stepvar stepper nil))
       (setq stepper (list var1 step) et `(null ,var1))
       (if (not pseudo) `(() ,stepper ,et () () () ,et ())
	   (if (eq (car step) 'cdr-of-cons)
	       `(,et ,pseudo () ,stepper)
	       `((null (setq ,@stepper)) () () ,pseudo ,et () () ,pseudo)))))


(defun-for-macro loop-for-arithmetic (var val data-type? kwd)
  ; Args to loop-sequencer:
  ; indexv indexv-type variable? vtype? sequencev? sequence-type
  ; stephack? default-top? crap prep-phrases
  (loop-sequencer
     var (or data-type? 'fixnum) nil nil nil nil nil nil `(for ,var ,kwd ,val)
     (cons (list kwd val)
	   (loop-gather-preps
	      '(from upfrom downfrom to upto downto above below by)
	      nil))))


(defun-for-macro loop-named-variable (name)
    (let ((tem (loop-tassoc name loop-named-variables)))
       (cond ((null tem) (loop-gentemp))
	     (t (setq loop-named-variables (delete tem loop-named-variables))
		(cdr tem)))))


; Note:  path functions are allowed to use loop-make-variable, hack
; the prologue, etc.
(defun-for-macro loop-for-being (var val data-type?)
   ; FOR var BEING something ... - var = VAR, something = VAL.
   ; If what passes syntactically for a pathname isn't, then
   ; we trap to the DEFAULT-LOOP-PATH path;  the expression which looked like
   ; a path is given as an argument to the IN preposition.  Thus,
   ; by default, FOR var BEING EACH expr OF expr-2
   ; ==> FOR var BEING DEFAULT-LOOP-PATH IN expr OF expr-2.
   (let ((tem nil) (inclusive? nil) (ipps nil) (each? nil) (attachment nil))
     (if (or (loop-tequal val 'each) (loop-tequal val 'the))
	 (setq each? 't val (car loop-source-code))
	 (push val loop-source-code))
     (cond ((and (setq tem (loop-tassoc val loop-path-keyword-alist))
		 (or each? (not (loop-tequal (cadr loop-source-code)
						'and))))
	      ;; FOR var BEING {each} path {prep expr}..., but NOT
	      ;; FOR var BEING var-which-looks-like-path AND {ITS} ...
	      (loop-pop-source))
	   (t (setq val (loop-get-form (list 'for var 'being)))
	      (cond ((loop-tequal (car loop-source-code) 'and)
		       ;; FOR var BEING value AND ITS path-or-ar
		       (or (null each?)
			   (loop-simple-error
			      "Malformed BEING EACH clause in LOOP" var))
		       (setq ipps `((of ,val)) inclusive? t)
		       (loop-pop-source)
		       (or (loop-tmember (setq tem (loop-pop-source))
					    '(its his her their each))
			   (loop-simple-error
			      "found where ITS or EACH expected in LOOP path"
			      tem))
		       (if (setq tem (loop-tassoc
					(car loop-source-code)
					loop-path-keyword-alist))
			   (loop-pop-source)
			   (push (setq attachment
				       `(in ,(loop-get-form
					      `(for ,var being \.\.\. in))))
				 ipps)))
		    ((not (setq tem (loop-tassoc
				       (car loop-source-code)
				       loop-path-keyword-alist)))
		       ; FOR var BEING {each} a-r ...
		       (setq ipps (list (setq attachment (list 'in val)))))
		    (t ; FOR var BEING {each} pathname ...
		       ; Here, VAL should be just PATHNAME.
		       (loop-pop-source)))))
     (cond ((not (null tem)))
	   ((not (setq tem (loop-tassoc 'default-loop-path
					   loop-path-keyword-alist)))
	      (loop-simple-error "Undefined LOOP iteration path"
				 (cadr attachment))))
     (setq tem (lisp:funcall (cadr tem) (car tem) var data-type?
			     (nreconc ipps (loop-gather-preps (caddr tem) t))
			     inclusive? (caddr tem) (cdddr tem)))
     (and loop-named-variables
	  (loop-simple-error "unused USING variables" loop-named-variables))
     ; For error continuability (if there is any):
     (setq loop-named-variables nil)
     ;; TEM is now (bindings prologue-forms . stuff-to-pass-back)
     (do ((l (car tem) (cdr l)) (x)) ((null l))
       (if (atom (setq x (car l)))
	   (loop-make-iteration-variable x nil nil)
	   (loop-make-iteration-variable (car x) (cadr x) (caddr x))))
     (setq loop-prologue (nconc (reverse (cadr tem)) loop-prologue))
     (cddr tem)))


(defun-for-macro loop-gather-preps (preps-allowed crockp)
   (do ((token (car loop-source-code) (car loop-source-code)) (preps nil))
       (nil)
     (cond ((loop-tmember token preps-allowed)
	      (push (list (loop-pop-source)
			  (loop-get-form `(for \... being \... ,token)))
		    preps))
	   ((loop-tequal token 'using)
	      (loop-pop-source)
	      (or crockp (loop-simple-error
			    "USING used in illegal context"
			    (list 'using (car loop-source-code))))
	      (do ((z (car loop-source-code) (car loop-source-code)) (tem))
		  ((atom z))
		(and (or (atom (cdr z))
			 (not (null (cddr z)))
			 (not (symbolp (car z)))
			 (and (cadr z) (not (symbolp (cadr z)))))
		     (loop-simple-error
		        "bad variable pair in path USING phrase" z))
		(cond ((not (null (cadr z)))
		         (and (setq tem (loop-tassoc
					   (car z) loop-named-variables))
			      (loop-simple-error
			         "Duplicated var substitution in USING phrase"
				 (list tem z)))
			 (push (cons (car z) (cadr z)) loop-named-variables)))
		(loop-pop-source)))
	   (t (return (nreverse preps))))))

(defun-for-macro loop-add-path (name data)
    (setq loop-path-keyword-alist
	  (cons (cons name data)
		(delete (loop-tassoc name loop-path-keyword-alist)
			loop-path-keyword-alist
			:test #'eq)))
    nil)


(defmacro define-loop-path (names &rest cruft)
  "(DEFINE-LOOP-PATH NAMES PATH-FUNCTION LIST-OF-ALLOWABLE-PREPOSITIONS
DATUM-1 DATUM-2 ...)
Defines PATH-FUNCTION to be the handler for the path(s) NAMES, which may
be either a symbol or a list of symbols.  LIST-OF-ALLOWABLE-PREPOSITIONS
contains a list of prepositions allowed in NAMES. DATUM-i are optional;
they are passed on to PATH-FUNCTION as a list."
  (if (not (member :no-macros *features*))
      (progn
	(setq names (if (atom names) (list names) names))
	(let ((forms (lisp:mapcar
		       #'(lambda (name) `(loop-add-path ',name ',cruft))
		       names)))
	  `(eval-when (eval load compile) ,@forms)))))


(defun-for-macro loop-sequencer (indexv indexv-type
			  variable? vtype?
			  sequencev? sequence-type?
			  stephack? default-top?
			  crap prep-phrases)
   (let ((endform nil) (sequencep nil) (test nil)
	 (step ; Gross me out!
	       (1+ (or (loop-typed-init indexv-type) 0)))
	 (dir nil) (inclusive-iteration? nil) (start-given? nil) (limit-given? nil))
     (and variable? (loop-make-iteration-variable variable? nil vtype?))
     (do ((l prep-phrases (cdr l)) (prep) (form) (odir)) ((null l))
       (setq prep (caar l) form (cadar l))
       (cond ((loop-tmember prep '(of in))
		(and sequencep (loop-simple-error
				  "Sequence duplicated in LOOP path"
				  (list variable? (car l))))
		(setq sequencep t)
		(loop-make-variable sequencev? form sequence-type?))
	     ((loop-tmember prep '(from downfrom upfrom))
	        (and start-given?
		     (loop-simple-error
		        "Iteration start redundantly specified in LOOP sequencing"
			(append crap l)))
		(setq start-given? t)
		(cond ((loop-tequal prep 'downfrom) (setq dir 'down))
		      ((loop-tequal prep 'upfrom) (setq dir 'up)))
		(loop-make-iteration-variable indexv form indexv-type))
	     ((cond ((loop-tequal prep 'upto)
		       (setq inclusive-iteration? (setq dir 'up)))
		    ((loop-tequal prep 'to)
		       (setq inclusive-iteration? t))
		    ((loop-tequal prep 'downto)
		       (setq inclusive-iteration? (setq dir 'down)))
		    ((loop-tequal prep 'above) (setq dir 'down))
		    ((loop-tequal prep 'below) (setq dir 'up)))
		(and limit-given?
		     (loop-simple-error
		       "Endtest redundantly specified in LOOP sequencing path"
		       (append crap l)))
		(setq limit-given? t)
		(setq endform (loop-maybe-bind-form form indexv-type)))
	     ((loop-tequal prep 'by)
		(setq step (if (loop-constantp form) form
			       (loop-make-variable
				 (loop-gentemp 'loop-step-by-)
				 form 'fixnum))))
	     (t ; This is a fatal internal error...
	        (loop-simple-error "Illegal prep in sequence path"
				   (append crap l))))
       (and odir dir (not (eq dir odir))
	    (loop-simple-error
	       "Conflicting stepping directions in LOOP sequencing path"
	       (append crap l)))
       (setq odir dir))
     (and sequencev? (not sequencep)
	  (loop-simple-error "Missing OF phrase in sequence path" crap))
     ; Now fill in the defaults.
     (setq step (list indexv step))
     (cond ((member dir '(nil up))
	      (or start-given?
		  (loop-make-iteration-variable indexv 0 indexv-type))
	      (and (or limit-given?
		       (cond (default-top?
			        (loop-make-variable
				  (setq endform (loop-gentemp
						  'loop-seq-limit-))
				  nil indexv-type)
				(push `(setq ,endform ,default-top?)
				      loop-prologue))))
		   (setq test (if inclusive-iteration? '(> . args)
				  '(>= . args))))
	      (push '+ step))
	   (t (cond ((not start-given?)
		       (or default-top?
			   (loop-simple-error
			      "Don't know where to start stepping"
			      (append crap prep-phrases)))
		       (loop-make-iteration-variable indexv 0 indexv-type)
		       (push `(setq ,indexv
				    (,(loop-typed-arith '1- indexv-type)
				     ,default-top?))
			     loop-prologue)))
	      (cond ((and default-top? (not endform))
		       (setq endform (loop-typed-init indexv-type)
			     inclusive-iteration? t)))
	      (and (not (null endform))
		   (setq test (if inclusive-iteration? '(< . args)
				  '(<= . args))))
	      (push '- step)))
     (and (and (numberp (caddr step)) (= (caddr step) 1))	;Generic arith
	  (rplacd (cdr (rplaca step (if (eq (car step) '+) '1+ '1-)))
		  nil))
     (rplaca step (loop-typed-arith (car step) indexv-type))
     (setq step (list indexv step))
     (setq test (loop-typed-arith test indexv-type))
     (setq test (subst (list indexv endform) 'args test))
     (and stephack? (setq stephack? `(,variable? ,stephack?)))
     `(() ,step ,test ,stephack?
       () () ,test ,stephack?)))


(defun-for-macro loop-sequence-elements-path
		 (path variable data-type
		       prep-phrases inclusive?
		       allowed-preps data)
    allowed-preps ; unused
    (let ((indexv (loop-named-variable 'index))
	  (sequencev (loop-named-variable 'sequence))
	  (fetchfun nil) (sizefun nil) (type nil) (default-var-type nil)
	  (crap `(for ,variable being the ,path)))
       (cond ((not (null inclusive?))
	        (rplacd (cddr crap) `(,(cadar prep-phrases) and its ,path))
		(loop-simple-error "Can't step sequence inclusively" crap)))
       (setq fetchfun (car data)
	     sizefun (car (setq data (cdr data)))
	     type (car (setq data (cdr data)))
	     default-var-type (cadr data))
       (list* nil nil ; dummy bindings and prologue
	      (loop-sequencer
	         indexv 'fixnum
		 variable (or data-type default-var-type)
		 sequencev type
		 `(,fetchfun ,sequencev ,indexv) `(,sizefun ,sequencev)
		 crap prep-phrases))))



(defmacro define-loop-sequence-path (path-name-or-names fetchfun sizefun
				     &optional sequence-type element-type)
  "Defines a sequence iiteration path.  PATH-NAME-OR-NAMES is either an
atomic path name or a list of path names.  FETCHFUN is a function of
two arguments, the sequence and the index of the item to be fetched.
Indexing is assumed to be zero-origined.  SIZEFUN is a function of
one argument, the sequence; it should return the number of elements in
the sequence.  SEQUENCE-TYPE is the name of the data-type of the
sequence, and ELEMENT-TYPE is the name of the data-type of the elements
of the sequence."
    `(define-loop-path ,path-name-or-names
	loop-sequence-elements-path
	(of in from downfrom to downto below above by)
	,fetchfun ,sizefun ,sequence-type ,element-type))



;;;; Setup stuff

;(if (not (member :no-macros *features*))	; from BOOTSTRAP -- abstract this!?
;    (progn
;      (mapc #'(lambda (x)
;		(mapc #'(lambda (y)
;			  (setq loop-path-keyword-alist
;				(cons `(,y loop-sequence-elements-path
;					(of in from downfrom to downto
;					    below above by)
;					,@(cdr x))
;				      (delete (loop-tassoc
;						y loop-path-keyword-alist)
;					      loop-path-keyword-alist
;					      :test #'eq :count 1))))
;		      (car x)))
;	    '( ((element elements) elt length sequence)
;						;The following should be done by using ELEMENTS and type dcls...
;	      ((vector-element 
;		 vector-elements 
;		 array-element    ;; Backwards compatibility -- DRM
;		 array-elements)
;	       aref length vector)
;	      ;; Fixed to use LENGTH instead of old dialect names
;	      ;;   simple-vector-length, simple-string-length,
;	      ;;   simple-bit-vector-length, and string-length.
;	      ;;   The type decls should make it be just as good.
;	      ;;     -- mhd, 6/2/87
;	      ((simple-vector-element simple-vector-elements
;				      simple-general-vector-element simple-general-vector-elements)
;	       svref elt simple-vector)
;	      ((bits bit bit-vector-element bit-vector-elements)
;	       bit length bit-vector bit)
;	      ((simple-bit-vector-element simple-bit-vector-elements)
;	       sbit length simple-bit-vector bit)
;	      ((character characters string-element string-elements)
;	       char length string string-char)
;	      ((simple-string-element simple-string-elements)
;	       schar length simple-string string-char)))))
