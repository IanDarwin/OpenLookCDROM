; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         common.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/common.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Commonly used Common-Lisp functionality not built-in to XLISP
; Author:       David Betz, Tom Almy, and others
; Created:      
; Modified:     Mon Jun  6 03:02:47 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       X11r6 contrib release
; 
; Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
; WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
; WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
; 
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies,
; Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Enterprise Integration
; Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
; Betz make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
; 
; ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
; LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
; COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions missing that are part of common lisp, and commonly used

;; It is assumed you are using XLISP with all Common Lisp related options
;; turned on before you load this file.

;; Author either unknown or Tom Almy unless indicated.

;; pairlis does not check for lengths of keys and values being unequal

(defun pairlis (keys values &optional list)
       (nconc (mapcar #'cons keys values) list))

(defun copy-list (list) (append list 'nil))

(defun copy-alist (list)
    (if (null list)
        'nil
        (cons (if (consp (car list))
		  (cons (caar list) (cdar list))
		  (car list))
	      (my-copy-alist (cdr list)))))

(defun copy-tree (list)
    (if (consp list)
        (cons (copy-tree (car list)) (copy-tree (cdr list)))
        list))

(unless (fboundp 'list*)
(defun list* (&rest list)	;; There must be a better way...
    (cond ((null list) 'nil)
	  ((null (cdr list)) (car list))
	  (t (do* ((head (cons (car list) 'nil))
		   (current head
			    (cdr (rplacd current (cons (car tail) 'nil))))
		   (tail (cdr list) (cdr tail)))
		  ((null (cdr tail)) (rplacd current (car tail)) head)
	      ))))
)

(defun identity (l) l)

(defun signum (x)
   (cond ((not (numberp x)) (error "not a number" x))
         ((zerop x) x)
	 (T (/ x (abs x)))))  

; Cruddy but simple versions of these functions.
; Commented out since XLISP will now expand macros once, making
; good version much preferred.

;(defmacro incf (var &optional (delta 1))
;    `(setf ,var (+ ,var ,delta)))

;(defmacro decf (var &optional (delta 1))
;    `(setf ,var (- ,var ,delta)))

;(defmacro push (v l)
;	`(setf ,l (cons ,v ,l))))

;(defmacro pushnew (a l &rest args)
;  `(unless (member ,a ,l ,@args) (push ,a ,l) nil))

;(defmacro pop (l)
;	`(prog1 (first ,l) (setf ,l (rest ,l)))))


; This is what one really needs to do for incf decf and
; (in common.lsp) push and pop. The setf form must only be evaluated once.
; But is it worth all this overhead for correctness?
; (By Tom Almy)

(defun |DoForm| (form) ; returns (cons |list for let| |new form|)
       (let* ((args (rest form)) ; raw form arguments
	      (letlist (mapcan #'(lambda (x) (when (consp x)
						   (list (list (gensym) x))))
			       form))
	      (revlist (mapcar #'(lambda (x) (cons (second x) (first x)))
			       letlist))
	      (newform (cons (first form) (sublis revlist args))))
	     (cons letlist newform)))

(defmacro incf (form &optional (delta 1))
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval) 
			 (setf ,(cdr retval)
			       (+ ,(cdr retval) ,delta))))
	      `(setf ,form (+ ,form ,delta))))

(defmacro decf (form &optional (delta 1))
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval) 
			 (setf ,(cdr retval)
			       (- ,(cdr retval) ,delta))))
	      `(setf ,form (- ,form ,delta))))

(defmacro push (val form)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval) 
			 (setf ,(cdr retval)
			       (cons ,val ,(cdr retval)))))
	      `(setf ,form (cons ,val ,form))))

(defmacro pop (form)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval) 
			 (prog1 (first ,(cdr retval))
				(setf ,(cdr retval)
				      (rest ,(cdr retval))))))
	      `(prog1 (first ,form)
		      (setf ,form (rest ,form)))))


(defmacro pushnew (val form &rest rest)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval) 
			 (setf ,(cdr retval)
			       (adjoin ,val ,(cdr retval) ,@rest))))
	      `(setf ,form (adjoin ,val ,form ,@rest))))


;; Hyperbolic functions    Ken Whedbee  from CLtL

(defun logtest (x y) (not (zerop (logand x y))))

(defconstant imag-one #C(0.0 1.0))

(defun cis (x) (exp (* imag-one x)))


(defun sinh (x) (/ (- (exp x) (exp (- x))) 2.0))
(defun cosh (x) (/ (+ (exp x) (exp (- x))) 2.0))
(defun tanh (x) (/ (sinh x) (cosh x)))

(defun asinh (x) (log (+ x (sqrt (+ 1.0 (* x x))))))
(defun acosh (x)
       (log (+ x
               (* (1+ x)
                    (sqrt (/ (1- x) (1+ x)))))))
(defun atanh (x)
       (when (or (= x 1.0) (= x -1.0))
             (error "logarithmic singularity" x))
       (log (/ (1+ x) (sqrt (- 1.0 (* x x))))))
    


;; Additional Common Lisp Functions by Luke Tierney
;; from xlisp-stat

;;
;; Defsetf and documentation functions
;;

(defun apply-arg-rotate (f args) 
  (apply f (car (last args)) (butlast args)))

; (defsetf) - define setf method
(defmacro defsetf (sym first &rest rest)
  (if (symbolp first)
      `(progn (setf (get ',sym '*setf*) #',first) ',sym)
      (let ((f `#'(lambda ,(append (car rest) first) ,@(cdr rest)))
            (args (gensym)))
        `(progn
          (setf (get ',sym '*setf*) 
                #'(lambda (&rest ,args) (apply-arg-rotate ,f ,args)))
          ',sym))))

  
;;;;
;;;;
;;;; Modules, provide and require:  Luke Tierney, from xlisp-stat
;;;;
;;;;

; Uncomment these if you want them. It's non-standard, and nothing else
; in this distribution  uses them, so I'm commenting them out.  Tom Almy

;; Furthermore, WINTERP users won't want to define these because PROVIDE
;; and REQUIRE are built in to WINTERP's XLISP -- Niels Mayer.

;(defvar *modules*)
    
;(defun provide (name)
;  (pushnew name *modules* :test #'equal))
  
;(defun require (name &optional (path name))
;  (let ((name (string name))
;        (path (string path)))
;    (unless (member name *modules* :test #'equal)
;            (if (load path)
;                t
;		(load (strcat *default-path* path))))))

;;;;
;;;;
;;;; Miscellaneous Functions:  Luke Tierney
;;;;    from xlisp-stat
;;;;

;(defun vectorp (x)
;  (and (arrayp x) (= (array-rank x) 1)))

; equalp rewritten by Tom Almy to better match Common Lisp
(defun equalp (x y)
  (cond ((equal x y) t)
      ((numberp x) (if (numberp y) (= x y) nil))
      ((characterp x) (if (characterp y) (char-equal x y) nil))
      ((and (or (arrayp x) (stringp x)) 
            (or (arrayp y) (stringp y))
            (eql (length x) (length y)))
       (every #'equalp x y))))

; also improved by TAA to use *terminal-io*
(defun y-or-n-p (&rest args)
  (do ((answer nil 
	       (let* ((*breakenable* nil)
		      (x (errset (read *terminal-io*) nil)))
		     (when (consp x) (car x)))))
      ((member answer '(y n)) (eq answer 'y))
      (when args (apply #'format *terminal-io* args))
      (princ " (Y/N)" *terminal-io*)))

; This implementation is questionable (says TAA), I'm commenting it out

; (defun getf (place indicator &optional default)
;   (let ((mem (member indicator place :test #'eq)))
;    (if mem (second mem) default)))


; Improved by TAA to match common lisp definition
(defun functionp (x)
    (if (typep x '(or closure subr symbol))
	t
        (and (consp x) (eq (car x) 'lambda))))

(defmacro with-input-from-string (stream-string &rest body)
  (let ((stream (first stream-string))
        (string (second stream-string)))
    `(let ((,stream (make-string-input-stream ,string)))
       (progn ,@body))))


(defmacro with-input-from-string
	  (stream-string &rest body)
	  (let ((stream (first stream-string))
		(string (second stream-string))
		(start (second (member :start (cddr stream-string))))
		(end (second (member :end (cddr stream-string))))
		(index (second (member :index (cddr stream-string)))))
	       (when (null start) (setf start 0))
	       (if index
		   (let ((str (gensym)))
		    `(let* ((,str ,string)
			    (,stream (make-string-input-stream ,str 
							       ,start 
							       ,end)))
			   (prog1 (progn ,@body)
				  (setf ,index 
					(- (length ,str)
					   (length (get-output-stream-list 
						     ,stream)))))))
		   `(let ((,stream (make-string-input-stream ,string ,start ,end)))
			 (progn ,@body)))))
		   

(defmacro with-output-to-string (str-list &rest body)
  (let ((stream (first str-list)))
    `(let ((,stream (make-string-output-stream)))
       (progn ,@body)
       (get-output-stream-string ,stream))))
 
(defmacro with-open-file (stream-file-args &rest body)
  (let ((stream (first stream-file-args))
        (file-args (rest stream-file-args)))
    `(let ((,stream (open ,@file-args)))
       (unwind-protect 
           (progn ,@body)
         (when ,stream (close ,stream))))))
 
; (unintern sym) - remove a symbol from the oblist
(defun unintern (symbol)
  (let ((subhash (hash symbol (length *obarray*))))
    (cond ((member symbol (aref *obarray* subhash))
             (setf (aref *obarray* subhash)
                   (delete symbol (aref *obarray* subhash)))
             t)
          (t nil))))


;; array functions.   KCW    from  Kyoto Common Lisp

(defun fill (sequence item
             &key (start 0) end)
       (when (null end) (setf end (length sequence)))
       (do ((i start (1+ i)))
	   ((>= i end) sequence)
	   (setf (elt sequence i) item)))


(defun replace (sequence1 sequence2
                &key (start1 0) end1
                     (start2 0) end2)
    (when (null end1) (setf end1 (length sequence1)))
    (when (null end2) (setf end2 (length sequence2)))
    (if (and (eq sequence1 sequence2)
             (> start1 start2))
        (do* ((i 0 (1+ i))
              (l (if (< (- end1 start1) (- end2 start2))
                     (- end1 start1)
                     (- end2 start2)))
              (s1 (+ start1 (1- l)) (1- s1))
              (s2 (+ start2 (1- l)) (1- s2)))
            ((>= i l) sequence1)
          (setf (elt sequence1 s1) (elt sequence2 s2)))
        (do ((i 0 (1+ i))
             (l (if (< (- end1 start1)(- end2 start2))
                    (- end1 start1)
                    (- end2 start2)))
             (s1 start1 (1+ s1))
             (s2 start2 (1+ s2)))
            ((>= i l) sequence1)
          (setf (elt sequence1 s1) (elt sequence2 s2)))))


(defun acons (x y a)         ; from CLtL
   (cons (cons x y) a))


;; more set functions.  KCW    from Kyoto Common Lisp

;; Modified to pass keys to subfunctions without checking here
;; (more efficient)

;; (Tom Almy states:) we can't get the destructive versions of union
;; intersection, and set-difference to run faster than the non-destructive
;; subrs. Therefore we will just have the destructive versions do their
;; non-destructive counterparts

(setf (symbol-function 'nunion) 
      (symbol-function 'union)
      (symbol-function 'nintersection) 
      (symbol-function 'intersection)
      (symbol-function 'nset-difference) 
      (symbol-function 'set-difference))

(defun set-exclusive-or (list1 list2 &rest rest)
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest)
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'set-difference list2 list1 rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xlisp-2.1d/common")		;NPM -- for WINTERP
