; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         stepper.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/stepper.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Single step debugger
; Author:       Ray Comas (comas@math.lsa.umich.edu)
; Created:      
; Modified:     Mon Jun  6 03:01:01 1994 (Niels Mayer) npm@indeed
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

;;
;; File: STEP.LSP
;; Author: Ray Comas (comas@math.lsa.umich.edu)
;;
 
(defmacro while (test &rest forms) `(do () ((not ,test)) ,@forms))

(defparameter *hooklevel* 0)		;create the nesting level counter.
(defvar *pdepth*	3)		;create depth counter
(defvar *plen*		3)		;create length counter
(defparameter *fcn*	'*all*)		;create "one-shot" breakpoint specifier
(defvar *steplist*	nil)		;create breakpoint list
(defparameter *steptrace* '(t . t))	;create stepping flags
(defparameter *callist*	nil)		;create call list for backtrace
 
; this macro invokes the stepper.
(defmacro step (form &aux val)
  `(progn
     (setq *hooklevel*	0		;init nesting counter
	   *fcn*	'*all*		;init break-point specifier
	   *steptrace*	'(t . t))
     (setq *callist* (list (car ',form))) ;init call list
     (terpri *debug-io*)
     (step-flush)
     (princ *hooklevel* *debug-io*)
     (princ " >==> " *debug-io*)
     (prin1 ',form *debug-io*)		;print the form
     (setq val (evalhook ',form		;eval, and kick off stepper
			 #'eval-hook-function
			 nil
			 nil))
     (terpri *debug-io*)
     (princ *hooklevel* *debug-io*)	;print returned value
     (princ " <==< " *debug-io*)
     (prin1 val *debug-io*)
     (terpri *debug-io*)
     val))				;and return it
 
(defun eval-hook-function (form env &aux val cmd)
  (setq *hooklevel* (1+ *hooklevel*))	;incr. the nesting level
  (cond ((consp form)			;if interpreted function ...
	 (setq *callist*
	       (cons (car form) *callist*)) ;add fn. to call list
	 (tagbody
	  (loop				;repeat forever ...
					;check for a breakpoint
	   (when (and (not (equal *fcn* '*all*))
		      (not (equal *fcn* (car form)))
		      (not (and (numberp *fcn*) (>= *fcn* *hooklevel*))))
		 (unless (and *fcn* (member (car form) *steplist*))
 
					;no breakpoint reached -- continue
			 (setf (cdr *steptrace*) nil)
			 (when (car *steptrace*)
			       (setf (cdr *steptrace*) t)
			       (fcprt form))
			 (fix-go)
			 (setq val (evalhook form
					     #'eval-hook-function
					     nil
					     env))
			 (go next)))
 
					;breakpoint reached -- fix things & get a command
	   (fcprt form)
	   (setf (cdr *steptrace*) t)
	   (setq *fcn* '*all*)		;reset breakpoint specifier
	   (princ " :" *debug-io*)	;prompt user
	   (setq cmd			;get command from user
		 (char-downcase (code-char (get-key))))
 
					;process user's command
	   (cond
	    ((or (eql cmd #\n) (eql cmd #\Space)) ;step into function
	     (fix-go)
	     (setq val (evalhook form
				 #'eval-hook-function
				 nil
				 env))
	     (go next))
	    ((or (eql cmd #\s) (eql cmd #\Newline)) ;step over function
	     (fix-go)
	     (setq val (evalhook form nil nil env))
	     (go next))
	    ((eql cmd #\g)		;go until breakpt. reached
	     (setq *fcn* t)
	     (fix-go)
	     (setq val (evalhook form
				 #'eval-hook-function
				 nil
				 env))
	     (go next))
	    ((eql cmd #\w)		;backtrace
	     (step-baktrace))
	    ((eql cmd #\h)		;display help
	     (step-help))
	    ((eql cmd #\p)		;pretty-print form
	     (terpri *debug-io*)
	     (pprint form *debug-io*))
	    ((eql cmd #\f)		;set function breakpoint
	     (princ "Go to fn.: " *debug-io*)
	     (setq *fcn* (read *debug-io*))
	     (step-flush))
	    ((eql cmd #\u)		;go up one level
	     (setq *fcn* (1- *hooklevel*)))
	    ((eql cmd #\b)		;set breakpoint
	     (princ "Bkpt.: " *debug-io*)
	     (step-set-breaks (read *debug-io*))
	     (step-flush))
	    ((eql cmd #\c)		;clear a breakpoint
	     (princ "Clear: " *debug-io*)
	     (step-clear-breaks (read *debug-io*))
	     (step-flush))
	    ((eql cmd #\t)		;toggle trace mode
	     (setf (car *steptrace*)
		   (not (car *steptrace*)))
	     (princ "Trace = " *debug-io*)
	     (prin1 (car *steptrace*) *debug-io*))
	    ((eql cmd #\q)		;quit stepper
	     (setq *fcn* nil))
	    ((eql cmd #\x)		;evaluate a form
	     (princ "Eval: " *debug-io*)
	     (step-do-form (read *debug-io*) env)
	     (step-flush))
	    ((eql cmd #\r)		;return given expression
	     (princ "Return: " *debug-io*)
	     (setq val (evalhook (read *debug-io*) nil nil env))
	     (step-flush)
	     (go next))
	    ((eql cmd #\#)		;set new compress level
	     (princ "Depth: " *debug-io*)
	     (step-set-depth (read *debug-io*))
	     (step-flush))
	    ((eql cmd #\.)
	     (princ "Len.: " *debug-io*)
	     (step-set-length (read *debug-io*))
	     (step-flush))
	    ((eql cmd #\e)		;print environment
	     (step-print-env env))
	    (t (princ "Bad command.  Type h for help\n" *debug-io*))))
 
	  next				;exit from loop
	  (setq *callist* (cdr *callist*)) ;remove fn. from call list
	  (when (cdr *steptrace*)
		(terpri *debug-io*)
		(step-spaces *hooklevel*)
		(princ *hooklevel* *debug-io*)
		(princ " <==< " *debug-io*) ;print the result
		(prin1 val *debug-io*))))
 
					;not an interpreted function -- just trace thru.
	(t (unless (not (symbolp form))
		   (when (car *steptrace*)
			 (terpri *debug-io*)
			 (step-spaces *hooklevel*) ;if form is a symbol ...
			 (princ "         " *debug-io*)
			 (prin1 form *debug-io*) ;... print the form ...
			 (princ " = " *debug-io*)))
	   (setq val (evalhook form nil nil env)) ;eval it
	   (unless (not (symbolp form))
		   (when (car *steptrace*)
			 (prin1 val *debug-io*))))) ;... and the value
  (setq *hooklevel* (1- *hooklevel*))	;decrement level
  val)					;and return the value
 
;compress a list
(defun compress (l cd cl ol)		;cd = depth, cl = length, ol = orig. length
  (cond
   ((null l) nil)
   ((eql cl 0) '(...))
   ((atom l) l)
   ((eql cd 0) '#\#)
   (t (cons (compress (car l) (1- cd) ol ol)
	    (compress (cdr l) cd (1- cl) ol)))))
 
;compress and print a form
(defun fcprt (form)
  (terpri *debug-io*)
  (step-spaces (min 20 *hooklevel*))
  (princ *hooklevel* *debug-io*)
  (princ " >==> " *debug-io*)
  (prin1 (compress form *pdepth* *plen* *plen*) *debug-io*)
  (princ " " *debug-io*))
 
;a non-recursive fn to print spaces (not as elegant, easier on the gc)
(defun step-spaces (n) (dotimes (i n) (princ " " *debug-io*)))
 
;and one to clear the input buffer
(defun step-flush () (while (not (eql (read-char *debug-io*) #\newline))))
 
;print help
(defun step-help ()
  (terpri *debug-io*)
  (princ "Stepper Commands\n" *debug-io*)
  (princ "----------------\n" *debug-io*)
  (princ " n or space - next form\n" *debug-io*)
  (princ " s or <cr>  - step over form\n" *debug-io*)
  (princ " f FUNCTION - go until FUNCTION is called\n" *debug-io*)
  (princ " b FUNCTION - set breakpoint at FUNCTION\n" *debug-io*)
  (princ " b <list>   - set breakpoint at each function in list\n" *debug-io*)
  (princ " c FUNCTION - clear breakpoint at FUNCTION\n" *debug-io*)
  (princ " c <list>   - clear breakpoint at each function in list\n" *debug-io*)
  (princ " c *all*    - clear all breakpoints\n" *debug-io*)
  (princ "          g - go until a breakpoint is reached\n" *debug-io*)
  (princ "          u - go up; continue until enclosing form is done\n" *debug-io*)
  (princ "          w - where am I? -- backtrace\n" *debug-io*)
  (princ "          t - toggle trace on/off\n" *debug-io*)
  (princ "          q - quit stepper, continue execution\n" *debug-io*)
  (princ "          p - pretty-print current form (uncompressed)\n" *debug-io*)
  (princ "          e - print environment\n" *debug-io*)
  (princ "   x <expr> - execute expression in current environment\n" *debug-io*)
  (princ "   r <expr> - execute and return expression\n" *debug-io*)
  (princ "       # nn - set print depth to nn\n" *debug-io*)
  (princ "       . nn - set print length to nn\n" *debug-io*)
  (princ "          h - print this summary\n" *debug-io*)
  (terpri *debug-io*))
 
;evaluate a form in the given environment
(defun step-do-form (f1 env)
  (step-spaces *hooklevel*)
  (princ *hooklevel* *debug-io*)
  (princ " res: " *debug-io*)
  (prin1 (evalhook f1 nil nil env) *debug-io*))	;print result
 
;set new print depth
(defun step-set-depth (cf)
  (cond ((numberp cf)
	 (setq *pdepth* (truncate cf)))
	(t (setq *pdepth* 3))))
 
;set new print length
(defun step-set-length (cf)
  (cond ((numberp cf)
	 (setq *plen* (truncate cf)))
	(t (setq *plen* 3))))
 
;print environment
(defun step-print-env (env)
  (terpri *debug-io*)
  (step-spaces *hooklevel*)
  (princ *hooklevel* *debug-io*)
  (princ " env: " *debug-io*)
  (prin1 env *debug-io*)
  (terpri *debug-io*))
 
;set breakpoints
(defun step-set-breaks (l)
  (cond ((null l) t)
	((symbolp l) (setq *steplist* (cons l *steplist*)))
	((listp l)
	 (step-set-breaks (car l))
	 (step-set-breaks (cdr l)))))
 
;clear breakpoints
(defun step-clear-breaks (l)
  (cond ((null l) t)
	((eql l '*all*) (setq *steplist* nil))
	((symbolp l) (delete l *steplist*))
	((listp l)
	 (step-clear-breaks (car l))
	 (step-clear-breaks (cdr l)))))
 
;print backtrace
(defun step-baktrace (&aux l n)
  (setq l *callist*
	n *hooklevel*)
  (while (>= n 0)
    (terpri *debug-io*)
    (step-spaces n)
    (prin1 n *debug-io*)
    (princ " " *debug-io*)
    (prin1 (car l) *debug-io*)
    (setq l (cdr l))
    (setq n (1- n)))
  (terpri *debug-io*))
 
(defun fix-go ()
  (when (equal (car *callist*) 'go)
	(setq *hooklevel* (1- *hooklevel*))
	(setq *callist* (cdr *callist*))))

