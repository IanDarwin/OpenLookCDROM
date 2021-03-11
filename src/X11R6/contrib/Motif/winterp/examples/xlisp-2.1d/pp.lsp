; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         pp.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/pp.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  A pretty-printer for XLISP.
; Author:       Don Cohen, Jim Chapman, David Betz, Tom Almy
; Created:      
; Modified:     Mon Jun  6 03:01:50 1994 (Niels Mayer) npm@indeed
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

; PP.LSP -- a pretty-printer for XLISP.

; Adapted by Jim Chapman (Bix: jchapman) from a program written originally
; for IQLISP by Don Cohen.  Copyright (c) 1984, Don Cohen; (c) 1987, Jim
; Chapman. Modified for XLISP 2.0 by David Betz.

; In addition to the pretty-printer itself, this file contains a few functions
; that illustrate some simple but useful applications.

; The basic function accepts two arguments:

;      (PP OBJECT STREAM)

; where OBJECT is any Lisp expression, and STREAM optionally specifies the
; output (default is *standard-output*).

; PP-FILE pretty-prints an entire file.  It is what I used to produce this
; file (before adding the comments manually).  The syntax is:

;       (PP-FILE "filename" STREAM)

; where the file name must be a string or quoted, and STREAM, again, is the
; optional output destination.

; PP-DEF works just like PP, except its first argument is assumed to be the
; name of a function or macro, which is translated back into the original
; DEFUN or DEFMACRO form before printing.


; MISCELLANEOUS USAGE AND CUSTOMIZATION NOTES:

; 1.  The program uses tabs whenever possible for indentation.
;     This greatly reduces the cost of the blank space.  If your output
;     device doesn't support tabs, set TABSIZE to NIL -- which is what I
;     did when I pretty-printed this file, because of uncertainty 
;     about the result after uploading.

; 2.  Printmacros are used to handle special forms.  A printmacro is not
;     really a macro, just an ordinary lambda form that is stored on the
;     target symbol's property list.  The default printer handles the form
;     if there is no printmacro or if the printmacro returns NIL.

; 3.  Note that all the pretty-printer subfunctions, including the
;     the printmacros, return the current column position.

; 4.  Miser mode is not fully implemented in this version, mainly because  
;     lookahead was too slow.  The idea is, if the "normal" way of
;     printing the current expression would exceed the right margin, then
;     use a mode that conserves horizontal space.

; 5.  When PP gets to the last 8th of the line and has more to print than
;     fits on the line, it starts near the left margin.  This is not 
;     wonderful, but neither are the alternatives.  If you have a better
;     idea, go for it.

;  6. Storage requirements are about 1450 cells to load.  

;  7. I tested this with XLISP 1.7 on an Amiga.

;  8. TAA modified to support prettyprinting arrays.  Fixed bug printing
;     (NIL ...).

;  9. TAA modified to support prettyprinting of structures, and some code
;     cleanup. Also added PP-PAIR-FORM to handle setq like structures
;     more nicely. 

; 10. TAA: It should be noted that you can't pretty print circular lists,
;     nor can you successfully read back the following:
;	* uninterned symbols, for instance those generated with gensym
;         as part of automatically generated code
;       * closures, since their environment cannot be reconstructed. These
;         are not even expanded.
;       * subrs, fsubrs, and streams cannot be represented

; 11. TAA modified so that non-class objects are shown by sending the
;	message :storeon (see classes.lsp), printing #. before the expression
;	making it an object literal.

; 11. TAA modified so that *print-level* and *print-length* are bound to  NIL
;	during the course of execution.

; An ugly false def so things don't fall apart if classes.lsp not loaded
(unless (fboundp 'defclass) (defun classp (x) (objectp x)))




;(DEFUN SYM-FUNCTION (X)	;for Xlisp 1.7
;    (CAR (SYMBOL-VALUE X)))
(defun sym-function (x)		;for Xlisp 2.0
    (get-lambda-expression (symbol-function x)))

(defvar tabsize 8)	;set this to NIL for no tabs

(defvar maxsize 60)	;for readability, PP tries not to print more
			;than this many characters on a line

(defvar miser-size 2)	;the indentation in miser mode

(defvar min-miser-car 4)	;used for deciding when to use miser mode

(defvar max-normal-car 9)	;ditto

(defconstant pp-lpar "(")	; self evident
(defconstant pp-rpar ")")
(defconstant pp-space " ")
(defconstant pp-immed "#.")

; The following function prints a file

(defun pp-file (filename &optional streamout)
    (or streamout (setq streamout *standard-output*))
    (princ "; Listing of " streamout)
    (princ filename streamout)
    (terpri streamout)
    (terpri streamout)
    (do* ((fp (open filename)) (expr (read fp) (read fp)))
         ((null expr) (close fp))
      (pp expr streamout)
      (terpri streamout)))


; Print a lambda or macro form as a DEFUN or DEFMACRO:

(defmacro pp-def (who &optional stream)
    `(pp (make-def ,who) ,stream))

(defmacro make-def (name &aux expr type)
    (setq expr (sym-function name))
    (setq type
          (cadr (assoc (car expr)
                       '((lambda defun) (macro defmacro)))))
    (list 'quote
          (append (list type name) (cdr expr))))



; The pretty-printer high level function:


(defun pp (x &optional stream)
       (let (*print-level* *print-length*)	; set special vars to NIL
	    (or stream (setq stream *standard-output*))
	    (pp1 x stream 1 80)
	    (terpri stream)
	    t))

; print X on STREAM, current cursor is CURPOS, and right margin is RMARGIN

(defun pp1 (x stream curpos rmargin 
	      &aux (anarray (arrayp x))
		   (astruct (typep x '(and struct (not random-state))))
		   size position width)
    (cond (anarray (setq x (coerce x 'cons)))
	  ((and (objectp x) (not (classp x)))
	   (princ pp-immed stream)		; immediate execute literal
	   (setq curpos (+ curpos 2))
	   (setq x (send x :storeon))))
    (cond (astruct (pp-astruct x stream curpos rmargin))
	  ((not (consp x))(prin1 x stream) (+ curpos (flatsize x)))
          ((printmacrop x stream curpos rmargin))
          ((and (> (flatsize x) (- rmargin curpos))
                (< (* 8 (- rmargin curpos)) rmargin))
           (setq size (+ (/ rmargin 8) (- curpos rmargin)))
           (pp-moveto stream curpos size)
           (setq position (pp1 x stream size rmargin))
           (pp-moveto stream position size))
          (t (when anarray (princ "#" stream) (setq curpos (1+ curpos)))
	     (princ pp-lpar stream)
             (setq position
                   (pp1 (car x) stream (1+ curpos) rmargin))
             (cond ((and (>= (setq width (- rmargin position))
                             (setq size (flatsize (cdr x))))
                         (<= size maxsize))
                    (pp-rest-across (cdr x) stream position rmargin))
                   ((consp (car x))
                    (pp-moveto stream position curpos)
                    (pp-rest (cdr x) stream curpos rmargin))
                   ((> (- position curpos) max-normal-car)
                    (pp-moveto stream position (+ curpos miser-size))
                    (pp-rest (cdr x) stream (+ curpos miser-size) rmargin))
                   (t (pp-rest (cdr x) stream position rmargin))))))

; PP-MOVETO controls indentating and tabbing.
; If CUR > GOAL then goes to new line first.
; will space to GOAL

(defun pp-moveto (stream curpos goalpos &aux i)
    (cond ((> curpos goalpos)
           (terpri stream)
           (setq curpos 1)
           (if tabsize
               (do nil
                   ((< (- goalpos curpos) tabsize))
                 (princ "\t" stream)
                 (setq curpos (+ curpos tabsize))))))
    (dotimes (i (- goalpos curpos)) (princ pp-space stream))
    goalpos)

; can print the rest of the list without new lines

(defun pp-rest-across (x stream curpos rmargin &aux position)
    (setq position curpos)
    (prog nil
      lp
      (cond ((null x) (princ pp-rpar stream) (return (1+ position)))
            ((not (consp x))
             (princ " . " stream)
             (prin1 x stream)
             (princ pp-rpar stream)
             (return (+ 4 position (flatsize x))))
            (t (princ pp-space stream)
               (setq position
                     (pp1 (car x) stream (1+ position) rmargin))
               (setq x (cdr x))
               (go lp)))))

; Can print the rest of the list, but must use new lines for each element


(defun pp-rest (x stream curpos rmargin &aux position pos2)
    (setq position curpos)
    (prog nil
      lp
      (cond ((null x) (princ pp-rpar stream) (return (1+ position)))
            ((not (consp x))
             (and (> (flatsize x) (- (- rmargin position) 3))
                  (setq position (pp-moveto stream position curpos)))
             (princ " . " stream)
             (prin1 x stream)
             (princ pp-rpar stream)
             (return (+ position 4 (flatsize x))))
            ((and 
		  (not (typep (car x) '(or list array struct)))
                  (<= (setq pos2 (+ 1 position (flatsize (car x))))
                      rmargin)
                  (<= pos2 (+ curpos maxsize)))
             (princ pp-space stream)
             (prin1 (car x) stream)
             (setq position pos2))
            (t (pp-moveto stream position (1+ curpos))
               (setq position
                     (pp1 (car x) stream (1+ curpos) rmargin))))
      (cond ((and (consp (car x)) (cdr x))
             (setq position (pp-moveto stream position curpos))))
      (setq x (cdr x))
      (go lp)))


; Handles structures by printing in form:
;	#S(structtype :slot val
; ...
;		      :slot val)
;
; code does not check for defaults.

(defun pp-astruct (x stream pos rmar &aux cur snames args)
       (setq cur pos
	     snames (mapcar #'car (get (type-of x) '*struct-slots*))
	     args 
	     (mapcan #'(lambda (p) 
			       (list p
				     (apply
				      (intern
				       (strcat (string (type-of x)) 
					       "-" 
					       (string p)))
				      (list x))))
		     snames))
       (princ "#s" stream)
       (if (and (>= (- rmar pos) (+ 2 (flatsize x)))
		(<= (flatsize x) maxsize))
	   (pp1 (cons (type-of x) args) stream (+ 2 pos) rmar)
	   (prog ()
		 (princ pp-lpar stream)
		 (prin1 (type-of x) stream)
		 (princ pp-space stream)
		 (setq pos (setq cur (+ pos 4 (flatsize (type-of x)))))
		 lp
		 (prin1 (first args) stream)
		 (princ pp-space stream)
		 (setq cur
		       (pp1 (second args)
			    stream
			    (+ pos 1 (flatsize (first args)))
			    rmar))
		 (setq args (cddr args))
		 (when (null args)
		       (princ pp-rpar stream)
		       (return-from pp-astruct (1+ cur)))
		 (pp-moveto stream cur pos)
		 (go lp))))

	     
; PRINTMACROP is the printmacro interface routine.  Note that the
; called function has the same argument list as PP1.  It may either
; decide not to handle the form, by returning NIL (and not printing)
; or it may print the form and return the resulting position.

(defun printmacrop (x stream curpos rmargin &aux macro)
    (and (symbolp (car x))
	 (car x)	; must not be NIL (TAA fix)
         (setq macro (get (car x) 'printmacro))
         (apply macro (list x stream curpos rmargin))))

; The remaining forms define various printmacros.


; Printing format (xxx xxx
;		       <pp-rest>)


(defun pp-binding-form (x stream pos rmar &aux cur)
    (setq cur pos)
    (cond ((and (>= (- rmar pos) (flatsize x))
                (<= (flatsize x) maxsize)) nil)
          ((> (length x) 2)
           (princ pp-lpar stream)
           (prin1 (car x) stream)
           (princ pp-space stream)
           (setq cur
                 (pp1 (cadr x)
                      stream
                      (+ 2 pos (flatsize (car x)))
                      rmar))
           (pp-moveto stream cur (+ pos 1))
           (pp-rest (cddr x) stream (+ pos 1) rmar))))

; Format (xxxx xxx xxx
;...
;	       xxx xxx)

(defun pp-pair-form (x stream pos rmar &aux cur)
    (setq cur pos)
    (cond ((and (>= (- rmar pos) (flatsize x))
                (<= (flatsize x) maxsize)) nil)
          ((> (length x) 1)
           (princ pp-lpar stream)
           (prin1 (first x) stream)
           (princ pp-space stream)
	   (setq pos (setq cur (+ pos 2 (flatsize (first x)))))
	   (setq x (rest x))
	   (loop
	    (pp-moveto stream cur pos)
	    (setq cur (pp1 (first x) stream pos rmar))
	    (princ pp-space stream)
	    (setq x (rest x))
	    (setq cur (pp1 (first x) stream (1+ cur) rmar))
	    (when (null (setq x (rest x)))
		  (princ pp-rpar stream)
		  (return-from pp-pair-form (1+ cur)))))))

; format (xxx xxx
;	      xxx
;	    <pprest>)

       
(defun pp-do-form (x stream pos rmar &aux cur pos2)
    (setq cur pos)
    (cond ((and (>= (- rmar pos) (flatsize x))
                (<= (flatsize x) maxsize)) nil)
          ((> (length x) 2)
           (princ pp-lpar stream)
           (prin1 (car x) stream)
           (princ pp-space stream)
           (setq pos2 (+ 2 pos (flatsize (car x))))
           (setq cur (pp1 (cadr x) stream pos2 rmar))
           (pp-moveto stream cur pos2)
           (setq cur (pp1 (caddr x) stream pos2 rmar))
           (pp-moveto stream cur (+ pos 1))
           (pp-rest (cdddr x) stream (+ pos 1) rmar))))

; format (xxx xxx xxx
;	   <pprest>)

(defun pp-defining-form (x stream pos rmar &aux cur)
    (setq cur pos)
    (cond ((and (>= (- rmar pos) (flatsize x))
                (<= (flatsize x) maxsize)) nil)
          ((> (length x) 3)
           (princ pp-lpar stream)
           (prin1 (car x) stream)
           (princ pp-space stream)
           (prin1 (cadr x) stream)
           (princ pp-space stream)
           (setq cur
                 (pp1 (caddr x)
                      stream
                      (+ 3 pos (flatsize (car x)) (flatsize (cadr x)))
                      rmar))
           (pp-moveto stream cur (+ 3 pos))
           (pp-rest (cdddr x) stream (+ 3 pos) rmar))))

(putprop 'quote
         '(lambda (x stream pos rmargin)
            (cond ((and (cdr x) (null (cddr x)))
                   (princ "'" stream)
                   (pp1 (cadr x) stream (1+ pos) rmargin))))
         'printmacro)

(putprop 'backquote
         '(lambda (x stream pos rmargin)
            (cond ((and (cdr x) (null (cddr x)))
                   (princ "`" stream)
                   (pp1 (cadr x) stream (1+ pos) rmargin))))
         'printmacro)

(putprop 'comma
         '(lambda (x stream pos rmargin)
            (cond ((and (cdr x) (null (cddr x)))
                   (princ "," stream)
                   (pp1 (cadr x) stream (1+ pos) rmargin))))
         'printmacro)

(putprop 'comma-at
         '(lambda (x stream pos rmargin)
            (cond ((and (cdr x) (null (cddr x)))
                   (princ ",@" stream)
                   (pp1 (cadr x) stream (+ pos 2) rmargin))))
         'printmacro)

(putprop 'function
         '(lambda (x stream pos rmargin)
            (cond ((and (cdr x) (null (cddr x)))
                   (princ "#'" stream)
                   (pp1 (cadr x) stream (+ pos 2) rmargin))))
         'printmacro)

(putprop 'prog
         'pp-binding-form
         'printmacro)

(putprop 'prog*
         'pp-binding-form
         'printmacro)

(putprop 'let
         'pp-binding-form
         'printmacro)

(putprop 'let*
         'pp-binding-form
         'printmacro)

(putprop 'lambda
         'pp-binding-form
         'printmacro)

(putprop 'macro
         'pp-binding-form
         'printmacro)

(putprop 'do 'pp-do-form 'printmacro)

(putprop 'do*
         'pp-do-form
         'printmacro)

(putprop 'defun
         'pp-defining-form
         'printmacro)

(putprop 'defmacro
         'pp-defining-form
         'printmacro)


(putprop 'setq
	 'pp-pair-form
	 'printmacro)

(putprop 'setf
	 'pp-pair-form
	 'printmacro)

(putprop 'psetq
	 'pp-pair-form
	 'printmacro)


(putprop 'send
	 'pp-defining-form
	 'printmacro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xlisp-2.1d/pp")		;NPM -- for WINTERP
