; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         infix.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/infix.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  An infix to prefix converter for algebraic expressions.
		From Winston and Horn, Second Edition, pp 185-189.
; Author:       Winston and Horn, Jonathan Roger Greenblatt
; Created:      
; Modified:     Mon Jun  6 03:02:05 1994 (Niels Mayer) npm@indeed
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

;;;
;;; An infix to prefix converter for algebraic expressions.
;;; From Winston and Horn, Second Edition, pp 185-189.
;;;
;
;	Adapted as a lisp macro by:
;		Jonathan Roger Greenblatt (jonnyg@rover.umd.edu)
;		University of Maryland at College Park
;
;
;	(usage:
;
;		[ <expr> <oper> <expr> ( <oper> <expr> ) ... ]
;
;	<expr>: a lisp expresion.
;	<oper>: =,+,-,*,/,mod.**,^
;
;	Note: [ and ] are part of the syntax, ( and ) mean this part is
;				optional.
;
;	Examples:
;
;		[a = 7 * 5 + 4]
;		[b = 7 + (sin (float a)) + (float [a / 7]) * [3 + a]]
;
;	These are expanded to:
;
;		(SETQ A (+ (* 7 5) 4))
;		(SETQ B (+ (+ 7 (SIN (FLOAT A))) (* (FLOAT (/ A 7)) (+ 3 A))))
;
;

(defun inf-to-pre (ae)
  (labels
	((weight (operator)
	  (case operator
	    (= 0)
	    (+ 1)
	    (- 1)
	    (* 2)
	    (/ 2)
	    (mod 2)
	    (** 3)
	    (^ 3)
	    (t 4)))

	(opcode (operator)
	  (case operator
	    (= 'setq)
	    (+ '+)
	    (- '-)
	    (* '*)
	    (/ '/)
	    (mod 'mod)
	    (** 'expt)
	    (^ 'expt)
	    (t (error "invalid operator" operator))))

	(inf-aux (ae operators operands)
	  (inf-iter (cdr ae)
	    operators
	    (cons (car ae) operands)))

	(inf-iter (ae operators operands)
	  (cond ((and (null ae) (null operators))
		 (car operands))
		((and (not (null ae))
		      (or (null operators)
			  (> (weight (car ae))
			     (weight (car operators)))))
		 (inf-aux (cdr ae)
			  (cons (car ae) operators)
			  operands))
		(t (inf-iter ae
			     (cdr operators)
			     (cons (list (opcode (car operators))
					 (cadr operands)
					 (car operands))
				   (cddr operands)))))))

  (if (atom ae)
      ae
      (inf-aux ae nil nil))))

(setf (aref *readtable* (char-int #\[))
  (cons :tmacro
	(lambda (f c &aux ex)
		(setf ex nil)
		(do () ((eq (peek-char t f) #\]))
			(setf ex (append ex (cons (read f) nil))))
		(read-char f)
		(cons (inf-to-pre ex) nil))))

(setf (aref *readtable* (char-int #\]))
  (cons :tmacro
	(lambda (f c)
		(error "misplaced right bracket"))))


