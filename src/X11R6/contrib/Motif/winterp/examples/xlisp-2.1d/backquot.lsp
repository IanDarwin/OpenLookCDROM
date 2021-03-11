; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         backquot.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/backquot.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Backquote Implementation from Common Lisp
; Author:       Guy L. Steele Jr., Tom Almy
; Created:      27 December 1985
; Modified:     Mon Jun  6 03:03:02 1994 (Niels Mayer) npm@indeed
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

;;; Backquote Implementation from Common Lisp
;;; Author: Guy L. Steele Jr.  Date: 27 December 1985
;;; This software is in the public domain


;;; TAA notes:
;;; Converted to XLISP from the CLtL book, July, 1991, by Tom Almy
;;; Expression simplification code removed.

;;; Reader Macros -- already exist for ` , and ,@ that generate correct
;;;  code for this backquote implementation.

;;; This implementation will execute far slower than the XLISP original, 
;;; but since macros expansions can replace the original code
;;; (at least with my modified XLISP implementation)
;;; most applications will run at their full speed after the macros have
;;; been expanded once.


(defmacro backquote (x)
	  (bq-process x))

(defun bq-process (x)
       (cond ((atom x) (list 'quote x))
	     ((eq (car x) 'backquote)
	      (bq-process (bq-process (cadr x))))
	     ((eq (car x) 'comma) (cadr x))
	     ((eq (car x) 'comma-at)
	      (error ",@ after `" (cadr x)))
	     (t (do ((p x (cdr p))
		     (q '() (cons (bq-bracket (car p)) q)))
		    ((atom p)
		     (if (null p)	;; simplify if proper list TAA MOD
			 (cons 'append (nreverse q))
			 (cons 'append
			       (nconc (nreverse q) (list (list 'quote p))))))
		    (when (eq (car p) 'comma)
			  (unless (null (cddr p)) (error "Malformed ," p))
			  (return (cons 'append
					(nconc (nreverse q) 
					       (list (cadr p))))))
		    (when (eq (car p) 'comma-at)
			  (error "Dotted ,@" p))
		    ))))

(defun bq-bracket (x)
       (cond ((atom x)
	      (list 'list (list 'quote x)))
	     ((eq (car x) 'comma)
	      (list 'list (cadr x)))
	     ((eq (car x) 'comma-at)
	      (cadr x))
	     (t (list 'list (bq-process x)))))
