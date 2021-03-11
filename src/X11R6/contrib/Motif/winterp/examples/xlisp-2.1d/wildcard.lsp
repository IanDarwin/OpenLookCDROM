; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wildcard.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/wildcard.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Wildcard Pattern matching algorithm
; Author:       Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:00:38 1994 (Niels Mayer) npm@indeed
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

; Wildcard Pattern matching algorithm
; * matches any substring (zero or more characters)
; ? matches any character
; ~c matches c

(defun match (pattern list)
       (labels ((match1 (pattern suspect)
		      (cond ((null pattern) (null suspect))
			    ((null suspect) (equal pattern '(:mult)))
			    ((eq (first pattern) :single)
			     (match1 (cdr pattern) (cdr suspect)))
			    ((eq (first pattern) :mult)
			     (if (null (rest pattern))
				 t 
				 (do ((p (rest pattern))
				      (l suspect (cdr l)))
				     ((or (null l) (match1 p l)) 
				      (not (null l))))))
			    ((eq (first pattern) (first suspect))
			     (match1 (rest pattern) (rest suspect)))
			    (t nil)))
	      (explode (list) 
		       (cond ((null list) nil)
			     ((eq (first list) #\*) 
			      (cons :mult (explode (rest list))))
			     ((eq (first list) #\?) 
			      (cons :single (explode (rest list))))
			     ((eq (first list) #\~) 
			      (cons (second list)
				    (explode (rest (rest list)))))
			     (t (cons (first list) (explode (rest list)))))))
	     (let ((pat (explode (coerce pattern 'cons))))
		  (mapcan #'(lambda (x) (when (match1 pat (coerce x 'cons))
					      (list x)))
			  list))))

(setq l (sort (apply #'nconc (map 'cons 
			    #'(lambda (x) (mapcar #'string x)) 
			    *obarray*))
	      #'string<))
