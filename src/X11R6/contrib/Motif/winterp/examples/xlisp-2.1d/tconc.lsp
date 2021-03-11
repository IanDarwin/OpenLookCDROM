; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         tconc.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/tconc.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Interfaces to stuff used internally by XLISP for string streams
; Author:       Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:00:49 1994 (Niels Mayer) npm@indeed
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

;; Not part of Common Lisp, but used in XLISP internally for string streams

;; THE CAR OF A TCONC POINTS TO THE TCONC LIST,
;; THE TAIL POINTS TO LAST ELEMENT

(defun make-tconc nil
    (cons 'nil 'nil))

(defun tconc (tc new)
    (let ((newl (cons new 'nil)))
      (if (null (cdr tc))
	  (rplaca tc newl)
	  (rplacd (cdr tc) newl))
      (rplacd tc newl)
      tc))

(defun lconc (tc list)
    (cond ((not (null list))
	   (if (null (cdr tc))
	       (rplaca tc list)
	       (rplacd (cdr tc) list))
	   (rplacd tc (last list))))
    tc)

(defun remove-head (tc)
    (cond ((null (car tc)) 'nil)
	  ((null (cdar tc))
	   (let ((element (caar tc)))
	     (rplaca tc 'nil)
	     (rplacd tc 'nil)
	     element))
	  (t (let ((element (caar tc)))
	       (rplaca tc (cdar tc))
	       element))))
