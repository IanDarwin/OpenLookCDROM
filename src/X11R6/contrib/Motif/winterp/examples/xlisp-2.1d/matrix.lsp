; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         matrix.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/matrix.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Matrix functions; multidimensional arrays.
; Author:       Tom Almy
; Created:      
; Modified:     Mon Jun  6 03:01:54 1994 (Niels Mayer) npm@indeed
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

; Matrix functions by Tom Almy
; Multidimensional arrays are implemented here as arrays of arrays
; make-array is redefined to mimic common lisp
; Unfortunately AREF cannot be changed since its operation in setf is
; "wired in", so we will use a new (macro) function MREF


(when (eq (type-of (symbol-function 'make-array))
	  'subr)
      (setf (symbol-function 'orig-make-array)
	    (symbol-function 'make-array)))

(defun make-array (dims &key initial)
    (cond ((null dims) initial)
	  ((atom dims) (make-array (list dims) :initial initial))
	  (t (let ((result (orig-make-array (first dims))))
	       (when (or (rest dims) initial)
		     (dotimes (i (first dims))
			      (setf (aref result i)
				    (make-array (rest dims) :initial initial))))
	       result))))

(defun mref (matrix &rest indices)
    (dolist (index indices)
	    (setq matrix (aref matrix index)))
    matrix)

(setf (get 'mref '*setf*)
      #'(lambda (mat &rest arglist)
	  (do ((index (first arglist) (first remainder))
	       (remainder (rest arglist) (rest remainder)))
	      ((null (rest remainder))
	       (setf (aref mat index) (first remainder)))
	    (setf mat (aref mat index)))))

