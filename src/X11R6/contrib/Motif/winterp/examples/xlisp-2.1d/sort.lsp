; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         sort.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/sort.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Sort Routines
; Author:       Tom Almy
; Created:      
; Modified:     Mon Jun  6 03:01:08 1994 (Niels Mayer) npm@indeed
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

;; Sort routines.  
;; by Tom Almy


;; The built in sort does a quick sort which does a bad job if the list is
;; already sorted.  INSERT is a destructive insertion into a sorted list.
;; Also, these are iterative and will handle lists of any size.  SORT can
;; cause eval stack overflows on big lists.
;; In these functions, "function" is a predicate that orders the list
;; (For numbers, typically #'< ).
(defun insert (element list function)
	(cond ((null list) (list element))
	      ((funcall function element (first list))
	       (cons element list))
	      (t (do ((prev list (rest prev)))
	      	     ((or (endp (rest prev)) 
		     	  (funcall function element (second prev)))
		      (rplacd prev (cons element (rest prev)))
		      list)))))

;; And this inserts a list of items into an existing list (which can be nil)

(defun insertall (elements list function)
	(dolist (element elements list)
		(setq list (insert element list function))))


;; Once the list has been sorted, accessing is faster if the list is
;; placed in an array, and a binary search is performed.
;; The advantage starts at about 250 elements

(defun memarray (element array &key (test #'eql) (function #'<))
	(let* ((max (1- (length array)))
	      (min 0)
	      (index (/ (+ max min) 2)))
	     (loop (when (funcall test element (aref array index))
	     		 (return index))
		   (if (funcall function element (aref array index))
		       (setq max (1- index))
		       (setq min (1+ index)))
		   (when (> min max) (return nil))
		   (setq index (/ (+ max min) 2)))))


