; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         match.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/match.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Pattern matching from Chapter 24 Winston&Horn 3rd Edition
; Author:       Winston&Horn, Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:01:56 1994 (Niels Mayer) npm@indeed
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

;; Pattern matching Chapter 24 Winston&Horn 3rd Edition

(defun add-binding (pve datum bindings)
	(if (eq '_ (extract-variable pve))
	    bindings
	    (cons (make-binding (extract-variable pve) datum) bindings)))

(defun extract-variable (pve) (second pve))

(defun make-binding (variable datum) (list variable datum))

(defun find-binding (pve binding)
	(unless (eq '_ (extract-variable pve))
		(assoc (extract-variable pve) binding)))


(defun extract-key (binding) (first binding))
(defun extract-value (binding) (second binding))

(defun match-atoms (p d bindings)
	(if  (eql p d)
	     bindings
	     'fail))

(defun match-variable (p d bindings)
	(let ((binding (find-binding p bindings)))
	     (if binding
	         (match (extract-value binding) d bindings)
		 (add-binding p d bindings))))

(defun match-pieces (p d bindings)
	(let ((result (match (first p) (first d) bindings)))
	     (if (eq 'fail result)
	         'fail
		 (match (rest p) (rest d) result))))

(defun elements-p (p d)
	(and (atom p) (atom d)))

(defun variable-p (p)
	(and (listp p) (eq '? (first p))))

(defun recursive-p (p d)
	(and (listp p) (listp d)))

(defun match (p d &optional bindings)
	(cond ((elements-p p d)
	       (match-atoms p d bindings))
	      ((variable-p p)
	       (match-variable p d bindings))
	      ((recursive-p p d)
	       (match-pieces p d bindings))
	      (t 'fail)))

(defun unify-atoms (p1 p2 bindings)
	(if  (eql p1 p2)
	     bindings
	     'fail))

(defun unify-pieces (p1 p2 bindings)
	(let ((result (unify (first p1) (first p2) bindings)))
	     (if (eq 'fail result)
	         'fail
		 (unify (rest p1) (rest p2) result))))

(defun insidep (variable expression bindings)
	(if (equal variable expression)
	    nil
	    (inside-or-equal-p variable expression bindings)))

(defun inside-or-equal-p (variable expression bindings)
   (cond ((equal variable expression) t)
   	 ((atom expression) nil)
	 ((eq '? (first expression))
	  (let ((binding (find-binding expression bindings)))
	       (when binding
	             (inside-or-equal-p variable (first expression) bindings))))
	 (t (or (inside-or-equal-p variable (first expression) bindings)
	 	(inside-or-equal-p variable (rest expression) bindings)))))

(defun unify-variable (p1 p2 bindings)
	(let ((binding (find-binding p1 bindings)))
	     (if binding
	         (unify (extract-value binding) p2 bindings)
		 (if (insidep p1 p2 bindings)
		     'fail
		     (add-binding p1 p2 bindings)))))

(defun unify (p1 p2 &optional bindings)
	(cond ((elements-p p1 p2)
	       (unify-atoms p1 p2 bindings))
	      ((variable-p p1)
	       (unify-variable p1 p2 bindings))
	      ((variable-p p2)
	       (unify-variable p2 p1 bindings))
	      ((recursive-p p1 p2)
	       (unify-pieces p1 p2 bindings))
	      (t 'fail)))










