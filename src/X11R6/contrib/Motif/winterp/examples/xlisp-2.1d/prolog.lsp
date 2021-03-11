; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         prolog.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/prolog.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Tiny Prolog interpreter
; Author:       Ken Kahn, David Betz
; Created:      
; Modified:     Mon Jun  6 03:01:47 1994 (Niels Mayer) npm@indeed
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

;; The following is a tiny Prolog interpreter in MacLisp
;; written by Ken Kahn and modified for XLISP by David Betz.
;; It was inspired by other tiny Lisp-based Prologs of
;; Par Emanuelson and Martin Nilsson.
;; There are no side-effects anywhere in the implementation.
;; Though it is VERY slow of course.

(defun prolog (database &aux goal)
       (do () ((not (progn (princ "Query?") (setq goal (read)))))
              (prove (list (rename-variables goal '(0)))
                     '((bottom-of-environment))
                     database
                     1)))

;; prove - proves the conjunction of the list-of-goals
;;         in the current environment

(defun prove (list-of-goals environment database level)
      (cond ((null list-of-goals) ;; succeeded since there are no goals
             (print-bindings environment environment)
             (not (y-or-n-p "More?")))
            (t (try-each database database
                         (cdr list-of-goals) (car list-of-goals)
                         environment level))))

(defun try-each (database-left database goals-left goal environment level 
                 &aux assertion new-enviroment)
       (cond ((null database-left) nil) ;; fail since nothing left in database
             (t (setq assertion
                      (rename-variables (car database-left)
                                        (list level)))
                (setq new-environment
                      (unify goal (car assertion) environment))
                (cond ((null new-environment) ;; failed to unify
                       (try-each (cdr database-left) database
                                 goals-left goal
                                 environment level))
                      ((prove (append (cdr assertion) goals-left)
                              new-environment
                              database
                              (+ 1 level)))
                      (t (try-each (cdr database-left) database
                                   goals-left goal
                                   environment level))))))

(defun unify (x y environment &aux new-environment)
       (setq x (value x environment))
       (setq y (value y environment))
       (cond ((variable-p x) (cons (list x y) environment))
             ((variable-p y) (cons (list y x) environment))
             ((or (atom x) (atom y))
                  (cond ((equal x y) environment)
    	                (t nil)))
             (t (setq new-environment (unify (car x) (car y) environment))
                (cond (new-environment (unify (cdr x) (cdr y) new-environment))
    		      (t nil)))))

(defun value (x environment &aux binding)
       (cond ((variable-p x)
              (setq binding (assoc x environment :test #'equal))
              (cond ((null binding) x)
                    (t (value (cadr binding) environment))))
             (t x)))

(defun variable-p (x)
       (and x (listp x) (eq (car x) '?)))

(defun rename-variables (term list-of-level)
       (cond ((variable-p term) (append term list-of-level))
             ((atom term) term)
             (t (cons (rename-variables (car term) list-of-level)
                      (rename-variables (cdr term) list-of-level)))))

(defun print-bindings (environment-left environment)
       (cond ((cdr environment-left)
              (cond ((= 0 (nth 2 (caar environment-left)))
                     (prin1 (cadr (caar environment-left)))
                     (princ " = ")
                     (print (value (caar environment-left) environment))))
              (print-bindings (cdr environment-left) environment))))

;; a sample database:
(setq db '(((father madelyn ernest))
           ((mother madelyn virginia))
	   ((father david arnold))
	   ((mother david pauline))
	   ((father rachel david))
	   ((mother rachel madelyn))
           ((grandparent (? grandparent) (? grandchild))
            (parent (? grandparent) (? parent))
            (parent (? parent) (? grandchild)))
           ((parent (? parent) (? child))
            (mother (? parent) (? child)))
           ((parent (? parent) (? child))
            (father (? parent) (? child)))))

;; the following are utilities
(defun y-or-n-p (prompt)
       (princ prompt)
       (eq (read) 'y))

;; start things going
(prolog db)
