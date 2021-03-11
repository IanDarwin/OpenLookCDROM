;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         evalenv.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/evalenv.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  evaluate an expression in the current lexical environment
; Author:       Tom Almy
; Created:      10/91
; Modified:     Mon Jun  6 03:02:36 1994 (Niels Mayer) npm@indeed
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
;; The EVAL function in the original XLISP evaluated in the current lexical
;; context. This was changed to evaluate in the NIL (global) context to
;; match Common Lisp. But this created a problem: how do you EVAL an 
;; expression in the current lexical context?
;;
;; The answer is you can use the evalhook facility. The evalhook function
;; will evaluate an expression using an environment given to it as an
;; argument. But then the problem is "how do you get the current 
;; environment?" Well the getenv macro, below obtains the environent by
;; using an *evalhook* form.
;;
;; The following two macros do the job. Insteading of executing (eval <expr>)
;; just execute (eval-env <expr>). If you want, you can dispense with the
;; macros and execute:
;;
;;(evalhook <expr> nil nil (let ((*evalhook* (lambda (x env) env)))
;;				(eval nil)))
;;
;; Tom Almy  10/91
;;

(defmacro getenv () 
	  '(let ((*evalhook* (lambda (x env) env)))
		(eval nil)))	; hook function evaluates by returning 
				; environment
				
(defmacro eval-env (arg)	; evaluate in current environment
	  `(evalhook ,arg nil nil (getenv)))


