; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         dragon.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/dragon.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Draw an Nth order Dragon Curve using turtle graphics
; Author:       Peter Ashwood-Smith, Tom Almy
; Created:      April 1986
; Modified:     Mon Jun  6 03:02:44 1994 (Niels Mayer) npm@indeed
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

;; DRAGON.L FOR PC-LISP V2.10
;; Modified for xlisp 2.1d (w. graphics extensions) by Tom Almy
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
;;     Draw an Nth order Dragon Curve requires Turtle.l routines to run.
;; Taken From Byte April 1986. Try (DragonCurve 16) then put on supper,
;; watch the news and come back in an hour and see the results. It takes 
;; about 1/2 hour on my machine so on a normal IBM-PC it should take about
;; an 1.5 hours.
;;
;;              Peter Ashwood-Smith.
;;              April 1986
;;
;;              P.S - This dragon is nicknamed "spot"

(unless (fboundp 'TurtleGoTo)(load 'turtle))

(defvar *StepSize* 1)

(defun Dragon(sign level)
       (if    (zerop level) 
       	      (TurtleForward *StepSize*)
              (progn
		(setq level (1- level))
		(TurtleRight (* 45 sign))
		(Dragon -1 level)
		(TurtleLeft (* 90 sign))
		(Dragon 1 level)
		(TurtleRight (* 45 sign))
              )
       )
)

(defun DragonCurve (n m)
       (setq *StepSize* m)                   ; *StepSize* is global variable
       (TurtleGraphicsUp)
       (TurtleCenter)
       (TurtleGoto 50 50)
       (TurtleRight 30)                          ; angle the serpent a bit
       (Dragon 1 n)
       (gc)
)

(print "Try (DragonCurve 14 1)")
