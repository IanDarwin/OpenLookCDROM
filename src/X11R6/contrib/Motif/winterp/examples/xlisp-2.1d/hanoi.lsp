; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         hanoi.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/hanoi.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  towers of hanoi algorithm/game/problem
; Author:       Tom Almy and/or David Betz
; Created:      
; Modified:     Mon Jun  6 03:02:15 1994 (Niels Mayer) npm@indeed
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

; Good ol towers of hanoi
;
; Usage:
;      (hanoi <n>)
;          <n> - an integer the number of discs

(defun hanoi(n)
  ( transfer 'A 'B 'C n ))

(defun print-move ( from to )
  (princ "Move Disk From ")
  (princ from)
  (princ " To ")
  (princ to)
  (princ "\n")
  nil)


(defun transfer ( from to via n )
  (cond ((equal n 1) (print-move from to ))
	(t (transfer from via to (- n 1))
	   (print-move from to)
	   (transfer via to from (- n 1)))))
