; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         queens.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xlisp-2.1d/RCS/queens.lsp,v 2.3 1994/06/06 15:00:34 npm Exp $
; Description:  Place n queens on a board, from Winston and Horn Ch. 11
; Author:       Winston&Horn, David Betz
; Created:      
; Modified:     Mon Jun  6 03:01:40 1994 (Niels Mayer) npm@indeed
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

;
; Place n queens on a board
;  See Winston and Horn Ch. 11
; 
; Usage:
;	(queens <n>)
;          where <n> is an integer -- the size of the board - try (queens 4)

; Do two queens threaten each other ?
(defun threat (i j a b)
  (or (eql i a)			;Same row
      (eql j b)			;Same column
      (eql (- i j) (- a b))	;One diag.
      (eql (+ i j) (+ a b))))	;the other diagonal

; Is position (n,m) on the board not safe for a queen ?
(defun conflict (n m board)
  (cond ((null board) nil)
	((threat n m (caar board) (cadar board)) t)
	(t (conflict n m (cdr board)))))


; Place queens on a board of size SIZE
(defun queens (size)
  (prog (board n m)
	(setq board nil)
	(setq n 1)			;Try the first row
	loop-n
	(setq m 1)			;Column 1
	loop-m
	(cond ((conflict n m board) (go un-do-m))) ;Check for conflict
	(setq board (cons (list n m) board))       ; Add queen to board
	(cond ((> (setq n (1+ n)) size)            ; Placed N queens ?
	       (print (reverse board))))           ; Print config
	(go loop-n)			           ; Next row which column?
	un-do-n
	(cond ((null board) (return 'Done)) 	   ; Tried all possibilities
	      (t (setq m (cadar board))		   ; No, Undo last queen placed
		 (setq n (caar board))
		 (setq board (cdr board))))

	un-do-m
	(cond ((> (setq m (1+ m)) size)          ; Go try next column
	       (go un-do-n))
	      (t (go loop-m)))))
