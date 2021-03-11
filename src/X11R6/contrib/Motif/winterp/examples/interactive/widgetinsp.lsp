;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         widgetinsp.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/widgetinsp.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Use 'GENERIC' to get at the insides of a WIDGETOBJ
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 00:32:12 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
;
; (C) Copyright 1994, Enterprise Integration Technologies Corporation.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Enterprise Integration Technologies
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;
; ENTERPRISE INTEGRATION TECHNOLOGIES DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "xlisp-2.1d/classes")
(require "xlisp-2.1d/pp")

(setq a (generic (send (get_moused_widget) :class)))
(aref a 0)				;the class
(pp (generic (aref a 0)))

(aref a 1)				;methods
(pp (generic (aref a 1)))

(aref a 2)				;ivars
(generic (aref a 2))

(aref a 3)				;cvars
(generic (aref a 3))

(aref a 4)				;cvals
(generic (aref a 4))

(aref a 5)				;superclass
(pp (generic (aref a 5)))

(aref a 6)				;ivar-cnt
(generic (aref a 6))

(aref a 7)				;cvar-cnt
(generic (aref a 7))

(aref a 8)				;PNAME

(nth 0 (generic (aref a 1)))		;get the first msg/method dotted-pr

(generic (cdr (nth 0 (generic (aref a 1))))) ;print the code for first method

(pp (generic (cdr (nth 0 (generic (aref a 1)))))) ;pretty print the method

