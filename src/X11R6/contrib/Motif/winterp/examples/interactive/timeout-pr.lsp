; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         timeout-pr.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/timeout-pr.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Print out the "code" associated with all pending WINTERP
;		timeouts
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 00:31:01 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       X11r6 contrib release
;
; Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
; WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
; WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
; 
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies,
; Hewlett-Packard Company, or Niels Mayer not be used in advertising or
; publicity pertaining to distribution of the software without specific,
; written prior permission. Enterprise Integration Technologies, Hewlett-Packard
; Company, and Niels Mayer makes no representations about the suitability of
; this software for any purpose.  It is provided "as is" without express or
; implied warranty.
; 
; ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
; DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
; INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
; RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
; CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "xlisp-2.1d/pp")

(xt_add_timeout 100000
		'(
		  (print "quack")
		  (print "quack")
		  ))

;; search for and print all active timeouts
(do 
 (
  (len (length *SAVED_OBJS*)
       )
  (i   0
       (1+ i)
       )
  (list '())
  )
 ((>= i len)
  )

 (setq list (aref *SAVED_OBJS* i))

 (if list
     (mapcar #'(lambda (x)
		 (if (equal (type-of x) 'TIMEOUT_OBJ)
		     (pp (generic (aref (generic x) 1)))
		   )
		 )
	     list)
   )

 )
