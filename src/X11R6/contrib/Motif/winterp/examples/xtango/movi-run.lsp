; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         movi-run.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/movi-run.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO TANGO:BITMAP_IMAGE_CLASS displaying movie of a little
;		man running. bitmap-movie is in bitmaps/movi-run.lsp
; Author:       Niels P. Mayer
; Created:      Fri May  7 20:58:43 1993
; Modified:     Mon Jun  6 03:55:32 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (load "rc-shell")

(defvar *run-movie* nil)
;;; (setq *run-movie* nil)

(let (run_tango_w run_timage)

(setq run_tango_w
      (send TANGO:WIDGET_CLASS :new :managed
	    "run_tango" rc_w
	    :XMN_HEIGHT 38
	    :XMN_WIDTH 280
	    :XMN_RESIZE_POLICY :resize_grow
	    ))

(send run_tango_w :begin_drawing)	;must call this after :realize
(send run_tango_w :SET_BGCOLOR "black")

(if (null *run-movie*)
    (load "bitmaps/movi-run")	;sets *run-movie*
  )

(setq run_timage
	(send TANGO:BITMAP_IMAGE_CLASS :new :show run_tango_w
	      #C(0.0 0.0)
	      *run-movie*
	      ))
(setq *run-movie* nil)			;allow the monstrosity to get garbage collected

(let
    ((tx_back_to_0
      (send run_timage :TX_MOVE (TANGO:PATH_CREATE #(-0.9) #(0.0))))
     (tx_increment
      (send run_timage :TX_MOVE (TANGO:PATH_CREATE #(0.05) #(0.0))))
     (tx_next_bitmap
      (send run_timage :TX_SHUFFLE 1))
     (x 0.00)
     )
  (xt_add_timeout
   0
   '(
     (TANGO:TX_PERFORM tx_next_bitmap)
     (cond
      ((>= x 0.9)
       (TANGO:TX_PERFORM tx_back_to_0)
       (setq x 0.0))
      (t
       (TANGO:TX_PERFORM tx_increment)
       (setq x (+ x 0.05)))
      )
     (xt_add_timeout 100 TIMEOUT_OBJ)
     ))
  )
)
