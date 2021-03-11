; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         movi-eyel.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/movi-eyel.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO TANGO:BITMAP_IMAGE_CLASS displaying movie of the left
;		of a walking eye-man...
;		bitmap-movie is in bitmaps/movi-eyel.lsp
; Author:       Niels P. Mayer
; Created:      Fri May  7 20:58:43 1993
; Modified:     Mon Jun  6 03:54:08 1994 (Niels Mayer) npm@indeed
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

; (require "rc-shell")

(defvar *eyel-movie* nil)
; (setq *eyel-movie* nil)

(let (eyel_tango_w eyel_timage eyel_to)

  (setq eyel_tango_w
	(send TANGO:WIDGET_CLASS :new :managed
	      "eyel_tango" rc_w
	      :XMN_HEIGHT 40
	      :XMN_WIDTH 60
	      :XMN_RESIZE_POLICY :resize_grow
	      ))

  (send eyel_tango_w :begin_drawing)	;must call this after :realize
  (send eyel_tango_w :SET_BGCOLOR "white")

  (if (null *eyel-movie*)
      (require "bitmaps/movi-eyel")	;sets *eyel-movie*
    )

  (setq eyel_timage
	(send TANGO:BITMAP_IMAGE_CLASS :new :show eyel_tango_w
	      #C(0.0 0.0)
	      *eyel-movie*
	      ))
  (setq *eyel-movie* nil)		;allow the monstrosity to get garbage collected

  (send eyel_timage :TAP_SHOW)

  (xt_add_timeout
   0
   '(
     (TANGO:TX_PERFORM (send eyel_timage :TX_shuffle 1))
     (setq eyel_to (xt_add_timeout 100 TIMEOUT_OBJ))
     ))
  )
