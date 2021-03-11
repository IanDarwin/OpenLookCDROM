; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         movi-earth.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/movi-earth.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO TANGO:BITMAP_IMAGE_CLASS displaying movie of earth 
;		rotating. bitmap-movie is in bitmaps/earth-movie.lsp
; Author:       Niels P. Mayer
; Created:      Fri May  7 20:58:43 1993
; Modified:     Mon Jun  6 03:53:10 1994 (Niels Mayer) npm@indeed
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

(defvar *earth-movie* nil)
; (setq *earth-movie* nil)

(let (earth_tango_w earth_timage earth_to)

  (setq earth_tango_w
	(send TANGO:WIDGET_CLASS :new :managed
	      "earth_tango" rc_w
	      :XMN_HEIGHT 100
	      :XMN_WIDTH 100
	      :XMN_RESIZE_POLICY :resize_grow
	      ))

  (send earth_tango_w :begin_drawing)	;must call this after :realize
  (send earth_tango_w :SET_BGCOLOR "white")

  (if (null *earth-movie*)
      (require "bitmaps/movi-earth") ;sets *earth-movie*
    )

  (setq earth_timage
	(send TANGO:BITMAP_IMAGE_CLASS :new :show earth_tango_w
	      #C(0.2 0.2)
	      *earth-movie*
	      ))
  (setq *earth-movie* nil)		;allow the monstrosity to get garbage collected

  (xt_add_timeout
   0
   '(
     (TANGO:TX_PERFORM (send earth_timage :TX_shuffle 1))
     (setq earth_to (xt_add_timeout 100 TIMEOUT_OBJ))
     ))
  )
