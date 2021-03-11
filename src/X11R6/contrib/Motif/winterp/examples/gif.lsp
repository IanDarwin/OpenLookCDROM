; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         gif.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/gif.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Demo of displaying GIF in a widget; GIF retrieved via
;		GIF_TO_PIXMAP.
; Author:       Niels P. Mayer
; Created:      Sun Jun  5 18:42:26 1994
; Modified:     Sun Jun  5 18:43:23 1994 (Niels Mayer) npm@indeed
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

;;
;; Display a GIF in a label widget
;;
(let (toplevel_w scrl_w gif_w)

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "gif-shell"
	    ))

(setq scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	    "sc" toplevel_w
	    :XMN_SCROLLING_POLICY	:automatic
	    ))

(setq gif_w
      (send XM_LABEL_GADGET_CLASS :new :managed
	    "gif" scrl_w
	    :XMN_LABEL_TYPE	:pixmap
	    :XMN_LABEL_PIXMAP	(gif_to_pixmap
				 "/usr/local/winterp/examples/xtango/fluid2.gif"
				 :verbose)
	    ))

(send toplevel_w :realize)
)

