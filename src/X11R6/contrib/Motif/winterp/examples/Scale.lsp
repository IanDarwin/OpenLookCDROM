;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         scale-widg.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/Scale.lsp,v 2.4 1994/06/06 14:43:25 npm Exp $
; Description:  shows use of XM_SCALE_WIDGET_CLASS. Just load this file to see
;		example.
; Author:       Niels Mayer
; Created:      Sat Jul  7 21:43:32 1990
; Modified:     Sun Jun  5 18:11:35 1994 (Niels Mayer) npm@indeed
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

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "sclshl"
	    :XMN_TITLE		"WINTERP: XmScale Test"
	    :XMN_ICON_NAME	"W:Scale"
	    ))

(setq rc_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "rc" toplevel_w
	    :XMN_ORIENTATION :HORIZONTAL
	    :XMN_PACKING :PACK_COLUMN
	    :XMN_ENTRY_ALIGNMENT :ALIGNMENT_CENTER
	    ))

(setq scale0_w 
      (send XM_SCALE_WIDGET_CLASS :new :managed "scale" rc_w
	    :XMN_TITLE_STRING "Utterness\nCoefficient"
	    :XMN_SENSITIVE t
	    ))
(send XM_LABEL_WIDGET_CLASS :new :managed "100" scale0_w
      )
(send XM_LABEL_WIDGET_CLASS :new :managed "50" scale0_w
      )
(send XM_LABEL_WIDGET_CLASS :new :managed "0" scale0_w
      )

(setq scale1_w 
      (send XM_SCALE_WIDGET_CLASS :new :managed "scale" rc_w
	    :XMN_TITLE_STRING "lameness\nper\nsecond^2"
	    :XMN_SENSITIVE t
	    :XMN_SHOW_VALUE t
	    ))

(send toplevel_w :realize)

(send scale0_w :set_value 50)
(send scale1_w :get_value)





