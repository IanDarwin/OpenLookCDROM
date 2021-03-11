; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-text.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-text.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Test for TANGO:TEXT_IMAGE_CLASS -- show bounding box around
;		text image.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 04:14:14 1994 (Niels Mayer) npm@indeed
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

(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	    "test-text"
	    :XMN_TITLE		"WINTERP: Xtango Text Image Class Test"
	    :XMN_ICON_NAME	"W:test-text"
	    ))

(setq tango_w
      (send TANGO:WIDGET_CLASS :new :managed
	    "tango" top_w
	    :XMN_HEIGHT		600
	    :XMN_WIDTH		600
	    ))

(send tango_w :add_event_handler (logior BUTTON_PRESS_MASK BUTTON_MOTION_MASK)
      '(EVHANDLER_XEVENT EVHANDLER_WIDGET)
      '(
	(format T "~A\n"
		(send EVHANDLER_WIDGET :get_event_coord EVHANDLER_XEVENT))
	))

(progn
  (send top_w :realize)
  (send tango_w :forced_expose_update)	;wait until exposed to ensure windows created for :begin_drawing call
  (send tango_w :begin_drawing)		;must call this after :realize
  )

;(setq f1 "-sun-open look glyph-*-*-*-*-5-*-*-*-*-*-*-*")
;(setq f1 "-sun-open look glyph-----14-140-75-75-p-128-sunolglyph-1")
;(setq f1 "-*-*-*-i-*--30-*-*-*-p-*-iso8859-1")
;(setq f1 "-*-*-*-r-*--30-*-*-*-p-*-iso8859-1")
;(setq f1 "-adobe-times-bold-i-normal--24-240-75-75-p-128-hp-roman8")
;(setq f1 "-adobe-new century schoolbook-medium-i-normal--34-240-100-100-p-182-iso8859-1")
;(setq f1 "fgi1-25")
;(setq f1 "krivo")
(setq f1 "vri-25")
;(setq f1 "hp8.10x20b")
;(setq str1 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=+|><,./?~!@#$%^&*()")
;(setq str1 "dogmajoggerr")
;(setq str1 "IMAGINE VISUALS")
(setq str1 "IMAGINE VISUALIZATION")

;(setq f2 "-sun-open look glyph-*-*-*-*-5-*-*-*-*-*-*-*")
;(setq f2 "-sun-open look glyph-----14-140-75-75-p-128-sunolglyph-1")
;(setq f2 "-*-*-*-i-*--36-*-*-*-p-*-iso8859-1")
;(setq f2 "-*-*-*-r-*--36-*-*-*-p-*-iso8859-1")
;(setq f2 "-adobe-new century schoolbook-bold-i-normal--18-180-75-75-p-111-hp-roman8")
;(setq f2 "-adobe-times-bold-i-normal--24-240-75-75-p-128-hp-roman8")
;(setq f2 "fg-25")
;(setq f2 "fgi-20")
(setq f2 "hp8.10x20b")
;(setq str2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_=+|><,./?~!@#$%^&*()")
;(setq str2 "0123456789-_=+|><,./?~!@#$%^&*()")
;(setq str2 "gggggXXXXIIIii")
;(setq str2 "dogmajoggerr")
;(setq str2 "IMAGINE VISUALS")
(setq str2 "IMAGINE VISUALIZATION")

(setq t1
      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	    #C(0.3 0.4)			;location_coord
	    str1			;text_string
	    TANGO_COLOR_BLACK		;tango_color
	    f1				;font
	    ))
(setq t2
      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
	    #C(0.3 0.5) :ctr		;location_coord
	    str2			;text_string
	    TANGO_COLOR_BLACK		;tango_color
	    f2				;font
	    ))

(setq nw1 (send t1 :image_loc :NW))
(setq n1 (send t1 :image_loc  :N))
(setq ne1 (send t1 :image_loc :NE))
(setq e1 (send t1 :image_loc  :E))
(setq se1 (send t1 :image_loc :SE))
(setq s1 (send t1 :image_loc  :S))
(setq sw1 (send t1 :image_loc :SW))
(setq w1 (send t1 :image_loc  :W))
(setq c1 (send t1 :image_loc  :CTR))
(setq nw2 (send t2 :image_loc :NW))
(setq n2 (send t2 :image_loc  :N))
(setq ne2 (send t2 :image_loc :NE))
(setq e2 (send t2 :image_loc  :E))
(setq se2 (send t2 :image_loc :SE))
(setq s2 (send t2 :image_loc  :S))
(setq sw2 (send t2 :image_loc :SW))
(setq w2 (send t2 :image_loc  :W))
(setq c2 (send t2 :image_loc  :CTR))
                            
(setq x1 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w nw1 0.003 TANGO_COLOR_RED 1.0000))
(setq x2 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w n1  0.003 TANGO_COLOR_RED 1.0000))
(setq x3 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w ne1 0.003 TANGO_COLOR_RED 1.0000))
(setq x4 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w e1  0.003 TANGO_COLOR_RED 1.0000))
(setq x5 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w se1 0.003 TANGO_COLOR_RED 1.0000))
(setq x6 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w s1  0.003 TANGO_COLOR_RED 1.0000))
(setq x7 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w sw1 0.003 TANGO_COLOR_RED 1.0000))
(setq x8 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w w1  0.003 TANGO_COLOR_RED 1.0000))
(setq x9 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w c1  0.003 TANGO_COLOR_RED 1.0000))

(setq y1 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w nw2 0.003 TANGO_COLOR_RED 1.0000))
(setq y2 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w n2  0.003 TANGO_COLOR_RED 1.0000))
(setq y3 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w ne2 0.003 TANGO_COLOR_RED 1.0000))
(setq y4 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w e2  0.003 TANGO_COLOR_RED 1.0000))
(setq y5 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w se2 0.003 TANGO_COLOR_RED 1.0000))
(setq y6 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w s2  0.003 TANGO_COLOR_RED 1.0000))
(setq y7 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w sw2 0.003 TANGO_COLOR_RED 1.0000))
(setq y8 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w w2  0.003 TANGO_COLOR_RED 1.0000))
(setq y9 (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w c2  0.003 TANGO_COLOR_RED 1.0000))
 
(send t1 :tx_raise :perform)
(send t2 :tx_raise :perform)
(send x9 :tx_raise :perform)
(send y9 :tx_raise :perform)



