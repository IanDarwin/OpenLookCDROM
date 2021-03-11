;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         pixmaps.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/pixmaps.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Play around with pixmaps. These are just random individual forms
;               I eval'd to play around and test pixmaps, pixmap garbage
;               collection, image cacheing, etc.
;               Many of the pixmaps mentioned in this file do not exist on your
;               machine.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:44:36 1989
; Modified:     Mon Jun  6 00:28:27 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*

(require "rc-shell")

(progn
  (setq b1
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "background"  "white" "DarkSlateGrey")))
  (setq b2
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "25_foreground" "white" "DarkSlateGrey")))
  (setq b3
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "50_foreground"  "white" "DarkSlateGrey")))
  (setq b4
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "75_foreground"  "white" "DarkSlateGrey")))
  (setq b5
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "vertical" "white" "DarkSlateGrey")))
  (setq b6
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "horizontal"  "white" "DarkSlateGrey")))
  (setq b7
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "slant_right"  "white" "DarkSlateGrey")))
  (setq b8
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "slant_left" "white" "DarkSlateGrey")))
  (setq b9
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "menu_cascade" "white" "DarkSlateGrey")))
  (setq b10
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "menu_checkmark"  "white" "DarkSlateGrey")))
  )

(progn
  (send b1 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b2 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b3 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b4 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b5 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b6 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b7 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b8 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b9 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  (send b10 :set_values
	:XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "white" "DarkSlateGrey"))
  )

(progn
  (setq b1
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "background"))
  (setq b2
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "25_foreground" ))
  (setq b3
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "50_foreground"  ))
  (setq b4
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "75_foreground"  ))
  (setq b5
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "vertical" ))
  (setq b6
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "horizontal"  ))
  (setq b7
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "slant_right"  ))
  (setq b8
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "slant_left" ))
  (setq b9
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "menu_cascade" ))
  (setq b10
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP "menu_checkmark"  ))
  )





(setq pb_w (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman") "red" "blue")))
(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs") "red" "blue"))
(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagup") "red" "blue"))
(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagdown") "red" "blue"))
(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16") "red" "blue"))
(send pb_w :set_values 
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman"))






(setq pb_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
  	    )
      )
(send pb_w :set_values 
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman")
      )
(gc)





(setq pb_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
	    :XMN_LABEL_TYPE :PIXMAP
	    :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16")
;	    :XMN_ARM_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagup")
	    )
      )

(setq pb_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
	    :XMN_LABEL_TYPE :PIXMAP
	    :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagdown")
	    )
      )

(send pb_w :set_values 
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman")
      )

(setq image1_pixmap (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman")
				 "white" "DarkSlateGrey"))

(setq pb_w (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
		 :XMN_LABEL_TYPE :PIXMAP
		 :XMN_LABEL_PIXMAP (xm_get_pixmap
				    (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs")
				    "white" "DarkSlateGrey")
		 ))

(send pb_w :set_values 
      :XMN_LABEL_PIXMAP (xm_get_pixmap
			 (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16")
			 "white" "DarkSlateGrey")
      )

(gc)

; (setq image1_pixmap nil)
(send pb_w :set_values 
      :XMN_LABEL_TYPE :PIXMAP
      :XMN_LABEL_PIXMAP image1_pixmap
      )


(setq pb_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
	    :XMN_LABEL_TYPE :PIXMAP
	    :XMN_LABEL_PIXMAP image1_pixmap
	    )
      )
(setq image1_pixmap (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagup")
				 "blue" "red"))

(gc)

(setq pb_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "foo" rc_w
	    :XMN_LABEL_TYPE :PIXMAP
	    :XMN_LABEL_PIXMAP image1_pixmap
;	    :XMN_ARM_PIXMAP   image2_pixmap
	    )
      )


(setq rc (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		       "rc"
		       (send (send TOP_LEVEL_SHELL_WIDGET_CLASS :new :XMN_GEOMETRY "500x500") :realize)
		       :XMN_ORIENTATION :vertical
		       :XMN_PACKING :pack_tight
		       :XMN_ENTRY_ALIGNMENT :alignment_center
		       :XMN_FOREGROUND "Black"
		       :XMN_BACKGROUND "LightGray"))

(setq to_w 
      (send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed "foo" rc
	    :XMN_LABEL_TYPE :PIXMAP
	    :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs")
	    :XMN_SELECT_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagup")
	    :XMN_SELECT_INSENSITIVE_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagdown")
	    )
      )

(setq to_w 
      (send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed "foo" rc
	    :XMN_LABEL_TYPE :PIXMAP
	    :XMN_LABEL_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman")  "white" "DarkSlateGrey")
	    :XMN_SELECT_PIXMAP (xm_get_pixmap (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs")  "red" "blue")
	    )
      )





(send to_w :set_values 
	    :XMN_LABEL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo64")
	    :XMN_SELECT_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16")
	    )

(setq mbox_w
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :message_dialog top_w
	    :XMN_SYMBOL_PIXMAP (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman")
	    ))



(xm_install_image
 (setq image0_xi (xm_get_ximage_from_file (concatenate 'string *X11-BITMAPS-DIRECTORY* "woman")))
 "image0")

(xm_install_image
 (setq image1_xi (xm_get_ximage_from_file (concatenate 'string *X11-BITMAPS-DIRECTORY* "wingdogs")))
 "image1")

(xm_install_image
 (setq image2_xi (xm_get_ximage_from_file (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagup")))
 "image2")

(xm_install_image
 (setq image3_xi (xm_get_ximage_from_file (concatenate 'string *X11-BITMAPS-DIRECTORY* "flagdown")))
 "image3")

(xm_install_image
 (setq image4_xi (xm_get_ximage_from_file (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16")))
 "image4")

(format T "~A, ~A, ~A, ~A, ~A\n" image0_xi image1_xi image2_xi image3_xi image4_xi)

(progn
  (setq b1
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "image0"  "white" "DarkSlateGrey")))
  (setq b2
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "image1" "white" "DarkSlateGrey")))
  (setq b3
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "image2"  "white" "DarkSlateGrey")))
  (setq b4
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "image3"  "white" "DarkSlateGrey")))
  (setq b5
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap "image4" "white" "DarkSlateGrey")))
  )

(xm_uninstall_image image0_xi)
(xm_uninstall_image image1_xi)
(xm_uninstall_image image2_xi)
(xm_uninstall_image image3_xi)
(xm_uninstall_image image4_xi)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; try out method :update_display in callback.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq pb_w (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rc_w
		 :XMN_LABEL_TYPE :PIXMAP
		 :XMN_LABEL_PIXMAP (xm_get_pixmap
				    (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16")
				    "white" "green")
		 ))
(send pb_w :set_callback :XMN_ACTIVATE_CALLBACK '(callback_widget)
      '(
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-1")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-2")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-3")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-4")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-5")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-6")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values 
	      :XMN_LABEL_TYPE :STRING
	      :XMN_LABEL_STRING "WAITING-7")
	(send callback_widget :update_display)
	(system "sleep 1")
	(send callback_widget :set_values
	      :XMN_LABEL_TYPE :PIXMAP
	      :XMN_LABEL_PIXMAP (xm_get_pixmap
				 (concatenate 'string *X11-BITMAPS-DIRECTORY* "xlogo16")
				 "white" "DarkSlateGrey"))
	))
