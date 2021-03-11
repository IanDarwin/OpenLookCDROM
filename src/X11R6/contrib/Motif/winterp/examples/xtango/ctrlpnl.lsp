; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xtango-ctrlpnl.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/ctrlpnl.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Pile of GUIs to let me easily play with and test WINTERP's Xtango
;		functionality.
; Author:       Niels P. Mayer
; Created:      Thu Mar 18 19:18:25 1993
; Modified:     Mon Jun  6 03:24:48 1994 (Niels Mayer) npm@indeed
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

(let* (toplevel_w
       rc_w
       set-tango_w-pb_w set-im1-pb_w set-im2-pb_w
       debug-toggle_w
       delay-rc_w delay-la_w delay-ed_w
       mono-pattern-rep-opt_w
       pan-rc_w pan-la_w pan-up-btn_w pan-down-btn_w pan-left-btn_w pan-right-btn_w
       zoom-rc_w zoom-la_w zoom-in-btn_w zoom-out-btn_w
       )
  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "xtango-controls"
	      :XMN_TITLE		"WINTERP: Xtango Operations"
	      :XMN_ICON_NAME		"W:ctrlpnl"
	      ))
  (setq rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" toplevel_w
	      :XMN_PACKING		:pack_column
	      :XMN_NUM_COLUMNS		2
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:vertical
	      ))
  (setq set-tango_w-pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "set-tango_w-pb" rc_w
	      :XMN_LABEL_STRING		"Set val of TANGO_W to pick'd widget"
	      ))
  (setq set-im1-pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "set-im1-pb" rc_w
	      :XMN_LABEL_STRING		"Set val of IM1 to pick'd image"
	      ))
  (setq set-im2-pb_w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "set-im2-pb_w" rc_w
	      :XMN_LABEL_STRING		"Set val of IM2 to pick'd image"
	      ))
  (setq debug-toggle_w
	(send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed
	      "debug-toggle" rc_w
	      :XMN_LABEL_STRING		"Print Xtango Debug Msgs"
	      ))
  (setq delay-rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "delay_rc" rc_w
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:horizontal
	      ))
  (setq delay-la_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "delay-la" delay-rc_w
	      :XMN_LABEL_STRING		"Set delay microseconds:"
	      ))
  (setq delay-ed_w
	(send XM_TEXT_WIDGET_CLASS :new :managed
	      "delay-ed" delay-rc_w
	      :XMN_EDIT_MODE		:single_line_edit
	      ))
  (setq mono-pattern-rep-opt_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_option_menu
	      "mono-pattern-rep-opt" rc_w
	      :XMN_OPTION_LABEL		"Patterns represent:\n(monochrome only)"
	      :XMN_OPTION_MNEMONIC	#\P
	      :XMN_BUTTON_COUNT		2
	      :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON)
	      :XMN_BUTTONS		#("Colors" "Fills")
	      :XMN_BUTTON_MNEMONICS	#(#\C #\F)
	      :XMN_BUTTON_SET		0
	      ))
  (setq pan-rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "pan-rc" rc_w
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:horizontal
	      ))
  (setq pan-la_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "pan-la" pan-rc_w
	      :XMN_LABEL_STRING		"Pan:"
	      ))
  (setq pan-up-btn_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "pan-up-btn" pan-rc_w
	      :XMN_ARROW_DIRECTION	:arrow_up
	      ))
  (setq pan-down-btn_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "pan-down-btn" pan-rc_w
	      :XMN_ARROW_DIRECTION	:arrow_down
	      ))
  (setq pan-left-btn_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "pan-left-btn" pan-rc_w
	      :XMN_ARROW_DIRECTION	:arrow_left
	      ))
  (setq pan-right-btn_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "pan-right-btn" pan-rc_w
	      :XMN_ARROW_DIRECTION	:arrow_right
	      ))
  (setq zoom-rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "zoom-rc" rc_w
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_ORIENTATION		:horizontal
	      ))
  (setq zoom-la_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "zoom-la" zoom-rc_w
	      :XMN_LABEL_STRING		"Zoom:"
	      ))
  (setq zoom-in-btn_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "zoom-in-btn" zoom-rc_w
	      :XMN_ARROW_DIRECTION	:arrow_up
	      ))
  (setq zoom-out-btn_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "zoom-out-btn" zoom-rc_w
	      :XMN_ARROW_DIRECTION	:arrow_down
	      ))
  (send toplevel_w :realize)
  (send set-tango_w-pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (setq tango_w (get_moused_widget))
	  (format T "Global variable TANGO_W set! ")
	  (send tango_w :show)
	  ))
  (send set-im1-pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (let ( (image (send tango_w :input_image)) )
	    (if image
		(progn
		  (TANGO:TX_PERFORM (send image :TAP_FLASH 1))
		  (setq im1 image)
		  (format T "Global variable IM1 set! ")
		  (send im1 :show)
		  )
	      (error ":INPUT_IMAGE failed")
	      )
	    )
	  ))
  (send set-im2-pb_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (let ( (image (send tango_w :input_image)) )
	    (if image
		(progn
		  (TANGO:TX_PERFORM (send image :TAP_FLASH 1))
		  (setq im2 image)
		  (format T "Global variable IM2 set! ")
		  (send im2 :show)
		  )
	      (error ":INPUT_IMAGE failed")
	      )
	    )
	  ))
  (send debug-toggle_w :add_callback :xmn_value_changed_callback
	'(callback_set)
	'(
	  (send tango_w :SET_DEBUG callback_set)
	  ))
  (send delay-ed_w :set_callback :XMN_ACTIVATE_CALLBACK
	'(CALLBACK_WIDGET)
	'(
	  (send tango_w :SET_DELAY
		(read (make-string-input-stream (send CALLBACK_WIDGET :get_string))))
	  ))
  (send mono-pattern-rep-opt_w :add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET)
	'(
	  ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	  ;; where <#> is 0 ... (button-count-1).
	  ;; we use 'read' to return the FIXNUM <#> after truncating the
	  ;; 7 chars "button_" from the front of the string.
	  (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	    (0 (send tango_w :MONO_PATTERN_REPRESENTATION :colors))
	    (1 (send tango_w :MONO_PATTERN_REPRESENTATION :fills))
	    (T (error "invalid option")))
	  ))
  (send pan-up-btn_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send tango_w :PAN :up 0.1)	  
	  ))
  (send pan-down-btn_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send tango_w :PAN :down 0.1)	  	  
	  ))
  (send pan-left-btn_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send tango_w :PAN :left 0.1)	  	  
	  ))
  (send pan-right-btn_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send tango_w :PAN :right 0.1)	  	  
	  ))
  (send zoom-in-btn_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send tango_w :ZOOM :in)
	  ))
  (send zoom-out-btn_w :set_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send tango_w :ZOOM :out)
	  ))
  )
