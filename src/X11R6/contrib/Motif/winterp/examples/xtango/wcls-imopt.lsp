; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wcls-imopt.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/wcls-imopt.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS -- a dialog box
;		for inputting image parameters for image creation, e.g.
;		line width, fill values, arrow directions, etc.
; Author:       Niels P. Mayer
; Created:      Mon Jun  6 04:26:43 1994
; Modified:     Mon Jun  6 04:31:51 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS -- a subclass of XM_FORM_WIDGET_CLASS/:dialog
;;;
(setq XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS ;name of the new subclass
      (send Class :new
            '(				;inst variables for subclass
	      fill-slider_w
	      line-style-slider_w
	      line-width-slider_w
	      line-arrow-opt_w
	      )
            '()                         ;no class variables for subclass
            XM_FORM_WIDGET_CLASS ;name of the superclass
	    )) 

;;;
;;; Override instance initializer (method :isnew).
;;;
(send XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS :answer :isnew
      '(managed_k widget_name widget_parent
		  tango_w		;NOTE special extra widget creation arg, a tango:widget_class instance
		  help_w		;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
		  &rest args)
      '(
	;; create 'self', an instance of XM_FORM_WIDGET_CLASS/:dialog
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k :dialog widget_name widget_parent
	       :XMN_DELETE_RESPONSE	:unmap
	       :XMN_AUTO_UNMANAGE	nil
	       :XMN_DIALOG_TITLE	"Image Creation Options"
	       :XMN_FRACTION_BASE	5
	       args
	       )
	(let (sep_w close_btn_w help_btn_w)
	  (setq fill-slider_w
		(send XM_SCALE_WIDGET_CLASS :new :managed
		      "fill-slider" self
		      :XMN_TITLE_STRING		"Fill Value"
		      :XMN_SENSITIVE		t
		      :XMN_SHOW_VALUE		t
		      :XMN_ORIENTATION		:horizontal
		      :XMN_MAXIMUM		100
		      :XMN_DECIMAL_POINTS	2
		      :XMN_TOP_ATTACHMENT	:attach_form
		      :XMN_LEFT_ATTACHMENT	:attach_form
		      :XMN_RIGHT_ATTACHMENT	:attach_form
		      :XMN_TOP_OFFSET		5
		      :XMN_LEFT_OFFSET		10
		      :XMN_RIGHT_OFFSET		10
		      ))
	  (setq line-style-slider_w
		(send XM_SCALE_WIDGET_CLASS :new :managed
		      "line-style-slider" self
		      :XMN_TITLE_STRING		"Line Style"
		      :XMN_SENSITIVE		t
		      :XMN_SHOW_VALUE		t
		      :XMN_ORIENTATION		:horizontal
		      :XMN_MAXIMUM		100
		      :XMN_DECIMAL_POINTS	2
		      :XMN_TOP_ATTACHMENT	:attach_widget
		      :XMN_TOP_WIDGET		fill-slider_w
		      :XMN_LEFT_ATTACHMENT	:attach_form
		      :XMN_RIGHT_ATTACHMENT	:attach_form
		      :XMN_LEFT_OFFSET		10
		      :XMN_RIGHT_OFFSET		10
		      ))
	  (setq line-width-slider_w
		(send XM_SCALE_WIDGET_CLASS :new :managed
		      "fill-slider" self
		      :XMN_TITLE_STRING		"Line Width"
		      :XMN_SENSITIVE		t
		      :XMN_SHOW_VALUE		t
		      :XMN_ORIENTATION		:horizontal
		      :XMN_MAXIMUM		100
		      :XMN_DECIMAL_POINTS	2
		      :XMN_TOP_ATTACHMENT	:attach_widget
		      :XMN_TOP_WIDGET		line-style-slider_w
		      :XMN_LEFT_ATTACHMENT	:attach_form
		      :XMN_RIGHT_ATTACHMENT	:attach_form
		      :XMN_LEFT_OFFSET		10
		      :XMN_RIGHT_OFFSET		10
		      ))
	  (setq line-arrow-opt_w
		(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_option_menu
		      "arrow-opt" self
		      :XMN_OPTION_LABEL		"Arrow Kind"
		      :XMN_OPTION_MNEMONIC	#\A
		      :XMN_BUTTON_COUNT		4
		      :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
		      :XMN_BUTTONS		#("None" "Forward" "Backward" "Bidirectional")
		      :XMN_BUTTON_MNEMONICS	#(#\N #\F #\B #\i)
		      :XMN_BUTTON_SET		0
		      :XMN_TOP_ATTACHMENT	:attach_widget
		      :XMN_TOP_WIDGET		line-width-slider_w
		      :XMN_LEFT_ATTACHMENT	:attach_form
		      :XMN_RIGHT_ATTACHMENT	:attach_form
		      :XMN_LEFT_OFFSET		10
		      :XMN_RIGHT_OFFSET		10
		      ))
	  (setq sep_w
		(send XM_SEPARATOR_GADGET_CLASS :new :managed 
		      "sep" self
		      :XMN_TOP_ATTACHMENT	:attach_widget
		      :XMN_TOP_WIDGET		line-arrow-opt_w
		      :XMN_LEFT_ATTACHMENT	:attach_form
		      :XMN_RIGHT_ATTACHMENT	:attach_form
		      :XMN_TOP_OFFSET		5
		      ))
	  (setq close_btn_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "close" self
		      :XMN_LABEL_STRING		"Close"
		      :XMN_TOP_ATTACHMENT	:attach_widget
		      :XMN_TOP_WIDGET		sep_w
		      :XMN_BOTTOM_ATTACHMENT	:attach_form
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	1
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	2
		      :XMN_TOP_OFFSET		5
		      :XMN_BOTTOM_OFFSET	5
		      ))
	  (setq help_btn_w
		(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		      "help" self
		      :XMN_LABEL_STRING		"Help"
		      :XMN_TOP_ATTACHMENT	:attach_widget
		      :XMN_TOP_WIDGET		sep_w
		      :XMN_BOTTOM_ATTACHMENT	:attach_form
		      :XMN_LEFT_ATTACHMENT	:attach_position
		      :XMN_LEFT_POSITION	3
		      :XMN_RIGHT_ATTACHMENT	:attach_position
		      :XMN_RIGHT_POSITION	4
		      :XMN_TOP_OFFSET		5
		      :XMN_BOTTOM_OFFSET	5
		      ))
	  (send-super :set_values 
;;;		      :XMN_DEFAULT_BUTTON nil
		      :XMN_CANCEL_BUTTON close_btn_w
		      )

	  (send close_btn_w :add_callback :XMN_ACTIVATE_CALLBACK '()
		'(
		  (send-super :unmanage)
		  ))
	  (send help_btn_w :add_callback :XMN_ACTIVATE_CALLBACK '() ;pseudo :XMN_HELP_CALLBACK
		'(
		  (send help_w :error-display-string "Help not implemented... (sorry!).")
		  ))
	  )
	))

;;;
;;; Method :GET_FILL
;;;
(send XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS :answer :GET_FILL
      '()
      '(
	(/ (send fill-slider_w :get_value) 100.0)
	))

;;;
;;; Method :GET_LINE_STYLE
;;;
(send XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS :answer :GET_LINE_STYLE
      '()
      '(
	(/ (send line-style-slider_w :get_value) 100.0)
	))

;;;
;;; Method :GET_LINE_WIDTH
;;;
(send XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS :answer :GET_LINE_WIDTH
      '()
      '(
	(/ (send line-width-slider_w :get_value) 100.0)
	))

;;;
;;; Method :GET_LINE_ARROW
;;;
(send XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS :answer :GET_LINE_ARROW
      '()
      '(
	(case (read
	       (make-string-input-stream
		(send
		 (car
		  (send (send line-arrow-opt_w :GET_SUB_MENU_WIDGET) :get_values
			:XMN_MENU_HISTORY nil))
		 :name) 7))
	      (0  :no_arrow)
	      (1  :forw_arrow)
	      (2  :back_arrow)
	      (3  :both_arrow)
	      )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/wcls-imopt")
