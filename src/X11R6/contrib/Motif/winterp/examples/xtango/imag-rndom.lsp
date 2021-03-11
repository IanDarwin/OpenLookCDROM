; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         imag-rndom.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/imag-rndom.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Load this file to bring up a test UI for randomly placing
;		the requested TANGOIMAGE within the Tango drawing area widget.
;		On the drawing area, remember that left-mouse selects, left-drag
;		moves, middle-drag resizes, and right-click pops up a menu of
;		methods on the selected image (doesn't actually do anything,
;		yet).
; Author:       Niels P. Mayer
; Created:      Thu Mar 18 19:18:25 1993
; Modified:     Mon Jun  6 03:52:26 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define read-exec-cmd and other unixisms...
(require "xtango/cls-image")		;define methods on all tango-image classes allowing movement/resizing, etc.
(require "xtango/cls-widget")		;define XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS
(require "lib-widgets/timed-msg")	;define TIMED-MESSAGE-DISPLAY-WIDGET-CLASS

(let (top_w main_w
	    menubar_w system_pulldown_w tango_pulldown_w images_pulldown_w
	    tango_w
	    workarea_w tf_w pick_pb_w loc_pb_w sep_w
	    )

  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "xtango"
	      ))
  (setq main_w
	(send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
	      "mainw" top_w
	      ))
  (setq menubar_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
	      "menubar" main_w
	      :XMN_BUTTON_COUNT		4
	      :XMN_BUTTONS		#("System" "Tango" "Images" "Zoom/Pan")
	      :XMN_BUTTON_MNEMONICS	#(#\S #\T #\I #\Z)
	      :XMN_BUTTON_TYPE		#(:CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON)
	      ))
  (send
   (setq system_pulldown_w
	 (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	       "system" menubar_w
	       :XMN_POST_FROM_BUTTON	0 ;post from "System"
	       :XMN_BUTTON_COUNT	1
	       :XMN_BUTTONS		#("Quit")
	       :XMN_BUTTON_MNEMONICS	#(#\Q)
	       :XMN_BUTTON_TYPE		#(:PUSHBUTTON)
	       ))
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET)
   '(
     ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
     ;; where <#> is 0 ... (button-count-1).
     ;; we use 'read' to return the FIXNUM <#> after truncating the
     ;; 7 chars "button_" from the front of the string.
     (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	   (0				;QUIT
	    (send top_w :destroy)
	    )
	   ))
   )
  (setq tango_pulldown_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	      "tango" menubar_w
	      :XMN_POST_FROM_BUTTON	1 ;post from "Tango"
	      :XMN_BUTTON_COUNT		3
	      :XMN_BUTTON_TYPE		#(:RADIOBUTTON :RADIOBUTTON :TOGGLEBUTTON)
	      :XMN_BUTTONS		#("Patterns Represent Colors (Monochrome Only)"
					  "Patterns Represent Fills  (Monochrome Only)"
					  "Debug Messages")
	      :XMN_BUTTON_MNEMONICS	#(#\C #\F #\D)
	      ))
  (setq images_pulldown_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	      "images" menubar_w
	      :XMN_POST_FROM_BUTTON	2 ;post from "Images"
	      :XMN_BUTTON_COUNT		10
	      :XMN_BUTTONS		#("Draw-Line" "Draw-Rectangle" "Draw-Circle" "Draw-Ellipse" "Draw-Polyline"
					  "Draw-Polygon" "Draw-Spline" "Draw-Bitmap" "Draw-Text" "Draw-Composite")
	      :XMN_BUTTON_MNEMONICS	#(#\L #\R #\C #\E #\P #\g #\S #\B #\T #\m)
	      :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	      ))
  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "zoom-pan" menubar_w
	 :XMN_POST_FROM_BUTTON		3	;post from "Zoom/Pan"
	 :XMN_BUTTON_COUNT		7
	 :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	 :XMN_BUTTONS			#("Zoom In"
					  "Zoom Out"
					  "Pan Left"
					  "Pan Right"
					  "Pan Up"
					  "Pan Down"
					  "Reset")
	 :XMN_BUTTON_MNEMONICS		#(#\I #\O #\L #\R #\U #\D #\R)
	 :XMN_BUTTON_ACCELERATORS	#("<Key>Next"
					  "<Key>Prior"
					  "<Key>Left"
					  "<Key>Right"
					  "<Key>Up"
					  "<Key>Down"
					  "<Key>Home")
	 :XMN_BUTTON_ACCELERATOR_TEXT	#("<Page Down>-Key"
					  "<Page Up>-Key"
					  "<Left Arrow>-Key"
					  "<Right Arrow>-Key"
					  "<Up Arrow>-Key"
					  "<Down Arrow>-Key"
					  "<Home>-Key")
	 :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET)
   '(
     (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	   (0 (send tango_w :zoom :in 0.95))
	   (1 (send tango_w :zoom :out 0.95))
	   (2 (send tango_w :pan :left 0.025))
	   (3 (send tango_w :pan :right 0.025))
	   (4 (send tango_w :pan :up 0.025))
	   (5 (send tango_w :pan :down 0.025))
	   (6 (send tango_w :set_coord 0.0 1.0 1.0 0.0))
	   (T (send tf_w :error-display-string "Error")))
     ))


  (setq workarea_w
	(send XM_FORM_WIDGET_CLASS :new :managed
	      "work" main_w
	      :XMN_FRACTION_BASE 2
	      ))
  (setq pick_pb_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "pick_pb" workarea_w
	      :XMN_LABEL_STRING		"pick image"
	      :XMN_TOP_ATTACHMENT	:attach_form
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      :XMN_RIGHT_ATTACHMENT	:attach_position
	      :XMN_RIGHT_POSITION	1
	      ))
  (setq loc_pb_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "loc_pb" workarea_w
	      :XMN_LABEL_STRING		"display location"
	      :XMN_TOP_ATTACHMENT	:attach_form
	      :XMN_LEFT_ATTACHMENT	:attach_position
	      :XMN_LEFT_POSITION	1
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))
  (setq sep_w
	(send XM_SEPARATOR_GADGET_CLASS :new :managed
	      "sep" workarea_w
	      :XMN_ORIENTATION		:horizontal
	      :XMN_TOP_ATTACHMENT	:attach_widget
	      :XMN_TOP_WIDGET		pick_pb_w
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      ))
  (setq tango_w
	(send XTANGO-WIDGET-CLASS :new :managed
	      "tango" workarea_w
	      :XMN_HEIGHT 400
	      :XMN_WIDTH 400
	      :XMN_TOP_ATTACHMENT	:attach_widget
	      :XMN_TOP_WIDGET		sep_w
	      :XMN_LEFT_ATTACHMENT	:attach_form
	      :XMN_RIGHT_ATTACHMENT	:attach_form
	      :XMN_BOTTOM_ATTACHMENT	:attach_form
	      ))
  (setq tf_w
	(send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :new :managed
	      "message" main_w
	      ))

  (send main_w :set_values
	:XMN_MENU_BAR		menubar_w
	:XMN_WORK_WINDOW	workarea_w
	:XMN_MESSAGE_WINDOW     tf_w
	) 

  (send top_w :realize)

  (send tango_w :FORCED_EXPOSE_UPDATE)	;wait until exposed to ensure windows created for :begin_drawing call
  (send tango_w :begin_drawing)		;must call this after :realize

  (send pick_pb_w :set_callback :xmn_activate_callback '()
	'(
	  (let ( (image (send tango_w :input_image)) )
	    (if image
		(progn
		  (TANGO:TX_PERFORM (send image :TAP_FLASH 1))
		  (send tf_w :display-string (format nil "~A" image))
		  )
	      (send tf_w :error-display-string ":INPUT_IMAGE failed")
	      )
	    )
	  ))

  (send loc_pb_w :set_callback :xmn_activate_callback '()
	'(
	  (let ((coord (send tango_w :INPUT_COORD)))
	    (if coord
		(send tf_w :display-string (format nil ":INPUT_COORD returned ~A"
					       coord))
	      (send tf_w :error-display-string ":INPUT_COORD failed")
	      ))
	  ))

  (send tango_pulldown_w :add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET)
	'(
	  (let (
		(expose_callback NIL)
		)
	    ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	    ;; where <#> is 0 ... (button-count-1).
	    ;; we use 'read' to return the FIXNUM <#> after truncating the
	    ;; 7 chars "button_" from the front of the string.
	    (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		  (0 (if CALLBACK_ENTRY_SET
			 (progn
			   (send tango_w :mono_pattern_representation :colors)
			   (send (aref (send (send CALLBACK_ENTRY_WIDGET :parent) :get_children) 1) ;must enforce radio behavior manually
				 :set_state nil t)
			   )))
		  (1 (if CALLBACK_ENTRY_SET
			 (progn
			   (send tango_w :mono_pattern_representation :fills)
			   (send (aref (send (send CALLBACK_ENTRY_WIDGET :parent) :get_children) 0) ;must enforce radio behavior manually
				 :set_state nil t)
			   )))
		  (2 (send tango_w :set_debug CALLBACK_ENTRY_SET))
		  (T (send msg_w :error-display-string "Error"))))
	  ))

  (send images_pulldown_w :add_callback :xmn_entry_callback ;use this instead of XmNsimpleCallback
	'(CALLBACK_ENTRY_WIDGET)
	'(
	  ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	  ;; where <#> is 0 ... (button-count-1).
	  ;; we use 'read' to return the FIXNUM <#> after truncating the
	  ;; 7 chars "button_" from the front of the string.
	  (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		(0			;"DRAW-LINE"
		 (setq im 
		       (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) ;size_coord
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;width_float
			     (/ (random 1000000) 1000000.0) ;line_style
			     (case (random 3) ;arrow_type
				   (0 :no_arrow)
				   (1 :forw_arrow)
				   (2 :back_arrow)
				   (3 :both_arrow)
				   )
			     ))
		 )
		(1			;"DRAW-RECTANGLE"
		 (setq im 
		       (send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (complex (/ (random 500000) 1000000.0) ;size_coord
				      (/ (random 500000) 1000000.0))
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;fill_float
			     ))
		 )
		(2			;"DRAW-CIRCLE"
		 (setq im 
		       (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (/ (random 500000) 1000000.0) ;radius_float
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;fill_float
			     ))
		 )
		(3			;"DRAW-ELLIPSE"
		 (setq im 
		       (send TANGO:ELLIPSE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (complex (/ (random 500000) 1000000.0) ;radius_coord
				      (/ (random 500000) 1000000.0))
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;fill_float
			     ))
		 )
		(4			;"DRAW-POLYLINE"
		 (setq im 
		       (send TANGO:POLYLINE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) ;vertices_coords
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) 
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;width_float
			     (/ (random 1000000) 1000000.0) ;line_style
			     (case (random 3) ;arrow_type
				   (0 :no_arrow)
				   (1 :forw_arrow)
				   (2 :back_arrow)
				   (3 :both_arrow)
				   )
			     ))
		 )
		(5			;"DRAW-POLYGON"
		 (setq im 
		       (send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) ;vertices_coords
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) 
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;fill_float
			     ))
		 )
		(6			;"DRAW-SPLINE"
		 (setq im 
		       (send TANGO:SPLINE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) ;vertices_coords
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5) 
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (complex (- (/ (random 1000000) 1000000.0) 0.5)
				      (- (/ (random 1000000) 1000000.0) 0.5))
			     (random 8)	;tango_color
			     (/ (random 1000000) 1000000.0) ;width_float
			     (/ (random 1000000) 1000000.0) ;line_style
			     ))
		 )
		(7			;"DRAW-BITMAP"
		 (setq im 
		       (send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0)) ;loc_y_float
			     #(		;bitmap array
			       #(
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(7 1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 #(1 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)
				 ))
			     ))
		 )
		(8			;"DRAW-TEXT"
		 (setq im 
		       (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     :ctr
			     (read-exec-cmd "date") ;text_string
			     (random 8)	;tango_color
			     "10x20"	;default font
			     ))
		 )
		(9			;"DRAW-COMPOSITE"
		 (setq im 
		       (send TANGO:COMPOSITE_IMAGE_CLASS :new :show tango_w
			     (complex (/ (random 1000000) 1000000.0) ;location_coord
				      (/ (random 1000000) 1000000.0))
			     TANGO:RECTANGLE_IMAGE_CLASS #C(0.00 0.00) #C(0.100 0.20) TANGO_COLOR_BLACK 0.0
			     TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.01) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
			     TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.11) #C(0.080 0.08) TANGO_COLOR_BLACK 0.0
			     TANGO:CIRCLE_IMAGE_CLASS    #C(0.09 0.10) 0.005 TANGO_COLOR_BLACK 0.00
			     )
		       )
		 )
		))
	)
  )
