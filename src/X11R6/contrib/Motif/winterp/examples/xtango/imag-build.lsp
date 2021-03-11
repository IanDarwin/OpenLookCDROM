; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         imag-build.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/imag-build.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Interactive "image builder". Allows you to create/edit/save/load
;		images consisting of tango-image-objects. The actual drawing
;		UI leaves much to be desired, and the use of flashing to show
;		the selected set of images is clearly the wrong metaphor. But
;		remember that I wrote this mostly to test out WINTERP's xtango
;		interface -- though with some work this could become a useful
;		drawing tool.
;
;		On the drawing area, remember that left-mouse selects, left-drag
;		moves, middle-drag resizes, and right-click pops up a menu of
;		methods on the selected image (doesn't actually do anything,
;		yet). Shift-left-mouse does a multiple-selection operation, adding
;		or removing the selected image-object to the multiple selection list.
;		Once multi-selection has occurred, "Tango-->Group Selected Images"
;		will group the images into a single composite image; composites
;		can be acted on as a group by other tango operators, e.g. movement.
;		"Tango-->Window Snapshot" requires you have xwdtopnm ppmtogif
;		executables installed and on the search path (from PBM-PLUS),
;		and you cannot currently save the GIF images resulting from a
;		screen snapshot... Please feel free to fix/improve/debug this
;		application and submit code back to winterp@netcom.com
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 03:43:39 1994 (Niels Mayer) npm@indeed
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

(defvar *imag-build-default-delay* 0)

(require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*
(require "xtango/cls-image")		;define methods on all tango-image classes allowing movement/resizing, etc.
(require "xtango/cls-widget")		;define XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS
(require "xtango/xbm-to-arr")		;define BITMAP-FILE-TO-ARRAY, used by bitmap file sel dialog.
(require "xtango/wcls-imopt")		;define XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS
(require "xtango/wcls-fgcol")		;define XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS
(require "xtango/wcls-bgcol")		;define XTANGO-BG-COLOR-SELECTION-WIDGET-CLASS
(require "xtango/wcls-delay")		;define XTANGO-DELAY-SELECTION-WIDGET-CLASS
(require "lib-widgets/timed-msg")	;define TIMED-MESSAGE-DISPLAY-WIDGET-CLASS
;; (require "xtango/wcls-compo")	;define XTANGO-COMPOSITE-SELECTION-WIDGET-CLASS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((fsb_w NIL))
  (defun Get-Selected-Bitmap-Via-FileSB (popup-parent_w tango_w fg-color_w bg-color_w help_w)
    (if fsb_w
	(if (send fsb_w :is_managed)
	    (progn
	      (send fsb_w :unmap)
	      (send (send fsb_w :parent) :raise_window)
	      (send fsb_w :map)
	      (send fsb_w :forced_expose_update)
	      (let ((file (xm_string_get_l_to_r (car (send fsb_w :get_values :XMN_DIR_SPEC nil)))))
		(if (char/= #\/ (aref file (1- (length file)))) ;if trailing character '\', then no file selected...
		    (bitmap-file-to-array file ;RETURN
					  (send fg-color_w :get-tango-color)
					  (send bg-color_w :get-tango-color))
		  NIL			;RETURN
		  )
		))
	  (progn
	    (send fsb_w :manage)
	    NIL				;RETURN
	    )
	  )
      (progn
	(setq fsb_w
	      (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
		    "files" popup-parent_w
		    :XMN_DELETE_RESPONSE	:unmap
		    :XMN_AUTO_UNMANAGE		nil
		    :XMN_OK_LABEL_STRING	"Apply"	;since we don't automatically unmanage
		    :XMN_DIALOG_TITLE		"Select bitmap file name:"
		    :XMN_DIRECTORY		*X11-BITMAPS-DIRECTORY*
		    ))
	(send fsb_w :add_callback :XMN_OK_CALLBACK '()
	      '(
		(let ((file (xm_string_get_l_to_r (car (send fsb_w :get_values :XMN_DIR_SPEC nil)))))
		  (if (char/= #\/ (aref file (1- (length file)))) ;if trailing character '\', then no file selected...
		      (send TANGO:BITMAP_IMAGE_CLASS :new :show tango_w
			    (send tango_w :input_coord)
			    (vector (bitmap-file-to-array file
							  (send fg-color_w :get-tango-color)
							  (send bg-color_w :get-tango-color))) ;only one "frame" created (no movie), thus array of length 1
			    )
		    )
		  )
		))

	(send fsb_w :add_callback :xmn_cancel_callback '()
	      '(
		(send fsb_w :unmanage)
		))

	(send fsb_w :add_callback :XMN_HELP_CALLBACK
		    '()
		    '(
		      (send help_w :error-display-string "Help not implemented... (sorry!).")
		      ))
	NIL				;RETURN
	)
      )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let (
      top_w
      main_w menubar_w
      table_w
      create-bitmap_w create-circle_w create-composite_w create-ellipse_w create-line_w
      create-polygon_w create-polyline_w create-rectangle_w create-spline_w create-text_w
      sep_w
      tango_w
      file-selection-dialog_w
;;;   composite-selection-dialog_w
      image-create-params-dialog_w
      bg-color-dialog_w
      fg-color-dialog_w
      delay-dialog_w
      msg_w
      )

  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "imag-build"
	      :XMN_TITLE	"WINTERP: Xtango Interactive Image Builder"
	      :XMN_ICON_NAME	"W:imag-build"
	      ))

  (setq main_w
	(send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed 
	      "mainwin" top_w
;;;	      :XMN_SHOW_SEPARATOR T
	      ))

  (setq menubar_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
	      "menubar" main_w
	      :XMN_BUTTON_COUNT		5
	      :XMN_BUTTONS		#("System" "Tango" "Zoom/Pan" "Options" "Help")
	      :XMN_BUTTON_MNEMONICS	#(#\S     #\T     #\Z        #\O	 #\H)
	      :XMN_BUTTON_TYPE		#(:CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON)
	      ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "system" menubar_w
	 :XMN_POST_FROM_BUTTON		0 ;post from "System"
	 :XMN_BUTTON_COUNT		5
	 :XMN_BUTTONS			#("Quit" "Clear" "Load" "Save" "Save As")
	 :XMN_BUTTON_MNEMONICS		#(#\Q    #\C	#\L    #\S    #\A)
	 :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET)
   '(
     (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	   (0				;QUIT
	    (send top_w :destroy))
	   (1				;CLEAR
	    (send tango_w :clear-images)
	    )
	   (2				;LOAD
	    (send file-selection-dialog_w :set_callback :XMN_OK_CALLBACK '(CALLBACK_VALUE)
		  '(
		    (let ((file (xm_string_get_l_to_r CALLBACK_VALUE)))
		      (send msg_w :display-string (format NIL "Loading Xtango drawing from ~S ..." file))
		      (if (send tango_w :load-images file)
			  (send msg_w :display-string (format NIL "Done: Loading Xtango drawing from ~S." file))
			(send msg_w :error-display-string (format NIL "Error loading Xtango drawing from ~S" file))
			))
		    ))
	    (send file-selection-dialog_w :set_values
		  :XMN_DIALOG_TITLE "Enter filename for Load"
		  )
	    (send file-selection-dialog_w :do_search) ;force it to be updated w/ any new files added since prev call.
	    (send file-selection-dialog_w :manage)
	    )
	   (3				;SAVE
	    (send msg_w :error-display-string "Save Function Called (NOT IMPLEMENTED)."))
	   (4				;SAVE AS
	    (send file-selection-dialog_w :set_callback :XMN_OK_CALLBACK '(CALLBACK_VALUE)
		  '(
		    (let* ((file (xm_string_get_l_to_r CALLBACK_VALUE)))
		      (send msg_w :display-string (format NIL "Saving Xtango drawing as ~S ..." file))
		      (send tango_w :save-images file)
		      (send msg_w :display-string (format NIL "Done: Saving Xtango drawing as ~S." file))
		      )))
	    (send file-selection-dialog_w :set_values
		  :XMN_DIALOG_TITLE "Enter filename for Save-As"
		  )
	    (send file-selection-dialog_w :do_search) ;force it to be updated w/ any new files added since prev call.
	    (send file-selection-dialog_w :manage)
	    )
	   (T (send msg_w :error-display-string "Error")))
     ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "tango" menubar_w
	 :XMN_POST_FROM_BUTTON		1 ;post from "Tango"
	 :XMN_BUTTON_COUNT		10
	 :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :DOUBLE_SEPARATOR :PUSHBUTTON :PUSHBUTTON :DOUBLE_SEPARATOR :PUSHBUTTON :PUSHBUTTON)
	 :XMN_BUTTONS			#("Set Background Color..." ;0
					  "Set Foreground Color..." ;1
					  "Set Image Parameters..." ;2
					  "Set Animation Delay..." ;3
					  "---"	;4
					  "Group Selected Images" ;5
					  "Ungroup Selected Composite" ;6
					  "---"	;7
					  "Refresh Screen" ;8
					  "Window Snapshot" ;9
					  )
	 :XMN_BUTTON_MNEMONICS		#(#\B #\F #\I #\D #\- #\G #\U #\- #\R #\W)
	 )
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET)
   '(
     (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	   (0 (send bg-color-dialog_w :manage)
	      (send (send bg-color-dialog_w :parent) :raise_window))
	   (1 (send fg-color-dialog_w :manage)
	      (send (send fg-color-dialog_w :parent) :raise_window))
	   (2 (send image-create-params-dialog_w :manage)
	      (send (send image-create-params-dialog_w :parent) :raise_window))
	   (3 (send delay-dialog_w :manage)
	      (send (send delay-dialog_w :parent) :raise_window))
	   (4 (if (not (send tango_w :group-selected-images))
		  (send msg_w :error-display-string "Error: GROUP requires multiple selection of images.")
		))
	   (5 (if (not (send tango_w :ungroup-selected-image))
		  (send msg_w :error-display-string "Error: UNGROUP requires selection to be a composite image.")
		))
	   ;; Refresh
	   (6 (send tango_w :refresh)
	      )
	   ;; Window Snapshot --
	   ;; NOTE DEPENDENCY: The code below requires PBMPLUS utilities xwdtopnm(1)
	   ;; and ppmtogif(1). If you don't have these executables, the code below
	   ;; will fail with message "Error in window snapshot ... aborted."...
	   ;; NOTE BUG: if you attempt to do "save as" in this application with any
	   ;; GIF images at all, you will get an error -- because TANGO:GIF_IMAGE_CLASS
	   ;; does not have a :STOREON method.
	   (7 (send msg_w :display-string "Left-Click cross-hairs on window to capture...")
	      (if (eq T (system "(xwd -frame | xwdtopnm | ppmtogif > /tmp/scrndump.gif ) 2>&1"))
		  (progn
		    (send msg_w :display-string "Captured screen snapshot ... processing...")
		    (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
			  #C(0.0 0.0)	;location_coord
			  "/tmp/scrndump.gif"
			  :verbose
			  )
		    (system "/bin/rm -f /tmp/scrndump.gif")
		    (send msg_w :display-string "Captured screen snapshot ... DONE!")
		    )
		(send msg_w :error-display-string "Error in window snapshot ... aborted.")
		))
	   (T (send msg_w :error-display-string "Error")))
     ))

  (send
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "zoom-pan" menubar_w
	 :XMN_POST_FROM_BUTTON		2	;post from "Zoom/Pan"
	 :XMN_BUTTON_COUNT		7
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
	   (T (send msg_w :error-display-string "Error")))
     ))
 
  (send  
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "options" menubar_w
	 :XMN_POST_FROM_BUTTON		3	;post from "Options"
	 :XMN_BUTTON_COUNT		3
	 :XMN_BUTTON_TYPE		#(:RADIOBUTTON :RADIOBUTTON :TOGGLEBUTTON )
	 :XMN_BUTTONS			#("Patterns Represent Colors (Monochrome Only)"
					  "Patterns Represent Fills  (Monochrome Only)"
					  "Debug Messages")
	 :XMN_BUTTON_MNEMONICS		#(#\C #\F #\D)
	 )
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET)
   '(
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
	   (T (send msg_w :error-display-string "Error")))
     ))

  (send  
   (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	 "help" menubar_w
	 :XMN_POST_FROM_BUTTON		4 ;post from "Help"
	 :XMN_BUTTON_COUNT		1
	 :XMN_BUTTONS			#("Help")
	 :XMN_BUTTON_MNEMONICS		#(#\H)
	 :XMN_BUTTON_TYPE		#(:PUSHBUTTON)
	 )
   :add_callback :xmn_entry_callback	;use this instead of XmNsimpleCallback
   '(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET)
   '(
     (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
	   (0 (send msg_w :error-display-string "Help not implemented... (sorry!)."))
	   (T (send msg_w :error-display-string "Error")))
     ))

  ;; special motif attachment forcing correct placement of help pulldown.
  (let ((ch (send menubar_w :get_children)))
    (send menubar_w :set_values :XMN_MENU_HELP_WIDGET (aref ch (1- (length ch))))
    )

  (setq table_w
	(send TABLE_WIDGET_CLASS :new :managed
	      "table" main_w
	      ))
  (setq msg_w
	(send TIMED-MESSAGE-DISPLAY-WIDGET-CLASS :new :managed
	      "msg" main_w
	      ))
  (setq create-bitmap_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-bitmap" table_w
	      `(,TANGO:TEXT_IMAGE_CLASS #C(0.5 0.5) :ctr "<bitmap>" "black" "5x7")
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-circle_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-circle" table_w
	      `(,TANGO:CIRCLE_IMAGE_CLASS #C(0.5 0.5) 0.3 "black" 0.0)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-composite_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-composite" table_w
	      `(
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.00 0.00) #C(0.40 0.80) "red" 0.0
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.01) #C(0.5 0.24) "pink" 0.0
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.11) #C(0.6 0.24) "cyan" 0.0
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.21) #C(0.7 0.24) "yellow" 0.0
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.41) #C(0.8 0.24) "midnight blue" 0.0
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.61) #C(0.9 0.24) "indian red" 0.0
		,TANGO:RECTANGLE_IMAGE_CLASS #C(0.01 0.81) #C(1.0 0.24) "orange" 0.0
		,TANGO:CIRCLE_IMAGE_CLASS    #C(0.09 0.10) 0.20 "green" 0.0
		,TANGO:CIRCLE_IMAGE_CLASS    #C(0.5 0.50) 0.10 "black" 0.0
		)
 	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-ellipse_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-ellipse" table_w
	      `(,TANGO:ELLIPSE_IMAGE_CLASS #C(0.5 0.5) #C(0.2 0.3) "black" 0.0)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-line_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-line" table_w
	      `(,TANGO:LINE_IMAGE_CLASS #C(0.1 0.1) #C(0.7 0.7) "black" 0.0 1.0 :both_arrow)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-polygon_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-polygon" table_w
	      `(,TANGO:POLYGON_IMAGE_CLASS #C(0.5 0.5) #C(+0.30 +0.30) #C(-0.30 +0.35) #C(-0.30 +0.30)
					   #C(+0.30 -0.30) #C(+0.30 -0.30) #C(+0.30 +0.30) #C(-0.30 +0.30)
					   "black" 0.5)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-polyline_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-polyline" table_w
	      `(,TANGO:POLYLINE_IMAGE_CLASS #C(0.5 0.5) #C(+0.30 +0.30) #C(-0.30 +0.35) #C(-0.30 +0.30)
					    #C(+0.30 -0.30) #C(+0.30 -0.30) #C(+0.30 +0.30) #C(-0.30 +0.30)
					    "black" 0.0 1.0 :both_arrow)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-rectangle_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-rectangle" table_w
	      `(,TANGO:RECTANGLE_IMAGE_CLASS #C(0.1 0.1) #C(0.8 0.7) ,TANGO_COLOR_BLACK 0.0)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-spline_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-spline" table_w
	      `(,TANGO:SPLINE_IMAGE_CLASS #C(0.5 0.5) #C(+0.30 +0.30) #C(-0.30 +0.35) #C(-0.30 +0.30)
					  #C(+0.30 -0.30) #C(+0.30 -0.30) #C(+0.30 +0.30) #C(-0.30 +0.30)
					  "black" 0.0 1.0)
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq create-text_w
	(send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	      "create-text" table_w
	      `(,TANGO:TEXT_IMAGE_CLASS #C(0.5 0.5) :ctr "<text>" "black" "5x7")
	      :XMN_HEIGHT		40
	      :XMN_WIDTH		40
	      ))
  (setq sep_w
	(send XM_SEPARATOR_GADGET_CLASS :new :managed
	      "sep" table_w 
	      :XMN_ORIENTATION		:vertical
	      :XMN_SEPARATOR_TYPE	:shadow_etched_out
	      ))
  (setq tango_w
	(send XTANGO-WIDGET-CLASS :new :managed
	      "drawing-area" table_w
	      :XMN_HEIGHT		400
	      :XMN_WIDTH		400
	      ))

  (XT_TBL_CONFIG create-bitmap_w
		 0  0  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-circle_w
		 0  1  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-composite_w
		 0  2  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-ellipse_w
		 0  3  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-line_w
		 0  4  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-polygon_w
		 0  5  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-polyline_w
		 0  6  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-rectangle_w
		 0  7  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-spline_w
		 0  8  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG create-text_w
		 0  9  1  1 :tbl_sm_width :tbl_lk_height)
  (XT_TBL_CONFIG sep_w
		 1  0  1  10 :tbl_sm_width)
  (XT_TBL_CONFIG tango_w
		 2  0  8  10)

  (setq file-selection-dialog_w
	(send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :unmanaged :dialog ;NOTE... CREATE UNMANAGED pulldown menu pops it up
	      "files" menubar_w
	      :XMN_DIR_MASK		"*.lsp"
	      :XMN_AUTO_UNMANAGE	t
	      :XMN_DIALOG_STYLE		:dialog_full_application_modal ;they've got to answer this dialog before doing anything else w/ WINTERP...
	      ))
;;;   (setq composite-selection-dialog_w
;;; 	(send XTANGO-COMPOSITE-SELECTION-WIDGET-CLASS :new :unmanaged ;NOTE... CREATE UNMANAGED pulldown menu pops it up
;;; 	      "composite-selection" menubar_w
;;; 	      tango_w
;;; 	      msg_w
;;; 	      ))
  (setq image-create-params-dialog_w
 	(send XTANGO-IMAGE-OPTIONS-SELECTION-WIDGET-CLASS :new :unmanaged ;NOTE... CREATE UNMANAGED pulldown menu pops it up
 	      "image-options" menubar_w
 	      tango_w			;NOTE special extra widget creation arg, a TANGO:WIDGET_CLASS instance
	      msg_w			;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
 	      ))
  (setq bg-color-dialog_w
	(send XTANGO-BG-COLOR-SELECTION-WIDGET-CLASS :new :unmanaged ;NOTE... CREATE UNMANAGED pulldown menu pops it up
	      "bg-color" menubar_w
	      tango_w			;NOTE special extra widget creation arg, a TANGO:WIDGET_CLASS instance
	      msg_w			;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
	      ))
  (setq fg-color-dialog_w
	(send XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS :new :unmanaged ;NOTE... CREATE UNMANAGED pulldown menu pops it up
	      "fg-color" menubar_w
	      tango_w			;NOTE special extra widget creation arg, a TANGO:WIDGET_CLASS instance
	      msg_w			;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
	      ))
  (setq delay-dialog_w
	(send XTANGO-DELAY-SELECTION-WIDGET-CLASS :new :unmanaged ;NOTE... CREATE UNMANAGED pulldown menu pops it up
	      "delay" menubar_w
	      tango_w			;NOTE special extra widget creation arg, a TANGO:WIDGET_CLASS instance
	      msg_w			;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
	      *imag-build-default-delay* ;NOTE special extra widget creation arg, a FIXNUM setting up the delay value for xtango animation
	      ))

  ;; set areas for the XmMainWindow (see also method :set_areas)
  (send main_w :set_values
	:XMN_MENU_BAR		menubar_w
	:XMN_WORK_WINDOW	table_w
	:XMN_MESSAGE_WINDOW     msg_w
	)

  (send top_w :realize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (send create-bitmap_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	`(
	  (let* ((bmap-array (Get-Selected-Bitmap-Via-FileSB ,create-bitmap_w ,tango_w ,fg-color-dialog_w ,bg-color-dialog_w ,msg_w)))
	    (if bmap-array		;if the FileSB already popped up w/ a bmap selected, then use that...
					;otherwise, Get-Selected-Bitmap-Via-FileSB returns NIL and ends up
					;calling :INPUT_COORD & the code below once the user clicks "Ok" button.

		(send TANGO:BITMAP_IMAGE_CLASS :new :show ,tango_w
		      (send ,tango_w :input_coord)
		      (vector bmap-array) ;only one "frame" created (no movie), thus array of length 1
		      )
	      ))
	  ))
  (send create-circle_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord)
		0.05			;radius_float
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_fill) ;fill_float
		)
	  ))
  (send create-composite_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
;;;	  (send composite-selection-dialog_w :manage)
;;;	  (send (send composite-selection-dialog_w :parent) :raise_window)
	  ))
  (send create-ellipse_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:ELLIPSE_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord)
		#C(0.1 0.05)
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_fill) ;fill_float
		)
	  ))
  (send create-line_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:LINE_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord)
		#C(0.2 0.0)
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_line_width) ;width_float
		(send image-create-params-dialog_w :get_line_style) ;style_float
		(send image-create-params-dialog_w :get_line_arrow) ;arrow_int
		)
	  ))
  (send create-polygon_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord) #C(+0.10 +0.10) #C(+0.10 -0.10) #C(-0.10 +0.10)
		#C(+0.05 -0.10) #C(-0.10 +0.10) #C(+0.10 +0.10) #C(-0.10 -0.10)
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_fill) ;fill_float
		)
	  ))
  (send create-polyline_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:POLYLINE_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord) #C(+0.10 +0.10) #C(+0.10 -0.10) #C(-0.10 +0.10)
		#C(+0.05 -0.10) #C(-0.10 +0.10) #C(+0.10 +0.10) #C(-0.10 -0.10)
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_line_width) ;width_float
		(send image-create-params-dialog_w :get_line_style) ;line_style
		(send image-create-params-dialog_w :get_line_arrow) ;arrow_type
		)
	  ))
  (send create-rectangle_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:RECTANGLE_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord)
		#C(0.1 0.05)
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_fill) ;fill_float
		)
	  ))
  (send create-spline_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send TANGO:SPLINE_IMAGE_CLASS :new :show tango_w
		(send tango_w :input_coord) #C(+0.10 +0.10) #C(+0.10 -0.10) #C(-0.10 +0.10)
		#C(+0.05 -0.10) #C(-0.10 +0.10) #C(+0.10 +0.10)#C(-0.10 -0.10)
		(send fg-color-dialog_w :get-tango-color) ;tango_color
		(send image-create-params-dialog_w :get_line_width) ;width_float
		(send image-create-params-dialog_w :get_line_style) ;line_style
		)
	  ))
  (send create-text_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (send msg_w :display-string "Click crosshairs on location to position text...")
	  (let*
	      ((bounds		(send tango_w :INQ_COORD))
	       (lx		(first  bounds))
	       (by		(second bounds))
	       (rx		(third  bounds))
	       (ty		(fourth bounds))
	       (coords		(send tango_w :input_coord))
	       (px		(realpart coords))
	       (py		(imagpart coords))
	       (xloc		NIL)
	       (yloc		NIL)
	       (txt-w		NIL)
	       )
	    (if (/= lx 0)		;handle case of panning of x axis
		(setq rx (- rx lx)
		      px (- px lx)
		      lx 0))
	    (if (/= ty 0)		;handle case of panning of y axis
		(setq by (- by ty)
		      py (- py ty)
		      ty 0))

	    (setq xloc (truncate (* px (send tango_w :get :XMN_WIDTH)) rx)
		  yloc (truncate (* py (send tango_w :get :XMN_HEIGHT)) by))

	    ;; create a text widget at chosen location to accept user's text
	    (send msg_w :display-string "Enter text...")
	    (setq txt-w
		  (send XM_TEXT_WIDGET_CLASS :new :managed
			"edit" tango_w
			:XMN_STRING		""
			:XMN_EDIT_MODE		:multi_line_edit
			:XMN_WORD_WRAP		nil
			:XMN_EDITABLE		t ;allow user to change text.
			:XMN_CURSOR_POSITION_VISIBLE t ;yes, we want a cursor
			:XMN_AUTO_SHOW_CURSOR_POSITION t ;need to show where the cursor is
			:XMN_RESIZE_WIDTH	t
			:XMN_RESIZE_HEIGHT	t
			:XMN_ALLOW_RESIZE	t
			:XMN_X			xloc
			:XMN_Y			yloc
			))
	    (send txt-w :set_values
		  :XMN_Y (- yloc (truncate (send txt-w :get :XMN_HEIGHT) 2)))

	    ;; when user exits the text widget, delete it and replace it with
	    ;; a tango text image at the chosen location
	    (send txt-w :add_callback :XMN_LOSING_FOCUS_CALLBACK '()
		  '(
		    (let ((str (send txt-w :get_string)))
		      (if (> (length str) 0)
			  (progn
			    (send msg_w :display-string "Created text image.")
			    (send txt-w :destroy) ;destroy the text widget

			    ;; for each line of text, create a TANGO:TEXT_IMAGE_CLASS
			    ;; if multiple lines of text, group all the images
			    ;; into one composite image
			    (let (pos
				  (imgs '())
				  )
			      (loop
			       (setq pos (search "\n" str))
			       (setq imgs
				     (cons
				      (send TANGO:TEXT_IMAGE_CLASS :new :show tango_w
					    coords
					    (subseq str 0 pos) ;text_string
					    (send fg-color-dialog_w :get-tango-color) ;tango_color
					    "-*-*-bold-r-*--18-*-*-*-p-*-iso8859-1" ;font
					    )
				      imgs))
			       (if (null pos) (return))
			       (setq str
				     (subseq str (1+ pos) nil))
			       (setq coords
				     (+ coords (- (send (car imgs) :IMAGE_LOC :s)
						  (send (car imgs) :IMAGE_LOC :n))))
			       )
			      (if (> (length imgs) 1)
				  (progn ;group multiline text into one object
				    (send tango_w :set-multi-selected-images imgs)
				    (send tango_w :group-selected-images)
				    )
				(send tango_w :set-selected-image (car imgs)))
			      )
			    )
			(progn
			  (send txt-w :destroy) ;destroy the text widget
			  (send msg_w :error-display-string "Aborted creating text image")
			  )
			)
		      )
		    ))
	    )
	  ))
  )
