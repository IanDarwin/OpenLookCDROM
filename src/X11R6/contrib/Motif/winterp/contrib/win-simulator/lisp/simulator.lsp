; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         simulator.lsp
; RCS:          $Header: $
; Description:  Simulator UI for David Jacobson
; Author:       Niels Mayer, HPLabs
; Created:      Mon Aug 24 21:04:00 1992
; Modified:     Sun Jun  5 16:29:56 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
;
; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *breakenable* nil)		; T allows entry into breakloop
(setq *tracenable* t)			; set this to T if you want to see a backtrace on error.
(setq *gc-flag* nil)			; we don't want to see garbage collection messages

(defun load-chk (file-str)
  (if (not (load file-str))		;load doesn't signal an error.
      (error (format nil "Couldn't load ~A" file-str))
    )
  )

(load-chk "err-hook.lsp")		;winterp-lib: pops up a dialog box on winterp/xlisp error
;; (load-chk "redir-err.lsp")		;winterp-lib: pops up a dialog box on stderr output
(load-chk "help.lsp")			;winterp-lib: context-sensitive help
(load-chk "wc_string-browser.lsp")	;winterp-lib: String_Browser_Widget_Class & Methods
(load-chk "wc_object-browser.lsp")	;winterp-lib: Object_Browser_Widget_Class & Methods
(load-chk "wc_sim-contrlpnl.lsp")	;defines Simulator_Control_Panel_Widget_Class & Methods
(load-chk "wc_sim-status.lsp")		;defines Simulator_Status_Display_Widget_Class & Methods
(load-chk "wc_sim-messagearea.lsp")	;defines Simulator_Message_Area_Widget_Class & Methods
(load-chk "wc_sim-progview.lsp")	;defines Simulator_Program_Viewer_Widget_Class & Methods
(load-chk "reg-globals.lsp")		;defines various globals for register displays below...
(load-chk "wc_sim-register-field.lsp")	;defines Simulator_Register_Field_Widget_Class & Methods
(load-chk "wc_sim-int-reg.lsp")		;defines Simulator_Integer_Register_Widget_Class & Methods
(load-chk "wc_sim-pred-reg.lsp")	;defines Simulator_Predicate_Register_Widget_Class & Methods
(load-chk "wc_sim-float-reg.lsp")	;defines Simulator_Float_Register_Widget_Class & Methods
(load-chk "wc_sim-mem-trace.lsp")	;defines Simulator_Mem_Trace_Widget_Class & Methods
;;; note load-chk of API.lsp at the end of this file.

;;;
;;; this global constant (defvar'd in help.lsp) needs to be changed if the
;;; location of the location of the help files, or location of application,  changes. 
;;;
(setq *help-file-directory* "/usr/local/win-simulator/help/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE Simulator_Application_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Application_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  main-w
	  menubar-w
	  file-pd-w
	  view-pd-w
	  help-pd-w
	  control-panel-w
	  work-area-w
	  program-viewer-w
	  message-area-w

	  int-reg-popup-w
	  pred-reg-popup-w
	  float-reg-popup-w
	  mem-trace-popup-w
	  )
	'()				;no class vars
	TOP_LEVEL_SHELL_WIDGET_CLASS	;superclass
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Application_Widget_Class
;;
;; (send Simulator_Message_Area_Widget_Class :new
;;       <name-str>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Application_Widget_Class :answer :ISNEW
      '(name-str &rest args)
      '(
	;; create the TOP_LEVEL_SHELL_WIDGET_CLASS inst by sending :isnew to superclass
	(apply 'send-super		;widget-inst is now bound to <self>
	       `(:isnew ,name-str	;splice in method arguments passed in above
			:XMN_ALLOW_SHELL_RESIZE t ;allows temporary resize to display longer string in message area
			,@args		
			))
	(setq main-w
	      (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		    "main" self
		    ))
	(setq menubar-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		    "menubar" main-w
		    :XMN_BUTTON_COUNT		3
		    :XMN_BUTTONS		#("File" "View" "Help")
		    :XMN_BUTTON_MNEMONICS	#(#\F    #\V    #\H) 
		    ))
	(setq file-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "file-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's "File" button.
		    :XMN_BUTTON_COUNT		4
		    :XMN_BUTTONS		#("Load" "Quit And Restart" "Quit, Continuing WINTERP" "Quit, Terminating WINTERP")
		    :XMN_BUTTON_MNEMONICS	#(#\L    #\Q		    #\C			       #\T)
		    ))
	(setq view-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "view-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	1 ;post pulldown from menubar's "View" button.
		    :XMN_BUTTON_COUNT		4
		    :XMN_BUTTONS		#("Integer Registers" "Predicate Registers" "Floating Point Registers" "Memory Access Trace")
		    :XMN_BUTTON_MNEMONICS	#(#\I                 #\P		    #\F			       #\M)
		    ))
	(setq help-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "help-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	2 ;post pulldown from menubar's "Help" button.
		    :XMN_BUTTON_COUNT		3
		    :XMN_BUTTONS		#("Help on Application" "Pick the widget you want help on" "Edit the help-file on a picked widget")
		    :XMN_BUTTON_MNEMONICS	#(#\H                   #\P                                #\E)
		    ))
	;; special motif attachment forcing correct placement of help pulldown.
	(let ((ch (send menubar-w :get_children)))
	  (send menubar-w :set_values :XMN_MENU_HELP_WIDGET (aref ch (1- (length ch))))
	  )
	(setq work-area-w
	      (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
 		    "simulator-work-area" main-w
 		    ))
	(setq control-panel-w
	      (send Simulator_Control_Panel_Widget_Class :new :managed
		    "simulator-control-panel" work-area-w
		    ))
	(setq program-viewer-w
	      (send Simulator_Program_Viewer_Widget_Class :new :managed
		    "program-viewer" work-area-w
		    :XMN_COLUMNS			80 ;should be in app-defaults
		    :XMN_ROWS				24 ;should be in app-defaults
		    ))
	(setq message-area-w
	      (send Simulator_Message_Area_Widget_Class :new :managed
		    "message-area" main-w
		    :XMN_ROWS			1 ;should be in app-defaults
		    :XMN_COLUMNS		80 ;should be in app-defaults
		    :XMN_MARGIN_HEIGHT		2 ;should be in app-defaults
		    ))

	;; pass in program-viewer-w into the object denoted by control-panel-w
	;; (the program-viewer-w needs to be accessed by callbacks in the control panel)
	(send control-panel-w :set-program-viewer-w program-viewer-w)

	;; pass in message-area-w into the object denoted by control-panel-w
	;; (the message-area-w needs to be accessed by callbacks in the control panel)
	(send control-panel-w :set-message-area-w message-area-w)

	;; setup widget attachments for parts of Motif XmMainWindowWidget.
	(send main-w :set_areas
	      menubar-w			;set up :XMN_MENU_BAR attachment
	      NIL			;don't set up :XMN_COMMAND_WINDOW attachment
	      NIL NIL			;don't set up horiz/vertical scrollbars
	      work-area-w)		;set up :XMN_MESSAGE_WINDOW

	(send main-w :set_values	;for some reason, you can't set this via XmMainWindowSetAreas()==:SET_AREAS
	      :XMN_MESSAGE_WINDOW     message-area-w
	      )

	(send self :realize)

	;; set constraint resources on control-panel-w so that paned window
	;; doesn't give it a resize sash...
	;; NOTE: this must be done after :realize; before :realize, we won't
	;; know any sizes since windows haven't been created yet...
	(let (height)
	  (send control-panel-w :get_values :xmn_height 'height)
	  (send control-panel-w :set_values
		:XMN_PANE_MAXIMUM height
		:XMN_PANE_MINIMUM height
		)
	  )

	(send self :add-all-callbacks)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK INITIALIZATION METHOD FOR Simulator_Application_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Application_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send file-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      (0		;"Load"
		       (let (
			     (fsb-w
			      (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
				    "file_selection_box" main-w
				    ))
			     )
			 (send fsb-w :add_callback :XMN_OK_CALLBACK '()
			       `(
				 (let ((file
					(xm_string_get_l_to_r
					 (car (send ,fsb-w :get_values :XMN_TEXT_STRING nil))))
				       )
				   (send ,control-panel-w :set-file-label file)
				   (send ,program-viewer-w :find_file file)
				   )
				 (send ,fsb-w :destroy)
				 ))
			 (send fsb-w :add_callback :XMN_CANCEL_CALLBACK '()
			       `(
				 (send ,fsb-w :destroy)
				 ))
			 (add-help-to-widget fsb-w)
			 )
		       )
		      (1		;"Quit And Restart"
		       (send self :destroy)
		       (load-chk "simulator.lsp")
		       )
		      (2		;"Quit, Continuing WINTERP"
		       (send self :destroy)
		       )
		      (3		;"Quit, Terminating WINTERP"
		       (exit)
		       )
		      ))
	      )

	(send view-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      (0		;"Integer Registers"
		       (if (not (and int-reg-popup-w (send int-reg-popup-w :exists_p)))
			   (setq int-reg-popup-w
				 (send Simulator_Integer_Register_Widget_Class :new
				       "sim-int-reg" self))
			 )
		       (send int-reg-popup-w :map_raised)
		       )
		      (1		;"Predicate Registers"
		       (if (not (and pred-reg-popup-w (send pred-reg-popup-w :exists_p)))
			   (setq pred-reg-popup-w
				 (send Simulator_Predicate_Register_Widget_Class :new
				       "sim-pred-reg" self))
			 )
		       (send pred-reg-popup-w :map_raised)
		       )
		      (2		;"Floating Point Registers"
		       (if (not (and float-reg-popup-w (send float-reg-popup-w :exists_p)))
			   (setq float-reg-popup-w
				 (send Simulator_Float_Register_Widget_Class :new
				       "sim-float-reg" self))
			 )
		       (send float-reg-popup-w :map_raised)
		       )
		      (3		;"Memory Access Trace"
		       (if (not (and mem-trace-popup-w (send mem-trace-popup-w :exists_p)))
			   (setq mem-trace-popup-w
				 (send Simulator_Mem_Trace_Widget_Class :new
				       "sim-mem-trace" self))
			 )
		       (send mem-trace-popup-w :map_raised)
		       )
		      ))
	      )

	;; make the help pulldowns actually do something.
	(send help-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      ;; setup application help
		      (0		;"Help on Application"
;;;		       (send main-w :call_action_proc "Help" CALLBACK_XEVENT)
		       (send Help_Dialog_Widget_Class :new :managed main-w)
		       )
		      ;; setup context-sensitive help...
		      (1		;"Pick the widget you want help on"
		       (context-sensitive-help self CALLBACK_XEVENT)
		       )
		      (2		;"Edit the help-file on a picked widget"
		       (edit-context-sensitive-help self CALLBACK_XEVENT)
		       )
		      )
		))

	;; add the main application help callback. This help text will be
	;; come up when "Help" is requested on a widget that doesn't have
	;; it's own help callback, or when the 'Help on Application' pulldown
	;; is selected.
	(add-help-to-widget main-w)
	(add-help-to-widget work-area-w)
	(add-help-to-widget program-viewer-w)
	(add-help-to-widget message-area-w)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sim* nil)
(setq *sim*				;application-widget instance set to *sim*
      (send Simulator_Application_Widget_Class :new "simulator"
	    :XMN_TITLE		"Simulator"
	    :XMN_ICON_NAME	"Sim"
	    )
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-chk "API.lsp")			;defines API (methods) on Simulator_Application_Widget_Class
