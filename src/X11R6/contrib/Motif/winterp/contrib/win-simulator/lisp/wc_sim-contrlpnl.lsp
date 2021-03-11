; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-contrlpnl.lsp
; RCS:          $Header: $
; Description:  Simulator_Control_Panel_Widget_Class & Methods
; Author:       Niels Mayer, HPLabs
; Created:      Wed Aug 26 14:18:27 1992
; Modified:     Sun Jun  5 16:31:37 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE Simulator_Control_Panel_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Control_Panel_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  buttons-table-w
	  step-btn-w
	  step-n-btn-w
	  step-n-edit-w
	  set-brkpnt-btn-w
	  clr-brkpnt-btn-w
	  interrupt-btn-w
	  run-btn-w
	  curtime-disp-w
	  pc-disp-w
	  status-disp-w
	  status-label-w
	  editfile-label-w

	  program-viewer-w		;value is passed in from Simulator_Application_Widget_Class...
	  message-area-w		;value is passed in from Simulator_Application_Widget_Class...
	  )
	'()				;no class vars
	XM_FORM_WIDGET_CLASS		;superclass
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Control_Panel_Widget_Class
;;
;; (send Simulator_Control_Panel_Widget_Class :new {:managed,:unmanaged}
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w &rest args)
      '(
	;; create the TABLE_WIDGET_CLASS inst by sending :isnew to superclass
	(apply 'send-super		;widget-inst is now bound to <self>
	       `(:isnew ,managed-kwd	;splice in method arguments passed in above
			,name-str ,parent-w
			,@args
			))
	(setq buttons-table-w
	      (send TABLE_WIDGET_CLASS :new :managed
		    "buttons-table" self
		    :XMN_LAYOUT "step-btn	0 0 1 1;\
				step-n-btn	1 0 1 1;\
				step-n-edit	2 0 1 1;\
				run-btn		3 0 1 1;\
				interrupt-btn	4 0 1 1;\
				set-brkpnt-btn	5 0 1 1;\
				clr-brkpnt-btn	6 0 1 1;"
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    ))
	(setq step-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "step-btn" buttons-table-w
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Single Step"
		    ))
	(setq step-n-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "step-n-btn" buttons-table-w
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Step by:"
		    ))
	(setq step-n-edit-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed 
		    "step-n-edit" buttons-table-w
		    :XMN_EDIT_MODE		:single_line_edit
		    :XMN_EDITABLE		t
		    :XMN_COLUMNS		3
		    ))
	(setq run-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "run-btn" buttons-table-w
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Run"
		    ))
	(setq interrupt-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "interrupt-btn" buttons-table-w
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Interrupt"
		    ))
	(setq set-brkpnt-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "set-brkpnt-btn" buttons-table-w
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Set BkPt."
		    ))
	(setq clr-brkpnt-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "clr-brkpnt-btn" buttons-table-w
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Clear BkPt."
		    ))
	(setq curtime-disp-w
	      (send XM_LABEL_WIDGET_CLASS :new :managed 
		    "curtime-disp" self
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		" Time: ??:??:??"
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		buttons-table-w
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    ))
	(setq pc-disp-w
	      (send XM_LABEL_WIDGET_CLASS :new :managed 
		    "pc-disp" self
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"PC: 0xff00ff00ff00ff00 "
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		buttons-table-w
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    ))
	(setq status-disp-w
	      (send Simulator_Status_Display_Widget_Class :new :managed
		    "status-disp" self
		    :XMN_ORIENTATION		:horizontal
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		curtime-disp-w
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    ))
	(setq status-label-w
	      (send XM_LABEL_WIDGET_CLASS :new :managed 
		    "status-label" self
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		"Status:"
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		curtime-disp-w
		    :XMN_RIGHT_ATTACHMENT	:attach_widget
		    :XMN_RIGHT_WIDGET		status-disp-w
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    ))
	(setq editfile-label-w
	      (send XM_LABEL_WIDGET_CLASS :new :managed 
		    "editfile-label" self
		    :XMN_LABEL_TYPE		:string
		    :XMN_LABEL_STRING		" <none>"
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		curtime-disp-w
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    ))

	(send self :add-all-callbacks)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK INITIALIZATION METHOD FOR Simulator_Control_Panel_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send status-disp-w :set-status :stop) ;initialize status display to "Stop"

	(send step-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send message-area-w :warning "single step")

		(send self :single-step-hook) ;def'd in API.lsp
		))

	(send step-n-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send message-area-w :warning (format nil "step by ~A" (send step-n-edit-w :get_string)))

		;; ensure that the string in the editor is an integer
		(let ((val (read (make-string-input-stream (send step-n-edit-w :get_string)))))
		  (if (not (integerp val))
		      (progn
			(send message-area-w :warning
			      (format nil "Non integer value, ~A, entered as \"step by\" value; substituting 1"
				      val))
			(setq val 1)
			(send step-n-edit-w :set_string (format nil "~A" val))
			))

		  (send self :multi-step-hook val) ;def'd in API.lsp
		  )
		))

	(send step-n-edit-w :add_callback :XMN_ACTIVATE_CALLBACK
	      '(CALLBACK_XEVENT)
	      `(
		(send ,step-n-btn-w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT) 
		))

	(send run-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(if (eq (send status-disp-w :get-status) :run)
		    (send message-area-w :warning "Can't 'run' -- program already running!!")

		  (progn
		    (send status-disp-w :set-status :run)
		    (send self :run-hook) ;def'd in API.lsp
		    )
		  )
		))

	(send interrupt-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(if (not (eq (send status-disp-w :get-status) :run))
		    (send message-area-w :warning "Can't 'interrupt' -- program not running!!")

		  (progn
		    (send status-disp-w :set-status :interrupt)
		    (send self :interrupt-hook)	;def'd in API.lsp
		    )
		  )
		))

	(send set-brkpnt-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(let ((brkpnt-line
		       (send program-viewer-w :HIGHLIGHT-CURRENT-LINE))
		      )

		  (send message-area-w :warning
			(format nil "set breakpoint at line ~A"
				brkpnt-line
				))

		  ;; may want/need to retain all breakpoints somewhere too...
		  (send self :set-breakpoint-hook brkpnt-line) ;def'd in API.lsp
		  )
		))

	(send clr-brkpnt-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(let ((brkpnt-line
		       (send program-viewer-w :UNHIGHLIGHT-CURRENT-LINE))
		      )

		  (send message-area-w :warning
			(format nil "clear breakpoint at line ~A"
				brkpnt-line
				))

		  ;; may want/need to remove this bkpt from breakpoint cache...
		  (send self :clear-breakpoint-hook brkpnt-line) ;def'd in API.lsp

		  )
		))

	;; add help callbacks on various widgets
	(add-help-to-widget step-btn-w)
	(add-help-to-widget step-n-btn-w)
	(add-help-to-widget step-n-edit-w)
	(add-help-to-widget run-btn-w)
	(add-help-to-widget interrupt-btn-w)
	(add-help-to-widget set-brkpnt-btn-w)
	(add-help-to-widget clr-brkpnt-btn-w)
	(add-help-to-widget curtime-disp-w)
	(add-help-to-widget pc-disp-w)
	(add-help-to-widget status-disp-w)
	(add-help-to-widget status-label-w)
	(add-help-to-widget editfile-label-w)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-PROGRAM-VIEWER-W
;;
;; (send <scp-w> :set-program-viewer-w <prog-view-w>
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :SET-PROGRAM-VIEWER-W
      '(w)
      '(
	(setq program-viewer-w w)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-MESSAGE-AREA-W
;;
;; (send <scp-w> :set-message-area-w <msg-area-w>
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :SET-MESSAGE-AREA-W
      '(w)
      '(
	(setq message-area-w w)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-FILE-LABEL
;;
;; (send <scp-w> :set-file-label <string>
;; 
;; -- sets <editfile-label-w> to the desired filename...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :SET-FILE-LABEL
      '(str)
      '(
	(send editfile-label-w :set_values 
;;;           :XMN_LABEL_STRING (format nil " File: ~A" str)
	      :XMN_LABEL_STRING (format nil " ~A" str)
	      )
	))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-CURTIME-LABEL
;;
;; (send <scp-w> :SET-CURTIME-LABEL <string>
;; 
;; -- sets <curtime-disp-w> to the desired time string...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :SET-CURTIME-LABEL
      '(time-str)
      '(
	(send curtime-disp-w :set_values 
	      :XMN_LABEL_STRING (format nil " Time: ~A" time-str)
	      )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-PC-LABEL
;;
;; (send <scp-w> :set-pc-label <string>
;; 
;; -- sets <pc-disp-w> to the desired PC string...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :SET-PC-LABEL
      '(pc-str)
      '(
	(send pc-disp-w :set_values 
	      :XMN_LABEL_STRING (format nil "PC: ~A" pc-str)
	      )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-SIM-STATUS-DISPLAY
;;
;; (send <scp-w> :set-sim-status-display <status>)
;; where <status> is one of
;; :RUN, :STOP, :INTERRUPT, :BREAK, :ERROR, :EXIT, NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :SET-SIM-STATUS-DISPLAY
      '(status)
      '(
	(send status-disp-w :SET-STATUS status)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :GET-SIM-STATUS-DISPLAY
;;
;; (send <scp-w> :get-sim-status-display
;; returns one of :RUN, :STOP, :INTERRUPT, :BREAK, :ERROR, :EXIT, NIL)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Control_Panel_Widget_Class :answer :GET-SIM-STATUS-DISPLAY
      '()
      '(
	(send status-disp-w :GET-STATUS)
	))


