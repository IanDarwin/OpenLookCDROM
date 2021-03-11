; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-status.lsp
; RCS:          $Header: $
; Description:  Simulator_Status_Display_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Tue Aug 25 23:10:26 1992
; Modified:     Sun Jun  5 16:34:21 1994 (Niels Mayer) npm@indeed
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
;; DEFINE Simulator_Status_Display_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Status_Display_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  sim-state			;set to one of NIL, :RUN, :STOP, :INTERRUPT, :BREAK, :ERROR, :EXIT
	  )
	'()				;no class vars
	XM_ROW_COLUMN_WIDGET_CLASS	;superclass
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Status_Display_Widget_Class
;;
;; (send Simulator_Status_Display_Widget_Class :new {:managed,:unmanaged}
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Status_Display_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w &rest args)
      '(
	(apply 'send-super		;widget-inst is now bound to <self>
	       `(:isnew ,managed-kwd	;splice in method arguments passed in above
			:simple_radio_box
			,name-str ,parent-w
			,@args
			:XMN_BUTTON_COUNT	6 ;create six buttons
			:XMN_BUTTON_TYPE	#(:TOGGLEBUTTON :TOGGLEBUTTON :TOGGLEBUTTON :TOGGLEBUTTON :TOGGLEBUTTON :TOGGLEBUTTON)
			:XMN_BUTTONS		#("Run"		"Stop"	      "Intr."       "Break"	  "Error"	"Exit")
			))

	(setq sim-state NIL)

	(send self :add-all-callbacks)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK INITIALIZATION METHOD FOR Simulator_Status_Display_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Status_Display_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	;; since this is a display-only widget, we set up a callback that 
	;; reasserts state set by sim-state if the user tries to change
	;; a button using the mouse...
	(send self :add_callback :XMN_ENTRY_CALLBACK ;use this instead of XmNsimpleCallback
	      '()
	      `(
		(send ,self :set-status sim-state)
		))

	;; add help callbacks on various widgets
	(map nil #'add-help-to-widget (send self :get_children))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-STATUS
;;
;; (send <status-disp-w> :set-status <status>)
;;
;; where <status> is one of
;; :RUN, :STOP, :INTERRUPT, :BREAK, :ERROR, :EXIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Status_Display_Widget_Class :answer :SET-STATUS
      '(status)
      '(
	(let ((c (send self :get_children)))
	  (case status
		(:RUN
		 (setq sim-state :run)
		 (send (aref c 0) :set_state t t)
		 )
		(:STOP
		 (setq sim-state :stop)
		 (send (aref c 1) :set_state t t)
		 )
		(:INTERRUPT
		 (setq sim-state :interrupt)
		 (send (aref c 2) :set_state t t)
		 )
		(:BREAK
		 (setq sim-state :break)
		 (send (aref c 3) :set_state t t)
		 )
		(:ERROR
		 (setq sim-state :error)
		 (send (aref c 4) :set_state t t)
		 )
		(:EXIT
		 (setq sim-state :exit)
		 (send (aref c 5) :set_state t t)
		 )
		(NIL
		 (setq sim-state nil)
		 (map nil		;turn them all off...
		      #'(lambda (w) (send w :set_state nil nil))
		      c)
		 )
		))
	NIL
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :GET-STATUS
;;
;; (send <status-disp-w> :get-status)
;;
;; returns one of :RUN, :STOP, :INTERRUPT, :BREAK, :ERROR, :EXIT, NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Status_Display_Widget_Class :answer :GET-STATUS
      '()
      '(
	sim-state
	))
