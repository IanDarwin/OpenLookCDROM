; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-int-reg.lsp
; RCS:          $Header: $
; Description:  Simulator_Integer_Register_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 05:27:32 1992
; Modified:     Sun Jun  5 16:29:04 1994 (Niels Mayer) npm@indeed
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
;; DEFINE Simulator_Integer_Register_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Integer_Register_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  main-w
	  menubar-w
	  reg-pd-w
	  view-pd-w
	  help-pd-w
	  table-w
	  message-area-w
	  label-array
	  textf-array
	  update-tick
	  display-mode			;one of :DECIMAL, :HEXADECIMAL, :OCTAL, :BINARY
	  )
	'()				;no class vars
	TOP_LEVEL_POPUP_SHELL_WIDGET_CLASS ;superclass
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Integer_Register_Widget_Class
;;
;; (send Simulator_Message_Area_Widget_Class :new
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Integer_Register_Widget_Class :answer :ISNEW
      '(name-str parent-w &rest args)
      '(
	(apply 'send-super
	       `(:isnew			;splice in method arguments passed in above
		 ,name-str ,parent-w
		 :XMN_DELETE_RESPONSE :unmap ;when the window is closed via the window manager, unmap it -- the default destroys the widget and all it's children...
		 :XMN_TITLE	"Simulator: Integer Registers"
		 :XMN_ICON_NAME	"Sim:IntReg"
		 ,@args		
		 ))
	(setq main-w
	      (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		    "integer-register-main" self
		    ))
	(setq menubar-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		    "menubar" main-w
		    :XMN_BUTTON_COUNT		3
		    :XMN_BUTTONS		#("Registers" "View" "Help")
		    :XMN_BUTTON_MNEMONICS	#(#\R        #\V    #\H) 
		    ))
	(setq reg-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "reg-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's "Registers" button.
		    :XMN_BUTTON_COUNT		1
		    :XMN_BUTTONS		#("Quit")
		    :XMN_BUTTON_MNEMONICS	#(#\Q)
		    ))
	(setq view-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "view-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	1 ;post pulldown from menubar's "View" button.
		    :XMN_BUTTON_COUNT		3
		    :XMN_BUTTONS		#("Decimal" "Hexadecimal" "Most Recently Set")
		    :XMN_BUTTON_TYPE		#(:RADIOBUTTON :RADIOBUTTON :RADIOBUTTON)
		    :XMN_BUTTON_MNEMONICS	#(#\D       #\H           #\M)
		    ))
	(send (aref (send view-pd-w :get_children) 0) :set_state t t) ;put it in "decimal mode" by default.
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

	(setq table-w
	      (send TABLE_WIDGET_CLASS :new :managed
		    "integer-register-table" main-w
		    ))

	(setq message-area-w
	      (send Simulator_Message_Area_Widget_Class :new :managed
		    "message-area" main-w
		    :XMN_ROWS			1 ;should be in app-defaults
		    :XMN_COLUMNS		80 ;should be in app-defaults
		    :XMN_MARGIN_HEIGHT		2 ;should be in app-defaults
		    ))

	;; setup widget attachments for parts of Motif XmMainWindowWidget.
	(send main-w :set_areas
	      menubar-w			;set up :XMN_MENU_BAR attachment
	      NIL			;don't set up :XMN_COMMAND_WINDOW attachment
	      NIL NIL			;don't set up horiz/vertical scrollbars
	      table-w)			;set up work area

	(send main-w :set_values	;for some reason, you can't set this via XmMainWindowSetAreas()==:SET_AREAS
	      :XMN_MESSAGE_WINDOW     message-area-w
	      )


	(setq display-mode :decimal)
	(setq update-tick 0)
	(setq label-array (make-array *num-registers*))
	(setq textf-array (make-array *num-registers*))

	;; create the labels in the register table
	(dotimes
	 (i *num-registers*)
	 (let ((label-w
		(send XM_LABEL_GADGET_CLASS :new :unmanaged 
		      (format nil "label~A" i) table-w
		      :XMN_LABEL_STRING		(format nil "R~A" i)
		      :XMN_MARGIN_HEIGHT	1 ;should be in app-defaults
		      :XMN_MARGIN_WIDTH		1 ;should be in app-defaults
		      )))
	   (XT_TBL_CONFIG label-w
			  (* 2 (truncate i *reg-num-rows*)) (rem i *reg-num-rows*) ;<col> <row>
			  1  1		;<h_span> <v_span>
			  :TBL_RIGHT :TBL_SM_WIDTH :TBL_SM_HEIGHT) ;table options
	   (setf (aref label-array i) label-w)
	   ))

	;; create the edit-field widgets in the register table
	(let ((bg-color (aref *reg-colorlist* (1- (length *reg-colorlist*)))))
	  (dotimes
	   (i *num-registers*)
	   (let ((textf-w
		  (send Simulator_Register_Field_Widget_Class :new :unmanaged 
			(format nil "edit~A" i) table-w
			i		;note this additional param passed in only to Simulator_Register_Field_Widget_Class
			:XMN_COLUMNS			10
			:XMN_CURSOR_POSITION_VISIBLE	t
			:XMN_BACKGROUND			bg-color
			:XMN_MARGIN_HEIGHT		1 ;should be in app-defaults
			:XMN_MARGIN_WIDTH		1 ;should be in app-defaults
			)))
	     (XT_TBL_CONFIG textf-w
			    (1+ (* 2 (truncate i *reg-num-rows*))) (rem i *reg-num-rows*) ;<col> <row>
			    1  1)	;<h_span> <v_span>
	     (setf (aref textf-array i) textf-w)

	     (send textf-w :add_callback :XMN_ACTIVATE_CALLBACK
		   '(CALLBACK_WIDGET)
		   '(
		     (send textf-w :register-changed-hook) ;user-defined hook in API.lsp

		     (setq update-tick (+ update-tick 1))
		     (case display-mode
			   (:DECIMAL
			    (progv	;locally bind *INTEGER-FORMAT*...
			     '(*INTEGER-FORMAT*) '("%ld") ;hack: print in decimal by setting string used by sprintf :redisplay
			     (let ((val (read (make-string-input-stream (send CALLBACK_WIDGET :get_string)))))
			       (if (not (integerp val))
				   (progn
				     (send message-area-w :warning (format nil "Non integer value, ~A, entered in ~A; substituting 0"
									   val (send callback_widget :name)))
				     (setq val 0)
				     ))
			       (send CALLBACK_WIDGET :set-register val update-tick)
			       )
			     (send CALLBACK_WIDGET :redisplay update-tick)
			     )
			    )
			   (:HEXADECIMAL
			    (progv	;locally bind *INTEGER-FORMAT*...
			     '(*INTEGER-FORMAT*) '("%lx") ;hack: print in hex by setting string used by sprintf :redisplay
			     (let ((val (read (make-string-input-stream (format nil "#x~A" (send CALLBACK_WIDGET :get_string))))))
			       (if (not (integerp val))
				   (progn
				     (send message-area-w :warning (format nil "Non integer value, ~A, entered in ~A; substituting 0"
									   val (send callback_widget :name)))
				     (setq val 0)
				     ))
			       (send CALLBACK_WIDGET :set-register val update-tick)
			       )
			     (send CALLBACK_WIDGET :redisplay update-tick)
			     )
			    )
			   )

		     ;; now that ticks have changed, need to recolorize all other registers...
		     (dotimes 
		      (i *num-registers*)
		      (send (aref textf-array i) :set-mru-color update-tick)
		      )
		     ))
	     )))

	(send self :add-all-callbacks)

	(xt_manage_children label-array)
	(xt_manage_children textf-array)

	(send self :popup :grab_none)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK INITIALIZATION METHOD FOR Simulator_Integer_Register_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Integer_Register_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send reg-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      (0		;"Quit"
		       (send self :unmap) ;does the same thing as wm.close w/ :XMN_DELETE_RESPONSE == :unmap
		       )
		      ))
	      )

	(send view-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET)
	      '(
		(if CALLBACK_ENTRY_SET
		    (let ((ch (send view-pd-w :get_children)))
		      (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
			    (0		;"Decimal"
			     (send (aref ch 1) :set_state nil nil) ;must manually enforce radio behavior...
			     (send (aref ch 2) :set_state nil nil)
			     (setq display-mode :decimal)
			     (send self :redisplay)
			     )
			    (1		;"Hexadecimal"
			     (send (aref ch 0) :set_state nil nil) ;must manually enforce radio behavior...
			     (send (aref ch 2) :set_state nil nil)
			     (setq display-mode :hexadecimal)
			     (send self :redisplay)
			     )
			    (2		;"Most Recently Set"
			     (send (aref ch 0) :set_state nil nil) ;must manually enforce radio behavior...
			     (send (aref ch 1) :set_state nil nil)
			     (send message-area-w :warning "'most recently set' view not yet implemented...")
			     )
			    )
		      )))
	      )

      ;; make the help pulldowns actually do something.
      (send help-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	    '(CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
	    '(
	      (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		    ;; setup application help
		    (0			;"Help on Application"
;;;		     (send main-w :call_action_proc "Help" CALLBACK_XEVENT)
		     (send Help_Dialog_Widget_Class :new :managed main-w)
		     )
		    ;; setup context-sensitive help...
		    (1			;"Pick the widget you want help on"
		     (context-sensitive-help self CALLBACK_XEVENT)
		     )
		    (2			;"Edit the help-file on a picked widget"
		     (edit-context-sensitive-help self CALLBACK_XEVENT)
		     )
		    )
	      ))

      ;; add the main application help callback. This help text will be
      ;; come up when "Help" is requested on a widget that doesn't have
      ;; it's own help callback, or when the 'Help on Application' pulldown
      ;; is selected.
      (add-help-to-widget main-w)
      (add-help-to-widget table-w)
      (add-help-to-widget message-area-w)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-REGISTERS
;;
;; (send <int-reg-w> :set-registers <max-tick> <array-of-int> <array-of-tick>
;;
;; -- this sets various internal instance variables, but doesn't display
;;    the new set of registers. Must call :REDISPLAY for that...
;; -- <array-of-int> and <array-of-tick> should be of length *num-registers*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Integer_Register_Widget_Class :answer :SET-REGISTERS
      '(max-tick array-of-int array-of-tick)
      '(
	(if (/= (length array-of-int) *num-registers*)
	    (error "<array-of-int> must be same length as *num-registers*" (length array-of-int)))
	(if (/= (length array-of-tick) *num-registers*)
	    (error "<array-of-tick> must be same length as *num-registers*" (length array-of-tick)))

	(setq update-tick max-tick)
	(map nil #'(lambda (widget int tick)
		     (send widget :set-register int tick))
	     textf-array
	     array-of-int
	     array-of-tick
	     )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :GET-REGISTERS
;;
;; (send <int-reg-w> :get-registers)
;;	--> returns (list <max-tick> <array-of-int> <array-of-tick>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Integer_Register_Widget_Class :answer :GET-REGISTERS
      '()
      '(
	(list
	 update-tick			;<max-tick>
	 (map 'array #'(lambda (x) (send x :get-value))	;<array-of-int>
	      textf-array)
	 (map 'array #'(lambda (x) (send x :get-tick)) ;<array-of-tick>
	      textf-array)
	      
	 )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :REDISPLAY
;;
;; (send <int-reg-w> :redisplay)
;;
;; -- displays the values stored in the registers in either
;;    :hexadecimal or :decimal depending on inst var 'display-mode'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Integer_Register_Widget_Class :answer :REDISPLAY
      '()
      '(
	(case display-mode
	      (:DECIMAL
	       (progv			;locally bind *INTEGER-FORMAT*...
		'(*INTEGER-FORMAT*) '("%ld") ;hack: print in decimal by setting string used by sprintf :redisplay
		(map nil #'(lambda (x) (send x :redisplay update-tick))
		     textf-array
		     )
		)
	       )
	      (:HEXADECIMAL
	       (progv			;locally bind *INTEGER-FORMAT*...
		'(*INTEGER-FORMAT*) '("%lx") ;hack: print in hex by setting string used by sprintf :redisplay
		(map nil #'(lambda (x) (send x :redisplay update-tick))
		     textf-array
		     )
		)
	       )
	      )
	))
