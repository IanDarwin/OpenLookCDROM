; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-mem-trace.lsp
; RCS:          $Header: $
; Description:  Simulator_Mem_Trace_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 05:31:29 1992
; Modified:     Sun Jun  5 16:29:32 1994 (Niels Mayer) npm@indeed
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
;; fldr_cmpbr:Mail_Folder_Class -- holds info on an mh folder, info
;; is taken from MH 'folders' command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Mem_Trace_Item_Class
  (send Class :new
	'( ;;;;;;;;;;;;;;;;;;;;;;; INSTANCE VARIABLES ;;;;;;;;;;;;;;;;;;;;;
	  cycle				;integer
	  read-write			;character, either #\R or #\W
	  address-int			;integer address
	  value-int			;integer value @ address
	  )
	'( ;;;;;;;;;;;;;;;;;;;;;;; CLASS VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Mem_Trace_Item_Class :answer :ISNEW 
      '(cyc rw addr val)
      '(
	(setq cycle cyc)
	(setq read-write rw)
	(setq address-int addr)
	(setq value-int val)
	self
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Mem_Trace_Item_Class :answer :DISPLAY_STRING
      '()
      '(
	(let (s1 s2)

	  (setq s1 (format nil " ~5,,,A ~1,,,A " cycle read-write))

	  (progv			;locally bind *INTEGER-FORMAT*...
	   '(*INTEGER-FORMAT*) '("%lx")	;hack: print in hex by setting string used by sprintf :redisplay
	   (setq s2 (format nil "~8,,,A ~8,,,A" address-int value-int))
	   )

	  (concatenate 'string s1 s2)
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE Simulator_Mem_Trace_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Mem_Trace_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  main-w
	  menubar-w
	  trace-pd-w
	  help-pd-w
	  browser-w
	  message-area-w
	  )
	'()				;no class vars
	TOP_LEVEL_POPUP_SHELL_WIDGET_CLASS ;superclass
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Mem_Trace_Widget_Class
;;
;; (send Simulator_Mem_Trace_Widget_Class :new
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Mem_Trace_Widget_Class :answer :ISNEW
      '(name-str parent-w &rest args)
      '(
	(apply 'send-super
	       `(:isnew			;splice in method arguments passed in above
		 ,name-str ,parent-w
		 :XMN_TITLE	"Simulator: Memory Trace"
		 :XMN_ICON_NAME	"Sim:MemTrc"
		 ,@args		
		 ))
	(setq main-w
	      (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		    "mem-trace-main" self
		    ))
	(setq menubar-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		    "menubar" main-w
		    :XMN_BUTTON_COUNT		2
		    :XMN_BUTTONS		#("Trace" "Help")
		    :XMN_BUTTON_MNEMONICS	#(#\R     #\H) 
		    ))
	(setq trace-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "trace-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's "Trace" button.
		    :XMN_BUTTON_COUNT		1
		    :XMN_BUTTONS		#("Quit")
		    :XMN_BUTTON_MNEMONICS	#(#\Q)
		    ))
	(setq help-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "help-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	1 ;post pulldown from menubar's "Help" button.
		    :XMN_BUTTON_COUNT		3
		    :XMN_BUTTONS		#("Help on Application" "Pick the widget you want help on" "Edit the help-file on a picked widget")
		    :XMN_BUTTON_MNEMONICS	#(#\H                   #\P                                #\E)
		    ))
	;; special motif attachment forcing correct placement of help pulldown.
	(let ((ch (send menubar-w :get_children)))
	  (send menubar-w :set_values :XMN_MENU_HELP_WIDGET (aref ch (1- (length ch))))
	  )

	(setq browser-w
	      (send Object_Browser_Widget_Class :new :managed :scrolled
		    "memory-trace-list" main-w
		    :XMN_LIST_SIZE_POLICY		:variable
		    :XMN_SCROLL_BAR_DISPLAY_POLICY	:static
		    :XMN_VISIBLE_ITEM_COUNT		24
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
	      NIL			;don't set up :XMN_COMMAND_AREA attachment
	      NIL NIL			;don't set up horiz/vertical scrollbars
	      (send browser-w :parent))	;set up :XMN_WORK_WINDOW attachment -- need to access parent since it's a scrolled list...

	(send main-w :set_values	;for some reason, you can't set this via XmMainWindowSetAreas()==:SET_AREAS
	      :XMN_MESSAGE_WINDOW     message-area-w
	      )

	(send self :add-all-callbacks)

	(send self :popup :grab_none)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK INITIALIZATION METHOD FOR Simulator_Mem_Trace_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Mem_Trace_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send trace-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      (0		;"Quit"
		       (send self :unmap)
		       )
		      ))
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
      (add-help-to-widget browser-w)
      (add-help-to-widget message-area-w)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-MEM-TRACE
;; (send <smt-w> :set-mem-trace <list-of-memobjs>)
;; where each elt is of class Mem_Trace_Item_Class...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Mem_Trace_Widget_Class :answer :SET-MEM-TRACE
      '(list-of-memobjs)
      '(
	(send browser-w :set_browser_items list-of-memobjs)
	))


