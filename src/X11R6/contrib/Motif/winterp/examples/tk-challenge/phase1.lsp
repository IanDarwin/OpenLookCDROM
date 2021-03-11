; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         phase1.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/phase1.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Phase 1 of toolkit challenge
; Author:       Niels P. Mayer
; Created:      Thu Apr  2 1992
; Modified:     Mon Jun  6 01:18:13 1994 (Niels Mayer) npm@indeed
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

;; phase1.lsp: the code in this file also serves as the basis for
;; the incremental changes in files phase{2,3,4,5}.lsp. To implement
;; the changes needed for those phases, those files will override
;; some of the methods defined here, in addition to adding other supporting
;; functionality.

;; Took approx 10 mins to transform "phase0" into "phase1"

;; took another 20 mins to transform the code to create the rolodex
;; form into a 3 new widget classes that can then be reused throughout other phases.
;; (payback will occur in future phases, and will also ensure that a minimum of
;; global variables (none) are used.

(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Rolodex_Application_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  main-w
	  menubar-w
	  file-pd-w
	  help-pd-w
	  rolodex-form-w
	  )
	'()				;no class vars
	TOP_LEVEL_SHELL_WIDGET_CLASS	;superclass
	))

(send Rolodex_Application_Widget_Class :answer :ISNEW
      '(name-str &rest args)
      '(
	;; create the TOP_LEVEL_SHELL_WIDGET_CLASS inst by sending :isnew to superclass
	(apply #'send-super :isnew	;widget-inst is now bound to <self>
	       name-str	
	       args		
	       )
	(setq main-w
	      (send XM_MAIN_WINDOW_WIDGET_CLASS :new :managed
		    "main" self
		    ))
	(setq menubar-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
		    "menubar" main-w
		    :XMN_BUTTON_COUNT		2
		    :XMN_BUTTONS		#("File" "Help")
		    :XMN_BUTTON_TYPE		#(:CASCADEBUTTON :CASCADEBUTTON)
		    ))
	(setq file-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "file-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's "File" button.
		    :XMN_BUTTON_COUNT		2
		    :XMN_BUTTONS		#("Load" "Exit")
		    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON)
		    ))
	(setq help-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "help-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	1 ;post pulldown from menubar's "Help" button.
		    :XMN_BUTTON_COUNT		2
		    :XMN_BUTTONS		#("Help on Application"
						  "Pick the widget you want help on")
		    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON)
		    ))
	;; special motif attachment forcing correct placement of help pulldown.
	(let ((ch (send menubar-w :get_children)))
	  (send menubar-w :set_values :XMN_MENU_HELP_WIDGET (aref ch (1- (length ch))))
	  )
	(setq rolodex-form-w		;create an instance of widgetclass def'd above
	      (send Rolodex_Form_Widget_Class :new :managed
		    "rolodex-form" main-w
		    ))
	;; setup widget attachments for parts of Motif XmMainWindowWidget.
	(send main-w :set_areas menubar-w NIL NIL NIL rolodex-form-w)
	(send self :realize)
	(send self :add-all-callbacks)
	))

(send Rolodex_Application_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send file-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      (0		;"Load"
		       (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
			     "file_selection_box" main-w
			     ))
		      (1		;"Exit"
		       (if (winterp-standalone-p)
			   (exit))
		       (send-super :destroy) ;destroy TOP_LEVEL_SHELL_WIDGET_CLASS this instance
		       )
		      ))
	      )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rolodex_Form_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Rolodex_Form_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  table+bbox-form-w
	  table-w
	  button-box-w 
	  )
	'()				;no class vars
	XM_FRAME_WIDGET_CLASS		;superclass
	))

(send Rolodex_Form_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w &rest args)
      '(
	;; create the XM_FRAME_WIDGET_CLASS inst by sending :isnew to superclass
	(apply #'send-super :isnew	;widget-inst is now bound to <self>
	       managed-kwd name-str parent-w
	       args		
	       )
	(setq table+bbox-form-w
	      (send XM_FORM_WIDGET_CLASS :new :managed
		    "table+bbox-form" self
		    ))
	(setq table-w
	      (send Rolodex_Table_Widget_Class :new :managed
		    "rolodex-table" table+bbox-form-w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    ))
	(setq button-box-w 
	      (send Rolodex_Buttons_Widget_Class :new :managed
		    "button-box" table+bbox-form-w
		    table-w		;note extra arg passed in for callbacks applied to table-w
		    :XMN_TOP_ATTACHMENT		:attach_widget
		    :XMN_TOP_WIDGET		table-w
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rolodex_Table_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Rolodex_Table_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  name-field-w
	  addrs-0-field-w
	  addrs-1-field-w
	  addrs-2-field-w
	  hophone-field-w
	  wophone-field-w
	  fax-field-w
	  )
	'()				;no class vars
	TABLE_WIDGET_CLASS		;superclass
	))

;; initialization method for composite widget
(send Rolodex_Table_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w &rest args)
      '(
	;; create the XM_TABLE_WIDGET_CLASS inst by sending :isnew to superclass
	(apply #'send-super :isnew	;widget-inst is now bound to <self>
	       managed-kwd name-str parent-w
	       :XMN_LAYOUT	"name-label    0 0 1 1 rWH;\
				 addrs-0-label 0 1 1 1 rWH;\
				 addrs-1-label 0 2 1 1 rWH;\
				 addrs-2-label 0 3 1 1 rWH;\
				 hophone-label 0 4 1 1 rWH;\
				 wophone-label 0 5 1 1 rWH;\
				 fax-label     0 6 1 1 rWH;\
				 name-field    1 0 1 1 h;\
				 addrs-0-field 1 1 1 1 h;\
				 addrs-1-field 1 2 1 1 h;\
				 addrs-2-field 1 3 1 1 h;\
				 hophone-field 1 4 1 1 h;\
				 wophone-field 1 5 1 1 h;\
				 fax-field     1 6 1 1 h;"
	       args
	       )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "name-label" self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING "Name:"
	      )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "addrs-0-label" self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING "Address:"
	      )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "addrs-1-label" self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING " "
	      )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "addrs-2-label" self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING " "
	      )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "hophone-label" self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING "Home Phone:"
	      )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "wophone-label"	self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING "Work Phone:"
	      )
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "fax-label" self
	      :XMN_ALIGNMENT	:alignment_end
	      :XMN_LABEL_STRING "Fax:"
	      )
	(setq name-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "name-field" self
		    ))
	(setq addrs-0-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "addrs-0-field" self
		    ))
	(setq addrs-1-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "addrs-1-field" self
		    ))
	(setq addrs-2-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "addrs-2-field" self
		    ))
	(setq hophone-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "hophone-field" self
		    ))
	(setq wophone-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "wophone-field" self
		    ))
	(setq fax-field-w
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "fax-field" self
		    ))
	(send self :add-all-callbacks)
	))

;; a NO-OP method (in phase 1, for later phases, we'll override).
(send Rolodex_Table_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rolodex_Buttons_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Rolodex_Buttons_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  rolodex-table-w
	  clear-btn-w
	  add-btn-w
	  search-btn-w
	  delete-btn-w
	  )
	'()				;no class vars
	TABLE_WIDGET_CLASS		;superclass
	))

(send Rolodex_Buttons_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w table-w &rest args)
      '(
	(setq rolodex-table-w table-w)	;init instance variable
	;; create the TABLE_WIDGET_CLASS inst by sending :isnew to superclass
	(apply #'send-super :isnew	;widget-inst is now bound to <self>
	       managed-kwd name-str parent-w
	       :XMN_LAYOUT	"clear-btn  0 0 1 1 WH;\
				 add-btn    1 0 1 1 WH;\
				 search-btn 2 0 1 1 WH;\
				 delete-btn 3 0 1 1 WH;"
	       args
	       )
	(setq clear-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "clear-btn" self
		    :XMN_LABEL_STRING "Clear"
		    ))
	(setq add-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "add-btn" self
		    :XMN_LABEL_STRING "Add"
		    ))
	(setq search-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "search-btn" self
		    :XMN_LABEL_STRING "Search"
		    ))
	(setq delete-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "delete-btn" self
		    :XMN_LABEL_STRING "Delete..."
		    ))
	(send self :add-all-callbacks)
	))

(send Rolodex_Buttons_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send delete-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :QUESTION_DIALOG
		      "confirm_form_deletion" self
		      :XMN_TITLE	  "Form Deletion Confirmation"
		      :XMN_MESSAGE_STRING "Are You sure?"
		      )
		))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create the phase 1 version of the widget
(send Rolodex_Application_Widget_Class :new "app-1"
      :XMN_TITLE	"WINTERP: Toolkit Challenge Application (phase 1)"
      :XMN_ICON_NAME	"W:phase1"
      :XMN_GEOMETRY	"+1+1"		;should be removed in real app, spec'd
					;here so I don't have to place widget...
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "tk-challenge/phase1")
