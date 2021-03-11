;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         application.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/Application.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Complete Application for "Solbourne Toolkit Challenge"
;		this is a one-file version of the code in 
;		phase1.lsp, phase2.lsp, phase3.lsp, phase4.lsp, phase5.lsp
;		(Note that the push-button accelerators will only 
;		work for Motif 1.1.3/1.1.4. Accelerators will cause
;		Motif-inspired segmentation violations to occur when using
;		HP-UEDK/1.1.0/1.1.1/1.1.2).
;		Motif 1.2 seems to have killed the Help and
;		Context-Sensitive-Help parts of this appliciation.
; Author:       Niels Mayer
; Created:      Thu Apr  2 19:38:47 1992
; Modified:     Mon Jun  6 01:13:25 1994 (Niels Mayer) npm@indeed
; Language:     Winterp-Lisp
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

(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P

;; this global constant needs to be changed if the location of the 
;; location of the help file changes.
(defvar *help-file-directory* "/users/mayer/src/widgit/examples/tk-challenge/help-files/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rolodex_Application_Widget_Class
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
		    :XMN_BUTTON_MNEMONICS	#(#\F    #\H) ;added for phase4
		    :XMN_BUTTON_TYPE		#(:CASCADEBUTTON :CASCADEBUTTON)
		    ))
	(setq file-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "file-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	0 ;post pulldown from menubar's "File" button.
		    :XMN_BUTTON_COUNT		2
		    :XMN_BUTTONS		#("Load" "Exit")
		    :XMN_BUTTON_MNEMONICS	#(#\L    #\E) ;added for phase4
		    :XMN_BUTTON_ACCELERATORS    #("Ctrl<Key>L" "Ctrl<Key>E") ;added for phase4
		    :XMN_BUTTON_ACCELERATOR_TEXT #("^L" "^E") ;added for phase4
		    :XMN_BUTTON_TYPE		#(:PUSHBUTTON :PUSHBUTTON)
		    ))
	(setq help-pd-w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
		    "help-pd" menubar-w
		    :XMN_POST_FROM_BUTTON	1 ;post pulldown from menubar's "Help" button.
		    :XMN_BUTTON_COUNT		2
		    :XMN_BUTTONS		#("Help on Application"
						  "Pick the widget you want help on")
		    :XMN_BUTTON_MNEMONICS	#(#\H #\P) ;added for phase4
		    :XMN_BUTTON_ACCELERATORS    #("Ctrl<Key>H" "CtrlMeta<Key>H") ;added for phase4
		    :XMN_BUTTON_ACCELERATOR_TEXT #("^H" "M-^H") ;added for phase4
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

	;; Conditionalize call to wtree-recurse-install-accels -- HP UEDK Motif
	;; (on HPUX 8.0-8.07) and Motif 1.1.1/1.1.2 core dump due to Motif bugs.
	(if *MOTIF-1.1.3-OR-LATER-P*	;only fixed on motif 1.1.3, 1.1.4, 1.1.5
	    (wtree-recurse-install-accels self self) ;added for phase4
	  )

	(send self :realize)
	(send self :add-all-callbacks)
	))

(send Rolodex_Application_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	;; same as before (phase1-4)
	(send file-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      (0		;"Load"
		       (let (
			     (w
			      (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :managed :dialog
				    "file_selection_box" main-w
				    ))
			     )
			 (send w :add_callback :XMN_OK_CALLBACK '()
			       '(
				 (format T "Selected File = '~A'\n"
					 (xm_string_get_l_to_r
					  (car
					   (send w :get_values :XMN_TEXT_STRING nil))))
				 (send w :unmanage) ;pop down fsb...
				 ))
			 )
		       )
		      (1		;"Exit"
		       (if (winterp-standalone-p)
			   (exit))
		       (send-super :destroy) ;destroy TOP_LEVEL_SHELL_WIDGET_CLASS this instance
		       )
		      ))
	      )
	;; added for phase5 -- make the help pulldowns actually do something.
	(send help-pd-w :add_callback :XMN_ENTRY_CALLBACK 
	      '(CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
	      '(
		(case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		      ;; setup application help
		      (0		;"Help on Application"
		       (send main-w :call_action_proc "Help" CALLBACK_XEVENT)
		       )
		      ;; setup context-sensitive help...
		      (1		;"Pick the widget you want help on"
		       (let ((picked-w 
			      (xm_tracking_locate
			       self	;confine modalness to this app-window
			       92	;X11/cursorfont.h:#define XC_question_arrow 92
			       t)))	;force confine of modalness to app-window
			 (cond
			  ((send picked-w :is_manager)
			   (send picked-w :call_action_proc "ManagerGadgetHelp" CALLBACK_XEVENT)
			   )
			  ((send picked-w :is_primitive)
			   (send picked-w :call_action_proc "PrimitiveHelp" CALLBACK_XEVENT)
			   )
			  (t
			   (send picked-w :call_action_proc "Help" CALLBACK_XEVENT)
			   )
			  )
			 ))
		      )
		))

	;; add the main application help callback. This help text will be
	;; come up when "Help" is requested on a widget that doesn't have
	;; it's own help callback, or when the 'Help on Application' pulldown
	;; is selected.
	(add-help-to-widget main-w)
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

(send Rolodex_Table_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(add-help-to-widget name-field-w) ;added for phase5
	(add-help-to-widget addrs-0-field-w) ;added for phase5
	(add-help-to-widget addrs-1-field-w) ;added for phase5
	(add-help-to-widget addrs-2-field-w) ;added for phase5
	(add-help-to-widget hophone-field-w) ;added for phase5
	(add-help-to-widget wophone-field-w) ;added for phase5
	(add-help-to-widget fax-field-w) ;added for phase5

	;; this help will get called if help is called on some widget in this
	;; composite which doesn't have it's own help callback
	(add-help-to-widget self)	;added for phase5
	))

(send Rolodex_Table_Widget_Class :answer :PRINT-FIELDS-TO-STREAM
      '(strm)
      '(
	(format strm "Name:\t'~A'\nAddrs:\t'~A'\n\t'~A'\n\t'~A'\nHome:\t'~A'\nWork:\t'~A'\nFax:\t'~A'\n"
		(send name-field-w :get_string)
		(send addrs-0-field-w :get_string)
		(send addrs-1-field-w :get_string)
		(send addrs-2-field-w :get_string)
		(send hophone-field-w :get_string)
		(send wophone-field-w :get_string)
		(send fax-field-w :get_string)
		)
	))

(send Rolodex_Table_Widget_Class :answer :CLEAR-FIELDS '()
      '(
	(send name-field-w :set_string "")
	(send addrs-0-field-w :set_string "")
	(send addrs-1-field-w :set_string "")
	(send addrs-2-field-w :set_string "")
	(send hophone-field-w :set_string "")
	(send wophone-field-w :set_string "")
	(send fax-field-w :set_string "")
	))

(send Rolodex_Table_Widget_Class :answer :SEARCH-FIELDS '()
      '(
	(send self :print-fields-to-stream *standard-output*)
	(send name-field-w :set_string "Niels Mayer")
	(send addrs-0-field-w :set_string "127 Flood St.")
	(send addrs-1-field-w :set_string "Lower Flat")
	(send addrs-2-field-w :set_string "San Francisco, CA 94131")
	(send hophone-field-w :set_string "(415)584-1236")
	(send wophone-field-w :set_string "(415)857-6288")
	(send fax-field-w :set_string	  "(415)867-8526")
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
		    :XMN_LABEL_STRING "Clear (^C)" ;phase4 modified
		    :XMN_ACCELERATORS "Ctrl<Key>C: ArmAndActivate()" ;phase4 added
		    ))
	(setq add-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "add-btn" self
		    :XMN_LABEL_STRING "Add (^A)" ;phase4 modified
		    :XMN_ACCELERATORS "Ctrl<Key>A: ArmAndActivate()" ;phase4 added
		    ))
	(setq search-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "search-btn" self
		    :XMN_LABEL_STRING "Search (^S)" ;phase4 modified
		    :XMN_ACCELERATORS "Ctrl<Key>S: ArmAndActivate()" ;phase4 added
		    ))
	(setq delete-btn-w
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		    "delete-btn" self
		    :XMN_LABEL_STRING "Delete... (^D)" ;phase4 modified
		    :XMN_ACCELERATORS "Ctrl<Key>D: ArmAndActivate()" ;phase4 added
		    ))
	(send self :add-all-callbacks)
	))

(send Rolodex_Buttons_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	;; added this for phase 3 -- callback for "clear"
	(send clear-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send rolodex-table-w :clear-fields)
		))
	;; added this for phase 3 -- callback for "search"
	(send search-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send rolodex-table-w :search-fields)
		))
	;; same as phase 2
	(send add-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send rolodex-table-w :print-fields-to-stream *standard-output*)
		))
	;; modified this for phase 3 -- ok button in question dialog box
	(send delete-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(let (
		      (w
		       (send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :QUESTION_DIALOG
			     "confirm_form_deletion" self
			     :XMN_TITLE			"Form Deletion Confirmation"
			     :XMN_MESSAGE_STRING	"Are You sure?"
			     ))
		      )
		  (send w :add_callback :XMN_OK_CALLBACK '()
			'(
			  (send rolodex-table-w :clear-fields)
			  ))
		  )))

	;; these were added for phase5 -- help callbacks on various widgets
	(add-help-to-widget clear-btn-w)
	(add-help-to-widget add-btn-w)
        (add-help-to-widget search-btn-w)
	(add-help-to-widget delete-btn-w)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help_Dialog_Widget_Class:
;; define a subclass of XM_MESSAGE_BOX_WIDGET_CLASS/:INFORMATION_DIALOG
;; which will look up help for a particular widget in *help-file-directory*
;; the name of the help file is named by the widget's name (as returned by
;; (send widget :name)==XtName().
;; 
;; Use 'add-help-to-widget' to register a widget for context-sensitive
;; help using the aforementioned subclass.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This function adds a help callback to a particular widget...
;;
(defun add-help-to-widget (widget)
  (send widget :add_callback :XMN_HELP_CALLBACK '()
	`(
	  (send Help_Dialog_Widget_Class :new :managed ,widget)
	  )) 
  )

(setq Help_Dialog_Widget_Class
  (send Class :new			;create a new class (of class Class)
	'(				;declare instance vars
	  )
	'()				;no class vars
	XM_MESSAGE_BOX_WIDGET_CLASS	;superclass
	))

(send Help_Dialog_Widget_Class :answer :ISNEW
      '(managed-kwd parent-w &rest args) ;note: widget name generated automatically from parent-w
      '(
	;; create the XM_MESSAGE_BOX_WIDGET_CLASS inst by sending :isnew to superclass
	(apply #'send-super :isnew	;widget-inst is now bound to <self>
	       managed-kwd :INFORMATION_DIALOG
	       (send parent-w :name)	;name of help dialog is name of widget help called 
	       parent-w
	       :XMN_AUTO_UNMANAGE  nil
	       :XMN_MESSAGE_STRING (read-help-file-into-string (send parent-w :name))
	       args		
	       )
	(send (send self :get_child :DIALOG_HELP_BUTTON) :unmanage) ;no help on help...
	(send self :add_callback :XMN_CANCEL_CALLBACK '()
	      '(
		(send self :destroy)
		))
	(send self :add_callback :XMN_OK_CALLBACK '()
	      '(
		(send self :destroy)
		))
	))

;; file reader function used above by Help_Dialog_Widget_Class...
(defun read-help-file-into-string (help-file)
  (let
      ((fp (open (concatenate 'string *help-file-directory* help-file)
		 :direction :input))
       (res-list '())
       linestr
       )
    (if (null fp)
	(format nil "Help file '~A' not found" ;return this msg to help-dialog-box on failure
		(concatenate 'string *help-file-directory* help-file))
      (progn
	(loop
	 (if (null (setq linestr (read-line fp)))
	     (return))
	 (setq res-list (cons (concatenate 'string linestr "\n") res-list))
	 )
	(close fp)
	(apply #'strcat (reverse res-list)) ;return this on success
	))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Support Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evaling the following prints cmd line arg to stdout
;; this is done through hack -- winterp is started up with usual
;; args... this code looks for a special bogus -xrm form. Thus, to
;; specify database 'foo', start up winterp with:
;;   winterp -enable_unix_server -xrm "bogus-resource.database: foo"
;; and "cmdline-arg = 'foo'" would get printed to stdout.
;;
(let* ((cmd-args (send *TOPLEVEL_WIDGET* :get_argv))
       (num-args (length cmd-args))
       (match-str "bogus-resource.database: ")
       (match-len (length match-str))
       )
  (do ((i 0 (1+ i)))
      ((>= i num-args)
       )
      (if (>= (length (aref cmd-args i)) match-len)
	  (if (string=  (subseq (aref cmd-args i) 0 match-len) match-str)
	      (format T "cmdline-arg = '~A'\n"
		      (subseq (aref cmd-args i) match-len NIL))
	    )
	)))

;;
;; recursive routine used in Rolodex_Application_Widget_Class/:ISNEW
;;
(defun wtree-recurse-install-accels (cur source) ;'cur' assumed to be composite
  (map nil				;for each child of composite 'cur'
       (lambda (w)			;recursively install accels...
	 (if (not (send w :is_gadget)) 
	     (send w :install_all_accelerators source) ;install on leaf node
	   )
	 (if (send w :is_composite)
	     (wtree-recurse-install-accels w source) ;recurse on tree nodes
	   )
	 )
       (send cur :get_children)		;retrieve array of children of 'cur'
       )	
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;            Create The Application             ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; (instatiate Rolodex_Application_Widget_Class) ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send Rolodex_Application_Widget_Class :new "application"
      :XMN_TITLE	"WINTERP: Toolkit Challenge Application"
      :XMN_ICON_NAME	"W:Application"
      )
