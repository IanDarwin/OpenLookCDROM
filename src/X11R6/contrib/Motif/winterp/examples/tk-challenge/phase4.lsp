; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         phase4.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/phase4.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Phase 4 of toolkit challenge
; Author:       Niels P. Mayer
; Created:      Thu Apr  2 1992
; Modified:     Mon Jun  6 01:19:27 1994 (Niels Mayer) npm@indeed
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

;; phase4.lsp: the code in this file requires that phase1.lsp, phase2.lsp,
;; and phase3.lsp be loaded prior to loading this file.

(require "tk-challenge/phase1")
(require "tk-challenge/phase2")
(require "tk-challenge/phase3")


;; Took approx 6 mins to add everything but accels for "Clear" "Add"
;; "Search" and "Delete" (it is easy to add accelerators and mnemonics
;; on motif menubars and pulldowns).

;; Took about 20 minutes to figure out how to add accelerators for
;; "Clear" "Add" "Search" and "Delete" pushbuttons as accelerators 
;; on all widgets. This isn't readily supported by motif and must
;; be done using kludgy XtInstallAllAccelerators() mechanism.

;; note: accelerator ^A (add) and ^D (delete) don't work in text widget
;; because of conflict with existing emacs binding. Choosing accel char
;; with non-conflicting binding will allow this to work in text widgets
;; too, but I decided to use the same accel chars as stated in the assignent.

(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P

;;
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

;; needed to modify this method (from phase1.lsp) to add accelerators to
;; "Clear" "Add" "Search" "Delete" buttons
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

;; recursive routine used in Rolodex_Application_Widget_Class/:ISNEW
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

;; needed to change this to add file/load/exit mnemonics
;; needed to add :install_all_accelerators to Rolodex_Form_Widget_Class inst.
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

	(send self :install_all_accelerators self) ;added for phase4
	(wtree-recurse-install-accels self self) ;added for phase4

	(send self :realize)
	(send self :add-all-callbacks)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create the phase 4 version of the widget
(send Rolodex_Application_Widget_Class :new "app-4"
      :XMN_TITLE	"WINTERP: Toolkit Challenge Application (phase 4)"
      :XMN_ICON_NAME	"W:phase4"
      :XMN_GEOMETRY	"+1+1"		;should be removed in real app, spec'd
					;here so I don't have to place widget...
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "tk-challenge/phase4")
