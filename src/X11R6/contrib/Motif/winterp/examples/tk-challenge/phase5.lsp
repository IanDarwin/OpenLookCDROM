; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         phase5.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/phase5.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Phase 5 of toolkit challenge
; Author:       Niels P. Mayer
; Created:      Thu Apr  2 1992
; Modified:     Mon Jun  6 01:20:20 1994 (Niels Mayer) npm@indeed
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

;; phase5.lsp: the code in this file requires that phase1.lsp, phase2.lsp
;; phase3.lsp, and phase4.lsp be loaded prior to loading this file.

(require "tk-challenge/phase1")
(require "tk-challenge/phase2")
(require "tk-challenge/phase3")
(require "tk-challenge/phase4")


(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P

;; this global constant needs to be changed if the location of the 
;; location of the help file changes.
(defvar *help-file-directory* "/users/mayer/src/widgit/examples/tk-challenge/help-files/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function adds a help callback to a particular widget...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-help-to-widget (widget)
  (send widget :add_callback :XMN_HELP_CALLBACK '()
	`(
	  (send Help_Dialog_Widget_Class :new :managed ,widget)
	  )) 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a subclass of XM_MESSAGE_BOX_WIDGET_CLASS/:INFORMATION_DIALOG
;; which will look up help for a particular widget in *help-file-directory*
;; the name of the help file is named by the widget's name (as returned by
;; (send widget :name)==XtName()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	(format nil "Help file '~A' not found" help-file) ;return this on failure
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
;; Update various :ADD-ALL-CALLBACKS methods to add help callbacks to the 
;; appropriate widgets.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
			     :XMN_TITLE	  "Form Deletion Confirmation"
			     :XMN_MESSAGE_STRING "Are You sure?"
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

(send Rolodex_Application_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	;; same as before
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

	;; add the main application help callback. This'll help text will be
	;; come up when "Help" is requested on a widget that doesn't have
	;; it's own help callback, or when the 'Help on Application' pulldown
	;; is selected.
	(add-help-to-widget main-w)
	))

;; for phase5, this method gets altered to set up help callbacks
;; on each field. previously, this method was a no-op.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create the phase 5 version of the widget
(send Rolodex_Application_Widget_Class :new "app-5"
      :XMN_TITLE	"WINTERP: Toolkit Challenge Application (phase 5)"
      :XMN_ICON_NAME	"W:phase5"
      :XMN_GEOMETRY	"+1+1"		;should be removed in real app, spec'd
					;here so I don't have to place widget...
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "tk-challenge/phase5")
