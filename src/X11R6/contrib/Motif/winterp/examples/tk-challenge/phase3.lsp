; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         phase3.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/phase3.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Phase 3 of toolkit challenge
; Author:       Niels P. Mayer
; Created:      Thu Apr  2 1992
; Modified:     Mon Jun  6 01:19:10 1994 (Niels Mayer) npm@indeed
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

;; phase3.lsp: the code in this file requires that phase1.lsp, phase2.lsp
;; be loaded prior to loading this file.

(require "tk-challenge/phase1")
(require "tk-challenge/phase2")

;; 5:16 -- 5:30 : 14 mins

(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P


;;
;; (1) modify this method to add callback for 'clear'
;;

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
	))

;; added method :CLEAR-FIELDS for phase3
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

;; added method :SEARCH-FIELDS for phase3
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

;; need to modify this method originally def'd in phase1.lsp.
;; mod adds callback to ok button in file sel box widget
(send Rolodex_Application_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
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
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create the phase 3 version of the widget
(send Rolodex_Application_Widget_Class :new "app-3"
      :XMN_TITLE	"WINTERP: Toolkit Challenge Application (phase 3)"
      :XMN_ICON_NAME	"W:phase3"
      :XMN_GEOMETRY	"+1+1"		;should be removed in real app, spec'd
					;here so I don't have to place widget...
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "tk-challenge/phase3")
