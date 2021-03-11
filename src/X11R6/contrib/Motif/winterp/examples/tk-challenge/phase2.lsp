; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         phase2.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/tk-challenge/RCS/phase2.lsp,v 2.4 1994/06/06 14:58:04 npm Exp $
; Description:  Phase 2 of toolkit challenge
; Author:       Niels P. Mayer
; Created:      Thu Apr  2 1992
; Modified:     Mon Jun  6 01:18:50 1994 (Niels Mayer) npm@indeed
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

;; phase2.lsp: the code in this file requires that phase1.lsp be loaded
;; prior to loading this file.
(require "tk-challenge/phase1")

;; 5:00 -- 5:07 -- 7 mins

(require "lib-utils/initialize")	;define STRCAT, etc.
(require "lib-utils/motif-vers")	;define MOTIF-1.1-OR-LATER-P, MOTIF-1.1.3-OR-LATER-P
(require "lib-utils/unixstuf")		;define WINTERP-STANDALONE-P

;;
;; (1) modify this method to add callback to 'add-btn-w'
;;
(send Rolodex_Buttons_Widget_Class :answer :ADD-ALL-CALLBACKS '()
      '(
	(send add-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send rolodex-table-w :print-fields-to-stream *standard-output*)
		))
	(send delete-btn-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	      '(
		(send XM_MESSAGE_BOX_WIDGET_CLASS :new :managed :QUESTION_DIALOG
		      "confirm_form_deletion" self
		      :XMN_TITLE	  "Form Deletion Confirmation"
		      :XMN_MESSAGE_STRING "Are You sure?"
		      )
		))
	))

;;
;; (2) add method :print-fields-to-stream to Rolodex_Table_Widget_Class
;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create the phase 2 version of the widget
(send Rolodex_Application_Widget_Class :new "app-2"
      :XMN_TITLE		"WINTERP: Toolkit Challenge Application (phase 2)"
      :XMN_ICON_NAME		"W:phase2"
      :XMN_GEOMETRY	"+1+1"		;should be removed in real app, spec'd
					;here so I don't have to place widget...
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "tk-challenge/phase2")
