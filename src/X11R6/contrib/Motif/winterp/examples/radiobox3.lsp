; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         radiobox3.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/radiobox3.lsp,v 2.4 1994/06/06 14:43:05 npm Exp $
; Description:  Create radio-box via XM_ROW_COLUMN_WIDGET_CLASS/
;		:simple_radio_box.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Sun Jun  5 19:16:30 1994 (Niels Mayer) npm@indeed
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

(setq toplevel-w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new  "radiobox3"
	    :XMN_GEOMETRY	"+1+1"
	    :XMN_TITLE		"WINTERP: Radio-Box-Test #3"
	    :XMN_ICON_NAME	"W:radiobox3"
	    ))

(setq options-w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_radio_box
	    "options" toplevel-w
	    :XMN_ORIENTATION		:horizontal
	    :XMN_BUTTON_COUNT		5 ;create five buttons
	    :XMN_BUTTON_TYPE		#(:RADIOBUTTON :RADIOBUTTON :RADIOBUTTON :RADIOBUTTON :RADIOBUTTON)
	    :XMN_BUTTONS 		#("Foo"        "Bar"        "Baz"        "Frobozz"    "Glorck")
	    :XMN_BUTTON_SET		0
	    ))

(send toplevel-w :realize)

(send options-w :add_callback :XMN_ENTRY_CALLBACK ;use this instead of XmNsimpleCallback
      '(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET)
      '(
	(if CALLBACK_ENTRY_SET
	    ;; (send CALLBACK_ENTRY_WIDGET :name) returns "button_<#>"
	    ;; where <#> is 0 ... (button-count-1).
	    ;; we use 'read' to return the FIXNUM <#> after truncating the
	    ;; 7 chars "button_" from the front of the string.
	    (case (read (make-string-input-stream (send CALLBACK_ENTRY_WIDGET :name) 7))
		  (0
		   (print "option 0\n")
		   )
		  (1
		   (print "option 1\n")
		   )
		  (2
		   (print "option 2\n")
		   )
		  (3
		   (print "option 3\n")
		   )
		  (4
		   (print "option 4\n")
		   )
		  )
	  )
	))

