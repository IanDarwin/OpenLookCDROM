;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         radiobox2.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/radiobox2.lsp,v 2.4 1994/06/06 14:43:06 npm Exp $
; Description:  A better (?) way of creating a radio box, using subclassing of
;               togglebutton. Note that this version doesn't waste as much
;               memory as radiobox1.lsp because it defines a single
;               entry-callback on the rowcolumn widget instead of forcing each
;               toggle-button to have separate copies of very similar
;               callback-closures. Just load this file to see the example.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:24:00 1989
; Modified:     Sun Jun  5 19:14:58 1994 (Niels Mayer) npm@indeed
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    :XMN_GEOMETRY	"500x500+1+1"
	    :XMN_TITLE		"WINTERP: Radio-Box-Test #2"
	    :XMN_ICON_NAME	"W:radiobox2"
	    ))

(setq rowcol_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :radio_box "rc" toplevel_w
	    ))

(send toplevel_w :realize)

(send rowcol_w :set_callback :xmn_entry_callback
       '(CALLBACK_ENTRY_WIDGET
	 CALLBACK_ENTRY_SET)
       '(
	 (if CALLBACK_ENTRY_SET
	     (send CALLBACK_ENTRY_WIDGET :print-which-button)
	   )
	 ))

;; make a subclass of XM_TOGGLE_BUTTON_GADGET_CLASS
(setq My_Toggle_Button			
      (send Class :new
	    '(button_name)		;a new ivar for this subclass
	    '()				;no class variables for subclass
	    XM_TOGGLE_BUTTON_GADGET_CLASS)) 

;; override XM_TOGGLE_BUTTON_GADGET_CLASS's instance initializer
(send My_Toggle_Button :answer :ISNEW
      '(name managed_k widget_name widget_parent &rest args)
      '(
	(setq button_name name)
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k widget_name widget_parent
	       :XMN_LABEL_STRING name
	       args)
	))

;; add a method that prints which button
(send My_Toggle_Button :answer :print-which-button '()
      '(
	(format T "option ~A selected\n" button_name)
	))


(do* 
 (;; local vars
  (i 0 (1+ i))
  )
 (;; test and return
  (eql i 20)
  )
 ;; body
 (send My_Toggle_Button :new (format nil "Button ~A" i)
       :managed "tb" rowcol_w)
 )
