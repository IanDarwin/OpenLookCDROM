;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         radiobox1.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/radiobox1.lsp,v 2.4 1994/06/06 14:43:06 npm Exp $
; Description:  The straighforward way to define a radio box.
;               See radiobox2.lsp for a better way using a WINTERP-subclassed
;               toggle-button. Just load this file to see the example.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:22:37 1989
; Modified:     Sun Jun  5 19:14:42 1994 (Niels Mayer) npm@indeed
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
	    :XMN_TITLE		"WINTERP: Radio-Box-Test #1"
	    :XMN_ICON_NAME	"W:radiobox1"
	    ))

(setq rowcol_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :radio_box "rc" toplevel_w
	    ))

(send toplevel_w :realize)

(do* 
 (;; local vars
  (i 0 (1+ i))
  but_w
  (buttons nil)
  )
 (;; test and return
  (eql i 20)
  )
 ;; body
 (setq but_w
       (send XM_TOGGLE_BUTTON_GADGET_CLASS :new :managed rowcol_w
	     :XMN_LABEL_STRING (format nil "Button ~A" i)
	     ))
 (send but_w :set_callback :xmn_value_changed_callback
       '(callback_widget callback_set)
       '((if callback_set
	     (print_set_button callback_widget buttons)
	   )))
 (setq buttons (cons (cons but_w i) buttons))
 )


(defun print_set_button (widget buttons)
  (format T "Selected button ~A\n" (cdr (assoc widget buttons)))
  )
