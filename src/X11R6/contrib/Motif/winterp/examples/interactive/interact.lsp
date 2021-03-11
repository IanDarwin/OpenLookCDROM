;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         interact.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/interact.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Play around with interactive features of winterp. 
;               GET_MOUSED_WIDGET allows you to send a message to any widget
;               that you can see. Thus you can interactively change your
;               interfaces' appearance or functionality without having to
;               remember the name  of the desired widget. Note that this'll
;               even work on big composite widgets that create other widgets
;               internally....
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:36:28 1989
; Modified:     Mon Jun  6 00:27:25 1994 (Niels Mayer) npm@indeed
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

(defun invert-moused-widget ()
  (let
      ((widget (get_moused_widget))
       foreground
       background
       )
    (send widget :get_values
	  :XMN_FOREGROUND 'foreground
	  :XMN_BACKGROUND 'background
	  )
    (send widget :set_values
	  :XMN_FOREGROUND background
	  :XMN_BACKGROUND foreground
	  )
    ))

(invert-moused-widget)

(send (get_moused_widget) :get_values
      :XMN_X nil
      :XMN_Y nil
      :XMN_HEIGHT nil
      :XMN_WIDTH nil
      )

(send (get_moused_widget) :set_values 
      :xmn_font_list "6x10")

(send  (GET_MOUSED_WIDGET) :destroy)

(get_moused_widget)

(send (GET_MOUSED_WIDGET) :set_values :xmn_foreground "red")
(send (GET_MOUSED_WIDGET) :set_values :xmn_foreground "white")
(send (GET_MOUSED_WIDGET) :set_values :xmn_foreground "blue")
(send (GET_MOUSED_WIDGET) :set_values :xmn_foreground "green")
(send (GET_MOUSED_WIDGET) :set_values :xmn_background "lightgrey")
(send (GET_MOUSED_WIDGET) :set_values :xmn_background "grey")
(send (GET_MOUSED_WIDGET) :set_values :xmn_background "dimgrey")

(send (get_moused_widget) :set_values
      :xmn_label_string (symbol-name (gensym))
      )

(require "rc-shell")			;need this for 'rc_w' below
(send (get_moused_widget) :set_callback :xmn_activate_callback
      '()
      '((send xm_push_button_gadget_class :new :managed "quackquack" rc_w)))

(send (get_moused_widget) :set_callback :xmn_activate_callback
      '(callback_reason callback_xevent)
      '(
	(format T "reason = ~A; event = ~A\n" callback_reason callback_xevent)
	)
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluate this and click on a widget to find out information about widget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let (
      (widget (get_moused_widget))
      height
      width
      )
  (send widget :get_values
      :xmn_height 'height
      :xmn_width 'width
      )
  (format T "\tparent=~A\n\twidget=~A\n\theight=~A\n\twidth=~A\n"
          (send widget :parent)
          widget
          height
          width)
  )
