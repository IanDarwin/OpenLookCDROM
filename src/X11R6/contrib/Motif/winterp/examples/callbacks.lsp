;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         callbacks.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/callbacks.lsp,v 2.4 1994/06/06 14:43:20 npm Exp $
; Description:  Demonstrates using callbacks and timeouts. Just
;		load this file and click on the "start" or "stop" button...
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:01:08 1989
; Modified:     Sun Jun  5 18:29:24 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TEST CALLBACKS AND TIMEOUTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-date ()
  (let*
      ((pipe (popen "date" :direction :input))
       (str (read-line pipe))
       )
    (pclose pipe)
    str))

(defun make-rc-shell ()
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	      :XMN_GEOMETRY	"500x500+0+0"
	      :XMN_TITLE	(get-date)
	      :XMN_ICON_NAME	"W:callbacks"
	      ))
  (setq rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed top_w
	      :XMN_ADJUST_LAST nil
	      ))
  (send top_w :realize)
  )

(make-rc-shell)

(setq start_but_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
	    "start" rc_w 
;;;	    :XMN_FOREGROUND "green"
;;;	    :XMN_BACKGROUND "black"
	    ))

(send start_but_w :has_callbacks :XMN_ARM_CALLBACK)
(send start_but_w :has_callbacks :XMN_DISARM_CALLBACK)
(send start_but_w :has_callbacks :XMN_ACTIVATE_CALLBACK)

(send start_but_w :set_callback :XMN_ACTIVATE_CALLBACK '()
      `((setq to 
	      (xt_add_timeout
	       1000 
	       '((send ,XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
		       (get-date) ,rc_w
;;;		       :XMN_BACKGROUND "magenta"
		       )
		 (setq to (xt_add_timeout 1000 TIMEOUT_OBJ))
		 )
	       ))
	))


(send start_but_w :has_callbacks :XMN_ARM_CALLBACK)
(send start_but_w :has_callbacks :XMN_DISARM_CALLBACK)
(send start_but_w :has_callbacks :XMN_ACTIVATE_CALLBACK)

(setq stop_but_w 
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed 
	    "stop" rc_w 
;;;	    :XMN_FOREGROUND "red"
;;;	    :XMN_BACKGROUND "black"
	    ))

(send stop_but_w :has_callbacks :XMN_ARM_CALLBACK)
(send stop_but_w :has_callbacks :XMN_DISARM_CALLBACK)
(send stop_but_w :has_callbacks :XMN_ACTIVATE_CALLBACK)

(send stop_but_w :set_callback :XMN_ACTIVATE_CALLBACK '()
      '(
	(xt_remove_timeout to)
	(format t "quack\n")
	))

(send stop_but_w :has_callbacks :XMN_ARM_CALLBACK) ;==>CALLBACK_HAS_NONE
(send stop_but_w :has_callbacks :XMN_DISARM_CALLBACK) ;==>CALLBACK_HAS_NONE
(send stop_but_w :has_callbacks :XMN_ACTIVATE_CALLBACK)	;==>CALLBACK_HAS_SOME
(send stop_but_w :has_callbacks :XMN_APPLY_CALLBACK) ;==>CALLBACK_NO_LIST
