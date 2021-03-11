;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         mng-test.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/mng-test.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Fooling around with managing and unmanaging widget arrays/lists
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:15:31 1989
; Modified:     Mon Jun  6 00:28:10 1994 (Niels Mayer) npm@indeed
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


(setq buts				;this gets set to an array of 100 pushbuttons
      (do* 
       (;; local vars
	(top_w (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
		     :XMN_GEOMETRY "500x500+0+0"
		     ))
	(rc_w (send XM_ROW_COLUMN_WIDGET_CLASS :new :unmanaged top_w
		    :XMN_ADJUST_LAST nil
		    ))
	(i 0 (1+ i))
	(num-buttons 100)
	(buttons (make-array num-buttons))
	)
       (;; test and return
	(= i num-buttons)
	(xt_manage_children buttons)
	(send rc_w :manage)
	(send top_w :realize)
	buttons				;return array of buttons
	)
       ;; body
       (setf (aref buttons i) 
	     (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed "name" rc_w
		   :XMN_LABEL_STRING (format nil "Button ~A" i)
		   ))
       (send (aref buttons i) :add_callback :XMN_ACTIVATE_CALLBACK
	     '()
	     `((format T "Hit Button ~A\n" ,i)
	       ))

       (setq *rowcol_w* rc_w)
       ))

(setq rc-children (send *rowcol_w* :get_children))
(setq num-children (length rc-children))
(progn buts)

(do ((i 0 (1+ i)))
    ((= i num-children) ":get_children ok...")
    (if (not (eq (aref buts i) (aref rc-children i)))
	(error "foo"))
    )


(setq buts2 (make-array 50))
(copy-array buts buts2)
(length buts)
(length buts2)

(xt_unmanage_children buts2)
(xt_manage_children buts2)

(xt_unmanage_children buts)
(xt_manage_children buts)

(xt_unmanage_children (send *rowcol_w* :get_children))
(xt_manage_children (send *rowcol_w* :get_children))

(setq butlist nil)
(dotimes (i 10)
	 (setq butlist 
	       (cons (send XM_PUSH_BUTTON_GADGET_CLASS :new :unmanaged "name" *rowcol_w*
			   :XMN_LABEL_STRING (format nil "Button ~A" (gensym))
			   )
		     butlist))
	 )

(xt_manage_children butlist)
(xt_unmanage_children butlist)

(xt_manage_children (cdr butlist))
(xt_unmanage_children (cdr butlist))
