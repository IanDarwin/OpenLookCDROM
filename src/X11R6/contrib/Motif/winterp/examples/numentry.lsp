; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         numentry.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/numentry.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Test of Cardinal_Number_Entry_Field_Widget_Class
; Author:       Niels P. Mayer
; Created:      Sun Apr 17 20:50:52 1994 (Niels Mayer) npm@indeed
; Modified:     Sun Jun  5 19:09:59 1994 (Niels Mayer) npm@indeed
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

(require "lib-widgets/numentry")	;define Cardinal_Number_Entry_Field_Widget_Class

(let*
    ((toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    :XMN_WIDTH		500
	    :XMN_HEIGHT		500
	    :XMN_TITLE		"WINTERP: numentry test"
	    :XMN_ICON_NAME	"W:numentry"
	    ))
     (scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	    "sc" toplevel_w
	    :XMN_SCROLLING_POLICY	:automatic
	    ))
     (rc_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "rc" scrl_w
	    :XMN_ORIENTATION		:vertical
	    :XMN_PACKING		:pack_tight
	    :XMN_ENTRY_ALIGNMENT	:alignment_center
	    )))

  (dotimes
   (i 20)
   (let ((row_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		"row" rc_w
		:XMN_PACKING	:pack_tight
		:XMN_ORIENTATION	:horizontal
		)))
     (let* ((numchildren	    4)
	    (numentry-children (make-array numchildren))
	    (label-children    (make-array numchildren))
	    )
       (dotimes (j numchildren)
		(setf (aref label-children j)
		      (send XM_LABEL_GADGET_CLASS :new :unmanaged
			    "tl" row_w
			    :XMN_LABEL_TYPE   :string
			    :XMN_LABEL_STRING (format nil "~2,,,'0@A,~2,,,'0@A" i j)
			    ))
		(setf (aref numentry-children j)
		      (send Cardinal_Number_Entry_Field_Widget_Class :new :unmanaged
			    "tf" row_w
			    :XMN_COLUMNS 4
			    ))
		(send (aref numentry-children j) :add_callback :XMN_ACTIVATE_CALLBACK
		      '(CALLBACK_WIDGET)
		      `(
			(let ((val (send CALLBACK_WIDGET :get_value)))
			  (if val
			      (format T "value at ~2,,,'0@A,~2,,,'0@A == ~A\n"
				      ,i ,j val))
			  )))
		)
       (xt_manage_children numentry-children)
       (xt_manage_children label-children)
       )
     )
   )
  (send toplevel_w :realize)
  )


