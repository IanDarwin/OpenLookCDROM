; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-gauge.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-gauge.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Test/demo of GAUGE-WIDGET-CLASS, defined in wcls-gauge.lsp.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 04:06:10 1994 (Niels Mayer) npm@indeed
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

(require "xtango/wcls-gauge")

(setq *gauge-size-fixnum* 20)

(progn
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	      "test-gauge"
	      :XMN_TITLE		"WINTERP: Xtango Gauge Widgets Test"
	      :XMN_ICON_NAME		"W:test-gauge"
	      ))

  (setq rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" top_w
	      :XMN_NUM_COLUMNS		5
	      :XMN_ORIENTATION		:horizontal
	      :XMN_PACKING		:pack_column
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      ))
  (setq gauges-array (make-array *gauge-size-fixnum*))

  (dotimes (i *gauge-size-fixnum*)
	   (setf (aref gauges-array i)
		 (send GAUGE-WIDGET-CLASS :new :managed
		       "tango" rc_w
		       :XMN_HEIGHT		40
		       :XMN_WIDTH		40
		       ))
	   )

  (send
   (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	 "clear" rc_w
	 :XMN_LABEL_STRING "Clear"
	 )
   :ADD_CALLBACK :XMN_ACTIVATE_CALLBACK '()
   '(
     (map nil
	  #'(lambda (x) (send x :SET-VALUE 0.0))
	  gauges-array)
     ))

  (send
   (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	 "random" rc_w
	 :XMN_LABEL_STRING "Random"
	 )
   :ADD_CALLBACK :XMN_ACTIVATE_CALLBACK '()
   '(
     (map nil
	  #'(lambda (x)
	      (send x :SET-VALUE (/ (random 1000) 1000)))
	  gauges-array)
     ))

  (send
   (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	 "print" rc_w
	 :XMN_LABEL_STRING "Print"
	 )
   :ADD_CALLBACK :XMN_ACTIVATE_CALLBACK '()
   '(
     (map nil
	  #'(lambda (x)
	      (format T "gauge: ~A\n" (send x :GET-VALUE)))
	  gauges-array)
     (fflush *standard-output*)
     ))

  (send top_w :realize)
  )


;;; (send meter_w :SET_COORD 0.0 1.0 1.0 0.0)
;;; (send meter_w :pan :down 0.1)

;;; (send meter_w :set-value 0.5)
;;; (send meter_w :get-value)
