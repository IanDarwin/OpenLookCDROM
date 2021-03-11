; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wcls-fgcol.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/wcls-fgcol.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS -- dialog box for
;		selecting foreground color of a tango image object.
; Author:       Niels P. Mayer
; Created:      Mon Jun  6 04:24:32 1994
; Modified:     Mon Jun  6 04:25:24 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/get-colors")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS -- a subclass of XM_SELECTION_BOX_WIDGET_CLASS/:dialog
;;;
(setq XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS ;name of the new subclass
      (send Class :new
            '(tango_color)		;inst variables for subclass
            '()                         ;no class variables for subclass
            XM_SELECTION_BOX_WIDGET_CLASS ;name of the superclass
	    )) 
;;;
;;; Override instance initializer (method :isnew).
;;;
(send XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS :answer :isnew
      '(managed_k widget_name widget_parent
		  tango_w		;NOTE special extra widget creation arg, a tango:widget_class instance
		  help_w		;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
		  &rest args)
      '(
	(setq tango_color TANGO_COLOR_BLACK) ;init to default value

	;; create 'self', an instance of XM_SELECTION_BOX_WIDGET_CLASS/:dialog
	(apply #'send-super :isnew	;call superclass's init to create widget
	       :unmanaged		;don't manage till through twiddling geometry by unmanaging children
	       :dialog widget_name widget_parent
	       :XMN_DELETE_RESPONSE		:unmap
	       :XMN_AUTO_UNMANAGE		nil
	       :XMN_OK_LABEL_STRING		"Apply"	;since we don't automatically unmanage
	       :XMN_MUST_MATCH			t
	       :XMN_DIALOG_TITLE		"Select Foreground Color"
	       :XMN_SELECTION_LABEL_STRING	"Select Foreground Color"
	       :XMN_LIST_ITEMS			*X11-COLORS-LIST*
	       :XMN_LIST_ITEM_COUNT		(length *X11-COLORS-LIST*)
	       :XMN_LIST_VISIBLE_ITEM_COUNT	5
	       args
	       )
	(send (send-super :get_child :DIALOG_APPLY_BUTTON) :unmanage) ;don't need this...
	(if (eq :managed managed_k)
	    (send-super :manage)	;manage the parent only when finished removing children
	  )

	(send-super :add_callback :XMN_NO_MATCH_CALLBACK '()
	    '(
	      (X_BELL)			;SIGNAL ERROR -- BEEP
	      ))

	(send-super :add_callback :XMN_OK_CALLBACK '(CALLBACK_VALUE)
	    '(
	      (setq tango_color (send tango_w :load_color
				      (xm_string_get_l_to_r CALLBACK_VALUE)))
	      ))

	(send-super :add_callback :XMN_CANCEL_CALLBACK '()
	    '(
	      (send-super :unmanage)
	      ))

	(send-super :add_callback :XMN_HELP_CALLBACK '()
	    '(
	      (send help_w :error-display-string "Help not implemented... (sorry!).")
	      ))
	))

;;;
;;; Method :GET-TANGO-COLOR
;;;
(send XTANGO-FG-COLOR-SELECTION-WIDGET-CLASS :answer :GET-TANGO-COLOR
      '()
      '(
	tango_color
	)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/wcls-fgcol")
