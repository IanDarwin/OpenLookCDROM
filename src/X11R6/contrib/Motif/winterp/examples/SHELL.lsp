;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         SHELL.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/SHELL.lsp,v 2.4 1994/06/06 14:43:26 npm Exp $
; Description:  tests out the following classes and methods on those classes
;		OVERRIDE_SHELL_WIDGET_CLASS, TRANSIENT_SHELL_WIDGET_CLASS,
;		TOP_LEVEL_SHELL_WIDGET_CLASS, APPLICATION_SHELL_WIDGET_CLASS,
;		TOP_LEVEL_POPUP_SHELL_WIDGET_CLASS,
;		APPLICATION_POPUP_SHELL_WIDGET_CLASS,
;		OVERRIDE_POPUP_SHELL_WIDGET_CLASS,
;		TRANSIENT_POPUP_SHELL_WIDGET_CLASS,
;		XM_DIALOG_POPUP_SHELL_WIDGET_CLASS. Just load this file to see
;		examples.
; Author:       Niels Mayer
; Created:      Sun Feb 10 20:34:10 1991
; Modified:     Sun Jun  5 18:10:37 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*

(if *MOTIF-1.1-OR-LATER-P*
    (print (send *TOPLEVEL_WIDGET* :get_argv)) ;get_argv has problems in Motif 1.0 when no args supplied at WINTERP startup time.
  )

(send *TOPLEVEL_WIDGET* :set_argv #("foo" "bar" "baz" "jimmy" "hat" "in" "the" "mix"))

(print (send *TOPLEVEL_WIDGET* :get_argv))


(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "top"
	    :XMN_GEOMETRY "=200x50+0+0"
	    ))
(send top_w :realize)
(send top_w :IS_MOTIF_WM_RUNNING)
(xt_add_timeout 10000
		`(
		  (send ,top_w :unrealize)
		  ))

(setq app_w 
      (send APPLICATION_SHELL_WIDGET_CLASS :new "app"
	    :XMN_GEOMETRY "=200x50+100+100"
	    ))
(send app_w :set_argv #("foo" "bar" "baz" "jimmy" "hat" "in" "the" "mix"))
(send app_w :realize)
(send app_w :IS_MOTIF_WM_RUNNING)
(print (send app_w :get_argv))
(xt_add_timeout 12000
		`(
		  (send ,app_w :unrealize)
		  ))

(cond (*MOTIF-1.1-OR-LATER-P*
       (setq ov_w
	     (send OVERRIDE_SHELL_WIDGET_CLASS :new "override"
		   :XMN_GEOMETRY "=200x50+200+200"
		   ))
       (send ov_w :realize)
       (send ov_w :IS_MOTIF_WM_RUNNING)
       (xt_add_timeout 14000
		       `(
			 (send ,ov_w :unrealize)
			 ))
       ))

(setq tx_w
      (send TRANSIENT_SHELL_WIDGET_CLASS :new "transient"
	    :XMN_GEOMETRY "=200x50+300+300"
	    ))
(send tx_w :realize)
(send tx_w :IS_MOTIF_WM_RUNNING)
(xt_add_timeout 16000
		`(
		  (send ,tx_w :unrealize)
		  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (setq top_popup_w 
	(send TOP_LEVEL_POPUP_SHELL_WIDGET_CLASS :new "top_popup" top_w
	      :XMN_GEOMETRY "=200x50+50+50"
	      ))
  (send top_popup_w :IS_MOTIF_WM_RUNNING)

  (setq app_popup_w
	(send APPLICATION_POPUP_SHELL_WIDGET_CLASS :new "app_popup" top_w
	      :XMN_GEOMETRY "=200x50+150+150"
	      ))
  (send app_popup_w :IS_MOTIF_WM_RUNNING)

  (cond (*MOTIF-1.1-OR-LATER-P*
	 (setq ov_popup_w
	       (send OVERRIDE_POPUP_SHELL_WIDGET_CLASS :new "override_popup" top_w
		     :XMN_GEOMETRY "=200x50+250+250"
		     ))
	 (send ov_popup_w :IS_MOTIF_WM_RUNNING)
	 ))

  (setq tx_popup_w
	(send TRANSIENT_POPUP_SHELL_WIDGET_CLASS :new "transient_popup" top_w
	      :XMN_GEOMETRY "=200x50+350+350"
	      ))
  (send tx_popup_w :IS_MOTIF_WM_RUNNING)

  (setq xmdia_popup_w
	(send XM_DIALOG_POPUP_SHELL_WIDGET_CLASS :new "dialog_popup" top_w
	      :XMN_GEOMETRY "=200x50+400+400"
	      ))
  (send xmdia_popup_w :IS_MOTIF_WM_RUNNING)

  ;; (setq xmnu_popup_w
  ;;(send XM_MENU_POPUP_SHELL_WIDGET_CLASS :new "menu_popup" top_w
  ;; 	    :XMN_GEOMETRY "=200x50+450+450"
  ;; 	    ))
  ;; 
  ;; (send xmnu_popup_w  :popup :grab_exclusive)
  ;; (send xmnu_popup_w :manage)


  (xt_add_timeout 1000 
		  `(
		    (send ,top_popup_w :popup :grab_none)
		    ))
  (xt_add_timeout 6000 
		  `(
		    (send ,top_popup_w :popdown)
		    ))

  (xt_add_timeout 2000 
		  `(
		    (send ,app_popup_w :popup :grab_none)
		    ))
  (xt_add_timeout 7000 
		  `(
		    (send ,app_popup_w :popdown)
		    ))

  (cond (*MOTIF-1.1-OR-LATER-P*
	 (xt_add_timeout 3000 
			 `(
			   (send ,ov_popup_w :popup :grab_none)
			   ))
	 (xt_add_timeout 8000 
			 `(
			   (send ,ov_popup_w :popdown)
			   ))
	 ))

  (xt_add_timeout 4000 
		  `(
		    (send ,tx_popup_w :popup :grab_none)
		    ))
  (xt_add_timeout 9000 
		  `(
		    (send ,tx_popup_w :popdown)
		    ))

  (xt_add_timeout 5000 
		  `(
		    (send ,xmdia_popup_w :manage)
		    ))
  (xt_add_timeout 10000 
		  `(
		    (send ,xmdia_popup_w :unmanage)
		    ))
  )
