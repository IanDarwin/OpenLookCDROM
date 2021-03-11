; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-register-field.lsp
; RCS:          $Header: $
; Description:  Simulator_Register_Field_Widget_Class and Methods
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 07:11:49 1992
; Modified:     Sun Jun  5 16:30:52 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       Experimental (Do Not Distribute)
;
; WINTERP Copyright 1989-1992 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and Niels Mayer not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and Niels Mayer
; makes no representations about the suitability of this software for any
; purpose.  It is provided "as is" without express or implied warranty.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE Simulator_Register_Field_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Register_Field_Widget_Class
  (send Class :new
	'(				;new instance vars
	  index				;FIXNUM position of this reg in array.
	  last-update-tick		;fixnum...
	  value
	  ) 
	'()				;no class vars
	XM_TEXT_FIELD_WIDGET_CLASS	;superclass
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Register_Field_Widget_Class
;;
;; (send Simulator_Register_Field_Widget_Class :new {:managed,:unmanaged}
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w idx &rest args)
      '(
	(apply 'send-super
	       `(:isnew ,managed-kwd	;splice in method arguments passed in above
			,name-str ,parent-w
			,@args
			))

	(setq index idx)
	(setq last-update-tick -9999)
	(setq value 0)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-MRU-COLOR
;;
;; sets color of the register-field-widget to show most recently used register.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :SET-MRU-COLOR
      '(tick)
      `(
	(send-super :set_values
		    :XMN_BACKGROUND
		    (aref ,*reg-colorlist*
			  (min
			   (- tick last-update-tick)
			   ,*reg-colorlist-maxidx*))
		    )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-LAST-UPDATE-TICK
;;
;; sets count of last updated field so that most recently used register coloring
;; works...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :SET-LAST-UPDATE-TICK
      '(t0)
      '(
	(setq last-update-tick t0)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :SET-REGISTER
;;
;; (send <reg-f-w> :set-register <int> <tick>)
;;
;; sets count of last updated field so that most recently used register coloring
;; works...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :SET-REGISTER
      '(int tick)
      '(
	(setq value int)
	(setq last-update-tick tick)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :REDISPLAY
;;
;; (send <reg-f-w> :redisplay <latest-tick>)
;;
;; sets count of last updated field so that most recently used register coloring
;; works...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :REDISPLAY
      '(latest-tick)
      `(
	(send-super :set_values
		    :XMN_STRING (format nil "~A" value)
		    :XMN_BACKGROUND
		    (aref ,*reg-colorlist*
			  (min
			   (- latest-tick last-update-tick)
			   ,*reg-colorlist-maxidx*))
		    )

	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :GET-VALUE
;;
;; (send <reg-f-w> :get-value)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :GET-VALUE
      '()
      '(
	value
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :GET-TICK
;;
;; (send <reg-f-w> :get-tick)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :GET-TICK
      '()
      '(
	last-update-tick
	))
