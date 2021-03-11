; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         wc_sim-messagearea.lsp
; RCS:          $Header: $
; Description:  Simulator_Message_Area_Widget_Class & Methods
; Author:       Niels Mayer, HPLabs.
; Created:      Wed Aug 26 14:39:19 1992
; Modified:     Sun Jun  5 16:33:24 1994 (Niels Mayer) npm@indeed
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

(defvar *MESSAGE-AREA-MESSAGE-HOLD-TIME* 2000) ;hold for 2000 milliseconds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE Simulator_Message_Area_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Simulator_Message_Area_Widget_Class
  (send Class :new
	'(				;new instance vars
	  clear?			;whether to clear last displayed msg
	  displayed-warning-count
	  ) 
	'()				;no class vars
	XM_TEXT_WIDGET_CLASS		;superclass
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION METHOD FOR Simulator_Message_Area_Widget_Class
;;
;; (send Simulator_Message_Area_Widget_Class :new {:managed,:unmanaged}
;;       <name-str> <parent-w>
;;	 [[resource-name resource-value] ...]
;;  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Message_Area_Widget_Class :answer :ISNEW
      '(managed-kwd name-str parent-w &rest args)
      '(
	(apply 'send-super
	       `(:isnew ,managed-kwd	;splice in method arguments passed in above
			,name-str ,parent-w
			;; XmText resources
			:XMN_STRING			" "
			:XMN_AUTO_SHOW_CURSOR_POSITION	nil
			:XMN_CURSOR_POSITION_VISIBLE	nil
			:XMN_EDITABLE			nil
			:XMN_RESIZE_WIDTH		nil
			;; in conjuction with TopLevelShell's resource setting
			;; XmNallowShellResize==true, this allows for multiline
			;; warnings to be temporarily displayed by increasing
			;; size of top level window...
			:XMN_RESIZE_HEIGHT		t
			,@args
			))
	(setq clear? NIL)
	(setq displayed-warning-count 0)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :WARNING 
;;
;; (send <msgarea-w> :warning <message-str> [:LEAVE-DISPLAYED {T,NIL}])
;;
;; -- Displays <message-str> in message area...
;; -- Add keyword ':LEAVE-DISPLAYED T' then doesn't automatically clear message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Message_Area_Widget_Class :answer :WARNING
      '(msg &key leave-displayed)
      '(
	(send self :set_string msg)
;;; :XMN_RESIZABLE		t
	(send self :update_display)

	(if leave-displayed
	    (progn
;;;           (setq displayed-warning-count (1+ displayed-warning-count))
	      (setq clear? NIL)
	      )
	  (progn
	    (setq clear? T)
	    (setq displayed-warning-count (1+ displayed-warning-count))
	    (xt_add_timeout *MESSAGE-AREA-MESSAGE-HOLD-TIME*
			    `(
			      (send ,self :warning-clear)
			      ))
	    )
	  )

	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHOD :WARNING-CLEAR
;;
;; (send <msgarea-w> :warning-clear [:reset {T,NIL}])
;; 
;; -- erases the current warning message if a new warning has
;;    not been displayed replacing the first one
;; -- Add keyword ':RESET T' to force clearing of warning message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Message_Area_Widget_Class :answer :WARNING-CLEAR
      '(&key reset)
      '(
	(cond
	 (reset
	  (send self :set_string "")
	  (setq displayed-warning-count 0)
	  )
	 ((and clear? (> displayed-warning-count 1))
	  ;; decrement the count of displayed messages to be cleared
	  (setq displayed-warning-count (1- displayed-warning-count))
	  )
	 ((and clear? (= displayed-warning-count 1))
	  (send self :set_string "")
	  (setq displayed-warning-count 0)
	  )
	 )
	))
