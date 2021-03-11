; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         timesheet.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/timesheet.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Project timesheet application. You probably want to modify this
;		file to create a project timesheet for your own organization --
;		the named entries in this timesheet example are bogus...
;		This example makes use of the timechart widgets in 
;		lib-widgets/timechart.lsp.
;
;		To start the chart application "standalone", do
;		"env WINTERP_STANDALONE_APP=TRUE winterp -init_file chart.lsp -no_stdin_serv -no_unix_serv"
;		doing so will cause WINTERP to terminate when the main
;		chart window is closed, rather than just deleting the window.
; Author:       Niels P. Mayer (based on example by Salil Deshpande @ EIT)
; Created:      Thu Apr 29 00:15:13 1993
; Modified:     Sun Jun  5 19:19:18 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define decide-winterp-top-level-shell and other unixisms...
(require "lib-widgets/timechart")	;define Interactive_Chart_Widget_Class and Interactive_Chart_Elt_Widget_Class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Project_Class nil)
(setq Project_Class
      (send Class :new
	    '(				;new instance vars
	      proj-id-fixnum		;fixnum assoc'd w/ project
	      proj-desc-str		;string describing project
	      )
	    '()				;no class vars
	    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Project_Class :answer :ISNEW
      '(id-fixnum desc-str)
      '(
	;; initialize the instance variables
	(setq proj-id-fixnum id-fixnum)
	(setq proj-desc-str desc-str)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :GET_STRING -- return string description of project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Project_Class :answer :GET_STRING
      '()
      '(
	(format nil "~A ~A" proj-id-fixnum proj-desc-str)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :PRINT -- prints the name of the project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Project_Class :answer :PRINT
      '()
      '(
	(format T "~A" proj-id-fixnum)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create the application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let*
    (
     (toplevel_w
      (send (decide-winterp-top-level-shell) :new
	    "timeshtShl"
	    :XMN_TITLE			"WINTERP: Timesheet (Timechart Widget Demo)"
	    :XMN_ICON_NAME		"W:timesheet"
	    ))
     (form_w
      (send XM_FORM_WIDGET_CLASS :new :managed
	    "form" toplevel_w 
	    ))
     (cmd_w
      (send XM_FORM_WIDGET_CLASS :new :managed
	    "commands" form_w
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_FRACTION_BASE		2
	    ))
     (submit_button_w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "submit_button" cmd_w 
	    :XMN_LABEL_STRING		"Submit"
	    :XMN_ALIGNMENT		:alignment_center
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_position
	    :XMN_RIGHT_POSITION		1
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
	    ))
     (total_label_w
      (send XM_LABEL_GADGET_CLASS :new :managed
	    "total_label" cmd_w
	    :XMN_LABEL_STRING		"Today's Total:"
	    :XMN_ALIGNMENT		:alignment_end
	    :XMN_LEFT_ATTACHMENT	:attach_position
	    :XMN_LEFT_POSITION		1
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
	    ))
     (value_label_w
      (send XM_LABEL_WIDGET_CLASS :new :managed
	    "value_label" cmd_w
	    :XMN_LABEL_STRING		(format nil "~4,1F" 0.0)
	    :XMN_ALIGNMENT		:alignment_beginning
	    :XMN_LEFT_ATTACHMENT	:attach_widget
	    :XMN_LEFT_WIDGET		total_label_w
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_TOP_ATTACHMENT		:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
	    ))
     (scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
 	    "scrl" form_w
	    :XMN_HEIGHT			600
	    :XMN_WIDTH		        300
 	    :XMN_SCROLLING_POLICY	:automatic
	    :XMN_TOP_ATTACHMENT		:attach_widget
	    :XMN_TOP_WIDGET		cmd_w
	    :XMN_LEFT_ATTACHMENT	:attach_form
	    :XMN_RIGHT_ATTACHMENT	:attach_form
	    :XMN_BOTTOM_ATTACHMENT	:attach_form
 	    ))
     (timesht_w
      (send Interactive_Chart_Widget_Class :new :managed
	    "timesht" scrl_w
	    ))
     )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Add Projects to the Interactive_Chart_Widget instance
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (send timesht_w :create_chart
	(vector
	 (send Project_Class :new 0001 "MS Dos Advocacy")
	 (send Project_Class :new 0002 "MS Windows Advocacy")
	 (send Project_Class :new 0003 "Lies and Deceit")
	 (send Project_Class :new 0004 "Fear Uncertainty and Doubt")
	 (send Project_Class :new 0005 "Aspersions Against Comptetitors")
	 (send Project_Class :new 0006 "Artful Schedule Frobbing")
	 (send Project_Class :new 0007 "Contract Overcharges")
	 (send Project_Class :new 0008 "Travel Time Inflation")
	 (send Project_Class :new 0010 "Indirect Costs")
	 (send Project_Class :new 0011 "Lawyer Jokes")
	 (send Project_Class :new 0012 "Conspiracy Design")
	 (send Project_Class :new 0013 "Monopoly Design")
	 (send Project_Class :new 0014 "Management Consulting")
	 (send Project_Class :new 0014 "Engineering Consulting")
	 ))

  ;; The "lambda" closure below gets called whenever a slider changes value
  (send timesht_w :set-value-changed-callback-closure
	(lambda ()
	  (send value_label_w :set_values
		:XMN_LABEL_STRING (format nil "~4,1F" (send timesht_w :get_total)))
	  ))

  (send submit_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	'()
	'(
	  (send timesht_w :print)
	  (if (winterp-standalone-p) 
	      (exit)
	    )
	  (send toplevel_w :destroy)
	  ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Realize Widgets to find out real sizes, then diddle constraints... ;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (send toplevel_w :realize)
  )

