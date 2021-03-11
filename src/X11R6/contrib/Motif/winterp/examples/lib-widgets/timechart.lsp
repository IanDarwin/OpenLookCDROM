; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         timechart.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/timechart.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Interactive_Chart_Widget_Class, and
;		Interactive_Chart_Elt_Widget_Class used by ../timesheet.lsp
; Author:       Niels P. Mayer (based on example by Salil Deshpande @ EIT)
; Created:      Thu Apr 29 00:15:26 1993
; Modified:     Mon Jun  6 01:10:19 1994 (Niels Mayer) npm@indeed
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

(defvar Interactive_Chart_Widget_Class nil)
(setq Interactive_Chart_Widget_Class
      (send Class :new
	    '(				;new instance vars
	      ivar-value-changed-callproc ;called whenever slider values changes -- closure is passed by user via :SET-VALUE-CHANGED-CALLBACK-CLOSURE
	      ivar-chart-widgetelt-list	;list of Interactive_Chart_Elt_Widget_Class instances
	      )
	    '()				;no class vars
	    XM_ROW_COLUMN_WIDGET_CLASS)) ;superclass

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Widget_Class :answer :ISNEW
      '(managed_k name parent_w &rest args)
      '(
	;; initialize the instance variables
	(setq ivar-value-changed-callproc	NIL
	      ivar-chart-widgetelt-list		NIL
	      )

	;; initialize the XM_ROW_COLUMN_WIDGET_CLASS superclass widget...
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k name parent_w
	       :XMN_ORIENTATION		:vertical
	       :XMN_PACKING		:pack_column
	       :XMN_NUM_COLUMNS		1
	       :XMN_ENTRY_ALIGNMENT	:alignment_beginning
	       :XMN_ADJUST_LAST		nil
	       :XMN_ALLOW_RESIZE	nil
	       :XMN_MARGIN_HEIGHT	0
	       :XMN_MARGIN_WIDTH	0
	       :XMN_ENTRY_BORDER	0
	       :XMN_SPACING		0
	       args)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send chart-w :create_chart '( <obj-1> ... <obj-n> ))
;; where <obj-i> is an object with following protocol
;;	method :get_string	-- returns string name of object
;;	method :print 		-- prints the object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Widget_Class :answer :CREATE_CHART
      '(sequence-of-chart-items)
      '(
	;; if chart previously created, destroy old ones, create new one
	(if ivar-chart-widgetelt-list
	    (progn
	      (map nil 
		   #'(lambda (widgetelt)
		       (send widgetelt :destroy))
		   ivar-chart-widgetelt-list)
	      (setq ivar-chart-widgetelt-list '())
	      ))

	;; create new chart...
	(map nil
	     #'(lambda (chart-item)
		 (setq ivar-chart-widgetelt-list
		       (cons
			(send Interactive_Chart_Elt_Widget_Class :new :managed 
			      "chart-elt" ;widget-name of all chart elts is the same...
			      self	;parent is the chart widget itself
			      chart-item ;pass in the chart-item element
			      )
			ivar-chart-widgetelt-list))
		 )
	     sequence-of-chart-items)
	(setq ivar-chart-widgetelt-list (reverse ivar-chart-widgetelt-list))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Widget_Class :answer :GET_TOTAL
      '()
      '(
	(apply #'+
	       (map 'list
		    #'(lambda (widgetelt)
			(send widgetelt :get_value)
			)
		    ivar-chart-widgetelt-list
		    ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Widget_Class :answer :PRINT
      '()
      '(
	(map nil
	     #'(lambda (widgetelt)
		 (send widgetelt :print)
		 )
	     ivar-chart-widgetelt-list
	     )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pass in code to execute (closure) when a slider is moved.
;; (send chart-w :set-value-changed-callback-closure
;; 	(lambda ()
;; 	  ;;;USER CODE EXECUTED IN SCOPE IN WHICH THIS METHOD CALLED...
;; 	  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Widget_Class :answer :SET-VALUE-CHANGED-CALLBACK-CLOSURE
      '(callproc)
      '(
	(setq ivar-value-changed-callproc callproc)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIVATE: executes the value changed hook s-exprs previously set via
;; :SET-VALUE-CHANGED-CALLBACK-CLOSURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Widget_Class :answer :_DO_VALUE_CHANGED_HOOK
      '()
      '(
	(if ivar-value-changed-callproc
	    (funcall ivar-value-changed-callproc))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar Interactive_Chart_Elt_Widget_Class nil)
(setq Interactive_Chart_Elt_Widget_Class
      (send Class :new
	    '(				;new instance vars
	      ivar-chart-item		;object supporting methods :get_string and :print
	      )
	    '()				;no class vars
	    XM_SCALE_WIDGET_CLASS))	;superclass


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Elt_Widget_Class :answer :ISNEW
      '(managed_k name parent_w item &rest args)
      '(
	;; initialize the instance variables
	(setq ivar-chart-item item)

	;; initialize the XM_SCALE_WIDGET_CLASS widget...
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k name parent_w
	       :XMN_TITLE_STRING	(send item :get_string)
	       :XMN_SENSITIVE		t
	       :XMN_SHOW_VALUE		t
	       :XMN_ORIENTATION		:HORIZONTAL
	       :XMN_MAXIMUM		100
	       :XMN_DECIMAL_POINTS	1
	       args)
      
	;; add callbacks
	(send-super :add_callback :XMN_VALUE_CHANGED_CALLBACK
		    '()
		    `(
		      (send ,parent_w :_do_value_changed_hook)
		      ))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :GET_VALUE -- returns a flonum between 0.0 and 10.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Elt_Widget_Class :answer :GET_VALUE
      '()
      '(
	(/ (send-super :get_value) 10.0)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :PRINT -- prints out whatever method :print on the ivar-chart-item, followed
;; by the value of this widget...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Interactive_Chart_Elt_Widget_Class :answer :PRINT
      '()
      '(
	(send ivar-chart-item :print)
	(format T " ~4,1F\n" (send self :get_value))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/timechart")
