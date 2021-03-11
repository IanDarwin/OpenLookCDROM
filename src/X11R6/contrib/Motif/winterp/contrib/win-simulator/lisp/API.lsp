; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         API.lsp
; RCS:          $Header: $
; Description:  Application Programmers Interface into WINTERP-based simulator
; Author:       Niels Mayer, HPLabs
; Created:      Fri Aug 28 11:03:54 1992
; Modified:     Sun Jun  5 16:30:26 1994 (Niels Mayer) npm@indeed
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
;; The following methods are all hooks into actions on the controlpanel
;; portion of the simulator. You're supposed to modify these and add your
;; code to all the HOOK methods below...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send Simulator_Control_Panel_Widget_Class :answer :SINGLE-STEP-HOOK
      '()
      '(
	;; add your code here!!
	))

(send Simulator_Control_Panel_Widget_Class :answer :MULTI-STEP-HOOK
      '(step-value-integer)
      '(
	;; add your code here!!
	))

(send Simulator_Control_Panel_Widget_Class :answer :RUN-HOOK
      '()
      '(
	;; add your code here!!
	))

(send Simulator_Control_Panel_Widget_Class :answer :INTERRUPT-HOOK
      '()
      '(
	;; add your code here!!
	))

(send Simulator_Control_Panel_Widget_Class :answer :SET-BREAKPOINT-HOOK
      '(brkpnt-line)
      '(
	;; add your code here!!
	))

(send Simulator_Control_Panel_Widget_Class :answer :CLEAR-BREAKPOINT-HOOK
      '(brkpnt-line)
      '(
	;; add your code here!!
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This hook gets called each time a register edit field is edited and
;; <return> is entered.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Simulator_Register_Field_Widget_Class :answer :REGISTER-CHANGED-HOOK
      '()
      '(
	;; add your code here
	
	;; example: (prints the edited register and contents to stdout)
	(format T "edited register-'~A'; value='~A'\n"
		index (send self :get_string))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following methods interface to the various register viewers.
;; For example, evaling
;; 	(send *sim* :get-integer-registers)
;; will result in a triplet list reflecting the state of the integer registers.
;;
;; Note, global variable *sim* is set to an instance of the sim. appl. widget
;; Simulator_Application_Widget_Class (see simulator.lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send Simulator_Application_Widget_Class :answer :GET-INTEGER-REGISTERS '()
      '(
	(if (not (and int-reg-popup-w (send int-reg-popup-w :exists_p)))
	    (error "int-reg-popup-w not initialized"))

	(send int-reg-popup-w :GET-REGISTERS)
	))

(send Simulator_Application_Widget_Class :answer :GET-PREDICATE-REGISTERS '()
      '(
	(if (not (and pred-reg-popup-w (send pred-reg-popup-w :exists_p)))
	    (error "pred-reg-popup-w not initialized"))

	(send pred-reg-popup-w :GET-REGISTERS)
	))

(send Simulator_Application_Widget_Class :answer :GET-FLOAT-REGISTERS '()
      '(
	(if (not (and float-reg-popup-w (send float-reg-popup-w :exists_p)))
	    (error "float-reg-popup-w not initialized"))

	(send float-reg-popup-w :GET-REGISTERS)
	))


(send Simulator_Application_Widget_Class :answer :SET-INTEGER-REGISTERS
      '(max-tick array-of-int array-of-tick)
      '(
	(if (not (and int-reg-popup-w (send int-reg-popup-w :exists_p)))
	    (error "int-reg-popup-w not initialized"))

	(send int-reg-popup-w :SET-REGISTERS max-tick array-of-int array-of-tick)
	))

(send Simulator_Application_Widget_Class :answer :SET-PREDICATE-REGISTERS
      '(max-tick array-of-predicate array-of-tick)
      '(
	(if (not (and pred-reg-popup-w (send pred-reg-popup-w :exists_p)))
	    (error "pred-reg-popup-w not initialized"))

	(send pred-reg-popup-w :SET-REGISTERS max-tick array-of-predicate array-of-tick)
	))

(send Simulator_Application_Widget_Class :answer :SET-FLOAT-REGISTERS
      '(max-tick array-of-float array-of-tick)
      '(
	(if (not (and float-reg-popup-w (send float-reg-popup-w :exists_p)))
	    (error "float-reg-popup-w not initialized"))

	(send float-reg-popup-w :SET-REGISTERS max-tick array-of-float array-of-tick)
	))

(send Simulator_Application_Widget_Class :answer :REDISPLAY-INTEGER-REGISTERS '()
      '(
	(if (not (and int-reg-popup-w (send int-reg-popup-w :exists_p)))
	    (error "int-reg-popup-w not initialized"))

	(send int-reg-popup-w :REDISPLAY)
	))

(send Simulator_Application_Widget_Class :answer :REDISPLAY-PREDICATE-REGISTERS '()
      '(
	(if (not (and pred-reg-popup-w (send pred-reg-popup-w :exists_p)))
	    (error "pred-reg-popup-w not initialized"))

	(send pred-reg-popup-w :REDISPLAY)
	))

(send Simulator_Application_Widget_Class :answer :REDISPLAY-FLOAT-REGISTERS '()
      '(
	(if (not (and float-reg-popup-w (send float-reg-popup-w :exists_p)))
	    (error "float-reg-popup-w not initialized"))

	(send float-reg-popup-w :REDISPLAY)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following methods provide interfaces to displays and such in the
;; controlpanel.
;;
;; For example, evaling
;; 	(send *sim* :set-file-label <filename-str>)
;; will set the filename label above the program viewer widget...
;;
;; Note, global variable *sim* is set to an instance of the sim. appl. widget
;; Simulator_Application_Widget_Class (see simulator.lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send Simulator_Application_Widget_Class :answer :VISIT-FILE
      '(filename-str)
      '(
	(send control-panel-w :set-file-label filename-str)
	(send program-viewer-w :find_file filename-str)
	))

(send Simulator_Application_Widget_Class :answer :SET-FILE-LABEL
      '(filename-str)
      '(
	(send control-panel-w :set-file-label filename-str)
	))

(send Simulator_Application_Widget_Class :answer :SET-CURTIME-LABEL
      '(time-str)
      '(
	(send control-panel-w :set-curtime-label time-str)
	))

(send Simulator_Application_Widget_Class :answer :SET-PC-LABEL
      '(pc-str)
      '(
	(send control-panel-w :set-pc-label pc-str)
	))

(send Simulator_Application_Widget_Class :answer :SET-SIM-STATUS-DISPLAY
      '(status)
      '(
	(send control-panel-w :set-sim-status-display status)
	))

(send Simulator_Application_Widget_Class :answer :GET-SIM-STATUS-DISPLAY
      '()
      '(
	(send control-panel-w :get-sim-status-display)
	))


(send Simulator_Application_Widget_Class :answer :SET-MEM-TRACE
      '(list-of-Mem_Trace_Item_Class)
      '(
	(if (not (and mem-trace-popup-w (send mem-trace-popup-w :exists_p)))
	    (error "mem-trace-popup-w not initialized"))

	(send mem-trace-popup-w :set-mem-trace list-of-Mem_Trace_Item_Class)
	))
