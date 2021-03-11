; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         hanoi.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/hanoi.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Animated "towers of hanoi" algorithm.
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 03:00:22 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
; Package:      N/A
; Status:       X11r6 contrib release
; 
; Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
; WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
; WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
; 
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Enterprise Integration Technologies,
; Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Enterprise Integration
; Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
; Betz make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
; 
; ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
; LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
; SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
; IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
; COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
; INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NPM -- the code in here once originated from XLISP-PLUS 2.1d hanoi.lsp...
;; Translation to Xtango graphics by NPM.

; Good ol towers of hanoi
;
; Usage:
;      (hanoi <n>)
;          <n> - an integer the number of discs

(defvar *hanoi-default-delay* 1000)	;on many machines, hanoi demo is too fast w/o setting up delay.

(require "xlisp-2.1d/classes")		;define DEFCLASS DEFMETHOD
(require "xtango/wcls-delay")		;define XTANGO-DELAY-SELECTION-WIDGET-CLASS
(require "lib-utils/initialize")	;define WIDGET_CLASS method :GET
(require "lib-widgets/application")	;define WINTERP:APPLICATION-WIDGET-CLASS
(require "lib-widgets/simple-RC")	;define WINTERP:POPUP-MENU-WIDGET-CLASS, WINTERP:RADIO-BOX-WIDGET-CLASS, WINTERP:CHECK-BOX-WIDGET-CLASS, WINTERP:OPTION-MENU-WIDGET-CLASS, WINTERP:MENU-BAR-WIDGET-CLASS, WINTERP:POPUP-MENU-WIDGET-CLASS...

;; set this to the full path to a GIF file that you want as background for 
;; Towers of Hanoi..
(defvar *hanoi-background-gif-file*
  "/usr/local/winterp/examples/xtango/fluid2.gif")


;; If the file isn't there, set it to NIL so that later code
;; doesn't attempt to put up the background GIF.
(if (null (open *hanoi-background-gif-file* :direction :probe))
    (setq *hanoi-background-gif-file* NIL)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hanoi (n tango_w
		&aux a_im b_im c_im)	;N.B. &aux declares local variables

  ;; delete all existing images in <tango_w>, except for background gif.
  (map nil #'(lambda (i) (if (not (eq (send i :class) TANGO:GIF_IMAGE_CLASS))
			     (send i :tx_delete :perform)))
       (send tango_w :get_images))

  (setq a_im
	(send STACKING-PEG-IMAGE-CLASS :new tango_w
	      (complex (/ 1 6) 0.94)
	      "Peg A"
	      ))
  (setq b_im
	(send STACKING-PEG-IMAGE-CLASS :new tango_w
	      (complex (/ 3 6) 0.94)
	      "Peg B"
	      ))
  (setq c_im
	(send STACKING-PEG-IMAGE-CLASS :new tango_w
	      (complex (/ 5 6) 0.94)
	      "Peg C"
	      ))

  (send tango_w :refresh)

  (if (> n 0)
      (let* ((disc-x-radius		(/ 1 6))
	     (disc-x-radius-delta	(/ disc-x-radius n))
	     (disc-y-radius		(/ (- (imagpart (send a_im :bot_loc))
					      (imagpart (send a_im :top_loc))) (* 2.5 n)))
	     )
	(dotimes (i n)
		 (send a_im :push_disc
		       (send TANGO:ELLIPSE_IMAGE_CLASS :new tango_w
			     (send a_im :top_loc) ;location_coord
			     (complex (- disc-x-radius (* i disc-x-radius-delta)) ;radius_size
				      disc-y-radius) 
			     TANGO_COLOR_YELLOW
			     1.0	;fill_float
			     ))
		 )
	(transfer a_im b_im c_im n)
	))
  )


(defun print-move (from to)
  (let ((disc (send from :pop_disc))
	)
    (send disc :tap_traverse :perform :s (send to :top_loc) :clockwise)
    (send to :push_disc disc)
    )
  )


(defun transfer (from to via n)
  (cond ((equal n 1) (print-move from to))
	(t (transfer from via to (- n 1))
	   (print-move from to)
	   (transfer via to from (- n 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass BOXED-TEXT-IMAGE-CLASS
  ()
  ()
  TANGO:RECTANGLE_IMAGE_CLASS)


(defmethod BOXED-TEXT-IMAGE-CLASS :ISNEW
  (tango_w origin_coord name_str tango_color font_str)
  (let* (
	 (text_im		(send TANGO:TEXT_IMAGE_CLASS :new
				      tango_w
				      origin_coord :ctr
				      name_str
				      TANGO_COLOR_WHITE
				      font_str))
	 (top_left_coord	(- (send text_im :image_loc :nw)
				   #C(0.05 0.0)))
	 (bot_right_coord	(+ (send text_im :image_loc :se)
				   #C(0.05 0.0)))
	 )
    
    (send-super :isnew tango_w		;create TANGO:RECTANGLE_IMAGE_CLASS instance
		top_left_coord		;loc_coord
		(- bot_right_coord top_left_coord) ;size_coord
		tango_color		;tango_color
		1.0			;fill_float
		)
    ;; Had to create text first to figure out the size of the box.
    ;; Now, raise text to be on top of the box.
    (send text_im :tx_raise :perform)	
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass STACKING-PEG-IMAGE-CLASS
  (					;new ivars for this subclass
   ivar_begin_coord			;
   ivar_end_coord			;
   (ivar_disc_list NIL)			;
   )				
  ()					;no class variables for subclass
  BOXED-TEXT-IMAGE-CLASS)		;name of the superclass


(defmethod STACKING-PEG-IMAGE-CLASS :ISNEW
  (tango_w origin_coord name_str)
  ;; create 'self', an instance of BOXED-TEXT-IMAGE-CLASS
  (send-super :ISNEW			;create BOXED-TEXT-IMAGE-CLASS instance
	      tango_w
	      origin_coord 		;loc_coord
	      name_str			;text_string
	      TANGO_COLOR_MAROON	;tango_color
	      "-*-new century schoolbook-medium-i-normal--18-*-*-*-*-*-*" ;font
	      )

  (setq ivar_begin_coord (send self :image_loc :n)
	ivar_end_coord   (- ivar_begin_coord #C(0.0 0.75)))
  
  (send TANGO:LINE_IMAGE_CLASS :new tango_w
	ivar_begin_coord		;loc_coord
	(- ivar_end_coord ivar_begin_coord) ;size_coord
	TANGO_COLOR_RED			;tango_color
	1.0				;width_float
	1.0				;style_float
	:no_arrow			;arrow_int
	)
  )


;; moves disc from location returned by :top_loc
;; onto stack of discs...
(defmethod STACKING-PEG-IMAGE-CLASS :PUSH_DISC (disc_im)
  (if ivar_disc_list
      (send disc_im :tap_traverse :perform :s
	    (- (send (car ivar_disc_list) :image_loc :n) #C(0.0 0.001))
	    :straight)
    (send disc_im :tap_traverse :perform :s
	  (- ivar_begin_coord #C(0.0 0.002))
	  :straight)
    )
  (setq ivar_disc_list (cons disc_im ivar_disc_list))
  )


;; moves top disc from stack of discs 
;; to location returned by :top_loc
(defmethod STACKING-PEG-IMAGE-CLASS :POP_DISC ()
  (if (null ivar_disc_list)
      (error "attempt to :pop_disc with no discs left...")
    )
  (let ((disc (car ivar_disc_list)))
    (send disc :tap_traverse :perform :s ivar_end_coord :straight)
    (setq ivar_disc_list (cdr ivar_disc_list))
    disc
    )
  )


(defmethod STACKING-PEG-IMAGE-CLASS :TOP_LOC ()
  ivar_end_coord
  )


(defmethod STACKING-PEG-IMAGE-CLASS :BOT_LOC ()
  ivar_begin_coord
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (top_w main_w tango_w (delay-dialog_w NIL))
  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "hanoi"
	      :XMN_TITLE	"WINTERP: Towers of Hanoi"
	      :XMN_ICON_NAME	"W:hanoi"
	      ))
  (setq main_w
	(send WINTERP:APPLICATION-WIDGET-CLASS :new :managed
	      "main" top_w
	      ))
  (send main_w :set-work-area
	(setq tango_w
	      (send TANGO:WIDGET_CLASS :new :managed
		    "tango" main_w
		    :XMN_WIDTH		491
		    :XMN_HEIGHT		500
		    ))
	)

  (send main_w :add-menu-entry "System"
	:mnemonic             #\S
	)
  (send main_w :add-menu-entry '("System" "Quit")
	:mnemonic                        #\Q
	:callback		#'(lambda (w x) (send top_w :destroy))
	)
  (send main_w :add-menu-entry '("System" "Set Animation Delay...")
	:mnemonic                        #\S
	:callback		#'(lambda (w x)
				    (cond
				     (delay-dialog_w
				      (send delay-dialog_w :manage)
				      (send (send delay-dialog_w :parent) :raise_window)
				      )
				     (T
				      (setq delay-dialog_w
					    (send XTANGO-DELAY-SELECTION-WIDGET-CLASS :new :managed
						  "delay" main_w
						  tango_w ;NOTE special extra widget creation arg, a TANGO:WIDGET_CLASS instance
						  (send main_w :GET-MSG-WIDGET)	;NOTE special extra widget creation arg, a TIMED-MESSAGE-DISPLAY-WIDGET-CLASS instance
						  *hanoi-default-delay*	;NOTE special extra widget creation arg, a FIXNUM setting up the delay value for xtango animation
						  ))
				      )))
	)

  (send main_w :add-menu-entry "Animate"
	:mnemonic             #\A
	)

  ;; declare local function used by animate callbacks below. If I weren't lazy,
  ;; i'd just turn the "let" above into a class definition and make this a method.
  (flet	((animate-hanoi	(num)
	  (send main_w :display-string "Type Esc/Break/Stop to interrupt animation.")
	  (cond
	   ((null (errset (hanoi num tango_w)
			  nil		;don't print error message caused by "abort" via typing <Esc> into animation window
			  ))
	    (send main_w :display-string "Animation interrupted.")
	    (xt_add_timeout
	     1500
	     '((send main_w :display-string "Select from 'Animate' pulldown to begin animation.")))
	    )
	   (T
	    (send main_w :display-string "Select from 'Animate' pulldown to begin animation.")
	    ))
	  ))
	(send main_w :add-menu-entry '("Animate" "3 discs")
	      :mnemonic                         #\3
	      :callback	#'(lambda (w x) (animate-hanoi 3))
	      )
	(send main_w :add-menu-entry '("Animate" "4 discs")
	      :mnemonic                         #\4
	      :callback	#'(lambda (w x) (animate-hanoi 4))
	      )
	(send main_w :add-menu-entry '("Animate" "5 discs")
	      :mnemonic                         #\5
	      :callback	#'(lambda (w x) (animate-hanoi 5))
	      )
	(send main_w :add-menu-entry '("Animate" "6 discs")
	      :mnemonic                         #\6
	      :callback	#'(lambda (w x) (animate-hanoi 6))
	      )
	(send main_w :add-menu-entry '("Animate" "7 discs")
	      :mnemonic                         #\7
	      :callback	#'(lambda (w x) (animate-hanoi 7))
	      )
	(send main_w :add-menu-entry '("Animate" "8 discs")
	      :mnemonic                         #\8
	      :callback	#'(lambda (w x) (animate-hanoi 8))
	      )
	(send main_w :add-menu-entry '("Animate" "9 discs")
	      :mnemonic                         #\9
	      :callback	#'(lambda (w x) (animate-hanoi 9))
	      )
	(send main_w :add-menu-entry '("Animate" "10 discs")
	      :mnemonic                         #\1
	      :callback	#'(lambda (w x) (animate-hanoi 10))
	      )
	)

  (send main_w :add-menu-entry "Help"
	:mnemonic             #\H
	)
  (send main_w :add-menu-entry '("Help" "Help")
	:mnemonic                  #\H
	:callback		#'(lambda (w x) (send main_w :display-string "Help not implemented..."))
	)

  (let ((initd_p NIL))
    (send tango_w :add_callback :XMN_EXPOSE_CALLBACK
	  '(CALLBACK_WIDGET)
	  '(
	    (if initd_p
		(send CALLBACK_WIDGET :refresh)
	      (progn
		(send CALLBACK_WIDGET :begin_drawing)
		(send CALLBACK_WIDGET :set_delay *hanoi-default-delay*)

		;; create background GIF, just for the hell of it; note that
		;; test above sets the GIF file to NIL if it can't be read ...
		(if *hanoi-background-gif-file*
		    (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
			  #C(0.0 0.0)	;location_coord
			  *hanoi-background-gif-file*
			  :verbose
			  ))
		;; put an "empty" towers of hanoi up prior to user selecting
		;; one of the animation sequences from menu...
		(hanoi 0 CALLBACK_WIDGET)
		(setq initd_p t)
		)
	      ))
	  ))

  (send main_w :make-menus)
  (send main_w :set-menu-help-widget)
  (send top_w :realize)
  (send main_w :display-string "Select from 'Animate' pulldown to begin animation.")
  )
