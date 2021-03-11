; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Random bits of code I wrote in testing WINTERP's Xtango widget.
;		This file isn't meant to be loaded, rather, each lisp form is
;		to be interactively evaluated using the gnuemacs interface 
;		or w_ctrlpnl.lsp...
; Author:       Niels P. Mayer
; Created:      1993
; Modified:     Mon Jun  6 04:18:43 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*
(require "xtango/cls-widget")		;define XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS

(let*
    ((toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    :XMN_GEOMETRY	"200x600+1+1"
	    :XMN_TITLE		"WINTERP: Xtango Test"
	    :XMN_ICON_NAME	"W:test"
	    ))
     (scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	    "sc" toplevel_w
	    :XMN_SCROLLING_POLICY :AUTOMATIC
	    ))
     (rowcol_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "rc" scrl_w
	    :XMN_ORIENTATION :VERTICAL
	    :XMN_PACKING :PACK_TIGHT
	    :XMN_IS_ALIGNED nil
;	    :XMN_ENTRY_ALIGNMENT :ALIGNMENT_CENTER
	    )))

  (send toplevel_w :realize)
  (setq rc_w rowcol_w)
  (setq top_w toplevel_w)
  )


(trace :XMN_EXPOSE_CALLBACK)

(let*
    ((toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
	    :XMN_GEOMETRY	"200x600+1+1"
	    :XMN_TITLE		"WINTERP: Xtango Test"
	    :XMN_ICON_NAME	"W:test"
	    ))
     (scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	    "sc" toplevel_w
	    :XMN_SCROLLING_POLICY :AUTOMATIC
	    ))
     (rowcol_w
      (send XM_FORM_WIDGET_CLASS :new :managed
	    "rc" scrl_w
	    ))
     (ivar_image_pb_w NIL)
     (prev_w	      NIL)
     )

  (send toplevel_w :realize)
  (dotimes (i 100)
	   (setq ivar_image_pb_w
		 (send TANGO:WIDGET_CLASS :new :managed :button
		       "foo"
		       rowcol_w
		       :XMN_HEIGHT			50
		       :XMN_WIDTH			50
		       :XMN_LEFT_ATTACHMENT		:attach_form
		       :XMN_RIGHT_ATTACHMENT		:attach_form
		       :XMN_TOP_ATTACHMENT		(if prev_w :attach_widget :attach_form)
		       (if prev_w :XMN_TOP_WIDGET :XMN_TOP_ATTACHMENT) (if prev_w prev_w :attach_form)
		       ))
	   (setq prev_w ivar_image_pb_w)

	   (let ((init_p NIL))
	     (send ivar_image_pb_w :add_callback :XMN_EXPOSE_CALLBACK '(CALLBACK_REASON CALLBACK_WIDGET)
		   '(
		     (format T "expose called on w=~A reason=~A\n"
			     CALLBACK_WIDGET CALLBACK_REASON)

		     (cond
		      (init_p
		       (send CALLBACK_WIDGET :refresh)
		       )
		      (T
		       (send CALLBACK_WIDGET :begin_drawing)
		       (send CALLBACK_WIDGET :refresh)
		       (setq init_p t)
		       ))
		     ))
	     )
	   )
  )


(setq create-circle_w
      (send XTANGO-BUTTON-WIDGET-CLASS :new :managed
	    "tango" rc_w
	    `(,TANGO:CIRCLE_IMAGE_CLASS #C(0.5 0.5) 0.3 "black" 0.0)
	    :XMN_HEIGHT		100
	    :XMN_WIDTH		100
 	    :XMN_BORDER_WIDTH		10
 	    :XMN_HIGHLIGHT_THICKNESS	10
	    :XMN_SHADOW_THICKNESS       10
;	    :XMN_MARGIN_WIDTH		10
; 	    :XMN_MARGIN_HEIGHT		10
; 	    :XMN_MARGIN_BOTTOM		10
; 	    :XMN_MARGIN_TOP		10
; 	    :XMN_MARGIN_LEFT		50
; 	    :XMN_MARGIN_RIGHT	        10
	    ))

;;; (let (height width border_width highlight_thickness shadow_thickness margin_width margin_height margin_bottom margin_top margin_left margin_right)
;;;   (send (get_moused_widget) :get_values
;;; 	:XMN_HEIGHT	        'height 
;;; 	:XMN_WIDTH	        'width 
;;; 	:XMN_BORDER_WIDTH       'border_width 
;;; 	:XMN_HIGHLIGHT_THICKNESS 'highlight_thickness
;;; 	:XMN_SHADOW_THICKNESS    'shadow_thickness
;;; 	:XMN_MARGIN_WIDTH        'margin_width
;;; 	:XMN_MARGIN_HEIGHT       'margin_height
;;; 	:XMN_MARGIN_BOTTOM       'margin_bottom
;;; 	:XMN_MARGIN_TOP          'margin_top 
;;; 	:XMN_MARGIN_LEFT         'margin_left
;;; 	:XMN_MARGIN_RIGHT        'margin_right
;;; 	)
;;;   (format T ":XMN_HEIGHT=~A\n:XMN_WIDTH=~A\n:XMN_BORDER_WIDTH=~A\n:XMN_HIGHLIGHT_THICKNESS=~A\n:XMN_SHADOW_THICKNESS=~A\n:XMN_MARGIN_WIDTH=~A\n:XMN_MARGIN_HEIGHT=~A\n:XMN_MARGIN_BOTTOM=~A\n:XMN_MARGIN_TOP=~A\n:XMN_MARGIN_LEFT=~A\n:XMN_MARGIN_RIGHT=~A\n"
;;; 	  height 
;;; 	  width 
;;; 	  border_width 
;;; 	  highlight_thickness
;;; 	  shadow_thickness
;;; 	  margin_width
;;; 	  margin_height
;;; 	  margin_bottom
;;; 	  margin_top 
;;; 	  margin_left
;;; 	  margin_right
;;; 	  ))

(send create-circle_w :add_event_handler BUTTON1_MOTION_MASK
      '(EVHANDLER_XEVENT EVHANDLER_WIDGET)
      '(
	(format T "~A\n" (send EVHANDLER_WIDGET :get_event_coord EVHANDLER_XEVENT))
	))

(require "bitmaps/movi-face")	
(length (aref *face-movie* 3))
(length (aref (aref *face-movie* 3) 0))

(require "xtango/xbm-to-arr")
(require "xlisp-2.1d/pp")
(setq a (bitmap-file-to-array (concatenate 'string *X11-BITMAPS-DIRECTORY*
					   "wingdogs")
			      TANGO_COLOR_BLACK TANGO_COLOR_WHITE
			      ))
(setq f (open "/tmp/wingdogs.lsp" :direction :output :if-exists :supersede))
(pp a f)
(close f)

(send tango_w :inq_coord)
(send tango_w :set_coord 0.1 1.1 1.1 0.1)

(trace :xmn_expose_callback)
(send tango_w :refresh)
(dotimes (i 100) (print i) (send tango_w :refresh))



;; (send tango_w :refresh)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "xtango/im_methpop")

(setq xxx (get_moused_widget))

(send xxx :add_event_handler BUTTON_PRESS_MASK
      '(EVHANDLER_WIDGET EVHANDLER_XEVENT EVHANDLER_BUTTON)
      '(
	(if (eq EVHANDLER_BUTTON 3)
	    (popup-menu-of-methods-of-object EVHANDLER_WIDGET EVHANDLER_WIDGET EVHANDLER_XEVENT)
	  )
	))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tw (get_moused_widget))
(setq ti0 (send tw :input_image))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tw (get_moused_widget))
(setq ti0 (send tw :input_image))
(setq ti1 (send tw :input_image))
(setq ti2 (send tw :input_image))
(setq ti3 (send tw :input_image))
(setq ti4 (send tw :input_image))
(setq ti5 (send tw :input_image))
(setq ti6 (send tw :input_image))

(setq pa-forw (tango:path_create #C(0.05 0.00) #C(0.05 0.00) #C(0.05 0.00) #C(0.05 0.00) #C(0.05 0.00)))

(setq tx-forw (tango:tx_compose 
	       (send ti0 :tx_move pa-forw)
	       (send ti1 :tx_move pa-forw)
	       (send ti2 :tx_move pa-forw)
	       (send ti3 :tx_move pa-forw)
	       (send ti4 :tx_move pa-forw)
	       (send ti5 :tx_move pa-forw)
	       (send ti6 :tx_move pa-forw)
	       ))

(setq pa-back (tango:path_create (- #C(0.05 0.00)) (- #C(0.05 0.00)) (- #C(0.05 0.00)) (- #C(0.05 0.00)) (- #C(0.05 0.00))))

(setq tx-back
      (tango:tx_compose
       (send ti0 :tx_move pa-back)
       (send ti1 :tx_move pa-back)
       (send ti2 :tx_move pa-back)
       (send ti3 :tx_move pa-back)
       (send ti4 :tx_move pa-back)
       (send ti5 :tx_move pa-back)
       (send ti6 :tx_move pa-back)
       ))

(tango:tx_perform tx-forw)
(tango:tx_perform tx-back)

(send ti6 :tx_delete :perform)
(send ti5 :tx_delete :perform)
(send ti4 :tx_delete :perform)
(send ti3 :tx_delete :perform)
(send ti2 :tx_delete :perform)
(send ti1 :tx_delete :perform)
(send ti0 :tx_delete :perform)


(setq t1 (send ti :tx_resize pa-forw)
(setq t3 (send ti :tx_resize pa-back))
(setq t2 (send ti :tx_delete))

(tango:tx_perform t1)
(tango:tx_perform t3)
(tango:tx_perform t2)

(setq tango_w (get_moused_widget))

(xt_add_timeout
 0
 '(
   (send tango_w :ZOOM :out)
   (send tango_w :ZOOM :out)
   (send tango_w :ZOOM :out)
   (send tango_w :ZOOM :out)
   (send tango_w :ZOOM :out)
   (send tango_w :ZOOM :in)
   (send tango_w :ZOOM :in)
   (send tango_w :ZOOM :in)
   (send tango_w :ZOOM :in)
   (send tango_w :ZOOM :in)
 
   (setq to (xt_add_timeout 0 TIMEOUT_OBJ))
   ))
(xt_remove_timeout to)

(require "xtango/test-bbox")
(progn tango_w)

(apply #'tango:tx_compose :perform
       (map 'list #'(lambda (i) (send i :tx_delete))
	    (send tango_w :get_images)))

(apply 'tango:tx_compose :perform
		  (map 'list #'(lambda (i) (send i :tx_move #C(0.5 0.5)))
		       (send tango_w :get_images)))
(apply 'tango:tx_compose :perform
		  (map 'list #'(lambda (i) (send i :tx_move (- #C(0.5 0.5) (send i :image_loc :ctr))))
		       (send tango_w :get_images)))
(apply 'tango:tx_compose :perform
		  (map 'list #'(lambda (i) (send i :tx_move (- (send i :image_loc :ctr) #C(0.5 0.5))))
		       (send tango_w :get_images)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "xtango/test-bbox")

(setq bi (send bitm_image :storeon))
(send bitm_image :tx_delete :perform)
(eval bi)
(send tango_w :refresh)
(send circ_image :storeon)
;; (send comp_image :storeon)
(send elli_image :storeon)
(setq li (send line_image :storeon))
(send polyg_image :storeon)
(send polyl_image :storeon)
(send rect_image :storeon)
(send spli_image :storeon)
(send text_image :storeon)



;;; Modify default format so that it always prints fractional part.
;;; Note that default print for 1.0 is "1" and 0.0 is "0"...
(setq *FLOAT-FORMAT* "%g") ;default

(progv '(*FLOAT-FORMAT*) '("%#g")
       (eval (read (make-string-input-stream (format nil "~A" (send (send tango_w :input_image) :storeon)))))
       )


(require "rc-shell")
;; (load "xtango/foo")
(setq www
      (send X-Size-Selector-Widget-Class :new :managed
	    "foo"
	    rc_w NIL
	    ))

(send www :set_values
      :XMN_SCALE_HEIGHT	10
      :XMN_FONT_LIST	"6x10")



(require "xtango/movi-earth")
;; (load "utils")
(setq tango_w (get_moused_widget))
;;;(length (send tango_w :GET-SELECTED-IMAGES))
(setq ti (send tango_w :input_image))
(setq tisexp (send ti :storeon))
(setq colsexp (send tango_w :COLORS_STOREON))
(setq bmap (send tango_w :COPY_TO_2D_BITMAP_ARRAY 0.0 0.0 1.0 1.0))

(setq f (open "/tmp/out.lsp" :direction :output :if-exists :supersede))
(pp tisexp f)
(close f)


(setq *TANGO_WIDGET* (get_moused_widget))
(setq xxx (send *TANGO_WIDGET* :COPY_TO_2D_BITMAP_ARRAY 0.0 0.0 1.0 1.0))
(send TANGO:BITMAP_IMAGE_CLASS :new :show *TANGO_WIDGET*
      #C(0.01 0.01)
      (vector xxx)
      )

(setq tango_w
      (send TANGO:WIDGET_CLASS :new :managed
	    "earth_tango" rc_w
	    :XMN_HEIGHT 100
	    :XMN_WIDTH 100
	    :XMN_RESIZE_POLICY :resize_grow
	    ))

(send tango_w :begin_drawing)	;must call this after :realize
(send tango_w :SET_BGCOLOR "white")

(send TANGO:BITMAP_IMAGE_CLASS :new :show :visible tango_w #C(0.0 0.0) (vector bmap))

(setq earth_timage
      (eval tisexp))

(send earth_timage :tap_show)
(send earth_timage :tx_shuffle :perform)

(xt_add_timeout
 0
 '(
   (TANGO:TX_PERFORM (send earth_timage :TX_shuffle 1))
   (setq earth_to (xt_add_timeout 100 TIMEOUT_OBJ))
   ))


(setq compo_desc_list
      '(SEND TANGO:COMPOSITE_IMAGE_CLASS :NEW :VISIBLE *TANGO_WIDGET* #C(0.462500 0.145000) TANGO:RECTANGLE_IMAGE_CLASS #C(0.00000 0.00000) #C(0.100000 0.200000) 7 0.00000 TANGO:RECTANGLE_IMAGE_CLASS #C(0.0100000 0.0100000) #C(0.0800000 0.0800000) 7 0.00000 TANGO:RECTANGLE_IMAGE_CLASS #C(0.0100000 0.110000) #C(0.0800000 0.0800000) 7 0.00000 TANGO:CIRCLE_IMAGE_CLASS #C(0.0900000 0.100000) 0.00500000 7 0.00000)
      )

(setq compo_desc_list (cdr (cddddr compo_desc_list)))
(nth 5 compo_desc_list)
