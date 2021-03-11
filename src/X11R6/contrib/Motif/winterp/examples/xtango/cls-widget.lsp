; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         cls-widget.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/cls-widget.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  XTANGO-WIDGET-CLASS, XTANGO-BUTTON-WIDGET-CLASS. 
;		XTANGO-WIDGET-CLASS requires special methods installed on 
;		all the xtango image classes -- see ./cls-image.lsp.
; Author:       Niels P. Mayer
; Created:      Fri Jul  2 10:36:58 1993
; Modified:     Mon Jun  6 03:24:37 1994 (Niels Mayer) npm@indeed
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

(require "xlisp-2.1d/classes")		;define OBJECT method :ISKINDOF
(require "xtango/util")			;defines TX-COMPOSE-LIST
(require "lib-utils/motif-vers")	;define *MOTIF-1.2-OR-LATER-P*

(defvar *xtango-image-selection-timeout-milliseconds* 750)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XTANGO-WIDGET-CLASS, -- a trivial subclass of TANGO:WIDGET_CLASS
;;;
(setq XTANGO-WIDGET-CLASS		;name of the new subclass
      (send Class :new
            '(				;inst variables for subclass
	      ivar_init_p		;indicates that underlying TANGO:WIDGET_CLASS instance may be init'd with :BEGIN_DRAWING
	      ivar_timage_sel		;for a single selection, this is set to a tango_image instance.
	      ivar_timage_sel_list	;for a multiple selection, this is set to a list of tango_image instances.
	      ivar_timeout		;timeout which flashes the selected image or images.
	      )
            '()                         ;no class variables for subclass
            TANGO:WIDGET_CLASS		;name of the superclass
	    )) 
;;;
;;; Override TANGO:WIDGET_CLASS instance initializer (method :isnew).
;;; This subclass of TANGO:WIDGET_CLASS will automatically call
;;; :begin_drawing when it is first exposed (thereby maintaining
;;; constraint that :begin_drawing only get called after the windows
;;; for the tango widget are created.) Subsequent exposes will automatically
;;; refresh the graphics in the tango widget.
;;;
(send XTANGO-WIDGET-CLASS :answer :isnew
      '(managed_k widget_name widget_parent &rest args)
      '(
	(setq ivar_init_p		NIL
	      ivar_timage_sel		NIL
	      ivar_timage_sel_list	NIL
	      ivar_timeout		NIL
	      )
 	(apply #'send-super :isnew	;call superclass's init to create widget
 	       managed_k widget_name widget_parent
	       args
	       )
	(send-super :add_callback :XMN_EXPOSE_CALLBACK
		    '(CALLBACK_WIDGET)
		    '(
		      (if ivar_init_p
			  (send-super :refresh)
			(progn
			  (send-super :begin_drawing)
			  (send-super :refresh)
			  (setq ivar_init_p t)
			  )
			)
		      ))

	;;	(send-super :SET_VALUES :XMN_TRANSLATIONS
	;;	)
	(if *MOTIF-1.2-OR-LATER-P*
	    ;; For Motif 1.2 or greater
	    (send-super :OVERRIDE_TRANSLATIONS
		    "Shift<Btn1Down>:     Lisp(send ACTION_WIDGET :button-shift-1-press-callproc ACTION_XEVENT) \
                     <Btn1Down>:          Lisp(send ACTION_WIDGET :button-1-press-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetArm() \
                     <Btn1Down>(2+):      Lisp(send ACTION_WIDGET :button-1-press-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetMultiArm() \
                     <Btn2Down>:          Lisp(send ACTION_WIDGET :button-2-press-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetDrag() \
                     <Btn3Down>:          Lisp(send ACTION_WIDGET :button-3-press-callproc ACTION_XEVENT) \
                     <Btn1Down>,<Btn1Up>: Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetActivate() \
                     <BtnUp>:             Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() \
                     <Btn1Up>:            Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetActivate() \
                     <Btn1Up>(2+):        Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetMultiActivate() \
                     ")
	  ;; For Motif 1.0 or 1.1, prevent warning "Warning: Actions not found: ManagerGadgetDrag"
	  (send-super :OVERRIDE_TRANSLATIONS
		    "Shift<Btn1Down>:     Lisp(send ACTION_WIDGET :button-shift-1-press-callproc ACTION_XEVENT) \
                     <Btn1Down>:          Lisp(send ACTION_WIDGET :button-1-press-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetArm() \
                     <Btn1Down>(2+):      Lisp(send ACTION_WIDGET :button-1-press-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetMultiArm() \
                     <Btn2Down>:          Lisp(send ACTION_WIDGET :button-2-press-callproc ACTION_XEVENT) DrawingAreaInput() \
                     <Btn3Down>:          Lisp(send ACTION_WIDGET :button-3-press-callproc ACTION_XEVENT) \
                     <Btn1Down>,<Btn1Up>: Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetActivate() \
                     <BtnUp>:             Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() \
                     <Btn1Up>:            Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetActivate() \
                     <Btn1Up>(2+):        Lisp(send ACTION_WIDGET :button-any-release-callproc ACTION_XEVENT) DrawingAreaInput() ManagerGadgetMultiActivate() \
                     ")
	  )

	(send-super :add_event_handler BUTTON1_MOTION_MASK
		    '(EVHANDLER_XEVENT)
		    `(
		      ;; if :get_event_image failed in button-press-ev-handler above,
		      ;; then this won't fire on button-motion events
		      (if ivar_timage_sel
			  (send ivar_timage_sel :button-1-motion-callproc ,self EVHANDLER_XEVENT)
			)
		      ))
	(send-super :add_event_handler BUTTON2_MOTION_MASK
		    '(EVHANDLER_XEVENT)
		    `(
		      ;; if :get_event_image failed in button-press-ev-handler above,
		      ;; then this won't fire on button-motion events
		      (if ivar_timage_sel			
			  (send ivar_timage_sel :button-2-motion-callproc ,self EVHANDLER_XEVENT)
			)
		      ))

	;; flash any selected images...
	(setq ivar_timeout
	      (xt_add_timeout
	       *xtango-image-selection-timeout-milliseconds*
	       '(
		 (progv '(*breakenable*) '(nil)
			(unwind-protect	;if an error occurs, trap it and reschedule subsequent time-out.
			    (cond
			     (ivar_timage_sel_list ;IF MULTIPLE SELECTION....
			      (let* ((tx_list
				      (map 'list #'(lambda (i) (send i :tx_visible 2)) ivar_timage_sel_list))
				     (tx_comp
				      (tx-compose-list tx_list)) ;tx-compose-list == TANGO:TX_COMPOSE which can compose >= 50 transs w/o error...
				     )
				(tango:tx_perform tx_comp)
				(tango:tx_free tx_comp)	;the garbage collector could do this, but here i explicitly free the transition...
				(map nil #'tango:tx_free tx_list) ;the garbage collector could do this, but here i explicitly free the transition...
				)
			      )
			     (ivar_timage_sel ;ELSE-IF SINGLE SELECTION
 			      (send ivar_timage_sel :tx_visible :perform 2)
			      )
			     )
			  ;; always unwind-protect 
			  (xt_add_timeout *xtango-image-selection-timeout-milliseconds* TIMEOUT_OBJ)
			  ))
		 )))

	(send-super :add_callback :XMN_DESTROY_CALLBACK
		    '()
		    '(
		      (if ivar_timeout
			  (progv '(*breakenable*) '(nil)
				 (errset (xt_remove_timeout ivar_timeout) nil)
				 )
			)
		      ))
	))

(send XTANGO-WIDGET-CLASS :answer :BUTTON-1-PRESS-CALLPROC '(ACTION_XEVENT)
      '(
;;;	(format t ":BUTTON-1-PRESS-CALLPROC\n")

	;; remove the flashing selection timeout (re-establish on release)
	;; so that a drag or resize operation will not have flashing going
	;; while the operation is being performed (for aesthetic reasons).
	(if ivar_timeout
	    (progv '(*breakenable*) '(nil)
		   (errset (xt_remove_timeout ivar_timeout) nil)
		   )
	  )

	(if (setq ivar_timage_sel (send-super :get_event_image ACTION_XEVENT))
	    (send ivar_timage_sel :button-1-press-callproc self ACTION_XEVENT)
	  )
	(setq ivar_timage_sel_list NIL)
	))

(send XTANGO-WIDGET-CLASS :answer :BUTTON-SHIFT-1-PRESS-CALLPROC '(ACTION_XEVENT)
      '(
;;;	(format t ":BUTTON-SHIFT-1-PRESS-CALLPROC\n")

	;; remove the flashing selection timeout (re-establish on release)
	;; so that a drag or resize operation will not have flashing going
	;; while the operation is being performed (for aesthetic reasons).
	(if ivar_timeout
	    (progv '(*breakenable*) '(nil)
		   (errset (xt_remove_timeout ivar_timeout) nil)
		   )
	  )

	;; add existing selection to multiple selection list, incase it's not there
	(let ((old_timage_sel ivar_timage_sel))
	  (if (setq ivar_timage_sel (send-super :get_event_image ACTION_XEVENT))
	      (progn
		(if (member ivar_timage_sel ivar_timage_sel_list :test #'eq)
		    ;; if the image is already in the multiple selection list, then remove it
		    (setq ivar_timage_sel_list (delete-if #'(lambda (x) (eq x ivar_timage_sel)) ivar_timage_sel_list))
		  ;; else add it to the multiple selection list...
		  (progn
		    (setq ivar_timage_sel_list (cons ivar_timage_sel ivar_timage_sel_list))
		    ;; if adding a new elt to multiple-select-list, also make sure to add previously
		    ;; selected node too (s.t. prior single select elt ends up in multiple-select list)
		    (if (and old_timage_sel
			     (not (eq ivar_timage_sel old_timage_sel)) 
			     (not (member old_timage_sel ivar_timage_sel_list :test #'eq)))
			(setq ivar_timage_sel_list (cons old_timage_sel ivar_timage_sel_list))
		      )
		    ))
		(send ivar_timage_sel :button-1-press-callproc self ACTION_XEVENT)
		))
	  )
	))

(send XTANGO-WIDGET-CLASS :answer :BUTTON-2-PRESS-CALLPROC '(ACTION_XEVENT)
      '(
;;;	(format t ":BUTTON-2-PRESS-CALLPROC\n")

	;; remove the flashing selection timeout (re-establish on release)
	;; so that a drag or resize operation will not have flashing going
	;; while the operation is being performed (for aesthetic reasons).
	(if ivar_timeout
	    (progv '(*breakenable*) '(nil)
		   (errset (xt_remove_timeout ivar_timeout) nil)
		   )
	  )

	(if (setq ivar_timage_sel (send-super :get_event_image ACTION_XEVENT))
	    (send ivar_timage_sel :button-2-press-callproc self ACTION_XEVENT)
	  )
	(setq ivar_timage_sel_list NIL)
	))

(send XTANGO-WIDGET-CLASS :answer :BUTTON-3-PRESS-CALLPROC '(ACTION_XEVENT)
      '(
;;;	(format t ":BUTTON-3-PRESS-CALLPROC\n")

	;; remove the flashing selection timeout (re-establish on release)
	;; so that a drag or resize operation will not have flashing going
	;; while the operation is being performed (for aesthetic reasons).
	(if ivar_timeout
	    (progv '(*breakenable*) '(nil)
		   (errset (xt_remove_timeout ivar_timeout) nil)
		   )
	  )

	(if (setq ivar_timage_sel (send-super :get_event_image ACTION_XEVENT))
	    (send ivar_timage_sel :button-3-press-callproc self ACTION_XEVENT)
	  )
	(setq ivar_timage_sel_list NIL)
	))

(send XTANGO-WIDGET-CLASS :answer :BUTTON-ANY-RELEASE-CALLPROC '(ACTION_XEVENT)
      '(
;;;	(format t ":BUTTON-ANY-RELEASE-CALLPROC\n")
	(if ivar_timage_sel
	    (send ivar_timage_sel :button-any-release-callproc self ACTION_XEVENT)
	  )

	;; re-establish the flashing selection timeout which was disabled
	;; in :BUTTON-1-PRESS-CALLPROC :BUTTON-2-PRESS-CALLPROC,
	;; :BUTTON-3-PRESS-CALLPROC and :BUTTON-SHIFT-1-PRESS-CALLPROC.
	(if (and ivar_timeout (not (timeout_active_p ivar_timeout)))
	    (xt_add_timeout *xtango-image-selection-timeout-milliseconds* ivar_timeout)
	  )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; allow people to explicitly call :BEGIN_DRAWING while preventing
;;; errors from occuring in the expose callback above; otherwise
;;; the expose callback code wouldn't know that the widget had already
;;; been initialized with :BEGIN_DRAWING in users code...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :BEGIN_DRAWING '()
      '(
	(if (null ivar_init_p)
	    (progn
	      (setq ivar_init_p t)
	      (send-super :begin_drawing)
	      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns NIL if no images selected.
;; Returns a single tango_image instance if only one image selected
;; Returns a list of tango_image instances if multiple images selected
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :GET-SELECTED-IMAGES '()
      '(
	(if ivar_timage_sel_list
	    ivar_timage_sel_list
	  ivar_timage_sel
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :SET-SELECTED-IMAGE '(image)
      '(
	(setq ivar_timage_sel_list NIL
	      ivar_timage_sel      image)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :SET-MULTI-SELECTED-IMAGES '(image_list)
      '(
	(setq ivar_timage_sel_list image_list
	      ivar_timage_sel      NIL)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :GROUP-SELECTED-IMAGES '()
      '(
	(if (consp ivar_timage_sel_list)
	    (progn
	      (send self :set-selected-image
		    (send self :group-image-list-returning-composite ivar_timage_sel_list))
	      T				;RETURN SUCCESS
	      )
	  NIL				;RETURN FAILURE
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :UNGROUP-SELECTED-IMAGE '()
      '(
	(if (and (null ivar_timage_sel_list) ;IF NOT IN MULTIPLE SELECTION MODE
		 (tango:imageobjp ivar_timage_sel) ;BUT IN SINGLE SELECTION MODE
		 (send ivar_timage_sel :iskindof TANGO:COMPOSITE_IMAGE_CLASS) ;AND SINGLE SELECTION IS A COMPOSITE IMAGE
		 )
	    (progn			;THEN UNGROUP THE COMPOSITE, SET MULTIPLE SELECTION TO RESULTING IMAGES
	      (send self :set-multi-selected-images
		    (send self :ungroup-composite-returning-image-list ivar_timage_sel))
	      T				;RETURN SUCCESS
	      )
	  NIL				;RETURN FAILURE
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :CLEAR-IMAGES '()
      '(
	(send self :set-selected-image nil)
	(let ((im_list (send-super :get_images)))
	  (if im_list
	      (let* ((tx_list
		      (map 'list #'(lambda (i) (send i :tx_delete)) im_list))
		     (tx_comp
		      (tx-compose-list tx_list)) ;tx-compose-list == TANGO:TX_COMPOSE which can compose >= 50 transs w/o error...
		     )
		(tango:tx_perform tx_comp)
		(tango:tx_free tx_comp)	;the garbage collector could do this, but here i explicitly free the transition...
		(map nil #'tango:tx_free tx_list) ;the garbage collector could do this, but here i explicitly free the transition...
		)
	    ))
	))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :LOAD-IMAGES '(file-str)
      '(
	(progv '(*tango_widget*) `(,self) ;temporarily bind 'special' global '*tango_widget*' to the xtango-widget, since '*tango_widget*' used in save-file...
	       (load file-str)
	       )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :SAVE-IMAGES '(file-str)
      '(
	(progv '(*FLOAT-FORMAT*) '("%#g") ;if we don't do this, then 1.000000 prints as "1", which will get read as an integer by :LOAD_IMAGES...
	       (let ((ff (open file-str :direction :output :if-exists :supersede)))
		 ;; write "color map"
		 (map nil #'(lambda (i) (format ff "~S\n" i))
		      (send-super :COLORS_STOREON))
		 ;; write out all images...
		 (map nil #'(lambda (i) (format ff "~S\n" (send i :storeon)))
		      (send-super :get_images))
		 ;; make images visible after they've loaded and been created
		 (format ff "~S\n" '(send *tango_widget* :refresh))
		 (close ff)
		 ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; given list of tango_image instances 'image-list', deletes all the images
;; and creates a new composite image containing the images. returns that value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :GROUP-IMAGE-LIST-RETURNING-COMPOSITE
      '(image-list)			;a list of TANGO:IMAGE_CLASS subclass instances
      '(
	(let* ((orig_images_desc_list	;((<tango_image_class> <location> ...)(<tango_image_class> <location> ...)(<tango_image_class> <location> ...))
		(map 'list #'(lambda (i	;LAMBDA returns (<tango_image_class> <location> ...)
				      &aux (im_desc (send i :storeon)))
			       (cons (cadr im_desc) ;CADR returns <tango_image_class>
				     (cdr (cddddr im_desc))) ;CDR CDDDDDR returns '(<location> ...)'
			       )
		     image-list)
		)
	       (nw_loc_list
		(map 'list #'(lambda(i) (send i :image_loc :nw)) image-list)
		)
	       (nw_loc
		(complex (apply #'min (map 'list #'realpart nw_loc_list))
			 (apply #'min (map 'list #'imagpart nw_loc_list)))
		)
	       (images_desc_list      NIL)
	       (images_desc_list_tail NIL)
	       )
    
	  ;; go through each image-description list in orig_images_desc_list; for any
	  ;; composite image description lists, expand them into top-level image-description
	  ;; lists in 'images_desc_list'
	  (dolist (im_desc orig_images_desc_list)
		  (if (eq (car im_desc) 'TANGO:COMPOSITE_IMAGE_CLASS) ;:STOREON returns symbol, so look for that rather than class-obj
		      ;; THEN TRANSFORM IMAGES IN COMPOSITE INTO DESCRIPTIONS OF TOP-LEVEL
		      ;; IMAGES AND APPEND THEM INTO images_desc_list, BEGINNING AT
		      ;; LIST POSITION WHERE im_desc WOULD HAVE GONE...
		      (progn		;im_desc==(TANGO:COMPOSITE_IMAGE_CLASS <location> <im1-class-sym> <im1-location> ... <im2-class-sym> <im2-location> ... ...)
			(setq im_desc (cdr im_desc)) ;im_desc==(<location> <im1-class-sym> <im1-location> ... <im2-class-sym> <im2-location> ... ...)
			(do*
			 ((compo_loc (car im_desc)) ;retrieve <location>
			  (im_prev im_desc
				   im_cur)
			  (im_cur  (cdr im_desc) ;im_cur==(<im1-class-sym> <im1-location> ... <im2-class-sym> <im2-location> ... ...)
				   (cdr im_cur))
			  )
			 ((null im_cur))
			 (if (classp (eval (car im_cur))) ;must eval <im-class-symbol> to find out if it represents a class or not...
			     (progn
			       (rplacd im_prev NIL) ;terminate previous image description list
			       ;; append list (<im-i-class-sym> <im-i-location> ...) as new last element in image_desc_list
			       (if images_desc_list
				   (setq images_desc_list_tail (cdr (rplacd images_desc_list_tail (cons im_cur NIL))))
				 (progn
				   (setq images_desc_list (cons im_cur NIL))
				   (setq images_desc_list_tail images_desc_list)
				   ))
			       (rplaca (cdr im_cur) ;update <im-i-location> with location of the composite image.
				       (+ (cadr im_cur) compo_loc)) 
			       ))
			 )
			)
		    ;; ELSE NOT COMPOSITE IMAGE, SO APPEND UNCHANGED im_desc TO IMAGES_DESC_LIST...
		    (if images_desc_list
			(setq images_desc_list_tail (cdr (rplacd images_desc_list_tail (cons im_desc NIL))))
		      (progn
			(setq images_desc_list (cons im_desc NIL))
			(setq images_desc_list_tail images_desc_list)
			))
		    )
		  )

;;; (format T "images_desc_list prior to location update '~S'\n" images_desc_list)

	  ;; <nw-loc> is the northwest most position in the group of selected images.
	  ;; Make that the origin of the composite image. Subtract that value
	  ;; from all the other <locations>, then create a composite image:
	  ;; (send TANGO:COMPOSITE_IMAGE_CLASS :NEW :VISIBLE self 
	  ;;		<nw-loc>
	  ;;		<tango_image_class_1> {<loc_1> - <nw-loc>} ...
	  ;;		<tango_image_class_2> {<loc_2> - <nw-loc>} ...
	  ;;                 ...)
	  ;; Here, we update the locations of <tango_image_class_i> in-place...
	  (do*
	   ((im_cur  images_desc_list
		     (cdr im_cur))
	    (im_desc (car im_cur)
		     (car im_cur))
	    )
	   ((null im_cur))
	   (rplaca (cdr im_desc)
		   (- (cadr im_desc) nw_loc))
	   )

;;; (format T "images_desc_list after location update '~S'\n" images_desc_list)

	  ;; delete the old individual images
	  (send self :set-selected-image nil)
	  (let* ((tx_list
		  (map 'list #'(lambda (i) (send i :tx_delete)) image-list))
		 (tx_comp
		  (tx-compose-list tx_list)) ;tx-compose-list == TANGO:TX_COMPOSE which can compose >= 50 transs w/o error...
		 )
	    (tango:tx_perform tx_comp)
	    (tango:tx_free tx_comp)	;the garbage collector could do this, but here i explicitly free the transition...
	    (map nil #'tango:tx_free tx_list) ;the garbage collector could do this, but here i explicitly free the transition...
	    )

	  ;; create composite image and return that value
	  (eval (append `(send ,TANGO:COMPOSITE_IMAGE_CLASS :NEW :SHOW :VISIBLE ,self ,nw_loc)
			(apply #'append images_desc_list)
			))
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; given a composite tango_image, deletes it and creates a set of images which
;; were once in the composite image. returns the list of images.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send XTANGO-WIDGET-CLASS :answer :UNGROUP-COMPOSITE-RETURNING-IMAGE-LIST
      '(composite-image)		;an instance of TANGO:COMPOSITE_IMAGE_CLASS
      '(
	(let* (
	       (compo_desc_list (cdr (cddddr (send composite-image :storeon)))) ;CDR CDDDR :STOREON returns '(<location> <im1-class-sym> <im1-location> ... <im2-class-sym> <im2-location> ...)
	       (compo_location  (car compo_desc_list))
	       (result_im_list  NIL)
	       (im_desc	  NIL)
	       (im_desc_tail    NIL)
	       (im_cls          NIL)
	       )

	  ;; delete the composite image prior to recreating individual images.
	  (send self :set-selected-image nil)
	  (send composite-image :tx_delete :perform)

	  (setq compo_desc_list (cdr compo_desc_list)) ;(<im1-class-sym> <im1-location> ... <im2-class-sym> <im2-location> ...)
	  (loop
	   (if (or (null compo_desc_list) ;IF AT END OF compo_desc_list or BEGINNING of a NEW IMAGE DESCRIPTION?
		   (classp (eval (car compo_desc_list))) ;must eval <im-class-symbol> to find out if it represents a class or not...
		   )
	       ;; THEN CREATE PREVIOUSLY ACCUMULATED IMAGE FROM ACUMULATED VALUES IN im_cls AND im_desc, ADD TO result_im_list.
	       (progn
		 (if (and im_desc im_cls) ;if prior image description accumulated...
		     (progn
;;; (format T "creating ~S w/ args ~S\n" im_cls im_desc)
		       (setq result_im_list ;create the image of class 'im_cls', add to result_im_list
			     (cons
			      (apply #'send im_cls :NEW :VISIBLE self ;create new image
				     (+ compo_location (car im_desc)) ;add the composite's location to each subimage's location coord
				     (cdr im_desc)) ;pass remaining image-class specific arguments.
			      result_im_list))
		       (setq im_desc NIL)
		       (setq im_cls NIL)
		       ))
		 (if (null compo_desc_list)
		     (return)		;TERMINATE LOOP
		   (progn
		     (setq im_cls (eval (car compo_desc_list)))
		     (setq compo_desc_list (cdr compo_desc_list))
		     )
		   )
		 )
	     ;; ELSE WE JUST ACCUMULATE MORE ITEMS FROM compo_desc_list into im_desc
	     (progn
	       (if im_desc
		   (setq im_desc_tail (cdr (rplacd im_desc_tail (cons (car compo_desc_list) NIL))))
		 (progn
		   (setq im_desc (cons (car compo_desc_list) NIL))
		   (setq im_desc_tail im_desc)
		   )
		 )
	       (setq compo_desc_list (cdr compo_desc_list))
	       ))
	   )

	  ;; display the new images
	  (let* ((tx_list
		  (map 'list #'(lambda (i) (send i :tx_delay)) result_im_list))
		 (tx_comp
		  (tx-compose-list tx_list)) ;tx-compose-list == TANGO:TX_COMPOSE which can compose >= 50 transs w/o error...
		 )
	    (tango:tx_perform tx_comp)
	    (tango:tx_free tx_comp)	;the garbage collector could do this, but here i explicitly free the transition...
	    (map nil #'tango:tx_free tx_list) ;the garbage collector could do this, but here i explicitly free the transition...
	    )

	  result_im_list		;RETURN VALUE
	  )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; XTANGO-BUTTON-WIDGET-CLASS -- a subclass of TANGO:WIDGET_CLASS/:button
;;; containing a TANGO:WIDGET_CLASS with button semantics displaying a
;;; composite image...
;;;
(setq XTANGO-BUTTON-WIDGET-CLASS	;name of the new subclass
      (send Class :new
	    '()				;no new ivars for this subclass
	    '()                         ;no class variables for subclass
	    TANGO:WIDGET_CLASS		;name of the superclass
 	    )) 

;;;
;;; override TANGO:WIDGET_CLASS instance initializer (method :isnew)...
;;;
;;; (send XTANGO-BUTTON-WIDGET-CLASS [:managed] <widget_name> <widget_parent>
;;;       <image-descr-list>
;;;       ...)
;;; where <image-descr-list> is same format as used for TANGO:COMPOSITE_IMAGE
;;;
(send XTANGO-BUTTON-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  image-descr-list	;same format as for composite...
		  &rest args)
      '(
	;; create 'self', an instance of TANGO:WIDGET_CLASS/:button
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k :button widget_name widget_parent
	       :XMN_PUSH_BUTTON_ENABLED t
	       args
	       )

	;; Set up expose callback to draw the button-image once
	;; the window is created... Subsequent exposes will refresh
	;; the drawing.
	(let ((init_p NIL))
	  (send-super :add_callback :XMN_EXPOSE_CALLBACK
		      '()
		      '(
			(if init_p
			    (send-super :refresh)
			  (progn
			    (send-super :begin_drawing)
			    (apply 'send
				   `(,TANGO:COMPOSITE_IMAGE_CLASS :new :show ,self #C(0.0 0.0)
								  ,@image-descr-list)
				   )
			    (send-super :refresh)
			    (setq init_p t)
			    )
			  )
			))
	  )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following commented out code represents an attempt to create a fake
;; Tango "Drawn Button" style widget without modifying the C source of WINTERP
;; to create an Xtango widget using XmDrawnButtonWidgetClass. Although the
;; code below works fine, I decided it would be better to use a real
;; XmDrawnButton widget in the new XTANGO-BUTTON-WIDGET-CLASS above...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ;;;
;;; ;;; XTANGO-BUTTON-WIDGET-CLASS -- a subclass of XM_FRAME_WIDGET_CLASS
;;; ;;; containing a TANGO:WIDGET_CLASS displaying a composite image...
;;; ;;;
;;; (setq XTANGO-BUTTON-WIDGET-CLASS	;name of the new subclass
;;;       (send Class :new
;;;             '(callback_closure_alist	;new ivars for this subclass
;;; 	      )			
;;;             '()                         ;no class variables for subclass
;;;             XM_FRAME_WIDGET_CLASS	;name of the superclass
;;; 	    )) 
;;; 
;;; ;;;
;;; ;;; override XM_FRAME_WIDGET_CLASS instance initializer (method :isnew)...
;;; ;;;
;;; ;;; (send XTANGO-BUTTON-WIDGET-CLASS [:managed] <widget_name> <widget_parent>
;;; ;;;       <image-descr-list>
;;; ;;;       ...)
;;; ;;; where <image-descr-list> is same format as used for TANGO:COMPOSITE_IMAGE
;;; ;;;
;;; (send XTANGO-BUTTON-WIDGET-CLASS :answer :ISNEW
;;;       '(managed_k widget_name widget_parent
;;; 		  image-descr-list	;same format as for composite...
;;; 		  &rest args)
;;;       '(
;;; 	;; create 'self', an instance of XM_FRAME_WIDGET_CLASS
;;; 	(apply #'send-super :isnew	;call superclass's init to create widget
;;; 	       managed_k widget_name widget_parent
;;; 	       :XMN_SHADOW_TYPE :shadow_out
;;; 	       args
;;; 	       )
;;; 	(let ((tango-w
;;; 	       ;; create an instance of TANGO:WIDGET_CLASS as child of the frame widget
;;; 	       (send TANGO:WIDGET_CLASS :new :managed
;;; 		     (concatenate 'string widget_name "-draw")
;;; 		     self
;;; 		     ;; use default resources of XM_DRAWING_AREA_WIDGET_CLASS underlying
;;; 		     ;; TANGO:WIDGET_CLASS...
;;; 		     ))
;;; 	      )
;;; 
;;; 	  ;; Set up expose callback to draw the button-image once
;;; 	  ;; the window is created... Subsequent exposes will refresh
;;; 	  ;; the drawing.
;;; 	  (let ((init_p NIL))
;;; 	    (send tango-w :add_callback :XMN_EXPOSE_CALLBACK
;;; 		  '(CALLBACK_WIDGET)
;;; 		  '(
;;; 		    (if init_p
;;; 			(send CALLBACK_WIDGET :refresh)
;;; 		      (progn
;;; 			(send CALLBACK_WIDGET :begin_drawing)
;;;			    (apply 'send
;;;				   `(,TANGO:COMPOSITE_IMAGE_CLASS :new :show ,self #C(0.0 0.0)
;;;								  ,@image-descr-list)
;;;				   )
;;; 			(send CALLBACK_WIDGET :refresh)
;;; 			(setq init_p t)
;;; 			)
;;; 		      )
;;; 		    ))
;;; 	    )
;;; 
;;; 	  (let ((button_armed_p NIL)
;;; 		(window_exited_while_armed_p NIL))
;;; 
;;; 	    ;; This causes the xtango-button frame to toggle
;;; 	    ;; the shadow direction when the widget is clicked.
;;; 	    ;; Calls the :XMN_ARM_CALLBACK.
;;; 	    (send tango-w :add_event_handler BUTTON_PRESS_MASK
;;; 		  '(EVHANDLER_XEVENT EVHANDLER_BUTTON)
;;; 		  '(
;;; 		    (case EVHANDLER_BUTTON
;;; 			  (1
;;; 			   (setq button_armed_p t)
;;; 			   (setq window_exited_while_armed_p nil)
;;; 			   (send self :set_values :XMN_SHADOW_TYPE :SHADOW_IN)
;;; 			   ;; call the :XMN_ARM_CALLBACK
;;; 			   ;; (Note BUG w/r/t normal callbacks -- only calls the most recently set callback!)
;;; 			   (let ((assoc-elt (assoc :XMN_ARM_CALLBACK callback_closure_alist)))
;;; 			     (if assoc-elt
;;; 				 (funcall (cdr assoc-elt) tango-w 'CR_ARM EVHANDLER_XEVENT)
;;; 			       ))
;;; 			   )
;;; 			  (2 NIL)	;NO-OP
;;; 			  (3 NIL)	;NO-OP
;;; 			  )
;;; 		    ))
;;; 
;;; 	    ;; this causes the xtango-button frame to toggle
;;; 	    ;; the shadow direction when the widget is clicked.
;;; 	    ;; Calls the :XMN_DISARM_CALLBACK and possibly calls
;;; 	    ;; :XMN_ACTIVATE_CALLBACK when appropriate.
;;; 	    (send tango-w :add_event_handler BUTTON_RELEASE_MASK
;;; 		  '(EVHANDLER_XEVENT EVHANDLER_BUTTON)
;;; 		  '(
;;; 		    (case EVHANDLER_BUTTON
;;; 			  (1
;;; 			   (setq window_exited_while_armed_p nil)
;;; 			   (if button_armed_p
;;; 			       (progn
;;; 				 (setq button_armed_p nil)
;;; 				 (send self :set_values :XMN_SHADOW_TYPE :SHADOW_OUT)
;;; 				 ;; call the :XMN_ACTIVATE_CALLBACK
;;; 				 ;; (Note BUG w/r/t normal callbacks -- only calls the most recently set callback!)
;;; 				 (let ((assoc-elt (assoc :XMN_ACTIVATE_CALLBACK callback_closure_alist)))
;;; 				   (if assoc-elt
;;; 				       (funcall (cdr assoc-elt) tango-w 'CR_ACTIVATE EVHANDLER_XEVENT)
;;; 				     ))				 
;;; 				 ))
;;; 			   ;; call the :XMN_DISARM_CALLBACK
;;; 			   ;; (Note BUG w/r/t normal callbacks -- only calls the most recently set callback!)
;;; 			   (let ((assoc-elt (assoc :XMN_DISARM_CALLBACK callback_closure_alist)))
;;; 			     (if assoc-elt
;;; 				 (funcall (cdr assoc-elt) tango-w 'CR_DISARM EVHANDLER_XEVENT)
;;; 			       ))
;;; 			   )
;;; 			  (2 NIL)	;NO-OP
;;; 			  (3 NIL)	;NO-OP
;;; 			  )
;;; 		    ))
;;; 
;;; 	    ;; FOR COMPATIBILITY w/ MOTIF BUTTONS -- cause the button to dis-arm
;;; 	    ;; itself if the window is exited prior to button-release (prevents
;;; 	    ;; call to :XMN_ACTIVATE_CALLBACK)
;;; 	    (send tango-w :add_event_handler LEAVE_WINDOW_MASK
;;; 		  '()
;;; 		  '(
;;; 		    (if button_armed_p
;;; 			(progn
;;; 			  (setq button_armed_p nil)
;;; 			  (setq window_exited_while_armed_p t)
;;; 			  (send self :set_values :XMN_SHADOW_TYPE :SHADOW_OUT)
;;; 			  ))
;;; 		    ))
;;; 
;;; 	    ;; FOR COMPATIBILITY w/ MOTIF BUTTONS cause the button to re-arm
;;; 	    ;; itself if the window is re-ented after the LEAVE_WINDOW_MASK
;;; 	    ;; evhandler above fired.
;;; 	    (send tango-w :add_event_handler ENTER_WINDOW_MASK
;;; 		  '()
;;; 		  '(
;;; 		    (if window_exited_while_armed_p
;;; 			(progn
;;; 			  (setq button_armed_p t)
;;; 			  (setq window_exited_while_armed_p nil)
;;; 			  (send self :set_values :XMN_SHADOW_TYPE :SHADOW_IN)
;;; 			  ))
;;; 		    ))
;;; 	    )
;;; 	  )
;;; 	))
;;; 
;;; ;; NOTE BUG w/r/t other :ADD_CALLBACKs, this one won't create closure. 
;;; ;; Should probably do this as a macro... Then we could pass in the
;;; ;; environment of the caller as the last arg to EVALHOOK.
;;; (send XTANGO-BUTTON-WIDGET-CLASS :answer :ADD_CALLBACK 
;;;       '(callback_name callback_vars callback_code)
;;;       '(
;;; 	(if (or (eq callback_name :XMN_ARM_CALLBACK) ;can't use CASE here -- please don't ask why....
;;; 		(eq callback_name :XMN_DISARM_CALLBACK)
;;; 		(eq callback_name :XMN_ACTIVATE_CALLBACK))
;;; 	    (setq callback_closure_alist
;;; 		  (cons
;;; 		   (cons callback_name 
;;;   			 (evalhook `(lambda (CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT)
;;; 				      ,@callback_code) nil nil NIL)
;;; 			 )
;;; 		   callback_closure_alist
;;; 		   ))
;;; 	  ;; ELSE -- pass any other callback defs on to superclass :add_callback
;;; 	  (send-super :add_callback callback_name callback_vars callback_code)
;;; 	  )
;;; 	))
;;; 
;;; ;; NOTE BUG w/r/t other :SET_CALLBACKs, this one won't create closure. 
;;; ;; Should probably do this as a macro... Then we could pass in the
;;; ;; environment of the caller as the last arg to EVALHOOK.
;;; (send XTANGO-BUTTON-WIDGET-CLASS :answer :SET_CALLBACK 
;;;       '(callback_name callback_vars callback_code)
;;;       '(
;;; 	(if (or (eq callback_name :XMN_ARM_CALLBACK) ;can't use CASE here -- please don't ask why....
;;; 		(eq callback_name :XMN_DISARM_CALLBACK)
;;; 		(eq callback_name :XMN_ACTIVATE_CALLBACK))
;;; 	    (progn
;;; 	      ;; delete all callbacks w/ <callback_name>
;;; 	      (setq callback_closure_alist
;;; 		    (delete-if
;;; 		     #'(lambda (x) (eq x callback_name))
;;; 		     callback_closure_alist
;;; 		     :key #'car)
;;; 		    )
;;; 	      ;; add the new callback...
;;; 	      (setq callback_closure_alist
;;; 		    (cons
;;; 		     (cons callback_name 
;;; 			   (evalhook `(lambda (CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT)
;;; 					,@callback_code) nil nil NIL)
;;; 			   )
;;; 		     callback_closure_alist
;;; 		     ))
;;; 	      )
;;; 	  ;; ELSE -- pass any other callback defs on to superclass :add_callback
;;; 	  (send-super :set_callback callback_name callback_vars callback_code)
;;; 	  )
;;; 	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "xtango/cls-widget")
