; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         barchart.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/barchart.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  A 256 elt barchart which meters the number of elements/hashbucket
;		within WINTERP's *saved_objs* hashtable.
;		The barchart element in this app is a TANGOIMAGEOBJ.
;		See barchart.lsp for a similar display using gadgets.
; Author:       Niels P. Mayer
; Created:      Thu Mar 18 19:18:25 1993
; Modified:     Mon Jun  6 03:23:59 1994 (Niels Mayer) npm@indeed
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

(let ((form_w nil)
      (tango_w nil)
      (x 0.0)
      ;; need to compute this ahead of time since widgets created
      ;; below (especially within the 'map' call) end up affecting
      ;; the hashbucket count of subsequent hashbuckets...
      (bucket-length-list (map 'list #'length *saved_objs*))
      (sav-obj-size (float (length *saved_objs*)))
      (callback_obj nil)
      )

  (setq top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	      "barchart"
	      :XMN_TITLE		"WINTERP: Xtango barchart of *saved_objs*"
	      :XMN_ICON_NAME		"W:barchart"
	      ))

  (setq tango_w
	(send TANGO:WIDGET_CLASS :new :managed
	      "tango" top_w
	      :XMN_HEIGHT		300
	      :XMN_WIDTH		300
	      ))

  (progn
    (send top_w :realize)
    (send tango_w :forced_expose_update) ;wait until exposed to ensure windows created for :begin_drawing call
    (send tango_w :begin_drawing)	;must call this after :realize
    )

  (map nil
       #'(lambda (len)
	   (send TANGO:LINE_IMAGE_CLASS :new tango_w
		 (complex (/ (setq x (1+ x)) sav-obj-size) 1.0)	;location_coord
		 (complex 0.0 (/ len -30.0)) ;size_coord
		 7			;color
		 0.0			;wid
		 1.0			;sty
		 :no_arrow		;arrow
		 )
	   )
       bucket-length-list
       )

  ;; now that the drawing is done, display it.
  (send tango_w :refresh)


  (send tango_w :add_callback :xmn_expose_callback
	'(CALLBACK_WIDGET)
	'(
	  (send CALLBACK_WIDGET :refresh)
	  ))

;;;  (setq callback_obj
;;;	(send tango_w :add_callback :xmn_expose_callback
;;;	      '(CALLBACK_WIDGET)
;;;	      '(
;;;		;; remove this expose callback
;;;		(xt_remove_callback callback_obj)
;;;
;;;		;; initialize the tango widget for drawing
;;;		(send CALLBACK_WIDGET :begin_drawing)
;;;		
;;;		(map nil
;;;		     #'(lambda (len)
;;;			 (send TANGO:LINE_IMAGE_CLASS :new CALLBACK_WIDGET
;;;			       (complex (/ (setq x (1+ x)) sav-obj-size) 1.0) ;location_coord
;;;			       (complex 0.0 (/ len -30.0)) ;size_coord
;;;			       7	;color
;;;			       0.0	;wid
;;;			       1.0	;sty
;;;			       :no_arrow ;arrow
;;;			       )
;;;			 )
;;;		     bucket-length-list
;;;		     )
;;;
;;;		;; now that the drawing is done, display it.
;;;		(send tango_w :refresh)
;;;
;;;		;; set up the real expose callback... this must be
;;;		;; called on an initialized tango widget
;;;		(send tango_w :add_callback :xmn_expose_callback
;;;		      '(CALLBACK_WIDGET)
;;;		      '(
;;;			(send CALLBACK_WIDGET :refresh)
;;;			))
;;;		))
;;;	)
  )
