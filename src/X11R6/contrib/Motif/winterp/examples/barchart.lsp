; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         barchart.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/barchart.lsp,v 2.2 1994/06/06 14:43:22 npm Exp $
; Description:  A 256 elt barchart which meters the number of elements/hashbucket
;		within WINTERP's *saved_objs* hashtable. This measurement
;		demonstrates the heisenberg uncertainty principle, :-) since
;		repeated re-evaluation of this form will show the growth of
;		each list-hashbucket within the hashtable *saved_objs*.
;		The barchart element in this app is a gadget, so these show
;		up on the graph in subsequent invocations of this app.
;		NB: *saved_objs* is a globally accessible storage area for all
;		objects in the outside world that WINTERP must interface
;		with -- in particular, WIDGETS, CALLBACKOBJs, TIMEOUTOBJs,
;		EVENTHANDLEROBJs INPUTCBOBJs, etc. 
; Author:       Niels P. Mayer
; Created:      Thu Mar 18 19:18:25 1993
; Modified:     Sun Jun  5 18:16:15 1994 (Niels Mayer) npm@indeed
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

(let ((rc_w nil)
;;;   (i 0)
      (x 1)
      (h 0)
      ;; need to compute this ahead of time since widgets created
      ;; below (especially within the 'map' call) end up affecting
      ;; the hashbucket count of subsequent hashbuckets...
      (bucket-length-list (map 'list #'length *saved_objs*))
      )

  (setq rc_w
	(send XM_BULLETIN_BOARD_WIDGET_CLASS :new :dialog
	      "bulletin-board-dialog" *toplevel_widget*
	      ;; :xmn_delete_response==:destroy means that when we
	      ;; close the barchart, we also destroy all it's subwidgets
	      ;; (the barchart-element-gadgets below)... This is a
	      ;; dialog-shell resource.
	      :XMN_DELETE_RESPONSE	:destroy
	      :XMN_TITLE		"WINTERP: barchart of *saved_objs* hashbuckets"
	      :XMN_ICON_NAME		"W:barchart"
	      :XMN_BACKGROUND		"dimgrey"
	      :XMN_FOREGROUND		"red"
	      ))

  (map nil
       #'(lambda (len)
;;;	   (send
	    (send XM_SEPARATOR_GADGET_CLASS :new :managed "x" rc_w
		  :XMN_WIDTH			1
		  :XMN_HEIGHT			(setq h (+ 2 (* 5 len))) ;offset by 2 cuz want a tiny "dot" for zero-length buckets
		  :XMN_X			(setq x (+ x 3)) ;increment barchart position by 3 each time.
		  :XMN_Y			(- 400 h) ;buckets grow upwards
		  :XMN_ORIENTATION		:vertical
		  :XMN_SEPARATOR_TYPE		:single_line		  
		  :XMN_HIGHLIGHT_THICKNESS	0
		  :XMN_SHADOW_THICKNESS		0
		  :XMN_MARGIN			0
		  :XMN_SENSITIVE		nil
		  )
;;;	    :add_callback :XMN_DESTROY_CALLBACK '()
;;;	    `(
;;;	      (format T
;;;		      ,(format NIL
;;;			       "savedobj-hashbucket ~A length ~A\n"
;;;			       i len)
;;;		      )
;;;	      ))
;;;	   (setq i (1+ i))
	   )
       bucket-length-list
       )
    
  (send rc_w :manage)
  )



