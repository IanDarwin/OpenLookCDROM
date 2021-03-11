;; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         XmGraph.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/XmGraph.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Define utility methods for WINTERP's built-in
;		XM_GRAPH_WIDGET_CLASS
; Author:       Niels P. Mayer
; Created:      Fri May 13 04:33:26 1994
; Modified:     Mon Jun  6 01:05:44 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define WIDGET_CLASS method :GET

;;
;; (send <graph-w> :tracking-locate-in-xmgraph-coordinate-system
;;		   <fontcursor-FIXNUM> [<confine-to-p>])
;;	--> #C(<x-loc> <y-loc>)
;;
;; Use this method for returning the X/Y location of user's selection of insertion
;; point in Scrolled XmGraph widget.
;; TODO -- replace all this crap w/ call to XM_TRACKING_EVENT, get x/y from that...
;; however, will still need to remap to xmgraph-coord-system's hairy case of
;; having a scrolled window.
;;
(send XM_GRAPH_WIDGET_CLASS :answer :tracking-locate-in-xmgraph-coordinate-system
      '(fontcursor-FIXNUM &optional confine-to-p)
      '(
	(XM_TRACKING_LOCATE self fontcursor-FIXNUM confine-to-p) ;X11/cursorfont.h:#define XC_crosshair 34
	(let* ((mousloc	(GET_MOUSE_LOCATION)
			)
	       (widgs		(do ((li nil)
				     (wi (send (send self :parent) :parent)
					 (send wi :parent))
				     )
				    ((null wi) ;stop when no more parent widgets 
				     (cdr li) ;return the list of all enclosing widgets excluding toplevel shell
				     )
				    (setq li (cons wi li))
				    ))
	       (x		(- (car mousloc)
				   (apply #'+ (map 'list #'(lambda (x) (send x :get :xmn_x)) widgs))
				   (send self :get :xmn_x)
				   (send (send self :parent) :get :xmn_x)
				   ))
	       (y		(- (cdr mousloc)
				   (apply #'+ (map 'list #'(lambda (x) (send x :get :xmn_y)) widgs))
				   (send self :get :xmn_y)
				   (send (send self :parent) :get :xmn_y)
				   ))
	       )
	  (complex x y)			;RETURN
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/XmGraph")
