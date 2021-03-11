;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         grph-sexpr.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/grph-sexpr.lsp,v 2.1 1994/06/06 14:43:13 npm Exp $
; Description:  Using XM_GRAPH_WIDGET_CLASS to display a lisp s-expression
;		(or any lisp list) as a tree. 
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ;; some tests of SHOW-SEXP
;  ;; (show-sexp '((5 6 7 '(6 7) 7 "quackity" #(0 1 2 3 4))))
;  ;; (show-sexp (map 'list #'(lambda (i) i) (generic #'show-sexp)))
;  ;; (show-sexp (map 'list #'(lambda (i) i) (generic #'show-sexp-aux)))
;  ;; (show-sexp (map 'list #'(lambda (i) i) (generic #'pp)))
;  ;; (show-sexp (map 'list #'(lambda (i) i) (generic #'pp1)))
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Author:       Niels Mayer
; Created:      Mon Dec 23 22:51:37 1991
; Modified:     Sun Jun  5 18:49:17 1994 (Niels Mayer) npm@indeed
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

(require "xlisp-2.1d/common")		;define WITH-OUTPUT-TO-STRING
(require "xlisp-2.1d/pp")		;define PP
(require "lib-utils/show-busy")		;define WINTERP-SHOW-BUSY-PROGN

(defun show-sexp (s)
  (let* ((top_w
	  (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "treeshl"
		:XMN_GEOMETRY		"500x500+1+1"
		:XMN_TITLE		"WINTERP: XmGraph S-Expression Display"
		:XMN_ICON_NAME		"W:grph-sexpr"
		))
	 (tree_w
	  (send XM_GRAPH_WIDGET_CLASS :new :unmanaged :scrolled
		"dag" top_w
		:XMN_SIBLING_SPACING    3
		:XMN_CHILD_SPACING	15
		:XMN_ARC_DRAW_MODE	:position_fixed
		:XMN_AUTO_LAYOUT_MODE	:never
		:XMN_ORIENTATION	:vertical
		:XMN_EDITABLE		t 
		))
	 )
    (show-sexp-aux tree_w nil s)
    (send tree_w :layout)
    (send tree_w :manage)    
    (send top_w :realize)
    ))

(defun show-sexp-aux (tree_w super_node_w s)
  (let ((nodes_array (make-array (length s)))
	(arcs_array  (make-array (length s))))
    (do ((li s (cdr li))
	 (i  0 (1+ i))
	 )
	((null li)
	 (xt_manage_children nodes_array)
	 (xt_manage_children (delete NIL arcs_array))
	 )
	(setf (aref nodes_array i)
	      (send XM_PUSH_BUTTON_GADGET_CLASS :new :unmanaged 
		    (format nil "pb~A" i) tree_w
		    :XMN_ALIGNMENT	  :alignment_beginning
		    :XMN_SHADOW_THICKNESS 1
		    :XMN_LABEL_STRING
		    (let ((str (with-output-to-string (strm) (pp (car li) strm))))
		      (subseq str 0 (1- (length str))))	;trim trailing newline
		    ))
	(setf (aref arcs_array i)
	      (if super_node_w
		  (send XM_ARC_WIDGET_CLASS :new :unmanaged
			(format nil "arc~A" i) tree_w
			:XMN_TO   (aref nodes_array i)
			:XMN_FROM super_node_w
			)
		NIL
		))

	(if (consp (car li))
	    (show-sexp-aux tree_w (aref nodes_array i) (car li))
	  )
	)
    ))


;; after evaling this, click on a widget containing user-defined methods.
;; this will print out the code in the methods as a graph...
(defun show-widget-methods (widg)
  (show-sexp 
   (map 'list
	#'(lambda (i)
	    (case (type-of i)
		  ('SUBR		i)
		  ('FSUBR		i)
		  ('CLOSURE	(map 'list #'(lambda (i) i) (generic i)))
		  (T		(error "???" i))
		  ))
	(map 'list
	     #'cdr			;get the SUBR/FSUBR/CLOSURE part of methods
	     (aref (generic (send widg :class)) 1)))) ;get methods list
  )


(let* (toplevel_w grph-sexpr-pb-w)
  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "grph-sexp"
	      :XMN_TITLE	"WINTERP: grph-sexpr"
	      :XMN_ICON_NAME	"W:grph-sexpr"
	      ))
  (setq grph-sexpr-pb-w
	(send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	      "grph-sexpr-pb" toplevel_w
	      :XMN_LABEL_STRING "Graph Methods of Selected Widget"
	      ))

  (send toplevel_w :realize)

  (send grph-sexpr-pb-w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (winterp-show-busy-progn
	   (show-widget-methods (get_moused_widget))
	   )
	  ))
  )

