; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         graph.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/graph.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Experiment with XmGraph and XmArc widgets. WINTERP must be
;		compiled with option WANT_XMGRAPH_WIDGET=-DHP_GRAPH_WIDGET.
; Author:       Niels P. Mayer
; Created:      January 1994
; Modified:     Mon Jun  6 00:27:08 1994 (Niels Mayer) npm@indeed
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

(setq graph-top-w 
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "winterp-XmGraph-test"
	    ))
(setq graph-w
      (send XM_GRAPH_WIDGET_CLASS :new :managed :scrolled
	    "graph" graph-top-w
	    :XMN_SIBLING_SPACING	3
	    :XMN_CHILD_SPACING		8
	    :XMN_ARC_DRAW_MODE		:position_fixed
	    :XMN_AUTO_LAYOUT_MODE	:never
	    :XMN_ORIENTATION		:horizontal
	    :XMN_EDITABLE		t 
	    ))
(send graph-top-w :realize)

;;; (send graph-w :add_event_handler POINTER_MOTION_MASK
;;;       '(EVHANDLER_XEVENT)
;;;       '(
;;; 	(format T "~A\n"
;;; 		(get_event_coords EVHANDLER_XEVENT)
;;; 		)
;;; 	))

(setq pb1-w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "pb-1" graph-w
	    :XMN_HIGHLIGHT_ON_ENTER nil ;setting to T interfere's w/ XmGraph's display of selection
	    :XMN_HIGHLIGHT_THICKNESS 1	;make sure user hasn't set it to 0
	    :XMN_ALIGNMENT	  :alignment_center 
	    :XMN_SHADOW_THICKNESS 1
	    ))
(setq pb2-w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "pb-2" graph-w
	    :XMN_HIGHLIGHT_ON_ENTER nil ;setting to T interfere's w/ XmGraph's display of selection
	    :XMN_HIGHLIGHT_THICKNESS 1	;make sure user hasn't set it to 0
	    :XMN_ALIGNMENT	  :alignment_center 
	    :XMN_SHADOW_THICKNESS 1
	    ))

(send XM_ARC_WIDGET_CLASS :new :managed
      "" graph-w
      :XMN_TO   pb1-w
      :XMN_FROM pb2-w
      :XMN_ARC_DIRECTION :directed
      )

(setq pb3-w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "pb-3" graph-w
	    :XMN_HIGHLIGHT_ON_ENTER nil ;setting to T interfere's w/ XmGraph's display of selection
	    :XMN_HIGHLIGHT_THICKNESS 1	;make sure user hasn't set it to 0
	    :XMN_ALIGNMENT	  :alignment_center 
	    :XMN_SHADOW_THICKNESS 1
	    ))

(send XM_ARC_WIDGET_CLASS :new :managed
      "" graph-w
      :XMN_TO   pb2-w
      :XMN_FROM pb3-w
      :XMN_ARC_DIRECTION :directed
      )

(setq pb4-w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "pb-4" graph-w
	    :XMN_HIGHLIGHT_ON_ENTER nil ;setting to T interfere's w/ XmGraph's display of selection
	    :XMN_HIGHLIGHT_THICKNESS 1	;make sure user hasn't set it to 0
	    :XMN_ALIGNMENT	  :alignment_center 
	    :XMN_SHADOW_THICKNESS 1 
	    ))

(send XM_ARC_WIDGET_CLASS :new :managed
      "" graph-w
      :XMN_TO   pb4-w
      :XMN_FROM pb3-w
      :XMN_ARC_DIRECTION :directed
      )

(send XM_ARC_WIDGET_CLASS :new :managed
      "" graph-w
      :XMN_TO   pb1-w
      :XMN_FROM pb4-w
      :XMN_ARC_DIRECTION :directed
      )

(send graph-w :layout)

(map nil #'(lambda (x) (send x :destroy))
     (send graph-w :GET_NODES))
(map nil #'(lambda (x) (send x :destroy))
     (send graph-w :GET_ARCS))

(setq node-w (get_moused_widget))
(setq arc-w (get_moused_widget))

(send graph-w :CENTER_AROUND_WIDGET node-w)
(send graph-w :DESTROY_ALL_ARCS)
(send graph-w :DESTROY_ALL_NODES)
(send graph-w :DESTROY_SELECTED_ARCS_OR_NODES)
(send graph-w :GET_ARCS)
(send graph-w :GET_NODES)
(send graph-w :GET_ARCS_BETWEEN_NODES from-node-w to-node-w)
(send graph-w :GET_NODE_ARCS node-w)
;;; (send graph-w :GET_ARC_NODES arc-w) ;;; core dumps if wrong widget!
(send graph-w :GET_ROOTS)
(send graph-w :GET_SELECTED_ARCS)
(send graph-w :GET_SELECTED_NODES)
(send graph-w :INPUT_OVER_ARC x-pos y-pos)
;;; (send graph-w :INSERT_ROOTS...)
(send graph-w :IS_POINT_IN_ARC x-pos y-pos)
(send graph-w :IS_SELECTED_ARC arc-w)
(send graph-w :IS_SELECTED_NODE node-w)
(send graph-w :MOVE_ARC arc-w from-node-w to-node-w)
(send graph-w :MOVE_NODE node-w x-pos y-pos)
(send graph-w :NUM_ARCS)
(send graph-w :NUM_NODES)
(send graph-w :NUM_NODE_ARCS node-w)
(send graph-w :NUM_ROOTS)
(send graph-w :NUM_SELECTED_ARCS)
(send graph-w :NUM_SELECTED_NODES)
(send graph-w :MOVE_ALL delta-x delta-y)
(send graph-w :LAYOUT)
(send graph-w :RELAY_SUBGRAPH node-w)
(send graph-w :REMOVE_ARC_BETWEEN_NODES node1-w node2-w)
;;; (send graph-w :REMOVE_ROOTS...)
(send graph-w :SELECT_ARC arc-w)
;;; (send graph-w :SELECT_ARCS...)
;;; (send graph-w :SELECT_NODES...)
(send graph-w :UNSELECT_ARC arc-w)
;;; (send graph-w :UNSELECT_ARCS)
;;; (send graph-w :UNSELECT_NODES)
(send graph-w :SELECT_NODE node-w)
(send graph-w :UNSELECT_NODE node-w)


(send graph-w :add_callback :XMN_DEFAULT_ACTION_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_SELECTED_WIDGETS
	CALLBACK_NUM_SELECTED_WIDGETS
	CALLBACK_SELECTED_ARCS
	CALLBACK_NUM_SELECTED_ARCS
	CALLBACK_OLD_TO
	CALLBACK_OLD_FROM
	CALLBACK_NEW_TO
	CALLBACK_NEW_FROM)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_SELECTED_WIDGETS='~A'; CALLBACK_NUM_SELECTED_WIDGETS='~A'; CALLBACK_SELECTED_ARCS='~A'; CALLBACK_NUM_SELECTED_ARCS='~A'; CALLBACK_OLD_TO='~A'; CALLBACK_OLD_FROM='~A'; CALLBACK_NEW_TO='~A'; CALLBACK_NEW_FROM='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_SELECTED_WIDGETS CALLBACK_NUM_SELECTED_WIDGETS
		CALLBACK_SELECTED_ARCS CALLBACK_NUM_SELECTED_ARCS CALLBACK_OLD_TO CALLBACK_OLD_FROM CALLBACK_NEW_TO CALLBACK_NEW_FROM
		)
	))

(send graph-w :add_callback :XMN_NEW_ARC_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
;;	CALLBACK_OLD_TO
;;	CALLBACK_OLD_FROM
	CALLBACK_NEW_TO
	CALLBACK_NEW_FROM
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_NEW_TO='~A'; CALLBACK_NEW_FROM='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT
;;		CALLBACK_OLD_TO CALLBACK_OLD_FROM
		CALLBACK_NEW_TO CALLBACK_NEW_FROM
		)
	))

(send graph-w :add_callback :XMN_NEW_NODE_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT
		)
	))

(send graph-w :add_callback :XMN_NODE_MOVED_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_SELECTED_WIDGETS
	CALLBACK_NUM_SELECTED_WIDGETS
	CALLBACK_SELECTED_ARCS
	CALLBACK_NUM_SELECTED_ARCS
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_SELECTED_WIDGETS='~A'; CALLBACK_NUM_SELECTED_WIDGETS='~A'; CALLBACK_SELECTED_ARCS='~A'; CALLBACK_NUM_SELECTED_ARCS='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_SELECTED_WIDGETS CALLBACK_NUM_SELECTED_WIDGETS
		CALLBACK_SELECTED_ARCS CALLBACK_NUM_SELECTED_ARCS
		)
	))

(send graph-w :add_callback :XMN_ARC_MOVED_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_OLD_TO
	CALLBACK_OLD_FROM
	CALLBACK_NEW_TO
	CALLBACK_NEW_FROM)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_OLD_TO='~A'; CALLBACK_OLD_FROM='~A'; CALLBACK_NEW_TO='~A'; CALLBACK_NEW_FROM='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT
		CALLBACK_OLD_TO CALLBACK_OLD_FROM CALLBACK_NEW_TO CALLBACK_NEW_FROM
		)
	))

(send graph-w :add_callback :XMN_SELECT_NODE_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_SELECTED_WIDGETS
	CALLBACK_NUM_SELECTED_WIDGETS
	CALLBACK_SELECTED_ARCS
	CALLBACK_NUM_SELECTED_ARCS
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_SELECTED_WIDGETS='~A'; CALLBACK_NUM_SELECTED_WIDGETS='~A'; CALLBACK_SELECTED_ARCS='~A'; CALLBACK_NUM_SELECTED_ARCS='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_SELECTED_WIDGETS CALLBACK_NUM_SELECTED_WIDGETS
		CALLBACK_SELECTED_ARCS CALLBACK_NUM_SELECTED_ARCS
		)
	))

(send graph-w :add_callback :XMN_DESELECT_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT
		)
	))

(send graph-w :add_callback :XMN_SELECT_ARC_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_SELECTED_WIDGETS
	CALLBACK_NUM_SELECTED_WIDGETS
	CALLBACK_SELECTED_ARCS
	CALLBACK_NUM_SELECTED_ARCS
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_SELECTED_WIDGETS='~A'; CALLBACK_NUM_SELECTED_WIDGETS='~A'; CALLBACK_SELECTED_ARCS='~A'; CALLBACK_NUM_SELECTED_ARCS='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_SELECTED_WIDGETS CALLBACK_NUM_SELECTED_WIDGETS
		CALLBACK_SELECTED_ARCS CALLBACK_NUM_SELECTED_ARCS
		)
	))

(send graph-w :add_callback :XMN_SELECT_SUBGRAPH_CALLBACK
      '(
	CALLBACK_WIDGET
	CALLBACK_REASON
	CALLBACK_XEVENT
	CALLBACK_SELECTED_WIDGETS
	CALLBACK_NUM_SELECTED_WIDGETS
	CALLBACK_SELECTED_ARCS
	CALLBACK_NUM_SELECTED_ARCS
	)
      '(
	(format T 
		"\nCALLBACK_WIDGET='~A'; CALLBACK_REASON='~A'; CALLBACK_XEVENT='~A'; CALLBACK_SELECTED_WIDGETS='~A'; CALLBACK_NUM_SELECTED_WIDGETS='~A'; CALLBACK_SELECTED_ARCS='~A'; CALLBACK_NUM_SELECTED_ARCS='~A'\n"
		CALLBACK_WIDGET CALLBACK_REASON CALLBACK_XEVENT CALLBACK_SELECTED_WIDGETS CALLBACK_NUM_SELECTED_WIDGETS
		CALLBACK_SELECTED_ARCS CALLBACK_NUM_SELECTED_ARCS
		)
	))
