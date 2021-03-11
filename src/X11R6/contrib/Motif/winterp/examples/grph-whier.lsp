;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         grph-whier.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/grph-whier.lsp,v 2.1 1994/06/06 14:43:12 npm Exp $
; Description:  Displays the widget hierarchy under a shell widget. Click
;		on "Show Widget Hierarchy" button, then click mouse on a
;		window created by WINTERP -- a widget hierarchy will be
;		displayed. Click on a "node" within the graph widget --
;		the fully qualified resource name and other info will be 
;		displayed in the Text widget more info will also be printed
;		onto standard output.
;		NOTE: this file requires version of WINTERP compiled with
;		the optional HP Graph Widget
;		(see "WANT_XMGRAPH_WIDGET=-DHP_GRAPH_WIDGET" in Makefile.*)
; Author:       Niels Mayer
; Created:      Tue Dec 24 21:25:50 1991
; Modified:     Sun Jun  5 18:49:56 1994 (Niels Mayer) npm@indeed
; Language:     Lisp
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

(require "xlisp-2.1d/classes")		;define :superclass, etc 
(require "lib-widgets/text-view")	;define Text_Display_Widget_Class

(setq *default_graph_orientation* :horizontal)

;; this global gets set to a WIDGETOBJ when you click on a node in the graph.
;; When experiemnting with applications, this can be used for further programmatic
;; manipulation/access on the selected widget.
(defvar *grph-whier-cur-widget* NIL)

(setq Reference-Widget-Class
      (send Class :new
	    '(ref-widget textdisp-widget) ;new instance vars
	    '()				;no class vars
	    XM_PUSH_BUTTON_GADGET_CLASS)) ;superclass

;;;
;;; Override instance initializer (method :isnew).
;;;
(send Reference-Widget-Class :answer :ISNEW
      '(managed_k widget_name widget_parent
		  ref_w txt_w)
      '(
	;; set instance variables
	(setq ref-widget	ref_w	;the widget to which the 'Reference-Widget-Class' instance refers to
	      textdisp-widget	txt_w)	;Text_Display_Widget_Class for displaying :display-info method result information

	;; create 'self', an instance of XM_PUSH_BUTTON_GADGET_CLASS
	(send-super :isnew		;call superclass's init to create widget
		    managed_k widget_name widget_parent
		    :XMN_HIGHLIGHT_ON_ENTER nil ;setting to T interfere's w/ XmGraph's display of selection
		    :XMN_HIGHLIGHT_THICKNESS 1 ;make sure user hasn't set it to 0
		    :XMN_ALIGNMENT	  :alignment_center 
		    :XMN_SHADOW_THICKNESS 1
		    :XMN_LABEL_STRING
		    (format nil "\"~A\"\n~A"
			    (send ref_w :name)
			    ;; The following looks for 'PNAME' ivar on the class, which
			    ;; is the string print name of the widget-class. Chains up
			    ;; superclasses until it finds a non-NIL 'PNAME'...
			    ;; Returns abbreviated class-name as string.
			    (do* ((clip_len (length "_WIDGET_CLASS"))
				  (cls (send ref_w :class)
				       (send cls :superclass))
				  (str (aref (generic cls) 8)
				       (aref (generic cls) 8))
				  )
				 (str	;stop iterating as soon as we find a non-NIL PNAME field giving the class name string
				  (let ((siz (- (length str) clip_len))) ;DO-RETURN
				    (if (plusp siz) (subseq str 0 siz) "")
				    ))
				 )) 
		    )

	;; create a callback
	(send-super :add_callback :XMN_ACTIVATE_CALLBACK
		    '()
		    `(
		      (send ,self :display-info)
		      ))
	
	self				;METHOD RETURN VALUE
	))


(send Reference-Widget-Class :answer :GET-REFERENCE '()
      '(
	ref-widget
	))


(send Reference-Widget-Class :answer :DISPLAY-INFO '()
      '(
	(setq *grph-whier-cur-widget* ref-widget)

	(send textdisp-widget :disable_redisplay)
	(send textdisp-widget :clear)

	;; print out the fully qualified resource name
	(let* ((cur-w ref-widget)
	       (name (send cur-w :name))
	       (resname (if (string= name "") "*" name))
	       (wildcard-p nil))
	     
	  (loop
	   (cond ((null (setq cur-w (send cur-w :parent))) ;loop up widget tree
		  ;; when you hit the root, print out the loop-accumulated resource name string..
		  (send textdisp-widget :append-string
			(format nil "Xresource Name = ~A\n" resname))
		  (return)		;END OF LOOP
		  ))
	   (setq name (send cur-w :name))
	   (if (string= name "")	;if no name given on this widget
	       (if (not wildcard-p)	;then if previously not in wildcard state, prepend a '*'
		   (setq resname (concatenate 'string "*" resname))
		 (setq wildcard-p t))	;else make sure to print only one '*' at beginning of a unnamed sequence...
	     (if wildcard-p		;if a name was given to a widget and previously wrote a unnamed sequence,
		 (setq resname (concatenate 'string name resname) ;then prepend the current resource to '*...' (we're going up-tree)
		       wildcard-p nil)	;and exit the wildcard state...
	       (setq resname (concatenate 'string name "." resname))) ;else prepend the current resource name with '.' separator
	     )
	   ))

	;; display a bunch of information on the selected widget.
	(let* ((x 0) (y 0) (height 0) (width 0)
	       (cls	     (send ref-widget :class))
	       (wclass       (generic cls))
	       (methods-list (aref wclass 1))
;;;            (ivars-list   (aref wclass 2))
	       (cvars-list   (aref wclass 3))
	       (cvals-list   (aref wclass 4))
	       (superclass   (aref wclass 5))
;;;            (ivar-cnt     (aref wclass 6))
;;;	       (cvar-cnt     (aref wclass 7))
;;;	       (pname        (aref wclass 8))
	       )

	  (send ref-widget :get_values
		:XMN_X		'x
		:XMN_Y		'y
		:XMN_HEIGHT	'height
		:XMN_WIDTH	'width)

	  (send textdisp-widget :append-string
		(format nil "Widget/ParentW = ~S // ~A\nClass/Supercls = ~S // ~A\nHxW+X+Y        = ~Ax~A+~A+~A\nInst.Vars/Vals = ~S // ~S\nCls. Vars/Vals = ~S // ~S\nMETHODS        = ~S"
			ref-widget (send ref-widget :parent)
			cls superclass
			height width x y
			(cdr (send cls :get-ivars-from-class-and-superclasses)) ;CDR removes the IVAR | _bogus_ivar_ | from WIDGET_CLASS
			(cddr (map 'list #'(lambda (x) x) (generic ref-widget))) ;(generic ref-widget) returns #(<class> NIL <ivar0> <ivar1> ...), so use CDDR to remove first two elements...
			cvars-list cvals-list
			methods-list 
			))
	  )
	(send textdisp-widget :show_position 0)
	(send textdisp-widget :set_insertion_position 0)
	(send textdisp-widget :enable_redisplay)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send WIDGET_CLASS :answer :IS-CASCADE-BUTTON '()
      `(
	(let ((cls (send self :class)))
	  (or (eq cls ,XM_CASCADE_BUTTON_GADGET_CLASS) 
	      (eq cls ,XM_CASCADE_BUTTON_WIDGET_CLASS))
	  )				;return BOOLEAN result
	))

;; returns the complete list of ivars from the class and suplerclasses
;; note that the first element returned from this when applying this to
;; a WIDGET_CLASS (or subclass) is | _bogus_ivar_ |, which corresponds
;; to an internal hack instance variable for WINTERP's WIDGET_CLASS.
(send CLASS :answer :GET-IVARS-FROM-CLASS-AND-SUPERCLASSES '()
      '(
	(let* ((wclass       (generic self))
	       (ivars-list   (aref wclass 2))
	       (superclass   (aref wclass 5))
	       )
	  (if superclass
	      (append (send superclass :get-ivars-from-class-and-superclasses ) ivars-list) ;RECURSE...
	    NIL				;terminate recursion by returning NIL
	    )
	  )
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recursive display of <node_w>'s children, where <node_w> is represented by
;; Reference-Widget-Class <super_node_w>.
;; <node_w> must be a composite widget.
(defun show-composite-widget-tree (tree_w text_w super_node_w node_w)
  (do*
   ((node_children_array   (send node_w :get_children))
    (len		   (length node_children_array))
    (node_ref_widget_array (make-array len))
    (arc_array		   (make-array len))
    (i                     0
			   (1+ i)))
   ;; do -- while more children of <node_w>
   ((<= len i)
    (xt_manage_children arc_array)
    (xt_manage_children node_ref_widget_array) ;RETURN/EXIT
    )

   (setf (aref node_ref_widget_array i)
	 (cond
	  ;; handle Motif OOP obfuscation (TM) -- "hidden" dialogues and "hidden" shells
	  ((not (eq (send super_node_w :get-reference)
		    (send (aref node_children_array i) :parent)))
	   (setf (aref node_children_array i) (send (aref node_children_array i) :parent))
	   (send Reference-Widget-Class :new :unmanaged 
		 (format nil "ref-~A" i) tree_w
		 (aref node_children_array i) text_w)
	   )
	  (t
	   (send Reference-Widget-Class :new :unmanaged 
		 (format nil "ref-~A" i) tree_w
		 (aref node_children_array i) text_w)
	   )
	  ))

   (setf (aref arc_array i)
	 (send XM_ARC_WIDGET_CLASS :new :unmanaged
	       "" tree_w
	       :XMN_TO   (aref node_ref_widget_array i)
	       :XMN_FROM super_node_w
	       ))

   ;; recurse on Composite or CascadeButton widgets -- for composites the call to
   ;; :GET_CHILDREN in 'SHOW-COMPOSITE-WIDGET-TREE' will return list of children of
   ;; the widget. For cascades, 'SHOW-CASCADE-WIDGET-TREE' calls :GET_SUB_MENU_WIDGET
   ;; and will subsequently recurse on the resulting ROW-COLUMN widget...
   (let ((cur_w (aref node_children_array i)))
     (cond
      ((send cur_w :is_composite)
       (show-composite-widget-tree tree_w text_w (aref node_ref_widget_array i) cur_w)
       )
      ((send cur_w :is-cascade-button)
       (show-cascade-widget-tree tree_w text_w (aref node_ref_widget_array i) cur_w)
       )
      (T
       super_node_w
       )
       ))
   ))

;; recursive display of <node_w>'s children, where <node_w> is represented by
;; Reference-Widget-Class <super_node_w>.
;; <node_w> must be a cascade-button widget/gadget
;; (i.e. method :IS-CASCADE-BUTTON returns true)
(defun show-cascade-widget-tree (tree_w text_w super_node_w node)
  (let ((submenu_widget (send node :get_sub_menu_widget))
	(node_ref_widget nil))
    
    (if submenu_widget
	(progn
	  (setq node_ref_widget
		(send Reference-Widget-Class :new :managed 
		      "ref" tree_w
		      submenu_widget text_w))
	  (send XM_ARC_WIDGET_CLASS :new :managed
		"" tree_w
		:XMN_TO   node_ref_widget
		:XMN_FROM super_node_w
		)

	  ;; recurse on Composite or CascadeButton widgets -- for composites the call to
	  ;; :GET_CHILDREN in 'SHOW-COMPOSITE-WIDGET-TREE' will return list of children of
	  ;; the widget. For cascades, 'SHOW-CASCADE-WIDGET-TREE' calls :GET_SUB_MENU_WIDGET
	  ;; and will subsequently recurse on the resulting ROW-COLUMN widget...
	  (cond
	   ((send submenu_widget :is_composite)
	    (show-composite-widget-tree tree_w text_w node_ref_widget submenu_widget)
	    )
	   ((send submenu_widget :is-cascade-button)
	    (show-cascade-widget-tree tree_w text_w node_ref_widget submenu_widget)
	    )
	   (T
	    super_node_w
	    )
	   )
	  )
      super_node_w
      )
    ))

;;
;; toplevel function to create a widget-tree displayer
;;
(defun show-widget-tree ()
  (let*
      ((top_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new
	      "grph-whier"
	      :XMN_TITLE	"WINTERP: Widget Hierarchy Inspector"
	      :XMN_ICON_NAME	"W:grph-whier"
	      ))
       (paned_w
	(send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	      "paned" top_w
	      ))
       (rc_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" paned_w
	      :XMN_PACKING	:pack_tight
	      :XMN_ORIENTATION	:horizontal
	      ))
       (sho_pb_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "sho_pb" rc_w
	      :XMN_LABEL_STRING "Show widget hierarchy of ..."
	      ))
       (graph_editable_tbw
	(send XM_TOGGLE_BUTTON_GADGET_CLASS :new :managed
	      "graph_editable_tb" rc_w
	      :XMN_LABEL_STRING "Editable"
	      :XMN_SET nil
	      ))
       (clr_pb_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "clr_pb" rc_w
	      :XMN_LABEL_STRING "Clear All"
	      ))
       (graph_destroy_selected_pbw
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "graph_destroy_selected_pb" rc_w
	      :XMN_LABEL_STRING "Clear Selected"
	      :XMN_SENSITIVE	nil	;note the button is inactive unless :XMN_EDITABLE is true
	      ))
       (graph_relayout_pbw
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "graph_relayout_pb" rc_w
	      :XMN_LABEL_STRING "Re-Layout"
	      ))
       (graph_reorient_pbw
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "graph_reorient_pb" rc_w
	      :XMN_LABEL_STRING "Flip Layout Direction"
	      ))
       (text_w
	(send Text_Display_Widget_Class :new :managed
	      "text" paned_w
	      :XMN_ROWS				7
	      :XMN_COLUMNS			100
	      ))
       (tree_w
	(send XM_GRAPH_WIDGET_CLASS :new :managed :scrolled
	      "w-hier-tree" paned_w
	      :XMN_SIBLING_SPACING	2
	      :XMN_CHILD_SPACING	15
	      :XMN_ARC_DRAW_MODE	:position_fixed
	      :XMN_ORIENTATION		*default_graph_orientation*
	      :XMN_EDITABLE		nil ;setting to T means the buttons won't be pushable
	      :XMN_AUTO_LAYOUT_MODE	:never
	      ))
       )

    ;;; set the size of the graph window pane -- note Motif's lamo use of hidden
    ;;; scroller->drawingarea->graph hierarchy when dealing with a scrolled widget (grrr).
    (send (send (send tree_w :parent) :parent) :set_values
	  :XMN_HEIGHT			 600
	  :XMN_SCROLL_BAR_DISPLAY_POLICY :static
	  )

    (send sho_pb_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (winterp_show_busy t)	;set busy cursor...
	    (send ,tree_w :unmanage)
	    (progv '(*breakenable*) '(nil)
		   (unwind-protect	;remanage tree_w and unset busy cursor no matter what happens
		       (let*
			   ((root_w
			     (do ((w (get_moused_widget) ;initialize to widget designated by user...
				     (send w :parent))) ;increment up tree...
				 ((send w :is_shell) ;loop until you hit a shell...
				  w)))
			    (super_root_w
			     (send Reference-Widget-Class :new :managed ;create the widget representing the root of the widget tree
				   "ref-root" ,tree_w root_w ,text_w))
			    )
			 ;; recursively display children and cascade-button-submenus of root_w
			 (show-composite-widget-tree ,tree_w ,text_w super_root_w root_w)
			 )
		     ;; always unwind-protect 
		     (progn
		       (send ,tree_w :layout)
		       (send ,tree_w :manage)
		       (winterp_show_busy nil) ;always unset busy cursor via unwind-protect 
		       )
		     ))
	    ))

    (send clr_pb_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (send ,tree_w :destroy_all_nodes)
	    ))

    (send graph_editable_tbw :set_callback :XMN_VALUE_CHANGED_CALLBACK '(CALLBACK_SET)
	  `(
	    (send ,tree_w :set_values :XMN_EDITABLE CALLBACK_SET)
	    (send ,graph_destroy_selected_pbw :set_values :XMN_SENSITIVE CALLBACK_SET)
	    (send ,text_w :set_values :XMN_SENSITIVE (not CALLBACK_SET))
	    ))

    (send graph_destroy_selected_pbw :set_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (let* ((array-sel-widgets   (send ,tree_w :get_selected_nodes))
		   (num-sel-widgets     (length array-sel-widgets))
		   )
	      (do 
	       ((i 0 (1+ i)))
	       ((eq i num-sel-widgets))
	       (progv '(*breakenable*) '(nil) ;temporarily unset *breakenable* s.t. errset works even when in "debugging" mode.
		      (errset (send (aref array-sel-widgets i) :destroy)) ;trap errors incase widget has already been destroyed via :destroy on it's parent...
		      )
	       )
	      )))

    (send graph_relayout_pbw :set_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (send ,tree_w :layout)
	    ))

    (send graph_reorient_pbw :set_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (send ,tree_w :set_values :xmn_reorient t)

	    (cond
	     ((eq *default_graph_orientation* :horizontal)
	      (setq *default_graph_orientation* :vertical)
	      )
	     ((eq *default_graph_orientation* :vertical)
	      (setq *default_graph_orientation* :horizontal)
	      )
	     )
	    ))

    (send top_w :realize)

    ;; remove paned window sash from rc_w
    (let (height)
      (send rc_w :get_values :xmn_height 'height)
      (send rc_w :set_values
	    :XMN_PANE_MAXIMUM height
	    :XMN_PANE_MINIMUM height
	    )
      )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-widget-tree)
