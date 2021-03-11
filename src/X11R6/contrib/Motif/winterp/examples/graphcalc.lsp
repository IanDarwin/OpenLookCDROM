;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         graphcalc.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/graphcalc.lsp,v 2.11 1994/06/06 14:43:16 npm Exp $
; Description:  A calculator with "direct manipulation" graphic display of
;               previous results. Expressions may be entered by typing
;		them in on the mock-keyboard, inputting the value into
;		the XmText widget, or by clicking on a "node" in the
;		XmGraph widget. Operators are entered by clicking the
;		operator buttons ('*' '/' '-' '+') on the mock-keyboard.
;		Expressions must be terminated by clicking on the '==' 
;		operator button.
;
;		Users may also enter expressions in infix notation into the text
;		widget, and these will be displayed with disambiguation provided
;		by precedence rules in the code below. As a test of this, paste
;		the following into the XmText widget
;		[ HW1 + HW2 + HW3 + 2 * MT1 + HW4 + HW5 + 2 * MT2 + HW6 + HW7 + 4 * FIN ] / 15
;		then hit the <return> key and watch the results. TODO: prevent
;		incremental refresh in XmGraph widget while adding many widgets in 
;		a row -- toggle ':XMN_AUTO_LAYOUT_MODE' between ':always' and ':never'.
;
;		NOTE: this file requires version of WINTERP compiled with
;		the optional HP Graph Widget
;		(see "WANT_XMGRAPH_WIDGET=-DHP_GRAPH_WIDGET" in Makefile.*)
; Author:       Niels Mayer
; Created:      Sat Jul  7 13:10:12 1990
; Modified:     Sun Jun  5 18:43:52 1994 (Niels Mayer) npm@indeed
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

(setq *default_graph_orientation* :horizontal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test: (display-equation '( 5 * [6 + 7] * 8 + 9 * [10 + 11] ))
;; test: (display-equation '( 5 ^ 6 + 6 * 7 + 8 / 9 * [10 + 11] ))
;; test: (display-equation '( 5 ^ A + (sin 300.0) * B + 1 / 222.3 * [C + D] ))
;; test: (display-equation '( x mod 10 * 11 + 12 / 13 / 14 / 15 ))
;; test: (display-equation '( [ HW1 + HW2 + HW3 + 2 * MT1 + HW4 + HW5 + 2 * MT2 + HW6 + HW7 + 4 * FIN ] / 15 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun display-equation (algebraic-expression-list)
  (let ((result-graphnode (display-s-expr (inf-to-pre algebraic-expression-list))))
;;;    (send graph_w :layout)
    (send graph_w :CENTER_AROUND_WIDGET result-graphnode)
    result-graphnode			;return
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun display-s-expr (s-expr)
  (let (result)
    (case (type-of s-expr)
	  (FIXNUM
	   (setq result
		 (send *calc_terminal_value_widget_class* :new
		       s-expr))
	   )
	  (FLONUM
	   (setq result
		 (send *calc_terminal_value_widget_class* :new
		       s-expr)) 
	   )
	  (COMPLEX
	   (setq result
		 (send *calc_terminal_value_widget_class* :new
		       s-expr)) 
	   )
	  (SYMBOL
	   (setq result		
		 (send *calc_terminal_variable_widget_class* :new
		       s-expr))
	   )
	  (CONS
	   (if (eq (type-of (car s-expr)) 'CONS)
	       (error (format nil "display-s-expr: invalid s-expr -- ~A\n\ttype=~A car of s-expr=~A\n"
			      s-expr (type-of (car s-expr)) (car s-expr) )))
	   (let ((operator-representor-class (get-operator-class (car s-expr))))
	     (if operator-representor-class ;get-operator-class returns NIL if the operator was not defined with make-operator
		 (setq result		;create a non-terminal node, an instance of subclass of *calc_operator_widget_class* (see make-operator)
		       (send operator-representor-class :new (cdr s-expr)))
	         (setq result		;if the functor is not a recognized operator, then treat entire sexp as a terminal node.
		       (send *calc_terminal_sexp_widget_class* :new s-expr))
		 ))
	   )
	  )
    result
    ))

;==============================================================================
;====================== conversion from infix to prefix========================
;==============================================================================
;;; Path: hplabsz!hplabs!hp-sdd!ucsdhub!sdcsvax!ucsd!ames!haven!umd5!jonnyg
;;; From: jonnyg@umd5.umd.edu (Jon Greenblatt)
;;; Newsgroups: comp.lang.lisp,comp.lang.lisp.x
;;; Subject: Re: Algebraic syntax...
;;; Keywords: syntax parse
;;; Message-ID: <4924@umd5.umd.edu>
;;; Date: 19 May 89 19:40:31 GMT
;;; References: <4919@umd5.umd.edu>
;;; Reply-To: jonnyg@umd5.umd.edu (Jon Greenblatt)
;;; Organization: University of Maryland, College Park
;;; Lines: 106
;;; Xref: hplabsz comp.lang.lisp:1655
;;; 
;;; 
;;; 	 Well I got one reply to my request in the for Algebraic conversion.
;;;  I took this information and made it a read macro for xlisp. This is
;;;  realy basic code and I plan to eventialy include a optimizer to pull
;;;  out constant expressions, reduce the number of function calls, and
;;;  add support for unary operators. Given this is so simple I though I would
;;;  post it in this form for right now. If anyone was wondering how to write
;;;  read macros in Xlisp, here is how its done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An infix to prefix converter for algebraic expressions.
;;; From Winston and Horn, Second Edition, pp 185-189.
;;;
;
;	Adapted as a lisp macro by:
;		Jonathan Roger Greenblatt (jonnyg@rover.umd.edu)
;		University of Maryland at College Park
;
;
;	(usage:
;
;		[ <expr> <oper> <expr> ( <oper> <expr> ) ... ]
;
;	<expr>: a lisp expresion.
;	<oper>: =,+,-,*,/,mod.**,^
;
;	Note: [ and ] are part of the syntax, ( and ) mean this part is
;				optional.
;
;	Examples:
;
;		[a = 7 * 5 + 4]
;		[b = 7 + (sin (float a)) + (float [a / 7]) * [3 + a]]
;
;	These are expanded to:
;
;		(SETQ A (+ (* 7 5) 4))
;		(SETQ B (+ (+ 7 (SIN (FLOAT A))) (* (FLOAT (/ A 7)) (+ 3 A))))
;
;

(defun inf-to-pre (ae)
  (labels
	((weight (operator)
	  (case operator
	    (= 0)
	    (+ 1)
	    (- 1)
	    (* 2)
	    (/ 2)
	    (mod 2)
	    (** 3)
	    (^ 3)
	    (t 4)))

	(opcode (operator)
	  (case operator
	    (= 'setq)
	    (+ '+)
	    (- '-)
	    (* '*)
	    (/ '/)
	    (mod 'mod)
	    (** 'expt)
	    (^ 'expt)
	    (t (error "invalid operator" operator))))

	(inf-aux (ae operators operands)
	  (inf-iter (cdr ae)
	    operators
	    (cons (car ae) operands)))

	(inf-iter (ae operators operands)
	  (cond ((and (null ae) (null operators))
		 (car operands))
		((and (not (null ae))
		      (or (null operators)
			  (> (weight (car ae))
			     (weight (car operators)))))
		 (inf-aux (cdr ae)
			  (cons (car ae) operators)
			  operands))
		(t (inf-iter ae
			     (cdr operators)
			     (cons (list (opcode (car operators))
					 (cadr operands)
					 (car operands))
				   (cddr operands)))))))

  (if (atom ae)
      ae
      (inf-aux ae nil nil))))

(setf (aref *readtable* (char-int #\[))
  (cons :tmacro
	(lambda (f c &aux ex)
		(setf ex nil)
		(do () ((eq (peek-char t f) #\]))
			(setf ex (append ex (cons (read f) nil))))
		(read-char f)
		(cons (inf-to-pre ex) nil))))

(setf (aref *readtable* (char-int #\]))
  (cons :tmacro
	(lambda (f c)
		(error "misplaced right bracket"))))


;==============================================================================
;======================= *calc_operator_widget_class* =========================
;==============================================================================

;; the metaclass for nonterminal nodes corresponding to arithmetic
;; operators.
(setq *calc_operator_widget_class*
      (send Class :new
	    '(operator-name
	      child-list
	      )
	    '()
	    XM_PUSH_BUTTON_WIDGET_CLASS))

;; override XM_PUSH_BUTTON_WIDGET_CLASS instance initializer
(send *calc_operator_widget_class* :answer :ISNEW
      '(args managed_k widget_name widget_parent &rest widget_args)
      '(
	(setq child-list NIL)
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k widget_name widget_parent
	       :XMN_FOREGROUND	"Blue"
	       :XMN_BACKGROUND	"LightGrey"
	       widget_args
	       )
	(send-super :set_callback :XMN_ACTIVATE_CALLBACK '() ;this method should really be part of a metaclass on all objects in the graphcalc graph_w
		    `(
		      (send *calc_display* :set_display_value_from_graphnode ,self)
		      ))
	(do*
	 ((arg-list args (cdr arg-list))
	  child-widget
	  )
	 ((null arg-list)
	  )
	 (setq child-widget
	       (display-s-expr (car arg-list)))
	 (setq child-list (append child-list (list child-widget)))

	 (send XM_ARC_WIDGET_CLASS :new :managed
	       "" graph_w
	       :XMN_TO self
	       :XMN_FROM child-widget
	       )
	 )
	self				;return self
	))

;; add individual argument widgets (operands for operator-name)
(send *calc_operator_widget_class* :answer :add_arg '(child-widget)
      '(
	(setq child-list (cons child-widget child-list))
	(send XM_ARC_WIDGET_CLASS :new :managed
	      "" graph_w
	      :XMN_TO self
	      :XMN_FROM child-widget
	      )
	self				;return value
	))

;; retrieve the value of applying operator on it's children (recursive)
(send *calc_operator_widget_class* :answer :get_value '()
      '(
	(let ((arg-list NIL))
	  (do
	   ((children child-list (cdr children)))
	   ((null children))
	   (setq arg-list
		 (append arg-list (list (send (car children) :get_value))))
	   )
	  (apply operator-name arg-list) ;return value
	  )
	))

;; throw away :reorient message
(send *calc_operator_widget_class* :answer :reorient '()
      '(
	self
	))

;;; not needed - handled by default WIDGET_CLASS :destroy
;;;;;;recursively destroy all the children of an operator
;;;(send *calc_operator_widget_class* :answer :destroy '()
;;;      '(
;;;	(do
;;;	 ((children child-list (cdr children)))
;;;	 ((null children))
;;;	 (send (car children) :destroy)
;;;	 )
;;;	(send-super :destroy)
;;;	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface to *calc_operator_widget_class* metaclass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((operators-alist NIL))

  ;; create a subclass of *calc_operator_widget_class*. for each different operator
  ;; we can fetch the subclass via (get-operator-class <operator-symbol>).
  (defun make-operator (operator-symbol)
    (let ((operator-class		;make a subclass of *calc_operator_widget_class*
	   (send Class :new
		 '()
		 '()			;no class variables for subclass
		 *calc_operator_widget_class*)))

      ;; override widget's instance initializer
      (send operator-class :answer :ISNEW
	    '(args &rest widget_args)
	    '(
	      (setq operator-name operator-symbol)
	      (apply #'send-super :isnew ;call superclass's init to create widget
		     args :managed
		     "operator" graph_w
		     :XMN_LABEL_STRING (symbol-name operator-symbol)
		     widget_args
		     )
	      self			;return self
	      ))
    
      (setq operators-alist (cons (cons operator-symbol operator-class) operators-alist))
      ))

  (make-operator '/)
  (make-operator '*)
  (make-operator '-)
  (make-operator '+)
  (make-operator 'mod)
  (make-operator 'expt)
					; (make-operator 'setq)
  ;;
  ;; Fetch the operator class created by make-operator
  ;;
  (defun get-operator-class (operator-symbol)
    (let ((alist-elt (assoc operator-symbol operators-alist)))
      (if alist-elt
	  (cdr alist-elt)
	NIL)
      ))
  )					;end let

; the rem operator is unsuitable for direct use as mod because it insists on FIXNUMS...
(defun mod (x y)
  (setq x (truncate x))
  (setq y (truncate y))
  (rem x y)
  )

;==============================================================================
;===================== *calc_terminal_value_widget_class* =====================
;==============================================================================

;; The class of value-bearing terminal nodes
(setq *calc_terminal_value_widget_class*
      (send Class :new
	    '(value
	      )
	    '()
	    XM_PUSH_BUTTON_WIDGET_CLASS))

;; override XM_LABEL_WIDGET_CLASS instance initializer
(send *calc_terminal_value_widget_class* :answer :ISNEW
      '(value-flonum &rest widget_args)
      '(
	(setq value value-flonum)
	(apply #'send-super :isnew :managed ;call superclass's init to create widget
	       "terminal_value" graph_w
	       :XMN_LABEL_STRING	(format nil "~A" value-flonum)
	       :XMN_FOREGROUND		"White"
	       :XMN_BACKGROUND		"Black"
	       widget_args
	       )
	(send-super :set_callback :XMN_ACTIVATE_CALLBACK '() ;this method should really be part of a metaclass on all objects in the graphcalc graph_w
		    `(
		      (send *calc_display* :set_display_value_from_graphnode ,self)
		      ))
	self				;return self
	))

(send *calc_terminal_value_widget_class* :answer :get_value '()
      '(
	value				;return value
	))

;;; throw away :reorient message
(send *calc_terminal_value_widget_class* :answer :reorient '()
      '(
	self
	))

;;; message :DESTROY handled by WIDGET_CLASS
;;;(send *calc_terminal_value_widget_class* :answer :destroy '()
;;;      '(
;;;	))



;==============================================================================
;=================== *calc_terminal_variable_widget_class* ====================
;==============================================================================

;; The class of value-bearing terminal nodes designated as variables.
;; These are represented as a horizontal row/column widget containing
;; a label widget (the variable) and a single line texteditor widget
;; containing the current value of the variable
(setq *calc_terminal_variable_widget_class*
      (send Class :new
	    '(
	      variable-name		;SYMBOL
	      name-pushbutton-widget	;WIDGETOBJ
	      value-input-editor-widget	;WIDGETOBJ or NIL if not created
	      value-input-scale-widget	;WIDGETOBJ or NIL if not yet created.
	      )
	    '()
	    XM_ROW_COLUMN_WIDGET_CLASS))

;; override XM_LABEL_WIDGET_CLASS instance initializer
(send *calc_terminal_variable_widget_class* :answer :ISNEW
      '(variable-symbol &rest widget_args)
      '(
	(setq variable-name variable-symbol)
	(apply #'send-super :isnew :managed
	       "terminal-variable-rowcol" graph_w
	       :XMN_BORDER_WIDTH	2
	       :XMN_ORIENTATION		*default_graph_orientation*
	       :XMN_PACKING		:pack_tight
	       :XMN_ADJUST_LAST		t
	       :XMN_ENTRY_ALIGNMENT	:alignment_center
	       :XMN_RESIZE_HEIGHT	t
	       :XMN_RESIZE_WIDTH	t
	       widget_args
	       )
	(setq name-pushbutton-widget
	      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "terminal-variable-name" self
		    :XMN_LABEL_STRING (symbol-name variable-symbol)
		    ))
	(setq value-input-editor-widget NIL)
	(setq value-input-scale-widget 
	      (send XM_SCALE_WIDGET_CLASS :new :managed "terminal-variable-scale" self
		    :XMN_ORIENTATION		*default_graph_orientation*
		    :XMN_PROCESSING_DIRECTION (if (eq *default_graph_orientation* :horizontal)
						  :MAX_ON_RIGHT 
						:MAX_ON_TOP)
		    :XMN_SENSITIVE		t
		    :XMN_SHOW_VALUE		t
		    :XMN_MINIMUM		-100
		    :XMN_MAXIMUM		+100
		    :XMN_VALUE			0
		    ))

	;; left mouse button on the pushbutton gets value
	(send name-pushbutton-widget :set_callback :XMN_ACTIVATE_CALLBACK '() ;this method should really be part of a metaclass on all objects in the graphcalc graph_w
	      `(
		(send *calc_display* :set_display_value_from_graphnode ,self)
		))
	
	;; middle mouse button on pushbutton toggles between slider and editor 
	(send name-pushbutton-widget :set_event_handler BUTTON_PRESS_MASK
	      '(EVHANDLER_BUTTON)	;gets bound to button of event
	      `(
		(if (= EVHANDLER_BUTTON 2) ;middle button
		    (progn
		      (send ,self :set_values ;special hack to work around resizing bug (comment-out to see problem)
			    :xmn_resize_height nil
			    :xmn_resize_width nil
			    )
		      (cond
		       ((null value-input-scale-widget)
			(send value-input-editor-widget :destroy) ;NOTE: ideally, i'd unmanage/manage rather than destroy/create, but this works around Motif 1.0 resize bugs
			(setq value-input-editor-widget nil)
			(setq value-input-scale-widget 
			      (send XM_SCALE_WIDGET_CLASS :new :managed "terminal-variable-scale" ,self
				    :XMN_ORIENTATION		*default_graph_orientation*
				    :XMN_PROCESSING_DIRECTION (if (eq *default_graph_orientation* :horizontal)
								  :MAX_ON_RIGHT 
								:MAX_ON_TOP)
				    :XMN_SENSITIVE		t
				    :XMN_SHOW_VALUE		t
				    :XMN_MINIMUM		-100
				    :XMN_MAXIMUM		+100
				    :XMN_VALUE			0
				    ))
			)
		       ((null value-input-editor-widget)
			(send value-input-scale-widget :destroy) ;NOTE: ideally, i'd unmanage/manage rather than destroy/create, but this works around Motif 1.0 resize bugs
			(setq value-input-scale-widget nil)
			(setq value-input-editor-widget
			      (send XM_TEXT_WIDGET_CLASS :new :managed "terminal-variable-editor" ,self
				    :XMN_STRING			"0"
				    :XMN_EDIT_MODE		:single_line_edit
				    :XMN_AUTO_SHOW_CURSOR_POSITION t
				    :XMN_EDITABLE		t
				    :XMN_FOREGROUND		"Black"
				    :XMN_BACKGROUND		"LightGrey"
				    ))
			)
		       )
		      (send ,self :set_values ;special hack to work around resizing bug (comment-out to see problem)
			    :xmn_resize_height t
			    :xmn_resize_width t
			    )
		      ))
		self			;return self
		))
	))

(send *calc_terminal_variable_widget_class* :answer :get_value '()
      '(
	(cond
	 (value-input-editor-widget
	  (read (make-string-input-stream (send value-input-editor-widget :get_string))) ;return value
	  )
	 (value-input-scale-widget
	  (send value-input-scale-widget :get_value) ;return value
	  )
	 )))

(send *calc_terminal_variable_widget_class* :answer :reorient '()
      '(
	(send-super :set_values		;special hack to work around resizing bug (comment-out to see problem)
	      :xmn_resize_height nil
	      :xmn_resize_width nil
	      )
	(send-super :set_values		;special hack to work around resizing bug (comment-out to see problem)
	      :XMN_ORIENTATION	*default_graph_orientation*
	      )
	(if value-input-scale-widget	
	    (let ((value (send value-input-scale-widget :get_value)))
	      (send value-input-scale-widget :destroy) ;NOTE: ideally, i'd unmanage/manage rather than destroy/create, but this works around Motif 1.0 resize bugs
	      (setq value-input-scale-widget 
		    (send XM_SCALE_WIDGET_CLASS :new :managed "terminal-variable-scale" self
			  :XMN_ORIENTATION	    *default_graph_orientation*
			  :XMN_PROCESSING_DIRECTION (if (eq *default_graph_orientation* :horizontal)
							:MAX_ON_RIGHT 
						      :MAX_ON_TOP)
			  :XMN_SENSITIVE	t
			  :XMN_SHOW_VALUE	t
			  :XMN_MINIMUM		-100
			  :XMN_MAXIMUM		+100
			  :XMN_VALUE		value
			  ))
	      ))
	(if value-input-editor-widget
	    (progn (send value-input-editor-widget :unmanage) (send value-input-editor-widget :manage)))

	(send-super :set_values
	      :xmn_resize_height t
	      :xmn_resize_width t
	      )

	self				;return value
	))

(send *calc_terminal_variable_widget_class* :answer :destroy '()
      '(
	(send name-pushbutton-widget :destroy)
	(if value-input-editor-widget
	    (send value-input-editor-widget :destroy))
	(if value-input-scale-widget
	    (send value-input-scale-widget :destroy))
	(send-super :destroy)
	))

;==============================================================================
;===================== *calc_terminal_sexp_widget_class* ======================
;==============================================================================

;; The class of value-bearing terminal nodes designated as lisp s-exprs.
;; These are represented as a label widget containing the sexpr.
(setq *calc_terminal_sexp_widget_class*
      (send Class :new
	    '(s-expression
	      )
	    '()
	    XM_PUSH_BUTTON_WIDGET_CLASS))

;; override XM_PUSH_BUTTON_WIDGET_CLASS instance initializer
(send *calc_terminal_sexp_widget_class* :answer :isnew
      '(sexpr &rest widget_args)
      '(
	(setq s-expression sexpr)
	(apply #'send-super :isnew :managed
	       "terminal_sexp" graph_w
	       :XMN_LABEL_STRING	(format nil "~A" sexpr)
	       :XMN_FOREGROUND		"Black"
	       :XMN_BACKGROUND		"White"
	       widget_args
	       )
	(send-super :set_callback :XMN_ACTIVATE_CALLBACK '() ;this method should really be part of a metaclass on all objects in the graphcalc graph_w
		    `(
		      (send *calc_display* :set_display_value_from_graphnode ,self)
		      ))
	self				;return self
	))

(send *calc_terminal_sexp_widget_class* :answer :get_value '()
      '(
	(eval s-expression)		;return value
	))

;; throw away :reorient message
(send *calc_terminal_sexp_widget_class* :answer :reorient '()
      '(
	self
	))

;;; message :DESTROY handled by WIDGET_CLASS
;;;(send *calc_terminal_sexp_widget_class* :answer :destroy '()
;;;      '(
;;;	))


;==============================================================================
;=========================== *calc_display_class* =============================
;==============================================================================

;; make a subclass of XM_TEXT_WIDGET_CLASS
(setq *calc_display_class*
      (send Class :new
	    '(
	      accumulator-value-widget
	      display-value-widget
	      begin-entry-p
	      prev-operator-symbol
	      modify-verify-callback-enabled
	      )
	    '()				;no class variables for subclass
	    XM_TEXT_WIDGET_CLASS)) 

;; override XM_TEXT_WIDGET_CLASS's instance initializer
(send *calc_display_class* :answer :ISNEW
      '(managed_k widget_name widget_parent &rest args)
      '(
	(setq accumulator-value-widget nil)
	(setq display-value-widget nil)
	(setq begin-entry-p nil)
	(setq prev-operator-symbol nil)
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k :scrolled widget_name widget_parent
	       :XMN_STRING		""
	       :XMN_EDIT_MODE		:single_line_edit
	       :XMN_AUTO_SHOW_CURSOR_POSITION t
	       :XMN_CURSOR_POSITION	0
	       :XMN_EDITABLE		t
	       :XMN_FOREGROUND		"Black"
	       :XMN_BACKGROUND		"LightGrey"
	       args
	       )

;;; 	;; set the colors of the scrollbar -- note Motif's lamo use of hidden
;;; 	;; scroller parent when dealing with a scrolled edit widget (grrr).
;;; 	(send
;;; 	 (car (send (send-super :parent) :get_values :XMN_HORIZONTAL_SCROLL_BAR nil))
;;; 	 :set_values			;the scrolled window
;;; 	 :XMN_HEIGHT     12
;;; 	 :XMN_FOREGROUND "lightgrey"
;;; 	 :XMN_BACKGROUND "dimgrey"
;;; 	 )

	;;note XmText BUG -- causes strange result if pasting into display while this callback is enabled.
	(send-super :set_callback :XMN_MODIFY_VERIFY_CALLBACK '()
		    `(
		      (if modify-verify-callback-enabled
			  (cond
			   (begin-entry-p
			    (setq display-value-widget nil)
			    (setq modify-verify-callback-enabled nil) ; :set_string will cause modify verify callback, so disable infinite recursion
			    (send ,self :set_string "")	
			    ;;text entry that caused callback will be input via text widget after clearing...
			    ;;NOTE that XmText is bug-laden and doesn't work right if input is from
			    ;;pasting, infact, it seems to cause a garbled display and possible memory
			    ;;corruption. I'd normally do all this myself, except that setting CALLBACK_DOIT
			    ;;to NIL and doing (send ,self :set_string CALLBACK_TEXT) causes a beep. XmText is a piece of shit.
			    ;;WORKAROUND is to enter a single char (' ') and then paste....
			    (setq modify-verify-callback-enabled t) ; resume handling modify verify callback
			    (setq begin-entry-p nil)
			    ))
			))
		    )

	;; this callback fires when <return> is entered into the text editor widget (single-line version only)
	(send-super :set_callback :XMN_ACTIVATE_CALLBACK '()
	      `(
		(send ,self :set_display_value_from_graphnode 
		      (display-equation	;returns a graphnode widget containing expression in the text display
		       (read
			(make-string-input-stream
			 (concatenate 'string "( " (send-super :get_string) " )")))))
		))
	self				;return self
	))

;;
;; this gets called when a graph node gets clicked, it displays the value
;; of the clicked graph node and sets that as a possible operand for other
;; operators
(send *calc_display_class* :answer :set_display_value_from_graphnode '(value-widget)
      '(
	(setq display-value-widget value-widget)
	(setq modify-verify-callback-enabled nil)
	(send-super :set_string (format nil "~A" (send value-widget :get_value)))
	(setq modify-verify-callback-enabled t)
	(setq begin-entry-p t)
	)
      )

(send *calc_display_class* :answer :exec_binary_operator '(operator-symbol)
      '(
	;; if display-value-widget is non-null, then a result has been set by clicking an operator node in the graph widget;
	;; on first entering an expression in the display, the :XMN_MODIFY_VERIFY_CALLBACK fires which clears the disp and
	;; sets display-value-widget to NIL. when display-value-widget is NIL we convert the expression in the display into a sexp and graph it.
	(if (null display-value-widget)
	    (setq display-value-widget
		  (display-equation (read (make-string-input-stream (concatenate 'string "( " (send-super :get_string) " )"))))))
	;; display-value-widget is now guaranteed to hold widget assoc'd with displayed value


	(cond
	 ;; if there is a prev operator, then we want to create a new node corresponding to prev-op-symbol
	 ;; whose args are the current value of the accumulator and the current display.
	 ;; if the accumilator is NIL, the we don't pass that arg to widget.
	 (prev-operator-symbol
	  (let ((operator-representor-class (get-operator-class prev-operator-symbol))
		w)
	    (cond
	     (operator-representor-class ;get-operator-class returns NIL if the operator was not defined with make-operator
	      (setq w (send operator-representor-class :new NIL))
	      (send w :add_arg display-value-widget)
	      (setq display-value-widget w)
	      (if accumulator-value-widget
		  (send display-value-widget :add_arg accumulator-value-widget))
	      (setq modify-verify-callback-enabled nil)
	      (send-super :set_string (format nil "~A" (send display-value-widget :get_value)))	;display the result
	      (setq modify-verify-callback-enabled t)
	      (setq accumulator-value-widget display-value-widget)
	      )))
	  )
	 ;; else there was no prev operator, meaning last operation was an ==
	 ;; just display the value, and save it in the accumulator for next time
	 (t
	  (setq modify-verify-callback-enabled nil)
	  (send-super :set_string (format nil "~A" (send display-value-widget :get_value)))
	  (setq modify-verify-callback-enabled t)
	  (setq accumulator-value-widget display-value-widget)    
	  ))

	(setq begin-entry-p t)
	(setq prev-operator-symbol operator-symbol)
	))

(send *calc_display_class* :answer :insert_string '(str)
      '(
	(let ((pos (send-super :get_insertion_position)))
	  (send-super :replace pos pos str)
	  (send-super :set_insertion_position (+ pos (length str)))
	  )
	))

(send *calc_display_class* :answer :change_sign '()
      '(
	(setq modify-verify-callback-enabled nil)
	(send-super :set_string (format nil "~A" (- (read (make-string-input-stream (send-super :get_string))))))
	(setq modify-verify-callback-enabled t)
	))

(send *calc_display_class* :answer :backspace '()
      '(
	(let ((pos (send-super :GET_INSERTION_POSITION)))
	  (cond
	   ((= pos 0)
	    (X_BELL)			;SIGNAL ERROR -- BEEP
	    )
	   (t
	    (setq modify-verify-callback-enabled nil)
	    (send-super :replace (1- pos) pos "")
	    (setq modify-verify-callback-enabled t)
	    ))
	  )))

(send *calc_display_class* :answer :delete '()
      '(
	(let ((pos (send-super :GET_INSERTION_POSITION)))
	  (cond
	   ((= pos (length (send-super :get_string)))
	    (X_BELL)			;SIGNAL ERROR -- BEEP
	    )
	   (t
	    (setq modify-verify-callback-enabled nil)
	    (send-super :replace pos (1+ pos) "")
	    (setq modify-verify-callback-enabled t)
	    ))
	  )))

(send *calc_display_class* :answer :clear '()
      '(
	(setq display-value-widget nil)
	(setq modify-verify-callback-enabled nil)
	(send-super :set_string "")
	(setq modify-verify-callback-enabled t)
	))

(send *calc_display_class* :answer :forward-char '()
      '(
	(let ((pos (send-super :GET_INSERTION_POSITION)))
	  (if (< pos (length (send-super :get_string)))
	      (send-super :set_insertion_position (1+ pos))
	    (X_BELL)			;SIGNAL ERROR -- BEEP
	    ))))

(send *calc_display_class* :answer :backward-char '()
      '(
	(let ((pos (send-super :GET_INSERTION_POSITION)))
	  (if (> pos 0)
	      (send-super :set_insertion_position (1- pos))
	    (X_BELL)			;SIGNAL ERROR -- BEEP
	    ))))


;==============================================================================
;=================== *calc_self_insert_button_widget_class* ===================
;==============================================================================
(setq *calc_self_insert_button_widget_class*
      (send Class :new
	    '(name-string
	      )
	    '()
	    XM_PUSH_BUTTON_WIDGET_CLASS))

;; override XM_PUSH_BUTTON_WIDGET_CLASS instance initializer
(send *calc_self_insert_button_widget_class* :answer :ISNEW
      '(managed_k widget_name widget_parent &rest args)
      '(
	(setq name-string widget_name)
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_k widget_name widget_parent
	       :XMN_LABEL_STRING	widget_name
	       :XMN_FOREGROUND		"Green"
	       :XMN_BACKGROUND		"IndianRed"
	       args
	       )
	(send-super :set_callback :xmn_activate_callback '()
	      `(
		(send *calc_display* :insert_string ,widget_name)
		))

	self				;return self
	))

(send *calc_self_insert_button_widget_class* :answer :get_name_string '()
      '(name-string)			;return value
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "graphcalc"
	    :XMN_TITLE 			"WINTERP: GraphCalc"
	    :XMN_ICON_NAME 		"W:graphcalc"
;;;	    :XMN_KEYBOARD_FOCUS_POLICY	:EXPLICIT ;a hack to kludge up a forced focus on the display widget so cursor can be seen...
	    ))

(setq vpaned_w
      (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	    "vpane" top_w
	    ))

(setq graph_w
      (send XM_GRAPH_WIDGET_CLASS :new :managed :scrolled
	    "dag" vpaned_w
;;;	    :XMN_HEIGHT			400 ;need to give a default height to start up -- can resize window to change
	    :XMN_ARC_DRAW_MODE		:position_relative
	    :XMN_ORIENTATION		*default_graph_orientation*
	    :XMN_CHILD_SPACING		15
	    :XMN_SIBLING_SPACING	10
	    :XMN_EDITABLE		nil ;setting to T means the buttons won't be pushable
	    :XMN_AUTO_LAYOUT_MODE	:always
	    ))

;;; set the size of the graph window pane -- note Motif's lamo use of hidden
;;; scroller->drawingarea->graph hierarchy when dealing with a scrolled widget (grrr).
(send (send (send graph_w :parent) :parent) :set_values
      :XMN_HEIGHT			500
      :XMN_SCROLL_BAR_DISPLAY_POLICY	:static
      )

;;;(send graph_w :set_values
;;;      :xmn_child_spacing	15
;;;      :xmn_sibling_spacing	10
;;;      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;; controlpanel for graph_w ;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq graph_controlpanel_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "controlpanel" vpaned_w
	    :XMN_ORIENTATION            :HORIZONTAL
	    :XMN_PACKING                :PACK_TIGHT
	    :XMN_ADJUST_LAST            nil
	    :XMN_ENTRY_ALIGNMENT	:alignment_center
	    ))

(setq graph_editable_tbw
      (send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed
	    "Editable" graph_controlpanel_w
	    :XMN_SET nil
	    ))
(send graph_editable_tbw :set_callback :xmn_value_changed_callback '(CALLBACK_SET)
      '(
	(send graph_w :set_values :xmn_editable CALLBACK_SET)
	(send graph_destroy_selected_pbw :set_values :xmn_sensitive CALLBACK_SET)
	))

(setq graph_destroy_selected_pbw
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "Clear Selected" graph_controlpanel_w
	    :xmn_sensitive nil		;note the button is inactive unless :XMN_EDITABLE is true
	    ))
(send graph_destroy_selected_pbw :set_callback :xmn_activate_callback '()
      '(
	(let* ((array-sel-widgets   (send graph_w :get_selected_nodes))
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

(setq graph_clear_pbw
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "Clear All" graph_controlpanel_w
	    ))
(send graph_clear_pbw :set_callback :xmn_activate_callback '()
      '(
	(send graph_w :destroy_all_nodes)
	))

(setq graph_relayout_pbw
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "Re-Layout" graph_controlpanel_w
	    ))
(send graph_relayout_pbw :set_callback :xmn_activate_callback '()
      '(
	(send graph_w :layout)
	))

(setq graph_reorient_pbw
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
	    "Flip Layout Direction" graph_controlpanel_w
	    ))
(send graph_reorient_pbw :set_callback :xmn_activate_callback '()
      '(
	(send graph_w :set_values :xmn_reorient t)

	(cond
	 ((eq *default_graph_orientation* :horizontal)
	  (setq *default_graph_orientation* :vertical)
	  )
	 ((eq *default_graph_orientation* :vertical)
	  (setq *default_graph_orientation* :horizontal)
	  )
	 )

	(let* ((array-widgets   (send graph_w :get_nodes))
	       (num-widgets     (length array-widgets)))
	  (do 
	   ((i 0 (1+ i)))
	   ((eq i num-widgets))
	   (send (aref array-widgets i) :reorient)
	   ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; calculator display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *calc_display*
      (send *calc_display_class* :new :managed
	    "display" vpaned_w
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the keyboard ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; just for kicks, I'm making a graph widget manage a bunch of buttons that
;; could easily be done via XmForm -- I want to play w/ using the graph widget
;; as a "direct manipulation" manager for widgets....
;;
(setq calc_keyboard_w
      (send XM_GRAPH_WIDGET_CLASS :new :managed
	    "calc-keyboard" vpaned_w
	    :xmn_arc_draw_mode		:position_fixed
	    :xmn_editable		nil ;setting to T means the buttons won't be pushable
	    :xmn_auto_layout_mode	:never ;we're doing our own layout here...
	    :xmn_orientation		:horizontal
	    ))

(let ((keyboard-interkey-spacing 5)
      (keyboard-x-width 0)
      (keyboard-y-height 0)
      )

  (let ((y-offset keyboard-interkey-spacing)
	(x-offset keyboard-interkey-spacing)
	last-w)
    (setq last-w
	  (send *calc_self_insert_button_widget_class* :new :managed "1" calc_keyboard_w
		:XMN_X x-offset
		:XMN_Y y-offset))
    (setq x-offset
	  (+ x-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_width nil))))
    (do*
     ((keys-list '("2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "+")
		 (cdr keys-list)))
     ((null keys-list))
     (setq last-w
	   (send *calc_self_insert_button_widget_class* :new :managed (car keys-list) calc_keyboard_w
		 :XMN_X x-offset
		 :XMN_Y y-offset))
     (setq x-offset
	   (+ x-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_width nil))))
     )

    (setq keyboard-x-width (max keyboard-x-width x-offset))
    (setq x-offset (+ keyboard-interkey-spacing (truncate (car (send last-w :get_values :xmn_height nil)) 2)))
    (setq y-offset (+ y-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_height nil))))

    (do*
     ((keys-list '("Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "[" "]" "(" ")")
		 (cdr keys-list)))
     ((null keys-list))
     (setq last-w
	   (send *calc_self_insert_button_widget_class* :new :managed (car keys-list) calc_keyboard_w
		 :XMN_X x-offset
		 :XMN_Y y-offset))
     (setq x-offset
	   (+ x-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_width nil))))
     )

    (setq keyboard-x-width (max keyboard-x-width x-offset))
    (setq x-offset (+ keyboard-interkey-spacing (car (send last-w :get_values :xmn_height nil))))
    (setq y-offset (+ y-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_height nil))))

    (do*
     ((keys-list '("A" "S" "D" "F" "G" "H" "J" "K" "L" ";" "'" "RETURN")
		 (cdr keys-list)))
     ((null keys-list))
     (setq last-w
	   (send *calc_self_insert_button_widget_class* :new :managed (car keys-list) calc_keyboard_w
		 :XMN_X x-offset
		 :XMN_Y y-offset))
     (setq x-offset
	   (+ x-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_width nil))))
     )

    (setq keyboard-x-width (max keyboard-x-width x-offset))
    (setq x-offset (+ keyboard-interkey-spacing (* 2 (car (send last-w :get_values :xmn_height nil)))))
    (setq y-offset (+ y-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_height nil))))

    (do*
     ((keys-list '("Z" "X" "C" "V" "B" "N" "M" "," "." "/")
		 (cdr keys-list)))
     ((null keys-list))
     (setq last-w
	   (send *calc_self_insert_button_widget_class* :new :managed (car keys-list) calc_keyboard_w
		 :XMN_X x-offset
		 :XMN_Y y-offset))
     (setq x-offset
	   (+ x-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_width nil))))
     )

    (setq keyboard-x-width (max keyboard-x-width x-offset))
    (setq y-offset (+ y-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_height nil))))
    
    (setq last-w
	  (send *calc_self_insert_button_widget_class* :new :managed " " calc_keyboard_w
		:XMN_X (- x-offset 200 keyboard-interkey-spacing)
		:XMN_Y y-offset
		:XMN_WIDTH 200
		))

    (setq keyboard-y-height (+ y-offset keyboard-interkey-spacing (car (send last-w :get_values :xmn_height nil))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq edit_pad_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed "edit-panel" calc_keyboard_w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_tight
	      :XMN_ADJUST_LAST 		nil
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_X			keyboard-x-width
	      :XMN_Y			0
	      ))

  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "edit-button" edit_pad_w
	      :XMN_LABEL_STRING "Backspace"
	      :XMN_FOREGROUND "Yellow"
	      :XMN_BACKGROUND "DimGrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :backspace)
	  ))

  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "edit-button" edit_pad_w
	      :XMN_LABEL_STRING "Delete"
	      :XMN_FOREGROUND "Yellow"
	      :XMN_BACKGROUND "DimGrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :delete)
	  ))

  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "edit-button" edit_pad_w
	      :XMN_LABEL_STRING "Clear"
	      :XMN_FOREGROUND "Yellow"
	      :XMN_BACKGROUND "DimGrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :clear)
	  ))

  (send (send XM_ARROW_BUTTON_WIDGET_CLASS :new :managed "edit-button" edit_pad_w
	      :XMN_ARROW_DIRECTION :arrow_right
	      :XMN_FOREGROUND "Yellow"
	      :XMN_BACKGROUND "DimGrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :forward-char)
	  ))

  (send (send XM_ARROW_BUTTON_WIDGET_CLASS :new :managed "edit-button" edit_pad_w
	      :XMN_ARROW_DIRECTION :arrow_left
	      :XMN_FOREGROUND "Yellow"
	      :XMN_BACKGROUND "DimGrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :backward-char)
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numberpad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq numpad_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed "numbers" calc_keyboard_w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_column
	      :XMN_NUM_COLUMNS		4
	      :XMN_ADJUST_LAST 		nil
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_X			keyboard-x-width
	      :XMN_Y			0
	      ))

  (send *calc_self_insert_button_widget_class* :new :managed "7" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "4" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "1" numpad_w)
  (send (send *calc_self_insert_button_widget_class* :new :managed "+/-" numpad_w)
	:set_callback :xmn_activate_callback '() ;override normal self-insert callback
	'(
	  (send *calc_display* :change_sign)
	  ))
  (send *calc_self_insert_button_widget_class* :new :managed "8" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "5" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "2" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "0" numpad_w)

  (send *calc_self_insert_button_widget_class* :new :managed "9" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "6" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "3" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "." numpad_w)

  (send *calc_self_insert_button_widget_class* :new :managed "(" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed ")" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "[" numpad_w)
  (send *calc_self_insert_button_widget_class* :new :managed "]" numpad_w)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the function buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq funcpad_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed "functions" calc_keyboard_w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_column
	      :XMN_NUM_COLUMNS		2
	      :XMN_ADJUST_LAST 		nil
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      :XMN_X			keyboard-x-width
	      :XMN_Y			0
	      ))

  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " * "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator '*)
	  ))
  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " / "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator '/)
	  ))
  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " - "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator '-)
	  ))
  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " + "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator '+)
	  ))
  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " ^ "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator 'expt)
	  ))
  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " MOD "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator 'mod)
	  ))
  (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	:XMN_LABEL_STRING " "
	:XMN_FOREGROUND "blue"
	:XMN_BACKGROUND "lightgrey"
	:XMN_MAPPED_WHEN_MANAGED nil	;NOTE: button unmapped - use it to take up space...
	)
  (send (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed "func-button" funcpad_w
	      :XMN_LABEL_STRING " == "
	      :XMN_FOREGROUND "blue"
	      :XMN_BACKGROUND "lightgrey"
	      )
	:add_callback :xmn_activate_callback '()
	'(
	  (send *calc_display* :exec_binary_operator nil)
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require "graphcalc1")		;adds "Options" button

  (send top_w :realize)			;create all the windows -- note that the code below doesn't get
					;the correct width/height values in laying out keyboard unless
					;the toplevel widget is realized...
					;We wouldnt need this kludge if the graph widget wasn't beign
					;used to layout the keyboard....

  (let (height)
    (send graph_controlpanel_w :get_values :xmn_height 'height)
    (send graph_controlpanel_w :set_values
	  :XMN_PANE_MAXIMUM height
	  :XMN_PANE_MINIMUM height
	  )
    )
  (let (height)
    (send (send *calc_display* :parent) :get_values :xmn_height 'height)
    (send (send *calc_display* :parent) :set_values
	  :XMN_PANE_MAXIMUM height
	  :XMN_PANE_MINIMUM height
	  )
    )
  (send calc_keyboard_w :set_values
	:XMN_PANE_MAXIMUM keyboard-y-height
	:XMN_PANE_MINIMUM keyboard-y-height
	)

  (setq keyboard-x-width (+ keyboard-x-width (car (send edit_pad_w :get_values :xmn_width nil))))
  (send numpad_w :set_values :xmn_x keyboard-x-width)
  (setq keyboard-x-width (+ keyboard-x-width (car (send numpad_w :get_values :xmn_width nil))))
  (send funcpad_w :set_values :xmn_x keyboard-x-width)
  (setq keyboard-x-width (+ keyboard-x-width (car (send funcpad_w :get_values :xmn_width nil))))
  
  ;; kludge to make toplevel window come up right -- this wouldn't happen if
  ;; the graph widget wasn't being used to layout the keyboard...
  (send top_w :set_values
 	:xmn_width (+ keyboard-x-width keyboard-interkey-spacing)
 	:xmn_height (+ 
 		     (car (send graph_w :get_values :xmn_height nil))
 		     (car (send graph_controlpanel_w :get_values :xmn_height nil))
 		     (car (send *calc_display* :get_values :xmn_height nil))
 		     keyboard-y-height)
 	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF PROGRAM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
