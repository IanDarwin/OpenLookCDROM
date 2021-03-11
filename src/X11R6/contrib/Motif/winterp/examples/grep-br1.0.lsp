;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         grep-br1.0.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/grep-br1.0.lsp,v 2.4 1994/06/06 14:43:13 npm Exp $
; Description:  Old version of grep-br.lsp, for Motif 1.0. Motif >= 1.1 users
;		should use grep-br.lsp. This file is loaded from grep-br.lsp
; Author:       Niels Mayer
; Created:      Mon Nov 20 18:13:23 1989
; Modified:     Sun Jun  5 18:48:47 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, *MOTIF-1.0-P*
(require "lib-widgets/file-view")	;define File_Viewer_Widget_Class

(if *MOTIF-1.1-OR-LATER-P*
    (error "grep-br1.0.lsp is to be used with Motif 1.0 only -- use grep-br.lsp for later versions of Motif.")
  )

;;
;; Make a subclass of XM_LIST_WIDGET_CLASS which holds an additional
;; instance variable 'items'. 'items' is an array of arbitrary objects
;; (BROWSER_OBJECT) to be displayed in a browser made from the list widget.
;;
;; BROWSER-OBJECT can be any arbitrary xlisp object that responds to
;; the message :display_string.
;;
;; Message :display_string must return a string which is used as the
;; textual representation of the object in the browser display.
;;
(setq List_Browser_Widget_Class
      (send Class :new
	    '(items)			;new instance vars
	    '()				;no class vars
	    XM_LIST_WIDGET_CLASS))	;superclass

;;
;; We add a method to set the items browsed by the list browser
;; and set the 'items' instance variable.
;;
;; (send <List_Browser_Widget_Class_inst> :set_browser_items <items_list>)
;; <items_list> is a list of BROWSER_OBJECTs as described above.
;;
(send List_Browser_Widget_Class :answer :SET_BROWSER_ITEMS '(items_list)
      '(
	(let* (
	       (items_end_idx (length items_list))
	       (display_items (make-array items_end_idx)))

	  ;; initialize the 'items' instance variable so that it
	  ;; holds all the BROWSER_OBJECTs passed in <items_list>
	  (setq items (make-array items_end_idx)) ;create the array
	  (do (				;copy elts from list to array
	       (i    0          (1+ i))
	       (elts items_list (cdr elts)))
	      ;; loop till no more elts
	      ((null elts))
	      ;; loop body
	      (setf (aref items i) (car elts))
	      (setf (aref display_items i) (send (car elts) :display_string))
	      )
	  ;; initialize the widget, passing in the browser items.
	  (send self :set_values
		:xmn_items display_items
		:xmn_item_count items_end_idx
		)
	  )
	)
      )


;;
;; Given a List Widget position, this returns the object associated
;; with that position. Note that the first item is at position 1.
;;
(send List_Browser_Widget_Class :answer :GET_ITEM_AT_POSITION '(position)
      '(
	(aref items (1- position))
	))

;;
;; override methods on XM_LIST_WIDGET_CLASS so that they work properly
;; with the list browser. Note that all other list methods work fine
;; on the list browser
;;
(send List_Browser_Widget_Class :answer :ADD_ITEM '(item position)
      '(
	(setq items (array-insert-pos items (1- position) item))
	(send-super :add_item (send item :display_string) position)
	)
      )

(send List_Browser_Widget_Class :answer :ADD_ITEM_UNSELECTED '(item position)
      '(
	(setq items (array-insert-pos items (1- position) item))
	(send-super :add_item_unselected (send item :display_string) position)
	)
      )

(send List_Browser_Widget_Class :answer :DELETE_ITEM '(item)
      '(
	;; this is too lame to implement... requires that we compare
	;; item with the result of :display_string done on every element
	;; of ivar 'items'
	(error "Message :DELETE_ITEM not supported in List_Browser_Widget_Class")
	)
      )

(send List_Browser_Widget_Class :answer :DELETE_POS '(position)
      '(
	(setq items (array-delete-pos items (1- position)))
	(send-super :delete_pos position)
	)
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grep_Item_Class -- a BROWSER_OBJECT for display within
;; the List_Browser_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Each BROWSER_OBJECT holds the information summarizing one line of 'grep'
;; output -- the line and filename matching the regexp suplied to 'grep'.
;; The information is split up into individual fields because we may want
;; to be able to sort on one field, or search for matches on one field.
;;
(setq Grep_Item_Class
      (send Class :new
	    '(file-name line-num match-line)
	    ))

;; this method will read a single line of grep output.
;; and sets the instance variables in the 
;; BROWSER_OBJECT to the individual fields of the grep output
(send Grep_Item_Class :answer :READ-GREP-INFO '(pipe)
      '(
	(if (and
	     (setq file-name (fscanf-string pipe "%[^:]:"))
	     (setq line-num  (fscanf-fixnum pipe "%d:"))
	     (setq match-line (fscanf-string pipe "%[^\n]\n"))
	     )
	    self			;return self if succesful
	  NIL				;return NIL if hit EOF
	  )
	)
      )

(send Grep_Item_Class :answer :DISPLAY_STRING '()
      '(
	(format nil "~A: ~A"
		file-name match-line)
	))


(send Grep_Item_Class :answer :FILE-NAME '()
      '(
	file-name
	))

(send Grep_Item_Class :answer :LINE-NUM '()
      '(
	line-num
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This returns a list of Grep_Item_Class instances corresponding
;; to the items matching the search pattern and file list given
;; in argument <grep-arg-string>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun grep (grep-arg-string)
  (do* 
   (;; loop variables, initializers, and increments.
    (fp (popen (concatenate 'string "grep -n " grep-arg-string " /dev/null")
	       :direction :input))
    (line (send (send Grep_Item_Class :new) :read-grep-info fp)
	  (send (send Grep_Item_Class :new) :read-grep-info fp))
    (result '())			;init to an empty list
    )
   ;; loop test and return
   ((null line)				; :read-grep-info returns NIL on EOF
    (pclose fp)				;close the pipe opened above
    (reverse result)			;return list of grep objects.
    )
   ;; loop body
   (setq result (cons line result))	;prepend grep-obj to list
   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Main program -- note that this doesn't use any global variables, so
;; you can have many grep browsers up all at once without having them
;; interact.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun grep-browser()

  (let (
	top_w paned_w controlpanel_w doit_button_w search_label_w
	search_editor_w files_label_w files_editor_w list_w filename_label_w
	viewtext_w
	)

    (setq top_w
	  (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
		:XMN_TITLE	"WINTERP: Grep Browser (Motif 1.0 Special)"
		:XMN_ICON_NAME	"W:grep-br1.0"
		))
    (setq paned_w
	  (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed top_w
		))
    (setq controlpanel_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed paned_w
		:XMN_ADJUST_LAST t
		:XMN_ORIENTATION :HORIZONTAL
		:XMN_PACKING :PACK_TIGHT
		:XMN_NUM_COLUMNS 1
		))
    (setq doit_button_w
	  (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed controlpanel_w
		:XMN_LABEL_STRING "DO SEARCH"
		))
    (setq search_label_w
	  (send XM_LABEL_WIDGET_CLASS :new :managed controlpanel_w
		:XMN_LABEL_STRING "Search for string:"
		))
    (setq search_editor_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed controlpanel_w
		:XMN_EDIT_MODE :SINGLE_LINE_EDIT
		))
    (setq files_label_w
	  (send XM_LABEL_WIDGET_CLASS :new :managed controlpanel_w
		:XMN_LABEL_STRING "From Files:"
		))      
    (setq files_editor_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed controlpanel_w
		:XMN_EDIT_MODE :SINGLE_LINE_EDIT
		))
    (setq list_w
	  (send List_Browser_Widget_Class :new :managed :scrolled "browser" paned_w
		:xmn_visible_item_count 10
		))
    (setq filename_label_w
	  (send XM_LABEL_WIDGET_CLASS :new :managed "label" paned_w
		:xmn_label_string "None"
		))
    (setq viewtext_w
	  (send Text_Viewer_Widget_Class :new :managed "view" paned_w
		:XMN_HEIGHT 200
		))

    (send top_w :realize)

    ;;
    ;; set constraint resources on controlpanel so that paned window
    ;; doesn't give it resize sashes.
    ;;
    (let (height)
      (send controlpanel_w :get_values :xmn_height 'height)
      ;; In the code below, the kludgery
      ;; "(if *MOTIF-1.0-P* ...)"
      ;; is there to work around a name change between Motif 1.0 and 1.1 (and later):
      ;; :XMN_MAXIMUM --> :XMN_PANE_MAXIMUM and :XMN_MINIMUM -->:XMN_PANE_MINIMUM
      (send controlpanel_w :set_values
	    (if *MOTIF-1.0-P* :XMN_MAXIMUM :XMN_PANE_MAXIMUM)
	    height
	    (if *MOTIF-1.0-P* :XMN_MINIMUM :XMN_PANE_MINIMUM)
	    height
	    ))

    ;;
    ;; set constraint resources on label widget so that paned window
    ;; doesn't give it resize sashes.
    ;;
    (let (height)
      (send filename_label_w :get_values :xmn_height 'height)
      ;; In the code below, the kludgery
      ;; "(if *MOTIF-1.0-P* ...)"
      ;; is there to work around a name change between Motif 1.0 and 1.1:
      ;; :XMN_MAXIMUM --> :XMN_PANE_MAXIMUM and :XMN_MINIMUM -->:XMN_PANE_MINIMUM
      (send filename_label_w :set_values
	    (if *MOTIF-1.0-P* :XMN_MAXIMUM :XMN_PANE_MAXIMUM)
	    height
	    (if *MOTIF-1.0-P* :XMN_MINIMUM :XMN_PANE_MINIMUM)
	    height
	    ))

    ;;
    ;; The doit_button initiates a grep search.
    ;;
    (send doit_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (send list_w :set_browser_items
		  (grep (concatenate 'string
			 "'"		;quotify string to protect from shell
			 (send ,search_editor_w :get_string) ;string to search for
			 "' "
			 (send ,files_editor_w :get_string)) ;wildcarded files
			))
	    ))

    ;;
    ;; set up a callback on the list widget initialized above such that
    ;; a double click on the browser-item will browse the object.
    ;;
    (send list_w :add_callback :xmn_default_action_callback
	  '(callback_item_position)
	  `(
	    (let* 
		((browsed-object
		  (send ,list_w :get_item_at_position callback_item_position))
		 (filename (send browsed-object :file-name))
		 (linenum (send browsed-object :line-num))
		 )
	      (send ,filename_label_w :set_values :xmn_label_string filename)
	      (send ,filename_label_w :update_display)	;incase reading file takes long time
	      (send ,viewtext_w :find_file filename linenum)
	      ))
	  )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bring up an instance of the grep browser.
(grep-browser)
