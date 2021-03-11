; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         grep-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/grep-br.lsp,v 2.12 1994/06/06 14:43:14 npm Exp $
; Description:  A file search browser using the Unix "grep" command to perform
;		search. Enter the desired search string in "Search Regexp" and
;		enter the set of files to search in "Wildcarded Files";
;		then enter <return> in one of those widgets, "Do Search".
;		Then use up and down arrow keys, or the following mouse/key
;		bindings to browse list of search items
;		   Mouse bindings on browsers:
;			* single left click	-- select item for use by
;						   $EDITOR button or other op...
;			* double left click	-- select item and browse it in
;						   associated viewer widget.
;			* single middle click	-- select item and browse it in
;						   associated viewer widget.
;			* single right click	-- select item and display
;						   corresponding text in $EDITOR.
;		   Key bindings on browsers::
;			* E			-- view selected item in user's
;						   editor ($EDITOR).
;			* ^E			-- select next item and view in
;						   user's editor ($EDITOR).
;			* ^N, ^<DownArrow>	-- select next item.
;			* ^P, ^<UpArrow>	-- select prev item.
;			* N , <DownArrow>	-- browse next item in viewer.
;			* P , <UpArrow>		-- browse prev item in viewer.
;		See docs for variables *SYSTEM-EDITOR*, *GREP-BR-EXECUTABLE* below.
;		To start up this grep browser "standalone", do "env WINTERP_STANDALONE_APP=TRUE winterp -init_file grep-br.lsp -no_stdin_serv -no_unix_serv"
; Author:       Niels Mayer
; Created:      Mon Nov 20 18:13:23 1989
; Modified:     Sun Jun  5 18:45:41 1994 (Niels Mayer) npm@indeed
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

;; Accelerators on browser:
;; (Note: bindings available everywhere other than "search regexp" and 
;; "wildcarded files" editors).
;;
;; 'E'  -- view selected item in user's editor.
;; '^E' -- select next item and view in user's editor.
;; '^N', ^<DownArrow>  -- select next item
;; '^P', ^<UpArrow>    -- select prev item
;; 'N' , <DownArrow>   -- view next item in built-in editor
;; 'P' , <UpArrow>     -- view prev item in built-in editor

;; Accelerators on editor:
;; (Note: bindings available everywhere other than "search regexp" and 
;; "wildcarded files" editors).
;;
;; <space>     -- pages forwards
;; <backspace> -- pages backwards


(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, *MOTIF-1.0-P*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the contents of this file have evolved into something that can only work
;; with Motif >= 1.1 and X11 >= r4. A more rudimentary grep browser is available
;; for Motif 1.0 -- see file grep-br1.0.lsp; we load that if we note you're
;; running Motif 1.0... Note that the 1.0 version isn't nearly as good as the
;; grep browser in this file. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if *MOTIF-1.0-P*
    (require "grep-br1.0.lsp")
(progn

(require "lib-utils/unixstuf")		;define winterp-standalone-p and other unixisms...
(require "lib-utils/show-busy")		;define WINTERP-SHOW-BUSY-PROGN
(require "lib-utils/redir-err")		;pops up dialog box showing stderr output
(require "lib-widgets/object-br")	;define Object_Browser_Widget_Class
(require "lib-widgets/file-view")	;define File_Viewer_Widget_Class

;; IF WINTERP started w/ "env WINTERP_STANDALONE_APP=TRUE winterp -init_file grep-br.lsp -no_stdin_serv -no_unix_serv"
(if (winterp-standalone-p)
    ;; THEN LOAD redir-out so that users get warned about XLISP errors occuring (e.g. from trying
    ;; browse a deleted file). Users using WINTERP interactively and loading this will probably 
    ;; not want their stdout suddenly appearing in a dialog box, so that's why we only load this
    ;; for a WINTERP application started standalone via "env WINTERP_STANDALONE_APP=TRUE ..."
    (require "lib-utils/redir-out")	;pops up dialog box showing stdout output
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an example of using multiple character sets with Object_Browser_Widget_Class
;; hopefully, your system has these fonts! if not, set this as appropriate...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *GREP-BR-NORMAL-CHARSET* "NORMAL_CS")
(defvar *GREP-BR-BOLD-CHARSET*	 "BOLD_CS")
(defvar *GREP-BR-FONTLIST*
  ;; N.B.: Motif 1.2.3 (and Irix 5.2) will puke if there are spaces/newlines
  ;; in the font-list definition, so don't allow space, Motif will either
  ;; generate an error message -- Warning: Unmatched quotation marks in string "", any remaining fonts in list unparsed
  ;; or it will core dump WINTERP. Therefore, don't put spaces/newlines here!
  (format nil
	  "fixed,-*-courier-medium-r-normal-*-12-*-*-*-m-*-iso8859-1=~A,-*-courier-bold-r-normal-*-12-*-*-*-m-*-iso8859-1=~A"
	  *GREP-BR-NORMAL-CHARSET* *GREP-BR-BOLD-CHARSET*
	  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *SYSTEM-EDITOR*:
;;; if NIL, then edit functionality will use editor set in environment variable 
;;; $EDITOR. If set to a string, then that string will be used as the name of
;;; the editor to use for the "Edit" button.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *SYSTEM-EDITOR* nil)

#| ;; NPM: commented out because simple-option-menu has motif bug
(require "xlisp-2.1d/common")		;define FILL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *GREP-EXECUTABLES*
;;; user may want to customize, by adding the following to their "~/.winterp"
;;; (setq *GREP-BR-EXECUTABLE* #("grep1" "grep2" "grep3" "grep4"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *GREP-EXECUTABLES*           #("grep" "fgrep" "egrep" "gnugrep"))
(defvar *GREP-EXECUTABLE-MNEMONICS* #(#\g    #\f     #\e      #\n))
|# ;; NPM: commented out because simple-option-menu has motif bug

; use this in place of above till option menu fixed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *GREP-BR-EXECUTABLE*
;;; user may want to customize, by adding the following to their "~/.winterp"
;;; (setq *GREP-BR-EXECUTABLE* "/usr/local/bin/gnugrep")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *GREP-BR-EXECUTABLE* "grep")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grep_Item_Class -- a BROWSER_OBJECT for display within
;; the Object_Browser_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Each BROWSER_OBJECT holds the information summarizing one line of 'grep'
;; output -- the line and filename matching the regexp suplied to 'grep'.
;; The information is split up into individual fields because we may want
;; to be able to sort on one field, or search for matches on one field.
;;
(defvar Grep_Item_Class
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

;; this method required if displaying Grep_Item_Class instances in Object_Browser_Widget_Class
(send Grep_Item_Class :answer :DISPLAY_STRING '()
      '(
	(let ((len (length file-name))
	      )
	  (if (> len 20)
	      (xm_string_concat
	       (xm_string_create (concatenate 'string
					      "..."
					      (subseq file-name (- len 17) nil)
					      ": ")
				 *GREP-BR-NORMAL-CHARSET*)
	       (xm_string_create match-line
				 *GREP-BR-BOLD-CHARSET*))
	    (xm_string_concat
	     (xm_string_create (format nil "~20A: " file-name)
			       *GREP-BR-NORMAL-CHARSET*)
	     (xm_string_create match-line
			       *GREP-BR-BOLD-CHARSET*))
	    )
	  )
	))

;; this method required if displaying Grep_Item_Class instances in Object_Browser_Widget_Class
(send Grep_Item_Class :answer :EXTERNAL_EDIT '()
      '(
	(system
	 (format nil 
		 "~A +~A ~A &"
		 (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
		 line-num file-name)
		)
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
;; This returns a list of Grep_Item_Class instances corresponding
;; to the items matching the search pattern and file list given
;; in argument <grep-arg-string>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun grep (executable-string flags-string grep-args-string)
  (do* 
   (;; loop variables, initializers, and increments.
    (fp (popen (concatenate 'string
		executable-string " "
		flags-string " "
		"-n "			;force grep to output line numbers
		grep-args-string
		" /dev/null"		;incase there's only one arg, forces filename to be output
		)
	       :direction :input))
    (line (send (send Grep_Item_Class :new) :read-grep-info fp)
	  (send (send Grep_Item_Class :new) :read-grep-info fp))
    (result '())			;init to an empty list
    )
   ;; loop test and return
   ((null line)				;:read-grep-info returns NIL on EOF
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
;; If optional parameter use-app-shell-p is true, then the top-level shell for
;; the grep-browser is an APPLICATION_SHELL_WIDGET inst, rather than a
;; TOP_LEVEL_SHELL_WIDGET inst. When use-app-shell-p is true, then
;; the f.kill ("Close") signal from the window manager will terminate
;; WINTERP, rather than just closing the window -- useful if running this
;; application "standalone".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun grep-browser (&optional use-app-shell-p)
  (let
      (;; declare local variables
       top_w paned_w controlpanel_w controlpanel3_w clone_button_w
#| ;; NPM: commented out because simple-option-menu has motif bug
       grepprog_opt_w
|# ;; NPM: commented out because simple-option-menu has motif bug
       doit_button_w case_toggle_w
       search_label_w  files_label_w 
       controlpanel2_w prev_button_w next_button_w
       srchsel_button_w edit_file_button_w
       filename_label_label_w filename_label_w
       list_w viewtext_w search_editor_w files_editor_w
       )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Create A Widget Hierarchy ;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setq top_w
	  (send (if use-app-shell-p APPLICATION_SHELL_WIDGET_CLASS TOP_LEVEL_SHELL_WIDGET_CLASS)
		:new "winterpGrepBr" "WinterpGrepBr"
		:XMN_TITLE		"WINTERP: Grep Browser"
		:XMN_ICON_NAME		"W:grep-br"
		))
    (setq paned_w
	  (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
		"paned_w" top_w 
		))
    (setq controlpanel_w
	  (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		"controlpanel_w" paned_w 
		:XMN_ORIENTATION	:horizontal
		:XMN_PACKING		:pack_tight
		))
    (setq clone_button_w
	  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		"clone_button_w" controlpanel_w 
		:XMN_LABEL_STRING	"New Search Window"
		))
    (setq doit_button_w
	  (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed ;NOTE -- this must be a WIDGET (not a GADGET) because ArmAndActivate() action is called below.
		"doit_button_w" controlpanel_w 
		:XMN_LABEL_STRING	"Do Search"
		:XMN_FILL_ON_ARM	t
		))
    (setq srchsel_button_w
	  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		"srchsel_button_w" controlpanel_w 
		:XMN_LABEL_STRING	"Search for Selected Text"
		))
#| ;; NPM: commented out because simple-option-menu has motif bug (in Motif < 1.1.3)
	(setq grepprog_opt_w
	      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_option_menu
		    "grep-opt" controlpanel_w
		    ;; :XMN_OPTION_LABEL "Search\nProgram:"
		    :XMN_OPTION_MNEMONIC	#\P
		    :XMN_BUTTON_COUNT		(length *GREP-EXECUTABLES*)
		    :XMN_BUTTONS		*GREP-EXECUTABLES*
		    :XMN_BUTTON_MNEMONICS	*GREP-EXECUTABLE-MNEMONICS*
	            :XMN_BUTTON_TYPE		(fill (make-array (length *GREP-EXECUTABLES*)) :PUSHBUTTON)
		    :XMN_BUTTON_SET		0
		    ))
|# ;; NPM: commented out because simple-option-menu has motif bug

    (setq case_toggle_w
	  (send XM_TOGGLE_BUTTON_GADGET_CLASS :new :managed
		"case_toggle_w" controlpanel_w 
		:XMN_LABEL_STRING	"Case Sensitive Search"
		:XMN_SET		T
		))
    (setq controlpanel3_w
	  (send XM_FORM_WIDGET_CLASS :new :managed
		"controlpanel3_w" paned_w 
		))
    (setq search_label_w
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"search_label_w" controlpanel3_w
		:XMN_LABEL_STRING	"Search Regexp:"
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_form
		))
    (setq search_editor_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed ;note that XM_TEXT_FIELD_WIDGET_CLASS is glitchy with :XMN_RESIZE_WIDTH & :XMN_ALLOW_RESIZE set
		"search_editor_w" controlpanel3_w
		:XMN_EDIT_MODE			:single_line_edit
		:XMN_EDITABLE			t ;don't allow user to change text.
		:XMN_CURSOR_POSITION_VISIBLE	t ;yes, we want a cursor
		:XMN_AUTO_SHOW_CURSOR_POSITION	t ;need to show where the cursor is
		:XMN_RESIZE_WIDTH		t
		:XMN_ALLOW_RESIZE		t
		:XMN_TOP_ATTACHMENT		:attach_form
		:XMN_BOTTOM_ATTACHMENT		:attach_form
		:XMN_LEFT_ATTACHMENT		:attach_form
		:XMN_LEFT_ATTACHMENT		:attach_widget
		:XMN_LEFT_WIDGET		search_label_w
		))
    (setq files_label_w
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"files_label_w" controlpanel3_w
		:XMN_LABEL_STRING	"Wildcarded Files:"
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_widget
		:XMN_LEFT_WIDGET	search_editor_w
		))
    (setq files_editor_w
	  (send XM_TEXT_WIDGET_CLASS :new :managed ;note that XM_TEXT_FIELD_WIDGET_CLASS is glitchy with :XMN_RESIZE_WIDTH & :XMN_ALLOW_RESIZE set
		"files_editor_w" controlpanel3_w
		:XMN_EDIT_MODE			:single_line_edit
		:XMN_EDITABLE			t ;don't allow user to change text.
		:XMN_CURSOR_POSITION_VISIBLE	t ;yes, we want a cursor
		:XMN_AUTO_SHOW_CURSOR_POSITION	t ;need to show where the cursor is
		:XMN_RESIZE_WIDTH		t
		:XMN_ALLOW_RESIZE		t
		:XMN_TOP_ATTACHMENT		:attach_form
		:XMN_BOTTOM_ATTACHMENT		:attach_form
		:XMN_LEFT_ATTACHMENT		:attach_widget
		:XMN_LEFT_WIDGET		files_label_w
		:XMN_RIGHT_ATTACHMENT		:attach_form
		))
    (setq list_w
	  (send Object_Browser_Widget_Class :new :managed
		"list_w" paned_w
		:XMN_FONT_LIST		*GREP-BR-FONTLIST*
		:XMN_VISIBLE_ITEM_COUNT	5
		:XMN_LIST_SIZE_POLICY	:RESIZE_IF_POSSIBLE
		))
    (setq controlpanel2_w
	  (send XM_FORM_WIDGET_CLASS :new :managed
		"controlpanel2_w" paned_w 
		))
    (setq prev_button_w
	  (send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
		"prev_button_w" controlpanel2_w 
		:XMN_ARROW_DIRECTION	:arrow_up
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_form
		))
    (setq next_button_w
	  (send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
		"prev_button_w" controlpanel2_w 
		:XMN_ARROW_DIRECTION	:arrow_down
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_widget
		:XMN_LEFT_WIDGET	prev_button_w
		))
    (setq edit_file_button_w
	  (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		"edit_button_w" controlpanel2_w 
		:XMN_LABEL_STRING	"$EDITOR(sel)"
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_widget
		:XMN_LEFT_WIDGET	next_button_w
		))
    (setq filename_label_label_w
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"filename_label_label_w" controlpanel2_w
		:XMN_LABEL_STRING	" Viewed File:"
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_widget
		:XMN_LEFT_WIDGET	edit_file_button_w
		:XMN_ALIGNMENT		:alignment_end
		))
    (setq filename_label_w
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"filename_label_w" controlpanel2_w
		:XMN_LABEL_STRING	"[ None ]"
		:XMN_TOP_ATTACHMENT	:attach_form
		:XMN_BOTTOM_ATTACHMENT	:attach_form
		:XMN_LEFT_ATTACHMENT	:attach_widget
		:XMN_LEFT_WIDGET	filename_label_label_w
		:XMN_RIGHT_ATTACHMENT	:attach_form
		:XMN_ALIGNMENT		:alignment_beginning
		))
    (setq viewtext_w
	  (send File_Viewer_Widget_Class :new :managed
		"viewtext_w" paned_w
		:XMN_ROWS		10
		:XMN_COLUMNS		80
		))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;; Setup Keyboard Accelerators ;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (send list_w :set_values
	  :XMN_ACCELERATORS	"#override \
		<Key>E:		Lisp(send ACTION_WIDGET :edit_selected_item) \
		Ctrl<Key>N:	Lisp(send ACTION_WIDGET :goto_next) \
		Ctrl<Key>osfDown: Lisp(send ACTION_WIDGET :goto_next) \
		Ctrl<Key>P:	Lisp(send ACTION_WIDGET :goto_prev) \
		Ctrl<Key>osfUp:	Lisp(send ACTION_WIDGET :goto_prev) \
		<Key>N:		Lisp(send ACTION_WIDGET :browse_next ACTION_XEVENT) \
		<Key>osfDown:	Lisp(send ACTION_WIDGET :browse_next ACTION_XEVENT) \
		<Key>P:		Lisp(send ACTION_WIDGET :browse_prev ACTION_XEVENT) \
		<Key>osfUp:	Lisp(send ACTION_WIDGET :browse_prev ACTION_XEVENT)"
	  )

    (send viewtext_w :set_values
	  :XMN_ACCELERATORS	"#override \
		<Key>space:	   next-page() \
		<Key>osfBackSpace: previous-page()"
	  )

    (send list_w :install_all_accelerators top_w)
    (send viewtext_w :install_all_accelerators top_w)
    (send controlpanel_w :install_all_accelerators top_w)
    (send controlpanel2_w :install_all_accelerators top_w)
    (send controlpanel3_w :install_all_accelerators top_w)
    (send paned_w :install_all_accelerators top_w)
    (send doit_button_w :install_all_accelerators top_w) ;this is a widget, so it needs accels, all other buttons are gadgets so they don't...

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Realize Widgets to find out real sizes, then diddle constraints... ;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (send top_w :realize)

    ;;
    ;; set constraint resources on controlpanels so that paned window
    ;; doesn't give them resize sashes.
    ;;
    (let (height)

      (send controlpanel_w :get_values :xmn_height 'height)
      (send controlpanel_w :set_values
	    :XMN_PANE_MAXIMUM height
	    :XMN_PANE_MINIMUM height
	    )

      (send controlpanel2_w :get_values :xmn_height 'height)
      (send controlpanel2_w :set_values
	    :XMN_PANE_MAXIMUM height
	    :XMN_PANE_MINIMUM height
	    )

      (send controlpanel3_w :get_values :xmn_height 'height)
      (send controlpanel3_w :set_values
	    :XMN_PANE_MAXIMUM height
	    :XMN_PANE_MINIMUM height
	    )
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;  Setup Callbacks ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;
    ;; The "Clone" button creates another browser
    ;;
    (send clone_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (grep-browser)
	    ))
    ;;
    ;; The doit_button initiates a grep search.
    ;;
    (send doit_button_w :add_callback :XMN_ARM_CALLBACK '() ;NOTE -- using :XMN_ARM_CALLBACK rather than :XMN_ACTIVATE_CALLBACK because the former will keep the button filled (indicating search-in-progress) while grep is doing it's thing.
	  `(
	    (winterp-show-busy-progn
	     ;; clear out old list while 'grep' is thinking
	     (send ,list_w :set_browser_items '())
	     (send ,list_w :update_display)
	    
	     ;; set new list contents to result of grep
	     (send ,list_w :set_browser_items
		   (grep 
		    *GREP-BR-EXECUTABLE* ;use this in place of commented out option menu code below ... for now.
#| ;; NPM: commented out because simple-option-menu has motif bug
                   (xm_string_get_l_to_r (send (send ,grepprog_opt_w :get :xmn_menu_history) :get :xmn_label_string))
|# ;; NPM: commented out because simple-option-menu has motif bug
		    (if (send ,case_toggle_w :get_state) "" "-i")
		    (concatenate 'string
				 "'"	;quotify string to protect regexps from being expanded by shell
				 (send ,search_editor_w :get_string) ;string to search for
				 "' "	;quotify string to protect regexps from being expanded by shell
				 (send ,files_editor_w :get_string)) ;wildcarded files
		    )
		   )
	     )
	    ))
    ;;
    ;; Bind doit_button arm callback to <return> key in search_editor_w.
    ;;
    (send search_editor_w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	  `(
	    (send ,doit_button_w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT) 
	    ))
    ;;
    ;; Bind doit_button arm callback to <return> key in files_editor_w.
    ;;
    (send files_editor_w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	  `(
	    (send ,doit_button_w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT) 
	    ))
    ;; 
    ;; This callback will get the selection from the viewtext_w first, set
    ;; that to the search string, and then call doit_button arm callback.
    ;;
    (send srchsel_button_w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	  `(
	    (send ,search_editor_w :set_string (send ,viewtext_w :get_selection))
	    (send ,doit_button_w :call_action_proc "ArmAndActivate" CALLBACK_XEVENT) 
	    ))
    ;;
    ;; On pressing "View Selection in $EDITOR" button, call the *SYSTEM-EDITOR*,
    ;; else call $EDITOR on file in view area.
    ;;
    (send edit_file_button_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	  `(
	    (send ,list_w :edit_selected_item)
	    ))
    ;;
    ;; Add callbacks for buttons browsing prev/next items in list
    ;;
    (send prev_button_w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	  `(
	    (send ,list_w :browse_prev CALLBACK_XEVENT)
	    ))
    (send next_button_w :add_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_XEVENT)
	  `(
	    (send ,list_w :browse_next CALLBACK_XEVENT)
	    ))
    ;;
    ;; set up a callback on the list widget such that a double click (button 1)
    ;; on the browser-item will browse the object.
    ;;
    (send list_w :add_callback :XMN_DEFAULT_ACTION_CALLBACK '()
	  `(
	    (winterp-show-busy-progn
	     (let ((browsed-object (send ,list_w :get_selected_item)))
	       (if browsed-object	;set to NIL if no browsed object
		   (let ((filename (send browsed-object :file-name))
			 (linenum  (send browsed-object :line-num)))
		     (send ,filename_label_w :set_values :XMN_LABEL_STRING filename)
		     (send ,filename_label_w :update_display) ;incase reading file takes long time
		     (send ,viewtext_w :find_file filename linenum)
		     ))
	       )
	     )
	    ))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bring up an instance of the grep browser upon loading this file.
;;
;; if this application started up stand-alone, with
;; "env WINTERP_STANDALONE_APP=TRUE winterp -init_file grep-br.lsp -no_stdin_serv -no_unix_serv"
;; the grep-browser created here will use an APPLICATION_SHELL_WIDGET instance
;; (window manager f.kill will terminate winterp rather than just close window).
(grep-browser (winterp-standalone-p))	;TRUE when envvar WINTERP_STANDALONE_APP is set

) ;; end -- progn					
) ;; end -- "if *MOTIF-1.0-P*"
