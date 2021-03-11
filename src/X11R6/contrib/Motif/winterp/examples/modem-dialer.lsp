; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         modem-dialer.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/modem-dialer.lsp,v 2.15 1994/06/06 14:43:07 npm Exp $
; Description:  Dials phone numbers on a Hayes-Compatible modem by running
;		kermit(1) as an asynchronous subprocess. Hayes-compatible
;		commands may be sent to the modem directly by entering text
;		in the edit-field widget directly below the menu bar. The app.
;		provides a browser of people/phone-numbers -- in the browser,
;		double-left click (or single-middle click) on the person to dial
;		his/her number (details on mouse and key bindings below).
;		Put your database of people in file "$HOME/people.lsp" (see
;		variable *MODEM-DIALER-FILEPATH* below)
;		To start up this modem-dialer "standalone", do
;		"env WINTERP_STANDALONE_APP=TRUE winterp -init_file modem-dialer.lsp -no_stdin_serv -no_unix_serv"
;		or run shell script ../bin/win-dialer. Running the app this
;		way will cause WINTERP to terminate when the main modem-dialer
;		window is closed, rather than just deleting the window.
; Author:       Niels Mayer
; Created:      Wed Sep 16 23:00:03 1992
; Modified:     Sun Jun  5 19:09:44 1994 (Niels Mayer) npm@indeed
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

;;
;; Mouse bindings on "people browser" (Object_Browser_Widget_Class):
;; single left click   -- select item 
;; double left click   -- select item and dial the number associated with person.
;; single middle click -- select item and dial the number associated with person.
;; single right click  -- select item and beep indicating no external edit avail.
;;
;; Key bindings on "people browser" (Object_Browser_Widget_Class):
;; '^N', ^<DownArrow>, 'N' , <DownArrow>  -- select next item
;; '^P', ^<UpArrow>  , 'P' , <UpArrow>    -- select prev item
;;
;; Key bindings on "transcript display" (Text_Display_Widget_Class):
;; Space     -- page forward.
;; Backspace -- page backwards.
;;

(require "lib-utils/unixstuf")		;define winterp-standalone-p decide-winterp-top-level-shell, *HOME-DIRECTORY-STR*, and other unixisms...
(require "lib-utils/redir-err")		;pops up dialog box showing stderr output
(require "lib-widgets/application")	;define WINTERP:APPLICATION-WIDGET-CLASS
(require "lib-widgets/clock-disp")	;define Clock_Display_Widget_Class
(require "lib-widgets/object-br")	;define Object_Browser_Widget_Class
(require "lib-widgets/text-view")	;define Text_Display_Widget_Class


;; put your database of people in the file referred to by variable
;; *MODEM-DIALER-FILEPATH* (default is "$HOME/people.lsp").
;; An example of the contents of the file:
;; (setq *people-list* (list
;;  (send person_class :new "BASS Tickets" ""
;;	  "1-510-762-2277")
;;  (send person_class :new "CALTRANS Road Cond." ""
;;        "1-800-427-7623")
;;  (send person_class :new "Hot Sexy Babes in Heat" ""
;;	  "1-900-666-6666")
;;  (send person_class :new "Chronicle Road Cond" ""
;;        "512-5000,,,,3140")
;;  (send person_class :new "NETCOM" "24x7 NOC"
;;        "1-408-554-8717")
;; ))

(defvar *MODEM-DIALER-FILEPATH* 
  (concatenate 'string *HOME-DIRECTORY-STR* "/people.lsp"))

;; IF WINTERP started w/ "env WINTERP_STANDALONE_APP=TRUE winterp -init_file ..."
(if (winterp-standalone-p)
    ;; THEN LOAD redir-out so that users get warned about XLISP errors occuring (e.g. from trying
    ;; browse a deleted file). Users using WINTERP interactively and loading this will probably 
    ;; not want their stdout suddenly appearing in a dialog box, so that's why we only load this
    ;; for a WINTERP application started standalone via "env WINTERP_STANDALONE_APP=TRUE ..."
    (require "lib-utils/redir-out")	;pops up dialog box showing stdout output
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Person_Class  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar person_class
#|
  nil)
  (setq fldr_cmpbr:Mail_Folder_Class
|#
  (send Class :new
	'( ;;;;;;;;;;;;;;;;;;;;;;; INSTANCE VARIABLES ;;;;;;;;;;;;;;;;;;;;;
	  first-name-str
	  last-name-str
	  phone-number-str
	  )
	'( ;;;;;;;;;;;;;;;;;;;;;;; CLASS VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;
	  )
	))

;; this method required if displaying person_class instances in Object_Browser_Widget_Class
(send person_class :answer :DISPLAY_STRING
      '()
      '(
	(format nil "~20A ~20A ~15A"
		first-name-str
		last-name-str
		phone-number-str)
	))

;; this method required if displaying person_class instances in Object_Browser_Widget_Class
;; in this case, it is a NO-OP
(send person_class :answer :EXTERNAL_EDIT
      '()
      '(
	(X_BELL)			;SIGNAL ERROR -- BEEP
	))

(send person_class :answer :ISNEW
      '(first-name last-name phone-no)
      '(
	(setq first-name-str first-name)
	(setq last-name-str  last-name)
	(setq phone-number-str phone-no)
	self
	))

(send person_class :answer :DIAL-PERSON
      '(f)
      '(
	(format f "ATDT ~A\r\n" phone-number-str)
	))

(defvar *people-list* NIL)

(if (not (load *MODEM-DIALER-FILEPATH* :verbose nil :print nil))
    (error
     "modem-dialer couldn't load list of phone numbers from *MODEM-DIALER-FILEPATH*"
     *MODEM-DIALER-FILEPATH*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; MODEM_DIALER_APPLICATION_SHELL_CLASS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODEM_DIALER_APPLICATION_SHELL_CLASS --> <SHELL_WIDGET_CLASS>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Modem_Dialer_Application_Shell_Class
      (send Class :new 
	    '( ;;;;;;;;;;;;;;;;;;;;;;; INSTANCE VARIABLES ;;;;;;;;;;;;;;;;;;;;;
	      ivar_repeat_to
	      ivar_kermit_subproc_pty
	      ivar_kermit_subproc_pid
	      ivar_kermitxterm_subproc_pty
	      ivar_kermitxterm_subproc_pid
	      ivar_input-cb
	      ivar_kermitxterm_but_w  
	      ivar_dialer_toggle_w
	      ivar_main_w
	      ivar_command_window_w
	      ivar_clock_w
	      ivar_command_editor_w
	      ivar_work_window_w
	      ivar_te_w
	      ivar_list_w
	      )
	    '( ;;;;;;;;;;;;;;;;;;;;;;; CLASS VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;
	      )
	    ;; Superclass is TOP_LEVEL_SHELL_WIDGET_CLASS or APPLICATION_SHELL_WIDGET_CLASS
	    ;; depending on state of environment variable $WINTERP_STANDALONE_APP.
	    (decide-winterp-top-level-shell) 
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization method.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :ISNEW
      '(name &rest args)
      '(
	;; initialize instance variables
	(setq ivar_kermitxterm_but_w nil)	;initialize for :START-KERMITXTERM-SUBPROCESS

	;; initialize SELF, instance of <SHELL_WIDGET_CLASS>
	(apply #'send-super :isnew
	       name
	       args
	       )
	(setq ivar_main_w
	      (send WINTERP:APPLICATION-WIDGET-CLASS :new :managed
		    "mainw" self
		    ))

	;; -------------- BEGIN: SETUP PULLDOWN MENU SYSTEM -----------------
	(send ivar_main_w :add-menu-entry "System"
	      :mnemonic		         #\S)
	(send ivar_main_w :add-menu-entry '("System" "Quit")
	      :mnemonic		                    #\Q
	      :callback		#'(lambda (w x) (send self :quit-application)))
	(send ivar_main_w :add-menu-entry '("System" "Load Database (~/people.lsp)")
	      :mnemonic		                    #\L
	      :callback		#'(lambda (w x) (send self :load-database)))
	(send ivar_main_w :add-menu-entry '("System" "Repeat Last Cmd (5s)")
	      :mnemonic		                    #\R
	      :callback		#'(lambda (w x) (send self :repeat-last-command)))
	(send ivar_main_w :add-menu-entry '("System" "Stop Repeat")
	      :mnemonic		                    #\S
	      :callback		#'(lambda (w x) (send self :stop-repeat-last-command)))

	(send ivar_main_w :add-menu-entry "Terminal Mode"
	      :mnemonic		                  #\M)
	(send ivar_main_w :add-menu-entry '("Terminal Mode" "Dialer")
	      :mnemonic		                           #\D
	      :type		:radiobutton
	      :callback		#'(lambda (w x) ;note kludge to make menu entries act like radio buttons.
				    (cond ((send w :get_state)
					   (send (aref (send (send w :parent) :get_children) 1) :set_state nil nil)
					   (if ivar_kermitxterm_subproc_pty
					       (send self :stop-kermitxterm-subprocess))
					   (if (null ivar_kermit_subproc_pty)
					       (send self :start-kermit-subprocess))
					   )
					  (T
					   (send (aref (send (send w :parent) :get_children) 1) :set_state t t)
					   ))))
	(send ivar_main_w :add-menu-entry '("Terminal Mode" "Xterm")
	      :mnemonic		                           #\X
	      :type		:radiobutton
	      :callback		#'(lambda (w x)	;note kludge to make menu entries act like radio buttons.
				    (setq ivar_dialer_toggle_w
					  (aref (send (send w :parent) :get_children) 0))
				    (cond ((send w :get_state)
					   (send ivar_dialer_toggle_w :set_state nil nil)
					   (if ivar_kermit_subproc_pty
					       (send self :stop-kermit-subprocess))
					   (if (null ivar_kermitxterm_subproc_pty)
					       (send self :start-kermitxterm-subprocess))
					   )
					  (T
					   (send ivar_dialer_toggle_w :set_state t t)
					   ))))

	(send ivar_main_w :add-menu-entry "Transcript Display"
	      :mnemonic		         #\T)
	(send ivar_main_w :add-menu-entry '("Transcript Display" "Clear")
	      :mnemonic		                                #\C
	      :callback		#'(lambda (w x) (send ivar_te_w :clear)))
	(send ivar_main_w :add-menu-entry '("Transcript Display" "Save")
	      :mnemonic		                                #\S
	      :callback		#'(lambda (w x) (send ivar_te_w :save-in-file-dialog
						      "Input filename for saving transcipt:")))

	(send ivar_main_w :add-menu-entry "Help"
	      :mnemonic		         #\H)
	(send ivar_main_w :add-menu-entry '("Help" "Help")
	      :mnemonic		                 #\H
	      :callback		#'(lambda (w x) (send ivar_main_w :error-display-string
						      "Help not implemented!")))

	(send ivar_main_w :make-menus)
	(send ivar_main_w :set-menu-help-widget)
	;; -------------- END: SETUP PULLDOWN MENU SYSTEM ------------------

	(setq ivar_work_window_w
	      (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
		    "work_window" ivar_main_w
		    ))
	(send ivar_main_w :set-work-area ivar_work_window_w)

	(setq ivar_command_window_w		;don't make this an XmNcommandWindow for XmMainWindow since Motif doesn't work right when you do that
	      (send XM_FORM_WIDGET_CLASS :new :managed
		    "command_window" ivar_work_window_w
		    ))
	(setq ivar_clock_w
	      (send Clock_Display_Widget_Class :new :managed
		    "clock" ivar_command_window_w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    ))
	(setq ivar_command_editor_w 
	      (send XM_TEXT_FIELD_WIDGET_CLASS :new :managed
		    "command_editor" ivar_command_window_w
		    :XMN_COLUMNS 70
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_widget
		    :XMN_LEFT_WIDGET		ivar_clock_w
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    ))
	(setq ivar_list_w
	      (send Object_Browser_Widget_Class :new :managed
		    "list" ivar_work_window_w
		    :XMN_LIST_SIZE_POLICY	:variable
		    :XMN_SCROLL_BAR_DISPLAY_POLICY :static
		    :XMN_VISIBLE_ITEM_COUNT	8
		    ))
	;; override 'N' , <DownArrow>, 'P' , <UpArrow> bindings from
	;; Object_Browser_Widget_Class s.t. they don't "browse" the
	;; item being traversed to. In the case of the modem dialer
	;; this could be a bad thing, since it would dial phone numbers.
	(send ivar_list_w :OVERRIDE_TRANSLATIONS
	      "<Key>N:		Lisp(send ACTION_WIDGET :goto_next) \
		<Key>osfDown:	Lisp(send ACTION_WIDGET :goto_next) \
		<Key>P:		Lisp(send ACTION_WIDGET :goto_prev) \
		<Key>osfUp:	Lisp(send ACTION_WIDGET :goto_prev)"
	      )
	(send ivar_list_w :set_browser_items *people-list*)

	(setq ivar_te_w
	      (send Text_Display_Widget_Class :new :managed
		    "te" ivar_work_window_w
		    :XMN_COLUMNS		80
		    :XMN_ROWS			12
		    ))

	(send-super :realize)

	;;??;;	(send (aref (send terminal_mode_pulldown_w :get_children) 0) :set_state t t)
	;;??;;	(send (aref (send terminal_mode_pulldown_w :get_children) 1) :set_state nil nil)

	;; Set constraint resources on controlpanels so that paned window
	;; doesn't give them resize sashes. (This must be done after 
	;; :REALIZE so we can find out exact widget sizes).
	(let (height)
	  (send ivar_command_window_w :get_values :xmn_height 'height)
	  (send ivar_command_window_w :set_values
		:XMN_PANE_MAXIMUM height
		:XMN_PANE_MINIMUM height
		))

	;; Set Up Callbacks

	;; double click on list item dials number.
	(send ivar_list_w :add_callback :XMN_DEFAULT_ACTION_CALLBACK
	      '(CALLBACK_WIDGET)
	      '(
		(if ivar_kermit_subproc_pty
		    (let ((item (send CALLBACK_WIDGET :get_selected_item)))
		      (send item :dial-person ivar_kermit_subproc_pty)
		      (send ivar_main_w :display-string
			    (format nil "Dialing: ~A" (send item :display_string)))
		      )
		  )
		))

	(send ivar_command_editor_w :add_callback :XMN_ACTIVATE_CALLBACK ;invoke when <return> ... hit.
	      '(CALLBACK_WIDGET)	;bound to the current value of ivar_command_editor_w
	      '(			;code to execute
		(if ivar_kermit_subproc_pty
		    (format ivar_kermit_subproc_pty "~A\r\n" ;send text in editor to the subprocess
			    (send CALLBACK_WIDGET :get_string))
;;;		    (format ivar_kermit_subproc_pty "~A\n" ;send text in editor to the subprocess
;;;			    (send CALLBACK_WIDGET :get_string))
		  )
		)
	      )

	;; start the subprocess!
	(send self :start-kermit-subprocess)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :START-KERMIT-SUBPROCESS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :START-KERMIT-SUBPROCESS
      '()
      '(
	(exp_stty_init "9600 echo")	;echo characters from exp_spawn'd subprocess, need to force this because running WINTERP as emacs subprocess doesn't echo, force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
	(setq ivar_kermit_subproc_pty (exp_spawn "kermit" "kermit")) ;create subprocess, the Modem through 'kermit'
	(setq ivar_kermit_subproc_pid (exp_get_pid))
	(send ivar_te_w :clear)

	;; for every line output from kermit(1), append result to text widget...
	(setq ivar_input-cb
	      (xt_add_input		;XtAppAddInput()
	       ivar_kermit_subproc_pty :READ_LINE_TO_STRING
	       '(;; READ_LINE_TO_STRING fires callback once per line, binding line to FDINPUTCB_STRING
		 ;; append lines into ivar_te_w
		 (if (string= FDINPUTCB_STRING "")
		     ;; if blank lines, just insert a blank line...
		     (send ivar_te_w :append-string "\n")
		   ;; else "timestamp" the output... useful for recording when calls made/received
		   (send ivar_te_w :append-string (concatenate 'string
							  (send ivar_clock_w :get-time-string)
							  " >> " 
							  FDINPUTCB_STRING
							  "\n"))
		   ))
	       ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :STOP-KERMIT-SUBPROCESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :STOP-KERMIT-SUBPROCESS
      '()
      '(
	(xt_remove_input ivar_input-cb)	;must remove this before closing
	(exp_kill "INT" ivar_kermit_subproc_pid) ;OSF1 seems to need this, otherwise 'exp_wait' hangs
	(close ivar_kermit_subproc_pty)	;however, for most systems 'close' will stop the subprocess
	(exp_wait)			;wait on the subprocess
	(setq ivar_input-cb nil)	;allow the input-callback to be freed by GC
	(setq ivar_kermit_subproc_pid nil)
	(setq ivar_kermit_subproc_pty nil)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :START-KERMITXTERM-SUBPROCESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :START-KERMITXTERM-SUBPROCESS
      '()
      '(
	(exp_stty_init "9600 echo")	;echo characters from exp_spawn'd subprocess, need to force this because running WINTERP as emacs subprocess doesn't echo, force 9600 baud since WINTERP-as-emacs-subprocess defaults to 300 (??)
	(setq ivar_kermitxterm_subproc_pty
	      (exp_spawn "xterm" "xterm"
			 "-name" "kermitxterm"
			 "-e" "kermit"
			 ))
	(setq ivar_kermitxterm_subproc_pid
	      (exp_get_pid)
	      )
	(send ivar_main_w :unmanage)
	(if ivar_kermitxterm_but_w
	    (send ivar_kermitxterm_but_w :manage)
	  (progn
	    (setq ivar_kermitxterm_but_w
		  (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed
			"kermitxterm_but" self
			:XMN_LABEL_TYPE	:string
			:XMN_LABEL_STRING	"\nPUSH THIS BUTTON TO RESTART MODEM-DIALER\n(make sure to quit kermit running in\nxterm; if in \"connect\" mode, type\n \"Ctrl-\\ C quit\" , where \"Ctrl-\\ C\"\n is kermit's default escape sequence)\n"
			))
	    (send ivar_kermitxterm_but_w :add_callback :xmn_activate_callback '()
		  '(
		    (send ivar_kermitxterm_but_w :unmanage)
		    (send ivar_main_w :manage)
		    (send ivar_dialer_toggle_w :set_state t t) ;this will stop kermitxterm, start kermit-dialer, by virtue of second 't' arg.
		    ))
	    ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :STOP-KERMITXTERM-SUBPROCESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :STOP-KERMITXTERM-SUBPROCESS
      '()
      '(
;;;	(format ivar_kermitxterm_subproc_pty "\034cquit\r\n") ;kermit-escape&quit, kermit escape is CTRL-\c == \034c
	(progv '(*breakenable*) '(nil)
	       (errset			;wrap errors just incase user manually quit 'kermitxterm'
		(exp_kill "INT" ivar_kermitxterm_subproc_pid) ;OSF1 seems to need this, otherwise 'exp_wait' hangs
		nil)
	       )
	(close ivar_kermitxterm_subproc_pty) ;however, for most systems 'close' will stop the subprocess
	(exp_wait)			;wait on the subprocess
	(setq ivar_kermitxterm_subproc_pid nil)
	(setq ivar_kermitxterm_subproc_pty nil)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :QUIT-APPLICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :QUIT-APPLICATION
      '()
      '(
	(if ivar_kermit_subproc_pty
	    (send self :stop-kermit-subprocess)
	  )
	(if ivar_kermitxterm_subproc_pty
	    (send self :stop-kermitxterm-subprocess)
	  )
	(if (winterp-standalone-p) 
	    (exit)
	  )
	(send self :destroy)		;sayonara, sucker.
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :LOAD-DATABASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :LOAD-DATABASE
      '()
      '(
	(send ivar_main_w :display-string 
	      (format nil "Loading '~A' ... " *MODEM-DIALER-FILEPATH*))
	(cond ((load *MODEM-DIALER-FILEPATH* :verbose nil :print nil)
	       (send ivar_list_w :set_browser_items *people-list*)
	       (send ivar_main_w :display-string 
		     (format nil "Loading '~A' ... DONE." *MODEM-DIALER-FILEPATH*))
	       )
	      (T
	       (send ivar_main_w :error-display-string 
		     (format nil "Error loading '~A'." *MODEM-DIALER-FILEPATH*))
	       ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :REPEAT-LAST-COMMAND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :REPEAT-LAST-COMMAND
      '()
      '(
	(setq ivar_repeat_to
	      (xt_add_timeout
	       0
	       '(
		 (if ivar_kermit_subproc_pty
		     (progn
		       (format ivar_kermit_subproc_pty "A/\r\n")
		       (setq ivar_repeat_to (xt_add_timeout 5000 TIMEOUT_OBJ))
		       ))
		 )
	       ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method :STOP-REPEAT-LAST-COMMAND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send Modem_Dialer_Application_Shell_Class :answer :STOP-REPEAT-LAST-COMMAND
      '()
      '(
	(if ivar_repeat_to
	    (xt_remove_timeout ivar_repeat_to))
	(setq ivar_repeat_to NIL)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Create the modem-dialer application ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *modem-dialer-w* NIL)
(setq *modem-dialer-w*
      (send Modem_Dialer_Application_Shell_Class :new "modem-dialer"
;;	    :XMN_TITLE		"WINTERP: Modem Dialer"
;;	    :XMN_ICON_NAME	"W:modem-dial"
	    ))
