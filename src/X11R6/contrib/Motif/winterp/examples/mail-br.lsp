; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         mail-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/mail-br.lsp,v 2.5 1994/06/06 14:43:10 npm Exp $
; Description:  A simple MH mail browser based on Object_Browser_Widget_Class.
;		This does MH's scan(1) operation on the last:30 messages in
;		folder +inbox, then allows you to browse the messages via
;		the following mouse/key bindings on the message browser:
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

; This application assumes
; (1) you have the MH, mail system (works w/ versions 6.7 and 6.8, poss. others).
; (2) you have a folder +inbox, containing some messages
; (3) MH's "scan" is on your $PATH.
; (4) Your MH Mail directory is ~/Mail (special settings from ~/.mh_profile not read)

(require "lib-utils/motif-vers")	;define *MOTIF-1.1-OR-LATER-P*, *MOTIF-1.0-P*
(require "lib-utils/initialize")	;define *SYSTEM-EDITOR*, etc.
(require "lib-utils/unixstuf")		;define *HOME-DIRECTORY-STR*, etc
(require "lib-utils/redir-err")		;pops up dialog box showing stderr output
(require "lib-widgets/object-br")	;define Object_Browser_Widget_Class
(require "lib-widgets/file-view")	;define File_Viewer_Widget_Class

(defvar *MH-MAILPATH*
  (concatenate 'string *HOME-DIRECTORY-STR* "/Mail") ;this is the default directory
					;for MH... this assumes you haven't
					;put the MH directory elsewhere
					;via a ~/.mh_profile entry.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail_Message_Class -- a BROWSER_OBJECT for display within the
;; Object_Browser_Widget_Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Each BROWSER_OBJECT holds the information summarizing one mail message.
;; the information is split up into individual fields because we may want
;; to be able to sort on one field, or search for matches on one field.
;;
(defvar Mail_Message_Class
  (send Class :new
	'(folder num anno month date no-date size sender subject)
	))

;; this string is passed to the mh 'scan' and 'inc' commands to determine
;; the formatting of the output of the message info summary. Each entry
;; here corresponds to an instance variable in Mail_Message_Class
(setq *MH-FOLDER-SCAN-FORMAT* 
      (concatenate 'string
       "%(msg)"				;output the message number
       "%<{replied}A%|"			;IF msg answered output "A" ELSE
       "%<{forwarded}F%|"		;IF msg forwarded output "F" ELSE
       "%<{resent}R%|"			;IF msg redisted output "R" ELSE
       "%<{printed}P%|"			;IF msg printed output "P"
       " %>%>%>%>"			;ELSE output " "
       "%02(mon{date})/%02(mday{date})"	;output mon/date
       "%<{date} %|*%>"			;IF no date output "*" else " "
       "%(size) "			;output the message's size
       "%<(mymbox{from})To:%14(friendly{to})%|"	;IF my message, output "To: <recipient>"
       "%17(friendly{from})%> "		;ELSE output sender field
       "%{subject}<<"			;output subject followed by ">>"
       "%{body}"			;output beginning of body, limited by SCAN_OUTPUT_WIDTH
       )
      )

;; this method will read a single line summary of a mail message as produced
;; by the mh 'scan' or 'inc' commands and sets the instance variables in the 
;; BROWSER_OBJECT to the individual fields of the message summary.
(send Mail_Message_Class :answer :READ-MSG-INFO '(pipe fldr)
      '(
	(if (and
	     (setq folder fldr)
	     (setq num     (fscanf-fixnum pipe "%ld"))
	     (setq anno    (fscanf-string pipe "%c"))
	     (setq month   (fscanf-fixnum pipe "%2ld"))
	     (setq date    (fscanf-fixnum pipe "/%2ld"))
	     (setq no-date (fscanf-string pipe "%c"))
	     (setq size    (fscanf-fixnum pipe "%d%*c"))
	     (setq sender  (fscanf-string pipe "%17[\001-\177]%*c"))
	     (setq subject (fscanf-string pipe "%[^\n]\n"))
	     )
	    self			;return self if succesful
	  NIL				;return NIL if hit EOF
	  )
	)
      )

;; this method required if displaying Mail_Message_Class instances in
;; Object_Browser_Widget_Class
(send Mail_Message_Class :answer :DISPLAY_STRING '()
      '(
	(format nil
		"~5A ~1A ~2,,,'0@A/~2,,,'0@A~1A ~17A ~5A ~A"
		num anno month date no-date sender size subject)
	))

;; this method required if displaying Mail_Message_Class instances in
;; Object_Browser_Widget_Class. in this case, it is a NO-OP
(send Mail_Message_Class :answer :EXTERNAL_EDIT '()
      '(
	(system
	 (format nil 
		 "~A ~A &"
		 (if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
		 (send self :get_filepath))
	 )
	))

(send Mail_Message_Class :answer :GET_FILEPATH '()
      '(
	(format nil "~A/~A/~A" *MH-MAILPATH* folder num)
	))

(send Mail_Message_Class :answer :GET_SUMMARY '()
      '(
	(format nil "Folder +~A, message ~A" folder num)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This returns a list of Mail_Message_Class instances corresponding
;; to the mail messages scanned from <foldername> over range <msgs>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mh-scan (foldername msgs)
  (do* 
   ((fp (popen (concatenate 'string "scan "
		       "+" foldername
		       " " msgs
		       " -noclear -noheader -reverse -width 80"
		       " -format '" *MH-FOLDER-SCAN-FORMAT* "'")
	       :direction :input))
    (msg (send (send Mail_Message_Class :new) :read-msg-info fp foldername)
	 (send (send Mail_Message_Class :new) :read-msg-info fp foldername))
    (result NIL)
    )
   ((null msg)				;:read-msg-info returns NIL on EOF
    (pclose fp)
    (cdr result)			;last msg was EOF, remove it
    )
   (setq result (cons msg result))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let*
    ((top_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "mail-br"
	    :XMN_TITLE			"WINTERP: MH-Mail Browser"
	    :XMN_ICON_NAME		"W:mail-br"
	    ))
     (paned_w
      (send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	    "paned" top_w
	    ))
     (list_w
      (send Object_Browser_Widget_Class :new :managed
	    "list" paned_w
	    :XMN_VISIBLE_ITEM_COUNT	10
	    ))
     (label_w
      (send XM_LABEL_WIDGET_CLASS :new :managed
	    "label" paned_w
	    :XMN_LABEL_STRING		"None"
	    ))
     (fileview_w 
      (send File_Viewer_Widget_Class :new :managed
	    "viewtext" paned_w
	    :XMN_ROWS			24
	    :XMN_COLUMNS		80
	    ))
     )

  ;; set list_w contents to a portion of user's inbox...
  (send list_w :set_browser_items (mh-scan "inbox" "last:30"))

  (send list_w :add_callback :XMN_DEFAULT_ACTION_CALLBACK '()
	`(
	  (let ((browsed-object (send ,list_w :get_selected_item)))
	    (cond (browsed-object	;set to NIL if no browsed object
		   (send ,label_w :set_values :XMN_LABEL_STRING
			 (send browsed-object :get_summary))

		   (send ,fileview_w :disable_redisplay)
		   (send ,fileview_w :find_file
			 (send browsed-object :get_filepath))

		   ;; if possible, skip past garbage headers in mail message,
		   ;; such that first line in viewer is an "interesting" header field.
		   (let ((search-list
			  (remove-if #'null ;removes :search failures-->NIL
				     (list
				      (progn (send ,fileview_w :set_cursor_position 0)
					     (send ,fileview_w :search "\nFrom:"))
				      (progn (send ,fileview_w :set_cursor_position 0)
					     (send ,fileview_w :search "\nTo:"))
				      (progn (send ,fileview_w :set_cursor_position 0)
					     (send ,fileview_w :search "\nSubject:"))
				      (progn (send ,fileview_w :set_cursor_position 0)
					     (send ,fileview_w :search "\nDate:"))
				      ))))
		     (cond (search-list
			    (let ((pos (1+ (apply #'min search-list))))
			      (send ,fileview_w :set_top_character pos)
			      (send ,fileview_w :set_cursor_position pos)
			      ))
			   (t
			    (send ,fileview_w :set_top_character 0)
			    (send ,fileview_w :set_cursor_position 0)
			    )
			   )
		     )
		   (send ,fileview_w :enable_redisplay)
		   ))
	    )
	  ))

  (send top_w :realize)

  ;;
  ;; set constraint resources on label widget so that paned window
  ;; doesn't give it resize sashes.
  ;;
  (let (height)
    (send label_w :get_values :xmn_height 'height)
    ;; In the code below, the kludgery
    ;; "(if *MOTIF-1.0-P* ...)"
    ;; is there to work around a name change between Motif 1.0 and 1.1:
    ;; :XMN_MAXIMUM --> :XMN_PANE_MAXIMUM and :XMN_MINIMUM -->:XMN_PANE_MINIMUM
    (send label_w :set_values
	  (if *MOTIF-1.0-P* :XMN_MAXIMUM :XMN_PANE_MAXIMUM)
	  height
	  (if *MOTIF-1.0-P* :XMN_MINIMUM :XMN_PANE_MINIMUM)
	  height
	  ))
  )
