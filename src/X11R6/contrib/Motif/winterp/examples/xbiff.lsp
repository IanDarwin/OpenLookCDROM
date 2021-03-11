; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xbiff.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/xbiff.lsp,v 2.11 1994/06/06 14:43:02 npm Exp $
; Description:  Load this file to have WINTERP check for new mail periodically.
;		This periodically runs '/usr/ucb/mail -H' (or if running HP-UX,
;		or OSF1 ... 'mailx -H') to create listing of unread mail in
;		/usr/mail/$LOGNAME... I make no promises this will work on other
;		Unix machines, but it does work on HP-UX, SunOS, OSF1, Ultrix, etc.
;		If other hosts have problems you need to set variable
;		*xbiff-nondestructively-scan-mail-headers-cmd* (see below).
;		Also, you may want to modify variable *xbiff-incorporate-new-mail-cmd*
;		-- the default calls MH's 'inc' program (must be on your
;		path) and outputs the result in the xbiff window. I personally use
;		one which calls "gnudoit '(mh-rmail)'" which uses GNU Emacs' mh-e
;		package to read the new mail; WINTERP notifies GNU Emacs through
;		Andy Norman's gnudoit/gnuclient package.
;
;		To start this xbiff application "standalone", do
;		"env WINTERP_STANDALONE_APP=TRUE winterp -init_file xbiff.lsp -no_stdin_serv -no_unix_serv"
;		or see the script '../bin/win-xbiff' in this distribution. Starting
;		xbiff.lsp this way will cause WINTERP to terminate when the "Exit"
;		button is pressed; f.close, "Close" and "Incorporate New Mail" will
;		just unmap the window, but leave the WINTERP process running.
; Author:       Niels Mayer
; Created:      Wed May 27 23:02:40 1992
; Modified:     Fri May  6 11:51:09 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define winterp-standalone-p, unix:get-uname, read-exec-cmd, read-exec-cmd-multiline, etc.
(require "lib-utils/redir-err")		;pops up dialog box showing stderr output

;; IF WINTERP started w/ "env WINTERP_STANDALONE_APP=TRUE winterp -init_file ..."
(if (winterp-standalone-p)
    ;; THEN LOAD redir-out so that users get warned about XLISP errors occuring (e.g. from trying
    ;; browse a deleted file). Users using WINTERP interactively and loading this will probably 
    ;; not want their stdout suddenly appearing in a dialog box, so that's why we only load this
    ;; for a WINTERP application started standalone via "env WINTERP_STANDALONE_APP=TRUE ..."
    (require "lib-utils/redir-out")	;pops up dialog box showing stdout output
  )


;;
;; Replace the value of *xbiff-incorporate-new-mail-cmd* with a command that
;; causes your mailer to incorporate new mail. For example, if you use
;; the MH mail handler in conjunction with, GNU emacs' MH-E package and
;; Andy Norman's gnudoit/gluclient/gnuserver package (walton.maths.tcd.ie:/src2/gnu/epoch-contrib/gnuserv/*)
;; you can override the variable by setting the following in ~/.winterp:
;;	 (setq *xbiff-incorporate-new-mail-cmd* "gnudoit '(rmail)'")
;; Alternately, if you're using GNU emacs' rmail package, along with 
;; Andy Norman's gnudoit/gluclient/gnuserver add the following
;;	 (setq *xbiff-incorporate-new-mail-cmd* "gnudoit '(mh-rmail)'")
;; to your ~/.winterp.
;;
(defvar *xbiff-incorporate-new-mail-cmd*
  "echo '   You forgot to set variable *xbiff-incorporate-new-mail-cmd*'"
  )


;;
;; if you have MH, you might also try using.
;; (setq *XBIFF-NONDESTRUCTIVELY-SCAN-MAIL-HEADERS-CMD* "scan -file $MAIL")
;;
(defvar *xbiff-nondestructively-scan-mail-headers-cmd*
  (cond ((equal (unix:get-uname) "IRIX") ;SGI Irix 5.X has weird versions of ATT and BSD mailers....
	 "if test -s /usr/mail/$LOGNAME \n then zmail -headers:n \n fi") ;must kludge w/ zmail...
	((equal (unix:get-uname) "HP-UX") ;hpux uses mailx...
	 "mailx -H")
	((equal (unix:get-uname) "OSF1") ;so does OSF1 (DEC Alpha AXP)
	 "mailx -H")
	((equal (unix:get-uname) "SunOS") ;"/usr/ucb/mail -H" works for 4.1.3 and 5.3 (Solaris 2.3)
	 "/usr/ucb/mail -H")
	((equal (unix:get-uname) "ULTRIX") ;unfortunately ultrix doesn't have a real 'mail(1)' program
	 "from")
	(t
	 "from")			;default -- most systems seem to have 'from(1)'
	))

(defvar *xbiff-nomail-timeout-ms*
  60000					;if no mail, check every minute
  )

(defvar *xbiff-hasmail-timeout-ms*
  300000				;if mail, recheck every 5 mins.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XBiff functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let*
    (
     (mailcheck_to nil)			;timeout-obj
     (timeout_ms *xbiff-nomail-timeout-ms*) ;variable timeout value...
     (popup_w 
      (send XM_MESSAGE_BOX_WIDGET_CLASS :new :unmanaged :message_dialog
	    "mailcheck_dialog" *TOPLEVEL_WIDGET*
	    :XMN_MESSAGE_ALIGNMENT	:alignment_beginning
	    :XMN_OK_LABEL_STRING	"Incorporate New Mail"
	    :XMN_CANCEL_LABEL_STRING	"Close Window"
	    :XMN_DIALOG_TITLE		"New Mail Has Arrived!"
	    :XMN_AUTO_UNMANAGE		NIL
	    ))
     (exit_w
      (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	    "exit" popup_w
	    :XMN_LABEL_STRING "Exit Program"
	    ))
     )

  (send (send popup_w :get_child :DIALOG_HELP_BUTTON) :unmanage)

  (send popup_w :add_callback :XMN_OK_CALLBACK '()
	'(
	  (setq timeout_ms *xbiff-nomail-timeout-ms*) ;now that mail is incorporated, begin more frequent recheck...
	  (let ((msgstr (read-exec-cmd-multiline *xbiff-incorporate-new-mail-cmd*)))
	    (if (and (> (length msgstr) 0) ;if the command returned something...
		     (string/= msgstr "nil")) ;but it wasn't gnudoit's "nil" result
		(send popup_w :set_values :XMN_MESSAGE_STRING ;then show it in dialog
		      (format nil "'Inc New Mail' returned:\n~A" msgstr))
	      (send popup_w :unmanage)	;else remove dialog if no output from inc cmd.
	      ))
	  ))

  (send popup_w :add_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send popup_w :unmanage)
	  ))

  (send exit_w :add_callback :XMN_ACTIVATE_CALLBACK '()
	'(
	  (if mailcheck_to
	      (progv '(*breakenable*) '(nil)
		     (errset (xt_remove_timeout mailcheck_to) nil)
		     ))
	  (if (winterp-standalone-p) 
	      (exit)
	    )
	  (send popup_w :destroy)
	  ))

  (flet ((check-for-new-mail
	  ()
	  (let ((msgstr (read-exec-cmd-multiline *xbiff-nondestructively-scan-mail-headers-cmd*)))
	    (if (> (length msgstr) 0)	;if there is mail waiting
		(progn			;popup dialog w/ summary of new mail
		  (send popup_w :set_values :XMN_MESSAGE_STRING msgstr)
		  (send popup_w :manage)
		  (send (send popup_w :parent) :raise_window) ;in popup already popped-up, but obscured
		  (setq timeout_ms *xbiff-hasmail-timeout-ms*) ;if mail exists don't recheck as frequently...
		  ))
	    )
	  ))
	(setq mailcheck_to
	      (xt_add_timeout
	       timeout_ms
	       '(
		 (check-for-new-mail)
		 (setq mailcheck_to
		       (xt_add_timeout timeout_ms TIMEOUT_OBJ))
		 )
	       )
	      )

	(check-for-new-mail)		;pop up immediately if new mail right now.
	)
  )
