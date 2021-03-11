; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         man-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/man-br.lsp,v 2.8 1994/06/06 14:43:09 npm Exp $
; Description:  Load this file to create a simple manual page browser for
;		looking at formatted manual pages installed in
;		/usr/local/X11R5/man/man3/*.3 (see *MAN-DIR-REGEXP-STR*). I use
;		this application to browse my X11/Motif manual pages, which are
;		kept in that directory.
;
;		To start up the WINTERP man-br application standalone, do
;		"env WINTERP_STANDALONE_APP=TRUE winterp -init_file man-br.lsp -no_stdin_serv -no_unix_serv -no_inet_serv -no_init_msgs &"
;		You may then quit winterp/man-br by closing the browser window....
;
;		Assumptions:
;			(1) You have 'nroff -man' formatted manual pages
;			    in *MAN-DIR-REGEXP-STR*.
;			(2) xterm(1), less(1), and man(1) programs are on
;			    your $PATH. (Note that less(1) required because
;			    more(1) pager will exit the xterm after showing the
;			    last page of output from man(1), which is bad.)
;
;		Hints:  For HPUX users -- the names of the Motif manual
;			pages displayed in the file-selection-box browser
;			will be more intuitive if you convert the manual page
;			names to long-filenames -- for details, see the
;			doc/build.doc script supplied with Motif 1.1.
; Author:       Niels Mayer
; Created:      Tue Jul  2 19:00:39 1991
; Modified:     Fri May  6 11:51:13 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define UNIX:GET-UNAME, FILE:REMOVE-EXTENSION, FILE:REMOVE-PATH, FILE:GET-EXTENSION, FILE:GET-PATH, WINTERP-STANDALONE-P, DECIDE-WINTERP-TOP-LEVEL-SHELL, and other unixisms...
(require "lib-utils/redir-err")		;pops up dialog box showing stderr output

;; IF WINTERP started w/ "env WINTERP_STANDALONE_APP=TRUE winterp -init_file man-br.lsp -no_stdin_serv -no_unix_serv"
(if (winterp-standalone-p)
    ;; THEN LOAD redir-out so that users get warned about XLISP errors occuring (e.g. from trying
    ;; browse a deleted file). Users using WINTERP interactively and loading this will probably 
    ;; not want their stdout suddenly appearing in a dialog box, so that's why we only load this
    ;; for a WINTERP application started standalone via "env WINTERP_STANDALONE_APP=TRUE ..."
    (require "lib-utils/redir-out")	;pops up dialog box showing stdout output
  )


;; Some other potential values for *MAN-DIR-REGEXP-STR*
;; HPUX: "/usr/man/man3.Z/*.3x"
;; SunOS: "/usr/local/man/man3/*.3"
;; Irix 5.1: "/usr/share/catman/p_man/cat3/Xm/*.z"
;; (defvar *MAN-DIR-REGEXP-STR* "/usr/local/X11R5/man/man3/*.3")

(defvar *MAN-DIR-REGEXP-STR*
  (cond
   ((equal (unix:get-uname) "SunOS")
    "/usr/openwin/man/man3/*.3")
   ((equal (unix:get-uname) "IRIX")
    "/usr/share/catman/p_man/cat3/Xm/*.z")
   ((equal (unix:get-uname) "HP-UX")
    "/usr/man/man3.Z/*.3x")
   ((equal (unix:get-uname) "OSF1")
    "/usr/man/man3/*.3X")
   (T
    "/usr/man/man3/*")			;default location
   ))



(defvar *MAN-PAGER* "")

(if (string= *MAN-PAGER* "")		;if variable not set by user, determine default for PAGER...
    (if (read-exec-cmd "which less")
	(setq *MAN-PAGER* "\"PAGER=less\"") ;if we have "less", use it, since it's better
      (cond
       ((equal (unix:get-uname) "IRIX")
	(setq *MAN-PAGER* "\"PAGER=more -w\"") ;default IRIX doesn't have "less" but has "more -w" which helps...
	)
       (T
	(setq *MAN-PAGER* "\"PAGER=more\"") ;use "more" by default, even though it sucks big time.
	)
       )
      )
  )

;;;
;;; resources for WinterpManBrowser and
;;; xterm displaying manual page ("xterm -name man-term")
;;;
;;; Mwm*WinterpManBrowser*iconImage: /usr/local/include/X11/bitmaps/xman-i.h
;;; Mwm*winterpManBrowser*iconImage: /usr/local/include/X11/bitmaps/xman-i.h
;;; Mwm*man-term*iconImage:          /usr/local/include/X11/bitmaps/xman-i.h
;;;
;;; !
;;; ! man-term == xterm -name man-term
;;; !
;;; ! man-term*name: xterm
;;; ! man-term*IconGeometry:
;;; ! man-term*title:
;;; ! man-term*iconName:
;;; man-term*VT100*geometry: 80x24
;;; man-term*VT100*font:     -*-*-medium-r-*--10-*-*-*-m-*-iso8859-1
;;; man-term*VT100*boldFont: -*-*-bold-r-*--10-*-*-*-m-*-iso8859-1
;;; man-term*c132: false
;;; man-term*curses: true
;;; man-term*VT100*background: LightGrey
;;; man-term*VT100*foreground: Black
;;; ! man-term*background: White
;;; ! man-term*foreground: Black
;;; man-term*VT100*cursorColor: FireBrick
;;; man-term*jumpScroll: false
;;; man-term*logging: false
;;; man-term*logInhibit: true
;;; man-term*loginShell: false
;;; man-term*marginBell: false
;;; man-term*multiScroll: false
;;; man-term*reverseVideo: false
;;; man-term*reverseWrap: false
;;; man-term*saveLines: 0
;;; man-term*scrollBar: false
;;; man-term*scrollInput: false
;;; man-term*scrollKey: false
;;; man-term*signalInhibit: false
;;; man-term*tekInhibit: true
;;; man-term*tekStartup: false
;;; man-term*visualBell: false
;;;


(defun manual-browser (manpage-dir-str)
  (let* (
	 (top_w
	  (send (decide-winterp-top-level-shell) :new
		"winterpManBrowser" "WinterpManBrowser"
		:XMN_TITLE			"WINTERP: Manual Page Browser"
		:XMN_ICON_NAME			"W:man-br"
		))
	 (fsb_w
	  (send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :unmanaged
		"man-files" top_w
		:XMN_OK_LABEL_STRING		"View Manual Page"
		:XMN_CANCEL_LABEL_STRING	"Close"
		))
	 (manpath-str
	  (getenv "MANPATH")
	  )
	 (display-str
	  (getenv "DISPLAY")
	  )
	 )

    (send (send fsb_w :get_child :DIALOG_HELP_BUTTON)   :unmanage)
    (send fsb_w :manage)		;manage only after specified subwidgets managed/unmanaged

    (send fsb_w :add_callback :XMN_OK_CALLBACK
	  '(CALLBACK_VALUE)
	  '(
	    (let (
		  (man-name 
		   (file:remove-extension (file:remove-path
					   (xm_string_get_l_to_r CALLBACK_VALUE)))
		   ))
	      (system (format NIL
			      "xterm -display ~A -name man-term -T ~A -n ~A -e env ~A ~A man ~A &"
			      display-str
			      man-name   
			      man-name
			      *MAN-PAGER*
			      (if (string= manpath-str "")
				  ""
				(format NIL "\"MANPATH=~A\"" manpath-str))
			      man-name
			      ))
	      )
	    ))
    (send fsb_w :add_callback :XMN_CANCEL_CALLBACK '()
	  '(
	    (if (winterp-standalone-p)
		(exit))
	    (send top_w :destroy)
	    ))
;;; :DO_SEARCH doesn't work on SGI IRIX's Motif (Grrr!!)
;;; (send fsb_w :do_search manpage-dir-str)
    (send fsb_w :set_values
	  :XMN_PATTERN		(concatenate 'string "*" (file:get-extension manpage-dir-str))
	  :XMN_DIRECTORY	(file:get-path manpage-dir-str)
	  )

    (send top_w :realize)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bring up an instance of the manpage browser upon loading this file.

(manual-browser *MAN-DIR-REGEXP-STR*)
