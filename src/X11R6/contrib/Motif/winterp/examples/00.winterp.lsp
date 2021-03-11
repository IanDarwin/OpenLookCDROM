; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         00.winterp.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/00.winterp.lsp,v 2.1 1994/06/06 14:28:15 npm Exp $
; Description:  WINTERP SESSION STARTUP FILE. This file is a template for
;		~/.winterp -- this is loaded each time WINTERP is started up.
;		Typically, ~/.winterp sets up the default environment for both
;		interactive WINTERP prototyping sessions as well as for sessions
;		where WINTERP is running an application (e.g. invoking
;		"winterp -init_file my-app.lsp"). If you do not have a
;		~/.winterp file, WINTERP will load "lib-utils/initialize.lsp"
;		instead, hopefully finding that file within the directory
;		specified by X Resource Winterp.lispLibDir. Therefore, you will
;		typically have a ~/.winterp file if you have personal
;		preferences or special environment settings which you want
;		different than those in "lib-utils/initialize.lsp". Note,
;		however, that you probably should have *AT* *LEAST*
;		(REQUIRE "lib-utils/initialize") within your ~/.winterp.
;
;		If you want to modify the startup environment for WINTERP
;		you should do the following:
;		(1) copy this file to ~/.winterp;
;		(2) Specify global default variables mentioned in
;		lib-utils/initialize.lsp section titled
;		"P E R S O N A L  C U S T O M I Z A T I O N S" as well as any
;		global defaults you want to customize from other applications.
;
;		You may want to load up certain "developer" files each time you
;		begin a WINTERP prototyping session (as opposed to a session
;		where WINTERP is running an application invoked via
;		"winterp -init_file my-app.lsp".) See 01.winterp.lsp for an
;		example file which loads a developer environment. You might
;		take that file, copy it to ~/.winterpapp, and modify as needed.
; Author:       Niels Mayer
; Created:      Mon Nov 20 18:13:23 1989
; Modified:     Sun Jun  5 16:31:37 1994 (Niels Mayer) npm@indeed
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

;;;
;;; initialize is required by all winterp applications
;;;

(require "lib-utils/initialize")


;;;
;;; set up editor used by "$EDITOR" button in w_ctrlpnl.lsp, dircmp.lsp,
;;; grep-br.lsp, etc. If you don't set this, then environment variable
;;; $EDITOR will be used.
;;;

;; ;; For those using Andy Norman's emacs gnuserv/gnuclient/gnudoit package...
;; (setq *SYSTEM-EDITOR* "gnuclient -q")
;;
;; ;; For those using emacs' standard emacsserver/emacsclient package...
;; (setq *SYSTEM-EDITOR* "emacsclient") 
;;
;; ;; The lowest common denominator external viewer option for Unix -- VI (puke!)
;; (setq *SYSTEM-EDITOR* "xterm -e vi") 


;;;
;;; Set the grep program used by the Motif >= 1.1 version of grep-br.lsp
;;; gnugrep is highly recommended because it is many times faster than standard grep...
;;;

;; (setq *GREP-BR-EXECUTABLE* "grep")


;;;
;;; This variable specifies the executable called by xbiff.lsp when
;;; the "Incorporate New Mail" button is selected.
;;;

;; (setq *XBIFF-INCORPORATE-NEW-MAIL-CMD* "gnudoit '(mh-rmail)'")


;;;
;;; This variable specifies the executable called periodically by xbiff.lsp
;;; to check if you have new mail. It should not zero the maildrop.
;;;

;; (setq *XBIFF-NONDESTRUCTIVELY-SCAN-MAIL-HEADERS-CMD* "scan -file /usr/mail/$LOGNAME")


;;;
;;; This variable holds the directory pathname (including trailing '/') of the
;;; X11 bitmaps directory, which is typically "/usr/include/X11/bitmaps/".
;;; This is defined in lib-utils/initialize.lsp...

;; (defvar *X11-BITMAPS-DIRECTORY*
;;   (cond
;;    ((equal (unix:get-uname) "SunOS")
;;     "/usr/openwin/include/X11/bitmaps/")
;;    (t
;;     "/usr/include/X11/bitmaps/")	;default location
;;    ))


;;;
;;; This variable holds the pathname for the X11 RGB file, which is used by
;;; xtango/imag-build.lsp (actually used by lib-utils/get-colors.lsp which
;;; is used by xtango/wcls-fgcol.lsp xtango/wcls-bgcol.lsp ...).
;;;

;; (defvar *X11-RGB-TXT-FILEPATH*
;;   (cond
;;    ((equal (unix:get-uname) "SunOS")
;;     "/usr/openwin/lib/X11/rgb.txt")
;;    (t
;;     "/usr/lib/X11/rgb.txt")		;default location
;;    ))
