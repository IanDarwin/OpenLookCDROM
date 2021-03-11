; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         01.winterp.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/01.winterp.lsp,v 2.1 1994/06/06 14:32:51 npm Exp $
; Description:  WINTERP DEVELOPMENT SESSION STARTUP FILE.  This file is a
;		template for ~/.winterpapp -- this is loaded each time WINTERP
;		is started up without an initialization file as specified
;		by X resource Winterp.lispInitFile or command line -init_file.
;		~/.winterpapp contains the default development environment.
;		To set this up:
;		1. Copy this file to "~/.winterpapp"
;		2. Select the desired developer subsystems you wish to have
;		   loaded up each time you start WINTERP. Uncomment the
;		   associated REQUIRE statement to add the subsystem.
;		3. Add any personal customizations you want in your development
;		   environment.
;		4. Note that ~/.winterp or "lib-utils/initialize.lsp" are
;		   loaded prior to loading ~/.winterpapp; therefore this
;		   file should only include subsystems that you wish to load
;		   for WINTERP development. ~/.winterp contains defaults and
;		   initializations that get loaded both when WINTERP is used
;		   for development, and when WINTERP is used to run applications.
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

;; Note that the load-filenames used below are correct if you set WINTERP
;; resource Winterp.lispLibDir: "<path>/winterp/examples/ 
;; (where <path> is the full path to wherever you've put the WINTERP dist.)

;;;
;;; Pop up a dialog box each time output to stderr occurs. This is especially
;;; useful for GUI applications which call Unix routines -- if any of the unix
;;; routines were to generate an error, the error output would be hidden away
;;; on some console or xterm rather than notifying the user.
;;;

(require "lib-utils/redir-err")

;;;
;;; Pop up a dialog box each time an XLISP error occurs. The dialog box displays
;;; a "backtrace" (aka stack trace) of the calls which produced the error. 
;;;

(require "lib-utils/err-hook")

;;;
;;; Load the WINTERP control panel -- provides control over the XLISP debugger,
;;; alongside a file-browser and an XmText editor. The XmText editor may be
;;; used to for entering Lisp expressions into WINTERP's XLISP evaluator --
;;; the expression underneath the cursor is evaluated by using the
;;; "Eval @ Point" button. Note that the editor and Lisp evaluator portions
;;; of w_ctrlpnl.lsp need some more work to be completely useful and robust.
;;; I use gnu-emacs myself, so I'm not too motivated (right now) to spend much
;;; time fixing up w_ctrlpnl.lsp... (w_ctrlpnl uses *SYSTEM-EDITOR*, so you
;;; should set that if you want to use an external editor to edit files browsed
;;; by w_ctrlpnl...)
;;;

(require "w_ctrlpnl")
