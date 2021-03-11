;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         rc-shell.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/rc-shell.lsp,v 2.4 1994/06/06 14:43:05 npm Exp $
; Description:  Load this file to put up a default rowcolumn shell for
;               experimentation purposes. Create other widgets with rc_w
;		as parent and they'll appear in this manager.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:27:22 1989
; Modified:     Sun Jun  5 19:17:09 1994 (Niels Mayer) npm@indeed
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

(defun make-rc-shell (s_geom)
  (let*
      ((toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "rc-shell"
	      :XMN_GEOMETRY		s_geom
	      :XMN_TITLE		"WINTERP: RC-SHELL"
	      :XMN_ICON_NAME		"W:rc-shell"
	      ))
       (scrl_w
	(send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	      "sw" toplevel_w
	      :XMN_SCROLLING_POLICY	:automatic
	      ))
       (rowcol_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" scrl_w
	      :XMN_ORIENTATION		:vertical
	      :XMN_PACKING		:pack_tight
	      :XMN_ENTRY_ALIGNMENT	:alignment_center
	      )))

    (send toplevel_w :realize)
    (setq rc_w rowcol_w)
    (setq top_w toplevel_w)
    )
  )

(make-rc-shell "=300x300+0+0")
(format T "RC-SHELL Loaded:\n")
(format T "	TopLevelShell widget instance is in variable TOP_W\n")
(format T "	RowColumn widget instance is in variable RC_W\n")
