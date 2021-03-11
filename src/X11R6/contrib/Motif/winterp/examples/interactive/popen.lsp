;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         popen.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/popen.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Play around with  POPEN to collect unix data.
; Author:       Niels Mayer
; Created:      Sat Nov 25 01:51:55 1989
; Modified:     Mon Jun  6 00:28:45 1994 (Niels Mayer) npm@indeed
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


(defun ls (dirname)
  (do* 
   ((fp (popen (concatenate 'string "/bin/ls -1 -r " dirname) :direction :input))
    (line (read-line fp) (read-line fp))
    (result (list line) (cons line result))
    )
   ((null line)
    (pclose fp)
    (cdr result)
    )
   )
  )

(defun vls (dirname)
  (do* 
   ((top_w (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
		 :XMN_GEOMETRY "500x500+0+0"
		 ))
    (rc_w (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed top_w
		:XMN_ADJUST_LAST nil
		))
    (fp (popen (concatenate 'string "/bin/ls -1 -r " dirname) :direction :input))
    (line (read-line fp) (read-line fp))
    )
   ((null line)
    (pclose fp)
    (send top_w :realize)
    )
   (send (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed rc_w
	       :XMN_LABEL_STRING line)
	 :add_callback :XMN_ACTIVATE_CALLBACK '()
	 `((system (concatenate 'string "$EDITOR " ,dirname "/" ,line))))
   )
  )

(defun mh-scan (foldername msgs)
  (do* 
   ((fp (popen (concatenate 'string "scan -reverse +" foldername " " msgs) 
	       :direction :input))
    (line (read-line fp) (read-line fp))
    (result (list line) (cons line result))
    )
   ((null line)
    (pclose fp)
    (cdr result)
    )
   )
  )

(mh-scan "inbox" "all")
(vls "/usr")
