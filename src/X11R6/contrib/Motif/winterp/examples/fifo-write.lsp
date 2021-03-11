; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         fifo-write.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/fifo-write.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  A text widget that writes to a named pipe (~/.fifo)
;		See fifo-read.lsp for a similar program which will
;		read text sent to this named pipe.
; Author:       Niels Mayer
; Created:      Tue Feb 23 18:52:45 1993
; Modified:     Sun Jun  5 18:40:55 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/unixstuf")		;define read-exec-cmd, unix:get-uname and other unixisms...


;;
;; Make sure directory named by *SOCKETS-DIRECTORY-STR* exists...

;;
(let ((str (read-exec-cmd (concatenate 'string "ls -ld " *SOCKETS-DIRECTORY-STR*))))
  ;; if the directory named by *SOCKETS-DIRECTORY-STR* doesn't exist
  (if (or (null str)			;no such file
	  (string/= "drwx" str :start1 0 :end1 4 :start2 0 :end2 4)) ;not a directory
      ;; THEN delete file (if any), create DIRECTORY ....
      (system (format nil "/bin/rm -f ~A ; mkdir ~A"
		      *SOCKETS-DIRECTORY-STR*
		      *SOCKETS-DIRECTORY-STR*
		      ))
    ))


(let (top-w edit-w fifo-name fifo)
  (setq top-w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "top-w"
	      :XMN_TITLE	"WINTERP: FIFO Writer"
	      :XMN_ICON_NAME	"W:fifo-write"
	      ))
  (setq edit-w
	(send XM_TEXT_WIDGET_CLASS :new :managed
	      "edit-w" top-w
	      :XMN_EDIT_MODE       :SINGLE_LINE_EDIT
	      :XMN_COLUMNS         80
	      :XMN_EDITABLE	   T
	      ))
   
  (send top-w :realize)

  (setq fifo-name (concatenate 'string *SOCKETS-DIRECTORY-STR* "/.fifo"))

  (let ((str (read-exec-cmd (concatenate 'string "ls -l " fifo-name))))
    ;; if the named pipe file doesn't exist, or isn't a pipe...
    (if (or (null str)			;no such file
	    (string/= "prw" str :start1 0 :end1 3 :start2 0 :end2 3)) ;not a pipe
	;; THEN delete file, create FIFO....
	(cond
	 ((and (equal (unix:get-uname) "SunOS") ;for this case, we're only interested in SunOS 4.X; Handle 5.X==Solaris below
	       (<= (unix:get-uname-rev) 4))
	  (system (format nil "/bin/rm -f ~A ; /usr/etc/mknod ~A p"
			  fifo-name
			  fifo-name
			  ))
	  )
	 (t				;default assumes system is POSIX.2 compliant thus has mkfifo(1M) command
	  ;; originally, I had "mkfifo -m0777 ~A", but it turned out that
	  ;; -m0777 argument only valid on HPUX, OSF1. Solaris 2.3, Irix 5.2
	  ;; don't support that flag, so I removed it.
	  (system (format nil "/bin/rm -f ~A ; mkfifo ~A"
			  fifo-name
			  fifo-name
			  ))
	  )
	 )
      ))
  
  ;; get handle on FIFO
  (setq fifo (open fifo-name :direction :output
		   :if-exists :overwrite))

  (send edit-w :set_callback :XMN_ACTIVATE_CALLBACK '(CALLBACK_WIDGET)
	'(
	  (format fifo "~A\n" (send CALLBACK_WIDGET :get_string))
	  (fflush fifo)
	  ))

  (send top-w :add_callback :XMN_DESTROY_CALLBACK '()
	'(
	  (format T "quitting named pipe writer\n")
	  (close fifo)
	  ))
  )
