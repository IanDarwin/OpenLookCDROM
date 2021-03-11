; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         xmu-menu.el
; RCS:          $Header: $
; Description:  Elisp Interface to a WINTERP-based menu server...
; Author:       Richard Hess, Consilium.
;		Updated for gnuvo and other fixes by Niels Mayer.
; Created:      Sat Oct  5 23:24:49 1991
; Modified:     Sat Oct  5 23:43:11 1991 (Niels Mayer) mayer@hplnpm
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       X11r5 contrib tape release
;
; WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;
; Permission to use, copy, modify, distribute, and sell this software and its
; documentation for any purpose is hereby granted without fee, provided that
; the above copyright notice appear in all copies and that both that
; copyright notice and this permission notice appear in supporting
; documentation, and that the name of Hewlett-Packard and David Betz not be
; used in advertising or publicity pertaining to distribution of the software
; without specific, written prior permission.  Hewlett-Packard and David Betz
; make no representations about the suitability of this software for any
; purpose. It is provided "as is" without express or implied warranty.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; +---------------------------------------------------------------------------
;  WHO:    Richard Hess                    CORP:   Consilium
;  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
;      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
;  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
; +---------------------------------------------------------------------------

(provide 'xmu-menu)

(defvar xmu-output "/tmp/.xmu_output" 
  "[ NEW ]:  the output file from the WINTERP-based menu server...")

(defvar xmu-socket "/tmp/.xmu_server"
  "[ NEW ]:  the socket name used by the WINTERP-based menu server...") 

(defvar xmu-client "wl"
  "[ NEW ]:  the name of the client program that sends lisp expressions to the
WINTERP-based menu server...") 

(defvar xmu-client-args nil
  "[ NEW ]:  the default arguments sent to 'xmu-client'...")

(defvar xmu-error nil
  "[ NEW ]:  the error flag from the last menu request... [ string ]")

(defvar xmu-errors '((":NoLock" "menu server currently active")
		     (":NoMenu" "invalid menu key")
		     (":NoPick" "menu selection aborted"))
  "[ NEW ]:  the list of valid menu server errors...")


;; --------------------------------------------------------------[ xmu_popup ]

(cond
 ;; special version of xmu_popup for gnuvo (based on 18.44.2)
 ((string= emacs-version "18.44.2")	
(defun xmu_popup (key &optional xloc yloc)
  "[ NEW ]:  elisp interface to a WINTERP-based menu server..."
  (interactive)
  (message "")
  (if (not (file-exists-p xmu-socket))
      (progn (message "Xmu::NoServer  [ no active menu server ]")
	     (setq xmu-error ":NoServer")
	     (format ""))
    (let ((xbuf  (current-buffer))
	  (xargs (format
		  "(xmu_popup %s %s %s 'GNU_cbk)"
		  key
		  xloc
		  yloc))
	  (buf (find-file-noshow xmu-output))
	  pick
	  )
      (setq xmu-error nil)
      (apply 'start-process "xmu-menu" nil
	     xmu-client
	     "-f"
	     xmu-socket
	     (append xmu-client-args (list xargs)))
      (while (verify-visited-file-modtime buf) (sleep-for 1))
      (while (eq (nth 7 (file-attributes xmu-output)) 0) (sleep-for 1))
      (kill-buffer buf)
      (setq buf (find-file-noshow xmu-output))
      (set-buffer buf)
      (setq pick (substring (buffer-string) 0 -1))
      (kill-buffer buf)
      (set-buffer xbuf)
      (let* ((ecode (assoc pick xmu-errors))
	     )
	(if ecode
	    (progn (setq xmu-error pick)
		   (message (format "Xmu:%s  [ %s ]" pick (car (cdr ecode))))
		   (format ""))
	  pick))
      ))
  )
)

  ;; normal gnuemacs version of xmu_popup
  (t
(defun xmu_popup (key &optional xloc yloc)
  "[ NEW ]:  elisp interface to a WINTERP-based menu server..."
  (interactive)
  (message "")
  (if (not (file-exists-p xmu-socket))
      (progn (message "Xmu::NoServer  [ no active menu server ]")
	     (setq xmu-error ":NoServer")
	     (format ""))
    (let ((xbuf  (current-buffer))
	  (xargs (format
		  "(xmu_popup %s %s %s 'GNU_cbk)"
		  key
		  xloc
		  yloc))
	  (buf (find-file-noselect xmu-output))
	  pick
	  )
      (setq xmu-error nil)
      (apply 'start-process "xmu-menu" nil
	     xmu-client
	     "-f"
	     xmu-socket
	     (append xmu-client-args (list xargs)))
      (while (verify-visited-file-modtime buf) (sleep-for 1))
      (while (eq (nth 7 (file-attributes xmu-output)) 0) (sleep-for 1))
      (kill-buffer buf)
      (setq buf (find-file-noselect xmu-output))
      (set-buffer buf)
      (setq pick (substring (buffer-string) 0 -1))
      (kill-buffer buf)
      (set-buffer xbuf)
      (let* ((ecode (assoc pick xmu-errors))
	     )
	(if ecode
	    (progn (setq xmu-error pick)
		   (message (format "Xmu:%s  [ %s ]" pick (car (cdr ecode))))
		   (format ""))
	  pick))
      ))
  )
)
)

;; ---------------------------------------------------------------[ xmu_menu ]

(defun xmu_menu (key heading def &optional note)
  "[ NEW ]:  elisp interface to a WINTERP-based menu server..."
  (interactive)
  (message "")
  (if (not note)
      (setq note "::GNU"))
  (if (not (file-exists-p xmu-socket))
      (progn (message "Xmu::NoServer  [ no active menu server ]")
	     (setq xmu-error ":NoServer")
	     (format ""))
    (let ((xargs (format
		  "(xmu_menu %s \"%s\" (quote %s) \"%s\")"
		  key
		  heading
		  def
		  note)))
      (setq xmu-error nil)
      (apply 'start-process "xmu-menu" nil
	     xmu-client
	     "-f"
	     xmu-socket
	     (append xmu-client-args (list xargs)))
      ))
  )

;; ----------------------------------------------------------------------<eof>
