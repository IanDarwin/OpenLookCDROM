; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         gnus-menus.el
; RCS:          $Header: $
; Description:  GNUS menu's using the WINTERP-based menu server xmu-menu.el
; Author:       Richard Hess, Consilium.
; Created:      Sat Oct  5 23:24:49 1991
; Modified:     Sat Oct  5 23:42:32 1991 (Niels Mayer) mayer@hplnpm
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

(setq *gnus_Group*
      '(("browse"   "gnus-Browse-killed-groups")
	("post"     "gnus-Group-post-news")
	("Group" (("catch up" "gnus-Group-catch-up-all")
		  ("read"     "gnus-Group-read-group")
		  ("select"   "gnus-Group-select-group")
		  ))
	("List" (("all" "gnus3d12-Group-list-all-groups")
		 ("new" "gnus3d12-Group-list-groups")
		 ))
	("Update" (("new"     "gnus-Group-get-new-news")
		   ("restart" "gnus-Group-restart")
		   ("save"    "gnus-Group-force-update")
		   ))
	nil
	("Kill File" (("global" "gnus-Group-edit-global-kill")
		      ("local"  "gnus-Group-edit-local-kill")
		      ))
	nil
	("info"     "gnus-Info-find-node")
	))

(setq *gnus_Subject*
      '(("expand"  "gnus-Subject-expand-window")
	("expunge" "gnus-Subject-delete-marked-as-read")
	("Mail" (("new"      "gnus-Subject-mail-other-window")
		 ("reply"    "gnus-Subject-mail-reply")
		 ("reply ++" "gnus-Subject-mail-reply-with-original")
		 ))
	("Post" (("new"   "gnus-Subject-post-news")
		 ("reply" "gnus-Subject-post-reply")
		 ))
	("Sort" (("author"  "gnus-Subject-sort-by-author")
		 ("date"    "gnus-Subject-sort-by-date")
		 ("number"  "gnus-Subject-sort-by-number")
		 ("subject" "gnus-Subject-sort-by-subject")
		 ))
	("Threads" (("hide" "gnus-Subject-hide-all-threads")
		    ("show" "gnus-Subject-show-all-threads")
		    ))
	nil
	("Kill File" (("global" "gnus-Subject-edit-global-kill")
		      ("local"  "gnus-Subject-edit-local-kill")
		      ))
	nil
	("info"    "gnus-Info-find-node")
	))

;; ---------------------------------------------------------------------------

(defun gnus-XmuPopup (&optional arg)
  "[ NEW ]:  Handle the GNUS pop-up menu... [ Xmu ]"
  (interactive)
  (let ((name (buffer-name))
	key
	pick
	)
    (cond ((eq name gnus-Group-buffer)   (setq key 900))
	  ((eq name gnus-Subject-buffer) (setq key 902))
	  ( t                            (setq key nil))
	  )
    (if key
	(progn
	  (setq pick (xmu_popup key t))
	  (if (not xmu-error)
	      (eval (list 'command-execute
			  (list 'quote (car (read-from-string pick)))))
	    (cond ((equal xmu-error ":NoMenu")
		   (gnus-XmuSetup))
		  (t nil))
	    ))
      (message "GNUS::XmuPopup [ no menu ]")
      )))

(defun gnus-XmuSetup ()
  "[ NEW ]:  define the GNUS menus in the WINTERP menu server... [ Xmu ]"
  (interactive)
  (xmu_menu 900 "GNUS" *gnus_Group*    "::GNUS [ Group ]")
  (xmu_menu 902 "GNUS" *gnus_Subject*  "::GNUS [ Subject ]")
  (message "GNUS::XmuSetup [ menu's created ]")
  )

(if (fboundp 'xterm-bind-key)
    (xterm-bind-key "F8"    'gnus-XmuPopup)                    ;; [ NEW ]
  )

;; ----------------------------------------------------------------------<eof>
