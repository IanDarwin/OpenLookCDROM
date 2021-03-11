;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         bitmap-br.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/bitmap-br.lsp,v 2.5 1994/06/06 14:41:26 npm Exp $
; Description:  Given a directory of X11 bitmaps at location
;               <bitmap_directory_path>, the function 
;                      (BROWSE-BITMAP-DIRECTORY <bitmap_directory_path>
;                                              [<ext-regexp-str>])
;               will put up a browser that will allow you to change your root
;               pixmap pattern by clicking on a bitmap image in the browser.
;		EXAMPLES: (BROWSE-BITMAP-DIRECTORY "/usr/local/include/X11/AIcons/bground/" "*.xbm")
;			  (BROWSE-BITMAP-DIRECTORY "/usr/include/X11/bitmaps/")
; Author:       Niels Mayer
; Created:      Sat Nov 25 00:53:06 1989
; Modified:     Sun Jun  5 19:27:47 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define *X11-BITMAPS-DIRECTORY*
(require "lib-utils/show-busy")		;define WINTERP-SHOW-BUSY-PROGN
(require "lib-utils/unixstuf")		;define FILE:REMOVE-PATH

(defun browse-bitmap-directory (dir &optional ext-regexp-str)

  (if (null ext-regexp-str)
      (setq ext-regexp-str "*"))

  (WINTERP-SHOW-BUSY-PROGN

   (let* (
	  (top_w (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "bmbrshl"
		       :XMN_GEOMETRY		"=360x720+0+0"
		       :XMN_TITLE		(concatenate 'string "WINTERP: Bitmap browser [" dir "]")
		       :XMN_ICON_NAME		(concatenate 'string "W:bitmap-br[" dir "]")
		       ))
	  (sc_w (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
		      "sc" top_w
		      :XMN_SCROLLING_POLICY	:automatic
		      ))
	  (rc_w (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
		      "rc" sc_w
		      :XMN_ORIENTATION		:vertical
		      :XMN_PACKING		:pack_tight
		      :XMN_ENTRY_ALIGNMENT	:alignment_center
		      ))
	  )
     (do* 
      ((fp (popen (concatenate 'string "/bin/ls -d -p " dir "/" ext-regexp-str) :direction :input))
       (name (read-line fp) (read-line fp))
       )
      ((null name)
       (pclose fp)
       (send top_w :realize)
       )

      ;; in conjuction w/ "ls -d -p", this ignores directories
      ;; since these are not files that can be passed to :XMN_LABEL_PIXMAP
      (if (string= "" (file:remove-path name)) 
	  (format T "skipped directory name=~A\n" name)	;
	(progn
	  (format T "file name=~A\n" name)
	  (send XM_LABEL_GADGET_CLASS :new :managed
		"filename" rc_w
		:XMN_LABEL_TYPE		:string
		:XMN_LABEL_STRING	name
		)
	  (send
	   (send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
		 "image" rc_w
		 :XMN_LABEL_TYPE	:pixmap
		 :XMN_LABEL_PIXMAP	name
		 )
	   :add_callback :XMN_ACTIVATE_CALLBACK '()
	   `((xsetroot ,name))
	   )
	  (send XM_SEPARATOR_GADGET_CLASS :new :managed
		"sep" rc_w
		:XMN_SEPARATOR_TYPE :DOUBLE_LINE
		)
	  ))
      )
     )
   )
  )

;; Function that gets called when a bitmap icon is activated -- sets
;; root pixmap to the selected bitmap....
(defun xsetroot (filename)
  (system (format nil "xsetroot -bitmap ~A -fg Black -bg DimGrey" filename)))

;;
;; Create toplevel window to allow user to select the appropriate bitmap directory...
;;
(let (fsb_w)
  (setq fsb_w
	(send XM_FILE_SELECTION_BOX_WIDGET_CLASS :new :unmanaged :dialog
	      "files" *toplevel_widget*
	      :XMN_DELETE_RESPONSE	:destroy
	      :XMN_AUTO_UNMANAGE	nil
	      :XMN_OK_LABEL_STRING	"Browse Dir"
	      :XMN_CANCEL_LABEL_STRING  "Close"
	      :XMN_DIALOG_TITLE		"Browse bitmap directory:"
	      :XMN_DIRECTORY		*X11-BITMAPS-DIRECTORY* ;default directory
	      ))

  (send (send fsb_w :get_child :DIALOG_SELECTION_LABEL)	:unmanage)
  (send (send fsb_w :get_child :DIALOG_TEXT)		:unmanage)
  (send (send fsb_w :get_child :DIALOG_HELP_BUTTON)	:unmanage)

  (send fsb_w :add_callback :XMN_OK_CALLBACK '()
	'(
	  (let (dir-xmstr dir-valid str)
	    (send fsb_w :get_values
		  :XMN_DIRECTORY	'dir-xmstr
		  :XMN_DIRECTORY_VALID	'dir-valid
		  )
	    (setq str (xm_string_get_l_to_r dir-xmstr))
	    (if dir-valid
		(browse-bitmap-directory
		 (subseq str 0 (1- (length str))))
	      ))
	  ))

  (send fsb_w :add_callback :XMN_CANCEL_CALLBACK '()
	'(
	  (send fsb_w :destroy)
	  ))

  (send fsb_w :manage)			;manage only after specified subwidgets managed/unmanaged
  )
