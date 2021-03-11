;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         bitmap-br2.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/bitmap-br2.lsp,v 2.4 1994/06/06 14:43:21 npm Exp $
; Description:  Similar to bitmap-br.lsp, except that simply loading this
		file will bring up a browser of the bitmaps in directory
		/usr/include/X11/bitmaps/*. Unlike bitmap-br.lsp, this file
		contains comments on what is happening in this simple application..
; Author:       Niels Mayer
; Created:      Wed Mar 14 21:13:36 1990
; Modified:     Sun Jun  5 18:18:00 1994 (Niels Mayer) npm@indeed
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; make a trivial subclass of XM_PUSH_BUTTON_GADGET_CLASS
;;;
(setq Niels_Pixmap_Push_Button_Class    ;name of the new subclass
      (send Class :new
            '(pixmap_file)              ;a new ivar for this subclass
            '()                         ;no class variables for subclass
            XM_PUSH_BUTTON_GADGET_CLASS ;name of the superclass
      )) 
;;;
;;; override XM_TOGGLE_BUTTON_GADGET_CLASS's instance initializer (method
;;; :isnew) such that the instance variable pixmap_file is initialized
;;; and such that the created pushbutton widget displays a pixmap.
;;;
(send Niels_Pixmap_Push_Button_Class :answer :ISNEW
      '(managed_k filename widget_name widget_parent &rest args)
      '(
        (setq pixmap_file filename)
        (apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       :XMN_LABEL_TYPE	 :pixmap
	       :XMN_LABEL_PIXMAP filename
	       args
	       )
        ))
;;;
;;; add a method responding to message :xsetroot that calls the
;;; xsetroot(1) program to set background tile. the 'system' call
;;; is the unix system(3s) call, and the 'format' call is equivalent
;;; to the unix sprintf(3s) call.
;;;
(send Niels_Pixmap_Push_Button_Class :answer :XSETROOT '()
      '(
        (system (format nil "xsetroot -bitmap ~A -fg Black -bg DimGrey" 
                        pixmap_file))
        ))


(let (toplevel_w scrl_w rowcol_w)

  ;;
  ;; create a toplevel widget that talks to the window manager.
  ;;
  (setq toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "bbr2"
	      :XMN_TITLE	"WINTERP: Bitmap Browser Example"
	      :XMN_ICON_NAME	"W:bitmap-br2"
	      ))
  ;;
  ;; inside the toplevel_w create a scrolled window widget to allow viewing
  ;; of a window larger than the toplevel window by panning around with
  ;; scrollbars.
  ;;
  (setq scrl_w
	(send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
	      "sc" toplevel_w
	      :XMN_SCROLLING_POLICY :automatic
	      ))
  ;;
  ;; Inside the scrl_w, create a "manager" widget that lays out the entries
  ;; in the bitmap browser (children of scrl_w) in a vertical fashion.
  ;;
  (setq rowcol_w
	(send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	      "rc" scrl_w
	      :XMN_ORIENTATION     :vertical
	      :XMN_PACKING         :pack_tight
	      :XMN_ENTRY_ALIGNMENT :alignment_center
	      ))
  ;;
  ;; Add a callback that sends the message :xsetroot to the child widget
  ;; of the rowcolumn widget that was activated via mouse click.
  ;;
  (send rowcol_w :set_callback :XMN_ENTRY_CALLBACK
	'(CALLBACK_ENTRY_WIDGET)	;local variable bound to the
					;callback-causing widget
	'(
	  (send CALLBACK_ENTRY_WIDGET :xsetroot)
	  ))

  ;; This loop creates a label,bitmap-pushbutton,separator triple for each
  ;; bitmap file read from the directory specified in the arg to popen(3s).
  ;;
  ;; Obvisouly, this loop should become a procedure with the name of the
  ;; bitmap directory passed in as parameter. However, since this is example
  ;; code to be read by people not conversant in WINTERP-Lisp, I am going to
  ;; hold off on introducing proceduralization...
  (do* 
   (;; local do-loop variables with initialize and increment expressions.

    ;; Get a list of the bitmap files in matching the pattern
    ;; /usr/local/mayer/src/bitmaps/*.xbm
    ;; We use the unix popen(3s) routine to read the results of ls(1), which returns
    ;; to stdout a list of matching filenames in the shell created by popen(3s). 
    ;; popen(3s) returns a FILE* that can be read by the XLISP primitive 'read-line'
    (ls_reader_pipe
     (popen (concatenate 'string "/bin/ls " *X11-BITMAPS-DIRECTORY* "*")
	    :direction :input))

    (file-name
     (read-line ls_reader_pipe) (read-line ls_reader_pipe))
    )

   (;; do-loop termination condition and termination code
    (null file-name)			;terminate when (read-line) ==> EOF
    (pclose ls_reader_pipe)		;close the pipe
    (send toplevel_w :realize)		;create the toplevel window and exit
    )

   ;; loop body
   (send XM_LABEL_GADGET_CLASS :new :managed
	 "filename" rowcol_w
	 :XMN_LABEL_TYPE	:string
	 :XMN_LABEL_STRING	file-name
	 )
   (send Niels_Pixmap_Push_Button_Class :new :managed
	 file-name "image" rowcol_w
	 )
   (send XM_SEPARATOR_GADGET_CLASS :new :managed
	 "sep" rowcol_w
	 :XMN_SEPARATOR_TYPE	:double_line
	 )
   )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of bitmap browser example -- note that in WINTERP you don't need to
;; mess with initializing the display nor calling XtMainLoop().
