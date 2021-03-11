; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         dircmp.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/dircmp.lsp,v 2.13 1994/06/06 14:43:19 npm Exp $
; Description:  A browser allowing the comparison of directories... a
;               motif'd version of the SYSV-Unix 'dircmp' program...
;		To run standalone, do "env WINTERP_STANDALONE_APP=TRUE winterp -init_file dircmp.lsp -no_stdin_serv -no_unix_serv"
;
;		To use this app, browse the desired directories in the file
;		selection widgets, then click the vertical "Compare Dirs" button.
;		Then use up and down arrow keys, or the following mouse/key
;		bindings to browse the differences between the directories.
;		   Mouse bindings on browsers:
;			* single left click	-- select item for use by
;						   $EDITOR button or other op...
;			* double left click	-- select item and browse it in
;						   associated viewer widget.
;			* single middle click	-- select item and browse it in
;						   associated viewer widget.
;			* single right click	-- select item and display
;						   corresponding text in $EDITOR.
;		   Key bindings on browsers::
;			* E			-- view selected item in user's
;						   editor ($EDITOR).
;			* ^E			-- select next item and view in
;						   user's editor ($EDITOR).
;			* ^N, ^<DownArrow>	-- select next item.
;			* ^P, ^<UpArrow>	-- select prev item.
;			* N , <DownArrow>	-- browse next item in viewer.
;			* P , <UpArrow>		-- browse prev item in viewer.
; Author:       Niels Mayer
; Created:      Sun Dec 29 19:32:28 1991
; Modified:     Sun Jun  5 18:35:44 1994 (Niels Mayer) npm@indeed
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

;; Resources:
;;
;; Mwm*winterpDircmpBr*iconImage:    /usr/local/include/X11/bitmaps/search-i.h
;; Mwm*WinterpDircmpBr*iconImage:    /usr/local/include/X11/bitmaps/search-i.h
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *SYSTEM-EDITOR*:
;;; if NIL, then edit functionality will use editor set in environment variable 
;;; $EDITOR. If set to a string, then that string will be used as the name of
;;; the editor to use for the "Edit" button.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *SYSTEM-EDITOR* nil)

(require "lib-utils/unixstuf")		;define winterp-standalone-p decide-winterp-top-level-shell and other unixisms...
(require "lib-utils/show-busy")		;define WINTERP-SHOW-BUSY-PROGN
(require "lib-utils/redir-err")		;pops up dialog box showing stderr output
(require "lib-widgets/fileselect")	;define WINTERP:FILE-SELECTION-WIDGET, :set-file-selected-callback-closure, :set-dir-selected-callback-closure, :get-filepath-str
(require "lib-widgets/file-br")		;define File_Browser_Widget_Class
(require "lib-widgets/object-br")	;define Object_Browser_Widget_Class
(require "lib-widgets/file-lview")	;define Labelled_File_Viewer_Widget_Class

;; IF WINTERP started w/ "env WINTERP_STANDALONE_APP=TRUE winterp -init_file dircmp.lsp -no_stdin_serv -no_unix_serv"
(if (winterp-standalone-p)
    ;; THEN LOAD redir-out so that users get warned about XLISP errors occuring (e.g. from trying
    ;; browse a deleted file). Users using WINTERP interactively and loading this will probably 
    ;; not want their stdout suddenly appearing in a dialog box, so that's why we only load this
    ;; for a WINTERP application started standalone via "env WINTERP_STANDALONE_APP=TRUE ..."
    (require "lib-utils/redir-out")	;pops up dialog box showing stdout output
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIRCMP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; # dircmp <dir1> <dir2>
; 
; pwd == <dir0>
; 
; % generate list of files in <dir1>
; cd <dir1>
; find . -print | sort > /tmp/dc1.files
; 
; % probably prevents weirdness w/ relative paths/symlinks...
; cd <dir0>
; 
; % generate list of files in <dir2>
; cd <dir2>
; find . -print | sort > /tmp/dc2.files
; 
; % find differences in files...
; comm /tmp/dc1.files /tmp/dc2.files | sed -n \
; 	-e "/^		/w /tmp/dc.common-files" \
; 	-e "/^	[^	]/w /tmp/dc.2-only-files" \
; 	-e "/^[^	]/w /tmp/dc.1-only-files"
; rm /tmp/dc1.files /tmp/dc2.files
; 
; % now put up <1>-only files in one browser, <2>-only files in another
; rm /tmp/dc.1-only-files /tmp/dc.2-only-files
; 
; % filter junk out of dc.common-files
; sed -e s/..// < /tmp/dc.common-files > /tmp/dc.common-files.clean
; rm /tmp/dc.common-files
; 
; % foreach i in common files
; if directory(i) then skip
; if exitstatus(cmp -s <dir1>/i <dir2>/i) != 0 then add i to list of difft files.
; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function compares two directories <d1> <d2>, strings representing the
;; relative or full path to the directories to be compared. It is called by
;; function dircmp below...
;;
;; The function returns a STRING represeting the process ID <pid> of the shell;
;; this is used to ensure unique naming of the temp files that are left around
;; as a result of calling this function.
;; /tmp/dc<pid>h == list of differing files
;; /tmp/dc<pid>e == list of files only in <d1>
;; /tmp/dc<pid>g == list of files only in <d2>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dircmp-aux (d1 d2)
  (let*
      ((fp
	(popen (concatenate 'string
		"D0=`pwd`\n"
		"cd " d1 "\n"
		"find . -print | sort > /tmp/dc$$a\n"
		"cd $D0\n"
		"cd " d2 "\n"
		"find . -print | sort > /tmp/dc$$b\n"
		"cd $D0\n"
		"comm /tmp/dc$$a /tmp/dc$$b | sed -n -e \"/^		/w /tmp/dc$$c\"	-e \"/^	[^	]/w /tmp/dc$$d\" -e \"/^[^	]/w /tmp/dc$$e\"\n"
		"rm -f /tmp/dc$$a /tmp/dc$$b\n"
		"sed -e s/..// < /tmp/dc$$c > /tmp/dc$$f\n"
		"rm -f /tmp/dc$$c\n"
		"sed -e s/.// < /tmp/dc$$d > /tmp/dc$$g\n"
		"rm -f /tmp/dc$$d\n"
		"while read a\n"
		"do\n"
		"if [ -f " d1 "/\"$a\" ]\n"
		"then cmp -s " d1 "/\"$a\" " d2 "/\"$a\"\n"
		"     if [ $? != 0 ]\n"
		"     then echo \"$a\"\n"
		"     fi\n"
		"fi\n"
		"done < /tmp/dc$$f > /tmp/dc$$h\n"
		"rm -f /tmp/dc$$f\n"
		"echo $$\n"
		) :direction :input))
       (pid
	(read-line fp))
       )
    (pclose fp)
    pid					;RETURN string representing process ID
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function compares two directories <d1> <d2>, strings representing the
;; relative or full path to the directories to be compared.
;;
;; The function returns a three element array:
;; * the first element is a list of strings representing the files whose contents
;;   are different between <d1> and <d2>.
;; * the second element is a list of strings representing the files only in <d1>.
;; * the third element is a list of strings representing the files only in <d2>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dircmp (d1 d2)
  (let*
      ((pid-str
	(dircmp-aux d1 d2))
       (difft-files-file
	(concatenate 'string "/tmp/dc" pid-str "h"))
       (only-d1-files-file
	(concatenate 'string "/tmp/dc" pid-str "e"))
       (only-d2-files-file
	(concatenate 'string "/tmp/dc" pid-str "g"))
       (result
	(make-array 3))
       )
    
    (setf (aref result 0)
	  (do* 
	   ((fp (open difft-files-file :direction :input))
	    (line (read-line fp) (read-line fp))
	    (result nil)
	    )
	   ((null line)
	    (close fp)
	    (reverse result)
	    )
	   (setq result (cons line result))
	   ))
    (setf (aref result 1)
	  (do* 
	   ((fp (open only-d1-files-file :direction :input))
	    (line (read-line fp) (read-line fp))
	    (result nil)
	    )
	   ((null line)
	    (close fp)
	    (reverse result)
	    )
	   (setq result (cons line result))
	   ))

    (setf (aref result 2)
	  (do* 
	   ((fp (open only-d2-files-file :direction :input))
	    (line (read-line fp) (read-line fp))
	    (result nil)
	    )
	   ((null line)
	    (close fp)
	    (reverse result)
	    )
	   (setq result (cons line result))
	   ))

    (system (concatenate 'string "rm -r "
		    difft-files-file " "
		    only-d1-files-file " "
		    only-d2-files-file))

    result
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DIFF   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar DIFF_ITEM_CLASS
  (send Class :new
	'(
	  dir-0-name
	  dir-1-name
	  file-name
	  file-0-line
	  file-1-line
	  diff-char
	  )))

(send DIFF_ITEM_CLASS :answer :isnew
      '(d0-name d1-name file f0-num f1-num ch)
      '(
	(setq dir-0-name d0-name)
	(setq dir-1-name d1-name)
	(setq file-name  file)
	(setq file-0-line f0-num)
	(setq file-1-line f1-num)
	(setq diff-char   ch)
	self
	))

(send DIFF_ITEM_CLASS :answer :DISPLAY_STRING '()
      '(
	(format nil "~A ~A ~A"
		file-0-line diff-char file-1-line)
	))

(send DIFF_ITEM_CLASS :answer :EXTERNAL_EDIT '()
      '(
	(system (format nil 
			"~A +~A ~A~A +~A ~A~A &"
			(if *SYSTEM-EDITOR* *SYSTEM-EDITOR* "$EDITOR")
			file-0-line dir-0-name file-name
			file-1-line dir-1-name file-name
			))
	))

(send DIFF_ITEM_CLASS :answer :get_file_name '()
      '(
	file-name
	))

(send DIFF_ITEM_CLASS :answer :get_dir_0_name '()
      '(
	dir-0-name
	))

(send DIFF_ITEM_CLASS :answer :get_dir_1_name '()
      '(
	dir-1-name
	))

(send DIFF_ITEM_CLASS :answer :get_file_0_linenum '()
      '(
	file-0-line
	))

(send DIFF_ITEM_CLASS :answer :get_file_1_linenum '()
      '(
	file-1-line
	))

;; (diff "/etc/passwd.~1~" "/etc/passwd")
;; (diff "/usr/local/mayer/src/widgit/src-server/wc_Table.c" "/usr/local/mayer/src/widgit/src-server/wc_Table.c.~1~")
(defun diff (dir-0 dir-1 file)
  (do (
       (fp (popen (concatenate 'string "diff "
			       dir-0 "/" file " "
			       dir-1 "/" file
			       ) :direction :input))
       (temp "<")
       (result nil)
       )
      ((or (null temp)			;EOF
	   (let ((c (char temp 0)))
	     (if (or (char= c #\<) (char= c #\>) (char= c #\-))
		 nil			;it read the right thing, let loop continue
	       (error "read unexpected character (not < > or -) in diff" c)
	       )))    
       (reverse result)			;RETURN RESULT
       )
      (do (
	   (ln0-start	(fscanf-fixnum fp "%d")	;read begining of token nnn,NNN*nnn,NNN where ,NNN is optional
			(fscanf-fixnum fp "%d"))
	   (ln0-end	nil	nil)
	   (diff-char	nil	nil)
	   (ln1-start	nil	nil)
	   (ln1-end	nil	nil)
	   tmp
	   )
	  ( (not (integerp ln0-start))	;exit loop if at EOF or reading sections text beginning with '<' '>' or '-'
	    ;; DO termination appends instance(s) to DIFF_ITEM_CLASS
	    )

	  (setq tmp (read-char fp))	;read over , or c or d or a

	  (when (char= tmp #\,)		;if it's a "," then we have ,NNN
		(setq ln0-end (fscanf-fixnum fp "%d")) ;so read over NNN
		(if (not (integerp ln0-end))
		    (error "expected an integer for ln0-end." ln0-end))
		(setq tmp (read-char fp))) ;read c or d or a
	  (cond
	   ((or (char= tmp #\c)		;or read over c|d|a
		(char= tmp #\d)
		(char= tmp #\a))
	    (setq ln1-start (fscanf-fixnum fp "%d")) ;read over next nnn
	    (if (not (integerp ln1-start))
		(error "expected an integer for ln1-start." ln1-start))
	    )
	   (t
	    (error "unexpected 'diff' character" tmp))
	   )
	  (setq diff-char tmp)
	  (setq tmp (read-char fp))	;read over separator
	  (cond
	   ((char= tmp #\, )
	    (setq ln1-end (fscanf-fixnum fp "%d")) ;so read over ,NNN
	    (if (not (integerp ln1-end))
		(error "expected an integer for ln1-end" ln1-end)))
	   ((char/= tmp #\newline)
	    (error "unexpected junk instead of end-of-line" tmp))
	   )

	  (cond 
	   ((and ln0-start ln0-end ln1-start ln1-end)
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-start ln1-start diff-char)
			       result))
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-end   ln1-end diff-char)
			       result))
	    )
	   ((and ln0-start ln1-start ln1-end (null ln0-end))
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-start ln1-start diff-char)
			       result))
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-start ln1-end   diff-char)
			       result))
	    )
	   ((and ln0-start ln1-start ln0-end (null ln1-end))
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-start ln1-start diff-char)
			       result))
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-end ln1-start diff-char)
			       result))
	    )
	   ((and ln0-start ln1-start (null ln0-end) (null ln1-end))
	    (setq result (cons (send DIFF_ITEM_CLASS :new
				     dir-0 dir-1 file ln0-start ln1-start diff-char)
			       result))
	    )
	   (t
	    ))
	  )				;end DO
      (setq temp (read-line fp))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIRCMP-BROWSER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dircmp-browser()
  (let*
      (
       (top_w
	(send (decide-winterp-top-level-shell) :new
	      "winterpDircmpBr" "WinterpDircmpBr"
	      :XMN_TITLE		"WINTERP Directory Comparison Browser"
	      :XMN_ICON_NAME		"W:dircmp"
	      ))
       (paned_w
	(send XM_PANED_WINDOW_WIDGET_CLASS :new :managed
	      "paned_w" top_w 
	      ))
       (fsb_form_w
	(send XM_FORM_WIDGET_CLASS :new :managed "fsb-form"
	      paned_w
	      :XMN_ALLOW_RESIZE			t ;paned_w constraint resource
	      :XMN_SKIP_ADJUST			nil ;paned_w constraint resource
	      ))
       (fsb1_frame_w
	(send XM_FRAME_WIDGET_CLASS :new :managed
	      "fsb1_frame" fsb_form_w
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		49
	      :XMN_RESIZABLE			nil
	      ))
       (fsb2_frame_w
	(send XM_FRAME_WIDGET_CLASS :new :managed
	      "fsb2_frame" fsb_form_w
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_position
	      :XMN_LEFT_POSITION		51
	      :XMN_RESIZABLE			nil
	      ))
       (do_dircmp_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "do_dircmp_button" fsb_form_w
	      :XMN_LABEL_STRING			"\nC\no\nm\np\na\nr\ne\n\nD\ni\nr\ns"
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			fsb1_frame_w
	      :XMN_RIGHT_ATTACHMENT		:attach_widget
	      :XMN_RIGHT_WIDGET			fsb2_frame_w
	      :XMN_RESIZABLE			nil
	      ))
       (fsb1_w
	(send WINTERP:FILE-SELECTION-WIDGET :new :managed
	      "fsb1" fsb1_frame_w
	      :XMN_DIR_MASK			"*"
	      :XMN_DIR_LIST_LABEL_STRING	"1:Directories"
	      :XMN_FILE_LIST_LABEL_STRING	"1:Files"
	      :XMN_FILTER_LABEL_STRING		"1:Filter"
	      :XMN_SELECTION_LABEL_STRING	"1:Selection"
	      :XMN_MARGIN_HEIGHT		2 ;should be in app-defaults
	      :XMN_MARGIN_WIDTH			2 ;should be in app-defaults
	      :XMN_LIST_VISIBLE_ITEM_COUNT	4 ;should be in app-defaults
	      ))
       (fsb2_w
	(send WINTERP:FILE-SELECTION-WIDGET :new :managed
	      "fsb2" fsb2_frame_w
	      :XMN_DIR_MASK			"*"
	      :XMN_DIR_LIST_LABEL_STRING	"2:Directories"
	      :XMN_FILE_LIST_LABEL_STRING	"2:Files"
	      :XMN_FILTER_LABEL_STRING		"2:Filter"
	      :XMN_SELECTION_LABEL_STRING	"2:Selection"
	      :XMN_MARGIN_HEIGHT		2 ;should be in app-defaults
	      :XMN_MARGIN_WIDTH			2 ;should be in app-defaults
	      :XMN_LIST_VISIBLE_ITEM_COUNT	4 ;should be in app-defaults
	      ))
       (dircmp_table_w
	(send XM_FORM_WIDGET_CLASS :new :managed
	      "dircmp_table" paned_w
	      :XMN_ALLOW_RESIZE			t ;paned_w constraint resource
	      :XMN_SKIP_ADJUST			nil ;paned_w constraint resource
	      ))
       (only_d1_files_form_w
	(send XM_FORM_WIDGET_CLASS :new :managed "only_d1_files_form"
	      (send XM_FRAME_WIDGET_CLASS :new :managed
		    "only_d1_files_frame" dircmp_table_w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_position
		    :XMN_RIGHT_POSITION		25
		    :XMN_RESIZABLE		nil
		    )
	      ))
       (only_d1_files_label_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "only_d1_files_label" only_d1_files_form_w
	      :XMN_LABEL_STRING			"Dir-1 Only:"
	      :XMN_ALIGNMENT			:alignment_beginning
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		50
	      :XMN_RESIZABLE			nil
	      ))
       (only_d1_files_list_w
	(send File_Browser_Widget_Class :new :managed
	      "only_d1_files_list" only_d1_files_form_w
	      :XMN_VISIBLE_ITEM_COUNT		3 ;SHOULD BE IN app-defaults (or equiv.)
	      :XMN_TOP_ATTACHMENT		:attach_widget
	      :XMN_TOP_WIDGET			only_d1_files_label_w
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_RESIZABLE			nil
	      ))
       (only_d1_files_prev_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "only_d1_files_prev_button" only_d1_files_form_w
	      :XMN_ARROW_DIRECTION		:arrow_up
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			only_d1_files_label_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET		only_d1_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (only_d1_files_next_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "only_d1_files_next_button" only_d1_files_form_w
	      :XMN_ARROW_DIRECTION		:arrow_down
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			only_d1_files_prev_button_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET		only_d1_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (only_d1_files_edit_file_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "only_d1_files_edit_file_button" only_d1_files_form_w
	      :XMN_LABEL_STRING			"$EDITOR"
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			only_d1_files_next_button_w
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET		only_d1_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (diff_files_form_w
	(send XM_FORM_WIDGET_CLASS :new :managed "diff_files_form"
	      (send XM_FRAME_WIDGET_CLASS :new :managed
		    "common_files_frame" dircmp_table_w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_position
		    :XMN_LEFT_POSITION		25
		    :XMN_RIGHT_ATTACHMENT	:attach_position
		    :XMN_RIGHT_POSITION		75
		    :XMN_RESIZABLE		nil
		    )
	      ))
       (common_files_label_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "common_files_label" diff_files_form_w
	      :XMN_LABEL_STRING			"Different Files:"
	      :XMN_ALIGNMENT			:alignment_beginning
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		25
	      :XMN_RESIZABLE			nil
	      ))
       (common_files_list_w
	(send File_Browser_Widget_Class :new :managed
	      "common_files_list" diff_files_form_w
	      :XMN_VISIBLE_ITEM_COUNT		3 ;SHOULD BE IN app-defaults (or equiv.)
	      :XMN_TOP_ATTACHMENT		:attach_widget
	      :XMN_TOP_WIDGET		        common_files_label_w
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		49
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_RESIZABLE			nil
	      ))
       (common_files_prev_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "common_files_prev_button" diff_files_form_w 
	      :XMN_ARROW_DIRECTION		:arrow_up
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			common_files_label_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET	        common_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (common_files_next_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "common_files_next_button" diff_files_form_w 
	      :XMN_ARROW_DIRECTION		:arrow_down
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			common_files_prev_button_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET	        common_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (common_files_edit_file_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "common_files_edit_file_button" diff_files_form_w 
	      :XMN_LABEL_STRING			"$EDITOR"
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			common_files_next_button_w
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		49
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET	        common_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (diffs_label_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "diffs_label" diff_files_form_w
	      :XMN_LABEL_STRING			"Differences:"
	      :XMN_ALIGNMENT			:alignment_beginning
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_position
	      :XMN_LEFT_POSITION		51
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		75
	      :XMN_RESIZABLE			nil
	      ))
       (diffs_list_w
	(send Object_Browser_Widget_Class :new :managed
	      "common_files_list" diff_files_form_w
	      :XMN_VISIBLE_ITEM_COUNT		3 ;SHOULD BE IN app-defaults (or equiv.)
	      :XMN_TOP_ATTACHMENT		:attach_widget
	      :XMN_TOP_WIDGET		        diffs_label_w
	      :XMN_LEFT_ATTACHMENT		:attach_position
	      :XMN_LEFT_POSITION		51
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_RESIZABLE			nil
	      ))
       (diffs_prev_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "diffs_prev_button" diff_files_form_w 
	      :XMN_ARROW_DIRECTION		:arrow_up
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET	        	diffs_label_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET	        diffs_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (diffs_next_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "diffs_next_button" diff_files_form_w 
	      :XMN_ARROW_DIRECTION		:arrow_down
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET	        	diffs_prev_button_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET	        diffs_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (diffs_edit_file_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "diffs_edit_file_button" diff_files_form_w 
	      :XMN_LABEL_STRING			"$EDITOR"
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET	        	diffs_next_button_w
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET	        diffs_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (only_d2_files_form_w
	(send XM_FORM_WIDGET_CLASS :new :managed "only_d2_files_form"
	      (send XM_FRAME_WIDGET_CLASS :new :managed
		    "only_d2_files_frame" dircmp_table_w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_position
		    :XMN_LEFT_POSITION		75
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    :XMN_RESIZABLE		nil
		    )
	      ))
       (only_d2_files_label_w
	(send XM_LABEL_WIDGET_CLASS :new :managed
	      "only_d2_files_label" only_d2_files_form_w
	      :XMN_LABEL_STRING			"Dir-2 Only:"
	      :XMN_ALIGNMENT			:alignment_beginning
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_position
	      :XMN_RIGHT_POSITION		50
	      :XMN_RESIZABLE			nil
	      ))
       (only_d2_files_list_w
	(send File_Browser_Widget_Class :new :managed
	      "only_d2_files_list" only_d2_files_form_w
	      :XMN_VISIBLE_ITEM_COUNT		3 ;SHOULD BE IN app-defaults (or equiv.)
	      :XMN_TOP_ATTACHMENT		:attach_widget
	      :XMN_TOP_WIDGET			only_d2_files_label_w
	      :XMN_LEFT_ATTACHMENT		:attach_form
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_form
	      :XMN_RESIZABLE			nil
	      ))
       (only_d2_files_prev_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "only_d2_files_prev_button" only_d2_files_form_w 
	      :XMN_ARROW_DIRECTION		:arrow_up
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			only_d2_files_label_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET		only_d2_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (only_d2_files_next_button_w
	(send XM_ARROW_BUTTON_GADGET_CLASS :new :managed
	      "only_d2_files_next_button" only_d2_files_form_w 
	      :XMN_ARROW_DIRECTION		:arrow_down
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			only_d2_files_prev_button_w
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET		only_d2_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (only_d2_files_edit_file_button_w
	(send XM_PUSH_BUTTON_GADGET_CLASS :new :managed
	      "only_d2_files_edit_file_button" only_d2_files_form_w 
	      :XMN_LABEL_STRING			"$EDITOR"
	      :XMN_TOP_ATTACHMENT		:attach_form
	      :XMN_LEFT_ATTACHMENT		:attach_widget
	      :XMN_LEFT_WIDGET			only_d2_files_next_button_w
	      :XMN_RIGHT_ATTACHMENT		:attach_form
	      :XMN_BOTTOM_ATTACHMENT		:attach_widget
	      :XMN_BOTTOM_WIDGET		only_d2_files_list_w
	      :XMN_RESIZABLE			nil
	      ))
       (editor_form_w
	(send XM_FORM_WIDGET_CLASS :new :managed
	      "editor_form" paned_w
	      :XMN_ALLOW_RESIZE			t ;paned_w constraint resource
	      :XMN_SKIP_ADJUST			nil ;paned_w constraint resource
	      :XMN_HEIGHT			200 ;should be in app-defaults
	      ))
       (d1_editor_w
	(send Labelled_File_Viewer_Widget_Class :new :managed "d1_editor"
	      (send XM_FRAME_WIDGET_CLASS :new :managed
		    "d1_editor_frame" editor_form_w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_position
		    :XMN_RIGHT_POSITION		50
		    :XMN_RESIZABLE		nil
		    )
	      ))
       (d2_editor_w
	(send Labelled_File_Viewer_Widget_Class :new :managed "d2_editor"
	      (send XM_FRAME_WIDGET_CLASS :new :managed
		    "d2_editor_frame" editor_form_w
		    :XMN_TOP_ATTACHMENT		:attach_form
		    :XMN_BOTTOM_ATTACHMENT	:attach_form
		    :XMN_RIGHT_ATTACHMENT	:attach_form
		    :XMN_LEFT_ATTACHMENT	:attach_position
		    :XMN_LEFT_POSITION		50
		    :XMN_RESIZABLE		nil
		    )
	      ))
       )

    (send top_w :realize)

    (send do_dircmp_button_w :set_callback :XMN_ACTIVATE_CALLBACK
	  '()
	  `(
	    (winterp-show-busy-progn
	     (let*
		 ((dir1 (send ,fsb1_w :get-directory-str))
		  (dir2 (send ,fsb2_w :get-directory-str))
		  (triplet
		   (dircmp dir1 dir2))
		  )
	       (send ,common_files_list_w :set_browser_items (aref triplet 0))
	       (send ,common_files_list_w :set_directory (cons dir1 dir2))
	       (send ,only_d1_files_list_w :set_browser_items (aref triplet 1))
	       (send ,only_d1_files_list_w :set_directory dir1)
	       (send ,only_d2_files_list_w :set_browser_items (aref triplet 2))
	       (send ,only_d2_files_list_w :set_directory dir2)
	       ))
	    ))

    (send common_files_list_w :set_callback :XMN_DEFAULT_ACTION_CALLBACK
	  '(CALLBACK_WIDGET)
	  `(
	    (winterp-show-busy-progn
	     (send ,diffs_list_w :set_browser_items 
		   (diff (car (send CALLBACK_WIDGET :get_directory))
			 (cdr (send CALLBACK_WIDGET :get_directory))
			 (send CALLBACK_WIDGET :get_selected_item)))
	     )
	    )
	  )
    (send diffs_list_w :set_callback :XMN_DEFAULT_ACTION_CALLBACK
	  '(CALLBACK_WIDGET)
	  `(
	    (winterp-show-busy-progn
	     (let ((browsed-obj (send CALLBACK_WIDGET :get_selected_item)))
	       (send ,d1_editor_w :find_file 
		     (concatenate 'string
				  (send browsed-obj :get_dir_0_name)
				  ;; "/"
				  (send browsed-obj :get_file_name))
		     (send browsed-obj :get_file_0_linenum)
		     )
	       (send ,d2_editor_w :find_file 
		     (concatenate 'string
				  (send browsed-obj :get_dir_1_name)
				  ;; "/"
				  (send browsed-obj :get_file_name))
		     (send browsed-obj :get_file_1_linenum)
		     )
	       ))
	    ))
    (send only_d1_files_list_w :set_callback :XMN_DEFAULT_ACTION_CALLBACK
	  '(CALLBACK_WIDGET)
	  `(
	    (winterp-show-busy-progn
	     (send ,d1_editor_w :find_file 
		   (concatenate 'string
				(send CALLBACK_WIDGET :get_directory)
				;; "/"
				(send CALLBACK_WIDGET :get_selected_item)))
	     )
	    ))
    (send only_d2_files_list_w :set_callback :XMN_DEFAULT_ACTION_CALLBACK
	  '(CALLBACK_WIDGET)
	  `(
	    (winterp-show-busy-progn
	     (send ,d2_editor_w :find_file 
		   (concatenate 'string
				(send CALLBACK_WIDGET :get_directory)
				;; "/"
				(send CALLBACK_WIDGET :get_selected_item)))
	     )
	    ))

    (send fsb1_w :set-file-selected-callback-closure ;note special method on WINTERP:FILE-SELECTION-WIDGET
	  (lambda (selected_file_str)
	    (winterp-show-busy-progn (send d1_editor_w :find_file selected_file_str))
	    ))
    (send fsb2_w :set-file-selected-callback-closure ;note special method on WINTERP:FILE-SELECTION-WIDGET
	  (lambda (selected_file_str)
	    (winterp-show-busy-progn (send d2_editor_w :find_file selected_file_str))
	    ))

    (send only_d1_files_prev_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,only_d1_files_list_w :browse_prev CALLBACK_XEVENT)
	    ))
    (send only_d1_files_next_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,only_d1_files_list_w :browse_next CALLBACK_XEVENT)
	    ))
    (send only_d1_files_edit_file_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '()
          `(
            (send ,only_d1_files_list_w :edit_selected_item)
            ))

    (send common_files_prev_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,common_files_list_w :browse_prev CALLBACK_XEVENT)
	    ))
    (send common_files_next_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,common_files_list_w :browse_next CALLBACK_XEVENT)
	    ))
    (send common_files_edit_file_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '()
          `(
            (send ,common_files_list_w :edit_selected_item)
            ))

    (send diffs_prev_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,diffs_list_w :browse_prev CALLBACK_XEVENT)
	    ))
    (send diffs_next_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,diffs_list_w :browse_next CALLBACK_XEVENT)
	    ))
    (send diffs_edit_file_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '()
          `(
            (send ,diffs_list_w :edit_selected_item)
            ))

    (send only_d2_files_prev_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,only_d2_files_list_w :browse_prev CALLBACK_XEVENT)
	    ))
    (send only_d2_files_next_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '(CALLBACK_XEVENT)
	  `(
	    (send ,only_d2_files_list_w :browse_next CALLBACK_XEVENT)
	    ))
    (send only_d2_files_edit_file_button_w :add_callback :XMN_ACTIVATE_CALLBACK
	  '()
          `(
            (send ,only_d2_files_list_w :edit_selected_item)
            ))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bring up an instance of the dircmp browser upon loading this file.
(dircmp-browser)
