; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         epoch-widg.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/epoch-widg.lsp,v 2.3 1994/06/06 14:43:17 npm Exp $
; Description:  Create a drawing-area widget containing a reparented epoch
;		4.0 "screen" -- this allows you to edit files with a real
;		text editor while placing the Epoch-edit windows into your
;		WINTERP-based applications. Epoch is a multiwindow
;		gnu-emacs-based editor available for free by anonymous ftp
;		from cs.uiuc.edu. You must load epoch-widg.el into Epoch
;		first, as this file calls epoch-functions defined there.
;		This file also assumes that you have Andy Norman's gnuserv
;		package running under Epoch -- the program
;		/usr/local/epoch/bin/gnudoit is used to send emacs-lisp
;		commands to Epoch.
; Author:       Niels P. Mayer
; Created:      Mon Mar  1 21:14:44 1993
; Modified:     Sun Jun  5 18:39:10 1994 (Niels Mayer) npm@indeed
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


(require "lib-utils/unixstuf")		;define read-exec-cmd and other unixisms...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call this function to create an epoch screen reparented inside WINTERP
;; widgetry. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-epoch-screen ()
  (let*
      (
       ;; The following call is intended to generate an initial shell widget size
       ;; from the generic screen returned by (epoch::screen-information nil).
       ;; The expose callback initialization code sets the shell to the exact size
       ;; determined from the epoch screen contained within. Since epoch has it's
       ;; own initial sizing idiosyncracies, we want to set our window to the epoch
       ;; window size, rather than forcing epoch window to size to the widget size...
       ;; This is obviously a kludge, as (epoch::screen-information nil)
       ;; will return screen-info for the most-recently-used screen, which may be
       ;; sized/font'd differently than the screen we'll create below. However, the
       ;; alternative is to have the window come up with no initial sizing info
       ;; whatsoever (i.e. a tiny window), and that seems worse...
       (epoch-screen-info		;#(x y width height border-w border-h map-state)
	(read (make-string-input-stream 
	       (concatenate 'string "#"		;read it as array
		       (read-exec-cmd	;returns string "(x y width height border-w border-h map-state)"
			"/usr/local/epoch/bin/gnudoit '(epoch::screen-information nil)'"
			))))
	)
       ;; TODO -- set resize hints so that resize increments are based off the
       ;; character sizes contained in the epoch window, rather than pixel based.
       ;; Currently, you can resize a winterp-epoch-window s.t. there can be a
       ;; partial-character-number-of-pixels "gap" at the border. Resize hints
       ;; would fix this. Implement this by using 'win-get-epoch-screen-info'
       ;; as below to figure out character sizes.
       (toplevel_w
	(send TOP_LEVEL_SHELL_WIDGET_CLASS :new "epoch-widg-shell"
	      :XMN_GEOMETRY
	      (format nil "~Ax~A"
		      (+ (aref epoch-screen-info 2) (aref epoch-screen-info 4))
		      (+ (aref epoch-screen-info 3) (aref epoch-screen-info 5))
		      )
	      :XMN_TITLE	"WINTERP: Reparented Epoch Screen"
	      :XMN_ICON_NAME	"W:epoch-widg"
	      ))
       ;; We don't actually "draw" into the drawing area widget, instead, it
       ;; becomes the parent window for the epoch screen. Handles resize and
       ;; expose callbacks.
       (da_w
	(send XM_DRAWING_AREA_WIDGET_CLASS :new :managed "epoch-da" toplevel_w
;;;	    :XMN_RESIZE_POLICY :RESIZE_NONE
	      ))
       ;; This is used as a predicate to determine whether epoch-window has been
       ;; initialized. It is used/set/cleared in the three callbacks below.
       (epoch-scrn-parent-win-id nil)	;initially set to NIL ==> initialization needed
       )

    (send da_w :set_callback :XMN_EXPOSE_CALLBACK
	  '(CALLBACK_WIDGET CALLBACK_WINDOW CALLBACK_REASON)
	  '(
	    (if (null epoch-scrn-parent-win-id)	;IF ... NOT INITIALIZED 
		;; THEN ... ON FIRST EXPOSE, SIZE SHELL BASED ON EPOCH-SCREEN SIZE
		(progn
		  ;; retrieve the x-window-id from the WINDOWOBJ retrieved from
		  ;; CALLBACK_WINDOW. The window-id is represented as an integer.
		  (setq epoch-scrn-parent-win-id (generic CALLBACK_WINDOW))

		  (format T "CREATE-EPOCH-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
			  epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)

		  (system (format nil
				  "/usr/local/epoch/bin/gnudoit '(win-create-scrn \"~A\")'"
				  epoch-scrn-parent-win-id))

		  (let ((epoch-screen-info ;#(x y width height border-w border-h map-state)
			 (read (make-string-input-stream 
				(concatenate 'string "#" ;read it as array
					(read-exec-cmd ;returns string "(x y width height border-w border-h map-state)"
					 (format nil
						 "/usr/local/epoch/bin/gnudoit '(epoch::screen-information (win-id-to-scrn \"~A\"))'"
						 epoch-scrn-parent-win-id)
					 )))))
			)
		    (send (send CALLBACK_WIDGET :parent) :set_values
			  :xmn_width (+ (aref epoch-screen-info 2) (aref epoch-screen-info 4)) 
			  :xmn_height (+ (aref epoch-screen-info 3) (aref epoch-screen-info 5))
			  )
		    )
		  )
	      ;; ELSE ... ONCE INITIALIZED, IGNORE EXPOSE EVENTS SINCE EPOCH TAKES CARE
	      ;; OF REFRESHING ITSELF
	      (format T "IGNORE-EXPOSE-EPOCH-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
		      epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
	      )
	    ))

    (send da_w :set_callback :XMN_RESIZE_CALLBACK
	  '(CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
	  '(
	    (if (null epoch-scrn-parent-win-id)	;IF ... NOT INITIALIZED
		;; THEN ... IGNORE THE RESIZE CALLBACK THAT OCCURS BEFORE
		;; INITIALIZATION IN THE FIRST EXPOSE CALLBACK.
		(format T "INIT-RESIZE-SCREEN! window=~A, widget=~A, reason=~A\n"
			CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
	      ;; ELSE ... ON SUBSEQUENT RESIZES, UPDATE THE SIZE OF THE EPOCH WINDOW.
	      (progn
		(format T "RESIZE-EPOCH-SCREEN! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
			epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
		(let* ((epoch-screen-info ;"(char-width char-height x y width height border-w border-h map-state)"
			(read (make-string-input-stream 
			       (concatenate 'string "#" ;read it as 9 element array "(char-width char-height x y width height border-w border-h map-state)"
				       (read-exec-cmd
					(format nil
						"/usr/local/epoch/bin/gnudoit '(win-get-epoch-screen-info \"~A\")'"
						epoch-scrn-parent-win-id)
					)
				       )
			       ))
			)
		       widget-pixel-width
		       widget-pixel-height
		       )

		  (send CALLBACK_WIDGET :get_values
			      :XMN_WIDTH 'widget-pixel-width
			      :XMN_HEIGHT 'widget-pixel-height
			      )

		  (system
		   (format nil
			   "/usr/local/epoch/bin/gnudoit '(epoch::change-screen-size ~A ~A (win-id-to-scrn \"~A\"))'"
			   (truncate (* widget-pixel-width
					(/
					 (float (aref epoch-screen-info 0)) ;epoch-screen-char-width
					 (aref epoch-screen-info 4) ;epoch-screen-pixel-width
					 )))
			   (truncate (* widget-pixel-height
					(/
					 (float (aref epoch-screen-info 1)) ;epoch-screen-char-height
					 (aref epoch-screen-info 5) ;epoch-screen-pixel-height
					 )))
			   epoch-scrn-parent-win-id
			   ))
		  ))
	      )
	    ))

    (send da_w :add_callback :XMN_DESTROY_CALLBACK
	  '(CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)
	  '(
	    (format T "DESTROY-EPOCH-SCREEN!! xwin-id=~A, window=~A, widget=~A, reason=~A\n"
		    epoch-scrn-parent-win-id CALLBACK_WINDOW CALLBACK_WIDGET CALLBACK_REASON)

	    (if epoch-scrn-parent-win-id
		(progn
		  (system (format nil
				  "/usr/local/epoch/bin/gnudoit '(win-destroy-scrn \"~A\")'"
				  epoch-scrn-parent-win-id
				  ))
		  (setq epoch-scrn-parent-win-id nil)
		  ))
	    ))

    (send toplevel_w :realize)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For people that want to see results from loading this file,
;; go ahead and create one!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(create-epoch-screen)
