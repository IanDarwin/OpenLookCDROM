; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         test-gif.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/xtango/RCS/test-gif.lsp,v 2.1 1994/06/06 15:01:47 npm Exp $
; Description:  Random tests involving TANGO:GIF_IMAGE_CLASS. This file is
;		meant to be evaluated interactively. Also, many of the paths
;		to GIFs below won't be valid on your system.
; Author:       Niels P. Mayer
; Created:      January 1994
; Modified:     Mon Jun  6 04:08:21 1994 (Niels Mayer) npm@indeed
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

(require "xtango/imag-build")
(setq tango_w (get_moused_widget))

(load "rc-shell")

(setq pb (send xm_push_button_widget_class :new :managed
	       "pb" rc_w
	       :XMN_LABEL_STRING "capture scrn"
	       ))

(send pb :set_callback :XMN_ACTIVATE_CALLBACK '()
      '(
	(system "(xwd -frame | xwdtopnm | ppmtogif > /tmp/scrndump.gif ) 2>&1")
	(send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	      #C(0.0 0.0)		;location_coord
	      "/tmp/scrndump.gif"
	      :verbose
	      )
	))

(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/usr/local/winterp/examples/xtango/fluid2.gif"
	    :verbose
	    ))

(send im7 :tap_show)
(send im7 :tx_visible :perform)
(send im7 :tx_delete :perform)
(send tango_w :refresh)


(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/fluid2.gif"
	    :verbose
	    ))

(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/button.gif"
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/foo.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/writer.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/evolution.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/foo-red.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/guinness.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/hgttg.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/mack.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/oliver.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/wicked.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/quickie.acl.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/calvin4.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/calvin.hobbes.gif"
	    :verbose
	    ))
(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/beaker.gif"
	    :verbose
	    ))

(setq im7
      (send TANGO:GIF_IMAGE_CLASS :new :show tango_w
	    #C(0.0 0.0)			;location_coord
	    "/users/mayer/src/gif/scrndump.gif"
	    :verbose
	    ))


