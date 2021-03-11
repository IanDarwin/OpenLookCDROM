; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         simple-RC.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/lib-widgets/RCS/simple-RC.lsp,v 2.1 1994/06/06 14:56:00 npm Exp $
; Description:  Subclasses of XM_ROW_COLUMN_WIDGET_CLASS which simplify
;		access to the "simple menu" creation routines. Specifically:
;		WINTERP:RADIO-BOX-WIDGET-CLASS, WINTERP:OPTION-MENU-WIDGET-CLASS,
;		WINTERP:MENU-BAR-WIDGET-CLASS, WINTERP:POPUP-MENU-WIDGET-CLASS,
;		WINTERP:PULLDOWN-MENU-WIDGET-CLASS, WINTERP:CHECK-BOX-WIDGET-CLASS
; Author:       Niels P. Mayer
; Created:      Sun Apr 17 20:50:52 1994 (Niels Mayer) npm@indeed
; Modified:     Mon Jun  6 01:09:19 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/motif-vers")	;define *MOTIF-1.2-OR-LATER-P*, *MOTIF-1.1-OR-LATER-P*

;;;
;;; WINTERP:SIMPLE-RC-CLASS -- a subclass of XM_ROW_COLUMN_WIDGET_CLASS/:simple_*menu*
;;;
(setq WINTERP:SIMPLE-RC-CLASS		;name of the new subclass
      (send Class :new
            '(				;inst variables for subclass
	      ivar_num_entries		;FIXNUM -- number of entries in the rc-menu
	      ivar_managed_k		;KEYWORD -- :managed or :unmanaged
	      ivar_widget_name		;STRING -- resource name of widget
	      ivar_widget_parent	;WIDGETOBJ -- the parent widget
	      ivar_args			;LIST of resource/value pairs or NIL

	      ivar_entry_names		;list of string names of menu entries
	      ivar_entry_types		;correspoding list of types for entries, e.g. :PUSHBUTTON :CASCADEBUTTON
	      ivar_entry_submenus	;corresponding list of submenu widgets (pulldowns under cascade menus) or NIL
	      ivar_entry_mnemonics	;corresponding list of mnemonic characters
	      ivar_entry_mnemonic_charsets ;corresponding list of mnemonic-charset strings
	      ivar_entry_accelerators	;corresponding list of accelerator strings
	      ivar_entry_accelerator_texts ;corresponding list of accelerator-text strings
	      ivar_entry_callbacks	;corresponding list of callback closures

	      ivar_post_from_button	;FIXNUM representing where this pulldown is located in ivar_widget_parent
	      ivar_rc_type		;RowCol/:simple... kind
	      )
            '()                         ;no class variables for subclass
            XM_ROW_COLUMN_WIDGET_CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:SIMPLE-RC-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!

	(setq ivar_num_entries			0
	      ivar_managed_k			managed_k
	      ivar_widget_name			widget_name
	      ivar_widget_parent		widget_parent
	      ivar_args				args

	      ivar_entry_names			NIL
	      ivar_entry_types			NIL
	      ivar_entry_submenus		NIL
	      ivar_entry_mnemonics		NIL
	      ivar_entry_mnemonic_charsets 	NIL
	      ivar_entry_accelerators		NIL
	      ivar_entry_accelerator_texts 	NIL
	      ivar_entry_callbacks		NIL
	      )
	))

;; wrapper around XM_ROW_COLUMN_WIDGET_CLASS/:SIMPLE_RADIO_BOX
(setq WINTERP:RADIO-BOX-WIDGET-CLASS	;name of the new subclass
      (send Class :new
            '()				;inst variables for subclass
            '()                         ;no class variables for subclass
            WINTERP:SIMPLE-RC-CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:RADIO-BOX-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!
	(setq ivar_rc_type		:SIMPLE_RADIO_BOX
	      ivar_post_from_button	NIL)
	(apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       args)
	))

;; wrapper around XM_ROW_COLUMN_WIDGET_CLASS/:SIMPLE_OPTION_MENU
(setq WINTERP:OPTION-MENU-WIDGET-CLASS	;name of the new subclass
      (send Class :new
            '()				;inst variables for subclass
            '()                         ;no class variables for subclass
            WINTERP:SIMPLE-RC-CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:OPTION-MENU-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!
	(setq ivar_rc_type		:SIMPLE_OPTION_MENU
	      ivar_post_from_button	NIL)
	(apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       args)
	))

;; wrapper around XM_ROW_COLUMN_WIDGET_CLASS/:SIMPLE_MENU_BAR
(setq WINTERP:MENU-BAR-WIDGET-CLASS	;name of the new subclass
      (send Class :new
            '()				;inst variables for subclass
            '()                         ;no class variables for subclass
            WINTERP:SIMPLE-RC-CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:MENU-BAR-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!
	(setq ivar_rc_type		:SIMPLE_MENU_BAR
	      ivar_post_from_button	NIL)
	(apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       args)
	))

;; wrapper around XM_ROW_COLUMN_WIDGET_CLASS/:SIMPLE_POPUP_MENU
(setq WINTERP:POPUP-MENU-WIDGET-CLASS	;name of the new subclass
      (send Class :new
            '()				;inst variables for subclass
            '()                         ;no class variables for subclass
            WINTERP:SIMPLE-RC-CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:POPUP-MENU-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!
	(setq ivar_rc_type		:SIMPLE_POPUP_MENU
	      ivar_post_from_button	NIL)
	(apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       args)
	))

;; wrapper around XM_ROW_COLUMN_WIDGET_CLASS/:SIMPLE_PULLDOWN_MENU
(setq WINTERP:PULLDOWN-MENU-WIDGET-CLASS ;name of the new subclass
      (send Class :new
            '()				;inst variables for subclass
            '()                         ;no class variables for subclass
            WINTERP:SIMPLE-RC-CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:PULLDOWN-MENU-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  k_post_from_num	;NOTE SPECIAL ADDITIONAL PARAMETER
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!
	(setq ivar_rc_type		:SIMPLE_PULLDOWN_MENU
	      ivar_post_from_button	k_post_from_num)
	(apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       args)
	))

;; wrapper around XM_ROW_COLUMN_WIDGET_CLASS/:SIMPLE_CHECK_BOX
(setq WINTERP:CHECK-BOX-WIDGET-CLASS ;name of the new subclass
      (send Class :new
            '()				;inst variables for subclass
            '()                         ;no class variables for subclass
            WINTERP:SIMPLE-RC-CLASS	;name of the superclass
	    ))

;;;
;;; Override instance initializer (method :isnew).
;;;
(send WINTERP:CHECK-BOX-WIDGET-CLASS :answer :ISNEW
      '(managed_k widget_name widget_parent
		  &rest args
		  )
      '(
	;; DON'T CREATE WIDGET HERE!
	(setq ivar_rc_type		:SIMPLE_CHECK_BOX
	      ivar_post_from_button	NIL)
	(apply #'send-super :isnew
	       managed_k widget_name widget_parent
	       args)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makes the last entry in the menu-bar the widget for :XMN_MENU_HELP_WIDGET.
;; This forces the "Help" menu bar entry to the far-right of the menu-bar.
;; This is optional because some applications may not want a help widget in
;; the menu bar.
;; One should only call this after :recursive-make-menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send WINTERP:MENU-BAR-WIDGET-CLASS :answer :SET-MENU-HELP-WIDGET
      '()
      '(
	;; special motif attachment forcing correct placement of help pulldown.
	(let ((ch (send-super :get_children)))
	  (send-super :set_values
		      :XMN_MENU_HELP_WIDGET (aref ch (1- (length ch)))
		      )
	  )
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send xxx :ADD-MENU-ENTRY "Files"
;;                         :mnemonic #\F
;;                         :type :CASCADEBUTTON)
;;
;; (send xxx :ADD-MENU-ENTRY '("Files" "Quit")
;;                         :mnemonic		#\Q
;;			    :mnemonic-charset	"ISO8859-1"
;;                         :type		:PUSHBUTTON
;;			    :accelerator	"Ctrl<Key>C")
;;			    :accelerator-text	"^C"
;;			    :callback		#'(lambda (widget xevent) ...)
;;
;;
;; :type <typeval>
;; 	 <typeval> is one of :PUSHBUTTON, :TOGGLEBUTTON, :CHECKBUTTON,
;;				:RADIOBUTTON, :CASCADEBUTTON, :SEPARATOR,
;;				:DOUBLE_SEPARATOR, :TITLE
;;		Note that 
;;			* for a radio-box, no matter what :type
;;			  value given, all buttons will be shown as :radiobutton;
;;			  in-fact, :type need not be specified for radio-box
;;			* for a check-box, no matter what :type
;;			  value given, all buttons will be shown as :checkbutton;
;;			  in-fact, :type need not be specified for check-box
;;			* for a menu-bar, no matter what :type
;;			  value given, all buttons will be shown as :cascadebutton;
;;			  in-fact, :type need not be specified for menu-bar.
;;			* for an option menu, if :type :cascadebutton given,
;;			  it will assume :pushbutton; in-fact, :type need not
;;			  be given for option-menu since it willd efault to :pushbutton.
;;			* for popup or pulldown menus, :type defaults to :pushbutton
;;			  if not given.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: this method is only used by WINTERP:POPUP-MENU-WIDGET-CLASS and
;; WINTERP:PULLDOWN-MENU-WIDGET-CLASS; other classes define their own specialized methods.
(send WINTERP:SIMPLE-RC-CLASS :answer :ADD-MENU-ENTRY
      '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
					;If LIST, goes through names left-to-right finding submenus
					;associated w/ names; and adds new entry to the next-to-last
					;entry-name -- the name is the last elt in entry-name.
	&key
	((:mnemonic		k_mnemonic_char)	NIL)
	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
	((:type			k_type_kwd)		NIL)
	((:accelerator		k_accelerator_str)	NIL)
	((:accelerator-text	k_accelerator_text_str)	NIL)
	((:callback		k_callback_funct)	NIL)
	)
      '(
	(cond
	 ((stringp entry-name)
	  ;; if user didn't pass the type of the menu entry, default it to :pushbutton
	  (if (null k_type_kwd)		
	      (setq k_type_kwd :pushbutton))
	  ;; cons appropriate entry data onto <self>'s ivars... widget and subwidgets
	  ;; get created at the end when :RECURSIVE-MAKE-MENUS is called...
	  (setq ivar_entry_names		(cons entry-name ivar_entry_names)
		ivar_entry_types		(cons k_type_kwd ivar_entry_types)
		ivar_entry_mnemonics		(cons k_mnemonic_char ivar_entry_mnemonics)
		ivar_entry_mnemonic_charsets	(cons k_mnemonic_charset_str ivar_entry_mnemonic_charsets)
		ivar_entry_accelerators		(cons k_accelerator_str ivar_entry_accelerators)
		ivar_entry_accelerator_texts	(cons k_accelerator_text_str ivar_entry_accelerator_texts)
		ivar_entry_callbacks		(cons k_callback_funct ivar_entry_callbacks)
		)
	  (if (eq k_type_kwd :CASCADEBUTTON)
	      (setq ivar_entry_submenus
		    (cons (send WINTERP:PULLDOWN-MENU-WIDGET-CLASS :new :unmanaged
				"pulldown" self
				ivar_num_entries ;sets XmNpostFromButton
				)
			  ivar_entry_submenus))
	    (setq ivar_entry_submenus
		  (cons NIL ivar_entry_submenus))
	    )
	  (setq ivar_num_entries (1+ ivar_num_entries))
	  )

	 ;; recurse 
	 ((consp entry-name)
	  ;; 'car' of entry-name should correspond to a cascade button
	  ;; entry on <self> find that cascade-button from ivar_entry_submenus,
	  ;; and call :add-menu-entry on the associated pulldown.
	  (let* (
		 (name (car entry-name))
		 (posn (position-if #'(lambda (x) (string= x name)) ivar_entry_names))
		 (subm (if posn (nth posn ivar_entry_submenus)
			 (error (format nil ":ADD-MENU-ENTRY -- can't create submenu ~S because entry name not found"
					entry-name)
				name)))
		 )
	    (if subm
		(send subm :add-menu-entry (if (cddr entry-name)
					       (cdr entry-name)
					     (cadr entry-name)
					     )
		      :mnemonic		k_mnemonic_char
		      :mnemonic-charset	k_mnemonic_charset_str
		      :type		k_type_kwd
		      :accelerator	k_accelerator_str
		      :accelerator-text	k_accelerator_text_str
		      :callback		k_callback_funct
		      )
	      (error (format nil ":ADD-MENU-ENTRY -- can't create submenu ~S because no associated :CASCADEBUTTON defined"
			     entry-name)
		     name)
	      )
	    ))

	 (T
	  (error ":ADD-MENU-ENTRY -- bad type for <entry-name>" entry-name)
	  ))
	))

(send WINTERP:RADIO-BOX-WIDGET-CLASS :answer :ADD-MENU-ENTRY
      '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
					;If LIST, goes through names left-to-right finding submenus
					;associated w/ names; and adds new entry to the next-to-last
					;entry-name -- the name is the last elt in entry-name.
	&key
	((:mnemonic		k_mnemonic_char)	NIL)
	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
	((:type			k_type_kwd)		NIL)
	((:accelerator		k_accelerator_str)	NIL)
	((:accelerator-text	k_accelerator_text_str)	NIL)
	((:callback		k_callback_funct)	NIL)
	)
      '(
	(cond
	 ((stringp entry-name)
	  ;; if user didn't pass the type of the menu entry, default it based on
	  ;; kind of "menu" -- for radio-box, set type to :radiobutton no matter
	  ;; what user specified
	  (setq k_type_kwd :radiobutton) 

	  ;; cons appropriate entry data onto <self>'s ivars... widget and subwidgets
	  ;; get created at the end when :RECURSIVE-MAKE-MENUS is called...
	  (setq ivar_entry_names		(cons entry-name ivar_entry_names)
		ivar_entry_types		(cons k_type_kwd ivar_entry_types)
		ivar_entry_mnemonics		(cons k_mnemonic_char ivar_entry_mnemonics)
		ivar_entry_mnemonic_charsets	(cons k_mnemonic_charset_str ivar_entry_mnemonic_charsets)
		ivar_entry_accelerators		(cons k_accelerator_str ivar_entry_accelerators)
		ivar_entry_accelerator_texts	(cons k_accelerator_text_str ivar_entry_accelerator_texts)
		ivar_entry_callbacks		(cons k_callback_funct ivar_entry_callbacks)
		)

	  (setq ivar_num_entries (1+ ivar_num_entries))
	  )
	 (T
	  (error "WINTERP:RADIO-BOX-WIDGET-CLASS/:ADD-MENU-ENTRY -- bad type for <entry-name>" entry-name)
	  ))
	))

(send WINTERP:OPTION-MENU-WIDGET-CLASS :answer :ADD-MENU-ENTRY
      '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
					;If LIST, goes through names left-to-right finding submenus
					;associated w/ names; and adds new entry to the next-to-last
					;entry-name -- the name is the last elt in entry-name.
	&key
	((:mnemonic		k_mnemonic_char)	NIL)
	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
	((:type			k_type_kwd)		NIL)
	((:accelerator		k_accelerator_str)	NIL)
	((:accelerator-text	k_accelerator_text_str)	NIL)
	((:callback		k_callback_funct)	NIL)
	)
      '(
	(cond
	 ((stringp entry-name)
	  ;; if user didn't pass the type of the menu entry, default it based on
	  ;; kind of "menu" -- for optionmenu, if no :type set, default to :pushbutton
	  (if (null k_type_kwd)		
	      (setq k_type_kwd :pushbutton)
	    (if (eq k_type_kwd :cascadebutton) ;disallow cascade-button entries -- replace w/ pushbutton
		(setq k_type_kwd :pushbutton)
	      ))
	  ;; cons appropriate entry data onto <self>'s ivars... widget and subwidgets
	  ;; get created at the end when :RECURSIVE-MAKE-MENUS is called...
	  (setq ivar_entry_names		(cons entry-name ivar_entry_names)
		ivar_entry_types		(cons k_type_kwd ivar_entry_types)
		ivar_entry_mnemonics		(cons k_mnemonic_char ivar_entry_mnemonics)
		ivar_entry_mnemonic_charsets	(cons k_mnemonic_charset_str ivar_entry_mnemonic_charsets)
		ivar_entry_accelerators		(cons k_accelerator_str ivar_entry_accelerators)
		ivar_entry_accelerator_texts	(cons k_accelerator_text_str ivar_entry_accelerator_texts)
		ivar_entry_callbacks		(cons k_callback_funct ivar_entry_callbacks)
		)
	  (setq ivar_num_entries (1+ ivar_num_entries))
	  )
	 (T
	  (error "WINTERP:OPTION-MENU-WIDGET-CLASS/:ADD-MENU-ENTRY -- bad type for <entry-name>" entry-name)
	  ))
	))

(send WINTERP:MENU-BAR-WIDGET-CLASS :answer :ADD-MENU-ENTRY
      '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
					;If LIST, goes through names left-to-right finding submenus
					;associated w/ names; and adds new entry to the next-to-last
					;entry-name -- the name is the last elt in entry-name.
	&key
	((:mnemonic		k_mnemonic_char)	NIL)
	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
	((:type			k_type_kwd)		NIL)
	((:accelerator		k_accelerator_str)	NIL)
	((:accelerator-text	k_accelerator_text_str)	NIL)
	((:callback		k_callback_funct)	NIL)
	)
      '(
	;; if user didn't pass the type of the menu entry, default it based on
	;; kind of "menu" -- for menu-bar, set type to :cascadebutton no matter
	;; what user specified.
	(if (stringp entry-name)
	    (setq k_type_kwd :cascadebutton))

	(send-super :add-menu-entry entry-name
		    :mnemonic             k_mnemonic_char
		    :mnemonic-charset     k_mnemonic_charset_str
		    :type                 k_type_kwd
		    :accelerator          k_accelerator_str
		    :accelerator-text     k_accelerator_text_str
		    :callback             k_callback_funct
		    )
	))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't define WINTERP:POPUP-MENU-WIDGET-CLASS/:ADD-MENU-ENTRY -- use the superclass'
;;; :ADD-MENU-ENTRY method.
;;;;;;;;;;;;;;;;;;;;;;;;
;;; (send WINTERP:POPUP-MENU-WIDGET-CLASS :answer :ADD-MENU-ENTRY
;;;       '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
;;; 					;If LIST, goes through names left-to-right finding submenus
;;; 					;associated w/ names; and adds new entry to the next-to-last
;;; 					;entry-name -- the name is the last elt in entry-name.
;;; 	&key
;;; 	((:mnemonic		k_mnemonic_char)	NIL)
;;; 	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
;;; 	((:type			k_type_kwd)		NIL)
;;; 	((:accelerator		k_accelerator_str)	NIL)
;;; 	((:accelerator-text	k_accelerator_text_str)	NIL)
;;; 	((:callback		k_callback_funct)	NIL)
;;; 	)
;;;       '(
;;; 
;;; 	))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't define WINTERP:POPUP-MENU-WIDGET-CLASS/:ADD-MENU-ENTRY -- use the superclass'
;;; :ADD-MENU-ENTRY method.
;;;;;;;;;;;;;;;;;;;;;;;;
;;; (send WINTERP:PULLDOWN-MENU-WIDGET-CLASS :answer :ADD-MENU-ENTRY
;;;       '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
;;; 					;If LIST, goes through names left-to-right finding submenus
;;; 					;associated w/ names; and adds new entry to the next-to-last
;;; 					;entry-name -- the name is the last elt in entry-name.
;;; 	&key
;;; 	((:mnemonic		k_mnemonic_char)	NIL)
;;; 	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
;;; 	((:type			k_type_kwd)		NIL)
;;; 	((:accelerator		k_accelerator_str)	NIL)
;;; 	((:accelerator-text	k_accelerator_text_str)	NIL)
;;; 	((:callback		k_callback_funct)	NIL)
;;; 	)
;;;       '(
;;; 	))

(send WINTERP:CHECK-BOX-WIDGET-CLASS :answer :ADD-MENU-ENTRY
      '(entry-name			;STRING or LIST. If STRING, adds a new entry under <self>.
					;If LIST, goes through names left-to-right finding submenus
					;associated w/ names; and adds new entry to the next-to-last
					;entry-name -- the name is the last elt in entry-name.
	&key
	((:mnemonic		k_mnemonic_char)	NIL)
	((:mnemonic-charset	k_mnemonic_charset_str)	NIL)
	((:type			k_type_kwd)		NIL)
	((:accelerator		k_accelerator_str)	NIL)
	((:accelerator-text	k_accelerator_text_str)	NIL)
	((:callback		k_callback_funct)	NIL)
	)
      '(
	(cond
	 ((stringp entry-name)
	  ;; if user didn't pass the type of the menu entry, default it based on
	  ;; kind of "menu" -- for check-box, set type to :checkbutton no matter
	  ;; what user specified.
	  (setq k_type_kwd :checkbutton)
	  ;; cons appropriate entry data onto <self>'s ivars... widget and subwidgets
	  ;; get created at the end when :RECURSIVE-MAKE-MENUS is called...
	  (setq ivar_entry_names		(cons entry-name ivar_entry_names)
		ivar_entry_types		(cons k_type_kwd ivar_entry_types)
		ivar_entry_mnemonics		(cons k_mnemonic_char ivar_entry_mnemonics)
		ivar_entry_mnemonic_charsets	(cons k_mnemonic_charset_str ivar_entry_mnemonic_charsets)
		ivar_entry_accelerators		(cons k_accelerator_str ivar_entry_accelerators)
		ivar_entry_accelerator_texts	(cons k_accelerator_text_str ivar_entry_accelerator_texts)
		ivar_entry_callbacks		(cons k_callback_funct ivar_entry_callbacks)
		)

	  (setq ivar_num_entries (1+ ivar_num_entries))
	  )

	 (T
	  (error "WINTERP:CHECK-BOX-WIDGET-CLASS/:ADD-MENU-ENTRY -- bad type for <entry-name>" entry-name)
	  ))
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (send xxx :recursive-make-menus)
;; Actually create widget superclass after calling :NEW and after
;; calling :ADD-ENTRY for all menus and submenus.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(send WINTERP:RADIO-BOX-WIDGET-CLASS :answer :RECURSIVE-MAKE-MENUS
      '()
      '(
	(if (> ivar_num_entries 0)
	    (progn
	      (send-super :_create_widgets ivar_managed_k)

	      ;; add special entry callback processor for :SIMPLE_RADIO_BOX
	      ;; this only calls the callback when the entry is set;
	      ;; normally, the callback would call both when entry set and cleared.
	      (let (;; ivar_entry_callbacks was cons'd together, must reverse the list;
		    ;; while reversing, might as well return an array which is faster
		    ;; to index by position...
		    (cb-array
		     (winterp:list-reverse-returning-array ivar_entry_callbacks)))
		(send-super :add_callback :XMN_ENTRY_CALLBACK
			    '(CALLBACK_ENTRY_WIDGET CALLBACK_ENTRY_SET CALLBACK_XEVENT)
			    '(
			      (if CALLBACK_ENTRY_SET
				  (let ((callback-fn
					 (aref cb-array
					       (winterp:get-simpleRC-entry-position CALLBACK_ENTRY_WIDGET)
					       )))
				    (if callback-fn
					(funcall callback-fn CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
				      )
				    ))
			      ))
		)

	      ;; don't bother recursing on checkbox or radiobox, since
	      ;; these can only have togglebuttons inside (no cascade-->pulldown combos)
	      ;; (send-super :_recursive_make_menus)
	      ))
	))

(send WINTERP:OPTION-MENU-WIDGET-CLASS :answer :RECURSIVE-MAKE-MENUS
      '()
      '(
	(if (> ivar_num_entries 0)
	    (progn
	      (send-super :_create_widgets ivar_managed_k)

	      ;; add special entry callback processor for :SIMPLE_OPTION_MENU
	      ;; Must use "(send <optmenu> :GET_SUB_MENU_WIDGET)" rather than <optmenu>:
	      ;; The reason for this is that the option menu is composed of a label + cascade
	      ;; button, with the cascade attached to a pulldown. The :XMN_ENTRY_CALLBACK
	      ;; would have to occur on the pulldown menu, not on the option r/c. Thus,
	      ;; we need to attach the entry callback to the widgetobj returned by doing
	      ;; :get_values on resource XmNsubMenuId. However, due to a weird bug in
	      ;; Motif 1.1, you have to use method :GET_SUB_MENU_WIDGET instead of retrieving
	      ;; :XMN_SUB_MENU_ID.
	      (let (;; ivar_entry_callbacks was cons'd together, must reverse the list
		    ;; while reversing, might as well return an array which is faster
		    ;; to index by position...
		    (cb-array
		     (winterp:list-reverse-returning-array ivar_entry_callbacks)))
		(send (send-super :get_sub_menu_widget) :add_callback :XMN_ENTRY_CALLBACK ;must use :GET_SUB_MENU_WIDGET rather than :get_values/:xmn_sub_menu_id, since that one reveals 1.1 bug.
		      '(CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
		      '(
			(let ((callback-fn
			       (aref cb-array
				     (winterp:get-simpleRC-entry-position CALLBACK_ENTRY_WIDGET)
				     )))
			  (if callback-fn
			      (funcall callback-fn CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
			    )
			  )
			))
		)

	      ;; don't bother recursing on option-menu since, these can't have
	      ;; further cascade-->pulldown combos.
	      ;; (send-super :_recursive_make_menus)
	      ))
	))

(send WINTERP:MENU-BAR-WIDGET-CLASS :answer :RECURSIVE-MAKE-MENUS
      '()
      '(
	(if (> ivar_num_entries 0)
	    (progn
	      (send-super :_create_widgets ivar_managed_k)

	      ;; don't bother calling entry callback for menu bar itself
	      ;; (send-super :_add_callbacks)

	      (send-super :_recursive_make_menus)
	      ))
	))

(send WINTERP:POPUP-MENU-WIDGET-CLASS :answer :RECURSIVE-MAKE-MENUS
      '()
      '(
	(if (> ivar_num_entries 0)
	    (progn
	      (send-super :_create_widgets :unmanaged) ;force unmanaged widget creation, otherwise you get XtWarnings...

	      (send-super :_add_callbacks)

	      (send-super :_recursive_make_menus)
	      ))
	))

(send WINTERP:PULLDOWN-MENU-WIDGET-CLASS :answer :RECURSIVE-MAKE-MENUS
      '()
      '(
	(if (> ivar_num_entries 0)
	    (progn
	      (send-super :_create_widgets :unmanaged) ;force unmanaged widget creation, otherwise you get XtWarnings...

	      (send-super :_add_callbacks)

	      (send-super :_recursive_make_menus)
	      ))
	))

(send WINTERP:CHECK-BOX-WIDGET-CLASS :answer :RECURSIVE-MAKE-MENUS
      '()
      '(
	(if (> ivar_num_entries 0)
	    (progn
	      (send-super :_create_widgets ivar_managed_k)

	      (send-super :_add_callbacks)

	      ;; don't bother recursing on checkbox or radiobox, since
	      ;; these can only have togglebuttons inside (no cascade-->pulldown combos)
	      ;; (send-super :_recursive_make_menus)
	      ))
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class-specific methods...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; (send <optmenu> :GET-SELECTED-POS)
;;; returns the position of the selected entry in the option menu as
;;; FIXNUM [0..n].
;;;
(send WINTERP:OPTION-MENU-WIDGET-CLASS :answer :GET-SELECTED-POS
      '()
      '(
	(winterp:get-simpleRC-entry-position
	 (car (send (send-super :get_sub_menu_widget) :get_values
		    :XMN_MENU_HISTORY nil)
	      ))
	))

;;;
;;; (send <optmenu> :SET-POS <pos>)
;;; set the optionmenu entry at FIXNUM position <pos> [0..n]
;;;
(send WINTERP:OPTION-MENU-WIDGET-CLASS :answer :SET-POS
      '(pos)
      '(
	(send-super :set_values :XMN_MENU_HISTORY
		    (aref (send (send-super :get_sub_menu_widget) :get_children)
			  pos)
		    )
	))

;;;
;;; (send <radiobox> :GET-SELECTED-POS)
;;; returns the position of the selected radiobox entry as FIXNUM [0..n].
;;;
(send WINTERP:RADIO-BOX-WIDGET-CLASS :answer :GET-SELECTED-POS
      '()
      '(
	(position-if #'(lambda (x) (send x :get_state))
		     (send-super :get_children))
	))

;;;
;;; (send <radiobox> :SET-POS <pos>)
;;; set the radiobox entry at FIXNUM position <pos> [0..n]
;;;
(send WINTERP:RADIO-BOX-WIDGET-CLASS :answer :SET-POS
      '(pos)
      '(
	(send (aref (send-super :get_children) pos) :set_state t t)
	))

;;;
;;; (send <radiobox> :CLEAR-POS <pos>)
;;; clear the radiobox entry at FIXNUM position <pos> [0..n]
;;;
(send WINTERP:RADIO-BOX-WIDGET-CLASS :answer :CLEAR-POS
      '(pos)
      '(
	(send (aref (send-super :get_children) pos) :set_state nil t)
	))

;;;
;;; (send <checkbox> :SET-POS <pos>)
;;; set the radiobox entry at FIXNUM position <pos> [0..n]
;;;
(send WINTERP:CHECK-BOX-WIDGET-CLASS :answer :SET-POS
      '(pos)
      '(
	(send (aref (send-super :get_children) pos) :set_state t t)
	))

;;;
;;; (send <checkbox> :CLEAR-POS <pos>)
;;; clear the radiobox entry at FIXNUM position <pos> [0..n]
;;;
(send WINTERP:CHECK-BOX-WIDGET-CLASS :answer :CLEAR-POS
      '(pos)
      '(
	(send (aref (send-super :get_children) pos) :set_state nil t)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private methods on WINTERP:SIMPLE-RC-CLASS -- called by subclass'
;; methods ":RECURSIVE-MAKE-MENUS" ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Warning: Don't call this method unless (> ivar_num_entries 0)
(send WINTERP:SIMPLE-RC-CLASS :answer :_CREATE_WIDGETS
      '(managed_kwd)
      '(
	;; create 'self', an instance of XM_ROW_COLUMN_WIDGET_CLASS
	(apply #'send-super :isnew	;call superclass's init to create widget
	       managed_kwd ivar_rc_type
	       ivar_widget_name ivar_widget_parent
	       :XMN_BUTTON_COUNT ivar_num_entries
	       (append
		(if ivar_post_from_button
		    (list :XMN_POST_FROM_BUTTON	ivar_post_from_button)
		  NIL)

		;; the following entries don't set the associated resource if
		;; the ALL corresponding entries in previous calls to
		;; :ADD-MENU-ENTRY weren't set. If even just one of the
		;; entries was set, 'winterp:list-reverse-while-replacing-nils-with-value'
		;; will enter default values in-place of the NILs...
		(if (not (eq nil (find-if #'(lambda (x) x) ivar_entry_names)))
		    (list :XMN_BUTTONS
			  (winterp:list-reverse-while-replacing-nils-with-value
			   ivar_entry_names ""))
		  NIL)
		(if (not (eq nil (find-if #'(lambda (x) x) ivar_entry_types)))
		    (list :XMN_BUTTON_TYPE
			  (winterp:list-reverse-while-replacing-nils-with-value
			   ivar_entry_types :pushbutton))
		  NIL)
		(if (not (eq nil (find-if #'(lambda (x) x) ivar_entry_mnemonics)))
		    (list :XMN_BUTTON_MNEMONICS
			  (winterp:list-reverse-while-replacing-nils-with-value
			   ivar_entry_mnemonics #\ ))
		  NIL)
		(if (not (eq nil (find-if #'(lambda (x) x) ivar_entry_mnemonic_charsets)))
		    (list :XMN_BUTTON_MNEMONIC_CHAR_SETS
			  (winterp:list-reverse-while-replacing-nils-with-value
			   ivar_entry_mnemonic_charsets
			   ""))
		  NIL)
		(if (not (eq nil (find-if #'(lambda (x) x) ivar_entry_accelerators)))
		    (list :XMN_BUTTON_ACCELERATORS
			  (winterp:list-reverse-while-replacing-nils-with-value
			   ivar_entry_accelerators ""))
		  NIL)
		(if (not (eq nil (find-if #'(lambda (x) x) ivar_entry_accelerator_texts)))
		    (list :XMN_BUTTON_ACCELERATOR_TEXT
			  (winterp:list-reverse-while-replacing-nils-with-value
			   ivar_entry_accelerator_texts ""))
		  NIL)
		ivar_args
		))
	))

;; This is the "default" entry callback processor; note that some 
;; superclasses will provide their own specialized callback processors.
;; Warning: Don't call this method unless (> ivar_num_entries 0)
(send WINTERP:SIMPLE-RC-CLASS :answer :_ADD_CALLBACKS
      '()
      '(
	(let (;; ivar_entry_callbacks was cons'd together, must reverse the list
	      ;; while reversing, might as well return an array which is faster
	      ;; to index by position...
	      (cb-array
	       (winterp:list-reverse-returning-array ivar_entry_callbacks)))
	  (send-super :add_callback :XMN_ENTRY_CALLBACK
		      '(CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
		      '(
			(let ((callback-fn
			       (aref cb-array
				     (winterp:get-simpleRC-entry-position CALLBACK_ENTRY_WIDGET)
				     )))
			  (if callback-fn
			      (funcall callback-fn CALLBACK_ENTRY_WIDGET CALLBACK_XEVENT)
			    )
			  )
			))
	  )
	))

;; Warning: Don't call this method unless (> ivar_num_entries 0)
(send WINTERP:SIMPLE-RC-CLASS :answer :_RECURSIVE_MAKE_MENUS
      '()
      '(
	(map nil #'(lambda (submenu)
		     (if submenu
			 (send submenu :recursive-make-menus)
		       )
		     )
	     ivar_entry_submenus)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions for methods above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 (*MOTIF-1.2-OR-LATER-P*		;only motif 1.2 supports :XMN_POSITION_INDEX resource
  ;; IF MOTIF >= 1.2, then can use XmRowColumn :XMN_POSITION_INDEX resource
  ;; to retrieve the position of <entry-widget> from the RowColumn container.
  (defmacro winterp:get-simpleRC-entry-position (entry-widget)
    `(car (send ,entry-widget :get_values :XMN_POSITION_INDEX nil))
    )
  )
 (*MOTIF-1.1-OR-LATER-P*
  ;; ELSE IF MOTIF 1.1 must use a crude hack: the XmRowColumn
  ;; "simple" creation routines create entry buttons named "button_<#>"
  ;; so we use 'read' to return the FIXNUM <#> after truncating the
  ;; first 7 chars "button_" from the front of the string....
  (defmacro winterp:get-simpleRC-entry-position (entry-widget)
    `(read (make-string-input-stream (send ,entry-widget :name) 7))
    )
  )
 (T
  (error "Cannot use Simple Menu creation classes with Motif 1.0") 
  ))

(defun winterp:list-reverse-while-replacing-nils-with-value (val-list value)
  (do* ((cur-list		val-list	(cdr cur-list))
	(result-list		'()		)
	)
       ((null cur-list)
	result-list			;RETURN value
	)
       (setq result-list
	     (cons (if (car cur-list)
		       (car cur-list)
		     value)
		   result-list))
       )
  )

(defun winterp:list-reverse-returning-array (x)
  (do* ((len (length x))
	(res (make-array len))
	(idx (1- len) (1- idx))
	(lis x			(cdr lis))
	)
       ((null lis)
	res
	)
       (setf (aref res idx) (car lis))
       )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "lib-widgets/simple-RC")
