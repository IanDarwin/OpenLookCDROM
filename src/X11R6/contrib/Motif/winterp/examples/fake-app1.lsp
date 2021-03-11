; -*-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         fake-app1.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/RCS/fake-app1.lsp,v 2.1 1994/06/06 14:35:02 npm Exp $
; Description:  Example application using higher-level widget-class subclasses
;		WINTERP:APPLICATION-WIDGET-CLASS, WINTERP:POPUP-MENU-WIDGET-CLASS,
;		WINTERP:RADIO-BOX-WIDGET-CLASS, WINTERP:CHECK-BOX-WIDGET-CLASS,
;		WINTERP:OPTION-MENU-WIDGET-CLASS...
;		to create a window with a menubar and pulldowns, etc.
; Author:       Niels P. Mayer
; Created:      Sun Apr 17 20:50:52 1994 (Niels Mayer) npm@indeed
; Modified:     Sun Jun  5 18:40:26 1994 (Niels Mayer) npm@indeed
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

(require "lib-utils/initialize")	;define WIDGET_CLASS method :GET
(require "lib-widgets/application")	;define WINTERP:APPLICATION-WIDGET-CLASS
(require "lib-widgets/simple-RC")	;define WINTERP:POPUP-MENU-WIDGET-CLASS, WINTERP:RADIO-BOX-WIDGET-CLASS, WINTERP:CHECK-BOX-WIDGET-CLASS, WINTERP:OPTION-MENU-WIDGET-CLASS, WINTERP:MENU-BAR-WIDGET-CLASS, WINTERP:POPUP-MENU-WIDGET-CLASS...

(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new "fakeapp1"
	    :XMN_TITLE		"WINTERP: Fake Application 1"
	    :XMN_ICON_NAME	"W:fake-app1"
	    ))
(setq app_w
      (send WINTERP:APPLICATION-WIDGET-CLASS :new :managed
	    "main" toplevel_w
	    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send app_w :add-menu-entry "Files"
      :mnemonic            #\F
      :type                :cascadebutton
      )
(send app_w :add-menu-entry '("Files" "Quit")
      :mnemonic			     #\Q
      :mnemonic-charset		     "ISO8859-1"
      :accelerator		     "Ctrl<Key>C"
      :accelerator-text              "^C"
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Files->Quit"
					       ))
      )
(send app_w :add-menu-entry '("Files" "Open")
      :mnemonic			     #\O
      :mnemonic-charset		     "ISO8859-1"
      :accelerator		     "Ctrl<Key>F"
      :accelerator-text              "^F"
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Files->Open"
					       ))
      )
(send app_w :add-menu-entry '("Files" "Open in New Window")
      :mnemonic			             #\N
      :mnemonic-charset		     "ISO8859-1"
      :accelerator		     "Ctrl<Key>O"
      :accelerator-text              "^O"
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Files->Open in New Window."
					       ))
      )
(send app_w :add-menu-entry '("Files" "Save")
      :mnemonic			     #\S
      :mnemonic-charset		     "ISO8859-1"
      :accelerator		     "Ctrl<Key>S"
      :accelerator-text              "^S"
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Files->Save"
					       ))
      )
(send app_w :add-menu-entry '("Files" "Save As")
      :mnemonic			          #\A
      :mnemonic-charset		     "ISO8859-1"
      :accelerator		     "Ctrl<Key>A"
      :accelerator-text              "^A"
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Files->Save As"
					       ))
      )

(send app_w :add-menu-entry "Edit"
      :mnemonic		   #\E
      :type :cascadebutton
      )
(send app_w :add-menu-entry '("Edit" "Title")
      :type			     :title
      )
(send app_w :add-menu-entry '("Edit" "One")
      :mnemonic			     #\O
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Edit->One"
					       ))
      )
(send app_w :add-menu-entry '("Edit" "")
      :type			     :double_separator
      )
(send app_w :add-menu-entry '("Edit" "Two")
      :mnemonic			     #\T
      :type			     :togglebutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Edit->Two"
					       ))
      )
(send app_w :add-menu-entry '("Edit" "")
      :type			     :separator
      )
(send app_w :add-menu-entry '("Edit" "Three")
      :mnemonic			     #\h
      :type			     :checkbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Edit->Three"
					       ))
      )
(send app_w :add-menu-entry '("Edit" "Four")
      :mnemonic			     #\F
      :type			     :radiobutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Edit-Four"
					       ))
      )
(send app_w :add-menu-entry '("Edit" "Five")
      :mnemonic			     #\i
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Edit->Five"
					       ))
      )

(send app_w :add-menu-entry "Fold"
      :mnemonic             #\o
      :type			:cascadebutton
      )
(send app_w :add-menu-entry '("Fold" "Submenu1")
      :mnemonic                            #\1
      :type			:cascadebutton
      )
(send app_w :add-menu-entry '("Fold" "One")
      :mnemonic			    #\C
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->One"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Two")
      :mnemonic			    #\S
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Two"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Three")
      :mnemonic			    #\P
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Three"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Submenu1" "One")
      :mnemonic			              #\O
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Submenu1->One"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Submenu1" "Two")
      :mnemonic			              #\T
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Submenu1->Two"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Submenu1" "Three")
      :mnemonic			               #\h
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Submenu1->Three"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Submenu1" "Submenu2")
      :mnemonic                                       #\2
      :type			:cascadebutton
      )

(send app_w :add-menu-entry '("Fold" "Submenu1" "Submenu2" "One")
      :mnemonic			              #\O
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Submenu1->Submenu2->One"
					       ))
      )
(send app_w :add-menu-entry '("Fold" "Submenu1" "Submenu2" "Two")
      :mnemonic			              #\T
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Submenu1->Submenu2->Two"
					       ))

      )
(send app_w :add-menu-entry '("Fold" "Submenu1" "Submenu2" "Three")
      :mnemonic			               #\h
      :type			     :pushbutton
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Fold->Submenu1->Submenu2->Three"
					       ))
      )
(send app_w :add-menu-entry "Spindle"
      :mnemonic            #\S
      :type		:cascadebutton
      )
(send app_w :add-menu-entry '("Spindle" "Title")
      :type		:title
      )
 
(send app_w :add-menu-entry "Mutilate"
      :mnemonic            #\M
      :type		:cascadebutton
      )
(send app_w :add-menu-entry '("Mutilate" "Title")
      :type		:title
      )

(send app_w :add-menu-entry "Help"
      :mnemonic            #\H
      :type :cascadebutton
      )
(send app_w :add-menu-entry '("Help" "Foo")
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Help->Foo"
					       ))
      )
(send app_w :add-menu-entry '("Help" "Bar")
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Help->Bar"
					       ))
      )
(send app_w :add-menu-entry '("Help" "Baz")
      :callback		             #'(lambda (widget xevent)
					 (send app_w :display-string
					       "Help->Baz"
					       ))
      )

(send app_w :make-menus)
(send app_w :set-menu-help-widget)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq rc_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
	    "rc" app_w
	    :XMN_ORIENTATION		:vertical
	    :XMN_PACKING		:pack_tight
	    :XMN_ENTRY_ALIGNMENT	:alignment_center
	    ))

(send app_w :set-work-area rc_w)

(let (pop_w)
  (setq pop_w		
	(send WINTERP:POPUP-MENU-WIDGET-CLASS :new :unmanaged
	      "popup" app_w
	      ))

  (defun popup-callback-fn (w x)
    (send app_w :display-string
	  (format nil  "popup-menu: ~A"
		  (xm_string_get_l_to_r (send w :get :XMN_LABEL_STRING)) x))
    )

  (defun fake-app1-popup ()
    (send pop_w :manage)
    )

  (send app_w :OVERRIDE_TRANSLATIONS
 	"<Btn3Down>: Lisp(fake-app1-popup)")

  (send pop_w :add-menu-entry "option1"
	:type     :pushbutton
	:mnemonic #\1
	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option2"
 	:type     :togglebutton
 	:mnemonic #\2
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option3"
 	:type     :checkbutton
 	:mnemonic #\3
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option4"
 	:type     :radiobutton
 	:mnemonic #\4
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option5"
 	:type     :cascadebutton
 	:mnemonic #\5
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry '("option5" "foo")
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry '("option5" "bar")
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry '("option5" "baz")
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option6"
 	:type     :separator
 	:mnemonic #\6
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option7"
 	:type     :double_separator
	:mnemonic #\7
 	:callback #'popup-callback-fn)
  (send pop_w :add-menu-entry "option8"
 	:type     :title
 	:mnemonic #\8
 	:callback #'popup-callback-fn)
  (send pop_w :recursive-make-menus)

  ;; must call this or any other widget function only after 
  ;; calling :recursive-make-menus. Until that function is called
  ;; no "widget" has actually been created.
  (send pop_w :add_callback :XMN_MAP_CALLBACK
 	'(CALLBACK_XEVENT)
 	'(
 	  (send pop_w :menu_position CALLBACK_XEVENT)
 	  ))
  )

(let (opt_w)
  (setq opt_w		
	(send WINTERP:RADIO-BOX-WIDGET-CLASS :new :managed
	      "radiobox" rc_w
	      :XMN_ORIENTATION		:horizontal
	      :XMN_BUTTON_SET		3
	      ))

  (defun radiobox-callback-fn (w x)
    (send app_w :display-string
	  (format nil  "radiobox ~A"
		  (xm_string_get_l_to_r (send w :get :XMN_LABEL_STRING))))
    )

  (send opt_w :add-menu-entry "option1"
	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option2"
 	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option3"
 	:mnemonic #\3
 	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option4"
 	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option5"
 	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option6"
 	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option7"
 	:callback #'radiobox-callback-fn)
  (send opt_w :add-menu-entry "option8"
 	:callback #'radiobox-callback-fn)

  (send opt_w :recursive-make-menus)
  )

(let (opt_w)
  (setq opt_w		
	(send WINTERP:CHECK-BOX-WIDGET-CLASS :new :managed
	      "checkbox" rc_w
	      :XMN_ORIENTATION		:horizontal
	      ;; Resource :XMN_BUTTON_SET doesn't do anything for 
	      ;; instances of XM_ROW_COLUMN_WIDGET_CLASS/:simple_check_box.
	      ;; see code below for a way to set the default check-item to
	      ;; something other than "0".
	      ;;; :XMN_BUTTON_SET	3
	      ))

  (defun checkbox-callback-fn (w x)
    (send app_w :display-string
	  (format nil  "checkbox ~A; state=~A"
		  (xm_string_get_l_to_r (send w :get :XMN_LABEL_STRING))
		  (send w :get_state)))
    )

  (send opt_w :add-menu-entry "option1"
	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option2"
 	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option3"
 	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option4"
 	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option5"
 	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option6"
 	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option7"
 	:callback #'checkbox-callback-fn)
  (send opt_w :add-menu-entry "option8"
 	:callback #'checkbox-callback-fn)

  (send opt_w :recursive-make-menus)

  ;; To set a specific checkbox entry, you need to do the following, since
  ;; setting resource :XMN_BUTTON_SET at widget creation time doesn't work
  ;; for :simple_check_box.
  ;; The code below, for example, sets the 4th item as the selected default
  ;; option. This needs to be done after calling method :recursive-make-menus
  ;; because no widgets within the checkbox exist prior to that...
  (send (aref (send opt_w :get_children) 3) :set_state t t)
  )

(let (opt_w)
  (setq opt_w		
	(send WINTERP:OPTION-MENU-WIDGET-CLASS :new :managed
	      "option-menu" rc_w
	      :XMN_OPTION_LABEL		"Option:"
	      :XMN_OPTION_MNEMONIC	#\p
	      ;; Resource :XMN_BUTTON_SET doesn't do anything for 
	      ;; instances of XM_ROW_COLUMN_WIDGET_CLASS/:simple_option_menu
	      ;; see code below (setting of resource :XMN_MENU_HISTORY) for a
	      ;; way to set the default option to something other than "0".
	      ;;; :XMN_BUTTON_SET	3
	      ))

  (defun optionmenu-callback-fn (w x)
    (send app_w :display-string
	  (format nil "optionmenu ~A"
		  (xm_string_get_l_to_r (send w :get :XMN_LABEL_STRING))
		  ))
    )

  (send opt_w :add-menu-entry "option1"
	:type     :pushbutton
	:mnemonic #\1
	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option2"
 	:type     :togglebutton
 	:mnemonic #\2
 	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option3"
 	:type     :checkbutton
 	:mnemonic #\3
 	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option4"
 	:type     :radiobutton
 	:mnemonic #\4
 	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option5"
 	:type     :cascadebutton
 	:mnemonic #\5
 	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option6"
 	:type     :separator
 	:mnemonic #\6
 	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option7"
 	:type     :double_separator
	:mnemonic #\7
 	:callback #'optionmenu-callback-fn)
  (send opt_w :add-menu-entry "option8"
 	:type     :title
 	:mnemonic #\8
 	:callback #'optionmenu-callback-fn)

  (send opt_w :recursive-make-menus)

  ;; to set a specific option-menu entry other than the default 0th entry,
  ;; you need to do the following. Setting resource :XMN_BUTTON_SET at widget
  ;; creation time doesn't work for :simple_option_menu because the option menu
  ;; itself is hidden beneath 'opt_w', but accessible as
  ;;	(send opt_w :get_sub_menu_widget)
  ;; The code below, for example, sets the 4th item as the selected default
  ;; option. This needs to be done after calling method :recursive-make-menus
  ;; because no widgets within the option menu exist prior to that...
  (send opt_w :set_values :XMN_MENU_HISTORY
	(aref (send (send opt_w :get_sub_menu_widget) :get_children)
	      3)
	)
  )

(send toplevel_w :realize)
