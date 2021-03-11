;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         menufrob.lsp
; RCS:          $Header: /users/npm/src/winterp/examples/interactive/RCS/menufrob.lsp,v 2.1 1994/06/06 14:53:12 npm Exp $
; Description:  Dynamically altering XmCreateSimple...() created menus...
; Author:       Niels Mayer
; Created:      Sat Oct  5 21:34:39 1991
; Modified:     Mon Jun  6 00:27:55 1994 (Niels Mayer) npm@indeed
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

; To: OSF/Motif source licencee mailing List <motif-talk@osf.org>
; Subject: Re: XmCreateSimpleWhatever 
; In-Reply-To: Your message of Thu, 05 Sep 91 08:33:01 -0600
; Organization: Hewlett-Packard Labs, Software & Systems Lab, Palo Alto, CA.
; X-Mailer: mh6.7
; Date: Thu, 05 Sep 91 16:28:57 -0700
; Message-Id: <25898.684113337@hplnpm.hpl.hp.com>
; From: "Niels P. Mayer" <mayer@hplnpm.hpl.hp.com>
; 
; >    May be your example is incomplete or I was not clear enough.
; >    The problems I see are not at *creation* time - any tool can deal
; >    with the creation.
; >    The problems I see are with dynamics. If I use a "Simple" style
; >    constructor, I use one set of controls. But I can't use the same
; >    set of controls to alter (change, modify) created object(s).
; >    I could use a different set (RowColumn controls in case of menus),
; >    but how do I tell that to a generic tool ? Do I have to describe
; >    such an objectt twice: intialization controls versus runtime controls?
; >
; >    Note that my tools typically rely on XtGetResourceLists from any object
; >    they control - which does not quite work with XmCreateSimple ....
; 
; For your case, I think the answer is yes, you have to describe the object
; twice, and write lots of kludges because many composite/compound widgets in
; Motif do not offer a consistent view of the world based on resources alone.
; The resources-are-everything assumption may have been sufficient for Xaw or
; Xw... but, IMHO, they are an oversimplification when dealing with Motif.
; 
; Thus, you may want to punt on XmCreateSimple*() routines in some "generic
; tools" especially ones that rely entirely on resources, such as WCL, or
; ones that provide a language expressive enough to only cover widget
; creation and static bindings (UIL, WCL).
; 
; As I mentioned, the XmCreateSimple*() routines are oriented towards
; creation of STATIC {pulldown,popup,option} menus, radioboxes, and menubars.
; For menus which change dynamically at runtime, you may be better off using
; the "traditional" menu creation routines.
; 
; Perhaps I don't understand what you're getting at here -- you will use the
; traditional runtime controls to do dynamic changes, the main difference is
; that you cannot directly access the children widgets created by
; XmCreateSimple*(). The other difference is that the
; XmCreateSimple*()-specific resources are CREATE-TIME only resources. The
; arrays of button types, button names, accelerator texts, accelerators, etc
; are indexed-through at create-time and the appropriate values from each
; array are passed on to each child created by the XmCreateSimple*()
; routines. To make any dynamic changes, you'll have to access the child
; widgets directly using traditional means (many of which lie outside the
; language-expressivity of WCL/UIL).
; 
; However, that's not to say that you cannot do dynamic changes to a menu
; that is created via, say, XmCreateSimplePulldownMenu(). With respect to WCL
; and other resource-based tools, you'll need to look up the desired button
; with resource name "button_<#>" where 0 <= <#> < XmNbuttonCount.  When
; programming in C, or with WINTERP, you'll need to access the children of
; the simple menu via XmRowColumn resources XmNchildren/XmNnumChildren.
; 
; For example, to grey-out the "Save" button within the menu-code I posted
; yesterday, I just evaluate the following WINTERP expression:
; 	(send (aref (send pulldown_w :get_children) 3) :set_values
; 	      :XMN_SENSITIVE nil)
; And the button will immediately change to insensitive (aka greyed-out).
; 
; The code above just accesses the child widget at index 3 retrieved from the
; XmNchildren resource (:get_children) of pulldown_w. (pulldown_w is bound to
; a row/col instance created via XmCreateSimplePulldownMenu()). Once the
; child widget is accessed, the code sends a message to do an XtSetValues()
; (send ... :set_values ...) on resource XmNsensitive (:XMN_SENSITIVE),
; setting it to FALSE (nil).
; 
; One can just as easily go through and dynamically change the resources
; XmNlabelString, XmNmnemonic, XmNacceleratorText, and XmNaccelerators on all
; the widgets contained in the pulldown widget bound to 'pulldown_w' by
; evaluating the following code:
; 
; (let*					
;     (;; declare, bind, initialize local variables
;      (children	(send pulldown_w :get_children)) ;retrieve XmNchildren array
;      (length	(length children))
;      )
;   ;; loop through children
;   (do
;    ;; Declare variables local to do loop, initialize, specify incrementor
;    ((i 0 (1+ i)))
;    ;; Test for loop termination
;    ((<= length i)			
;     )
;    ;; Loop body
;    ;; Note: in the code below,
;    ;; (format nil "...~A..." i) is similar to the C code
;    ;; sprintf(buf, "...%d..." i)
;    (send (aref children i) :set_values	;XtSetValues() on i-th child
; 	 :XMN_LABEL_STRING	(format nil "menu: ~A" i)
; 	 :XMN_MNEMONIC		(format nil "~A" i)
; 	 :XMN_ACCELERATOR_TEXT	(format nil "^~A" i)
; 	 :XMN_ACCELERATORS      (format nil "Ctrl<Key>~A: Lisp(print ~A)" i i)
; 	 )
;    )					;end-do
;   )					;end-let*
; 
; This will change the menu to look like
; 		menu: _0_	^0
; 		menu: _1_	^1
; 		menu: _2_	^2
; 		menu: _3_	^3
; 		menu: _4_	^4
; 		menu: _5_	^5
; 
; (The only problem with the above code is that the accelerators do not get
; automatically installed on other widgets. This needs to be done with
; XtInstallAccelerators()...)


(require "rc-shell")

(setq menubar_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed :simple_menu_bar
	    "menubar" rc_w
	    :XMN_BUTTON_COUNT	   5	;create five cascadebuttons in menubar
	    :XMN_BUTTONS	   #("Files" "Edit" "Fold" "Spindle" "Mutilate")
	    :XMN_BUTTON_MNEMONICS #(#\F     #\E    #\o    #\S       #\M)
	    :XMN_BUTTON_TYPE       #(:CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON :CASCADEBUTTON)
	    ))

(setq pulldown_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :simple_pulldown_menu
	    "files" menubar_w
	    :XMN_POST_FROM_BUTTON		0 ;post pulldown from menubar's "Files"
	    :XMN_BUTTON_COUNT			5 ;create five buttons in this pulldown
	    :XMN_BUTTONS			#("Quit" "Open" "Open in New Window" "Save" "Save As")
	    :XMN_BUTTON_MNEMONICS		#(#\Q    #\O           #\N          #\S         #\A)
	    :XMN_BUTTON_MNEMONIC_CHAR_SETS	#("" "" "ISO8859-1" "ISO8859-1" "ISO8859-1")
	    :XMN_BUTTON_ACCELERATORS		#("Ctrl<Key>C" "Ctrl<Key>F" "Ctrl<Key>O" "Ctrl<Key>S" "Ctrl<Key>W")
	    :XMN_BUTTON_ACCELERATOR_TEXT	#("^C" "^F" "^O" "^S" "^W")
	    :XMN_BUTTON_TYPE			#(:PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON :PUSHBUTTON)
	    ))


(send pulldown_w :add_callback :XMN_ENTRY_CALLBACK ;use instead of XmNsimpleCallback
      '(CALLBACK_ENTRY_WIDGET)		;gets bound to widget causing c.b.
      '(
	;; (send CALLBACK_ENTRY_WIDGET :name)==XtName() returns "button_<#>"
	;; where <#> is 0 ... (:XMN_BUTTON_COUNT - 1).
	;; we use 'read' to return the FIXNUM <#> after truncating the
	;; 7 chars "button_" from the front of the string.
	(case (read (make-string-input-stream
		     (send CALLBACK_ENTRY_WIDGET :name) 7))
	      (0 (format T "Quit Function Called\n"))
	      (1 (format T "Open Function Called\n"))
	      (2 (format T "Open in New Window Function Called\n"))
	      (3 (format T "Save Function Called\n"))
	      (4 (format T "Save As Function Called\n"))
	      (T (format T "Error\n")))
	))


;;
;; do interactive changes
;;

(let*					
    (;; declare and bind local variables
     (children	(send pulldown_w :get_children)) ;XtGetValues(XmNchildren/XmNnumChildren)
     (length	(length children))
     )

  ;; loop through children
  (do
   ;; set up local loop variables and incrementors
   ((i 0 (1+ i)))
   ;; test and return value
   ((<= length i)			
    )
   ;; loop body
   (send (aref children i) :set_values	;XtSetValues() on i-th child
	 :XMN_LABEL_STRING	(format nil "menu: ~A" i)
	 :XMN_MNEMONIC		(format nil "~A" i)
	 :XMN_ACCELERATOR_TEXT	(format nil "^~A" i)
	 :XMN_ACCELERATORS      (format nil "Ctrl<Key>~A: Lisp(print ~A)" i i)
	 )
   )					;end-do
  )					;end-let*


(send (aref (send pulldown_w :get_children) 3) :set_values
      :XMN_SENSITIVE nil)

(send (aref (send pulldown_w :get_children) 4) :destroy)
