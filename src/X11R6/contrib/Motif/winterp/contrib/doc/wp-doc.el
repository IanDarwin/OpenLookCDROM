;;!emacs
;;
;; FILE:         wp-doc.el
;; SUMMARY:      Provide completion and documentation lookup for Winterp.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Brown University
;; ORIG-DATE:    19-Dec-90
;; LAST-MOD:     17-Sep-91 at 22:22:04 by Bob Weiner
;;
;; Copyright (C) 1990, 1991 Bob Weiner
;; WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
;; XLISP version 2.1, Copyright (c) 1989, by David Betz.
;;
;; Permission to use, copy, modify, distribute, and sell this software and
;; its documentation for any purpose is hereby granted without fee,
;; provided that the above copyright notice appear in all copies and that
;; both that copyright notice and this permission notice appear in
;; supporting documentation, and that the name of Hewlett-Packard, Niels
;; Mayer, Brown University and Bob Weiner not be used in advertising or
;; publicity pertaining to distribution of the software without specific,
;; written prior permission.  Hewlett-Packard, Niels Mayer, Brown University
;; and Bob Weiner makes no representations about the suitability of this
;; software for any purpose.  It is provided "as is" without express or
;; implied warranty.
;;
;; This file is not part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   The function (rd-doc) takes a Motif widget class as input and displays the
;;   corresponding WINTERP manual section.  Parent entries which are matched
;;   are extracted with all of their children.
;;
;;   (wp-doc) is bound to {C-c w} below, change it if you like.
;;   You might also want to alter the keybinding in (rd-doc-init).
;;
;;   You MUST set 'rd-doc-files' within the function (wp-doc-init) below.
;;   Set it to a list of the path for your own WINTERP documentation file.
;;
;; DESCRIP-END.
;;

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(require 'rd-doc)

;; ************************************************************************
;; Public variables
;; ************************************************************************

;;
;;  Select your own key.
;;
(defvar wp-doc-key "\C-cw"
  "*Key binding with which to invoke 'wp-doc'.
Nil means no binding.")

;; ************************************************************************
;; Private functions
;; ************************************************************************

(defun wp-doc-init ()
  "Setup for WINTERP doc searching."
  (if (or (equal (user-login-name) "rsw")
	  (equal (user-login-name) "weiner"))
      (progn (load "/u/rsw/.em-X-sun")
	     ;; Bind the Help key on my Sun keypad
	     (setq wp-doc-key "\e[-1z")
	     (and wp-doc-key (global-set-key wp-doc-key 'rd-doc))))
  ;;
  ;;  Set your own path.
  ;;
  (setq rd-doc-files '("/cs/src/winterp/w/doc/winterp.doc")
	rd-doc-buf "*Winterp Help*"
	rd-doc-list wp-doc-list
	rd-identifier-chars "^]()[{}. \t\n"
	rd-identifier "[^]()[{}. \t\n]+")
  )

;; ************************************************************************
;; Private variables
;; ************************************************************************

(defconst wp-doc-list
'(
("<POPUP_SHELL_WIDGET_CLASS>")
("<SHELL_WIDGET_CLASS>")
("<WIDGET_CLASS>")
("<WINTERP>")
("<X11/PassivGrab.h>")
("<X11/Protocols.h>")
("<X11/Selection.h>")
("<Xm/CutPaste.h>")
("Colorpicker:")
("Motif")
("Widget")
("WidgetClass")
("XM_ARROW_BUTTON_GADGET_CLASS")
("XM_ARROW_BUTTON_WIDGET_CLASS")
("XM_BULLETIN_BOARD_WIDGET_CLASS")
("XM_CASCADE_BUTTON_GADGET_CLASS")
("XM_CASCADE_BUTTON_WIDGET_CLASS")
("XM_COMMAND_WIDGET_CLASS")
("XM_CONVERT_UNITS:") ;; [Motif 1.1 only]
("XM_CVT_CT_TO_XM_STRING::") ;; [Motif 1.1 only]
("XM_CVT_XM_STRING_TO_CT::") ;; [Motif 1.1 only]
("XM_DRAWING_AREA_WIDGET_CLASS")
("XM_DRAWN_BUTTON_WIDGET_CLASS")
("XM_FILE_SELECTION_BOX_WIDGET_CLASS")
("XM_FORM_WIDGET_CLASS")
("XM_FRAME_WIDGET_CLASS")
("XM_GET_COLORS::") ;; [Motif 1.1 only]
("XM_GET_XIMAGE_FROM_FILE")
("XM_INSTALL_IMAGE")
("XM_LABEL_GADGET_CLASS")
("XM_LABEL_WIDGET_CLASS")
("XM_LIST_WIDGET_CLASS")
("XM_MAIN_WINDOW_WIDGET_CLASS")
("XM_MESSAGE_BOX_WIDGET_CLASS")
("XM_PANED_WINDOW_WIDGET_CLASS")
("XM_PUSH_BUTTON_GADGET_CLASS")
("XM_PUSH_BUTTON_WIDGET_CLASS")
("XM_ROW_COLUMN_WIDGET_CLASS")
("XM_SCALE_WIDGET_CLASS")
("XM_SCROLLED_WINDOW_WIDGET_CLASS")
("XM_SCROLL_BAR_WIDGET_CLASS")
("XM_SELECTION_BOX_WIDGET_CLASS")
("XM_SEPARATOR_GADGET_CLASS")
("XM_SEPARATOR_WIDGET_CLASS")
("XM_SET_FONT_UNITS::") ;; [Motif 1.1 only]
("XM_STRING_BYTE_COMPARE")
("XM_STRING_COMPARE")
("XM_STRING_CONCAT")
("XM_STRING_COPY")
("XM_STRING_CREATE")
("XM_STRING_CREATE_L_TO_R")
("XM_STRING_DIRECTION_CREATE")
("XM_STRING_EMPTY")
("XM_STRING_GET_L_TO_R")
("XM_STRING_HAS_SUBSTRING::") ;; [Motif 1.1 only]
("XM_STRING_LENGTH")
("XM_STRING_LINE_COUNT")
("XM_STRING_SEGMENT_CREATE")
("XM_STRING_SEPARATOR_CREATE")
("XM_TEXT_WIDGET_CLASS")
("XM_TOGGLE_BUTTON_GADGET_CLASS")
("XM_TOGGLE_BUTTON_WIDGET_CLASS")
("XM_TRACKING_LOCATE::") ;; [Motif 1.1 only]
("XM_UNINSTALL_IMAGE")
("XT_ADD_TIMEOUT")
("XT_MANAGE_CHILDREN")
("XT_PARSE_TRANSLATION_TABLE")
("XT_REMOVE_TIMEOUT")
("XT_RESOLVE_PATHNAME::") ;; [Motif 1.1 only]
("XT_UNMANAGE_CHILDREN")
("XmAddTabGroup():")
("XmCascadeButtonHighlight():")
("XmCommandAppendValue():")
("XmCommandError():")
("XmCommandGetChild():")
("XmCommandSetValue():")
("XmFileSelectionBoxGetChild():")
("XmFileSelectionDoSearch():")
("XmFontList")
("XmGetPostedFromWidget()::") ;; [Motif 1.1 only]
("XmInstallImage():")
("XmIsGadget():")
("XmIsManager():")
("XmIsMotifWMRunning():")
("XmIsPrimitive():")
("XmListAddItem()")
("XmListAddItemUnselected():")
("XmListAddItems():") ;; [Motif 1.1 only]
("XmListDeleteAllItems():") ;; [Motif 1.1 only]
("XmListDeleteItem():")
("XmListDeleteItems():") ;; [Motif 1.1 only]
("XmListDeleteItemsPos():") ;; [Motif 1.1 only]
("XmListDeletePos():")
("XmListDeselectAllItems():")
("XmListDeselectItem():")
("XmListDeselectPos():")
("XmListGetMatchPos():") ;; [Motif 1.1 only]
("XmListGetSelectedPos():") ;; [Motif 1.1 only]
("XmListItemExists():")
("XmListItemPos():") ;; [Motif 1.1 only]
("XmListReplaceItems():") ;; [Motif 1.1 only]
("XmListReplaceItemsPos():") ;; [Motif 1.1 only]
("XmListSelectItem():")
("XmListSelectPos():")
("XmListSetAddMode():") ;; [Motif 1.1 only]
("XmListSetBottomItem():")
("XmListSetBottomPos():")
("XmListSetHorizPos():")
("XmListSetItem():")
("XmListSetPos():")
("XmMainWindowSep1():")
("XmMainWindowSep2():")
("XmMainWindowSep3()::") ;; [Motif 1.1 only]
("XmMainWindowSetAreas():")
("XmMenuPosition():")
("XmMessageBoxGetChild():")
("XmOptionButtonGadget():")
("XmOptionLabelGadget():")
("XmProcessTraversal():") ;; [Motif 1.1 only]
("XmRAcceleratorTable:")
("XmRAlignment:")
("XmRArrowDirection:")
("XmRAttachment:")
("XmRBoolean:")
("XmRButtonType:") ;; [MOTIF_1.1 only]
("XmRCallback:")
("XmRChar:")
("XmRChar:") ;; [MOTIF 1.1 only]
("XmRDefaultButtonType:")
("XmRDeleteResponse:")
("XmRDialogStyle:")
("XmRDialogType:")
("XmRDimension:")
("XmREditMode:")
("XmRFileTypeMask::") ;; [MOTIF 1.1 only]
("XmRFontList:")
("XmRHorizontalDimension::") ;; [MOTIF 1.1 only]
("XmRHorizontalInt::") ;; [MOTIF 1.1 only]
("XmRHorizontalPosition::") ;; [Motif 1.1 only]
("XmRIndicatorType:")
("XmRInitialState::") ;; [MOTIF 1.1 only]
("XmRInt:")
("XmRKeyboardFocusPolicy:")
("XmRLabelType:")
("XmRListSizePolicy:")
("XmRManForegroundPixmap:")
("XmRManHighlightPixmap:")
("XmRMultiClick::") ;; [Motif 1.1 only]
("XmRNavigationType::") ;; [Motif 1.1 only]
("XmROrientation:")
("XmRPacking:")
("XmRPixel:")
("XmRPixmap:")
("XmRPrimForegroundPixmap:")
("XmRPrimHighlightPixmap:")
("XmRProcessingDirection:")
("XmRScrollBarDisplayPolicy:")
("XmRScrollBarPlacement:")
("XmRScrollingPolicy:")
("XmRSelectionPolicy:")
("XmRSeparatorType:")
("XmRShadowType:")
("XmRTextPosition::") ;; [MOTIF 1.1 only]
("XmRVerticalDimension::") ;; [MOTIF 1.1 only]
("XmRVerticalInt::") ;; [MOTIF 1.1 only]
("XmRVerticalPosition::") ;; [Motif 1.1 only]
("XmRVisualPolicy:")
("XmRWidgetClass:")
("XmRWindowGravity::") ;; [MOTIF 1.1 only]
("XmRXmString:")
("XmRemoveTabGroup():")
("XmScaleGetValue():")
("XmScaleSetValue():")
("XmScrollBarGetValues():")
("XmScrollBarSetValues():")
("XmScrolledWindowSetAreas():")
("XmSelectionBoxGetChild():")
("XmString")
("XmStringByteCompare():")
("XmStringCompare():")
("XmStringConcat():")
("XmStringCopy():")
("XmStringCreate():")
("XmStringCreateLtoR():")
("XmStringDirectionCreate():")
("XmStringEmpty():")
("XmStringGetLtoR():")
("XmStringLength():")
("XmStringLineCount():")
("XmStringSegmentCreate():")
("XmStringSeparatorCreate():")
("XmTextClearSelection():")
("XmTextCopy():") ;; [Motif 1.1 only]
("XmTextCut()::") ;; [Motif 1.1 only]
("XmTextDisableRedisplay():")
("XmTextDisableRedisplay():") ;; [MOTIF_1.0]
("XmTextEnableRedisplay():")
("XmTextEnableRedisplay():") ;; [MOTIF_1.0]
("XmTextGetAddMode()::") ;; [Motif 1.1 only]
("XmTextGetBaseline()::") ;; [Motif 1.1 only]
("XmTextGetCursorPosition()::") ;; [Motif 1.1 only]
("XmTextGetEditable():")
("XmTextGetInsertionPosition():")
("XmTextGetLastPosition():")
("XmTextGetMaxLength():")
("XmTextGetSelection():")
("XmTextGetSelectionPosition():")
("XmTextGetSource():")
("XmTextGetString():")
("XmTextGetTopPosition():")
("XmTextInsert()::") ;; [Motif 1.1 only]
("XmTextPaste()::") ;; [Motif 1.1 only]
("XmTextPosToXY():")
("XmTextRemove()::") ;; [Motif 1.1 only]
("XmTextReplace():")
("XmTextScroll():")
("XmTextSetAddMode()::") ;; [Motif 1.1 only]
("XmTextSetCursorPosition()::") ;; [Motif 1.1 only]
("XmTextSetEditable():")
("XmTextSetHighlight()::") ;; [Motif 1.1 only]
("XmTextSetInsertionPosition():")
("XmTextSetMaxLength():")
("XmTextSetSelection():")
("XmTextSetSource():")
("XmTextSetString():")
("XmTextSetTopPosition():")
("XmTextShowPosition():")
("XmTextXYToPos():")
("XmToggleButtonGadgetGetState():")
("XmToggleButtonGadgetSetState():")
("XmToggleButtonGetState()")
("XmToggleButtonSetState()")
("XmUninstallImage():")
("XmUpdateDisplay():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddCallback():")
("XtAddEventHandler()")
("XtAddGrab():")
("XtAddInput()")
("XtAddRawEventHandler():")
("XtAppAddTimeout()")
("XtAppAddWorkProc()")
("XtAppCreateShell():")
("XtAppRemoveWorkProc():")
("XtAugmentTranslations():")
("XtBuildEventMask():")
("XtCallActionProc()::") ;; [Motif 1.1 only]
("XtCreatePopupShell():")
("XtCreateWidget():")
("XtDestroyWidget():")
("XtGetValues():")
("XtGetValues()::") ;; [MOTIF_1.1 only]
("XtGetValues()::") ;; [MOTIF_1.1 only]
("XtGetValues()::") ;; [Motif 1.1 only -- Motif bug work-around]
("XtGetValues()::") ;; [Motif 1.1 only -- Motif bug work-around]
("XtGetValues()::") ;; [Motif 1.1 only]
("XtInstallAccelerators():")
("XtInstallAllAccelerators():")
("XtIsComposite()::") ;; [MOTIF_1.1]
("XtIsCompositeObject():")
("XtIsCompositeObject()::") ;; [MOTIF_1.0]
("XtIsConstraint():")
("XtIsManaged():")
("XtIsRealized():")
("XtIsSensitive():")
("XtIsShell():")
("XtManageChild():")
("XtManageChildren():")
("XtMapWidget():")
("XtName()::") ;; [Motif 1.1 only]
("XtOverrideTranslations():")
("XtParent():")
("XtParseTranslationTable():")
("XtPopDown():")
("XtPopup():")
("XtRealizeWidget():")
("XtRemoveAllCallbacks():")
("XtRemoveEventHandler()")
("XtRemoveGrab():")
("XtRemoveInput():")
("XtRemoveRawEventHandler()")
("XtRemoveTimeout()")
("XtSetMappedWhenManaged():")
("XtSetSensitive():")
("XtSetValues():")
("XtUninstallTranslations():")
("XtUnmanageChild():")
("XtUnmanageChildren():")
("XtUnmapWidget():")
("XtUnrealizeWidget():")
("XtWindow():")
("XtWindow()::") ;; [MOTIF_1.0]
("XtWindowOfObject()::") ;; [MOTIF_1.1]
("_XmGetImageFromFile():")
("_XmTextDisableRedisplay():") ;; [MOTIF_1.1]
("_XmTextEnableRedisplay()::") ;; [MOTIF_1.1]
("browser:")
("palette")
)
"List of single entry lists of Winterp symbol names and related strings.
This is the format used by the winterp name completion function.")

;; ************************************************************************
;; Function calls
;; ************************************************************************

(wp-doc-init)

(provide 'wp-doc)


