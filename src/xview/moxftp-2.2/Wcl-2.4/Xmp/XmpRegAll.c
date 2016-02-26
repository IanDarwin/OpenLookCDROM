#include <X11/Wc/COPY>
/*
* SCCS_data: @(#) XmpRegAll.c	1.4 92/10/28 07:50:41
*
*	This module contains registration routine for all Motif
*	widget/gadget constructors and classes.  
*
* Module_interface_summary: 
*
* All are exactly equivalent.  Register all Motif widgets and Xmp widgets.
*
*    void XmpRegisterMotif ( XtAppContext app )
*    void XmpRegisterAll   ( XtAppContext app )
*    void MriRegisterMotif ( XtAppContext app )
*
*******************************************************************************
*/

#include <X11/IntrinsicP.h>

#ifdef sun
#include <X11/ObjectP.h>        /* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include <X11/Wc/WcCreate.h>
#include <X11/Xmp/XmpP.h>

#ifdef DEBUG
#include <X11/Xmp/XmpMotifP.h>
#else
#include <X11/Xmp/XmpMotif.h>
#endif


/* -- Register all Motif widgets, converters, callbacks, actions.
*******************************************************************************
*/

void MriRegisterMotif ( app )
    XtAppContext app;
{
    XmpRegisterMotif(app);
}

void XmpRegisterAll ( app )
    XtAppContext app;
{
    XmpRegisterMotif(app);
}

void XmpRegisterMotif ( app )
    XtAppContext app;
{
    static XtActionsRec XmpActions[] = {
	{"XmpPopup",			XmpPopupACT			},
	{"XmpPopupACT",			XmpPopupACT			},
	{"XmpFixTranslations",		XmpFixTranslationsACT		},
	{"XmpFixTranslationsACT",	XmpFixTranslationsACT		},
	{"XmpAddMwmCloseCallback",	XmpAddMwmCloseCallbackACT	},
	{"XmpAddMwmCloseCallbackACT",	XmpAddMwmCloseCallbackACT	},
	{"XmpAddTabGroup",		XmpAddTabGroupACT		},
	{"XmpAddTabGroupACT",		XmpAddTabGroupACT		},
	{"XmpTableChildConfig",		XmpTableChildConfigACT		},
	{"XmpTableChildConfigACT",	XmpTableChildConfigACT		},
	{"XmpTableChildPosition",	XmpTableChildPositionACT	},
	{"XmpTableChildPositionACT",	XmpTableChildPositionACT	},
	{"XmpTableChildResize",		XmpTableChildResizeACT		},
	{"XmpTableChildResizeACT",	XmpTableChildResizeACT		},
	{"XmpTableChildOptions",	XmpTableChildOptionsACT		},
	{"XmpTableChildOptionsACT",	XmpTableChildOptionsACT		},
	/* -- compatibility: */
	{"MriPopup",			XmpPopupACT			},
	{"MriPopupACT",			XmpPopupACT			},
    };

    ONCE_PER_XtAppContext( app );

/* -- Register Motif specific action functions */
    XtAppAddActions(app, XmpActions, XtNumber(XmpActions));

/* -- Register Motif specific callback functions */
#define RCALL( name, func ) WcRegisterCallback ( app, name, func, (XtPointer)0 )

    RCALL("XmpFixTranslations",		XmpFixTranslationsCB		);
    RCALL("XmpFixTranslationsCB",	XmpFixTranslationsCB		);
    RCALL("XmpAddMwmCloseCallback",	XmpAddMwmCloseCallbackCB	);
    RCALL("XmpAddMwmCloseCallbackCB",	XmpAddMwmCloseCallbackCB	);
    RCALL("XmpAddTabGroup",		XmpAddTabGroupCB		);
    RCALL("XmpAddTabGroupCB",		XmpAddTabGroupCB		);
    RCALL("XmpTableChildConfig",	XmpTableChildConfigCB		);
    RCALL("XmpTableChildConfigCB",	XmpTableChildConfigCB		);
    RCALL("XmpTableChildPosition",	XmpTableChildPositionCB		);
    RCALL("XmpTableChildPositionCB",	XmpTableChildPositionCB		);
    RCALL("XmpTableChildResize",	XmpTableChildResizeCB		);
    RCALL("XmpTableChildResizeCB",	XmpTableChildResizeCB		);
    RCALL("XmpTableChildOptions",	XmpTableChildOptionsCB		);
    RCALL("XmpTableChildOptionsCB",	XmpTableChildOptionsCB		);

/* -- Force Motif to load all of its converters */
    XmRegisterConverters();
    _XmRegisterPixmapConverters();
    XtAddConverter( XmRString, XmRUnitType, XmCvtStringToUnitType, NULL, 0 );


/* -- Register Motif specific converters */

    /* Some, not all, old versions of Motif incorrectly say some resources
    ** which should be of type "Widget" are of type "Window"
    */
    XtAddConverter       (XtRString, "Window",
                          WcCvtStringToWidget,
                          wcWidgetCvtArgs, wcWidgetCvtArgsCount );

    XtAddConverter       (XtRString, XmRMenuWidget,
                          WcCvtStringToWidget,
                          wcWidgetCvtArgs, wcWidgetCvtArgsCount );

    XtAddConverter       (XtRString, XmRXmString,
                          XmpCvtStringToXmString,
                          (XtConvertArgList)0, (Cardinal)0);

#define RCO( name, func  )  WcRegisterConstructor ( app, name, func  )
#define RCP( name, class )  WcRegisterClassPtr    ( app, name, class )

/* -- register all Xmp widget constructors */
 RCO( "XmpCreateTable",			XmpCreateTable			);
 RCO( "XmpCreateTableDialog",		XmpCreateTableDialog		);
 RCO( "XmpCreateTableTransient",	XmpCreateTableTransient		);

/* -- register all Xmp widget classes */
 RCP( "XmpTable",			xmpTableWidgetClass		);
 RCP( "xmpTableWidgetClass",		xmpTableWidgetClass		);

/* -- register all Motif constructors */
 RCO( "XmCreateArrowButton",		XmCreateArrowButton		);
 RCO( "XmCreateArrowButtonGadget",	XmCreateArrowButtonGadget	);
 RCO( "XmCreateBulletinBoard",		XmCreateBulletinBoard		);
 RCO( "XmCreateBulletinBoardDialog",	XmCreateBulletinBoardDialog	);
 RCO( "XmCreateCascadeButton",		XmCreateCascadeButton		);
 RCO( "XmCreateCascadeButtonGadget",	XmCreateCascadeButtonGadget	);
#ifndef _OLD_MOTIF
 RCO( "XmCreateCheckBox",		XmCreateSimpleCheckBox		);
#endif
 RCO( "XmCreateCommand",		XmCreateCommand			);
 RCO( "XmCreateDialogShell",		XmCreateDialogShell		);
 RCO( "XmCreateDrawingArea",		XmCreateDrawingArea		);
 RCO( "XmCreateDrawnButton",		XmCreateDrawnButton		);
 RCO( "XmCreateErrorDialog",		XmCreateErrorDialog		);
 RCO( "XmCreateFileSelectionBox",	XmCreateFileSelectionBox	);
 RCO( "XmCreateFileSelectionDialog",	XmCreateFileSelectionDialog	);
 RCO( "XmCreateForm",			XmCreateForm			);
 RCO( "XmCreateFormDialog",		XmCreateFormDialog		);
 RCO( "XmCreateFrame",			XmCreateFrame			);
 RCO( "XmCreateInformationDialog",	XmCreateInformationDialog	);
 RCO( "XmCreateLabel",			XmCreateLabel			);
 RCO( "XmCreateLabelGadget",		XmCreateLabelGadget		);
 RCO( "XmCreateList",			XmCreateList			);
 RCO( "XmCreateMainWindow",		XmCreateMainWindow		);
 RCO( "XmCreateMenuBar",		XmCreateMenuBar			);
 RCO( "XmCreateMenuShell",		XmCreateMenuShell		);
 RCO( "XmCreateMessageBox",		XmCreateMessageBox		);
 RCO( "XmCreateMessageDialog",		XmCreateMessageDialog		);
 RCO( "XmCreateOptionMenu",		XmCreateOptionMenu		);
 RCO( "XmCreatePanedWindow",		XmCreatePanedWindow		);
 RCO( "XmCreatePopupMenu",		XmCreatePopupMenu		);
 RCO( "XmCreatePromptDialog",		XmCreatePromptDialog		);
 RCO( "XmCreatePulldownMenu",		XmCreatePulldownMenu		);
 RCO( "XmCreatePushButton",		XmCreatePushButton		);
 RCO( "XmCreatePushButtonGadget",	XmCreatePushButtonGadget	);
 RCO( "XmCreateQuestionDialog",		XmCreateQuestionDialog		);
 RCO( "XmCreateRadioBox",		XmCreateRadioBox		);
 RCO( "XmCreateRowColumn",		XmCreateRowColumn		);
 RCO( "XmCreateScale",			XmCreateScale			);
 RCO( "XmCreateScrollBar",		XmCreateScrollBar		);
 RCO( "XmCreateScrolledList",		XmCreateScrolledList		);
 RCO( "XmCreateScrolledText",		XmCreateScrolledText		);
 RCO( "XmCreateScrolledWindow",		XmCreateScrolledWindow		);
 RCO( "XmCreateSelectionBox",		XmCreateSelectionBox		);
 RCO( "XmCreateSelectionDialog",	XmCreateSelectionDialog		);
 RCO( "XmCreateSeparator",		XmCreateSeparator		);
 RCO( "XmCreateSeparatorGadget",	XmCreateSeparatorGadget		);
#ifndef _OLD_MOTIF
 RCO( "XmCreateSimpleCheckBox",		XmCreateSimpleCheckBox		);
#endif
 RCO( "XmCreateText",			XmCreateText			);
#ifndef _OLD_MOTIF
 RCO( "XmCreateTextField",		XmCreateTextField		);
 RCO( "XmCreateToggleButton",		XmCreateToggleButton		);
#endif
 RCO( "XmCreateToggleButtonGadget",	XmCreateToggleButtonGadget	);
 RCO( "XmCreateWarningDialog",		XmCreateWarningDialog		);
 RCO( "XmCreateWorkingDialog",		XmCreateWorkingDialog		);

/* -- register Motif widget classes */
 RCP("xmArrowButtonWidgetClass",	xmArrowButtonWidgetClass	);
 RCP("XmArrowButton",			xmArrowButtonWidgetClass	);
 RCP("xmArrowButtonGadgetClass",	xmArrowButtonGadgetClass	);
 RCP("XmArrowButtonGadget",		xmArrowButtonGadgetClass	);
 RCP("xmBulletinBoardWidgetClass",	xmBulletinBoardWidgetClass	);
 RCP("XmBulletinBoard",			xmBulletinBoardWidgetClass	);
 RCP("xmCascadeButtonWidgetClass",	xmCascadeButtonWidgetClass	);
 RCP("XmCascadeButton",			xmCascadeButtonWidgetClass	);
 RCP("xmCascadeButtonGadgetClass",	xmCascadeButtonGadgetClass	);
 RCP("XmCascadeButtonGadget",		xmCascadeButtonGadgetClass	);
 RCP("xmCommandWidgetClass",		xmCommandWidgetClass		);
 RCP("XmCommand",			xmCommandWidgetClass		);
 RCP("xmDialogShellWidgetClass",	xmDialogShellWidgetClass	);
 RCP("XmDialogShell",			xmDialogShellWidgetClass	);
 RCP("xmDrawingAreaWidgetClass",	xmDrawingAreaWidgetClass	);
 RCP("XmDrawingArea",			xmDrawingAreaWidgetClass	);
 RCP("xmDrawnButtonWidgetClass",	xmDrawnButtonWidgetClass	);
 RCP("XmDrawnButton",			xmDrawnButtonWidgetClass	);
 RCP("xmFileSelectionBoxWidgetClass",	xmFileSelectionBoxWidgetClass	);
 RCP("XmFileSelectionBox",		xmFileSelectionBoxWidgetClass	);
 RCP("xmFormWidgetClass",		xmFormWidgetClass		);
 RCP("XmForm",				xmFormWidgetClass		);
 RCP("xmFrameWidgetClass",		xmFrameWidgetClass		);
 RCP("XmFrame",				xmFrameWidgetClass		);
 RCP("xmGadgetClass",			xmGadgetClass			);
 RCP("XmGadget",			xmGadgetClass			);
 RCP("xmLabelWidgetClass",		xmLabelWidgetClass		);
 RCP("XmLabel",				xmLabelWidgetClass		);
 RCP("xmLabelGadgetClass",		xmLabelGadgetClass		);
 RCP("XmLabelGadget",			xmLabelGadgetClass		);
 RCP("xmListWidgetClass",		xmListWidgetClass		);
 RCP("XmList",				xmListWidgetClass		);
 RCP("xmMainWindowWidgetClass",		xmMainWindowWidgetClass		);
 RCP("XmMainWindow",			xmMainWindowWidgetClass		);
 RCP("xmManagerWidgetClass",		xmManagerWidgetClass		);
 RCP("XmManager",			xmManagerWidgetClass		);
 RCP("xmMenuShellWidgetClass",		xmMenuShellWidgetClass		);
 RCP("XmMenuShell",			xmMenuShellWidgetClass		);
 RCP("xmMessageBoxWidgetClass",		xmMessageBoxWidgetClass		);
 RCP("XmMessageBox",			xmMessageBoxWidgetClass		);
 RCP("xmPanedWindowWidgetClass",	xmPanedWindowWidgetClass	);
 RCP("XmPanedWindow",			xmPanedWindowWidgetClass	);
 RCP("xmPrimitiveWidgetClass",		xmPrimitiveWidgetClass		);
 RCP("XmPrimitive",			xmPrimitiveWidgetClass		);
 RCP("xmPushButtonWidgetClass",		xmPushButtonWidgetClass		);
 RCP("XmPushButton",			xmPushButtonWidgetClass		);
 RCP("xmPushButtonGadgetClass",		xmPushButtonGadgetClass		);
 RCP("XmPushButtonGadget",		xmPushButtonGadgetClass		);
 RCP("xmRowColumnWidgetClass",		xmRowColumnWidgetClass		);
 RCP("XmRowColumn",			xmRowColumnWidgetClass		);
 RCP("xmScaleWidgetClass",		xmScaleWidgetClass		);
 RCP("XmScale",				xmScaleWidgetClass		);
 RCP("xmScrollBarWidgetClass",		xmScrollBarWidgetClass		);
 RCP("XmScrollBar",			xmScrollBarWidgetClass		);
 RCP("xmScrolledWindowWidgetClass",	xmScrolledWindowWidgetClass	);
 RCP("XmScrolledWindow",		xmScrolledWindowWidgetClass	);
 RCP("xmSelectionBoxWidgetClass",	xmSelectionBoxWidgetClass	);
 RCP("XmSelectionBox",			xmSelectionBoxWidgetClass	);
 RCP("xmSeparatorWidgetClass",		xmSeparatorWidgetClass		);
 RCP("XmSeparator",			xmSeparatorWidgetClass		);
 RCP("xmSeparatorGadgetClass",		xmSeparatorGadgetClass		);
 RCP("XmSeparatorGadget",		xmSeparatorGadgetClass		);
 RCP("xmTextWidgetClass",		xmTextWidgetClass		);
 RCP("XmText",				xmTextWidgetClass		);
#ifndef _OLD_MOTIF
 RCP("xmTextFieldWidgetClass",		xmTextFieldWidgetClass		);
 RCP("XmTextField",			xmTextFieldWidgetClass		);
#endif
 RCP("xmToggleButtonWidgetClass",	xmToggleButtonWidgetClass	);
 RCP("XmToggleButton",			xmToggleButtonWidgetClass	);
 RCP("xmToggleButtonGadgetClass",	xmToggleButtonGadgetClass	);
 RCP("XmToggleButtonGadget",		xmToggleButtonGadgetClass	);
}
