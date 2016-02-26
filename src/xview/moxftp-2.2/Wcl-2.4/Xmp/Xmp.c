#include <X11/Xmp/COPY>
/*
* SCCS_data: @(#) Xmp.c	1.5 92/10/28 07:50:36
*
* Xmp - Motif Public Utilities for Wcl - Xmp.c
*
* This file contains Motif specific converters, actions, and convenience
* functions for Motif and Xmp widgets.
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
#include <Xm/RowColumnP.h>	/* RC_Type */
#include <Xm/Text.h>		/* xmTextWidgetClass */
#include <X11/Shell.h>		/* wmShellWidgetClass */
#include <X11/Wc/WcCreateP.h>

#include <X11/Xmp/Table.h>
#include <X11/Xmp/XmpP.h>

/******************************************************************************
* Private_macro_definitions.
******************************************************************************/

/* The `done' macro, used by the converters, is defined in WcCreateP.h */

/******************************************************************************
**  Motif specific resource converters
******************************************************************************/

/*
    -- Convert String To XmString
*******************************************************************************
    This conversion converts strings into XmStrings.  This one overrides
    the one provided by Motif which calls XmCreateString.  Instead, this
    one uses XmCreateStringLtoR which allows multi-line strings to be
    created by embedding "\n" in a resource specification.  In order to
    allow this to be used, XmpRegAll MUST install this converter AFTER
    Motif has installed its broken converter.
*/

void XmpCvtStringToXmString (args, num_args, fromVal, toVal)
    XrmValue *args;
    Cardinal *num_args;
    XrmValue *fromVal;
    XrmValue *toVal;
{
    XmString xmString;
    xmString = XmStringCreateLtoR( fromVal->addr, XmSTRING_DEFAULT_CHARSET );
    done( XmString, xmString );
}

void XmpFixTranslationsACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    static int            firstTime = 1;
    static XtTranslations transTable;
    static char           fixedTranslations[] = "<Key>Return: newline()";

    int     i;

    if (firstTime)
    {
	firstTime = 0;
	transTable = XtParseTranslationTable(fixedTranslations);
    }

    if ( *num_params == 0 )
    {
	WcWARN( w, "XmpFixTranslations", "usage",
		"Usage: XmpFixTranslations( widget [widgetName] ...)");
	return;
    }

    for ( i = 0 ; i < *num_params ; i++ )
    {
	Widget toFix = WcFullNameToWidget( w, params[i] );

        if (!toFix)
        {
	    WcWARN1( w, "XmpFixTranslations", "notFound",
		   "XmpFixTranslations(%s) - Widget not found", params[i] );
	    return;
        }

	if (XtIsSubclass( toFix, xmTextWidgetClass ))
	    XtOverrideTranslations( toFix, transTable );
	else
	{
	    WcWARN1( w, "XmpFixTranslations", "notText",
		   "XmpFixTranslations(%s) - Not an XmText widget", params[i] );
	    return;
        }
    }
}

void XmpFixTranslationsCB( w, widgetNames, ignore )
    Widget	w;
    XtPointer	widgetNames;
    XtPointer	ignore;
{
    WcInvokeAction( XmpFixTranslationsACT, w, widgetNames );
}


/*
    -- Action to popup a Motif PopupMenu
*******************************************************************************
*/

void XmpPopupACT( w, event, params, num_params )
    Widget      w;
    XEvent     *event;
    String     *params;
    Cardinal   *num_params;
{
    Widget      menuPane;

    if (*num_params < 1)
	return;

    if (menuPane = WcFullNameToWidget ( w, params[0] ))
    {
        XmMenuPosition( menuPane, event );
        XtManageChild( menuPane );
    }
}


/*
    -- Provide `Close' callback
*******************************************************************************
    Provide a callback which gets invoked when the user select `Close' from
    the Mwm frame menu on the top level shell.  MUST be done after shell widget
    is realized!
*/

void XmpAddMwmCloseCallback( shell, callback, clientData )
    Widget         shell;
    XtCallbackProc callback;
    XtPointer      clientData;
{
    Atom wmProtocol, wmDeleteWindow, wmSaveYourself;
    wmProtocol    =XmInternAtom( XtDisplay(shell), "WM_PROTOCOLS",     False );

    wmDeleteWindow=XmInternAtom( XtDisplay(shell), "WM_DELETE_WINDOW", False );
    wmSaveYourself=XmInternAtom( XtDisplay(shell), "WM_SAVE_YOURSELF", False );
    XmAddProtocolCallback( shell, wmProtocol, wmDeleteWindow,
			   callback, clientData );
    XmAddProtocolCallback( shell, wmProtocol, wmSaveYourself,
			   callback, clientData );
}

/*
    -- Action and callback to invoke XmpAddMwmCloseCallback
*******************************************************************************
*/
void XmpAddMwmCloseCallbackCB( w, widget_callbackList, notUsed )
    Widget	w;
    XtPointer	widget_callbackList;
    XtPointer	notUsed;
{
    char		widgetName[MAX_XRMSTRING];
    String		callbackListString;
    Widget		target;
    XtCallbackRec*	callbackList;
    XtCallbackRec*	cb;

    callbackListString = WcCleanName( (char*)widget_callbackList, widgetName );
    callbackListString = WcSkipWhitespace_Comma( callbackListString );

    target = WcFullNameToWidget( w, widgetName );

    if ( (Widget)0 == (target = WcFullNameToWidget( w, widgetName )) )
    {
	WcWARN1( w, "XmpAddMwmCloseCallback", "targetNotFound",
		"XmpAddMwmCloseCallback( %s ... ) - Target widget not found",
		widgetName );
	return;
    }

    callbackList = WcStringToCallbackList( w, callbackListString );

    for ( cb = callbackList  ;
	  cb != (XtCallbackRec*)0 && cb->callback != (XtCallbackProc)0  ;
	  cb++ )
	XmpAddMwmCloseCallback( target, cb->callback, cb->closure );

    WcFreeCallbackList( callbackList );
}

void XmpAddMwmCloseCallbackACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    WcInvokeCallback( XmpAddMwmCloseCallbackCB, w, params, num_params );
}

/*  -- Define a Widget as a Tab Group
*******************************************************************************
    If no widget is named, then use the widget argument.
    If the widget is a  manager widget, then this means all the children
    are in a tab group together.  If a widget is a primitive, then that
    widget is in its own tab group (this is the default).
*/

void XmpAddTabGroupACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    if (*num_params == 0)
    {
	XmAddTabGroup(w);
    }
    else if (*num_params == 1)
    {
	Widget target = WcFullNameToWidget( w, params[0] );

	if ((Widget)0 != target)
	{
	    XmAddTabGroup(w);
	}
	else
	{
	    WcWARN1( w, "XmpAddTabGroup", "notFound",
		"XmpAddTabGroup(%s) - widget not found", params[0] );
	}
	
    }
    else
    {
	WcWARN( w, "XmpAddTabGroup", "usage",
		"XmpAddTabGroup() requires 0 or 1 widget name argument.");
    }
}

void XmpAddTabGroupCB( w, widgetName, notUsed )
    Widget	w;
    XtPointer	widgetName;
    XtPointer	notUsed;
{
    WcInvokeAction( XmpAddTabGroupACT,  w, widgetName );
}

/*  -- XmpTable Callbacks and Actions
*******************************************************************************
    These callbacks and actions make the XmpTable convenience functions
    accessible via the Xrm database.
*/

void XmpTableChildConfigCB( w, client, ignored )
    Widget	w;
    XtPointer	client;
    XtPointer	ignored;
{
    WcInvokeAction( XmpTableChildConfigACT, w, client );
}

void XmpTableChildConfigACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    Widget	widget;
    Position	col;
    Position	row;
    Dimension	hSpan = 1;		/* optional arg, default */
    Dimension	vSpan = 1;		/* optional arg, default */
    XmpTableOpts opt  = TBL_DEF_OPT;	/* optional arg, default */

    if (*num_params < 3 || 6 < *num_params )
    {
	WcWARN( w, "XmpTableChildConfig", "wrongNumArgs", 
"XmpTableChildConfig( ... ) Failed - Wrong number of arguments.\n\
\tUsage: XmpTableChildConfig( w col row [h_span [v_span [opts]]] )" );
	return;
    }

    widget = WcFullNameToWidget( w, params[0] );
    col = atoi( params[1] );
    row = atoi( params[2] );

    if ((Widget)0 == widget)
    {
	WcWARN1( w, "XmpTableChildConfig", "widgetNotFound", 
"XmpTableChildConfig( %s ... ) Failed - Could not find widget.",
			params[0] );
	return;
    }
    else if ( ! XtIsSubclass( XtParent(widget), xmpTableWidgetClass ))
    {
	WcWARN1( w, "XmpTableChildConfig", "notXmpTable", 
"XmpTableChildConfig( %s ... ) Failed - Widget must be child of an XmpTable.",
			params[0] );
	return;
    }

    if ( 3 < *num_params )
	hSpan = atoi( params[3] );
    if ( 4 < *num_params )
	vSpan = atoi( params[4] );
    if ( 5 < *num_params )
	opt = XmpTableOptsParse( params[5] );

    XmpTableChildConfig( widget, col, row, hSpan, vSpan, opt );
}

void XmpTableChildPositionCB( w, client, ignored )
    Widget	w;
    XtPointer	client;
    XtPointer	ignored;
{
    WcInvokeAction( XmpTableChildPositionACT, w, client );
}

void XmpTableChildPositionACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    Widget	widget;
    Position	col;
    Position	row;

    if (*num_params != 3 )
    {
	WcWARN( w, "XmpTableChildPosition", "wrongNumArgs", 
"XmpTableChildPosition( ... ) Failed - Wrong number of arguments.\n\
\tUsage: XmpTableChildPosition( w col row )" );
	return;
    }

    widget = WcFullNameToWidget( w, params[0] );
    col = atoi( params[1] );
    row = atoi( params[2] );

    if ((Widget)0 == widget)
    {
	WcWARN1( w, "XmpTableChildPosition", "widgetNotFound", 
"XmpTableChildPosition( %s ... ) Failed - Could not find widget.",
			params[0] );
	return;
    }
    else if ( ! XtIsSubclass( XtParent(widget), xmpTableWidgetClass ))
    {
	WcWARN1( w, "XmpTableChildPosition", "notXmpTable", 
"XmpTableChildPosition( %s ... ) Widget is not a child of an XmpTable.",
			params[0] );
	return;
    }

    XmpTableChildPosition( widget, col, row );
}

void XmpTableChildResizeCB( w, client, ignored )
    Widget	w;
    XtPointer	client;
    XtPointer	ignored;
{
    WcInvokeAction( XmpTableChildResizeACT, w, client );
}

void XmpTableChildResizeACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    Widget	widget;
    Dimension	hSpan;
    Dimension	vSpan;

    if (*num_params != 3 )
    {
	WcWARN( w, "XmpTableChildResize", "wrongNumArgs", 
"XmpTableChildResize( ... ) Failed - Wrong number of arguments.\n\
\tUsage: XmpTableChildResize( w h_span v_span )" );
	return;
    }

    widget = WcFullNameToWidget( w, params[0] );
    hSpan = atoi( params[1] );
    vSpan = atoi( params[2] );

    if ((Widget)0 == widget)
    {
	WcWARN1( w, "XmpTableChildResize", "widgetNotFound", 
"XmpTableChildResize( %s ... ) Failed - Widget not found.",
			params[0] );
	return;
    }
    else if ( ! XtIsSubclass( XtParent(widget), xmpTableWidgetClass ))
    {
	WcWARN1( w, "XmpTableChildResize", "notXmpTable", 
"XmpTableChildResize( %s ... ) Failed - Widget must be child of an XmpTable.",
			params[0] );
	return;
    }

    XmpTableChildResize( widget, hSpan, vSpan );
}

void XmpTableChildOptionsCB( w, client, ignored )
    Widget	w;
    XtPointer	client;
    XtPointer	ignored;
{
    WcInvokeAction( XmpTableChildOptionsACT, w, client );
}

void XmpTableChildOptionsACT( w, unusedEvent, params, num_params )
    Widget      w;
    XEvent     *unusedEvent;
    String     *params;
    Cardinal   *num_params;
{
    Widget 	 widget;
    XmpTableOpts opt;

    if (*num_params != 2 )
    {
	WcWARN( w, "XmpTableChildOptions", "wrongNumArgs", 
"XmpTableChildOptions() Failed - Wrong number of arguments.\n\
\tUsage: XmpTableChildOptions( w options )" );
	return;
    }

    if ((Widget)0 == (widget = WcFullNameToWidget( w, params[0] )) )
    {
	WcWARN1( w, "XmpTableChildOptions", "widgetNotFound", 
"XmpTableChildOptions( %s ... ) Failed - Widget not found.",
			params[0] );
	return;
    }
    else if ( ! XtIsSubclass( XtParent(widget), xmpTableWidgetClass ))
    {
	WcWARN1( w, "XmpTableChildOptions", "notXmpTable", 
"XmpTableChildOptions( %s ... ) Failed - Widget must be child of an XmpTable.",
			params[0] );
	return;
    }

    opt = XmpTableOptsParse( params[2] );
    XmpTableChildOptions( widget, opt );
}
