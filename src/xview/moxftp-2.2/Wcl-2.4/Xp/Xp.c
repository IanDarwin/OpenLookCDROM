#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) Xp.c	1.2 92/10/28 07:53:56
*
*     This module contains registration routine for all Athena
*     widget constructors and classes.  
*
* Module_interface_summary: 
*
*     void XpRegisterAthena  ( XtAppContext app )
*     void XpRegisterAll     ( XtAppContext app )
*     void AriRegisterAthena ( XtAppContext app )
*
*******************************************************************************
*/

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Wc/WcCreateP.h>
#include <X11/Xp/Xp.h>

#ifdef DEBUG
#include <X11/Xp/XpAthenaP.h>
#else
#include <X11/Xp/XpAthena.h>
#endif

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
#define R5
#endif

/* --  Register all Athena widgets, converters, callbacks, actions.
*******************************************************************************
*/

void AriRegisterAthena ( app )
    XtAppContext app;
{
    XpRegisterAthena(app);
}

void XpRegisterAll ( app )
    XtAppContext app;
{
    XpRegisterAthena(app);
}

void XpRegisterAthena ( app )
    XtAppContext app;
{
    ONCE_PER_XtAppContext( app );

/* -- Force Athena to load XmuNewCvtStringToWidget so
 *    WcCvtStringToWidget stays in effect when loaded by WcInitialize()
 */
    XtInitializeWidgetClass( toggleWidgetClass );
    XtInitializeWidgetClass( formWidgetClass );
    
#define RCP( name, class ) WcRegisterClassPtr  ( app, name, class );
#define RCO( name, func )  WcRegisterConstructor(app, name, func  )

    /* -- register all Xp widget classes */
    RCP("XpTable",			xpTableWidgetClass	);
    RCP("xpTableWidgetClass",		xpTableWidgetClass	);

    /* -- register all Athena widget classes */
    /* Simple Widgets (Chapt 3) */
    RCP("Command",			commandWidgetClass	);
    RCP("commandWidgetClass",		commandWidgetClass	);
    RCP("Core",				coreWidgetClass		);
    RCP("coreWidgetClass",		coreWidgetClass		);
    RCP("Grip",				gripWidgetClass		);
    RCP("gripWidgetClass",		gripWidgetClass		);
    RCP("Label",			labelWidgetClass	);
    RCP("labelWidgetClass",		labelWidgetClass	);
    RCP("List",				listWidgetClass		);
    RCP("listWidgetClass",		listWidgetClass		);
    RCP("Object",			objectClass		);
    RCP("objectClass",			objectClass		);
#ifdef R5
    RCP("Panner",			pannerWidgetClass	);
    RCP("pannerWidgetClass",		pannerWidgetClass	);
    RCP("Porthole",			portholeWidgetClass	);
    RCP("portholeWidgetClass",		portholeWidgetClass	);
#endif
    RCP("Rect",				rectObjClass		);
    RCP("rectObjClass",			rectObjClass		);
#ifdef R5
    RCP("Repeater",			repeaterWidgetClass	);
    RCP("repeaterWidgetClass",		repeaterWidgetClass	);
#endif
    RCP("Scrollbar",			scrollbarWidgetClass	);
    RCP("scrollbarWidgetClass",		scrollbarWidgetClass	);
    RCP("Simple",			simpleWidgetClass	);
    RCP("simpleWidgetClass",		simpleWidgetClass	);
    RCP("StripChart",			stripChartWidgetClass	);
    RCP("stripChartWidgetClass",	stripChartWidgetClass	);
    RCP("Toggle",			toggleWidgetClass	);
    RCP("toggleWidgetClass",		toggleWidgetClass	);

    /* Menus (Chapt 4) */
    RCP("SimpleMenu",			simpleMenuWidgetClass	);
    RCP("simpleMenuWidgetClass",	simpleMenuWidgetClass	);
    RCO("XawCreateSimpleMenu",		XpCreateSimpleMenu	);
    RCP("SmeBSB",			smeBSBObjectClass	);
    RCP("smeBSBObjectClass",		smeBSBObjectClass	);
    RCP("SmeLine",			smeLineObjectClass	);
    RCP("smeLineObjectClass",		smeLineObjectClass	);
    RCP("Sme",				smeObjectClass		);
    RCP("smeObjectClass",		smeObjectClass		);
    RCP("MenuButton",			menuButtonWidgetClass	);
    RCP("menuButtonWidgetClass",	menuButtonWidgetClass	);

    /* Text Widgets (Chapt 5) */
    RCP("AsciiText",			asciiTextWidgetClass	); /* NB name */
    RCP("asciiTextWidgetClass",		asciiTextWidgetClass	);
    RCP("AsciiSrc",			asciiSrcObjectClass	);
    RCP("asciiSrcObjectClass",		asciiSrcObjectClass	);
    RCP("AsciiSink",			asciiSinkObjectClass	);
    RCP("asciiSinkObjectClass",		asciiSinkObjectClass	);
    RCP("TextSink",			textSinkObjectClass	);
    RCP("textSinkObjectClass",		textSinkObjectClass	);
    RCP("TextSrc",			textSrcObjectClass	);
    RCP("textSrcObjectClass",		textSrcObjectClass	);
    RCP("Text",				textWidgetClass		);
    RCP("textWidgetClass",		textWidgetClass		);

    /* Composite and Constraint Widgets (Chapt 6) */
    RCP("ApplicationShell",		applicationShellWidgetClass	);
    RCP("applicationShellWidgetClass",	applicationShellWidgetClass	);
    RCP("Box",				boxWidgetClass		);
    RCP("boxWidgetClass",		boxWidgetClass		);
    RCP("Composite",			compositeWidgetClass	);
    RCP("compositeWidgetClass",		compositeWidgetClass	);
    RCP("Constraint",			constraintWidgetClass	);
    RCP("constraintWidgetClass",	constraintWidgetClass	);
    RCP("Dialog",			dialogWidgetClass	);
    RCP("dialogWidgetClass",		dialogWidgetClass	);
    RCP("Form",				formWidgetClass		);
    RCP("formWidgetClass",		formWidgetClass		);
    RCP("OverrideShell",		overrideShellWidgetClass	);
    RCP("overrideShellWidgetClass",	overrideShellWidgetClass	);
    RCP("Paned",			panedWidgetClass	);
    RCP("panedWidgetClass",		panedWidgetClass	);
    RCP("Shell",			shellWidgetClass	);
    RCP("shellWidgetClass",		shellWidgetClass	);
    RCP("TopLevelShell",		topLevelShellWidgetClass	);
    RCP("topLevelShellWidgetClass",	topLevelShellWidgetClass	);
    RCP("TransientShell",		transientShellWidgetClass	);
    RCP("transientShellWidgetClass",	transientShellWidgetClass	);
#ifdef R5
    RCP("Tree",				treeWidgetClass		);
    RCP("treeWidgetClass",		treeWidgetClass		);
#endif
    RCP("VendorShell",			vendorShellWidgetClass	);
    RCP("vendorShellWidgetClass",	vendorShellWidgetClass	);
    RCP("Viewport",			viewportWidgetClass	);
    RCP("viewportWidgetClass",		viewportWidgetClass	);
    RCP("WmShell",			wmShellWidgetClass	);
    RCP("wmShellWidgetClass",		wmShellWidgetClass	);

    /* Other Interestng Widgets (not in ref manual) */
#ifndef R6
    RCP("ClockWidget",			clockWidgetClass	);
    RCP("clockWidgetClass",		clockWidgetClass	);
    RCP("LogoWidget",			logoWidgetClass		);
    RCP("logoWidgetClass",		logoWidgetClass		);
#endif
#if defined(R5)&&!defined(R6)
    RCP("Mailbox",			mailboxWidgetClass	);
    RCP("mailboxWidgetClass",		mailboxWidgetClass	);
#endif
}

/*
    -- Create SimpleMenu as pop-up child
*******************************************************************************
*/

Widget XpCreateSimpleMenu( pw, name, args, nargs )
    Widget      pw;     /* children's parent                            */
    String      name;   /* widget name to create                        */
    Arg        *args;   /* args for widget                              */
    Cardinal    nargs;  /* args count                                   */
{
  return XtCreatePopupShell(name, simpleMenuWidgetClass, pw, args, nargs);
}

/* backward compatibility:
*/
Widget WcCreateSimpleMenu( pw, name, args, nargs )
    Widget      pw;     /* children's parent                            */
    String      name;   /* widget name to create                        */
    Arg        *args;   /* args for widget                              */
    Cardinal    nargs;  /* args count                                   */
{
  return XtCreatePopupShell(name, simpleMenuWidgetClass, pw, args, nargs);
}

/*
    -- Append to end of XawText widget
*******************************************************************************
*/

int XpTextAppend( w, msg )
    Widget w;
    char*  msg;
{
    XawTextBlock textBlock;
    XawTextPosition textPos;

    textPos = XawTextGetInsertionPoint( w );
    textBlock.firstPos = 0;			/* start at ptr */
    textBlock.length = strlen( msg );
    textBlock.ptr = msg;
    textBlock.format = FMT8BIT;
    return XawTextReplace( w, textPos, textPos, &textBlock );
}
