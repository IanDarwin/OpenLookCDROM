#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcRegXt.c 1.4 92/06/10 06:11:18
*
* Widget Creation Library - WcRegXt.c
*
* This module contains registration routine for all Intrinsic
* widget constructors and classes.  
*
*******************************************************************************
*/

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Vendor.h>

#include <X11/Wc/WcCreateP.h>

/*
*******************************************************************************
** Constructors for Xt Shell Widgets
*******************************************************************************
*/

#define CONSTRUCT( constr, class )				\
Widget constr( pw, name, args, nargs )				\
    Widget	pw;	/* parent of new widget		*/	\
    String	name;	/* name of new widget		*/	\
    Arg*	args;	/* args for new widget		*/	\
    Cardinal	nargs;	/* args count			*/	\
{								\
    return XtCreatePopupShell( name, class, pw, args, nargs);	\
}

CONSTRUCT( WcCreateOverrideShell,	overrideShellWidgetClass	)
CONSTRUCT( WcCreateShell,		shellWidgetClass		)
CONSTRUCT( WcCreateTopLevelShell,	topLevelShellWidgetClass	)
CONSTRUCT( WcCreateTransientShell,	transientShellWidgetClass	)
CONSTRUCT( WcCreateVendorShell,		vendorShellWidgetClass		)
CONSTRUCT( WcCreateWMShell,		wmShellWidgetClass		)

/*  -- Create ApplicationShell 
*******************************************************************************
    Create with argc, argv just like XtAppInitialize
*/

Widget WcCreateApplicationShell( w, name, args, nargs )
    Widget	w;
    String	name;
    Arg*	args;
    Cardinal	nargs;
{
    int i, found;

    for ( i = 0, found = 0 ; i < nargs && !found ; i++ )
	found = WcStrCmp( XtNargc, args[i].name );

    if (!found)
    {
	args = (Arg*)XtRealloc( (char*)args, sizeof(Arg) * (nargs+2) );
	XtSetArg(args[nargs], XtNargc, 1);	++nargs;
	XtSetArg(args[nargs], XtNargv, &name);	++nargs;
    }

    {
	char*	class	= WcAppNameToAppClass( name );
	Widget	shell	= XtAppCreateShell( name, class,
				applicationShellWidgetClass, XtDisplay(w),
				args, nargs );
	XtFree(class);
	return shell;
    }
}

/*  -- Register XtIntrisics Widget Classes and Constructors
*******************************************************************************
*/
void WcRegisterIntrinsic ( app )
    XtAppContext app;
{
    ONCE_PER_XtAppContext( app );

#define RCP( name, class ) WcRegisterClassPtr  ( app, name, class );
#define RCR( name, func )  WcRegisterConstructor(app, name, func  );

    /* -- register all Intrinsic widget classes
    */
    RCP("Object",			(WidgetClass)objectClass );
    RCP("objectClass",			(WidgetClass)objectClass );
    RCP("RectObj",			(WidgetClass)rectObjClass );
    RCP("rectObjClass",			(WidgetClass)rectObjClass );
#ifdef XtSpecificationRelease
    RCP("Core",				coreWidgetClass );
    RCP("coreWidgetClass",		coreWidgetClass );
#else
    RCP("Core",				widgetClass );
    RCP("coreWidgetClass",		widgetClass );
#endif
    RCP("Composite",			compositeWidgetClass );
    RCP("compositeWidgetClass",		compositeWidgetClass );
    RCP("Constraint",			constraintWidgetClass );
    RCP("constraintWidgetClass",	constraintWidgetClass );
    RCP("ApplicationShell",		applicationShellWidgetClass );
    RCP("applicationShellWidgetClass",	applicationShellWidgetClass );
    RCP("OverrideShell",		overrideShellWidgetClass );
    RCP("overrideShellWidgetClass",	overrideShellWidgetClass );
    RCP("Shell",			shellWidgetClass );
    RCP("shellWidgetClass",		shellWidgetClass );
    RCP("TopLevelShell",		topLevelShellWidgetClass );
    RCP("topLevelShellWidgetClass",	topLevelShellWidgetClass );
    RCP("TransientShell",		transientShellWidgetClass );
    RCP("transientShellWidgetClass",	transientShellWidgetClass );
    RCP("VendorShell",			vendorShellWidgetClass );
    RCP("vendorShellWidgetClass",	vendorShellWidgetClass );
    RCP("WmShell",			wmShellWidgetClass );
    RCP("wmShellWidgetClass",		wmShellWidgetClass );

    /* -- register all Intrinsic constructors
    */
    RCR("XtCreateApplicationShell",	WcCreateApplicationShell);
    RCR("XtCreateOverrideShell",	WcCreateOverrideShell);
    RCR("XtCreateShell",		WcCreateShell);
    RCR("XtCreateTopLevelShell",	WcCreateTopLevelShell);
    RCR("XtCreateTransientShell",	WcCreateTransientShell);
    RCR("XtCreateWMShell",		WcCreateWMShell);
    RCR("XtCreateVendorShell",		WcCreateVendorShell);
}
