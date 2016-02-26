#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcReg.c 1.3 92/06/10 06:11:17
*
* Widget Creation Library - WcReg.c
*
* The registration functions are used to map strings, which may then appear in
* the Xrm database, and data: the name of a widget class and the class pointer,
* the name of a constructor and a pointer to the constructor function, and the
* name of a callback and the XtCallbackRec (ptr to func and client data).
*
* The registration functions store information using Mapping Agents.
*
* The class, constructor, and callbacks are then retreived by
* WcCvtStringToClassPtr, WcCvtStringToConstr, and WcCvtStringToCallback
* as the widget using such values is being created (when its resources are
* being fetched during pre-creation or post-creation processing by Wcl, and
* during the widgets own Initialize method as it is initializing its own 
* fields).
*
* DIFFERENCES FROM PREVIOUS Wcl VERSIONS: 
*	o  Duplicates are not a problem, they replace previous registration.
*
*******************************************************************************
*/

#include <X11/Intrinsic.h>
#include <X11/Wc/WcCreateP.h>

/*  -- Register Class Pointer Name
*******************************************************************************
    This procedure adds a mapping from a name to a class pointer.  The
    name is case insensitive .  It is suggested that two names are provided 
    for each class pointer: the name of the pointer as it would appear in
    C code in a call to XtCreate(), and the name of the widget class:

   WcRegisterClassPtr( app, "xmPushButtonWidgetClass", xmPushButtonWidgetClass);
   WcRegisterClassPtr( app, "XmPushButton",            xmPushButtonWidgetClass);
*/

void WcRegisterClassPtr ( app, name, class )
    XtAppContext	app;
    char*		name;
    WidgetClass		class;
{
    WcMapClass( app, WcStringToQuark(name), class );
}

void WcRegisterClassName ( app, name, class )
    XtAppContext	app;
    char*		name;
    WidgetClass		class;
{
    WcMapClass( app, WcStringToQuark(name), class );
}

/*  -- Register constructor
*******************************************************************************
    This procedure adds a mappign between a name and a "Motif Style"
    constructor.  The names are CASE INSENSITIVE.
*/

void WcRegisterConstructor( app, name, Constructor )
    XtAppContext	app;
    char*		name;
    WcWidgetConstructor	Constructor;
{
    WcMapConstructor( app, WcStringToQuark(name), Constructor );
}

/*  -- Register dynamic libraries
*******************************************************************************
    Argument: one or more (whitespace or comma separated) full path name of
    dynamic (shared) library.

    Shared libraries are remembered using a mapping agent.  The library
    is accessed using the ld style abbreviation, including the "-l".  I.e.,
    "/usr/openwin/lib/libXol.so.3.0" is accessed by "-lXol".

    Dynamic libraries are used by the late binder.
*/
void WcRegisterDynamicLibs( app, libNames )
    XtAppContext app;
    char*	 libNames;
{
    char	*libPath, libPathBuf[MAX_XRMSTRING], *permLibPath;
    char	*dashL,   dashLBuf[MAX_XRMSTRING];
    char*	name;
    char*	cp = libNames;

    /* dashL always begins with "-l"
    */
    dashLBuf[0] = '-';  dashLBuf[1] = 'l';

    while( *cp )
    {
	/* We need the entire pathname, and name following the last slash
	*/
	libPath = libPathBuf;
	name = cp;
	while ( *cp && *cp != ',' && *cp >= ' ' )
	{
	    *libPath++ = *cp;		/* every char goes into path	*/
	    if ( *cp++ == '/' )		/* always incr to next char	*/
		name = cp;		/* character after slash	*/
	}
	*libPath = '\0';

	/* already have "-l" in 1st two characters of dashLBuf
	*/
	dashL = &(dashLBuf[2]);

	/* skip "lib" at beginning, and any filename extension at end of name
	*/
	if ( name[0] == 'l' && name[1] == 'i' && name[2] == 'b' )
	    name += 3;
	while (*name && *name != '.' && *name != ',' && *name >= ' ' )
	    *dashL++ = *name++;
	*dashL = '\0';

	/* Allocate permanent copy of libPath for mapping
	*/
	permLibPath = XtNewString( libPathBuf );
	WcMapDynLib( app, XrmStringToQuark( dashLBuf ), permLibPath );
    }
}

/*  -- Register Actions
*******************************************************************************
    A simple wrapper around XtAppAddActions().
    Register action procs which can then be bound to widget
    actions using standard Xt mechanisms.
*/

void WcRegisterAction(app, name, proc)
    XtAppContext app;
    String       name;
    XtActionProc proc;
{
    static XtActionsRec action_rec[] = {
        { (String)NULL, (XtActionProc)NULL }
    };

    action_rec[0].string = name;
    action_rec[0].proc = proc;
    XtAppAddActions(app, action_rec, 1);
}

/* -- Register Callbacks
*******************************************************************************
    Register callback functions which can then be bound to widget callback
    lists by the string-to-callback converter CvtStringToCallback(), or by
    the WcLateBind mechanism at invocation time.
*/

void WcRegisterCallback ( app, name, Callback, closure )
    XtAppContext	app;		/* each app has separate registry */
    String		name;		/* callback name, CASE INSENSITIVE*/
    XtCallbackProc	Callback;	/* callback function pointer	  */
    XtPointer		closure;	/* default client data		  */
{
    XtCallbackRec* cbRec;

    cbRec = (XtCallbackRec*)XtMalloc( sizeof(XtCallbackRec) );
    cbRec->callback = Callback;
    cbRec->closure  = closure;

    WcMapCallback( app, WcStringToQuark(name), cbRec );
}

/*  -- Register Methods
*******************************************************************************
    Register methods which can be resolved by the WcLateBind mechanism at
    invocation time.
*/

void WcRegisterMethod ( app, className, methodName, Method, closure )
    XtAppContext	app;		/* each app has separate registry */
    String		className;	/* class name,  CASE SENSITIVE	  */
    String		methodName;	/* method name, CASE SENSITIVE    */
    XtCallbackProc	Method;		/* method function pointer	  */
    XtPointer		closure;	/* registered client data	  */
{
    XtCallbackRec*	cbRec;
    XrmQuark		classQ, nameQ;

    if ( className && className[0] == '-' && className[1] == 'l' )
    {
	WcWARN2( (Widget)0, "WcRegisterMethod", "invalidClassName",
	  "Class names cannot begin with `-l' - registration of %s::%s ignored",
	  className, methodName );
	return;
    }

    cbRec = (XtCallbackRec*)XtMalloc( sizeof(XtCallbackRec) );
    cbRec->callback = Method;
    cbRec->closure  = closure;

    classQ = XrmStringToQuark(className);
    nameQ  = XrmStringToQuark(methodName);

    WcMapCallbackMethod( app, classQ, nameQ, cbRec );
}

/*  -- Attach Object to Widget
*******************************************************************************
    Sort of like registration, so its in here...
*/
void WcAttachThisToWidget( object, className, widget )
    XtPointer	object;
    String	className;
    Widget	widget;
{
    if ( className && className[0] == '-' && className[1] == 'l' )
    {
	char* fullName = WcWidgetToFullName( widget );
        WcWARN2( widget, "WcAttachThisToWidget", "invalidClassName",
"Class names cannot begin with `-l',\n\
\t- cannot attach object of class `%s'\n\
to widget %s",
		className, fullName );
	XtFree( fullName );
        return;
    }

    WcMapObject( widget, XrmStringToQuark(className), object );
}

/*  -- Detach object from widget
*******************************************************************************
*/
/*ARGSUSED*/
void WcDetachThisFromWidget( object, className, widget )
    XtPointer	object;
    String	className;
    Widget	widget;
{
    WcMapObjectForget( widget, XrmStringToQuark(className) );
}


/*  -- For Backward Compatibility Only
*******************************************************************************
*/

void WcAllowDuplicateRegistration( ignored )
    int ignored;
{
    return;	/* obsolete */
}

void WcAllowDuplicateClassPtrReg( ignored )
    int ignored;
{
    return;	/* obsolete */
}

void WcAllowDuplicateClassNameReg( ignored )
    int ignored;
{
    return;	/* obsolete */
}

void WcAllowDuplicateConstructorReg( ignored )
    int ignored;
{
    return;	/* obsolete */
}

void WcAllowDuplicateCallbackReg( ignored )
    int ignored;
{
    return;	/* obsolete */
}
