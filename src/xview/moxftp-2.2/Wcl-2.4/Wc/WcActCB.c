#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcActCB.c    1.8 92/10/28 07:48:58
*
* Widget Creation Library - WcActCB.c - Actions and Callbacks
*
* Wcl provides equivalent callbacks and actions for many typical application
* behaviors.  These callbacks and actions take string arguments.  If the
* arguments consist as whitespace or comma separated words, then it is
* easier to implement the behavior as an XtActionProc.  If the arguments
* need to be parsed according to more complex rules, then they are implemented
* as callbacks.
*
*******************************************************************************
*/

#ifdef WC_HAS_dlopen_AND_dlsym
#include <dlfcn.h>
#endif

#include <X11/IntrinsicP.h>

#ifdef sun
#include <X11/ObjectP.h>	/* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/ShellP.h>
#include <X11/Wc/WcCreateP.h>
#include <X11/StringDefs.h>

/*
 * Must be ultrix.
 */
#if !defined(XtNwaitForWm)
#include <X11/Shell.h>
#endif

/*
*******************************************************************************
* Actions Utilities
*******************************************************************************
*/

/*  -- Invoke XtFunctions using params as widget names
*******************************************************************************
    Many actions are simple wrappers around Xt functions.  These utilities
    make writing such wrapper actions trivial.
*/

#define CAN_BE_GADGET  0
#define MUST_BE_WIDGET 1
#define MUST_BE_SHELL  2

typedef void (*PFVWidget) _((Widget));

static void WcxActProcWidget( reqType, caller, w, params, num_params, Proc )
    int		reqType;	/* some things only work for widgets	*/
    char*	caller;		/* action invoked by translation mgr	*/
    Widget	w;		/* action invoked on this widget	*/
    char**	params;		/* action arguments			*/
    Cardinal*	num_params;	/* count of params			*/
    PFVWidget	Proc;		/* the procedure to be invoked		*/
{
    int		i;
    for ( i = 0 ; i < *num_params ; i++ )
    {
	Widget target = WcFullNameToWidget( w, params[i] );
	if ( target )
	{
	    if (   reqType == CAN_BE_GADGET
	      || ( reqType == MUST_BE_WIDGET && XtIsWidget(target) )
	      || ( reqType == MUST_BE_SHELL  && XtIsShell( target) ) )
	    {
		Proc( target );
	    }
	    else
	    {
		if ( reqType == MUST_BE_WIDGET )
		{
		    WcWARN2( w, caller, "mustBeWidget",
			    "%s(%s) - Must be a Widget",
			    caller, params[i] );
		}
		else
		{
		     WcWARN2( w, caller, "mustBeShell",
			     "%s(%s) - Must be a Shell widget",
			     caller, params[i] );
		}
	    }
	}
	else
	{
	    WcWARN2( w, caller, "notFound", 
		   "%s(%s) - Widget not found", caller, params[i] );
	}
    }
}

/*  -- For invoking XtSomething( widget, int )
*******************************************************************************
    For example, XtPopup, XtSetSensitive, ...
*/

typedef void (*PFVWidInt) _((Widget,int));

static void WcxActProcWidgetInt( reqType, caller, w, params, num_params,
				 Proc, arg )
    int		reqType;
    char*	caller;
    Widget	w;
    char**	params;
    Cardinal*	num_params;
    PFVWidInt	Proc;
    int		arg;		/* arg reqd by Proc */
{
    int		i;
    for ( i = 0 ; i < *num_params ; i++ )
    {
	Widget target = WcFullNameToWidget( w, params[i] );
	if ( target  )
	{
	    if (   reqType == CAN_BE_GADGET
	      || ( reqType == MUST_BE_WIDGET && XtIsWidget(target) )
	      || ( reqType == MUST_BE_SHELL  && XtIsShell( target) ) )
	    {
		Proc( target, arg );
	    }
	    else
	    {
		if ( reqType == MUST_BE_WIDGET )
		{
		    WcWARN2( w, caller, "mustBeWidget",
			    "%s(%s) - Must be a Widget",
			    caller, params[i] );
		}
		else
		{
		    WcWARN2( w, caller, "mustBeShell",
			    "%s(%s) - Must be a Shell widget",
			    caller, params[i] );
		}
	    }
	}
	else
	{
	    WcWARN2( w, caller, "notFound", 
		   "%s(%s) - Widget not found", caller, params[i] );
	}
    }
}

/*
*******************************************************************************
* Actions which actually do something (equivalent CB invokes the ACT)
*******************************************************************************
*/

#define ACTION_DECL(name)			\
void name( w, unused, params, num_params )	\
    Widget	w;				\
    XEvent*	unused;				\
    char**	params;				\
    Cardinal*	num_params;


/*  --	Manage or Unmanage Widgets
*******************************************************************************
	WcManage( widget [, widget] ... )
	WcUnmanage( widget [, widget] ... )
*/
#ifdef XtManageChild
#undef XtManageChild
#endif

ACTION_DECL( WcManageACT )
{
    WcxActProcWidget( CAN_BE_GADGET, "WcManage", w, params, num_params,
		      XtManageChild );
}

#ifdef XtUnmanageChild
#undef XtUnmanageChild
#endif

ACTION_DECL( WcUnmanageACT )
{
    WcxActProcWidget( CAN_BE_GADGET, "WcUnmanage", w, params, num_params,
		      XtUnmanageChild );
}

/*  --	Manage or unamange named children
******************************************************************************* 
	WcManageChildren( parent, child [, child] ... )
	WcUnmanageChildren( parent, child [, child] ... )
*/

typedef void (*PFVWidsCard) _((WidgetList,Cardinal));

static void WcxChgMan( caller, Proc, w, params, num_params )
    char*	caller;
    PFVWidsCard	Proc;
    Widget	w;
    char**	params;
    Cardinal*	num_params;
{
    Widget	parent;
    Widget	children[MAX_CHILDREN];
    int    	i;

    if (*num_params < 2)
    {
	WcWARN1( w, caller, "usage", 
		"Usage: %s( parent, child [, child] ... )", caller );
	return;
    }

    if ( NULL == (parent = WcFullNameToWidget( w, *params )) )
    {
	WcWARN2( w, caller, "parentNotFound",
		"%s( %s, ... ) - parent not found", caller, *params );
	return;
    }

    for ( i = 1 ; i < *num_params ; i++ )
    {
	if ( NULL == (children[i-1] = WcChildNameToWidget( parent, params[i] )))
	{
	    WcWARN2( w, caller, "childNotFound",
		"%s() - child %s not found", caller, params[i] );
	    return;
	}
    }
    Proc( children, *num_params-1 );
}

#ifdef XtManageChildren
#undef XtManageChildren
#endif

ACTION_DECL( WcManageChildrenACT )
{
    WcxChgMan( "WcManageChildren", XtManageChildren, w, params, num_params );
}

#ifdef XtUnmanageChildren
#undef XtUnmanageChildren
#endif

ACTION_DECL( WcUnmanageChildrenACT )
{
    WcxChgMan( "WcUnmanageChildren", XtUnmanageChildren, w, params, num_params);
}

/*  -- Destroy named children
*******************************************************************************
    WcDestroy( name [, name] ... )
*/

#ifdef XtDestroyWidget
#undef XtDestroyWidget
#endif

ACTION_DECL( WcDestroyACT )
{
    WcxActProcWidget( CAN_BE_GADGET, "WcDestroy", w, params, num_params,
		      XtDestroyWidget );
}

/* -- Change sensitivity of widgets
*******************************************************************************
    WcSetSensitive( name [, name] ... )
    WcSetInsensitive( name [, name] ... )
*/

#ifdef XtSetSensitive
#undef XtSetSensitive
#endif

ACTION_DECL( WcSetSensitiveACT )
{
 WcxActProcWidgetInt( MUST_BE_WIDGET, "WcSetSensitive", w, params, num_params,
		      XtSetSensitive, TRUE );
}

ACTION_DECL( WcSetInsensitiveACT )
{
 WcxActProcWidgetInt( MUST_BE_WIDGET, "WcSetInsensitive", w, params, num_params,
		      XtSetSensitive, FALSE );
}

/* -- Popup and Popdown widgets
*******************************************************************************
    WcPopup( name [, name] ... )		uses XtGrabNone
    WcPopupGrab( name [, name] ... )		uses XtGrabExclusive
    WcPopdown( name [, name] ... )
*/
#ifdef XtPopup
#undef XtPopup
#endif

ACTION_DECL( WcPopupACT )
{
    WcxActProcWidgetInt( MUST_BE_SHELL, "WcPopup", w, params, num_params, 
			 XtPopup, XtGrabNone );
}

ACTION_DECL( WcPopupGrabACT )
{
    WcxActProcWidgetInt( MUST_BE_SHELL, "WcPopupGrab", w, params, num_params,
			 XtPopup, XtGrabExclusive );
}

#ifdef XtPopdown
#undef XtPopdown
#endif

ACTION_DECL( WcPopdownACT )
{
    WcxActProcWidget( MUST_BE_SHELL, "WcPopdown", w, params, num_params,
		      XtPopdown );
}


/*  -- Map or Unmap widgets
*******************************************************************************
    Some old Xt versions do not provide declarations for XtMapWidget()
    and XtUnmapWidget(), they may only be declared as macros.  Wcl provides
    the functions if they do not exist.

    WcMap( name [, name] ... )
    WcUnmap( name [, name] ... )
*/
#ifdef XtMapWidget
#undef XtMapWidget
#endif
#ifdef XtUnmapWidget
#undef XtUnmapWidget
#endif

extern void XtMapWidget _(( Widget ));
extern void XtUnmapWidget _(( Widget ));

ACTION_DECL( WcMapACT )
{
    WcxActProcWidget( MUST_BE_WIDGET, "WcMap", w, params, num_params,
		      XtMapWidget );
}

ACTION_DECL( WcUnmapACT )
{
    WcxActProcWidget( MUST_BE_WIDGET, "WcUnmap", w, params, num_params,
		      XtUnmapWidget );
}

/*  -- Install Accelerators
*******************************************************************************
    Invokes XtInstallAcclerators aand XtInstallAllAccelerators.

    dest - widget that can activate accelerators,
    src  - widget which provides functionality,
	   and defines the accel translation.
*/

ACTION_DECL( WcInstallAcceleratorsACT )
{
    int i;
    Widget dest, src;
    static char* usage = 
"Usage: WcInstallAccelerators( dest, src [, src] ... )\n\
\tdest - (root of tree) widget that can activate accelerators,\n\
\tsrc  - widget which provides functionality, \n\
\t       and defines the accel translation (accelerators resource)." ;

    if ( *num_params < 2 )
    {
	WcWARN( w, "WcInstallAccelerators", "usage", usage );
	return;
    }
    if ( NULL == (dest = WcFullNameToWidget( w, params[0] )) )
    {
	WcWARN1( w, "WcInstallAccelerators", "destNotFound",
		"Destination widget %s not found.", params[0] );
	WcWARN( w, "WcInstallAccelerators", "usage", usage );
	return;
    }
    for ( i = 1 ; i < *num_params ; i++ )
    {
	if ( NULL == (src = WcFullNameToWidget( w, params[i] )) )
	{
	    WcWARN1( w, "WcInstallAccelerators", "srcNotFound",
		    "Source widget %s not found.", params[i] );
	    WcWARN( w, "WcInstallAccelerators", "usage", usage );
	}
	else
	{
	    XtInstallAccelerators( dest, src );
	}
    }
}

ACTION_DECL( WcInstallAllAcceleratorsACT )
{
    Widget dest, src;
    static char* usage = 
"Usage: WcInstallAllAccelerators( dest, src )\n\
\tdest - (root of tree) widget that can activate accelerators,\n\
\tsrc  - root of widget tree, each child widget can provide functionality, \n\
\t       and can define a accel translation (accelerators resource)." ;

    if ( *num_params != 2 )
    {
	WcWARN( w, "WcInstallAllAccelerators", "usage", usage );
	return;
    }

    if ( NULL == (dest = WcFullNameToWidget( w, params[0] )) )
    {
	WcWARN1( w, "WcInstallAllAccelerators", "destNotFound",
		"Destination widget %s not found.", params[0] );
	WcWARN( w, "WcInstallAllAccelerators", "usage", usage );
	return;
    }

    if ( NULL == (src = WcFullNameToWidget( w, params[1] )) )
    {
	WcWARN1( w, "WcInstallAccelerators", "srcNotFound",
		"Source widget %s not found.", params[1] );
	WcWARN( w, "WcInstallAllAccelerators", "usage", usage );
	return;
    }

    XtInstallAllAccelerators( dest, src );
}


/*  -- Create New Widget Heirarchy
*******************************************************************************
    WcCreateRoot( shell [on: display] [shell [on: display]] ... )
*/

ACTION_DECL( WcCreateRootACT )
{
    int i = 0;

    while(*num_params)	/* must have args or give usage message */
    {

	if (WcStrEq(params[i], "on:"))
	    break;			/* give usage message */

	if ( i+2 < *num_params && WcStrEq(params[i+1], "on:") )
	{
	    XtAppContext	app   = XtWidgetToApplicationContext( w );
	    char*		class = WcAppNameToAppClass( params[i] );
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
	    int			zero  = 0;
#else
	    Cardinal		zero  = 0;
#endif
	    char*		empty = "";
	    Display* dpy;

	    dpy = XtOpenDisplay( app, 
		params[i+2],		/* display name			*/
		params[i], class,	/* shell name and class		*/
		NULL, 0,		/* no options			*/
		&zero, &empty );	/* no argv - XtOpenDisplay	*/

	    XtFree(class);

	    if ( dpy == NULL)
	    {
		WcWARN1( w, "WcCreateRoot", "openDisplayFailed",
			"WcCreateRoot() could not open display %s",params[i+2]);
		return;
	    }
	    (void)WcCreateRoot( dpy, params[i] );
	    i+=3;	/* used 3 arguments */
	}
	else
	{
	    (void)WcCreateRoot( XtDisplay(w), params[i] );
	    ++i;
	}
	if (i == *num_params)
	    return;
    }

    /* Something went wrong
    */
    WcWARN( w, "WcCreateRoot", "usage",
	"Usage: WcCreateRoot( shell [on: display] [shell [on: display]] ... )");
}

/*  -- Fork and exec
*******************************************************************************
    WcSpawn( cmd line )
*/

ACTION_DECL( WcSpawnACT )
{
    int    pid;
    int    i;
    char** argv = (char**)XtCalloc(*num_params + 1, sizeof(char*));

    for (i = 0 ; i < *num_params ; i++)
	argv[i] = params[i];

    if ( 0 == (pid = fork()) )
    {
	/* subprocess */
	execvp( params[0], argv );
	WcWARN1( w, "WcSpawn", "execFailed",
		"WcSpawn(%s ...) - Could not execvp.", params[0] );
	exit(1);
    }
    if (pid == -1)
    {
	WcWARN1( w, "WcSpawn", "forkFailed", 
		"WcSpawn(%s ...) Failed - Could not fork.", params[0] );
    }
}

/*  -- Load Resource File into Resource Database
*******************************************************************************
    WcLoadResourceFile( file [, file] )
*/

ACTION_DECL( WcLoadResourceFileACT )
{
    int		i;

    for ( i = 0 ; i < *num_params ; i++ )
	(void)WcLoadResourceFile( w, params[i] );
}

/*  -- Print Tree of named widgets
*******************************************************************************
    WcPrintTree( name [, name] )
*/

ACTION_DECL( WcPrintTreeACT )
{
    if (*num_params == 0)
	WcPrintTree(w);
    else
	WcxActProcWidget( CAN_BE_GADGET, "WcPrintTree", w, params, num_params,
			  WcPrintTree );
}

/*  -- Print resources under a widget instance
*******************************************************************************
    WcDumpResources( name [, name] )
*/

ACTION_DECL( WcDumpResourcesACT )
{
    if (*num_params == 0)
	WcPostCreateDumpResources( w, stderr );
    else
    {
	int i;
	for ( i = 0 ; i < *num_params ; i++ )
	{
	    Widget target = WcFullNameToWidget( w, params[i] );
	    if ( target  )
	    {
		WcPostCreateDumpResources( target, stderr );
	    }
	    else
	    {
		WcWARN1( w, "WcDumpResources", "notFound", 
		   "WcDumpResources(%s) - Widget not found", params[i] );
	    }
	}
    }
}

/*  -- Make two or more widgets the same (max) size
*******************************************************************************
    WcSameSize( child, child [,child] ... )
*/

ACTION_DECL( WcSameSizeACT )
{
#define MAX_SAME (BUFSIZ/sizeof(Widget))
    Widget	sameWidgets[MAX_SAME];
    int		i, numWidgets;
    Dimension	maxBorder, maxHeight, maxWidth;
    Arg		arg[10];
    int		argc;

    if ( *num_params < 2 )
    {
	WcWARN1( w, "WcSameSize", "needTwoOrMore",
		"WcSameSize(%s) - Need at least two widgets", params[0] );
	return;
    }

    for ( i = numWidgets = 0 ; i < *num_params && numWidgets < MAX_SAME ; i++ )
    {
	char*  name   = (char*)params[i];
	Widget widget = WcFullNameToWidget( w, name );

	if ( (Widget)0 != widget )
	{
	    sameWidgets[numWidgets++] = widget;
	}
	else
	{
	    char* thisName = WcWidgetToFullName( w );
	    WcWARN2( w, "WcSameSize", "notFound",
		"WcSameSize: cannot find widget %s from %s\n",
		name, thisName );
	    XtFree( thisName );
	}
    }

#define MAX_DIM(a,b) ((a)>(b)?(a):(b))
    for ( maxBorder = maxHeight = maxWidth = i = 0  ;  i < numWidgets  ;  i++ )
    {
	maxBorder = MAX_DIM( maxBorder, sameWidgets[i]->core.border_width );
	maxHeight = MAX_DIM( maxHeight, sameWidgets[i]->core.height );
	maxWidth  = MAX_DIM( maxWidth,  sameWidgets[i]->core.width );
    }

    argc = 0;
    XtSetArg( arg[argc], XtNborderWidth, (XtArgVal)maxBorder ); ++argc;
    XtSetArg( arg[argc], XtNheight,      (XtArgVal)maxHeight ); ++argc;
    XtSetArg( arg[argc], XtNwidth,       (XtArgVal)maxWidth  ); ++argc;

    for ( i = 0  ;  i < numWidgets  ;  i++ )
    {
	XtSetValues( sameWidgets[i], arg, argc );
    }

#undef MAX_DIM
#undef MAX_SAME
}

/*  -- Exit the application
*******************************************************************************
    WcExit( [exitVal] )
*/

ACTION_DECL( WcExitACT )
{
    if (*num_params)
	exit( atoi(*params) );
    else
	exit(0);
}

/*
*******************************************************************************
* Callbacks Which Actually Do Something
*******************************************************************************
*/

/*  -- Create Dynamically Created Children from Xrm Database
*******************************************************************************
    WcCreateChildren( parent, child [,child] ... )
    WcCreatePopups(   parent, child [,child] ... )
*/

typedef void (*PVFWidStr) _((Widget,char*));

static void WcxCreateKids ( w, parent_children, caller, CreateFunc )
    Widget	w;
    char*	parent_children;	/* parent + list of named children */
    char*	caller;			/* name of calling CB func 	   */
    PVFWidStr	CreateFunc;
{
    char*	children;
    Widget	parent;
    char	parentName[MAX_XRMSTRING];

    if ( *parent_children == NUL ) 
    {
	WcWARN1( w, caller, "noNames",
		"%s( ) - No widget names provided.", caller );
	return;
    }

    children = WcCleanName( parent_children, parentName );

    children = WcSkipWhitespace_Comma( children );

    if ((Widget)NULL == (parent = WcFullNameToWidget( w, parentName )) )
    {
	WcWARN2( w, caller, "parentNotFound",
		"%s( %s ...) - Parent widget not found.", caller, parentName );
	return;
    }

    if (*children == NUL)
    {
        WcWARN2( w, caller, "noChildNames",
		"%s(%s) - No child names provided.", caller, parentName );
        return;
    }

    CreateFunc( parent, children );
}

void WcCreateChildrenCB ( w, mom_children, unused )
    Widget	w;
    XtPointer	mom_children;	/* parent + list of named children */
    XtPointer	unused;
{
    WcxCreateKids(w, (char*)mom_children,
			"WcCreateChildren", WcCreateNamedChildren);
}

void WcCreatePopupsCB ( w, mom_children, unused )
    Widget	w;
    XtPointer	mom_children;	/* parent + list of named children */
    XtPointer	unused;
{
    WcxCreateKids(w, (char*)mom_children,
			"WcCreatePopups", WcCreateNamedPopups );
}

/* -- Position a TransientShell in middle of parent widget
*******************************************************************************
    This callback is useful as a popupCallback for transient widgets.
    WcPositionTransient()
*/

void WcPositionTransientCB( w, ignored, unused )
    Widget	w;
    XtPointer	ignored;
    XtPointer	unused;
{
    Widget		 child, transientFor;
    XtWidgetGeometry	 request;

#ifndef XtSpecificationRelease
    ShellWidget		 shell = (ShellWidget)w;
#else
    TransientShellWidget shell = (TransientShellWidget)w;

    if ( XtIsSubclass((Widget)shell,transientShellWidgetClass) )
	transientFor = shell->transient.transient_for;
    else
#endif
	transientFor = XtParent(w);

    if ( !XtIsSubclass((Widget)shell,shellWidgetClass)
      || (NULL == transientFor)
      || (NULL == ( child = shell->composite.children[0] )) )
	return;

    request.x = (Position)(transientFor->core.width  - child->core.width) / 2;
    request.y = (Position)(transientFor->core.height - child->core.height)/ 2;

    if (XtIsRealized (transientFor))
	XtTranslateCoords( transientFor, request.x,  request.y,
					&request.x, &request.y );

    request.request_mode  = ( request.x != shell->core.x ? CWX : 0 );
    request.request_mode |= ( request.y != shell->core.y ? CWY : 0 );

    if ( request.request_mode )
	(void)XtMakeGeometryRequest( (Widget)shell, &request, NULL );
}

/*  -- Set Resource Value on Widget
*******************************************************************************
    The client data argument consists of one or more resource specifications
    in this syntax:

	client_data	::=	res_spec
			|	res_spec_list

	res_spec	::=	targetName.resName: resValue
			|	targetName.resName(resType): resValue

	res_spec_list	::=	(res_spec)
			|	res_spec_list (res_spec)

*/

void WcSetValueCB ( w, name_res_resType_resVal, unused )
    Widget	w;  
    XtPointer	name_res_resType_resVal;
    XtPointer	unused;
{
    WcSetValue( w, (char*)name_res_resType_resVal );
}

/*  -- WcTraceCB
*******************************************************************************
    Prints out the wiget pathname of the invoking wiget, and an optional
    annotation on stderr.
*/

void WcTraceCB ( w, annotation, unused )
    Widget	w;
    XtPointer	annotation;	/* client data, traceback annotation */
    XtPointer	unused;
{
    char* name = WcWidgetToFullName( w );
    
    fprintf(stderr,"Trace for %s: %s\n", name, (char*)annotation );
    XtFree( name );
}

/*  -- Invoke shell command
*******************************************************************************
    Call system().
*/

void WcSystemCB ( w, shellCmdString, unused )
    Widget	w;
    XtPointer	shellCmdString;
    XtPointer	unused;
{
    system( (char*)shellCmdString );
}

/*  -- Add and Remove Callbacks
*******************************************************************************
    WcAddCallbacks(    widget callbackName CallbackProc( args ) ... )
    WcRemoveCallbacks( widget callbackName CallbackProc( args ) ... )

    Invokes XtAddCallbacks and XtRemoveCallbacks, as callbacks should not just
    be trashed using XtSetValues, since other things besides the application
    code may express intrest in callbacks.  Especially true for popup, map,
    and destroy callbacks
*/

typedef void (*AddOrRemoveProc) _(( Widget, char*, XtCallbackRec* ));

static void WcxAddOrRemoveCallbacks ( w, string, caller, AddOrRemove )
    Widget		w;
    char*		string;
    char*		caller;
    AddOrRemoveProc	AddOrRemove;
{
    char                name[MAX_XRMSTRING];
    XtCallbackRec*      callbacks;
    Widget		target;

    string = WcCleanName( string, name );	/* name of a widget */
    string = WcSkipWhitespace_Comma( string );

    if ( NULL == (target = WcFullNameToWidget( w, name )) )
    {
	WcWARN2( w, caller, "destNotFound",
		"%s(%s) - Widget not found.", caller, name );
	return;
    }

    string = WcCleanName( string, name );	/* name now callback name */
    string = WcSkipWhitespace_Comma( string );

    if ( NULL != (callbacks = WcStringToCallbackList( w, string ) ) )
    {
	AddOrRemove( target, name, callbacks );
	WcFreeCallbackList( callbacks );
    }
}

void WcAddCallbacksCB ( w,  callbackString, unused )
    Widget      w;
    XtPointer   callbackString;
    XtPointer   unused;
{
    WcxAddOrRemoveCallbacks( w, (char*)callbackString,
				"WcAddCallbacks", XtAddCallbacks );
}

void WcRemoveCallbacksCB ( w,  callbackString, unused )
    Widget      w;
    XtPointer   callbackString;
    XtPointer   unused;
{
    WcxAddOrRemoveCallbacks( w, (char*)callbackString,
				"WcRemoveCallbacks", XtRemoveCallbacks );
}

/*  -- Invoke Callback Once Only
*******************************************************************************
    WcOnceOnly( name, callback(clientData) [callback(clientData) ...] )

    I sure wish we did not need to pass the callback name, but we must...
    We don't know which callback list name (i.e., activateCallback,
    destroyCallback, wcAfterChildren etc) is being invoked.  We can't get it
    at conversion time, as the converter does not have enough information.
*/

void WcOnceOnlyCB ( w, name_callbackString, passedAlong )
    Widget	w;
    XtPointer	name_callbackString;
    XtPointer	passedAlong;
{
    static char* usage = 
"Usage: WcOnceOnly( name, callback(clientData) [callback(clientData) ...] )\n\
\tname - callback name (i.e., activateCallback, destroyCallback, etc)\n\
\t\tCallbacks are each invoked with the client data,\n\
\t\tthen this callback is removed.";
    XtCallbackRec*	cb;
    XtCallbackRec*	callbackRecs;   
    char		name[MAX_XRMSTRING];
    char*		callbacks = (char*)name_callbackString;

    callbacks = WcCleanName( callbacks, name );
    callbacks = WcSkipWhitespace_Comma( callbacks );

    if ( NULL != ( callbackRecs = WcStringToCallbackList( w, callbacks ) ) )
    {
	for ( cb = callbackRecs  ;  cb->callback != 0  ;  cb++ )
	{
	    cb->callback( w, cb->closure, passedAlong );
	}
	XtRemoveCallback( w, name, WcOnceOnlyCB, name_callbackString );
	XtFree( (char*)callbackRecs );
    }
    else
    {
	char* fullName = WcWidgetToFullName(w);

	WcWARN2( w, "WcOnceOnly", "noSuchCallbackList",
		"Widget %s does not have a callback list named %s.",
		fullName, name );
	WcWARN( w, "WcOnceOnly", "usage", usage );
	XtFree( fullName );
    }
}

/*  -- Augment or override translations
*******************************************************************************
    WcTranslations( widget translationString )
*/

void WcTranslationsCB ( w, name_trans, unused )
    Widget	w;
    XtPointer	name_trans;
    XtPointer	unused;
{
    char	name[MAX_XRMSTRING];
    char*	translations	= (String)name_trans;
    Widget	target;

    translations = WcCleanName( (char*)name_trans, name );
    translations = WcSkipWhitespace_Comma( translations );
    if ( NULL == (target = WcFullNameToWidget( w, name ) ) )
    {
	WcWARN1( w, "WcTranslations", "notFound",
		"WcTranslations(%s ... ) - Widget not found.", name );
	return;
    }
    WcSetTranslations( target, translations );
}


/*  -- Invoke Action or Callback in Dynamically Linked Libraries
*******************************************************************************
	WcDynamicAction(   sharedLibrary entryPointName([optArgs]) )
	WcDynamicCallback( sharedLibrary entryPointName([optArgs]) )

    Invokes named callback or action.

    Works only on machines which support dlopen() and dlsym(), in other
    words, SunOS and SVR4 machines.  The idea and proof of concept was
    provided by John Coyne (coyne@seismo.CSS.GOV).

    This has really been superceeded by the upgraded string-to-callback
    converter and WcLateBinder()
*/
/*ARGSUSED*/
void WcDynamicInvoke( w, clientData, callData, invokeAction, caller )
    Widget	w;
    XtPointer	clientData, callData;
    int		invokeAction;
    char*	caller;
{
#ifdef WC_HAS_dlopen_AND_dlsym
    static char usageAction[] =
"Usage: WcDynamicAction( sharedLibrary entryPointName([optArgs]) )\n\
  sharedLibrary  - path from / or ~, or name registered with WclDynamicLibs\n\
  entryPointName - name of XtActionProc\n\
  optArgs        - passed as string args to XtActionProc (do NOT free)";

    static char usageCallback[] =
"Usage: WcDynamicCallback( sharedLibrary entryPointName([optArgs]) )\n\
  sharedLibrary  - path from / or ~, or name registered with WclDynamicLibs\n\
  entryPointName - name of XtCallbackProc\n\
  optArgs        - passed as clientData string to XtCallbackProc (do NOT free)";

    char*		usage = ( invokeAction? usageAction : usageCallback );
    char*		msg;
    char		librarySpec[ MAX_XRMSTRING ];
    char*		sharedLibrary;
    char		entryPoint[ MAX_XRMSTRING ];
    char		optArgs[ MAX_XRMSTRING ];
    char*		from;
    char*		to;
    void*		handle;

    /* First word is the shared library
    */
    from = WcCleanName( (char*)clientData, librarySpec );

    /* Get name of entry point
    */
    from = WcSkipWhitespace_Comma( from );
    for ( to = entryPoint  ;  *from && ' ' <= *from && *from != '('  ; )
	*to++ = *from++;
    *to = '\0';

    if ( '\0' == *entryPoint )
    {
	WcWARN(  w, caller, "usage", usage );
	return;
    }

    /* Get optional arguments, strip leading and trailing blanks
    */
    while ( *from && *from <= ' ' && *from != '(' )
	from++;

    if ( *from == '\0' )
    {
	/* No parens means no optional args
	*/
	*optArgs = '\0';
    }
    else if ( *from == '(' )
    {
	/* Get chars in parens, no leading or trailing whitespace
	*/
	char* firstNonWhitespace = (char*)0;
	char* lastNonWhitespace;
	int inParens = 0;
	while ( *from )
	{
	    if      ( '(' == *from ) inParens++;
	    else if ( ')' == *from ) inParens--;
	    else if ( ' ' <  *from )
	    {
		if ( firstNonWhitespace == (char*)0 )
		    firstNonWhitespace = from;
		lastNonWhitespace = from;
	    }
	    ++from;
	}
	if ( 0 != inParens )
	{
	    if ( invokeAction )
		msg = "WcDynamicAction( %s ) - Unbalanced parens." ;
	    else
		msg = "WcDynamicCallback( %s ) - Unbalanced parens." ;
	    WcWARN1( w, caller, "unbalancedParens", msg, (char*)clientData );
	    return;
	}

	to = optArgs;
	if ( firstNonWhitespace != (char*)0 )
	{
	    from = firstNonWhitespace;
	    while ( from <= lastNonWhitespace )
		*to++ = *from++;
	}
	*to = '\0';
    }
    else
    {
	/* Junk following entry point name
	*/
	WcWARN(  w, caller, "usage", usage );
	return;
    }

    if ( librarySpec[0] == '-' && librarySpec[1] == 'l' )
    {
	/* Library which should have been registered with Wcl
	*/
	XtAppContext app = XtWidgetToApplicationContext(w);
	sharedLibrary = WcMapDynLibFind( app, XrmStringToQuark(librarySpec) );

	if ( (char*)0 == sharedLibrary )
	{
	    if ( invokeAction )
		msg = "WcDynamicAction( %s ... ) - library not registered.";
	    else
		msg = "WcDynamicCallback( %s ... ) - library not registered.";

	    WcWARN1( w, caller, "unknownLibrary", msg, librarySpec );
	    return;
	}
    }
    else if ( librarySpec[0] == '~' )
    {
	char  user[ MAX_PATHNAME ];
	char  path[ MAX_PATHNAME ];
	char* homeDir;

	from = &librarySpec[1];		/* skip the tilda */
	to   = &user[0];

	while ( *from && *from != '/' )
	    *to++ = *from++;
	*to = '\0';

	homeDir = WcHomeDirectory( user );	/* homeDir is static storage */

	if( WcStrLen(homeDir) + WcStrLen(from) >= MAX_PATHNAME )
	{
	    if ( invokeAction )
	      msg="WcDynamicAction( %s ... ) - Expanded name too long: %s%s";
	    else
	      msg="WcDynamicCallback( %s ... ) - Expanded name too long: %s%s";
	    WcWARN3( w, caller, "tooLongExpanded",
		msg, librarySpec, homeDir, from );
	    return;
	}

	WcStrCpy( path, homeDir );
	WcStrCat( path, from );		/* from points at first '/' */

	sharedLibrary = path;
    }
    else
    {
	sharedLibrary = librarySpec; /* It must be a path name */
    }

    if ( NULL == (handle = dlopen( sharedLibrary, RTLD_LAZY )) )
    {
	if ( invokeAction )
	    msg = "WcDynamicAction( %s ... ) Failed - %s";
	else
	    msg = "WcDynamicCallback( %s ... ) Failed - %s";
	WcWARN2( w, "WcDynamicCallback", "dlopenFailed",
		msg, sharedLibrary, dlerror() );
	return;
    }

    if ( invokeAction )
    {
	XtActionProc ActProc = (XtActionProc)dlsym( handle, entryPoint );

	if ( NULL != ActProc )
	    WcInvokeAction( ActProc, w, optArgs );
	else
	    WcWARN3( w, caller, "dlsymFailed",
		"WcDynamicAction( %s %s ) Failed - %s",
		sharedLibrary, entryPoint, dlerror() );
    }
    else
    {
	XtCallbackProc CbProc = (XtCallbackProc)dlsym( handle, entryPoint );

	if ( NULL != CbProc )
	    CbProc( w, (XtPointer)optArgs, callData );
	else
	    WcWARN3( w, "WcDynamicCallback", "dlsymFailed",
		"WcDynamicCallback( %s %s ) Failed - %s",
		sharedLibrary, entryPoint, dlerror() );
    }
#else
    WcWARN( w, "WcDynamicInvoke", "notSupported",
      "WcDynamicInvoke() requires dynamic linking via dlopen() and dlsym()" );
#endif
}

/*ARGSUSED*/
void WcDynamicActionCB( w, clientData, callData )
    Widget	w;
    XtPointer	clientData, callData;
{
#ifdef WC_HAS_dlopen_AND_dlsym
    WcDynamicInvoke( w, clientData, callData, 1, "WcDynamicAction" );
#else
    WcWARN( w, "WcDynamicAction", "notSupported",
      "WcDynamicAction() requires dynamic linking via dlopen() and dlsym()" );
#endif
}

/*ARGSUSED*/
void WcDynamicCallbackCB( w, clientData, callData )
    Widget	w;
    XtPointer	clientData, callData;
{
#ifdef WC_HAS_dlopen_AND_dlsym
    WcDynamicInvoke( w, clientData, callData, 0, "WcDynamicCallback" );
#else
    WcWARN( w, "WcDynamicCallback", "notSupported",
      "WcDynamicCallback() requires dynamic linking via dlopen() and dlsym()" );
#endif
}

/*
*******************************************************************************
* Callbacks which simply invoke equivalent actions
*******************************************************************************
*/

#define CB_INVOKES_ACT( cb, act )		\
void    cb( w, client_data, unused )		\
    Widget	w;				\
    XtPointer	client_data;			\
    XtPointer	unused;				\
{						\
    WcInvokeAction( act, w, client_data );	\
}

CB_INVOKES_ACT( WcManageCB,			WcManageACT		)
CB_INVOKES_ACT( WcUnmanageCB,			WcUnmanageACT		)
CB_INVOKES_ACT( WcManageChildrenCB,		WcManageChildrenACT	)
CB_INVOKES_ACT( WcUnmanageChildrenCB,		WcUnmanageChildrenACT	)
CB_INVOKES_ACT( WcDestroyCB,			WcDestroyACT		)
CB_INVOKES_ACT( WcSetSensitiveCB,		WcSetSensitiveACT	)
CB_INVOKES_ACT( WcSetInsensitiveCB,		WcSetInsensitiveACT	)
CB_INVOKES_ACT( WcPopupCB,			WcPopupACT		)
CB_INVOKES_ACT( WcPopupGrabCB,			WcPopupGrabACT		)
CB_INVOKES_ACT( WcPopdownCB,			WcPopdownACT		)
CB_INVOKES_ACT( WcMapCB,			WcMapACT		)
CB_INVOKES_ACT( WcUnmapCB,			WcUnmapACT		)
CB_INVOKES_ACT( WcInstallAcceleratorsCB,	WcInstallAcceleratorsACT )
CB_INVOKES_ACT( WcInstallAllAcceleratorsCB,	WcInstallAllAcceleratorsACT )
CB_INVOKES_ACT( WcCreateRootCB,			WcCreateRootACT		)
CB_INVOKES_ACT( WcSpawnCB,			WcSpawnACT		)
CB_INVOKES_ACT( WcLoadResourceFileCB,		WcLoadResourceFileACT	)
CB_INVOKES_ACT( WcPrintTreeCB,			WcPrintTreeACT		)
CB_INVOKES_ACT( WcDumpResourcesCB,		WcDumpResourcesACT	)
CB_INVOKES_ACT( WcSameSizeCB,			WcSameSizeACT		)
CB_INVOKES_ACT( WcExitCB,			WcExitACT		)

/*  -- WcRegisterCreateCallbacks
*******************************************************************************
    Register all Wcl callbacks.  Called from WcInitialize, so applications
    normally never call this directly.
*/

void WcRegisterWcCallbacks ( app )
    XtAppContext app;
{
    ONCE_PER_XtAppContext( app );

#define RCALL( name, func ) WcRegisterCallback ( app, name, func, NULL );

  /* Actually implemented as XtActionProcs:
  */
  RCALL( "WcManage",			WcManageCB			)
  RCALL( "WcManageCB",			WcManageCB			)
  RCALL( "WcUnmanage",			WcUnmanageCB			)
  RCALL( "WcUnmanageCB",		WcUnmanageCB			)
  RCALL( "WcManageChildren",		WcManageChildrenCB		)
  RCALL( "WcManageChildrenCB",		WcManageChildrenCB		)
  RCALL( "WcUnmanageChildren",		WcUnmanageChildrenCB		)
  RCALL( "WcUnmanageChildrenCB",	WcUnmanageChildrenCB		)
  RCALL( "WcDestroy",			WcDestroyCB			)
  RCALL( "WcDestroyCB",			WcDestroyCB			)
  RCALL( "WcSetSensitive",		WcSetSensitiveCB		)
  RCALL( "WcSetSensitiveCB",		WcSetSensitiveCB		)
  RCALL( "WcSetInsensitive",		WcSetInsensitiveCB		)
  RCALL( "WcSetInsensitiveCB",		WcSetInsensitiveCB		)
  RCALL( "WcPopup",			WcPopupCB			)
  RCALL( "WcPopupCB",			WcPopupCB			)
  RCALL( "WcPopupGrab",			WcPopupGrabCB			)
  RCALL( "WcPopupGrabCB",		WcPopupGrabCB			)
  RCALL( "WcPopdown",			WcPopdownCB			)
  RCALL( "WcPopdownCB",			WcPopdownCB			)
  RCALL( "WcMap",			WcMapCB				)
  RCALL( "WcMapCB",			WcMapCB				)
  RCALL( "WcUnmap",			WcUnmapCB			)
  RCALL( "WcUnmapCB",			WcUnmapCB			)
  RCALL( "WcInstallAccelerators",	WcInstallAcceleratorsCB		)
  RCALL( "WcInstallAcceleratorsCB",	WcInstallAcceleratorsCB		)
  RCALL( "WcInstallAllAccelerators",	WcInstallAllAcceleratorsCB	)
  RCALL( "WcInstallAllAcceleratorsCB",	WcInstallAllAcceleratorsCB	)
  RCALL( "WcCreateRoot",		WcCreateRootCB			)
  RCALL( "WcCreateRootCB",		WcCreateRootCB			)
  RCALL( "WcSpawn",			WcSpawnCB			)
  RCALL( "WcSpawnCB",			WcSpawnCB			)
  RCALL( "WcLoadResourceFile",		WcLoadResourceFileCB		)
  RCALL( "WcLoadResourceFileCB",	WcLoadResourceFileCB		)
  RCALL( "WcPrintTree",			WcPrintTreeCB			)
  RCALL( "WcPrintTreeCB",		WcPrintTreeCB			)
  RCALL( "WcDumpResources",		WcDumpResourcesCB		)
  RCALL( "WcDumpResourcesCB",		WcDumpResourcesCB		)
  RCALL( "WcSameSize",			WcSameSizeCB			)
  RCALL( "WcSameSizeCB",		WcSameSizeCB			)
  RCALL( "WcExit",			WcExitCB			)
  RCALL( "WcExitCB",			WcExitCB			)

  /* Actually implemented as XtCallbackProcs:
  */
  RCALL( "WcCreateChildren",		WcCreateChildrenCB		)
  RCALL( "WcCreateChildrenCB",		WcCreateChildrenCB		)
  RCALL( "WcCreatePopups",		WcCreatePopupsCB		)
  RCALL( "WcCreatePopupsCB",		WcCreatePopupsCB		)
  RCALL( "WcPositionTransient",		WcPositionTransientCB		)
  RCALL( "WcPositionTransientCB",	WcPositionTransientCB		)
  RCALL( "WcSetValues",			WcSetValueCB			)
  RCALL( "WcSetValuesCB",		WcSetValueCB			)
  RCALL( "WcSetValue",			WcSetValueCB			)
  RCALL( "WcSetValueCB",		WcSetValueCB			)
  RCALL( "WcTrace",			WcTraceCB			)
  RCALL( "WcTraceCB",			WcTraceCB			)
  RCALL( "WcSystem",			WcSystemCB			)
  RCALL( "WcSystemCB",			WcSystemCB			)
  RCALL( "WcAddCallbacks",		WcAddCallbacksCB		)
  RCALL( "WcAddCallbacksCB",		WcAddCallbacksCB		)
  RCALL( "WcRemoveCallbacks",		WcRemoveCallbacksCB		)
  RCALL( "WcRemoveCallbacksCB",		WcRemoveCallbacksCB		)
  RCALL( "WcOnceOnly",			WcOnceOnlyCB			)
  RCALL( "WcOnceOnlyCB",		WcOnceOnlyCB			)
  RCALL( "WcTranslations",		WcTranslationsCB		)
  RCALL( "WcTranslationsCB",		WcTranslationsCB		)
  RCALL( "WcDynamicAction",		WcDynamicActionCB		)
  RCALL( "WcDynamicActionCB",		WcDynamicActionCB		)
  RCALL( "WcDynamicCallback",		WcDynamicCallbackCB		)
  RCALL( "WcDynamicCallbackCB",		WcDynamicCallbackCB		)
}

/*
*******************************************************************************
* Actions which simply invoke equivalent callbacks
*******************************************************************************
*/

#define ACT_INVOKES_CB( act, cb )			\
void    act( w, unused, params, num_params )		\
    Widget	w;					\
    XEvent*	unused;					\
    char**	params;					\
    Cardinal*	num_params;				\
{							\
    WcInvokeCallback( cb, w, params, num_params );	\
}

ACT_INVOKES_CB( WcCreateChildrenACT,	WcCreateChildrenCB	)
ACT_INVOKES_CB( WcCreatePopupsACT,	WcCreatePopupsCB	)
ACT_INVOKES_CB( WcPositionTransientACT,	WcPositionTransientCB	)
ACT_INVOKES_CB( WcSetValueACT,		WcSetValueCB		)
ACT_INVOKES_CB( WcTraceACT,		WcTraceCB		)
ACT_INVOKES_CB( WcSystemACT,		WcSystemCB		)
ACT_INVOKES_CB( WcAddCallbacksACT,	WcAddCallbacksCB	)
ACT_INVOKES_CB( WcRemoveCallbacksACT,	WcRemoveCallbacksCB	)
ACT_INVOKES_CB( WcOnceOnlyACT,		WcOnceOnlyCB		)
ACT_INVOKES_CB( WcTranslationsACT,	WcTranslationsCB	)
ACT_INVOKES_CB( WcDynamicActionACT,	WcDynamicActionCB	)
ACT_INVOKES_CB( WcDynamicCallbackACT,	WcDynamicCallbackCB	)

/*  -- WcRegisterWcActions
*******************************************************************************
   Register all Wcl supplied actions.  Called from WcInitialize(), so 
   applications usually do not call this directly.
*/

void WcRegisterWcActions ( app )
XtAppContext app;
{
    static XtActionsRec WcActions[] = {

      {"WcManage",			WcManageACT			},
      {"WcManageACT",			WcManageACT			},
      {"WcUnmanage",			WcUnmanageACT			},
      {"WcUnmanageACT",			WcUnmanageACT			},
      {"WcManageChildren",		WcManageChildrenACT		},
      {"WcManageChildrenACT",		WcManageChildrenACT		},
      {"WcUnmanageChildren",		WcUnmanageChildrenACT		},
      {"WcUnmanageChildrenACT",		WcUnmanageChildrenACT		},
      {"WcDestroy",			WcDestroyACT			},
      {"WcDestroyACT",			WcDestroyACT			},
      {"WcSetSensitive",		WcSetSensitiveACT		},
      {"WcSetSensitiveACT",		WcSetSensitiveACT		},
      {"WcSetInsensitive",		WcSetInsensitiveACT		},
      {"WcSetInsensitiveACT",		WcSetInsensitiveACT		},
      {"WcPopup",			WcPopupACT			},
      {"WcPopupACT",			WcPopupACT			},
      {"WcPopupGrab",			WcPopupGrabACT			},
      {"WcPopupGrabACT",		WcPopupGrabACT			},
      {"WcPopdown",			WcPopdownACT			},
      {"WcPopdownACT",			WcPopdownACT			},
      {"WcMap",				WcMapACT			},
      {"WcMapACT",			WcMapACT			},
      {"WcUnmap",			WcUnmapACT			},
      {"WcUnmapACT",			WcUnmapACT			},
      {"WcInstallAccelerators",		WcInstallAcceleratorsACT	},
      {"WcInstallAcceleratorsACT",	WcInstallAcceleratorsACT	},
      {"WcInstallAllAccelerators",	WcInstallAllAcceleratorsACT	},
      {"WcInstallAllAcceleratorsACT",	WcInstallAllAcceleratorsACT	},
      {"WcCreateRoot",			WcCreateRootACT			},
      {"WcCreateRootACT",		WcCreateRootACT			},
      {"WcSpawn",			WcSpawnACT			},
      {"WcSpawnACT",			WcSpawnACT			},
      {"WcLoadResourceFile",		WcLoadResourceFileACT		},
      {"WcLoadResourceFileACT",		WcLoadResourceFileACT		},
      {"WcPrintTree",			WcPrintTreeACT			},
      {"WcPrintTreeACT",		WcPrintTreeACT			},
      {"WcDumpResources",		WcDumpResourcesACT		},
      {"WcDumpResourcesACT",		WcDumpResourcesACT		},
      {"WcSameSize",			WcSameSizeACT			},
      {"WcSameSizeACT",			WcSameSizeACT			},
      {"WcExit",			WcExitACT			},
      {"WcExitACT",			WcExitACT			},

      {"WcCreateChildren",		WcCreateChildrenACT		},
      {"WcCreateChildrenACT",		WcCreateChildrenACT		},
      {"WcCreatePopups",		WcCreatePopupsACT		},
      {"WcCreatePopupsACT",		WcCreatePopupsACT		},
      {"WcPositionTransient",		WcPositionTransientACT		},
      {"WcPositionTransientACT",	WcPositionTransientACT		},
      {"WcSetValues",			WcSetValueACT			},
      {"WcSetValuesACT",		WcSetValueACT			},
      {"WcSetValue",			WcSetValueACT			},
      {"WcSetValueACT",			WcSetValueACT			},
      {"WcTrace",			WcTraceACT			},
      {"WcTraceACT",			WcTraceACT			},
      {"WcSystem",			WcSystemACT			},
      {"WcSystemACT",			WcSystemACT			},
      {"WcAddCallbacks",		WcAddCallbacksACT		},
      {"WcAddCallbacksACT",		WcAddCallbacksACT		},
      {"WcRemoveCallbacks",		WcRemoveCallbacksACT		},
      {"WcRemoveCallbacksACT",		WcRemoveCallbacksACT		},
      {"WcOnceOnly",			WcOnceOnlyACT			},
      {"WcOnceOnlyACT",			WcOnceOnlyACT			},
      {"WcTranslations",		WcTranslationsACT		},
      {"WcTranslationsACT",		WcTranslationsACT		},
      {"WcDynamicAction",		WcDynamicActionACT		},
      {"WcDynamicActionACT",		WcDynamicActionACT		},
      {"WcDynamicCallback",		WcDynamicCallbackACT		},
      {"WcDynamicCallbackACT",		WcDynamicCallbackACT		},
    };

    ONCE_PER_XtAppContext( app );

    XtAppAddActions(app, WcActions, XtNumber(WcActions));
}
