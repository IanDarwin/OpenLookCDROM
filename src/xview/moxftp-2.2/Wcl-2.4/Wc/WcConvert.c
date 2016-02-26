#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcConvert.c 1.11 92/11/16 08:09:34
*
* Widget Creation Library - WcConvert.c
*
* This module contains Xt converter functions which convert strings,
* as found in the Xrm database, into useful types, specifically:
* quarks (for classes and constructors), widgets, and callback lists.
*
* Wherever possible, old style converters are used so they will run
* on pre R3 machines which includes a LOT of the machines out there...
*
* It also contains the routine which registers all Wc converters.
*
*******************************************************************************
*/

#include <X11/IntrinsicP.h>		/* Need WidgetRec decl for cvt args */

#ifdef sun
#include <X11/ObjectP.h>	/* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/ShellP.h>
#include <X11/StringDefs.h> 

#include <X11/Wc/WcCreateP.h>

/*
 * Must be ultrix.
 */
#if !defined(XtNwaitForWm)
#include <X11/Shell.h>
#endif


/*
*******************************************************************************
* Resource Converters - Invoked by Xrm
*******************************************************************************
*/

/*  -- Convert String To QuarkRec
*******************************************************************************
    This is used to get the quark identifying a class name or a constructor,
    as required by WcCreate.c functions.

    We cannot use the done() macro here since we are assigning 2 members
    of a struct, so we need to re-implement done in code here.
*/

/*ARGSUSED*/
void WcCvtStringToQuarkRec(args, num_args, fromVal, toVal )
    XrmValue*	args;
    Cardinal*	num_args;
    XrmValue*	fromVal;
    XrmValue*	toVal;
{
    char*	fromString = (char*)(fromVal->addr);

    if ( toVal->addr != NULL )
    {
	if ( toVal->size < sizeof(QuarkRec) )
	{
	    toVal->size = sizeof(QuarkRec);
	    return;
	}
	((QuarkRec*)(toVal->addr))->string = fromString;
	((QuarkRec*)(toVal->addr))->quark  = WcStringToQuark( fromString );
    }
    else
    {
	static QuarkRec staticQuarkRec;
	staticQuarkRec.string = fromString;
	staticQuarkRec.quark  = WcStringToQuark( fromString );
	toVal->addr = (caddr_t)&staticQuarkRec;
    }
    toVal->size = sizeof(QuarkRec);
}

/*  -- Convert String To Quark
*******************************************************************************
*/

/*ARGSUSED*/
void WcCvtStringToQuark(args, num_args, fromVal, toVal )
    XrmValue*   args;
    Cardinal*   num_args;
    XrmValue*   fromVal;
    XrmValue*   toVal;
{
    char*       fromString = (char*)(fromVal->addr);
    XrmQuark	quark = XrmStringToQuark( fromString );
    done( XrmQuark, quark );
}

/*  -- Convert String To Widget
*******************************************************************************
    This conversion creates a Widget id from the X resource database string.
    If the widget so named has not been created when this converter is called,
    then the conversion will fail (return a NULL), and issue a warning.

    Sure would be nice if we could keep track of failed conversions, and
    attempt to fill them in later...

    BOGOSITY:  There is a bug in (seemingly) all versions of Xt up to and
    including Xt release 5 which forces us to have at least two arguments
    to this function.
*/

XtConvertArgRec wcWidgetCvtArgs[] = {
#ifdef XtOffsetOf
 { XtBaseOffset, (caddr_t)XtOffsetOf(WidgetRec, core.self), sizeof(Widget) },
 { XtBaseOffset, (caddr_t)XtOffsetOf(WidgetRec, core.self), sizeof(Widget) }
#else
 { XtBaseOffset, (caddr_t)XtOffset(Widget, core.self), sizeof(Widget) },
 { XtBaseOffset, (caddr_t)XtOffset(Widget, core.self), sizeof(Widget) }
#endif
};
int wcWidgetCvtArgsCount = XtNumber(wcWidgetCvtArgs);

void WcCvtStringToWidget (args, num_args, fromVal, toVal)
    XrmValue *args;
    Cardinal *num_args;
    XrmValue *fromVal;
    XrmValue *toVal;
{
    Widget	refWidget, parent, namedWidget;
    char*	name = fromVal->addr;
    char	cleanName[MAX_XRMSTRING];
    char*	nameToFind;
    int		firstRelTravOp;
    int		child, numKids, found;
    WidgetList	wl;

    if (*num_args != wcWidgetCvtArgsCount)
    {
	Cardinal zero = 0;
	XtErrorMsg( "WcCvtStringToWidget", "wrongArgs", "WcLibError",
		"WcCvtStringToWidget registered without wcCvtWidgetArgs",
		(String*)0, &zero );
	/*NOTREACHED*/
    }
    refWidget = *((Widget*)args[0].addr);

    /* OK to not specify a widget, then its NULL
    */
    if (WcNull(name))
	done( Widget, (Widget)0 )

    (void)WcCleanName( name, cleanName );

    /* Pre-parse of cleanName - must always do some of this.  Its long
     * but meant to be as efficient as possible at run-time.
     */
    if ( cleanName[0] == '.' )
    {
	if ( cleanName[1] == '\0' )
	    done( Widget, refWidget )		/* "." means this widget */
	firstRelTravOp = 1;
    }
    else
    {
	if ( cleanName[0] == 't'
	  && cleanName[1] == 'h'
	  && cleanName[2] == 'i'
	  && cleanName[3] == 's' )
	{
	    if ( cleanName[4] == '\0' )
		done( Widget, refWidget )	/* "this" means this widget */
	    if ( cleanName[4] == '.' )
	    {
		if ( cleanName[5] == '\0' )
		    done( Widget, refWidget )	/* "this." means this widget */
		firstRelTravOp = 5;
	    }
	    else
		firstRelTravOp = 4;
	}
	else
	    firstRelTravOp = 0;
    }

    /* Ugly, ugly, ugly.  Xt does not calloc the widget instance struct, it
     * stupidly mallocs it.  Yet it does not initialize all the fields to 
     * legal values (like zero!).  Specifically, during widget creation, the
     * num_popups and num_children fields are not initialized!!  Therefore,
     * passing the refWidget widget to XtNameToWidget may dump core.
     *
     *========================================================================
     * LESSON #1 in OOP in C: NEVER, NEVER use malloc to allocate an object,
     * ALWAYS use calloc.  All this crap is due to some perceived "efficiency"
     * of using malloc instead of calloc().
     *========================================================================
     *
     * However, converters can also be called AFTER a widget has been fully
     * initialized.  Once initialized, it is OK to pass the refWidget
     * widget to XtNameToWidget.
     *
     * I figured out this way to tell if a widget is being created or not:
     * if the widget exists on its parent's popups or children lists, then
     * it is NOT being created, it has already been initialized.
     */
    parent = XtParent( refWidget );

    if ( parent == (Widget)0 )
    {
	/* Aw shit.  This converter is being invoked on the top level shell.
	 * Shells themselves don't seem to have any resources of type XtRWidget,
	 * but of course, your top level shell may vary.  We make a daring
	 * assumption: it must be the application which is fetching these
	 * resources, so we simply set found True: we assume its fully created.
	 */
	found = 1;
    }
    else
    {
	wl = parent->core.popup_list;
	child = found = 0;
	numKids = parent->core.num_popups;
	while ( child < numKids )
	{
	    if ( *wl == refWidget )
	    {
		found = 1;
		break;
	    }
	    ++child;
	    ++wl;
	}
    }

    if ( !found  &&  XtIsComposite( parent ) )
    {
	CompositeWidget cw = (CompositeWidget)parent;

	wl = cw->composite.children;
	child = 0;
	numKids = cw->composite.num_children;
	while ( child < numKids )
	{
	    if ( *wl == refWidget )
	    {
		found = 1;
		break;
	    }
	    ++child;
	    ++wl;
	}
    }

    if ( found )
    {
	/* Easy: refWidget is NOT being created, so its num_popups and (if a
	 * composite) num_children fields are valid.  Find named widget from
	 * refWidget.
	 */
	namedWidget = WcFullNameToWidget( refWidget, cleanName );
    }
    else if ( *cleanName == '*' )
    {
	/* child IS being created, we handle it like this for backward
	 * compatibility (more or less).
	 */
	namedWidget = WcFullNameToWidget( parent, cleanName );
    }
    else
    {
	/* Hard: refWidget is not initialized.  We must do the search starting
	 * from the parent.  For consistency, in this case the naming pretends
	 * we still are starting at the refWidget: we process the first
	 * relative naming operator.  We must return NULL if the name does not
	 * start with a relative naming operator.
	 */
	String nameToFind = cleanName + firstRelTravOp;

	switch ( *nameToFind )
	{
	case '^':	++nameToFind;  break;
	case '~':	if (XtIsShell(parent))   ++nameToFind;  break;
	case '/':	if (XtIsWMShell(parent)) ++nameToFind;  break;
	default:
	    /* Does not start with upward traversing relative naming operator!
	    */
	    WcWARN2( refWidget, "WcCvtStringToWidget", "notInit",
"WcCvtStringToWidget could not find\n\
    widget %s from uninitialized widget %s.\n\
    Perhaps try starting from parent (pre-pend ^ to widget name)",
		     cleanName, XtName(refWidget) );
	    done( Widget, (Widget)0 );
	}

	namedWidget = WcFullNameToWidget( parent, nameToFind );
    }

    if ((Widget)0 == namedWidget )
    {
	WcWARN2( refWidget, "WcCvtStringToWidget", "notFound",
		"WcCvtStringToWidget could not find widget %s from widget %s.",
		cleanName, XtName(refWidget) );
    }
    done( Widget, namedWidget )
}

/*  -- Convert String to Widget List
*******************************************************************************
    New style converter, Requires R4.
*/
#ifdef XtSpecificationRelease
/* ARGSUSED */
Boolean WcCvtStringToWidgetList(dpy, args, num_args, from, to, data)
    Display*	dpy;
    XrmValuePtr	args;
    Cardinal*	num_args;
    XrmValuePtr	from, to;
    XtPointer*	data;
{
    Widget	beingCreated;
    Widget*	widgetList;
    char*	nameList = (char*)from->addr;
    char*	name;

    if (*num_args != wcWidgetCvtArgsCount)
    {
	Cardinal zero = 0;
	XtErrorMsg("WcCvtStringToWidgetList", "wrongArgs", "WcLibError",
		   "WcCvtStringToWidgetList registered without wcCvtWidgetArgs",
		   (String*)0, &zero );
	/*NOTREACHED*/
    }

    beingCreated = *((Widget*)args[0].addr);

    if ( to->addr != NULL && to->size < sizeof(Widget*) )
    {
	/* User didn't provide enough space
	*/
	to->size = sizeof(Widget *);
	return False;
    }

    name = WcSkipWhitespace_Comma(nameList);

    if (WcNull(name))
	widgetList = (Widget*)0;
    else
    {
	/* Count the comma or whitespace separated widget names
	*/
	int numNames = 0;

	do
	{
	    /* skip through name, stop at null, comma, or whitespace
	    */
	    while ( *name && *name != ',' && ' ' < *name )
		++name;
	    ++numNames;

	    name = WcSkipWhitespace_Comma(name);
	}
	while( *name );

	/* Always at least one name - alloc null terminated widget array.
	 * Note that error (NULL) return is handled below.
	 */
	widgetList = (Widget*)XtCalloc( numNames+1, sizeof(Widget) );
    }

    if ( widgetList != (Widget*)0 )
    {
	char    cleanName[MAX_XRMSTRING];
	Widget* wp = widgetList;

	name = WcSkipWhitespace_Comma(nameList);
	name = WcCleanName( name, cleanName );

	while (cleanName[0])
	{
	    if ((*wp = WcFullNameToWidget(beingCreated, cleanName)) == NULL)
	    {
		WcWARN2(beingCreated, "WcCvtStringToWidgetList", "notFound",
	    "WcCvtStringToWidgetList could not find widget %s from widget %s.",
	    		cleanName, XtName(beingCreated));
		XtFree( (char*)widgetList );
		return False;
	    }

	    name = WcSkipWhitespace_Comma(name);
	    name = WcCleanName( name, cleanName );
	    wp++;
	}
    }

    if (to->addr == NULL)
	to->addr = (caddr_t) &widgetList;
    else
	*(Widget **) to->addr = widgetList;

    to->size = sizeof(Widget *);

    return True;
}

/* ARGSUSED */
void WcWidgetListDestructor( app, to, converter_data, args, num_args )
    XtAppContext app;
    XrmValuePtr	 to;
    XtPointer	 converter_data;
    XrmValuePtr	 args;
    Cardinal*	 num_args;
{
    Widget* widgetList = *(Widget**)to->addr;

    XtFree( (char*)widgetList );
}

#endif /*XtSpecificationRelease for WcCvtStringToWidgetList */

/*  -- Convert String To Callback
*******************************************************************************
    This conversion creates a callback list structure from the X resource
    database string in format:

    <classOpt>name(arg),<classOpt>name(arg).....

    The classOpt part is, as you might guess, optional.  It consists of
    a string followed by "::".  The string can either name a class of object,
    or it may be the short name for a library (like -lWc).  Class names
    cannot begin with "-" but you can't declare a class, struct, or
    typedef starting (or containing) a "-" either, so I think this is
    a feature, not a bug!

    If an object class or a library name is given, then we use late binding.

    Otherwise, if we can find a registered callback with the name (sing
    a mapping agent) then we can fully resolve the callback.

    If we cannot resolve the callback, then we may give a warning, but we
    certainly will try late binding at invokation time.

    The arg is passed to a callback as client data as a null terminated string
    (first level parenthesis stripped off, leading and trailing whitespace is
    removed).  If no default client data is registered when the callback is
    registered (the typical case) then a null terminated string is ALWAYS
    passed: Foo() and Foo both get "" as client data is registered with NULL
    as default client data.

    The default client data is ONLY passed if it is non-null AND there is no
    specification in the resource file - either no parens, or empty parens.
*/

typedef struct _Segment {

    char *lib_start,   *lib_end;	/* dynamic lib name  start, end */
    char *class_start, *class_end;	/* object class name start, end */
    char *name_start,  *name_end;	/* callback name     start, end */
    char *arg_start,   *arg_end;	/* argument string   start, end */

} Segment;


/*  -- Copy arguments from segment into XtCallbackList
*******************************************************************************
    We must return the *same* string so that the callbacks can be removed
    (XtRemoveCallbacks compares both the callback proc and the client data).
    We use quarks, so the same (whitespace stripped) client data really is
    the SAME.  THEREFORE, DON'T CHANGE THE CLIENT DATA IN CALLBACKS!!
*/

static caddr_t WcxClosureFromSeg( seg )
    Segment*	   seg;
{
    if ( seg->arg_start )
    {
	/* arg in parens - pass as string
	*/
	XrmQuark quark;
	char*	 closure;
	int	 alen	= 1 + seg->arg_end - seg->arg_start;
	char*	 arg	= XtMalloc(alen+1);

	strncpy ( arg, seg->arg_start, alen );
	arg[alen] = NUL;

	quark   = XrmStringToQuark( arg );
	closure = WcMapClosureFind( quark );

	if ( closure == NULL )
	{
	    /* DO NOT FREE XtMallocd CLOSURE !!!!
	    */
	    WcMapClosure( quark, arg );
	    return (caddr_t)arg;
	}
	else
	{
	    XtFree( arg );
	    return (caddr_t)closure;
	}
    }
    else
    {
	return (caddr_t)"";		/* note this is a CONSTANT string */
    }
}

/*  -- Build XtCallbackList based on libOpt+classOpt+name+arg segments
*******************************************************************************
    Syntax:
	<callbackResource>	::=	<cbSpec> [<cbSpec> ...]
	<cbSpec>		::=	<dynOpt>[<dynOpt>]<name><argOpt>
	<dynOpt>		::=	empty
				|	<className>::
				|	-l<libName>::
	<name>			::=	id
	<className>		::=	id
	<libName>		::=	id
	<argOpt>		::=	empty
				|	( anything )

    A resource can be one or more callback specifications.

    Each specification can name an object class and/or a dynamically linked
    library "shortname" -- i.e., you might refer to the library file
    /usr/openwin/lib/libXol.so.3.0 as "-lXol" -- The object class or library
    name is followed immediatly by a "::".  

    If no class name or library name, then a segment describes a conventional
    (old-fashioned) callback.

    In any event, we ALWAYS do late binding.  Therefore, the resultant
    callback procedure is actually bound by WcLateBindCB().

    Wcl library options control if warning messages are given for
    unresolved methods or callbacks.
*/

static XtCallbackRec* WcxBuildCallbackList( app, seg )
    XtAppContext app;
    Segment*     seg;
{
    XtCallbackRec	cbList[MAX_CALLBACKS];	/* temporary */
    int			cb;

    int			bytes;
    XtCallbackRec*	cbRecPtr;

    int			verboseWarnings = (WcMapWclFind(app))->verboseWarnings;

    /* Process individual callback string segments "lib::class::name(arg)".
    */
    for( cb = 0 ;  seg->name_start  ;  seg++, cb++ )
    {
	/* Always do late binding
	*/
	WcLateBind lb = (WcLateBind)XtCalloc( sizeof(WcLateBindRec), 1 );

	lb->app    = app;
	lb->libQ   = WcCsSubStringToQuark(seg->lib_start,   seg->lib_end  );
	lb->classQ = WcCsSubStringToQuark(seg->class_start, seg->class_end);
	lb->nameQ  = WcCsSubStringToQuark(seg->name_start,  seg->name_end );
	lb->nameq  = WcSubStringToQuark( seg->name_start, seg->name_end );

	/* do NOT simply copy the args or we can never remove the callback!
	*/
	lb->args   = WcxClosureFromSeg( seg );

	cbList[cb].callback = WcLateBinderCB;
	cbList[cb].closure = (XtPointer)lb;

	/* Verbose warning - only if old-fashioned callback
	*/
	if ( verboseWarnings
          && lb->libQ   == NULLQUARK
          && lb->classQ == NULLQUARK
	  && (XtCallbackRec*)0 == WcMapCallbackFind( app, lb->nameq ) )
	{
	    WcWARN1( (Widget)0,
		     "WcStringToCallbackList", "undefinedCallback",
		     "Callback not registered: %s",
		     XrmQuarkToString( lb->nameQ ) );
	}
    }

    /*  -- terminate the callback list
    */
    cbList[cb].callback = NULL;
    cbList[cb].closure  = NULL;
    cb++;

    /*  -- make a permanent copy of the callback list, and return a pointer
    */
    bytes  = cb * sizeof(XtCallbackRec);
    cbRecPtr = (XtCallbackRec*)XtMalloc( bytes );

    for ( cb--  ;  0 <= cb  ;  cb-- )
    {
	cbRecPtr[cb].callback = cbList[cb].callback;
	cbRecPtr[cb].closure  = cbList[cb].closure;
    }

    return cbRecPtr;
}

/*  -- Parse string into name+argument segments: returns 1 if failed
*******************************************************************************
*/

static int WcxParseCallbackString( string, seg, num_segs )
    char*	string;
    Segment*	seg;
    int		num_segs;
{
    register char* 	cp = string;
    register int	in_parens = 0;

    /*  Don't need to do anything if there is nothing interesting in string
    */
    if (WcNull(cp)) return 1;

    /* skip leading whitespace
    */
    while( *cp <= ' ' )
	if (*++cp == NULL) return 1;

/*  -- Always end the parse on a null to make life easier
*/
#define NEXT_CHAR if (*++cp == NULL) goto end_of_parse;
#define BARF(str) { XtStringConversionWarning( string, str ); return 1; }

    while (1)		/* parsing loop */
    {
	int	method, library;

	/* Take care of possible error - barf if *cp is left paren or comma
	*/
	if ( *cp == '(' || *cp == ',' )
	    BARF( "Callback, name cannot start with `(' or `,'" )

	/* Start new segment: at start of library, class or callback name.
	** Will end with whitespace, a left paren, or comma.
	*/
	seg->lib_start = seg->class_start = seg->name_start = cp;
	seg->arg_start = seg->arg_end = (char*)0;

	method = library = 0;

	while (   ' ' < *cp    &&    *cp != '('    &&    *cp != ','   )
	{
	    if ( *cp == ':' && *(cp+1) == ':' )
	    {
		/* Found "::" - if string before starts with "-l" its a
		** library, otherwise its a class name and this is a method.
		*/
		if ( cp == seg->class_start || cp == seg->lib_start )
		    BARF("Callback, missing class or library name before `::'")

		if ( cp - seg->lib_start > 2	/* need more than 2 chars ... */
		  && seg->lib_start[0] == '-'	/* ... which start with "-l"  */
		  && seg->lib_start[1] == 'l' )
		{
		    /* Dynamic library name - starts with "-l"
		    */
		    if ( library++ )
			BARF("Callback, multiple dynamic library names found")

		    seg->lib_end    = cp-1;	/* char before "::" is end */

		    /* Skip "::" and start class name and/or cb name
		    */
		    cp += 2;
		    if ( !method )
			seg->class_start = cp;
		}
		else
		{
		    /* Class name for method.
		    */
		    if ( method++ )
			BARF("Callback, multiple class names found")

		    seg->class_end = cp-1;	/* char before "::" is end */

		    /* Skip "::" and start library name and/or cb name
		    */
		    cp += 2;
		    if ( !library )
			seg->lib_start = cp;
		}
		seg->name_start = cp;
	    }
	    else
		cp++;
	}
	if ( seg->name_start == cp )
	    BARF( "Callback, missing method name after `::'" )

	if ( !library )
	    seg->lib_start = seg->lib_end = (char*)0;

	if ( !method )
	    seg->class_start = seg->class_end = (char*)0;

	seg->name_end = cp-1;	/* found end of the callback name */

	/* May have hit end of string - the parens and args are optional
	*/
	if ( *cp == '\0' )
	    goto end_of_parse;

	/* There may be whitespace between name and left paren or comma
	*/
	while( *cp <= ' ' )
            NEXT_CHAR

	/* if we've found a left paren, collect the argument
	*/
	if ( *cp == '(' )
	{
	    char* ws = cp;		/* points at initial left paren */
	    in_parens = 1;
	    while (in_parens)
	    {
		NEXT_CHAR	/* 1st time skips the initial left paren */
		if ( '(' == *cp ) in_parens++;
		if ( ')' == *cp ) in_parens--;
	    }
	    /* Now cp points at final right paren.  Lets get rid of leading
	    ** and trailing argument whitespace.  If only whitespace then
	    ** this leaves seg->arg_start == seg->arg_end == NULL 
	    */
	    do {
		ws++;	/* skip initial '(', will hit ')' at cp if no arg */
	    } while ( *ws <= ' ' );
	    if (ws != cp)
	    {
		/* only need to change seg->arg_start and seg->arg_end
		** if there is an argument.
		*/
		seg->arg_start = ws; /* first non-whitespace of argument */
		ws = cp;	     /* now look at the final right paren... */
		do {
		    ws--;	     /* and back-up over trailing whitespace */
		} while ( *ws <= ' ' );
		seg->arg_end = ws;
	    }
	    NEXT_CHAR		/* skip final right paren */
	}
	/* Skip optional comma separator, then do next segment
	*/
	while( *cp <= ' ' || *cp == ',' )
	    NEXT_CHAR

	seg++;
	/* NB: we must have space for following `NULL' segment
	*/
	if ( --num_segs < 2 )
	    BARF( "Callback, Too many callbacks" );
    }

end_of_parse:
    /* Got here because we've detected the NULL terminator.  We have at
    ** least found the optional class and the callback name. We could
    ** have hit the NULL at any of the "NEXT_CHAR" invocations.
    */

    if ( in_parens )
	BARF( "Callback, Unbalanced parens in callback argument" )

    seg++; seg->name_start = (char*)NULL;	/* last seg is NULL */

    return 0; /* sucessful parse */
}

/*  -- Convert String To Callback
*******************************************************************************
    Note that the string being converted is in the Xrm database.  We must
    not alter this string.  Therefore the client data MUST be a new string.

    This converter should soon be modified to free the allocated client data 
    using the XtDestructor mechanism.
*/

void WcCvtStringToCallback (args, num_args, fromVal, toVal)
    XrmValue *args;
    Cardinal *num_args;
    XrmValue *fromVal;
    XrmValue *toVal;
{
    static XtCallbackRec* retval;	/* return value MUST be static */

    char*	 string = (char *) fromVal->addr;
    Widget	 beingCreated;

    if (*num_args != wcWidgetCvtArgsCount)
    {
	Cardinal zero = 0;
	XtErrorMsg( "WcCvtStringToCallback", "wrongArgs", "WcLibError",
		"WcCvtStringToCallback registered without wcCvtWidgetArgs",
		(String*)0, &zero );
	/*NOTREACHED*/
    }
    beingCreated = *((Widget*)args[0].addr);
    retval = WcStringToCallbackList( beingCreated, string );
    done( XtCallbackRec*, retval )
}

/*  -- Parse String into Callbacks and Arguments (client data)
*******************************************************************************
*/
XtCallbackRec* WcStringToCallbackList( w, callbacks )
    Widget	w;
    String	callbacks;
{
    Segment	 name_arg_segments[MAX_CALLBACKS];	
    XtAppContext app;

    app = XtWidgetToApplicationContext( w );

    if (WcxParseCallbackString( callbacks, name_arg_segments, MAX_CALLBACKS ))
	return (XtCallbackRec*)NULL;

    /* name_arg_segments[MAX_CALLBACKS-1].name_start MUST be NULL 
    */
    return WcxBuildCallbackList( app, name_arg_segments );
}

/*  -- Free callback list obtained with WcStringToCallbackList()
*******************************************************************************
    Keep consistent with the way the callback list is allocated in 
    WcxBuildCallbackList().  Note that we do NOT free closure data,
    as this data is now owned by the client data mapping agent.
    It can really only be freed when no callback needs the client
    data anymore, and we really have NO way of knowing when this
    will occur.
*/
void WcFreeCallbackList( callbackList )
    XtCallbackRec* callbackList;
{
    XtFree( (char*)callbackList );
}

/*  -- Convert String To Dynamic Library Registrations
*******************************************************************************
    Take a list of names of libraries, convert them into "-lName" and
    register the entire path name with the Wcl mapping agent.  Enhancements
    planned: use background proc to find the appropriate library using,
    LD_LIBRARY_PATH, and to call dlopen() to register the handle instead.

    This does not really assign anything to the Wcl record: instead, it
    registers the names with the wcAgent mapping agent.
*/

/*ARGSUSED*/
void WcCvtStringToDynamicLibs ( args, num_args, fromVal, toVal )
    XrmValue*	args;
    Cardinal*	num_args;
    XrmValue*	fromVal;
    XrmValue*	toVal;
{
    Widget	 beingCreated;
    XtAppContext app;
    char*	 libNames;

    if (*num_args != wcWidgetCvtArgsCount)
    {
	Cardinal zero = 0;
	XtErrorMsg( "WcCvtStringToDynamicLibs", "wrongArgs", "WcLibError",
		"WcCvtStringToDynamicLibs registered without wcCvtWidgetArgs",
		(String*)0, &zero );
	/*NOTREACHED*/
    }
    beingCreated = *((Widget*)args[0].addr);
    app          = XtWidgetToApplicationContext( beingCreated );
    libNames     = (char*)(fromVal->addr);

    WcRegisterDynamicLibs( app, libNames );

    return;
}

/*
*******************************************************************************
*  Register Converters
*******************************************************************************
*/

/*  -- Add String To ... Convertors
*******************************************************************************
    This function is invoked by WcInitialize().
*/


void WcAddConverters ( app )
    XtAppContext app;
{
    ONCE_PER_XtAppContext( app );

    XtAddConverter	(XtRString, WcRQuarkRec,
			WcCvtStringToQuarkRec,
			(XtConvertArgList)NULL, (Cardinal)0);

    XtAddConverter	(XtRString, WcRQuark,
			WcCvtStringToQuark,
			(XtConvertArgList)NULL, (Cardinal)0);

/* for systems without XtRWidget (SCO SVR3 for example) */
#ifndef XtRWidget
#define XtRWidget "Widget"
#endif

    XtAddConverter	(XtRString, XtRWidget,
			WcCvtStringToWidget,
			wcWidgetCvtArgs, wcWidgetCvtArgsCount );

#ifdef XtSpecificationRelease
    XtSetTypeConverter	(XtRString, XtRWidgetList,
			WcCvtStringToWidgetList,
			wcWidgetCvtArgs, wcWidgetCvtArgsCount,
			XtCacheNone, WcWidgetListDestructor );
#endif

    XtAddConverter	(XtRString, XtRCallback,
			WcCvtStringToCallback,
			wcWidgetCvtArgs, wcWidgetCvtArgsCount );

    XtAddConverter	(XtRString, WcRDynamicLibs,
			WcCvtStringToDynamicLibs,
			wcWidgetCvtArgs, wcWidgetCvtArgsCount );
}
