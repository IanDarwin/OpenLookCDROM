#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcName.c 1.12 92/12/10 15:36:12
*
* Widget Creation Library - WcName.c
*
* Implements several name-to-widget and widget-to-name functions as well as
* other general purpose parsing functions.
*
*******************************************************************************
*/

#include <X11/IntrinsicP.h>
#if defined(CRAY)||defined(SVR4)
#include <X11/Xos.h>
#endif

#ifdef sun
#include <X11/ObjectP.h>	/* why don't they just use X from mit!?! */
#include <X11/RectObjP.h>
#endif

#include <X11/StringDefs.h>

#include <X11/Wc/WcCreateP.h>

#ifndef XtSpecificationRelease
#include <X11/Shell.h>
#endif

/* Private Data involving the root widget list
*******************************************************************************
*/

static int    numRoots = 0;
static Widget rootWidgets[MAX_ROOT_WIDGETS];


/*  -- WcChildNameToWidget
*******************************************************************************
    If we have a recent version of Xt with an XtNameToWidget() which does
    not barf on Gadgets, then we can just use XtNameToWidget().
    Otherwise, we use the WcChildNameToWidget function implemented in the file
    XtName.c which is simply a fixed version of XtNameToWidget.
*/

#ifndef XtNameToWidgetBarfsOnGadgets

Widget WcChildNameToWidget( ref, childName )
    Widget ref;
    char*  childName;
{
    return XtNameToWidget( ref, childName );
}

#endif

/*  -- Skip leading whitespace
*******************************************************************************
*/

char* WcSkipWhitespace( cp )
    char* cp;
{
    while ( *cp && *cp <= ' ' )
        cp++;
    return cp;
}

char* WcSkipWhitespace_Comma( cp )
    char* cp;
{
    if (*cp == NUL) return cp;

    while ( *cp && *cp <= ' ' )	cp++;
    if    ( *cp == ',' )	cp++;
    while ( *cp && *cp <= ' ' ) cp++;
    return cp;
}

/*  -- Extract "clean" name from input string
*******************************************************************************
    Copies clean name into output buffer which caller provides.  Returns
    pointer to whitespace or NUL following clean name.
*/

char* WcCleanName( in, out )
    char* in;
    char* out;
{
    if (*in == NUL) { *out = NUL ; return in; }

    while ( *in && *in <= ' ' )		/* in = WcSkipWhitespace( in ); */
	in++;
    for ( ; (*in > ' ' && *in != ',') ; in++ )
        *out++ = *in;
    *out = NUL;
    return in;  /* this points at 1st whitespace or comma following "out" */
}

/*
******************************************************************************
**  -- Full Name To Widget and Supporting Static Functions
******************************************************************************
*/

/*  -- Map Name To Widget starting from any root widget.
******************************************************************************
    Goes down list of all RootWidgets maintained by WcRootWidget, and
    tries each one in turn as the root used to resolve the name into
    a widget.  Therefore, the name must be a path name starting at
    a root: i.e., it can be `*foo' or `root*foo' or similar.
*/

static Widget WcxPathFromAnyRootToWidget( widgetName )
    char* widgetName;
{
    int	widgetNameLen = WcStrLen(widgetName);
    int	i;

    for ( i = 0 ; i < numRoots ; i++ )
    {
	Widget	root		   = rootWidgets[i];
	char*	rootName	   = XtName( root );
	int	rootNameLen	   = WcStrLen( rootName );
	int	startsWithRootName = WcStrEqN(widgetName,rootName,rootNameLen);

	if ( startsWithRootName && widgetName[rootNameLen] == '*' )
	{
	    /* the root widget name is followed by a `*' so strip the
	    ** root name, but keep the star as it implies loose binding.
	    */
	    char* stripped = &widgetName[rootNameLen];
	    return WcChildNameToWidget( root, stripped );
	}

	else if ( startsWithRootName && widgetName[rootNameLen] == '.' )
	{
	    /* the root widget name is followed by a `.' so strip the
	    ** root name and the period to imply tight binding.
	    */
	    char* stripped = &widgetName[++rootNameLen];
	    return WcChildNameToWidget( root, stripped );
	}

	else if ( startsWithRootName && (widgetNameLen == rootNameLen) )
	{
	    /* widgetName is the root widget. */
	    return root;
	}

	/* Did not find the root name.  Try the next.
	*/
    }
    /* Did not find the named widget under any of the known roots.
    */
    return (Widget)NULL;
}

/*  -- Find the named widget
*******************************************************************************

    WcFullNameToWidget examines the first few characters of the `name' argument
    in order to determine which widget is the reference whidget where the search
    will begin.

    The possibilities are these:  The `name' can begin with one or more of the
    relative prefix characters: ^ or ~ or /, which means the reference widget 
    will be some relative node in the widget tree, such as the parent or the 
    shell ancestor or the WMShell (dialogShell, topLevelShell, applicationShell)
    ancestor.  Otherwise, the  root widget will be the starting point.

    Once the starting point is found, XtNameToWidget is invoked.
*/

Widget WcFullNameToWidget( refWidget, name )
    Widget refWidget;
    char*  name;
{
    Widget    retWidget;
    WclRecPtr wcl;

    if ( WcNull( name ))
	return NULL;

    if (WcStrEq( "this", name ))
	return refWidget;

    /* OK, now it gets more compilicated.
    ** See if we have a relative name.
    */

    if (   name[0] == '^'
	|| name[0] == '~'
	|| name[0] == '/'
	|| name[0] == '.'
	|| (   WcStrEqN( "this", name, 4 )
	    && (   name[4] == '.'
		|| name[4] == '*'
		|| name[4] == '^'
		|| name[4] == '~'
		|| name[4] == '/'	) ) )
    {
	/* Relative naming: widget must be in the same widget tree, and
	** must be the relative-named widget or a child thereof.
	*/
	int i = 0;

	if (name[0] == 't')	/* skip the "this" - not needed */
	    i = 4;

	while (name[i] == '^' 	/* parent */
	    || name[i] == '~' 	/* shell ancestor */
	    || name[i] == '/' 	/* WM shell ancestor */
	    || name[i] == '.')	/* eaten and ignored */
	{
	    /* Careful! don't go above a root widget! 
	    */
	    if (!XtParent(refWidget)
	     && (name[i] == '^' || name[i] == '~'|| name[i] == '/'))
	    {
		return NULL;
	    }
	    else if (name[i] == '^')
	    {
		refWidget = XtParent( refWidget );
	    }
	    else if (name[i] == '~')
	    {
		/* some IntrinsicP.h forget to parenthesize the argument in
		** the definition of XtIsShell(), so the seemingly redundant
		** parenthesis are NECESSARY on some machines.
		*/
		while ( XtParent(refWidget) &&
			(! XtIsShell( (refWidget = XtParent(refWidget)) ) ))
		    ; /* go up widget tree until we find a Shell */
	    }
	    else if (name[i] == '/')
	    {
		while ( XtParent(refWidget) &&
			(! XtIsWMShell( (refWidget = XtParent(refWidget)) ) ))
		    ; /* find shell the window manager plays with (not menu) */
	    }
	    i++;	/* eat this character */
	}
	/* We have eaten "this" and all of the relative naming characters
	** leaving nothing, or a name, or a name starting with '*'
	*/
	if ( name[i] == NUL )
	{
	    return refWidget;
	}
	return WcChildNameToWidget( refWidget, &(name[i]) );
    }

    /* Name did not begin with a relative naming operator.
    ** See if named widget is descended from reference widget.
    */
    if (retWidget = WcChildNameToWidget( refWidget, name ))
    {
	return retWidget;
    }

    /* The Xaw CvtStringToWidget converter walks up the widget tree,
    ** parent by parent, looking for a child to match.  Perhaps this
    ** is a good idea.  It is certainly slow.  By default, we don't do
    ** this.  A Wcl library-wide resource can turn this on.
    */
    wcl = WcMapWclFind( XtWidgetToApplicationContext(refWidget) );
    if ( wcl->slowTightNames )
    {
	Widget parent = XtParent( refWidget );
	while ( refWidget != (Widget)0 )
	{
	    if ( retWidget = WcChildNameToWidget( parent, name ) )
		return retWidget;
	    parent = XtParent( parent );
	}
    }

    /* Start at top of this tree, and try to find named widget
    */
    if (retWidget = WcChildNameToWidget( WcRootWidget(refWidget), name ))
    {
	return retWidget;
    }

    /* name not in widget tree under this root, or it begins with a root name.
    */
    if (retWidget = WcxPathFromAnyRootToWidget( name ))
    {
	return retWidget;
    }

    /* Completely unsucessful in parsing this name.
    */
    return NULL;
}

/*  -- WidgetToFullName
*******************************************************************************
    Traverse up the widget tree, sprintf each name right up to
    the root of the widget tree.  sprintf the names to buffer.  Use
    recursion so order of names comes out right.  Client MUST free
    the char string alloc's and returned by WcWidgetToFullName().
*/

static char* nextChar;

static int WcxFullNameLen( w )
    Widget w;
{
    int len = 1 + WcStrLen( XtName(w) );

    if (w->core.parent)
	len += WcxFullNameLen(w->core.parent);
    return len;
}

static void WcxWidgetToFullName( w )
    Widget w;
{
    char* cp;

    if (w->core.parent)
    {
        WcxWidgetToFullName( w->core.parent );	/* nextChar AFTER parent name */
	*nextChar++ = '.';			/* inter-name `dot' */
    }

    cp = XtName(w);

    while (*cp)
	*nextChar++ = *cp++;
}

char* WcWidgetToFullName( w )
    Widget w;
{
    char* buff = XtMalloc( WcxFullNameLen( w ) );

    nextChar = buff;

    WcxWidgetToFullName( w );
    *nextChar = NUL;

    return buff;
}

/*  -- Create an Application Class Name from Application Name
*******************************************************************************
    Initialize first letter to make class, or first two if first is already
    capitalized, or don't worry about it.
*/

char* WcAppNameToAppClass( appName )
    char* appName;
{
    char* lastSlash = rindex( appName, '/' );
    char* appClass  = XtNewString( lastSlash ? lastSlash+1 : appName );

    if (islower(appClass[0]))
	appClass[0] = toupper(appClass[0]);
    else if (islower(appClass[1]))
	appClass[1] = toupper(appClass[1]);

    return appClass;
}

/*  -- Find Application Class Name in argc, argv
*******************************************************************************
    Looks for "-name" followed by application class.
    If there is no "-name", it invokes WcAppNameToAppClass with argv[0].
*/

char* WcAppClass( argc, argv )
    int argc;
    char** argv;
{
    int i;
    for ( i = 0 ; i < argc ; i++ )
    {
	if ( WcStrEq( argv[i], "-name" ) )
	{
	    if ( i < argc-1 )
		return XtNewString( argv[i+1] );
	    else
		return WcAppNameToAppClass( argv[0] );
	}
    }
    return WcAppNameToAppClass( argv[0] );
}

/*  --  Quark from Lower Case String
*******************************************************************************
    This allows strings to be quarkified in a case-insensitve manner,
    as used by the registration and conversion routines.
*/

XrmQuark WcStringToQuark( cp )
    char* cp;
{
    char  lower[NAME_RESOLUTION];
    int   i;

    while ( *cp && *cp <= ' ' )
	++cp;
    for ( i = 0 ; *cp > ' ' && i < NAME_RESOLUTION-1 ; cp++, i++ )
	lower[i] = ( isupper(*cp) ? tolower(*cp) : *cp );
    lower[i] = NUL;

    return XrmStringToQuark(lower);
}

/*   --- Quark from segment of string
*******************************************************************************
    Two versions to maintain /&'$%/'& backward compatibility.  Yuck!
    The first is the old a stupid version, where for some goddamn reason
    I flatten the case of the callback proc names, so the resource files can
    be more case insensitive.  

    For dynamic binding, however, we MUST have a case sensitive quark, so
    we can go from quark to actual string.  We don't need registration,
    so the string in the resource file is the only thing we can go on,
    and the dynamic linking stuff is (of course) case sensitive.  Man, I
    am so pissed I ever took the suggestion to flatten the case.  Its been
    a pain in the ass for years.
*/
XrmQuark WcSubStringToQuark( cp, end )
    char* cp;	/* first character of substring */
    char* end;	/* last character of substring */
{
    char  lower[NAME_RESOLUTION];
    int   i;

    if ( cp == (char*)0 || end == (char*)0 )
	return NULLQUARK;

    for ( i = 0 ; cp <= end && i < NAME_RESOLUTION-1 ; cp++, i++ )
	lower[i] = ( isupper(*cp) ? tolower(*cp) : *cp );
    lower[i] = NUL;

    return XrmStringToQuark(lower);
}

XrmQuark WcCsSubStringToQuark( cp, end )
    char* cp;	/* first character of substring */
    char* end;	/* last character of substring */
{
    char  name[NAME_RESOLUTION];
    int   i;

    if ( cp == (char*)0 || end == (char*)0 )
	return NULLQUARK;

    for ( i = 0 ; cp <= end && i < NAME_RESOLUTION-1 ; cp++, i++ )
	name[i] = *cp;
    name[i] = NUL;

    return XrmStringToQuark(name);
}

/*  --  Recursive PrintTree
*******************************************************************************
    Prints the names and classes of all children of a given widget.
    Each nested level is indented 2 additional spaces.
*/

static void WcxPrintTree( w, depth )
    Widget w;
    int    depth;
{
    int i;
    char* class = w->core.widget_class->core_class.class_name;
    char* name  = WcWidgetToFullName( w );
    char  spaces[81];
    *spaces = NUL;

    for (i = 0 ; i < depth && i < 40 ; i++)
	WcStrCat( spaces, "  " );
    depth++;

    fprintf(stderr, "%s%s of class %s\n", spaces, name, class );

    if ( !XtIsWidget( w ) )
	return; 		/* Gadgets cannot have children of any kind */

    if ( 0 < w->core.num_popups )
    {
	fprintf(stderr, "%sPopups:\n", spaces );
	for (i = 0 ; i < w->core.num_popups ; i++ )
	    WcxPrintTree( w->core.popup_list[i], depth );
    }

    if ( XtIsComposite( w ) )
    {
	CompositeWidget cw = (CompositeWidget) w;
	if ( 0 == cw->composite.num_children )
	    fprintf(stderr, "%sNo Children.\n", spaces );
	else
	{
	    fprintf(stderr, "%sChildren:\n", spaces );
	    for (i = 0 ; i < cw->composite.num_children ; i++ )
	        WcxPrintTree( cw->composite.children[i], depth );
	}
    }
}

void WcPrintTree( w )
    Widget w;
{
    WcxPrintTree( w, 0 );
}

/*
*******************************************************************************
* Private Data involving the root widget list, declared at top of this file
*	static int numRoots = 0;
*	static Widget rootWidgets[MAX_ROOT_WIDGETS];
*******************************************************************************
*/


/*  -- Forget about a root widget
*******************************************************************************
    When a root widget gets destroyed, we need to take that widget out
    of our list of root widgets.  This is a destroy callback routine
    which is added to a root widget's destroy callback list by WcRootWidget.
*/

static void ForgetRootCB( w, ignored, unused )
    Widget	w;
    XtPointer	ignored;
    XtPointer	unused;
{
    int i;
    for (i = 0 ; i < numRoots ; i++ )
    {
        if ( w == rootWidgets[i] )
	{
	    /* move all following widgets up to close the gap */
	    for ( ; i < numRoots ; i++ )
	    {
		rootWidgets[i] = rootWidgets[i+1];
	    }
	    numRoots-- ;
	    return;
	}
    }
    /* should never get here */
}

/*  -- Find root widget
*******************************************************************************
    If a widget is passed, then find the root of that widget.  See if
    it is one of the root widgets we already know about.  Add to list
    if not.  Return the root widget.

    If no widget is passed, then return the first root widget we know
    about.  If we know of no root widgets, then we will return a NULL
    since the rootWidgets[] array starts out filled with nulls, and
    gets re-filled as roots are destroyed.
*/

Widget WcRootWidget( w )
    Widget w;
{
    int i;

    if (w)
    {
	while ( XtParent(w) )
	    w = XtParent(w);

	for (i = 0 ; i < numRoots ; i++)
	{
	    if ( w == rootWidgets[i] )
		return w;
	}

	rootWidgets[i] = w;
	numRoots++;
	XtAddCallback( w, XtNdestroyCallback, ForgetRootCB, NULL );
	return w;
    }
    else
    {
	return rootWidgets[0];
    }
}

/* -- Equivalent to ANSI C library function strstr()
*******************************************************************************
*/

char* WcStrStr( s1, s2 )
    char* s1;
    char* s2;
{
    while (*s1)
    {
	if (*s1 == *s2)
	{
	    char* start = s1;
	    char* c = s2;
	    while (*++s1 & *++c && *s1 == *c)
		;
	    if (*c == '\0')
		return start;
	    else
		s1 = ++start;
	}
	else
	{
	    s1++ ;
	}
    }
    return (char*)0;
}

/* Safe forms of strlen, strcpy, strcat, strcmp, and strncmp (also streq)
** Some are macros in WcCreate.h
*/

char* WcStrCpy( str1, str2 )
    char* str1;
    char* str2;
{
    if ( str1 == NULL )
    {
	WcWARN( (Widget)0, "WcStrCpy", "nullTarget",
		"WcStrCpy() called with NULL target!");
	abort();
    }
    if ( str2 == NULL )
	*str1 = NUL;
    else
	str1 = strcpy( str1, str2 );

    return str1;
}

char* WcStrCat( str1, str2 )
    char* str1;
    char* str2;
{
    if ( str1 == NULL )
    {
	WcWARN( (Widget)0, "WcStrCat", "nullTarget",
		"WcStrCat() called with NULL target!");
	abort();
    }
    if ( str2 == NULL )
	return strcat( str1, "" );
    else
	return strcat( str1, str2 );
}

int WcStrCmp( str1, str2 )
    char* str1;
    char* str2;
{
    if ( WcNonNull(str1) && WcNonNull(str2) )
	return strcmp( str1, str2 );	/* str1 and str2 are non-null */
    if ( WcNull(str1) && WcNull(str2) )
	return 0;			/* both null, I guess that's equal */
    if ( WcNull(str1) )
	return -1;			/* a NULL is less than anything */
    /* if ( WcNull(str2) ) */
	return 1;			/* anything is greater than NULL */
}

int WcStrCmpN( str1, str2, num )
    char* str1;
    char* str2;
    int   num;
{
    if ( num > 0 && (WcNonNull(str1) && WcNonNull(str2)) )
	return strncmp( str1, str2, num);
    if ( num == 0 || (WcNull(str1) && WcNull(str2)) )
	return 0;			/* equal, I guess */
    if ( WcNull(str1) )
	return -1;			/* NULL is less than anything */
    /* if ( WcNull(str2) ) */
	return 1;			/* anything is greater than NULL */
}

/**************************************************************************
These functions make it easy to determine if a format string
can be used by printf, fprintf, or sprintf to print the appropriate
number of strings.  This allows error messages to be re-configured 
but remain safe at execution time.  This makes it much safer to change
error messages used by the WARN procedures.

It also allows textual values to be set on arbitrary widgets like this:

void RegisterNameDisplayCB( w, sprintFmt, ignored )
    Widget w; XtPointer sprintFmt, ignored;
{
    format = (char*)sprintFmt;
    if ( 2 != WcPrintfFormatStrings( format ) )
    {
	WARN( w, "RegisterNameDisplay", "needTwoStringFormats",
		"Sorry, you blew it!" )
	format = "this.labelString: Send %s To %s";
    }
}

void DisplaySendMessage( w, note, name )
    Widget w; String note, name;
{
    char buf[MAX_XRMSTRING];
    sprintf( buf, format, note, name );
    WcSetValues( w, buf );
}

See? If the user interface instead uses Athena widgets, the format string
passed from the RegisterNameDisplay() in the resource file passes something
like "this.label: Send %s to %s" or if the UI really wants the name to show
up in the shell, pass "~.title: Send %s to %s".
**************************************************************************/

/* Number Of Strings Which May Be Printf'd
**=========================================**
   Returns 0 if something other than strings are in the format
   specification.  Returns 0 if a string format specification contains a
   '*' which means the width or precision is parametric.  Therefore, a
   positive return value means printf() can be safely called with the
   returned number of strings as arguments following the format.
*/
int WcPrintfFormatStrings( cp )
    char* cp;
{
    int strings = 0;
#ifdef SUNOS_PRINTF
    int digitDollarSpec, field[10], i;
    for ( i = 1 ; i <= 9 ; i++ )	/* NB: 1 <= field <= 9 */
	field[i] = 0;
#endif

    while ( cp && *cp )
    {
	if ( *cp != '%' )
	    ++cp;
	else if ( *++cp == '%' )	/* look at character after %	*/
	    ++cp;			/* also a % - not a conversion	*/
	else
	{
	    /* conversion specification: already ate the %
	    */

#ifdef SUNOS_PRINTF
	    /* WARNING! Most machines do NOT support this %digit$ concept.
	    */
	    if ( '1' <= *cp && *cp <= '9' && *(cp+1) == '$' )
	    {
		digitDollarSpec = 1;
		field[*cp - '0']++;
		cp = cp+2;
	    }
	    else
		digitDollarSpec = 0;
#endif

	    /* optional flags:
	    */
	    while (*cp == '+' || *cp == '-' || *cp == ' ' || *cp == '#' )
		cp++;

	    /* optional field width:
	    */
	    if ( *cp == '*' )
		return 0;		/* No parametric field widths! */
	    while (*cp && '0' <= *cp && *cp <= '9')
		++cp;

	    /* optional precision:
	    */
	    if (*cp == '.')
	    {
		++cp;
		if ( *cp == '*' )
		    return 0;		/* No parametric precision! */
		while (*cp && '0' <= *cp && *cp <= '9')
		    ++cp;
	    }

	    /* optional long conversion
	    */
	    if (*cp == 'l')
		++cp;

	    /* type of conversion: anything but string returns zero
	    */
	    if (*cp != 's')
		return 0;

#ifdef SUNOS_PRINTF
	    if ( !digitDollarSpec )
		strings++;
#else
	    strings++;
#endif
	}
    }
#ifdef SUNOS_PRINTF
    /* If format spec named specific fields with %digit$ specification,
    ** then the number of strings is at least that number of fields.
    */
    for ( i = 1 ; i <= 9 ; i++ )
	if ( field[i] && strings < i )
	    strings = i;
#endif 
    return strings;
}
