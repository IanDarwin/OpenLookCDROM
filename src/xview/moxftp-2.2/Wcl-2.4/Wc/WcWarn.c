#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcWarn.c 1.3 92/10/12 08:29:41
*
* Widget Creation Library - WcWarn.c
*
* Implements simple procedures for printing warning messages similar to
* XtWarningMsg().  The number or string substitutions are checked for
* run-time safety, and no fixed sized buffers are used (instead, fprintf()
* is called multiple times which may be inefficient, but who cares for
* warning messages!).
*
* Error messages can be changed on a system wide basis be changing the Wcl
* error database file.  Note the way messages can be changed:
*	<func>.<msgName>:	<msg>
*
*******************************************************************************
*/

#include <X11/Intrinsic.h>
#include <X11/Wc/WcCreateP.h>

/* If only VarargsI.h was a standard include file...
*/
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

/* Wcl Xrm Error Database
**========================**
   Note that it is safe if no ErrorDb has been mapped to the app.
*/
static MapAgRec errDbAgentRec = MapAg_STATIC;
       MapAg    errDbAgent    = &errDbAgentRec;

#define WcMapErrorDb( app, errorDb ) \
	MapAg_Define( errDbAgent, app, NULL, NULL, errorDb )

#define WcMapErrorDbFind( app ) \
	(XrmDatabase)MapAg_Find( errDbAgent, app, NULL, NULL )

/* Initialize ErrorDB
**====================**
*/
void WcWarningInitialize( app, wcl )
    XtAppContext	app;
    WclRecPtr		wcl;
{
    if ( (String)0 != wcl->errorDatabaseFile )
    {
	XrmDatabase errorDB = XrmGetFileDatabase( wcl->errorDatabaseFile );
	WcMapErrorDb( app, errorDB );
    }
}

/* Get Error Message From ErrorDB
**================================**
   Abort if trashed arguments, as this is a programming error.
*/
String WcErrorDatabaseText( w, func, msgName )
    Widget w;
    String func, msgName;
{
    XrmDatabase errorDB;
    char	func_name[BUFSIZ];
    String	ignored_type;
    XrmValue	result;
    XtAppContext app;

    if ( (Widget)0 == w )
	w = WcRootWidget(0);

    app = XtWidgetToApplicationContext(w);

    if ( BUFSIZ < WcStrLen(func) + WcStrLen(msgName) + 2 )
    {
	fprintf(stderr,
	    "Wcl Error: WcErrorDatabaseText: strlen(func+msgName) > BUFSIZ\n");
	abort();
    }

    sprintf( func_name, "%s.%s", func, msgName );

    if ( (XrmDatabase)0 != (errorDB = WcMapErrorDbFind(app) ) )
    {
	XrmGetResource( errorDB,
		func_name, func_name,
		&ignored_type, &result );
	return (String)result.addr;
    }
    else
	return (String)0;
}

void WcWARN( w, func, msgName, defMsg )
    Widget w;
    String func, msgName, defMsg;
{
    String dbMsg = WcErrorDatabaseText( w, func, msgName );

    if ( WcNonNull(dbMsg) )
	fprintf( stderr, "Wcl Warning: %s\n", dbMsg );
    else
	fprintf( stderr, "Wcl Warning: %s\n", defMsg );
}

void WcWARN1( w, func, msgName, defMsg, a1 )
    Widget w;
    String func, msgName, defMsg, a1;
{
    int		edbStrs, defStrs;
    String	dbMsg = WcErrorDatabaseText( w, func, msgName );

    if ( WcNonNull(dbMsg) )
    {
	if ( 1 == ( edbStrs = WcPrintfFormatStrings( dbMsg ) ) )
	{
	    fprintf( stderr, "Wcl Warning: " );
	    fprintf( stderr, dbMsg, a1 );
	    fprintf( stderr, "\n" );
	}
	else
	{
	    fprintf( stderr, "Wcl Warning: Wcl Error DB Problem: Wrong number of `%%s' in message from ErrorDB ( expected 1, found %d )\n", edbStrs );
	    fprintf( stderr, "Wcl Warning: %s (%s)\n", dbMsg, a1 );
	}
    }
    else
    {
	if ( 1 == ( defStrs = WcPrintfFormatStrings( defMsg ) ) )
	{
	    fprintf( stderr, "Wcl Warning: " );
	    fprintf( stderr, defMsg, a1 );
	    fprintf( stderr, "\n" );
	}
	else
	{
	    fprintf( stderr, "Wcl Error: Programming Error: Wrong number of `%%s' in default message ( expected 1, found %d )\n", defStrs );
	    fprintf( stderr, "Wcl Warning: %s (%s)\n", defMsg, a1 );
	    abort();
	}
    }
}

void WcWARN2( w, func, msgName, defMsg, a1, a2 )
    Widget w;
    String func, msgName, defMsg, a1, a2;
{
    int		edbStrs, defStrs;
    String	dbMsg = WcErrorDatabaseText( w, func, msgName );

    if ( WcNonNull(dbMsg) )
    {
	if ( 2 == ( edbStrs = WcPrintfFormatStrings( dbMsg ) ) )
	{
	    fprintf( stderr, "Wcl Warning: " );
	    fprintf( stderr, dbMsg, a1, a2 );
	    fprintf( stderr, "\n" );
	}
	else
	{
	    fprintf( stderr, "Wcl Warning: Wcl Error DB Problem: Wrong number of `%%s' in message from ErrorDB ( expected 2, found %d )\n", edbStrs );
	    fprintf( stderr, "Wcl Warning: %s (%s) (%s)\n", dbMsg, a1, a2 );
	}
    }
    else
    {
	if ( 2 == ( defStrs = WcPrintfFormatStrings( defMsg ) ) )
	{
	    fprintf( stderr, "Wcl Warning: " );
	    fprintf( stderr, defMsg, a1, a2 );
	    fprintf( stderr, "\n" );
	}
	else
	{
	    fprintf( stderr, "Wcl Error: Programming Error: Wrong number of `%%s' in default message ( expected 2, found %d )\n", defStrs );
	    fprintf( stderr, "Wcl Warning: %s (%s) (%s)\n", defMsg, a1, a2 );
	    abort();
	}
    }
}

void WcWARN3( w, func, msgName, defMsg, a1, a2, a3 )
    Widget w;
    String func, msgName, defMsg, a1, a2, a3;
{
    int		edbStrs, defStrs;
    String	dbMsg = WcErrorDatabaseText( w, func, msgName );

    if ( WcNonNull(dbMsg) )
    {
	if ( 3 == ( edbStrs = WcPrintfFormatStrings( dbMsg ) ) )
	{
	    fprintf( stderr, "Wcl Warning: " );
	    fprintf( stderr, dbMsg, a1, a2, a3 );
	    fprintf( stderr, "\n" );
	}
	else
	{
	    fprintf( stderr, "Wcl Warning: Wcl Error DB Problem: Wrong number of `%%s' in message from ErrorDB ( expected 3, found %d )\n", edbStrs );
	    fprintf( stderr,
			"Wcl Warning: %s (%s) (%s) (%s)\n", dbMsg, a1, a2, a3 );
	}
    }
    else
    {
	if ( 3 == ( defStrs = WcPrintfFormatStrings( defMsg ) ) )
	{
	    fprintf( stderr, "Wcl Warning: " );
	    fprintf( stderr, defMsg, a1, a2, a3 );
	    fprintf( stderr, "\n" );
	}
	else
	{
	    fprintf( stderr, "Wcl Error: Programming Error: Wrong number of `%%s' in default message ( expected 3, found %d )\n", defStrs );
	    fprintf( stderr, 
			"Wcl Warning: %s (%s) (%s) (%s)\n", defMsg, a1, a2, a3);
	    abort();
	}
    }
}
