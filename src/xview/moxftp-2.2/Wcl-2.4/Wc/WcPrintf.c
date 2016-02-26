
/* WcPrintf - Replacement for printf
****************************************************************************
   Allows the application to easily take control of warning output.
   Wcl provides two resources: wclWarningFileDescriptor, which can be an
   int or "stderr" or "stdout", and wclWarningFileName, defaults to NULL.
   If not null, then the file is opened and dup'd to use the desired
   file descriptior, otherwise the descriptor is used.

   The client (and libXmp etc) can provide replacements which send the
   messages to a widget.
*/

typedef void (*WcPrintfProc) _(( char*, va_list ));
extern void WcPrintf _(( char*, ... ));
extern WcPrintfProc WcSetPrintfProc _(( WcPrintfProc ));

static void WcxDefaultPrintf _(( char*, va_list ));
static WcPrintfProc Printf = WcxDefaultPrintf;
static FILE* wcxFileDescriptor;

WcPrintfProc WcSetPrintfProc( UserPrintfProc )
    WcPrintfProc	UserPrintfProc;
{
    WcPrintfProc OldPrintfProc = Printf;
    Printf = UserPrintfProc;
    return OldPrintfProc;
}

#ifdef _NO_VFPRINTF
#ifdef _NO_DOPRNT

/*
 * No way to do this reliably and portably.  If your machine is like this,
 * you need to fully implement vfprintf according to the ANSI spec so it
 * works on your specific machine.  Yuck!
 *
 * Luckily, there are probably no machines on earth still plugged in
 * which are this deficient.
 */

/*VARARGS1*/
static void WcPrintf( format, va_alist )
    char*  format;
    va_dcl
{
    if ( Printf == WcxDefaultPrintf )
	fprintf( wcxFileDescriptor, format, va_alist );

    va_list var;

    Va_start( var, format );
    Printf( format, var );
    va_end( var );
}

/*ARGSUSED*/
static void WcxDefaultPrintf( format, var )
    char*	format;
    va_list	var;
{
    (void)fprintf( wcxFileDescriptor, 
		   "No vprintf, no _doprnt: Cannot safely format <%s>\n",
		   format );
}

#else	/* the system does have _doprnt, but no vfprintf() */

/*
 * Implement vfprintf() using _doprnt
 */

int vfprintf( fd, format, var )
    FILE*	fd;
    char*	format;
    va_list	var;
{
    return _doprnt( format, var, fd );
}

#undef _NO_VFPRINTF

#endif /*_NO_DOPRNT*/
#endif /*_NO_VFPRINTF*/

#ifndef _NO_VFPRINTF
/*
 * All modern systems will end up here.
 */

static void WcxDefaultPrintf( format, var )
    char*	format;
    va_list	var;
{
    vfprintf( wcxFileDescriptor, format, var );
}


#if NeedFunctionPrototypes
void WcPrintf( 
    char*	format,
    ... )
#else
/*VARARGS1*/
void WcPrintf( format, va_alist )
    char*	format;
    va_dcl
#endif
{
    va_list var;

    Va_start( var, format );
    Printf( format, var );
    va_end( var );
}
#endif /*!_NO_VFPRINTF*/
