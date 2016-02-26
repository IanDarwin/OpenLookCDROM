#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) WcInvoke.c 1.5 92/03/18 11:02:21
*
* Widget Creation Library - WcInvoke.c
*
* This module implements functions which allow an action to easily invoke 
* an equivalent callback, and a callback to easily invoke an equivalent
* action.

* WcInvokeCallback converts action arguments (params and num_params) into
* callback arguments (a single string).  Any params with whitespace are 
* enclosed in double quotes( "like this").  The params are separated by single
* blanks.  It then invokes the XtCallbackProc.

* WcInvokeAction converts callback arguments (a single string) into action 
* arguments (params[] and num_params).  It then invokes the XtActionProc.
* The clientData string is broken up on whitespace and commas.  Repeated
* commas with only whitespace between causes NULL params.

* WcInvokeNamedAction does the same, but it invokes XtCallAction to 
* allow actions to be named, rather than requiring a pointer to an 
* XtActionProc.

*******************************************************************************
*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Wc/WcCreateP.h>

/*  -- Conversion Methods
=========================
*/

static char* WcxCvtParamsToClientData ( params, num_params )
    char**    params;
    Cardinal* num_params;
{
    char *cp, *pp, *clientData;
    int  len, par, needToQuote;

    if ( *num_params == 0 || params == NULL )
	return NULL;

    /* Compute the total length of the required equivalent string.
    ** Note that we need a separating blank, and we may need to
    ** quote the arg, hence the `+ 3' as we increment the len.
    */
    for ( len = par = 0  ;  par < *num_params  ;  par++ )
	if ( *(pp = params[par]) != '\0' )
	    for ( len +=3  ;  *pp  ;  pp++ )
		++len; /*looking for end of each parameter*/

    clientData = cp = (char*)XtMalloc( len+1 );

    /* Concatenate and possibly quote parameters into the clientData.
    */
    for ( par = 0  ;  par < *num_params  ;  par++ )
    {
	/* See if there are any special chars in
	** the param[par] which will require quoting.
	*/
	for ( needToQuote = 0, pp = params[par]  ;  *pp  ;  pp++ )
	{
	    if ( *pp == '\t' || *pp == ')' || *pp == ' '
	      || *pp == '\n' || *pp == ',' )
            {
		needToQuote++;
		break;
	    }
	}
	if ( needToQuote ) *cp++ = '"';

	for ( pp = params[par]  ;  *pp  ;  pp++ )
	    *cp++ = *pp;

	if ( needToQuote ) *cp++ = '"';

	*cp++ = ' ';
    }
    *--cp = '\0';	/* change final blank character to NUL */

    return clientData;
}

typedef struct _WcxParamsStruct {
    char**	params;
    Cardinal	num_params;
} WcxParamsStruct, *WcxParams;

static void WcxCvtClientDataToParams ( widget, clientData, wcxParams )
    Widget	widget;
    char*	clientData;
    WcxParams	wcxParams;	/* values filled in and returned */
{
    char*	params[MAX_ARGS];	/* must copy into alloc'd params[] */
    Cardinal	num_params	= 0;
    char*	data		= clientData;
    int		len;
    char	*cp, *end, quote;

    /* Skip initial whitespace.
    */
    while (*data  == ' ' || *data == '\t' || *data == '\n')
        data++;

    while ( *data && num_params < MAX_ARGS )
    {
	/* Check first character of each argument.
	*/
	switch (*data)
	{
	case ',' :
	    /* -- Null Argument is NULL param - NOT XtMalloc'd !!
	    */
	    params[num_params] = NULL;
	    data++;				/* eat the comma */
	    break;

	case '\"' : case '\'' :
	    /* -- Quoted argument: drop the quotes.
	    */
	    quote = *data;
	    data++;				/* skip quote */

	    for ( len = 0, end = data ; *end && *end != quote ; end++ )
		++len; /* find length and end of quoted string */

	    if (len == 0)
	    {
		/* nothing inside quotes is same as null argument.
		*/
		params[num_params] = NULL;
	    }
	    else
	    {
		/* end points to either the quote or the NULL terminator.
		*/
		cp = params[num_params] = XtMalloc( len+1 );
		while ( data < end )
		    *cp++ = *data++;
		*cp = '\0';
	    }

	    /* Now data points to either the quote or the NULL terminator.
	    */
	    if ( *data == NULL )
	    {
		WcWARN1( widget, "WcInvokeAction", "unbalQuote",
			"SomeCallback(%s) - unbalanced quotes.", clientData );
	    }
	    else
	    {
		/* Skip trailing quote, whitespace, optional comma:
		*/
		data++;
		while (*data && *data  == ' ' || *data == '\t' || *data == '\n')
		    data++;
		if ( *data == ',')
		    data++;
	    }

	    break;

	default:
	    /* -- Non-quoted, non-null argument.  Always take first character,
	    ** then everything up to whitespace or parens as the parameter.
	    */
	    for  ( len = 1, end = data, end++ ; 
		   *end && *end != '\t' && *end != '\n' && *end != ' '
			&& *end != ','  && *end != '('  && *end != ')' ; )
		end++, len++;
	    cp = params[num_params] = XtMalloc( len+1 );
	    while ( data < end )
		*cp++ = *data++;
	    *cp = '\0';

	    /* skip whitespace, optional comma
	    */
	    while (*data && *data  == ' ' || *data == '\t' || *data == '\n')
		data++;
	    if ( *data == ',')
		data++;

	    break;
	}

	/* skip whitespace which may follow optional comma we've already skipped
	*/
	while (*data && *data  == ' ' || *data == '\t' || *data == '\n')
	    data++;

	++num_params;
    }
    wcxParams->num_params = num_params;

    if ( 0 == num_params )
	wcxParams->params = (char**)NULL;
    else
    {
	/* Malloc storage for params, and copy from automatic params.
	*/
	wcxParams->params = (char**)XtMalloc(sizeof(char*) * num_params);

	while( num_params-- )
	wcxParams->params[num_params] = params[num_params];
    }
}

/*
*******************************************************************************
* Public definitions of WcInvokeCallback, WcInvokeAction, WcInvokeNamedAction
*******************************************************************************
*/

/*  -- Invoke XtActionProc from XtCallbackProc 
*******************************************************************************
*/

void WcInvokeCallback( Callback, widget, params, num_params )
    XtCallbackProc	Callback;
    Widget   		widget;
    char**		params;
    Cardinal*		num_params;
{
    char* clientData = WcxCvtParamsToClientData( params, num_params );

    Callback( widget, clientData, (XtPointer)NULL );

    if ( NULL != clientData )
	XtFree( clientData );
}

/*  -- Invoke XtActionProc from XtCallbackProc 
*******************************************************************************
*/

void WcInvokeAction( Action, widget, clientData )
    XtActionProc Action;
    Widget       widget;
    char*        clientData;
{
    WcxParamsStruct	wcxParams;
    Cardinal		num_params;	/* must copy cuz Action can change */

    WcxCvtClientDataToParams( widget, clientData, &wcxParams );
    num_params = wcxParams.num_params;

    Action( widget, (XEvent*)NULL, wcxParams.params, &num_params );

    if ( NULL != wcxParams.params )
    {
	while ( wcxParams.num_params-- )
	    if ( NULL != wcxParams.params[wcxParams.num_params] )
		XtFree( wcxParams.params[wcxParams.num_params] );
	XtFree( (char*)wcxParams.params );
    }
}

/*  -- Invoke Named XtActionProc from XtCallbackProc 
*******************************************************************************
*/

void WcInvokeNamedAction( actionName, widget, clientData )
    char*  actionName;
    Widget widget;
    char*  clientData;
{
#ifdef XtSpecificationRelease
    WcxParamsStruct	wcxParams;
    Cardinal		num_params;	/* must copy cuz Action can change */

    WcxCvtClientDataToParams( widget, clientData, &wcxParams );
    num_params = wcxParams.num_params;

    XtCallActionProc( widget, actionName,
		      (XEvent*)NULL, wcxParams.params, num_params );

    if ( NULL != wcxParams.params )
    {
	while ( wcxParams.num_params-- )
	    if ( NULL != wcxParams.params[wcxParams.num_params] )
		XtFree( wcxParams.params[wcxParams.num_params] );
	XtFree( (char*)wcxParams.params );
    }
#else
    WcWARN1( widget, "WcInvokeNamedAction", "oldXt",
	   "WcInvokeNamedAction(%s) requires Xt version 4.0 or later.",
	    actionName );
#endif
}
