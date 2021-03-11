/* $XConsortium: pex_util.c,v 5.4 94/04/17 20:42:18 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include "pex_priv.h"

/* Global list of servers using the PEX extension. */
Pex_srvr_info	*Pex_srvr_list;

#define INIT_SCRATCH_SIZE	8096


Pointer
PexGrowScratch( srvr, size )
    Pex_srvr_info	*srvr;
    unsigned		size;
{
    if ( srvr->scratch )
	XFree((char *)srvr->scratch);

    if ( !(srvr->scratch = (Pointer)Xmalloc( srvr->scratch_size = size )) )
	srvr->scratch_size = 0;

    return srvr->scratch;
}


static void
display_close_proc( display, ec )
    Display	*display;
    XExtCodes	*ec;
{
    register Pex_srvr_info	**srvrp;

    /* Find the entry corresponding to the specified display and remove it. */
    for ( srvrp = &Pex_srvr_list; *srvrp; srvrp = &(*srvrp)->next ) {
	if ( (*srvrp)->display == display ) {
	    if ( (*srvrp)->scratch )
		XFree( (char *)(*srvrp)->scratch );
	    XFree( (char *)*srvrp );
	    *srvrp = (*srvrp)->next;
	    break;
	}
    }
}


/* Data for client error handler */
static caddr_t	client_data;
static int	(*client_error_handler)();

static int
pex_error_handler( display, err, codes, ret_code )
    Display	*display;
    xError	*err;
    XExtCodes	*codes;
    int		*ret_code;
{
    if ( client_error_handler )
	return( (*client_error_handler)
	    ( display, err, codes, ret_code, client_data ) );
    else
	return 0;
}

void
PexSetErrorHandler( data, proc )
    caddr_t	data;
    int		(*proc)();
{
    client_data = data;
    client_error_handler = proc;
}


XExtCodes*
PexGetExtCodes( display )
    Display	*display;
{
    Pex_srvr_info	*srvrp;

    if ( srvrp = PexEntryCheck( display, 0 ) )
	return( srvrp->ext_codes );
    else
	return (XExtCodes *)NULL;
}

Pex_srvr_info*
PexEntryCheck( display, add_it )
    Display	*display;
    int		add_it;
{
    register Pex_srvr_info	**srvrp;

    /* Find the entry corresponding to the specified display. */
    for ( srvrp = &Pex_srvr_list; *srvrp; srvrp = &(*srvrp)->next ) {
	if ( (*srvrp)->display == display )
	    break;
    }

    /* Return the info if found or add to the list if not found. */
    if ( !(*srvrp) && add_it ) {
	Pex_srvr_info	*new;

	if ( new = (Pex_srvr_info *)Xcalloc( 1, sizeof(Pex_srvr_info) ) ) {
	    if ( !PEX_SCRATCH( new, INIT_SCRATCH_SIZE ) ) {
		XFree( (char *)new );
	    } else {
		new->display = display;
		new->next = (Pex_srvr_info*)NULL;
		if ( new->ext_codes =
		    XInitExtension( display, PEX_NAME_STRING ) ) {
		    *srvrp = new;	/* add it to the list */
		    (void)XESetError( display, new->ext_codes->extension,
			pex_error_handler );
		    (void)XESetCloseDisplay( display, new->ext_codes->extension,
			(int(*)())display_close_proc );
#ifdef DEBUG
    {	extern char	*getenv();
	if ( getenv( "PEX_SI_API_SHOW_FIRST_ERR" ) )
	    fprintf( stderr, "PEX first error = %d\n",
		new->ext_codes->first_error );
    }
#endif
		} else {
		    XFree( (char *)new );
		    XFree( (char *)new->scratch );
		}
	    }
	}
    }

    return *srvrp;
}

void
PexClearReply( display, length )
    Display	*display;
    CARD32	length;	/* in units of CARD32's */
{
    CARD32	dummy;

    /* Clear out all the unread data on the connection. */
    for ( ; length; length-- )
	_XRead( display, (char*)&dummy, (long)sizeof(dummy) );
}


int
PEXResourceIdNoReplyFunc(opcode, display, resource)
int	      opcode;
Display      *display;
XID           resource;
{
    int               status = 0;
    Pex_srvr_info    *srvr;
    pexResourceReq   *req;
 
    if (srvr = PexEntryCheck(display, 1)) {
	LOCK_DISPLAY(display);
	/** Create a generic resource id request, and then substitute
	 ** in the appropriate opcode  **/
        PEX_RESOURCE_ID_REQUEST(CreateNameSet, display,
                PEX_OPCODE(srvr), resource, req);
	req->opcode = opcode;
        status = 1;
	UNLOCK_DISPLAY(display);
    }
    PEX_SYNC_HANDLE(display);
    return status;
}
