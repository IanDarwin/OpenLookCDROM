/* $XConsortium: err_loc.c,v 5.3 94/04/17 20:41:32 rws Exp $ */

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

#include "phg.h"

/* check for both EAGAIN and EWOULDBLOCK, because some supposedly POSIX
 * systems are broken and return EWOULDBLOCK when they should return EAGAIN
 */
#if defined(EAGAIN) && defined(EWOULDBLOCK)
#define ETEST(err) (err == EAGAIN || err == EWOULDBLOCK)
#else
#ifdef EAGAIN
#define ETEST(err) (err == EAGAIN)
#else
#define ETEST(err) (err == EWOULDBLOCK)
#endif
#endif

extern Err_handle	phg_erh_create();
extern int		phg_err_pex_reply();
extern int		phg_err_x();

static void
err_destroy_local( erh)
    Err_handle		erh;
{
    if ( erh->data.local.fname)
	free( erh->data.local.fname);
    free( (char *)erh);
}

static void
err_destroy_client( erh)
    Err_handle		erh;
{
    if ( erh->data.client.fname)
	free( erh->data.client.fname);
    free( (char *)erh);
}

static void
err_buf_local( erh, errnum)	/* error mode checked before calling */
    Err_handle	erh;
    Pint	errnum;
{
    /* call the PHIGS error handler */
    ERR_HANDLE( errnum, erh->cur_func_num, erh->data.local.fname);
}

static void
err_buf_client( erh, errnum)	/* error mode checked before calling */
    Err_handle	erh;
    Pint	errnum;
{
    /* Don't do it if an inquiry. */
    if ( erh->cur_func_num != Pfn_INQUIRY )
	ERR_HANDLE( errnum, erh->cur_func_num, erh->data.client.fname);
}

static void
err_flush_client( erh)
    Err_handle	erh;
{
    register int	cnt, wc, s = erh->data.client.sfd;
    Err_msg		message;
    register Err_msg	*msg = &message;

    if ( erh->data.client.sfd >= 0) {
	cnt = 0;
	while (1) {
	    if ( (wc = read( s, (char *)(msg + cnt),
		    sizeof(message) - cnt)) > 0) {
		cnt += wc;
		if ( cnt == sizeof(message)) {
		    ERR_HANDLE( msg->errnum, msg->funcnum,
			erh->data.client.fname);
		    cnt = 0;
		}
	    } else if ( wc < 0 &&
		( ETEST(errno) && cnt || errno == EINTR)) {
		    continue;
	    } else
		break;
	}
    }
}

static void
err_report_client( erh, errnum)
    Err_handle	erh;
    Pint	errnum;
{
    err_flush_client( erh);
    /* Don't do it if an inquiry. */
    if ( erh->cur_func_num != Pfn_INQUIRY)
	ERR_HANDLE( errnum, erh->cur_func_num, erh->data.client.fname);
}

Err_handle
phg_err_init_local( err_file)
    char		*err_file;	/* !assumes name is valid! */
{
    Err_handle		erh;

    if ( erh = phg_erh_create()) {
	if ( !phg_err_store_name( erh, err_file, &erh->data.local.fname)) {
		free( (char *)erh);
		erh = NULL;
	} else {
	    erh->buf_func = err_buf_local;
	    erh->flush_func = NULL;	/* errors not buffered for local */
	    erh->report_func = err_buf_local;
	    erh->destroy_func = err_destroy_local;
	    erh->cur_func_num = Pfn_open_phigs;
	}
    } else {
	ERR_HANDLE( ERR900, Pfn_open_phigs, err_file);
    }
    return erh;
}

Err_handle
phg_err_init_client( err_file)		/* !assumes name is valid! */
    char	*err_file;
{
    Err_handle		erh;
    extern void		PexSetErrorHandler();

    if ( (erh = phg_erh_create())) {
	if ( !phg_err_store_name( erh, err_file, &erh->data.client.fname)) {
		free( (char *)erh);
		erh = NULL;
	} else {
	    erh->buf_func = err_buf_client;
	    erh->flush_func = err_flush_client;
	    erh->report_func = err_report_client;
	    erh->destroy_func = err_destroy_client;
	    erh->cur_func_num = Pfn_open_phigs;
	    /* the error flush function will only read the error socket if
	     * the fd is >= 0, which is true if the server startup works.
	     */
	    erh->data.client.sfd = -1;
	    (void)PexSetErrorHandler( (caddr_t)erh, phg_err_pex_reply );
	    (void)XSetErrorHandler( phg_err_x );
	}
    } else {
	ERR_HANDLE( ERR900, Pfn_open_phigs, err_file);
    }
    return erh;
}
