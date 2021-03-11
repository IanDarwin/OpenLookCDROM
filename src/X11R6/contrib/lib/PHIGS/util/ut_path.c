/* $XConsortium: ut_path.c,v 5.7 94/04/17 20:42:22 rws Exp $ */

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

#include "phg.h"		/* includes Intrinsic.h and Xos.h */

#if defined(SYSV) && defined(SYSV386)
#include <net/errno.h>
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif

#ifndef PEXAPIDIR
#define PEXAPIDIR "/usr/lib/X11/PEX"
#endif

/* The constant below is based on the max value given in access(1) */
#define PHG_MAXPATH	1023


char *
phg_path( fname, erh, test)
    char		*fname;
    Err_handle		erh;
    int			test;
{
    /* Return a full path to "fname" and possibly test for existence of
     * the file.  The path is made from the environment variable PEXAPIDIR
     * if it's specified, else from the PEXAPIDIR define.
     *
     * The PEXAPIDIR path is stored in a static area to avoid lookup next
     * time it's needed.
     *
     * If "test" is true, the existence of the file is checked and NULL is
     * returned if it's not found.
     *
     * If erh is NULL, no errors will be reported.
     */

     /* TODO: put in the correct error codes. */

    static char		dir[PHG_MAXPATH + 1];
    static int		dir_length;

    register char	*str;
    int			err;

    if ( !*dir) {	/* get the path */
	if ( !(str = getenv("PEXAPIDIR")))
	    str = PEXAPIDIR;
	strncpy( dir, str, PHG_MAXPATH - 1);

	if ( *dir)
	    strcat( dir, "/");
	else
	    strcpy( dir, "./");
	dir_length = strlen(dir);
    }
    str = dir;

    /* see if the full path is too big for the file system */
    if ( dir_length + strlen(fname) > PHG_MAXPATH) {
	str = NULL;
	if ( erh) {
	    ERR_BUF( erh, ERRN52);
	}
	*dir = '\0';

    } else {
	strcpy( &dir[dir_length], fname);

	if ( test && access( dir, F_OK) < 0) {
	    str = NULL;
	    switch ( errno) {
		case ENOENT:		err = ERRN54; break;
		case ENOTDIR:		err = ERRN53; break;
		case EACCES:		err = ERRN53; break;
		case ENAMETOOLONG:	err = ERRN52; break;
		case ELOOP:		err = ERRN53; break;
		default:
		    err = ERRN54;
		    break;
	    }
	    if ( erh) {
		ERR_BUF( erh, err);
	    }
	}
    }

    return str;
}
