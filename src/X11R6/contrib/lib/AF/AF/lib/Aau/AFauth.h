/*
 * $RCS: Xauth.h,v 1.4 89/12/12 17:15:08 rws Exp $
 *
 * Copyright 1988, 1990 Massachusetts Institute of Technology and
 * Digital Equipment Coporation.
 * All rights reserved.
 *
 * Author:  Keith Packard, MIT X Consortium
 */
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */


#ifndef __cplusplus				/* DBW: added this */
#ifdef mips
#ifndef	sgi
#define const
#endif
#endif
#endif

#ifndef __STDC__
#ifdef	sparc
#define	const					/* rsp noted broken. */
#endif
#endif


#ifndef NeedWidePrototypes
#if defined(NARROWPROTO)
#define NeedWidePrototypes 0
#else
#define NeedWidePrototypes 1		/* default to make interropt. easier */
#endif
#endif

#include <stdio.h>
#include <stdlib.h>		/* Pick up free proto.	*/
#include <string.h>		/* Pick up strlen... protos. */
#include <unistd.h>		/* Pick up access proto. */

# define AFFamilyLocal (256)	/* not part of X standard (i.e. X.h) */
# define AFFamilyWild  (65535)

typedef struct aauth {
    unsigned short   family;
    unsigned short   address_length;
    const char 	    *address;
    unsigned short   number_length;
    const char 	    *number;
    unsigned short   name_length;
    const char 	    *name;
    unsigned short   data_length;
    const char	    *data;
} AFauth;

#ifdef __cplusplus			/* do not leave open across includes */
extern "C" {					/* for C++ V2.0 */
#endif

char *AFauFileName();

AFauth *AFauReadAuth(
	FILE*	/* auth_file */
	);

int AFauLockAuth(
	const char*	/* file_name */,
	int		/* retries */,
	int		/* timeout */,
	long		/* dead */
	);
	
void AFauUnlockAuth(
	const char*	/* file_name */
	);
	
int AFauWriteAuth(
	FILE*		/* auth_file */,
	AFauth*		/* auth */
	);
	
AFauth *AauGetAuthByName(
	const char*	/* display_name */
	);
	
AFauth *AFauGetAuthByAddr(
#if NeedWidePrototypes
	unsigned int	/* family */,
	unsigned int	/* address_length */,
#else
	unsigned short	/* family */,
	unsigned short	/* address_length */,
#endif
	const char*	/* address */,
#if NeedWidePrototypes
	unsigned int	/* number_length */,
#else
	unsigned short	/* number_length */,
#endif
	const char*	/* number */,
#if NeedWidePrototypes
	unsigned int	/* name_length */,
#else
	unsigned short	/* name_length */,
#endif
	const char*	/* name */
	);
	
void AFauDisposeAuth(
	AFauth*		/* auth */
	);

#ifdef __cplusplus
}						/* for C++ V2.0 */
#endif

/* Return values from AFauLockAuth */

# define LOCK_SUCCESS	0	/* lock succeeded */
# define LOCK_ERROR	1	/* lock unexpectely failed, check errno */
# define LOCK_TIMEOUT	2	/* lock failed, timeouts expired */

