/*
 * MIT-MAGIC-COOKIE-1 authorization scheme
 *
 * $XConsortium: mitauth.c,v 1.3 89/03/14 15:53:36 rws Exp $
 *
 * Copyright 1988, 1990 Massachusetts Institute of Technology, and 
 * Digital Equipment
 * Coporation
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

#include "audio.h"
#include "os.h"

static struct auth {
    struct auth	*next;
    unsigned short	len;
    char	*data;
    AID		id;
} *mit_auth;

int
MitAddCookie (data_length, data, id)
unsigned short	data_length;
char	*data;
AID	id;
{
    struct auth	*new;

    new = (struct auth *) xalloc (sizeof (struct auth));
    if (!new)
	return 0;
    new->data = (char *) xalloc ((unsigned) data_length);
    if (!new->data) {
	xfree(new);
	return 0;
    }
    new->next = mit_auth;
    mit_auth = new;
    bcopy (data, new->data, (int) data_length);
    new->len = data_length;
    new->id = id;
    return 1;
}

AID
MitCheckCookie (data_length, data)
unsigned short	data_length;
char	*data;
{
    struct auth	*auth;

    for (auth = mit_auth; auth; auth=auth->next) {
        if (data_length == auth->len &&
	   bcmp (data, auth->data, (int) data_length) == 0)
	    return auth->id;
    }
    return (AID) -1;
}

int
MitResetCookie ()
{
    struct auth	*auth, *next;

    for (auth = mit_auth; auth; auth=next) {
	next = auth->next;
	xfree (auth->data);
	xfree (auth);
    }
    mit_auth = 0;
}

AID
MitToID (data_length, data)
unsigned short	data_length;
char	*data;
{
    struct auth	*auth;

    for (auth = mit_auth; auth; auth=auth->next) {
	if (data_length == auth->len &&
	    bcmp (data, auth->data, data_length) == 0)
	    return auth->id;
    }
    return (AID) -1;
}

MitFromID (id, data_lenp, datap)
AID id;
unsigned short	*data_lenp;
char	**datap;
{
    struct auth	*auth;

    for (auth = mit_auth; auth; auth=auth->next) {
	if (id == auth->id) {
	    *data_lenp = auth->len;
	    *datap = auth->data;
	    return 1;
	}
    }
    return 0;
}

MitRemoveCookie (data_length, data)
unsigned short	data_length;
char	*data;
{
    struct auth	*auth, *prev;

    prev = 0;
    for (auth = mit_auth; auth; auth=auth->next) {
	if (data_length == auth->len &&
	    bcmp (data, auth->data, data_length) == 0)
 	{
	    if (prev)
		prev->next = auth->next;
	    else
		mit_auth = auth->next;
	    xfree (auth->data);
	    xfree (auth);
	    return 1;
	}
    }
    return 0;
}
