/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auth.c,v 1.3 1993/08/28 00:34:10 lemke Exp $
 */
/*
 * Some portions derived from:
 *
 * authorization hooks for the server
 *
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

# include   <audio/audio.h>
# include   <X11/Xauth.h>
# include   "misc.h"

struct protocol {
    unsigned short   name_length;
    char    *name;
    int     (*Add)();	    /* new authorization data */
    AuID	    (*Check)();	    /* verify client authorization data */
    int     (*Reset)();	    /* delete all authorization data entries */
    AuID	    (*ToID)();	    /* convert cookie to ID */
    int	    (*FromID)();    /* convert ID to cookie */
    int	    (*Remove)();    /* remove a specific cookie */
};

extern int  MitAddCookie ();
extern AuID  MitCheckCookie ();
extern int  MitResetCookie ();
extern AuID  MitToID ();
extern int  MitFromID (), MitRemoveCookie ();

static struct protocol   protocols[] = {
{   (unsigned short) 18,    "MIT-MAGIC-COOKIE-1",
		MitAddCookie,	MitCheckCookie,	MitResetCookie,
		MitToID,	MitFromID,	MitRemoveCookie,
},
};

# define NUM_AUTHORIZATION  (sizeof (protocols) /\
			     sizeof (struct protocol))

/*
 * Initialize all classes of authorization by reading the
 * specified authorization file
 */

static char *authorization_file = (char *)NULL;

static int  AuthorizationIndex = 0;
static Bool ShouldLoadAuth = TRUE;

InitAuthorization (file_name)
char	*file_name;
{
    authorization_file = file_name;
}

int
LoadAuthorization ()
{
#ifdef notyet
    FILE    *f;
    Xauth   *auth;
    int	    i;
    int	    count = 0;

    ShouldLoadAuth = FALSE;
    if (!authorization_file)
	return 0;
    f = fopen (authorization_file, "r");
    if (!f)
	return 0;
    AuthorizationIndex = 0;
    while (auth = XauReadAuth (f)) {
	for (i = 0; i < NUM_AUTHORIZATION; i++) {
	    if (protocols[i].name_length == auth->name_length &&
		bcmp (protocols[i].name, auth->name, (int) auth->name_length) == 0)
	    {
		++count;
		(*protocols[i].Add) (auth->data_length, auth->data,
					 ++AuthorizationIndex);
	    }
	}
	XauDisposeAuth (auth);
    }
    fclose (f);
    return count;
#else
    return 0;
#endif /* notyet */
}


AuID
CheckAuthorization (name_length, name, data_length, data)
unsigned short	name_length;
char	*name;
unsigned short	data_length;
char	*data;
{
    int	i;

    if (ShouldLoadAuth)
    {
	if (!LoadAuthorization())
	    EnableLocalHost ();
    }
    if (name_length)
	for (i = 0; i < NUM_AUTHORIZATION; i++) {
	    if (protocols[i].name_length == name_length &&
		bcmp (protocols[i].name, name, (int) name_length) == 0)
	    {
		return (*protocols[i].Check) (data_length, data);
	    }
	}
    return (AuID) ~0L;
}

ResetAuthorization ()
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++)
	(*protocols[i].Reset)();
    ShouldLoadAuth = TRUE;
}

AuID
AuthorizationToID (name_length, name, data_length, data)
unsigned short	name_length;
char	*name;
unsigned short	data_length;
char	*data;
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++) {
    	if (protocols[i].name_length == name_length &&
	    bcmp (protocols[i].name, name, (int) name_length) == 0)
    	{
	    return (*protocols[i].ToID) (data_length, data);
    	}
    }
    return (AuID) ~0L;
}

AuthorizationFromID (id, name_lenp, namep, data_lenp, datap)
AuID id;
unsigned short	*name_lenp;
char	**namep;
unsigned short	*data_lenp;
char	**datap;
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++) {
	if ((*protocols[i].FromID) (id, data_lenp, datap)) {
	    *name_lenp = protocols[i].name_length;
	    *namep = protocols[i].name;
	    return 1;
	}
    }
    return 0;
}

RemoveAuthorization (name_length, name, data_length, data)
unsigned short	name_length;
char	*name;
unsigned short	data_length;
char	*data;
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++) {
    	if (protocols[i].name_length == name_length &&
	    bcmp (protocols[i].name, name, (int) name_length) == 0)
    	{
	    return (*protocols[i].Remove) (data_length, data);
    	}
    }
    return 0;
}

AddAuthorization (name_length, name, data_length, data)
unsigned short	name_length;
char	*name;
unsigned short	data_length;
char	*data;
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++) {
    	if (protocols[i].name_length == name_length &&
	    bcmp (protocols[i].name, name, (int) name_length) == 0)
    	{
	    return (*protocols[i].Add) (data_length, data,
					++AuthorizationIndex);
    	}
    }
    return 0;
}
