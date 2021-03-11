/*
 * authorization hooks for the server
 *
 * $XConsortium: auth.c,v 1.8 89/12/13 14:42:27 keith Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
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

# include   "audio.h"
# include   "AFauth.h"
# include   "stdio.h"
# include   "misc.h"

struct protocol {
    unsigned short   name_length;
    char    *name;
    int     (*Add)();	    /* new authorization data */
    AID	    (*Check)();	    /* verify client authorization data */
    int     (*Reset)();	    /* delete all authorization data entries */
    AID	    (*ToID)();	    /* convert cookie to ID */
    int	    (*FromID)();    /* convert ID to cookie */
    int	    (*Remove)();    /* remove a specific cookie */
};

extern int  MitAddCookie ();
extern AID  MitCheckCookie ();
extern int  MitResetCookie ();
extern AID  MitToID ();
extern int  MitFromID (), MitRemoveCookie ();

#ifdef HASDES
extern int  AdmAddCookie ();
extern AID  AdmCheckCookie ();
extern int  AdmResetCookie ();
extern AID  AdmToID ();
extern int  AdmFromID (), AdmRemoveCookie ();
#endif

static struct protocol   protocols[] = {
{   (unsigned short) 18,    "MIT-MAGIC-COOKIE-1",
		MitAddCookie,	MitCheckCookie,	MitResetCookie,
		MitToID,	MitFromID,	MitRemoveCookie,
},
#ifdef HASDES
{   (unsigned short) 19,    "XDM-AUTHORIZATION-1",
		AdmAddCookie,	AdmCheckCookie,	AdmResetCookie,
		AdmToID,	AdmFromID,	AdmRemoveCookie,
},
#endif
};

# define NUM_AUTHORIZATION  (sizeof (protocols) /\
			     sizeof (struct protocol))

/*
 * Initialize all classes of authorization by reading the
 * specified authorization file
 */

static char *authorization_file = (char *)NULL;

static int  AuthorizationIndex = 0;
static ABool ShouldLoadAuth = TRUE;

InitAuthorization (file_name)
char	*file_name;
{
    authorization_file = file_name;
}

int
LoadAuthorization ()
{
    FILE    *f;
    AFauth   *auth;
    int	    i;
    int	    count = 0;

    ShouldLoadAuth = FALSE;
    if (!authorization_file)
	return 0;
    f = fopen (authorization_file, "r");
    if (!f)
	return 0;
    AuthorizationIndex = 0;
    while ( (auth = AFauReadAuth (f)) != NULL) {
	for (i = 0; i < NUM_AUTHORIZATION; i++) {
	    if (protocols[i].name_length == auth->name_length &&
		bcmp (protocols[i].name, auth->name, (int) auth->name_length) == 0)
	    {
		++count;
		(*protocols[i].Add) (auth->data_length, auth->data,
					 ++AuthorizationIndex);
	    }
	}
	AFauDisposeAuth (auth);
    }
    fclose (f);
    return count;
}

#ifdef XDMCP
/*
 * AdmcpInit calls this function to discover all authorization
 * schemes supported by the display
 */
RegisterAuthorizations ()
{
    int	    i;

    for (i = 0; i < NUM_AUTHORIZATION; i++)
	AdmcpRegisterAuthorization (protocols[i].name,
				    (int)protocols[i].name_length);
}
#endif

AID
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
    return (AID) ~0L;
}

ResetAuthorization ()
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++)
	(*protocols[i].Reset)();
    ShouldLoadAuth = TRUE;
}

AID
AuthorizationToID (name_length, name, data_length, data)
unsigned short name_length;
char *name;
unsigned short data_length;
char *data;
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++) {
    	if (protocols[i].name_length == name_length &&
	    bcmp (protocols[i].name, name, (int) name_length) == 0)
    	{
	    return (*protocols[i].ToID) (data_length, data);
    	}
    }
    return (AID) ~0L;
}

AuthorizationFromID (id, name_lenp, namep, data_lenp, datap)
AID id;
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
