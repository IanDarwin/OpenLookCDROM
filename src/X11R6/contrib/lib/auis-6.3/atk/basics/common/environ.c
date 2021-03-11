/*LIBS: -lutil
*/

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/environ.c,v 2.12 1993/07/05 18:51:54 rr2b Exp $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 

/* environ.c
 * Class procedures to get at user environment and preference parameters.
 */


#include <class.h>
#include <environ.eh>

extern ProgramName[];	/* blechhh */

/* used to avoid problems with null pointers expected to be
  treated as null strings */
static char *nullstring="";

void environ__SetProgramName(classID,s)
struct classheader *classID;
char *s;
{
    if(s==NULL) s=nullstring;
    strcpy(ProgramName,s);
}

/* Quick and simple routine for checking if a environment entry is for a given
 * variable, probably faster than raw strlen and strncmp combinations.
 */
static boolean varcmp(variable, envEntry)
    register char *variable;
    register char *envEntry;
{

    while (*variable != '\0' && *variable++ == *envEntry++)
	;
    return (*variable == '\0' && (*envEntry == '=' || *envEntry == '\0'));
}

/* This function inherently leaks core under certain circumstances. This
 * shouldn't be a problem since it shouldn't be called too much.
 */
void environ__Put(classID, variable, value)
    struct classheader *classID;
    char   *variable, *value;
{

    extern char **environ;
/* lastEnviron and lastEnvironLength are an attempt to do intelligent allocation
 * of environment space while still maintaining compatability with the UNIX
 * notion of an envrment. We check them each time we add an element because
 * it is possible (but very unlikely) that some other piece of code has
 * replaced the environment since we last touched it...
 */
    static char **lastEnviron = NULL; /* The last environment we allocated. */
    static int lastEnvironLength = 0; /* The maximum number of entries in lastEnviron. */
    register char **p;

    /* check for a NULL value so we can make it the expected null string */
    if(value==NULL) value=nullstring;
    for (p = environ; *p != NULL && !varcmp(variable, *p); p++)
	;

    if (*p == NULL) {
        if (environ != lastEnviron || p - environ > lastEnvironLength) {

	    register char **np;

	    lastEnvironLength = (p - environ) + 12; /* 1 for new entry, 1 for NULL, and 10 for expansion. */
	    if (lastEnviron != NULL)
	        lastEnviron = (char **) realloc(lastEnviron, sizeof(char *) * lastEnvironLength);
	    else
	        lastEnviron = (char **) malloc(sizeof(char *) * lastEnvironLength);

	    for (p = lastEnviron, np = environ; *np;)
		*p++ = *np++;

	    environ = lastEnviron;
	}
	p[1] = NULL;
    }

    *p = (char *) malloc (strlen(variable) + strlen(value) + 2);
    sprintf (*p, "%s=%s", variable, value);
}

void environ__Delete(classID, variable)
    struct classheader *classID;
    char	*variable;
{

    extern char **environ;
    register char **p;

    for (p = environ; *p != NULL && !varcmp(variable, *p); p++)
	;
    for (; *p != NULL; p++)
	    *p = p[1];
}

char *environ__Get(classID, variable)
    struct classheader *classID;
    char *variable;
{

    extern char *getenv();

    return(getenv(variable));
}

char *environ__GetProfile(classID, preference)
    struct classheader *classID;
    char *preference;
{

    extern char *getprofile();

    return getprofile(preference);
}

boolean environ__GetProfileSwitch(classID, preference, defaultValue)
    struct classheader *classID;
    char *preference;
    boolean defaultValue;
{

    return getprofileswitch(preference, defaultValue);
}

long environ__GetProfileInt(classID, preference, defaultValue)
    struct classheader *classID;
    char *preference;
    long defaultValue;
{

    return getprofileint(preference, defaultValue);
}

boolean environ__ProfileEntryExists(classID, preference, useDefault)
    struct classheader *classID;
    char *preference;
    boolean useDefault;
{
    return profileentryexists(preference, (int) useDefault);
}

char *environ__GetConfiguration(classID, key)
    struct classheader *classID;
    char *key;
{
    return (char *) GetConfiguration(key);
}

char *environ__AndrewDir(classID, str)
    struct classheader *classID;
    char *str;
{
    return (char *) AndrewDir(str);
}

char *environ__LocalDir(classID, str)
    struct classheader *classID;
    char *str;
{
    return (char *) LocalDir(str);
}

struct configurelist *environ__ReadConfigureFile(classID, filename)
    struct classheader *classID;
    char *filename;
{
    return (struct configurelist *) ReadConfigureFile(filename);
}

char *environ__GetConfig(classID, cList, key, usedefault)
    struct classheader *classID;
    struct configurelist *cList;
    char *key;
    boolean usedefault;
{
    return (char *) GetConfig(cList, key, usedefault);
}

void environ__FreeConfigureList(classID, cList)
    struct classheader *classID;
    struct configurelist *cList;
{
    FreeConfigureList(cList);
}

char *environ__GetHome(classID,user)
struct classheader *classID;
char *user;
{
    char *gethome();

    return gethome(user);
}

extern char *GetProfileFileName();

char *environ__GetProfileFileName(classID)
struct classheader *classID;
{
    return GetProfileFileName();
}
