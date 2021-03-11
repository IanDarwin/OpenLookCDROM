/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pcserver/RCS/pcsutils.c,v 2.14 1993/09/22 19:51:25 gk5g Exp $";
#endif

/*
 *	PC Server - Utility Component
 *	Access to the VICE File System for IBM PC/XT/ATs
 *
 *	(C)Copyright IBM Corporation, 1984, 1985, 1986, 1987
 *	Program Property of IBM
 *
 *	Version 2.7 by Larry Raper
 *	Developed for the Information Technology Center at
 *	Carnegie-Mellon University
 *
 *	09/87 2.5 Coerced canonical form on ActualCaseCurrentDir
 *	09/87 2.6 Split names into name.ext at first dot (rather than last)
 *	10/87 2.7 Removed unreferenced variables
 *
 */
#include <andrewos.h> /* sys/time.h strings.h sys/file.h */
#include <stdio.h>
#include <sys/errno.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pcserver.h>

#define TRUE	1
#define FALSE	0
extern int PCS_debuglevel;

/* Forward Declarations */
void FoldLower();

int SplitName ();
int SplitNameAllowDots ();

/*
 * Split the pathname provided into its directory and name components.
 * The smallest extension possible is chosen.  Folds the name and
 * extension to lower case if they are completely in upper case.
 * Only accepts full, possibly generic, pathnames.
 *
 * Returns -1 on error, 0 on success.
 *
 */

int SplitPath(PathName, ToDir, ToName, allowdots, allowfold)
char *PathName;
char *ToDir;
PCNAME *ToName;
int allowdots, allowfold;
    {
    char *slash;
    int dirlen;
    int (*sn)();

    if (allowdots)
	sn = SplitNameAllowDots;
    else
	sn = SplitName;
    DBGMSG (12,("SplitPath - pathname: %s",PathName));
    if (strcmp (PathName, "/") == 0) {
	strcpy(ToDir, PathName);
	ToName->name[0] = '\0';
	ToName->ext[0] = '\0';
	ToName->hidden = 0;
	DBGMSG(12,("SplitPath - returning path: \"/\", name: \"\", ext: \"\""));
	return 0;
	}
    if (*PathName != '/' || (slash = rindex(PathName, '/')) == 0) {
	DBGMSG (12,("SplitPath - no slashes in pathname"));
	return -1;
	}
    dirlen = slash-PathName+1;
    if (dirlen > MAXPATHLEN) {
	DBGMSG (12,("SplitPath - dir len > %d",MAXPATHLEN));
	return -1;
	}
    strncpy(ToDir, PathName, dirlen);
    ToDir[dirlen] = '\0';
    if ((*sn)(slash+1, ToName) == -1) {
	DBGMSG (12,("SplitPath - SplitName returned -1, exiting with -1"));
	return -1;
	}
    if (allowfold && UpperCase(ToName->ext) && UpperCase(ToName->name)) {
	FoldLower(ToName->ext), FoldLower(ToName->name);
	if (UpperCase(PathName))
	    FoldLower (ToDir);
	}

    DBGMSG (12,("SplitPath - dir: \"%s\"",ToDir));
    DBGMSG (12,("SplitPath - name: \"%s\".\"%s\"",ToName->name,ToName->ext));
    DBGMSG (12,("SplitPath - hidden: %s",
	(ToName->hidden != 0 ? "TRUE" : "FALSE")));
    return 0;
    }

/*
 * Split the name provided into components
 *
 */

int SplitName(name, ToName)
char *name;
PCNAME *ToName;
    {
    char *dot;
    if (strlen(name) > MAXNAMLEN)
	return -1;
    if (*name == '.') {
	ToName->hidden = 1;
	if (*++name == '.')
	    /* Don't allow more than one dot at the beginning of the name, */
	    /* this filters out nasty files like '..'. */
	    return -1;
	}
    else
	ToName->hidden = 0;
    if (*name == '\0')
	/* Null names, names with extensions only, and '.' disallowed. */
	return -1;
    strcpy(ToName->name, name);
    dot = index(ToName->name, '.');     /* split name at first dot */
    if (dot == 0) {
	if (ToName->name[strlen(ToName->name)-1] == '*') {
	    ToName->ext[0] = '*';
	    ToName->ext[1] = '\0';
	    }
	else
	    ToName->ext[0] = '\0';
	}
    else {
	*dot = '\0';
	strcpy(ToName->ext,dot+1);
	}
    return 0;
    }

/*
 * Split the name provided into components and allow "." and ".." names
 *
 */

int SplitNameAllowDots(name, ToName)
char *name;
PCNAME *ToName;
    {
    if ((strcmp (name, "..") == 0) || (strcmp (name, ".") == 0)) {
	ToName->hidden = 0;
	strcpy (ToName->name, name);
	ToName->ext[0] = 0;
	return 0;
	}
    return (SplitName (name, ToName));
    }

/*
 * Returns true if the name passed has no lower case characters,
 * and is not generic (*.CKP should be folded!!!).
 *
 */

int UpperCase(name)
register char *name;
    {
    register char c;
    while (c = *name++)
	if (islower(c))
	    return 0;
    return 1;
    }

/*
 * Fold the string passed to lower case
 *
 */

void FoldLower(name)
register char *name;
    {
    register char c;
    while (c = *name) {
	if (isupper(c))
	    *name = tolower(c);
	name++;
	}
    }

/*
 * Return 1 if the specified generic name is the same as the specified
 * non-generic name.
 *
 */

int GenericEquality(generic, name)
PCNAME *generic, *name;
    {
    register char *np = name->name;
    if (generic->hidden != name->hidden)
	return 0;
    return ComponentEqual(generic->name, np) &&
	ComponentEqual(generic->ext, name->ext);
    }

/*
 * Return 1 if the specified generic name is the same as the specified
 * non-generic name, using case-insensitive compares.
 *
 */

int NoCaseGenericEquality(generic, name)
PCNAME *generic, *name;
    {
    register char *np = name->name;
    if (generic->hidden != name->hidden)
	return 0;
    return NC_ComponentEqual(generic->name, np) &&
	NC_ComponentEqual(generic->ext, name->ext);
    }

/*
 * Return 1 if the two component names passed are generically equal,
 * 0 otherwise. The first name passed is generic, the second is not.
 * N.B. does not check for lower case equality.
 *
 */

int ComponentEqual(gc, c)
register char *gc, *c;
    {
    while (*gc) {
	if (*gc == '*')
	    return 1;
	if (*gc != *c && *gc != '?')
	    return 0;
	gc++;
	if (*c) c++;
	}
    return *c == 0;
    }

/*
 * Return 1 if the two component names passed are generically equal,
 * 0 otherwise. The first name passed is generic, the second is not.
 * Uses case-insensitive compare logic.
 *
 */

int NC_ComponentEqual(gc, c)
register char *gc, *c;
    {
    while (*gc) {
	if (*gc == '*')
	    return 1;
	if (*gc != '?') {
	    if ( (isupper(*gc) ? tolower(*gc) : *gc)
	      != (isupper(*c) ? tolower(*c) : *c) )
		return 0;
	    }
	gc++;
	if (*c) c++;
	}
    return *c == 0;
    }

/*
 * Rename a filename by applying a generic name to it.
 * Rewrites the name in place.
 *
 */

MakeNewName(name, generic)
PCNAME *name, *generic;
    {
    MakeNewComponent(name->name, generic->name);
    MakeNewComponent(name->ext, generic->ext);
    }

/*
 * Rename a filename component by applying a generic component to it.
 * Rewrites the component in place.
 *
 */

MakeNewComponent(c, generic)
char *c, *generic;
    {
    register char gc;
    while (*c) {
	gc = *generic++;
	if (gc == '*')
	    return;
	if (gc != '?')
	    *c = gc;
	c++;
	}
    while (gc = *generic++)
	if (gc != '?' && gc != '*')
	    *c++ = gc;
    *c = '\0';
    return;
    }


/*
 * Combine the directory and name structure passed into a single
 * pathname, new.  If hidden is true, convert the name into a Unix
 * hidden name (i.e. put a dot before it).
 *
 */

MakePath(new,dir,name)
char *new, *dir; /* Again, typedef PATH should be here... */
PCNAME *name;
{
    int dirlen = strlen(dir);
    int namelen = strlen(name->name);
    int extlen = strlen(name->ext);
#ifdef DEBUG
    char *orignew = new;
#endif /* DEBUG */

    DBGMSG (12,("MakePath - dir: \"%s\"",dir));
    DBGMSG (12,("MakePath - name: \"%s\".\"%s\"",name->name,name->ext));
    DBGMSG (12,("MakePath - hidden: %s",
		 (name->hidden != 0 ? "TRUE" : "FALSE")));
    if (dirlen+name->hidden+namelen+1+extlen > MAXPATHLEN) {
	new[0] = '\0';
	DBGMSG (12,("MakePath - error: path limit exceeded (%d)",MAXPATHLEN));
	return;
    }
    strcpy (new, dir);
    new += dirlen;
    if (name->hidden)
	*new++ = '.';
    strcpy (new, name->name);
    new += namelen;
    if (extlen) {
	*new++ = '.';
	strcpy (new, name->ext);
    }
    DBGMSG (12,("Makepath - path: %s",orignew));
    return;
}

/*
  * Convert Unix date to pc format date and time words
  *
  */

GetPCDateTime (unixtime, pcdate, pctime)
int unixtime, *pcdate, *pctime;
{
    register struct tm *tm = (struct tm *) localtime(&unixtime);
    if (tm->tm_year < 80) {
	tm->tm_year = 80;
	tm->tm_mon = 0;
	tm->tm_mday = 1;
    }
    *pcdate = ((tm->tm_year+1900-1980)<<9) | ((tm->tm_mon+1)<<5) | (tm->tm_mday);
    *pctime = (tm->tm_hour<<11) | (tm->tm_min<<5) | (tm->tm_sec>>1);
}

struct	scb {			    /* Session Control Block	    */
    struct scb	 *nextscb;	    /* Pointer to next SCB	    */
    int 	 scid;		    /* CID for session		    */
    char	 reserved;	    /* Add more stuff later	    */
};

PRIVATE struct scb *scbhead = NULL; /* Anchor for SCB list	    */

/*
  * Create a new SCB
  *
  */

PRIVATE struct scb *NewSCB (prevscb, cid)
struct scb *prevscb;
int cid;
{
    struct scb *newscb;

    DBGMSG (9,("NewSCB - cid: %d",cid));

    if ((newscb = (struct scb *) malloc (sizeof (struct scb))) == NULL) {
	DBGMSG (4,("NewSCB allocation failed"));
	return (NULL);
    }

    DBGMSG (9,("NewSCB allocated at %08x",newscb));

    if (prevscb != NULL) {
	DBGMSG (9,("NewSCB - prior SCB in list: %08x",prevscb));
	prevscb->nextscb = newscb;
    }
    else {
	DBGMSG (9,("NewSCB - first SCB"));
	scbhead = newscb;
    }

    newscb->scid = cid;
    newscb->reserved = ' ';
    newscb->nextscb = NULL;

    DBGMSG (9,("NewSCB - SCB initialized"));

    return (newscb);
}

/*
  * Find an SCB from the SCB list using its cid.
  * [One will be created and added to the list if necessary]
  *
  */

PRIVATE struct scb *FindSCBbyCID (cid)
int cid;
{
    struct scb *p,*nextp,*prevp;

    DBGMSG(9,("FindSCBbyCID - cid: %d", cid));

    prevp = NULL;
    for (p=scbhead; p != NULL; prevp=p,p=nextp) {
	nextp = p->nextscb;
	if (cid == p->scid) {
	    DBGMSG (9,("FindSCBbyCID - SCB at %08x",p));
	    return (p);
	}
    }
    return (NewSCB (prevp, cid));
}

/*
  * Find an SCB from the existing SCB list using its CID.
  *
  */

PRIVATE struct scb *FindExistingSCBbyCID (cid)
int cid;
{
    struct scb *p,*nextp;

    DBGMSG(9,("FindExistingSCBbyCID - cid: %d",cid));

    for (p=scbhead; p != NULL; p=nextp) {
	nextp = p->nextscb;
	if (cid == p->scid) {
	    DBGMSG (9,("FindExistingSCBbyCID - SCB at %08x",p));
	    return (p);
	}
    }
    DBGMSG (8,("FindExistingSCBbyCID - no matching SCB!"));
    return (NULL);
}

/*
  * Free an SCB and its associated storage
  *
  */

PRIVATE FreeSCB (cp)
struct scb *cp;
{
    DBGMSG (5,("FreeSCB - SCB at %08x",cp));
    free (cp);
}

/*
  * Remove an SCB from the list and free it
  *
  */

PRIVATE ReleaseSCB (cp)
struct scb *cp;
{
    struct scb *prevp, *nextp, *p;
    DBGMSG (5,("ReleaseSCB - SCB: %08x",cp));
    if (cp != NULL) {
	prevp = NULL;
	for (p=scbhead; p != NULL; prevp=p,p=nextp) {
	    nextp = p->nextscb;
	    if (cp == p) {
		FreeSCB (cp);
		if (prevp == NULL)
		    scbhead = nextp;
		else
		    prevp->nextscb = nextp;
		return;
	    }
	}
	DBGMSG (4,("ReleaseSCB - error: SCB not on list"));
    }
    else
	DBGMSG (4,("ReleaseSCB - error: SCB ptr is null"));
}

EndSession (cid)
int cid;
{
    ReleaseSCB (FindExistingSCBbyCID (cid));
}

CheckSessions (cid)
int cid;
{
    FindSCBbyCID (cid);
}

int SessionCount ()
{
    struct scb *p;
    int count;

    count = 0;
    for (p=scbhead; p != NULL; p=p->nextscb)
	count++;
    DBGMSG (8,("SessionCount: %d", count));
    return (count);
}

PRIVATE PATH CurrentDir;
PRIVATE PATH ActualCaseCurrentDir;

/*
  * Change to the indicated directory.  Don't bother issuing the system call
	     * if we're already there.  Because this routine keeps track of the current
  * directory with internal state, all directory changes must use this
  * function.
  *
  */

ChangeDirectory (path)
char *path;
{
    DIR *dirp;
    DIRENT_TYPE *entry;

    DBGMSG (12,("CD - requested dir: \"%s\"",path));
    DBGMSG (12,("CD - old dir: %s",CurrentDir));
    DBGMSG (12,("CD - actual case: %s",ActualCaseCurrentDir));
    if ((strcmp(path, ActualCaseCurrentDir) == 0) ||
	 (NoCaseStrcmp (path, CurrentDir) == 0)) {
	DBGMSG (12,("CD - no change required"));
	return 0;
    }

    /* 1st step is just to try vanilla chdir */

    if (chdir(path) == -1) {
	int i, len, nlen, foundslash, rc;
	char temppath[MAXPATHLEN+1];

	if ((len = strlen (path)) > MAXPATHLEN) {
	    DBGMSG (3,("CD - path too long - %d bytes", len));
	    return -1;
	}

	strcpy (temppath, path);
	foundslash = FALSE;
	for (i=len-1; i>=0; i--) {
	    if (temppath[i] == '/') {
		temppath[i] = 0;
		foundslash = TRUE;
		break;
	    }
	}
	if (!foundslash) {
	    DBGMSG (3,("CD - error: no final component name to remove"));
	    return (-1);
	}

	if ((rc = ChangeDirectory (*temppath == 0 ? "/" : temppath)) != 0) {
	    DBGMSG (3,("CD - error %d from recursive ChangeDirectory", rc));
	    return (-1);
	}

	if (chdir (temppath+i+1) == 0) {
	    temppath[i] = '/';
	    strcpy (CurrentDir, temppath);
	    FinalSlash ();
	    strcat (ActualCaseCurrentDir, temppath+i+1);
	    FinalSlash ();
	    DBGMSG (12,("CD - new dir is %s",CurrentDir));
	    DBGMSG (12,("CD - actual case is %s",ActualCaseCurrentDir));
	    return (0);
	}

	if ((nlen = strlen (temppath+i+1)) == 0) {
	    strcpy (CurrentDir, temppath);
	    DBGMSG (12,("CD - new dir is %s",CurrentDir));
	    DBGMSG (12,("CD - actual case is %s",ActualCaseCurrentDir));
	    return (0);
	}

	DBGMSG (12,("CD - final name component is %s", temppath+i+1));

	if ((dirp = opendir (".")) == NULL) {
	    DBGMSG (3,("CD - opendir for \".\" failed"));
	    return -1;
	}

	for (entry = readdir (dirp); entry != NULL; entry = readdir (dirp))
	    if (DIRENT_NAMELEN(entry) != 0) {
		if ((DIRENT_NAMELEN(entry) == nlen) &&
		    (dirmatch (temppath+i+1, entry->d_name) == 0)) {
		    strcpy (temppath+i+1, entry->d_name);
		    temppath[i] = '/';
		    closedir (dirp);
		    if (chdir (temppath+i+1) != 0) {
			DBGMSG (3,("CD - final chdir error: %s",temppath+i+1));
			return (-1);
		    }
		    strcpy (CurrentDir, temppath);
		    FinalSlash ();
		    strcat (ActualCaseCurrentDir, temppath+i+1);
		    FinalSlash ();
		    DBGMSG (12,("CD - new dir is %s",CurrentDir));
		    DBGMSG (12,("CD - actual case is %s",ActualCaseCurrentDir));
		    return (0);
		}
	    }
	closedir (dirp);
	DBGMSG (3,("CD - chdir failed to find final component match"));
	return (-1);
    }

    strcpy(CurrentDir, path);
    strcpy(ActualCaseCurrentDir, path);
    FinalSlash ();
    DBGMSG (12,("CD - new dir is %s",CurrentDir));
    DBGMSG (12,("CD - actual case is %s",ActualCaseCurrentDir));
    return 0;
}

PRIVATE FinalSlash ()
{
    register int i;
    i = strlen (ActualCaseCurrentDir);
    if (i == 0) {
	strcpy (ActualCaseCurrentDir, ".");
	return;
    }
    if ((i <= MAXPATHLEN) && (ActualCaseCurrentDir [i - 1] != '/'))
	strcat (ActualCaseCurrentDir, "/");
}

char *GetCurrentDir ()
{
    DBGMSG (12,("GetCurrentDir: \"%s\"",ActualCaseCurrentDir));
    return ActualCaseCurrentDir;
}

int MixedCase (s)
char *s;
{
    short upperfound, lowerfound;
    register int i;

    upperfound = FALSE;
    lowerfound = FALSE;

    for (i=0; s[i]; i++) {
	if (isupper (s[i]))
	    upperfound = TRUE;
	else if (islower (s[i]))
	    lowerfound = TRUE;
	if (upperfound && lowerfound)
	    return (TRUE);
    }
    return (FALSE);
}

PRIVATE int dirmatch (s1, s2)
char *s1, *s2;
{
    if (MixedCase (s1))
	return (strcmp (s1, s2));

    return (NoCaseStrcmp (s1, s2));
}

int NoCaseStrcmp (s1, s2)
char *s1, *s2;
{
    char c1, c2;

    for (;;) {
	c1 = *s1;
	c2 = *s2;
	s1++; s2++;
	c1 = isupper (c1) ? tolower (c1) : c1;
	c2 = isupper (c2) ? tolower (c2) : c2;
	if (c1 > c2)
	    return (1);
	if (c2 > c1)
	    return (-1);
	if (c1 == 0)
	    return (0);
    }
}

