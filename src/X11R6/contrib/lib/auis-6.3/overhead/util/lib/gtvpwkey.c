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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/gtvpwkey.c,v 2.15 1993/01/08 16:30:59 rr2b R6tape $";
#endif

/* ************************************************************ *\
	gtvpwkey.c
	getvpwkey(key) is like getpwent given a WP key value, for Vice IDs.
\* ************************************************************ */
#include <andyenv.h>
#include <andrewos.h> /* syslog.h */
#include <stdio.h>
#include <errno.h>
#include <util.h>
#ifdef WHITEPAGES_ENV
#include <pwd.h>
#include <wp.h>
#endif /* WHITEPAGES_ENV */

extern int errno;

#ifdef WHITEPAGES_ENV
static char *EMPTYFIELD = "";

static int idxID = -1, idxPW, idxNI, idxGI, idxN, idxHD, idxSh;

static enum {WpOpen_NotOpen, WpOpen_Open, WpOpen_CannotOpen}
WpOpen = WpOpen_NotOpen;

int _pw_OpenWP()
{/* Open WP for this use */
    wp_ErrorCode wpErr;

    if (WpOpen == WpOpen_NotOpen) {
	wpErr = wp_Initialize();
	if (wpErr == wperr_NoError)
	    WpOpen = WpOpen_Open;
	else {
	    WpOpen = WpOpen_CannotOpen;
	    errno = ETIMEDOUT;
	}
    }
    return (WpOpen == WpOpen_Open ? 1 : 0);
}

void _pw_CloseWP()
{/* Close WP for this use */
    if (WpOpen == WpOpen_Open) {
	wp_Terminate();
    }
    WpOpen = WpOpen_NotOpen;
}

static char StgBuf[BUFSIZ+1];
static char *StgPtr;
static char *AddStg(loc)
char *loc;
{/* Store a value in StgBuf and return a pointer to it */
    char *OldStg;
    int LocLen;

    OldStg = StgPtr;
    LocLen = strlen(loc) + 1;
    StgPtr += LocLen;
    if (StgPtr >= &StgBuf[sizeof(StgBuf)]) return NULL;
    strncpy(OldStg, loc, LocLen);	/* copy the NUL also */
    return OldStg;
}

struct passwd *_pw_getvpwkey(PKey)
wp_PrimeKey PKey;
{/* Return a struct passwd for PKey, a White Pages prime key, or NULL with errno set */
    static struct passwd RetP;
    wp_ErrorCode Res;
    char *NewPtr;

    if (idxID < 0) {
	idxID =	wp_FieldNameToIndex("ID");
	idxPW =	wp_FieldNameToIndex("PW");
	idxNI =	wp_FieldNameToIndex("NI");
	idxGI =	wp_FieldNameToIndex("GI");
	idxN =	wp_FieldNameToIndex("N");
	idxHD =	wp_FieldNameToIndex("HD");
	idxSh =	wp_FieldNameToIndex("Sh");
	if (idxID < 0 || idxPW < 0 || idxNI < 0 || idxGI < 0
	    || idxN < 0 || idxHD < 0 || idxSh < 0) {
#ifdef LOG_WARNING
	    syslog(LOG_WARNING, "White Pages does not contain passwd info");
#endif /* LOG_WARNING */
	    idxID = -1;
	    errno = ENXIO;
	    return NULL;
	}
    }
    StgPtr = StgBuf;

    if (WpOpen != WpOpen_Open) {errno = ENOTTY; return NULL;}

    Res = wp_Read(PKey, idxID, &NewPtr);	/* pw_name */
    if (Res == wperr_NoError) RetP.pw_name = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_name = EMPTYFIELD;
    else {errno = ETIMEDOUT; return NULL;}
    Res = wp_Read(PKey, idxPW, &NewPtr);	/* pw_passwd */
    if (Res == wperr_NoError) RetP.pw_passwd = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_passwd = EMPTYFIELD;
    else {errno = ETIMEDOUT; return NULL;}
    Res = wp_Read(PKey, idxNI, &NewPtr);	/* pw_uid */
    if (Res == wperr_NoError) RetP.pw_uid = atoi(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_uid = -1;
    else {errno = ETIMEDOUT; return NULL;}
    Res = wp_Read(PKey, idxGI, &NewPtr);	/* pw_gid */
    if (Res == wperr_NoError) RetP.pw_gid = atoi(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_gid = -1;
    else {errno = ETIMEDOUT; return NULL;}
#ifndef _IBMR2
    RetP.pw_comment = EMPTYFIELD;		/* pw_comment */
#endif /* _IBMR2 */
    Res = wp_Read(PKey, idxN, &NewPtr);	/* pw_gecos */
    if (Res == wperr_NoError) RetP.pw_gecos = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_gecos = EMPTYFIELD;
    else {errno = ETIMEDOUT; return NULL;}
    Res = wp_Read(PKey, idxHD, &NewPtr);	/* pw_dir */
    if (Res == wperr_NoError) RetP.pw_dir = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_dir = EMPTYFIELD;
    else {errno = ETIMEDOUT; return NULL;}
    Res = wp_Read(PKey, idxSh, &NewPtr);	/* pw_shell */
    if (Res == wperr_NoError) RetP.pw_shell = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_shell = EMPTYFIELD;
    else {errno = ETIMEDOUT; return NULL;}

    return &RetP;
}
#endif /* WHITEPAGES_ENV */

