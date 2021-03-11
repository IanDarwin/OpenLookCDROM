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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/getcpw.c,v 2.19 1993/01/08 16:30:59 rr2b R6tape $";
#endif

/* ************************************************************ *\
	getcpw.c
	Routines to do Vice-cell-based getpwuid and getpwnam.
\* ************************************************************ */
#include <andyenv.h>
#include <andrewos.h> /* syslog.h */
#include <stdio.h>
#include <errno.h>
#include <util.h>
#include <pwd.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <svcconf.h>


extern int errno;

#ifdef WHITEPAGES_ENV
int cpw_error;
#endif /* WHITEPAGES_ENV */

#ifdef WHITEPAGES_ENV
static char *EMPTYFIELD = "";

static int idxID = -1, idxPW, idxNI, idxGI, idxN, idxHD, idxSh;

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

static struct passwd *getcpwkey(PKey, cd)
wp_PrimeKey PKey; struct wp_cd *cd;
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
	    cpw_error = wperr_UnImplementedFunction;
	    errno = ENXIO;
	    return NULL;
	}
    }
    StgPtr = StgBuf;

    Res = cwp_Read(cd, PKey, idxID, &NewPtr);	/* pw_name */
    if (Res == wperr_NoError) RetP.pw_name = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_name = EMPTYFIELD;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}
    Res = cwp_Read(cd, PKey, idxPW, &NewPtr);	/* pw_passwd */
    if (Res == wperr_NoError) RetP.pw_passwd = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_passwd = EMPTYFIELD;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}
    Res = cwp_Read(cd, PKey, idxNI, &NewPtr);	/* pw_uid */
    if (Res == wperr_NoError) RetP.pw_uid = atoi(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_uid = -1;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}
    Res = cwp_Read(cd, PKey, idxGI, &NewPtr);	/* pw_gid */
    if (Res == wperr_NoError) RetP.pw_gid = atoi(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_gid = -1;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}
#ifndef _IBMR2
    RetP.pw_comment = EMPTYFIELD;		/* pw_comment */
#endif /* _IBMR2 */
    Res = cwp_Read(cd, PKey, idxN, &NewPtr);	/* pw_gecos */
    if (Res == wperr_NoError) RetP.pw_gecos = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_gecos = EMPTYFIELD;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}
    Res = cwp_Read(cd, PKey, idxHD, &NewPtr);	/* pw_dir */
    if (Res == wperr_NoError) RetP.pw_dir = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_dir = EMPTYFIELD;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}
    Res = cwp_Read(cd, PKey, idxSh, &NewPtr);	/* pw_shell */
    if (Res == wperr_NoError) RetP.pw_shell = AddStg(NewPtr);
    else if (Res == wperr_NoSuchField) RetP.pw_shell = EMPTYFIELD;
    else {cpw_error = Res; errno = ETIMEDOUT; return NULL;}

    return &RetP;
}

static wp_ErrorCode open_wp(vcell, cdp)
char *vcell; struct wp_cd **cdp;
{/* Do the hassle of opening a WP cell. */
    wp_ErrorCode Res;
    struct wp_cd *cd;

    Res = wp_InitializeCell(vcell, &cd);
    if (Res != wperr_NoError) {
	errno = ENXIO;
	return Res;
    }
    *cdp = cd;
    return wperr_NoError;
}

struct passwd *getcpwuid(vuid, vcell)
int vuid; char *vcell;
{/* Return a struct passwd for vuid, a Vice UID, in Vice cell vcell. */
    wp_ErrorCode Res;
    wp_PrimeKey KVal;
    struct passwd *RV;
    struct wp_cd *cd;

    CheckServiceConfiguration();
    if (AMS_UseWP) {
	if (vcell == NULL || vcell[0] == '\0') vcell = WorkstationCell;
	Res = open_wp(vcell, &cd);
	if (Res != wperr_NoError) {
	    cpw_error = Res;
	    if (ULstrcmp(vcell, WorkstationName) == 0) return getpwuid(vuid);
	    return NULL;
	}
	Res = cwp_GetNIDOnly(cd, vuid, &KVal);
	if (Res != wperr_NoError) {cpw_error = Res; cwp_Terminate(cd); errno = ETIMEDOUT; return NULL;}
	RV = getcpwkey(KVal, cd);
	free(KVal);
	cwp_Terminate(cd);
	return RV;
    } else {
	return getpwuid(vuid);
    }
}

struct passwd *getcpwnam(vnam, vcell)
char *vnam, *vcell;
{/* Return a struct passwd for vuid, a Vice pw_nam, in Vice cell vcell. */
    wp_ErrorCode Res;
    wp_PrimeKey KVal;
    struct passwd *RV;
    struct wp_cd *cd;

    CheckServiceConfiguration();
    if (AMS_UseWP) {
	if (vcell == NULL || vcell[0] == '\0') vcell = WorkstationCell;
	Res = open_wp(vcell, &cd);
	if (Res != wperr_NoError) {
	    cpw_error = Res;
	    if (ULstrcmp(vcell, WorkstationName) == 0) return getpwnam(vnam);
	    return NULL;
	}
	Res = cwp_GetUIDOnly(cd, vnam, &KVal);
	if (Res != wperr_NoError) {cpw_error = Res; cwp_Terminate(cd); errno = ETIMEDOUT; return NULL;}
	RV = getcpwkey(KVal, cd);
	free(KVal);
	cwp_Terminate(cd);
	return RV;
    } else {
	return getpwnam(vnam);
    }
}
#endif /* WHITEPAGES_ENV */

#ifdef TESTINGONLYTESTING
main (argc, argv)
int argc;
char **argv;
{
    struct passwd *P;

    if (argc != 3) {
	fprintf(stderr, "usage: %s [nam|uid] cellname\n", argv[0]);
	exit(1);
    }
    P = NULL;
    if ('0' <= *argv[1] && *argv[1] <= '9') P = getcpwuid(atoi(argv[1]), argv[2]);
    if (P != NULL) {
	printf("getcpwuid(%d, %s) returns", atoi(argv[1]), argv[2]);
    } else {
	P = getcpwnam(argv[1], argv[2]);
	if (P != NULL) printf("getcpwnam(%s, %s) returns", argv[1], argv[2]);
    }
    if (P != NULL) {
	printf(" %s:%s:%d:%d:%s:%s:%s\n",
	       P->pw_name, P->pw_passwd, P->pw_uid, P->pw_gid,
	       P->pw_gecos, P->pw_dir, P->pw_shell);
    } else {
	printf("Can't do it: %s\n", wp_ErrorString(cpw_error));
    }
}
#endif /* TESTINGONLYTESTING */
