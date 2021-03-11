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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/cawp.c,v 1.14 1993/05/04 00:53:32 susan Exp $";
#endif

/* ************************************************************ *\
	cawp.c
	The part of cellauth.c that needs the White Pages code.
\* ************************************************************ */


 

#include <andyenv.h>
#include <stdio.h>
#include <andrewos.h>
#include <errno.h>
#include <pwd.h>
#include <util.h>
#include <svcconf.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */


#ifdef AFS_ENV
static int idxID = -1;
static int idxN, idxHD, idxNI;

static wp_ErrorCode GetWP(ca)
struct CellAuth *ca;
{/* Fill in White Pages values and return any errors encountered. */
#ifdef AFS_ENV
    wp_ErrorCode wpErr;
    wp_PrimeKey KVal;
    struct wp_cd *cd;
    char *Val, *newVal;
#endif /* AFS_ENV */

    if (idxID < 0) {
	idxID =	wp_FieldNameToIndex("ID");
	idxN =	wp_FieldNameToIndex("N");
	idxHD =	wp_FieldNameToIndex("HD");
	idxNI =	wp_FieldNameToIndex("NI");
	if (idxID < 0 || idxN < 0 || idxHD < 0 || idxNI < 0) {
	    idxID = -1;
	    return(wperr_InternalError);
	}
    }

    wpErr = wp_InitializeCell(ca->CellName, &cd);
    if (wpErr != wperr_NoError) return(wpErr);
    if (ca->ViceID >= 0) {
	wpErr = cwp_GetNIDOnly(cd, ca->ViceID, &KVal);
	if (wpErr != wperr_NoError) {
	    cwp_Terminate(cd);
	    return(wpErr);
	}
    } else {
	wpErr = cwp_GetUIDOnly(cd, ca->UserName, &KVal);
	if (wpErr != wperr_NoError) {
	    cwp_Terminate(cd);
	    return(wpErr);
	}
    }
    if (ca->UserName == NULL) {
	wpErr = cwp_Read(cd, KVal, idxID, &Val);
	if (wpErr == wperr_NoError) {
	    newVal = NewString(Val);
	    if (newVal == NULL) {
		free(KVal);
		cwp_Terminate(cd);
		return(wperr_OutOfMemory);
	    }
	    ca->UserName = newVal;
	} else if (wpErr != wperr_NoSuchField) {
	    free(KVal);
	    cwp_Terminate(cd);
	    return(wpErr);
	}
    }
    if (ca->ViceID < 0) {
	wpErr = cwp_Read(cd, KVal, idxNI, &Val);
	if (wpErr == wperr_NoError) {
	    ca->ViceID = atoi(Val);
	} else if (wpErr != wperr_NoSuchField) {
	    free(KVal);
	    cwp_Terminate(cd);
	    return(wpErr);
	}
    }
    wpErr = cwp_Read(cd, KVal, idxN, &Val);
    if (wpErr == wperr_NoError) {
	newVal = NewString(Val);
	if (newVal == NULL) {
	    free(KVal);
	    cwp_Terminate(cd);
	    return(wperr_OutOfMemory);
	}
	ca->PersonName = newVal;
    } else if (wpErr != wperr_NoSuchField) {
	free(KVal);
	cwp_Terminate(cd);
	return(wpErr);
    }
    wpErr = cwp_Read(cd, KVal, idxHD, &Val);
    if (wpErr == wperr_NoError) {
	newVal = NewString(Val);
	if (newVal == NULL) {
	    free(KVal);
	    cwp_Terminate(cd);
	    return(wperr_OutOfMemory);
	}
	ca->homeDir = newVal;
    } else if (wpErr != wperr_NoSuchField) {
	free(KVal);
	cwp_Terminate(cd);
	return(wpErr);
    }
    free(KVal);
    cwp_Terminate(cd);
    return(wperr_NoError);
}
#endif /* AFS_ENV */

void FillInCell(cellAuth)
struct CellAuth *cellAuth;
{/* Fill in the accounting (WP/passwd) values for the given cell pointer; an error (or success) code is left in cellAuth->WpError. */
#ifdef AFS_ENV
    wp_ErrorCode wpErr;
#endif /* AFS_ENV */
    struct passwd *PW;

    if (cellAuth->WpError == 0) return;

#ifdef AFS_ENV
    if (cellAuth->CellName == NULL) {
	cellAuth->WpError = wperr_UnImplementedFunction;
	return;
    }
    if (AMS_ViceIsRunning && cellAuth->IsLocal == 0) {
	wpErr = GetWP(cellAuth);
	if (wpErr == wperr_NoError || AMS_ThisDomainAuthFromWS == 0 || ULstrcmp(ThisDomain, cellAuth->CellName) != 0) {
	    cellAuth->WpError = wpErr;
	    return;
	}
	/* Fall through on WP error, if AMS_ThisDomainAuthFromWS and client is asking about ThisDomain. */
    }
#endif /* AFS_ENV */
    if (cellAuth->ViceID >= 0) {
	PW = getpwuid(cellAuth->ViceID);
    } else {
	PW = getpwnam(cellAuth->UserName);
    }
    if (PW == NULL) {cellAuth->WpError = -1; return;}
    if (cellAuth->ViceID < 0) cellAuth->ViceID = PW->pw_uid;
    if (cellAuth->UserName == NULL) cellAuth->UserName = NewString(PW->pw_name);
    cellAuth->PersonName = NewString(PW->pw_gecos);
    cellAuth->homeDir = NewString(PW->pw_dir);
    if (cellAuth->UserName == NULL || cellAuth->PersonName == NULL || cellAuth->homeDir == NULL) {
	if (cellAuth->PersonName != NULL) {
	    free(cellAuth->PersonName);
	    cellAuth->PersonName = NULL;
	}
	if (cellAuth->homeDir != NULL) {
	    free(cellAuth->homeDir);
	    cellAuth->homeDir = NULL;
	}
	cellAuth->WpError = -2;
	return;
    }
    cellAuth->WpError = 0;
    return;
}

#ifdef TESTINGONLYTESTING
main ()
{
    struct CellAuth *ca;
    int RC, ix;

    CheckServiceConfiguration();
    RC = FindCell(WorkstationCell, &ca);
    if (RC == 0) {
	printf("Workstation cell: ``%s'', vid %d, expires %s.\n",
	       ca->CellName, ca->ViceID, NiceTime(ca->ExpireTime));
	FillInCell(ca);
	if (ca->WpError == wperr_NoError) {
	    printf("\tUserName is ``%s''; PersonName is ``%s''; homeDir is ``%s''.\n",
		   (ca->UserName == NULL ? "NULL" : ca->UserName),
		   (ca->PersonName == NULL ? "NULL" : ca->PersonName),
		   (ca->homeDir == NULL ? "NULL" : ca->homeDir));
	} else {
	    printf("\tCan't find WP info: %s\n", wp_ErrorString(ca->WpError));
	}
    } else {
	printf("Can't get workstation cell: %d\n", RC);
    }

    printf("Before referring to workstation %s:\n", WorkstationName);
    ca = NULL; ix = 1;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	printf("(%d) ``%s'', %s%svid %d, expires %s.\n", ix, ca->CellName,
	       (ca->IsPrimary < 0 ? "" : ca->IsPrimary ? "primary, " : "non-primary, "),
	       (ca->IsLocal < 0 ? "" : ca->IsLocal ? "local, " : "non-local, "),
	       ca->ViceID, NiceTime(ca->ExpireTime));
	++ix;
	FillInCell(ca);
	if (ca->WpError == wperr_NoError) {
	    printf("\tUserName is ``%s''; PersonName is ``%s''; homeDir is ``%s''\n",
		   (ca->UserName == NULL ? "NULL" : ca->UserName),
		   (ca->PersonName == NULL ? "NULL" : ca->PersonName),
		   (ca->homeDir == NULL ? "NULL" : ca->homeDir));
	} else {
	    printf("\tCan't find WP info: %s\n", wp_ErrorString(ca->WpError));
	}
    }
    if (RC != 0) printf("Can't find a next cell: %d\n", RC);
    RC = FindCell(WorkstationName, &ca);
    if (RC != 0) printf("Can't find workstation-name %s: %d\n", WorkstationName, RC);
    printf("After referring to workstation %s:\n", WorkstationName);
    ca = NULL; ix = 1;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	printf("(%d) ``%s'', %s%svid %d, expires %s.\n", ix, ca->CellName,
	       (ca->IsPrimary < 0 ? "" : ca->IsPrimary ? "primary, " : "non-primary, "),
	       (ca->IsLocal < 0 ? "" : ca->IsLocal ? "local, " : "non-local, "),
	       ca->ViceID, NiceTime(ca->ExpireTime));
	++ix;
	FillInCell(ca);
	if (ca->WpError == wperr_NoError) {
	    printf("\tUserName is ``%s''; PersonName is ``%s''; homeDir is ``%s''\n",
		   (ca->UserName == NULL ? "NULL" : ca->UserName),
		   (ca->PersonName == NULL ? "NULL" : ca->PersonName),
		   (ca->homeDir == NULL ? "NULL" : ca->homeDir));
	} else {
	    printf("\tCan't find WP info: %s\n", wp_ErrorString(ca->WpError));
	}
    }
    if (RC != 0) printf("Can't find a next cell: %d\n", RC);
}
#endif /* TESTINGONLYTESTING */
