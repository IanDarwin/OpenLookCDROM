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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/cellauth.c,v 2.26 1993/05/04 00:53:32 susan Exp $";
#endif

/* ************************************************************ *\
	cellauth.c
	Keep track of the current user's authentication status in different cells.
\* ************************************************************ */


 

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <util.h>
#include <svcconf.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <rx/xdr.h>
#include <afs/auth.h>
#include <netinet/in.h>
#include <afs/cellconfig.h>
#endif /* AFS_ENV */

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* IBMR2 */

#define INITIALAUTHS	5
struct CellAuth *myAuth = NULL;
static int PrimIx = -1;
static int localIx = -1;
static int maxAuth = -1;
static int numAuth = -1;

void EraseCellMemory()
{/* Erase all memory of cell authentications. */
#ifdef AFS_ENV
	int Ix;

	if (myAuth != NULL) {
	    for (Ix = 0; Ix < numAuth; ++Ix) {
		if (myAuth[Ix].CellName != NULL) free(myAuth[Ix].CellName);
		if (myAuth[Ix].UserName != NULL) free(myAuth[Ix].UserName);
		if (myAuth[Ix].PersonName != NULL) free(myAuth[Ix].PersonName);
		if (myAuth[Ix].homeDir != NULL) free(myAuth[Ix].homeDir);
	    }
	    free(myAuth);
	    myAuth = NULL;
	}
	PrimIx = localIx = maxAuth = numAuth = -1;
#endif /* AFS_ENV */
}

static void ClearSome(lowBd, upBd)
int lowBd, upBd;
{/* Clears myAuth[ix] for ix in [lowBd, upBd). */
    int Ix;
    for (Ix = lowBd; Ix < upBd; ++Ix) {
	myAuth[Ix].CellName = NULL;
	myAuth[Ix].ViceID = -1;
	myAuth[Ix].UserName = NULL;
	myAuth[Ix].PersonName = NULL;
	myAuth[Ix].homeDir = NULL;
	myAuth[Ix].WpError = -1;
	myAuth[Ix].IsPrimary = -1;
	myAuth[Ix].UsesAMSDelivery = 0;
	myAuth[Ix].ExpireTime = 0;
	myAuth[Ix].IsLocal = -1;
    }
}

static int GrowBasics()
{/* Grow the myAuth array.  Return 0 if OK, -1 if malloc failed us. */
    int newMax;

    newMax = (numAuth * 2) + 2;
    myAuth = (struct CellAuth *) realloc(myAuth, newMax * sizeof(struct CellAuth));
    if (myAuth == NULL) {
	EraseCellMemory();
	return -1;		/* out of memory */
    }
    maxAuth = newMax;
    ClearSome(numAuth, maxAuth);	/* Init the new stuff */
    return 0;
}

#ifdef AFS_ENV
static int AnyNumber(someName, someKVNo, theNum)
char *someName; int someKVNo, *theNum;
{
    char *cp;

    if (/* someKVNo == 999 && */ strncmp(someName, "ViceID=", 7) == 0) {
	for (cp = someName+7; *cp != '\0'; ++cp) {
	    if (!isdigit(*cp)) break;
	}
	if (*cp == '\0') {
	    *theNum = atoi(someName+7);
	    return 1;
	}
    }
    if (/* someKVNo == 999 && */ strncmp(someName, "AFS ID ", 7) == 0) {
	for (cp = someName+7; *cp != '\0'; ++cp) {
	    if (!isdigit(*cp)) break;
	}
	if (*cp == '\0') {
	    *theNum = atoi(someName+7);
	    return 1;
	}
    }
    if (/* someKVNo == 999 && */ strncmp(someName, "Unix UID ", 9) == 0) {
	for (cp = someName+9; *cp != '\0'; ++cp) {
	    if (!isdigit(*cp)) break;
	}
	if (*cp == '\0') {
	    *theNum = atoi(someName+9);
	    return 1;
	}
    }
    if (/* someKVNo == 999 && */ strncmp(someName, "(for ", 5) == 0) {
	for (cp = someName+5; *cp != '\0'; ++cp) {
	    if (!isdigit(*cp)) break;
	}
	if (*cp == ')' && cp[1] == '\0') {
	    *theNum = atoi(someName+5);
	    return 1;
	}
    }
    return 0;
}

static int GetCellBasics()
{/* Build the basic dynamic array of cell information.  Return 0 if all is OK, else return a non-zero error code (positive for permanent failures, negative for temporary ones). */
    int cellIx, RC;
    struct ktc_principal serviceName, clientName;	/* service name for ticket */
    struct ktc_token token;			/* the token we're printing */
    int theNumber;

    if (myAuth != NULL) return 0;
    maxAuth = INITIALAUTHS;
    numAuth = 0;
    PrimIx = localIx = -1;
    myAuth = (struct CellAuth *) malloc(maxAuth * sizeof(struct CellAuth));
    if (myAuth == NULL) return -1;	/* out of memory */
    ClearSome(0, maxAuth);	/* Init the newly-allocated stuff */
    for (cellIx = 0;;) {
	RC = ktc_ListTokens(cellIx, &cellIx, &serviceName);
	if (RC)  return 0;
	/* get the ticket info itself */
	RC = ktc_GetToken(&serviceName, &token, sizeof(token), &clientName);
	if (RC) {
	    fprintf(stderr, "cellauth: failed to get token info for service %s.%s.%s (code %d)\n",
		   serviceName.name, serviceName.instance, serviceName.cell, RC);
	    return (-2);
	}
	if (clientName.instance[0] != '\0' || serviceName.instance[0] != '\0' || strcmp(serviceName.name, "afs") != 0 || strcmp(serviceName.cell, clientName.cell) != 0) continue;
	/* Got another token; add to the list. */
	if (numAuth >= maxAuth) {	/* make room */
	    if (GrowBasics() != 0) return -1;
	}
	myAuth[numAuth].CellName = NewString(serviceName.cell);
	if (myAuth[numAuth].CellName == NULL) {
	    EraseCellMemory();
	    return -1;		/* out of memory */
	}
	if (AnyNumber(clientName.name, token.kvno, &theNumber)) {
	    myAuth[numAuth].ViceID = theNumber;
	} else {
	    myAuth[numAuth].UserName = NewString(clientName.name);
	    if (myAuth[numAuth].UserName == NULL) {
		free(myAuth[numAuth].CellName);
		EraseCellMemory();
		return -1;		/* out of memory */
	    }
	}
	myAuth[numAuth].IsPrimary = 0;
	myAuth[numAuth].ExpireTime = token.endTime;
	myAuth[numAuth].IsLocal = 0;
	++numAuth;
    }
}
#endif /* AFS_ENV */

int ca_UpdateCellAuths()
{/* Update the CellAuth information.  In particular, update the expiration times for the authentications.  Return 0 if all is OK or non-zero on failures: positive numbers for permanent failures, negative ones for transient ones. */
#ifdef AFS_ENV
    int RC, cellIx, Ix, AnyExp, theNumber;
    struct ktc_principal serviceName, clientName;	/* service name for ticket */
    struct ktc_token token;			/* the token we're printing */
    struct timeval TV;

    if (gettimeofday(&TV, NULL) != 0) return -2;
    AnyExp = 0;
    for (Ix = 0; Ix < numAuth; ++Ix) {
	if (myAuth[Ix].IsLocal == 0 && myAuth[Ix].ExpireTime < (TV.tv_sec + 1*60*60)) {
	    AnyExp = 1;
	    break;
	}
    }
    if (AnyExp == 0) return 0;	/* No need yet to check authentications. */
    for (Ix = 0; Ix < numAuth; ++Ix) if (myAuth[Ix].IsLocal == 0) myAuth[Ix].IsPrimary = -1;
    for (cellIx = 0;;) {
	RC = ktc_ListTokens(cellIx, &cellIx, &serviceName);
	if (RC) break;
	/* get the ticket info itself */
	RC = ktc_GetToken(&serviceName, &token, sizeof(token), &clientName);
	if (RC) {
	    fprintf(stderr,"cellauth: failed to get token info for service %s.%s.%s (code %d)\n",
		   serviceName.name, serviceName.instance, serviceName.cell, RC);
	    return (-2);
	}
	if (clientName.instance[0] != '\0' || serviceName.instance[0] != '\0' || strcmp(serviceName.name, "afs") != 0 || strcmp(serviceName.cell, clientName.cell) != 0) continue;
	AnyExp = -1;
	for (Ix = 0; Ix < numAuth; ++Ix) {
	    if (myAuth[Ix].IsLocal == 0 && ULstrcmp(myAuth[Ix].CellName, serviceName.cell) == 0) {
		AnyExp = Ix;
		break;
	    }
	}
	if (AnyExp < 0) {
	    /* Got another token; add to the list. */
	    if (numAuth >= maxAuth) {	/* make room */
		if (GrowBasics() != 0) return -1;
	    }
	    myAuth[numAuth].CellName = NewString(serviceName.cell);
	    if (myAuth[numAuth].CellName == NULL) {
		EraseCellMemory();
		return -1;		/* out of memory */
	    }
	    if (AnyNumber(clientName.name, token.kvno, &theNumber)) {
		myAuth[numAuth].ViceID = theNumber;
	    } else {
		myAuth[numAuth].UserName = NewString(clientName.name);
		if (myAuth[numAuth].UserName == NULL) {
		    free(myAuth[numAuth].CellName);
		    EraseCellMemory();
		    return -1;		/* out of memory */
		}
	    }
	    myAuth[numAuth].IsPrimary = 0;
	    myAuth[numAuth].ExpireTime = token.endTime;
	    myAuth[numAuth].IsLocal = 0;
	    ++numAuth;
	} else {
	    if (AnyNumber(clientName.name, token.kvno, &theNumber)) {
		myAuth[AnyExp].ViceID = theNumber;
	    } else {
		myAuth[AnyExp].UserName = NewString(clientName.name);
		if (myAuth[AnyExp].UserName == NULL) {
		    free(myAuth[AnyExp].CellName); myAuth[AnyExp].CellName = NULL;
		    EraseCellMemory();
		    return -1;		/* out of memory */
		}
	    }
	    myAuth[AnyExp].IsPrimary = 0;
	    myAuth[AnyExp].ExpireTime = token.endTime;
	    myAuth[AnyExp].IsLocal = 0;
	}
    }
    for (Ix = 0; Ix < numAuth; ++Ix) if (myAuth[Ix].IsLocal == 0 && myAuth[Ix].IsPrimary < 0) myAuth[Ix].ExpireTime = 0;    /* Invalidate the token this way. */
#endif /* AFS_ENV */
    return 0;
}

static int AddALocal(LocalName)
char *LocalName;
{/* Add the given name as a bit of local identity.  Return -1 on malloc failure or the myAuth index of the added structure. */

    if (localIx >= 0) return localIx;
    if (myAuth == NULL) {
	maxAuth = INITIALAUTHS;
	numAuth = 0;
	PrimIx = localIx = -1;
	myAuth = (struct CellAuth *) malloc(sizeof(struct CellAuth));
	if (myAuth == NULL) return -1;
	numAuth = maxAuth = 1;
	ClearSome(0, maxAuth);
    } else {
	if (numAuth >= maxAuth) {	/* make room */
	    if (GrowBasics() != 0) return -1;
	}
	++numAuth;
    }
    localIx = numAuth-1;
    myAuth[localIx].CellName = NewString(LocalName);
    if (myAuth[localIx].CellName == NULL) {
	EraseCellMemory();
	return -1;
    }
    myAuth[localIx].ViceID = getuid();
    myAuth[localIx].ExpireTime = 0x7fffffff;
    myAuth[localIx].IsLocal = 1;
    return localIx;
}

int FindCell(cellName, ppCellAuth)
char *cellName;
struct CellAuth **ppCellAuth;
{/* Return a pointer to our authentication for cell cellName, via ppCellAuth.
    Return 0 if it was found, or an error code (>0 for permanent, <0 for temporary).
	  Return 1 if we don't have any authentication in that cell, or if there's no such cell.
	      Return 2 if we're completely unauthenticated. */

    int rc;

    CheckServiceConfiguration();
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	if (myAuth == NULL) {
	    rc = GetCellBasics();
	    if (rc < 0) return rc;
	    if (rc > 0) return 3;
	}
	for (rc = 0; rc < numAuth; ++rc) {
	    if (ULstrcmp(myAuth[rc].CellName, cellName) == 0) {
		*ppCellAuth = &myAuth[rc];
		return 0;
	    }
	}
    }
#endif /* AFS_ENV */
    if (ULstrcmp(cellName, WorkstationName) == 0) {
	if (localIx < 0) {
	    rc = AddALocal(WorkstationName);
	    if (rc < 0) return -1;
	    if (PrimIx < 0 && ULstrcmp(ThisDomain, WorkstationName) == 0) myAuth[localIx].IsPrimary = 1;
	}
	*ppCellAuth = &myAuth[localIx];
	return 0;
    }
    if ((AMS_ThisDomainAuthFromWS || !AMS_ViceIsRunning) && ULstrcmp(ThisDomain, cellName) == 0) {
	if (localIx < 0) {
	    rc = AddALocal(ThisDomain);
	    if (rc < 0) return -1;
	    if (PrimIx < 0) myAuth[localIx].IsPrimary = 1;
	}
	*ppCellAuth = &myAuth[localIx];
	return 0;
    }
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) return (numAuth == 0 ? 2 : 1);
#endif /* AFS_ENV */
    return 1;
}

int FindAnyCell(ppCellAuth)
struct CellAuth **ppCellAuth;
{/* Like FindCell, except that it returns a pointer to any authenticated cell, if there is one. */

	CheckServiceConfiguration();
#ifdef AFS_ENV
	if (AMS_ViceIsRunning) {
	    int rc;

	    if (myAuth == NULL) {
		rc = GetCellBasics();
		if (rc < 0) return rc;
		if (rc > 0) return 3;
	    }
	    if (numAuth == 0) return 2;
	    *ppCellAuth = &myAuth[0];	/* first one is as good as any one */
	    return 0;
	} else
#endif /* AFS_ENV */
	{
	    return (FindCell(ThisDomain, ppCellAuth));
	}
}

int FindNextCell(ppCellAuth)
struct CellAuth **ppCellAuth;
{/* Like FindCell, except that it returns a pointer to the next authenticated cell, if there is one.  Start it by setting what's pointed to by ppCellAuth to NULL. */
	CheckServiceConfiguration();
#ifdef AFS_ENV
	if (AMS_ViceIsRunning) {
	    int rc;

	    if (myAuth == NULL) {
		rc = GetCellBasics();
		if (rc < 0) return rc;
		if (rc > 0) return 3;
	    }
	    if (numAuth == 0) return 2;
	    if (*ppCellAuth == NULL) {*ppCellAuth = &myAuth[0]; return 0;}
	    for (rc = 0; rc < numAuth; ++rc)
		if (*ppCellAuth == &myAuth[rc]) break;
	    ++rc;
	    *ppCellAuth = (rc < numAuth ? &myAuth[rc] : NULL);
	    return 0;
	} else
#endif /* AFS_ENV */
	{
	    if (*ppCellAuth == NULL) return (FindCell(ThisDomain, ppCellAuth));
	    else {*ppCellAuth = NULL; return 0;}
	}
}

#ifdef TESTINGONLYTESTING
main ()
{
    struct CellAuth *ca;
    int RC, ix;

    CheckServiceConfiguration();
    RC = FindCell(WorkstationCell, &ca);
    if (RC == 0) {
	printf("Workstation cell: ``%s'', vid %d/``%s'', expires %s.\n",
	       ca->CellName, ca->ViceID, ca->UserName ? ca->UserName : "none", NiceTime(ca->ExpireTime));
    } else {
	printf("Can't get workstation cell %s: %d\n", WorkstationCell, RC);
    }

    printf("Before referring to workstation %s:\n", WorkstationName);
    ca = NULL; ix = 1;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	printf("(%d) ``%s'', %s%svid %d/``%s'', expires %s.\n", ix, ca->CellName,
	       (ca->IsPrimary < 0 ? "" : ca->IsPrimary ? "primary, " : "non-primary, "),
	       (ca->IsLocal < 0 ? "" : ca->IsLocal ? "local, " : "non-local, "),
	       ca->ViceID, ca->UserName ? ca->UserName : "none", NiceTime(ca->ExpireTime));
	++ix;
    }
    if (RC != 0) printf("Can't find a next cell: %d\n", RC);
    RC = FindCell(WorkstationName, &ca);
    if (RC != 0) printf("Can't find workstation-name %s: %d\n", WorkstationName, RC);
    printf("After referring to workstation %s:\n", WorkstationName);
    ca = NULL; ix = 1;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	printf("(%d) ``%s'', %s%svid %d/``%s'', expires %s.\n", ix, ca->CellName,
	       (ca->IsPrimary < 0 ? "" : ca->IsPrimary ? "primary, " : "non-primary, "),
	       (ca->IsLocal < 0 ? "" : ca->IsLocal ? "local, " : "non-local, "),
	       ca->ViceID, ca->UserName ? ca->UserName : "none", NiceTime(ca->ExpireTime));
	++ix;
    }
    if (RC != 0) printf("Can't find a next cell: %d\n", RC);
    RC = FindCell(ThisDomain, &ca);
    if (RC != 0) printf("Can't find ThisDomain %s: %d\n", ThisDomain, RC);
    printf("After referring to ThisDomain name %s:\n", ThisDomain);
    ca = NULL; ix = 1;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	printf("(%d) ``%s'', %s%svid %d/``%s'', expires %s.\n", ix, ca->CellName,
	       (ca->IsPrimary < 0 ? "" : ca->IsPrimary ? "primary, " : "non-primary, "),
	       (ca->IsLocal < 0 ? "" : ca->IsLocal ? "local, " : "non-local, "),
	       ca->ViceID, ca->UserName ? ca->UserName : "none", NiceTime(ca->ExpireTime));
	++ix;
    }
    if (RC != 0) printf("Can't find a next cell: %d\n", RC);
    RC = ca_UpdateCellAuths();
    if (RC != 0) printf("Can't update the authentications: %d\n", RC);
    printf("After updating the cell authentications:\n");
    ca = NULL; ix = 1;
    for (RC = FindNextCell(&ca); RC == 0 && ca != NULL; RC = FindNextCell(&ca)) {
	printf("(%d) ``%s'', %s%svid %d/``%s'', expires %s.\n", ix, ca->CellName,
	       (ca->IsPrimary < 0 ? "" : ca->IsPrimary ? "primary, " : "non-primary, "),
	       (ca->IsLocal < 0 ? "" : ca->IsLocal ? "local, " : "non-local, "),
	       ca->ViceID, ca->UserName ? ca->UserName : "none", NiceTime(ca->ExpireTime));
	++ix;
    }
    if (RC != 0) printf("Can't find a next cell: %d\n", RC);
}
#endif /* TESTINGONLYTESTING */
