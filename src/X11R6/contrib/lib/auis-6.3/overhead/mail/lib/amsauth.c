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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/amsauth.c,v 2.19 1993/05/04 00:51:42 susan Exp $";
#endif

/* ************************************************************ *\
	amsauth.c
	Keep track of the current AMSHome cell.
\* ************************************************************ */

#include <andyenv.h>
#include <stdio.h>
#include <andrewos.h> /* sys/types.h strings.h sys/file.h */
#include <errno.h>
#include <util.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <mailconf.h>
#include <mail.h>

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */

static struct CellAuth *AMSHome = NULL;
static int AMSHomeVal = -1;
char AMSHome_errmsg[1500];

void ForgetAMSHome()
{/* Erase all memory of the AMS home cell. */
    AMSHome = NULL;
    AMSHomeVal = -1;
    EraseCellMemory();
}

int FindAMSHomeCell(ppCellAuth)
struct CellAuth **ppCellAuth;
{/* Returns a pointer to the cell that AMS thinks is the user's home, if there is one.
    Return 1 if have to use the workstation cell (not prime auth), 2 if no authentication at all. */
    int rc;
#ifdef AFS_ENV
    int isLocal;
    struct cell_msPath *MSP;
    char *resultAuth, *homeVal;
    char homeCell[200];
#endif /* AFS_ENV */
    struct CellAuth *hca;

    AMSHome_errmsg[0] = '\0';
    CheckAMSConfiguration();
    if (AMSHome != NULL && AMSHomeVal >= 0) {
	*ppCellAuth = AMSHome;
	return AMSHomeVal;
    }
    *ppCellAuth = NULL;
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	errno = 0;
	homeVal = getprofile("AMSHome");
	if (homeVal == NULL) {
	    if (tfail(errno)) {
		sprintf(AMSHome_errmsg, "Can't read AMSHome preference: %s", UnixError(errno));
		return -4; /* more temp fail */
	    } /* Else clause is the non-existent AMSHome entry; fall through. */
	} else {  /* Check the value of the user's preference option. */
	    if (strcmp(homeVal, "*") == 0) homeVal = WorkstationName;
	    else if (strcmp(homeVal, "&") == 0 && WorkstationCell[0] != '\0') homeVal = WorkstationCell;
	    rc = FindCell(homeVal, &hca);
	    if (rc != 0 || hca == NULL) {
		if (rc == 1) {
		    sprintf(AMSHome_errmsg, "No authentication for AMSHome domain %s", homeVal);
		} else if (rc == 2) {
		    sprintf(AMSHome_errmsg, "Not authenticated (checking for AMSHome %s)", homeVal);
		} else {
		    sprintf(AMSHome_errmsg, "%s failure (%d) finding AMSHome domain %s", (rc > 0 ? "Permanent" : "Temporary"), rc, homeVal);
		}
		return (rc == 1 ? 7 : rc);
	    }
	    if (hca->UsesAMSDelivery == 0) hca->UsesAMSDelivery = CheckAMSDelivery(hca->CellName);
	    /* Given that the user's asking to use domain homeVal, see if the AMS will be able to use homeVal. */
	    /* If homeVal is running AMS delivery, no problem.  If homeVal is ThisDomain, then AndrewSetup tells us how to use AMS. */
	    if (ULstrcmp(hca->CellName, ThisDomain) == 0 || hca->UsesAMSDelivery >= 0) {
		if (hca->UsesAMSDelivery < 0 && AMS_ViceIsRunning && !AMS_LocalMailSystemExists && ULstrcmp(ThisDomain, WorkstationName) == 0 && ULstrcmp(ThisDomain, WorkstationCell) != 0) {
		    sprintf(AMSHome_errmsg, "ThisDomain %s unacceptable as AMSHome because it doesn't run a mail system", hca->CellName);
		    return 5;   /* ThisDomain is unusable. */
		}
		if (ULstrcmp(hca->CellName, ThisDomain) != 0 && hca->UsesAMSDelivery == 0) {
		    sprintf(AMSHome_errmsg, "Temp failure checking whether AMSHome %s runs AMS delivery", hca->CellName);
		    return -4;	/* temp fail */
		}
		sprintf(AMSHome_errmsg, "AMSHome %s OK: ", hca->CellName);
		if (hca->UsesAMSDelivery > 0) strcat(AMSHome_errmsg, "runs AMS delivery");
		else strcat(AMSHome_errmsg, "is ThisDomain");
		AMSHome = hca;  /* Otherwise, we can use it. */
		AMSHomeVal = 0;
		*ppCellAuth = AMSHome;
		return AMSHomeVal;
	    }
	    /* Hm.  Not AMS delivery, not ThisDomain.  Could it be a non-AMS-delivery cell that nonetheless is configured OK?  Check for the presence of the /afs/CELL/service/configuration/AMS-Server file. */
	    errno = 0;
	    rc = CheckAMSDfMSPath(hca->CellName, &MSP);
	    if (rc < 0) {
		if (tfail(errno)) {
		    sprintf(AMSHome_errmsg, "Cannot check AMSHome %s for configuredness: %s", hca->CellName, UnixError(errno));
		    return -4;
		} else {
		    sprintf(AMSHome_errmsg, "AMSHome %s is not ThisDomain (%s) and is not configured for AMS use", hca->CellName, ThisDomain);
		     return 8;
		}
	    }
	    sprintf(AMSHome_errmsg, "AMSHome %s OK: configured for AMS use", hca->CellName);
	    AMSHome = hca;  /* Otherwise, we can use it. */
	    AMSHomeVal = 0;
	    *ppCellAuth = AMSHome;
	    return AMSHomeVal;
	}
	/* The user gave no *.AMSHome: xxx field. */
	if (AMS_DefaultToAFSCellMail > 0) isLocal = 0;
	else if (AMS_DefaultToAFSCellMail == 0 && AMS_LocalMailSystemExists) isLocal = 1;
	else if (!AMS_LocalMailSystemExists) isLocal = 0;
	else if (ULstrcmp(ThisDomain, WorkstationCell) == 0) isLocal = 0;
	else if (ULstrcmp(ThisDomain, WorkstationName) == 0) isLocal = 1;
	else {	/* WS admin hasn't said which is which.  Guess based on HOME location. */
	    homeVal = getMyHome();
	    if (homeVal != NULL) {
		homeCell[0] = '\0';
		rc = GetCellFromFileName(homeVal, homeCell, sizeof(homeCell));
		if (rc == EINVAL && homeCell[0] == '\0') {
		    isLocal = 1;   /* On the local disk. */
		} else {
		    isLocal = 0;   /* Somewhere in AFS. */
		}
	    } else isLocal = 1;
	}
	resultAuth = (isLocal ? WorkstationName : WorkstationCell);
	if (resultAuth == NULL || resultAuth[0] == '\0') {
	    sprintf(AMSHome_errmsg, "Can't guess a (%slocal) domain: null WSName '%s' or WSCell '%s'", (isLocal ? "" : "non"), (WorkstationName ? WorkstationName : ""), (WorkstationCell ? WorkstationCell : ""));
	    return -5;
	}
	hca = NULL;
	rc = FindCell(resultAuth, &hca);
	if (rc != 0 || hca == NULL) {
	    sprintf(AMSHome_errmsg, "%s failure (%d) finding default domain %s", (rc > 0 ? "Permanent" : "Temporary"), rc, resultAuth);
	    if (rc == 1) strcat(AMSHome_errmsg, "; no authentication in default domain");
	    return (rc == 1 ? 7 : rc);
	}
	if (hca->UsesAMSDelivery == 0)
	    hca->UsesAMSDelivery = CheckAMSDelivery(hca->CellName);
	if (hca->UsesAMSDelivery == 0 && ULstrcmp(hca->CellName, ThisDomain) != 0) {
	    sprintf(AMSHome_errmsg, "Temp fail checking default domain %s (not ThisDomain %s) for AMS delivery", hca->CellName, ThisDomain);
	    return -4;		/* temp fail */
	}
	if (hca->UsesAMSDelivery > 0 || ULstrcmp(hca->CellName, ThisDomain) == 0) {
	    sprintf(AMSHome_errmsg, "Default domain %s OK: either is ThisDomain (%s) or runs AMS delivery", hca->CellName, ThisDomain);
	    AMSHome = hca;
	    AMSHomeVal = 0;
	    *ppCellAuth = AMSHome;
	    return AMSHomeVal;
	}
	errno = 0;
	rc = CheckAMSDfMSPath(hca->CellName, &MSP);
	if (rc < 0) {
	    if (tfail(errno)) {
		sprintf(AMSHome_errmsg, "Cannot check default domain %s for configuredness: %s", hca->CellName, UnixError(errno));
		return -4;
	    } else {
		sprintf(AMSHome_errmsg, "Default domain %s is not ThisDomain (%s) and is not configured for AMS use", hca->CellName, ThisDomain);
		return 8;
	    }
	}
	sprintf(AMSHome_errmsg, "Default domain %s OK: configured for AMS use", hca->CellName);
	AMSHome = hca;  /* Otherwise, we can use it. */
	AMSHomeVal = 0;
	*ppCellAuth = AMSHome;
	return AMSHomeVal;
    } else
#endif /* AFS_ENV */
    {
	rc = FindCell(ThisDomain, &hca);
	if (rc != 0) rc = FindCell(WorkstationName, &hca);
	if (rc == 0) {
	    AMSHome = hca;
	    AMSHomeVal = rc;
	    *ppCellAuth = hca;
	}
	return rc;
    }
}

int SetAMSHomeCell(cellAuth)
struct CellAuth *cellAuth;
{/* Choose the given cell to operate as the AMS home cell.  Returns 0 if all OK, <0 for a temp fail, >0 for persistent failures.  Returning 1 means that the given cell can't be an AMS home cell since it isn't the workstation cell and it doesn't run AMS delivery.  Returning 2 means that there aren't any authentications.  Returning -4 means that the given cell can't be set as an AMS home cell since it's not the workstation cell and it's not clear that the cell runs AMS delivery. */
    int rc;
    struct CellAuth *othCA;
#ifdef AFS_ENV
    struct cell_msPath *MSP;
#endif /* AFS_ENV */

    CheckServiceConfiguration();
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	othCA = NULL;
	FindCell(cellAuth->CellName, &othCA);
	if (othCA != cellAuth) return 5;
	if (cellAuth->UsesAMSDelivery == 0)
	    cellAuth->UsesAMSDelivery = CheckAMSDelivery(cellAuth->CellName);
	if (cellAuth->UsesAMSDelivery > 0
	    || ULstrcmp(cellAuth->CellName, ThisDomain) == 0) {
	    sprintf(AMSHome_errmsg, "%s ok: either AMS delivery or is ThisDomain (%s)", cellAuth->CellName, ThisDomain);
	    AMSHome = cellAuth;
	    AMSHomeVal = 0;
	    return 0;
	}
	errno = 0;
	rc = CheckAMSDfMSPath(cellAuth->CellName, &MSP);
	if (rc >= 0) {
	    sprintf(AMSHome_errmsg, "%s ok: configured for AMS use", cellAuth->CellName);
	    AMSHome = cellAuth;
	    AMSHomeVal = 0;
	    return 0;
	}
	if (tfail(errno)) {
	    sprintf(AMSHome_errmsg, "Cannot check %s for configuredness: %s", cellAuth->CellName, UnixError(errno));
	    return -4;
	} else {
	    if (cellAuth->UsesAMSDelivery == 0) {
		sprintf(AMSHome_errmsg, "%s unknown AMS delivery, not AMS configured", cellAuth->CellName);
		return -4;
	    }
	    sprintf(AMSHome_errmsg, "AMSHome %s is not ThisDomain (%s) and is not configured for AMS use", cellAuth->CellName, ThisDomain);
	    return 8;
	}
    } else
#endif /* AFS_ENV */
    {
	rc = FindCell(WorkstationName, &othCA);
	if (rc != 0) return rc;
	return (othCA == cellAuth ? 0 : 5);
    }
}

#ifdef TESTINGONLYTESTING
main () {
    struct CellAuth *ca;
    int RC;

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

    RC = FindAMSHomeCell(&ca);
    if (RC == 0) {
	printf("AMS home cell: ``%s'', vid %d, expires %s.\n",
	       ca->CellName, ca->ViceID, NiceTime(ca->ExpireTime));
	printf("(%s.)\n", AMSHome_errmsg);
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
	printf("Can't get AMS home cell: %d; %s\n", RC, AMSHome_errmsg);
    }	
}
#endif /* TESTINGONLYTESTING */

