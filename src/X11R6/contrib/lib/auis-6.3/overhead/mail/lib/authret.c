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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/authret.c,v 2.16 1993/05/04 00:51:42 susan Exp $";
#endif

/* ************************************************************ *\
	authret.c
	Handy functions to build authentication and return-path information.
\* ************************************************************ */

#include <andrewos.h> /* sys/types.h strings.h sys/file.h */
#include <andyenv.h>
#include <stdio.h>
#include <pwd.h>
#include <util.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <errno.h>
#include <svcconf.h>
#include <mail.h>

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */


int GetAuthInfo(FName, pBuff)
char *FName, **pBuff;
{/* Generate authentication information for us relative to the file FName.  Put it in a static buffer, and return a pointer to that static buffer in pBuff.  Return zero for all-OK, negative numbers for temporary error conditions, and positive numbers for permanent error conditions. */
	static char AuthBuff[2000];
	auto char CellName[200];
	struct CellAuth *ca;
	int rc, OurReturn;
	char *S, *D;

	CheckServiceConfiguration();
	ca = NULL;
	OurReturn = 0;
	rc = GetCellFromFileName(FName, CellName, sizeof(CellName));
	if (rc != 0 && rc != EINVAL && rc != ENOENT) OurReturn = (vdown(rc) ? -2 : 3);
	if (rc == 0) {	/* Got a cell name; see if we're authenticated. */
		rc = FindCell(CellName, &ca);
		if (rc != 0) OurReturn = rc;
	}
	if (ca == NULL) {	/* That failed; get our home cell. */
		rc = FindAMSHomeCell(&ca);
		if (rc != 0 && OurReturn == 0) OurReturn = rc;
	}
	if (ca == NULL) {	/* That failed, too; get any cell. */
		rc = FindAnyCell(&ca);
		if (rc != 0 && OurReturn == 0) OurReturn = rc;
	}
	if (ca != NULL) FillInCell(ca);
	if (ca != NULL) {	/* Make an Auth-format string */
		sprintf(AuthBuff, "%d;%s;", ca->ViceID, ca->CellName);
		D = &AuthBuff[strlen(AuthBuff)];
		S = ca->PersonName;
		if (S != NULL) {
			while (*S != '\0') {
				if (*S == ';' || *S == '\\') *D++ = '\\';
				*D++ = *S++;
			}
		}
		*D++ = '\0';
	} else {
		AuthBuff[0] = '\0';
	}
	*pBuff = AuthBuff;
	return OurReturn;
}

static int IsPlusOK(ca)
struct CellAuth *ca;
{/* uses CheckAMSDelivery(ca->CellName), but sometimes faster. */
	if (ca->UsesAMSDelivery > 0) return ca->UsesAMSDelivery;
	return CheckAMSUseridPlusWorks(ca->CellName);
}

int GetRetPath(FName, pBuff)
char *FName, **pBuff;
{/* Generate a return-path to us relative to the file FName.  Put it in a static buffer, and return a pointer to that static buffer in pBuff.  Return zero for all-OK, negative numbers for temporary error conditions, and positive numbers for permanent error conditions. */
	static char RetPathBuff[2000];
	auto char CellName[200];
	struct CellAuth *ca;
	int rc, OurReturn;
	char *S, *D;
	struct passwd *PW;

	CheckServiceConfiguration();
	ca = NULL;
	OurReturn = 0;
	rc = GetCellFromFileName(FName, CellName, sizeof(CellName));
	if (rc != 0 && rc != EINVAL && rc != ENOENT) OurReturn = (vdown(rc) ? -2 : 3);
	if (rc == 0) {	/* Got a cell name; see if we're authenticated. */
		rc = FindCell(CellName, &ca);
		if (rc != 0) OurReturn = rc;
	}
	if (ca == NULL) {	/* That failed; get our home cell. */
		rc = FindAMSHomeCell(&ca);
		if (rc != 0 && OurReturn == 0) OurReturn = rc;
	}
	if (ca == NULL) {	/* That failed, too; get any cell. */
		rc = FindAnyCell(&ca);
		if (rc != 0 && OurReturn == 0) OurReturn = rc;
	}
	if (ca != NULL) {
		if (ca->WpError < 0) FillInCell(ca);
		if (ca->WpError != 0) OurReturn = 3;
		if (ca->UserName != NULL) {
			sprintf(RetPathBuff, "<%s%s@%s>", ca->UserName,
					(IsPlusOK(ca) >= 0 ? "+" : ""),
					ca->CellName);
		} else if (ca->PersonName != NULL) {
			for (S = ca->PersonName; *S != '\0'; ++S)
				if (index("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&'*+-./=?^_`{|}~", *S) == NULL) break;
			if (*S == '\0') {	/* each char was one we can handle */
				D = RetPathBuff;
				*D++ = '<';
				for (S = ca->PersonName; *S != '\0'; ++S) {
						/* Map spaces to underscores */
					*D++ = (*S == ' ' ? '_' : *S);
				}
				*D++ = '@';
				*D = '\0';	/* make it whole again... */
				strcat(RetPathBuff, ca->CellName);
				strcat(RetPathBuff, ">");
			}
		} else {RetPathBuff[0] = '\0'; OurReturn = (ca->WpError != 0 ? 3 : 4);}
	} else {	/* Oh, dear--no auth anywhere.  Use local auth. */
		rc = geteuid();
		if (rc == 0) rc = getuid();
		PW = getpwuid(rc);
		if (PW != NULL) {
			sprintf(RetPathBuff, "<%s%s@%s>", PW->pw_name,
				(AMS_DeliverySystem ? "+" : ""), ThisDomain);
			OurReturn = 5;
		} else {RetPathBuff[0] = '\0'; OurReturn = 6; }
	}
	*pBuff = RetPathBuff;
	return OurReturn;
}

#ifdef TESTINGONLYTESTING
main () {
	char *Whatever;
	int RC, Ix;
	static char *FNs[] = {"/afs/andrew.cmu.edu/usr13/cfe", "/tmp", "/afs/.cs.cmu.edu", "/afs/not/here"};
#define numFNs (sizeof(FNs) / sizeof(FNs[0]))
	CheckServiceConfiguration();
	printf("Testing file-relative authentication lines and return-paths.\n");
	for (Ix = 0; Ix < numFNs; ++Ix) {
		Whatever = "not-being-set";
		RC = GetAuthInfo(FNs[Ix], &Whatever);
		printf("GetAuthInfo(%s) returns %d: ``%s''.\n", FNs[Ix], RC, Whatever);
		Whatever = "not-being-set";
		RC = GetRetPath(FNs[Ix], &Whatever);
		printf("GetRetPath(%s) returns %d: ``%s''.\n", FNs[Ix], RC, Whatever);
	}
}
#endif /* TESTINGONLYTESTING */

