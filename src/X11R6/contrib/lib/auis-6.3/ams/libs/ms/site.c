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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/site.c,v 1.7 1992/12/15 21:20:00 rr2b R6tape $";
#endif

#include <andyenv.h>
#include <util.h>
/* Some header files that are useful */
#include <ms.h>
#include <stdio.h>
#include <ctype.h>
#include <andrewos.h> /* sys/file.h */
#include <sys/stat.h>
#include <parseadd.h>
#include <mail.h>
#include <mailconf.h>
#include <pwd.h>

/* Some useful functions and buffers */
extern char *StripWhiteEnds();
extern int NeedToTimeOut;
extern char home[], Me[], MyMailDomain[];
extern ADDRESS_HOST *MakeHost();
extern PARSED_ADDRESS *SingleAddress();

int LookupInLocalDatabase(Addr, laType, IDpart, PostID, Domain, UnderAMSDelivery, NameSep, MaxNameMatches, Answered, MswpCodeP)
/* This routine, LookupInLocalDatabase, is a site-dependent routine for performing non-standard name validation. */
/* A trivial change to this would validate all local names as OK.  The body would have 3 lines:

  *Answered = 1;
  *MswpCodeP = (CheckAMSFmtOK(Domain) <= 0 ? MSWP_GOODNETMAIL : MSWP_GOODUSER);
  return(0):

(Sigh.  MSWP_GOODUSER is a user who is ready to receive ATK-formatted mail; MSWP_GOODNETMAIL is a user who is not necessarily ready for that.  To say that a local name is NOT valid, set *Answered to 1 and set *MswpCodeP to MSWP_CRAP.)

Of course, the AMS_LocalDatabaseValidation variable would also have to be turned on in mailconf.c, also, either statically or via an AndrewSetup or AMS-Server file.
*/
PARSED_ADDRESS *Addr; /* IN-OUT.  The address structure, from parseadd.h.  Parts should be rewritten if needed, as this will be unparsed to produce the address in the header. */
int laType; /* IN. latype_ value defined in mail.h, flags type of local address */
char *IDpart; /* IN. The part preceding the + in nsb+foobar */
char *PostID; /* IN. The part following the + in nsb+foobar */
char *Domain; /* IN. the address domain (used, e.g. in choosing white pages to open) */
int UnderAMSDelivery; /* IN. >0 if this address should be treated as an AMS_DeliverySystem one, <0 if not as one, 0 if unknown. */
int NameSep; /* IN.  >0 means it's the character to separate firstname and lastname in the validated name; <0 means use the userid; 0 means unknown. */
int MaxNameMatches; /* IN. Maximum number of matches expected */
int *Answered; /* OUT.  Boolean that says whether or not this routine actually provided an answer. */
int *MswpCodeP; /* OUT.  Integer giving the MSWP_xxx code that best describes this Boolean that says whether or not this routine actually provided an answer. */
{
    *Answered = 0;
    return(0);
}

#ifdef USE_MMDF_ENV
#include <mmdf.h>

int LookupInMMDFDatabase(Addr, laType, IDpart, PostID, Domain, UnderAMSDelivery, NameSep, MaxNameMatches, Answered, MswpCodeP)
PARSED_ADDRESS *Addr;
int laType;
char *IDpart;
char *PostID;
char *Domain;
int UnderAMSDelivery;
int NameSep;
int MaxNameMatches;
int *Answered;
int *MswpCodeP;
{

    struct rp_bufstruct reply;
    int len;

    mm_sbinit();
    if (rp_isbad(mm_winit(NULL, "mv", "postmaster")) ||
	rp_isbad(mm_rrply(&reply, &len)) ||
	rp_isbad(reply.rp_val)) {
	*MswpCodeP = MSWP_CRAP;
	return(0);
    }
    if (rp_isbad(mm_wadr(NULL,Addr->LocalPart)) ||
        rp_isbad(mm_rrply(&reply, &len))) {
	    *MswpCodeP = MSWP_CRAP;
    }
    else switch(rp_gval(reply.rp_val)) {
    case RP_AOK:
    case RP_DOK:
	    *MswpCodeP = (CheckAMSFmtOK(Domain) <= 0 ? MSWP_GOODNETMAIL : MSWP_GOODUSER);
	    break;
    default:
	    *MswpCodeP = MSWP_CRAP;
    }
    mm_end(-1);
    *Answered = 1;
    return(0);
}

#endif /* USE_MMDF_ENV */
int GetNameFromGecos(GecosField, LoginID, Domain, PersonalNameP)
/* Find the User name in a gecos field, if posible, returning the malloced result */
char *GecosField;	/* IN   The gecos field from the password file for userid */
char *LoginID;	/* IN   The login id of the user */
char *Domain;	/* IN   Name of this domain */
char **PersonalNameP;	/* OUT  The users full name, freshly allocated. */
{
    if (AMS_GecosHacks && ULstrcmp(Domain, ThisDomain) == 0) {
	char *ampersand, *strt, *othr;
	char NameCopy[250];
	int len;

	strncpy(NameCopy, GecosField, sizeof(NameCopy));
	NameCopy[sizeof(NameCopy)-1] = '\0';
	strt = NameCopy;
	if (*strt == '*') ++strt;
	for (othr = strt; *othr != '\0'; ++othr)	{   /* look for sysV gecos format */
	    if (*othr == '-' && *othr != '\0') {	/* Aha!  digits, hyphen: call it sysV. */
		++othr;
		strt = othr;
		break;
	    }
	    if (!isdigit(*othr)) break;   /* nope--not a leading digit string */
	}
	othr = strchr(strt, ',');
	if (othr != NULL) *othr = '\0';
	ampersand = strchr(strt, '&');
	len = strlen(strt) + 1;
	if (ampersand != NULL) {
	    len += strlen(LoginID);
	}
	*PersonalNameP = malloc(len);
	if (*PersonalNameP != NULL) {
	    if (ampersand != NULL) {
		strncpy(*PersonalNameP, strt, ampersand - strt + 1);
		othr = (*PersonalNameP) + (ampersand - strt);
		strcpy(othr, LoginID);
		if (islower(*othr)) *othr = toupper(*othr);
		strcat(*PersonalNameP, ampersand+1);
	    } else {
		strcpy(*PersonalNameP, strt);
	    }
	}
    } else {
	*PersonalNameP = malloc(1+strlen(GecosField));
	if (*PersonalNameP != NULL) strcpy(*PersonalNameP, GecosField);
    }
    return(0);
}
