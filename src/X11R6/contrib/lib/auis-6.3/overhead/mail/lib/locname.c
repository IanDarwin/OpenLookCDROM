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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/locname.c,v 2.19 1992/12/15 21:03:27 rr2b R6tape $";
#endif

/*
	locname.c -- Low-level resolver of local addresses
*/

#include <andrewos.h> /* sys/types.h strings.h */
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef hpux
/*  inet.h not included in hpux distribution.
    Only the following declaration is needed */
char *inet_ntoa();
#else /* hpux */
#include <arpa/inet.h>
#endif /* hpux */
#include <ctype.h>
#include <util.h>
#include "parseadd.h"
#include "mailconf.h"
#include "mail.h"

static struct MailDom mdRoot = {NULL, NULL, 1, NULL, NULL};
static char ThisHostName[250] = "";
static char ThisHostAddr[20] = "";

static struct MailDom *newMD()
{/* Allocate and return a new MailDom structure. */
    struct MailDom *md;

    md = (struct MailDom *) malloc(sizeof(struct MailDom));
    if (md == NULL) return NULL;
    md->Next = md->Prev = NULL;
    md->Refs = 0;
    md->Orig = md->Final = NULL;
    md->Qual = mailhost_indeterminate;
    md->NumFwds = 0;
    md->FwdPrefs = NULL;
    md->Fwds = NULL;
    return md;
}

static int EvalRec(md)
struct MailDom *md;
{/* Put md->Orig through a cycle of evaluation.  Return 0 if out of memory. */
#define	DomLen 150
#define	MaxMX	2   /* Bump this when can get more than one MX rec */
    auto char final[DomLen];
    /*	auto unsigned short MXPrefs[MaxMX];	*/
    auto char MXHosts[MaxMX][DomLen];

    if (md->Qual != mailhost_indeterminate) return 1;	/* done already */
    final[0] = '\0';
    MXHosts[0][0] = '\0';
    md->Qual = ValidateDomainMail(md->Orig, final, sizeof(final),
				   &MXHosts[0][0], DomLen, 10);
    if (final[0] != '\0') {
	if (md->Final != NULL) free(md->Final);
	md->Final = NewString(final);
	if (md->Final == NULL) {md->Qual = mailhost_indeterminate; return 0;}
    }
    if (MXHosts[0][0] != '\0') {		/* just returns one MX record for now */
	if (md->Fwds != NULL) {
	    if (md->Fwds[0] != NULL) free(md->Fwds[0]);
	    free(md->Fwds);
	}
	md->Fwds = (char **) malloc(1 * sizeof(char *));
	if (md->Fwds == NULL) return 1;
	md->Fwds[0] = NewString(&MXHosts[0][0]);
	if (md->Fwds[0] == NULL) {free(md->Fwds); md->Fwds = NULL; return 1;}
	if (md->FwdPrefs != NULL) free(md->FwdPrefs);
	md->FwdPrefs = (unsigned short int *) malloc(1 * sizeof(unsigned short int));
	if (md->FwdPrefs == NULL) {free(md->Fwds[0]); free(md->Fwds); md->Fwds = NULL; return 1;}
	md->FwdPrefs[0] = 1;
	md->NumFwds = 1;
    }
    return 1;
}

static int TestMD(MDName, pMD, currDom)
char *MDName; struct MailDom **pMD; char *currDom;
{/* Test the given name and return a pointer to its struct MailDom.  Return -1 if it can't be done somehow, 0 if it's the given currDom domain name, and 1 if it's done OK. */
    struct MailDom *md;

    if (ULstrcmp(MDName, currDom) == 0) return 0;
    /* This next clause takes effect only for those workstations (AFS clients) that have no independent mail systems of their own. */
    if (AMS_LocalMailSystemExists == 0 && ULstrcmp(WorkstationCell, currDom) == 0 &&
	 (ULstrcmp(MDName, ThisHostName) == 0
	  || ULstrcmp(MDName, ThisHostAddr) == 0)) return 0;
    for (md = (mdRoot.Next); md != &mdRoot; md = md->Next) {
	if ((md->Orig != NULL && ULstrcmp(MDName, md->Orig) == 0)
	    || (md->Final != NULL && ULstrcmp(MDName, md->Final) == 0))
	{*pMD = md; return 1;}
    }
    md = newMD();
    if (md == NULL) return -1;
    md->Refs = 0;
    md->Orig = NewString(MDName);
    if (md->Orig == NULL) {free(md); return -1;}
    if (EvalRec(md) == 0) {md->Refs = 1; la_FreeMD(md); return -1;}
    if (md->Final != NULL) {
	if (ULstrcmp(md->Final, currDom) == 0
	    /* Again, this next clause takes effect only when the workstation has no independent mail system of its own. */
	    || (AMS_LocalMailSystemExists == 0
		&& ULstrcmp(WorkstationCell, currDom) == 0 
		&& ULstrcmp(md->Final, ThisHostName) == 0))
	{md->Refs = 1; la_FreeMD(md); return 0;}
    }
    *pMD = md; return 1;
}

int la_KindDomain(Addr, outType, outPrime, outSecond, Domain)
PARSED_ADDRESS *Addr;
int *outType;
char **outPrime, **outSecond, *Domain;
{
    /* Pass it an Addr; it fills in outType, outPrime, and, optionally, outSecond.  If outPrime is non-null, it is malloc()'ed storage; free it when you're done.  Domain will be used as the ``current'' default mail domain.
	*/
    int HostCount, LastLocal, SpecIx, DomTest;
    char *dum, *dum2, *CopyLocal, *TypeCode;
    struct MailDom *md;
    static 	struct {char *specialName; int retCode;} Specials[] = {
	{"dist",		latype_DistList},
	{"dlist",		latype_DistList},
	{"dir-insert",	latype_DirInsert}, 
	{"fs-members",	latype_FSMembers},
    };
#define NumSpecials (sizeof(Specials) / sizeof(Specials[0]))

	*outPrime = *outSecond = NULL;
	HostCount = 0;
	FOR_ALL_REVERSE_HOSTS(hst, Addr, {++HostCount;})

	  if (mdRoot.Next == NULL) {	/* Initialize the list head */
	      struct in_addr ThisAddr;

	      CheckAMSConfiguration();
	      mdRoot.Next = &mdRoot;
	      mdRoot.Prev = &mdRoot;
	      mdRoot.Refs = 1;
	      GetHostDomainName(ThisHostName, sizeof(ThisHostName));
	      ThisAddr.s_addr = getaddr();
	      if (ThisAddr.s_addr != 0) sprintf(ThisHostAddr, "[%s]", inet_ntoa(ThisAddr));
	  }

	md = Addr->MD;
	if (md != NULL && Addr->Hosts != NULL
	    && Addr->Hosts != Addr->Hosts->Prev) {
	    ADDRESS_HOST *hst;
	    hst = Addr->Hosts->Prev;
	    if ((md->Orig != NULL && ULstrcmp(md->Orig, hst->Name) == 0)
		|| (md->Final != NULL && ULstrcmp(md->Final, hst->Name) == 0)) {
		EvalRec(md);
		*outType = latype_Remote;
		return laerr_NoError;
	    }
	}
	md = Addr->MD;
	if (md != NULL) {la_FreeMD(md); md = NULL;}
	LastLocal = 1;
	FOR_ALL_REVERSE_HOSTS(hst, Addr, {
			      DomTest = TestMD(hst->Name, &md, Domain);
			      if (DomTest == 0) {
				  LastLocal = 1;
				  if (HostCount > 1) {
				      FreeHost(hst); --HostCount;
				  } else if (strcmp(hst->Name, Domain) != 0) {
				      dum = NewString(Domain);
				      if (dum == NULL) return laerr_OutOfMemory;
				      free(hst->Name); hst->Name = dum;
				  }
			      } else {LastLocal = 0; break;}
 			      })
	if (LastLocal != 0 && index(Addr->LocalPart, '!') != NULL) {
	      if (CheckAMSUUCPSupp(Domain) > 0) LastLocal = 0;	/* not really local */
	  }
	if (LastLocal == 0) {
	      if (md != NULL) {
		  ++(md->Refs);
		  if (md->Next == NULL) {	/* A new one; link it in. */
		      md->Next = mdRoot.Next;
		      md->Prev = &mdRoot;
		      mdRoot.Next->Prev = md;
		      mdRoot.Next = md;
		  }
	      }
	      Addr->MD = md;
	      *outType = latype_Remote;
	      return laerr_NoError;
	}
	CopyLocal = NewString(Addr->LocalPart);
	if (CopyLocal == NULL) return laerr_OutOfMemory;
	if (Unquote(CopyLocal) != PA_OK) return laerr_SyntaxError;
	if (CheckAMSUseridPlusWorks(Domain) >= 0) {
	    if (*CopyLocal == '+' && CheckAMSDelivery(Domain) >= 0) {
		/* a special identifier */
		TypeCode = &CopyLocal[1];
		dum = index(TypeCode, '+');
		if (dum == NULL) {free(CopyLocal); return laerr_SyntaxError;}
		*dum++ = '\0';
		if (*dum == '\0') {free(CopyLocal); return laerr_SyntaxError;}
		for (SpecIx = 0; SpecIx < NumSpecials; ++SpecIx) {
		    if (ULstrcmp(TypeCode, Specials[SpecIx].specialName) == 0) {
			dum2 = NewString(dum);
			free(CopyLocal);
			if (dum2 == NULL) return laerr_OutOfMemory;
			*outPrime = dum2;
			*outType = Specials[SpecIx].retCode;
			return laerr_NoError;
		    }
		}
		free(CopyLocal);		/* No such code found */
		return laerr_UnrecSpecial;
	    }
	    if (strncmp(CopyLocal, "lost+found", 10) == 0)
		dum2 = &CopyLocal[9];
	    else
		dum2 = CopyLocal;
	    dum = index(dum2, '+');
	    if (dum == NULL) dum = index(dum2, '#');
	    if (dum != NULL) {
		for (dum2 = dum+1; *dum2 != '\0'; ++dum2) {
		    if (isspace(*dum2)) {
			free(CopyLocal);	/* white space can't be after + or # */
			return laerr_BadSecond;
		    }
		}
		*dum++ = '\0';
		*outSecond = dum;
		*outPrime = CopyLocal;
		*outType = latype_LocalID;
		return laerr_NoError;
	    }
	}
	CanonicalizePersonName(CopyLocal);
	*outPrime = CopyLocal;
	*outType = latype_LocalName;
	return laerr_NoError;
}

int la_Kind(Addr, outType, outPrime, outSecond)
PARSED_ADDRESS *Addr;
int *outType;
char **outPrime, **outSecond;
{
    /* Pass it an Addr; it fills in outType, outPrime, and, optionally, outSecond.  If outPrime is non-null, it is malloc()'ed storage; free it when you're done.
    */
    char *PrevailingDomain;
    struct CellAuth *ca;

    CheckAMSConfiguration();
    ca = NULL;
    FindAMSHomeCell(&ca);
    PrevailingDomain = (ca != NULL ? ca->CellName : WorkstationCell);
    return la_KindDomain(Addr, outType, outPrime, outSecond, PrevailingDomain);
}

char *la_ErrorString(errcode)
int errcode;
{	/* Return a static string describing the laerr_XXX code */
    static char *ErrDesc[] = {
	"no error",		/* laerr_NoError */
	"out of memory",		/* laerr_OutOfMemory */
	"syntax error",		/* laerr_SyntaxError */
	"unrecognized type",	/* laerr_UnrecSpecial */
	"white pages error",	/* laerr_WPerror */
	"invalid text after plus sign"	/* laerr_BadSecond */
    };
#define	NumErrorDescs	(sizeof(ErrDesc) / sizeof(ErrDesc[0]))
    static char ErrBuf[60];

    if (errcode >= 0 && errcode < NumErrorDescs)
	return ErrDesc[errcode];
    sprintf(ErrBuf, "local-address error %d", errcode);
    return ErrBuf;
}
