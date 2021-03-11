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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/fwdvalid.c,v 1.22 1994/03/01 22:57:11 rr2b Exp $";
#endif

/* fwdvalid.c: implement the ValidateAddr() routine that validates mail addresses. */

#include <andrewos.h>
#include <stdio.h>
#include <andyenv.h>
#include <ctype.h>
#include <pwd.h>
#include <util.h>
#include <mailconf.h>
#include <mail.h>
#include <parseadd.h>
#include <svcconf.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */

#ifdef WHITEPAGES_ENV
extern wp_ErrorCode WpError;
static struct wp_cd *wpCD = NULL;
static char *wp_domain = NULL;
#endif /* WHITEPAGES_ENV */

extern PARSED_ADDRESS *SingleAddress();

char fwdvalid_msgbuf[2000] = "";

static int FatalError, TempFail, Uncertain;
static char *tildeuser = "";
static int Debugging = 0;

static int ResolveTilde(Addr, laType, PrimePtr, Dom)
PARSED_ADDRESS *Addr;
int laType; char **PrimePtr; char *Dom;
{/* Try to resolve a leading tilde in the current +dist+ or +dir-insert+ argument.  If all is well, rewrite the laPrime result and return 0.  If anything else comes up, issue an error message and return non-0. */
    char *OldPrime;
    char *NewPrime, *PrPtr, *NewLocal, *UserToUse, *TagToUse;
    struct passwd *PW;

    OldPrime = *PrimePtr;
    if (*OldPrime++ != '~') return 0;
    PrPtr = index(OldPrime, '/');
    if (PrPtr == NULL) return 0;	/* not our problem */
    *PrPtr = '\0';	/* temporarily */

    UserToUse = (OldPrime[0] == '\0' ? tildeuser : OldPrime);

    PW = getcpwnam(UserToUse, ThisDomain);
    /* Now--did we find the userid? */
    if (PW == NULL) {
#ifdef WHITEPAGES_ENV
	switch (cpw_error) {
	    case wperr_NoKeysFound:
	        (void) sprintf(fwdvalid_msgbuf, "There is no userid ``%s'' for address ``%s''.", UserToUse, Addr->LocalPart);
		FatalError = 1;
		*PrPtr = '/';	/* restore the '/' */
		return 1;
	    case wperr_TooManyKeysFound:
		(void) sprintf(fwdvalid_msgbuf, "There are multiple userids ``%s'' for address ``%s''.", UserToUse, Addr->LocalPart);
		FatalError = 1;
		*PrPtr = '/';	/* restore the '/' */
		return 1;
	    default:
		(void) sprintf(fwdvalid_msgbuf, "Cannot resolve userid ``%s'': %s.", UserToUse, wp_ErrorString(cpw_error));
		FatalError = 1;
		*PrPtr = '/';	/* restore the '/' */
		return 1;
	}
#else WHITEPAGES_ENV
	FatalError = 1;
	*PrPtr = '/';	/* restore the '/' */
	return 1;
#endif /* WHITEPAGES_ENV */
    }
    *PrPtr++ = '/';	/* restore the '/' and bump PrPtr */
    if (laType == latype_DistList) {
	TagToUse = "dist";
    } else {
	if (laType == latype_DirInsert) {
	    TagToUse = "dir-insert";
	} else {
	    (void) sprintf(fwdvalid_msgbuf, "Unknown local address type (%d): %s.", laType, Addr->LocalPart);
	    FatalError = 1;
	    return 1;
	}
    }
    NewLocal = (char *) malloc(strlen(TagToUse) + strlen(PW->pw_dir) + strlen(PrPtr) + 4);
    NewPrime = (char *) malloc(strlen(PW->pw_dir) + strlen(PrPtr) + 2);
    if (NewPrime == NULL || NewLocal == NULL) {
	if (NewPrime != NULL) free(NewPrime);
	if (NewLocal != NULL) free(NewLocal);
	TempFail = 1;
	(void) strcpy(fwdvalid_msgbuf, "Out of memory.");
	return 1;
    }
    /* Got the storage.  Set it properly and install it in place. */
    (void) sprintf(NewPrime, "%s/%s", PW->pw_dir, PrPtr);
    (void) sprintf(NewLocal, "+%s+%s/%s", TagToUse, PW->pw_dir, PrPtr);
    free(*PrimePtr);
    *PrimePtr = NewPrime;
    free(Addr->LocalPart);
    Addr->LocalPart = NewLocal;
    return 0;
}


static void ValidateRecipient(Addr, PrevailingDomain)
PARSED_ADDRESS *Addr;
char *PrevailingDomain;
{
    ADDRESS_HOST *HostPtr;
    char *CanonID;
#ifdef WHITEPAGES_ENV
    char *NewName;
    wp_SearchToken STok;
    wp_PrimeKey KVal;
    wp_ErrorCode ErrCode;
    int MinMatch, OutMatch, NewLen, AMSDel, PlusOK;
    int NameSepForEntry;
    char *CAF, *p;
    PARSED_ADDRESS *CAF_ListHead, *CAF_Addr;
#endif /* WHITEPAGES_ENV */
    extern ADDRESS_HOST *MakeHost();
    int laErr, laType;
    char *laPrime, *laSecond;

    laSecond = NULL;
    laErr = la_KindDomain(Addr, &laType, &laPrime, &laSecond, PrevailingDomain);
    /* If all the hosts were just removed, we're local; add back the preferred name of our host. */
    if (Addr->Hosts == Addr->Hosts->Next) {
	CanonID = NewString(PrevailingDomain);
	if (CanonID == NULL) {
	    if (laPrime != NULL) free(laPrime);
	    TempFail = 1;
	    (void) strcpy(fwdvalid_msgbuf, "No memory storage.");
	    return;
	}
	HostPtr = MakeHost(CanonID);
	if (HostPtr == NULL) {
	    free(CanonID);
	    if (laPrime != NULL) free(laPrime);
	    TempFail = 1;
	    (void) strcpy(fwdvalid_msgbuf, "No memory storage.");
	    return;
	}
	CanonID = NULL;		/* ptr is in Host str now */
	if (AddHost(Addr, HostPtr) != PA_OK) {
	    if (laPrime != NULL) free(laPrime);
	    TempFail = 1;
	    (void) strcpy(fwdvalid_msgbuf, "AddHost failed.");
	    return;
	}
    }
    switch (laErr) {
	case laerr_NoError:
	    break;
	case laerr_UnrecSpecial:
	    if (ULstrcmp(PrevailingDomain, ThisDomain) == 0) {
		FatalError = 1;
	    } else {
		Uncertain = 1;
	    }
	    (void) strcpy(fwdvalid_msgbuf, "The text between initial plus signs in the destination address does not name a valid special address type.");
	    return;
	case laerr_SyntaxError:
	    FatalError = 1;
	    (void) strcpy(fwdvalid_msgbuf, "A local destination address is syntactically invalid.");
	    return;
	default:
	    TempFail = 1;
	    (void) sprintf(fwdvalid_msgbuf, "la_Kind temp failure %d: %s.", laErr, la_ErrorString(laErr));
	    return;
    }
    if (laType == latype_Remote) {	/* Directed to a non-local host. */
	if (laPrime != NULL) free(laPrime);
	if (Addr->MD != NULL) {
	    if (Addr->MD->Qual == mailhost_bad) {
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "Unknown host: %s.", Addr->MD->Orig);
		return;
	    } else{
		if (Addr->MD->Qual == mailhost_indeterminate) {
		    Uncertain = 1;
		    (void) sprintf(fwdvalid_msgbuf, "Uncertain whether domain %s exists and can receive mail.", Addr->MD->Orig);
		    /* fall through */
		}
	    }
	}
	if (Addr->MD != NULL && Addr->MD->Final != NULL
	    && ULstrcmp(Addr->Hosts->Prev->Name, Addr->MD->Final) != 0) {
	    char *BetterHost = NewString(Addr->MD->Final);
	    if (BetterHost != NULL) {
		free(Addr->Hosts->Prev->Name);
		Addr->Hosts->Prev->Name = BetterHost;
	    } else {
		TempFail = 1;
		(void) strcpy(fwdvalid_msgbuf, "No memory.");
		return;
	    }
	}
	if (ULstrcmp(Addr->Hosts->Prev->Name, PrevailingDomain) != 0) {
	    if (CheckAMSDelivery(Addr->Hosts->Prev->Name) > 0 || CheckAMSValidationMask(Addr->Hosts->Prev->Name) == vld_WPValid) {
		char *newDom = NewString(Addr->Hosts->Prev->Name);
		if (newDom == NULL) {
		    TempFail = 1;
		    (void) strcpy(fwdvalid_msgbuf, "No memory.");
		    return;
		}
		if (Addr->MD != NULL) {la_FreeMD(Addr->MD); Addr->MD = NULL;}
		ValidateRecipient(Addr, newDom);	/* recurse!! */
		free(newDom);
	    }
	}
	return;
    }

    /* Now see what kind of local address it is. */
    switch (laType) {
	case latype_LocalID:
	case latype_LocalName:
	    break;		/* fall through */
	case latype_DistList:
	    if (laPrime == NULL) {
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "No file was named to contain distribution information in address %s.", Addr->LocalPart);
		return;
	    }
	    if (ResolveTilde(Addr, laType, &laPrime, PrevailingDomain) != 0) {free(laPrime); return;}
	    if (strlen(laPrime) < 2 || *laPrime != '/') {
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "The file named ``%s'' is to contain distribution information, yet that name is syntactically unusable.", laPrime);
		free(laPrime);
		return;
	    }
	    free(laPrime);
	    return;
	case latype_DirInsert:
	    if (laPrime == NULL) {
		FatalError = 1;
		(void) strcpy(fwdvalid_msgbuf, "No directory was named into which mail is to be inserted.");
		return;
	    }
	    if (ResolveTilde(Addr, laType, &laPrime, PrevailingDomain) != 0) {
		free(laPrime); return;
	    }
	    if (strlen(laPrime) < 2 || *laPrime != '/') {
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "Mail is to be inserted into the directory named ``%s'', yet that name is syntactically unusable.", laPrime);
		free(laPrime);
		return;
	    }
	    free(laPrime);
	    return;
	case latype_FSMembers:
	    if (laPrime == NULL) {
		FatalError = 1;
		(void) strcpy(fwdvalid_msgbuf, "No file system group name, whose members are to receive mail, was given.");
		return;
	    }
#ifdef AFS30_ENV
	    {
	      int ret_code;
	      
	      if ((ret_code = aq_GroupP(laPrime, PrevailingDomain))<0) {
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "Encountered the following error trying to determine the groupness of the name ``%s@%s'': ``%s'' (AQ error code %d)",
			       laPrime, PrevailingDomain,
			       aq_GetLastErrorMessage(),
			       ret_code);
		free(laPrime);
		return;
	      }
	      if (ret_code==0) {
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "The name `%s' does not appear to be a file system group name in domain `%s'.", 
			       laPrime, PrevailingDomain);
		free(laPrime);
		return;
	      } 
	    }
#else /* AFS30_ENV */
				/* if we don't have PRS, just let it fall
				   through silently. */
#endif /* AFS30_ENV */
	    free(laPrime);
	    return;
	default:
	    TempFail = 1;
	    (void) sprintf(fwdvalid_msgbuf, "Unrecognized local address type in %s: %d", Addr->LocalPart, laType);
	    if (laPrime != NULL) free(laPrime);
	    return;
    }
    if (laPrime == NULL) {
	FatalError = 1;
	(void) sprintf(fwdvalid_msgbuf, "Null address in %s.", Addr->LocalPart);
	return;
    }

    /* It's a local name, either latype_LocalID or latype_LocalName--try to evaluate it. */
#ifdef WHITEPAGES_ENV
     AMSDel = CheckAMSDelivery(PrevailingDomain);
     if (AMSDel <= 0 && CheckAMSValidationMask(PrevailingDomain) != vld_WPValid) {
 	/* can't do validation within PrevailingDomain: return with no further changes */
 	free(laPrime);
 	return;
     }
     PlusOK = (AMSDel > 0 || CheckAMSUseridPlusWorks(PrevailingDomain) > 0);
     if ((!wpCD) || (strcmp(PrevailingDomain, wp_domain))) { /* need to initialize WP */
	 wp_ErrorCode err;

	 if (wpCD) cwp_Terminate(wpCD); /* free last wpCD */
	 /* Set up the WP */
	 if ((err=wp_InitializeCell(PrevailingDomain, &wpCD))!=wperr_NoError) {
	     if (laPrime != NULL) free(laPrime);
	     TempFail = 1;
	     (void) sprintf(fwdvalid_msgbuf, "White Pages for cell %s inaccessible: %s.", PrevailingDomain, wp_ErrorString(err));
	     return;
	 } else {
	     if (wp_domain) free(wp_domain);	/* alloc'd with NewString */
	     wp_domain = NewString(PrevailingDomain);
	 }
     }
     if (Debugging) (void) fprintf(stderr, "raw=%s, laType=%d, prime=%s, second=%s\n",
				    Addr->LocalPart, laType, laPrime, (laSecond == NULL ? "NULL" : laSecond));
     ErrCode = wp_SetUp(laPrime,
			 (laType == latype_LocalID ?
			  LookupUIDOnly :
			  LookupUIDWithLastPart),
			 &STok);
     if (ErrCode != wperr_NoError) {
	 free(laPrime);
	 TempFail = 1;
	 (void) sprintf(fwdvalid_msgbuf, "White Pages setup failed: %s.", wp_ErrorString(ErrCode));
	 return;
     }
    KVal = NULL;
    ErrCode = cwp_Lookup(wpCD, STok, &MinMatch, MatchAll, &OutMatch, &KVal);
    if (Debugging) {
	(void) fprintf(stderr, "ValidateRecipient: Lookup returns %d (%s).\n", ErrCode, wp_ErrorString(ErrCode));
    }
    switch (ErrCode) {
	case wperr_NoError:
	    break;	/* fall through */
	case wperr_NoKeysFound:	/* Since WP could be out of date, check /etc/passwd too */
	    FatalError = 1;
	    (void) sprintf(fwdvalid_msgbuf, "There is no such addressee %s in domain %s.", Addr->LocalPart, PrevailingDomain);
	    free(laPrime);
	    (void) wp_DeAllocate(STok);
	    return;
	case wperr_TooManyKeysFound:
	    FatalError = 1;
	    (void) sprintf(fwdvalid_msgbuf, "The addressee %s is ambiguous in domain %s.", Addr->LocalPart, PrevailingDomain);
	    free(laPrime);
	    (void) wp_DeAllocate(STok);
	    return;
	default:
	    TempFail = 1;
	    (void) sprintf(fwdvalid_msgbuf, "Can't evaluate addressee %s in domain %s: %s.", Addr->LocalPart, PrevailingDomain, wp_ErrorString(ErrCode));
	    free(laPrime);
	    (void) wp_DeAllocate(STok);
	    return;
    }
    /* Got just one result name.  Was the match too fuzzy? */
    if (OutMatch > MatchExPA) {
	FatalError = 1;
	(void) sprintf(fwdvalid_msgbuf, "The name %s is at best a fuzzy match in domain %s.", Addr->LocalPart, PrevailingDomain);
	free(KVal);
	free(laPrime);
	(void) wp_DeAllocate(STok);
	return;
    }
    (void) wp_DeAllocate(STok);
    /* OK, the string in KVal is the prime key for the unique local addressee. */
/* What we need to do here is to find out what the name separator is.  If 0 or -1, or if there's a second part or a +foo+ form, use uid+ spelling.  If there is a canonical address format, use that.  If result is >0, read the N wp field and spell that way. */

    ErrCode = cwp_Read(wpCD, KVal, wp_FieldNameToIndex("CAF"), &CAF);
    if (ErrCode == wperr_NoSuchField) {
	NameSepForEntry = CheckAMSNameSep(PrevailingDomain);
    }
    else if (ErrCode != wperr_NoError) {
	TempFail = 1;
	(void) sprintf(fwdvalid_msgbuf, "Can't read CAF field for %s: %s.", Addr->LocalPart, wp_ErrorString(ErrCode));
	free(KVal); free(laPrime);
	return;
    }
    else {
	p = CAF;
	if (*p == '-') p++;
	while (*p && isdigit(*p)) p++;
	if (!*p) {
	    NameSepForEntry = atoi(CAF);
	}
	else if (*laSecond == '\0' && ParseAddressList(CAF, &CAF_ListHead) == PA_OK) {
	    OutMatch = 0;
	    CAF_Addr = SingleAddress(CAF_ListHead, &OutMatch);
	    if (OutMatch == 1) {
		ReplaceAddress(Addr, CAF_Addr);
		FreeAddressList(CAF_ListHead);
		free(KVal);
		free(laPrime);
		return;
	    }
	    FreeAddressList(CAF_ListHead);
	}
    }

    if (NameSepForEntry > 0 && (laType == latype_LocalName || (laType == latype_LocalID && *laSecond == '\0'))) {
	    ErrCode = cwp_Read(wpCD, KVal, wp_FieldNameToIndex("N"), &CanonID);
	if (ErrCode == wperr_NoSuchField) {
	    /* Fall through and use ID spelling */;
	}
	else if (ErrCode != wperr_NoError) {
		TempFail = 1;
		(void) sprintf(fwdvalid_msgbuf, "Can't read N field for %s: %s.", Addr->LocalPart, wp_ErrorString(ErrCode));
	}
	else {
		NewName = NewString(CanonID);
		if (NewName == NULL) {
		    TempFail = 1;
		    (void) strcpy(fwdvalid_msgbuf, "No memory for local name.");
		} else {
		    for (laSecond = NewName; *laSecond != '\0'; ++laSecond) {
		    if (*laSecond == ' ') *laSecond = NameSepForEntry;
		    }
		    /* If the full-name address is ambiguous, don't use it */
		    ErrCode = AddressMatchesUnambiguously(wpCD, NewName, KVal);
		    if (ErrCode == wperr_NoError) {
			if (Addr->LocalPart != NULL) free(Addr->LocalPart);
			Addr->LocalPart = NewName;
			free(KVal); free(laPrime);
			return;
		    }
		}
	    }
	}
    ErrCode = cwp_Read(wpCD, KVal, wp_FieldNameToIndex("ID"), &CanonID);
    if (ErrCode != wperr_NoError) {
	TempFail = 1;
	(void) sprintf(fwdvalid_msgbuf, "Can't read ID field for %s: %s.", Addr->LocalPart, wp_ErrorString(ErrCode));
	free(KVal); free(laPrime);
	return;
    }
    NewLen = strlen(CanonID) + 2;
    if (laType == latype_LocalID) NewLen += strlen(laSecond);
    NewName = (char *) malloc(NewLen);
    if (NewName == NULL) {
	TempFail = 1;
	(void) strcpy(fwdvalid_msgbuf, "No memory storage.");
	free(KVal);
	free(laPrime);
	return;
    }
    if (PlusOK) {
	(void) sprintf(NewName, "%s+%s", CanonID, (laType == latype_LocalID ? laSecond : ""));
    } else {
	(void) strcpy(NewName, CanonID);
    }
    if (Addr->LocalPart != NULL) free(Addr->LocalPart);
    Addr->LocalPart = NewName;
    free(KVal);
#endif /* WHITEPAGES_ENV */
    free(laPrime);
}

static void ValidateAddresses(AddrList, PrevailingDomain)
PARSED_ADDRESS *AddrList;
char *PrevailingDomain;
{
    FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
		       switch (ThisAddr->Kind) {
			   case SIMPLE_ADDRESS:
			       ValidateRecipient(ThisAddr, PrevailingDomain);
			       break;
			   case GROUP_ADDRESS:
			       ValidateAddresses(ThisAddr->Members, PrevailingDomain);
			       break;
			   default:
			       break;
		       }
		       })
}


static void CanonicalizeList(newList)
char *newList;
{/* Canonicalize white space in the address list. */
    char *Src, *Dst, C;
    if (newList != NULL) {
	if (Debugging) {
	    (void) fprintf(stdout, "Address was: ``%s''.\n", newList);
	}
	for (Src = newList; *Src != '\0'; ++Src) {
	    if (*Src == '\n'
		|| *Src == '\r'
		|| *Src == '\t'
		|| *Src == '\f'
		|| *Src == '\v') *Src = ' ';
	}
	Dst = newList; C = ' ';
	for (Src = newList; *Src != '\0'; ++Src) {
	    if (*Src != ' ' || C != ' ') {
		C = *Src;
		*Dst++ = C;
	    }
	}
	*Dst-- = '\0';
	while (*Dst == ' ' && Dst > Src) *Dst-- = '\0';
	if (Dst == Src) {
	    FatalError = 1;
	    (void) strcpy(fwdvalid_msgbuf, "Null address list after validation.");
	    return;
	}
	if (Debugging) {
	    (void) fprintf(stdout, "Address is now: ``%s''.\n", newList);
	}
    }
}


static char *ListUnparse(AddrList, ErrPtr, InitialSize)
PARSED_ADDRESS *AddrList; int *ErrPtr, InitialSize;
{ /* Unparse the address Addr and return it as a string, or NULL on any errors */
    char *UnPBuff;
    int UnPBuffSize, Code, NumUnparsed;

    if (InitialSize < 20) InitialSize = 110;
    UnPBuffSize = InitialSize;
    UnPBuff = (char *) malloc(UnPBuffSize);
    if (UnPBuff == NULL) {*ErrPtr = PA_NO_MEM; return NULL;}
    for (;;) {
	Code = UnparseAddressList(AddrList,
				  UP_SPACES_TO_DOTS | UP_NO_COMMENTS,
				  UnPBuff, UnPBuffSize,
				  "", "", 68, &NumUnparsed);
	*ErrPtr = Code;
	if (Code == PA_OK) break;	/* leave UnPBuff alone */
	free(UnPBuff);
	UnPBuff = NULL;
	if (Code == PA_PARTIAL || Code == PA_NO_MEM) {	/* try to grow buffer */
	    UnPBuffSize *= 2;	/* double the requested size */
	    UnPBuff = (char *)malloc(UnPBuffSize);
	    if (UnPBuff == NULL) {*ErrPtr = PA_NO_MEM; break;}
	} else break;
    }
    return UnPBuff;	/* NULL on errors */
}


static int FunkyParseAddressList(Strg, OutAddr)
char *Strg; PARSED_ADDRESS **OutAddr;
{
    PARSED_ADDRESS *Addr;
    int PACode, Code2;
    char *NewN, *Ptr;

    /* Flush newlines. */
    for (Ptr = Strg; *Ptr != '\0'; Ptr++) {
	if (*Ptr == '\n' || *Ptr == '\r')
	    *Ptr = ' ';
    }

    PACode = ParseAddressList(Strg, &Addr);
    if (Debugging) {
	(void) fprintf(stderr, "Parse of address list ``%s'' returns %d.\n", Strg, PACode);
    }
    if (PACode == PA_SYNTAX_ERROR && index(Strg, ',') == NULL) {
	/* Maybe addresses are separated by WSP rather than commas (groan) */
	NewN = NewString(Strg);
	if (NewN == NULL) return PA_NO_MEM;
	Ptr = NewN;
	while (isspace(*Ptr)) ++Ptr;
	while (*Ptr != '\0') {
	    while (*Ptr != '\0' && ! isspace(*Ptr)) Ptr++;
	    /* replace delim */
	    if (isspace(*Ptr)) *Ptr++ = ',';
	    while (isspace(*Ptr)) ++Ptr;
	}
	Code2 = ParseAddressList(NewN, &Addr);
	if (Debugging) {
	    (void) fprintf(stderr, "Second-try parse of address ``%s'' returns %d.\n", NewN, Code2);
	}
	free(NewN);
	if (Code2 == PA_OK) PACode = PA_OK;	/* it worked! */
    }
    if (PACode == PA_OK) *OutAddr = Addr;
    return PACode;
}

void fwdvalid_SetTildeUser(s)
char *s;
{
  tildeuser = s;
  return;
}


int ValidateFwdAddr(/* IN */ NewAddr, /* OUT */ FixedAddr)
char *NewAddr, **FixedAddr;
{/* Takes a string, and checks it for validity as a mail address.  Returns 0 if successful.  The "canonicalized" form of NewAddr will be returned in FixedAddr (malloc'd). */

    int parse_code;
    PARSED_ADDRESS *ParsedDestinations;
    char *UnparsedAddresses;

    FatalError = TempFail = Uncertain = 0;

    parse_code = FunkyParseAddressList(NewAddr, &ParsedDestinations);
    if (parse_code != PA_OK) {
	switch (parse_code) {
	    case PA_SYNTAX_ERROR:
		FatalError = 1;
		(void) strcpy(fwdvalid_msgbuf, "Syntax error in address list.");
		break;
	    case PA_NO_MEM:
	    case PA_PARTIAL:
		TempFail = 1;
		(void) strcpy(fwdvalid_msgbuf, "No more memory.");
		break;
	    default:
		FatalError = 1;
		(void) sprintf(fwdvalid_msgbuf, "Error (%d) parsing address list.", parse_code);
		break;
	}
    } else {
	ValidateAddresses(ParsedDestinations, ThisDomain);
	if (FatalError == 0 && TempFail == 0) {
	    UnparsedAddresses = ListUnparse(ParsedDestinations, &parse_code, 2*strlen(NewAddr) + 10);
	    FreeAddressList(ParsedDestinations);
	    ParsedDestinations = NULL;
	    if (UnparsedAddresses == NULL) {
		if (parse_code == PA_NO_MEM) {
		    TempFail = 1;
		} else {
		    FatalError = 1;
		}
		(void) sprintf(fwdvalid_msgbuf, "Unparse error: %d.", parse_code);
	    } else {
		CanonicalizeList(UnparsedAddresses);
		if (FatalError == 0 && TempFail == 0) {
		    *FixedAddr = UnparsedAddresses;
		}
	    }
	}
    }
    if (FatalError != 0) return (3);
    else if (TempFail != 0) return (2);
    else if (Uncertain != 0) return (1);
    else return(0);
}

#ifdef TESTINGONLYTESTING
main(argc,argv)
int argc;
char **argv;
{
    int err;
    char *out;

    Debugging = 1;
    CheckServiceConfiguration();
    strcpy(fwdvalid_msgbuf,"");
    err=ValidateFwdAddr(argv[1], &out);
    printf("Diagnostic: %s\n",fwdvalid_msgbuf);
    printf("'%s'\n", out);
    exit(err);
}
#endif /* TESTINGONLYTESTING */
