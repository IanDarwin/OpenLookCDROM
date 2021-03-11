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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/ckamsdel.c,v 2.25 1993/05/04 00:51:42 susan Exp $";
#endif

/* ************************************************************ *\
	ckamsdel.c
	Check for AMS delivery system attributes in domains that may not even be cells.
\* ************************************************************ */

#include <andrewos.h> /* sys/types.h strings.h sys/file.h */
#include <andyenv.h>
#include <stdio.h>
#include <util.h>
#include <mailconf.h>
#include <mail.h>
#include <ctype.h>
#include <errno.h>

extern int errno;

#ifndef _IBMR2
extern char *malloc();
extern char *realloc();
#endif /* _IBMR2 */


/* Private structure to retain information about each domain asked for. */
static struct AMSConfig {
    struct AMSConfig	*Next;
    char		*Domain;
    int		AMSDel;	/* <0 no, 0 unknown, >0 yes */
    int		NameSep;	/* <0 none, 0 unknown, >0 the char to use */
    char		*MBoxName;	/* 0 unknown; if AMSDel>0, the name to use */
    char		*PManName;	/* 0 unknown; if AMSDel>0, the name to use */
    int		numdflt_MSElts;	/* number of cell_msPath structures allocated in array */
    struct cell_msPath	*dflt_MSElts;    /* array of cell_msPath structures. */
    int		ValidMask;	/* Mask of vld_*Valid values saying how name validation is done */
    int		ATKFmtOK;	/* <0 no, 0 unknown, >0 yes */
    int		UUCPSupp;	/* <0 no, 0 unknown, >0 yes */
    char		*WPIAddr;	/* 0 unknown, else the name to use */
    int		UseridPlusOK;	/* <0 no, 0 unknown, >0 yes */
} *AMSConfigRoot = NULL;

#ifdef AMS_DELIVERY_ENV
static struct AMSConfig *getThis(someDomain, pAlloc)
char *someDomain; int *pAlloc;
{/* Get, or allocate, a struct AMSConfig record corresponding to the given domain. */
    register struct AMSConfig *acf;

    for (acf = AMSConfigRoot; acf != NULL; acf = acf->Next) {
	if (ULstrcmp(someDomain, acf->Domain) == 0) {
	    *pAlloc = 0;
	    return acf;
	}
    }
    if (acf == NULL) {
	acf = (struct AMSConfig *) malloc(sizeof(struct AMSConfig));
	if (acf != NULL) {
	    bzero(acf, sizeof(struct AMSConfig));
	    acf->Domain = NewString(someDomain);
	    if (acf->Domain == NULL) {
		free(acf); acf = NULL;
	    } else {
		acf->ValidMask = -1;
		*pAlloc = 1;
	    }
	}
    }
    return acf;
}
#endif /* AMS_DELIVERY_ENV */

int CheckAMSDelivery(someDomain)
char *someDomain;
{/* Test whether the given domain runs the AMS delivery system.  Return +1 if it does, -1 if it doesn't, and 0 if you can't tell. */
#ifdef AMS_DELIVERY_ENV
    char *MailQDir;
    struct CellAuth *ca;
    int fc, xl, alloced;
    register struct AMSConfig *acf;
#ifdef AFS_ENV
    char DirCell[200];
#endif /* AFS_ENV */
#endif /* AMS_DELIVERY_ENV */

    /* If AMS_DELIVERY_ENV is off, that means that we have no code to support AMS delivery in the current build.  But if AMS_DeliverySystem is off, that means only that there's no AMS delivery system in ThisDomain, while the AMS delivery system may be in some other domains.  Thus, even if AMS_DeliverySystem is off, we check explicitly. */
#ifdef AMS_DELIVERY_ENV
    CheckAMSConfiguration();
    acf = getThis(someDomain, &alloced);
    if (acf != NULL && !alloced && acf->AMSDel != 0) return acf->AMSDel;
    ca = NULL;
    FindCell(someDomain, &ca);
    xl = strlen(someDomain) + strlen(CellCommonPrefix);
    xl += strlen(CellCommonSuffix) + strlen(CellCommonMailQueueDirSuffix) + 2;
    MailQDir = malloc(xl);
    if (MailQDir == NULL) {
	if (acf != NULL && alloced) free(acf);
	errno = ENOMEM;
	return 0;
    }
/* Check whether ``/afs/somedomain/service/mailqs'' exists. */
    strcpy(MailQDir, CellCommonPrefix);
    LCappend(MailQDir, someDomain);
    strcat(MailQDir, CellCommonSuffix);
    strcat(MailQDir, CellCommonMailQueueDirSuffix);
    fc = access(MailQDir, X_OK);
#ifdef AFS_ENV
    if (fc == 0 && AMS_ViceIsRunning) {/* Ensure that any dir we found is in the correct cell. */
	if (GetCellFromFileName(MailQDir, DirCell, sizeof(DirCell)) == 0 && ULstrcmp(DirCell, someDomain) != 0) {fc = -1; errno = EMLINK;}	/* mis-configured */
    }
#endif /* AFS_ENV */
    if (fc == 0) {
	xl = (ca == NULL ? 1 : 2);  /* yes: there is AMS delivery there */
    } else if (tfail(errno)) {
	xl = 0;	/* unknown */
    } else {
	xl = -1;	/* no; no AMS delivery there. */
    }
    free(MailQDir);
/* We have a backup path to get this info for our local domain, if we got a temp fail here. */
    if (xl == 0 && ULstrcmp(someDomain, ThisDomain) == 0) {
	xl = (AMS_DeliverySystem ? 1 : -1);
    }
    if (ca != NULL) ca->UsesAMSDelivery = xl;  /* If we're auth'd in the cell, stash the result. */
    if (acf != NULL) {
	acf->AMSDel = xl;
	if (alloced) {
	    acf->Next = AMSConfigRoot;	/* now link onto list */
	    AMSConfigRoot = acf;
	}
    }
    return xl;
#else /* AMS_DELIVERY_ENV */
    return -1;	/* as if there were no ``mailqs'' dirs anywhere. */
#endif /* AMS_DELIVERY_ENV */
}

#ifdef AMS_DELIVERY_ENV
static int readBool(inval, filename)
char *inval, *filename;
{/* Read a Boolean from the string ``inval''. */
    char *val;

    val = inval;
    while (*val != '\0' && (isspace(*val) || *val == '-')) ++val;
    switch (*val) {
	case 'y': case 'Y': case '1': case 't': case 'T':
	    return 1;
	case 'n': case 'N': case '0': case 'f': case 'F':
	    return 0;
	default:
	    fprintf(stderr, "Warning: Bad Boolean value '%s' reading file %s; assuming FALSE\n", inval, filename);
	    return 0;
    }
}

static int LoadServerFile(acf, someDomain)
struct AMSConfig *acf; char *someDomain;
{/* Load values from the AMS-Server file for the given domain.  If the given domain is running AMS delivery, it can set default values if the AMS-Server file doesn't have a given field. */
    char *SrvFile, *vp, *cp, *cp2;
    int xl, accum;
    char DirCell[1000];
    struct configurelist *srvHead;

    xl = strlen(someDomain) + strlen(CellCommonPrefix) + strlen(CellCommonSuffix) + strlen(CellCommonConfigDirSuffix) + strlen(CellConfigMessageServer) + 3;
    SrvFile = malloc(xl);
    if (SrvFile == NULL) {errno = ENOMEM; return -1;}
    strcpy(SrvFile, CellCommonPrefix);
    LCappend(SrvFile, someDomain);
    strcat(SrvFile, CellCommonSuffix);
    strcat(SrvFile, CellCommonConfigDirSuffix);
    strcat(SrvFile, "/");
    strcat(SrvFile, CellConfigMessageServer);
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	if (GetCellFromFileName(SrvFile, DirCell, sizeof(DirCell)) == 0
	    && ULstrcmp(DirCell, someDomain) != 0) {	/* mis-configured */
	    errno = EMLINK; return 1;
	}
    }
#endif /* AFS_ENV */
    errno = 0;
    srvHead = ReadConfigureFile(SrvFile);
    if (srvHead == NULL) {
	free(SrvFile);
	return -1;	/* Errno will have something for the caller. */
    }
    if (acf->AMSDel > 0) {
	accum = vld_WPValid;
    } else {
	accum = (vld_PasswdValid | vld_AliasesValid);
	vp = GetConfig(srvHead, "AMS_WPValidation", 1);
	if (vp != NULL) {
	    if (atoi(vp) > 0) accum |= (vld_WPValid);
	    else accum &= (~vld_WPValid);
	}
	vp = GetConfig(srvHead, "AMS_PasswdValidation", 1);
	if (vp != NULL) {
	    if (atoi(vp) > 0) accum |= (vld_PasswdValid);
	    else accum &= (~vld_PasswdValid);
	}
	vp = GetConfig(srvHead, "AMS_LocalDatabaseValidation", 1);
	if (vp != NULL) {
	    if (atoi(vp) > 0) accum |= (vld_LocalDBValid);
	    else accum &= (~vld_LocalDBValid);
	}
	vp = GetConfig(srvHead, "AMS_AliasesValidation", 1);
	if (vp != NULL) {
	    if (atoi(vp) > 0) accum |= (vld_AliasesValid);
	    else accum &= (~vld_AliasesValid);
	}
    }
    acf->ValidMask = accum;
    vp = GetConfig(srvHead, "AMS_NameSeparator", 1);
    if (vp != NULL) {
	acf->NameSep = atoi(vp);
    } else if (acf->AMSDel > 0) {
	acf->NameSep = -1;  /* If running AMS delivery, absence means we know for sure. */
    }
    vp = GetConfig(srvHead, "AMS_UsersHandleFormatting", 1);
    if (vp != NULL) {
	xl = readBool(vp, SrvFile);
	acf->ATKFmtOK = (xl != 0 ? 1 : -1);
    } else if (acf->AMSDel > 0) {
	acf->ATKFmtOK = 1;  /* If running AMS delivery, absence means we know for sure. */
    }
    vp = GetConfig(srvHead, "AMS_UUCPSupported", 1);
    if (vp != NULL) {
	xl = readBool(vp, SrvFile);
	acf->UUCPSupp = (xl != 0 ? 1 : -1);
    }
    if (acf->AMSDel > 0) {
	acf->UseridPlusOK = 1;	/* AMDS means we know */
    } else {
	vp = GetConfig(srvHead, "AMS_UseridPlusWorks", 1);
	if (vp != NULL) {
	    xl = readBool(vp, SrvFile);
	    acf->UseridPlusOK = (xl != 0 ? 1 : -1);
	} else acf->UseridPlusOK = -1;	/* if we're reading the server file, we know. */
    }
    vp = GetConfig(srvHead, "MailboxName", 1);
    if (vp != NULL) {
	acf->MBoxName = NewString(vp);
    } else if (acf->AMSDel > 0) {
	acf->MBoxName = "Mailbox";  /* Again, AMS delivery means we know. */
    }
    vp = GetConfig(srvHead, "PostmasterName", 1);
    if (vp != NULL) {
	acf->PManName = NewString(vp);
    } else if (acf->AMSDel > 0) {
	acf->PManName = "postman";  /* Again, AMS delivery means we know. */
    }
    vp = GetConfig(srvHead, "WPIUpdateReqAddr", 1);
    if (vp != NULL) {
	acf->WPIAddr = NewString(vp);
    }
    vp = GetConfig(srvHead, "DefaultMSPath", 1);
    if (vp == NULL) {
	acf->dflt_MSElts = (struct cell_msPath *) -1;	/* authoritative no-such-entries, if AMSdeliv set */
	acf->numdflt_MSElts = 0;
    } else {
	xl = 1;
	cp = vp;
	for (;;) {  /* Estimate how many slots we'll need. */
	    cp2 = index(cp, ':');
	    if (cp2 == NULL) break;
	    ++xl;
	    cp = cp2+1;
	}
	errno = 0;
	acf->dflt_MSElts = (struct cell_msPath *) malloc(xl * sizeof(struct cell_msPath));
	if (acf->dflt_MSElts == NULL) {	/* Now fill the slots. */
	    errno = ENOMEM;
	} else {
	    cp = vp;
	    xl = 0;
	    for (;;) {
		acf->dflt_MSElts[xl].Abbrev = NULL;
		acf->dflt_MSElts[xl].RootDir = NULL;
		acf->dflt_MSElts[xl].Decorate = NULL;	/* not yet set */
		acf->dflt_MSElts[xl].RootCell = NULL;
		acf->dflt_MSElts[xl].Validated = acf->dflt_MSElts[xl].CkMailbox = 0;
		while (*cp == ':') ++cp;
		cp2 = index(cp, ':');
		strncpy(DirCell, cp, sizeof(DirCell));
		if (cp2 != NULL) {
		    if (((cp2 - cp) + 12) > sizeof(DirCell)) break;
		    DirCell[cp2-cp] = '\0';
		} else {
		    if ((strlen(cp) + 12) > sizeof(DirCell)) break;
		}
		if (DirCell[0] == '$') {
		    if (ULstrcmp(DirCell, "$mail") == 0) {if (cp2 == NULL) break; cp = cp2+1; continue;}
		    acf->dflt_MSElts[xl].Abbrev = NewString(&DirCell[1]);
		    strcat(DirCell, "BboardRoot");
		    cp = GetConfig(srvHead, &DirCell[1], 1);	/* skip the ``$'' */
		    if (cp == NULL) {
			acf->dflt_MSElts[xl].RootDir = NULL;  /* aggh.  Abandon ship. */
			errno = EDESTADDRREQ;
		    } else {
			acf->dflt_MSElts[xl].RootDir = NewString(cp);
		    }
		} else {
		    acf->dflt_MSElts[xl].RootDir = NewString(DirCell);
		}
		if (acf->dflt_MSElts[xl].RootDir == NULL) {
		    if (errno == 0) errno = ENOMEM;
		    for (;xl >= 0; --xl) {
			if (acf->dflt_MSElts[xl].RootDir != NULL) free(acf->dflt_MSElts[xl].RootDir);
			if (acf->dflt_MSElts[xl].Abbrev != NULL) free(acf->dflt_MSElts[xl].Abbrev);
		    }
		    free(acf->dflt_MSElts); acf->dflt_MSElts = NULL;	/* flag temp fail */
		    xl = 0; break;
		}
		++xl;
		if (cp2 == NULL) break;
		cp = cp2+1;
	    }
	    acf->numdflt_MSElts = xl;
	}
    }
    FreeConfigureList(srvHead);
    free(SrvFile);
    return 0;
}
#endif /* AMS_DELIVERY_ENV */

int CheckAMSNameSep(someDomain)
char *someDomain;
{/* Test whether the given domain uses a name-separator characters.  Return -1 if no, 0 if you can't tell, and >0 if it does.  If the value is >0, it's the separator character itself. */
#ifdef AMS_DELIVERY_ENV
    char *NameSepFile;
    int fc, xl, alloced; unsigned char TheChar;
    register struct AMSConfig *acf;
#ifdef AFS_ENV
    char DirCell[200];
#endif /* AFS_ENV */
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return AMS_NameSeparator;
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return 0;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->NameSep != 0) return acf->NameSep;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return 0;
    if (acf->NameSep != 0) return acf->NameSep;
    /* It wasn't named in the AMS-Server file.  Check the NameSep file. */
    xl = strlen(someDomain) + strlen(CellCommonPrefix) + strlen(CellCommonSuffix) + strlen(CellCommonConfigDirSuffix) + strlen(CellConfigNameSeparator) + 3;
    NameSepFile = malloc(xl);
    if (NameSepFile == NULL) {errno = ENOMEM; return 0;}
    strcpy(NameSepFile, CellCommonPrefix);
    LCappend(NameSepFile, someDomain);
    strcat(NameSepFile, CellCommonSuffix);
    strcat(NameSepFile, CellCommonConfigDirSuffix);
    strcat(NameSepFile, "/");
    strcat(NameSepFile, CellConfigNameSeparator);
    fc = open(NameSepFile, O_RDONLY, 0666);
#ifdef AFS_ENV
    if (AMS_ViceIsRunning && fc >= 0) {
	if (GetCellFromFileName(NameSepFile, DirCell, sizeof(DirCell)) == 0 && ULstrcmp(DirCell, someDomain) != 0) {	/* mis-configured */
	    close(fc); fc = -1; errno = EMLINK;
	}
    }
#endif /* AFS_ENV */
    free(NameSepFile);
    if (fc < 0) {	/* failure; how bad? */
	xl = (tfail(errno) ? 0 : -1);	/* either don't know or no sep. */
    } else {
	xl = read(fc, &TheChar, 1);
	if (xl <= 0) {
	    xl = 0;	/* temp fail on any error from read() */
	} else if (xl > 0 && isascii(TheChar) && !isalnum(TheChar)
		   && (isprint(TheChar) || TheChar == ' ')) {
	    xl = TheChar;
	} else {
	    xl = '.';	/* the default value */
	}
	close(fc);
    }
    acf->NameSep = xl;
    return xl;
#else /* AMS_DELIVERY_ENV */
    return 0;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

int CheckAMSValidationMask(someDomain)
char *someDomain;
{/* Return how the given domain validates its local user names, giving a mask with a bit on for each method that is used.  Return a negative number if it can't tell. */
#ifdef AMS_DELIVERY_ENV
    int alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */
    int fc;

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) {
	fc = 0;
	if (AMS_WPValidation > 0) fc |= vld_WPValid;
	if (AMS_PasswdValidation > 0) fc |= vld_PasswdValid;
	if (AMS_LocalDatabaseValidation > 0) fc |= vld_LocalDBValid;
	if (AMS_AliasesValidation > 0) fc |= vld_AliasesValid;
	return fc;
    }
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->ValidMask >= 0) return acf->ValidMask;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return -1;	/* some problem reading the AMS-Server file */
    return acf->ValidMask;
#else /* AMS_DELIVERY_ENV */
    return -1;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

char *CheckAMSMBName(someDomain)
char *someDomain;
{/* Determine what the given domain uses as the name of the mailbox directory for users.  Return NULL if it can't tell, or the directory name (e.g. "Mailbox") if it can. */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return MailboxName;
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->MBoxName != NULL) return acf->MBoxName;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return NULL;	/* some problem reading the AMS-Server file */
    return acf->MBoxName;
#else /* AMS_DELIVERY_ENV */
    return NULL;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

char *CheckAMSPMName(someDomain)
char *someDomain;
{/* Determine what the given domain uses as the username of the distinguished delivery agent.  Return NULL if it can't tell, or the username (e.g. "postman") if it can. */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return PostmasterName;
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->PManName != NULL) return acf->PManName;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return NULL;	/* some problem reading the AMS-Server file */
    return acf->PManName;
#else /* AMS_DELIVERY_ENV */
    return NULL;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

char *CheckAMSWPIAddr(someDomain)
char *someDomain;
{/* Determine what the given domain uses as the submission address for WPI update requests.  Return NULL if it can't tell, or the address (e.g. "wpi+@foobar.baz") if it can. */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return WPIUpdateReqAddr;
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->WPIAddr != NULL) return acf->WPIAddr;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return NULL;	/* some problem reading the AMS-Server file */
    return acf->WPIAddr;
#else /* AMS_DELIVERY_ENV */
    return NULL;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

int CheckAMSFmtOK(someDomain)
char *someDomain;
{/* Determine whether the given domain accepts ATK-formatted mail.  Return <0 if it doesn't, >0 if it does, and 0 if it can't tell.  (In general, though, a site's running AMS Delivery implies that it does.) */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return (AMS_UsersHandleFormatting ? 1 : -1);
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->ATKFmtOK != 0) return acf->ATKFmtOK;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return 0;	/* some problem reading the AMS-Server file */
    return acf->ATKFmtOK;
#else /* AMS_DELIVERY_ENV */
    return 0;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

int CheckAMSUUCPSupp(someDomain)
char *someDomain;
{/* Determine whether the given domain thinks a!b is a remote address.  Return >0 if it does, <0 if it doesn't, and 0 if we can't tell. */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return (AMS_UUCPSupported ? 1 : -1);
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->UUCPSupp != 0) return acf->UUCPSupp;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return 0;	/* some problem reading the AMS-Server file */
    return acf->UUCPSupp;
#else /* AMS_DELIVERY_ENV */
    return 0;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

int CheckAMSUseridPlusWorks(someDomain)
char *someDomain;
{/* Determine whether the given domain supports a+ and a+b types of local addresses.  Return >0 if it does, <0 if it doesn't, and 0 if we can't tell. */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
    if (ULstrcmp(someDomain, ThisDomain) == 0) return (AMS_UseridPlusWorks ? 1 : -1);
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) return NULL;
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->UseridPlusOK != 0) return acf->UseridPlusOK;
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return 0;	/* some problem reading the AMS-Server file */
    return acf->UseridPlusOK;
#else /* AMS_DELIVERY_ENV */
    return 0;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

int CheckAMSDfMSPath(someDomain, valP)
char *someDomain;
struct cell_msPath **valP;
{/* Determine what the given domain uses as its default MS path.  Returns a value of 0 if nothing, <0 as an error code, or >0 with valP pointing at an array of (value) cell_msPath structures describing the site's default MS path. */
#ifdef AMS_DELIVERY_ENV
    int fc, alloced;
    register struct AMSConfig *acf;
#endif /* AMS_DELIVERY_ENV */

    CheckAMSConfiguration();
#ifdef AMS_DELIVERY_ENV
    CheckAMSDelivery(someDomain);   /* let it do some of the heavy work */
    acf = getThis(someDomain, &alloced);
    if (acf == NULL) {errno = ENOMEM; return -1;}
    if (alloced) {
	acf->Next = AMSConfigRoot;	/* now link onto list */
	AMSConfigRoot = acf;
    }
    if (acf->numdflt_MSElts >= 0 && acf->dflt_MSElts != NULL) {
	*valP = acf->dflt_MSElts;
	return acf->numdflt_MSElts;
    }
    fc = LoadServerFile(acf, someDomain);
    if (fc != 0) return -1;	/* some problem reading the AMS-Server file */
    *valP = acf->dflt_MSElts;
    return acf->numdflt_MSElts;
#else /* AMS_DELIVERY_ENV */
    errno = EDOM;
    return -1;	/* Well, we just don't know. */
#endif /* AMS_DELIVERY_ENV */
}

#ifdef TESTINGONLYTESTING
main () {
    int RC, ix, wasAMS; char *retV;
    char TrialDomain[200];
    struct cell_msPath *Path;

    CheckServiceConfiguration();
    for (;;) {
	printf("Cell to try: "); fflush(stdout);
	if (gets(TrialDomain) == NULL) exit(0);
	wasAMS = RC = CheckAMSDelivery(TrialDomain);
	printf("CheckAMSDelivery(``%s'') returns %d.\n", TrialDomain, RC);
	RC = CheckAMSNameSep(TrialDomain);
	printf("CheckAMSNameSep(``%s'') returns %d.\n", TrialDomain, RC);
	retV = CheckAMSMBName(TrialDomain);
	printf("CheckAMSMBName(``%s'') returns ``%s''.\n", TrialDomain, (retV == NULL ? "NULL" : retV));
	retV = CheckAMSPMName(TrialDomain);
	printf("CheckAMSPMName(``%s'') returns ``%s''.\n", TrialDomain, (retV == NULL ? "NULL" : retV));
	RC = CheckAMSFmtOK(TrialDomain);
	printf("CheckAMSFmtOK(``%s'') returns ``%d''.\n", TrialDomain, RC);
	RC = CheckAMSUUCPSupp(TrialDomain);
	printf("CheckAMSUUCPSupp(``%s'') returns ``%d''.\n", TrialDomain, RC);
	RC = CheckAMSUseridPlusWorks(TrialDomain);
	printf("CheckAMSUseridPlusWorks(``%s'') returns ``%d''.\n", TrialDomain, RC);
	RC = CheckAMSValidationMask(TrialDomain);
	printf("CheckAMSValidationMask(``%s'') ", TrialDomain);
	if (RC < 0) printf("fails: %d/%d/%s\n", RC, errno, UnixError(errno));
	else printf("returns ``%#o''.\n", RC);
	retV = CheckAMSWPIAddr(TrialDomain);
	printf("CheckAMSWPIAddr(``%s'') returns ``%s''.\n", TrialDomain, (retV == NULL ? "NULL" : retV));
	errno = 0;
	RC = CheckAMSDfMSPath(TrialDomain, &Path);
	if (RC < 0) {
	    printf("CheckAMSDfMSPath(``%s'') returns error %d/%s.\n", TrialDomain, RC, UnixError(errno));
	} else {
	    printf("CheckAMSDfMSPath(``%s'') returns %d elements:\n", TrialDomain, RC);
	    if (Path == (struct cell_msPath *) -1) {
		printf("  --authoritative no-default-MS-path: %s", UnixError(errno));
		if (wasAMS <= 0) printf(" (but cell wasn't running AMS delivery)");
	    } else for (ix = 0; ix < RC; ++ix) {
		printf("    ");
		if (Path[ix].Decorate != NULL || Path[ix].CkMailbox) {
		    printf("[%s%s]",
			   (Path[ix].CkMailbox ? "*" : ""),
			   (Path[ix].Decorate != NULL ? Path[ix].Decorate : ""));
		}
		printf("$%s: %s", Path[ix].Abbrev, Path[ix].RootDir);
		if (Path[ix].RootCell != NULL) printf(" (%s)", Path[ix].RootCell);
	    }
	    printf("\n");
	}
    }
}
#endif /* TESTINGONLYTESTING */

