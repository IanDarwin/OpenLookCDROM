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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/mswp.c,v 2.62 1994/01/16 01:11:22 rr2b Exp $";
#endif


 
#include <andrewos.h> /* sys/file.h */
#include <andyenv.h>
#include <util.h>
#include <ms.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#include <parseadd.h>
#include <mail.h>
#include <mailconf.h>
#include <pwd.h>

#ifdef WHITEPAGES_ENV
#include <wp.h>
#include <bt.h>
#endif /* WHITEPAGES_ENV */

extern char *StripWhiteEnds();

extern int NeedToTimeOut;
extern char home[], Me[], MyMailDomain[];
extern ADDRESS_HOST *MakeHost();
extern PARSED_ADDRESS *SingleAddress();

#define EXTBBPROTFILE "../extenable/ext.enable"
#define EXTBBENABLEFILE ".TurnOnExternalPosting"

#define MagicMapFileName ".MS.DirectPost"

static int PathEltOnLocalDfltMSPath(FileName)
char *FileName;
{/* Return TRUE iff FileName is one of the site's default MS path elements. */
    if (strcmp(FileName, LOCALSEARCHPATHTEMPLATE) == 0
    ||  strcmp(FileName, EXTERNALSEARCHPATHTEMPLATE) == 0
    ||  strcmp(FileName, OFFICIALSEARCHPATHTEMPLATE) == 0) {
	return (TRUE);
    } else {
	return (FALSE);
    }
}

int GenMSPathElts(genPos, cellName, pathName, outCell, SPEix)
int *genPos, *SPEix;
char *cellName, **pathName, **outCell;
{/* Generate MSPath elements from the current user's list plus, if cellName is non-null and not MyMailDomain, the default mspath for that cell.
    Initialize genPos to 0 to initialize the generation.
   Return the path name for the element's root, whether it is on the default mspath, and the cell that it's in (where a 0 pointer means the cell was indeterminate and a zero-length string is a root on the local disk), via outCell.
	 Return 1 if anything was generated, or 0 if the end of elements was reached.
	     */
    char cellBuf[100];
    int code, c2, Ix;
#ifdef NOTDEF
    int Found;
#endif /* NOTDEF */
    struct cell_msPath *CP;

    *pathName = *outCell = NULL;
    *SPEix = -1;
    if (*genPos < MS_NumDirsInSearchPath) {
	*pathName = SearchPathElements[*genPos].Path;
	if (SearchPathElements[*genPos].Cell == NULL) {
	    code = GetCellFromFileName(SearchPathElements[*genPos].Path, cellBuf, sizeof(cellBuf));
	    SearchPathElements[*genPos].Cell = (code == 0 ? NewString(cellBuf) : (code == EINVAL ? "" : NULL));
	}
	*outCell = SearchPathElements[*genPos].Cell;
	*SPEix = *genPos;
	++*genPos;
	return(1);
    }
    if (cellName != NULL && *cellName != '\0' && ULstrcmp(cellName, MyMailDomain) != 0) {
	/* Advance into the remote cell's path space now. */
	c2 = CheckAMSDfMSPath(cellName, &CP);
	if (c2 > 0) while (*genPos < (c2 + MS_NumDirsInSearchPath)) {
	    Ix = *genPos - MS_NumDirsInSearchPath;  /* Translate to a CP index */
#ifdef NOTDEF/* code in FindInMSSearchPath may need to see a cross-cell root more than once. */
	    /* Eliminate duplicates with the paths we already returned. */
	    Found = 0;
	    for (code = 0; code < MS_NumDirsInSearchPath; ++code) {
		if (strcmp(CP[Ix].RootDir, SearchPathElements[code].Path) == 0) {
		    Found = 1; break;
		}
	    }
	    if (Found) {++*genPos; continue;}
#endif /* NOTDEF */
	    if (CP[Ix].RootCell == NULL) {
		code = GetCellFromFileName(CP[Ix].RootDir, cellBuf, sizeof(cellBuf));
		CP[Ix].RootCell = (code == 0 ? NewString(cellBuf) : (code == EINVAL ? "" : NULL));
	    }
	    *pathName = CP[Ix].RootDir;
	    *outCell = CP[Ix].RootCell;
	    ++*genPos;
	    return (1);
	}
    }
    return(0);
}

static int FindInMSSearchPath(name, FullName, CurrDomain, IsCertain)
char *name, *FullName, *CurrDomain;
int *IsCertain;
{
    int i, save_errno, save_mserr, Auth, RandomError, GenNum, AnyFound, Printed, CrossCell;
    char ErrText[100+MAXPATHLEN];
    char *PathName, *outCell;
    char CellName[100]; int CellRes;
    struct stat statbuf;

    *IsCertain = TRUE;
    AnyFound = FALSE;
    CrossCell = FALSE;
    if (CurrDomain != NULL && CurrDomain[0] != '\0' && ULstrcmp(CurrDomain, MyMailDomain) != 0) CrossCell = TRUE;
    GenNum = 0;
    while (GenMSPathElts(&GenNum, CurrDomain, &PathName, &outCell, &i)) {
	if (i >= 0 && !SearchPathElements[i].HasValidated) {
	    if (ValidateSearchPath(i)) return (mserrcode);
	}
	if (PathName) {
	    sprintf(FullName, "%s/%s", PathName, name);
	} else {
	    strcpy(FullName, name);
	}
	debug(16,("Trying %s\n", FullName));
/* Two cases.  If the user isn't asking for a cross-cell identification, cell considerations are irrelevant; the local mspath is what's used.  If the user is asking for a cross-cell identification, lots of things could make the user feel that a bboard is in a named cell: the root could be in that cell's default mspath, the root could be a directory in that cell, or the bboard itself could be a directory in that cell.  We respect all of these.  Thus, the only kind of bboard we reject for being in the wrong cell is that in which we're asking for a foreign cell but we are looking at a bboard found in the default local mspath with root and bboard-dir all not in the asked-for cell. */
	errno = 0;
	CellName[0] = '\0';
	CellRes = GetCellFromFileName(FullName, CellName, sizeof(CellName));
	if (CellRes == 0 || CellRes == EINVAL) {
	    if (CrossCell && i >= 0 && outCell != NULL && outCell != ((char *) -1) && ULstrcmp(CurrDomain, (*outCell != '\0' ? outCell : WorkstationName)) != 0 && ULstrcmp(CurrDomain, (CellName[0] != '\0' ? CellName : WorkstationName)) != 0) {
		debug(16,("Cross-cell, looking in cell %s; %s isn't in that cell nohow.\n", CurrDomain, FullName));
		continue;
	    }
	}
	if (stat(FullName, &statbuf) == 0) {
	    AnyFound = TRUE;
	    break;
	}
	save_errno = errno;
	if (vdown(save_errno)) {
	    *IsCertain = FALSE;
	} else {
	    if (save_errno != ENOENT) {
		ErrText[0] = '\0'; RandomError = Printed = FALSE;
		mserrcode = MS_CheckAuthentication(&Auth);
		save_mserr = mserrcode;
		if (i >= 0) {
		    SearchPathElements[i].HasValidated = 0;	/* force re-validation */
		    if (ValidateSearchPath(i)) return (mserrcode);
		    if (!SearchPathElements[i].HasValidated) Printed = TRUE;
		} else {
		    if (!OKRoot(PathName)) Printed = TRUE;
		}
		/* Now if HasValidated is still zero, we just printed an error message. */
		mserrcode = save_mserr;
		if (vdown(AMS_ERRNO)) {
		    sprintf(ErrText, "AFS/network error checking %s; continuing...", ap_Shorten(FullName));
		    RandomError = TRUE;
		    *IsCertain = FALSE;
		} else if ((save_errno == EACCES || AMS_ERRNO == EACCES) && !Auth) {
		    sprintf(ErrText, "You are apparently unauthenticated and cannot access directory %s.", ap_Shorten(FullName));
		    RandomError = TRUE;
		    *IsCertain = FALSE;
		}
		if (access(PathName, R_OK) != 0) {
		    if (vdown(errno) || errno == EACCES) {
			RandomError = TRUE;
			if (errno != EACCES || !Auth) *IsCertain = FALSE;
		    }
		}
		if (RandomError) {
		    /* If we've composed a message, print it if ValidateSearchPath hasn't just printed one about the path element.  If Vice has been down or this isn't an official MSPATH element, just go on to the next mspath element.  Otherwise, it's really a permission-denied error. */
		    if (ErrText[0] != '\0' && !Printed) NonfatalBizarreError(ErrText);
		    if (!*IsCertain) continue;
		    if (i >= 0 && PathEltOnLocalDfltMSPath(PathName) == 0 && strncmp(PathName, home, strlen(home))) continue;
		}
		AMS_RETURN_ERRCODE(save_errno, EIN_ACCESS, EVIA_DISAMB);
	    }
	}
    }
    if (!AnyFound) {
	AMS_RETURN_ERRCODE(*IsCertain ? ENOENT : ETIMEDOUT, EIN_PATHSEARCH, EVIA_DISAMB);
    }
    return(0);
}

static int
CheckFolderAddress(recip, rcode, IsCertain, FullName, orgname, WouldCreate, pCellN)
char **recip; /* Will swing the pointer if we rewrite */
int *rcode;
int *IsCertain;
char *FullName, *orgname;
char **pCellN;	/* Will set if the address came from a file with an identifiable cell. */
Boolean WouldCreate;
{
    static char RealRecip[1000], FileCell[200];
    char *s, recipuser[200];
    Boolean TopLevel, IsExternal;
    int PostOK, ScanOK, CreateOK;
    struct stat statbuf;

    debug(16, ("Checking external postability %s %s\n", FullName, EXTERNALSEARCHPATHTEMPLATE));
    if (strncmp(FullName, EXTERNALSEARCHPATHTEMPLATE, strlen(EXTERNALSEARCHPATHTEMPLATE))) {
	IsExternal = FALSE;
    } else {
	Boolean Prevent = FALSE;
	char Fnam[1+MAXPATHLEN];

	IsExternal = TRUE;
	sprintf(Fnam, "%s/%s", EXTERNALSEARCHPATHTEMPLATE, EXTBBPROTFILE);
	debug(16, ("Checking for access to file %s\n", Fnam));
	if (!AMS_UsersAreGrownups) {
	    if (stat(Fnam, &statbuf)) {
		Prevent = TRUE;
		debug(16, ("Nope, inhibit posting\n"));
	    } else {
		sprintf(Fnam, "%s/%s", home, EXTBBENABLEFILE);
		debug(16, ("Checking for access to file %s\n", Fnam));
		if (stat(Fnam, &statbuf)) {
		    Prevent = TRUE;
		    debug(16, ("Nope, inhibit posting\n"));
		}
	    }
	}
	if (Prevent) {
	    strcpy(RealRecip, orgname);
	    *recip = RealRecip;
	    if (vdown(errno)) {
		*rcode = MSWP_TEMPFAIL;
		*IsCertain = FALSE;
	    } else {
		*rcode = MSWP_NOEXTPOSTING;
	    }
	    return(0);
	}
    }
	    
    strcpy(recipuser, FullName);
    strcat(recipuser, "/");
    TopLevel = TRUE;
    CreateOK = -1;  /* ignore the scan/noscan distinction for this */
    for (;;) {
	FILE *mfp;
	char *subst, SecondBuf[1000], MapName[1000], *cp;
	int MayCreate, LastPass;

	s = strrchr(recipuser, '/');
	if (!s) {
	    *rcode = MSWP_CRAP;
	    return(0);
	}
	if (!strncmp(s+1, MS_TREEROOT, sizeof(MS_TREEROOT) -1)) {
	    LastPass = TRUE;
	} else {
	    LastPass = FALSE;
	}
	strcpy(s+1, MagicMapFileName);	
	mfp = fopen(recipuser, "r");
	if (mfp == NULL) {
	    if (vdown(errno)) {
		*IsCertain = FALSE;
		*rcode = MSWP_TEMPFAIL;
		return(0);
	    }
	    if (errno == EACCES) {
		strcpy(RealRecip, orgname);
		*recip = RealRecip;
		*rcode = MSWP_BADMSDIR;
		return(0);
	    }
	    if (errno != ENOENT) {
		AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_REWRITEADDRESS);
	    }
	} else {
	    /* Found magic map file!! Lets read it and be done!!! */
	    if (fgets(RealRecip, sizeof(RealRecip), mfp) == NULL) {
		fclose(mfp);
		AMS_RETURN_ERRCODE(errno, EIN_FGETS, EVIA_REWRITEADDRESS);
	    }
	    PostOK = ScanOK = -1;	/* all OK initially; flagged as not set. */
	    cp = RealRecip;	    /* Scan the first line, keeping backward compatibility. */
	    while (*cp && isspace(*cp)) ++cp;
	    MayCreate = atoi(cp);
	    while (*cp && (isdigit(*cp) || *cp == '-')) ++cp;
	    while (*cp && isspace(*cp)) ++cp;
	    while (*cp && isalpha(*cp)) {
		if (ULstlmatch(cp, "scan") == 1) ScanOK = 1;
		else if (ULstlmatch(cp, "noscan") == 1) ScanOK = 0;
		else if (ULstlmatch(cp, "post") == 1) PostOK = 1;
		else if (ULstlmatch(cp, "nopost") == 1) PostOK = 0;
		else if (ULstlmatch(cp, "createsub") == 1) {if (CreateOK < 0) CreateOK = 1;}
		else if (ULstlmatch(cp, "nocreatesub") == 1) {if (CreateOK < 0) CreateOK = 0;}
		else if (ULstlmatch(cp, "CC") != 1) {	    /* leave room for expansion */
		    char errTxt[MAXPATHLEN+1000];
		    sprintf(errTxt, "Unrecognized flag text ``%s'' in file %s", cp, ap_Shorten(recipuser));
		    NonfatalBizarreError(errTxt);
		}
		while (*cp && isalpha(*cp)) ++cp;
		while (*cp && isspace(*cp)) ++cp;
	    }
	    if (CreateOK < 0) CreateOK = MayCreate;
	    if (CreateOK == 0 && WouldCreate) {
		strcpy(RealRecip, orgname);
		*recip = RealRecip;
		*rcode = MSWP_CRAP;
		fclose(mfp);
		return(0);
	    }
	    if (fgets(RealRecip, sizeof(RealRecip), mfp) == NULL) {
		if (PostOK == 0) {
		    RealRecip[0] = '\0';
		} else {
		    fclose(mfp);
		    AMS_RETURN_ERRCODE(EMSUNKNOWN, EIN_FGETS, EVIA_REWRITEADDRESS);
		}
	    }
	    subst = FullName + (s - recipuser) + 1;
	    strcpy(MapName, subst);
	    while (subst = strchr(MapName, '/')) {
		*subst = '.';
	    }
	    subst = strchr(RealRecip, '*');
	    if (ScanOK < 0 && subst == NULL) ScanOK = 0;    /* Default its value: for parent directories, the DP file makes sense only if the address has an asterisk. */
	    if (ScanOK == 0 && !TopLevel) {
		fclose(mfp);		/* Ignore this one and look for a higher-level DP file. */
	    } else {
		if (PostOK == 0) {
		    fclose(mfp);
		    strcpy(RealRecip, orgname);
		    *recip = RealRecip;
		    *rcode = MSWP_BADMSDIR;	/* No posting here. */
		    return(0);
		}
		/* If we're about to use the file contents, then clobber the pCellN pointer. */
		if (GetCellFromFileName(recipuser, FileCell, sizeof(FileCell)) == 0) *pCellN = FileCell;
		fclose(mfp); /* reading only; no need to check the result */
		while (subst) {
		    if (subst == RealRecip || *(subst-1) != '\134') {
			/* splice the sucker in here */
			strcpy(SecondBuf, RealRecip);
			SecondBuf[subst - RealRecip] = '\0';
			if (TopLevel) {
			    char *sdum;

			    sdum = &SecondBuf[subst-RealRecip -1];
			    if (*sdum == '.' || *sdum == '/') *sdum = '\0';
			} else {
			    strcat(SecondBuf, MapName);
			}
			strcat(SecondBuf, subst+1);
			subst += strlen(MapName);
			strcpy(RealRecip, SecondBuf);
		    } else {
			++subst;
		    }
		    subst = strchr(subst, '*');
		}
		ReduceWhiteSpace(RealRecip);
		*recip = RealRecip;
		if (WouldCreate) {
		    *rcode = MSWP_CREATABLEMSDIR;
		} else {
		    if (*IsCertain) {
			*rcode = IsExternal ? MSWP_GOODEXTMSDIR : MSWP_GOODMSDIR;
		    } else {
			*rcode = MSWP_PROBABLYGOOD;
		    }
		}
		return(0);
	    }
	}
	if (LastPass) {
	    strcpy(RealRecip, orgname);
	    *recip = RealRecip;
	    *rcode = WouldCreate ? MSWP_CRAP : MSWP_BADMSDIR;
	    return(0);
	}
	TopLevel = FALSE;
	*s = '\0';
    }
}

static int
IsRecipientAnMSDirectory(recip, rcode, IsCertain, CurrDomain, pCellN)
char **recip; /* Will swing the pointer if we rewrite */
int *rcode;
int *IsCertain;
char *CurrDomain, **pCellN;
{
    char FullName[1+MAXPATHLEN], bbname[1+MAXPATHLEN], orgname[1+MAXPATHLEN+1], *s;
    Boolean WouldCreate = FALSE;

    strncpy(bbname, *recip, sizeof(bbname));
    s = strchr(bbname, '@');
    if (s) *s = '\0';
    strcpy(orgname, bbname);
    for (s = bbname + 1; *s; ++s) {
	if (*s == '.' && *(s - 1) != '/') {
	    *s = '/';		/* Convert cmu.general to cmu/general, etc. */
	}
    }
    if (FindInMSSearchPath(bbname, FullName, CurrDomain, IsCertain)) {
	if (vdown(AMS_ERRNO)) {
	    *IsCertain = FALSE;
	    *rcode = MSWP_TEMPFAIL;
	    return(0);
	} else if (AMS_ERRNO != ENOENT) {
	    return(mserrcode);
	}
	s = strrchr(bbname, '/');
	if (s) {
	    *s = '\0';
	    if (FindInMSSearchPath(bbname, FullName, CurrDomain, IsCertain)) {
		*s = '/';
		if (vdown(AMS_ERRNO)) {
		    *IsCertain = FALSE;
		    *rcode = MSWP_TEMPFAIL;
		    return(0);
		}
		if (AMS_ERRNO == EACCES) {
		    AMS_RETURN_ERRCODE(EMSUNAUTH, AMS_ERRCAUSE, AMS_ERRVIA);
		}
		if (AMS_ERRNO != ENOENT) {
		    return(mserrcode);
		}
		*rcode = MSWP_CRAP;
		return(0);
	    }
	    *s = '/';
	    strcat(FullName, s);
	    WouldCreate = TRUE;
	} else {
	    *rcode = *IsCertain ? MSWP_CRAP : MSWP_TEMPFAIL;
	    return(0);
	}
    }
    return(CheckFolderAddress(recip, rcode, IsCertain, FullName, orgname, WouldCreate, pCellN));
}

static int GetPostableStatus(pathname, bbname, status, IsCertain, recip, pcellN)
char *pathname, *bbname, **recip, **pcellN;
int *status, *IsCertain;
{
    char FileName[1+MAXPATHLEN], *s, *suffix;
    int Creatable = FALSE, rcode;

    sprintf(FileName, "%s/%s", pathname, bbname);
    s = &FileName[strlen(pathname)];
    while (*s) {
	if (*s == '.') *s = '/';
	++s;
    }
    if (access(FileName, R_OK)) {
	if (vdown(errno)) {
	    *IsCertain = FALSE;
	    *status = MSWP_TEMPFAIL;
	    return(0);
	}
	*status = MSWP_INVALIDMSGDIR;
	suffix = strrchr(FileName, '/');
	if (suffix) {
	    *suffix = '\0';
	    if (strlen(FileName) > strlen(pathname) && !access(FileName, R_OK)) {
		Creatable = TRUE;
	    }
	    *suffix = '/';
	}
	if (!Creatable) return(0);
    }
    rcode = CheckFolderAddress(recip, status, IsCertain, FileName, bbname, Creatable, pcellN);
    return(rcode);
}


#define WPTOLERANCE 600 /* ten minutes */

#ifdef WHITEPAGES_ENV

static enum WpState {wpopen_NotOpen, wpopen_CannotOpen, wpopen_Open}
		WpOpen = wpopen_NotOpen;
static char WpCell[150] = "\0";
static struct wp_cd *wpCD = NULL;

static wp_FieldIndex idxN = -1, idxID, idxAf, idxCAF;


static wp_ErrorCode OpenWhitePages(cellName)
char *cellName;
{/* Open the white pages and get the indices we'll need. */
	wp_ErrorCode wpErr;

	if (WpOpen == wpopen_Open && ULstrcmp(WpCell, cellName) == 0)
		return wperr_NoError;
	if (WpOpen == wpopen_Open) {
		cwp_Terminate(wpCD);
		wpCD = NULL;
		WpCell[0] = '\0';
		WpOpen = wpopen_NotOpen;
	}

	wpErr = wp_InitializeCell(cellName, &wpCD);
	if (wpErr != wperr_NoError) {
		WpOpen = wpopen_CannotOpen;
		return (wpErr);
	}
	if (idxN < 0) {
		idxN = wp_FieldNameToIndex("N");
		idxID = wp_FieldNameToIndex("ID");
		idxAf = wp_FieldNameToIndex("Af");
		idxCAF = wp_FieldNameToIndex("CAF");
	}
	if (idxN < 0 || idxID < 0 || idxAf < 0) {
		cwp_Terminate(wpCD);
		wpCD = NULL;
		WpOpen = wpopen_CannotOpen;
		WpCell[0] = '\0';
		return (wperr_NoSuchTokenKind);
	} else {
		WpOpen = wpopen_Open;
		strcpy(WpCell, cellName);
		return (wperr_NoError);
	}
}
#endif /* WHITEPAGES_ENV */

#ifdef WHITEPAGES_ENV
void CloseWhitePages()
{
    if (WpOpen == wpopen_Open) {
	cwp_Terminate(wpCD);
	wpCD = NULL;
	WpOpen = wpopen_NotOpen;
	WpCell[0] = '\0';
    }
}
#endif /* WHITEPAGES_ENV */

static int UseWPOnly(mscode, wpcode, Vacuous)
int *mscode, *wpcode, Vacuous;
{
/* Some kinds of recipients lexically override */
    switch(*wpcode) {
	case MSWP_GOODNETMAIL:	/* non-local network mail */
	case MSWP_EXTFORCEFORMAT:
	case MSWP_EXTFORCESTRIP:
	case MSWP_EXTFORCETRUST:
	case MSWP_BADNETMAIL:
	case MSWP_UNKNOWNNETMAIL:
	case MSWP_DISTLIST:		/* the special delivery types */
	case MSWP_BADDISTLIST:
	case MSWP_PROTDISTLIST:
	case MSWP_DISTLISTDIR:
	case MSWP_DIRINSERT:
	case MSWP_BADDIRINSERT:
	case MSWP_PROTDIRINSERT:
	case MSWP_DIRINSERTFILE:
	case MSWP_FSMEMBERS:
	case MSWP_BADFSMEMBERS:
	case MSWP_UNKNOWNFSMEMBERS:
		return(1);
	default:
		break;
    }
/* If WP validates everything vacuously, give preference to MS codes */
    if (Vacuous && *mscode != MSWP_CRAP) return(0);
    switch(*mscode) {
	case MSWP_BADMSDIR:
	    if (*wpcode == MSWP_CRAP) return(0);
	    else return(1);
	case MSWP_CREATABLEMSDIR:
	    switch(*wpcode) {
		case MSWP_MATCHTOOFUZZY:
		case MSWP_CRAP:
		case MSWP_AMBIGUOUS:
		case MSWP_VERYAMBIGUOUS:
		case MSWP_PROBABLYAMBIGUOUS:
		case MSWP_PROBABLYGOOD:
		    return(0);
		case MSWP_GOODUSER:
		case MSWP_TEMPFAIL:
		default:
		    return(1);
	    }
	case MSWP_CRAP:
	    return(1);
	case MSWP_TEMPFAIL:
	    switch (*wpcode) {
		case MSWP_GOODUSER:
		case MSWP_MATCHTOOFUZZY:
		case MSWP_PROBABLYGOOD:
		    *wpcode = MSWP_PROBABLYGOOD;
		    return(1);
		case MSWP_AMBIGUOUS:
		case MSWP_PROBABLYAMBIGUOUS:
		case MSWP_FUZZYAMBIGMATCH:
		    *wpcode = MSWP_PROBABLYAMBIGUOUS;
		    return(1);
		default:
		    *wpcode = MSWP_TEMPFAIL;
		    return(1);
	    }
	case MSWP_PROBABLYGOOD:
	case MSWP_GOODMSDIR:
	case MSWP_GOODEXTMSDIR:
	case MSWP_EXTFORCEFORMAT:
	case MSWP_EXTFORCESTRIP:
	case MSWP_EXTFORCETRUST:
	default: /* No other codes returned by IsRecipient... */
	    return(0);
    }
}


static int UseMSCodeOnly(mscode, wpcode, Vacuous)
int *mscode, *wpcode, Vacuous;
{
    if (*wpcode == MSWP_CRAP) {
	return(1);
    }
    if (Vacuous && (*mscode != MSWP_CRAP)) return(1);
    if ((*mscode == MSWP_GOODMSDIR) || (*mscode == MSWP_GOODEXTMSDIR) || (*mscode == MSWP_EXTFORCEFORMAT) || (*mscode == MSWP_EXTFORCESTRIP) || (*mscode == MSWP_EXTFORCETRUST)) {
	if ((*wpcode == MSWP_MATCHTOOFUZZY)
	    || (*wpcode == MSWP_FUZZYAMBIGMATCH)
	    || (*wpcode == MSWP_FUZZYTOOMANYMATCHES)
	    || (*wpcode == MSWP_PROBABLYFUZZYAMBIG)) {
	    return(1);
	}
    }
    return(0);
}

static int MergeMSWPCodes(mscode, wpcode)
{/* Return an MSDir code as modified by a WP code. */
    if (mscode == MSWP_GOODMSDIR || mscode == MSWP_GOODEXTMSDIR) {
	if (wpcode == MSWP_EXTFORCEFORMAT) return MSWP_EXTFORCEFORMATDIR;
	if (wpcode == MSWP_EXTFORCESTRIP) return MSWP_EXTFORCESTRIPDIR;
	if (wpcode == MSWP_EXTFORCETRUST) return MSWP_EXTFORCETRUSTDIR;
    }
    if (mscode == MSWP_GOODMSDIR) {
	if (wpcode == MSWP_GOODNETMAIL || wpcode == MSWP_BADNETMAIL || wpcode == MSWP_UNKNOWNNETMAIL) return MSWP_GOODEXTMSDIR;
    }
/*    if (mscode == MSWP_GOODEXTMSDIR) {
	if (wpcode == MSWP_GOODUSER || wpcode == MSWP_DISTLIST || wpcode == MSWP_UNKNOWNGOOD || wpcode == MSWP_DIRINSERT)
	        return MSWP_GOODMSDIR;
    }
*/
    return(mscode);
}

static struct alias {
	char *nick, *real;
	int nicklen;
	struct alias *next;
} *FirstAlias = NULL;

static struct ForceExtDescriptor {
    char *addrpattern;
    int ForceMail, ForceDir;
    struct ForceExtDescriptor *next;
} *ForceExtRoot = NULL;

static int RefreshAliasFile()
{
    static char *RawAliases = NULL;
    struct alias *tempalias;
    static int Checked = 0; /* 0 unchecked, 1 stat failed, 2 file there. */
    static unsigned long aliasreadtime = 0L;
    Boolean NeedToRead;
    char AliasFileName[MAXPATHLEN+1], *s, ErrorText[750];
    struct ForceExtDescriptor *fetmp;
    struct stat statbuf;
    int fd;

    NeedToRead = FALSE;
    sprintf(AliasFileName, "%s/.AMS_aliases", home);
    if (Checked == 0) {
	if (stat(AliasFileName, &statbuf)) {
	    Checked = 1;
	} else {
	    Checked = 2;
	    aliasreadtime = statbuf.st_mtime;
	    NeedToRead = TRUE;
	}
    }
    if (!NeedToRead) {
	if (stat(AliasFileName, &statbuf)) {
	    if (vdown(errno)) {
		NeedToRead = FALSE;	/* assume OK */
	    } else if (errno == ENOENT) {
		if (Checked == 2) {
		    Checked = 1;
		    aliasreadtime = 0L;
		    NonfatalBizarreError("Your personal alias file is suddenly unreadable!");
		    while (ForceExtRoot) {
			fetmp = ForceExtRoot->next;
			free(ForceExtRoot);
			ForceExtRoot = fetmp;
		    }
		    while (FirstAlias != NULL) {
			tempalias = FirstAlias->next;
			free(FirstAlias);
			FirstAlias = tempalias;
		    }
		}
		return(0); /* Might really have deleted it! */
	    } else {
		AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_CHECKPERSONALALIAS);
	    }
	} else if (Checked == 1 || statbuf.st_mtime > aliasreadtime) {
	    Checked = 2;
	    debug(16, ("Alias file has changed: %d > %d\n", statbuf.st_mtime, aliasreadtime));
	    NeedToRead = TRUE;
	}
    }
    if (NeedToRead) {
	fd = open(AliasFileName, O_RDONLY);
	if (fd<0) {
	    if (errno == ENOENT) return(0);
	    AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_CHECKPERSONALALIAS);
	}
	if (fstat(fd, &statbuf)) {
	    close(fd);
	    AMS_RETURN_ERRCODE(errno, EIN_FSTAT, EVIA_CHECKPERSONALALIAS);
	}
	while (ForceExtRoot) {
	    fetmp = ForceExtRoot->next;
	    free(ForceExtRoot);
	    ForceExtRoot = fetmp;
	}
	while (FirstAlias != NULL) {
	    tempalias = FirstAlias->next;
	    free(FirstAlias);
	    FirstAlias = tempalias;
	}
	if (RawAliases) {
	    free(RawAliases);
	}
	RawAliases = malloc(1+statbuf.st_size);
	if (!RawAliases) {
	    close(fd);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHECKPERSONALALIAS);
	}
	if (read(fd, RawAliases, statbuf.st_size) != statbuf.st_size) {
	    free(RawAliases);
	    RawAliases = NULL;
	    close(fd);
	    AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_CHECKPERSONALALIAS);
	}
	RawAliases[statbuf.st_size] = '\0';
	close(fd); /* read only, no error checking */
	s = RawAliases;
	/* Skip initial white space */
	while (*s == ' ' || *s == '\t' || *s == '\n') ++s;
	while (*s != '\0') {
	    char *firstpart;
	    int forcemail, forcedir;
	    struct ForceExtDescriptor *fetmp;

	    if (*s == '#' || *s=='\n') {	/* comment line */
		while (*s != '\0' && *s != '\n') ++s;
		if (*s != '\0') {
		    *s++ = '\0';
		    while (*s == ' ' || *s == '\t' || *s == '\n') ++s;
		}
		continue;
	    }
	    firstpart = s;
	    while (*s != '\0' && *s != ' ' && *s != '\t' && *s != '\n') {
		++s;
	    }
	    if (!strncmp(firstpart, "$force", 6)) {
		if (!strncmp(firstpart, "$forceformat", 12)) {
		    forcemail = MSWP_EXTFORCEFORMAT;
		    forcedir = MSWP_EXTFORCEFORMATDIR;

		} else if (!strncmp(firstpart, "$forcestrip", 11)) {
		    forcemail = MSWP_EXTFORCESTRIP;
		    forcedir = MSWP_EXTFORCESTRIPDIR;

		} else if (!strncmp(firstpart, "$forcetrust", 11)) {
		    forcemail = MSWP_EXTFORCETRUST;
		    forcedir = MSWP_EXTFORCETRUSTDIR;

		} else {
		    firstpart = ++s;
		    while (*s != '\0' && *s != '\n') ++s;
		    if (*s != '\0') *s++ = '\0';
		    sprintf(ErrorText, "Unparsable $force line '%0.700s': ignored", firstpart);
		    NonfatalBizarreError(ErrorText);
		    continue;
		}
		fetmp = (struct ForceExtDescriptor *) malloc(sizeof(struct ForceExtDescriptor));
		if (!fetmp) AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHECKPERSONALALIAS);
		firstpart = ++s;
		while (*s != '\0' && *s != '\n') ++s;
		if (*s != '\0') *s++ = '\0';
		fetmp->addrpattern = StripWhiteEnds(firstpart);
		fetmp->ForceMail = forcemail;
		fetmp->ForceDir = forcedir;
		fetmp->next = ForceExtRoot;
		ForceExtRoot = fetmp;
	    } else {
		tempalias = (struct alias *) malloc(sizeof (struct alias));
		if (!tempalias) {
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CHECKPERSONALALIAS);
		}
		tempalias->nick = firstpart;
		if (*s == '\0' || *s == '\n') {
		    if (*s != '\0') *s++ = '\0';
		    sprintf(ErrorText, "Unparsable alias line '%0.700s': ignored (length %d)", tempalias->nick, strlen(tempalias->nick));
		    NonfatalBizarreError(ErrorText);
		    free(tempalias);
		    continue;
		}
		*s++ = '\0';
		tempalias->real = s;
		while (*s != '\0') {
		    if (*s == '\n' && !isspace(*(s+1))) break;
		    ++s;
		}
		if (*s) {
		    *s++ = '\0';
		}
		tempalias->nicklen = strlen(tempalias->nick);
		LowerStringInPlace(tempalias->nick, tempalias->nicklen);
		tempalias->next = FirstAlias;
		FirstAlias = tempalias;
		while (*s == ' ' || *s == '\t' || *s == '\n') ++s;
	    }
	}
	aliasreadtime = statbuf.st_mtime;
    }
    return(0);
}

static int CheckPersonalAlias(name, Buf, bufsize, code)
char *name, *Buf;
int bufsize, *code;
{
    struct alias *tempalias;
    char *s;

    debug(1, ("CheckPersonalAlias %s\n", name));
    if (RefreshAliasFile()) return(mserrcode);
    for (tempalias = FirstAlias; tempalias; tempalias = tempalias->next) {
	if (!lc2strncmp(tempalias->nick, name, tempalias->nicklen) && (strlen(name) == tempalias->nicklen)) {
	    if (strlen(tempalias->real) >= bufsize) {
		AMS_RETURN_ERRCODE(EMSBIGALIAS, EIN_PARAMCHECK, EVIA_CHECKPERSONALALIAS);
	    }
	    *code = MSWP_PERSONALALIAS;
	    strcpy(Buf, StripWhiteEnds(tempalias->real));
	    ReduceWhiteSpace(Buf);
	    for (s= Buf; *s; ++s) {
		if (!isprint(*s) && *s != ' ') {
		    /* Bogus nonsense in ctype.h -- isprint rejects space! */
		    *code = MSWP_BADALIAS;
		    break;
		}
	    }
	    return(0);
	}
    }
    return(0); /* But never changed the code value since nothing found */
}


#ifdef WHITEPAGES_ENV
static int getWpErrno(code)
wp_ErrorCode code;
{/* analysis for WP codes.  Returns -1 if unknown weird error, or EMSWPCORRUPTION, or a real errno value. */
    int ncode;

    switch(code) {
	case wperr_NoError:
	    return 0;
	case wperr_OutOfMemory:
	    return(ENOMEM);
	case wperr_BTreeTempFail:
	case wperr_IndexedRecordNotFound:
	    return(EMSWPCORRUPTION);
	default:
	    break;
    }
    if (code >= wperr_BTreeBaseValue && code < wperr_GritsBaseValue) {
	ncode = code - wperr_BTreeBaseValue;
	switch(ncode) {
	    case bterr_OutOfMemory:
		return(ENOMEM);
	    case bterr_NotABTree:
	    case bterr_BTreeNotCurrVersion:
	    case bterr_BTreeDamaged:
	    case bterr_NotOpeningRoot:
	    case bterr_CursorTreeDamaged:
	    case bterr_EmptyTree:
		return(EMSWPCORRUPTION);
	}
	if (ncode >= bterr_FileSystemErrorBegin && ncode <= bterr_FileSystemErrorEnd) {
	    return(ncode - bterr_FileSystemErrorBegin);
	}
    }
    if (code >= wperr_FileSystemErrorBegin && code <= wperr_FileSystemErrorEnd) return (code - wperr_FileSystemErrorBegin);
    return(-1);
}

ReportWPErrno(wperr)
int wperr;
{
    char ErrorText[256];

    sprintf(ErrorText, "cwp_Lookup error %d (%s) in cell %s", wperr, wp_ErrorString(wperr), WpCell);
    CriticalBizarreError(ErrorText);
}

int ConvertWpErrToMSErr(wperr, Default, Report)
int wperr, Default, Report;
{
    int newCode;

    newCode = getWpErrno(wperr);
    if (Report) if (newCode < 0 || newCode == EMSWPCORRUPTION) ReportWPErrno(wperr);
    if (newCode < 0) newCode = Default;
    return(newCode);
}
#endif /* WHITEPAGES_ENV */

static int LookupInWP(Addr, laType, IDpart, PostID, WpCode, MaxNameMatches, AllowHeuristics, Domain, UnderAMSDelivery, NameSep, Answered)
PARSED_ADDRESS *Addr;
int laType;
char *IDpart, *PostID, *Domain;
int *WpCode;
int MaxNameMatches, AllowHeuristics, UnderAMSDelivery, NameSep, *Answered;
{/* Do the White Pages lookup here; result in the Addr structure and MSWP_code to WpCode. */
#ifdef WHITEPAGES_ENV
    wp_SearchToken STok;
    wp_PrimeKey PKey;
    wp_ErrorCode wpErr;
    wp_PrimeKeySet *PKs;
    int MinMatch, OutMatch, size, i, code, WasFuzzy, UsePlus, NameSepForEntry;
    ADDRESS_COMMENT *Comment;
    char *NewName, *CanonID, *CanonName, *CommText, *CPtr;
    char *CAF;
    PARSED_ADDRESS *CAF_ListHead, *CAF_Addr;
    char *affiliation, *tempstring;
#endif /* WHITEPAGES_ENV */

    debug(1,("Validating %s (type %d) in domain %s\n", Addr->LocalPart, laType, Domain));
#ifndef WHITEPAGES_ENV
    *Answered = 0;  /* This guy never answers */
#else /* WHITEPAGES_ENV */
    *Answered = 1;  /* This guy always answers */
    mserrcode = 0;
    UsePlus = CheckAMSUseridPlusWorks(Domain) > 0;
    if ((wpErr = OpenWhitePages(Domain)) != wperr_NoError) {
	*WpCode = MSWP_TEMPFAIL;
	AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPOPENFAIL, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
    }
    wpErr = wp_SetUp(IDpart,
		(laType == latype_LocalID ? LookupUIDOnly : LookupUIDWithLastPart),
		&STok);
    if (wpErr != wperr_NoError) {
	*WpCode = MSWP_TEMPFAIL;
	AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPSETUP, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
    }
    PKey = NULL;
    /* formerly used MatchNoHeuristics instead of MatchAnyName */
    wpErr = cwp_Lookup(wpCD, STok, &MinMatch, AllowHeuristics ? MatchAll : MatchAnyName, &OutMatch, &PKey);
    WasFuzzy = OutMatch > MatchIDFirstNameAbbrev;
    if (wpErr != wperr_NoError && wpErr != wperr_TooManyKeysFound) {
	wp_DeAllocate(STok);
	if (wpErr != wperr_NoKeysFound) {
	    *WpCode = MSWP_TEMPFAIL;
	    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPLOOKUPFAIL, TRUE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	} else {
	    return 0;
	}
    }
    if (wpErr == wperr_NoError) {	/* a unique match was found */
	wp_DeAllocate(STok);
	*WpCode = WasFuzzy ? MSWP_MATCHTOOFUZZY : (CheckAMSFmtOK(Domain) <= 0 ? MSWP_GOODNETMAIL : MSWP_GOODUSER);
	CanonID = NULL;
	wpErr = cwp_Read(wpCD, PKey, idxN, &CanonName);
	if (wpErr == wperr_NoSuchField) CanonName = NULL;

	if (laType == latype_LocalID && *PostID) {
	    NameSepForEntry = -1;
	}
	else {
	    NameSepForEntry = NameSep;
	    wpErr = cwp_Read(wpCD, PKey, idxCAF, &CAF);
	    if (wpErr == wperr_NoError) {
		CPtr = CAF;
		if (*CPtr == '-') CPtr++;
		while (*CPtr && isdigit(*CPtr)) CPtr++;
		if (!*CPtr) {
		    NameSepForEntry = atoi(CAF);
		}
		else if (ParseAddressList(CAF, &CAF_ListHead) == PA_OK) {
		    i = 0;
		    CAF_Addr = SingleAddress(CAF_ListHead, &i);
		    if (i == 1) {
			ReplaceAddress(Addr, CAF_Addr);
			FreeAddressList(CAF_ListHead);
			free(PKey);
			return 0;
		    }
		    FreeAddressList(CAF_ListHead);
		}
	    }
	    else if (wpErr != wperr_NoSuchField) {
		*WpCode = MSWP_TEMPFAIL;
		free(PKey);
		AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	}
	if (CanonName != NULL && NameSepForEntry > 0 && !(laType == latype_LocalID && *PostID)) {
	    NewName = NewString(CanonName);	/* Need to fool with a copy */
	    if (NewName == NULL) {
		*WpCode = MSWP_TEMPFAIL;
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    for (CPtr = NewName; *CPtr != '\0'; ++CPtr)
		if (*CPtr == ' ') *CPtr = NameSepForEntry;
	    tempstring = Quote822LPart(NewName);
	    free(NewName);
	    if (tempstring == NULL) {
		*WpCode = MSWP_TEMPFAIL;
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    NewName = tempstring;
	    wpErr = AddressMatchesUnambiguously(wpCD, NewName, PKey);
	    if (wpErr != wperr_NoError) {
		free(NewName);
		NameSepForEntry = -1;
	    }
	}
	if (NameSepForEntry <= 0) {
	    wpErr = cwp_Read(wpCD, PKey, idxID, &CanonID);
	    if (wpErr == wperr_NoSuchField) {
		CanonID = NULL;
		wpErr = cwp_Read(wpCD, PKey, idxN, &CanonName);
		if (wpErr != wperr_NoError) {	/* must have name if no ID */
		    free(PKey);
		    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
		}
	    } else if (wpErr != wperr_NoError) {
		free(PKey);
		AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    if (CanonID == NULL) {
		OutMatch = strlen(CanonName);
	    } else {
		OutMatch = strlen(CanonID) + 1;
		if (laType == latype_LocalID) OutMatch += strlen(PostID);
	    }
	    NewName = malloc(OutMatch + 1);
	    if (NewName == NULL) {
		free(PKey);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    if (CanonID == NULL) {
		strcpy(NewName, CanonName);
		for (CPtr = NewName; *CPtr != '\0'; ++CPtr) {
			if (*CPtr == ' ') *CPtr = '_';
		}
		tempstring = Quote822LPart(NewName);
		free(NewName);
		if (!tempstring) {
		    *WpCode = MSWP_TEMPFAIL;
		    free(PKey);
		    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
		}
		NewName = tempstring;
	    } else {
		if (laType == latype_LocalID)
			sprintf(NewName, "%s+%s", CanonID, PostID);
		else
			sprintf(NewName, UsePlus ? "%s+" : "%s", CanonID);

		CanonName = NULL;
		wpErr = cwp_Read(wpCD, PKey, idxN, &CanonName);	/* get name, if any */
		if (wpErr != wperr_NoSuchField && wpErr != wperr_NoError) {
		    *WpCode = MSWP_TEMPFAIL;
		    free(PKey);
		    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
		}
		if (CanonName != NULL && Addr->RoutePhrase == NULL) {
		    Addr->RoutePhrase = NewString(CanonName); /* not to worry on err */
		}
	    }
	    free(PKey);
	} else {
	    free(PKey);	/* we already have all we need */
	}
	if (Addr->LocalPart != NULL) free(Addr->LocalPart);
	Addr->LocalPart = NewName;
	return 0;
    }
/* That handled all cases except for ambiguous matches in the initial wp_Lookup.
   At this point PKey may be allocated, and STok is allocated. */

#define INITIAL_COMM_SIZE 400
    *WpCode = WasFuzzy ? MSWP_FUZZYAMBIGMATCH : MSWP_AMBIGUOUS;
    if (PKey != NULL) free(PKey);
    size = INITIAL_COMM_SIZE;	/* Track the existing size */
    Comment = (ADDRESS_COMMENT *) malloc(sizeof(ADDRESS_COMMENT));
    if (Comment == NULL) {
	wp_DeAllocate(STok);
	AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
    }
    CommText = malloc(INITIAL_COMM_SIZE);	/* Do an initial allocation */
    if (CommText == NULL) {
	wp_DeAllocate(STok);
	free(Comment);
	AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
    }
    PKs = NULL;
    wpErr = cwp_Search(wpCD, STok, MaxNameMatches, MatchAll, &OutMatch, &PKs);
    wp_DeAllocate(STok);
    if (wpErr == wperr_TooManyKeysFound) {
	if (PKs != NULL) wp_DeAllocate(PKs);
	free(Comment);
	free(CommText);
	*WpCode = WasFuzzy ? MSWP_FUZZYTOOMANYMATCHES : MSWP_VERYAMBIGUOUS;
	return(0);
    } else if (wpErr != wperr_NoError) {
	sprintf(CommText, "(Cannot disambiguate address: %s)", wp_ErrorString(wpErr));
	Comment->Text = CommText;
	Comment->Next = Addr->Comments;
	Addr->Comments = Comment;
	if (PKs != NULL) wp_DeAllocate(PKs);
	*WpCode = WasFuzzy ? MSWP_PROBABLYFUZZYAMBIG : MSWP_AMBIGWITHERRORS;
	code = ConvertWpErrToMSErr(wpErr, -1, FALSE);
	if (code >= 0 && vdown(code)) {
	    AMS_RETURN_ERRCODE(code, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	}
	return(0);
    }
    /* Ambiguous name, reasonable number of matches--list them */
    if (MaxNameMatches < 1) {
	strcpy(CommText, AMS_VALIDATION_ERR_PREFIX);
	strcat(CommText, "Ambiguous address matches the following:\n\t");
    } else {
	strcpy(CommText, "(");
    }
    for (i = 0; i < PKs->KeyCount; ++i) {
	CanonID = CanonName = NULL;
	wpErr = cwp_Read(wpCD, PKs->Keys[i], idxID, &CanonID);
	if (wpErr == wperr_NoSuchField) {
	    CanonID = NULL;
	} else if (wpErr != wperr_NoError) {
	    free(Comment);
	    free(CommText);
	    wp_DeAllocate(PKs);
	    *WpCode = MSWP_TEMPFAIL;
	    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	}

	wpErr = cwp_Read(wpCD, PKs->Keys[i], idxN, &CanonName);
	if (wpErr == wperr_NoSuchField) {
	    CanonName = NULL;
	} else if (wpErr != wperr_NoError) {
	    free(Comment);
	    free(CommText);
	    wp_DeAllocate(PKs);
	    *WpCode = MSWP_TEMPFAIL;
	    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	}
	if (CanonName == NULL && CanonID == NULL) {
	    free(Comment);
	    free(CommText);
	    wp_DeAllocate(PKs);
	    *WpCode = MSWP_TEMPFAIL;
	    AMS_RETURN_ERRCODE(EINVAL, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	}
	size += (CanonID ? strlen(CanonID) : 0) + (CanonName ? strlen(CanonName) : 0)  + 100 + strlen(Domain);
	CommText = realloc(CommText, size);
	if (CommText == NULL) {
		free(Comment);
		wp_DeAllocate(PKs);
		*WpCode = MSWP_TEMPFAIL;
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	}

	wpErr = cwp_Read(wpCD, PKs->Keys[i], idxCAF, &CAF);
	if (wpErr == wperr_NoSuchField) {
	    NameSepForEntry = NameSep;
	}
	else if (wpErr != wperr_NoError) {
	    free(Comment);
	    free(CommText);
	    wp_DeAllocate(PKs);
	    *WpCode = MSWP_TEMPFAIL;
	    AMS_RETURN_ERRCODE(ConvertWpErrToMSErr(wpErr, EMSWPREAD, FALSE), EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	}
	else {
	    CPtr = CAF;
	    if (*CPtr == '-') CPtr++;
	    while (*CPtr && isdigit(*CPtr)) CPtr++;
	    if (!*CPtr) {
		NameSepForEntry = atoi(CAF);
	    }
	    else {
		OutMatch = (CanonID ? strlen(CanonID) : 0) + (CanonName ? strlen(CanonName) : 0)  + strlen(Domain);
		if (OutMatch < strlen(CAF)) {
		    size += strlen(CAF)-OutMatch;
		    CommText = realloc(CommText, size);
		    if (CommText == NULL) {
			free(Comment);
			wp_DeAllocate(PKs);
			*WpCode = MSWP_TEMPFAIL;
			AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
		    }
		}
		strcat(CommText, CAF);
		wpErr = cwp_Read(wpCD, PKs->Keys[i], idxAf, &affiliation);
		if (wpErr == wperr_NoError) {
		    strcat(CommText, " (");
		    strcat(CommText, affiliation);
		    strcat(CommText, ")");
		    if (strlen(affiliation) > 5) size += (strlen(affiliation) - 5);
		}
		goto didentry;
	    }
	}

	if (CanonName == NULL) NameSepForEntry = -1;
	if (CanonID == NULL && NameSepForEntry <= 0) NameSepForEntry = '_';
	if (NameSepForEntry > 0) {
	    CPtr = &CommText[strlen(CommText)];
	    strcpy(CPtr, CanonName);
	    for (tempstring = CPtr; *tempstring != '\0'; ++tempstring)
		if (*tempstring == ' ') *tempstring = NameSepForEntry;
	    tempstring = Quote822LPart(CPtr);
	    if (!tempstring) {
		free(Comment);
		wp_DeAllocate(PKs);
		*WpCode = MSWP_TEMPFAIL;
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }	    
	    strcpy(CPtr, tempstring);
	    free(tempstring);

	    wpErr = AddressMatchesUnambiguously(wpCD, CPtr, PKs->Keys[i]);
	    if (CanonID && wpErr != wperr_NoError) {
		NameSepForEntry = -1;
		*CPtr = '\0';
	    }
	    else {
		strcat(CPtr, "@");
		strcat(CPtr, Domain);
		wpErr = cwp_Read(wpCD, PKs->Keys[i], idxAf, &affiliation);
		if (wpErr == wperr_NoError) {
		    strcat(CommText, " (");
		    strcat(CommText, affiliation);
		    strcat(CommText, ")");
		    if (strlen(affiliation) > 5) size += (strlen(affiliation) - 5);
		}
	    }
	}
	if (NameSepForEntry <= 0) {
	    if (CanonName != NULL) {
		tempstring = Quote822Phrase(CanonName);
		if (!tempstring) {
		    free(Comment);
		    wp_DeAllocate(PKs);
		    *WpCode = MSWP_TEMPFAIL;
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
		}	    
		strcat(CommText, tempstring);
		free(tempstring);
		wpErr = cwp_Read(wpCD, PKs->Keys[i], idxAf, &affiliation);
		if (wpErr == wperr_NoError) {
		    strcat(CommText, " (");
		    strcat(CommText, affiliation);
		    strcat(CommText, ")");
		    if (strlen(affiliation) > 5) size += (strlen(affiliation) - 5);
		}
		strcat(CommText, " <");
	    }
	    CPtr = &CommText[strlen(CommText)];
	    strcpy(CPtr, CanonID);
	    strcat(CommText, UsePlus ? "+@" : "@");
	    strcat(CommText, Domain);
	    if (CanonName != NULL) strcat(CommText, ">");
	}

    didentry:

	if (i < (PKs->KeyCount - 1)) {
		strcat(CommText, ", ");
		if (MaxNameMatches < 1) {
		    strcat(CommText, "\n\t");
		}
	}
    }
    strcat(CommText, ")");
    wp_DeAllocate(PKs);
    Comment->Text = CommText;
    Comment->Next = Addr->Comments;
    Addr->Comments = Comment;
#endif /* WHITEPAGES_ENV */
    return 0;
}

static int LookupInPasswdFile(Addr, laType, IDpart, PostID, WpCode, MaxNameMatches, AllowHeuristics, Domain, UnderAMSDelivery, NameSep, Answered)
PARSED_ADDRESS *Addr;
int laType;
char *IDpart, *PostID, *Domain;
int *WpCode;
int MaxNameMatches, AllowHeuristics, UnderAMSDelivery, NameSep, *Answered;
{
    struct passwd *p;
    char *NewName, *RealName;

    p = getpwnam(IDpart);
    if (p) {
	*WpCode = (CheckAMSFmtOK(Domain) <= 0 ? MSWP_GOODNETMAIL : MSWP_GOODUSER);
	RealName = NULL;
	if (p->pw_gecos && *(p->pw_gecos)) {
		GetNameFromGecos(p->pw_gecos, p->pw_name, Domain, &RealName);
		if (!RealName) AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
	}
	if (NameSep <= 0 || !RealName || (laType == latype_LocalID && *PostID)) {
	    NewName = malloc(strlen(p->pw_name) + ((laType == latype_LocalID) ? strlen(PostID) : 0) + 2);
	    if (NewName == NULL) {
		if (RealName) free(RealName);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    if (laType == latype_LocalID) {
		sprintf(NewName, "%s+%s",  p->pw_name, PostID);
	    } else {
		sprintf(NewName, (CheckAMSUseridPlusWorks(Domain) > 0 ? "%s+" : "%s"), p->pw_name);
	    }
	    if (RealName != NULL && Addr->RoutePhrase == NULL) {
		Addr->RoutePhrase = RealName;
		RealName = NULL;	/* Consume the name */
	    }
	} else {		/* using fullname as local-part */
	    char *S;
	    for (S = RealName; *S != '\0'; ++S) if (*S == ' ') *S = NameSep;
	    NewName = RealName;
	    RealName = NULL;	/* Consume the name */
	}
	if (Addr->LocalPart != NULL) free(Addr->LocalPart);
	Addr->LocalPart = NewName;
	if (RealName) free(RealName);
	*Answered = 1;
    }
    return(0);
}

static void StripExtraComments(addr, ExtraThingToNuke)
PARSED_ADDRESS *addr;
char *ExtraThingToNuke;
{/* Strip out duplicate comments and comments beginning with either AMS_VALIDATION_ERR_PREFIX or ExtraThingToNuke. */
    ADDRESS_COMMENT *Prev, *This, *Top;

    for (This = addr->Comments; This != NULL; This = This->Next) {
	if (This->Text != NULL && This->Text[0] != '\0'
	    && (strncmp(This->Text, AMS_VALIDATION_ERR_PREFIX,
			sizeof(AMS_VALIDATION_ERR_PREFIX) -1) == 0
		|| (ExtraThingToNuke && strcmp(This->Text, ExtraThingToNuke) == 0))) {
		This->Text[0] = '\0';
	}
    }
    for (Top = addr->Comments; Top != NULL; Top = Top->Next) {
	Prev = Top;
	while (Prev->Next != NULL) {
		This = Prev->Next;
		if (strcmp(Top->Text, This->Text) == 0) {	/* a dupl */
			Prev->Next = This->Next;
			free(This->Text);
			free(This);
		} else {		/* not a dupl */
			Prev = Prev->Next;
		}
	}
    }
}

#ifdef NOTDEF
/* we may want this again some day */
static void StripExtraCommentsFromList(AddrList, ExtraThingToNuke)
PARSED_ADDRESS *AddrList;
char *ExtraThingToNuke;
{/* Traverse the AddrList tree stripping known-useless comments. */

	FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
	    switch (ThisAddr->Kind) {
	case SIMPLE_ADDRESS:
		StripExtraComments(ThisAddr, ExtraThingToNuke);
		break;
	case GROUP_ADDRESS:
		StripExtraCommentsFromList(ThisAddr->Members, ExtraThingToNuke);
		break;
	default:
		break;
	    }
	})
}
#endif /* NOTDEF */

static int LookupLocalName(Addr, laType, IDpart, PostID, WpCode, MaxNameMatches, AllowHeuristics, Domain, UnderAMSDelivery, NameSep, IsVacuous)
PARSED_ADDRESS *Addr;
int laType;
char *IDpart, *PostID, *Domain;
int *WpCode;
int MaxNameMatches, AllowHeuristics, UnderAMSDelivery, NameSep, *IsVacuous;
{
    int Answered = 0, curr = 1, nextcurr = 0, LocalDBVal, PwdVal, WPVal;
#ifdef USE_MMDF_ENV
    int MMDFVal;
#endif
    Boolean isnext;

    if (UnderAMSDelivery > 0) {	/* Override how one does name validation */
	LocalDBVal = PwdVal = 0;
	WPVal = 1;
    } else {
	if (ULstrcmp(Domain, ThisDomain) == 0) {
	    LocalDBVal = AMS_LocalDatabaseValidation;
	    PwdVal = AMS_PasswdValidation;
	    WPVal = AMS_WPValidation;
#ifdef USE_MMDF_ENV
	    MMDFVal = AMS_MMDFValidation;
#endif
	} else {
	    nextcurr = CheckAMSValidationMask(Domain);
	    if (nextcurr != vld_WPValid) { /* don't know how to do lookups other than WP-only */
		AMS_RETURN_ERRCODE(ESPIPE, EIN_PARAMCHECK, EVIA_REWRITEADDRESS);
	    }
	    LocalDBVal = PwdVal = 0;
	    WPVal = 1;
	}
    }
    if (NameSep == 0) NameSep = CheckAMSNameSep(Domain);
    if ((LocalDBVal < 0) || (PwdVal < 0) || (WPVal < 0)) {
	NonfatalBizarreError("This site is misconfigured with a negative value for an AMS_XXXValidation value in AndrewSetup or mailconf.c");
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_REWRITEADDRESS);
    }
    if (!PwdVal && !LocalDBVal && !WPVal) {
	*IsVacuous = TRUE;
	*WpCode = (CheckAMSFmtOK(Domain) <= 0 ? MSWP_GOODNETMAIL : MSWP_GOODUSER);
	return(0);
    }
    *IsVacuous = FALSE;
    *WpCode = MSWP_CRAP; /* A default in case no validation succeeds */
    do {
	isnext = FALSE;
	if (curr == LocalDBVal) {
	    int OutCode;

	    if (LookupInLocalDatabase(Addr, laType, IDpart, PostID, Domain, UnderAMSDelivery, NameSep, MaxNameMatches, &Answered, &OutCode)) {
		AMS_RETURN_ERRCODE(errno, EIN_LOCALNAMEDB, EVIA_REWRITEADDRESS);
	    }
	    if (Answered) {
		*WpCode = OutCode;
		return(0);
	    }
	} else if ((curr < LocalDBVal)) {
	    if (!isnext || (nextcurr > LocalDBVal)) {
		nextcurr = LocalDBVal;
		isnext = TRUE;
	    }
	}
#ifdef USE_MMDF_ENV
	if (curr == MMDFVal) {
	    int OutCode;

	    mserrcode = LookupInMMDFDatabase(Addr, laType, IDpart, PostID, Domain, UnderAMSDelivery, NameSep, MaxNameMatches, &Answered, &OutCode);
	    if (mserrcode)
		return(mserrcode);
	    if (Answered) {
		*WpCode = OutCode;
		return(0);
	    }
	} else if ((curr < MMDFVal)) {
	    if (!isnext || (nextcurr > MMDFVal)) {
		nextcurr = MMDFVal;
		isnext = TRUE;
	    }
	}
#endif
	if (curr == PwdVal) {
	    mserrcode = LookupInPasswdFile(Addr, laType, IDpart, PostID, WpCode, MaxNameMatches, AllowHeuristics, Domain, UnderAMSDelivery, NameSep, &Answered);
	    if (mserrcode || Answered) return(mserrcode);
	} else if ((curr < PwdVal)) {
	    if (!isnext || (nextcurr > PwdVal)) {
		nextcurr = PwdVal;
		isnext = TRUE;
	    }
	}
	if (curr == WPVal) {
	    mserrcode = LookupInWP(Addr, laType, IDpart, PostID, WpCode, MaxNameMatches, AllowHeuristics, Domain, UnderAMSDelivery, NameSep, &Answered);
	    if (mserrcode || Answered) return(mserrcode);
	} else if ((curr < WPVal)) {
	    if (!isnext || (nextcurr > WPVal)) {
		nextcurr = WPVal;
		isnext = TRUE;
	    }
	}
	curr = nextcurr;
    } while (isnext);
    return(0);
}

static int SameCompleteAddress(addr1, addr2)
PARSED_ADDRESS *addr1, *addr2;
{/* Boolean: returns whether the non-comment parts of the given addresses are the same. */
    ADDRESS_HOST *h1, *h2;
    if (addr1 == NULL || addr2 == NULL) return (FALSE);
    if (strcmp(addr1->LocalPart, addr2->LocalPart) != 0) return (FALSE);
    h1 = addr1->Hosts->Next; h2 = addr2->Hosts->Next;
    for (;;) {
	if (h1 == addr1->Hosts && h2 == addr2->Hosts) return (TRUE);
	if (h1 == addr1->Hosts || h2 == addr2->Hosts) return (FALSE);
	if (ULstrcmp(h1->Name, h2->Name) != 0) return (FALSE);
	h1 = h1->Next; h2 = h2->Next;
    }
}

static void SetAMSDelNameSep(Domain, AMSDel, AMSNameSep)
char *Domain;
int *AMSDel, *AMSNameSep;
{/* Set these two flags based on Domain. */
	*AMSDel = CheckAMSDelivery(Domain);
	*AMSNameSep = CheckAMSNameSep(Domain);
}

static int local_RewriteAddress();	/* forward declaration */

static int AddrRewrite(Addr, ErrCode, Domain, recdepth, SideBuf, SideBufLen, upAddr)
PARSED_ADDRESS *Addr, *upAddr;
int *ErrCode, recdepth, SideBufLen;
char *Domain, *SideBuf;
{/* Recursive worker for MS_RewriteAddress.  Modify the Addr structure, but don't parse or unparse. */
    int la_errcode, outType, SawTempFail = 0, IsCertain=1, WasGoodGlobalAlias;
    struct passwd *pass;
    char *IDpart, *PostID, *AddressAsPost, *PostCell;
#ifdef AFS_ENV
    char *NewFilename;
#endif /* AFS_ENV */
    char InBuf[1000], *t, FileBuf[1+MAXPATHLEN];
    int WpCode, DisambRes, OutLen, HadHost, AMSDel, AMSNameSep, IsVacuous;
    
    debug(1, ("AddrRewrite localpart %s/%s prevailing domain %s recdepth %d\n", Addr->LocalPart, (Addr->Hosts->Prev->Name != NULL ? Addr->Hosts->Prev->Name : "noDomain"), Domain, recdepth));
    *ErrCode = MSWP_CRAP;
    SetAMSDelNameSep(Domain, &AMSDel, &AMSNameSep);
    if (AMSDel == 0) {
	AMS_RETURN_ERRCODE(ETIMEDOUT, EIN_GETCELLFROMFILE, EVIA_REWRITEADDRESS);
    }
    IDpart = PostID = NULL;
    la_errcode = la_KindDomain(Addr, &outType, &IDpart, &PostID, Domain);
    if (la_errcode != laerr_NoError) {
	switch(la_errcode) {
	    case laerr_OutOfMemory:
		AMS_RETURN_ERRCODE(ENOMEM, EIN_LAKIND, EVIA_REWRITEADDRESS);
	    case laerr_UnrecSpecial:
		if (ULstrcmp(Domain, MyMailDomain) != 0) {
		    *ErrCode = MSWP_TEMPFAIL;
		    return 0;	/* not sure what other cells will use for +foo+ types */
		} /* else fall through */
	    case laerr_SyntaxError:
	    case laerr_BadSecond:
		if (AMSDel <= 0 || recdepth > 0) {outType = latype_LocalName; break;}
		AMS_RETURN_ERRCODE(EMSBADLOCALSYNTAX, EIN_LAKIND, EVIA_REWRITEADDRESS);
	    default:
		AMS_RETURN_ERRCODE(EMSUNKNOWN, EIN_LAKIND, EVIA_REWRITEADDRESS);
	}
    }
/* Add back the default host name if there is none; record whether there was one. */
    if (Addr->Hosts != NULL) {
	HadHost = 1;
	if(Addr->Hosts == Addr->Hosts->Next) {
	    char *myhost;
	    ADDRESS_HOST *HostPtr;

	    HadHost = 0;
	    if (!AMS_NoDomainPreferredOnLocalMail) {
		myhost = malloc(1+strlen(Domain));
		if (!myhost) {
		    if (IDpart) free(IDpart);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		}
		strcpy(myhost, Domain);
		HostPtr = MakeHost(myhost);
		if (!HostPtr) {
		    if (IDpart) free(IDpart);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		}
		if (AddHost(Addr, HostPtr) != PA_OK) {
		    if (IDpart) free(IDpart);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		}
	    }
	}
    } else {
	if (IDpart) free(IDpart);
	AMS_RETURN_ERRCODE(EMSUNKNOWN, EIN_PARSEADDR, EVIA_REWRITEADDRESS);
    }
    switch(outType) {
	case latype_DistList:
	case latype_DirInsert:
	    /* Should check for file existence */
#ifdef AFS_ENV
	    if (IDpart && IDpart[0] == '~' && ULstrcmp(Domain, MyMailDomain) != 0) {
		/* Resolve it here (in ``Domain'') when there's another cell involved. */
		strncpy(FileBuf, IDpart, sizeof(FileBuf));
		mserrcode = ResolveTildes(FileBuf, &NewFilename, Domain);
		if (IDpart) free(IDpart);
		if (mserrcode) {
		    return (mserrcode);
		}
		IDpart = NewFilename;
	    }
#endif /* AFS_ENV */
	    DisambRes = MS_DisambiguateFile(IDpart, FileBuf,
				(outType == latype_DirInsert ?
					AMS_DISAMB_ISADIR :
					AMS_DISAMB_ISAFILE));
	    if (IDpart) {
		OutLen = strlen(IDpart);
		free(IDpart);
	    } else {
		OutLen = 10;
	    }
	    if (DisambRes) {
		if (vdown(AMS_ERRNO)) {
		    IsCertain = FALSE;
		    *ErrCode = MSWP_PROBABLYGOOD;
		} else if (AMS_ERRNO == EACCES) {
		    *ErrCode = (outType == latype_DirInsert ?
				MSWP_PROTDIRINSERT :
				MSWP_PROTDISTLIST);
		} else if (AMS_ERRNO == ENOENT) {
		    *ErrCode = (outType == latype_DirInsert ?
				MSWP_BADDIRINSERT :
				MSWP_BADDISTLIST);
		} else if (AMS_ERRNO == EISDIR && outType == latype_DistList) {
		    *ErrCode = MSWP_DISTLISTDIR;
		} else if (AMS_ERRNO == ENOTDIR && outType == latype_DirInsert) {
		    *ErrCode = MSWP_DIRINSERTFILE;
		} else {
		    return(mserrcode);
		}
	    } else {
		char *newloc; char FileCell[250]; char ErrMsg[1000];

		if (outType == latype_DistList && recdepth < 13 && GetCellFromFileName(FileBuf, FileCell, sizeof(FileCell)) == 0 && ULstrcmp(FileCell, Domain) != 0 && CheckAMSDelivery(FileCell) < 0) {
		    sprintf(ErrMsg, "Distribution list %s is in cell %s, which does not support AMDS.", ap_Shorten(FileBuf), FileCell);
		    NonfatalBizarreError(ErrMsg);
		}
		newloc = malloc(6+strlen(FileBuf) + OutLen);
		if (!newloc) {
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		}
		sprintf(newloc, "+%s+%s",
			(outType == latype_DistList ? "dist" : "dir-insert"),
			FileBuf);
		free(Addr->LocalPart);
		Addr->LocalPart = newloc;
		*ErrCode = (outType == latype_DistList ?
					MSWP_DISTLIST :
					MSWP_DIRINSERT);
	    }
	    return(0);
#ifdef AFS30_ENV
	case latype_FSMembers:
	    /* make sure that we're really dealing with a group here */
	    { int ret_code;

	      if (IDpart == NULL) {
		*ErrCode = MSWP_BADFSMEMBERS;
	      }
	      if ((ret_code = aq_GroupP(IDpart, Domain)) < 0) {
		*ErrCode = MSWP_UNKNOWNFSMEMBERS;
	      } else {
		if (ret_code == 0) {
		  *ErrCode = MSWP_BADFSMEMBERS;
		} else {
		  *ErrCode = MSWP_FSMEMBERS;
		}
	      }
	    }
	    if (IDpart) free(IDpart);
	    return(0);
#endif /* AFS30_ENV */
	case latype_Remote :
	    {
	    char OldHostName[500], NewHostName[500], *nhost, *uglyhost=NULL;
	    int hostcode = MSWP_CRAP;
	    ADDRESS_HOST *tmphost;
	    enum MailHostQuality mhq;

	    tmphost = Addr->Hosts->Prev;
	    if (tmphost && tmphost->Name) {
		  strcpy(OldHostName, "@");
		  strcat(OldHostName, tmphost->Name);
		  if (recdepth == 0) {
		    if (CheckPersonalAlias(OldHostName, NewHostName, sizeof(NewHostName), &hostcode)) {
		      if (vdown(AMS_ERRNO)) {
			  ++SawTempFail;
		      } else {
			  return(mserrcode);
		      }
		    }
		    if (hostcode != MSWP_CRAP) {
		      nhost = malloc(1+strlen(NewHostName));
		      if (!nhost) {
			  AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		      }
		      strcpy(nhost, &NewHostName[1]);
		      free(tmphost->Name);
		      tmphost->Name = nhost;
		      if (Addr->MD != NULL)
			{la_FreeMD(Addr->MD); Addr->MD = NULL;}
		    }
		  }
		  if (Addr->MD != NULL) {
			mhq = Addr->MD->Qual;
			strncpy(NewHostName, (Addr->MD->Final != NULL ? Addr->MD->Final : ""), sizeof(NewHostName));
		  } else {
		    mhq = ValidateMailHostName(tmphost->Name, NewHostName, sizeof(NewHostName), 10);
		  }
		  switch(mhq) {
		      case mailhost_good:
			  if (strcmp(tmphost->Name, NewHostName) != 0) {
			    nhost = malloc(1+strlen(NewHostName));
			    if (!nhost) {
			      AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
			    }
			    strcpy(nhost, NewHostName);
			    free(tmphost->Name);
			    tmphost->Name = nhost;
			  }
			  debug(1, ("Considering recursion: recdepth %d, given dom %s, prevail %s\n", recdepth, tmphost->Name, Domain));
			  if (recdepth < 13 && ULstrcmp(tmphost->Name, Domain) != 0 && !SameCompleteAddress(Addr, upAddr) && (CheckAMSDelivery(tmphost->Name) > 0 || CheckAMSValidationMask(tmphost->Name) == vld_WPValid)) {/* try rmt valid */
			    struct MailDom *oldMD;
			    int NewRes, NewErrcode, oldmserr;

			    RemHost(tmphost);	/* Unlink from the Addr structure. */
			    oldMD = Addr->MD;	/* and save the analysis of the host */
			    Addr->MD = NULL;
			    oldmserr = mserrcode;
			    NewRes = AddrRewrite(Addr, &NewErrcode, tmphost->Name, recdepth+1, SideBuf, SideBufLen, upAddr);
			    mserrcode = oldmserr;
			    debug(1, ("Recursion result is %d/%d\n", NewRes, NewErrcode));
			    if (NewRes != 0 || NewErrcode == MSWP_BADNETMAIL || NewErrcode == MSWP_UNKNOWNNETMAIL) {
				/* Failure; put the host back on. */
				AddHost(Addr, tmphost);
				la_FreeMD(Addr->MD);
				Addr->MD = oldMD;
				if (NewRes != 0) return (mserrcode = NewRes);
				/* if ``bad host'' via another cell, not to worry; let the other delivery system handle it.  Call it ``good host'' because we recognize the other cell. */
			    } else {	/* Success; keep the outer host off. */
				FreeHost(tmphost);
				la_FreeMD(oldMD);
				*ErrCode = NewErrcode;
				break;	/* Done with structure rewriting. */
			    }
			  }
			  *ErrCode = (CheckAMSFmtOK(tmphost->Name) <= 0 ? MSWP_GOODNETMAIL: MSWP_GOODUSER);
			  break;
		      case mailhost_bad:
			  uglyhost = tmphost->Name;
			  *ErrCode = MSWP_BADNETMAIL;
			  break;
		      case mailhost_indeterminate:
		      default:
			  uglyhost = tmphost->Name;
			  *ErrCode = MSWP_UNKNOWNNETMAIL;
			  break;
		  }
	    } else {
		*ErrCode = CheckAMSUUCPSupp(Domain) > 0 ? MSWP_GOODNETMAIL : MSWP_UNKNOWNNETMAIL;
	    }
	    if (IDpart) free(IDpart);
	    errno = 0;
	    if (uglyhost) {
		char *DumBuf;
		ADDRESS_COMMENT *TempComm, *ThisComm, *NextComm;

		DumBuf = malloc(3+strlen(uglyhost));
		if (!DumBuf) {
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		}
		TempComm = (ADDRESS_COMMENT *) malloc(sizeof(ADDRESS_COMMENT));
		if (!TempComm) {
		    free(DumBuf);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
		}
		sprintf(DumBuf, "(%s)", uglyhost);
		ThisComm = Addr->Comments;
		while (ThisComm) {
		    NextComm = ThisComm->Next;
		    if (ThisComm->Text) free(ThisComm->Text);
		    free(ThisComm);
		    ThisComm = NextComm;
		}
		Addr->Comments = TempComm;
		TempComm->Next = NULL;
		TempComm->Text = DumBuf;
	    }
	    return(0);
	    }
	case latype_LocalID:
	case latype_LocalName:
	    break; /* handle it below */
	default:
	    *ErrCode = MSWP_UNKNOWNGOOD;
	    if (IDpart) free(IDpart);
	    return(0);
    }
/* Got here if outType was latype_LocalID or latype_LocalName */
/* The CheckPersonalAlias case, as well as the .MS.DirectPost file case, put their results into SideBuf, not into the address list.  Those two cases are the only such ones: all other cases put their results into the address list. */
    if (!HadHost && recdepth == 0 && CheckPersonalAlias(Addr->LocalPart, SideBuf, SideBufLen, ErrCode)) {
	if (vdown(AMS_ERRNO)) {
	    ++SawTempFail;
	} else {
	    if (IDpart) free(IDpart);
	    return(mserrcode);
	}
    }
    if (*ErrCode != MSWP_CRAP) {
	if (IDpart) free(IDpart);
	return(0);
    }
    if (outType == latype_LocalID && CheckAMSUseridPlusWorks(Domain) >= 0) {
	char *newloc;

	newloc = malloc(2+strlen(IDpart)+(PostID ? strlen(PostID) : 0));
	if (!newloc) {
	    if (IDpart) free(IDpart);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_REWRITEADDRESS);
	}
	sprintf(newloc, "%s+%s", IDpart, PostID ? PostID : "");
	free(Addr->LocalPart);
	Addr->LocalPart = newloc;
	    
	pass =  getcpwnam(IDpart, Domain);
	if (pass) { /* Is a valid user id */
	    int PostCode;

	    PostCode = -1;
	    if (PostID && *PostID && ULstrcmp(IDpart, Me) && pass->pw_dir && *pass->pw_dir && pass->pw_dir && !SameCompleteAddress(Addr, upAddr)) {
		char *recip, *pathname, *cellN;
		int mspathgen, PostableStatus, newVal, newErr, DirLen;

		mspathgen = 0;
		DirLen = strlen(pass->pw_dir);

		if (recdepth < 13) while (GenMSPathElts(&mspathgen, Domain, &pathname, &cellN, &newErr)) {
		    if (strncmp(pathname, pass->pw_dir, DirLen) != 0) continue;
		    if (cellN != NULL && cellN != ((char *) -1)) { /* If this root isn't in this cell (this WS), bypass it. */
			if (ULstrcmp(Domain, (*cellN != '\0' ? cellN : WorkstationName)) != 0) continue;
		    }
		    /* left part is on mspath and right part non-empty*/
		    recip = cellN = NULL;
		    mserrcode = GetPostableStatus(pathname, PostID, &PostableStatus, &IsCertain, &recip, &cellN);
		    if (mserrcode) {if (IDpart) free(IDpart); return(mserrcode);}
		    if (PostCode < 0) PostCode = PostableStatus;    /* remember the first one */
		    if (recip != NULL && *recip != '\0' && PostID != recip && strcmp(PostID, recip) != 0) {
/* We've read the address (recip) from some .MS.DirectPost file.  We need to validate the .MS.DirectPost file address according to the cell in which we found that file, if for no other reason than to get that cell's name appended to the address. */
			if (IDpart) free(IDpart);
			if (cellN != NULL) {
			    if (ULstrcmp(cellN, Domain) == 0) newVal = 1;
			    else newVal = CheckAMSDelivery(cellN);
			    if (newVal < 0) if (CheckAMSValidationMask(cellN) == vld_WPValid) newVal = 1;
			    if (newVal < 0) AMS_RETURN_ERRCODE(EPROTONOSUPPORT, EIN_GETCELLFROMFILE, EVIA_REWRITEADDRESS);
			    if (newVal > 0) {
				newVal = local_RewriteAddress(recip, SideBuf, SideBufLen, &newErr, cellN, recdepth+1, Addr);
				if (newVal != 0) return (newVal);
				/* *ErrCode = newErr; ---Rewrite the address for the other cell (local_RewriteAddress stores it into SideBuf), but don't overwrite the classification-as-folder code. */
				*ErrCode = MergeMSWPCodes(PostableStatus, newErr);
				return(0);
			    }
			}
			strncpy(SideBuf, recip, SideBufLen);    /* Copy address text to SideBuf. */
			*ErrCode = PostableStatus;
			return (0);
		    }
		}
	    }
	} 
    }
    /* non-bbroot user id will pass through to here for a wp check (in LookupLocalName) */
    WasGoodGlobalAlias = -1;
    if (AMSDel <= 0 && recdepth == 0 && AMS_AliasesValidation > 0) {
	if (CheckGlobalAlias(Addr->LocalPart, &WasGoodGlobalAlias, Domain)) {
	    if (tfail(AMS_ERRNO)) {
		++SawTempFail;
	    } else {
		if (IDpart) free(IDpart);
		return(mserrcode);
	    }
	}
    }
/* Names with spaces or tabs in them aren't bboards, but be kind on the ends. */
    StripWhiteEnds(Addr->LocalPart);
    AddressAsPost = Addr->LocalPart;
    PostCell = NULL;
    if (strchr(Addr->LocalPart, ' ') || strchr(Addr->LocalPart, '\t')) {
	*ErrCode = MSWP_CRAP;
    } else {
	if (mserrcode = IsRecipientAnMSDirectory(&AddressAsPost, ErrCode, &IsCertain, Domain, &PostCell)) {
	    if (IDpart) free(IDpart);
	    return(mserrcode);
	}
	if (!IsCertain) ++SawTempFail;
    }
    debug(1,("Before validation: *ErrCode %d, IsCertain %d, SawTempFail %d\n", *ErrCode, IsCertain, SawTempFail));
    if (LookupLocalName(Addr, outType, IDpart, PostID, &WpCode, MSWP_MAXAMBIGMATCHES-1, (WasGoodGlobalAlias < 0 && (*ErrCode != MSWP_GOODMSDIR) && (*ErrCode != MSWP_GOODEXTMSDIR) && (*ErrCode != MSWP_EXTFORCEFORMATDIR) && (*ErrCode != MSWP_EXTFORCESTRIPDIR) && (*ErrCode != MSWP_EXTFORCETRUSTDIR)), Domain, AMSDel, AMSNameSep, &IsVacuous) != 0) { 
	if (IDpart) free(IDpart);
	if (*ErrCode != MSWP_CRAP) {
	    *ErrCode = MSWP_PROBABLYGOOD;
	    return(0);
	} else if (WasGoodGlobalAlias >= 0) {
	    *ErrCode = WasGoodGlobalAlias;
	    return(0);
	} else if (WpCode != MSWP_CRAP) {
	    *ErrCode = MSWP_TEMPFAIL;
	    return(0);
	} else {
	    return(mserrcode);
	}
    }
    debug(1,("LookupLocalName returns WpCode=%d, IsVacuous=%d\n", WpCode, IsVacuous));
    if (WasGoodGlobalAlias >= 0) {
	WpCode = WasGoodGlobalAlias;
	debug(1,("Using code %d from global alias.\n", WpCode));
    }
    if (IDpart) free(IDpart);
    if (UseWPOnly(ErrCode, &WpCode, IsVacuous)) {
	if (SawTempFail && (WpCode == MSWP_GOODUSER || WpCode == MSWP_GOODNETMAIL)) {
	    WpCode = MSWP_PROBABLYGOOD;
	}
	*ErrCode = WpCode;
	return(0);
    } else if (UseMSCodeOnly(ErrCode, &WpCode, IsVacuous)) {
	if (AddressAsPost != Addr->LocalPart && strcmp(AddressAsPost, Addr->LocalPart) != 0) {
/* Here when we have read the address from some .MS.DirectPost file.  We need to validate the .MS.DirectPost file address according to the cell in which we found that file, if for no other reason than to get that cell's name appended to the address. */
	    int NewVal, NewErr;
	    strncpy(SideBuf, AddressAsPost, SideBufLen);    /* Copy the new address text to SideBuf in case we don't overwrite it with local_RewriteAddress. */
	    if (recdepth < 13 && PostCell != NULL) {
		if (ULstrcmp(Domain, PostCell) == 0) HadHost = 1;
		else HadHost = CheckAMSDelivery(PostCell);
		if (HadHost < 0) if (CheckAMSValidationMask(PostCell) == vld_WPValid) HadHost = 1;
		if (HadHost < 0) {
		    AMS_RETURN_ERRCODE(EPROTONOSUPPORT, EIN_GETCELLFROMFILE, EVIA_REWRITEADDRESS);
		} else if (HadHost > 0) {
		    NewVal = local_RewriteAddress(AddressAsPost, SideBuf, SideBufLen, &NewErr, PostCell, recdepth+1, Addr);
		    if (NewVal != 0) return (NewVal);
		    /* *ErrCode = NewErr; ---Rewrite the address for the other cell (local_RewriteAddress stores it into SideBuf), but don't overwrite the classification-as-folder code. */
		    *ErrCode = MergeMSWPCodes(*ErrCode, NewErr);
		}
	    }
	}
	if (SawTempFail && *ErrCode != MSWP_CRAP) {
	    *ErrCode = MSWP_PROBABLYGOOD;
	}
	return(0);
    } else {
	/* Got a valid code back from both sources; must merge! */
	auto char MergeBuf[2000], NewBuf[1500], AddrPrint[1500];
	ADDRESS_COMMENT *Comment;

	strncpy(NewBuf, AddressAsPost, sizeof(NewBuf));
	if ((*ErrCode == MSWP_GOODMSDIR || *ErrCode == MSWP_GOODEXTMSDIR || (*ErrCode == MSWP_EXTFORCEFORMAT) || (*ErrCode == MSWP_EXTFORCESTRIP) || (*ErrCode == MSWP_EXTFORCETRUST) || *ErrCode == MSWP_PROBABLYGOOD) && (WpCode == MSWP_GOODUSER || WpCode == MSWP_GOODNETMAIL)) {
	    char *s;
	    int len;

	    /* special hack to handle advisor/advisor+advisor, etc... */

	    debug(1, ("Checking for special identity between '%s' and '%s'\n", NewBuf, InBuf));
	    s = strchr(NewBuf, '+');
	    if (!s) s = strchr(NewBuf, '#');
	    if (s) {
		char *comparison;

		comparison = Addr->LocalPart;
		len = s - NewBuf;
		if ((len == (strlen(s+1)) || *(s+len+1) == '@')
		&& !strncmp(NewBuf, s+1, len)
		&& !strncmp(NewBuf, comparison, len)
		&& (*(comparison+len) == '@' || *(comparison+len) == '+' || *(comparison+len) == '#' || *(comparison+len) == '\0')) {
		    *ErrCode = WpCode;
		    return(0);
		}
	    }
	}
	if (WpCode != MSWP_AMBIGUOUS && WpCode != MSWP_FUZZYAMBIGMATCH) {
	    errno = 0;
	    if (UnparseOneAddress(Addr, UP_SPACES_TO_DOTS, AddrPrint, sizeof(AddrPrint), "    ", 69) != PA_OK) {
		if (errno == 0) errno = ENOMEM;
		AMS_RETURN_ERRCODE(errno, EIN_UNPARSEADDR, EVIA_REWRITEADDRESS);
	    }
	    ReduceWhiteSpace(AddrPrint);
	    strcpy(MergeBuf, "(");
	    strcat(MergeBuf, StripWhiteEnds(AddrPrint));
	    strcat(MergeBuf, ", ");
	    strcat(MergeBuf, NewBuf);
	    strcat(MergeBuf, ")");
	    Comment = (ADDRESS_COMMENT *) malloc(sizeof(ADDRESS_COMMENT));
	    if (Comment == NULL) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    t = NewString(MergeBuf);
	    if (t == NULL) {
		free(Comment);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    }
	    Comment->Text = t;
	    Comment->Next = Addr->Comments;
	    Addr->Comments = Comment;
	} else {
	    Comment = Addr->Comments;
	    if (Comment->Text) {
		strcpy(MergeBuf, Comment->Text);
		t = &MergeBuf[strlen(MergeBuf)-1];
		strcpy(t, ", ");
	    } else {
		strcpy(MergeBuf, "(");
	    }
	    strcat(MergeBuf, NewBuf);
	    strcat(MergeBuf, ")");
	    t = NewString(MergeBuf);
	    if (!t) AMS_RETURN_ERRCODE(ENOMEM, EIN_HANDLEADDRESS, EVIA_REWRITEADDRESS);
	    if (Comment->Text) free(Comment->Text);
	    Comment->Text = t;
	}
	if (IsCertain && !SawTempFail) {
	    *ErrCode = MSWP_AMBIGUOUS;
	} else {
	    *ErrCode = MSWP_PROBABLYAMBIGUOUS;
	}
	return(0);
    }
}

static int local_RewriteAddress(old, new, newsize, ErrCode, Domain, recDepth, upAddr)
char *old, *new, *Domain;
int newsize, *ErrCode, recDepth;
PARSED_ADDRESS *upAddr;
{
    PARSED_ADDRESS *Addr, *ListHead;
    int dummy, Val;
    
    debug(1, ("local_RewriteAddress old %s newsize %d domain %s\n", old, newsize, Domain));
    *new = '\0';

    *ErrCode = MSWP_CRAP;
    if (ParseAddressList(old, &ListHead) != PA_OK) {
	AMS_RETURN_ERRCODE(EFAULT, EIN_PARSEADDR, EVIA_REWRITEADDRESS);
    }
    dummy = 0;
    Addr = SingleAddress(ListHead, &dummy);
    if (dummy != 1) {
	FreeAddressList(ListHead);
	AMS_RETURN_ERRCODE(EFAULT, EIN_PARAMCHECK, EVIA_REWRITEADDRESS);
    }
    Val = AddrRewrite(Addr, ErrCode, Domain, recDepth, new, newsize, upAddr);
    if (Val != 0) return Val;
    if (*new == '\0') {	/* If this had been set, it would be the alias result */
	errno = 0;
	if (UnparseAddressList(ListHead, UP_SPACES_TO_DOTS, new, newsize, "", "    ", 69, &dummy) != PA_OK) {
	    FreeAddressList(ListHead);
	    AMS_RETURN_ERRCODE(errno, EIN_UNPARSEADDR, EVIA_REWRITEADDRESS);
	}
    }
    FreeAddressList(ListHead);
    *ErrCode = ExternalForcingCode(new, *ErrCode);
    debug(1, ("local_RewriteAddress returns code %d, address %s\n", *ErrCode, new));
    return(0);
}

/* MS_RewriteAddress used to be a snapified routine.  The server still recognizes the snap op code for backward compatibility, but it is basically now an internal routine. */

MS_RewriteAddress(old, new, newsize, ErrCode)
char *old, *new;
int newsize, *ErrCode;
{
    int code;
    debug(1, ("MS_RewriteAddress old %s newsize %d\n", old, newsize));

    code = local_RewriteAddress(old, new, newsize, ErrCode, MyMailDomain, 0, NULL);
    return(code);
}

static int MSV_LastFileTime = -1;
static char MSV_LastFileName[MAXPATHLEN+1];
static PARSED_ADDRESS *InListHead = NULL;

MS_ValidateAndReplaceChunk(FileName, inaddr, outaddr, outaddrsize, which, outcode)
char *FileName, *inaddr; /* IN */
char *outaddr; /* OUT */
int outaddrsize, which; /* IN */
int *outcode; /* OUT */
{
    PARSED_ADDRESS *InAddrHead;
    struct stat stbuf;
    char *inlist, *outlist;
    int outlistsize, fd, errsave, dummy, len;

    debug(1, ("MS_ValidateAndReplaceChunk file %s inaddr %s which %d\n", FileName, inaddr, which));
    if (which < 0) {
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_VALCHUNK);
    }
    if (stat(FileName, &stbuf)) {
	AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_VALCHUNK);
    }
    if (stbuf.st_size <= 0) {
	AMS_RETURN_ERRCODE(EMSENDOFLIST, EIN_PARAMCHECK, EVIA_VALCHUNK);
    }
    if (stbuf.st_mtime != MSV_LastFileTime || strcmp(FileName, MSV_LastFileName)) {
	debug(1, ("Not cached, sorry... %d %d %s %s\n", stbuf.st_mtime, MSV_LastFileTime, FileName, MSV_LastFileName));
	if (InListHead) FreeAddressList(InListHead);
	InListHead = NULL;
	MSV_LastFileTime = -1;
	inlist = malloc(1+stbuf.st_size);
	if (!inlist) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_VALCHUNK);
	}
	fd = open(FileName, O_RDONLY, 0);
	if (fd<0) {
	    free(inlist);
	    AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_VALCHUNK);
	}
	if (read(fd, inlist, stbuf.st_size) != stbuf.st_size) {
	    errsave = errno;
	    close(fd);
	    free(inlist);
	    AMS_RETURN_ERRCODE(errsave, EIN_READ, EVIA_VALCHUNK);
	}
	close(fd);
	*(inlist + stbuf.st_size) = '\0';
	debug(1, ("In list is %s\n", inlist));
	if (ParseAddressList(inlist, &InListHead) != PA_OK) {
	    free(inlist);
	    AMS_RETURN_ERRCODE(EFAULT, EIN_PARSEADDR, EVIA_VALCHUNK);
	}
	free(inlist);
    } else {
	debug(1, ("Bingo, cached!\n"));
	MSV_LastFileTime = -1;
    }
    outlistsize = 4 * (stbuf.st_size + (inaddr ? strlen(inaddr) : 0)); /* Bogus, but should suffice */
    outlist = malloc(outlistsize);
    if (!outlist) {
	FreeAddressList(InListHead);
	InListHead = NULL;
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_VALCHUNK);
    }
    if (which > 0 && inaddr) {
	if (ParseAddressList(inaddr, &InAddrHead) != PA_OK) {
	    FreeAddressList(InListHead);
	    InListHead = NULL;
	    free(outlist);
	    AMS_RETURN_ERRCODE(EFAULT, EIN_PARSEADDR, EVIA_VALCHUNK);
	}
	if (ReplaceNthListElement(InListHead, InAddrHead, &which)) {
	    free(outlist);
	    FreeAddressList(InListHead);
	    InListHead = NULL;
	    FreeAddressList(InAddrHead);
	    return(mserrcode);
	}
	FreeAddressList(InAddrHead);
    } else {
	++which;
    }
    if (UnparseAddressList(InListHead, UP_SPACES_TO_DOTS, outlist, outlistsize, "", "    ", 69, &dummy)) {
	free(outlist);
	FreeAddressList(InListHead);
	InListHead = NULL;
	AMS_RETURN_ERRCODE(errno, EIN_UNPARSEADDR, EVIA_VALCHUNK);
    }
    fd = open(FileName, O_WRONLY | O_TRUNC | O_CREAT, 0600);
    if (fd < 0) {
	FreeAddressList(InListHead);
	InListHead = NULL;
	free(outlist);
	AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_VALCHUNK);
    }
    if (writeall(fd, outlist, strlen(outlist)) != strlen(outlist)) {
	FreeAddressList(InListHead);
	InListHead = NULL;
	free(outlist);
	AMS_RETURN_ERRCODE(errno, EIN_WRITE, EVIA_VALCHUNK);
    }
    free(outlist);
    if (vclose(fd)) {
	FreeAddressList(InListHead);
	InListHead = NULL;
	AMS_RETURN_ERRCODE(errno, EIN_VCLOSE, EVIA_VALCHUNK);
    }
    /* We have safely written the file, can use it next time,
	hence no longer need to free InListHead after here. */
    if (stat(FileName, &stbuf)) {
	FreeAddressList(InListHead);
	InListHead = NULL;
    } else {
	MSV_LastFileTime = stbuf.st_mtime;
	strcpy(MSV_LastFileName, FileName);
    }
    /* Using outlist as convenient scratchpad here */
    outlist = malloc(outlistsize);
    if (!outlist) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_VALCHUNK);
    }
    if (UnparseNthElement(InListHead, which, outlist, outlistsize, TRUE)) {
	free(outlist);
	return(mserrcode);
    }
    if (MS_RewriteAddress(outlist, outaddr, outaddrsize, outcode)) {
	free(outlist);
	return(mserrcode);
    }
    len = strlen(outaddr);
    if (len + 2 < outaddrsize) {
	char *s = outaddr + len + 1;
	*s++ = '\001';
	strncpy(s, outlist, outaddrsize - len - 3);
	outaddr[outaddrsize - 1] = '\0';
    }
    free(outlist);
    return(0);
}

static PARSED_ADDRESS *FindNthListElement(AddrList, which, howfar)
PARSED_ADDRESS *AddrList;
int which, *howfar;
{
    PARSED_ADDRESS *TempAddr;

    FOR_ALL_ADDRESSES(ThisAddr, AddrList, {
	switch (ThisAddr->Kind) {
	    case SIMPLE_ADDRESS:
		if (++*howfar >= which) {
		    return(ThisAddr);
		}
		break;
	    case GROUP_ADDRESS:
		TempAddr = FindNthListElement(ThisAddr->Members, which, howfar);
		if (TempAddr) {
		    return(TempAddr);
		}
		break;
	    default:
		break;
	}
    })
    return(NULL);
}


static int ReplaceNthListElement(BigList, ShortList, which)
PARSED_ADDRESS *BigList, *ShortList;
int *which;
{
    PARSED_ADDRESS *nthaddr, *nthpred, *nthsucc;
    int howfar = 0, numitems = 0;

    nthaddr = FindNthListElement(BigList, *which, &howfar);
    if (!nthaddr) {
	AMS_RETURN_ERRCODE(EMSENDOFLIST, EIN_PARAMCHECK, EVIA_VALCHUNK);
    }
    (void) SingleAddress(ShortList, &numitems);
    *which += numitems;

    nthpred = nthaddr->Prev;
    nthsucc = nthaddr->Next;

    nthpred->Next = ShortList->Next;
    nthpred->Next->Prev = nthpred;
    nthsucc->Prev = ShortList->Prev;
    nthsucc->Prev->Next = nthsucc;
    ShortList->Next = nthaddr;
    ShortList->Prev = nthaddr;
    nthaddr->Prev = ShortList;
    nthaddr->Next = ShortList;

    return(0);
}

static int UnparseNthElement(AddrList, which, Buf, size, StripComments)
PARSED_ADDRESS *AddrList;
int which, size, StripComments;
char *Buf;
{
    PARSED_ADDRESS *tempaddr;
    int howfar = 0;

    tempaddr = FindNthListElement(AddrList, which, &howfar);
    if (!tempaddr) {
	AMS_RETURN_ERRCODE(EMSENDOFLIST, EIN_PARAMCHECK, EVIA_VALCHUNK);
    }
    if (StripComments) StripExtraComments(tempaddr, NULL);
    errno = 0;
    if (UnparseOneAddress(tempaddr, UP_SPACES_TO_DOTS, Buf, size, "", "    ", 69) != PA_OK) {
	AMS_RETURN_ERRCODE(errno, EIN_UNPARSEADDR, EVIA_VALCHUNK);
    }
    return(0);
}

MS_WriteAllMatchesToFile(ambigname, FileName)
char *ambigname; /* passed in */
char *FileName; /* passed out */
{
    PARSED_ADDRESS *Addr, *SingAddr;
    int la_errcode, outType, wpCode, numitems, AMSDel, AMSNameSep, IsVacuous;
    char *IDpart, *PostID, *DomainToUse, *DomainCopy;
    FILE *fp;

    debug(1, ("Entering MS_WriteAllMatchesToFile %s\n", ambigname));
    if (ParseAddressList(ambigname, &Addr) != PA_OK) {
	AMS_RETURN_ERRCODE(EFAULT, EIN_PARSEADDR, EVIA_WRITEALLMATCHES);
    }
    numitems = 0;
    SingAddr = SingleAddress(Addr, &numitems);
    if (numitems != 1) {
	FreeAddressList(Addr);
	AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_WRITEALLMATCHES);
    }
    IDpart = PostID = NULL;
    if (SingAddr->Hosts == NULL || SingAddr->Hosts == SingAddr->Hosts->Prev) {
	DomainToUse = MyMailDomain;
    } else {
	DomainToUse = SingAddr->Hosts->Prev->Name;	/* pick the outermost name */
    }
    SetAMSDelNameSep(DomainToUse, &AMSDel, &AMSNameSep);
    if (AMSDel < 0 && ULstrcmp(DomainToUse, MyMailDomain) != 0) {
	if (CheckAMSValidationMask(DomainToUse) != vld_WPValid) {
	    FreeAddressList(Addr);
	    AMS_RETURN_ERRCODE(EPROTONOSUPPORT, EIN_GETCELLFROMFILE, EVIA_WRITEALLMATCHES);
	}
    }
/* At this point, the address is either in the local domain or in a cell using AMS delivery. */
    DomainCopy = NewString(DomainToUse);	/* don't let it disappear */
    if (DomainCopy == NULL) {
	FreeAddressList(Addr);
	AMS_RETURN_ERRCODE(ENOMEM, EIN_LAKIND, EVIA_WRITEALLMATCHES);
    }
    la_errcode = la_KindDomain(SingAddr, &outType, &IDpart, &PostID, DomainCopy);
    if (la_errcode != laerr_NoError) {
	switch(la_errcode) {
	    case laerr_OutOfMemory:
		FreeAddressList(Addr);
		free(DomainCopy);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_LAKIND, EVIA_WRITEALLMATCHES);
	    case laerr_SyntaxError:
	    case laerr_UnrecSpecial:
	    case laerr_BadSecond:
		FreeAddressList(Addr);
		free(DomainCopy);
		AMS_RETURN_ERRCODE(EMSBADLOCALSYNTAX, EIN_LAKIND, EVIA_WRITEALLMATCHES);
	    default:
		FreeAddressList(Addr);
		free(DomainCopy);
		AMS_RETURN_ERRCODE(EMSUNKNOWN, EIN_LAKIND, EVIA_WRITEALLMATCHES);
	}
    }
    if (LookupLocalName(SingAddr, outType, IDpart, PostID, &wpCode, -1, TRUE, DomainCopy, AMSDel, AMSNameSep, &IsVacuous) != 0) {
	FreeAddressList(Addr);
	free(DomainCopy);
	if (IDpart) free(IDpart);
	return(mserrcode);
    }
    if (IDpart) free(IDpart);
    GenTempName(FileName);
    fp = fopen(FileName, "w");
    if (!fp) {
	FreeAddressList(Addr);
	free(DomainCopy);
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_WRITEALLMATCHES);
    }
    fputs(ambigname, fp);
    fputs(" ", fp);
    fputs(SingAddr->Comments->Text, fp);
    FreeAddressList(Addr);
    free(DomainCopy);
    if (ferror(fp) || feof(fp)) {
	fclose(fp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_WRITEALLMATCHES);
    }
    if (vfclose(fp)) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_WRITEALLMATCHES);
    }
    return(0);
}

static int CheckGlobalAlias(name, code, Domain)
char *name, *Domain;
int *code;
{
/* Define the macro YELLOWPAGES_ENV in site.h to use YP aliases lookup before the default /usr/lib/aliases. */
/*  AMS_AliasesValidation must also be set to 1 (true) for global alias validation. */
    char LineBuf[1000], *s;
    FILE *fp;
    int Found;
#ifdef YELLOWPAGES_ENV
    static char *LocalYPDomain = NULL;
    char *alias;
    int Len;
#endif /* YELLOWPAGES_ENV */

    Found = 0;
#ifdef YELLOWPAGES_ENV
    if (LocalYPDomain == NULL) {
	errno = 0;
	if (yp_get_default_domain(&LocalYPDomain)) {
	    AMS_RETURN_ERRCODE(errno, EIN_GETCELLFROMWS, EVIA_REWRITEADDRESS);
	}
    }
    if (LocalYPDomain != NULL && *LocalYPDomain != '\0') {
	alias = NULL;
	if (yp_match(LocalYPDomain, "mail.aliases", name, strlen(name)+1, &alias, &Len) == 0) {
	    if (Len != 0) {
		free(alias);
		Found = 1;
	    }
	}
    }
#endif /* YELLOWPAGES_ENV */
    if (Found == 0) {
	errno = 0;
	fp = fopen(AMS_AliasFileName, "r");
	if (!fp) {
	    if (errno == 0) errno = ENOMEM;
	    if (errno == ENOENT) return(0); /* Not our problem if there ain't no alias file */
	    else AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_REWRITEADDRESS);
	}
	name = StripWhiteEnds(name);
	while (fgets(LineBuf, sizeof(LineBuf), fp)) {
	    if (LineBuf[0] == '#') continue;
	    s = strchr(LineBuf, ':');
	    if (!s) continue;
	    *s = '\0';
	    s = StripWhiteEnds(LineBuf);
	    if (!ULstrcmp(s, name)) {
		Found = 1;
		break;
	    }
	}
	fclose(fp);
    }
    if (Found != 0) {
	*code = (CheckAMSFmtOK(Domain) <= 0 ? MSWP_GOODNETMAIL : MSWP_GOODUSER);
	debug(16, ("CheckGlobalAlias: given %s/%s, returning %d\n", name, Domain, *code));
    }
    return(0);
}

static int ExternalForcingCode(ExtAddress, codein)
char *ExtAddress;
int codein;
{
    struct ForceExtDescriptor *fetmp;
    char *domainindex;

    debug(16, ("ExternalForcingCode: given %s, code %d\n", ExtAddress, codein));
    if (codein != MSWP_GOODNETMAIL && codein != MSWP_GOODEXTMSDIR && codein != MSWP_GOODUSER && codein != MSWP_GOODMSDIR) return(codein);
    if (RefreshAliasFile()) return (mserrcode);
    fetmp = ForceExtRoot;
    domainindex = strchr(ExtAddress, '@');
    while (fetmp) {
	/* Ideally, this shouldn't be a strcmp at all, but should be a general attempt to see if two addresses actually match.  If anyone gets ambitious enough to make it work that way, bear in mind that if the pattern starts with "@" it is supposed to match ANYTHING at that domain address. */
	if (!ULstrcmp(fetmp->addrpattern, (domainindex && (fetmp->addrpattern[0] == '@')) ? domainindex : ExtAddress)) {
	    if (codein == MSWP_GOODNETMAIL || codein == MSWP_GOODUSER) {
		debug(16, ("ExternalForcingCode: returning %d\n", fetmp->ForceMail));
		return(fetmp->ForceMail);
	    } else {
		debug(16, ("ExternalForcingCode: returning %d\n", fetmp->ForceDir));
		return(fetmp->ForceDir);
	    }
	}
	fetmp = fetmp->next;
    }
    debug(16, ("ExternalForcingCode: found no mapping (returning input=%d)\n", codein));
    return(codein);
}

MS_DomainHandlesFormatting(domname, codeP)
char *domname; int *codeP;
{/* Tell the caller whether users in the given domain accept ATK-formatted messages as a matter of course.  Set *codeP <0 for NO, >0 for YES, and 0 for can't-tell. */
    *codeP = CheckAMSFmtOK(domname);
    return (0);
}
