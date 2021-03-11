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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/disambig.c,v 2.25 1993/05/06 18:02:17 susan Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <util.h>
#include <ms.h>
#include <sys/stat.h>
#include <stdio.h>
#include <mail.h>

extern char *permanentmalloc();

char *SearchPath;
struct SearchPathElement SearchPathElements[MAXPATHELTS];
Boolean DidInit = FALSE;
int     MS_NumDirsInSearchPath;

#define DSABGCACHESIZE 1024 /* Power of 2 */
struct DsabgCacheEntry {
    int SearchPathIndex;
    unsigned folderHash;
    char *folder;
    struct DsabgCacheEntry *next;
};
static struct DsabgCacheEntry *DsabgCache[DSABGCACHESIZE];
static ReadSubsFile();
char *MS_LookupInDsabgCache();

/* This routine should be cleaned up to set error codes properly, and then
	the routines that call it should pass on its error codes */

MS_DisambiguateFile(source, target, AccessCode)
char *source, *target;
short AccessCode;
{
    int     i, RC;
    char   *tempname, *SlashPtr = NULL, possiblename[MAXPATHLEN + 1];
#ifdef AFS_ENV
    char *AtPtr;
#endif /* AFS_ENV */
    char *obracket, *cbracket;
    char CopyName[MAXPATHLEN+1], AtName[MAXPATHLEN+1];
    struct stat statbuf;
    struct cell_msPath *MSP;

    debug(1,("Disambiguating %s...\n", source));
    if (!DidInit) {		/* Do first-time-only initialization? */
	if ((i = InitializeSearchPaths()) != 0)
	    return(i);
    }
    strncpy(CopyName, source, sizeof(CopyName));
    AtName[0] = '\0';
#ifdef AFS_ENV
    AtPtr = strrchr(CopyName, '@');
    if (AtPtr != NULL) {
	*AtPtr++ = '\0';    /* terminate the first part */
	for (tempname = AtPtr; *tempname != '\0'; ++tempname)
	  if (*tempname == '/') *tempname = '.';  /* restore dots in domain name */
	strcpy(possiblename, "/afs/");
	LCappend(possiblename, AtPtr);
	i = readlink(possiblename, AtName, sizeof(AtName));
	if (i >= 0) {
	    AtName[i] = '\0';
	    AtPtr = strrchr(AtName, '/');
	    if (AtPtr != NULL) {
		strncpy(possiblename, ++AtPtr, sizeof(possiblename));
		strncpy(AtName, possiblename, sizeof(AtName));
	    }
	} else {
	    AtName[0] = '\0';
	    LCappend(AtName, AtPtr);
	}
	sprintf(possiblename, "/afs/%s", AtName);
	if (stat(possiblename, &statbuf) != 0) {
	    AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
	}
    }
#endif /* AFS_ENV */
    obracket = strchr(CopyName, '[');
    if (obracket) {
	cbracket = strrchr(++obracket, ']');
	if (cbracket) {
	    *cbracket++ = '\0';
	    for (i=0; i< MS_NumDirsInSearchPath; ++i) {
		if (SearchPathElements[i].label && !ULstrcmp(SearchPathElements[i].label, obracket)) break;
	    }
	    if (i < MS_NumDirsInSearchPath && AtName[0] != '\0') {
		if (GetCellFromFileName(SearchPathElements[i].Path, possiblename, sizeof(possiblename)) < 0) possiblename[0] = '\0';
	    }
	    if (i < MS_NumDirsInSearchPath && (AtName[0] == '\0' || possiblename[0] == '\0' || ULstrcmp(AtName, possiblename) == 0)) {
		strncpy(possiblename, SearchPathElements[i].Path, sizeof(possiblename)-1);
		strcat(possiblename, "/");
		strncat(possiblename, cbracket, sizeof(possiblename) - strlen(possiblename));
		strncpy(CopyName, possiblename, sizeof(CopyName));
	    } else {
		if (AtName[0] != '\0') {
		    RC = CheckAMSDfMSPath(AtName, &MSP);
		    i = RC;
		    if (RC > 0) for (i = 0; i < RC; ++i) {
			if (MSP[i].Abbrev != NULL && ULstrcmp(MSP[i].Abbrev, obracket) == 0) break;
		    }
		    if (i < RC) {
			strncpy(possiblename, MSP[i].RootDir, sizeof(possiblename)-1);
			strcat(possiblename, "/");
			strncat(possiblename, cbracket, sizeof(possiblename) - strlen(possiblename));
			strncpy(CopyName, possiblename, sizeof(CopyName));
		    } else {
			--cbracket;
			*cbracket = ']';
		    }
		} else {
		    --cbracket;
		    *cbracket = ']';
		}
	    }
	}
    }
    if (CopyName[0] == '~') {
	mserrcode = ResolveTildes(CopyName, &tempname, AtName);
	if (mserrcode)
	    return(mserrcode);
	if (AccessCode == AMS_DISAMB_FILEMAYBECREATED) {
	    SlashPtr = strrchr(tempname, '/');
	    if (SlashPtr) *SlashPtr = '\0';
	}
	debug(16,("Trying resolved tilde %s\n", tempname));
	errno = 0;		/* Just make sure */
	if (AccessCode == AMS_DISAMB_DIREXISTS) {
	    char kidname[1+MAXPATHLEN];

	    sprintf(kidname, "%s/%s", tempname, MS_DIRNAME);
	    if (stat(kidname, &statbuf)) {
		if (errno == ENOENT) {
		    /* If the MS_MsgDir doesn't exist, return ENOTDIR
		     * if the parent directory exists, otherwise return the
		     * error we get stat'ing the parent directory
		     */
		    if (stat(tempname, &statbuf) == 0) {
			free(tempname);
			AMS_RETURN_ERRCODE(ENOTDIR, EIN_ACCESS, EVIA_DISAMB);
		    }
		}
		free(tempname);
		AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
	    }
	} else if (stat(tempname, &statbuf) != 0) {
		free(tempname);
		AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
	    }
	if (AccessCode == AMS_DISAMB_ISAFILE || AccessCode == AMS_DISAMB_ISADIR) {
	    if (AccessCode == AMS_DISAMB_ISAFILE) {
		if ((statbuf.st_mode & S_IFMT) != S_IFREG) {
		    free(tempname);
		    AMS_RETURN_ERRCODE(EISDIR, EIN_ACCESS, EVIA_DISAMB);
		}
	    }
	    if (AccessCode == AMS_DISAMB_ISADIR) {
		if ((statbuf.st_mode & S_IFMT) != S_IFDIR) {
		    free(tempname);
		    AMS_RETURN_ERRCODE(ENOTDIR, EIN_ACCESS, EVIA_DISAMB);
		}
	    }
	}
	if (SlashPtr) *SlashPtr = '/';
	abspath(tempname, target);
				/* Depends on stable working directory */
	free(tempname);
	debug(16,("returning tilde resolved %s\n", target));
	return(0);
    }
    if (CopyName[0] == '/') {
	if (AccessCode == AMS_DISAMB_FILEMAYBECREATED) {
	    SlashPtr = strrchr(CopyName, '/');
	    if (SlashPtr) *SlashPtr = '\0';
	}
	if (AccessCode == AMS_DISAMB_DIREXISTS) {
	    char kidname[1+MAXPATHLEN];

	    sprintf(kidname, "%s/%s", CopyName, MS_DIRNAME);
	    if (stat(kidname, &statbuf)) {
		if (errno == ENOENT) {
		    /* If the MS_MsgDir doesn't exist, return ENOTDIR
		     * if the parent directory exists, otherwise return the
		     * error we get stat'ing the parent directory
		     */
		    if (stat(CopyName, &statbuf) == 0) {
			AMS_RETURN_ERRCODE(ENOTDIR, EIN_ACCESS, EVIA_DISAMB);
		    }
		}
		AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
	    }
	} else if (stat(CopyName, &statbuf) != 0) {
	    AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
	}
	if (AccessCode == AMS_DISAMB_ISAFILE || AccessCode == AMS_DISAMB_ISADIR) {
	    if (AccessCode == AMS_DISAMB_ISAFILE) {
		if ((statbuf.st_mode & S_IFMT) != S_IFREG) {
		    AMS_RETURN_ERRCODE(EISDIR, EIN_ACCESS, EVIA_DISAMB);
		}
	    }
	    if (AccessCode == AMS_DISAMB_ISADIR) {
		if ((statbuf.st_mode & S_IFMT) != S_IFDIR) {
		    AMS_RETURN_ERRCODE(ENOTDIR, EIN_ACCESS, EVIA_DISAMB);
		}
	    }
	}
	if (SlashPtr) *SlashPtr = '/';
	strcpy(possiblename, CopyName);
    }
    else {
	if (AccessCode == AMS_DISAMB_FILEEXISTS) {
	    AMS_RETURN_ERRCODE(ENOENT, EIN_ACCESS, EVIA_DISAMB);
	}
	if (AccessCode == AMS_DISAMB_ISAFILE || AccessCode == AMS_DISAMB_ISADIR) {
	    if (stat(CopyName, &statbuf) != 0) {
		AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
	    }
	    if (AccessCode == AMS_DISAMB_ISAFILE) {
		if ((statbuf.st_mode & S_IFMT) != S_IFREG) {
		    AMS_RETURN_ERRCODE(EISDIR, EIN_ACCESS, EVIA_DISAMB);
		}
	    }
	    if (AccessCode == AMS_DISAMB_ISADIR) {
		if ((statbuf.st_mode & S_IFMT) != S_IFDIR) {
		    AMS_RETURN_ERRCODE(ENOTDIR, EIN_ACCESS, EVIA_DISAMB);
		}
	    }
	    strcpy(possiblename, CopyName);
	} else if (AtName[0] == '\0' && (tempname = MS_LookupInDsabgCache(CopyName))) {
	    strcpy(possiblename, tempname);
	} else {
	    Boolean triedvalidating;
	    char *PathName, *outCell;
	    int LatestViceError = 0, LatestPermError = 0, GenIx, SawAny;

	    GenIx = 0; SawAny = FALSE;
	    while (GenMSPathElts(&GenIx, AtName, &PathName, &outCell, &i)) {
		if (AtName[0] != '\0' && outCell != NULL && outCell != ((char *) -1)) {    /* check cell match */
		    if (ULstrcmp(AtName, outCell) != 0) continue;
		}
		if (i >= 0 && !SearchPathElements[i].HasValidated) {
		    if (ValidateSearchPath(i)) return(mserrcode);
		    triedvalidating = TRUE;
		} else {
		    triedvalidating = FALSE;
		}
		if (i >= 0 && !SearchPathElements[i].HaveReadSubs) {
		    ReadSubsFile(i);
		    if (tempname = MS_LookupInDsabgCache(CopyName)) {
			strcpy(possiblename, tempname);
			SawAny = TRUE;
			break;
		    }
		}
		if (PathName) {
		    sprintf(possiblename, "%s/%s", PathName, CopyName);
		} else {
		    strcpy(possiblename, CopyName);
		}
		debug(16,("Trying %s\n", possiblename));
		errno = 0;
		if (AccessCode != AMS_DISAMB_DIREXISTS) {
		    if (stat(possiblename, &statbuf) == 0) {
			SawAny = TRUE;
			break;
		    }
		} else {
		    char kidname[1+MAXPATHLEN];

		    sprintf(kidname, "%s/%s", possiblename, MS_DIRNAME);
		    if (stat(kidname, &statbuf) == 0) {
			SawAny = TRUE;
			if (i >= 0) {
			    MS_AddToDsabgCache(CopyName, i);
			}
			break;
		    }
		}
		if (errno) {
		    if (vdown(errno)) {
			/* if it was a vice error and the validation routine did not
			    complain, we should complain here */
			LatestViceError = errno;
			if (!triedvalidating) {
			    auto char ErrorText[1000];

			    sprintf(ErrorText, "AFS/network error; can't check existence of %s", ap_Shorten(possiblename));
			    NonfatalBizarreError(ErrorText);
			}
		    } else if (errno == EACCES) {
			LatestPermError = errno;
			if (!triedvalidating) {
			    auto char ErrorText[1000];

			    sprintf(ErrorText, "Permission denied; can't check existence of %s", ap_Shorten(possiblename));
			    NonfatalBizarreError(ErrorText);
			}
		    } else if (errno != ENOENT)  {
			AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_DISAMB);
		    }
		}
	    }
	    if (!SawAny) {
		AMS_RETURN_ERRCODE(LatestViceError ? LatestViceError : (LatestPermError ? LatestPermError : ENOENT), EIN_PATHSEARCH, EVIA_DISAMB);
	    }
	}
    }
    abspath(possiblename, target);
				/* Depends on stable working directory */
    debug(16,("returning %s\n", target));
    return(0);
}

static unsigned hashfunc(s)
char *s;
{
    int c;
    unsigned int result = 0;

    /* aargh.  need better hash function */
    while (c = *s++) result += c;
    return result;
}

MS_AddToDsabgCache(folder, index)
char *folder;
int index;
{
    unsigned hashval = hashfunc(folder);
    struct DsabgCacheEntry *thisentry;
    struct DsabgCacheEntry **insertentry = &DsabgCache[hashval & (DSABGCACHESIZE-1)];

    while (thisentry = *insertentry) {
	if (hashval == thisentry->folderHash && !strcmp(folder, thisentry->folder)) {
	    break;
	}
	insertentry = &thisentry->next;
    }
    if (thisentry) {
	if (index < thisentry->SearchPathIndex) {
	    /* found a folder that shadows one already in the cache */
	    thisentry->SearchPathIndex = index;
	}
	return;
    }
    thisentry = (struct DsabgCacheEntry *) permanentmalloc(sizeof(struct DsabgCacheEntry));
    if (!thisentry) return;
    thisentry->folder = permanentmalloc(strlen(folder)+1);
    if (!thisentry->folder) return; /* No way to free thisentry */

    thisentry->SearchPathIndex = index;
    thisentry->folderHash = hashval;
    strcpy(thisentry->folder, folder);
    thisentry->next = 0;
    *insertentry = thisentry;
}

char *MS_LookupInDsabgCache(folder)
char *folder;
{
    static char retbuf[MAXPATHLEN + 1];
    unsigned hashval = hashfunc(folder);
    struct DsabgCacheEntry *thisentry = DsabgCache[hashval & (DSABGCACHESIZE-1)];

    for (;thisentry; thisentry = thisentry->next) {
	if (hashval == thisentry->folderHash && !strcmp(folder, thisentry->folder)) {
	    strcpy(retbuf, SearchPathElements[thisentry->SearchPathIndex].Path);
	    strcat(retbuf, "/");
	    strcat(retbuf, folder);
	    return retbuf;
	}
    }
    return 0;
}

static ReadSubsFile(idx)
int idx;
{
    char MapFileName[MAXPATHLEN + 1];
    char LineBuf[2 * MAXPATHLEN];
    char *p;
    FILE *fp;

    strcpy(MapFileName, SearchPathElements[idx].Path);
    strcat(MapFileName, "/");
    strcat(MapFileName, AMS_SUBSCRIPTIONMAPFILE);

    if ((fp = fopen(MapFileName, "r")) == NULL) {
	/* report NonFatalBizarreError? */
	return;
    }
    while (fgets(LineBuf, sizeof(LineBuf), fp) != NULL) {
	p = strchr(LineBuf, ':');
	if (!p) continue;

	*p = '\0';
	for (p = LineBuf; *p != '\0'; ++p)
	  if (*p == '.') *p = '/';  /* convert dots to slashes */
	
	MS_AddToDsabgCache(LineBuf, idx);
    }
    fclose(fp);
    SearchPathElements[idx].HaveReadSubs = 1;
}

	    
	  
    

