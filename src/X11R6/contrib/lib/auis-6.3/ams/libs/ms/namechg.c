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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/namechg.c,v 2.22 1993/05/05 19:49:43 susan Exp $";
#endif

#include <mailconf.h>
#include <ms.h>
#include <stdio.h>
#include <andrewos.h>

extern char home[], *fixDate();
extern int NumSubsInUse;
extern struct SubscriptionProfile **SubsInPathOrder, *SubsInUserOrder;

/* cn0h 9/19/91 -- import from muclient.c: */
extern void mu_quickcheck();

/* mustopen and mustfopen

These routines are used to get around a well-understood but hard-to-fix Vice problem. 
Rename operations on Vice are not really atomic as seen from the workstation.  Thus in
certain locking algorithms, a file can temporarily disappear.  Using this instead 
of open should greatly reduce the frequency of the bugs that result.

*/

mustopen(path, flags, mode)
char *path;
int flags, mode;
{
    int numtries = 5, code;

    while (1) {
	code = open(path, flags, mode);
        if (code >= 0 || errno != ENOENT || numtries-- < 0) return(code);
    }
}


FILE *
mustfopen(fname, ftype)
char *fname, *ftype;
{
    int numtries = 5;
    FILE *fp;

    while (1) {
	fp = fopen(fname, ftype);
        if (fp || errno != ENOENT || numtries-- < 0) return(fp);
    }
}

MS_NameChangedMapFile(MapFile, MailOnly, ListAll, NumChanged, NumUnavailable, NumMissingFolders, NumSlowpokes, NumFastFellas)
char *MapFile; /* passed out */
int MailOnly; /* passed in */
int ListAll;  /* Passed in */
int *NumChanged; /* Passed out */
int *NumUnavailable; /* Passed out */
int *NumMissingFolders; /* ditto */
int *NumSlowpokes; /* ditto */
int *NumFastFellas; /* ditto */
{
    int i, subdiff, mailpathelt = 0, patheltinuse = -1, unavail = 0, code = 0;
    FILE *fp = NULL, *outfp;
    char MupFileName[1+MAXPATHLEN];
    char UpdLine[100+MAXPATHLEN], *auxstuff = NULL, *s;

    GenTempName(MapFile);
    if (MakeSubsListInPathOrder()) {
	unlink(MapFile);
	return(mserrcode);
    }
    *NumChanged = 0;
    *NumUnavailable = 0;
    *NumMissingFolders = 0;
    *NumSlowpokes = 0;
    *NumFastFellas = 0;
    UpdLine[0] = '\0';
    if (MailOnly) {
	char MailPath[1+MAXPATHLEN];

	sprintf(MailPath, "%s%s", home, MAILSEARCHPATHTEMPLATE);
	for (i=0; i<MS_NumDirsInSearchPath; ++i) {
	    if (!strcmp(MailPath, SearchPathElements[i].Path)) {
		mailpathelt = i;
		break;
	    }
	}
    }
    mu_quickcheck(SubsInPathOrder, NumSubsInUse);
    for (i=0; i<NumSubsInUse; ++i) {
	debug(2048, ("Processing subs entry %s status %d\n", SubsInPathOrder[i]->key, SubsInPathOrder[i]->status));
	/* the HasChanged field is now initialized in mu_quickcheck(): */
	if (SubsInPathOrder[i]->HasChanged != 0) {
	    ++*NumFastFellas;
	    if (--SubsInPathOrder[i]->HasChanged) ++*NumChanged;
	    continue;
	}
	if (SubsInPathOrder[i]->status == AMS_UNSUBSCRIBED) continue;
	if (MailOnly && SubsInPathOrder[i]->pathelt != mailpathelt) continue;
	if (patheltinuse != SubsInPathOrder[i]->pathelt) {

	    /* Open a new master update file */
	    unavail = 0;
	    patheltinuse = SubsInPathOrder[i]->pathelt;
	    if ((patheltinuse >= 0) && (patheltinuse < MS_NumDirsInSearchPath) && !SearchPathElements[patheltinuse].HasValidated) {
		ValidateSearchPath(patheltinuse); /* ignore errors here */
	    }
	    if (fp) {
		fclose(fp);
		ViceFlushPlusParent(MupFileName);
	    }
	    if ((patheltinuse >= 0) && (patheltinuse < MS_NumDirsInSearchPath)) {
		sprintf(MupFileName, "%s/%s", SearchPathElements[patheltinuse].Path, MS_MASTERUPDATE);
		debug(2048, ("Trying to open %s\n", MupFileName));
		fp = mustfopen(MupFileName, "r");
	    } else {
		fp = NULL;
		errno = ENOENT; /* Pretend we tried the open */
	    }
	    if (!fp) {
		if (errno == EACCES || vdown(errno)) {
		    unavail = 1;
		} else if (errno != ENOENT) {
		    unlink(MapFile);
		    AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_WRITECHANGEDSUBS);
		}
		UpdLine[0] = '\0';
	    } else {
		if (fgets(UpdLine, sizeof(UpdLine), fp)) {
		    auxstuff = strchr(UpdLine, ' ');
		    if (auxstuff) {*auxstuff++ = '\0'; fixDate(auxstuff);}
		} else {
		    fclose(fp);
		    ViceFlushPlusParent(MupFileName);
		    fp = NULL;
		}
	    }
	}
#ifdef OBSOLETE
	if (/* SubsInPathOrder[i]->status == AMS_ASKSUBSCRIBED
	|| */ SubsInPathOrder[i]->status == AMS_SHOWALLSUBSCRIBED) {
	    SubsInPathOrder[i]->HasChanged = 1;
	    ++*NumChanged;
	    ++*NumFastFellas;
	    continue;
	}
#endif /* OBSOLETE */
	/* find out about SubsInPathOrder[i]->*/
	debug(2048, ("Going into do loop\n"));
	if (fp) do {
	    code = PreorderSubscriptionStrcmp(SubsInPathOrder[i]->key, UpdLine);
	    debug(2048, ("compared %s to %s; code is %d\n", SubsInPathOrder[i]->key, UpdLine, code));
	    if (code>0) {
		if (fgets(UpdLine, sizeof(UpdLine), fp)) {
		    auxstuff = strchr(UpdLine, ' ');
		    if (auxstuff) {*auxstuff++ = '\0'; fixDate(auxstuff);}
		} else {
		    fclose(fp);
		    ViceFlushPlusParent(MupFileName);
		    fp = NULL;
		    break;
		}
	    }
	} while (code > 0);
	if (!fp || code < 0) {
	    if (unavail) {
		++*NumUnavailable;
	    } else {
		int new, tot;
		char Dbuf[AMS_DATESIZE];

		debug(2048, ("Slowpoke: %s\n", SubsInPathOrder[i]->key));
		++*NumSlowpokes;
		/* LOOK UP CHANGES THE SLOW WAY HERE */
		mserrcode = MS_GetNewMessageCount(SubsInPathOrder[i]->key, &new, &tot, Dbuf, (SubsInPathOrder[i]->status == AMS_SHOWALLSUBSCRIBED) ? 1 : 0);
		/* In the previous line, we have to get the tot variable right only for show-all subscribed things. */
		if (mserrcode) {
		    if (AMS_ERRNO == ENOENT) {
			++*NumMissingFolders;
			new = 1; /* Show it to them */
		    } else if (AMS_ERRNO == EACCES || vdown(AMS_ERRNO)|| (AMS_ERRNO == EMSBADDIRFORMAT)) {
			++*NumUnavailable;
			continue;
		    } else {
			if (fp) {
			    fclose(fp);
			    ViceFlushPlusParent(MupFileName);
			}
			unlink(MapFile);
			return(mserrcode);
		    }
		}
		if (fp && AMS_ERRNO != ENOENT) {
		    char ErrorText[MAXPATHLEN+100];

		    debug(2048, ("..even though there IS a master update file!\n"));
		    sprintf(ErrorText, "%s is not in the master update file.", ap_Shorten(SubsInPathOrder[i]->key));
		    NonfatalBizarreError(ErrorText);
		}
		if ((new > 0) || (tot > 0 && (SubsInPathOrder[i]->status == AMS_SHOWALLSUBSCRIBED))) {
		    debug(2048, ("It has CHANGED!\n"));
		    SubsInPathOrder[i]->HasChanged = 1;
		    ++*NumChanged;
		}
	    }
	} else {
	    debug(2048,  ("Bingo!  key is %s mupname %s auxstuff %s\n", SubsInPathOrder[i]->key, UpdLine, auxstuff));
	    ++*NumFastFellas;
	    s = strchr(auxstuff, ' ');
	    if (s) *s = '\0';
	    subdiff = strcmp(auxstuff, "000000") ?
		strcmp(auxstuff, SubsInPathOrder[i]->time64) : 0;
	    if (subdiff < 0) FixSubsDate(SubsInPathOrder[i], auxstuff);
	    if (SubsInPathOrder[i]->status == AMS_SHOWALLSUBSCRIBED || subdiff > 0) {
		debug(2048, ("It has CHANGED!  profile says %s\n", SubsInPathOrder[i]->time64));
		SubsInPathOrder[i]->HasChanged = 1;
		++*NumChanged;
	    }
	    if (fgets(UpdLine, sizeof(UpdLine), fp)) {
		auxstuff = strchr(UpdLine, ' ');
		if (auxstuff) {*auxstuff++ = '\0'; fixDate(auxstuff);}
	    } else {
		fclose(fp);
		ViceFlushPlusParent(MupFileName);
		fp = NULL;
	    }
	}
    }
    if (fp) {
	fclose(fp);
	ViceFlushPlusParent(MupFileName);
    }
    outfp = fopen(MapFile, "w");
    if (!outfp) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_WRITECHANGEDSUBS);
    }
    for (i=0; i<NumSubsInUse; ++i) {
	if (MailOnly && SubsInUserOrder[i].pathelt != mailpathelt) continue;
	if (SubsInUserOrder[i].status == AMS_UNSUBSCRIBED) continue;

	/* prime the disambig.c cache */
	for (s = SubsInUserOrder[i].sname; *s != '\0'; ++s)
	    if (*s == '.') *s = '/';
	if (SubsInUserOrder[i].pathelt >= 0) {
	    MS_AddToDsabgCache(SubsInUserOrder[i].sname, SubsInUserOrder[i].pathelt);
	}
	for (s = SubsInUserOrder[i].sname; *s != '\0'; ++s)
	    if (*s == '/') *s = '.';
	
	if (ListAll || SubsInUserOrder[i].HasChanged || (SubsInUserOrder[i].status == AMS_SHOWALLSUBSCRIBED)) {
	    fprintf(outfp, "%s:%s %d %d\n", SubsInUserOrder[i].sname, SubsInUserOrder[i].key, SubsInUserOrder[i].status, SubsInUserOrder[i].HasChanged ? 1 : 0);
	}
    }
    if (ferror(outfp) || feof(outfp)) {
	fclose(outfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_WRITECHANGEDSUBS);
    }
    if (vfclose(outfp)) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_WRITECHANGEDSUBS);
    }
    return(0);
}

