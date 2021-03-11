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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/subs.c,v 2.44 1993/05/05 19:49:43 susan Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <ms.h>
#include <mailconf.h>
#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#include <util.h>

extern char *StripWhiteEnds(), *getprofile(), *convlongto64(), *fixDate();
extern char home[], MyMailDomain[];
extern int NeedToTimeOut;

#define SUBSALLOCATIONCHUNK 50

int SubscriptionsAreDirty = FALSE, NumSubsInUse = 0;
struct SubscriptionProfile *SubsInUserOrder = NULL, **SubsInPathOrder = NULL;

static struct subsorderelt {
    int iswild;
    char *pattern;
    int pathelt;
} *SubsOrderElts = NULL;

static char *RawSubs = NULL, *SubsOrderToFree = NULL;
static int NumSubsAllocated = 0, LastSubsModTime = 0, NumSubsOrderElts = 0, NumSubsOrderAllocated = 0, HasInitializedSubsPriorities = 0, SubsModCtr = 0;
static char *oldmail = NULL, *oldlocal = NULL, *oldexternal = NULL, *oldofficial = NULL;
static Boolean PathsHaveChanged = FALSE;

#define FIND_MUSTFIND 0
#define FIND_MUSTFINDNEXT 1
#define FIND_FINDPROPERPLACE 2

int CompareSubsPtrPriority();

FindSubsEntry(name, findcode)
char *name;
int findcode; /* one of FIND_ codes defined above */
{
    int i, top, bottom, split, code, splen;
    static struct SubscriptionProfile subs;
    static int LastHit = -1;
    char Nick[1+MAXPATHLEN], Full1[1+MAXPATHLEN], Full2[1+MAXPATHLEN];

    debug(1, ("FindSubsEntry %s\n", name));
    if (ReadSubs() != 0) {
	return(-1);		/* error code set */
    }
    if (*name == '\0' && (findcode == FIND_MUSTFINDNEXT)) {
	return(0);		/* Give the first entry on null request for next */
    }
    if (SubsInUserOrder == NULL) {
	return(NumSubsInUse);
    }
    if (LastHit >= 0) {		/* Pander to most common access patterns */
	if (!strcmp(name, SubsInUserOrder[LastHit].key)) {
	    return((findcode == FIND_MUSTFINDNEXT) ? LastHit + 1 : LastHit);
	}
	if (((LastHit + 1) < NumSubsInUse) && !strcmp(name, SubsInUserOrder[1+LastHit].key)) {
	    ++LastHit;
	    return((findcode == FIND_MUSTFINDNEXT) ? LastHit + 1 :LastHit);
	}
    }
    subs.key = name;
    subs.pathelt = -1;
    for (i=0; i<MS_NumDirsInSearchPath; ++i) {
	splen = strlen(SearchPathElements[i].Path);
	if (!strncmp(name, SearchPathElements[i].Path, splen) && name[splen] == '/') {
	    subs.pathelt = i;
	    break;
	}
    }
    BuildNickName(subs.key, Nick);
    subs.sname = Nick;
    subs.priority = ComputeSubsPriority(subs.sname, subs.pathelt);
    top = NumSubsInUse - 1;
    bottom = 0;
    split = top/2;
    debug(16, ("Entering main comparison loop looking for %s nickname %s path %d priority %d\n", name, subs.sname, subs.pathelt, subs.priority));
    while (TRUE) {
	debug(16, ("comparing with %s\n", SubsInUserOrder[split].key));
	code = CompareSubsPtrPriority(&subs, &SubsInUserOrder[split]);
	if (code == 0) break;	/* found it */
	if (code > 0) {
	    debug(16, ("Too low, check upper half\n"));
	    bottom = split +1;
	} else {
	    debug(16, ("Too high, check lower half\n"));
	    top = split -1;
	    if (top<0) {
		split = 0;
		break;
	    }
	}
	split = bottom + (top-bottom)/2;
	if (split > top) split = top;
	if (bottom >= top) { 
	    code = CompareSubsPtrPriority(&subs, &SubsInUserOrder[split]);
	    break;
	}
    }
    if (code) {
	if (findcode == FIND_MUSTFIND) {
	    return(NumSubsInUse);
	}
	/* This is the case for either FIND_MUSTFINDNEXT or FIND_FINDPROPERPLACE */
	if (code < 0) {
	    return(split);
	} else {
	    return(split+1);
	}
    }
    debug(16, ("Found it!\n"));
    LastHit = split;
    return((findcode == FIND_MUSTFINDNEXT) ? split+1 : split);
}

static char *SubsFileName = NULL;
static int GetSubsFileName()
{/* Find the absolute path to the .AMS.prof file so we can update it properly. */
    char Copy1[MAXPATHLEN+1], Copy2[MAXPATHLEN+1];
    char *cp;

    if (SubsFileName == NULL) {
	if ((mserrcode = ResolveTildes(AMS_SUBSPROFFILE, &SubsFileName, MyMailDomain) != 0)) {
	    return(mserrcode);
	}
	(void) DeSymLink(SubsFileName, Copy1, 1);
	if (abspath(Copy1, Copy2) != 0) strcpy(Copy2, Copy1);
	if (strcmp(SubsFileName, Copy2) != 0) {
	    cp = NewString(Copy2);
	    if (cp != NULL) {
		free(SubsFileName);
		SubsFileName = cp;
	    }
	}
    }
    mserrcode = 0;
    return(0);
}

WriteSubs()
{
    FILE *fp;
    struct stat statbuf;
    char InProgressSubsFile[MAXPATHLEN+1], ErrTxt[MAXPATHLEN+1];
    int i, Recovered;
    static int HasBackedUp = 0;

    debug(1, ("WriteSubs\n"));
    if (LockProfile()) return(mserrcode);
    if (SubsFileName == NULL) {
	if (GetSubsFileName() != 0) return(mserrcode);
    }
    if (!HasBackedUp) {
	char Old[1+MAXPATHLEN], Older[1+MAXPATHLEN], ErrText[50+MAXPATHLEN];
	strcpy(Old, SubsFileName);
	strcat(Old, ".OLD");
	if (stat(Old, &statbuf) != 0) {
	    if (errno != ENOENT && !vdown(errno)) {
		NonfatalBizarreError("Couldn't stat your old profile; no backup being made.");
	    }
	} else {
#define BACKUPINTERVAL 60*60*24
	    if ((time(0) - statbuf.st_mtime) >= BACKUPINTERVAL) {
		strcpy(Older, SubsFileName);
		strcat(Older, ".OLDR");
		unlink(Older);
		if (link(Old, Older) != 0) {
		    if (errno != ENOENT) {
			sprintf(ErrText, "Link failed for %s (%d)\n", ap_Shorten(Older), errno);
			NonfatalBizarreError(ErrText);
		    }
		}
		unlink(Old);
		if (link(SubsFileName, Old) != 0) {
		    if (errno != ENOENT) {
			sprintf(ErrText, "Link failed for %s (%d)\n", ap_Shorten(Old), errno);
			NonfatalBizarreError(ErrText);
		    }
		}
	    } else {
		debug(16, ("TOO SOON TO DO ANOTHER BACKUP -- %d - %d = %d seconds!\n", time(0), statbuf.st_mtime, (time(0) - statbuf.st_mtime)));
	    }
	}
	HasBackedUp = 1;
    }
    sprintf(InProgressSubsFile, "%s.NEW", SubsFileName);
    debug(16, ("Writing out Subs %s\n", InProgressSubsFile));
    if ((fp=fopen(InProgressSubsFile, "w")) == NULL) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_WRITESUBS);
    }
    fprintf(fp, "$mail %s/%s\n", home, MS_TREEROOT);
    fprintf(fp, "$official %s\n", OFFICIALSEARCHPATHTEMPLATE);
    fprintf(fp, "$local %s\n", LOCALSEARCHPATHTEMPLATE);
    fprintf(fp, "$external %s\n", EXTERNALSEARCHPATHTEMPLATE);
    for (i=0; i<NumSubsInUse; ++i) {
	if (SubsInUserOrder[i].sname != NULL) {
	    if (SubsInUserOrder[i].sname[0] == '\0') {
		sprintf(ErrTxt, "%s/%s/mail", home, MS_TREEROOT);
		if (strcmp(ErrTxt, SubsInUserOrder[i].key) == 0) {
		    fprintf(fp, "mail %s %d %s %d\n", "mail", ErrTxt, SubsInUserOrder[i].status, SubsInUserOrder[i].time64, SubsInUserOrder[i].filedate);
		    Recovered = 1;
		} else Recovered = 0;
		sprintf(ErrTxt, "Null subscription name for ``%s''--%s", ap_Shorten(SubsInUserOrder[i].key), Recovered ? "recovered as ``mail''" : "NOT RECORDED");
		NonfatalBizarreError(ErrTxt);
	    } else {
		fprintf(fp, "%s %s %d %s %d\n", SubsInUserOrder[i].sname, SubsInUserOrder[i].key, SubsInUserOrder[i].status, SubsInUserOrder[i].time64, SubsInUserOrder[i].filedate);
	    }
	}
    }
    if (ferror(fp) || feof(fp)) {
	fclose(fp);
	AMS_RETURN_ERRCODE(EMSFILEERR, EIN_FERROR, EVIA_WRITESUBS);
    }
    if (vfclose(fp) != 0) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_WRITESUBS);
    }
    if (RenameEvenInVice(InProgressSubsFile, SubsFileName)) {
	AMS_RETURN_ERRCODE(errno, EIN_RENAME, EVIA_WRITESUBS);
    }
    if (stat(SubsFileName, &statbuf) != 0) {
	LastSubsModTime = 0;
	AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_WRITESUBS);
    }
    LastSubsModTime = statbuf.st_mtime;
    UnlockProfile();
    SubscriptionsAreDirty = FALSE;
    return(0);
}

/* Routine used to fix future dates in AMS profiles -- added 12/12/91 by cn0h
 */
FixSubsDate(sub, time64)
    struct SubscriptionProfile *sub;
    char *time64;
{
    char tomorrow[AMS_DATESIZE];

    strncpy(tomorrow, convlongto64(time(0) + (60 * 60 * 24), 0), AMS_DATESIZE);
    if (strncmp(sub->time64, tomorrow, AMS_DATESIZE) > 0) {
	(void) strncpy(sub->time64, time64, AMS_DATESIZE);
	SubscriptionsAreDirty = TRUE;
    }
}

RefreshSubs() {
    if (SubscriptionsAreDirty) {
	if (WriteSubs()) {
	    NonfatalBizarreError("Could not write profile before refreshing it; some profile information may have been lost");
	}
    }
    while (NumSubsInUse-- > 0) {
	if (SubsInUserOrder[NumSubsInUse].NeedsFreed) {
	    free(SubsInUserOrder[NumSubsInUse].key);
	    free(SubsInUserOrder[NumSubsInUse].sname);
	}
    }
    if (RawSubs) {
	free(RawSubs);
	RawSubs = NULL;
    }
    NumSubsInUse = 0;
    LastSubsModTime = 0;
    NumSubsOrderElts = 0;
    if (SubsOrderToFree) {
	free(SubsOrderToFree);
	SubsOrderToFree = NULL;
    }
    HasInitializedSubsPriorities = 0;
    ++SubsModCtr;
    return(0);
}

ReadSubs() 
{
    int fd, i, mistakes;
    char *s, *nextline, *space, ErrorText[1000], AncientDate[AMS_DATESIZE+1];
    struct stat statbuf;
    struct SubscriptionProfile *sub;
    Boolean GoodParse;
    Boolean HasCheckedPathChanges = FALSE;

    debug(1, ("ReadSubs\n"));

    if (SubscriptionsAreDirty) return(0);
    if (SubsFileName == NULL) {
	if (GetSubsFileName() != 0) return(mserrcode);
    }
    debug(16, ("Subs file is %s\n", SubsFileName));
    CheckForInconsistentSubscriptions(SubsFileName);
    if (stat(SubsFileName, &statbuf) != 0) {
	if (errno == ENOENT) {
	    int foundct;

	    SubscriptionsAreDirty = 1; /* Inihibit infinite recursion */
	    if (CheckForOldFashionedSubscriptions(&foundct)) return(mserrcode);
	    if (foundct <= 0) {
		SubscriptionsAreDirty = 0;
	    }
	    if (stat(SubsFileName, &statbuf) != 0) {
		if (errno == ENOENT) return(0);
		AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_READSUBS);
	    }
	} else {
	    AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_READSUBS);
	}
    }
    if (statbuf.st_mtime == LastSubsModTime) {
	return(0);
    }
    ++SubsModCtr;
    if (RawSubs) {
	RawSubs = realloc(RawSubs, statbuf.st_size+1);
    } else {
	RawSubs = malloc(statbuf.st_size + 1);
    }
    if (!RawSubs) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READSUBS);
    }
    strncpy(AncientDate, convlongto64(time(0) - (60 * 60 * 24 * 45), 0), AMS_DATESIZE); /* 45 days ago */
    if ((fd=open(SubsFileName, O_RDONLY, 0)) < 0) {
	free(RawSubs);
	RawSubs = NULL;
	AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_READSUBS);
    }
    if (read(fd, RawSubs, statbuf.st_size) != statbuf.st_size) {
	close(fd);
	free(RawSubs);
	RawSubs = NULL;
	AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_READSUBS);
    }
    close(fd);
    RawSubs[statbuf.st_size] = '\0';
    debug(16, ("Parsing new raw Subscription\n"));
    s=RawSubs;
    if (!SubsInUserOrder) {
        SubsInUserOrder = (struct SubscriptionProfile *) malloc(sizeof(struct SubscriptionProfile) * SUBSALLOCATIONCHUNK);
	if (!SubsInUserOrder) {
	    free(RawSubs);
	    RawSubs = NULL;
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READSUBS);
	}
	NumSubsAllocated = SUBSALLOCATIONCHUNK;
    } else {
	while (NumSubsInUse-- > 0) {
	    if (SubsInUserOrder[NumSubsInUse].NeedsFreed) {
		free(SubsInUserOrder[NumSubsInUse].key);
		free(SubsInUserOrder[NumSubsInUse].sname);
	    }
	}
    }
    NumSubsInUse = 0;
    while(*s) {
	nextline = strchr(s, '\n');
	if (nextline) *nextline++ = '\0';
	if (*s == '#') {
	    /* skip comment lines, but discard them */
	    s = nextline;
	    continue;
	}
	if (*s == '$') {
	    /* Special state information -- should always be at beginning */
	    HandleSpecialProfileLine(s);
	    s = nextline;
	    continue;
	}
	if (!HasCheckedPathChanges) {
	    /* Has to come AFTER all special state information was read */
	    CheckPathChanges();
	    HasCheckedPathChanges = TRUE;
	}
	sub = &SubsInUserOrder[NumSubsInUse];
	sub->NeedsFreed = 0;
	GoodParse = FALSE;
	mistakes = BadLine(s, "AMS profile");
	sub->sname = s;
	if (mistakes == 0) {
	    space = strchr(s, ' ');
	    if (space) {
		*space++ = '\0';
		sub->key = space;
		space = strchr(space, ' ');
		if (space) {
		    *space++ = '\0';
		    s = strchr(space, ' ');
		    if (s) {
			*s++ = '\0';
			sub->status = atoi(space);
			space = strchr(s, ' ');
			if (space) {
			    *space++ = '\0';
			    strncpy(sub->time64, fixDate(s), AMS_DATESIZE);
				s = strchr(space, ' ');
				if (s) *s = '\0'; /* forward compatibility */
				sub->filedate = atoi(space);
			    GoodParse = TRUE;
			}
		    }
		}
	    }
	}
	if (PathsHaveChanged) {
	    HandlePathChange(sub);
	}
	if (!GoodParse) {
	    sprintf(ErrorText, "Ignoring an illegal profile line: %s", sub->sname);
	    NonfatalBizarreError(ErrorText);
	    s = nextline;
	    continue;
	}
	if ((sub->status == AMS_UNSUBSCRIBED) && (strncmp(sub->time64, AncientDate, AMS_DATESIZE) < 0)) {
	    debug(16, ("Profile line for %s is out-of-date; discarding it.\n", sub->sname));
	    s = nextline;
	    continue;
	}
	sub->pathelt = -2;
	do {
	    char sname[MAXPATHLEN+1], fixedpath[MAXPATHLEN+1];
	    char *src, *dst;
	    int splen;

	    /* get short name with dots */
	    dst = sname;
	    for (src = sub->sname; *dst = *src; ++src, ++dst) {
		if (*dst == '.') *dst = '/';
	    }

	    for (i=0; i<MS_NumDirsInSearchPath; ++i) {
		splen = strlen(SearchPathElements[i].Path);
		if (!strncmp(sub->key, SearchPathElements[i].Path, splen) &&
		    sub->key[splen] == '/') {
		    sub->pathelt = i;
		    /* make sure path matches short name -- added cn0h 3/8/92 */
		    if (strcmp(sname, sub->key + splen + 1) != 0) {
			sub->pathelt = -2; /* lie so we fall through to path fixer */
		    }
		    break;
		}
	    }

	    if (sub->pathelt != -2) break;

	    /* here we try to fix bad path elements -- added cn0h 12/12/91 */
	    sub->pathelt = -1;
	    if (MS_DisambiguateFile(sname, fixedpath, AMS_DISAMB_DIREXISTS) != 0 || !strcmp(fixedpath, sub->key)) {
		break;
	    }
	    SubscriptionsAreDirty = TRUE;
	    if (sub->NeedsFreed) {
		free(sub->key);
	    } else {
		dst = malloc(1 + strlen(sub->sname));
		if (dst == (char *) NULL) {
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READSUBS);
		}
		(void) strcpy(dst, sub->sname);
		sub->sname = dst;
		sub->NeedsFreed = TRUE;
	    }
	    sub->key = malloc(1 + strlen(fixedpath));
	    if (sub->key == (char *) NULL) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READSUBS);
	    }
	    strcpy(sub->key, fixedpath);
	} while (sub->pathelt < 0);
	sub->priority = ComputeSubsPriority(sub->sname, sub->pathelt);
	debug(16, ("Parsed entry:  path elt %d key %s status %d time64 %s filedate %d priority %d\n", sub->pathelt, sub->key, sub->status, sub->time64, sub->filedate, sub->priority));
	if (++NumSubsInUse >= NumSubsAllocated) {
	    NumSubsAllocated += SUBSALLOCATIONCHUNK;
	    SubsInUserOrder = (struct SubscriptionProfile *) realloc(SubsInUserOrder, NumSubsAllocated * sizeof(struct SubscriptionProfile));
	    if (!SubsInUserOrder) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READSUBS);
	    }
	}
	s = nextline;
    }
    qsort(SubsInUserOrder, NumSubsInUse, sizeof(struct SubscriptionProfile), CompareSubsPtrPriority);
    CheckSubsDuplication();
    LastSubsModTime = statbuf.st_mtime;
    return(CheckGlobalSubscriptions());
}

HandleSpecialProfileLine(line)
char *line;
{
    char *parm, *val, ErrorText[100+MAXPATHLEN];
    static char NomeM[] = "NO MEMORY TO HANDLE PATH CHANGES";

    sprintf(ErrorText, "Ignoring unrecognized profile line %s", line);
    parm = ++line;
    val = strchr(line, ' ');
    if (!val) {
	NonfatalBizarreError(ErrorText);
	return;  /* No currently recognized special lines have no values */
    }
    *val++ = '\0';
    if (strcmp(parm, "mail") == 0) {
	if (strncmp(val, home, strlen(home))) {
	    oldmail = NewString(val);
	    if (oldmail == NULL) {
		NonfatalBizarreError(NomeM);
	    }
	}
    } else if (strcmp(parm, "local") == 0) {
	if (strcmp(val, OLDLOCALSEARCHPATHTEMPLATE)) {
	    oldlocal = NewString(val);
	    if (oldlocal == NULL) {
		NonfatalBizarreError(NomeM);
	    }
	}
    } else if (strcmp(parm, "external") == 0) {
	if (strcmp(val, OLDEXTERNALSEARCHPATHTEMPLATE)) {
	    oldexternal = NewString(val);
	    if (oldexternal == NULL) {
		NonfatalBizarreError(NomeM);
	    }
	}
    } else if (strcmp(parm, "official") == 0) {
	if (strcmp(val, OLDOFFICIALSEARCHPATHTEMPLATE)) {
	    oldofficial = NewString(val);
	    if (oldofficial == NULL) {
		NonfatalBizarreError(NomeM);
	    }
	}
    } else {
	NonfatalBizarreError(ErrorText);
    }
    return(0);
}

static Boolean MailPathChanged = FALSE, LocalPathChanged = FALSE, ExtPathChanged = FALSE, OffPathChanged = FALSE;

static int oldlocallen, oldextlen, oldofflen, oldmaillen;

CheckPathChanges() {
    if (!oldlocal) oldlocal = OLDLOCALSEARCHPATHTEMPLATE;
    if (!oldexternal) oldexternal = OLDEXTERNALSEARCHPATHTEMPLATE;
    if (!oldofficial) oldofficial = OLDOFFICIALSEARCHPATHTEMPLATE;
    if (oldmail) {
     char MP[1+MAXPATHLEN];

     sprintf(MP, "%s/%s", home, MS_TREEROOT);
     if (MS_RebuildOneSubscriptionMap(MP)) {
     sprintf(MP, "Could not rebuild subscription map %s/%s/%s (%d, %d, %d)", ap_Shorten(home), MS_TREEROOT, AMS_SUBSCRIPTIONMAPFILE, AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
     NonfatalBizarreError(MP);
     }
     MailPathChanged = TRUE;
    } else {
     MailPathChanged = FALSE;
    }
    LocalPathChanged =  strcmp(oldlocal, LOCALSEARCHPATHTEMPLATE);
    ExtPathChanged = strcmp(oldexternal, EXTERNALSEARCHPATHTEMPLATE);
    OffPathChanged = strcmp(oldofficial, OFFICIALSEARCHPATHTEMPLATE);
    if (MailPathChanged || LocalPathChanged || ExtPathChanged || OffPathChanged) {
     PathsHaveChanged = TRUE;
    }
    oldmaillen = oldmail ? strlen(oldmail) : 0;
    oldlocallen = strlen(oldlocal);
    oldofflen = strlen(oldofficial);
    oldextlen = strlen(oldexternal);
}

HandlePathChange(sub)
struct SubscriptionProfile *sub;
{
    if (MailPathChanged) {
	char MP[1+MAXPATHLEN];

	sprintf(MP, "%s/%s", home, MS_TREEROOT);
	HandleChange(sub, oldmail, MP, oldmaillen);
    }
    if (LocalPathChanged) HandleChange(sub, oldlocal, LOCALSEARCHPATHTEMPLATE, oldlocallen);
    if (ExtPathChanged) HandleChange(sub, oldexternal, EXTERNALSEARCHPATHTEMPLATE, oldextlen);
    if (OffPathChanged) HandleChange(sub, oldofficial, OFFICIALSEARCHPATHTEMPLATE, oldofflen);
    return(0);
}

HandleChange(sub, oldpath, newpath, oldlen)
struct SubscriptionProfile *sub;
char *oldpath, *newpath;
int oldlen;
{
    char ErrorText[3000], NewName[1+MAXPATHLEN], *s;

    if (!oldpath || (oldlen <= 0)) return;
    if (!strncmp(sub->key, oldpath, oldlen)) {
	strcpy(NewName, newpath);
	strcat(NewName, sub->key + oldlen);
	(void) DeSymLink(NewName, ErrorText, 0);
	if (abspath(ErrorText, NewName) != 0) strcpy(NewName, ErrorText);
	sprintf(ErrorText, "Changing profile entry for %s to %s", sub->key, ap_Shorten(NewName));
	NonfatalBizarreError(ErrorText);
	SubscriptionsAreDirty = TRUE;
	if (sub->NeedsFreed) {
	    free(sub->key);
	} else {
	    s = malloc(1+strlen(sub->sname));
	    strcpy(s, sub->sname);
	    sub->sname = s; /* Make it free-able later */
	    sub->NeedsFreed = TRUE;
	}
	sub->key = malloc(1+strlen(NewName));
	strcpy(sub->key, NewName);
    }
    return(0);
}


CheckSubsDuplication() {
    int i, j;

    for (i=0; i<(NumSubsInUse-1); ++i) {
	if (!strcmp(SubsInUserOrder[i].key, SubsInUserOrder[i+1].key)) {
	    if (SubsInUserOrder[i+1].NeedsFreed) {
		free(SubsInUserOrder[i+1].key);
		free(SubsInUserOrder[i+1].sname);
	    }
	    for (j=i+1; j<(NumSubsInUse-1); ++j) {
		bcopy(&SubsInUserOrder[j+1], &SubsInUserOrder[j], sizeof(struct SubscriptionProfile));
	    }
	    --NumSubsInUse;
	    --i; /* Recheck this spot again! */
	}
    }
    return(0);
}

/* For the qsort call */

CompareSubsPtrPriority(sub1, sub2)
struct SubscriptionProfile *sub1, *sub2;
{
    if (!sub1 || !sub1->key) return(1);
    if (!sub2 || !sub2->key) return(-1);
    if (sub1->priority < sub2->priority) return(-1);
    if (sub1->priority > sub2->priority) return(1);
    if (sub1->pathelt < sub2->pathelt) return(-1);
    if (sub1->pathelt > sub2->pathelt) return(1);
    return(PreorderSubscriptionStrcmp(sub1->key, sub2->key));
}

CompareSubsPtrInternalPriority(sub1, sub2)
struct SubscriptionProfile **sub1, **sub2;
{
    if (!*sub1 || !(*sub1)->key) return(1);
    if (!*sub2 || !(*sub2)->key) return(-1);
    if ((*sub1)->pathelt < (*sub2)->pathelt) return(-1);
    if ((*sub1)->pathelt > (*sub2)->pathelt) return(1);
    return(PreorderSubscriptionStrcmp((*sub1)->key, (*sub2)->key));
}

int PreorderSubscriptionStrcmp(s, t)
char *s, *t;
{
    register char sc, tc;
    if (!s || !t)
      return (0);
    for (;;) {
	sc = *s;
	if (sc == '/') sc = '.';
	tc = *t;
	if (tc == '/') tc = '.';
	if (sc != tc) break;
	if (sc == '\0') return(0);
	++s; ++t;
    }
/*    for ( ; *s == *t; s++, t++) {
	if (*s == '\0') return(0);
    }
*/
    if (sc == '\0') return(-1);
    if (tc == '\0') return(1);
    if (tc == '.') return (1);
    if (sc == '.') return (-1);
    return((int) sc - tc);
}

RemoveSubsEntry(FullName)
char *FullName;
{
    int i;

    debug(1, ("RemoveSubsEntry %s\n", FullName));
    i = FindSubsEntry(FullName, FIND_MUSTFIND);
    if (i < 0 || i>= NumSubsInUse) return;
    --NumSubsInUse;
    while (i<NumSubsInUse) { /* slight occasional core leak here -- not always */
	bcopy(&SubsInUserOrder[i+1], &SubsInUserOrder[i], sizeof(struct SubscriptionProfile));
/*	SubsInUserOrder[i].status = SubsInUserOrder[i+1].status;
	SubsInUserOrder[i].sname = SubsInUserOrder[i+1].sname;
	SubsInUserOrder[i].key = SubsInUserOrder[i+1].key;
	SubsInUserOrder[i].pathelt = SubsInUserOrder[i+1].pathelt; */
	++i;
    }
    ++SubsModCtr;
    return(0);
}


SetSubsEntry(FullName, NickName, status)
char *FullName, *NickName;
int status;
{

    return(SetFullProfileEntry(TRUE, FullName, NickName, status, FALSE, NULL, 0, FALSE));
}

SetProfileEntry(FullName, newvalue, newdate)
char *FullName, *newvalue;
long newdate;
{
    char MyBuf[1+AMS_DATESIZE];

    debug(1, ("SetProfileEntry %s to %s (%ld)\n", FullName, newvalue, newdate));
    if ( strlen(newvalue) != (AMS_DATESIZE -1) || !strcmp(newvalue, "zzzzzz")) {
	char ErrorText[1000];

	sprintf(ErrorText, "Warning -- profile date for %s is dubious (%s); using current time instead", ap_Shorten(FullName), newvalue);
	NonfatalBizarreError(ErrorText);
	strcpy(MyBuf, convlongto64(time(0), 0));
	newvalue = MyBuf;
    }
    return(SetFullProfileEntry(FALSE, FullName, NULL, 0, TRUE, newvalue, newdate, FALSE));
}

SetFullProfileEntry(DoSubs, FullName, NickName, status, DoProf, time64, filedate, NeedToCheckPath)
Boolean DoSubs, DoProf, NeedToCheckPath;
char *FullName, *NickName, *time64;
int status, filedate;
{
    int i, j, IsNew = 0;
    int splen;
    struct SubscriptionProfile subs;
    char NickBuf[1+MAXPATHLEN], Full1[1+MAXPATHLEN], Full2[1+MAXPATHLEN];

    debug(1, ("SetFullProfileEntry %s nickname %s status %d time64 %s filedate %d\n", FullName, NickName ? NickName : "<null>", status, time64 ? time64 : "<null>", filedate));
    i = FindSubsEntry(FullName, FIND_MUSTFIND);
    if (i < 0) {
	return(-1); /* error code was set */
    }
    if (NickName) {
	NickName = StripWhiteEnds(NickName);
    } else {
	BuildNickName(FullName, NickBuf);
	NickName = NickBuf;
    }
    if (i >= NumSubsInUse) {
	if (LockProfile()) {
	    return(mserrcode);
	}
	debug(16, ("It is a new one!\n"));
	IsNew = 1;
	j = DeSymLink(FullName, Full1, 0);
	if (j != 0) {
	    if (j == -1) j = errno;
	    AMS_RETURN_ERRCODE(j, EIN_STAT, EVIA_SETSUBSENTRY);
	}
	if (abspath(Full1, Full2) != 0) strcpy(Full2, Full1);
	subs.key = NewString(Full2);
	if (subs.key == NULL) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETSUBSENTRY);
	}
	subs.sname = malloc(strlen(NickName) + 1);
	if (subs.sname == NULL) {
	    free(subs.key);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETSUBSENTRY);
	}
	strcpy(subs.sname, NickName);
	subs.pathelt = -1;
	for (j=0; j<MS_NumDirsInSearchPath; ++j) {
	    splen = strlen(SearchPathElements[j].Path);
	    if (!strncmp(subs.key, SearchPathElements[j].Path, splen) &&
		subs.key[splen] == '/') {
		subs.pathelt = j;
		break;
	    }
	}
	strcpy(subs.time64, "000000");
	subs.filedate = 0;
	subs.NeedsFreed = 1;
	subs.status = AMS_UNSUBSCRIBED;
	subs.priority = ComputeSubsPriority(subs.sname, subs.pathelt);
	if (NeedToCheckPath && PathsHaveChanged) {
	    HandlePathChange(&subs);
	}
	if (InsertInSubsList(&subs, &i)) {
	    free(subs.sname);
	    free(subs.key);
	    return(mserrcode);
	}
	SubscriptionsAreDirty = TRUE;
	debug(16, ("Path element is %d\n", subs.pathelt));
    }
    if (DoSubs) {
	if (SubsInUserOrder[i].status != status) {
	    if (LockProfile()) {
		if (IsNew) {
		    free(subs.sname);
		    free(subs.key);
		}
		return(mserrcode);
	    }
	    SubsInUserOrder[i].status = status;
	    SubscriptionsAreDirty = TRUE;
	}
    }
    if (DoProf) {
	if (SubsInUserOrder[i].filedate != filedate || strcmp(SubsInUserOrder[i].time64, time64)) {
	    if (LockProfile()) {
		if (IsNew) {
		    free(subs.sname);
		    free(subs.key);
		}
		return(mserrcode);
	    }
	    SubsInUserOrder[i].filedate = filedate;
	    strncpy(SubsInUserOrder[i].time64, time64, AMS_DATESIZE);
	    SubscriptionsAreDirty = TRUE;
	}
    }
    NeedToTimeOut = 1;
    return(0);
}

InsertInSubsList(subs, index)
struct SubscriptionProfile *subs;
int *index;
{
    int i, j;

    i = FindSubsEntry(subs->key, FIND_FINDPROPERPLACE);
    if (i < 0) return(mserrcode);
    if (++NumSubsInUse >= NumSubsAllocated) {
	NumSubsAllocated += SUBSALLOCATIONCHUNK;
	if (SubsInUserOrder) {
	    SubsInUserOrder = (struct SubscriptionProfile *) realloc(SubsInUserOrder, NumSubsAllocated * sizeof(struct SubscriptionProfile));
	} else {
	    SubsInUserOrder = (struct SubscriptionProfile *) malloc(NumSubsAllocated * sizeof(struct SubscriptionProfile));
	}
	if (!SubsInUserOrder) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETSUBSENTRY);
	}
    }
    ++SubsModCtr;
    if ((i < (NumSubsInUse-1)) && !strcmp(subs->key, SubsInUserOrder[i].key)) {
	--NumSubsInUse;
    } else {
	debug(16, ("New addition to subs list: %s nick %s position %d\n", subs->key, subs->sname, i));
	for(j=NumSubsInUse-1; j>i; --j) {
	    bcopy(&SubsInUserOrder[j-1], &SubsInUserOrder[j], sizeof(struct SubscriptionProfile));
	}
    }
    bcopy(subs, &SubsInUserOrder[i], sizeof(struct SubscriptionProfile));
    *index = i;
    return(0);
}

GetSubsEntry(FullName, NickName, status)
char *FullName, *NickName;
int *status;
{
    int i;

    debug(1, ("GetSubsEntry %s\n", FullName));
    i = FindSubsEntry(FullName, FIND_MUSTFIND);
    if (i < 0) return(mserrcode); /* error code was set */
    if (i == NumSubsInUse) {
	BuildNickName(FullName, NickName);
	*status = AMS_UNSUBSCRIBED;
	return(0);
    }
    strcpy(NickName, SubsInUserOrder[i].sname);
    *status = SubsInUserOrder[i].status;
    return(0);
}


GetAssocFileTime(FullName, fdate)
char *FullName;
long *fdate;
{
    int i;

    i = FindSubsEntry(FullName, FIND_MUSTFIND);
    if (i<0) return(mserrcode);
    if (i == NumSubsInUse) {
	*fdate = 0;
	debug(4, ("Key not found, returning null value\n"));
	return(0);
    }
    *fdate = SubsInUserOrder[i].filedate;
    return(0);
}

GetAssocTime(FullName, Answer, lim)
char *FullName, *Answer;
int lim;
{
    int i;

    i = FindSubsEntry(FullName, FIND_MUSTFIND);
    if (i<0) return(mserrcode);
    if (i == NumSubsInUse) {
	*Answer = '\0';
	debug(4, ("Key not found, returning null value\n"));
	return(0);
    }
    strncpy(Answer, SubsInUserOrder[i].time64, lim);
    Answer[lim-1] = '\0';
    return(0);
}

/* This writes it out in an old & simpler format, for direct use by clients */

WriteSimpleSubsMap(fname)
char *fname;
{
    FILE *fp;
    int subsindex;

    if (ReadSubs() != 0) {
	return(mserrcode);
    }
    if ((fp = fopen(fname, "w")) == NULL) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_NAMESUBSCRIPTIONMAP);
    }
    for (subsindex = 0; subsindex<NumSubsInUse; ++subsindex) {
	if (SubsInUserOrder[subsindex].status != AMS_UNSUBSCRIBED) {
	    fprintf(fp, "%s:%s %d\n", SubsInUserOrder[subsindex].sname, SubsInUserOrder[subsindex].key, SubsInUserOrder[subsindex].status);
	}
    }
    errno = 0;
    if (ferror(fp) || feof(fp)) {
	fclose(fp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_NAMESUBSCRIPTIONMAP);
    }
    if (vfclose(fp)) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_NAMESUBSCRIPTIONMAP);
    }
    return(0);

}

GetNextSubsEntry(FullName, NickName, status)
char *FullName, *NickName;
int *status;
{
    static int i = 0;

    debug(1, ("GetNextSubsEntry %s\n", FullName));
    if (i>=0 && i<NumSubsInUse && !strcmp(FullName, SubsInUserOrder[i].key)) {
	debug(1, ("Good job!  A trivial caching mechanism wins big!\n"));
	++i;
    } else {
	i = FindSubsEntry(FullName, FIND_MUSTFINDNEXT);
	if (i < 0) return(-1); /* error code was set */
    }
    while (i<NumSubsInUse && (SubsInUserOrder[i].status == AMS_UNSUBSCRIBED)) {
	++i;
    }
    if (i >= NumSubsInUse) {
	FullName[0] = '\0';
	NickName[0] = '\0';
	*status = AMS_UNSUBSCRIBED;
	return(0);
    }
    strcpy(FullName, SubsInUserOrder[i].key);
    strcpy(NickName, SubsInUserOrder[i].sname);
    *status = SubsInUserOrder[i].status;
    return(0);
}



static FILE *ProfLockFP = NULL;
#define OLDLOCK 600 /* 10 minutes */

LockProfile() {
    struct stat statbuf;
    static char ProfLockFile[1+MAXPATHLEN] = "";

    if (ProfLockFP) return(0);
    if (ProfLockFile[0] == '\0') {
	if (SubsFileName == NULL) if (GetSubsFileName() != 0) return(mserrcode);
	sprintf(ProfLockFile, "%s.LOCK", SubsFileName);
    }
    ProfLockFP = fopen(ProfLockFile, osi_F_READLOCK);
    if (!ProfLockFP) ProfLockFP = fopen(ProfLockFile, "w");
    if (!ProfLockFP) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_LOCKPROFILE);
    }
    if (osi_ExclusiveLockNoBlock(fileno(ProfLockFP))){
	if ((fstat(fileno(ProfLockFP), &statbuf) != 0)
	|| (time(0) - statbuf.st_mtime < OLDLOCK)) {
	    fclose(ProfLockFP);
	    AMS_RETURN_ERRCODE(errno, EIN_FLOCK, EVIA_LOCKPROFILE);
	}
    }
    return(0);
}

UnlockProfile() {
    if (ProfLockFP) {
	fclose(ProfLockFP);
	ProfLockFP = NULL;
    }
    return(0);
}

MakeSubsListInPathOrder() {
    int i;
    static int LastSubsModCtr = -1;

    if (SubsModCtr == LastSubsModCtr) {
	debug(256, ("No need to re-sort by path order; not changed since last sort\n"));
	return(0);
    }
    if (ReadSubs() != 0) {
	return(mserrcode);
    }
    if (!SubsInPathOrder) {
	SubsInPathOrder = (struct SubscriptionProfile **) malloc(sizeof(struct SubscriptionProfile *) * (NumSubsInUse + 1));
    } else {
	SubsInPathOrder = (struct SubscriptionProfile **) realloc(SubsInPathOrder, sizeof(struct SubscriptionProfile *) * (NumSubsInUse + 1));
    }
    if (!SubsInPathOrder) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READSUBS);
    }
    for (i=0; i<=NumSubsInUse; ++i) {
	SubsInPathOrder[i] = &SubsInUserOrder[i];
    }
    qsort(SubsInPathOrder, NumSubsInUse, sizeof(struct SubscriptionProfile *), CompareSubsPtrInternalPriority);
    LastSubsModCtr = SubsModCtr;
    return(0);
}

ComputeSubsPriority(sname, pathelt)
char *sname;
int pathelt;
{
    int i, bestmatch=32767, bestmatchscore = -1, matchscore, dum;

    if (!HasInitializedSubsPriorities) InitializeSubsPriorities();
    debug(256, ("Computing subs priority for %s, pathelt %d\n", sname, pathelt));
    for (i = 0; i<NumSubsOrderElts; ++i) {
	debug(256, ("SubsOrderElts[%d].pattern is %s, wild %d, pathelt %d\n", i, SubsOrderElts[i].pattern, SubsOrderElts[i].iswild, SubsOrderElts[i].pathelt));
	if (SubsOrderElts[i].pathelt < 0 || SubsOrderElts[i].pathelt == pathelt) {
	    if (SubsOrderElts[i].iswild) {
		dum = 0;
		matchscore = ScoreMatch(SubsOrderElts[i].pattern, sname, &dum);
		if (matchscore) {
		    matchscore = dum;
		} else {
		    matchscore = -1;
		}
	    } else if (strcmp(sname, SubsOrderElts[i].pattern)) {
		matchscore = -1;
	    } else {
		matchscore = strlen(sname);
	    }
	    if (matchscore > bestmatchscore) {
		bestmatchscore = matchscore;
		bestmatch = i;
		debug(256, ("Bestmatchscore is now %d, bestmatch %d\n", bestmatchscore, bestmatch));
	    }
	}
    }
    debug(256, ("Priority is %d\n", bestmatch));
    return(bestmatch);
}

ScoreMatch(pattern, string, matchct)
register char *pattern, *string;
int *matchct;
{
/*     debug(256, ("Matching pattern %s against name %s\n", pattern, string)); */
    if (!pattern || !string) return(0);
    while(*string && *pattern) {
	if (*pattern == '*') {
	    for (; *string; string++) {
		if (ScoreMatch(pattern+1, string, matchct)) return (1);
	    }
	    if (!*(pattern+1)) {
		/* Note that to keep matchct right, this *follows* the above loop */
		return(1);
	    }
	    return (0);
	} else {
	    if (*pattern != *string) return (0);
	    ++*matchct;
	}
	++string;
	++pattern;
    }
    if (!*string) {
	while (*pattern == '*') ++pattern;
    }
    return(*string == *pattern);
}


static char *NoSubsPriMem = "Out of memory in subscription ordering -- things may appear in a strange order";

InitializeSubsPriorities() {
    char *s, *t, *dollarsign;
    Boolean SkipThis;

    if (!SubsOrderElts) {
	SubsOrderElts = (struct subsorderelt *) malloc(25 * sizeof(struct subsorderelt));
	if (!SubsOrderElts) {
	    NonfatalBizarreError(NoSubsPriMem);
	    return;
	}
    }
    s = getprofile("subsorder");
    if (s) {
	SubsOrderToFree = malloc(1+strlen(s));
	if (!SubsOrderToFree) {
	    NonfatalBizarreError(NoSubsPriMem);
	    return;
	}
	strcpy(SubsOrderToFree, s);
	s = SubsOrderToFree;
    }
    while (s) {
	SkipThis = FALSE;
	t = strchr(s, ':');
	if (t) *t++ = '\0';
	s = StripWhiteEnds(s);
	debug(256, ("Processing subs priority element %s\n", s));
	if (NumSubsOrderElts >= NumSubsOrderAllocated) {
	    NumSubsOrderAllocated += 25;
	    SubsOrderElts = (struct subsorderelt *) realloc(SubsOrderElts, NumSubsOrderAllocated * sizeof(struct subsorderelt));
	    if (!SubsOrderElts) {
		NonfatalBizarreError(NoSubsPriMem);
		return;
	    }
	}
	dollarsign = strchr(s+1, '$');
	if (dollarsign) {
	    *dollarsign++ = '\0';
	    SubsOrderElts[NumSubsOrderElts].pathelt = WhichPath(s);
	    if (SubsOrderElts[NumSubsOrderElts].pathelt < 0) {
		char MsgTxt[100+MAXPATHLEN];
		sprintf(MsgTxt, "Ignoring unrecognized mspath element in subsorder preference: '%s'", s);
		NonfatalBizarreError(MsgTxt);
		SkipThis = TRUE;
	    } else {
		s = dollarsign;
		if (!*s) s = "*";
	    }
	} else {
	    SubsOrderElts[NumSubsOrderElts].pathelt = -1;
	}
	if (s && strchr(s, '*')) {
	    SubsOrderElts[NumSubsOrderElts].iswild = 1;
	} else {
	    SubsOrderElts[NumSubsOrderElts].iswild = 0;
	}
	SubsOrderElts[NumSubsOrderElts].pattern = s;
	debug(256, ("Parsed:  Is wild %d pattern %s pathelt %d Skipping %d\n", SubsOrderElts[NumSubsOrderElts].iswild, SubsOrderElts[NumSubsOrderElts].pattern, SubsOrderElts[NumSubsOrderElts].pathelt, SkipThis));
	s = t;
	if (!SkipThis) ++NumSubsOrderElts;
    }
    HasInitializedSubsPriorities = 1;
    return(0);
}

WhichPath(s)
char *s;
{
    char Fnam[1+MAXPATHLEN], *fresh = NULL, *extraslash = NULL;
    int i;

    if (*s == '$') {
	if (! lc2strncmp("$mail", s, 5)) {
	    sprintf(Fnam, "%s/%s", home, MS_TREEROOT);
	    s = Fnam;
	} else if (! lc2strncmp("$local", s, 6)) {
	    s = LOCALSEARCHPATHTEMPLATE;
	} else if (! lc2strncmp("$external", s, 9)) {
	    s = EXTERNALSEARCHPATHTEMPLATE;
	} else if (! lc2strncmp("$official", s, 9)) {
	    s = OFFICIALSEARCHPATHTEMPLATE;
	}
    } else {
	extraslash = s+strlen(s) -1;
	if (*extraslash == '/') {
	    *extraslash = '\0';
	} else {
	    extraslash = NULL;
	}
	ResolveTildes(s, &fresh, MyMailDomain);
	s = fresh;
    }
    for (i=0; i < MS_NumDirsInSearchPath; ++i) {
	if (!strcmp(s, SearchPathElements[i].Path)) {
	    if (extraslash) *extraslash = '/';
	    if (fresh) free(fresh);
	    return(i);
	}
    }
    if (extraslash) *extraslash = '/';
    if (fresh) free(fresh);
    return(-1);
}
	

CheckGlobalSubscriptions() {
    FILE *fp;
    char LineBuf[100+MAXPATHLEN], *s, *fullname;

    fp = fopen(GlobalRequiredSubsFile, "r");
    if (fp) { /* No complaint on errors for now */
	while (fgets(LineBuf, sizeof(LineBuf), fp)) {
	    /* handle a line of global subscriptions */
	    s = strchr(LineBuf, ':');
	    fullname = StripWhiteEnds(s ? ++s : LineBuf);
	    s = strchr(fullname, ':');
	    if (s) *s = '\0';
	    s = strchr(fullname, ' ');
	    if (s) *s = '\0';
	    SetFullProfileEntry(TRUE, fullname, NULL, AMS_ALWAYSSUBSCRIBED, FALSE, NULL, 0, TRUE);
	}
	fclose(fp);
    }
    return(0);
}

CheckForInconsistentSubscriptions(SubsFileName)
char *SubsFileName;
{
    char OldStuff[1+MAXPATHLEN];
    struct stat st1, st2;
    int errs = 0;
    static Boolean HasChecked = FALSE;

    if (HasChecked) return(0);
    sprintf(OldStuff, "%s/.MS.profile", home);
    if (stat(SubsFileName, &st1) != 0) {
	return(errno != ENOENT);
    }
    if (stat(OldStuff, &st2) != 0) {
	if (errno == ENOENT) return(0);
	NonfatalBizarreError("Could not check your old .MS.profile; hope that's OK.");
	return(-1);
    }
    /* both exist, must choose between them! */
    if (st1.st_mtime > st2.st_mtime) {	/* AMS prof is newer */
	char OldStuffNewName[1+MAXPATHLEN];

	sprintf(OldStuffNewName, "%s/OLD.MS.profile", home);
	if (rename(OldStuff, OldStuffNewName)) {
	    NonfatalBizarreError("Could not delete your outdated .MS.profile.");
	    ++errs;
	} else {
	    NonfatalBizarreError("Renamed your outdated .MS.profile.");
	}
	sprintf(OldStuff, "%s/.MS.subscriptions", home);
	sprintf(OldStuffNewName, "%s/OLD.MS.subscriptions", home);
	if (rename(OldStuff, OldStuffNewName)) {
	    NonfatalBizarreError("Could not delete your outdated .MS.subs.");
	    ++errs;
	} else {
	    NonfatalBizarreError("Renamed your outdated .MS.subs.");
	}
    } else {	/* AMS prof is older!  Poor soul... */
	char SubsNewName[1+MAXPATHLEN], *cp, *cp2;

	strcpy(SubsNewName, SubsFileName);
	cp = strrchr(SubsNewName, '/');
	cp2 = strrchr(SubsFileName, '/');
	if (cp == NULL || cp2 == NULL || *cp == '\0' || *cp2 == '\0') {
	    strcat(SubsNewName, ".OLD");
	} else {
	    ++cp; ++cp2;
	    *cp++ = 'O'; *cp++ = 'L'; *cp++ = 'D';
	    strcpy(cp, cp2);
	}
	if (rename(SubsFileName, SubsNewName)) {
	    NonfatalBizarreError("Could not delete your outdated .AMS.prof.");
	    ++errs;
	} else {
	    NonfatalBizarreError("Renamed your outdated .AMS.prof.");
	}
    }
    HasChecked = TRUE;
    return(errs);
}


CheckForOldFashionedSubscriptions(foundthem) 
int *foundthem;
{
    char SFile[1+MAXPATHLEN], PFile[1+MAXPATHLEN], LineBuf[100+MAXPATHLEN], *full, *nick, *time64;
    FILE *rfp;
    int subscode, filedate = 0, GoodLine, foundct = 0, mistakes = 0;

    *foundthem = 0;
    sprintf(SFile, "%s/.MS.subscriptions", home);
    rfp = fopen(SFile, "r");
    if (rfp) {
	while (fgets(LineBuf, sizeof(LineBuf), rfp)) {
	    StripWhiteEnds(LineBuf);
	    mistakes = BadLine(LineBuf, "old-fashioned subscription file");
	    if (mistakes) {
		NonfatalBizarreError("Ignoring a corrupted old subscription line");
		continue;
	    }
	    subscode = AMS_ALWAYSSUBSCRIBED;
	    full = strchr(LineBuf, ':');
	    if (full) {
		nick = LineBuf;
		*full++ = '\0';
		switch(*full) {
		    case '&':
			subscode = AMS_SHOWALLSUBSCRIBED;
			++full;
			break;
		    case '*':
			subscode = AMS_ASKSUBSCRIBED;
			++full;
			break;
		    case '^':
			subscode = AMS_PRINTSUBSCRIBED;
			++full;
			break;
		    /* No default -- ignore other cases */
		} 
	    } else {
		full = LineBuf;
		nick = NULL;
	    }
	    if (SetSubsEntry(full, nick, subscode)) {
		return(mserrcode);
	    }
	    ++foundct;
	}
	fclose(rfp);
    } else if (errno != ENOENT) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_READPROFILE);
    }
    
    sprintf(PFile, "%s/.MS.profile", home);
    rfp = fopen(PFile, "r");
    if (rfp) {
	while (fgets(LineBuf, sizeof(LineBuf), rfp)) {
	    GoodLine = FALSE;
	    full = StripWhiteEnds(LineBuf);
	    mistakes = BadLine(LineBuf, "old-fashioned MS profile");
	    if (mistakes) {
		NonfatalBizarreError("Ignoring a corrupted old profile line");
		continue;
	    }
	    time64 = strchr(full, ' ');
	    if (time64) {
		*time64++ = '\0';
		nick = strchr(time64, ' ');
		if (nick) {
		    *nick++ = '\0';
		    filedate = atoi(nick);
		    GoodLine = TRUE;
		}
	    }
	    if (!GoodLine) {
		sprintf(SFile, "Ignoring illegal profile line %s", LineBuf);
		NonfatalBizarreError(SFile);
	    } else {
		if (SetProfileEntry(full, time64, filedate)) {
		    return(mserrcode);
		}
		++foundct;
	    }
	}
	fclose(rfp);
	
    } else if (errno != ENOENT) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_READPROFILE);
    }
    *foundthem = foundct;
    if (WriteSubs()) return(mserrcode);
    unlink(SFile); unlink(PFile); /* Checking error codes is pretty useless here */
    return(0);
}

BadSubMapLine(s)
char *s;
{
    return(BadLine(s, "subscription map"));
}

BadUpdFileLine(s)
char *s;
{
    return(BadLine(s, "master update file"));
}

BadLine(s, what)
char *s, *what;
{
    char *tmp, ErrorText[256];
    int mistakes = 0;

    for (tmp = s; *tmp; ++tmp) { /* The error message should be kind to console */
	if (!isprint(*tmp) && !isspace(*tmp)) {
	    sprintf(ErrorText, "Illegal character in %s: ASCII %d (decimal)", what, *tmp);
	    NonfatalBizarreError(ErrorText);
	    ++mistakes;
	    break;
	}
    }
    return(mistakes);
}
