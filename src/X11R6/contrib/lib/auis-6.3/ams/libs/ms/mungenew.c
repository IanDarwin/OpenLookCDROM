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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/mungenew.c,v 2.33 1993/09/21 21:57:19 gk5g Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <ms.h>
#include <sys/stat.h>
#include <mailconf.h>
#ifdef AFS_ENV
#include <netinet/in.h>
#include <afs/param.h>
#include <sys/types.h>
#include <afs/venus.h>
#include <afs/prs_fs.h>
#endif /* AFS_ENV */

#define MAILBOXFILECHUNK 1000

extern char  home[], Me[], *PersonalMailCollectionCommand, *GetPersonalMailbox(), *getprofile(), *getenv();
extern int CompareFileTimeStructs(), homeUsesAMSDelivery;

/* The following two are to allow us to get decent reporting data from the postman daemon even when it gets a quit signal. */
int MS_DataCollectionHackInProgress = 0;
int MS_UnreportedCollections = 0;
char *MS_UnreportedMailbox = "";

MS_ProcessNewMessages(SourceDir, NumGood, NumBad, NumLocks, ParseSpecFile, resultcode, FirstError, NumInProgress, EliErrBuf, EliErrBufLim)
char *SourceDir, *ParseSpecFile, *EliErrBuf;
int *NumGood, *NumBad, *NumLocks, *resultcode, *FirstError, *NumInProgress, EliErrBufLim;
{
    MS_UnreportedCollections = 0;
    MS_DataCollectionHackInProgress = 1;
    MS_UnreportedMailbox = SourceDir;
    mserrcode = ReallyTruly_ProcessNewMessages(SourceDir, &MS_UnreportedCollections, NumBad, NumLocks, ParseSpecFile, resultcode, FirstError, NumInProgress, EliErrBuf, EliErrBufLim);
    *NumGood = MS_UnreportedCollections;
    MS_DataCollectionHackInProgress = 0;    /* Slight hole between this & normal report, sigh... */
    return (mserrcode);
}

static ReallyTruly_ProcessNewMessages(SourceDir, NumGood, NumBad, NumLocks, ParseSpecFile, resultcode, FirstError, NumInProgress, EliErrBuf, EliErrBufLim)
char *SourceDir, *ParseSpecFile, *EliErrBuf;
int *NumGood, *NumBad, *NumLocks, *resultcode, *FirstError, *NumInProgress, EliErrBufLim;
{
    int i, numfiles = 0, MailboxFilesAllocated, UnlinkFailures = 0, OldErrno;
    struct stat statbuf;
    char *ParseSpec, *s;
    struct FileTime *FTList = NULL;
    char SpecFileName[1 + MAXPATHLEN], ThisFileName[MAXPATHLEN + 1];
    int NeedToSort = 1;
    DIR *dirp;
    DIRENT_TYPE  *dirent;
#ifdef AFS_ENV
    struct ViceIoctl thud;
    long int Permissions;
#endif /* AFS_ENV */

    debug(1, ("Entering MS_ProcessNewMessages\n"));

    FlushClosableDir(&UnlinkFailures);
    CloseDirsThatNeedIt();
    *NumBad = *NumGood = *NumInProgress = *NumLocks = *FirstError = *resultcode = 0;
    if (EliErrBufLim) *EliErrBuf = '\0';
    if (homeUsesAMSDelivery < 0 &&
	( !(PersonalMailCollectionCommand || AMS_MailCollectionCommand) )) {
	char SpoolFileName[1 + MAXPATHLEN], *HomeBox, *SpoolFile;
	int numfound;

	/* Only convert /usr/spool/mail-type stuff if we're checking our personal mailbox */
	HomeBox = GetPersonalMailbox();
	sprintf(SpoolFileName, "%s/Mailbox", home);
	if (strcmp(HomeBox, SourceDir) == 0 || strcmp(SpoolFileName, SourceDir) == 0) {
	    SpoolFile = getenv("MAIL");
	    if (SpoolFile == NULL || *SpoolFile == '\0') SpoolFile = getprofile("mailboxfile");
	    if (SpoolFile == NULL || *SpoolFile == '\0') {
		sprintf(SpoolFileName, "%s/%s", AMS_MailBoxPrefix, Me);
	    } else {
		strcpy(SpoolFileName, SpoolFile);
	    }
	    mserrcode = ConvertIncomingMail(SpoolFileName, SourceDir, &numfound);
	    if (mserrcode) return (mserrcode);
	}
    }
    if (PersonalMailCollectionCommand) {
	int syscode;

	syscode = system(PersonalMailCollectionCommand);
	if (syscode) {
	    char ErrorText[100+MAXPATHLEN];

	    sprintf(ErrorText, "Personal mail collection command %s failed with code %d", ap_Shorten(PersonalMailCollectionCommand), syscode);
	    NonfatalBizarreError(ErrorText);
	    AMS_RETURN_ERRCODE(EMSBOGUS, EIN_SYSTEM, EVIA_PROCNEWMSGS);
	}
    }
    if (AMS_MailCollectionCommand) {
	int syscode;

	syscode = system(AMS_MailCollectionCommand);
	if (syscode) {
	    char ErrorText[100+MAXPATHLEN];

	    sprintf(ErrorText, "System mail collection command %s failed with code %d", ap_Shorten(AMS_MailCollectionCommand), syscode);
	    NonfatalBizarreError(ErrorText);
	    AMS_RETURN_ERRCODE(EMSBOGUS, EIN_SYSTEM, EVIA_PROCNEWMSGS);
	}
    }
    debug(4, ("Reading directory %s\n", SourceDir));
    if (access(SourceDir, W_OK) != 0) {
	AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_PROCNEWMSGS);
    }
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	Permissions = PRSFS_DELETE;
	thud.in = (char *) &Permissions;
	thud.in_size = sizeof(Permissions);
	thud.out = NULL;
	thud.out_size = 0;
	if (pioctl(SourceDir, VIOCACCESS, &thud, 1) != 0) {
	    if (errno == EACCES)
		AMS_RETURN_ERRCODE(EACCES, EIN_PIOCTL, EVIA_PROCNEWMSGS);
	}
    }
#endif /* AFS_ENV */
    strcpy(ThisFileName, SourceDir);
    s = strrchr(ThisFileName, '/');
    if (s) {
	strcpy(++s, "AMS_NO_MAILBOX_SORTING");
	if (stat(ThisFileName, &statbuf) == 0) {
	    char ErrorText[1000];

	    sprintf(ErrorText, "Not sorting mail files in %s", ap_Shorten(SourceDir));
	    NonfatalBizarreError(ErrorText);
	    FTList = NULL;
	    NeedToSort = 0;
	}
    }
    if (NeedToSort) {
	if ((dirp = opendir(SourceDir)) == NULL) {
	    debug(4, ("Can't open source directory %s\n", SourceDir));
	    AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_PROCNEWMSGS);
	}
	FTList = (struct FileTime *) malloc(sizeof(struct FileTime) * MAILBOXFILECHUNK);
	MailboxFilesAllocated = MAILBOXFILECHUNK;
	if (!FTList) {
	    closedir(dirp);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PROCNEWMSGS);
	}
	while ((dirent = readdir(dirp)) != NULL) {
	    debug(4, ("Considering %s\n", dirent->d_name));
	    if (dirent->d_name[0] == '.') {
		debug(4, ("Ignoring a dot file\n"));
		continue; /* Ignore all hidden files */
	    }
	    sprintf(ThisFileName, "%s/%s", SourceDir, dirent->d_name);
	    if (stat(ThisFileName, &statbuf)) {
		OldErrno = errno;
		FreeFTList(FTList, numfiles);
		closedir(dirp);
		AMS_RETURN_ERRCODE(OldErrno, EIN_STAT, EVIA_PROCNEWMSGS);
	    }
	    if ((statbuf.st_mode & S_IFMT) != S_IFREG) {
		char ErrorText[1000];

		sprintf(ErrorText, "Mailbox %s has a %s %s which I will ignore", ap_Shorten(SourceDir), ((statbuf.st_mode & S_IFMT) == S_IFDIR ? "subdirectory" : "strange file"), dirent->d_name);
		NonfatalBizarreError(ErrorText);
		continue;
	    }
	    FTList[numfiles].Time = statbuf.st_mtime;
	    FTList[numfiles].Name = malloc(1 + strlen(ThisFileName));
	    if (!FTList[numfiles].Name) {
		FreeFTList(FTList, numfiles);
		closedir(dirp);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PROCNEWMSGS);
	    }
	    strcpy(FTList[numfiles++].Name, ThisFileName);
	    if (numfiles >= MailboxFilesAllocated) {
		MailboxFilesAllocated += MAILBOXFILECHUNK;
		FTList = (struct FileTime *) realloc(FTList, sizeof(struct FileTime) * MailboxFilesAllocated);
		if (!FTList) {
		    /* Oops, the core in the old pointers has just leaked... */
		    closedir(dirp);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PROCNEWMSGS);
		}

	    }
	}
	closedir(dirp);
	if (numfiles <= 0) {
	    if (FTList) {
		free(FTList);
	    }
	    return (0);
	}
	qsort(FTList, numfiles, sizeof(struct FileTime), CompareFileTimeStructs);
    }
    if (!ParseSpecFile[0]) {
	strcpy(SpecFileName, SourceDir);
	s = strrchr(SpecFileName, '/');
	if (s) {
	    strcpy(++s, DEFAULTFLAMESFILENAME);
	    if (!stat(SpecFileName, &statbuf)) {
		ParseSpecFile = SpecFileName;
	    }
	}
    }
    if (ParseSpecFile[0]) {
	int len;

	len = strlen(ParseSpecFile);
	ParseSpec = malloc(1 + len);
	if (!ParseSpec) {
	    FreeFTList(FTList, numfiles);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PROCNEWMSGS);
	}
	strcpy(ParseSpec, ParseSpecFile);
    }
    else {
	ParseSpec = NULL;
    }
    if (NeedToSort) {
	for (i = 0; i < numfiles; ++i) {
	    if (ProcessNewMail(FTList[i].Name, ParseSpec, PROCESSNEW_MBOX, &UnlinkFailures, EliErrBuf, EliErrBufLim)) {
		debug(4, ("Error: errno %d errcause %d errvia %d\n", AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA));
		++*NumBad;
		if (!*FirstError)
		    *FirstError = mserrcode;
		if (AMS_ERRNO == EMSYOUNGMAIL)
		    ++* NumInProgress;
		if (AMS_ERRNO == EWOULDBLOCK || AMS_ERRNO == EMSYOUNGLOCK)
		    ++* NumLocks;
	    }
	    else {
		++*NumGood;
	    }
	    free(FTList[i].Name);
	}
	free(FTList);
    }
    else {
	if ((dirp = opendir(SourceDir)) == NULL) {
	    OldErrno = errno;
	    debug(4, ("Can't open source directory %s\n", SourceDir));
	    if (ParseSpec)
		free(ParseSpec);
	    AMS_RETURN_ERRCODE(OldErrno, EIN_OPENDIR, EVIA_PROCNEWMSGS);
	}
	while ((dirent = readdir(dirp)) != NULL) {
	    debug(4, ("Considering %s\n", dirent->d_name));
	    if (dirent->d_name[0] == '.') {
		debug(4, ("Ignoring a dot file\n"));
		continue; /* Ignore all hidden files */
	    }
	    sprintf(ThisFileName, "%s/%s", SourceDir, dirent->d_name);
	    if (stat(ThisFileName, &statbuf)) {
		OldErrno = errno;
		closedir(dirp);
		if (ParseSpec)
		    free(ParseSpec);
		AMS_RETURN_ERRCODE(OldErrno, EIN_STAT, EVIA_PROCNEWMSGS);
	    }
	    if ((statbuf.st_mode & S_IFMT) == S_IFDIR) {
		char ErrorText[1000];

		sprintf(ErrorText, "Mailbox %s has a subdirectory %s which I will ignore", ap_Shorten(SourceDir), dirent->d_name);
		NonfatalBizarreError(ErrorText);
		continue;
	    }
	    if (ProcessNewMail(ThisFileName, ParseSpec, PROCESSNEW_MBOX, &UnlinkFailures, EliErrBuf, EliErrBufLim)) {
		debug(4, ("Error: errno %d errcause %d errvia %d\n", AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA));
		++*NumBad;
		if (!*FirstError)
		    *FirstError = mserrcode;
		if (AMS_ERRNO == EMSYOUNGMAIL)
		    ++* NumInProgress;
		if (AMS_ERRNO == EWOULDBLOCK || AMS_ERRNO == EMSYOUNGLOCK)
		    ++* NumLocks;
	    }
	    else {
		++*NumGood;
	    }
	}
	closedir(dirp);
    }
    debug(4, ("All done with directory %s, cleaning up now...\n", SourceDir));
    if (ParseSpec)
	free(ParseSpec);
    mserrcode = CloseDirsThatNeedIt();
    *resultcode = (UnlinkFailures ? 1 : 0) | (*NumBad ? 2 : 0);
    return (mserrcode);
}
