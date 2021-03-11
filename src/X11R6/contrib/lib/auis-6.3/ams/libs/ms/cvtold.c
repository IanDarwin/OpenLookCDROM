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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/cvtold.c,v 2.22 1994/04/05 13:43:53 rr2b Exp $";
#endif


 

#include <sys/param.h>
#include <andrewos.h> /* sys/types.h sys/file.h sys/time.h */
#include <sys/stat.h>
#include <ms.h>
#include <mailconf.h>
#include <stdio.h>
#include <ctype.h>
#include <parseadd.h>

extern int errno;
extern char home[];

/*
  * Remove the mail lock, and note that we no longer
  * have it locked.
  */
static int rmlock(name, lockFD)
char name[]; int lockFD;
{
    struct stat statb;

    if (lockFD >= 0) close(lockFD);
    if (stat(name, &statb) < 0)
	return(-1);
    if ((statb.st_mode & S_IFMT) != S_IFREG) {
	errno = EISDIR;
	return(-1);
    }
    if (unlink(name)) {
#if SY_B4x != 0
	return(truncate(name, 0));
#else /* SY_B4x */
	return 0;
#endif /* SY_B4x */
    }
    return(0);
}

/*
 * Lock the specified mail file by setting the file mailfile.lock.
 * We must, of course, be careful to rmlock the lock file by a call
 * to unlock before we stop.  The algorithm used here is to see if
 * the lock exists, and if it does, return an error.
 *
 * Attempt to set the lock by creating the temporary file,
 * then doing a link/unlink.  If it fails, return -1 else 0
 */
static int lock(file, lockedFile, lockedFDp)
char *file, *lockedFile; int *lockedFDp;
{
    register int f, g;
    char	locktmp[1+MAXPATHLEN];	    /* Usable lock temporary */
    char *s;

    s = strrchr(file, '/');
    if (*s != '/') return -1;
    strcpy(lockedFile, SpoolMailLockDir);
    strcat(lockedFile, s);
    strcat(lockedFile, ".lock");
    g = open(lockedFile, osi_O_READLOCK, 0);
    if (g < 0) return -1;
    if (osi_ExclusiveLockNoBlock(g) != 0) {
	close(g);
	return -1;
    }
    strcpy(locktmp, SpoolMailLockTemp);
    mktemp(locktmp);
    rmlock(locktmp, -1);
    f = creat(locktmp, 0);
    if (f < 0) {
	close(g);
	return(-1);
    }
    close(f);
    if (link(locktmp, lockedFile) < 0) {
	rmlock(locktmp, g);
	return(-1);
    }
    rmlock(locktmp, -1);
    *lockedFDp = g;
    return(0);
}

static int SetHoldFromFile(fname, holdP)
char *fname; int *holdP;
{/* Set or unset ``hold'' as in the file ``fname''. */
    FILE *fp;
    char InBuf[300];
    char *sp, *scmd;
    int DoSet, errsave;

    errno = 0;
    fp = fopen(fname, "r");
    if (fp == NULL) {
	if (errno == ENOENT) return 0;
	if (errno == 0) errno = ENOMEM;
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_CONVERTINCOMING);
    }
    for (;;) {
      NextLine:
	sp = fgets(InBuf, sizeof(InBuf), fp);
	if (sp == NULL) break;
	while (*sp != '\0' && isspace(*sp)) ++sp;
	scmd = sp;
	while (*sp != '\0' && !isspace(*sp)) ++sp;
	*sp++ = '\0';
	if (strcmp(scmd, "set") == 0) DoSet = 1;
	else if (strcmp(scmd, "unset") == 0) DoSet = 0;
	else continue;
	for (;;) {
	    while (*sp != '\0' && isspace(*sp)) ++sp;
	    if (*sp == '\0') goto NextLine;
	    scmd = sp;
	    while (*sp != '\0' && !isspace(*sp)) ++sp;
	    *sp++ = '\0';
	    if (strcmp(scmd, "hold") == 0) *holdP = DoSet;
	}
    }
    if (ferror(fp)) {
	errsave = errno;
	(void) fclose(fp);
	if (errsave == 0) errsave = ENOMEM;
	AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_CONVERTINCOMING);
    }
    (void) fclose(fp);
    return 0;
}

static int CheckMailrcHold()
{/* Check whether the ~/.mailrc or /usr/lib/Mail.rc file has set the ``hold'' variable; return 0 if it isn't set, or an mserrcode. */
    static int HoldVal = -1;
    int RC, TempVal;
    char MyMailrc[1+MAXPATHLEN];

    if (HoldVal >= 0) return HoldVal;
    TempVal = 0;
    RC = SetHoldFromFile("/usr/lib/Mail.rc", &TempVal);
    if (RC != 0) return RC;
    sprintf(MyMailrc, "%s/.mailrc", home);
    RC = SetHoldFromFile(MyMailrc, &TempVal);
    if (RC != 0) return RC;
    HoldVal = TempVal;	/* Got it all right. */
    if (HoldVal != 0) {
	AMS_RETURN_ERRCODE(EMSHOLDSET, EIN_PARAMCHECK, EVIA_CONVERTINCOMING);
    }
    return 0;
}

#define FALSE 0
#define TRUE 1
#define buffsize 1024
#define MAXTRIES 25

int ConvertIncomingMail(MailSpoolFile, MailDir, FilesReadIn)
char *MailSpoolFile, *MailDir;
int *FilesReadIn;
{
    FILE *fp;
    int		wfd = 0, i, errsave, tfd, AnyWrittenToThisOne, LockFD,blen;
    short FileOpen, ReadyToStartAgain;
    struct stat statbuf;
    char	buffer[buffsize], FName[1+MAXPATHLEN], CurLock[1+MAXPATHLEN];

    *FilesReadIn = 0;
    if ((stat(MailDir, &statbuf)) == -1){
	 if (errno == ENOENT) {
	     return(0);
	 }
	AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_CONVERTINCOMING);
    }
    if ((stat(MailSpoolFile, &statbuf)) == -1){
	if (errno == ENOENT) {
	    return(0);
	}
	AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_CONVERTINCOMING);
    }
    if (statbuf.st_size <= 0){
	return(0);
    }
    errsave = CheckMailrcHold();
    if (errsave != 0) return(errsave);
    if (AMS_StrictStandaloneLocking && (lock(MailSpoolFile, CurLock, &LockFD)) != 0){
	AMS_RETURN_ERRCODE(errno, EIN_UCBMAILLOCK, EVIA_CONVERTINCOMING);
    }
    if ((fp = fopen(MailSpoolFile, "r")) == NULL){
	if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
	AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_CONVERTINCOMING);
    }
    FileOpen = FALSE;
    ReadyToStartAgain = 0;
    AnyWrittenToThisOne = 0;
    while (TRUE) {
	if (fgets(buffer, buffsize, fp) == NULL) {
	    errsave = errno;
	    if (feof(fp)) break; /* Done reading */
	    fclose(fp);
	    if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
	    AMS_RETURN_ERRCODE(errsave, EIN_READ, EVIA_CONVERTINCOMING);
	}
	if (ReadyToStartAgain && FileOpen && (AMS_DemandSeparatingCharacter || IsNewFrom(buffer)) && AnyWrittenToThisOne) {
	    (*FilesReadIn)++;
	    if (vclose(wfd)) {
		fclose(fp);
		if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
		AMS_RETURN_ERRCODE(errno, EIN_VCLOSE, EVIA_CONVERTINCOMING);
	    }
	    FileOpen = FALSE;
	}
	if (!FileOpen){
	    for (i=0; i<MAXTRIES; ++i) {
		sprintf(FName, "%s/%s", MailDir, ams_genid(1));
		wfd = open(FName, O_CREAT|O_EXCL|O_WRONLY, 0600);
		if (wfd < 0) continue;
		if (osi_ExclusiveLockNoBlock(wfd)) {
		    errsave = errno;
		    close(wfd);
		    if (errsave == EWOULDBLOCK) {
			continue;
		    }
		    AMS_RETURN_ERRCODE(errsave, EIN_FLOCK, EVIA_CONVERTINCOMING);
		}
		break; /* flock succeeded */
	    }
	    if (wfd < 0) {
		errsave = errno;
		fclose(fp);
		if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
		AMS_RETURN_ERRCODE(errsave, EIN_VMOPEN, EVIA_CONVERTINCOMING);
	    }
	    FileOpen = TRUE;
	    AnyWrittenToThisOne = 0;
	}
	if (AMS_DemandSeparatingCharacter) {
	    if ((buffer[0] == AMS_SeparatingCharacter) && (AnyWrittenToThisOne != 0)) {
		ReadyToStartAgain = 1;
	    } else {
		/* maybe delete all lines starting with separating char */
		if (!AMS_DeleteLinesWithSeparator || buffer[0] != AMS_SeparatingCharacter) {
		    blen = strlen(buffer);
		    if(writeall(wfd, buffer,blen) != blen){
			errsave = errno;
			fclose(fp);
			close(wfd);
			if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
			AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_CONVERTINCOMING);
		    }
		    AnyWrittenToThisOne = 1;
		    ReadyToStartAgain = 0;
		}
	    }
 	} else if (!AnyWrittenToThisOne && strncmp(buffer, "From ", 5) == 0) {
 	    /* UNIX From lines make sendmail barf on re-send,  so we pre-empt by rewriting them here with a patch from Lennart Lovstrand.  The format of a "From " line is: "From <sender> <date> [remote from <host>]" There is a chance sender will contain spaces (enclosed in quotes or escaped by backslash), so we can't just index on a space. */
 	    char *p;
 	    int quoted = FALSE, escaped = FALSE;
 
 	    for (p = &buffer[5]; *p != '\0'; p++) {
 		if (escaped)
 		    escaped = FALSE;
 		else if (*p == '\\')
 		    escaped = TRUE;
 		else if (*p == '"')
 		    quoted = !quoted;
 		else if (*p == ' ')
 		    break;
 	    }
 	    *p = '\0';
 	    if ((writeall(wfd, "Return-Path: <", 14) != 14)
 		|| (writeall(wfd, &buffer[5], strlen(&buffer[5])) != strlen(&buffer[5]))
 		|| (writeall(wfd, ">\n", 2) != 2)) {
 		errsave = errno;
 		fclose(fp);
 		close(wfd);
 		if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
 		AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_CONVERTINCOMING);
 	    }
 	    AnyWrittenToThisOne = TRUE;
 	    ReadyToStartAgain = FALSE;
	} else {
	    ReadyToStartAgain = (buffer[0] == '\n') ? 1 : 0;
	    blen =  strlen(buffer);
	    if(writeall(wfd, buffer,blen) != blen){
		errsave = errno;
		fclose(fp);
		close(wfd);
		if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
		AMS_RETURN_ERRCODE(errsave, EIN_WRITE, EVIA_CONVERTINCOMING);
	    }
	    AnyWrittenToThisOne = 1;
	}
    }
    if (FileOpen){
	(*FilesReadIn)++;
	if (vclose(wfd)) {
	    errsave = errno;
	    fclose(fp);
	    if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
	    AMS_RETURN_ERRCODE(errsave, EIN_VCLOSE, EVIA_CONVERTINCOMING);
	}
    }
    if ((tfd = creat(MailSpoolFile, 666)) < 0){
	char ErrorText[500];

	sprintf(ErrorText, "Could not Zero-out %s (%d)...continuing anyway", MailSpoolFile, errno);
	NonfatalBizarreError(ErrorText);
    } else {
	close(tfd); /* Just creating it, that's all */
    }
    fclose(fp);
    if (AMS_StrictStandaloneLocking) rmlock(CurLock, LockFD);
    return(0);
}

static int IsNewFrom(line)
char *line;
{
    PARSED_ADDRESS *ListHead = NULL;
    struct tm TmBuf;
    char *addr, *date, *space;

    /* First pass, very simple */
    if (strncmp(line, "From", 4)) {
	return(0);
    }
    if (line[4] != ' ' && (!AMS_AllowColonInSeparatingFrom || line[4] != ':')) return(0);
    addr = strchr(line, ' ');
    if (!addr) return(0);
    while (*addr && isspace(*addr)) ++addr;
    if (!*addr) return(0);
    space = strchr(addr, ' ');
    if (!space) return(0);
    *space = '\0';
    date = space+1;
    if (AMS_CheckAddressInSeparatingFrom && *addr != '@' && ParseAddressList(addr, &ListHead) != PA_OK) {
	*space = ' ';
	return(0);
    }
    if (ListHead) FreeAddressList(ListHead);
    *space = ' ';
    if (AMS_CheckDateInSeparatingFrom && parsedateheader(date, &TmBuf, 1, 1, 1, 0)) return(0);
    return(1);
}
