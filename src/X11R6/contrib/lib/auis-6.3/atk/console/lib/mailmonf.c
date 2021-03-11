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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/mailmonf.c,v 2.27 1993/09/22 19:22:02 gk5g Exp $";
#endif


 

/* ***************************************************************
	Mail system monitoring routine for Instrument Console
	Also now monitors random directories and print requests
***************************************************************** */

#include <andyenv.h>
#include <class.h>
#include <andrewos.h>
#include <conclass.ih>
#include <console.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>
#include <sys/ioctl.h>
#ifdef AFS_ENV
#include <netinet/in.h>
#include <afs/param.h>
#include <afs/vice.h>
#include <afs/errors.h>
#include <afs/prs_fs.h>
#define bool_t int
#include <afs/afsint.h>
#include <afs/venus.h>
#endif /* AFS_ENV */

extern struct classinfo *consoleClass_GetInfoHack();
extern char *sys_errlist[];
extern int LastDirMod[], OutgoingAge;
extern char MyHomeDir[];
extern char *PrintDirName, OutgoingDir[];
extern int PrintDirModTime;
extern boolean UseNonAndrewPrint, UseNonAndrewMail, NonViceHost, NonAFSDHost, CheckMyMail;
extern SetPrintEnv();



#define PREFIX 1
#define SUFFIX 2
#define NO_DOTS 3

CheckDir(self, Name, LastModTime, LastValue)
struct consoleClass *self;
char *Name;
int *LastModTime, LastValue;
{
    DIR *dp;
    DIRENT_TYPE *dirent;
    int dc;
    struct stat dirstatbuf;
    int WildCard = 0;
    char NewName[MAXPATHLEN], *f;

    mydbg(("entering: CheckDir\n"));
    strcpy(NewName, Name);
    if((f = (char *)index(NewName, '\052')) != NULL){
	if ((!index(f, '/'))&&(!index(f + 1, '*'))){
	    if ((*(f - 1) != '/')&&(*(f + 1) == '\0')){
		/* prefix given */
		WildCard = PREFIX;
		*f = '\0'; /* change * to NUL */
		f = (char *)rindex(NewName, '/'); /* find last '/' */
		*f = '\0'; /* NewName is now just a dir path */
		*f++; /* f is now the prefix */
	    }
	    else{
		if (*(f - 1) == '/'){
		    /* suffix given */
		    WildCard = SUFFIX;
		    *f = '\0'; /* NewName is now just a dir path */
		    *f++; /* f is now the suffix */
		}
		else{
		    *f = ' ';
		    sprintf(ErrTxt, "console: '%s' is neither prefix nor suffix");
		    ReportInternalError(self, ErrTxt);
		    return(-1);
		}
	    }
	}
	else{
	    *f = ' ';
	    sprintf(ErrTxt, "console: '%s' is invalid InitString specification");
	    ReportInternalError(self, ErrTxt);
	    return(-1);
	    /* bogus string - contains either two * or the * is within the path */
	}
    }
    else{
	if((NewName[strlen(NewName) - 1] == '^')&&(NewName[strlen(NewName) - 2] == '/')){
	    WildCard = NO_DOTS;
	    NewName[strlen(NewName) - 1] = '\0';
	}
    }
    /* We now have a valid dirpath NewName, and possibly a wildcard f */
    if (stat(NewName, &dirstatbuf) == -1) {
	sprintf(ErrTxt, "console: Cannot stat directory '%s': cannot check %s now.", NewName, LastModTime == &LastMailMod ? "your mail" : "its contents");
	ReportInternalError(self, ErrTxt);
	*LastModTime = 0;
	return(-1);
    }
    if ((dirstatbuf.st_mode & S_IFMT) != S_IFDIR) {
	sprintf(ErrTxt, "console: '%s' is not a directory!  %s", NewName, LastModTime == &LastMailMod ? "You probably can't receive mail!" : "Can't count any files in it!");
	ReportInternalError(self, ErrTxt);
	*LastModTime = 0;
	return(-1);
    }
    if (dirstatbuf.st_atime == *LastModTime) {
	return(LastValue);
    }
    if ((dp = opendir(NewName)) == NULL) {
	sprintf(ErrTxt, "console: Cannot open directory '%s'; problem with AFS?", NewName);
	ReportInternalError(self, ErrTxt);
	*LastModTime = 0;
	return(-1);
    }
    if(WildCard){
	dc = 0;
    }
    else{
	dc = -2; /* Do not count . and .. */
    }
    *LastModTime = dirstatbuf.st_atime;
    while ((dirent = readdir(dp)) != NULL){
	switch(WildCard){
	    case NO_DOTS:
		if(dirent->d_name[0] == '.') continue;
		break;
	    case PREFIX:
		{
		int len = strlen(f);
		if (strncmp(dirent->d_name, f, len)) continue;
		}
		break;
	    case SUFFIX:
		{
		int len = strlen(f);
		if (strncmp(&dirent->d_name[strlen(dirent->d_name) - len], f, len)) continue;
		}
		break;
	}
	++dc;
    }
    closedir(dp);
    return(dc);
}

CheckMail(self, requested)
struct consoleClass *self;
int requested;
{
    int value, iserror;
    char ErrTxt[256];

    mydbg(("entering: CheckMail\n"));
    if (envmail == NULL) {
	ReportInternalError(self, "console: Attempting to Re-Set Mail Environment.");
	SetMailEnv();
	if(envmail == NULL){
	    ReportInternalError(self, "console: Mail Environment undefined.");
	    LastMailMod = 0;
	    return;
	}
    }
    errno=0;
    iserror = 0;
    if (UseNonAndrewMail) {
        value = NonAndrewCheckMail(self, envmail, &LastMailMod, Numbers[MAIL].Value);
        if (value<0 && errno != ENOENT) iserror=1;
    } else {
        value = CheckDir(self, envmail, &LastMailMod, Numbers[MAIL].Value);
        if (value < 0 && !IsViceError(errno)) iserror = 1;
        if (OutgoingDir[0]) CheckOutgoingMail(self);
    }
    if (iserror) {
        value = -1;
	sprintf(ErrTxt, "console: Mail checking terminated (%s)", sys_errlist[errno]);
        ReportInternalError(self, ErrTxt);
        DoMailChecking = FALSE;
    }
#ifdef AFS_ENV
    if (!NonViceHost){
	if ((Numbers[MAIL].Value != value || requested) && !UseNonAndrewMail) {
	    PrefetchMailFiles(self, envmail, requested);
	}
    }
#endif /* AFS_ENV */
    NewValue(self, &Numbers[MAIL], value, NULL, FALSE);
}

CheckDirectories(self)
struct consoleClass *self;
{
    int i, val;
    char ErrTxt[256];

    mydbg(("entering: CheckDirectories\n"));
    for (i= DIRECTORY1; i<= LASTDIRECTORY-ExternalsInUse; ++i) {
        if (Numbers[i].IsDisplaying) {
            val = CheckDir(self, Numbers[i].RawText, &LastDirMod[i-DIRECTORY1],
                           Numbers[i].Value);
            if (Numbers[i].Value < 0) {
                if (!IsViceError(errno)) {
		    sprintf(ErrTxt, "console: Monitoring of %s terminated (%s)", Numbers[i].RawText, sys_errlist[errno]);
                    ReportInternalError(self, ErrTxt);
                    free(Numbers[i].RawText);
                    Numbers[i].RawText = Nullity;
                    Numbers[i].IsDisplaying = FALSE;
                } else {
		    sprintf(ErrTxt, "console: Temporary failure to check %s", Numbers[i].RawText);
                    ReportInternalError(self, ErrTxt);
                }
            }
            NewValue(self, &Numbers[i], val, NULL, FALSE);
        }
    }
}

CheckPrint(self)
struct consoleClass *self;
{
    DIR * dp;
    DIRENT_TYPE  *dirent;
    struct stat dirstatbuf;
    int     InQueue = 0,
        Sent = 0,
        Error = 0;

    mydbg(("entering: CheckPrint\n"));
    if (PrintDirName == NULL) {
	SetPrintEnv();
    }
    if (stat(PrintDirName, &dirstatbuf) == -1) {
        if (IsViceError(errno)) {
	    sprintf(ErrTxt, "console:  Cannot stat directory %s: cannot check print queue now.", PrintDirName);
	    ReportInternalError(self, ErrTxt);
            PrintDirModTime = 0;
            return(-1);
	}
	sprintf(ErrTxt, "console:  Printing monitor terminated (%s)", sys_errlist[errno]);
        return AbortPrintChecking(self, ErrTxt);
    }
    if ((dirstatbuf.st_mode & S_IFMT) != S_IFDIR) {
        return AbortPrintChecking(self, "console: Printing monitor terminated -- PrintDir is not a directory");
    }
    if (dirstatbuf.st_atime == PrintDirModTime) {
        return;
    }
    if ((dp = opendir(PrintDirName)) == NULL) {
	sprintf(ErrTxt, "console: Printing monitor terminated on opendir (%s)", sys_errlist[errno]);
        return AbortPrintChecking(self, ErrTxt);
    }
    PrintDirModTime = dirstatbuf.st_atime;
    while ((dirent = readdir(dp)) != NULL) {
        if (UseNonAndrewPrint) {
            /*
             * Making this "d" instead of "df" should make it work with
             * either the system 5 spooler or the Berkeley spooler.
             */
            if (dirent->d_name[0] == 'd') {
                InQueue++;
            }
            continue;
        }
        if (dirent->d_name[0] != '.') {
            InQueue++;
        }
        else
            if (!strncmp(dirent->d_name, ".sent:", 6)) {
                Sent++;
            }
            else
                if (!strncmp(dirent->d_name, ".error:", 7)) {
                    Error++;
                }
    }
    closedir(dp);
    if (Error != Numbers[PRINTERRORS].Value) {
	if (Error) {
	    ReportInternalError(self, "A printing error occured - use 'print -clear'");
	}
    }
    NewValue(self, &Numbers[PRINTQUEUE], InQueue, NULL, FALSE);
    NewValue(self, &Numbers[PRINTSENT], Sent, NULL, FALSE);
    NewValue(self, &Numbers[PRINTERRORS], Error, NULL, FALSE);
}

static int OutgoingModTime = 0;

CheckOutgoingMail(self)
struct consoleClass *self;
{
    DIR * dp;
    DIRENT_TYPE  *dirent;
    struct stat statbuf;
    int goingoutct = 0, thistime;
    char FullName[1+MAXPATHLEN], *NewPart;

    mydbg(("entering: CheckOutgoingMail\n"));
    if (stat(OutgoingDir, &statbuf) == -1) {
        if (IsViceError(errno)) {
	    sprintf(ErrTxt, "console:  Cannot stat directory %s: cannot check outgoing mail now.", OutgoingDir);
	    ReportInternalError(self, ErrTxt);
            OutgoingModTime = 0;
            return;
        }
        /* BOGUS -- this should get put back when outgoing directories are universal */
        /* 	ReportInternalError(self, sprintf(ErrTxt, "console: terminated monitoring of outgoing mail (%s)", sys_errlist[errno])); */
        OutgoingDir[0] = '\0';
        return;
    }
    if ((statbuf.st_mode & S_IFMT) != S_IFDIR) {
        ReportInternalError(self, "console: Outgoing mail monitoring terminated -- .Outgoing is not a directory");
    }
    if (statbuf.st_atime == OutgoingModTime) {
        return;
    }
    if ((dp = opendir(OutgoingDir)) == NULL) {
	sprintf(ErrTxt, "console: Printing monitor terminated on opendir (%s)", sys_errlist[errno]);
        ReportInternalError(self, ErrTxt);
        return;
    }
    OutgoingModTime = statbuf.st_atime;
    strcpy(FullName, OutgoingDir);
    NewPart = FullName + strlen(FullName);
    *NewPart++='/';
    thistime = time(0);
    while ((dirent = readdir(dp)) != NULL) {
        if (dirent->d_name[0] == 'Q') {
            strcpy(NewPart, dirent->d_name);
            if (stat(FullName, &statbuf) == -1) {
                if (errno != ENOENT){
		    sprintf(ErrTxt, "console: Cannot stat outgoing mail file %s (%s)", dirent->d_name, sys_errlist[errno]);
                    ReportInternalError(self, ErrTxt);
                }
                continue;
            }
            if ((thistime - statbuf.st_mtime) > OutgoingAge) {
                goingoutct++;
            }
        }
    }
    closedir(dp);
    NewValue(self,&Numbers[OUTGOINGMAIL], goingoutct, NULL, FALSE);
}

#ifdef AFS_ENV
PrefetchMailFiles(self, MailboxDir, requested)
struct consoleClass *self;
char *MailboxDir;
int requested;
{
    DIR *dp;
    DIRENT_TYPE *dirent;
    struct ViceIoctl blob;
    char fullname[1+MAXPATHLEN], *s, ErrTxt[256];

    mydbg(("entering: PrefetchMailFiles\n"));
    if(!NonViceHost){
	if ((dp = opendir(MailboxDir)) == NULL) {
	    ReportInternalError(self, "console: Cannot open your mailbox to prefetch mail");
	    return(-1);
	}
	strcpy(fullname, MailboxDir);
	strcat(fullname, "/");
	s = fullname + strlen(fullname);
	blob.out_size = 0;
	blob.in_size = 0;
	while ((dirent = readdir(dp)) != NULL) {
	    if (dirent->d_name[0] == '.'){
		if((strcmp(dirent->d_name, "."))&&(strcmp(dirent->d_name, ".."))){
		    sprintf(ErrTxt, "ERROR: Non-Mail-File: (%s) in Mailbox", dirent->d_name);
		    ReportInternalError(self, ErrTxt);
		}
		continue;
	    }
	    strcpy(s, dirent->d_name); /* appends it to path prefix */
	    if (CheckMyMail){
		ParseMail(self, fullname, requested);
	    }
	    else {
		if (pioctl(fullname, VIOCPREFETCH, &blob)) {
		    if ((errno != ENOENT) && (errno != EBUSY) && (errno != EACCES) && (errno != EWOULDBLOCK)){
			sprintf(ErrTxt, "console: Cannot pre-fetch your new mail %s (error %d); sorry...", fullname, errno);
			ReportInternalError(self, ErrTxt);
		    }
		}
	    }
	}
	closedir(dp);
	return(0);
    }
}
#endif /* AFS_ENV */


ParseMail(self, fname, requested)
struct consoleClass *self;
char *fname;
int requested;
{
    FILE *fp;
    int c, onenewline, eofcheck;
    char header[256], buf[280];
    struct stat statbuf;
    static long LastTime = 0;
    long ThisTime;

    mydbg(("entering: ParseMail\n"));
    if (!stat(fname, &statbuf)){
	/* if there's an error - don't bother - I don't think it's worth reporting */
	ThisTime = statbuf.st_mtime;
	if (ThisTime > LastTime || requested){
	    if (statbuf.st_size == 0){
		sleep(2); /* the mail might be getting written while we're checking */
		if (!stat(fname, &statbuf)){
		    ThisTime = statbuf.st_mtime;
		}
	    }
	    LastTime = (ThisTime > LastTime) ? ThisTime : LastTime;
	    if (statbuf.st_size == 0){
		ReportInternalError(self, "mail>> An empty mail file found in Mailbox");
		return;
	    }
	    if ((fp = fopen(fname, "r")) != NULL){
		onenewline = FALSE;
		c = 0;
		while(1){
		    if(((eofcheck = getc(fp)) != EOF) && ((header[c++] = eofcheck) != '\n')){
			if (!isascii(header[c - 1])){
			    ReportInternalError(self, "mail>> core or other binary file found in Mailbox");
			    fclose(fp);
			    return;
			}
			while(((eofcheck = getc(fp)) != EOF) && ((header[c++] = eofcheck) != '\n') && isascii(header[c - 1]) && (c < 254));
			if (eofcheck == EOF){
			    ReportInternalError(self, "mail>> From **unknown sender**");
			    fclose(fp);
			    return;
			}			
			if (!isascii(header[c - 1])){
			    ReportInternalError(self, "mail>> core or other binary file found in Mailbox");
			    fclose(fp);
			    return;
			}
			if (header[c - 1] == '\n'){
			    onenewline = TRUE;
			    c--;
			}
			else{
			    onenewline = FALSE;
			}
			header[c] = '\0';
			if ((c > 14 && !strncmp(header, "ReSent-From:", 12))
			    || (c > 7 && !strncmp(header, "From:", 5))){
			    sprintf(buf, "mail>> %s", header);
			    ReportInternalError(self, buf);
			    fclose(fp);
			    return;
			}
		    }
		    else{
			if ((eofcheck == EOF) || (onenewline)){
			    ReportInternalError(self, "mail>> From **unknown sender**");
			    fclose(fp);
			    return;
			}
			if (!isascii(header[c - 1])){
			    ReportInternalError(self, "mail>> core or other binary file found in Mailbox");
			    fclose(fp);
			    return;
			}
		    }
		    c = 0;
		}
	    }
	    ReportInternalError(self, "mail>> From **unknown sender**");
	    if (fp) fclose(fp);
	}
    }
}

	  
