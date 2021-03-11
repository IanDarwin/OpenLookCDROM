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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/vmail.c,v 2.24 1993/07/16 14:24:15 rr2b Exp $";
#endif

/*
		vmail.c -- Deliver a message to a user.
*/

#include <system.h>
#include <errno.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <andrewos.h> /* sys/file.h */
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */
#ifdef sys_sun4_51
#include "/usr/ucbinclude/sysexits.h"
#endif /* sys_sun4_51 */

#include <pwd.h>

#include <fdplumb.h>
#include <util.h>
#include <mailconf.h>
#include <mail.h>	/* for ITC libmail */

#ifdef AFS_ENV
/* AFS-specific error codes */
#include <afs/errors.h>
#endif /* AFS_ENV */

typedef int bool;
#define FALSE	0
#define TRUE	1

#define NIL	0

enum lockcodes { LOCK_OK, LOCK_FAILED_OK,
			LOCK_FAILED_ERROR, LOCK_FAILED_TEMP_ERROR };

extern int errno;

static FILE *msgfile = NULL;	/* Descriptor of open msg file: NULL if unopen */
static char msgfilename[MAXPATHLEN+1];

char VM_text[MAXPATHLEN*2+1];
int VM_errordetail;

static char AFSDownMsg[] = "AFS is apparently down for ``%s'': %s";

/* Logging definitions */
#ifndef Logs
#define Logs 0
#endif /* Logs */
#ifndef LogsYes
#define LogsYes 0
#endif /* LogsYes */

#if LogsYes
static int DoingTiming = 0;		/* Set by VM_SetTiming, cleared on VM_close */
/*VARARGS2*/
static void Log(num, fmt, p1, p2, p3, p4, p5, p6)
int num; char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
{
    if (DoingTiming) Logstat("vmail.c", num, fmt, p1, p2, p3, p4, p5, p6);
}
#endif /* LogsYes */

enum OnAFSCodes { ON_AFS, AFS_DOWN, NOT_ON_AFS, NOT_A_DIR, INACCESSIBLE };

static enum OnAFSCodes dirinAFS(name)
    register char *name;
{
    static struct stat buf;
    static int statres, fd;

#if LogsYes
    Log(100, "About to stat dir");
#endif /* LogsYes */
    fd = open(name, O_RDONLY);	/* Get a file descriptor for IsOnVice() */
    statres = -1;
    if (fd >= 0) {
	statres = fstat(fd, &buf);
	if (statres < 0 && errno == EACCES) {
		VenusFlush(name);	/* try again to see if it's a glitch */
#if LogsYes
		Log(101, "Dir flushed--stating again");
#endif /* LogsYes */
		statres = fstat(fd, &buf);
	}
	if (statres < 0) {close(fd); fd = -1;}
    }
#if LogsYes
    Log(102, "Stat of dir done: %d, %d", statres, errno);
#endif /* LogsYes */

    if (statres < 0) {

	/* Stat (and/or open) failed -- see why */
	if (vdown(errno)) {
	    sprintf(VM_text, AFSDownMsg, name, UnixError(errno));
	    VM_errordetail = vm_ViceDown;
	    return AFS_DOWN;
	} else {
	    sprintf(VM_text, "Dir ``%s'' not found: %s", name, UnixError(errno));
	    VM_errordetail = vm_SystemErrorOffset + errno;
	    return (INACCESSIBLE);
	}

    } else {	/* Stat and open both succeeded; make sure in AFS */

	if ((buf.st_mode & S_IFDIR) == 0) {
	    sprintf(VM_text, "Target ``%s'' is not a directory", name);
	    VM_errordetail = vm_NotADir;
	    close(fd);
	    return NOT_A_DIR;
	}
	if (IsOnVice(fd)) {
	    close(fd);
	    return ON_AFS;
	} else {
	    sprintf(VM_text, "Dir ``%s'' is on the local disk (%d)", name, errno);
	    VM_errordetail = vm_NotOnVice;
	    close(fd);
	    return NOT_ON_AFS;
	}
    }
}

static int CheckDirInAFS(dir, proc)
    char *dir, *proc;
{
    static enum OnAFSCodes status;

    status = dirinAFS(dir);
    switch (status) {
	case ON_AFS:
		return 0;
	case AFS_DOWN:
		return EX_TEMPFAIL;
	case NOT_ON_AFS:
		return EX_CANTCREAT;
	case INACCESSIBLE:
		return -1;
	case NOT_A_DIR:
		return EX_CANTCREAT;
	default:
		sprintf(VM_text,
			"Bad dirinAFS return in routine %s for ``%s'': %d",
			proc, dir, status);
		VM_errordetail = vm_InternalError;
		return EX_SOFTWARE;
    }
}

int VM_SetTiming(param)
int param;
{
#if LogsYes
	int OldValue;

	OldValue = DoingTiming;
	DoingTiming = param;
	return OldValue;
#else /* LogsYes */
	return 0;
#endif /* LogsYes */
}

VM_open(User, Mailbox, ReturnPath, For, Authenticated, PgmName)
    char *User, *Mailbox, *ReturnPath, *For, *Authenticated, *PgmName;
{
    int rc, ExCode;

    /* See if still in a previous VM_open */
    if (msgfile != NULL) {
	strcpy(VM_text, "Previous VM_open still in progress");
	VM_errordetail = vm_AlreadyOpen;
	return EX_USAGE;
    }

    /* Check specification of user name and mailbox. */
    if (User != NIL) {	/* Cellular use: make this parameter obsolete. */
	strcpy(VM_text, "Can no longer specify a destination as a user");
	VM_errordetail = vm_BadParameters;
	return EX_USAGE;
    }
    if (Mailbox == NIL) {
	strcpy(VM_text, "Neither user or mailbox was specified");
	VM_errordetail = vm_BadParameters;
	return EX_USAGE;
    }

#if Logs
    Log(300, "VM_open called");
#endif /* Logs */

/* CheckDirInAFS will return 0 if it's on AFS, -1 if we got ENOENT.  Maybe a home dir moved. */
    ExCode = CheckDirInAFS(Mailbox, "VM_open");
    if (ExCode != 0) {	/* Inaccessible. */
#if LogsYes
	DoingTiming = 0;
#endif /* LogsYes */
	return (ExCode > 0 ? ExCode : EX_TEMPFAIL);
    }
    rc = CreateAndInitFile(Mailbox, ReturnPath, For, Authenticated, PgmName);
    if (rc != 0) {
#if LogsYes
	DoingTiming = 0;
#endif /* LogsYes */
	return rc;
    }

    /* If you made it this far, everything was ok */
    sprintf(VM_text, "VM_open completed succesfully: ``%s''", msgfilename);
    VM_errordetail = 0;
    return EX_OK;
}

int VM_write(s, n)
    char *s;
    int n;
{
    if (msgfile == NULL) {
	strcpy(VM_text, "VM_open not called before VM_write");
	VM_errordetail = vm_NotOpen;
	return EX_USAGE;
    }

#if Logs
    Log(350, "Calling VM_write with %d characters", n);
#endif /* Logs */
    if (n <= 0) return EX_OK;
    if (fwriteallchars(s, n, msgfile) <= 0 || ferror(msgfile)) {
	int err = errno;
#if Logs
	Log(351, "VM_write failed (errno %d)", err);
#endif /* Logs */
	fclose(msgfile);
	msgfile = NULL;
#if LogsYes
	DoingTiming = 0;
#endif /* LogsYes */
	sprintf(VM_text,
		"Error during write in VM_write to ``%s'': %s",
		msgfilename, UnixError(err));
	VM_errordetail = vm_SystemErrorOffset + err;
	return EX_IOERR;
    }
#if Logs
    Log(352, "VM_write returning OK");
#endif /* Logs */
    return EX_OK;
}

int VM_printf(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    char *fmt;
{
    if (msgfile == NULL) {
	strcpy(VM_text, "VM_open not called before VM_printf");
	VM_errordetail = vm_NotOpen;
	return EX_USAGE;
    }

#if Logs
    Log(360, "Calling VM_printf");
#endif /* Logs */
    fprintf(msgfile, fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
    if (ferror(msgfile)) {
	int err = errno;
#if Logs
	Log(361, "VM_printf failed (errno %d)", err);
#endif /* Logs */
	fclose(msgfile);
	msgfile = NULL;
#if LogsYes
	DoingTiming = 0;
#endif /* LogsYes */
	sprintf(VM_text,
		"Error during write in VM_printf to ``%s'': %s",
		msgfilename, UnixError(err));
	VM_errordetail = vm_SystemErrorOffset + err;
	return EX_IOERR;
    }
#if Logs
    Log(362, "VM_printf returning OK");
#endif /* Logs */
    return EX_OK;
}

int VM_close()
{
    int err; char *s;

    if (msgfile == NULL) {
	strcpy(VM_text, "VM_open not called before VM_close");
	VM_errordetail = vm_NotOpen;
	return EX_USAGE;
    }

#if LogsYes
    Log(400, "VM_close called");
#endif /* LogsYes */
    if (vfclose(msgfile) < 0) {
	/* Special check for over quota */
	err = errno;
#if LogsYes
	Log(401, "VM_close failed--errno is %d", err);
#endif /* LogsYes */
	msgfile = NULL;
#if LogsYes
	DoingTiming = 0;
#endif /* LogsYes */
	if ((err == EFBIG)
#ifdef ENAMETOOLONG
	    ||(err == ENAMETOOLONG)
#endif /* ENAMETOOLONG */
	    ){
		sprintf(VM_text, "Can't close ``%s'': %s", msgfilename, UnixError(err));
		VM_errordetail = vm_SystemErrorOffset + err;
		return EX_CANTCREAT;
	    }
	sprintf(VM_text, "Close failed for file ``%s'': %s",
		msgfilename, UnixError(err));
	VM_errordetail = vm_SystemErrorOffset + err;
	return EX_TEMPFAIL;
    }

    msgfile = NULL;
#if LogsYes
    Log(402, "VM_close done--about to VenusFlushCallback");
#endif /* LogsYes */
    VenusFlushCallback(msgfilename);	/* flush callbacks from this WS's cache, ignoring errors */
    s = rindex(msgfilename, '/');
    if (s != NULL) {
	*s = '\0';
	VenusFlushCallback(msgfilename);    /* Flush the parent directory, too. */
    }
#if LogsYes
    Log(403, "VenusFlushCallback done: VM_close returning");
#endif /* LogsYes */
    sprintf(VM_text, "VM_close succeeded for file ``%s''", msgfilename);
    VM_errordetail = 0;
#if LogsYes
    DoingTiming = 0;
#endif /* LogsYes */
    msgfilename[0] = '\0';
    return EX_OK;
}

static char *host()
{
#define NAMELEN 256
    static char name[NAMELEN] = { '\0' };

    if (name[0] == '\0')
	if (GetHostDomainName(name, NAMELEN) < 0)
	    sprintf(name, "UNKNOWN: %s", UnixError(errno));
	else
	    name[NAMELEN-1] = '\0';
    return name;
}

static enum lockcodes lock(f, name)
    int f;
    char *name;
{
    int SaveErrno;

    errno = 0;
    if (osi_ExclusiveLockNoBlock(f) < 0){
	SaveErrno = errno;
	switch (SaveErrno) {
	    case EACCES:
		return LOCK_OK;	/* Arggh!! */
	    case EWOULDBLOCK:
#ifdef AFS_ENV
	    case VNOVNODE:
#endif /* AFS_ENV */
		return LOCK_FAILED_OK;
	    default:
		sprintf(VM_text, "Flock failed on file ``%s'': %s",
			name, UnixError(SaveErrno));
		VM_errordetail = vm_SystemErrorOffset + SaveErrno;
		if (tfail(SaveErrno)) return LOCK_FAILED_TEMP_ERROR;
		else return LOCK_FAILED_ERROR;
	}
    } else
	return LOCK_OK;
}

static int WriteHeaders(f, fname, returnpath, four, auth, pgmname)
    register FILE *f;
    char *fname, *returnpath, *four, *auth, *pgmname;
{
    int Res;

#if Logs
    Log(410, "WriteHeaders called");
#endif /* Logs */
    CheckAMSConfiguration();
    if (returnpath != NIL && returnpath[0] == '\0') returnpath = NIL;
    if (returnpath == NIL) {
	Res = GetRetPath(fname, &returnpath);
	if (returnpath == NIL || *returnpath == '\0') {
		sprintf(VM_text, "Can't build a return path for %s: %d\n", fname, Res);
		VM_errordetail = vm_OutOfMemory;
		return EX_TEMPFAIL;
	}
    }
    if (*returnpath != '<')	/* Guaranteed to have a return-path now. */
	fprintf(f, "Return-path: <%s>\n", returnpath);
    else
	fprintf(f, "Return-path: %s\n", returnpath);

    if (auth != NIL && auth[0] == '\0') auth = NIL;
    if (auth == NIL) {
	Res = GetAuthInfo(fname, &auth);
	if (Res == 2) {	/* unauthenticated */
		auth = NIL;
	} else if (Res < 0 || auth == NIL || (auth != NIL && auth[0] == '\0')) {
		sprintf(VM_text, "Can't build authentication info for %s: %d\n", fname, Res);
		VM_errordetail = vm_OutOfMemory;
		return EX_TEMPFAIL;
	}
    }
    if (auth != NIL) {	/* either the one passed in or our own authentication */
	fprintf(f, "X-Andrew-Authenticated-as: %s\n", auth);
    }

    fprintf(f, "Received: from %s via %s", host(),
		(pgmname == NIL ? "vmail" : pgmname));
    if (four != NIL) fprintf(f, " for %s", four);
	/*        1234567890	      1234567890 */
    fprintf(f, "\n          ID <%s>;\n          %s", fname, arpadate());
    return 0;
}

#define MAX_MAIL_TRIES	250
#define MAX_PERM_TRIES	10

/* CreateAndInitFile -- create file & write headers */

static int CreateAndInitFile(mailbox, returnpath, four, auth, pgmname)
    char *mailbox;
    char *returnpath, *four, *auth, *pgmname;
{
    register int n, f;
    register bool gotfile;
    int PermTries = 0;	/* Count of permission failures */
    int firsterr;
    char NameExtra[100];

#if Logs
    Log(500, "CreateAndInitFile() called");
#endif /* Logs */
    gotfile = FALSE;
    NameExtra[0] = '\0';
    for (n=0; !gotfile && n<MAX_MAIL_TRIES; ++n) {
	register enum lockcodes value;

	sprintf(msgfilename, "%s/%s%s", mailbox, ams_genid(1), NameExtra);
	/* NOTE: open will fail if file exists */
#if LogsYes
	Log(510, "Try %d: opening mailbox file", n);
#endif /* LogsYes */
	firsterr = 0;
	f = open(msgfilename, O_CREAT|O_WRONLY, 0600);
/* If permission denied, maybe AFS just disappeared: try again to be sure */
	if (f < 0 && errno == EACCES) {
	    firsterr = errno;
	    VenusFlush(mailbox);
#if LogsYes
	    Log(511, "EACCES: Retrying open of mailbox file");
#endif /* LogsYes */
	    f = open(msgfilename, O_CREAT|O_WRONLY, 0600);
	}
#if LogsYes
	Log(512, "Open returns %d, errno %d", f, errno);
#endif /* LogsYes */
	if (f < 0) {
	    sprintf(NameExtra, ".Open.n%d.f%d.e%d", n, firsterr, errno);
	    if (vdown(errno)) {
		/* AFS is down */
		sprintf(VM_text, AFSDownMsg, mailbox, UnixError(errno));
		VM_errordetail = vm_ViceDown;
		return EX_TEMPFAIL;
	    }
	    switch (errno) {
		case EEXIST:	/* Already there, just try again */
				continue;
#ifdef EDQUOT
		case EDQUOT:		/* Over quota */
				sprintf(VM_text, "Can't write in dir %s: %s",
					mailbox, UnixError(errno));
				VM_errordetail = vm_SystemErrorOffset + errno;
				return EX_TEMPFAIL;
#endif /* EDQUOT */
		case ENOSPC:	/* Disk or partition is full--a temporary failure. */
				sprintf(VM_text, "Can't create in dir %s: Local disk or AFS partition is full",
					mailbox);
				VM_errordetail = vm_SystemErrorOffset + errno;
				return EX_TEMPFAIL;
		case EACCES:	/* Permission denied: Venus says this when
				   the file exists and is old in an unreadable place */
				if (++PermTries < MAX_PERM_TRIES) continue;
				/* else stop trying. */
				VM_errordetail = vm_SystemErrorOffset + errno;
				sprintf(VM_text, "Can't create file in dir: %s: %s",
					mailbox, UnixError(errno));
				return EX_TEMPFAIL;
		default:
				VM_errordetail = vm_SystemErrorOffset + errno;
				if (tfail(errno)) {
					sprintf(VM_text, "Temporary failure creating file in dir ``%s'': %s",
						mailbox, UnixError(errno));
					return EX_TEMPFAIL;
				}
				sprintf(VM_text,
					"Can't create file in dir ``%s'': %s",
					mailbox, UnixError(errno));
				return EX_CANTCREAT;
	    }
	}

	/* If here, file created okay */
#if LogsYes
	Log(513, "About to lock mailbox file");
#endif /* LogsYes */
	value = lock(f, msgfilename);
	sprintf(NameExtra, ".Lock.n%d.f%d.e%d.v%d", n, firsterr, errno, value);
#if LogsYes
	Log(514, "Lock of mailbox file returns %d", value);
#endif /* LogsYes */
	switch (value) {
	    case LOCK_OK:
		gotfile = TRUE;
		break;
	    case LOCK_FAILED_OK:
		close(f);
		continue;
	    case LOCK_FAILED_ERROR:
                close(f);
		return EX_CANTCREAT;
            case LOCK_FAILED_TEMP_ERROR:
                close(f);
		return EX_TEMPFAIL;
	    default:
		sprintf(VM_text, "Impossible lock value for ``%s'': %d",
			msgfilename, value);
		VM_errordetail = vm_InternalError;
                close(f);
		return EX_SOFTWARE;
	}
    }

    if (!gotfile) {
	sprintf(VM_text, "Couldn't create message file after %d tries",
		MAX_MAIL_TRIES);
	VM_errordetail = vm_OutOfRetries;
	return EX_CANTCREAT;
    }

    /* Try to get stdio descriptor for file */
#if Logs
    Log(520, "About to do fdopen");
#endif /* Logs */
    errno = ENOMEM;
    msgfile = fdopen(f, "w");
#if Logs
    Log(521, "fdopen returns 0x%x", msgfile);
#endif /* Logs */
    if (msgfile == NULL) {
	sprintf(VM_text, "Fdopen failed for ``%s'': %s",
		msgfilename, UnixError(errno));
	VM_errordetail = vm_OutOfMemory;
        close(f);
	return EX_TEMPFAIL;
    }

    /* Write header lines */
    return WriteHeaders(msgfile, msgfilename, returnpath, four, auth, pgmname);
}

#ifdef TESTINGONLYTESTING
main() {
	char MBox[400], RetPath[400], Auth[400];
	int RC;

	for (;;) {
		printf("Mailbox dir: "); fflush(stdout);
		gets(MBox); if (feof(stdin)) exit(0);
		printf("Return path: "); fflush(stdout);
		gets(RetPath); if (feof(stdin)) exit(0);
		printf("Authentication: "); fflush(stdout);
		gets(Auth); if (feof(stdin)) exit(0);

		RC = VM_open(NIL, MBox,
			(RetPath[0] == '\0' ? NIL : RetPath), NIL,
			(Auth[0] == '\0' ? NIL : Auth), "test-vmail");
		if (RC != 0) {
			fprintf(stderr, "Can't VM_open: %d %d %s\n", 
					RC, VM_errordetail, VM_text);
			exit(RC);
		}
		RC = VM_printf("This is a line.\n");
		if (RC != 0) {
			fprintf(stderr, "Can't VM_printf: %d %d %s\n",
					RC, VM_errordetail, VM_text);
			exit(RC);
		}
		RC = VM_close();
		if (RC != 0) {
			fprintf(stderr, "Can't VM_close: %d %d %s\n",
					RC, VM_errordetail, VM_text);
			exit(RC);
		}
	}
}
#endif /* TESTINGONLYTESTING */
