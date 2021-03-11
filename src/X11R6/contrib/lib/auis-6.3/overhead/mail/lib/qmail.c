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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/qmail.c,v 2.33 1993/09/22 20:16:47 gk5g Exp $";
#endif

/*
		qmail.c -- Subroutines for performing dropoff
			   of a piece of user mail.
*/


/*

*************************

int qmail(dir, tolist, file, returnpath, auth)

    char *dir, *tolist[];
    int file;
    char *returnpath, *auth;

This routine is used to attempt to enqueue a message in a specified
directory according to the queuemail discipline.  If the return
value is not Q_OK, then a printable explanation of the error can be
found in Qmail_ErrMsg.  The complete set of return values can be
found in qmail.h.

    dir		    Null-terminated string naming directory in
		    which to enqueue message.  The current process
		    must have at least IL access to the directory.

    tolist	    Null-terminated list of addresses.  Each
		    address is a null-terminated string.  There
		    must be at least one address.

    file	    Descriptor to be read to obtain message to
		    deliver.  The message is assumed to be
		    preceded by headers (at least From, To,
		    Subject & Date headers) followed by a blank
		    line.  The headers should obey RFC822 syntax.

    returnpath	    This is the return path to be used for this
		    message.  This is a null-terminated string.
		    It should be surrounded by <> characters,
		    although this will not be checked by this
		    routine.

    auth	    This is a null-terminated string that defines
		    the #AUTH field for the SF file.  If this
		    pointer is null, then an #AUTH field will be
		    created from the caller's authentication.

int qmail_ext(dir, tolist, file, returnpath, auth, enq, forstr, holduntil)

    char *dir, *tolist[];
    int file, enq, holduntil;
    char *returnpath, *auth, *forstr;

Like qmail, but includes extra parameters giving (in ``enq'') the time(0) value at which the mail request was enqueued, (in ``forstr'') the string to be used in making up the FOR clause of the Received: header, and (in ``holduntil'') a time(0) value before which not to initiate delivery (NOT IMPLEMENTED).



int tryvicequeues(tolist, file, returnpath, auth)

    char *tolist[];
    int file;
    char *returnpath, *auth;

This routine is used to attempt to enqueue a message to any one of the
available Vice queues.  The number of Vice queues & their location
is specified in the include file mailconf.h.  This routine has the
same return codes as qmail.

    tolist	    Null-terminated list of addresses.  Each
		    address is a null-terminated string.  There
		    must be at least one address.

    file	    Descriptor to be read to obtain message to
		    deliver.  The message is assumed to be
		    preceded by headers (at least From, To,
		    Subject & Date headers) followed by a blank
		    line.  The headers should obey RFC822 syntax.

    returnpath	    This is the return path to be used for this
		    message.  This is a null-terminated string.
		    It should be surrounded by <> characters,
		    although this will not be checked by this
		    routine.

    auth	    This is a null-terminated string that defines
		    the #AUTH field for the SF file.  If this
		    pointer is null, then no #AUTH field will be
		    written.

int tryvicequeues_ext(tolist, file, returnpath, auth, enq, forstr, holduntil)

    char *tolist[];
    int file, enq, holduntil;
    char *returnpath, *auth, *forstr;

This routine is to tryvicequeues as qmail_ext is to qmail.  It takes extra parameters: ``enq'' to give the time(0) value at which this request was first queued, ``forstr'' to give a string that should be included in the FOR clause of the Received: line written here, and ``holduntil'' to give a time(0) value before which delivery shouldn't be attempted (NOT IMPLEMENTED).

int tryvicequeues_dir(tolist, file, returnpath, auth, enq, forstr, holduntil, dirname, pfx)
    char *tolist[];
    int file, enq, holduntil;
    char *returnpath, *auth, *forstr, *dirname, *pfx;
int tryvicequeues_cell(tolist, file, returnpath, auth, enq, forstr, holduntil, cellname, pfx)
    char *tolist[];
    int file, enq, holduntil;
    char *returnpath, *auth, *forstr, *cellname, *pfx;

These two routines are just like tryvicequeues_ext except that they implement two extra parameters each: the directory or cell name in which queues should be found, and the prefix that must match the entries in that directory (or the directory name derived from the cell name) in order for those entries to be considered by this routine.  Thus, to enqueue mail in any of the queues (directories) matched by the shell wildcard /abc/def/ghi/GG*, tryvicequeues_dir would be passed as its final two arguments "/abc/def/ghi" and "GG".

*/

#include <andrewos.h> /* sys/time.h sys/file.h */
#include <errno.h>
#include <stdio.h>
#include <sys/param.h>
#include <fdplumb.h>
#include <sys/stat.h>
#include <util.h>
#include "mail.h"
#include "mailconf.h"
#include "qmail.h"

extern int errno;
extern char *UnixError(), *arpadate();

typedef unsigned char bool;

#define FALSE	0
#define TRUE	1

#define NIL	0

char Qmail_ErrMsg[2*MAXPATHLEN];

/* Forward declarations */

/* Tunable parameters */

#define MAXRETRIES  10

int qmail_ext(dir, tolist, file, returnpath, auth, enq, forstr, holduntil)
char *dir, *tolist[];
int file, enq, holduntil;
char *returnpath, *auth, *forstr;
{
    static bool virgin = TRUE;
    static char host[200];
    register int i, rc;
    bool rewind;

    /* Check parameters */
    if (tolist == NIL || tolist[0] == NIL || returnpath == NIL) {
	strcpy(Qmail_ErrMsg, "Empty `tolist'/`returnpath'");
	return Q_BAD_PARMS;
    }

    /* Do 1-time initialization */
    if (virgin) {
	if (GetHostDomainName(host, sizeof host) < 0) {
	    sprintf(Qmail_ErrMsg, "Can't find host name: %d", UnixError(errno));
	    return Q_OSERR;
	}
	virgin = FALSE;
    }
    CheckAMSConfiguration();

    /* Loop & try to create files in dir */
    rewind = TRUE;
    if (auth == NIL) {	/* Give this a default value if we can. */
	rc = GetAuthInfo(dir, &auth);
	if (auth != NIL) if (auth[0] == '\0') auth = NIL;	/* don't use a zero-length string. */
    }
    for (i=0; i<MAXRETRIES; i++) {
	char suffix[100], sf[MAXPATHLEN+1], qf[MAXPATHLEN+1], gf[MAXPATHLEN+1];
	char *extra;

	/* Construct file names */
	strcpy(suffix, ams_genid(1));
	extra = (AMS_UseShortFileNames ? "" : "F.");
	sprintf(sf, "%s/S%s%s", dir, extra, suffix);
	sprintf(qf, "%s/Q%s%s", dir, extra, suffix);
	sprintf(gf, "%s/G%s%s", dir, extra, suffix);

	/* See if necessary to rewind input file */
	if (rewind && lseek(file, 0, 0) < 0) {
	    sprintf(Qmail_ErrMsg, "Lseek failed: %s", UnixError(errno));
	    return Q_CANT_REWIND;
	}
	rewind = FALSE;

	/* Now try to write them */
	rc = writesf(sf, tolist, returnpath, auth, enq, forstr);
	if (rc == Q_OK) {
	    rewind = TRUE;
	    rc = writeqf(qf, file, host);
	    if (rc == Q_OK) {
		rc = writegf(gf);
		if (rc == Q_OK) {
		    sprintf(Qmail_ErrMsg,
			    "Queued in \"%s\" as \"%s\"",
			    dir, suffix);
		    return Q_OK;
		} else {
		    unlink(qf);
		    unlink(sf);
		}
	    } else {
		unlink(sf);
	    }
	}
#ifdef NOTDEF
	oops--might have gotten EEXIST
	  if (rc == Q_TEMP_FAIL)
	      break;	/* No use trying again right now */
#endif /* NOTDEF */
    }

    /* Couldn't write them */
    return rc;
}

int qmail(dir, tolist, file, returnpath, auth)
char *dir, *tolist[];
int file;
char *returnpath, *auth;
{
    return qmail_ext(dir, tolist, file, returnpath, auth, 0, NIL, 0);
}

static int tryvicequeues_intern(tolist, file, returnpath, auth, enq, forstr, holduntil, dirname, pfx, Cell)
char *tolist[];
int file, enq, holduntil;
char *returnpath, *auth, *forstr, *dirname, *pfx, *Cell;
{
    int start, q, rc = 0;
    register DIR *dirp = NIL;
    register DIRENT_TYPE *dp;
    static char oldDirName[200] = {0377, '\0'}, oldPfx[30] = {0377, '\0'};
    static int oldDirMatch = -1;
    char ThisDir[MAXPATHLEN+1];
    int pfxlen, MatchCount;
    char FileCell[100];
    long int the_time;

    pfxlen = strlen(pfx);
    /* Maybe we cached the old size of matching entries.  Maybe we can cache it for next time. */
    if (oldDirMatch <= 0 || strcmp(dirname, oldDirName) != 0 || strcmp(pfx, oldPfx) != 0) {
	errno = 0;			/* not cached */
	if (Cell != NULL) {
	    if (GetCellFromFileName(dirname, FileCell, sizeof(FileCell)) == 0
		&& ULstrcmp(FileCell, Cell) != 0) {
		sprintf(Qmail_ErrMsg, "Bad cell config: dir %s in cell %s, not %s",
			dirname, FileCell, Cell);
		return Q_OSERR;
	    }
	}
	dirp = opendir(dirname);
	if (dirp == NIL) {
	    sprintf(Qmail_ErrMsg, "Can't open dir %s: %s", dirname, UnixError(errno));
	    return ((errno == 0 || tfail(errno) || errno == ENFILE) ? Q_TEMP_FAIL : Q_DIR_ERR);
	}
	MatchCount = 0;
	while ((dp = readdir(dirp)) != NIL) {
	    if (
		DIRENT_NAMELEN(dp) >= pfxlen &&
		strncmp(pfx, dp->d_name, pfxlen) == 0
		&& strcmp(dp->d_name, ".") != 0 && strcmp(dp->d_name, "..") != 0)
		++MatchCount;
	}
	/* Were there any matches? */
	if (MatchCount <= 0) {
	    sprintf(Qmail_ErrMsg, "No dirs under %s match prefix \"%s\"", dirname, pfx);
	    closedir(dirp);
	    return ((errno == 0 || tfail(errno) || errno == ENFILE) ? Q_TEMP_FAIL : Q_DIR_ERR);
	}
	/* Can we cache the count for next time? */
	if (strlen(dirname) < sizeof(oldDirName) && strlen(pfx) < sizeof(oldPfx)) {
	    strcpy(oldDirName, dirname); strcpy(oldPfx, pfx);
	    oldDirMatch = MatchCount;
	}
	rewinddir(dirp);		/* Leave directory opened, at its beginning. */
    } else {
	MatchCount = oldDirMatch;		/* use cached value */
	errno = 0;		/* Leave directory opened, at its beginning. */
	dirp = opendir(dirname);
	if (dirp == NIL) {
	    sprintf(Qmail_ErrMsg, "Can't open dir %s: %s", dirname, UnixError(errno));
	    return ((errno == 0 || tfail(errno) || errno == ENFILE) ? Q_TEMP_FAIL : Q_DIR_ERR);
	}
    }
    the_time = osi_GetSecs();
    start = the_time % MatchCount;
    q = 0;		/* Cycle to an arbitrary point in the set of matching directories */
    while (q < start && (dp = readdir(dirp)) != NIL) {
	if (
	    DIRENT_NAMELEN(dp) >= pfxlen &&
	    strncmp(pfx, dp->d_name, pfxlen) == 0
	    && strcmp(dp->d_name, ".") != 0 && strcmp(dp->d_name, "..") != 0) ++q;
    }
    q = 0;		/* Now cycle through the matching dirs, trying to get our message queued */
    start = 0;
    while (q < MatchCount) {
	errno = 0;
	dp = readdir(dirp);
	while (dp == NIL) {
	    if (++start > 2) {
		sprintf(Qmail_ErrMsg, "Error (2) cycling through %s", dirname);
		break;
	    }
	    rewinddir(dirp);
	    dp = readdir(dirp);
	}
	if (dp == NIL) {
	    sprintf(Qmail_ErrMsg, "Error (1) cycling through %s: %s",
		    dirname, UnixError(errno));
	    rc = (errno == 0 || tfail(errno) || errno == ENFILE) ? Q_TEMP_FAIL : Q_DIR_ERR;
	    break;
	}
	if (
	    DIRENT_NAMELEN(dp) >= pfxlen &&
	    strncmp(pfx, dp->d_name, pfxlen) == 0
	    && strcmp(dp->d_name, ".") != 0 && strcmp(dp->d_name, "..") != 0) {
	    ++q;		/* We're trying a match */
	    sprintf(ThisDir, "%s/%s", dirname, dp->d_name);
	    if (Cell != NULL) {
		if (GetCellFromFileName(ThisDir, FileCell, sizeof(FileCell)) == 0
		    && ULstrcmp(FileCell, Cell) != 0) {
		    sprintf(Qmail_ErrMsg,
			    "Bad cell config: dir %s is in cell %s, not cell %s",
			    ThisDir, FileCell, Cell);
		    rc = Q_OSERR;
		    break;
		}
	    }
	    rc = qmail_ext(ThisDir, tolist, file, returnpath, auth, enq, forstr, holduntil);

	    /* Check for permament problems */
	    if (rc == Q_OK || rc == Q_BAD_PARMS || rc == Q_CANT_REWIND) break;
	}
    }
    closedir(dirp);
    return rc;
}

int tryvicequeues_dir(tolist, file, returnpath, auth, enq, forstr, holduntil, dirname, pfx)
char *tolist[];
int file, enq, holduntil;
char *returnpath, *auth, *forstr, *dirname, *pfx;
{
    return tryvicequeues_intern(tolist, file, returnpath, auth, enq, forstr, holduntil, dirname, pfx, NULL);
}

int tryvicequeues_cell(tolist, file, returnpath, auth, enq, forstr, holduntil, cellname, pfx)
char *tolist[];
int file, enq, holduntil;
char *returnpath, *auth, *forstr, *cellname, *pfx;
{
    char DirDir[MAXPATHLEN+1];

    CheckAMSConfiguration();
    strcpy(DirDir, CellCommonPrefix);
    LCappend(DirDir, cellname);
    strcat(DirDir, CellCommonSuffix);
    strcat(DirDir, CellCommonMailQueueDirSuffix);
    return tryvicequeues_intern(tolist, file, returnpath, auth, enq, forstr, holduntil, DirDir, pfx, cellname);
}

int tryvicequeues_ext(tolist, file, returnpath, auth, enq, forstr, holduntil)
char *tolist[];
int file, enq, holduntil;
char *returnpath, *auth, *forstr;
{
    int rc;
    char *CellToTry;
    struct CellAuth *ca;

    CheckAMSConfiguration();
    ca = NIL;
    FindAMSHomeCell(&ca);
    CellToTry = (ca != NIL ? ca->CellName : WorkstationCell);
    rc = tryvicequeues_cell(tolist, file, returnpath, auth, enq, forstr, holduntil,
			     CellToTry, MailQueueNamePrefix);
    return rc;
}

int tryvicequeues(tolist, file, returnpath, auth)
char *tolist[];
int file;
char *returnpath, *auth;
{
    return tryvicequeues_ext(tolist, file, returnpath, auth, 0, NIL, 0);
}

static int create(name, f, filekind, mode)
char *name, *filekind;
FILE **f;
int mode;
{
    register int fd;

    fd = open(name, O_CREAT|O_WRONLY, mode);
    if (fd < 0)
	if (vdown(errno)) {
	    sprintf(Qmail_ErrMsg,
		    "Vice is down for \"%s\": %s", name, UnixError(errno));
	    return Q_TEMP_FAIL;
	} else {
	    int err = errno;
	    sprintf(Qmail_ErrMsg, 
		    "Can't create %s file \"%s\": %s",
		    filekind, name, UnixError(errno));
	    return (err == EEXIST || err == EFBIG || err == ENFILE || tfail(errno) ? Q_TEMP_FAIL : Q_CANT_CREAT);
	}

    *f = fdopen(fd, "w");
    return Q_OK;
}

static int tryclose(f, name)
FILE *f;
char *name;
{
    /* Try to close it */
    if (vfclose(f) == EOF) {
	int err = errno;
	sprintf(Qmail_ErrMsg,
		"Close failed for \"%s\": %s", name, UnixError(err));
	return (tfail(err) ? Q_TEMP_FAIL : Q_FILE_ERR);
    } else
	return Q_OK;
}

static void quote(s, f)
register char *s;
register FILE *f;
{
    fputc('|', f);

    for (; *s!='\0'; s++)
	switch (*s) {
	    case '\n':  if (*(s+1) == ' ')
		s++;
		break;
	    case '|':   fputc('|', f);
	    default:    fputc(*s, f);
	}

    fputc('|', f);
}

static int writesf(name, tolist, returnpath, auth, enq, forstr)
char *name, *tolist[], *returnpath, *auth, *forstr; int enq;
{
    FILE *f;
    register int rc, i;

    /* Try to open the shadow file */
    rc = create(name, &f, "shadow", 0400);
    if (rc != Q_OK) return rc;

    /* File is open; now write it */
    fputs("#From ", f);
    quote(returnpath, f);
    fputs("\n#To", f);
    for (i=0; tolist[i]!=NIL; i++) {
	fputc(' ', f);
	quote(tolist[i], f);
    }
    if (auth != NIL) {fputs("\n#Auth ", f); quote(auth, f);}
    if (enq != 0) fprintf(f, "\n#EQD %d", enq);
    if (forstr != NIL) {
	fputs("\n#For ", f);
	quote(forstr, f);
    }
    fputc('\n', f);

    return tryclose(f, name);
}

static int writeqf(name, file, host)
char *name;
register int file;
char *host;
{
    FILE *f;
    register int rc, bufsize;
    struct stat buf;
    char buffer[8192];

    /* Try to create the file */
    rc = create(name, &f, "queue", 0400);
    if (rc != Q_OK) return rc;

    /* Write headers 1st */
    fprintf(f,
	     "Received: from %s via qmail\n          ID <%s>;\n          %s",
	     host, name, arpadate());

    /* Now write body */
#if !defined(AIX) && !defined(M_UNIX)&& !defined(SGI_4D_ENV)
    if (fstat(file, &buf) < 0) {
	int err = errno;
	fclose(f);
	sprintf(Qmail_ErrMsg, "Can't stat message file: %s", UnixError(err));
	return ((tfail(err) || err == ENFILE) ? Q_TEMP_FAIL : Q_FILE_ERR);
    }
    bufsize = buf.st_blksize;
    if (bufsize > sizeof buffer) bufsize = sizeof buffer;
#else /* #ifndef AIX */
    bufsize = sizeof buffer;
#endif /* #ifndef AIX */
    for (;;) {
	register int n;

	n = read(file, buffer, bufsize);
	if (n < 0) {
	    int err = errno;
	    sprintf(Qmail_ErrMsg, "Read failed: %s", UnixError(err));
	    fclose(f);
	    return ((tfail(err) || err == ENFILE) ? Q_TEMP_FAIL : Q_FILE_ERR);
	}
	if (n == 0) break;

	if (fwriteallchars(buffer, n, f) <= 0) {
	    int err = errno;
	    sprintf(Qmail_ErrMsg,
		    "Write failed to \"%s\": %s", name, UnixError(err));
	    fclose(f);
	    return ((tfail(err) || err == ENFILE) ? Q_TEMP_FAIL : Q_FILE_ERR);
	}
    }

    return tryclose(f, name);
}

static int writegf(name)
char *name;
{
    FILE *f;
    register int rc;

/* create in mode 0600, not 0400, so POSIX-compliant readers can flock/lockf the file */
    rc = create(name, &f, "go", 0600);
    return (rc != Q_OK ? rc : tryclose(f, name));
}

#ifdef TESTINGONLYTESTING
main()
{
    char Dest[200], FileName[1000], Temp1[1000], Temp2[1000];
    int fd; char *list[2]; int RC;

    printf("Destination: "); fflush(stdout);
    gets(Dest);
    printf("Filename: "); fflush(stdout);
    gets(FileName);
    list[0] = Dest;
    list[1] = NIL;
    fd = open(FileName, O_RDONLY, 0644);
    if (fd <= 0) {fprintf(stderr, "Unopenable file ``%s'': %s\n", FileName, UnixError(errno)); exit(1);}
    for (;;) {
	printf("Dir: "); fflush(stdout);
	gets(Temp1);
	if (feof(stdin)) exit(0);
	if (Temp1[0] != '\0') {
	    printf("Pfx: "); fflush(stdout);
	    gets(Temp2);
	    RC = tryvicequeues_dir(list, fd, "<cfe+@andrew.cmu.edu>",
				   NIL, 0, NIL, 0, Temp1, Temp2);
	    printf("tryvicequeues_dir returns: %d, ``%s''\n", RC, Qmail_ErrMsg);
	}
	printf("Cell: "); fflush(stdout);
	gets(Temp1);
	if (feof(stdin)) exit(0);
	if (Temp1[0] != '\0') {
	    printf("Pfx: "); fflush(stdout);
	    gets(Temp2);
	    RC = tryvicequeues_cell(list, fd, "<cfe+@andrew.cmu.edu>",
				    NIL, 0, NIL, 0, Temp1, Temp2);
	    printf("tryvicequeues_cell returns: %d, ``%s''\n", RC, Qmail_ErrMsg);
	}
	printf("normal: "); fflush(stdout);
	gets(Temp1);
	if (feof(stdin)) exit(0);
	if (Temp1[0] != '\0') {
	    RC = tryvicequeues_ext(list, fd, "<cfe+@andrew.cmu.edu>",
				   NIL, 0, NIL, 0);
	    printf("tryvicequeues_ext returns: %d, ``%s''\n", RC, Qmail_ErrMsg);
	}
    }
}
#endif /* TESTINGONLYTESTING */
