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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/undigest/RCS/undigest.c,v 1.16 1993/09/21 22:06:37 gk5g Exp $";
#endif

/* undigest: Split internet digests into component mail messages */

/* About 80% of internet digests use a common format, which is of the form: */
/*
Table of contents

[70 dashes]

Message (headers, blank line, body)

[30 dashes]

Message (headers, blank line, body)

[30 dashes]

Closing notes
*/

/* This program will reliably take files from a directory and split them into component messages according to this format.  A new file is generated in a temporary directory (on vice) for each message in the digest.  The actual message is renamed into the temporary directory upon completion, and the presence of it is checked for before moving files from the temporary directory to the destination directory (mailbox).  For example, foo is split into foo.1, foo.2, foo.3, ..., foo.n.  foo.0 existed for the purpose of holding headers, but is deleted.  Then foo is moved into the same directory as foo.i.  When Tidy is called, it generates a list of files x.y such that the file x exists, so it would have the list of foo.1...foo.n.  These are moved into the destination, and then all other files in the temporary directory (including foo) are deleted.  If this process is interrupted at any point,  no mail will be lost or duplicated, since the flag file must exist because of the semantics of rename.  */

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errprntf.h>

#include <util.h>
#include <mail.h>
#include <svcconf.h>

static char topline[] = "----------------------------------------------------------------------\n";
static char sepline[] = "------------------------------\n";

extern int errno;

/* filename for error messages.  Since errprintf only does this in conjunction with console, this is really ignored. */
static char *Log = NULL;

/* name of facility for error messages. */
static char *prog = "undigest";

struct dirlist {
    char *name;
    time_t time;
    struct dirlist *next;
};

/******************************************************************************/

main(argc, argv)
int argc;
char **argv;
{
    int	naptime	= 360;	    /* polling interval */
    /* -->>>> NOTE: the following MUST be on the same volume for renames to work <<<---- */
    static char *MailboxDir = "/afs/andrew.cmu.edu/usr0/digestbb/Mailbox/";
    static char *TempDir = "/afs/andrew.cmu.edu/usr0/digestbb/temp/";
    static char *DestDir = "/afs/andrew.cmu.edu/usr0/digestbb/ReadyBox/";

    CheckServiceConfiguration();

    while (--argc) {
	if (**++argv == '-') {
	    switch (argv[0][1]) {
		case 's': case 'S':
		    naptime = atoi(*++argv);
		    argc--;
		    break;
		case 'm': case 'M':
		    MailboxDir = *++argv;
		    argc--;
		    break;
		case 't': case 'T':
		    TempDir = *++argv;
		    argc--;
		    break;
		case 'r': case 'R':
		    DestDir = *++argv;
		    argc--;
		    break;
		case 'l': case 'L':
		    Log = *++argv;
		    argc--;
		    break;
		default:
		    errprintf(prog, ERR_CRITICAL, Log, 0, "Unknown switch '%c'", argv[0][1]);
		case 'x': case 'X':
		    usage();
	    }
	} else {
	    errprintf(prog, ERR_CRITICAL, Log, 0, "Unknown item on command line '%s'", *argv);
	    usage();
	}
    }

    Tidy(TempDir, DestDir);	/* clean up any previous invocation */

    if (chdir(MailboxDir)) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
			"Couldn't chdir to mailboxdir: %s", UnixError(errno));
	exit(-1);
    }

    for (;;) {
	int filecount;
	struct dirlist *maillist;
	struct dirlist *mp;

	/* get a list of files in the current (Mailbox) directory */
	filecount = readlist(&maillist);
	errprintf(prog, ERR_MONITOR, Log, 0, "Found %d files", filecount);

	/* split each file into components */
	for (mp = maillist; mp; mp = mp->next) {
	    errprintf(prog, ERR_MONITOR, Log, 0, "Splitting %s", mp->name);
	    if ((filecount = split(mp->name, TempDir)) < 0) {
		errprintf(prog, ERR_CRITICAL, Log, 0, "Couldn't split. (error is %d)", filecount);
		continue;
	    } else {
		errprintf(prog, ERR_MONITOR, Log, 0, "Split into %d files", filecount);
	    }
	}

	/* move results into destination directory */
	Tidy(TempDir, DestDir);

	errprintf(prog, ERR_MONITOR, Log, 0, "Sleeping %d seconds", naptime);
	sleep(naptime);
    }
}

/******************************************************************************/

/* Generate a linked list of filenames from the current directory, and sort them by time and name.  Insertion sort is used, since the number of files at any given polling point should probably never exceed 100. */

static int readlist(listp)
struct dirlist **listp;
{
    DIR *dirp;
    DIRENT_TYPE *dp;
    struct dirlist head;
    struct dirlist *d2;
    struct dirlist *d;
    struct stat statbuf;
    int nument = 0;

    head.time = 0;
    head.next = NULL;
    dirp = opendir(".");
    if (dirp == NULL) {
	errprintf(prog, ERR_CRITICAL, Log, 0, "Couldn't opendir '.': %s", UnixError(errno));
	return 0;
    }

    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	if (*dp->d_name	== '.')	continue;   /* ignore invisible files */
	if (stat(dp->d_name, &statbuf)) {
	    errprintf(prog, ERR_CRITICAL, Log, 0,
		"Couldn't stat %s: %s", dp->d_name, UnixError(errno));
	    return 0;
	}
	if (!statbuf.st_size) continue;	    /* ignore 0-length files */

	/* allocate and store in element */
	d2 = (struct dirlist *) malloc(sizeof(struct dirlist));
	if (d2 == NULL) return 0;
	d2->name = (char *) malloc(strlen(dp->d_name)+1);
	if (d2->name == NULL) return 0;
	strcpy(d2->name, dp->d_name);
	d2->time = statbuf.st_mtime;

	/* insert into sorted linked-list structure */
	for (d = &head; d->next && (d2->time > d->next->time); d = d->next) ;
	for (; d->next && (d2->time == d->next->time)
			&& (strcmp(d2->name, d->next->name)>0); d = d->next) ;
	d2->next = d->next;
	d->next = d2;
	++nument;
    }
    closedir(dirp);
    *listp = head.next;
    return nument;
}

/******************************************************************************/

/* Clean out the temporary directory.  Files from digests which have been successfully split are moved into the destination directory, and files from incomplete splits are deleted.  Success is indicated by the presence of a flag file for that digest, which is deleted after the last file is moved into the destination directory */

static int Tidy(dirname, destdir)
char *dirname, *destdir;
{
    DIR *dirp;
    char name[MAXPATHLEN+1];
    char *np;
    char newname[MAXPATHLEN+1];
    char *newp;
    DIRENT_TYPE *dp;
    struct stat statbuf;
    struct dirlist allfiles;
    struct dirlist *keep;
    struct dirlist ok;
    struct dirlist *d;

    /* read in a sorted list of filenames */
    dirp = opendir(dirname);
    if (dirp == NULL) return 0;
    allfiles.next = NULL;
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	struct dirlist *d2;

	if (*dp->d_name == '.')	continue;   /* ignore invisible files */
	d2 = (struct dirlist *) malloc(sizeof(struct dirlist));
	d2->name = (char *) malloc(strlen(dp->d_name)+1);
	strcpy(d2->name, dp->d_name);
	for (d = &allfiles; d->next && (strcmp(d2->name,d->next->name)>0); d = d->next) ;
	d2->next = d->next;
	d->next = d2;
    }
    closedir(dirp);

    /* Split into a new list of filenames that are "successful", that is, a flag file with a shorter name exists.  */
    keep = NULL;
    for (d = allfiles.next; d != NULL;) {
	int len;
	struct dirlist *d2;

	len = strlen(d->name);
	for (d2 = d->next; d2 != NULL && !strncmp(d->name, d2->name, len); ) {
	    struct dirlist *newhead;
	    
	    d->next = d2->next;
	    newhead = d2;
	    d2 = d2->next;
	    newhead->next = keep;
	    keep = newhead;
	}
	d = d2;
    }

    /* Build a new list sorted by time from keep.  Again, insertion sort makes the new list */
    strcpy(name, dirname);	/* base of absolute pathname */
    np = name + strlen(name);	/* varying part of absolute pathname */
    ok.next = NULL;
    for (; keep != NULL;) {
	struct dirlist *d2;

	d2 = keep;
	keep = keep->next;
	strcpy(np, d2->name);
	if (stat(name, &statbuf)) {
	    errprintf(prog, ERR_CRITICAL, Log, 0,
		"Couldn't stat %s: %s", name, UnixError(errno));
	    exit(-1);
	}
	d2->time = statbuf.st_mtime;
	for (d = &ok; d->next && (d2->time > d->next->time); d = d->next) ;
	d2->next = d->next;
	d->next = d2;
    }

    /* move the list of "good" files into the destination directory, destroying the list as we go. */
    strcpy(newname, destdir);		/* base of absolute pathname */
    newp = newname + strlen(newname);	/* varying part of absolute pathname */
    for (d = ok.next; d != NULL;) {
	struct dirlist *d2;

	strcpy(np, d->name);
	strcpy(newp, d->name);
	if (rename(name, newname)) {
	    errprintf(prog, ERR_CRITICAL, Log, 0,
		"Couldn't rename %s: %s", name, UnixError(errno));
	    exit(-1);
	}
	d2 = d;
	d = d->next;
	free(d2->name);
	free(d2);
    }

    /* delete the files we didn't move (includes the flag file), destroying the list as we go. */
    for (d = allfiles.next; d != NULL;) {
	struct dirlist *d2;

	strcpy(np, d->name);
	if (unlink(name)) {
	    errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error unlinking file %s: %s", name, UnixError(errno));
	    exit(-1);
	}
	d2 = d;
	d = d->next;
	free(d2->name);
	free(d2);
    }
    return 0;
}

/******************************************************************************/

/* Take a single file in the current directory, presumably a digest, and split it into a number of files in the temporary directory.  When done, rename the file into the temporary directory to flag success. */

static int split(filename, tempdir)
char *filename, *tempdir;
{
    FILE *in;
    FILE *out=NULL;
    FILE *headers;
    char *match=topline;
    int outcount, newfile, maybenew, len;
    int AnyReSent, SawMsgId, SawReSentMsgId, InHeader;
    auto char outname[MAXPATHLEN+1];
    auto char headername[MAXPATHLEN+1];
    auto char buf[1024+1];
    auto char buf2[1024+1];
#define SAVHDRSIZ 5000
    auto char HdrCopy[SAVHDRSIZ+1];
    char *shp; int shcount;
    char *bp;

    shp = HdrCopy;
    HdrCopy[0] = '\0';
    shcount = SAVHDRSIZ;
    if ((in = fopen(filename, osi_F_READLOCK)) == NULL) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"Couldn't open %s: %s", filename, UnixError(errno));
	return(-1);
    }

    /* Don't fight with other invocations over a mail file */
    if (osi_ExclusiveLockNoBlock(fileno(in))) {
	if (errno == EWOULDBLOCK)
	    errprintf(prog, ERR_WARNING, Log, 0, "File %s is already locked", filename);
	else
	    errprintf(prog, ERR_CRITICAL, Log, 0, "Error locking %s: %s", filename, UnixError(errno));
	fclose(in);
	return(-1);
    }

    AnyReSent = SawMsgId = SawReSentMsgId = InHeader = 0;
    outcount = maybenew = 0;
    newfile = 1;
    /* read all headers into a file, since they have to be copied into each message */
    sprintf(headername, "%s%s.%03d", tempdir, filename, outcount++);
    if ((headers = fopen(headername, "w")) == NULL) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error opening %s: %s", headername, UnixError(errno));
	fclose(in);
	return(-2);
    }
    len = 1;
    while (fgets(buf, sizeof(buf), in) != NULL) {
	len = strlen(buf);
	if (len == 1) break;  /* a blank line indicates end of headers. */
	fputs(buf, headers);
    }
    if (safeclose(headers)) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error closing headers %s: %s", headername, UnixError(errno));
	return(-3);
    }
    if (len != 1) {	/* This shouldn't ever happen in a mail file */
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"No end of headers %s: %s", filename, UnixError(errno));
	unlink(headername);
	fclose(in);
	return(-4);
    }
    if ((headers = fopen(headername, "r")) == NULL) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error reopening %s: %s", headername, UnixError(errno));
	fclose(in);
	return(-5);
    }

    /* everything else that falls between message delimiters gets copied into a new file */
    while (fgets(buf, sizeof(buf), in) != NULL) {
	if (maybenew > 0) {	    /* check for header or digest closing */
	    if (blankp(buf)) {	    /* handle more than one newline by counting */
		maybenew++;
		continue;
	    }
	    if (ULstlmatch(buf, "end of ")) break;
	    for (bp = buf; *bp && *bp != ':' && *bp != ' '; bp++) ;
	    if (*bp == ':') {
		newfile = 1;
		match = sepline;    /* All remaining delimiters should be separators */
	    } else {
		fputs(sepline, out);
		while (--maybenew > 0) fputs("\n", out);
	    }
	    maybenew = 0;
	}
	if (newfile) {
	    newfile = 0;
	    if (out && safeclose(out)) {
		errprintf(prog, ERR_CRITICAL, Log, 0,
			"Error closing %s: %s", outname, UnixError(errno));
		fclose(in);
		unlink(headername);
		fclose(headers);
		return(-6);
	    }
	    sprintf(outname, "%s%s.%03d", tempdir, filename, outcount++);
	    sleep(1);	    /* WARNING: this is a crock to try to guarantee unique timestamps!!! */
	    if ((out = fopen(outname, "w")) == NULL) {
		errprintf(prog, ERR_CRITICAL, Log, 0,
			"Error opening %s: %s", outname, UnixError(errno));
		fclose(in);
		unlink(headername);
		fclose(headers);
		return(-7);
	    }
	    /* copy initial headers to new file */
	    AnyReSent = SawMsgId = SawReSentMsgId = 0;
	    InHeader = 1;
	    shp = HdrCopy;
	    HdrCopy[0] = '\0';
	    shcount = SAVHDRSIZ;
	    rewind(headers);
	    fputs("X-Andrew-Authenticated-as: 0\n", out);
	    while(fgets(buf2, sizeof(buf2), headers)) {
		/* modify initial headers to avoid conflicts past the Table of Contents */
		if (outcount > 2) {	/* in the messages past the first one. */
		    if (ULstlmatch(buf2, "Date:")
		      || ULstlmatch(buf2, "Reply-To:")
		      || ULstlmatch(buf2, "From:")
		      || ULstlmatch(buf2, "Sender:")
		      || ULstlmatch(buf2, "Subject:")
		      || ULstlmatch(buf2, "To:")) {
			fputs("X-Digest-", out); fputs(buf2, out);
		    } else if (ULstlmatch(buf2, "Message-ID")) {
			fputs("References", out); fputs(&buf2[10], out);
		    } else if (ULstlmatch(buf2, "ReSent-Message-ID")) {
			fputs("References", out); fputs(&buf2[17], out);
		    } else {
			fputs(buf2, out);
		    }
		} else {	/* still writing initial message */
		    fputs(buf2, out);
		}
	    }
	    if (outcount == 2) {fputs("\n", out); InHeader = 0;}    /* blank line before TOC */
	}
	if (strcmp(match, buf))	{   /* inside delimiters, keep line */
	    if (blankp(buf)) {
		if (InHeader) {
			if (AnyReSent) {
			    if (!SawReSentMsgId) {
				char *S, *ID;
				ID = ams_genid(0);
				for (S = ID; *S != '\0'; ++S) if (*S == ':') *S = '_';
				fprintf(out, "ReSent-Message-ID: <digest.%s@%s>\n",
					ID, WorkstationCell);
			    }
			} else {
			    if (!SawMsgId) {
				char *S, *ID;
				ID = ams_genid(0);
				for (S = ID; *S != '\0'; ++S) if (*S == ':') *S = '_';
				fprintf(out, "Message-ID: <digest.%s@%s>\n",
					ID, WorkstationCell);
			    }
			}
		}
		fputs("\n", out);
		InHeader = 0;
		if (HdrCopy[0] != '\0') Capture(HdrCopy);
	    } else {
		fputs(buf, out);
	    }
	    if (InHeader) {
		if (ULstlmatch(buf, "ReSent-Date") || ULstlmatch(buf, "ReSent-From")) {
			AnyReSent = 1;
		} else if (ULstlmatch(buf, "ReSent-Message-ID")) {
			SawReSentMsgId = 1;
		} else if (ULstlmatch(buf, "Message-ID")) {
			SawMsgId = 1;
		}
		len = strlen(buf);
		if (len < shcount) {
		    strcpy(shp, buf);
		    shcount -= len;
		    shp += len;
		}
	    }
	    continue;
	}
	maybenew = 1;
    }
    if (out == NULL) {	    /* no message text */
	sprintf(outname, "%s%s.%03d", tempdir, filename, outcount++);
	sleep(1);	    /* WARNING: this is a crock to try to guarantee unique timestamps!!! */
	if ((out = fopen(outname, "w")) == NULL) {
	    errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error opening %s: %s", outname, UnixError(errno));
	    fclose(in);
	    unlink(headername);
	    fclose(headers);
	    return(-7);
	}
	/* copy headers to new file */
	rewind(headers);
	while(fgets(buf2, sizeof(buf2), headers)) fputs(buf2, out);
    }	
    unlink(headername);	    /* delete the header file */
    fclose(headers);
    if (safeclose(out)) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error closing %s: %s", outname, UnixError(errno));
	return(-8);
    }

    /* flag success by moving the mail file into the temporary directory */
    sprintf(outname, "%s%s", tempdir, filename);
    if (rename(filename, outname)) {
	errprintf(prog, ERR_CRITICAL, Log, 0,
		"Error renaming %s: %s", filename, UnixError(errno));
	return(-7);
    }
    fclose(in);
    return(outcount-1);
}

/******************************************************************************/

/* To guarantee that AFS hasn't crapped out, we want to do a few special things when we close. */

static int safeclose(f)
FILE *f;
{
    int ferr;
    int vferr;

    fflush(f);
    ferr = ferror(f);
    vferr = vfclose(f);
    return (ferr ? ferr : vferr);
}

/******************************************************************************/

/* Returns true if the string consists entirely of zero or more spaces followed by a newline */

static int blankp(string)
char *string;
{
    for (; *string == ' '; string++) ;
    return (*string == '\n');
}

/******************************************************************************/

static int usage()
{
    printf("usage: undigest -s sleepinterval -m mailboxdir -t tempdir -r readybox -l logfile\n");
    exit(1);
    return 0;
}
