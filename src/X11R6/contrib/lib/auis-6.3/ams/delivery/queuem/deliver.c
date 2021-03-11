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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/delivery/queuem/RCS/deliver.c,v 1.13 1992/12/15 21:24:39 rr2b R6tape $";
#endif

/*

			    QUEUEMAIL

		The ITC network mail queueing program.


	This program reads mail from a file or stdin, and attempts
	to deliver the mail.

	OPTIONS:
		-d 	ILLEGAL
		-w #	ILLEGAL
		-t #	ILLEGAL
		-q dir	ILLEGAL
		-v dir	ILLEGAL
		-b dir	ILLEGAL
		-p	ILLEGAL
		-l dir	ILLEGAL
		-f file	Send mail stored in file
		-i	Read mail from standard input
		-T	ILLEGAL
		-z	Don't try local delivery before queueing or oldsendmail
		-o name	ILLEGAL
		-r return-path
			Specify return path
		-4 string
			ILLEGAL
		-m	ILLEGAL
		-a	All following arguments are addresses, not further
			options.
		-X[la]	ILLEGAL
		-D #	Debugging mode.  The debugging level is a long,
			and each debugging statement looks for a certain
			bit.  The current bit meanings:

			1		Show options & recipients
			2		Show creation of temp file
			4		Debug dropoff calls
			8		Don't blip daemon after dropoff

*/


#include <errno.h>
#include <stdio.h>
#include <sys/param.h>
#include <andrewos.h> /* sys/file.h */
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */

#include <mail.h>
#include <dropoff.h>
#include <errprntf.h>

/* Program name for error messages */
static char Qname[] = "QUEUEMAIL";

extern int errno;
extern char *malloc(), *realloc(), *UnixError(), *mktemp();
extern char Dropoff_ErrMsg[];

typedef unsigned char bool;
#define TRUE	1
#define FALSE	0

#define NIL	0

static char *inputfile,	    /* Specified/derived input file */
	    *returnpath;    /* Specified/derived return path */

static bool stdingiven		/* -i */,
	    nolocaldelivery,	/* -z */
	    addressesfollow;	/* -a */

static int Debugging;

#ifdef DEBUG
#define Debug(n, x) ((Debugging & (n)) ? printf x : 0)
#else /* DEBUG */
#define Debug(n, x)
#endif /* DEBUG */

static int OrigStderrFD, OrigStdoutFD;

/* Address stuff */
static int naddresses, maxaddresses;

#define FASTADDRESSES	100
static char **addresses;
static char *addrstorage[FASTADDRESSES];  /* Handle 1st addresses statically */

/* Forward declarations */

#define MIN_MESSAGE_SIZE    2

static void nostorage(msg)
    char *msg;
{
    errprintf(Qname, ERR_CRITICAL, NIL, NIL,
	      "Out of storage while allocating %s -- exiting", msg);
    exit(EX_OSERR);
}

static void redirectio()
{
    if (OrigStdoutFD != fileno(stdout)) {
	fflush(stdout);
	close(fileno(stdout));
	dup2(OrigStdoutFD, fileno(stdout));
    }
    if (OrigStderrFD != fileno(stderr)) {
	fflush(stderr);
	close(fileno(stderr));
	dup2(OrigStderrFD, fileno(stderr));
    }
}

static int getfile(name)
    char *name;
{
    ;	/* Nothing to do */
}

static getstdin()
{
    register int fd, size;

    inputfile = mktemp("/tmp/qmailXXXXXX");
    Debug(2, ("Temp file is \"%s\"\n", inputfile));
    fd = open(inputfile, O_RDWR|O_CREAT|O_TRUNC, 0600);
    if (fd < 0) {
	errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		  "Can't create temp file \"%s\": %s",
		  inputfile, UnixError(errno));
	exit(EX_CANTCREAT);
    }

    /* Now read stdin into the file */
    for (size=0;;) {
	char buffer[512];
	int n;

	n = read(0, buffer, sizeof buffer);
	if (n < 0) {
	    errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		      "Read from stdin failed: %s", UnixError(errno));
	    exit(EX_DATAERR);
	}

	if (n == 0) break;	/* Got EOF */

	size += n;
	if (write(fd, buffer, n) < 0) {
	    errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		      "Error writing to temp file \"%s\": %s",
		      inputfile, UnixError(errno));
	    exit(EX_DATAERR);
	}
    }

    /* Check message size */
    Debug(2, ("Got message, size is %d\n", size));
    if (size < MIN_MESSAGE_SIZE) {
	errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		  "Message on stdin too small: %d", size);
	exit(EX_DATAERR);
    }
    close(fd);
}

main(argc, argv)
    int argc;
    char **argv;
{
    /* Redirect stderr and stdout to /dev/console */
    fflush(stderr);
    OrigStderrFD = dup(fileno(stderr));
    if (OrigStderrFD < 0)
	errprintf(Qname, ERR_MONITOR, NULL, NULL, "Cannot redirect stderr");
    else {
	if (freopen("/dev/console", "w", stderr) == NULL)
	    errprintf(Qname, ERR_MONITOR, NULL, NULL, "Cannot redirect stderr");
	else
	    setbuf(stderr, NULL);
    }
    fflush(stdout);
    OrigStdoutFD = dup(fileno(stdout));
    if (OrigStdoutFD < 0)
	errprintf(Qname, ERR_MONITOR, NULL, NULL, "Cannot redirect stdout");
    else {
	if (freopen("/dev/console", "w", stdout) == NULL)
	    errprintf(Qname, ERR_MONITOR, NULL, NULL, "Cannot redirect stdout");
	else
	    setbuf(stdout, NULL);
    }

    ProcessArguments(argc, argv);

#ifdef DEBUG
    if (Debugging & 1) {
	register int i;

	fprintf(stderr, "Input file: %s\n",
		(inputfile != NIL ? inputfile : "stdin"));
	fprintf(stderr, "Local delivery: %d\n", !nolocaldelivery);
	fprintf(stderr, "Return path: \"%s\"\n",
		(returnpath != NIL ? returnpath : "<NONE>"));
	fputs("Recipients: ", stderr);
	for (i=0; i<naddresses; i++) {
	    if (i != 0) fputs(", ", stderr);
	    fputs(addresses[i], stderr);
	}
	fputc('\n', stderr);
    }
#endif /* DEBUG */

    /* Make sure there are some addresses */
    if (naddresses == 0) {
	errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		  "No recipients specified");
	exit(EX_USAGE);
    }

    if (inputfile != NIL)
	if (stdingiven) {
	    errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		      "Can't specify both -i & -f -- exiting");
	    exit(EX_USAGE);
	}
	else
	    getfile(inputfile);		
    else
    if (stdingiven)
	getstdin();	/* Read stdin into temporary file */
    else {
	errprintf(Qname, ERR_CRITICAL, NIL, NIL,
		  "No input specified -- exiting");
	exit(EX_USAGE);
    }

    NonDaemonMain();
}

static void cleanup()
{
    /* If temporary file, unlink */
#ifdef DEBUG
    if ((Debugging&2) == 0)
#endif /* DEBUG */
    if (stdingiven) unlink(inputfile);
}

static void mapdropoffrc(rc)
    int rc;
{
    register int severity, status;

    if (rc == D_OK) exit(EX_OK);

    switch (rc) {
	case D_OK_WARN:
	case D_LOCALQ:		severity = ERR_WARNING;
				status = EX_OK;
				break;
	case D_CANT_QUEUE:	severity = ERR_CRITICAL;
				status = EX_UNAVAILABLE;
				break;
	case D_BAD_PARMS:	severity = ERR_CRITICAL;
				status = EX_USAGE;
				break;
	case D_TEMP_FAIL:	severity = ERR_WARNING;
				status = EX_TEMPFAIL;
				break;
	case D_BAD_MESGFILE:	severity = ERR_CRITICAL;
				status = EX_NOINPUT;
				break;
	case D_OSERR:		severity = ERR_CRITICAL;
				status = EX_OSERR;
				break;
	default:		errprintf(Qname, ERR_CRITICAL, NIL, NIL,
					  "Impossible dropoff return code: %d",
					  rc);
				exit(EX_SOFTWARE);
    }

    errprintf(Qname, severity, NIL, NIL, Dropoff_ErrMsg);
    exit(status);
}

static NonDaemonMain()
{
    register int rc, flags;

    /* Stick NIL at end of addresses */
    addresses[naddresses] = NIL;

    /* Call dropoff as required */
    if (nolocaldelivery)
	flags = DF_NOLOCALDELIVERY;
    else
	flags = 0;
#ifdef DEBUG
    if (Debugging & 8) flags |= DF_NOBLIP;
#endif /* DEBUG */
    rc = dropoff(addresses, inputfile, returnpath, NIL, flags);
    Debug(4, ("Dropoff: %d\n", rc));
    mapdropoffrc(rc);

    /* Clean up & exit */
    cleanup();
    exit(EX_OK);
}

static ProcessArguments(argc, argv)
    int argc;
    register char *argv[];
{
    register int i;

    addressesfollow = FALSE;
    stdingiven = FALSE;
    inputfile = NIL;
    returnpath = NIL;
    nolocaldelivery = FALSE;
    Debugging = 0;
    naddresses = 0;
    maxaddresses = FASTADDRESSES-1;   /* Leave slot for NIL */
    addresses = addrstorage;

    for (i=1; argv[i]!=NIL; i++)
	if (addressesfollow || *argv[i] != '-')
	    NewAddress(argv[i]);
	else {
	    register char *arg;

	    arg = argv[i];
	    switch (*++arg) {
		case 'd':
		case 'w':
		case 't':
		case 'q':
		case 'v':
		case 'b':
		case 'p':
		case 'l':
		case 'T':
		case 'o':
		case '4':
		case 'm':
		case 'X':   errprintf(Qname, ERR_CRITICAL, NIL, NIL,
				      "Illegal option: '%c'", *arg);
			    exit(EX_USAGE);
		case 'a':   addressesfollow = TRUE;
			    break;
		case 'f':   inputfile = argv[++i];
			    if (inputfile == NIL) {
				errprintf(Qname, ERR_CRITICAL, NIL, NIL,
					  "No file name given");
				exit(EX_USAGE);
			    }
			    break;
		case 'i':   stdingiven = TRUE;
			    break;
		case 'z':   nolocaldelivery = TRUE;
			    break;
		case 'r':   returnpath = argv[++i];
			    break;
		case 'D':   Debugging = atoi(argv[++i]);
			    redirectio();
			    break;
		default:    errprintf(Qname, ERR_CRITICAL, NIL, NIL,
				      "Unknown option: '%c'", *arg);
			    exit(EX_USAGE);
	    }
	}
}

/* Address processing */

static NewAddress(addr)
    register char *addr;
{
    if (naddresses < maxaddresses) {
	/* There's room for this room */
	addresses[naddresses++] = addr;
	return;
    }

    /* Need to allocate more room */
    if (addresses == addrstorage) {
	/* Special case 1st time */
	register int i;

	addresses = (char **) calloc(maxaddresses+FASTADDRESSES, sizeof(char *));
	if (addresses == NIL) nostorage("addresses");
	for (i=0; i<maxaddresses; i++) addresses[i] = addrstorage[i];
	maxaddresses += FASTADDRESSES;
    } else {
	addresses =
	(char **) realloc(addresses,(maxaddresses+FASTADDRESSES)*sizeof(char *));
	if (addresses == NIL) nostorage("addresses");
	maxaddresses += FASTADDRESSES;
    }

    addresses[naddresses++] = addr;
}
