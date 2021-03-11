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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/delivery/vicemail/RCS/vicemail.c,v 1.16 1993/08/25 20:36:54 susan Exp $";
#endif

/*
		vicemail.c -- Deliver a message to a user.
*/


/*
       Vicemail is invoked as:

		vicemail [-f] [-m<directory>] [-r<address>] \
			 [-4<addr-spec>] [-S<number>] [-a<ident>] \
			 [-C<anything>]

	-f	any failure should be treated as temporary

	-m	deliver to specified directory

	-r	<address> is the return-path to use for delivering
		error messages (it will be surrounded by "<>" if the
		1st char of the <address> is not a '<')

	-4	<addr-spec> will be used as the value of the "for" keyword
			in the Received header (see RFC822)

	-S	input message must be at least <number> characters long
		or it is an error (<number> must be positive)

	-a	write an X-Andrew-Authenticated-As header that indicates who this
		message is really from.

	-C<anything>	will be ignored; for future compatibility.

	<user>	If -m is not specified, mail will be written in the mailbox of
			<user> ((OBSOLETE))

	The -m argument must be specified.
	The order of the parameters is unimportant.
	A Received header will only be written if -r or -a is specified.

*/

#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <sys/param.h>
#include <andrewos.h> /* sys/time.h */
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */

typedef unsigned int bool;
#define FALSE	0
#define TRUE	1

#define NIL	0

extern int errno;
#include <mailconf.h>
#include <mail.h>

/* External VM entry points */
extern int VM_open(), VM_printf(), VM_write(), VM_close();
extern char VM_text[];

/* For error messages */
static char errortext[512];
static char notimpl[] = "not implemented";
static char aborted[] = "delivery aborted";

static char **GlobalArgv;

static char *host()
{
#define NAMELEN 256
    static char name[NAMELEN] = { '\0' };

    if (name[0] == '\0')
	if (GetHostDomainName(name, NAMELEN) < 0)
	    sprintf(name, "UNKNOWN: %d", errno);
	else
	    name[NAMELEN-1] = '\0';
    return name;
}

static header(f, user, mesg, original, curfile, who)
    register FILE *f;
    char *user, *mesg, *original, *curfile, *who;
{
    register char **p;

    /* Write mail header */
    fprintf(f, "Received: from %s via %s ID <%s>; %s", who, host(), curfile, arpadate());
    fprintf(f, "Date: %s", arpadate());
    fprintf(f, "From: vicemail <%s+@%s>\n", PostmasterName, ThisDomain);
    fprintf(f, "Subject: %s (%s)\n\n", mesg, user);

    /* Body of message */
    fprintf(f, "***** Error delivering to user %s *****\n\n", user);
    fprintf(f, "Host: %s\n", host());
    fprintf(f, "Message: %s\n", mesg);
    fputs("Command: ", f);
    if (original != NIL)
	fprintf(f, "Original error was: %s\n", original);
    for (p=GlobalArgv; *p!=NIL; p++) fprintf(f, " %s", *p);
    fputs("\n\n", f);
}

static bool compose_message(f, user, mesg, original)
    register FILE *f;
    char *user, *mesg, *original;
{
    static char separator[] = "=============================";

    fprintf(f, "Undelivered mail follows...\n\n%s\n\n", separator);
    for (;;) {
	register int n;
#define BUFSIZE	512
	char buf[BUFSIZE];
	n = read(0, buf, sizeof buf);
	if (n == 0) {
	    fprintf(f, "\n%s\n", separator);
	    if (ferror(f)) return FALSE;
	    return TRUE;
	}
	if (n < 0) return FALSE;
	if (fwrite(buf, sizeof(char), n, f) == 0) return FALSE;
    }
}

static bailout(mesg, original, body)
    char *mesg, *original;
    bool body;
{
    register FILE *f;
    register int i;
    char current[MAXPATHLEN+1];

    /* Try to create error file */
    f = NIL;	/* Just in case */
    for (i = 0; i < nbailoutprefixes; ++i) {
	sprintf(current, "%s/%s", bailoutPrefixes[i], ams_genid(1));
	f = fopen(current, "w");
	if (f != NIL) break;
    }

    /* If that fails, I give up */
    if (f == NIL) exit(EX_TEMPFAIL);
    header(f, PostmasterName, mesg, original, current, "vicemail(bailout)");
    if (body) {
	if (!compose_message(f, PostmasterName, mesg, original))
	    exit(EX_TEMPFAIL);
    }
    fclose(f);
    exit(EX_TEMPFAIL);
}

static bool is_postmaster(user)
    char *user;
{
    char *pmDomain;
    if (strcmp(user, PostmasterName) == 0) return TRUE;
    if (strcmp(user, PostmasterMailbox) == 0) return TRUE;
    if (strcmp(user, PostmasterTitle) == 0) return TRUE;
    pmDomain = CheckAMSPMName(ThisDomain);
    if (pmDomain != NULL && strcmp(user, pmDomain) == 0) return TRUE;
    return FALSE;
}

static postmaster(user, mesg, include_body)
    char *user, *mesg;
    bool include_body;
{
    register FILE *fout;
    int p[2], stat, pid;

    if (is_postmaster(user)) {
	sprintf(errortext, "%s loop on \"%s\"", PostmasterTitle, user);
	bailout(errortext, mesg, include_body);
    }

    if (pipe(p) < 0) {
	sprintf(errortext, "PIPE failed: %d", errno);
	bailout(errortext, mesg, include_body);
    }
    /* Ignore broken pipe */
    signal(SIGPIPE, SIG_IGN);

    pid = osi_vfork();
    if (pid < 0) {
	sprintf(errortext, "VFORK failed: %d", errno);
	bailout(errortext, mesg, include_body);
    }
    if (pid == 0) {
	/* I am child */
	pid = getpid();
	setpgrp(pid, pid);
	close(p[1]);
	if (dup2(p[0], 0) < 0) {
	    sprintf(errortext, "DUP2 failed: %d", errno);
	    bailout(errortext, mesg, include_body);
	}
	close(p[0]);
	execl(queuemail, "queuemail", "-i", "-z", PostmasterTitle, 0);
	/* Can't return if correct */
	sprintf(errortext, "EXECL failed: %d", errno);
	bailout(errortext, mesg, include_body);
    }
    close(p[0]);
    fout = fdopen(p[1], "w");
    if (fout == NIL) {
	sprintf(errortext, "FDOPEN failed: %d", errno);
	bailout(errortext, mesg, include_body);
    }
    if (include_body)
	if (!compose_message(fout, user, mesg, NIL))
	    bailout("Error during compose_message", mesg, FALSE);
	else
	    ;
    else
	header(fout, user, mesg, NIL, NIL, "vicemail(to queuemail)");
    fclose(fout);
    if (wait(&stat) < 0) {
	sprintf(errortext, "WAIT failed: %d", errno);
	bailout(errortext, mesg, FALSE);
    }
    if (stat != 0) {
	sprintf(errortext, "Bad status: 0x%x", stat);
	bailout(errortext, mesg, FALSE);
    }
}

static void CheckVMReturn(rc, final, user)
    register int rc;
    bool final;
    char *user;
{
    if (rc == EX_OK) return;
    if (!final) exit(EX_TEMPFAIL);

    /* This is final delivery */
    switch (rc) {
	case EX_TEMPFAIL:
	case EX_NOUSER:
	case EX_NOHOST:
		exit(rc);
	default:
		postmaster(user, VM_text, TRUE);
		exit(rc);
    }
}

/* Minimum legal size for a message */

#define MIN_MESSAGE_SIZE	2

static SendMessage(Mailbox, ReturnPath, For,
		   Authenticated, Final, MinSize)

    char *Mailbox, *ReturnPath, *For, *Authenticated;
    bool Final;
    int MinSize;
{
    register int size;

    CheckVMReturn(VM_open(NIL, Mailbox, ReturnPath, For, Authenticated, "vicemail"),
		  Final, Mailbox);

    /* The open worked; now try to write the message */
    size = 0;
    for (;;) {
	register int n;
	char buf[512];

	n = read(0, buf, sizeof buf);

	if (n < 0) {
	    /* Error */
	    VM_printf("\n<<<WARNING: read error (%d); message may be incomplete>>>\n", errno);
	    VM_close();
	    if (!Final) exit(EX_TEMPFAIL);
	    sprintf(errortext, "Error during read of 0: %d", errno);
	    postmaster(Mailbox, errortext, TRUE);
	}

	if (n == 0) break;	/* End of file */

	CheckVMReturn(VM_write(buf, n), Final, Mailbox);
	size += n;
    }

    /* Check size */
    if ((MinSize > 0 && size < MinSize) || size < MIN_MESSAGE_SIZE) {
	sprintf(errortext, "Bad message size: %d/%d", size, MinSize);
	postmaster(Mailbox, errortext, FALSE);
	VM_printf("\n<<< WARNING: message may have been truncated>>>\n\n");
	VM_printf("Reason: %s\n", errortext);
	VM_close();
	exit(Final ? EX_IOERR : EX_TEMPFAIL);
    }

    CheckVMReturn(VM_close(), Final, Mailbox);
}

static ProcessOption(arg,
		     For, Final,
		     Mailbox, ReturnPath,
		     MinSize, Authenticated)

    char *arg, **For;
    bool *Final;
    char **Mailbox, **ReturnPath, **Authenticated;
    int *MinSize;
{
    switch (*++arg) {
	case '4':
		*For = ++arg;
		if (**For == '\0') {
		    postmaster("EMPTY -4", aborted, FALSE);
		    exit(EX_USAGE);
		}
		break;
	case 'f':
		*Final = FALSE;
		break;
	case 'm':
		if (*Mailbox != '\0') {
		    postmaster(notimpl, "MULTIPLE MAILBOXES", FALSE);
		    exit(EX_USAGE);
		}
		*Mailbox = ++arg;
		if (**Mailbox == '\0') {
		    postmaster("EMPTY -m", aborted, FALSE);
		    exit (EX_USAGE);
		}
		break;
	case 'r':
		*ReturnPath = ++arg;
		break;
	case 'S':
		if (*++arg == '\0') {
		    postmaster("EMPTY -S", aborted, FALSE);
		    exit(EX_USAGE);
		}
		*MinSize = atoi(arg);
		break;
	case 'a':
		*Authenticated = ++arg;
		if (**Authenticated == '\0') {
		    postmaster("EMPTY -a", aborted, FALSE);
		    exit(EX_USAGE);
		}
		break;
	case 'C':
		/* ignore any such argument for now */
		break;
	default:
		postmaster("UNKNOWN OPTION", arg, FALSE);
		exit(EX_USAGE);
    }
}

main(argc, argv)
    int argc;
    char *argv[];
{
    char *User, *ReturnPath, *Mailbox, *For, *Authenticated;
    bool Final;
    int MinSize;
    register int i;

    /* Place argv in global for error reports */
    GlobalArgv = argv;
    User = NIL;
    ReturnPath = NIL;
    For = NIL;
    Mailbox = NIL;
    Final = TRUE;
    MinSize = -1;	/* < 0 means unspecified */
    Authenticated = NIL;	/* unspecified */

    CheckAMSConfiguration();

    for (i=1; i<argc; i++)
	switch (*argv[i]) {
	    case '-':
		ProcessOption(argv[i],
			      &For, &Final,
			      &Mailbox, &ReturnPath,
			      &MinSize, &Authenticated);
		break;
	    default:
		if (User != NIL) {
		    postmaster(notimpl, "MULTIPLE NAMES", FALSE);
		    exit(EX_USAGE);
		}
		User = argv[i];
	}

    /* Check specification of user name/mailbox */
    if (User != NIL) {
	postmaster(aborted, "Can't specify a user name to vicemail",
		   FALSE);
	exit(EX_USAGE);
    }
    if (Mailbox == NIL) {
	postmaster(aborted, "Must specify mailbox", FALSE);
	exit(EX_USAGE);
    }

    SendMessage(Mailbox, ReturnPath, For,
		Authenticated, Final, MinSize);

    /* SendMessage never returns unless everything went okay */
    exit(EX_OK);
}
