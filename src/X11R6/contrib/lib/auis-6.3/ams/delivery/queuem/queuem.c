/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 /*      $Disclaimer: 
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
 *  $Header: /obj/v6src/ams/delivery/queuem/RCS/queuem.c,v 1.86 1993/05/04 23:16:34 susan Exp $";
#endif

/*
			    QUEUEMAIL

	This program services a queue of messages, making periodic
	delivery attempts on each message.  The queue is an ordinary
	directory; each message is a triple of files in it.

	This program also reads mail from a file, stdin or a mail queue,
	and either calls a mail delivery program (trymail or switchmail)
	or writes the entry into one of three queues:  the [optionally 
	specified] vice queue, the [optionally specified] local queue,
	or the [unchanging] backup queue.

	OPTIONS:
		-d 	Run as daemon
		-w #	Wake up every # seconds (daemon only)
		-t #	Warn postmaster about mail in queue more than # days
		-q dir	Process mail already in queue dir
		-v dir	Use dir as primary vice queue
		-b dir	Use dir as backup vice queue
		-p	Allow writing to default queues if writes
			to primary/backup queues fail;
			also, inhibit moving unchanged requests to Vice queues.
		-P	Inhibit moving unchanged requests to slow Vice queues.
		-l dir	Use dir as local queue
		-f file	Send mail stored in file
		-i	Read mail from standard input
		-T	Allow write-to-/tmp failure posts only once an hour
		-s	Use switchmail rather than trymail for local delivery.
		-z	Don't try local delivery before queueing or trymail
		-r return-path
			Specify return path
		-A string
			String will be used as -A argument (authentication) to
			trymail if our current UID is that of the postman.
		-4 string
			String will be used as value of FOR subfield in
			Received header (if it is prepended)
		-E	Examine the queue given via ``-q''; don't process mail in it.
			That is, print a listing of the contents of the queue.
		-a	All following arguments are addresses, not further
			options.
		-X[la]	Listen to local mail daemon port.  (If la is provided,
			it will become the threshold load average.  I.e.,
			the daemon will not actually try to send the mail until
			the load average is lower than this.  La is a floating
			point number.)
		-O[t]	Run as a daemon inspecting all .Outgoing directories.
			If t is specified, it is the length of time in seconds
			to sleep between passes.
		-C anything	Ignored; for future compatibility.
		-D #	Debugging mode.  The debugging level is a long,
			and each debugging statement looks for a certain
			bit.  The current bit meanings:

			1	Print Globals after processing args.
			2	Don't fork when running daemon.
			4	Don't ever send errors to postmaster or
					write bailout errors file
			8	Debug minor routines
				    (bailout, Random,
					RunOldSendMail, TryLocalDelivery,
					GetNextToken...)
			16	Debug main routine
			32	Debug GetMessageFromQueue routine
			64	Debug CheckMessageAge routine
			128	Debug ReadMsgFromSourceFile & GetMessageText
				routines
			256	Debug CheckFiles routine
			512	Debug ErrorToPostmaster routine
			1024	Debug ParseShadowFile routine
			2048	Debug LockPlumber stuff
			4096	*UNUSED*
			8192	Debug choosing of Vice queue(s) & locking
			16384	*UNUSED*
			32768	Debug TryLocalDelivery
			65536	Debug AddRecipient
			131072	Don't send timing traces
			262144	Debug Outgoing daemon
			524288	When running as outgoing daemon, only
				look around entry for "cfe"

	ARGUMENTS:
		Any arguments not starting with hyphen, or any arguments
		following the ``-a'' option, are taken as being recipient
		names.  Some non-obvious behavior: if -q is used, then the
		"to" names are ignored.  If -q is not used but no arguments
		are specified, then an error message is printed and sent to
		the postmaster.

*/

#include <andrewos.h> /* sys/file.h sys/time.h strings.h sys/types.h */
#include <fdplumb.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <signal.h>
#include <sys/param.h>

#ifdef _IBMR2
#include <sys/select.h>
#else
#include <sys/types.h>
#endif

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <sys/stat.h>
#include <pwd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif /* HAS_SYSEXITS */

#include <util.h>
#include <mail.h>
#include <mailconf.h>

#ifdef AFS_ENV
#include <afs/param.h>
#include <rx/xdr.h>
#include <afs/afsint.h>
#include <afs/auth.h>
#include <afs/errors.h>
#ifdef CMUCS
#include <afs/ptint.h>
#include <afs/ptserver.h>
#else
#include <afs/print.h>
#include <afs/prserver.h>
#endif
#include <afs/venus.h>
#include <afs/cellconfig.h>
#else /* AFS_ENV */
#define	MAXCELLCHARS 100    /* as big as ThisDomain */
#endif /* AFS_ENV */
#ifdef AMS_DELIVERY_ENV
#include <errprntf.h>
#include <tokens.h>
#include <trymail.h>
#endif /* AMS_DELIVERY_ENV */

#include <bt.h>
#include <wp.h>

#include <qmail.h>
#define _DROPOFF_SYS 1
#include <dropoff.h>

/* Program name for error messages */
static char Qname[] = "QUEUEMAIL";

extern int errno;

extern char *arpadate();
extern char Qmail_ErrMsg[];
extern int tok_GetStr();

typedef short Boolean;
#define TRUE	1
#define FALSE	0

#define NIL	0

#ifdef AFS_ENV
#define No_Owner ANONYMOUSID
#else /* AFS_ENV */
#define No_Owner -1
#endif /* AFS_ENV */

#define debug(n, x) ((Debugging & (n)) ? printf x : 0)
#define prbool(x) (x) ? "Yes" : "No"

#define MINUTE	60	/* Seconds/minute */

#define DAY	(MINUTE*60*24)		/* Seconds/day */

#define MAXQUEUERETRIES 25		/* Max times to try writing in queue */
#define MAXNAMESIZE 2500		/* Longest user name */
#define MAXROUTINEQUEUEREADS 200

/* Forward declarations */
static void FatalError();
static int ErrorToPostmaster();
static Boolean RepeatError();

/* How a queued-request triple is named */
#define	SHADOWFILEPREFIX    ('S')
#define	FINALFILEPREFIX    ('Q')
#define	GOFILEPREFIX    ('G')

/* Global Variables */

static Boolean IsDaemon = FALSE, 		/* Daemon or 1-time only? */
	       BypassLocalDelivery = FALSE, 	/* Bypass local delivery */
	       ReadStdin = FALSE,		/* Get message from stdin */
	       UseOldSendmail = FALSE;		/* Call send mail instead of queueing */
static Boolean	SvcRegViceQueues = FALSE;	/* -p says we're servicing the
					regular Vice queues, so we shouldn't
					gratuitously move requests to them. */
static Boolean	SvcSlowViceQueues = FALSE;	/* -P says we're servicing the
					slow Vice queues, so we shouldn't
					gratuitously move requests to them. */
static Boolean  UseTestQueues = FALSE;  /* /etc/UseTestQueues file says to
					   use the test Vice queues */
static Boolean	AnyDelivered;		/* TRUE iff trymail delivered any pieces of mail */
static Boolean	GotChangedAddresses;	/* TRUE iff trymail delivered any and we got the edited address list */
static Boolean	ShuttingDown = FALSE;	/* TRUE if sigXCPU received */
static Boolean	CheckRepeatErrors = FALSE;	/* TRUE if -T given (not as many messages on out-of-space) */
static Boolean	JustLooking = FALSE;
static Boolean  portlisten = FALSE;	/* Should I listen to port? (-X) */
static double Initial_Xload;

#define DEFAULT_XLOAD	(0.8)
#define DEFAULT_XLOAD_INCR	(0.25)
#define DEFAULT_MAXWAIT	(15*60)	/* 15 minutes, in seconds */

static Boolean OutgoingDaemon = FALSE;	/* Should I be a .Outgoing daemon */
#define DEFAULT_COTIME	3600
static int cotime;

static int mailport = -1;		/* Descriptor for port */
static long DropoffFlags;		/* Flags controlling dropoff */
extern double getla();
extern int getla_ShutDown();

static char *Four	= NULL,			/* -4 value */
	    *AuthString	= NULL;			/* -A argument */
#define RETPATHSIZE 2500
static char ReturnPath[RETPATHSIZE];		/* Static alloc--sigh. */
static char *RPArg = NULL;

static int SleepPeriod = 600,	/* How long daemon should sleep */
	   OldMessageAge = 24 * 60 * 60;    /* Definition of old message (secs); default is 1 day. */

static long Debugging = 0;
static char **OriginalArgv;   /* proctitle munging from sendmail */
static int OriginalArgc;

static int OrigStdoutFD, OrigStderrFD;

static int SourceDirIsOpen = FALSE, nmsgfiles = 0;
typedef struct {
    char *name;
    int Owner;
    long int time;
    int Size;
    int gf, sf, qf;
} FileList;
static FileList *msgfiles;

/* # arguments sent to oldsendmail, besides list of recipients */
	/* oldsendmail from -oi ... */
#define MAILARGS	4
/* Index of first recipient */
#define FIRSTRECIPIENT	MAILARGS

static char NL[] = "%s\n";

static struct RECIPIENTS {
    int 	size;		/* Current size of array */
    char	**ToName;	/* Recipient names */
    int		NextRecipient;	/* Index of next vacant recipient slot */
} OldRecipients, NewRecipients, *CurrentRecipients;

static char ErrorText[MAXPATHLEN+300],
	    WhoIAmBuf[MAXNAMESIZE],
	    *WhoIAm,
	    MailSourceFile[MAXPATHLEN+1] = { '\0' },	/* File holding msg */
	    MailSourceFileShadow[MAXPATHLEN+1] = "",
	    SourceQueueDir[MAXPATHLEN+1],
	    PrimaryQueueDir[MAXPATHLEN+1] = { '\0' },
	    BackupQueueDir[MAXPATHLEN+1] = { '\0' },
	    LocalQueueDir[MAXPATHLEN+1];
#ifdef AFS_ENV
static char SourceQueueCell[2*MAXCELLCHARS+1] = { '\0' };
static int SourceInVice = -1;		/* -1 for unknown, 0 for no, 1 for yes */
#endif /* AFS_ENV */
static int MailSourceFileSize = -1;

static char *PrevailingCell = NULL;
static char AMSHomeCell[2*MAXCELLCHARS+1] = { '\0' };
static int GotAMSHomeCell = FALSE;
static char *homeCell = NULL;
static char TrymailArgBuff[300];
static char *TrymailArg = NULL;

static char AuthorizationRequest[300];
static int AuthNonQueue = 0;

static int MailSourceFD;
static struct stat MailSourceStat;
static int MyUID, MyEUID;
static int EnqueueDate;
static char MailSourceAuth[400] = { '\0' };
static char OstensibleMailSourceAuth[400] = { '\0' };

static struct osi_Times TVal;

/* Logging definitions */
#ifndef Logs
#define Logs 0
#endif /* Logs */
#ifndef LogsYes
#define LogsYes 0
#endif /* LogsYes */
#ifndef LogsDefaultOn
#define LogsDefaultOn LogsYes
#endif /* LogsDefaultOn */

#if LogsYes
static int Logging = 0;
static int LogTransaction = 0;
/*VARARGS2*/
static void Log(num, fmt, p1, p2, p3, p4, p5, p6)
int num; char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
  { if (Logging) Logstat("queuem.c", num, fmt, p1, p2, p3, p4, p5, p6); }

static void NewTransaction()
{
    ++LogTransaction;
    SetTransaction(LogTransaction);
}

static void CloseLog() { if (Logging) {TermStats(); Logging = 0;} }
#endif /* LogsYes */

/* File-lock plumbing. */
/* #ifndef LPPlumb */
/*  #define LPPlumb 1
/*  #endif LPPlumb */

#ifdef LPPlumb
#define LPNUM 30
#define LPLEN 350
static struct {char lpN[LPLEN]; int Set;} lp[LPNUM] = {0, 0};
static void SetLocked(name, where)
char *name; int where;
{/* Store the fact that a file is locked. */
    int ix; char ErrM[2*LPLEN+100];
    if (name == NULL || *name == '\0' || strlen(name) >= LPLEN) return;
    for (ix = 0; ix < LPNUM; ++ix) {
	if (strcmp(lp[ix].lpN, name) == 0) {
	    sprintf(ErrM, "lock(%s, %d): File already locked at %d", name, where, lp[ix].Set);
	    ErrorToPostmaster(ErrM, NIL, FALSE);
	    return;
	} else if (lp[ix].lpN[0] == '\0') {
	    debug(2048, ("remembering lock[%d]: %s, %d\n", ix, name, where));
	    strncpy(lp[ix].lpN, name, LPLEN);
	    lp[ix].Set = where;
	    return;
	}
    }
    sprintf(ErrM, "SetLocked(%s, %d): can't remember lock", name, where);
    ErrorToPostmaster(ErrM, NIL, FALSE);
}

static void ClearLocked(name, where)
char *name; int where;
{/* Store the fact that a file is being unlocked. */
    int ix, inthere; char ErrM[2*LPLEN+100];
    if (name == NULL || *name == '\0' || strlen(name) >= LPLEN) return;
    inthere = 0;
    for (ix = 0; ix < LPNUM; ++ix) {
	if (strcmp(lp[ix].lpN, name) == 0) {
	    debug(2048, ("forgetting lock[%d/%d]: %s, %d\n", ix, lp[ix].Set, name, where));
	    lp[ix].lpN[0] = '\0';   /* Free the record. */
	    inthere = 1;
	}
    }
    if (inthere == 0) {
	sprintf(ErrM, "unlock(%s, %d): file was never locked", name, where);
	ErrorToPostmaster(ErrM, NIL, FALSE);
    }
}

static void CheckALock(name, where, whether)
char *name; int where, whether;
{/* Check that a file (whether ? "is" : "is not") locked. */
    int ix, timesIn, inIx; char ErrM[2*LPLEN+100];
    if (name == NULL || *name == '\0' || strlen(name) >= LPLEN) return;
    timesIn = 0; inIx = -1;
    for (ix = 0; ix < LPNUM; ++ix) {
	if (strcmp(lp[ix].lpN, name) == 0) {
	    ++timesIn; inIx = ix;
	}
    }
    debug(2048, ("checking %slock[%d, %d]: %s, %d: in %d times\n", whether ? "" : "un", inIx, inIx < 0 ? -1 : lp[inIx].Set, name, where, timesIn));
    if (whether && (timesIn == 1)) return;
    if (!whether && (timesIn == 0)) return;
    if (timesIn > 0) {
	sprintf(ErrM, "Check%socked(%s, %d): file locked %d times, once from %d", (whether ? "L" : "Unl"), name, where, timesIn, lp[inIx].Set);
    } else {
	sprintf(ErrM, "Check%socked(%s, %d): file not locked", (whether ? "L" : "Unl"), name, where);
    }
    ErrorToPostmaster(ErrM, NIL, FALSE);
}
#define CheckLocked(name, where) (CheckALock(name, where, TRUE))
#define CheckUnlocked(name, where) (CheckALock(name, where, FALSE))
#endif /* LPPlumb */

static ListAllRecipients(f, between)
    register FILE *f; char *between;
{
    register int i;

    for (i=FIRSTRECIPIENT; i<CurrentRecipients->NextRecipient; ++i) {
	if (i != FIRSTRECIPIENT) fputs(between, f);
	fputs(CurrentRecipients->ToName[i], f);
    }
}

static char *copy(s)
    register char *s;
{
    register char *new;
    register int len;

    len = strlen(s) + 1;
    new = malloc(len);
    if (new == NULL) {
	sprintf(ErrorText, "Malloc failed in copy: %s", s);
	debug(8, (NL, ErrorText));
	ErrorToPostmaster(ErrorText, NIL, FALSE);
	return NULL;
    }
    bcopy(s, new, len);
    return new;
}

static int SetReturnPath(arg)
char *arg;
{/* Put sanitized return-path in ReturnPath.  Return 0 if it couldn't be done.  This routine compensates for some of the strangely-formatted return paths that arrive over the net, such as route-addrs that are surrounded by multiple pairs of angle brackets. */
	int argLen, extraBkts, begBkt, endBkt;
	char *RPAt;

	if (arg == NULL) return 0;
	if (arg[0] == '\0') {
	    strcpy(ReturnPath, "<>");
	    return 1;
	}
	argLen = strlen(arg);
	extraBkts = 0;
	while (1) {
	    begBkt = strncmp(arg+extraBkts, "<<", 2) == 0;
	    endBkt = strncmp(&arg[argLen-2-extraBkts], ">>", 2) == 0;
	    if (begBkt && !endBkt) return 0;
	    if (!begBkt && endBkt) return 0;
	    if (!begBkt && !endBkt) break;
	    ++extraBkts;
	}
	argLen -= 2*extraBkts;
	if (strncmp(arg+extraBkts, "<>", argLen) == 0) {
	    strcpy(ReturnPath, "<>");
	    return 1;
	}
	if ((arg[extraBkts] == '<' && arg[extraBkts+argLen-1] != '>')
	    || (arg[extraBkts] != '<' && arg[extraBkts+argLen-1] == '>')) return 0;
	RPAt = strchr(arg, '@');
	if (RPAt != NULL) {
		if (arg[extraBkts] == '<') {
		    strncpy(ReturnPath, arg+extraBkts, argLen);
		    ReturnPath[argLen] = '\0';
		} else {
		    ReturnPath[0] = '<';
		    strncpy(ReturnPath+1, arg+extraBkts, argLen);
		    strcpy(ReturnPath+argLen+1, ">");
		}
	} else {
	    if (arg[extraBkts] == '<') {
		strncpy(ReturnPath, arg+extraBkts, argLen);
		ReturnPath[argLen-1] = '\0';
	    } else {
		ReturnPath[0] = '<';
		strncpy(ReturnPath+1, arg+extraBkts, argLen);
		ReturnPath[argLen+1] = '\0';
	    }
	    strcat(ReturnPath, "+@");
	    RPAt = PrevailingCell;
	    if (RPAt == NULL || RPAt[0] == '\0') RPAt = AMSHomeCell;
	    if (RPAt == NULL || RPAt[0] == '\0') RPAt = ThisDomain;
	    strcat(ReturnPath, RPAt);
	    strcat(ReturnPath, ">");
	}
	return 1;
}

static void FoldTent(statval)
int statval;
{/* Close file descriptors and exit with status statval. */

	getla_ShutDown();
	if (portlisten && mailport >= 0) {
		close(mailport); mailport = -1;
	}
#if LogsYes
	CloseLog();
#endif /* LogsYes */
#ifdef PLUMBFDLEAKS
	fdplumb_SpillGuts();
#endif /* PLUMBFDLEAKS */
	exit(statval);
	/*NOTREACHED*/
}

static int InitAMSHomeCell()
{/* Sets up AMSHomeCell if it can.  Returns -1 if can't get it, 0 for unauth or no home, 1 for got home. */
	int RC;
	struct CellAuth *ca;

	if (! GotAMSHomeCell) {
		RC = FindAMSHomeCell(&ca);
		if (RC == 2) {AMSHomeCell[0] = '\0'; GotAMSHomeCell = TRUE;}
		else if (ca != NULL) {
			strncpy(AMSHomeCell, ca->CellName, sizeof(AMSHomeCell));
			GotAMSHomeCell = TRUE;
		}
	}
	if (GotAMSHomeCell && AMSHomeCell[0] != '\0') {
		PrevailingCell = AMSHomeCell;
	} else {
		PrevailingCell = ThisDomain;
	}
	return(GotAMSHomeCell ? ((AMSHomeCell[0] != '\0') ? 1 : 0) : -1);
}

static int DebugToggle = 0;
static int SaveStdoutFile;

void static DebugIntercept()
{
    DebugToggle = 1 - DebugToggle;
    errprintf(Qname, ERR_DEBUG, NULL, NULL, "Toggle is now %d", DebugToggle);
    if (DebugToggle != 0) {
	fflush(stdout);
	SaveStdoutFile = dup(fileno(stdout));
	if (SaveStdoutFile < 0) {
	    errprintf(Qname, ERR_DEBUG, NULL, NULL,
		      "Log file DUP failed"); return;}
	if (freopen("/tmp/QLOG", "w", stdout) == NULL) {
	    errprintf(Qname, ERR_DEBUG, NULL, NULL,
		      "Log file OPEN failed"); return;}
	setbuf(stdout, NULL);
	puts("Starting debugging log\n");
	Debugging = -1;
    } else {
	fflush(stdout);
	close(fileno(stdout));
	if (dup2(SaveStdoutFile, fileno(stdout)) < 0)
	    {puts("QUEUEMAIL: 2nd DUP2 failed"); return;}
	setlinebuf(stdout);
	close(SaveStdoutFile);
	Debugging = 0;
    }
}

void ResetOutErr()
{/* Reset the original stdout and stderr files. */

	if (OrigStdoutFD != fileno(stdout) && OrigStdoutFD >= 0) {
		fflush(stdout);
		close(fileno(stdout));
		dup2(OrigStdoutFD, fileno(stdout));
		close(OrigStdoutFD); OrigStdoutFD = -1;
	}
	if (OrigStderrFD != fileno(stderr) && OrigStderrFD >= 0) {
		fflush(stderr);
		close(fileno(stderr));
		dup2(OrigStderrFD, fileno(stderr));
		close(OrigStderrFD); OrigStderrFD = -1;
	}
}

void static ShutDownIntercept()
{
    ShuttingDown = TRUE;
}

#ifdef ANDREW_MALLOC_ENV
static int ReturnZero(i)
int i;
{/* Dummy for SetM0Handler */
	return 0;
}
#endif /* #ifdef ANDREW_MALLOC_ENV */

main(argc, argv, envp)
    int argc;
    char **argv, **envp;
{
    struct CellAuth *ca;
#ifdef ANDREW_MALLOC_ENV
    extern void SetM0Handler();
    extern void SetMallocCheckLevel();
#endif /* ANDREW_MALLOC_ENV */
    extern int SetPauseOff();
    int RC;

    OriginalArgv = argv;
    OriginalArgc = argc;
    SetInitialArgs(argc, argv, envp);

    /* Set up for debugging signal */
#ifdef SIGXFSZ
    signal(SIGXFSZ, DebugIntercept);
#else /* SIGXFSZ */
#ifdef SIGUSR2
    signal(SIGUSR2, DebugIntercept);
#else /* SIGUSR2 */
    signal(SIGHUP, DebugIntercept);
#endif /* SIGUSR2 */
#endif /* SIGXFSZ */
#ifdef ANDREW_MALLOC_ENV
    SetM0Handler(ReturnZero);
#endif /* ANDREW_MALLOC_ENV */
#if LogsYes
    Logging = 0;
#endif /* LogsYes */
    MyUID = getuid();
    MyEUID = geteuid();

    /* redirect stderr and stdout to /dev/console */
    fflush(stderr);
    OrigStderrFD = dup(fileno(stderr));
    if (OrigStderrFD < 0)
	errprintf(Qname, ERR_MONITOR, NULL, NULL, "Cannot redirect stderr");
    else {
	if (freopen("/dev/console", "w", stderr) == NULL) {
	    freopen("/dev/null", "w", stderr);
	    dup2(OrigStderrFD, fileno(stderr));
	    errprintf(Qname, ERR_MONITOR, NULL, NULL,
		      "Cannot redirect stderr");
	}
	else setbuf(stderr, NULL);
    }
    fflush(stdout);
    OrigStdoutFD = dup(fileno(stdout));
    if (OrigStdoutFD < 0)
	errprintf(Qname, ERR_MONITOR, NULL, NULL, "Cannot redirect stdout");
    else {
	if (freopen("/dev/console", "w", stdout) == NULL) {
	    freopen("/dev/null", "w", stdout);
	    dup2(OrigStdoutFD, fileno(stdout));
	    errprintf(Qname, ERR_MONITOR, NULL, NULL,
		      "Cannot redirect stdout");
	}
	else setbuf(stdout, NULL);
    }

    /* Initialize configuration-dependent stuff */
    strcpy(SourceQueueDir, LocalQueue);
    strcpy(LocalQueueDir, LocalQueue);

    (void) SetPauseOff();		/* Turn off ENOSPC-pausing in Computer Science */

#if LogsDefaultOn
    if (InitStats("queuemail", 0) == 0) {
	Logging = 1;
	osi_GetTimes(&TVal);
	LogTransaction = ((TVal.USecs / 10000) ^ TVal.Secs ^ getpid()) % 1000;
	NewTransaction();
	Log(100, "Starting");
    }
#endif /* LogsDefaultOn */
    CheckAMSConfiguration();

    {
	struct stat sbuf;

	if (stat("/etc/UseTestQueues", &sbuf) == 0) {
	    UseTestQueues = TRUE;
	    SlowQueueNamePrefix = "testsq";
	    MailQueueNamePrefix = "testq";
	}
    }

/* Process the arguments right away.  Keep track of things that require magic-ness. */
    /* Set up recipient arrays */
    OldRecipients.size = 0;
    OldRecipients.ToName = NULL;
    OldRecipients.NextRecipient = FIRSTRECIPIENT;
    NewRecipients.size = 0;
    NewRecipients.ToName = NULL;
    NewRecipients.NextRecipient = FIRSTRECIPIENT;
    CurrentRecipients = &OldRecipients;

    AuthorizationRequest[0] = '\0';
    ReturnPath[0] = '\0';	/* Initialize the return-path. */
    ProcessArguments(argc, argv);
    if (JustLooking && ((Debugging & 2) == 0)) ResetOutErr();
#if LogsYes
#if LogsDefaultOn
    if (Debugging & 131072) CloseLog();
#else /* LogsDefaultOn */
    if ((Debugging & 131072) && (Logging == 0)) {
	if (InitStats("queuemail", 0) == 0) {
	      Logging = 1;
	      osi_GetTimes(&TVal);
	      LogTransaction = ((TVal.USecs / 10000) ^ TVal.Secs ^ getpid()) % 1000;
	      NewTransaction();
	      Log(150, "Logging turned on after argument parsing");
	}
    }
#endif /* LogsDefaultOn */
#endif /* LogsYes */
    if (OrigStderrFD >= 0) {close(OrigStderrFD); OrigStderrFD = -1;}
    if (OrigStdoutFD >= 0) {close(OrigStdoutFD); OrigStdoutFD = -1;}
#ifdef ANDREW_MALLOC_ENV
    if (Debugging & 131071) SetMallocCheckLevel(3);
#endif /* ANDREW_MALLOC_ENV */

    if (MyUID != MyEUID) setreuid(MyEUID, MyUID);
    if (homeCell != NULL) {	/* Caller wants a specific home cell. */
	ca = NULL;
	RC = FindCell(homeCell, &ca);
	if (RC != 0 || ca == NULL) {
		if (RC == -2 || RepeatError(-1, RC)) FoldTent(EX_TEMPFAIL);
		sprintf(ErrorText, "Can't find desired home cell %s: code %d", homeCell, RC);
		FatalError(ErrorText, NIL, FALSE, EX_TEMPFAIL);
	}
	RC = SetAMSHomeCell(ca);
	if (RC != 0 && RC != -4) {
		if (RC == -2 || RepeatError(-1, RC)) FoldTent(EX_TEMPFAIL);
		sprintf(ErrorText, "Can't set to desired home cell %s: code %d; ", homeCell, RC);
		strcat(ErrorText, AMSHome_errmsg);
		FatalError(ErrorText, NIL, FALSE, EX_TEMPFAIL);
	}
    }
    ca = NULL;
    FindAMSHomeCell(&ca);	/* Now find what it's to be. */
    if (ca != NULL && ULstrcmp(ca->CellName, ThisDomain) != 0) {
	/* Configure for cross-cell work. */
	errno = 0;
	RC = CkAMSCellConfig(ca->CellName);
	if (RC != 0) {
	    if (RC < 0 || (RC > 0 && tfail(errno)) || RepeatError(1, RC)) FoldTent(EX_TEMPFAIL);
	    sprintf(ErrorText, "Can't read %s file for cell %s: %s", CellConfigMessageServer, ca->CellName, UnixError(errno));
	    FatalError(ErrorText, NIL, FALSE, EX_TEMPFAIL);
	}
    }
    WhoIAmBuf[0] = '\0';
    if (ca == NULL) {
	if (MyUID == 0) {
		strcpy(WhoIAmBuf, "root");
	}
    } else {
	FillInCell(ca);
	if (ca->UserName != NULL) strcpy(WhoIAmBuf, ca->UserName);
	strcpy(AMSHomeCell, ca->CellName);
	GotAMSHomeCell = TRUE;
    }
    if (WhoIAmBuf[0] == '\0') {	/* Try authenticating via local /etc/passwd. */
	struct passwd *PW;
	PW = getpwuid(MyUID);
	if (PW != NULL) strcpy(WhoIAmBuf, PW->pw_name);
    }
    WhoIAm = WhoIAmBuf;	/* Not guaranteed to be non-null, but it will have an approx. */
    InitAMSHomeCell();
    if (RPArg != NULL) {
	if (SetReturnPath(RPArg) == 0) {
		sprintf(ErrorText, "Bad return path: \"%s\"", RPArg);
		FatalError(ErrorText, NIL, FALSE, EX_USAGE);
	}
    }
    if (ReturnPath[0] == '\0') {	/* Set this if the argument parsing didn't. */
	if (ca != NULL) {
		char *RP = NULL;
		GetRetPath("/", &RP);
		if (RP != NULL) strcpy(ReturnPath, RP);
	}
	if (ReturnPath[0] == '\0' && WhoIAm[0] != '\0') SetReturnPath(WhoIAm);
    }
    if (MyUID != MyEUID) setreuid(MyUID, MyEUID);	/* switch 'em back */
#ifdef AFS_ENV
    SourceInVice = -1;
#endif /* AFS_ENV */

    if (AuthorizationRequest[0] != '\0' && (!JustLooking || AuthNonQueue))
	CheckAuthorization(AuthorizationRequest);

    debug(16, ("Done processing arguments; caller identity is %s.\n", WhoIAm));

    CheckConsistentInvocation();
    debug(16, ("I guess your arguments make sense\n"));

    if (JustLooking)
	LookAtQueue();
    else if (IsDaemon || OutgoingDaemon)
	DaemonMain();
    else
	NonDaemonMain();
}

static Boolean SentAction(Flag)
    int Flag;
{
    int Res, ErrCopy;
    Res = errno = ErrCopy = 0;
    if (unlink(MailSourceFile)) {ErrCopy = errno; Res = 1;}
    if (unlink(MailSourceFileShadow)) {ErrCopy = errno; Res = 1;}
    if (Res) {
	sprintf(ErrorText, "Couldn't remove queue entry %s or %s (%s)",
		MailSourceFile, MailSourceFileShadow, UnixError(ErrCopy));
	debug(Flag, (NL, ErrorText));
	if (!vdown(ErrCopy) && ErrCopy != ENFILE && !RepeatError(EACCES, ErrCopy))
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	return FALSE;
    } else
	return TRUE;
}

static int HandleMessageFromQueue(LockFileName, LockFD, age, MaxLoadAv, ForceThrough)
    char *LockFileName;
    int LockFD, age, ForceThrough;
    double MaxLoadAv;
{/* Returns: 0 for all-sent, 1 for load-too-high, 2 for leaving-in-queue */
#if LogsYes
    NewTransaction();
#endif /* LogsYes */
#ifdef LPPlumb
    CheckLocked(LockFileName, 20);
#endif /* LPPlumb */
    /* Make sure there are recipients */
    if (CurrentRecipients->NextRecipient <= FIRSTRECIPIENT) {
	sprintf(ErrorText, "No recipients found in shadow file: \"%s\"", MailSourceFileShadow);
	ErrorToPostmaster(ErrorText, NIL, TRUE);
	unlock(LockFileName, LockFD, TRUE, 1);
	SentAction(16);
	close(MailSourceFD);
	return 0;
    }
    debug(16, ("Age of message is %d seconds\n", age));

    AnyDelivered = GotChangedAddresses = FALSE;
    if (!BypassLocalDelivery) {
#if Logs
	Log(200, "Trying local delivery");
#endif /* Logs */
	if (portlisten && ((DropoffFlags & DF_FORCE) == 0) && !ForceThrough) {
		if (getla(0) >= MaxLoadAv) {
		    close(MailSourceFD);
		    unlock(LockFileName, LockFD, FALSE, 2);
		    return 1;
		}
	}
#ifdef LPPlumb
	CheckLocked(LockFileName, 21);
#endif /* LPPlumb */
	if (TryLocalDelivery() == 0) {
#if Logs
	    Log(201, "Local delivery did it all");
#endif /* Logs */
	    debug(16, ("TryLocalDelivery did the whole job!\n"));
	    unlock(LockFileName, LockFD, TRUE, 3);
	    SentAction(16);
	    close(MailSourceFD);
	    return 0;
	}
#if Logs
	Log(202, "Local delivery failed");
#endif /* Logs */
    }

#ifdef LPPlumb
    CheckLocked(LockFileName, 22);
#endif /* LPPlumb */
    if (SvcRegViceQueues || (GotChangedAddresses && SvcSlowViceQueues)) {
	if (!WriteToVice(GotChangedAddresses, 1)) {	/* Try writing in a slow queue */
		debug(16, ("Requeued message in slow queue\n"));
		unlock(LockFileName, LockFD, TRUE, 4);
		SentAction(16);
		close(MailSourceFD);
		return 0;
	}
    }
    if ((GotChangedAddresses && SvcRegViceQueues)
		|| (!SvcRegViceQueues && !SvcSlowViceQueues)) {
	if (!WriteToVice(GotChangedAddresses, 0)) {	/* Try writing in a normal queue */
		debug(16, ("Requeued message in Vice queue\n"));
		unlock(LockFileName, LockFD, TRUE, 5);
		SentAction(16);
		close(MailSourceFD);
		return 0;
	}
    }
#ifdef LPPlumb
    CheckLocked(LockFileName, 23);
#endif /* LPPlumb */
/* Try extra-hard to requeue any changed addresses */
    if (GotChangedAddresses || CheckMessageAge(age) < 0) {
	if (WriteQueueEntry(SourceQueueDir, FALSE, SvcSlowViceQueues) == 0) {
		debug(16, ("Requeued message in source queue\n"));
		unlock(LockFileName, LockFD, TRUE, 6);
		SentAction(16);
		close(MailSourceFD);
		return 0;
	}
    }
#ifdef LPPlumb
    CheckLocked(LockFileName, 24);
#endif /* LPPlumb */
    close(MailSourceFD);
    unlock(LockFileName, LockFD, FALSE, 7);
    return 2;
}

static void ClearRecipients()
{
    register int i;

    for (i=FIRSTRECIPIENT; i<OldRecipients.NextRecipient; ++i)
	free(OldRecipients.ToName[i]);
    OldRecipients.NextRecipient = FIRSTRECIPIENT;
    for (i=FIRSTRECIPIENT; i<NewRecipients.NextRecipient; ++i)
	free(NewRecipients.ToName[i]);
    NewRecipients.NextRecipient = FIRSTRECIPIENT;
    CurrentRecipients = &OldRecipients;
}

static void awaitSleep(secs)
{/* Sleep but return early on a signal or interrupt. */
    struct timeval TV;

    TV.tv_sec = secs;
    TV.tv_usec = 0;
    (void) select(0, 0, 0, 0, &TV);
}

#ifdef AFS_ENV
static Boolean authenticated(dir)
    char *dir;
{
    struct ViceIoctl blob;
    long auth;
    int code;

    blob.in_size = 0;
    blob.out = (char *) &auth;
    blob.out_size = sizeof auth;
    code = pioctl(dir, VIOCCKCONN, &blob);
    if (code != 0) {
	errprintf(Qname, ERR_WARNING, NULL, NULL,
		  "VIOCCKCONN failed: %s", UnixError(errno));
	return FALSE;
    } else {
	if (auth == EACCES) {
	    errprintf(Qname, ERR_WARNING, NULL, NULL,
		      "Tokens have expired -- ignoring request");
	    return FALSE;
	} else {
	    return TRUE;
	}
    }
}
#endif /* AFS_ENV */

#ifdef AFS_ENV
static int InitSourceCell(srcDir)
char *srcDir;
{/* Set up more globals based on the source queue name.  Returns -1 if it can't do it, 0 if it can.  Return 1 if the source cell doesn't match my AMSHome cell. */
	int RC;

	if (SourceInVice >= 0) {
		if (SourceInVice == 0) return 0;
		else {
			if (AMSHomeCell[0] == '\0') return 1;
			if (ULstrcmp(AMSHomeCell, SourceQueueCell) != 0) return 1;
			return 0;
		}
	}
	RC = GetCellFromFileName(srcDir, SourceQueueCell, sizeof(SourceQueueCell));
	if (RC == 0) {
		SourceInVice = 1;
		if (AMSHomeCell[0] == '\0') return 1;
		if (ULstrcmp(AMSHomeCell, SourceQueueCell) != 0) return 1;
		return 0;
	} else if (RC == EINVAL) {
		SourceInVice = 0;
		SourceQueueCell[0] = '\0';
		return 0;
	} else {
		errno = RC;
		return -1;
	}
}
#endif /* AFS_ENV */

/* Process the named directory as a queue, possibly while in the middle of other processing */

static void InterjectDir(dirname, tokens, toklen, newFmt, forceUid)
    char *dirname;
    unsigned char *tokens;
    int toklen, newFmt;
    long forceUid;
{
    char oldSrcDir[MAXPATHLEN+1];
#ifdef AFS_ENV
    char oldSrcCell[2*MAXCELLCHARS+1];
    int oldSrcInVice;
#endif /* AFS_ENV */
    int AnySentNow, WhenToForce;
    double TargetLoadAv;

    /* Set up to fake out GetMessageFromQueue */
    if (SourceDirIsOpen) {
	FreeFiles(msgfiles, &nmsgfiles);
	SourceDirIsOpen = FALSE;
    }

    /* Establish directory name to look in */
    strcpy(oldSrcDir, SourceQueueDir);
#ifdef AFS_ENV
    strcpy(oldSrcCell, SourceQueueCell);
    oldSrcInVice = SourceInVice;
    SourceInVice = -1;
    if (InitSourceCell(dirname) != 0) {
	strcpy(SourceQueueCell, oldSrcCell);
	SourceInVice = oldSrcInVice;
	return;
    }
#endif /* AFS_ENV */
    strcpy(SourceQueueDir, dirname);

    /* Now loop and get messages */
    AnySentNow = FALSE;
    WhenToForce = time(0) + DEFAULT_MAXWAIT;
    TargetLoadAv = Initial_Xload;
    for (;;) {
	register int Rslt;
	char LockFileName[MAXPATHLEN+1];
	int LockFD, age;

#ifdef AFS_ENV
	/* Make sure still authenticated */
	if (SourceInVice > 0 && !authenticated(dirname)) {
		if (tokens == NULL) break;
		if (newFmt) {
			UnpackAndSetTokens(tokens, toklen, 0, 0);
		} else {
			unpacktokens(tokens, 0, 0, 0, 0);	/* a last-nano save? */
		}
		if (!authenticated(dirname)) break;
	}
#endif /* AFS_ENV */

	/* Sets up globals */
	Rslt = GetMessageFromQueue(LockFileName, &LockFD, &age, FALSE, forceUid);
	if (Rslt != 0 && Rslt != 1) {
#ifdef LPPlumb
	    CheckUnlocked(LockFileName, 26);
#endif /* LPPlumb */
	    break;	/* something unexpected */
	}
	if (Rslt == 1) {
#ifdef LPPlumb
	    CheckUnlocked(LockFileName, 25);
#endif /* LPPlumb */
	    /* Can't find any valid requests in queue */
	    if (!AnySentNow) break;	/* quit after a completely null pass */
	    AnySentNow = FALSE;
	    continue;
	}
	Rslt = HandleMessageFromQueue(LockFileName, LockFD, age,
			TargetLoadAv, (time(0) >= WhenToForce));
/*
   If we sent one (0), tag it;
   if the machine was too loaded (1), wait and then start this queue again;
   if delivery processing could do nothing (2), skip this one and try the rest.
*/
#ifdef LPPlumb
	CheckUnlocked(LockFileName, 27);
#endif /* LPPlumb */
	if (Rslt == 0)	/* something was sent */
	    AnySentNow = TRUE;
	else if (Rslt == 1) {	/* too loaded */
	    if (SourceDirIsOpen) {	/* start over again */
		FreeFiles(msgfiles, &nmsgfiles);
		SourceDirIsOpen = FALSE;
	    }
	    TargetLoadAv += DEFAULT_XLOAD_INCR;
	    awaitSleep(60);
	} else if (Rslt == 2) {	/* can't move req out */
	    ;		/* do nothing--fall through to next request */
	}
    }

    /* Restore old queuemail stuff */
    if (SourceDirIsOpen) {
	FreeFiles(msgfiles, &nmsgfiles);
	SourceDirIsOpen = FALSE;
    }
    strcpy(SourceQueueDir, oldSrcDir);
#ifdef AFS_ENV
    strcpy(SourceQueueCell, oldSrcCell);
    SourceInVice = oldSrcInVice;
#endif /* AFS_ENV */
}

#ifdef AFS_ENV
static int ExtractAndSetAMSHomeCell(tokens, toklen)
char *tokens; int toklen;
{/* Set the current AMS home cell from what we found in the packet.  Return 0 if OK, non-0 if failure. */
#ifdef AFS_ENV
    struct CellAuth *ca;
    char *Read;
    auto char StrCellName[MAXCELLCHARS+1];
    int IsLocal, IsPrimary /* really means IsAMSHome */, Remaining, Expd, VID, RC;

    Read = tokens;
    Remaining = toklen;
    while (GenTokens(&Read, &Remaining, &Expd, &VID, StrCellName, &IsPrimary, &IsLocal, 0) > 0) {
	if (IsPrimary) {
	    ca = NULL;
	    RC = FindCell(StrCellName, &ca);
	    if (RC != 0 || ca == NULL) {
		errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			      "Can't find home cell %s: code %d", StrCellName, RC);
		return 1;
	    }
	    RC = SetAMSHomeCell(ca);
	    if (RC != 0 && RC != 1) {
		errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			      "Can't set AMS home cell %s: %s", StrCellName, AMSHome_errmsg);
		return 2;
	    }
	    return 0;
	}
    }
    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
	       "No cell was marked as AMSHome in dropoff packet");
    return -1;	/* nothing was marked as primary (i.e. as an AMSHome from dropoff.c) */
#else /* AFS_ENV */
    return 0;
#endif /* AFS_ENV */
}
#endif /* AFS_ENV */

static void ProcessRequest(packet, len)
    register unsigned char *packet;
    int len;
{
    register int uid;
    register struct passwd *pw;
    char newname[MAXPATHLEN+1];
    unsigned char *tokens;
    int toklen, isNewFormat, NumTokenPairs;
    char oldRetPath[RETPATHSIZE];
#ifdef AFS_ENV
    struct ViceIoctl blob;
    int PairsSet, Res;
    int oldGotAMSHome;
    char oldAMSHome[2*MAXCELLCHARS+1];
#endif /* AFS_ENV */

    newname[0] = '\0';
    isNewFormat = 0;
    NumTokenPairs = 0;
    pw = NULL;
    if ((ntohl(*(long *) packet) & DF_NEW_FMT) != 0) {
	/* Third format */
	isNewFormat = 1;
	DropoffFlags = ntohl(*(long *) packet);
	uid = ntohl(*(long *) (packet+4));
	NumTokenPairs = ntohl(*(long *) (packet+8));
	tokens = packet+12;
	if (tok_GetStr(&tokens, packet+len, newname, sizeof(newname)) == 0) {
		errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			"Directory name overflow in received packet: len %d", len);
		return;
	}
	++tokens;		/* There's always a NUL byte. */
	while (((tokens - packet) % sizeof(long)) != 0) ++tokens;	/* skip padding */
	toklen = (packet + len) - tokens;
    } else {
	/* First and Second formats no longer supported */
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		"Bad packet received: len %d, flags %#x", len, ntohl(*(long *) packet));
	return;
    }
    if (toklen <= 0) {
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			"No room for tokens in packet: len %d", len);
	return;
    }

    /* Look up pw info */
    if (isNewFormat == 0) {		/* don't need this if it's in new format */
	pw = getvpwuid(uid);
	if (pw == NULL) {
		errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			  "Can't find my uid: %d", uid);
		return;
	}
    }

#ifdef AFS_ENV
    oldGotAMSHome = GotAMSHomeCell;
    strcpy(oldAMSHome, AMSHomeCell);
#endif /* AFS_ENV */

    ForgetAMSHome();
    /* First, try to become person */
    if (setreuid(0, uid) < 0) {
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		  "Setreuid (3) failed: %s", UnixError(errno));
	return;
    }

#ifdef AFS_ENV
    if (isNewFormat) {
	PairsSet = UnpackAndSetTokens(tokens, toklen, 0, 1);
	if (PairsSet <= 0 || PairsSet > NumTokenPairs) {
		errprintf(Qname, ERR_WARNING, NULL, NULL,
			  "Couldn't set Venus tokens set(s): %d out of %d",
			PairsSet, NumTokenPairs);
		if (setreuid(0, 0) < 0)
		    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			      "Setreuid (7) failed: %s", UnixError(errno));
		return;
	}
	if (PairsSet != NumTokenPairs) {
		errprintf(Qname, ERR_WARNING, NULL, NULL,
			  "Couldn't set all Venus tokens set(s): %d out of %d",
			PairsSet, NumTokenPairs);
	}
	if (ExtractAndSetAMSHomeCell(tokens, toklen) != 0) {
	    if (setreuid(0, 0) < 0)
		errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			      "Setreuid (9) failed: %s", UnixError(errno));
	    return;
	}
    } else {
	if (!unpacktokens(tokens, 0, 0, 0, 1)) {
		errprintf(Qname, ERR_WARNING, NULL, NULL,
			  "Couldn't get/set Venus tokens");
		if (setreuid(0, 0) < 0)
		    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			      "Setreuid (5) failed: %s", UnixError(errno));
		return;
	}
    }

    GotAMSHomeCell = FALSE;
    Res = InitAMSHomeCell();
    if (Res <= 0) {
	errprintf(Qname, ERR_WARNING, NULL, NULL, "No AMS home cell (result %d)", Res);
	/* Unset the pag */
	blob.in_size = 0;
	blob.out_size = 0;
	pioctl(ViceFile, VIOCUNPAG, &blob, 1);
	ForgetAMSHome();
	if (setreuid(0, 0) < 0)
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		      "Setreuid (6) failed: %s", UnixError(errno));
	GotAMSHomeCell = oldGotAMSHome;
	strcpy(AMSHomeCell, oldAMSHome);
	InitAMSHomeCell();
	return;
    }
#endif /* AFS_ENV */
    strncpy(oldRetPath, ReturnPath, RETPATHSIZE);

    if (isNewFormat == 0) {		/* already have a newname if it's new format. */
	strcpy(newname, pw->pw_dir);
	strcat(newname, "/");
	strcat(newname, OutgoingName);
    }

    /* Process all the requests in that dir at least once */
    InterjectDir(newname, tokens, toklen, isNewFormat, uid);

#ifdef AFS_ENV
    /* Unset the pag */
    blob.in_size = 0;
    blob.out_size = 0;
    pioctl(ViceFile, VIOCUNPAG, &blob, 1);
#endif /* AFS_ENV */

    ForgetAMSHome();
    if (setreuid(0, 0) < 0)
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		  "Setreuid (4) failed: %s", UnixError(errno));
    strncpy(ReturnPath, oldRetPath, RETPATHSIZE);
#ifdef AFS_ENV
    GotAMSHomeCell = oldGotAMSHome;
    strcpy(AMSHomeCell, oldAMSHome);
#endif /* AFS_ENV */
    InitAMSHomeCell();
}

static void ProcessMailRequests()
{
#define BIGPKTLEN (1500+DROPOFF_PKT_LEN)
    union {
	long dummy;
	unsigned char packet[BIGPKTLEN];
    } aligned;

    /* Read requests from mail port & process */
    for (;;) {
	register int len, nfds;
	int fromlen;
	fd_set readfds;
	struct timeval timeout;
	struct sockaddr_in from;

	if (ShuttingDown) return;

	fromlen = sizeof from;
	len = recvfrom(mailport, aligned.packet, sizeof aligned.packet, 0,
		       &from, &fromlen);
	if (len < 0) {
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		      "Recvfrom failed: %s", UnixError(errno));
	    return;
	}
	ProcessRequest(aligned.packet, len);

	/* See if any more requests */
	FD_ZERO(&readfds);
	FD_SET(mailport, &readfds);
	timeout.tv_sec = 0;
	timeout.tv_usec = 0;
	nfds = select(mailport+1, &readfds, (fd_set *)0, (fd_set *)0, &timeout);
	if (nfds < 0) {
	    if (errno == EINTR && ShuttingDown) return;
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		      "Select (2) failed: %s", UnixError(errno));
	    return;
	}
	if (nfds == 0) return;	/* Nothing there, go away */
    }
}

/* Bind to the mail.daemon port */

static void bindtoport()
{
    struct sockaddr_in sin;

    mailport = socket(AF_INET, SOCK_DGRAM, 0);
    if (mailport < 0)
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		  "Socket failed: %s", UnixError(errno));

    sin.sin_family = AF_INET;
    sin.sin_port = htons(MAILDAEMON_PORT);
    sin.sin_addr.s_addr = INADDR_ANY;
    if (bind(mailport, &sin, sizeof sin) < 0) {
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		  "Bind failed: %s", UnixError(errno));
	close(mailport); mailport = -1;
    }
}

/* Wait for SleepPeriod or for input on mailport (if set up) */

static void waitforaction()
{
    struct timeval timeout;
    fd_set rmask;
    register int nfds;

    if (ShuttingDown) return;

    timeout.tv_sec = SleepPeriod;
    timeout.tv_usec = 0;
    FD_ZERO(&rmask);
    if (portlisten) {
	FD_SET(mailport, &rmask);
    }
    nfds = select(portlisten ? mailport+1 : 0, &rmask, (fd_set *)0, (fd_set *)0, &timeout);
    if (nfds < 0) {
	if (errno == EINTR && ShuttingDown) return;
	errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		  "Select (1) failed: %s", UnixError(errno));
	return;
    }

    /* See if something on mailport */
    if (portlisten && FD_ISSET(mailport, &rmask))
	ProcessMailRequests();

    /* In either case just return to main loop */
}

static FreeFiles(files, np)
    register FileList *files;
    register int *np;
{
    register int n = *np;

    if (n == 0) return;
    for (n--; n>=0; n--) if (files[n].name != NIL) free(files[n].name);
    free(files);
    *np = 0;
}

static int FComp(f1, f2)
FileList *f1, *f2;
{/* Comparator for qsort. */
	if (f1->time < f2->time) return -1;
	else if (f1->time > f2->time) return 1;
	else return 0;
}

static void SortFiles(files, n)
    register FileList *files;
    register int n;
{
    if (n > 1) qsort(files, n, sizeof(FileList), FComp);
}

/* Let 'em hang in there for three hours. */
#define IGNORE_TIME	(3 * 60 * 60)

static void AppFileInfo(str, fldn, filep, ix)
char *str, *fldn; FileList *filep; int ix;
{/* Append file information to the given string */
	char NumBuff[15];

	if (ix >= 0) {
		strcat(str, "\t");
		strcat(str, fldn);
		strcat(str, " ``");
		strcat(str, filep[ix].name);
		strcat(str, "'': ");
		sprintf(NumBuff, "%d", filep[ix].Owner);
		strcat(str, NumBuff);
		strcat(str, "/");
		sprintf(NumBuff, "%d", filep[ix].Size);
		strcat(str, NumBuff);
		strcat(str, "/");
		strcat(str, ctime(&filep[ix].time));
	} else {
		strcat(str, "\tNo corresponding ");
		strcat(str, fldn);
		strcat(str, " file!\n");
	}
}

static int CheckFiles(dirName, files, n)
    register char *dirName;
    register FileList *files;
    register int n;
{/* Return TRUE iff client needs to re-read the dir, FALSE if no changes and all OK. */
    register int i, j;
    int NeedReRead = FALSE, isOne;
    char BigComplaint[3 * MAXPATHLEN],
	SubjBuff[MAXPATHLEN],
	FNBuff[MAXPATHLEN];

/* Get a snapshot of the current time. */
    osi_GetTimes(&TVal);

/* Mark all the files as having no brethren. */
    for (i = 0; i < n; ++i) {files[i].sf = files[i].gf = files[i].qf = -1;}

    /* Now walk through & account for every file */
    for (i = 0; i < n; ++i) {
	if (ShuttingDown) return NeedReRead;
	if (files[i].name == NIL) continue;	/* Never allocated */

	/* See what kind of file it is */
	if (files[i].name[0] == '\0') {
	    continue;
	}
	isOne = FALSE;
	switch (files[i].name[0]) {
	    case GOFILEPREFIX:
	    case FINALFILEPREFIX:
	    case SHADOWFILEPREFIX:
		isOne = TRUE;
	}
	if (isOne) {	/* Search for its brothers and mark 'em all. */
	    for (j = 0; j < n; ++j) {
		if (files[j].name != NIL
		    && files[j].name[0] != '\0'
		    && strcmp(&files[i].name[1], &files[j].name[1]) == 0) {
			/* This file shares a name with our target. */
			switch (files[j].name[0]) {
			    case GOFILEPREFIX:
				files[i].gf = j; break;
			    case FINALFILEPREFIX:
				files[i].qf = j; break;
			    case SHADOWFILEPREFIX:
				files[i].sf = j; break;
			}
		}
	    }
	}
    }
/* Now we've marked each file according to whether it has its two brothers, or whether it's in the queue scheme at all. */
    for (i = 0; i < n; ++i) {
	if (ShuttingDown) return NeedReRead;
	if (files[i].name == NIL) continue;
	if (files[i].time > (TVal.Secs - IGNORE_TIME)) continue;	/* A babe. */

	if (files[i].gf < 0 || files[i].qf < 0 || files[i].sf < 0) {		/* Any reason to complain? */
	    if (files[i].gf >= 0 || files[i].qf >= 0 || files[i].sf >= 0) {	/* Yes.  Any good here? */
		sprintf(BigComplaint,
			"CheckFiles: want to unlink [%d]=``%s'', age %d seconds; gf,qf,sf=%d, %d, %d\n",
			i, files[i].name, (TVal.Secs - files[i].time),
			files[i].gf, files[i].qf, files[i].sf);
		AppFileInfo(BigComplaint, "GF", files, files[i].gf);
		AppFileInfo(BigComplaint, "QF", files, files[i].qf);
		AppFileInfo(BigComplaint, "SF", files, files[i].sf);
		BigComplaint[strlen(BigComplaint) - 1] = '\0';	/* flush last \n */
	    } else {
		sprintf(BigComplaint,
			"CheckFiles: want to unlink garbage file [%d]=``%s'', age %d seconds: %d/%d/%s",
			i, files[i].name, TVal.Secs - files[i].time, files[i].Owner,
			files[i].Size, ctime(&files[i].time));
	    }
	    debug(256, (NL, BigComplaint));
	    errno = 0;
	    sprintf(FNBuff, "%s/%s", dirName, files[i].name);
	    if (unlink(FNBuff) == 0) {	/* unlink succeeded */
#if 0
		if (files[i].Size != 0) {	/* Only report deleting files of non-zero size */
			sprintf(SubjBuff, "Unlinked unusable file ``%s''", FNBuff);
			ErrorToPostmaster(SubjBuff, BigComplaint, FALSE);
		}
#endif /* 0 */
		NeedReRead = TRUE;
	    } else {
		j = errno;
		if (j == ENOENT || j == ENFILE || vdown(j)) {
			NeedReRead = TRUE;
		} else {
			if (!RepeatError(EACCES, j)) {
				sprintf(SubjBuff, "Can't unlink unusable file ``%s'': %s",
					FNBuff, UnixError(j));
				ErrorToPostmaster(SubjBuff, BigComplaint, FALSE);
			}
		}
	    }
	}
    }

    return NeedReRead;
}

#define INITIAL_DIR_SIZE    99
#define INCR_DIR_SIZE	    499
/* Assume that if we need to increment dirsize, we are	*/
/* on a post office machine with large queues -- Sohan	*/


static FileList *GetDirEntries(dirName, nfiles, err)
    char *dirName;
    register int *nfiles, *err;
{
    DIR *dp;
    register DIRENT_TYPE *ent;
    static char msg[] = "Out of storage (%s) in GetDirEntries for \"%s\"";
    register FileList *files;
    register int fsize;	    /* Max # slots in files array */
    int i, dummy;

    *nfiles = 0;
    if (err == NIL) err = &dummy;
    *err = 0;
    dp = opendir(dirName);
    if (dp == NULL) {
	*err = errno;
	return NIL;
    }

    /* Allocate initial space */
    files = (FileList *) malloc(INITIAL_DIR_SIZE*sizeof(FileList));
    if (files == NIL) {
	sprintf(ErrorText, msg, "malloc", dirName);
	ErrorToPostmaster(ErrorText, NIL, FALSE);
	closedir(dp);
	*err = -1;
	return NIL;
    }
    fsize = INITIAL_DIR_SIZE;

    for (i=0, ent=readdir(dp); ent!=NULL; ent=readdir(dp)) {
	char f[MAXPATHLEN+1];
	struct stat buf;

	/* Ignore . & .. */
	if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
	    continue;

	if (ShuttingDown) {
		closedir(dp);
		FreeFiles(files, &i);
		*err = -1;
		return NIL;
	}

	if (i >= fsize) {
	    /* Need more space */
	    fsize += INCR_DIR_SIZE;
	    files = (FileList *) realloc(files, fsize*sizeof(FileList));
	    if (files == NIL) {
		sprintf(ErrorText, msg, "realloc", dirName);
		ErrorToPostmaster(ErrorText, NIL, FALSE);
		closedir(dp);
		*err = -1;
		return NIL;
	    }
	}

	/* Make sure file is still there */
	strcpy(f, dirName);
	strcat(f, "/");
	strcat(f, ent->d_name);
	if (stat(f, &buf) < 0) {
	    if (errno == ENOENT)	/* Someone else picked it up */
		continue;

	    *err = errno;
	    sprintf(ErrorText, "Can't stat \"%s\": %s", f, UnixError(*err));
	    if (*err != ENFILE && !vdown(*err) && !RepeatError(EACCES, *err)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	    closedir(dp);
	    FreeFiles(files, &i);
	    return NIL;
	}

	files[i].time = buf.st_mtime;
	files[i].Owner = buf.st_uid;
	files[i].Size = buf.st_size;
	files[i].name = (char *) malloc(DIRENT_NAMELEN(ent)+1);
	if (files[i].name == NIL) {
	    sprintf(ErrorText, msg, "2nd malloc", dirName);
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    closedir(dp);
	    FreeFiles(files, &i);
	    *err = -1;
	    return NIL;
	}
	strcpy(files[i++].name, ent->d_name);
    }

    closedir(dp);
    if (i == 0) {free(files); files = NULL;}
    *nfiles = i;
    return files;
}

static long LastCleanupTime = 0;

/* Look in dir for lone SF, QF, GF files & remove them */

static void CleanupOutgoing(dirName)
    char *dirName;
{
    struct stat buf;
    int nfiles;
    register FileList *files;

    /* 1st check date on dir -- if not modified since last sweep, ignore */
    if (stat(dirName, &buf) < 0) return;
    if (buf.st_mtime < LastCleanupTime) return;

    files = GetDirEntries(dirName, &nfiles, 0);
    if (nfiles == 0) return;
    if (! ShuttingDown) (void) CheckFiles(dirName, files, nfiles);
    FreeFiles(files, &nfiles);
}

/*
   This is the routine for acting as a daemon that checks people's
   .Outgoing directories.
*/

static CheckOutgoings()
{
    register struct passwd *pwd;
    long int CleanupStartTime, FirstStartTime, LDum; int Pass;
    char Times[100]; char *cp; struct tm *tpack; char StartAt;

    FirstStartTime = 0;
    osi_GetTimes(&TVal);
    LDum = TVal.Secs / 60;    /* make room for the multiplication: get minutes-since-epoch */
    Pass = ((LDum * 17) / (24*60)) % 27;    /* increment by 17 per day, modulus 27 */
    StartAt = (Pass == 26 ? '\0' : ('a' + Pass));
    OriginalArgv[1] = NULL;
    OriginalArgc = 1;
    for (Pass = 1;;++Pass) {
	osi_GetTimes(&TVal);
	CleanupStartTime = TVal.Secs;
	tpack = localtime(&CleanupStartTime);
	sprintf(Times, "pass %d:%02d", tpack->tm_hour, tpack->tm_min);
	cp = Times + strlen(Times);
	sprintf(cp, "/'%c'", (StartAt == '\0' ? '0' : StartAt));
	if (FirstStartTime == 0) {
	    FirstStartTime = CleanupStartTime;
	} else {
	    tpack = localtime(&FirstStartTime);
	    cp = Times + strlen(Times);	/* point to the NUL */
	    sprintf(cp, "/start %d:%02d", tpack->tm_hour, tpack->tm_min);
	}
	setvpwent();
#ifdef DEBUG
	if (Debugging & 524288) {
	    for (pwd=getvpwent(); strcmp(pwd->pw_name, "cf19") < 0; pwd=getvpwent());
	} else
#endif /* DEBUG */
	if (Pass == 1 && StartAt != '\0') {
	    SetProcTitle("(queuemail) -O%d p%d scanning to ``%c'' (%s)", cotime, Pass, (StartAt == '\0' ? '0' : StartAt), Times);
	    for (pwd=getvpwent(); pwd != NIL && pwd->pw_name != NIL && pwd->pw_name[0] < StartAt; pwd=getvpwent());
	}
	for (pwd=getvpwent(); pwd!=NIL; pwd=getvpwent()) {
	    char dirName[MAXPATHLEN+1];
	    register DIR *dp;
	    register DIRENT_TYPE *ent;
	    register Boolean closed;

	    /* See if shutdown signal received */
	    if (ShuttingDown) {
		errprintf(Qname, ERR_CRITICAL, NULL, NULL,
			  "PID %d: Shutting down outgoing daemon", getpid());
		sleep(2);
		FoldTent(0);
	    }
#ifdef DEBUG
	    if ((Debugging&524288) && strcmp(pwd->pw_name, "cg0o") == 0) break;
#endif /* DEBUG */
	    /* See if any files in ~/.Outgoing */
	    SetProcTitle("(queuemail) -O%d p%d checking ~%s (%s)", cotime, Pass, pwd->pw_name, Times);
	    strcpy(dirName, pwd->pw_dir);
	    strcat(dirName, "/");
	    strcat(dirName, OutgoingName);
	    dp = opendir(dirName);
	    if (dp == NIL) {
		VenusFlush(pwd->pw_dir);
		continue;	    /* Not there, Vice down, etc. */
	    }
	    closed = FALSE;
	    for (ent=readdir(dp); ent!=NIL; ent=readdir(dp)) {
		if (*ent->d_name != '.') {
		    register char *whoiwas;

		    closedir(dp);
		    closed = TRUE;
		    whoiwas = WhoIAm;
		    if (! ShuttingDown) {
			debug(262144, ("Looking at queued mail in %s\n", dirName));
			SetProcTitle("(queuemail) -O%d p%d delivering for ~%s (%s)", cotime, Pass, pwd->pw_name, Times);
			InterjectDir(dirName, NULL, 0, 0, (long) pwd->pw_uid);
		    }
		    WhoIAm = whoiwas;
		    if (! ShuttingDown) CleanupOutgoing(dirName);
		    break;
		}
	    }
	    if (!closed) closedir(dp);
	    VenusFlush(dirName);
	    VenusFlush(pwd->pw_dir);
	}

	LastCleanupTime = CleanupStartTime;
	if (! ShuttingDown) {
	    osi_GetTimes(&TVal);
	    SetProcTitle("(queuemail) -O%d p%d done in %d secs (%s)", cotime, Pass, TVal.Secs - CleanupStartTime, Times);
	    awaitSleep(cotime);
	}
    }
}

static LookAtQueue()
{
    int age, PrintCount, authId;
    unsigned long NowTime;
    char *Ptr, *XPtr;

    debug(16, ("Examining a queue\n"));

    /* Switch effective uid to real uid for Vice */
    (void) setreuid(MyUID, MyUID);

    /* Set up to intercept shutdown signal */
#ifdef SIGXCPU
    signal(SIGXCPU, ShutDownIntercept);
#else /* SIGXCPU */
#ifdef SIGUSR1
    signal(SIGUSR1, ShutDownIntercept);
#else /* SIGUSR1 */
    signal(SIGFPE, ShutDownIntercept);
#endif /* SIGUSR1 */
#endif /* SIGXCPU */

    NowTime = osi_GetSecs();
    PrintCount = 0;
    while (TRUE) { /* Forever */
	char LockFileName[MAXPATHLEN+1];
	int LockFD;

	if (ShuttingDown) {
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		      "PID %d: Shutting down service to queue %s",
		      getpid(), SourceQueueDir);
	    sleep(2);
	    FoldTent(0);
	}

#ifdef AFS_ENV
	if (SourceInVice < 0) {
		if (InitSourceCell(SourceQueueDir) > 0) {
			fprintf(stderr, "home and source-queue cells differ: %s != %s\n",
				AMSHomeCell, SourceQueueCell);
			break;
		}
	}
	if (SourceInVice < 0) {
		fprintf(stderr, "Don't yet know whether queue %s is in AFS.\n",
				SourceQueueCell);
		break;
	}
#endif /* AFS_ENV */
	/* Sets up globals */
	if (GetMessageFromQueue(LockFileName, &LockFD, &age, TRUE, No_Owner) != 0) break;
#ifdef LPPlumb
	CheckUnlocked(LockFileName, 30);
#endif /* LPPlumb */
	close(MailSourceFD);

	/* Got a file description in some globals.  Print it out now. */
	if (++PrintCount == 1) {
		fputs("Messages queued in ", stdout);
		fputs(SourceQueueDir, stdout);
		fputs(" on ", stdout);
		fputs(NiceTime(NowTime), stdout);
		fputs(":\n-----id------   -size- -----enqueued-----\n-from-	--to--\n", stdout);
	}
	Ptr = NULL; XPtr = strrchr(LockFileName, '/');
	if (XPtr != NULL) {
		*XPtr = '\0';
		Ptr = strrchr(LockFileName, '/');
		*XPtr = '/';
	}
	printf("%s   %d chrs", (Ptr != NULL ? Ptr+1 : LockFileName), MailSourceFileSize);
	if (EnqueueDate != 0) {fputs("   ", stdout); fputs(NiceTime(EnqueueDate), stdout);}
	if (Four != NULL) {fputs("\nFor: ", stdout); fputs(Four, stdout);}
	XPtr = NULL;
	Ptr = strchr(MailSourceAuth, ';');	/* Now scan MailSourceAuth */
	if (Ptr != NULL) {
		*Ptr++ = '\0';
		XPtr = strchr(Ptr+1, ';');
		if (XPtr != NULL) *XPtr++ = '\0';
	}
	fputc('\n', stdout);
	authId = atoi(MailSourceAuth);
#ifdef AFS_ENV
	if (authId == ANYUSERID || authId == ANONYMOUSID)
	    authId = No_Owner;
#endif /* AFS_ENV */
	if (authId != 0 || authId != No_Owner) {
	    if (XPtr != NULL) {
		fputs(XPtr, stdout);
	    } else {
		printf("uid=%d", authId);
	    }
#ifdef AFS_ENV
	    if (Ptr != NULL && ULstrcmp(Ptr, SourceQueueCell) != 0) {
		/* indicate other-cell-ness */
		fputc('/', stdout); fputs(Ptr, stdout);
	    }
#endif /* AFS_ENV */
	    fputc(' ', stdout);
	}
	fputs(ReturnPath, stdout);
	fputs("\n\t", stdout);
	ListAllRecipients(stdout, "\n\t");
	fputc('\n', stdout);
    }  /* end of big while loop */
    if (PrintCount == 0) printf("No messages queued in %s.\n", SourceQueueDir);
}

static DaemonMain()
{
    int age, GetRslt, AnySentThisPass;

    debug(16, ("Running as daemon\n"));

    if (! (Debugging & 2)) {
	NEWPGRP();
	debug(16, ("Did setpgrp\n"));
    }

    /* Switch effective uid to real uid for Vice */
    if (setreuid(MyUID, MyUID) < 0) {
	sprintf(ErrorText, "Setreuid (2) failed: %s; exiting", UnixError(errno));
	errprintf(Qname, ERR_CRITICAL, NULL, NULL, ErrorText);
	ErrorToPostmaster(ErrorText, NIL, FALSE);
	FoldTent(CheckRepeatErrors ? EX_TEMPFAIL : EX_OSERR);
    }

    if (Debugging & 2)
	debug(16, ("Not forking or doing setpgrp\n"));
    else {
	if (fork() != 0) FoldTent(0);
	debug(16, ("Forked\n"));
    }

    /* Bind to mail port if desired */
    if (portlisten) bindtoport();

    /* Set up to intercept shutdown signal */
#ifdef SIGXCPU
    signal(SIGXCPU, ShutDownIntercept);
#else /* SIGXCPU */
#ifdef SIGUSR1
    signal(SIGUSR1, ShutDownIntercept);
#else /* SIGUSR1 */
    signal(SIGFPE, ShutDownIntercept);
#endif /* SIGUSR1 */
#endif /* SIGXCPU */

    /* See if I should be an outgoing daemon */
    if (OutgoingDaemon) CheckOutgoings();	/* which will never return. */

    AnySentThisPass = FALSE;
    while (TRUE) { /* Forever */
	char LockFileName[MAXPATHLEN+1];
	int LockFD;

	if (ShuttingDown) {
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		      "PID %d: Shutting down service to queue %s",
		      getpid(), SourceQueueDir);
	    sleep(2);
	    FoldTent(0);
	}

#ifdef AFS_ENV
	if (SourceInVice < 0) {
	    if (InitSourceCell(SourceQueueDir) > 0) {
		char BigComplaint[100 + 2*MAXCELLCHARS + MAXPATHLEN];

		sprintf(ErrorText, "Cell diff: %s != %s", AMSHomeCell, SourceQueueCell);
		sprintf(BigComplaint,
			"Cell mismatch: source dir ``%s'' is in cell ``%s'', which is not my home cell (%s).",
			SourceQueueDir, SourceQueueCell, AMSHomeCell);
		errprintf(Qname, ERR_CRITICAL, NULL, NULL, ErrorText);
		ErrorToPostmaster(ErrorText, BigComplaint, FALSE);
		FoldTent(CheckRepeatErrors ? EX_TEMPFAIL : EX_OSERR);
	    }
	}
	LockFileName[0] = '\0';
	if (SourceInVice < 0) {
	    GetRslt = 1;	/* Still waiting to learn queue's cell name */
	} else {
	    /* Sets up globals */
	    GetRslt = GetMessageFromQueue(LockFileName, &LockFD, &age, FALSE, No_Owner);
	}
#else /* AFS_ENV */
	GetRslt = GetMessageFromQueue(LockFileName, &LockFD, &age, FALSE, No_Owner);
#endif /* AFS_ENV */
	if (GetRslt != 0) {	/* Can't find any valid requests in queue */
#ifdef LPPlumb
	    CheckUnlocked(LockFileName, 35);
#endif /* LPPlumb */
	    debug(16, ("GetMessageFromQueue returns %d; AnySent is %d.\n",
		  GetRslt, AnySentThisPass));
	    if (!AnySentThisPass) waitforaction();
	    AnySentThisPass = FALSE;
	    continue;
	}
#ifdef LPPlumb
	CheckLocked(LockFileName, 36);
#endif /* LPPlumb */
	if (HandleMessageFromQueue(LockFileName, LockFD, age, Initial_Xload, 1) == 0)
	    AnySentThisPass = TRUE;
    }  /* end of big while loop */
}

static NonDaemonMain()
{
    debug(16, ("Not a daemon\n"));

    if (! (Debugging & 2)) {
	NEWPGRP();
	debug(16, ("Did a setpgrp\n"));
    }

    /* Switch effective & real uids for Vice */
    if (MyUID != MyEUID) {
      if (setreuid(MyEUID, MyUID) < 0) {
	sprintf(ErrorText, "Setreuid (1) failed: %s; mail not sent", UnixError(errno));
	errprintf(Qname, ERR_CRITICAL, NULL, NULL, ErrorText);
	ErrorToPostmaster(ErrorText, NIL, FALSE);
	FoldTent(CheckRepeatErrors ? EX_TEMPFAIL : EX_OSERR);
      }
    }

#if Logs
    Log(300, "Know I'm not a daemon");
#endif /* Logs */
    /* Make sure there are some recipients */
    if (CurrentRecipients->NextRecipient <= FIRSTRECIPIENT) {
	FatalError("No recipients specified", NIL, FALSE, EX_USAGE);
    }

    MailSourceAuth[0] = OstensibleMailSourceAuth[0] = '\0';
    EnqueueDate = 0;

#if Logs
    Log(301, "About to get message text");
#endif /* Logs */
    GetMessageText();	/* Only returns if successful */
#if Logs
    Log(302, "GetMessageText returned");
#endif /* Logs */

    AnyDelivered = GotChangedAddresses = FALSE;
    if (!BypassLocalDelivery) {
#if Logs
	Log(310, "Trying local delivery");
#endif /* Logs */
	if (!TryLocalDelivery()) {
#if Logs
	    Log(311, "Local delivery did everything");
#endif /* Logs */
	    debug(16, ("TryLocalDelivery did all the work\n"));
	    CleanupMessageText();
#if LogsYes
	    Log(312, "Exiting");
#endif /* LogsYes */
	    FoldTent(EX_OK);
	}
#if Logs
	Log(313, "Local delivery didn't do everything");
#endif /* Logs */
    }

#if Logs
    Log(330, "Trying to write to Vice");
#endif /* Logs */
    if (WriteToVice(TRUE, 0)) {
#if LogsYes
	Log(331, "Vice write failed--trying local queue");
#endif /* LogsYes */
	if (MyUID != MyEUID) setreuid(MyUID, MyEUID);	/* maybe necessary */
	if (WriteQueueEntry(LocalQueueDir, FALSE, FALSE) != 0) {
	    debug(16, ("Disaster: can not queue mail at all\n"));
	    /* Total disaster for mail not yet in queue */
	    sprintf(ErrorText, "%san't queue mail; informing postmaster",
		    CheckRepeatErrors ? "C" : "Absolutely c");
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL, ErrorText);
	    ErrorToPostmaster(ErrorText, NIL, TRUE);
	    CleanupMessageText();
	    FoldTent(CheckRepeatErrors ? EX_TEMPFAIL : EX_CANTCREAT);
	} else {
	    sync();
	    /* Message to console */
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
		      "Can't write to AFS; mail queued locally");
	}
    }

#if Logs
    Log(332, "Entry written--about to clean up message text");
#endif /* Logs */
    debug(16, ("Wrote queue entry and ready to exit\n"));
    CleanupMessageText();
#if LogsYes
    Log(333, "Cleanup done--about to exit");
#endif /* LogsYes */
    FoldTent(EX_OK);
}

static ProcessArguments(argc, argv) 
    int argc;
    char **argv;
{
    register char *s;
    int ArgsAreAddresses;

    Debugging = 0;
    ArgsAreAddresses = 0;
    while (--argc > 0) {
	++argv;
	if ((ArgsAreAddresses != 0) || (argv[0][0] != '-')) {
	    /* Ignore null recipients */
	    if (*argv[0] != '\0')
		if (AddRecipient(CurrentRecipients, argv[0])) {
		    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
	    "Ran out of storage while processing recipients -- mail not sent");
		    FoldTent(EX_TEMPFAIL);
		}
	} else {
	    s = argv[0] + 1;
	    switch (*s) {
		case '4':
			Four = argv[1];
			++argv;
			--argc;
			break;
		case 'a':
			ArgsAreAddresses = 1;
			break;
		case 'A':
			AuthString = argv[1];
			++argv;
			--argc;
			break;
		case 'b':
			AuthNonQueue = 1;
			strcpy(AuthorizationRequest, "change backup Vice queue");
			strcpy(BackupQueueDir, argv[1]);
			++argv;
			--argc;
			break;
		case 'C':
			/* Ignore most arguments (for future compatibility). */
			if (s[1] == 'h') {	/* Set home cell */
			    homeCell = argv[1];
			} else if (s[1] == 'a') {	/* set extra arg to trymail/switchmail */
			    if (strlen(argv[1]) >= sizeof(TrymailArgBuff)) {
			        sprintf(ErrorText, "Trymail arg too long: %d chars (%d max)", strlen(argv[1]), sizeof(TrymailArgBuff)-1);
			        FatalError(ErrorText, NIL, FALSE, EX_USAGE);
			    }
			    strncpy(TrymailArgBuff, argv[1], sizeof(TrymailArgBuff));
			    TrymailArg = TrymailArgBuff;
			}
			++argv;
			--argc;
			break;
		case 'd':
			AuthNonQueue = 1;
			strcpy(AuthorizationRequest, "run as daemon");
			IsDaemon = TRUE;
			break;
		case 'D':
			Debugging |= atoi(argv[1]);
			++argv;
			--argc;
			if (Debugging & 2) ResetOutErr();
			break;
		case 'E':
			JustLooking = TRUE;
			break;
		case 'f':
			strcpy(MailSourceFile, argv[1]);
			++argv;
			--argc;
			break;
		case 'i':
			ReadStdin = TRUE;
			break;
		case 'l':
			AuthNonQueue = 1;
			strcpy(AuthorizationRequest, "change local queue");
			strcpy(LocalQueueDir, argv[1]);
			++argv;
			--argc;
			break;
		case 'O':
			OutgoingDaemon = TRUE;
			if (*(s+1) != '\0')
			    cotime = atoi(s+1);
			else
			    cotime = DEFAULT_COTIME;
			break;
		case 'p':
			SvcRegViceQueues = TRUE;
			break;
		case 'P':
			SvcSlowViceQueues = TRUE;
			break;
		case 'q':
			strcpy(AuthorizationRequest, "change mail queue");
			strcpy(SourceQueueDir, argv[1]);
			++argv;
			--argc;
			break;
		case 'r':
			RPArg = argv[1];
			++argv;
			--argc;
			break;
		case 's':
			UseOldSendmail = TRUE;
			break;
		case 't':
			OldMessageAge = atoi(argv[1]);
			if (OldMessageAge < 1 || OldMessageAge > 30) {
			    if (! Debugging) {
			        sprintf(ErrorText, "Bad timeout age: %d", OldMessageAge);
			        FatalError(ErrorText, NIL, FALSE, EX_USAGE);
			    }
			}
			if (OldMessageAge < 300) OldMessageAge *= DAY;
			++argv;
			--argc;
			break;
		case 'T':
			CheckRepeatErrors = TRUE;
			break;
		case 'v':
			AuthNonQueue = 1;
			strcpy(AuthorizationRequest, "change primary Vice queue");
			strcpy(PrimaryQueueDir, argv[1]);
			++argv;
			--argc;
			break;
		case 'w':
			SleepPeriod = atoi(argv[1]);
			if (SleepPeriod < 1 || SleepPeriod > 1*DAY) {
			    sprintf(ErrorText, "Bad sleep period: %d seconds", SleepPeriod);
			    FatalError(ErrorText, NIL, FALSE, EX_USAGE);
			}
			++argv;
			--argc;
			break;
		case 'X':
			portlisten = TRUE;
			if (*(s+1) != '\0')
			    Initial_Xload = atof(s+1);
			else
			    Initial_Xload = DEFAULT_XLOAD;
			break;
		case 'z':
			BypassLocalDelivery = TRUE;
			break;
		default: 
			sprintf(ErrorText, "Unrecognized option: %s", argv[0]);
			FatalError(ErrorText, NIL, FALSE, EX_USAGE);
	    }
	}
    }

    if (Debugging & 1) {
	printf("Global variables:\n\tDebugging: %d\n\tIsDaemon: %s\n",
	       Debugging, prbool(IsDaemon));
	printf("\tBypassLocalDelivery: %s\n\tUseOldSendmail: %s\n",
	       prbool(BypassLocalDelivery), prbool(UseOldSendmail));
	if (TrymailArg != NULL) {
	    printf("\tExtra trymail/switchmail arg: ``%s''\n", TrymailArg);
	}
	printf("\tSleepPeriod: %d\n\tOldMessageAge: %d (seconds)\n\tToNames: ",
	       SleepPeriod, OldMessageAge);
	ListAllRecipients(stdout, ", ");
	printf("\n\tMailSourceFile: %s\n\tSourceQueueDir: %s\n\tPrimaryQueueDir: %s\n",
	       MailSourceFile, SourceQueueDir, PrimaryQueueDir);
	printf("\tBackupQueueDir: %s\n\tLocalQueueDir: %s\n",
	       BackupQueueDir, LocalQueueDir);
	fflush(stdout);
    }
}

static void FatalError(Msg, AuxMsg, SendBody, ExStat)
    char *Msg, *AuxMsg;
    Boolean SendBody;
    int ExStat;
{
    char temp[sizeof ErrorText];

    sprintf(temp, "%s--mail not sent", Msg);
    errprintf(Qname, ERR_CRITICAL, NULL, NULL, temp);
    errprintf(Qname, ERR_CRITICAL, NULL, NULL,
	    "Mailing warning message to Postmaster, in case you're not human");
    ErrorToPostmaster(Msg, AuxMsg, SendBody);
    FoldTent(ExStat);
}

/*
   Return uid corresponding to authenticated author of message.
   The uid for Anonymous (or, on non-AFS systems, -1) is returned if we are not sure.

   Warning: may clobber 1st argument!!!!
*/

static int GetOwner(buf, fd, name)
    struct stat *buf;
    int fd;
    char *name;
{
    register int owner1, owner2, owner3;

    /* Try to get all 3 uids */
    owner1 = buf->st_uid;
#ifdef AFS_ENV
    if (owner1 == ANYUSERID || owner1 == ANONYMOUSID)
	return No_Owner;
#endif AFS_ENV

#if LogsYes
    Log(401, "GetOwner doing an fstat");
#endif /* LogsYes */
    if (fstat(fd, buf) < 0) {
	sprintf(ErrorText, "Can't stat fd %d: %s", fd, UnixError(errno));
	if (!tfail(errno)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	return No_Owner;
    }
#if LogsYes
    Log(402, "GetOwner: fstat returned; calling stat(%s)", name);
#endif /* LogsYes */
    owner2 = buf->st_uid;
#ifdef AFS_ENV
    if (owner2 == ANYUSERID || owner2 == ANONYMOUSID)
	return No_Owner;
#endif /* AFS_ENV */

    if (stat(name, buf) < 0) {
	sprintf(ErrorText, "Can't stat file \"%s\": %s", name, UnixError(errno));
	if (errno != ENFILE && !vdown(errno)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	return No_Owner;
    }
#if LogsYes
    Log(403, "GetOwner: stat returned");
#endif /* LogsYes */
    owner3 = buf->st_uid;
#ifdef AFS_ENV
    if (owner3 == ANYUSERID || owner3 == ANONYMOUSID)
	return No_Owner;
#endif /* AFS_ENV */

    /* Got all 3 uids */
    if (owner1 != owner2 || owner2 != owner3) {
	sprintf(ErrorText, "Incompatible owners for \"%s\": %d, %d, %d", name, owner1, owner2, owner3);
	ErrorToPostmaster(ErrorText, NIL, FALSE);
	return No_Owner;
    }

    return owner1;
}

#ifdef AFS_ENV
static int IsTempWPErr(wperr)
wp_ErrorCode wperr;
{/* Return TRUE iff wperr represents a temporary failure code. */
	switch(wperr) {
		case wperr_OutOfMemory:
		case wperr_BTreeTempFail:
		case wperr_IndexedRecordNotFound:
			return TRUE;
	}
	if (wperr >= wperr_FileSystemErrorBegin && wperr <= wperr_FileSystemErrorEnd) {
		return tfail(wperr - wperr_FileSystemErrorBegin);
	}
	if (wperr >= wperr_BTreeBaseValue && wperr < wperr_GritsBaseValue) {
		int bterr = wperr - wperr_BTreeBaseValue;
		switch(bterr) {
		    case bterr_OutOfMemory:
		    case bterr_NotABTree:
		    case bterr_BTreeNotCurrVersion:
		    case bterr_BTreeDamaged:
		    case bterr_NotOpeningRoot:
		    case bterr_CursorTreeDamaged:
		    case bterr_EmptyTree:
			return TRUE;
		}
		if (bterr >= bterr_FileSystemErrorBegin && bterr <= bterr_FileSystemErrorEnd) {
		    return tfail(bterr - bterr_FileSystemErrorBegin);
		}
	}
	return FALSE;
}
#endif /* AFS_ENV */

static int ErrorsInGet = 0;
static char LatestGetError[450];
/*VARARGS1*/
static void GetError(fmt, p1, p2, p3, p4, p5)
char *fmt, *p1;
int p2, p3, p4, p5;
{	/* Record the latest GetMessageFromQueue error and handle reporting. */

    sprintf(LatestGetError, fmt, p1, p2, p3, p4, p5);
    debug(32, (NL, LatestGetError));
    if (JustLooking == 0) {
	if ((++ErrorsInGet) > MAXROUTINEQUEUEREADS) {
	    debug(32, ("Sending error to postmaster: \n\t%s\n", LatestGetError));
	    sprintf(ErrorText, "Too many errors reading from queue.  Latest Error: %s", LatestGetError);
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    ErrorsInGet = 0;
	}
    }
}

static int GetMessageFromQueue(LockFile, refFD, refAge, JustLook, forceUid)
    char *LockFile;
    int *refFD, *refAge, JustLook;
    long forceUid;
{
    char *suffix, *PostmanInCell;
    int SomeErrno, err, OwnerInCell;
    static int index;
#ifdef AFS_ENV
    struct passwd *PW;	/* For getting file owner's name */
#endif /* AFS_ENV */

	/*
	   DESIGN:  We assume, in essence, that any error we detect may
		be a Vice error, and we just sleep a little while and try
		again.  However, we keep count of how many errors we've
		detected without reading from the queue, and warn the 
		postmaster when this is greater than MAXROUTINEQUEUEREADS.
		Even then we don't give up, we just zero the error count
		and keep trying.

		This routine functions as a generator.  On successive calls,
		it will open successive entries from its source directory,
		returning a value of 0 to its caller for each one.  A
		non-zero return value indicates that no entry was opened;
		the caller may want to sleep.
	*/

    debug(32, ("Entering GetMessageFromQueue\n"));

    ClearRecipients();
    LockFile[0] = '\0';
    PostmanInCell = NULL;

    if (!SourceDirIsOpen) {
	errno = 0;
	debug(32, ("Opening source dir %s\n", SourceQueueDir));
	msgfiles = GetDirEntries(SourceQueueDir, &nmsgfiles, &err);
	if (err != 0) {
	    if (err > 0 && err != ENFILE && !vdown(err))
		GetError("opendir(%s) returned NULL: %s",
			 SourceQueueDir, UnixError(err));
	    return 2;
	}
	if (nmsgfiles > 0 && !JustLook) {
	    if (CheckFiles(SourceQueueDir, msgfiles, nmsgfiles)) {	/* need to re-read dir */
		FreeFiles(msgfiles, &nmsgfiles);
		errno = 0;
		debug(32, ("Opening source dir %s again\n", SourceQueueDir));
		msgfiles = GetDirEntries(SourceQueueDir, &nmsgfiles, &err);
		if (err != 0) {
		    if (err > 0 && err != ENFILE && !vdown(err))
			GetError("opendir(%s) returned NULL a second time: %s",
				 SourceQueueDir, UnixError(err));
		    return 2;
		}
	    }
	}
	SourceDirIsOpen = TRUE;
	index = 0;
	SortFiles(msgfiles, nmsgfiles);
    }

    while (index<nmsgfiles) {
#ifdef LPPlumb
	CheckUnlocked(LockFile, 49);
#endif /* LPPlumb */
	ClearRecipients();
	MailSourceAuth[0] = OstensibleMailSourceAuth[0] = '\0';
	EnqueueDate = 0;
	Four = NULL;

	if (ShuttingDown) return 3;

	/* Only consider GF Files */
	if (msgfiles[index].name[0] != GOFILEPREFIX) {
	    ++index;
	    continue;	    /* Look for another file in the directory */
	}

	/* This is a GF file */
	sprintf(LockFile, "%s/%s", SourceQueueDir, msgfiles[index].name);
#ifdef LPPlumb
	CheckUnlocked(LockFile, 40);
#endif /* LPPlumb */
/* don't lock if just looking. */
	*refFD = (JustLook ? open(LockFile, O_RDONLY, 0) : lock(LockFile, 50));
	if (*refFD < 0) {
		++index;
		continue;
	}
#ifdef LPPlumb
	CheckALock(LockFile, 41, !JustLook);
#endif /* LPPlumb */

	debug(32, ("Got %s from queue\n", LockFile));
	suffix = msgfiles[index].name + 1;
	MakeName(MailSourceFile, SourceQueueDir, FINALFILEPREFIX, suffix);

	/* ReadMsgFromSourceFile sets MailSourceFD if it succeeds */
	if (ReadMsgFromSourceFile(&SomeErrno)) {
		if (JustLook) close(*refFD); else unlock(LockFile, *refFD, FALSE, 8);
#ifdef LPPlumb
		CheckUnlocked(LockFile, 42);
#endif /* LPPlumb */
		debug(32, ("But cannot read it!\n"));
		if (SomeErrno != ENOENT && SomeErrno != ENFILE && !vdown(SomeErrno))
		    GetError("Can't get message from source file %s: %s",
		    	MailSourceFile, SomeErrno == 0 ? "File too short" : UnixError(SomeErrno));
		++index;
		continue;
	}
	MailSourceFileSize = MailSourceStat.st_size;	/* for JustLooking mode */
	MakeName(MailSourceFileShadow, SourceQueueDir, SHADOWFILEPREFIX, suffix);
	if (ParseShadowFile() != 0) {
	    err = errno;
	    close(MailSourceFD);
	    if (JustLook) close(*refFD); else unlock(LockFile, *refFD, FALSE, 10);
#ifdef LPPlumb
	    CheckUnlocked(LockFile, 44);
#endif /* LPPlumb */
	    debug(32, ("But cannot parse %s\n", MailSourceFileShadow));
	    if (err != ENOENT && !tfail(err)) GetError("Can't parse shadow of source file %s", MailSourceFile);
	    ++index;
	    continue;
	}
	*refAge = time(0) - MailSourceStat.st_mtime;
	debug(32, ("Message age is %d seconds\n", *refAge));

	/* Now, check owners of all 3 files */
	OwnerInCell = GetOwner(&MailSourceStat, *refFD, MailSourceFileShadow);
#ifdef AFS_ENV
	debug(32, ("Author of message is %d in cell %s\n", OwnerInCell, SourceQueueCell));
#else /* AFS_ENV */
	debug(32, ("Author of message is %d\n", OwnerInCell));
#endif /* AFS_ENV */
	if (JustLook) close(*refFD);
	if (forceUid != No_Owner && forceUid != OwnerInCell) {
	    close(MailSourceFD);
	    if (!JustLook) unlock(LockFile, *refFD, FALSE, 12);
#ifdef LPPlumb
	    CheckUnlocked(LockFile, 46);
#endif /* LPPlumb */
	    sprintf(ErrorText, ".Outgoing file, e.g. `%s', not owned by caller!  Files=%ld, caller=%ld.\n", LockFile, OwnerInCell, forceUid);
	    if (!RepeatError(EPERM, EPERM)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	    ++index;
	    continue;
	}

#ifdef AFS_ENV
	if (SourceQueueCell[0] == '\0' || OwnerInCell == ANONYMOUSID) {
	    sprintf(MailSourceAuth, "%d", ANONYMOUSID);
	} else {
	    PW = getcpwuid(OwnerInCell, SourceQueueCell); /* Get PW-ent for file owner in file cell */
	    if (PW != NULL) {	/* Did we get anything at all? */
		if (PostmanInCell == NULL) {
		    errno = 0;
		    PostmanInCell = CheckAMSPMName(SourceQueueCell);
		    if (PostmanInCell == NULL) {
			close(MailSourceFD);
			if (!JustLook) unlock(LockFile, *refFD, FALSE, 11);
#ifdef LPPlumb
			CheckUnlocked(LockFile, 45);
#endif /* LPPlumb */
			debug(32, ("Can't find postman for cell %s: %s\n",
					SourceQueueCell, UnixError(errno)));
			GetError("Can't find postman in cell %s: %s\n",
					SourceQueueCell, UnixError(errno));
			++index;
			continue;
		    }
		}
		if (strcmp(PW->pw_name, PostmanInCell) == 0	/* Writ by postman? */
				&& OstensibleMailSourceAuth[0] != '\0') {
			strcpy(MailSourceAuth, OstensibleMailSourceAuth);
			debug(32, ("Believing the ostensible mail owner: %s\n",
				MailSourceAuth));
		} else {	/* Must generate an Auth in case we're running as postman. */
			char *S, *D;

			sprintf(MailSourceAuth, "%d;%s;", OwnerInCell, SourceQueueCell);
			D = &MailSourceAuth[strlen(MailSourceAuth)];
			for (S = PW->pw_gecos; *S != '\0'; ++S) {
				if (*S == ';' || *S == '\\') *D++ = '\\';
				*D++ = *S;
			}
			*D++ = '\0';
		}
	    } else {
		if (IsTempWPErr(cpw_error)) {
			close(MailSourceFD);
			if (!JustLook) unlock(LockFile, *refFD, FALSE, 12);
#ifdef LPPlumb
			CheckUnlocked(LockFile, 46);
#endif /* LPPlumb */
			debug(32, ("Can't find owner %d in cell %s: %s\n",
					OwnerInCell, SourceQueueCell,
					wp_ErrorString(cpw_error)));
			GetError("Can't find owner (%d) of file %s in cell %s: %s\n",
					OwnerInCell, MailSourceFile, SourceQueueCell,
					wp_ErrorString(cpw_error));
			++index;
			continue;
		} else if (OwnerInCell == ANONYMOUSID) {
			sprintf(MailSourceAuth, "%d;%s;Anonymous", OwnerInCell, SourceQueueCell);
		} else {		/* just tell 'em the owner VUID */
			sprintf(MailSourceAuth, "%d;%s", OwnerInCell, SourceQueueCell);
		}
	    }
	}
#else /* AFS_ENV */
	sprintf(MailSourceAuth, "%d;%s", OwnerInCell, ThisDomain);
#endif /* AFS_ENV */
#ifdef LPPlumb
	if (!JustLook) CheckLocked(LockFile, 47);
#endif /* LPPlumb */

	debug(32, ("Done getting message from queue\n"));
	++index;
	return 0;		/* We've got an entry to consider */
    }

    LockFile[0] = '\0';
    debug(32, ("Done with files; cleaning up.\n"));
    FreeFiles(msgfiles, &nmsgfiles);
    SourceDirIsOpen = FALSE;
    return 1;   /* always return on end-of-directory.  Let caller decide how soon to retry.  */
}

static CheckMessageAge(age) 
    int age;
{/* Returns 0 for all OK, +1 for retouched, -1 for retouch error */
    register int fd;
    char c;
    char *EMsg;
    int Res, XRes;

    debug(64, ("Entering CheckMessageAge\n"));
    XRes = 0;
    if (age > OldMessageAge && EnqueueDate == 0) {
	debug(64, ("File is too old -- trying to retouch\n"));
	EMsg = NULL;
	fd = open(MailSourceFile, O_RDWR, 0);
	if (fd < 0) EMsg = "open";
	else {
	   Res = read(fd, &c, 1);
	   if (Res < 0) EMsg = "read";
	   else {
	      Res = lseek(fd, 0, 0);
	      if (Res < 0) EMsg = "lseek";
	      else {
		Res = write(fd, &c, 1);
		if (Res < 0) EMsg = "write";
		else {
		   Res = vclose(fd);
		   if (Res < 0) EMsg = "vclose";
		}
	      }
	   }
	}
	XRes = 1;
	if (EMsg != NULL)
	    sprintf(ErrorText, "Old queued file %s (retouch failed in %s: %s)", MailSourceFile, EMsg, UnixError(errno));
	if (fd >= 0 && EMsg != NULL) close(fd);
	if (EMsg != NULL) {
	    debug(64, (NL, ErrorText));
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    XRes = -1;
	} else {
	    debug(64, ("Retouch succeeded; postmaster will be told\n"));
	    sprintf(ErrorText, "Queue file %s is too old and has been retouched", MailSourceFile);
	    ErrorToPostmaster(ErrorText, NIL, TRUE);
	}
	debug(64, ("Postmaster warned\n"));
    }
    debug(64, ("Done checking message age\n"));
    return XRes;
}

#define MIN_MESSAGE_SIZE	2

static int CheckMessageSize(sblk, name)
    struct stat *sblk;
    char *name;
{
    /* Check its size -- avoid errors due to fucked-up files */
    debug(128, ("Message size is %d bytes; min size is %d bytes.\n", sblk->st_size, MIN_MESSAGE_SIZE));
    if ((sblk->st_size) < MIN_MESSAGE_SIZE) {
	sprintf(ErrorText, "Message too small: size is %d, min is %d, file is %s", sblk->st_size, MIN_MESSAGE_SIZE, name);
	debug(128, (NL, ErrorText));
	errno = 0;
	return -1;
    }
    return 0;
}

static int ReadMsgFromSourceFile(err)
    int *err;
{
    debug(128, ("Entering ReadMsgFromSourceFile, file: %s\n", MailSourceFile));

    *err = -1;

    /* Try to open file for reading */
#if Logs
    Log(460, "ReadMsgFromSourceFile about to open %s", MailSourceFile);
#endif /* Logs */
    MailSourceFD = open(MailSourceFile, O_RDONLY, 0);
    if (MailSourceFD < 0) {
	*err = errno;
	debug(128, ("Open failed: %d\n", errno));
	return -1;
    }
#if Logs
    Log(461, "open returned");
#endif /* Logs */
    if (fstat(MailSourceFD, &MailSourceStat) != 0) {
	*err = errno;
	close(MailSourceFD);
	debug(128, ("fstat failed: %d\n", *err));
	return -1;
    }

    /* Check size of message */
    if (CheckMessageSize(&MailSourceStat, MailSourceFile)) {
	*err = errno;
	close(MailSourceFD);
	if (*err != ENFILE && !vdown(*err) && !RepeatError(0, *err)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	return -1;
    }
    return 0;
}

static char *host()
{
#define NAMELEN 256
    static char name[NAMELEN] = { '\0' };

    if (name[0] == '\0')
	if (GetHostDomainName(name, NAMELEN) < 0)
	    sprintf(name, "Unknown host: %s", UnixError(errno));
	else
	    name[NAMELEN-1] = '\0';
    return name;
}


static MakeName(Dest, Dir, Prefix, Suffix)
    char *Dest, *Dir, Prefix, *Suffix;
{
    sprintf(Dest, "%s/%c%s", Dir, Prefix, Suffix);	/* just one char of prefix */
}

/* Returns zero if queue write succeeds */

static int WriteQueueEntry(Qdir, Tolerant, IntoSlowQueue)
    char *Qdir; int Tolerant, IntoSlowQueue;
{
    register int rc;
    register char *Auth;

    CurrentRecipients->ToName[CurrentRecipients->NextRecipient] = NIL;
    if (Tolerant && ReturnPath[0] == '\0') return -1;
    if (AuthString != NULL) {
	Auth = AuthString;
    } else if (MailSourceAuth[0] != '\0') {
	Auth = MailSourceAuth;
    } else {
	Auth = NIL;
    }
    if (EnqueueDate == 0) {
	if (IntoSlowQueue) {
		/* No enqueue-date so far, and we're about to stash it in long-term Vice storage.  Establish ``now'' as the enqueue-date. */
		if (osi_GetTimes(&TVal) == 0) EnqueueDate = TVal.Secs;
	}
    }

    rc = qmail_ext(Qdir, &CurrentRecipients->ToName[FIRSTRECIPIENT],
	       MailSourceFD, (ReturnPath[0] == '\0' ? "<>" : ReturnPath),
	       Auth, EnqueueDate, Four, 0);

    switch (rc) {
	case Q_OK:
	    return 0;
	case Q_TEMP_FAIL:
	    return -1;
	case Q_BAD_PARMS:
	case Q_OSERR:
	case Q_CANT_REWIND:
	case Q_CANT_CREAT:
	case Q_FILE_ERR:
	case Q_NO_PERM:
	case Q_DIR_ERR:
	    sprintf(ErrorText, "Bad return (%d) from qmail: %s",
		    rc, Qmail_ErrMsg);
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    return -1;
	default:
	    sprintf(ErrorText, "Impossible value from qmail: %d", rc);
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    return -1;
    }
}

/****************************************************************************\
* 									     *
* 	Try to write queue files to vice.  If user has specified a primary   *
* 	queue, just try that & the backup.  Otherwise, choose a queue name   *
* 	and try it.  If that fails, cycle through available queues until     *
* 	one is written to.  If all that fails return failure ( != 0).	     *
* 									     *
\****************************************************************************/

static int WriteToVice(UseAllQueues, UseSlowQueue)
int UseAllQueues, UseSlowQueue;
{
    register char *Auth;
    register int rc;

    /* See if user specified queues */
    if (PrimaryQueueDir[0] != '\0') {
	if (WriteQueueEntry(PrimaryQueueDir, UseAllQueues, TRUE) == 0)
	    return 0;  /* primary OK */
	if (!UseAllQueues) return 1;  /* failure if only using primary queue */
	if (BackupQueueDir[0] != '\0')
	   if (WriteQueueEntry(BackupQueueDir, SvcRegViceQueues, TRUE) == 0)
		return 0;  /* backup OK */
	if (!SvcRegViceQueues) return 1;	/* failure */
    }

    /* He didn't, or write failed -- choose queues for him */
    if (lseek(MailSourceFD, 0, 0) < 0) {
	sprintf(ErrorText, "Can't rewind file: %s", UnixError(errno));
	if (!tfail(errno)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	return 1;
    }
    CurrentRecipients->ToName[CurrentRecipients->NextRecipient] = NIL;
    if (ReturnPath[0] == '\0') return 1;
    if (AuthString != NULL) {
	Auth = AuthString;
    } else if (MailSourceAuth[0] != '\0') {
	Auth = MailSourceAuth;
    } else {
	Auth = NIL;
    }
    if (EnqueueDate == 0 && UseSlowQueue) {
	/* No enqueue-date so far, and we're about to stash it in a slow queue.  Establish ``now'' as the enqueue-date. */
	if (osi_GetTimes(&TVal) == 0) EnqueueDate = TVal.Secs;
    }
    rc = tryvicequeues_cell(&CurrentRecipients->ToName[FIRSTRECIPIENT],
		MailSourceFD, ReturnPath, Auth, EnqueueDate, Four, 0, PrevailingCell,
		(UseSlowQueue ? SlowQueueNamePrefix : MailQueueNamePrefix));

    /* Now check return code */
    switch (rc) {
	case Q_OK:
	    return 0;
	case Q_TEMP_FAIL:
	    return 1;
	case Q_BAD_PARMS:
	case Q_OSERR:
	case Q_CANT_REWIND:
	case Q_CANT_CREAT:
	case Q_FILE_ERR:
	case Q_NO_PERM:
	case Q_DIR_ERR:
	    sprintf(ErrorText, "Bad return (%d) from tryvicequeues: %s", rc, Qmail_ErrMsg);
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    return 1;
	default:
	    sprintf(ErrorText, "Impossible value from tryvicequeues: %d (%s)", rc, Qmail_ErrMsg);
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    return 1;
    }
}

static Header(f, msg)
    register FILE *f;
    char *msg;
{
    fprintf(f, "Date: %s", arpadate());
    fprintf(f, "From: Queuemail %s <%s+@%s>\n",
			IsDaemon ? "Daemon" : "Process",
			PostmasterName, ThisDomain);
    fprintf(f, "Subject: %s\n\n", msg);
}

static int ErrorToPostmaster(Msg, AuxMsg, SendBody)
    char *Msg, *AuxMsg;
    Boolean SendBody;
{
    char Text[sizeof ErrorText];
    register FILE *f;
    int i; 
    char *argv[3];
    char PMMailbox[200];

    debug(512, ("Entering ErrorToPostmaster\n"));
    if (JustLooking) return 0;
    if (Debugging & 4) {
	debug(512, ("Not actually sending the message\n"));
	printf("Not sending error to postmaster: %s\n", Msg);
	fflush(stdout);
	return 0;
    }
    argv[0] = "vicemail";
    sprintf(PMMailbox, "-m%s", PostmasterMailbox);
    argv[1] = PMMailbox;
    argv[2] = NULL;
    f = (FILE *) qopen(vicemail, argv, "w");
    if (f == NULL) {
	debug(512, ("Horrible bailout: can't run vicemail\n"));
	sprintf(Text, "Can not run vicemail to give error to postmaster: %s", Msg);
	bailout(Text, AuxMsg, SendBody);
	return -1;
    }
    Header(f, Msg);
    fprintf(f, "Queuemail error: %s\n", Msg);
    if (AuxMsg != NIL) fprintf(f, "Error info: %s\n", AuxMsg);
    fputs("Current To list is: ", f);
    ListAllRecipients(f, ", ");
    fprintf(f, "\nUser was: %s/%d/%d/%s\nHost was: %s\n", WhoIAm, MyUID, MyEUID, PrevailingCell, host());
    fprintf(f, "Return path was: ``%s''\n", ReturnPath);
    fprintf(f, "Original command line:\n\t");
    for (i=0; i < OriginalArgc; ++i) fprintf(f, "%s ", OriginalArgv[i]);
    fputc('\n', f);
    if (SendBody) {
	fprintf(f, "\nMail contents follow:\n\n");
	fprintf(f, "\t\t********************************\n\n");
	WriteMessage(f);
    }
    if (qclose(f) != 0) {
	debug(512, ("Error in qclose while sending postmaster error\n"));
	sprintf(Text, "Error from vicemail sending postmaster error: %s\n", Msg);
	bailout(Text, AuxMsg, SendBody);
	return -1;
    }
    return 0;
}

static int bailout(mesg, auxmsg, sendbody)
    char *mesg, *auxmsg;
    Boolean sendbody;
{
    register FILE * f;
    register int i;
    char current[MAXPATHLEN+1];

    debug(8, ("Entering bailout routine\n"));

    if (Debugging & 4) {
	debug(8, ("Not writing last-ditch error file: %s\n", mesg));
	f = stdout;
	strcpy(current, "no-file");
    } else {
		/* Try to create error file */
	f = NULL;		/* Just in case */
	for (i = 0; i < nlastditchprefixes; ++i) {

	    sprintf(current, "%s/%s", lastDitchPrefixes[i], ams_genid(1));
 	    f = fopen(current, "w");
	    if (f != NULL)
		break;
	}

    /* If that fails, I give up */
	if (f == NULL) {
	    errprintf(Qname, ERR_CRITICAL, NULL, NULL, "BAILOUT: %s", mesg);
	    abort();
	}
    }
    fprintf(f, "Received: from %s via queuemail(bailout) ID <%s>; %s", host(), current, arpadate());
    Header(f, mesg);
    fprintf(f, "Queuemail error: %s\n", mesg);
    if (auxmsg != NIL) fprintf(f, "Error info: %s\n", auxmsg);
    fprintf(f, "Original command line:\n\t");
    for (i = 0; i < OriginalArgc; ++i) {
	fprintf(f, "%s ", OriginalArgv[i]);
    }
    fprintf(f, "\n\nMail %s follow...\n\n", (sendbody ? "contents" : "headers"));
    fprintf(f, "\t\t********************************\n\n");
    fputs("Current To list is: ", f);
    ListAllRecipients(f, ", ");
    fprintf(f, "\nUser was: %s/%d/%d/%s\nHost was: %s\n", WhoIAm, MyUID, MyEUID, PrevailingCell, host());
    fprintf(f, "Return path was: ``%s''\n\n", ReturnPath);
    if (sendbody) WriteMessage(f);
    if (!(Debugging & 4)) fclose(f);

    return 0;
}

#define MESSAGE_SIZE	256

static int ReadNextMessage(f, code, RefBuffer, RefLen, pgmname)
    register FILE *f;
    int *code;
    char **RefBuffer;
    int *RefLen;
    char *pgmname;
{
    int cont, Ix, Ch;
    register char *next;
    Boolean first;
    register int count;
    char *buffer = *RefBuffer;
    int len = *RefLen;
#define HowMuchMore 80
    static char MoreText[HowMuchMore+1];
    static char eofmsg[64];

    /* Allocate buffer if necessary */
    if (buffer == NULL) {
	debug(32768, ("Allocating initial buffer of %d bytes\n", MESSAGE_SIZE));
#if Logs
	Log(600, "ReadNextMessage allocating initial buffer");
#endif /* Logs */
	buffer = malloc(MESSAGE_SIZE);
#if Logs
	Log(601, "malloc returns 0x%x", buffer);
#endif /* Logs */
	if (buffer == NULL) {
	    static char mesg[] = "Initial MALLOC failed in ReadNextMessage";
	    debug(32768, (NL, mesg));
	    ErrorToPostmaster(mesg, NIL, FALSE);
	    return -1;
	}
	len = MESSAGE_SIZE;
    }

    sprintf(eofmsg, "Unexpected END-OF-FILE from %s", pgmname);

    /* Now read the message */
    next = buffer;
    count = 0;
    first = TRUE;	/* Flags 1st attempt to read */
    do {
	static char cbuf[3];

	/* 1st, read result code */
#if LogsYes
	Log(610, "ReadNextMessage about to read a code");
#endif /* LogsYes */
	if (fread(cbuf, sizeof(char), 3, f) == 0) {
	    if (ferror(f) != 0) {
		sprintf(ErrorText, "Can't get code from %s: %d (%s)", pgmname, errno, UnixError(errno));
		debug(32768, (NL, ErrorText));
		if (!tfail(errno)) ErrorToPostmaster(ErrorText, NIL, FALSE);
	    } else
		/*
		   If this is the 1st time through, just ignore EOF,
		   it is likely that queuemail just didn't start because
		   there wasn't enough virtual memory.
		*/
		if (!first) ErrorToPostmaster(eofmsg, NIL, FALSE);
	    return -1;
	}
	first = FALSE;
	debug(32768, ("Code: %c%c%c\n", cbuf[0], cbuf[1], cbuf[2]));

	/* Continuation char */
	cont = fgetc(f);
	if (cont == EOF) {
	    ErrorToPostmaster(eofmsg, NIL, FALSE);
	    return -1;
	}
	debug(32768, ("Continuation char is '%c'\n", cont));
	for (Ix = 0; Ix < 3; ++Ix) {
	    if (cbuf[Ix]<'0' || cbuf[Ix]>'9'
		    || (cont!=tmflag_More && cont!=tmflag_End)) {
		for (Ix = 0; Ix < HowMuchMore; ++Ix) {
			Ch = fgetc(f);
			if (Ch == EOF) break;
			MoreText[Ix] = (Ch == '\n' ? '^' : Ch);
		}
		MoreText[Ix] = '\0';
		for (Ix = 0; Ix < 3000; ++Ix) {Ch = fgetc(f); if (Ch == EOF) break;}
		sprintf(ErrorText, "Bad prefix from %s: ``%c%c%c%c'' (then ``%s'')", pgmname, cbuf[0], cbuf[1], cbuf[2], cont, MoreText);
		debug(32768, (NL, ErrorText));
		ErrorToPostmaster(ErrorText, NIL, FALSE);
		return -1;
	    }
	}

	*code = (cbuf[0]-'0')*100 + (cbuf[1]-'0')*10 + (cbuf[2]-'0');
#if LogsYes
	Log(611, "ReadNextMessage has code (%d), reading text", *code);
#endif /* LogsYes */
	/* Read a line of the actual message */
	for (;;) {
	    register int c;
	    c = fgetc(f);
	    if (c == EOF) {
		ErrorToPostmaster(eofmsg, NIL, FALSE);
		return -1;
	    }
	    if ((c != '\n' || cont == tmflag_More))
	      if (++count >= len) {
		/* New character to be inserted & there's no room */
		debug(32768, ("REALLOCating buffer, new size is %d bytes\n",
			      len+MESSAGE_SIZE));
		buffer = realloc(buffer, len+MESSAGE_SIZE);
		if (buffer == NULL) {
		    static char mesg[] = "REALLOC failed in ReadNextMessage";
		    debug(32768, (NL, mesg));
		    ErrorToPostmaster(mesg, NIL, FALSE);
		    return -1;
		}
		next = buffer + len - 1;
		len += MESSAGE_SIZE;
	      }
	    if (c == '\n') {
		if (cont == tmflag_More) *next++ = '\n';
		break;
	    }
	    *next++ = c;
	}
#if Logs
	Log(612, "ReadNextMessage read text too");
#endif /* Logs */
    } while (cont == tmflag_More);

    debug(32768, ("ReadNextMessage returning normally...\n"));
    *next = '\0';
    *RefBuffer = buffer;
    *RefLen = len;
    return 1;
}

/**************************\
* 			   *
*  Decoding address codes  *
* 			   *
\**************************/

static Boolean GlobalTempFailure(code)
    int code;
{
    return (code >= tm_GlobalTempFailLowest) && (code <= tm_GlobalTempFailHighest);
}

static Boolean GlobalPermFailure(code)
    int  code;
{
    return (code >= tm_GlobalPermFailLowest) && (code <= tm_GlobalPermFailHighest);
}

static Boolean GlobalFailure(code)
    int code;
{
    return GlobalTempFailure(code) || GlobalPermFailure(code);
}

static Boolean AddrDelivered(code)
    int code;
{
    return (code >= tm_AddrDeliveredLowest && code <= tm_AddrDeliveredHighest);
}
#ifdef NOTDEF
static Boolean AddrTempFail(code)
    int code;
{
    return (code >= tm_AddrTempFailLowest && code <= tm_AddrTempFailHighest);
}

static Boolean AddrPermFail(code)
    int code;
{
    return (code >= tm_AddrPermFailLowest && code <= tm_AddrPermFailHighest);
}
#endif /* NOTDEF */

static int ReadAddresses(f, pgmname)
    register FILE *f;
    char *pgmname;
{
    char *line;		/* Read results here */
    int code, len;

    debug(32768, ("Entering ReadAddresses\n"));

    AnyDelivered = FALSE;

    /* Read each line */
    line = NULL;
    for (;;) {
	int result;

#if Logs
	Log(620, "ReadAddresses calling ReadNextMessage");
#endif /* Logs */
	result = ReadNextMessage(f, &code, &line, &len, pgmname);
#if Logs
	Log(621, "ReadNextMessage returned (%d) to ReadAddresses", result);
#endif /* Logs */
	debug(32768, ("ReadNextMessage returns %d (code %d, len %d)\n", result, code, len));
	if (result < 0) return -1;	/* Error */
	debug(32768, ("Next message read, code: %d, message: \"%s\"\n", code, line));
	if (code == tm_EndOfInteraction) break;

	/* Look at code & decide what to do with message */
	if (GlobalFailure(code)) {
	    if (GlobalPermFailure(code)) {
		if (code != tmgpf_DestinationSyntaxError) {
			sprintf(ErrorText, "Failure from %s: %d %s\n", pgmname, code, line);
			ErrorToPostmaster(ErrorText, NIL, FALSE);
		}
	    }
	    /* Just bail out after global error -- no more messages will come */
	    return 1;
	} else {	/* not global failure */
	    if (AddrDelivered(code)) AnyDelivered = TRUE;
	    else {
		if (AddRecipient(&NewRecipients, copy(line))) return -1;
	    }
	}
    }

    debug(32768, ("Freeing line buffer at 0x%x\n", line));
    if (line != NULL) free(line);
    return 0;
}

static Boolean Contains(small, large)
    register struct RECIPIENTS *small, *large;

{/* Return whether each element of small is somewhere in large. */

    register int inSmall, inLarge, WasInLarge;

    for (inSmall=FIRSTRECIPIENT; inSmall<small->NextRecipient; ++inSmall) {
	WasInLarge = 0;
	for (inLarge=FIRSTRECIPIENT; inLarge<large->NextRecipient; ++inLarge) {
	    if (strcmp(small->ToName[inSmall], large->ToName[inLarge]) == 0) {
		WasInLarge = 1;
		break;
	    }
	}
	if (WasInLarge == 0) return FALSE;
    }
    return TRUE;
}

static Boolean EqualLists(l1, l2)
    register struct RECIPIENTS *l1, *l2;
{
    if (!AnyDelivered) return TRUE;
    if (l1->NextRecipient != l2 ->NextRecipient)
	return FALSE;

    /*
       Have to compare each address.  But the new addresses might
       appear in any order!  Check that each contains the other.
    */
    if (Contains(l1, l2) == 0) return FALSE;
    if (Contains(l2, l1) == 0) return FALSE;

    return TRUE;
}

#define TRYMAIL_TIME_INTERVAL	(20*MINUTE)
#define SWITCHMAIL_TIME_INTERVAL	(40*MINUTE)

static int TryLocalDelivery()
{
    static char *argv[17];
    static char *program;
    int WillWait, i, pgrp, status, timedout, result, termcode;
    FILE *r, *w;
    char AuthValue[450], EnqDateValue[30];
    char ForStringValue[MAXNAMESIZE+2];
#ifdef AFS_ENV
    char cellArg[2*MAXCELLCHARS + 5];
#endif /* AFS_ENV */

    /* If no recipients, send through long route */
    if (CurrentRecipients->NextRecipient <= FIRSTRECIPIENT) return -1;

    program = (UseOldSendmail
		    ? switchmail
		    : trymail);
    /* Set up arguments to trymail */
    i = -1;
    argv[++i] = (UseOldSendmail ? "switchmail" : "trymail");
    argv[++i] = "-F";
    if (Debugging & 32768) argv[++i] = "-d";
    if (AuthString != NULL) {
	sprintf(AuthValue, "-A%s", AuthString);
	argv[++i] = AuthValue;
    } else if (MailSourceAuth[0] != '\0') {
	sprintf(AuthValue, "-A%s", MailSourceAuth);
	argv[++i] = AuthValue;
    }
    if (EnqueueDate != 0) {
	sprintf(EnqDateValue, "-E%d", EnqueueDate);
	argv[++i] = EnqDateValue;
    }
    if (Four != NULL) {
	sprintf(ForStringValue, "-4%s", Four);
	argv[++i] = ForStringValue;
    }
#ifdef AFS_ENV
    if (GotAMSHomeCell && AMSHomeCell[0] != '\0') {
	sprintf(cellArg, "-Ch%s", AMSHomeCell);
	argv[++i] = cellArg;
    }
#endif /* AFS_ENV */
#if LogsYes
    if (Logging) argv[++i] = "-t";
#endif /* LogsYes */
    if (TrymailArg != NULL) argv[++i] = TrymailArg;
    argv[++i] = MailSourceFile;

#if Logs
    Log(630, "TryLocalDelivery going to call %s", argv[0]);
#endif /* Logs */
    debug(8, ("WhoIAm ``%s'', ReturnPath now ``%s''.\n",
	     WhoIAm, ReturnPath));
    if (ReturnPath[0] == '\0') return -1;
    argv[++i] = ReturnPath;
    argv[++i] = NULL;
    if (Debugging & 32768) {
	int j;

	printf("Entering TryLocalDelivery, command: %s", program);
	for (j=0; j < i; ++j) printf(" %s", argv[j]);
	putchar('\n');
	fflush(stdout);
    }

    /* Fork trymail & have 2-way pipes set up */
#if LogsYes
    Log(631, "TryLocalDelivery about to t2open %s", argv[0]);
#endif /* LogsYes */
    pgrp = t2open(program, argv, &r, &w);
#if LogsYes
    Log(632, "t2open returned %d--about to call ListAllRecipients", pgrp);
#endif /* LogsYes */
    if (pgrp < 0) {
	debug(32768, ("t2open failed: %d\n", pgrp));
	return -1;
    }

    /* Send it the list of addresses */
    ListAllRecipients(w, ", ");
    debug(32768, ("About to close the address output to %s.\n", argv[0]));
#if Logs
    Log(633, "TryLocalDelivery about to call fclose on t2opened pipe");
#endif /* Logs */
    fclose(w);
#if Logs
    Log(634, "fclose done");
#endif /* Logs */

    /* Read back messages */
#if Logs
    Log(635, "TryLocalDelivery about to call ReadAddresses");
#endif /* Logs */
    result = ReadAddresses(r, argv[0]);
#if Logs
    Log(636, "ReadAddresses returns %d to TryLocalDelivery", result);
#endif /* Logs */
    debug(32768, ("ReadAddresses returns %d\n", result));
    if (result < 0) killpg(pgrp, SIGKILL);	/* format error or some such */

    /* Wait for it to finish */
    debug(32768, ("About to call t2close on %s\n", argv[0]));
#if Logs
    Log(637, "TryLocalDelivery about to call t2close on %s", argv[0]);
#endif /* Logs */
    argv[0] = (UseOldSendmail ? "switchmail" : "trymail");
    WillWait = (UseOldSendmail ?
		SWITCHMAIL_TIME_INTERVAL :
		TRYMAIL_TIME_INTERVAL),
    status = t2close(r, WillWait, &timedout);
#if Logs
    Log(638, "t2close returned; status 0x%04x, timedout: %d", status, timedout);
#endif /* Logs */
    debug(32768, ("T2CLOSE status: 0x%04x, timedout: %d\n", status, timedout));

    /* First, check for timeout */
    if (status == -1 || timedout != 0) {
	/* It timed out */
	killpg(pgrp, SIGKILL);
	sprintf(ErrorText, "Had to kill %s.  Timeout %d, file %s", argv[0], WillWait, MailSourceFile);
	ErrorToPostmaster(ErrorText, NIL, FALSE);
	return -1;
    }

    /* Next, check termination status byte */
    termcode = status & 0x7f;	/* Ignore core dump bit */
    switch (termcode) {
	case 0:		/* All is cool */
			break;
	case SIGSEGV:	/* Guess out of VM */
			debug(32768, ("Out of VM (?)\n"));
			return -1;
	case SIGKILL:	/* Killed (via Butler reclaim or kernel needing swap space) */
			debug(32768, ("Killed\n"));
			return -1;
	default:	debug(32768, ("Problem: 0x%04x\n", status));
			sprintf(ErrorText, "Bad termination from %s: 0x%04x\n", argv[0], status);
			if (result == 0) ErrorToPostmaster(ErrorText, NIL, FALSE);
			return -1;
    }

    /* Now check for special t2open exit indicating execv failed */
    status = status >> 8;
    if (status == 0377) {/* Couldn't exec TRYMAIL */
	return -1;
    }

    if (result != 0) {	/* Didn't get individual messages back from TRYMAIL */
	return -1;	
    }

    /* It went okay, see if any names left to deliver */
    if (NewRecipients.NextRecipient > FIRSTRECIPIENT) {
	debug(32768, ("Addresses left to queue:\n"));

#if Logs
	Log(639, "TryLocalDelivery: OK, with some addresses left to queue");
#endif /* Logs */
	GotChangedAddresses = !EqualLists(CurrentRecipients, &NewRecipients);
	CurrentRecipients = &NewRecipients;
	if (Debugging & 32768) {
	    ListAllRecipients(stdout, ", ");
	    putchar('\n');
	}
	return -1;
    }

    /* No addresses left */
    debug(32768, ("No addresses left to queue\n"));
#if Logs
    Log(640, "TryLocalDelivery: OK, all addresses queued");
#endif /* Logs */
    return 0;
}

static Boolean bol;

static char *GetNextToken(fp) 
    register FILE *fp;
{
    register Boolean StillLooking = TRUE;
    register Boolean InQuotes = FALSE;
    int TokenPointer = 0;
    static char Token[MAXNAMESIZE];
    register int c;

    debug(8, ("Entering GetNextToken\n"));

    while (StillLooking) {
	switch (c=getc(fp)) {
	    case EOF:
		StillLooking = FALSE;
		break;
	    case ' ':
	    case ',':
	    case ';':
	    case '\t':
	    case '\n':
		if (InQuotes)
		    Token[TokenPointer++] = c;
		else {
		    if (c == '\n') bol = TRUE;
		    if (TokenPointer) StillLooking = FALSE;
		}
		break;
	    case '"':
		InQuotes = ! InQuotes;
		break;
	    case '|':
		c = getc(fp);
		if (c == '|') {
		    /* Escaped '|' */
		    Token[TokenPointer++] = c;
		    break;
		}
		/* Bracketed token */
		for (;;) {
		    switch (c) {
			case EOF: debug(8, ("Unexpected EOF in GetNextToken"));
				  sprintf(ErrorText, "EOF in bracketed token in shadow file \"%s\"", MailSourceFileShadow);
				  ErrorToPostmaster(ErrorText, NIL, TRUE);
				  return NULL;
			case '|': c = getc(fp);
				  if (c == '|')
				      Token[TokenPointer++] = c;
				  else {
				      ungetc(c, fp);
				      StillLooking = FALSE;
				  }
				  break;
			default:  Token[TokenPointer++] = c;
		    }
		    if (StillLooking)
			c = getc(fp);
		    else
			break;
		}
		break;
	    default:
		Token[TokenPointer++] = c;
		break;
	}
    }
    Token[TokenPointer] = '\0';
    if (TokenPointer > 0) {
	debug(8, ("Got token: %s\n", Token));
	return Token;
    } else {
	debug(8, ("Could not get a token\n"));
	return NULL;
    }
}	    

static char WhoIAmSave[MAXNAMESIZE];
static char SaveForString[MAXNAMESIZE];

static int ParseShadowFile()
{
    register char *s;
    FILE *f;
    register enum {
		UNKNOWN_FIELD, TO_FIELD, FROM_FIELD, AUTH_FIELD,
		ENQUEUEDATE_FIELD, FOR_FIELD, HOLDUNTIL_FIELD,
		IGNORE_FIELD
	} WhichField;
    int NewEnqDate = 0;

    debug(1024, ("Entering ParseShadowFile routine\n"));
    debug(1024, ("Shadow file name of %s is %s\n",
		 MailSourceFile, MailSourceFileShadow));
    WhichField = IGNORE_FIELD;
    if ((f=fopen(MailSourceFileShadow, "r")) == NULL) {
	int err = errno;
	debug(1024, ("But I can't open it\n"));
	if (err != ENOENT && err != ENFILE && !vdown(err) && !RepeatError(EACCES, err)) {
	    sprintf(ErrorText, "Can not open shadow file %s: %s", MailSourceFileShadow, UnixError(err));
	    ErrorToPostmaster(ErrorText, NIL, FALSE);
	}
	errno = err;
	return -1;
    }

    OstensibleMailSourceAuth[0] = '\0';
    EnqueueDate = 0;
    Four = NULL;
    bol = TRUE;
    while ((s = GetNextToken(f)) != NULL) {
	if (s[0] == '#' && bol) {
	    debug(1024, ("Magic token\n"));
	    if (strcmp(s, "#To") == 0)
		WhichField = TO_FIELD;
	    else if (strcmp(s, "#From") == 0)
		WhichField = FROM_FIELD;
	    else if (strcmp(s, "#Auth") == 0)
		WhichField = AUTH_FIELD;
	    else if (strcmp(s, "#EQD") == 0)
		WhichField = ENQUEUEDATE_FIELD;
	    else if (strcmp(s, "#For") == 0)
		WhichField = FOR_FIELD;
	    else if (strcmp(s, "#Hold") == 0)
		WhichField = HOLDUNTIL_FIELD;
	    else if (strncmp(s, "#X", 2) == 0)		/* Field name start with ``X''? */
		WhichField = IGNORE_FIELD;	/* for future compatibility */
	    else {
		sprintf(ErrorText, "Shadow %s contains unknown field %s", MailSourceFileShadow, s);
		debug(1024, (NL, ErrorText));
		ErrorToPostmaster(ErrorText, NIL, FALSE);
		WhichField = UNKNOWN_FIELD;
	    }
	    bol = FALSE;
	} else {
	    switch (WhichField) {
		case TO_FIELD:
		    debug(1024, ("To: %s\n", s));
		    if (AddRecipient(CurrentRecipients, copy(s))) {
			fclose(f);
			errno = ENOMEM;
			return -1;
		    }
		    break;
		case FROM_FIELD:
		    debug(1024, ("From: %s\n", s));
		    if (SetReturnPath(s) == 0) {fclose(f); errno = 0; return -1;}
		    strcpy(WhoIAmSave, s);
		    WhoIAm = WhoIAmSave;
		    break;
		case AUTH_FIELD:
		    debug(1024, ("Auth: %s\n", s));
		    strncpy(OstensibleMailSourceAuth, s, sizeof(OstensibleMailSourceAuth));
		    break;
		case ENQUEUEDATE_FIELD:
		    debug(1024, ("Enqueued: %s\n", s));
		    NewEnqDate = atoi(s);
		    break;
		case FOR_FIELD:
		    debug(1024, ("For: %s\n", s));
		    strcpy(SaveForString, s);
		    Four = SaveForString;
		    break;
		case HOLDUNTIL_FIELD:
		    debug(1024, ("HoldUntil: %s\n", s));
		    break;
		case IGNORE_FIELD:	/* Do nothing with future-extension fields. */
		    break;
		default:
		case UNKNOWN_FIELD:
		    debug(1024, ("Ignoring arg to unknown field: %s\n", s));
		    break;
	    }
	}
    }
    fclose(f);
    if (NewEnqDate != 0) {	/* Do sanity checks first. */
	if (osi_GetTimes(&TVal) == 0) { /* sanity check on EnqueueDate */
	    if (NewEnqDate < (TVal.Secs - 35*24*60*60)) {
		    EnqueueDate = TVal.Secs - 35*24*60*60;
		    sprintf(ErrorText, "Shadow %s date %d too early; setting to %d",
				MailSourceFileShadow,
				NewEnqDate, EnqueueDate);
		    debug(1024, (NL, ErrorText));
		    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    } else if (NewEnqDate > (TVal.Secs + 30*60)) {
		    EnqueueDate = TVal.Secs;
		    sprintf(ErrorText, "Shadow %s date %d too late; setting to %d",
				MailSourceFileShadow,
				NewEnqDate, EnqueueDate);
		    debug(1024, (NL, ErrorText));
		    ErrorToPostmaster(ErrorText, NIL, FALSE);
	    } else EnqueueDate = NewEnqDate;
	}
    }
    debug(1024, ("Shadow file parsed, %d recipients\n",
		CurrentRecipients->NextRecipient-MAILARGS));
    return 0;
}

static CheckConsistentInvocation()
{
    if (OutgoingDaemon || JustLooking) return;

    if ((!strcmp(SourceQueueDir, PrimaryQueueDir)
	    && !UseOldSendmail && BypassLocalDelivery)
	|| (IsDaemon && ReadStdin)
	|| (!ReadStdin && !IsDaemon && MailSourceFile[0] == '\0'))
    {
	FatalError("Arguments to queuemail are inconsistent--aborting",
		   NIL, FALSE, EX_USAGE);
    }
}

static CheckAuthorization(mesg)
    char *mesg;
{
    char *cellWho;
    if (MyUID == 0) return;
    cellWho = CheckAMSPMName(PrevailingCell);
    if (cellWho != NULL && strcmp(WhoIAm, cellWho) == 0) return;
    sprintf(ErrorText, "User (\"%s\"/%d/%s) not authorized to %s", WhoIAm, MyUID, PrevailingCell, mesg);
    FatalError(ErrorText, NIL, FALSE, EX_USAGE);
}

/* File locking and unlocking stuff */

static int lock(lockfile, idIx)
    char *lockfile; int idIx;
{
    int fd, err;

    debug(8192, ("Locking %s...", lockfile));
#if LogsYes
    Log(750, "lock: about to open %s", lockfile);
#endif /* LogsYes */
    fd = open(lockfile, osi_O_READLOCK, 0);
#if LogsYes
    Log(751, "lock: open returned %d, about to flock", fd);
#endif /* LogsYes */
    if (fd < 0) {
	/* Couldn't open file, see why */
	if (errno == ENOENT || tfail(errno)) {
	    /* Vice/Venus is down, or file has gone away */
	    debug(8192, ("open failed: AFS down, %d\n", errno));
	    return -1;
	} else {
	    int err;
	    /* Unknown problem, this is serious */
	    err = errno;
	    debug(8192, ("open failed: unknown, %s\n", UnixError(err)));
	    if (!RepeatError(EACCES, err)) {
		sprintf(ErrorText, "Can't open lock file: %s, %s", lockfile, UnixError(err));
		ErrorToPostmaster(ErrorText, NIL, FALSE);
	    }
	    return -1;
	}
    } else {
	/* Got it, now try for lock */
	if (osi_ExclusiveLockNoBlock(fd) < 0) {
#if LogsYes
	    Log(752, "lock: lock failed with errno=%d", errno);
#endif /* LogsYes */
	    /* It failed, see why */
	    if (errno == EWOULDBLOCK) {
		close(fd);
		return -1;
	    } else {
		/* If error is EINVAL other guy deleted it before I tried */
		if
#ifdef AFS_ENV
		    (errno != EINVAL && errno != VNOVNODE && !tfail(errno))
#else /* AFS_ENV */
		    (errno != EINVAL && !tfail(errno))
#endif /* AFS_ENV */
		    {
		    /* Some other reason -- trouble */
		    err = errno;
		    debug(8192, ("lock failed: unknown, %d\n", err));
		    if (!RepeatError(EACCES, err)) {
			sprintf(ErrorText, "Flock failed on %s: %s", lockfile, UnixError(err));
			ErrorToPostmaster(ErrorText, NIL, FALSE);
		    }
		}
		close(fd);
		return -1;
	    }
	}
#ifdef LPPlumb
	SetLocked(lockfile, idIx);
#endif /* LPPlumb */
    }

#if LogsYes
    Log(753, "lock: flock succeeded");
#endif /* LogsYes */
    debug(8192, ("succeeded: %d\n", fd));
    return fd;
}

static int unlock(lockfile, fd, delete, idIx)
    char *lockfile;
    int fd, idIx;
    Boolean delete;
{
    debug(8192, ("Unlock(%s, %d, %s)...", lockfile, fd, prbool(delete)));
    if (delete) {
#if LogsYes
	Log(760, "unlock: about to unlink %s", lockfile);
#endif /* LogsYes */
	if (unlink(lockfile) < 0) {
	    int err;
	    err = errno;
	    sync();
	    debug(8192, ("unlink failed: %d\n", err));
	    if (!tfail(err) && !RepeatError(EACCES, err)) {
		sprintf(ErrorText, "Can't unlink lockfile %s: %s", lockfile, UnixError(err));
		ErrorToPostmaster(ErrorText, NIL, FALSE);
	    }
	    close(fd);
	    return -1;
	} else {
#if LogsYes
	    Log(761, "unlock: unlink succeeded");
#endif /* LogsYes */
	    debug(8192, ("succeeded\n"));
	    close(fd);
#if Logs
	    Log(762, "unlock: close done, about to sync()");
#endif /* Logs */
	    sync();
#if Logs
	    Log(763, "unlock: sync() done");
#endif /* Logs */
#ifdef LPPlumb
	    ClearLocked(lockfile, idIx);
#endif /* LPPlumb */
	    return 0;
	}
    } else {	/* Unlock only */
#if LogsYes
	Log(770, "unlock: about to flock(%d, LOCK_UN)", fd);
#endif /* LogsYes */
	if (osi_UnLock(fd) < 0) {
#ifdef AFS_ENV
	    if (errno != VNOVNODE) {
#endif /* AFS_ENV */
		int err;
		err = errno;
		debug(8192, ("unlock failed: %d\n", err));
		if (!tfail(err) && !RepeatError(EACCES, err)) {
		    sprintf(ErrorText, "Can't unlock lockfile %s: %s", lockfile, UnixError(err));
		    ErrorToPostmaster(ErrorText, NIL, FALSE);
		}
#ifdef AFS_ENV
	    }
#endif /* AFS_ENV */
	    close(fd);
	    return -1;
	} else {
#if LogsYes
	    Log(771, "unlock: flock succeeded, about to close");
#endif /* LogsYes */
	    close(fd);
#if Logs
	    Log(772, "unlock: close done, returning");
#endif /* Logs */
	    debug(8192, ("succeeded\n"));
#ifdef LPPlumb
	    ClearLocked(lockfile, idIx);
#endif /* LPPlumb */
	    return 0;
	}
    }
}

static int MapErrno(errval, dflt)
    int errval, dflt;
{/* Maps an errno value to an exit status for local disk usage. */

    if (vdown(errval)) return EX_TEMPFAIL;

    switch (errval) {
	case EINTR:
	case EIO:
	case ENXIO:
	case EAGAIN:
	case ENOMEM:
	case ENFILE:
	case EMFILE:
	case ETXTBSY:
	case ENOSPC:
	case ENETDOWN:
	case ENETUNREACH:
	case ENETRESET:
	case ECONNABORTED:
	case ECONNRESET:
	case ENOBUFS:
	case ETIMEDOUT:
	case ECONNREFUSED:
	case EHOSTDOWN:
	case EHOSTUNREACH:
#ifdef EDQUOT
	case EDQUOT:
#endif /* EDQUOT */
	    return EX_TEMPFAIL;
	default:
			    return dflt;
    }
}

/*
   Create a temporary file to hold the text of a message.  The file name
   will be placed in the global MailSourceFile, the descriptor of the
   open file in MailSourceFD.  The file will be opened for reading/writing.
*/

#define MAXTEMPTRIES	10

static CreateMessageTemp()
{
    register int i, pid, errn;

    debug(128, ("Entering CreateMessageTemp\n"));
    pid = getpid();
    for (i=0; i<MAXTEMPTRIES; ++i) {
	sprintf(MailSourceFile, "/tmp/QMT%d.%d", pid, i);
#if Logs
	Log(780, "CreateMessageTemp: try %d, opening %s", i, MailSourceFile);
#endif /* Logs */
	MailSourceFD = open(MailSourceFile, O_RDWR|O_CREAT|O_EXCL, 0600);
#if Logs
	Log(781, "CreateMessageTemp: open returned %d", MailSourceFD);
#endif /* Logs */
	errn = errno;
	debug(128, ("Trying %s: %d\n", MailSourceFile, errn));
	if (MailSourceFD >= 0) break;
    }

    if (i >= MAXTEMPTRIES) {
	sprintf(ErrorText, "Can't create temporary message file \"%s\": %s", MailSourceFile, UnixError(errn));
	debug(128, (NL, ErrorText));
	if (RepeatError(ENOSPC, errn)) {
		FoldTent(EX_TEMPFAIL);
	}
	if (errn != ENOSPC && tfail(errn)) FoldTent(EX_TEMPFAIL);
	FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : MapErrno(errn, EX_USAGE));
    }

    debug(128, ("Succeeded\n"));
}

static CleanupMessageText()
{
#if Logs
    Log(785, "CleanupMessageText: about to unlink/close");
#endif /* Logs */
    if (ReadStdin) unlink(MailSourceFile);
    close(MailSourceFD);
#if Logs
    Log(786, "CleanupMessageText: returning");
#endif /* Logs */
}

/*
   Get message text into file (if necessary).  Make sure file is
   open for reading.  Leave name of file in global MailSourceFile,
   leave descriptor for file in MailSourceFD.
*/

static int GetMessageText()
{
    register int size;
    int olderrno;

    debug(128, ("Entering GetMessageText\n"));

    if (!ReadStdin) {
	/* Message in file, try to open it */
	debug(128, ("Attemtping to open %s\n", MailSourceFile));
#if LogsYes
	Log(790, "GetMessageText: about to open %s", MailSourceFile);
#endif /* LogsYes */
	MailSourceFD = open(MailSourceFile, O_RDONLY, 0);
#if LogsYes
	Log(791, "GetMessageText: open returned %d", MailSourceFD);
#endif /* LogsYes */
	if (MailSourceFD < 0) {
	    olderrno = errno;
	    debug(128, ("Can't open it: %s\n", UnixError(olderrno)));
	    if (tfail(olderrno)) FoldTent(EX_TEMPFAIL);
	    sprintf(ErrorText, "Can't open source file \"%s\": %s", MailSourceFile, UnixError(olderrno));
	    FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : MapErrno(olderrno, EX_DATAERR));
	}
	debug(128, ("Opened it: %d\n", MailSourceFD));
	if (fstat(MailSourceFD, &MailSourceStat) != 0) {
	    olderrno = errno;
	    debug(128, ("fstat failed: %d\n", olderrno));
	    close(MailSourceFD);
	    sprintf(ErrorText, "Stat of message file %s failed: %s", MailSourceFile, UnixError(olderrno));
	    if (!tfail(olderrno)) FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : MapErrno(olderrno, EX_DATAERR));
	    return -1;
	}
	if (CheckMessageSize(&MailSourceStat, MailSourceFile)) {
	    olderrno = errno;
	    close(MailSourceFD);
	    if (!RepeatError(0, olderrno)) FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : EX_DATAERR);
	    return -1;
	}
	return 0;
    }

    /* Have to read from standard input */
    CreateMessageTemp();	/* Will bail out if it fails */
    debug(128, ("Reading standard input...\n"));
#if Logs
    Log(794, "GetMessageText: reading standard input into MailSourceFile");
#endif /* Logs */
    for (size=0;;) {
	char buffer[512];
	int n;

	n = read(0, buffer, sizeof buffer);
	if (n < 0) {
	    olderrno = errno;
	    CleanupMessageText();
	    sprintf(ErrorText, "Error reading stdin: %s", UnixError(olderrno));
	    debug(128, (NL, ErrorText));
	    FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : MapErrno(olderrno, EX_DATAERR));
	}

	if (n == 0) break;	/* Got EOF */

	size += n;
	if (writeall(MailSourceFD, buffer, n) < 0) {
	    olderrno = errno;
	    CleanupMessageText();
	    sprintf(ErrorText, "Error writing message file %s: %s", MailSourceFile, UnixError(olderrno));
	    debug(128, (NL, ErrorText));
	    if (RepeatError(ENOSPC, olderrno)) {
		FoldTent(EX_TEMPFAIL);
	    }
	    if (olderrno != ENOSPC && tfail(olderrno)) FoldTent(EX_TEMPFAIL);
	    FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : MapErrno(olderrno, EX_DATAERR));
	}
    }
#if Logs
    Log(795, "GetMessageText: all copied");
#endif /* Logs */

    /* Check message size */
    debug(128, ("Message is %d bytes, min is %d bytes\n", size, MIN_MESSAGE_SIZE));
    if (size < MIN_MESSAGE_SIZE) {
	CleanupMessageText();
	sprintf(ErrorText, "Message on standard input is too small: %d (min is %d)", size, MIN_MESSAGE_SIZE);
	FatalError(ErrorText, NIL, FALSE, CheckRepeatErrors ? EX_TEMPFAIL : EX_DATAERR);
    }

    debug(128, ("Message read into %s\n", MailSourceFile));
    return 0;
}

/* Try to write contents of message to f */

static int WriteMessage(f)
    register FILE *f;
{
    struct stat buf;
    register int bufsize;
    char buffer[8192];

    debug(8, ("Entering WriteMessage(0x%x), message file %s\n", f, MailSourceFile));

    /* Rewind it */
#if Logs
    Log(796, "WriteMessage: about to lseek to rewind");
#endif /* Logs */
    if (lseek(MailSourceFD, 0, 0) < 0) {
	debug(128, ("Error rewinding message file %s: %s\n",
	      MailSourceFile, UnixError(errno)));
	return -1;
    }

#if Logs
    Log(797, "WriteMessage: about to fstat MailSourceFD");
#endif /* Logs */
#if SY_B4x
    if (fstat(MailSourceFD, &buf) < 0) {
	debug(8, ("Can't fstat source file: %s\n", UnixError(errno)));
	return -1;
    }

#if Logs
    Log(798, "WriteMessage: fstat done, about to copy");
#endif /* Logs */
    bufsize = buf.st_blksize;
#else /* SY_B4x */
    bufsize = sizeof(buffer);
#endif /* SY_B4x */
    debug(8, ("Optimal block size is %d bytes\n", bufsize));
    if (bufsize > sizeof buffer) bufsize = sizeof buffer;
    debug(8, ("Buffer size will be %d bytes\n", bufsize));
    for (;;) {
	register int n;

	n = read(MailSourceFD, buffer, bufsize);
	if (n < 0) {
	    debug(8, ("Read failed: %s\n", UnixError(errno)));
	    return -1;
	}

	if (n == 0) {
#if Logs
	    Log(799, "WriteMessage: copy done, returning");
#endif /* Logs */
	    return 0;
	}

	if (fwriteallchars(buffer, n, f) <= 0) {
	    debug(8, ("fwrite failed: %s\n", UnixError(errno)));
	    return -1;
	}
    }
}

#define RECIPIENT_INCREMENT	100

/*
   Notice that this routine always guarantees that there is an
   extra unused slot at NextRecipient.  This slot may be used for
   null-terminating the list.
*/

static int AddRecipient(r, name)
    register struct RECIPIENTS *r;
    char *name;
{
    debug(65536, ("AddRecipient(<%d, 0x%x, %d>, %s)\n",
		  r->size, r->ToName, r->NextRecipient, name));

    /* If name is null, just return error */
    if (name == NULL) return -1;

    if (r->NextRecipient >= r->size-1) {
	/* Need more storage */

	debug(65536, ("Need to allocate more storage\n"));
	r->size += RECIPIENT_INCREMENT;
	if (r->NextRecipient >= r->size-1) r->size = (r->NextRecipient*2) + 2;
	if (r->ToName == NULL) {
	    debug(65536, ("Doing MALLOC for %d addresses\n", r->size));
	    r->ToName = (char **) malloc(r->size*sizeof(char *));
	} else {
	    debug(65536, ("Doing REALLOC for %d addresses\n", r->size));
	    r->ToName = (char **) realloc(r->ToName, r->size*sizeof(char *));
	}
	if (r->ToName == NULL) {
	    ErrorToPostmaster("MALLOC/REALLOC failed in AddRecipient", NIL, FALSE);
	    return -1;
	}
    }

    debug(65536, ("Succeeded, adding name\n"));
    r->ToName[r->NextRecipient++] = name;
    return 0;
}

/*
This is to stop the sending of hundreds of "No space left on device /tmp"
error messages when a postoffice machine's hard disk gets full.  If an
error of this type occurs, it checks for the time stamp on a zero-length
file "/tmp/LastError.nnn" , where nnn is the errno condition, and compares
it with the current time.  If the file is less than an hour old, it exits
without sending a message to the postman.  If it is more than an hour
old, a message goes out, and the time stamp on the file gets updated.  If
the file doesn't exist, it is created and an error message goes out.

    Adam Stoller  (ghoti)  -- fish  11/86
    J. Rosenberg	   -- modified 2/10/87 to be more general
*/

static Boolean RepeatError(error, errno)
    int error, errno;
{
    struct stat FileTime;
    int tmp_file;
    char file[MAXPATHLEN+1];

    if (error != errno || !CheckRepeatErrors) return FALSE;

    sprintf(file, "/tmp/LastError.%d", error);
    debug(8, ("Error %d--looking for file \"%s\"\n", error, file));
    osi_GetTimes(&TVal);
    if (stat(file, &FileTime) == 0) {
	debug(8, ("File time is %d, current is %d, diff is %d seconds\n",
	      FileTime.st_ctime, TVal.Secs,
	      TVal.Secs - FileTime.st_ctime));
	if ((TVal.Secs - FileTime.st_ctime) < 3600) {
	    debug(8, ("File isn't old\n"));
	    return TRUE;
	}
	debug(8, ("File exists, but is too old\n"));
    }

    /* File wasn't there at all or is too old, (re)create it */
    tmp_file = open(file, O_CREAT|O_WRONLY|O_TRUNC, 0644);
    debug(8, ("Tried to open %s: result %d\n", file, tmp_file));
    if (tmp_file >= 0) close(tmp_file);

    return FALSE;
}
