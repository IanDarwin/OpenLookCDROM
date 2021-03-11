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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/rdemo/ms/RCS/init.c,v 1.3 1992/12/15 22:00:08 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <andyenv.h>
#include <ms.h>
#include <stdio.h>
#include <util.h>
#include <hdrparse.h>
#include <pwd.h>
#include <ctype.h>
#include <mailconf.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <dropoff.h>
#include <mail.h>
#include <errprntf.h>
#ifdef hpux
#include <signal.h>
#endif /* hpux */

extern char **environ;
extern char *getprofile(), *index(), *rindex(), *StripWhiteEnds();

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern FILE *fopen();

extern char *getenv();

char *StandardHeaderNames[] =  {
    "reply-to", /* HP_REPLY_TO */
    "from", /* HP_FROM */
    "sender", /* HP_SENDER */
    "to", /* HP_TO */
    "cc", /* HP_CC */
    "ack", /* HP_ACK */
    "ack-to", /* HP_ACKTO */
    "ack-type", /* HP_ACKTYPE */
    "date", /* HP_DATE */
    "subject", /* HP_SUBJECT */
    "enclosure", /* HP_ENCLOSURE */
    "newsgroup", /* HP_NEWSGROUP */
    "newsgroups", /* HP_NEWSGROUPS */
    "x-andrew-widereply", /* HP_WIDEREPLY */
    "attention", /* HP_ATTENTION */
    "x-andrew-directorycreation", /* HP_DIRECTORYCREATION */
    "return-path", /* HP_RETURNPATH */
    "x-andrew-scribeformat", /* HP_SCRIBEFORMAT */
    "resent-from", /* HP_RESENTFROM */
    "resent-to", /* HP_RESENTTO */
    "x-andrew-allreply", /* HP_ALLREPLY */
    "x-andrew-authenticated-as", /* HP_AUTHENTICATED_AS */
    "received", /* HP_RECEIVED */
    "message-id", /* HP_MESSAGEID */
    "x-andrew-vote", /* HP_OLDVOTE */
    "vote-request", /* HP_VOTEREQUEST */
    "vote-to", /* HP_VOTETO */
    "vote-choices", /* HP_VOTECHOICES */
    "content-type", /* HP_CONTENTTYPE */
    "if-type-unsupported", /* HP_UNSUPPORTEDTYPE */
    "bcc", /* HP_BCC */
    "in-reply-to", /* HP_INREPLYTO */
    "references", /* HP_REFERENCES */
    "distribution", /* HP_DISTRIBUTION */
    "x-andrew-message-size", /* HP_MESSAGESIZE */
    "resent-date", /* HP_RESENTDATE */
    "resent-message-id", /* HP_RESENTMESSAGEID */
    "x-andrew-redistribution-to", /* HP_REDISTRIBUTION */
    NULL, /* HP_END_PREDEFINED */
};

extern char *SnapVersionString;

#define MS_VERSTRINGSIZE 50
char MS_VersionString[MS_VERSTRINGSIZE] = "";

#define BIGGESTBUFFER 20000

int myvuid, postmanvuid, DelayPrinting, AlwaysPrintImmediately;
char home[MAXPATHLEN] = "";
char Me[100] = "";
char MeInFull[250] = "", *MyPrettyAddress = NULL, *MyShortAddress = NULL;
char MyMailDomain[125] = "";
int homeUsesAMSDelivery = 0;	/* Whether my home auth uses AMS delivery */
int homeUsesUseridPlus = 0;
int MS_DeliveryType = -1;	/* -1 not yet set, 0 OK, 1 can't send any mail, ... (DT_xxx from dropoff.h) */
static int MS_NameSep = 0;	/* State from initialize call, used in GetConfigurationParameters */
char *BCCto = NULL, *DefaultPrinter = NULL;
int SwapPerformanceTimeout, SwapPerformanceBackoff,BCCFlag, IsLoggingMailStats;
int MS_SnapIsRunning;
char MAILLOGSTATFILE[60] = "";
#define ENVIRONMENTSIZE 50
char *MyEnvironment[ENVIRONMENTSIZE] = {NULL};
char *PersonalMailCollectionCommand = NULL;
/* The following routine is used to allocate memory that will never be freed.  It is
  used instead of the standard malloc to help avoid memory fragmentation, which
  grew to be a real problem with the permanent directory cache (which is small 
 but comes in lots of small pieces) in the messageserver process running the bboard daemon. */

static char *CurArena=NULL;
static int LeftInCurArena=0, WastedByPermanentMalloc=0;
#define EVENBREAK sizeof(char **)

char *
permanentmalloc(ct)
int ct;
{
    int AmtToGrab;
    char *retstr;

    if (ct <= 1) ct = 1;
    if (((ct/EVENBREAK)*EVENBREAK) < ct) ct = (((ct/EVENBREAK)+1)*EVENBREAK);
    if (!CurArena || (ct > LeftInCurArena)) {
	WastedByPermanentMalloc += LeftInCurArena;
	AmtToGrab = 20000;
	if (AmtToGrab < ct) AmtToGrab = ct+20000;
	CurArena = malloc(AmtToGrab);
	if (!CurArena) {
	    LeftInCurArena=0;
	    return(NULL); /* out of memory */
	}
	LeftInCurArena = AmtToGrab;
    }
    retstr = CurArena;
    CurArena += ct;
    LeftInCurArena -= ct;
    return(retstr);
}

ReportPermanentMallocWaste() {
    return(WastedByPermanentMalloc);
}

/* The following routine gets called in two ways:  From MS_Initialize,
    when the program starts, and explicitly by clients each time they 
    connect.  This is fine.  However, it is inefficient for the first
    client's connection, because it will be called in both ways for him.
    This is especially painful since it calls InitializeSearchPaths, which
    can spend a lot of time mucking about in /etc/passwd.  Therefore the
    hacks in the first line that attempt to avoid doing it twice in
    such circumstances. */

MS_ReInitialize() {
    char FileNameBuf[1+MAXPATHLEN], *s;
    int mycode;

    debug(1, ("In MS_Reinitialize\n"));
    refreshprofile();
    s = getprofile("PersonalMailCollectionCommand");
    if (s) {
	if (PersonalMailCollectionCommand) free(PersonalMailCollectionCommand);
	PersonalMailCollectionCommand = malloc(1+strlen(s));
	if (!PersonalMailCollectionCommand) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	}
	strcpy(PersonalMailCollectionCommand, s);
    }
    s = getprofile("print.printer");
    if (!s) s = getprofile("print.spooldir");
    if (s) {
	if (DefaultPrinter) free(DefaultPrinter);
	DefaultPrinter = malloc(1+strlen(s));
	if (!DefaultPrinter) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	}
	strcpy(DefaultPrinter, s);
    }
    DelayPrinting = getprofileswitch("DelayPrinting", 0);
    AlwaysPrintImmediately = getprofileswitch("AlwaysPrintImmediately", 0);
    IsLoggingMailStats = getprofileswitch("LogMailHeaders", 0);
    if (IsLoggingMailStats) {
	sprintf(MAILLOGSTATFILE, "/usr/spool/stats/MS_stats.%d.%d", getuid(), getpid());
    }
    SwapPerformanceTimeout = getprofileint("swapperformancetimeout", 0);
    SwapPerformanceBackoff = getprofileint("swapperformancebackoff", 0);
    MS_SetDeathKnell(getprofileint("deathknell",AMS_InitialDeathKnell));
    BCCFlag = getprofileswitch("bcc", 0);
    mycode = InitializeSearchPaths();
    RefreshSubs();
    if (BCCto) free(BCCto);
    BCCto = getprofile("bccto");
    if (!BCCto || !*BCCto) {
	BCCto = NULL;
    } else {
	if (*BCCto == '/') {
	    strcpy(FileNameBuf, BCCto); /* Still want to malloc it */
	} else {
	    for (s=BCCto; *s; ++s) {
		if (*s == '.') *s = '/';
	    }
	    mserrcode = MS_DisambiguateFile(BCCto, FileNameBuf, AMS_DISAMB_EXISTS);
	    if (mserrcode) {
		if (AMS_ERRNO == EMSUNAUTH) {
		    BCCto = NULL;
		    return(mserrcode);
		}
		sprintf(FileNameBuf, "BCCto preference %s not on search path; blind copies will go through the mail", BCCto);
		NonfatalBizarreError(FileNameBuf);
		BCCto = NULL;
	    }
	}
	if (BCCto) {
	    BCCto = malloc(strlen(FileNameBuf) + 1);
	    if (!BCCto) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	    }
	    strcpy(BCCto, FileNameBuf);
	}
    }
    return(mycode);
}


MS_Initialize(MaxBufSize, UsingSnap) 
int *MaxBufSize;
Boolean UsingSnap;
{
#ifdef AFS_ENV
    struct passwd *p;
#endif /* AFS_ENV */
    char NameBuf[2000], *newname, *S;
    struct CellAuth *ca; int RC;
    int UseNameSepForMe = 0;
#ifdef WHITEPAGES_ENV
    wp_ErrorCode wpErr;
    wp_PrimeKey KVal = 0;
    struct wp_cd *wpCD = 0;
#endif /* WHITEPAGES_ENV */

    debug(1, ("Initializing MS\n"));
    
    MS_SnapIsRunning = UsingSnap;
    errno = 0;
    RC = CheckAMSConfiguration();
    if (RC) {
	if (errno == 0) errno = ENOMEM;
	AMS_RETURN_ERRCODE(errno, EIN_GETCELLFROMWS, EVIA_MSCUIINIT);
    }
    errno = 0;
    ca = NULL;
    RC = FindAMSHomeCell(&ca);
    if (RC != 0 && ca != NULL) {
	sprintf(NameBuf, "Possible problem with AMS home %s: %s", ca->CellName, AMSHome_errmsg);
	NonfatalBizarreError(NameBuf);
    }
    if (ca == NULL) {
	sprintf(NameBuf, "Your authentication is unusable (%d): %s", RC, AMSHome_errmsg);
	FatalError(NameBuf);
	safeexit(-1);
    }
    if (ca->ViceID < 0) FillInCell(ca);
    myvuid = ca->ViceID;
    strncpy(MyMailDomain, ca->CellName, sizeof(MyMailDomain));
    if (ULstrcmp(MyMailDomain, ThisDomain) != 0) {	/* Need to configure cell-specific things out of the proper cell's config file. */
	errno = 0;
	RC = CkAMSCellConfig(MyMailDomain);
	if (RC < 0) AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	if (RC > 0) {
	    if (errno == ENOENT) sprintf(NameBuf, "AMSHome domain %s doesn't have a server config file; can't configure for it!", MyMailDomain);
	    else sprintf(NameBuf, "Can't read server config file for AMSHome domain %s: %s; can't configure to use that domain!", MyMailDomain, UnixError(errno));
	    FatalError(NameBuf);
	    safeexit(-1);
	}
    }
    if (ca->UsesAMSDelivery == 0) {
	ca->UsesAMSDelivery = CheckAMSDelivery(MyMailDomain);
    }
    homeUsesAMSDelivery = ca->UsesAMSDelivery;	/* -1 for no, 0 for unk, >0 for yes. */
    homeUsesUseridPlus = (homeUsesAMSDelivery > 0 ? 1 : CheckAMSUseridPlusWorks(MyMailDomain));
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	S = CheckAMSPMName(MyMailDomain);
	p = getcpwnam(S ? S : PostmasterName, MyMailDomain);
	if (p == NULL) {
	    if (homeUsesAMSDelivery >= 0) {
		sprintf(NameBuf, "Can't find Postmaster name (%s) for %s: %s; not trusting authentication for mail delivered by the postmaster.",
			S ? S : PostmasterName, MyMailDomain, wp_ErrorString(cpw_error));
		    NonfatalBizarreError(NameBuf);
	    }
	    postmanvuid = -2;		/* unlikely value */
	} else {
	    postmanvuid = p->pw_uid;
	}
    } else
#endif /* AFS_ENV */
    {
	postmanvuid = -2;
    }
    FillInCell(ca);
    if (ca->WpError != 0) {
#ifdef WHITEPAGES_ENV
	sprintf(NameBuf, "FillInCell failed for domain %s: %s.  I don't know who I am!",
		ca->CellName, wp_ErrorString(ca->WpError));
#else /* WHITEPAGES_ENV */
	sprintf(NameBuf, "FillInCell failed for domain %s: %d.  I don't know who I am!",
		ca->CellName, ca->WpError);
#endif /* WHITEPAGES_ENV */
	FatalError(NameBuf);
	safeexit(-1);
    }
    (void) DeSymLink(ca->homeDir, home, 0);
    if (abspath(home, NameBuf) == 0) strcpy(home, NameBuf);

    /* For rdemo:  Ignore all this home stuff, and just trust $HOME */
    strcpy(home, getenv("HOME"));

    strcpy(Me, ca->UserName);
    GetNameFromGecos(ca->PersonName, ca->UserName, ca->CellName, &newname);
    if (!newname) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
    }
    strcpy(MeInFull, newname);
    free(newname);
    MS_NameSep = CheckAMSNameSep(MyMailDomain);
    if (MS_NameSep > 0) {	/* Make MyShortAddress first */
	strcpy(NameBuf, MeInFull);
	for (S = NameBuf; *S != '\0'; ++S) if (*S == ' ') *S = MS_NameSep;
	newname = Quote822LPart(NameBuf);
	if (!newname) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	}
	strcpy(NameBuf, newname);
	free(newname);
#ifdef WHITEPAGES_ENV
	/* See if NameBuf unambiguously matches Me */
	wpErr = wp_InitializeCell(ca->CellName, &wpCD);
	if (wpErr == wperr_NoError) {
	    wpErr = cwp_GetUIDOnly(wpCD, Me, &KVal);
	}
	if (wpErr == wperr_NoError) {
	    wpErr = AddressMatchesUnambiguously(wpCD, NameBuf, KVal);
	}
	if (wpErr == wperr_NoError) {
	    UseNameSepForMe = 1;
	}
	if (KVal) free(KVal);
	if (wpCD) cwp_Terminate(wpCD);
#else /* WHITEPAGES_ENV */
	UseNameSepForMe = 1;
#endif /* WHITEPAGES_ENV */
    }
    if (UseNameSepForMe) {
	if (!AMS_NoDomainPreferredOnLocalMail) {
	    strcat(NameBuf, "@");
	    strcat(NameBuf, MyMailDomain);
	}
    }
    else {
	strcpy(NameBuf, Me);
	if (homeUsesUseridPlus > 0) strcat(NameBuf, "+");
	if (!AMS_NoDomainPreferredOnLocalMail) {
	    strcat(NameBuf, "@");
	    strcat(NameBuf, MyMailDomain);
	}
    }
    MyShortAddress = permanentmalloc(1+strlen(NameBuf));
    if (!MyShortAddress) {
	MyShortAddress = Me; /* Better than nothing... */
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
    }
    strcpy(MyShortAddress, NameBuf);
    if (UseNameSepForMe) {	/* Now make MyPrettyAddress */
	MyPrettyAddress = MyShortAddress;	/* in this case, they're the same. */
    } else {
	newname = Quote822Phrase(MeInFull);
	if (!newname) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	}
	strcpy(NameBuf, newname);
	strcat(NameBuf, " <");
	strcat(NameBuf, MyShortAddress);
	strcat(NameBuf, ">");
	MyPrettyAddress = permanentmalloc(1+strlen(NameBuf));
	if (!MyPrettyAddress) {
		MyPrettyAddress = Me; /* Better than nothing... */
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MSCUIINIT);
	}
	strcpy(MyPrettyAddress, NameBuf);
    }
    /* Now we have to set the MS_DeliveryType cell to 1 if dropoff will be preventing delivery.  The codes are defined in full in dropoff.h as the DT_xxx codes. */
    MS_DeliveryType = test_dropoff();
    if (UsingSnap) {
	int i, offset;
	char *adir, *ldir;
	static char USUALPATH[] = "PATH=%s/bin:%s/bin:%s/bin:/usr/ucb:/usr/bin:/bin";

	adir = (char *)AndrewDir(NULL);
	if (!adir) adir = "/usr/andrew";
	ldir = (char *)LocalDir(NULL);
	if (!ldir) ldir = "/usr/local";
	MyEnvironment[0] = permanentmalloc(8+strlen(home));
	sprintf(MyEnvironment[0], "HOME=%s", home);
	MyEnvironment[1] = permanentmalloc(sizeof(USUALPATH)+strlen(home) + strlen(adir) + strlen(ldir) + 4);
	sprintf(MyEnvironment[1], USUALPATH, home, adir, ldir);
	MyEnvironment[2] = permanentmalloc(12+strlen(adir));
	sprintf(MyEnvironment[2], "ANDREWDIR=%s", adir);
	for(i = offset = 3; environ[i-offset] && i < ENVIRONMENTSIZE; ++i) {
	    if (!strncmp(environ[i-offset], "HOME=", 5) || !strncmp(environ[i-offset], "PATH=", 5) || !strncmp(environ[i-offset], "ANDREWDIR=", 10) ) {
		/* disagreeing duplicate env settings confuse /bin/sh to death */ 
		MyEnvironment[i] = "BOGUS=BOGUS";
	    } else {
		MyEnvironment[i] = environ[i-offset];
	    }
	}
	MyEnvironment[i] = NULL;
	environ = MyEnvironment;
	for (i=0; environ[i] && i<ENVIRONMENTSIZE; ++i) {
	    debug(1, ("Environment param %d: %s\n", i, environ[i]));
	}
    }

#ifdef USE_MMDF_ENV
    mmdf_init("messageserver");
    mm_init();
#endif

    *MaxBufSize = BIGGESTBUFFER;
    sprintf(MS_VersionString, "MS %d.%d (AMS %d.%d, SNAP %s)", 
	MS_MAJORVERSION, MS_MINORVERSION, AMS_MAJOR_VERSION, AMS_MINOR_VERSION,
	SnapVersionString);
    debug(4, ("MS version %s\n", MS_VersionString));


    InitializeDirCacheState();
    InitializeDeathSignals();
    return(MS_ReInitialize());
}

DieYouHeathenSwine(signum) 
int signum;
{
    FILE *fp;
    char *Text, ErrorText[25+MAXPATHLEN], Fname[1+MAXPATHLEN];
    static int plumbcount = 0;
    int ActNormal = 1, errct = 0;

    VitalCheckpoints(TRUE, &errct);
    Machine_HandleClientSignal(signum, &ActNormal);
    if (!ActNormal) return;
    switch(signum) {
	case SIGHUP:
	    Text = "Hangup";
	    break;
	case SIGINT:
	    Text = "Interrupt";
	    break;
	case SIGQUIT:
	    Text = "Quit";
	    break;
	case SIGBUS:
	    Text = "Bus Error";
	    break;
	case SIGSEGV:
	    Text = "Segmentation Violation";
	    break;
	case SIGTERM:
	    Text = "Terminate";
	    break;
#ifdef SIGTSTP
	case SIGTSTP:
	    Text = "Stop";
	    break;
#endif /* SIGTSTP */
#ifdef SIGCONT
	case SIGCONT:
	    NonfatalBizarreError("Continuing...");
	    signal(SIGTSTP, DieYouHeathenSwine);
	    return;
#endif /* SIGCONT */
#ifdef SIGXCPU
	case SIGXCPU:
	    if (MSDebugging) {
		MSDebugging = (MSDebugging == 1) ? -1 : 0;
	    } else {
		MSDebugging = 1;
	    }
	    /* Drop through */
#endif /* SIGXCPU */
#ifdef SIGXFSZ
	case SIGXFSZ:
	    /* Code for using plumber to dig out core leaks */
	    sprintf(Fname, "/tmp/ms_plumber_stats.%d.%d", getpid(), plumbcount++);
	    fp  = fopen(Fname, "w");
	    if (fp) {
		sprintf(ErrorText, "Writing malloc table in %s. (Permanent waste: %d)", Fname, ReportPermanentMallocWaste());
		NonfatalBizarreError(ErrorText);
		fprintf(fp, "Permanent-malloc waste for process %d: %d\n\n", getpid(), ReportPermanentMallocWaste());
#ifdef DEBUG_MALLOC_ENV
		fprintf(fp, "Messageserver malloc table:\n\n");
		plumber(fp);
		fprintf(fp, "\n\n");
#else /* #ifdef DEBUG_MALLOC_ENV */
		fprintf(fp, "(Malloc table not available without DEBUG_MALLOC_ENV.)\n\n");
#endif /* #ifdef DEBUG_MALLOC_ENV */
#ifdef PLUMBFDLEAKS
		fprintf(fp, "Messageserver fdplumb table:\n\n");
		fdplumb_SpillGutsToFile(fp, 0);
		fprintf(fp, "\n\n");
#else /* #ifdef PLUMBFDLEAKS */
		fprintf(fp, "(Messageserver fdplumb table not available without FD plumber.)\n\n");
#endif /* #ifdef PLUMBFDLEAKS */
	    } else {
		fprintf(stderr, "cannot open /tmp/plumber_stats; sorry. (Permanent waste: %d)", ReportPermanentMallocWaste());
	    }
	    fclose(fp);
	    return;
#endif /* SIGXFSZ */
	default:
	    Text = "Unknown";
	    break;
    }
    /* NEED TO PRINT OUT MAIL IN PROGRESS COLLECTED HERE */
    if (myvuid == postmanvuid && postmanvuid > 0) {
	extern int MS_DataCollectionHackInProgress, MS_UnreportedCollections;
	extern char *MS_UnreportedMailbox;

	if (MS_DataCollectionHackInProgress && MS_UnreportedCollections) {
	    /* Really BOGUS -- Pretend we're CUI if the postman ever gets a dump signal during mailbox processing, just to keep Adam's stat daemons accurate. */
	    errprintf("cui", ERR_WARNING, 0, 0, "%d message%s read in from %s.", MS_UnreportedCollections, MS_UnreportedCollections > 1 ? "s were" : " was", MS_UnreportedMailbox);
	}
    }
    errprintf("ms", ERR_WARNING, 0, 0, "%s signal caught; checkpointed server state...", Text);
    signal(signum, SIG_DFL);
    kill(getpid(), signum);
}

InitializeDeathSignals() {
    struct sigvec oldV;
    /* Originally, I handled sighups.  As of 7/22/87, mas/jr changed it.  Now, I change it back.  -- nsb, 9/30/88.  CFE removed it 10/7/88 so that CUI will work even when /etc/rc processing sends CUI a signal. */
/*    signal(SIGHUP, DieYouHeathenSwine); */
/*    signal(SIGHUP, SIG_IGN); */
    oldV.sv_handler = SIG_IGN;
    sigvec(SIGHUP, 0, &oldV);  /* CFE read the old value.  If it's not ignored, override the handler. */
    if (oldV.sv_handler != SIG_IGN) {
	signal(SIGHUP, DieYouHeathenSwine);
    }

    signal(SIGINT, DieYouHeathenSwine);
    signal(SIGQUIT, DieYouHeathenSwine);
    signal(SIGBUS, DieYouHeathenSwine);
    signal(SIGSEGV, DieYouHeathenSwine);
    signal(SIGTERM, DieYouHeathenSwine);
#ifdef SIGTSTP
    signal(SIGTSTP, DieYouHeathenSwine);
#endif /* SIGTSTP */
#ifdef SIGCONT
    signal(SIGCONT, DieYouHeathenSwine);
#endif /* SIGCONT */
#ifdef SIGXFSZ
    signal(SIGXFSZ, DieYouHeathenSwine);
#endif /* SIGXFSZ */
#ifdef SIGXCPU
    signal(SIGXCPU, DieYouHeathenSwine);
#endif /* SIGXCPU */
}

MS_GetConfigurationParameters(MailDomain, len, UseAmsDelivery, UseNameSep, DelType)
int *UseAmsDelivery, *UseNameSep, len, *DelType;
char *MailDomain;
{
    strncpy(MailDomain, MyMailDomain, len);
    *UseAmsDelivery = homeUsesAMSDelivery;
    *UseNameSep = MS_NameSep;
    *DelType = MS_DeliveryType;
    return(0);
}

MS_AndrewDir(Dir, len)
char *Dir;
int len;
{
    char *adir;

    adir = (char *)AndrewDir(NULL);
    if (!adir) adir = "/usr/andrew";
    strncpy(Dir, adir, len);
    return(0);
}
