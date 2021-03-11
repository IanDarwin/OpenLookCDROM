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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/cui/RCS/andmchs.c,v 2.36 1994/03/29 03:32:35 rr2b Exp $";
#endif

/* Machine dependent module --  Andrew Version                     */
#include <andrewos.h>
#include <util.h>
#include <cui.h>
#include <stdio.h>
#include <errprntf.h>
#include <signal.h>
#include <pwd.h>
#include <cuimach.h>
#ifdef SNAP_ENV
#include <gasp.h>
#else /* SNAP_ENV */
#define GASP_PWD_STRING 1
#define GASP_PWD_VTOKENS 2
#define GASP_PWD_MULTI_TOKENS 4
#endif /* SNAP_ENV */
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <mailconf.h>
#include <mail.h>

#ifdef AFS_ENV
#include <afs/param.h>
#include <rx/xdr.h>
#include <afs/auth.h>
#include <tokens.h>
#else /* AFS_ENV */
#define TOKENS_LEN 100
#endif /* AFS_ENV */

extern int (*CUI_GenericClientSignalHandler)();
extern char *sys_errlist[];
extern int sys_nerr;
char **unix_sys_errlist = sys_errlist;
int unix_sys_nerr = 0; /* initialized below */

extern long CUI_LastCallFinished;
extern int CUI_SnapIsRunning;
extern char *getenv(), *getprofile();

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

int     SNAP_debuglevel=0,
        SNAP_socket=0,
        CUI_OnSameHost = FALSE;

static char HostNameBuf[256];
static char ReconnNameBuf[256];
static char UserNameBuf[200];
char EditorBuf[200] = "";

SetEditorToUse(ed)
char *ed;
{
    strncpy(EditorBuf, ed, sizeof(EditorBuf));
}


union alignme {
    char Text[TOKENS_LEN];
    long dummy;
} AlignBuf;

Machine_Init(ThisHost, ThisUser, ThisPassword, len, type, IsRecon)
char **ThisHost, **ThisUser, **ThisPassword;
int *len, *type, IsRecon;
{
    char ErrorText[600], *s;
    struct passwd *pw;

    if (fpacheck()) {
	ReportError("This machine has a bad floating point board.  If desperate, try 'rem'.", ERR_FATAL, FALSE);
	/* This is, I think, the only place left where the LIBRARY forces exit.
	    Running with a bad FPA board produces errors that are simply too
	    bizarre and varied and hard to track down for me to EVER tolerate
	    running with a bad FPA.  -- Nathaniel */
	exit(-1); /* not reached */
    }
    InitializeClientSignals();
    unix_sys_nerr = sys_nerr;
#ifdef SYS_NAME
    CUI_SetMachineType(SYS_NAME); /* from sys.h */
#endif /* SYS_NAME */
    GetHostDomainName(HostNameBuf, sizeof(HostNameBuf));
    CUI_SetMachineName(HostNameBuf);
    SetTerminalParams(24, 80);
    *ThisPassword = AlignBuf.Text; /* Do not pass in a null */
    AlignBuf.Text[0] = '\0';
    *ThisUser = NULL;
    if (!CUI_SnapIsRunning) {
	struct CellAuth *ca;
	int RC;

	CUI_OnSameHost = TRUE;
	*ThisHost = HostNameBuf;

	ca = NULL;
	RC = FindAMSHomeCell(&ca);
	if (ca == NULL) {
	    if (RC == 2) {
		strcpy(ErrorText, "You are not authenticated in your designated AMS home domain");
	    } else {
		sprintf(ErrorText, "Can't find AMS home cell: %s", AMSHome_errmsg);
	    }
	    ReportError(ErrorText, ERR_CRITICAL, FALSE);
	    AMS_RETURN_ERRCODE(EMSNOVUID, EIN_GETVUID, EVIA_MACHINEINIT);
	}
	if (RC == 1) {
	    sprintf(ErrorText, "Possible problem with AMS Home %s: %s", ca->CellName, AMSHome_errmsg);
	    ReportError(ErrorText, ERR_WARNING, FALSE);
	}
	errno = 0;
	FillInCell(ca);
	if (ca->UserName == NULL) {
	    AMS_RETURN_ERRCODE(errno, EIN_GETVPWUID, EVIA_MACHINEINIT);
	}
	*ThisUser = UserNameBuf;
	strcpy(*ThisUser, ca->UserName);
	return(0);
    }

    s = getprofile("AMS_RemoteUser");
    if(s && *s) {
	strcpy(UserNameBuf, s);
	*ThisUser = UserNameBuf;
    } else {
	pw = getpwuid(geteuid());
	if (pw && pw->pw_name) {
	    strcpy(UserNameBuf, pw->pw_name);
	    *ThisUser = UserNameBuf;
	}
    }
    HostNameBuf[0] = '\0';
    s = getprofile("AMS_RemoteLogin");
    if (s && *s) {
	strcpy(HostNameBuf, s);
    }
    if (HostNameBuf[0] == '\0') {
	s = GetConfiguration("AMS_RemoteLogin");
	if (s && *s) strcpy(HostNameBuf, s);
    }
    if (!AMS_ViceIsRunning) {
	if (HostNameBuf[0] == '\0') {
	    HostNameBuf[0] = '?';
	    HostNameBuf[1] = '\0';
	}
    }
    if (IsRecon)
	strcpy(HostNameBuf,ReconnNameBuf);

    if (HostNameBuf[0] != '\0') { /* *always* on non-vice, due to above check */
	if (HostNameBuf[0] == '?') {
	    if (!IsRecon) {
		GetStringFromUser("Name of host to run message server for you", HostNameBuf, sizeof HostNameBuf, FALSE);
		sprintf(ErrorText, "User id on host %s", HostNameBuf);
		GetStringFromUser(ErrorText, UserNameBuf, sizeof UserNameBuf, FALSE);
	    }
	    *ThisUser = UserNameBuf;
	}
	GetNewPassword(ThisPassword, IsRecon, *ThisUser, HostNameBuf);
	*len = 1+strlen(*ThisPassword);
	*type = GASP_PWD_STRING;
    } else {
#ifdef AFS_ENV
	if (AMS_ViceIsRunning) {
	    static char *SomeToks = NULL;
	    int someToksLen, someToksMax;

	    s = getprofile("AMS_RemoteServer");
	    if (s && *s) {
		strcpy(HostNameBuf, s);
	    }
	    if (HostNameBuf[0] == '\0') {
		s = GetConfiguration("AMS_RemoteServer");
		if (s && *s) strcpy(HostNameBuf, s);
	    }
	    if (HostNameBuf[0] == '\0') {
		CUI_OnSameHost  = TRUE;
		GetHostDomainName(HostNameBuf, sizeof HostNameBuf);
	    }
	    if (SomeToks != NULL) {free(SomeToks); SomeToks = NULL;}
	    someToksLen = someToksMax = 0;
	    if (AMS_getandpackALLtokens(&SomeToks, &someToksLen, &someToksMax, 0) <= 0) {
		AMS_RETURN_ERRCODE(EMSNOVUID, EIN_GETVPWUID, EVIA_MACHINEINIT);
	    }
	    *ThisPassword = SomeToks;
	    *len = someToksLen;
	    *type = GASP_PWD_MULTI_TOKENS;
	}
#endif /* AFS_ENV */
    }
    ReduceWhiteSpace(HostNameBuf);
    strcpy(ReconnNameBuf,HostNameBuf);
    *ThisHost = HostNameBuf;
    if (!*ThisUser) {
	struct CellAuth *ca;
	int RC;

	ca = NULL;
	RC = FindAMSHomeCell(&ca);
	if (ca == NULL) {
	    if (RC == 2) {
		strcpy(ErrorText, "You are not authenticated in your AMS Home domain");
	    } else {
		sprintf(ErrorText, "Can't find AMS home cell: %s", AMSHome_errmsg);
	    }
	    ReportError(ErrorText, ERR_CRITICAL, FALSE);
	    AMS_RETURN_ERRCODE(EMSNOVUID, EIN_GETVUID, EVIA_MACHINEINIT);
	}
	errno = 0;
	FillInCell(ca);
	if (ca->UserName == NULL) {
	    AMS_RETURN_ERRCODE(errno, EIN_GETVPWUID, EVIA_MACHINEINIT);
	}
	*ThisUser = UserNameBuf;
	strcpy(*ThisUser, ca->UserName);
    }
    return(0);
}

/* This routine generates a temporary file name to be written on the 
	local machine.  */

CUI_GenLocalTmpFileName(nmbuf)
char   *nmbuf;
{
    /* On Andrew/UNIX, we can use a normal /tmp file */

    return(CUI_GenTmpFileName(nmbuf));
}

EditLocalFile(LocalName, FinishedElsewhere)
char   *LocalName;
Boolean    *FinishedElsewhere;
{
    int     pid,
            pid2 = 0,
            status;
    char    ErrorText[256];
    char   *EditVec[5];

    *FinishedElsewhere = FALSE; /* BOGUS -- really obsolete */
    if (EditorBuf[0]) {
	EditVec[0] = EditorBuf;
    } else {
	EditVec[0] = getenv("EDITOR");
    }
    EditVec[1] = LocalName;
    EditVec[2] = NIL;
    EditVec[3] = NIL;

    if (!strcmp(getenv("TERM"), "wm")) {
	ReportSuccess("Warning: If your editor starts a new window, it might not communicate properly with this program.");
    }

    if (pid = osi_vfork()) {
	pid2 = 0;
	while (pid != pid2 && pid2 >= 0) {
	    pid2 = wait(&status);
	}
    }
    else {
	execv(EditVec[0], EditVec);
	_exit(-1);
    }
    if ((pid2 < 0) || status) {
	sprintf(ErrorText, "Error editing local file '%s' with editor '%s'", LocalName, EditVec[0]);
	ReportError(ErrorText, ERR_WARNING, FALSE);
	return(-1);
    }
    return(0);
}

/* The following code gathers statistics about message server calls.
    It need not be ported, but certainly could be.
 */

static  FILE * MSLogFP = NULL;
static long laststart = 0,
            lastfinish = 0,
            inittime = 0;

InitializeLogging() {
    if (getprofileswitch("cuimslog", 0)) {
	MSLogFP = fopen("/tmp/cuims.log", "w");
	if (MSLogFP) {
	    fprintf(MSLogFP, "idle\tstart\tdone\telapsed\tFunction\n");
	}
    }
}


LogStart() {
    if (!inittime) {
	inittime = time(0);
    }
    if (MSLogFP) {
	laststart = time(0) - inittime;
	fprintf(MSLogFP, "%d\t%d\t", laststart - lastfinish, laststart);
    }
}

LogEnd(name) 
char *name;
{
    if (MSLogFP) {
	lastfinish = time(0) - inittime;
	fprintf(MSLogFP, "%d\t%d\t%s\n", lastfinish, lastfinish - laststart, name);
	fflush(MSLogFP);
    }
}

RedirectOutput() {
    int cfd = 0;

    /* Redirect stdout & stderr */
    if (!AMS_DevConsoleIsSacred) {
	cfd = open("/dev/console", O_WRONLY, 0644);
	if (cfd>=0) { /* If I could not open /dev/console, I just hope for the best */
	    dup2(cfd, 1);
	    dup2(cfd, 2);
#ifdef hpux
	    setvbuf(fdopen(1, "w"), NULL, _IOLBF, BUFSIZ);
	    setvbuf(fdopen(2, "w"), NULL, _IOLBF, BUFSIZ);
#else /* hpux */
	    setlinebuf(fdopen(1, "w"));
	    setlinebuf(fdopen(2, "w"));
#endif /* hpux */
	    close(cfd);
	} else {
	    printf("Warning: cui cannot open /dev/console");
	    fprintf(stderr, "Warning: cui cannot open /dev/console");
	    fflush(stdout);
	    fflush(stderr);
	}
    }
}

static char PasswordBuf[100];

GetNewPassword(ptr, IsRecon, ThisUser, ThisHost)
char **ptr, *ThisUser, *ThisHost;
int IsRecon;
{
    char ErrorText[256];

    if (!IsRecon || !*ptr) {
	sprintf(ErrorText, "Password for user %s on host %s", ThisUser , ThisHost);
	GetStringFromUser(ErrorText, PasswordBuf, sizeof(PasswordBuf), 1);
    }
    *ptr = PasswordBuf;
}

#define ALARMPERIOD 180		/* 3 minutes */

AlarmSignalHandler() {
    if (CUI_LastCallFinished && (time(0) - CUI_LastCallFinished > ALARMPERIOD)) {
	if (mserrcode = MS_FastUpdateState()) {
	    ReportError("Could not update message server state", ERR_WARNING, TRUE);
	}
    }
    alarm(ALARMPERIOD);
}

CUI_InitializeKeepalives() {
    if (CUI_SnapIsRunning) {
#ifdef POSIX_ENV
	struct sigaction alrm;
	alrm.sa_handler=AlarmSignalHandler;
	sigemptyset(&alrm.sa_mask);
	sigaction(SIGALRM, &alrm, NULL);
#else
	signal(SIGALRM, AlarmSignalHandler);
#endif
	alarm(ALARMPERIOD);
#ifndef CUI_KEEPALIVE_WITHOUT_SNAP
    } else {
	fprintf(stderr, "Warning:  This is a no-snap version of the system with NO message server checkpointing.  That is probably a mistake.\n");
#endif /* CUI_KEEPALIVE_WITHOUT_SNAP */
    }
}

Machine_HandleClientSignal(signum, ActNormal)
int signum;
int *ActNormal;
{
    *ActNormal = 1;
    if (CUI_GenericClientSignalHandler) (*CUI_GenericClientSignalHandler)(signum, ActNormal);
}

    
SnapifiedClientSignalHandler(signum) 
int signum;
{
    int ActNormal = 1;
    char *Text;

    Machine_HandleClientSignal(signum, &ActNormal);
    if (!ActNormal) return;
    switch(signum) {
	case SIGHUP:
	    Text = "Hangup";
/*            kill(getpid(),SIGKILL); */
	    break;
	case SIGINT:
	    Text = "Interrupt";
	    break;
	case SIGQUIT:
	    Text = "Quit";
	    break;
#ifdef SIGBUS
	case SIGBUS:
	    Text = "Bus Error";
	    break;
#endif
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
	    printf("Continuing...\n");
	    signal(SIGTSTP, SnapifiedClientSignalHandler);
	    return;
#endif /* SIGCONT */
#ifdef SIGXFSZ
	case SIGXFSZ:
	    Text = "XFSZ";
	    break;
#endif /* SIGXFSZ */
#ifdef SIGXCPU
	case SIGXCPU:
	    Text = "XCPU";
	    break;
#endif /* SIGXCPU */
	default:
	    Text = "Unknown";
	    break;
    }
    errprintf("ams", ERR_WARNING, 0, 0, "%s signal received.", Text);
    signal(signum, SIG_DFL);
    kill(getpid(), signum);
}

InitializeClientSignals() {
    if (CUI_SnapIsRunning) {
#ifdef POSIX_ENV
	struct sigaction oldV;
	oldV.sa_handler = SIG_IGN;
	sigaction(SIGHUP, 0, &oldV);	/* CFE read the old value.  If it's not ignored, override the handler. */
	if (oldV.sa_handler != SIG_IGN) {
		signal(SIGHUP, SnapifiedClientSignalHandler);
	}
#else
	struct sigvec oldV;
	oldV.sv_handler = SIG_IGN;
	sigvec(SIGHUP, 0, &oldV);	/* CFE read the old value.  If it's not ignored, override the handler. */
	if (oldV.sv_handler != SIG_IGN) {
		signal(SIGHUP, SnapifiedClientSignalHandler);
	}
#endif
/*	signal(SIGHUP, SnapifiedClientSignalHandler); CFE 10/7/88 */

	signal(SIGINT, SnapifiedClientSignalHandler);
	signal(SIGQUIT, SnapifiedClientSignalHandler);
#ifdef SIGBUS
	signal(SIGBUS, SnapifiedClientSignalHandler);
#endif
	signal(SIGSEGV, SnapifiedClientSignalHandler);
	signal(SIGTERM, SnapifiedClientSignalHandler);
#ifdef SIGTSTP
	signal(SIGTSTP, SnapifiedClientSignalHandler);
#endif /* SIGTSTP */
#ifdef SIGCONT
	signal(SIGCONT, SnapifiedClientSignalHandler);
#endif /* SIGCONT */
#ifdef SIGXFSZ
	signal(SIGXFSZ, SnapifiedClientSignalHandler);
#endif /* SIGXFSZ */
    }
}

