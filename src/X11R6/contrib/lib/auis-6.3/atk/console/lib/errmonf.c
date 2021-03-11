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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/errmonf.c,v 2.20 1993/06/24 17:10:49 gk5g Exp $";
#endif


 

/* 
 ***************************************************************
 * Routines for monitoring messages on console service
 * and (on some machines) /dev/console.
 * Some routines swiped from ttyscript.
 ***************************************************************
 */

#include <class.h>
#include <im.ih>
#include <conclass.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <console.h>
#include <ctype.h>
#include <andrewos.h> /* sys/types.h sys/time.h sys/file.h */
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef hpux
#include <arpa/inet.h>
#endif /* hpux */
#include <netdb.h>
/* #include <sys/tty.h> */
#include <sitevars.h>
#include <errno.h>

extern struct classinfo *consoleClass_GetInfoHack();
extern FILE *qopen();
extern int ConsoleSocket;
extern struct sockaddr_in ConsoleAddr;
extern int SubChannel;
extern char ptyname[30];
extern char ThisHostName[256];
extern boolean NonAFSDHost;
extern u_long ThisHostAddr;

#define ERRBUFSIZE 300
char PrimaryErrorBuffer[ERRBUFSIZE];

/* ****************************************************************
 * CheckErrorsIn -- checks the /dev/console channel, if it exists.
 * DESIGN:  When CheckErrorsIn detects a fatal error on the ErrorsIn
 * channel,it increments a counter and tries to open it again.  If this
 * happens more than MAXERRORRESTARTS times in one running of console, it
 * then gives up and resets ErrorsIn to be the same as winin (after
 * printing an error msg).  This should prevent this routine from ever
 * waking up again, which is good if the input is unreadable, but it has
 * the unfortunate effect of turning off all /dev/console error receiving
 * mechanisms on some machines.
 *****************************************************************
 */
/* Right now, zombie processes get left around on the romp, and they
 * tie up ptys,so it wouldn't be wise to restart too many times.
 */

#define DEVCONSOLEBUFSIZE 160
CheckErrorsIn(ErrorsIn, self)
    FILE *ErrorsIn;
    struct consoleClass *self;
{
    static char buf[DEVCONSOLEBUFSIZE];
    static char *fillptr = buf;
    register int c;

    mydbg(("entering: CheckErrorsIn\n"));
    errno = 0;

    while ((c = getc(ErrorsIn)) != EOF)  {
	if (c == '\r')
	    continue;
	if (c != '\n')  {
	    *fillptr++ = c;
	}
	if (c == '\n' || fillptr >= buf + sizeof(buf) - 2)  {
	    *fillptr++ = '\n';
	    *fillptr = '\0';
	    AnotherError(self, buf, FALSE);
	    fillptr = buf;
	}
    }
	
    if (errno != EWOULDBLOCK && errno != EAGAIN)  {
	if (errno != 0)  {
	    RestartErrorMonitoring(sys_errlist[errno]);
	}
#if !SY_AIX221 && !defined(M_UNIX) && !defined(__hpux)
	else  {
	    RestartErrorMonitoring("Odd EOF on error channel");
	}
#endif
    }

    return;
}


#define MESSAGESIZE 1500
boolean WasUDPAction = FALSE;
CheckConsoleSocket(ConsoleIn, self)
    FILE *ConsoleIn;
    struct consoleClass *self;
{
    int j, len;
    static char hostname[80];
    static char Message[MESSAGESIZE+100] = "";
    struct hostent *hp;
    static u_long lastHostAddr = 0;
    u_long newHostAddr;

    mydbg(("entering: CheckConsoleSocket\n"));
    WasUDPAction = TRUE;
    len = sizeof(ConsoleAddr);

    while ((j = recvfrom(ConsoleSocket, Message, MESSAGESIZE, 0, &ConsoleAddr, &len)) >= 0)  {
	Message[j] = '\0';
	for (; j >= 0 && (Message[j] == '\n' || Message[j] == ' ' || Message[j] == '\t' || Message[j] == '\0'); j--)
		;
	Message[++j] = '\0';
	    
	newHostAddr = ConsoleAddr.sin_addr.s_addr;
	if (newHostAddr != ThisHostAddr)  {
	    if (newHostAddr != lastHostAddr)  {
		lastHostAddr = newHostAddr;
	        hp = gethostbyaddr(&ConsoleAddr.sin_addr, sizeof(ConsoleAddr.sin_addr), AF_INET);
	        if (hp == 0) {
		    strcpy(hostname, inet_ntoa(ConsoleAddr.sin_addr));
	        } else {
		    strcpy(hostname, hp->h_name);
	        }
	    }
	    if (lc_strcmp(hostname, ThisHostName) && lc_strcmp(hostname, "localhost")) {
		sprintf(&(Message[j]), " [From host %s]", hostname);
	    }
	}
	AnotherError(self, Message, TRUE);
    }

    if (j<0) {
	if (errno != EWOULDBLOCK && errno != EAGAIN)  {
	    ReportInternalError(self, "Error message possibly lost (reading console service");
	    return(-1);
	}
    }
}

static char *mailargvector[] = {_SITE_QUEUEMAIL, "-i", "bogus", 0};

static char MyHost[100]="";

AnotherError(self, errstring, AddNewline)
    struct consoleClass *self;
    char *errstring;
    boolean AddNewline;
{
    int Priority = DefaultErrorPriority, i, j, max, nerr;
    char *ErrText, *ErrPri=NULL, *Application=NULL, *LogAddress=NULL, *sdum;
    struct proctable_Entry *menuProc;
    extern TogVar();

    mydbg(("entering: AnotherError\n"));
    if (MyHost[0] == '\0') gethostname(MyHost, sizeof(MyHost)-1);
    ErrText = errstring;
    if (errstring[0] == '<') {  /* Find and strip message priority, etc. */
	max = strlen(errstring);
	for (i=1; i<max && errstring[i] != '>'; ++i) {
	    ;
	}
	if (i<max) {
	    ErrText += i + 1;
	    errstring[i] = '\0';
	    ErrPri = errstring +1;
	    for (sdum=ErrPri; *sdum && *sdum != ':'; ++sdum) { ; }
	    if (*sdum == ':') {
		*sdum++ = '\0';
		if (*sdum) Application = sdum;
	    }
	    if (*sdum) {
		while (*sdum && *sdum != ':') ++sdum;
		if (*sdum == ':') {
		    *sdum++='\0';
		    if (*sdum) LogAddress = sdum;
		}
	    }
	    if (*sdum) {
		while (*sdum && *sdum != ':') ++sdum;
		if (*sdum == ':') {
		    *sdum++='\0';
		}
	    }
	    if ((j=atoi(ErrPri)) > 0 && j < 10) {
		Priority = j;
	    } else {
		if (!lc_strcmp(ErrPri, "critical") || !lc_strcmp(ErrPri, "0")) {
		    Priority = 0;
		} else if (!lc_strcmp(ErrPri, "warning")) {
		    Priority = 4;
		} else if (!lc_strcmp(ErrPri, "monitor")) {
		    Priority = 6;
		} else if (!lc_strcmp(ErrPri, "debug")){
		    Priority = 9;
		} else {
		    errstring[i] = '>';
		    ErrText = errstring;
		}
	    }
	}
    }
    /* May eventually want to log everything at high priority with no ID,
     * but for now we require an explicit log address.
     */
    if (LogAddress && *LogAddress) {  
	/* Log everything here */
	FILE *lfp;

	if (*LogAddress == '/') {
	    /* Log by appending to file */

	    sprintf(ErrTxt, "console:  I cannot log anything to file %s", LogAddress);
	    if ((lfp = fopen(LogAddress, "a")) == NULL) {
		ReportInternalError(self, ErrTxt);
	    } else {
		fprintf(lfp, "%s (from %s at %s on %s)\n", 
		    Application ? Application : "Unknown program",  ErrText, 
		    Numbers[CLOCKALL].RawText, MyHost);
		if (fclose(lfp) != 0) {
		    ReportInternalError(self, ErrTxt);
		}
	    }	    
	} else {
	    /* Log by sending mail */
	    sprintf(ErrTxt, "console:  I cannot send logging mail to %s", LogAddress);
	    mailargvector[2] = LogAddress;
	    if ((lfp = qopen(_SITE_QUEUEMAIL, mailargvector, "w")) == NULL) {
		ReportInternalError(self, ErrTxt);
	    } else {
		fprintf(lfp, "From: console\nTo:%s\nSubject:Console Error Report\n\n%s\n", LogAddress, ErrText);
		if (fclose(lfp) != 0) {
		    ReportInternalError(self, ErrTxt);
		}
	    }
	}
    }
    if (Priority > HighestPriority) {
	return(0);
    }
    if (20+ (Application ? strlen(Application) : 0)+strlen(ErrText) > ERRBUFSIZE) {
	ErrText[ERRBUFSIZE -20 -( Application ? strlen(Application) : 0 )] = '\0'; /* Truncate rather than dump core */
    }
    if (Priority == 0) {
	if (Application) {
	    sprintf(PrimaryErrorBuffer, "ERROR: <%s>%s%s", Application, ErrText, AddNewline ? "\n" : "");
	} else {
	    sprintf(PrimaryErrorBuffer, "ERROR: %s%s", ErrText, AddNewline ? "\n" : "");
	}
    } else {
	if (Application) {
	    sprintf(PrimaryErrorBuffer, "<%s>%s%s", Application, ErrText, AddNewline ? "\n" : "");
	} else {
	    sprintf(PrimaryErrorBuffer, "%s%s", ErrText, AddNewline ? "\n" : "");
	}
    }
    nerr = Numbers[ERRORS].Value + 1;
    if (Numbers[ERRORS].Value > 1000 || Numbers[ERRORS].Value < 0) {
	nerr = 1;
    }
    /*
      arrgh(("Calling NewValue\n"));
      arrgh(("Numbers[ERRORS].RawText = %s\n", Numbers[ERRORS].RawText));
      */
    /*    AddStringToLog(Numbers[ERRORS].RawText, RegionLogs[ERRORREGIONLOG]); */
    NewValue(self, &Numbers[ERRORS], nerr, NULL, TRUE);
    if (IntrnlVars[0].InUse && !IntrnlVars[0].Value) {
	IntrnlVars[0].Value = TRUE;
	consoleClass_WantUpdate(self, self);
        if (IntrnlVars[0].turnon) {
            sprintf(ErrTxt, "%s", IntrnlVars[0].turnoff);
	    menuProc = proctable_DefineProc("TV", TogVar, consoleClass_GetInfoHack(), NULL, "dummy.");
            menulist_AddToML(self->userMenulist, ErrTxt, menuProc, 0, 0);
            menulist_DeleteFromML(self->userMenulist, IntrnlVars[0].turnon);
	}
    }
    consoleClass_PostMenus(self, self->stdMenulist);
}

/* ****************************************************************
 * Take an all lower case word and a mixed case word and return TRUE
 * if they match, case insensitively. Expectation is usually that it
 * won't match,so the goal is to return FALSE as soon as possible.
 ****************************************************************
 */

