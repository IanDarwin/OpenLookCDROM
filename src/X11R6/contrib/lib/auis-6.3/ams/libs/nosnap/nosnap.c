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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/nosnap/RCS/nosnap.c,v 2.10 1992/12/15 21:21:37 rr2b R6tape $";
#endif

#include <stdio.h>
#include <mserrno.h>
#include <errprntf.h>
#define TRUE 1
#define FALSE 0

char *ReconHost = "<this host>";
int MSDebugging = 0;
int SNAP_debugmask = 0;
int  CUI_RPCInProgress = 0; /* Never in progress in this version ! */
int CUI_SnapIsRunning = 0, CUI_LastCallFinished = 0;
char *SnapVersionString = "NOT LINKED";

/* the following overrides the messageserver routine in the no-snap version. */
BizarreError(text, level)
char *text;
int level;
{
    if (level >= ERR_WARNING) {
	ReportSuccess(text);
    } else {
	ReportError(text, level, FALSE);
    }
}

ReconnectMS(s) char *s; {}  /* Dummy function here to satisfy the linker */

int MS_CUI_Init(host, user, passwd, len, type, bufsize)
char *host, *user, *passwd;
int len, type, bufsize;
{
    char *ThisError = "Messageserver initialization failed.  Program can not run.";
    char *errorMsg = NULL;
    int dummy, level = ERR_FATAL;
    int Decode = TRUE;

    if(mserrcode = MS_Initialize(&dummy, FALSE)) {
	if (AMS_ERRCAUSE == EIN_MSPATHCHECK) {
	    switch(AMS_ERRNO) {
		case ENOENT:
		    ThisError = "An element of your mspath does not exist.  Please check and fix your mspath preference.";
		      errorMsg = (char*) malloc(strlen(ThisError) + 1);
		    strcpy(errorMsg, ThisError);
		    ThisError = errorMsg;
		    Decode = FALSE;
		    break;
		case EACCES:
		case EMSUNAUTH:
		    ThisError = "Unreadable mspath element.  If you're authenticated, please check & fix your mspath preference.";
		    errorMsg = (char*) malloc(strlen(ThisError) + 1);
		    strcpy(errorMsg, ThisError);
		    ThisError = errorMsg;
		    Decode = FALSE;
		    break;
		case EMSNOSUCHVAR:
		    ThisError = "Your mspath preference refers to an unknown variable.  Please check & fix it.";
		    errorMsg = (char*) malloc(strlen(ThisError) + 1);
		    strcpy(errorMsg, ThisError);
		    ThisError = errorMsg;
		    Decode = FALSE;
		    break;
	    }
	    level = ERR_CRITICAL;
	}
	else if(AMS_ERRCAUSE == EIN_OPENDIR) {
	    switch(AMS_ERRVIA) {
		case EVIA_DOIHAVEMAIL:
		    {
		    extern char *GetPersonalMailbox();
		    char *mailbox;

		    mailbox = GetPersonalMailbox();
		    ThisError = "Could not open your personal mailbox";
		    errorMsg = (char*) malloc(strlen(ThisError) + strlen(mailbox) + 5);
		    sprintf(errorMsg, "%s [%s].", ThisError, mailbox);
		    ThisError = errorMsg;
		    Decode = FALSE;
		    level = ERR_CRITICAL;
		    break;
		    }
	    }
	}
	ReportError(ThisError, level, Decode);
	if(errorMsg) free(errorMsg);
	return(mserrcode); /* Not reached if level is ERR_FATAL */
    }
    return(0);
}

AMS_getandpackALLtokens() {} /* Not needed for standalone version */

CUI_EndConversation() {
    MS_UpdateState();
}

