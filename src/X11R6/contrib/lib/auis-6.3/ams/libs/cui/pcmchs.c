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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/cui/RCS/pcmchs.c,v 2.11 1992/12/15 21:17:22 rr2b R6tape $";
#endif


 

/* Machine dependent modules --  PC Version                     */
#include <andyenv.h>
#include <stdio.h>
#include <errno.h>
#include <cui.h>
#ifdef IBMPC
#include <process.h>
#endif /* IBMPC */
#include <errprntf.h>
#ifdef SNAP_ENV /* fix for makedepends error */
#include <snap.h>
#endif /* SNAP_ENV  */
#include <snapams.h>
#include <pwd.h>
struct passwd *getpwuid();
#define MAXBUFFERSIZE 20000
char *mktemp(), *getenv();

#ifndef _IBMR2
char *malloc();
#endif /* _IBMR2 */

int     SNAP_debuglevel=0,
        SNAP_socket=0,
        CUI_OnSameHost = FALSE;
char EditorBuf[200] = "";

SetEditorToUse(ed)
char *ed;
{
    strncpy(EditorBuf, ed, sizeof(EditorBuf));
}


Machine_Init(ThisHost, ThisUser, ThisPassword, ThisPasswordLen, ThisPasswordType, IsRecon)
char **ThisHost, **ThisUser, **ThisPassword;
int *ThisPasswordLen, *ThisPasswordType, IsRecon;
{
        CUI_SetMachineType("IBMPC");
        CUI_SetMachineName("<anonymous>");
        SetTerminalParams(25, 80);
        *ThisHost = NIL;
        *ThisUser = NIL;
        *ThisPassword = NIL;
        *ThisPasswordLen = 0;
        *ThisPasswordType = 0;
        return(FALSE);
    /* * return(!PC_Login());  ** */
}

PC_Login()
{
    SNAP_CPARMS parms;
    char tmpHost[20], ErrorText[500];
    int result;
    char *EditVec[3];
    CallPacket *request;
    ReturnPacket *reply;
    /* Try to allocate SNAP buffers */
    request = (CallPacket *) malloc(MAXBUFFERSIZE);
    reply = (ReturnPacket *) malloc(MAXBUFFERSIZE);
    if (request == NIL || reply == NIL) {
        sprintf(ErrorText, "Fatal Initialization error: Can't allocate SNAP buffers: request=0x%x, reply=0x%x\n", request, reply);
        ReportError(ErrorText, ERR_CRITICAL, FALSE);
        printf("Execution terminated.\n");
        exit(1);
    }
    /* Do SNAP initialization */
    result = SNAP_ClientInit();
    if (result != SNAP_SUCCESS) {
        sprintf(ErrorText, "Fatal initialization error: SNAP_ClientInit failed: %d\n", result);
        ReportError(ErrorText, ERR_CRITICAL, FALSE);
        printf("Execution terminated.\n");
        exit(1);
    }
    /* Start up conversation */
    parms.maxtime = AMS_SNAP_TIMEOUT;
    parms.timeout = 15;
    parms.encryptlevel = SNAP_ENCRYPT;
    result = SNAP_BeginConv("snap.guardian", NIL, "MessageServer", NIL, NIL, 0, 0, &parms);
    free(request);    free(reply);   /* These will be re-done later   */
    if (result >= 0) return(TRUE);

    EditVec[0] = "login";
    EditVec[1] = "MessageServer";
    EditVec[2] = NIL;
    result=spawnvp(P_WAIT,EditVec[0], EditVec);
    if ( result >= 0 ) return(TRUE);
    ReportError("Error logging in.  Please make sure 'login' is in your path.\n",
                ERR_WARNING, FALSE);
    return(FALSE);
}

/* This routine generates a temporary file name to be written on the
        local machine.  */

CUI_GenLocalTmpFileName(nmbuf)
char *nmbuf;
{
    static char temp[9];
    strcpy(temp,"MSXXXXXX");
    strcpy(nmbuf,mktemp(temp));
}

EditLocalFile(LocalName, FinishedElsewhere)
char *LocalName;
Boolean *FinishedElsewhere;
{
    int pid, pid2, status;
    char ErrorText[256];
    char *EditVec[4];

    if (!strcmp(getenv("TERM"), "wm")) {
        *FinishedElsewhere = TRUE;
        EditVec[0] = AndrewDir("/bin/sendmessage");
        EditVec[1] = "-f";
        EditVec[2] = LocalName;
        EditVec[3] = NIL;
    } else {
        *FinishedElsewhere = FALSE;
	if (EditorBuf[0]) {
	    EditVec[0] = EditorBuf;
	} else {
	    EditVec[0] = getenv("EDITOR");
	}
        if (!EditVec[0] || ! *EditVec[0]) {
	    ReportError("No local editor specified.", ERR_WARNING, FALSE);
	    return(-1);
	}
        EditVec[1] = LocalName;
        EditVec[2] = NIL;
        EditVec[3] = NIL;
    }

    pid2=spawnvp(P_WAIT,EditVec[0], EditVec);

    if ( pid2 < 0 ) {
	if (errno == ENOMEM)
	    sprintf(ErrorText,"Not enough memory to load '%s'",EditVec[0]);
	else
	    sprintf(ErrorText, "Error editing local file '%s' with editor '%s'", LocalName, EditVec[0]);
        ReportError(ErrorText, ERR_WARNING, FALSE);
        return(-1);
    }
    return(0);
}

/* Logging of ms interactions is not currently implemented on the PC. */
/* See the Andrew module for what these routines might be made to do. */

InitializeLogging() {}
LogStart() {}
LogEnd(name) char *name; {}
amsconfig(argc, argv, name)
int argc; char **argv, *name;
{}
RedirectOutput() {}
/* ap_Shorten is available in Andrew's libutil.a but probably not on the PC. */
char *ap_Shorten(arg) char *arg; {return (arg);}

/* The following routine must be fleshed in if you want keepalives on the
    PC; see the andrew version of machines.c for an example.  */

CUI_InitializeKeepalives() {}

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

vfclose(fp)
FILE *fp;
{
        return(fclose(fp));
}

AlarmSignalHandler() {}
