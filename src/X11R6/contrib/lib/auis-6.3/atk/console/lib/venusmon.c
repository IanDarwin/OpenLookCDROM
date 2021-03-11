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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/venusmon.c,v 2.27 1993/08/25 20:38:46 susan Exp $";
#endif


 

/* 
 ***************************************************************
 * Routines swiped from mariner for Instrument Console
 * These routines monitor file system traffic.
 ***************************************************************
 */

#include <andyenv.h>
#include <andrewos.h>
#include <class.h>
#include <conclass.ih>
#include <im.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <rect.h>
#include <console.h>
#include <environ.ih>
#include <ctype.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <afs/vice.h>
#include <afs/errors.h>
#include <afs/prs_fs.h>
#define bool_t int
#include <afs/afsint.h>
#endif /* AFS_ENV */
#include <sitevars.h>

extern ClearWindow();
extern InitPstrings();
extern PromptToWindow();
extern char Pstring1[],Pstring2[],Pstring3[];

extern CheckMariner(); /* something like this seems to be needed - ghoti */
int venusSocket;		/* socket to Venus */
boolean FetchInProgress, StoreInProgress;
char LastFetchMsg[128],  LastStoreMsg[128];
extern struct fontdesc *console10font;
extern char *sys_errlist[];
char OtherVenusStr[150], FetchVenusStr[150], FinishedVenusStr[150];
extern boolean NonViceHost, NonAFSDHost;

IsViceRunning()
{
    char *s;
    
    mydbg(("entering: IsViceRunning\n"));
    NonViceHost = (ViceIsRunning() != 0 ? FALSE : TRUE);
    if (! NonViceHost) {
	if ((s = environ_GetConfiguration("AMS_NonViceHost")) != NULL){
	    if (s[0] == 'y' || s[0] == 'Y' || s[0] == 't' || s[0] == 'T' || s[0] == '1'){
		NonViceHost = TRUE;
	    }
	    else if (s[0] == 'n' || s[0] == 'N' || s[0] == 'f' || s[0] == 'F' || s[0] == '0'){
		NonViceHost = FALSE;
	    }
	}
    }
     /* find out if AFSD is not running */
    if (! NonViceHost){
	if ((s = environ_GetConfiguration("VENUS")) != NULL){
	    if (s[0] == 'y' || s[0] == 'Y' || s[0] == 't' || s[0] == 'T' || s[0] == '1'){
		NonAFSDHost = TRUE;
	    }
	    else if (s[0] == 'n' || s[0] == 'N' || s[0] == 'f' || s[0] == 'F' || s[0] == '0'){
		NonAFSDHost = FALSE;
	    }
	}
	else {
	    NonAFSDHost = FALSE;
	}
    }
}



int make_socket(self, port)
struct consoleClass *self;
int port;
{

    int desc, protonum;
    struct protoent *proto;
    struct sockaddr_in socketname;

    mydbg(("entering: make_socket\n"));
    if ((proto = getprotobyname("udp")) == NULL) /* Some machines don't have /etc/protocols... */
        protonum = 6;
    else
        protonum = proto->p_proto;

    desc = socket(AF_INET, SOCK_DGRAM, protonum);
    if (desc < 0) {
        ReportInternalError(self, "console:<make_socket> socket failed");
        return(-1);
    }

    socketname.sin_family = AF_INET;
    /* Address info must be in network byte order! */
    socketname.sin_port = htons(port);
    socketname.sin_addr.s_addr = INADDR_ANY;

    if (bind(desc, &socketname, sizeof(socketname)) < 0) {
	ReportInternalError(self, "console:<make_socket> bind failed");
        return(-1);
    }
    return(desc);
}

InitializeMariner(self)
    struct consoleClass *self;
{
#ifdef AFS_ENV
    char *p;
    int flags;

/* 
 * Look for initialized name of venus -- could have used initstring on any of
 * three instruments.  "Both" is out of date but we try to handle it robustly
 * since lots of console files use it.
 */

    mydbg(("entering: InitializeMariner\n"));
    IsViceRunning();
    if(!NonViceHost){
	Numbers[MARINEROTHER].RawText = OtherVenusStr;
	Numbers[MARINERFETCH].RawText = FetchVenusStr;
	Numbers[MARINERFINISHED].RawText = FinishedVenusStr;
	FetchInProgress = StoreInProgress = FALSE;
	LastFetchMsg[0] = '\0';
	LastStoreMsg[0] = '\0';
	/* Find Venus and start talking to it. */
	if (NonAFSDHost){
	    if ((venusSocket = Bind(_SITE_VENUS_ITC_SOCKET, NULL)) < 0) {
		ReportInternalError(self, "console: Initial bind to venus.itc (2106) failed; trying 2107...");
		if ((venusSocket = Bind(_SITE_VENUS_ITC_SOCKET_ALT, NULL)) < 0) {
		    ReportInternalError(self, "console: Can't find venus.itc (210[67]): can't report AFS traffic.");
		    VenusIn = NULL;
		    return;
		}
	    }
	    flags = fcntl(venusSocket, F_GETFL, 0);
#if POSIX_ENV
	    fcntl(venusSocket, F_SETFL, flags | O_NONBLOCK);
#else
	    fcntl(venusSocket, F_SETFL, flags | FNDELAY);
#endif
	    VenusIn = fdopen(venusSocket, "r");
	    /* Set the options we want. */
	    p = "set:fetch\n";
	    write(venusSocket, p, strlen(p));
	    im_AddFileHandler(VenusIn, CheckMariner, self, 1);
	}
	else{
	    venusSocket = 0;

	    if ((venusSocket = make_socket(self, _SITE_VENUS_ITC_SOCKET)) < 0){
		ReportInternalError(self, "Venus will not be monitored");
		return;/* should I be setting some global flag ? */
	    }
	    flags = fcntl(venusSocket, F_GETFL, 0);
#if POSIX_ENV
	    fcntl(venusSocket, F_SETFL, flags | O_NONBLOCK);
#else
	    fcntl(venusSocket, F_SETFL, flags | FNDELAY);
#endif
	    VenusIn = fdopen(venusSocket, "r");
	    if (VenusIn == NULL){
		ReportInternalError(self, "fdopen of VenusIn failed");
		return; /*global flags anyone? */
	    }
	    im_AddFileHandler(VenusIn, CheckMariner, self, 1);
	}
    }
#endif /* AFS_ENV */
}




int Bind (service, host)
    int service;
    char *host;
{
    int s;
    char buf[100];
    struct sockaddr_in server;
    struct servent *sp;
    struct hostent *hp;

    mydbg(("entering: Bind\n"));
    if (host == NULL) {
        gethostname(buf, sizeof(buf));
        host = buf;
    }
    sp = getservbyport(htons(service), "tcp");
    if (sp == NULL){
	return -1;
    }
    hp = gethostbyname(host);
    if (hp == NULL){
	return -1;
    }
    bzero((char *) &server, sizeof(server));
    bcopy(hp->h_addr, (char *) &server.sin_addr, hp->h_length);
    server.sin_family = hp->h_addrtype;
    server.sin_port = sp->s_port;
    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0){
	return -1;
    }
    if (connect(s, &server, sizeof(server)) < 0) {
        close(s);
        return -1;
    }
    return s;
}

extern boolean REVSCROLL;

LogMarinerFetchInfo(disp)
    struct display *disp;
{
#ifdef AFS_ENV
    int ct = 0;
    char DoneString[256];
    struct RegionLog *logptr;

    mydbg(("entering: LogMarinerFetchInfo\n"));
    if(!NonViceHost){
	if (disp->DisplayStyle == REVERSESCROLLING){
	    REVSCROLL = TRUE;
	}
	logptr = disp->AssociatedLog;
	if (FetchInProgress) {
	    AddStringToLog(LastFetchMsg, logptr);
	    ++ct;
	}
	if (StoreInProgress) {
	    AddStringToLog(LastStoreMsg, logptr);
	    ++ct;
	}
	if (ct == 0) { /* Nothing going on, so report on what is DONE */
	    if (LastFetchMsg[0]) {
		sprintf(DoneString, "%s%s", (NonAFSDHost) ? "Done " : "-->> ", LastFetchMsg);
		AddStringToLog(DoneString, logptr);
		++ct;
	    }
	    if (LastStoreMsg[0]) {
		sprintf(DoneString, "%s%s", (NonAFSDHost) ? "Done " : "-->> ", LastStoreMsg);
		AddStringToLog(DoneString, logptr);
		++ct;
	    }
	}
	if (ct == 0) {
	    AddStringToLog("There has been no file system activity.\n", logptr);
	}
    }
#endif /* AFS_ENV */
}



VenusNovelty(self, rock)
    struct consoleClass *self;
    char *rock;
{
    int dum;

    mydbg(("entering: VenusNovelty\n"));
    PauseEnqueuedEvents = TRUE;
    ClearWindow(self);
    InitPstrings();
    sprintf(Pstring1, "Once Upon A Time, In The AFS...");
    sprintf(Pstring2, "...When Venus Died... ");
    sprintf(Pstring3, "(click anywhere)");
    PromptToWindow(self);
    consoleClass_SetTransferMode(self, graphic_BLACK);
    consoleClass_SetFont(self, console10font);
    consoleClass_MoveTo(self, 0, consoleClass_GetLogicalHeight(self));
    consoleClass_DrawString(self, "X", graphic_ATLEFT | graphic_ATBOTTOM);
    while (! im_GetCharacter(consoleClass_GetIM(self))) {
	;
    }
    ClearWindow(self);
    InitPstrings();
    sprintf(Pstring1, "...She Would Rise From The Ashes...");
    sprintf(Pstring2, ".....Like A Phoenix.....");
    PromptToWindow(self);
    consoleClass_SetTransferMode(self, graphic_BLACK);
    consoleClass_SetFont(self, console10font);
    consoleClass_MoveTo(self,0, consoleClass_GetLogicalHeight(self));
    consoleClass_DrawString(self, "X", graphic_ATLEFT | graphic_ATBOTTOM);
    consoleClass_FlushGraphics(self);
    sleep(4);

    ClearWindow(self);
    consoleClass_SetTransferMode(self, graphic_BLACK);
    consoleClass_SetFont(self, console10font);
    consoleClass_MoveTo(self, 0, consoleClass_GetLogicalHeight(self));
    consoleClass_DrawString(self, "X", graphic_ATLEFT | graphic_ATBOTTOM);
    consoleClass_SetFont(self, console10font);
    for (dum = consoleClass_GetLogicalHeight(self); dum > 0; --dum) {
        if (dum < consoleClass_GetLogicalHeight(self)) {
	    consoleClass_SetTransferMode(self, graphic_WHITE);
	    consoleClass_MoveTo(self, 0, dum + 1);
	    consoleClass_DrawString(self, "V", graphic_ATLEFT | graphic_ATTOP);
	    consoleClass_SetTransferMode(self, graphic_BLACK);
        }
	consoleClass_MoveTo(self, 0, dum);
	consoleClass_DrawString(self, "V", graphic_ATLEFT | graphic_ATTOP);
	consoleClass_FlushGraphics(self);
    }
    InitPstrings();
    sprintf(Pstring1, "....And Live On Again....");
    sprintf(Pstring2, "(in a somewhat confused state)");
    sprintf(Pstring3, "And Now - Back To The Show");
    PromptToWindow(self);
    consoleClass_FlushGraphics(self);
    sleep(4);
    PauseEnqueuedEvents = FALSE;
    RedrawDisplays(self);
}

IsViceError(n)
    int n;
{
    mydbg(("entering: IsViceError\n"));
    switch(errno) {
        case EPIPE:
        case ENXIO:
        case ETIMEDOUT:
        case ENOTTY: case EIO: case ENODEV:
        case 0: /* Is the pioctl stuff *really* fixed?  I doubt it */
#ifdef AFS_ENV
#ifdef VOFFLINE
        case VOFFLINE:
#endif /* VOFFLINE */
#ifdef VBUSY
        case VBUSY:
#endif /* VBUSY */
#endif /* AFS_ENV */
            return(1);
        default:
            return(0);
    }
}
