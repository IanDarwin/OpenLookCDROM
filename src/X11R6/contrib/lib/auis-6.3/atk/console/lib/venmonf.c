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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/venmonf.c,v 2.16 1992/12/15 21:31:10 rr2b R6tape $";
#endif


 

/* ***************************************************************
/*	These routines monitor file system traffic. 
*/

/* ***************************************************************** */


#include <system.h>
#include <class.h>
#include <im.ih>
#include <environ.ih>
#include <conclass.ih>
#include <console.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <afs/vice.h>
#include <afs/errors.h>
#include <afs/prs_fs.h>
#define bool_t int
#include <afs/afsint.h>
#include <afs/venus.h>
#endif /* AFS_ENV */

#define MAXVQSIZE 2000
extern int venusSocket;		/* socket to Venus */
extern boolean FetchInProgress, StoreInProgress;
extern char LastFetchMsg[128],  LastStoreMsg[128];
extern char *getenv();
extern char OtherVenusStr[], FetchVenusStr[], FinishedVenusStr[];
extern boolean NonViceHost, NonAFSDHost;

extern char MyHomeDir[];

int SetHomeEnv()
{
    mydbg(("entering: SetHomeEnv\n"));
    strcpy(MyHomeDir, environ_GetHome(NULL));
    if (MyHomeDir[0] == '\0'){/* should not happen */
	arrgh(("console: Could not determine home Directory.....exiting\n"));
	return(-1);
    }
    return(0);
}


CheckVenusQuota(self)
    struct consoleClass *self;
 {
#ifdef AFS_ENV
    struct ViceIoctl blob;
    VolumeStatus  *status;
    long code, *authcode;
    char space[MAXVQSIZE], ErrTxt[256];

    mydbg(("entering: CheckVenusQuota\n"));
    if(!NonViceHost){
	if (!MyHomeDir || !*MyHomeDir) {
	    SetHomeEnv();
	}
	if (Numbers[UNAUTHENTICATED].IsDisplaying) {
	    int answer = 0;

	    blob.out = space; 
	    blob.out_size = sizeof(long);
	    blob.in_size = 0;
	    code = pioctl("/afs", VIOCCKCONN, &blob, 1);
	    authcode = (long *) space;
	    if (code) {
		answer = Numbers[UNAUTHENTICATED].Value;
		if (answer < 0) {
		    --answer;
		    if (answer < -50) {
			answer = -1;
		    }
		}
		else {
		    answer = -1;
		}
		if (answer == -1) {
		    /* Only complain every 50 times on repeat errors */
		    if (errno == EINVAL) {
			sprintf(ErrTxt, "console: /afs not in AFS: cannot check authentication status");
		    } else {
			sprintf(ErrTxt, "console: Temporary failure to check authentication status (%d)", errno);
		    }
		    ReportInternalError(self, ErrTxt);
		}
	    }
	    else {
		switch (*authcode) {
		    case 0:
			break;
		    case EACCES:
			answer = 1;
			break;
		    default:
			sprintf(ErrTxt, "console: Cannot check authentication (bad auth code %d %d)", code, *authcode);
			ReportInternalError(self, ErrTxt);
			answer = -1;
		}
	    }
	    NewValue(self, &Numbers[UNAUTHENTICATED], answer, NULL, FALSE);
	}
	if (Numbers[VICEPERSONAL].IsDisplaying || Numbers[VICEPARTITION].IsDisplaying) {
	    blob.out = space;
	    blob.out_size = MAXVQSIZE;
	    blob.in_size = 0;
	    code = pioctl(MyHomeDir, VIOCGETVOLSTAT, &blob, 1);
	    if (code) {
		int answer = 0;

		answer = Numbers[VICEPERSONAL].Value;
		if (answer < 0) {
		    --answer;
		    if (answer < -50) {
			answer = -1;
		    }
		}
		else {
		    answer = -1;
		}
		if (answer == -1) {
		    /* Only complain every 50 times on repeat errors */
		    if (errno == EINVAL) {
			sprintf(ErrTxt, "console: $HOME of '%s' not in AFS; cannot check quota usage", MyHomeDir);
		    } else {
			sprintf(ErrTxt, "console: Temporary failure to check vice quota usage (%d, %s)", errno, MyHomeDir);
		    }
		    ReportInternalError(self, ErrTxt);
		}
		NewValue(self, &Numbers[VICEPERSONAL], answer, NULL, FALSE);
		NewValue(self, &Numbers[VICEPARTITION], answer, NULL, FALSE);
		return;
	    } 
	    status = (VolumeStatus *) space;
	    NewValue(self, &Numbers[VICEPERSONAL], status->MaxQuota ?
		     (int) (((1.0 * status->BlocksInUse)/status->MaxQuota) * 100.0): 0, NULL, FALSE);
	    NewValue(self, &Numbers[VICEPARTITION], status->PartMaxBlocks ?
		     (100 * (status->PartMaxBlocks - status->PartBlocksAvail)) / status->PartMaxBlocks : 0, NULL, FALSE);
	}
    }
#endif /* AFS_ENV */

}

#define MARINERBUFSIZE 1000

CheckMariner(ActiveVenus, self)
FILE *ActiveVenus;
struct consoleClass *self;
{
#ifdef AFS_ENV
    static char buf[MARINERBUFSIZE];
    static char *fillptr = buf;
    register int c;

    mydbg(("entering: CheckMariner\n"));
    if(!NonViceHost){
	errno = 0;
	while ((c = getc(ActiveVenus)) != EOF)  {
	    if (c != '\n')  {
		*fillptr++ = c;
	    }
	    if (c == '\n' || fillptr >= buf + sizeof(buf) - 2)  {
		--fillptr; /* back off one from the ending NULL */
		while (fillptr != buf && *fillptr == ' ')
		    --fillptr;
		if (fillptr != buf)
		    ++fillptr;
		*fillptr++ = '\n';
		*fillptr = '\0';
		CheckTheMariner(buf, self);
		fillptr = buf;
	    }
	}

    }
#endif /* AFS_ENV */
}


CheckTheMariner(buf, self)
char *buf;
struct consoleClass *self;
{
#ifdef AFS_ENV
    mydbg(("entering: CheckTheMariner\n"));
    if(!NonViceHost){
	char    *s;
	if (NonAFSDHost){/* PROCESS VENUS IN USE */
	    if (!strncmp(buf, "fetch::", 7)) {
		s = &(buf[7]);
		if (!strncmp(s, "fetch done", 10)) {
		    sprintf(FinishedVenusStr, "Done %s", LastFetchMsg);
		    FetchInProgress = FALSE;
		    NewValue(self, &Numbers[MARINERFINISHED], 0, NULL, TRUE);
		    FetchVenusStr[0] = '\0';
		    NewValue(self, &Numbers[MARINERFETCH],(FetchInProgress || StoreInProgress) ? 1 : 0, NULL, TRUE);
		}
		else {
		    strcpy(LastFetchMsg, s);
		    NewValue(self, &Numbers[MARINERFETCH], TRUE, LastFetchMsg, TRUE);
		    FetchInProgress = TRUE;
		}
	    }
	    else{
		if (!strncmp(buf, "store::", 7)) {
		    s = &(buf[7]);
		    if (!strncmp(s, "store done", 10)) {
			sprintf(FinishedVenusStr, "Done %s", LastStoreMsg);
			StoreInProgress = FALSE;
			NewValue(self, &Numbers[MARINERFINISHED], 0, NULL, TRUE);
			FetchVenusStr[0] = '\0';
			NewValue(self, &Numbers[MARINERFETCH],(FetchInProgress || StoreInProgress) ? 1 : 0, NULL, TRUE);
		    }
		    else {
			strcpy(LastStoreMsg, s);
			NewValue(self, &Numbers[MARINERFETCH], 1, LastStoreMsg, TRUE);
			StoreInProgress = TRUE;
		    }
		}
		else {
		    NewValue(self, &Numbers[MARINEROTHER], 1, buf, TRUE);
		    if (!Numbers[MARINEROTHER].IsDisplaying) {
			ReportInternalError(self, buf);
		    }
		}
	    }
	}
	else {/* AFSD IN USE */
	    s = &buf[6];
	    if (!strncmp(buf, "fetch:", 6)) {/* log & light on */
		strcpy(LastFetchMsg, s);
		NewValue(self, &Numbers[MARINERFETCH], 1, LastFetchMsg, TRUE);
		FetchInProgress = TRUE;
	    }
	    else{
		if (!strncmp(buf, "store:", 6)) {/* log & light on */
		    strcpy(LastStoreMsg, s);
		    NewValue(self, &Numbers[MARINERFETCH], 1, LastStoreMsg, TRUE);
		    StoreInProgress = TRUE;
		}
		else{
		    if (!strncmp(buf, "fetch$", 6)){/* log - no light */
			strcpy(LastFetchMsg, s);
			NewValue(self, &Numbers[MARINERFETCH], (FetchInProgress || StoreInProgress) ? 1 : 0, LastFetchMsg, TRUE);
		    }
		    else{
			if (!strncmp(buf, "store$", 6)){/* log - no light */
			    strcpy(LastStoreMsg, s);
			    NewValue(self, &Numbers[MARINERFETCH], (FetchInProgress || StoreInProgress) ? 1 : 0, LastStoreMsg, TRUE);
			}
			else{
			    if (!strncmp(buf, "done:0", 6)){/* no log - turn off light */
				FetchInProgress = StoreInProgress = FALSE;
				NewValue(self, &Numbers[MARINERFINISHED], 0, NULL, TRUE);
				FetchVenusStr[0] = '\0'; /*??*/
				NewValue(self, &Numbers[MARINERFETCH], 0, NULL, TRUE);
			    }
			    else {
				NewValue(self, &Numbers[MARINEROTHER], 1, buf, TRUE);
				if (!Numbers[MARINEROTHER].IsDisplaying) {
				    ReportInternalError(self, buf);
				}
			    }
			}
		    }
		}
	    }
	}
    }		
#endif /* AFS_ENV */
}

