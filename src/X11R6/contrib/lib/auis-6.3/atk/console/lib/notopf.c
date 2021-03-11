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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/notopf.c,v 2.9 1992/12/15 21:30:21 rr2b R6tape $";
#endif


 

/* 
 * notop.c -- Routines for instrument console that are
 * NOT shared by the operator's console.
 */

#include <class.h>
#include <conclass.ih>
#include <im.ih>
#include <event.ih>
#include <console.h>
#include <andrewos.h> /* sys/time.h */
#include <errno.h>
#include <signal.h>


WakeUp(self)
    struct consoleClass *self;
{
    mydbg(("entering: WakeUp\n"));
    if (!PauseEnqueuedEvents){
	if (!RingingAlarm){
	    if (DoVMStat && ++VMPollCt > VMPollFreq) {
		VMPollCt = 1;
/*		ComputeStatistics(self);*//*handled directly by im_AddFileHandler in vmmon.c:InitStatistics ? */
	    }
	    if (DoDiskFreeStat && ++DiskPollCt > DiskPollFreq) {
		DiskPollCt = 1;
/*		FindFreeDiskSpace(self);*//*handled directly by im_AddFileHandler in vmmon.c:InitStatistics ? */
	    }
	    if (DoVenusChecking && ++VenusPollCt > VenusPollFreq) {
		VenusPollCt = 1;
		CheckVenusQuota(self);
	    }
	    if (DoMailChecking && ++MailPollCt > MailPollFreq) {
		MailPollCt = 1;
		CheckMail(self, FALSE);
	    }
	    if (DoPrintChecking && ++PrintPollCt > PrintPollFreq) {
		PrintPollCt = 1;
		CheckPrint(self);	/* In mailmon.c, for no good reason */
	    }
	    if (DoDirChecking && ++DirPollCt > DirPollFreq) {
		DirPollCt = 1;
		CheckDirectories(self);	/* In mailmon.c */
	    }
	    if (DoCheckClock && ++ClockPollCt > ClockPollFreq) {
		ClockPollCt = 1;
		CheckClock(self);
	    }
	    if (++FPAPollCt > FPAPollFreq){
		FPAPollCt = 1;
		CheckFPA(self);
	    }
	}
	else
	    NewValue(self, &Numbers[CLOCKALL], Numbers[CLOCKALL].Value, NULL, TRUE);
	/* That last line is a hack so that the * alarm flashes as often as
	 possible */
	if (DoTroubleChecking) {
	    CheckTrouble(self);
	}
    }
    im_EnqueueEvent(WakeUp, self, event_SECtoTU(Period));
}

