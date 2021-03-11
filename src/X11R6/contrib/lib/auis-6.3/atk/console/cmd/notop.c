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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/cmd/RCS/notop.c,v 2.14 1993/01/08 16:30:59 rr2b R6tape $";
#endif


 

/* 
 ****************************************************************
 * notop.c -- Routines for instrument console that are NOT
 * shared by the operator's console.
 ****************************************************************
 */

#include <class.h>
#include <conclass.ih>
#include <console.h>
#include <andrewos.h> /* sys/time.h */
#include <errno.h>
#include <signal.h>

/* the following char* values get used in:
  1.) ../lib/input.c
  2.) ../lib/setup.c
*/
char *RealProgramName = "Console";
char EXTENSION[] = "con";
char EXTENSION1[] = "console";
char EXTENSION2[] = "Console";



ChooseColumns(numcol)
    int numcol;
{
    mydbg(("entering: ChooseColumns\n"));
}

ChooseMachines(self, machinelist)
struct consoleClass *self;
    char *machinelist;
{
    mydbg(("entering: ChooseMachines\n"));
}

ConfigureMachines(self, Rows, Columns, Machines, Initialize)
struct consoleClass *self;
    int *Rows, *Columns, *Machines;
    boolean Initialize;
{
    mydbg(("entering: ConfigureMachines\n"));
    *Rows = *Columns = *Machines = 1;
}

struct datum *BuildDatum(keyword, machine)
    char *keyword;
    int machine;
{
    mydbg(("entering: BuildDatum\n"));
    return(&Numbers[ALWAYS]);
}

OneTimeRemoteInit(self)
    struct consoleClass *self;
{
    mydbg(("entering: OneTimeRemoteInit\n"));
    /* Does nothing if not operator console */
}

InitializeGetStats(self)
    struct consoleClass *self;
{
    static boolean DidInitDisks = FALSE;
    if ((DoVMStat && ! DidInitGVM) || (DoDiskFreeStat && !DidInitDisks)){
	console_InitStats(self);
	DidInitGVM = TRUE;
	DidInitDisks=TRUE;
    }

}

InitializeInstruments(self)
    struct consoleClass *self;
{
    static boolean DidInitMail = FALSE, DidInitDirs = FALSE, DidInitPrint = FALSE;

    mydbg(("entering: InitializeInstruments\n"));
    if (DoVenusMonitoring && ! DidInitVenus) {
	InitializeMariner(self);
	DidInitVenus = TRUE;
    }
    if (! DidInitVenus) {
/*	VenusIn = winin; */
	VenusIn = NULL; /* ??? */
    }
    if (DoMailChecking && !DidInitMail) {
	InitMail(self);
	DidInitMail = TRUE;
    }
    if (DoPrintChecking && !DidInitPrint) {
	InitPrint(self);
	DidInitPrint = TRUE;
    }
    if (DoDirChecking && !DidInitDirs) {
	InitDirectories(); /* In mailmon.c */
	DidInitDirs = TRUE;
    }
}

struct RegionLog *WhichErrorLog(i)
    int i;
{
    mydbg(("entering: WhichErrorLog\n"));
    return &RegionLogs[ERRORREGIONLOG];
}

ReInitializeRemoteInstruments() {}
