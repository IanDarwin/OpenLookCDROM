/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see: andrew/config/
******************************************************************* */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/input.c,v 2.45 1994/04/01 20:57:14 rr2b Exp $";
#endif


 

/* 
 ***************************************************************
 * Routines for handling kb & mouse input in Instrument Console
 **************************************************************
 */

/* 
 * By using conclass.eh - I am effectively including the entire
 * consoleClass object within this code - all procedures called via the
 * menuproc will autmatically have the first argument be the object
 * "self"
 */

#include <andrewos.h>
#include <class.h>
#include <menulist.ih>
#include <im.ih>
#include <view.ih>
#include <fontdesc.ih>
#include <proctbl.ih>
#include <cursor.ih>
#include <graphic.ih>
#include <filetype.ih> /* for ~ expansion */
#include <rect.h>
#include <console.h>
#include <convers.h>
#include <ctype.h>
#include <sitevars.h>
#include <sys/param.h>

#define AUXMODULE 1
#include <conclass.eh>

extern struct classinfo *consoleClass_GetInfoHack();
extern ClearWindow();
extern PromptToWindow();
extern GetStringFromWindow();
extern InvertWindow();
extern InitPstrings();
extern int Pposx, Pposy;
extern char Pstring1[256], Pstring2[256], Pstring3[256], Pstring4[MAXPATHLEN];
extern VenusNovelty ();
extern WriteMonsterLog ();
extern ClearAllLogs();


/* EXTENSION(simple, 1, and 2) get set for:
    vopcon files in: ../cmd/viceop.c
    console files in: ../cmd/notop.c
*/
extern char EXTENSION[];
extern char EXTENSION1[];
extern char EXTENSION2[];

#define MAXPATHLENGTH 1024
#define MAXMENUCARD 6

/* | mask = turn on, & ~mask = turn off */
#define	STD_MASK	0   /* standard menulist */
#define	ALR_OFF_MASK	1   /* Alarm -is- off, Set Alarm menu -is- showing */
#define	ALR_ON_MASK	2   /* Alarm -is- set, Turn Off Alarm -is- showing */
#define	SHR_MASK	4   /* Menus -are- shrunk, Expand Menus -is- showing */
#define	EXP_MASK	8   /* Menus -are- expanded, Shrink Menus -is- showing */
#define	DEB_OFF_MASK   16   /* Debugging -is- off, Turn On Debugging -is- showing */
#define	DEB_ON_MASK    32   /* Debugging -is- on, Turn Off Debugging -is- showing */
#define	GETSTAT_MASK   64   /* Getstats -is- dead, Restart Getstats -is- showing */

#define MAXMATCHES 2
#define MAXEXTLTH 7

#ifdef NOTUSED
static char *PriorityTexts[] = { 
    "Urgent",
    "Error",
    "Error",
    "Error",
    "Error",
    "Warning",
    "Warning",
    "Debug",
    "Debug",
    "Debug",
    0
};
#endif /* NOTUSED */

struct menulist *PrepareUserMenus();

DebugMenu(self, rock)
struct consoleClass *self;
char *rock;
{
    mydbg(("entering: DebugMenu\n"));
    ToggleDebugging(self, NULL);
    if(MYDEBUGGING){
	self->menuMask &= ~DEB_ON_MASK;
	self->menuMask |= DEB_OFF_MASK;
	menulist_SetMask(self->stdMenulist, self->menuMask);
    }
    else{
	self->menuMask &= ~DEB_OFF_MASK;
	self->menuMask |= DEB_ON_MASK;
	menulist_SetMask(self->stdMenulist, self->menuMask);
    }
    consoleClass_PostMenus(self, self->stdMenulist);
    consoleClass_WantUpdate(self, self);
}


DoQuit(self, rock)
struct consoleClass *self;
char *rock;
{
    mydbg(("entering: DoQuit\n"));
    KillInitExecProcesses(TRUE);
    exit(0);
}


char *TitleFromFile(fname, IncludeVersion)
char *fname;
boolean IncludeVersion;
{
    char   *s,
        *t;
    int     i;
    static char Title[40];

    mydbg(("entering: TitleFromFile\n"));
    t = NULL;
    s = fname;
    while (*s != '\0') {
        if (*s == '/')
            t = s;
        ++s;
    }
    if (t == NULL) {
        t = fname;
    }
    else {
        ++t;
    }
    s = t;
    while (*s != '\0' && *s != '.') {
        ++s;
    }
    if (*s == '.') {
        i = s - t;
    }
    else {
        i = strlen(t);
    }
    strncpy(Title, t, i);
    Title[i] = '\0';
    if (IncludeVersion) {
        strcat(Title, CONSOLE_VERSION);
    }
    return(Title);
}


extern char *RealProgramName;
/* consoleName gets referenced in setup.c, in  SetupFromConsole...
    I did this to avoid changing the calling parameters to the proc in setup. */
char *consoleName;

ReadNewConsoleFile(self, rock)
struct consoleClass *self;
char *rock;
{
    consoleName = rock;

    mydbg(("entering: ReadNewConsoleFile\n"));
    if (consoleName == NULL) {
	if(GetConsoleFileFromTypeIn(self, FALSE) == FALSE) {
	    fprintf(stderr,"FATAL error getting the console file type-in!\n");
	    fflush(stderr);
	    exit(-1);
	}
    } else {
	ClearWindow(self);
	strcpy(ConFile, consoleName);
    }
    SetStandardCursor(self, Cursor_Wait);
    /* this is a jump in the control flow, we may never get back from this if there's a failure */
    SetupFromConsoleFile(self, FALSE);
    self->userMenulist = PrepareUserMenus(self, consoleClass_GetInfoHack());
    InitializeInstruments(self);
    if (Numbers[ERRORS].RawText != Nullity && Numbers[ERRORS].RawText)
        Numbers[ERRORS].RawText[0] = '\0';
    if (Numbers[MARINEROTHER].RawText != Nullity && Numbers[ERRORS].RawText)
        Numbers[MARINEROTHER].RawText[0] = '\0';
    if (Numbers[MARINERFETCH].RawText != Nullity && Numbers[ERRORS].RawText)
        Numbers[MARINERFETCH].RawText[0] = '\0';
    im_SetProgramName(RealProgramName);
    im_SetTitle(consoleClass_GetIM(self), TitleFromFile (ConFile, TRUE));
    menulist_ChainBeforeML(self->stdMenulist, self->userMenulist, self->userMenulist);
    consoleClass_PostMenus(self, self->stdMenulist);
    RedrawDisplays(self);
    SetStandardCursor(self, Cursor_Arrow);
    consoleClass_WantUpdate(self, self);
}



int adjmon(mday, max, mon)
int *mday, max, *mon;
{
    if (*mday > max){
	*mon += 1;
	*mday -= max;
	return(FALSE);
    }
    return(TRUE);
}


SetAlarm(self, rock)
struct consoleClass *self;
char *rock;
{
    boolean isPM, isAM;
    int     hr, min, day, mday, wday, mon, year;
    static int TFHC = -1;
    char    buf2[80], *s, event[120];
    static char *DayOfWeek[] = {
	"Sunday", "Monday", "Tuesday", "Wednesday", 
	"Thursday", "Friday", "Saturday", 0};
    long    now;
    struct tm  *t;

    mydbg(("entering: SetAlarm\n"));
    PauseEnqueuedEvents = TRUE;

    isPM = isAM = FALSE;

    if (TFHC == -1){/* only do this once */
	if (!index(Numbers[CLOCKALL].RawText, 'A') && !index(Numbers[CLOCKALL].RawText, 'P')){
	    TFHC = TRUE;
	}
	else{
	    TFHC = FALSE;
	}
    }
    buf2[0] = '\0';
    InitPstrings();
    sprintf(Pstring1, "Alarm Time:");
    sprintf(Pstring2, (TFHC) ? "[12:00]" : "[12:00 Noon]");
    sprintf(Pstring3, "==>> ");
    PromptToWindow(self);
    GetStringFromWindow(self, sizeof(buf2));
    if (Pstring4[0] == '\0'){
	sprintf(Pstring4, (TFHC) ? "12:00" : "12:00 PM");
    }
    now = time(0);
    t = localtime(&now);
    day = t->tm_yday;
    mday = t->tm_mday;
    wday = t->tm_wday;
    mon = t->tm_mon;
    year = t->tm_year;

    hr = min = 0;

    sscanf(Pstring4, "%d:%d%s", &hr, &min, buf2);

    /* if no minutes are given the resulting value may not be what they think they entered */
    
    /* if value entered for hour is greater than 24 assume a millitary like time entry - PM and AM declarations are ignored */

    if (!TFHC){
	if (hr <= 12 && hr > 0) {
	    for (s = buf2; *s; ++s) {
		if (*s == 'P' || *s == 'p')  {
		    isPM = TRUE;
		    break;
		}
		if (*s == 'A' || *s == 'a') {
		    isAM = TRUE;
		    break;
		}
	    }
	    if (isAM || isPM){
		if (hr == 12)  {
		    hr = 0;
		}
		if (isPM)  {
		    hr += 12;
		}
	    }
	    else  {

		/* User did not specify whether it is AM or PM.  Must find the next proper time */

		if (hr == 12){ /* generally the trickiest case */
		    if ((t->tm_hour == 12) && (min <= t->tm_min)){
			hr += 12; /* bump 12 to 24 - next available 12 */
		    }
		    if (t->tm_hour == 0){
			hr += (min <= t->tm_min) ? 24 : 12; /* ? next afternoon : this morning */
		    }
		    if (t->tm_hour > 12){ /* assume less than 24 ? */
			hr += 12; 
		    }
		}
		else  {
		    while ((hr < t->tm_hour) || (hr == t->tm_hour && min <= t->tm_min))  {
			hr += 12;
		    }
		}
	    }
	}
    }
    /* We now have the right time but need to know whether we need to move to the next day */

    if (hr < t->tm_hour || (hr == t->tm_hour && min <= t->tm_min))  {
	hr += 24;
    }

    /* Now adjust for the number of extra days  */

    while (hr >= 24)  { /* 24 should be reduced to 0 - it will be bumbped to 12 later */
	day++;
	wday++;
	mday++;
	hr -= 24;
    }

    wday %= 7;

    if (day > 365){
	int tmpday = day;
	while (tmpday > 365){
	    year++;
	    tmpday -= 365;
	}
    }
    if ((mday > 28 && mon == 1) || mday > 30){
	/* rough elimination so we don't have to go through this every time */
	int done = FALSE;
	mon++; /* 1 - 12 instead of 0 - 11 */
	while (!done){
	    switch(mon){
		case 1: case 3: case 5: case 7: case 8: case 10: case 12:
		    done = adjmon(&mday, 31, &mon);
		    break;
		case 4: case 6: case 9: case 11:
		    done = adjmon(&mday, 30, &mon);
		    break;
		case 2:
		    /* not perfect - but for now will do */
		    done = adjmon(&mday, ((year % 4) == 0) ? 29 : 28, &mon);
		    break;
	    }
	}
    }
    while (mon > 12) {
	mon -= 12;
	year++;
    }
    

    event[0]='\0';
    InitPstrings();
    sprintf(Pstring1, "Event:");
    sprintf(Pstring2, "Wake Up!!");
    sprintf(Pstring3, "==>> ");
    PromptToWindow(self);
    GetStringFromWindow(self, 120);
    if (Pstring4[0] == '\0'){
        strcpy(event,"Wake Up!!");
    }
    else{
	strcpy(event, Pstring4);
    }
    if (event[strlen(event) - 1] == '.'){
	event[strlen(event) - 1] = '\0';
    }
    PauseEnqueuedEvents = FALSE;

    VeryFirstDisplay->Threshhold = day * 24 * 3600 + hr * 3600 + min * 60;

    if (!TFHC){
	if (hr == 12){
	    isPM = TRUE;
	}
	if (hr > 12)   {
	    hr -= 12;
	    isPM = TRUE;
	}

	if (hr == 0){
	    hr += 12;
	    isPM = FALSE;
	}
    }
/*    consoleClass_WantUpdate(self, self);*/
    self->menuMask &= ~ALR_OFF_MASK;
    self->menuMask |= ALR_ON_MASK;
    menulist_SetMask(self->stdMenulist, self->menuMask);
    Numbers[ALARM].Value = TRUE;
    if (!TFHC){
	sprintf(Numbers[ALARM].RawText, "Alarm: %d:%02d %s - %s %d/%d/%d: %s.", hr, min, (isPM) ? "PM" : "AM", DayOfWeek[wday], mon + 1, mday,  year, event);
    }
    else{
	sprintf(Numbers[ALARM].RawText, "Alarm: %02d:%02d - %s %d/%d/%d: %s.", hr, min, DayOfWeek[wday], mon + 1, mday,  year, event);
    }
    consoleClass_PostMenus(self, self->stdMenulist);
    RedrawDisplays(self);
    consoleClass_FlushGraphics(self);
    consoleClass_ReceiveInputFocus(self);
    consoleClass_WantUpdate(self, self);
}

TurnOffAlarm(self, rock)
struct consoleClass *self;
char *rock;
{
    mydbg(("entering: TurnOffAlarm\n"));
    VeryFirstDisplay->Threshhold = 36500000;
    Numbers[ALARM].Value=FALSE;
    strcpy(Numbers[ALARM].RawText, "The alarm clock is not set.") ;
    self->menuMask &= ~ALR_ON_MASK;
    self->menuMask |= ALR_OFF_MASK;
    menulist_SetMask(self->stdMenulist, self->menuMask);
    consoleClass_PostMenus(self, self->stdMenulist);
    PauseEnqueuedEvents = FALSE;
    RingingAlarm = FALSE;
    RedrawDisplays(self);
    consoleClass_FlushGraphics(self);
    consoleClass_ReceiveInputFocus(self);
    consoleClass_WantUpdate(self, self);
}


int LastX = 0, LastY = 0, MovingX;
/* 
 * MovingX is 1 if X boundary is being moved, 0 if y is
 * being moved, and -1 if the right click was not near
 * any boundary.
 */

struct display *FindInstrument(self, x, y)
struct consoleClass *self;
int x,y;
{
    struct display *mydisp;

    mydbg(("entering: FindInstrument\n"));
    for (mydisp = VeryFirstDisplay; mydisp; mydisp = mydisp->NextOfAllDisplays) {
        if (mydisp->Xmin <= x && mydisp->Xmax >= x && mydisp->Ymin <= y && mydisp->Ymax >= y && (!mydisp->DependentUponVariables || IntrnlVars[mydisp->WhichVariable].Value == mydisp->AppearIfTrue)){
            break;
        }
    }
    if (!mydisp) {
	InvertWindow(self);
	InvertWindow(self);
        consoleClass_SetTransferMode(self, graphic_BLACK);
        return(NULL);
    }
    mydisp->LastClickValue = mydisp->WhatToDisplay->Value;
    return(mydisp);
}



#define ABSVALDIFF(a,b) (((a) > (b)) ? ((a) - (b)) : ((b) - (a)))
#define CLOSEFIT 4

CheckMovingX(self, x, y)
struct consoleClass *self;
int x, y;
{
    struct display *mydisp;

    mydbg(("entering: CheckMovingX\n"));
    for (mydisp = VeryFirstDisplay; mydisp; mydisp = mydisp->NextOfAllDisplays) {
        if (mydisp->DependentUponVariables && IntrnlVars[mydisp->WhichVariable].Value != mydisp->AppearIfTrue) {
            continue;
        }
        if ((((ABSVALDIFF(mydisp->Xmin, x)) < CLOSEFIT) || ((ABSVALDIFF(mydisp->Xmax, x)) < CLOSEFIT)) && (mydisp->Ymin <= y) && (mydisp->Ymax >= y)){
            SetStandardCursor(self, Cursor_VerticalBars);
            return(1);
        }
        if ((((ABSVALDIFF(mydisp->Ymin, y)) < CLOSEFIT) || ((ABSVALDIFF(mydisp->Ymax, y)) < CLOSEFIT)) && (mydisp->Xmin <= x) && (mydisp->Xmax >= x)){
            SetStandardCursor(self, Cursor_HorizontalBars);
            return(0);
        }
    }
    SetStandardCursor(self, Cursor_Cross);
    return(-1);
}


ResizeDisplay(self, x, y, LastX, LastY, MovingX)
struct consoleClass *self;
int x, y, LastX, LastY, MovingX;
{
    struct display *mydisp;
    int     old, new, this;

    mydbg(("entering: ResizeDisplay\n"));
    /* Normalize coordinates to relative scales */
    x = x * ScaleFactor / consoleClass_GetLogicalWidth(self);
    y = y * ScaleFactor / consoleClass_GetLogicalHeight(self);
    LastX = LastX * ScaleFactor / consoleClass_GetLogicalWidth(self);
    LastY = LastY * ScaleFactor / consoleClass_GetLogicalHeight(self);

    old = MovingX ? LastX : LastY;
    new = MovingX ? x : y;
    if (ABSVALDIFF(old, new) < CLOSEFIT) {
        return;
    }
    for (mydisp = VeryFirstDisplay; mydisp; mydisp = mydisp->NextOfAllDisplays) {
        this = MovingX ? mydisp->RelXmin : mydisp->RelYmin;
        if (this < old) {
            this = ((this * 1000 * new) / old) / 1000;
        }
        else {
            this = ScaleFactor - ((((ScaleFactor - this) * (ScaleFactor - new) * 1000) / (ScaleFactor - old)) / 1000);
        }
        if (MovingX) {
            mydisp->RelXmin = this;
        }
        else {
            mydisp->RelYmin = this;
        }
        this = MovingX ? mydisp->RelXmax : mydisp->RelYmax;
        if (this < old) {
            this = ((this * 1000 * new) / old) / 1000;
        }
        else {
            this = ScaleFactor - ((((ScaleFactor - this) * (ScaleFactor - new) * 1000) / (ScaleFactor - old)) / 1000);
        }
        if (MovingX) {
            mydisp->RelXmax = this;
        }
        else {
            mydisp->RelYmax = this;
        }
    }
    RedrawDisplays(self);
    consoleClass_WantUpdate(self, self);
}

/* To make things a little easier to read ... for me */
#define IntVarOn(x)    IntrnlVars[(x)].turnon
#define IntVarOff(x)   IntrnlVars[(x)].turnoff

TogVar(self, rock)
    struct consoleClass *self;
    int rock;
{
    int whichvar = rock;
    struct proctable_Entry *menuProc;

    mydbg(("entering: TogVar\n"));
    if (whichvar < 0 || whichvar > NUMINTERNALVARIABLES) {
	sprintf(ErrTxt, "console: Variable %d does not exist", whichvar);
        ReportInternalError(self, ErrTxt);
    }
    else {
        if (IntrnlVars[whichvar].Value) {
            if (whichvar == 0) {
                Numbers[ERRORS].Value = 0;
                Numbers[ERRORS].RawText[0] = '\0';
            }
            IntrnlVars[whichvar].Value = FALSE;
            if (IntVarOn(whichvar)) {
                menulist_DeleteFromML(self->userMenulist, IntVarOff(whichvar));
                menuProc = proctable_DefineProc("console-toggle-var", TogVar, consoleClass_GetInfoHack(), NULL, "dummy.");
                menulist_AddToML(self->userMenulist, IntVarOn(whichvar), menuProc, whichvar, 0);
            }
        }
        else {
            IntrnlVars[whichvar].Value = TRUE;
            if (IntrnlVars[whichvar].turnon) {
                menulist_DeleteFromML(self->userMenulist, IntVarOn(whichvar));
                menuProc = proctable_DefineProc("console-toggle-var", TogVar, consoleClass_GetInfoHack(), NULL, "dummy.");
                menulist_AddToML(self->userMenulist, IntVarOff(whichvar), menuProc, whichvar, 0);
            }
        }
    }
    consoleClass_PostMenus(self, self->stdMenulist);
    RedrawDisplays(self);
    consoleClass_ReceiveInputFocus(self);
    consoleClass_WantUpdate(self, self);

}
    

ExpandMenu(self, rock)
struct consoleClass *self;
char *rock;
{
    mydbg(("entering: ExpandMenu\n"));
    self->menuMask &= ~SHR_MASK;
    self->menuMask |= EXP_MASK;
    menulist_SetMask(self->stdMenulist, self->menuMask);
    if (MYDEBUGGING){
	self->menuMask &= ~DEB_OFF_MASK;
	self->menuMask |= DEB_ON_MASK;
	menulist_SetMask(self->stdMenulist, self->menuMask);
    }
    else{
	self->menuMask &= ~DEB_ON_MASK;
	self->menuMask |= DEB_OFF_MASK;
	menulist_SetMask(self->stdMenulist, self->menuMask);
    }
    consoleClass_PostMenus(self, self->stdMenulist);
    consoleClass_WantUpdate(self, self);
}

ShrinkMenu(self)
struct consoleClass *self;
{
    mydbg(("entering: ShrinkMenu\n"));
    self->menuMask &= ~EXP_MASK;
    self->menuMask &= ~DEB_OFF_MASK;
    self->menuMask &= ~DEB_ON_MASK;
    self->menuMask |= SHR_MASK;
    menulist_SetMask(self->stdMenulist, self->menuMask);
    consoleClass_PostMenus(self, self->stdMenulist);
    consoleClass_WantUpdate(self, self);
}




extern SetConsoleLib();
extern RestartStats();

void GetStdItems(tempMenulist)
struct menulist *tempMenulist;
{
    struct proctable_Entry *menuProc;

    sprintf(ErrTxt, "Display~2, Restart %s", conlib[0].confile?conlib[0].confile:"");
    menuProc = proctable_DefineProc("console-read-new-console-file", ReadNewConsoleFile, consoleClass_GetInfoHack(), NULL, "dummye");
    menulist_AddToML(tempMenulist, ErrTxt, menuProc, conlib[0].path, 0);
    menuProc = proctable_DefineProc("console-quit", DoQuit, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Quit~99", menuProc, NULL, 0);
    menuProc = proctable_DefineProc("console-read-new-console-file", ReadNewConsoleFile, consoleClass_GetInfoHack(), NULL, "dummye");
    menulist_AddToML(tempMenulist, "Display~2, Read New Console File~24", menuProc, NULL, 0);
    menuProc = proctable_DefineProc("console-expand-menu", ExpandMenu, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Display~2, Expand Menu~52", menuProc, NULL, SHR_MASK);
    menuProc = proctable_DefineProc("console-shrink-menu", ShrinkMenu, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Display~2, Shrink Menu~52", menuProc, NULL, EXP_MASK);
    menuProc = proctable_DefineProc("console-set-alarm", SetAlarm, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Set Alarm~5", menuProc, NULL, ALR_OFF_MASK);
    menuProc = proctable_DefineProc("console-turn-off-alarm", TurnOffAlarm, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Turn Off Alarm~5", menuProc, NULL, ALR_ON_MASK);
    menuProc = proctable_DefineProc("console-write-log-file", WriteMonsterLog, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Write Log File~40", menuProc, NULL, 0);
    menuProc = proctable_DefineProc("console-clear-logs", ClearAllLogs, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Clear All Logs~45", menuProc, NULL, 0);
    menuProc = proctable_DefineProc("console-getstats", RestartStats, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Restart Getstats~60", menuProc, NULL, GETSTAT_MASK);
    menuProc = proctable_DefineProc("console-debug", DebugMenu, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Expert Options~90, Turn Off Debugging~90", menuProc, NULL, DEB_ON_MASK);
    menulist_AddToML(tempMenulist, "Expert Options~90, Turn On Debugging~90", menuProc, NULL, DEB_OFF_MASK);
    menuProc = proctable_DefineProc("console-venus-novelty", VenusNovelty, consoleClass_GetInfoHack(), NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Expert Options~90, Venus Novelty~10", menuProc, FALSE, EXP_MASK);
    menuProc = proctable_DefineProc("console-debug", DebugMenu, consoleClass_GetInfoHack() , NULL, "dummy.");
    menulist_AddToML(tempMenulist, "Expert Options~90, Turn Off Debugging~90", menuProc, NULL, DEB_ON_MASK);
    menulist_AddToML(tempMenulist, "Expert Options~90, Turn On Debugging~90", menuProc, NULL, DEB_OFF_MASK);

}


void GetStdConsoles(tempMenulist)
struct menulist *tempMenulist;
{
    char TmpBuf[MAXPATHLEN];
    int len, len2, i, j;
    char *nm, *tail;
    DIRENT_TYPE *dp;
    DIR *dirp;
    struct proctable_Entry *menuProc;
    unsigned char loopCtr;
    char copyOfEXTENSION[1 + MAXEXTLTH];

    mydbg(("Entering: GetStdConsoles\n"));
    connum = 1;

    for (loopCtr = 0; loopCtr <= MAXMATCHES; ++loopCtr) {
	dirp = opendir(libpaths[BasicLib]);
	if (dirp == NULL) {
	    arrgh(("console: Can't open Console Library '%s'\n", libpaths[BasicLib]));
	    if(loopCtr == MAXMATCHES) {
		mydbg(("Leaving: GetStdConsoles(0)\n"));
	    }
	    return;
	}
	bzero(TmpBuf, MAXPATHLEN);
	bzero(copyOfEXTENSION, MAXEXTLTH);
	switch(loopCtr) {
	    case 0:
		strcpy(copyOfEXTENSION, EXTENSION);
		break;
	    case 1:
		strcpy(copyOfEXTENSION, EXTENSION1);
		break;
	    case 2:
		strcpy(copyOfEXTENSION, EXTENSION2);
		break;
	    default:
		strcpy(copyOfEXTENSION, EXTENSION);
		break;
	}
	sprintf(TmpBuf, ".%s", copyOfEXTENSION);
	len = strlen(TmpBuf);
	/* load up conlib with appropriate filenames */
	while (((dp = readdir(dirp)) != NULL) && connum < MAXCONFILES) {
	    if (DIRENT_NAMELEN(dp) > len) {
		nm  = dp->d_name;
		len2 = strlen(nm);
		tail = nm + len2 - len;
		if(!strcmp(tail, TmpBuf)){
		    *tail = '\0';
		    conlib[connum].path = (char *) malloc(strlen(libpaths[0]) + len2 + 
							  strlen(copyOfEXTENSION) + 3);
		    sprintf(conlib[connum].path, "%s/%s.%s", libpaths[0], nm, copyOfEXTENSION);
		    conlib[connum].confile = (char *) malloc(len2 + 1);
		    strcpy(conlib[connum].confile, nm);
		    mydbg(("conlib[%d]: %s\n", connum, conlib[connum].confile));
		    ++connum;
		}
	    }
	}
	closedir(dirp);
    }

    /* sort conlib.  0 is reserved for startup console */
    for (i = 1; i < connum; i++) {
	for (j = 1; j < i; j++) {
	    char *s, *p;
	    if (lc_strcmp(conlib[i].confile, conlib[j].confile) < 0) {
		s = conlib[i].confile;
		p = conlib[i].path;
		conlib[i].confile = conlib[j].confile;
		conlib[i].path = conlib[j].path;
		conlib[j].confile = s;
		conlib[j].path = p;
	    }
	}
    }
    /* stick into menus */
    for (i = 1; i < connum; i++) {
	sprintf(ErrTxt,"Consoles~5, %s~%d", conlib[i].confile, i);
	menuProc = proctable_DefineProc("console-read-new-console-file", ReadNewConsoleFile, consoleClass_GetInfoHack(), NULL, "read in a console file");
	mydbg(("ExtraMenus: %s\n", ErrTxt));
	menulist_AddToML(tempMenulist, ErrTxt, menuProc, conlib[i].path, 0);
    }
    mydbg(("Leaving: GetStdConsoles(0)\n"));
}

extern char *getenv();

char *GetUserPaths()
{
    static char *paths = NULL, *start = NULL;
    char *end = NULL;
    static boolean alldone = FALSE;
    char TmpBuf[MAXPATHLEN];
    int len;

    bzero(TmpBuf, MAXPATHLEN);
    mydbg(("Entering GetUserPaths\n"));

    if (alldone) return(NULL);
    
    if (paths == NULL || *paths == NULL){
	if ((paths = getenv("CONSOLELIB")) == NULL) return(NULL);
    }
    if (start == NULL || *start == NULL) start = paths;
    if ((end = index(start, ':')) == 0){
	alldone = TRUE;
	strcpy(TmpBuf, start);
	return(TmpBuf);
    }
    len = end - start;
    strncpy(TmpBuf, start, len);
    TmpBuf[len] = '\0';
    start = end;
    *start++;
    mydbg(("Leaving GetUserPaths\n"));
    return(TmpBuf);
}


void GetExtraConsoles(tempMenulist, conpath, cardTitle)
struct menulist *tempMenulist;
char *conpath;
char *cardTitle;
{
    char TmpBuf[MAXPATHLEN];
    int len, len2, i, j;
    char *nm, *tail;
    DIRENT_TYPE *dp;
    DIR *dirp;
    long start;
    struct proctable_Entry *menuProc;
    unsigned char loopCtr;
    char copyOfEXTENSION[1 + MAXEXTLTH];

    mydbg(("Entering: GetExtraConsoles\n"));
    start = connum;

    for (loopCtr = 0; loopCtr <= MAXMATCHES; ++loopCtr) {
	dirp = opendir(conpath);
	if (dirp == NULL) {
	    arrgh(("console: Can't open Console Library '%s'\n", conpath));
	    if (loopCtr == MAXMATCHES) {
		mydbg(("Leaving: GetExtraConsoles(0)\n"));
	    }
	    return;
	}
	bzero(TmpBuf, MAXPATHLEN);
	bzero(copyOfEXTENSION, MAXEXTLTH);
	switch(loopCtr) {
	    case 0:
		strcpy(copyOfEXTENSION, EXTENSION);
		break;
	    case 1:
		strcpy(copyOfEXTENSION, EXTENSION1);
		break;
	    case 2:
		strcpy(copyOfEXTENSION, EXTENSION2);
		break;
	    default:
		strcpy(copyOfEXTENSION, EXTENSION);
		break;
	}
	sprintf(TmpBuf, ".%s", copyOfEXTENSION);
	len = strlen(TmpBuf);

	/* load conlib with appropriate files */
	while (((dp = readdir(dirp)) != NULL) && connum < MAXCONFILES){
	    if (DIRENT_NAMELEN(dp) > len) {
		nm = dp->d_name;
		len2 = strlen(nm);
		tail = nm + len2 - len;
		if (!strcmp(tail, TmpBuf)) {
		    *tail = '\0';
		    conlib[connum].path = (char *) malloc(strlen(conpath) + len2 + strlen(copyOfEXTENSION) + 3);
		    sprintf(conlib[connum].path, "%s/%s.%s", conpath, nm, copyOfEXTENSION);
		    conlib[connum].confile = (char *) malloc(len2 + 1);
		    strcpy(conlib[connum].confile, nm);
		    mydbg(("conlib[%d]: %s\n", connum, conlib[connum].confile));
		    ++connum;
		}
	    }
	}
	closedir(dirp);
    }
    
    /* sort conlib */
    for (i = start; i < connum; i++){
	for (j = start; ((j < i) && (j < connum)); j++){
	    char *s, *p;
	    if (lc_strcmp(conlib[i].confile, conlib[j].confile) < 0){
		s = conlib[i].confile;
		p = conlib[i].path;
		conlib[i].confile = conlib[j].confile;
		conlib[i].path = conlib[j].path;
		conlib[j].confile = s;
		conlib[j].path = p;
	    }
	}
    }

    /* put into menus @ 6 per menucard*/
    {
	int cardnum = 1, pos = 0;
	static int cardloc = 5 + 1; /* 5 is position of Std Console Menu Card */

	for (i = start; i < connum; i++){
	    sprintf(ErrTxt, "%s(%d)~%d,%s~%d", cardTitle, cardnum, cardloc, conlib[i].confile, (pos % 6));
	    pos++;
	    if ((pos % 6) == 0){
		cardnum++;
		cardloc++;
	    }
	    menuProc = proctable_DefineProc("console-read-new-console-file", ReadNewConsoleFile, consoleClass_GetInfoHack(), NULL, "read new console file");
	    mydbg(("STD: Menu: %s\n", ErrTxt));
	    menulist_AddToML(tempMenulist, ErrTxt, menuProc, conlib[i].path, EXP_MASK);
	}
	cardloc++; /* bump for next invocation */
    }
    mydbg(("Leaving: GetExtraConsoles(1)\n"));
}

void SetStartUpConsole(path, ConFile)
char *path;
char *ConFile;
{
    mydbg(("Entering: SetStartUpConsole(%s, %s)\n", path, ConFile));
    /* These mallocs could be space leaks since there's no checking, 
         need to check for zeroth element initialization (is it NULL),
         i.e. can we compare the path to NULL safely? */
    conlib[0].path = (char *)malloc(strlen(path) + 1);
    strcpy(conlib[0].path, path);
    conlib[0].confile = (char *) malloc(strlen(ConFile) + 1);
    strcpy(conlib[0].confile, ConFile);
    mydbg(("Leaving: SetStartUpConsole(%s, %s)\n", path, ConFile));
}

#ifndef getwd
extern char *getwd();
#endif /* getwd */

FindStartUpConsole(ConFile, IsStartUp)
char *ConFile;
boolean IsStartUp;
{
    FILE *pfd = NULL;
    int i;
    char *pwd= NULL;
    char tmpbuf[MAXPATHLEN];
    boolean haspath = FALSE;
    char *confile = NULL;
    char *e = NULL;
    char tmpStr[MAXPATHLEN];
    unsigned char loopCtr;
    char copyOfEXTENSION[1 + MAXEXTLTH];

    mydbg(("Entering: FindStartUpConsole(%s, %d)\n", ConFile, IsStartUp));
    
    if ((e = index(ConFile, '/')) != NULL){ /* we can be certain of the index behaviour */
	haspath = TRUE;
	e = rindex(ConFile, '/');
	e++;
	confile = (char *)malloc(strlen(e) + 1);
	if (confile == NULL) {
	    fprintf(stderr, "FATAL ERROR: cannot allocate memory for confile; in input.FindStartUpConsole...\n");
	    fflush(stderr);
	    exit(-1);
	}
	strcpy(confile, e);
    } else {
	confile = (char *)malloc(strlen(ConFile) + 1);
	if (confile == NULL) {
	    fprintf(stderr, "FATAL ERROR: cannot allocate memory for confile; in input.FindStartUpConsole...\n");
	    fflush(stderr);
	    exit(-1);
	}
	strcpy(confile, ConFile);
    }

    if (haspath){
	if ((pfd = fopen(ConFile, "r")) != NULL){
	    /* use ConFile for full path and add the path to the path list in conlib */
	    if (IsStartUp) {
		SetStartUpConsole(ConFile, confile);
	    }
	    mydbg(("Leaving: FindStartUpConsole(0)\n"));
	    if (confile != NULL) {
		free(confile);
	    }
	    fclose(pfd);
	    return;
	}
	for (loopCtr = 0; loopCtr <= MAXMATCHES; ++loopCtr) {
	    bzero(tmpbuf, MAXPATHLEN);
	    bzero(copyOfEXTENSION, MAXEXTLTH);
	    switch(loopCtr) {
		case 0: 
		    strcpy(copyOfEXTENSION, EXTENSION);
		    break;
		case 1:
		    strcpy(copyOfEXTENSION, EXTENSION1);
		    break;
		case 2:
		    strcpy(copyOfEXTENSION, EXTENSION2);
		    break;
		default: 
		    strcpy(copyOfEXTENSION, EXTENSION);
		    break;
	    }
	    sprintf(tmpbuf, "%s.%s", ConFile, copyOfEXTENSION);
	    if ((pfd = fopen(tmpbuf, "r")) != NULL){
		if (IsStartUp) {
		    SetStartUpConsole(tmpbuf, confile);
		}
		mydbg(("Leaving: FindStartUpConsole(1)\n"));
		if (confile != NULL) {
		    free(confile);
		}
		fclose(pfd);
		return;
	    }
	}
    }

    /* else no path was specified */
    pwd = (char *)malloc(MAXPATHLEN);
    if (pwd == NULL) {
	fprintf(stderr, "FATAL ERROR: cannot allocate memory for pwd in input.FindStartUpConsole...\n");
	fflush(stderr);
	exit(-1);
    }
    bzero(tmpStr, MAXPATHLEN);
    getwd(tmpStr);
    strcpy(pwd, tmpStr);
    sprintf(tmpbuf, "%s/%s", pwd, confile);

    /* first, try it with an extension, otherwise add the extension and try again */
    if ((pfd = fopen(tmpbuf, "r")) != NULL){
	if (IsStartUp) {
	    SetStartUpConsole(tmpbuf, confile);
	}
	mydbg(("Leaving: FindStartUpConsole(2)\n"));
	if (confile != NULL) {
	    free(confile);
	}
	free(pwd);
	fclose(pfd);
	return;
    }
    for (loopCtr = 0; loopCtr <= MAXMATCHES; ++loopCtr) {
	bzero(tmpbuf, MAXPATHLEN);
	bzero(copyOfEXTENSION, MAXEXTLTH);
	switch (loopCtr) {
	    case 0:
		strcpy(copyOfEXTENSION, EXTENSION);
		break;
	    case 1:
		strcpy(copyOfEXTENSION, EXTENSION1);
		break;
	    case 2:
		strcpy(copyOfEXTENSION, EXTENSION2);
		break;
	    default:
		strcpy(copyOfEXTENSION, EXTENSION);
		break;
	}
	sprintf(tmpbuf, "%s/%s.%s", pwd, confile, copyOfEXTENSION);
	if ((pfd = fopen(tmpbuf, "r")) != NULL){
	    if (IsStartUp) {
		SetStartUpConsole(tmpbuf, confile);
	    }
	    mydbg(("Leaving: FindStartUpConsole(3)\n"));
	    if (confile != NULL) {
		free(confile);
	    }
	    if (pwd != NULL) {
		free(pwd);
	    }
	    fclose(pfd);
	    return;
	}
    }

    /* the path failed for the pwd, try the paths in the library */
    for (i = 0; i < libnum && libpaths[i] != NULL; i++){
	/* first, try it with an extension, otherwise add the extension and try again */
	bzero(tmpbuf, MAXPATHLEN);
	sprintf(tmpbuf, "%s/%s", libpaths[i], confile);
	if ((pfd = fopen(tmpbuf, "r")) != NULL){
	    if (IsStartUp) {
		SetStartUpConsole(tmpbuf, confile);
	    }
	    mydbg(("Leaving: FindStartUpConsole(4)\n"));
	    if (confile != NULL) {
		free(confile);
	    }
	    if (pwd != NULL) {
		free(pwd);
	    }
	    fclose(pfd);
	    return;
	}
	for (loopCtr = 0; loopCtr <= MAXMATCHES; ++loopCtr) {
	    bzero(tmpbuf, MAXPATHLEN);
	    bzero(copyOfEXTENSION, MAXEXTLTH);
	    switch (loopCtr) {
		case 0:
		    strcpy(copyOfEXTENSION, EXTENSION);
		    break;
		case 1:
		    strcpy(copyOfEXTENSION, EXTENSION1);
		    break;
		case 2:
		    strcpy(copyOfEXTENSION, EXTENSION2);
		    break;
		default:
		    strcpy(copyOfEXTENSION, EXTENSION);
		    break;
	    }
	    sprintf(tmpbuf, "%s/%s.%s", libpaths[i], confile, copyOfEXTENSION);
	    if ((pfd = fopen(tmpbuf, "r")) != NULL){
		if (IsStartUp) {
		    SetStartUpConsole(tmpbuf, confile);
		}
		mydbg(("Leaving: FindStartUpConsole(5)\n"));
		if (confile != NULL) {
		    free(confile);
		}
		if (pwd != NULL) {
		    free(pwd);
		}
		fclose(pfd);
		return;
	    }
	}
    }
    mydbg(("Leaving: FindStartUpConsole(6)\n"));
    if (confile != NULL) {
	free(confile);
    }
    if (pwd != NULL) {
	free(pwd);
    }
    /* perhaps this should be boolean - but if the file can't be opened in setup.c then it will prompt the user */
}


PrepareStdMenus(IsStartup, stdMenulist, ClassInfo)
boolean IsStartup;
struct menulist **stdMenulist;
struct classinfo *ClassInfo;
{
    struct menulist *tempMenulist;
    int num;

    mydbg(("Entering: PrepareStdMenus\n"));
    if (!IsStartup) {
        return;
    }
    SetConsoleLib();
    FindStartUpConsole(ConFile, IsStartup);

    tempMenulist = menulist_New();

    GetStdItems(tempMenulist);
    if (BasicLib >= 0) GetStdConsoles(tempMenulist);
    if (LocalLib >= 0) GetExtraConsoles(tempMenulist, libpaths[LocalLib], "Local Consoles");
    for (num = UserLib; num < libnum; ++num) {
	char tmp[25];
	sprintf(tmp, "CONSOLELIB-%d", num - UserLib + 1);
	GetExtraConsoles(tempMenulist, libpaths[num], tmp);
    }
    *stdMenulist = tempMenulist;
    mydbg(("Leaving: PrepareStdMenus\n"));
}



struct menulist *PrepareUserMenus(self, ClassInfo)
struct consoleClass *self;
struct classinfo *ClassInfo;
{
    struct menulist *tempMenulist;
    struct proctable_Entry *menuProc;
    int     i;

    mydbg(("entering: PrepareUserMenus\n"));
    tempMenulist = menulist_Create(self);
    if (IntrnlVars[0].InUse && IntrnlVars[0].turnon) {
        if (IntrnlVars[0].Value) {
            sprintf(ErrTxt, "%s", IntrnlVars[0].turnoff);
            menuProc = proctable_DefineProc("console-toggle-var", TogVar, ClassInfo, NULL, "dummy.");
            menulist_AddToML(tempMenulist, ErrTxt, menuProc, 0, 0);
        }
        else {
            sprintf(ErrTxt, "%s", IntrnlVars[0].turnon);
            menuProc = proctable_DefineProc("console-toggle-var", TogVar, ClassInfo, NULL, "dummy.");
            menulist_AddToML(tempMenulist, ErrTxt, menuProc, 0, 0);
        }
    }
    for (i = 1; i <= NUMINTERNALVARIABLES; ++i) {
        IntVarCount[i] = i; 
        if (IntrnlVars[i].InUse && IntrnlVars[i].turnon) {
            sprintf(ErrTxt, "%s", IntrnlVars[i].turnon);
            menuProc = proctable_DefineProc("console-toggle-var", TogVar, ClassInfo, NULL, "dummy.");
            menulist_AddToML(tempMenulist, ErrTxt, menuProc, IntVarCount[i], 0);
        }
    }
    return(tempMenulist);
}
