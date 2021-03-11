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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/conclass.c,v 2.13 1992/12/15 21:30:21 rr2b R6tape $";
#endif


 

/* 
 * console -- User-configurable instrument console for Andrew
 * -originally written for use under the Andrew Window Manager
 *
 * -This program collects all the monitoring information you might
 * -ever want, and lets you configure it in a single instrument
 * -console in a very flexible way.  It is intended to supersede
 * -clock, gvmstat, mariner, wdf, ttyscript, and probably a bunch
 * -of other stuff as well.
 *
 */

#include <class.h>
#include <conclass.eh>
#include <menulist.ih>
#include <event.ih>
#include <im.ih>
#include <environ.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <scroll.ih>
#include <graphic.ih>
#include <cursor.ih>
#include <text.ih>
#include <console.h>
#include <signal.h>

extern RedrawPrompt();
extern DoQuit();

struct classinfo *consoleClass_GetInfoHack()  {
    return &consoleClass_classinfo;
}
static struct menulist *stdMenulist;

boolean consoleClass__InitializeObject(classID, self)
struct classheader *classID;
struct consoleClass *self;
{
    mydbg(("entering: consoleClass__InitializeObject"));
    self->stdMenulist = menulist_DuplicateML(stdMenulist, self);
#if 0
    self->altMenulist = NULL;
#endif /* 0 */
    self->haveInputFocus = FALSE;
    self->interactive = FALSE;
    self->menuMask = 0;
    signal(SIGTERM, DoQuit);
    return TRUE;
}

void consoleClass__FullUpdate(self, type, left, top, width, height)
    struct consoleClass *self;
    enum view_UpdateType type;
    long left;
    long top;
    long width;
    long height;
{
    static boolean firstTime = TRUE;

    mydbg(("entering: consoleClass__FullUpdate\n"));
    consoleClass_SetTransferMode(self,graphic_BLACK);
    if (firstTime)  {
	im_SetTitle(consoleClass_GetIM(self), TitleFromFile (ConFile, TRUE));
	firstTime = FALSE;
	InitDisplay(self);
	InitErrorMonitoring(self,TRUE); 
	InitializeInstruments(self);
	WakeUp(self);
    }
    if (!PauseEnqueuedEvents && !RingingAlarm){
	RedrawDisplays(self);
    }
    else{
	RedrawPrompt(self);
    }
}

void consoleClass__WantUpdate(self, requestor)
struct consoleClass *self;
struct view *requestor;
{
    mydbg(("entering: consoleClass__WantUpdate\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
	super_WantUpdate(self, requestor);
    }
}  


void consoleClass__Update(self)
struct consoleClass *self;
{
    struct display *mydisp;

    mydbg(("entering: consoleClass__Update\n"));
    for (mydisp = VeryFirstDisplay; mydisp; mydisp = mydisp->NextOfAllDisplays) {
	if (mydisp->WhatToDisplay->NeedsUpdate) {
	    UpdateDisplay(self, mydisp);
	}
    }
    for (mydisp = VeryFirstDisplay; mydisp; mydisp = mydisp->NextOfAllDisplays) {
	mydisp->WhatToDisplay->NeedsUpdate = FALSE;
    }
}

void consoleClass__PostMenus(self, menu)
    struct consoleClass *self;
    struct menulist *menu;
{
    mydbg(("entering: consoleClass__PostMenus\n"));
    menulist_UnchainML(self->stdMenulist, 0);
    if (menu != self->stdMenulist)
	menulist_ChainBeforeML(self->stdMenulist, menu, 0);
    super_PostMenus(self, self->stdMenulist);
}

extern struct display *FindInstrument(); 
extern SetStandardCursor(); 

struct view *consoleClass__Hit(self, action, x, y, numberOfClicks)
struct consoleClass *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    struct view *ret = NULL;
    struct RegionLog   *whichlog;
    struct display *mydisp;
    static int    MovingX = -1;
    static long LastX,LastY;

    mydbg(("entering: consoleClass__Hit\n"));
    if (((mydisp = FindInstrument(self, x, y)) != NULL)&& (mydisp->AssociatedLogView != NULL) && (MovingX == -1)){
	long newx = x - mydisp->Xmin;
	long newy = y - mydisp->Ymin;
	ret = scroll_Hit(mydisp->ScrollLogView, action, newx, newy, numberOfClicks);
    }
    else{
	if(! self->haveInputFocus){
	    consoleClass_WantInputFocus(self, self);
	}
	switch (action) {
	    case view_LeftDown:
		if ((mydisp = FindInstrument(self, x, y)) == NULL) {
		    return(NULL);
		}
		if (mydisp->ClickWhenInvisible || (mydisp->WhatToDisplay->Value <= mydisp->Ceiling && mydisp->WhatToDisplay->Value >= mydisp->Threshhold)) {
		    whichlog = mydisp->AssociatedLog;
		    AddToLog(self, mydisp, TRUE, whichlog, FALSE);
		}
		break;
	    case view_LeftUp: 
		break;
	    case view_RightDown: 
		MovingX = CheckMovingX(self, x, y);
		LastX = x;
		LastY = y;
		break;
	    case view_RightUp:
		if (MovingX != -1) {
		    ResizeDisplay(self,x, y, LastX, LastY, MovingX);
		    MovingX = -1;
		}
		SetStandardCursor(self, Cursor_Arrow);
		break;
	    case view_LeftMovement:
	    case view_RightMovement:
		break;
	    default: 
		ReportInternalError(self, "console: Unrecognized mouse std; this should never happen");
		return(NULL);
	}
	ret = (struct view *) self;
    }
    return( ret );
}


void consoleClass__ReceiveInputFocus(self)
    struct consoleClass *self;
{
    mydbg(("entering: consoleClass__ReceiveInputFocus\n"));
    super_ReceiveInputFocus(self);
    self->haveInputFocus = TRUE;
    menulist_ChainBeforeML(self->stdMenulist, self->userMenulist, self->userMenulist);
    consoleClass_PostMenus(self, self->stdMenulist);
    consoleClass_PostKeyState(self, NULL);
}

void consoleClass__LoseInputFocus(self)
    struct consoleClass *self;
{
    mydbg(("entering: consoleClass__LoseInputFocus\n"));
    self->haveInputFocus = FALSE;
}

void SetLogFont(textObj)
struct text *textObj;
{
    char FontBuffer[50];
    long FontSize, FontStyle;
    static struct style *globalStyle = NULL;

    if (globalStyle == NULL){
	char *s = NULL;
	if (PromptFont == NULL){
	    s = environ_GetProfile("bodyfont");
	    if (!s || !*s) s = PromptFontName;
	}
	fontdesc_ExplodeFontName(s, FontBuffer, sizeof(FontBuffer), &FontStyle, &FontSize);
	PromptFont = fontdesc_Create(FontBuffer, FontStyle, FontSize);
	globalStyle = style_New();
	style_SetFontFamily(globalStyle, FontBuffer);
	style_SetFontSize(globalStyle, style_ConstantFontSize, FontSize);
	style_AddNewFontFace(globalStyle, FontStyle);
    }
    text_SetGlobalStyle(textObj, globalStyle);
}



boolean consoleClass__InitializeClass(classID)
    struct classheader *classID;
{
    mydbg(("entering: consoleClass__InitializeClass\n"));
    stdMenulist = menulist_New();
    PrepareStdMenus(TRUE, &stdMenulist, &consoleClass_classinfo);
    RegionLogs[ERRORREGIONLOG].TextLog = text_New();
    SetLogFont(RegionLogs[ERRORREGIONLOG].TextLog);
    text_SetExportEnvironments(RegionLogs[ERRORREGIONLOG].TextLog , FALSE);
    RegionLogs[ERRORREGIONLOG].WhichDatum = NULL;
    RegionLogs[ERRORREGIONLOG].ScrollReverse = FALSE;
    RegionLogs[REPORTREGIONLOG].TextLog = text_New();
    SetLogFont(RegionLogs[REPORTREGIONLOG].TextLog);
    text_SetExportEnvironments(RegionLogs[REPORTREGIONLOG].TextLog , FALSE);
    RegionLogs[REPORTREGIONLOG].WhichDatum = NULL;
    RegionLogs[REPORTREGIONLOG].ScrollReverse = FALSE;
    RegionLogs[USERREGIONLOG].TextLog = text_New();
    SetLogFont(RegionLogs[USERREGIONLOG].TextLog);
    text_SetExportEnvironments(RegionLogs[USERREGIONLOG].TextLog , FALSE);
    RegionLogs[USERREGIONLOG].WhichDatum = NULL;
    RegionLogs[USERREGIONLOG].ScrollReverse = FALSE;
    RegionLogs[SILLYREGIONLOG].TextLog = text_New();
    SetLogFont(RegionLogs[SILLYREGIONLOG].TextLog);
    text_SetExportEnvironments(RegionLogs[SILLYREGIONLOG].TextLog , FALSE);
    RegionLogs[SILLYREGIONLOG].WhichDatum = NULL;
    RegionLogs[SILLYREGIONLOG].ScrollReverse = FALSE;
    return TRUE;
}

