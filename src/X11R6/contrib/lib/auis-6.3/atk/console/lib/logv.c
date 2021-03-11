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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/logv.c,v 2.9 1992/12/15 21:30:21 rr2b R6tape $";
#endif


 

#include <class.h>
#include <view.ih>
#include <textv.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <logv.eh>

static struct menulist *logviewMenu;

static int logview_NoTextviewKey()
{
    return(0);
}

boolean logview__InitializeObject(classID, self)
    struct classheader *classID;
    struct logview *self;
{
    self->menu = menulist_DuplicateML(logviewMenu, self);
    return TRUE;
}

void logview__FinalizeObject(classID, self)
struct classheader *classID;
struct logview *self;
{
    if (self->menu != NULL)
	menulist_Destroy(self->menu);
}
	

void logview__LoseInputFocus(self)
    struct logview *self;
{
    logview_SetDotLength(self, 0);
    logview_PostMenus(self, NULL);
    self->header.textview.hasInputFocus = FALSE;
    logview_WantUpdate(self, self);
}

void logview__PostMenus(self, menulist)
    struct logview *self;
    struct menulist *menulist;
{
    if (logview_GetDotLength(self) > 0)  {
	menulist_SetMask(self->menu, 1);
    }
    else {
	menulist_SetMask(self->menu, 0);
    }
    super_PostMenus(self, self->menu);
}

void logview__FrameDot(self, pos)
    struct logview *self;
    long pos;
{
    struct linedesc *lastLine;
    long topPos;

    if (self->header.textview.nLines > 0)  {
	lastLine = &(self->header.textview.lines[self->header.textview.nLines - 1]);
	if (lastLine->nChars != 0 && lastLine->y + 2 * lastLine->height > logview_GetLogicalHeight(self))  {
	    topPos = logview_GetTopPosition(self);
	    topPos = logview_MoveBack(self, topPos, 0, textview_MoveByLines, 0, 0);
	    topPos = logview_MoveForward(self, topPos, 1, textview_MoveByLines, 0, 0);
	    if (self->header.textview.scroll == textview_ScrollBackward) self->header.textview.scroll = textview_MultipleScroll;
	    logview_SetTopPosition(self, topPos);
	}
    }
    super_FrameDot(self, pos);
}

boolean logview__InitializeClass(classID)
    struct classheader *classID;
{
    struct proctable_Entry *tempProc;
    struct classinfo *classInfo = &logview_classinfo;
    int (*textview_CopyCmd)();

    logviewMenu = menulist_New();

    class_Load("textview");

    if ((tempProc = proctable_Lookup("textview-copy-region")) != NULL)
        textview_CopyCmd = proctable_GetFunction(tempProc);
    else
        textview_CopyCmd = logview_NoTextviewKey;

    tempProc = proctable_DefineProc("logview-copy-region", textview_CopyCmd, classInfo, NULL, "Copy text from console log.");

    menulist_AddToML(logviewMenu, "Copy~12", tempProc, NULL, 1);
    return TRUE;
}
