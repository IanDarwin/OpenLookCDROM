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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/monthv.c,v 1.8 1993/05/05 19:49:43 susan Exp $";
#endif

#include <andrewos.h>	/* time.h */
#include <class.h>
#include <month.ih>
#include <monthv.eh>
#include <fontdesc.ih>
#include <graphic.ih>
#include <champ.ih>
#include <message.ih>
#include <text.ih>
#include <textv.ih>
#include <im.ih>
#include <style.ih>

static struct style *daystyle;

boolean monthview__InitializeClass(c)
struct classinfo *c;
{
    champ_ReadDatesFromChampPath(NULL);
    daystyle = style_New();
    style_AddNewFontFace(daystyle, fontdesc_Bold);
    return(TRUE);
}

boolean monthview__InitializeObject(c, self)
struct classinfo *c;
struct monthview *self;
{
    self->tv = NULL;
    self->skippedatstart = 0;
    self->mymonth = self->myyear = -1;
    self->t = text_New();
    if (self->t == NULL) return (FALSE);
    self->AnnounceArraySize = 0;
    return(TRUE);
}

void monthview__FinalizeObject(c, self)
struct classinfo *c;
struct monthview *self;
{
    if (self->t) text_Destroy(self->t);
}

void monthview__SetTextview(self, tv)
struct monthview *self;
struct textview *tv;
{
    self->tv = tv;
    if(self->tv && monthview_GetIM(self))
	textview_LinkTree(self->tv, self);
}

static char *DayAbbrevs[] = {"M", "Tu", "W", "Th", "F", "Sa", "Su"};
static int RawMonthLengths[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static char *MonthNames[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
static char *Weekdays[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
static char *DayStrs[] = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"};

static MonthLength(yr, mon)
int yr, mon;
{
    if (mon != 1) return(RawMonthLengths[mon]);
    if (yr%4 != 0 || (yr%100 == 0 && yr%400 != 0)) return(28);
    return(29);
}

static void LogEvent(en, self)
struct eventnode *en;
struct monthview *self;
{
    int len, slen;

    len = text_GetLength(self->t);
    slen = strlen(en->event);
    text_AlwaysInsertCharacters(self->t, len, en->event, slen);
    len += slen;
    text_AlwaysInsertCharacters(self->t, len, "\n", 1);
}

void monthview__ResetMonth(self, ForceRedraw)
struct monthview *self;
boolean ForceRedraw;
{
    struct tm MyTime, *FullTime;
    int dayct, i, len, slen, mlen;
    struct month *mon;
    long clock;
    char MyString[500];

    bzero(&MyTime, sizeof(struct tm));
    mon = (struct month *) monthview_GetDataObject(self);
    self->mymonth = MyTime.tm_mon = month_GetMonth(mon);
    self->myyear = MyTime.tm_year = month_GetYear(mon);
    MyTime.tm_mday = 1;
    for (dayct = 0, i=0; i<self->mymonth; ++i) {
	dayct += MonthLength(self->myyear, i);
    }
    MyTime.tm_yday = dayct;
    clock = gtime(&MyTime);
    FullTime = gmtime(&clock);
    self->skippedatstart = FullTime->tm_wday - 1;
    if (self->skippedatstart == -1) self->skippedatstart = 6;
    text_Clear(self->t);
    mlen = MonthLength(self->myyear, self->mymonth);
    for (i=0; i<mlen; ++i) {
	champ_ClearAllFlaggedEvents();
	self->EventCt[i] = champ_FlagEventsMatchingDate(FullTime);
	len = text_GetLength(self->t);
	self->textpositions[i] = len;
	if (self->EventCt[i] > 0) {
	    sprintf(MyString, "%s %s %d, %d\n",	Weekdays[FullTime->tm_wday], MonthNames[self->mymonth], FullTime->tm_mday, 1900+FullTime->tm_year);
	    slen = strlen(MyString);
	    text_AlwaysInsertCharacters(self->t, len, MyString, slen);
	    text_AlwaysAddStyle(self->t, len, slen-1, daystyle);
	    champ_IterateFlaggedEvents(LogEvent, self);
	}
	bcopy(FullTime, &self->FullTimes[i], sizeof(struct tm));
	champ_IncrementDate(FullTime);
    }
    if (self->tv) {
	int top;

	clock = time(0);
	FullTime = localtime(&clock);
	if (FullTime->tm_year == self->myyear && FullTime->tm_mon == self->mymonth) {
	    top = self->textpositions[FullTime->tm_mday - 1];
	} else {
	    top = 0;
	}
	monthview_GrabTextview(self, ForceRedraw);
	textview_FrameDot(self->tv, -1);
	textview_SetTopPosition(self->tv, top);
	textview_SetDotPosition(self->tv, top);
    }
}

void monthview__FullUpdate(self, type, left, top, width, height)
struct monthview *self;
enum view_UpdateType type;
long left;
long top;
long width;
long height;
{
    struct month *mon;
    static struct fontdesc *plainfont = NULL, *boldfont = NULL;
    int xcenter, x, y, i, j, startday, highlight;
    struct rectangle Rect;
    char MyString[150], *StrToUse;

    if((type == view_LastPartialRedraw) || (type == view_FullRedraw)) {
	mon = (struct month *) monthview_GetDataObject(self);
	if (self->mymonth != month_GetMonth(mon) || self->myyear !=month_GetYear(mon)) {
	    monthview_ResetMonth(self, (self->mymonth != -1));
	}
	if (!plainfont) {
	    plainfont = fontdesc_Create("andy", fontdesc_Plain, 12);
	}
	if (!boldfont) {
	    boldfont = fontdesc_Create("andy", fontdesc_Bold, 12);
	}
	startday = -self->skippedatstart;
	monthview_SetTransferMode(self, graphic_COPY);
	monthview_GetLogicalBounds(self, &Rect);
	monthview_SetFont(self, plainfont); 
	xcenter = Rect.left + (Rect.width/2);
	y = Rect.top + (Rect.height/16);
	monthview_MoveTo(self, xcenter, y);
	sprintf(MyString, "%s %d", MonthNames[self->mymonth], self->FullTimes[1].tm_year+1900);
	monthview_DrawString(self, MyString, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
	monthview_FillTrapezoid(self, Rect.left+25, Rect.top, 0, Rect.left+5, Rect.top+8, 20, monthview_BlackPattern(self));
	monthview_FillTrapezoid(self, Rect.left+5, Rect.top+8, 20, Rect.left+25, Rect.top+16, 0, monthview_BlackPattern(self));
	monthview_FillTrapezoid(self, Rect.left+Rect.width - 25, Rect.top, 0, Rect.left+Rect.width-25, Rect.top+8, 20, monthview_BlackPattern(self));
	monthview_FillTrapezoid(self, Rect.left+Rect.width-25, Rect.top+8, 20, Rect.left+Rect.width-25, Rect.top+16, 0, monthview_BlackPattern(self));
	for (i = -1; i<6; ++i) {
	    y += Rect.height/8;
	    x = Rect.left + (Rect.width/14);
	    for (j=0; j<7; ++j, ++startday) {
		highlight = 0;
		if (i < 0) {
		    StrToUse = DayAbbrevs[j];
		    --startday;
		} else if (startday <0 || startday >= MonthLength(self->myyear, self->mymonth)) {
		    StrToUse =  "   ";
		} else {
		    StrToUse = DayStrs[startday];
		    if (self->EventCt[startday] > 0) {
			highlight = 1;
			monthview_SetFont(self, boldfont);
			if (self->EventCt[startday] > 2) {
			    monthview_FillRectSize(self, x-8, y-6, 18, 14, monthview_BlackPattern(self));
			    monthview_SetTransferMode(self, graphic_WHITE);
			} else if (self->EventCt[startday] > 1) {
			    monthview_FillRectSize(self, x-8, y-6, 18, 14, monthview_GrayPattern(self, 3, 10));
			}
		    }
		}
		monthview_MoveTo(self, x, y);
		monthview_DrawString(self, StrToUse, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
		if (highlight) {
		    monthview_SetTransferMode(self, graphic_COPY);
		    monthview_SetFont(self, plainfont);
		}
		x += Rect.width/7;
	    }
	}
    }	
}

void monthview__Update(self)
struct monthview *self;
{
    struct rectangle Rect;

    monthview_GetLogicalBounds(self, &Rect);
    monthview_SetTransferMode(self, graphic_COPY);
    monthview_FillRect(self, &Rect, monthview_WhitePattern(self));
    monthview_FullUpdate(self, view_FullRedraw, Rect.left, Rect.top, Rect.width, Rect.height);
}

static void ClearAnnouncements(self)
struct monthview *self;
{
    int result;

    self->AnnounceArray[self->AnnounceArraySize] = "Continue";
    self->AnnounceArray[self->AnnounceArraySize+1] = NULL;
    message_MultipleChoiceQuestion(self, 50, "Scheduled events:", self->AnnounceArraySize, &result, self->AnnounceArray, NULL);
    self->AnnounceArraySize = 0;
}

static void AnnounceEvent(en, self)
struct eventnode *en;
struct monthview *self;
{
    if (self->AnnounceArraySize > 8) {
	ClearAnnouncements(self);
    }
    self->AnnounceArray[self->AnnounceArraySize++] = en->event;
/*    message_DisplayString(self, 90, en->event); */
}


struct view *monthview__Hit(self, action, x, y, numberOfClicks)
struct monthview *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    int row, column, mday, tpos = 0;
    struct rectangle Rect;
    char Msg[250];

    if (action != view_LeftDown && action != view_RightDown) return((struct view *) self);
    monthview_GetLogicalBounds(self, &Rect);
    row = (y-Rect.top)/(Rect.height/8);
    column = (x-Rect.left)/(Rect.width/7);
    if (row == 0) {
	if (x< (Rect.left + 25)) {
	    ChangeMonth(self, -1);
	} else if (x > (Rect.left + Rect.width -25)) {
	    ChangeMonth(self, 1);
	} else {
	    int i, total = 0;

	    for (i = 0; i<MonthLength(self->myyear, self->mymonth); ++i) {
		total += self->EventCt[i];
	    }
	    sprintf(Msg, "There are %d events scheduled in %s, %d", total, MonthNames[self->mymonth], 1900+self->myyear);
	    message_DisplayString(self, 10, Msg);
	}
    } else if (row == 1) {
	message_DisplayString(self, 10, "These are the days of our lives.");
    } else {
	mday = ((row-2) * 7) + column - self->skippedatstart;
	if (mday < 0 || mday >= MonthLength(self->myyear, self->mymonth)) {
	    message_DisplayString(self, 10, "The days we are allotted are few.");
	} else {
	    tpos = self->textpositions[mday];
	    if (self->EventCt[mday] > 0) {
		if (!self->tv){
		    champ_ClearAllFlaggedEvents();
		    champ_FlagEventsMatchingDate(&self->FullTimes[mday]);
		    champ_IterateFlaggedEvents(AnnounceEvent, self);
		    ClearAnnouncements(self);
		}
	    } else {
		sprintf(Msg, "No events scheduled on %s %d, %d\n", MonthNames[self->mymonth], mday+1, 1900+self->myyear);
		message_DisplayString(self, 10, Msg);
	    }
	}
    }
    if (self->tv) {
	monthview_GrabTextview(self, FALSE);
	textview_FrameDot(self->tv, -1);
	textview_SetTopPosition(self->tv, tpos);
    }
    return((struct view *) self);
}


enum view_DSattributes monthview__DesiredSize(self, width, height, pass, dWidth, dheight)
struct monthview *self;
long width;
long height;
enum view_DSpass pass;
long *dWidth;
long *dheight;
{
    *dWidth = 168;
    *dheight = 120;
    return(view_WidthFlexible | view_HeightFlexible);
}

static ChangeMonth(self, change)
struct monthview *self;
int change;
{
    int m, y;
    struct month *mon;

    mon = (struct month *) monthview_GetDataObject(self);
    m = month_GetMonth(mon);
    y = month_GetYear(mon);
    m += change;
    while (m > 11) {
	m -= 12;
	y += 1;
    }
    while (m < 0) {
	m += 12;
	y -= 1;
    }
    month_SetMonthAndYear(mon, m, y);
}

void monthview__GrabTextview(self, ForceRedraw)
struct monthview *self;
boolean ForceRedraw;
{
    if (self->tv && self->t){
	if ((struct dataobject *) self->t != ((struct view *) self->tv)->dataobject) {
	    textview_SetDataObject(self->tv, self->t);
	    ForceRedraw = TRUE;
	}
	if (ForceRedraw) {
	    struct rectangle Rect;

	    textview_GetLogicalBounds(self->tv, &Rect);
	    if (Rect.height > 0 || Rect.width > 0) {
		textview_FullUpdate(self->tv, view_FullRedraw, Rect.left, Rect.top, Rect.width, Rect.height);
	    }
	}
    }
}

void monthview__LinkTree(self, parent)
struct monthview *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if(self->tv && monthview_GetIM(self))
	textview_LinkTree(self->tv, self);
}
