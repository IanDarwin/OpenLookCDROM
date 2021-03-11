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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/enodev.c,v 1.4 1994/05/11 17:25:39 rr2b Exp $";
#endif

#include "class.h"
#include "butter.ih"
#include "butterv.ih"
#include "enode.ih"
#include "lpair.ih"
#include "message.ih"
#include "chimpv.ih"
#include "chimp.ih"
#include "chlistv.ih"
#include "enodev.eh"

static char *WkDays[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Any Day", NULL};
static char *months[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Any Month", NULL};
static char *HebrewMonths[] = {"Tishri", "Heshvan", "Kislev", "Tevet", "Shvat", "First Adar", "Second Adar", "Nisan", "Iyyar", "Sivan", "Tammuz", "Av", "Elul", "Any Month", NULL};
static char *DefaultString[] = {"calendar system", "year", "month", "date", "day of week", "week", "hour", "minute", "Add Event", "Delete Event"};
static char *WeekStrs[] = {"First Week in Month", "Second Week in Month", "Third Week in Month", "Fourth Week in Month", "Fifth Week in Month", "Last Week in Month", "Any Week", NULL};
static char *LandmarkStrs[] = {"First Sunday of Advent", "First Sunday after Christmas", "First Sunday after Epiphany", "Easter", NULL};
static char *SysChoices[] = {"Gregorian", "Hebrew", "Ecclesiastical", NULL};
static char *BooleanChoices[] = {"Yes", "No", NULL};

static ButtHit(self, buttcode, butter, action)
struct enodeview *self;
int buttcode;
struct butter *butter;
enum view_MouseAction action;
{
    long result, pos, len;
    struct eventnode *en;
    struct enode *enode;
    char buf[1500], *dbuf;

    if (action == view_LeftUp || action == view_RightUp) {
	enode = (struct enode *) enodeview_GetDataObject(self);
	if (!enode) {
	    message_DisplayString(self, 10, "No event is currently on display.");
	    return;
	}
	if (buttcode == BUTT_ADD) {
	    struct eventnode *adden;
	    
	    if (message_AskForString(self, 75, "Describe the event: ", NULL, buf, sizeof(buf)) < 0) return;
	    adden = (struct eventnode *) malloc(sizeof(struct eventnode));
	    if (!adden) {
		message_DisplayString(self, 10, "Out of memory!");
		return;
	    }
	    adden->event = malloc(1+strlen(buf));
	    if (!adden->event) {
		message_DisplayString(self, 10, "Out of memory!");
		return;
	    }
	    strcpy(adden->event, buf);
	    adden->flagged = 0;
	    adden->next = NULL;
	    adden->ds.calsys = CALSYS_GREGORIAN;
	    adden->ds.sys.gd.year = -1;
	    adden->ds.sys.gd.month = -1;
	    adden->ds.sys.gd.day = -1;
	    adden->ds.sys.gd.hour = -1;
	    adden->ds.sys.gd.min = -1;
	    adden->ds.sys.gd.wkday = -1;
	    adden->ds.sys.gd.wkdayselector = -1;
	    chimp_AddNew(enode->mychimp, adden);
	    enode_SetEvent(enode, adden);
	    chlistview_ActivateItem(self->mychimpview->lv, chimp_GetLength(enode->mychimp) - 1);
	    message_DisplayString(self, 10, "Added new event to end of list.");
	    pos = chimp_GetLength(enode->mychimp);
	    chlistview_FrameDot(self->mychimpview->lv, pos);
	    return;
	}
	en = enode_GetEvent(enode);
	if (!en) {
	    message_DisplayString(self, 10, "No event is currently on display.");
	    return;
	}
	if (buttcode == BUTT_DEL) {
	    pos = chlistview_GetDotPosition(self->mychimpview->lv);
	    len = chlistview_GetDotLength(self->mychimpview->lv);
	    dbuf = malloc(len);
	    if (!dbuf) {
		message_DisplayString(self, 10, "Out of memory!");
		return;
	    }
	    chimp_CopySubString(enode->mychimp, pos, len-1, dbuf, FALSE);
	    sprintf(buf, "Really delete event `%s'? ", dbuf);
	    if (message_MultipleChoiceQuestion(self, 50, buf, 1, &result, BooleanChoices, NULL) < 0) return;
	    if (result != 0) return;
	    chimp_DeleteItem(enode->mychimp, dbuf);
	    free(dbuf);
	    message_DisplayString(self, 10, "Deleted event as requested.");
	    return;
	}
	switch(en->ds.calsys) {
	    case CALSYS_HEBREW:
		switch(buttcode) {
		    case BUTT_SYS:
			if (message_MultipleChoiceQuestion(self, 50, "What calendar system do you want to use for this event? ", 1, &result, SysChoices, NULL) < 0) return;
			if (result == 0) en->ds.calsys = CALSYS_GREGORIAN;
			if (result == 1) en->ds.calsys = CALSYS_HEBREW;
			if (result == 2) en->ds.calsys = CALSYS_ECCLESIASTICAL;
			break;
		    case BUTT_YEAR:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens every year? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.hd.year = -1;
			} else {
			    if (message_AskForString(self, 50, "What year does it happen in? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 5730 || result > 5780) {
				message_DisplayString(self, 25, "Valid answers are 5730 to 5780.");
				return;
			    } else {
				en->ds.sys.hd.year = result;
			    }
			}
			break;
		    case BUTT_MON:
			result = en->ds.sys.hd.month;
			if (result < 0) result = 13;
			if (message_MultipleChoiceQuestion(self, 50, "In what month does this event happen? ", result, &result, HebrewMonths, NULL) < 0) return;
			if (result >= 14 || result < 0) result = -1;
			en->ds.sys.hd.month = result;
			break;
		    case BUTT_DAY:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens every day? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.hd.day = -1;
			} else {
			    if (message_AskForString(self, 50, "On what day does it happen? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result <= 0 || result > 38) {
				message_DisplayString(self, 25, "Valid answers are 1 to 38 (a high max, for date wrapping).");
				return;
			    } else {
				en->ds.sys.hd.day = result;
			    }
			}
			break;
		    default:
			message_DisplayString(self, 10, "This field is not used by the Hebrew calendar.");
		}
		break;
	    case CALSYS_GREGORIAN:
		switch(buttcode) {
		    case BUTT_SYS:
			if (message_MultipleChoiceQuestion(self, 50, "What calendar system do you want to use for this event? ", 0, &result, SysChoices, NULL) < 0) return;
			if (result == 0) en->ds.calsys = CALSYS_GREGORIAN;
			if (result == 1) en->ds.calsys = CALSYS_HEBREW;
			if (result == 2) en->ds.calsys = CALSYS_ECCLESIASTICAL;
			break;
		    case BUTT_YEAR:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens every year? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.gd.year = -1;
			} else {
			    if (message_AskForString(self, 50, "What year does it happen in? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 1900 || result > 2100) {
				message_DisplayString(self, 25, "Valid answers are 1900 to 2100.");
				return;
			    } else {
				en->ds.sys.gd.year = result - 1900;
			    }
			}
			break;
		    case BUTT_MON:
			result = en->ds.sys.gd.month;
			if (result < 0) result = 12;
			if (message_MultipleChoiceQuestion(self, 50, "In what month does this event happen? ", result, &result, months, NULL) < 0) return;
			if (result >= 12 || result < 0) result = -1;
			en->ds.sys.gd.month = result;
			break;
		    case BUTT_DAY:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens every day? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.gd.day = -1;
			} else {
			    if (message_AskForString(self, 50, "On what day does it happen? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result <= 0 || result > 31) {
				message_DisplayString(self, 25, "Valid answers are 1 to 31.");
				return;
			    } else {
				en->ds.sys.gd.day = result;
			    }
			}
			break;
		    case BUTT_WKDAY:
			result = en->ds.sys.gd.wkday;
			if (result < 0) result = 7;
			if (message_MultipleChoiceQuestion(self, 50, "On what day does this event happen? ", result, &result, WkDays, NULL) < 0) return;
			if (result >= 7 || result < 0) result = -1;
			en->ds.sys.gd.wkday = result;
			break;
		    case BUTT_WKDAYSELECT:
			result = en->ds.sys.gd.wkdayselector;
			if (result > 5) result = 5;
			if (result < 0) result = 6;
			if (message_MultipleChoiceQuestion(self, 50, "In what month does this event happen? ", result, &result, WeekStrs, NULL) < 0) return;
			if (result >= 6 || result < 0) result = -1;
			if (result == 5) result = 99;
			en->ds.sys.gd.wkdayselector = result;
			break;
		    case BUTT_HR:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens during every hour? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.gd.hour = -1;
			} else {
			    if (message_AskForString(self, 50, "In what hour does it happen? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 0 || result >= 24) {
				message_DisplayString(self, 25, "Valid answers are 0 to 23.");
				return;
			    } else {
				en->ds.sys.gd.hour = result;
			    }
			}
			break;
		    case BUTT_MIN:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens during every minute? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.gd.min = -1;
			} else {
			    if (message_AskForString(self, 50, "In what minute does it happen? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 0 || result >= 60) {
				message_DisplayString(self, 25, "Valid answers are 0 to 59.");
				return;
			    } else {
				en->ds.sys.gd.min = result;
			    }
			}
			break;
		    default:
			message_DisplayString(self, 10, "Unrecognized button code.");
			break;
		}
		break;
	    case CALSYS_ECCLESIASTICAL:
		switch(buttcode) {
		    case BUTT_SYS:
			if (message_MultipleChoiceQuestion(self, 50, "What calendar system do you want to use for this event? ", 0, &result, SysChoices, NULL) < 0) return;
			if (result == 0) en->ds.calsys = CALSYS_GREGORIAN;
			if (result == 1) en->ds.calsys = CALSYS_HEBREW;
			if (result == 2) en->ds.calsys = CALSYS_ECCLESIASTICAL;
			break;
		    case BUTT_YEAR:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens every year? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.ed.year = -1;
			} else {
			    if (message_AskForString(self, 50, "What year does it happen in? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 1900 || result > 2100) {
				message_DisplayString(self, 25, "Valid answers are 1900 to 2100.");
				return;
			    } else {
				en->ds.sys.ed.year = result - 1900;
			    }
			}
			break;
		    case BUTT_MON:/* landmark */
			result = en->ds.sys.ed.landmark;
			if (result < 0) result = 4;
			if (message_MultipleChoiceQuestion(self, 50, "With what landmark is this event associated? ", result, &result, LandmarkStrs, NULL) < 0) return;
			if (result >= 4 || result < 0) result = -2;
			en->ds.sys.ed.landmark = result + 1;
			break;
		    case BUTT_DAY:/* offset from landmark */
			if (message_AskForString(self, 50, "How many days is this event offset from the landmark? ", NULL, buf, sizeof(buf)) < 0) return;
			result = atoi(buf);
			if (result <-100 || result > 250) {
			    message_DisplayString(self, 25, "Valid answers are between -100 and 250.");
			    return;
			} else {
			    en->ds.sys.ed.offset = result;
			}
			break;
		    case BUTT_HR:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens during every hour? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.ed.hour = -1;
			} else {
			    if (message_AskForString(self, 50, "In what hour does it happen? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 0 || result >= 24) {
				message_DisplayString(self, 25, "Valid answers are 0 to 23.");
				return;
			    } else {
				en->ds.sys.ed.hour = result;
			    }
			}
			break;
		    case BUTT_MIN:
			if (message_MultipleChoiceQuestion(self, 50, "Is this an event that happens during every minute? ", 0, &result, BooleanChoices, NULL) < 0) return;
			if (result == 0) {
			    en->ds.sys.ed.min = -1;
			} else {
			    if (message_AskForString(self, 50, "In what minute does it happen? ", NULL, buf, sizeof(buf)) < 0) return;
			    result = atoi(buf);
			    if (result < 0 || result >= 60) {
				message_DisplayString(self, 25, "Valid answers are 0 to 59.");
				return;
			    } else {
				en->ds.sys.ed.min = result;
			    }
			}
			break;
		    default:
			message_DisplayString(self, 10, "Unrecognized button code.");
			break;
		}
		break;
	    default:
		message_DisplayString(self, 50, "Sorry; chimp so far only knows about Gregorian, Hebrew, and Christian liturgical dates.");
		break;
	}
	enode_NotifyObservers(enode, 0);
	chimp_SetModified(enode->mychimp);
    }
}



boolean enodeview__InitializeObject(c, self)
struct classheader *c;
struct enodeview *self;
{
    struct butterview *buttviews[NUMBUTTS];
    struct lpair *lps[NUMBUTTS-1];
    int i;
    
    self->mychimpview = NULL;
    for (i=0; i<NUMBUTTS; ++i) {
	self->butts[i] = butter_New();
	buttviews[i] = butterview_New();
	butterview_SetDataObject(buttviews[i], self->butts[i]);
	butter_SetRocks(self->butts[i], self, i);
	butter_SetHitFunction(self->butts[i], ButtHit);
    }
    ResetButterTexts(self);
    lps[0] = lpair_New();
    lpair_SetUp(lps[0], buttviews[0], buttviews[1], 50, lpair_PERCENTAGE, lpair_VERTICAL, FALSE);
    lps[1] = lpair_New();
    lpair_SetUp(lps[1], buttviews[2], buttviews[3], 50, lpair_PERCENTAGE, lpair_VERTICAL, FALSE);
    lps[2] = lpair_New();
    lpair_SetUp(lps[2], buttviews[4], buttviews[5], 50, lpair_PERCENTAGE, lpair_VERTICAL, FALSE);
    lps[3] = lpair_New();
    lpair_SetUp(lps[3], buttviews[6], buttviews[7], 50, lpair_PERCENTAGE, lpair_VERTICAL, FALSE);
    lps[4] = lpair_New();
    lpair_SetUp(lps[4], lps[0], lps[1], 50, lpair_PERCENTAGE, lpair_HORIZONTAL, FALSE);
    lps[5] = lpair_New();
    lpair_SetUp(lps[5], lps[2], lps[3], 50, lpair_PERCENTAGE, lpair_HORIZONTAL, FALSE);

    lps[6] = lpair_New();
    lpair_SetUp(lps[6], buttviews[8], buttviews[9], 50, lpair_PERCENTAGE, lpair_VERTICAL, FALSE);
    lps[7] = lpair_New();
    lpair_SetUp(lps[7], lps[4], lps[5], 50, lpair_PERCENTAGE, lpair_HORIZONTAL, FALSE);

    enodeview_SetUp(self, lps[6], lps[7], 40, lpair_TOPFIXED, lpair_HORIZONTAL, FALSE);

    return(TRUE);
}

void enodeview__ObservedChanged(self, changed, value)
struct enodeview *self;
struct enode *changed;
int value;
{
    ResetButterTexts(self);
}

ResetButterTexts(self)
struct enodeview *self;
{
    char Buf[50], *mystr;
    int i, j;
    struct eventnode *en = NULL;
    struct enode *enode;

    enode = (struct enode *) enodeview_GetDataObject(self);
    if (enode) en = enode_GetEvent(enode);
    if (!enode || !en) {
	for (i=0; i<NUMBUTTS; ++i) {
	    butter_SetText(self->butts[i], DefaultString[i]);
	}
	return;
    }
    if (en->ds.calsys != CALSYS_GREGORIAN) {
	for (i=0; i<NUMBUTTS; ++i) {
	    butter_SetText(self->butts[i], DefaultString[i]);
	}
	switch (en->ds.calsys) {
	    case CALSYS_HEBREW:
		butter_SetText(self->butts[BUTT_SYS], "Hebrew date");
		break;
	    case CALSYS_ECCLESIASTICAL:
		butter_SetText(self->butts[BUTT_SYS], "Ecclesiastical date");
		break;
	    default:
		butter_SetText(self->butts[BUTT_SYS], "Non-Gregorian date");
		return;	/* OTHER CASES FALL THROUGH */
	}
    } else {
	butter_SetText(self->butts[BUTT_SYS], "Gregorian date");
    }

    switch (en->ds.calsys) {
	case CALSYS_GREGORIAN:
	    i = en->ds.sys.gd.year;
	    j = i + 1900;
	    break;
	case CALSYS_HEBREW:
	    i = en->ds.sys.hd.year;
	    j = i;
	    break;
	case CALSYS_ECCLESIASTICAL:
	    i = en->ds.sys.ed.year;
	    j = i + 1900;
	    break;
    }
    if (i < 0) {
	strcpy(Buf, "Any year");
    } else {
	sprintf(Buf, "Year: %d", j);
    }
    butter_SetText(self->butts[BUTT_YEAR], Buf);

    switch (en->ds.calsys) {
	case CALSYS_GREGORIAN:
	    butter_SetText(self->butts[BUTT_MON], (en->ds.sys.gd.month < 0) ? months[12] : months[en->ds.sys.gd.month]);
	    break;
	case CALSYS_HEBREW:
	    butter_SetText(self->butts[BUTT_MON], (en->ds.sys.hd.month < 0) ? HebrewMonths[13] : HebrewMonths[en->ds.sys.hd.month]);
	    break;
	case CALSYS_ECCLESIASTICAL:
	    butter_SetText(self->butts[BUTT_MON], (en->ds.sys.ed.landmark <= 0) ? "No Landmark" : LandmarkStrs[en->ds.sys.ed.landmark - 1]);
	    break;
    }

    if (en->ds.calsys != CALSYS_ECCLESIASTICAL) {
	switch (en->ds.calsys) {
	    case CALSYS_GREGORIAN:
		i = en->ds.sys.gd.day;
		break;
	    case CALSYS_HEBREW:
		i = en->ds.sys.hd.day;
		break;
	}
	if (i < 0) {
	    strcpy(Buf, "Any date");
	} else {
	    sprintf(Buf, "Date: %d", i);
	}
    } else {
	sprintf(Buf, "Day offset: %d", en->ds.sys.ed.offset);
    }
    butter_SetText(self->butts[BUTT_DAY], Buf);

    if (en->ds.calsys == CALSYS_GREGORIAN) {
	butter_SetText(self->butts[BUTT_WKDAY], (en->ds.sys.gd.wkday < 0) ? "Any day of week" : WkDays[en->ds.sys.gd.wkday]);
	if (en->ds.sys.gd.wkdayselector < 0) {
	    mystr = "Any week";
	} else if (en->ds.sys.gd.wkdayselector > 5) {
	    mystr = WeekStrs[5];
	} else {
	    mystr = WeekStrs[en->ds.sys.gd.wkdayselector];
	}
	butter_SetText(self->butts[BUTT_WKDAYSELECT], mystr);
    } else {
	butter_SetText(self->butts[BUTT_WKDAY], "Unused field");
	butter_SetText(self->butts[BUTT_WKDAYSELECT], "Unused field");
    }

    if (en->ds.calsys == CALSYS_GREGORIAN || en->ds.calsys == CALSYS_ECCLESIASTICAL) {
	i = (en->ds.calsys == CALSYS_GREGORIAN ? en->ds.sys.gd.hour : en->ds.sys.ed.hour);
	if (i < 0) {
	    strcpy(Buf, "Any hour");
	} else {
	    sprintf(Buf, "Hour of day: %d", i);
	}
	butter_SetText(self->butts[BUTT_HR], Buf);

	i = (en->ds.calsys == CALSYS_GREGORIAN ? en->ds.sys.gd.min : en->ds.sys.ed.min);
	if (i < 0) {
	    strcpy(Buf, "Any minute");
	} else {
	    sprintf(Buf, "minute of hour: %d", i);
	}
	butter_SetText(self->butts[BUTT_MIN], Buf);
    } else {
	butter_SetText(self->butts[BUTT_HR], "Unused field");
	butter_SetText(self->butts[BUTT_MIN], "Unused field");
    }

    butter_SetText(self->butts[BUTT_ADD], DefaultString[BUTT_ADD]);
    butter_SetText(self->butts[BUTT_DEL], DefaultString[BUTT_DEL]);
}

void enodeview__SetChimpview(self, cv)
struct enodeview *self;
struct chimpview *cv;
{
    self->mychimpview = cv;
}
