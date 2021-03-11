static char *clockob_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/clockob.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
#include <time.h>
#include <clockob.eh>
#include <observe.ih>
#include <contimer.ih>

static struct contimer *clockob_contimer;
static char *Days[] = {"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
static char *Months[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
static struct tm *LatestLocalTime;

static CheckTime(dummy)
long dummy;  /* should just be clockob_contimer, ignored here */
{
    long clock;
    
    clock = time(0);
    LatestLocalTime = localtime(&clock);
}

boolean clockob__InitializeClass(ci)
struct classinfo *ci;
{
    clockob_contimer = contimer_New();
    contimer_SetInterval(clockob_contimer, 1000); /* 1 second */
    contimer_SetDataCollectionProc(clockob_contimer, CheckTime);
    CheckTime((long) clockob_contimer); /* initialize time */
    return(TRUE);
}

boolean clockob__InitializeObject(ci, self)
struct classinfo *ci;
struct clockob *self;
{
    contimer_AddObserver(clockob_contimer, self);
    clockob_SetClockPart(self, CP_MIN);
    return(TRUE);
}

void clockob__FinalizeObject(c, self)
struct classheader *c;
struct clockob *self;
{
    contimer_RemoveObserver(clockob_contimer, self);
}

void clockob__ObservedChanged(self, ct, code)
struct clockob *self;
struct contimer *ct;
long code;
{
    if (code == observable_OBJECTDESTROYED) {
	clockob_contimer = NULL; /* eliminate pointer to it */
	clockob_Destroy(self); /* can't go on without a timer */
	return;
    }
    switch(self->clockpart) {
	case CP_SEC:
	    clockob_SetNumval(self, LatestLocalTime->tm_sec);
	    break;
	case CP_MIN:
	    clockob_SetNumval(self, LatestLocalTime->tm_min);
	    break;
	case CP_HR:
	    if (LatestLocalTime->tm_hour > 12) {
		clockob_SetNumval(self, LatestLocalTime->tm_hour
				  - 12);
	    } else {
		clockob_SetNumval(self, LatestLocalTime->tm_hour);
	    }
	    break;
	case CP_HRMIL:
	    clockob_SetNumval(self, LatestLocalTime->tm_hour);
	    break;
	case CP_MDAY:
	    clockob_SetNumval(self, LatestLocalTime->tm_mday);
	    break;
	case CP_WDAY:
	    clockob_SetNumval(self, LatestLocalTime->tm_wday);
	    clockob_SetStrval(self,
		Days[LatestLocalTime->tm_wday]);
	    break;
	case CP_MON:
	    clockob_SetNumval(self, LatestLocalTime->tm_mon);
	    clockob_SetStrval(self,
		Months[LatestLocalTime->tm_mon]);
	    break;
	case CP_YEAR:
	    clockob_SetNumval(self,
		LatestLocalTime->tm_year + 1900);
	    break;
	case CP_YDAY:
	    clockob_SetNumval(self, LatestLocalTime->tm_yday);
	    break;
    }
    clockob_NotifyObservers(self, observable_OBJECTCHANGED);
}

void clockob__SetClockPart(self, part)
struct clockob *self;
int part;
{
    self->clockpart = part;
}

void
clockob__WriteState(self, fp)
struct clockob *self;
FILE *fp;
{
    fprintf(fp, "*a %d\n", self->clockpart);
    super_WriteState(self, fp);
}

void
clockob__HandleDataLine(self, line)
struct clockob *self;
char *line;
{
    if (*line == '*' && *(line+1) == 'a') {
	clockob_SetClockPart(self, atoi(line+3));
    } else {
	super_HandleDataLine(self, line);
    }
}

void clockob__GetStringToDisplay(self, buf, len, IsClick)
struct clockob *self;
char *buf;
int len;
boolean IsClick;
{
    char *dt = clockob_GetDisplayTemplate(self);

    if (dt == NULL || *dt == '!' || IsClick) {
	int hr;
	if (LatestLocalTime->tm_hour > 12) {
	    hr = LatestLocalTime->tm_hour - 12;
	} else {
	    hr = LatestLocalTime->tm_hour;
	}
	sprintf(buf, "%d:%02d:%02d %cM, %s, %s %d, %d", hr,
		LatestLocalTime->tm_min,
		LatestLocalTime->tm_sec,
		(LatestLocalTime->tm_hour > 11) ? 'P' : 'A',
		Days[LatestLocalTime->tm_wday],
		Months[LatestLocalTime->tm_mon],
		LatestLocalTime->tm_mday,
		LatestLocalTime->tm_year + 1900);
    } else {
	super_GetStringToDisplay(self, buf, len, IsClick);
    }
}
