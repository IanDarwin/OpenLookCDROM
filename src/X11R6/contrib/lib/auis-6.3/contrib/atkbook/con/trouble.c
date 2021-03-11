static char *trouble_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/trouble.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

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
#include <trouble.eh>
#include <conob.ih>
#include <observe.ih>
#include <contimer.ih>
#include <statsob.ih>
#include <getstats.h>

#define MAXWORRIES 25
struct worry {
    struct conob *co;
    int TooMuch;
};
struct worry Worries[MAXWORRIES];
static int NumWorries = 0, NumProblems = 0;
static struct contimer *trouble_contimer = NULL;

static CheckTrouble();

boolean trouble__InitializeClass(ci)
struct classinfo *ci;
{
    struct statsob *go;

    go = statsob_New();
    statsob_SetStatPart(go, VM);
    Worries[0].co = (struct conob *) go;
    Worries[0].TooMuch = 95;

    go = statsob_New();
    statsob_SetStatPart(go, DISK1);
    Worries[1].co = (struct conob *) go;
    Worries[1].TooMuch = 95;

    NumWorries = 2;

    trouble_contimer = contimer_New();
    contimer_SetInterval(trouble_contimer, 1000); /* 1 second */
    contimer_SetDataCollectionProc(trouble_contimer,
				    CheckTrouble);
    return(TRUE);
}

static CheckTrouble(dummy)
long dummy;  /* should just be trouble_contimer, ignored here */
{
    int i;

    NumProblems = 0;
    for (i=0; i<NumWorries; ++i) {
	if (Worries[i].co->numval >= Worries[i].TooMuch) {
	    ++NumProblems;
	}
    }
}

boolean trouble__InitializeObject(ci, self)
struct classinfo *ci;
struct trouble *self;
{
    contimer_AddObserver(trouble_contimer, self);
    return(TRUE);
}

void trouble__FinalizeObject(c, self)
struct classheader *c;
struct trouble *self;
{
    contimer_RemoveObserver(trouble_contimer, self);
}

void trouble__ObservedChanged(self, ct, code)
struct trouble *self;
struct contimer *ct;
long code;
{
    if (code == observable_OBJECTDESTROYED) {
	trouble_Destroy(self); /* can't go on without a timer */
	return;
    }
    trouble_SetNumval(self, NumProblems);
    trouble_NotifyObservers(self, observable_OBJECTCHANGED);
}

void trouble__GetStringToDisplay(self, buf, buflen, IsClick)
struct trouble *self;
char *buf;
int buflen;
boolean IsClick;
{
    int i, buffilled;
    boolean SawTrouble = FALSE;
    char *dt = trouble_GetDisplayTemplate(self);

    if (!IsClick && dt != NULL && *dt != '!') {
	super_GetStringToDisplay(self, buf, buflen, IsClick);
	return;
    }
    if (NumProblems <= 0) {
	strncpy(buf, "This workstation is just fine.", buflen);
	return;
    }
    if (NumProblems == 1) {
	strncpy(buf, "Trouble: ", buflen);
    } else {
	sprintf(buf, "%d troubles: ", NumProblems);
    }
    for (i=0; i<NumWorries; ++i) {
	if (Worries[i].co->numval >= Worries[i].TooMuch) {
	    buffilled = strlen(buf);
	    if (SawTrouble && (buflen-buffilled) > 2) {
		strcat(buf, "  ");
		buffilled += 2;
	    } else {
		SawTrouble = TRUE;
	    }
	    conob_GetStringToDisplay(Worries[i].co,
		buf+buffilled, buflen-buffilled, IsClick);
	}
    }
}
