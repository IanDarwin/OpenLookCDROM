static char *statsob_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/statsob.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

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
#include <statsob.eh>
#include <conob.ih>
#include <message.ih>
#include <environ.ih>
#include <im.ih>
#include <getstats.h>
#include <errno.h>

extern char *malloc();
static void HandleGetstatsInput();

struct oblink {
    struct statsob *gsob;
    struct oblink *next;
};

static struct oblink *Objects[LASTVAL+1];
static long NumValues[LASTVAL+1];
static char *StringValues[LASTVAL+1];

boolean statsob__InitializeClass(c)
struct classinfo *c;
{
    int i, vmfreq, diskfreq;
    FILE *gfp;
    char CmdBuf[1000];

    for (i=0; i<=LASTVAL; ++i) {
	Objects[i] = NULL;
	NumValues[i] = 0;
	StringValues[i] = NULL;
    }
    vmfreq = environ_GetProfileInt("vmfreq", 1); 
    diskfreq = environ_GetProfileInt("diskfreq", 60);
    sprintf(CmdBuf, "getstats %d %d %d", getuid(),
	     vmfreq, diskfreq);
    gfp = popen(CmdBuf, "r");
    if (gfp == NULL){
	message_DisplayString(NULL, 99, "popen failed");
	return(FALSE);
    }
    if (!im_AddFileHandler(gfp, HandleGetstatsInput, NULL, 1)) {
	message_DisplayString(NULL, 99,
		"im_AddFileHandler failed");
	return(FALSE);
    }
    return(TRUE);
}

boolean statsob__InitializeObject(c, self)
struct classinfo *c;
struct statsob *self;
{
    statsob_SetStatPart(self, LOADCPU);
    return(TRUE);
}

void statsob__FinalizeObject(c, self)
struct classinfo *c;
struct statsob *self;
{
    RemoveFromLinkedObjectList(self);
}

void statsob__SetStatPart(self, which)
struct statsob *self;
int which;
{
    static char *DefaultDisplayTemplates[] = {
	"<nothing>",
	"The CPU load is $%.",
	"The IO load is $%.",
	"The User load is $%.",
	"The System load is $%.",
	"The idle time is $%.",
	"$% of available virtual memory is in use.",
	"$ pages paged in.",
	"$ pages paged out.",
	"$ pages replacable",
	"$ page deficit",
	"$% of memory is active.",
	"$% of memory is free.",
	"There are $ processes in the run queue.",
	"There are $ processes blocked.",
	"There are $ processes waiting for memory.",
	"$ IO interrupts.",
	"$ system interrupts.",
	"$ swapping interrupts.",
	"$ ndstatin.",
	"$ ndstatout",
	"$ ndstaterr",
	"$% of the maximum user processes are in use.",
	"$% of the maximum possible processes are in use.",
	"$ processes are in use by someone else.",
	"Disk partition * is $% full.",
	"Disk partition * is $% full.",
	"Disk partition * is $% full.",
	"Disk partition * is $% full.",
	"Disk partition * is $% full.",
	"Disk partition * is $% full.",
	NULL
    };
    struct oblink *ol;

    if (which < 0 || which > LASTVAL) return;
    ol = (struct oblink *) malloc(sizeof(struct oblink));
    if (ol == NULL) return;
    /* get rid of old link entry */
    RemoveFromLinkedObjectList(self); 

    ol->gsob = self;
    ol->next = Objects[which];
    Objects[which] = ol;
    
    self->statpart = which;
    statsob_SetDisplayTemplate(self,
		DefaultDisplayTemplates[which]);
    statsob_SetStrval(self, StringValues[which]);
    statsob_SetNumval(self, NumValues[which]);
}

static RemoveFromLinkedObjectList(self)
struct statsob *self;
{
    struct oblink *oltmp, *olprev = NULL;

    if (self->statpart < 0 || self->statpart > LASTVAL) {
	return; /* not on any list! */
    }
    for (oltmp = Objects[self->statpart];
	  oltmp != NULL;
	  oltmp = oltmp->next) {
	if (oltmp->gsob == self) {
	    if (olprev == NULL) {
		Objects[self->statpart] = oltmp->next;
	    } else {
		olprev->next = oltmp->next;
	    }
	    free(oltmp);
	    break;
	}
	olprev = oltmp;
    }
}

void statsob__WriteState(self, fp)
struct statsob *self;
FILE *fp;
{
    fprintf(fp, "*a %d\n", self->statpart);
    super_WriteState(self, fp);
}

void statsob__HandleDataLine(self, line)
struct statsob *self;
char *line;
{
    if (*line == '*' && *(line+1) == 'a') {
	statsob_SetStatPart(self, atoi(line+3));
    } else {
	super_HandleDataLine(self, line);
    }
}

static void HandleGetstatsInput(gfp, ignored)
FILE *gfp;
long ignored;
{
    int id = 0, val = 0, num = 0;
    char dname[100], buf[200];
    struct oblink *ol;

    if (fgets(buf, sizeof(buf), gfp) == NULL) {
	if (errno != EWOULDBLOCK) {
	    message_DisplayString(NULL, 99, "Got EOF.");
	    im_RemoveFileHandler(gfp);
	    fclose(gfp);
	}
	return;
    }
    num = sscanf(buf, "%d:%d:%99s", &id, &val, dname);
    if (num < 2 || num > 3 || id < 0 || id > LASTVAL){
	message_DisplayString(NULL, 10, "Bad getstats input");
	return;
    }
    if (num == 3){ /* there was a string value */
	if (StringValues[id] != NULL) {
	    StringValues[id] = realloc(StringValues[id],
				       1+strlen(dname));
	} else {
	    StringValues[id] = malloc(1+strlen(dname));
	}
	strcpy(StringValues[id], dname);
    }
    NumValues[id] = val;
    for (ol = Objects[id]; ol != NULL; ol=ol->next) {
	statsob_SetNumval(ol->gsob, val);
	if (num == 3) {
	    statsob_SetStrval(ol->gsob, StringValues[id]);
	}
	statsob_NotifyObservers(ol->gsob,
		observable_OBJECTCHANGED);
    }
}
