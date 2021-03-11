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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/chimp.c,v 1.3 1992/12/15 21:48:53 rr2b R6tape $";
#endif

#include "class.h"
#include "champ.ih"
#include "view.ih"
#include "enode.ih"
#include "chimp.eh"

boolean chimp__InitializeObject(c, self)
struct classheader *c;
struct chimp *self;
{
    self->en = enode_New();
    self->comment = NULL;

    if (!self->en) return(FALSE);
    enode_SetChimp(self->en, self);
    return(TRUE);
}

static void ChimpCallBack(en, self, action, nclicks)
struct eventnode *en;
struct chimp *self;
enum view_MouseAction action;
long nclicks;
{
    if (action == view_LeftUp || action == view_RightUp) {
	enode_SetEvent(self->en, en);
    }
}

long chimp__Read(self, fp, id)
struct chimp *self;
FILE *fp;
long id;
{
    char LineBuf[250];
    struct eventnode *en;

    chimp_SetID(self,chimp_UniqueID(self));/* change id to unique number */

    while (fgets(LineBuf, sizeof(LineBuf)-1, fp) != NULL) {
	if (strncmp(LineBuf, "\\enddata{", 9) == 0) {
	    return(dataobject_NOREADERROR);
	}
	en = champ_ReadDateIntoEventNode(LineBuf);
	if (en) {
	    /* need to add event to list */
	    chimp_AddItemToEnd(self, en->event, ChimpCallBack, en);
	} else {
	    struct comment *cm, *nextcm;

	    cm = (struct comment *) malloc(sizeof(struct comment));
	    if (cm) cm->line = malloc(1+strlen(LineBuf));
	    if (cm && cm->line) {
		strcpy(cm->line, LineBuf);
		cm->next = NULL;
		for (nextcm = self->comment; nextcm && nextcm->next; nextcm = nextcm->next) {
		    ;
		}
		if (nextcm) {
		    nextcm->next = cm;
		} else {
		    self->comment = cm;
		}
	    } else {
		printf("Throwing away: %s\n", LineBuf);
	    }		
	}
    }
    return dataobject_NOREADERROR; /* What, me worry? */
}

long chimp__Write(self, fp, id, level)
struct chimp *self;
FILE *fp;
long id;
int level;
{
    struct listitem *li = chimp_GetItemList(self);
    int numitems = chimp_GetNumItems(self);
    int i;
    struct comment *cm;

    if (id != chimp_GetWriteID(self)) {
	/* New write operation */
	chimp_SetWriteID(self, id);
	if (level>0) {
	    fprintf(fp, "\\begindata{%s,%d}\n", class_GetTypeName(self), chimp_UniqueID(self));
	}
	for (cm = self->comment; cm; cm = cm->next) {
	    fprintf(fp, "%s", cm->line); /* newline already there */
	}
	for (i=0; i<numitems; ++i) {
	    /* write out single item */
	    WriteOutEvent(fp, li[i].rock);
	}
	if (level > 0) {
	    fprintf(fp, "\\enddata{%s,%d}\n", class_GetTypeName(self), chimp_UniqueID(self));
	}
    }
    return(chimp_UniqueID(self));
}

static WriteOutEvent(fp, en)
FILE *fp;
struct eventnode *en;
{
    switch(en->ds.calsys) {
	case CALSYS_GREGORIAN:
	    fprintf(fp, "%d %d %d %d %d %d %d %d #%s\n",
		    CALSYS_GREGORIAN, en->ds.sys.gd.year,
		    en->ds.sys.gd.month, en->ds.sys.gd.day,
		    en->ds.sys.gd.wkday, en->ds.sys.gd.wkdayselector,
		    en->ds.sys.gd.hour, en->ds.sys.gd.min,
		    en->event);
	    break;
	case CALSYS_HEBREW:
	    fprintf(fp, "%d %d %d %d #%s\n",
		    CALSYS_HEBREW, en->ds.sys.hd.year,
		    en->ds.sys.hd.month, en->ds.sys.hd.day,
		    en->event);
	    break;
	case CALSYS_ECCLESIASTICAL:
	    fprintf(fp, "%d %d %d %d %d %d #%s\n",
		    CALSYS_ECCLESIASTICAL, en->ds.sys.ed.year,
		    en->ds.sys.ed.landmark, en->ds.sys.ed.offset,
		    en->ds.sys.ed.hour, en->ds.sys.ed.min,
		    en->event);
	    break;
	default:
	    fprintf(fp, "%d #%s\n", en->ds.calsys, en->event);
	    break;
    }
}

void chimp__AddNew(self, en)
struct chimp *self;
struct eventnode *en;
{
    chimp_AddItemToEnd(self, en->event, ChimpCallBack, en);
    chimp_NotifyObservers(self, 0);
}

