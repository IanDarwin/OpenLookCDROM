/* ********************************************************************** *\
 *         Copyright MIT 1990 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/tabs.c,v 1.12 1993/10/25 17:13:28 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <style.ih>
#include <textv.ih>
#include <txtstvec.h>
#include <tabs.eh>

static struct tabs *DefaultTabs = NULL;

int
FindPrevTab(tabs, pos)
struct tabs *tabs;
long pos;
/* Post: returns...
 *	-1 if no tabs
 *	n if found
 *	NumTabs if not found in list
 */
{
    register int i;

    if (tabs->number == 0)
        return -1;

    for(i = 0; i < tabs->number; i++)
        if (pos < tabs->Positions[i])
            return i - 1;

    return tabs->number;
}

/* This is to take account of Andrew lying about the size of fonts */
#define	RealityHack(x) ((x)*14)


void
tabs__OutputTroff(self, indent, file)
struct tabs *self;
long indent;
FILE *file;
/* Output all tabs past indent, with a tab at indent */
{
    int i;

    if (indent < 0) {
	fprintf(file, "'ta %dp", -indent);
    }
    else{
	fprintf(file, "'ta");
    }

    for (i = 0; i < self->number; i++)
	if (self->Positions[i] > indent)
	    switch(self->Types[i]) {
		case style_LeftAligned:
		    fprintf(file, " %dp", self->Positions[i] - indent);
		    break;
		case style_RightAligned:
		    fprintf(file, " %dpR", self->Positions[i] - indent);
		    break;
		case style_CenteredOnTab:
		    fprintf(file, " %dpC", self->Positions[i] - indent);
		    break;
		default:
		    ; /* Ignoring all others for now... XXX */
	    }
    fprintf(file, "\n");
}


int
tabs__Different(a, b)
struct tabs *a, *b;
/* returns 1 if different, 0 if same */
{
    if (a->number == b->number) {
	int i;
	for (i = 0; i < a->number; i++)
	    if (!(a->Positions[i] == b->Positions[i] &&
		  a->Types[i] == b->Types[i]))
		return 1;
    } else
	return 1;
    return 0;
}


struct tabs *
tabs__Delete(self, n)
struct tabs *self;
int n;
/* delete the n'th tab */
/* precondition: n represents a valid tab: 0 <= n < CurNumTabs */
/* post: the original tabs are destroyed, if links > 1 */
{
    register struct tabs *nt;

    register int num = self->number;

    if (num == 1) {
	/* Last tab in this list */
	/* Before we throw away the list, is it someone elses? */
	if (self->links > 1) {
	    nt = tabs_New();
	    nt->links = 1;
	    self->links--;
	    return nt;
	} else {
	    free(self->Positions);
	    free(self->Types);
	    self->Positions = NULL;
	    self->Types = NULL;
	    self->number = 0;
	    return self;
	}
    } else {
	/* create new tablist..
	 * copy tabs from 0..n-1 into newlist[0..n-1] (That's n elements);
	 * copy	tabs from n+1..NumTabs into newlist[n..NumTabs-1]   
	 * free	the old	tab lists   
	 * dec(NumTabs)	
	 */ 
	nt = tabs_New();
	nt->Positions = (long *) malloc(sizeof(long) * (num-1));
	nt->Types = (long *) malloc(sizeof(long) * (num-1));
    	if (n) {
	    bcopy(self->Positions, nt->Positions, sizeof(long) * n);
	    bcopy(self->Types, nt->Types, sizeof(long) * n);
	}
	if (n != num - 1) {
	    bcopy(&(self->Positions[n+1]), &(nt->Positions[n]), sizeof(long) * (num-n-1));
	    bcopy(&(self->Types[n+1]), &(nt->Types[n]), sizeof(long) * (num-n-1));
	}

	tabs_Death(self);

	nt->links = 1;
	nt->number = num-1;
	return nt;
    }
}


struct tabs *
tabs__Add(self, pos, op)
struct tabs *self;
long pos;
enum style_TabAlignment op;
{
    /* Add tab into the list */
    /* The original lists ARE DESTROYED */
    register struct tabs *nt;
    register int PrevTab;
    register int num = self->number;

    /* Find out what tab is before the destination spot */
    PrevTab = FindPrevTab(self, pos);
    /* If there is already a tab at pos, then let's just overwrite it */
    if (PrevTab >= 0 && self->Positions[PrevTab] == pos)
	/* There is one at this spot */ {
	if (self->Types[PrevTab] == (long) op) {
	    /* But no change... */
	    return self;
	}

	if (self->links > 1) { /* But someone else is using this list, so we need to copy */
	    nt = tabs_New();
	    nt->Positions = (long *) malloc(sizeof(long) * num);
	    nt->Types = (long *) malloc(sizeof(long) * num);
	    bcopy(self->Positions, nt->Positions, sizeof(long)*num);
	    bcopy(self->Types, nt->Types, sizeof(long)*num);
	    nt->Types[PrevTab] = (long) op;
	    self->links--;
	    nt->links = 1;
	    nt->number = num;
	    return nt;
	} else {
	    self->Types[PrevTab] = (long) op;
	    return self;
	}
    } else {
	/* newpos is where in the array to place the new tabstop - it
	 * is similar to PrevTab, but takes into account the case
	 * where PrevTab indicates the new pos is outside the old array bounds
	 */
	register int newpos = PrevTab;
	if (newpos < num)
	    newpos++;

	/* malloc new arrays */
	nt = tabs_New();
	nt->Positions = (long *) malloc(sizeof(long) * (num+1));
	nt->Types = (long *) malloc(sizeof(long) * (num+1));

	/* Copy all the old tabs before this new one */
	if (PrevTab >= 0) {
	    bcopy(self->Positions, nt->Positions, sizeof(long) * (newpos));
	    bcopy(self->Types, nt->Types, sizeof(long) * (newpos));
	}

	/* Put in the new one */
	nt->Positions[newpos] = pos;
	nt->Types[newpos] = (long) op;
	
	/* Put in all the old tabs after the new one */
	if (PrevTab != num) {
	    bcopy(&(self->Positions[newpos]), &(nt->Positions[newpos+1]), sizeof(long) * (num-newpos));
	    bcopy(&(self->Types[newpos]), &(nt->Types[newpos+1]), sizeof(long) * (num-newpos));
	}
	/* And get rid off all the old tabs */
	tabs_Death(self);
	nt->number = num+1;
	nt->links = 1;
	return nt;
    }
}

struct tabs *
tabs__Clear(self)
struct tabs *self;
/* 
 * Post: if links == 1, then list is destroyed, else links is kept
 */
{
    if (self->number == 0)
	/* Tabs are already cleared */
	return self;

    if (self->links == 1) {
	free(self->Positions);
	free(self->Types);
	self->number = 0;
	self->Positions = NULL;
	self->Types = NULL;
	return self;
    } else {
	struct tabs *nt = tabs_New();
	nt->links = 1;
	return nt;
    }
}


struct tabs *
tabs__Create(classID)
struct classheader *classID;
{
    register long x;
    register int i;
    /*
     * We are using:
     * Default Tabs, every 1/2 inch, for the first 10inches;
     * equiv: every 36 pts, from 36 .. 720
     */
    
    if (DefaultTabs) {
	DefaultTabs->links++;
	return DefaultTabs;
    }

    DefaultTabs = tabs_New();
    DefaultTabs->links = 2;
    DefaultTabs->Positions = (long *) malloc(sizeof(long)*19);
    DefaultTabs->Types = (long *) malloc(sizeof(long)*19);

    for (x = 36,i=0; x < 720; x+=36, i++) {
	DefaultTabs->Positions[i] = x;
	DefaultTabs->Types[i] = (long) style_LeftAligned;
    }
    DefaultTabs->number = i;
    
    return DefaultTabs;
}

boolean
tabs__InitializeObject(classID, self)
struct classheader *classID;
struct tabs *self;
{
    self->Positions = NULL;
    self->Types = NULL;
    self->links = 0;
    self->number = 0;
    return TRUE;
}

void
tabs__FinalizeObject(classID, self)
struct classheader *classID;
struct tabs *self;
{
    if (self->Positions) free(self->Positions);
    if (self->Types) free(self->Types);
    self->Positions=NULL;
    self->Types=NULL;
}

void
tabs__Death(classID, self)
struct classheader *classID;
struct tabs *self;
{
    if (--self->links > 0)
	return;
    else if(self==DefaultTabs) {
	fprintf(stderr, "tabs: WARNING: would have destroyed default tabs object!\n");
#ifdef DEBUG
	abort();
#endif	
	self->links=1;
	return;
    } else tabs_Destroy(self); 
}


struct tabs *
tabs__ApplyChange(self, tabChange)
struct tabs *self;
struct tabentry *tabChange;
{
    long Pos;	    /* Position for current tab */
    int PrevTab;   /* Tab location at or immediately before proposed tab */
    struct tabs *tabs = self;

    Pos = tabChange->DotLocation;
    /* DotLocation is in the units style_RawDots... (calculated by style.c)
     */

    switch(tabChange->TabOpcode) {
        case style_LeftAligned:
        case style_RightAligned:
        case style_CenteredOnTab:
        case style_CenteredBetweenTab:
	case style_CharAligned:
	    tabs = tabs_Add(self, Pos, tabChange->TabOpcode);
	    break;
        case style_TabDivide:
	    /* First get rid of old tabs */
	    tabs = tabs_Clear(self);
	    /* Calculate where new tab stops should go */
	    /* XXX - what is TabDivide supposed to *DO* ???!! */
	    break;
        case style_TabClear:
	    PrevTab = FindPrevTab(self, Pos);
	    if (PrevTab >= 0 && tabChange->DotLocation == self->Positions[PrevTab])
		/* There is a tab at this postion, so we get rid of it */
		tabs = tabs_Delete(self, PrevTab);
	    /* If there is no tab at the precise position specified, then
	     * this operation is ignored. Perhaps it should find another
	     * tab within a certain tolerance?
	     */
	    break;
	case style_AllClear:
	    tabs = tabs_Clear(self);
	    break;
	default:
	    /* Unknown -- should return error */
	    return tabs;
    }
    return tabs;
}

struct tabs *
tabs__Copy(self)
struct tabs *self;
{
    self->links++;
    return self;
}
