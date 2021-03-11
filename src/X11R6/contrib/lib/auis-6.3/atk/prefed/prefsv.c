/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/prefsv.c,v 1.4 1992/12/15 21:38:20 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "prefsv.eh"

#include "prefs.ih"

#include <message.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <observe.ih>

#define DATA(self) ((struct prefs *)prefsv_GetDataObject(self))

static struct menulist *prefsvMenus=NULL;
static struct keymap *prefsvKeymap=NULL;

static void sort(pv, rock)
struct prefsv *pv;
char *rock;
{
    char buf[256];
    if((long)rock<=255) return;
    printf("sorting by %s\n",rock);
    strcpy(buf, "Sorted by ");
    switch(*rock) {
	case 'N':
	    prefs_Sort(DATA(pv), prefs_Name, TRUE);
	    strcat(buf, "name.");
	    break;
	case 'A':
	    prefs_Sort(DATA(pv), prefs_App, TRUE);
	    strcat(buf, "application.");
	    break;
	case 'G':
	    prefs_Sort(DATA(pv), prefs_Group, TRUE);
	    strcat(buf, "group.");
	    break;
	case 'O':
	    prefs_Sort(DATA(pv), prefs_Order, TRUE);
	    strcat(buf, "order.");
	    break;
	default:
	    if(strlen(rock)<200) {
		strcpy(buf, rock);
		strcat(buf, " is an invalid argument to sort.");
	    }
	    break;
    }
    prefs_NotifyObservers(DATA(pv), observable_OBJECTCHANGED);
    message_DisplayString(pv, 0, buf);
}

static struct bind_Description prefsvBindings[]={
    {
	"prefsv-sort",
	NULL,
	NULL,
	"Sort~20,By name",
	(long)"Name",
	0,
	sort,
	"Sorts the preferences. (Valid arguments are: Name, Group, App, Order)",
	"prefsv"
    },{
	"prefsv-sort",
	NULL,
	NULL,
	"Sort~20,By order",
	(long)"Order",
	0,
	sort,
	"Sorts the preferences. (Valid arguments are: Name, Group, App, Order)",
	"prefsv"
    },
    {
	"prefsv-sort",
	NULL,
	NULL,
	"Sort,By group",
	(long)"Group",
	0,
	sort,
	"Sorts the preferences. (Valid arguments are: Name, Group, App, Order)",
	"prefsv"
    },
    {
	"prefsv-sort",
	NULL,
	NULL,
	"Sort,By application",
	(long)"App",
	0,
	sort,
	"Sorts the preferences. (Valid arguments are: Name, Group, App, Order)",
	"prefsv"
    },
    {
	NULL,
	NULL,
	0,
	0,
	0,
	NULL,
	NULL,
	NULL
    }
};
    
    
boolean prefsv__InitializeClass(classID)
struct classheader *classID;
{
    prefsvMenus=menulist_New();
    prefsvKeymap=keymap_New();

    if(prefsvMenus == NULL || prefsvKeymap==NULL) {
	if(prefsvMenus != NULL) menulist_Destroy(prefsvMenus);
	if(prefsvKeymap != NULL) keymap_Destroy(prefsvKeymap);
	return FALSE;
    }
    
    bind_BindList(prefsvBindings, prefsvKeymap, prefsvMenus, &prefsv_classinfo);
    return TRUE;
}

boolean prefsv__InitializeObject(classID, self)
struct classheader *classID;
struct prefsv *self;
{
    self->ks=keystate_Create(self, prefsvKeymap);
    if(self->ks==NULL) return FALSE;

    self->menulist=menulist_DuplicateML(prefsvMenus, self);

    if(self->menulist==NULL) {
	keystate_Destroy(self->ks);
	return FALSE;
    }
    return TRUE;
}

void prefsv__FinalizeObject(classID, self)
struct classheader *classID;
struct prefsv *self;
{
    if(self->ks) keystate_Destroy(self->ks);
    if(self->menulist) menulist_Destroy(self->menulist);
}


void prefsv__PostMenus(self, ml)
struct prefsv *self;
struct menulist *ml;
{
    if(self->menulist) {
	if(ml) menulist_ChainAfterML(self->menulist, ml, ml);
	super_PostMenus(self, self->menulist);
    } else super_PostMenus(self, ml);
}

void prefsv__PostKeyState(self, ks)
struct prefsv *self;
struct keystate *ks;
{
    struct keystate *lks=self->ks;
    if(lks) {
	keystate_AddBefore(lks, ks);
	super_PostKeyState(self, lks);
    } else super_PostKeyState(self, ks);
}


