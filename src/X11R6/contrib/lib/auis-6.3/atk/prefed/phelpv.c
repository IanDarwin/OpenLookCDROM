/* Copyright 1992 Carnegie Mellon University. All rights Reserved.
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

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/phelpv.c,v 1.4 1992/12/14 20:51:24 rr2b R6tape $ */

#ifndef NORCSID
static char *rcsid_phelpv_c = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/phelpv.c,v 1.4 1992/12/14 20:51:24 rr2b R6tape $";
#endif 

#include <andrewos.h>
#include <class.h>

#include <message.ih>
#include <text.ih>
#include <list.ih>
#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <prefs.ih>
#include <strcache.ih>
#define SAVESTR(str) (str?strcache_SaveStr(str):NULL)
#include "phelpv.eh"


#define TEXT(s) ((struct text *)phelpv_GetDataObject(s))
#define PREFS(s) (phelpv_GetPrefs(s))

boolean phelpv__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

boolean phelpv__InitializeObject(classID, self)
struct classheader *classID;
struct phelpv *self;
{
    return TRUE;
}

void phelpv__FinalizeObject(classID, self)
struct classheader *classID;
struct phelpv *self;
{
}

struct fghrock {
    struct phelpv *self;
    char *name;
};

static boolean WarpToGroupHelp(pg, rock)
struct prefgroup *pg;
struct fghrock *rock;
{
    if(rock->name==pg->name) {
	struct phelpv *self=rock->self;
	if(pg->grouphelp==-1) {
	    message_DisplayString(self, 0, "No help on this preference category.");
	    return FALSE;
	}
	super_SetTopPosition(self, pg->grouphelp);
	super_SetDotPosition(self, pg->grouphelp);
	super_SetDotLength(self, 0);
    }
    return TRUE;
}

static void DoGroupHelp(self)
struct phelpv *self;
{
    struct text *txt=TEXT(self);
    struct environment *renv = txt->rootEnvironment;
    struct environment *env = environment_GetInnerMost(renv, phelpv_GetDotPosition(self));
    if(env != NULL && env->type==environment_Style) {
	struct stylesheet *ss=text_GetStyleSheet(TEXT(self));
	struct style *s=stylesheet_Find(ss, "groupname");
	if(s==env->data.style) {
	    struct fghrock rock;
	    char buf[1024];
	    long pos=environment_Eval(env);
	    long len=environment_GetLength(env);

	    if(len>sizeof(buf)-1) return;
	    text_CopySubString(TEXT(self), pos, len, buf, FALSE);
	    rock.self=self;
	    rock.name=SAVESTR(buf);
	    list_Enumerate(PREFS(self)->categories, WarpToGroupHelp, &rock);
	}

    }
}
void phelpv__SetDotPosition(self, pos)
struct phelpv *self;
long pos;
{
    super_SetDotPosition(self, pos);
    DoGroupHelp(self);
}

struct view *phelpv__Hit(self, action, x, y, clicks)
struct phelpv *self;
enum view_MouseAction action;
long x, y, clicks;
{
    struct view *result=super_Hit(self, action, x, y, clicks);
    if(result==(struct view *)self && action==view_LeftUp || action==view_RightUp) {
	long pos=phelpv_GetDotPosition(self);
	long len=phelpv_GetDotLength(self);
	phelpv_SetDotPosition(self, pos);
	phelpv_SetDotLength(self, len);
    }
    return result;
}
