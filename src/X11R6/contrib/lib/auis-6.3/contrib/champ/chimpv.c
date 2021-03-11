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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/chimpv.c,v 1.4 1993/07/07 17:32:49 rr2b Exp $";
#endif

#include "class.h"
#include "scroll.ih"
#include "chlistv.ih"
#include "enode.ih"
#include "enodev.ih"
#include "chimp.ih"
#include "chimpv.eh"

boolean chimpview__InitializeObject(c, self)
struct classheader *c;
struct chimpview *self;
{
    struct scroll *s = scroll_New();

    self->env = enodeview_New();
    self->lv = chlistview_New();
    if (!self->lv || !s || !self->env) return(FALSE);
    enodeview_SetChimpview(self->env, self);
    scroll_SetView(s, self->lv);
    chimpview_SetUp(self, s, self->env, 50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);
    return(TRUE);
}

void chimpview__LinkTree(self, parent)
struct chimpview *self;
struct view *parent;
{
    super_LinkTree(self, parent);
    if(self->env) enodeview_LinkTree(self->env, self);
    if(self->lv) chlistview_LinkTree(self->lv, self);
}

void chimpview__SetDataObject(self, c)
struct chimpview *self;
struct chimp *c;
{
    chlistview_SetDataObject(self->lv, c);
    enodeview_SetDataObject(self->env, c->en);
}
