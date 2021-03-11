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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/rofftext/RCS/roffstyl.c,v 2.14 1992/12/15 21:40:52 rr2b R6tape $";
#endif


 

/* styles */

#include <class.h>
#include <text.ih>
#include <style.ih>
#include <stylesht.ih>
#include <envrment.ih>
#include <hash.ih>
#include <rofftext.ih>

/*
 *  fixed by cch@mtgzx.att.com 1/10/90
 *  program calling environment_InsertStyle with union environmentcontents 
 *  where it should be passing struct *style.
 *  This is a no-no on SPARC.
 */

WriteText(self)
struct rofftext *self;
{
    rofftext_Write(self,stdout,(long)self,1);
}


/* change style with id='id' to new style 'st'
 * returns id of new style
 *
 */

ChangeStyle(self,id,st)
struct rofftext *self;
int id;
char *st;
{
    int l, newid;

    DEBUG(1, (stderr,"ChangeStyle: <current = %d>, changing %d to (%s)\n",self->stack->ID,id,(st?st:"")));

    if (id == 0) {
        return (BeginStyle(self,st));
    }

    for (l=self->stack->level;(id != self->stack->ID)&&(self->stack->level >= 0);l--) {
        self->tempstack++;
        self->tempstack->pos = self->stack->pos;
        self->tempstack->style = self->stack->env->data.style;
        self->tempstack->level = (self->tempstack-1)->level+1;
        self->tempstack->ID = self->stack->ID;
        CloseStyle(self);
    }
    CloseStyle(self);
    newid = BeginStyle(self,st);
    for(l=self->tempstack->level;l>=0;l--) {
        self->stack++;
        self->stack->pos = self->pos;
	self->stack->env = environment_InsertStyle((self->stack-1)->env, self->pos - (self->stack-1)->pos, self->tempstack->style, TRUE);
        self->stack->level = (self->stack-1)->level+1;
        self->stack->ID = self->tempstack->ID;
        self->tempstack--;
    }
    return newid;
    
}

/* close innermost style */

CloseStyle(self)
struct rofftext *self;
{
    DEBUG(1, (stderr,"Closing style %d (%d to %d, length %d)\n", self->stack->ID, self->stack->pos, self->pos, (self->pos - self->stack->pos)));

    if (self->stack->level < 0) {
        DEBUG(1, (stderr,"rofftext: WARNING: tried to close bottom-level style\n"));
        return;
    }

    if (self->pos > self->stack->pos) {
        environment_SetLength(self->stack->env,self->pos - self->stack->pos);
        environment_SetStyle(self->stack->env,FALSE,FALSE);
    }
    else {
        DEBUG(1, (stderr,"(Removing the 0-length style)\n"));
        environment_Delete(self->stack->env);
    }
    self->stack--;

}

/* begin a style.  Caller must hang onto returned ID to close style */

BeginStyle(self,st)
struct rofftext *self;
char *st;
{
    struct style *style;
    
    if (st == NULL)
        return 0;

    style = stylesheet_Find(self->text->styleSheet,st);

    if (style == NULL) {
        DEBUG(1, (stderr,"BeginStyle: opening non-existent style (%s)\n",st));
        return 0; /* null style */
    }
    self->stack++;
    self->stack->pos = self->pos;
    self->stack->env = environment_InsertStyle((self->stack-1)->env, self->pos - (self->stack-1)->pos, style, TRUE);
DEBUG(4, (stderr, "Inserting style %s at pos %d offset %d\n", st, self->pos, self->pos - (self->stack-1)->pos));	
    self->stack->level = (self->stack-1)->level+1;
    self->stack->ID = self->styleID++;

    if (self->styleID == 0)   /* make sure we don't get a '0' style id */
        self->styleID = 1;

    DEBUG(1, (stderr,"BeginStyle: opening style %d (%s)\n",self->stack->ID,st));
    return self->stack->ID;
}

/* ends a style and cleans up */

EndStyle(self,ID)
struct rofftext *self;
int ID;
{
    int l;

    DEBUG(1, (stderr,"EndStyle: current %d, closing %d\n",self->stack->ID,ID));

    if (ID == 0)
        return; /* null style */

    for (l=self->stack->level;(ID != self->stack->ID)&&(self->stack->level >= 0);l--) {
        self->tempstack++;
        self->tempstack->pos = self->stack->pos;
        self->tempstack->style = self->stack->env->data.style;
        self->tempstack->level = (self->tempstack-1)->level+1;
        self->tempstack->ID = self->stack->ID;
        CloseStyle(self);
    }
    CloseStyle(self);
    for(l=self->tempstack->level;l>=0;l--) {
        self->stack++;
        self->stack->pos = self->pos;
	self->stack->env = environment_InsertStyle((self->stack-1)->env, self->pos - (self->stack-1)->pos, self->tempstack->style, TRUE);
        self->stack->level = (self->stack-1)->level+1;
        self->stack->ID = self->tempstack->ID;
        self->tempstack--;
    }

} 

CloseAllStyles(self)
struct rofftext *self;
{
    while(self->stack->level >= 0)
        CloseStyle(self);
}
