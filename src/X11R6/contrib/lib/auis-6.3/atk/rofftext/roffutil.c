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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/rofftext/RCS/roffutil.c,v 2.8 1992/12/15 21:40:52 rr2b R6tape $";
#endif


 

/* utility stuff for rofftext
 *
 */

#include <class.h>
#include <glist.ih>
#include <rofftext.ih>

/* magic buffers */
BUF NewBuf()
{
    BUF b = (BUF)malloc(sizeof(struct MagicBuf));
    b->begin = b->ptr =  (char *)malloc(BUFSIZ);
    b->blocksize = BUFSIZ;
    b->end = b->begin + BUFSIZ - 1;
    b->size = BUFSIZ;
    return b;
}

FreeBuf(b)
BUF b;
{
    free(b->begin);
    free(b);
}

Add2Buf(b,c)
BUF b;
char c;
{
    char *temp;
    if (b->ptr >= b->end) {
        DEBUG(1, (stderr,"<<Bumping size of buffer %x, text=%x,p=%x,end=%x>>",b,b->begin,b->ptr,b->end));
        b->size += b->blocksize;
        temp = realloc(b->begin,b->size);
        if (temp == NULL)
            DEBUG(1, (stderr,"rofftext: HELP! Buffer couldn't grow!\n"));
        b->end = (temp + b->size - 1);
        b->ptr = (b->ptr - b->begin + temp);
        b->begin = temp;
    }
    *b->ptr++ = c;
}

/* create environments with default values */

CreateEnvirons(self)
struct rofftext *self;
{
    struct roffEnviron *e ;
    int i;

    for (i=0;i<4;i++) {
        e = self->Environs[i] = (struct roffEnviron *)malloc(sizeof(struct roffEnviron));
        e->pointSize = 10;
	e->fill	= TRUE;
        e->font = "roman";
        e->fontStyle = 0;
        e->prevFont = e->font;
        e->indent = 0;
        e->prevIndent = 0;
        e->tempIndent = 0;
        e->NextInputTrap = 0;
        e->InputTrapCmd = NULL;
        e->controlChar = '.';
        e->NBControlChar = '\'';
    }
    self->CurrentEnviron = self->Environs[0];
}

DestroyEnvirons(self)
struct rofftext *self;
{
    int i; struct roffEnviron *e;

    for(i=0;i<4;i++) {
        e = self->Environs[i];
        if(e->InputTrapCmd)
            free(e->InputTrapCmd);
        free(e);
    }
}

/* push down to environment X */

PushEnviron(self,env)
struct rofftext *self;
int env;
{
    int indent;
    DEBUG(1, (stderr,"<<<Pushing Environment>>>\n"));
    glist_Push(self->EnvironStack,self->CurrentEnviron);

    indent = self->CurrentEnviron->indent;
    EndStyle(self,self->CurrentEnviron->fontStyle);
    self->CurrentEnviron->fontStyle = 0;

    self->CurrentEnviron = self->Environs[env];

    self->CurrentEnviron->fontStyle = BeginStyle(self,self->CurrentEnviron->font);
    if (indent != self->CurrentEnviron->indent)
        SetIndent(self,self->CurrentEnviron->indent);

}

PopEnviron(self)
struct rofftext *self;
{
    int indent;
    DEBUG(1, (stderr,"<<<Popping environment>>>\n"));
    indent = self->CurrentEnviron->indent;
    EndStyle(self,self->CurrentEnviron->fontStyle);
    self->CurrentEnviron = (struct roffEnviron *)glist_Pop(self->EnvironStack);
    if (self->CurrentEnviron == NULL)
        self->CurrentEnviron = self->Environs[0];
    self->CurrentEnviron->fontStyle = BeginStyle(self,self->CurrentEnviron->font);
    if (indent != self->CurrentEnviron->indent)
        SetIndent(self,self->CurrentEnviron->indent);
}

struct diversionLevel *CreateDiversion(self,c)
struct rofftext *self;
struct diversionLevel *c;
{
    struct diversionLevel *d = (struct diversionLevel *)malloc(sizeof(struct diversionLevel));

    if (c == NULL) {
        d->NextDiversionTrap = 0;
        d->name = NULL;
        d->level = 0;
        d->NoSpaceMode = FALSE;
        d->OutputDone = 0;
	d->SnarfOutput = NULL;
    }
    else {
        d->NextDiversionTrap = c->NextDiversionTrap;
        if (c->name)
            d->name = StrDup(c->name);
        else
            d->name = NULL;
        d->level = c->level+1;
        d->NoSpaceMode = c->NoSpaceMode;
        d->OutputDone = c->OutputDone;
	d->SnarfOutput = c->SnarfOutput;

    }
    return d;
}

DestroyDiversion(self,d)
struct rofftext *self;
struct diversionLevel *d;
{
    if (d->name)
        free(d->name);
    free(d);
}

PushDiversion(self)
struct rofftext *self;
{
    glist_Push(self->DiversionStack,self->CurrentDiversion);
    self->CurrentDiversion = CreateDiversion(self,self->CurrentDiversion);
    self->v_DiversionLevel++;
}

PopDiversion(self)
struct rofftext *self;
{
    if (self->v_DiversionLevel > 0) {
        DestroyDiversion(self,self->CurrentDiversion);
        self->v_DiversionLevel--;
        self->CurrentDiversion = (struct diversionLevel *)glist_Pop(self->DiversionStack);
    }
}
