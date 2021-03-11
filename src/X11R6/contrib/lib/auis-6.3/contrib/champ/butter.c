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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/champ/RCS/butter.c,v 1.5 1993/05/04 01:40:19 susan Exp $";
#endif

#include "class.h"
#include "butter.eh"
#include "fontdesc.ih"
#include "graphic.ih"
#include "cursor.ih"

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

boolean
butter__InitializeObject(c, self)
struct classheader *c;
struct butter *self;
{
    self->text = NULL;
    self->mycursor=NULL;
    self->HitFunction = NULL;
    self->rockptr = NULL;
    self->rockint = 0;
    self->myfontdesc = NULL;
    self->myfontname = NULL;
    self->myfontsize = 12;
    self->myfonttype = fontdesc_Bold;
    return(TRUE);
}

void
butter__SetText(self, txt)
struct butter *self;
char *txt;
{
    if (self->text) free(self->text);
    self->text = malloc(1+strlen(txt));
    strcpy(self->text, txt);
    butter_NotifyObservers(self, 0);
}

void
butter__SetButtonFont(self, f)
struct butter *self;
struct fontdesc *f;
{
    self->myfontdesc = f;
    butter_NotifyObservers(self, 0);
}

void
butter__SetHitFunction(self, f)
struct butter *self;
int (*f)();
{
    self->HitFunction = f;
}

void
butter__SetRocks(self, r1, r2)
struct butter *self;
char *r1;
int r2;
{
    self->rockptr = r1;
    self->rockint = r2;
}

