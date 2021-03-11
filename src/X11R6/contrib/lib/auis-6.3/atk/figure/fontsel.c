/* fontsel.c - font selection inset dataobject */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
char *fontsel_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/fontsel.c,v 1.2 1992/12/14 20:44:19 rr2b R6tape $";
#endif

#include <fontsel.eh>

#include <fontdesc.ih>

static char *CopyString();

boolean fontsel__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    return TRUE;
}

boolean fontsel__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct fontsel *self;
{
    self->active = ~(unsigned long)0;

    self->family = CopyString(fontsel_default_Family);
    self->size = fontsel_default_Size;
    self->style = fontsel_default_Style;

    return TRUE;
}

void fontsel__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct fontsel *self;
{
    free(self->family);
}

void fontsel__SetStyle(self, mask)
struct fontsel *self;
long mask;
{
    self->style = mask;
    self->active |= ((unsigned long)1<<fontsel_Style);
}

void fontsel__SetSize(self, newsize)
struct fontsel *self;
short newsize;
{
    self->size = newsize;
    self->active |= ((unsigned long)1<<fontsel_Size);
}

void fontsel__SetFamily(self, newfam)
struct fontsel *self;
char *newfam;
{
    if (strcmp(newfam, self->family)) {
	free(self->family);
	self->family = CopyString(newfam);
    }
    self->active |= ((unsigned long)1<<fontsel_Family);
}

static char *CopyString(str)
char *str;
{
    char *tmp;

    if (str==NULL)
	return NULL;
    tmp = malloc(strlen(str)+1);
    if (!tmp)
	return NULL;
    strcpy(tmp, str);
    return tmp;
}

