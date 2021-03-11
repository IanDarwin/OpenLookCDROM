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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/rofftext/RCS/mantext.c,v 1.7 1993/11/12 22:49:35 gk5g Exp $";
#endif


/* mantext: link to rofftext -man. */

#include <andrewos.h>
#include <class.h>

#include <mark.ih>
#include <rofftext.ih>
#include <mantext.eh>

#define INITIAL 100
#define INCREMENT 1000

boolean mantext__InitializeObject(classID,self)
struct classheader *classID;
struct mantext *self;
{
    self->nLines = 0;
    self->nAlloc = INITIAL;
    self->filename[0] = '\0';
    self->lineMark = (struct mark **)malloc(self->nAlloc*sizeof(struct mark *));
    if (self->lineMark == NULL) return FALSE;
    mantext_SetLinePos(self, 0L, 0L);
    return TRUE;
}

long mantext__Read(self, file, id)
struct mantext *self;
FILE *file;
long id; {
    long tmpRetValue;
    struct rofftext *r = (struct rofftext *)self;

    /* copy the filename that was put into rofftext via SetAttributes */
    if (r->filename != NULL) strcpy(self->filename, r->filename);
    r->inputfiles = (char **)malloc(2 * sizeof(char *));
    r->inputfiles[0] = NULL;
    r->inputfiles[1] = NULL;
    r->filename = NULL;

    r->macrofile = TMACMANFILE; /* this comes from <sys>/system.h via andrewos.h */

    r->RoffType = FALSE;
    r->HelpMode = FALSE;

    tmpRetValue = super_Read(r, file, (long)r); 

    return tmpRetValue;

}

long mantext__GetLinePos(self, line)
struct mantext *self;
long line; {
    line -= 2;
    if (line > self->nLines) line = self->nLines;
    if (line < 0) line = 0;
    return mark_GetPos(self->lineMark[line]);
}

void mantext__SetLinePos(self, line, pos)
struct mantext *self;
long line;
long pos; {
    if (line < 0) return;
    if (line >= self->nAlloc) {
	self->nAlloc += INCREMENT;
	self->lineMark = (struct mark **)realloc(self->lineMark, self->nAlloc*sizeof(struct mark *));
    }
    if (self->lineMark == NULL) return;
    self->lineMark[line] = mantext_CreateMark(self, pos, 0);
    self->nLines = line;
    /*  must make sure we have not skipped some */
}


void mantext__GetFilename(self, filename)
struct mantext *self;
char *filename; {
    strcpy(filename, self->filename);
}
