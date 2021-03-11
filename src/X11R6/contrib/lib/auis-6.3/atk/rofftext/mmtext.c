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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/rofftext/RCS/mmtext.c,v 1.6 1992/12/15 21:40:52 rr2b R6tape $";
#endif


/* ********************************************************************** *\
 *         Copyright AT&T Bell Laboratories - All Rights Reserved         *
 *        For full copyright information see:'andrew/config/COPYRITE.att' *
\* ********************************************************************** */

/* mmtext: link to rofftext -mm. */

#include <class.h>

#include <buffer.ih>
#include <environ.ih>
#include <mark.ih>
#include <rofftext.ih>
#include <text.ih>

#include <mmtext.eh>

#define INITIAL 100
#define INCREMENT 1000

boolean mmtext__InitializeObject(classID,self)
struct classheader *classID;
struct mmtext *self;
{
    self->nLines = 0;
    self->nAlloc = INITIAL;
    self->filename[0] = '\0';
    self->lineMark = (struct mark **)malloc(self->nAlloc*sizeof(struct mark *));
    if (self->lineMark == NULL) return FALSE;
    mmtext_SetLinePos(self, 0L, 0L);
    return TRUE;
}

long mmtext__Read(self, file, id)
struct mmtext *self;
FILE *file;
long id; {
    long tmpRetValue;
    struct rofftext *r = (struct rofftext *)self;
    struct buffer *buf = buffer_FindBufferByData(self);

    /* copy the filename that was put into rofftext via SetAttributes */
    if (r->filename != NULL) strcpy(self->filename, r->filename);
    r->inputfiles = (char **)malloc(2 * sizeof(char *));
    r->inputfiles[0] = NULL;
    r->inputfiles[1] = NULL;
    r->filename = NULL;

    r->macrofile = environ_AndrewDir("/lib/tmac/tmac.m");
    r->RoffType = FALSE;
    r->HelpMode = FALSE;

    tmpRetValue = super_Read(r, file, (long)r); 

    /* set read-only for buffer */
    if (buf != NULL) {
	struct text *text = (struct text *)buffer_GetData(buf);
	buffer_SetReadOnly(buf, TRUE);
	if (class_IsTypeByName(class_GetTypeName(text), "text")) {
	    text_SetReadOnly(text, TRUE);
	}
    }

    return tmpRetValue;

}

long mmtext__GetLinePos(self, line)
struct mmtext *self;
long line; {
    line -= 2;
    if (line > self->nLines) line = self->nLines;
    if (line < 0) line = 0;
    return mark_GetPos(self->lineMark[line]);
}

void mmtext__SetLinePos(self, line, pos)
struct mmtext *self;
long line;
long pos; {
    if (line < 0) return;
    if (line >= self->nAlloc) {
	self->nAlloc += INCREMENT;
	self->lineMark = (struct mark **)realloc(self->lineMark, self->nAlloc*sizeof(struct mark *));
    }
    if (self->lineMark == NULL) return;
    self->lineMark[line] = mmtext_CreateMark(self, pos, 0);
    self->nLines = line;
    /*  must make sure we have not skipped some */
}


void mmtext__GetFilename(self, filename)
struct mmtext *self;
char *filename; {
    strcpy(filename, self->filename);
}
