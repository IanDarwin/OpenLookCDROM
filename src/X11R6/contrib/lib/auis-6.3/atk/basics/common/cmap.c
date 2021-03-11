/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/cmap.c,v 1.11 1994/01/11 19:11:32 rr2b Exp $";
#endif
#include <andrewos.h>
#include <color.ih>
#include <cmap.eh>

boolean
colormap__InitializeClass( classID )
    struct classheader *classID;
{
    return(TRUE);
}

boolean
colormap__InitializeObject( classID, self )
    struct classheader *classID;
    struct colormap *self;
{
    self->size = self->used = 0;
    self->colors = NULL;
    return(TRUE);
}

void
colormap__FinalizeObject( classID, self )
struct classheader *classID;
struct colormap *self;
{
    if(self->colors) {
	free(self->colors);
	self->colors=NULL;
    }
}

int
colormap__SetColor( self, color, needpixel )
    struct colormap *self;
    struct color *color;
    boolean needpixel;
{
    if(!color || self->used == self->size)
	return(-1);
    else {
	/* create new color and place in linked list */
	self->colors[self->used++] = color;
	return(0);
    }
}

int
colormap__Copy( self, source )
    struct colormap *self, *source;
{
}

int
colormap__Merge( self, other )
    struct colormap *self, *other;
{
}

struct color *
colormap__AllocColor( self, name, red, green, blue, needpixel )
    struct colormap *self;
    char *name;
    unsigned int red, green, blue;
    boolean needpixel;
{
}

int
colormap__ChangeColor( self, c )
    struct colormap *self;
    struct color *c;
{
}

struct color *
colormap__LookupColor( self, name, r, g, b, needpixel )
    struct colormap *self;
    char *name;
    unsigned int r, g, b;
    boolean needpixel;
{
}

int
colormap__SetSize( self, size)
    struct colormap *self;
    int size;
{
    self->size = size;
    if(self->colors = (struct color **) calloc(size, sizeof(struct color*))) {
	return(0);
    }
    else
	return(-1);
}

void
colormap__Clear( self )
    struct colormap *self;
{
    int used = colormap_Used(self), i;
    struct color *c;

    if(used > 0) {
	for(i = 0; i < used; i++) {
	    c = colormap_NthColor(self, i);
	    if(c) color_Destroy(c);
	}
    }
    if(self->colors) {
	free(self->colors);
	self->colors = NULL;
    }
}

char *
colormap__ViewName( self )
    struct colormap *self;
{
    return("colormapv");
}

void
colormap__DestroyColor( self, c )
    struct colormap *self;
    struct color *c;
{
    register int i;
    for(i = 0; i < self->used; i++)
	if(c == *(self->colors + i))
	    break;
    if(i != self->used) {
	self->used--;
	for(; i < self->used; i++)
	    *(self->colors + i) = *(self->colors + i + 1);
    }
}
