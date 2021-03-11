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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/winsys.c,v 2.12 1993/01/13 19:52:51 gk5g Exp $";
#endif


 

#include <class.h>
#include <environ.ih>
#include <cursor.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <im.ih>
#include <winsys.eh>


struct windowsysteminfo *windowSystem;

struct cursor *windowsystem__CreateCursor(self)
    struct windowsystem *self;
{
    return NULL;
}

struct fontdesc *windowsystem__CreateFontdesc(self)
    struct windowsystem *self;
{
    return NULL;
}

struct graphic *windowsystem__CreateGraphic(self)
    struct windowsystem *self;
{
    return NULL;
}

struct im *windowsystem__CreateIM(self)
    struct windowsystem *self;
{
    return NULL;
}

struct offscrwin * windowsystem__CreateOffScreenWindow(self,host,width,height)
    struct windowsystem *self;
    char * host;
    long width, height;
{
    return NULL;
}

void windowsystem__FlushAllWindows(self)
    struct windowsystem *self;
{
}

boolean windowsystem__HandleFiles(self, WaitTime, beCheap)
    struct windowsystem *self;
    long WaitTime;
    boolean beCheap;
{
    return FALSE;
}

struct colormap *
windowsystem__CreateColormap( self, im )
struct windowsystem *self;
struct im *im;
{
    return(NULL);
}

struct color *
windowsystem__CreateColor( self, name, r, g, b )
char *name;
unsigned int r, g, b; 
    struct windowsystem *self;
{
    return(NULL);
}
