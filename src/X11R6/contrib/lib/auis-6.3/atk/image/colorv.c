

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/colorv.c,v 1.3 1992/12/15 21:36:09 rr2b R6tape $";
#endif
#include <andrewos.h>
#include <rect.h>
#include <view.ih>
#include <color.ih>
#include <colorv.eh>

boolean
colorv__InitializeClass( classID )
  struct classheader *classID;
{
  return(TRUE);
}

boolean
colorv__InitializeObject( classID, self )
  struct classheader *classID;
  struct colorv *self;
{
  return(TRUE);
}

void
colorv__FinalizeObject( classID, self)
  struct classheader *classID;
  struct colorv *self;
{

}

void
colorv__FullUpdate( self, type, left, top, width, height )
  struct colorv *self;
  enum view_UpdateType type;
  long left, top, width, height;
{
  struct color *c = (struct color*) colorv_GetDataObject(self);
  unsigned short R, G, B;
  struct rectangle r;

  color_GetRGB(c, R, G, B);
  colorv_SetForegroundColor(self, color_Name(c), R, G, B);
  colorv_GetVisualBounds(self, &r);
  colorv_FillRect(self, &r, colorv_BlackPattern(self));
}

void
colorv__Update( self )
  struct colorv *self;
{
  colorv_FullUpdate(self, view_FullRedraw, 0, 0, 0, 0);
}
