/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

 

#include <fontdesc.ih>
#include <sbutton.ih>

#define buttonV_NONE 0
#define buttonV_SURROUND 1
#define buttonV_ADJACENT 2
#define buttonV_EITHER 3
class buttonV[buttonv] : valueview[valuev]
{
 overrides:
Hit(enum view_MouseAction type, long x, long y, long numberOfClicks)
    returns struct view *;
  DrawFromScratch(long x,y,width,height);
  DrawHighlight();
  DrawDehighlight();
  DrawNewValue();
  DoHit(enum view_MouseAction type, long x,y,hits)
    returns struct buttonV *;
  LookupParameters();
methods:
  CacheSettings();
  DrawButtonText(char *text,long len,struct rectangle *rect,struct rectangle *rect2,boolean pushd);
  HandleStyleString(char *s);
classprocedures:
  InitializeClass() returns boolean;
  InitializeObject() returns boolean;
  FinalizeObject(); 
macromethods:
  SetButtonType(TYPE) (self->buttontype = TYPE)
  GetButtonType() (self->buttontype)
  SetFixedCount(VAL) (self->fixedcount = (VAL))
  GetFixedCount() (self->fixedcount)
  SetFourway(VAL) (self->fourway = (VAL))
  GetFourway() (self->fourway)
  SetFixedColumns(VAL) (self->fixedcolumns = self->columns = (VAL))
  GetFixedColumns() (self->fixedcolumns)
 data:
  char * label;
  char *l[4];
  char * fontname;
  short fontsize;
  short buttontype;
  short count,fixedcount;
  long max,rtl,rhw,x,y;
  struct fontdesc * activefont;
  boolean pushed,valueset,movedoff,vertical,topdown,fourway;
  struct list *list;
  struct buttonV_rl *current;
  long bcnt,offset,columns,bsize,fixedcolumns; 
  struct sbutton_info si; /* TO BE REMOVED */
  struct sbutton_prefs *prefs;
  int mono;
};

/* parameters to spst
   label		string		Defaults to NULL
   bodyfont		string		Defaults to "andytype"
   bodyfont-size	long		Defaults to 10
*/
