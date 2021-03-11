/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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


 

class iconview : view
{
  overrides:
    GetOrigin(long width, long height, long *originX, long *originY);
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    ReceiveInputFocus();
    Update();
    FullUpdate(enum view_UpdateType type, long left, long top,
		long width, long right);
    DesiredSize(long width, long height, enum view_DSpass pass,	
		 long *dWidth, long *dheight) 
      returns enum view_DSattributes;
    SetDataObject(struct dataobject * do);
    ObservedChanged(struct thisobject * data, long value);
    LinkTree(struct view *parent);
 methods:
    DecidedSize(w,h);
    RecommendSize(w,h);
    Open();
    Close();
    SetIconFont(char *iconfont, int iconstyle, int iconpts);
    SetIconChar(int iconchar);
    SetTitleFont(char *titlefont, int titlestyle, int titlepts);
    SetChild(char *viewclass);
    GetChild() returns struct view *;
classprocedures:
    InitializeObject(struct thisobject *self) returns boolean;
    InitializeClass() returns boolean;
    FinalizeObject(struct view *self);
    CloseRelated(struct view *v);
    OpenRelated(struct view *v);
  data:
    struct fontdesc * titlefont;
    char iconchar;
    struct fontdesc * iconfont;
    struct view * child;
    struct view * bottomview;
    int	isopen;
    long cx, cy, cw, ch;   /* childs extents in my logical space */
    long dw, dh;    /* desired hight and width */
    short neednewsize;
    struct iconview *next;
};
