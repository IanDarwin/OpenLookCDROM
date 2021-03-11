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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apt/suite/RCS/suiteev.c,v 1.59 1993/11/03 19:49:11 rr2b Exp $";
#endif


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Suite-object

MODULE	suiteev.c

VERSION	0.0

AUTHOR	TC Peters & GW Keim
 	Information Technology Center, Carnegie-Mellon University

DESCRIPTION
	This is the suite of Methods that support the Suite-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


END-SPECIFICATION  ************************************************************/



#include <ctype.h>
#include <graphic.ih>
#include <dataobj.ih>
#include <view.ih>
#include <cursor.ih>
#include <fontdesc.ih>
#include <proctbl.ih>
#include <menulist.ih>
#include <scroll.ih>
#include <text.ih>
#include <textv.ih>
#include <im.ih>
#include <region.ih>
#include <sbuttonv.ih>
#include <sbutton.ih>
#include <apt.h>
#include <vector.ih>
#include <suite.h>
#include <suite.ih>
#include <suitecv.ih>
#include <suiteev.eh>

#define Suite			    (self->parent)
#define	CurrentItem		    (Suite->current_item)
#define Rows			    (Suite->rows)
#define Columns			    (Suite->columns)
#define ItemHeight		    (Suite->itemheight)
#define ItemWidth		    (Suite->itemwidth)
#define ItemFixedHeight		    (Suite->itemfixedheight)
#define ItemFixedWidth		    (Suite->itemfixedwidth)
#define Items			    (Suite->items)
#define ItemArray		    (Suite->itemarray)
#define Scroll			    (Suite->scroll)    
#define ScrollType		    (Suite->scrolltype)
#define ScrollTOP		    (ScrollType & scroll_TOP)
#define ScrollBOTTOM		    (ScrollType & scroll_BOTTOM)
#define ScrollLEFT		    (ScrollType & scroll_LEFT)
#define ScrollRIGHT		    (ScrollType & scroll_RIGHT)
#define HScroll			    (ScrollTOP || ScrollBOTTOM)
#define VScroll			    (ScrollLEFT || ScrollRIGHT)
#define ItemViewObjectName	    (Suite->itemviewobjectname)
#define ItemOrder		    (Suite->itemorder)
#define RowMajor		    (ItemOrder & suite_RowMajor)
#define ColumnMajor		    (ItemOrder & suite_ColumnMajor)
#define Arrangement		    (Suite->arrangement)
#define List			    (Arrangement & suite_List)
#define	WrapStyle		    (Suite->wrappingstyle)
#define Unbalanced		    (Arrangement & suite_Unbalanced)
#define Balanced		    (Arrangement & suite_Balanced)
#define SingleColumn		    (Arrangement & suite_Column)
#define SingleRow		    (Arrangement & suite_Row)
#define Matrix			    (Arrangement & suite_Matrix)
#define RowLine			    (Arrangement & suite_RowLine)
#define ColumnLine		    (Arrangement & suite_ColumnLine)
#define	Fixed			    (Arrangement & suite_Fixed)
#define SelectionMode		    (Suite->selection_mode)
#define CaptionFont		    (Suite->captionfont)
#define	TitleFont		    (Suite->titlefont)
#define	TitlePlacement		    (Suite->titleplacement)
#define CaptionFontSize		    (Suite->captionfontsize)
#define ItemBorderStyle		    (Suite->itemborderstyle)
#define ItemBorderSize		    (Suite->itembordersize)
#define ItemHighlightStyle	    (Suite->itemhighlightstyle)
#define ItemPassiveStyle	    (Suite->itempassivestyle)
#define HitHandler		    (Suite->hithandler)
#define ClientAnchor		    (Suite->anchor)
#define FirstVisible		    (Suite->firstvisible)
#define	FirstVisibleSubString	    (self->firstvisiblesubstring)
#define NewFirstVisible		    (Suite->newfirstvisible)
#define LastVisible		    (Suite->lastvisible)
#define VisibleRows		    (Suite->visiblerows)
#define VisibleColumns		    (Suite->visiblecolumns)
#define NumVisible		    (Suite->numvisible)
#define Container		    (Suite->container)
#define ContainerLeft		    (Container.left)
#define ContainerTop		    (Container.top)
#define ContainerWidth		    (Container.width)
#define ContainerHeight		    (Container.height)
#define	ContainerRight		    (ContainerLeft + ContainerWidth)
#define	ContainerBottom		    (ContainerTop + ContainerHeight)
#define ExceptionHandler	    (Suite->exception)
#define ExceptionStatus		    (Suite->exception_status)    
#define	SuiteBackground		    (Suite->suiteColor->background_name)
#define	SuiteForeground		    (Suite->suiteColor->foreground_name)
#define	SuiteBGShade		    (Suite->suite_bg_shade)
#define	ActiveItemCaptionColor 	    (Suite->activeItemColor->caption_name)
#define	PassiveItemCaptionColor 	    (Suite->passiveItemColor->caption_name)
#define	ActiveItemForeground 	    (Suite->activeItemColor->foreground_name)
#define	ActiveItemBackground 	    (Suite->activeItemColor->background_name)
#define	PassiveItemForeground 	    (Suite->passiveItemColor->foreground_name)
#define	PassiveItemBackground 	    (Suite->passiveItemColor->background_name)
#define XGutterSize		    (Suite->x_guttersize)
#define YGutterSize		    (Suite->y_guttersize)
#define ItemCursor		    (Suite->itemcursor)
#define ItemCursorFont		    (Suite->itemcursorfont)
#define ItemCursorByte		    (Suite->itemcursorbyte)
#define CVIF			    (self->cvif)
#define Debug			    (self->debug)
#define FirstHit		    (self->firsthit)
#define LastHit			    (self->lasthit)
#define HasFocus		    (Suite->has_focus)
#define TitleMWidth		    (Suite->title_m_width)
#define CaptionMWidth		    (Suite->caption_m_width)
#define ITEM(i)			    (struct suite_item*) vector_Item(Items,i)
#define Active(item)		    (item->mode & item_Active)
#define Exposed(item)		    suite_ItemExposed(Suite,item)
#define Normalized(item)	    (item->mode & item_Normalized)
#define Highlighted(item)	    (item->mode & item_Highlighted)
#ifndef MAX
#define MAX(a,b)		    (((a)>=(b))?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b)		    (((a)<=(b))?(a):(b))
#endif
#define Bounds			    (item->bounds)
#define Left			    (Bounds.left)
#define Top			    (Bounds.top)
#define Width			    (Bounds.width)
#define Height			    (Bounds.height)
#define	Right			    (Left + Width)
#define	Bottom			    (Top + Height)
#define	FarthestRightItemBorder	    (ContainerRight-XGutterSize)
#define	FarthestBottomItemBorder    (ContainerBottom-YGutterSize)
#define CaptionRect		    (item->caption_rect)
#define TitleRect		    (item->title_rect)
#define InsetRect		    (item->inset_rect)
#define InsetLeft		    (InsetRect.left)
#define InsetTop		    (InsetRect.top)
#define InsetWidth		    (InsetRect.width)
#define InsetHeight		    (InsetRect.height)
#define	myInsetRect(r,deltax,deltay) rectangle_SetRectSize((r), rectangle_Left((r)) + (deltax), rectangle_Top((r)) + (deltay), rectangle_Width((r)) - 2*(deltax), rectangle_Height((r)) - 2*(deltay));
#define	DecrementRect(r,n) myInsetRect(r,n,n)
#define SetTransferMode(v,m) if((m) != view_GetTransferMode(((struct view*)(v)))) view_SetTransferMode(((struct view*)(v)),(m));
#define SetFont(v,f) if((f) != view_GetFont(((struct view*)(v))))\
			view_SetFont(((struct view*)(v)),(f));
#define graphicIsMono			(Suite->mono)

static int suiteev_debug = 0;

static void xsetframe(), ysetframe(), getinfo(), endzone();
static long ywhatis(), xwhatis();

static void DrawRect();
#define LIT TRUE

static struct scrollfns horizInterface = { getinfo, xsetframe, endzone, xwhatis };
static struct scrollfns vertInterface =  { getinfo, ysetframe, endzone, ywhatis };

static void ReadWriteHandler(); 
static void MaxSubStringSize();
static long MaxListSubStringWidth();
static struct menulist *menulist = NULL;
static ItemFullUpdate();

static void
AllocNameSpace(target, source)
  register char **target, *source;
{
  if(target && *target) {
    free(*target);
    *target = NULL;
  }
  if(source && *source) {
    *target = (char*) malloc(strlen(source)+1);
    if(*target) strcpy(*target,source);
  }
  else *target = NULL;
}

static void
CheckForNewFirstVisible( self )
    struct suiteev *self;
{
    if(NewFirstVisible) {
	FirstVisible = NewFirstVisible;
	NewFirstVisible = NULL;
    }
    else if( !FirstVisible )
	FirstVisible = ITEM(0);
}

static void
SetBackgroundShade( self )
    struct suiteev *self;
{
    if(!graphicIsMono) {
	SetTransferMode(self, graphic_COPY);
	if(SuiteBGShade[0] < 0)
	    sbuttonv_InteriorBGColor(Suite, Suite->buttonprefs, FALSE, SuiteBGShade);
	suiteev_SetFGColor(self, SuiteBGShade[0], SuiteBGShade[1], SuiteBGShade[2]);
    }
    else
	SetTransferMode(self, graphic_WHITE);
}

static long
Within(x, y, left, top, width, height)
  register long x, y, left, top, width, height;
{
  return((x >= left) && (x <= left + width) && (y >= top) && (y <= top + height));
}

static long
WithinRect(x, y, r)
  register long x, y;
  register struct rectangle *r;
{
  return(Within(x, y, r->left, r->top, r->width, r->height));
}

static long
CopySelected(self, suite, item, datum)
  register struct suiteev *self;
  register struct suite *suite;
  register struct suite_item *item;
  register long datum;
{
  register long int status = 0;
  static char buffer[1025];

  IN(CopySelected);
  if(suite && item)
    if(Exposed(item) && Active(item) && Highlighted(item)) {
      sprintf(buffer,"%s ",(char*)suite_ItemAttribute(suite,item,
				       suite_ItemCaption(0)));
      fputs(buffer,(FILE*)datum);
    }
  OUT(CopySelected);
  return(status);
}

static void
Copy(self)
  register struct suiteev *self;
{
  register FILE *CutFile = NULL;
  register struct im *im = NULL;

  IN(Copy);
  if(im = suiteev_GetIM(self)) {
    CutFile = im_ToCutBuffer(im);
    suite_Apply(Suite,CopySelected,Suite,CutFile);
    im_CloseToCutBuffer(im,CutFile);
  }
  OUT(Copy);
}

boolean
suiteev__InitializeClass(ClassID)
  register struct classheader *ClassID;
{
  struct proctable_Entry *tempProc = NULL;

  IN(suiteev_InitializeClass);
  menulist = menulist_New();
  tempProc = proctable_DefineProc( "suiteev-copy", Copy, 
	&suiteev_classinfo, NULL, "copy selected region to cut buffer" );
  menulist_AddToML( menulist, "Copy~10", tempProc, NULL, NULL );
  OUT(suiteev_InitializeClass);
  return(TRUE);
}

static char suiteButtonActive[] = "suiteButtonActive";
static char suiteButtonPassive[] = "suiteButtonPassive";

boolean
suiteev__InitializeObject(ClassID, self)
  register struct classheader *ClassID;
  register struct suiteev *self;
{
  self->parent = NULL;
  self->cvif = NULL;
  self->firsthit = self->lasthit = NULL;
  self->menulist = menulist_DuplicateML(menulist,self);
  FirstVisibleSubString = 0;
  suiteev_debug = Debug = 1;
  self->buttonPrefsActive = sbutton_GetNewPrefs(suiteButtonActive);
  sbutton_InitPrefs(self->buttonPrefsActive, suiteButtonActive);
  self->buttonPrefsPassive = sbutton_GetNewPrefs(suiteButtonPassive);
  sbutton_InitPrefs(self->buttonPrefsPassive, suiteButtonPassive);
  return(TRUE);
}

void
suiteev__FinalizeObject(ClassID, self)
  register struct classheader *ClassID;
  register struct suiteev *self;
{
  if(self->menulist) 
      menulist_Destroy(self->menulist);
}

void
suiteev__PostMenus(self, menulist)
  struct suiteev *self;
  struct menulist *menulist;
{
  IN(suiteev_PostMenus);
  if(List) {
    menulist_ClearChain(self->menulist);
    if(menulist) 
      menulist_ChainBeforeML(self->menulist,menulist,NULL);
    super_PostMenus(self,self->menulist);
  }
  else super_PostMenus(self,menulist);
  OUT(suiteev_PostMenus);
}

static struct suite_item *
NthAfter(self, start, numToSkip)
  register struct suiteev *self;
  register struct suite_item *start;
  register long numToSkip;
{
  register int i = 0;
  register struct suite_item *item = start;
  register long count = 0, size = 0;

  IN(NthAfter);
  if(Items && ITEM(0) && (i = vector_Subscript(Items,start)) != -1) {
    size = vector_Count(Items);
    while((count < numToSkip) && (i < size) && (item = ITEM(++i)))
      if(suite_ItemExposed(Suite,item)) count++;
  }
  OUT(NthAfter);
  return(item);
}

static struct suite_item *
NthPrior(self, start, numToSkip)
  register struct suiteev *self;
  register struct suite_item *start;
  register long numToSkip;
{
  register int i = 0;
  register struct suite_item *item = start;
  register long count = 0;

  IN(NthAfter);
  if(Items && ITEM(0) && (i = vector_Subscript(Items,start)) != -1)
    while((count < numToSkip) && (i > 0) && (item = ITEM(--i)))
      if(suite_ItemExposed(Suite,item)) count++;
  OUT(NthPrior);
  return(item);
}

static struct suite_item *
NPixelsAfter(self, start, pix, numToSkip)
  register struct suiteev *self;
  register struct suite_item *start;
  register long pix, *numToSkip;
{
  register int i = 0;
  register struct suite_item *item = start;
  register long count = 0, size = 0, tmp = 0;

  IN(NthAfter);
  if(Items && ITEM(0) && (i = vector_Subscript(Items,start)) != -1) {
    size = vector_Count(Items);
    if(ColumnMajor) tmp = YGutterSize;
    else tmp = XGutterSize;
    while((tmp < pix) && (i < size) && (item = ITEM(++i)))
      if(suite_ItemExposed(Suite,item)) {
	if(ColumnMajor) tmp += (Height + YGutterSize);
	else tmp += (Width + XGutterSize);
	count++;
      }
  }
  *numToSkip = count;
  OUT(NPixelsAfter);
  return(item);
}

static struct suite_item *
NPixelsPrior(self, start, pix, numToSkip)
  register struct suiteev *self;
  register struct suite_item *start;
  register long pix, *numToSkip;
{
  register int i = 0;
  register struct suite_item *item = start;
  register long count = 0, tmp = 0;

  IN(NthAfter);
  if(Items && ITEM(0) && (i = vector_Subscript(Items,start)) != -1) {
    if(ColumnMajor) tmp = YGutterSize;
    else tmp = XGutterSize;
    while((tmp < pix) && (i > 0) && (item = ITEM(--i)))
      if(suite_ItemExposed(Suite,item)) {
	if(ColumnMajor || List) tmp += (Height + YGutterSize);
	else tmp += (Width + XGutterSize);
	count++;
      }
  }
  *numToSkip = count;
  OUT(NPixelsPrior);
  return(item);
}

static void
DrawGutterLines(self)
  register struct suiteev *self;
{
  register struct suite_item *item = FirstVisible;
  register long i;
  long numToDo, numToSkip, last, first;
  long Offset, clearLeft, clearTop;

  IN(DrawGutterLines);
  if(Items && ITEM(0) && FirstVisible ) {
      if(RowLine || ColumnLine) {
	  last =  vector_Subscript(Items, LastVisible);
	  first = vector_Subscript(Items, FirstVisible);
	  if(RowLine) {
	      if(RowMajor)
		  numToSkip = VisibleColumns;
	      else if(ColumnMajor)
		  numToSkip = 1;
	      numToDo = (last - first)/VisibleColumns;
	      Offset = YGutterSize/2;
	      for(i = 0 ;i < numToDo && item ; i++) {
		  suiteev_MoveTo(self, ContainerLeft, Top + Height + Offset);
		  suiteev_DrawLineTo(self, ContainerLeft + ContainerWidth, Top + Height + Offset);
		  item = NthAfter(self, item, numToSkip);
	      }
	      clearTop = LastVisible->bounds.top + LastVisible->bounds.height;
	  }
	  if(ColumnLine) {
	      if(RowMajor)
		  numToSkip = 1;
	      else if(ColumnMajor)
		  numToSkip = VisibleRows;
	      item = FirstVisible;
	      numToDo = (last - first)/VisibleRows;
	      Offset = XGutterSize/2;
	      for(i = 0 ; i < numToDo && item ; i++) {
		  suiteev_MoveTo(self, Left + Width + Offset, Top);
		  suiteev_DrawLineTo(self, Left + Width + Offset, Top + ContainerHeight);
		  item = NthAfter(self, item, numToSkip);
	      }
	      clearLeft = LastVisible->bounds.left + LastVisible->bounds.width; 
	  }
#if 0
	  SetBackgroundShade(self);
	  if(ColumnMajor) {
	      suiteev_FillRectSize(self, 
				   clearLeft = LastVisible->bounds.left, 
				   clearTop = (LastVisible->bounds.top + LastVisible->bounds.height), 
				   ContainerWidth - clearLeft, 
				   ContainerHeight - clearTop, NULL);
	      suiteev_FillRectSize(self, 
				   clearLeft = LastVisible->bounds.left + LastVisible->bounds.width, 
				   0, 
				   ContainerWidth - clearLeft, 
				   ContainerHeight, NULL);
	  }
	  else {	/* RowMajor */
	      suiteev_FillRectSize(self, 
				   clearLeft = LastVisible->bounds.left + LastVisible->bounds.width, 
				   clearTop = LastVisible->bounds.top, 
				   ContainerWidth - clearLeft, 
				   ContainerHeight - clearTop, NULL);
	      suiteev_FillRectSize(self, 
				   0, 
				   clearTop = (LastVisible->bounds.top + LastVisible->bounds.height), 
				   ContainerWidth, 
				   ContainerHeight - clearTop, NULL);
	  }
#endif
      }
  }
  SetTransferMode(self, graphic_COPY);
  OUT(DrawGutterLines);
}

static long
ResetItemBreaks(self, suite, item, datum)
  register struct suite *self;
  register struct suite *suite;
  register struct suite_item *item;
  register long datum;
{
  register int status = 0, i = 0;

  if(item) {
    for(i = 0; i < BreakCount(item); i++)
      vector_Item(Breaks(item), i) = 0;
    BreakCount(item) = 0;
  }
  return(status);
}

void
suiteev__FullUpdate(self, type, left, top, width, height)
  register struct suiteev *self;
  register enum view_UpdateType type;
  register long left, top, width, height;
{
  struct rectangle r;
  
    IN(suiteev_FullUpdate);
    if((type == view_FullRedraw) || (type == view_LastPartialRedraw)) {
	suiteev_GetVisualBounds(self, &r);
	if(List) 
	    suite_Apply(Suite, ResetItemBreaks, Suite, 0);
	suiteev_Arrange(self, &r);
	suiteev_DrawItems(self, &r);
    }
    DrawGutterLines(self);
    OUT(suiteev_FullUpdate);
}

char *
suiteev__GetInterface(self, type)
  register struct suiteev *self;
  register char *type;
{
    IN(suiteev_GetInterface);
    if(!strcmp(type, "scroll,vertical"))
	return((char *) &vertInterface);
    else if(!strcmp(type, "scroll,horizontal"))
	return((char *) &horizInterface);
    else
	return(NULL);
}

static void
getinfo(self, total, seen, dot)
  register struct suiteev *self;
  register struct range *total, *seen, *dot;
{
    IN(getinfo);
    total->beg = 0;
    total->end = suiteev_NumberExposed(self) - 1;
    if(Items && ITEM(0) && FirstVisible) 
	seen->beg = vector_Subscript(Items,FirstVisible);
    else
	seen->beg = 0;
    if(Items && ITEM(0) && LastVisible) 
	seen->end = vector_Subscript(Items,LastVisible);
    else
	seen->end = total->end;
    dot->beg = dot->end = seen->beg;
}

static void
endzone(self, zone, action)
  register struct suiteev *self;
  register int zone;
  register enum view_MouseAction action;
{
  register int numVisible = 0, EndOffset = 0;
  register struct suite_item *LastItem = NULL;

    IN(endzone);
    if(Items && ITEM(0)) {
	if(action != view_LeftDown && action != view_RightDown)
	    return;
	if(action == view_LeftDown) {
	    if(zone == scroll_BOTTOMENDZONE) {
		numVisible = suiteev_NumberExposed(self);
		if((LastItem = ITEM(vector_Count(Items) - 1)) != LastVisible) {
		    if(Matrix)
			if(numVisible > (VisibleRows * VisibleColumns)) {
			    EndOffset = (VisibleRows * VisibleColumns) - 1;
			    NewFirstVisible = NthPrior(self, LastItem, EndOffset);
			}
			else NewFirstVisible = ITEM(0);
		    else if(SingleRow)
			if(numVisible > VisibleColumns) {
			    EndOffset = VisibleColumns - 1;
			    NewFirstVisible = NthPrior(self, LastItem, EndOffset);
			}
			else NewFirstVisible = ITEM(0);
		    else if(SingleColumn)
			if(numVisible > VisibleRows) {
			    EndOffset = VisibleRows - 1;
			    NewFirstVisible = NthPrior(self, LastItem, EndOffset);
			}
			else NewFirstVisible = ITEM(0);
		}
	    }
	    else if(zone == scroll_TOPENDZONE) /* obviously */
		if(Items && ITEM(0))
		    NewFirstVisible = ITEM(0);
	}
	else if(action == view_RightDown) {
	    if((numVisible = suiteev_NumberExposed(self)) > 1) {
		if(zone == scroll_BOTTOMENDZONE)
		    NewFirstVisible = NthAfter(self, FirstVisible, 1);
		else
		    NewFirstVisible = NthPrior(self, FirstVisible, 1);
	    }
	}
	if(NewFirstVisible && (NewFirstVisible != FirstVisible))
	    suiteev_WantUpdate(self, self);
    }
    OUT(endzone);
}

static long
ywhatis(self, num, denom)
  register struct suiteev *self;
  register long num, denom;
{
  return(suiteev_Locate( self, 0, (num * suiteev_GetLogicalHeight(self)) / denom) );
}

static long
xwhatis(self, num, denom)
  register struct suiteev *self;
  register long num, denom;
{
    return(suiteev_Locate(self, (num * suiteev_GetLogicalWidth(self)) / denom, 0));
}

static void
ysetframe(self, posn, coord, outof)
  register struct suiteev *self;
  register long posn, coord, outof;
{
  register long vertOffset = 0, height = ItemHeight;
  long numToSkip = 0;

    IN(ysetframe);
    if(coord) { /* Right Click */
	if(RowLine)
	    height = ItemHeight + 3;
	vertOffset = suiteev_GetLogicalHeight(self) * coord / outof;
	if(List) {
	    NPixelsPrior(self, FirstVisible, vertOffset, &numToSkip);
	    if(RowMajor)
		numToSkip *= VisibleColumns;
	}
	else {
	    if(ColumnMajor)
		numToSkip = vertOffset/height;
	    else if(RowMajor)
		numToSkip = ((vertOffset/height) * VisibleColumns);
	}
	NewFirstVisible = NthPrior(self, FirstVisible, numToSkip);
	if(!NewFirstVisible)
	    NewFirstVisible = ITEM(0);
    }
    else /* Left Click */ 
	NewFirstVisible = NthAfter(self, ITEM(0), posn);
    if(NewFirstVisible != FirstVisible)
	suiteev_WantUpdate(self, self);
}

static void
xsetframe(self, posn, coord, outof)
  register struct suiteev *self;
  register long posn, coord, outof;
{
  register long width = ItemWidth, horizOffset = 0, numToSkip = 0;

  IN(xsetframe);
  if(coord) { /* Right Click */
    if(ColumnLine)
	width += 3;
      horizOffset = suiteev_GetLogicalWidth(self) * coord / outof;
      if(ColumnMajor)
	  numToSkip = ((horizOffset/width) * VisibleRows);
      else if(RowMajor)
	  numToSkip = horizOffset/width;
      NewFirstVisible = NthPrior(self, FirstVisible, numToSkip);
      if(!NewFirstVisible)
	  NewFirstVisible = ITEM(0);
  }
  else /* Left Click */ 
      NewFirstVisible = NthAfter(self, ITEM(0), posn);
  if(NewFirstVisible != FirstVisible)
      suiteev_WantUpdate(self, self);
}

#define TwiceBorderSize (2 * ItemBorderSize)

static void
AttemptSymmetry(self, numItems, rows, columns)
  register struct suiteev *self;
  register long numItems, *rows, *columns;
{
  if(numItems >= ((*rows) * (*columns))) {
      return;
  }
  else {
      if(RowMajor && FALSE) {
	  *columns = (numItems/(*rows));
	  *columns += ((numItems % (*rows)) ? 1 : 0);
      }
      else {
	  *rows = (numItems/(*columns));
	  *rows += ((numItems % (*columns)) ? 1 : 0);
      }
  }
}

static void
SetBreakPoint(self, item, end)
  register struct suiteev *self;
  register struct suite_item *item;
  register char *end;
{
  vector_AddItem(Breaks(item),(long)(end - item_Caption));
}

static boolean
DoesItFit(self, item, head, tail, width)
  struct suiteev *self;
  struct suite_item *item;
  char *head;
  char *tail;
  long width;
{
  char save = *tail;
  long XWidth = 0, YWidth = 0;

  *tail = '\0';
  fontdesc_StringBoundingBox(item_CaptionFont,suiteev_GetDrawable(self),
		       head,&XWidth,&YWidth);
  *tail = save; /*replace potential break*/
  return((XWidth < width) ? TRUE : FALSE);
}

static char *
WalkBackwardToPunctuation(head, tail)
  register char *head;
  register char *tail;
{
  if(tail && (*tail != ' ') && (*(tail - 1) == ' ')) tail--;
  while((tail > head) && (*tail == ' ')) tail--;
  while(tail > head) {
    if(*tail == '.' || *tail == '-' || *tail == ';' || *tail == ':')
      break;
    else tail--;	
  }
  if(*tail == ' ') tail++;
  return(tail);
}

static char *
WalkBackwardBlackSpace( self, item, head, tail, width )
  register struct suiteev	*self;
  register struct suite_item	*item;
  register char			*head;
  register char			*tail;
  register long			 width;
{
  char				*saved_tail = tail;

  if((*tail != ' ') && (*(tail - 1) == ' ')) (tail)--;
  while((tail > head) && (*tail == ' ')) (tail)--;	
  while((tail > head) && (*tail != ' ')) (tail)--;
  if(*tail == ' ') (tail)++;
  if(tail == head) {
    tail = saved_tail;
    while((tail > head) && (tail = WalkBackwardToPunctuation(head,tail)))
      if(DoesItFit(self,item,head,tail,width)) break;
      else tail--;
  }
  if(tail == head) {
    tail = saved_tail-1;
    while(tail > head) {
      if(DoesItFit(self,item,head,tail,width)) break;
      else tail--;
    }
    if(tail == head) tail = head + 1;
  }
  return(tail);
}

void
suiteev__ShrinkWrap( self, width, height )
  struct suiteev		    *self;
  long				     width, height;
{
  boolean			     end = FALSE, ResetWidthForOffset = TRUE;
  register int			     indx = 0, i = 0, numLines = 0;
  register struct suite_item	    *item = NULL;
  long				     saved_width = 0;
  register char			    *head = NULL, *nl = NULL, *t = NULL;
  long				     carriedHeight = YGutterSize + ItemBorderSize;

  IN(suiteev_ShrinkWrap);
  saved_width = width;
  if(Items && ITEM(0)) {
    indx = vector_Subscript(Items,FirstVisible);
    while((indx < vector_Count(Items)) && (item = ITEM(indx)) && (carriedHeight < height)) {
      if(width > (3 * CaptionMWidth)) 
	  ResetWidthForOffset = FALSE;
      else ResetWidthForOffset = TRUE;
      width = saved_width;
      if(Exposed(item) && item_Caption) {
	BreakCount(item) = 0;
	numLines = suiteev_LineCount(self, item_Caption);
	head = item_Caption;
	for(i = 0 ; i < numLines ; i++) {
	  end = FALSE;
	  while(!end) {
	    if(nl = (char*)index(head,'\n')) 
	      *nl = '\0';
	    else {
	      nl = head + strlen(head);
	      end = TRUE;
	    }
	    for(t = nl;t > head;t = WalkBackwardBlackSpace(self, item, head, t, width))
	      if(DoesItFit(self, item, head, t, width)) break;
	        if(!end) *nl = '\n';
		if(t == head) t = head+2;
		if(t > head && *t != '\0') {
		  if(*t == '\n') head = t + 1;
		  else {
		    SetBreakPoint(self,item,t-1);
		    end = FALSE;
		    head = t;
		  }
		  if(!ResetWidthForOffset) {
		    width -= (2 * CaptionMWidth);
		    ResetWidthForOffset = TRUE;
		  }
		}
	  }
	}
	carriedHeight += (((item_CaptionFontSize+1) * (numLines+BreakCount(item))) + item_BorderSize + YGutterSize);
      }
      indx++;
    }
  }
  OUT(suiteev_ShrinkWrap);
}

static void
PlaceItems( self, rect, rows, cols, numleftOvers, itemWidth, itemHeight )
  register struct suiteev	    *self;
  struct rectangle		    *rect;
  long				     rows, cols, numleftOvers;
  long				     itemWidth, itemHeight;
{
  register int			     i = 0;
  register struct suite_item	    *item = NULL;
  long				     itemIndex = 0;
  long int			     width = 0, height = 0, top = 0, left = 0; 
  long int			     OrigWidth = 0, OrigHeight = 0;
  long int			     OrigTop = 0, OrigLeft = 0; 
  long				     delta = 0;
  register long			     XIndex = 0, XMax = cols;
  register long			     YIndex = 0, YMax = rows;
  register long			     leftOverIndex = 0;
  long				     Ax = 0, Ay = 0;
  long				     X_LeftOvers = 0, Y_LeftOvers = 0;
  register long			     AggrigateLeft = 0, AggrigateTop = 0;
  long				     X_epsilon = 0, Y_epsilon = 0;
  unsigned			     newlineHeight;

  IN(PlaceItems);
  rectangle_GetRectSize(rect,&left,&top,&width,&height);
  rectangle_GetRectSize(rect,&OrigLeft,&OrigTop,&OrigWidth,&OrigHeight );
  if(Items && ITEM(0)) LastVisible = ITEM(vector_Count(Items) - 1);
  else return;
  if(RowMajor && (numleftOvers > 0)) rows++;
  else if(ColumnMajor && (numleftOvers > 0)) cols++;
  width -= ((cols+1) * XGutterSize);
  height -= ((rows+1) * YGutterSize);
  Y_LeftOvers = height - (rows * itemHeight);
  X_LeftOvers = width - (cols * itemWidth);
  if(Y_LeftOvers < 0) Y_LeftOvers = 0;
  if(X_LeftOvers < 0) X_LeftOvers = 0;
  if(!List) {
    if(ItemFixedWidth && (delta = (width - (cols * itemWidth))) > 0) {
      left += (delta / 2);
      width -= delta;
    }
    if(ItemFixedHeight && (delta = (height - (rows * itemHeight))) > 0) {
      top += (delta / 2);
      height -= delta;
    }
  }
  AggrigateLeft = Ax = left + XGutterSize;
  AggrigateTop = Ay = top + YGutterSize;
  i = vector_Subscript(Items,FirstVisible);
  while(item = ITEM(i++))
    if(Exposed(item)) {
      newlineHeight = fontdesc_FontSummary(item_CaptionFont, suite_GetDrawable(Suite))->newlineHeight;
      itemIndex++;
      if(X_LeftOvers> XIndex) X_epsilon = 1;
      else X_epsilon = 0;
      if(Y_LeftOvers> YIndex) Y_epsilon = 1;
      else Y_epsilon = 0;
      if(List && VisibleColumns == 1) {
	itemHeight = ((newlineHeight+2) * (suiteev_LineCount(self,item_Caption) + BreakCount(item))) + TwiceBorderSize;
	item_SetUpperLeft(item,AggrigateLeft,AggrigateTop);
	item_SetDimensions(item,itemWidth,itemHeight);
	rectangle_SetRectSize(&InsetRect,AggrigateLeft,AggrigateTop,
			      itemWidth,itemHeight);
      }
      else {
	item_SetUpperLeft(item,AggrigateLeft,AggrigateTop);
	item_SetDimensions(item,itemWidth + X_epsilon,
			   itemHeight + Y_epsilon);
	rectangle_SetRectSize(&InsetRect,AggrigateLeft,AggrigateTop,
			      itemWidth + X_epsilon,
			      itemHeight + Y_epsilon);
      }
      if(RowMajor) {
	AggrigateLeft += (itemWidth + X_epsilon + XGutterSize);
	if(++XIndex == XMax) {
	    AggrigateTop += (itemHeight + Y_epsilon + YGutterSize);
	    XIndex = 0;
	    AggrigateLeft = Ax;
	    YIndex++;
	}
      }
      else {
	AggrigateTop += (itemHeight + Y_epsilon + YGutterSize);
	if(++YIndex == YMax) {
	    AggrigateLeft += (itemWidth + X_epsilon + XGutterSize);
	    YIndex = 0;
	    AggrigateTop = Ay;
	    XIndex++;
	}
      }
      if((itemIndex == NumVisible) && (LastVisible = item)) break;
	if( numleftOvers && 
	   ((RowMajor && (YIndex == (YMax - 1))) || 
	    (ColumnMajor && (XIndex == (XMax - 1))))) break;
    }
  if(numleftOvers) {
    leftOverIndex = 0;
    X_LeftOvers = Y_LeftOvers = 0;
    left = OrigLeft;top = OrigTop;
    width = OrigWidth;height = OrigHeight; 
    if(RowMajor) {
	if(Balanced) {
	    width -= ((numleftOvers+1) * XGutterSize);
	    AggrigateLeft = (abs(width-(numleftOvers*itemWidth))/2);
	    Ax = left + XGutterSize;
	}
	X_LeftOvers = width - (numleftOvers * itemWidth);
	if(X_LeftOvers < 0) X_LeftOvers = 0;
    }
    else if(ColumnMajor) {
	if(Balanced) {
	    height -= ((numleftOvers+1) * YGutterSize);
	    AggrigateTop = (abs(height-(numleftOvers*itemHeight))/2);
	    Ay = top + YGutterSize;
	}
	Y_LeftOvers = height - (numleftOvers * itemHeight);
	if(Y_LeftOvers < 0) Y_LeftOvers = 0;
    }
    while(item = ITEM(i++))
      if(Exposed(item)) {
	  if(X_LeftOvers > leftOverIndex) X_epsilon = 1;
	  else X_epsilon = 0;
	  if(Y_LeftOvers > leftOverIndex) Y_epsilon = 1;
	  else Y_epsilon = 0;
	  leftOverIndex++;
	  itemIndex++;
	  item_SetUpperLeft(item, AggrigateLeft, AggrigateTop);
	  item_SetDimensions(item,itemWidth + X_epsilon, itemHeight + Y_epsilon);
	  rectangle_SetRectSize(&InsetRect, AggrigateLeft, AggrigateTop, itemWidth + X_epsilon, itemHeight + Y_epsilon);
	  if(RowMajor)
	      AggrigateLeft += (itemWidth + X_epsilon + XGutterSize);
	  else
	      AggrigateTop += (itemHeight + Y_epsilon + YGutterSize);
	  if((itemIndex == NumVisible) && (LastVisible = item)) break;
      }
  }
  NumVisible = itemIndex;
  OUT(PlaceItems);
}

static void
DetermineVisibleListItems( self, height )
  struct suiteev *self;
  long height;
{
  register int i = 0, count = 0;
  struct suite_item *item = NULL;
  int sum = YGutterSize, newlineHeight;

    IN(DetermineVisibleListItems);
    i = vector_Subscript(Items, FirstVisible);
    while(item = ITEM(i++)) {
	newlineHeight = fontdesc_FontSummary(item_CaptionFont, suite_GetDrawable(Suite))->newlineHeight;
	count++;
	sum += (TwiceBorderSize + YGutterSize + ((suiteev_LineCount(self, item_Caption) + BreakCount(item)) *  (newlineHeight + 2)));
	if(sum > height)
	    break;
    }
    NumVisible = count;
    OUT(DetermineVisibleListItems);
}

void
suiteev__Arrange( self, rect )
  register struct suiteev	*self;
  register struct rectangle	*rect;
{
  long				 itemWidth = 0, itemHeight = 0, 
                                 minHeight = 0, minWidth = 0, 
			         width = 0, height = 0, top = 0, left = 0; 
  long				 numleftOvers = 0, maxCols = 0;
  long				 maxRows = 0, numItems = 0;
  int				 newlineHeight;

  IN(suiteev_Arrange);
  newlineHeight = fontdesc_FontSummary(CaptionFont, suite_GetDrawable(Suite))->newlineHeight;
  if(!Items || ((numItems = suiteev_NumberExposed(self)) <= 0) ||
      (List && (rect->width < (3 * CaptionMWidth))) ||
      (List && (rect->height < (newlineHeight + TwiceBorderSize)))) {
    FirstVisible = LastVisible = NULL;
    NumVisible = 0;
    return;
  }
  else
      CheckForNewFirstVisible(self);
  suiteev_MaxStringSize(self, &itemWidth, &itemHeight);
  rectangle_GetRectSize(rect, &left, &top, &width, &height);
  if(ItemFixedHeight) minHeight = ItemFixedHeight + YGutterSize;
  else minHeight = itemHeight + TwiceBorderSize + 2 + YGutterSize;
  if(ItemFixedWidth) minWidth = ItemFixedWidth + XGutterSize;
  else minWidth = itemWidth + TwiceBorderSize + (2 * CaptionMWidth) + XGutterSize;
  if(List && (minWidth > width)) {
      if(width > (TwiceBorderSize + (2 * CaptionMWidth) + XGutterSize)) {
	  suiteev_ShrinkWrap(self, width - TwiceBorderSize - 
			     (2 * CaptionMWidth) - (2 * XGutterSize), height);
	  suiteev_MaxStringSize(self, &itemWidth, &itemHeight);
	  minHeight = itemHeight + TwiceBorderSize + 2;
	  minWidth = width - TwiceBorderSize - (2*XGutterSize);
      }
  }
  if(!Scroll) {
    NumVisible = numItems;
    FirstVisible = ITEM(0);
  }
  if(Matrix) {
    if( ! Fixed ) {
        maxCols = ((width/minWidth > 1) ? (width/minWidth) : 1);
        maxRows = ((height/minHeight > 1) ? (height/minHeight) : 1);
    }
    else {
	maxCols = Columns;
	maxRows = Rows;
    }
    if(RowMajor && (maxCols > numItems)) {
      maxCols = numItems;
      maxRows = 1;
    }
    else if(ColumnMajor && (maxRows > numItems)) {
      maxRows = numItems;
      maxCols = 1;
    }
    if( ! Fixed && ! List) AttemptSymmetry(self,numItems,&maxRows,&maxCols);
    width -= ((maxCols+1) * XGutterSize);
    height -= ((maxRows+1) * YGutterSize);
    if(List) {
      ItemWidth = itemWidth = minWidth;
      ItemHeight = itemHeight = minHeight;
    }
    else {
      if(ItemFixedWidth) ItemWidth = itemWidth = ItemFixedWidth;
      else ItemWidth = itemWidth = width/maxCols;
      if(ItemFixedHeight) ItemHeight = itemHeight = ItemFixedHeight;
      else ItemHeight = itemHeight = height/maxRows;
    }
    VisibleColumns = Columns = maxCols;
    VisibleRows = Rows = maxRows;
    if( numItems <= (VisibleRows * VisibleColumns) ) 
      NumVisible = numItems;
    else NumVisible = VisibleRows * VisibleColumns;
    if(RowMajor) numleftOvers = NumVisible % maxCols;
    else if(ColumnMajor) numleftOvers = NumVisible % maxRows;
    if(List && (maxCols == 1)) {
      DetermineVisibleListItems(self,rect->height);
      VisibleRows = Rows = maxRows = NumVisible;
      numleftOvers = 0;
    }
  }
  else if(SingleColumn) { 
    if(ItemFixedHeight) { 
      itemHeight = ItemFixedHeight;
      if(Scroll) NumVisible = height/minHeight;
      else NumVisible = numItems;
    }
    else {
      if(Scroll) {
        if((NumVisible = height/minHeight) == 0)
	  NumVisible = 1;
      }
      else NumVisible = numItems;
      height -= ((NumVisible+1) * YGutterSize );
      itemHeight = height/NumVisible;
    }
    if(ItemFixedWidth)
      itemWidth = ItemFixedWidth;
    else 
      itemWidth = width - (2 * XGutterSize);
    ItemHeight = itemHeight;
    ItemWidth = itemWidth;
    if(Scroll) {
      if(VScroll) {
        Rows = numItems;
	maxRows = VisibleRows = NumVisible;
      }
    }
    else NumVisible = Rows = VisibleRows = maxRows = numItems;
    Columns = VisibleColumns = maxCols = 1;
    if(List) {
      DetermineVisibleListItems(self,rect->height);
      VisibleRows = Rows = maxRows = NumVisible;
      numleftOvers = 0;
    }
  }
  else {
    if(ItemFixedWidth) {
      minWidth = itemWidth = ItemFixedWidth;
      if(Scroll) NumVisible = width/minWidth;
      else NumVisible = numItems;
    }
    else {
      if(Scroll) 
	if((NumVisible = width/minWidth) == 0)
          NumVisible = 1;
	else NumVisible = numItems;
      width -= ((NumVisible+1) * XGutterSize);
      itemWidth = width/NumVisible;
    }
    if(ItemFixedHeight)
      itemHeight = ItemFixedHeight;
    else 
      itemHeight = height - (2 * YGutterSize);
    ItemHeight = itemHeight;
    ItemWidth = itemWidth;
    if(Scroll) {
      if(HScroll) {
	Columns = numItems;
	maxCols = VisibleColumns = NumVisible;
      }
    }
    else NumVisible = Columns = VisibleColumns = maxCols = numItems;
    Rows = VisibleRows = maxRows = 1;
  }
  PlaceItems(self,rect,maxRows,maxCols,numleftOvers,itemWidth,itemHeight);
  OUT(suiteev_Arrange);
}

void
suiteev__DrawItems( self, rect )
  register struct suiteev *self;
  register struct rectangle *rect;
{
  register int i = 0;
  register struct suite_item *item = NULL;

    IN(suiteev_DrawItems);
    if(!Items || !ITEM(0))
	return;
    CheckForNewFirstVisible(self);
    i = vector_Subscript(Items, FirstVisible);
    while(item = ITEM(i++))
	if(Exposed(item)) {
	    ItemFullUpdate(self, item, view_FullRedraw, 0, 0, 0, 0);
	    if(item == LastVisible) 
		break;
	}
    OUT(suiteev_DrawItems);
}

static void
EraseItems( self )
  register struct suiteev *self;
{
  register int i = 0;
  register struct suite_item *item = NULL;

    IN(EraseItems);
    if(!Items || !ITEM(0) || !FirstVisible) 
	return;
    i = vector_Subscript(Items,FirstVisible);
    while(item = ITEM(i++))
	if(Exposed(item)) {
	    suiteev_ItemClear(self,item);	    
	    if(item == LastVisible) 
		break;
	}
    SetTransferMode(self,graphic_COPY);
    OUT(EraseItems);
}

void
suiteev__Update( self )
  register struct suiteev *self;
{
  struct rectangle r;

  IN(suiteev_Update);    
  suiteev_GetVisualBounds(self, &r);
  EraseItems(self);
  if(List) 
      suite_Apply(Suite, ResetItemBreaks, Suite, 0);
  suiteev_Arrange(self, &r);
  suiteev_DrawItems(self, &r);
  DrawGutterLines(self);
  OUT(suiteev_Update);    
}

long
suiteev__NumberItems( self )
  register struct suiteev	*self;
{
  if(Items && ITEM(0)) 
      return(vector_Count(Items));
  else return(-1);
}

long
suiteev__NumberVisible( self )
  register struct suiteev	*self;
{
  register long i = 0;
  register struct suite_item *item = FirstVisible;

  while(item && ++i && (item != LastVisible));
  return(i);
}

long
suiteev__NumberExposed( self )
  register struct suiteev *self;
{
  register long i = 0, index = 0;
  register struct suite_item *item = NULL;

  if(Items && ITEM(0))
    while(item = ITEM(index++))
      if(Exposed(item)) i++;
  return(i);
}

void
suiteev__Clear( self )
  register struct suiteev *self;
{ struct rectangle r;

  suiteev_GetVisualBounds(self, &r);
  SetBackgroundShade(self);
  suiteev_FillRect(self, &r, NULL);
}

struct suite_item *
suiteev__WhichItem( self, x, y )
  register struct suiteev *self;
  register long x, y;
{
  register int i = 0;
  register struct suite_item *item = NULL;

    IN(suiteev_WhichItem);
    if(Items && ITEM(0)) {
	CheckForNewFirstVisible(self);
	i = vector_Subscript(Items, FirstVisible);
	while(item = ITEM(i++))
	    if(WithinRect(x, y, &Bounds)) {
		if(Exposed(item) && Active(item)) 
		    return(item);
		else
		    return(NULL);
	    }
	    else if(item == LastVisible)
		return(NULL);
    }
    return(NULL);
}    

void
suiteev_HandleExclusiveHit( self, item, action, x, y, numberOfClicks )
  register struct suiteev *self;
  register struct suite_item *item;
  enum view_MouseAction action;
  long x, y;
  long numberOfClicks;
{
    IN(suiteev_HandleExclusiveHit);
    switch(action) {
	case view_LeftDown:
	    if(Highlighted(item))
		break;
	    else {
		suite_Reset(Suite, suite_Normalize);
		suiteev_ItemHighlight(self, item);
	    }
	    break;
	case view_LeftUp:
	case view_LeftMovement:
	    if(LastHit != item) {
		suite_Reset(Suite, suite_Normalize);
		suiteev_ItemHighlight(self, item);
	    }
	    break;
	case view_RightUp:  break;
	case view_RightMovement:
	    if(LastHit != item) {
		if(Highlighted(item))
		    suiteev_ItemNormalize(self, item);
		else {
		    suite_Reset(Suite, suite_Normalize);
		    suiteev_ItemHighlight(self, item);
		}
	    }
	    break;
	case view_RightDown:
	    if(Highlighted(item))
		suiteev_ItemNormalize(self, item);
	    else {
		suite_Reset(Suite, suite_Normalize);
		suiteev_ItemHighlight(self, item);
	    }
	    break;
    }
    LastHit = item;
    OUT(suiteev_HandleExclusiveHit);
}

void
suiteev_HighlightFirstToLast( self, first, last )
  register struct suiteev *self;
  register struct suite_item *first;
  register struct suite_item *last;
{
    IN(suiteev_HighlightFirstToLast);
    if(Items && ITEM(0)) {
	register struct suite_item *item = NULL;
	boolean lastFound = FALSE;
	register int f, l, i = 0;

	while(item = ITEM(i++)) {
	    if(last == item) {
		lastFound = TRUE;
		break;
	    }
	    else if(first == item)
		break;
	}
	f = vector_Subscript(Items, first);
	l = vector_Subscript(Items, last);
	if(lastFound) {
	    for( i = 0; i < l && (item = ITEM(i)); i++ )
		if(Exposed(item) && Active(item) && Highlighted(item))
		    suiteev_ItemNormalize(self, item);
	    for( i = l; i <= f && (item = ITEM(i)); i++ )
		if(Exposed(item) && Active(item) && Normalized(item))
		    suiteev_ItemHighlight(self, item);
	}
	else {
	    for( i = f; i <= l && (item = ITEM(i)); i++ )
		if(Active(item) && Exposed(item) && Normalized(item))
		    suiteev_ItemHighlight(self, item);
	}
	if(item)
	    while(item = ITEM(i++))
		if(Exposed(item) && Active(item) && Highlighted(item))
		    suiteev_ItemNormalize(self, item);
    }
    OUT(suiteev_HighlightFirstToLast);
}

void
suiteev_HandleInclusiveHit( self, item, action, x, y, numberOfClicks )
  register struct suiteev *self;
  register struct suite_item *item;
  register enum view_MouseAction action;
  register long x, y, numberOfClicks;
{
    IN(suiteev_HandleInclusiveHit);
    switch(action) {
	case view_LeftMovement:
	    if(LastHit != item)
		suiteev_HighlightFirstToLast(self, FirstHit, item);
	    break;
	case view_LeftDown:
	    if(FirstHit != LastHit || FirstHit != item)
		suite_Reset(Suite, suite_Normalize);
	    if(Normalized(item)) 
		suiteev_ItemHighlight(self, item);
	    FirstHit = item;
	    break;
	case view_RightUp:
	case view_LeftUp:
	    break;
	case view_RightMovement:
	    if(LastHit != item) 
		suiteev_ItemToggle(self, item);
	    break;
	case view_RightDown:
	    FirstHit = item;
	    suiteev_ItemToggle(self, item);
	    break;
    }
    LastHit = item;
    OUT(suiteev_HandleInclusiveHit);
}

void
suiteev_HandleToggleHit( self, item, action, x, y, numberOfClicks )
  register struct suiteev *self;
  register struct suite_item *item;
  register enum view_MouseAction action;
  register long x, y, numberOfClicks;
{
    IN(suiteev_HandleToggleHit);
    if((action == view_LeftDown) || (action == view_RightDown))
	suiteev_ItemToggle(self, item);
    OUT(suiteev_HandleToggleHit);
}

struct view *
suiteev__Hit( self, action, x, y, numClicks )
register struct suiteev *self;
register enum view_MouseAction action;
register long x, y, numClicks;
{
    register struct suite_item *item = NULL;
    struct rectangle r;
    register struct view *ret = (struct view *) self;

    IN(suiteev_Hit);
    suiteev_GetVisualBounds(self, &r);
    if(WithinRect(x, y, &r)) {
	if((item = suiteev_WhichItem(self, x, y)) && Active(item)) {
	    CurrentItem = item;
	    switch(SelectionMode) {
		case suite_Toggle:
		    suiteev_HandleToggleHit(self, item, action, x, y, numClicks);
		    break;
		case suite_Exclusive:
		    suiteev_HandleExclusiveHit(self, item, action, x, y, numClicks);
		    break;
		case suite_Inclusive:
		    suiteev_HandleInclusiveHit(self, item, action, x, y, numClicks);
		    break;
	    }
	    if(HitHandler && !item->hithandler)
		HitHandler(ClientAnchor, Suite, item, suite_ItemObject, action, x, y, numClicks);
	    if(action != view_LeftUp && action != view_RightUp) {
		if(CVIF && (CVIF != item->viewobject))
		    CVIF = NULL;
		if(item->viewobject)
		    CVIF = item->viewobject;
	    }
	    ret = suiteev_ItemHit(self, item, action, x, y, numClicks);
	}
	else if(HitHandler && (!item || Active(item)))
	    HitHandler(ClientAnchor, Suite, item, suite_NoObject, action, x, y, numClicks);
    }
    OUT(suiteev_Hit);
    return(ret);
}

static
ItemFullUpdate( self, item, type, left, top, width, height )
  register struct suiteev *self;
  register struct suite_item *item;
  register enum	view_UpdateType type;
  register long left, top, width, height;
{
  struct rectangle *r = NULL;

  IN(ItemFullUpdate);
  if(!ItemCursor && !item->cursor && item->cursorbyte) {
    item->cursor = cursor_Create(self);
    if(item->cursor && item_CursorFont && item_CursorByte) 
      cursor_SetGlyph(item->cursor, item_CursorFont, item_CursorByte);
    else if(item_CursorByte) cursor_SetStandard(item->cursor, item_CursorByte);
    else cursor_SetStandard(item->cursor, Cursor_Octagon);
  }
  if(Active(item)) { 
    if(item_AccessType & suite_ReadWrite) {
	if(!item->dataobject) suiteev_SetItemToReadWrite(self, item);
	if(!(item_BorderStyle & suite_Invisible)) 	
	    suiteev_DrawItemBorder(self, item);
	if( item->title ) suiteev_ItemDrawTitle(self, item, NOFORCEDMODE);
	r = rectangle_Duplicate(&InsetRect);
	DrawRect(self, item, r, TRUE);
	DecrementRect(r, item_BorderSize);
	suitecv_LinkTree((struct suitecv *) item->viewobject, self);
	suitecv_InsertView((struct suitecv *) item->viewobject, self, r);
	suitecv_FullUpdate((struct suitecv *) item->viewobject, type, 0, 0, r->width, r->height);
    }
    else if(item->viewobject) {
	if(!(item_BorderStyle & suite_Invisible))
	    suiteev_DrawItemBorder(self, item);
	if(item->title)
	    suiteev_ItemDrawTitle(self, item, graphic_WHITE);
	view_LinkTree(item->viewobject, self);
	view_InsertView(item->viewobject, self, &InsetRect);
	if(item_ActiveForegroundColor) {
	    if(graphicIsMono) {
		view_SetForegroundColor(item->viewobject, item_ActiveForegroundColor, 0, 0, 0);
		view_SetBackgroundColor(item->viewobject, item_ActiveBackgroundColor, 0, 0, 0);
	    }
	    else {
		view_SetForegroundColor(item->viewobject, "black", 0, 0, 0);
		view_SetBackgroundColor(item->viewobject, "white", 0, 0, 0);
	    }
	}
	view_FullUpdate(item->viewobject, type, 0, 0, InsetWidth, InsetHeight);
    }
    else if(item_DataObjectName) {
      if(!item->dataobject) { 
	item->dataobject = (struct dataobject*)class_NewObject(item_DataObjectName);
	if(item_DataObjectHandler)
	  item_DataObjectHandler(ClientAnchor,Suite,item);
      }
      if(!item_ViewObjectName) {
	if(!item->viewobjectname) 
	  AllocNameSpace(&ItemViewObjectName, dataobject_ViewName(item->dataobject));
      }
      if(!item->viewobject) { 
	item->viewobject = (struct view*)class_NewObject(item_ViewObjectName);
	if(item_ViewObjectHandler)
	  item_ViewObjectHandler(ClientAnchor,Suite,item);
      }
      if(!(item_BorderStyle & suite_Invisible))
        suiteev_DrawItemBorder(self,item);
      if(item->title)
	  suiteev_ItemDrawTitle(self, item, graphic_WHITE);
      view_LinkTree(item->viewobject,self);
      view_InsertView(item->viewobject,self,&InsetRect);
      if(item_ActiveForegroundColor) {
	  if(!graphicIsMono) {
	      view_SetForegroundColor(item->viewobject, item_ActiveForegroundColor, 0, 0, 0);
	      view_SetBackgroundColor(item->viewobject, item_ActiveBackgroundColor, 0, 0, 0);
	  }
	  else {
	      view_SetForegroundColor(item->viewobject, "black", 0, 0, 0);
	      view_SetBackgroundColor(item->viewobject, "white", 0, 0, 0);
	  }
      }
      view_FullUpdate(item->viewobject, type, 0, 0, InsetWidth, InsetHeight);
    }
    else if(Normalized(item)) suiteev_ItemNormalize(self,item);
    else if(Highlighted(item)) suiteev_ItemHighlight(self,item);
  }
  else if(ItemPassiveStyle & suite_Pale)
      suiteev_ItemShade(self, item);
  if(item->cursor || ItemCursorByte)
    suiteev_PostCursor(self, &Bounds, item_Cursor);
  OUT(ItemFullUpdate);
}

void
suiteev__ItemUpdate( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemUpdate);
  if(item->viewobject && item->dataobject) {
      view_Update(item->viewobject);
  }
  else {
      suiteev_ItemClear(self,item);
      ItemFullUpdate(self,item,view_FullRedraw,0,0,0,0);
  }
  OUT(suiteev_ItemUpdate);
}

struct view *
suiteev__ItemHit( self, item, action, x, y, numClicks )
  register struct suiteev *self;
  register struct suite_item *item;
  register enum view_MouseAction action;
  register long x, y, numClicks;
{
  struct view *retval = (struct view*)self;

  IN(suiteev_ItemHit);
  if(item && Active(item)) {
    if(!CVIF || (item->viewobject && (item->viewobject == CVIF))) {
      if(item_AccessType & suite_ReadWrite) {
	if(WithinRect(x,y,&InsetRect)) {
	    return((struct view*)suitecv_Hit((struct suitecv*)item->viewobject,
				action,x - InsetLeft,y - InsetTop,numClicks));
	}
	else 
	  return((struct view*)suitecv_Hit((struct suitecv*)item->viewobject,
		    action,0,0,numClicks));
      }
      else if(item->viewobject) 
	  retval = view_Hit(item->viewobject,action,x - InsetLeft,
		    y - InsetTop,numClicks);
      if(item->hithandler) 
	  item->hithandler(ClientAnchor,Suite,item,suite_ItemObject,
			   action,x,y,numClicks);
    }
  }
  OUT(suiteev_ItemHit);
  return((struct view*)retval);
}

void
suiteev__ItemClear( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{ struct rectangle r;
  IN(suiteev_ItemClear);
  rectangle_SetRectSize(&r, Left, Top, Width, Height);
  SetBackgroundShade(self);
  suiteev_FillRect(self, &r, NULL);
  OUT(suiteev_ItemClear);
}

void
suiteev__ItemBlackOut( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  struct rectangle *r;

  IN(suiteev_ItemBlackOut);
  r = rectangle_Duplicate(&Bounds);
  if(!graphicIsMono) {
      DrawRect(self, item, r, Highlighted(item));
      rectangle_SetRectSize(r, Left + 1, Top + 1, Width - 2, Height - 2);
      DecrementRect(r, item_BorderSize);
  }
  else {
      SetTransferMode(self, graphic_INVERT);
      suiteev_FillRect(self, r, NULL);
  }
  SetTransferMode(self, graphic_COPY);
  OUT(suiteev_ItemBlackOut);
}

void
suiteev__ItemHighlightReverseVideo( self, item, border )
  register struct suiteev *self;
  register struct suite_item *item;
  boolean border;
{
  struct rectangle *rect = rectangle_Duplicate(&Bounds);

  IN(suiteev_ItemHighlightReverseVideo);
  if(item_BorderStyle & suite_Rectangle)
    suiteev_ItemBlackOut(self,item);
  OUT(suiteev_ItemHighlightReverseVideo);
}

void
suiteev__ItemHighlightBorder( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemHighlightBorder);
  suiteev_DrawItemBorder(self,item);
  OUT(suiteev_ItemHighlightBorder);
}

void
suiteev__ItemHighlightCaptionBoldItalic( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemHighlightCaptionBoldItalic);
  item->captionfonttype = fontdesc_Bold | fontdesc_Italic;
  item->captionfont = NULL;
  OUT(suiteev_ItemHighlightCaptionBoldItalic);
}

void
suiteev__ItemHighlightCaptionBold( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemHighlightCaptionBold);
  item->captionfonttype = fontdesc_Bold;
  item->captionfont = NULL;
  OUT(suiteev_ItemHighlightCaptionBold);
}

void
suiteev__ItemHighlightCaptionItalic( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemHighlightCaptionItalic);
  item->captionfonttype = fontdesc_Italic;
  item->captionfont = NULL;
  OUT(suiteev_ItemHighlightCaptionItalic);
}

void
suiteev__ItemNormalize( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemNormalize);
  if(item_AccessType & suite_ReadWrite) return;
  suiteev_ItemClear(self,item);
  item->mode = ((item_Active | item_Normalized) & ~item_Highlighted);
  if(!(item_BorderStyle & (suite_Invisible | suite_None)))
    suiteev_DrawItemBorder(self,item);
  if(item_HighlightStyle & (suite_Italic | suite_Bold)) {
    item->captionfonttype = fontdesc_Plain;
    item->captionfont = NULL;
  }
  if(item->title) 
      suiteev_ItemDrawTitle(self,item,NOFORCEDMODE);
  if(item->viewobject) 
    view_FullUpdate(item->viewobject,view_FullRedraw,0,0,Width,Height);
  else if(item_Caption) suiteev_ItemDrawCaption(self,item,NOFORCEDMODE);
  OUT(suiteev_ItemNormalize);
}

#define BorderPlusOne (ItemBorderSize+1)
#define TwiceBorderPlusOne (BorderPlusOne*2)

static void
ItemPlaceCaption( self, item, captionwidth, captionheight, place )
  register struct suiteev *self;
  register struct suite_item *item;
  long captionwidth;
  long captionheight;
  unsigned *place;
{
  unsigned char alignment = 0;
  long l = Left + ItemBorderSize, t = Top + ItemBorderSize;
  long w = Width - (2*ItemBorderSize);
  long h = Height - (2*ItemBorderSize);

  IN(ItemPlaceCaption);
  if(item) {
    alignment = item_CaptionAlignment;
    if(captionwidth > w) captionwidth = w;
    if(captionheight > h) captionheight = h;
    rectangle_SetRectSize(&CaptionRect,
			   l + ((w - captionwidth)/2),
			   t + ((h - captionheight)/2),
			   captionwidth,captionheight);
    if(alignment & suite_Left) CaptionRect.left = l;
    if(alignment & suite_Right) CaptionRect.left = l + w - captionwidth;
    if(alignment & suite_Top) CaptionRect.top = t;
    if(alignment & suite_Bottom) CaptionRect.top = t + h - captionheight;
  }
  *place = graphic_BETWEENTOPANDBOTTOM;
  if(alignment & suite_Middle) *place |= graphic_BETWEENTOPANDBOTTOM;
  if(alignment & suite_Center) *place |= graphic_BETWEENLEFTANDRIGHT;
  if(alignment & suite_Left) *place |= graphic_ATLEFT;
  if(alignment & suite_Right) *place |= graphic_ATRIGHT;
  OUT(ItemPlaceCaption);
}

void
suiteev__ItemDrawCaption( self, item, forcedTransferMode )
  register struct suiteev *self;
  register struct suite_item *item;
  short forcedTransferMode;
{
  long captionwidth = 0, captionheight = 0, totalWidth = 0;
  long X = 0, Y = 0, SubStringIndex = 0;
  unsigned tMode = graphic_COPY, placement = 0;
  unsigned alignment = item_CaptionAlignment;
  register long pos = 0, i = 0, j = 0, numLines = 0;
  char *tmp = NULL, *head = NULL, save;
  boolean WasBreakPos = FALSE, WasNewLine = FALSE;
  boolean dontDraw = FALSE;
  unsigned newlineHeight;

  IN(suiteev_ItemDrawCaption);
  numLines = suiteev_LineCount(self, item_Caption) + BreakCount(item);
  if(forcedTransferMode != NOFORCEDMODE)
      tMode = forcedTransferMode; 
  item->captionfontsize = CaptionFontSize;
  item->captionfont = fontdesc_Create(item_CaptionFontName, item_CaptionFontType, item->captionfontsize);
  SetFont(self, item->captionfont);
  suiteev_SetClippingRect(self, &Bounds);
  if(List) {
      totalWidth = MaxListSubStringWidth(self, item, item_Caption, item_CaptionFont);
      if(item_Title) totalWidth += TitleRect.width;
      if((totalWidth > Width) && (Width > (4 * CaptionMWidth))) {
	  suiteev_Update(self);
	  return;
      }
  }
  newlineHeight = fontdesc_FontSummary(item_CaptionFont, suite_GetDrawable(Suite))->newlineHeight;
  captionheight = numLines * newlineHeight;
  ItemPlaceCaption(self, item, captionwidth, captionheight, &placement);
  if(forcedTransferMode == NOFORCEDMODE)
      if((item_HighlightStyle & suite_Invert) && Highlighted(item))
	  tMode = graphic_WHITE;
      else tMode = graphic_COPY;
  if (graphicIsMono) {
      SetTransferMode(self, tMode);
  } else {
      SetTransferMode(self, graphic_COPY);
  }

  if(Active(item)) {
      if(graphicIsMono)
	  suiteev_SetForegroundColor(self, "black", 0, 0, 0);
      else
	  suiteev_SetForegroundColor(self, item_CaptionColor, 0, 0, 0);
  }
  else {
      if(graphicIsMono)
	  suiteev_SetForegroundColor(self, "grey50", 0, 0, 0);
      else
	  suiteev_SetForegroundColor(self, item_CaptionColor, 0, 0, 0);
  }
  tmp = head = item_Caption;
  Y = CaptionRect.top + newlineHeight/2;
  X = CaptionRect.left + CaptionRect.width/2;
  if(alignment & suite_Left) X = CaptionRect.left;
  if(alignment & suite_Right) X = CaptionRect.left + CaptionRect.width;
  for( j = 0, i = 0 ; i < numLines ; i++ ) {
    WasNewLine = WasBreakPos = FALSE;
    save = '\0';
    while(tmp && (*tmp != '\0')) {
      if(*tmp == '\n') {
        WasNewLine = TRUE;
	break;
      }
      else if((j < BreakCount(item)) && (pos == BreakPos(item,j))) {
	WasBreakPos = TRUE;
	break;
      }
      else {
	pos++;
	tmp++;
      }
    }
    if(WasNewLine) *tmp = '\0';
    else if(WasBreakPos) {
      save = *(tmp + 1);
      *(tmp+1) = (char)0;
    }
    if(List) {
      if(i > 0) {
	if(WrapStyle & suite_LeftIndent) {
          X = Left + CaptionMWidth + (2 * CaptionMWidth);
	  placement = graphic_ATLEFT | graphic_BETWEENTOPANDBOTTOM;
	}
	else if(WrapStyle & suite_LeftRight) {
	  X = Left + Width - (2 * CaptionMWidth);
	  placement = graphic_ATRIGHT | graphic_BETWEENTOPANDBOTTOM;
	}
      }
      else {
	X = Left + CaptionMWidth;
	placement = graphic_ATLEFT | graphic_BETWEENTOPANDBOTTOM;
      }
      if(item == FirstVisible && i < FirstVisibleSubString) 
	dontDraw = TRUE;
      else dontDraw = FALSE;
    }
    if(i > 0) 
	Y += (newlineHeight + 2);
    if(!List ||
	(Y + (newlineHeight/2) + 1) < suiteev_GetVisualBottom(self)) {
      if(!dontDraw) {
	suiteev_MoveTo(self, X, Y);
	suiteev_DrawString(self, head, placement);
      }
    }
    if(WasNewLine) *tmp = '\n'; 
    else if(WasBreakPos) {
      *(tmp + 1) = save;
      j++;
    }
    tmp++;
    pos++;
    head = tmp;
  }
  SetTransferMode(self, graphic_COPY);
  suiteev_ClearClippingRect(self);
  OUT(suiteev_ItemDrawCaption);
}


void
ItemPlaceTitle(self, item, titlewidth, titleheight, newlineHeight)
  struct suiteev *self;
  struct suite_item *item;
  long titlewidth, titleheight;
  int newlineHeight;
{
  unsigned char titleplacement = item_TitlePlacement;
  boolean left = FALSE, right = FALSE, top = FALSE, bottom = FALSE;

  IN(suiteev_ItemPlaceTitle);
  if(titleplacement & suite_Left) {
    left = TRUE;
    rectangle_SetRectSize(&TitleRect,
			   Left + BorderPlusOne,
			   Top + BorderPlusOne,
			   titlewidth + 2,
			   Height - TwiceBorderPlusOne);
    rectangle_SetRectSize(&InsetRect,
			   TitleRect.left + TitleRect.width + 1,
			   Top + BorderPlusOne,
			   Width - TitleRect.width - TwiceBorderPlusOne - 1, 
			   Height - TwiceBorderPlusOne);
  }
  else if(titleplacement & suite_Right) {
    right = TRUE;
    rectangle_SetRectSize(&TitleRect,
			   Left + Width - (titlewidth + 2),
			   Top + BorderPlusOne,
			   titlewidth + 2,
			   Height - TwiceBorderPlusOne);
    rectangle_SetRectSize(&InsetRect,
			   Left + BorderPlusOne,
			   Top + BorderPlusOne,
			   Width - TitleRect.width - TwiceBorderPlusOne - 1, 
			   Height - TwiceBorderPlusOne);
  }
  else if(titleplacement & suite_Top) {
    top = TRUE;
    rectangle_SetRectSize(&TitleRect,
			   Left + BorderPlusOne,
			   Top + BorderPlusOne,
			   Width - TwiceBorderPlusOne,
			   titleheight + 2);
    rectangle_SetRectSize(&InsetRect,
			   Left + BorderPlusOne,
			   TitleRect.top + TitleRect.height + 1,
			   Width - TwiceBorderPlusOne,
			   Height - TitleRect.height - TwiceBorderPlusOne);
  }
  else if(titleplacement & suite_Bottom) {
    bottom = TRUE;
    rectangle_SetRectSize(&TitleRect,
			   Left + BorderPlusOne,
			   Top + Height - titleheight - BorderPlusOne,
			   Width - TwiceBorderPlusOne,
			   titleheight + 2);
    rectangle_SetRectSize(&InsetRect,
			   Left + BorderPlusOne,
			   Top + BorderPlusOne,
			   Width - TwiceBorderPlusOne,
			   Height - TitleRect.height - BorderPlusOne);
  }
  if(item_AccessType & suite_ReadWrite) {
    InsetHeight = (2 * newlineHeight);
    if(Height < InsetHeight) 
      InsetHeight = Height - 4;
    if(left || right) 
      InsetTop = TitleRect.top + (TitleRect.height - InsetHeight)/2;
    else if(top) 
      InsetTop = TitleRect.top + TitleRect.height + 2;
    else if(bottom) 
      InsetTop = TitleRect.top + 2 + InsetHeight;
  }
  OUT(suiteev_ItemPlaceTitle);
}

void
suiteev__ItemDrawTitle( self, item, forcedTransferMode )
  register struct suiteev *self;
  register struct suite_item *item;
  short forcedTransferMode;
{
  long x = 0, y = 0, count = 0;
  char *tmp = NULL, *head = NULL;
  unsigned tMode = 0, alignment = 0;
  unsigned placement = graphic_BETWEENTOPANDBOTTOM;
  long titlewidth = 0, titleheight = 0;
  long titleLines = 0;
  int newlineHeight;

  IN(suiteev_ItemDrawTitle);
  tMode = (forcedTransferMode == NOFORCEDMODE) ? 
    graphic_COPY : forcedTransferMode; 
  item->titlefont = fontdesc_Create(item_TitleFontName, item_TitleFontType, item_TitleFontSize);
  SetFont(self, item->titlefont);
  suiteev_SetClippingRect(self, &Bounds);
  if(item_Title) {
    fontdesc_StringBoundingBox(item->titlefont, suiteev_GetDrawable(self), item_Title, &titlewidth, &titleheight);
    titleLines = suiteev_LineCount(self, item_Title);
    newlineHeight = fontdesc_FontSummary(item->titlefont, suite_GetDrawable(Suite))->newlineHeight;
    titleheight = titleLines * newlineHeight;
    alignment = item_TitleCaptionAlignment;
    if(alignment & suite_Left) placement |= graphic_ATLEFT;
    else if(alignment & suite_Right) placement |= graphic_ATRIGHT;
    else placement |= graphic_BETWEENLEFTANDRIGHT;
    ItemPlaceTitle(self, item, titlewidth, titleheight, newlineHeight);
  }
#if 0
    else if(item->titleviewobject) {
	titlewidth = Width/3;
	titleheight = Height/3;
    }
#endif
  if(forcedTransferMode == NOFORCEDMODE) {
    if((item_HighlightStyle & suite_Invert) && Highlighted(item)) 
      tMode = graphic_WHITE;
    else tMode = graphic_COPY;
  }
  if (graphicIsMono) {
      SetTransferMode(self, tMode);
  } else {
      SetTransferMode(self, graphic_COPY);
  }

  if(Active(item)) {
      if(!graphicIsMono) {
	  suiteev_SetForegroundColor(self, item_ActiveForegroundColor, 0, 0, 0);
	  suiteev_SetBackgroundColor(self, item_ActiveBackgroundColor, 0, 0, 0);
      }
      else {
	  suiteev_SetForegroundColor(self, "black", 0, 0, 0);
	  suiteev_SetBackgroundColor(self, "white", 0, 0, 0);
      }
  }
  else {
      if(!graphicIsMono) {
	  suiteev_SetForegroundColor(self, PassiveItemForeground, 0, 0, 0);
	  suiteev_SetBackgroundColor(self, PassiveItemBackground, 0, 0, 0);
      }
      else {
	  suiteev_SetForegroundColor(self, "gray50", 0, 0, 0);
	  suiteev_SetBackgroundColor(self, "white", 0, 0, 0);
      }
  }
  if(item_Title) {
    head = item_Title;
    if(alignment & suite_Left) x = TitleRect.left;
    else if(alignment & suite_Right) x = TitleRect.left + TitleRect.width;
    else x = TitleRect.left + TitleRect.width/2;
    if(alignment & suite_Top) y = TitleRect.top + newlineHeight/2;
    else if(alignment & suite_Bottom) 
      y = TitleRect.top + TitleRect.height - titleheight + newlineHeight/2;
    else y = TitleRect.top + (TitleRect.height - titleheight)/2 + newlineHeight/2;
    for( count = 0; (count < titleLines) && (head != '\0'); count++) {
      if(tmp = (char*) index(head,'\n')) *tmp = (char)0;
      suiteev_MoveTo(self, x, y + (newlineHeight * count));
      suiteev_DrawString(self, head, placement);
      if(tmp) {
	*tmp = '\n';
	head = tmp + 1;
      }
    }
  }
#if 0
    else if(item->titleviewobject)
	view_FullUpdate(item->titleviewobject,view_FullRedraw,0,0,
			  TitleRect.width,TitleRect.height);
#endif
  SetTransferMode(self, graphic_COPY);
  suiteev_ClearClippingRect(self);
  OUT(suiteev_ItemDrawTitle);
}

void
suiteev__ItemHighlight( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemHighlight);
  if( (item_HighlightStyle & suite_None) || 
      (item_AccessType & suite_ReadWrite)) 
      return;
  if(item_HighlightStyle & suite_Invert)
    suiteev_ItemHighlightReverseVideo(self, item, item_HighlightStyle & suite_Border);
  else if(!item->viewobject) 
      suiteev_ItemClear(self, item);
  if((item_HighlightStyle & suite_Bold) && (item_HighlightStyle & suite_Italic))
    suiteev_ItemHighlightCaptionBoldItalic(self, item);
  else {
    if(item_HighlightStyle & suite_Bold)
      suiteev_ItemHighlightCaptionBold(self, item);
    if(item_HighlightStyle & suite_Italic) 
      suiteev_ItemHighlightCaptionItalic(self, item);
  }
  item->mode = ((item_Active | item_Highlighted) & ~item_Normalized);
  if(!(item_BorderStyle & suite_None)) 
      suiteev_DrawItemBorder(self, item);
  if(item->title) 
      suiteev_ItemDrawTitle(self, item, NOFORCEDMODE);
  if(item_Caption && !(item_AccessType & suite_ReadWrite)) 
    suiteev_ItemDrawCaption(self, item, NOFORCEDMODE);
  OUT(suiteev_ItemHighlight);
}

void
suiteev__ItemClearCaption( self, item )
  struct suiteev *self;
  struct suite_item *item;
{
  IN(suiteev_ItemClearCaption);
  if(item_Caption) 
      suiteev_ItemDrawCaption(self, item, graphic_WHITE);
  OUT(suiteev_ItemClearCaption);
}

void
suiteev__ItemShade( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  short int shade = 0;
  struct rectangle *rect = rectangle_Duplicate(&Bounds);

  IN(suiteev_ItemShade);
  item->mode &= ~item_Active;
  item->mode &= ~item_Normalized;
  if(!(item_BorderStyle & suite_None))
      suiteev_DrawItemBorder(self, item);
  if(item->title)
    suiteev_ItemDrawTitle(self, item, graphic_WHITE);
  if(item_Caption)
    suiteev_ItemDrawCaption(self, item, graphic_WHITE);
  OUT(suiteev_ItemShade);
}

long
suiteev__Locate( self, x, y )
  register struct suiteev *self;
  register long x, y;
{
  register struct suite_item *item = NULL;
  register long i = 0;
  long realLeft = 0, realTop = 0, realWidth = 0, realHeight = 0;
  long YGutterOffset = YGutterSize + 1;
  long XGutterOffset = XGutterSize + 1;

  IN(suiteev_Locate);
  if(Items && ITEM(0) && (i = vector_Subscript(Items, FirstVisible)) != -1) {
    if(!x) 
      while(item = ITEM(i)) {
	realTop = Top - (YGutterOffset/2);
	realHeight = Height + YGutterOffset;
	if(suite_ItemExposed(Suite, item) &&
	   (realTop <= y) && (y <= (realTop + realHeight))) {
	  if((item == FirstVisible) && ((i+1) < vector_Count(Items))) 
	    return(i+1);
	  else return(i);
	}
	else i++;
      }
    else if(!y)
      while(item = ITEM(i)) {
	realLeft = Left - (XGutterOffset/2);
	realWidth = Width + XGutterOffset;
	if(suite_ItemExposed(Suite, item) &&
	   (realLeft <= x) && (x <= (realLeft + realWidth))) return(i);
	else i++;
      }
  }
  else return(0);
  OUT(suiteev_Locate);
  return(vector_Subscript(Items,FirstVisible)); /* === */
}

void
suiteev__DrawItemBorder( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  register long i = 0;
  struct rectangle *rect = NULL;

  IN(suiteev_DrawItemBorder);
  SetTransferMode(self, graphic_COPY);
  rect = rectangle_Duplicate(&Bounds);
  if(item_BorderStyle & suite_Rectangle) {
      DrawRect(self, item, rect, Highlighted(item));
      DecrementRect(rect, item_BorderSize);
  }
  rectangle_SetRectSize(&InsetRect, rect->left, rect->top, rect->width, rect->height);
  OUT(suiteev_DrawItemBorder);
}

void
suiteev__ItemToggle( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  IN(suiteev_ItemToggle);
  if(Highlighted(item)) 
      suiteev_ItemNormalize(self, item);
  else if(Normalized(item)) 
      suiteev_ItemHighlight(self, item);
  OUT(suiteev_ItemToggle);
}

void
suiteev__AllocItemArray( self, count )
  register struct suiteev *self;
  long count;
{
  IN(suiteev_AllocItemArray);
  if(count > 0) 
    ItemArray = (struct suite_item **)
	calloc(count + 1, sizeof(struct suite_item *));
  OUT(suiteev_AllocItemArray);
}

static void
ReadWriteHandler( anchor, suite, item )
  register long anchor;
  register struct suite *suite;
  register struct suite_item *item;
{
  register struct text *txt = NULL;
  register struct suitecv *txtv = NULL;

  if((txt = (struct text*) suite_ItemDataObject(suite, item)) && 
     (txtv = (struct suitecv*) suite_ItemViewObject(suite, item))) {
    suitecv_SetBorder(txtv, 1, 1);
    suitecv_SetDataObject(txtv, txt);
  }
}

void
suiteev__SetItemToReadWrite( self, item )
  register struct suiteev *self;
  register struct suite_item *item;
{
  AllocNameSpace(&item->dataobjectname,"text");
  item->dataobject = (struct dataobject*) class_NewObject(item_DataObjectName);
  AllocNameSpace(&item->viewobjectname,"suitecv");
  item->viewobject = (struct view*) class_NewObject(item_ViewObjectName);
  ((struct suitecv*)item->viewobject)->parent_EV = self;
  ((struct suitecv*)item->viewobject)->parent_item = item;
  item->viewobjecthandler = (long(*)())ReadWriteHandler;
  ReadWriteHandler(ClientAnchor, Suite, item);
  item->highlightstyle = suite_Border;
  if(item_Caption) 
    text_AlwaysInsertCharacters((struct text*)item->dataobject, 0, item_Caption, strlen(item_Caption));
}

static void
MaxSubStringSize( self, item, str, font, w, h )
  struct suiteev *self;
  struct suite_item *item;
  char *str;
  struct fontdesc *font;
  long int *w, *h;
{
  long int HMax = 0, WMax = 0;
  char *tmp = NULL, *head = NULL, save;
  register int j = 0, pos = 0, i = 0;

  *w = *h = 0;
  tmp = head = str;
  while(tmp && *tmp != (char)0) {
    if(*tmp == '\n') {
      *tmp = '\0';
      fontdesc_StringBoundingBox(font, suiteev_GetDrawable(self), head, w, h);
      if(*w > WMax) {
	WMax = *w;
      }
      if(*h > HMax) {
	HMax = *h;
      }
      *tmp++ = '\n';
      pos++;
      head = tmp;
      i++;
    }
    else if((j < BreakCount(item)) && (pos == BreakPos(item, j))) {
      save = *(item_Caption + pos + 1);
      *(item_Caption + pos + 1) = '\0';
      fontdesc_StringBoundingBox(font, suiteev_GetDrawable(self), head, w, h);
      if(*w > WMax) {
	WMax = *w;
      }
      if(*h > HMax) {
	HMax = *h;
      }
      *(item_Caption + pos + 1) = save;
      tmp++;pos++;
      head = tmp;
      j++;
      i++;
    }
    else { 
      tmp++;
      pos++; 
    }
  }
  if(*tmp == '\0' && *head != '\0' ) {
      fontdesc_StringBoundingBox(font, suiteev_GetDrawable(self), head, w, h);
      if(*w > WMax)
	  WMax = *w;
      if(*h > HMax) 
	  HMax = *h;
  }
  *w = WMax;
  *h = fontdesc_FontSummary(font, suite_GetDrawable(Suite))->newlineHeight;
}

static long
MaxListSubStringWidth( self, item, str, font )
  struct suiteev *self;
  struct suite_item *item;
  char *str;
  struct fontdesc *font;
{
  long int WMax = 0, h = 0, w = 0;
  char *tmp = NULL, *head = NULL, save;
  register int j = 0, pos = 0, i = 0;

  w = 0;
  tmp = head = str;
  while(tmp && *tmp != '\0') {
    if(*tmp == '\n') {
      *tmp = '\0';
      fontdesc_StringBoundingBox(font, suiteev_GetDrawable(self), head, &w, &h);
      if(i > 0) w += (2 * CaptionMWidth);
      if(w > WMax) WMax = w;
      *tmp++ = '\n';
      pos++;
      head = tmp;
      i++;
    }
    else if((j < BreakCount(item)) && (pos == BreakPos(item,j))) {
      save = *(item_Caption + pos + 1);
      *(item_Caption + pos + 1) = '\0';
      fontdesc_StringBoundingBox(font, suiteev_GetDrawable(self), head, &w, &h);
      if(i > 0) w += (2 * CaptionMWidth);
      if(w > WMax) WMax = w;
      *(item_Caption + pos + 1) = save;
      tmp++;
      pos++;
      head = tmp;
      j++;
      i++;
    }
    else { 
      tmp++;
      pos++; 
    }
  }
  if(*tmp == '\0' && *head != '\0' ) {
    fontdesc_StringBoundingBox(font, suiteev_GetDrawable(self), head, &w, &h);
    if(i > 0) w += (2 * CaptionMWidth);
    if(w > WMax) WMax = w;
  }
  return(WMax);
}

void
suiteev__MaxStringSize( self, width, height )
  struct suiteev *self;
  long *width, *height;
{   
  register int i = 0;
  register struct suite_item *item = NULL;
  register long maxWidth = 0, maxHeight = 0;
  long XWidth = 0, YWidth = 0;
  register numLines = 0;

  IN(MaxStringSize);
  while(item = ITEM(i++))
    if(Exposed(item)) {
      if(item_Caption) {
	numLines = suiteev_LineCount(self, item_Caption) + BreakCount(item);
	MaxSubStringSize(self, item, item_Caption, item_CaptionFont, &XWidth, &YWidth);
	YWidth *= numLines;
      }
      if(item->title) {
	long titleYWidth = 0, titleXWidth = 0;
	numLines = suiteev_LineCount(self, item->title);
	MaxSubStringSize(self, item, item->title, item->titlefont ? item->titlefont : item_CaptionFont, &titleXWidth, &titleYWidth);
	if(TitlePlacement & (suite_Left | suite_Right))
	    XWidth += titleXWidth;
	if(TitlePlacement & (suite_Top | suite_Bottom))
	  YWidth += (numLines * titleYWidth);
      }
      if(XWidth > maxWidth) 
	  maxWidth = XWidth;
      if(YWidth > maxHeight) 
	  maxHeight = YWidth;
    }
  *width = maxWidth;
  *height = maxHeight;
  OUT(MaxStringSize);
}

long
suiteev__LineCount( self, str )
  register struct suiteev *self;
  register char *str;
{
  register long number = 1;
  register char *tmp = str;

  if(tmp)
    while(*tmp != (char)0) 
      if(*tmp++ == '\n') 
        number++;
  return(number);
}

static void 
DrawRectSize(self, x, y, width, height)
  struct suiteev	*self;
  long x, y, width, height;
{
  long left = x;
  long right = x+width-1;
  long top = y;
  long bottom = y+height-1;

  if (left > right) left = right;
  if(top > bottom) top = bottom;
  suiteev_MoveTo(self, left, top);
  suiteev_DrawLineTo(self, right, top);
  suiteev_DrawLineTo(self, right, bottom);
  suiteev_DrawLineTo(self, left, bottom);
  suiteev_DrawLineTo(self, left, top);
}

static struct sbutton_info thebutton = {
    NULL,   /* the prefs struct will be filled later */
    "",	    /* the label is empty */
    0,	    /* the rock isn't needed */
    NULL,   /* ditto for the trigger atom */
    FALSE,  /* initially not lit, will be set appropriately */
};

static void 
DrawRect(self, item, Rect, lit)
  struct suiteev *self;
  struct suite_item *item;
  struct rectangle *Rect;
  boolean lit;
{
  struct rectangle *childrect = rectangle_Duplicate(Rect);
  struct region *re1 = region_CreateEmptyRegion(), *re2 = region_CreateEmptyRegion();

  if(!graphicIsMono) {
      if(!re1 || !re2) {
	  if(re1) region_Destroy(re1);
	  if(re2) region_Destroy(re2);
	  return;
      }
      SetTransferMode(self, graphic_COPY);
      DecrementRect(childrect, item_BorderSize);
      if(Active(item)) {
	  self->buttonPrefsActive->bdepth = item_BorderSize;
	  if (item_ActiveBackgroundColor) {
	      self->buttonPrefsActive->colors[sbutton_BACKGROUND] = item_ActiveBackgroundColor;
	      suiteev_SetBackgroundColor(self, item_ActiveBackgroundColor, 0, 0, 0);
	  }
	  if (item_ActiveForegroundColor) {
	      self->buttonPrefsActive->colors[sbutton_FOREGROUND] = item_ActiveForegroundColor;
	      suiteev_SetForegroundColor(self, item_ActiveForegroundColor, 0, 0, 0);
	  }
	  thebutton.prefs = self->buttonPrefsActive;
	  thebutton.lit = lit;
      }
      else {
	  self->buttonPrefsPassive->bdepth = item_BorderSize;
	  if (PassiveItemBackground) {
	      self->buttonPrefsPassive->colors[sbutton_BACKGROUND] = PassiveItemBackground;
	      suiteev_SetBackgroundColor(self, PassiveItemBackground, 0, 0, 0);
	  }
	  if (PassiveItemForeground) {
	      self->buttonPrefsPassive->colors[sbutton_FOREGROUND] = PassiveItemForeground;
	      suiteev_SetForegroundColor(self, PassiveItemForeground, 0, 0, 0);
	  }
	  thebutton.prefs = self->buttonPrefsPassive;
	  thebutton.lit = FALSE;
      }
      sbuttonv_DrawButton(self, &thebutton, Rect);
      region_RectRegion(re2, childrect);
      region_Destroy(re1);
      region_Destroy(re2);
      free(childrect);
  }
  else {
      register int i = item_BorderSize;
      suiteev_SetBackgroundColor(self, "white", 0, 0, 0);
      if(Active(item))
	  suiteev_SetForegroundColor(self, "black", 0, 0, 0);
      else
	  suiteev_SetForegroundColor(self, "gray50", 0, 0, 0);
      while(i > 0) {
	  DrawRectSize(self, rectangle_Left(Rect), rectangle_Top(Rect), rectangle_Width(Rect), rectangle_Height(Rect));
	  DecrementRect(Rect, 1);
	  i--;
      }
  }
}


void
suiteev__LinkTree(self, parent)
    struct suiteev *self;
    struct view *parent;
{
    register int i = 0;
    register struct suite_item *item = NULL;

    super_LinkTree(self, parent);
    if(suiteev_GetIM(self)) {
	if(Items && ITEM(0) && (FirstVisible || NewFirstVisible)) {
	    CheckForNewFirstVisible(self);
	    i = vector_Subscript(Items, FirstVisible);
	    while(item = ITEM(i++))
		if(Exposed(item)) {
		    if(Active(item)) { 
			if(item_AccessType & suite_ReadWrite)
			    suitecv_LinkTree((struct suitecv *) item->viewobject, self);
			else if(item->viewobject)
			    view_LinkTree(item->viewobject, self);
		    }
		    if(item == LastVisible) 
			break;
		}
	}
    }
}
