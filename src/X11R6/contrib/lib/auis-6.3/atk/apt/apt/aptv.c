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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apt/apt/RCS/aptv.c,v 1.27 1993/08/25 20:37:07 susan Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Apt View-object

MODULE	aptv.c

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Apt View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/23/88	Created (TCP)
  05/01/89	Change cursor font-names (shorten) (TCP)
  05/08/89	Move Cursor-setting in FullUpdate to within FullRedraw (TCP)
  05/09/89	Use GetDrawable in StringSize (TCP)
  05/18/89	Have SetDataObject do super_SetDataObject (TCP)
  05/22/89	Directly refer to Data-object Title/Legend strings (TCP)
		Correctly set fonts & sizes for each enclosure
  05/26/89	Remove area blanking for bounded string drawing (TCP)
  06/07/89	Added code the QueryFileName() and QueryDirectoryName()
	        to check for the occurance of "/.." and "/." at the 
		end of the result returned from completion_GetFilename().
		(GW Keim)
  06/16/89	Fixed a bug in aptv_Query{File,Directory}Name() that resulted when
                the completion_GetFilename() call failed.  Bogus if-then parenthesizing
		was the culprit that allowed some internal string manipulation and a
		aptv_Announce() call when the GetFilename() call was cancelled; (GW Keim)
  07/25/89	Remove arg from im_ForceUpdate (TCP)
  08/07/89	Only emit ShowPage when topLevel (TCP)
		Fix EndPS args -- drop PRF usage (TCP)
  08/24/89	Simplify use of macros for AIX compiler (TCP)
  10/07/89      Changed a few chained assignments in InitializeObject to
                single assignnments to satisfy the MIPS C compiler. (zs01)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <graphic.ih>
#include <fontdesc.ih>
#include <observe.ih>
#include <view.ih>
#include <cursor.ih>
#include <im.ih>
#include <attribs.h>
#include <filetype.ih>
#include <message.ih>
#include <complete.ih>
#include <text.ih>
#include <txttroff.ih>
#include <textv.ih>
#include <rect.h>
#include <math.h>
#include <apt.h>
#include <apts.ih>
#include <apt.ih>
#include <aptv.eh>
#include <ctype.h>

#define  Balanced		    (view_BETWEENLEFTANDRIGHT | view_BETWEENTOPANDBOTTOM)

#define  Initialized		      self->states.initialized

#define  Data			      self->data_object

#define  Cursor			      self->cursor

#define  OriginalWidth		      self->original_width
#define  OriginalHeight		      self->original_height
#define  DimensionWidth		      self->dimension_width
#define  DimensionHeight	      self->dimension_height

#define  Enclosure(e)		      self->enclosures[e]
#define  EnclosureAreas(e)	    (&Enclosure(e).areas)
#define  Areas(e)		     (EnclosureAreas(e))
#define  Area(e,a)		    (&Enclosure(e).areas[a])
#define  EnclosureBounds(e)	    (&Enclosure(e).bound)
#define  Bounds(e)		     (EnclosureBounds(e))

#define  Outer		   	      0
#define  Border			      1
#define  Control	    	      2
#define  Title			      3
#define  Legend			      4
#define  Body		    	      5

#define  Left(e)		     (Bounds(e)->left)
#define  Center(e)		     (Left(e) + Width(e)/2)
#define  Right(e)		     (Left(e) + Width(e))
#define  Top(e)			     (Bounds(e)->top)
#define  Middle(e)    		     (Top(e) + Height(e)/2)
#define  Bottom(e)		     (Top(e) + Height(e))
#define  Width(e)		     (Bounds(e)->width)
#define  Height(e)		     (Bounds(e)->height)

#define  LeftArea		     0
#define  TopArea		     1
#define  RightArea		     2
#define  BottomArea		     3

#define  AreaBounds(e,a)	    (&Area(e,a)->bound)
#define  AreaSize(e,a)		     (Area(e,a)->size)
#define  AreaFont(e,a)		     (Area(e,a)->font)
#define  AreaFontName(e,a)	     (Area(e,a)->font_name)
#define  AreaStringAnchor(e,a)	     (Area(e,a)->strings)
#define  AreaStringCount(e,a)	     (Area(e,a)->strings_count)
#define  AreaString(e,a,i)	     ((*Area(e,a)->strings)[i])
#define  AreaLeft(e,a)		     (Area(e,a)->bound.left)
#define  AreaCenter(e,a)	     (AreaLeft(e,a) + AreaWidth(e,a)/2)
#define  AreaRight(e,a)		     (AreaLeft(e,a) + AreaWidth(e,a))
#define  AreaTop(e,a)		     (Area(e,a)->bound.top)
#define  AreaMiddle(e,a)	     (AreaTop(e,a)  + AreaHeight(e,a)/2)
#define  AreaBottom(e,a)	     (AreaTop(e,a)  + AreaHeight(e,a))
#define  AreaWidth(e,a)		     (Area(e,a)->bound.width)
#define  AreaHeight(e,a)	     (Area(e,a)->bound.height)

#define  DefaultFont		      self->default_font
#define  DefaultFontName	      "AndySans10"
#define  IconFont		      self->icon_font
#define  IconFontName		      "apticn20"
#define  CursorFont		      self->cursor_font
#define  CursorFontName		      "aptcsr20"

#define  ShrinkIcon		      self->shrink_icon
#define  ShrinkTitle		      self->shrink_title
#define  ShrinkIconFont		      self->shrink_icon_font
#define  ShrinkTitleFont	      self->shrink_title_font
#define  Shrinking		      self->states.shrinking
#define  Shrunk			      self->states.shrunk

#define  ShrinkerBounds	    	    (&self->shrinker_bounds)
#define  ShrinkerLeft		      ShrinkerBounds->left
#define  ShrinkerCenter		     (ShrinkerLeft + ShrinkerWidth/2)
#define  ShrinkerRight		     (ShrinkerLeft + ShrinkerWidth)
#define  ShrinkerTop		      ShrinkerBounds->top
#define  ShrinkerMiddle		     (ShrinkerTop + ShrinkerHeight/2)
#define  ShrinkerBottom		     (ShrinkerTop + ShrinkerHeight)
#define  ShrinkerWidth	    	      ShrinkerBounds->width
#define  ShrinkerHeight	    	      ShrinkerBounds->height

#define  ShrinkIconBounds	    (&self->shrink_icon_bounds)
#define  ShrinkIconLeft		      ShrinkIconBounds->left
#define  ShrinkIconCenter	     (ShrinkIconLeft + ShrinkIconWidth/2)
#define  ShrinkIconRight	     (ShrinkIconLeft + ShrinkIconWidth)
#define  ShrinkIconTop		      ShrinkIconBounds->top
#define  ShrinkIconMiddle	     (ShrinkIconTop + ShrinkIconHeight/2)
#define  ShrinkIconBottom	     (ShrinkIconTop + ShrinkIconHeight)
#define  ShrinkIconWidth	      ShrinkIconBounds->width
#define  ShrinkIconHeight	      ShrinkIconBounds->height
#define  ShrinkTitleBounds	    (&self->shrink_title_bounds)
#define  ShrinkTitleLeft	      ShrinkTitleBounds->left
#define  ShrinkTitleCenter	     (ShrinkTitleLeft + ShrinkTitleWidth/2)
#define  ShrinkTitleRight	     (ShrinkTitleLeft + ShrinkTitleWidth)
#define  ShrinkTitleTop		      ShrinkTitleBounds->top
#define  ShrinkTitleMiddle	     (ShrinkTitleTop + ShrinkTitleHeight/2)
#define  ShrinkTitleBottom	     (ShrinkTitleTop + ShrinkTitleHeight)
#define  ShrinkTitleWidth	      ShrinkTitleBounds->width
#define  ShrinkTitleHeight	      ShrinkTitleBounds->height

#define  HelperBounds	    	    (&self->helper_bounds)
#define  HelperLeft		      HelperBounds->left
#define  HelperCenter		     (HelperLeft + HelperWidth/2)
#define  HelperRight		     (HelperLeft + HelperWidth)
#define  HelperTop		      HelperBounds->top
#define  HelperMiddle		     (HelperTop + HelperHeight/2)
#define  HelperBottom		     (HelperTop + HelperHeight)
#define  HelperWidth	    	     (HelperBounds->width)
#define  HelperHeight	    	     (HelperBounds->height)
#define  HelpDisplayed		      self->states.help_displayed
#define  HelpText		      self->help_text
#define  HelpTextView		      self->help_textview
#define  HelpString		      self->help_string
#define  HelpFileName		      self->help_file_name

#define  BypassUpdate		      self->bypass_update

#define  InputFocus		      self->states.inputfocus
#define  ControlSuppressed	      self->options.controls_suppressed
#define  BorderSuppressed	      self->options.border_suppressed
#define  EnclosuresSuppressed	      self->options.enclosures_suppressed
#define  GrayFill		      self->gray_fill
#define  WhiteFill		      self->white_fill

#define  PrintStream		     (self->print_stream)
#define  PrintFile		     (PrintStream->file)
#define  PrintProcessor		     (PrintStream->processor)
#define  PrintFormat		     (PrintStream->format)
#define  PrintLevel		     (PrintStream->level)
#define  PrintTopLevel		     (PrintStream->level==1)
#define  PrintPrefix		     (PrintStream->prefix)
#define  PrintPortrait		     (PrintStream->portrait)
#define  PrintLandScape		     (PrintStream->landscape)
#define  PrintResolution	     (PrintStream->pixels_per_inch)
#define  PrintResolutionFactor	     (PrintStream->resolution_factor)
#define  PRF(x)			     (PrintResolutionFactor*x)
#define  PrintUnitInchWidth	     (PrintStream->unit_inch_width)
#define  PrintUnitInchHeight	     (PrintStream->unit_inch_height)
#define  PrintPageInchWidth	     (PrintStream->page_inch_width)
#define  PrintPageInchHeight	     (PrintStream->page_inch_height)
#define  PrintPagePelWidth	     (PrintStream->page_pel_width)
#define  PrintPagePelHeight	     (PrintStream->page_pel_height)
#define  PrintPreserveAspect	     (PrintStream->preserve_aspect)
#define  PrintFillPage		     (PrintStream->fill_page)

#define  Troff			      (1<<0)
#define  PostScript		      (1<<1)

static Size_Enclosures();
static Print_Area();
static Help();
static Help_FullUpdate();
static Unhelp();
static Draw_String();
static Draw_Enclosures();

struct  aptv_print_stream
  {
  FILE				     *file;
  char				      processor;
  char				      format;
  short				      level;
  char				     *prefix;
  float				      pixels_per_inch, resolution_factor;
  float				      unit_inch_width, unit_inch_height;
  float				      page_inch_width, page_inch_height;
  float				      page_pel_width, page_pel_height;
  char				      portrait, landscape, preserve_aspect,
				      fill_page;
  };


boolean 
aptv__InitializeObject( classID, self)
  register struct classheader *classID;
  register struct aptv	      *self;
  {
  register long		       e, a;

  IN(aptv_InitializeObject);
  DEBUGst(RCSID,rcsidaptv);
  self->header.view.imPtr = NULL;
  Data = NULL;
  PrintStream = (struct aptv_print_stream *)
		calloc( 1, sizeof(struct aptv_print_stream) );
  HelpText = NULL;
  HelpTextView = NULL;
  HelpString = HelpFileName = NULL;
  BypassUpdate = HelpDisplayed = false;
  Cursor = cursor_Create( self );
  IconFont = aptv_BuildFont( self, IconFontName, NULL );
  CursorFont = aptv_BuildFont( self, CursorFontName, NULL );
  DefaultFont = aptv_BuildFont( self, DefaultFontName, NULL );
  ShrinkIcon[0] = 0;
  ShrinkIcon[1] = 0;
  ShrinkTitle = NULL;
  Initialized = false;
  InputFocus = false;
  Shrunk = false;
  ControlSuppressed = false;
  BorderSuppressed = false;
  EnclosuresSuppressed = false;
  OriginalWidth = OriginalHeight = DimensionWidth = DimensionHeight = 0;
  for ( e = 0; e < EnclosureCount; e++ )
    for ( a = 0; a < 4; a++ )
      {
      AreaSize(e,a) = 0;
      AreaFont(e,a) = DefaultFont;
      AreaFontName(e,a) = NULL;
      AreaStringAnchor(e,a) = NULL;
      AreaStringCount(e,a) = 0;
      }
  AreaSize(Border,LeftArea) = AreaSize(Border,TopArea) =
   AreaSize(Border,RightArea) = AreaSize(Border,BottomArea) = 3;
  AreaSize(Control,TopArea) = 20; /*===*/
  OUT(aptv_InitializeObject);
  return  TRUE;
  }

void 
aptv__FinalizeObject( classID, self )
  register struct classheader *classID;
  register struct aptv	      *self;
  {
  IN(aptv_FinalizeObject);
  if ( PrintStream )	free( PrintStream );
  if ( Cursor )		cursor_Destroy( Cursor );
  OUT(aptv_FinalizeObject);
  }

void
aptv__SetDataObject( self, data )
  register struct aptv	      *self;
  register struct apt	      *data;
  {
  IN(aptv_SetDataObject);
  super_SetDataObject( self, data );
  Data = data;
  apt_AddObserver( Data, self );
  OUT(aptv_SetDataObject);
  }

void
aptv__SetOptions( self, options )
  register struct aptv	      *self;
  register long		       options;
  {
  register long		       e, a;

  IN(aptv_SetOptions);
  if ( options & aptv_Iconified )   Shrunk = true;
  if ( options & aptv_SuppressControl )
    {
    ControlSuppressed = true;
    AreaSize(Control,TopArea) = 0; /*===*/
    }
  if ( options & aptv_SuppressBorder )
    {
    BorderSuppressed = true;
    AreaSize(Border,LeftArea) = AreaSize(Border,TopArea) =
	AreaSize(Border,RightArea) = AreaSize(Border,BottomArea) = 0;
    }
  if ( options & aptv_SuppressEnclosures )
    {
    EnclosuresSuppressed = true;
    for ( e = 0; e < EnclosureCount; e++ )
      for ( a = 0; a < 4; a++ )
       AreaSize(e,a) = 0;
    }
  OUT(aptv_SetOptions);
  }

void
aptv__SetDimensions( self, width, height )
  register struct aptv	      *self;
  register long		       width, height;
  {
  IN(aptv_SetDimensions);
  DimensionWidth  = OriginalWidth  = width;
  DimensionHeight = OriginalHeight = height;
  OUT(aptv_SetDimensions);
  }

void
aptv__SetHelpString( self, string )
  register struct aptv	      *self;
  register char		      *string;
  {
  HelpString = string;
  }

void
aptv__SetHelpFileName( self, file_name )
  register struct aptv	      *self;
  register char		      *file_name;
  {
  HelpFileName = file_name;
  }

struct fontdesc *
aptv__BuildFont( self, font_name, height )
  register struct aptv	      *self;
  register char		      *font_name;
  register short	      *height;
  {
  register struct fontdesc    *font = NULL;
  char			      family[257];
  long			      style, size;
  register struct FontSummary *summary;

  IN(aptv_BuildFont);
  DEBUGst(Font-name,font_name);
  fontdesc_ExplodeFontName( font_name, family, sizeof(family), &style, &size );
  DEBUGst(Family,family);
  font = fontdesc_Create( family, style, size );
  if ( height  &&  aptv_GetIM( self ) )
    {
    if ( summary = fontdesc_FontSummary( font, aptv_GetDrawable( self ) ) )
      *height = summary->maxHeight;
      else  *height = 0;
    }
  OUT(aptv_BuildFont);
  return  font;
  }

void
aptv__SetShrinkIcon( self, icon, icon_font_name, title, title_font_name )
  register struct aptv	      *self;
  register char		       icon;
  register char		      *icon_font_name;
  register char		      *title;
  register char		      *title_font_name;
  {
  short			       height = 0;

  IN(aptv_SetShrinkIcon);
  DEBUGct(Icon,icon);
  DEBUGst(Icon-font-name,icon_font_name);
  DEBUGst(Title,title);
  DEBUGst(Title-font-name,title_font_name);
  ShrinkIcon[0] = icon; ShrinkIcon[1] = 0;
  if ( icon_font_name  &&  *icon_font_name )
    ShrinkIconFont = aptv_BuildFont( self, icon_font_name, NULL );
  if ( title  &&  *title )
    {
    ShrinkTitle = (char *) malloc( strlen( title ) + 1 );
    strcpy( ShrinkTitle, title );
    if ( title_font_name  &&  *title_font_name )
      {
      ShrinkTitleFont = aptv_BuildFont( self, title_font_name, &height );
      height += 2;
      }
    ShrinkTitleHeight = height;
    }
  OUT(aptv_SetShrinkIcon);
  }

static struct aptv *
Parent_AptView( self )
  register struct aptv	      *self;
  {
  register struct aptv	      *parent = NULL,
			      *candidate =
				(struct aptv *)self->header.view.parent;

  IN(Parent_AptView);
  while ( candidate  &&  parent == NULL )
    {
    DEBUGst(Parent Name,class_GetTypeName( candidate ));
    if ( class_IsTypeByName( class_GetTypeName( candidate ), "aptv" ) )
      parent = candidate;
      else
      candidate = (struct aptv *) candidate->header.view.parent;
    }
  OUT(Parent_AptView);
  return  parent;
  }

void
aptv__ShrinkView( self, apt_view )
  register struct aptv	      *self;
  register struct aptv	      *apt_view;
  {
  IN(aptv_ShrinkView);
  /* NOP */
  OUT(aptv_ShrinkView);
  }

void
aptv__ExpandView( self, apt_view )
  register struct aptv	      *self;
  register struct aptv	      *apt_view;
  {
  IN(aptv_ExpandView);
  /* NOP */
  OUT(aptv_ExpandView);
  }

void
aptv__Shrink( self )
  register struct aptv	      *self;
  {
  register struct aptv	      *parent;

  IN(aptv_Shrink);
  if ( ! Shrunk )
    {
    DEBUG(Shrink);
    Shrinking = true;
/*===
    OriginalWidth = Width(Outer);
    OriginalHeight = Height(Outer);
===*/
    if ( parent = Parent_AptView( self ) )
      aptv_ShrinkView( parent, self );
      else
      aptv_WantNewSize( self, self );
    }
  OUT(aptv_Shrink);
  }

void
aptv__Expand( self )
  register struct aptv	      *self;
  {
  register struct aptv	      *parent;

  IN(aptv_Expand);
  if ( Shrunk )
    {
    DEBUG(Expand);
    Shrunk = false;
    if ( parent = Parent_AptView( self ) )
      aptv_ExpandView( parent, self );
      else
      aptv_WantNewSize( self, self );
    }
  OUT(aptv_Expand);
  }

void 
aptv__FullUpdate( self, type, left, top, width, height )
  register struct aptv		*self;
  register enum view_UpdateType	 type;
  register long			 left, top, width, height;
  {
  register long			 e;

  IN(aptv_FullUpdate);
  if ( Data  &&  (type == view_FullRedraw || type == view_LastPartialRedraw) )
    {
    Size_Enclosures( self );
    for ( e = 0; e < EnclosureCount; e++ )
      Left(e) = Top(e) = Width(e) = Height(e) = 0;
    aptv_GetLogicalBounds( self, Bounds(Outer) );
    ShrinkerLeft    = ShrinkerTop     = ShrinkerWidth    =
    ShrinkIconLeft  = ShrinkIconTop   = ShrinkIconWidth  =
    ShrinkTitleLeft = ShrinkTitleTop  = ShrinkTitleWidth =
    HelperLeft	    = HelperTop	      = HelperWidth	 = 
    ShrinkTitleHeight = 0;
    if ( ! Shrunk )
      {
      for ( e = 1; e < EnclosureCount; e++ )
	{
	Left(e) = Left(e-1) + AreaSize(e-1,LeftArea);
	Top(e)  = Top(e-1)  + AreaSize(e-1,TopArea);
	AreaLeft(e,LeftArea)     = AreaLeft(e,TopArea)  = AreaLeft(e,BottomArea) = Left(e);
	AreaTop(e,LeftArea)      = AreaTop(e,TopArea)   = 
	AreaTop(e,RightArea)     = Top(e);
	AreaWidth(e,LeftArea)    = AreaSize(e,LeftArea);
	AreaWidth(e,RightArea)   = AreaSize(e,RightArea);
	AreaHeight(e,TopArea)    = AreaSize(e,TopArea);
	AreaHeight(e,BottomArea) = AreaSize(e,BottomArea);
	}
      for ( e = 1; e < EnclosureCount; e++ )
	{
	Width(e)  = (((Right(e-1) - AreaSize(e-1,RightArea)) - Left(e))) -
		     (AreaSize(e-1,RightArea)  ? 1 : ((e==1) ? 1 : 0));
	Height(e) = (((Bottom(e-1) - AreaSize(e-1,BottomArea)) - Top(e))) -
		     (AreaSize(e-1,BottomArea) ? 1 : ((e==1) ? 1 : 0));
	AreaWidth(e,TopArea)   = AreaWidth(e,BottomArea) = Width(e);
	AreaHeight(e,LeftArea) = AreaHeight(e,RightArea) = Height(e);
	AreaTop(e,BottomArea)  = Bottom(e) - AreaSize(e,BottomArea);
	AreaLeft(e,RightArea)  = Right(e)  - AreaSize(e,RightArea);
	}
      if ( ! ControlSuppressed )
        {
        ShrinkerLeft    = Left(Control) + 1;
        ShrinkerTop     = Top(Control) + 1;
        ShrinkerWidth   = 20 /*===*/;
        ShrinkerHeight  = 20 /*===*/;
        HelperLeft	= Right(Control) - 22;
        HelperTop       = Top(Control) + 1;
        HelperWidth     = 20 /*===*/;
        HelperHeight    = 20 /*===*/;
        }
      }
      else /* Shrunk */
      {
      Width(Border)	= Width(Outer) - 1;
      Height(Border)	= Height(Outer) - 1;
      ShrinkIconLeft    =
      ShrinkTitleLeft	= Left(Border);
      ShrinkIconTop     = Top(Border);
      ShrinkIconWidth   = Width(Border);
      ShrinkIconHeight  = Height(Border);
      if ( ShrinkIconWidth > ShrinkIconHeight )
	ShrinkIconWidth  = ShrinkTitleWidth = ShrinkIconHeight;
	else
	ShrinkIconHeight = ShrinkTitleWidth = ShrinkIconWidth;
      ShrinkTitleTop	= Bottom(Border) - ShrinkTitleHeight;
      }
    GrayFill  = aptv_GrayPattern( self, 1, 2 );
    WhiteFill = aptv_WhitePattern( self );
    Draw_Enclosures( self );
    if ( HelpDisplayed )  Help_FullUpdate( self );
    aptv_UseNormalCursor( self );
    }
  OUT(aptv_FullUpdate);
  }

boolean
aptv__Within( self, x, y, bounds )
  register struct aptv	     *self;
  register long		      x, y;
  register struct rectangle  *bounds;
  {
  register char		      status = 0;

  IN(aptv_Within);
  if ( x >= bounds->left  &&  x <= (bounds->left + bounds->width)  &&
       y >= bounds->top   &&  y <= (bounds->top  + bounds->height ) )
    status = 1;;
  OUT(aptv_Within);
  return  status;
  }

struct view *
aptv__Hit( self, action, x, y, clicks )
  register struct aptv		 *self;
  register enum view_MouseAction  action;
  register long			  x, y, clicks;
  {
  register struct view		 *hit = NULL;

  IN(aptv_Hit );
  if ( Shrunk  &&  action == view_LeftDown )
    {
    DEBUG(De-Shrinking);
    aptv_Expand( self );
    }
  else
  if ( HelpDisplayed  &&  aptv_Within( self, x, y, Bounds(Title ) ) )
    {
    DEBUG(Helping);
    hit = textview_Hit( HelpTextView, action,
			textview_EnclosedXToLocalX( HelpTextView, x ),
			textview_EnclosedYToLocalY( HelpTextView, y ), clicks );
    }
  else
  if ( ! aptv_Within( self, x, y, Bounds(Body) ) )
    switch ( action )
      {
      case  view_LeftDown:
	if ( ! ControlSuppressed  && aptv_Within( self, x, y, Bounds(Control) ) )
	  {
	  if ( aptv_Within( self, x, y, ShrinkerBounds ) )
	    {
	    DEBUG(Shrinking);
	    aptv_Shrink( self );
	    }
	  if ( aptv_Within( self, x, y, HelperBounds ) )
	    {
	    DEBUG(Helping);
	    if ( HelpDisplayed )
	      Unhelp( self );
	      else
	      Help( self );
	    }
	  }
        break;
      case  view_LeftMovement:

        break;
      case  view_LeftUp:

        break;
      }
  OUT(aptv_Hit );
  return  hit;
  }

void
aptv__ClearBody( self )
  register struct aptv		    *self;
  {
  IN(aptv_ClearBody);
  aptv_SetTransferMode( self, graphic_WHITE );
  aptv_FillRect( self, Bounds(Body), graphic_WHITE );
  aptv_SetTransferMode( self, graphic_BLACK );
  OUT(aptv_ClearBody);
  }

void
aptv__PrintContinue( self )
  register struct aptv	     *self;
  {
  IN(aptv_PrintContinue);
  /*  NOP  */
  OUT(aptv_PrintContinue);
  }

void
aptv__PrintObject( self, file, processor, format, level, printer )
  register struct aptv	     *self;
  register FILE		     *file;
  register char		     *processor;
  register char		     *format;
  register boolean	      level;
  register void		    (*printer)();
  {
  register long		      e, a;

  IN(aptv_PrintObject);
  aptv_OpenPrintStream( self, file, processor, format, level );
  aptv_SetPrintLineWidth( self, 2 );
  if ( !BorderSuppressed )
    aptv_PrintFilledRoundBox( self, Left(Border),  Top(Border),
			      Width(Border), Height(Border), 0, 1 );
  aptv_SetPrintLineWidth( self, 1 );
  for ( e = 0; e < EnclosureCount; e++ )
    for ( a = 0; a < 4; a++ )
      Print_Area( self, e, a );
  aptv_PreservePrintState( self );
  if ( printer )
    (*printer)( self );
  aptv_RestorePrintState( self );
  aptv_ClosePrintStream( self );
  OUT(aptv_PrintObject);
  }

static
Print_Area( self, enclosure, area )
  register struct aptv	     *self;
  register long		      enclosure, area;
  {
  register long		      i, width, center;

  if ( AreaSize(enclosure,area)  &&  AreaStringAnchor(enclosure,area) )
    {
    if ( AreaStringAnchor(enclosure,area)  &&  AreaFont(enclosure,area) )
      aptv_SetPrintFont( self, AreaFontName(enclosure,area) );
    width = AreaWidth(enclosure,area) / AreaStringCount(enclosure,area);
    center = AreaLeft(enclosure,area) + width / 2;
    for ( i = 0; i < AreaStringCount(enclosure,area); i++ )
      {
      aptv_PrintString( self, center,
				     AreaMiddle(enclosure,area),
				     AreaString(enclosure,area,i), 0 );
      center += width;
      }
    }
  }

static char	*PostScript_prelude[] =
{
"% Begin AptView PostScript Prelude  Version 0.0",
"gsave % Save Environment Around AptView Drawing",
"1 -1 scale",
"1 setlinewidth",
"/andy { /Times-Roman findfont exch scalefont} def",
"/Andy { /Times-Roman findfont exch scalefont} def",
"/andysans { /Helvetica findfont exch scalefont} def",
"/AndySans { /Helvetica findfont exch scalefont} def",
"/Andysans { /Helvetica findfont exch scalefont} def",
"/andyb { /Times-Bold findfont exch scalefont} def",
"/Andyb { /Times-Bold findfont exch scalefont} def",
"/andysansb { /Helvetica-Bold findfont exch scalefont} def",
"/AndySansb { /Helvetica-Bold findfont exch scalefont} def",
"/Andysansb { /Helvetica-Bold findfont exch scalefont} def",
"/andyi { /Times-Italic findfont exch scalefont} def",
"/Andyi { /Times-Italic findfont exch scalefont} def",
"/andysansi { /Helvetica-Oblique findfont exch scalefont} def",
"/AndySansi { /Helvetica-Oblique findfont exch scalefont} def",
"/Andysansi { /Helvetica-Oblique findfont exch scalefont} def",
"/andybi { /Times-BoldItalic findfont exch scalefont} def",
"/Andybi { /Times-BoldItalic findfont exch scalefont} def",
"/andysansbi { /Helvetica-BoldOblique findfont exch scalefont} def",
"/AndySansbi { /Helvetica-BoldOblique findfont exch scalefont} def",
"/Andysansbi { /Helvetica-BoldOblique findfont exch scalefont} def",
"/centerstring {",
"	gsave",
"	1 -1 scale",
"	dup stringwidth",
"	pop -5 exch 2 div neg exch",
"	rmoveto",
"	show",
"	grestore",
"} def",
"/box {",
"	gsave",
"	left top	moveto",
"	right top	lineto	% Across",
"	right bottom	lineto	% Down",
"	left  bottom	lineto	% Back",
"	left  top	lineto	% Up",
"	closepath  stroke",
"	grestore",
"} def",
"/roundbox {",
"	gsave",
"	/radius 10 def",
"	left radius add   top moveto",
"	right top    right bottom radius arcto  4 {pop} repeat	% Across",
"	right bottom left  bottom radius arcto  4 {pop} repeat	% Down",
"	left  bottom left  top    radius arcto  4 {pop} repeat	% Back",
"	left  top    right top    radius arcto  4 {pop} repeat	% Up",
"	stroke",
"	grestore",
"} def",
"/fillroundbox {",
"	gsave",
"	/radius 10 def",
"	left radius add   top moveto",
"	right top    right bottom radius arcto  4 {pop} repeat	% Across",
"	right bottom left  bottom radius arcto  4 {pop} repeat	% Down",
"	left  bottom left  top    radius arcto  4 {pop} repeat	% Back",
"	left  top    right top    radius arcto  4 {pop} repeat	% Up",
"	gsave shade setgray fill grestore stroke",
"	grestore",
"} def",
"% End AptView PostScript Prelude  Version 0.0",
"",
"12 andysans setfont",
"% Begin AptView Drawing",
NULL
};

boolean
aptv__OpenPrintStream( self, file, processor, format, level )
  register struct aptv	     *self;
  register FILE		     *file;
  register char		     *processor;
  register char		     *format;
  register long		      level;
  {
  register char		    **ptr = PostScript_prelude;

  if ( PrintStream  ||  (PrintStream = (struct aptv_print_stream *)
		calloc( 1, sizeof(struct aptv_print_stream) )) )
    {
    PrintFile = file;
    if ( apts_CompareStrings( processor, "troff" ) == 0 )
      PrintProcessor = Troff;
    if ( apts_CompareStrings( processor, "PostScript" ) == 0 )
      PrintProcessor = PostScript;
    if ( apts_CompareStrings( format, "troff" ) == 0 )
      PrintFormat = Troff;
    if ( apts_CompareStrings( format, "PostScript" ) == 0 )
      PrintFormat = PostScript;
    PrintResolution = aptv_GetHorizontalResolution( self );
    DEBUGdt(H-Res,PrintResolution);
    PrintResolutionFactor = 72.0 / PrintResolution;
    DEBUGgt(H-ResFac,PrintResolutionFactor);
    PrintPageInchWidth = 8.5;
    PrintPageInchHeight = 11.0;
    PrintPagePelWidth  = PrintPageInchWidth  * 72;
    PrintPagePelHeight = PrintPageInchHeight * 72;
    PrintLevel = level;
    }
  if ( PrintTopLevel  &&  PrintProcessor == Troff )
    texttroff_BeginDoc( PrintFile );
  if ( PrintProcessor == Troff )
    {
    texttroff_BeginPS( PrintFile, Width(Outer), Height(Outer) );
    PrintPrefix = "\\!";
    }
  if ( PrintProcessor == PostScript )
    {
    PrintPrefix = "";
    }
  if ( PrintTopLevel  ||  PrintProcessor == Troff )
    fprintf( PrintFile, "%s%%!PS-Adobe-2.0 EPSF-1.2\n", PrintPrefix );
  while ( *ptr )  /* Emit PostScript Prolog */
    fprintf( PrintFile, "%s%s\n", PrintPrefix, *ptr++ );
  if ( PrintLandScape )
    {
    fprintf( PrintFile, "%s-90 rotate  -17 0 translate  %% LandScape Orientation\n",
	     PrintPrefix );
    if ( PrintTopLevel )
      fprintf( PrintFile, "%s%g %g translate  %% Centering\n",
	     PrintPrefix, (PrintPagePelHeight - PRF(Width(Outer)))/2,
			  (PrintPagePelWidth  - PRF(Height(Outer)))/2 );
    }
    else
    {
    fprintf( PrintFile, "%s%d %d translate  %% Portrait Orientation\n",
	     PrintPrefix, 0, -Height(Outer) );
    if ( PrintTopLevel )
      fprintf( PrintFile, "%s%g %g translate  %% Centering\n",
	     PrintPrefix, (PrintPagePelWidth  - PRF(Width(Outer)))/2,
			  (PrintPagePelHeight - PRF(Height(Outer)))/2 );
    }
  fprintf( PrintFile, "%s%g %g translate\n",
	   PrintPrefix, PRF(Left(Outer))+10,PRF(Top(Outer))+10 );
  fprintf( PrintFile, "%s /width %g def /height %g def\n",
	   PrintPrefix, PRF(Width(Outer)), PRF(Height(Outer)) );
  fprintf( PrintFile, "%s newpath 0 0 moveto\n", PrintPrefix );
  fprintf( PrintFile, "%s 0 height lineto width height lineto\n", PrintPrefix );
  fprintf( PrintFile, "%s width 0 lineto 0 0 lineto  newpath\n", PrintPrefix );
  return  TRUE;
  }

void
aptv__ClosePrintStream( self )
  register struct aptv	     *self;
  {
  fprintf( PrintFile, "%sgrestore  %% Restore Environment Around AptView Drawing\n", PrintPrefix );
  if ( PrintProcessor == PostScript )
    {
    if ( PrintTopLevel )
      fprintf( PrintFile, "%sshowpage  %% End AptView Drawing\n", PrintPrefix );
    }
  if ( PrintProcessor == Troff )
    {
    texttroff_EndPS( PrintFile,  Width(Outer), Height(Outer) );
    if ( PrintTopLevel )
      texttroff_EndDoc( PrintFile );
    }
  }

void
aptv__PreservePrintState( self )
  register struct aptv	     *self;
  {
  fprintf( PrintFile, "%sgsave  %% Preserve Nested Print State\n", PrintPrefix );
  }

void
aptv__RestorePrintState( self )
  register struct aptv	     *self;
  {
  fprintf( PrintFile, "%sgrestore  %% Restore Nested Print State\n", PrintPrefix );
  }

void
aptv__SetPrintStream( self, print_stream )
  register struct aptv		    *self;
  register struct aptv_print_stream *print_stream;
  {
  IN(aptv_SetPrintStream);
  PrintStream = print_stream;
  OUT(aptv_SetPrintStream);
  }

void
aptv__SetPrintOrigin( self, left, top )
  register struct aptv	     *self;
  register long		      left, top;
  {
  fprintf( PrintFile, "%s%g %g translate\n", PrintPrefix, PRF(left), PRF(top) );
  }

void
aptv__SetPrintResolution( self, pixels_per_inch )
  register struct aptv	     *self;
  register float	      pixels_per_inch;
  {
  PrintResolution = pixels_per_inch;
  }

void
aptv__SetPrintUnitDimensions( self, inch_width, inch_height )
  register struct aptv	     *self;
  register float	      inch_width, inch_height;
  {
  PrintUnitInchWidth = inch_width;
  PrintUnitInchHeight = inch_height;
  }

void
aptv__SetPrintPageDimensions( self, inch_width, inch_height )
  register struct aptv	     *self;
  register float	      inch_width, inch_height;
  {
  PrintPageInchWidth = inch_width;
  PrintPageInchHeight = inch_height;
  }

void
aptv__SetPrintOptions( self, options )
  register struct aptv	     *self;
  register long		      options;
  {
  if ( options & aptv_PrintPortrait )
    PrintPortrait = true;
  if ( options & aptv_PrintLandScape )
    PrintLandScape = true;
  if ( options & aptv_PrintPreserveAspect )
    PrintPreserveAspect = true;
  if ( options & aptv_PrintFillPage )
    PrintFillPage = true;
  }

void
aptv__SetPrintPath( self, path )
  register struct aptv	     *self;
  register struct aptv_path  *path;
  {
  register long		      i;

  fprintf( PrintFile, "%s newpath %g %g moveto\n",
	   PrintPrefix, PRF(path->points[0].x), PRF(path->points[0].y) );
  for ( i = 1; i < path->count; i++ )
    fprintf( PrintFile, "%s%g %g lineto\n",
	     PrintPrefix, PRF(path->points[i].x), PRF(path->points[i].y) );
  fprintf( PrintFile, "%s closepath\n", PrintPrefix );
  }

void
aptv__PrintPath( self, path )
  register struct aptv	     *self;
  register struct aptv_path  *path;
  {
  if ( path )
    aptv_SetPrintPath( self, path );
  fprintf( PrintFile, "%s stroke\n", PrintPrefix );
  }

void
aptv__PrintPathFilled( self, path )
  register struct aptv	     *self;
  register struct aptv_path  *path;
  {
  if ( path )
    aptv_SetPrintPath( self, path );
  fprintf( PrintFile, "%s fill\n", PrintPrefix );
  }

void
aptv__PrintBox( self, left, top, width, height, mode )
  register struct aptv	     *self;
  register long		      left, top, width, height;
  register int		      mode;
  {
  fprintf( PrintFile,"%s/left %g def /top %g def /right %g def /bottom %g def box\n",
	   PrintPrefix, PRF(left), PRF(top),
			PRF(left) + (PRF(width) - 1), PRF(top) + (PRF(height) - 1) );
  }

void
aptv__PrintRoundBox( self, left, top, width, height, mode )
  register struct aptv	     *self;
  register long		      left, top, width, height;
  register long		      mode;
  {
  fprintf( PrintFile,"%s/left %g def /top %g def /right %g def /bottom %g def roundbox\n",
	   PrintPrefix, PRF(left), PRF(top),
			PRF(left) + (PRF(width) - 1), PRF(top) + (PRF(height) - 1) );
  }

void
aptv__PrintFilledRoundBox( self, left, top, width, height, mode, shade )
  register struct aptv	     *self;
  register long		      left, top, width, height;
  register long		      mode, shade;
  {
  fprintf( PrintFile,"%s/left %g def /top %g def /right %g def /bottom\
	    %g def /shade %d def fillroundbox\n",
	    PrintPrefix, PRF(left), PRF(top),
			 PRF(left) + (PRF(width) - 1), PRF(top) + (PRF(height) - 1), shade );
  }

void
aptv__PrintLine( self, x1, y1, x2, y2 )
  register struct aptv	     *self;
  register long		      x1, y1, x2, y2;
  {
  fprintf( PrintFile,"%s%g %g moveto %g %g lineto stroke\n",
	   PrintPrefix, PRF(x1), PRF(y1), PRF(x2), PRF(y2) );
  }

void
aptv__PrintMoveTo( self, x, y )
  register struct aptv	     *self;
  register long		      x, y;
  {
  fprintf( PrintFile,"%s%g %g moveto\n", PrintPrefix, PRF(x), PRF(y) );
  }

void
aptv__PrintLineTo( self, x, y )
  register struct aptv	     *self;
  register long		      x, y;
  {
  fprintf( PrintFile,"%s%g %g lineto stroke\n", PrintPrefix, PRF(x), PRF(y) );
  }

void
aptv__PrintCircle( self, x1, y1, r )
  register struct aptv	     *self;
  register long		      x1, y1, r;
  {
  fprintf( PrintFile,"%snewpath %g %g %g 0 360 arc closepath stroke\n",
	    PrintPrefix, PRF(x1), PRF(y1), PRF(r) );
  }

void
aptv__PrintSlice( self, x, y, r, start, end, shade_n, shade_d, mode )
  register struct aptv	     *self;
  register long		      x, y, r, shade_n, shade_d;
  register float	      start, end;
  register long		      mode;
  {
  fprintf( PrintFile,"%sgsave newpath %g %g moveto %g %g %g %.3f %.3f arcn closepath 1.415 setmiterlimit\n",
	    PrintPrefix, PRF(x), PRF(y), PRF(x), PRF(y), PRF(r), 360.0 - start, 360.0 - end );
  fprintf( PrintFile,"%sgsave %.3f setgray fill grestore stroke grestore\n",
	    PrintPrefix, 1.0 - ((shade_n * 1.0)/(shade_d * 1.0)) );
  }

void
aptv__SetPrintGrayLevel( self, level )
  register struct aptv	     *self;
  register float	      level;
  {
  fprintf( PrintFile, "%s %f setgray\n", PrintPrefix, level );
  }

void
aptv__SetPrintLineWidth( self, width )
  register struct aptv	     *self;
  register long		      width;
  {
  fprintf( PrintFile, "%s %g setlinewidth\n", PrintPrefix, PRF(width) );
  }

void
aptv__SetPrintFont( self, font_name )
  register struct aptv	     *self;
  register char		     *font_name;
  {
  char			      family[257], style_name[3];
  long			      style, size;

  IN(aptv_SetPrintFont);
  if ( font_name  &&  *font_name )
    {
    fontdesc_ExplodeFontName( font_name, family, sizeof(family), &style, &size );
    *style_name = 0;
    if ( style & fontdesc_Bold )    strcat( style_name, "b" );
    if ( style & fontdesc_Italic )  strcat( style_name, "i" );
    fprintf( PrintFile, "%s %d %s%s  setfont\n", PrintPrefix, size, family, style_name );
    }
  OUT(aptv_SetPrintFont);
  }

void
aptv__ResetPrintFont( self )
  register struct aptv	     *self;
  {
  IN(aptv_ResetPrintFont);
/*===*/
  OUT(aptv_ResetPrintFont);
  }

void
aptv__PrintString( self, x, y, string, placement )
  register struct aptv	     *self;
  register long		      x,y;
  register char		     *string;
  register long		      placement;
  {
  register char		     *place;
/*===
static char *places[] =
    {"aptv_LeftTop","aptv_LeftMiddle","apt_LeftBottom",
===*/
place = "centerstring";
  fprintf( PrintFile, "%s%g %g moveto ( %s ) %s\n",
			PrintPrefix, PRF(x), PRF(y), string, place );
  }

enum view_DSattributes
aptv__DesiredSize( self, given_width, given_height,
		      pass, desired_width, desired_height )
  register struct aptv	     *self;
  register long		      given_width, given_height;
  register enum view_DSpass   pass;
  register long		     *desired_width, *desired_height;
  {
  register enum view_DSattributes result = view_WidthFlexible |
					       view_HeightFlexible;

  IN(aptv_DesiredSize);
  if ( Shrinking  ||  Shrunk )
    {
    DEBUG(Shink);
    Shrinking = false;
    Shrunk = true;
    *desired_width = 50;
    *desired_height = 50;
    }
    else
    {
    if ( OriginalWidth  &&  OriginalHeight )
      {
      DEBUG(Original);
      *desired_width  = OriginalWidth;
      *desired_height = OriginalHeight;
      }
      else
      {
      if ( DimensionWidth  &&  DimensionHeight )
	{
	DEBUG(Dimensioned);
        *desired_width   = DimensionWidth;
        *desired_height  = DimensionHeight;
	}
	else
	{
	DEBUG(Default);
        *desired_width  = 300;
        *desired_height = 300;
	}
      }
    }
  OUT(aptv_DesiredSize);
  return  result;
  }

static
Size_Enclosures( self )
  register struct aptv	     *self;
  {
  register long		      i;
  long			      w, h;
  register char		     *font_name;

  IN(Size_Enclosures);
  if ( Data  &&  ! EnclosuresSuppressed )
    {
    DEBUG(Data Exists);
    Initialized = true;
    for ( i = 0; i < 4; i++ )
      {
      AreaSize(Title,i) = 0;
      if ( apt_AreaTitle( Data, i ) )
	{
	DEBUGst(Title,apt_AreaTitle( Data, i ));
	AreaStringAnchor(Title,i) = apt_AreaTitlesAnchor( Data, i );
	AreaStringCount(Title,i) = apt_AreaTitlesCount( Data, i );
	if ( apt_AreaTitleFontName( Data, i ) )
	  {
	  DEBUGst(Title Font,apt_AreaTitleFontName( Data, i ));
	  font_name = AreaFontName(Title,i) = apt_AreaTitleFontName( Data, i );
	  }
	  else  font_name = DefaultFontName;
        AreaFont(Title,i) = aptv_BuildFont( self, font_name, &AreaSize(Title,i) );
	fontdesc_StringSize( AreaFont(Title,i),
				aptv_GetDrawable( self ), AreaString(Title,i,0), &w, &h );
	if ( i == LeftArea  ||  i == RightArea )
	  AreaSize(Title,i) = w;
	AreaSize(Title,i) += 2;
	}
      AreaSize(Legend,i) = 0;
      if ( apt_AreaLegend( Data, i ) )
	{
	DEBUGst(Legend,apt_AreaLegend( Data, i ));
	AreaStringAnchor(Legend,i) = apt_AreaLegendsAnchor( Data, i );
	AreaStringCount(Legend,i) = apt_AreaLegendsCount( Data, i );
	if ( apt_AreaLegendFontName( Data, i ) )
	  {
	  DEBUGst(Legend Font,apt_AreaLegendFontName( Data, i ));
	  font_name = AreaFontName(Legend,i) = apt_AreaLegendFontName( Data, i );
	  }
	  else  font_name = DefaultFontName;
        AreaFont(Legend,i) = aptv_BuildFont( self, font_name, &AreaSize(Legend,i) );
	fontdesc_StringSize( AreaFont(Legend,i),
				aptv_GetDrawable( self ), AreaString(Legend,i,0), &w, &h );
	if ( i == LeftArea  ||  i == RightArea )
	  AreaSize(Legend,i) = w;
	AreaSize(Legend,i) += 2;
	}
      }
    }
  OUT(Size_Enclosures);
  }

static
Help( self )
  register struct aptv	     *self;
  {
  static char		     *notice = "Sorry, No help available for this Object.";
  FILE			     *file;
  struct attributes	      attrs;
  long			      id;

  HelpDisplayed = true;
  BypassUpdate = true;
  aptv_UseWaitCursor( self );
  if ( ! HelpText )
    {
    HelpText = text_New();
    attrs.next = 0;
    attrs.key = "readonly";
    attrs.value.integer = 0;
    text_Clear( HelpText );
    if ( HelpString )
      text_InsertCharacters( HelpText, 0, HelpString, strlen( HelpString ) );
    else
    if ( HelpFileName  &&  (file = fopen( HelpFileName, "r" )) )
      {
      filetype_Lookup( file, (char *) 0, &id, 0);
      text_Read( HelpText, file, id );
      fclose( file );
      }
    else  text_InsertCharacters( HelpText, 0, notice, strlen(notice) );
    attrs.value.integer = 1;
    text_SetAttributes( HelpText, &attrs );
    HelpTextView = textview_New();
    textview_SetDataObject( HelpTextView, HelpText );
    HelpTextView = (struct textview *) textview_GetApplicationLayer( HelpTextView );
    }
  Help_FullUpdate( self );
  aptv_UseNormalCursor( self );
  }

static
Help_FullUpdate( self )
  register struct aptv	     *self;
  {
  aptv_ClearClippingRect( self );
  aptv_SetTransferMode( self, graphic_WHITE );
  aptv_EraseRect( self, Bounds(Title) );
  aptv_SetTransferMode( self, graphic_BLACK );
  aptv_DrawRect( self, Bounds(Title) );
  textview_LinkTree( HelpTextView, self );
  textview_InsertViewSize( HelpTextView, self,
		       Left(Title)+1, Top(Title)+1, Width(Title)-3, Height(Title)-3 );
  textview_FullUpdate( HelpTextView, view_FullRedraw,
		       Left(Title)+1, Top(Title)+1, Width(Title)-3, Height(Title)-3 );
  textview_WantInputFocus( HelpTextView, HelpTextView );
  }

static
Unhelp( self )
  register struct aptv	     *self;
  {
  HelpDisplayed = false;
  BypassUpdate = false;
  aptv_UseWaitCursor( self );
  aptv_WantInputFocus( self, self );
  im_ForceUpdate();
  aptv_ClearClippingRect( self );
  textview_FullUpdate( HelpTextView, view_Remove, 0,0,0,0 );
  aptv_SetTransferMode( self, graphic_WHITE );
  aptv_EraseRectSize( self, Left(Title),    Top(Title),
			    Width(Title)+1, Height(Title)+1 );
  aptv_FullUpdate( self, view_FullRedraw,
	    Left(Outer), Top(Outer), Width(Outer), Height(Outer) );
  aptv_UseNormalCursor( self );
  }

void
aptv__UseNormalCursor( self )
  register struct aptv	     *self;
  {
  if ( Cursor  &&  aptv_GetIM( self ) )
    {
    cursor_SetStandard( Cursor, Cursor_Arrow );
    im_SetWindowCursor( aptv_GetIM( self ), NULL );
    }
  }

void
aptv__UseWaitCursor( self )
  register struct aptv	     *self;
  {
  if ( Cursor  &&  aptv_GetIM( self ) )
    {
    cursor_SetStandard( Cursor, Cursor_Wait );
    im_SetWindowCursor( aptv_GetIM( self ), Cursor );
    im_ForceUpdate();
    }
  }

void
aptv__UseInvisibleCursor( self )
  register struct aptv	     *self;
  {
  if ( Cursor  &&  aptv_GetIM( self ) )
    {
    cursor_SetGlyph( Cursor, CursorFont, '@' );
    im_SetWindowCursor( aptv_GetIM( self ), Cursor );
    im_ForceUpdate();
    }
  }

void
aptv__DrawBoundedString( self, string, font, bounds, x, y, mode )
  register struct aptv	     *self;
  register char		     *string;
  register struct fontdesc   *font;
  register struct rectangle  *bounds;
  register long		      x, y, mode;
  {
  IN(aptv_DrawBoundedString);
  if ( string  &&  *string )
    {
    aptv_SetTransferMode( self, graphic_BLACK );
    Draw_String( self, string, font, bounds, x, y, mode );
    }
  OUT(aptv_DrawBoundedString);
  }

void
aptv__ClearBoundedString( self, string, font, bounds, x, y, mode )
  register struct aptv	     *self;
  register char		     *string;
  register struct fontdesc   *font;
  register struct rectangle  *bounds;
  register long		      x, y, mode;
  {
  IN(aptv_ClearBoundedString);
  if ( string  &&  *string )
    {
    aptv_SetTransferMode( self, graphic_WHITE );
    Draw_String( self, string, font, bounds, x, y, mode );
    }
  OUT(aptv_ClearBoundedString);
  }

static
Draw_String( self, string, font, bounds, x, y, mode )
  register struct aptv	     *self;
  register char		     *string;
  register struct fontdesc   *font;
  register struct rectangle  *bounds;
  register long		      x, y, mode;
  {
  struct rectangle	      bound_interior;

  IN(Draw_String);
  bound_interior.left = bounds->left + 1;
  bound_interior.top = bounds->top + 1;
  bound_interior.width = bounds->width - 2;
  bound_interior.height = bounds->height - 2;
  aptv_SetClippingRect( self, &bound_interior );
  if ( font )
    aptv_SetFont( self, font );
  aptv_MoveTo( self, x, y );
  aptv_DrawString( self, string, mode );
  aptv_SetClippingRect( self, Bounds(Body) );
  OUT(Draw_String);
  }

static
Draw_Enclosures( self )
  register struct aptv	     *self;
  {
  register long		      e, a, i, width, center, alignment;
  register char		     *string;
  register struct fontdesc   *font;
  register struct rectangle  *bounds;

  IN(Draw_Enclosures);
  aptv_SetTransferMode( self, graphic_BLACK );
  aptv_ClearClippingRect( self );
  if ( Shrunk )
    {
    aptv_DrawOval( self, ShrinkIconBounds );
    if ( ShrinkIcon )
      aptv_DrawBoundedString( self, ShrinkIcon, ShrinkIconFont, ShrinkIconBounds,
				    ShrinkIconCenter, ShrinkIconMiddle, NULL  );
    if ( ShrinkTitle )
      aptv_DrawBoundedString( self, ShrinkTitle, ShrinkTitleFont, ShrinkTitleBounds,
				    ShrinkTitleCenter, ShrinkTitleMiddle, Balanced  );
    }
    else
    {
    if ( ! BorderSuppressed )
      aptv_DrawRRectSize( self, Left(Border),  Top(Border),
			           Width(Border), Height(Border), 10, 10 );
    if ( ! ControlSuppressed )
      {
      aptv_DrawBoundedString( self, "G", IconFont, ShrinkerBounds,
			      ShrinkerCenter, ShrinkerMiddle, NULL  );
      aptv_DrawBoundedString( self, "B", IconFont, HelperBounds,
			      HelperCenter, HelperMiddle, NULL  );
      }
    if ( ! EnclosuresSuppressed )
      {
      for ( e = 0; e < EnclosureCount; e++ )
      for ( a = 0; a < 4; a++ )
        if ( AreaSize(e,a)  &&  AreaStringCount(e,a) )
	  {
	  width = AreaWidth(e,a) / AreaStringCount(e,a);
	  center = AreaLeft(e,a) +  width / 2;
	  for ( i = 0; i < AreaStringCount(e,a); i++ )
	    {
	    string = AreaString(e,a,i);
	    font = AreaFont(e,a);
	    bounds = AreaBounds(e,a);
	    alignment = AreaMiddle(e,a);
	    aptv_DrawBoundedString( self, string, font, bounds,
				   center, alignment, Balanced );
	    center += width;
	    }
	  }
      }
    }
  aptv_SetClippingRect( self, Bounds(Body) );
  OUT(Draw_Enclosures);
  }

/*===  HANDLES
    aptv_MoveTo( self, Left(Border) + 9, Top(Border) );
    aptv_DrawLineTo( self, Right(Border) - 9, Top(Border) );
    aptv_MoveTo( self, Right(Border), Top(Border) + 9 );
    aptv_DrawLineTo( self, Right(Border), Bottom(Border) - 9 );
    aptv_MoveTo( self, Right(Border) - 9, Bottom(Border) );
    aptv_DrawLineTo( self, Left(Border) + 9, Bottom(Border) );
    aptv_MoveTo( self, Left(Border), Bottom(Border) - 9 );
    aptv_DrawLineTo( self, Left(Border), Top(Border) + 9 );
    aptv_SetFont( self, IconFont );
    aptv_MoveTo( self, Left(Border), Top(Border) );
    aptv_DrawString( self, "I", NULL );
    aptv_MoveTo( self, Right(Border), Top(Border) );
    aptv_DrawString( self, "K", NULL );
    aptv_MoveTo( self, Right(Border), Bottom(Border) );
    aptv_DrawString( self, "J", NULL );
    aptv_MoveTo( self, Left(Border), Bottom(Border) );
    aptv_DrawString( self, "L", NULL );
===*/

long
aptv__Query( self, query, default_response, response )
  register struct aptv	      *self;
  register char		      *query, *default_response, **response;
  {
  register long		      status = ok;
  static char		      buffer[512];

  IN(aptv_Query);
  *buffer = 0;
  *response = buffer;
  if ( (message_AskForString( self, 0, query, default_response,
	 buffer, sizeof buffer ) == -1)  ||  *buffer == 0 )
    {
    aptv_Announce( self, "Cancelled." );
    status = failure;
    }
  DEBUGst(Buffer,buffer);
  OUT(aptv_Query);
  return  status;
  }

long
aptv__QueryFileName( self, query, response )
  register struct aptv	     *self;
  register char		     *query;
  register char		    **response;
  {
  register enum message_CompletionCode  result;
  static char				path[257];
  static char				buffer[513];
  static char				msg[513];
  register long				status = ok;
  char					*tmp = NULL;

  IN(aptv_QueryFileName);
  im_GetDirectory(path);
  *buffer = 0;
  *response = buffer;
  result = (enum message_CompletionCode) completion_GetFilename( self, 
			  query,			/* prompt */
			  path,				/* initial string */
			  buffer,			/* working area */
			  sizeof buffer - 1,		/* size of working area */
			  0,				/* want file, not directory */
			  0 );				/* need not be existing file */
  DEBUGdt(Result,result);
  DEBUGst(File-name,buffer);
  if((result != message_Complete && result != message_CompleteValid) || *buffer==0) {
      status = failure;
      aptv_Announce(self,"Cancelled.");
  }
  else {
      if(buffer[strlen(buffer)-1] == '/')
	  buffer[strlen(buffer)-1] = 0;
      if(*(tmp = buffer + strlen(buffer) - 1) == '.') {
	  if((*(tmp-1) == '.') && (*(tmp-2) == '/')) {
	      *(tmp-2) = '\0';
	      if(tmp = (char*)rindex(buffer,'/')) 
		  *tmp = '\0';
	  }
	  else if(*(tmp-1) == '/') 
	      *(tmp-1) = '\0';
      }
      strncpy(path,buffer,sizeof(path) - 1);
      path[sizeof(path) - 1] = 0;
      sprintf(msg,"%s %s",query,path);
      aptv_Announce(self,msg);
  }
 OUT(aptv_QueryFileName);
  return  status;
}

long
aptv__QueryDirectoryName( self, query, response )
  register struct aptv	     *self;
  register char		     *query;
  register char		    **response;
  {
  register enum message_CompletionCode  result;
  static char				path[257];
  static char				buffer[513];
  static char				msg[513];
  register long				status = ok;
  char					*tmp = NULL;

  IN(aptv_QueryDirectoryName);
  im_GetDirectory(path);
  *buffer = 0;
  *response = buffer;
  result = (enum message_CompletionCode) completion_GetFilename( self, 
			  query,			/* prompt */
			  path,				/* initial string */
			  buffer,			/* working area */
			  sizeof buffer - 1,		/* size of working area */
			  1,				/* want directory, not file */
			  1 );				/* need not be existing file */
  DEBUGdt(Result,result);
  DEBUGst(File-name,buffer);
  if((result != message_Complete && result != message_CompleteValid) || *buffer==0) {
      status = failure;
      aptv_Announce(self,"Cancelled.");
  }
  else{
      if(buffer[strlen(buffer)-1] == '/')
	  buffer[strlen(buffer)-1] = 0;
      if(*(tmp = buffer+strlen(buffer)-1) == '.') {
	  if((*(tmp-1) == '.') && (*(tmp-2) == '/')) {
	      *(tmp-2) = '\0';
	      if(tmp = (char*)rindex(buffer,'/')) 
		  *tmp = '\0';
	  }
	  else if(*(tmp-1) == '/') 
	      *(tmp-1) = '\0';
      }
      strncpy( path, buffer, sizeof(path) - 1 );
      path[sizeof(path) - 1] = 0;
      sprintf(msg,"%s %s",query,path);
      aptv_Announce(self,msg);
  }
  OUT(aptv_QueryFileName);
  return(status);
  }


long
aptv__Announce( self, message )
  register struct aptv	      *self;
  register char		      *message;
  {
  register long		      status = ok;

  IN(aptv_Announce);
  if ( message_DisplayString( self, 0, message ) == -1 )
    status = failure;
  im_ForceUpdate();
  OUT(aptv_Announce);
  return  status;
  }
