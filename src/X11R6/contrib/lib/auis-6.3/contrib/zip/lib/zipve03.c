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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipve03.c,v 1.5 1993/05/04 01:51:05 susan Exp $";
#endif


 

/* zipve03.c	Zip EditView-object  -- Hit Handling		      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip EditView-object  --  Hit Handling

MODULE	zipve03.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Editing facilities
	of the Zip View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/31/88	Created (TCP)
  10/19/88	Reduce frequency of re-drawing Pane (Draw_Pane) (TCP)
  05/01/89	Use symbolic font-names (TCP)
  08/28/89	Suppress Draw_Pane() after building each figure build (SCG)
   08/16/90	Remove old color and line_style attribute copying from Duplicate_Selection (SCG)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include "class.h"
#include "view.ih"
#include "rasterv.ih"
#include "fontdesc.ih"
#include "zip.ih"
#include "zipv.ih"
#include "zipobj.ih"
#include "zipedit.ih"
#include "zipedit.h"
#include <math.h>

#define  InitialX		    (self->prior_x)
#define  InitialY		    (self->prior_y)

static int RBDT();
static int RBDM();
static int RBUT();
static int Handle_Edit_Selection();
static int Handle_Edit_Selection_Modification();
static int Edit_Modification_LBDT();
static int Edit_Modification_LBDM();
static int Edit_Modification_LBUT();
static Move_Selection();
static Duplicate_Selection();
static Within_Enclosure();
static Draw_Enclosure_Shadow();
static Clear_Enclosure_Shadow();
static Show_Enclosure_Shadow();
static Check_Enclosure();
static Draw_Enclosure();
static Clear_Enclosure();
static Enclosure_Bounds();
static Show_Names();
static Show_Point();
static Show_Font();
static Clear_Font();
static Set_Constraints();

int
zipedit__Accept_Hit( self, hit_pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      hit_pane;
  register long			      action, x, y, clicks;
  {
  register int			      status = zip_ok;
  register zip_type_pane	      pane;

  IN(zipedit_Accept_Hit);
  DEBUGst(HitPane-name,hit_pane->zip_pane_name);
  pane = View->pane;
  DEBUGst(Pane-name,pane->zip_pane_name);
  DEBUGdt(Abeyant,Abeyant);
  if ( hit_pane  &&  !Abeyant )
    { DEBUG(Not Abeyant);
    if ( Moving )  hit_pane = EditPane;
    if ( hit_pane == EditPane  ||  hit_pane == BackgroundPane )
      { DEBUG(EditPane Selected);
	hit_pane = EditPane;
        if ( KeyboardProcessor )
          {
	  DEBUG(>>> Keyboard Processor);
	  action = (long)((*KeyboardProcessor)( KeyboardAnchor, pane, NULL, action, x, y, clicks ));
	  DEBUG(<<< Keyboard Processor);
	  }
        switch ( action )
          {
          case view_RightDown:     status = RBDT( self, hit_pane, x, y, clicks ); break;
          case view_RightMovement: status = RBDM( self, hit_pane, x, y, clicks ); break;
          case view_RightUp:       status = RBUT( self, hit_pane, x, y, clicks ); break;
          default:  status = Handle_Edit_Selection( self, hit_pane, action, x, y, clicks );
	  }
      }
    else    if ( hit_pane == FontFamilyPane )
      status = zipedit_Handle_Font_Family_Selection( self, hit_pane, action, x, y, clicks );
    else    if ( hit_pane == FontHeightPane )
      status = zipedit_Handle_Font_Height_Selection( self, hit_pane, action, x, y, clicks );
    else    if ( hit_pane == FontItalicPane )
      status = zipedit_Handle_Font_Italic_Selection( self, hit_pane, action, x, y, clicks );
    else    if ( hit_pane == FontBoldPane )
      status = zipedit_Handle_Font_Bold_Selection(   self, hit_pane, action, x, y, clicks );
    else    if ( hit_pane == FontSamplePane )
      status = zipedit_Handle_Font_Sample_Selection( self, hit_pane, action, x, y, clicks );
/*===
    else    if ( hit_pane == HierarchyPane )
      status = zipedit_Handle_Hierarchy_Selection( self, hit_pane, action, x, y, clicks );
===*/
    else
    if ( x >= zipview_Pane_Left( View, FiguresPane )   &&
	 x <= zipview_Pane_Right( View, FiguresPane )  &&  
	 y >= zipview_Pane_Top( View, FiguresPane )    &&  
	 y <= zipview_Pane_Bottom( View, FiguresPane ) &&
	 hit_pane != FiguresPane )
      { DEBUG(Figure Palette Hit);
      Building = true;
      status = zipedit_Handle_Figure_Palette_Hit( self, hit_pane, action, x, y, clicks );
      }
    else
    if ( x >= zipview_Pane_Left( View, ShadesPane )   &&
	 x <= zipview_Pane_Right( View, ShadesPane )  &&  
	 y >= zipview_Pane_Top( View, ShadesPane )    &&  
	 y <= zipview_Pane_Bottom( View, ShadesPane ) )
      { DEBUG(Shade Palette Hit);
      status = zipedit_Handle_Shade_Palette_Hit( self, hit_pane, action, x, y, clicks );
      }
    }
    else  if ( BackgroundSelected ) 
      { DEBUG(BackgroundPane Hit);
      rasterview_Hit( BackgroundView, action,
			rasterview_EnclosedXToLocalX( BackgroundView, x ),
			rasterview_EnclosedYToLocalY( BackgroundView, y ), clicks );
      }
  OUT(zipedit_Accept_Hit);
  return status;
  }

static zip_type_point	      initial_x_point, initial_y_point,
			      final_x_point, final_y_point;

static int
RBDT( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register int			      x, y;
  {
  register int			      status = zip_ok;

  IN(RBDT);
  if ( KeyboardProcessor )
    (*KeyboardProcessor)( KeyboardAnchor, pane, NULL, NULL, NULL, NULL, NULL );
  zipedit_Reset_Editing_Selection( self, pane );
  zipedit_Cancel_Enclosure( self, pane );
  if ( ForegroundPinning )
    {
/*===    status = zipview_Handle_Pinning( View, pane, x, y, NULL, NULL );*/
    status = zip_ok;
    }
    else
    {
    if ( ForegroundPanning )
      Set_Constraints( self, pane, x, y, &initial_x_point, &initial_y_point );
    status = zipview_Initiate_Panning( View, pane, x, y, NULL /*===*/ );
    }
  OUT(RBDT);
  return status;
  }

static int
RBDM( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register int			      x, y;
  {
  register int			      status = zip_ok;

  IN(RBDM);
  status = zipview_Continue_Panning( View, pane, x, y );
  OUT(RBDM);
  return status;
  }

static int
RBUT( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register int			      x, y;
  {
  register int			      status = zip_ok;
  register zip_type_image	      image =
			CurrentStream->zip_stream_image_anchor;
  long				      x_delta, y_delta;

  IN(RBUT);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( ForegroundPanning )
    { DEBUG(ForeGround Panning);
    status = zipview_Terminate_Panning( View, pane, x, y, NULL, NULL, 0 );
    Set_Constraints( self, pane, x, y, &final_x_point, &final_y_point );
    if ( final_x_point - initial_x_point  ||  final_y_point - initial_y_point )
      {
      zipview_Clear_Image( View, image, pane );
/*===      CurrentStream->zip_stream_pseudo_x_offset +=
	            final_x_point - initial_x_point;
      CurrentStream->zip_stream_pseudo_y_offset +=
	            final_y_point - initial_y_point;===*/
      zip_Adjust_Image_Point_Suite( Data, image,
		    final_x_point - initial_x_point,
		    final_y_point - initial_y_point/*===, true*/ );
      zipview_Draw_Image( View, image, pane );
      }
    }
    else
    { DEBUG(Not ForeGround Panning);
    if ( BackgroundPane )
      { DEBUG(BackgroundPane Exists);
      DEBUGst(BackgroundPane-name,BackgroundPane->zip_pane_name);
      zipview_Terminate_Panning( View, pane, x, y, &x_delta, &y_delta, 0 );
      if ( x_delta  ||  y_delta )
	{
        BackgroundPane->zip_pane_x_offset += x_delta;
        BackgroundPane->zip_pane_y_offset += -y_delta;
        BackgroundPane->zip_pane_x_origin_offset = 
	  BackgroundPane->zip_pane_x_origin + BackgroundPane->zip_pane_x_offset;
        BackgroundPane->zip_pane_y_origin_offset =
	  BackgroundPane->zip_pane_y_origin - BackgroundPane->zip_pane_y_offset;
	if ( BackgroundExposed )
	  { DEBUG(BackgroundPane Exposed);
	  pane->zip_pane_x_offset += x_delta;
	  pane->zip_pane_y_offset += -y_delta;
	  pane->zip_pane_x_origin_offset =
	    pane->zip_pane_x_origin + pane->zip_pane_x_offset;
	  pane->zip_pane_y_origin_offset =
	    pane->zip_pane_y_origin - pane->zip_pane_y_offset;
          zipedit_Display_Background_Pane( self, pane );
          zipview_Draw_Pane( View, pane );
	  }
	  else
	  { DEBUG(BackgroundPane Not Exposed);
	  zipview_Pan_Pane( View, pane, x_delta, -y_delta );
	  }
	}
      }
      else  status = zipview_Terminate_Panning( View, pane, x, y, NULL, NULL, 1 );
    }
  OUT(RBUT);
  return status;
  }

static int
Handle_Edit_Selection( self, pane, action, x, y, clicks )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  {
  register long				  status = zip_ok;
  long					  X, Y;

  IN(Handle_Edit_Selection);
  if ( BuildPending == NULL )
    status = Handle_Edit_Selection_Modification( self, pane, action, x, y, clicks );
    else
    {
    DEBUG(Build Pending);
    zipedit_Cancel_Enclosure( self, pane );
    if ( CurrentFigure )
      if ( SelectionLevel >= ImageSelection )
        zipedit_Normalize_Image_Points( self, CurrentImage, pane );
        else
        zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
    SelectionLevel = 0;
    zipedit_Set_Pane_Highlight_Icon( self, pane, zip_figure_point_icon );

    Set_Constraints( self, pane, x, y, &X, &Y );
    if ( ( X - LastPointX )  ||  ( Y - LastPointY )  ||
	 ! (action == view_LeftMovement) )
      {
      LastPixelX = x;      LastPixelY = y;
      LastPointX = X;      LastPointY = Y;
      if ( action == view_LeftDown )
	{ DEBUG(LeftDown);
	/* Make Cursor disappear -- this currently crashes PS/2 AIX X Server 9/28/89  */
	EditingIcon(pane) = pane->zip_pane_cursor_icon;
	zipview_Set_Pane_Cursor( View, pane, '@', CursorFontName );
	}
      status = zipobject_Build_Object( Objects(BuildPending), pane, action, x, y, clicks, X, Y );
      if ( action == view_LeftUp )
	{ DEBUG(LeftUp);
	zipview_Set_Pane_Cursor( View, pane, EditingIcon(pane), CursorFontName );
	if ( CurrentFigure )
	  {
/*===	  if ( !ManualRefresh )
	    zipview_Draw_Pane( View, pane ); ===*/
	  zipedit_Highlight_Figure_Points( self, CurrentFigure, pane );
          zipedit_Expose_Selection_Menu( self );
	  }
	}
      }
    }
  OUT(Handle_Edit_Selection);
  return status;
  }

static int
Handle_Edit_Selection_Modification( self, pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register int			      status = zip_ok;

  IN(Handle_Edit_Selection_Modification);
  switch ( action )
    {
    case view_LeftDown:
      status = Edit_Modification_LBDT( self, pane, x, y );
      break;
    case view_LeftUp:
      status = Edit_Modification_LBUT( self, pane, x, y );
      break;
    case view_LeftMovement:
      status = Edit_Modification_LBDM( self, pane, x, y );
      break;
    default:
      ;
    }
  OUT(Handle_Edit_Selection_Modification);
  return status;
  }

static int
Edit_Modification_LBDT( self, pane, x, y )
  register struct zipedit	      *self;
  register zip_type_pane	      pane;
  register zip_type_pixel	      x, y;
  {
  register long			      status = zip_ok;
  register zip_type_figure	      figure = NULL;
  register long			      point = NULL;
  struct timeval		      dummy_time, dummy_zone;
  static long			      prior_time, this_time;

  IN(Edit_Modification_LBDT);
  Moving = false;
  InitialX = x;  InitialY = y;
  gettimeofday( &dummy_time, &dummy_zone );
  this_time = (dummy_time.tv_sec * 1000) + (dummy_time.tv_usec / 1000);
  zipview_Set_Pane_Cursor( View, pane, 'A', CursorFontName );
  EnclosureShadowStartX = EnclosureShadowStartY =
	 EnclosureShadowLastX = EnclosureShadowLastY = -1;
  if ( !Within_Enclosure( self, pane, x, y ) )
    { DEBUG(Not Within Enclosure);
    zipedit_Cancel_Enclosure( self, pane );
    EnclosureLeft = x; EnclosureTop = y; EnclosureWidth = EnclosureHeight = 0;
    if ( CurrentFigure  &&  CurrentFigure->zip_figure_visibility == zip_figure_exposed  &&
         zipobject_Proximate_Object_Points( Objects(CurrentFigure->zip_figure_type),
		CurrentFigure, pane, x, y ) )
	figure = CurrentFigure;
    if ( figure  ||  (figure = zipview_Which_Pane_Figure( View, x, y, pane )) )
      {
      zipedit_Expose_Selection_Menu( self );
      point = zipedit_Which_Figure_Point( self, figure, pane, x, y );
      }
    if ( figure == NULL  ||  figure != CurrentFigure )
      {
      if ( SelectionLevel >= ImageSelection )
        zipedit_Normalize_Image_Points( self, CurrentImage, pane );
        else
        zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
      if ( figure )
        SelectionLevel = PointSelection;
        else
        SelectionLevel = 0;
      zipedit_Set_Pane_Highlight_Icon( self, pane, zip_figure_point_icon );
      }
    if ( figure  &&  figure == CurrentFigure  &&
       point  == pane->zip_pane_edit->zip_pane_edit_last_point_id  &&
       (this_time - prior_time) < ZIP_double_click_parameter )
      {
      if ( ++SelectionLevel == ImageSelection )
        {
        zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
        CurrentImage = figure->zip_figure_image;
        zipedit_Set_Pane_Highlight_Icon( self, pane, zip_image_point_suite_icon );
        }
        else
        {
        if ( SelectionLevel >= ImageSelection  &&  CurrentImage )
          {
          zipedit_Normalize_Image_Points( self, CurrentImage, pane );
	  if ( CurrentImage->zip_image_superior )
            CurrentImage = CurrentImage->zip_image_superior;
          zipedit_Set_Pane_Highlight_Icon( self, pane, zip_image_point_suite_icon );
          }
          else
	  {
          zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
          zipedit_Set_Pane_Highlight_Icon( self, pane, zip_figure_point_suite_icon );
	  }
	}
      }
    else
    {
    SelectionLevel = PointSelection;
    zipedit_Set_Pane_Highlight_Icon( self, pane, zip_figure_point_icon );
    if ( figure )
      {
      CurrentFigure = figure;
      CurrentImage  = figure->zip_figure_image;
      CurrentStream = figure->zip_figure_image->zip_image_stream;
/*===      CurrentPane   = pane;*/
      }
      else
      {
      zipedit_Hide_Selection_Menu( self );
      CurrentFigure = NULL;
      if ( CurrentImage )
	CurrentStream = CurrentImage->zip_image_stream;
	else
	{
	if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source  &&
	     pane->zip_pane_source.zip_pane_stream )
	  CurrentStream = pane->zip_pane_source.zip_pane_stream;
	else
	if ( pane->zip_pane_attributes.zip_pane_attribute_image_source  &&
	     pane->zip_pane_source.zip_pane_image )
    	  CurrentStream = pane->zip_pane_source.zip_pane_image->zip_image_stream;
	else
	if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source  &&
	     pane->zip_pane_source.zip_pane_figure )
    	  CurrentStream = pane->zip_pane_source.zip_pane_figure->zip_figure_image->zip_image_stream;
	}
      }
    }
  pane->zip_pane_edit->zip_pane_edit_beginning = true;
  LastPixelX = x;  LastPixelY = y;
  if ( figure )
    {
    zipview_Set_Pane_Cursor( View, pane, '@', CursorFontName ); /* Make Cursor disappear */
    Set_Constraints( self, pane, LastPixelX, LastPixelY, &LastPointX, &LastPointY );
    if ( SelectionLevel >= ImageSelection )
      zipedit_Highlight_Image_Points( self, CurrentImage, pane );
      else
      zipedit_Highlight_Figure_Points( self, CurrentFigure, pane );
    pane->zip_pane_edit->zip_pane_edit_last_point_id = point;	 
    Show_Names( self, pane );
    Show_Point( self, pane, figure, point );
/*=== should check upward also */
/*=== don't we have a macro for addressing font-name ? */
    if ( figure->zip_figure_font )
      Show_Font( self, pane, Fonts->zip_fonts_vector[figure->zip_figure_font].
				 zip_fonts_table_name );
      else
      Clear_Font( self, pane );
    }
    }
    else
    { DEBUG(Within Enclosure);
    Draw_Enclosure_Shadow( self, pane, x, y );
    }
  prior_time = this_time;
  OUT(Edit_Modification_LBDT);
  return status;
  }

static int
Edit_Modification_LBDM( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register int			      x, y;
  {
  register int			      status = zip_ok;
  int				      X, Y;

  IN(Edit_Modification_LBDM);
  Moving = true;
  if ( EnclosureExposed )
    { DEBUG(Within Enclosure);
    Draw_Enclosure_Shadow( self, pane, x, y );
    }
    else
    { DEBUG(Outside Enclosure);
    if ( CurrentFigure )
      {
      Set_Constraints( self, pane, x, y, &X, &Y );
      if ( abs(X - LastPointX)  ||  abs(Y - LastPointY) )
        {
        if ( pane->zip_pane_edit->zip_pane_edit_beginning )
	  if ( SelectionLevel >= ImageSelection )
	    zipedit_Normalize_Image_Points( self, CurrentImage, pane );
	    else
	    zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
        zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
        switch ( SelectionLevel )
	  {
	  case PointSelection:
	    if ( pane->zip_pane_edit->zip_pane_edit_beginning )
	      zipview_Clear_Figure( View, CurrentFigure, pane );
	      else
	      zipview_Draw_Figure( View, CurrentFigure, pane );
	    zip_Set_Figure_Point( Data, CurrentFigure,
		pane->zip_pane_edit->zip_pane_edit_last_point_id, X, Y );
	    zipview_Draw_Figure( View, CurrentFigure, pane );
	    break;
	  case FigureSelection:
	    if ( pane->zip_pane_edit->zip_pane_edit_beginning )
	      zipview_Clear_Figure( View, CurrentFigure, pane );
	      else
	      zipview_Draw_Figure( View, CurrentFigure, pane );
	    zip_Adjust_Figure_Point_Suite( Data, CurrentFigure,
		 X - LastPointX,
		 Y - LastPointY );
	    zipview_Draw_Figure( View, CurrentFigure, pane );
	    break;
	  default:
	    if ( pane->zip_pane_edit->zip_pane_edit_beginning )
	      zipview_Clear_Image( View, CurrentImage, pane );
	      else
	      zipview_Draw_Image( View, CurrentImage, pane );
	    zip_Adjust_Image_Point_Suite( Data, CurrentImage,
		 X - LastPointX, Y - LastPointY );
	    zipview_Draw_Image( View, CurrentImage, pane );
	  }
        zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
        Show_Point( self, pane, CurrentFigure,
			      pane->zip_pane_edit->zip_pane_edit_last_point_id );
        LastPointX = X;  LastPointY = Y;
        pane->zip_pane_edit->zip_pane_edit_beginning = false;
	}
      }
      else
      { DEBUG(Enclosuring);
      Draw_Enclosure( self, pane, x, y );
      }
    }
  LastPixelX = x;  LastPixelY = y;
  OUT(Edit_Modification_LBDM);
  return status;
  }

static int
Edit_Modification_LBUT( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register long			      x, y;
  {
  IN(Edit_Modification_LBUT);
  Moving = false;
  if ( CurrentFigure )
    {
    pane->zip_pane_edit->zip_pane_edit_beginning = false;
    Show_Point( self, pane, CurrentFigure, pane->zip_pane_edit->zip_pane_edit_last_point_id );
    if ( SelectionLevel >= ImageSelection )
      zipedit_Normalize_Image_Points( self, CurrentImage, pane );
      else
      zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
    if ( !ManualRefresh  &&  (abs(x - InitialX) > 3  ||  abs(y - InitialY) > 3) )
      zipview_Draw_Pane( View, pane );
    if ( SelectionLevel >= ImageSelection )
      zipedit_Highlight_Image_Points( self, CurrentImage, pane );
      else
      zipedit_Highlight_Figure_Points( self, CurrentFigure, pane );
    }
    else
    {
    if ( CurrentStream  &&  EnclosureWidth )
      {
      EnclosureExposed = true;
      Draw_Enclosure_Shadow( self, pane, x, y );
      if ( EnclosedFigures ||
	   Check_Enclosure( self, pane, CurrentStream->zip_stream_image_anchor, 0 ) )
	{
        zipedit_Expose_Selection_Menu( self );
	Move_Selection( self, pane );
	}
	else
	{
	zipedit_Cancel_Enclosure( self, pane );
	}
      EnclosureShadowStartX = EnclosureShadowStartY =
	 EnclosureShadowLastX = EnclosureShadowLastY = -1;
      }
    }
  zipview_Set_Pane_Cursor( View, pane, 'A', CursorFontName );
  OUT(Edit_Modification_LBUT);
  return  zip_ok;
  }

zip_type_figure
zipedit_Next_Selected_Figure( self, pane, figure )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_figure	      figure;
  {
  register zip_type_figure	      next = NULL;
  register long			      i = 0;

  IN(zipedit_Next_Selected_Figure);
  if ( figure  &&  figure != CurrentFigure )
    { DEBUG(Nth selection);
    while ( EnclosedFigures  &&  EnclosedFigures[i] )
      if ( EnclosedFigures[i] == figure )
	{
	next = EnclosedFigures[i+1];
	break;
	}
	else  i++;
    }
    else
    { DEBUG(Initial selection);
    if ( EnclosedFigures )
      next = EnclosedFigures[0];
      else
      if ( figure == NULL )
	next = CurrentFigure;
    }
  DEBUGxt(Next,next);
  OUT(zipedit_Next_Selected_Figure);
  return  next;
  }

static
Move_Selection( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  register zip_type_point	      X_delta, Y_delta;
  zip_type_point		      X1, Y1, X2, Y2;
  long				      L, T, W, H,
				      i = 0;

  IN(Move_Selection);
  if ( EnclosedFigures  &&  EnclosureShadowStartX > 0  &&
	( abs(EnclosureShadowStartX - EnclosureShadowLastX) > 2  ||
	  abs(EnclosureShadowStartY - EnclosureShadowLastY) > 2 ) )
    {
    Clear_Enclosure_Shadow( self, pane );
    Set_Constraints( self, pane, EnclosureShadowStartX, EnclosureShadowStartY, &X1, &Y1 );
    Set_Constraints( self, pane, EnclosureShadowLastX,  EnclosureShadowLastY,  &X2, &Y2 );
    X_delta = X2 - X1;
    DEBUGdt(X-delta,X_delta);
    Y_delta = Y2 - Y1;
    DEBUGdt(Y-delta,Y_delta);
    if ( DuplicateSelection )
      {
      Duplicate_Selection( self, pane, X_delta, Y_delta );
      Show_Enclosure( self, pane );
      }
      else
      {
      while( EnclosedFigures[i] )
        {
        DEBUGst(Adjust Figure,EnclosedFigures[i]->zip_figure_name);
        zipedit_Normalize_Figure_Points( self, EnclosedFigures[i], pane );
        zip_Adjust_Figure_Point_Suite( Data, EnclosedFigures[i], X_delta, Y_delta );
        i++;
     	}
      Enclosure_Bounds( self, pane, &L, &T, &W, &H );
      zipview_SetTransferMode( View, graphic_WHITE );
      zipview_EraseRectSize( View, L-2, T-2, W+4, H+4 );
      EnclosureLeft = L + (EnclosureShadowLastX - EnclosureShadowStartX);
      EnclosureTop  = T + (EnclosureShadowLastY - EnclosureShadowStartY);
      EnclosureHeight = H; EnclosureWidth = W;
      }
    zipview_Draw_Pane( View, pane );
    Show_Enclosure( self, pane );
    }
  OUT(Move_Selection);
  }

static
Duplicate_Selection( self, pane, x_delta, y_delta )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_point	      x_delta, y_delta;
  {
  register zip_type_figure	      original_figure = NULL;
  zip_type_figure		      new_figure, peer_figure = NULL;
/*===
  char				      name[512];
  static long			      name_serial = 1;
===*/
  register long			      i;

  IN(Duplicate_Selection);
  peer_figure = CurrentImage->zip_image_figure_anchor;
  while ( peer_figure  &&  peer_figure->zip_figure_next )
    peer_figure = peer_figure->zip_figure_next;
  while ( original_figure = zipedit_Next_Selected_Figure( self, Pane, original_figure ) )
    { DEBUGst(Duplicating Figure-name,original_figure->zip_figure_name );
    zipedit_Normalize_Figure_Points( self, original_figure, pane );
    zip_Create_Figure( Data, &new_figure, NULL, original_figure->zip_figure_type,
		       CurrentImage, peer_figure );
    if ( original_figure->zip_figure_name )
	{
/*=== generate non-duplicate name*/
	}
    new_figure->zip_figure_datum.zip_figure_anchor =
	original_figure->zip_figure_datum.zip_figure_anchor;
    new_figure->zip_figure_font = original_figure->zip_figure_font;
    new_figure->zip_figure_state = original_figure->zip_figure_state;
    new_figure->zip_figure_state.zip_figure_state_points_highlighted = off;
    new_figure->zip_figure_visibility = original_figure->zip_figure_visibility;
    new_figure->zip_figure_mode = original_figure->zip_figure_mode;
    new_figure->zip_figure_style = original_figure->zip_figure_style;
    new_figure->zip_figure_line_width = original_figure->zip_figure_line_width;
/* === duplicate proper line style and color attributes here === */
/*    new_figure->zip_figure_line_style = original_figure->zip_figure_line_style;
    new_figure->zip_figure_color = original_figure->zip_figure_color; */
    new_figure->zip_figure_fill.zip_figure_shade =
	original_figure->zip_figure_fill.zip_figure_shade;
    new_figure->zip_figure_zoom_level = original_figure->zip_figure_zoom_level;
    new_figure->zip_figure_detail_level = original_figure->zip_figure_detail_level;
    new_figure->zip_figure_point.zip_point_x = original_figure->zip_figure_point.zip_point_x;
    new_figure->zip_figure_point.zip_point_y = original_figure->zip_figure_point.zip_point_y;
    if ( original_figure->zip_figure_points )
      {
      new_figure->zip_figure_points = (zip_type_point_pairs)
	malloc( sizeof(long) + (original_figure->zip_figure_points->zip_points_count *
	    sizeof(struct zip_point_pair)) );
      new_figure->zip_figure_points->zip_points_count =
	original_figure->zip_figure_points->zip_points_count;
      DEBUGdt(Points-count,new_figure->zip_figure_points->zip_points_count);
      for ( i = 0; i < original_figure->zip_figure_points->zip_points_count; i++ )
        {
        new_figure->zip_figure_points->zip_points[i].zip_point_x = 
	    original_figure->zip_figure_points->zip_points[i].zip_point_x;
        new_figure->zip_figure_points->zip_points[i].zip_point_y = 
	    original_figure->zip_figure_points->zip_points[i].zip_point_y;
        }
      }
    zip_Adjust_Figure_Point_Suite( Data, new_figure, x_delta, y_delta );
    peer_figure = new_figure;
    }
  OUT(Duplicate_Selection);
  }

zipedit_Cancel_Enclosure( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  register long			      i = 0;

  IN(zipedit_Cancel_Enclosure);
  zipedit_Hide_Selection_Menu( self );
  if ( EnclosureExposed )
    { DEBUG(Exposed);
    Clear_Enclosure( self, pane );
    }
  if ( EnclosedFigures )
    { DEBUG(Enclosed Figure to Normalize);
    while ( EnclosedFigures[i] )
      {
      zipedit_Normalize_Figure_Points( self, EnclosedFigures[i], pane );
      i++;
      }
    free( EnclosedFigures );
    EnclosedFigures = NULL;
    }
  DuplicateSelection = false;
  EnclosureLeft = EnclosureTop = EnclosureWidth = EnclosureHeight = 0;
  EnclosureShadowStartX = EnclosureShadowStartY =
    EnclosureShadowLastX = EnclosureShadowLastY = -1;
  OUT(zipedit_Cancel_Enclosure);
  }

static
Within_Enclosure( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_pixel	      x, y;
  {
  register boolean		      within = false;
  long				      L, T, W, H;

  IN(Within_Enclosure);
  Enclosure_Bounds( self, pane, &L, &T, &W, &H );
  if ( EnclosureExposed  &&
       x > L  &&  x < L + W  &&  y > T  &&  y < T + H )
    within = true;
  OUT(Within_Enclosure);
  return  within;
  }

static
Draw_Enclosure_Shadow( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register long			      x, y;
  {
  IN(Draw_Enclosure_Shadow);
  if ( EnclosureShadowStartX < 0 )
    {
    EnclosureShadowStartX = EnclosureShadowLastX = x;
    EnclosureShadowStartY = EnclosureShadowLastY = y;
    }
    else
    {
    Clear_Enclosure_Shadow( self, pane );
    EnclosureShadowLastX = x;
    EnclosureShadowLastY = y;
    Show_Enclosure_Shadow( self, pane );
    }
  OUT(Draw_Enclosure_Shadow);
  }

static
Clear_Enclosure_Shadow( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  IN(Clear_Enclosure_Shadow);
  if ( EnclosureShadowLastX > 0 )
    Show_Enclosure_Shadow( self, pane );
  OUT(Clear_Enclosure_Shadow);
  }

static
Show_Enclosure_Shadow( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  long				      L, T, W, H;

  IN(Show_Enclosure_Shadow);
  if ( abs(EnclosureShadowStartX - EnclosureShadowLastX) > 2  ||
       abs(EnclosureShadowStartY - EnclosureShadowLastY) > 2 )
    {
    Enclosure_Bounds( self, pane, &L, &T, &W, &H );
/*===*/
zipview_SetTransferMode( View, graphic_INVERT );
if ( zipview_GetLineWidth( View ) != 1 )
  zipview_SetLineWidth( View, 1 );
zipview_DrawRectSize( View,
L + (EnclosureShadowLastX - EnclosureShadowStartX),
T + (EnclosureShadowLastY - EnclosureShadowStartY),
W, H );
zipview_FlushGraphics( View );
/*===*/
    }
  OUT(Show_Enclosure_Shadow);
  }

zipedit_Enclose_Figure( self, figure, pane )
  register struct zipedit	     *self;
  register zip_type_figure	      figure;
  register zip_type_pane	      pane;
  {
  IN(zipedit_Enclose_Figure);
  if ( figure )
    {
    zipedit_Normalize_Figure_Points( self, figure, pane );
    zipobject_Object_Enclosure( Objects(figure->zip_figure_type), figure, pane,
		&EnclosureLeft, &EnclosureTop, &EnclosureWidth, &EnclosureHeight );
    EnclosureLeft   -= 4;
    EnclosureTop    -= 4;
    EnclosureWidth  += 6;
    EnclosureHeight += 6;
    EnclosureExposed = true;
    Show_Enclosure( self, pane );
    Check_Enclosure( self, pane, CurrentImage, 0 );
    }
  OUT(zipedit_Enclose_Figure);
  }

static
Check_Enclosure( self, pane, image, count )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_image	      image;
  register long			      count;
  {
  register zip_type_figure	      figure;
  long				      L, T, W, H;

  IN(Check_Enclosure);
  Enclosure_Bounds( self, pane, &L, &T, &W, &H );
  if ( image )
    {
    DEBUGst(Image-name,image->zip_image_name);
    if ( count == 0  &&  EnclosedFigures )
      {
      free( EnclosedFigures );
      EnclosedFigures = NULL;
      }
    figure = image->zip_image_figure_anchor;
    while ( figure )
      {
      DEBUGst(Figure-name,figure->zip_figure_name);
      if ( (figure->zip_figure_visibility == zip_figure_exposed)  &&
	   zipobject_Enclosed_Object( Objects(figure->zip_figure_type), figure, pane,
		L, T, W, H ) )
        { DEBUG(Figure is Enclosed);
        zipedit_Highlight_Figure_Points( self, figure, pane );
        if ( EnclosedFigures )
	  { DEBUG(Enclosed Figures exist);
	  EnclosedFigures = (zip_type_figure *) realloc( EnclosedFigures,
			    (count + 2) * sizeof(zip_type_figure *) );
	  EnclosedFigures[count] = figure;
	  EnclosedFigures[count+1] = NULL;
	  count++;
	  }
	  else
	  { DEBUG(First Enclosed Figure);
	  EnclosedFigures = (zip_type_figure *) calloc( 2, sizeof(zip_type_figure *) );
	  EnclosedFigures[0] = figure;
	  EnclosedFigures[1] = NULL;
	  count = 1;
	  }
        }
      figure = figure->zip_figure_next;
      }
    count = Check_Enclosure( self, pane, image->zip_image_inferior, count );
    count = Check_Enclosure( self, pane, image->zip_image_peer, count );
    }
  DEBUGdt(Count,count);
  OUT(Check_Enclosure);
  return  count;
  }

static
Draw_Enclosure( self, pane, x, y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register long			      x, y;
  {
  IN(Draw_Enclosure);
  if ( x > (zipview_Pane_Left(   View, pane ) + 2)  &&
       x < (zipview_Pane_Right(  View, pane ) - 3)  &&  
       y > (zipview_Pane_Top(    View, pane ) + 2)  &&  
       y < (zipview_Pane_Bottom( View, pane ) - 3) )
    {
    Clear_Enclosure( self, pane );
    EnclosureWidth  = x - EnclosureLeft;
    EnclosureHeight = y - EnclosureTop;
    Show_Enclosure( self, pane );
    }
  OUT(Draw_Enclosure);
  }

static
Clear_Enclosure( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  IN(Clear_Enclosure);
  EnclosureExposed = false;
  if ( EnclosureWidth > 2  ||  EnclosureWidth < -2 )
    {
    Show_Enclosure( self, pane );
    }
  OUT(Clear_Enclosure);
  }

Show_Enclosure( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  long				     L, T, W, H;

  IN(Show_Enclosure);
  Enclosure_Bounds( self, pane, &L, &T, &W, &H );
  if ( W > 2  &&  H > 2 )
    {
/*===*/
zipview_SetTransferMode( View, graphic_INVERT );
if ( zipview_GetLineWidth( View ) != 1 )
  zipview_SetLineWidth( View, 1 );
zipview_DrawRectSize( View, L, T, W, H );
zipview_DrawRectSize( View, L-1, T-1, W+2, H+2 );
zipview_FlushGraphics( View );
/*===*/
    }
  OUT(Show_Enclosure);
  }

static
Enclosure_Bounds( self, pane, L, T, W, H )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_pixel	     *L, *T, *W, *H;
  {
  register zip_type_pixel	      left = EnclosureLeft,  top = EnclosureTop,
				      width = EnclosureWidth, height = EnclosureHeight;
  IN(Enclosure_Bounds);
  if ( width  < 0 )
    { *L = left + width;  *W = -width; }
    else
    { *L = left;  *W = width; }
  if ( height < 0 )
    { *T = top  + height; *H = -height; }
    else
    { *T = top; *H = height; }
  OUT(Enclosure_Bounds);
  }

static
Show_Names( self, pane )
  register struct zipedit	      *self;
  register zip_type_pane	       pane;
  {
  register zip_type_figure	       name_figure;

  if ( PalettesExposed  &&  NamePalette )
    {
    name_figure = zip_Figure( Data, ZIP_current_figure_text_name );
    zipview_Clear_Figure( View, name_figure, NamesPane );
    zip_Set_Figure_Text( Data, name_figure, zip_Figure_Name( Data, CurrentFigure ) );
    zipview_Draw_Figure( View, name_figure, NamesPane );

    name_figure = zip_Figure( Data, ZIP_current_image_text_name );
    zipview_Clear_Figure( View, name_figure, NamesPane );
    zip_Set_Figure_Text( Data, name_figure, zip_Image_Name( self, CurrentImage ) );
    zipview_Draw_Figure( View, name_figure, NamesPane );

    name_figure = zip_Figure( Data, ZIP_current_stream_text_name );
    zipview_Clear_Figure( View, name_figure, NamesPane );
    zip_Set_Figure_Text( Data, name_figure, zip_Stream_Name( self, CurrentStream ) );
    zipview_Draw_Figure( View, name_figure, NamesPane );
    }
  }

static
Show_Point( self, pane, figure, point )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_figure	      figure;
  register int			      point;
  {
  register zip_type_figure	      name_figure;
  char				      msg[257];
  zip_type_point		      X, Y;

  IN(Show_Point);
  if ( PalettesExposed  &&  AttributePalette )
    {
    zipobject_Object_Point( Objects(figure->zip_figure_type), figure, point, &X, &Y );
    name_figure = zip_Figure( Data, ZIP_current_x_point_text_name );
    zipview_Clear_Figure( View, name_figure, AttributesPane );
    sprintf( msg, "%d", X );
    zip_Set_Figure_Text( Data, name_figure, msg);
    zipview_Draw_Figure( View, name_figure, AttributesPane );

    name_figure = zip_Figure( Data, ZIP_current_y_point_text_name );
    zipview_Clear_Figure( View, name_figure, AttributesPane );
    sprintf( msg, "%d", Y );
    zip_Set_Figure_Text( Data, name_figure, msg);
    zipview_Draw_Figure( View, name_figure, AttributesPane );
    }
  OUT(Show_Point);
  }

static
Show_Font( self, pane, font_name )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register char			     *font_name;
  {
  register zip_type_figure	      name_figure;

  IN(Show_Font);
  if ( PalettesExposed  &&  AttributePalette )
    {
    name_figure = zip_Figure( Data, ZIP_current_font_text_name );
    zipview_Clear_Figure( View, name_figure, AttributesPane );
    zip_Set_Figure_Text( Data, name_figure, font_name );
    zipview_Draw_Figure( View, name_figure, AttributesPane );
    }
  OUT(Show_Font);
  }

static
Clear_Font( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  register zip_type_figure	      name_figure;

  IN(Clear_Font);
  if ( PalettesExposed  &&  AttributePalette )
    {
    name_figure = zip_Figure( Data, ZIP_current_font_text_name );
    zipview_Clear_Figure( View, name_figure, AttributesPane );
    zip_Set_Figure_Text( Data, name_figure, "" );
    }
  OUT(Clear_Font);
  }

static
Set_Constraints( self, pane, x, y, X, Y )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register zip_type_pixel	      x,  y;
  register zip_type_point	     *X, *Y;
  {
  register double		      point_offset, point_delta;

  IN(Set_Constraints);
  *X = zipview_X_Pixel_To_Point( View, pane, NULL, x );
  *Y = zipview_Y_Pixel_To_Point( View, pane, NULL, y );
  if ( pane->zip_pane_edit->zip_pane_edit_coordinate_grid )
    {
    point_delta = pane->zip_pane_edit->zip_pane_edit_mark_point_delta /
	    pane->zip_pane_edit->zip_pane_edit_coordinate_grid;
    /* point_offset = abs(X) % point_delta */
    point_offset = abs(abs((double)*X) -
			((int)(abs((double)*X) / point_delta) * point_delta));
    if ( point_offset > point_delta/2.0 )
      if ( *X > 0 )
        *X += (point_delta - point_offset);
	else
        *X -= (point_delta - point_offset);
      else
      if ( *X > 0 )
        *X -= point_offset;
	else
        *X += point_offset;
    point_offset = abs(abs((double)*Y) -
			((int)(abs((double)*Y) / point_delta) * point_delta));
    if ( point_offset > point_delta/2.0 )
      if ( *Y > 0 )
        *Y += (point_delta - point_offset);
	else
        *Y -= (point_delta - point_offset);
      else
      if ( *Y > 0 )
        *Y -= point_offset;
	else
        *Y += point_offset;
    }
  OUT(Set_Constraints);
  }
