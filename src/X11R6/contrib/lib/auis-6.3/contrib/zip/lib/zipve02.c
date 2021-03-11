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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipve02.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipve02.c	Zip EditView-object  -- Palettes		      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipve02.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:03:54  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:58:46  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.16  1991/09/12  16:45:06  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.15  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.14  1990/08/21  14:44:39  sg08
 * Improve Re-Highlight handles on Change_Shade, to work all the time
 *
 * Revision 2.13  90/01/26  13:11:08  susan
 * sprintf used to return the pointer to the string it assembled.
 * now it returns int.  
 * 
 * Revision 2.12  89/09/11  08:17:55  ghoti
 * fix enumeration type clashes - specifically those dealing with mouse actions
 * 
 * Revision 2.11  89/08/30  16:29:54  sg08
 * Retain highlighting of selected figures when changing shade.
 * 
 * Revision 2.10  89/08/03  12:18:29  ghoti
 * added inlcude of andrewos.h for bcopy and index (HPUX)
 * changed #include "" to #include <>
 * 
 * Revision 2.9  89/05/01  22:13:07  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.8  89/02/25  03:29:01  ghoti
 * hc fixes
 * 
 * Revision 2.7  89/02/24  19:43:42  ghoti
 * dataobj - dataobject
 * 
 * Revision 2.6  89/02/17  18:10:03  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.5  89/02/08  16:52:28  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  20:51:33  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/10/13  13:03:16  tom
 * Fix setting of Caption Vert/Horiz mode.
 * 
 * Revision 2.2  88/10/11  20:42:42  tom
 * Correct null cursor for X wm.
 * 
 * Revision 2.1  88/09/27  18:20:06  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:50:41  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:47:43  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip EditView-object  --  Palettes

MODULE	zipve02.c

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
  10/13/88	Fix Mode Setting on Captions (TCP)
  05/01/89	Use symbolic font-names (TCP)
  08/22/89	Re-Highlight handles on Change_Shade (SCG)
   08/16/90	Improve Re-Highlight handles on Change_Shade, to work all the time (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <andrewos.h>
#include <view.ih>
#include <fontdesc.ih>
#include <zip.ih>
#include <zipv.ih>
#include <zipobj.ih>
#include <zipedit.ih>
#include <zipedit.h>

static int Parse_Stream_Image_Ending();
static Change_Shade();
static int Figure_Palette_LBDT();
static int Create_Name_Palette();
static int Create_Font_Palette();
static int Create_Font_Icon();
static int Create_Shade_Palette();
static int Create_Figure_Palette();
static int Create_Figure_Icon();
static int Create_Attribute_Palette();
static int Create_TL_Palette();
static int Create_TR_Palette();
static int Create_BL_Palette();
static int Create_BR_Palette();
static int Create_Palette_Surround();
static Set_Sample();
static Change_Figure_Font_And_Mode();

long
zipedit__Set_Palettes( self, pane, palette_mode )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register int			      palette_mode;
  {
  register int			      status = zip_success;

  IN(zipedit__Set_Palettes);
  if ( pane )
    {
    if ( (status = zipedit_Prepare_Editing_Control( self, pane )) == zip_ok )
      PaletteMode = palette_mode;
    }
  ZIP_STATUS();
  DEBUGdt(Status,status);
  OUT(zipedit__Set_Palettes);
  return status;
  }

long
zipedit__Expose_Palettes( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  register int			      status = zip_ok;
  register char			     *client_data;
  register struct fontdesc	     *current_font =
					zipview_GetFont( View );
  register zip_type_pane	      container;
  static int			      container_number = 1;
  char				      container_full_name[257];
  register long			      modified;

  IN(zipedit__Expose_Palette);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( pane  &&  pane->zip_pane_edit )
    {
    if ( PalettePanes == NULL )
      {
      status = zipview_Create_Nested_Pane( View,
		     &pane->zip_pane_edit->zip_pane_edit_containing_pane,
		     "ContainingPane", pane, 0 );
      container = pane->zip_pane_edit->zip_pane_edit_containing_pane;
      bcopy( pane, container, sizeof(struct zip_pane) );
      container->zip_pane_source.zip_pane_stream = NULL;
      sprintf( container_full_name, "%s-CONTAINER-%02d",
	       pane->zip_pane_name, container_number );
      container->zip_pane_name = (char *) malloc( strlen( container_full_name ) + 1 );
      strcpy( container->zip_pane_name, container_full_name );
      DEBUG(Name Set);
      if ( (PalettePanes = (struct zip_pane_palettes *)
		calloc( 1, sizeof(struct zip_pane_palettes) +
			 PalettePaneCount * sizeof(zip_type_pane) )) == NULL )
	{
	DEBUG(Unable to Allocate Palette-Panes);
	status = zip_insufficient_pane_space;
/*===	apt_Acknowledge( "Insufficient Space for Palettes" );===*/
	}
      DEBUG(Allocated Palette-Panes);
      PalettePanes->zip_pane_palette_count = PalettePaneCount;
      modified = zip_GetModified( Data );
      if ( status != zip_ok  ||
        (status = Create_Name_Palette(	    self, container, pane, &NamesPane ))	||
        (status = Create_Font_Palette(	    self, container, pane, &FontsPane ))	||
        (status = Create_Figure_Palette(    self, container, pane, &FiguresPane ))	||
        (status = Create_Shade_Palette(	    self, container, pane, &ShadesPane ))	||
        (status = Create_Attribute_Palette( self, container, pane, &AttributesPane ))	||
	(status = Create_TL_Palette(	    self, container, pane, &TLPane ))		||
	(status = Create_TR_Palette(	    self, container, pane, &TRPane ))		||
        (status = Create_BL_Palette(	    self, container, pane, &BLPane ))		||
	(status = Create_BR_Palette(	    self, container, pane, &BRPane ))  )
	{ /*=== handle error ===*/}
      Data->header.dataobject.modified = modified;
      zipview_Remove_Pane( View, container );
      }

    if ( status == zip_ok )
      {
      client_data = pane->zip_pane_client_data;
      pane->zip_pane_client_data = (char *)
		calloc( 1, sizeof(struct ZIP_edit_client_data) );
      PriorClientData	     = client_data;
      PriorXOrigin         = pane->zip_pane_x_origin_percent;
      PriorYOrigin         = pane->zip_pane_y_origin_percent;
      PriorWidth           = pane->zip_pane_width_percent;
      PriorHeight          = pane->zip_pane_height_percent;
      PriorBorderFont      = pane->zip_pane_border_font;
      PriorBorderPattern   = pane->zip_pane_border_pattern;
      PriorBorderThickness = pane->zip_pane_border_thickness;
      EditPane = pane;
      if ( FigurePalette )
	{
        zipedit_Expose_Figure_Palette( self, pane );
	zipview_Display_Pane( View, TRPane );
	zipview_Display_Pane( View, BRPane );
	}
      if ( NamePalette )
	{
        zipview_Display_Pane( View, NamesPane );
	zipview_Display_Pane( View, TLPane );
	zipview_Display_Pane( View, TRPane );
	}
      if ( FontPalette )
	{
        zipedit_Expose_Font_Palette( self, pane );
	zipview_Display_Pane( View, TLPane );
	zipview_Display_Pane( View, TRPane );
	}
      if ( ShadePalette )
	{
        zipedit_Expose_Shade_Palette( self, pane );
	zipview_Display_Pane( View, BLPane );
	zipview_Display_Pane( View, BRPane );
	}
      if ( AttributePalette )
	{
        zipview_Display_Pane( View, AttributesPane );
        zipview_Display_Pane( View, TLPane );
	zipview_Display_Pane( View, BLPane );
	}
      zipedit_Align_Pane( self, pane );
      zipedit_Redisplay_Edit_Pane( self, pane );
      zipview_SetFont( View, current_font ); zipview_FlushGraphics( View );
      }
    PalettesExposed = true;
    }
  ZIP_STATUS();
  OUT(zipedit__Expose_Palettes);
  return status;
  }

long
zipedit__Hide_Palettes( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  register int			      status = zip_ok;

  IN(zipedit__Hide_Palettes);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( pane )
    {
    zipedit_Hide_Font_Palette( self, pane );
    zipedit_Hide_Figure_Palette( self, pane );
    zipedit_Hide_Shade_Palette( self, pane );
    zipview_Remove_Pane( View, NamesPane );
    zipview_Remove_Pane( View, AttributesPane );
    zipview_Remove_Pane( View, TLPane );
    zipview_Remove_Pane( View, TRPane );
    zipview_Remove_Pane( View, BLPane );
    zipview_Remove_Pane( View, BRPane );
    DEBUG(Palettes Removed);
    if ( PriorPane )
      zipview_Pane_Inverted( View, PriorPane ) = false;
    zipview_Set_Pane_Coordinates( View, pane,
	PriorXOrigin, PriorYOrigin, PriorWidth, PriorHeight );
    zipview_Set_Pane_Border( View, pane, NULL,
	PriorBorderPattern, PriorBorderThickness );
    pane->zip_pane_border_font = PriorBorderFont;
    pane->zip_pane_client_data = PriorClientData;
    zipview_Display_Pane( View, pane );
    PalettesExposed = false;
    }
  ZIP_STATUS();
  OUT(zipedit__Hide_Palettes);
  return status;
  }

zipedit__Align_Pane( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
/*===
  char					 *p = NULL;
  zip_type_stream			  stream = zipview_Pane_Stream( View, pane );
  int					  count, x_offset, y_offset, pseudo_x, pseudo_y;
  float					  scale;
===*/
  IN(zipedit__Align_Pane);
/*===
  if ( stream && pane->zip_pane_auxiliary_stream )
    {
    p = zip_Image_Name( Data, zip_Image_Inferior( Data, stream->zip_stream_image_anchor ));
    if ( p )
      {
      count = sscanf( p, "%f:%d:%d:%d:%d", &scale, &x_offset, &y_offset,
                        &pseudo_x, &pseudo_y );
      if ( count == 5 )
        {
        pane->zip_pane_x_offset = x_offset;
        pane->zip_pane_y_offset = y_offset;
        zipview_Set_Pane_Scale( View, pane, scale );
        stream->zip_stream_pseudo_x_offset = pseudo_x;
        stream->zip_stream_pseudo_y_offset = pseudo_y;
/*=== investigate args
        zip_Adjust_Image_Point_Suite( Data, stream->zip_stream_image_anchor, NULL,
                                      pseudo_x, pseudo_y, true );
===!/
	}
      }
    }
===*/
  OUT(zipedit__Align_Pane);
  }

int
zipedit_Redisplay_Edit_Pane( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;
  register int				  pad_width = 100, pad_height = 100,
					  pad_x_center = 50, pad_y_center = 50;

  IN(zipedit_Redisplay_Edit_Pane);
  if ( FigurePalette )	{ pad_width  -= 10;  pad_x_center -= 5; }
  if ( ShadePalette )	{ pad_height -= 10;  pad_y_center -= 5; }
  if ( AttributePalette ){ pad_width  -= 10;  pad_x_center += 5; }
  if ( NamePalette  ||  FontPalette ) { pad_height -= 10;  pad_y_center += 5; }
  zipview_Set_Pane_Coordinates( View, pane, pad_x_center, pad_y_center,
				pad_width, pad_height );
  zipview_Set_Pane_Border( View, pane, NULL, NULL, 1 );
  zipview_Clear_Pane( View, pane );
  if ( BackgroundPane  &&  !zipview_Pane_Removed( View, BackgroundPane ) )
    zipedit_Display_Background_Pane( self, pane );
  zipview_Draw_Pane( View, pane );
  if ( EnclosureExposed )
    Show_Enclosure( self, pane );
  if ( PendingProcessor )
    (*PendingProcessor)( PendingAnchor, pane );
  OUT(zipedit_Redisplay_Edit_Pane);
  return status;
  }

int
zipedit_Handle_Shade_Palette_Hit( self, pane, action, x, y, clicks )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  {
  register long				  status = zip_ok, shade = 0, changed = false;
  register zip_type_figure		  figure, display, 
					  selector, transparent;
  register char				 *ptr;

  IN(zipedit_Handle_Shade_Palette_Hit);
  if ( action == view_LeftDown  &&
       (figure = zipview_Within_Which_Figure( View, x, y )) )
    {
    DEBUGst(Figure-name,figure->zip_figure_name);
    display  = zip_Figure( Data, "ZIP-SHADE-DISPLAY" );
    selector = zip_Figure( Data, "ZIP-SHADE-SELECTOR" );
    transparent = zip_Figure( Data, "ZIP-SHADE-TRANSPARENT" );
    zipview_Clear_Figure(  View, selector, pane );
    zipview_Hide_Figure(  View, transparent, pane );
    if ( ptr = (char *) index( figure->zip_figure_name, '=' ) )
      { DEBUG(Change Shade);
      shade = atoi( ++ptr );
      DEBUGdt(Shade,shade);
      zip_Set_Figure_Shade( Data, display, shade );
      zipview_Draw_Figure(  View, display, pane );
      selector->zip_figure_point.zip_point_x = figure->zip_figure_point.zip_point_x +
	((figure->zip_figure_points->zip_points[0].zip_point_x -
	    figure->zip_figure_point.zip_point_x) / 2);
      selector->zip_figure_point.zip_point_y = figure->zip_figure_point.zip_point_y +
	((figure->zip_figure_points->zip_points[0].zip_point_y -
	    figure->zip_figure_point.zip_point_y) / 2);
      zipview_Draw_Figure(  View, selector, pane );
      EditingPane(pane)->zip_pane_edit->zip_pane_edit_current_shade = shade;
      }
      else
      {
      if ( figure == display  ||  figure == transparent )
	{ DEBUG(Become Transparent);
        zipview_Clear_Figure( View, display, pane );
        zip_Set_Figure_Shade( Data, display, 0 );
        zipview_Draw_Figure(  View, display, pane );
        zipview_Expose_Figure( View, transparent, pane );
        EditingPane(pane)->zip_pane_edit->zip_pane_edit_current_shade = shade = 0; 
	}
      }
    figure = NULL;
    while ( figure = zipedit_Next_Selected_Figure( self, EditingPane(pane), figure ) )
      if ( Change_Shade( self, EditingPane(pane), figure, shade ) )
	changed = true;
    if ( changed )
      {
      Show_Enclosure( self, EditingPane(pane) );
      zipview_Draw_Pane( View, EditingPane(pane) );
      figure = NULL;
      while ( figure = zipedit_Next_Selected_Figure( self, EditingPane(pane), figure ))
        zipedit_Highlight_Figure_Points( self, figure, EditingPane(pane));
      Show_Enclosure( self, EditingPane(pane) );
      }
    }
  OUT(zipedit_Handle_Shade_Palette_Hit);
  return status;
  }

static
Change_Shade( self, pane, figure, shade )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register long				  shade;
  {
  register long				  changed = false;

  IN(Change_Shade);
  zipedit_Normalize_Figure_Points( self, figure, pane );
  if ( figure->zip_figure_mode.zip_figure_mode_shaded )
    zipview_Clear_Figure( View, figure, pane );
  if ( zipobject_Set_Object_Shade(
	Objects(figure->zip_figure_type), figure, shade ) == zip_ok )
    changed = true;
  OUT(Change_Shade);
  return  changed;
  }

int
zipedit_Handle_Figure_Palette_Hit( self, pane, action, x, y, clicks )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register int				  action, x, y, clicks;
  {
  register int				  status = zip_ok;

  IN(zipedit_Handle_Figure_Palette_Hit);
  switch ( action )
    {
    case view_LeftDown:
      Figure_Palette_LBDT( self, pane, x, y, clicks );
      break;
    case view_LeftUp:
    case view_LeftMovement:
    case view_RightDown:
    case view_RightUp:
    case view_RightMovement:
    default: ;
    }
  OUT(zipedit_Handle_Figure_Palette_Hit);
  return status;
  }

static int
Figure_Palette_LBDT( self, icon_pane, x, y, clicks )
  register struct zipedit		 *self;
  register zip_type_pane		  icon_pane;
  register int				  x, y, clicks;
  {
  register int				  status = zip_ok;
  register zip_type_pane		  pane = EditingPane(icon_pane);

  IN(Figure_Palette_LBDT);
  if ( icon_pane->zip_pane_client_data )
    {
    if ( KeyboardProcessor )
      (*KeyboardProcessor)( KeyboardAnchor, pane, NULL, NULL, NULL, NULL, NULL );
    zipedit_Cancel_Enclosure( self, pane );
    zipedit_Hide_Selection_Menu( self );
    if ( CurrentFigure )
      if ( SelectionLevel >= ImageSelection )
      zipedit_Normalize_Image_Points( self, CurrentImage, pane );
      else
      zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
    SelectionLevel = 0;
    zipedit_Set_Pane_Highlight_Icon( self, pane, zip_figure_point_icon );

    pane = icon_pane;
    zipview_Invert_Pane( View, FigurePane );
    zipview_Invert_Pane( View, FigurePane = pane );
    PendingFigureType = EditingType(pane);
    zipview_Set_Pane_Cursor( View, EditingPane(pane), EditingIcon(pane), CursorFontName );
    status = zipobject_Show_Object_Properties( Objects(PendingFigureType), EditingPane(pane), NULL );
    }
  zipview_FlushGraphics( View );
  OUT(Figure_Palette_LBDT);
  return status;
  }

static int
Create_Name_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  {
  register int				  status = zip_ok;
  zip_type_stream			  stream;
  static char				  source[] =
"{ZIP_NAME_STREAM\n\
Fandysans10b\n\
*A;-590,75\nT Stream:\n\
*A;-300,75\nNZIP_current_stream_name\nFandysans10\n\
*A;-590,0\nT Image:\n\
*A;-300,0\nNZIP_current_image_name\nFandysans10\n\
*A;-590,-75\nT Figure:\n\
*A;-300,-75\nNZIP_current_figure_name\nFandysans10\n\
}";

  IN(Create_Name_Palette);
  Create_Palette_Surround( self, containing_pane, palette, "NAME", 30,5,40,10 );
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipview_Remove_Pane( View, *palette );
  OUT(Create_Name_Palette);
  return status;
  }

static int
Create_Font_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  {
  register int				  status = zip_ok;
  static char				  family[] =
"{ZIP_FONT_FAMILY\n*A;0,0\nNfont_catalog_family\nMMC\nFandysans10\nTSerif\n}";
  static char					  height[] =
"{ZIP_FONT_HEIGHT\n*A;0,0\nNfont_catalog_height\nMMC\nFandysans10\nT12\n}";
  static char					  italic[] =
"{ZIP_FONT_ITALIC\n*A;0,0\nNfont_catalog_italic\nMMC\nFandysans10\nTItalic\n}";
  static char					  bold[] =
"{ZIP_FONT_BOLD\n*A;0,0\nNfont_catalog_bold\nMMC\nFandysans10\nTBold\n}";
  static char					  sample[] =
"{ZIP_FONT_SAMPLE\n*A;0,0\nNfont_catalog_sample\nMMC\nFandy12\nTSample\n*C;0,50\n>0,-50\n*C;-100,0\n>100,0\n}";

  IN(Create_Font_Palette);
  if (
    (status = Create_Palette_Surround( self, containing_pane, palette,  
		"FONT-SURROUND", 70,5,40,10 )) != zip_ok  ||
    (status = Create_Font_Icon( self, containing_pane, pane, &FontFamilyPane,
		family, '2', 55, 3, 6, 5, "FONT-FAMILY" )) != zip_ok  || 
    (status = Create_Font_Icon( self, containing_pane, pane, &FontHeightPane,
		height, '2', 65, 3, 4, 5, "FONT-HEIGHT" )) != zip_ok  || 
    (status = Create_Font_Icon( self, containing_pane, pane, &FontItalicPane,
		italic, '2', 55, 7, 6, 5, "FONT-ITALIC" )) != zip_ok  || 
    (status = Create_Font_Icon( self, containing_pane, pane, &FontBoldPane,
		bold,   '2', 65, 7, 6, 5, "FONT-BOLD" )) != zip_ok  || 
    (status = Create_Font_Icon( self, containing_pane, pane, &FontSamplePane,
		sample, '2', 80, 5, 19, 8, "FONT-SAMPLE" )) != zip_ok  || 
    (status = zipedit_Hide_Font_Palette( self, pane )) != zip_ok 
    )
    {}
    else
    {
    zip_Define_Font( Data, "andy12", &pane->zip_pane_current_font );
    pane->zip_pane_current_mode.zip_figure_mode_middle = on;
    pane->zip_pane_current_mode.zip_figure_mode_center = on;
    }
  OUT(Create_Font_Palette);
  return status;
  }

static int
Create_Font_Icon( self, containing_pane, pane, palette,
		  source, cursor, x, y, width, height, name )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane; 
  register zip_type_pane		  pane; 
  register zip_type_pane		 *palette;
  register char				 *source;
  register char				  cursor;
  register int				  x, y, width, height;
  {
  register int				  status = zip_ok;
  zip_type_stream			  stream;

  IN(Create_Font_Icon);
  if (
    (status = zipview_Create_Nested_Pane( View, palette, name, containing_pane, zip_default )) != zip_ok  ||
    (status = zipview_Set_Pane_Coordinates( View, *palette, x, y,
		 width, height )) != zip_ok  ||
    (status = zipview_Set_Pane_Cursor( View, *palette, cursor, NULL )) != zip_ok  ||
    (status = zip_Create_Stream( Data, &stream, NULL, zip_default )) != zip_ok  ||
    (status = zip_Set_Stream_Source( Data, stream, source )) != zip_ok  ||
    (status = zipview_Set_Pane_Stream( View, *palette, stream )) != zip_ok
    )
    {DEBUG(ERROR);}
    else
    {
    (*palette)->zip_pane_client_data = (char *)
		 calloc( 1, sizeof(struct ZIP_edit_client_data) );
    EditingIcon(*palette) = cursor;
    EditingPane(*palette) = pane;
    }
  OUT(Create_Font_Icon);
  return status;
  }

zipedit_Expose_Font_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit_Expose_Font_Palette);
  zipview_Display_Pane( View, FontsPane );
  zipview_Display_Pane( View, FontFamilyPane );
  zipview_Display_Pane( View, FontHeightPane );
  zipview_Display_Pane( View, FontItalicPane );
  zipview_Display_Pane( View, FontBoldPane );
  zipview_Display_Pane( View, FontSamplePane );
  OUT(zipedit_Expose_Font_Palette);
  return  zip_ok;
  }

zipedit_Hide_Font_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit_Hide_Font_Palette);
  zipview_Remove_Pane( View, FontsPane );
  zipview_Remove_Pane( View, FontFamilyPane );
  zipview_Remove_Pane( View, FontHeightPane );
  zipview_Remove_Pane( View, FontItalicPane );
  zipview_Remove_Pane( View, FontBoldPane );
  zipview_Remove_Pane( View, FontSamplePane );
  OUT(zipedit_Hide_Font_Palette);
  return  zip_ok;
  }

static int
Create_Shade_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  {
  register int				  status = zip_ok;
  zip_type_stream			  stream;
  static char				  source[] =
"{\n\
*J;-1100,0\n\
NZIP-SHADE-SELECTOR\n\
>77,0\n\
*G;-1200,100\n\
>-1000,-100\n\
*G;-1175,75\n\
NZIP-SHADE-DISPLAY\n\
>-1025,-75\n\
*A;-1100,0\n\
NZIP-SHADE-TRANSPARENT\n\
Fzipicn20\n\
T2\n\
*G;-900,50\n\
#White\n\
NZIP-SHADE=01\n\
G1\n\
>-800,-50\n\
*G;-700,50\n\
NZIP-SHADE=10\n\
G10\n\
>-600,-50\n\
*G;-500,50\n\
NZIP-SHADE=20\n\
G20\n\
>-400,-50\n\
*G;-300,50\n\
NZIP-SHADE=30\n\
G30\n\
>-200,-50\n\
*G;-100,50\n\
NZIP-SHADE=40\n\
G40\n\
>0,-50\n\
*G;100,50\n\
NZIP-SHADE=50\n\
G50\n\
>200,-50\n\
*G;300,50\n\
NZIP-SHADE=60\n\
G60\n\
>400,-50\n\
*G;500,50\n\
NZIP-SHADE=70\n\
G70\n\
>600,-50\n\
*G;700,50\n\
NZIP-SHADE=80\n\
G80\n\
>800,-50\n\
*G;900,50\n\
NZIP-SHADE=100\n\
#Black\n\
G100\n\
>1000,-50\n\
#*G;1100,100\n\
#NZIP-SHADE=100\n\
#G100\n\
#>1200,-100\n\
}";


  IN(Create_Shade_Palette);
  Create_Palette_Surround( self, containing_pane, palette, "SHADE-SURROUND", 50, 95, 80, 10 );
  (*palette)->zip_pane_client_data = (char *)
	 calloc( 1, sizeof(struct ZIP_edit_client_data) );
  EditingPane(*palette) = pane;
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipedit_Hide_Shade_Palette( self, pane );
  OUT(Create_Shade_Palette);
  return status;
  }

zipedit_Expose_Shade_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit_Expose_Shade_Palette);
  zipview_Display_Pane( View, ShadesPane );
  OUT(zipedit_Expose_Shade_Palette);
  }

zipedit_Hide_Shade_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit_Hide_Shade_Palette);
  zipview_Remove_Pane( View, ShadesPane );
  OUT(zipedit_Hide_Shade_Palette);
  }

#define FigureIconPane(i)   PalettePanes->zip_pane_palette_vector[20+i]
static int
Create_Figure_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  {
  register long				  status = zip_ok, i;
  register long				  icon_count = 0,
					  serial = 0;

  IN(Create_Figure_Palette);
  Create_Palette_Surround( self, containing_pane, palette, "FIGURE-SURROUND", 95, 50, 10, 80 );
  for ( i = 1; Objects(i); i++ )
    {
    DEBUGct(Icon,zipobject_Object_Icon( Objects(i) ));
    DEBUGst(Icon-Font-Name,zipobject_Object_Icon_Font_Name( Objects(i) ));
    DEBUGct(Cursor,zipobject_Object_Icon_Cursor( Objects(i) ));
    DEBUGst(Cursor-Font-Name,zipobject_Object_Icon_Cursor_Font_Name( Objects(i) ));
    DEBUGct(DataStream-Code,zipobject_Object_Datastream_Code( Objects(i) ));
    if ( zipobject_Object_Icon( Objects(i) ) )
      icon_count++;
      else {DEBUGdt(No Icon,i);}
    }
  DEBUGdt(Icon-count,icon_count);
  for ( i = 1; Objects(i); i++ )
    {
    if ( zipobject_Object_Icon( Objects(i) ) )
      Create_Figure_Icon( self, containing_pane, pane,
	&FigureIconPane(i),
	zipobject_Object_Datastream_Code( Objects(i) ) - '@',
	zipobject_Object_Icon( Objects(i) ),
	zipobject_Object_Icon_Font_Name( Objects(i) ),
	zipobject_Object_Icon_Cursor( Objects(i) ),
	zipobject_Object_Icon_Cursor_Font_Name( Objects(i) ),
	++serial, icon_count );
    }
  DEBUGdt(Serial,serial);
  zipedit_Hide_Figure_Palette( self, pane );
  OUT(Create_Figure_Palette);
  return status;
  }

zipedit_Expose_Figure_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  i;

  IN(zipedit_Expose_Figure_Palette);
  zipview_Display_Pane( View, FiguresPane );
  for ( i = 1; Objects(i); i++ )
    if ( FigureIconPane(i) )
      zipview_Display_Pane( View, FigureIconPane(i) );
  OUT(zipedit_Expose_Figure_Palette);
  }


zipedit_Hide_Figure_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  i;

  IN(zipedit_Hide_Figure_Palette);
  zipview_Remove_Pane( View, FiguresPane );
  for ( i = 1; Objects(i); i++ )
    if ( FigureIconPane(i) )
      zipview_Remove_Pane( View, FigureIconPane(i) );
  OUT(zipedit_Hide_Figure_Palette);
  }

static int
Create_Figure_Icon( self, containing_pane, editing_pane, palette, type,
		    icon, icon_font_name, cursor, cursor_font_name,
		    serial, count )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane;
  register zip_type_pane		  editing_pane;
  register zip_type_pane		 *palette;
  register long				  type;
  register char				  icon;
  register char				 *icon_font_name;
  register char				  cursor;
  register char				 *cursor_font_name;
  register long				  serial, count;
  {
  register long				  status = zip_ok,
					  percentage = 80/count;
  zip_type_stream			  stream;
  char					  source[257];

  IN(Create_Figure_Icon);
  sprintf( source, "Figure_Icon_%c", cursor );
  zipview_Create_Nested_Pane( View, palette, source, containing_pane, zip_default );
  zipview_Set_Pane_Coordinates( View, *palette, 95, serial * percentage + 8, 8, percentage );
  zipview_Set_Pane_Cursor( View, *palette, cursor, cursor_font_name );
  (*palette)->zip_pane_client_data = (char *)
	 calloc( 1, sizeof(struct ZIP_edit_client_data) );
  EditingType(*palette) = type;
  EditingIcon(*palette) = cursor;
  EditingPane(*palette) = editing_pane;
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  sprintf( source, "*A;0,0\nT%c\nF%s\n", icon, icon_font_name );
  zip_Set_Stream_Source( Data, stream, source ); 
  zipview_Set_Pane_Stream( View, *palette, stream );
  OUT(Create_Figure_Icon);
  return status;
  }

static int
Create_Attribute_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane			  containing_pane;
  register zip_type_pane			  pane;
  register zip_type_pane			 *palette;
  {
  register int					  status = zip_ok;
  zip_type_stream				  stream;
  static char					  source[] =
"{ZIP_ATTRIBUTES\n\
Fandysans10b\n\
##*G;-120,1000\n##>120,-1000\n\
\n\
*C;-100,1000\n>-100,1000\n\
*A;0,900\nTX-Point\nMMC\n\
*A;0,850\nNZIP_current_x_point_name\nFandysans10\nMMC\n\
\n\
*A;0,700\nTY-Point\nMMC\n\
*A;0,650\nNZIP_current_y_point_name\nFandysans10\nMMC\n\
\n\
*A;0,500\nTFont\nMMC\n\
*A;0,450\nNZIP_current_font_name\nFandysans10\nMMC\n\
*C;100,-1000\n>100,-1000\n\
}";

  Create_Palette_Surround( self, containing_pane, palette, "ATTRIBUTE-SURROUND", 5,50,10,80 );
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipview_Remove_Pane( View, AttributesPane );
  return status;
  }

static int
Create_TL_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  {
  register int				  status = zip_ok;
  zip_type_stream			  stream;
  static char				  source[] =
"{\n\
Fandysans16\n\
##*1;0,0\n##T*\n\
}";

  Create_Palette_Surround( self, containing_pane, palette, "TL-SURROUND", 5,5,10,10 );
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipview_Remove_Pane( View, TLPane );
  return status;
  }

zipedit_Expose_TL_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Display_Pane( View, TLPane );
  }

zipedit_Hide_TL_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Remove_Pane( View, TLPane );
  }

static int
Create_TR_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane		  containing_pane;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  {
  register int				  status = zip_ok;
  zip_type_stream			  stream;
  static char				  source[] =
"{\n\
Fandysans10b\n\
##*1;0,0\n##T*\n\
}";

  Create_Palette_Surround( self, containing_pane, palette, "TR-SURROUND", 95, 5,10,10 ); 
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipview_Remove_Pane( View, TRPane );
  return status;
  }

zipedit_Expose_TR_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Display_Pane( View, TRPane );
  }

zipedit_Hide_TR_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Remove_Pane( View, TRPane );
  }

static int
Create_BL_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane			  containing_pane;
  register zip_type_pane			  pane;
  register zip_type_pane			 *palette;
  {
  register int					  status = zip_ok;
  zip_type_stream				  stream;
  static char					  source[] =
"{\n\
Fandysans10b\n\
##*1;0,0\n##T*\n\
}";

  Create_Palette_Surround( self, containing_pane, palette, "BL-SURROUND", 5,95, 10,10 );
  zip_Create_Stream( Data, &stream, NULL, zip_default ); 
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipview_Remove_Pane( View, BLPane );
  return status;
  }

zipedit_Expose_BL_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Display_Pane( View, BLPane );
  }

zipedit_Hide_BL_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Remove_Pane( View, BLPane );
  }

static int
Create_BR_Palette( self, containing_pane, pane, palette )
  register struct zipedit		 *self;
  register zip_type_pane			  containing_pane;
  register zip_type_pane			  pane;
  register zip_type_pane			 *palette;
  {
  register int					  status = zip_ok;
  zip_type_stream				  stream;
  static char					  source[] =
"{\n\
Fandysans10b\n\
##*1;0,0\n##T*\n\
}";

  Create_Palette_Surround( self, containing_pane, palette, "BR-SURROUND", 95,95,10,10 );
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  zipview_Remove_Pane( View, BRPane );
  return status;
  }

zipedit_Expose_BR_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Display_Pane( View, BRPane );
  }

zipedit_Hide_BR_Palette( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_Remove_Pane( View, BRPane );
  }

static int
Create_Palette_Surround( self, pane, palette, name,
			    x_origin, y_origin, width, height )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pane		 *palette;
  register int				  x_origin, y_origin, width, height;
  {
  register int				  status = zip_ok;
  zip_type_stream			  stream;
  static char				  source[] =
"{\n\
Fandysans10b\n\
##*1;0,0\n\
##T*\n\
}";

  IN(Create_Palette_Surround);
  zipview_Create_Nested_Pane( View, palette, name, pane, zip_default );
  zipview_Set_Pane_Coordinates( View, *palette, x_origin, y_origin, width, height );
  zipview_Set_Pane_Cursor( View, *palette, 'A', CursorFontName );
  zipview_Set_Pane_Border( View, *palette, 0, 0, 2 );
  (*palette)->zip_pane_client_data = (char *)
	calloc( 1, sizeof(struct ZIP_edit_client_data) );
  EditingType( *palette ) = NULL;
  EditingIcon( *palette ) = 'A';
  zip_Create_Stream( Data, &stream, NULL, zip_default );
  zip_Set_Stream_Source( Data, stream, source );
  zipview_Set_Pane_Stream( View, *palette, stream );
  OUT(Create_Palette_Surround);
  return status;
  }

int
zipedit_Handle_Font_Family_Selection( self, pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register long			      status = zip_success;
  register zip_type_figure	      figure;

  IN(zipedit_Handle_Font_Family_Selection);
  if ( action == view_LeftDown )
    {
    zipview_Clear_Pane( View, pane );
    figure = zip_Figure( Data, "font_catalog_family" );
    if ( FontFamily = ! FontFamily )
	{
	zip_Set_Figure_Text( Data, figure, "Sans" );
	}
	else
	{
	zip_Set_Figure_Text( Data, figure, "Serif" );
	}
    zipview_Draw_Figure( View, figure, pane );
    }
  OUT(zipedit_Handle_Font_Family_Selection);
  return status;
  }

int
zipedit_Handle_Font_Height_Selection( self, pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register long			      status = zip_success;
  register zip_type_figure	      figure;
  char				      msg[257];

  IN(zipedit_Handle_Font_Height_Selection);
  if ( action == view_LeftDown  ||  action == view_RightDown )
    {
    zipview_Clear_Pane( View, pane );
    figure = zip_Figure( Data, "font_catalog_height" );
    if ( action == view_LeftDown )
      {
      if ( FontHeight < 144 )
	FontHeight += 2;
	else
	FontHeight = 2;
        sprintf( msg, "%2d", FontHeight );
      zip_Set_Figure_Text( Data, figure, msg);
      }
    else
    if ( action == view_RightDown )
      {
      if ( FontHeight > 2 )
	FontHeight -= 2;
	else
	FontHeight = 144;
        sprintf( msg, "%2d", FontHeight );
      zip_Set_Figure_Text( Data, figure, msg);
      }
    zipview_Draw_Figure( View, figure, pane );
    }
  OUT(zipedit_Handle_Font_Height_Selection);
  return status;
  }

int
zipedit_Handle_Font_Italic_Selection( self, pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register int			      action, x, y, clicks;
  {
  register int			      status = zip_success;
  register zip_type_figure	      figure;

  IN(zipedit_Handle_Font_Italic_Selection);
  if ( action == (int)view_LeftDown )
    {
    zipview_Clear_Pane( View, pane );
    figure = zip_Figure( Data, "font_catalog_italic" );
    FontItalic = ! FontItalic;
    zip_Set_Figure_Font( Data, figure, (FontItalic) ?
			 "andysans10b" : "andysans10" );
    zipview_Draw_Figure( View, figure, pane );
    }
  OUT(zipedit_Handle_Font_Italic_Selection);
  return status;
  }

int
zipedit_Handle_Font_Bold_Selection( self, pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register long			      status = zip_success;
  register zip_type_figure	      figure;

  IN(zipedit_Handle_Font_Bold_Selection);
  if ( action == view_LeftDown )
    {
    zipview_Clear_Pane( View, pane );
    figure = zip_Figure( Data, "font_catalog_bold" );
    FontBold = ! FontBold;
    zip_Set_Figure_Font( Data, figure, (FontBold) ?
			 "andysans10b" : "andysans10" );
    zipview_Draw_Figure( View, figure, pane );
    }
  OUT(zipedit_Handle_Font_Bold_Selection);
  return status;
  }

int
zipedit_Handle_Font_Sample_Selection( self, pane, action, x, y, clicks )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register long			      status = zip_ok, reshow = false;

  IN(zipedit_Handle_Font_Sample_Selection);
  if ( action == view_LeftDown )
    {
    PriorX = x;  PriorY = y;
    Set_Sample( self, pane, false );
    }
  else
    if ( action == view_LeftMovement )
      {
      zipview_Set_Pane_Cursor( View, pane, '@', CursorFontName ); /* Make Cursor disappear */
      if ( x < (PriorX - 5) )
	{
	switch ( FontHorizontal )
	  {
	  case zip_left:    FontHorizontal = zip_center;  break;
	  case zip_center:  FontHorizontal = zip_right;   break;
	  case zip_right:   break;
	  default:	    FontHorizontal = zip_left;    break;
	  }
	PriorX = x;  PriorY = y; reshow = true;
	}
      else  if ( x > (PriorX + 5) )
	{
	switch ( FontHorizontal )
	  {
	  case zip_right:   FontHorizontal = zip_center;  break;
	  case zip_center:  FontHorizontal = zip_left;    break;
	  case zip_left:    break;
	  default:	    FontHorizontal = zip_left;    break;;
	  }
	PriorX = x;  PriorY = y; reshow = true;
	}
      else  if ( y < (PriorY - 5) )
	{
	switch ( FontVertical )
	  {
	  case zip_top :     FontVertical = zip_middle;    break;
	  case zip_middle:   FontVertical = zip_baseline;  break;
	  case zip_baseline: FontVertical = zip_bottom;    break;
	  case zip_bottom:   break;
	  default:	     FontVertical = zip_bottom;    break;
	  }
	PriorX = x;  PriorY = y; reshow = true;
	}
      else  if ( y > (PriorY + 5) )
	{
	switch ( FontVertical )
	  {
	  case zip_bottom:   FontVertical = zip_baseline;  break;
	  case zip_baseline: FontVertical = zip_middle;    break;
	  case zip_middle:   FontVertical = zip_top;       break;
	  case zip_top:      break;
	  default:	     FontVertical = zip_bottom;    break;
	  }
	PriorX = x;  PriorY = y; reshow = true;
	}
      }
  if (  action == view_LeftUp  ||
       ((action == view_LeftMovement)  &&  reshow ) )
    Set_Sample( self, pane, action == view_LeftUp );
  if ( action == view_LeftUp )
    zipview_Set_Pane_Cursor( View, pane, '2', CursorFontName );
  OUT(zipedit_Handle_Font_Sample_Selection);
  return status;
  }

static
Set_Sample( self, pane, draw_pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register boolean		      draw_pane;
  {
  register zip_type_figure	      figure;
  char				      msg[257];
  register long			      changed = false;

  IN(Set_Sample);
  figure = zip_Figure( Data, "font_catalog_sample" );
  sprintf( msg, "%s%d%s%s",
	 (FontFamily) ? "andysans" : "andy",
	 FontHeight,
	 (FontBold) ? "b" : "",
	 (FontItalic) ? "i" : "" );
  zip_Set_Figure_Font( Data, figure, msg );
  zip_Set_Figure_Mode( Data, figure, FontHorizontal | FontVertical );
  EditingPane(pane)->zip_pane_current_font = figure->zip_figure_font;
  EditingPane(pane)->zip_pane_current_mode = figure->zip_figure_mode;
  zipview_Display_Pane( View, pane );
  figure = NULL;
  while ( figure = zipedit_Next_Selected_Figure( self, EditingPane(pane), figure ) )
    if ( Change_Figure_Font_And_Mode( self, EditingPane(pane), figure,
		EditingPane(pane)->zip_pane_current_font, FontHorizontal | FontVertical ) )
      changed = true;
  if ( changed  &&  draw_pane)
    {
    Show_Enclosure( self, EditingPane(pane) );
    zipview_Draw_Pane( View, EditingPane(pane) );
    Show_Enclosure( self, EditingPane(pane) );
    }
  OUT(Set_Sample);
  }

static
Change_Figure_Font_And_Mode( self, pane, figure, font, mode )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register long				  font, mode;
  {
  register boolean			  changed = false;

  IN(Change_Figure_Font_And_Mode);
  zipedit_Normalize_Figure_Points( self, figure, pane );
  if ( figure->zip_figure_font )
    zipview_Clear_Figure( View, figure, pane );
  if ( zipobject_Set_Object_Font(
	Objects(figure->zip_figure_type), figure, font ) == zip_ok )
    {
    changed = true;
/*===ADD Set_Object_Mode===*/
if ( figure->zip_figure_type == zip_caption_figure )
zip_Set_Figure_Mode( Data, figure, mode );
    }
  OUT(Change_Figure_Font_And_Mode);
  return  changed;
  }
