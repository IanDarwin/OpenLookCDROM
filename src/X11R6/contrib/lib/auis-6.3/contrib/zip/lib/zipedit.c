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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipedit.c,v 1.6 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipedit.c	Zip EditView-object				      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipedit.c,v $
 * Revision 1.6  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.4.1.1  1993/02/02  06:55:04  rr2b
 * new R6tape branch
 *
 * Revision 1.4  1993/01/08  16:36:06  rr2b
 * cutting down on duplicate global symbols
 *
 * Revision 1.3  1992/12/15  21:57:55  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.16  1991/09/12  16:42:12  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.15  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.14  1990/08/21  14:20:55  sg08
 * Added Show_Enclosure in Update to improve enclosure behavior
 *
 * Revision 2.13  89/10/07  22:56:59  zs01
 * Changed some chained asignments in InitializeObjects into single assignments to get
 * code to pass through MIPS C compiler.
 * 
 * Revision 2.12  89/09/11  08:16:22  ghoti
 * fix enumeration type clashes - specifically those dealing with mouse actions
 * 
 * Revision 2.11  89/08/30  16:24:49  sg08
 * no change.
 * 
 * Revision 2.10  89/07/24  16:20:06  tom
 * Fix core-dump on pending Caption figure.
 * 
 * Revision 2.9  89/05/26  19:33:18  tom
 * Suppress Hide-Palette - till Form inset used;
 * Deal with WM vs X Background Lightening curiosity.
 * 
 * Revision 2.8  89/05/07  18:43:23  tom
 * Prevent loop when Background show.
 * 
 * Revision 2.7  89/05/01  22:13:59  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.6  89/02/17  18:07:20  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.5  89/02/08  16:49:18  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  19:03:01  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/10/12  18:39:36  tom
 * Fix enum-clashes warning msg.
 * 
 * Revision 2.2  88/10/11  20:40:23  tom
 * Additional Background-pane facilities.
 * 
 * Revision 2.1  88/09/27  18:11:58  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:28:57  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:43:41  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip EditView-object

MODULE	zipedit.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Zip EditView-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
   Such curiosities need be resolved prior to Project Completion...

HISTORY
  03/31/88	Created for ATK (TCP)
  10/12/88	Fixed enumeration clash warning (TCP)
  05/01/89	Use symbolic font-names (TCP)
  05/07/89	Restore fix to prevent Background loop (TCP)
  05/26/89	Suppress Hide-Palette - till Form inset used (TCP)
		Deal with WM vs X Background Lightening curiosity
  07/24/89	Terminate-Editing: reset Processors to NULL (TCP)
  10/07/89      Changed chained assignment in InitializeObject to single
                assignment to satisfy MIPS compiler. (zs01)
   08/14/90	Added Show_Enclosure in Update to improve enclosure behavior (SCG)

END-SPECIFICATION  ************************************************************/

#include "graphic.ih"
#include "view.ih"
#include "proctbl.ih"
#include "menulist.ih"
#include "keymap.ih"
#include "keystate.ih"
#include "im.ih"
#include "cursor.ih"
#include "bind.ih"
#include "rect.h"
#include "message.ih"
#include "filetype.ih"
#include "environ.ih"
#include <ctype.h>
#include "fontdesc.ih"
#include "raster.ih"
#include "rasterv.ih"
#include "zip.ih"
#include "zipv.ih"
#include "zipobj.ih"
#include "zipedit.h"
#include "zipedit.eh"

static boolean debug=FALSE;
static struct menulist		     *class_menulist;
static struct keymap		     *class_keymap;

#define  menu_default		      (1<<0)
#define  menu_app		      (1<<1)
#define  menu_grid_expose	      (1<<10)
#define  menu_grid_hide		      (1<<11)
#define  menu_grid_visible	      (1<<12)
#define  menu_grid_invisible	      (1<<13)
#define  menu_object_absolute	      (1<<14)
#define  menu_object_relative	      (1<<15)
#define  menu_coordinates_expose      (1<<16)
#define  menu_coordinates_hide	      (1<<17)
#define  menu_palettes_expose	      (1<<18)
#define  menu_palettes_hide	      (1<<19)
#define  menu_selection		      (1<<20)
#define  menu_background	      (1<<21)
#define  menu_background_hide	      (1<<22)
#define  menu_background_expose	      (1<<23)
#define  menu_pan_alone		      (1<<24)
#define  menu_pan_together	      (1<<25)
#define  menu_stream_expose	      (1<<26)
#define  menu_stream_hide	      (1<<27)
#define  menu_background_select	      (1<<28)
#define  menu_background_unselect     (1<<29)
#define  menu_fit		      (1<<30)

#define  pending_redisplay	      (1<<1)
#define  pending_redraw		      (1<<2)
#define  pending_palettes	      (1<<3)
#define  pending_delete		      (1<<4)
#define  pending_grid		      (1<<5)
#define  pending_double_grid	      (1<<6)
#define  pending_halve_grid	      (1<<7)
#define  pending_coordinates	      (1<<8)

#define  Pending_redisplay	      Action & pending_redisplay
#define  Set_pending_redisplay	      Action |= pending_redisplay
#define  Reset_pending_redisplay      Action ^= pending_redisplay

#define  Pending_redraw		      Action & pending_redraw
#define  Set_pending_redraw	      Action |= pending_redraw
#define  Reset_pending_redraw	      Action ^= pending_redraw

#define  Pending_delete	    	      Action & pending_delete
#define  Set_pending_delete	      Action |= pending_delete
#define  Reset_pending_delete	      Action ^= pending_delete

#define  Pending_coordinates          Action & pending_coordinates
#define  Set_pending_coordinates      Action |= pending_coordinates
#define  Reset_pending_coordinates    Action ^= pending_coordinates

#define  Pending_grid		      Action & pending_grid
#define  Set_pending_grid	      Action |= pending_grid
#define  Reset_pending_grid	      Action ^= pending_grid
#define  Pending_double_grid	      Action & pending_double_grid
#define  Set_pending_double_grid      Action |= pending_double_grid
#define  Reset_pending_double_grid    Action ^= pending_double_grid
#define  Pending_halve_grid	      Action & pending_halve_grid
#define  Set_pending_halve_grid       Action |= pending_halve_grid
#define  Reset_pending_halve_grid     Action ^= pending_halve_grid

#define  Pending_palettes	      Action & pending_palettes
#define  Set_pending_palettes	      Action |= pending_palettes
#define  Reset_pending_palettes	      Action ^= pending_palettes

static void Accept_Character();
static Build_Menu();
static Lighten_Background();
static void Accept_Character();
static Pending_Delete();
static Pending_Palettes();
static Pending_Coordinates();
static Pending_Grid();
static Pending_Double_Grid();
static Pending_Redisplay();
static Pending_Redraw();
static Pending_Halve_Grid();

boolean 
zipedit__InitializeClass( classID )
  register struct classheader	     *classID;
  {
  register struct proctable_Entry    *proc;
  char				      string[2];

/*===debug=1;===*/
  /*IN(zipedit__InitializeClass );*/
  class_keymap = keymap_New();
  proc = proctable_DefineProc( "self-insert", Accept_Character,
				&zipedit_classinfo, NULL, "Enter Character" );
  string[1] = 0;
  for ( *string = ' '; *string < 127; (*string)++ )
    keymap_BindToKey( class_keymap, string, proc, *string );
  *string = '\010';  keymap_BindToKey( class_keymap, string, proc, *string );
  *string = '\177';  keymap_BindToKey( class_keymap, string, proc, *string );
  *string = '\012';  keymap_BindToKey( class_keymap, string, proc, *string );
  *string = '\015';  keymap_BindToKey( class_keymap, string, proc, *string );
  class_menulist = menulist_New();
  Build_Menu();
  OUT(zipedit__InitializeClass );
  return TRUE;
  }


boolean 
zipedit__InitializeObject( classID, self)
  register struct classheader	      *classID;
  register struct zipedit	      *self;
  {
  IN(zipedit__InitializeObject );
  Action = (long)view_NoMouseEvent;
  Data = NULL;
  View = NULL;
  KeyboardProcessor = NULL;
  PendingProcessor = NULL;
  KeyboardAnchor = PendingAnchor = NULL;
  IconFont = PointsFont = DotsFont = NULL;
  EnclosedFigures = NULL;
  DuplicateSelection = false;
  GridExposed = false;
  CoordinatesExposed = false;
  PalettesExposed = false;
  BackgroundExposed = false;
  BackgroundSelected = false;
  BackgroundLightened = false;
  EnclosureExposed = false;
  ForegroundPanning = false;
  PriorX = PriorY = 0;
  FontFamily = 0;
  FontHeight = 12;
  FontBold = FontItalic = 0;
  FontVertical = zip_middle;
  FontHorizontal = zip_center;
  OUT(zipedit__InitializeObject );
  return TRUE;
  }

void 
zipedit__FinalizeObject( classID, self )
  register struct classheader	      *classID;
  register struct zipedit	      *self;
  {
  IN(zipedit__FinalizeObject );
  DEBUGst( Pane-name, Pane->zip_pane_name );
/*===*/
  if ( Menu )	    menulist_Destroy( Menu );
  if ( KeyState )   keystate_Destroy( KeyState );
  OUT(zipedit__FinalizeObject );
  }

void 
zipedit__Update( self )
  register struct zipedit	   *self;
  {
  IN(zipedit__Update);
  DEBUGst( Pane-name, Pane->zip_pane_name );
  if ( Pending_delete )		    Pending_Delete( self, Pane );
  if ( Pending_coordinates )	    Pending_Coordinates( self, Pane );
  if ( Pending_grid )		    Pending_Grid( self, Pane );
  if ( Pending_double_grid )	    Pending_Double_Grid( self, Pane );
  if ( Pending_halve_grid )	    Pending_Halve_Grid( self, Pane );
  if ( Pending_palettes )	    Pending_Palettes( self, Pane );
  if ( Pending_redisplay )    	    Pending_Redisplay( self, Pane );
  if ( Pending_redraw )    	    Pending_Redraw( self, Pane );
  if ( EnclosureExposed )
    Show_Enclosure( self, Pane );
  OUT(zipedit__Update);
  }

long
zipedit__Set_Data_Object( self, data_object )
  register struct zipedit	      *self;
  register struct zip	    	      *data_object;
  {
  IN(zipedit__Set_Data_Object);
  Data = data_object;
  OUT(zipedit__Set_Data_Object);
  return zip_ok;
  }

long
zipedit__Set_View_Object( self, view_object )
  register struct zipedit	      *self;
  register struct zipview    	      *view_object;
  {
  IN(zipedit__Set_View_Object);
  View = view_object;
  KeyState = keystate_Create( self, class_keymap );
  KeyState->next = NULL;
  zipview_PostKeyState( View, KeyState );
  Menu = menulist_DuplicateML( class_menulist, View );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) | menu_default |
	menu_grid_expose | menu_coordinates_expose | menu_stream_expose |
	menu_palettes_hide | menu_background_select | menu_fit |
	((ObjectWidth) ? menu_object_relative : menu_object_absolute) ) );
  OUT(zipedit__Set_View_Object);
  return zip_ok;
  }

void
zipedit__Set_Debug( self, state )
  register struct zipedit	      *self;
  register char			       state;
  {
  IN(zipedit__Set_Debug);
  debug = state;
  OUT(zipedit__Set_Debug);
  }

/*===
static void
Group_Command( self )
  register struct zipedit	     *self;
  {
  register zip_type_figure	      figure = NULL, peer_figure = NULL;
  zip_type_image		      image;
  char				      name[512];
  static long			      name_serial = 1;
  register zip_type_pane	      pane = Pane;

  IN(Group_Command);
  sprintf( name, "ZIP-GROUP-%d", name_serial++ );
  if ( CurrentImage )
    zip_Create_Inferior_Image( Data, &image, name, CurrentStream, CurrentImage );
  image->zip_image_attributes.zip_image_attribute_group = true;
  while ( figure = zipedit_Next_Selected_Figure( self, Pane, figure ) )
    {
    DEBUGst(Grouping Figure-name,figure->zip_figure_name );
    zip_Unhook_Figure( Data, figure );
    if ( peer_figure )
      zip_Hook_Figure( Data, figure, peer_figure );
      else
      {
      image->zip_image_figure_anchor = figure;
      figure->zip_figure_image = image;
      figure->zip_figure_state.zip_figure_state_unhooked = false;
      }
    peer_figure = figure;
    }
  OUT(Group_Command);
  }

static void
Ungroup_Command( self )
  register struct zipedit	      *self;
  {
  register zip_type_figure	      figure;

  IN(Ungroup_Command);
  if ( figure = zipedit_Next_Selected_Figure( self, Pane, NULL ) )
    figure->zip_figure_image->zip_image_attributes.zip_image_attribute_group = false;
  OUT(Ungroup_Command);
  }
===*/

static void
Fit_Command( self )
  register struct zipedit	   *self;
  {
  register zip_type_pane	    pane = Pane;
  register zip_type_point	    x, y;
  register float		    scale, EW, EH;

  IN(Fit_Command);
  if ( CurrentImage )
    {
    if ( CurrentFigure  &&  !EnclosureExposed )
      {
      zipedit_Enclose_Figure( self, CurrentFigure, pane );
      zipedit_Reset_Editing_Selection( self, pane );
      CurrentFigure = NULL;
      BuildPending = NULL;
      } 
    scale = Pane->zip_pane_scale;
    EW = abs(EnclosureWidth);
    EH = abs(EnclosureHeight);
    DEBUGgt(Old Scale,scale);
    DEBUGgt(Enclosure-Width,EW);
    DEBUGdt(Pane-Width,zipview_Pane_Width( View, pane ));
    DEBUGgt(Pct-A,EW / zipview_Pane_Width( View, pane ));
    DEBUGgt(Pct-B,1.0/(EW / zipview_Pane_Width( View, pane )));
    if ( EW > EH )
      scale *= (1.0/(EW / zipview_Pane_Width( View, pane )));
      else
      scale *= (1.0/(EH / zipview_Pane_Height( View, pane )));
    DEBUGgt(New Scale,scale);
    x = zipview_X_Pixel_To_Point( View, Pane, NULL, EnclosureLeft + EnclosureWidth/2 );
    y = zipview_Y_Pixel_To_Point( View, Pane, NULL, EnclosureTop  + EnclosureHeight/2 );
    zipedit_Cancel_Enclosure( self, pane );
    zipview_Scale_Pane_To_Point( View, Pane, x, y, scale,
			    zip_center|zip_middle );
    }
  OUT(Fit_Command);
  }

static void
Duplicate_Command( self )
  register struct zipedit	     *self;
  {
  register zip_type_pane	      pane = Pane;

  IN(Duplicate_Command);
  if ( CurrentImage )
    {
    if ( CurrentFigure  &&  !EnclosureExposed )
      {
      zipedit_Enclose_Figure( self, CurrentFigure, pane );
      zipedit_Reset_Editing_Selection( self, pane );
      CurrentFigure = NULL;
      BuildPending = NULL;
      }
    DuplicateSelection = true;
    zipview_Announce( View, ". . . Move Duplication to Desired Position." );
    }
  OUT(Duplicate_Command);
  }
/*===
static void
Replicate_Command( self )
  register struct zipedit	      *self;
  {

  }

static void
Cut_Command( self )
  register struct zipedit	      *self;
  {

  }

static void
Copy_Command( self )
  register struct zipedit	      *self;
  {

  }
===*/

static void
To_Front_Command( self )
  register struct zipedit	      *self;
  {
  register zip_type_pane	      pane = Pane;
  register zip_type_figure	      figure = NULL,
				      peer_figure = CurrentImage->zip_image_figure_anchor;

  while ( peer_figure  &&  peer_figure->zip_figure_next )
    peer_figure = peer_figure->zip_figure_next;
  while ( figure = zipedit_Next_Selected_Figure( self, Pane, figure ) )
    {
    if ( figure != peer_figure )
      {
      DEBUGst(Fronting Figure-name,figure->zip_figure_name );
      zip_Unhook_Figure( Data, figure );
      if ( peer_figure )
        zip_Hook_Figure( Data, figure, peer_figure );
        else
        {
        CurrentImage->zip_image_figure_anchor = figure;
        figure->zip_figure_image = CurrentImage;
        figure->zip_figure_state.zip_figure_state_unhooked = false;
        }
      peer_figure = figure;
      }
    }
  Set_pending_redraw;
  zipview_WantUpdate( View, View );
  }

static void
To_Rear_Command( self )
  register struct zipedit	      *self;
  {
  register zip_type_pane	      pane = Pane;
  register zip_type_figure	      figure = NULL,
				      peer_figure = NULL;

  while ( figure = zipedit_Next_Selected_Figure( self, Pane, figure ) )
    {
    if ( figure != peer_figure )
      {
      DEBUGst(Rearing Figure-name,figure->zip_figure_name );
      zip_Unhook_Figure( Data, figure );
      if ( peer_figure )
        zip_Hook_Figure( Data, figure, peer_figure );
        else
        {
        figure->zip_figure_next = CurrentImage->zip_image_figure_anchor;
        CurrentImage->zip_image_figure_anchor = figure;
        figure->zip_figure_image = CurrentImage;
        figure->zip_figure_state.zip_figure_state_unhooked = false;
        }
      peer_figure = figure;
      }
    }
  Set_pending_redraw;
  zipview_WantUpdate( View, View );
  }
/*===
static void
Foreward_Command( self )
  register struct zipedit	      *self;
  {

  }

static void
Rearward_Command( self )
  register struct zipedit	      *self;
  {

  }
===*/

static void
Manipulate_Pane( self, action )
  register struct zipedit	      *self;
  register int			       action;
  {

  IN(Manipulate_Pane);
  Action |= action;
  zipview_WantUpdate( View, View );
  OUT(Manipulate_Pane);
  }

static void
Delete_Command( self )	    register struct zipedit    *self;
  {  Manipulate_Pane( self, pending_delete );  }

static void
Expose_Coordinates_Command( self ) register struct zipedit     *self;
  {  Manipulate_Pane( self, pending_coordinates );  }

static void
Hide_Coordinates_Command( self ) register struct zipedit     *self;
  {  Manipulate_Pane( self, pending_coordinates );  }

static void
Expose_Grid_Command( self )  register struct zipedit     *self;
  {  Manipulate_Pane( self, pending_grid );  }

static void
Hide_Grid_Command( self )    register struct zipedit    *self;
  {  Manipulate_Pane( self, pending_grid );  }

static void
Grid_Double_Command( self ) register struct zipedit     *self;
  { if ( GridExposed )  Manipulate_Pane( self, pending_double_grid ); }

static void
Grid_Halve_Command( self )  register struct zipedit     *self;
  { if ( GridExposed )  Manipulate_Pane( self, pending_halve_grid ); }
/*===
static void
Expose_Palettes_Command( self )	    register struct zipedit     *self;
  {  Manipulate_Pane( self, pending_palettes );  }

static void
Hide_Palettes_Command( self )	    register struct zipedit     *self;
  {  Manipulate_Pane( self, pending_palettes );  }
===*/

static
Insert_File_By_Name( self, name )
  register struct zipedit	     *self;
  register char			     *name;
  {
  register int			      status = zip_ok;
  char				      msg[512];
  char				      full_name[257];

  IN(Insert_File_By_Name);
  DEBUGst( Name, name );
  filetype_CanonicalizeFilename( full_name, name, 256 );
  DEBUGst( Full-name, full_name );
  SetStreamModified;
  if ( zip_Open_Stream( Data, &Stream, full_name, zip_default ) )
    {
    DEBUG( Open_Stream Failed);
    sprintf( msg, "ZipEdit ERROR: Failed to Insert '%s'", full_name );
    zipview_Announce( View, msg );
    }
    else
    {
    DEBUG( Open_Stream Successful);
    zipview_Announce( View, "Done" );
    if ( status = zip_Read_Stream( Data, Stream ) )
    {
    DEBUG( Read_Stream Failed);
    sprintf( msg, "ZipEdit ERROR: Failed to Insert '%s'", full_name );
    zipview_Announce( View, msg );
    }
    else
    if ( status = zipview_Set_Pane_Stream( View, Pane, Stream ) )
      {
      DEBUGdt( ZipEdit ERROR: Set_Pane_Stream Status, status);
      }
      else
      {
      DEBUG( Set_Pane_Stream successful);
      Set_pending_redisplay;
      zipview_WantUpdate( View, View );
      }
    }
  OUT(Insert_File_By_Name);
  return  status;
  }

static void
Insert_File( self )
  register struct zipedit	     *self;
  {
  char				     *reply;

  IN(Insert_File);
  if ( zipview_Query_File_Name( View, "Enter File Name: ", &reply ) == zip_ok )
    {
    if ( reply  &&  *reply )
      Insert_File_By_Name( self, reply );
    }
  OUT(Insert_File);
  }

static void
Insert_Reference_Command( self )
  register struct zipedit	      *self;
  {
  IN(Insert_Reference_Command);
  Insert_File( self );
  OUT(Insert_Reference_Command);
  }

static void
Insert_Stream_Command( self )
  register struct zipedit	      *self;
  {
  IN(Insert_Stream_Command);
  Insert_File( self );
/*===
  free( Data->stream_file_name );
  Data->stream_file_name = NULL;
===*/
  OUT(Insert_Stream_Command);
  }

static void
Object_Absolute_Command( self )
  register struct zipedit     *self;
  {
  IN(Object_Absolute_Command);
  ObjectWidth = ViewWidth;
  ObjectHeight = ViewHeight;
  DEBUGdt(ObjectWidth,ObjectWidth);
  DEBUGdt(ObjectHeight,ObjectHeight);
  zipview_Set_Pane_Object_Width( View, Pane, ObjectWidth );
  zipview_Set_Pane_Object_Height( View, Pane, ObjectHeight );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_object_absolute)
			    | menu_object_relative );
  zipview_PostMenus( View, Menu );
  OUT(Object_Absolute_Command);
  }

static void
Object_Relative_Command( self )
  register struct zipedit        *self;
  {
  IN(Object_Relative_Command);
  ObjectWidth = ObjectHeight = NULL;
  zipview_Set_Pane_Object_Width( View, Pane, NULL );
  zipview_Set_Pane_Object_Height( View, Pane, NULL );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_object_relative)
			    | menu_object_absolute );
  zipview_PostMenus( View, Menu );
  zipview_Display_Pane( View, Pane );
  OUT(Object_Relative_Command);
  }

static long
Display_Processor( self, pane, action )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  register long			      action;
  {
  IN(Display_Processor);
  zipedit_Display_Background_Pane( self, pane );
  if ( ! BackgroundSelected )
    {
    zipview_Set_Pane_Display_Processor( View, pane, NULL, NULL );
    zipview_Draw_Pane( View, pane );
    zipview_Set_Pane_Display_Processor( View, pane, Display_Processor, self );
    }
  OUT(Display_Processor);
  return  zip_ok;
  }

static void
Background_Command( self )
  register struct zipedit	     *self;
  {
  char				     *reply;
  register long			      status;
  char				      msg[512];
  register zip_type_pane	      pane = Pane;
  register FILE			     *file;

  IN(Background_Command);
   if ( zipview_Query_File_Name( View, "Enter Background File Name: ", &reply ) == zip_ok )
    {
    if ( reply  &&  *reply )
      {
      DEBUGst(Reply,reply);
      if ( (status = zipview_Create_Nested_Pane( View, &BackgroundPane,
			    "ZIP-BACKGROUND-PANE", Pane, zip_default )) == zip_ok )
	{
	zipview_Use_Working_Pane_Cursors( View );
	BackgroundData = raster_New();
	BackgroundView = rasterview_New();
	rasterview_SetDataObject( BackgroundView, BackgroundData );
	file = fopen( reply, "r" );
	raster_Read( BackgroundData, file, 0 );
	BackgroundExposed = true;
	zipedit_Display_Background_Pane( self, Pane );
        zipview_Draw_Pane( View, Pane );
	zipview_Set_Pane_Display_Processor( View, pane, Display_Processor, self );
	zipview_Use_Normal_Pane_Cursors( View );
	}
      if ( status == zip_ok )
	{
	zipview_Announce( View, "Done" );
	menulist_SetMask( Menu, menulist_GetMask( Menu ) |
			    menu_background | menu_background_expose |
			    menu_background_select |  menu_pan_together );
	zipview_PostMenus( View, Menu );
	}
	else
	{
	sprintf( msg, "ZipEdit ERROR: Failed to Set Background '%s', Status = %d", reply, status );
	zipview_Announce( View, msg );
	}
      }
    }
  OUT(Background_Command);
  }

static void
Hide_Background_Command( self )
  register struct zipedit	     *self;
  {
  IN(Hide_Background_Command);
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_background_expose) |
			   menu_background_hide );
  zipview_PostMenus( View, Menu );
  BackgroundExposed = false;
  zipview_Remove_Pane( View, Pane->zip_pane_edit->zip_pane_edit_background_pane );
  zipview_Display_Pane( View, Pane );
  OUT(Hide_Background_Command);
  }

static void
Expose_Background_Command( self )
  register struct zipedit	     *self;
  {
  IN(Expose_Background_Command);
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_background_hide)
			    | menu_background_expose );
  zipview_PostMenus( View, Menu );
  BackgroundExposed = true;
  zipedit_Display_Background_Pane( self, Pane );
  zipview_Draw_Pane( View, Pane );
  OUT(Expose_Background_Command);
  }

zipedit_Display_Background_Pane( self, pane )
  register struct zipedit	     *self;
  register zip_type_pane	      pane;
  {
  register long			      status = zip_ok;
  register long			      left = zipview_Pane_Left( View, pane )+1,
				      top =  zipview_Pane_Top( View, pane )+1,
				      width = zipview_Pane_Width( View, pane )-2,
				      height = zipview_Pane_Height( View, pane )-2;

  IN(zipedit_Display_Background_Pane);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( pane  &&  BackgroundPane  &&  BackgroundExposed )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    zipview_SetTransferMode( View, graphic_WHITE );
    zipview_EraseRectSize( View, left, top, width, height );
    rasterview_LinkTree( BackgroundView, View );
    rasterview_InsertViewSize( BackgroundView, View, left, top, width, height );
    rasterview_FullUpdate( BackgroundView, view_FullRedraw, left, top, width, height );
    if ( BackgroundLightened )
      Lighten_Background( self );
    }
  OUT(zipedit_Display_Background_Pane);
  return status;
  }

static
Lighten_Background( self )
  register struct zipedit	     *self;
  {
  register zip_type_pane	      pane = Pane;
  register long			      left = zipview_Pane_Left( View, pane )+1,
				      top =  zipview_Pane_Top( View, pane )+1,
				      width = zipview_Pane_Width( View, pane )-2,
				      height = zipview_Pane_Height( View, pane )-2;

  IN(Lighten_Background_Command);
  if ( BackgroundExposed )
    {
    if ( strcmp( zipview_GetWindowManagerType( View ), "AndrewWM" ) == 0 )
      zipview_SetTransferMode( View, graphic_WHITE );
      else
      zipview_SetTransferMode( View, graphic_AND );
    zipview_FillTrapezoid( View, left,top,width, left,top+height,width,
      zipview_Define_Graphic( View, zip_Define_Font( Data, ShadeFontName, NULL ), '5' ) );
    if ( !BackgroundSelected )
      {
      zipview_Set_Pane_Display_Processor( View, pane, NULL, NULL );
      zipview_Draw_Pane( View, Pane );
      zipview_Set_Pane_Display_Processor( View, pane, Display_Processor, self );
      }
    }
  OUT(Lighten_Background_Command);
  }

static void
Lighten_Background_Command( self )
  register struct zipedit	     *self;
  {
  IN(Lighten_Background_Command);
  if ( BackgroundExposed )
    {
    BackgroundLightened = true;
    Lighten_Background( self );
    }
  OUT(Lighten_Background_Command);
  }

static void
Darken_Background_Command( self )
  register struct zipedit	     *self;
  {
  IN(Darken_Background_Command);
  if ( BackgroundExposed )
    {
    BackgroundLightened = false;
/*===*/  Expose_Background_Command( self );
    }
  OUT(Darken_Background_Command);
  }

static void
Select_Background_Command( self )
  register struct zipedit	     *self;
  {
  register zip_type_pane	      pane = Pane;

  IN(Select_Background_Command);
  BackgroundSelected = BackgroundExposed = true;
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_background_select)
			    | menu_background_unselect );
  zipedit_Display_Background_Pane( self, Pane );
  Abeyant = on;
  rasterview_WantInputFocus( BackgroundView, BackgroundView );
  im_ForceUpdate();
  OUT(Select_Background_Command);
  }

static void
Unselect_Background_Command( self )
  register struct zipedit	     *self;
  {
  IN(Unselect_Background_Command);
  zipview_WantInputFocus( View, View );
  im_ForceUpdate();
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_background_unselect)
			    | menu_background_select );
  menulist_ClearChain( Menu );
  zipview_PostMenus( View, Menu );
  Abeyant = off;
  BackgroundSelected = false;
  zipview_Draw_Pane( View, Pane );
  OUT(Unselect_Background_Command);
  }

/*===
static void
Page_New_Command( self )
  register struct zipedit	     *self;
  {
  char				     *page_name = "ZIP_PAGE_IMAGE_nnn";
  register zip_type_image	      root_image =
	    Pane->zip_pane_current_stream->zip_stream_image_anchor;
  zip_type_image		      page_image;
  zip_type_figure		      page_figure;
  register zip_type_figure	      figure;
  register long			      status = zip_ok;

  IN(Page_New_Command);
  if ( Data->page_count == 0 )
    {
    DEBUG(Page-count Zero);
    if ( status = zip_Create_Inferior_Image( Data, &page_image, "ZIP_PAGE_IMAGE_001",
      Pane->zip_pane_current_stream, root_image ) )
      {
      DEBUGdt(Status,status);
      }
      else
      {
      zip_Create_Figure( Data, &page_figure, "ZIP_PAGE_FIGURE_001",
		         zip_rectangle_figure/*===!/, page_image, NULL );
      zip_Set_Figure_Point( Data, page_figure, zip_figure_origin_point,
			    root_image->zip_image_stream->zip_stream_least_x,
			    root_image->zip_image_stream->zip_stream_greatest_y );
      zip_Set_Figure_Point( Data, page_figure, zip_figure_auxiliary_point,
			    root_image->zip_image_stream->zip_stream_greatest_x,
			    root_image->zip_image_stream->zip_stream_least_y );
      figure = root_image->zip_image_figure_anchor;
      root_image->zip_image_figure_anchor = NULL;
      page_figure->zip_figure_next = figure;
      while ( figure )
        {
        DEBUGst(Figure-name,figure->zip_figure_name);
        figure->zip_figure_image = page_image;
        figure = figure->zip_figure_next;
        }
      Data->page_count++;
      }
    }
  if ( status == zip_ok )
    {
    sprintf( page_name, "ZIP_PAGE_IMAGE_%03d", ++Data->page_count );
    DEBUGst(Page-name,page_name);
    zip_Create_Inferior_Image( Data, &page_image, page_name,
	    Pane->zip_pane_current_stream, root_image );
    sprintf( page_name, "ZIP_PAGE_FIGURE_%03d", Data->page_count );
    zip_Create_Figure( Data, &page_figure, page_name,
		       zip_rectangle_figure/*===!/, page_image, NULL );
    zip_Set_Figure_Point( Data, page_figure, zip_figure_origin_point,-1000,1400 );
    zip_Set_Figure_Point( Data, page_figure, zip_figure_auxiliary_point, 1000,-1400 );
    zipview_Display_Image( View, page_image, Pane );
    }
  OUT(Page_New_Command);
  }

static void
Page_Delete_Command( self )
  register struct zipedit	     *self;
  {
  IN(Page_Delete_Command);

  OUT(Page_Delete_Command);
  }

static void
Pan_Alone_Command( self )
  register struct zipedit	     *self;
  {
  IN(Pan_Alone_Command);

  OUT(Pan_Alone_Command);
  }

static void
Pan_Together_Command( self )
  register struct zipedit	     *self;
  {
  IN(Pan_Together_Command);

  OUT(Pan_Together_Command);
  }
===*/

/*===
static void
Expose_Stream_Command( self )
  register struct zipedit	     *self;
  {
  IN(Expose_Stream);

  OUT(Expose_Stream);
  }

static void
Hide_Stream_Command( self )
  register struct zipedit	     *self;
  {
  IN(Hide_Stream_Command);

  OUT(Hide_Stream_Command);
  }

static void
ReRead_Stream_Command( self )
  register struct zipedit	     *self;
  {
  IN(ReRead_Stream_Command);

  OUT(ReRead_Stream_Command);
  }
===*/

static struct bind_Description	      bound_menu[] =
{
{ "zipedit-insert-stream",  "\033Is",	0,	"File,Insert Stream",	0,
	menu_default | menu_background_select,
	Insert_Stream_Command,	    "...",	NULL },
{ "zipedit-insert-reference","\033Ir",	0,	"File,Insert Reference",0,
	menu_default | menu_background_select,
	Insert_Reference_Command,   "...", NULL },
/*===
{ "zipedit-expose-stream", "File,Expose Stream",	"",
	Expose_Stream_Command,"...",	menu_app | menu_stream_expose | menu_background_select },
{ "zipedit-hide-stream", "File,Hide Stream",	"",
	Hide_Stream_Command,"...",	menu_app | menu_stream_hide | menu_background_select },
{ "zipedit-reread-stream", "File,ReRead Stream",	"",
	ReRead_Stream_Command,"...",	menu_app | menu_stream_hide | menu_background_select },
===*/
{ "zipedit-background",	    "\033BN",	0,	"Pane,Background~90",	    0,
	menu_default | menu_background_select,
	Background_Command,	    "...",	NULL },
{ "zipedit-hide-background","\033BH",	0,	"Background~97,Hide~10",    0,
	menu_background_expose | menu_background_select,
	Hide_Background_Command,    "...",	NULL },
{ "zipedit-expose-background","\033BE",	0,	"Background~97,Expose~10",  0,
	menu_background_hide | menu_background_select,
	Expose_Background_Command,  "...",	NULL },
{ "zipedit-lighten-background","\033BL",    0,	"Background~97,Lighten~20", 0,
	menu_background | menu_background_select,
	Lighten_Background_Command, "...",	NULL },
{ "zipedit-darken-background","\033BD",	0,	"Background~97,Darken~21",  0,
	menu_background | menu_background_select ,
	Darken_Background_Command,  "...",	NULL},
{ "zipedit-select-background","\033BS",	0,	"Background~97,Select~30",  0,
	menu_background | menu_background_select,
	Select_Background_Command,  "...",	NULL },
{ "zipedit-unselect-background","\033BU",0,	"Background~97,Unselect~30",0,
	menu_background | menu_background_unselect,
	Unselect_Background_Command,"...",	NULL },
/*===
{ "zipedit-pan-alone", "Foreground~98,Pan Alone~30",	"",
	Pan_Alone_Command,  "...",	menu_pan_together | menu_background_select },
{ "zipedit-pan-together", "Foreground~98,Pan Together~30",	"",
	Pan_Together_Command,  "...",	menu_pan_alone | menu_background_select },
===*/

{ "zipedit-delete",	"\033SD",	0,	"Selection,Delete~10",	    0,
	menu_selection | menu_background_select,
	Delete_Command,		    "...",	NULL },
/*===
{ "zipedit-group",   "Selection,Group~20",	"",
	Group_Command,"...",	 menu_selection | menu_background_select },
{ "zipedit-ungroup", "Selection,Un-Group~21",	"",
	Ungroup_Command,"...", menu_selection | menu_background_select },
===*/
{ "zipedit-duplicate",	"\033SM",	0,	"Selection,Duplicate~30",   0,
	menu_selection | menu_background_select,
	Duplicate_Command,	    "...",	NULL },
/*===
{ "zipedit-replicate", "Selection,Replicate~31",	"",
	Replicate_Command,"...", menu_selection | menu_background_select },
{ "zipedit-copy", "Selection,Copy~32",	"",
	Copy_Command,"...", menu_selection | menu_background_select },
{ "zipedit-cut", "Selection,Cut~33",	"",
	Cut_Command,"...", menu_selection | menu_background_select },
===*/
{ "zipedit-to-front",	"\033SF",   0,		"Selection,To Front~40",    0,
	menu_selection | menu_background_select,
	To_Front_Command,	    "...",	NULL },
{ "zipedit-to-rear",	"\033SR",   0,		"Selection,To Rear~41",	    0,
	menu_selection | menu_background_select,
	To_Rear_Command,	    "...",	NULL },
{ "zipedit-fit",	"\033SZ",   0,		"Selection,Fit~50",	    0,
	menu_selection | menu_background_select | menu_fit,
	Fit_Command,	    "...",	NULL },
/*===
{ "zipedit-foreward", "Selection,Foreward~42",	"",
	Foreward_Command,"...", menu_selection | menu_background_select },
{ "zipedit-rearward", "Selection,Rearward~43",	"",
	Rearward_Command,"...", menu_selection | menu_background_select },

{ "zipedit-palettes-expose","\033PE",	0,	"Pane,Expose Palettes~80",  0,
	menu_palettes_expose | menu_background_select,
	Expose_Palettes_Command,    "...",  NULL },
{ "zipedit-palettes-hide",  "\033PH",	0,	"Pane,Hide Palettes~80",    0,
	menu_palettes_hide | menu_background_select,
	Hide_Palettes_Command,	    "...",  NULL },
===*/
{ "zipedit-grid-expose",    "\033GE",	0,      "Grid~50,Expose~10",	    0,
	menu_grid_expose | menu_background_select,
	Expose_Grid_Command,	    "...",  NULL },
{ "zipedit-grid-hide",	    "\033GH",	0,	"Grid~50,Hide~10",	    0,
	menu_grid_hide | menu_background_select,
	Hide_Grid_Command,	    "...",  NULL },
{ "zipedit-grid-double",    "\033GD",	0,	"Grid~50,Double~20",	    0,
	menu_grid_hide | menu_background_select,
	Grid_Double_Command,	    "...",  NULL },
{ "zipedit-grid-halve",	    "\033GL",	0,	"Grid~50,Halve~21",	    0,
	menu_grid_hide | menu_background_select,
	Grid_Halve_Command,	    "...",  NULL },

{ "zipedit-coordinates-expose","\033GCE",0, "Grid~50,Expose Coordinates~30", 0,
	menu_coordinates_expose | menu_background_select,
	Expose_Coordinates_Command, "...",  NULL },
{ "zipedit-coordinates-hide","\033GCH",	0,  "Grid~50,Hide Coordinates~30",  0,
	menu_coordinates_hide | menu_background_select,
	Hide_Coordinates_Command,   "...",  NULL },
{ "zipedit-absolute",	    "\033OA",	0,  "Object~70,Make Absolute~10",   0,
	menu_object_absolute | menu_background_select,
	Object_Absolute_Command,    "...",  NULL },
{ "zipedit-relative",	    "\033OR",	0,  "Object~70,Make Relative~10",   0,
	menu_object_relative | menu_background_select,
	Object_Relative_Command,    "...",  NULL },
/*===
{ "zipedit-new-page",	    "Page,New~30",	"",
	Page_New_Command,"...",	menu_default | menu_background_select },
{ "zipedit-delete-page",    "Page,Delete~31",	"",
	Page_Delete_Command,"...", menu_default | menu_background_select },
===*/
NULL
};

static
Build_Menu()
  {
  IN(Build_Menu);
  bind_BindList( bound_menu, class_keymap, class_menulist, &zipedit_classinfo );
  OUT(Build_Menu);
  }

static void
Accept_Character( self, c )
  register struct zipedit	     *self;
  register char			      c;
  {
  IN(Accept_Character);
  DEBUGct(C,c);
  if ( KeyboardProcessor )
    {
    DEBUG(>>> Keyboard Processor);
    (*KeyboardProcessor)( KeyboardAnchor, Pane, c, NULL, NULL, NULL, NULL );
    DEBUG(<<< Keyboard Processor);
    }
  OUT(Accept_Character);
  }

static
Pending_Delete( self, pane )
  register struct zipedit	    *self;
  register zip_type_pane	     pane;
  {
  register zip_type_figure	     figure;

  IN(Pending_Delete);
  Reset_pending_delete;
  if ( pane->zip_pane_current_image  &&
       pane->zip_pane_edit->zip_pane_edit_selection_level >= ImageSelection )
    zipedit_Delete_Image( self, pane->zip_pane_current_image, pane );
    else
    if ( pane->zip_pane_current_figure  &&
	 pane->zip_pane_edit->zip_pane_edit_selection_level >= PointSelection )
      zipedit_Delete_Figure( self, pane->zip_pane_current_figure, pane );
  figure = zipedit_Next_Selected_Figure( self, pane, NULL );
  while ( figure )
    {
    zipedit_Delete_Figure( self, figure, pane );
    figure = zipedit_Next_Selected_Figure( self, pane, figure );
    }
  zipedit_Cancel_Enclosure( self, pane );
  zipview_Draw_Pane( View, pane );
  zipedit_Hide_Selection_Menu( self );
  zip_NotifyObservers( Data, View );
  OUT(Pending_Delete);
  }

zipedit_Expose_Selection_Menu( self )
  register struct zipedit	   *self;
  {
  menulist_SetMask( Menu, menulist_GetMask( Menu ) | menu_selection );
  zipview_PostMenus( View, Menu );
  }

zipedit_Hide_Selection_Menu( self )
  register struct zipedit	   *self;
  {
  menulist_SetMask( Menu, menulist_GetMask( Menu ) & ~menu_selection );
  zipview_PostMenus( View, Menu );
  }

static
Pending_Palettes( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  register long			    mask = menulist_GetMask( Menu );

  IN(Pending_Palettes);
  if ( PalettesExposed = ! PalettesExposed )
    {
    zipedit_Expose_Palettes( self, pane );
    mask = (mask & ~menu_palettes_expose) | menu_palettes_hide;
    }
    else
    {
    zipedit_Hide_Palettes( self, pane );
    mask = (mask & ~menu_palettes_hide) | menu_palettes_expose;
    }
  menulist_SetMask( Menu, mask );
  zipview_PostMenus( View, Menu );
  Reset_pending_palettes;
  OUT(Pending_Palettes);
  }

static
Pending_Coordinates( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  register long			    mask = menulist_GetMask( Menu );

  IN(Pending_Coordinates);
  if ( CoordinatesExposed = ! CoordinatesExposed )
    {
    zipedit_Expose_Pane_Coordinates( self, pane );
    mask = (mask & ~menu_coordinates_expose) | menu_coordinates_hide;
    }
    else
    {
    zipedit_Hide_Pane_Coordinates( self, pane );
    mask = (mask & ~menu_coordinates_hide) | menu_coordinates_expose;
    }
  menulist_SetMask( Menu, mask );
  zipview_PostMenus( View, Menu );
  Reset_pending_coordinates;
  OUT(Pending_Coordinates);
  }

static
Pending_Grid( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  register long			    mask = menulist_GetMask( Menu );

  IN(Pending_Grid);
  if ( GridExposed = ! GridExposed )
    {
    zipedit_Expose_Pane_Grid( self, pane );
    mask = (mask & ~menu_grid_expose) | menu_grid_hide;
    }
    else
    {
    zipedit_Hide_Pane_Grid( self, pane );
    mask = (mask & ~menu_grid_hide) | menu_grid_expose;
    }
  menulist_SetMask( Menu, mask );
  zipview_PostMenus( View, Menu );
  Reset_pending_grid;
  OUT(Pending_Grid);
  }

static
Pending_Double_Grid( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  IN(Pending_Double_Grid);
  zipedit_Double_Pane_Grid( self, pane );
  Reset_pending_double_grid;
  OUT(Pending_Double_Grid);
  }

static
Pending_Redisplay( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  IN(Pending_Redisplay);
  Reset_pending_redisplay;
  zipview_Display_Pane( View, pane );
  OUT(Pending_Redisplay);
  }

static
Pending_Redraw( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  IN(Pending_Redraw);
  Reset_pending_redraw;
  zipview_Draw_Pane( View, pane );
  OUT(Pending_Redraw);
  }

static
Pending_Halve_Grid( self, pane )
  register struct zipedit	   *self;
  register zip_type_pane	    pane;
  {
  IN(Pending_Halve_Grid);
  zipedit_Halve_Pane_Grid( self, pane );
  Reset_pending_halve_grid;
  OUT(Pending_Halve_Grid);
  }

long
zipedit__Redisplay_Panes( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit__Redisplay_Panes);
  DEBUGst(Pane-name,pane->zip_pane_name);
  zipview_Redisplay_Panes( View );
  zipedit_Redisplay_Edit_Pane( self, pane );
  OUT(zipedit__Redisplay_Panes);
  return  zip_ok;
  }

long
zipedit__Handle_Editing( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit__Handle_Editing);
  DEBUGst(Pane-name,pane->zip_pane_name);

  OUT(zipedit__Handle_Editing);
/*===*/return  zip_failure;
  }

long
zipedit__Initialize_Editing( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;

  IN(zipedit__Initialize_Editing);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( pane )
    {
    zipedit_Prepare_Editing_Control( self, pane );
    zipedit_Expose_Palettes( self, pane );
    zipview_PostMenus( View, Menu );
    KeyState->next = NULL;
    zipview_PostKeyState( View, KeyState );
    }
  OUT(zipedit__Initialize_Editing);
  return status;
  }

long
zipedit__Terminate_Editing( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;

  IN(zipedit__Terminate_Editing);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( pane )
    {
    if ( KeyboardProcessor )
      (*KeyboardProcessor)( KeyboardAnchor, Pane, NULL, NULL, NULL, NULL, NULL );
    KeyboardProcessor = NULL;
    PendingProcessor = NULL;
    zipview_Reset_Pane_Display_Processor( View, pane );
    zipedit_Cancel_Enclosure( self, pane );
    zipedit_Hide_Selection_Menu( self );
    zipedit_Reset_Editing_Selection( self, pane );
/*=== avoid ???===*/
    pane->zip_pane_state.zip_pane_state_coordinates_exposed = false;
    pane->zip_pane_edit->zip_pane_edit_coordinate_grid = 0;
    if ( PalettesExposed )
      zipedit_Hide_Palettes( self, pane );
    zipedit_Reset_Editing_Control( self, pane );
    menulist_UnchainML( Menu, Menu );
    zipview_PostMenus( View, Menu );
    }
  OUT(zipedit__Terminate_Editing);
  return status;
  }

int
zipedit_Prepare_Editing_Control( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;

  IN(zipedit_Prepare_Editing_Control);
  if ( pane->zip_pane_edit == NULL )
    if ( (pane->zip_pane_edit = (zip_type_pane_edit)
		 calloc( 1, sizeof(struct zip_pane_edit ) )) == NULL )
      {
      status = zip_insufficient_pane_space;
/*=== provide message for failed allocation of pane_edit structure*/
      }
  DEBUGdt(Status,status);
  OUT(zipedit_Prepare_Editing_Control);
  return status;
  }

int
zipedit_Reset_Editing_Control( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit_Reset_Editing_Control);
/*=== consider whether state should be left alive for next return into editing... ===*/
  if ( PriorPane )
    zipview_Invert_Pane( View, PriorPane );
  PriorPane = NULL;
  pane->zip_pane_edit->zip_pane_edit_last_point_id = NULL;
  pane->zip_pane_edit->zip_pane_edit_beginning = NULL;
  pane->zip_pane_edit->zip_pane_edit_build_constraint = NULL;
  pane->zip_pane_edit->zip_pane_edit_build_pending = NULL;
  pane->zip_pane_edit->zip_pane_edit_build_figure = NULL;
  pane->zip_pane_edit->zip_pane_edit_selection_level = NULL;
  pane->zip_pane_edit->zip_pane_edit_coordinate_grid = NULL;
  pane->zip_pane_edit->zip_pane_edit_mark_point_delta = NULL;
  pane->zip_pane_edit->zip_pane_edit_mark_point_spacing = NULL;
  OUT(zipedit_Reset_Editing_Control);
  return zip_success;
  }

zipedit_Reset_Editing_Selection( self, pane )
  register struct zipedit	      *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit_Reset_Editing_Selection);
  BuildPending = NULL;
  Building = true;
  if ( CurrentFigure )
    if ( SelectionLevel >= ImageSelection )
      zipedit_Normalize_Image_Points( self, CurrentImage, pane );
      else
      zipedit_Normalize_Figure_Points( self, CurrentFigure, pane );
  CurrentFigure = NULL;
  SelectionLevel = 0;
  zipview_Set_Pane_Cursor( View, pane, 'A', CursorFontName );
  if ( PriorPane )
    {
    if ( FigurePalette )
       zipview_Invert_Pane( View, PriorPane );
    zipview_Pane_Inverted( self, PriorPane ) = false;
    PriorPane = NULL;
    }
  OUT(zipedit_Reset_Editing_Selection);
  }
