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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/utility/RCS/ltv.c,v 1.4 1993/01/08 16:36:51 rr2b R6tape $";
#endif


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The LightTable View-object Program

MODULE	ltv.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the LightTable View-object.

HISTORY
  10/10/88	Created (TCP)
  08/02/89	Accomodate Suite interface upgrade -- suite_ChangeItemCaption (TCP)
  08/25/89	More Accomodations (TCP)
  08/29/89	More Accomodations (TCP)
  08/31/89	Change Data to Datum (TCP)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <im.ih>
#include <environ.ih>
#include <graphic.ih>
#include <fontdesc.ih>
#include <observe.ih>
#include <bind.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <view.ih>
#include <message.ih>
#include <rect.h>
#include <lt.ih>
#include <ltv.eh>
#include <suite.ih>
#include <zip.ih>
#include <zipv.ih>
#include <zipobj.ih>
#include <rasterv.ih>

static boolean debug=FALSE;
static struct menulist		     *class_menulist;
static struct keymap		     *class_keymap;


static long				      Exceptions();
static /*===*/struct ltv *SELF;

#define  tolerance		    5

#define  enclosure_cursor	    'P'
#define  normal_cursor		    'A'
#define  invisible_cursor	    '@'
#define  build_cursor		    '/'

#define  hide_background	    (1<<0)
#define  expose_background	    (1<<1)
#define  pan_foreground		    (1<<2)
#define  pan_together		    (1<<3)

#define  InputFocus		    (self->input_focus)
#define  StreamLocal		    (self->stream)
#define  Image			    (self->image)
#define  Figure			    (self->figure)
#define  Point			    (self->point)
#define  Zip			    (self->data->zip)
#define  ZipView		    (self->zipview)
#define  ForegroundPane		    (self->foreground_pane)
#define  Build			    (self->build)
#define  Building		    (self->building)
#define  Modifying		    (self->modifying)
#define  Modified		    (self->modified)
#define  Tracking		    (self->tracking)
#define	 Block			    (&self->block)
#define  Left			    (self->block.left)
#define  Top			    (self->block.top)
#define  Width			    (self->block.width)
#define  Height			    (self->block.height)
#define  Bottom			    (Top + self->block.height)
#define  ForegroundPanning	    (self->foreground_panning)
#define  BackgroundPane		    (self->background_pane)
#define  BackgroundLight	    (self->background_light)
#define  BackgroundExposed	    (self->background_exposed)
#define  BackgroundXOffset	    (self->background_x_offset)
#define  BackgroundYOffset	    (self->background_y_offset)
#define  BackgroundLeft		    (self->background_left)
#define  BackgroundLeftX	    (self->background_left + self->background_x_offset )
#define  BackgroundTop		    (self->background_top)
#define  BackgroundTopY		    (self->background_top + self->background_y_offset )
#define  BackgroundWidth	    (self->data->background_width)
#define  BackgroundHeight	    (self->data->background_height)
#define  Raster			    (self->data->raster)
#define  RasterView		    (self->rasterview)
#define  LeftNameItem		    (self->left_name_item)
#define  RightNameItem		    (self->right_name_item)
#define  Buttons		    (self->buttons)
#define  ButtonHeight		    (63)
#define  ButtonTop		    (Top + Height - ButtonHeight)
#define  Menu			    (self->menu)
#define  InitialX		    (self->initial_x_pixel)
#define  InitialY		    (self->initial_y_pixel)
#define  WM			    (self->wm_type)
#define  WMWM			    (self->wm_type == 1)
#define  XWM			    (self->wm_type == 2)
#define  TentativeName		    (self->tentative)
#define  EnclosureExposed	    (self->enclosure_exposed)
#define  EnclosureLeft		    (self->enclosure.left)
#define  EnclosureTop		    (self->enclosure.top)
#define  EnclosureWidth		    (self->enclosure.width)
#define  EnclosureHeight	    (self->enclosure.height)

#define  ChangeItemCaption(o,i,c) \
      suite_ChangeItemAttribute( o, i, suite_ItemCaption(c) )

static int				    Begin_Chain_Button(), End_Chain_Button(),
				    Rename_Chain_Button(), Delete_Chain_Button(),
				    Left_Chain_Button(), Right_Chain_Button();

static Detect();
static Initialize();
static Build_Chain();
static Modify_Chain();
static End_Chain();
static Track_Enclosure();
static Cancel_Enclosure();
static Clear_Enclosure();
static Draw_Enclosure();
static Invert_Enclosure();
static Neighbor();
static Show_Chain_Names();
static Clear_Chain_Names();
static Name_Chain();
static Split_Chain_Name();
static Passivate();
static Lighten_Background();
static Show_Background();
static Build_Menu();
static Activate();

#define  right_code	    1
#define  left_code	    2
#define  begin_code	    3
#define  end_code	    4
#define  rename_code	    5
#define  delete_code	    6

static suite_Specification		    begin_button[] =
  {
  suite_ItemCaption( "Begin Chain" ),
  suite_ItemDatum( begin_code ),
  suite_ItemHitHandler( Begin_Chain_Button ),
  NULL
  };

static suite_Specification		    end_button[] =
  {
  suite_ItemCaption( "End Chain" ),
  suite_ItemDatum( end_code ),
  suite_ItemHitHandler( End_Chain_Button ),
  NULL
  };

static suite_Specification		    rename_button[] =
  {
  suite_ItemCaption( "Rename Chain" ),
  suite_ItemDatum( rename_code ),
  suite_ItemHitHandler( Rename_Chain_Button ),
  NULL
  };

static suite_Specification		    delete_button[] =
  {
  suite_ItemCaption( "Delete Chain" ),
  suite_ItemDatum( delete_code ),
  suite_ItemHitHandler( Delete_Chain_Button ),
  NULL
  };

static suite_Specification		    right_name_button[] =
  {
  suite_ItemTitleCaption( "Right:" ),
  suite_ItemDatum( right_code ),
  suite_ItemTitlePlacement( suite_Left ),
  suite_ItemTitleFontName( "andysans10b" ),
  suite_ItemCaptionFontName( "andy10b" ),
/*===  suite_ItemType( suite_ReadWrite ),*/
  suite_ItemHitHandler( Right_Chain_Button ),
  NULL
  };

static suite_Specification		    left_name_button[] =
  {
  suite_ItemTitleCaption( "Left:" ),
  suite_ItemDatum( left_code ),
  suite_ItemTitlePlacement( suite_Left ),
  suite_ItemTitleFontName( "andysans10b" ),
  suite_ItemCaptionFontName( "andy10b" ),
/*===  suite_ItemType( suite_ReadWrite ),*/
  suite_ItemHitHandler( Left_Chain_Button ),
  NULL
  };

static suite_Specification		    buttons[] =
  {
  suite_ItemCaptionFontName( "andysans10b" ),
  suite_Item( begin_button ),
  suite_Item( end_button ),
  suite_Item( rename_button ),
  suite_Item( delete_button ),
  suite_Item( right_name_button ),
  suite_Item( left_name_button ),
/*===  suite_Arrangement( suite_Row ),*/
  suite_Arrangement( suite_Balanced | suite_Matrix ),
  NULL
  };

void
ltv__Set_Debug( self, mode )
  register struct ltv		     *self;
  {
  debug = mode;
  zipview_Set_Debug( ZipView, debug );
  }

boolean
ltv__InitializeClass( classID )
  register struct classheader   *classID;
  {
  IN(ltv_InitializeClass);
  class_menulist = menulist_New();
  class_keymap = keymap_New();
  Build_Menu();
  OUT(ltv_InitializeClass);
  return TRUE;
  }

boolean 
ltv__InitializeObject( classID, self )
  register struct classheader *classID;
  register struct ltv	      *self;
  {
  SELF=self;
  IN(ltv_InitializeObject);
  ForegroundPane = BackgroundPane = NULL;
  self->data = NULL;
  ZipView = zipview_New();
  zipview_Set_Debug( ZipView, debug );
  Menu = menulist_DuplicateML( class_menulist, self );
  InputFocus = BackgroundExposed = true;
  BackgroundLight = 0;
  Building = Build = Modifying = Modified = ForegroundPanning =
  Tracking = EnclosureExposed = false;
  BackgroundXOffset = BackgroundYOffset = 0;
  EnclosureLeft = EnclosureTop = EnclosureWidth = EnclosureHeight = 0;
  LeftNameItem = RightNameItem = NULL;
  StreamLocal = NULL;
  Image = NULL;
  Figure = NULL;
  Point = NULL;
  WM = 0;
  OUT(ltv_InitializeObject);
  return  TRUE;
  }

void
ltv__FinalizeObject( classID, self )
  register struct classheader	*classID;
  register struct ltv		*self;
{
  if(Menu) menulist_Destroy(Menu);
  if(ZipView) {
      zipview_UnlinkTree(ZipView);
      zipview_Destroy(ZipView);
      ZipView = NULL;
  }
  if(Buttons) {
      suite_UnlinkTree(Buttons);
      suite_Destroy(Buttons);
      Buttons = NULL;
  }
}

void
ltv__SetDataObject( self, data )
  register struct ltv	      *self;
  register struct lt	      *data;
  {
  self->data = data;
  zipview_SetDataObject( ZipView, Zip );
  lt_AddObserver( self->data, self );
  zip_Set_general_Exception_Handler( Zip, Exceptions );
  }

void
ltv__ReceiveInputFocus( self )
  register struct ltv	     *self;
  {
  IN(ltv_ReceiveInputFocus);
  InputFocus = true;
  menulist_SetMask( Menu, hide_background | pan_foreground );
  ltv_PostMenus( self, Menu );
  OUT(ltv_ReceiveInputFocus);
  }

void
ltv__LoseInputFocus( self )
  register struct ltv	     *self;
  {
  IN(ltv_LoseInputFocus);
  InputFocus = false;
  OUT(ltv_LoseInputFocus);
  }

enum view_DSattributes
ltv__DesiredSize( self, given_width,   given_height,
			 pass, desired_width, desired_height )
  register struct ltv	     *self;
  register long		      given_width, given_height;
  register enum view_DSpass   pass;
  register long		     *desired_width, *desired_height;
  {
  IN(ltv_DesiredSize);
  *desired_width  = 50;
  *desired_height = 100;
  OUT(ltv_DesiredSize);
  return  view_Fixed;
  }

void
ltv__FullUpdate( self, type, left, top, width, height )
  register struct ltv	     *self;
  register enum view_UpdateType    type;
  register long		      left, top, width, height;
  {
  IN(ltv_FullUpdate);
  if ( type == view_FullRedraw || type == view_LastPartialRedraw )
    {
    Cancel_Enclosure( self );
    ltv_GetVisualBounds( self, Block );
    DEBUGdt(Left,Left); DEBUGdt(Top,Top);
    DEBUGdt(Width,Width); DEBUGdt(Height,Height);
    if ( ForegroundPane == NULL )
      Initialize( self );
    zipview_InsertViewSize( ZipView, self, Left, Top, Width, Height - ButtonHeight );
    suite_InsertViewSize( Buttons, self, Left, Bottom - ButtonHeight, Width, ButtonHeight );
    suite_FullUpdate( Buttons, type, 0, 0, Width, ButtonHeight );
    if ( Figure == NULL )
      {
      Passivate( self, delete_code );
      Passivate( self, rename_code );
      Passivate( self, end_code );
      }
    BackgroundLeft = (Left + Width/2) - BackgroundWidth/2;
    BackgroundTop  = (Top + Height/2) - BackgroundHeight/2;
    Show_Background( self );
    zipview_Use_Normal_Pane_Cursors( ZipView );
    }
  OUT(ltv_FullUpdate);
  }

static
Detect( self, suite, item, datum )
  register struct ltv	   *self;
  register struct suite	   *suite;
  register struct suite_item   *item;
  register long		    datum;
  {
  IN(Detect);
  switch ( suite_ItemAttribute( Buttons/*===*/, item, suite_ItemDatum(0) ) )
    {
    case  left_code:
      LeftNameItem = item;
      break;
    case  right_code:
      RightNameItem = item;
      break;
    }
  OUT(Detect);
  }

static
Initialize( self )
  register struct ltv	     *self;
  {
  register long		      status = 0;
  register zip_type_image     root_image;
  register boolean	      satisified = false;
  char			     *reply, stream_name[512], raster_name[512];

  IN(Initialize);
  zipview_LinkTree( ZipView, self );
  reply = zipview_GetWindowManagerType( ZipView );
  DEBUGst(Window Manager,reply);
  if ( strcmp( "AndrewWM", reply ) == 0 )   WM = 1;  else  WM = 2;
  zipview_Use_Working_Pane_Cursors( ZipView );
  zipview_Create_Pane( ZipView, &ForegroundPane, "Foreground-Pane", Block, zip_opaque );
  zipview_Set_Pane_Cursor( ZipView, ForegroundPane, normal_cursor, "aptcursors20" );
  zipview_Set_Pane_Panning_Precision( ZipView, ForegroundPane, 1 );
  RasterView = rasterview_New();
  rasterview_SetDataObject( RasterView, Raster );
  rasterview_LinkTree( RasterView, self );
  if ( self->data->foreground_stream == NULL )
    {
    zipview_Use_Normal_Pane_Cursors( ZipView );
    while ( ! satisified )
	{
    if ( zipview_Query_File_Name( ZipView, "Enter Zip file-name: ", &reply ) )
      {
/*===*/printf("ERROR -- Exiting\n");
/*===*/exit (-1);
      }
      else  strcpy( stream_name, reply );
    if ( zipview_Query_File_Name( ZipView, "Enter Raster file-name: ", &reply ) )
      {
/*===*/printf("ERROR -- Exiting\n");
/*===*/exit (-1);
      }
      else  strcpy( raster_name, reply );
    zipview_Announce( ZipView, "Thanks." );
    if ( lt_Read_Visuals( self->data, stream_name, raster_name ) )
      {
      zipview_Query( ZipView, "Error Reading Files. Continue?: ", "Y", &reply );
      if ( reply  &&  (*reply == 'n' || *reply == 'N' ) )
	exit (-1);
      }
      else satisified = true;
      }
    zipview_Use_Working_Pane_Cursors( ZipView );
    }
  zipview_Set_Pane_Stream( ZipView, ForegroundPane, StreamLocal = self->data->foreground_stream );
  zipview_Set_Pane_Object_Width( ZipView, ForegroundPane, BackgroundWidth );
  zipview_Set_Pane_Object_Height( ZipView, ForegroundPane, BackgroundHeight );
  if ( Image = zip_Image( Zip, "Chains" ) )
    { DEBUG(Chains Image Exists);
    }
    else
    { DEBUG(Chains Image Non-Existant);
    root_image = zip_Image( Zip, "ZIP_ROOT_IMAGE" );
    if ( status = zip_Create_Inferior_Image( Zip, &Image, "Chains", StreamLocal, root_image ) )
      { DEBUG(ERROR -- Create 'Chains' Image);
/*===*/printf("ERROR -- Failed to create 'Chains' Image (Status %d)\n",status );
      }
    }
  Buttons = suite_Create( buttons, self );
  suite_Apply( Buttons, Detect, self, 0 );
  suite_LinkTree( Buttons, self );
  zipview_Use_Normal_Pane_Cursors( ZipView );
  OUT(Initialize);
  return  status;
  }

void
ltv__ObservedChanged( self, changed, value )
  register struct ltv	     *self;
  register struct observable *changed;
  register long		      value;
  {
  IN(ltv_ObservedChanged);
  ltv_WantUpdate( self, self );
  OUT(ltv_ObservedChanged);
  }

void
ltv__Update( self )
  register struct ltv	     *self;
  {
  IN(ltv_Update);

  OUT(ltv_Update);
  }

struct view *
ltv__Hit( self, action, x, y, clicks )
  register struct ltv	     *self;
  register enum view_MouseAction      action;
  register long		      x, y, clicks;
  {
  long			      x_delta, y_delta;
  register struct view	     *hit = (struct view *) self;

  IN(ltv_Hit);
  if ( x < 0 ) x = 0;
  if ( y < 0 ) y = 0;
  if ( x > Left+Width ) x = Left+Width;
  if ( y > Top+Height ) y = Top+Height;
  if ( y < ButtonTop  &&  !InputFocus  &&  action == view_LeftDown )
    ltv_WantInputFocus( self, self );
  if ( InputFocus )
    { DEBUG(InputFocus);
    if ( y > ButtonTop )
      { DEBUG(Buttons Hit);
      hit = suite_Hit( Buttons, action,
	 suite_EnclosedXToLocalX( Buttons, x ),
	 suite_EnclosedYToLocalY( Buttons, y ), clicks );
      }
    else
    {
    switch ( action )
      {
      case view_LeftDown:
	Clear_Chain_Names( self );
      case view_LeftMovement:
      case view_LeftUp:
        if ( Building )
	  { DEBUG(Building);
	  Build_Chain( self, action, x, y, clicks );
	  }
	  else
	  { DEBUG(NotBuilding);
	  Passivate( self, end_code );
	  if ( Modifying  ||  (action == view_LeftDown  &&  
		(Figure = zipview_Which_Pane_Figure( ZipView, x, y, ForegroundPane ))) )
	    { DEBUGst(Figure-name, Figure->zip_figure_name);
	    Modify_Chain( self, action, x, y, clicks );
	    }
	    else
	    {
	    zipview_Announce( ZipView, " " );
	    Track_Enclosure( self, action, x, y, clicks );
	    }
	  }
	if ( Figure  &&  !Building )
	  {
	  Activate( self, delete_code );
	  Activate( self, rename_code );
	  }
	  else
	  {
	  Passivate( self, delete_code );
	  Passivate( self, rename_code );
	  }
        break;
      case view_RightDown:
        Cancel_Enclosure( self );
        zipview_Initiate_Panning( ZipView, ForegroundPane, x, y, 0 );
        break;
      case view_RightMovement:
        zipview_Continue_Panning( ZipView, ForegroundPane, x, y );
        break;
      case view_RightUp:
        zipview_Terminate_Panning( ZipView, ForegroundPane, x, y, &x_delta, &y_delta, 0 );
        if ( abs(x_delta)  ||  abs(y_delta) )
	  {
          if ( !ForegroundPanning )
	    {
            BackgroundXOffset += x_delta;
            BackgroundYOffset += y_delta;
	    }
	    else  zipview_Clear_Pane( ZipView, ForegroundPane );
          zipview_Pan_Pane( ZipView, ForegroundPane, x_delta, -y_delta );
          Show_Background( self );
	  }
        zipview_Use_Normal_Pane_Cursors( ZipView );
        break;
      }
    }
    }
  OUT(ltv_Hit);
  return  hit;
  }

static
Build_Chain( self, action, x, y, clicks )
  register struct ltv	     *self;
  register enum view_MouseAction      action;
  register long		      x, y, clicks;
  {
  register long		      X, Y, status = 0;
  register boolean	      neighbor = false;
  zip_type_figure	      neighbor_figure;
  zip_type_point	      neighbor_x, neighbor_y;
  long			      neighbor_point;
  char			      msg[512], *reply;

  IN(Build_Chain);
  X = zipview_X_Pixel_To_Point( ZipView, ForegroundPane, NULL, x );
  Y = zipview_Y_Pixel_To_Point( ZipView, ForegroundPane, NULL, y );
  if ( action != view_LeftMovement )
    neighbor = Neighbor( self, x, y, &neighbor_figure, &neighbor_x, &neighbor_y, &neighbor_point );
  switch ( action )
    {
    case view_LeftDown:
      DEBUG(LeftDown);
      zipview_Set_Pane_Cursor( ZipView, ForegroundPane, invisible_cursor, "aptcursors20" );
      if ( Build )
	{ DEBUG(Begin Build);
	Build = false;
	InitialX = x;  InitialY = y;
	Clear_Chain_Names( self );
	if ( status = zip_Create_Figure( Zip, &Figure, NULL, zip_poly_line_figure, Image, NULL ) )
	  { DEBUGdt(ERROR,status);
	  Figure = NULL;
	  }
	  else
	  { DEBUG(Success);
	  Point = 2;
	  if ( neighbor )
	    { DEBUG( Snuggle to Neighbor);
	    sprintf( msg, "Beginning At Chain '%s'", zip_Figure_Name( Zip, neighbor_figure ) );
	    zipview_Announce( ZipView, msg );
	    X = neighbor_x;
	    Y = neighbor_y;
	    }
	  zip_Set_Figure_Point( Zip, Figure, 1, X, Y );
	  zip_Set_Figure_Point( Zip, Figure, 2, X, Y );
	  }
	}
	else
	{ DEBUG(Continue Building);
	if ( Figure )
  	  ++Point;
	}
      break;
    case view_LeftMovement:
      DEBUG(LeftMovement);
      break;
    case view_LeftUp:
      DEBUG(LeftUp);
      zipview_Set_Pane_Cursor( ZipView, ForegroundPane, build_cursor, "icon12" );
      if ( Figure  &&  neighbor )
	{
	if ( neighbor_figure == Figure  &&  Point > 3  &&
	     abs(x - InitialX) < tolerance  &&  abs(y - InitialY) < tolerance )
	  { DEBUG(Snuggle to own Origin Point);
	  X = Figure->zip_figure_point.zip_point_x;
	  Y = Figure->zip_figure_point.zip_point_y;
	  while ( zipview_Query( ZipView, "At Current Chain Origin.  End Chain?: ", "Y", &reply ) );
	  if ( reply  &&  (*reply == 'Y' || *reply == 'y' ) )
	    End_Chain( self );
	    else  zipview_Announce( ZipView, " ");
	  }
	  else
	  if ( neighbor_figure != Figure )
	    { DEBUG(Snuggle to Neighbor);
	    X = neighbor_x;
	    Y = neighbor_y;
	    sprintf( msg, "At Chain '%s'.  End Current Chain?: ",
		     zip_Figure_Name( Zip, neighbor_figure ) );
	    while ( zipview_Query( ZipView, msg, "Y", &reply ) );
	    if ( reply  &&  (*reply == 'Y' || *reply == 'y' ) )
	      End_Chain( self );
	      else  zipview_Announce( ZipView, " ");
	    }
	}
      break;
    }
  if ( status == 0  &&  Figure )
    {
    zipview_Set_Pane_Painting_Mode( ZipView, ForegroundPane, zipview_paint_inverted );
    zipview_Draw_Figure( ZipView, Figure, ForegroundPane );
    zip_Set_Figure_Point( Zip, Figure, Point, X, Y );
    zipview_Draw_Figure( ZipView, Figure, ForegroundPane );
    zipview_Set_Pane_Painting_Mode( ZipView, ForegroundPane, NULL );
    }
  OUT(Build_Chain);
  }

 /*=========*/
static int
Which_Figure_Point( self, figure, pane, x, y )
  register struct ltv	     *self;
  register zip_type_figure    figure;
  register zip_type_pane      pane;
  register zip_type_pixel     x, y;
  {
  register long		      point;
  static struct zipobject    *PO;

  IN(Which_Figure_Point);
  if ( PO == NULL )
    {
    PO = (struct zipobject *)class_NewObject( "zipoplin" );
    zipobject_Set_Data_Object( PO, Zip );
    zipobject_Set_View_Object( PO, ZipView );
    }
  point = zipobject_Proximate_Object_Points( PO, figure, pane, x, y );
  DEBUGdt(Point,point);
  OUT(Which_Figure_Point);
  return  point;
  }


static
Modify_Chain( self, action, x, y, clicks )
  register struct ltv	     *self;
  register enum view_MouseAction      action;
  register long		      x, y, clicks;
  {
  register long		      X, Y, status = 0;
  static long		      down_x, down_y,
			      down_X, down_Y, moved;

  IN(Modify_Chain);
  X = zipview_X_Pixel_To_Point( ZipView, ForegroundPane, NULL, x );
  Y = zipview_Y_Pixel_To_Point( ZipView, ForegroundPane, NULL, y );
  switch ( action )
    {
    case view_LeftDown:
      DEBUG(LeftDown);
      Cancel_Enclosure( self );
      moved = false;
      down_x = x;  down_y = y;
      zipview_Set_Pane_Cursor( ZipView, ForegroundPane, invisible_cursor, "aptcursors20" );
      zipview_Announce( ZipView, " " );
      if ( Point = /*===zipv==*/Which_Figure_Point( self, Figure, ForegroundPane, x, y ) )
	{
        Modifying = Modified = true;
        Show_Chain_Names( self );
        if ( Point == 1 )
	  {
          down_X = Figure->zip_figure_point.zip_point_x;
	  down_Y = Figure->zip_figure_point.zip_point_y;
	  }
	  else
	  {
          down_X = Figure->zip_figure_points->zip_points[Point-2].zip_point_x;
          down_Y = Figure->zip_figure_points->zip_points[Point-2].zip_point_y;
	  }
	}
      break;
    case view_LeftMovement:
      DEBUG(LeftMovement);
      if ( abs(x - down_x) > tolerance  ||  abs(y - down_y) > tolerance )
	moved = true;
      break;
    case view_LeftUp:
      DEBUG(LeftUp);
      zipview_Set_Pane_Cursor( ZipView, ForegroundPane, normal_cursor, "aptcursors20" );
      Modifying = false;
      if ( abs(x - down_x) < tolerance  &&  abs(y - down_y) < tolerance )
	{X = down_X;  Y = down_Y;}
      break;
    }
  if ( status == 0  &&  Figure  &&  Point )
    {
    zipview_Set_Pane_Painting_Mode( ZipView, ForegroundPane, zipview_paint_inverted );
    zipview_Draw_Figure( ZipView, Figure, ForegroundPane );
    if ( moved )
      zip_Set_Figure_Point( Zip, Figure, Point, X, Y );
    zipview_Draw_Figure( ZipView, Figure, ForegroundPane );
    zipview_Set_Pane_Painting_Mode( ZipView, ForegroundPane, NULL );
    }
  OUT(Modify_Chain);
  }

static
Track_Enclosure( self, action, x, y, clicks )
  register struct ltv	     *self;
  register enum view_MouseAction      action;
  register long		      x, y, clicks;
  {
  IN(Track_Enclosure);
  Clear_Enclosure( self );
  switch ( action )
    {
    case view_LeftDown:
      Tracking = true;
      EnclosureExposed = false;
      zipview_Set_Pane_Cursor( ZipView, ForegroundPane, enclosure_cursor, "aptcursors20" );
      EnclosureLeft = x;
      EnclosureTop = y;
      EnclosureWidth = EnclosureHeight = 0;
      break;
    case view_LeftMovement:
      if ( Tracking  &&
	   (abs(x - EnclosureLeft) > tolerance  ||  abs(y - EnclosureTop) > tolerance)  )
	{
        EnclosureExposed = true;
        EnclosureWidth  = x - EnclosureLeft;
        EnclosureHeight = y - EnclosureTop;
	}
      break;
    case view_LeftUp:
      Tracking = false;
      zipview_Set_Pane_Cursor( ZipView, ForegroundPane, normal_cursor, "aptcursors20" );
      if ( EnclosureExposed )
	{
        EnclosureWidth  = x - EnclosureLeft;
        EnclosureHeight = y - EnclosureTop;
        if ( EnclosureWidth < 0 )
	  {
	  EnclosureLeft += EnclosureWidth;
	  EnclosureWidth = abs(EnclosureWidth);
	  }
        if ( EnclosureHeight < 0 )
	  {
	  EnclosureTop += EnclosureHeight;
	  EnclosureHeight = abs(EnclosureHeight);
	  }
	}
      break;
    }
  Draw_Enclosure( self );
  OUT(Track_Enclosure);
  }

static
Cancel_Enclosure( self )
  register struct ltv	     *self;
  {
  IN(Cancel_Enclosure);
  Clear_Enclosure( self );
  EnclosureExposed = false;
  EnclosureLeft = EnclosureTop = EnclosureWidth = EnclosureHeight = 0;
  OUT(Cancel_Enclosure);
  }

static
Clear_Enclosure( self )
  register struct ltv	     *self;
  {
  IN(Clear_Enclosure);
  Invert_Enclosure( self );
  OUT(Clear_Enclosure);
  }

static
Draw_Enclosure( self )
  register struct ltv	     *self;
  {
  IN(Draw_Enclosure);
  Invert_Enclosure( self );
  OUT(Draw_Enclosure);
  }

static
Invert_Enclosure( self )
  register struct ltv	     *self;
  {
  IN(Invert_Enclosure);
  if ( EnclosureExposed )
    {
    ltv_SetTransferMode( self, graphic_INVERT );
    ltv_DrawRectSize( self, EnclosureLeft, EnclosureTop, EnclosureWidth, EnclosureHeight );
    ltv_DrawRectSize( self, EnclosureLeft-1, EnclosureTop-1, EnclosureWidth+2, EnclosureHeight+2 );
    ltv_FlushGraphics( self );
    }
  OUT(Invert_Enclosure);
  }

/*******************************************************************\

    Neighbor is recognized iff the point nearest to the pixel x,y is
    either a Start or and End Point of that neighbor.

\*******************************************************************/
static
Neighbor( self, x, y, figure, X, Y, point )
  register struct ltv	     *self;
  register zip_type_pixel     x, y;
  register zip_type_figure   *figure;
  register zip_type_point    *X, *Y;
  register long		     *point;
  {
  register long		      status = false;

  IN(Neighbor);
  *point = 0;
  if ( *figure = zipview_Which_Pane_Figure( ZipView, x, y, ForegroundPane ) )
    { DEBUGst(Near Figure,(*figure)->zip_figure_name);
    if ( *point = Which_Figure_Point( self, *figure, ForegroundPane, x, y ) )
      {
/*===zipview_Figure_Point...*/
    if ( *point == 1 )
      {
      status = true;
      *X = (*figure)->zip_figure_point.zip_point_x;
      *Y = (*figure)->zip_figure_point.zip_point_y;
      }
      else
      {
      if ( (*figure)->zip_figure_points->zip_points_count == (*point - 1)  )
	{
        status = true;
        *X = (*figure)->zip_figure_points->zip_points[*point-2].zip_point_x;
        *Y = (*figure)->zip_figure_points->zip_points[*point-2].zip_point_y;
	}
      }
/*===*/
      }
    }
  OUT(Neighbor);
  return status;
  }

static void
Normalize_Command( self )
  register struct ltv	     *self;
  {
  IN(Normalize_Command);
  Cancel_Enclosure( self );
  zipview_Normalize_Pane( ZipView, ForegroundPane );
  BackgroundXOffset = BackgroundYOffset = 0;
  zipview_Clear_Pane( ZipView, ForegroundPane );
  Show_Background( self );
  OUT(Normalize_Command);
  }

static void
Zoom_In_Command( self )
  register struct ltv	     *self;
  {
  IN(Zoom_In_Command);
  Cancel_Enclosure( self );
  zipview_Set_Pane_Zoom_Level( ZipView, ForegroundPane,
	    zipview_Pane_Zoom_Level( ZipView, ForegroundPane ) + 1 );
  if ( BackgroundExposed )
    Show_Background( self );
    else
    zipview_Display_Pane( ZipView, ForegroundPane );
  OUT(Zoom_In_Command);
  }

static void
Zoom_Out_Command( self )
  register struct ltv	     *self;
  {
  IN(Zoom_Out_Command);
  Cancel_Enclosure( self );
  if ( zipview_Pane_Zoom_Level( ZipView, ForegroundPane ) > 0 )
    {
    zipview_Set_Pane_Zoom_Level( ZipView, ForegroundPane,
	    zipview_Pane_Zoom_Level( ZipView, ForegroundPane ) - 1 );
    if ( BackgroundExposed )
      Show_Background( self );
      else
      zipview_Display_Pane( ZipView, ForegroundPane );
    }
    else zipview_Announce( ZipView, "Already at Zoom Level Zero" );
  OUT(Zoom_Out_Command);
  }

static void
Scale_Pane( self, scale )
  register struct ltv	      *self;
  register float	       scale;
  {
/*  register float	       x, y; */

  IN(Scale_Pane);
  DEBUGgt( Scale, scale);
  Cancel_Enclosure( self );
  zipview_Set_Pane_Scale( ZipView, ForegroundPane,
	    zipview_Pane_Scale( ZipView, ForegroundPane ) + scale );
/*===
  x = zipview_X_Pixel_To_Point( ZipView, ForegroundPane, NULL,
    zipview_Pane_Left( ZipView, ForegroundPane ) +
		      (zipview_Pane_Width( ZipView, ForegroundPane )/2) );
  y = zipview_Y_Pixel_To_Point( ZipView, ForegroundPane, NULL,
    zipview_Pane_Top( ZipView, ForegroundPane ) +
		     (zipview_Pane_Height( ZipView, ForegroundPane )/2) );
  zipview_Scale_Pane_To_Point( ZipView, ForegroundPane, x, y,
    Scale, zip_center|zip_middle );
===*/
  if ( BackgroundExposed )
    Show_Background( self );
    else
    zipview_Display_Pane( ZipView, ForegroundPane );
  OUT(Scale_Pane);
  }

static void
Scale_Normal_Command( self )
  register struct ltv	      *self;
  {
  zipview_Set_Pane_Scale( ZipView, ForegroundPane, 1.0 );
  Scale_Pane( self, 0.0 );
  }

static void
Scale_Smaller_Command( self )
  register struct ltv	      *self;
  {  Scale_Pane( self, -0.01 );  }

static void
Scale_Smaller_10_Command( self )
  register struct ltv	      *self;
  {  Scale_Pane( self, -0.1 );  }

static void
Scale_Larger_Command( self )
  register struct ltv	      *self;
  {  Scale_Pane( self, 0.01 );  }

static void
Scale_Larger_10_Command( self )
  register struct ltv	      *self;
  {  Scale_Pane( self, 0.1 );  }

static void
Scale_Half_Command( self )
  register struct ltv	      *self;
  {
  IN(Scale_Half_Command);
  zipview_Set_Pane_Scale( ZipView, ForegroundPane,
	zipview_Pane_Scale( ZipView, ForegroundPane ) * 0.5 );
  Scale_Pane( self, 0.0 );
  OUT(Scale_Half_Command);
  }

static void
Scale_Double_Command( self )
  register struct ltv	      *self;
  {
  IN(Scale_Double_Command);
  zipview_Set_Pane_Scale( ZipView, ForegroundPane,
	zipview_Pane_Scale( ZipView, ForegroundPane ) * 2.0 );
  Scale_Pane( self, 0.0 );
  OUT(Scale_Double_Command);
  }

static void
Pan_Foreground_Command( self )
  register struct ltv	     *self;
  {
  IN(Pan_Foreground_Command);
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~pan_foreground) | pan_together );
  ltv_PostMenus( self, Menu );
  ForegroundPanning = true;
  OUT(Pan_Foreground_Command);
  }

static void
Pan_Together_Command( self )
  register struct ltv	     *self;
  {
  IN(Pan_Together_Command);
  Cancel_Enclosure( self );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~pan_together) | pan_foreground );
  ltv_PostMenus( self, Menu );
  ForegroundPanning = false;
  OUT(Pan_Together_Command);
  }

static void
Fit_Command( self )
  register struct ltv	     *self;
  {
  register float	      scale, EW, EH;
  register zip_type_point     x, y;

  IN(Fit_Command);
  if ( EnclosureExposed )
    {
    scale = zipview_Pane_Scale( ZipView, ForegroundPane );
    EW = abs( EnclosureWidth);  EH = abs( EnclosureHeight );
    if ( EW > EH )
      scale *= (1.0 / (EW / zipview_Pane_Width( ZipView, ForegroundPane )));
      else
      scale *= (1.0 / (EH / zipview_Pane_Height( ZipView, ForegroundPane )));
    x = zipview_X_Pixel_To_Point( ZipView, ForegroundPane, NULL, EnclosureLeft + EnclosureWidth/2 );
    y = zipview_Y_Pixel_To_Point( ZipView, ForegroundPane, NULL, EnclosureTop + EnclosureHeight/2 );
    Cancel_Enclosure( self );
    zipview_Scale_Pane_To_Point( ZipView, ForegroundPane, x, y, scale, zip_center | zip_middle );
    Show_Background( self );
    }
    else  zipview_Announce( ZipView, "No Selected Region" );
  OUT(Fit_Command);
  }

static
Show_Chain_Names( self )
  register struct ltv	     *self;
  {
  char			     *left_name, *right_name;

  IN(Show_Chain_Names);
  if ( Figure->zip_figure_name )
    {
    Split_Chain_Name( self, Figure, &right_name, &left_name );
    if ( LeftNameItem )
      ChangeItemCaption( Buttons, LeftNameItem, left_name );
    if ( RightNameItem )
      ChangeItemCaption( Buttons, RightNameItem, right_name );
im_ForceUpdate();
    }
    else  zipview_Announce( ZipView, "Un-named Chain." );
  OUT(Show_Chain_Names);
  }

static
Clear_Chain_Names( self )
  register struct ltv	     *self;
  {
  IN(Clear_Chain_Names);
  if ( LeftNameItem )
    ChangeItemCaption( Buttons, LeftNameItem, "" );
  if ( RightNameItem )
    ChangeItemCaption( Buttons, RightNameItem, "" );
/*===*/ zipview_Announce( ZipView, " " );
  OUT(Clear_Chain_Names);
  }

static
Begin_Chain_Button( self, suite, item, type, action, x, y, clicks )
  register struct ltv	     *self;
  register struct suite	     *suite;
  register struct suite_item    *item;
  register enum view_MouseAction action;
  {
  IN(Begin_Chain_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
    ltv_WantInputFocus( self, self );
    Cancel_Enclosure( self );
    zipview_Set_Pane_Cursor( ZipView, ForegroundPane, build_cursor, "icon12" );
    Building = Build = true;
    zipview_Announce( ZipView, " ");
    Passivate( self, begin_code );
    Activate( self, end_code  );
    Passivate( self, delete_code );
    Passivate( self, rename_code );
    }
  OUT(Begin_Chain_Button);   
  }

static
End_Chain_Button( self, suite, item, type, action, x, y, clicks )
  register struct ltv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(End_Chain_Button);
  if ( Building  &&  type == suite_ItemObject  &&  action == view_LeftUp )
    {
    ltv_WantInputFocus( self, self );
    End_Chain( self );
    }
  OUT(End_Chain_Button);  
  }

static
End_Chain( self )
  register struct ltv	     *self;
  {
  IN(End_Chain);
  Passivate( self, end_code );
  Activate( self, begin_code );
  Activate( self, rename_code );
  Activate( self, delete_code );
  zipview_Set_Pane_Cursor( ZipView, ForegroundPane, normal_cursor, "aptcursors20" );
  Building = false;
  Name_Chain( self );
  OUT(End_Chain);
  }

static
Delete_Chain_Button( self, suite, item, type, action, x, y, clicks )
  register struct ltv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(Delete_Chain_Button);
  Building = false;
  if ( Figure )
    {
    if ( type == suite_ItemObject  &&  action == view_LeftUp )
      {
      ltv_WantInputFocus( self, self );
      zipview_Clear_Figure( ZipView, Figure, ForegroundPane );
      zip_Destroy_Figure( Zip, Figure );
      Figure = NULL;
      Clear_Chain_Names( self );
      Passivate( self, delete_code );
      Passivate( self, rename_code );
      }
    }
    else  zipview_Announce( ZipView, "No Chain Selected." );
  OUT(Delete_Chain_Button);
  }

static long
Rename_Exception( self, facility, status )
  register struct ltv	     *self;
  register long			      status, facility;
  {
  char				      msg[512];
  char				      chain_name[512];
  register char			     *ptr;
  register boolean		      duplicate = true;
  register long			      number = 0;

  IN(Exceptions);
/*===*/self = SELF;
  zip_Set_general_Exception_Handler( Zip, NULL );
  if ( status == zip_duplicate_figure_name )
    {
    strcpy( msg, TentativeName );
    DEBUGst(Original Chain-name,msg);
    if ( ptr = index( msg, '[' ) )
      *ptr = 0;
    while ( duplicate )
      {
      sprintf( chain_name, "%s[%d]", msg, number );
      DEBUGst(New Chain-name,chain_name);
      if ( zip_Set_Figure_Name( Zip, Figure, chain_name ) == zip_ok )
	duplicate = false;
	else  number++;
      }
    sprintf( msg, "Duplicate Chain Name.  Re-named to '%s'", chain_name );
    }
    else
    sprintf( msg, "Rename-Exception  Facility = %d  Status = %d", facility, status );
  zipview_Announce( ZipView, msg );
  zip_Set_general_Exception_Handler( Zip, Exceptions );
  OUT(Exceptions);
  return  0;
  }

static
Rename_Chain_Button( self, suite, item, type, action, x, y, clicks )
  register struct ltv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(Rename_Chain_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
    ltv_WantInputFocus( self, self );
    Name_Chain( self );
    }
  OUT(Rename_Chain_Button); 
  }

static
Left_Chain_Button( self, suite, item, type, action, x, y, clicks )
  register struct ltv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(Left_Chain_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
/*===*/
    ltv_WantInputFocus( self, self );
    suite_NormalizeItem( Buttons, suite_ItemOfDatum( Buttons, left_code  ) );
    }
  OUT(Left_Chain_Button); 
  }

static
Right_Chain_Button( self, suite, item, type, action, x, y, clicks )
  register struct ltv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(Right_Chain_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
/*===*/
    ltv_WantInputFocus( self, self );
    suite_NormalizeItem( Buttons, suite_ItemOfDatum( Buttons, right_code  ) );
    }
  OUT(Right_Chain_Button); 
  }

static
Name_Chain( self )
  register struct ltv	     *self;
  {
  char			     *reply, *left_name, *right_name;

  IN(Name_Chain);
  if ( Figure )
    {
    Split_Chain_Name( self, Figure, &right_name, &left_name );
    while ( zipview_Query( ZipView, "Enter Chain-Right Name: ",
			   right_name, &reply ) );
    strcpy( right_name, reply );
    while ( zipview_Query( ZipView, "Enter Chain-Left Name: ",
			   left_name, &reply ) );
    strcpy( left_name, reply );
    zip_Set_general_Exception_Handler( Zip, Rename_Exception );
    sprintf( TentativeName, "%s,%s", right_name, left_name );
    if ( zip_Set_Figure_Name( Zip, Figure, TentativeName ) == zip_ok )
      {
      Modified = true;
      zipview_Announce( ZipView, " " );
      Show_Chain_Names( self );
      suite_NormalizeItem( Buttons, suite_ItemOfDatum( Buttons, rename_code  ) );
      }
    }
    else  zipview_Announce( ZipView, "No Chain Selected." );
  OUT(Name_Chain);
  }

static
Split_Chain_Name( self, figure, right_name, left_name )
  register struct ltv	     *self;
  register zip_type_figure    figure;
  register char		    **left_name, **right_name;
  {
  static char		      left[257], right[257],
			      full[257], *comma;

  *left = *right = 0;
  *left_name = left;
  *right_name = right;
  strcpy( full, figure->zip_figure_name );
  if ( comma = index( full, ',' ) )
    {
    *comma = 0;
    strcpy( right, full );
    strcpy( left, comma + 1 );
    }
  }

static
Passivate( self, datum )
  register struct ltv	     *self;
  register long		      datum;
  {
  suite_PassivateItem( Buttons, suite_ItemOfDatum( Buttons, datum ) );
  }

static Activate( self, datum )
  register struct ltv	     *self;
  register long		      datum;
  {
  suite_ActivateItem( Buttons, suite_ItemOfDatum( Buttons, datum ) );
  }

static void
Lighten_Background_Command( self )
  register struct ltv	     *self;
  {
  IN(Lighten_Background_Command);
  if ( BackgroundLight == 0 )
    if ( WMWM ) BackgroundLight = '4';  else BackgroundLight = '6';
  if ( BackgroundLight > '1'  &&  BackgroundLight < '9' )
    {
    if ( WMWM )
      BackgroundLight += 1;
      else
      BackgroundLight -= 1;
    Lighten_Background( self );
    }
    else  zipview_Announce( ZipView, "Already as Light as Possible." );
  OUT(Lighten_Background_Command);
  }

static
Lighten_Background( self )
  register struct ltv	     *self;
  {
  if ( WMWM )
    ltv_SetTransferMode( self, graphic_WHITE );
    else
    ltv_SetTransferMode( self, graphic_AND );
  ltv_FillTrapezoid( self, Left, Top, Width,  Left, Bottom-ButtonHeight,Width,
    zipview_Define_Graphic( ZipView,
	zip_Define_Font( Zip, "zipshades16", NULL ), BackgroundLight ) );
  zipview_Redraw_Panes( ZipView );
  if ( EnclosureExposed )
    Draw_Enclosure( self );
  }

static void
Darken_Background_Command( self )
  register struct ltv	     *self;
  {
  IN(Darken_Background_Command);
  BackgroundLight = 0;
  Show_Background( self );
  if ( EnclosureExposed )
    Draw_Enclosure( self );
  OUT(Darken_Background_Command);
  }

static void
Hide_Background_Command( self )
  register struct ltv	     *self;
  {
  IN(Hide_Background_Command);
  BackgroundExposed = false;
  zipview_Clear_Pane( ZipView, ForegroundPane );
  zipview_Redisplay_Panes( ZipView );
  if ( EnclosureExposed )
    Draw_Enclosure( self );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~hide_background) | expose_background );
  ltv_PostMenus( self, Menu );
  OUT(Hide_Background_Command);
  }

static void
Expose_Background_Command( self )
  register struct ltv	     *self;
  {
  IN(Expose_Background_Command);
  BackgroundExposed = true;
  Show_Background( self );
  if ( EnclosureExposed )
    Draw_Enclosure( self );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~expose_background) | hide_background  );
  ltv_PostMenus( self, Menu );
  OUT(Expose_Background_Command);
  }

static
Show_Background( self )
  register struct ltv	     *self;
  {
  register long		      height = (Height - ButtonHeight) - BackgroundTopY;

  IN(Show_Background);
  if ( BackgroundExposed )
    {
    rasterview_InsertViewSize( RasterView, self,
      BackgroundLeftX, BackgroundTopY, BackgroundWidth, height );
    rasterview_FullUpdate( RasterView, view_FullRedraw,
      BackgroundLeftX, BackgroundTopY, BackgroundWidth, height );
    if ( BackgroundLight )
      Lighten_Background( self );
      else
      zipview_Redraw_Panes( ZipView );
    }
    else  zipview_Redraw_Panes( ZipView );
  OUT(Show_Background);
  }

static void
Save_Command( self )
  register struct ltv	     *self;
  {
  char			      msg[512];
  register long		      status;

  IN(Save_Command);
  if ( (status = zip_Write_Stream( Zip, StreamLocal )) == zip_ok )
    sprintf( msg, "Wrote File '%s'", StreamLocal->zip_stream_name );
    else
    sprintf( msg, "Error Writing File '%s'  (%d)", StreamLocal->zip_stream_name, status );
  Modified = false;
  zipview_Announce( ZipView, msg );
  OUT(Save_Command);
  }

static void
Print_Command( self )
  register struct ltv	     *self;
  {
  char			      msg[512];
  register long		      status;
  register FILE		     *file;

  IN(Print_Command);
  zipview_Use_Working_Pane_Cursors( ZipView );
  file = fopen( "tmp_print", "w" );
  zipview_Set_Print_Language( ZipView, "PostScript" );
  zipview_Set_Print_Processor( ZipView, "PostScript" );
  zipview_Set_Print_Level( ZipView, 1 );
  zipview_Set_Print_File( ZipView, file );
  zipview_Set_Print_Dimensions( ZipView, 8.5, 11.0 );
  if ( (status = zipview_Print_Stream( ZipView, StreamLocal, ForegroundPane )) == zip_ok )
    {
    fclose( file );
    system( "print -Tnative tmp_print" );
    sprintf( msg, "Printed File '%s'", StreamLocal->zip_stream_name );
    }
    else
    sprintf( msg, "Error Printing File '%s'  (%d)", StreamLocal->zip_stream_name, status );
  zipview_Announce( ZipView, msg );
  zipview_Use_Normal_Pane_Cursors( ZipView );
  OUT(Print_Command);
  }

static void
Quit_Command( self )
  register struct ltv	     *self;
  {
  static char		     *choices[] =
		{"Cancel", "Save", "Save & Quit", "Quit Anyway", 0};
  long			      response = 0, result;

  IN(Quit_Command);
  if ( Modified )
    {
    result =  message_MultipleChoiceQuestion(
	    self, 0, "Outstanding Modifications:", 0, &response, choices, NULL );
    DEBUGdt(Result,result);
    DEBUGdt(Response,response);
    switch ( response )
      {
      case 0:
	break;
      case 1:
	Save_Command( self );
	break;
      case 2:
	Save_Command( self );
      case 3:
	exit(0);
	break;
      default:
	break;
      }
    }
    else  exit(0);
  OUT(Quit_Command);
  }

static void
Debug_Command( self )
  register struct ltv	     *self;
  {
  IN(Debug_Command);
  debug = !debug;
  zip_Set_Debug( Zip, debug );
  zipview_Set_Debug( ZipView, debug );
  OUT(Debug_Command);
  }


static struct bind_Description	      menu[] =
{
{   "ltv-save",		"\033s",    0,	    "Save~10",		0,  0,
    Save_Command,		"Save",		    NULL},
{   "ltv-print",		"\033p",    0,	    "Print~20",		0,  0,
    Print_Command,		"Print",	    NULL},
{   "ltv-quit",		"\033q",    0,	    "Quit~99",		0,  0,
    Quit_Command,		"Quit",		    NULL},
{   "ltv-debug",		"\033z",    0,	    "DEBUG~88",		0,  0,
    Debug_Command,		"Debug",	    NULL},

{   "ltv-hide-background",	"\033x",    0,	    "Background~20,Hide",0,  hide_background,
    Hide_Background_Command,	"Hide Background",  NULL},
{   "ltv-expose-background",	"\033y",    0,	    "Background~20,Expose",0,  expose_background,
    Expose_Background_Command,	"Expose Background", NULL},
{   "ltv-lighten-background","\033l",    0,	    "Background~20,Lighten",0,  0,
    Lighten_Background_Command,	"Lighten Background", NULL},
{   "ltv-darken-background",	"\033d",    0,	    "Background~20,Darken",0,  0,
    Darken_Background_Command,	"Darken Background", NULL},

{   "ltv-normalize",		"\033b",    0,	    "Pane~30,Normalize~00",0,  0,
    Normalize_Command,		"Normalize",	    NULL},
{   "ltv-zoom_in",		"\033b",    0,	    "Pane~30,Zoom-In~10",	0,  0,
    Zoom_In_Command,		"Zoom In",	    NULL},
{   "ltv-zoom_out",		"\033b",    0,	    "Pane~30,Zoom-Out~11",	0,  0,
    Zoom_Out_Command,		"Zoom Out",	    NULL},
{   "ltv-pan_foreground",	"\033b",    0,	    "Pane~30,Pan Foreground~30",0,  pan_foreground,
    Pan_Foreground_Command,	"Pan Foreground",    NULL},
{   "ltv-pan_together",	"\033b",    0,	    "Pane~30,Pan Together~30",	0,  pan_together,
    Pan_Together_Command,	"Pan Together",	    NULL},
{   "ltv-fit",		"\033b",    0,	    "Pane~30,Fit~40",	0,  0,
    Fit_Command,		"Fit",    NULL},

{   "ltv-scale-normal",	"\033s0",   0,	    "Pane Scale~40,Normal~00",0, 0,
    Scale_Normal_Command,	"...",		    NULL },
{   "ltv-scale-100s",	"\033ssh",  0,	    "Pane Scale~40,100th Smaller~10",0,0,
    Scale_Smaller_Command,	"...",		    NULL },
{   "ltv-scale-10s",		"\033sst",  0,	    "Pane Scale~40,10th Smaller~11", 0,0,
    Scale_Smaller_10_Command,	"...",		    NULL },
{   "ltv-scale-half",        "\033sh",   0,	    "Pane Scale~40,Half Size~12", 0,0,
    Scale_Half_Command,		"...",		    NULL },
{   "ltv-scale-100l",        "\033slh",  0,	    "Pane Scale~40,100th Larger~20", 0, 0,
    Scale_Larger_Command,	"...",		     NULL },
{   "ltv-scale-10l",		"\033slt",  0,	    "Pane Scale~40,10th Larger~21",  0, 0,
    Scale_Larger_10_Command,	"...",		    NULL },
{   "ltv-scale-double",	"\033sd",   0,	    "Pane Scale~40,Double Size~22",  0,0,
    Scale_Double_Command,	"...",		    NULL },
NULL
};


static
Build_Menu()
  {
  IN(Build_Menu);
  bind_BindList( menu, class_keymap, class_menulist, &ltv_classinfo );
  OUT(Build_Menu);
  }

static long
Exceptions( self, facility, status )
  register struct ltv	     *self;
  register long		      status, facility;
  {
  char			      msg[512];

  IN(Exceptions);
/*===*/
self = SELF;
sprintf( msg, "Exception  Status = %d  Facility = %d", status, facility );
zipview_Announce( ZipView, msg );
  OUT(Exceptions);
  return  0;
  }
