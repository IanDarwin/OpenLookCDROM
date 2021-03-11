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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/utility/RCS/schedv.c,v 1.4 1993/01/08 16:36:51 rr2b R6tape $";
#endif


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Sched View-object Program

MODULE	schedv.c

VERSION	0.0

DESCRIPTION
	This is the suite of Methods that support the Sched View-object.

HISTORY
  01/20/89	Created (TCP)
  08/24/89	Upgrade to Suite V1.0 interface (TCP)

END-SPECIFICATION  ************************************************************/

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
#include <sched.ih>
#include <schedv.eh>
#include <suite.ih>
#include <zip.ih>
#include <zipv.ih>
#include <andrewos.h>

static boolean debug=FALSE;
static struct menulist		     *class_menulist;
static struct keymap		     *class_keymap;


static long				      Exceptions();
static /*===*/struct schedv *SELF;

#define  InputFocus		    (self->input_focus)
#define  ScheduleStream		    (self->stream)
#define  Zip			    (self->data->zip)
#define  ZipView		    (self->zipview)
#define  ChartPane		    (self->chart_pane)
#define  Modified		    (self->modified)
#define  Tracking		    (self->tracking)
#define  PendingQuestion	    (self->pending_question)
#define  PendingDuplicate	    (self->pending_duplicate)
#define	 Block			    (&self->block)
#define  Left			    (self->block.left)
#define  Top			    (self->block.top)
#define  Width			    (self->block.width)
#define  Height			    (self->block.height)
#define  Bottom			    (Top + Height)
#define	 ChartBlock		    (&self->chart_block)
#define  ChartLeft		    (self->chart_block.left)
#define  ChartTop		    (self->chart_block.top)
#define  ChartWidth		    (self->chart_block.width)
#define  ChartHeight		    (self->chart_block.height)

#define  CurrentSlotFigure	    (self->current_slot_figure)
#define  PreviousSlotFigure	    (self->previous_slot_figure)
#define  CurrentTextFigure	    (self->current_text_figure)
#define  PreviousTextFigure	    (self->previous_text_figure)
#define  ControlButtons		    (self->control_buttons)
#define  ControlButtonHeight	    (40)
#define  ButtonHeight		    (ControlButtonHeight)
#define  ButtonTop		    (Bottom - ControlButtonHeight)
#define  ControlButtonTop	    (ButtonTop)
#define  Menu			    (self->menu)

static int			    Extend_Button(), Split_Button(), Clear_Button(),
				    Save_Button(), Print_Button(), Quit_Button();
static void			    Quit_Command(), Debug_Command();

static Initialize();
static Handle_Slot_Hit();
static Remember_Slot_Hit();
static Move_Slot();
static Normalize_Previous_Slot_Figure();
static Normalize_Current_Slot_Figure();

static struct bind_Description	      menu[] =
{
{   "schedv-quit",		"\033q",    0,	    "Quit~99",		0,  0,
    Quit_Command,		"Quit",		    NULL},
{   "schedv-debug",		"\033z",    0,	    "DEBUG~88",		0,  0,
    Debug_Command,		"Debug",	    NULL},
NULL
};
suite_Specification		    extend_button[] =
  {
  suite_ItemCaption( "Extend" ),
  suite_ItemHitHandler( Extend_Button ),
  NULL
  };
suite_Specification		    split_button[] =
  {
  suite_ItemCaption( "Split" ),
  suite_ItemHitHandler( Split_Button ),
  NULL
  };
suite_Specification		    clear_button[] =
  {
  suite_ItemCaption( "Clear" ),
  suite_ItemHitHandler( Clear_Button ),
  NULL
  };
suite_Specification		    save_button[] =
  {
  suite_ItemCaption( "Save" ),
  suite_ItemHitHandler( Save_Button ),
  NULL
  };
suite_Specification		    print_button[] =
  {
  suite_ItemCaption( "Print" ),
  suite_ItemHitHandler( Print_Button ),
  NULL
  };
suite_Specification		    quit_button[] =
  {
  suite_ItemCaption( "Quit" ),
  suite_ItemHitHandler( Quit_Button ),
  NULL
  };
suite_Specification		    control_buttons[] =
  {
  suite_ItemCaptionFontName( "andysans10b" ),
/*===
  suite_Item( extend_button ),
  suite_Item( split_button ),
===*/
  suite_Item( clear_button ),
  suite_Item( save_button ),
  suite_Item( print_button ),
  suite_Item( quit_button ),
  suite_Arrangement( suite_Row ),
  NULL
  };
void
schedv__Set_Debug( self, mode )
  register struct schedv		     *self;
  {
  debug = mode;
  }

boolean
schedv__InitializeClass( classID )
  register struct classheader		     *classID;
  {
  IN(schedv_InitializeClass);
  class_menulist = menulist_New();
  class_keymap = keymap_New();
  bind_BindList( menu, class_keymap, class_menulist, &schedv_classinfo );
  OUT(schedv_InitializeClass);
  return TRUE;
  }

boolean 
schedv__InitializeObject( classID, self )
  register struct classheader		      *classID;
  register struct schedv 	      *self;
  {
SELF=self;
  IN(schedv_InitializeObject);
  ChartPane = NULL;
  self->data = NULL;
  ZipView = zipview_New();
  Menu = menulist_DuplicateML( class_menulist, self );
  InputFocus = Modified = Tracking = PendingQuestion = PendingDuplicate = false;
  ScheduleStream = NULL;
  CurrentSlotFigure = CurrentTextFigure =
     PreviousSlotFigure = PreviousTextFigure = NULL;
  OUT(schedv_InitializeObject);
  return  TRUE;
  }

void
schedv__FinalizeObject( classID, self )
  register struct classheader	*classID;
  register struct schedv	*self;
{
  if(Menu) menulist_Destroy(Menu);
  if(ZipView) {
      zipview_UnlinkTree(ZipView);
      zipview_Destroy(ZipView);
      ZipView = NULL;
  }
}

void
schedv__SetDataObject( self, data )
  register struct schedv	      *self;
  register struct sched		      *data;
  {
  self->data = data;
  zipview_SetDataObject( ZipView, Zip );
  zip_Set_general_Exception_Handler( Zip, Exceptions );
  }

void
schedv__ReceiveInputFocus( self )
  register struct schedv	     *self;
  {
  IN(schedv_ReceiveInputFocus);
  InputFocus = true;
  schedv_PostMenus( self, Menu );
  OUT(schedv_ReceiveInputFocus);
  }

void
schedv__LoseInputFocus( self )
  register struct schedv	     *self;
  {
  IN(schedv_LoseInputFocus);
  InputFocus = false;
  OUT(schedv_LoseInputFocus);
  }

void
schedv__FullUpdate( self, type, left, top, width, height )
  register struct schedv	     *self;
  register enum view_UpdateType	      type;
  register long			      left, top, width, height;
  {
  IN(schedv_FullUpdate);
  if ( type == view_FullRedraw || type == view_LastPartialRedraw )
    {
    schedv_GetVisualBounds( self, Block );
    ChartLeft = Left;   ChartTop = Top;
    ChartWidth = Width; ChartHeight = Height - ButtonHeight;
    if ( ChartPane == NULL )
      Initialize( self );
    zipview_InsertViewSize( ZipView, self, ChartLeft, ChartTop, ChartWidth, ChartHeight );
    zipview_FullUpdate( ZipView, type, 0, 0, ChartWidth, ChartHeight );
    suite_InsertViewSize( ControlButtons, self, Left, ControlButtonTop,
					    Width, ControlButtonHeight );
    suite_FullUpdate( ControlButtons, type, 0, 0, Width, ControlButtonHeight );
    }
  OUT(schedv_FullUpdate);
  }

static
Initialize( self )
  register struct schedv	     *self;
  {
  register long			      status = 0;
  char				     *reply;

  IN(Initialize);
  zipview_LinkTree( ZipView, self );
  reply = zipview_GetWindowManagerType( ZipView );
  DEBUGst(Window Manager,reply);
  zipview_Use_Working_Pane_Cursors( ZipView );
  zipview_Create_Pane( ZipView, &ChartPane, "Chart-Pane", ChartBlock, zip_opaque );
  zipview_Set_Pane_Cursor( ZipView, ChartPane, 'A', "aptcsr20" );
  zipview_Set_Pane_Stream( ZipView, ChartPane, ScheduleStream = self->data->stream );
  ControlButtons = suite_Create( control_buttons, self );
  suite_LinkTree( ControlButtons, self );
  zipview_Use_Normal_Pane_Cursors( ZipView );
  OUT(Initialize);
  return  status;
  }

struct view *
schedv__Hit( self, action, x, y, clicks )
  register struct schedv	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register struct view		     *hit = (struct view *) self;
  register zip_type_figure	      figure;

  IN(schedv_Hit);
  DEBUGdt(Action,action);
  if ( PendingQuestion  &&  (action == view_LeftDown  ||  action == view_RightDown) )
    {
    PendingQuestion = false;
    message_CancelQuestion( self );
    zipview_Announce( ZipView, " " );
    Normalize_Current_Slot_Figure( self );
    }
  if ( PendingDuplicate &&  action == view_LeftDown )
    Normalize_Previous_Slot_Figure( self );
  if ( y > ButtonTop )
    { DEBUG(Buttons Hit);
    if ( action == view_RightUp && PreviousTextFigure != NULL )
      Normalize_Previous_Slot_Figure( self );
    hit = suite_Hit( ControlButtons, action,
		suite_EnclosedXToLocalX( ControlButtons, x ),
		suite_EnclosedYToLocalY( ControlButtons, y ), clicks );
    }
    else
    { DEBUG(Chart Hit);
    if ( !InputFocus  &&  (action == view_LeftDown ||
			   action == view_RightDown ||
			   action == view_RightUp) )
      schedv_WantInputFocus( self, self );
    if ( InputFocus )
      { DEBUG(Have InputFocus);
      if ( (figure = zipview_Within_Which_Figure( ZipView, x, y ))  &&
	      zip_Figure_Name( Zip, figure ) )
        switch ( action )
        {
        case view_LeftDown:
	  if ( figure )
	    Handle_Slot_Hit( self, figure );
	  break;
	case view_RightDown:
	  if ( figure )
	    Remember_Slot_Hit( self, figure );
            else
            if ( PendingDuplicate )
              Normalize_Previous_Slot_Figure( self ); 
	  break;
	case view_RightUp:
	  if ( figure )
	    Move_Slot( self, figure );
            else
            if ( PreviousSlotFigure )
              Normalize_Previous_Slot_Figure( self );
          break;
        }
      }
    }
  OUT(schedv_Hit);
  return  hit;
  }

static
Handle_Slot_Hit( self, slot_figure )
  register struct schedv	     *self;
  register zip_type_figure	      slot_figure;
  {
  char				      reply[512], string[512];
  register long			      shade;
  register zip_type_figure	      text_figure;

  IN(Handle_Slot_Hit);
  CurrentSlotFigure = slot_figure;
  DEBUGst(Slot Figure Name,zip_Figure_Name(Zip,slot_figure));
  sprintf( string, "%sText", zip_Figure_Name( Zip, slot_figure) );
  if ( text_figure = zip_Figure( Zip, string ) )
    {  DEBUGst(Text Figure Name,zip_Figure_Name(Zip,text_figure));
    CurrentTextFigure = text_figure;
    shade = zip_Figure_Shade( Zip, slot_figure );
    zip_Set_Figure_Shade( Zip, slot_figure, 100 );
    zipview_Draw_Figure( ZipView, slot_figure, ChartPane );
    sprintf( string, "Enter  %s:  ", zip_Figure_Name( Zip, slot_figure ) );
    PendingQuestion = true;
    if ( message_AskForString( self, 0, string,
	    zip_Figure_Text( Zip, text_figure ), reply, sizeof reply ) )
      { DEBUG(YANK-AWAY);message_CancelQuestion( self );}
      else
      { DEBUGst(Reply,reply);
      PendingQuestion = false;
      if ( strcmp( reply, zip_Figure_Text( Zip, text_figure ) ) )
        {
        Modified = true;
        zipview_Clear_Figure( ZipView, text_figure, ChartPane );
        zip_Set_Figure_Text( Zip, text_figure, reply );
        }
      if ( *reply )
        shade = 10;
        else
        shade = 0;
      zipview_Clear_Figure( ZipView, slot_figure, ChartPane );
      zip_Set_Figure_Shade( Zip, slot_figure, shade );
      zipview_Draw_Figure( ZipView, slot_figure, ChartPane );
      zipview_Draw_Figure( ZipView, text_figure, ChartPane );
      zipview_Announce( ZipView, " " );
      }
    }
  OUT(Handle_Slot_Hit);
  }

static
Remember_Slot_Hit( self, slot_figure )
  register struct schedv	     *self;
  register zip_type_figure	      slot_figure;
  {
  char				      string[512];
  register zip_type_figure	      text_figure;

  IN(Remember_Slot_Hit);
  
  DEBUGst(Slot Figure Name,zip_Figure_Name(Zip,slot_figure));
  sprintf( string, "%sText", zip_Figure_Name( Zip, slot_figure) );
  if ( !PendingDuplicate && (text_figure = zip_Figure( Zip, string )) )
    if ( zip_Figure_Text( Zip, text_figure) ) 
      {  DEBUGst(Text Figure Name,zip_Figure_Name(Zip,text_figure));
      PreviousSlotFigure = slot_figure;
      PreviousTextFigure = text_figure;
      zip_Set_Figure_Shade( Zip, slot_figure, 50 );
      zipview_Draw_Figure( ZipView, slot_figure, ChartPane );
      zipview_Draw_Figure( ZipView, text_figure, ChartPane );
    }
  OUT(Remember_Slot_Hit);
  }
 
static
Move_Slot( self, slot_figure )
  register struct schedv	     *self;
  register zip_type_figure	      slot_figure;
  {
  char				      string[512];
  register zip_type_figure	      text_figure;

  IN(Move_Slot);
  if ( PreviousSlotFigure != NULL )
    {
    if (PreviousSlotFigure == slot_figure )
      PendingDuplicate = true;
    else 
      {
      DEBUGst(Slot Figure Name,zip_Figure_Name(Zip,slot_figure));
      sprintf( string, "%sText", zip_Figure_Name( Zip, slot_figure) );
      if ( text_figure = zip_Figure( Zip, string ) )
        {  DEBUGst(Text Figure Name,zip_Figure_Name(Zip,text_figure));
        Modified = true;
        /* Copy into Target from Previous  */  
        if ( zip_Figure_Text( Zip, text_figure ) )
          zipview_Clear_Figure( ZipView, text_figure, ChartPane );
         zip_Set_Figure_Text( Zip, text_figure, zip_Figure_Text( Zip, PreviousTextFigure )  );
        zipview_Clear_Figure( ZipView, slot_figure, ChartPane );
        zip_Set_Figure_Shade( Zip, slot_figure, 10 );
        zipview_Draw_Figure( ZipView, slot_figure, ChartPane );
        zipview_Draw_Figure( ZipView, text_figure, ChartPane );
        if ( !PendingDuplicate )
          {   
          /* Clear out Previous figure  */ 
          zip_Set_Figure_Text( Zip, PreviousTextFigure, NULL  );
          zipview_Clear_Figure( ZipView, PreviousSlotFigure, ChartPane );
          zip_Set_Figure_Shade( Zip, PreviousSlotFigure, 0 );
          zipview_Draw_Figure( ZipView, PreviousSlotFigure, ChartPane );
          PreviousSlotFigure = NULL;
          PreviousTextFigure = NULL;
	}
      }
      else
        Normalize_Previous_Slot_Figure( self );
    }
  }  /* first if */
  OUT(Move_Slot);
  }

static
Normalize_Previous_Slot_Figure( self )
  register struct schedv	     *self;
  {
  IN(Normalize_Previous_Slot_Figure);
  zipview_Clear_Figure( ZipView, PreviousSlotFigure, ChartPane );
  zip_Set_Figure_Shade( Zip, PreviousSlotFigure, 10 );
  zipview_Draw_Figure( ZipView, PreviousSlotFigure, ChartPane );
  zipview_Draw_Figure( ZipView, PreviousTextFigure, ChartPane );
  PreviousSlotFigure = PreviousTextFigure = NULL;
  PendingDuplicate = false;
  OUT(Normalize_Previous_Slot_Figure);
  }

static
Normalize_Current_Slot_Figure( self )
  register struct schedv	     *self;
  {
  register long			      shade = 0;

  IN(Normalize_Current_Slot_Figure);
  if ( CurrentSlotFigure  &&  zip_Figure_Shade( Zip, CurrentSlotFigure ) == 100 )
    {
    if ( zip_Figure_Text( Zip, CurrentTextFigure ) )
      shade = 10;
    zipview_Clear_Figure( ZipView, CurrentSlotFigure, ChartPane );
    zip_Set_Figure_Shade( Zip, CurrentSlotFigure, shade );
    zipview_Draw_Figure( ZipView, CurrentSlotFigure, ChartPane );
    zipview_Draw_Figure( ZipView, CurrentTextFigure, ChartPane );
    PreviousSlotFigure = PreviousTextFigure = NULL;
    PendingDuplicate = false;
    }
  OUT(Normalize_Current_Slot_Figure);
  }

static
Extend_Button( self, suite, item, type, action, x, y, clicks )
  register struct schedv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(Extend_Button);
  switch ( action )
    {
    case view_LeftDown:

      break;
    case view_RightDown:

      break;
    }
  schedv_WantInputFocus( self, self );
  OUT(Extend_Button);
  }

static
Split_Button( self, suite, item, type, action, x, y, clicks )
  register struct schedv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  IN(Split_Button);
  switch ( action )
    {
    case view_LeftDown:

      break;
    case view_RightDown:

      break;
    }
  schedv_WantInputFocus( self, self );
  OUT(Split_Button);
  }

static
Clear_Button( self, suite, item, type, action, x, y, clicks )
  register struct schedv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  register zip_type_image	      image = zip_Image_Root( Zip, ScheduleStream );
  register zip_type_figure	      figure,
				      slot_figure;
  char				      name[512];

  IN(Clear_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
    figure = zip_Figure_Root( Zip, image );
    while ( figure )
      {
      if ( zip_Figure_Name( Zip, figure )  &&  zip_Figure_Text( Zip, figure ) )
	  { DEBUGst(Text,zip_Figure_Text( Zip, figure ));
	  Modified = true;
	  zip_Set_Figure_Text( Zip, figure, NULL );
	  strcpy( name, zip_Figure_Name( Zip, figure ) );
	  name[strlen( name ) - 4] = 0;
	  DEBUGst(Name,name);
	  slot_figure = zip_Figure( Zip, name );
	  zipview_Clear_Figure( ZipView, slot_figure, ChartPane );
	  zip_Set_Figure_Shade( Zip, slot_figure, 0 );
	  zipview_Draw_Figure( ZipView, slot_figure, ChartPane );
	  }
      if ( (figure = zip_Next_Figure( Zip, figure )) == NULL )
	  {
	  image = zip_Next_Image( Zip, image );
	  figure = zip_Figure_Root( Zip, image );
	  }
      }
    }
  schedv_WantInputFocus( self, self );
  OUT(Clear_Button);
  }

static
Save_Button( self, suite, item, type, action, x, y, clicks )
  register struct schedv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  char				      msg[512];
  register long			      status;

  IN(Save_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
    zipview_Use_Working_Pane_Cursors( ZipView );
    if ( (status = zip_Write_Stream( Zip, ScheduleStream )) == zip_ok )
      sprintf( msg, "Wrote File '%s'", ScheduleStream->zip_stream_name );
      else
      sprintf( msg, "Error Writing File '%s'  (%d)", ScheduleStream->zip_stream_name, status );
    Modified = false;
    zipview_Announce( ZipView, msg );
    zipview_Use_Normal_Pane_Cursors( ZipView );
    }
  schedv_WantInputFocus( self, self );
  OUT(Save_Button);
  }

static
Print_Button( self, suite, item, type, action, x, y, clicks )
  register struct schedv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  char				      msg[512];
  register long			      status;
  register FILE			     *file;

  IN(Print_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
    zipview_Use_Working_Pane_Cursors( ZipView );
    file = fopen( "tmp_print", "w" );
    zipview_Set_Print_Language( ZipView, "PostScript" );
    zipview_Set_Print_Processor( ZipView, "PostScript" );
    zipview_Set_Print_Level( ZipView, 1 );
    zipview_Set_Print_File( ZipView, file );
    zipview_Set_Print_Dimensions( ZipView, 8.5, 11.0 );
    if ( (status = zipview_Print_Stream( ZipView, ScheduleStream, ChartPane )) == zip_ok )
      {
      fclose( file );
      system( "print -Tnative tmp_print" );
      sprintf( msg, "Printed File '%s'", ScheduleStream->zip_stream_name );
      }
      else
      sprintf( msg, "Error Printing File '%s'  (%d)", ScheduleStream->zip_stream_name, status );
    zipview_Announce( ZipView, msg );
    zipview_Use_Normal_Pane_Cursors( ZipView );
    }
  schedv_WantInputFocus( self, self );
  OUT(Print_Button);
  }

static
Quit_Button( self, suite, item, type, action, x, y, clicks )
  register struct schedv	     *self;
  register struct suite		     *suite;
  register struct suite_item	     *item;
  register enum view_MouseAction      action;
  {
  static char			     *choices[] =
		{"Cancel", "Save", "Save & Quit", "Quit Anyway", 0};
  long				       response = 0, result;

  IN(Quit_Button);
  if ( type == suite_ItemObject  &&  action == view_LeftUp )
    {
    if ( Modified )
      {
      result =  message_MultipleChoiceQuestion(
	    self, 0, "Outstanding Modifications:", 0, &response, choices, NULL );
      DEBUGdt(Result,result);
      DEBUGdt(Response,response);
      switch ( response )
        {
        case 0:	    break;
        case 1:	    Save_Button( self, suite, item, type, action, x, y, clicks ); break;
        case 2:	    Save_Button( self, suite, item, type, action, x, y, clicks );
        case 3:	    exit(0);			break;
        default:    break;
        }
      }
      else  exit(0);
    }
  schedv_WantInputFocus( self, self );
  OUT(Quit_Button);
  }

static void
Debug_Command( self )
  register struct schedv	     *self;
  {
  IN(Debug_Command);
  debug = !debug;
  OUT(Debug_Command);
  }

static void
Quit_Command( self )
  register struct schedv	     *self;
  {
  IN(Quit_Command);
  Quit_Button( self, NULL, NULL, suite_ItemObject, view_LeftUp, NULL, NULL, NULL );
  OUT(Quit_Command);
  }

static long
Exceptions( self, facility, status )
  register struct schedv	     *self;
  register long			      status, facility;
  {
  char				      msg[512];

  IN(Exceptions);
/*===*/
self = SELF;
sprintf( msg, "Exception  Status = %d  Facility = %d", status, facility );
zipview_Announce( ZipView, msg );
  OUT(Exceptions);
  return  0;
  }
