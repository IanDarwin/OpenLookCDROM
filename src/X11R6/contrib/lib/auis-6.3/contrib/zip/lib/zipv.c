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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipv.c,v 1.6 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1987
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipv.c	Zip View-object					      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipv.c,v $
 * Revision 1.6  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.4.1.1  1993/02/02  06:55:15  rr2b
 * new R6tape branch
 *
 * Revision 1.4  1993/01/08  16:36:06  rr2b
 * cutting down on duplicate global symbols
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
 * Revision 2.14  1991/09/12  16:44:27  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.13  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.12  1991/08/26  22:31:18  gk5g
 * Patch submitted by Guy Harris:
 * ATK seems not to have made up its mind whether PostScript format in printing is called "postscript" or "PostScript", nor even whether "troff" format is called "troff" or "Troff". Attached are some patches that teach some insets confused about this subject that they're called "PostScript" and "troff", respectively.
 * The patches to "zip" succeeded in improving the quality of the PostScript it generated, by causing it to be properly preceded with "\!" or whatever so that "*roff" would know what to do with it.
 *
 * Revision 2.11  1990/08/21  14:42:48  sg08
 * Add Normalize_Line_Attributes in {Highlight,Normalize}_View
 *
 * Revision 2.10  89/07/13  15:33:10  tom
 * Check for IM existence in Announce;
 * (Use printf if Null).
 * 
 * Revision 2.9  89/05/26  19:34:02  tom
 * Suppress Page feature - till Form inset used.
 * 
 * Revision 2.8  89/02/17  18:09:16  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.7  89/02/08  16:52:50  ghoti
 * change copyright notice
 * 
 * Revision 2.6  89/02/07  21:15:40  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.5  88/11/18  21:13:19  tom
 * Fix null-ptr (SUN3) on Query method.
 * 
 * Revision 2.4  88/11/16  18:27:49  tom
 * Fix Keystate next field -- NULL;
 * Recognize ManualRefresh option.
 * 
 * Revision 2.3  88/10/12  18:38:55  tom
 * Fix enum-clashes warning msg.
 * 
 * Revision 2.2  88/10/11  20:41:27  tom
 * Additional Background-pane facilities.
 * 
 * Revision 2.1  88/09/27  18:20:56  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:53:45  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:48:14  tom
 * Initial revision
 * 
 * Revision 1.5  87/12/02  20:41:05  tom
 * Add Handles to Line-figure;
 * Complete zipview_Finalize freeing.
 * 
 * Revision 1.4  87/11/12  19:14:39  tom
 * Fix handling of Caption building.
 * 
 * Revision 1.3  87/11/11  21:59:41  tom
 * Improve Figure Handles; Add Absolute/Relative feature.
 * 
 * Revision 1.2  87/10/30  14:10:32  tom
 * React to Major Makefile change: remove ifdef on WM.
 * 
 * Revision 1.1  87/10/28  21:42:24  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object

MODULE	zipv.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Zip View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
   Such curiosities need be resolved prior to Project Completion...

HISTORY
  06/16/87	Created (TCP)
  08/06/87	Once more to the breach, dear friends ...
  10/30/87	Remove old ifdef on WM and remove include of mrl.ih (TCP)
  11/09/87	Support "Absolute" as well as "Relative" sizing (TCP)
		Cope with resizing of View-rectangle when not via Menu
		Utilize menu-mask to update Menu-items
  11/12/87	Fix destroying of Null Text figure building (TCP)
  11/25/87	Add more xxx_Destroys to Finalize (TCP)
  12/02/87	Suppress View Menu-card & Flip/Flop Pane Menu items (TCP)
  03/31/88	Revised for ATK (TCP)
  10/12/88	Fix enumeration clash warning (TCP)
  10/19/88	Fix PostKeyState (set keystate->next NULL again) (TCP) 
  10/27/88	Add preference ZipManualRefresh (TCP)
  11/18/88	Check NULL arg in PostKeyState (TCP)
	              Ensure non-null ptr on Query facilities
  05/26/89	Suppress Page feature - till Form inset used (TCP)
  07/12/89	Check for Im existence in Announce (TCP)
   08/16/90	Add Normalize_Line_Attributes in {Highlight,Normalize}_View (SCG)

END-SPECIFICATION  ************************************************************/

#include <stdio.h>
#include "class.h"
#include "environ.ih"
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
#include "complete.ih"
#include "filetype.ih"
#include <ctype.h>
#include "fontdesc.ih"
#include "zip.ih"
#include "zipedit.ih"
#include "zipprint.ih"
#include "zipstat.ih"
#include "zipobj.ih"
#include "zipv.eh"

static boolean debug=FALSE;
#define	 Data			      (self->data_object)
#define	 View			      self
#define	 Editor			      self->edit_object
#define	 Printer		      self->print_object
#define	 DataObjects(i)		      ((*Data->objects)[i])
#define	 Objects(i)		      ((*self->objects)[i])
#define	 Pane			      self->pane
#define  PanningPrecision	      self->panning_precision
#define  CurrentPane		      self->current_pane
#define  CurrentPage		      self->current_page
#define	 Edge			      self->edge
#define  Block			      self->block
#define  BlockTop		      self->block.top
#define  BlockLeft		      self->block.left
#define  BlockHeight		      self->block.height
#define  BlockWidth		      self->block.width
#define  ObjectWidth		      Data->object_width
#define  ObjectHeight		      Data->object_height
#define  DesiredWidth		      Data->desired_view_width
#define  DesiredHeight		      Data->desired_view_height
#define	 Stream			      Data->stream
#define  Editing		      self->states.editing
#define  FirstTime		      self->states.first_time
#define  Abeyant		      self->states.abeyant
#define  Application		      self->states.application
#define  Action			      self->action
#define  Menu			      self->menu
#define  Keystate		      self->keystate
#define  Cursor			      self->cursor
#define	 MouseAction		      self->mouse_action
#define	 MouseX			      self->mouse_x
#define	 MouseY			      self->mouse_y
#define	 MouseClicks		      self->mouse_clicks
#define  InputFocus		      self->states.inputfocus

static struct menulist		     *class_menulist;
static struct keymap		     *class_keymap;
/*===static struct keystate	     *class_keystate;*/

#define  menu_default		     (1<<0)
#define  menu_app		     (1<<1)
#define  menu_edit		     (1<<2)
#define  menu_browse		     (1<<3)
#define  menu_portrait		     (1<<4)
#define  menu_landscape		     (1<<5)

#define  pending_hit		      (1<<1)
#define  pending_redisplay	      (1<<2)
#define  pending_redraw		      (1<<3)
#define  pending_edit		      (1<<4)
#define  pending_browse		      (1<<5)
#define  pending_receiveinputfocus    (1<<6)
#define  pending_loseinputfocus	      (1<<7)
#define  pending_pan		      (1<<8)
#define  pending_scrollbars	      (1<<9)
#define  pending_zoom		      (1<<10)
#define  pending_scale		      (1<<11)

#define  Pending_hit		      Action & pending_hit
#define  Set_pending_hit	      Action |= pending_hit
#define  Reset_pending_hit	      Action ^= pending_hit
#define  Pending_redisplay	      Action & pending_redisplay
#define  Set_pending_redisplay	      Action |= pending_redisplay
#define  Reset_pending_redisplay      Action ^= pending_redisplay
#define  Pending_redraw		      Action & pending_redraw
#define  Set_pending_redraw	      Action |= pending_redraw
#define  Reset_pending_redraw	      Action ^= pending_redraw
#define  Pending_edit		      Action & pending_edit
#define  Set_pending_edit	      Action |= pending_edit
#define  Reset_pending_edit	      Action ^= pending_edit
#define  Pending_browse		      Action & pending_browse
#define  Set_pending_browse	      Action |= pending_browse
#define  Reset_pending_browse	      Action ^= pending_browse
#define  Pending_receiveinputfocus       Action & pending_receiveinputfocus
#define  Set_pending_receiveinputfocus	 Action |= pending_receiveinputfocus
#define  Reset_pending_receiveinputfocus Action ^= pending_receiveinputfocus
#define  Pending_loseinputfocus		 Action & pending_loseinputfocus
#define  Set_pending_loseinputfocus	 Action |= pending_loseinputfocus
#define  Reset_pending_loseinputfocus    Action ^= pending_loseinputfocus
#define  Pending_pan		      Action & pending_pan
#define  Set_pending_pan	      Action |= pending_pan
#define  Reset_pending_pan	      Action ^= pending_pan
#define  Pending_scrollbars	      Action & pending_scrollbars
#define  Set_pending_scrollbars       Action |= pending_scrollbars
#define  Reset_pending_scrollbars     Action ^= pending_scrollbars
#define  Pending_zoom		      Action & pending_zoom
#define  Set_pending_zoom	      Action |= pending_zoom
#define  Reset_pending_zoom           Action ^= pending_zoom
#define  Pending_scale		      Action & pending_scale
#define  Set_pending_scale	      Action |= pending_scale
#define  Reset_pending_scale          Action ^= pending_scale


static Build_Menu();
static Initialize_Printing();
static Pending_Hit();
static Pending_Redisplay();
static Pending_Redraw();
static Pending_Zoom();
static Pending_Scale();
static Pending_Pan();
static Pending_ReceiveInputFocus();
static Pending_LoseInputFocus();
static Pending_Edit();
static Pending_Browse();
static Highlight_View();
static Normalize_View();
static Prepare_Default_Stream();
static Prepare_Default_Pane();


boolean 
zipview__InitializeClass( classID )
  register struct classheader	     *classID;
  {
  IN(zipview_InitializeClass );
  class_menulist = menulist_New();
  class_keymap = keymap_New();
  Build_Menu();
  OUT(zipview_InitializeClass );
  return TRUE;
  }


boolean 
zipview__InitializeObject( classID, self)
  register struct classheader	      *classID;
  register struct zipview	      *self;
  {
  IN(zipview_InitializeObject);
  self->options.manual_refresh =
    environ_GetProfileSwitch( "ZipManualRefresh", false );
  Application = Abeyant = Editing = InputFocus = false;
  Data = NULL;
  Editor = NULL;
  Printer = NULL;
  PaneAnchor = NULL;
  Printing = NULL;
/*===  PaneExceptionHandler = NULL;*/
  PanningPrecision = 0;
  FirstTime = true;
  Pane = CurrentPane = NULL;
  CurrentPage = 1;
  Action = NULL;
  MouseAction = view_NoMouseEvent;
  self->objects = NULL;
  BlockTop = BlockLeft = BlockHeight = BlockWidth = 0;
  self->header.view.imPtr = NULL;/*=== WHY DONE ? ===*/
  Menu = menulist_DuplicateML( class_menulist, self );
  menulist_SetView( Menu, self );
  Keystate = keystate_Create( self, class_keymap );
  Cursor = cursor_Create( self );
  cursor_SetStandard( Cursor, Cursor_Gunsight );
  OUT(zipview_InitializeObject);
  return TRUE;
  }

void
zipview__FinalizeObject( classID, self )
  register struct classheader	      *classID;
  register struct zipview	      *self;
  {
  IN(zipview_FinalizeObject);
  if ( Cursor )	    cursor_Destroy( Cursor );
  if ( Menu )	    menulist_Destroy( Menu );
  if ( Keystate )   keystate_Destroy( Keystate );
/*===*/
  OUT(zipview_FinalizeObject);
  }

void
zipview__PostKeyState( self, keystate )
  register struct zipview	     *self;
  register struct keystate	     *keystate;
  {
  register struct keystate	     *keys;

  IN(zipview_PostKeyState);
  if ( keystate != Keystate  &&  Editing  &&  Editor->keystate )
    { DEBUG(Editor-keystate);
    keys = Editor->keystate;
    }
    else
    { DEBUG(View-keystate);
    keys = Keystate;
    }
  if ( keys  &&  keystate != keys )
    { DEBUG(Different);
    keys->next = NULL;
    if ( keystate )
      keystate_AddBefore( keystate, keys );
    }
  super_PostKeyState( self, keystate );
  OUT(zipview_PostKeyState);
  }

void
zipview__PostMenus( self, menulist )
  register struct zipview	     *self;
  register struct menulist	     *menulist;
  {
  register struct menulist	     *menu = Menu;

  IN(zipview_PostMenus);
  menulist_ClearChain( menu );
  if ( menulist != menu )
    { DEBUG(Different);
    if ( InputFocus )
      menulist_ChainAfterML( menu, menulist, menulist );
      else
      { DEBUG(Not InputFocus);
      menu = menulist;
      if ( Editing  &&  Editor->menu ) /*===dirty===*/
        menulist_ChainAfterML( menu, Editor->menu, Editor->menu );
      }
    }
  super_PostMenus( self, menu );
  OUT(zipview_PostMenus);
  }

void
zipview__SetDataObject( self, data )
  register struct zipview	      *self;
  register struct zip		      *data;
  {
  register int			       i;

  IN(zipview_SetDataObject);
  Data = data;
  self->objects = (struct zipobject *((*)[])) calloc( 28, sizeof(char *) );
  for ( i = 1; DataObjects(i); i++ )
    {
    DEBUGst(Object-name,class_GetTypeName( DataObjects(i) ));
    Objects(i) = (struct zipobject *) 
	class_NewObject( class_GetTypeName( DataObjects(i) ) );
    zipobject_Set_Data_Object( Objects(i), Data );
    zipobject_Set_View_Object( Objects(i), self );
    }
  DEBUGdt(Desired-width, DesiredWidth);
  DEBUGdt(Desired-height, DesiredHeight);
  if ( !(DesiredWidth) )   DesiredWidth  = zipview_default_block_width;
  if ( !(DesiredHeight) )  DesiredHeight = zipview_default_block_height;
  DEBUG(super-SetDataObject);
  super_SetDataObject( self, data );
  OUT(zipview_SetDataObject );
  }

enum view_DSattributes
zipview__DesiredSize( self, given_width, given_height,
		      pass, desired_width, desired_height )
  register struct zipview	     *self;
  register long			      given_width, given_height;
  register enum view_DSpass	      pass;
  register long			     *desired_width, *desired_height;
  {
  register enum view_DSattributes     result = view_WidthFlexible |
					       view_HeightFlexible;

  IN(zipview_DesiredSize);
  DEBUGdt( Given Width, given_width);
  DEBUGdt( Given Height, given_height);
  DEBUGdt( Pass, pass);
  DEBUGdt( Pending-Action,Action);
  *desired_width  = DesiredWidth;
  *desired_height = DesiredHeight;
  DEBUGdt( Desired Width,  *desired_width);
  DEBUGdt( Desired Height, *desired_height);
  DEBUGxt( Result-code, result );
  OUT(zipview_DesiredSize);
  return result;
  }

void
zipview__ObservedChanged( self, changed, value )
  register struct zipview	     *self;
  register struct observable	     *changed;
  register long			      value;
  {
  IN(zipview_ObservedChanged);
  if ( self != (struct zipview *) value )
    {
    DEBUG(Not Self Observed);
    Set_pending_redraw;
    zipview_WantUpdate( self, self );
    }
  OUT(zipview_ObservedChanged);
  }

void
zipview__SetOptions( self, options )
  register struct zipview	      *self;
  register int			       options;
  {
  IN(zipview_SetOptions );
  DEBUGxt( Options, options);
/*===*/
  OUT(zipview_SetOptions );
  }

void 
zipview__ReceiveInputFocus( self )
  register struct zipview	      *self;
  {
  IN(zipview_ReceiveInputFocus );
  DEBUGxt(Action,Action);
  Keystate->next = NULL;
  zipview_PostKeyState( self, Keystate );
  if ( ! Editing )
    {
    menulist_SetMask( Menu, ((Application) ? menu_app : 0) | 
		      menu_default | menu_browse | menu_portrait );
    }
    else
    { DEBUG(Editing);
    Editor->keystate->next = NULL;
    zipview_PostKeyState( self, Editor->keystate );
    }
  cursor_SetStandard( Cursor, Cursor_Arrow );
  zipview_PostCursor( self, &Block, Cursor );
  Set_pending_receiveinputfocus;
  zipview_WantUpdate( self, self );
  zipview_PostMenus( self, Menu );
  OUT(zipview_ReceiveInputFocus );
  }

void
zipview__LoseInputFocus( self )
  register struct zipview	      *self;
  {
  IN(zipview_LoseInputFocus );
  DEBUGxt(Action,Action);
  if ( ! Abeyant )
    {
    if ( Editing )
      Pending_Browse( self );
    zipview_RetractViewCursors( self, self );
    cursor_SetStandard( Cursor, Cursor_Gunsight );
    zipview_PostCursor( self, &Block, Cursor );
    Set_pending_loseinputfocus;
    zipview_WantUpdate( self, self );
    }
  InputFocus = false;
  OUT(zipview_LoseInputFocus );
  }

void
zipview__WantUpdate( self, requestor )
  register struct zipview	     *self;
  register struct view		     *requestor;
  {
  IN(zipview_WantUpdate);
/*===*/
  super_WantUpdate( self, requestor );
/*===*/
  OUT(zipview_WantUpdate);
  }

void 
zipview__FullUpdate( self, type, left, top, width, height )
  register struct zipview	     *self;
  register enum view_UpdateType	      type;
  register long			      left, top, width, height;
  {
  IN(zipview_FullUpdate);
  DEBUGdt( Type, type);
  DEBUGlt( Left,  left);   DEBUGlt( Top,   top);
  if ( type == view_FullRedraw || type == view_LastPartialRedraw )
    {
    zipview_GetLogicalBounds( self, &Block );
    DesiredWidth  = BlockWidth;
    DesiredHeight = BlockHeight;
    if ( FirstTime )
      {
      FirstTime = false;
      if ( StreamAnchor == NULL )
	Prepare_Default_Stream( self );
      if ( PaneAnchor == NULL )
	Prepare_Default_Pane( self );
      }
    if ( Editing )
      zipedit_Redisplay_Panes( Editor, Pane );
      else
      zipview_Redisplay_Panes( self );
    zipview_Update( self );
    }
  else
  if ( type == view_MoveNoRedraw )
    {
    DEBUG(Move-No-Redraw);
    zipview_Use_Normal_Pane_Cursors( self );
    }
  if ( InputFocus )
    Highlight_View( self );
    else
    {
    cursor_SetStandard( Cursor, Cursor_Gunsight );
    zipview_PostCursor( self, &Block, Cursor );
    }
  OUT(zipview_FullUpdate);
  }

void 
zipview__Update( self )
  register struct zipview	   *self;
  {
  IN(zipview_Update);
  DEBUGxt(Action,Action);
  if ( Pending_hit )		    Pending_Hit( self );
  if ( Pending_redisplay )	    Pending_Redisplay( self );
  if ( Pending_redraw )		    Pending_Redraw( self );
  if ( Pending_pan )		    Pending_Pan( self );
  if ( Pending_zoom )		    Pending_Zoom( self );
  if ( Pending_scale )		    Pending_Scale( self );
  if ( Pending_receiveinputfocus )  Pending_ReceiveInputFocus( self );
  if ( Pending_loseinputfocus )	    Pending_LoseInputFocus( self );
  if ( Pending_edit )		    Pending_Edit( self );
  if ( Pending_browse )		    Pending_Browse( self );
  if ( Editing )		    zipedit_Update( Editor );
  OUT(zipview_Update);
  }

struct view *
zipview__Hit( self, action, x, y, clicks )
  register struct zipview	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register zip_type_pane	      pane;
  register zip_type_figure	      figure;
  register struct view		     *view = NULL;

  IN(zipview_Hit );
  if ( (CurrentPane = pane = zipview_Which_Pane( self, x, y ))  &&  !Abeyant )
    {
    if ( action == view_LeftDown  ||  action == view_RightDown )
      zipview_Announce( View, " " );
    MouseAction = action;  MouseX = x;  MouseY = y;  MouseClicks = clicks;
    if ( pane->zip_pane_hit_processor )
      { DEBUG(>>> Hit Processor);
      view = (struct view *)
	(*pane->zip_pane_hit_processor)( pane->zip_pane_hit_processor_anchor,
					 pane, action, x, y, clicks );
      DEBUG(<<< Hit Processor);
      }
    else
    if ( Editing )
      {
      view = (struct view *) self;
      zipedit_Accept_Hit( Editor, pane, action, x, y, clicks );
      }
    else
    if ( figure = zipview_Within_Which_Figure( self, x, y ) )
      {  DEBUG(Within a Figure);
      if ( !InputFocus  ||  action == view_LeftDown )
        view = zipobject_Object_Hit( Objects(figure->zip_figure_type),
				   figure, action, x, y, clicks );
      }
    if ( view == NULL )
      {
      if ( InputFocus )
	{ DEBUG(Have InputFocus);
	if ( action == view_RightDown  ||  action == view_RightMovement  ||
	     action == view_RightUp )
	  { DEBUG(RightButton);
	  Set_pending_hit;
	  view = (struct view *) self;
	  zipview_Update( self );
	  }
	}
	else
	{ DEBUG(Not InputFocus);
	if ( action == view_LeftDown ) 
	  { DEBUG(Grab InputFocus);
	  zipview_WantInputFocus( self, self );
	  view = (struct view *) self;
	  }
	}
      }
    }
    else  /* Abeyant...*/
    {
    if ( Editing  &&  CurrentPane )
      {
      view = (struct view *) self;
      zipedit_Accept_Hit( Editor, pane, action, x, y, clicks );
      }
      else  DEBUG(No Pane Hit);
    }
  MouseAction = view_NoMouseEvent;
  OUT(zipview_Hit );
  return  view;
  }

void
zipview__Print( self, file, processor, format, top_level )
  register struct zipview	     *self;
  register FILE			     *file;
  register char			     *processor;
  register char			     *format;
  register boolean		      top_level;
  {
  register float		      inch_width, inch_height;

  IN(zipview_Print);
  DEBUGst(Processor, processor);
  DEBUGst(Format, format);
  DEBUGdt(Level, top_level);
  zipview_Set_Print_Processor( self, processor );
  zipview_Set_Print_Language( self, format );
  zipview_Set_Print_Level( self, top_level );
  zipview_Set_Print_File( self, file );
/*===*/
#define pixels_per_inch	80.0
  if ( !(inch_width = DesiredWidth) )
    inch_width = BlockWidth;
  if ( !(inch_height = DesiredHeight) )
    inch_height = BlockHeight;
  if ( top_level )
    {
    if ( Printing->zip_printing_orientation == zip_landscape )
      zipview_Set_Print_Dimensions( self, 10.5, 7.5 );
      else
      zipview_Set_Print_Dimensions( self, 7.5, 10.5 );
    }
    else
    zipview_Set_Print_Dimensions( self, inch_width  / pixels_per_inch,
				        inch_height / pixels_per_inch );
  if ( PaneAnchor == NULL )
    Prepare_Default_Pane( self );
  zipview_Print_Pane( self, Pane );
/*===*/
  OUT(zipview_Print);
  }

long
zipview__Set_Print_Language( self, language )
  register struct zipview		 *self;
  register char				 *language;
  {
  register long				  status;

  IN(zipview_Set_Print_Language);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Language( Printer, language );
  OUT(zipview_Set_Print_Language);
  return  status;
  }

long
zipview__Set_Print_Processor( self, processor )
  register struct zipview		 *self;
  register char				 *processor;
  {
  register long				  status;

  IN(zipview_Set_Print_Processor);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Processor( Printer, processor );
  OUT(zipview_Set_Print_Processor);
  return  status;
  }

long
zipview__Set_Print_Level( self, level )
  register struct zipview		 *self;
  register long				  level;
  {
  register long				  status;

  IN(zipview_Set_Print_Level);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Level( Printer, level );
  OUT(zipview_Set_Print_Level);
  return  status;
  }

long
zipview__Set_Print_File( self, file )
  register struct zipview		 *self;
  register FILE				 *file;
  {
  register long				  status;

  IN(zipview_Set_Print_File);
  Initialize_Printing( self );
  status = zipprint_Set_Print_File( Printer, file );
  OUT(zipview_Set_Print_File);
  return  status;
  }

long
zipview__Set_Print_Resolution( self, resolution )
  register struct zipview		 *self;
  register long				  resolution;
  {
  register long				  status;

  IN(zipview_Set_Print_Resolution);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Resolution( Printer, resolution );
  OUT(zipview_Set_Print_Resolution);
  return  status;
  }

long
zipview__Set_Print_Dimensions( self, inch_width, inch_height )
  register struct zipview		 *self;
  register float			  inch_width, inch_height;
  {
  register long				  status;

  IN(zipview_Set_Print_Dimensions);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Dimensions( Printer, inch_width, inch_height );
  OUT(zipview_Set_Print_Dimensions);  
  return  status;
  }

long
zipview__Set_Print_Coordinates( self, x_origin, y_origin, width, height )
  register struct zipview		 *self;
  register zip_type_percent		  x_origin, y_origin, width, height;
  {
  int					  status = zip_success;

  IN(zipview_Set_Print_Coordinates);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Coordinates( Printer, x_origin, y_origin, width, height );
  OUT(zipview_Set_Print_Coordinates);
  return status;
  }

long
zipview__Set_Print_Orientation( self, orientation )
  register struct zipview		 *self;
  register long				  orientation;
  {
  int					  status = zip_success;

  IN(zipview_Set_Print_Orientation);
  Initialize_Printing( self );
  status = zipprint_Set_Print_Orientation( Printer, orientation );
  OUT(zipview_Set_Print_Orientation);
  return status;
  }

long
zipview__Print_Figure( self, figure, pane )
  register struct zipview	       *self;
  register zip_type_figure		figure;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(zipview_Print_Figure);
  Initialize_Printing( self );
  status = zipprint_Print_Figure( Printer, figure, pane );
  OUT(zipview_Print_Figure);
  return  status;
  }

long
zipview__Print_Image( self, image, pane )
  register struct zipview	       *self;
  register zip_type_image		image;
  register zip_type_pane		pane;
  {
  register long			        status;

  IN(zipview_Print_Image);
  Initialize_Printing( self );
  status = zipprint_Print_Image( Printer, image, pane );
  OUT(zipview_Print_Image);
  return  status;
  }

long
zipview__Print_Stream( self, stream, pane )
  register struct zipview	       *self;
  register zip_type_stream		stream;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(zipview_Print_Stream);
  Initialize_Printing( self );
  status = zipprint_Print_Stream( Printer, stream, pane );
  OUT(zipview_Print_Stream);
  return  status;
  }

long
zipview__Print_Pane( self, pane )
  register struct zipview	       *self;
  register zip_type_pane		pane;
  {
  register long			        status;

  IN(zipview_Print_Pane);
  Initialize_Printing( self );
  status = zipprint_Print_Pane( Printer, pane );
  OUT(zipview_Print_Pane);
  return  status;
  }

static
Initialize_Printing( self )
  register struct zipview	     *self;
  {
  register int			      i, status = zip_ok;

  IN(Initialize_Printing);
  if ( Printer == NULL )
    {
    Printer = zipprint_New();
    zipprint_Set_Data_Object( Printer, Data );
    zipprint_Set_View_Object( Printer, self );
    zipprint_Set_Debug( Printer, debug );
    for ( i = 1; Objects(i); i++ )/*===*/
      zipobject_Set_Print_Object( Objects(i), Printer );
    }
  OUT(Initialize_Printing);
  return  status;
  }

void
zipview__Set_Debug( self, state )
  register struct zipview	      *self;
  register char			       state;
  {
  IN(zipview_Set_Debug);
  debug = state;
  OUT(zipview_Set_Debug);
  }

static void
DEBUG_Command( self )
  register struct zipview	      *self;
  {
  IN(DEBUG_Command);
  debug = !debug;
  zip_Set_Debug( Data, debug );
  if ( Printer )
    zipprint_Set_Debug( Printer, debug );
  if ( Editor )
    zipedit_Set_Debug( Editor, debug );
  OUT(DEBUG_Command);
  }

static void
Edit_Command( self )
  register struct zipview	      *self;
  {
  IN(Edit_Command);
  Set_pending_edit;
  zipview_WantUpdate( self, self );
  OUT(Edit_Command);
  }

static void
Browse_Command( self )
  register struct zipview     *self;
  {
  IN(Browse_Command);
  Set_pending_browse;
  zipview_WantUpdate( self, self );
  OUT(Browse_Command);
  }

static void
Portrait_Command( self )
  register struct zipview     *self;
  {
  IN(Portrait_Command);
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_landscape) | menu_portrait );
  zipview_PostMenus( self, Menu );
  zipview_Set_Print_Orientation( self, zip_portrait );
  OUT(Portrait_Command);
  }

static void
Landscape_Command( self )
  register struct zipview       *self;
  {
  IN(Landscape_Command);
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_portrait) | menu_landscape );
  zipview_PostMenus( self, Menu );
  zipview_Set_Print_Orientation( self, zip_landscape );
  OUT(Landscape_Command);
  }

static void
Print_To_File_Command( self )
  register struct zipview       *self;
  {
  char			        *reply;
  register FILE			*file;
  char				 msg[512];

  IN(Print_To_File_Command);
  /* Would like to prompt with source stream-name -- currently infeasible */
  if ( zipview_Query( self, "Enter Print-File Name: ", NULL, &reply ) == zip_ok  &&
       reply  &&  *reply )
    {
    if ( file = fopen( reply, "w" ) )
      {
      sprintf( msg, "Generating PostScript Print-File '%s'", reply );
      zipview_Announce( self, msg );
      zipview_Use_Working_Pane_Cursors( self );
      zipview_Print( self, file, "PostScript", "PostScript", true );
      fclose( file );
      sprintf( msg, "Wrote PostScript Print-File '%s'", reply );
      zipview_Announce( self, msg );
      zipview_Use_Normal_Pane_Cursors( self );
      }
      else
      {
      sprintf( msg, "Unable to Open '%s'", reply );
      zipview_Announce( self, msg );
      }
    }
  OUT(Print_To_File_Command);
  }

/*===
static void
Page_First_Command( self )
  register struct zipview     *self;
  {
  IN(Page_First_Command);
  if ( CurrentPage > 1 )
    {
    CurrentPage = 1;
    zipview_Display_Image( self, zip_Image( Data, "ZIP_PAGE_IMAGE_001" ), Pane );
    zipview_Announce( View, "Page 1" );
    }
    else    zipview_Announce( View, "At First Page." );
  OUT(Page_First_Command);
  }

static void
Page_Last_Command( self )
  register struct zipview     *self;
  {
  char			      *page_image_name = "ZIP_PAGE_IMAGE_nnn";

  IN(Page_Last_Command);
  if ( CurrentPage < Data->page_count )
    {
    CurrentPage = Data->page_count;
    sprintf( page_image_name, "ZIP_PAGE_IMAGE_%03d", CurrentPage );
    DEBUGst(Page-name,page_image_name);
    zipview_Display_Image( self, zip_Image( Data, page_image_name ), Pane );
    sprintf( page_image_name, "Page %d", CurrentPage );
    zipview_Announce( View, page_image_name );
    }
    else    zipview_Announce( View, "At Last Page." );
  OUT(Page_Last_Command);
  }

static void
Page_Next_Command( self )
  register struct zipview     *self;
  {
  char			      *page_image_name = "ZIP_PAGE_IMAGE_nnn";
  register zip_type_image      image;

  IN(Page_Next_Command);
  sprintf( page_image_name, "ZIP_PAGE_IMAGE_%03d", ++CurrentPage );
  DEBUGst(Page-name,page_image_name);
  if ( image = zip_Image( Data, page_image_name ) )
    {
    zipview_Display_Image( self, image, Pane );
    sprintf( page_image_name, "Page %d", CurrentPage );
    zipview_Announce( View, page_image_name );
    }
    else
    {
    CurrentPage--;
    zipview_Announce( View, "At Last Page." );
    }
  OUT(Page_Next_Command);
  }

static void
Page_Prior_Command( self )
  register struct zipview     *self;
  {
  char			      *page_image_name = "ZIP_PAGE_IMAGE_nnn";

  IN(Page_Prior_Command);
  if ( CurrentPage > 1 )
    {
    sprintf( page_image_name, "ZIP_PAGE_IMAGE_%03d", --CurrentPage );
    DEBUGst(Page-name,page_image_name);
    zipview_Display_Image( self, zip_Image( Data, page_image_name ), Pane );
    sprintf( page_image_name, "Page %d", CurrentPage );
    zipview_Announce( View, page_image_name );
    }
    else    zipview_Announce( View, "At First Page." );
  OUT(Page_Prior_Command);
  }
===*/

/*===*/static long ZOOM_LEVEL;
static void
Zoom_Pane( self, factor )
  register struct zipview	      *self;
  register int			       factor;
  {
  IN(Zoom_Pane);
  Set_pending_zoom;
  if ( factor != 0 )
    ZOOM_LEVEL = Pane->zip_pane_zoom_level + factor;
    else
    ZOOM_LEVEL = 0;
  zipview_WantUpdate( self, self );
  OUT(Zoom_Pane);
  }

static void
Zoom_Normal_Command( self )  register struct zipview  *self;
  {  Zoom_Pane( self, 0 );  }

static void
Zoom_In_Command( self )  register struct zipview     *self;
  {  Zoom_Pane( self, 1 );  }

static void
Zoom_In5_Command( self )  register struct zipview     *self;
  {  Zoom_Pane( self, 5 );  }

static void
Zoom_Out5_Command( self )  register struct zipview     *self;
  {  Zoom_Pane( self, -5 );  }

static void
Zoom_Out_Command( self )  register struct zipview     *self;
  {  Zoom_Pane( self, -1 );  }


static void
Normalize_Pane_Command( self )
  register struct zipview	      *self;
  {
  IN(Normalize_Pane_Command);
  zipview_Normalize_Pane( self, Pane );
  Set_pending_redisplay;
  zipview_WantUpdate( self, self );
  OUT(Normalize_Pane_Command);
  }

static void
Refresh_Pane_Command( self )
  register struct zipview	      *self;
  {
  IN(Refresh_Pane_Command);
  Set_pending_redisplay;
  zipview_WantUpdate( self, self );
  OUT(Refresh_Pane_Command);
  }

static void
Pan_Pane( self, edge )
  register struct zipview	      *self;
  register int			       edge;
  {
  Edge = edge;
  Set_pending_pan;
  zipview_WantUpdate( self, self );
  }

static void
Center_Pane_Command( self )
  register struct zipview	      *self;
  {
  IN(Center_Pane_Command);
  Pane->zip_pane_x_offset = 0;
  Pane->zip_pane_y_offset = 0;
  Pane->zip_pane_x_origin_offset = Pane->zip_pane_x_origin;
  Pane->zip_pane_y_origin_offset = Pane->zip_pane_y_origin;
  Set_pending_redisplay;
  zipview_WantUpdate( self, self );
  OUT(Center_Pane_Command);
  }

static void
Top_Pane_Command( self )  register struct zipview     *self;
  {  Pan_Pane( self, zipview_pane_top_edge );  }

static void
Bottom_Pane_Command( self )  register struct zipview     *self;
  {  Pan_Pane( self, zipview_pane_bottom_edge );  }

static void
Left_Pane_Command( self )  register struct zipview     *self;
  {  Pan_Pane( self, zipview_pane_left_edge );  }

static void
Right_Pane_Command( self )  register struct zipview     *self;
  {  Pan_Pane( self, zipview_pane_right_edge );  }

/*===*/static float SCALE;
static void
Scale_Pane( self, scale )
  register struct zipview	      *self;
  register float		       scale;
  {
  IN(Scale_Pane);
  DEBUGgt( Scale, scale);
  Set_pending_scale;
  SCALE = zipview_Pane_Scale( self, Pane ) + scale;
  zipview_WantUpdate( self, self );
  OUT(Scale_Pane);
  }

static void
Scale_Normal_Command( self )
  register struct zipview	      *self;
  {
  Set_pending_scale;
  SCALE = 1.0;
  zipview_WantUpdate( self, self );
  }

static void
Scale_Smaller_Command( self )  register struct zipview     *self;
  {  Scale_Pane( self, -0.01 );  }

static void
Scale_Smaller_10_Command( self )  register struct zipview     *self;
  {  Scale_Pane( self, -0.1 );  }

static void
Scale_Larger_Command( self )  register struct zipview     *self;
  {  Scale_Pane( self, 0.01 );  }

static void
Scale_Larger_10_Command( self )  register struct zipview     *self;
  {  Scale_Pane( self, 0.1 );  }

static void
Scale_Half_Command( self )
  register struct zipview	      *self;
  {
  IN(Scale_Half_Command);
  Set_pending_scale;
  SCALE = zipview_Pane_Scale( self, Pane ) * 0.5;
  zipview_WantUpdate( self, self );
  OUT(Scale_Half_Command);
  }

static void
Scale_Double_Command( self )
  register struct zipview	      *self;
  {
  IN(Scale_Double_Command);
  Set_pending_scale;
  SCALE = zipview_Pane_Scale( self, Pane ) * 2.0;
  zipview_WantUpdate( self, self );
  OUT(Scale_Double_Command);
  }

static struct bind_Description 	      bound_menu[] =
{
{ "zipview-compose",	    "\033e",    0,	"Compose",		0,  menu_browse,
	Edit_Command,		"Switch to Compose-mode",   NULL },
{ "zipview-browse",	    "\033b",    0,	"Browse",		0,  menu_edit,
	Browse_Command,		"Switch to Browse-mode",   NULL },
{ "zipview-normalize",	    "\033n",    0,	"Pane~20,Normalize~10",	0,  menu_default,
	Normalize_Pane_Command, "...",   NULL },
{ "zipview-refesh",	    "\033r",    0,	"Pane~20,Refresh~11",	0,  menu_default,
	Refresh_Pane_Command,   "...",   NULL },
{ "zipview-center",	    "\033PC",    0,	"Pane~20,Center~20",	0,  menu_default,
	Center_Pane_Command,	"...",   NULL },
{ "zipview-pane-top",	    "\033PT",    0,	"Pane~20,Top~30",	0,  menu_default,
	Top_Pane_Command,   	"...",   NULL },
{ "zipview-pane-bottom",    "\033PB",	0,      "Pane~20,Bottom~31",	0,  menu_default,
	Bottom_Pane_Command,	"...",   NULL },
{ "zipview-pane-left",	    "\033PL",	0,	"Pane~20,Left~32",	0,  menu_default,
	Left_Pane_Command,	"...",   NULL },
{ "zipview-pane-right",	    "\033PR",	0,      "Pane~20,Right~33",	0,  menu_default,
	Right_Pane_Command,	"...",   NULL },
{ "zipview-zoom-zero",	    "\033z0",	0,      "Zoom~30,Normal~10",	0,  menu_default,
	Zoom_Normal_Command,	"...",   NULL },
{ "zipview-zoom-in",	    "\033zi",	0,      "Zoom~30,In~20",	0,  menu_default,
	Zoom_In_Command,	"...",   NULL },
{ "zipview-zoom-in5",	    "\033zi5",	0,	"Zoom~30,In +5~21",	0,  menu_default,
	Zoom_In5_Command,	"...",   NULL },    
{ "zipview-zoom-out",	    "\033zo",	0,      "Zoom~30,Out~30",	0,  menu_default,
	Zoom_Out_Command,	"...",   NULL },
{ "zipview-zoom-out5",	    "\033zo5",	0,	"Zoom~30,Out -5~31",	0,  menu_default,
	Zoom_Out5_Command,	"...",   NULL },
{ "zipview-scale-normal",   "\033s0",	0,      "Scale~40,Normal~10",	0,  menu_default,
	Scale_Normal_Command,	"...",   NULL },
{ "zipview-scale-100s",	    "\033ssh",0,      "Scale~40,100th Smaller~20",0,	menu_default,
	Scale_Smaller_Command,	"...",   NULL },
{ "zipview-scale-10s",	    "\033sst",	0,	"Scale~40,10th Smaller~21", 0,	menu_default,
	Scale_Smaller_10_Command,"...",   NULL },
{ "zipview-scale-half",	    "\033sh",	0,	"Scale~40,Half Size~22",    0,	menu_default,
	Scale_Half_Command,	"...",   NULL },
{ "zipview-scale-100l",	    "\033slh",0,	"Scale~40,100th Larger~30", 0,	menu_default,
	Scale_Larger_Command,	"...",   NULL },
{ "zipview-scale-10l",	    "\033slt",	0,	"Scale~40,10th Larger~31",  0,	menu_default,
	Scale_Larger_10_Command,"...",   NULL },
{ "zipview-scale-double",   "\033sd",	0,	"Scale~40,Double Size~32",  0,	menu_default,
	Scale_Double_Command,	"...",   NULL },
{ "zipview-print_portrait", "\033pp",	0,	"Printing~50,Portrait~10",  0,	menu_app | menu_landscape,
	Portrait_Command,	"...",   NULL },
{ "zipview-print_landscape", "\033pl",	0,	"Printing~50,Landscape~10", 0,	menu_app | menu_portrait,
	Landscape_Command,	"...",   NULL },
{ "zipview-print_to_file",  "\033pf",	0,	"Printing~50,To File ...~20",	0,  menu_app,
	Print_To_File_Command,	"...",   NULL },
/*===
{ "zipview-next-page",	    " ",	0,	"Page~50,Next~10",	0,  menu_default,
	Page_Next_Command,	"...",   NULL },
{ "zipview-next-page",	    "n",	0,	"Page~50,Next~10",	0,  menu_default,
	Page_Next_Command,	"...",   NULL },
{ "zipview-prior-page",	    "p",	0,	"Page~50,Prior~11",	0,  menu_default,
	Page_Prior_Command,	"...",   NULL },
{ "zipview-first-page",	    "f",	0,	"Page~50,First~20",	0,  menu_default,
	Page_First_Command,	"...",   NULL },
{ "zipview-last-page",	    "l",	0,	"Page~50,Last~21",	0,  menu_default,
	Page_Last_Command,	"...",   NULL },
===*/
{ "zipview-DEBUG",	    "D",	0,	"",			0,  menu_default,
	DEBUG_Command,		"...",   NULL },
NULL
};

static
Build_Menu()
  {
  IN(Build_Menu);
  bind_BindList( bound_menu, class_keymap, class_menulist, &zipview_classinfo );
  OUT(Build_Menu);
  }

static
Pending_Hit( self )
  register struct zipview	   *self;
  {
  IN(Pending_Hit);
  Reset_pending_hit;
  if ( InputFocus )
    switch ( MouseAction )
      {
      case view_RightDown:
	zipview_Initiate_Panning( self, Pane, MouseX, MouseY, 0/*===*/ );
	break;
      case view_RightMovement:
	zipview_Continue_Panning( self, Pane, MouseX, MouseY );
	break;
      case view_RightUp:
	Normalize_View( self );
	zipview_Terminate_Panning( self, Pane, MouseX, MouseY, 0, 0, 1 );
	Highlight_View( self );
	break;
      }
  OUT(Pending_Hit);
  }

static
Pending_Redisplay( self )
  register struct zipview	   *self;
  {
  IN(Pending_Redisplay);
  DEBUGxt(Action, Action);
  Reset_pending_redisplay;
  if ( zipview_Display_Pane( self, Pane ) )
    {DEBUG(ZipView Redisplay ERROR);}
  if ( InputFocus )
    Highlight_View( self );
  OUT(Pending_Redisplay);
  }

static
Pending_Redraw( self )
  register struct zipview	   *self;
  {
  IN(Pending_Redraw);
  DEBUGxt(Action, Action);
  Reset_pending_redraw;
  if ( zipview_Draw_Pane( self, Pane ) )
    {DEBUG(ZipView Redraw ERROR);}
  if ( InputFocus )
    Highlight_View( self );
  OUT(Pending_Redraw);
  }

static
Pending_Zoom( self )
  register struct zipview	   *self;
  {
  register zip_type_point	    x, y;

  IN(Pending_Zoom);
  Reset_pending_zoom;
  x = zipview_X_Pixel_To_Point( self, Pane, NULL,
    zipview_Pane_Left( self, Pane ) + (zipview_Pane_Width( self, Pane )/2) );
  y = zipview_Y_Pixel_To_Point( self, Pane, NULL,
    zipview_Pane_Top( self, Pane ) + (zipview_Pane_Height( self, Pane )/2) );
  zipview_Zoom_Pane_To_Point( self, Pane, x, y,
    ZOOM_LEVEL, zip_center|zip_middle );
  OUT(Pending_Zoom);
  }

static
Pending_Scale( self )
  register struct zipview	   *self;
  {
  register zip_type_point	    x, y;

  IN(Pending_Scale);
  Reset_pending_scale;
  x = zipview_X_Pixel_To_Point( self, Pane, NULL,
    zipview_Pane_Left( self, Pane ) + (zipview_Pane_Width( self, Pane )/2) );
  y = zipview_Y_Pixel_To_Point( self, Pane, NULL,
    zipview_Pane_Top( self, Pane ) + (zipview_Pane_Height( self, Pane )/2) );
  zipview_Scale_Pane_To_Point( self, Pane, x, y,
    SCALE, zip_center|zip_middle );
  OUT(Pending_Scale);
  }

static
Pending_Pan( self )
  register struct zipview	   *self;
  {
  IN(Pending_Pan);
  Reset_pending_pan;
  Normalize_View( self );
  zipview_Pan_Pane_To_Edge( self, Pane, Edge );
  Highlight_View( self );
  OUT(Pending_Pan);
  }

static
Pending_ReceiveInputFocus( self )
  register struct zipview	   *self;
  {
  IN(Pending_ReceiveInputFocus);
  Reset_pending_receiveinputfocus;
  InputFocus = true;
  Highlight_View( self );
  OUT(Pending_ReceiveInputFocus);
  }

static
Pending_LoseInputFocus( self )
  register struct zipview	   *self;
  {
  IN(Pending_LoseInputFocus);
  Reset_pending_loseinputfocus;
  Normalize_View( self );
  OUT(Pending_LoseInputFocus);
  }

static
Pending_Edit( self )
  register struct zipview	   *self;
  {
  register long			    i;

  IN(Pending_Edit);
  Reset_pending_edit;
  zipview_Use_Working_Pane_Cursors( self );
  if ( Editor == NULL )
    {
    Editor = zipedit_New();
    zipedit_Set_Debug( Editor, debug );
    zipedit_Set_Data_Object( Editor, Data );
    zipedit_Set_View_Object( Editor, self );
    for ( i = 1; Objects(i); i ++ )
      zipobject_Set_Edit_Object( Objects(i), Editor );
    }
  if ( zipedit_Set_Palettes( Editor, Pane,
	zip_name_palette | zip_shade_palette | zip_attribute_palette |
	zip_figure_palette | zip_font_palette ) )
    {DEBUG(ZipView ERROR: Set_Palettes);}
  Editing = true;
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_browse) | menu_edit );
  zipview_PostMenus( self, Menu );
  if ( zipedit_Initialize_Editing( Editor, Pane ) )
    {DEBUG(ZipView ERROR: Initialize_Editing);
    Editing = false;
    menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_edit) | menu_browse );
    zipview_PostMenus( self, Menu );
    }
  Highlight_View( self );
  zipview_Use_Normal_Pane_Cursors( self );
  OUT(Pending_Edit);
  }

static
Pending_Browse( self )
  register struct zipview	   *self;
  {
  IN(Pending_Browse);
Set_pending_browse;
  Reset_pending_browse;
  Editing = false;
  zipedit_Terminate_Editing( Editor, Pane );
  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_edit) | menu_browse );
  zipview_PostMenus( self, Menu );
  Highlight_View( self );
  OUT(Pending_Browse);
  }

static
Highlight_View( self )
  register struct zipview	     *self;
  {
  IN(Highlight_View);
  zipview_SetTransferMode( self, graphic_BLACK );
  zipview_Normalize_Line_Attributes( self );
  zipview_DrawRectSize( self, 0, 0, zipview_GetLogicalWidth(  self ) - 1,
				    zipview_GetLogicalHeight( self ) - 1);
  OUT(Highlight_View);
  }

static
Normalize_View( self )
  register struct zipview	     *self;
  {
  IN(Normalize_View);
  zipview_SetTransferMode( self, graphic_WHITE );
  zipview_Normalize_Line_Attributes( self );
  zipview_DrawRectSize( self, 0, 0, zipview_GetLogicalWidth(  self ) - 1,
				    zipview_GetLogicalHeight( self ) - 1);
  OUT(Normalize_View);
  }

static
Prepare_Default_Pane( self )
  register struct zipview	      *self;
  {
  register long			      status = zip_ok;
  char				      pane_name[257];
  static int			      pane_number = 1;

  IN(Prepare_Default_Pane);
  sprintf( pane_name, "ZipView-Pane-%02d", pane_number++ );
  zipview_Create_Pane( self, &Pane, pane_name, &Block, zip_opaque );
  if ( ObjectWidth )	zipview_Set_Pane_Object_Width(  self, Pane, ObjectWidth );
  if ( ObjectHeight )	zipview_Set_Pane_Object_Height( self, Pane, ObjectHeight );
  if ( Data->page_count )
    status = zipview_Set_Pane_Image( self, Pane, zip_Image( Data, "ZIP_PAGE_IMAGE_001" ) );
    else
    status = zipview_Set_Pane_Stream( self, Pane, Stream );
  OUT(Prepare_Default_Pane);
  return  status;
  }

static int
Prepare_Default_Stream( self )
  register struct zipview	      *self;
  {
  register long			      status = zip_ok;
  char				      stream_name[257];
  static int			      stream_number = 1;
  char				     *source =
	"*D;-1000,1400\nN8.5X11\n>-1000,1400";

  IN(Prepare_Default_Stream);
  sprintf( stream_name, "ZipView-Created-Stream-%02d", stream_number++ );
  if ( status = zip_Create_Stream( Data, &Stream, stream_name, zip_default ) )
    {DEBUGdt( ZipView ERROR: Create-Stream Status, status);}
  if ( status = zip_Set_Stream_Source( Data, Stream, source ) )
    {DEBUGdt( ZipView ERROR: Set-Stream-Source Status, status);}
  Set_pending_edit;
  OUT(Prepare_Default_Stream);
  return status;
  }

long
zipview__Query( self, query, default_response, response )
  register struct zipview	      *self;
  register char			      *query, *default_response, **response;
  {
  register long			      status = zip_ok;
  static char			      buffer[512];

  IN(zipview_Query);
  Abeyant = true;
  *buffer = 0;
  *response = buffer;
  if ( (message_AskForString( self, 0, query, default_response,
	 buffer, sizeof buffer ) == -1)  ||  *buffer == 0 )
    {
    zipview_Announce( self, "Cancelled." );
    status = zip_failure;
    }
  DEBUGst(Buffer,buffer);
  Abeyant = false;
  OUT(zipview_Query);
  return  status;
  }

long
zipview__Query_File_Name( self, query, response )
  register struct zipview	     *self;
  register char			     *query;
  register char			    **response;
  {
  register enum message_CompletionCode  result;
  static char			      path[257];
  static char			      buffer[513];
  register long			      status = zip_ok;

  IN(zipview_Query_File_Name);
  Abeyant = true;
  *buffer = 0;
  *response = buffer;
  result = (enum message_CompletionCode) completion_GetFilename( self, 
			  query,			/* prompt */
			  path,				/* initial string */
			  buffer,			/* working area */
			  sizeof buffer - 1,		/* size of working area */
			  0,				/* want file, not directory */
			  1 );				/* must be existing file */
  DEBUGdt(Result,result);
  DEBUGst(File-name,buffer);
  if ( (result != message_Complete  &&  result != message_CompleteValid)  ||
       *buffer == 0 ||
	buffer[strlen(buffer)-1] == '/' )
    {
    status = zip_failure;
    zipview_Announce( self, "Cancelled." );
    }
    else /* Preserve path for next time around */
    {
    strncpy( path, buffer, sizeof(path) - 1 );
    path[sizeof(path) - 1] = 0;
    }
  Abeyant = false;
  OUT(zipview_Query_File_Name);
  return  status;
  }

long
zipview__Announce( self, message )
  register struct zipview	      *self;
  register char			      *message;
  {
  register long			      status = zip_ok;

  IN(zipview_Announce);
  if ( self->header.view.imPtr )
    {
    if ( message_DisplayString( self, 0, message ) == -1 )
      status = zip_failure;
    im_ForceUpdate();
    }
    else  fprintf( stderr, "Zip: %s\n", message );
  OUT(zipview_Announce);
  return  status;
  }

/* The Scroll-Bar facilities */

#include "scroll.ih"

static void			      zipview_y_getinfo(), zipview_y_setframe(),
				      zipview_x_getinfo(), zipview_x_setframe(),
				      zipview_y_endzone(),  zipview_x_endzone();
static long			      zipview_y_whatisat(), zipview_x_whatisat();
static struct scrollfns		      vertical_scroll_interface =
		{ zipview_y_getinfo, zipview_y_setframe,
		  zipview_y_endzone, zipview_y_whatisat };
static struct scrollfns		      horizontal_scroll_interface =
		{ zipview_x_getinfo, zipview_x_setframe,
		  zipview_x_endzone, zipview_x_whatisat };



struct view *
zipview__GetApplicationLayer( self )
  register struct zipview	     *self;
  {
  register struct scroll	     *view;

  IN(zipview_GetApplicationLayer);
  Application = true;
  view = scroll_Create( self, scroll_LEFT | scroll_BOTTOM );
  scroll_SetView( view, self );
  OUT(zipview_GetApplicationLayer);
  return  (struct view *) view;
  }


struct scrollfns *
zipview__GetInterface( self, interface_name )
  register struct zipview	     *self;
  register char			     *interface_name;
  {
  register struct scrollfns	     *interface = NULL;

  IN(zipview_GetInterface);
  if ( strcmp( interface_name, "scroll,vertical" ) == 0 )
    interface = &vertical_scroll_interface;
    else
    if ( strcmp( interface_name, "scroll,horizontal" ) == 0 )
      interface = &horizontal_scroll_interface;
  OUT(zipview_GetInterface);
  return interface;
  }

static void
zipview_y_getinfo( self, total, seen, dot )
  register struct zipview	     *self;
  register struct range		     *total, *seen, *dot;
  {
  register long			      height, top, bottom, x;

  IN(zipview_y_getinfo);
  total->beg = total->end = dot->beg = dot->end = seen->beg = seen->end = 0;
  if ( Pane )
    {
  top = zipview_Y_Point_To_Pixel( self, Pane, NULL /*===*/,
	  zip_Image_Greatest_Y( Data, zip_Image_Root( Data, Stream ) ) );
  bottom = zipview_Y_Point_To_Pixel( self, Pane, NULL /*===*/,
	     zip_Image_Least_Y( Data, zip_Image_Root( Data, Stream ) ) );
  if ( top > bottom )
    {
    x = top;
    top = bottom;
    bottom = x;
    }
  height = bottom - top;
  total->end = height;
  seen->beg = (top >= zipview_Pane_Top( self, Pane )) ? 0 : -top;
  seen->end = (bottom <= zipview_Pane_Bottom( self, Pane )) ?
		height : height - (bottom - zipview_Pane_Bottom( self, Pane ));
  DEBUGdt( Pane Top, zipview_Pane_Top( self, Pane ) );
  DEBUGdt( Pane Bottom, zipview_Pane_Bottom( self, Pane ) );
  DEBUGdt( Imagery Top, top);
  DEBUGdt( Imagery Bottom, bottom);
  DEBUGdt( Imagery Height, height);
  DEBUGdt( Total-beg, total->beg);
  DEBUGdt( Total-end, total->end);
  DEBUGdt( Seen-beg, seen->beg);
  DEBUGdt( Seen-end, seen->end);
    }
  OUT(zipview_y_getinfo);
  }


static long
zipview_y_whatisat( self, coordinate, outof )
  register struct zipview	     *self;
  register long			      coordinate, outof;
  {
  register long			      value;

  IN(zipview_y_whatisat);
  DEBUGlt( Coordinate,coordinate);
  DEBUGlt( Outof, outof);
  DEBUGdt( y-offset,Pane->zip_pane_y_offset);
  value = coordinate + Pane->zip_pane_y_offset;
  DEBUGlt( Value,value);
  OUT(zipview_y_whatisat);
  return value;
  }

static void 
zipview_y_setframe( self, position, coordinate, outof )
  register struct zipview	     *self;
  register int			      position;
  register long			      coordinate, outof;
  {
  IN(zipview_y_setframe);
  DEBUGdt( Position, position );
  DEBUGlt( Coordinate, coordinate );
  DEBUGlt( Outof, outof );
  DEBUGdt( y-offset(1),Pane->zip_pane_y_offset);
  Pane->zip_pane_y_offset = position - coordinate;
  DEBUGdt( y-offset(2),Pane->zip_pane_y_offset);
  Set_pending_redisplay;
  zipview_WantUpdate(self, self);
  OUT(zipview_y_setframe);
  }


static void
zipview_y_endzone( self, zone, action )
  register struct zipview	     *self;
  register int			      zone, action;
  {
  IN(zipview_y_endzone);
  DEBUGdt( Zone,zone);
  DEBUGdt( Action,action);
  if ( zone == scroll_TOPENDZONE )
    {
    DEBUG( Top);
    Pan_Pane( self, zipview_pane_top_edge );
    }
    else
    {
    DEBUG( Bottom);
    Pan_Pane( self, zipview_pane_bottom_edge );
    }
  OUT(zipview_y_endzone);
  }

static void
zipview_x_getinfo( self, total, seen, dot )
  register struct zipview	     *self;
  register struct range		     *total, *seen, *dot;
  {
  register long			      width, left, right, x;

  IN(zipview_x_getinfo);
  total->beg = total->end = dot->beg = dot->end = seen->beg = seen->end = 0;
  if ( Pane )
    {
  left = zipview_X_Point_To_Pixel( self, Pane, NULL /*===*/,
	  zip_Image_Least_X( Data, zip_Image_Root( Data, Stream ) ) );
  right = zipview_X_Point_To_Pixel( self, Pane, NULL /*===*/,
	     zip_Image_Greatest_X( Data, zip_Image_Root( Data, Stream ) ) );
  if ( left > right )
    {
    x = left;
    left = right;
    right = x;
    }
  width = right - left;
  total->end = width;
  seen->beg = (left >= zipview_Pane_Left( self, Pane )) ? 0 : -left;
  seen->end = (right <= zipview_Pane_Right( self, Pane )) ?
		width : width - (right - zipview_Pane_Right( self, Pane ));
  DEBUGdt( Pane Left, zipview_Pane_Left( self, Pane ) );
  DEBUGdt( Pane Right, zipview_Pane_Right( self, Pane ) );
  DEBUGdt( Imagery Left, left);
  DEBUGdt( Imagery Right, right);
  DEBUGdt( Imagery Width, width);
  DEBUGdt( Total-beg, total->beg);
  DEBUGdt( Total-end, total->end);
  DEBUGdt( Seen-beg, seen->beg);
  DEBUGdt( Seen-end, seen->end);
    }
  OUT(zipview_x_getinfo);
  }


static long
zipview_x_whatisat( self, coordinate, outof )
  register struct zipview	     *self;
  register long			      coordinate, outof;
  {
  register long			      value;

  IN(zipview_x_whatisat);
  DEBUGlt( Coordinate,coordinate);
  DEBUGlt( Outof, outof);
  DEBUGdt( x-offset,Pane->zip_pane_x_offset);
  value = coordinate + Pane->zip_pane_x_offset;
  DEBUGlt( Value,value);
  OUT(zipview_x_whatisat);
  return value;
  }

static void 
zipview_x_setframe( self, position, coordinate, outof )
  register struct zipview	     *self;
  register int			      position;
  register long			      coordinate, outof;
  {
  IN(zipview_x_setframe);
  DEBUGdt( Position, position );
  DEBUGlt( Coordinate, coordinate );
  DEBUGlt( Outof, outof );
  DEBUGdt( x-offset(1),Pane->zip_pane_x_offset);
  Pane->zip_pane_x_offset = coordinate - position;
  DEBUGdt( x-offset(2),Pane->zip_pane_x_offset);
  Set_pending_redisplay;
  zipview_WantUpdate(self, self);
  OUT(zipview_x_setframe);
  }


static void
zipview_x_endzone( self, zone, action )
  register struct zipview	     *self;
  register int			      zone, action;
  {
  IN(zipview_x_endzone);
  DEBUGdt( Zone,zone);
  DEBUGdt( Action,action);
  if ( zone == scroll_TOPENDZONE )
    {
    DEBUG( Left);
    Pan_Pane( self, zipview_pane_left_edge );
    }
    else
    {
    DEBUG( Right);
    Pan_Pane( self, zipview_pane_right_edge );
    }
  OUT(zipview_x_endzone);
  }
/*=== === ===*/

int 
apt_MM_Compare( s1, s2 )
  /* Assumes "s1" must be shifted to lower-case
             "s2" must be shifted to lower-case
  */
  register unsigned char		 *s1, *s2;
  {
  register unsigned char		  c1, c2;
  register int				  result = 0;

  do
    {
    c1 = isupper( *s1 ) ? tolower( *s1++ ) : *s1++;
    c2 = isupper( *s2 ) ? tolower( *s2++ ) : *s2++;
    if ( c1 != c2 )
      break;
    } while ( c1 );
  if ( c1 != c2 )
    if ( c1 > c2 )
      result = 1;
      else result = -1;
  return result;
  }


