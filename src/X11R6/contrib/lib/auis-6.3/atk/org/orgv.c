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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/org/RCS/orgv.c,v 1.52 1993/01/15 23:23:59 gk5g Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Org View-object

MODULE	orgv.c

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Org View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  01/19/89	Created (TCP)
  05/02/89	Introduce additional Attributes (TCP)
  05/04/89	Remove ForceUpdate in FullUpdate to prevent recursion in EZ (TCP)
  05/09/89	Improve placement of InputFocus Highlight lines (TCP)
  05/18/89	Have SetDataObject do super_SetDataObject (TCP)
  05/24/89	Change control-actions (TCP)
		Only access controls if exposed.
		Add Expose/Hide Description
		Force Write to DataStream to have ATK brackets
  05/25/89	Add Vertical/Horizontal Arrangement choice (TCP)
		Also Fold/UnFold choice
		Improve Menus
  05/26/89	Switch Horizontal & Vertical terminology (TCP)
  05/31/89	Add NodeBorder & Connector style to Menu (TCP)
		Provide initial set-up for new org-chart
		Ensure initial set-up only when necessary
  06/01/89	Split Menu into two cards (TCP)
  05/02/89	Automatically use Add command for new chart (TCP)
  06/06/89	Prevent infinite-loop on multiple Rename clicks (TCP)
		Also other bogus user missteps.
		Use WantUpdate for RIF & LIF
		(Curious need for printf in R.I.F. ...)
  06/07/89	Automatically expose Description area if Root has one (TCP)
		Make Palette taller
		Always expose Description with 50% of window
		Ensure ReadOnly-ness (when detectable)
  06/09/89	Temporarily cope with no horizontal explosion (TCP)
  06/23/89	Support Horizontal Explosion (TCP)
  07/12/89	Ensure update of Description appears only after resizing (TCP)
  07/25/89	Remove arg from im_ForceUpdate (TCP)
  07/31/89	Change invocation to suite: ChangeItemCaption to ChangeItemAttribute (TCP)
  08/23/89	Correct setting of ExposeControls/Description flags (TCP)
		Remove Create method
  08/24/89	Upgrade to Tree & Suite V1.0 Interfaces (TCP)
  08/31/89	Change OfData to OfDatum (TCP)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <graphic.ih>
#include <observe.ih>
#include <view.ih>
#include <message.ih>
#include <fontdesc.ih>
#include <im.ih>
#include <lpair.ih>
#include <text.ih>
#include <textv.ih>
#include <filetype.ih>
#include <attribs.h>
#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <bind.ih>
#include <rect.h>
#include <apt.h>
#include <apts.ih>
#include <aptv.ih>
#include <org.ih>
#include <orgv.eh>
#include <tree.ih>
#include <treev.ih>
#include <suite.ih>

static  struct menulist		 *class_menulist;
static  struct keymap		 *class_keymap;

#define  menu_default		  (1<<0)
#define  menu_buttons		  (1<<3)
#define  menu_exploded		  (1<<4)
#define  menu_imploded		  (1<<5)
#define  menu_description_hidden  (1<<6)
#define  menu_description_exposed (1<<7)
#define  menu_palette_hidden	  (1<<8)
#define  menu_palette_exposed	  (1<<9)
#define  menu_horizontal	  (1<<10)
#define  menu_vertical		  (1<<11)
#define  menu_folded		  (1<<12)
#define  menu_unfolded		  (1<<13)
#define  menu_debug		  (1<<14)

extern int			  sys_nerr;
extern char			 *sys_errlist[];

static char			  ExplodePhrase[]   = "Explode",
				  ImplodePhrase[]   = "Implode",
				  VerticalPhrase[]  = "Vertical",
				  HorizontalPhrase[] = "Horizontal",
				  FoldPhrase[]	    = "Fold",
				  UnFoldPhrase[]    = "UnFold",
				  NodeBorderPhrase[] = "NodeBorder",
				  NodeConnectorPhrase[] = "NodeConnector",
				  RenamePhrase[]    = "Rename",
				  AddPhrase[]	    = "Add",
				  DeletePhrase[]    = "Delete",
				  DescriptionPhrase[] = "Description";

#define  Org			  (self->data_object)
#define  Anchor			  (self->anchor)
#define  Menu			  (self->menu)
#define  Keystate		  (self->keystate)
#define  Bounds			  (&self->bounds)
#define  Left			  (self->bounds.left)
#define  Top			  (self->bounds.top)
#define  Width			  (self->bounds.width)
#define  Height			  (self->bounds.height)
#define  Center			  (Left + Width/2)
#define  Middle			  (Top + Height/2)
#define  Bottom			  (Top + Height)
#define  PaletteHeight		  (85)
#define  PaletteTop		  (Top + Height - PaletteHeight)

#define  HitHandler		  (self->hit_handler)

#define  Tree			  (Org->tree_data_object)
#define  TreeView		  (self->tree_view_object)
#define  PairView		  (self->pair_view)
#define  DescriptionExposed	  (self->description_exposed)
#define  Description		  (self->description_text)
#define  DescriptionView	  (self->description_textview)
#define  DescriptionViewScroll	  (self->description_textview_scroll)
#define  PreviousNode		  (self->previous_node)
#define  Suite			  (self->suite_object)

#define  FirstTime		  (self->first_time)
#define  InputFocus		  (self->input_focus)
#define  PaletteExposed		  (self->controls_exposed)
#define  Exploded		  (self->exploded)
#define  ExplodedNode		  (self->exploded_node)
#define  IgnoreLoseInputFocus	  (self->ignore_loseinputfocus)
#define  IgnoreFullUpdate	  (self->ignore_fullupdate)

#define  Arrangement		  (self->arrangement)
#define  HorizontalArrangement	  (self->arrangement==treev_Horizontal)
#define  VerticalArrangement	  (self->arrangement==treev_Vertical)
#define  NodeBorderStyle	  (self->node_border_style)
#define  NodeConnectorStyle	  (self->node_connector_style)
#define  BackgroundShade	  (self->background_shade)
#define  Fold			  (self->fold)
#define  LastModified		  (self->last_modified)
#define  DescriptionLastModified  (self->description_last_modified)
#define  InitialNodeCount	  (self->initial_node_count)

static long Tree_Hit();
static Alter_Control_Button();
static Passivate();
static Activate();
static FullUpdate_Tree();
static Prepare_Description();


static treev_Specification specification[] = {
  treev_NodeFontName( "andysans10" ),
  treev_NodeBorderStyle( treev_Rectangle ),
  treev_NodeConnectorStyle( treev_DogLeg | treev_Fold ),
  treev_NodeFiligree( treev_DropShadow ),
  treev_NodeOrder( treev_ColumnMajor ),
  treev_HitHandler( Tree_Hit ),
  treev_Scroll( treev_Left | treev_Bottom ),
  treev_BackgroundShade( 25 ),
  treev_Cursor( 'z' ),
  treev_CursorFontName( "aptcsr20" ),
  NULL
};

#define  add_code	      1
#define  delete_code	      2
#define  rename_code	      3
#define  x_code		      4
#define  print_code	      5
#define  save_code	      6
#define  y_code		      7
#define  plode_code	      8
#define  description_code     9
#define  arrangement_code    10
#define  fold_code	     11
#define  node_border_code    12
#define  node_connector_code 13

int Orgv_Debug = 0;
#define debug Orgv_Debug


static suite_Specification add_button[] = {
  suite_ItemCaption(AddPhrase),
  suite_ItemDatum(add_code),  0
};

static suite_Specification delete_button[] = {
  suite_ItemCaption(DeletePhrase),
  suite_ItemDatum(delete_code),  0
};

static suite_Specification rename_button[] = {
  suite_ItemCaption(RenamePhrase),
  suite_ItemDatum(rename_code),  0
};

static suite_Specification plode_button[] = {
  suite_ItemCaption(ExplodePhrase),
  suite_ItemDatum(plode_code),  0
};

static suite_Specification description_button[] = {
  suite_ItemCaption(DescriptionPhrase),
  suite_ItemDatum(description_code),  0
};

static suite_Specification arrangement_button[] = {
  suite_ItemCaption(HorizontalPhrase),
  suite_ItemDatum(arrangement_code),  0
};

static suite_Specification fold_button[] = {
  suite_ItemCaption(UnFoldPhrase),
  suite_ItemDatum(fold_code),  0
};

static suite_Specification node_border_button[] = {
  suite_ItemCaption(NodeBorderPhrase),
  suite_ItemDatum(node_border_code),  0
};

static suite_Specification node_connector_button[] = {
  suite_ItemCaption(NodeConnectorPhrase),
  suite_ItemDatum(node_connector_code),  0
};

static long Control_Button_Hit();

static suite_Specification control_buttons[] = {
  suite_Item( add_button ),
  suite_Item( delete_button ),
  suite_Item( rename_button ),
  suite_Item( plode_button ),
  suite_Item( description_button ),
  suite_Item( arrangement_button ),
  suite_Item( fold_button ),
  suite_Item( node_border_button ),
  suite_Item( node_connector_button ),
  suite_ItemCaptionFontName( "andysans10b" ),
  suite_HitHandler( Control_Button_Hit ),
  suite_Arrangement( suite_Matrix | suite_Fixed ),
  suite_Rows( 3 ), suite_Columns( 3 ),
  NULL
};

static void 	    Add_Command(),
	    Delete_Command(),
            Rename_Command(),
	    Description_Command(), 
            Plode_Command(),
	    Arrangement_Command(), 
            Fold_Command(),
	    Node_Border_Command(), 
            Node_Connector_Command(),
	    Palette_Command(), 
            DEBUG_Command();

static struct bind_Description view_menu[] = {
  { "orgv-Add", "",	    0,	"Add Node~20",	 0, menu_default,
    Add_Command,	"Add Node" },
  { "orgv-Delete", "",   0,	"Delete Node~21",0, menu_default,
    Delete_Command,	"Delete Node" },
  { "orgv-Rename", "",   0,	"Rename Node~22",0, menu_default,
    Rename_Command,	"Rename Node" },
  { "orgv-Plode", "",    0,	"Explode Nodes~30",0, menu_imploded,
    Plode_Command,	"Plode Tree" },
  { "orgv-Plode", "",    0,	"Implode Nodes~30",0, menu_exploded,
    Plode_Command,	"Plode Tree" },
  { "orgv-Description", "", 0,	"Expose Description~31",0, menu_description_hidden,
    Description_Command,"Description" },
  { "orgv-Description", "", 0,	"Hide Description~31",0, menu_description_exposed,
    Description_Command,"Description" },
  { "orgv-Palette",  "", 0,	"Expose Palette~40",	0, menu_palette_hidden,
    Palette_Command,	"Palette-toggle" },
  { "orgv-Palette",  "", 0,	"Hide Palette~40",	0, menu_palette_exposed,
    Palette_Command,	"Palette-toggle" },
  { "orgv-DEBUG",  "",   0,	"DEBUG~98",	0, menu_debug,
    DEBUG_Command,	"Debug-toggle" },
  { "org-Arrangement", "", 0,	"Styling~10,Vertical Layout~10",0, menu_horizontal,
    Arrangement_Command,"Arrangement" },
  { "org-Arrangement", "", 0,	"Styling~10,Horizontal Layout~10",0, menu_vertical,
    Arrangement_Command,"Arrangement" },
  { "org-Fold", "", 0,		"Styling~10,Fold Nodes~73",11, menu_unfolded,
    Fold_Command,	"Fold" },
  { "org-Fold", "", 0,		"Styling~10,UnFold Nodes~73",11, menu_folded,
    Fold_Command,	"Fold" },
  { "orgv-Border", "",    0,	"Styling~10,Node Border~20",	0, menu_default,
    Node_Border_Command,"Change Node Border" },
  { "orgv-Connector", "", 0,	"Styling~10,Node Connector~21",	0, menu_default,
    Node_Connector_Command,"Change Node Connector" },
  NULL
};

boolean
orgv__InitializeClass( classID )
  register struct classheader *classID;
{
  IN(orgv_InitializeClass );
  DEBUGst(RCSID,rcsid);
  class_menulist = menulist_New();
  class_keymap = keymap_New();
  bind_BindList( view_menu, class_keymap, class_menulist, &orgv_classinfo );
  OUT(orgv_InitializeClass );
  return(TRUE);
}

boolean
orgv__InitializeObject( classID, self)
  register struct classheader *classID;
  register struct orgv *self;
{
  register boolean status = true;

  IN(orgv_InitializeObject);
  DEBUGst(RCSID,rcsid);
  orgv_SetOptions( self, aptv_SuppressControl |
			 aptv_SuppressBorder |
			 aptv_SuppressEnclosures );
  InitialNodeCount = 0;
  Org = NULL;
  HitHandler = NULL;
  PreviousNode = NULL;
  InputFocus = PaletteExposed = DescriptionExposed = Exploded =
      IgnoreLoseInputFocus  = IgnoreFullUpdate = false;
  LastModified = DescriptionLastModified = 0;
  Arrangement = treev_Vertical;
  NodeConnectorStyle = treev_DogLeg;
  NodeBorderStyle = treev_Rectangle;
  FirstTime = Fold = true;
  Menu = menulist_DuplicateML( class_menulist, self );
  menulist_SetView( Menu, self );
  Keystate = keystate_Create( self, class_keymap );
  DEBUG(Create TreeView);
  if ( (TreeView = treev_Create( specification, self )) == NULL ) {
    printf( "OrgV: Unable to Create Tree View Object\n" );
    status = false;
  }
  if ( status == true ) { DEBUG(Created TreeView);
    treev_SetDebug( TreeView, debug );
    if ( (Suite = suite_Create( control_buttons, self )) == NULL ) {
      printf( "OrgV: Unable to Create Suite Object\n" );
      status = false;
    }
  }
  if ( status == true ) { DEBUG(Created Suite);
    Description = text_New();
    DescriptionView = textview_New();
    textview_SetDataObject( DescriptionView, Description );
    DescriptionViewScroll = textview_GetApplicationLayer( DescriptionView );
    text_AddObserver( Description, self );
  }
  if ( status == true ) { DEBUG(Created Description);
    PairView = lpair_New();
    lpair_VSplit( PairView, TreeView, DescriptionViewScroll, 0, 1 );
  }
  OUT(orgv_InitializeObject);
  return(status);
}

void
orgv__FinalizeObject( classID, self )
  register struct classheader *classID;
  register struct orgv *self;
{
  IN(orgv_FinalizeObject );
  if ( Description ) {
      lpair_Destroy(PairView);
      textview_DeleteApplicationLayer(DescriptionView,DescriptionViewScroll);
      textview_Destroy(DescriptionView);
      text_Destroy(Description);
  }
  if ( TreeView )   treev_Destroy( TreeView );
  if ( Suite )	    suite_Destroy( Suite );
  if ( Menu )	    menulist_Destroy( Menu );
  if ( Keystate )   keystate_Destroy( Keystate );
  OUT(orgv_FinalizeObject );
}

void
orgv__SetDataObject( self, data )
  register struct orgv *self;
  register struct org *data;
{
  IN(orgv_SetDataObject);
  super_SetDataObject( self, data );
  Org = data;
  org_AddObserver( Org, self );
  treev_SetDataObject( TreeView, Tree );
  InitialNodeCount = tree_NodeCount( Tree, tree_RootNode( Tree ) );
  OUT(orgv_SetDataObject);
}

void
orgv__ReceiveInputFocus( self )
  register struct orgv *self;
{
  IN(orgv_ReceiveInputFocus);
  InputFocus = true;
  if ( Keystate ) {
    Keystate->next = NULL;
    orgv_PostKeyState( self, Keystate );
  }
  if ( Menu ) {
    menulist_SetMask( Menu, menulist_GetMask( Menu ) | menu_default |
	(((debug) ? menu_debug : 0) |
	 ((Exploded) ? menu_exploded : menu_imploded) |
	 ((DescriptionExposed) ? menu_description_exposed : menu_description_hidden) |
	 ((Fold) ? menu_folded : menu_unfolded) |
	 ((PaletteExposed) ? menu_palette_exposed : menu_palette_hidden) |
	 ((HorizontalArrangement) ? menu_horizontal : menu_vertical) ) );
    orgv_PostMenus( self, Menu );
  }
  orgv_WantUpdate( self, self );
  OUT(orgv_ReceiveInputFocus);
}

void
orgv__LoseInputFocus( self )
  register struct orgv *self;
{
  IN(orgv_LoseInputFocus);
  InputFocus = false; 
  if ( ! IgnoreLoseInputFocus ) {
    orgv_WantUpdate( self, self );
  }
  OUT(orgv_LoseInputFocus);
}

void
orgv__SetDebug( self, state )
  register struct orgv *self;
  register char state;
{
  IN(orgv_SetDebug);
  debug = state;
  if ( Org )		org_SetDebug( Org, debug );
  if ( Org  &&  Tree )	tree_SetDebug( Tree, debug );
  if ( TreeView )	treev_SetDebug( TreeView, debug );
  OUT(orgv_SetDebug);
}

void
orgv__SetHitHandler( self, handler, anchor )
  register struct orgv *self;
  register struct view *(*handler)();
  register struct view *anchor;
{
  IN(orgv_SetHitHandler);
  HitHandler = handler;
  if ( anchor )
    Anchor = anchor;
  OUT(orgv_SetHitHandler);
}

void
orgv__FullUpdate( self, type, left, top, width, height )
  register struct orgv *self;
  register enum view_UpdateType type;
  register long left, top, width, height;
{
  register long controls = PaletteExposed * PaletteHeight;

  IN(orgv_FullUpdate);
  DEBUGdt(Type,type);
  if ( ! IgnoreFullUpdate  &&  (type == view_FullRedraw  ||  type == view_LastPartialRedraw) ) {
    super_FullUpdate( self, type, left, top, width, height );
    Left = orgv_BodyLeft( self );
    Top = orgv_BodyTop( self );
    Width = orgv_BodyWidth( self );
    Height = orgv_BodyHeight( self );
    orgv_SetTransferMode( self, graphic_COPY );
    if ( FirstTime ) { DEBUG(FirstTime);
      FirstTime = false;
      if ( tree_RootNode( Tree ) ) { DEBUG(RootNode);
	if ( tree_NodeDatum( Tree, tree_RootNode( Tree ) ) )
          DescriptionExposed = true;
      }
      if ( tree_NodeCount( Tree, tree_RootNode( Tree )) == 0 ) {
	DescriptionExposed = PaletteExposed = true;
	controls = PaletteExposed * PaletteHeight;
	menulist_SetMask( Menu, (menulist_GetMask( Menu ) &
			~menu_palette_hidden) | menu_palette_exposed );
      }
      if ( DescriptionExposed ) {
	  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & 			 ~menu_description_hidden) | menu_description_exposed );
	  lpair_VSplit( PairView, TreeView, DescriptionViewScroll, 40, 1 );
      }
      else {
	  lpair_VSplit( PairView, TreeView, DescriptionViewScroll, 0, 1 );
	  menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_description_exposed) | menu_description_hidden );
      }
      if ( InputFocus ) 
	  orgv_PostMenus( self, Menu );
    }
    Prepare_Description( self, treev_CurrentNode( TreeView ) );
    lpair_LinkTree(PairView, self);
    lpair_InsertViewSize( PairView, self, Left, Top, Width, (Height - controls) );
    lpair_FullUpdate( PairView, type, 0,0, Width, (Height - controls) );
    if ( PaletteExposed ) { DEBUG(Palette Exposed);
      suite_LinkTree( Suite, self);
      suite_InsertViewSize( Suite, self, Left, PaletteTop, Width, PaletteHeight );
      suite_FullUpdate( Suite, type, 0,0, Width, PaletteHeight );
    }
  }
  OUT(orgv_FullUpdate);
}

struct view *
orgv__Hit( self, action, x, y, clicks )
  register struct orgv *self;
  register enum view_MouseAction action;
  register long x, y, clicks;
{
  register struct view *hit = (struct view *) self;

  IN(orgv_Hit );
  orgv_Announce( self, "" );
  if ( !InputFocus  &&  action == view_LeftDown ) { DEBUG(Grab IF);
    orgv_WantInputFocus( self, self );
  }
  if ( InputFocus ) {
    IgnoreLoseInputFocus = true;
    if ( PaletteExposed  &&  y > PaletteTop ) { DEBUG(Palette Hit);
      hit = (struct view *) suite_Hit( Suite, action,
	    orgv_EnclosedXToLocalX( Suite, x ),
	    orgv_EnclosedYToLocalY( Suite, y ), clicks );
    }
    else { DEBUG(Pair Hit);
      hit = (struct view *) lpair_Hit( PairView, action,
	    orgv_EnclosedXToLocalX( PairView, x ),
	    orgv_EnclosedYToLocalY( PairView, y ), clicks );
    }
    IgnoreLoseInputFocus = false;
  }
  else
      hit = (struct view *) lpair_Hit( PairView, action,
	    orgv_EnclosedXToLocalX( PairView, x ),
	    orgv_EnclosedYToLocalY( PairView, y ), clicks );
  OUT(orgv_Hit);
  return(hit);
}

enum view_DSattributes
orgv__DesiredSize( self, given_width, given_height, pass, desired_width, desired_height )
  register struct orgv	    *self;
  register long		     given_width, given_height;
  register enum view_DSpass  pass;
  register long		    *desired_width, *desired_height;
{
  register enum view_DSattributes result = view_WidthFlexible | view_HeightFlexible;
  IN(orgv_DesiredSize);
  if ( TreeView )
    result = treev_DesiredSize( TreeView, given_width, given_height, pass, desired_width, desired_height );
  OUT(orgv_DesiredSize);
  return(result);
}

static long
Control_Button_Hit( self, suite, item, type, action, x, y, clicks )
  register struct orgv		  *self;
  register struct suite		  *suite;
  register struct suite_item	  *item;
  register long			   type;
  register enum view_MouseAction   action;
  register long			   x, y, clicks;
{
  char msg[512];

  IN(Control_Button_Hit);
  DEBUGdt(Action,action);
  if ( type == suite_ItemObject  &&  action == view_LeftUp ) {
    switch ( suite_ItemAttribute( suite, item, suite_ItemDatum(0) ) ) {
      case  add_code:		Add_Command( self );		break;
      case  delete_code:	Delete_Command( self );		break;
      case  rename_code:	Rename_Command( self );		break;
      case  plode_code:		Plode_Command( self );		break;
      case  description_code:	Description_Command( self );	break;
      case  arrangement_code:	Arrangement_Command( self );    break;
      case  fold_code:		Fold_Command( self );		break;
      case  node_border_code:	Node_Border_Command( self );	break;
      case  node_connector_code:Node_Connector_Command( self );	break;
      default:
	sprintf( msg, "Unknown control-code (%d)",
		    suite_ItemAttribute( suite, item, suite_ItemDatum(0) ) );
	orgv_Announce( self, msg );
    } 
    suite_NormalizeItem( suite, item );
    if ( !InputFocus )
	orgv_WantInputFocus( self, self );
  }
  OUT(Control_Button_Hit);
  return(0);
}

static void
Add_Command( self )
  register struct orgv *self;
{
  register struct tree_node *node;
  char *reply;

  IN(Add_Command);
  while ( true ) {
      orgv_Query( self, "Enter Node Name: ", "", &reply );
      if ( reply == NULL  ||  *reply == 0 )
	  break;
      if ( node = tree_CreateChildNode( Tree, "?", NULL, treev_CurrentNode( TreeView ) ) ) {
	  tree_SetNotificationCode( Tree, tree_NodeCreated );
	  tree_SetNotificationNode( Tree, node );
	  orgv_Announce( self, "" );
	  tree_SetNodeName( Tree, node, apts_StripString( reply ) );
	  tree_NotifyObservers( Tree, NULL );
	  org_SetModified( Org );
	  if ( PaletteExposed )
	      Activate( self );
      }
      else {
	  orgv_Announce( self, "Unable to Create Node." );
	  break;
      }
  }
  OUT(Add_Command);
}

static void
Delete_Command( self )
  register struct orgv *self;
{
  register struct tree_node *current_node = treev_CurrentNode( TreeView );
  IN(Delete_Command);
  if ( current_node ) {
      tree_SetNotificationCode( Tree, tree_NodeDestroyed );
      tree_SetNotificationNode( Tree, current_node );
      tree_NotifyObservers( Tree, NULL );
      if(tree_NodeDatum(Tree, current_node))
	  text_Destroy((struct text *) tree_NodeDatum(Tree, current_node));
      tree_DestroyNode( Tree, current_node );
      org_SetModified( Org );
      if ( PaletteExposed  &&  tree_NodeCount( Tree, tree_RootNode( Tree ) ) == 0 ) {
	  Passivate( self );
	  im_ForceUpdate();
      }
  }
  else
      orgv_Announce( self, "Nothing to Delete." );
  OUT(Delete_Command);
}

static void
Rename_Command( self )
  register struct orgv *self;
{
  register struct tree_node *current_node = treev_CurrentNode( TreeView );
  char *reply;

  IN(Rename_Command);
  if ( current_node ) {
      orgv_Query( self, "Enter New Node Name: ",
		  tree_NodeName( Tree, current_node ), &reply );
      orgv_Announce( self, "" );
      if ( reply  &&  *reply ) {
	  tree_SetNodeName( Tree, current_node, apts_StripString( reply ) );
	  tree_SetNotificationCode( Tree, tree_NodeNameChanged );
	  tree_SetNotificationNode( Tree, current_node );
	  tree_NotifyObservers( Tree, NULL );
	  org_SetModified( Org );
      }
  }
  else
      orgv_Announce( self, "Nothing to Rename." );
  OUT(Rename_Command);
}

void
orgv__Print( self, file, processor, format, level )
  register struct orgv	     *self;
  register FILE		     *file;
  register char		     *processor;
  register char		     *format;
  register boolean	      level;
{
  IN(orgv_Print);
  treev_Print( TreeView, file, processor, format, level );
  OUT(orgv_Print);
}

static void
Plode_Command( self )
  register struct orgv *self;
{
  IN(Plode_Command);
  if ( treev_CurrentNode( TreeView ) ) {
    if ( Exploded = !Exploded ) {
      treev_ExplodeNode( TreeView, ExplodedNode = treev_CurrentNode( TreeView ) );
      Alter_Control_Button( self, plode_code, ImplodePhrase );
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_imploded) | menu_exploded );
    }
    else {
      treev_ImplodeNode( TreeView, ExplodedNode );
      Alter_Control_Button( self, plode_code, ExplodePhrase );
      ExplodedNode = NULL;
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_exploded) | menu_imploded );
    }
    orgv_PostMenus( self, Menu );
  }
  else
      orgv_Announce( self, "Nothing to Explode." );
  OUT(Plode_Command);
}

static void
Fold_Command( self )
  register struct orgv *self;
{
  IN(Fold_Command);
  if ( Fold = !Fold ) {
    treev_SetTreeAttribute( TreeView, treev_NodeConnectorStyle( treev_Fold | NodeConnectorStyle ) );
    Alter_Control_Button( self, fold_code, UnFoldPhrase );
    menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_unfolded) | menu_folded );
  }
  else {
    treev_SetTreeAttribute( TreeView, treev_NodeConnectorStyle( treev_NoFold | NodeConnectorStyle ) );
    Alter_Control_Button( self, fold_code, FoldPhrase );
    menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_folded) | menu_unfolded );
  }
  FullUpdate_Tree( self );
  OUT(Fold_Command);
}

static void
Node_Border_Command( self )
  register struct orgv *self;
{
  static char *choices[] = {"Cancel", "Rectangle", "Round", "Oval", "Circle", 0};
  long response = 0, style, state = 0;

  IN(Node_Border_Command);
  IgnoreLoseInputFocus = IgnoreFullUpdate = true;
  if (NodeBorderStyle==treev_Rectangle)	    state = 1; 
  if (NodeBorderStyle==treev_RoundAngle)    state = 2;
  if (NodeBorderStyle==treev_Oval)	    state = 3;
  if (NodeBorderStyle==treev_Circle)	    state = 4;
  message_MultipleChoiceQuestion(
	    self, 0, "Choose Style:", state, &response, choices, NULL );
  DEBUGdt(Response,response);
  switch ( response ) {
      case 0:	  break;
      case 1:	  style = treev_Rectangle;	break;
      case 2:	  style = treev_RoundAngle;	break;
      case 3:	  style = treev_Oval;		break;
      case 4:	  style = treev_Circle;		break;
      default:    response = 0;		 break;
  }
  if ( response  &&  style != NodeBorderStyle )
    treev_SetTreeAttribute( TreeView,
	treev_NodeBorderStyle( (NodeBorderStyle = style) ) );
  IgnoreLoseInputFocus = IgnoreFullUpdate = false;
  orgv_FullUpdate( self, view_FullRedraw, 0, 0, Width-3, Height-3 );
  OUT(Node_Border_Command);
}

static void
Node_Connector_Command( self )
  register struct orgv *self;
{
  static char *choices[] = {"Cancel", "Dog Leg", "Direct", 0};
  long response = 0, style, state = 0;

  IN(Node_Connector_Command);
  IgnoreLoseInputFocus = IgnoreFullUpdate = true;
  state = (NodeConnectorStyle == treev_DogLeg) ? 1 : 2;
  message_MultipleChoiceQuestion(
	    self, 0, "Choose Style:", state, &response, choices, NULL );
  DEBUGdt(Response,response);
  switch ( response ) {
      case 0:	  break;
      case 1:	  style = treev_DogLeg;  break;
      case 2:	  style = treev_Direct;  break;
      default:    response = 0;		 break;
  }
  if ( response  &&  style != NodeConnectorStyle ) {
    DEBUGxt(style,((Fold) ? treev_Fold : treev_NoFold) | (NodeConnectorStyle = style));
    treev_SetTreeAttribute( TreeView,
	treev_NodeConnectorStyle( ((Fold) ? treev_Fold : treev_NoFold) |
	    (NodeConnectorStyle = style) ) );
  }
  IgnoreLoseInputFocus = IgnoreFullUpdate = false;
  orgv_FullUpdate( self, view_FullRedraw, 0, 0, Width-3, Height-3 );
  OUT(Node_Connector_Command);
}

static void
DEBUG_Command( self )
  register struct orgv	     *self;
{
  IN(DEBUG_Command);
  orgv_SetDebug( self, !debug );
  org_SetDebug( Org, debug );
  OUT(DEBUG_Command);
}

static void
Palette_Command( self )
  register struct orgv *self;
{
  IN(Palette_Command);
  if ( PaletteExposed = !PaletteExposed )
    menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_palette_hidden) | menu_palette_exposed );
  else
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_palette_exposed) | menu_palette_hidden );
  orgv_FullUpdate( self, view_FullRedraw, 0, 0, Width, Height );
  orgv_PostMenus( self, Menu );
  OUT(Palette_Command);
}

static void
Description_Command( self )
  register struct orgv *self;
{
  IN(Description_Command);
  if ( DescriptionExposed = !DescriptionExposed ) {
      lpair_VSplit( PairView, TreeView, DescriptionViewScroll, 40, 1 );
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) &
			~menu_description_hidden) | menu_description_exposed );
  }
  else {
      lpair_VSplit( PairView, TreeView, DescriptionViewScroll, 0, 1 );
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_description_exposed) | menu_description_hidden );
  }
  lpair_FullUpdate( PairView, view_FullRedraw, 0, 0, Width, Height - (PaletteExposed * PaletteHeight));
  orgv_SetTransferMode( self, graphic_COPY );
  orgv_PostMenus( self, Menu );
  OUT(Description_Command);
}


static void
Arrangement_Command( self )
  register struct orgv *self;
{
  IN(Arrangement_Command);
  if ( HorizontalArrangement ) {
      Arrangement = treev_Vertical;
      treev_SetTreeAttribute( TreeView, treev_Arrangement( treev_Vertical ) );
      treev_SetTreeAttribute( TreeView, treev_Cursor( 'z' ) );
      Alter_Control_Button( self, arrangement_code, HorizontalPhrase );
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_horizontal) | menu_vertical );
  }
  else {
      Arrangement = treev_Horizontal;
      treev_SetTreeAttribute( TreeView, treev_Arrangement( treev_Horizontal ) );
      treev_SetTreeAttribute( TreeView, treev_Cursor( 'b' ) );
      Alter_Control_Button( self, arrangement_code, VerticalPhrase );
      menulist_SetMask( Menu, (menulist_GetMask( Menu ) & ~menu_vertical) | menu_horizontal );
  }
  FullUpdate_Tree( self );
  OUT(Arrangement_Command);
}

static
Alter_Control_Button( self, datum, new )
  register struct orgv *self;
  register long datum;
  register char *new;
{
  if ( PaletteExposed )
      suite_ChangeItemAttribute( Suite, suite_ItemOfDatum( Suite, datum ),
				suite_ItemCaption(new) );
  else
      suite_SetItemAttribute( Suite, suite_ItemOfDatum( Suite, datum ),
	    suite_ItemCaption( new ) );
}

static
Passivate( self )
  register struct orgv *self;
  {
  if ( PaletteExposed ) {
    suite_PassivateItem( Suite, suite_ItemOfDatum( Suite, plode_code   ) );
    suite_PassivateItem( Suite, suite_ItemOfDatum( Suite, rename_code  ) );
    suite_PassivateItem( Suite, suite_ItemOfDatum( Suite, delete_code  ) );
    suite_PassivateItem( Suite, suite_ItemOfDatum( Suite, print_code   ) );
    suite_PassivateItem( Suite, suite_ItemOfDatum( Suite, save_code    ) );
  }
}

static
Activate( self )
  register struct orgv *self;
{
    if ( PaletteExposed ) {
	suite_ActivateItem( Suite, suite_ItemOfDatum( Suite, plode_code   ) );
	suite_ActivateItem( Suite, suite_ItemOfDatum( Suite, rename_code  ) );
	suite_ActivateItem( Suite, suite_ItemOfDatum( Suite, delete_code  ) );
	suite_ActivateItem( Suite, suite_ItemOfDatum( Suite, print_code   ) );
	suite_ActivateItem( Suite, suite_ItemOfDatum( Suite, save_code    ) );
    }
}

static
FullUpdate_Tree( self )
  register struct orgv *self;
{
  struct rectangle bounds;

  view_GetLogicalBounds( lpair_GetNth( PairView, 0 ), &bounds );
  view_FullUpdate( lpair_GetNth( PairView, 0 ), view_FullRedraw, bounds.left, bounds.top, bounds.width, bounds.height );
  orgv_Announce( self, "" );
  orgv_PostMenus( self, Menu );
}

static long
Tree_Hit( self, tree_view, node, type, action, x, y, clicks )
  register struct orgv		 *self;
  register struct treev	         *tree_view;
  register struct tree_node	 *node;
  register long			  type;
  register enum view_MouseAction  action;
  register long			  x, y, clicks;
{
  IN(Tree_Hit);
  DEBUGdt(Type,type);
  DEBUGdt(Action,action);
  if ( type == treev_NodeObject ) { DEBUG(Node Hit);
    if ( action == view_LeftDown  &&  node != PreviousNode ) {
	PreviousNode = node;
	treev_HighlightNode( TreeView, node );
	if ( ! treev_NodeExploded( TreeView, node ) ) {
	    if ( Exploded )
		Plode_Command( self );
	    treev_ExposeNodeChildren( TreeView, node );
	}
	Prepare_Description( self, node );
    }
    if ( HitHandler )
	(HitHandler)(Anchor, self, node, action, x, y, clicks );
  }
  OUT(Tree_Hit);
  return(NULL);
}

static
Prepare_Description( self, node )
  register struct orgv *self;
  register struct tree_node *node;
{
  register FILE *file;
  struct text *text;

  IN(Prepare_Description);
  if ( DescriptionExposed ) {
    orgv_UseWaitCursor( self );
    if (( text = (struct text *) tree_NodeDatum(Tree, node)) == NULL )
	tree_SetNodeDatum( Tree, node, (long) (text = text_New()));
    textview_SetDataObject(DescriptionView, text);
    DescriptionLastModified = text_GetModified( text );
    orgv_UseNormalCursor( self );
  }
  OUT(Prepare_Description);
}

void
orgv__ObservedChanged( self, changed, change )
  register struct orgv		     *self;
  register struct observable	     *changed;
  register long			      change;
  {
  IN(orgv_ObservedChanged);
/*=== needed ? */
  OUT(orgv_ObservedChanged);
  }

void
orgv__LinkTree(self, parent)
    struct orgv *self;
    struct view *parent;
{
    super_LinkTree(self, parent);
    if(parent && orgv_GetIM(self)) {
	lpair_LinkTree(PairView, (struct view *) self);
	if ( PaletteExposed )
	    suite_LinkTree( Suite, self );
    }
}
