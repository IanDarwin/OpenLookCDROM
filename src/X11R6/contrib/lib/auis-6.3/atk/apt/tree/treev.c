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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apt/tree/RCS/treev.c,v 1.46 1993/08/03 18:43:17 rr2b Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Tree View-object

MODULE	treev.c

VERSION	1.0

NOTICE	IBM Internal Use Only

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Tree View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/14/88	Created (TCP)
  05/05/89	Use SetPrintStream to pass print-setttings to pseudo-view (TCP)
		Remove ForceUpdate from FullUpdate
  05/11/89	Fix overwriting of fold-box bottom line (TCP)
		Reduce Sizing of Node borders
		Change placement of captions from ..BASELINE to ..BOTTOM
		Change Check_Dimensions to use Update instead of FullUpdate
  05/24/89	Support Vertical arrangement (TCP)
  05/26/89	Switch Horizontal & Vertical terminology (TCP)
  05/30/89	Prevent dump in FinalizeObject/Destroy_Shadows (TCP)
		Fix Explode to check for exposed children -- VAX detected bug
  05/31/89	Fix changing of NodeBorder to Circle to use Set_Dimensions (TCP)
		Improve Scrolling
		Improve Background filling on Redisplay.
  06/05/89	Implement Pale Footprints (TCP)
  06/06/89	Fix Horizontal Node-connector printing (TCP)
  06/06/89	Set CurrentNode default to Root-node (TCP)
  06/23/89	Support Horizontal Explosion (TCP)
  06/28/89	Fix Folded flag setting (TCP)
		Ensure node-connectors drawn even when nodes out of sight
  06/29/89	Fix Null CurrentNodeShadow within ImplodeNode (TCP)
		Use only 1-pixel highlight border
  07/14/89	Changed treev_Hit() to call the top-level hit-handler, iff
                one exists, even if no node is hit (GW Keim)
  07/19/89	Changed the #define ViewLinked from (self->header.view.parent) to
		 (ScrollView ? ScrollView->header.view.parent : self->header.view.parent).
		 This bug caused treev graphics ops to be done even when the treev was not
		 in the viewTree; (GW Keim)
  07/25/89	Remove arg from im_ForceUpdate (TCP)
  07/26/89	Use graphic_COPY instead of graphic_OR for Pale: X oddities (TCP)
  08/24/89	Pass object-type arg to Hit-handlers (TCP)
   10/24/89	Changed the Mark_Child_Exposure call in Expose_Node_Children to 
	              be recursive. (GW Keim)
	              Changed the definition of the macro ViewLinked to check for an imPtr.

END-SPECIFICATION  ************************************************************/



#include  <andrewos.h>
#include  <graphic.ih>
#include  <observe.ih>
#include  <view.ih>
#include  <fontdesc.ih>
#include  <im.ih>
#include  <cursor.ih>
#include  <rect.h>
#include  <scroll.ih>
#include  <apt.h>
#include  <apts.ih>
#include  <aptv.ih>
#include  <tree.ih>
#include  <treev.eh>
#include  <ctype.h>
#include <atom.ih>

int treev_debug = 0;

struct treev_instance
  {
  struct tree			 *data_object, *shadow_tree_object;
  struct view			 *(*hit_handler)();
  struct view			 *anchor;
  struct scroll			 *scroll_view;
  struct treev			 *scrolled_treev;
  struct node_shadow		 *root_node_shadow, *current_node_shadow, *prior_node_shadow;
  struct tree_node		 *current_node, *prior_node;
  struct rectangle		  bounds;
  void				(*pending_update)();
  short				  cell_height, cell_width,
				  node_width, node_height,
				  column_width, row_height,
				  max_name_width, max_name_height,
				  desired_width, desired_height,
  				  greatest_row, greatest_column;
  long				  max_peer_count, max_level_count,
				  vertical_offset, horizontal_offset,
				  pending_vertical_offset,
				  pending_horizontal_offset,
				  m_width, half_m_width, m_height;
  unsigned char			  scroll, node_border_style, node_highlight_style,
				  node_connector_style, fold, node_filigree,
				  arrangement, background_shade, node_footprint_style,
				  node_order;
  struct graphic		 *background_pattern;
  boolean			  graphics_initialized;
  char				 *node_font_name, *title_font_name, *tree_cursor_font_name;
  struct fontdesc		 *node_font, *title_font, *tree_cursor_font;
  unsigned char			  tree_cursor_byte;
  struct cursor			 *tree_cursor;
  struct graphic		 *black_tile, *dotted_tile;
  };

#define  Tree			      (self->instance->data_object)
#define  RootNode		      (tree_RootNode(Tree))
#define  NextNode(node)		      (tree_NextNode(Tree,node))
#define  CurrentNode		      (self->instance->current_node)
#define  PriorNode		      (self->instance->prior_node)
#define  TitleFontName		      (self->instance->title_font_name)
#define  TitleFont		      (self->instance->title_font)
#define  NodeFontName		      (self->instance->node_font_name)
#define  NodeFont		      (self->instance->node_font)
#define  MaxNameWidth		      (self->instance->max_name_width)
#define  MaxNameHeight		      (self->instance->max_name_height)
#define  MaxPeerCount		      (self->instance->max_peer_count)
#define  MaxLevelCount		      (self->instance->max_level_count)
#define  DesiredWidth		      (self->instance->desired_width)
#define  DesiredHeight		      (self->instance->desired_height)
#define  GraphicsInitialized	      (self->instance->graphics_initialized)

struct node_shadow		     *Next_Shadow();
tree_type_node			      Node_Shadow_Node();

#define  ShadowTree		      ((self)->instance->shadow_tree_object)
#define  ShadowRootNode		      (tree_RootNode(ShadowTree))
#define  NextShadowNode(node)	      (tree_NextNode(ShadowTree,node))
#define  ShadowNodeDatum(node)	      ((node_shadow_type)node->datum)

#define  CurrentNodeShadow	      ((self)->instance->current_node_shadow)
#define  PriorNodeShadow	      ((self)->instance->prior_node_shadow)
#define  ShadowedNode(shadow)	      ((shadow)->node)
#define  ShadowExposed(shadow)	      ((shadow)->exposed)
#define  ShadowHidden(shadow)	      ((shadow)->hidden)
#define  ShadowExploded(shadow)	      ((shadow)->exploded)
#define  ShadowFolded(shadow)	      ((shadow)->folded)
#define  ShadowChildrenExposed(shadow) ((shadow)->children_exposed)
#define  ShadowChildrenFolded(shadow)  ((shadow)->children_folded)
#define  ShadowVisible(shadow)	      ((shadow)->visible)
#define  ShadowVisage(shadow)	      ((shadow)->visage)
#define  Plain			      (0)
#define  ShadowPlain(shadow)	      (ShadowVisage(shadow) == Plain)
#define  Highlighted		      (1)
#define  ShadowHighlighted(shadow)    (ShadowVisage(shadow) == Highlighted)
#define  Footprinted		      (2)
#define  ShadowFootprinted(shadow)    (ShadowVisage(shadow) == Footprinted)
#define  ShadowSubWidth(shadow)	      ((shadow)->sub_width)
#define  ShadowSubHeight(shadow)      ((shadow)->sub_height)
#define  ShadowBounds(shadow)	      (&(shadow)->bounds)
#define  ShadowLeft(shadow)	      ((shadow)->bounds.left)
#define  SL			      (ShadowLeft(shadow))
#define  ShadowTop(shadow)	      ((shadow)->bounds.top)
#define  ST			      (ShadowTop(shadow))
#define  ShadowWidth(shadow)	      ((shadow)->bounds.width)
#define  SW			      (ShadowWidth(shadow))
#define  ShadowHeight(shadow)	      ((shadow)->bounds.height)
#define  SH			      (ShadowHeight(shadow))
#define  ShadowRight(shadow)	      ((shadow)->bounds.left+(shadow)->bounds.width)
#define  SR			      (ShadowRight(shadow))
#define  ShadowCenter(shadow)	      ((shadow)->bounds.left+(shadow)->bounds.width/2-DropShadow*2)
#define  SC			      (ShadowCenter(shadow))
#define  ShadowBottom(shadow)	      ((shadow)->bounds.top+(shadow)->bounds.height)
#define  SB			      (ShadowBottom(shadow))
#define  ShadowMiddle(shadow)	      ((shadow)->bounds.top+(shadow)->bounds.height/2)
#define  SM			      (ShadowMiddle(shadow))
#define  ShadowChildrenRowCount(shadow) ((shadow)->sub_row_count)
#define  ShadowChildrenColumnCount(shadow) ((shadow)->sub_column_count)

#define  ParentNode(node)	      ((node)->parent)
#define  ChildNode(node)	      ((node)->child)
#define  LeftNode(node)		      ((node)->left)
#define  RightNode(node)	      ((node)->right)
#define  NodeName(node)	    	      ((node)->name)
#define  NodeCaption(node)	      ((node)->caption)
#define  NodeCaptionName(node)	      (((node)->caption) ? (node)->caption : NodeName(node) )
#define  NodeTitle(node)	      ((node)->title)
#define  NodeDatum(node)	      ((node)->datum)
#define  NodeMode(node)	    	      ((node)->mode)
#define  NodeLevel(node)	      (tree_NodeLevel(Tree,node))
#define  NodeModified(node)	      ((node)->modified)

#define  NodeShadow(node)	      (Node_Shadow(self,node))
#define  NodeShadowNode(node)	      (Node_Shadow_Node(self,node))
#define  NodeHitHandler(node)	      (NodeShadow(node)->hit_handler)
#define  NodeViewObject(node)	      (NodeShadow(node)->view_object)
#define  NodeDataObject(node)	      (NodeShadow(node)->data_object)
#define  NodeExposed(node)	      (NodeShadow(node)->exposed)
#define  NodePlain(node)	      (NodeShadow(node)->visage==Plain)
#define  NodeHighlighted(node)	      (NodeShadow(node)->visage==Highlighted)
#define  NodeFootPrinted(node)	      (NodeShadow(node)->visage==FootPrinted)
#define  NodeChildrenExposed(node)    (NodeShadow(node)->children_exposed)
#define  NodeExploded(node)	      (NodeShadow(node)->exploded)
#define  NodeVisible(node)	      (NodeShadow(node)->visible)

#define  Anchor			      (self->instance->anchor)
#define  HitHandler		      (self->instance->hit_handler)
#define  Bounds			     (&self->instance->bounds)
#define  Left			      (self->instance->bounds.left)
#define  Top			      (self->instance->bounds.top)
#define  Width			      (self->instance->bounds.width)
#define  Right			      (Left+Width)
#define  Center			      (Left+Width/2)
#define  Middle			      (Top+Height/2)
#define  Height			      (self->instance->bounds.height)
#define  Bottom			      (Top+Height)

#define  CellWidth		      (self->instance->cell_width)
#define  CellHeight		      (self->instance->cell_height)
#define  ColumnWidth		      (self->instance->column_width)
#define  RowHeight		      (self->instance->row_height)
#define  MWidth			      (self->instance->m_width)
#define  MHeight		      (self->instance->m_height)
#define  HalfMWidth		      (self->instance->half_m_width)
#define  GreatestRow		      (self->instance->greatest_row)
#define  GreatestColumn		      (self->instance->greatest_column)
#define  VerticalOffset		      (self->instance->vertical_offset)
#define  HorizontalOffset	      (self->instance->horizontal_offset)
#define  PendingVerticalOffset	      (self->instance->pending_vertical_offset)
#define  PendingHorizontalOffset      (self->instance->pending_horizontal_offset)

#define  NodeWidth	    	      (self->instance->node_width)
#define  NodeHeight	    	      (self->instance->node_height)
#define  NodeBorderStyle	      (self->instance->node_border_style)
#define  NodeHighlightStyle	      (self->instance->node_highlight_style)
#define  NodeFootprintStyle	      (self->instance->node_footprint_style)
#define  NodeConnectorStyle	      (self->instance->node_connector_style)
#define  Arrangement		      (self->instance->arrangement)
#define  HorizontalArrangement	      (self->instance->arrangement == treev_Horizontal)
#define  VerticalArrangement	      (self->instance->arrangement == treev_Vertical)
#define  NodeOrder		      (self->instance->node_order)
#define  NodeOrderRowMajor	      (self->instance->node_order == treev_RowMajor)
#define  NodeOrderColumnMajor	      (self->instance->node_order == treev_ColumnMajor)
#define  NodeFiligree		      (self->instance->node_filigree)
#define  DropShadow		      (NodeFiligree & treev_DropShadow)

#define  ScrollView		      (self->instance->scroll_view)
#define  ScrolledView		      (self->instance->scrolled_treev)
#define  ViewLinked		      (treev_IsAncestor(self,treev_GetIM(self)))
#define  PendingUpdate		      (self->instance->pending_update)

#define  TreeCursor		      (self->instance->tree_cursor)
#define  TreeCursorByte		      (self->instance->tree_cursor_byte)
#define  TreeCursorFont		      (self->instance->tree_cursor_font)
#define  TreeCursorFontName	      (self->instance->tree_cursor_font_name)

#define  Scroll			      (self->instance->scroll)
#define  Fold			      (self->instance->fold)
#define  BackgroundShade	      (self->instance->background_shade)
#define  BackgroundPattern	      (self->instance->background_pattern)
#define  BackgroundWhite	      (self->instance->background_shade == 1)
#define  BackgroundNonWhite	      (self->instance->background_shade != 1)

#define  BlackTile		      (self->instance->black_tile)
#define  DottedTile		      (self->instance->dotted_tile)

#define  UpdateScrollbars	      {PendingUpdate=NULL;\
				       if(ViewLinked) treev_WantUpdate(self,self);}

typedef  struct node_shadow	     *node_shadow_type;
struct  node_shadow
  {
  struct tree_node		     *node;
  struct rectangle		      bounds;
  struct view			   *(*hit_handler)();
  struct view			     *view_object;
  struct observable		     *data_object;
  struct view			   *(*observer)();
  short				      sub_width, sub_height;
  unsigned char			      sub_row_count, sub_column_count, visage;
  unsigned			      visible	:1,
				      exposed	:1,
				      hidden	:1,
				      children_exposed	:1,
				      folded	:1,
				      children_folded	:1,
				      exploded	:1;
  };

#define  Balanced		    (view_BETWEENTOPANDBOTTOM | view_BETWEENLEFTANDRIGHT)
#define  Halo			    true
#define  NoHalo			    false

#define  max(a,b)		    (((a)>(b)) ? (a):(b))
#define  min(a,b)		    (((a)<(b)) ? (a):(b))
#define  abs(x)			    (((x)>0) ? (x):-(x))

static SetTreeAttribute();
static Name_Sizing();
static Initialize_Graphics();
static First_Time();
static void Redisplay();
static Set_Dimensions();
static Check_Dimensions();
static Erase_Node_Children();
static Redisplay_Node_Children();
static struct tree_node *Which_Node_Hit();
static Fill_Area();
static Fill_Shadow();
static Clear_Shadow();
static Normalize_Other_Nodes();
static Normalize_Node_Shadow();
static Highlight_Node_Shadow();
static Footprint_Node_Shadow();
static Hide_Node_Children();
static Expose_Node_Children();
static Printer();
static Print_Tree();
static Print_Node_Border();
static Print_Node_Connector();
static Arrange_Tree();
static Arrange_Horizontal_Tree();
static Arrange_Vertical_Tree();
static Arrange_Exploded_Tree();
static long Compute_Horizontal_Exploded_SubTree_Heights();
static Arrange_Horizontal_Exploded_Tree();
static Arrange_Horizontal_Exploded_SubTree();
static long Compute_Vertical_Exploded_SubTree_Widths();
static Arrange_Vertical_Exploded_Tree();
static Arrange_Vertical_Exploded_SubTree();
static Mark_Child_Exposure();
static Draw_Tree();
static Draw_Background();
static Fill_Background();
static Draw_Node_Caption();
static Draw_Node_Title();
static Draw_Node_Border();
static Draw_Node_Connector();
static struct node_shadow *Node_Shadow();
static struct node_shadow *Create_Shadow();
static Generate_Shadows();
static Generate_Children_Shadows();
static Destroy_Shadows();
static Destroy_Children_Shadows();
static Dump_Trees();

static char *treev_fg=NULL, *treev_bg=NULL;
static long treev_fgr, treev_fgg, treev_fgb;
static long treev_bgr, treev_bgg, treev_bgb;

static void treev_FlipColors(self)
struct treev *self;
{
    struct atom *a;
    treev_GetForegroundColor(self, &treev_fg, &treev_fgr, &treev_fgg, &treev_fgb);
    treev_GetBackgroundColor(self, &treev_bg, &treev_bgr, &treev_bgg, &treev_bgb);
    if(treev_fg) {
	a=atom_Intern(treev_fg);
	if(a) treev_fg=atom_Name(a);
	else treev_fg=NULL;
    }
    if(treev_bg) {
	a=atom_Intern(treev_bg);
	if(a) treev_bg=atom_Name(a);
	else treev_bg=NULL;
    }
    treev_SetForegroundColor(self, treev_bg, treev_bgr, treev_bgg, treev_bgb);
    treev_SetBackgroundColor(self, treev_fg, treev_fgr, treev_fgg, treev_fgb);
}

static void treev_RestoreColors(self)
struct treev *self;
{
    treev_SetForegroundColor(self, treev_fg, treev_fgr, treev_fgg, treev_fgb);
    treev_SetBackgroundColor(self, treev_bg, treev_bgr, treev_bgg, treev_bgb);
}


struct treev *
treev__Create( ClassID, specification, anchor )
  register struct  classheader	 *ClassID;
  treev_Specification		 *specification;
  register struct view		 *anchor;
  {
  register struct treev		 *self;
  register long			  mode = NULL;
  treev_Specification		 *spec = specification;
  static boolean		  bypass_scroll = false;

  IN(treev_Create);
  self = treev_New();
  Anchor = anchor;
  while ( specification  &&  specification->attribute )
    {
    SetTreeAttribute( self, specification->attribute, specification->value );
    specification++;
    }
  if ( Scroll  &&  !bypass_scroll )
    { DEBUG(Scroll SetUp);
    if ( Scroll & treev_Left )	 mode |= scroll_LEFT;
    if ( Scroll & treev_Bottom ) mode |= scroll_BOTTOM;
    bypass_scroll = true;
    ScrolledView = treev_Create( spec, anchor );
    bypass_scroll = false;
    ScrollView = scroll_Create( ScrolledView, mode );
    scroll_SetView( ScrollView, ScrolledView );
    }
  OUT(treev_Create);
  return  self;
  }

boolean
treev__InitializeObject( classID, self)
  register struct classheader *classID;
  register struct treev	      *self;
  {
  register long		       status = true;

  IN(treev_InitializeObject);
  DEBUGst(RCSID,rcsid);
  self->instance = (struct treev_instance *) calloc( 1, sizeof(struct treev_instance ) );
  treev_SetDimensions( self, 150, 200 );
  treev_SetOptions( self, aptv_SuppressControl | aptv_SuppressBorder );
  DesiredWidth = DesiredHeight = 200;
  NodeFootprintStyle = treev_Pale;
  NodeOrder = treev_ColumnMajor;
  Arrangement = treev_Vertical;
  Fold = true;
BackgroundShade = 1; /*=== force to white because some clients neglect to clear */
  if ( (ShadowTree = tree_New()) == NULL )
    {
    printf( "TreeView: ShadowTree not Created\n");
    status = false;
    }
  OUT(treev_InitializeObject);
  return  status;
  }

void
treev__FinalizeObject( classID, self )
  register struct classheader *classID;
  register struct treev	      *self;
  {
  IN(treev_FinalizeObject );
  if ( self->instance )
    {
    if ( ScrolledView ) {
      tree_Destroy( ShadowTree );
      scroll_Destroy( ScrollView );
      ScrollView = NULL;
      treev_Destroy( ScrolledView );
    }
    else{
      Destroy_Shadows( self, RootNode );
      tree_Destroy( ShadowTree );
    }
    if ( NodeFontName )		free( NodeFontName );
    if ( TitleFontName )	free( TitleFontName );
    if ( TreeCursorFontName )	free( TreeCursorFontName );
    free( self->instance );
    }
  OUT(treev_FinalizeObject );
  }

void
treev__SetDataObject( self, data_object )
  register struct treev	      *self;
  register struct tree	      *data_object;
  {
  IN(treev_SetDataObject);
  Tree = data_object;
  if ( ScrolledView )
    treev_SetDataObject( ScrolledView, data_object );
    else
    super_SetDataObject( self, data_object );
  OUT(treev_SetDataObject);
  }

long
treev__SetTreeAttribute( self, attribute, value )
  register struct treev	     *self;
  register long		      attribute, value;
  {
  register long		      status = ok;

  IN(treev_SetTreeAttribute);
  SetTreeAttribute( self, attribute, value );
  OUT(treev_SetTreeAttribute);
  return  status;
  }

static
SetTreeAttribute( self, attribute, value )
  register struct treev	     *self;
  register long		      attribute, value;
  {
  register long		      status = ok;

  IN(SetTreeAttribute);
  if ( ScrolledView )
    status = SetTreeAttribute( ScrolledView, attribute, value );
  else
  switch ( attribute )
    {
    case  treev_nodeborderstyle:
      NodeBorderStyle = value; Set_Dimensions( self );  break;
    case  treev_nodehighlightstyle:
      NodeHighlightStyle = value;			break;
    case  treev_nodefootprintstyle:
      NodeFootprintStyle = value;			break;
    case  treev_nodeconnectorstyle:
      if ( value & treev_DogLeg )
        NodeConnectorStyle = treev_DogLeg;
        else  NodeConnectorStyle = treev_Direct;
      if ( value & treev_Fold )    Fold = true;
      if ( value & treev_NoFold )  Fold = false;
      break;
    case  treev_nodefiligree:
      NodeFiligree = value;				break;
    case  treev_scroll:
      Scroll = value;					break;
    case  treev_nodefontname:
      apts_CaptureString( value, &NodeFontName );	break;
    case  treev_hithandler:
      HitHandler = (struct view *(*)()) value;		break;
    case  treev_arrangement:
      Arrangement = value; Set_Dimensions( self );	break;
    case  treev_nodeorder:
      NodeOrder = value;				break;
    case  treev_backgroundshade:
      BackgroundShade = value;				break;
    case  treev_cursor:
      TreeCursorByte = value;
      if ( GraphicsInitialized  &&  TreeCursor )
	{
	if ( TreeCursorFont )
	  cursor_SetGlyph( TreeCursor, TreeCursorFont, TreeCursorByte );
	  else
	  cursor_SetStandard( TreeCursor, TreeCursorByte );
	}
      break;
    case  treev_cursorfontname:
      apts_CaptureString( value, &TreeCursorFontName ); break;
    default:/*===*/
	printf( "treev: Unrecognized Attribute (%d) (Ignored)\n", attribute );
    }
  OUT(SetTreeAttribute);
  return  status;
  }

long
treev__TreeAttribute( self, attribute )
  register struct treev	     *self;
  register long		      attribute;
  {
  register long		      value = NULL;

  IN(treev_TreeAttribute);
  if ( ScrolledView )
    value = treev_TreeAttribute( ScrolledView, attribute );
  else
  switch ( attribute )
    {
    case  treev_nodeborderstyle:
      value = (long) NodeBorderStyle;      break;
    case  treev_nodehighlightstyle:
      value = (long) NodeHighlightStyle;    break;
    case  treev_nodeconnectorstyle:
      value = (long) NodeConnectorStyle;    break;
    case  treev_nodefiligree:
      value = (long) NodeFiligree;	    break;
    case  treev_scroll:
      value = (long) Scroll;		    break;
    case  treev_nodefontname:
      value = (long) NodeFontName;	    break;
    case  treev_hithandler:
      value = (long) HitHandler;	    break;
    case  treev_arrangement:
      value = (long) Arrangement;	    break;
    case  treev_nodeorder:
      value = NodeOrder;		    break;
    case  treev_backgroundshade:
      value = (long) BackgroundShade;	    break;
    case  treev_cursor:
      value = (long) TreeCursorByte;	    break;
    case  treev_cursorfontname:
      value = (long) TreeCursorFontName;    break;
    default:/*===*/
      printf( "TreeView: Unrecognized Attribute (%d)\n", attribute );
    }
  OUT(treev_TreeAttribute);
  return  value;
  }

void
treev__SetDebug( self, state )
  register struct treev	      *self;
  register char		       state;
  {
  IN(treev_SetDebug);
  treev_debug = state;
  if ( ScrolledView )
    treev_SetDebug( ScrolledView, state );
  OUT(treev_SetDebug);
  }

tree_type_node
treev__CurrentNode( self )
  register struct treev	      *self;
  {
  register tree_type_node      node;

  if ( (node = (self->instance->scrolled_treev) ?
	    self->instance->scrolled_treev->instance->current_node :
	    self->instance->current_node) == NULL )
    node = RootNode;
  return  node;
  }

static
Name_Sizing( self, tree, node, datum )
  register struct treev	     *self;
  register struct tree	     *tree;
  register tree_type_node     node;
  {
  long			      width, height;
  register long		      peers, level;

  IN(Name_Sizing);
  fontdesc_StringSize( NodeFont,
	treev_GetDrawable( self ), NodeCaptionName(node), &width, &height );
  if ( width > MaxNameWidth )
    MaxNameWidth = width;
  if ( (peers = tree_PeerNodeCount( Tree, node )) > MaxPeerCount )
    MaxPeerCount = peers + 1;
  if ( (level = tree_NodeLevel( Tree, node )) > MaxLevelCount )
    MaxLevelCount = level;
  OUT(Name_Sizing);
  return  NULL;
  }

static
Initialize_Graphics( self )
  register struct treev	     *self;
  {
  char			      font_family[256];
  long			      font_type, font_size;

  IN(Initialize_Graphics);
  if ( !GraphicsInitialized  &&  Tree )
    {
    GraphicsInitialized = true;
    BlackTile = treev_BlackPattern( self );
    DottedTile = treev_GrayPattern( self, 50, 100 );
    if ( TreeCursorByte )
      {
      TreeCursor = cursor_Create( self );
      if ( TreeCursorFontName )
	{
	fontdesc_ExplodeFontName( TreeCursorFontName,
				font_family, sizeof(font_family), &font_type, &font_size );
	TreeCursorFont = fontdesc_Create( font_family, font_type, font_size );
	cursor_SetGlyph( TreeCursor, TreeCursorFont, TreeCursorByte );
	}
	else cursor_SetStandard( TreeCursor, TreeCursorByte );
      }
    fontdesc_ExplodeFontName( (NodeFontName) ? NodeFontName : "andysans10b",
				font_family, sizeof(font_family), &font_type, &font_size );
    NodeFont = fontdesc_Create( font_family, font_type, font_size );
    tree_Apply( Tree, RootNode, Name_Sizing, self, NULL );
    MaxNameHeight =
	fontdesc_FontSummary( NodeFont, treev_GetDrawable( self ) )->maxHeight;
    fontdesc_StringSize( NodeFont, treev_GetDrawable( self ), "M", &MWidth, &MHeight );
    MWidth = MWidth - (MWidth % 2);
    MWidth -= MWidth % 2;
    HalfMWidth = MWidth/2;
    Set_Dimensions( self );
    if ( (MaxPeerCount * ColumnWidth) > DesiredWidth )
      DesiredWidth = MaxPeerCount * ColumnWidth;
    DEBUGdt(MaxPeerCount,MaxPeerCount);  DEBUGdt(DesiredWidth,DesiredWidth);
    if ( (MaxLevelCount * RowHeight) > DesiredHeight )
      DesiredHeight = MaxLevelCount * RowHeight;
    DEBUGdt(MaxLevelCount,MaxLevelCount);  DEBUGdt(DesiredHeight,DesiredHeight);
    }
  OUT(Initialize_Graphics);
  }

static
First_Time( self )
  register struct treev	     *self;
  {
  IN(First_Time);
  if ( RootNode )
    {
    Initialize_Graphics( self );
    Generate_Shadows( self, RootNode );
    ShadowExposed(ShadowNodeDatum(ShadowRootNode)) = true;
    ShadowVisage(ShadowNodeDatum(ShadowRootNode)) = Highlighted;
    CurrentNode = RootNode;
    CurrentNodeShadow = ShadowNodeDatum(ShadowRootNode);
    treev_ExposeNode( self, RootNode );
    }
  OUT(First_Time);
  }

static void
Redisplay( self )
  register struct treev	     *self;
  {
  PendingUpdate = NULL;
  VerticalOffset = PendingVerticalOffset;
  HorizontalOffset = PendingHorizontalOffset;
  treev_SetClippingRect( self, Bounds );
  GreatestRow = GreatestColumn = 0;
  Arrange_Tree( self, ShadowRootNode, Left, Top, Width, Height );
  Fill_Background( self );
  Draw_Tree( self, ShadowRootNode );
  }

static
Set_Dimensions( self )
  register struct treev	     *self;
  {
  IN(Set_Dimensions);
  PendingHorizontalOffset = 0;
  PendingVerticalOffset = 0;		
  if ( NodeWidth )
    CellWidth = NodeWidth;
    else
    CellWidth = MaxNameWidth + MWidth;
  CellWidth -= CellWidth % 2;
  ColumnWidth = CellWidth + HalfMWidth;
  if ( ! VerticalArrangement )
    ColumnWidth += HalfMWidth;
  if ( NodeHeight )
    CellHeight = NodeHeight;
    else
    CellHeight = MaxNameHeight + MWidth;
  CellHeight -= CellHeight % 2;
  if ( NodeBorderStyle == treev_Circle )
      CellHeight = CellWidth;
  RowHeight = CellHeight + MWidth;
  if ( ! VerticalArrangement )
    RowHeight -= HalfMWidth;
  OUT(Set_Dimensions);
  }

static
Check_Dimensions( self, node )
  register struct treev	     *self;
  register tree_type_node     node;
  {
  long			      width, height;
  register struct tree_node  *peer;

  IN(Check_Dimensions);
  fontdesc_StringSize( NodeFont,
	treev_GetDrawable( self ), NodeCaptionName(node), &width, &height );
  if ( width > MaxNameWidth )
    {
    MaxNameWidth = width;
    Set_Dimensions( self );
    PendingUpdate = Redisplay;
    treev_WantUpdate( self, self );
    }
  if ( peer = ChildNode(node) )
    {
    while( peer )
      {
      Check_Dimensions( self, peer );
      peer = RightNode(peer);
      }
    }
  OUT(Check_Dimensions);
  }

enum view_DSattributes
treev__DesiredSize( self, given_width, given_height,
		      pass, desired_width, desired_height )
  register struct treev		  *self;
  register long			   given_width, given_height;
  register enum view_DSpass	   pass;
  register long			  *desired_width, *desired_height;
  {
  register enum view_DSattributes  result = view_WidthFlexible |
					       view_HeightFlexible;

  IN(treev_DesiredSize);
  if ( ScrolledView )
    result = treev_DesiredSize( ScrolledView, given_width, given_height,
		      pass, desired_width, desired_height );
    else
    {
    DEBUGdt(Given Width,given_width);  DEBUGdt(Given Height,given_height);
    DEBUGdt(Pass, pass);
    *desired_width  = ((DesiredWidth > given_width) ? DesiredWidth : given_width);
    *desired_height = DesiredHeight;
    DEBUGdt(Desired Width,*desired_width);  DEBUGdt(Desired Height,*desired_height);
    DEBUGxt(Result-code,result);
    }
  OUT(treev_DesiredSize);
  return result;
  }

void 
treev__FullUpdate( self, type, left, top, width, height )
  register struct treev	         *self;
  register enum view_UpdateType   type;
  register long			  left, top, width, height;
  {
  IN(treev_FullUpdate);
  if ( Tree  &&  (type == view_FullRedraw || type == view_LastPartialRedraw) )
    {
    if ( ScrollView )
      { DEBUG(Use Scrolled View);
      scroll_InsertViewSize( ScrollView, self, left, top, width, height );
      scroll_FullUpdate( ScrollView, type, left, top, width, height );
      DEBUG(Back from Pseudo Update);
      }
    else
    { DEBUG(Use Real View);
    PendingUpdate = NULL;
    VerticalOffset = PendingVerticalOffset;
    HorizontalOffset = PendingHorizontalOffset;
    super_FullUpdate( self, type, left, top, width, height );
    if ( ! treev_BypassUpdate(self) )
      { DEBUG(Not Bypassed);
      GreatestRow = GreatestColumn = 0;
      treev_GetLogicalBounds(self, Bounds);
      treev_SetClippingRect( self, Bounds );
      BackgroundPattern = treev_WhitePattern( self );
      Fill_Background( self );
      if ( ShadowRootNode  &&  ShadowExposed(ShadowNodeDatum(ShadowRootNode)) )
        {
        Arrange_Tree( self, ShadowRootNode, Left, Top, Width, Height );
        Draw_Tree( self, ShadowRootNode );
        }
        else
        { DEBUG(First Time);
	First_Time( self );
	}
      if ( TreeCursor )
        treev_PostCursor( self, Bounds, TreeCursor );
      }
    }
    }
  OUT(treev_FullUpdate);
  }

static
Erase_Node_Children( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct tree_node  *shadow_peer;

  IN(Erase_Node_Children);
  shadow_peer = ChildNode(shadow_node);
  while ( shadow_peer )
    {
    if ( ShadowChildrenExposed(ShadowNodeDatum(shadow_peer)) )
      Erase_Node_Children( self, shadow_peer );
    shadow_peer = RightNode(shadow_peer);
    }
  GreatestRow -= ShadowChildrenRowCount(ShadowNodeDatum(shadow_node));
  if ( GreatestRow <= 0 )    GreatestRow = 1;
  GreatestColumn -= ShadowChildrenColumnCount(ShadowNodeDatum(shadow_node));
  if ( GreatestColumn <= 0 )  GreatestColumn = 1;
  OUT(Erase_Node_Children);
  }

static
Redisplay_Node_Children( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct tree_node  *shadow_peer;

  IN(Redisplay_Node_Children);
    if(shadow_node) {
      Draw_Background( self );
      if( ChildNode(shadow_node) ) {
          VerticalOffset = PendingVerticalOffset;
          HorizontalOffset = PendingHorizontalOffset;
	  if( VerticalArrangement )
	      Arrange_Vertical_Tree( self, ChildNode(shadow_node),
				    Left, Top + GreatestRow * RowHeight, Width, Height );
	  else
	      Arrange_Horizontal_Tree( self, ChildNode(shadow_node),
				      Left + GreatestColumn * ColumnWidth, Top, Width, Height );
	  Draw_Tree( self, ChildNode(shadow_node) );
      }
  }
  else {
      CurrentNode = NULL;
      GreatestColumn = GreatestRow = 0;
      Draw_Background(self);
  }
  OUT(Redisplay_Node_Children);
  }

void
treev__ObservedChanged( self, changed, change )
  register struct treev	     *self;
  register struct observable *changed;
  register long		      change;
  {
  register tree_type_node     parent, tmp_parent, node;
  register node_shadow_type   shadow;

  IN(treev_ObservedChanged);
  if ( ScrolledView )
    treev_ObservedChanged( ScrolledView, changed, change );
  else
  {
  DEBUGdt(Notification Code,tree_NotificationCode(Tree));
  if ( node = tree_NotificationNode(Tree) )
    {
    boolean inExplosion = FALSE;

    DEBUGst(Notification Node,NodeName(node));
    tmp_parent = parent = ParentNode(node);
    while(tmp_parent)
	if(NodeExploded(tmp_parent)) {
	    inExplosion = TRUE;
	    break;
	}
	else tmp_parent = ParentNode(tmp_parent);
    switch ( tree_NotificationCode( Tree ) )
      {
      case  tree_NodeCreated:	    DEBUG(NodeCreated);
      case  tree_NodeHooked:	    DEBUG(NodeHooked);
	Generate_Shadows( self, node );
	if ( parent ) {
	    if(inExplosion) {
		ShadowExposed(NodeShadow(node)) = true;
		treev_FullUpdate(self,view_FullRedraw,0,0,Width,Height);
	    }
	    else {
		Check_Dimensions( self, node );
		treev_ExposeNodeChildren( self, parent );
	    }
	  }
	  else
	  {
	  Initialize_Graphics( self );
          CurrentNode = node;
          CurrentNodeShadow = NodeShadow(node);
          ShadowExposed(CurrentNodeShadow) = true;
	  Check_Dimensions( self, node );
	  treev_ExposeNode( self, node );
	  treev_HighlightNode( self, node );
	  treev_ExposeNodeChildren( self, node );
	  }
        break;
      case  tree_NodeDestroyed:	    DEBUG(NodeDestroyed);
      case  tree_NodeUnhooked:	    DEBUG(NodeUnhooked);
	  if(parent) {
	      if(treev_NodeHighlighted(self,node)) {
	        treev_NormalizeNode(self,node);
		if(CurrentNode = parent) {
   	          CurrentNodeShadow = NodeShadow(CurrentNode);
		  treev_HighlightNode(self,parent);
		}
	      }
	      if(!inExplosion)
		  Erase_Node_Children(self,NodeShadowNode(parent));
	  }
	  Destroy_Shadows(self,node);
	  if(inExplosion)
	      treev_FullUpdate(self,view_FullRedraw,0,0,Width,Height);
	  else
	      Redisplay_Node_Children(self,parent ? NodeShadowNode(parent) : NULL);
	  if(parent && !LeftNode(node) && !RightNode(node)) {
	    ShadowChildrenExposed(ShadowNodeDatum(NodeShadowNode(parent))) = false;
	    ShadowChildrenRowCount(ShadowNodeDatum(NodeShadowNode(parent))) = 0;
	  }
	  break;
      case  tree_NodeChildrenCreated:    DEBUG(NodeChildrenCreated);
	Check_Dimensions( self, node );
	Generate_Children_Shadows( self, node );
	treev_ExposeNodeChildren( self, node );
	break;
      case  tree_NodeChildrenDestroyed:	DEBUG(NodeChildrenDestroyed);
	treev_HideNodeChildren( self, node );
	Destroy_Children_Shadows( self, node );
	break;
      case  tree_NodeNameChanged:	DEBUG(NodeNameChanged);
      case  tree_NodeCaptionChanged:	DEBUG(NodeCaptionChanged);
	Check_Dimensions( self, node );
	shadow = NodeShadow(node);
	Clear_Shadow( self, shadow );
	treev_SetTransferMode( self, graphic_COPY );
	Draw_Node_Caption( self, shadow, NoHalo );
	if ( ShadowHighlighted(shadow) )
	  Highlight_Node_Shadow( self, shadow );
	else
	if ( ShadowFootprinted(shadow) )
          Footprint_Node_Shadow( self, shadow );
	break;
      case  tree_NodeTitleChanged:	DEBUG(NodeTitleChanged);
	Check_Dimensions( self, node );
    	shadow = NodeShadow(node);
	Clear_Shadow( self, shadow );
	treev_SetTransferMode( self, graphic_COPY );
	Draw_Node_Title( self, shadow );
	if ( ShadowHighlighted(shadow) )
	  Highlight_Node_Shadow( self, shadow );
	else
	if ( ShadowFootprinted(shadow) )
          Footprint_Node_Shadow( self, shadow );
	break;
      default:
/*===*/ printf( "TreeView: Unknown Notification-type (%d)\n",
		    tree_NotificationCode( Tree ));
      }
    UpdateScrollbars;
    }
    else
/*===*/ printf( "TreeView: Null Notification-node\n" );
  }
  OUT(treev_ObservedChanged);
  }

void 
treev__Update( self )
  register struct treev	   *self;
  {
  IN(treev_Update);
  if ( ViewLinked && PendingUpdate )
    {
    PendingUpdate( self );
    }
  OUT(treev_Update);
  }

void
treev__SetHitHandler( self, handler, anchor )
  register struct treev	   *self;
  register struct view	 *(*handler)();
  register struct view	   *anchor;
  {
  IN(treev_SetHitHandler);
  HitHandler = handler;
  Anchor = anchor;
  OUT(treev_SetHitHandler);
  }

static struct tree_node *
Which_Node_Hit( self, x, y )
  register struct treev	      *self;
  register long		       x, y;
  {
  register struct tree_node   *node = NULL, *shadow_node;
  register node_shadow_type    shadow;

  IN(Which_Node_Hit);
  shadow_node = ShadowRootNode;
  while ( shadow_node )
    {
    shadow = ShadowNodeDatum(shadow_node);
    if ( ShadowExposed(shadow)  &&
	 x >= SL   &&  y >= ST  &&  x <= SR  &&  y <= SB )
      {
      CurrentNode = node = ShadowedNode(CurrentNodeShadow = shadow);
      break;
      }
    shadow_node = NextShadowNode(shadow_node);
    }
  OUT(Which_Node_Hit);
  return  node;
  }

struct view *
treev__Hit( self, action, x, y, clicks )
  register struct treev		  *self;
  register enum view_MouseAction   action;
  register long			   x, y, clicks;
  {
  register struct tree_node	  *node = NULL;
  register struct view		  *hit;

  IN(treev_Hit );
  if ( ScrollView )
    hit = scroll_Hit( ScrollView, action, x, y, clicks );
  else
  if ( (hit = super_Hit( self, action, x, y, clicks )) == NULL )
    {
    hit = (struct view *) self;
    if ( node = Which_Node_Hit( self, x, y ) )
    { DEBUGst(Node Name,NodeName(node));
    if ( NodeHitHandler(node) )
      { DEBUG(Specific Node HitHandler Found);
      (NodeHitHandler(node))
		( Anchor, self, node, treev_NodeObject, action, x, y, clicks );
      }
      else
      { DEBUG(No Specific Node Hit Handler Found);
      if ( NodeViewObject(node) )
	{ DEBUG(Viewer Node Hit Handler Found);
	Normalize_Other_Nodes( self, node );
	hit = view_Hit( NodeViewObject(node), action,
			view_EnclosedXToLocalX( NodeViewObject(node), x ),
			view_EnclosedYToLocalY( NodeViewObject(node), y ), clicks );
	}
	else
        if ( HitHandler )
	  { DEBUG(General Node Hit Handler Found);
	  (HitHandler)( Anchor, self, node, treev_NodeObject, action, x, y, clicks );
	  }
	  else
	  { DEBUG(No Node Hit Handler Found);
	  if ( node != PriorNode )
	    {
	    PriorNode = node;
	    treev_HighlightNode( self, node );
	    treev_ExposeNodeChildren( self, node );
	    }
	  }
      }
    }
    else { DEBUG(No Node Hit);
    if ( HitHandler )
      { DEBUG(General Node Hit Handler Found);
      (HitHandler)( Anchor, self, node, NULL, action, x, y, clicks );
      }
    }
    }
  OUT(treev_Hit );
  return  hit;
  }

static
Fill_Area( self, mode, tile, shape, left, top, width, height )
  register struct treev	     *self;
  register long		      mode, shape;
  register struct graphic    *tile;
  register long		      left, top, width, height;
  {
  register long		      current_mode;

  IN(Fill_Area);
  if ( (current_mode = treev_GetTransferMode( self )) != mode )
    treev_SetTransferMode( self, mode );
  switch ( shape )
    {
    case  treev_Rectangle:
      treev_FillRectSize( self, left, top, width, height, tile );
      break;
    case  treev_Oval:
    case  treev_Circle:
      treev_FillOvalSize( self, left, top, width, height, tile );
      break;
    case  treev_RoundAngle:
      treev_FillRRectSize( self, left, top, width, height, 10, 10, tile );
      break;
    case  treev_Folder:
      treev_FillRectSize( self, left, top+5, width+1, height-4, tile );
      treev_FillTrapezoid( self, left+4, top, width/3-5,
				       left, top+4, width/3+5, tile );
      break;
    }
  im_ForceUpdate();
  if ( current_mode != mode )
    treev_SetTransferMode( self, current_mode );
  OUT(Fill_Area);
  }

static
Fill_Shadow( self, shadow, mode, tile )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  register long		       mode;
  register struct graphic     *tile;
  {
  register long		       offset = DropShadow * 2;
  
  IN(Fill_Shadow);
  Fill_Area( self, mode, tile, NodeBorderStyle,
	SL+2, ST+2, (SW-3) - offset, (SH-3) - offset );
  OUT(Fill_Shadow);
  }

static
Clear_Shadow( self, shadow )
  register struct treev		*self;
  register struct node_shadow	*shadow;
  {
  register long			 offset = DropShadow * 2;
  
  IN(Clear_Shadow);
  treev_FlipColors(self);
  Fill_Area( self, graphic_COPY, NULL, NodeBorderStyle,
	    SL+1, ST+1, (SW-1) - offset, (SH-1) - offset );
  treev_RestoreColors(self);
  OUT(Clear_Shadow);
  }

void
treev__HighlightNode( self, node )
  register struct treev	      *self;
  register struct tree_node   *node;
  {
  register struct node_shadow *shadow;

  IN(treev_HighlightNode);
  if ( ScrolledView )
    treev_HighlightNode( ScrolledView, node );
  else
  if ( node  &&  !ShadowHighlighted(shadow = NodeShadow(node)) )
    {
    if ( ShadowFootprinted(shadow) )
      Normalize_Node_Shadow( self, shadow );
    Highlight_Node_Shadow( self, shadow );
    Normalize_Other_Nodes( self, shadow );
    }
  OUT(treev_HighlightNode);
  }

boolean
treev__NodeHighlighted( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  if ( ScrolledView )
    return  treev_NodeHighlighted( ScrolledView, node );
    else  if ( node )
      return  NodeHighlighted(node);
      else  return false;
  }

static
Normalize_Other_Nodes( self, node_shadow )
  register struct treev	        *self;
  register struct node_shadow   *node_shadow;
  {
  register struct tree_node	*shadow_node, *given_node;
  register struct node_shadow	*shadow;

  IN(Normalize_Other_Nodes);
  given_node = ShadowedNode(node_shadow);
  shadow_node = ShadowRootNode;
  while ( shadow_node )
    { DEBUGst(Node Name,NodeName(ShadowedNode(ShadowNodeDatum(shadow_node))));
    shadow = ShadowNodeDatum(shadow_node);
    if ( shadow != node_shadow  &&  ! ShadowPlain(shadow)  &&
	  ! tree_NodeAncestor( Tree, ShadowedNode(shadow), given_node ) )
      Normalize_Node_Shadow( self, shadow );
    shadow_node = NextShadowNode(shadow_node);
    }
  shadow_node = NodeShadowNode(ShadowedNode(node_shadow));
  while ( shadow_node )
    { DEBUGst(Node Name,NodeName(ShadowedNode(ShadowNodeDatum(shadow_node))));
    if ( ParentNode(shadow_node) )
      { DEBUGst(Parent Node Name,
	NodeName(ParentNode(ShadowedNode(ShadowNodeDatum(shadow_node)))));
      shadow = ShadowNodeDatum(ParentNode(shadow_node));
      if ( ! ShadowFootprinted(shadow) )
	{
	if ( ! ShadowPlain(shadow) )
          Normalize_Node_Shadow( self, shadow );
        Footprint_Node_Shadow( self, shadow );
	}
      }
    shadow_node = ParentNode(shadow_node);
    }
  OUT(Normalize_Other_Nodes);
  }

void
treev__NormalizeNode( self, node )
  register struct treev		 *self;
  register struct tree_node	 *node;
  {
  register struct node_shadow	 *shadow;

  IN(treev_NormalizeNode);
  if ( ScrolledView )
    treev_NormalizeNode( ScrolledView, node );
  else
  if ( node  &&  ! ShadowPlain(shadow = NodeShadow(node)) )
    Normalize_Node_Shadow( self, shadow );
  OUT(treev_NormalizeNode);
  }

static
Normalize_Node_Shadow( self, shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  {
  register unsigned char       style = NULL;

  IN(Normalize_Node_Shadow);
  if ( ShadowExposed(shadow) )
    {
    if ( ShadowHighlighted(shadow) )
      style = NodeHighlightStyle;
    if ( ShadowFootprinted(shadow) )
      style = NodeFootprintStyle;
    DEBUGdt(Style,style);
    switch ( style )
      {
      case  treev_Invert:
        Fill_Shadow( self, shadow, graphic_INVERT, NULL );
	break;
      case  treev_Pale:
	Clear_Shadow( self, shadow );
	Draw_Node_Title( self, shadow );
	Draw_Node_Caption( self, shadow, NoHalo );
	break;
      case  treev_Border:

	break;
      case  treev_Bold:

	break;
      case  treev_Italic:

	break;
      default: ; /* (Neither Highlighted nor Footprinted) */
      }
    }
  ShadowVisage(shadow) = Plain;
  OUT(Normalize_Node_Shadow);
  }

static
Highlight_Node_Shadow( self, shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  {
  IN(Highlight_Node_Shadow);
  if ( ShadowExposed(shadow) )
    { DEBUGdt(NodeHighlightStyle,NodeHighlightStyle);
    switch ( NodeHighlightStyle )
      {
      case  treev_Invert:
        Fill_Shadow( self, shadow, graphic_INVERT, NULL );
	break;
      case  treev_Pale:
	Fill_Shadow( self, shadow, graphic_COPY, DottedTile );
	Draw_Node_Title( self, shadow );
	Draw_Node_Caption( self, shadow, Halo );
	break;
      case  treev_Border:

	break;
      case  treev_Bold:

	break;
      case  treev_Italic:

	break;
      }
    }
  ShadowVisage(shadow) = Highlighted;
  OUT(Highlight_Node_Shadow);
  }

static
Footprint_Node_Shadow( self, shadow )
  register struct treev		 *self;
  register struct node_shadow	 *shadow;
  {
  IN(Footprint_Node_Shadow);
  if ( ShadowExposed(shadow) )
    { DEBUGdt(NodeFootprintStyle,NodeFootprintStyle);
    switch ( NodeFootprintStyle )
      {
      case  treev_Invert:
        Fill_Shadow( self, shadow, graphic_INVERT, NULL );
	break;
      case  treev_Pale:
	Fill_Shadow( self, shadow, graphic_COPY, DottedTile );
	Draw_Node_Title( self, shadow );
	Draw_Node_Caption( self, shadow, Halo );
	break;
      case  treev_Border:

	break;
      case  treev_Bold:

	break;
      case  treev_Italic:

	break;
      }
    }
  ShadowVisage(shadow) = Footprinted;
  OUT(Footprint_Node_Shadow);
  }

void
treev__HighlightNodeCaption( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  IN(treev_HighlightNodeCaption);
  if ( node  &&  ! NodeViewObject(node) )
    {
/*===*/
    }
  OUT(treev_HighlightNodeCaption);
  }

boolean
treev__NodeCaptionHighlighted( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  if ( node  &&  ! NodeViewObject(node) )
    {
/*===*/
    }
return false;
  }

void
treev__NormalizeNodeCaption( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  IN(treev_NormalizeNodeCaption);
  if ( node  &&  ! NodeViewObject(node) )
    {
/*===*/
    }
  OUT(treev_NormalizeNodeCaption);
  }

void
treev__HideNodeChildren( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *shadow_node;

  IN(treev_HideNodeChildren);
  if ( ScrolledView )
    treev_HideNodeChildren( ScrolledView, node );
  else
  if ( node  &&  ChildNode(node)  &&
       (shadow_node = NodeShadowNode(node))  &&
	  ShadowChildrenExposed(ShadowNodeDatum(shadow_node)) )
    {
    Hide_Node_Children( self, shadow_node );
    Draw_Background( self );
    UpdateScrollbars;
    }
  OUT(treev_HideNodeChildren);
  }

static
Hide_Node_Children( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct tree_node  *shadow_peer;

  IN(Hide_Node_Children);
  shadow_peer = ChildNode(shadow_node);
  while ( shadow_peer )
    {
    if ( ShadowChildrenExposed(ShadowNodeDatum(shadow_peer)) )
      Hide_Node_Children( self, shadow_peer );
    shadow_peer = RightNode(shadow_peer);
    }
  GreatestRow -= ShadowChildrenRowCount(ShadowNodeDatum(shadow_node));
  if ( GreatestRow <= 0 )    GreatestRow = 1;
  GreatestColumn -= ShadowChildrenColumnCount(ShadowNodeDatum(shadow_node));
  if ( GreatestColumn <= 0 )  GreatestColumn = 1;
  Mark_Child_Exposure( self, shadow_node, false, false );
  OUT(Hide_Node_Children);
  }

void
treev__ExposeNodeChildren( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  IN(treev_ExposeNodeChildren);
  if ( ScrolledView )
    treev_ExposeNodeChildren( ScrolledView, node );
  else
  if ( node )
    {
    Expose_Node_Children( self, NodeShadowNode(node) );
    UpdateScrollbars;
    }
  OUT(treev_ExposeNodeChildren);
  }

static
Expose_Node_Children( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct tree_node  *shadow_peer;

  IN(Expose_Node_Children);
  if ( ParentNode(shadow_node) )
    {
    shadow_peer = ChildNode(ParentNode(shadow_node));
    while ( shadow_peer )
      {
      if ( ShadowChildrenExposed(ShadowNodeDatum(shadow_peer)) )
        Hide_Node_Children( self, shadow_peer );
      shadow_peer = RightNode(shadow_peer);
      }
    }
    else  Hide_Node_Children( self, ShadowRootNode );
  Draw_Background( self );
  if ( ChildNode(shadow_node) )
    {
    Mark_Child_Exposure( self, shadow_node, true, false );
    VerticalOffset = PendingVerticalOffset;
    HorizontalOffset = PendingHorizontalOffset;
    if ( VerticalArrangement )
      Arrange_Vertical_Tree( self, ChildNode(shadow_node),
	    Left, Top + GreatestRow * RowHeight, Width, Height );
      else
      Arrange_Horizontal_Tree( self, ChildNode(shadow_node),
	    Left + GreatestColumn * ColumnWidth, Top, Width, Height );
    Draw_Tree( self, ChildNode(shadow_node) );
    }
  OUT(Expose_Node_Children);
  }

boolean
treev__NodeChildrenExposed( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  if ( ScrolledView )
    return  treev_NodeChildrenExposed( ScrolledView, node );
    else  if ( node )
      return  NodeChildrenExposed(node);
      else  return false;
  }

void
treev__HideNode( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *shadow_node;
  register struct tree_node  *parent;

  IN(treev_HideNode);
  if ( ScrolledView )
    treev_HideNode( ScrolledView, node );
  else
    {
    if ( node )
      {
      parent = ParentNode(node);
      if ( treev_NodeHighlighted( self, node ) )
	{
	treev_NormalizeNode( self, node );
	if ( CurrentNode = parent )
	  {
	  CurrentNodeShadow = NodeShadow(CurrentNode);
	  treev_HighlightNode( self, parent );
	  }
	}
      if ( parent )
	{
        if ( shadow_node = NodeShadowNode(parent) )
	  {
	  Hide_Node_Children( self, shadow_node );
	  Expose_Node_Children( self, shadow_node );
	  }
	}
	else
          if ( shadow_node = NodeShadowNode(node) )
	  {
	  Hide_Node_Children( self, shadow_node );
	  GreatestRow = GreatestColumn = 0;
	  Mark_Child_Exposure( self, shadow_node, false, false );
	  }
      ShadowHidden(NodeShadow(node)) = true;
      Draw_Background( self );
      }
    }
  OUT(treev_HideNode);
  }

void
treev__ExposeNode( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *parent;
  register struct tree_node  *shadow_node;

  IN(treev_ExposeNode);
  if ( ScrolledView )
    treev_ExposeNode( ScrolledView, node );
  else
    {
    if ( node )
      {
      ShadowHidden(NodeShadow(node)) = false;
      if ( parent = ParentNode(node) )
        {
/*===
need to expose any parents not now exposed, down to node itself
might need to have parent hide its children, then expose them.
*/
        }
        else
        {
        shadow_node = NodeShadowNode(node);
        Mark_Child_Exposure( self, shadow_node, true, false );
        VerticalOffset = PendingVerticalOffset;
        HorizontalOffset = PendingHorizontalOffset;
	if ( VerticalArrangement )
          Arrange_Vertical_Tree( self, shadow_node,
	    Left, Top + GreatestRow * RowHeight, Width, Height );
	  else
          Arrange_Horizontal_Tree( self, shadow_node,
	    Left + GreatestColumn * ColumnWidth, Top, Width, Height );
        Draw_Tree( self, shadow_node );
        }
      }
    }
  OUT(treev_ExposeNode);
  }

boolean
treev__NodeExposed( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  if ( ScrolledView )
    return  treev_NodeExposed( ScrolledView, node );
    else  if ( node )
      return  NodeExposed(node);
      else  return false;
  }

void
treev__ExplodeNode( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *shadow_node;

  IN(treev_ExplodeNode);
  if ( ScrolledView )
    treev_ExplodeNode( ScrolledView, node );
  else  if ( node )
    {
    treev_HideNodeChildren( self, node );
    shadow_node = NodeShadowNode(node);
    VerticalOffset = PendingVerticalOffset;
    HorizontalOffset = PendingHorizontalOffset;
    Mark_Child_Exposure( self, shadow_node, true, true );
    Arrange_Exploded_Tree( self, shadow_node, Left + GreatestColumn * ColumnWidth,
			    Top + GreatestRow * RowHeight, Width, Height  );
    Draw_Tree( self, shadow_node );
    }
  OUT(treev_ExplodeNode);
  }

void
treev__ImplodeNode( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  IN(treev_ImplodeNode);
  if ( ScrolledView )
    treev_ImplodeNode( ScrolledView, node );
  else  if ( node )
    {
    treev_ExposeNodeChildren( self, node );
    CurrentNode = node;
    CurrentNodeShadow = NodeShadow(node);
    }
  OUT(treev_ImplodeNode);
  }

boolean
treev__NodeExploded( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  if ( ScrolledView )
    return  treev_NodeExploded( ScrolledView, node );
    else  if ( node )
      return  NodeExploded(node);
      else  return false;
  }

static
Printer( self )
  register struct treev	     *self;
  {
  IN(Printer);
  treev_SetPrintOrigin( self, Left, Top );
  treev_SetPrintFont( self, NodeFontName );
  Print_Tree( self, ShadowRootNode );
  OUT(Printer);
  }

static
Print_Tree( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct node_shadow *shadow, *parent_shadow = NULL;
  register struct tree_node   *shadow_parent, *shadow_peer;
  register long		       L, T, R, B;
  register boolean	       folded = false;

  IN(Print_Tree);
  if(shadow_node) {
      L = T = 99999; R = B = 0;
      shadow_peer = shadow_node;
      if ( shadow_parent = ParentNode(shadow_node) )
	  parent_shadow = ShadowNodeDatum(shadow_parent);
      while ( shadow_peer )
      {
	  shadow = ShadowNodeDatum(shadow_peer);
	  DEBUGst(Node Name,NodeName(ShadowedNode(shadow)));
	  if ( ShadowExposed(shadow)  &&  ShadowTop(shadow) < Bottom )
	  { DEBUG(Exposed);
	  Print_Node_Border( self, shadow );
	  treev_PrintString( self, SC, SM /*===*/-1,
			    NodeCaptionName(ShadowedNode(shadow)), 0 );
	  if ( folded = ShadowFolded(shadow) )
	  {
	      if ( SL < L )  L = SL - HalfMWidth;
	      if ( ST < T )  T = ST - HalfMWidth;
	      if ( SR > R )  R = SR + HalfMWidth;
	      if ( SB > B )  B = SB + HalfMWidth;
	  }
	  else
	      if ( parent_shadow )
		  Print_Node_Connector( self, shadow, parent_shadow );
	  if ( ShadowChildrenExposed(shadow) )
	      Print_Tree( self, ChildNode(shadow_peer) );
	  }
	  shadow_peer = RightNode(shadow_peer);
      }
      if ( folded )
      {
	  treev_PrintBox( self, L+1, T+1, R-L-2, B-T-2, 0 );
	  treev_PrintBox( self, L+2, T+2, R-L-4, B-T-4, 0 );
	  if ( parent_shadow  &&  !ShadowFolded(shadow) )
	  {
	      treev_PrintLine( self, SC, T, SC, SB+1 );
	  }
      }
  }
  OUT(Print_Tree);
  }

static
Print_Node_Border( self, shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  {
  IN(Print_Node_Border);
  switch ( NodeBorderStyle )
    {
    case  treev_Rectangle:
      if ( DropShadow )
        { DEBUG(DropShadow);
        treev_PrintBox( self, SL, ST, SW-1, SH-1, 0 );
/*===suppress until Leaf attribute intriduced
	if ( ChildNode(ShadowedNode(shadow)) )
===*/
	  {
          treev_PrintLine( self, SR-1, ST+2, SR-1, SB-1 );
          treev_PrintLine( self, SR-1, SB-1, SL+2, SB-1 );
          treev_PrintLine( self, SR, ST+2, SR, SB );
          treev_PrintLine( self, SR, SB, SL+2, SB );
	  }
        }
        else
        treev_PrintBox( self, SL, ST, SW, SH, 0 );
      break;
    case  treev_Circle:
      treev_PrintCircle( self, SC, SM, SH/2 );
      break;
    case  treev_Oval:
/*===*/
      break;
    case  treev_RoundAngle:
      treev_PrintRoundBox( self, SL, ST, SW, SH, 0 );
      break;
    case  treev_Folder:
/*===*/
      break;
    }
  OUT(Print_Node_Border);
  }

static
Print_Node_Connector( self, shadow, parent_shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow, *parent_shadow;
  {
  IN(Print_Node_Connector);
  if ( !ShadowFolded(shadow)  &&  !ShadowFolded(parent_shadow) )
    { DEBUG(Connect to Parent);
    if ( VerticalArrangement )
      treev_PrintMoveTo( self, SC, ST );
      else
      treev_PrintMoveTo( self, SL, SM );
    switch ( NodeConnectorStyle )
      {
      case  treev_Direct:
	if ( VerticalArrangement )
	  treev_PrintLineTo( self, ShadowCenter(parent_shadow),
				   ShadowBottom(parent_shadow) + 1 );
	  else
	  treev_PrintLineTo( self, ShadowRight(parent_shadow),
				   ShadowMiddle(parent_shadow) );
	break;
      case  treev_DogLeg:
	if ( VerticalArrangement )
	  {
          treev_PrintLineTo( self, SC, ST - HalfMWidth );
          treev_PrintLine( self, SC, ST - HalfMWidth, ShadowCenter(parent_shadow),
				 ShadowBottom(parent_shadow) + HalfMWidth );
          treev_PrintLine( self, ShadowCenter(parent_shadow),
				 ShadowBottom(parent_shadow) + HalfMWidth,
				 ShadowCenter(parent_shadow),
				 ShadowBottom(parent_shadow) + 1 );
	  }
	  else
	  {
          treev_PrintLineTo( self, SL - HalfMWidth, SM );
          treev_PrintLine( self, SL - HalfMWidth, SM,
				ShadowRight(parent_shadow) + HalfMWidth,
				ShadowMiddle(parent_shadow) );
          treev_PrintLine( self, ShadowRight(parent_shadow) + HalfMWidth,
				ShadowMiddle(parent_shadow),
				ShadowRight(parent_shadow),
				ShadowMiddle(parent_shadow) );
	  }
        break;
      }
    }
  OUT(Print_Node_Connector);
  }

void
treev__Print( self, file, processor, format, level )
  register struct treev	     *self;
  register FILE		     *file;
  register char		     *processor;
  register char		     *format;
  register boolean	      level;
  {
  static struct aptv_print_stream   *print_stream;

  IN(treev_Print);
  if ( ScrollView )
    {
    print_stream = treev_PrintStream( self );
    treev_Print( ScrolledView, file, processor, format, level );
    }
  else
  {
  if ( print_stream )
    treev_SetPrintStream( self, print_stream );
  if ( ! GraphicsInitialized )
      return; /* can't print from ezprint yet */
  treev_PrintObject( self, file, processor, format, level, Printer );
  }
  OUT(treev_Print);
  }

struct view *
treev__GetApplicationLayer( self )
  register struct treev	     *self;
  {
  register struct scroll     *view;
  register long		      mode = NULL;

  IN(treev_GetApplicationLayer);
  if ( Scroll & treev_Left )	mode |= scroll_LEFT;
  if ( Scroll & treev_Bottom )	mode |= scroll_BOTTOM;
  view = scroll_Create( self, mode );
  scroll_SetView( view, self );
  OUT(treev_GetApplicationLayer);
  return  (struct view *) view;
  }


static void			      y_getinfo(), y_setframe(),
				      x_getinfo(), x_setframe(),
				      y_endzone(),  x_endzone();
static long			      y_whatisat(), x_whatisat();
static struct scrollfns		      vertical_scroll_interface =
		{ y_getinfo, y_setframe, y_endzone, y_whatisat };
static struct scrollfns		      horizontal_scroll_interface =
		{ x_getinfo, x_setframe, x_endzone, x_whatisat };

struct scrollfns *
treev__GetInterface( self, interface_name )
  register struct treev	     *self;
  register char		     *interface_name;
  {
  register struct scrollfns  *interface = NULL;

  IN(treev_GetInterface);
  DEBUGst(Interface Name,interface_name);
  if ( (Scroll & treev_Left)  &&
	apts_CompareStrings( interface_name, "scroll,vertical" ) == 0 )
    interface = &vertical_scroll_interface;
    else
    if ( (Scroll & treev_Bottom)  &&
	  apts_CompareStrings( interface_name, "scroll,horizontal" ) == 0 )
      interface = &horizontal_scroll_interface;
  OUT(treev_GetInterface);
  return  interface;
  }

static void
y_getinfo( self, total, seen, dot )
  register struct treev	     *self;
  register struct range	     *total, *seen, *dot;
  {
  register long		      extent = GreatestRow * RowHeight;
/* debug=1; */
  IN(y_getinfo);
  total->beg = 0;
  total->end = extent;
  DEBUGdt(VerticalOffset,VerticalOffset);
  seen->beg = -VerticalOffset;
  seen->end = min(-VerticalOffset + Height, extent);
  dot->beg = dot->end = 0;
  DEBUGdt(Total-beg, total->beg);  DEBUGdt(Total-end, total->end);
  DEBUGdt(Seen-beg, seen->beg);  DEBUGdt(Seen-end, seen->end);
  OUT(y_getinfo);
/* debug=0; */
  }

static long
y_whatisat( self, pos, outof )
  register struct treev	     *self;
  register long		      pos, outof;
  {
  register long		      value, coord;
/* debug=1; */
  IN(y_whatisat);
  DEBUGlt(Pos,pos);  DEBUGlt(Outof, outof);

    coord = pos * treev_GetLogicalHeight(self);
    coord /= outof;

  value = -VerticalOffset + coord;
  DEBUGlt(Value,value);
  OUT(y_whatisat);
/* debug=0; */
  return  value;
  }

static void
y_setframe( self, place, pos, outof )
  register struct treev	     *self;
  register int		      place;
  register long		      pos, outof;
  {
/* debug=1; */
  IN(y_setframe);
  DEBUGdt(Place, place);  DEBUGlt(Pos, pos);  DEBUGlt(Outof, outof);
  PendingVerticalOffset = -(place - pos);
  if ( -PendingVerticalOffset >= GreatestRow * RowHeight )
    PendingVerticalOffset = -((GreatestRow-1) * RowHeight);
  DEBUGdt(PendingVerticalOffset,PendingVerticalOffset);
  PendingUpdate = Redisplay;
  treev_WantUpdate( self, self );
  OUT(y_setframe);
/* debug=0; */
  }

static void
y_endzone( self, zone, action )
  register struct treev	     *self;
  register int		      zone, action;
  {
  register long		      nrows, proposed_offset;
/* debug=1; */
  IN(y_endzone);
  DEBUGdt(Zone,zone);  DEBUGdt(Action,action);
  if ( zone == scroll_TOPENDZONE )
    { DEBUG(Top);
    proposed_offset = 0;
    }
    else
    { DEBUG(Bottom);
    nrows = Height / RowHeight;
    proposed_offset = (nrows - 2)*RowHeight - (GreatestRow * RowHeight);
    }
  DEBUGdt(proposed_offset,proposed_offset);
  if (proposed_offset != PendingVerticalOffset) 
    {
    PendingVerticalOffset = proposed_offset; 
    PendingUpdate = Redisplay;
    treev_WantUpdate( self, self );
    }
  OUT(y_endzone);
/* debug=0; */
  }

/* HorizontalOffset will be positive to center image */

static void
x_getinfo( self, total, seen, dot )
  register struct treev	     *self;
  register struct range	     *total, *seen, *dot;
  {
  register long		      extent = GreatestColumn * ColumnWidth;
/*debug=1;*/
  IN(x_getinfo);
  total->beg = 0;
  total->end = extent;
  DEBUGdt(HorizontalOffset,HorizontalOffset);
  seen->beg = max(0, -HorizontalOffset);
  seen->end = min(seen->beg + Width, extent);
  dot->beg = dot->end = 0;
  DEBUGdt(Total-beg, total->beg);  DEBUGdt(Total-end, total->end);
  DEBUGdt(Seen-beg, seen->beg);  DEBUGdt(Seen-end, seen->end);
  OUT(x_getinfo);
/*debug=0;*/
  }

static long
x_whatisat( self, pos, outof )
  register struct treev	     *self;
  register long		      pos, outof;
  {
  register long		      value, coord, extent = GreatestColumn * ColumnWidth;
/*debug=1;*/
  IN(x_whatisat);
  DEBUGlt(Pos,pos);  DEBUGlt(Outof, outof);

    coord = pos * treev_GetLogicalWidth(self);
    coord /= outof;

  value = -HorizontalOffset + coord;
  DEBUGlt(Value,value);
  OUT(x_whatisat);
/*debug=0;*/
  return value;
  }

static void
x_setframe( self, place, pos, outof )
  register struct treev	     *self;
  register long		      place, pos, outof;
  {
  register long		      delta, ncols, extent = GreatestColumn * ColumnWidth;
/*debug=1;*/
  IN(x_setframe);
  DEBUGdt(Place,place);  DEBUGlt(Pos,pos);  DEBUGlt(Outof,outof );
  PendingHorizontalOffset = -(place - pos);
  DEBUGdt(PendingHorizontalOffset,PendingHorizontalOffset);
  if ( -PendingHorizontalOffset >= extent )
    PendingHorizontalOffset = -((GreatestColumn-1) * ColumnWidth);
  DEBUGdt(PendingHorizontalOffset,PendingHorizontalOffset);
  PendingUpdate = Redisplay;
  treev_WantUpdate( self, self );
  OUT(x_setframe);
/*debug=0;*/
  }

static void
x_endzone( self, zone, action )
  register struct treev	     *self;
  register long		      zone, action;
  {
  register long		      proposed_offset, ncols;
/*debug=1;*/
  IN(x_endzone);
  DEBUGdt(Zone,zone);  DEBUGdt(Action,action);
  if ( zone == scroll_TOPENDZONE )
    { DEBUG(Left);
    proposed_offset = 0;
    }
    else
    { DEBUG(Right);
    ncols = Width / ColumnWidth;
    proposed_offset = (ncols-2)*ColumnWidth - (GreatestColumn * ColumnWidth);
    }
  DEBUGdt(proposed_offset,proposed_offset);
  if (proposed_offset != PendingHorizontalOffset) 
    {
    PendingHorizontalOffset = proposed_offset; 
    PendingUpdate = Redisplay;
    treev_WantUpdate( self, self );
    }
  OUT(x_endzone);
/*debug=0;*/
  }

static
Arrange_Tree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  IN(Arrange_Tree);
  if ( VerticalArrangement )
    Arrange_Vertical_Tree( self, shadow_node, left, top, width, height );
    else
    Arrange_Horizontal_Tree( self, shadow_node, left, top, width, height );
  OUT(Arrange_Tree);
  }

static
Arrange_Horizontal_Tree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  register boolean	      folded = false;
  register long		      L = left, T = top, top_offset,
			      peers = 0, column = 0, columns = 1,
			      row = 0, rows, indent;
  register struct node_shadow *shadow;
  register struct tree_node   *shadow_parent, *shadow_peer;

  IN(Arrange_Horizontal_Tree);
  peers = 1 + tree_PeerNodeCount( ShadowTree, shadow_node );
  rows = (height / RowHeight) ? (height / RowHeight) : 1;
  if ( Fold )
    columns = peers / rows + ((peers % rows) ? 1 : 0);
  DEBUGdt(Peers,peers);  DEBUGdt(Columns,columns);  DEBUGdt(Rows,rows);
  GreatestColumn += columns;
  DEBUGdt(GreatestColumn,GreatestColumn);
  if ( peers > rows  &&  Fold )
    { DEBUG(Fold);
    folded = true;
    if ( NodeOrderColumnMajor )
      {
      if ( (indent = ((peers / columns) + ((peers % columns) ? 1 : 0))) <= rows )
        T += (height - (indent * RowHeight)) / 2;
      }
      else
      T += (top_offset = (height - (rows * RowHeight)) / 2);
    }
    else
    T += (height - (peers * RowHeight)) / 2;
  if ( ! folded )    rows = peers;
  GreatestRow = max( GreatestRow, rows );
  shadow_peer = shadow_node;
  while ( shadow_peer )
    {
    shadow = ShadowNodeDatum(shadow_peer);
    DEBUGst(Node Name,NodeName(ShadowedNode(shadow)));
    if ( ShadowExposed(shadow)  &&  !ShadowHidden(shadow) )
      { DEBUG(Exposed);
      ShadowFolded(shadow) = folded;
      if ( ShadowExploded(shadow) )
	Arrange_Horizontal_Exploded_Tree( self, shadow_peer,
	    left + (columns * ColumnWidth), top, width, height );
        else  if ( ChildNode(shadow_peer)  &&  ShadowChildrenExposed(shadow) )
	Arrange_Horizontal_Tree( self, ChildNode(shadow_peer),
	    left + (columns * ColumnWidth), top, width, height );
      if ( NodeOrderRowMajor )
	{
        if ( row++ )
	  if ( row > rows  &&  folded )
            {
            row = 1;
            T = top + top_offset;
	    L += ColumnWidth;
            }
            else  T += RowHeight;
	}
	else
	{
	if ( ! folded  &&  row++ )
	  T += RowHeight;
	  else
	  if ( column++ )
	    if ( column > columns )
	      {
	      column = 1;
	      T += RowHeight;
	      L  = left;
	      }
	      else  L += ColumnWidth;
	}
      SL = L + HalfMWidth + HorizontalOffset;
      ST = T + HalfMWidth + VerticalOffset;
      SW = CellWidth;  SH = CellHeight;
      }
    shadow_peer = RightNode(shadow_peer);
    }
  DEBUG(Peers Done);
  if ( (shadow_parent = ParentNode(shadow_node))  &&
       (shadow = ShadowNodeDatum(shadow_parent)) )
    {
    ShadowChildrenFolded(shadow) = folded;
    ShadowChildrenColumnCount(shadow) = columns;
    DEBUGdt(ColumnCount,ShadowChildrenColumnCount(shadow));
    }
  OUT(Arrange_Horizontal_Tree);
  }

static
Arrange_Vertical_Tree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  register boolean	      folded = false;
  register long		      L = left, T = top, left_offset,
			      peers = 0, column = 0, columns,
			      row = 0, rows = 1, indent;
  register struct node_shadow *shadow;
  register struct tree_node   *shadow_parent, *shadow_peer;

  IN(Arrange_Vertical_Tree);
  peers = 1 + tree_PeerNodeCount( ShadowTree, shadow_node );
  columns = (width / ColumnWidth) ? (width / ColumnWidth) : 1;
  if ( Fold )
    rows = peers / columns + ((peers % columns) ? 1 : 0);
  DEBUGdt(Peers,peers);  DEBUGdt(Columns,columns);  DEBUGdt(Rows,rows);
  GreatestRow += rows;
  DEBUGdt(GreatestRow,GreatestRow);
  if ( peers > columns  &&  Fold )
    { DEBUG(Fold);
    folded = true;
    if ( NodeOrderColumnMajor )
      {
      if ( (indent = ((peers / rows) + ((peers % rows) ? 1 : 0))) <= columns )
        L += (width - (indent * ColumnWidth)) / 2;
      }
      else
      L += (left_offset = (width - (columns * ColumnWidth)) / 2);
    }
    else
    L += (width - (peers * ColumnWidth)) / 2;
  if ( ! folded )    columns = peers;
  GreatestColumn = max( GreatestColumn, columns );
  shadow_peer = shadow_node;
  while ( shadow_peer )
    {
    shadow = ShadowNodeDatum(shadow_peer);
    DEBUGst(Node Name,NodeName(ShadowedNode(shadow)));
    if ( ShadowExposed(shadow)  &&  !ShadowHidden(shadow) )
      { DEBUG(Exposed);
      ShadowFolded(shadow) = folded;
      if ( ShadowExploded(shadow) )
	Arrange_Vertical_Exploded_Tree( self, shadow_peer,
	    left, top + (rows * RowHeight), width, height );
        else  if ( ChildNode(shadow_peer)  &&  ShadowChildrenExposed(shadow) )
        Arrange_Vertical_Tree( self, ChildNode(shadow_peer),
	    left, top + (rows * RowHeight), width, height );
      if ( NodeOrderRowMajor )
	{
        if ( column++ )
	  if ( column > columns  &&  folded )
            {
            column = 1;
            L = left + left_offset;
	    T += RowHeight;
            }
            else  L += ColumnWidth;
	}
	else
	{
	if ( ! folded  &&  column++ )
	  L += ColumnWidth;
	  else
	  if ( row++ )
	    if ( row > rows )
	      {
	      row = 1;
	      T = top;
	      L += ColumnWidth;
	      }
	      else  T += RowHeight;
	}
      SL = L + HalfMWidth + HorizontalOffset;
      ST = T + HalfMWidth + VerticalOffset;
      SW = CellWidth;  SH = CellHeight;
      }
    shadow_peer = RightNode(shadow_peer);
    }
  DEBUG(Peers Done);
  if ( (shadow_parent = ParentNode(shadow_node))  &&
       (shadow = ShadowNodeDatum(shadow_parent)) )
    {
    ShadowChildrenFolded(shadow) = folded;
    ShadowChildrenRowCount(shadow) = rows;
    DEBUGdt(RowCount,ShadowChildrenRowCount(shadow));
    }
  OUT(Arrange_Vertical_Tree);
  }

static
Arrange_Exploded_Tree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  if ( VerticalArrangement )
    Arrange_Vertical_Exploded_Tree( self, shadow_node, left, top, width, height );
    else
    Arrange_Horizontal_Exploded_Tree( self, shadow_node, left, top, width, height );
  }

static long
Compute_Horizontal_Exploded_SubTree_Heights( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct tree_node  *shadow_peer;
  register struct node_shadow *shadow;
  register long		      height = 0;

  IN(Compute_Horizontal_Exploded_SubTree_Heights);
  if ( shadow_peer = ChildNode(shadow_node) )
    while ( shadow_peer )
      {
      height += Compute_Horizontal_Exploded_SubTree_Heights( self, shadow_peer );
      shadow_peer = RightNode(shadow_peer);
      }
    else height = CellHeight + HalfMWidth;
  DEBUGdt(height,height);
  shadow = ShadowNodeDatum(shadow_node);
  ShadowSubHeight(shadow) = height;
  OUT(Compute_Horizontal_Exploded_SubTree_Heights);
  return  height;
  }

static
Arrange_Horizontal_Exploded_Tree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  register long		      sub_height;

  IN(Arrange_Horizontal_Exploded_Tree);
  ShadowExploded(ShadowNodeDatum(shadow_node)) = true;
  sub_height = Compute_Horizontal_Exploded_SubTree_Heights( self, shadow_node );
  DEBUGdt(sub_height,sub_height);
  GreatestRow = sub_height/RowHeight;
  Arrange_Horizontal_Exploded_SubTree( self, shadow_node, left,
			Middle - ShadowSubHeight(ShadowNodeDatum(shadow_node))/2,
			width, ShadowSubHeight(ShadowNodeDatum(shadow_node)) );
  OUT(Arrange_Horizontal_Exploded_Tree);
  }

static
Arrange_Horizontal_Exploded_SubTree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  register struct node_shadow *shadow;
  register struct tree_node  *shadow_peer;
  register long		      T = top, H;

  IN(Arrange_Horizontal_Exploded_SubTree);
  if ( shadow_peer = ChildNode(shadow_node) )
    {
    ShadowChildrenColumnCount(ShadowNodeDatum(shadow_node)) = 1;
    GreatestColumn++;
    while ( shadow_peer )
      {
      shadow = ShadowNodeDatum(shadow_peer);
      ShadowExploded(shadow) = true;
      H = ShadowSubHeight(shadow);
      SL = left + HalfMWidth + HorizontalOffset;
      ST = ((T + H/2) - CellHeight/2) + HalfMWidth + VerticalOffset;
      SW = CellWidth;  SH = CellHeight;
      Arrange_Horizontal_Exploded_SubTree( self, shadow_peer, left + ColumnWidth,
					     T, width, H );
      T += H;
      shadow_peer = RightNode(shadow_peer);
      }
    }
  OUT(Arrange_Horizontal_Exploded_SubTree);
  }

static long
Compute_Vertical_Exploded_SubTree_Widths( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct tree_node  *shadow_peer;
  register struct node_shadow *shadow;
  register long		      width = 0;

  IN(Compute_Vertical_Exploded_SubTree_Widths);
  if ( shadow_peer = ChildNode(shadow_node) )
    while ( shadow_peer )
      {
      width += Compute_Vertical_Exploded_SubTree_Widths( self, shadow_peer );
      shadow_peer = RightNode(shadow_peer);
      }
    else width = CellWidth + HalfMWidth;
  DEBUGdt(width,width);
  shadow = ShadowNodeDatum(shadow_node);
  ShadowSubWidth(shadow) = width;
  OUT(Compute_Vertical_Exploded_SubTree_Widths);
  return  width;
  }

static
Arrange_Vertical_Exploded_Tree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  register long		      sub_width;

  IN(Arrange_Vertical_Exploded_Tree);
  ShadowExploded(ShadowNodeDatum(shadow_node)) = true;
  sub_width = Compute_Vertical_Exploded_SubTree_Widths( self, shadow_node );
  DEBUGdt(Sub-width,sub_width);
  GreatestColumn = sub_width/ColumnWidth;
  Arrange_Vertical_Exploded_SubTree( self, shadow_node,
			Center - ShadowSubWidth(ShadowNodeDatum(shadow_node))/2,
			top, ShadowSubWidth(ShadowNodeDatum(shadow_node)), height );
  OUT(Arrange_Vertical_Exploded_Tree);
  }

static
Arrange_Vertical_Exploded_SubTree( self, shadow_node, left, top, width, height )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register long		      left, top, width, height;
  {
  register struct node_shadow *shadow;
  register struct tree_node  *shadow_peer;
  register long		      L = left, W;

  IN(Arrange_Vertical_Exploded_SubTree);
  if ( shadow_peer = ChildNode(shadow_node) )
    {
    ShadowChildrenRowCount(ShadowNodeDatum(shadow_node)) = 1;
    GreatestRow++;
    while ( shadow_peer )
      {
      shadow = ShadowNodeDatum(shadow_peer);
      ShadowExploded(shadow) = true;
      W = ShadowSubWidth(shadow);
      SL = ((L + W/2) - CellWidth/2) + HalfMWidth + HorizontalOffset;
      ST = top + HalfMWidth + VerticalOffset;
      SW = CellWidth;  SH = CellHeight;
      Arrange_Vertical_Exploded_SubTree( self, shadow_peer, L, top + RowHeight, W, height );
      L += W;
      shadow_peer = RightNode(shadow_peer);
      }
    }
  OUT(Arrange_Vertical_Exploded_SubTree);
  }

static
Mark_Child_Exposure( self, shadow_node, state, recursive )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  register boolean	      state, recursive;
  {
  register struct tree_node  *shadow_peer = ChildNode(shadow_node);
  register struct node_shadow *shadow = ShadowNodeDatum(shadow_node);

  IN(Mark_Child_Exposure);
  ShadowChildrenExposed(shadow) = state;
  if ( state == false )
    {
    ShadowChildrenRowCount(shadow) = 0;
    ShadowExploded(shadow) = false;
    }
  while ( shadow_peer )
    {
    shadow = ShadowNodeDatum(shadow_peer);
    ShadowExposed(shadow) = state;
    if ( state == false )
      ShadowExploded(ShadowNodeDatum(shadow_node)) = false;
    if ( recursive  ||  state == false )
      Mark_Child_Exposure( self, shadow_peer, state, recursive );
    shadow_peer = RightNode(shadow_peer);
    }
  OUT(Mark_Child_Exposure);
  }

static
Draw_Tree( self, shadow_node )
  register struct treev	     *self;
  register struct tree_node  *shadow_node;
  {
  register struct node_shadow *shadow, *parent_shadow = NULL;
  register struct tree_node   *shadow_parent, *shadow_peer;
  register long		       L, T, R, B;
  register boolean	       folded = false;

  IN(Draw_Tree);
  if( ViewLinked )
  {
  DEBUGst(Given Node Name,NodeName(ShadowedNode(ShadowNodeDatum(shadow_node))));
  L = T = 99999; R = B = 0;
  if ( treev_GetFont( self ) != NodeFont )
    treev_SetFont( self, NodeFont );
  shadow_peer = shadow_node;
  if ( shadow_parent = ParentNode(shadow_node) )
    parent_shadow = ShadowNodeDatum(shadow_parent);
  while ( shadow_peer )
    {
    shadow = ShadowNodeDatum(shadow_peer);
    folded = ShadowFolded(shadow);
    DEBUGst(Node Name,NodeName(ShadowedNode(shadow)));
    ShadowVisible(shadow) = false;
    if ( ShadowExposed(shadow) )
      { DEBUG(Exposed);
      if ( SB > (Top - RowHeight)    &&  ST < (Bottom + RowHeight)  &&
	   SR > (Left - ColumnWidth) &&  SL < (Right + ColumnWidth) )
	{
	ShadowVisible(shadow) = true;
        Draw_Node_Border( self, shadow );
	Clear_Shadow( self, shadow );
	Draw_Node_Title( self, shadow );
	Draw_Node_Caption( self, shadow, NoHalo );
	if ( ShadowHighlighted(shadow) )
	  Highlight_Node_Shadow( self, shadow );
	else
	  if ( ShadowFootprinted(shadow) )
            Footprint_Node_Shadow( self, shadow );
	}
      if ( ! folded  &&  parent_shadow )
	Draw_Node_Connector( self, shadow, parent_shadow );
      if ( ChildNode(shadow_peer)  &&  ShadowChildrenExposed(shadow) )
	{ DEBUG(Expose Children);
        Draw_Tree( self, ChildNode(shadow_peer) );
	}
      }
    if ( folded )
      {
      if ( SL < L )  L = SL - HalfMWidth;
      if ( ST < T )  T = ST - HalfMWidth;
      if ( SR > R )  R = SR + HalfMWidth;
      if ( SB > B )  B = SB + HalfMWidth;
      }
    shadow_peer = RightNode(shadow_peer);
    }
  if ( folded )
    {
    treev_DrawRectSize( self, L+1, T+1, R-L-2, B-T-2 );
    treev_DrawRectSize( self, L+2, T+2, R-L-4, B-T-4 );
    if ( parent_shadow  &&  !ShadowFolded(shadow) )
      {
      treev_MoveTo( self, SC, T );
      treev_DrawLineTo( self, SC, SB+1 );
      }
    }
  }
  OUT(Draw_Tree);
  }

static
Draw_Background( self )
  register struct treev	     *self;
  {
  register long		      left, top, width, height;

  IN(Draw_Background);
  if ( ViewLinked && BackgroundShade )
    {
    DEBUGxt(CN,CurrentNode);  DEBUGxt(CNS,CurrentNodeShadow);
    if ( VerticalArrangement )
      {
      left = Left;
      top = Top + VerticalOffset + (GreatestRow * RowHeight) +
	     (DropShadow * 2);
      if ( ! ShadowFolded(CurrentNodeShadow) )
        top -= HalfMWidth;
      top--;
      }
      else
      {
      left = Left + HorizontalOffset + (GreatestColumn * ColumnWidth) +
	    (DropShadow * 2);
      if ( ! ShadowFolded(CurrentNodeShadow) )
	left -= HalfMWidth;
      top = Top;
      }
    width = Right - left + 1;
    height = Bottom - top + 1;
    if ( BackgroundNonWhite )
      treev_SetTransferMode( self, graphic_COPY );
    else {
	treev_FlipColors(self);
    }
    DEBUGdt(l,left);DEBUGdt(t,top);DEBUGdt(w,width);DEBUGdt(h,height);
    treev_FillRectSize( self, left, top, width, height, BackgroundPattern );
    if(!BackgroundNonWhite) treev_RestoreColors(self);
    treev_SetTransferMode( self, graphic_COPY );
    }
  OUT(Draw_Background);
  }

static
Fill_Background( self )
  register struct treev	     *self;
  {
  if ( BackgroundShade )
    {
    if ( BackgroundNonWhite )
      {
      BackgroundPattern = treev_GrayPattern( self, BackgroundShade, 100 );
      treev_SetTransferMode( self, graphic_COPY );
      treev_FillRect( self, Bounds, BackgroundPattern );
      }
      else
      {
      treev_FlipColors(self);
      treev_FillRect( self, Bounds, BackgroundPattern );
      treev_RestoreColors(self);
      }
    treev_SetTransferMode( self, graphic_COPY );
    }
  }

static
Draw_Node_Caption( self, shadow, halo )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  register boolean	       halo;
  {
  register short	       i, j;

  IN(Draw_Node_Caption);
  if ( NodeCaptionName(ShadowedNode(shadow)) )
    { DEBUG(Captioned);
    if ( halo )
    {
	treev_FlipColors(self);
      for ( i = -1; i < 2; i++ )
      for ( j = -1; j < 2; j++ )
	{
        treev_MoveTo( self, SC + i, SM+1 + j );
        treev_DrawString( self, NodeCaptionName(ShadowedNode(shadow)), Balanced );
      }
      treev_RestoreColors(self);
    }
    treev_MoveTo( self, SC, SM+1 );
    treev_DrawString( self, NodeCaptionName(ShadowedNode(shadow)), Balanced );
    }
  OUT(Draw_Node_Caption);
  }

static
Draw_Node_Title( self, shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  {
  IN(Draw_Node_Title);
  if ( NodeTitle(ShadowedNode(shadow)) )
    { DEBUG(Titled);
/*===*/  treev_MoveTo( self, SC, SM+1 );
    treev_DrawString( self, NodeTitle(ShadowedNode(shadow)), Balanced );
    }
  OUT(Draw_Node_Title);
  }

static
Draw_Node_Border( self, shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow;
  {
  IN(Draw_Node_Border);
  switch ( NodeBorderStyle )
    {
    case  treev_Rectangle:
      if ( DropShadow )
        { DEBUG(DropShadow);
        treev_DrawRectSize( self, SL, ST, SW-2, SH-2 );
/*===suppress until Leaf attribute intriduced
	if ( ChildNode(ShadowedNode(shadow)) )
===*/
	  {
          treev_MoveTo( self, SR-1, ST+2 );
          treev_DrawLineTo( self, SR-1, SB-1 );
          treev_DrawLineTo( self, SL+2, SB-1 );
          treev_MoveTo( self, SR, ST+2 );
          treev_DrawLineTo( self, SR, SB );
          treev_DrawLineTo( self, SL+2, SB );
	  }
        }
        else
        treev_DrawRect( self, ShadowBounds(shadow) );
      break;
    case  treev_Oval:
    case  treev_Circle:
      treev_DrawOval( self, ShadowBounds(shadow) );
      break;
    case  treev_RoundAngle:
      treev_DrawRRectSize( self, SL, ST, SW-2, SH-2, 10, 10 );
      break;
    case  treev_Folder:
      treev_MoveTo( self, SL + 5, ST );
      treev_DrawLineTo( self, SL + SW/3, ST );
      treev_DrawLineTo( self, SL + SW/3+4, ST+5 );
      treev_DrawLineTo( self, SR-1, ST+5 );
      treev_DrawLineTo( self, SR-1, SB-1 );
      treev_DrawLineTo( self, SL, SB-1 );
      treev_DrawLineTo( self, SL, ST+5 );
      treev_DrawLineTo( self,SL+5, ST );
      break;
    }
  OUT(Draw_Node_Border);
  }

static
Draw_Node_Connector( self, shadow, parent_shadow )
  register struct treev	      *self;
  register struct node_shadow *shadow, *parent_shadow;
  {
  IN(Draw_Node_Connector);
  if ( !ShadowFolded(shadow)  &&  !ShadowFolded(parent_shadow) )
    { DEBUG(Connect to Parent);
    if ( VerticalArrangement )
      treev_MoveTo( self, SC, ST );
      else
      treev_MoveTo( self, SL, SM );
    switch ( NodeConnectorStyle )
      {
      case  treev_Direct:
	if ( VerticalArrangement )
	  treev_DrawLineTo( self, ShadowCenter(parent_shadow),
				  ShadowBottom(parent_shadow) + 1 );
	  else
	  treev_DrawLineTo( self, ShadowRight(parent_shadow),
				  ShadowMiddle(parent_shadow) );
	break;
      case  treev_DogLeg:
	if ( VerticalArrangement )
	  {
          treev_DrawLineTo( self, SC, ST - HalfMWidth );
          treev_DrawLineTo( self, ShadowCenter(parent_shadow),
				  ShadowBottom(parent_shadow) + HalfMWidth );
          treev_DrawLineTo( self, ShadowCenter(parent_shadow),
				  ShadowBottom(parent_shadow) + 1 );
	  }
	  else
	  {
          treev_DrawLineTo( self, SL - HalfMWidth, SM );
          treev_DrawLineTo( self, ShadowRight(parent_shadow) + HalfMWidth,
				  ShadowMiddle(parent_shadow) );
          treev_DrawLineTo( self, ShadowRight(parent_shadow),
				  ShadowMiddle(parent_shadow) );
	  }
        break;
      }
    }
  OUT(Draw_Node_Connector);
  }

static struct node_shadow *
Node_Shadow( self, node )   /*=== might want to optimize !!! ===*/
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *candidate;
  register struct node_shadow *shadow = NULL, *shadow_candidate;

  IN(Node_Shadow);
  if ( node )
    { DEBUGst(Node Name,tree_NodeName( Tree, node ) );
    candidate = ShadowRootNode;
    while ( candidate )
      {
      shadow_candidate = ShadowNodeDatum(candidate);
      DEBUGst(Candidate Name,tree_NodeName( Tree, ShadowedNode(shadow_candidate) ) );
      if ( ShadowedNode(shadow_candidate) == node )
	{
	shadow = shadow_candidate;
	break;
	}
      candidate = NextShadowNode(candidate);
      }
    }
  OUT(Node_Shadow);
  return  shadow;
  }

tree_type_node
Node_Shadow_Node( self, node )   /*=== might want to optimize !!! ===*/
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *candidate, *shadow_node = NULL;
  register struct node_shadow *shadow = NULL;

  IN(Node_Shadow_Node);
  if ( node )
    { DEBUGst(Node Name,tree_NodeName( Tree, node ) );
    candidate = ShadowRootNode;
    while ( candidate )
      {
      shadow = ShadowNodeDatum(candidate);
      DEBUGst(Candidate Name,NodeName(ShadowedNode(shadow)));
      if ( ShadowedNode(shadow) == node )
	{
	shadow_node = candidate;
	break;
	}
      candidate = NextShadowNode(candidate);
      }
    }
  OUT(Node_Shadow_Node);
  return  shadow_node;
  }

static struct node_shadow *
Create_Shadow( self )
  register struct treev	      *self;
  {
  register struct node_shadow *shadow;

  if ( (shadow = (struct node_shadow *)
	    calloc( 1, sizeof(struct node_shadow) )) == NULL )
    { DEBUG(ERROR -- No Space);
/*===*/ printf( "TreeView:  No ShadowTree Node Space.\n" );
    Destroy_Shadows( self, RootNode );
    }
  return  shadow;
  }

static
Generate_Shadows( self, node )
  register struct treev	      *self;
  register struct tree_node   *node;
  {
  register long		       status = ok;
  register struct node_shadow *shadow = NULL;
  register struct tree_node   *shadow_node = NULL;

  IN(Generate_Shadows);
  if ( node )
    {
    if ( shadow = Create_Shadow( self ) )
      {
      ShadowedNode(shadow) = node;
      if ( ShadowRootNode )
        {
        if ( LeftNode(node) )
	  shadow_node = tree_CreateRightNode( ShadowTree, NULL,
			shadow, NodeShadowNode(LeftNode(node)) );
	  else
	  if ( RightNode(node) )
	    shadow_node = tree_CreateLeftNode( ShadowTree, NULL,
			    shadow, NodeShadowNode(RightNode(node)) );
	    else
	    if ( ParentNode(node) )
	      shadow_node = tree_CreateChildNode( ShadowTree, NULL,
			    shadow, NodeShadowNode(ParentNode(node)) );
	      else
	      { DEBUG(Orphan Node);
	      status = failure;
	      }
        }
        else  shadow_node = tree_CreateRootNode( ShadowTree, NULL, shadow );
      }
      else
      { DEBUG(ERROR -- No Space);
      status = failure;
      }
    if ( treev_debug  &&  shadow_node )
      tree_SetNodeName( ShadowTree, shadow_node, NodeName(node) );
    if ( shadow_node  &&  ChildNode(node) )
      status = Generate_Children_Shadows( self, node );
    Dump_Trees( self );
    }
  OUT(Generate_Shadows);
  return  status;
  }

static
Generate_Children_Shadows( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register long		      status = ok, i, current_level, node_level, start_level;
  register struct node_shadow *shadow = NULL;
  register struct tree_node   *shadow_node = NULL, *shadow_node_parent = NULL;

  IN(Generate_Children_Shadows);
  current_level = start_level = NodeLevel(node);  DEBUGdt(Start Level,start_level);
  shadow_node = NodeShadowNode(node);
  while ( (node = NextNode(node))  &&  (node_level = NodeLevel(node)) > start_level )
    { DEBUGst(Node Name,NodeName(node));  DEBUGdt(Level,node_level);
    if ( shadow = Create_Shadow( self ) )
      {
      ShadowedNode(shadow) = node;
      if ( node_level == current_level )
	{ DEBUG(Same Level);
	shadow_node = tree_CreateRightNode( ShadowTree, NULL, shadow, shadow_node );
	}
	else
	  if ( node_level > current_level )
	    { DEBUG(Deeper Level);
	    shadow_node_parent = shadow_node;
	    shadow_node = tree_CreateChildNode( ShadowTree, NULL,
				shadow, shadow_node_parent );
	    }
	else
	  { DEBUG(Higher Level);
	  for ( i = current_level - node_level; i; i-- )
	    shadow_node_parent = shadow_node = tree_ParentNode( ShadowTree, shadow_node );
	  shadow_node = tree_CreateRightNode( ShadowTree, NULL, shadow, shadow_node );
	  }
      if ( treev_debug )
	tree_SetNodeName( ShadowTree, shadow_node, NodeName(node) );
      current_level = node_level;
      }
      else
      {
      status = failure;
      break;
      }
    }
  OUT(Generate_Children_Shadows);
  return  status;
  }

static
Destroy_Shadows( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *shadow_node;

  IN(Destroy_Shadows);
  if ( node  &&  (shadow_node = NodeShadowNode(node)) )
    { DEBUGst(Prime Node Name,NodeName(node));
    if ( ChildNode( node ) )
      Destroy_Children_Shadows( self, node );
    if ( ShadowNodeDatum(shadow_node) )
      free( ShadowNodeDatum(shadow_node) );
    tree_DestroyNode( ShadowTree, shadow_node );
    if ( tree_RootNode( ShadowTree ) == NULL )
      MaxNameWidth = MaxPeerCount = MaxLevelCount = 0;
    }
  Dump_Trees( self );
  OUT(Destroy_Shadows);
  }

static
Destroy_Children_Shadows( self, node )
  register struct treev	     *self;
  register struct tree_node  *node;
  {
  register struct tree_node  *shadow_node, *next;
  register long		      level;

  IN(Destroy_Children_Shadows);
  if ( node )
    { DEBUGst(Prime Node Name,NodeName(node));
    shadow_node = NodeShadowNode(node);
    level = tree_NodeLevel( ShadowTree, shadow_node );
    next = NodeShadowNode(ChildNode(node));
    while ( next )
    {DEBUGst(Node Name,NodeName(ShadowedNode(ShadowNodeDatum(next))));
      if ( ShadowNodeDatum(next) )
	free( ShadowNodeDatum(next) );
      if ( (next = NextShadowNode(next))  &&
	    tree_NodeLevel( ShadowTree, next ) <= level )
	break;
      }
    tree_DestroyNodeChildren( ShadowTree, shadow_node );
    }
  Dump_Trees( self );
  OUT(Destroy_Children_Shadows);
  }

static
Dump_Trees( self )
  register struct treev	     *self;
  {
  register struct tree_node  *node;

  IN(Dump_Trees);
  if ( !treev_debug ) return;
  tree_SetDebug( Tree, 0 );
  printf("\nNodes");
  node = RootNode;
  while ( node )
    {
    printf( "\n%*s%s",2*tree_NodeLevel(Tree,node),"",
	    NodeCaptionName(node));
    node = NextNode(node);
    }
  printf("\nShadows");
  node = ShadowRootNode;
  while ( node )
    {
    printf( "\n%*s%s",2*tree_NodeLevel(ShadowTree,node),"",
	    NodeCaptionName(ShadowedNode(ShadowNodeDatum(node))));
    node = NextShadowNode(node);
    }
  printf("\n");
  tree_SetDebug( Tree, 1 );
  OUT(Dump_Trees);
  }

void
treev__LinkTree(self, parent)
    struct treev *self;
    struct view *parent;
{
    super_LinkTree(self, parent);
    if(ScrollView) {
	scroll_LinkTree(ScrollView, parent);
	scroll_SetView(ScrollView, ScrolledView);
    }
    else {
	super_LinkTree(self, parent);
    }
}

void
treev__UnlinkTree(self)
    struct treev *self;
{
    super_UnlinkTree(self);
    if(ScrollView) {
	scroll_UnlinkTree(ScrollView);
    }
    else {
	super_UnlinkTree(self);
    }
}
