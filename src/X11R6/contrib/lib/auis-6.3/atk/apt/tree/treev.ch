/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Tree View-Class

MODULE	treev.ch

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Tree View-Class.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/14/88	Created (TCP)
  08/24/89	Upgrade to Version 1.0 (TCP)
		Pass object-type arg to Hit-handlers (TCP)

END-SPECIFICATION  ************************************************************/



#define  treev_VERSION	      1

class treev : aptv
  {
overrides:

  SetDataObject( struct tree *data );
  DesiredSize( long width, long height, enum view_DSpass pass, long *dWidth, long *dheight )
								returns enum view_DSattributes;
  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  Update();
  ObservedChanged( struct view *changed, long value );
  Hit( enum view_MouseAction action, long x, long y, long n)	returns struct view *;
  Print( FILE *file, char *processor, char *finalFormat, boolean topLevel );
  GetApplicationLayer()						returns struct view *;
  GetInterface( char *interface_name )				returns struct scrollfns *;

  LinkTree( struct view *parent );
  UnlinkTree();

methods:

  /**  Methods Dealing with Tree as a Whole  **/

  SetTreeAttribute( long attribute_codevalue )			returns long;
  TreeAttribute( long attribute_code )				returns long;
  CurrentNode()							returns struct tree_node *;
  SetHitHandler( (long *handler)(), char *anchor );
  SetDebug( boolean state );

  /**  Methods Dealing with Individual Nodes  **/

  ExposeNode( struct tree_node *node );
  NodeExposed( struct tree_node *node )				returns boolean;
  HideNode( struct tree_node *node );
  ExplodeNode( struct tree_node *node );
  NodeExploded( struct tree_node *node )			returns boolean;
  ImplodeNode( struct tree_node *node );
  ExposeNodeChildren( struct tree_node *node );
  NodeChildrenExposed( struct tree_node *node )			returns boolean;
  HideNodeChildren( struct tree_node *node );
  HighlightNode( struct tree_node *node );
  NodeHighlighted( struct tree_node *node )			returns boolean;
  NormalizeNode( struct tree_node *node );
  HighlightNodeCaption( struct tree_node *node );
  NodeCaptionHighlighted( struct tree_node *node )		returns boolean;
  NormalizeNodeCaption( struct tree_node *node );

macromethods:

classprocedures:

  Create( treev_Specification, struct view *anchor )		returns struct treev *;
  InitializeObject( struct treev *self )			returns boolean;
  FinalizeObject( struct treev *self );

data:

  struct treev_instance		     *instance;
  };

/***  Object Types  ***/
#define  treev_NodeObject		    (1)
#define  treev_TitleObject		    (2)

/***  Node Ordering  ***/
#define  treev_ColumnMajor		    (0)
#define  treev_RowMajor			    (1<<0)

/***  Node Border Styles  ***/
#define  treev_Rectangle		    (0)
#define  treev_Circle			    (1<<0)
#define  treev_Oval			    (1<<1)
#define  treev_RoundAngle		    (1<<2)
#define  treev_Roundangle		    (1<<2)
#define	 treev_Folder			    (1<<3)

/***  Tree Scrolling  ***/
#define  treev_NoVertical		    (1<<0)
#define  treev_NoHorizontal		    (1<<1)

/***  Node Filigree  ***/
#define  treev_NoDropShadow		    (0)
#define  treev_DropShadow		    (1<<0)

/***  Node Connector Styles  ***/
#define  treev_Direct			    (0)
#define  treev_DogLeg			    (1<<0)
#define  treev_Fold			    (1<<1)
#define  treev_NoFold			    (1<<2)

/****  Placements  ***/
#define  treev_Center			    (0)
#define  treev_Left			    (1<<0)
#define  treev_Right			    (1<<1)
#define  treev_Top			    (1<<2)
#define  treev_Bottom			    (1<<3)

/***  Highlight & FootPrint Styles ***/
#define  treev_Invert			    (0)
#define  treev_Border			    (1<<0)
#define  treev_Bold			    (1<<1)
#define  treev_Italic			    (1<<2)
#define  treev_Pale			    (1<<3)

/***  Arrangements ***/
#define  treev_Horizontal		    (0)
#define  treev_Vertical			    (1<<0)

/***  Attribute Specifications  ***/
#define  treev_NodeHighlightStyle(x)	    treev_nodehighlightstyle,	(long) x
#define  treev_NodeFootPrintStyle(x)	    treev_nodefootprintstyle,	(long) x
#define  treev_NodeBorderStyle(x)	    treev_nodeborderstyle,	(long) x
#define  treev_Scroll(x)		    treev_scroll,		(long) x
#define  treev_NodeConnectorStyle(x)	    treev_nodeconnectorstyle,	(long) x
#define  treev_NodeFontName(x)		    treev_nodefontname,		(long) x
#define  treev_NodeFiligree(x)		    treev_nodefiligree,	    	(long) x
#define  treev_HitHandler(x)		    treev_hithandler,		(long) x
#define  treev_Arrangement(x)		    treev_arrangement,		(long) x
#define  treev_BackgroundShade(x)	    treev_backgroundshade,	(long) x
#define  treev_NodeWidth(x)		    treev_nodewidth,		(long) x
#define  treev_NodeHeight(x)		    treev_nodeheight,		(long) x
#define  treev_Cursor(x)		    treev_cursor,		(long) x
#define  treev_CursorFontName(x)	    treev_cursorfontname,	(long) x
#define  treev_NodeOrder(x)		    treev_nodeorder,		(long) x

/***  Tree View Attribute Codes  ***/

#define  treev_nodeborderstyle		    1
#define  treev_scroll			    2
#define  treev_nodehighlightstyle	    3
#define  treev_nodeconnectorstyle	    4
#define  treev_nodefontname		    5
#define  treev_titlefontname		    6
#define  treev_nodefiligree		    7
#define  treev_hithandler		    8
#define  treev_arrangement		    9
#define  treev_backgroundshade		   10
#define  treev_nodewidth		   11
#define  treev_nodeheight		   12
#define  treev_nodefootprintstyle	   13
#define  treev_cursor			   14
#define  treev_cursorfontname		   15
#define  treev_nodeorder		   16


typedef struct treev_specification  treev_Specification;
struct  treev_specification
  {
  char	attribute;
  long	value;
  };
