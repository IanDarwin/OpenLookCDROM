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

TITLE	The Tree Data-Class

MODULE	tree.ch

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Tree Data-Class.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  02/14/88	Created (TCP)
  05/19/89	Add NodeAncestry method (TCP)
  06/05/89	Add NodeAncestor method (TCP)
  08/24/89	Upgrade to Version 1.0 (TCP)
  08/31/89	Change OfData to OfDatum (TCP)

END-SPECIFICATION  ************************************************************/



#define  tree_VERSION	      1

class tree : apt
  {
overrides:

methods:

  /**  Methods Dealing with Tree as a Whole  **/

  SetTreeAttribute( long attribute_code, long attribute_value )	returns long;
  TreeAttribute( long attribute_code )				returns long;
  TreeWidth()							returns long;
  TreeHeight()							returns long;
  SetTreeModified( boolean state );
  TreeModified()						returns boolean;
  SetDebug( boolean state );

  /**  Methods Dealing with Individual Nodes  **/

  CreateRootNode( char *name, long datum )			returns struct tree_node *;
  CreateParentNode( char *name, long datum, struct tree_node *child ) 
								returns struct tree_node *;
  CreateChildNode( char *name, long datum, struct tree_node *parent ) 
								returns struct tree_node *;
  CreateRightNode( char *name, long datum, struct tree_node *left ) 
								returns struct tree_node *;
  CreateLeftNode( char *name, long datum, struct tree_node *right ) 
								returns struct tree_node *;
  DestroyNode( struct tree_node *node );
  DestroyNodeChildren( struct tree_node *node );
  NextNode( struct tree_node *node )				returns struct tree_node *;
  PriorNode( struct tree_node *node )				returns struct tree_node *;
  NodeOfName( char *name, struct tree_node *node )		returns struct tree_node *;
  NodesOfName( char *name, struct tree_node *node )		returns struct tree_node **;
  NodeOfDatum( long datum, struct tree_node *node )		returns struct tree_node *;
  NodesOfDatum( long datum, struct tree_node *node )		returns struct tree_node **;
  HookNode( struct tree_node *node, struct tree_node *parent, 
		struct tree_node *left, struct tree_node *right ) 
								returns struct tree_node *;
  UnHookNode( struct tree_node *node )				returns struct tree_node *;
  MoveNode( struct tree_node *node, struct tree_node *parent, 
		struct tree_node *left, struct tree_node *right )
								returns struct tree_node *;
  DuplicateNode( struct tree_node *node, struct tree_node *parent, 
		struct tree_node *left, struct tree_node *right )
								returns struct tree_node *;
  Apply( struct tree_node *node, (long(*)()) proc, char *anchor, char *datum )
								returns long;
  SetNodeName( struct tree_node *node, char *name )		returns boolean;
  SetNodeCaption( struct tree_node *node, char *caption )	returns boolean;
  SetNodeTitle( struct tree_node *node, char *title )		returns boolean;
  SetNodeDatum( struct tree_node *node, long datum )		returns boolean;
  NodeCount( struct tree_node *node )				returns long;
  PeerNodeCount( struct tree_node *node )			returns long;
  ChildNodeCount( struct tree_node *node )			returns long;
  LeafNodeCount( struct tree_node *node )			returns long;
  NodeLevel( struct tree_node *node )				returns long;
  NodePosition( struct tree_node *node )			returns long;
  NodeIndex( struct tree_node *node )				returns char *;
  NodeAncestry( struct tree_node *node, char *separator )	returns char *;
  NodeAncestor( struct tree_node *candidate, struct tree_node *node ) returns boolean;

macromethods:

  RootNode()	    		      ((self)->root_node)
  NodeName( node )		      ((node) ? node->name : NULL)
  NodeCaption( node )		      ((node) ? node->caption : NULL)
  NodeTitle( node )		      ((node) ? node->title : NULL)
  NodeDatum( node )		      ((node) ? node->datum : NULL)
  ParentNode( node )		      ((node) ? node->parent : NULL)
  ChildNode( node )		      ((node) ? node->child : NULL)
  LeftNode( node )		      ((node) ? node->left : NULL)
  RightNode( node )		      ((node) ? node->right : NULL)

  SetNotificationCode(code)	      ((self)->notification_code = code)
  NotificationCode()	    	      ((self)->notification_code)
  SetNotificationNode(node)	      ((self)->notification_node = node)
  NotificationNode()		      ((self)->notification_node)

classprocedures:

  Create( tree_Specification, char *anchor )   		returns struct tree *;
  InitializeObject( struct tree *self )			returns boolean;
  FinalizeObject( struct tree *Self );

data:

  struct tree_node	  	 *root_node,
			  	 *unhooked_nodes;
  struct dataobject		 *anchor;
  char				  traversal_order, tree_modified;
  char				  notification_code;
  struct tree_node		 *notification_node;
  };

/***  Node Object Structure  ***/
typedef  struct tree_node	     *tree_type_node;
struct tree_node
  {
  struct tree_node		     *parent, *child, *left, *right;
  char				     *name, *caption, *title;
  long				      datum;
  long				      mode;
  boolean			      modified;
  };

/***  Ordering  ***/
#define  tree_PreOrder				    (0)
#define  tree_PostOrder				    (1<<0)

/***  Attribute Specifications  ***/
#define  tree_Order(x)	tree_order,		    (long) x

/***  Notification Codes  ***/
#define  tree_NodeCreated			 1
#define  tree_NodeDestroyed			 2
#define  tree_NodeUnhooked			 3
#define  tree_NodeHooked			 4
#define  tree_NodeNameChanged			 5
#define  tree_NodeCaptionChanged		 6
#define  tree_NodeTitleChanged			 7
#define  tree_NodeChildrenDestroyed		 8
#define  tree_NodeChildrenCreated		 9
#define  tree_NodeMoved				10
#define  tree_NodeDuplicated			11

/***  Tree Attribute Codes  ***/
#define  tree_order				 1


typedef struct tree_specification  tree_Specification;
struct  tree_specification
  {
  char	attribute;
  long	value;
  };

