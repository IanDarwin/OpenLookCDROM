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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apt/tree/RCS/tree.c,v 1.10 1992/12/15 21:26:22 rr2b R6tape $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Tree Data-Class

MODULE	tree.c

VERSION	1.0

NOTICE	IBM Internal Use Only

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



#include <apt.h>
#include <tree.eh>
#include <ctype.h>

static char tree_debug = 0;
#define  RootNode		((self)->root_node)
#define  ParentNode(node)	((node)->parent)
#define  ChildNode(node)	((node)->child)
#define  LeftNode(node)		((node)->left)
#define  RightNode(node)	((node)->right)
#define  NodeName(node)	    	((node)->name)
#define  NodeCaption(node)	((node)->caption)
#define  NodeTitle(node)	((node)->title)
#define  NodeDatum(node)	((node)->datum)
#define  NodeMode(node)	    	((node)->mode)
#define  NodeModified(node)	((node)->modified)
#define  NodeLevel(node)	(tree_NodeLevel(self,node))
#define  NextNode(node)		(tree_NextNode(self,node))
#define  UnHookedNodes		(self->unhooked_nodes)
#define  TreeModified		(self->tree_modified)

#define  Anchor			(self->anchor)
#define  TraversalOrder		(self->traversal_order)

struct tree *
tree__Create( ClassID, specification, anchor )
  register struct  classheader  *ClassID;
  tree_Specification	        *specification;
  register struct dataobject    *anchor;
  {
  register struct tree		*self;

  IN(tree_Create);
  self = tree_New();
  Anchor = anchor;
  while ( specification  &&  specification->attribute )
    {
    tree_SetTreeAttribute( self, specification->attribute, specification->value );
    specification++;
    }
  OUT(tree_Create);
  return self;
  }

boolean 
tree__InitializeObject( classID, self)
  register struct classheader *classID;
  register struct tree	      *self;
  {
  IN(tree_InitializeObject);
  DEBUGst(RCSID,rcsid);
  RootNode = UnHookedNodes = NULL;
  TreeModified = false;
  OUT(tree_InitializeObject);
  return TRUE;
  }

void
tree__FinalizeObject( classID, self )
  register struct classheader *classID;
  register struct tree	      *self;
  {
  IN(tree_FinalizeObject );
  tree_DestroyNode( self, RootNode );
  if ( UnHookedNodes )
    {
/*===*/
    }
/*===*/
  OUT(tree_FinalizeObject );
  }

long
tree__SetTreeAttribute( self, attribute, value )
  register struct tree	     *self;
  register long		      attribute, value;
  {
  register long		      status = ok;

  IN(tree_SetTreeAttribute);
  switch ( attribute )
    {
    case  tree_order:	DEBUG(order);
      TraversalOrder = value;
    break;

    default:/*===*/
	printf( "Tree: ERROR -- Unrecognized Attribute '%d' (Ignored)\n", attribute );
    }
  OUT(tree_SetTreeAttribute);
  return  status;
  }

long
tree__TreeAttribute( self, attribute )
  register struct tree	     *self;
  register long		      attribute;
  {
  register long		      value = NULL;

  IN(tree_TreeAttribute);
  switch ( attribute )
    {
    case  tree_order:	DEBUG(order);
      value = TraversalOrder;
    break;

    default:/*===*/
	printf( "Tree: ERROR -- Unrecognized Attribute '%d'\n", attribute );
    }
  OUT(tree_TreeAttribute);
  return  value;
  }

static
tree_type_node
Build_Node( self, name, datum )
  register struct tree	     *self;
  register char	    	     *name;
  register long		      datum;
  {
  register struct tree_node  *node;

  IN(Build_Node);
  DEBUGst(Name,name);
  if ( node = (struct tree_node *) calloc( 1, sizeof(struct tree_node) ) )
    {
    tree_SetNodeDatum( self, node, datum );
    tree_SetNodeName( self, node, name );
    }
  OUT(Build_Node);
  return  node;
  }

tree_type_node
tree__CreateRootNode( self, name, datum )
  register struct tree	     *self;
  register char	    	     *name;
  register long		      datum;
  {
  register tree_type_node     node = NULL;

  IN(tree_CreateRootNode);
  if ( RootNode == NULL  &&  (node = Build_Node( self, name, datum )) )
    {
    RootNode = node;
    }
  OUT(tree_CreateRootNode);
  return  node;
  }

tree_type_node
tree__CreateParentNode( self, name, datum, child )
  register struct tree	     *self;
  register char	    	     *name;
  register long		      datum;
  register tree_type_node     child;
  {
  register tree_type_node     node = NULL;

  IN(tree_CreateParentNode);
/*===*/
  OUT(tree_CreateParentNode);
  return  node;
  }

tree_type_node
tree__CreateChildNode( self, name, datum, parent )
  register struct tree	     *self;
  register char	    	     *name;
  register long		      datum;
  register tree_type_node     parent;
  {
  register tree_type_node     node = NULL,  prior = NULL;

  IN(tree_CreateChildNode);
  if ( parent  &&  (node = Build_Node( self, name, datum )) )
    {
    ParentNode(node) = parent;
    if ( prior = ChildNode(parent) )
      {
      while ( RightNode(prior) )
	prior = RightNode(prior);
      RightNode(prior) = node;
      LeftNode(node) = prior;
      }
      else  ChildNode(parent) = node;
    }
    else
    if ( parent == NULL )
      node = tree_CreateRootNode(  self, name, datum );
  OUT(tree_CreateChildNode);
  return  node;
  }

tree_type_node
tree__CreateRightNode( self, name, datum, left )
  register struct tree	     *self;
  register char	    	     *name;
  register long		      datum;
  register tree_type_node     left;
  {
  register tree_type_node     node = NULL;

  IN(tree_CreateRightNode);
  if ( left  &&  (node = Build_Node( self, name, datum )) )
    {
    ParentNode(node) = ParentNode(left);
    RightNode(node) = RightNode(left);
    RightNode(left) = node;
    LeftNode(node) = left;
    if ( RightNode(node) )
      LeftNode(RightNode(node)) = node;
    }
  OUT(tree_CreateRightNode);
  return  node;
  }

tree_type_node
tree__CreateLeftNode( self, name, datum, right )
  register struct tree	     *self;
  register char	    	     *name;
  register long		      datum;
  register tree_type_node     right;
  {
  register tree_type_node     node = NULL;

  IN(tree_CreateLeftNode);
  if ( right  &&  (node = Build_Node( self, name, datum )) )
    {
    ParentNode(node) = ParentNode(right);
    LeftNode(node) = LeftNode(right);
    LeftNode(right) = node;
    RightNode(node) = right;
    if ( LeftNode(node) )
      RightNode(LeftNode(node)) = node;
    }
  OUT(tree_CreateLef_Node);
  return  node;
  }

void
tree__DestroyNode( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register tree_type_node     child, peer;

  IN(tree_DestroyNode);
  if ( node  ||  (node = RootNode) )
    { DEBUGst(Name,NodeName(node));
    if ( child = ChildNode(node) )
      while (child )
	{
        if ( ChildNode(child) )
          tree_DestroyNode( self, ChildNode(child) );
	peer = RightNode(child);
        tree_DestroyNode( self,child );
	child = peer;
	}
    if ( LeftNode(node) )
      { DEBUG(Adjust Left);
      RightNode(LeftNode(node)) = RightNode(node);
      }
      else
      { DEBUG(Adjust Parent);
      if ( ParentNode(node) )
        ChildNode(ParentNode(node)) = RightNode(node);
      }
    if ( RightNode(node) )
      { DEBUG(Adjust Right);
      LeftNode(RightNode(node)) = LeftNode(node);
      }
    if ( NodeName(node) )	free( NodeName(node) );
    if ( NodeCaption(node) )	free( NodeCaption(node) );
    if ( NodeTitle(node) )	free( NodeTitle(node) );
    free( node );
    if ( node == RootNode )  RootNode = NULL;
    }
  OUT(tree_DestroyNode);
  }

void
tree__DestroyNodeChildren( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register tree_type_node     child, peer;

  IN(tree_DestroyNodeChildren);
  if ( node  ||  (node = RootNode) )
    { DEBUGst(Name,NodeName(node));
    if ( child = ChildNode(node) )
      while (child )
	{
        if ( ChildNode(child) )
          tree_DestroyNode( self, ChildNode(child) );
	peer = RightNode(child);
        tree_DestroyNode( self,child );
	child = peer;
	}
    }
  OUT(tree_DestroyNodeChildren);
  }

tree_type_node
tree__HookNode( self, node, parent, left, right )
  register struct tree	     *self;
  register tree_type_node     node, parent, left, right;
  {
  IN(tree_HookNode);
  if ( node  ||  (node = RootNode) )
    {
/*===*/    
    }
  OUT(tree_HookNode);
  return  node;
  }

tree_type_node
tree__UnHookNode( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  IN(tree_UnhookNode);
  if ( node  ||  (node = RootNode) )
    {
/*===*/    
    }
  OUT(tree_UnhookNode);
  return  node;
  }

tree_type_node
tree__MoveNode( self, node, parent, left, right )
  register struct tree	     *self;
  register tree_type_node     node, parent, left, right;
  {
  IN(tree_MoveNode);
  if ( node  ||  (node = RootNode) )
    {
/*===*/    
    }
  OUT(tree_MoveNode);
  return  node;
  }

tree_type_node
tree__DuplicateNode( self, node, parent, left, right )
  register struct tree	     *self;
  register tree_type_node     node, parent, left, right;
  {
  IN(tree_DuplicateNode);
  if ( node  ||  (node = RootNode) )
    {
/*===*/    
    }
  OUT(tree_DuplicateNode);
  return  node;
  }

tree_type_node
tree__NodeOfName( self, name, node )
  register struct tree	     *self;
  register char		     *name;
  register tree_type_node     node;
  {
  register tree_type_node     candidate = NULL;
  register long		      level;

  IN(tree_NodeOfName);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  NodeLevel(node) >= level )
      {
      if ( strcmp( NodeName(node), name) == 0 )
	{
	candidate = node;
	break;
	}
      node = NextNode(node);
      }
    }
  OUT(tree_NodeOfName);
  return  candidate;
  }

tree_type_node
tree__NodeOfDatum( self, datum, node )
  register struct tree	     *self;
  register long		      datum;
  register tree_type_node     node;
  {
  register tree_type_node     candidate = NULL;
  register long		      level;

  IN(tree_NodeOfDatum);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  NodeLevel(node) >= level )
      {
      if ( NodeDatum(node) == datum )
	{
	candidate = node;
	break;
	}
      node = NextNode(node);
      }
    }
  OUT(tree_NodeOfDatum);
  return  candidate;
  }

tree_type_node *
tree__NodesOfName( self, name, node )
  register struct tree	     *self;
  register char		     *name;
  register tree_type_node     node;
  {
  register tree_type_node    *candidates = NULL;
  register long		      level, count = 0;

  IN(tree_NodesOfName);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  NodeLevel(node) >= level )
      {
      if ( strcmp( NodeName(node), name) == 0 )
	{
	if ( candidates )
	  candidates = (tree_type_node *)
	    realloc( candidates, (++count +1)  * sizeof(tree_type_node) );
	  else
	  candidates = (tree_type_node *)
	    malloc( (++count + 1) * sizeof(tree_type_node) );
	candidates[count-1] = node;
	candidates[count] = NULL;
	}
      node = NextNode(node);
      }
    }
  OUT(tree_NodesOfName);
  return  candidates;
  }

tree_type_node *
tree__NodesOfDatum( self, datum, node )
  register struct tree	     *self;
  register long		      datum;
  register tree_type_node     node;
  {
  register tree_type_node    *candidates = NULL;
  register long		      level, count = 0;

  IN(tree_NodesOfDatum);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  NodeLevel(node) >= level )
      {
      if ( NodeDatum(node) == datum )
	{
	if ( candidates )
	  candidates = (tree_type_node *)
	    realloc( candidates, (++count +1)  * sizeof(tree_type_node) );
	  else
	  candidates = (tree_type_node *)
	    malloc( (++count + 1) * sizeof(tree_type_node) );
	candidates[count-1] = node;
	candidates[count] = NULL;
	}
      node = NextNode(node);
      }
    }
  OUT(tree_NodesOfDatum);
  return  candidates;
  }

long
tree__TreeWidth( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      width = 0;

  IN(tree_TreeWidth);
/*===*/
  OUT(tree_TreeWidth);
  return  width;
  }

long
tree__TreeHeight( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      height = 0, level;

  IN(tree_TreeHeight);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  (level = NodeLevel(node)) >= level )
      {
      if ( level > height )
	height = level;
      node = NextNode(node);
      }
    }
  OUT(tree_TreeHeight);
  return  height;
  }

long
tree__NodeLevel( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      level = 1;
  register tree_type_node     parent = node;

  IN(tree_NodeLevel);
  if ( node  ||  (node = parent = RootNode) )
    while ( parent = ParentNode(parent) )
      level++;
  OUT(tree_NodeLevel);
  return  level;
  }

long
tree__NodePosition( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      position = 0;
  register tree_type_node     peer = node;

  IN(tree_NodePosition);
  if ( node  ||  (node = peer = RootNode) )
    {
    position = 1;
    while ( peer = LeftNode(peer) )
      position++;
    }
  DEBUGdt(Position,position);
  OUT(tree_NodePosition);
  return  position;
  }

char *
tree__NodeIndex( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register tree_type_node     parent = node;
  static char		      string[1024];
  char			      temp[1024];
  register long		      length;

  IN(tree_NodeIndex);
  *string = 0;
  if ( node  ||  (node = parent = RootNode) )
    {
    while ( parent  &&  (parent != RootNode) )
      {
      if ( strlen( string ) > (sizeof(string) - 20) )
	{
	strcat( string, "*TOO LONG*." );
	break;
	}
      strcpy( temp, string );
      sprintf( string, "%d.%s", tree_NodePosition( self, parent ), temp );
      parent = ParentNode(parent);
      }
    if ( length = strlen( string ) )
      string[length-1] = 0;
      else
      strcpy( string, "0" );
    }
  DEBUGst(Index,string);
  OUT(tree_NodeIndex);
  return  string;
  }

static char *
Ancestry( self, node, separator, string )
  register struct tree	     *self;
  register tree_type_node     node;
  register char		     *separator, *string;
  {
  IN(Ancestry);
  if ( ParentNode(node) )
    string = Ancestry( self, ParentNode(node), separator, string );
  string = (char *) realloc( string, strlen( string ) +
		strlen( NodeName(node) ) + strlen( separator ) + 2 );
  if ( *string )
    strcat( string, separator );
  strcat( string, NodeName(node) );
  IN(Ancestry);
  return  string;
  }

char *
tree__NodeAncestry( self, node, separator )
  register struct tree	     *self;
  register tree_type_node     node;
  register char		     *separator;
  {
  register char		     *ancestry = NULL;

  IN(tree_NodeAncestry);
  if ( node  &&  ParentNode(node)  &&
      (ancestry = (char *) calloc( 1, 2 )) )
    ancestry = Ancestry( self, ParentNode(node), separator, ancestry );
  DEBUGst(Ancestry,ancestry);
  OUT(tree_NodeAncestry);
  return  ancestry;
  }

boolean
tree__NodeAncestor( self, candidate, node )
  register struct tree	     *self;
  register tree_type_node     candidate, node;
  {
  register boolean	      ancestor = false;

  IN(tree_NodeAncestor);
  if ( candidate  &&  node )
    while ( node = ParentNode(node) )
      if ( candidate == node )
	{
	ancestor = true;
	break;
	}
  DEBUGdt(ancestor,ancestor);
  OUT(tree_NodeAncestor);
  return  ancestor;
  }

long
tree__Apply( self, node, proc, anchor, datum )
  register struct tree	     *self;
  register tree_type_node     node;
  register long		    (*proc)();
  register char		     *anchor;
  register char		     *datum;
  {
  register long		      level, result = NULL;

  IN(tree_Apply);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( result == NULL  &&  node  &&  NodeLevel(node) >= level )
      {
      result = (proc)( anchor, self, node, datum );
      node = NextNode(node);
      }
    }
  OUT(tree_Apply);
  return  result;
  }

tree_type_node
tree__NextNode( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register tree_type_node     next = NULL, parent;

  IN(tree_NextNode);
  if ( node )
    {
    if ( ! (next = node->child) )
      if ( ! (next = node->right) )
	if ( parent = node->parent )
	  while ( parent )
	    if ( next = parent->right )
	      break;
	    else
	    parent = parent->parent;
    }
    else  next = RootNode;
  OUT(tree_NextNode);
  return  next;
  }

tree_type_node
tree__PriorNode( self, node )
  register struct tree 	     *self;
  register tree_type_node     node;
  {
  register tree_type_node     prior = NULL;

  IN(tree_PriorNode);
/*===*/
  OUT(tree_PriorNode);
  return  prior;
  }

void
tree__SetNodeModified( self, node, state )
  register struct tree 	     *self;
  register tree_type_node     node;
  register char		      state;
  {
  IN(tree_SetNodeModified);
  if ( node  ||  (node = RootNode) )
    NodeModified(node) = TreeModified = state;
  OUT(tree_SetNodeModified);
  }

boolean
tree__NodeModified( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  IN(tree_NodeModified);
  OUT(tree_NodeModified);
  return  (node) ? NodeModified(node) : false;
  }

void
tree__SetTreeModified( self, state )
  register struct tree 	     *self;
  register char		      state;
  {
  IN(tree_SetTreeModified);
  TreeModified = state;
  OUT(tree_SetTreeModified);
  }

boolean
tree__TreeModified( self )
  register struct tree	     *self;
  {
  IN(tree_TreeModified);
  OUT(tree_TreeModified);
  return  TreeModified;
  }

boolean
tree__SetNodeName( self, node, name )
  register struct tree         *self;
  register tree_type_node       node;
  register char		       *name;
  {
  register long		        status = ok;

  IN(tree_SetNodeName);
  if ( node )
    {
    if ( NodeName(node) )   free( NodeName(node) );
    NodeName(node) = NULL;
    if ( name  &&  *name )
      {
      if ( NodeName(node) = (char *) malloc( strlen( name ) + 1 ) )
        strcpy( NodeName(node), name );
	else  status = failure;
      }
    }
  OUT(tree_SetNodeName);
  return  status;
  }

boolean
tree__SetNodeCaption( self, node, caption )
  register struct tree    	 *self;
  register tree_type_node         node;
  register char			 *caption;
  {
  IN(tree_SetNodeCaption);
  if ( node )
    {
    if ( NodeCaption(node) )   free( NodeCaption(node) );
    NodeCaption(node) = NULL;
    if ( caption  &&  *caption )
      {
      NodeCaption(node) = (char *) malloc( strlen( caption ) + 1 );
      strcpy( NodeCaption(node), caption );
      }
    }
  OUT(tree_SetNodeCaption);
  return  TRUE;
  }

boolean
tree__SetNodeTitle( self, node, title )
  register struct tree          *self;
  register tree_type_node        node;
  register char			*title;
  {
  IN(tree_SetNodeTitle);
  if ( node )
    {
    if ( NodeTitle(node) )   free( NodeTitle(node) );
    NodeTitle(node) = NULL;
    if ( title  &&  *title )
      {
      NodeTitle(node) = (char *) malloc( strlen( title ) + 1 );
      strcpy( NodeTitle(node), title );
      }
    }
  OUT(tree_SetNodeTitle);
  return  TRUE;
  }

boolean
tree__SetNodeDatum( self, node, datum )
  register struct tree         *self;
  register tree_type_node       node;
  register long		        datum;
  {
  IN(tree_SetNodeDatum);
  if ( node )
    NodeDatum(node) = datum;
  OUT(tree_SetNodeDatum);
  return  TRUE;
  }

long
tree__NodeCount( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      count = 0, level;

  IN(tree_NodeCount);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  NodeLevel(node) >= level )
      {
      count++;
      node = NextNode(node);
      }
    }
  OUT(tree_NodeCount);
  return  count;
  }

long
tree__PeerNodeCount( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      count = 0;
  register tree_type_node     peer;

  IN(tree_PeerNodeCount);
  if ( node  &&  ParentNode(node) )
    {
    peer = ChildNode(ParentNode(node));
    while ( peer = RightNode(peer) )
      count++;
    }
  OUT(tree_PeerNodeCount);
  return  count;
  }


long
tree__ChildNodeCount( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      count = 0;

  IN(tree_ChildNodeCount);
  if ( node  &&  ChildNode(node) )
    count = 1 + tree_PeerNodeCount( self, ChildNode(node) );
  OUT(tree_ChildNodeCount);
  return  count;
  }

long
tree__LeafNodeCount( self, node )
  register struct tree	     *self;
  register tree_type_node     node;
  {
  register long		      count = 0, level;

  IN(tree_LeafNodeCount);
  if ( node  ||  (node = RootNode) )
    {
    level = NodeLevel(node);
    while ( node  &&  NodeLevel(node) >= level )
      {
      if ( ChildNode(node) == NULL )
        count++;
      node = NextNode(node);
      }
    }
  OUT(tree_LeafNodeCount);
  return  count;
  }

void
tree__SetDebug( self, state )
  register struct tree	      *self;
  register char		       state;
  {
  IN(tree_SetDebug);
  tree_debug = state;
  OUT(tree_SetDebug);
  }
