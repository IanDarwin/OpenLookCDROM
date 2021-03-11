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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/rm.c,v 2.13 1993/05/04 01:08:09 susan Exp $";
#endif


 

#include <class.h>
#include <rm.eh>
#include <namespc.ih> 
#include <stdio.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

#define DEBUG_RM 1
static int debug_rm = 0;

struct resourceTree
{
  struct namespace * data;
  struct namespace * children;
};

struct resourceTree * resourceTree;
struct namespace * converters;


/****************************************************************/
/*		debugging code					*/
/****************************************************************/

#if DEBUG_RM
void Patom( atom )
     struct atom * atom;
{
  printf("%s",atom_Name(atom));
}

void Plist( list )
     struct atomlist * list;
{
  struct atoms * current = atomlist_TraversalStart(list);
  if (current != NULL)
    {
      Patom(atomlist_TraversalAtom(list,current));
      for ( ; current != NULL; current = atomlist_TraversalNext(list,current))
	{
	  putchar('.');
	  Patom(atomlist_TraversalAtom(list,current));
	}
      fflush(stdout);
    }      
}
#endif /* DEBUG_RM */

/****************************************************************/
/*		private functions				*/
/****************************************************************/
int FindResource( name, namecar, class, classcar, tree )
     struct atomlist * name;
     struct atoms * namecar;
     struct atomlist * class;
     struct atoms * classcar;
     struct resourceTree ** tree;
{
  short found;
  struct resourceTree * nexttree;
  struct atoms * namecdr;
  struct atoms * classcdr;
  
 tail_recursion_can_be_fun:
  
  namecdr = atomlist_TraversalNext( name, namecar );
  classcdr = atomlist_TraversalNext( class, classcar );
  
  if (*tree == NULL)
    found = FALSE;
  if (namecar == NULL && classcar == NULL)
    found = (*tree)->data != NULL;
  else
    {
      if (namespace_Boundp( (*tree)->children,
			   atomlist_TraversalAtom(name, namecar),
			   (long *) &nexttree ) &&
	  FindResource( name, namecdr, class, classcdr, &nexttree ))
	{
#if DEBUG_RM
	  if (debug_rm)
	    {
	      printf("Found at name: %x ",namecar);
	      Patom(atomlist_TraversalAtom(name,namecar));
	      printf("\n");
	    }
#endif /* DEBUG_RM */
	  *tree = nexttree;
	  found = TRUE;
	}
      else
	{
	  if (atomlist_TraversalAtom(class,classcar)
	      != atomlist_TraversalAtom(name,namecar)
	      && namespace_Boundp( (*tree)->children,
				  atomlist_TraversalAtom(class, classcar),
				  (long *) &nexttree ) 
	      && FindResource(name, namecdr, class, classcdr, &nexttree ))
	    {
#if DEBUG_RM
	      if (debug_rm)
		{
		  printf("Found at class: %x ",classcar);
		  Patom(atomlist_TraversalAtom(class,classcar));
		  printf("\n");
		}
#endif /* DEBUG_RM */
	      *tree = nexttree;
	      found = TRUE;
	    }
	  else
	    {
	      struct atom * nameatom;
	      struct atom * classatom;
#if DEBUG_RM
	      if (debug_rm)
		{
		  printf("discarded at name: %x ",namecar);
		  Patom(atomlist_TraversalAtom(name,namecar));
		  printf("\n");
		}
#endif /* DEBUG_RM */
	      /* this optimization should win */
	      nameatom = atomlist_TraversalAtom(name,namecar);
	      classatom = atomlist_TraversalAtom(class,classcar);
	      while (nameatom == atomlist_TraversalAtom(name,namecdr)
		     && classatom == atomlist_TraversalAtom(class,classcdr))
		{
		  namecdr   = atomlist_TraversalNext(name,namecdr);
		  classcdr  = atomlist_TraversalNext(class,classcdr);
		}
	      namecar = namecdr;
	      classcar = classcdr;
	      goto tail_recursion_can_be_fun;
	    }
	}
    }
  
  return found;
}



int FindManyResources( resources, name, namecar, class, classcar, tree )
     struct resourceList * resources;
     struct atomlist * name;
     struct atoms * namecar;
     struct atomlist * class;
     struct atoms * classcar;
     struct resourceTree ** tree;
{
  short foundAll;
  struct resourceTree * nexttree;
  struct atoms * namecdr;
  struct atoms * classcdr;
  struct resourceTree * t;
  int x;
  
 tail_recursion_can_be_fun:
  
  namecdr = atomlist_TraversalNext( name, namecar );
  classcdr = atomlist_TraversalNext( class, classcar );
  
  if (*tree == NULL)
    foundAll = FALSE;
  if (namecar == NULL && classcar == NULL)
    {
      foundAll = TRUE;
      for (x = 0; resources[x].name != NULL; ++x)
	{
	  if (resources[x].found)
	    continue;
	  t = *tree;
	  namecar = atomlist_TraversalStart(resources[x].name);
	  if (!FindResource( resources[x].name, namecar, resources[x].name,
			    namecar, &t ))
	    foundAll = FALSE;
	  else
	    {
	      resources[x].found = TRUE;
	      resources[x].data  = (long)t;
	    }
	}
    }
  else
    {
      if (namespace_Boundp( (*tree)->children,
			   atomlist_TraversalAtom(name, namecar),
			   (long *) &nexttree ) &&
	  FindManyResources( resources, name, namecdr, class, classcdr, &nexttree ))
	{
#if DEBUG_RM
	  if (debug_rm)
	    {
	      printf("Found at name: %x ",namecar);
	      Patom(atomlist_TraversalAtom(name,namecar));
	      printf("\n");
	    }
#endif /* DEBUG_RM */
	  *tree = nexttree;
	  foundAll = TRUE;
	}
      else
	{
	  if (atomlist_TraversalAtom(class,classcar)
	      != atomlist_TraversalAtom(name,namecar)
	      && namespace_Boundp( (*tree)->children,
				  atomlist_TraversalAtom(class, classcar),
				  (long *) &nexttree ) 
	      && FindManyResources(resources, name, namecdr, class,
				  classcdr, &nexttree ))
	    {
#if DEBUG_RM
	      if (debug_rm)
		{
		  printf("Found at class: %x ",classcar);
		  Patom(atomlist_TraversalAtom(class,classcar));
		  printf("\n");
		}
#endif /* DEBUG_RM */
	      *tree = nexttree;
	      foundAll = TRUE;
	    }
	  else
	    {
	      struct atom * nameatom;
	      struct atom * classatom;
#if DEBUG_RM
	      if (debug_rm)
		{
		  printf("discarded at name: %x ",namecar);
		  Patom(atomlist_TraversalAtom(name,namecar));
		  printf("\n");
		}
#endif /* DEBUG_RM */
	      /* this optimization should win */
	      nameatom = atomlist_TraversalAtom(name,namecar);
	      classatom = atomlist_TraversalAtom(class,classcar);
	      while (nameatom == atomlist_TraversalAtom(name,namecdr)
		     && classatom == atomlist_TraversalAtom(class,classcdr))
		{
		  namecdr   = atomlist_TraversalNext(name,namecdr);
		  classcdr  = atomlist_TraversalNext(class,classcdr);
		}
	      namecar = namecdr;
	      classcar = classcdr;
	      goto tail_recursion_can_be_fun;
	    }
	}
    }
  
  return foundAll;
  /* slick, eh? */
}





int Convertp( toconverters, namespace, index )
     struct namespace * toconverters;
     struct namespace * namespace;
     int index;
{
  return !namespace_Boundp( toconverters,
			   namespace_NameAt(namespace,index),
			   NULL );
}


short TryConversion( tree, type, data )
     struct resourceTree * tree;
     struct atom * type;
     long * data;
{
  int fromtype;
  struct namespace * toconverters;
  procedure converter;
  short gotgooddata = FALSE;

  if (tree->data != NULL)
    if (namespace_Boundp( tree->data, type, data ))
      gotgooddata = TRUE;
    else
      if (namespace_Boundp( converters, type, (long *) &toconverters ) &&
	  ((fromtype = namespace_Enumerate(tree->data, Convertp, (long) toconverters)) >= 0))
	{
	  converter =
	    (procedure)namespace_GetValue( toconverters,
					  namespace_NameAt(tree->data,fromtype) );
	  gotgooddata =  (*converter)(namespace_ValueAt(tree->data,fromtype),data);
	}
  return gotgooddata;
}


/****************************************************************/
/*		class procedures				*/
/****************************************************************/
boolean rm__InitializeClass( classID )
     struct classheader *classID;
{
  resourceTree = (struct resourceTree *)malloc(sizeof( struct resourceTree ));
  resourceTree->data = NULL;
  resourceTree->children = namespace_New();
  converters = namespace_New();
  return TRUE;
}

void rm__PostConverter( classID, from, to, converter )
     struct classheader *classID;
     struct atom * from;
     struct atom * to;
     procedure converter;
{
  struct namespace * toconverters;

  if (!namespace_Boundp( converters, to, (long *) &toconverters ))
    {
      toconverters = namespace_New();
      namespace_SetValue( converters, to, (long) toconverters );
    }

  namespace_SetValue( toconverters, from, (long) converter );
}



static struct  resourceTree * FindNodeCreate( path, tree )
     struct atomlist * path;
     struct resourceTree * tree;
{
  struct atoms * car = atomlist_TraversalStart( path );

  while (car != NULL)
    {
      if (!namespace_Boundp( tree->children,
			    atomlist_TraversalAtom(path,car), (long *) &tree ))
	{
	  struct resourceTree * newTree;
	  newTree = (struct resourceTree *)malloc(sizeof( struct resourceTree ));
	  newTree->data = NULL;
	  newTree->children = namespace_New();
	  namespace_SetValue( tree->children,
			     atomlist_TraversalAtom(path,car),
			     (long) newTree );
	  tree = newTree;
	}
      car = atomlist_TraversalNext( path, car );
    }
  return tree;
}


void PostResourceAt( root, path, data, type )
     struct resourceTree * root;
     struct atomlist * path;
     long data;
     struct atom * type;
{
  struct resourceTree * tree = FindNodeCreate( path, root );

  if (tree->data != NULL)
    namespace_Clear( tree->data );
  else
    tree->data = namespace_New();
  namespace_SetValue( tree->data, type, data );
}


void rm__PostManyResources( classID, resources, context )
     struct classheader *classID;
     struct resourceList * resources;
     struct atomlist * context;
{
  int x;
  struct resourceTree * tree = FindNodeCreate(context, resourceTree);

  for (x = 0;
       resources[x].name != NULL;
       ++x)
    PostResourceAt( tree, resources[x].name, resources[x].data,
		   resources[x].type );
}


void rm__PostResource( classID, path, data, type )
     struct classheader *classID;
     struct atomlist * path;
     long data;
     struct atom * type;
{
#if DEBUG_RM
  if (debug_rm)
    {
      printf("Posting resource: ");
      Plist( path );
      printf("\nType: %x ",type);
      Patom(type);
      putchar('\n');
    }
#endif /* DEBUG_RM */
  PostResourceAt( resourceTree, path, data, type );
}


/* name and class had better be the same length! */
short rm__GetResource( classID, name, class, type, data )
     struct classheader *classID;
     struct atomlist * name;
     struct atomlist * class;
     struct atom * type;
     long * data;
{
  struct resourceTree * tree = resourceTree;
  int x;
  int y = 0;

#if DEBUG_RM
  if (debug_rm)
    {
      printf("Seeking resource: ");
      Plist( name );
      printf("\nClass: ");
      Plist(class);
      printf("\nType: %x ",type);
      Patom(type);
      printf("\n");
    }
#endif /* DEBUG_RM */

  x = FindResource( name, atomlist_TraversalStart(name),
		   class, atomlist_TraversalStart(class),
		   &tree );
#if DEBUG_RM
  if (debug_rm)
    {
      if (x)
	printf("Found such a resource %x\n", tree);
      else
	printf("no such resource\n");
    }
#endif /* DEBUG_RM */

  if (x) y  = TryConversion( tree, type, data );

#if DEBUG_RM
  if (debug_rm)
    {
      if (x && y)
	printf("Conversion succeeded\n");
      else
	printf("Conversion failed\n");
    }
#endif /* DEBUG_RM */

  return x && y;
}


void rm__GetManyResources( classID, resources, name, class)
     struct classheader *classID;
     struct resourceList* resources;
     struct atomlist * name;
     struct atomlist * class;
{
  int x;
  struct resourceTree * tree = resourceTree;

  for (x = 0; resources[x].name != NULL; ++x)
    resources[x].found = 0;

  (void)FindManyResources( resources, name, atomlist_TraversalStart(name),
		   class, atomlist_TraversalStart(class),
		   &tree );

  for (x = 0; resources[x].name != NULL; ++x)
    if (resources[x].found)
      {
	resources[x].found =
	  TryConversion( (struct resourceTree *)resources[x].data,
			resources[x].type,
			&resources[x].data );
      }

}



void
rm__ContextualPostResource( classID, context, path, data, type )
     struct classheader *classID;
     struct atomlist * context;
     struct atomlist * path;
     long data;
     struct atom * type;
{
  struct atoms * m;
  m = atomlist_Mark(path);
  if (context != NULL)
    atomlist_JoinToBeginning(path,context);
  rm_PostResource(path,data,type);
  atomlist_Cut(path,m);
}
