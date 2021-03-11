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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/org/RCS/org.c,v 1.19 1992/12/15 21:38:20 rr2b R6tape $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Org Data-Class

MODULE	org.c

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Org Data-Class.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  01/19/89	Created (TCP)
  08/23/89	Remove Create method (TCP)
  08/24/89	Upgrade to Version 1.0 (TCP)
  08/31/89	Change OfData to OfDatum (TCP)

END-SPECIFICATION  ************************************************************/

#include <ctype.h>
#include <sys/param.h>
#include <andrewos.h>
#include <apt.h>
#include <dataobj.ih>
#include <text.ih>
#include <tree.ih>
#include <filetype.ih>
#include <org.eh>

#define Tree (self->tree_data_object)

static tree_Specification specification[] = {
  tree_Order( tree_PreOrder ),
  NULL
};

int Org_Debug = 0;

#define debug Org_Debug

static Read_Body();
static Write_Body();
static Strip();

char *
org__ViewName( self )
  register struct org *self;
{
    return ( "orgv" );
}

boolean 
org__InitializeObject( classID, self )
  register struct classheader *classID;
  register struct org *self;
{
  register boolean status = true;

  IN(org_InitializeObject);
  DEBUGst(RCSID,rcsid);
  if ( (Tree = tree_Create( specification, self )) == NULL ) {
    printf( "ORGVIEW: Unable to Create AptTree Object\n" );
    status = false;
  }
  OUT(org_InitializeObject);
  return(status);
}

static
Free_Elements( self, tree, node, datum )
  register struct org *self;
  register struct tree *tree;
  register tree_type_node node;
{
  if ( tree_NodeDatum( tree, node ) )
    free( tree_NodeDatum( tree, node ) );
  return(NULL);
}

void
org__FinalizeObject( classID, self )
  register struct classheader *classID;
  register struct org *self;
{
  IN(org_FinalizeObject );
  if ( Tree ) {
      tree_Apply( Tree, tree_RootNode( Tree ), Free_Elements, self, NULL );
      tree_Destroy( Tree );
  }
  OUT(org_FinalizeObject );
}

long
org__Read( self, file, id )
  register struct org *self;
  register FILE *file;
  register long id;
{
  register long status;

  IN(org_Read);
  status = Read_Body( self, file );
  OUT(org_Read);
  return(status);
}

static
Read_Body( self, file )
  register struct org		     *self;
  register FILE			     *file;
  {
  register boolean		      done = false;
  register long			      c, count, braces = 0, brackets = 0, status = ok,
				      description_size, description_length,
				      description_increment = 32;
  char				      string[4096], *ptr, *end_ptr,
				     *count_ptr, counter[32];
  register char			     *description_ptr = NULL, *description;
  register tree_type_node	      parent = NULL, child = NULL, node;
  struct text			     *text = NULL;

  IN(Read_Body);
  ptr = string;
  end_ptr = ptr + sizeof(string) - 2;
  while ( !done  &&  (c = getc( file )) ) {
    switch ( c ) {
      case '\n':
	if ( ptr > string ) {
	  Strip( ptr = string );
	  if ( parent ) {
	    if ( (child = node = tree_CreateChildNode( Tree, string, NULL, parent )) == NULL )
	      { DEBUG(ERROR Creating Node);/*===*/      }
	  }
	  else {
	    if ( (parent = node = tree_CreateRootNode( Tree, string, NULL )) == NULL )
	      { DEBUG(ERROR Creating RootNode);/*===*/  }
	  }
	}
        break;
      case '{':
	braces++;
	if ( child )  parent = child;
        break;
      case '}':
	braces--;
	if ( ptr > string ) {
	  Strip( ptr = string );
	  if ( parent ) {
	    if ( (child = node = tree_CreateChildNode( Tree, string, NULL, parent )) == NULL )
	      { DEBUG(ERROR Creating Node);/*===*/      }
	  }
	  else {/*===*/}
	}
	else {
	  child = parent;
	  parent = tree_ParentNode( Tree, parent );
	}
        break;
      case '[':
	brackets++;
	count_ptr = counter;
	while ( (c = getc( file ))  &&  c != EOF  && c != '\n' )
	  *count_ptr++ = c;
	*count_ptr = 0;
	count = atoi( counter );
	DEBUGdt(Count,count);
	description_size = 32;
	description_length = 0;
	description_ptr = description = (char *) malloc( description_size );
	while ( (c = getc( file ))  &&  --count  &&  c != EOF ) {
	  *description_ptr++ = c;
	  description_length++;
	  if ( description_length == (description_size-1) ) {
	    *description_ptr = 0;
	    DEBUGst(Description,tree_NodeDatum( Tree, node ));
	    description_size += description_increment;
	    description_ptr = description = (char *) realloc( description, description_size );
	    description_ptr += description_length;
	  }
	}
	*description_ptr = 0;
	{
	    char fName[MAXPATHLEN], seed[MAXPATHLEN];
	    FILE *f;
	    sprintf(seed, "/tmp/.%d.%d", getuid(), getpid());
	    strcpy(fName, tmpnam(seed));
	    if(f = fopen(fName, "w")) {
		fputs(description, f);
		fputc('\n', f);
		fclose(f);
		if(f = fopen(fName, "r")) {
		    long objID;
		    filetype_Lookup(f, fName, &objID, NULL);
		    text = text_New();
		    text_Read(text, f, objID);
		    unlink(fName);
		}
		else fprintf(stderr, "org: couldn't open temp file for reading.\n");
	    }
	    else fprintf(stderr, "org: couldn't open temp file for writing.\n");
	}
	tree_SetNodeDatum( Tree, node, (long) text );
	DEBUGst(Description, description);
	break;
      case ']':
	brackets--;
	break;
      case '\\':
	while ( (c = getc( file )) != '\n'  &&  c != EOF ) ;
	done = true;
	break;
      case EOF:
	done = true;
        break;
      default:
	if ( ptr < end_ptr ) {
	  if ( (ptr > string)  ||  (c != ' '  &&  c != '\t') )
	    {  *ptr++ = c;  *ptr = 0;  }
	}
	else
	  { DEBUG(ERROR: exceeded string);/*===*/ }
        break;
      }
    }
  if ( braces ) {
    status = failure;
/*===*/printf("ORG: ERROR  %d Unbalanced Braces\n", braces);
  }
  if ( brackets ) {
      status = failure;
/*===*/printf("ORG: ERROR  %d Unbalanced Brackets\n", brackets);
  }
/*===*/
  OUT(Read_Body);
  return(status);
  }

long
org__Write( self, file, writeID, level )
  register struct org		     *self;
  register FILE			     *file;
  register long			      writeID;
  register long			      level;
{
  register long			      status, id;

  IN(org_Write);
  DEBUGdt(Headerwriteid,self->header.dataobject.writeID);
  DEBUGdt(Given Id,writeID);
  DEBUGdt(Given Level,level);
  id = org_UniqueID( self );
  DEBUGdt(Local-ID,id);
  if ( self->header.dataobject.writeID != writeID ) {
    self->header.dataobject.writeID = writeID;
    fprintf( file, "\\begindata{%s,%d}\n", class_GetTypeName( self ), id );
    status = Write_Body( self, file );
    fprintf( file, "\n\\enddata{%s,%d}\n", class_GetTypeName( self ), id );
  }
  DEBUGdt(Status,status);
  OUT(org_Write);
  return  self->header.dataobject.id;
}

static
Write_Body( self, file )
  register struct org *self;
  register FILE *file;
{
  register long status = ok;
  register tree_type_node node = tree_RootNode( Tree );
  register long level, current_level = 1;
  struct text *text;
  int size;

  IN(Write_Body);
  while ( node ) {
    if ( (level = tree_NodeLevel( Tree, node )) > current_level )
	fprintf( file, "%*s{\n", 2 * level, "" );
    else 
	if ( level < current_level )
	    for ( ; current_level > level; current_level-- )
		fprintf( file, "%*s}\n", 2 * current_level, "" );
    current_level = level;
    fprintf( file, "%*s%s\n", 2 * level, "", tree_NodeName( Tree, node ) );
    if ( (text = (struct text *) tree_NodeDatum(Tree, node)) && 
	 (size = text_GetLength(text)) > 0 ) {
	long realSize = 0;
	char *description;
	char fName[MAXPATHLEN];
	FILE *f;
	strcpy(fName, tmpnam(NULL));
	if(f = fopen(fName, "w")) {
	    text_Write(text, f, text_UniqueID(text), 1);
	    fseek(f, 0, 2);
	    realSize = ftell(f);
	    fclose(f);
	    if(f = fopen(fName, "r")) {
		if(description = (char*) malloc(realSize + 1)) {
		    if(fread(description, realSize, 1, f) != 1) {
			fprintf(stderr, "org: incomplete read on temp file\n");
			realSize = 0;
		    }
		    description[realSize] = (char)0;
		}
		unlink(fName);
	    }	
	}
	fprintf( file, "%*s[%d\n%s]\n", 2 * level, "", realSize, description);
	free(description);
    }
    node = tree_NextNode( Tree, node );
  }
  for ( ; current_level > 1; current_level-- )
    fprintf( file, "%*s}\n", 2 * current_level, "" );
  OUT(Write_Body);
  return(status);
}

char *
org__NodeName( self, node )
  register struct org *self;
  register struct tree_node *node;
{
  register char *name = NULL;

  IN(org_NodeName);
  if ( node )
    name = tree_NodeName( Tree, node );
  OUT(org_NodeName);
  return  name;
}

void
org__SetDebug( self, state )
  register struct org *self;
  register char state;
{
  IN(org_SetDebug);
  debug = state;
  tree_SetDebug( Tree, debug );
  OUT(org_SetDebug);
}

static
Strip( string )
  register char *string;
{
  register char *ptr = string;

  while ( *ptr == ' '  ||  *ptr == '\t' )  ptr++;
  strcpy( string, ptr );
  while ( *ptr )  ptr++;
  ptr--;
  while ( *ptr == ' '  ||  *ptr == '\t' ) *ptr-- = 0;
}
