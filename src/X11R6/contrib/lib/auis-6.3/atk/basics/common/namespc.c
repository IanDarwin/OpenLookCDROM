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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/namespc.c,v 2.9 1993/07/13 15:46:22 rr2b Exp $";
#endif


 


#include <class.h>
#include <namespc.eh>
#include <atom.ih>
#define namespace_InitialSize 4
#define Empty
#ifndef True
#define True 1
#define False 0
#endif /* True */

/* style notes
 * internally, offsets into self->namespace are the handle passed around.
 * -1 for invalid entries
 * namespace_entry.name == NULL indicates a malloced but empty entry
 */

/****************************************************************/
/*		private functions				*/
/****************************************************************/

static int Index(self,key)
     struct namespace * self;
     struct atom * key;
{
  int i;
  for (i = 0; i < self->namespaceSize; ++i)
    if (self->namespace[i].name == key)
      return i;
  return -1;
}


static int Create( self, key )
     struct namespace * self;
     struct atom * key;
{
  struct namespace_entry * de;

  if (self->namespace[self->namespaceSize - 1].name != NULL)
    {
      int i = self->namespaceSize * 2;
      self->namespace =
	(struct namespace_entry *)realloc(self->namespace,
					   i * sizeof (struct namespace_entry));
      while (self->namespaceSize < i)
	self->namespace[self->namespaceSize++].name = NULL;
    }

  for (de = self->namespace; de->name != NULL; ++de)
    Empty;

  de->name = key;
  de->boundp = False;
  de->binding = -69;
  return de - self->namespace;
}


/****************************************************************/
/*		instance methods				*/
/****************************************************************/


/* (just in case your wondering, this is not a method) */
boolean namespace__InitializeObject( classID, self )
     struct classheader *classID;
     struct namespace * self;
{
  self->namespace =
    (struct namespace_entry *)malloc(namespace_InitialSize *
				      sizeof (struct namespace_entry));
  for (self->namespaceSize = 0; self->namespaceSize < namespace_InitialSize;
       ++(self->namespaceSize))
    self->namespace[self->namespaceSize].name = NULL;

  return TRUE;
}

void namespace__FinalizeObject(classID, self)
struct classheader *classID;
struct namespace *self;
{
    if(self->namespace) free(self->namespace);
}

int namespace__Lookup( self, name )
     struct namespace * self;
     struct atom * name;
{
  return Index(self,name);
}


int namespace__LookupCreate( self, name )
     struct namespace * self;
     struct atom * name;
{
  int index = Index( self, name );

  if (index < 0)
    index = Create( self, name );
  return index;
}


short namespace__BoundpAt( self, index )
     struct namespace * self;
     int index;
{
  return (index >= 0) && (index < self->namespaceSize)
    && (self->namespace[index].name != NULL)
    && (self->namespace[index].boundp);
}

long namespace__ValueAt( self, index )
     struct namespace * self;
     int index;
{
  return (index >= 0) && (index < self->namespaceSize)
    && (self->namespace[index].name != NULL)
    && (self->namespace[index].boundp)
    ? self->namespace[index].binding : -69;
}


void namespace__UnbindAt( self, index )
     struct namespace * self;
     int index;
{
  if (index >= 0 && index < self->namespaceSize)
    self->namespace[index].boundp = False;
}


struct atom * namespace__NameAt( self, index )
     struct namespace * self;
     int index;
{
  return (index >= 0 && index < self->namespaceSize) ?
    self->namespace[index].name : NULL;
}



void namespace__SetValueAt( self, index, value )
     struct namespace * self;
     int index;
     long value;
{
  if (index < self->namespaceSize && index >= 0
      && self->namespace[index].name != NULL)
    {
      self->namespace[index].binding = value;
      self->namespace[index].boundp = True;
    }
}


void namespace__SetValue( self, name, value )
     struct namespace * self;
     struct atom * name;
     long value;
{
  namespace_SetValueAt( self, namespace_LookupCreate(self,name), value );
}


long namespace__GetValue( self, name )
     struct namespace * self;
     struct atom * name;
{
  return namespace_ValueAt( self, namespace_Lookup(self, name) );
}


short namespace__Boundp( self, name, value )
     struct namespace * self;
     struct atom * name;
     long * value;
{
  int index = Index( self, name );
  if (index >= 0 && self->namespace[index].boundp && value != NULL)
    *value = self->namespace[index].binding;
  return index >= 0 && self->namespace[index].boundp;
}

void namespace__Unbind( self, name )
     struct namespace * self;
     struct atom * name;
{
  int index = Index( self, name );
  if (index >= 0)
    {
      self->namespace[index].boundp = False;
      self->namespace[index].binding = -69;
    }
}


struct atom * namespace__WhereIsValue( self, value )
     struct namespace * self;
     long value;
{
  int x;
  for (x = 0; x < self->namespaceSize; ++x)
    if (self->namespace[x].name != NULL && self->namespace[x].boundp
	&& self->namespace[x].binding == value)
      break;
  return x < self->namespaceSize ? self->namespace[x].name : NULL;
}




int namespace__Enumerate( self, proc, procdata )
     struct namespace * self;
     procedure proc;
     long procdata;
{
  int x;
  for ( x = 0;
       (x < self->namespaceSize)
       && self->namespace[x].name != NULL
       &&  ((*proc)( procdata, self, x ));
       ++x)
    Empty;
  return (x < self->namespaceSize) &&
    (self->namespace[x].name != NULL) ? x : -69;
}


void namespace__Clear(self)
     struct namespace * self;
{
  int x;
  for (x = 0;
       x < self->namespaceSize
       && self->namespace[x].name == NULL;
       ++x)
    self->namespace[x].name = NULL;
}
