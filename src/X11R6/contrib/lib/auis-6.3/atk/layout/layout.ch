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

/* $Header $ */
/* $ACIS$ */

 

/* layout.ch - layout data object definition and interface */

#define layout_VERSION 1
#define SafeFromNull(s) (((s) != NULL) ? (s) : "(null)")

#define getDataObject(self) ((self) -> header.dataobject)


/* globals for entire package */

boolean	debug;				/* nonzero if debugging */

/* component data structure */

/*
Components are stored in a simple linked list.  Order is significant,
with earlier objects in the list drawn in front of later ones.

The forallcomponents(self, c) macro provides a loop down the list.
The cXxxxx macros provide geometry information.
*/

struct component {
    struct component *nextcomponent;	/* forward link in component list */
    struct dataobject *data;		/* embedded data object or NULL if none */
    long left, top, width, height;	/* rectangle containing imbedded object */
    boolean varies:1;			/* TRUE if can be changed in execution mode */
};

#define forallcomponents(self, c) for (c = (self)->firstcomponent; c != NULL; c = c->nextcomponent)

#define cLeft(c)  ((c)->left)
#define cTop(c)  ((c)->top)
#define cRight(c) ((c)->left + (c)->width)
#define cBottom(c) ((c)->top + (c)->height)
#define cWidth(c) ((c)->width)
#define cHeight(c) ((c)->height)

#define cData(c) ((c)->data)
#define cVaries(c) ((c)->varies)


/* actual interface definition */

class layout: dataobject[dataobj] {

overrides:
  ViewName() returns char *;
  Read (FILE * f, long id) returns long;
  Write (FILE * f, long writeid, int level) returns long;
  GetModified() returns long;

methods:
  ToggleDebug();
  RemoveComponent(struct component *c);
  FillInComponent(char *name, struct component *c);
  CreateComponent() returns struct component *;
  SetComponentSize(struct component *c, long x, long y, long w, long h);
  Promote(struct component *c);
  Demote(struct component *c);
  MakeVariable(struct component *c);
  MakeFixed(struct component *c);

macromethods:
  GetFirstComponent() (self->firstcomponent)

classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct layout *self) returns boolean;
  FinalizeObject(struct layout *self);

data:
  struct component *firstcomponent;	    /* first in component list */
  int inGetModified;			    /* recursion protection switch */
};

/* end of layout.ch */
