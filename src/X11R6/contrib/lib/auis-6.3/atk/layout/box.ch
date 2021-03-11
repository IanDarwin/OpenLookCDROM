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

 

/* box.ch - box data object definition and interface */

#include <rect.h>

#define box_VERSION 1
#define SafeFromNull(s) (((s) != NULL) ? (s) : "(null)")

#define getDataObject(self) ((self) -> header.dataobject)


/* globals for entire package */

/* actual interface definition */

class box: dataobject[dataobj] {

overrides:
  ViewName() returns char *;
  Read (FILE * f, long id) returns long;
  Write (FILE * f, long writeid, int level) returns long;
  GetModified() returns long;

methods:
  ToggleDebug();
  FillInContents(char *name);

macromethods:

classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct box *self) returns boolean;
  FinalizeObject(struct box *self);

data:
  int inGetModified;			    /* recursion protection switch */
  struct dataobject *contents;		    /* contained object */
};

/* end of box.ch */
