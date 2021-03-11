/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *	   Copyright Carnegie Mellon, 1992 - All Rights Reserved
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


 

/*
null.ch

	null data object

	This module stores the coordinates of a set of blobs and
	provides methods to add blobs and clear the list.

	The file format for a null begins and ends with the standard 
	\begindata \enddata.  There is one intervening line for each blob 
	in the image.  This line gives the coordinates of the upper left corner 
	of the blob as two ASCII integers separated by a space.  The origin 
	for the coordinates is the upper left corner of the rectangle.

*/
/*
 *    $Log: null.ch,v $
*Revision 1.5  1993/05/04  01:14:27  susan
*RCS Tree Split
*
*Revision 1.4.1.1  1993/02/02  01:40:42  rr2b
*new R6tape branch
*
*Revision 1.4  1992/12/14  20:40:31  rr2b
*disclaimerization
*
Revision 1.3  1992/06/22  21:48:52  wjh
added types in function declaractions
.MARKER
revised description of __Read
.

Revision 1.2  1991/09/12  19:30:59  bobg
Update copyright notice

Revision 1.1  1989/07/31  15:34:54  wjh
Initial revision

 *
 * Revision 1.0  88/05/14  15:40:32  wjh
 * Copied from /usr/andrew/lib/genericinset
 */




/* status values for NotifyObservers */
#define null_DATACHANGED 1

/* $$$ declare any structs that will be needed */
struct dotlist {
	long x, y;
	struct dotlist *next;
};


class null : dataobject [dataobj]
{

overrides:

	Read(/* struct null *self, */ FILE *file, long id)
			returns long;
	Write(/* struct null *self, */ FILE *file, long id, long level)
			returns long;

methods:

	/* $$$ unique to this data object */
	AddDot(/* struct null *self, */ int x, int y);
		/* add a pair to list of dots */
	ClearDots(/* struct null *self */);
		/* remove all dots from list */

macromethods:

	/* $$$ unique to this data object */
	GetFirstDot(/* struct null *self */) (self->dots)
	GetNextDot(/* struct null *self, */ struct dotlist *pair) (pair->next)
	GetDotX(/* struct null *self, */ struct dotlist *pair) (pair->x)
	GetDotY(/* struct null *self, */ struct dotlist *pair) (pair->y)

classprocedures:

	InitializeClass() returns boolean;
	InitializeObject(struct null *self) returns boolean;
	FinalizeObject(struct null *self);

data:

	/* $$$ data values unique to this data object */
	struct dotlist *dots;

};

