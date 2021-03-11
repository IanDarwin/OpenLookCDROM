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


 

/*
lookz.H

	lookz data object

	This inset provides for editing the styles of its parent text

	The file format for a lookz has a single line between
	\begindata and \enddata.  The only text on this line is one of the 
	words "visible" or "hidden", determining whether the 
	style editor is exposed or hidden in the text.

*/

#define  lookz_PROGRAMMERVERSION    1

/* status values for NotifyObservers */
#define lookz_VISIBILITYCHANGED 1
#define lookz_CANCLOSECHANGED 2
#define lookz_TEXTOBJECTCHANGED 4

class lookz : dataobject[dataobj]  {

overrides:

	Read( /* struct lookz *self, */ FILE *file, long id ) returns long;
	Write( /* struct lookz *self, */ FILE *file, long id, long level )
			returns long;
	ObservedChanged(/* struct lookzview *self, */ struct dataobject *dobj, long status);

methods:
	
	SetVisibility( /* struct lookz *self, */ boolean visible );
	SetTextObject(struct text *text);
	SetCanClose(boolean canClose);

macromethods:
	GetVisibility( /* struct lookz *self */ ) (((struct lookz *)self)->visible + 0)

        GetCanClose() (self->canClose)
        GetTextObject() (self->text)
classprocedures:

	InitializeObject(/* struct classhdr *ClassID;*/ struct lookz *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID;*/ struct lookz *self);

data:

	boolean visible;		/* if FALSE, only an icon is displayed
						if TRUE, the editor is displayed 
						this field is constrained to be 
						TRUE or FALSE  */

	boolean canClose;
	struct text *text;
};

