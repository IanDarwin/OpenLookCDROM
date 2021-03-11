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
label.H

	label data object

	stores the font and text string for labelview

	The file format for a label has multiple lines between the lines for
	\begindata and \enddata.  The first line has a font specification consisting
	of familyname, style(as a long value), and size.  
	Subsequent lines are the text to be displayed.
	The text is truncated to be no more than 255 characters 
		and backslashes and curly braces are removed.
	The default font is andy10b.
	If there is no text, the self->text field is NULL.  (strlen(self->text) is never 0)

*/

#define  label_PROGRAMMERVERSION    1

/* status values for NotifyObservers */
#define label_DATACHANGED 1

#include <graphic.ih>

/* values for various attributes */

#define	label_NOJUST		graphic_NOMOVEMENT
#define	label_LEFTJUST		graphic_ATLEFT
#define	label_RIGHTJUST		graphic_ATRIGHT
#define	label_HCENTERJUST	graphic_BETWEENLEFTANDRIGHT
#define	label_TOPJUST		graphic_ATTOP
#define	label_BOTTOMJUST	graphic_ATBOTTOM
#define	label_VCENTERJUST	graphic_BETWEENTOPANDBOTTOM
#define	label_CENTERED		label_HCENTERJUST | label_VCENTERJUST

#define	label_BOXED		0x80


class label : dataobject[dataobj] {

overrides:

	Read(/* struct label *self, */ FILE *file, long id) returns long;
	Write(/* struct label *self, */ FILE *file, long id, long level)
			returns long;

methods:

	SetText(/* struct label *self, */ char *text);
		/* the text is copied within the struct label, so the source is no longer needed */
	SetFont(/* struct label *self, */ char *fontfamily, long style, long size);
		/* The parameters to SetFont are the same as those to fontdesc_Create */
	GetText(/* struct label *self */) returns char *;
		/* returns a pointer to the text.  It must not be modified by the caller */
	GetFont(/* struct label *self, */ long *style, long *size) returns char *;
		/* returns a pointer to an internal buffer containing the font family name;
			it must not be modified */
	SetFlags(/* struct  label *self, */ int flags);
		/* sets the internal flags */

macromethods:
	GetFlags() (self->flags)

classprocedures:

	InitializeObject(/* struct classhdr *ClassID;*/ struct label *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID;*/ struct label *self);

data:

	char *text;	/* the text itself */
	int flags;		/* justification and whether boxed, as per above */
	struct fontdesc *font;	/* the font */
};

