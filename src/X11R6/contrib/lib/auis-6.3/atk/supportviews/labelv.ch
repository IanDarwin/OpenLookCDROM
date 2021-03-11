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
labelview.H

	Class definitions for label view


	A labelview displays the text stored by a label data object.
	In addition, the labelview can provide image inversion and a hit proc.
	The HitProc is called whenever there is a mouse action on the 
	text within the label image.  The proc is called with these parameters:
		HitProc(labelview, mouseaction, rock)
	where the rock is a 32 bit value passed as a parameter to SetHitProc.
*/

#define  labelview_PROGRAMMERVERSION    1

class labelview[labelv] : view {
overrides:

	FullUpdate(/* struct labelview *self, */ enum view_UpdateType type, 
			long left, long top, long width, long height );
	Update(/* struct labelview *self, */);
	Hit(/* struct labelview *self, */ enum view_MouseAction action, 
			long x, long y, long n)	returns struct view *;
	DesiredSize(/* struct labelview *self, */ long width, long height, 
			enum view_DSpass pass, long *desiredWidth, long *desiredHeight ) 
			returns enum view_DSattributes;
	ObservedChanged(/* struct labelview *self, */ struct label *dobj, long status );
	Print(/* struct labelview *self, */ FILE *file, 
			char *processor, char *finalFormat, boolean topLevel );
	GetApplicationLayer(/* struct labelview *self */) returns struct labelview *;
	
methods:

	SetHitProc(/* struct labelview *self, */ void (*proc)(), char *rock);
	GetHitRock(/* struct labelview *self */) returns char *;
	SetInversion(/* struct labelview *self, */ boolean invert);
	GetInversion(/* struct labelview *self */) returns boolean;

macromethods:

	/* the following is a macromethod because the class processor cannot handle 
			"  returns void(*)()  "		XXX
	*/
	GetHitProc() (self->hitproc)

classprocedures:

	InitializeObject(/* struct classhdr *ClassID;*/ struct labelview *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID;*/ struct labelview *self);

data:

	boolean GaveSize;			/* set after successful DesiredSize */
	
	struct graphic  *BlackPattern;	/* these might differ between instances . . . */
	struct graphic  *WhitePattern;	/*		. . . on different displays */

	boolean embedded;		/* TRUE if no call to GetApplicationLayer */
	boolean OnScreen;			/* if not view_Removed */

	void (*hitproc)();			/* called for a mouse hit */
	char *hitrock;				/* passed to the hit proc */
	boolean inverted;			/* TRUE if image is supposed to be inverted */
	long minwidth, minheight;		/* minimum width and height of the text */
};
