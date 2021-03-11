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
nullv.H

	Class definitions for null inset view

*/
/*
 *    $Log: nullv.ch,v $
*Revision 1.6  1993/05/04  01:14:27  susan
*RCS Tree Split
*
*Revision 1.5.1.1  1993/02/02  01:41:13  rr2b
*new R6tape branch
*
*Revision 1.5  1992/12/14  20:40:31  rr2b
*disclaimerization
*
Revision 1.4  1992/06/22  21:48:52  wjh
minor reformatting
.ting fontdesc
made blobs chartreuse on color displays
removed extraneous clearing of screen in _ClearRectangle
check for NULLs in InitializeClass
give a default desiredSize
.

Revision 1.3  1991/09/12  19:31:03  bobg
Update copyright notice

Revision 1.2  1989/12/12  15:00:16  ghoti
sync with MIT tape

Revision 1.2  89/12/05  11:33:54  xguest
change instances of nullv to nullview

Revision 1.1  89/12/04  20:27:58  xguest
Initial revision

Revision 1.1  89/07/31  15:35:27  wjh
Initial revision


Revision 1.0  88/05/14  15:40:34  wjh
Copied from /usr/andrew/lib/genericinset
 */


class nullview [nullv] : view
{
overrides:

	FullUpdate(/* struct nullview *self, */ enum view_UpdateType type, 
			long left, long top, long width, long height);
	Update(/* struct nullview *self, */);
	Hit(/* struct nullview *self, */ enum view_MouseAction action, 
			long x, long y, long n)	returns struct view *;
	DesiredSize(/* struct nullview *self, */ long width, long height, 
			enum view_DSpass pass, 
			long *desiredWidth, long *desiredHeight) 
				returns enum view_DSattributes;
	ReceiveInputFocus(/* struct nullview *self */);
	LoseInputFocus(/* struct nullview *self */);
	ObservedChanged(/* struct nullview *self, */ struct null *dobj, long status);
	Print(/* struct nullview *self, */ FILE *file, 
			char *processor, char *finalFormat, boolean topLevel);
	GetApplicationLayer(/* struct nullview *self */) returns struct nullview *;
	

methods:

	/* $$$ methods unique to this view.
		These would be defined if a parent view were to be given 
		greater control over this view. */

classprocedures:

	InitializeClass() returns boolean; 	/* Create default Keymap & Menus */
	InitializeObject(struct nullview *self) returns boolean;
	FinalizeObject(struct nullview *self);

data:
	struct menulist  *Menus;
	struct keystate *Keystate;

	struct graphic  *BlackPattern;	/* these might differ between instances . . . */
	struct graphic  *WhitePattern;	/*		. . . on different displays */

	boolean OnScreen;			/* if not view_Removed */
	boolean embedded;		/* TRUE if no call to GetApplicationLayer */
	boolean HasInputFocus;		/* T if received input focus */
	boolean ignoreUp;			/* T iff have just asked for InputFocus */
	boolean sizeknown;		/* T if haven't called ComputeArea */

	/* $$$ some facts basic to computing desired size
		null chooses to have a given area if the parent
		constrains height or width */
	long DesiredArea;			/* total space desirable in square pixels */

	/* $$$ save desired size if the computation of it was expensive */
	long DesiredHeight, DesiredWidth;	/* requested dimensions; set by 
						ComputeArea*/
};
