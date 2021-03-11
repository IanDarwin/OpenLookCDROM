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


 


#include <rect.h>

class cursor {
methods:
	SetGlyph(struct fontdesc *fd,short ch);
	SetStandard(short ch);
/************ private methods, not to be called by modules outside of cursor *******/
        ChangeShape();
macromethods:
        IsPosted() ((self)->posted != NULL)
        IsWindowCursor() ((self)->windowim != NULL)
        IsProcessCursor() ((self)->processC)
classprocedures:
        InitializeObject(struct cursor *self) returns boolean;
	Create(struct view *v) returns struct cursor *;
	FinalizeObject(struct cursor *self); /*  used to get rid of active cursors;
 */
data:
	struct view *view;
	struct im *posted;
	struct im *windowim;
	struct cursor *next;
	boolean processC;
	boolean changed;
	struct fontdesc *fillFont;
	int fillChar;
};

#define Cursor_Arrow			1
#define Cursor_Gunsight			2
#define Cursor_Cross			3
#define Cursor_Wait			4
#define Cursor_HorizontalBars		5
#define Cursor_LowerRightCorner		6
#define Cursor_UpperLeftCorner		7
#define Cursor_VerticalBars		8
#define Cursor_DangerousBend		9
#define Cursor_Caret			10
#define Cursor_VerticalArrows		11
#define Cursor_HorizontalArrows		12
#define Cursor_Octagon			13
#define Cursor_LeftPointer		14
#define Cursor_RightPointer		15
#define Cursor_CrossHairs		16
#define Cursor_DownPointer		17
#define Cursor_UpPointer		18
#define	Cursor_Cutter			19
#define Cursor_LargeUpArrow		20
#define	Cursor_LargeDownArrow		21
#define	Cursor_EnclosedDot		22
#define	Cursor_SmallCross		23
#define	Cursor_SmallLeftPointer		24
#define	Cursor_SmallRightPointer	25


#define	Cursor_TABLELEN			26

