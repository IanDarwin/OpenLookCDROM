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



#define  tabrulerview_PROGRAMMERVERSION    1

#define tabrulerview_NoValue (-999<<16)

class tabrulerview[tabrulv] : view {
overrides:
    FullUpdate( enum view_UpdateType type, 
		long left, long top, long width, long height );
    Update();
    Hit( enum view_MouseAction action, long x, long y, long n)
      returns struct view *;
    DesiredSize( long width, long height, enum view_DSpass pass, 
		 long *desiredWidth, long *desiredHeight ) 
      returns enum view_DSattributes;

methods:
    SetValues(/* struct tabrulerview *self, */ struct tabs *tabs);
    GetValues(/* struct tabrulerview *self, */ struct tabs **tabs);
macromethods:	
    SetValueChangeProc(proc, rock)   /* store a proc to call when a value changes */   \
      (self->ValueChangeRock = (long)rock, \
	self->ValueChangeProc = (void (*)())proc)
classprocedures:
    InitializeClass(/* struct classhdr *ClassID*/) returns boolean;
    InitializeObject(/* struct classhdr *ClassID;*/ struct tabrulerview *self) returns boolean;
    FinalizeObject(/* struct classhdr *ClassID;*/ struct tabrulerview *self);
data:
    boolean OnScreen;			/* if not view_Removed */
    boolean tabrulerchanged;
    boolean iconschanged;
    boolean textchanged;	/* indicate redraw requirements */

    /* Image patterns might differ between instances  on different displays: */
    struct graphic  *Grey25Pattern, *WhitePattern;

    long leftline, rightline, bottomline, topline, textloc;
    long cleartxt, canceltxt; /* Where to place the text */
    long clearpos, cancelpos; /* Where to place the button */
    long leftzero;
    struct TickTbl *TickTbl;

    void (*ValueChangeProc)();		/* called when a value changes */
    /* ValueChangeProc(tabrulerview, ValueChangeRock, iconcode, newvalue) */
    long ValueChangeRock;			/* passed as second arg to ValueChangeProc */

    enum style_Unit unit;
    struct tabs *tabs;
    short icony;			/* baseline for icons */
    
    boolean Moving;
    long Movex;
    int oldtab;

    int mul; /* Used to scale the ruler to pretend it's real... */
    int div;
};
