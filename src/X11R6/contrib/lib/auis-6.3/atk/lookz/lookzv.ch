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
lookzview.H

	Class definitions for lookz view
*/


#define  lookzview_PROGRAMMERVERSION    1

class lookzview[lookzv] : view {
overrides:

	FullUpdate(/* struct lookzview *self, */ enum view_UpdateType type, 
		long left, long top, long width, long height);
	Update(/* struct lookzview *self */ );
	Hit(/* struct lookzview *self, */ enum view_MouseAction action, long x, long y, long n)
			returns struct view *;
	DesiredSize(/* struct lookzview *self, */ long width, long height, enum view_DSpass pass, 
				long *desiredWidth, long *desiredHeight) 
			returns enum view_DSattributes;
	WantInputFocus(/* struct lookzview *self, */ struct view *child);
	ReceiveInputFocus(/* struct lookzview *self */);
	LoseInputFocus(/* struct lookzview *self */);
	Print(/* struct lookzview *self, */ FILE *file, char *processor, char *finalFormat, boolean topLevel);
	ObservedChanged(/* struct lookzview *self, */ struct stylesheet *dobj, long status);
	GetApplicationLayer(/* struct lookzview *self */) returns struct lookzview *;
	LinkTree(/* struct lookzview *self, */ struct view *parent);
	SetDataObject(struct dataobject *dobj);

methods:

	SetVisibility( /* struct lookzview *self, */ boolean visibility );
	GetVisibility( /* struct lookzview *self */ ) returns boolean;
	SetStyleSheet( /* struct lookzview *self, */ struct stylesheet *ss );
	GetStyleSheet( /* struct lookzview *self */ ) returns struct stylesheet *;

	SetEmbedded(boolean isEmbedded);

macromethods:
        GetEmbedded() self->embedded;

classprocedures:

	InitializeClass(/* struct classhdr *ClassID*/) returns boolean;
			/* Create default Keymap & Menus */
	InitializeObject(/* struct classhdr *ClassID;*/ struct lookzview *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID;*/ struct lookzview *self);

data:

	boolean embedded;
	struct stringtbl *(st[11]);
	struct strtblview *(stv[11]);
	struct lprruler *ruler;
	struct lprrulerview *rulerview;
	struct lprruler *tabruler;
	struct tabrulerview *tabrulerv;
	struct lpair *image;
	struct labelview *shrinkicon;
	struct lpair *shrinkparent;
	boolean Linked;
	boolean OnceOnlyInUpdate;

	boolean foundstylesheet;
	struct stylesheet *curss;		/* the stylesheet being edited */
	char *curcard;			/* points to the stringtbl name entry for
							current menucard */
	struct style *curstyle;		/* the style named by menucard/stylename */

	struct menulist *MyMenus;	/* copy of the class menus in MenuList */
	struct keystate *Keystate;		/* retain state for multi-keystroke inputs */
	boolean HasInputFocus;
	boolean NeedsUnpack;	/* specifically for change to stylesheet (via another lookz) */
	boolean OnScreen;
};
