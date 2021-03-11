/* bdffontv.ch  -  view for font editor for bdf files */

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
	Copyright Carnegie Mellon University 1991, 1992 - All rights reserved
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

#include <bdffont.ih>

class bdffontv : lpair {
    classprocedures:
	InitializeClass() returns boolean;
	InitializeObject(struct bdffontv *self) returns boolean;
	FinalizeObject(struct bdffontv *self);
    overrides:
	PostKeyState(struct keystate *k);
	PostMenus(struct menulist *m);
	SetDataObject(struct dataobject *dataobject);
	FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
	ObservedChanged(struct observable *changed, long value);
	ReceiveInputFocus();
	WantInputFocus(struct view *target);
    methods:
	SelectCharacter(int encoding);
	UpdateCharModification() returns boolean;
    data:
	boolean initialized;
	long charmodified;
	long rastermodified;
	struct bdffont_fontchar *charinfo;
	struct bdffont_fontchar modinfo;
	long zoomdelta;
	long prevzoom;
	long defns_size;

	struct chlist *charmenuL;

	struct keystate *keys;
	struct menulist *menus;

	struct rasterview *chareditV;
	struct raster *charedit;

	struct lpair *fontinfo;
	struct lpair *fontdata;
	struct lpair *fontpresentation;
	struct lpair *fontsummary;
	struct lpair *ptres;
	struct lpair *originextent;
	struct lpair *menuedit;
	struct lpair *infoedit;
	struct lpair *encodingextent;
	struct chlistview *fontnameV;
	struct chlistview *fontfamilyV;
	struct chlistview *fontfaceV;
	struct chlistview *pthelpV;
	struct chlistview *resV;
	struct chlistview *fontoriginV;
	struct chlistview *fontextentV;
	struct chlistview *charmenuV;
	struct chlistview *charencodingV;
	struct chlistview *charextentV;
};
