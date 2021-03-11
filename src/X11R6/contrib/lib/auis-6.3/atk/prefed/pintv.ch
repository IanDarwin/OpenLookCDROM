/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

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



class pintv : lpair {
classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct pvaltv *self) returns boolean;
    FinalizeObject(struct pvaltv *self);
methods:
overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
    SetDataObject(struct dataobject *d);
    Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    WantInputFocus(struct view *requestor);
    WantUpdate(struct view *requestor);
    ObservedChanged(struct observable *changed, long value);
macromethods:
data:
    struct keystate *ks;
    struct menulist *menulist;
    boolean redosizes;
    struct sbuttonv *buttons;
    struct view *cpref_al;
    struct textview *cpref;
    struct lpair *top;
    struct lpair *labelpair;
    struct lpair *leftpair;
    struct lpair *rightpair;
    struct lpair *blr;
    struct labelview *catlabel;
    struct view *categories_al;
    struct textview *categories;
    struct labelview *plabel;
    struct view *preferences_al;
    struct textview *preferences;
    struct environment *cat_sel, *pref_sel;
    char *category;
    char *pref;
    struct prefdesc *cpd;
    struct list *prefslist;
    boolean newerrors;
    struct text *errors;
    struct event *reportevent;
    struct event *uevent;
    struct event *oevent;
    struct phelpv *helpv;
    struct view *helpva;
    struct sbutton_prefs *prefs;
    boolean autolist;
    boolean lockdown;
};
