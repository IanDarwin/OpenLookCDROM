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


 
#define arbiterview_SEPCHAR ':'

class arbiterview[arbiterv]: celview[celv] {
overrides:
    ObservedChanged (struct observable *changed, long value);
    SetDataObject(struct dataobject *ls);
    WantHandler(char *handlerName) returns struct basicobject *;
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    LinkTree(struct view *parent);
methods:
    GetDataName() returns char *;
    registername(struct celview *cv,char *refname) returns int;
    InitCell(struct celview *cv);
    DeleteCell(struct celview *cv);
    CreateCon(struct text *EditText) returns boolean;
    lookupname(char *ViewName) returns struct celview *;
    ArbRead(char *frs);
    InitArbcon();
    GetArbName(char *buf,long buflen) returns long;
    AddHandler(int (*handler)(),long rock);
    InTree() returns boolean;
macromethods:
    GetCurrentCelview(cl) (self->currentcelview)
    GetCopyMode() (self->CopyMode)
    SetCopyMode(bool) self->CopyMode = bool;
    GetCelMode() (self->CelMode)
    SetCelMode(bool) self->CelMode = bool;
classprocedures:
    InitializeClass()returns boolean;
    FinalizeObject(struct arbiterv *self);
    InitializeObject(struct arbiterv *self) returns boolean;
    GetNamedObject(struct view *vw,char *ObjectName) returns struct dataobject *;
    GetNamedView(struct view *vw,char *ViewName) returns struct view *;
    GetNamedCelview(struct view *vw,char *ViewName) returns struct celview *;
    SetIgnoreUpdates(struct view *vw,boolean val);
    GetFirstLink() returns struct arbiterview*;
    FindArbByName(char *str) returns struct arbiterview *;
    FindArb(struct view *vw) returns struct arbiterview *;
data:
    struct celview **celviewlist;
    struct celview *currentcelview;
    int celcount , celsize;
    struct value *obval,*vwval,*objectchoice,*viewchoice,*applicationchoice,*NameChoice,*CelNameVal;
    struct text *EditText;
    struct arbiterview *next;
    boolean NeedsInit,CopyMode,CelMode;
    int (*handler)();
    long hrock;
};

