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


 


class tmview[tmv]: textview[textv] {
    overrides:
	SetDataObject(struct termulator *do);
	PostKeyState(struct keystate *ks);
	PostMenus(struct menulist *ml);
	FullUpdate(enum view_UpdateType type,long left,top,width,height);
	Update();
	ReceiveInputFocus();
	GetClickPosition(pos,noc,action,startLeft,startRight,leftPosP,rightPosP);
    methods:
	SetFileMenus(boolean on);
	ReadShMenus(char *filename) returns boolean;
    macromethods:
	SetTitle(char *t) (self->title=(t))
        GetTitle() (self->title)
    classprocedures:
	InitializeClass() returns boolean;
        InitializeObject(struct tmview *tmv) returns boolean;
	FinalizeObject(struct tmview *tmv);
    data:
        int screen;
        struct keystate *keystate;
	struct menulist *menus, *shmenus;
	struct mark *curpos;
	char *cwd, *title;
	int height,width;
};
