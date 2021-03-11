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



class wrapv : view {
classprocedures:
    InitializeObject(struct wrapv *self) returns boolean;
    FinalizeObject(struct wrapv *self);
methods:
    Keys() returns struct keystate *;
    Menus() returns struct menulist *;
macromethods:
    GetInterfaceView() (self->intv)
    SetInterfaceView(struct view *v) (self->intv=(struct view *)(v))
    GetView() (self->tv)
    GetData() (self->t)
    SetView(struct view *v) (self->tv=(struct view *)(v))
    SetData(struct dataobject *d) (self->t=(struct dataobject *)d)
overrides:
    LinkTree(struct view *parent);
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    GetOrigin(long width, long height, long *originX, long *originY);
    WantInputFocus(struct view *requestor);
    InitChildren();
    PostMenus(struct menulist *ml);
    PostKeyState(struct keystate *ks);
    WantUpdate(struct view *requestor);

    
macromethods:
data:
   struct dataobject *t;
   struct view *tv;
   struct view *intv;
};
