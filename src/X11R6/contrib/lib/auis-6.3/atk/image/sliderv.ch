

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
class sliderv : view {
classprocedures:
	InitializeClass() returns boolean;
	InitializeObject(struct sliderv *self) returns boolean;
	FinalizeObject(struct sliderv *self);
overrides:
	GetInterface(char *interfaceName) returns char *;
    	FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    	Update();
    	WantUpdate(struct view *requestor);
	LinkTree(struct view *parent);
	UnlinkNotification(struct view *unlinkedTree);
        Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
methods:
        SetCallback(long (*callback)(), long rock);
        DoCallback(long arg);
	SetCurval(long curval); /* scale of 0 to RANGE */
	GetCurval() returns long;
data:
        long (*callback)();
        long rock;
	struct scroll *scrollbar;	/* real atk scrollbar view */
	long curval, range;
};
