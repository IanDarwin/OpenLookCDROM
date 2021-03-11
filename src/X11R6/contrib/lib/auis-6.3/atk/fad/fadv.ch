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


 

#include <fad.ih>
class fadview[fadv] : view {
overrides:
	DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
	Update();
    	FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
   	ReceiveInputFocus();
    	LoseInputFocus();
    	Hit (enum view_MouseAction action, long mousex, long mousey, long numberOfClicks) returns struct fadview *;
	Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
	ObservedChanged( struct observable *changed, long value);
	SetDataObject(struct dataobject *dataObject);
methods:
	aniframe(int framecount,int startat,int gofor,int mtm);
	nextframe(struct fad *cp);
	lastframe(struct fad *cp);
	showfad(int i,struct fad *cp);
	geticons(char *s)returns int;	
	fileread(char *fnm);

classprocedures:
InitializeObject(struct fadview *self) returns boolean;
FinalizeObject(struct fadview *self);
InitializeClass() returns boolean;
data:
	int HasFocus;
	struct keystate *keystate;
	struct menulist *menulist;
	int framecount;
	int startat;
	int gofor;
	int mtm;
	int DoAnimation;
	struct cursor *cursor;
	int Moving;
    int removed;
    int anbufsize;
    boolean Redraw,FrameChanged,animationPending,needUpdate,FocusChanged;
    struct fad_frame *f;
    int mode;
struct aniinfo *anobj;
struct event *nextevent;
boolean updatedue;
};
