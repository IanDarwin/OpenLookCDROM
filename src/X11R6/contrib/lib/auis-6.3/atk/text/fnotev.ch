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




class fnotev: view {
overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    ObservedChanged (struct observable *changed, long value);
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    LinkTree(struct view *parent);
    GetOrigin(long width, long height, long *originX, long *originY);
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
    ReceiveInputFocus();
methods:
    pushchild();
    popchild();
macromethods:
    SetDisplayStr(S) ((self)->displaystr = (S))
    GetParentTextview() ((self)->parentview)
    GetParentText() ((self)->parenttext)
classprocedures:
    SetEndnote(boolean doendnotes);
    InitializeClass() returns boolean;
    InitializeObject(struct fnotev *self) returns boolean;
    FinalizeObject(struct fnotev *self) ;
data:
    struct impair *imp;
    struct fontdesc *fd,*ofd;
    struct cursor *cursor;
    int fnotetype;
    struct text *parenttext;
    struct textview *parentview;
    char *displaystr;
    struct  fontdesc_charInfo *ci;
};

