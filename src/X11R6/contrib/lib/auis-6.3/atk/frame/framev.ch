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


 

#define frameview_VERSION 1

class frameview[framev] : textview[textv] {
    overrides:
        Update();
        WantInputFocus(struct view *requestor);
        LoseInputFocus();
        PostMenus(struct menulist *menulist);
        PostKeyState(struct keystate *keystate);
	DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
	WantNewSize(struct view *requestor);
	
    methods:
        SetMessageLine(struct framemessage *messageLine);
        GetMessageLine() returns struct framemessage *;
	Return(long key); 
	Help(long key); 
	Complete(long key); 
	SetWantedLines(int lines);
	
    classprocedures:
        FinalizeObject(struct frameview *self);
        Create(struct framemessage *messageLine) returns struct frameview *;
        InitializeClass() returns boolean;
	InitializeObject(struct frameview *self) returns boolean;    

    data:
        struct framemessage *messageLine;
        struct keystate *keystate;
        struct menulist *menulist;
        struct mark *transientMark;
	struct event *event; /* Timer evet to erase transient messages. */
	boolean amLosingInputFocus;
	long lineHeight, lines;
	long wantsize;
	boolean dynamicsize;
	long minlines;
};

