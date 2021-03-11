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


 

struct pagev_switcheroo {
    struct page_switchee *switchee;
    struct view *v;
    struct pagev_switcheroo *next;
};

class pagev:view {
    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct pagev *self) returns boolean;
      FinalizeObject(struct pagev *self);
    overrides:
      FullUpdate(enum view_UpdateType type, long left,
		  long top, long width, long height);
      Update();
      Hit (enum view_MouseAction action, long x, long y,
	    long numberOfClicks) returns struct view *;
      PostKeyState(struct keystate *keystate);
      PostMenus(struct menulist *ml);
      LinkTree(struct view *parent);
      WantInputFocus(struct view *v);
      InitChildren() ;
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
    ObservedChanged (struct observable *changed, long value);

    data:
      struct keystate *ks;
      struct menulist *ml;
      struct pagev_switcheroo *Firstswitcheroo, *NowPlaying;
};
