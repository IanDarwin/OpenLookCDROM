/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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


/*
  Trigger:  "buttonpushed"
     called when the user lets up on the pushbutton over the pushbutton.

  Call pushbuttonview_HitButton(self) as shorthand for pulling the trigger.

*/

class pushbuttonview[pshbttnv]: view {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct pushbuttonview *self) returns boolean;
    FinalizeObject(struct pushbuttonview *self);
  overrides:	
    ObservedChanged (struct observable *changed, long value);
    DesiredSize(long width, long height, enum view_DSpass pass, long * desired_width, long * desired_height) returns enum view_DSattributes;
    GetOrigin(long width, long height, long * originX, long * originY);
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    LinkTree(struct view *parent);
    PostMenus(struct menulist *ml);
    WantUpdate(struct view *requestor);
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);

  macromethods:
    HitButton() (pushbutton_PullTrigger((struct pushbutton *) pushbuttonview_GetDataObject(self), atom_Intern("buttonpushed")))
  data:
    short lit;
    struct cursor *cursor;
    double foreground_color[3], background_color[3];
    char *cached_label;
    int cached_style;
    struct fontdesc *cached_fontdesc;
    struct menulist *ml;
    int awaitingUpdate;
};

