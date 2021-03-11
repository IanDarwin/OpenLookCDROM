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

#include <view.ih>

struct dialogv_HitRock {
    procedure func;
    long rock;
};

class dialogv : view {
classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct sbutton *self) returns boolean;
    FinalizeObject(struct sbutton *self);
    Create(char **list, char *font, int style) returns struct dialogv *;
    
overrides:
    SetDataObject(struct dataobject *dataobject);
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);

    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    LinkTree(struct view *parent);
    PostMenus(struct menulist *ml);
    ReceiveInputFocus();
    
macromethods:
    GetTextView() ((self)->text)
    GetButtonsView() ((self)->buttons)
    GetTextData() ((struct text *)view_GetDataObject((struct view *)(self)->text))
    GetButtonsData() ((struct sbutton *)view_GetDataObject((struct view *)(self)->buttons))
    GetExtraMenus() ((self)->extramenus)
    GetExtraKeys()  ((self)->extrakeys)
    
methods:
   PostChoice(struct im *im, struct view *client, boolean *cflag, int deflt, boolean blocking, long pos) returns int;
   PostInput(struct im *im, struct view *client, procedure choicefunc, long choicerock, boolean blocking, long pos) returns int;
   SetLayout(int rows, int cols);
   InstallRider(struct view *rider);
   InstallSidekick(struct view *sidekick);
   SetExtraMenus(struct menulist *ml);
   SetExtraKeyState(struct keystate *ks);
   ActivateButton(int ind);
   DeActivateButton(int ind);
   Vanish();
   UnVanish();
   CancelQuestion();
   
data:
    struct textview *text;
    struct sbuttonv *buttons;
    struct view *rider, *sidekick;
    long rwidth, rheight, roffset;
    long twidth, theight;
    long swidth, sheight;
    long bwidth, bheight;
    boolean destroyall;
    struct dialogv_HitRock hr;
    struct menulist *extramenus;
    struct keystate *extrakeys;
    struct view *client;
    boolean didsizeonce;
    int lastchoice;
    boolean *cont;
};

