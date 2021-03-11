/* ********************************************************************** *\
 *         Copyright IBM Corporation 1989, 1991 - All Rights Reserved           *
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

*/

#include <view.ih>

#define sbuttonv_BORDEROUT FALSE
#define sbuttonv_BORDERIN TRUE

/* for saving a views state from before to after a sbuttonv drawing operation */
struct sbuttonv_view_info {
    struct fontdesc *font;
    double fgr, fgg, fgb;
    double bgr, bgg, bgb;
    short transfermode;
};

/* info relevant to each individual button */
struct sbuttonv_info {
    struct rectangle rect;
    long drawflag;
};

#define sbuttonv_LEFTBUTTON 1
#define sbuttonv_RIGHTBUTTON 2

enum sbuttonv_conv {
    sbuttonv_Interior,
    sbuttonv_Enclosing
};

class sbuttonv: view {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct sbuttonv *self) returns boolean;
    FinalizeObject(struct sbuttonv *self);
    SaveViewState(struct view *self, struct sbuttonv_view_info *vi);
    RestoreViewState(struct view *self, struct sbuttonv_view_info *vi);

    DrawBorder(struct view *v, long x, long y, long w, long h, struct sbutton_prefs *prefs, boolean lit, boolean draw, struct rectangle *interior);
/*
  just like DrawBorder except it uses a rectangle for the enclosing box */
    DrawRectBorder(struct view *v, struct rectangle *enclosing, struct sbutton_prefs *prefs, boolean lit, boolean draw, struct rectangle *interior);
    
    SizeForBorder(struct view *v, enum sbuttonv_conv dir, int style, boolean lit, long w, long h, long *rw, long *rh);
    InteriorBGColor(struct view *v, struct sbutton_prefs *prefs, boolean lit, double *result);
    DrawButton(struct view *v, struct sbutton_info *b, struct rectangle *r);
    SafeDrawButton(struct view *v, struct sbutton_info *b, struct rectangle *r);
    HighlightButton(struct view *v, struct sbutton_info *b, struct rectangle *r);
    UnHighlightButton(struct view *v, struct sbutton_info *b, struct rectangle *r);

    CreateFilledSButtonv(char *defview, struct sbutton_prefs *prefs, struct sbutton_list *blist) returns struct sbuttonv *;
    
    DrawLabel(struct view *v, char *label, long x, long y, struct sbutton_prefs *b, boolean lit, long flags);
    
/*  special case of drawlabel which draws the label in the middle of the rectangle passed. */
    DrawButtonLabel(struct view *v, char *label, struct rectangle *interior, struct sbutton_prefs *prefs,  boolean lit);

  overrides:
    ObservedChanged (struct observable *changed, long value);
    DesiredSize(long width, long height, enum view_DSpass pass, long * desired_width, long * desired_height) returns enum view_DSattributes;
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    LinkTree(struct view *parent);
    WantUpdate(struct view *requestor);
    GetOrigin(long width, long height, long *originX, long *originY);
    SetDataObject(struct dataobject *d);
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);

    
  methods:
    WhichButton(long x, long y) returns int;
    SetActiveMouseButtons(unsigned char active, unsigned char deactive) returns unsigned char;
    Touch(int ind, enum view_MouseAction action) returns boolean;
    
  macromethods:
    GetVBorder() ((self)->vborder)
    GetHBorder() ((self)->hborder)
    GetVSpacing() ((self)->vspacing)
    GetHSpacing() ((self)->hspacing)
    ButtonData() ((struct sbutton *)view_GetDataObject((struct view *)self))
    LastButton() (((self)->lastbutton<((struct sbutton *)view_GetDataObject((struct view *)self))->count && (self)->lastbutton>=0)?(self)->lastbutton:0)
    Litp(ind) (!((ind)<0 || (ind)>=((struct sbutton *)sbuttonv_GetDataObject((struct sbuttonv *)self))->count || !((struct sbutton *)sbuttonv_GetDataObject((struct sbuttonv *)self))->buttons[ind].litp))
    
    GetDoTriggers() ((self)->dotriggers)
    
  data:
    int maxheight, maxwidth, lastbutton, lasthighlight;
    int needredraw;
    int vborder, hborder;
    int vspacing, hspacing;
    boolean awaitingUpdate;
    int specialwidth;
    boolean dotriggers;
    unsigned char activebuttons;
    struct sbuttonv_info *info;
    int bcount;
    boolean forceupdate;
    long lwidth, lheight;
    long drawcount;
    boolean drawmatte;
};

