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


 

/* lpair.H
 * class header file for lpair view.
 *
 */

#define lpair_VERSION 2

#define lpair_VERTICAL			0 /* The split line runs top to bottom of the screen */
#define lpair_HORIZONTAL		1 /* The split line runs left to right on the screen */

#define lpair_NOCHANGE		(-1)	/* Used when it is wished not to change the current value of a state */

/* values for lpair.sizeform */
#define lpair_PERCENTAGE		0
#define lpair_FIXED			1 /* Compatibility is the mother of hacks... */
#define lpair_BOTTOMFIXED		1
#define lpair_TOPFIXED			2

class lpair:view {
overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    WantUpdate(struct view *requestor);
    LinkTree(struct view *parent);
    DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
    InitChildren();

methods:
    Init(struct view *l1,struct view *l2,long x) ;
    VFixed(struct view *l1,struct view *l2,int bsize, int moveable) returns struct lpair *;
    VTFixed(struct view *l1,struct view *l2,int bsize, int moveable) returns struct lpair *;
    VSplit(struct view *l1,struct view *l2,int pct, int moveable) returns struct lpair *;
    HFixed(struct view *l1,struct view *l2,int bsize, int moveable) returns struct lpair *;
    HTFixed(struct view *l1,struct view *l2,int bsize, int moveable) returns struct lpair *;
    HSplit(struct view *l1,struct view *l2,int pct, int moveable) returns struct lpair *;
    SetMovable(int i) ;
    SetLPState(int porf, int vorh, int movable);
    /* Warning -- if you try to use setlpstate to change top vs bottom fixed, you will
	run into problems that need to be corrected by setting the appropriate objsize.
	This interface needs to be entirely rethought. */
    GetLPState(int *porf, int *vorh, int *movable) ;
    GetNth(int ai) returns struct view *;
    SetNth(int ai, struct view *x) ;
    SetUp(struct view *l1, struct view *l2, int bsize, int porf, int vorh, boolean moveable) returns struct lpair *;
macromethods:
    GetObjSize(i) (self->objsize[i])
classprocedures:
    Create(struct view *l1,struct view *l2,long x) returns struct lpair *;
    InitializeObject(struct lpair *self) returns boolean;
    FinalizeObject(struct lpair *self);

data:
    struct cursor *cursor; /* The "correct" cursor for this lpair. */
    struct view *obj[2];	/* object */
    int objsize[2];	/* obj size */
    int objcvt[2];	/* converted size */
    int typex;	/* vertical or horizontal */
    int lasthit;        /* Coordinate (perpendicular to bar) of last mouse down. */
    char sizeform;	/* form of the size specs */
    char movable; 	/* May we drag the boundary? */
    char ismoving;	/* In the middle of a move */
    char needsfull;	/* Full update needed 'cause either size or children changed */
    boolean maybeZero;		/* true if one half can shrink to 0 */
};
