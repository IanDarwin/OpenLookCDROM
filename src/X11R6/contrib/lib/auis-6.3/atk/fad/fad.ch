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


 

#ifndef FAD_DEFS
#define FAD_DEFS
#define fad_NEWFAD 10000
#define OLD 1
#define NEW 0
#define PFUG 4
#define min(a,b) (a < b) ? a:b
#define ICONFLAG -10
#define LABELFLAG -10000
#define ANIMATEFLAG -9999
#define ISICONORLABEL(A) ((A) < 0)
#define ISICON(A) ((A) < 0 && (A) != LABELFLAG)
#define ISICONVEC(V) (ISICON(V->p2->x))
#define MAXWIDTH 20000
#define MAXHEIGHT 20000
#define LINEMODE 0
#define BOXMODE 1
#define ANIMATEMODE 2
#define FRTIME 30 	/* default minimum number of milliseconds between frames */
struct anivect {
	float x1,y1,y2,x2,dx1,dx2,dy1,dy2;
	char *label;
	char mode;
	};
struct fadpoint {
	long x,y;
	struct fadpoint *p;
	};
struct vector {
	struct fadpoint *p1,*p2;
	struct vector *v;
	char *label;
	char stat;
	char mode;
	};
struct fad_frame {
	struct vector *v;
	struct fadpoint *p;
	struct fad_frame *f;
	struct anivect *av;
	};
#endif /* FAD_DEFS */
class fad :dataobject[dataobj] {
overrides:
    Read(FILE *f, long id) returns long;
    Write(FILE *f, long writeid, int level) returns long;
    SetAttributes(struct attributes *attributes);
methods:
    newpoint(long x,long y) returns struct fadpoint *;
    newframe() returns struct fad_frame *;
    SetName(char *name);
    setpoint(long x,long y, int type,struct fad_frame *f) returns struct fadpoint *;
    setvector(struct fadpoint *pp1,struct fadpoint *pp2,struct fad_frame *f) returns struct vector *;
    delvector(struct fad_frame *f);
    iconnum(char *s) returns short;
    freeframe(struct fad_frame *ff);	
    flipicons()returns int;
    unflipicons()returns int;
    SetReadOnly(boolean readOnly);
classprocedures:
    InitializeObject(struct fad *self) returns boolean;
    FinalizeObject(struct fad *self);
data:
    short ox,oy,w,h,pltnum,desw,desh;
    struct fad_frame *f,*bf,*deleated;
    struct fadpoint *fp,*lp;
    struct vector *veclist[64];
    short  Frames,iconmode,currentfontindex,topinmp ,fad_square,frtime,readonly,initializedfonts ;
    char cfname[128],currentistr[128], currenticon,fadname[256], iconnamebuf[512],*inmp[15],labelfontname[256];
    struct fontdesc *fontpt[15], *currentfont,*labelfont;
    struct fadpoint *iconpoints[200],**iconpointend, **iconpointnow;
    struct cursor *cursor;
    char mode;
};
