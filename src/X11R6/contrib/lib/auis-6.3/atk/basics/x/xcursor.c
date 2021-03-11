/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/x/RCS/xcursor.c,v 1.16 1994/04/11 19:12:03 rr2b Exp $";
#endif


 

#include <andrewos.h>
#include <class.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include <fontdesc.ih>
#include <xfontd.ih>
#include <xgraphic.ih>
#include <cursor.ih>
#include <view.ih>
#include <im.ih>
#include <xim.ih>
#include <environ.ih>

static char CursorTable[] = {  'a',
    'a','g','x','H','h','l','u','v','w','|',
    'J',':','C','R','M',';','/','^','s','U',
    'D','@','X','~','}' };
#define lookup(A) ((A >= 0 && A < Cursor_TABLELEN)?  CursorTable[A] :CursorTable[0])

#include <xcursor.eh>

extern int cursordebug;
static char *foreground;
static char *background;
static XColor forecolor;
static XColor backcolor;
static Display *colorDisplay = NULL;
static int colorScreen = -1;

static struct ccache {
    Display *dpy;
    struct xfontdesc *xfd;
    int c;
    Cursor cursor;
    struct ccache *next;
} *fc=NULL;


void xcursor__ChangeShape(self)
struct xcursor *self;
{
    self->valid=FALSE;
    if(xcursor_IsPosted(self)){
	self->header.cursor.changed = TRUE;
	im_PostCursor(self->header.cursor.posted,NULL,self);
    }

    if(xcursor_IsWindowCursor(self)){
	self->header.cursor.changed = TRUE;
	im_SetWindowCursor(self->header.cursor.windowim,self);
	}
    if(xcursor_IsProcessCursor(self)){
	self->header.cursor.changed = TRUE;
	im_SetProcessCursor(self);
	}
}


#define DEFAULTFONTNAME "icon"
#define DEFAULTFONTSIZE 12
#define DEFAULTMASKNAME "icon"
void xcursor__SetStandard(self,ch)
struct xcursor *self;
short ch;
{
    struct fontdesc *oldfont=self->header.cursor.fillFont;
    char oldchar=self->header.cursor.fillChar;
    
    self->header.cursor.fillFont = fontdesc_Create(DEFAULTFONTNAME,0,DEFAULTFONTSIZE);
    self->header.cursor.fillChar = lookup(ch);
    if(oldfont!=self->header.cursor.fillFont || oldchar!=self->header.cursor.fillChar) xcursor_ChangeShape(self);
}

Cursor LookupCursor(dpy, xfd, c)
Display *dpy;
struct xfontdesc *xfd;
int c;
{
    struct ccache *cc=fc;
    while(cc!=NULL) {
	if(cc->dpy==dpy && cc->xfd==xfd && cc->c==c) return cc->cursor;
	cc=cc->next;
    }
    return None;
}

void xcursor__Make(self, im)
struct xcursor *self;
struct xim *im;
{
    Cursor tmp = None;
    XFontStruct *info;
    XCharStruct *ci;
    XGCValues gcattr;
    Pixmap sourcePixmap, maskPixmap;
    GC gc;
    char str[1];
    unsigned int width, height;
    long x, y;
    struct xfontdesc * xfillFont = (struct xfontdesc *) self->header.cursor.fillFont;

    if (cursordebug) printf("xim_MakeXCursor: making cursor for %d in %X for %X\n", self->header.cursor.fillChar, xfillFont, self);

    if (self->valid && self->Xc != None && self->im == im) {
	if(!self->header.cursor.changed) return; /* Already valid cursor */

	/* XFreeCursor(xim2display(im), self->Xc); don't free it since it will stay in the cache */

	self->Xc=None;	
    }

    tmp=LookupCursor(xim2display(im), xfillFont, self->header.cursor.fillChar);

    if (tmp==None && self->header.cursor.fillChar) {

	info = (XFontStruct *)xfontdesc_GetRealFontDesc(xfillFont,
							im->header.view.drawable);

	/* This is the code for constructing an andrew semantics cursor from the single character
	    The basic idea is:
	    get the size of the icon
	    make the mask:
	    or in the icon at each of the nine locations centered around its hot spot
	    make the shape
	    or in the icon at 8 locations around hot spot and xor in at the hotspot
	    create it
	    toss away temp pixmaps, gc, etc. */

	/*      fillChr = cursor->header.cursor.fillChar; */
	if (self->header.cursor.fillChar >= info->min_char_or_byte2 && self->header.cursor.fillChar <= info->max_char_or_byte2)  {
	    ci = (info->per_char != NULL) ? &(info->per_char[self->header.cursor.fillChar - info->min_char_or_byte2]) : &(info->max_bounds);
	    /* Note that we use a slightly bigger pixmap so that we have room to create a white outline for a font that is defined right to the edge. Diito for moving the original over a little bit, so that we have room in the larger pixmap to fit the white board on any of the four sides */
	    width = ci->rbearing - ci->lbearing + 2;
	    /* Bug patch -- 15 pixel wide cursors don't work, so instead we enalrge it by one more pixel. Sigh. */
	    if (width == 15) width++;
	    height = ci->descent + ci->ascent + 2;
	    x = -ci->lbearing + 1;
	    y = ci->ascent + 1;
	    if (width == 0) {
		fprintf(stderr,"xim_MakeXCursor: pixmap of width 0 requested by %X\n", im);
		width++;
	    }
	    if (height == 0) {
		fprintf(stderr,"xim_MakeXCursor: pixmap of height 0 requested by %X\n", im);
		height++;
	    }

#ifndef M_UNIX
	    XQueryBestCursor(xim2display(im), xim2window(im), width, height, &width, &height);
#endif /* M_UNIX */

	    maskPixmap = XCreatePixmap(xim2display(im),RootWindow(xim2display(im), DefaultScreen(xim2display(im))), width, height, 1);
	    gcattr.fill_style = FillSolid;

	    gcattr.foreground = 1;
	    gcattr.font = info->fid;
	    gcattr.function = GXclear;
	    gc = XCreateGC(xim2display(im), maskPixmap, GCFillStyle | GCForeground | GCFont | GCFunction, &gcattr);

	    /* Clear out the pixmap so it has all zeros */
	    XFillRectangle(xim2display(im),maskPixmap, gc, 0, 0, width, height);
	    XSetFunction(xim2display(im),gc,GXor);
	    str[0] = self->header.cursor.fillChar;
	    XDrawString(xim2display(im), maskPixmap, gc, x, y, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x-1, y, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x, y-1, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x-1, y-1, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x+1, y, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x, y+1, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x+1, y+1, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x-1, y+1, str, 1);
	    XDrawString(xim2display(im), maskPixmap, gc, x+1, y-1, str, 1);
	    XFreeGC(xim2display(im), gc);

	    /* Now we have the mask, let's build the source */
	    sourcePixmap = XCreatePixmap(xim2display(im),RootWindow(xim2display(im), DefaultScreen(xim2display(im))),width,height,1);
	    gc = XCreateGC(xim2display(im), sourcePixmap, GCFillStyle | GCForeground | GCFont | GCFunction, &gcattr);

	    /* Clear out the pixmap so it has all zeros */
	    XFillRectangle(xim2display(im),sourcePixmap, gc, 0, 0, width, height);
	    XSetFunction(xim2display(im),gc,GXor);
	    str[0] = self->header.cursor.fillChar;
	    XSetFunction(xim2display(im),gc,GXcopy);
	    XDrawString(xim2display(im), sourcePixmap, gc, x, y, str, 1);
	    XFreeGC(xim2display(im), gc);

	    if (xim2display(im) != colorDisplay || xim2screen(im) != colorScreen) {
		Status status;

		colorDisplay = xim2display(im);
		colorScreen = xim2screen(im);

		status = XParseColor(colorDisplay, DefaultColormap(colorDisplay, colorScreen), foreground, &forecolor);
		if (status == 0) {
		    forecolor.pixel = 0 /* or so FontCursor does */;
		    forecolor.red = forecolor.green = forecolor.blue = 0 /* ~0 */ ;
		    forecolor.flags = 0;
		}

		status = XParseColor(colorDisplay, DefaultColormap(colorDisplay, colorScreen), background, &backcolor);
		if (status == 0) {
		    backcolor.pixel = 0 /* or so FontCursor does */;
		    backcolor.red = backcolor.green = backcolor.blue = 0 /* ~0 */ ;
		    backcolor.flags = 0;
		}
	    }

	    /* We have the pixmaps in hand, let's create cursors! */
	    tmp = XCreatePixmapCursor(xim2display(im),sourcePixmap,maskPixmap, &forecolor,&backcolor, x, y);
	    if(tmp!=None) {
		struct ccache *nc=(struct ccache *)malloc(sizeof(struct ccache));
		if(nc==NULL) return;
		nc->dpy=xim2display(im);
		nc->xfd=xfillFont;
		nc->c=self->header.cursor.fillChar;
		nc->cursor=tmp;
		nc->next=fc;
		fc=nc;
	    }
	    /* And toss away the pixmaps */
	    XFreePixmap(xim2display(im),maskPixmap);
	    XFreePixmap(xim2display(im),sourcePixmap);
	} /* have legal characters */
	else printf("xim_ActivateCursor: illegal character %c used for cursor\n",self->header.cursor.fillChar);
    }
    
    if (tmp != None) {
	/* if (self->Xc) XFreeCursor(self->Xd, self->Xc); */
	self->Xc = tmp;
	self->Xd = xim2display(im);
	self->im = im;
	self->valid = TRUE;
    }
}

boolean xcursor__InitializeObject(classID,self)
struct classheader *classID;
struct xcursor *self;
{
    self->Xw = 0;
    self->Xc = 0;
    self->Xd = NULL;
    self->im = NULL;
    self->valid = FALSE;

    return TRUE;
}


void xcursor__FinalizeObject(classID,self)
struct classheader *classID;
struct xcursor *self;
{
	/* if(self->Xc) XFreeCursor(self->Xd, self->Xc); */ /* bogus */
	self->Xc = 0;
	if(self->Xw) XDestroyWindow(self->Xd, self->Xw);
        self->Xw = 0;
}

boolean xcursor__InitializeClass(classID)
struct classheader *classID;
{
    foreground = environ_GetProfile("cursorforegroundcolor");
    if (foreground == NULL || *foreground == '\0') {
	foreground = "black";
    }
    if(foreground!=NULL) foreground=NewString(foreground);
    background = environ_GetProfile("cursorbackgroundcolor");
    if (background == NULL || *background == '\0') {
	background = "white";
    }
    if(background!=NULL) background=NewString(background);
    return TRUE;
}


void xcursor__FinalizeDisplay(classID, dpy)
struct classheader *classID;
Display *dpy;
{
    struct ccache *cc=fc;
    struct ccache **lc=(&fc);
    while(cc) {
	if(cc->dpy==dpy) {
	    (*lc)=cc->next;
	    XFreeCursor(dpy, cc->cursor);
	    free(cc);
	    cc=(*lc);
	} else cc=cc->next;
    }
}
	    
