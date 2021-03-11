/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */


/***********************************************************************
 *
 * $XConsortium: util.c,v 1.47 91/07/14 13:40:37 rws Exp $
 *
 * utility routines for twm
 *
 * 28-Oct-87 Thomas E. LaStrange	File created
 *
 * Do the necessary modification to be integrated in ctwm.
 * Can no longer be used for the standard twm.
 *
 * 22-April-92 Claude Lecommandeur.
 *
 *
 ***********************************************************************/

#include "twm.h"
#include "util.h"
#include "gram.h"
#include "screen.h"
#include <stdio.h>
#ifdef VMS
#include <decw$include/Xos.h>
#include <decw$include/Xatom.h>
#include <X11Xmu/Drawing.h>
#include <X11Xmu/CharSet.h>
#include <X11Xmu/WinUtil.h>
#include <unixlib.h>
#include <starlet.h>
#include <ssdef.h>
#include <psldef.h>
#include <lib$routines.h>
#define USE_SIGNALS
#else
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xmu/CharSet.h>
#include <X11/Xmu/WinUtil.h>
#include "X11/XWDFile.h"
#endif

#if defined(USE_SIGNALS) && defined(__sgi)
#  define _BSD_SIGNALS
#endif

#include <signal.h>
#ifndef VMS
#include <sys/time.h>
#endif

#if defined (XPM)
#ifdef VMS
#include "xpm.h"
#else
#   include <X11/xpm.h>
#endif
#endif

#ifdef IMCONV
#   include "im.h"
#   include "sdsc.h"
#endif

#define MAXANIMATIONSPEED 20

extern int captive;
extern Atom _XA_WM_WORKSPACESLIST;

static Image *LoadBitmapImage ();
static Image *GetBitmapImage  ();
#ifndef VMS
static Image *LoadXwdImage    ();
static Image *GetXwdImage     ();
#endif
#ifdef XPM
static Image *LoadXpmImage    ();
static Image *GetXpmImage     ();
#endif
#ifdef IMCONV
static Image *GetImconvImage  ();
#endif
static Pixmap CreateXLogoPixmap(), CreateResizePixmap();
static Pixmap CreateQuestionPixmap(), CreateMenuPixmap();
static Pixmap CreateDotPixmap ();
static Image  *Create3DMenuImage ();
static Image  *Create3DDotImage ();
static Image  *Create3DResizeImage ();
static Image  *Create3DZoomImage ();
static Image  *Create3DBarImage ();
static Image  *Create3DResizeAnimation ();

extern FILE *errorlog;

void FreeImage ();

static int    reportfilenotfound = 1;
static Colormap AlternateCmap = None;

int  HotX, HotY;

int  AnimationSpeed   = 3;
Bool AnimationActive  = False;
Bool MaybeAnimate     = True;
#ifdef USE_SIGNALS
   Bool AnimationPending = False;
#else
   static unsigned long InitialTime;
   static long NextTime;
   struct timeval AnimateTimeout;
#endif /* USE_SIGNALS */

/***********************************************************************
 *
 *  Procedure:
 *	MoveOutline - move a window outline
 *
 *  Inputs:
 *	root	    - the window we are outlining
 *	x	    - upper left x coordinate
 *	y	    - upper left y coordinate
 *	width	    - the width of the rectangle
 *	height	    - the height of the rectangle
 *      bw          - the border width of the frame
 *      th          - title height
 *
 ***********************************************************************
 */

/* ARGSUSED */
void MoveOutline(root, x, y, width, height, bw, th)
    Window root;
    int x, y, width, height, bw, th;
{
    static int	lastx = 0;
    static int	lasty = 0;
    static int	lastWidth = 0;
    static int	lastHeight = 0;
    static int	lastBW = 0;
    static int	lastTH = 0;
    int		xl, xr, yt, yb, xinnerl, xinnerr, yinnert, yinnerb;
    int		xthird, ythird;
    XSegment	outline[18];
    register XSegment	*r;

    if (x == lastx && y == lasty && width == lastWidth && height == lastHeight
	&& lastBW == bw && th == lastTH)
	return;
    
    r = outline;

#define DRAWIT() \
    if (lastWidth || lastHeight)			\
    {							\
	xl = lastx;					\
	xr = lastx + lastWidth - 1;			\
	yt = lasty;					\
	yb = lasty + lastHeight - 1;			\
	xinnerl = xl + lastBW;				\
	xinnerr = xr - lastBW;				\
	yinnert = yt + lastTH + lastBW;			\
	yinnerb = yb - lastBW;				\
	xthird = (xinnerr - xinnerl) / 3;		\
	ythird = (yinnerb - yinnert) / 3;		\
							\
	r->x1 = xl;					\
	r->y1 = yt;					\
	r->x2 = xr;					\
	r->y2 = yt;					\
	r++;						\
							\
	r->x1 = xl;					\
	r->y1 = yb;					\
	r->x2 = xr;					\
	r->y2 = yb;					\
	r++;						\
							\
	r->x1 = xl;					\
	r->y1 = yt;					\
	r->x2 = xl;					\
	r->y2 = yb;					\
	r++;						\
							\
	r->x1 = xr;					\
	r->y1 = yt;					\
	r->x2 = xr;					\
	r->y2 = yb;					\
	r++;						\
							\
	r->x1 = xinnerl + xthird;			\
	r->y1 = yinnert;				\
	r->x2 = r->x1;					\
	r->y2 = yinnerb;				\
	r++;						\
							\
	r->x1 = xinnerl + (2 * xthird);			\
	r->y1 = yinnert;				\
	r->x2 = r->x1;					\
	r->y2 = yinnerb;				\
	r++;						\
							\
	r->x1 = xinnerl;				\
	r->y1 = yinnert + ythird;			\
	r->x2 = xinnerr;				\
	r->y2 = r->y1;					\
	r++;						\
							\
	r->x1 = xinnerl;				\
	r->y1 = yinnert + (2 * ythird);			\
	r->x2 = xinnerr;				\
	r->y2 = r->y1;					\
	r++;						\
							\
	if (lastTH != 0) {				\
	    r->x1 = xl;					\
	    r->y1 = yt + lastTH;			\
	    r->x2 = xr;					\
	    r->y2 = r->y1;				\
	    r++;					\
	}						\
    }

    /* undraw the old one, if any */
    DRAWIT ();

    lastx = x;
    lasty = y;
    lastWidth = width;
    lastHeight = height;
    lastBW = bw;
    lastTH = th;

    /* draw the new one, if any */
    DRAWIT ();

#undef DRAWIT


    if (r != outline)
    {
	XDrawSegments(dpy, root, Scr->DrawGC, outline, r - outline);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	Zoom - zoom in or out of an icon
 *
 *  Inputs:
 *	wf	- window to zoom from
 *	wt	- window to zoom to
 *
 ***********************************************************************
 */

void
Zoom(wf, wt)
    Window wf, wt;
{
    int fx, fy, tx, ty;			/* from, to */
    unsigned int fw, fh, tw, th;	/* from, to */
    long dx, dy, dw, dh;
    long z;
    int j;

    if (Scr->SmartIconify || !Scr->DoZoom || Scr->ZoomCount < 1) return;

    if (wf == None || wt == None) return;

    XGetGeometry (dpy, wf, &JunkRoot, &fx, &fy, &fw, &fh, &JunkBW, &JunkDepth);
    XGetGeometry (dpy, wt, &JunkRoot, &tx, &ty, &tw, &th, &JunkBW, &JunkDepth);

    dx = (long) tx - (long) fx;	/* going from -> to */
    dy = (long) ty - (long) fy;	/* going from -> to */
    dw = (long) tw - (long) fw;	/* going from -> to */
    dh = (long) th - (long) fh;	/* going from -> to */
    z = (long) (Scr->ZoomCount + 1);

    for (j = 0; j < 2; j++) {
	long i;

	XDrawRectangle (dpy, Scr->Root, Scr->DrawGC, fx, fy, fw, fh);
	for (i = 1; i < z; i++) {
	    int x = fx + (int) ((dx * i) / z);
	    int y = fy + (int) ((dy * i) / z);
	    unsigned width = (unsigned) (((long) fw) + (dw * i) / z);
	    unsigned height = (unsigned) (((long) fh) + (dh * i) / z);
	
	    XDrawRectangle (dpy, Scr->Root, Scr->DrawGC,
			    x, y, width, height);
	}
	XDrawRectangle (dpy, Scr->Root, Scr->DrawGC, tx, ty, tw, th);
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	ExpandFilename - expand the tilde character to HOME
 *		if it is the first character of the filename
 *
 *  Returned Value:
 *	a pointer to the new name
 *
 *  Inputs:
 *	name	- the filename to expand
 *
 ***********************************************************************
 */

char *
ExpandFilename(name)
char *name;
{
    char *newname;

    if (name[0] != '~') return name;

#ifdef VMS
    newname = (char *) malloc (HomeLen + strlen(name) + 1);
    if (!newname) {
        fprintf (stderr, 
 		 "%s:  unable to allocate %d bytes to expand filename %s%s\n",
 		 ProgramName, HomeLen + strlen(name) + 1, Home, &name[1]);
    } else {
        (void) sprintf (newname, "%s%s", Home, &name[1]);
    }
#else
    newname = (char *) malloc (HomeLen + strlen(name) + 2);
    if (!newname) {
	fprintf (stderr, 
		 "%s:  unable to allocate %d bytes to expand filename %s/%s\n",
		 ProgramName, HomeLen + strlen(name) + 2, Home, &name[1]);
    } else {
	(void) sprintf (newname, "%s/%s", Home, &name[1]);
    }
#endif

    return newname;
}

char *ExpandPixmapPath (name)
char *name;
{
    char    *ret;

    ret = NULL;
#ifdef VMS
    if (name[0] == '~') {
	ret = (char *) malloc (HomeLen + strlen (name) + 1);
	sprintf (ret, "%s%s", Home, &name[1]);
    }
    if (name[0] == '/') {
	ret = (char *) malloc (strlen (name));
	sprintf (ret, "%s", &name[1]);
    }
    else
    if (Scr->PixmapDirectory) {
        ret = (char *) malloc (strlen (Scr->PixmapDirectory) + strlen (name) + 1);
	sprintf (ret, "%s%s", Scr->PixmapDirectory, name);
    }
#else
    if (name[0] == '~') {
	ret = (char *) malloc (HomeLen + strlen (name) + 2);
	sprintf (ret, "%s/%s", Home, &name[1]);
    }
    else
    if (name[0] == '/') {
	ret = (char *) malloc (strlen (name) + 1);
	strcpy (ret, name);
    }
    else
    if (Scr->PixmapDirectory) {
	ret = (char *) malloc (strlen (Scr->PixmapDirectory) + strlen (name) + 2);
	sprintf (ret, "%s/%s", Scr->PixmapDirectory, name);
    }
#endif
    return (ret);
}

/***********************************************************************
 *
 *  Procedure:
 *	GetUnknownIcon - read in the bitmap file for the unknown icon
 *
 *  Inputs:
 *	name - the filename to read
 *
 ***********************************************************************
 */

void
GetUnknownIcon(name)
char *name;
{
    Scr->UnknownImage = GetImage (name, Scr->IconC);
}

/***********************************************************************
 *
 *  Procedure:
 *	FindBitmap - read in a bitmap file and return size
 *
 *  Returned Value:
 *	the pixmap associated with the bitmap
 *      widthp	- pointer to width of bitmap
 *      heightp	- pointer to height of bitmap
 *
 *  Inputs:
 *	name	- the filename to read
 *
 ***********************************************************************
 */

Pixmap FindBitmap (name, widthp, heightp)
    char *name;
    unsigned int *widthp, *heightp;
{
    char *bigname;
    Pixmap pm;

    if (!name) return None;

    /*
     * Names of the form :name refer to hardcoded images that are scaled to
     * look nice in title buttons.  Eventually, it would be nice to put in a
     * menu symbol as well....
     */
    if (name[0] == ':') {
	int i;
	static struct {
	    char *name;
	    Pixmap (*proc)();
	} pmtab[] = {
	    { TBPM_DOT,		CreateDotPixmap },
	    { TBPM_ICONIFY,	CreateDotPixmap },
	    { TBPM_RESIZE,	CreateResizePixmap },
	    { TBPM_XLOGO,	CreateXLogoPixmap },
	    { TBPM_DELETE,	CreateXLogoPixmap },
	    { TBPM_MENU,	CreateMenuPixmap },
	    { TBPM_QUESTION,	CreateQuestionPixmap },
	};
	
	for (i = 0; i < (sizeof pmtab)/(sizeof pmtab[0]); i++) {
	    if (XmuCompareISOLatin1 (pmtab[i].name, name) == 0)
	      return (*pmtab[i].proc) (widthp, heightp);
	}
	fprintf (stderr, "%s:  no such built-in bitmap \"%s\"\n",
		 ProgramName, name);
	return None;
    }

    /*
     * Generate a full pathname if any special prefix characters (such as ~)
     * are used.  If the bigname is different from name, bigname will need to
     * be freed.
     */
    bigname = ExpandFilename (name);
    if (!bigname) return None;

    /*
     * look along bitmapFilePath resource same as toolkit clients
     */
    pm = XmuLocateBitmapFile (ScreenOfDisplay(dpy, Scr->screen), bigname, NULL,
			      0, (int *)widthp, (int *)heightp, &HotX, &HotY);
    if (pm == None && Scr->IconDirectory && bigname[0] != '/') {
	if (bigname != name) free (bigname);
	/*
	 * Attempt to find icon in old IconDirectory (now obsolete)
	 */
#ifdef VMS
	bigname = (char *) malloc (strlen(name) + strlen(Scr->IconDirectory) + 1);
	if (!bigname) {
	    fprintf (stderr,
		"%s:  unable to allocate memory for \"%s%s\"\n",
		ProgramName, Scr->IconDirectory, name);
	    return None;
	}
	(void) sprintf (bigname, "%s%s", Scr->IconDirectory, name);
#else
	bigname = (char *) malloc (strlen(name) + strlen(Scr->IconDirectory) + 2);
	if (!bigname) {
	    fprintf (stderr,
		     "%s:  unable to allocate memory for \"%s/%s\"\n",
		     ProgramName, Scr->IconDirectory, name);
	    return None;
	}
	(void) sprintf (bigname, "%s/%s", Scr->IconDirectory, name);
#endif
	if (XReadBitmapFile (dpy, Scr->Root, bigname, widthp, heightp, &pm,
			     &HotX, &HotY) != BitmapSuccess) {
	    pm = None;
	}
    }
    if (bigname != name) free (bigname);
    if ((pm == None) && reportfilenotfound) {
	fprintf (stderr, "%s:  unable to find bitmap \"%s\"\n", ProgramName, name);
    }

    return pm;
}

Pixmap GetBitmap (name)
    char *name;
{
    return FindBitmap (name, &JunkWidth, &JunkHeight);
}

static Image *LoadBitmapImage (name, cp)
char  *name;
ColorPair cp;
{
    Image	*image;
    Pixmap	bm;
    int		width, height;
    XGCValues	gcvalues;

    if (Scr->rootGC == (GC) 0) Scr->rootGC = XCreateGC (dpy, Scr->Root, 0, &gcvalues);
    bm = FindBitmap (name, &width, &height);
    if (bm == None) return (None);

    image = (Image*) malloc (sizeof (struct _Image));
    image->pixmap = XCreatePixmap (dpy, Scr->Root, width, height, Scr->d_depth);
    gcvalues.background = cp.back;
    gcvalues.foreground = cp.fore;
    XChangeGC   (dpy, Scr->rootGC, GCForeground | GCBackground, &gcvalues);
    XCopyPlane  (dpy, bm, image->pixmap, Scr->rootGC, 0, 0, width, height,
		0, 0, (unsigned long) 1);
    XFreePixmap (dpy, bm);
    image->mask   = None;
    image->width  = width;
    image->height = height;
    image->next   = None;
    return (image);
}

static Image *GetBitmapImage (name, cp)
char  *name;
ColorPair cp;
{
    Image	*image, *r, *s;
    Pixmap	bm;
    int		width, height;
    char	path [128], pref [128];
    char	*perc;
    int		i;

    if (! strchr (name, '%')) return (LoadBitmapImage (name, cp));
    s = image = None;
    strcpy (pref, name);
    perc  = strchr (pref, '%');
    *perc = '\0';
    reportfilenotfound = 0;
    for (i = 1;; i++) {
	sprintf (path, "%s%d%s", pref, i, perc + 1);
	r = LoadBitmapImage (path, cp);
	if (r == None) break;
	r->next = None;
	if (image == None) s = image = r;
	else {
	    s->next = r;
	    s = r;
	}
    }
    reportfilenotfound = 1;
    if (s != None) s->next = image;
    if (image == None) {
	fprintf (stderr, "Cannot open any %s bitmap file\n", name);
    }
    return (image);
}

#ifdef XPM
static int reportxpmerror = 1;

static Image *LoadXpmImage (name)
char *name;
{
    char	*fullname;
    Image	*image;
    int		status;
    XpmAttributes attributes;

    fullname = ExpandPixmapPath (name);
    if (! fullname) return (None);

    image = (Image*) malloc (sizeof (struct _Image));
    if (image == None) return (None);

    attributes.valuemask  = 0;
    attributes.valuemask |= XpmSize;
    attributes.valuemask |= XpmReturnPixels;
    attributes.valuemask |= XpmColormap;
    attributes.valuemask |= XpmDepth;
    attributes.valuemask |= XpmVisual;

    attributes.colormap = AlternateCmap ? AlternateCmap : DefaultColormap (dpy, Scr->screen);
    attributes.depth    = DefaultDepth  (dpy, Scr->screen);
    attributes.visual   = DefaultVisual (dpy, Scr->screen);
    status = XpmReadFileToPixmap(dpy, Scr->Root, fullname,
				 &(image->pixmap), &(image->mask), &attributes);
    free (fullname);
    switch (status) {
	case XpmSuccess:
	    break;

	case XpmColorError:
	    if (reportxpmerror)
		fprintf (stderr, "Could not parse or alloc requested color : %s\n", fullname);
	    free (image);
	    return (None);

	case XpmOpenFailed:
	    if (reportxpmerror && reportfilenotfound)
		fprintf (stderr, "Cannot open XPM file : %s\n", fullname);
	    free (image);
	    return (None);

	case XpmFileInvalid:
	    fprintf (stderr, "invalid XPM file : %s\n", fullname);
	    free (image);
	    return (None);

	case XpmNoMemory:
	    if (reportxpmerror)
		fprintf (stderr, "Not enough memory for XPM file : %s\n", fullname);
	    free (image);
	    return (None);

	case XpmColorFailed:
	    if (reportxpmerror)
		fprintf (stderr, "Color not found in : %s\n", fullname);
	    free (image);
	    return (None);

	default :
	    fprintf (stderr, "Unknown error in : %s\n", fullname);
	    free (image);
	    return (None);
    }
    image->width  = attributes.width;
    image->height = attributes.height;
    image->next   = None;
    return (image);
}

static Image *GetXpmImage (name)
char *name;
{
    char    path [128], pref [128];
    Image   *image, *r, *s;
    char    *perc;
    int     i;

    if (! strchr (name, '%')) return (LoadXpmImage (name));
    s = image = None;
    strcpy (pref, name);
    perc  = strchr (pref, '%');
    *perc = '\0';
    reportfilenotfound = 0;
    for (i = 1;; i++) {
	sprintf (path, "%s%d%s", pref, i, perc + 1);
	r = LoadXpmImage (path);
	if (r == None) break;
	r->next = None;
	if (image == None) s = image = r;
	else {
	    s->next = r;
	    s = r;
	}
    }
    reportfilenotfound = 1;
    if (s != None) s->next = image;
    if (image == None) {
	fprintf (stderr, "Cannot open any %s XPM file\n", name);
    }
    return (image);
}

#endif

void MaskScreen (file)
char *file;
{
    unsigned long valuemask;
    XSetWindowAttributes attributes;
    XEvent event;
    Cursor waitcursor;
    int x, y;
    ColorPair WelcomeCp;
    XColor black;

    NewFontCursor (&waitcursor, "watch");

    valuemask = (CWBackingStore | CWSaveUnder | CWBackPixel |
		 CWOverrideRedirect | CWEventMask | CWCursor);
    attributes.backing_store	 = NotUseful;
    attributes.save_under	 = False;
    attributes.override_redirect = True;
    attributes.event_mask	 = ExposureMask;
    attributes.cursor		 = waitcursor;
    attributes.background_pixel	 = BlackPixel (dpy, Scr->screen);
    Scr->WindowMask = XCreateWindow (dpy, Scr->Root, 0, 0,
			(unsigned int) Scr->MyDisplayWidth,
			(unsigned int) Scr->MyDisplayHeight,
			(unsigned int) 0,
			CopyFromParent, (unsigned int) CopyFromParent,
			(Visual *) CopyFromParent, valuemask,
			&attributes);
    XMapWindow (dpy, Scr->WindowMask);
    XMaskEvent (dpy, ExposureMask, &event);

    if (Scr->Monochrome != COLOR) return;

    WelcomeCp.fore = Scr->Black;
    WelcomeCp.back = Scr->White;
    Scr->WelcomeCmap  = XCreateColormap (dpy, Scr->WindowMask,
				DefaultVisual (dpy, Scr->screen), AllocNone);
    if (! Scr->WelcomeCmap) return;
    black.red   = 0;
    black.green = 0;
    black.blue  = 0;
    XAllocColor (dpy, Scr->WelcomeCmap, &black);

    reportfilenotfound = 0;
    AlternateCmap = Scr->WelcomeCmap;
    if (! file) {
	Scr->WelcomeImage  = GetImage ("xwd:welcome.xwd", WelcomeCp);
#ifdef XPM
	if (Scr->WelcomeImage == None)
		Scr->WelcomeImage  = GetImage ("xpm:welcome.xpm", WelcomeCp);
#endif
    }
    else {
	Scr->WelcomeImage  = GetImage (file, WelcomeCp);
    }
    AlternateCmap = None;
    reportfilenotfound = 1;
    if (Scr->WelcomeImage == None) return;

    if (captive) XSetWindowColormap (dpy, Scr->Root, Scr->WelcomeCmap);
    else XInstallColormap (dpy, Scr->WelcomeCmap);

    Scr->WelcomeGC = XCreateGC (dpy, Scr->WindowMask, 0, NULL);
    x = (Scr->MyDisplayWidth  -  Scr->WelcomeImage->width) / 2;
    y = (Scr->MyDisplayHeight - Scr->WelcomeImage->height) / 2;

    XSetWindowBackground (dpy, Scr->WindowMask, black.pixel);
    XClearWindow (dpy, Scr->WindowMask);
    XCopyArea (dpy, Scr->WelcomeImage->pixmap, Scr->WindowMask, Scr->WelcomeGC, 0, 0,
		Scr->WelcomeImage->width, Scr->WelcomeImage->height, x, y);
}

UnmaskScreen () {
#ifdef VMS
    float  timeout;
#else
    struct timeval	timeout;
#endif
    Pixel		stdpixels [256];
    Colormap		stdcmap = DefaultColormap (dpy, Scr->screen);
    Colormap		cmap;
    XColor		colors [256], stdcolors [256];
    int			i, j, usec;
    Status		status;
    unsigned long	planemask;

#ifdef VMS
    timeout = 0.017;
#else
    usec = 17000;
    timeout.tv_usec = usec % (unsigned long) 1000000;
    timeout.tv_sec  = usec / (unsigned long) 1000000;
#endif
    for (i = 0; i < 256; i++) stdpixels [i] = i;

    if (Scr->WelcomeImage) {
	Pixel pixels [256];

	cmap = Scr->WelcomeCmap;
	for (i = 0; i < 256; i++) {
	    pixels [i] = i;
	    colors [i].pixel = i;
	}
	XQueryColors (dpy, cmap, colors, 256);
	XFreeColors  (dpy, cmap, pixels, 256, 0L);
	XFreeColors  (dpy, cmap, pixels, 256, 0L); /* Ah Ah */

	status = XAllocColorCells (dpy, cmap, False, &planemask, 0, stdpixels, 256);
	for (i = 0; i < 256; i++) {
	    colors [i].pixel = i;
	    colors [i].flags = DoRed | DoGreen | DoBlue;
	    stdcolors [i].red   = colors [i].red;
	    stdcolors [i].green = colors [i].green;
	    stdcolors [i].blue  = colors [i].blue;
	}
	for (i = 0; i < 128; i++) {
	    for (j = 0; j < 256; j++) {
		colors [j].red   = stdcolors [j].red   * ((127.0 - i) / 128.0);
		colors [j].green = stdcolors [j].green * ((127.0 - i) / 128.0);
		colors [j].blue  = stdcolors [j].blue  * ((127.0 - i) / 128.0);
	    }
	    XStoreColors (dpy, cmap, colors, 256);
#ifdef VMS
	    lib$wait(&timeout);
#else
	    select (0, (void *) 0, (void *) 0, (void *) 0, &timeout);
#endif
	}
	XFreeColors   (dpy, cmap, pixels, 256, 0L);
	XFreeGC       (dpy, Scr->WelcomeGC);
	FreeImage     (Scr->WelcomeImage);
    }
    if (Scr->Monochrome != COLOR) goto fin;

    cmap = XCreateColormap (dpy, Scr->Root, DefaultVisual (dpy, Scr->screen), AllocNone);
    if (! cmap) goto fin;
    XAllocColorCells (dpy, cmap, False, &planemask, 0, stdpixels, 256);
    for (i = 0; i < 256; i++) {
	colors [i].pixel = i;
	colors [i].red   = 0;
	colors [i].green = 0;
	colors [i].blue  = 0;
	colors [i].flags = DoRed | DoGreen | DoBlue;
    }
    XStoreColors (dpy, cmap, colors, 256);
    if (captive) XSetWindowColormap (dpy, Scr->Root, cmap);
    else XInstallColormap (dpy, cmap);

    XUnmapWindow (dpy, Scr->WindowMask);
    XClearWindow (dpy, Scr->Root);
    XSync (dpy, 0);
    PaintAllDecoration ();

    for (i = 0; i < 256; i++) stdcolors [i].pixel = i;
    XQueryColors (dpy, stdcmap, stdcolors, 256);
    for (i = 0; i < 128; i++) {
	for (j = 0; j < 256; j++) {
	    colors [j].red   = stdcolors [j].red   * (i / 127.0);
	    colors [j].green = stdcolors [j].green * (i / 127.0);
	    colors [j].blue  = stdcolors [j].blue  * (i / 127.0);
	}
	XStoreColors (dpy, cmap, colors, 256);
#ifdef VMS
        lib$wait(&timeout);
#else
	select (0, (void *) 0, (void *) 0, (void *) 0, &timeout);
#endif
    }
    if (captive) XSetWindowColormap (dpy, Scr->Root, DefaultColormap (dpy, Scr->screen));
    else XInstallColormap (dpy, DefaultColormap (dpy, Scr->screen));
    XFreeColormap (dpy, cmap);

fin:
    if (Scr->WelcomeCmap) XFreeColormap (dpy, Scr->WelcomeCmap);
    XDestroyWindow (dpy, Scr->WindowMask);
    Scr->WindowMask = (Window) 0;
}

#ifdef VMS

/* use the VMS system services to request the timer to issue an AST */
void AnimateHandler ();

unsigned int tv[2];
int status;
static unsigned long timefi;
/* unsigned long timefe = 17; */
unsigned long timefe;

#define TIMID 12L

void StartAnimation () {
    if (AnimationSpeed > MAXANIMATIONSPEED) AnimationSpeed = MAXANIMATIONSPEED;
    if (AnimationSpeed <= 0) return;
    if (AnimationActive) return;

    if (!timefi) lib$get_ef(&timefi);
    if (!timefe) lib$get_ef(&timefe);

    tv[1] = 0xFFFFFFFF;                   /* quadword negative for relative */
    tv[0] = -(10000000 / AnimationSpeed); /* time. In units of 100ns. */
    sys$clref(timefe);
    status = sys$setimr (timefi, &tv, AnimateHandler, TIMID );
    if (status != SS$_NORMAL) lib$signal(status);
    AnimationActive = True;
}

void StopAnimation () {
    if (AnimationSpeed <= 0) return;
    if (! AnimationActive) return;
    AnimationActive = False;

    status = sys$cantim(TIMID, PSL$C_USER);
    if (status != SS$_NORMAL) lib$signal(status);
}

void SetAnimationSpeed (speed)
int speed;
{
    AnimationSpeed = speed;
    if (AnimationSpeed > MAXANIMATIONSPEED) AnimationSpeed = MAXANIMATIONSPEED;
}

void ModifyAnimationSpeed (incr)
int incr;
{
    if ((AnimationSpeed + incr) < 0) return;
    if ((AnimationSpeed + incr) == 0) {
	if (AnimationActive) StopAnimation ();
	AnimationSpeed = 0;
	return;
    }
    AnimationSpeed += incr;

    status = sys$cantim(TIMID, PSL$C_USER);
    if (status != SS$_NORMAL) lib$signal(status);

    tv[1] = 0xFFFFFFFF;
    tv[0] = -(10000000 / AnimationSpeed);

    sys$clref(timefe);
    status = sys$setimr (timefi, &tv, AnimateHandler, TIMID);
    if (status != SS$_NORMAL) lib$signal(status);

    AnimationActive = True;
}

void AnimateHandler () {
    AnimationPending = True;

    sys$setef(timefe);
    status = sys$setimr (timefi, &tv, AnimateHandler, TIMID);
    if (status != SS$_NORMAL) lib$signal(status);
}
#else /* VMS */

#ifdef USE_SIGNALS
SIGNAL_T AnimateHandler ();
#endif

#ifndef USE_SIGNALS
void TryToAnimate () {
    struct timeval  tp;
    struct timezone tzp;
    long   theTime;

    gettimeofday (&tp, &tzp);
    theTime = ((tp.tv_sec - InitialTime) * 1000000) + tp.tv_usec;
    if (theTime < NextTime) return;
    Animate ();
    NextTime = theTime + (1000000 / AnimationSpeed);
}
#endif /* USE_SIGNALS */

void StartAnimation () {
#ifdef USE_SIGNALS
    struct itimerval tv;
#else
    struct timeval  tp;
    struct timezone tzp;
#endif

    if (AnimationSpeed > MAXANIMATIONSPEED) AnimationSpeed = MAXANIMATIONSPEED;
    if (AnimationSpeed <= 0) AnimationSpeed = 0;
    if (AnimationActive) return;
#ifdef USE_SIGNALS
    if (AnimationSpeed == 0) return;
    signal (SIGALRM, AnimateHandler);
    if (AnimationSpeed == 1) {
	tv.it_interval.tv_sec  = 1;
	tv.it_interval.tv_usec = 0;
	tv.it_value.tv_sec     = 1;
	tv.it_value.tv_usec    = 0;
    }
    else {
	tv.it_interval.tv_sec  = 0;
	tv.it_interval.tv_usec = 1000000 / AnimationSpeed;
	tv.it_value.tv_sec     = 0;
	tv.it_value.tv_usec    = 1000000 / AnimationSpeed;
    }
    setitimer (ITIMER_REAL, &tv, (struct itimerval*) NULL);
#else /* USE_SIGNALS */
    gettimeofday (&tp, &tzp);
    InitialTime = tp.tv_sec;
    NextTime    = 0;
    switch (AnimationSpeed) {
	case 0 :
	    return;
	case 1 :
	    AnimateTimeout.tv_sec  = 1;
	    AnimateTimeout.tv_usec = 0;
	    break;
	default :
	    AnimateTimeout.tv_sec  = 0;
	    AnimateTimeout.tv_usec = 1000000 / AnimationSpeed;
    }
#endif /* USE_SIGNALS */
    AnimationActive = True;
}

void StopAnimation () {
#ifdef USE_SIGNALS
    struct itimerval tv;

    if (AnimationSpeed <= 0) return;
    if (! AnimationActive) return;
    signal (SIGALRM, SIG_IGN);

    tv.it_value.tv_sec     = 0;
    tv.it_value.tv_usec    = 0;
    setitimer (ITIMER_REAL, &tv, (struct itimerval*) NULL);
#endif
    AnimationActive = False;
}

void SetAnimationSpeed (speed)
int speed;
{
    AnimationSpeed = speed;
    if (AnimationSpeed > MAXANIMATIONSPEED) AnimationSpeed = MAXANIMATIONSPEED;
}

void ModifyAnimationSpeed (incr)
int incr;
{
#ifdef USE_SIGNALS
    struct itimerval tv;
#endif

    if ((AnimationSpeed + incr) < 0) return;
    if ((AnimationSpeed + incr) == 0) {
	if (AnimationActive) StopAnimation ();
	AnimationSpeed = 0;
	return;
    }
    AnimationSpeed += incr;
    if (AnimationSpeed > MAXANIMATIONSPEED) AnimationSpeed = MAXANIMATIONSPEED;

#ifdef USE_SIGNALS
    signal (SIGALRM, AnimateHandler);
    if (AnimationSpeed == 1) {
	tv.it_interval.tv_sec  = 1;
	tv.it_interval.tv_usec = 0;
	tv.it_value.tv_sec     = 1;
	tv.it_value.tv_usec    = 0;
    }
    else {
	tv.it_interval.tv_sec  = 0;
	tv.it_interval.tv_usec = 1000000 / AnimationSpeed;
	tv.it_value.tv_sec     = 0;
	tv.it_value.tv_usec    = 1000000 / AnimationSpeed;
    }
    setitimer (ITIMER_REAL, &tv, (struct itimerval*) NULL);
#else /* USE_SIGNALS */
    if (AnimationSpeed == 1) {
	AnimateTimeout.tv_sec  = 1;
	AnimateTimeout.tv_usec = 0;
    }
    else {
	AnimateTimeout.tv_sec  = 0;
	AnimateTimeout.tv_usec = 1000000 / AnimationSpeed;
    }
#endif /* USE_SIGNALS */
    AnimationActive = True;
}

#ifdef USE_SIGNALS
SIGNAL_T AnimateHandler (dummy)
int dummy;
{
    signal (SIGALRM, AnimateHandler);
    AnimationPending = True;
}
#endif
#endif /* VMS */

void Animate () {
    TwmWindow	*t;
    Icon	*icon;
    Image	*image;
    int		scrnum;
    ScreenInfo	*scr;
    int		i;
    TBWindow	*tbw;
    int		nb;

    if (AnimationSpeed == 0) return;
#ifdef USE_SIGNALS
    AnimationPending = False;
#endif

    MaybeAnimate = False;
    for (scrnum = 0; scrnum < NumScreens; scrnum++) {
	if ((scr = ScreenList [scrnum]) == NULL) continue;

	for (t = scr->TwmRoot.next; t != NULL; t = t->next) {
	    if (! OCCUPY (t, scr->workSpaceMgr.activeWSPC)) continue;
	    if (t->icon_on && t->icon && t->icon->bm_w && t->icon->image &&
		t->icon->image->next) {
		AnimateIcons (scr, t->icon);
		MaybeAnimate = True;
	    }
	    else
	    if (t->mapped && t->titlebuttons) {
		nb = scr->TBInfo.nleft + scr->TBInfo.nright;
		for (i = 0, tbw = t->titlebuttons; i < nb; i++, tbw++) {
		    if (tbw->image && tbw->image->next) {
			AnimateButton (tbw);
			MaybeAnimate = True;
		    }
		}
	    }
	}
	if (scr->Focus) {
	    t = scr->Focus;
	    if (t->mapped && t->titlehighlight && t->title_height &&
		t->HiliteImage && t->HiliteImage->next) {
		AnimateHighlight (t);
		MaybeAnimate = True;
	    }
	}
    }
    MaybeAnimate |= AnimateRoot ();
    XFlush (dpy);
    return;
}

void InsertRGBColormap (a, maps, nmaps, replace)
    Atom a;
    XStandardColormap *maps;
    int nmaps;
    Bool replace;
{
    StdCmap *sc = NULL;

    if (replace) {			/* locate existing entry */
	for (sc = Scr->StdCmapInfo.head; sc; sc = sc->next) {
	    if (sc->atom == a) break;
	}
    }

    if (!sc) {				/* no existing, allocate new */
	sc = (StdCmap *) malloc (sizeof (StdCmap));
	if (!sc) {
	    fprintf (stderr, "%s:  unable to allocate %d bytes for StdCmap\n",
		     ProgramName, sizeof (StdCmap));
	    return;
	}
    }

    if (replace) {			/* just update contents */
	if (sc->maps) XFree ((char *) maps);
	if (sc == Scr->StdCmapInfo.mru) Scr->StdCmapInfo.mru = NULL;
    } else {				/* else appending */
	sc->next = NULL;
	sc->atom = a;
	if (Scr->StdCmapInfo.tail) {
	    Scr->StdCmapInfo.tail->next = sc;
	} else {
	    Scr->StdCmapInfo.head = sc;
	}
	Scr->StdCmapInfo.tail = sc;
    }
    sc->nmaps = nmaps;
    sc->maps = maps;

    return;
}

void RemoveRGBColormap (a)
    Atom a;
{
    StdCmap *sc, *prev;

    prev = NULL;
    for (sc = Scr->StdCmapInfo.head; sc; sc = sc->next) {  
	if (sc->atom == a) break;
	prev = sc;
    }
    if (sc) {				/* found one */
	if (sc->maps) XFree ((char *) sc->maps);
	if (prev) prev->next = sc->next;
	if (Scr->StdCmapInfo.head == sc) Scr->StdCmapInfo.head = sc->next;
	if (Scr->StdCmapInfo.tail == sc) Scr->StdCmapInfo.tail = prev;
	if (Scr->StdCmapInfo.mru == sc) Scr->StdCmapInfo.mru = NULL;
    }
    return;
}

void LocateStandardColormaps()
{
    Atom *atoms;
    int natoms;
    int i;

    atoms = XListProperties (dpy, Scr->Root, &natoms);
    for (i = 0; i < natoms; i++) {
	XStandardColormap *maps = NULL;
	int nmaps;

	if (XGetRGBColormaps (dpy, Scr->Root, &maps, &nmaps, atoms[i])) {
	    /* if got one, then append to current list */
	    InsertRGBColormap (atoms[i], maps, nmaps, False);
	}
    }
    if (atoms) XFree ((char *) atoms);
    return;
}

void GetColor(kind, what, name)
int kind;
Pixel *what;
char *name;
{
    XColor color, junkcolor;
    Status stat = 0;
    Colormap cmap = Scr->TwmRoot.cmaps.cwins[0]->colormap->c;

#ifndef TOM
    if (!Scr->FirstTime)
	return;
#endif

    if (Scr->Monochrome != kind)
	return;

    if (! XParseColor (dpy, cmap, name, &color)) {
	fprintf (stderr, "%s:  invalid color name \"%s\"\n", ProgramName, name);
	return;
    }
    if (! XAllocColor (dpy, cmap, &color))
    {
	/* if we could not allocate the color, let's see if this is a
	 * standard colormap
	 */
	XStandardColormap *stdcmap = NULL;

	if (! XParseColor (dpy, cmap, name, &color)) {
	    fprintf (stderr, "%s:  invalid color name \"%s\"\n", ProgramName, name);
	    return;
	}

	/*
	 * look through the list of standard colormaps (check cache first)
	 */
	if (Scr->StdCmapInfo.mru && Scr->StdCmapInfo.mru->maps &&
	    (Scr->StdCmapInfo.mru->maps[Scr->StdCmapInfo.mruindex].colormap ==
	     cmap)) {
	    stdcmap = &(Scr->StdCmapInfo.mru->maps[Scr->StdCmapInfo.mruindex]);
	} else {
	    StdCmap *sc;

	    for (sc = Scr->StdCmapInfo.head; sc; sc = sc->next) {
		int i;

		for (i = 0; i < sc->nmaps; i++) {
		    if (sc->maps[i].colormap == cmap) {
			Scr->StdCmapInfo.mru = sc;
			Scr->StdCmapInfo.mruindex = i;
			stdcmap = &(sc->maps[i]);
			goto gotit;
		    }
		}
	    }
	}

      gotit:
	if (stdcmap) {
            color.pixel = (stdcmap->base_pixel +
			   ((Pixel)(((float)color.red / 65535.0) *
				    stdcmap->red_max + 0.5) *
			    stdcmap->red_mult) +
			   ((Pixel)(((float)color.green /65535.0) *
				    stdcmap->green_max + 0.5) *
			    stdcmap->green_mult) +
			   ((Pixel)(((float)color.blue  / 65535.0) *
				    stdcmap->blue_max + 0.5) *
			    stdcmap->blue_mult));
        } else {
	    fprintf (stderr, "%s:  unable to allocate color \"%s\"\n", 
		     ProgramName, name);
	    return;
	}
    }

    *what = color.pixel;
    return;
}

void GetShadeColors (cp)
ColorPair *cp;
{
    XColor	xcol;
    Colormap	cmap = Scr->TwmRoot.cmaps.cwins[0]->colormap->c;
    int		save;
    float	clearfactor;
    float	darkfactor;
    char	clearcol [32], darkcol [32];

    clearfactor = (float) Scr->ClearShadowContrast / 100.0;
    darkfactor  = (100.0 - (float) Scr->DarkShadowContrast)  / 100.0;
    xcol.pixel = cp->back;
    XQueryColor (dpy, cmap, &xcol);

    sprintf (clearcol, "#%04x%04x%04x",
		xcol.red   + (unsigned short) ((65535 -   xcol.red) * clearfactor),
		xcol.green + (unsigned short) ((65535 - xcol.green) * clearfactor),
		xcol.blue  + (unsigned short) ((65535 -  xcol.blue) * clearfactor));
    sprintf (darkcol,  "#%04x%04x%04x",
		(unsigned short) (xcol.red   * darkfactor),
		(unsigned short) (xcol.green * darkfactor),
		(unsigned short) (xcol.blue  * darkfactor));

    save = Scr->FirstTime;
    Scr->FirstTime = True;
    GetColor (Scr->Monochrome, &cp->shadc, clearcol);
    GetColor (Scr->Monochrome, &cp->shadd,  darkcol);
    Scr->FirstTime = save;
}

void GetFont(font)
MyFont *font;
{
    char *deffontname = "fixed";

    if (font->font != NULL)
	XFreeFont(dpy, font->font);

    if ((font->font = XLoadQueryFont(dpy, font->name)) == NULL)
    {
	if (Scr->DefaultFont.name) {
	    deffontname = Scr->DefaultFont.name;
	}
	if ((font->font = XLoadQueryFont(dpy, deffontname)) == NULL)
	{
	    fprintf (stderr, "%s:  unable to open fonts \"%s\" or \"%s\"\n",
		     ProgramName, font->name, deffontname);
	    exit(1);
	}

    }
    font->height = font->font->ascent + font->font->descent;
    font->y = font->font->ascent;
}


void SetFocusVisualAttributes (tmp_win, focus)
TwmWindow *tmp_win;
Bool focus;
{
    if (! tmp_win) return;

    if (focus && tmp_win->hasfocusvisible) return;
    if (! focus && ! tmp_win->hasfocusvisible) return;
    if (tmp_win->highlight) {
	if (Scr->use3Dborders) {
	    if (focus && ! tmp_win->hasfocusvisible) PaintBorders (tmp_win, True);
	    if (! focus && tmp_win->hasfocusvisible) PaintBorders (tmp_win, False);
	}
	else {
	    if (focus) {
		XSetWindowBorder (dpy, tmp_win->frame, tmp_win->borderC.back);
		if (tmp_win->title_w)
		    XSetWindowBorder (dpy, tmp_win->title_w, tmp_win->borderC.back);
	    } else {
		XSetWindowBorderPixmap (dpy, tmp_win->frame, tmp_win->gray);
		if (tmp_win->title_w)
		    XSetWindowBorderPixmap (dpy, tmp_win->title_w, tmp_win->gray);
	    }
	}
    }
    if (focus) {
	if (tmp_win->hilite_wl) XMapWindow (dpy, tmp_win->hilite_wl);
	if (tmp_win->hilite_wr) XMapWindow (dpy, tmp_win->hilite_wr);
	if (tmp_win->list) ActiveIconManager (tmp_win->list);
	MaybeAnimate = True;
    }
    else {
	if (tmp_win->hilite_wl) XUnmapWindow (dpy, tmp_win->hilite_wl);
	if (tmp_win->hilite_wr) XUnmapWindow (dpy, tmp_win->hilite_wr);
	if (tmp_win->list) NotActiveIconManager (tmp_win->list);
    }
    if (Scr->use3Dtitles && Scr->SunkFocusWindowTitle && tmp_win->title_height) {
	ButtonState bs;

	bs = focus ? on : off;
	Draw3DBorder (tmp_win->title_w, Scr->TBInfo.titlex, 0,
			tmp_win->title_width  - Scr->TBInfo.titlex -
			Scr->TBInfo.rightoff,
			Scr->TitleHeight, 2,
			tmp_win->title, bs, False, False);
    }
    tmp_win->hasfocusvisible = focus;
}

/*
 * SetFocus - separate routine to set focus to make things more understandable
 * and easier to debug
 */
void SetFocus (tmp_win, time)
    TwmWindow *tmp_win;
    Time	time;
{
    Window w = (tmp_win ? tmp_win->w : PointerRoot);

    XSetInputFocus (dpy, w, RevertToPointerRoot, time);
    if (Scr->Focus == tmp_win) return;

    if (Scr->Focus) {
	SetFocusVisualAttributes (Scr->Focus, False);
    }
    if (tmp_win)    {
	SetFocusVisualAttributes (tmp_win, True);
    }
    if (Scr->ClickToFocus) ChangeFocusGrab (tmp_win);
    Scr->Focus = tmp_win;
}

#define MyWindow(win) ((win)->iconmgr || \
		      ((win) == Scr->workSpaceMgr.workspaceWindow.twm_win) || \
		      ((win) == Scr->workSpaceMgr.occupyWindow.twm_win))

void ChangeFocusGrab (tmp_win)
TwmWindow *tmp_win;
{
    if (tmp_win) {
	ClickToFocusUngrab (tmp_win);
    }
    if (Scr->Focus && ! MyWindow (Scr->Focus)) {
	ClickToFocusGrab (Scr->Focus);
    }
}

#ifdef NOPUTENV
/*
 * define our own putenv() if the system doesn't have one.
 * putenv(s): place s (a string of the form "NAME=value") in
 * the environment; replacing any existing NAME.  s is placed in
 * environment, so if you change s, the environment changes (like
 * putenv on a sun).  Binding removed if you putenv something else
 * called NAME.
 */
int
putenv(s)
    char *s;
{
    char *v;
    int varlen, idx;
    extern char **environ;
    char **newenv;
    static int virgin = 1; /* true while "environ" is a virgin */

    v = strchr(s, '=');
    if(v == 0)
	return 0; /* punt if it's not of the right form */
    varlen = (v + 1) - s;

    for (idx = 0; environ[idx] != 0; idx++) {
	if (strncmp(environ[idx], s, varlen) == 0) {
	    if(v[1] != 0) { /* true if there's a value */
		environ[idx] = s;
		return 0;
	    } else {
		do {
		    environ[idx] = environ[idx+1];
		} while(environ[++idx] != 0);
		return 0;
	    }
	}
    }
    
    /* add to environment (unless no value; then just return) */
    if(v[1] == 0)
	return 0;
    if(virgin) {
	register i;

	newenv = (char **) malloc((unsigned) ((idx + 2) * sizeof(char*)));
	if(newenv == 0)
	    return -1;
	for(i = idx-1; i >= 0; --i)
	    newenv[i] = environ[i];
	virgin = 0;     /* you're not a virgin anymore, sweety */
    } else {
	newenv = (char **) realloc((char *) environ,
				   (unsigned) ((idx + 2) * sizeof(char*)));
	if (newenv == 0)
	    return -1;
    }

    environ = newenv;
    environ[idx] = s;
    environ[idx+1] = 0;
    
    return 0;
}
#endif /* NOPUTENV */


static Pixmap CreateXLogoPixmap (widthp, heightp)
    unsigned int *widthp, *heightp;
{
    int h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (h < 0) h = 0;

    *widthp = *heightp = (unsigned int) h;
    if (Scr->tbpm.xlogo == None) {
	GC gc, gcBack;

	Scr->tbpm.xlogo = XCreatePixmap (dpy, Scr->Root, h, h, 1);
	gc = XCreateGC (dpy, Scr->tbpm.xlogo, 0L, NULL);
	XSetForeground (dpy, gc, 0);
	XFillRectangle (dpy, Scr->tbpm.xlogo, gc, 0, 0, h, h);
	XSetForeground (dpy, gc, 1);
	gcBack = XCreateGC (dpy, Scr->tbpm.xlogo, 0L, NULL);
	XSetForeground (dpy, gcBack, 0);

	/*
	 * draw the logo large so that it gets as dense as possible; then white
	 * out the edges so that they look crisp
	 */
	XmuDrawLogo (dpy, Scr->tbpm.xlogo, gc, gcBack, -1, -1, h + 2, h + 2);
	XDrawRectangle (dpy, Scr->tbpm.xlogo, gcBack, 0, 0, h - 1, h - 1);

	/*
	 * done drawing
	 */
	XFreeGC (dpy, gc);
	XFreeGC (dpy, gcBack);
    }
    return Scr->tbpm.xlogo;
}


static Pixmap CreateResizePixmap (widthp, heightp)
    unsigned int *widthp, *heightp;
{
    int h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (h < 1) h = 1;

    *widthp = *heightp = (unsigned int) h;
    if (Scr->tbpm.resize == None) {
	XPoint	points[3];
	GC gc;
	int w;
	int lw;

	/*
	 * create the pixmap
	 */
	Scr->tbpm.resize = XCreatePixmap (dpy, Scr->Root, h, h, 1);
	gc = XCreateGC (dpy, Scr->tbpm.resize, 0L, NULL);
	XSetForeground (dpy, gc, 0);
	XFillRectangle (dpy, Scr->tbpm.resize, gc, 0, 0, h, h);
	XSetForeground (dpy, gc, 1);
	lw = h / 16;
	if (lw == 1)
	    lw = 0;
	XSetLineAttributes (dpy, gc, lw, LineSolid, CapButt, JoinMiter);

	/*
	 * draw the resize button, 
	 */
	w = (h * 2) / 3;
	points[0].x = w;
	points[0].y = 0;
	points[1].x = w;
	points[1].y = w;
	points[2].x = 0;
	points[2].y = w;
	XDrawLines (dpy, Scr->tbpm.resize, gc, points, 3, CoordModeOrigin);
	w = w / 2;
	points[0].x = w;
	points[0].y = 0;
	points[1].x = w;
	points[1].y = w;
	points[2].x = 0;
	points[2].y = w;
	XDrawLines (dpy, Scr->tbpm.resize, gc, points, 3, CoordModeOrigin);

	/*
	 * done drawing
	 */
	XFreeGC(dpy, gc);
    }
    return Scr->tbpm.resize;
}

static Pixmap CreateDotPixmap (widthp, heightp)
    unsigned int *widthp, *heightp;
{
    int h = Scr->TBInfo.width - Scr->TBInfo.border * 2;

    h = h * 3 / 4;
    if (h < 1) h = 1;
    if (!(h & 1))
	h--;
    *widthp = *heightp = (unsigned int) h;
    if (Scr->tbpm.delete == None) {
	GC  gc;
	Pixmap pix;

	pix = Scr->tbpm.delete = XCreatePixmap (dpy, Scr->Root, h, h, 1);
	gc = XCreateGC (dpy, pix, 0L, NULL);
	XSetLineAttributes (dpy, gc, h, LineSolid, CapRound, JoinRound);
	XSetForeground (dpy, gc, 0L);
	XFillRectangle (dpy, pix, gc, 0, 0, h, h);
	XSetForeground (dpy, gc, 1L);
	XDrawLine (dpy, pix, gc, h/2, h/2, h/2, h/2);
	XFreeGC (dpy, gc);
    }
    return Scr->tbpm.delete;
}

static Image *Create3DDotImage (cp)
ColorPair cp;
{
    Image *image;
    int	  h;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = (Image*) malloc (sizeof (struct _Image));
    if (! image) return (None);
    image->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
    if (image->pixmap == None) return (None);

    Draw3DBorder (image->pixmap, 0, 0, h, h, 2, cp, off, True, False);
    Draw3DBorder (image->pixmap, (h / 2) - 2, (h / 2) - 2, 5, 5, 2, cp, off, True, False);
    image->mask   = None;
    image->width  = h;
    image->height = h;
    image->next   = None;
    return (image);
}

static Image *Create3DBarImage (cp)
ColorPair cp;
{
    Image *image;
    int	  h;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = (Image*) malloc (sizeof (struct _Image));
    if (! image) return (None);
    image->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
    if (image->pixmap == None) return (None);

    Draw3DBorder (image->pixmap, 0, 0, h, h, 2, cp, off, True, False);
    Draw3DBorder (image->pixmap, 4, (h / 2) - 2, h - 8, 5, 2, cp, off, True, False);
    image->mask   = None;
    image->width  = h;
    image->height = h;
    image->next   = None;
    return (image);
}

static Image *Create3DMenuImage (cp)
ColorPair cp;
{
    Image *image;
    int	  h, i;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = (Image*) malloc (sizeof (struct _Image));
    if (! image) return (None);
    image->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
    if (image->pixmap == None) return (None);

    Draw3DBorder (image->pixmap, 0, 0, h, h, 2, cp, off, True, False);
    for (i = 4; i < h - 7; i += 5) {
	Draw3DBorder (image->pixmap, 4, i, h - 8, 4, 2, cp, off, True, False);
    }
    image->mask   = None;
    image->width  = h;
    image->height = h;
    image->next   = None;
    return (image);
}

static Image *Create3DResizeImage (cp)
ColorPair cp;
{
    Image *image;
    int	  h;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = (Image*) malloc (sizeof (struct _Image));
    if (! image) return (None);
    image->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
    if (image->pixmap == None) return (None);

    Draw3DBorder (image->pixmap, 0, 0, h, h, 2, cp, off, True, False);
    Draw3DBorder (image->pixmap, 0, h / 4, ((3 * h) / 4) + 1, ((3 * h) / 4) + 1, 2,
		cp, off, True, False);
    Draw3DBorder (image->pixmap, 0, h / 2, (h / 2) + 1, (h / 2) + 1, 2, cp, off, True, False);
    image->mask   = None;
    image->width  = h;
    image->height = h;
    image->next   = None;
    return (image);
}

static Image *Create3DZoomImage (cp)
ColorPair cp;
{
    Image *image;
    int		h;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = (Image*) malloc (sizeof (struct _Image));
    if (! image) return (None);
    image->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
    if (image->pixmap == None) return (None);

    Draw3DBorder (image->pixmap, 0, 0, h, h, 2, cp, off, True);
    Draw3DBorder (image->pixmap, h / 4, h / 4, (h / 2) + 2, (h / 2) + 2, 2, cp, off, True, False);
    image->mask   = None;
    image->width  = h;
    image->height = h;
    image->next   = None;
    return (image);
}

struct Colori {
    Pixel color;
    Pixmap pix;
    struct Colori *next;
};

Pixmap Create3DMenuIcon (height, widthp, heightp, cp)
unsigned int height, *widthp, *heightp;
ColorPair cp;
{
    unsigned int h, w;
    int		i;
    struct Colori *col;
    static struct Colori *colori = NULL;

    h = height;
    w = h * 7 / 8;
    if (h < 1)
	h = 1;
    if (w < 1)
	w = 1;
    *widthp  = w;
    *heightp = h;

    for (col = colori; col; col = col->next) {
	if (col->color == cp.back) break;
    }
    if (col != NULL) return (col->pix);
    col = (struct Colori*) malloc (sizeof (struct Colori));
    col->color = cp.back;
    col->pix   = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
    col->next = colori;
    colori = col;
    Draw3DBorder (col->pix, 0, 0, w, h, 1, cp, off, True, False);
    for (i = 3; i + 5 < h; i += 5) {
	Draw3DBorder (col->pix, 4, i, w - 8, 3, 1, Scr->MenuC, off, True, False);
    }
    return (colori->pix);
}

#include "siconify.bm"

Pixmap Create3DIconManagerIcon (cp)
ColorPair cp;
{
    int		i;
    unsigned int w, h;
    struct Colori *col;
    static struct Colori *colori = NULL;

    w = (unsigned int) siconify_width;
    h = (unsigned int) siconify_height;

    for (col = colori; col; col = col->next) {
	if (col->color == cp.back) break;
    }
    if (col != NULL) return (col->pix);
    col = (struct Colori*) malloc (sizeof (struct Colori));
    col->color = cp.back;
    col->pix   = XCreatePixmap (dpy, Scr->Root, w, h, Scr->d_depth);
    Draw3DBorder (col->pix, 0, 0, w, h, 4, cp, off, True, False);
    col->next = colori;
    colori = col;

    return (colori->pix);
}

static Image *Create3DResizeAnimation (in, left, top, cp)
Bool in, left, top;
ColorPair cp;
{
    int		h, i, j;
    Image	*image, *im, *im1;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = None;
    for (i = (in ? 0 : (h/4)-1); (i < h/4) && (i >= 0); i += (in ? 1 : -1)) {
	im = (Image*) malloc (sizeof (struct _Image));
	if (! im) return (None);
	im->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
	if (im->pixmap == None) {
	    free (im);
	    return (None);
	}
	Draw3DBorder (im->pixmap, 0, 0, h, h, 2, cp, off, True, False);
	for (j = i; j <= h; j += (h/4)){
	    Draw3DBorder (im->pixmap, (left ? 0 : j), (top ? 0 : j),
			  h - j, h - j, 2, cp, off, True, False);
	}
	im->mask   = None;
	im->width  = h;
	im->height = h;
	im->next   = None;
	if (image == None) {
	    image = im1 = im;
	}
	else {
	    im1->next = im;
	    im1 = im;
	}
    }
    if (im1 != None) im1->next = image;
    return (image);
}

static Image *Create3DResizeInTopAnimation (cp)
ColorPair cp;
{
    return Create3DResizeAnimation (TRUE, FALSE, TRUE, cp);
}

static Image *Create3DResizeOutTopAnimation (cp)
ColorPair cp;
{
    return Create3DResizeAnimation (False, FALSE, TRUE, cp);
}

static Image *Create3DResizeInBotAnimation (cp)
ColorPair cp;
{
    return Create3DResizeAnimation (TRUE, TRUE, FALSE, cp);
}

static Image *Create3DResizeOutBotAnimation (cp)
ColorPair cp;
{
    return Create3DResizeAnimation (False, TRUE, FALSE, cp);
}

static Image *Create3DMenuAnimation (up, cp)
Bool up;
ColorPair cp;
{
    int               h, i, j;
    Image     *image, *im, *im1;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    image = None;
    for (j = (up ? 4 : 0); j != (up ? -1 : 5); j+= (up ? -1 : 1)) {
	im = (Image*) malloc (sizeof (struct _Image));
	if (! im) return (None);
	im->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
	if (im->pixmap == None) {
	    free (im);
	    return (None);
	}
	Draw3DBorder (im->pixmap, 0, 0, h, h, 2, cp, off, True, False);
	for (i = j; i < h - 3; i += 5) {
	    Draw3DBorder (im->pixmap, 4, i, h - 8, 4, 2, cp, off, True, False);
	}
	im->mask   = None;
	im->width  = h;
	im->height = h;
	im->next   = None;
	if (image == None) {
	    image = im1 = im;
	}
	else {
	    im1->next = im;
	    im1 = im;
	}
    }
    if (im1 != None) im1->next = image;
    return (image);
}

static Image *Create3DMenuUpAnimation (cp)
ColorPair cp;
{
    return Create3DMenuAnimation (TRUE, cp);
}

static Image *Create3DMenuDownAnimation (cp)
ColorPair cp;
{
    return Create3DMenuAnimation (False, cp);
}

static Image *Create3DZoomAnimation (in, out, n, cp)
int n;
Bool in;
Bool out;
ColorPair cp;
{
    int		h, i, j, k;
    Image	*image, *im, *im1;

    h = Scr->TBInfo.width - Scr->TBInfo.border * 2;
    if (!(h & 1)) h--;

    if (n == 0) n = (h/2) - 2;

    image = None;
    for (j = (out ? -1 : 1) ; j < (in ? 2 : 0); j += 2){
	for(k = (j > 0 ? 0 : n-1) ; (k >= 0) && (k < n); k += j){
	    im = (Image*) malloc (sizeof (struct _Image));
	    im->pixmap = XCreatePixmap (dpy, Scr->Root, h, h, Scr->d_depth);
	    Draw3DBorder (im->pixmap, 0, 0, h, h, 2, cp, off, True, False);
	    for (i = 2 + k; i < (h / 2); i += n) {
		Draw3DBorder (im->pixmap, i, i, h - (2 * i), h - (2 * i), 2, cp, off, True, False);
	    }
	    im->mask   = None;
	    im->width  = h;
	    im->height = h;
	    im->next   = None;
	    if (image == None) {
		image = im1 = im;
	    }
	    else {
		im1->next = im;
		im1 = im;
	    }
	}
    }
    if (im1 != None) im1->next = image;
    return (image);
}

static Image *Create3DMazeInAnimation (cp)
ColorPair cp;
{
    return Create3DZoomAnimation(TRUE, FALSE, 6, cp);
}

static Image *Create3DMazeOutAnimation (cp)
ColorPair cp;
{
    return Create3DZoomAnimation(FALSE, TRUE, 6, cp);
}

static Image *Create3DZoomInAnimation (cp)
ColorPair cp;
{
    return Create3DZoomAnimation(TRUE, FALSE, 0, cp);
}

static Image *Create3DZoomOutAnimation (cp)
ColorPair cp;
{
    return Create3DZoomAnimation(FALSE, TRUE, 0, cp);
}

static Image *Create3DZoomInOutAnimation (cp)
ColorPair cp;
{
    return Create3DZoomAnimation(TRUE, TRUE, 0, cp);
}

#define questionmark_width 8
#define questionmark_height 8
static char questionmark_bits[] = {
   0x38, 0x7c, 0x64, 0x30, 0x18, 0x00, 0x18, 0x18};

static Pixmap CreateQuestionPixmap (widthp, heightp)
    unsigned int *widthp, *heightp;
{
    *widthp = questionmark_width;
    *heightp = questionmark_height;
    if (Scr->tbpm.question == None) {
	Scr->tbpm.question = XCreateBitmapFromData (dpy, Scr->Root,
						    questionmark_bits,
						    questionmark_width,
						    questionmark_height);
    }
    /*
     * this must succeed or else we are in deep trouble elsewhere
     */
    return Scr->tbpm.question;
}


static Pixmap CreateMenuPixmap (widthp, heightp)
    int *widthp, *heightp;
{
    CreateMenuIcon (Scr->TBInfo.width - Scr->TBInfo.border * 2,widthp,heightp);
}

Pixmap CreateMenuIcon (height, widthp, heightp)
    int	height;
    int	*widthp, *heightp;
{
    int h, w;
    int ih, iw;
    int	ix, iy;
    int	mh, mw;
    int	tw, th;
    int	lw, lh;
    int	lx, ly;
    int	lines, dly;
    int off;
    int	bw;

    h = height;
    w = h * 7 / 8;
    if (h < 1)
	h = 1;
    if (w < 1)
	w = 1;
    *widthp = w;
    *heightp = h;
    if (Scr->tbpm.menu == None) {
	Pixmap  pix;
	GC	gc;

	pix = Scr->tbpm.menu = XCreatePixmap (dpy, Scr->Root, w, h, 1);
	gc = XCreateGC (dpy, pix, 0L, NULL);
	XSetForeground (dpy, gc, 0L);
	XFillRectangle (dpy, pix, gc, 0, 0, w, h);
	XSetForeground (dpy, gc, 1L);
	ix = 1;
	iy = 1;
	ih = h - iy * 2;
	iw = w - ix * 2;
	off = ih / 8;
	mh = ih - off;
	mw = iw - off;
	bw = mh / 16;
	if (bw == 0 && mw > 2)
	    bw = 1;
	tw = mw - bw * 2;
	th = mh - bw * 2;
	XFillRectangle (dpy, pix, gc, ix, iy, mw, mh);
	XFillRectangle (dpy, pix, gc, ix + iw - mw, iy + ih - mh, mw, mh);
	XSetForeground (dpy, gc, 0L);
	XFillRectangle (dpy, pix, gc, ix+bw, iy+bw, tw, th);
	XSetForeground (dpy, gc, 1L);
	lw = tw / 2;
	if ((tw & 1) ^ (lw & 1))
	    lw++;
	lx = ix + bw + (tw - lw) / 2;

	lh = th / 2 - bw;
	if ((lh & 1) ^ ((th - bw) & 1))
	    lh++;
	ly = iy + bw + (th - bw - lh) / 2;

	lines = 3;
	if ((lh & 1) && lh < 6)
	{
	    lines--;
	}
	dly = lh / (lines - 1);
	while (lines--)
	{
	    XFillRectangle (dpy, pix, gc, lx, ly, lw, bw);
	    ly += dly;
	}
	XFreeGC (dpy, gc);
    }
    return Scr->tbpm.menu;
}

void Draw3DBorder (w, x, y, width, height, bw, cp, state, fill, forcebw)
Window w;
int    x, y, width, height, bw;
ColorPair cp;
int state, fill, forcebw;
{
    int		  i;
    XGCValues	  gcv;
    unsigned long gcm;
    static int firsttime = 1;

    if ((width < 1) || (height < 1)) return;
    if (Scr->Monochrome != COLOR) {
	if (fill) {
	    gcm = GCFillStyle;
	    gcv.fill_style = FillOpaqueStippled;
	    XChangeGC (dpy, Scr->GreyGC, gcm, &gcv);
	    XFillRectangle (dpy, w, Scr->GreyGC, x, y, width, height);
	}
	gcm  = 0;
	gcm |= GCLineStyle;		
	gcv.line_style = (state == on) ? LineSolid : LineDoubleDash;
	gcm |= GCFillStyle;
	gcv.fill_style = FillSolid;
	XChangeGC (dpy, Scr->GreyGC, gcm, &gcv);
	for (i = 0; i < bw; i++) {
	    XDrawLine (dpy, w, Scr->GreyGC, x,                 y + i,
					    x + width - i - 1, y + i);
	    XDrawLine (dpy, w, Scr->GreyGC, x + i,                  y,
					    x + i, y + height - i - 1);
	}

	gcm  = 0;
	gcm |= GCLineStyle;		
	gcv.line_style = (state == on) ? LineDoubleDash : LineSolid;
	gcm |= GCFillStyle;
	gcv.fill_style = FillSolid;
	XChangeGC (dpy, Scr->GreyGC, gcm, &gcv);
	for (i = 0; i < bw; i++) {
	    XDrawLine (dpy, w, Scr->GreyGC, x + width - i - 1,          y + i,
					    x + width - i - 1, y + height - 1);
	    XDrawLine (dpy, w, Scr->GreyGC, x + i,         y + height - i - 1,
					    x + width - 1, y + height - i - 1);
	}
	return;
    }

    if (fill) {
	FB (cp.back, cp.fore);
	XFillRectangle (dpy, w, Scr->NormalGC, x, y, width, height);
    }
    if (Scr->BeNiceToColormap) {
	int dashoffset = 0;

	gcm  = 0;
	gcm |= GCLineStyle;		
	gcv.line_style = (forcebw) ? LineSolid : LineDoubleDash;
	gcm |= GCBackground;
	gcv.background = cp.back;
	XChangeGC (dpy, Scr->ShadGC, gcm, &gcv);
	    
	if (state == on)
	    XSetForeground (dpy, Scr->ShadGC, Scr->Black);
	else
	    XSetForeground (dpy, Scr->ShadGC, Scr->White);
	for (i = 0; i < bw; i++) {
	    XDrawLine (dpy, w, Scr->ShadGC, x + i,     y + dashoffset,
					    x + i, y + height - i - 1);
	    XDrawLine (dpy, w, Scr->ShadGC, x + dashoffset,    y + i,
					    x + width - i - 1, y + i);
	    dashoffset = 1 - dashoffset;
	}
	if (state == on)
	    XSetForeground (dpy, Scr->ShadGC, Scr->White);
	else
	    XSetForeground (dpy, Scr->ShadGC, Scr->Black);
	for (i = 0; i < bw; i++) {
	    XDrawLine (dpy, w, Scr->ShadGC, x + i,         y + height - i - 1,
					    x + width - 1, y + height - i - 1);
	    XDrawLine (dpy, w, Scr->ShadGC, x + width - i - 1,          y + i,
					    x + width - i - 1, y + height - 1);
	}
	return;
    }

    if (state == on) {
	FB (cp.shadd, cp.shadc);
    }
    else {
	FB (cp.shadc, cp.shadd);
    }
    for (i = 0; i < bw; i++) {
	XDrawLine (dpy, w, Scr->NormalGC, x,                 y + i,
					  x + width - i - 1, y + i);
	XDrawLine (dpy, w, Scr->NormalGC, x + i,                  y,
					  x + i, y + height - i - 1);
    }

    if (state == on) {
	FB (cp.shadc, cp.shadd);
    }
    else {
	FB (cp.shadd, cp.shadc);
    }
    for (i = 0; i < bw; i++) {
	XDrawLine (dpy, w, Scr->NormalGC, x + width - i - 1,          y + i,
					  x + width - i - 1, y + height - 1);
	XDrawLine (dpy, w, Scr->NormalGC, x + i,         y + height - i - 1,
					  x + width - 1, y + height - i - 1);
    }
    return;
}

void Draw3DCorner (w, x, y, width, height, thick, bw, cp, type)
Window		w;
int		x, y, width, height, thick, bw;
ColorPair	cp;
int		type;
{
    Draw3DBorder (w, x, y, width, height, bw, cp, off, True, False);
    switch (type) {
	case 0 :
	    Draw3DBorder (w, x + thick - bw, y + thick - bw,
			width - thick + 2 * bw, height - thick + 2 * bw,
			bw, cp, on, True, False);
	    break;
	case 1 :
	    Draw3DBorder (w, x, y + thick - bw,
			width - thick + bw, height - thick,
			bw, cp, on, True, False);
	    break;
	case 2 :
	    Draw3DBorder (w, x, y,
			width - thick + bw, height - thick + bw,
			bw, cp, on, True, False);
	    break;
	case 3 :
	    Draw3DBorder (w, x + thick - bw, y,
			width - thick, height - thick + bw,
			bw, cp, on, True, False);
	    break;
    }
    return;
}

void PaintAllDecoration () {
    TwmWindow *tmp_win;

    for (tmp_win = Scr->TwmRoot.next; tmp_win != NULL; tmp_win = tmp_win->next) {
	if (! OCCUPY (tmp_win, Scr->workSpaceMgr.activeWSPC)) continue;
	if (tmp_win->mapped == TRUE) {
	    if (tmp_win->frame_bw3D)
		if (tmp_win->highlight && tmp_win == Scr->Focus)
		    PaintBorders (tmp_win, True);
		else
		    PaintBorders (tmp_win, False);
	    if (tmp_win->title_w)      PaintTitle        (tmp_win);
	    if (tmp_win->titlebuttons) PaintTitleButtons (tmp_win);
	}
	else
	if ((tmp_win->icon_on == TRUE) && (tmp_win->icon) && (tmp_win->icon->w) &&
		! Scr->NoIconTitlebar &&
		! LookInList (Scr->NoIconTitle, tmp_win->full_name, &tmp_win->class)) {
	    PaintIcon (tmp_win);
	}
    }
    PaintWorkSpaceManager ();
}

void PaintBorders (tmp_win, focus)
TwmWindow *tmp_win;
Bool focus;
{
    ColorPair cp;

    cp = (focus && tmp_win->highlight) ? tmp_win->borderC : tmp_win->border_tile;
    if (tmp_win->title_height == 0) {
	Draw3DBorder (tmp_win->frame,
	    0,
	    0,
	    tmp_win->frame_width,
	    tmp_win->frame_height,
	    2, cp, off, True, False);
	Draw3DBorder (tmp_win->frame,
	    tmp_win->frame_bw3D - 2,
	    tmp_win->frame_bw3D - 2,
	    tmp_win->frame_width  - 2 * tmp_win->frame_bw3D + 4,
	    tmp_win->frame_height - 2 * tmp_win->frame_bw3D + 4,
	    2, cp, on, True, False);
	return;
    }
    Draw3DCorner (tmp_win->frame,
		tmp_win->title_x - tmp_win->frame_bw3D,
		0,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		tmp_win->frame_bw3D, 2, cp, 0);
    Draw3DCorner (tmp_win->frame,
		tmp_win->title_x + tmp_win->title_width - Scr->TitleHeight,
		0,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		tmp_win->frame_bw3D, 2, cp, 1);
    Draw3DCorner (tmp_win->frame,
		tmp_win->frame_width  - (Scr->TitleHeight + tmp_win->frame_bw3D),
		tmp_win->frame_height - (Scr->TitleHeight + tmp_win->frame_bw3D),
		Scr->TitleHeight + tmp_win->frame_bw3D,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		tmp_win->frame_bw3D, 2, cp, 2);
    Draw3DCorner (tmp_win->frame,
		0,
		tmp_win->frame_height - (Scr->TitleHeight + tmp_win->frame_bw3D),
		Scr->TitleHeight + tmp_win->frame_bw3D,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		tmp_win->frame_bw3D, 2, cp, 3);
    Draw3DBorder (tmp_win->frame,
		tmp_win->title_x + Scr->TitleHeight,
		0,
		tmp_win->title_width - 2 * Scr->TitleHeight,
		tmp_win->frame_bw3D,
		2, cp, off, True, False);
    Draw3DBorder (tmp_win->frame,
		tmp_win->frame_bw3D + Scr->TitleHeight,
		tmp_win->frame_height - tmp_win->frame_bw3D,
		tmp_win->frame_width - 2 * (Scr->TitleHeight + tmp_win->frame_bw3D),
		tmp_win->frame_bw3D,
		2, cp, off, True, False);
    Draw3DBorder (tmp_win->frame,
		0,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		tmp_win->frame_bw3D,
		tmp_win->frame_height - 2 * (Scr->TitleHeight + tmp_win->frame_bw3D),
		2, cp, off, True, False);
    Draw3DBorder (tmp_win->frame,
		tmp_win->frame_width  - tmp_win->frame_bw3D,
		Scr->TitleHeight + tmp_win->frame_bw3D,
		tmp_win->frame_bw3D,
		tmp_win->frame_height - 2 * (Scr->TitleHeight + tmp_win->frame_bw3D),
		2, cp, off, True, False);
}

void PaintTitle (tmp_win)
TwmWindow *tmp_win;
{
    if (Scr->use3Dtitles)
	if (Scr->SunkFocusWindowTitle && (Scr->Focus == tmp_win) &&
	    (tmp_win->title_height != 0))
	    Draw3DBorder (tmp_win->title_w, Scr->TBInfo.titlex, 0,
		tmp_win->title_width  - Scr->TBInfo.titlex - Scr->TBInfo.rightoff,
		Scr->TitleHeight, 2,
		tmp_win->title, on, True, False);
	else
	    Draw3DBorder (tmp_win->title_w, Scr->TBInfo.titlex, 0,
		tmp_win->title_width  - Scr->TBInfo.titlex - Scr->TBInfo.rightoff,
		Scr->TitleHeight, 2,
		tmp_win->title, off, True, False);

    FBF(tmp_win->title.fore, tmp_win->title.back, Scr->TitleBarFont.font->fid);

    if (Scr->use3Dtitles) {
	if (Scr->Monochrome != COLOR) {
	    XDrawImageString (dpy, tmp_win->title_w, Scr->NormalGC,
		 tmp_win->name_x, Scr->TitleBarFont.y + 2, 
		 tmp_win->name, strlen(tmp_win->name));
	}
	else
	    XDrawString (dpy, tmp_win->title_w, Scr->NormalGC,
		 tmp_win->name_x, Scr->TitleBarFont.y + 2, 
		 tmp_win->name, strlen(tmp_win->name));
    }
    else
        XDrawString (dpy, tmp_win->title_w, Scr->NormalGC,
		 tmp_win->name_x, Scr->TitleBarFont.y, 
		 tmp_win->name, strlen(tmp_win->name));
}

void PaintIcon (tmp_win)
TwmWindow *tmp_win;
{
    if (Scr->use3Diconmanagers) {
	Draw3DBorder (tmp_win->icon->w, 0, tmp_win->icon->height,
		tmp_win->icon->w_width, Scr->IconFont.height + 6,
		2, tmp_win->icon->iconc, off, False, False);
    }
    FBF(tmp_win->icon->iconc.fore, tmp_win->icon->iconc.back,
	Scr->IconFont.font->fid);

    XDrawString (dpy, tmp_win->icon->w,
	Scr->NormalGC,
	tmp_win->icon->x, tmp_win->icon->y,
	tmp_win->icon_name, strlen(tmp_win->icon_name));
}

void PaintTitleButton (tmp_win, tbw)
TwmWindow *tmp_win;
TBWindow  *tbw;
{
    TitleButton *tb = tbw->info;

    XCopyArea (dpy, tbw->image->pixmap, tbw->window, Scr->NormalGC,
		tb->srcx, tb->srcy, tb->width, tb->height,
		tb->dstx, tb->dsty);
    return;
}

void PaintTitleButtons (tmp_win)
TwmWindow *tmp_win;
{
    int i;
    TBWindow *tbw;
    int nb = Scr->TBInfo.nleft + Scr->TBInfo.nright;

    for (i = 0, tbw = tmp_win->titlebuttons; i < nb; i++, tbw++) {
	if (tbw) PaintTitleButton (tmp_win, tbw);
    }
}

void adoptWindow () {
    unsigned long	data [2];
    Bool		savestate;
    Window		localroot, w;
    unsigned char	*prop;
    unsigned long	bytesafter;
    unsigned long	len;
    Atom		actual_type;
    int			actual_format;
    XEvent		event;
    Window		root, parent, child, *children;
    unsigned int	nchildren, key_buttons;
    int			root_x, root_y, win_x, win_y;

    localroot = w = RootWindow (dpy, Scr->screen);
    XGrabPointer (dpy, localroot, False,
			ButtonPressMask | ButtonReleaseMask,
			GrabModeAsync, GrabModeAsync,
		        None, Scr->SelectCursor, CurrentTime);

    XMaskEvent (dpy, ButtonPressMask | ButtonReleaseMask, &event);
    child = event.xbutton.subwindow;
    while (1) {
	if (child == (Window) 0) break;

	w = XmuClientWindow (dpy, child);
	if (XGetWindowProperty (dpy, w, _XA_WM_WORKSPACESLIST, 0L, 512,
			False, XA_STRING, &actual_type, &actual_format, &len,
			&bytesafter, &prop) != Success) break;
	if (len == 0) break; /* it is not a local root window */
	localroot = w;
	XQueryPointer (dpy, localroot, &root, &child, &root_x, &root_y,
					&win_x, &win_y, &key_buttons);
    }
    XMaskEvent (dpy, ButtonPressMask | ButtonReleaseMask, &event);
    XUngrabPointer (dpy, CurrentTime);

    if (localroot == Scr->Root) return;
    if (w == localroot) {  /* try to not adopt an ancestor */
	XQueryTree (dpy, Scr->Root, &root, &parent, &children, &nchildren);
	while (parent != (Window) 0) {
	    XFree ((char *) children);
	    if (w == parent) return;
	    XQueryTree (dpy, parent, &root, &parent, &children, &nchildren);
	}
	XFree ((char *) children);
	if (w == root) return;
    }
    if (localroot == RootWindow (dpy, Scr->screen)) {
	XWithdrawWindow (dpy, w, Scr->screen);
    }
    else {
	XUnmapWindow (dpy, w);
    }
    XReparentWindow (dpy, w, Scr->Root, 0, 0);

    data [0] = (unsigned long) NormalState;
    data [1] = (unsigned long) None;

    XChangeProperty (dpy, w, _XA_WM_STATE, _XA_WM_STATE, 32, 
			PropModeReplace, (unsigned char *) data, 2);
    XFlush (dpy);
    SimulateMapRequest (w);
    return;
}

void testfunc () {
    char logname [64];

    if (errorlog) {
	fclose (errorlog);
	printf ("stop logging events\n");
	errorlog = NULL;
    }
    else {
	sprintf (logname, "CtwmLog.%d", getpid());
	printf ("logging events to : %s\n", logname);
	errorlog = fopen (logname, "w");
    }
}

extern Cursor	TopRightCursor, TopLeftCursor, BottomRightCursor, BottomLeftCursor,
		LeftCursor, RightCursor, TopCursor, BottomCursor;

void SetBorderCursor (tmp_win, x, y)
TwmWindow *tmp_win;
int       x, y;
{
    Cursor cursor;
    XSetWindowAttributes attr;
    int h  = Scr->TitleHeight + tmp_win->frame_bw3D;
    int fw = tmp_win->frame_width;
    int fh = tmp_win->frame_height;
    int wd = tmp_win->frame_bw3D;

    if (! tmp_win) return;
    if ((x < 0) || (y < 0)) cursor = Scr->FrameCursor;
    else
    if (x < wd) {
	if (y < h) cursor = TopLeftCursor;
	else
	if (y >= fh - h) cursor = BottomLeftCursor;
	else cursor = LeftCursor;
    }
    else
    if (x >= fw - wd) {
	if (y < h) cursor = TopRightCursor;
	else
	if (y >= fh - h) cursor = BottomRightCursor;
	else cursor = RightCursor;
    }
    else
    if (y < wd) {
	if (x < h) cursor = TopLeftCursor;
	else
	if (x >= fw - h) cursor = TopRightCursor;
	else cursor = TopCursor;
    }
    else
    if (y >= fh - wd) {
	if (x < h) cursor = BottomLeftCursor;
	else
	if (x >= fw - h) cursor = BottomRightCursor;
	else cursor = BottomCursor;
    }
    else cursor = Scr->FrameCursor;
    attr.cursor = cursor;
    XChangeWindowAttributes (dpy, tmp_win->frame, CWCursor, &attr);
    tmp_win->curcurs = cursor;
}

Image *GetImage (name, cp)
char      *name;
ColorPair cp;
{
    name_list **list;
    char fullname [256];
    int   startn;
    Image *image;

    if (name == NULL) return (None);
    image = None;

    list = &Scr->ImageCache;
#ifdef XPM
    if ((name [0] == '@') || (strncmp (name, "xpm:", 4) == 0)) {
	startn = (name [0] == '@') ? 1 : 4;
	if ((image = (Image*) LookInNameList (*list, name)) == None) {
	    if ((image = GetXpmImage (&(name [startn]))) != None) {
		AddToList (list, name, (char*) image);
	    }
	}
    }
    else
#endif
#ifdef IMCONV
    if (strncmp (name, "im:", 3) == 0) {
	if ((image = (Image*) LookInNameList (*list, name)) == None) {
	    if ((image = GetImconvImage (&name [3])) != None) {
		AddToList (list, name, (char*) image);
	    }
	}
    }
    else
#endif
#ifndef VMS
    if ((strncmp (name, "xwd:", 4) == 0) || (name [0] == '|')) {
	startn = (name [0] == '|') ? 0 : 4;
	if ((image = (Image*) LookInNameList (*list, name)) == None) {
	    if ((image = GetXwdImage (&name [startn])) != None) {
		AddToList (list, name, (char*) image);
	    }
	}
    }
    else
#endif
    if (strncmp (name, ":xpm:", 5) == 0) {
	int    i;
	static struct {
	    char *name;
	    Image* (*proc)();
	} pmtab[] = {
	    { TBPM_3DDOT,	Create3DDotImage },
	    { TBPM_3DRESIZE,	Create3DResizeImage },
	    { TBPM_3DMENU,	Create3DMenuImage },
	    { TBPM_3DZOOM,	Create3DZoomImage },
	    { TBPM_3DBAR,	Create3DBarImage },
	};
	
	sprintf (fullname, "%s%dx%d", name, (int) cp.fore, (int) cp.back);
	if ((image = (Image*) LookInNameList (*list, fullname)) == None) {
	    for (i = 0; i < (sizeof pmtab) / (sizeof pmtab[0]); i++) {
		if (XmuCompareISOLatin1 (pmtab[i].name, name) == 0) {
		    image = (*pmtab[i].proc) (cp);
		    break;
		}
	    }
	    if (image == None) {
		fprintf (stderr, "%s:  no such built-in pixmap \"%s\"\n", ProgramName, name);
		return (None);
	    }
	    AddToList (list, fullname, (char*) image);
	}
    }
    else
    if (strncmp (name, "%xpm:", 5) == 0) {
	int    i;
	static struct {
	    char *name;
	    Image* (*proc)();
	} pmtab[] = {
	    { "%xpm:menu-up", Create3DMenuUpAnimation },
	    { "%xpm:menu-down", Create3DMenuDownAnimation },
	    { "%xpm:resize", Create3DZoomOutAnimation }, /* compatibility */
	    { "%xpm:resize-out-top", Create3DResizeInTopAnimation },
	    { "%xpm:resize-in-top", Create3DResizeOutTopAnimation },
	    { "%xpm:resize-out-bot", Create3DResizeInBotAnimation },
	    { "%xpm:resize-in-bot", Create3DResizeOutBotAnimation },
	    { "%xpm:maze-out", Create3DMazeOutAnimation },
	    { "%xpm:maze-in", Create3DMazeInAnimation },
	    { "%xpm:zoom-out", Create3DZoomOutAnimation },
	    { "%xpm:zoom-in", Create3DZoomInAnimation },
	    { "%xpm:zoom-inout", Create3DZoomInOutAnimation }
	};
	
	sprintf (fullname, "%s%dx%d", name, (int) cp.fore, (int) cp.back);
	if ((image = (Image*) LookInNameList (*list, fullname)) == None) {
	    for (i = 0; i < (sizeof pmtab) / (sizeof pmtab[0]); i++) {
		if (XmuCompareISOLatin1 (pmtab[i].name, name) == 0) {
		    image = (*pmtab[i].proc) (cp);
		    break;
		}
	    }
	    if (image == None) {
		fprintf (stderr, "%s:  no such built-in pixmap \"%s\"\n", ProgramName, name);
		return (None);
	    }
	    AddToList (list, fullname, (char*) image);
	}
    }
    else
    if (name [0] == ':') {
	int       i, width, height;
	Pixmap    pm;
	XGCValues gcvalues;
	static struct {
	    char *name;
	    Pixmap (*proc)();
	} pmtab[] = {
	    { TBPM_DOT,		CreateDotPixmap },
	    { TBPM_ICONIFY,	CreateDotPixmap },
	    { TBPM_RESIZE,	CreateResizePixmap },
	    { TBPM_XLOGO,	CreateXLogoPixmap },
	    { TBPM_DELETE,	CreateXLogoPixmap },
	    { TBPM_MENU,	CreateMenuPixmap },
	    { TBPM_QUESTION,	CreateQuestionPixmap },
	};
	
	sprintf (fullname, "%s%dx%d", name, (int) cp.fore, (int) cp.back);
	if ((image = (Image*) LookInNameList (*list, fullname)) == None) {
	    for (i = 0; i < (sizeof pmtab) / (sizeof pmtab[0]); i++) {
		if (XmuCompareISOLatin1 (pmtab[i].name, name) == 0) {
		    pm = (*pmtab[i].proc) (&width, &height);
		    break;
		}
	    }
	    if (pm == None) {
		fprintf (stderr, "%s:  no such built-in bitmap \"%s\"\n", ProgramName, name);
		return (None);
	    }
	    image = (Image*) malloc (sizeof (struct _Image));
	    image->pixmap = XCreatePixmap (dpy, Scr->Root, width, height, Scr->d_depth);
	    if (Scr->rootGC == (GC) 0) Scr->rootGC = XCreateGC (dpy, Scr->Root, 0, &gcvalues);
	    gcvalues.background = cp.back;
	    gcvalues.foreground = cp.fore;
	    XChangeGC   (dpy, Scr->rootGC, GCForeground | GCBackground, &gcvalues);
	    XCopyPlane  (dpy, pm, image->pixmap, Scr->rootGC, 0, 0, width, height, 0, 0,
			(unsigned long) 1);
	    image->mask   = None;
	    image->width  = width;
	    image->height = height;
	    image->next   = None;
	    AddToList (list, fullname, (char*) image);
	}
    }
    else {
	sprintf (fullname, "%s%dx%d", name, (int) cp.fore, (int) cp.back);
	if ((image = (Image*) LookInNameList (*list, fullname)) == None) {
	    if ((image = GetBitmapImage (name, cp)) != None) {
		AddToList (list, fullname, (char*) image);
	    }
	}
    }
    return (image);
}

void FreeImage (image)
Image *image;
{
    Image *im, *im2;

    im = image;
    while (im != None) {
	if (im->pixmap) XFreePixmap (dpy, im->pixmap);
	if (im->mask)   XFreePixmap (dpy, im->mask);
	im2 = im->next;
	free (im);
	im = im2;
    }
}

#ifndef VMS
static void compress ();

static Image *LoadXwdImage (filename)
char	*filename;
{
    FILE	*file;
    char	*fullname;
    XColor	colors [256];
    XWDColor	xwdcolors [256];
    unsigned	buffer_size;
    XImage	*image;
    unsigned char *imagedata;
    Pixmap	pixret;
    Visual	*visual;
    char	win_name [256];
    int		win_name_size;
    int		unit;
    int		ispipe;
    int		i, len;
    int		w, h, depth, ncolors;
    int		scrn;
    Colormap	cmap;
    GC		gc;
    XWDFileHeader header;
    Image	*ret;
    Bool	anim;
    unsigned long swaptest = 1;

    ispipe = 0;
    anim   = False;
    if (filename [0] == '|') {
	file = (FILE*) popen (filename + 1, "r");
	if (file == NULL) return (None);
	ispipe = 1;
	anim = AnimationActive;
	if (anim) StopAnimation ();
	goto file_opened;
    }
    fullname = ExpandPixmapPath (filename);
    if (! fullname) return (None);
    file = fopen (fullname, "r");
    free (fullname);
    if (file == NULL) {
	if (reportfilenotfound) fprintf (stderr, "unable to locate %s\n", filename);
        return (None);
    }
file_opened:
    len = fread ((char *) &header, sizeof (header), 1, file);
    if (len != 1) {
	fprintf (stderr, "ctwm: cannot read %s\n", filename);
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }
    if (header.file_version != XWD_FILE_VERSION) {
	fprintf(stderr,"ctwm: XWD file format version mismatch : %s\n", filename);
	return (None);
    }
    if (*(char *) &swaptest) _swaplong ((char *) &header, sizeof (header));
    win_name_size = header.header_size - sizeof (header);
    len = fread (win_name, win_name_size, 1, file);
    if (len != 1) {
	fprintf (stderr, "file %s has not the correct format\n", filename);
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }

    w       = header.pixmap_width;
    h       = header.pixmap_height;
    depth   = header.pixmap_depth;
    ncolors = header.ncolors;
    len = fread ((char *) xwdcolors, sizeof (XWDColor), ncolors, file);
    if (len != ncolors) {
	fprintf (stderr, "file %s has not the correct format\n", filename);
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }
    if (*(char *) &swaptest) {
	for (i = 0; i < ncolors; i++) {
	    _swaplong  ((char *) &xwdcolors [i].pixel, 4);
	    _swapshort ((char *) &xwdcolors [i].red, 3 * 2);
	}
    }
    for (i = 0; i < ncolors; i++) {
	colors [i].pixel = xwdcolors [i].pixel;
	colors [i].red   = xwdcolors [i].red;
	colors [i].green = xwdcolors [i].green;
	colors [i].blue  = xwdcolors [i].blue;
	colors [i].flags = xwdcolors [i].flags;
	colors [i].pad   = xwdcolors [i].pad;
    }

    scrn    = Scr->screen;
    cmap    = AlternateCmap ? AlternateCmap : DefaultColormap (dpy, scrn);
    visual  = DefaultVisual (dpy, scrn);
    gc      = DefaultGC     (dpy, scrn);

    buffer_size = header.bytes_per_line * h;
    imagedata = (unsigned char*) malloc (buffer_size);
    if (! imagedata) {
	fprintf (stderr, "cannot allocate memory for image %s\n", filename);
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }
    len = fread (imagedata, (int) buffer_size, 1, file);
    if (len != 1) {
	free (imagedata);
	fprintf (stderr, "file %s has not the correct format\n", filename);
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }
    if (ispipe) pclose (file); else fclose (file);

    image = XCreateImage (dpy, visual,  depth, header.pixmap_format,
                          0, (char*) imagedata, w, h,
                          header.bitmap_pad, header.bytes_per_line);
    if (image == None) {
	free (imagedata);
	fprintf (stderr, "cannot create image for %s\n", filename);
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }
    compress (image, colors, &ncolors);

    for (i = 0; i < ncolors; i++) {
        XAllocColor (dpy, cmap, &(colors [i]));
    }
    for (i = 0; i < buffer_size; i++) {
        imagedata [i] = (unsigned char) colors [imagedata [i]].pixel;
    }

    if (w > Scr->MyDisplayWidth)  w = Scr->MyDisplayWidth;
    if (h > Scr->MyDisplayHeight) h = Scr->MyDisplayHeight;

    ret = (Image*) malloc (sizeof (struct _Image));
    if (! ret) {
	fprintf (stderr, "unable to allocate memory for image : %s\n", filename);
	free (image);
	free (imagedata);
	for (i = 0; i < ncolors; i++) {
            XFreeColors (dpy, cmap, &(colors [i].pixel), 1, 0L);
	}
#ifdef USE_SIGNALS
	if (ispipe && anim) StartAnimation ();
#endif
	return (None);
    }
    if ((w > (Scr->MyDisplayWidth / 2)) || (h > (Scr->MyDisplayHeight / 2))) {
	int x, y;

	pixret = XCreatePixmap (dpy, Scr->Root, Scr->MyDisplayWidth,
				Scr->MyDisplayHeight, depth);
	x = (Scr->MyDisplayWidth  - w) / 2;
	y = (Scr->MyDisplayHeight - h) / 2;
	XFillRectangle (dpy, pixret, gc, 0, 0, Scr->MyDisplayWidth, Scr->MyDisplayHeight);
	XPutImage (dpy, pixret, gc, image, 0, 0, x, y, w, h);
	ret->width  = Scr->MyDisplayWidth;
	ret->height = Scr->MyDisplayHeight;
    }
    else {
	pixret = XCreatePixmap (dpy, Scr->Root, w, h, depth);
	XPutImage (dpy, pixret, gc, image, 0, 0, 0, 0, w, h);
	ret->width  = w;
	ret->height = h;
    }
    XFree (image);

    ret->pixmap = pixret;
    ret->mask   = None;
    ret->next   = None;
#ifdef USE_SIGNALS
    if (ispipe && anim) StartAnimation ();
#endif
    return (ret);
}

static Image *GetXwdImage (name)
char *name;
{
    Image *image, *r, *s;
    char  path [128];
    char  pref [128], *perc;
    int   width, height, i;

    if (! strchr (name, '%')) return (LoadXwdImage (name));
    s = image = None;
    strcpy (pref, name);
    perc  = strchr (pref, '%');
    *perc = '\0';
    reportfilenotfound = 0;
    for (i = 1;; i++) {
	sprintf (path, "%s%d%s", pref, i, perc + 1);
	r = LoadXwdImage (path);
	if (r == None) break;
	r->next   = None;
	if (image == None) s = image = r;
	else {
	    s->next = r;
	    s = r;
	}
    }
    reportfilenotfound = 1;
    if (s != None) s->next = image;
    if (image == None) {
	fprintf (stderr, "Cannot open any %s xwd file\n", name);
    }
    return (image);
}

static void compress (image, colors, ncolors)
XImage *image;
XColor *colors;
int    *ncolors;
{
    unsigned char ind  [256];
    unsigned int  used [256];  
    int           i, j, size, nused;
    unsigned char color;
    XColor        newcolors [256];
    unsigned char *imagedata;

    for (i = 0; i < 256; i++) {
	used [i] = 0;
	ind  [i] = 0;
    }
    nused = 0;
    size  = image->bytes_per_line * image->height;
    imagedata = (unsigned char *) image->data;
    for (i = 0; i < size; i++) {
	if ((i % image->bytes_per_line) > image->width) continue;
        color = imagedata [i];
        if (used [color] == 0) {
            for (j = 0; j < nused; j++) {
                if ((colors [color].red   == newcolors [j].red)   &&
                    (colors [color].green == newcolors [j].green) &&
                    (colors [color].blue  == newcolors [j].blue)) break;
            }
            ind  [color] = j;
            used [color] = 1;
            if (j == nused) {
                newcolors [j].red   = colors [color].red;
                newcolors [j].green = colors [color].green;
                newcolors [j].blue  = colors [color].blue;
                nused++;
            }
        }
    }
    for (i = 0; i < size; i++) {
        imagedata [i] = ind [imagedata [i]];
    }
    for (i = 0; i < nused; i++) {
        colors [i] = newcolors [i];
    }
    *ncolors = nused;
}
#endif

#ifdef IMCONV

static void free_images  ();

static Image *GetImconvImage (filename, width, height)
char	*filename;
int	*width, *height;
{
    TagTable		*toolInTable;
    ImVfb		*sourceVfb;
    ImVfbPtr		vptr;
    ImClt		*clt;
    int			i, j, ij, k, retval;

    XColor		colors [256];
    unsigned		buffer_size;
    XImage		*image;
    unsigned char	*imagedata;
    Pixmap		pixret;
    Visual		*visual;
    int			w, h, depth, ncolors;
    int			scrn;
    Colormap		cmap;
    GC			gc;
    unsigned char	red, green, blue;
    int			icol;
    char		*fullname;

    TagEntry		*dataEntry;
    FILE		*fp;
    char		the_format[1024];
    char		*tmp_format;
    Image		*ret;

    if (*filename == NULL) return (None);
    fullname = ExpandPixmapPath (filename);
    if (! fullname) return (None);

    fp = fopen (fullname, "r");
    if (!fp) {
	if (reportfilenotfound) fprintf (stderr, "Cannot open the image %s\n", filename);
	free (fullname);
	return (None);
    }
    if ((toolInTable = TagTableAlloc ()) == TAGTABLENULL ) {
	fprintf (stderr, "TagTableAlloc failed\n");
	free_images (toolInTable);
	free (fullname);
	return (None);
    }
    if ((tmp_format = ImFileQFFormat (fp, fullname)) == NULL)  {
	fprintf (stderr, "Cannot determine image type of %s\n", filename);
	free_images  (toolInTable);
	free (fullname);
	return (None);
    }
    strcpy (the_format, tmp_format);
    retval = ImFileFRead (fp, the_format, NULL, toolInTable);
    if(retval < 0) {
	fprintf(stderr, "Cannot read image file %s: ", fullname);
	switch(ImErrNo) {
	    case IMESYS:
		fprintf (stderr, "System call error\n");
		break;
	    case IMEMALLOC:
		fprintf (stderr, "Cannot allocate memory\n");
		break;
	    case IMEFORMAT:
		fprintf (stderr, "Data in file is corrupt\n");
		break;
	    case IMENOREAD:
		fprintf (stderr, "Sorry, this format is write-only\n");
		break;
	    case IMEMAGIC:
		fprintf (stderr, "Bad magic number in image file\n");
		break;
	    case IMEDEPTH:
		fprintf (stderr, "Unknown image depth\n");
		break;
	    default:
		fprintf(stderr, "Unknown error\n");
		break;
	}
	free_images (toolInTable);
	free (fullname);
	return (None);
    }

    if (TagTableQNEntry (toolInTable, "image vfb") == 0)  {
	fprintf (stderr, "Image file %s contains no images\n", fullname);
	free_images (toolInTable);
	free (fullname);
	return (None);
    }
    dataEntry = TagTableQDirect (toolInTable, "image vfb", 0);
    TagEntryQValue (dataEntry, &sourceVfb);
    fclose (fp);

    w = ImVfbQWidth  (sourceVfb);
    h = ImVfbQHeight (sourceVfb);
    depth = 8 * ImVfbQNBytes (sourceVfb);
    if (depth != 8) {
	fprintf (stderr, "I don't know yet how to deal with images not of 8 planes depth\n");
	free_images (toolInTable);
	return (None);
    }

    *width  = w;
    *height = h;

    scrn   = Scr->screen;
    cmap   = AlternateCmap ? AlternateCmap : DefaultColormap (dpy, scrn);
    visual = DefaultVisual (dpy, scrn);
    gc     = DefaultGC     (dpy, scrn);

    buffer_size = w * h;
    imagedata = (unsigned char*) malloc (buffer_size);
    if (imagedata == (unsigned char*) 0) {
	fprintf (stderr, "Can't alloc enough space for background images\n");
	free_images (toolInTable);
	return (None);
    }

    clt  = ImVfbQClt   (sourceVfb);
    vptr = ImVfbQFirst (sourceVfb);
    ncolors = 0;
    for (i = 0; i < h - 1; i++) {
	for (j = 0; j < w; j++) {
	    ij = (i * w) + j;
	    red   = ImCltQRed   (ImCltQPtr (clt, ImVfbQIndex (sourceVfb, vptr)));
	    green = ImCltQGreen (ImCltQPtr (clt, ImVfbQIndex (sourceVfb, vptr)));
	    blue  = ImCltQBlue  (ImCltQPtr (clt, ImVfbQIndex (sourceVfb, vptr)));
	    for (k = 0; k < ncolors; k++) {
		if ((colors [k].red   == red) &&
		    (colors [k].green == green) &&
		    (colors [k].blue  == blue)) {
		    icol = k;
		    break;
		}
	    }
	    if (k == ncolors) {
		icol = ncolors;
		ncolors++;
	    }
	    imagedata [ij] = icol;
	    colors [icol].red   = red;
	    colors [icol].green = green;
	    colors [icol].blue  = blue;
	    ImVfbSInc (sourceVfb, vptr);
	}
    }
    for (i = 0; i < ncolors; i++) {
	colors [i].red   *= 256;
	colors [i].green *= 256;
	colors [i].blue  *= 256;
    }
    for (i = 0; i < ncolors; i++) {
        if (! XAllocColor (dpy, cmap, &(colors [i]))) {
	    fprintf (stderr, "can't alloc color for image %s\n", filename);
	}
    }
    for (i = 0; i < buffer_size; i++) {
        imagedata [i] = (unsigned char) colors [imagedata [i]].pixel;
    }

    image  = XCreateImage  (dpy, visual, depth, ZPixmap, 0, (char*) imagedata, w, h, 8, 0);
    if (w > Scr->MyDisplayWidth)  w = Scr->MyDisplayWidth;
    if (h > Scr->MyDisplayHeight) h = Scr->MyDisplayHeight;

    if ((w > (Scr->MyDisplayWidth / 2)) || (h > (Scr->MyDisplayHeight / 2))) {
	int x, y;

	pixret = XCreatePixmap (dpy, Scr->Root, Scr->MyDisplayWidth, Scr->MyDisplayHeight, depth);
	x = (Scr->MyDisplayWidth  - w) / 2;
	y = (Scr->MyDisplayHeight - h) / 2;
	XFillRectangle (dpy, pixret, gc, 0, 0, Scr->MyDisplayWidth, Scr->MyDisplayHeight);
	XPutImage (dpy, pixret, gc, image, 0, 0, x, y, w, h);
	ret->width  = Scr->MyDisplayWidth;
	ret->height = Scr->MyDisplayHeight;
    }
    else {
	pixret = XCreatePixmap (dpy, Scr->Root, w, h, depth);
	XPutImage (dpy, pixret, gc, image, 0, 0, 0, 0, w, h);
	ret->width  = w;
	ret->height = h;
    }
    XFree (image);
    ret = (Image*) malloc (sizeof (struct _Image));
    ret->pixmap = pixret;
    ret->mask   = None;
    ret->next   = None;
    return (ret);

}

static void free_images (table)
TagTable *table;
{
    int		i, n;
    ImVfb	*v;
    ImClt	*c;
    TagEntry	*dataEntry;

    n = TagTableQNEntry (table, "image vfb");
    for (i = 0 ; i < n ; i++) {
	dataEntry = TagTableQDirect (table, "image vfb", i);
	TagEntryQValue (dataEntry, &v);
	ImVfbFree (v);
    }
    n = TagTableQNEntry (table, "image clt");
    for (i = 0 ; i < n ; i++) {
	dataEntry = TagTableQDirect (table, "image clt", i );
	TagEntryQValue (dataEntry, &c);
	ImCltFree (c);
    }
    TagTableFree (table);
}

#endif

_swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

_swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}
