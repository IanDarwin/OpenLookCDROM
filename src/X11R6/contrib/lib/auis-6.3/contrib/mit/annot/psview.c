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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/psview.c,v 1.17 1993/12/07 02:15:37 rr2b Exp $";
#endif


 

/*
  Still needs to be done:

  - fixed UI to refer to the point (not pixel) size, but code is
    still confused about point .vs. pixel
  - there's too much whitespace before the PostScript
  - showpages are wiped out in print and view modes
  - AskOrCancel leaves the input focus in the frameview on ^G
  - Still have a problem with dpstextview sucking up input focus
    and losing the menus....

*/

#include <andrewos.h>		/* for DPS_ENV */
#include <class.h>
#include <stdio.h>
#include <signal.h>

#include <view.ih>
#include <textv.ih>
#include <note.ih>
#include <iconview.ih>
#include <ps.ih>
#include <psview.eh>
#include <bind.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <text.ih>
#include <proctbl.ih>
#include <fontdesc.ih>
#include <style.ih>
#include <txttroff.ih>
#include <message.ih>
#include <search.ih>

#ifdef DPS_ENV
#include <dpstextv.ih>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <xgraphic.ih>
#endif /* DPS_ENV */


#define DEBUG 1
#ifdef DEBUG
#define DBG(x) fprintf(stderr, "%s\n", (x))
#else
#define DBG(x) 
#endif

#define ICONFONT "icon"
#define ICONSTYLE fontdesc_Plain
#define ICONPTS 12
#define ICONCHAR '\151'
#define TITLEFONT "andysans"
#define TITLESTYLE fontdesc_Plain
#define TITLEPTS 12

#ifdef DPS_ENV
#define EDIT_MODE 0x1
#define VIEW_MODE 0x2
#endif /* DPS_ENV */

struct menulist *psviewMenus;
static struct keymap *psviewKeyMap;

#define DisplayAndReturn(self, String) {message_DisplayString(self, 0, String); return;}

#define AskOrCancel(self, string, buf) \
	{if (message_AskForString(self, 0, string, "", buf, sizeof buf - 1) < 0) \
	{message_DisplayString(self, 0, "Cancelled."); \
	return;}}

/****************************************************************/
/*		private functions				*/
/****************************************************************/

static void
Close(v,l)
struct psview *v;
long l;
{
    psview_Close(v);
}
static void
ps_open(v,l)
struct psview *v;
long l;
{
    psview_Open(v);
}
static void
closeall(v,l)
struct view *v;
long l;
{
    iconview_CloseRelated(v);
}
static void
openall(v,l)
struct view *v;
long l;
{
    iconview_OpenRelated(v);
}

static void
insert(tv,l)
struct textview *tv;
long l;
{
    struct text *t;
    long pos;
    t = (struct text *) textview_GetDataObject(tv);
    pos = textview_GetDotPosition(tv) + textview_GetDotLength(tv);
    tv->currentViewreference = text_InsertObject(t, pos,"ps", NULL);
    text_NotifyObservers(t,0);
}

static void update_dpstextview(self)
     struct psview *self;
{
#ifdef DPS_ENV
    struct ps *psobj = (struct ps *)psview_GetDataObject(self);
    long w = ps_GetPixelWidth(psobj);
    long h = ps_GetPixelHeight(psobj);
    double dpi_x, dpi_y;
    long pix_w, pix_h;
    struct xgraphic *xgr=(struct xgraphic *)psview_GetDrawable(self);

    if (!(self->interpret) || xgr==NULL) return;
    dpi_x = xgraphic_GetHorizontalResolution(xgr);
    dpi_y = xgraphic_GetVerticalResolution(xgr);
    pix_w = w / 72.0 * dpi_x;
    pix_h = h / 72.0 * dpi_y;
    dpstextview_SetDesired((struct dpstextview *)(((struct iconview *)self)->child), pix_w, pix_h);

#endif /* DPS_ENV */

    return;
} /* update_dpstextview */

static void inchsize(self)
struct psview *self;
{
    struct ps *psobj = (struct ps *)psview_GetDataObject(self);
    long w = ps_GetPixelWidth(psobj);
    long h = ps_GetPixelHeight(psobj);
    double dpi_x, dpi_y;
    double newdw = 0.0, newdh = 0.0;
    char newxsize[75], newysize[75], request[150];

    dpi_x = 72.0;
    dpi_y = 72.0;

    /* ask for height */
    sprintf(request, "Print height %0.2f  width %0.2f in.   New height: ", h / dpi_y, w / dpi_x);
    AskOrCancel(self, request, newysize);
    if (*newysize) {
	/* height specified.  parse it and set width request */
	if (sscanf(newysize, "%lf", &newdh) != 1)		
	    DisplayAndReturn(self,
			     "Value must be digits with at most one decimal point."); }
    h = (long) (newdh * dpi_y);
    sprintf(request, "Print height %0.2f in.   New width [%0.2f] ", h / dpi_y, w / dpi_x);

    /* request new width */
    AskOrCancel(self, request, newxsize);
    if (*newxsize && sscanf(newxsize, "%lf", &newdw) != 1)
	DisplayAndReturn(self, "Value must be digits with at most one decimal point.");
    w = (long) (newdw * dpi_x);

    /* display the new size */
    sprintf(request, "Print size is now height %0.2f width %0.2f in. ", h / dpi_y, w / dpi_x );
    message_DisplayString(self, 0, request);
    ps_SetPixelWidth(psobj, w);
    ps_SetPixelHeight(psobj, h);
    update_dpstextview(self);
}

static void pixelsize(self)
struct psview *self;
{
    struct ps *psobj = (struct ps *)psview_GetDataObject(self);
    long w = ps_GetPixelWidth(psobj);
    long h = ps_GetPixelHeight(psobj);

    long newdw = 0.0, newdh = 0.0;
    char newxsize[75], newysize[75], request[150];

    /* ask for height */
    sprintf(request, "Print height %ld  width %ld points.   New height: ", h, w);
    AskOrCancel(self, request, newysize);
    if (*newysize) {
	/* height specified.  parse it and set width request */
	if (sscanf(newysize, "%ld", &newdh) != 1)		
	    DisplayAndReturn(self,
			     "Value must be digits with no decimal point."); }
    h = newdh;
    sprintf(request, "Print height %ld points.   New width [%ld] ", h, w);

    /* request new width */
    AskOrCancel(self, request, newxsize);
    if (*newxsize && sscanf(newxsize, "%ld", &newdw) != 1)
	DisplayAndReturn(self, "Value must be digits with no decimal point.");
    w = newdw;

    /* display the new size */
    sprintf(request, "Print size is now height %ld points width %ld points. ", h, w);
    message_DisplayString(self, 0, request);
    ps_SetPixelWidth(psobj, w);
    ps_SetPixelHeight(psobj, h);
    update_dpstextview(self);
}


#ifdef DPS_ENV
static void display(self, rock)
     struct psview *self;
     long rock;
{
    struct ps *psobj = (struct ps *)psview_GetDataObject(self);
    long w = ps_GetPixelWidth(psobj);
    long h = ps_GetPixelHeight(psobj);

    self->interpret = !0;

    if(menulist_SetMask(self->menus, (menulist_GetMask(self->menus) & ~EDIT_MODE) | VIEW_MODE)) {
	psview_PostMenus(self, NULL);
    }
    psview_SetChild(self, "dpstextview");
    update_dpstextview(self);
} /* display */

static void edit(self, rock)
     struct psview *self;
     long rock;
{
    struct style * ds;

    self->interpret = 0;

    if(menulist_SetMask(self->menus, (menulist_GetMask(self->menus) & ~VIEW_MODE) | EDIT_MODE)) {
	psview_PostMenus(self, NULL);
    }
    psview_SetChild(self, "textview");
    ds = textview_GetDefaultStyle((struct textview *)
				  self->header.iconview.bottomview);
    style_SetFontFamily(ds, "AndyType");
    style_SetFontSize(ds, style_ConstantFontSize, 10);

} /* edit */
#endif /* DPS_ENV */

static void autobounds(self, rock)
     struct psview *self;
     long rock;
{
    struct textview *tvobj = (struct textview *)(((struct iconview *)self)->bottomview);
    struct ps *psobj = (struct ps *)psview_GetDataObject(self);
    struct text *tobj = (struct text *)ps_GetChild(psobj);
    char *pattern = "\n%%BoundingBox:";
    char *pat = NULL;		/* compiled search pattern */
    long pos, i, c;
    char bbox_buf[200];
    long llx, lly, urx, ury;
    char *translate = "%ld %ld translate %% inserted by ps inset at the request of user to make image visible\n";
    char tr_buf[200];
    char *pattern2 = "translate % inserted by ps inset at the request of user to make image visible\n";

    /* look for "\n%%BoundingBox: llx lly urx ury\n" information */
    if (search_CompilePattern(pattern, &pat) != NULL) {
	message_DisplayString(self, 50, "should not happen:  psview could not compile search pattern.\n");
	return;
    }

    if ((pos = search_MatchPattern(tobj, 0L, pat)) < 0) {
	/* bbox not found, exit */
	message_DisplayString(self, 50, "Could not find %%BoundingBox information.");
	return;
    }

    for (i=0, ++pos; ((c = text_GetChar(tobj, pos)) > 0) && ((char)c != '\n'); ++pos, ++i) {
	bbox_buf[i] = c;
    }
    bbox_buf[i] = '\0';
    if (sscanf(bbox_buf, "%%%%BoundingBox: %d %d %d %d", &llx, &lly, &urx, &ury) != 4) {
	message_DisplayString(self, 50, "%%BoundingBox was incomplete");
	return;
    }
    /* set the print size dimensions */
    ps_SetPixelWidth(psobj, urx-llx);
    ps_SetPixelHeight(psobj, ury-lly);

    /* insert the funky translate command */
    sprintf(tr_buf, translate, -llx, -lly);
    text_AlwaysInsertCharacters(tobj, 0L, tr_buf, strlen(tr_buf)); 
    text_NotifyObservers(tobj, observable_OBJECTCHANGED);
    textview_SetDotPosition(tvobj, 0L);
    textview_SetDotLength(tvobj, strlen(tr_buf));
    textview_FrameDot(tvobj, 0L);
    textview_WantUpdate(tvobj, tvobj);

    /* did an old translate command exist?  kill it */
    pat = NULL;			/* bug:  should we do this? */
    if (search_CompilePattern(pattern2, &pat) != NULL) {
	message_DisplayString(self, 50, "should not happen:  psview could not compile second search pattern.\n");
	return;
    }

    if ((pos = search_MatchPattern(tobj, strlen(tr_buf), pat)) < 0) {
	/* old not found, okay */
	message_DisplayString(self, 50, "BBox information set.");
	return;
    }
    /* find beginning of old */
    for(i = pos; (c = text_GetChar(tobj, i)) > 0 && c != '\n'; --i){
    }
    text_AlwaysDeleteCharacters(tobj, i+1, pos+strlen(pattern2)-i-1);
    message_DisplayString(self, 50, "BBox information set (and old translate nuked).");

    return;
} /* autobounds */

static struct bind_Description psviewBindings[]={
    {"psview-set-inch-size", NULL, 0, "PostScript,Set Inch Size~10", 0, 0, inchsize, "Set print size in inches", NULL},
    {"psview-set-point-size", NULL, 0, "PostScript,Set Point Size~11", 0, 0, pixelsize, "Set print size in points", NULL},
#ifdef DPS_ENV
    {"psview-edit", NULL, 0, "PostScript,Edit~1", 0, VIEW_MODE, edit, "Edit the PostScript", NULL},
    {"psview-display", NULL, 0, "PostScript,View~1", 0, EDIT_MODE, display, "Display the PostScript as an image", NULL},
#endif /* DPS_ENV */
    {"psview-autobound", NULL, 0, "PostScript,Scan for bounds~20", 0, 0, autobounds, "Suck out the bounding box information and use it to set our bounds", NULL},
    NULL
};


void psview__PostMenus(self, menulist)
struct psview *self;
struct menulist *menulist;
{
    menulist_ClearChain(self->menus);
    if (menulist) menulist_ChainBeforeML(self->menus, menulist, menulist);
    super_PostMenus(self, self->menus);
}


/****************************************************************/
/*		class procedures				*/
/****************************************************************/

boolean
psview__InitializeClass(classID)
    struct classheader * classID;
{
    struct classinfo *textviewtype = class_Load("textview");
    struct classinfo *viewtype = class_Load("view");

    psviewMenus = menulist_New();
    psviewKeyMap =  keymap_New();

    bind_BindList(psviewBindings, psviewKeyMap , psviewMenus, &psview_classinfo);
    return TRUE;
}


boolean
psview__InitializeObject(classID,self)
struct classheader * classID;
struct psview * self;
{

    self->menus = menulist_DuplicateML(psviewMenus, self);
    self->interpret = 0;

    psview_SetIconFont(self,ICONFONT,ICONSTYLE,ICONPTS); 
    psview_SetIconChar(self,ICONCHAR);
    psview_SetTitleFont(self,TITLEFONT,TITLESTYLE,TITLEPTS);
    return TRUE;
}

void psview__FinalizeObject(classID, self)
struct classheader *classID;
struct psview *self;
{
    if(self->menus) menulist_Destroy(self->menus);
}

/****************************************************************/
/*		instance methods				*/
/****************************************************************/
void 
psview__Print(self, file, processor, format, toplevel)
register struct psview	*self;	
register FILE   *file;
register char	*processor;
register char	*format;
register boolean toplevel;
{

    static char *PSheader[] = {
	"%s  /width %d def  /height %d def /xScale %0.4f def /yScale %0.4f def\n",
	"%s xScale yScale scale\n",
	"%s /showpage {} def\n",
	NULL };
    struct ps *psobj;
    struct text *textobject;
    register char *text;
    char **psx;
    char *line = (char *)malloc(BUFSIZ);
    long c, pos = 0, textlength = 0;
    char *prefix;
    double xdscale = 1.0, ydscale = 1.0;
    long height, width;

    psobj = (struct ps *) psview_GetDataObject(self);
    width = ps_GetPixelWidth(psobj);
    height = ps_GetPixelHeight(psobj);

    textobject = (struct text *) ps_GetChild(psobj);
    textlength = text_GetLength(textobject);
    if (strcmp(processor, "troff") == 0) {
	if (toplevel)
	    texttroff_BeginDoc(file);
	/*  Put macro to interface to postscript */
	texttroff_BeginPS(file, width, height);
	if ((width != 0) && (height != 0)) {
	    fprintf(file,"\\!  newpath 0 0 moveto %d 0 lineto ", width);
	    fprintf(file,"%d %d lineto 0 %d lineto closepath clip\n\n",
		    width,height,height);
	}

	prefix = "\\!  ";
    }
    else if (strcmp(format, "troff") == 0)
	prefix = "\\!  ";
    else prefix = "";

    for (psx = PSheader; *psx; psx++)  /*see PSheader def above. */
	fprintf(file, *psx, prefix, width, height, xdscale, ydscale);

    /*print out the file with prefix in front of each line */
    while ((c = text_GetChar(textobject, pos)) != EOF &&
	    pos < textlength){
	if (pos++ == 0) fprintf(file, "%s", prefix);
	if (c == '\n') fprintf(file,"%c%s",c, prefix);
	else fputc(c, file);
    }

    if (strcmp(processor, "troff") == 0) {
	fprintf(file, "\n");
	texttroff_EndPS(file, width, height);
	if (toplevel)
	    texttroff_EndDoc(file); }


}


void
psview__SetDataObject(self,dobj)
struct psview * self;
struct dataobject * dobj;
{
    struct style * ds;
    super_SetDataObject(self,dobj);


#ifdef DPS_ENV
    if(menulist_SetMask(self->menus, (menulist_GetMask(self->menus) & ~VIEW_MODE) | EDIT_MODE)) {
	psview_PostMenus(self, NULL);
    }
#endif /* DPS_ENV */
    ds = textview_GetDefaultStyle((struct textview *)
				 self->header.iconview.bottomview);
    style_SetFontFamily(ds, "AndyType");
    style_SetFontSize(ds, style_ConstantFontSize, 10);
}
