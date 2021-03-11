/* bdffontv.ch  -  view for font editor for bdf files */

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/bdffont/RCS/bdffontv.c,v 1.12 1993/08/17 22:04:57 Zarf Exp $";
#endif

/*
	Copyright Carnegie Mellon University 1991, 1993 - All rights reserved
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

/* known problems  26 Mar, 1993 
BUGS
	while editing a character, the user is prompted to save it everytime 
		a checkpoint happens
	the initially displayed character is centered, but thereafter characters are upper left
		this is a problem in rasterv.c  (and missing methods in rasterv.ch)
	when zoomed, the initial display of a character should be with top left corner 
		at top left,  but it is not.  Again a rasterv problem.
	changing the width of the initially displayed character clears its raster (!)
		another raster problem ?
	^X^V does not refresh from the file 
	Suppose font foo12.bdf is being edited and character C has been changed, 
		but not updated.  If I then try to edit foo12.bdf in raw mode, I am 
		prompted to save the current character.  After saving it, the file
		is brought up in regular mode instead of raw mode.
	after changing pointsize (or other action), menus for raster are not reposted
		(raster is the currentinputfocus, but menus are not up ! )
		this is a bug in rasterv:PostMenus.  The mask doesn't change, so menus
		are not reposted.
	a click in the charedit raster gives it the input focus, but without calling 
		WantInputFocus of bdffontv  ?!?!

INTERFACE IMPROVEMENTS
	as the initial parameters for a new font are entered, they should be displayed
		in the relevant fields
	there ought to be a way to say "update the char" without getting 
		prompted each time
	there oughta be a way to review/edit other font parameters such as defaults for
		deltas, ascent, and descent
	it would be awfully nice to have the origin point displayed in the raster image
	should add the dash to character menu after adding a new character

BIG CHANGES
	display the characters in the char menu
	display character names in the char menu
	provide a mode for typing text in the revised font
*/


#include <bdffontv.eh>

#include <ansitext.h>
#include <mathaux.h>

#include <buffer.ih>
#include <frame.ih>
#include <im.ih>

#include <view.ih>
#include <bind.ih>
#include <keymap.ih>
#include <proctbl.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <rastimg.ih>
#include <raster.ih>
#include <rasterv.ih>

#if 0
#include <frasterv.ih>
#endif

#include <message.ih>
#include <chlist.ih>
#include <chlistv.ih>
#include <text.ih>
#include <textv.ih>
#include <lpair.ih>
#include <style.ih>
#include <fontdesc.ih>

#include <rect.h>
#include <ctype.h>
#include <txtstvec.h>

static void CreateIndexString();


#define RoundUp(x) ((long) ((x) + 0.5))

static long bdffontv_DefaultPointSize = 12;
static long bdffontv_DefaultResolution = 100;
static long bdffontv_DefaultWidth = 12;



static void EnsureFocus(self)
struct bdffontv *self;
{
#if 0
    if (self) {
	struct im *im=bdffontv_GetIM(self);
	struct view *v=NULL;
	if (im != NULL) {
	    v=im_GetInputFocus(im);
	}
	if ( ! self->chareditV) {}
/* 	else if ((struct view *)self->chareditV != v)
*/ else
		rasterview_WantInputFocus(self->chareditV, self->chareditV);
    }
#endif
}
    
static char bdffontv_EncodingLabel[] = "Index: ";
static char bdffontv_NameLabel[] = "Name: ";
static char bdffontv_DeltaXLabel[] = "dx: ";
static char bdffontv_DeltaYLabel[] = "dy: ";
static char bdffontv_OriginXLabel[] = "origin X: ";
static char bdffontv_OriginYLabel[] = "origin Y: ";
static char bdffontv_WidthLabel[] = "Width: ";
static char bdffontv_HeightLabel[] = "Height: ";
static char bdffontv_ResolutionXLabel[] = "X res: ";
static char bdffontv_ResolutionYLabel[] = "Y res: ";
static char bdffontv_PointLabel[] = "Pt size: ";
static char bdffontv_FaceLabel[] = "Face: ";
static char bdffontv_FamilyLabel[] = "Family: ";
static char bdffontv_HelpLabel[] = " HELP! ";

#define bdffontv_CharNameMod	(0x01)
#define bdffontv_CharDeltaMod	(0x02)
#define bdffontv_CharOriginMod	(0x04)
#define bdffontv_CharExtentMod	(0x08)
#define bdffontv_NewCharDefnMod	(0x0F)
#define	bdffontv_CreatedMod	(0x10)

#define bdffontv_ZoomInCmd	 (1)
#define bdffontv_ZoomOutCmd	(-1)
#define bdffontv_ZoomNormalCmd	 (2)
#define bdffontv_ZoomPreviousCmd (3)
#define bdffontv_ZoomToFitCmd	 (4)

static long bdffontv_ComputeZoom(self, zoomhow)
    struct bdffontv *self;
    long zoomhow;
{
    struct rectangle viewbds;
    long oldzoom, newzoom, zw, zh;

    oldzoom = rasterview_GetScale(self->chareditV);

    switch (zoomhow) {
	case bdffontv_ZoomNormalCmd:
	    newzoom = 1;
	    break;
	case bdffontv_ZoomPreviousCmd:
	    newzoom = self->prevzoom;
	    break;
	case bdffontv_ZoomToFitCmd:
	    rasterview_GetVisualBounds(self->chareditV, &viewbds);
	    zw = rectangle_Width(&viewbds) / raster_GetWidth(self->charedit);
	    zh = rectangle_Height(&viewbds) / raster_GetHeight(self->charedit);
	    newzoom = (zw < zh) ? zw : zh;
	    break;
	default:	
	    newzoom = oldzoom + zoomhow * self->zoomdelta;
	    break;
    }

    if (newzoom < 1) {
	newzoom = 1;
    }

    return (newzoom);
} /* bdffontv_ComputeZoom */

static void bdffontv_ZoomCmd(self	, zoomhow)
    struct bdffontv *self;
    long zoomhow;
{
    long oldzoom, newzoom;

    oldzoom = rasterview_GetScale(self->chareditV);
    newzoom = bdffontv_ComputeZoom(self, zoomhow);

    if (newzoom == oldzoom) {
	return;
    }

    if (oldzoom != 1) {
	self->prevzoom = oldzoom;
    }

    rasterview_SetScale(self->chareditV, newzoom);
} /* bdffontv_ZoomCmd */

static void bdffontv_SetZoomDeltaCmd(self, rock)
    struct bdffontv *self;
    long rock;
{
    char msg[128];
    char buffer[128], *bufp;
    long newdelta;

    sprintf(msg, "New zoom in/out delta [%d]: ", self->zoomdelta);

    if (message_AskForString(self, 0, msg, "", buffer, sizeof(buffer) - 1) < 0)
    {
	message_DisplayString(self, 0, "Cancelled.");
	return;
    }

    bufp = &buffer[0];
    while (isspace(*bufp)) {
	bufp++;
    }

    if (*bufp != '\0') {
	if (sscanf(buffer, "%d", &newdelta) != 1) {
	    message_DisplayString(self, 0, "Value must be a number.");
	    return;
	}
	self->zoomdelta = newdelta;

	sprintf(msg, "New display delta set to %d.", self->zoomdelta);
	message_DisplayString(self, 0, msg);
    }
    else {
	message_DisplayString(self, 0, "No change to display delta.");
    }
} /* bdffontv_SetZoomDeltaCmd */

static void bdffontv_ForwardCmd(self, proc)
    struct bdffontv *self;
    procedure proc;
{
    (*proc)(self, NULL, view_LeftUp, 1, 0, 0);
} /* bdffontv_ForwardCmd */

#define bdffontv_AskAborted	(0)
#define bdffontv_AskUnchanged	(1)
#define bdffontv_AskChanged	(2)

static int AskFor1(self, value, prompt)
    struct bdffontv *self;
    long *value;
    char *prompt;
{
    char msg[128];
    char buffer[128], *bufp;

    sprintf(msg, "%s [%d]: ", prompt, *value);

    if (message_AskForString(self, 0, msg, "", buffer, sizeof(buffer) - 1) < 0)
    {
	message_DisplayString(self, 0, "Cancelled.");
	return (bdffontv_AskAborted);
    }

    bufp = &buffer[0];
    while (isspace(*bufp)) {
	bufp++;
    }

    if (*bufp != '\0') {
	if (sscanf(buffer, "%d", value) != 1) {
	    message_DisplayString(self, 0, "Value must be a number.");
	    return (bdffontv_AskAborted);
	}

	return (bdffontv_AskChanged);
    }

    return (bdffontv_AskUnchanged);
} /* AskFor1 */

static int AskFor2(self, value1, value2, prompt1, prompt2)
    struct bdffontv *self;
    long *value1, *value2;
    char *prompt1, *prompt2;
{
    switch (AskFor1(self, value1, prompt1)) {
	case bdffontv_AskAborted:
	    return (bdffontv_AskAborted);

	case bdffontv_AskUnchanged:
	    return (AskFor1(self, value2, prompt2));

	case bdffontv_AskChanged:
	    if (AskFor1(self, value2, prompt2) == bdffontv_AskAborted) {
		return (bdffontv_AskAborted);
	    }
	    return (bdffontv_AskChanged);
    }
} /* AskFor2 */

static char *bdffontv_FaceChoices[] =
      { "Cancel",
	"Normal",
	"Bold",
	"Italic",
	"Bold Italic",
	"Fixed",
	"Bold Fixed",
	"Italic Fixed",
	"Bold Italic Fixed",
	"Shadowed",
	"Bold Shadowed",
	"Italic Shadowed",
	"Bold Italic Shadowed",
	"Fixed Shadowed",
	"Bold Fixed Shadowed",
	"Italic Fixed Shadowed",
	"Bold Italic Fixed Shadowed",
	NULL };

static boolean FindBuffer(f,b)
    struct frame *f;
    struct buffer *b;
{
/*
  Little, dippy routine passed to frame_Enumerate to find the
  frame which contains the buffer we want.
*/

  return (frame_GetBuffer(f) == b);
} /* FindBuffer */

static struct view *FindViewOfBuffer(b)
    struct buffer *b;
{
/*
  I don't know why *I* have to do this, it should be a buffer method.
  Anyways, this tries to find the frame of our buffer.  If there is no
  such frame, make one, make a new IM for it (new window), and put the
  buffer in the frame in the IM.  *phew*

	(he probably should have used frame_GetFrameInWindowForBuffer  -wjh)
*/

  struct frame *f;
  struct im *im;

  if ((f = frame_Enumerate(FindBuffer, b)) == NULL) {
    /* No frame--need to map buffer to new window */

    if ((f = frame_New()) == NULL) {
	fprintf(stderr,"bdffontv: Could not create new frame.\n");
	return ((struct view *) NULL);
    }
    if ((im = im_Create(NULL)) == NULL) {
	fprintf(stderr,"bdffontv: Could not create new window.\n");
	frame_Destroy(f);
	return ((struct view *) NULL);
    }
    im_SetView(im, f);

    frame_SetCommandEnable(f, TRUE);
    frame_PostDefaultHandler(f, "message", frame_WantHandler(f, "message"));
    frame_SetBuffer(f, b, TRUE);
  }
  return (frame_GetView(f));
} /* FindViewOfBuffer */

static int bdffontv_HelpCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    struct buffer *buf;
    struct view *v;

    buf = buffer_GetBufferOnFile(AndrewDir("/help/bdffont.help"), 0); 
    if (buf == NULL) {
	message_DisplayString(self, 0, "Could not find help text file.");
    }
    else {
	v = FindViewOfBuffer(buf);
	if (v == NULL) {
	    message_DisplayString(self, 0, "Could not create help window.");
	}
	else {
	    im_ExposeWindow(view_GetIM(v));
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_HelpCmd */

static int bdffontv_FontExtentCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    message_DisplayString(self,
			  0,
			  "Cannot change font extent summary!");

    EnsureFocus(self);

    return (0);
} /* bdffontv_FontExtentCmd */

static int bdffontv_SetFontFaceCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    struct chlist *chl;
    char msg[128];
    int face, newface;
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	face = bdffont_GetFontFace(fontinfo) + 1;
	if (message_MultipleChoiceQuestion(self,
					   0,
					   "Select font face:",
					   face,
					   &newface,
					   bdffontv_FaceChoices,
					   NULL) != -1)
	{
	    if ((newface != face) && (newface != 0 /* i.e. cancel */)) {
		bdffont_SetFontFace(fontinfo, newface - 1);
		bdffont_SetModified(fontinfo);

		chl =
		   (struct chlist *) chlistview_GetDataObject(self->fontfaceV);
		sprintf(msg,
			"%s%s",
			bdffontv_FaceLabel, bdffontv_FaceChoices[newface]);
		chlist_ChangeItemByIndex(chl, 0, msg);

		sprintf(msg,
			"New font face = %s.",
			bdffontv_FaceChoices[newface]);
		message_DisplayString(self, 0, msg);
	    }
	    else {
		message_DisplayString(self, 0, "No change to font face.");
	    }
	}
	else {
	    message_DisplayString(self, 0, "No change to font face.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetFontFaceCmd */

static int bdffontv_SetResolutionCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long x, y;
    struct chlist *chl;
    char msg[128];
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	bdffont_GetResolution(fontinfo, &x, &y);
	if (AskFor2(self,
		    &x, &y,
		    "New X resolution",
		    "New Y resolution") == bdffontv_AskChanged)
	{
	    bdffont_SetResolution(fontinfo, x, y);
	    bdffont_SetModified(fontinfo);

	    chl = (struct chlist *) chlistview_GetDataObject(self->resV);
	    sprintf(msg, "%s%d", bdffontv_ResolutionXLabel, x);
	    chlist_ChangeItemByIndex(chl, 0, msg);

	    sprintf(msg, "%s%d", bdffontv_ResolutionYLabel, y);
	    chlist_ChangeItemByIndex(chl, 1, msg);

	    sprintf(msg,
		    "New X resolution = %d, new Y resolution = %d.", x, y);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to font resolution.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetResolutionCmd */

static int bdffontv_SetPointSizeCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long pts;
    struct chlist *chl;
    char msg[128];
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	pts = bdffont_GetPointSize(fontinfo);
	if (AskFor1(self, &pts, "New point size") == bdffontv_AskChanged) {
	    bdffont_SetPointSize(fontinfo, pts);
	    bdffont_SetModified(fontinfo);

	    chl = (struct chlist *) chlistview_GetDataObject(self->pthelpV);
	    sprintf(msg, "%s%d", bdffontv_PointLabel, pts);
	    chlist_ChangeItemByIndex(chl, 0, msg);

	    sprintf(msg, "New point size = %d.", pts);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to font point size.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetPointSizeCmd */

static int bdffontv_SetDefaultDeltaCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long x, y;
    char msg[128];
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	bdffont_GetDefaultDelta(fontinfo, &x, &y);
	if (AskFor2(self,
		    &x, &y,
		    "New default X delta",
		    "New default Y delta") == bdffontv_AskChanged)
	{
	    bdffont_SetDefaultDelta(fontinfo, x, y);
	    bdffont_SetModified(fontinfo);

	    sprintf(msg,
		    "New default X delta = %d, new default Y delta = %d.",
		    x, y);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to default delta.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetDefaultDeltaCmd */

static int bdffontv_SetDefaultOriginCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long x, y;
    char msg[128];
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	bdffont_GetDefaultOrigin(fontinfo, &x, &y);
	if (AskFor2(self,
		    &x, &y,
		    "New default X origin",
		    "New default Y origin") == bdffontv_AskChanged)
	{
	    bdffont_SetDefaultOrigin(fontinfo, x, y);
	    bdffont_SetModified(fontinfo);

	    sprintf(msg,
		    "New default X origin = %d, new default Y origin = %d.",
		    x, y);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to default origin.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetDefaultOriginCmd */

static int bdffontv_SetDefaultExtentCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long width, height;
    char msg[128];
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	bdffont_GetDefaultExtent(fontinfo, &width, &height);
	if (AskFor2(self,
		    &width, &height,
		    "New default width",
		    "New default height") == bdffontv_AskChanged)
	{
	    bdffont_SetDefaultExtent(fontinfo, width, height);
	    bdffont_SetModified(fontinfo);

	    sprintf(msg,
		    "New default width = %d, new default height = %d.",
		    width, height);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to default extent.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetDefaultExtentCmd */

static int bdffontv_SetFontNameCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    char buffer[512], *bufp, *tail, *oldname;
    struct chlist *chl;
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	oldname = bdffont_GetFontName(fontinfo);

	if (message_AskForString(self, 0,
				 "New name: ",
				 oldname,
				 buffer, sizeof(buffer) - 1) < 0)
	{
	    message_DisplayString(self, 0, "Cancelled.");
	}
	else {
	    /* trim leading white space */
	    bufp = &buffer[0];
	    while (isspace(*bufp)) {
		bufp++;
	    }

	    /* trim trailing white space */
	    tail = bufp + strlen(bufp) - 1;
	    while ((tail > bufp) && isspace(*tail)) {
		*tail-- = '\0';
	    }

/* ... no embedded spaces??? */

	    if ((*bufp != '\0') && ( ! oldname || (strcmp(oldname, bufp) != 0))) {
		bdffont_SetFontName(fontinfo, bufp);
		bdffont_SetModified(fontinfo);

		chl = (struct chlist *)
			chlistview_GetDataObject(self->fontnameV);
		chlist_ChangeItemByIndex(chl, 0, bdffont_GetFontName(fontinfo));

		sprintf(buffer,
			"New font name set to %s.",
			bdffont_GetFontName(fontinfo));
		message_DisplayString(self, 0, buffer);
	    }
	    else {
		message_DisplayString(self, 0, "No change to font name.");
	    }
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetFontNameCmd */

static int bdffontv_SetFontFamilyCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    char buffer[512], *bufp, *tail, *oldfamily;
    struct chlist *chl;
    struct bdffont *fontinfo;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	oldfamily = bdffont_GetFontFamily(fontinfo);

	if (message_AskForString(self, 0,
				 "New font family: ",
				 oldfamily,
				 buffer, sizeof(buffer) - 1) < 0)
	{
	    message_DisplayString(self, 0, "Cancelled.");
	}
	else {
	    /* trim leading white space */
	    bufp = &buffer[0];
	    while (isspace(*bufp)) {
		bufp++;
	    }

	    /* trim trailing white space */
	    tail = bufp + strlen(bufp) - 1;
	    while ((tail > bufp) && isspace(*tail)) {
		*tail-- = '\0';
	    }

/* ... no embedded spaces??? */

	    if ((*bufp != '\0') && ( ! oldfamily || (strcmp(oldfamily, bufp) != 0))) {
		bdffont_SetFontFamily(fontinfo, bufp);
		bdffont_SetModified(fontinfo);

		chl = (struct chlist *)
			chlistview_GetDataObject(self->fontfamilyV);
		sprintf(buffer,
			"%s%s",
			bdffontv_FamilyLabel,
			bdffont_GetFontFamily(fontinfo));
		chlist_ChangeItemByIndex(chl, 0, buffer);

		sprintf(buffer,
			"New font family set to %s.",
			bdffont_GetFontFamily(fontinfo));
		message_DisplayString(self, 0, buffer);
	    }
	    else {
		message_DisplayString(self, 0, "No change to font family.");
	    }
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetFontFamilyCmd */

static int bdffontv_SetDeltaCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long x, y;
    struct chlist *chl;
    char msg[128];

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	bdffont_GetDWidth(&self->modinfo, &x, &y);
	if (AskFor2(self,
		    &x, &y,
		    "New character X delta",
		    "New character Y delta") == bdffontv_AskChanged)
	{
	    bdffont_SetDWidth(&self->modinfo, x, y);
	    self->charmodified |= bdffontv_CharDeltaMod;

	    chl = (struct chlist *) chlistview_GetDataObject(self->charencodingV);
	    sprintf(msg, "%s%d", bdffontv_DeltaXLabel, x);
	    chlist_ChangeItemByIndex(chl, 2, msg);

	    sprintf(msg, "%s%d", bdffontv_DeltaYLabel, y);
	    chlist_ChangeItemByIndex(chl, 3, msg);

	    sprintf(msg, "New X delta = %d, new Y delta = %d.", x, y);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to character delta.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetDeltaCmd */

static int bdffontv_SetOriginCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long x, y;
    struct chlist *chl;
    char msg[128];

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	bdffont_GetOrigin(&self->modinfo, &x, &y);
	if (AskFor2(self,
		    &x, &y,
		    "New character X origin",
		    "New character Y origin") == bdffontv_AskChanged)
	{
	    bdffont_SetOrigin(&self->modinfo, x, y);
	    self->charmodified |= bdffontv_CharOriginMod;

	    chl = (struct chlist *) chlistview_GetDataObject(self->charextentV);
	    sprintf(msg, "%s%d", bdffontv_OriginXLabel, x);
	    chlist_ChangeItemByIndex(chl, 0, msg);

	    sprintf(msg, "%s%d", bdffontv_OriginYLabel, y);
	    chlist_ChangeItemByIndex(chl, 1, msg);

	    sprintf(msg, "New X origin = %d, new Y origin = %d.", x, y);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to character origin.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetOriginCmd */

static int bdffontv_SetExtentCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    long width, height;
    struct chlist *chl;
    char msg[128];

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	bdffont_GetExtent(&self->modinfo, &width, &height);
	if (AskFor2(self,
		    &width, &height,
		    "New character width",
		    "New character height") == bdffontv_AskChanged)
	{
	    bdffont_SetExtent(&self->modinfo, width, height);
	    self->charmodified |= bdffontv_CharExtentMod;

	    chl = (struct chlist *) chlistview_GetDataObject(self->charextentV);
	    sprintf(msg, "%s%d", bdffontv_WidthLabel, width);
	    chlist_ChangeItemByIndex(chl, 2, msg);

	    sprintf(msg, "%s%d", bdffontv_HeightLabel, height);
	    chlist_ChangeItemByIndex(chl, 3, msg);

	    rasterview_ResizeRaster(self->chareditV, width, height);

	    sprintf(msg, "New width = %d, new height = %d.", width, height);
	    message_DisplayString(self, 0, msg);
	}
	else {
	    message_DisplayString(self, 0, "No change to character extent.");
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetExtentCmd */

static int bdffontv_SetNameCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    char msg[128];
    char buffer[128], *bufp, *tail;
    struct chlist *chl;

    if ((action == view_LeftUp) || (action == view_RightUp)) {
	sprintf(msg,
		"New name for character (max. length: 14) [%s]: ",
		bdffont_GetCharName(&self->modinfo));

	if (message_AskForString(self, 0, msg, "", buffer, sizeof(buffer) - 1) < 0)
	{
	    message_DisplayString(self, 0, "Cancelled.");
	}
	else {
	    /* trim leading white space */
	    bufp = &buffer[0];
	    while (isspace(*bufp)) {
		bufp++;
	    }

	    /* trim trailing white space */
	    tail = bufp + strlen(bufp) - 1;
	    while ((tail > bufp) && isspace(*tail)) {
		*tail-- = '\0';
	    }

	    /* check characters */
	    for (tail = bufp;  bufp && *tail; tail++) 
		if ( ! isprint(*tail) || isspace(*tail))
			bufp = NULL;

	    if ( ! bufp) {
		message_DisplayString(self, 0, "Names must be printable chars.");
	    }
	    else if (*bufp != '\0') {
		bdffont_SetCharName(&self->modinfo, bufp);

		self->charmodified |= bdffontv_CharNameMod;

		chl = (struct chlist *)
			chlistview_GetDataObject(self->charencodingV);
		sprintf(msg,
			"%s%s",
			bdffontv_NameLabel,
			bdffont_GetCharName(&self->modinfo));
		chlist_ChangeItemByIndex(chl, 1, msg);

		sprintf(msg,
			"New character name set to %s.",
			bdffont_GetCharName(&self->modinfo));
		message_DisplayString(self, 0, msg);
	    }
	    else {
		message_DisplayString(self, 0, "No change to character name.");
	    }
	}
    }

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetNameCmd */

static int bdffontv_SetEncodingCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    message_DisplayString(self,
			  0,
			  "Please use menu at left to select character to edit.");

    EnsureFocus(self);

    return (0);
} /* bdffontv_SetEncodingCmd */

static struct bind_Description bdffontv_Bindings[] =
{
/*  {"external-name", "key", KeyRock,
	"menu,entry~11", MenuRock, MenuMask,
	xyzzy_BindingProc, "One line description"}, */

    {"bdffont-set-font-name", NULL, 0,
	"Font Edit,Set Font Name~29", (long) bdffontv_SetFontNameCmd, 0,
	bdffontv_ForwardCmd, "Set font name"},
    {"bdffont-set-font-family", NULL, 0,
	"Font Edit,Set Font Family~29", (long) bdffontv_SetFontFamilyCmd, 0,
	bdffontv_ForwardCmd, "Set font family"},
    {"bdffont-set-font-face", NULL, 0,
	"Font Edit,Set Font Face~29", (long) bdffontv_SetFontFaceCmd, 0,
	bdffontv_ForwardCmd, "Set font face"},
    {"bdffont-set-font-resolution", NULL, 0,
	"Font Edit,Set Font Resolution~29", (long) bdffontv_SetResolutionCmd, 0,
	bdffontv_ForwardCmd, "Set font resolution"},
    {"bdffont-set-font-size", NULL, 0,
	"Font Edit,Set Point Size~29", (long) bdffontv_SetPointSizeCmd, 0,
	bdffontv_ForwardCmd, "Set font point size"},
    {"bdffont-set-default-extent", NULL, 0,
	"Font Edit,Set Default Extent~29", (long) bdffontv_SetDefaultExtentCmd, 0,
	bdffontv_ForwardCmd, "Set default character extent"},
    {"bdffont-set-default-origin", NULL, 0,
	"Font Edit,Set Default Origin~29", (long) bdffontv_SetDefaultOriginCmd, 0,
	bdffontv_ForwardCmd, "Set default character origin"},
    {"bdffont-set-default-delta", NULL, 0,
	"Font Edit,Set Default Delta~29", (long) bdffontv_SetDefaultDeltaCmd, 0,
	bdffontv_ForwardCmd, "Set default character delta"},
    {"bdffont-help", NULL, 0,
	"Font Edit,Help~10", (long) bdffontv_HelpCmd, 0,
	bdffontv_ForwardCmd, "Display help information"},

    {"bdffont-zoom-in", NULL, bdffontv_ZoomInCmd,
	"Char Edit,Zoom In~21", bdffontv_ZoomInCmd, 0,
	bdffontv_ZoomCmd, "Increase zoom factor by delta to max"},
    {"bdffont-zoom-out", NULL, bdffontv_ZoomOutCmd,
	"Char Edit,Zoom Out~22", bdffontv_ZoomOutCmd, 0,
	bdffontv_ZoomCmd, "Decrease zoom factor by delta to min"},
    {"bdffont-zoom-normal", NULL, bdffontv_ZoomNormalCmd,
	"Char Edit,Normal Zoom~23", bdffontv_ZoomNormalCmd, 0,
	bdffontv_ZoomCmd, "View at normal zoom"},
    {"bdffont-zoom-previous", NULL, bdffontv_ZoomPreviousCmd,
	"Char Edit,Previous Zoom~25", bdffontv_ZoomPreviousCmd, 0,
	bdffontv_ZoomCmd, "View at previous zoom factor"},
    {"bdffont-zoom-fit", NULL, bdffontv_ZoomToFitCmd,
	"Char Edit,Zoom To Fit~24", bdffontv_ZoomToFitCmd, 0,
	bdffontv_ZoomCmd, "View at highest zoom that fits"},
    {"bdffont-set-zoom-delta", NULL, 0,
	"Char Edit,Set Zoom Delta~26", 0, 0,
	bdffontv_SetZoomDeltaCmd, "Set delta for zoom in/out"},

    {"bdffont-set-char-extent", NULL, 0,
	"Char Edit,Set Character Extent~11", (long) bdffontv_SetExtentCmd, 0,
	bdffontv_ForwardCmd, "Set character extent"},
    {"bdffont-set-char-origin", NULL, 0,
	"Char Edit,Set Character Origin~12", (long) bdffontv_SetOriginCmd, 0,
	bdffontv_ForwardCmd, "Set character origin"},
    {"bdffont-set-char-delta", NULL, 0,
	"Char Edit,Set Character Delta~13", (long) bdffontv_SetDeltaCmd, 0,
	bdffontv_ForwardCmd, "Set character delta"},
    {"bdffont-set-char-name", NULL, 0,
	"Char Edit,Set Character Name~14", (long) bdffontv_SetNameCmd, 0,
	bdffontv_ForwardCmd, "Set character name"},

    { NULL }
};

static struct keymap *keys;
static struct menulist *menus;
static struct style *fieldstyle;
static struct style *valuestyle;
static struct style *fontnamestyle;
static struct style *helpstyle;

boolean bdffontv__InitializeClass(c)
    struct classheader *c;
{
    keys = keymap_New();
    menus = menulist_New();

    bind_BindList(bdffontv_Bindings, keys, menus, &bdffontv_classinfo);

    fieldstyle = style_New();
    style_AddNewFontFace(fieldstyle, fontdesc_Italic);

    valuestyle = style_New();
    style_AddNewFontFace(valuestyle, fontdesc_Plain);

    fontnamestyle = style_New();
    style_AddNewFontFace(fontnamestyle, fontdesc_Bold);

    helpstyle = style_New();
    style_AddNewFontFace(helpstyle, fontdesc_Bold);
    style_AddNewFontFace(helpstyle, fontdesc_Outline);
    style_SetJustification(helpstyle, style_Centered);

    return (TRUE);
} /* bdffontv__InitializeClass */


static void bdffontv_UpdateFontExtent(self, fontinfo)
    struct bdffontv *self;
    struct bdffont *fontinfo;
{
    struct chlist *chl;
    char msg[512];
    long x, y, w, h;

    bdffont_GetBoundingBox(fontinfo, &w, &h, &x, &y);
    chl = (struct chlist *) chlistview_GetDataObject(self->fontoriginV);
    sprintf(msg, "%s%d", bdffontv_OriginXLabel, x);
    chlist_ChangeItemByIndex(chl, 0, msg);
    sprintf(msg, "%s%d", bdffontv_OriginYLabel, y);
    chlist_ChangeItemByIndex(chl, 1, msg);

    chl = (struct chlist *) chlistview_GetDataObject(self->fontextentV);
    sprintf(msg, "%s%d", bdffontv_WidthLabel, w);
    chlist_ChangeItemByIndex(chl, 0, msg);
    sprintf(msg, "%s%d", bdffontv_HeightLabel, h);
    chlist_ChangeItemByIndex(chl, 1, msg);
} /* bdffontv_UpdateFontExtent */

static char bdffontv_UpdateConfirm[] = "Retain the changes you made to";

static char *bdffontv_UpdateChoices[] = { "Yes", "No", "Cancel", NULL };

#define bdffontv_UpdateYes	0
#define bdffontv_UpdateNo	1
#define bdffontv_UpdateCancel	2

/* FALSE means cancel, TRUE means continue */
boolean bdffontv__UpdateCharModification(self)
    struct bdffontv *self;
{
    char buf[100];
    long choice;
    struct bdffont *fontinfo;
    long x, y;
    struct rasterimage *pix;

    /* put up update dialog if anything has been modified, unless the only change is having created the character. */
    if ((self->charmodified!=0 && self->charmodified!=bdffontv_CreatedMod)
	 || (self->rastermodified != raster_GetModified(self->charedit)))
    {
	sprintf(buf, "%s '%s'?", 
		bdffontv_UpdateConfirm, bdffont_GetCharName(&self->modinfo));
	if (-1 == message_MultipleChoiceQuestion(self, 0, buf, bdffontv_UpdateYes, &choice, bdffontv_UpdateChoices, NULL))
	{
	    return (FALSE);
	}

	if (choice == bdffontv_UpdateCancel) {
	    return (FALSE);
	}

	if (choice == bdffontv_UpdateYes) {
	    /* if character was just created, all fields have been updated */
	    if (self->charmodified & bdffontv_CreatedMod)
		self->charmodified = bdffontv_NewCharDefnMod;

	    fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	    bdffont_SetModified(fontinfo);
   
	    if (! bdffont_IsActive(self->charinfo)) {
		bdffont_ModifyActiveDefnsBy(fontinfo, 1);
		bdffont_SetBitmap(self->charinfo,
				  malloc(bdffont_BitmapSize(&self->modinfo)));
	    }

	    if (self->charmodified & bdffontv_CharNameMod) {
		bdffont_SetCharName(self->charinfo,
				    bdffont_GetCharName(&self->modinfo));
	    }

	    if (self->charmodified & bdffontv_CharDeltaMod) {
		bdffont_GetDWidth(&self->modinfo, &x, &y);
		bdffont_SetCharDWidth(fontinfo,
				      bdffont_GetCharEncoding(self->charinfo),
				      x, y);
	    }

	    if (self->charmodified & bdffontv_CharOriginMod) {
		bdffont_GetOrigin(&self->modinfo, &x, &y);
		bdffont_SetCharOrigin(fontinfo,
				      bdffont_GetCharEncoding(self->charinfo),
				      x, y);
		bdffontv_UpdateFontExtent(self, fontinfo);
	    }

	    if (self->charmodified & bdffontv_CharExtentMod) {
		bdffont_GetExtent(&self->modinfo, &x, &y);
		bdffont_SetCharExtent(fontinfo,
				      bdffont_GetCharEncoding(self->charinfo),
				      x, y);
		bdffontv_UpdateFontExtent(self, fontinfo);
	    }

	    if (self->rastermodified != raster_GetModified(self->charedit)) {
		pix = raster_GetPix((struct raster *) self->charedit);
		memcpy(bdffont_GetBitmap(self->charinfo),
		       rasterimage_GetBitsPtr(pix),
		       bdffont_BitmapSize(self->charinfo));
		self->rastermodified = raster_GetModified(self->charedit);

/* ... some font manipulation here! */
		chlistview_WantUpdate(self->charmenuV, self->charmenuV);
	    }
	}
    }

    return (TRUE);
} /* bdffontv__UpdateCharModification */ 

void bdffontv__SelectCharacter(self, index)
    struct bdffontv *self;
    int index;
{
    struct bdffont *fontinfo;
    long x, y, w, h, zoom;
    struct chlist *encodingl, *extentl;
    char msg[128];
    struct rasterimage *pix;

    if (bdffontv_UpdateCharModification(self)) {
	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);

	self->charinfo = bdffont_GetDefinition(fontinfo, index);

	encodingl =
	    (struct chlist *) chlistview_GetDataObject(self->charencodingV);
	extentl =
	    (struct chlist *) chlistview_GetDataObject(self->charextentV);

	strcpy(msg, bdffontv_EncodingLabel);
	CreateIndexString(msg + strlen(bdffontv_EncodingLabel), index);

	chlist_ChangeItemByIndex(encodingl, 0, msg);

	if (! bdffont_IsActive(self->charinfo)) {
		/* creating a new character */

	    self->charmodified = bdffontv_CreatedMod;

	    /* generate name in usual style  (sp, char, or ch-ddd) */
	    if (index == 32)
		strcpy(msg, "sp");
	    else if (isprint((char) index)) {
		sprintf(msg, "%c", (char) index);
	    }
	    else {
		sprintf(msg, "ch-%d", index);
	    }
	    bdffont_SetCharName(&self->modinfo, msg);

	    bdffont_GetDefaultExtent(fontinfo, &w, &h);
	    bdffont_SetExtent(&self->modinfo, w, h);

	    bdffont_GetDefaultOrigin(fontinfo, &x, &y);
	    bdffont_SetOrigin(&self->modinfo, 0, 0);

	    bdffont_GetDefaultDelta(fontinfo, &x, &y);
	    bdffont_SetSWidth(&self->modinfo, 0, 0); /* doesn't get used */
	    bdffont_SetDWidth(&self->modinfo, x, y);

	    bdffont_SetAttrs(&self->modinfo, 0);
	}
	else {
	    self->charmodified = 0;
	    self->modinfo = *self->charinfo;
	}

	sprintf(msg,
		"%s%s",
		bdffontv_NameLabel,
		bdffont_GetCharName(&self->modinfo));
	chlist_ChangeItemByIndex(encodingl, 1, msg);

	bdffont_GetDWidth(&self->modinfo, &x, &y);
	/* ... frasterv_SetDelta(self->chareditV, x, y); */
	sprintf(msg, "%s%d", bdffontv_DeltaXLabel, x);
	chlist_ChangeItemByIndex(encodingl, 2, msg);

	sprintf(msg, "%s%d", bdffontv_DeltaYLabel, y);
	chlist_ChangeItemByIndex(encodingl, 3, msg);

	bdffont_GetOrigin(&self->modinfo, &x, &y);
	/* ... frasterv_SetOrigin(self->chareditV, x, y); */
	sprintf(msg, "%s%d", bdffontv_OriginXLabel, x);
	chlist_ChangeItemByIndex(extentl, 0, msg);

	sprintf(msg, "%s%d", bdffontv_OriginYLabel, y);
	chlist_ChangeItemByIndex(extentl, 1, msg);

	bdffont_GetExtent(&self->modinfo, &w, &h);
	/* ... frasterv_ResizeRaster(self->chareditV, w, h); */
	/* ... */ rasterview_ResizeRaster(self->chareditV, w, h);

	pix = raster_GetPix(self->charedit);
	rasterimage_Clear(pix);
	if (bdffont_IsActive(self->charinfo)) {
	    memcpy(rasterimage_GetBitsPtr(pix),
		   bdffont_GetBitmap(self->charinfo),
		   bdffont_BitmapSize(self->charinfo));
	}
	rasterimage_NotifyObservers(pix, raster_BITSCHANGED);
	self->rastermodified = raster_GetModified(self->charedit);

	/* ... char info is never NULL. need boolean once zoomtofit works */
	if (self->charinfo != NULL) {
	    zoom = rasterview_GetScale(self->chareditV);
	}
	else {
	    zoom = bdffontv_ComputeZoom(self, bdffontv_ZoomToFitCmd);
	}
	rasterview_SetScale(self->chareditV, zoom);

	sprintf(msg, "%s%d", bdffontv_WidthLabel, w);
	chlist_ChangeItemByIndex(extentl, 2, msg);

	sprintf(msg, "%s%d", bdffontv_HeightLabel, h);
	chlist_ChangeItemByIndex(extentl, 3, msg);

	chlistview_HighlightItem(self->charmenuV, index);	

	bdffontv_WantUpdate(self, self);	/* ... need this? */
    }
} /* bdffontv__SelectCharacter */

static int bdffontv_SelectCharCmd(self, l, action, nclicks, index, rgn)
    struct bdffontv *self;
    struct chlist *l;
    enum view_MouseAction action;
    long nclicks;
    int index;
    int rgn;
{
    if ((action != view_LeftUp) && (action != view_RightUp)) {
	return (0);
    }

    bdffontv_SelectCharacter(self, index);

    EnsureFocus(self);

    return (0);
} /* bdffontv_SelectCharCmd */

static void CreateIndexString(menustr, menuval)
char *menustr;
int menuval;
{
    if (menuval==32)
	strcpy(menustr, "space");
    else if (menuval>=512)
	sprintf(menustr, "\\%06o", menuval);
    else if (menuval>=256)
	sprintf(menustr, "\\%03o", menuval);
    else if (menuval>=128 || isprint(menuval))
	sprintf(menustr, "%c", menuval);
    else
	sprintf(menustr, "\\%03o", menuval);
}

static void EnsureCharMenu(self, num)
struct bdffontv *self;
long num;
{
    char menustr[25];
    int menuval;
    struct bdffont_fontchar *fc;
    struct bdffont *font = (struct bdffont *)bdffontv_GetDataObject(self);

    /* we display all control characters in \0xx form, because they would probably look bad in the charmenuV. If your default text font has glyphs for chars 0..31, you don't get to see them. Tough. Doing it correctly would involve checking fontdesc_ValidChar, I think. */

    chlist_Clear(self->charmenuL);
    for (menuval=0; menuval < num; menuval++) {
	CreateIndexString(menustr, menuval);
	if (font 
			&& (fc=bdffont_GetDefinition(font, menuval)) 
			&&  bdffont_IsActive(fc))
		strcat(menustr, " -");
	chlist_AddItemToEnd(self->charmenuL, menustr, bdffontv_SelectCharCmd, self);
    } 
    self->defns_size = num;
}

static void SetFontCharacteristics(self)
    struct bdffontv *self;
{
    struct bdffont *fontinfo;
    struct chlist *chl;
    char msg[512];
    long x, y;
    char *fam;

    fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
    fam = bdffont_GetFontFamily(fontinfo);

    chl = (struct chlist *) chlistview_GetDataObject(self->fontnameV);
    chlist_ChangeItemByIndex(chl, 0, bdffont_GetFontName(fontinfo));

    chl = (struct chlist *) chlistview_GetDataObject(self->fontfamilyV);
    sprintf(msg,
	    "%s%s",
	    bdffontv_FamilyLabel, (fam) ? fam : "");
    chlist_ChangeItemByIndex(chl, 0, msg);

    chl = (struct chlist *) chlistview_GetDataObject(self->fontfaceV);
    sprintf(msg,
	    "%s%s",
	    bdffontv_FaceLabel,
	    bdffontv_FaceChoices[bdffont_GetFontFace(fontinfo) + 1]);
    chlist_ChangeItemByIndex(chl, 0, msg);

    chl = (struct chlist *) chlistview_GetDataObject(self->pthelpV);
    sprintf(msg, "%s%d", bdffontv_PointLabel, bdffont_GetPointSize(fontinfo));
    chlist_ChangeItemByIndex(chl, 0, msg);

    bdffont_GetResolution(fontinfo, &x, &y);
    chl = (struct chlist *) chlistview_GetDataObject(self->resV);
    sprintf(msg, "%s%d", bdffontv_ResolutionXLabel, x);
    chlist_ChangeItemByIndex(chl, 0, msg);
    sprintf(msg, "%s%d", bdffontv_ResolutionYLabel, y);
    chlist_ChangeItemByIndex(chl, 1, msg);

    bdffontv_UpdateFontExtent(self, fontinfo);

    x = bdffont_GetDefaultChar(fontinfo);
    if (x<0) x = 0;
    if (x>=fontinfo->defns_size) x = fontinfo->defns_size;
    bdffontv_SelectCharacter(self, x);

    EnsureCharMenu(self, fontinfo->defns_size);

} /* SetFontCharacteristics */

	static void
GetNewFontParameters(self, time)
	struct bdffontv *self;
	long time;	/* ignored */
{
	struct bdffont *fontinfo;
	long ptsize, res, dwidth, dheight, descent;
	double size;
	char name[1024], *newname;

	fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
	if (message_AskForString(self, 100,
				 "Name for NEW font: ", "",
				 name, sizeof(name) - 1) >= 0)
	{
	    /* trim trailing white space */
	    newname = &name[sizeof(name) - 1];
	    while ((newname > &name[0]) && isspace(*newname)) {
		*newname-- = '\0';
	    }

	    /* trim leading white space */
	    newname = &name[0];
	    while (isspace(*newname)) {
		newname++;
	    }

/* ... no embedded spaces??? */

	    if (*newname != '\0') {
		bdffont_SetFontName(fontinfo, newname);
	    }
	}
	ptsize = bdffontv_DefaultPointSize;
	if (AskFor1(self,
		    &ptsize,
		    "Nominal point size") == bdffontv_AskChanged)
	{
	    bdffontv_DefaultPointSize = ptsize;
	}
	else {
	    ptsize = bdffontv_DefaultPointSize;
	}

	res = bdffontv_DefaultResolution;
	if (AskFor1(self,
		    &res,
		   "Target resolution") == bdffontv_AskChanged)
	{
	    bdffontv_DefaultResolution = res;
	}
	else {
	    res = bdffontv_DefaultResolution;
	}

	dwidth = bdffontv_DefaultWidth;
	if (AskFor1(self,
		    &dwidth,
		    "Default width") == bdffontv_AskChanged)
	{
	    bdffontv_DefaultWidth = dwidth;
	}
	else {
	    dwidth = bdffontv_DefaultWidth;
	}

	bdffont_SetPointSize(fontinfo, ptsize);
	bdffont_SetResolution(fontinfo, res, res);
	size = bdffont_ComputeFontSize(fontinfo);
	dheight = /*mathaux_*/RoundUp(ansitext_ComputeAscent(size));
	descent = /*mathaux_*/RoundUp(ansitext_ComputeDescent(size));
	bdffont_SetFontAscent(fontinfo, dheight);
	bdffont_SetFontDescent(fontinfo, descent);
	bdffont_SetBoundingBox(fontinfo, dwidth, dheight+descent, 0, -descent);
	bdffont_SetDefaultExtent(fontinfo, dwidth, dheight);
	bdffont_SetDefaultOrigin(fontinfo, 0, 0);
	bdffont_SetDefaultDelta(fontinfo, dwidth, 0);

	SetFontCharacteristics(self);
    }


static boolean bdffontv_InitializeViews(self)
    struct bdffontv *self;
{
    struct chlist *chl;
    int i;
    int boxheight;

    if (self->initialized) {
	return TRUE;
    }

    self->fontinfo = lpair_New();
    self->fontdata = lpair_New();
    self->fontpresentation = lpair_New();
    self->fontsummary = lpair_New();
    self->ptres = lpair_New();
    self->originextent = lpair_New();
    self->menuedit = lpair_New();
    self->infoedit = lpair_New();
    self->encodingextent = lpair_New();

    /* setup pthelpV so we can interrrogate font */
   chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_PointLabel,
			bdffontv_SetPointSizeCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_PointLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_HelpLabel,
			bdffontv_HelpLabel, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_HelpLabel) - 2,
			  helpstyle);
    self->pthelpV = chlistview_New();
    chlistview_SetDataObject(self->pthelpV, chl);

    /* try to determine size of lines needed */
    {
	struct text_statevector sv;
	struct FontSummary *chvfont;

	chlistview_GetStyleInformation(self->pthelpV, &sv, 1, NULL);

	chvfont = fontdesc_FontSummary(
			fontdesc_Create(sv.CurFontFamily,
					sv.CurFontAttributes, sv.CurFontSize),
			bdffontv_GetDrawable(self)  );

	boxheight = chvfont->maxHeight + chvfont->maxBelow + 5;
	if (boxheight < chvfont->newlineHeight + 5)
		boxheight = chvfont->newlineHeight + 5;
	if (boxheight < 20) boxheight = 20;
	if (boxheight > 150) boxheight = 150;
/* printf("newline ht:  %d  maxHt: %d   maxBelow: %d  ==>  boxht: %d\n",
chvfont->newlineHeight, chvfont->maxHeight, 
chvfont->maxBelow, boxheight); */
    }

    /* split window.  Put font info on top and character info below */
    bdffontv_SetUp(self,
		   self->fontinfo,
		   self->menuedit,
		   4*boxheight - 3, lpair_TOPFIXED, lpair_HORIZONTAL, TRUE);	/* FALSE */

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, " ", bdffontv_SetFontNameCmd, self);
    chlist_AlwaysAddStyle(chl, i, 1L, fontnamestyle);

    self->fontnameV = chlistview_New();
    chlistview_SetDataObject(self->fontnameV, chl);

    /* split top area horizontally: top is font name and below is data */
    lpair_SetUp(self->fontinfo,
		self->fontnameV,
		self->fontdata,
		boxheight, lpair_TOPFIXED, lpair_HORIZONTAL, TRUE);	/* FALSE */

    /* split data area in the middle: left is family, pointsize, resolution
		right is face, origin, and width&height */
    lpair_SetUp(self->fontdata,
		self->fontpresentation,
		self->fontsummary,
		50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);	/* FALSE */

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_FamilyLabel,
			bdffontv_SetFontFamilyCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_FamilyLabel) - 2,
			  fieldstyle);

    self->fontfamilyV = chlistview_New();
    chlistview_SetDataObject(self->fontfamilyV, chl);

    /* split left : family above pointsize/resolution */
    lpair_SetUp(self->fontpresentation,
		self->fontfamilyV,
		self->ptres,
		boxheight, lpair_TOPFIXED, lpair_HORIZONTAL, TRUE);	/* FALSE */

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_FaceLabel,
			bdffontv_SetFontFaceCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_FaceLabel) - 2,
			  fieldstyle);

    self->fontfaceV = chlistview_New();
    chlistview_SetDataObject(self->fontfaceV, chl);

    /* split right: face above origin and width&height */
    lpair_SetUp(self->fontsummary,
		self->fontfaceV,
		self->originextent,
		boxheight, lpair_TOPFIXED, lpair_HORIZONTAL, TRUE);	/* FALSE */

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_ResolutionXLabel,
			bdffontv_SetResolutionCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_ResolutionXLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_ResolutionYLabel,
			bdffontv_SetResolutionCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_ResolutionYLabel) - 2,
			  fieldstyle);

    self->resV = chlistview_New();
    chlistview_SetDataObject(self->resV, chl);

    /* split to put pt size on left and resolution on the right */
    lpair_SetUp(self->ptres,
		self->pthelpV,
		self->resV,
		50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);	/* FALSE */

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_OriginXLabel,
			bdffontv_FontExtentCmd, self);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_OriginYLabel,
			bdffontv_FontExtentCmd, self);

    self->fontoriginV = chlistview_New();
    chlistview_SetDataObject(self->fontoriginV, chl);

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_WidthLabel,
			bdffontv_FontExtentCmd, self);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_HeightLabel,
			bdffontv_FontExtentCmd, self);

    self->fontextentV = chlistview_New();
    chlistview_SetDataObject(self->fontextentV, chl);

    /* split to put origin on the left and width&height on the right */
    lpair_SetUp(self->originextent,
		self->fontoriginV,
		self->fontextentV,
		50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);	/* FALSE */

    self->charmenuL = chlist_New();
    self->defns_size = 0;

    self->charmenuV = chlistview_New();
    chlistview_SetDataObject(self->charmenuV, self->charmenuL);

    EnsureCharMenu(self, 256);

    /* list of characters on the left / info and edit area on the right */
    lpair_SetUp(self->menuedit,
		chlistview_GetApplicationLayer(self->charmenuV),
		self->infoedit,
		80, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);	/* FALSE */

    self->charedit = raster_Create(16, 16);
    self->rastermodified = raster_GetModified(self->charedit);
    self->chareditV = /* ... frasterv_New() */ rasterview_New();
    /* ... frasterv */ rasterview_SetDataObject(self->chareditV, self->charedit);

   /* character info above raster for editing */
    lpair_SetUp(self->infoedit,
		self->encodingextent,
		/* ... frasterv */ rasterview_GetApplicationLayer(self->chareditV),
		4*boxheight-6, lpair_TOPFIXED, lpair_HORIZONTAL, TRUE);	/* FALSE */

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_EncodingLabel,
			bdffontv_SetEncodingCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_EncodingLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_NameLabel, bdffontv_SetNameCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_NameLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_DeltaXLabel, bdffontv_SetDeltaCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_DeltaXLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_DeltaYLabel, bdffontv_SetDeltaCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_DeltaYLabel) - 2,
			  fieldstyle);

    self->charencodingV = chlistview_New();
    chlistview_SetDataObject(self->charencodingV, chl);

    chl = chlist_New();
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_OriginXLabel,
			bdffontv_SetOriginCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_OriginXLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_OriginYLabel,
			bdffontv_SetOriginCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_OriginYLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_WidthLabel, bdffontv_SetExtentCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_WidthLabel) - 2,
			  fieldstyle);
    i = chlist_GetLength(chl);
    chlist_AddItemToEnd(chl, bdffontv_HeightLabel,
			bdffontv_SetExtentCmd, self);
    chlist_AlwaysAddStyle(chl,
			  i, sizeof(bdffontv_HeightLabel) - 2,
			  fieldstyle);
    self->charextentV = chlistview_New();
    chlistview_SetDataObject(self->charextentV, chl);

    lpair_SetUp(self->encodingextent,
		self->charencodingV,
		self->charextentV,
		50, lpair_PERCENTAGE, lpair_VERTICAL, TRUE);	/* FALSE */

    self->initialized = TRUE;

    SetFontCharacteristics(self);

    return (self->initialized);
} /* bdffontv_InitializeViews */

void bdffontv__SetDataObject(self, dobj)
struct bdffontv *self;
struct bdffont *dobj;
{
    super_SetDataObject(self, dobj);
    if (self->charmenuL)
	SetFontCharacteristics(self);

} /* bdffontv__SetDataObject */

void bdffontv__ObservedChanged(self, changed, value)
struct bdffontv *self;
struct observable *changed;
long value;
{
    struct bdffont *dobj = (struct bdffont *)bdffontv_GetDataObject(self);
    if (value == bdffont_Writing) {
	if ( ! bdffontv_UpdateCharModification(self)) 
		message_DisplayString(self, 0, "Character changes NOT saved");
    }
    else if (dobj && self->charmenuL) {
	EnsureCharMenu(self, dobj->defns_size);
    }
    super_ObservedChanged(self, changed, value);
}

boolean bdffontv__InitializeObject(c, self)
    struct classheader *c;
    struct bdffontv *self;
{
    self->initialized = FALSE;
    self->charmodified = 0;
    self->rastermodified = 0;
    self->charinfo = NULL;
    self->zoomdelta = 1;
    self->prevzoom = 1;
    self->charmenuL = NULL;
    self->chareditV = NULL;

    self->keys = keystate_Create(self, keys);
    self->menus = menulist_DuplicateML(menus, self);

    return (TRUE);
} /* bdffontv__InitializeObject */

void bdffontv__FinalizeObject(c, self)
    struct classheader *c;
    struct bdffontv *self;
{
    struct chlist *chl;

    if (self->keys) {
	keystate_Destroy(self->keys);
    }

    if (self->menus) {
	menulist_Destroy(self->menus);
    }

    if (self->fontinfo) {
	/* destroy views from top down. Because, that's why. */
	bdffontv_SetNth(self, 0, NULL);
	bdffontv_SetNth(self, 1, NULL);

	lpair_Destroy(self->fontinfo);
	lpair_Destroy(self->fontdata);
	lpair_Destroy(self->fontpresentation);
	lpair_Destroy(self->fontsummary);
	lpair_Destroy(self->ptres);
	lpair_Destroy(self->originextent);
	lpair_Destroy(self->menuedit);
	lpair_Destroy(self->infoedit);
	lpair_Destroy(self->encodingextent);

	chl = (struct chlist *) chlistview_GetDataObject(self->charextentV);
	chlistview_Destroy(self->charextentV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->charencodingV);
	chlistview_Destroy(self->charencodingV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->charmenuV);
	chlistview_Destroy(self->charmenuV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->fontextentV);
	chlistview_Destroy(self->fontextentV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->fontoriginV);
	chlistview_Destroy(self->fontoriginV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->resV);
	chlistview_Destroy(self->resV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->pthelpV);
	chlistview_Destroy(self->pthelpV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->fontfaceV);
	chlistview_Destroy(self->fontfaceV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->fontfamilyV);
	chlistview_Destroy(self->fontfamilyV);
	chlist_Destroy(chl);
	chl = (struct chlist *) chlistview_GetDataObject(self->fontnameV);
	chlistview_Destroy(self->fontnameV);
	chlist_Destroy(chl);
    }
    if (self->chareditV) {
	rasterview_Destroy(self->chareditV);
    }

    if (self->charedit) {
	raster_Destroy(self->charedit);
    }


} /* bdffontv__FinalizeObject */

void bdffontv__PostKeyState(self, keystate)
    struct bdffontv *self;
    struct keystate *keystate;
{
    keystate_Reset(self->keys);
    keystate_AddBefore(self->keys, keystate);
    super_PostKeyState(self, self->keys);
} /* bdffontv__PostKeyState */

void bdffontv__PostMenus(self, menulist)
    struct bdffontv *self;
    struct menulist *menulist;
{
    menulist_ClearChain(self->menus);
    if (menulist) {
        menulist_ChainBeforeML(self->menus, menulist, (long) menulist);
    }
    super_PostMenus(self, self->menus);
} /* bdffontv__PostMenus */


void bdffontv__FullUpdate(self, type, left, top, width, height)
    struct bdffontv *self;
    enum view_UpdateType type;
    long left;
    long top;
    long width;
    long height;
{
    struct bdffont *fontinfo;

    if ( ! self->initialized && type != view_MoveNoRedraw && type != view_Remove)
	bdffontv_InitializeViews(self);

    super_FullUpdate(self, type, left, top, width, height);

    /*
	Check here if no info in order to generate questions of user
	to fill in parameters
    */

    fontinfo = (struct bdffont *) bdffontv_GetDataObject(self);
    if (bdffont_GetFontName(fontinfo) == NULL) 
	im_EnqueueEvent(GetNewFontParameters, self, 0);

    /* EnsureFocus(self); */
    if (self->chareditV && type != view_MoveNoRedraw && type != view_Remove)
	rasterview_WantInputFocus(self->chareditV, self->chareditV);

} /* bdffontv__FullUpdate */

	void
bdffontv__ReceiveInputFocus(self)
	struct bdffontv *self;
{
	if (self->chareditV)
		rasterview_WantInputFocus(self->chareditV, self->chareditV);
}

	void
bdffontv__WantInputFocus(self, target)
	struct bdffontv *self;
	struct view *target;
{
	if (self->chareditV) {
		if (target == (struct view *)self->chareditV)
			super_WantInputFocus(self, target);
		else rasterview_WantInputFocus(self->chareditV, self->chareditV);
	}
}
