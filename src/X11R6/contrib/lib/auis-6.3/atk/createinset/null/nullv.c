/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *	   Copyright Carnegie Mellon, 1992 - All Rights Reserved
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/createinset/null/RCS/nullv.c,v 1.8 1994/02/15 16:12:28 wjh Exp $";
#endif


 

/* nullv.c	

	The view module for the null dataobject

*/

/*
 *   $Log: nullv.c,v $
 * Revision 1.8  1994/02/15  16:12:28  wjh
 * change to display round blobs with chartreuse center
 *
 * Revision 1.7  1993/05/04  01:14:27  susan
 * RCS Tree Split
 *
 * Revision 1.6.1.1  1993/02/02  01:41:01  rr2b
 * new R6tape branch
 *
 * Revision 1.6  1992/12/15  21:32:24  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.5  1992/12/14  20:40:31  rr2b
 * disclaimerization
 *
 * Revision 1.4  1992/06/22  21:48:52  wjh
 * test for NULL in getting fontdesc
 * made blobs chartreuse on color displays
 * removed extraneous clearing of screen in _ClearRectangle
 * check for NULLs in InitializeClass
 * give a default desiredSize
 * .
 *
 * Revision 1.3  1991/09/12  16:11:44  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.2  1989/12/12  15:00:08  ghoti
 * sync with MIT tape
 *
 * Revision 1.2  89/12/05  11:33:39  xguest
 * change instances of nullv to nullview
 * 
 * Revision 1.1  89/12/04  20:28:25  xguest
 * Initial revision
 * 
 * Revision 1.1  89/07/31  15:35:20  wjh
 * Initial revision
 * 
 *  
 * Revision 1.0  88/05/14  15:40:34  wjh
 * Copied from /usr/andrew/lib/genericinset
 */


#include <strings.h>

#include <graphic.ih>
#include <view.ih>
#include <fontdesc.ih>

#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <bind.ih>
#include <im.ih>
#include <rect.h>
#include <txttroff.ih>

/* $$$ include xxx.ih for any other file for which there is below a call to xxx_routine() */

/* include headers for the data object and THIS view */
#include <null.ih>
#include <nullv.eh>


static char  debug;      /* This debug switch is toggled with ESC-^D-D */
	/*	$$$ you could provide a method in this module which sets -debug-
		Then nullvt.c could call it to set this debug from the -d switch */
#define DEBUG(s) {if (debug) {printf s ; fflush(stdout);}}
#define ENTER(r) DEBUG(("Enter %s(0x%lx)\n", "r", self))
#define LEAVE(r) DEBUG(("Leave %s(0x%lx)\n", "r", self))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *
 *	Supporting procedures
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* $$$ ComputeArea is one approach to a computation satisfying xxx_DesiredSize. 
	This code also illustrates how to find the size of a character or string.
	You can delete this function if size is computed entirely 
	within the xxx_DesiredSize routine. */

/* ComputeArea(self)
	set fields self->Area / Width / Height
	The width and height are 10 times the size of a 'W' and area is the product.

	The routine is more general, though; variable c can be set to any string and 
	ten times the size of that string is used.

	(Should modify so it checks the coordinates in its data object and
	requests at least enough room for the largest.)
*/
	static void
ComputeArea(self)
	register struct nullview *self;
{
	long minwidth, minheight;
	char *tail;
	char *c = "W";
	register struct graphic *g;
	struct fontdesc *fd;
	struct FontSummary *FS;
	long cnt;

	if (self->sizeknown)
		return;

	g = (struct graphic *)nullview_GetDrawable(self);
	fd = nullview_GetFont(self);
	if (g == NULL || fd == NULL) 
		return;
	FS = fontdesc_FontSummary(fd, g);
	if (FS == NULL) return;

	minwidth = minheight = 0;
	for (cnt=0; cnt < 2; cnt++) {
		tail = c;
		/* if c is multiple lines, the size must be computed by
			adding the sizes of each line */
		while (tail) {
			register char *tend = index(tail, '\n');
			long w, h;
			if (tend) {
				fontdesc_TextSize(fd, g, tail, tend-tail, &w, &h);
				tail = tend+1;
			}
			else {
				fontdesc_StringSize(fd, g, tail, &w, &h);
				tail = NULL;
			}
			if (w > minwidth)  minwidth = w;
			minheight += FS->maxHeight;
		}
		if (minwidth != 0  &&  minheight != 0) 
			break;
		/* first tried current font: it had no 'W'.  Now try andy12 font */
		fd = fontdesc_Create("andy", 0, 12);
	}
	/* The string width is more or less minwidth.  
		(In fact minwidth is the distance
		from the origin of the first character of the string 
		to the origin of the next character after the string.)
	    The string height is no greater than minheight
		(We should really use FS->newlineHeight, 
		but that field is not implemented at this writing.)  */

	self->DesiredWidth = 10 * minwidth;
	self->DesiredHeight = 10 * minheight;
	self->DesiredArea = 100 * minwidth * minheight;

	self->sizeknown = TRUE;

	DEBUG(("ComputeArea: %d x %d   (cnt: %d)\n", 
			self->DesiredWidth, self->DesiredHeight, cnt));
}


/* 	$$$ This routine is not used.  It illustrates using a graphic primitive */
/* InvertRectangle(self, left, top, width, height)
	Changes white to black and black to white within the rectangle. 
*/
	static void 
InvertRectangle(self, left, top, width, height)
	register struct nullview *self;
{
	/* this code shows how to invert rectangles */
	/* it will give unpredictable results on color displays */
	struct rectangle r;

	nullview_SetTransferMode(self, graphic_INVERT);
	rectangle_SetRectSize(&r, left, top, width, height);
	nullview_FillRect(self, &r, self->BlackPattern);
}

/* 	$$$ This routine is not used.  It illustrates using a graphic primitive */
/* ShowString(self, x, y, string)
	Writes the text of 'string' centered at (x, y) in the logical rectangle.
*/
	static void
ShowString(self, x, y, string)
	register struct nullview *self;
	long x, y;
	char *string;
{
	nullview_MoveTo(self, x, y);
	nullview_DrawString(self, string,
			graphic_BETWEENLEFTANDRIGHT |
			    graphic_BETWEENTOPANDBASELINE);
}


/* 	$$$ replace the following routine with one that redraws 
	whatever image your view is to provide 
		The algorithm here flashes the screen because it erases all the
		dots each time a new one is added.
		The correct approach is for the view to keep track of
		what it has on the screen and compare it with the data object
		each time there is to be a redraw.
		This will then continue to work if we add an operation to 
		remove dots.
*/
/* RedrawView(self, type, left, top, width, height)
	Redraws the view from the data structure.
	If the type specifies a partial redraw, we only really need to redraw the
	portion delimited by left, top, width, height. ($$$)
*/
	static 
RedrawView(self, type, left, top, width, height)
	register struct nullview *self;
	enum view_UpdateType type;
	long left, top, width, height;
{
	struct rectangle r;
	register struct dotlist *d;
	register struct null *dobj 
		= (struct null *)self->header.view.dataobject;
	boolean colordpy = (nullview_DisplayClass(self) & graphic_Color) != 0;
	long fgr, fgg, fgb;

	nullview_SetTransferMode(self, graphic_COPY);
	nullview_GetLogicalBounds(self, &r);		/* find rectangle bounds */
	nullview_FillRect(self, &r, self->WhitePattern);	/* clear the rectangle */

	rectangle_GetRectSize(&r, &left, &top, &width, &height);
		/* we shamelessly utilize the parameters as locals */
	DEBUG(("RedrawView(%d) in [%d, %d, %d, %d]\n", type, left, top, width, height));
		/* we have to subtract one from 
			right and bottom to get inside the rectangle */
	if (! self->HasInputFocus)  {
		/* draw a diagonal line to indicate lack of input focus */
		nullview_MoveTo(self, left, top);
		nullview_DrawLineTo(self, left+width-1, top+height-1);
	}
	else 
		/* has input focus.  draw outline of rectangle */
		nullview_DrawRectSize(self, left, top, width-1, height-1);

	if (colordpy) 
		nullview_GetForegroundColor(self, NULL, &fgr, &fgg, &fgb);

	for (d = null_GetFirstDot(dobj);  d != NULL;
			d = null_GetNextDot(dobj, d)) {
		/* display each dot as a ring  */
		struct rectangle r;
		nullview_SetTransferMode(self, graphic_COPY);
		rectangle_SetRectSize(&r, null_GetDotX(dobj, d)-4,
			null_GetDotY(dobj, d)-4, 9, 9);
		nullview_FillOval(self, &r, self->BlackPattern);
		if (colordpy) {
			rectangle_SetRectSize(&r, null_GetDotX(dobj, d)-2,
				null_GetDotY(dobj, d)-2, 5, 5);
			/* for colors, see /usr/lib/X11/rgb.txt */
			nullview_SetForegroundColor(self, NULL,
					127<<8, 255<<8, 0<<8);  /* chartreuse */
			nullview_FillOval(self, &r, NULL);	/* use foreground color */
			nullview_SetForegroundColor(self, NULL, fgr, fgg, fgb);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	User Interface 
 *	
 *	Routines called from keystrokes or menu
 *
 *	$$$ in this section put any operations that are to apply to your view
 *	The -rock- is the 3rd or 5th value from the bind description line (depending
 *	on whether the operation is invoked from menu or keyboard).
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	static void
nullview_ClearRectangle(self, rock)
	register struct nullview *self;
	long rock;
{
	register struct null *dobj = (struct null *)self->header.view.dataobject;
	null_ClearDots(dobj);
	null_NotifyObservers(dobj, null_DATACHANGED);
}

	static void
ToggleDebug(self, rock)
	register struct nullview *self;
	long rock;
{
	debug = ! debug;
	printf("debug is now %d\n", debug);  fflush (stdout);
}


/*  Menus and Keymaps
	The menus and keymaps are initialized to those of
	    EmbeddedMenus and EmbeddedKeymap in InitializeObject.
	The ApplicationMenus and ApplicationKeymap get
	    added to the menus and keymap in GetApplicationLayer

	Debugging can be toggled with the key sequence  ESC-^D -D

	$$$ Replace the initialization of the arrays with keybindings (2nd value)
	and/or menu entries (4th value) of your choice.  The function to be called
	is the 7th value in each array line.  The first value is a name and the 8th is 
	a comment.  The 6th controls non-constant menus and the 9th is ignored.
*/
static struct menulist  *EmbeddedMenus, *ApplicationMenus;
static struct keymap  *EmbeddedKeymap, *ApplicationKeymap;

static struct bind_Description EmbeddedBindings[]={
    {"null-clear-rectangle", "\003", 0, "Clear~10", 0, 0, 
			nullview_ClearRectangle, "Clear the rectangle", NULL},
    {"null-invert-debug", "\033\004D",		/* ESC - ^D - D */
			0, 0, 0, 0, ToggleDebug,
			"Toggle the nullview debug flag", NULL},
	NULL
};
static struct bind_Description ApplicationBindings[]={
	NULL
};



	boolean
nullview__InitializeClass(ClassID)
	struct classhdr *ClassID;
{
	EmbeddedMenus = menulist_New();
	ApplicationMenus = menulist_New();
	EmbeddedKeymap = keymap_New();
	ApplicationKeymap = keymap_New();
	if (EmbeddedMenus == NULL || ApplicationMenus == NULL
			|| EmbeddedKeymap == NULL 
			|| ApplicationKeymap == NULL)
		return FALSE;
	bind_BindList(EmbeddedBindings, EmbeddedKeymap, EmbeddedMenus,
				&nullview_classinfo);
	bind_BindList(ApplicationBindings, ApplicationKeymap, ApplicationMenus,
				&nullview_classinfo);
	return TRUE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	
 *	Override methods
 *	
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	boolean
nullview__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct nullview  *self;
{
	self->Menus = menulist_DuplicateML(EmbeddedMenus, self);
	/* ApplicationMenus are added in GetApplicationLayer */
	self->Keystate = keystate_Create(self, EmbeddedKeymap);

	if (self->Menus == NULL || self->Keystate == NULL)
		return FALSE;

	self->sizeknown = FALSE;
	self->OnScreen = FALSE;
	self->HasInputFocus = FALSE;
	self->embedded = TRUE;
	self->ignoreUp = FALSE;

	self->DesiredWidth = 145;
	self->DesiredHeight = 91;
	self->DesiredArea = 100 * 91 * 45;

	/* $$$ here initialize any variables added to the "data:" portion of 
		nullv.ch */

	return TRUE;
}

	void 
nullview__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	register struct nullview  *self;
{
	menulist_Destroy(self->Menus);
	keystate_Destroy(self->Keystate);

	/* $$$ is variables in the data: of nullv.ch have been assigned
	pointers to malloc'ed storage, that memory should be free'd here.  */

}

	void
nullview__ObservedChanged(self, dobj, status)
	register struct nullview  *self;
	struct null *dobj;
	long status;
{
	if (status == observable_OBJECTDESTROYED) 
		/* do not call wantupdate in this case,
			it will bomb the program */
		{}
	else if (status == null_DATACHANGED) 
		/* $$$ it would be a good idea to make a note of which parts 
			of the data have changed, so only the corresponding parts 
			of the image need to be changed.  */ 
		nullview_WantUpdate(self, self);	/* request a chance to 
						update the image */
}

	struct nullview *
nullview__GetApplicationLayer(self)
	register struct nullview *self;
{
	self->embedded = FALSE;
	menulist_ChainBeforeML(self->Menus, 
		menulist_DuplicateML(ApplicationMenus, self), NULL);
	keystate_AddBefore(keystate_Create(self, ApplicationKeymap), self->Keystate);
	return self;
}

/* DON'T DO screen operations in ReceiveInputFocus */

	void 
nullview__ReceiveInputFocus(self)
	register struct nullview  *self;
{
	ENTER(nullv_ReceiveInputFocus);
	nullview_PostMenus(self, self->Menus);

	self->Keystate->next = NULL;
	nullview_PostKeyState(self, self->Keystate);

	self->HasInputFocus = TRUE;
	nullview_WantUpdate(self, self);   /* schedule an update to 
			provide an indication that we have the input focus */
	LEAVE(nullv_ReceiveInputFocus);
}

/* DON'T DO screen operations in ReceiveInputFocus,
	but DO remove any defined cursor */

	void 
nullview__LoseInputFocus(self)
	register struct nullview  *self;
{
	ENTER(nullview_LoseInputFocus);
	self->HasInputFocus = FALSE;
	nullview_WantUpdate(self, self);  /* schedule an update to display 
			 indication that we no longer have the input focus */

	/* menus and keystate are deactivated by parent */

	LEAVE(nullview_LoseInputFocus);
}

/* nullview__FullUpdate(self, type, left, top, width, height)
	Redraws the entire image.  (The last four arguments are only used in 
	case the 'type' is a PartialRedraw;  they specify which part to update.)
*/
	void 
nullview__FullUpdate(self, type, left, top, width, height)
	register struct nullview  *self;
	register enum view_UpdateType  type;
	register long  left, top, width, height;
{
	DEBUG(("FullUpdate(%d, %d, %d, %d, %d)\n", type, left, top, width, height));
	if (type == view_Remove  
			||  nullview_GetLogicalWidth(self) == 0 
			|| nullview_GetLogicalHeight(self) == 0) {
		/* view_Remove means the view has left the screen.
			A zero dimension means the view is not visible */
		self->OnScreen = FALSE;
		return;
	}
	if (type != view_FullRedraw && type != view_LastPartialRedraw)
		return;
	/* we continue only for a FullRedraw or the last of a sequence of PartialRedraw
		requests.  */

	self->OnScreen = TRUE;
/* the following line is appropriate, but on 24 April 1988 it tickles a bug in ez 
	if (type == view_FullRedraw) 
*/
	{
		/* must recompute graphics info because image
			may be on different display hardware */

		self->WhitePattern = nullview_WhitePattern(self);
		self->BlackPattern = nullview_BlackPattern(self);
		DEBUG(("Drawable: 0x%lx   White: 0x%lx   Black: 0x%lx\n",
			nullview_GetDrawable(self), 
			self->WhitePattern, self->BlackPattern));
	}
	RedrawView(self, type, left, top, width, height);
	LEAVE(nullview_FullUpdate);
}


	void 
nullview__Update(self)
	register struct nullview *self;
{
ENTER(nullview_Update);
	if (! self->OnScreen) return;

	/* $$$ the code in this example is highly bogus: it redraws the entire view
		every time there is any change.  Note that this leads to unacceptable
		flashing.  Instead the notes taken in ObservedChanged should be
		used here to update only the changed portion of the image */

	RedrawView(self, view_FullRedraw, 0, 0, 0, 0);

LEAVE(nullview_Update);
}

	struct view *
nullview__Hit(self, action, x, y, num_clicks)
	register struct nullview  *self;
	register enum view_MouseAction  action;
	register long  x, y, num_clicks;
{
	register struct null *dobj
		= (struct null *)self->header.view.dataobject;
DEBUG(("Hit at (%d, %d) type %d\n", x, y, action));
	if (action == view_NoMouseEvent)
		return (struct view *)self;
	if (! self->OnScreen) return NULL;

	/* $$$ replace the following section with code to provide the
		desired response to mouse actions.  In this example, 
		a new dot is drawn and added to the data structure
		at the coordinates of the mouse hit.  It is a convention
		that permanent actions should take place on Up movement
		of a mouse button.   */
	if (action == view_LeftDown || action == view_RightDown) {
		if ( ! self->HasInputFocus) {
			nullview_WantInputFocus(self, self);
			self->ignoreUp = TRUE;
		}
	}
	else if (action == view_LeftUp || action == view_RightUp) {
		struct rectangle r;

		if (self->ignoreUp) {
			self->ignoreUp = FALSE;
			return NULL;
		}
		nullview_GetLogicalBounds(self, &r);	/* find rectangle bounds */
		if (x < rectangle_Left(&r)  ||  x >= rectangle_Right(&r)
				|| y < rectangle_Top(&r)
				|| y >= rectangle_Bottom(&r))
			/* if the mouse up is out of the rectangle, we do not
				want to add a dot.  Note the '=' signs;
				rectangles are not symmetrically bounded.  */
			return NULL;

		null_AddDot(dobj, x, y);
		null_NotifyObservers(dobj, null_DATACHANGED);

		/* draw the image immediately, but only in foreground color
			(It will be redrawn by the Update routine
			called because of the NotifyObservers) */
		nullview_SetTransferMode(self, graphic_COPY);
		rectangle_SetRectSize(&r, x-4, y-4, 9, 9);
		nullview_FillOval(self, &r, self->BlackPattern);
	}

LEAVE(Hit);
	return (struct view *)self;		/* where to send subsequent hits */
}

/* 	$$$ replace with some routine that responds with a size for your view */
/* nullview__DesiredSize(self, width, height, pass, desiredWidth, desiredHeight) 
	The parent calls this to find out how big the view would like to be.
	This routine sets 'desiredWidth' and 'desiredHeight' and returns a
		code value indicating which is flexible for further negotiation.
	The 'width' and 'height' parameters are tentative values the parent is suggesting.
	'pass' indicates which of 'width' and 'height' should be considered fixed.
	If neither is fixed, they may be arbitrary values. 
*/
	enum view_DSattributes
nullview__DesiredSize(self, width, height, pass, desiredWidth, desiredHeight) 
	register struct nullview *self;
	long width;
	long height;
	enum view_DSpass pass;
	long *desiredWidth;
	long *desiredHeight;
{
	DEBUG(("DesiredSize(...%d, %d, %d...)\n", width, height, pass));

	ComputeArea(self);	/* set self->DesiredArea/Width/Height */

	if (pass == view_NoSet) {
		*desiredWidth = self->DesiredWidth;
		*desiredHeight = self->DesiredHeight;
	}
	else if (pass == view_WidthSet) {
		*desiredWidth = width;
		*desiredHeight = self->DesiredArea / width;
	}
	else /* pass == view_HeightSet */ {
		*desiredWidth = self->DesiredArea / height;
		*desiredHeight = height;
	}

	DEBUG(("Leave DesiredSize: %d x %d\n", *desiredWidth, *desiredHeight));
	return view_HeightFlexible | view_WidthFlexible;
}

/* # # # # # # # # # # # # # # 
 *	PRINTING	
 *  # # # # # # # # # # # # #  */

	void
nullview__Print(self, file, processor, format, topLevel)
	register struct nullview 	*self;
	register FILE  *file;
	register char   *processor;
	register char   *format;
	register boolean   topLevel;
{
#ifdef texttroff_HASPBPE		/* use this until new texttroff is everywhere  */
	register struct null *dobj = (struct null *)self->header.view.dataobject;
	long wpts, hpts;  /* image dimensions in points */
	struct dotlist *dot;
	char *prefix;

	/* $$$  If you want to describe the print image of your view in troff,
		replace the contents of this routine.  To use PostScript,
		replace the pieces noted below. */

	wpts = nullview_GetLogicalWidth(self);
	hpts = nullview_GetLogicalHeight(self);
	if (wpts == 0  ||  hpts == 0) {
		/* the parent has GOOFED and has not
			supplied a logical rectangle for printing */
		ComputeArea(self);
		wpts = self->DesiredWidth;
		hpts = self->DesiredHeight;
	}

	/* We should restrict wpts and hpts to (say) 576x720  (8x10 inches).
		We could do so by scaling the image */

	if (strcmp(processor, "troff") == 0) {
		/* output to troff */
		if (topLevel)
			/* take care of initial troff stream */
			texttroff_BeginDoc(file);
		/*  Put macro to interface to postscript */
		texttroff_BeginPS(file, wpts, hpts);
		prefix = "\\!  ";
	}
	else prefix = "";

	/* generate PostScript  */
	fprintf(file, "%s  /width %d def  /height %d def\n", prefix, wpts, hpts);
	fprintf(file, "%s  newpath 0 0 moveto 0 height lineto width height lineto\n", prefix);
	fprintf(file, "%s  \twidth 0 lineto clip    %% clip to assigned area\n", prefix);	

	/* $$$ if you want to describe the image of your inset in postscript, replace the 
	following fprints and the for-loop below with code to generate the PostScript. */

	/* define a postscript routine to display a dot */
	fprintf(file, "%s  /blob {\n", prefix);
	fprintf(file, "%s  \tneg height add    %% convert from screen to postscript coords\n",
			prefix);
	fprintf(file, "%s  \tmoveto	%% use coords from call\n", prefix);
	fprintf(file, "%s  \t10 0 rlineto\n", prefix);
	fprintf(file, "%s  \t0 -7 rlineto\n", prefix);
	fprintf(file, "%s  \t-10 0 rlineto\n", prefix);
	fprintf(file, "%s  \tfill\n", prefix);
	fprintf(file, "%s  } def\n", prefix);

	fprintf(file, "%s  newpath width 0 moveto 0 height lineto 0.5 setlinewidth stroke \t\n",
			prefix);		/* draw diagonal line */

	/* draw the dots */
	for (dot = null_GetFirstDot(dobj); dot != NULL; 
			dot = null_GetNextDot(dobj, dot))
		fprintf(file, "%s  %d %d blob\n", prefix,
			null_GetDotX(dobj, dot),
			null_GetDotY(dobj, dot));

	if (strcmp(processor, "troff") == 0){
		texttroff_EndPS(file, wpts, hpts);
		if (topLevel)
			texttroff_EndDoc(file);
	}
#endif texttroff_HASPBPE	/* use this until new texttroff is everywhere */
}
