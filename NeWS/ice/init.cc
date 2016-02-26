/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <netdb.h>
#include <sys/param.h>
#include <X11/cursorfont.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "ice_defines.h"
#include "ice_externs.h"
#include "Raster.h"
#include "Text.h"
#include "Vector.h"
#include "Curve.h"
#include "Marker.h"
#include "Rectangle.h"
#include "Polygon.h"
#include "Axis.h"
#include "Path.h"

#include "hourglass.bitmap"

extern "C" {
char *			gconvert(double, int, int, char *);
char *			getenv(char *);
long			ntohl(long);
int			pprintf( ... );
int			ps_checkfor(PSFILE *, int, int);
PSFILE *		ps_open_server(char *);
int			psio_flush(PSFILE *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
}

extern void		attrgdf_proc(Menu *, Menu_item *);
extern void		attrins_proc(Menu *, Menu_item *);
extern void		attrpg_proc(Menu *, Menu_item *);
extern void		attrsel_proc(Menu *, Menu_item *);
extern void		axisattrabort_proc(Panel *, Panel_item *);
extern void		axisattraxiswd_proc(Panel *, Panel_item *);
extern void		axisattrcont_proc(Panel *, Panel_item *);
extern void		axisattrdtk_proc(Panel *, Panel_item *);
extern void		axisattrfont_proc(Panel *, Panel_item *);
extern void		axisattrfontfamily_proc(Panel *, Panel_item *);
extern void		axisattrfontloc_proc(Panel *, Panel_item *);
extern void		axisattrfontsz_proc(Panel *, Panel_item *);
extern void		axisattrlab_proc(Panel *, Panel_item *);
extern void		axisattrlin_proc(Panel *, Panel_item *);
extern void		axisattrmvloc1_proc(Panel *, Panel_item *);
extern void		axisattrmvloc2_proc(Panel *, Panel_item *);
extern void		axisattrtickloc_proc(Panel *, Panel_item *);
extern void		axisattrtickwd_proc(Panel *, Panel_item *);
extern void		cmap_copy(Colormap, Colormap);
extern void		cmap_copyrange(Colormap, Colormap, int, int);
extern void		cmap_defcolors(Colormap, int);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmapdef_proc(Menu *, Menu_item *);
extern void		cmpaddsel_proc(Menu *, Menu_item *);
extern void		cmpattrabort_proc(Panel *, Panel_item *);
extern void		cmpattrcont_proc(Panel *, Panel_item *);
extern void		cmpattrsel_proc(Menu *, Menu_item *);
extern void		cmpbd_proc(Menu *, Menu_item *);
extern void		cmpopdone_proc(Menu *, Menu_item *);
extern void		cmprmsel_proc(Menu *, Menu_item *);
extern void		cmpubdsel_proc(Menu *, Menu_item *);
extern void		cpsel_proc(Menu *, Menu_item *);
extern void		crvattrabort_proc(Panel *, Panel_item *);
extern void		crvattrcont_proc(Panel *, Panel_item *);
extern void		crvattrdash_proc(Panel *, Panel_item *);
extern void		crvattrdtk_proc(Panel *, Panel_item *);
extern void		crvattrfg_proc(Panel *, Panel_item *);
extern void		crvattrline_proc(Panel *, Panel_item *);
extern void		crvattrlinewd_proc(Panel *, Panel_item *);
extern void		crvattrmvcnt1_proc(Panel *, Panel_item *);
extern void		crvattrmvcnt2_proc(Panel *, Panel_item *);
extern void		crvattrmvloc1_proc(Panel *, Panel_item *);
extern void		crvattrmvloc2_proc(Panel *, Panel_item *);
extern void		delall_proc(Menu *, Menu_item *);
extern void		delsel_proc(Menu *, Menu_item *);
extern void		dmpallips_proc(Menu *, Menu_item *);
extern void		dmpallras_proc(Menu *, Menu_item *);
extern void		dmpipsabort_proc(Panel *, Panel_item *);
extern void		dmpipscont_proc(Panel *, Panel_item *);
extern void		dmpipsmode_proc(Panel *, Panel_item *);
extern void		dmpsel_proc(Menu *, Menu_item *);
extern void		exit_proc(Menu *, Menu_item *);
extern void		gdfattrabort_proc(Panel *, Panel_item *);
extern void		gdfattrbg_proc(Panel *, Panel_item *);
extern void		gdfattrbnd_proc(Panel *, Panel_item *);
extern void		gdfattrcont_proc(Panel *, Panel_item *);
extern void		gdfattrdtk_proc(Panel *, Panel_item *);
extern void		gdfattrfg_proc(Panel *, Panel_item *);
extern void		gdfattrfill_proc(Panel *, Panel_item *);
extern void		gdfattrfontfamily_proc(Panel *, Panel_item *);
extern void		get_pathdirs();
extern void		get_psfontfamilies();
extern void		get_psfonts();
extern void		get_xdefaults(char *);
extern void		ice_err(char *, int);
extern void		insice_proc(Menu *, Menu_item *);
extern void		insiceabort_proc(Panel *, Panel_item *);
extern void		insicecont_proc(Panel *, Panel_item *);
extern void		insicegdf_proc(Panel *, Panel_item *);
extern void		insattrabort_proc(Panel *, Panel_item *);
extern void		insattrcont_proc(Panel *, Panel_item *);
extern void		insaxis_proc(Menu *, Menu_item *);
extern void		inscrv_proc(Menu *, Menu_item *);
extern void		insmrk_proc(Menu *, Menu_item *);
extern void		inspoly_proc(Menu *, Menu_item *);
extern void		inspsd_proc(Menu *, Menu_item *);
extern void		inspth_proc(Menu *, Menu_item *);
extern void		insras_proc(Menu *, Menu_item *);
extern void		insrect_proc(Menu *, Menu_item *);
extern void		instext_proc(Menu *, Menu_item *);
extern void		insvec_proc(Menu *, Menu_item *);
extern void		locattrabort_proc(Panel *, Panel_item *);
extern void		locattrcont_proc(Panel *, Panel_item *);
extern void		mrkattrabort_proc(Panel *, Panel_item *);
extern void		mrkattrcont_proc(Panel *, Panel_item *);
extern void		mrkattrbndclr_proc(Panel *, Panel_item *);
extern void		mrkattrbndmode_proc(Panel *, Panel_item *);
extern void		mrkattrbndwd_proc(Panel *, Panel_item *);
extern void		mrkattrdtk_proc(Panel *, Panel_item *);
extern void		mrkattrfillclr_proc(Panel *, Panel_item *);
extern void		mrkattrfillmode_proc(Panel *, Panel_item *);
extern void		mrkattrsz_proc(Panel *, Panel_item *);
extern void		pgattrabort_proc(Panel *, Panel_item *);
extern void		pgattrbg_proc(Panel *, Panel_item *);
extern void		pgattrcont_proc(Panel *, Panel_item *);
extern void		pgattrloc_proc(Panel *, Panel_item *);
extern void		pgattrunits_proc(Panel *, Panel_item *);
extern void		pgdsp_proc(Menu *, Menu_item *);
extern void		polyattrabort_proc(Panel *, Panel_item *);
extern void		polyattrcls_proc(Panel *, Panel_item *);
extern void		polyattrcont_proc(Panel *, Panel_item *);
extern void		polyattrbndclr_proc(Panel *, Panel_item *);
extern void		polyattrbndmode_proc(Panel *, Panel_item *);
extern void		polyattrbndwd_proc(Panel *, Panel_item *);
extern void		polyattrdtk_proc(Panel *, Panel_item *);
extern void		polyattrfillclr_proc(Panel *, Panel_item *);
extern void		polyattrfillmode_proc(Panel *, Panel_item *);
extern void		psdattrabort_proc(Panel *, Panel_item *);
extern void		psdattrcont_proc(Panel *, Panel_item *);
extern void		psdattrdtk_proc(Panel *, Panel_item *);
extern void		pthattrabort_proc(Panel *, Panel_item *);
extern void		pthattrcont_proc(Panel *, Panel_item *);
extern void		pthattrfg_proc(Panel *, Panel_item *);
extern void		pthattrsrc_proc(Panel *, Panel_item *);
extern void		pthattrvis_proc(Panel *, Panel_item *);
extern void		rasattrabort_proc(Panel *, Panel_item *);
extern void		rasattrbg_proc(Panel *, Panel_item *);
extern void		rasattrcont_proc(Panel *, Panel_item *);
extern void		rasattrfg_proc(Panel *, Panel_item *);
extern void		rectattrabort_proc(Panel *, Panel_item *);
extern void		rectattrcont_proc(Panel *, Panel_item *);
extern void		rectattrbndclr_proc(Panel *, Panel_item *);
extern void		rectattrbndmode_proc(Panel *, Panel_item *);
extern void		rectattrbndwd_proc(Panel *, Panel_item *);
extern void		rectattrdash_proc(Panel *, Panel_item *);
extern void		rectattrdimmode_proc(Panel *, Panel_item *);
extern void		rectattrdtk_proc(Panel *, Panel_item *);
extern void		rectattrfillclr_proc(Panel *, Panel_item *);
extern void		rectattrfillmode_proc(Panel *, Panel_item *);
extern void		rectattrline_proc(Panel *, Panel_item *);
extern void		textattrabort_proc(Panel *, Panel_item *);
extern void		textattrcont_proc(Panel *, Panel_item *);
extern void		textattrbg_proc(Panel *, Panel_item *);
extern void		textattrbgmode_proc(Panel *, Panel_item *);
extern void		textattrdtk_proc(Panel *, Panel_item *);
extern void		textattrfg_proc(Panel *, Panel_item *);
extern void		textattrfont_proc(Panel *, Panel_item *);
extern void		textattrfontfamily_proc(Panel *, Panel_item *);
extern void		textattrfontsz_proc(Panel *, Panel_item *);
extern void		textattrjust_proc(Panel *, Panel_item *);
extern void		textattrsrc_proc(Panel *, Panel_item *);
extern void		trsel_proc(Menu *, Menu_item *);
extern void		vecattrabort_proc(Panel *, Panel_item *);
extern void		vecattrcont_proc(Panel *, Panel_item *);
extern void		vecattrdash_proc(Panel *, Panel_item *);
extern void		vecattrdtk_proc(Panel *, Panel_item *);
extern void		vecattrfg_proc(Panel *, Panel_item *);
extern void		vecattrline_proc(Panel *, Panel_item *);
extern void		vecattrlinewd_proc(Panel *, Panel_item *);
extern void		vecattrmvloc1_proc(Panel *, Panel_item *);
extern void		vecattrmvloc2_proc(Panel *, Panel_item *);
extern void		vecattrptr_proc(Panel *, Panel_item *);
extern void		vecattrptrstyle_proc(Panel *, Panel_item *);

static XWMHints xwmh= {
	(InputHint | StateHint),	/* flags */
	True,				/* input */
	NormalState,			/* initial_state */
	0,				/* icon pixmap */
	0,				/* icon window */
	0, 0,				/* icon location */
	0,				/* icon mask */
	0,				/* Window group */
};

static Pixmap hourglass_pm;
static Atom wmpr_atoms[WMPR_ATOMS];

void
init_windows()
{
	PSFILE *psf;
	XVisualInfo visinfo;
	XWindowAttributes xwa;
	Colormap root_cmap, static_cmap;
	char *xtonewsdpystr(char *);
	void init_cursors();
	void init_canvas();
	void init_pgattrpanel();
	void init_gdfattrpanel();
	void init_insattrpanel();
	void init_psdattrpanel();
	void init_rasattrpanel();
	void init_textattrpanel();
	void init_vecattrpanel();
	void init_crvattrpanel();
	void init_mrkattrpanel();
	void init_rectattrpanel();
	void init_polyattrpanel();
	void init_axisattrpanel();
	void init_cmpattrpanel();
	void init_pthattrpanel();
	void init_locattrpanel();
	void init_dmpipspanel();
	void init_insicepanel();

	/* convert X11 display name to NeWS display name */
	if (xdisplay != (char *) NULL) {
		if ((newsdisplay= xtonewsdpystr(xdisplay)) == (char *) NULL)
			exit(-1);
	}

	/* open X connection */
	if ((dpy= XOpenDisplay(xdisplay)) == NULL) {
		(void) fprintf(stderr, "%s: can't open display %s\n", progname, xdisplay);
		exit(-1);
	}

	/* open NeWS connection */
	if (newsdisplay == (char *) NULL)
		newsdisplay= getenv("NEWSSERVER");
	if ((psf= ps_open_server(newsdisplay)) == 0) {
		fprintf(stderr, "%s: can't connnect to NeWS display\n", progname);
		exit(-1);
	}
	if (PostScript)
		ps_flush_PostScript();
	PostScriptInput= psf;
	PostScript= psio_getassoc(psf);
	news_pathtextinit();
	news_vectorinit();
	news_syncserver();

	screen= DefaultScreen(dpy);
	root_win= DefaultRootWindow(dpy);

	switch (DefaultDepth(dpy, screen)) {
	case 1:
		pg_pixdepth= 1;
		if (XMatchVisualInfo(dpy, screen, 1, StaticGray, &visinfo) == 0) {
			(void) fprintf(stderr, "%s: 1-bit StaticGray unavailable on display %s\n", progname, DisplayString(dpy));
			exit(-1);
		}
		def_cmap= DefaultColormap(dpy, screen);
		visual= visinfo.visual;
		cmap= def_cmap;
		cmap_defcolors(def_cmap, 2);
		pg_bgpixel= cmap_lookup(pg_rbg, pg_gbg, pg_bbg, 2);
		black_pixel= cmap_lookup((unsigned char) 0, (unsigned char) 0, (unsigned char) 0, 2);
		white_pixel= cmap_lookup((unsigned char) 255, (unsigned char) 255, (unsigned char) 255, 2);
		break;

	case 8:
		pg_pixdepth= 8;

		/* need PseudoColor so that the colormap can be changed */
		if (XMatchVisualInfo(dpy, screen, 8, PseudoColor, &visinfo) == 0) {
			(void) fprintf(stderr, "%s: 8-bit PseudoColor unavailable on display %s\n", progname, DisplayString(dpy));
			exit(-1);
		}
		visual= visinfo.visual;
		def_cmap= XCreateColormap(dpy, root_win, visual, AllocAll);

		/* the following block of code is highly dependent upon color
		   policies implemented by OpenWindows 2.0 -- this has changed
		   significantly from previous releases and may well change
		   again in future releases */
		/*********** BEGIN VOLATILE CODE ***************/
		XGetWindowAttributes(dpy, root_win, &xwa);
		root_cmap= xwa.colormap;

		if (XMatchVisualInfo(dpy, screen, 8, StaticColor, &visinfo) == 0) {
			fprintf(stderr, "%s: can't retrieve NeWS static colormap\n", progname);
			exit(-1);
		}
		static_cmap= XCreateColormap(dpy, root_win, visinfo.visual, AllocNone);

		/* copy the first few colors from the root window colormap --
		   these are used to draw the root background and window titlebars,
		   and should be copied into our colormap to avoid annoying
		   color flashes when moving to and from our windows */
		cmap_copyrange(root_cmap, def_cmap, 0, STATICCOLOR_FREESZ);

		/* copy the rest of the colormap from the StaticColor map -- this
		   is the 'color cube' used by NeWS for all PostScript rendering */
		cmap_copyrange(static_cmap, def_cmap, STATICCOLOR_FREESZ, STATICCOLOR_CUBESZ);
		XFreeColormap(dpy, static_cmap);
		/************ END VOLATILE CODE ****************/

		cmap= XCreateColormap(dpy, root_win, visual, AllocAll);
		cmap_defcolors(def_cmap, PSEUDOCOLOR_MAPSZ);
		cmap_copy(def_cmap, cmap);
		pg_bgpixel= cmap_lookup(pg_rbg, pg_gbg, pg_bbg, PSEUDOCOLOR_MAPSZ);
		black_pixel= cmap_lookup((unsigned char) 0, (unsigned char) 0, (unsigned char) 0, PSEUDOCOLOR_MAPSZ);
		white_pixel= cmap_lookup((unsigned char) 255, (unsigned char) 255, (unsigned char) 255, PSEUDOCOLOR_MAPSZ);
		break;
	}
	curr_cmap= def_cmap;

	get_xdefaults(progname);
	get_pathdirs();
	get_psfonts();
	get_psfontfamilies();

	wmpr_atom= XInternAtom(dpy, "WM_PROTOCOLS", False);
	wmsy_atom= wmpr_atoms[0]= XInternAtom(dpy, "WM_SAVE_YOURSELF", False);
	wmdw_atom= wmpr_atoms[1]= XInternAtom(dpy, "WM_DELETE_WINDOW", False);

	init_cursors();
	init_canvas();
	init_pgattrpanel();
	init_gdfattrpanel();
	init_insattrpanel();
	init_psdattrpanel();
	init_rasattrpanel();
	if (npsfonts > 0)
		init_textattrpanel();
	init_vecattrpanel();
	init_crvattrpanel();
	init_mrkattrpanel();
	init_rectattrpanel();
	init_polyattrpanel();
	if (npsfonts > 0)
		init_axisattrpanel();
	init_cmpattrpanel();
	init_pthattrpanel();
	init_locattrpanel();
	init_dmpipspanel();
	init_insicepanel();

	(void) alert_setdefaults(LXA_FONT, fontnm,
				LXA_FOREGROUND, fg_pixel,
				LXA_BACKGROUND, bg_pixel,
				LXA_BWIDTH, border_wd,
				LXA_VISUAL, visual,
				LXA_COLORMAP, cmap,
				LXA_NULL);

	XDefineCursor(dpy, pg_cwin, std_cursor);
	XMapRaised(dpy, pg_frame);
	return;
}

void
init_cursors()
{
	XImage hourglass_image;
	XColor fg_xc, bg_xc;
	GC gc;
	XGCValues gcv;

	std_cursor= XCreateFontCursor(dpy, XC_arrow);

	hourglass_pm= XCreatePixmap(dpy, DefaultRootWindow(dpy), hourglass_width, hourglass_height, 1);
	gcv.foreground= 1;
	gcv.background= 0;
	gc= XCreateGC(dpy, hourglass_pm, GCForeground | GCBackground, &gcv);
	hourglass_image.height= hourglass_height;
	hourglass_image.width= hourglass_width;
	hourglass_image.xoffset= 0;
	hourglass_image.format= XYBitmap;
	hourglass_image.data= hourglass_bits;
	hourglass_image.byte_order= LSBFirst;
	hourglass_image.bitmap_unit= 8;
	hourglass_image.bitmap_bit_order= LSBFirst;
	hourglass_image.bitmap_pad= 8;
	hourglass_image.bytes_per_line= (hourglass_width+7)>>3;
	hourglass_image.depth= 1;
	XPutImage(dpy, hourglass_pm, gc, &hourglass_image, 0, 0, 0, 0, hourglass_width, hourglass_height);
	XFreeGC(dpy, gc);
	fg_xc.pixel= fg_pixel;
	XQueryColor(dpy, def_cmap, &fg_xc);
	fg_xc.flags= DoRed | DoGreen | DoBlue;
	bg_xc.pixel= bg_pixel;
	XQueryColor(dpy, def_cmap, &bg_xc);
	bg_xc.flags= DoRed | DoGreen | DoBlue;
	hg_cursor= XCreatePixmapCursor(dpy, hourglass_pm, hourglass_pm, &fg_xc, &bg_xc, hourglass_x_hot, hourglass_y_hot);
}

void
init_canvas()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	XGCValues gcv;
	void init_menus();

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL)
		ice_err("Memory allocation error.", FATAL);
	xsh->flags= (PPosition | PSize);
	xsh->width= pg_framewidth;
	xsh->height= pg_frameheight;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	pg_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, pg_frame, wmpr_atoms, WMPR_ATOMS);
	XSetStandardProperties(dpy, pg_frame, "ICE Page", "ICE Pg", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, pg_frame, &xwmh);
	XFree((char *) xsh);

	pg_pixwidth= (int) (pg_width*((double) pg_dpi));
	pg_pixheight= (int) (pg_height*((double) pg_dpi));

	if ((pg_canvas= canvas_create(progname, dpy, pg_frame,
			LXC_WIDTH, pg_pixwidth,
			LXC_HEIGHT, pg_pixheight,
			LXC_FOREGROUND, fg_pixel,
			LXC_BACKGROUND, bg_pixel,
			LXC_NULL)) == (Canvas *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	pg_cwin= *((Window *) canvas_get(pg_canvas, LXC_WINDOW));
	pg_cpm= *((Pixmap *) canvas_get(pg_canvas, LXC_PIXMAP));

	gcv.function= GXcopy;
	gcv.font= font->fid;
	gcv.foreground= fg_pixel;
	gcv.background= bg_pixel;
	gcv.plane_mask= AllPlanes; /**************/
	pg_gc= XCreateGC(dpy, pg_cwin, (GCFunction | GCFont | GCForeground | GCBackground | GCPlaneMask), &gcv);

	gcv.function= GXor;
	pg_ogc= XCreateGC(dpy, pg_cwin, (GCFunction | GCFont | GCForeground | GCBackground | GCPlaneMask), &gcv);

	gcv.function= GXinvert;
	gcv.plane_mask= fg_pixel ^ bg_pixel;
	pg_igc= XCreateGC(dpy, pg_cwin, (GCFunction | GCFont | GCForeground | GCBackground | GCPlaneMask), &gcv);

	gcv.function= GXxor;
	pg_xgc= XCreateGC(dpy, pg_cwin, (GCFunction | GCFont | GCForeground | GCPlaneMask), &gcv);

	gcv.function= GXcopy;
	gcv.foreground= bg_pixel;
	gcv.plane_mask= AllPlanes;
	pg_cgc= XCreateGC(dpy, pg_cwin, (GCFunction | GCFont | GCForeground | GCPlaneMask), &gcv);

	gcv.function= GXcopy;
	gcv.foreground= pg_bgpixel;
	gcv.plane_mask= AllPlanes;
	pg_bggc= XCreateGC(dpy, pg_cwin, (GCFunction | GCForeground | GCPlaneMask), &gcv);

	/* canvas input mask */
	XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);

	XFillRectangle(dpy, pg_cpm, pg_bggc, 0, 0, pg_pixwidth, pg_pixheight);

	init_menus();
	return;
}

void
init_menus()
{
	Menu_item *mi;

	if ((ins_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Insert",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PROC, inspsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PROC, insras_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PROC, instext_proc,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(ins_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PROC, insvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PROC, inscrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PROC, insmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PROC, insrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PROC, inspoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PROC, insaxis_proc,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(ins_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Path",
				LXMI_PROC, inspth_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "ICE",
				LXMI_PROC, insice_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(ins_menu, mi);

	if ((delpsd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((delras_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((deltext_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((delvec_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((delcrv_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((delmrk_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((delrect_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((delpoly_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((delaxis_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((delcmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((delpth_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);


	if ((del_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Delete",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, delsel_proc,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PULLRIGHT, delpsd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PULLRIGHT, delras_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PULLRIGHT, deltext_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(del_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PULLRIGHT, delvec_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PULLRIGHT, delcrv_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PULLRIGHT, delmrk_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PULLRIGHT, delrect_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PULLRIGHT, delpoly_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PULLRIGHT, delaxis_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(del_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Composite",
				LXMI_PULLRIGHT, delcmp_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Path",
				LXMI_PULLRIGHT, delpth_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "All",
				LXMI_PROC, delall_proc,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(del_menu, mi);

	if ((attrpsd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((attrras_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((attrtext_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((attrvec_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((attrcrv_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((attrmrk_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((attrrect_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((attrpoly_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((attraxis_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((attrpth_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((attr_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Attributes",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Page",
				LXMI_PROC, attrpg_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Default",
				LXMI_PROC, attrgdf_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Insert",
				LXMI_PROC, attrins_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, attrsel_proc,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PULLRIGHT, attrpsd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PULLRIGHT, attrras_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PULLRIGHT, attrtext_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(attr_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PULLRIGHT, attrvec_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PULLRIGHT, attrcrv_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PULLRIGHT, attrmrk_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PULLRIGHT, attrrect_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PULLRIGHT, attrpoly_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PULLRIGHT, attraxis_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(attr_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Path",
				LXMI_PULLRIGHT, attrpth_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(attr_menu, mi);

	if ((trpsd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((trras_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((trtext_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((trvec_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((trcrv_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((trmrk_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((trrect_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((trpoly_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((traxis_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((trcmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((trpth_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((tr_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Translate",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, trsel_proc,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PULLRIGHT, trpsd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PULLRIGHT, trras_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PULLRIGHT, trtext_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(tr_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PULLRIGHT, trvec_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PULLRIGHT, trcrv_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PULLRIGHT, trmrk_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PULLRIGHT, trrect_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PULLRIGHT, trpoly_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PULLRIGHT, traxis_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(tr_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Composite",
				LXMI_PULLRIGHT, trcmp_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Path",
				LXMI_PULLRIGHT, trpth_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(tr_menu, mi);

	if ((cppsd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cpras_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((cptext_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((cpvec_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cpcrv_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cpmrk_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cprect_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cppoly_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((cpaxis_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((cpcmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((cp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Copy",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, cpsel_proc,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PULLRIGHT, cppsd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PULLRIGHT, cpras_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PULLRIGHT, cptext_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(cp_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PULLRIGHT, cpvec_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PULLRIGHT, cpcrv_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PULLRIGHT, cpmrk_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PULLRIGHT, cprect_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PULLRIGHT, cppoly_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PULLRIGHT, cpaxis_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(cp_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Composite",
				LXMI_PULLRIGHT, cpcmp_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cp_menu, mi);

	if ((cmpattr_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Attributes",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, cmpattrsel_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpattr_menu, mi);

	if ((cmpubd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Unbind",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, cmpubdsel_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpubd_menu, mi);

	if ((cmpadd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Add",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, cmpaddsel_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpadd_menu, mi);

	if ((cmprm_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Remove",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, cmprmsel_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmprm_menu, mi);

	if ((cmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Composite",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Bind",
				LXMI_PROC, cmpbd_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Attributes",
				LXMI_PULLRIGHT, cmpattr_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Unbind",
				LXMI_PULLRIGHT, cmpubd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Add",
				LXMI_PULLRIGHT, cmpadd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Remove",
				LXMI_PULLRIGHT, cmprm_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmp_menu, mi);

	if ((cmpoppsd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cmpopras_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((cmpoptext_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((cmpopvec_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cmpopcrv_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cmpopmrk_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cmpoprect_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((cmpoppoly_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((cmpopaxis_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((cmpopcmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((cmpop_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Done",
				LXMI_PROC, cmpopdone_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PULLRIGHT, cmpoppsd_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PULLRIGHT, cmpopras_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PULLRIGHT, cmpoptext_menu,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(cmpop_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PULLRIGHT, cmpopvec_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PULLRIGHT, cmpopcrv_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PULLRIGHT, cmpopmrk_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PULLRIGHT, cmpoprect_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PULLRIGHT, cmpoppoly_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PULLRIGHT, cmpopaxis_menu,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(cmpop_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Composite",
				LXMI_PULLRIGHT, cmpopcmp_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmpop_menu, mi);

	if ((cmap_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Colormap",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Default",
				LXMI_PROC, cmapdef_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(cmap_menu, mi);

	if ((dmpall_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "ICE/PostScript",
				LXMI_PROC, dmpallips_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmpall_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PROC, dmpallras_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmpall_menu, mi);

	if ((dmppsd_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((dmpras_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((dmptext_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((dmpvec_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((dmpcrv_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((dmpmrk_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((dmprect_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((dmppoly_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if (npsfonts > 0) {
		if ((dmpaxis_menu= menu_create(progname, dpy, pg_cwin,
					LXM_FOREGROUND, fg_pixel,
					LXM_BACKGROUND, bg_pixel,
					LXM_NULL)) == (Menu *) NULL)
			ice_err("Internal toolkit error.", FATAL);
	}
	if ((dmpcmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((dmppth_menu= menu_create(progname, dpy, pg_cwin,
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((dmp_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Dump",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "All",
				LXMI_PULLRIGHT, dmpall_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Select",
				LXMI_PROC, dmpsel_proc,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "PS Document",
				LXMI_PULLRIGHT, dmppsd_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Raster",
				LXMI_PULLRIGHT, dmpras_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Text",
					LXMI_PULLRIGHT, dmptext_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(dmp_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Vector",
				LXMI_PULLRIGHT, dmpvec_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Curve",
				LXMI_PULLRIGHT, dmpcrv_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Marker",
				LXMI_PULLRIGHT, dmpmrk_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Rectangle",
				LXMI_PULLRIGHT, dmprect_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Polygon",
				LXMI_PULLRIGHT, dmppoly_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if (npsfonts > 0) {
		if ((mi= menuitem_create(LXMI_STRING, "Axis",
					LXMI_PULLRIGHT, dmpaxis_menu,
					LXMI_STATE, LXMI_INACTIVE,
					LXMI_NULL)) == (Menu_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);
		(void) menuitem_insert(dmp_menu, mi);
	}
	if ((mi= menuitem_create(LXMI_STRING, "Composite",
				LXMI_PULLRIGHT, dmpcmp_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Path",
				LXMI_PULLRIGHT, dmppth_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(dmp_menu, mi);

	if ((pg_menu= menu_create(progname, dpy, pg_cwin,
				LXM_TITLE, "Main",
				LXM_FOREGROUND, fg_pixel,
				LXM_BACKGROUND, bg_pixel,
				LXM_NULL)) == (Menu *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mi= menuitem_create(LXMI_STRING, "Insert",
				LXMI_PULLRIGHT, ins_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Delete",
				LXMI_PULLRIGHT, del_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Attributes",
				LXMI_PULLRIGHT, attr_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Translate",
				LXMI_PULLRIGHT, tr_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Copy",
				LXMI_PULLRIGHT, cp_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Composite",
				LXMI_PULLRIGHT, cmp_menu,
				LXMI_STATE, LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Colormap",
				LXMI_PULLRIGHT, cmap_menu,
				LXMI_STATE, (visual->c_class == PseudoColor) ? LXMI_ACTIVE : LXMI_INACTIVE,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Dump",
				LXMI_PULLRIGHT, dmp_menu,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Redisplay",
				LXMI_PROC, pgdsp_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);
	if ((mi= menuitem_create(LXMI_STRING, "Exit",
				LXMI_PROC, exit_proc,
				LXMI_NULL)) == (Menu_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) menuitem_insert(pg_menu, mi);

	return;
}

void
init_pgattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	pgattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, pgattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((pgattr_panel= panel_create(progname, dpy, pgattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((pgattr_w= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pgattr_h= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x+(40*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Height:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_dpi= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dots Per Inch:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_update= panelitem_create(pgattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Update Mode:",
			LXPENUM_SELSTRINGS, "Automatic", "Manual", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_loc= panelitem_create(pgattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Location Mode:",
			LXPENUM_SELSTRINGS, "Cursor", "Text", (char *) NULL,
			LXPI_PROC, pgattrloc_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_locdisplay= panelitem_create(pgattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Location Display:",
			LXPENUM_SELSTRINGS, "Enable", "Disable", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_units= panelitem_create(pgattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Displayed Units:",
			LXPENUM_SELSTRINGS, "Pixels", "Points", "Inches", "User Defined", (char *) NULL,
			LXPI_PROC, pgattrunits_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_hsi= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Horizontal Units Per Inch:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_vsi= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Vertical Units Per Inch:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_xri= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "X Reference (Inches):",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pgattr_yri= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x+(40*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Y Reference (Inches):",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_xru= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "X Reference (User):",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pgattr_yru= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x+(40*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Y Reference (User):",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_clip= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_originhlt= panelitem_create(pgattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Object Origin Highlight:",
			LXPENUM_SELSTRINGS, "Disable", "Enable", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_bg= panelitem_create(pgattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Background Color:",
			LXPENUM_SELSTRINGS, "White", "Black", "Other", (char *) NULL,
			LXPI_PROC, pgattrbg_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pgattr_rbg= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pgattr_gbg= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pgattr_bbg= panelitem_create(pgattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((pgattr_cont= panelitem_create(pgattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, pgattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(pgattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((pgattr_abort= panelitem_create(pgattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, pgattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(pgattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(pgattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, pgattr_frame, "ICE Page Attributes", "ICE PgAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, pgattr_frame, &xwmh);
	XSetNormalHints(dpy, pgattr_frame, xsh);
	XMoveResizeWindow(dpy, pgattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_gdfattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y, i;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	gdfattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, gdfattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((gdfattr_panel= panel_create(progname, dpy, gdfattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if (npsfonts > 0) {
		boolean found;
		int ff, fn;

		if ((gdfattr_fontfamily= panelitem_create(gdfattr_panel, LXPI_CYCLE,
				LXPI_X, x,
				LXPI_Y, y,
				LXPI_STRING, "Font Family:",
				LXPI_PROC, gdfattrfontfamily_proc,
				LXPI_NULL)) == (Panel_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);

		y+= (int) (fontht*1.5);
		for (i= 0; i < npsfontfamilies; i++) {
			int j;

			if (panelitem_addsel(gdfattr_panel, gdfattr_fontfamily,
					LXPENUM_SELSTRING, psfontfamilies[i].psff_name,
					LXPI_NULL) != LX_SUCCESS)
				ice_err("Internal toolkit error.", FATAL);
			if ((psfontfamilies[i].psff_gdfpi= panelitem_create(gdfattr_panel, LXPI_CYCLE,
					LXPI_X, x,
					LXPI_Y, y,
					LXPI_STRING, "Font Name:",
					LXPI_STATE, LXPI_INACTIVE,
					LXPI_NULL)) == (Panel_item *) NULL)
				ice_err("Internal toolkit error.", FATAL);

			for (j= 0; j < psfontfamilies[i].psff_count; j++) {
				if (panelitem_addsel(gdfattr_panel, psfontfamilies[i].psff_gdfpi,
						LXPENUM_SELSTRING, psfontfamilies[i].psff_fonts[j],
						LXPI_NULL) != LX_SUCCESS)
					ice_err("Internal toolkit error.", FATAL);
			}
			(void) panelitem_set(gdfattr_panel, psfontfamilies[i].psff_gdfpi,
					LXPENUM_VALUE, 0,
					LXPI_NULL);
		}
		(void) panelitem_set(gdfattr_panel, gdfattr_fontfamily,
				LXPENUM_VALUE, 0,
				LXPI_NULL);

		y+= (int) (fontht*1.5);
		if ((gdfattr_fontsize= panelitem_create(gdfattr_panel, LXPI_TEXT,
				LXPI_X, x,
				LXPI_Y, y,
				LXPI_STRING, "Font Size:",
				LXPTEXT_MAXSTORE, 10,
				LXPTEXT_MAXDISPLAY, 10,
				LXPI_NULL)) == (Panel_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);

		if ((gdfattr_fontlead= panelitem_create(gdfattr_panel, LXPI_TEXT,
				LXPI_X, x+(40*charwd),
				LXPI_Y, y,
				LXPI_STRING, "Font Leading:",
				LXPTEXT_MAXSTORE, 10,
				LXPTEXT_MAXDISPLAY, 10,
				LXPI_NULL)) == (Panel_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);

		found= FALSE;
		for (ff= 0; ff < npsfontfamilies; ff++) {
			for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
				if (!strcmp("Times-Roman", psfontfamilies[ff].psff_fonts[fn])) {
					found= TRUE;
					break;
				}
			}
			if (found == TRUE)
				break;
		}
		if (found == TRUE) {
			gdf_fontname= psfontfamilies[ff].psff_fonts[fn];
			gdf_psff= psfontfamilies[ff].psff_gdfpi;
		}
		else {
			gdf_fontname= psfontfamilies[0].psff_fonts[0];
			gdf_psff= psfontfamilies[0].psff_gdfpi;
		}
		gdf_fontsize= 10.;
		gdf_fontlead= 0.;

		y+= (int) (fontht*1.5);
	}

	if ((gdfattr_linewidth= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Line Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_fg= panelitem_create(gdfattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Foreground Color:",
			LXPENUM_SELSTRINGS, "Black", "White", "Other", (char *) NULL,
			LXPI_PROC, gdfattrfg_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_rfg= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_gfg= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_bfg= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_bg= panelitem_create(gdfattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Background Color:",
			LXPENUM_SELSTRINGS, "White", "Black", "Other", (char *) NULL,
			LXPI_PROC, gdfattrbg_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_rbg= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_gbg= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_bbg= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_mrktype= panelitem_create(gdfattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Marker Type:",
			LXPENUM_SELSTRINGS, "Square", "Triangle", "Circle", "Cross", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_mrksize= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Marker Radius:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_bndwidth= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_bnd= panelitem_create(gdfattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Color:",
			LXPENUM_SELSTRINGS, "Black", "White", "Other", (char *) NULL,
			LXPI_PROC, gdfattrbnd_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_rbnd= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_gbnd= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_bbnd= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_fill= panelitem_create(gdfattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill Color:",
			LXPENUM_SELSTRINGS, "White", "Black", "Other", (char *) NULL,
			LXPI_PROC, gdfattrfill_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_rfill= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_gfill= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_bfill= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_dtk= panelitem_create(gdfattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPENUM_SELSTRINGS, "White", "Black", "Other", (char *) NULL,
			LXPI_PROC, gdfattrdtk_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((gdfattr_rdtk= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_gdtk= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((gdfattr_bdtk= panelitem_create(gdfattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((gdfattr_cont= panelitem_create(gdfattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, gdfattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(gdfattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((gdfattr_abort= panelitem_create(gdfattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, gdfattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(gdfattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(gdfattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, gdfattr_frame, "ICE Default Attributes", "ICE DefAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, gdfattr_frame, &xwmh);
	XSetNormalHints(dpy, gdfattr_frame, xsh);
	XMoveResizeWindow(dpy, gdfattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_insattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	insattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, insattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((insattr_panel= panel_create(progname, dpy, insattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((insattr_newobj= panelitem_create(insattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "New Object Attributes:",
			LXPENUM_SELSTRINGS, "Default", "Last Edit", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((insattr_cont= panelitem_create(insattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, insattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(insattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((insattr_abort= panelitem_create(insattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, insattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(insattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(insattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, insattr_frame, "ICE Insert Attributes", "ICE InsAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, insattr_frame, &xwmh);
	XSetNormalHints(dpy, insattr_frame, xsh);
	XMoveResizeWindow(dpy, insattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_psdattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;
	char buf[10];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	psdattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, psdattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((psdattr_panel= panel_create(progname, dpy, psdattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((psdattr_fname= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Filename:",
			LXPTEXT_MAXSTORE, MAX_FILENMLEN,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((psdattr_hscale= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Horizontal Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((psdattr_vscale= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Vertical Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((psdattr_rot= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Rotation:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((psdattr_clip= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((psdattr_dtk= panelitem_create(psdattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, psdattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", (int) gdf_rdtk);
	if ((psdattr_rdtk= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", (int) gdf_gdtk);
	if ((psdattr_gdtk= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", (int) gdf_bdtk);
	if ((psdattr_bdtk= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((psdattr_seq= panelitem_create(psdattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((psdattr_cont= panelitem_create(psdattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, psdattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(psdattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((psdattr_abort= panelitem_create(psdattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, psdattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(psdattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(psdattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, psdattr_frame, "ICE PS Document Attributes", "ICE PsAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, psdattr_frame, &xwmh);
	XSetNormalHints(dpy, psdattr_frame, xsh);
	XMoveResizeWindow(dpy, psdattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_rasattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;
	char buf[20];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	rasattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, rasattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((rasattr_panel= panel_create(progname, dpy, rasattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((rasattr_fname= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Filename:",
			LXPTEXT_MAXSTORE, MAX_FILENMLEN,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rasattr_pixrep= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Pixel Replication:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rasattr_orig= panelitem_create(rasattr_panel, LXPI_CYCLE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Raster Origin:",
			LXPENUM_SELSTRINGS, "Upper Left", "Lower Left", "Lower Right", "Upper Right", (char *) NULL,
			LXPENUM_VALUE, RASTER_ULORIG,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rasattr_draw= panelitem_create(rasattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Display Mode:",
			LXPENUM_SELSTRINGS, "Full", "Outline", (char *) NULL,
			LXPENUM_VALUE, RASTER_FULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rasattr_fg= panelitem_create(rasattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Foreground Color:",
			LXPI_PROC, rasattrfg_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, RASTER_GLOBALFG,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", (int) gdf_rfg);
	if ((rasattr_rfg= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", (int) gdf_gfg);
	if ((rasattr_gfg= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", (int) gdf_bfg);
	if ((rasattr_bfg= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rasattr_bg= panelitem_create(rasattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Background Color:",
			LXPI_PROC, rasattrbg_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, RASTER_GLOBALBG,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", (int) gdf_rbg);
	if ((rasattr_rbg= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", (int) gdf_gbg);
	if ((rasattr_gbg= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", (int) gdf_bbg);
	if ((rasattr_bbg= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rasattr_seq= panelitem_create(rasattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((rasattr_cont= panelitem_create(rasattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, rasattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(rasattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((rasattr_abort= panelitem_create(rasattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, rasattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(rasattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(rasattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, rasattr_frame, "ICE Raster Attributes", "ICE RasAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, rasattr_frame, &xwmh);
	XSetNormalHints(dpy, rasattr_frame, xsh);
	XMoveResizeWindow(dpy, rasattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_textattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y, i, ff, fn;
	boolean found;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	textattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, textattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((textattr_panel= panel_create(progname, dpy, textattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((textattr_name= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_src= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Source:",
			LXPI_PROC, textattrsrc_proc,
			LXPENUM_SELSTRINGS, "Interactive", "File", (char *) NULL,
			LXPENUM_VALUE, TEXT_USERINPUT,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_text= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Text:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((textattr_filenm= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Filename:",
			LXPTEXT_MAXSTORE, MAX_FILENMLEN,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_font= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Font:",
			LXPI_PROC, textattrfont_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, TEXT_GLOBALFONT,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	found= FALSE;
	for (ff= 0; ff < npsfontfamilies; ff++) {
		for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
			if (!strcmp(gdf_fontname, psfontfamilies[ff].psff_fonts[fn])) {
				found= TRUE;
				break;
			}
		}
		if (found == TRUE)
			break;
	}
	if (!found)
		ff= fn= 0;

	y+= (int) (fontht*1.5);
	if ((textattr_fontfamily= panelitem_create(textattr_panel, LXPI_CYCLE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Font Family:",
			LXPI_PROC, textattrfontfamily_proc,
			LXPENUM_VALUE, ff,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	for (i= 0; i < npsfontfamilies; i++) {
		int j;

		if (panelitem_addsel(textattr_panel, textattr_fontfamily,
				LXPENUM_SELSTRING, psfontfamilies[i].psff_name,
				LXPI_NULL) != LX_SUCCESS)
			ice_err("Internal toolkit error.", FATAL);
		if ((psfontfamilies[i].psff_textpi= panelitem_create(textattr_panel, LXPI_CYCLE,
				LXPI_X, x,
				LXPI_Y, y,
				LXPI_STRING, "Font Name:",
				LXPI_STATE, LXPI_INACTIVE,
				LXPI_NULL)) == (Panel_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);

		for (j= 0; j < psfontfamilies[i].psff_count; j++) {
			if (panelitem_addsel(textattr_panel, psfontfamilies[i].psff_textpi,
					LXPENUM_SELSTRING, psfontfamilies[i].psff_fonts[j],
					LXPI_NULL) != LX_SUCCESS)
				ice_err("Internal toolkit error.", FATAL);
		}
	}

	panelitem_set(textattr_panel, textattr_fontfamily, LXPENUM_VALUE, ff, LXPI_NULL);
	text_psff= psfontfamilies[ff].psff_textpi;
	panelitem_set(textattr_panel, text_psff, LXPENUM_VALUE, fn, LXPI_NULL);

	y+= (int) (fontht*1.5);
	if ((textattr_fontsz= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Font Size:",
			LXPI_PROC, textattrfontsz_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, TEXT_GLOBALFONTSZ,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_fontsize, 10, 0, buf);
	if ((textattr_size= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Size:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) gconvert((double) gdf_fontlead, 10, 0, buf);
	if ((textattr_lead= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*30),
			LXPI_Y, y,
			LXPI_STRING, "Lead:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_just= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Justification:",
			LXPENUM_SELSTRINGS, "Flush Left", "Flush Right", "Center", "Justify", "Path", (char *) NULL,
			LXPENUM_VALUE, TEXT_FLUSHLEFT,
			LXPI_PROC, textattrjust_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_path= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Path Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_pathoffset= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Path Offset:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((textattr_letterspace= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*30),
			LXPI_Y, y,
			LXPI_STRING, "Letterspace:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_fg= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Foreground Color:",
			LXPI_PROC, textattrfg_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, TEXT_GLOBALFG,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfg);
	if ((textattr_rfg= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	if ((textattr_gfg= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	if ((textattr_bfg= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_bgmode= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Background:",
			LXPI_PROC, textattrbgmode_proc,
			LXPENUM_SELSTRINGS, "Transparent", "Opaque", (char *) NULL,
			LXPENUM_VALUE, TEXT_TRANSPARENTBGM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_bg= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Background Color:",
			LXPI_PROC, textattrbg_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, TEXT_GLOBALBG,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rbg);
	if ((textattr_rbg= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gbg);
	if ((textattr_gbg= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bbg);
	if ((textattr_bbg= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_hscale= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Horizontal Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((textattr_vscale= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Vertical Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_rot= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Rotation:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_clip= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_dtk= panelitem_create(textattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, textattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((textattr_rdtk= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((textattr_gdtk= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((textattr_bdtk= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((textattr_seq= panelitem_create(textattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((textattr_cont= panelitem_create(textattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, textattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(textattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((textattr_abort= panelitem_create(textattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, textattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(textattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(textattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, textattr_frame, "ICE Text Attributes", "ICE TextAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, textattr_frame, &xwmh);
	XSetNormalHints(dpy, textattr_frame, xsh);
	XMoveResizeWindow(dpy, textattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_vecattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y, rbx;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	vecattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, vecattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((vecattr_panel= panel_create(progname, dpy, vecattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((vecattr_name= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_linewd= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Line Width:",
			LXPI_PROC, vecattrlinewd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, VECTOR_GLOBALWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	if ((vecattr_lwidth= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_linestyle= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Line Style:",
			LXPI_PROC, vecattrline_proc,
			LXPENUM_SELSTRINGS, "Solid", "Dashed", (char *) NULL,
			LXPENUM_VALUE, VECTOR_SOLID,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_dashstyle= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Style:",
			LXPI_PROC, vecattrdash_proc,
			LXPENUM_SELSTRINGS, "Simple", "Complex", (char *) NULL,
			LXPENUM_VALUE, VECTOR_SIMPLEDASH,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_dashlen= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((vecattr_dashgaplen= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Gap Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((vecattr_dashpattern= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Pattern:",
			LXPTEXT_MAXSTORE, 40,
			LXPTEXT_MAXDISPLAY, 25,
			LXPTEXT_VALUE, "4 4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((vecattr_dashoffset= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Dash Offset:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_capstyle= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Cap Style:",
			LXPENUM_SELSTRINGS, "Butt", "Round", "Square", (char *) NULL,
			LXPENUM_VALUE, VECTOR_BUTTCAP,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_ptr= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Pointers:",
			LXPI_PROC, vecattrptr_proc,
			LXPENUM_SELSTRINGS, "None", "Origin", "Terminus", "Both", (char *) NULL,
			LXPENUM_VALUE, VECTOR_NOPTR,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_ptrstyle= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Pointer Style:",
			LXPI_PROC, vecattrptrstyle_proc,
			LXPENUM_SELSTRINGS, "Open", "Closed", (char *) NULL,
			LXPENUM_VALUE, VECTOR_OPENPTR,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_ptrwd= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Pointer Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "10",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((vecattr_ptrolen= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*30),
			LXPI_Y, y,
			LXPI_STRING, "Outside Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "10",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((vecattr_ptrilen= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*60),
			LXPI_Y, y,
			LXPI_STRING, "Inside Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "10",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_fg= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Foreground Color:",
			LXPI_PROC, vecattrfg_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, VECTOR_GLOBALFG,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfg);
	if ((vecattr_rfg= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	if ((vecattr_gfg= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	if ((vecattr_bfg= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_clip= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_dtk= panelitem_create(vecattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, vecattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((vecattr_rdtk= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((vecattr_gdtk= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((vecattr_bdtk= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((vecattr_seq= panelitem_create(vecattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2);
	if ((vecattr_mvloc1= panelitem_create(vecattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Move Origin",
			LXPI_PROC, vecattrmvloc1_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rbx= (int) *((int *) panel_get(vecattr_panel, LXP_VWIDTH));
	rbx-= strlen("Move Terminus")*charwd;
	if (rbx < (strlen("Move Origin")+9)*charwd)
		rbx= (strlen("Move Origin")+9)*charwd;
	if ((vecattr_mvloc2= panelitem_create(vecattr_panel, LXPI_BUTTON,
			LXPI_X, rbx,
			LXPI_Y, y,
			LXPI_STRING, "Move Terminus",
			LXPI_PROC, vecattrmvloc2_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((vecattr_cont= panelitem_create(vecattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, vecattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rbx+= (strlen("Move Terminus")-strlen("Abort"))*charwd;
	if ((vecattr_abort= panelitem_create(vecattr_panel, LXPI_BUTTON,
			LXPI_X, rbx,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, vecattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(vecattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(vecattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, vecattr_frame, "ICE Vector Attributes", "ICE VecAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, vecattr_frame, &xwmh);
	XSetNormalHints(dpy, vecattr_frame, xsh);
	XMoveResizeWindow(dpy, vecattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_crvattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y, rb, rx;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	crvattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, crvattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((crvattr_panel= panel_create(progname, dpy, crvattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((crvattr_name= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_linewd= panelitem_create(crvattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Line Width:",
			LXPI_PROC, crvattrlinewd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, CURVE_GLOBALWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	if ((crvattr_lwidth= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_linestyle= panelitem_create(crvattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Line Style:",
			LXPI_PROC, crvattrline_proc,
			LXPENUM_SELSTRINGS, "Solid", "Dashed", (char *) NULL,
			LXPENUM_VALUE, CURVE_SOLID,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_dashstyle= panelitem_create(crvattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Style:",
			LXPI_PROC, crvattrdash_proc,
			LXPENUM_SELSTRINGS, "Simple", "Complex", (char *) NULL,
			LXPENUM_VALUE, CURVE_SIMPLEDASH,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_dashlen= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((crvattr_dashgaplen= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Gap Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((crvattr_dashpattern= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Pattern:",
			LXPTEXT_MAXSTORE, 40,
			LXPTEXT_MAXDISPLAY, 25,
			LXPTEXT_VALUE, "4 4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((crvattr_dashoffset= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Dash Offset:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_capstyle= panelitem_create(crvattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Cap Style:",
			LXPENUM_SELSTRINGS, "Butt", "Round", "Square", (char *) NULL,
			LXPENUM_VALUE, CURVE_BUTTCAP,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_fg= panelitem_create(crvattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Foreground Color:",
			LXPI_PROC, crvattrfg_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, CURVE_GLOBALFG,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfg);
	if ((crvattr_rfg= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	if ((crvattr_gfg= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	if ((crvattr_bfg= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_clip= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_dtk= panelitem_create(crvattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, crvattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((crvattr_rdtk= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((crvattr_gdtk= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((crvattr_bdtk= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((crvattr_seq= panelitem_create(crvattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2);
	if ((crvattr_mvloc1= panelitem_create(crvattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Move Origin",
			LXPI_PROC, crvattrmvloc1_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rb= (int) *((int *) panel_get(crvattr_panel, LXP_VWIDTH));
	rx= rb-strlen("Move Terminus")*charwd;
	if ((crvattr_mvloc2= panelitem_create(crvattr_panel, LXPI_BUTTON,
			LXPI_X, rx,
			LXPI_Y, y,
			LXPI_STRING, "Move Terminus",
			LXPI_PROC, crvattrmvloc2_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2);
	if ((crvattr_mvcnt1= panelitem_create(crvattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Move Control A",
			LXPI_PROC, crvattrmvcnt1_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rx= rb-strlen("Move Control B")*charwd;
	if ((crvattr_mvcnt2= panelitem_create(crvattr_panel, LXPI_BUTTON,
			LXPI_X, rx,
			LXPI_Y, y,
			LXPI_STRING, "Move Control B",
			LXPI_PROC, crvattrmvcnt2_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((crvattr_cont= panelitem_create(crvattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, crvattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rx= rb-strlen("Abort")*charwd;
	if ((crvattr_abort= panelitem_create(crvattr_panel, LXPI_BUTTON,
			LXPI_X, rx,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, crvattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(crvattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(crvattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, crvattr_frame, "ICE Curve Attributes", "ICE CrvAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, crvattr_frame, &xwmh);
	XSetNormalHints(dpy, crvattr_frame, xsh);
	XMoveResizeWindow(dpy, crvattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_mrkattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	mrkattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, mrkattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((mrkattr_panel= panel_create(progname, dpy, mrkattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((mrkattr_name= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_type= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Type:",
			LXPENUM_SELSTRINGS, "Default", "Square", "Triangle", "Circle", "Cross", (char *) NULL,
			LXPENUM_VALUE, MARKER_GLOBALTYPE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_size= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Size:",
			LXPI_PROC, mrkattrsz_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, MARKER_GLOBALSIZE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_mrksize, 10, 0, buf);
	if ((mrkattr_radius= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Radius:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_bndmode= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary:",
			LXPI_PROC, mrkattrbndmode_proc,
			LXPENUM_SELSTRINGS, "Opaque", "Transparent", (char *) NULL,
			LXPENUM_VALUE, MARKER_OPAQUEBNDM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_bndwd= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Width:",
			LXPI_PROC, mrkattrbndwd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, MARKER_GLOBALBNDWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	if ((mrkattr_bwidth= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_bndcolor= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Color:",
			LXPI_PROC, mrkattrbndclr_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, MARKER_GLOBALBND,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rbnd);
	if ((mrkattr_rbnd= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gbnd);
	if ((mrkattr_gbnd= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bbnd);
	if ((mrkattr_bbnd= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_fillmode= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill:",
			LXPI_PROC, mrkattrfillmode_proc,
			LXPENUM_SELSTRINGS, "Transparent", "Opaque", (char *) NULL,
			LXPENUM_VALUE, MARKER_TRANSPARENTFILLM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_fillcolor= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill Color:",
			LXPI_PROC, mrkattrfillclr_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, MARKER_GLOBALFILL,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfill);
	if ((mrkattr_rfill= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfill);
	if ((mrkattr_gfill= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfill);
	if ((mrkattr_bfill= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_hscale= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Horizontal Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((mrkattr_vscale= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Vertical Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_rot= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Rotation:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_clip= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_dtk= panelitem_create(mrkattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, mrkattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((mrkattr_rdtk= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((mrkattr_gdtk= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((mrkattr_bdtk= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((mrkattr_seq= panelitem_create(mrkattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((mrkattr_cont= panelitem_create(mrkattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, mrkattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(mrkattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((mrkattr_abort= panelitem_create(mrkattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, mrkattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(mrkattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(mrkattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, mrkattr_frame, "ICE Marker Attributes", "ICE MrkAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, mrkattr_frame, &xwmh);
	XSetNormalHints(dpy, mrkattr_frame, xsh);
	XMoveResizeWindow(dpy, mrkattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_rectattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	rectattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, rectattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((rectattr_panel= panel_create(progname, dpy, rectattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((rectattr_name= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_bndmode= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary:",
			LXPI_PROC, rectattrbndmode_proc,
			LXPENUM_SELSTRINGS, "Opaque", "Transparent", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_OPAQUEBNDM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_bndwd= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Width:",
			LXPI_PROC, rectattrbndwd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_GLOBALBNDWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	if ((rectattr_bwidth= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_linestyle= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Line Style:",
			LXPI_PROC, rectattrline_proc,
			LXPENUM_SELSTRINGS, "Solid", "Dashed", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_SOLID,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_dashstyle= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Style:",
			LXPI_PROC, rectattrdash_proc,
			LXPENUM_SELSTRINGS, "Simple", "Complex", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_SIMPLEDASH,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_dashlen= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((rectattr_dashgaplen= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Gap Length:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((rectattr_dashpattern= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dash Pattern:",
			LXPTEXT_MAXSTORE, 40,
			LXPTEXT_MAXDISPLAY, 25,
			LXPTEXT_VALUE, "4 4",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((rectattr_dashoffset= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Dash Offset:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_bndcolor= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Color:",
			LXPI_PROC, rectattrbndclr_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_GLOBALBND,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rbnd);
	if ((rectattr_rbnd= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gbnd);
	if ((rectattr_gbnd= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bbnd);
	if ((rectattr_bbnd= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_fillmode= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill:",
			LXPI_PROC, rectattrfillmode_proc,
			LXPENUM_SELSTRINGS, "Transparent", "Opaque", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_TRANSPARENTFILLM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_fillcolor= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill Color:",
			LXPI_PROC, rectattrfillclr_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, RECTANGLE_GLOBALFILL,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfill);
	if ((rectattr_rfill= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfill);
	if ((rectattr_gfill= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfill);
	if ((rectattr_bfill= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_dimmode= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dimensioning Mode:",
			LXPI_PROC, rectattrdimmode_proc,
			LXPENUM_SELSTRINGS, "Cursor", "Text", (char *) NULL,
			LXPENUM_VALUE, RECT_CURSORDIM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_width= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((rectattr_height= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*45),
			LXPI_Y, y,
			LXPI_STRING, "Height:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_rot= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Rotation:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_clip= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_dtk= panelitem_create(rectattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, rectattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((rectattr_rdtk= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((rectattr_gdtk= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((rectattr_bdtk= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((rectattr_seq= panelitem_create(rectattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((rectattr_cont= panelitem_create(rectattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, rectattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(rectattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((rectattr_abort= panelitem_create(rectattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, rectattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(rectattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(rectattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, rectattr_frame, "ICE Rectangle Attributes", "ICE RectAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, rectattr_frame, &xwmh);
	XSetNormalHints(dpy, rectattr_frame, xsh);
	XMoveResizeWindow(dpy, rectattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_polyattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	polyattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, polyattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((polyattr_panel= panel_create(progname, dpy, polyattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((polyattr_name= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_closure= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Type:",
			LXPI_PROC, polyattrcls_proc,
			LXPENUM_SELSTRINGS, "Closed", "Open", (char *) NULL,
			LXPENUM_VALUE, POLYGON_CLOSED,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_bndmode= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary:",
			LXPI_PROC, polyattrbndmode_proc,
			LXPENUM_SELSTRINGS, "Opaque", "Transparent", (char *) NULL,
			LXPENUM_VALUE, POLYGON_OPAQUEBNDM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_bndwd= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Width:",
			LXPI_PROC, polyattrbndwd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, POLYGON_GLOBALBNDWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	if ((polyattr_bwidth= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_bndcolor= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Boundary Color:",
			LXPI_PROC, polyattrbndclr_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, POLYGON_GLOBALBND,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rbnd);
	if ((polyattr_rbnd= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gbnd);
	if ((polyattr_gbnd= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bbnd);
	if ((polyattr_bbnd= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_fillmode= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill:",
			LXPI_PROC, polyattrfillmode_proc,
			LXPENUM_SELSTRINGS, "Transparent", "Opaque", (char *) NULL,
			LXPENUM_VALUE, POLYGON_TRANSPARENTFILLM,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_fillcolor= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Fill Color:",
			LXPI_PROC, polyattrfillclr_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, POLYGON_GLOBALFILL,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfill);
	if ((polyattr_rfill= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfill);
	if ((polyattr_gfill= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfill);
	if ((polyattr_bfill= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_hscale= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Horizontal Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((polyattr_vscale= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Vertical Scale:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "1",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_rot= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Rotation:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_clip= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_dtk= panelitem_create(polyattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, polyattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((polyattr_rdtk= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((polyattr_gdtk= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((polyattr_bdtk= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((polyattr_seq= panelitem_create(polyattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((polyattr_cont= panelitem_create(polyattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, polyattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(polyattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((polyattr_abort= panelitem_create(polyattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, polyattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(polyattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(polyattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, polyattr_frame, "ICE Polygon Attributes", "ICE PolyAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, polyattr_frame, &xwmh);
	XSetNormalHints(dpy, polyattr_frame, xsh);
	XMoveResizeWindow(dpy, polyattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_axisattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y, i, ff, fn, rbx;
	boolean found;
	char buf[80];

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	axisattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, axisattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((axisattr_panel= panel_create(progname, dpy, axisattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((axisattr_name= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_orig= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Origin:",
			LXPTEXT_MAXSTORE, 20,
			LXPTEXT_MAXDISPLAY, 20,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((axisattr_term= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(32*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Terminus:",
			LXPTEXT_MAXSTORE, 20,
			LXPTEXT_MAXDISPLAY, 20,
			LXPTEXT_VALUE, "100",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_linlog= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Type:",
			LXPENUM_SELSTRINGS, "Linear", "Logarithmic", (char *) NULL,
			LXPENUM_VALUE, AXIS_LINEAR,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_div= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Subdivisions:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "10",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_axiswd= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Axis Width:",
			LXPI_PROC, axisattraxiswd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, AXIS_GLOBALWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	if ((axisattr_axiswidth= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_tickloc= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Tick Location:",
			LXPI_PROC, axisattrtickloc_proc,
			LXPENUM_SELSTRINGS, "Standard", "Alternate", "None", (char *) NULL,
			LXPENUM_VALUE, AXIS_STDLOC,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_ptickht= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Primary Tick Height:",
			LXPTEXT_MAXSTORE, 6,
			LXPTEXT_MAXDISPLAY, 6,
			LXPTEXT_VALUE, "7",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((axisattr_stickht= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(32*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Secondary Tick Height:",
			LXPTEXT_MAXSTORE, 6,
			LXPTEXT_MAXDISPLAY, 6,
			LXPTEXT_VALUE, "5",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((axisattr_ttickht= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(64*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Tertiary Tick Height:",
			LXPTEXT_MAXSTORE, 6,
			LXPTEXT_MAXDISPLAY, 6,
			LXPTEXT_VALUE, "3",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_tickwd= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Tick Width:",
			LXPI_PROC, axisattrtickwd_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, AXIS_GLOBALWIDTH,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	if ((axisattr_tickwidth= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Width:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_lin= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Axis/Tick Color:",
			LXPI_PROC, axisattrlin_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, AXIS_GLOBALLINE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfg);
	if ((axisattr_rlin= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	if ((axisattr_glin= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	if ((axisattr_blin= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_fontloc= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Location:",
			LXPI_PROC, axisattrfontloc_proc,
			LXPENUM_SELSTRINGS, "Standard", "Alternate", "None", (char *) NULL,
			LXPENUM_VALUE, AXIS_STDLOC,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_font= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Font:",
			LXPI_PROC, axisattrfont_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, AXIS_GLOBALFONT,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	found= FALSE;
	for (ff= 0; ff < npsfontfamilies; ff++) {
		for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
			if (!strcmp(gdf_fontname, psfontfamilies[ff].psff_fonts[fn])) {
				found= TRUE;
				break;
			}
		}
		if (found == TRUE)
			break;
	}
	if (!found)
		ff= fn= 0;

	y+= (int) (fontht*1.5);
	if ((axisattr_fontfamily= panelitem_create(axisattr_panel, LXPI_CYCLE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Font Family:",
			LXPI_PROC, axisattrfontfamily_proc,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	for (i= 0; i < npsfontfamilies; i++) {
		int j;

		if (panelitem_addsel(axisattr_panel, axisattr_fontfamily,
				LXPENUM_SELSTRING, psfontfamilies[i].psff_name,
				LXPI_NULL) != LX_SUCCESS)
			ice_err("Internal toolkit error.", FATAL);
		if ((psfontfamilies[i].psff_axispi= panelitem_create(axisattr_panel, LXPI_CYCLE,
				LXPI_X, x,
				LXPI_Y, y,
				LXPI_STRING, "Label Font Name:",
				LXPI_STATE, LXPI_INACTIVE,
				LXPI_NULL)) == (Panel_item *) NULL)
			ice_err("Internal toolkit error.", FATAL);

		for (j= 0; j < psfontfamilies[i].psff_count; j++) {
			if (panelitem_addsel(axisattr_panel, psfontfamilies[i].psff_axispi,
					LXPENUM_SELSTRING, psfontfamilies[i].psff_fonts[j],
					LXPI_NULL) != LX_SUCCESS)
				ice_err("Internal toolkit error.", FATAL);
		}
	}

	panelitem_set(axisattr_panel, axisattr_fontfamily, LXPENUM_VALUE, ff, LXPI_NULL);
	axis_psff= psfontfamilies[ff].psff_axispi;
	panelitem_set(axisattr_panel, axis_psff, LXPENUM_VALUE, fn, LXPI_NULL);

	y+= (int) (fontht*1.5);
	if ((axisattr_fontsz= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Font Size:",
			LXPI_PROC, axisattrfontsz_proc,
			LXPENUM_SELSTRINGS, "Default", "Other", (char *) NULL,
			LXPENUM_VALUE, AXIS_GLOBALFONTSZ,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_fontsize, 10, 0, buf);
	if ((axisattr_fontsize= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Size:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_fontorient= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Orientation:",
			LXPENUM_SELSTRINGS, "Parallel", "Perpendicular", "Parallel Reverse", "Perpendicular Reverse", (char *) NULL,
			LXPENUM_VALUE, AXIS_FONTORIENT0,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) gconvert((double) gdf_fontsize, 10, 0, buf);
	if ((axisattr_fontoff= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Offset:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, buf,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_lab= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Label Color:",
			LXPI_PROC, axisattrlab_proc,
			LXPENUM_SELSTRINGS, "Default", "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, AXIS_GLOBALLABEL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rfg);
	if ((axisattr_rlab= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	if ((axisattr_glab= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	if ((axisattr_blab= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_clip= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Clipping Path:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_dtk= panelitem_create(axisattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Dump Transparency Key:",
			LXPI_PROC, axisattrdtk_proc,
			LXPENUM_SELSTRINGS, "Default", "White", "Black", "Other", (char *) NULL,
			LXPENUM_VALUE, GROBJ_GLOBALDTK,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	if ((axisattr_rdtk= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	if ((axisattr_gdtk= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	if ((axisattr_bdtk= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, buf,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((axisattr_seq= panelitem_create(axisattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Sequence:",
			LXPTEXT_MAXSTORE, 10,
			LXPTEXT_MAXDISPLAY, 10,
			LXPTEXT_VALUE, "0",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2);
	if ((axisattr_mvloc1= panelitem_create(axisattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Move Origin",
			LXPI_PROC, axisattrmvloc1_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rbx= (int) *((int *) panel_get(axisattr_panel, LXP_VWIDTH));
	rbx-= strlen("Move Terminus")*charwd;
	if (rbx < (strlen("Move Origin")+9)*charwd)
		rbx= (strlen("Move Origin")+9)*charwd;
	if ((axisattr_mvloc2= panelitem_create(axisattr_panel, LXPI_BUTTON,
			LXPI_X, rbx,
			LXPI_Y, y,
			LXPI_STRING, "Move Terminus",
			LXPI_PROC, axisattrmvloc2_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((axisattr_cont= panelitem_create(axisattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, axisattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	rbx+= (strlen("Move Terminus")-strlen("Abort"))*charwd;
	if ((axisattr_abort= panelitem_create(axisattr_panel, LXPI_BUTTON,
			LXPI_X, rbx,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, axisattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(axisattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(axisattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, axisattr_frame, "ICE Axis Attributes", "ICE AxisAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, axisattr_frame, &xwmh);
	XSetNormalHints(dpy, axisattr_frame, xsh);
	XMoveResizeWindow(dpy, axisattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_cmpattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	cmpattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, cmpattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((cmpattr_panel= panel_create(progname, dpy, cmpattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((cmpattr_name= panelitem_create(cmpattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((cmpattr_scale= panelitem_create(cmpattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Scale:",
			LXPTEXT_MAXSTORE, 20,
			LXPTEXT_MAXDISPLAY, 20,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((cmpattr_scaleattr= panelitem_create(cmpattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Internal Scale:",
			LXPENUM_SELSTRINGS, "No", "Yes", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((cmpattr_rot= panelitem_create(cmpattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Rotation:",
			LXPTEXT_MAXSTORE, 20,
			LXPTEXT_MAXDISPLAY, 20,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((cmpattr_cont= panelitem_create(cmpattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, cmpattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(cmpattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((cmpattr_abort= panelitem_create(cmpattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, cmpattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(cmpattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(cmpattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, cmpattr_frame, "ICE Composite Attributes", "ICE CmpAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, cmpattr_frame, &xwmh);
	XSetNormalHints(dpy, cmpattr_frame, xsh);
	XMoveResizeWindow(dpy, cmpattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_pthattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	pthattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, pthattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((pthattr_panel= panel_create(progname, dpy, pthattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((pthattr_name= panelitem_create(pthattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Name:",
			LXPTEXT_MAXSTORE, 80,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pthattr_src= panelitem_create(pthattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Source:",
			LXPI_PROC, pthattrsrc_proc,
			LXPENUM_SELSTRINGS, "Interactive", "File", (char *) NULL,
			LXPENUM_VALUE, PTH_USERINPUT,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pthattr_filenm= panelitem_create(pthattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Filename:",
			LXPTEXT_MAXSTORE, MAX_FILENMLEN,
			LXPTEXT_MAXDISPLAY, 40,
			LXPTEXT_VALUE, "",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pthattr_closure= panelitem_create(pthattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Type:",
			LXPENUM_SELSTRINGS, "Open", "Closed", (char *) NULL,
			LXPENUM_VALUE, PATH_OPEN,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pthattr_vis= panelitem_create(pthattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Display:",
			LXPI_PROC, pthattrvis_proc,
			LXPENUM_SELSTRINGS, "No", "Yes", (char *) NULL,
			LXPENUM_VALUE, PATH_INVISIBLE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pthattr_fg= panelitem_create(pthattr_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Color:",
			LXPI_PROC, pthattrfg_proc,
			LXPENUM_SELSTRINGS, "Black", "White", "Other", (char *) NULL,
			LXPENUM_VALUE, PATH_BLACKFG,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((pthattr_rfg= panelitem_create(pthattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Red:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pthattr_gfg= panelitem_create(pthattr_panel, LXPI_TEXT,
			LXPI_X, x+(15*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Green:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((pthattr_bfg= panelitem_create(pthattr_panel, LXPI_TEXT,
			LXPI_X, x+(30*charwd),
			LXPI_Y, y,
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_STRING, "Blue:",
			LXPTEXT_MAXSTORE, 3,
			LXPTEXT_MAXDISPLAY, 3,
			LXPTEXT_VALUE, "0",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((pthattr_cont= panelitem_create(pthattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, pthattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(pthattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((pthattr_abort= panelitem_create(pthattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, pthattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(pthattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(pthattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, pthattr_frame, "ICE Path Attributes", "ICE PathAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, pthattr_frame, &xwmh);
	XSetNormalHints(dpy, pthattr_frame, xsh);
	XMoveResizeWindow(dpy, pthattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_locattrpanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	locattr_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, locattr_frame, wmpr_atoms, WMPR_ATOMS);

	if ((locattr_panel= panel_create(progname, dpy, locattr_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((locattr_lab= panelitem_create(locattr_panel, LXPI_LABEL,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Vertex XX",
			LXPI_STATE, LXPI_INACTIVE,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((locattr_x= panelitem_create(locattr_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "X Coordinate:",
			LXPTEXT_MAXSTORE, 15,
			LXPTEXT_MAXDISPLAY, 15,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	if ((locattr_y= panelitem_create(locattr_panel, LXPI_TEXT,
			LXPI_X, x+(charwd*35),
			LXPI_Y, y,
			LXPI_STRING, "Y Coordinate:",
			LXPTEXT_MAXSTORE, 15,
			LXPTEXT_MAXDISPLAY, 15,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((locattr_cont= panelitem_create(locattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, locattrcont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(locattr_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((locattr_abort= panelitem_create(locattr_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, locattrabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(locattr_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(locattr_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, locattr_frame, "ICE Location", "ICE Loc", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, locattr_frame, &xwmh);
	XSetNormalHints(dpy, locattr_frame, xsh);
	XMoveResizeWindow(dpy, locattr_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_dmpipspanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	dmpips_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, dmpips_frame, wmpr_atoms, WMPR_ATOMS);

	if ((dmpips_panel= panel_create(progname, dpy, dmpips_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((dmpips_filenm= panelitem_create(dmpips_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Filename:",
			LXPTEXT_MAXSTORE, MAX_FILENMLEN,
			LXPTEXT_MAXDISPLAY, 40,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((dmpips_mode= panelitem_create(dmpips_panel, LXPI_TOGGLE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Contents:",
			LXPENUM_SELSTRINGS, "ICE", "PostScript", (char *) NULL,
			LXPI_PROC, dmpipsmode_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

#if 0
	y+= (int) (fontht*1.5);
	if ((dmpips_ras= panelitem_create(dmpips_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Include Rasters:",
			LXPENUM_SELSTRINGS, "No", "Yes", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
#endif

	y+= (int) (fontht*2.5);
	if ((dmpips_cont= panelitem_create(dmpips_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, dmpipscont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(dmpips_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((dmpips_abort= panelitem_create(dmpips_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, dmpipsabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(dmpips_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(dmpips_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, dmpips_frame, "ICE Dump Attributes", "ICE DumpAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, dmpips_frame, &xwmh);
	XSetNormalHints(dpy, dmpips_frame, xsh);
	XMoveResizeWindow(dpy, dmpips_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

void
init_insicepanel()
{
	XSizeHints *xsh;
	XSetWindowAttributes xswa;
	int x, y;

	if ((xsh= XAllocSizeHints()) == (XSizeHints *) NULL) {
		(void) fprintf(stderr, "%s: memory allocation error\n", progname);
		exit(-1);
	}
	xsh->width= ATTR_FRAMEWIDTH;
	xsh->height= ATTR_FRAMEHEIGHT;
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;

	xswa.background_pixel= bg_pixel;
	xswa.border_pixel= fg_pixel;
	xswa.colormap= cmap;

	insice_frame= XCreateWindow(dpy, root_win, xsh->x, xsh->y, xsh->width, xsh->height, border_wd, pg_pixdepth, InputOutput, visual, CWBackPixel | CWBorderPixel | CWColormap, &xswa);
	XSetWMProtocols(dpy, insice_frame, wmpr_atoms, WMPR_ATOMS);

	if ((insice_panel= panel_create(progname, dpy, insice_frame,
			LXP_FONT, fontnm,
			LXP_FOREGROUND, fg_pixel,
			LXP_BACKGROUND, bg_pixel,
			LXP_NULL)) == (Panel *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= charwd*2;
	y= fontht*2;

	if ((insice_filenm= panelitem_create(insice_panel, LXPI_TEXT,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Filename:",
			LXPTEXT_MAXSTORE, MAX_FILENMLEN,
			LXPTEXT_MAXDISPLAY, 40,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((insice_pg= panelitem_create(insice_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Page Attributes:",
			LXPENUM_SELSTRINGS, "Current", "New", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((insice_gdf= panelitem_create(insice_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Default Attributes:",
			LXPENUM_SELSTRINGS, "Current", "New", (char *) NULL,
			LXPI_PROC, insicegdf_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*1.5);
	if ((insice_currobj= panelitem_create(insice_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Existing Object Attributes:",
			LXPENUM_SELSTRINGS, "Preserve", "Use New Defaults", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);
	if ((insice_newobj= panelitem_create(insice_panel, LXPI_CHOICE,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "New Object Attributes:",
			LXPENUM_SELSTRINGS, "Preserve", "Use Current Defaults", (char *) NULL,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	y+= (int) (fontht*2.5);
	if ((insice_cont= panelitem_create(insice_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Continue",
			LXPI_PROC, insicecont_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	x= (int) *((int *) panel_get(insice_panel, LXP_VWIDTH));
	x-= strlen("Abort")*charwd;
	if (x < (strlen("Continue")+9)*charwd)
		x= (strlen("Continue")+9)*charwd;
	if ((insice_abort= panelitem_create(insice_panel, LXPI_BUTTON,
			LXPI_X, x,
			LXPI_Y, y,
			LXPI_STRING, "Abort",
			LXPI_PROC, insiceabort_proc,
			LXPI_NULL)) == (Panel_item *) NULL)
		ice_err("Internal toolkit error.", FATAL);

	xsh->flags= (PPosition | PSize);
	xsh->width= (int) *((int *) panel_get(insice_panel, LXP_VWIDTH));
	xsh->height= (int) *((int *) panel_get(insice_panel, LXP_VHEIGHT));
	xsh->x= (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh->width) / 2;
	xsh->y= (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh->height) / 2;
	XSetStandardProperties(dpy, insice_frame, "ICE Insert Attributes", "ICE InsAttr", None, (char **) NULL, 0, xsh);
	XSetWMHints(dpy, insice_frame, &xwmh);
	XSetNormalHints(dpy, insice_frame, xsh);
	XMoveResizeWindow(dpy, insice_frame, xsh->x, xsh->y, xsh->width, xsh->height);
	XFree((char *) xsh);

	return;
}

char *
xtonewsdpystr(char *xdpy)
{
	char *ndpy;
	char host[MAXHOSTNAMELEN+1];
	int i, dpyno;
	char buf[MAXHOSTNAMELEN+30];
	char errmsg[MAX_ERRMSGLEN+1];
	struct hostent *hp;
	long addr;

	bzero(host, MAXHOSTNAMELEN+1);
	for (i= 0; (xdpy[i] != ':') && (xdpy[i] != '\0'); i++)
		host[i]= xdpy[i];
	if (xdpy[i] != ':') {
		sprintf(errmsg, "Invalid X11 display string format '%s'.", xdpy);
		ice_err(errmsg, FATAL);
	}
	i++;
	if (sscanf(xdpy+i, "%d", &dpyno) != 1) {
		sprintf(errmsg, "Invalid X11 display string format '%s'.", xdpy);
		ice_err(errmsg, FATAL);
	}
	if (dpyno != 0)
		ice_err("Must use X11 display 0.", FATAL);

	if ((hp= gethostbyname(host)) == (struct hostent *) NULL) {
		sprintf(errmsg, "Unknown host 's'.", host);
		ice_err(errmsg, FATAL);
	}
	addr= ntohl(*((unsigned long *) hp->h_addr));
	sprintf(buf, "%u.2000;%s", (unsigned long) addr, host);
	if ((ndpy= new char[strlen(buf)+1]) == (char *) NULL)
		ice_err("Memory allocation error.", FATAL);
	(void) strcpy(ndpy, buf);
	return ndpy;
}
