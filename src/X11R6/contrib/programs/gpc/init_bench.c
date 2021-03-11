/* $XConsortium: init_bench.c,v 5.7 94/04/17 20:44:37 hersh Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/
/***********************************************************
Copyright(c) 1989,1990, 1991 by Sun Microsystems, Inc.

						All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that Sun Microsystems
not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	SimGraphics Engineering Corportation
|
| File          :	init_bench.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	
| Status        :	Version 1.0
|
| Revisions     :	
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|	void init_bench( *Bench_setup, *Wk_info)
|		:	Initialize the graphics subsystem for testing
|	void init_border_str(int, int)
|		:	Defines the border structure
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "biftypes.h"
#include "globals.h"
#include "ph_map.h"
#include "bifparse.h"
#include "stopwatch.h"
#ifdef USING_PHIGS
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/Xatom.h>
#include <ctype.h>
#include "brfexption.h"
#include "brftypes.h"
#include "brfexmacro.h"
#endif /* USING_PHIGS */

/* ---------------------------------------------------------------------*\
|Local #define                                                           |
\*--------------------------------------------------------------------- */
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

/* Constants to add specific system info to Report Header at run time */
#define NCOUNT 40
#define NSYSLN 3
#define NHRDLN 4
#define NOSVLN 7
#define NWNVLN 8
#define NWNSLN 9
#define NTMALN 10

extern char *BRF_appendToColon();

/*--------------------------------------------------------------------*\
|	Local MACROS
\*--------------------------------------------------------------------*/

#ifdef USING_PHIGS
#define	CONNECT_TO_DISPLAY(name)\
	if(!(dpy=XOpenDisplay(name))){\
		fprintf(stderr,"Cannot open display\n");\
		exit(-1);\
	}

#define CHECK_PHIGS_OPEN \
        { Psys_st sys_st; \
	        pinq_sys_st(&sys_st); \
		if (sys_st != PSYS_ST_PHOP) { \
			fprintf(stderr,"PHIGS not open, Aborting.\n"); \
			exit(-1); \
		} \
	}

#define CHECK_WS_OPEN \
        { Pws_st ws_st; \
	        pinq_ws_st(&ws_st); \
		if (ws_st != PWS_ST_WSOP) { \
			fprintf(stderr,"PHIGS WORKSTATION not open, Aborting.\n"); \
			pclose_phigs(); \
			exit(-1); \
		} \
	}

#define MXPIX (Pfloat)(0xffff)
#define CALC_COLOR_PIXEL(cm,r,g,b) \
        ((cm).base_pixel + \
         ((unsigned long)(0.5 + (r) * (cm).red_max) * (cm).red_mult) + \
         ((unsigned long)(0.5 + (g) * (cm).green_max) * (cm).green_mult) + \
         ((unsigned long)(0.5 + (b) * (cm).blue_max) * (cm).blue_mult))
#endif /* USING_PHIGS */

/*--------------------------------------------------------------------*\
|	Local global variables
\*--------------------------------------------------------------------*/

extern char *brf_sysinfo[];

#ifdef USING_PHIGS
static Pfloat ur,ug,ub;
static Display *dpy;
static Window win;
static XSetWindowAttributes xswa;
static Visual visual;
static XStandardColormap ci;
static XrmValue val;
static XSizeHints size_hints;
static XrmDatabase dis_db;
static char tmpln[80];
static char *str_type;
#endif /* USING_PHIGS */

/* ---------------------------------------------------------------------*\
| Contents:                                                              |
\*--------------------------------------------------------------------- */
#ifdef FULL_FUNCT_PROTO_TYPE

void init_bench( Bench_setup*,Wk_invo*);

#else /* ifdef FULL_FUNCT_PROTO_TYPE */

void init_bench();

#endif /* ifdef FULL_FUNCT_PROTO_TYPE */


/* ---------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE                                                   |
\*--------------------------------------------------------------------- */

#ifdef USING_PHIGS
#define LIST_SIZE 30
typedef struct _err_def {
    int type;
    char *type_string;
} err_def;

#define NUM_MARKER_TYPES 5
static err_def marker_types[NUM_MARKER_TYPES] = {
    { 1, "MARKER_TYPE MK_POINT not supported.\n" },
    { 2, "MARKER_TYPE MK_PLUS not supported.\n" },
    { 3, "MARKER_TYPE MK_ASTER not supported.\n" },
    { 4, "MARKER_TYPE MK_CIRCLE not supported.\n" },
    { 5, "MARKER_TYPE MK_X not supported.\n" }
};

#define NUM_LINE_TYPES 4
static err_def line_types[NUM_LINE_TYPES] = {
    { 1, "LINE_TYPE LINE_SOLID not supported.\n" },
    { 2, "LINE_TYPE LINE_DASH not supported.\n" },
    { 3, "LINE_TYPE LINE_DOTTED not supported.\n" },
    { 4, "LINE_TYPE LINE_DASHDOT not supported.\n" }
};

#define NUM_LINE_SHADES 2
static err_def line_shades[NUM_LINE_SHADES] = {
    { 1, "LINE_SHADING LINE_SHADE_FLAT not supported.\n" },
    { 2, "LINE_SHADING LINE_SHADE_COLOR not supported.\n" }
};

#define NUM_INTERIOR_TYPES 4
static err_def interior_types[NUM_INTERIOR_TYPES] = {
    { 0, "INTERIOR_STYLE HOLLOW not supported.\n" },
    { 1, "INTERIOR_STYLE SOLID not supported.\n" },
    { 2, "INTERIOR_STYLE PATTERN not supported.\n" },
    { 4, "INTERIOR_STYLE EMPTY not supported.\n" }
};

#define NUM_INTERIOR_SHADES 4
static err_def interior_shade_types[NUM_INTERIOR_SHADES] = {
    { 1, "INTERIOR_SHADING SHADE_FLAT not supported.\n" },
    { 2, "INTERIOR_SHADING SHADE_GOURAUD not supported.\n" },
    { 3, "INTERIOR_SHADING SHADE_DOT not supported.\n" },
    { 4, "INTERIOR_SHADING SHADE_PHONG not supported.\n" }
};

#define NUM_INTERIOR_REFLECS 4
static err_def interior_reflecs[NUM_INTERIOR_REFLECS] = {
    { 1, "INTERIOR_LIGHTING LIGHT_NONE not supported.\n" },
    { 2, "INTERIOR_LIGHTING LIGHT_AMBIENT not supported.\n" },
    { 3, "INTERIOR_LIGHTING LIGHT_DIFFUSE not supported.\n" },
    { 4, "INTERIOR_LIGHTING LIGHT_SPECULAR not supported.\n" }
};

#define NUM_EDGE_TYPES 4
static err_def edge_types[NUM_EDGE_TYPES] = {
    { 1, "EDGE_TYPE EDGE_SOLID not supported.\n" },
    { 2, "EDGE_TYPE EDGE_DASH not supported.\n" },
    { 3, "EDGE_TYPE EDGE_DOTTED not supported.\n" },
    { 4, "EDGE_TYPE EDGE_DASHDOT not supported.\n" }
};

#define NUM_HLHSR_MODES 5
static char *hlhsr_modes[NUM_HLHSR_MODES] = {
    "NONE", "ZBUFF", "PAINTERS", "SCANLINE", "LINE"
};

Pconnid_x_drawable conn;
extern int BRF_do_optiontest();
extern int BRF_realrange();
extern int BRF_intrange();
extern int BRF_truecolor();
extern int BRF_pseudocolor();
extern BRF_exception brf_exception[];
#endif /* USING_PHIGS */

static int upcase(source,dest)
char *source;
char *dest;
{
    for (;*source != 0; source++,dest++) {
	*dest = islower(*source) ? toupper(*source) : *source;
    }
    *dest = 0;
}

/*----------------------------------------------------------------------*\
| Procedure     :	void init_bench( *Bench_setup, *Wk_info)
|--------------------------------------------------------------------------|
| Description   :	Initialize the graphics subsystem for testing
|--------------------------------------------------------------------------|
| Return        :	
\*----------------------------------------------------------------------*/
void init_bench( benchsetup,wkinfo)
Bench_setup *benchsetup;
Wk_info *wkinfo;

{
/*----------------------------------------------------------------------*\
| 	Open the phigs and the workstation
\*----------------------------------------------------------------------*/
#ifdef USING_PHIGS
	static char err_file[] = "BIFPEX.err";
	static char *Single_Buffer[] = {
	    (char *)PHIGS_X_BUF_MODE, (char *)PHIGS_BUF_SINGLE,
	    (char *)0
	    };
	static char *Double_Buffer[] = {
	    (char *)PHIGS_X_BUF_MODE, (char *)PHIGS_BUF_DOUBLE,
	    (char *)0
	    };
	Pconnid conn_id;
	Pint gpc_ws;
	Pfloat xdist, ydist;
	Pint err_ind;
	Pupd_st gpc_upd;
	Plimit3 gpc_win;
	Plimit3 gpc_dum,gpc_cur_vp;
	char user_display[256];
	int depth;
	char *resources;

	user_display[0] = '\0'; /* Set default display */

	/* Check for display on the command line */
	(void *)strcpy(tmpln, Prog_name);
	(void *)strcat(tmpln, ".display");
	if (XrmGetResource(gpc_res_db, tmpln, (char *)NULL, &str_type, &val))
	    (void *)strcpy(user_display, val.addr);

	CONNECT_TO_DISPLAY(user_display);

	/* Now get the resources from the server */
	resources = XResourceManagerString(dpy);
	if (!resources)
	    resources = "";
	dis_db = XrmGetStringDatabase(resources);
	XrmMergeDatabases(gpc_res_db, &dis_db);

	/* Fill out vendor specific entries in the report file */
	SetVendorEntries();

	/* Get the appropriate visual and colormap */
	if (!CreateColormap(dpy, &visual, &depth, &ci)) {
	    fprintf(stderr,"Could not find appropriate color map\n");
	    exit(-1);
	}

	CreateTheWindow(wkinfo, depth);

	gpc_ws = phigs_ws_type_x_drawable;
	conn.display = dpy;
	conn.drawable_id = win;

	/* Open PHIGS */
	popen_phigs((char *)err_file,PDEF_MEM_SIZE);

	CHECK_PHIGS_OPEN;

	/* Set up for Single/Double buffer, PEX supports PSEUDO-COLOR and
	   TRUE_COLOR so no explicit setting is required */

	/* First check if the user wants a different buffer mode */
	(void *)strcpy(tmpln, Prog_name);
	(void *)strcat(tmpln, ".bufMode");
	if (XrmGetResource(dis_db, tmpln, (char *)NULL, &str_type, &val)) {
	    upcase(val.addr,tmpln);
	    if (!strcmp(tmpln,"SINGLE")) {
		if (wkinfo->buffer_mode != SINGLE_BUFFER)
		    PLB_EXCEPTION(BIF_EX_BUFFMODE);
		wkinfo->buffer_mode = SINGLE_BUFFER;
	    }
	    else if (!strcmp(tmpln,"DOUBLE")) {
		if (wkinfo->buffer_mode != DOUBLE_BUFFER)
		    PLB_EXCEPTION(BIF_EX_BUFFMODE);
		wkinfo->buffer_mode = DOUBLE_BUFFER;
	    }

	  switch(wkinfo->buffer_mode)
	  {
	    case DOUBLE_BUFFER:
	      gpc_ws = phigs_ws_type_create(gpc_ws, (char *)PHIGS_X_BUF_MODE,
					  (char *)PHIGS_BUF_DOUBLE, (char *)0);
	      break;
	    case SINGLE_BUFFER:
	      gpc_ws = phigs_ws_type_create(gpc_ws, (char *)PHIGS_X_BUF_MODE, 
					  (char *)PHIGS_BUF_SINGLE, (char *)0);
	      break;
	  }
	}

	popen_ws((Pint)benchsetup->workid,(Pconnid)&conn,gpc_ws);

	CHECK_WS_OPEN;

	pset_disp_upd_st((Pint)benchsetup->workid,PDEFER_WAIT,PMODE_NIVE);

	/* Now get the connected Workstation type */
	pinq_ws_conn_type((Pint)benchsetup->workid, 0, &err_ind,
			  &conn_id, &gpc_ws);

	/* Get the Viewport Size from the PHIGS API */
	pinq_ws_tran3((Pint)benchsetup->workid,&err_ind,&gpc_upd,
		      &gpc_dum,&gpc_dum,&gpc_dum,&gpc_cur_vp);

	if (!err_ind) {
	    xdist = gpc_cur_vp.x_max - gpc_cur_vp.x_min;
	    ydist = gpc_cur_vp.y_max - gpc_cur_vp.y_min;
	}
	else {
	    xdist = wkinfo->x;
	    ydist = wkinfo->y;
	}
	/* Fill out the size field in the report file */
	sprintf(tmpln,"%ld x %ld",(int)xdist,(int)ydist);
	BRF_appendToColon(brf_sysinfo[NWNSLN],tmpln);

	/* Check for window size being the same as the test file requested */
	if ((xdist != wkinfo->x) || (ydist != wkinfo->y)) 
	    PLB_EXCEPTION(BIF_EX_WINSIZE);

	/* Set the aspect ratio and workstation window transform */
	wkinfo->aspect_ratio = xdist/ydist;
	gpc_win.x_min = gpc_win.y_min = gpc_win.z_min = 0.0;
	gpc_win.x_max = gpc_win.y_max = gpc_win.z_max = 1.0;
	if (wkinfo->aspect_ratio != 1.0) {
	    if (wkinfo->aspect_ratio > 1.0)
		gpc_win.y_max /= wkinfo->aspect_ratio;
	    else
		gpc_win.x_max *= wkinfo->aspect_ratio;
	}

	pset_ws_win3((Pint)benchsetup->workid, &gpc_win);

#endif /* USING_PHIGS */

/*----------------------------------------------------------------------*\
| 	Initialize the Bif traverser to execute mode.
\*----------------------------------------------------------------------*/
	do_endstructure(traverser_state,NULL);

/*----------------------------------------------------------------------*\
| 	Get workstation information: color table depth, screen aspect ratio
\*----------------------------------------------------------------------*/
#ifdef USING_PHIGS
	GetWorkstationInfo(gpc_ws, (Pint)benchsetup->workid, wkinfo);
#else
	wkinfo->cmap_size = 256;
	wkinfo->aspect_ratio = 1.0;
#endif /* USING_PHIGS */

/*----------------------------------------------------------------------*\
| 	Set up the base_state structure.  This structure contains all the 
|	attributes which are allowed outside of bif structures but not 
|	allowed outside of phigs structures (which is ALL of them)
\*----------------------------------------------------------------------*/
	init_base_state(benchsetup->base_state_stid,
			benchsetup->base_state_vid);


/*----------------------------------------------------------------------*\
| 	Set up the border structure.  The border is always displayed
\*----------------------------------------------------------------------*/

	/*
	init_border_struct(benchsetup->border_stid,
			   benchsetup->border_vid);
        */
	/*
	 * We do not have a border structure, the window title supports
	 * the name 
	 */
	benchsetup->border_stid = 0; /* Don't display border structure */

	/* Set the phigs open state to open */
	wkinfo->phigs_open = 1;

/*----------------------------------------------------------------------*\
|       Set Color Maps to Default Values.
\*----------------------------------------------------------------------*/


#ifdef USING_PHIGS
	SetDefaultColors(benchsetup->workid);
#endif
	

} /* End procedure init_bench */

#ifdef USING_PHIGS
#define REDS_256 5
#define GREENS_256 9
#define BLUES_256 5

extern XStandardColormap *XmuStandardColormap();
extern Screen *_XScreenOfWindow ();
/*
 * 				    WARNING
 * 
 * This is a ICCCM routine.  It will reference the new fields
 * in the XStandardColormap structure.
 */

Status ICCM_XGetStandardColormap (display, w, cmap, property)
    Display *display;
    Window w;
    XStandardColormap *cmap;
    Atom property;		/* XA_RGB_BEST_MAP, etc. */
{
    Status stat;			/* return value */
    XStandardColormap *stdcmaps;	/* will get malloced value */
    int nstdcmaps;			/* count of above */

    stat = XGetRGBColormaps (display, w, &stdcmaps, &nstdcmaps, property);
    if (stat) {
	XStandardColormap *use;

	if (nstdcmaps > 1) {
	    VisualID vid;
	    Screen *sp = _XScreenOfWindow (display, w);
	    int i;

	    if (!sp) {
		if (stdcmaps) XFree ((char *) stdcmaps);
		return FALSE;
	    }
	    vid = sp->root_visual->visualid;

	    for (i = 0; i < nstdcmaps; i++) {
		if (stdcmaps[i].visualid == vid) break;
	    }

	    if (i == nstdcmaps) {	/* not found */
		XFree ((char *) stdcmaps);
		return FALSE;
	    }
	    use = &stdcmaps[i];
	} else {
	    use = stdcmaps;
	}
	    
	cmap->colormap	 = use->colormap;
	cmap->red_max	 = use->red_max;
	cmap->red_mult	 = use->red_mult;
	cmap->green_max	 = use->green_max;
	cmap->green_mult = use->green_mult;
	cmap->blue_max	 = use->blue_max;
	cmap->blue_mult	 = use->blue_mult;
	cmap->base_pixel = use->base_pixel;
	cmap->visualid   = use->visualid;
	cmap->killid     = use->killid;

	XFree ((char *) stdcmaps);	/* don't need alloced memory */
    }
    return stat;
}

CreateColormap(display, vs, depth, cm)
Display *display;
Visual *vs;
int *depth;
XStandardColormap *cm;
{
    XVisualInfo		*available_visuals, *the_best, template;
    XStandardColormap	 std, *cmap;
    Visual		*root_visual;
    int			 nvisuals, i, found;
    Atom		 property;
    Display		*ndisplay = NULL;
    
    template.screen = DefaultScreen(display); 
    available_visuals = XGetVisualInfo(display, VisualScreenMask,
				       &template, &nvisuals);

    /* 
     * Get a standard colormap, the visual, depth and property
     * Precedence is defined as follows:
     *    Use existing RGB_DEFAULT_MAP or RGB_BEST_MAP
     *    Use root-visual and create RGB_BEST_MAP
     *    Fail
     *
     */

    if ((ICCM_XGetStandardColormap(display,RootWindow(display,
						      DefaultScreen(display)),
				  &std,XA_RGB_BEST_MAP) && std.colormap) ||
	(ICCM_XGetStandardColormap(display,RootWindow(display,
						      DefaultScreen(display)),
				  &std,XA_RGB_DEFAULT_MAP) && std.colormap)) {
	/* The standard colormap visual might not be the Default */
	*cm = std;
	for (i = 0; i < nvisuals; i++) {
	    if (available_visuals[i].visualid == std.visualid) {
		the_best = &available_visuals[i];
		break;
	    }
	}
    }
    else {
	found = FALSE;
	/* Try to use the root-visual */
	root_visual = DefaultVisual(display, template.screen);
	for (i = 0; i < nvisuals; i++) {
	    if (available_visuals[i].visualid == root_visual->visualid) {
		the_best = &available_visuals[i];
		break;
	    }
	}
	switch (the_best->visual->class) {
	  case StaticGray:
	  case GrayScale:
	    break; /* Can't use */
	  case DirectColor:
	  case PseudoColor:
	    /* Try the default map */
	    ndisplay = XOpenDisplay(DisplayString(display));
	    if (!ndisplay)
		break;
	    property = XA_RGB_DEFAULT_MAP;
	    cmap = XmuStandardColormap(ndisplay,DefaultScreen(display),
				       the_best->visual->visualid,
				       the_best->depth,property,
				       DefaultColormap(display,
						       DefaultScreen(display)),
				       REDS_256-1,GREENS_256-1,BLUES_256-1);
	    if (!cmap) {
		/* That didn't work, try any map */
		property = XA_RGB_BEST_MAP;
		cmap = XmuStandardColormap(ndisplay,DefaultScreen(display),
					   the_best->visual->visualid,
					   the_best->depth,property,
					   (Colormap)None, REDS_256-1,
					   GREENS_256-1,BLUES_256-1);
		if (!cmap) 
		    break;
	    }
	    *cm = *cmap;
	    found = TRUE;
	    break;
	  case StaticColor:
	  case TrueColor:
	    /* Create an RGB_DEFAULT_MAP if values are correct */
	    if(the_best->visual->red_mask && the_best->visual->green_mask
	       && the_best->visual->blue_mask) {
		std.colormap = DefaultColormap(display,
					       DefaultScreen(display));
		std.red_max = the_best->visual->red_mask;
		std.red_mult = 1;
		while(!(std.red_max & 0x01)) {
		    std.red_max >>= 1;
		    std.red_mult <<= 1;
		}
		std.green_max = the_best->visual->green_mask;
		std.green_mult = 1;
		while(!(std.green_max & 0x01)) {
		    std.green_max >>= 1;
		    std.green_mult <<= 1;
		}
		std.blue_max = the_best->visual->blue_mask;
		std.blue_mult = 1;
		while(!(std.blue_max & 0x01)) {
		    std.blue_max >>= 1;
		    std.blue_mult <<= 1;
		}
		std.base_pixel = 0;
		std.visualid = the_best->visual->visualid;
		std.killid = 0;
		*cm = std;
		property = XA_RGB_DEFAULT_MAP;
		found = TRUE;
	    }
	    break;
	}
	if (found) {
	    XSetRGBColormaps(display,
			     RootWindow(display, DefaultScreen(display)),
			     cm,1,property);
	    if (ndisplay)
		XSetCloseDownMode(ndisplay, RetainPermanent);
	}
	if (ndisplay)
	    XCloseDisplay(ndisplay);
	if (!found) {
	    XFree((caddr_t)available_visuals);
	    return(0);
	}
    }
    *vs = *(the_best->visual);
    *depth = the_best->depth;

    XFree((caddr_t)available_visuals);
    return(1);
}

SetVendorEntries()
{

    /* Get the System name from the X Display Type entry */
    BRF_appendToColon(brf_sysinfo[NSYSLN],ServerVendor(dpy));

    /* Get the number of bit Planes for this display */
    sprintf(tmpln,"%ld Bit Planes",DisplayPlanes(dpy,DefaultScreen(dpy)));
    BRF_appendToColon(brf_sysinfo[NHRDLN],tmpln);

    /* Use the Version and Revision numbers from the display */
    sprintf(tmpln,"X%ldR%ld",ProtocolVersion(dpy),ProtocolRevision(dpy));
    BRF_appendToColon(brf_sysinfo[NWNVLN],tmpln);

    /* Don't know how to generally get the OS name */
#ifdef UTEK
    BRF_appendToColon(brf_sysinfo[NOSVLN],"UTek");
#endif /* UTEK */
#ifdef UTEKV
    BRF_appendToColon(brf_sysinfo[NOSVLN],"UTekV");
#endif /* UTEK */

    /* Need to find out the real Timing Accuracy for this system
     * Use the stopwatch function, this should be correct
     */
    sprintf(tmpln,"%.2lf ms",(double)stopwatch(WATCH_PRECISION));
    BRF_appendToColon(brf_sysinfo[NTMALN],tmpln);
}

CreateTheWindow(wkinfo,depth)
Wk_info *wkinfo;
int depth;
{
    static char *BIF_title = "NCGA Picture Level Benchmark";
    static char *BIF_icon_name = "NCGA PLB";
    XColor real;
    XEvent evt;
    int border_width;

    /* Set up default window attributes */
    xswa.backing_store = NotUseful;
    xswa.colormap = ci.colormap;
    xswa.event_mask = ExposureMask | StructureNotifyMask;

    /* Check for user preference borderWidth */
    border_width = 1;
    (void *)strcpy(tmpln, Prog_name);
    (void *)strcat(tmpln, ".borderWidth");
    if (XrmGetResource(dis_db, tmpln, (char *)NULL, &str_type, &val)) {
	if (sscanf(val.addr,"%i",&border_width) != 1)
	    border_width = 1;
    }

    /* Check for user preference border color */
    (void *)strcpy(tmpln, Prog_name);
    (void *)strcat(tmpln, ".bordercolor");
    if (XrmGetResource(dis_db, tmpln, (char *)NULL, &str_type, &val)) {
	if(XParseColor(dpy, ci.colormap, val.addr, &real) == 0) {
	    xswa.border_pixel = CALC_COLOR_PIXEL(ci,
						 (Pfloat)(real.red)/MXPIX,
						 (Pfloat)(real.green)/MXPIX,
						 (Pfloat)(real.blue)/MXPIX);
	}
    }
    else
	xswa.border_pixel = CALC_COLOR_PIXEL(ci,0.0,0.0,0.0);

    /* Check for user preference background color */
    ur = ug = ub = 0.0;
    user_background = FALSE;
    (void *)strcpy(tmpln, Prog_name);
    (void *)strcat(tmpln, ".background");
    if (XrmGetResource(dis_db, tmpln, (char *)NULL, &str_type, &val)) {
	if (XParseColor(dpy, ci.colormap, val.addr, &real)) {
	    ur = (Pfloat)real.red/MXPIX;
	    ug = (Pfloat)real.green/MXPIX;
	    ub = (Pfloat)real.blue/MXPIX;
	    user_background = TRUE;
	}
    }
    xswa.background_pixel = CALC_COLOR_PIXEL(ci,ur,ug,ub);

    /* Check for user preference size and postition */
    size_hints.flags = 0;
    size_hints.x = size_hints.y = 100;
    size_hints.width = wkinfo->x;
    size_hints.height = wkinfo->y;

    (void *)strcpy(tmpln, Prog_name);
    (void *)strcat(tmpln, ".geometry");
    if (XrmGetResource(dis_db, tmpln, (char *)NULL, &str_type, &val)) {
	char Geometry[20];
	int bits, x, y;
	unsigned int width, height;

	(void *)strcpy(Geometry, val.addr);
	bits = XParseGeometry(Geometry, &x, &y, &width, &height);

	if (bits & (WidthValue)) {
	    size_hints.flags |= USSize;
	    size_hints.width = width;
	}

	if (bits & (HeightValue)) {
	    size_hints.flags |= USSize;
	    size_hints.height = height;
	}

	if (bits & XValue) {
	    if (bits & XNegative)
		x = DisplayWidth(dpy, DefaultScreen(dpy)) +
		    x - size_hints.width;
	    size_hints.flags |= USPosition;
	    size_hints.x = x;
	}

	if (bits & YValue) {
	    if (bits & YNegative)
		y = DisplayHeight(dpy, DefaultScreen(dpy)) +
		    y - size_hints.height;
	    size_hints.flags |= USPosition;
	    size_hints.y = y;
	}
    }

    /* Create the window */
    win = XCreateWindow(dpy, RootWindow(dpy, DefaultScreen(dpy)),
			size_hints.x, size_hints.y, size_hints.width,
			size_hints.height, border_width, depth,
			InputOutput, &visual,
			CWEventMask | CWColormap | CWBackingStore
			| CWBorderPixel | CWBackPixel, &xswa);

    XSetStandardProperties( dpy, win, BIF_title, BIF_icon_name,
			   None, Argv, Argc, &size_hints );

    XMapWindow(dpy, win);

    do {
	XNextEvent(dpy, &evt);
    } while(evt.type != Expose);

    XSelectInput(dpy, win, (unsigned long)0);
}

GetWorkstationInfo(ws, workid, wkinfo)
Pint ws;
Pint workid;
Wk_info *wkinfo;
{
    Pws_tables_plus gpc_ws_tables;
    Pint err_ind,length,lin_len,shd_len,refl_len,hat_len;
    Pint_list gpc_inq_int_list;
    Pint gpc_inq_ints[LIST_SIZE];
    Pint hlhsr_mode;
    Pmarker_facs gpc_marker_facs;
    Pline_facs_plus gpc_ext_line_facs;
    Pint_facs_plus gpc_ext_int_facs;
    Pedge_facs gpc_edge_facs;
    int listptr;
    int i,j;

    pset_colr_model((Pint)workid, (Pint)wkinfo->color_model);

    pinq_ws_st_table_plus(ws,&err_ind,&gpc_ws_tables);
    wkinfo->cmap_size = gpc_ws_tables.colr_reps - 2;
    wkinfo->view_size = gpc_ws_tables.view_reps - 1;
    wkinfo->dcue_size = gpc_ws_tables.dcue_rep - 1;
    wkinfo->light_size = gpc_ws_tables.light_src_rep;

    /* Set up exceptions based on workstation facilities */
    listptr = brf_num_ex_tests; /* get the number of messages */

    /* Marker Exceptions */
    pinq_marker_facs(ws,0,0,&err_ind,&gpc_marker_facs,&length);
    if (!err_ind && (length < LIST_SIZE)) {
	gpc_marker_facs.types.ints = gpc_inq_ints;
	pinq_marker_facs(ws,length,0,&err_ind,&gpc_marker_facs,
			 &length);

	for (i=0;i < NUM_MARKER_TYPES;i++) {
	    for (j=0;j < gpc_marker_facs.types.num_ints;j++) {
		if (gpc_marker_facs.types.ints[j] == marker_types[i].type)
		    break;
	    }
	    if (j == gpc_marker_facs.types.num_ints) {
		LIST_INVERT_IRANGE(MARKER_TYPE,BRF_WARNING,
				   marker_types[i].type,
				   marker_types[i].type,BRF_intrange,
				   marker_types[i].type_string);
	    }
	}

	LIST_RRANGE(MARKER_SIZE,BRF_WARNING,gpc_marker_facs.min_size,
		    gpc_marker_facs.max_size,BRF_realrange,
		    "MARKER_WIDTH Scale_factor(s) not supported.\n");
    }

    /* Line Exceptions */
    pinq_line_facs_plus(ws,0,0,0,0,&err_ind,&lin_len,&shd_len,
			&gpc_ext_line_facs);
    if (!err_ind && ((lin_len + shd_len) < LIST_SIZE)) {
	gpc_ext_line_facs.types.ints = gpc_inq_ints;
	gpc_ext_line_facs.shads.ints = &gpc_inq_ints[lin_len];
	pinq_line_facs_plus(ws,lin_len,0,shd_len,0,&err_ind,
			    &lin_len,&shd_len,&gpc_ext_line_facs);

	for (i=0;i < NUM_LINE_TYPES;i++) {
	    for (j=0;j < gpc_ext_line_facs.types.num_ints;j++) {
		if (gpc_ext_line_facs.types.ints[j] == line_types[i].type)
		    break;
	    }
	    if (j == gpc_ext_line_facs.types.num_ints) {
		LIST_INVERT_IRANGE(LINE_TYPE,BRF_WARNING,
				   line_types[i].type,
				   line_types[i].type,BRF_intrange,
				   line_types[i].type_string);
	    }
	}

	for (i=0;i < NUM_LINE_SHADES;i++) {
	    for (j=0;j < gpc_ext_line_facs.shads.num_ints;j++) {
		if (gpc_ext_line_facs.shads.ints[j] == line_shades[i].type)
		    break;
	    }
	    if (j == gpc_ext_line_facs.shads.num_ints) {
		LIST_INVERT_IRANGE(LINE_SHADING,BRF_WARNING,
				   line_shades[i].type,
				   line_shades[i].type,BRF_intrange,
				   line_shades[i].type_string);
	    }
	}

	LIST_RRANGE(LINE_WIDTH,BRF_WARNING,gpc_ext_line_facs.min_width,
		    gpc_ext_line_facs.max_width,BRF_realrange,
		    "LINE_WIDTH Scale_factor(s) not supported.\n");
    }

    /* Pattern Index Exceptions */
    pinq_pat_facs(ws,&err_ind,&length);
    if (!err_ind && length) {
	LIST_IRANGE(INTERIOR_PATTERN_INDEX,BRF_WARNING,0,length,
		    BRF_intrange,
		    "INTERIOR_PATTERN_INDEX not supported.\n");
    }
    else {
	LIST_NONSUP(INTERIOR_PATTERN_INDEX,BRF_ERROR,
		    "INTERIOR_PATTERN_INDEX not supported.\n");
    }

    /* Interior Exceptions */
    pinq_int_facs_plus(ws,0,0,0,0,0,0,0,0,&err_ind,
		       &gpc_ext_int_facs,&lin_len,&hat_len,
		       &refl_len,&shd_len);
    if (!err_ind && ((lin_len + refl_len + shd_len) < LIST_SIZE)) {
	gpc_ext_int_facs.int_styles = (Pint_style *)&gpc_inq_ints[0];
	gpc_ext_int_facs.refl_eqns.ints = &gpc_inq_ints[lin_len];
	gpc_ext_int_facs.shad_meths.ints = &gpc_inq_ints[lin_len+refl_len];
	pinq_int_facs_plus(ws,lin_len,0,0,0,refl_len,0,shd_len,0,
			   &err_ind,&gpc_ext_int_facs,&lin_len,&hat_len,
			   &refl_len,&shd_len);

	for (i=0;i < NUM_INTERIOR_TYPES;i++) {
	    for (j=0;j < gpc_ext_int_facs.num_int_styles;j++) {
		if (gpc_ext_int_facs.int_styles[j] == interior_types[i].type)
		    break;
	    }
	    if (j == gpc_ext_int_facs.num_int_styles) {
		LIST_INVERT_IRANGE(INTERIOR_STYLE,BRF_WARNING,
				   interior_types[i].type,
				   interior_types[i].type,BRF_intrange,
				   interior_types[i].type_string);
	    }
	}

	for (i=0;i < NUM_INTERIOR_REFLECS;i++) {
	    for (j=0;j < gpc_ext_int_facs.refl_eqns.num_ints;j++) {
		if (gpc_ext_int_facs.refl_eqns.ints[j] ==
		    interior_reflecs[i].type)
		    break;
	    }
	    if (j == gpc_ext_int_facs.refl_eqns.num_ints) {
		LIST_INVERT_IRANGE(INTERIOR_LIGHTING,BRF_WARNING,
				   interior_reflecs[i].type,
				   interior_reflecs[i].type,BRF_intrange,
				   interior_reflecs[i].type_string);
	    }
	}

	for (i=0;i < NUM_INTERIOR_SHADES;i++) {
	    for (j=0;j < gpc_ext_int_facs.shad_meths.num_ints;j++) {
		if (gpc_ext_int_facs.shad_meths.ints[j] ==
		    interior_shade_types[i].type)
		    break;
	    }
	    if (j == gpc_ext_int_facs.shad_meths.num_ints) {
		LIST_INVERT_IRANGE(INTERIOR_SHADING,BRF_WARNING,
				   interior_shade_types[i].type,
				   interior_shade_types[i].type,
				   BRF_intrange,
				   interior_shade_types[i].type_string);
	    }
	}
    }

    /* Edge Exceptions */
    pinq_edge_facs(ws,0,0,&err_ind,&gpc_edge_facs,&length);
    if (!err_ind && (length < LIST_SIZE)) {
	gpc_edge_facs.types.ints = gpc_inq_ints;
	pinq_edge_facs(ws,length,0,&err_ind,&gpc_edge_facs,
		       &length);

	for (i=0;i < NUM_EDGE_TYPES;i++) {
	    for (j=0;j < gpc_edge_facs.types.num_ints;j++) {
		if (gpc_edge_facs.types.ints[j] == edge_types[i].type)
		    break;
	    }
	    if (j == gpc_edge_facs.types.num_ints) {
		LIST_INVERT_IRANGE(EDGE_TYPE,BRF_WARNING,
				   edge_types[i].type,
				   edge_types[i].type,BRF_intrange,
				   edge_types[i].type_string);
	    }
	}

	LIST_RRANGE(EDGE_WIDTH,BRF_WARNING,gpc_edge_facs.min_width,
		    gpc_edge_facs.max_width,BRF_realrange,
		    "EDGE_WIDTH Scale_factor(s) not supported.\n");
    }

    /* NURB Exceptions */
    /*
     * There currently is no support for NURBs.
     * The API does not have an interface for SET TRIMMING CURVE
     * APPROXIMATION CRITERIA, and the PLB is not settled on the
     * enumeration for APPROXIMATION CRITERIA.
     */
    LIST_NONSUP(CURVE_APPROXIMATION_CRITERIA,BRF_ERROR,
		"CURVE_APPROXIMATION_CRITERIA not supported.\n");
    LIST_NONSUP(SURFACE_APPROXIMATION_CRITERIA,BRF_ERROR,
		"SURFACE_APPROXIMATION_CRITERIA not supported.\n");
    LIST_NONSUP(TRIMCURVE_APPROXIMATION_CRITERIA,BRF_ERROR,
		"TRIMCURVE_APPROXIMATION_CRITERIA not supported.\n");
    LIST_NONSUP(NON_UNIFORM_BSPLINE_CURVE,BRF_ERROR,
		"NON_UNIFORM_BSPLINE_CURVE not supported.\n");
    LIST_NONSUP(NON_UNIFORM_BSPLINE_SURFACE,BRF_ERROR,
		"NON_UNIFORM_BSPLINE_SURFACE not supported.\n");

/*----------------------------------------------------------------------*\
| 	Global Enable Hidden Surface Removal
\*----------------------------------------------------------------------*/

    /* Inquire on the Hidden Surface Removal type and set to an
       appropriate value. Preferred is PHIGS_HLHSR_MODE_ZBUFF. */
    hlhsr_mode = PHIGS_HLHSR_MODE_ZBUFF;

    /* Find out if the user wants a different mode */
    (void *)strcpy(tmpln, Prog_name);
    (void *)strcat(tmpln, ".hlhsrMode");
    if (XrmGetResource(dis_db, tmpln, (char *)NULL, &str_type, &val)) {
	upcase(val.addr,tmpln);
	for (i = 0;i < NUM_HLHSR_MODES;i++) {
	    if (!strcmp(tmpln,hlhsr_modes[i])) {
		hlhsr_mode = (Pint)i;
		break;
	    }
	}
    }

    gpc_inq_int_list.ints = gpc_inq_ints;
    pinq_hlhsr_mode_facs(ws,LIST_SIZE,0,&err_ind,&gpc_inq_int_list,
			 &length);
    if (!err_ind) {
	for (i = 0;i < gpc_inq_int_list.num_ints;i++) {
	    if (gpc_inq_int_list.ints[i] == hlhsr_mode) {
		pset_hlhsr_mode(workid,hlhsr_mode);
		break;
	    }
	}
	if ( i == gpc_inq_int_list.num_ints ) {
	    LIST_INVERT_IRANGE(HLHS_REMOVAL,BRF_WARNING,
			       BIF_HLHS_ENABLE,BIF_HLHS_ENABLE,
			       BRF_intrange,
			       "HLHS_REMOVAL not supported.\n");
	    LIST_INVERT_IRANGE(HLHSRID,BRF_WARNING,
			       BIF_HLHS_ENABLE,BIF_HLHS_ENABLE,
			       BRF_intrange,
			       "HLHSRID not supported.\n");
	}
    }

    brf_num_ex_tests = listptr; /* save the number of messages */
}

SetDefaultColors(work_id)
Pint work_id;
{
    Pint indx;
    Pcolr_rep rep;

    /* Map location 0, background color */
    indx = 0; rep.rgb.red = ur; rep.rgb.green = ug; rep.rgb.blue = ub;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 1, color Black */
    indx = 1; rep.rgb.red = 0.0; rep.rgb.green = 0.0; rep.rgb.blue = 0.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 2, color White */
    indx = 2; rep.rgb.red = 1.0; rep.rgb.green = 1.0; rep.rgb.blue = 1.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 3, color Red */
    indx = 3; rep.rgb.red = 1.0; rep.rgb.green = 0.0; rep.rgb.blue = 0.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 4, color Green */
    indx = 4; rep.rgb.red = 0.0; rep.rgb.green = 1.0; rep.rgb.blue = 0.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 5, color Blue */
    indx = 5; rep.rgb.red = 0.0; rep.rgb.green = 0.0; rep.rgb.blue = 1.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 6, color Cyan */
    indx = 6; rep.rgb.red = 0.0; rep.rgb.green = 1.0; rep.rgb.blue = 1.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 7, color Magenta */
    indx = 7;  rep.rgb.red = 1.0; rep.rgb.green = 0.0; rep.rgb.blue = 1.0;
    pset_colr_rep(work_id, indx, &rep);

    /* Map location 8, color Yellow */
    indx = 8; rep.rgb.red = 1.0; rep.rgb.green = 1.0; rep.rgb.blue = 0.0;
    pset_colr_rep(work_id, indx, &rep);

}
#endif /* USING_PHIGS */
