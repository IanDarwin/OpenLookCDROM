/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern "C" {
char *		getenv(char *);
char *		strcmp(char *, char *);
char *		strcpy(char *, char *);
char *		strncmp(char *, char *, int);
char *		strrchr(char *, int);
}

extern void	ice_usage();
extern void	init_windows();
extern void	ice_read(char *);
extern void	ice_event(XEvent *);
extern void	stdinpsd_proc();
extern void	wmpr_event(XClientMessageEvent *);

main(int argc, char *argv[])
{
	XEvent event;
	int stdin_contents;

	if ((progname= strrchr(*argv, (int) '/')) != (char *) NULL)
		progname++;
	else
		progname= *argv;

	init_complete= FALSE;
	verbose= 0;
	ice_filenm= (char *) NULL;
	initpsd_filenm= (char *) NULL;
	stdin_contents= STDIN_EMPTY;
	xdisplay= (char *) NULL;
	newsdisplay= (char *) NULL;
	pg_width= 8.5;
	pg_height= 11.;
	pg_dpi= 82;
	pg_units= PG_PIXELS;
	pg_hsi= pg_vsi= 1.;
	pg_xri= pg_yri= 0.;
	pg_xru= pg_yru= 0.;
	pg_update= PG_AUTOMATIC;
	pg_forceupdate= FALSE;
	pg_loc= PG_CURSORLOC;
	pg_locdisplay= PG_DISPLAYLOC;
	pg_clip= (Path *) NULL;
	pg_originhlt= PG_NOORIGINHLT;
	pg_bg= PG_WHITEBG;
	pg_rbg= pg_gbg= pg_bbg= (unsigned char) 255;
	gdf_linewidth= 2.;
	gdf_fg= GDF_BLACKFG;
	gdf_rfg= gdf_gfg= gdf_bfg= (unsigned char) 0;
	gdf_bg= GDF_WHITEBG;
	gdf_rbg= gdf_gbg= gdf_bbg= (unsigned char) 255;
	gdf_mrktype= GDF_SQUARE;
	gdf_mrksize= 5.;
	gdf_bndwidth= 2.;
	gdf_bnd= GDF_BLACKBND;
	gdf_rbnd= gdf_gbnd= gdf_bbnd= (unsigned char) 0;
	gdf_fill= GDF_WHITEFILL;
	gdf_rfill= gdf_gfill= gdf_bfill= (unsigned char) 255;
	gdf_dtk= GDF_WHITEDTK;
	gdf_rdtk= gdf_gdtk= gdf_bdtk= (unsigned char) 255;
	ins_newobj= INS_DEFAULTS;
	grobjs= (Grobj *) NULL;
	npsdocs= nrasters= ntexts= nvectors= 0;
	ncurves= nmarkers= nrectangles= npolygons= naxes= 0;
	unnamed_texts= unnamed_vectors= unnamed_curves= unnamed_markers= 1;
	unnamed_rectangles= unnamed_polygons= unnamed_axes= 1;
	cmpobjs= (Composite *) NULL;
	paths= (Path *) NULL;
	npaths= 0;

	for (argc--, argv++; argc > 0; argc--, argv++) {
		if (!strcmp(*argv, "-display")) {
			argc--, argv++;
			if (argc <= 0)
				ice_usage();
			xdisplay= *argv;
		}
		else if (!strcmp(*argv, "-w")) {
			argc--, argv++;
			if (argc <= 0)
				ice_usage();
			pg_width= atof(*argv);
		}
		else if (!strcmp(*argv, "-h")) {
			argc--, argv++;
			if (argc <= 0)
				ice_usage();
			pg_height= atof(*argv);
		}
		else if (!strcmp(*argv, "-dpi")) {
			argc--, argv++;
			if (argc <= 0)
				ice_usage();
			pg_dpi= atoi(*argv);
		}
		else if (!strcmp(*argv, "-v")) {
			argc--, argv++;
			if (argc <= 0)
				ice_usage();
			verbose= atoi(*argv);
		}
		else if (!strcmp(*argv, "-ps"))
			stdin_contents= STDIN_POSTSCRIPT;
		else if (!strcmp(*argv, "-H"))
			ice_usage();
		else
			ice_filenm= *argv;
	}

	init_windows();
	ice_op= MAIN_MENU;

	switch (stdin_contents) {
	case STDIN_POSTSCRIPT:
		stdinpsd_proc();
		break;
	}
	if (ice_filenm != (char *) NULL)
		ice_read(ice_filenm);

	init_complete= TRUE;

	for (;;) {
		XNextEvent(dpy, &event);

		/* handle WM_PROTOCOL messages */
		if ((event.type == ClientMessage) &&
		    (event.xclient.message_type == wmpr_atom)) {
			wmpr_event((XClientMessageEvent *) &event);
			continue;
		}

		if (lxt_event(&event))
			continue;

		ice_event(&event);
	}
}
