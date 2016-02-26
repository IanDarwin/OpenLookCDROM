/* $Id: main.c,v 1.48 1992/07/25 16:46:49 pturner Exp pturner $
 *
 * (C) COPYRIGHT 1991, 1992 Paul J Turner
 * All Rights Reserved
 *
 * ACE/GR IS PROVIDED "AS IS" AND WITHOUT ANY WARRANTY EXPRESS OR IMPLIED. THE
 * USER ASSUMES ALL RISKS OF USING  ACE/GR.  THERE IS NO CLAIM OF THE
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * YOU MAY MAKE COPIES OF ACE/GR FOR YOUR OWN USE, AND MODIFY THOSE COPIES.
 * YOU MAY NOT DISTRIBUTE ANY MODIFIED SOURCE CODE OR DOCUMENTATION TO USERS
 * AT ANY SITES OTHER THAN YOUR OWN.
 *
 * ACE/gr - Graphics for Exploratory Data Analysis
 * xvgr - The XView version
 * xmgr - The Motif version
 *
 * Comments, bug reports, etc to:
 *
 * Paul J. Turner
 * Department of Environmental Science and Engineering
 * Oregon Graduate Institute of Science and Technology
 * 19600 NW von Neumann Dr.
 * Beaverton, OR  97006-1999
 * pturner@amb4.ese.ogi.edu
 *
*/

/*
 *
 * main.c - entry point
 *
 */

/* for globals.h */
#define MAIN

#include <stdio.h>
#include <math.h>
#include "globals.h"

void usage();

int bc = 0;			/* for X11R4 servers w/XView 2.0 */

void main(argc, argv)
    int argc;
    char *argv[];

{
    FILE *fp;
    int i, j;
    int gno;
    int cur_graph = cg;		/* default graph is graph 0 */
    int loadlegend = 0;		/* legend on and load file names */
    int gcols = 1, grows = 1;
    int autoscale[200];		/* check for autoscaling override */
    int noautoscale[200];	/* check for no autoscaling override */
    extern int gotbatch;	/* flag if a parameter file to execute */
    extern int realtime;	/* if data is to be plotted as it is read in */
    extern int inpipe;		/* if xmgr is to participate in a pipe */
    extern char batchfile[];	/* name of file to execute */
    extern int density_flag;	/* temp, for density plots, defined in
				 * plotone.c */
    char xvgrrc_file[256], *s, *getenv();

    getcwd(currentdir, 1024);	/* get the starting directory */

    printf("%s\n", version);
    printf("(C) Copyright 1991, 1992 Paul J. Turner\n");
    printf("All Rights Reserved\n");

    for (i = 0; i < maxgraph; i++) {
	autoscale[i] = 0;
	noautoscale[i] = 0;
    }

    /*
     * if program name is grbatch then don't initialize the X toolkit - set
     * hardcopyflag for batch plotting
     */
    if (strcmp("grbatch", argv[0])) {
	initialize_screen(&argc, argv);
    } else {
	hardcopyflag = TRUE;
    }

    /* initialize plots, strings, graphs */
    set_program_defaults();

    /* initialize colormap data */
    initialize_cms_data();

    /* initialize the rng */
    srand48(100L);

    /* initialize device, here tdevice is always 0 = Xlib */
    device = tdevice = 0;

    /* check for startup file in effective users home dir */
    if ((s = getenv("HOME")) != NULL) {
	strcpy(xvgrrc_file, s);
#ifdef XVIEW
	strcat(xvgrrc_file, "/.xvgrrc");
#endif
#ifdef MOTIF
	strcat(xvgrrc_file, "/.xmgrrc");
#endif
	if ((fp = fopen(xvgrrc_file, "r")) != NULL) {
	    fclose(fp);
	    getparms(cur_graph, xvgrrc_file);
	}
    } else {
	fprintf(stderr, "Unable to read environment variable HOME, skipping startup file\n");
    }
    plfile[0] = 0;		/* parameter file name */

    /*
     * check for changed printer spooling strings
     */
    if ((s = getenv("GR_PS_PRSTR")) != NULL) {
	strcpy(ps_prstr, s);
    }
    if ((s = getenv("GR_MIF_PRSTR")) != NULL) {
	strcpy(mif_prstr, s);
    }
    if ((s = getenv("GR_HPGL_PRSTR")) != NULL) {
	strcpy(hp_prstr1, s);
    }
    if ((s = getenv("GR_LEAF_PRSTR")) != NULL) {
	strcpy(leaf_prstr, s);
    }
    /*
     * check for changed hardcopy device
     */
    if ((s = getenv("GR_HDEV")) != NULL) {
	hdevice = atoi(s);
    }
    set_printer(hdevice, NULL);

    set_graph_active(cur_graph);

    if (argc >= 2) {
	for (i = 1; i < argc; i++) {
	    if (argv[i][0] == '-') {
		if (argmatch(argv[i], "-debug", 6)) {
#ifdef LOCAL
#define YYDEBUG
#endif

#ifdef YYDEBUG
		    extern int yydebug;

#endif

		    i++;
		    debuglevel = atoi(argv[i]);
		    if (debuglevel == 4) {	/* turn on debugging in
						 * pars.y */

#ifdef YYDEBUG
			yydebug = 1;
#endif

		    }
		} else if (argmatch(argv[i], "-autoscale", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for autoscale flag, should be one of 'x', 'y', 'xy'\n");
			usage(argv[0]);
		    } else {
			if (!strcmp("x", argv[i])) {
			    autoscale[cur_graph] = 1;
			} else if (!strcmp("y", argv[i])) {
			    autoscale[cur_graph] = 2;
			} else if (!strcmp("xy", argv[i])) {
			    autoscale[cur_graph] = 3;
			} else {
			    fprintf(stderr, "%s: Improper argument for -a flag should be one of 'x', 'y', 'xy'\n", argv[0]);
			    usage(argv[0]);
			}
		    }
		} else if (argmatch(argv[i], "-noauto", 7)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for no autoscale flag, should be one of 'x', 'y', 'xy'\n");
			usage(argv[0]);
		    } else {
			if (!strcmp("x", argv[i])) {
			    noautoscale[cur_graph] = 1;
			} else if (!strcmp("y", argv[i])) {
			    noautoscale[cur_graph] = 2;
			} else if (!strcmp("xy", argv[i])) {
			    noautoscale[cur_graph] = 3;
			} else {
			    fprintf(stderr, "%s: Improper argument for -noauto flag should be one of 'x', 'y', 'xy'\n", argv[0]);
			    usage(argv[0]);
			}
		    }
		} else if (argmatch(argv[i], "-batch", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for batch file\n");
			usage(argv[0]);
		    } else {
			gotbatch = TRUE;
			strcpy(batchfile, argv[i]);
		    }
		} else if (argmatch(argv[i], "-pipe", 5)) {
		    inpipe = TRUE;
		} else if (argmatch(argv[i], "-realtime", 5)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for realtime plotting\n");
			usage(argv[0]);
		    } else {
			realtime = atoi(argv[i]);
		    }
		} else if (argmatch(argv[i], "-maxsets", 8)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for max number of sets\n");
			usage(argv[0]);
		    } else {
			maxplot = atoi(argv[i]);
			realloc_plots(maxplot);
		    }
		} else if (argmatch(argv[i], "-graphsets", 10)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for max number of sets for he current graph\n");
			usage(argv[0]);
		    } else {
			realloc_graph_plots(cur_graph, atoi(argv[i]));
		    }
		} else if (argmatch(argv[i], "-maxgraph", 8)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for max number of graphs\n");
			usage(argv[0]);
		    } else {
			maxgraph = atoi(argv[i]);
			realloc_graphs();
		    }
/*
		} else if (argmatch(argv[i], "-defaultcmap", 12)) {
		    use_defaultcmap = 0;
*/
		} else if (argmatch(argv[i], "-mono", 5)) {
		    monomode = 1;
		} else if (argmatch(argv[i], "-bs", 3)) {
		    backingstore = 1;
		} else if (argmatch(argv[i], "-nobs", 5)) {
		    backingstore = 0;
		} else if (argmatch(argv[i], "-dc", 3)) {
		    allow_dc = 1;
		} else if (argmatch(argv[i], "-nodc", 5)) {
		    allow_dc = 0;
		} else if (argmatch(argv[i], "-redraw", 7)) {
		    allow_refresh = 1;
		} else if (argmatch(argv[i], "-noredraw", 9)) {
		    allow_refresh = 0;
		} else if (argmatch(argv[i], "-maxcolors", 10)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for number of colors\n");
			usage(argv[0]);
		    }
		    maxcolors = atoi(argv[i]);
		    initialize_cms_data();
		    if (maxcolors > 256) {
			fprintf(stderr, "Number of colors exceeds 256, set to 256\n");
		    }
		} else if (argmatch(argv[i], "-refresh", 8)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for refresh initialization\n");
			usage(argv[0]);
		    }
		    redraw_now = atoi(argv[i]);
		} else if (argmatch(argv[i], "-GXxor", 6)) {
		    invert = 0;
		} else if (argmatch(argv[i], "-GXinvert", 6)) {
		    invert = 1;
		} else if (argmatch(argv[i], "-device", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for hardcopy device select flag\n");
			usage(argv[0]);
		    }
		    set_printer(atoi(argv[i]), NULL);
		} else if (argmatch(argv[i], "-log", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for log plots flag\n");
			usage(argv[0]);
		    }
		    if (!strcmp("x", argv[i])) {
			g[cur_graph].type = LOGX;
		    } else if (!strcmp("y", argv[i])) {
			g[cur_graph].type = LOGY;
		    } else if (!strcmp("xy", argv[i])) {
			g[cur_graph].type = LOGXY;
		    } else {
			fprintf(stderr, "%s: Improper argument for -l flag should be one of 'x', 'y', 'xy'\n", argv[0]);
			g[cur_graph].type = XY;
		    }
		} else if (argmatch(argv[i], "-printfile", 6)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing file name for printing\n");
			usage(argv[0]);
		    } else {
			set_printer(FILEP, argv[i]);
		    }
		} else if (argmatch(argv[i], "-pexec", 6)) {
		    if (i == argc) {
			fprintf(stderr, "Missing argument for exec\n");
			usage(argv[0]);
		    } else {
			char pstring[256];
			int ilen;

			i++;
			strcpy(pstring, argv[i]);
			ilen = strlen(pstring);
			pstring[ilen] = '\n';
			pstring[ilen + 1] = 0;
			read_param(pstring);
		    }
		} else if (argmatch(argv[i], "-graph", 6)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing parameter for graph select\n");
			usage(argv[0]);
		    } else {
			sscanf(argv[i], "%d", &gno);
			if (gno >= 0 && gno < maxgraph) {
			    cg = cur_graph = gno;
			    set_graph_active(gno);
			} else {
			    fprintf(stderr, "Graph number must be between 0 and %d\n", maxgraph - 1);
			}
		    }
		} else if (argmatch(argv[i], "-block", 6)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing parameter for block data\n");
			usage(argv[0]);
		    } else {
			if (getdata(cur_graph, argv[i], cursource, 5)) {
			}
		    }
		} else if (argmatch(argv[i], "-xy", 3)) {
		    curtype = XY;
		} else if (argmatch(argv[i], "-xydx", 5)) {
		    curtype = XYDX;
		} else if (argmatch(argv[i], "-xydy", 5)) {
		    curtype = XYDY;
		} else if (argmatch(argv[i], "-xydxdx", 7)) {
		    curtype = XYDXDX;
		} else if (argmatch(argv[i], "-xydydy", 7)) {
		    curtype = XYDYDY;
		} else if (argmatch(argv[i], "-xydxdy", 7)) {
		    curtype = XYDXDY;
		} else if (argmatch(argv[i], "-xyz", 4)) {
		    curtype = XYZ;
		} else if (argmatch(argv[i], "-xyd", 4)) {
		    curtype = XYZ;
		    density_flag = TRUE;
		} else if (argmatch(argv[i], "-xyr", 4)) {
		    curtype = XYRT;
		} else if (argmatch(argv[i], "-ihl", 4)) {
		    curtype = IHL;
		} else if (argmatch(argv[i], "-nxy", 4)) {
		    curtype = NXY;
		} else if (argmatch(argv[i], "-hilo", 5)) {
		    curtype = XYHILO;
		} else if (argmatch(argv[i], "-type", 2)) {
		    /* other file types here */
		    i++;
		    if (argmatch(argv[i], "bin", 3)) {
			curtype = BIN;
		    } else if (argmatch(argv[i], "xydx", 4)) {
			curtype = XYDX;
		    } else if (argmatch(argv[i], "xydy", 4)) {
			curtype = XYDY;
		    } else if (argmatch(argv[i], "xydxdx", 6)) {
			curtype = XYDXDX;
		    } else if (argmatch(argv[i], "xydydy", 6)) {
			curtype = XYDYDY;
		    } else if (argmatch(argv[i], "xydxdy", 6)) {
			curtype = XYDXDY;
		    } else if (argmatch(argv[i], "xyz", 3)) {
			curtype = XYZ;
		    } else if (argmatch(argv[i], "xyr", 3)) {
			curtype = XYRT;
		    } else if (argmatch(argv[i], "hilo", 4)) {
			curtype = XYHILO;
		    } else {
			fprintf(stderr, "%s: Unknown file type '%s' assuming XY\n", argv[0], argv[i]);
			curtype = XY;
		    }
		} else if (argmatch(argv[i], "-graphtype", 7)) {
		    if (i == argc) {
			fprintf(stderr, "Missing argument for graph type\n");
			usage(argv[0]);
		    } else {
			i++;
			if (!strcmp("xy", argv[i])) {
			    g[cur_graph].type = XY;
/*
			} else if (!strcmp("fixed", argv[i])) {
			    g[cur_graph].type = XYFIXED;
			} else if (!strcmp("polar", argv[i])) {
			    g[cur_graph].type = POLAR;
*/
			} else if (!strcmp("bar", argv[i])) {
			    g[cur_graph].type = BAR;
			} else if (!strcmp("hbar", argv[i])) {
			    g[cur_graph].type = HBAR;
			} else if (!strcmp("stackedbar", argv[i])) {
			    g[cur_graph].type = STACKEDBAR;
			} else if (!strcmp("stackedhbar", argv[i])) {
			    g[cur_graph].type = STACKEDHBAR;
			} else if (!strcmp("logx", argv[i])) {
			    g[cur_graph].type = LOGX;
			} else if (!strcmp("logy", argv[i])) {
			    g[cur_graph].type = LOGY;
			} else if (!strcmp("logxy", argv[i])) {
			    g[cur_graph].type = LOGXY;
			} else {
			    fprintf(stderr, "%s: Improper argument for -graphtype should be one of 'xy', 'logx', 'logy', 'logxy', 'bar', 'stackedbar'\n", argv[0]);
			    usage(argv[0]);
			}
		    }
		} else if (argmatch(argv[i], "-arrange", 7)) {
		    if (i == argc) {
			fprintf(stderr, "Missing argument for graph arrangement\n");
			usage(argv[0]);
		    } else {
			i++;
			grows = atoi(argv[i]);
			i++;
			gcols = atoi(argv[i]);
		    }
		} else if (argmatch(argv[i], "-cols", 5)) {
		    if (i == argc) {
			fprintf(stderr, "Missing argument for graph column arrangement\n");
			usage(argv[0]);
		    } else {
			i++;
			gcols = atoi(argv[i]);
		    }
		} else if (argmatch(argv[i], "-rows", 5)) {
		    if (i == argc) {
			fprintf(stderr, "Missing argument for graph row arrangement\n");
			usage(argv[0]);
		    } else {
			i++;
			grows = atoi(argv[i]);
		    }
		} else if (argmatch(argv[i], "-legend", 4)) {
		    if (i == argc) {
			fprintf(stderr, "Missing argument for -legend\n");
			usage(argv[0]);
		    } else {
			i++;
			if (!strcmp("load", argv[i])) {
			    loadlegend = TRUE;
			    g[cur_graph].l.active = ON;
			} else {
			    fprintf(stderr, "Improper argument for -legend\n");
			    usage(argv[0]);
			}
		    }
		} else if (argmatch(argv[i], "-rvideo", 7)) {
		    revflag = 1;
		} else if (argmatch(argv[i], "-param", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing parameter file name\n");
			usage(argv[0]);
		    } else {
			strcpy(plfile, argv[i]);
			if (!getparms(cur_graph, plfile)) {
			    g[cur_graph].parmsread = FALSE;
			    fprintf(stderr, "Unable to read parameter file %s\n", plfile);
			} else {
			    g[cur_graph].parmsread = TRUE;
			}
		    }
/*
		} else if (argmatch(argv[i], "-results", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing results file name\n");
			usage(argv[0]);
		    } else {
			strcpy(resfile, argv[i]);
		    }
*/
		} else if (argmatch(argv[i], "-source", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing argument for data source parameter\n");
			usage(argv[0]);
		    }
		    if (argmatch(argv[i], "pipe", 4)) {
			cursource = PIPE;
		    } else if (argmatch(argv[i], "disk", 4)) {
			cursource = DISK;
		    } else if (argmatch(argv[i], "stdin", 5)) {
			cursource = 2;
		    }
		    /* we are in a pipe */
		    if (cursource == 2) {
			if (getdata(cur_graph, "STDIN", cursource, curtype)) {
			}
		    }
		} else if (argmatch(argv[i], "-viewport", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing parameters for viewport setting\n");
			usage(argv[0]);
		    } else {
			g[cur_graph].v.xv1 = atof(argv[i++]);
			g[cur_graph].v.yv1 = atof(argv[i++]);
			g[cur_graph].v.xv2 = atof(argv[i++]);
			g[cur_graph].v.yv2 = atof(argv[i]);
		    }
		} else if (argmatch(argv[i], "-world", 2)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing parameters for world setting\n");
			usage(argv[0]);
		    } else {
			g[cur_graph].w.xg1 = atof(argv[i++]);
			g[cur_graph].w.yg1 = atof(argv[i++]);
			g[cur_graph].w.xg2 = atof(argv[i++]);
			g[cur_graph].w.yg2 = atof(argv[i]);
		    }
		} else if (argmatch(argv[i], "-seed", 5)) {
		    i++;
		    if (i == argc) {
			fprintf(stderr, "Missing seed for srand48()\n");
			usage(argv[0]);
		    } else {
			srand48(atol(argv[i])); /* note atol() */
		    }
		} else if (argmatch(argv[i], "-bc", 3)) {
		    bc = 1;
#ifdef MOTIF
		} else if (argmatch(argv[i], "-help", 5)) {
		    printf("\n%s understands all standard X Toolkit command-line options\n\n", argv[0]);
		    exit(0);
#endif
		} else if (argmatch(argv[i], "-usage", 2)) {
		    usage(argv[0]);
		} else {
		    fprintf(stderr, "No option %s\n", argv[i]);
		    usage(argv[0]);
		}
	    } else {
		/* a data file (we hope) */
		if (!(i == argc)) {
		    if (getdata(cur_graph, argv[i], cursource, curtype)) {
		    }
		}		/* end if */
	    }			/* end else */
	}			/* end for */
    }				/* end if */
    /*
     *  if successfully read data and no parameter file was read then find
     * appropriate defaults
     */
    for (i = 0; i < maxgraph; i++) {
	if (isactive_graph(i) && (activeset(i))) {
	    if (g[i].parmsread == FALSE) {
		if (noautoscale[i]) {
		    switch (noautoscale[i]) {
		    case 1:
			defaulty(i, -1);
			default_axis(i, g[i].auto_type, Y_AXIS);
			default_axis(i, g[i].auto_type, ZY_AXIS);
			break;
		    case 2:
			defaultx(i, -1);
			default_axis(i, g[i].auto_type, X_AXIS);
			default_axis(i, g[i].auto_type, ZX_AXIS);
			break;
		    case 3:
			/* no autoscaling in x or y */
			break;
		    }
		} else {
		    defaultgraph(i);
		    default_axis(i, g[i].auto_type, X_AXIS);
		    default_axis(i, g[i].auto_type, ZX_AXIS);
		    default_axis(i, g[i].auto_type, Y_AXIS);
		    default_axis(i, g[i].auto_type, ZY_AXIS);
		}
	    }
/*
 * if auto scaling type set with -a option, then scale appropriate axis
 */
	    else {
		if (autoscale[i]) {
		    switch (autoscale[i]) {
		    case 1:
			defaultx(i, -1);
			default_axis(i, g[i].auto_type, X_AXIS);
			default_axis(i, g[i].auto_type, ZX_AXIS);
			break;
		    case 2:
			defaulty(i, -1);
			default_axis(i, g[i].auto_type, Y_AXIS);
			default_axis(i, g[i].auto_type, ZY_AXIS);
			break;
		    case 3:
			defaultgraph(i);
			default_axis(i, g[i].auto_type, X_AXIS);
			default_axis(i, g[i].auto_type, ZX_AXIS);
			default_axis(i, g[i].auto_type, Y_AXIS);
			default_axis(i, g[i].auto_type, ZY_AXIS);
			break;
		    }
		}
	    }
	}
    }				/* end for */
    cg = 0;			/* default is graph 0 */
/*
 * load legend
 */
    if (loadlegend) {
	for (i = 0; i < maxgraph; i++) {
	    if (isactive_graph(i) && (activeset(i))) {
		for (j = 0; j < MAXPLOT; j++) {
		    if (isactive(i, j)) {
			strcpy(g[i].l.str[j].s, g[i].p[j].comments);
		    }
		}
	    }
	}
    }
/*
 * straighten our cursource if a pipe was used
 */
    if (cursource == 2) {
	cursource = DISK;
    }
/*
 * arrange graphs if grows & gcols set
 */
    arrange_graphs(grows, gcols);
/*
 * initialize the Hershey fonts
 */
    hselectfont(g[cg].d.font);
/*
 * if -h on command line just plot the graph and quit
 */
    if (hardcopyflag) {
	if (hdevice == 0) {
	    fprintf(stderr, "%s: Device 0 (Xlib) is not a legitimate device for batch plotting\n", argv[0]);
	    exit(1);
	}
	noerase = 1;
	do_hardcopy();
	exit(0);
    }
/*
 * go window things up - do_main_loop is in x[v,m]gr.c
 */
    defineworld(g[cg].w.xg1, g[cg].w.yg1, g[cg].w.xg2, g[cg].w.yg2, islogx(cg), islogy(cg));
    viewport(g[cg].v.xv1, g[cg].v.yv1, g[cg].v.xv2, g[cg].v.yv2);
    do_main_loop();
}

void usage(progname)
    char *progname;
{
    fprintf(stderr, "Usage of %s command line arguments: \n", progname);
    fprintf(stderr, "-maxsets   [number_of_sets]           Set the number of data sets per graph (minimum is 30)\n");
    fprintf(stderr, "-maxgraph  [number_of_graphs]         Set the number of graphs for this session (minimum is 10)\n");
    fprintf(stderr, "-autoscale [x|y|xy]                   Override any parameter file settings\n");
    fprintf(stderr, "-noauto    [x|y|xy]                   Supress autoscaling for the specified axis\n");
    fprintf(stderr, "-arrange   [rows] [cols]              Arrange the graphs in a grid rows by cols\n");
    fprintf(stderr, "-cols      [cols]\n");
    fprintf(stderr, "-rows      [rows]\n");
    fprintf(stderr, "-batch     [batch_file]               Execute batch_file on start up\n");
    fprintf(stderr, "-pipe                                 Read data from stdin on startup\n");
    fprintf(stderr, "-device    [hardcopy device number]\n");
#ifdef XVIEW
    fprintf(stderr, "-bc                                   Suppress double draw on start up for some (X11r4?) servers\n");
#endif
    fprintf(stderr, "-log       [x|y|xy]                   Set the graph type to logarithmic\n");
    fprintf(stderr, "-legend    [load]                     Turn the graph legend on\n");
    fprintf(stderr, "-printfile [file for hardcopy output]\n");
    fprintf(stderr, "-graph     [graph number]             Set the current graph number\n");
    fprintf(stderr, "-graphsets [number_of_sets]           Set the number of data sets for the current graph\n");
    fprintf(stderr, "-graphtype [xy|bar|stackedbar|hbar|stackedhbar]  Set the type of the current graph\n");
    fprintf(stderr, "-world     [xmin ymin xmax ymax]      Set the user coordinate system for the current graph\n");
    fprintf(stderr, "-view      [xmin ymin xmax ymax]      Set the viewport for the current graph\n");
    fprintf(stderr, "-results   [results_file]             write the results from regression to results_file\n");
    fprintf(stderr, "-source    [disk|pipe|stdin]          Source of next data file\n");
    fprintf(stderr, "-param     [parameter_file]           Load parameters from parameter_file to the current graph\n");
    fprintf(stderr, "-pexec     [parameter_string]         Interpret string as a parameter setting\n");
    fprintf(stderr, "-type      [xy|xydx|xydy|xydxdx|xydydy|hilo] Set the type of the next data file\n");
    fprintf(stderr, "-ihl       [ihl_file]                 Assume data file is in IHL format (local)\n");
    fprintf(stderr, "-xy       [xy_file]                   Assume data file is in X Y format - sets are separated by lines containing non-numeric data\n");
    fprintf(stderr, "-nxy       [nxy_file]                  Assume data file is in X Y1 Y2 Y3 ... format\n");
    fprintf(stderr, "-xydx      [xydx_file]                Assume data file is in X Y DX format\n");
    fprintf(stderr, "-xydy      [xydy_file]                Assume data file is in X Y DY format\n");
    fprintf(stderr, "-xydxdx    [xydxdx_file]              Assume data file is in X Y DX1 DX2 format\n");
    fprintf(stderr, "-xydydy    [xydydy_file]              Assume data file is in X Y DY1 DY2 format\n");
    fprintf(stderr, "-xydxdy    [xydxdy_file]              Assume data file is in X Y DX DY format\n");
    fprintf(stderr, "-xyz       [xyz_file]                 Assume data file is in X Y Z format\n");
    fprintf(stderr, "-xyd       [xyd_file]                 Assume data file is in X Y density format\n");
    fprintf(stderr, "-xyr       [xyr_file]                 Assume data file is in X Y RADIUS format\n");
    fprintf(stderr, "-block     [block_data]               Assume data file is block data\n");
    fprintf(stderr, "-hilo      [hilo_file]                Assume data is in X HI LO OPEN CLOSE format\n");
    fprintf(stderr, "-rvideo                               Exchange the color indices for black and white\n");
    fprintf(stderr, "-mono                                 Run %s in monochrome mode (affects the display only)\n", progname);
    fprintf(stderr, "-seed      [seed_value]               Integer seed for random number generator\n");
    fprintf(stderr, "-GXxor                                Use xor to draw rubberband lines and graph focus markers\n");
    fprintf(stderr, "-GXinvert                             Use invert to draw rubberband lines and graph focus markers\n");
    fprintf(stderr, "-bs                                   Do backing store\n");
    fprintf(stderr, "-nobs                                 Suppress backing store\n");
    fprintf(stderr, "-dc                                   Allow double click operations on the canvas\n");
    fprintf(stderr, "-nodc                                 Disallow double click operations on the canvas\n");
    fprintf(stderr, "-maxcolors  [max_colors]              Set the number of colors to allocate (minimum is 17)\n");
    fprintf(stderr, "-refresh    [value]                   Hack for initial draw on our RS6000's run without backing store - 2 is a good value\n");
    fprintf(stderr, "-redraw                               Do a redraw for refreshing the canvas when the server doesn't do backing store\n");
    fprintf(stderr, "-noredraw                             Don't do a redraw for refreshing the canvas when the server doesn't do backing store\n");
    fprintf(stderr, "-debug     [debug_level]              Set debugging options\n");
    fprintf(stderr, "-usage                                This message\n");
    exit(0);
}
