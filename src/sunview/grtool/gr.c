/*
 *
 * gr - Graphics for Exploratory Data Analysis
 *
 * Paul J. Turner
 * Department of Environmental Science and Engineering
 * Oregon Graduate Center
 * 19600 NW von Neumann Dr.
 * Beaverton, OR  97006-1999
 * pturner@cse.ogc.edu
 * tektronix!ogccse!pturner
 *
 * gr.c - entry point
 * 
 */

char *rcsid = "$Header: gr.c,v 1.12 89/09/12 05:10:22 pturner Locked $";

char *version = "Grtool version 1.01 Beta test";

char *open_err_msg = "Grtool needs more windows, please close some windows and try again";

#include <stdio.h>
#include "defines.h"

/*
	Global data
*/

/*
	set definition
*/

typedef struct {
    double *x;			/* ordinates */
    double *y;			/* abscissi */
    double xmin, xmax;		/* min max for x */
    double ymin, ymax;		/* min max for y */
    int len;			/* set length */
    int plotsym;		/* set plot symbol */
    int linesym;		/* set line style */
    int color;			/* color for symbol and linestyle */
    int active;			/* boolean true = > set will be plotted */
    int errbar;			/* boolean, true => set used as error bars */
    int errxy;			/* type of error bar */
    char comments[80];		/* how did this set originate */
} plotarr;

plotarr *plots[MAXPLOT];	/* sets go here */

int maxplot = MAXPLOT;		/* so the number of sets can be changed on
				 * the fly */
int maxarr = MAXARR;		/* so the lengths of sets may be changed on
				 * the fly */

double *ax, *bx, *cx, *dx;	/* scratch arrays used in scanner */

double xg1 = 0.0, xg2 = 1.0, yg1 = 0.0, yg2 = 1.0;	/* world coordinates */
double xv1 = 0.2, xv2 = 0.9, yv1 = 0.2, yv2 = 0.9;	/* viewpoint coordinates */
double xt1 = 1.0, xt2 = 1.0, yt1 = 1.0, yt2 = 1.0;	/* xmajor, xminor,
							 * ymajor, yminor tic
							 * marks */
char xlabel[80];		/* x-axis label */
char ylabel[80];		/* y-axis label */
char title[80];			/* graph title */
char stitle[80];		/* graph subtitle */

int xform;			/* format for x-axis labels */
int yform;			/* format for y-axis labels */

int hardcopyflag = FALSE;	/* TRUE if printing out a hardcopy */

int inplotter = FALSE;		/* TRUE if plotting the current graph */

int xticsintflag = FALSE;	/* major tic marks on integer divisions */
int yticsintflag = FALSE;	/* not used in grtool */

int boxflag = TRUE;		/* boxflag=TRUE => closed box, =FALSE => half
				 * open */
int boxon = TRUE;		/* toggle box on or off */

int xticflag = TRUE;		/* toggle ticmark display */
int yticflag = TRUE;

int fformx = TRUE;		/* decimal or exponential ticmark labels .. */
int fformy = TRUE;		/* default to decimal */

int xticlflag = TRUE;		/* toggle ticmark labels on or off */
int yticlflag = TRUE;

int xgridflag = FALSE;		/* toggle ticmarks as grid lines */
int ygridflag = FALSE;

int xticslog = FALSE;		/* logarithmic ticmarks */
int yticslog = FALSE;
int logtransflag = FALSE;	/* TRUE if all data has been logarithmized */

int xticinoutflag = FALSE;	/* tics inward by default */
int yticinoutflag = FALSE;

int xabsflag = FALSE;	/* tic labels absolute value */
int yabsflag = FALSE;

int xticopflag = TRUE;		/* tics on opposite side */
int yticopflag = TRUE;
int xtopflag = FALSE;
int ytopflag = FALSE;
int xticangle = 0;

int xzflag = TRUE;		/* toggle the lines x=0 or y=0 */
int yzflag = TRUE;
int xztflag = FALSE;		/* toggle ticmarks for the lines x=0, y=0 */
int yztflag = FALSE;

int defline = 1;	/* default linestyle */
int curfont = 2;	/* default font */

int legendflag = FALSE; /* flag for legend */
int lgap = 2,llen = 6;  /* gap between legned entries and length of legend line */
double legx, legy;	/* legend location */

double errbarper = 1.0; /* length of error bar */

char plfile[80];		/* load paramters file name */
char fname[80];			/* last data file read */

int device;			/* graphics device */
int tdevice = TDEV;		/* default devices */
int hdevice = HDEV;

int erronread = FALSE;		/* in startup code was there an error on
				 * reading a data file, assume not  */
char progname[80];		/* our name */

int gflag = FALSE;		/* hacks for generic graphics drivers */
char gfile[80];			/* used if gflag set to true on command line */
char resfile[80];		/* results to file resfile */

int inwin = FALSE;		/* true if running sunview */

void main(argc, argv)
    int argc;
    char *argv[];

{
    int i, pstat, parmsread, readihl = 0;
    char tmpstr[80], *getcomment();

    initplots();
    strcpy(progname, argv[0]);
    device = tdevice;
    pstat = FALSE;
    parmsread = FALSE;
    if (argc >= 2) {
	for (i = 1; i < argc; i++) {
	    if (argv[i][0] == '-') {
		switch (argv[i][1]) {
		case 'p':	/* read a parameter file */
		    i++;
		    if (i == argc)
			fprintf(stderr, "Missing parameter file name");
		    else {
			strcpy(plfile, argv[i]);
			getparms(plfile);
			parmsread = (plfile[0]);
		    }
		    break;
		case 'h':	/* plot using hardcopy device */
		    hardcopyflag = TRUE;
		    break;
		case 'r':	/* results from regression routines to file */
		    i++;
		    strcpy(resfile, argv[i]);
		    break;
		case 'i':	/* IHL format */
		    readihl = 3;
		    break;
		case 'd':	/* select hardcopy device */
		    i++;
		    hdevice = atoi(argv[i]);
		    break;
		case 'f':	/* for generic graphics output */
		    i++;
		    strcpy(gfile, argv[i]);
		    gflag = TRUE;
		    break;
		default:
		    sprintf(tmpstr, "No option %c\n", argv[i][1]);
		    i++;
		}
	    } else {
		if (!(i == argc)) {
		    if (getdata(argv[i], readihl)) {
			pstat = TRUE;
			readihl = 0;
		    }
		}
	    }
	}
/*
/* if successfully read data and no parameter
/* file was read then find appropriate defaults
*/
	if ((pstat) && (!parmsread)) {
	    defaultgraph();
	    defaulttics(0);
	}
    }
    hselectfont(2);
/*
 * if -h on command line just plot the graph and quit
 * - sunview is not a legitimate device
*/
    if (hardcopyflag) {
	device = hdevice;
	plotone();
	exit(0);
    }
/*
 * go window things up
*/
    do_main_loop(argc, argv);
}
