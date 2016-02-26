/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#ifndef DEFAULTS_HEADER
#define DEFAULTS_HEADER

/* This file contains user defined DEFAULTS */
#define USAGE_STR "usage: dstool [-windows] [-tcl] [-pvm] [-version] [-debug] [-help] [-infile <filename>] [-outfile <filename>]"
#define MAX_CMD_LINE_ARG_LEN 200

/* Mode on startup: WINDOWS_MODE, BATCH_MODE, or TCL_MODE */
#define DEFAULT_MODE WINDOWS_MODE
#define DEBUG_MODE FALSE

/* automatically read configuration file */
#define CONFIG_FILENAME "dstool.config"

/* multiple */
#define MULT_RADIUS_FACTOR 0.01

/* pvm */
#define DSTOOL_PVM_SLAVE "dstool_pvm"

/* default values for the Defaults window */
#define DISP_TRAJ_ITERS 100       /* number of points to compute before displaying */
#define DEF_SYMBOL_INDEX 0        /* a point */
#define DEF_SYMBOL_SIZE_INDEX 1   /* medium */
#define DEF_PRECISION    8        /* sig digits displayed in text items */
#define DEF_SHOWCOLOR        0	  /* yes, show the colormap */
#define DEF_DEPTHCOORD       0	  /* make 1st varb depth-coding variable */
#define DEF_CMAPTYPE         0	  /* alternating colormap */
#define DEF_BGND_COLOR 0        /* background color for view windows */
#define DEF_COLORTABLE "colortable_alt" /* trajectory colortable */


/* defaults for computational routines */
#define DIVERG_CUTOFF 1.e8        /* infinity for propagating */





/* BELOW stuff not used yet - paw  4/18/93 */


/* file loaded into message win */
#define DSTOOL_MSG "dstool.msg"

/* default trajectory colormap */
#define TRAJ_COLORMAP "colormap"

/* default system colormap */
#define SYS_COLORMAP "sys_colormap"


/* defaults for orbit window */
#define TOTAL_ITERATES 5000
#define START_SAVE_POINTS 0			/* paw  4/14/92 */
#define SKIP_SIZE 1
#define STEPSIZE 0.01

/* default values for print window */
#define		PRINTER_NAME	"lp"
#define		PROLOG_FILE	"Prolog.ps"
#define		TITLE_PT_SIZE	14
#define		LABEL_PT_SIZE	10
#define		NUM_X_TICKS	3
#define		NUM_Y_TICKS	3
#define		BBOX_HOR_LEN	450 		/* fjw 7/30/92 */
#define		BBOX_VER_LEN	450
#define		BBOX_HOR_OFF	80
#define		BBOX_VER_OFF	145

#define		BUFFERSIZE	1024


/* defaults values for the periodic/fixed point algorithms */
#define         MAP_PERIOD      1
#define         VF_PERIOD       0.0
#define         FXPT_ALGORITHM  NEWTON             /* NEWTON or SECANT */
#define         FXPT_GUESS      0
#define         MC_GUESSES      10
#define         FIXPT_ITERS     30
#define 	DUP_DIFF	1.e-6
#define		VARB_CONV	1.e-9
#define		FUNCTION_CONV	1.e-8
#define		EIGEN_DIST	1.e-6
#define         STAB_POINTS     1
#define         STAB_STEPS      200
#define         UNSTAB_POINTS   1
#define         UNSTAB_STEPS    200
#define         DELTAX          1.e-6
#define         EVAL_TOLERANCE  1.e-6
#define         UNSTAB_MAN_COLOR SYS_RED
#define         UNSTAB_MAN_SYMBOL MED_POINT
#define         STAB_MAN_COLOR  SYS_BLUE
#define         STAB_MAN_SYMBOL MED_POINT
#define         SADDLE_COLOR    SYS_GREEN
#define         SADDLE_SYMBOL   LARGE_CROSS
#define         SOURCE_COLOR    SYS_RED
#define         SOURCE_SYMBOL   LARGE_BOX
#define         SINK_COLOR      SYS_BLUE
#define         SINK_SYMBOL     LARGE_TRI
#define         DEFAULT_FP_COLOR SYS_GREEN
#define         DEFAULT_FP_SYMBOL HUGE_POINT


/* default values for multiple window */
#define         MULT_MAXSTEPS   10000


#endif





