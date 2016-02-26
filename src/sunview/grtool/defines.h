/*
	defines.h

	$Header: defines.h,v 1.3 89/08/08 21:46:59 pturner Locked $
*/

/* #define LOCAL			/* My version or yours */

#define onoff(x) ((x)?"ON":"OFF")

#define BIG 1.7014118346047e+308
#define MBIG -1.0e+307

#define MAXPLOT 15		/* max number of sets */
#define MAXARR 2048		/* max elements in an array */
#define MAXFIT 7		/* max degree of polynomial+1 that can be
				 * fitted */

#define MAXBOXES 15		/* max number of boxes */
#define MAXLINES 15		/* max number of lines */

#define MAXSYM 22		/* max number of symbols */

#define MAXSTR 15		/* max number of strings */

#define HISTOSYM 18		/* plotsymbol for histograms */

/* set HDEV to the default hardcopy device */
/*  1 = HP 7550 8.5x11 landscape */
/*  2 = HP 7550 8.5x11 portrait */
/*  3 = HP 7550 11x17 landscape */
/*  4 = HP 7550 11x17 portrait */
/*  5 = Generic */
/*  6 = PostScript landscape */
/*  7 = PostScript portrait */
/*  8 = HPGL cartridge in LaserJet II landscape */
/*  9 = HPGL cartridge in LaserJet II portrait */

#define HDEV 1			/* default hardcopy device */
				/* for our site a 7550 in landscape mode */

#define TDEV 0			/* default terminal device - sunview only */

#define TRUE 1
#define FALSE 0

#define WINDOWW 1261		/* window dimensions - reduce for lower
				 * resolution monitors - say 800 */
#define WINDOWH 1047

#define TWINDOWW 600		/* size of textsw */
#define TWINDOWH 500

#define SIZE 500		/* size of popups */

/* for canvas event proc */
#define SELECT_REGION 256
#define RUBBER_BAND 512
#define ZOOM_1ST 3
#define ZOOM_2ND 4
#define VIEW_1ST 5
#define VIEW_2ND 6
#define STR_LOC 7
#define LEG_LOC 8
#define FIND_POINT 9
#define DEL_POINT 10
#define DEL_OBJECT 11
#define MOVE_OBJECT_1ST 12
#define MOVE_OBJECT_2ND 13
#define MAKE_BOX_1ST 14
#define MAKE_BOX_2ND 15
#define MAKE_LINE_1ST 16
#define MAKE_LINE_2ND 17
