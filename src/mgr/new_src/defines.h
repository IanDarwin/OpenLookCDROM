/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/defines.h,v 1.3 91/03/01 11:05:23 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/defines.h,v $
*/
static char	h_defines_[] = "$Source: /home/sau/mgr/nsrc/RCS/defines.h,v $$Revision: 1.3 $";

/* potentially changable defines for mgr */

#define MAXWIN 		24		/* Maximum number of windows */
					/* < getdtablesize()-5 */
#define MAXCLIENT	25		/* max number of client windows */
#define MAXESC		20		/* max number of leading esc. digits */
#define TEXT_COUNT	MAXESC-1	/* text download index */
#define MAXBUF		80		/* max chars put to window per cycle */
#define MAXSHELL	128		/* max chars read from shell at once */
#define POLL_INT	0		/* us's of pause at each select */
#define MAXNAME		35		/* max argv[0] length */
#define MAXTTY		35		/* max tty device name length */
#define MAXFONT		100		/* # of different fonts */
#define MAXMENU		20		/* max number of menus per window */
#define MAXBITMAPS	50		/* max number of bitmaps per window */
#define MAXITEMS	200		/* 2 * max # of menu items */
#define MAXEVENTS	22		/* max number of events, -4 thru 17,
					see event.h */
#define MIN_Y		1		/* minimum # of rows in a window */
#define MIN_X		5		/* min # of columns in a window */
#define MAX_PATH	100		/* max path length for font file */
#define GMAX		1000L		/* max graphics coordinate */
#define MOUSE_BUFF	30		/* size of mouse input buffer */
#define MSG_MODEMASK	02		/* invalid permission mask for tty */

#define SCREEN_DEV	"/dev/fb"	/* where to find the frame buffer */
#define MOUSE_DEV	"/dev/mouse"	/* where to find the mouse */
#define TERMNAME	"mgr"		/* name of termcap entry */
#define STARTFILE	".mgrc"		/* name of mgr startup file */
#define HOST		"win "		/* name of host for utmp file */
#ifndef FONTDIR
#  define FONTDIR		"/usr/mgr/font" /* where to find the fonts */
#endif
#ifndef ICONDIR
#  define ICONDIR		"/usr/mgr/icon"	/* readable by all */
#endif
#define DEFAULT_FONT	"DEFAULT_FONT"	/* default font environ variable (full path name) */
#define DUP_CHAR	'\005'		/* default dup character for DUPKEY mode */

#define COLORMAP_SIZE	10		/* max number of "built-in" colors */
#define MENU_COLOR	BUILDOP(BIT_SRC,color_map[4],color_map[5])/* menu colors */

#define BLK_BDR		2		/* thickness of border */
#define WH_BDR		3		/* border gap */
#define SUM_BDR		(BLK_BDR + WH_BDR)
#define STRIPE		2		/* # windowless pixels on left */

#define M_QUIT		3		/* confirm quit choice */
#define M_SUSPEND	1		/* suspend instead of quit */

#ifdef DEBUG
extern char debug_level[];
char *index();
#define dprintf(x) \
	if (debug && index(debug_level,'x')) fprintf
#endif
