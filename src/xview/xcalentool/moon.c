/*
 * $Id: moon.c,v 2.3 1994/08/29 17:32:54 billr Exp $
 */
/*
 * moon.c
 *
 * Compute various interesting data about the moon.  Based on the
 * moontool program by John Walker (see below), and modified by
 * Bill Randle, Tektronix, Inc. to interface to the calentool
 * program.
 */
#include "ct.h"     /* for the NO_SUN_MOON #define */

#ifndef NO_SUN_MOON
/*

    A Moon for the Sun

    Release 2.3

    Designed and implemented by John Walker in December 1987,
    revised and updated in February of 1988.
    revised and updated again in June of 1988 by Ron Hitchens
    converted to XView/Xlib in June 1991 by Bill Randle

    This
    program is a SunView tool which displays, as the  icon  for  a  closed
    window,  the  current phase of the Moon.  A subtitle in the icon gives
    the age of the Moon in days  and  hours.   If  called  with  the  "-t"
    switch,  it  rapidly  increments  forward  through time to display the
    cycle of phases.

    If you open the window, additional information is displayed  regarding
    the  Moon.	 The  information  is  generally  accurate  to	within ten
    minutes.

    The algorithms used in this program to calculate the positions Sun and
    Moon as seen from the Earth are given in the book "Practical Astronomy
    With  Your  Calculator"  by  Peter  Duffett-Smith,   Second   Edition,
    Cambridge University Press, 1981.  Ignore the word "Calculator" in the
    title;  this  is  an  essential  reference  if  you're  interested  in
    developing	software  which  calculates  planetary	positions, orbits,
    eclipses, and  the  like.   If  you're  interested  in  pursuing  such
    programming, you should also obtain:

    "Astronomical  Formulae for Calculators" by Jean Meeus, Third Edition,
    Willmann-Bell, 1985.  A must-have.

    "Planetary  Programs  and  Tables  from  -4000  to  +2800"  by  Pierre
    Bretagnon  and Jean-Louis Simon, Willmann-Bell, 1986.  If you want the
    utmost  (outside  of  JPL)  accuracy  for  the  planets,  it's   here.

    "Celestial BASIC" by Eric Burgess, Revised Edition, Sybex, 1985.  Very
    cookbook oriented, and many of the algorithms are hard to dig  out	of
    the turgid BASIC code, but you'll probably want it anyway.

    Many of these references can be obtained from Willmann-Bell, P.O.  Box
    35025,  Richmond,  VA 23235, USA.  Phone: (804) 320-7016.  In addition
    to their own publications, they stock most of the standard	references
    for mathematical and positional astronomy.

    This program was written by:

       John Walker
       Autodesk, Inc.
       2320 Marinship Way
       Sausalito, CA  94965
       (415) 332-2344 Ext. 829

       Usenet: {sun!well}!acad!kelvin

    This  program is in the public domain: "Do what thou wilt shall be the
    whole of the law".  I'd appreciate  receiving  any  bug  fixes  and/or
    enhancements,  which  I'll  incorporate  in  future  versions  of  the
    program.  Please leave the original attribution information intact	so
    that credit and blame may be properly apportioned.

   Revision history:

        1.0  11/5/87   First version.  Only displayed icon, no
                       open window information display.

        2.0  3/27/88   First posting to comp.sources.unix.

        2.1  6/16/88   Bug fix.  Table of phases didn't update
                       at the moment of the new moon.  Call on
                       phasehunt didn't convert civil Julian date
                       to astronomical Julian date.  Reported by
                       Dag Bruck (dag@control.lth.se).

        2.2  2/27/89   Michael McClary  (michael@xanadu.COM)
                       Added moon map, derived from the sun "fullmoon"
                       image file.  (It has not been checked for
                       rotation from the correct orientation.  Also, a
                       fixed icon doesn't model the librations of the
                       moon as viewed from the earth.) Also: tweaked
                       corners of icon.

        2.3  6/7/89    Bug fix.  Table of phases skipped the phases
                       for July 1989.  This occurred due to sloppy
                       maintenance of the synodic month index in the
                       interchange of information between phasehunt()
                       and meanphase().  I simplified and corrected
                       the handling of the month index as phasehunt()
                       steps along and removed unneeded code from
                       meanphase().  Reported by Bill Randle of
                       Tektronix, Inc. (billr@saab.CNA.TEK.COM).

Additional History:

	June 1988	Modified by Ron Hitchens to produce version 2.1
			modified icon generation to show surface texture
			 on visible moon face.  Eliminated "illegal" direct
			 modification of icon image memory.
			added a menu to allow switching in and out of
			 test mode, for entertainment value mostly.
			reworked timer behaviour so that process doesn't
			 wake up unnecessarily.
			trap sigwinch signals to notice more easily when the
			 tool opens and closes.
			modified layout of information in open window display
			 to reduce the amount of pixels modified in each
			 update.  Batched pixwin updates so that only one
			 screen rasterop per cycle is done.
			changed open window to display white-on-black for a
			 more aesthetic look, and to suggest the effect of
			 looking at the moon in the nighttime sky.
			setup default tool and canvas colors to be the same
			 as B&W monochrome, for those us lucky enough to have
			 color monitors and who have the default monochrome
			 colors set to something other than B&W (I like white
			 on dark blue myself)
			various code reformatting and pretty-printing to suit
			 my own coding taste (I got a bit carried away).
			code tweaking to make lint happy.
			returned my hacked version to John.

			Ron Hitchens
				ronbo@vixen.uucp
				...!uunet!cs.utexas.edu!vixen!ronbo
				hitchens@cs.utexas.edu
*/


#include <stdio.h>
#include <math.h>
#ifdef HAS_STRFTIME
# ifdef linux
# include <linux/time.h>
# else
# include <time.h>
# endif
#else
# include <sys/time.h>
#endif

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include "xv_ct.h"

/*
 * define standard B/W monochrome colors as defaults in case we're running
 * on a color system with the monochrome colors set differently
 */


/*  Astronomical constants  */

#define epoch	    2444238.5	   /* 1980 January 0.0 */
#define J1970       2440587.5      /* VAX clock Epoch 1970 Jan 1 (0h UT) */

/*  Constants defining the Sun's apparent orbit  */

#define elonge	    278.833540	   /* Ecliptic longitude of the Sun
				      at epoch 1980.0 */
#define elongp	    282.596403	   /* Ecliptic longitude of the Sun at
				      perigee */
#define eccent      0.016718       /* Eccentricity of Earth's orbit */
#define sunsmax     1.495985e8     /* Semi-major axis of Earth's orbit, km */
#define sunangsiz   0.533128       /* Sun's angular size, degrees, at
				      semi-major axis distance */

/*  Elements of the Moon's orbit, epoch 1980.0  */

#define mmlong      64.975464      /* Moon's mean lonigitude at the epoch */
#define mmlongp     349.383063	   /* Mean longitude of the perigee at the
				      epoch */
#define mlnode	    151.950429	   /* Mean longitude of the node at the
				      epoch */
#define minc        5.145396       /* Inclination of the Moon's orbit */
#define mecc        0.054900       /* Eccentricity of the Moon's orbit */
#define mangsiz     0.5181         /* Moon's angular size at distance a
				      from Earth */
#define msmax       384401.0       /* Semi-major axis of Moon's orbit in km */
#define mparallax   0.9507	   /* Parallax at distance a from Earth */
#define synmonth    29.53058868    /* Synodic month (new Moon to new Moon) */
#define lunatbase   2423436.0      /* Base date for E. W. Brown's numbered
				      series of lunations (1923 January 16) */

/*  Properties of the Earth  */

#define earthrad    6378.16	   /* Radius of Earth in kilometres */


#ifdef PI
#undef PI
#endif
#define PI 3.14159265358979323846  /* Assume not near black hole nor in
				      Tennessee */

/*  Handy mathematical functions  */

#define sgn(x) (((x) < 0) ? -1 : ((x) > 0 ? 1 : 0))	  /* Extract sign */
#ifdef abs
#undef abs
#endif
#define abs(x) ((x) < 0 ? (-(x)) : (x)) 		  /* Absolute val */
#define fixangle(a) ((a) - 360.0 * (floor((a) / 360.0)))  /* Fix angle	  */
#define torad(d) ((d) * (PI / 180.0))			  /* Deg->Rad	  */
#define todeg(d) ((d) * (180.0 / PI))			  /* Rad->Deg	  */
#define dsin(x) (sin(torad((x))))			  /* Sin from deg */
#define dcos(x) (cos(torad((x))))			  /* Cos from deg */


/*
 * Moon image.  This is a standard icon-sized picture of the moon's face.
 * The visible part, as calculated by the current time, is extracted from
 * this image to create the displayed image.
 */

#include "moon.xbm"

extern Xv_Font	font;			/* pointer to regular icon font */
extern Xv_Font	sfont;			/* pointer to tiny icon font */
extern Canvas	mcanvas;		/* handle for the canvas */
extern GC gcm;
extern XGCValues gc_val;
extern char *monthnames[];
extern unsigned long black_pix, white_pix;
static GC gcms;
static XGCValues mgc_val;
static Pixmap moon_mpr;
static Pixmap icon_mpr;			/* actual displayed pixrect */
static int	charhgt, charwid;	/* default std font height/width */
static int info_col;
#ifndef HAS_STRFTIME
static struct timezone tzp;
#endif

static char *labels [] = {
	"Moon phase:",
	"Age of moon:",
	"Moon's distance:",
	"Moon subtends:",
	"Last new moon:",
	"First quarter:",
	"Full moon:",
	"Last quarter:",
	"Next new moon:"
};

/*  Forward functions  */
#ifdef __STDC__
static double		jtime(struct tm *t), phase(double pdate, double *pphase, double *mage, double *dist, double *angdia, double *sudist, double *suangdia);
static void		phasehunt(double sdate, double *phases);
static void		drawmoon(double ph, Pixmap src_pr, Pixmap dst_pr), jyear(double td, int *yy, int *mm, int *dd);
void 			fmt_phase_time (double utime, char *buf);
void			paint_labels ();
#else
static double		jtime(), phase();
static void		phasehunt();
static void		drawmoon(), jyear();
void 			fmt_phase_time ();
void			paint_labels ();
#endif



/*
 * DRAWMOON  --  Construct icon for moon, given phase of moon.
 */

static void drawmoon (ph, src_pr, dst_pr)
double		ph;
Pixmap	src_pr, dst_pr;
{
	register int	i, lx, rx;
	register double	cp, xscale;
#define RADIUS		27.0
#define IRADIUS		27
#define OFFSET		28
#define CENTER		32

	/* Clear the destination pixrect to all one-bits (black) */

	XFillRectangle(mydisplay, dst_pr, gcms,
		0, 0,
		64, 64);

	xscale = cos (2 * PI * ph);
	/* set FillOpaqueStippled */
	/****
	mgc_val.fill_style = FillOpaqueStippled;
	mgc_val.function = GXcopy;
	XChangeGC(mydisplay, gcms, GCFunction|GCFillStyle, &mgc_val);
	XSetStipple(mydisplay, gcms, src_pr);
	****/
	XSetFunction(mydisplay, gcms, GXcopy);
	XSetForeground(mydisplay, gcms, white_pix);
	XSetBackground(mydisplay, gcms, black_pix);
	for (i = 0; i < IRADIUS; i++) {
		cp = RADIUS * cos (asin (i / RADIUS));
		if (ph < 0.5) {
			rx = CENTER + cp;
			lx = CENTER + xscale * cp;
		} else {
			lx = CENTER - cp;
			rx = CENTER - xscale * cp;
		}

		/*
		 * We now know the left and right endpoints of the scan line
		 * for this y coordinate.  We raster-op the corresponding
		 * scanlines from the source Pixmap to the destination
		 * Pixmap, offsetting to properly place it in the Pixmap and
		 * reflecting vertically.
		 */
		/*****
		XSetTSOrigin(mydisplay, gcms, lx, OFFSET + i);
		XFillRectangle(mydisplay, dst_pr, gcms,
			lx, OFFSET + i,
			(rx - lx) + 1, 1);
		XSetTSOrigin(mydisplay, gcm, lx, OFFSET - i);
		XFillRectangle(mydisplay, dst_pr, gcms,
			lx, OFFSET - i,
			(rx - lx) + 1, 1);
		****/
		XCopyPlane(mydisplay, src_pr, dst_pr, gcms,
			lx, OFFSET + i,
			(rx - lx) + 1, 1,
			lx, OFFSET + i, 1L);
		XCopyPlane(mydisplay, src_pr, dst_pr, gcms,
			lx, OFFSET - i,
			(rx - lx) + 1, 1,
			lx, OFFSET - i, 1L);
	}
}

#define prt(y) XDrawString(mydisplay, drawable, gcm, info_col,\
	charhgt*(y), tbuf, strlen(tbuf))
#define prtxy(x,y) XDrawString(mydisplay, drawable, gcm, charwid*(y+1),\
	charhgt*(x), tbuf, strlen(tbuf))
#define EPL(x) (x), (x) == 1 ? "" : "s"
#define APOS(x) (x + 11)

/*  MOON_DATA  -- print useful info about the moon */

void
moon_data(seconds)
long seconds;
{
	int		lunation;
	int		i;
	int		aom_d, aom_h, aom_m;
	double		jd, p, aom, cphase, cdist, cangdia, csund, csuang;
	double		phasar [5];
	char		tbuf[80];
	struct tm	*gm, *localtime();
#ifndef HAS_STRFTIME
	struct timeval tvp;
#endif
	int	txt_size;
	XFontStruct *small;

	moon_mpr = XCreatePixmapFromBitmapData(mydisplay, drawable,
		moon_bits,
		64, 64,
		1L, 0L, 1);
	icon_mpr = XCreatePixmap(mydisplay, drawable,
		64, 64,
		1);
	mgc_val = gc_val;
	mgc_val.function = GXset;
	mgc_val.foreground = black_pix;
	mgc_val.background = white_pix;
	small = (XFontStruct *)xv_get(sfont, FONT_INFO);
	mgc_val.font = small->fid;
	gcms = XCreateGC(mydisplay, icon_mpr,
		GCBackground|GCForeground|GCFunction|GCFont,
		&mgc_val);
	charwid = xv_get(font, FONT_DEFAULT_CHAR_WIDTH);
	charhgt = xv_get(font, FONT_DEFAULT_CHAR_HEIGHT);
	info_col = charwid * 20;
	jd = jtime ((gm = gmtime (&seconds)));
#ifndef HAS_STRFTIME
	gettimeofday(&tvp, &tzp); /* for timezone info */
#endif
	p = phase (jd, &cphase, &aom, &cdist, &cangdia, &csund, &csuang);
	aom_d = (int) aom;
	aom_h = (int) (24 * (aom - floor(aom)));
	aom_m = (int) (1440 * (aom - floor(aom))) % 60;

	drawmoon(p, moon_mpr, icon_mpr);

	if (aom_d == 0) {
		(void)sprintf(tbuf, "%dh %dm", aom_h, aom_m);
	} else {
		(void)sprintf(tbuf, "%dd %dh", aom_d, aom_h);
	}
	txt_size = XTextWidth(xfont, tbuf, strlen(tbuf));
	/* reverse fg and bg colors to get inverted text */
	XSetForeground(mydisplay, gcms, black_pix);
	XSetBackground(mydisplay, gcms, white_pix);
	XDrawString(mydisplay, icon_mpr, gcms,
		(64 - txt_size) / 2, 63,
		tbuf, strlen(tbuf));
	XFreeGC(mydisplay, gcms);

	/* Update textual information for open window */
	drawable = (Drawable)xv_get(canvas_paint_window(mcanvas), XV_XID);
	paint_labels();
	/* moon phase */
	(void)sprintf (tbuf, "%d%%   [0%% = New, 100%% = Full]  ",
		(int) (cphase * 100));
	prt(1);

	/* Information about the Moon */

	/* age of moon */
	(void)sprintf (tbuf, "%d day%s, %d hour%s, %d minute%s.       ",
		EPL(aom_d), EPL(aom_h), EPL(aom_m));
	prt(2);

	/* moon distance */
	(void)sprintf (tbuf, "%ld kilometres, %.1f Earth radii.  ",
		(long) cdist, cdist / earthrad);
	prt(3);

	/* moon subtends */
	(void)sprintf (tbuf, "%.4f degrees.       ", cangdia);
	prt(4);

	/* paint the moon's image in the upper right of the canvas */
	/* first change colors to white on black */
	XSetForeground(mydisplay, gcm, white_pix);
	XSetBackground(mydisplay, gcm, black_pix);
	XCopyPlane(mydisplay, icon_mpr, drawable, gcm, 0, 0,
		64, 64,
		60 * charwid, charhgt, 1L);
	/* restore previous foreground/background colors */
	XChangeGC(mydisplay, gcm, GCBackground|GCForeground, &gc_val);

	/*
	 * Calculate times of phases of this lunation.
	 */
	phasehunt(jd + 0.5, phasar);

	for (i = 0; i < 5; i++) {
		fmt_phase_time (phasar[i], tbuf);
		prt(APOS(i));
	}

	lunation = floor(((phasar[0] + 7) - lunatbase) / synmonth) + 1;
	(void)sprintf(tbuf, "Lunation %d ", lunation);
	prtxy(APOS(0), 49);			      
	(void)sprintf(tbuf, "Lunation %d ", lunation + 1);
	prtxy(APOS(4), 49);
}
#undef APOS


/*
 * FMT_PHASE_TIME -- Format the provided julian date into the provided buffer
 *		in local time format for screen display
 */

void
fmt_phase_time (utime, buf)
	double	utime;
	char	*buf;
{
	long	clock;
	struct tm	*ltm, *localtime();

	/* convert to seconds local time, so we can use localtime()
	   to handle dst calculations */
	clock = (long)((utime - J1970) * 24. * 3600.);
	ltm = localtime(&clock);
#ifdef HAS_STRFTIME
	(void)strftime(buf, 80, " %H:%M %Z %d %b %Y             ", ltm);
#else
	(void)sprintf (buf, " %02d:%02d %s %2d %s %d             ",
	ltm->tm_hour, ltm->tm_min, timezone(tzp.tz_minuteswest, ltm->tm_isdst), ltm->tm_mday, monthnames[ltm->tm_mon], ltm->tm_year+1900);
#endif
}


/*
 * PAINT_LABELS -- Draw the labels into the canvas (open) window and right
 *		justify them.  Done once at startup.  We paint the labels
 *		separately to minimize the amount of screen real-estate
 *		being modified on each update.
 */

void
paint_labels ()
{
	int	i, len;

	/* use standard size fonts */
	for (i = 0; i < 4; i++) {
		len = strlen(labels[i]);
		XDrawString(mydisplay, drawable, gcm,
			charwid * (17 - len),
			charhgt * (i + 1),
			labels[i], len);
	}
	for (i = 4; i < 9; i++) {
		len = strlen(labels[i]);
		XDrawString(mydisplay, drawable, gcm,
			charwid * (17 - len),
			charhgt * (i + 7),
			labels[i], len);
	}
}


/*
 * JDATE  --  Convert internal GMT date and time to Julian day
 *	       and fraction.
 */

static
long
jdate (t)
	struct tm	*t;
{
	long		c, m, y;

	y = t->tm_year + 1900;
	m = t->tm_mon + 1;
	if (m > 2) {
		m = m - 3;
	} else {
		m = m + 9;
		y--;
	}
	c = y / 100L;		   /* Compute century */
	y -= 100L * c;
	return (t->tm_mday + (c * 146097L) / 4 + (y * 1461L) / 4 +
	    (m * 153L + 2) / 5 + 1721119L);
}


/*
 * JTIME --    Convert internal GMT date and time to astronomical Julian
 *	       time (i.e. Julian date plus day fraction, expressed as
 *	       a double).
 */

static
double
jtime (t)
	struct tm *t;
{
	return (jdate (t) - 0.5) + 
	   (t->tm_sec + 60 * (t->tm_min + 60 * t->tm_hour)) / 86400.0;
}


/*
 * JYEAR  --  Convert Julian date to year, month, day, which are
 *	       returned via integer pointers to integers.  
 */

static
void
jyear (td, yy, mm, dd)
	double	td;
	int	*yy, *mm, *dd;
{
	double j, d, y, m;

	td += 0.5;				/* Astronomical to civil */
	j = floor(td);
	j = j - 1721119.0;
	y = floor(((4 * j) - 1) / 146097.0);
	j = (j * 4.0) - (1.0 + (146097.0 * y));
	d = floor(j / 4.0);
	j = floor(((4.0 * d) + 3.0) / 1461.0);
	d = ((4.0 * d) + 3.0) - (1461.0 * j);
	d = floor((d + 4.0) / 4.0);
	m = floor(((5.0 * d) - 3) / 153.0);
	d = (5.0 * d) - (3.0 + (153.0 * m));
	d = floor((d + 5.0) / 5.0);
	y = (100.0 * y) + j;
	if (m < 10.0)
		m = m + 3;
	else {
		m = m - 9;
		y = y + 1;
	}
	*yy = y;
	*mm = m;
	*dd = d;
}


/*
 * MEANPHASE  --  Calculates mean phase of the Moon for a given
 *		base date.  This argument K to this function is
 *		the precomputed synodic month index, given by:
 *
 *			K = (year - 1900) * 12.3685
 *
 *		where year is expressed as a year and fractional
 *		year.
 */

static
double
meanphase (sdate, k)
	double	sdate, k;
{
	double	t, t2, t3, nt1;

	/* Time in Julian centuries from 1900 January 0.5 */
	t = (sdate - 2415020.0) / 36525;
	t2 = t * t;		   /* Square for frequent use */
	t3 = t2 * t;		   /* Cube for frequent use */

	nt1 = 2415020.75933 + synmonth * k
		+ 0.0001178 * t2
		- 0.000000155 * t3
		+ 0.00033 * dsin(166.56 + 132.87 * t - 0.009173 * t2);

	return nt1;
}


/*
 * TRUEPHASE  --  Given a K value used to determine the
 *		mean phase of the new moon, and a phase
 *		selector (0.0, 0.25, 0.5, 0.75), obtain
 *		the true, corrected phase time.
 */

static
double
truephase(k, phase)
	double k, phase;
{
	double t, t2, t3, pt, m, mprime, f;
	int apcor = FALSE;

	k += phase;		   /* Add phase to new moon time */
	t = k / 1236.85;	   /* Time in Julian centuries from
				      1900 January 0.5 */
	t2 = t * t;		   /* Square for frequent use */
	t3 = t2 * t;		   /* Cube for frequent use */
	pt = 2415020.75933	   /* Mean time of phase */
	     + synmonth * k
	     + 0.0001178 * t2
	     - 0.000000155 * t3
	     + 0.00033 * dsin(166.56 + 132.87 * t - 0.009173 * t2);

        m = 359.2242               /* Sun's mean anomaly */
	    + 29.10535608 * k
	    - 0.0000333 * t2
	    - 0.00000347 * t3;
        mprime = 306.0253          /* Moon's mean anomaly */
	    + 385.81691806 * k
	    + 0.0107306 * t2
	    + 0.00001236 * t3;
        f = 21.2964                /* Moon's argument of latitude */
	    + 390.67050646 * k
	    - 0.0016528 * t2
	    - 0.00000239 * t3;
	if ((phase < 0.01) || (abs(phase - 0.5) < 0.01)) {

	   /* Corrections for New and Full Moon */

	   pt +=     (0.1734 - 0.000393 * t) * dsin(m)
		    + 0.0021 * dsin(2 * m)
		    - 0.4068 * dsin(mprime)
		    + 0.0161 * dsin(2 * mprime)
		    - 0.0004 * dsin(3 * mprime)
		    + 0.0104 * dsin(2 * f)
		    - 0.0051 * dsin(m + mprime)
		    - 0.0074 * dsin(m - mprime)
		    + 0.0004 * dsin(2 * f + m)
		    - 0.0004 * dsin(2 * f - m)
		    - 0.0006 * dsin(2 * f + mprime)
		    + 0.0010 * dsin(2 * f - mprime)
		    + 0.0005 * dsin(m + 2 * mprime);
	   apcor = TRUE;
	} else if ((abs(phase - 0.25) < 0.01 || (abs(phase - 0.75) < 0.01))) {
	   pt +=     (0.1721 - 0.0004 * t) * dsin(m)
		    + 0.0021 * dsin(2 * m)
		    - 0.6280 * dsin(mprime)
		    + 0.0089 * dsin(2 * mprime)
		    - 0.0004 * dsin(3 * mprime)
		    + 0.0079 * dsin(2 * f)
		    - 0.0119 * dsin(m + mprime)
		    - 0.0047 * dsin(m - mprime)
		    + 0.0003 * dsin(2 * f + m)
		    - 0.0004 * dsin(2 * f - m)
		    - 0.0006 * dsin(2 * f + mprime)
		    + 0.0021 * dsin(2 * f - mprime)
		    + 0.0003 * dsin(m + 2 * mprime)
		    + 0.0004 * dsin(m - 2 * mprime)
		    - 0.0003 * dsin(2 * m + mprime);
	   if (phase < 0.5)
	      /* First quarter correction */
	      pt += 0.0028 - 0.0004 * dcos(m) + 0.0003 * dcos(mprime);
	   else
	      /* Last quarter correction */
	      pt += -0.0028 + 0.0004 * dcos(m) - 0.0003 * dcos(mprime);
	   apcor = TRUE;
	}
	if (!apcor) {
           (void)fprintf (stderr,
		"TRUEPHASE called with invalid phase selector.\n");
	   abort();
	}
	return pt;
}


/*
 * PHASEHUNT  --  Find time of phases of the moon which surround
 *		the current date.  Five phases are found, starting
 *		and ending with the new moons which bound the
 *		current lunation.
 */

static
void
phasehunt (sdate, phases)
	double	sdate;
	double	phases [5];
{
	int	yy, mm, dd;
	double	adate, k1, k2, nt1, nt2;

	adate = sdate - 45;
	jyear(adate, &yy, &mm, &dd);
	k1 = floor((yy + ((mm - 1) * (1.0 / 12.0)) - 1900) * 12.3685);

	adate = nt1 = meanphase(adate, k1);
	while (TRUE) {
		adate += synmonth;
		k2 = k1 + 1;
		nt2 = meanphase(adate, k2);
		if (nt1 <= sdate && nt2 > sdate)
			break;
		nt1 = nt2;
		k1 = k2;
	}
	phases[0] = truephase(k1, 0.0);
	phases[1] = truephase(k1, 0.25);
	phases[2] = truephase(k1, 0.5);
	phases[3] = truephase(k1, 0.75);
	phases[4] = truephase(k2, 0.0);
}


/*
 * KEPLER  --	Solve the equation of Kepler.
 */

static
double
kepler(m, ecc)
	double m, ecc;
{
	double e, delta;
#define EPSILON 1E-6

	e = m = torad(m);
	do {
		delta = e - ecc * sin(e) - m;
		e -= delta / (1 - ecc * cos(e));
	} while (abs (delta) > EPSILON);
	return e;
}


/*
 * PHASE  --  Calculate phase of moon as a fraction:
 *
 *	The argument is the time for which the phase is requested,
 *	expressed as a Julian date and fraction.  Returns the terminator
 *	phase angle as a percentage of a full circle (i.e., 0 to 1),
 *	and stores into pointer arguments the illuminated fraction of
 *	the Moon's disc, the Moon's age in days and fraction, the
 *	distance of the Moon from the centre of the Earth, and the
 *	angular diameter subtended by the Moon as seen by an observer
 *	at the centre of the Earth.
 */

static
double
phase (pdate, pphase, mage, dist, angdia, sudist, suangdia)
	double	pdate;
	double	*pphase;		/* Illuminated fraction */
	double	*mage;			/* Age of moon in days */
	double	*dist;			/* Distance in kilometres */
	double	*angdia;		/* Angular diameter in degrees */
	double	*sudist;		/* Distance to Sun */
	double	*suangdia;		/* Sun's angular diameter */
{

	double	Day, N, M, Ec, Lambdasun, ml, MM, MN, Ev, Ae, A3, MmP,
		mEc, A4, lP, V, lPP, NP, y, x, Lambdamoon, BetaM,
		MoonAge, MoonPhase,
		MoonDist, MoonDFrac, MoonAng, MoonPar,
		F, SunDist, SunAng;

        /* Calculation of the Sun's position */

	Day = pdate - epoch;			/* Date within epoch */
	N = fixangle((360 / 365.2422) * Day);	/* Mean anomaly of the Sun */
	M = fixangle(N + elonge - elongp);	/* Convert from perigee
				 		co-ordinates to epoch 1980.0 */
	Ec = kepler(M, eccent);			/* Solve equation of Kepler */
	Ec = sqrt((1 + eccent) / (1 - eccent)) * tan(Ec / 2);
	Ec = 2 * todeg(atan(Ec));		/* True anomaly */
        Lambdasun = fixangle(Ec + elongp);	/* Sun's geocentric ecliptic
							longitude */
	/* Orbital distance factor */
	F = ((1 + eccent * cos(torad(Ec))) / (1 - eccent * eccent));
	SunDist = sunsmax / F;			/* Distance to Sun in km */
        SunAng = F * sunangsiz;		/* Sun's angular size in degrees */


        /* Calculation of the Moon's position */

        /* Moon's mean longitude */
	ml = fixangle(13.1763966 * Day + mmlong);

        /* Moon's mean anomaly */
	MM = fixangle(ml - 0.1114041 * Day - mmlongp);

        /* Moon's ascending node mean longitude */
	MN = fixangle(mlnode - 0.0529539 * Day);

	/* Evection */
	Ev = 1.2739 * sin(torad(2 * (ml - Lambdasun) - MM));

	/* Annual equation */
	Ae = 0.1858 * sin(torad(M));

	/* Correction term */
	A3 = 0.37 * sin(torad(M));

	/* Corrected anomaly */
	MmP = MM + Ev - Ae - A3;

	/* Correction for the equation of the centre */
	mEc = 6.2886 * sin(torad(MmP));

	/* Another correction term */
	A4 = 0.214 * sin(torad(2 * MmP));

	/* Corrected longitude */
	lP = ml + Ev + mEc - Ae + A4;

	/* Variation */
	V = 0.6583 * sin(torad(2 * (lP - Lambdasun)));

	/* True longitude */
	lPP = lP + V;

	/* Corrected longitude of the node */
	NP = MN - 0.16 * sin(torad(M));

	/* Y inclination coordinate */
	y = sin(torad(lPP - NP)) * cos(torad(minc));

	/* X inclination coordinate */
	x = cos(torad(lPP - NP));

	/* Ecliptic longitude */
	Lambdamoon = todeg(atan2(y, x));
	Lambdamoon += NP;

	/* Ecliptic latitude */
	BetaM = todeg(asin(sin(torad(lPP - NP)) * sin(torad(minc))));

	/* Calculation of the phase of the Moon */

	/* Age of the Moon in degrees */
	MoonAge = lPP - Lambdasun;

	/* Phase of the Moon */
	MoonPhase = (1 - cos(torad(MoonAge))) / 2;

	/* Calculate distance of moon from the centre of the Earth */

	MoonDist = (msmax * (1 - mecc * mecc)) /
	   (1 + mecc * cos(torad(MmP + mEc)));

        /* Calculate Moon's angular diameter */

	MoonDFrac = MoonDist / msmax;
	MoonAng = mangsiz / MoonDFrac;

        /* Calculate Moon's parallax */

	MoonPar = mparallax / MoonDFrac;

	*pphase = MoonPhase;
	*mage = synmonth * (fixangle(MoonAge) / 360.0);
	*dist = MoonDist;
	*angdia = MoonAng;
	*sudist = SunDist;
	*suangdia = SunAng;
	return fixangle(MoonAge) / 360.0;
}
#endif  /* NO_SUN_MOON */
