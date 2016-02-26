
/*
  driver for an hp 7550

	$Header: hp.c,v 1.3 89/08/20 11:04:57 pturner Locked $
*/

#include <stdio.h>
#include "externs.h"

extern double charsize;
extern double devcharsize;

#define HPXMIN 0
#define HPXMAX 10170
#define HPYMIN 0
#define HPYMAX 7840
#define DXHP 10170
#define DYHP 7840

#define HP2XMIN 0
#define HP2XMAX 16450
#define HP2YMIN 0
#define HP2YMAX 10170
#define DXHP2 16450
#define DYHP2 10170

#define MINCOLOR 0
#define MAXCOLOR 9
#define MAXLINESTYLE 14

static int hpxmin = HPXMIN;
static int hpxmax = HPXMAX;
static int hpymin = HPYMIN;
static int hpymax = HPYMAX;
static int hpdx = DXHP;
static int hpdy = DYHP;

static int hpcolor;
static int hpdmode;
static int hpfont = 0;
static double hpcharsize = 6.1;
static int hplinestyle;

double xconv(), yconv();

static FILE *hpout;

void putstrhp(s)
    char *s;
{
    fprintf(hpout, s);
}

static char *fname;

static int orientflag = 0;

void hpsetmode(mode)
    int mode;
{
    char tbuf[128];
    char *mktemp();

    if (mode % 2) {
	fname = mktemp("/usr/tmp/XXXXXX");
	hpout = fopen(fname, "w");
	if ((mode == 5) || (mode == 7))
	    putstrhp("IN;DF;SP1;");
	else
	    putstrhp("PG;IN;DF;SP1;");
    }
    switch (mode) {
    case 3:			/* HP portrait */
    case 7:			/* PS portrait */
	orientflag = 1;
    case 1:			/* HP landscape */
    case 5:			/* PS landscape */
	hpxmin = HPXMIN;
	hpxmax = HPXMAX;
	hpymin = HPYMIN;
	hpymax = HPYMAX;
	hpdx = DXHP;
	hpdy = DYHP;
	break;
    case 11:			/* portrait */
	orientflag = 1;
    case 9:			/* HP landscape 11x17 */
	hpxmin = HP2XMIN;
	hpxmax = HP2XMAX;
	hpymin = HP2YMIN;
	hpymax = HP2YMAX;
	hpdx = DXHP2;
	hpdy = DYHP2;
	break;
    case 2:			/* HP landscape */
    case 4:			/* portrait */
    case 10:			/* HP landscape 11x17 */
    case 12:			/* portrait */
	putstrhp("PAPU0,0;SP 0;\n");
	fclose(hpout);
	sprintf(tbuf, "lpr -h -Php %s", fname);
	system(tbuf);
	unlink(fname);
	orientflag = 0;
	break;
    case 6:			/* HPGL to Laserjet series II landscape */
    case 8:			/* HPGL tp Laserjet Series II portrait */
	putstrhp("PAPU0,0;SP 0;\n");
	fclose(hpout);
	sprintf(tbuf, "lpr -h %s", fname);
	system(tbuf);
	unlink(fname);
	orientflag = 0;
	break;
    }
}

static int x1 = 99999, y1 = 99999;

void drawhp(x2, y2, mode)
    int x2, y2, mode;
{
    if (orientflag) {
	int xtmp, ytmp;
	char stmp[30];

	xtmp = y2;
	ytmp = (-x2) + hpymax;

	if (mode) {
	    sprintf(stmp, "PAPD%1d,%1d;", xtmp, ytmp);
	    putstrhp(stmp);
	} else {
	    if (!(x1 == xtmp && y1 == ytmp)) {
		sprintf(stmp, "PAPU%1d,%1d;", xtmp, ytmp);
		putstrhp(stmp);
	    }
	}
	x1 = xtmp;
	y1 = ytmp;
    } else {
	char stmp[30];

	if (mode) {
	    sprintf(stmp, "PAPD%1d,%1d;", x2, y2);
	    putstrhp(stmp);
	} else {
	    if (!(x1 == x2 && y1 == y2)) {
		sprintf(stmp, "PAPU%1d,%1d;", x2, y2);
		putstrhp(stmp);
	    }
	}
	x1 = x2;
	y1 = y2;
    }
}

int xconvhp(x)
    double x;
{
    if (orientflag) {
	return ((int) (hpdy * xconv(x)));
    } else {
	return ((int) (hpdx * xconv(x)));
    }
}

int yconvhp(y)
    double y;
{
    if (orientflag) {
	return ((int) (hpdx * yconv(y)));
    } else {
	return ((int) (hpdy * yconv(y)));
    }
}

void hpsetfont(n)
    int n;
{
    hselectfont(hpfont = n);
}

int hpsetcolor(c)
    int c;
{
    if (c) {
	char stmp[10];

	c = c % MAXCOLOR;
	sprintf(stmp, "SP%d;PU;", c);
	putstrhp(stmp);
    }
    hpcolor = c;
    return c;
}

void hpdrawtic(x, y, dir, updown)
    int x, y, dir, updown;
{
    switch (dir) {
    case 0:
	switch (updown) {
	case 0:
	    drawhp(x, y, 0);
	    drawhp(x, y + devxticl, 1);
	    break;
	case 1:
	    drawhp(x, y, 0);
	    drawhp(x, y - devxticl, 1);
	    break;
	}
	break;
    case 1:
	switch (updown) {
	case 0:
	    drawhp(x, y, 0);
	    drawhp(x + devyticl, y, 1);
	    break;
	case 1:
	    drawhp(x, y, 0);
	    drawhp(x - devyticl, y, 1);
	    break;
	}
	break;
    }
}

int hpsetlinestyle(style)
    int style;
{
    char stmp[20];

    switch (style) {
    case 1:
	strcpy(stmp, "LT;");
	break;
    case 2:
	strcpy(stmp, "LT2,1;");
	break;
    case 3:
	strcpy(stmp, "LT2,2;");
	break;
    case 4:
	strcpy(stmp, "LT2,3;");
	break;
    case 5:
	strcpy(stmp, "LT2,4;");
	break;
    case 6:
	strcpy(stmp, "LT4,2;");
	break;
    case 7:
	strcpy(stmp, "LT5,2;");
	break;
    case 8:
	strcpy(stmp, "LT6,2;");
	break;
    default:
	strcpy(stmp, "LT;");
	break;
    }
    putstrhp(stmp);
    return (hplinestyle = style);
}

void dispstrhp(x, y, rot, s)
    int x, y, rot;
    char *s;
{
    puthersh(x, y, hpcharsize * charsize, rot, hpcolor, vector, s);
}

void hpleavegraphics()
{
    hpsetmode(hpdmode + 1);
}

hpinitgraphics(dmode)
    int dmode;
{
    hpdmode = dmode;
    hpsetmode(hpdmode);
    devconvx = xconvhp;
    devconvy = yconvhp;
    vector = drawhp;
    devwritestr = dispstrhp;
    devsetcolor = hpsetcolor;
    devsetfont = hpsetfont;
    devsetline = hpsetlinestyle;
    devdrawtic = hpdrawtic;
    devleavegraphics = hpleavegraphics;
    devcharsize = hpcharsize;
    devxticl = 60;
    devyticl = 60;
    setfont(2);
    setcolor(1);
    setlinestyle(0);
}
