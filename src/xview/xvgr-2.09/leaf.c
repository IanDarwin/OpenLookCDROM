/* driver for Inteleaf ASCII document format
 * by Mike Putnam
 * putnam@nosc.mil
 *
 * based upon:
 *
 *      Framemaker driver by
 *      A. Snyder
 *      asnyder@artorg.hmc.psu.edu
 *
 *	postscript printer driver by
 *	Jim Hudgens
 *	hudgens@ray.met.fsu.edu
 *
 *
 * Notes:
 *
 *	Pattern fills on bar graphs are a bit flakey, sometimes they show
 *        sometimes not. Color fills are more dependable.
 *
 *      You can't change fonts within a string.
 *
 *      Center and right justification on lines of text rotated other
 *        than in 90 deg increments may not line up correctly.
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "externs.h"
#ifndef M_PI
#	define M_PI  3.14159265358979323846
#endif

extern char version[];
extern double charsize;
extern double devcharsize;
extern int ptofile;
extern char printstr[];
extern unsigned char red[], green[], blue[];


/*
 * printer control string
 */
#ifndef LEAF_PRSTR
char leaf_prstr[128] = "cat >acegr.leaf <";

#else
char leaf_prstr[128] = LEAF_PRSTR;

#endif

/* Assume a landscape-shaped area 5.5 x 4.25 " (half size).  Address in 0.001" increments */
#define LEAFXMIN 0
#define LEAFXMAX 10500
#define LEAFYMIN 0
#define LEAFYMAX 8000
#define DXLEAF 10500
#define DYLEAF 8000
#define CHARS 6.0
#define TICL 80

/* Alternative is a portrait-shaped area 5.5 x 6 " */
#define LEAFXMINP 0
#define LEAFXMAXP 8000
#define LEAFYMINP 0
#define LEAFYMAXP 10500
#define DXLEAFP 8000
#define DYLEAFP 10500
#define CHARSP 6.0
#define TICLP 80

#define MINCOLOR 0
#define MAXCOLOR 15
#define MINLINEWIDTH 1
#define MAXLINEWIDTH 9
#define MINLINESTYLE 0
#define MAXLINESTYLE 6
#define MINPATTERN 0
#define MAXPATTERN 15

#define LINEWIDTHINC 0.5

#define PORTRAIT 0
#define LANDSCAPE 1

static int leafxmin = LEAFXMIN;
static int leafxmax = LEAFXMAX;
static int leafymin = LEAFYMIN;
static int leafymax = LEAFYMAX;
static int leafdx = DXLEAF;
static int leafdy = DYLEAF;
static int leafcolor = 0;
static int leaflinewidth = -1;
static int leafdmode;
static int leafpattern = 5;
static int leaffont = 0;
static double leafcharsize = 1.5;
static int leafticl;
static int leaflinestyle;
static char *fname;
static int orientflag = PORTRAIT;
static int styles[5] = {0, 5, 4, 1, 2};
static int pattern[16] = {5, 10, 9, 8, 3, 2, 1, 4, 5, 11, 10, 9, 8, 3, 2, 1};

struct ileaffonts {
    char fontnames[14];
};
static struct ileaffonts fontlist[] = {
    {"wst:timsps\0"},
    {"wst:timsps\0"},
    {"wst:timsps\0"},
    {"wst:timsps\0"},
    {"wst:helvps\0"},
    {"wst:helvps\0"},
    {"wst:helvps\0"},
    {"wst:helvps\0"},
    {"grk:dutchbs\0"},
    {"sym:clas\0"},
    {"sym:clas\0"}
};
static struct ileaffonts fonttypelist[] = {
    {" \0"},
    {"b\0"},
    {"i\0"},
    {"bi\0"},
    {" \0"},
    {"b\0"},
    {"i\0"},
    {"bi\0"},
    {" \0"},
    {" \0"},
    {" \0"}
};

#define MAXPATHLEN 1000		/* MAXPATHLEN points in a path between
				 * strokes */
static int xpoints[MAXPATHLEN], ypoints[MAXPATHLEN];
static int pathlength = 0;
static int x_current = 99999, y_current = 99999;
double xconv(), yconv();
static FILE *leafout;

static void stroke()
{
    int i;


    if (pathlength > 1) {
	fprintf(leafout, "(p8,1,0,,%d,%d,127\n", leafpattern, leafcolor);
	fprintf(leafout, "  (g9,1,0\n");
	fprintf(leafout, "    (g9,1,0\n");
	for (i = 1; i < pathlength; i++) {
	    fprintf(leafout, "      (v7,1,0,%6.3f,%6.3f,%6.3f,%6.3f,%d,0,%d,%d)\n",
		    (double) xpoints[i - 1] * 0.001, (double) (leafymax - ypoints[i - 1]) * 0.001,
		    (double) xpoints[i] * 0.001, (double) (leafymax - ypoints[i]) * 0.001,
		    leafcolor, leaflinewidth, styles[leaflinestyle - 1]);
	}			/* for i */
	fprintf(leafout, ")))\n");
	xpoints[0] = xpoints[--pathlength];
	ypoints[0] = ypoints[pathlength];
	pathlength = 1;

    }				/* if pathlength */
}

void otherdefs()
{
    int i;

    fprintf(leafout, "<!Color Definitions,\n");
    for (i = 0; i <= MAXCOLOR; i++) {
	fprintf(leafout, "C%d =  %5.1f, %5.1f, %5.1f, 0.0,\n",
		i, (255 - red[i]) * 0.3922,
		(255 - green[i]) * 0.3922,
		(255 - blue[i]) * 0.3922);
    }				/* for ncolors */
    fprintf(leafout, ">\n");
    fprintf(leafout, "\n");
    fprintf(leafout, "\n");
    fprintf(leafout, "<\"para\">\n");
    fprintf(leafout, "<Frame,\n");
    fprintf(leafout, "	Placement =		Overlay,\n");
}

int leafsetmode(mode)
    int mode;
{
    static char tbuf[128];
    char sysbuf[128];
    char *mktemp();

    if (mode % 2) {
	if (!ptofile) {
	    strcpy(tbuf, "/tmp/ACEgrXXXXXX");
	    fname = mktemp(tbuf);
	} else {
	    fname = printstr;
	}
	if ((leafout = fopen(fname, "w")) == NULL) {
	    return 0;
	}
    }
    devoffsx = devoffsy = 0;
    switch (mode) {
    case 1:			/* Interleaf landscape */
	orientflag = LANDSCAPE;
	leafcharsize = CHARS;
	leafxmin = LEAFXMIN;
	leafxmax = LEAFXMAX;
	leafymin = LEAFYMIN;
	leafymax = LEAFYMAX;
	leafdx = DXLEAF;
	leafdy = DYLEAF;
	devwidth = DXLEAF;
	devwidthmm = (int) (DXLEAF / 1000.0 * 25.4);
	devheight = DYLEAF;
	devheightmm = (int) (DYLEAF / 1000.0 * 25.4);
	leafticl = TICL;
	fprintf(leafout, "<!OPS, Version = 8.0>\n");
	fprintf(leafout, "<!Class, \"para\">\n");
	fprintf(leafout, "<!Page,\n");
	fprintf(leafout, "  Height = 8.5 Inches,\n");
	fprintf(leafout, "  Width = 11.0 Inches>\n");
	otherdefs();
	fprintf(leafout, "	Width =			11.0 Inches,\n");
	fprintf(leafout, "	Height =		8.5 Inches,\n");
	fprintf(leafout, "	Diagram =\n");
	fprintf(leafout, "V11,\n");
	fprintf(leafout, "(g9,1,0,\n");
	fprintf(leafout, "(g9,1,0,\n");
	break;
    case 3:			/* Interleaf portrait */
	orientflag = PORTRAIT;
	leafcharsize = CHARSP;
	leafxmin = LEAFXMINP;
	leafxmax = LEAFXMAXP;
	leafymin = LEAFYMINP;
	leafymax = LEAFYMAXP;
	leafdx = DXLEAFP;
	leafdy = DYLEAFP;
	devwidth = DXLEAFP;
	devwidthmm = (int) (DXLEAFP / 1000.0 * 25.4);
	devheight = DYLEAFP;
	devheightmm = (int) (DYLEAFP / 1000.0 * 25.4);
	leafticl = TICLP;
	fprintf(leafout, "<!OPS, Version = 8.0>\n");
	fprintf(leafout, "<!Class, \"para\">\n");
	fprintf(leafout, "<!Page,\n");
	fprintf(leafout, "  Height = 11.0 Inches,\n");
	fprintf(leafout, "  Width = 8.5 Inches>\n");
	otherdefs();
	fprintf(leafout, "	Width =			8.50 Inches,\n");
	fprintf(leafout, "	Height =		11.0 Inches,\n");
	fprintf(leafout, "	Diagram =\n");
	fprintf(leafout, "V11,\n");
	fprintf(leafout, "(g9,1,0,\n");
	fprintf(leafout, "(g9,1,0,\n");
	break;
    case 2:
    case 4:
	stroke();
	fprintf(leafout, "))>\n");
	fclose(leafout);
	if (!ptofile) {
	    sprintf(sysbuf, "%s %s", leaf_prstr, fname);
	    system(sysbuf);
	    unlink(fname);
	}
	orientflag = PORTRAIT;
	break;
    }
    return mode;
}

void drawleaf(x2, y2, mode)
    int x2, y2, mode;
{
    register int xtmp, ytmp;

    if (x2 < 0 || y2 < 0)	/* Eliminate garbage on output */
	return;

    xtmp = x2;
    ytmp = y2;

    if (mode) {			/* draw */
	if (pathlength == MAXPATHLEN) {
	    stroke();
	    xpoints[0] = xpoints[MAXPATHLEN - 1];
	    ypoints[0] = ypoints[MAXPATHLEN - 1];
	}
    } else {			/* moveto */
	/* Avoid excessive moveto's generated by grtool */
	if (xtmp == x_current && ytmp == y_current)
	    return;
	stroke();
	pathlength = 0;
    }
    xpoints[pathlength] = xtmp;
    ypoints[pathlength++] = ytmp;
    x_current = xtmp;
    y_current = ytmp;
}

int xconvleaf(x)
    double x;
{
    return ((int) (leafxmin + leafdx * xconv(x)));
}

int yconvleaf(y)
    double y;
{
    return ((int) (leafymin + leafdy * yconv(y)));
}

int leafsetcolor(c)
    int c;
{
    if (c != leafcolor) {
	stroke();
	if ((leafcolor = c) > MAXCOLOR) {
	    leafcolor = 1;
	} else if (leafcolor < MINCOLOR) {
	    leafcolor = 1;
	}
    }
    return c;
}

int leafsetlinewidth(c)
    int c;
{
    if (c != leaflinewidth) {
	stroke();
	if ((c = c % MAXLINEWIDTH) < MINLINEWIDTH)
	    c = MINLINEWIDTH;
    }
    leaflinewidth = c;
    return c;
}

void leafdrawtic(x, y, dir, updown)
    int x, y, dir, updown;
{
    switch (dir) {
    case 0:
	switch (updown) {
	case 0:
	    drawleaf(x, y, 0);
	    drawleaf(x, y + devxticl, 1);
	    break;
	case 1:
	    drawleaf(x, y, 0);
	    drawleaf(x, y - devxticl, 1);
	    break;
	}
	break;
    case 1:
	switch (updown) {
	case 0:
	    drawleaf(x, y, 0);
	    drawleaf(x + devyticl, y, 1);
	    break;
	case 1:
	    drawleaf(x, y, 0);
	    drawleaf(x - devyticl, y, 1);
	    break;
	}
	break;
    }
}

int leafsetlinestyle(style)
    int style;
{
    if (style == leaflinestyle) {
	return (leaflinestyle);
    }
    stroke();
    if ((leaflinestyle = style) < MINLINESTYLE)
	leaflinestyle = MINLINESTYLE;
    else if (leaflinestyle > MAXLINESTYLE)
	leaflinestyle = MAXLINESTYLE;
    return (leaflinestyle = style);
}

char leafcurfont[128];
static int leaffontsize = 15;

void leafsetfont(n)
    int n;
{
    leaffont = n;
}

void leafsetfontsize(size)
    double size;
{
    leaffontsize = (int) (size * 15);
}

void dispstrleaf(x, y, rot, s, just, fudge)
    int x, y, rot, just, fudge;
    char *s;
{
    int ilenx, ileny, oldx, oldy;
    double si, co, scale, rotrad;
    static int leafjust[] = {0, 2, 1};

    rotrad = M_PI / 180.0 * rot;
    si = sin(rotrad);
    co = cos(rotrad);

    scale = leaffontsize / 2.0;

    ilenx = stringextentx(scale, s);
    ileny = 0;
    oldx = x;
    oldy = y;
    switch (just) {
    case 1:
	x = x - co * ilenx;
	y = y - si * ilenx;
	break;
    case 2:
	x = x - (co * ilenx) / 2;
	y = y - (si * ilenx) / 2;
	break;
    }
    if (strlen(s) > 0) {
	if ((rot == 0) || (rot == 90) || (rot == 180) || (rot == 270)) {
	    if (fudge) {
		fprintf(leafout, "   (t14,1,0,%lf6.3,%lf6.3,%d,%d,0,%d,,%s%d%s,",
			oldx * 0.001, (leafymax - (oldy - leaffontsize / 216.0 * 1000.0)) * 0.001,
			leafjust[just], leafcolor, -rot, &fontlist[leaffont], leaffontsize, &fonttypelist[leaffont]);
	    } else {
		fprintf(leafout, "   (t14,1,0,%lf6.3,%lf6.3,%d,%d,0,%d,,%s%d%s,",
			oldx * 0.001, (leafymax - oldy) * 0.001,
			leafjust[just], leafcolor, -rot, &fontlist[leaffont], leaffontsize, &fonttypelist[leaffont]);
	    }
	}
	 /* if rotrad */ 
	else {
	    if (fudge) {
		fprintf(leafout, "   (o4,1,0,%lf6.3,%lf6.3,%lf6.3,%s%d%s,",
			x * 0.001, (leafymax - (y - leaffontsize / 216.0 * 1000.0)) * 0.001, -rotrad,
		&fontlist[leaffont], leaffontsize, &fonttypelist[leaffont]);
	    } else {
		fprintf(leafout, "   (o4,1,0,%lf6.3,%lf6.3,%lf6.3,%s%d%s,",
			x * 0.001, (leafymax - y) * 0.001, -rotrad,
		&fontlist[leaffont], leaffontsize, &fonttypelist[leaffont]);
	    }
	}			/* else rotrad */
	putleaf(s);
    }
}

putleaf(s)
    char *s;
{
    int i, cf, slen = strlen(s), curcnt = 0;
    int underline = 0, offset = 0;
    double saves = leaffontsize / 15.0, scale = leaffontsize / 15.0;
    char curstr[256];

    for (i = 0; i < slen; i++) {
	if (isoneof(s[i], "<>(), ")) {
	    fprintf(leafout, "\\%c", s[i]);
	} else if (s[i] == '\\') {
	    fprintf(leafout, "\\\\");
	} else {
	    fprintf(leafout, "%c", s[i]);
	};
    }				/* for s[i] */
    fprintf(leafout, ")\n");
    leafsetfontsize(saves);
}


int leafsetpat(k)
    int k;
{
    if (k > 15) {
	k = 15;
    } else if (k < 0) {
	k = 0;
	stroke();
    }
    return (leafpattern = pattern[k]);
}

void leaffill(n, px, py)
    int n;
    int px[], py[];

{
    int i;

    stroke();
    if (n) {
	fprintf(leafout, "(p8,1,0,,%d,%d,0\n", leafpattern, leafcolor);
	fprintf(leafout, "  (g9,1,0\n");
	fprintf(leafout, "    (g9,1,0\n");
	for (i = 1; i < n; i++) {
	    fprintf(leafout, "      (v7,1,0,%6.3f,%6.3f,%6.3f,%6.3f,7,127,%d,%d)\n",
		    (double) px[i - 1] * 0.001, (double) (leafymax - py[i - 1]) * 0.001,
		(double) px[i] * 0.001, (double) (leafymax - py[i]) * 0.001,
		    leaflinewidth, styles[leaflinestyle - 1]);
	}			/* for i */
	fprintf(leafout, ")))\n");
	pathlength = 0;

    }				/* if n */
}

void leaffillcolor(n, px, py)
    int n;
    int px[], py[];

{
    int i;

    stroke();
    if (n) {
	fprintf(leafout, "(p8,1,0,,%d,%d,0\n", 5, leafcolor);
	fprintf(leafout, "(g9,1,0\n");
	fprintf(leafout, "  (g9,1,0\n");
	for (i = 1; i < n; i++) {
	    fprintf(leafout, "    (v7,1,0,%6.3f,%6.3f,%6.3f,%6.3f,7,127,%d,%d)\n",
		    (double) px[i - 1] * 0.001, (double) (leafymax - py[i - 1]) * 0.001,
		(double) px[i] * 0.001, (double) (leafymax - py[i]) * 0.001,
		    leaflinewidth, styles[leaflinestyle - 1]);
	}			/* for i */
	fprintf(leafout, ")))\n");
	pathlength = 0;
    }				/* if n */
}


void leafdrawarc(x, y, r)
    int x, y, r;
{
    int i;

    stroke();
    fprintf(leafout, "(e8,1,0,\n");
    fprintf(leafout, " %6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,\n",
	    (x - r) * 0.001, (leafymax - y - r) * 0.001,
	    (x + r) * 0.001, (leafymax - y - r) * 0.001,
	    (x - r) * 0.001, (leafymax - y + r) * 0.001);
    fprintf(leafout, "  %d,127,%d,%d,0,%d,%d)\n", leafcolor, leafpattern, leafcolor, leaflinewidth, styles[leaflinestyle - 1]);
    pathlength = 0;

}

void leaffillarc(x, y, r)
    int x, y, r;
{
    int i;

    stroke();
    fprintf(leafout, "(e8,1,0,\n");
    fprintf(leafout, " %6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,\n",
	    (x - r) * 0.001, (leafymax - y - r) * 0.001,
	    (x + r) * 0.001, (leafymax - y - r) * 0.001,
	    (x - r) * 0.001, (leafymax - y + r) * 0.001);
    fprintf(leafout, "  %d,0,%d,%d,0,%d,%d)\n", leafcolor, leafpattern, leafcolor, leaflinewidth, styles[leaflinestyle - 1]);

    pathlength = 0;
}

void leafdrawellipse(x, y, xm, ym)
    int x, y, xm, ym;
{

    stroke();
    fprintf(leafout, "(e8,1,0,\n");
    fprintf(leafout, " %6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,\n",
	    (x - xm) * 0.001, (leafymax - y - ym) * 0.001,
	    (x + xm) * 0.001, (leafymax - y - ym) * 0.001,
	    (x - xm) * 0.001, (leafymax - y + ym) * 0.001);
    fprintf(leafout, "  %d,127,%d,%d,0,%d,%d)\n", leafcolor, leafpattern, leafcolor, leaflinewidth, styles[leaflinestyle - 1]);
    pathlength = 0;
}

void leaffillellipse(x, y, xm, ym)
    int x, y, xm, ym;
{
    stroke();
    fprintf(leafout, "(e8,1,0,\n");
    fprintf(leafout, " %6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,\n",
	    (x - xm) * 0.001, (leafymax - y - xm) * 0.001,
	    (x + xm) * 0.001, (leafymax - y - xm) * 0.001,
	    (x - xm) * 0.001, (leafymax - y + xm) * 0.001);
    fprintf(leafout, "  %d,0,%d,%d,0,%d,%d)\n", leafcolor, leafpattern, leafcolor, leaflinewidth, styles[leaflinestyle - 1]);
    pathlength = 0;
}

void leafleavegraphics()
{
    leafsetmode(leafdmode + 1);
}

/*           leaf initialization routine  */
int leafinitgraphics(dmode)
    int dmode;
{
    leafdmode = dmode;
    if (!leafsetmode(leafdmode)) {
	return -1;
    }
    devconvx = xconvleaf;
    devconvy = yconvleaf;
    vector = drawleaf;
    devwritestr = dispstrleaf;
    devsetcolor = leafsetcolor;
    devsetfont = leafsetfont;
    devsetline = leafsetlinestyle;
    devsetlinew = leafsetlinewidth;
    devdrawtic = leafdrawtic;
    devsetpat = leafsetpat;
    devdrawarc = leafdrawarc;
    devfillarc = leaffillarc;
    devdrawellipse = leafdrawellipse;
    devfillellipse = leaffillellipse;
    devfill = leaffill;
    devfillcolor = leaffillcolor;
    devleavegraphics = leafleavegraphics;
    devcharsize = leafcharsize;
    devxticl = leafticl;
    devyticl = leafticl;
    devsymsize = leafticl;
    devarrowlength = 80;
    leafsetcolor(1);
    leafsetlinewidth(2);
    setlinestyle(0);
    setfont(2);

    return (0);
}
