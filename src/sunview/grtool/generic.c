
/*
  generic driver

	$Header: generic.c,v 1.2 89/08/19 09:24:00 pturner Locked $
*/

#include <stdio.h>
#include "externs.h"

extern int gflag;		/* if true then use gfile as the output file */
extern char gfile[];		/* write to this file rather than prompt */

#define GENXMIN 0
#define GENXMAX 10170
#define GENYMIN 0
#define GENYMAX 7840
#define DXGEN 10170
#define DYGEN 7840
#define MINCOLOR 0
#define MAXCOLOR 9
#define MAXLINESTYLE 9
static double xmin, ymin;
static double xmax, ymax;
static int gencolor;
static int gendmode;
static int genfont = 0;
static int genlinestyle;
static double gencharsize = 6.0;
extern double devcharsize;

double xconv(), yconv();

static FILE *genout;

putstrgen(s)
    char *s;
{
    fprintf(genout, s);
}

static char outf[80];

gensetmode(mode)
    int mode;
{
    char tbuf[128];
    char *mktemp();

    switch (mode) {
    case 1:
	if (gflag) {
	    strcpy(outf, gfile);
	    gflag = 0;
	} else {
	    printf("Enter file name for output : ");
	    scanf("%s", outf);
	}
	if ((genout = fopen(outf, "w")) == NULL)
	    return;
	putstrgen("                                                                                                             \n");
	xmin = 1000000.0;
	ymin = 1000000.0;
	xmax = (-1000000.0);
	ymax = (-1000000.0);
	break;
    case 2:
	sprintf(tbuf, "# Display Manager graphics file\nW2 %lf %lf %lf %lf\n", xmin, ymin, xmax, ymax);
	putstrgen("# End graphics\n");
	rewind(genout);
	putstrgen(tbuf);
	fclose(genout);
	break;
    }
}

static int x1 = 99999, y1 = 99999;

void drawgen(x2, y2, mode)
    int x2, y2, mode;
{
    char stmp[30];
    double xtmp = x2 / 10170.0;
    double ytmp = y2 / 7840.0;

    if (mode) {
	if (xtmp < xmin)
	    xmin = xtmp;
	if (ytmp < ymin)
	    ymin = ytmp;
	if (xtmp > xmax)
	    xmax = xtmp;
	if (ytmp > ymax)
	    ymax = ytmp;
	sprintf(stmp, "L %lf %lf\n", x2 / 10170.0, y2 / 7840.0);
	putstrgen(stmp);
    } else {
	if (!(x1 == x2 && y1 == y2)) {
	    sprintf(stmp, "M %lf %lf\n", x2 / 10170.0, y2 / 7840.0);
	    putstrgen(stmp);
	}
    }
    x1 = x2;
    y1 = y2;
}

int xconvgen(x)
    double x;
{
    return ((int) (DXGEN * xconv(x)));
}

int yconvgen(y)
    double y;
{
    return ((int) (DYGEN * yconv(y)));
}

void gensetfont(n)
    int n;
{
    hselectfont(genfont = n);
}

int gensetcolor(c)
    int c;
{
    if (c) {
	char stmp[10];

	c = c % MAXCOLOR;
	sprintf(stmp, "P %d\n", c);
	putstrgen(stmp);
    }
    gencolor = c;
    return c;
}

void gendrawtic(x, y, dir, updown)
    int x, y, dir, updown;
{
    switch (dir) {
    case 0:
	switch (updown) {
	case 0:
	    drawgen(x, y, 0);
	    drawgen(x, y + devxticl, 1);
	    break;
	case 1:
	    drawgen(x, y, 0);
	    drawgen(x, y - devxticl, 1);
	    break;
	}
	break;
    case 1:
	switch (updown) {
	case 0:
	    drawgen(x, y, 0);
	    drawgen(x + devyticl, y, 1);
	    break;
	case 1:
	    drawgen(x, y, 0);
	    drawgen(x - devyticl, y, 1);
	    break;
	}
	break;
    }
}

int gensetlinestyle(style)
    int style;
{
    style--;
    if (style >= 0) {
	char stmp[10];

	style = style % MAXLINESTYLE;
	if (style == 0)
	    strcpy(stmp, "LS 0\n");
	else
	    sprintf(stmp, "LS %d\n", style);
	putstrgen(stmp);
    }
    return (genlinestyle = style);
}

void dispstrgen(x, y, rot, s)
    int x, y, rot;
    char *s;
{
    extern double charsize;

    puthersh(x, y, gencharsize * charsize, rot, gencolor, vector, s);
}

void genleavegraphics()
{
    gensetmode(2);
}

geninitgraphics(dmode)
    int dmode;
{
    gendmode = dmode;
    gensetmode(0x01);
    devconvx = xconvgen;
    devconvy = yconvgen;
    vector = drawgen;
    devwritestr = dispstrgen;
    devsetcolor = gensetcolor;
    devsetfont = gensetfont;
    devsetline = gensetlinestyle;
    devdrawtic = gendrawtic;
    devleavegraphics = genleavegraphics;
    devcharsize = gencharsize;
    devxticl = 50;
    devyticl = 50;
    setfont(1);
    setcolor(1);
    setlinestyle(0);
}
