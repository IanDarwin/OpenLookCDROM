/*
	setutils.c - routines to allocate, manipulate, and return
		     information about sets. Autoscaling stuff.

	$Header: setutils.c,v 1.8 89/09/02 10:07:59 pturner Locked $
*/

#include <stdio.h>
#include <math.h>
#include "defines.h"
#include "objdefs.h"
#include "globals.h"

typedef struct {
    double *x;
    double *y;
    double xmin, xmax, ymin, ymax;
    int len, plotsym, linesym, color, active, errbar, errbarxy;
    char comments[80];
} plotarr;

extern plotarr *plots[MAXPLOT];

extern int defline;
static int nsets = 0;

extern plotstr pstr[];

static char buf[256];

char *calloc();

plotarr *allocplot()
{
    return ((plotarr *) calloc(1, sizeof(plotarr)));
}

initplots()
{
    int i;
    extern double *ax, *bx, *cx, *dx;	/* used in compute.c */
    static int goodcolors[15] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};

    if ((ax = (double *) calloc(maxarr, sizeof(double))) == NULL) {
	fprintf(stderr, "Insufficient memory to allocate for ax\n");
	exit(1);
    }
    if ((bx = (double *) calloc(maxarr, sizeof(double))) == NULL) {
	fprintf(stderr, "Insufficient memory to allocate for bx\n");
	exit(1);
    }
    if ((cx = (double *) calloc(maxarr, sizeof(double))) == NULL) {
	fprintf(stderr, "Insufficient memory to allocate for cx\n");
	exit(1);
    }
    if ((dx = (double *) calloc(maxarr, sizeof(double))) == NULL) {
	fprintf(stderr, "Insufficient memory to allocate for dx\n");
	exit(1);
    }
    for (i = 0; i < maxplot; i++) {
	if ((plots[i] = allocplot()) == NULL) {
	    fprintf(stderr, "Insufficient memory to allocate for plots\n");
	    exit(1);
	}
	if ((plots[i]->x = (double *) calloc(maxarr, sizeof(double))) == NULL ||
	(plots[i]->y = (double *) calloc(maxarr, sizeof(double))) == NULL) {
	    fprintf(stderr, "Insufficient memory to allocate for plots\n");
	    exit(1);
	}
	plots[i]->comments[0] = 0;
	plots[i]->xmin = 0.0;
	plots[i]->ymin = 0.0;
	plots[i]->xmax = 0.0;
	plots[i]->ymax = 0.0;
	plots[i]->plotsym = 0;
	plots[i]->linesym = defline;
	plots[i]->color = goodcolors[i];
	plots[i]->len = 0;
	plots[i]->active = FALSE;
	plots[i]->errbar = -1;
	plots[i]->errbarxy = 0;
    }
/*
	initialize strings - mainly need to do the size parameter
*/
    for (i = 0; i < MAXSTR; i++) {
	pstr[i].s[0] = 0;
	pstr[i].x = pstr[i].y = 0.0;
	pstr[i].size = 1.0;
	pstr[i].rot = 0;
	pstr[i].font = 2;
    }
}

int iserrbar(setno)
    int setno;
{
    return (plots[setno]->errbar);
}

seterrbarxy(setno,etype)
    int setno, etype;
{
    plots[setno]->errbarxy=etype;
}

getseterrbarxy(setno)
    int setno;
{
    return (plots[setno]->errbarxy);
}

makeseterrbar(setno, setto)
    int setno, setto;
{
    plots[setno]->errbar = setto;
    if (setto>=0) {
       sprintf(buf,"Error bar on set %d",setto);
       setcomment(setno,buf);
       update_status(setno);
    }
}

clearseterrbar(setno)
    int setno;
{
    if (plots[setno]->errbar>=-1) {
       sprintf(buf,"Was error bar on set %d",plots[setno]->errbar);
       setcomment(setno,buf);
       update_status(setno);
    }
    plots[setno]->errbar = -1;
}

setminmax(setno, x1, x2, y1, y2)
    int setno;
    double x1, x2, y1, y2;
{
    plots[setno]->xmin = x1;
    plots[setno]->xmax = x2;
    plots[setno]->ymin = y1;
    plots[setno]->ymax = y2;
}

getsetminmax(setno, x1, x2, y1, y2)
    int setno;
    double *x1, *x2, *y1, *y2;
{
    *x1 = plots[setno]->xmin;
    *x2 = plots[setno]->xmax;
    *y1 = plots[setno]->ymin;
    *y2 = plots[setno]->ymax;
}

fminmax(p, xmi, xma, ymi, yma)
    plotarr *p;
    double *xmi, *xma, *ymi, *yma;
{
    int i;

    *xmi = p->x[0];
    *xma = p->x[0];
    *ymi = p->y[0];
    *yma = p->y[0];
    for (i = 1; i < (p->len); i++) {
	if (p->x[i] < *xmi)
	    *xmi = p->x[i];
	if (p->x[i] > *xma)
	    *xma = p->x[i];
	if (p->y[i] < *ymi)
	    *ymi = p->y[i];
	if (p->y[i] > *yma)
	    *yma = p->y[i];
    }
}

updatesetminmax(i)
    int i;
{
    double x1, x2, y1, y2;

    fminmax(plots[i], &x1, &x2, &y1, &y2);
    plots[i]->xmin = x1;
    plots[i]->xmax = x2;
    plots[i]->ymin = y1;
    plots[i]->ymax = y2;
}

double *getx(i)
    int i;
{
    return plots[i]->x;
}

double *gety(i)
    int i;
{
    return plots[i]->y;
}

int getsetplotsym(i)
    int i;
{
    return plots[i]->plotsym;
}

int getsetlinesym(i)
    int i;
{
    return plots[i]->linesym;
}

int getsetcolor(i)
    int i;
{
    return plots[i]->color;
}

int getsetlength(setno)
    int setno;
{
    return plots[setno]->len;
}

char *getcomment(setno)
    int setno;
{
    return (char *) (plots[setno]->comments);
}

setplotsym(i, sym)
    int i, sym;
{
    plots[i]->plotsym = sym;
}

setlinesym(i, sym)
    int i, sym;
{
    plots[i]->linesym = sym;
}

setplotcolor(i, sym)
    int i, sym;
{
    plots[i]->color = sym;
}

setlength(i, length)
    int i, length;
{
    plots[i]->len = length;
}

setcomment(i, s)
    int i;
    char *s;
{
    strcpy(plots[i]->comments, s);
}

copyx(setfrom, setto)
    int setfrom, setto;
{
    int i, n;

    n = getsetlength(setfrom);
    for (i = 0; i < n; i++)
	plots[setto]->x[i] = plots[setfrom]->x[i];
}

copyy(setfrom, setto)
    int setfrom, setto;
{
    int i, n;

    n = getsetlength(setfrom);
    for (i = 0; i < n; i++)
	plots[setto]->y[i] = plots[setfrom]->y[i];
}

int nextset()
{
    int i;

    i = 0;
    for (i = 0; i < maxplot; i++) {
	if (!isactive(i)) {
	    return (i);
	}
    }
    errwin("Error - no sets available");
    return (-1);
}

killset(setno)
    int setno;
{
    if (isactive(setno)) {
	plots[setno]->active = FALSE;
	plots[setno]->len = 0;
	plots[setno]->xmin = 0.0;
	plots[setno]->xmax = 0.0;
	plots[setno]->ymin = 0.0;
	plots[setno]->ymax = 0.0;
	nsets--;
    }
}

activateset(setno)
    int setno;
{
    if (!(plots[setno]->active)) {
	plots[setno]->active = TRUE;
	nsets++;
    }
}

int activeset()
{
    int i;

    for (i = 0; i < maxplot; i++) {
	if (plots[i]->active)
	    return (1);
    }
    return (0);
}

int isactive(setno)
    int setno;
{
    if (0 <= setno && setno < maxplot)
	return (plots[setno]->active);
    return (0);
}

int legalset(setno)
    int setno;
{
    if (0 <= setno && setno < maxplot)
	return (1);
    return (0);
}

/*
	 the following routines determine default scaling and ticmarks
*/

defaultgraph()
{
    double xmax, xmin, ymax, ymin;
    double xgmax, xgmin, ygmax, ygmin;
    int i;

    xgmax = (MBIG);
    xgmin = BIG;
    ygmax = (MBIG);
    ygmin = BIG;
    for (i = 0; i < maxplot; i++) {
	if (isactive(i) && !(iserrbar(i) >= 0)) {
	    getsetminmax(i, &xmin, &xmax, &ymin, &ymax);
	    if (xmin < xgmin)
		xgmin = xmin;
	    if (xmax > xgmax)
		xgmax = xmax;
	    if (ymin < ygmin)
		ygmin = ymin;
	    if (ymax > ygmax)
		ygmax = ymax;
	}
    }
    if (xgmin != xgmax) {
	xg2 = xgmax;
	xg1 = xgmin;
    } else {
	if ((xgmin == 0.0) && (xgmax == 0.0)) {
	    xgmin = 1.0;
	}
	xg1 = xgmin - 0.1 * fabs(xgmin);
	xg2 = xgmin + 0.1 * fabs(xgmin);
    }
    if (ygmin != ygmax) {
	yg2 = ygmax;
	yg1 = ygmin;
    } else {
	if ((ygmin == 0.0) && (ygmax == 0.0)) {
	    ygmin = 1.0;
	}
	yg1 = ygmin - 0.1 * fabs(ygmin);
	yg2 = ygmin + 0.1 * fabs(ygmin);
    }
}

defaultx()
{
    int i;
    double xgmin, xgmax, xmax, xmin, tmp;

    xgmax = (MBIG);
    xgmin = BIG;
    for (i = 0; i < maxplot; i++) {
	if (isactive(i) && !(iserrbar(i) >= 0)) {
	    getsetminmax(i, &xmin, &xmax, &tmp, &tmp);
	    if (xmin < xgmin)
		xgmin = xmin;
	    if (xmax > xgmax)
		xgmax = xmax;
	}
    }
    if (xgmin != xgmax) {
	xg2 = xgmax;
	xg1 = xgmin;
    } else {
	if ((xgmin == 0.0) && (xgmax == 0.0)) {
	    xgmin = 1.0;
	}
	xg1 = xgmin - 0.1 * fabs(xgmin);
	xg2 = xgmin + 0.1 * fabs(xgmin);
    }
}

defaulty()
{
    int i;
    double ygmax, ygmin, ymin, ymax, tmp;

    ygmax = (MBIG);
    ygmin = BIG;
    for (i = 0; i < maxplot; i++) {
	if (isactive(i) && !(iserrbar(i) >= 0)) {
	    getsetminmax(i, &tmp, &tmp, &ymin, &ymax);
	    if (ymin < ygmin)
		ygmin = ymin;
	    if (ymax > ygmax)
		ygmax = ymax;
	}
    }
    if (ygmin != ygmax) {
	yg2 = ygmax;
	yg1 = ygmin;
    } else {
	if ((ygmin == 0.0) && (ygmax == 0.0)) {
	    ygmin = 1.0;
	}
	yg1 = ygmin - 0.1 * fabs(ygmin);
	yg2 = ygmin + 0.1 * fabs(ygmin);
    }
}

/*
 * label: test program to demonstrate nice graph axis labeling
 *
 * Paul Heckbert, 2 Dec 88
 */

double expt(), tic(), nicenum();

int ntic = 6;			/* desired number of tic marks */

ylabeldef(ymin, ymax)
    double ymin, ymax;
{
    int exp;
    double graphymin, graphymax, range, d;

    /* we expect ymin!=ymax */
    range = nicenum(ymax - ymin, 0);
    d = nicenum(range / (ntic - 1), 1);	/* tic mark spacing */
    graphymin = floor(ymin / d) * d;
    graphymax = ceil(ymax / d) * d;
    yg1 = graphymin;
    yg2 = graphymax;
    yt1 = d;
    yt2 = d * 0.5;
}

xlabeldef(xmin, xmax)
    double xmin, xmax;
{
    int exp;
    double graphxmin, graphxmax, range, d;

    /* we expect xmin!=xmax */
    range = nicenum(xmax - xmin, 0);
    d = nicenum(range / (ntic - 1), 1);	/* tic mark spacing */
    graphxmin = floor(xmin / d) * d;
    graphxmax = ceil(xmax / d) * d;
    xg1 = graphxmin;
    xg2 = graphxmax;
    xt1 = d;
    xt2 = d * 0.5;
}

/*
 * nicenum: find a "nice" number approximately equal to x
 * round if round=1, ceil if round=0
 */

static double nicenum(x, round)
    double x;
    int round;
{
    int exp;
    double f, y;

    exp = floor(log10(x));
    f = x / expt(10., exp);	/* fraction between 1 and 10 */
    if (round)
	if (f < 1.5)
	    y = 1.;
	else if (f < 3.)
	    y = 2.;
	else if (f < 7.)
	    y = 5.;
	else
	    y = 10.;
    else if (f <= 1.)
	y = 1.;
    else if (f <= 2.)
	y = 2.;
    else if (f <= 5.)
	y = 5.;
    else
	y = 10.;
    return y * expt(10., exp);
}

/*
 * expt(a,n)=a^n for integer n
 * roundoff errors in pow were causing problems, so I wrote my own
 */

double expt(a, n)
    double a;
    register int n;
{
    double x;

    x = 1.;
    if (n > 0)
	for (; n > 0; n--)
	    x *= a;
    else
	for (; n < 0; n++)
	    x /= a;
    return x;
}

defaultsetgraph(setno)
    int setno;
{
    double xmax, xmin, ymax, ymin;
    double xgmax, xgmin, ygmax, ygmin;

    xgmax = (MBIG);
    xgmin = BIG;
    ygmax = (MBIG);
    ygmin = BIG;
    getsetminmax(setno, &xmin, &xmax, &ymin, &ymax);
    if (xmin < xgmin)
	xgmin = xmin;
    if (xmax > xgmax)
	xgmax = xmax;
    if (ymin < ygmin)
	ygmin = ymin;
    if (ymax > ygmax)
	ygmax = ymax;
    if (xgmin != xgmax) {
	xg2 = xgmax;
	xg1 = xgmin;
    } else {
	if ((xgmin == 0.0) && (xgmax == 0.0)) {
	    xgmin = 1.0;
	}
	xg1 = xgmin - 0.1 * fabs(xgmin);
	xg2 = xgmin + 0.1 * fabs(xgmin);
    }
    if (ygmin != ygmax) {
	yg2 = ygmax;
	yg1 = ygmin;
    } else {
	if ((ygmin == 0.0) && (ygmax == 0.0)) {
	    ygmin = 1.0;
	}
	yg1 = ygmin - 0.1 * fabs(ygmin);
	yg2 = ygmin + 0.1 * fabs(ygmin);
    }
}

defaulttics(method)
    int method;
{
    double log10();
    int cx, cy;

    if (xg1 >= xg2 || yg1 >= yg2) {
	errwin("World coordinates not properly set.");
	return;
    }
    xt1 = (xg2 - xg1) / 2.0;
    xt2 = xt1 / 2.0;
    yt1 = (yg2 - yg1) / 2.0;
    yt2 = yt1 / 2.0;
    cx = (int) log10(xt1);
    cy = (int) log10(yt1);
    xform = ((cx < 0) ? -cx + 1 : 1);
    yform = ((cy < 0) ? -cy + 1 : 1);
    if (xform > 9) {
	xform = 2;
	fformx = 0;
    }
    if (yform > 9) {
	yform = 2;
	fformy = 0;
    }
    if (!method) {
	ylabeldef(yg1, yg2);
	xlabeldef(xg1, xg2);
    }
}

defaultxtics(method)
    int method;
{
    double log10();
    int cx;

    if (xg1 >= xg2) {
	errwin("World coordinates not properly set.");
	return;
    }
    xt1 = (xg2 - xg1) / 2.0;
    xt2 = xt1 / 2.0;
    cx = (int) log10(xt1);
    xform = ((cx < 0) ? -cx + 1 : 1);
    if (xform > 9) {
	xform = 2;
	fformx = 0;
    }
    if (!method)
	xlabeldef(xg1, xg2);
}

defaultytics(method)
    int method;
{
    double log10();
    int cy;

    if (yg1 >= yg2) {
	errwin("World coordinates not properly set.");
	return;
    }
    yt1 = (yg2 - yg1) / 2.0;
    yt2 = yt1 / 2.0;
    cy = (int) log10(yt1);
    yform = ((cy < 0) ? -cy + 1 : 1);
    if (yform > 9) {
	yform = 2;
	fformy = 0;
    }
    if (!method)
	ylabeldef(yg1, yg2);
}
