/*
        draw.c
        generic draw routines for gr running under UNIX
        for 4010 and varieties.
        pturner 12/87

        $Header: draw.c,v 1.10 89/09/07 22:16:13 pturner Locked $
*/

#include <stdio.h>
#include <math.h>

double charsize = 1.0;

static double xg1, xg2, yg1, yg2;	/* world coordinates */
static double dxg, dyg;		/* delta x, y of world co-ordinates */
static double xv1, xv2, yv1, yv2;	/* viewpoint coordinates */
static double dxv, dyv;		/* distance covered by viewport */
static double rx, ry;		/* dxv/dxg & dyv/dyg */
static double curx, cury;	/* current location of pen */
static int color;		/* current color */
static int lines;		/* current linestyle */
static int ylabpos;		/* info for positioning y-axis label */
static int devtype;		/* current device */
static int clipflag = 1;	/* clip in draw2 */
double devcharsize;		/* device charsize */

int (*devsetcolor) ();		/* device set color */
int (*devsetfont) ();		/* device set font */
int (*devsetline) ();		/* device set line style */
int (*devconvx) ();		/* viewport to device mapping for x */
int (*devconvy) ();		/* viewport to device mapping for y */
int (*vector) ();		/* device draw line */
int (*devwritestr) ();		/* device write string */
int (*devdrawtic) ();		/* device draw tic */
int (*devleavegraphics) ();	/* device exit */

int devxticl;			/* device tic length for x-axis */
int devyticl;			/* device tic length for y-axis */
int curfontd = 0;		/* current font */
static double hdelta;

int ylabdis;			/* hack to place y-axis labels correctly */

double xconv(), yconv();

get_world(x, y, wx, wy)
    int x, y;
    double *wx, *wy;
{
    extern int win_h, win_w;

    *wx = x / (win_w * rx) - xv1 / rx + xg1;
    *wy = (y - win_h) / (-win_h * ry) - yv1 / ry + yg1;
}

get_view(x, y, vx, vy)
    double x, y;
    double *vx, *vy;
{
    *vx = xconv(x);
    *vy = yconv(y);
}

get_device(wx, wy, x, y)
    double wx, wy;
    int *x, *y;
{
    extern int win_h, win_w;

    *x = (*devconvx) (wx);
    *y = win_h - (*devconvy) (wy);
}

double devtoworldx(fac)
    double fac;
{
    return (fac - xv1) / rx + xg1;
}

double devtoworldy(fac)
    double fac;
{
    return (fac - yv1) / ry + yg1;
}

/*
        map world co-ordinates to viewport
*/
double xconv(x)
    double x;
{
    return (rx * (x - xg1) + xv1);
}

double yconv(y)
    double y;
{
    return (ry * (y - yg1) + yv1);
}

defineworld(x1, y1, x2, y2)
    double x1, y1, x2, y2;
{
    xg1 = x1;
    xg2 = x2;
    yg1 = y1;
    yg2 = y2;
    dxg = x2 - x1;
    dyg = y2 - y1;
}

viewport(x1, y1, x2, y2)
    double x1, x2, y1, y2;
{
    xv1 = x1;
    xv2 = x2;
    yv1 = y1;
    yv2 = y2;
    dxv = x2 - x1;
    dyv = y2 - y1;
    rx = dxv / dxg;
    ry = dyv / dyg;
}

setclipping(fl)
{
    clipflag = fl;
}

/*
        clip lines within rectangle defined in world co-ordinates
*/
int region(xend, yend)
    double xend, yend;
{
    int endpoint;

    endpoint = 0;
    if (xend < xg1)
	endpoint = 1;		/* left */
    else {
	if (xend > xg2)
	    endpoint = 2;	/* right */
    }
    if (yend < yg1)
	endpoint |= 4;		/* bottom */
    else {
	if (yend > yg2)
	    endpoint |= 8;	/* top */
    }
    return (endpoint);
}

draw2(x2, y2)
    double x2, y2;
{
    double m, minverse, x, y, x1, y1, xtmp, ytmp;
    int dir, dir1, dir2, region();

    x1 = curx;
    y1 = cury;
    xtmp = x2;
    ytmp = y2;
    if (!clipflag) {
	move2(x1, y1);
	(*vector) ((*devconvx) (x2), (*devconvy) (y2), color);
	curx = xtmp;
	cury = ytmp;
	return;
    }
    dir1 = region(x1, y1);
    dir2 = region(x2, y2);
    if (x1 != x2)
	m = (y2 - y1) / (x2 - x1);
    if (y2 != y1)
	minverse = (x2 - x1) / (y2 - y1);
    while ((dir1 != 0) || (dir2 != 0)) {
	if (dir1 & dir2) {
	    curx = xtmp;
	    cury = ytmp;
	    return;
	}
	if (dir1 == 0) {
	    dir = dir2;
	    x = x2;
	    y = y2;
	} else {
	    dir = dir1;
	    x = x1;
	    y = y1;
	}
	if (1 & dir) {
	    y = m * (xg1 - x1) + y1;
	    x = xg1;
	} else {
	    if (2 & dir) {
		y = m * (xg2 - x1) + y1;
		x = xg2;
	    } else {
		if (4 & dir) {
		    x = minverse * (yg1 - y1) + x1;
		    y = yg1;
		} else {
		    if (8 & dir) {
			x = minverse * (yg2 - y1) + x1;
			y = yg2;
		    }
		}
	    }
	}
	if (dir == dir1) {
	    x1 = x;
	    y1 = y;
	    dir1 = region(x1, y1);
	} else {
	    x2 = x;
	    y2 = y;
	    dir2 = region(x2, y2);
	}
    }
    move2(x1, y1);
    (*vector) ((*devconvx) (x2), (*devconvy) (y2), color);
    curx = xtmp;
    cury = ytmp;
}

move2(x, y)
    double x, y;
{
    (*vector) ((*devconvx) (x), (*devconvy) (y), 0);
    curx = x;
    cury = y;
}

setfont(f)
    int f;
{
    (*devsetfont) (f);
    curfontd = f;
}

rect(x1, y1, x2, y2)
    double x1, x2, y1, y2;
{
    move2(x1, y1);
    draw2(x1, y2);
    draw2(x2, y2);
    draw2(x2, y1);
    draw2(x1, y1);
}

int symok(x, y)
    double x, y;
{
    return ((x >= xg1) && (x <= xg2) && (y >= yg1) && (y <= yg2));
}

histbox(x, y)
    double x, y;
{
    move2(x - hdelta, 0.0);
    draw2(x - hdelta, y);
    draw2(x + hdelta, y);
    draw2(x + hdelta, 0.0);
}

histbox2(x, y, y2, hflag)	/* h is flag set to true if this is the last
				 * point */
    double x, y, y2;
    int hflag;
{
    move2(x - hdelta, y);
    draw2(x + hdelta, y);
    if (!hflag)
	draw2(x + hdelta, y2);
}

drawsym(x, y, sym)
    double x, y;
    int sym;
{
    double ytmp;

    if (sym == 0)
	return;
    if (symok(x, y)) {
	if ((sym > 0) && (sym < 16))
	    hwritesym(sym, (*devconvx) (x), (*devconvy) (y), devcharsize, color, vector);
	else {
	    switch (sym) {
	    case 16:
		if (yg1 < 0.0 && yg2 > 0.0)
		    ytmp = 0.0;
		else
		    ytmp = yg1;
		move2(x, ytmp);
		draw2(x, y);
		break;
	    case 17:
		move2(x, yg1);
		draw2(x, yg2);
		break;
	    }
	}
    }
}

drawpoly(x, y, n)
    double x[], y[];
int n;

{
    int i;

    move2(x[0], y[0]);
    for (i = 1; i < n; i++) {
	draw2(x[i], y[i]);
    }
}

drawerrbar(x, y, n1, el, et, n2, etype, ebarlen)
    double x[], y[], el[], et[], ebarlen;
    int n1, n2, etype;

{
    int i, n, ebarleny, ebarlenx;

    n = n2;
    if (n1 < n2) {
	n = n1;
    }
    ebarleny = (int) (ebarlen * devyticl);
    ebarlenx = (int) (ebarlen * devxticl);
    for (i = 0; i < n; i++) {
	switch (etype) {
	case 0:
	    move2(x[i], y[i] - el[i]);
	    draw2(x[i], y[i] + et[i]);
	    if (symok(x[i], y[i] - el[i])) {
		(*vector) ((*devconvx) (x[i]) - ebarleny, (*devconvy) (y[i] - el[i]), 0);
		(*vector) ((*devconvx) (x[i]) + ebarleny, (*devconvy) (y[i] - el[i]), color);
	    }
	    if (symok(x[i], y[i] + et[i])) {
		(*vector) ((*devconvx) (x[i]) - ebarleny, (*devconvy) (y[i] + et[i]), 0);
		(*vector) ((*devconvx) (x[i]) + ebarleny, (*devconvy) (y[i] + et[i]), color);
	    }
	    break;
	case 2:
	    move2(x[i], y[i]);
	    draw2(x[i], y[i] + et[i]);
	    if (symok(x[i], y[i] + et[i])) {
		(*vector) ((*devconvx) (x[i]) - ebarleny, (*devconvy) (y[i] + et[i]), 0);
		(*vector) ((*devconvx) (x[i]) + ebarleny, (*devconvy) (y[i] + et[i]), color);
	    }
	    break;
	case 4:
	    move2(x[i], y[i] - el[i]);
	    draw2(x[i], y[i]);
	    if (symok(x[i], y[i] - el[i])) {
		(*vector) ((*devconvx) (x[i]) - ebarleny, (*devconvy) (y[i] - el[i]), 0);
		(*vector) ((*devconvx) (x[i]) + ebarleny, (*devconvy) (y[i] - el[i]), color);
	    }
	    break;
	case 1:
	    move2(x[i] - el[i], y[i]);
	    draw2(x[i] + et[i], y[i]);
	    if (symok(x[i] - el[i], y[i])) {
		(*vector) ((*devconvx) (x[i] - el[i]), (*devconvy) (y[i]) - ebarlenx, 0);
		(*vector) ((*devconvx) (x[i] - el[i]), (*devconvy) (y[i]) + ebarlenx, color);
	    }
	    if (symok(x[i] + et[i], y[i])) {
		(*vector) ((*devconvx) (x[i] + et[i]), (*devconvy) (y[i]) - ebarlenx, 0);
		(*vector) ((*devconvx) (x[i] + et[i]), (*devconvy) (y[i]) + ebarlenx, color);
	    }
	    break;
	case 3:
	    move2(x[i] - el[i], y[i]);
	    draw2(x[i], y[i]);
	    if (symok(x[i] - el[i], y[i])) {
		(*vector) ((*devconvx) (x[i] - el[i]), (*devconvy) (y[i]) - ebarlenx, 0);
		(*vector) ((*devconvx) (x[i] - el[i]), (*devconvy) (y[i]) + ebarlenx, color);
	    }
	    break;
	case 5:
	    move2(x[i], y[i]);
	    draw2(x[i] + et[i], y[i]);
	    if (symok(x[i] + et[i], y[i])) {
		(*vector) ((*devconvx) (x[i] + et[i]), (*devconvy) (y[i]) - ebarlenx, 0);
		(*vector) ((*devconvx) (x[i] + et[i]), (*devconvy) (y[i]) + ebarlenx, color);
	    }
	    break;
	}
    }
}

setcolor(col)
    int col;
{
    color = (*devsetcolor) (col);
}

setlinestyle(style)
    int style;
{
    lines = (*devsetline) (style);
}

/*
        dir = 0 = draw in up (for x-axis) or right (for y-axis)
        axis = 0 = draw x-axis tic,   1 = y-axis
*/
drawtic(x, y, dir, axis)
    double x, y;
    int dir, axis;
{
    (*devdrawtic) ((*devconvx) (x), (*devconvy) (y), dir, axis);
}

setcharsize(size)
    double size;
{
    charsize = charsize * size;
}

writestr(x, y, dir, s)
    double x, y;
    int dir;
    char *s;
{
    (*devwritestr) ((*devconvx) (x), (*devconvy) (y), dir, s);
}

int stringextentx();
int stringextenty();

drawlabels(xl, yl, title, stitle)
    char xl[], yl[], title[], stitle[];

{
    int fudge, ix, iy;
    double tmp;

    fudge = 4;
    tmp = (xg2 + xg1) / 2.0;	/* center x-axis label and title */
    if (xl[0]) {
	ix = stringextentx(devcharsize, xl);
	iy = stringextenty(devcharsize, "Xy");
	(*devwritestr) ((*devconvx) (tmp) - ix / 2, (*devconvy) (yg1) - fudge * iy, 0, xl);
    }
    if (title[0]) {
	charsize *= 1.5;
	iy = (int) (3.0 * stringextenty(devcharsize, "Xy"));
	ix = stringextentx(charsize * devcharsize, title);
	(*devwritestr) ((*devconvx) (tmp) - ix / 2, (*devconvy) (yg2) + iy, 0, title);
	charsize /= 1.5;
    }
    if (stitle[0]) {
	ix = stringextentx(devcharsize, stitle);
	iy = stringextenty(devcharsize, "Xy");
	(*devwritestr) ((*devconvx) (tmp) - ix / 2, (*devconvy) (yg2) + iy, 0, stitle);
    }
    if (yl[0]) {
	tmp = (yg2 + yg1) / 2.0;
	ix = stringextentx(devcharsize, yl);
	iy = stringextenty(devcharsize, yl);
	(*devwritestr) ((*devconvx) (xg1) - (int) (1.4 * ylabpos), (*devconvy) (tmp) - ix / 2, 90, yl);
    }
}

/*
        write axis labels from start to end starting at y with format
        form and decimal or exponential labels selected by style
*/
xlabels(start, end, y, step, form, style, logflag, absflag, angle, topbot)
    double start, end, y, step;
    int form, style, logflag, absflag, angle, topbot;
{
    char s[40];
    char format[20];
    double loc = start;
    int i = 1, ix, iy;

    if (topbot)
	topbot = -1;
    else
	topbot = 1;
    while (loc <= end) {
	if (style)
	    strcpy(format, "%.*lf");
	else
	    strcpy(format, "%.*le");
	if (logflag) {
	    strcpy(format, "10\\S%.0lf\\N");
	    /* sprintf(s, format, form, pow(10.0, loc)); */
	    sprintf(s, format, loc);
	} else {
	    if (absflag) {
		sprintf(s, format, form, fabs(loc));
	    } else {
		sprintf(s, format, form, loc);
	    }
	}
	ix = stringextentx(devcharsize, s);
	iy = stringextenty(devcharsize, s);
	if (angle) {
	    ix = 0;
	} else {
	    ix = stringextentx(devcharsize, s);
	}
	(*devwritestr) ((*devconvx) (loc) - ix / 2, (*devconvy) (y) - 2 * iy * topbot, angle, s);
	loc = start + i++ * step;
    }
}

/*
        ticmarks from start to end starting at y1 and y2
        depending on opflag
*/
xmajor(start, end, y1, y2, step, opflag, outflag)
    double start, end, y1, y2, step;
    int opflag, outflag;
{
    double s = start;

    while (start <= end) {
	if (!outflag)
	    drawtic(start, y1, 0, 0);	/* draw up from y1 */
	else
	    drawtic(start, y1, 0, 1);	/* draw down from y2 */
	start += step;
    }
    if (opflag) {
	start = s;
	while (start <= end) {
	    if (!outflag)
		drawtic(start, y2, 0, 1);	/* draw down from y2 */
	    else
		drawtic(start, y2, 0, 0);	/* draw up from y2 */
	    start += step;
	}
    }
}

xminor(start, end, y1, y2, step, opflag, outflag)
    double start, end, y1, y2, step;
    int opflag, outflag;
{
    int saveticl = devxticl;
    double s = start;

    devxticl /= 2;
    while (start <= end) {
	if (!outflag)
	    drawtic(start, y1, 0, 0);	/* draw up from y1 */
	else
	    drawtic(start, y1, 0, 1);	/* draw down from y2 */
	start += step;
    }
    if (opflag) {
	start = s;
	while (start <= end) {
	    if (!outflag)
		drawtic(start, y2, 0, 1);	/* draw down from y2 */
	    else
		drawtic(start, y2, 0, 0);	/* draw up from y2 */
	    start += step;
	}
    }
    devxticl = saveticl;
}

ylabels(start, end, x, step, form, style, logflag, absflag, leftright)
    double start, end, x, step;
    int form, style, logflag, absflag, leftright;
{
    char s[20];
    char format[20];
    int i = 1, ix, ifudge;
    double loc = start;

    ylabpos = 0;
    while (loc <= end) {
	if (style)
	    strcpy(format, "%.*lf");
	else
	    strcpy(format, "%.*le");
	if (logflag) {
	    sprintf(s, format, form, pow(10.0, loc));
	    strcpy(format, "10\\S%.0lf\\N");
	    /* sprintf(s, format, form, pow(10.0, loc)); */
	    sprintf(s, format, loc);
	} else {
	    if (absflag) {
		sprintf(s, format, form, fabs(loc));
	    } else {
		sprintf(s, format, form, loc);
	    }
	}

	ix = stringextentx(devcharsize, s);
	ifudge = stringextentx(devcharsize, "N");
	if (ix > ylabpos)
	    ylabpos = ix + ifudge;
	if (leftright)
	    (*devwritestr) ((*devconvx) (x) + ifudge, (*devconvy) (loc), 0, s);
	else
	    (*devwritestr) ((*devconvx) (x) - ix - ifudge, (*devconvy) (loc), 0, s);
	loc = start + i++ * step;
    }
    ylabdis = leftright ? ifudge : ylabpos;
}

ymajor(start, end, x1, x2, step, opflag, outflag)
    double start, end, x1, x2, step;
    int opflag, outflag;
{
    double s = start;

    while (start <= end) {
	if (!outflag)
	    drawtic(x1, start, 1, 0);
	else
	    drawtic(x1, start, 1, 1);
	start += step;
    }
    if (opflag) {
	start = s;
	while (start <= end) {
	    if (!outflag)
		drawtic(x2, start, 1, 1);
	    else
		drawtic(x2, start, 1, 0);
	    start += step;
	}
    }
}

yminor(start, end, x1, x2, step, opflag, outflag)
    double start, end, x1, x2, step;
    int opflag, outflag;
{
    int saveticl = devyticl;
    double s = start;

    devyticl /= 2;
    while (start <= end) {
	if (!outflag)
	    drawtic(x1, start, 1, 0);
	else
	    drawtic(x1, start, 1, 1);
	start += step;
    }
    if (opflag) {
	start = s;
	while (start <= end) {
	    if (!outflag)
		drawtic(x2, start, 1, 1);
	    else
		drawtic(x2, start, 1, 0);
	    start += step;
	}
    }
    devyticl = saveticl;
}

xminorlog(start, end, y1, y2, step, opflag, outflag)
    double start, end, y1, y2, step;
    int opflag;
{
    double begint, endt, i, j;
    int saveticl = devxticl;

    devxticl /= 2;
    begint = start;
    endt = end;
    for (i = begint; i < endt; i += 1.0) {
	for (j = 1.0; j <= step; j += 1.0) {
	    if (log10(pow(10.0, i) * j) > endt)
		goto skip;
	    if (!outflag)
		drawtic(log10(pow(10.0, i) * j), y1, 0, 0);
	    else
		drawtic(log10(pow(10.0, i) * j), y1, 0, 1);
	}
    }
skip:
    if (opflag)
	for (i = begint; i < endt; i += 1.0) {
	    for (j = 1.0; j <= step; j += 1.0) {
		if (log10(pow(10.0, i) * j) > endt)
		    return;
		if (!outflag)
		    drawtic(log10(pow(10.0, i) * j), y2, 0, 1);
		else
		    drawtic(log10(pow(10.0, i) * j), y2, 0, 0);
	    }
	}
    devxticl = saveticl;
}

yminorlog(start, end, x1, x2, step, opflag, outflag)
    double start, end, x1, x2, step;
    int opflag;
{
    double begint, endt, i, j;
    int saveticl = devyticl;

    devyticl /= 2;
    begint = start;
    endt = end;
    for (i = begint; i < endt; i += 1.0) {
	for (j = 1.0; j <= step; j += 1.0) {
	    if (log10(pow(10.0, i) * j) > endt)
		goto skip;
	    if (!outflag)
		drawtic(x1, log10(pow(10.0, i) * j), 1, 0);
	    else
		drawtic(x1, log10(pow(10.0, i) * j), 1, 1);
	}
    }
skip:
    if (opflag)
	for (i = begint; i < endt; i += 1.0) {
	    for (j = 1.0; j <= step; j += 1.0) {
		if (log10(pow(10.0, i) * j) > endt)
		    return;
		if (!outflag)
		    drawtic(x2, log10(pow(10.0, i) * j), 1, 1);
		else
		    drawtic(x2, log10(pow(10.0, i) * j), 1, 0);
	    }
	}
    devyticl = saveticl;
}

drawzerox()
{
    move2(xg1, 0.0);
    draw2(xg2, 0.0);
}

drawxzerotics(xt1, xt2)
    double xt1, xt2;
{
    double loc;
    int saveticl = devxticl;

    loc = xg1;
    while (loc <= xg2) {
	drawtic(loc, 0.0, 0, 0);
	drawtic(loc, 0.0, 0, 1);
	loc = loc + xt1;
    }
    loc = xg1;
    devxticl /= 2;
    while (loc <= xg2) {
	drawtic(loc, 0.0, 0, 0);
	drawtic(loc, 0.0, 0, 1);
	loc = loc + xt2;
    }
    devxticl = saveticl;
}

drawzeroy()
{
    move2(0.0, yg1);
    draw2(0.0, yg2);
}

drawyzerotics(yt1, yt2)
    double yt1, yt2;
{
    double loc;
    int saveticl = devyticl;

    loc = yg1;
    while (loc <= yg2) {
	drawtic(0.0, loc, 1, 0);
	drawtic(0.0, loc, 1, 1);
	loc += yt1;
    }
    loc = yg1;
    devyticl /= 2;
    while (loc <= yg2) {
	drawtic(0.0, loc, 1, 0);
	drawtic(0.0, loc, 1, 1);
	loc += yt2;
    }
    devxticl = saveticl;
}

/*
        draw grid lines rather than ticmarks
*/
drawxgrid(start, end, y1, y2, step)
    double start, end, y1, y2, step;
{
    while (start <= end) {
	move2(start, y1);
	draw2(start, y2);
	start += step;
    }
}

drawygrid(start, end, x1, x2, step)
    double start, end, x1, x2, step;
{
    while (start <= end) {
	move2(x1, start);
	draw2(x2, start);
	start += step;
    }
}

/*
        place symbols at the vertices of a polygon specified by x & y.
*/

double barwid = 0.01;

drawpolysym(x, y, len, sym)
    double x[], y[];
int len, sym;

{
    int i;
    char s[10];

    if (sym >= 18) {
	if (sym == 20)
	    hdelta = barwid * (xg2 - xg1);
	else
	    hdelta = fabs(x[1] - x[0]) / 2.0;
	for (i = 0; i < len; i++) {
	    switch (sym) {
	    case 18:
	    case 20:
		histbox(x[i], y[i]);
		break;
	    case 19:
		if (i != len - 1)
		    histbox2(x[i], y[i], y[i + 1], 0);
		else
		    histbox2(x[i], y[i], y[i + 1], 1);
		break;
	    case 21:
		if (symok(x[i], y[i])) {
		    sprintf(s, "%d", i + 1);
		    writestr(x[i], y[i], 0, s);
		}
		break;
	    }
	}
    } else {
	for (i = 0; i < len; i++) {
	    drawsym(x[i], y[i], sym);
	}
    }
}

draw_head(ix1, iy1, ix2, iy2)
    int ix1, iy1, ix2, iy2;
{
    double dist, s = 0.8, ang, dx, dy;
    double adj = 8.0 * M_PI / 180.0;
    int xr, yr, xl, yl;

    dx = ix2 - ix1;
    dy = iy2 - iy1;
    dist = hypot(dx, dy);
    s *= dist;
    ang = atan2(dy, dx);
    (*vector) (ix1, iy1, 0);
    (*vector) (ix2, iy2, color);
    xr = (int) (s * cos(ang - adj));
    yr = (int) (s * sin(ang - adj));
    (*vector) (ix1 + xr, iy1 + yr, color);
    xl = (int) (s * cos(ang + adj));
    yl = (int) (s * sin(ang + adj));
    (*vector) (ix2, iy2, 0);
    (*vector) (ix1 + xl, iy1 + yl, color);
}

draw_arrow(x1, y1, x2, y2, end)
    double x1, y1, x2, y2;
    int end;			/* 0 = none 1 = arrow at x1, y1  2 = arrow at
				 * x2, y2 3 arrow at both ends */
{
    int ix1, iy1, ix2, iy2;

    ix1 = (*devconvx) (x1);
    ix2 = (*devconvx) (x2);
    iy1 = (*devconvy) (y1);
    iy2 = (*devconvy) (y2);
    switch (end) {
    case 0:
	(*vector) (ix1, iy1, 0);
	(*vector) (ix2, iy2, color);
	break;
    case 1:
	draw_head(ix2, iy2, ix1, iy1);
	break;
    case 2:
	draw_head(ix1, iy1, ix2, iy2);
	break;
    case 3:
	draw_head(ix2, iy2, ix1, iy1);
	draw_head(ix1, iy1, ix2, iy2);
	break;
    }
}

putlegend(i, xlen, ylen, size, x, y, sy, ly, cy, s)
    int i, xlen, ylen, sy, ly, cy;
    double size, x, y;
    char *s;
{
    int ifudgex, ifudgey, scol, slin, xtmp, ytmp;

    scol = color;
    slin = lines;
    size *= devcharsize;
    setcolor(cy);
    setlinestyle(ly);
    ifudgey = stringextenty(size, "N");
    ifudgex = stringextentx(size, "N");
    xtmp = (*devconvx) (x);
    ytmp = (*devconvy) (y) - i * ylen * ifudgey;
    (*vector) (xtmp, ytmp, 0);
    if (ly)
	(*vector) (xtmp + xlen * ifudgex, ytmp, color);
    if ((sy > 0) && (sy <= 16)) {
    	setlinestyle(slin);
	if (ly)
	    hwritesym(sy, xtmp, ytmp, size, color, vector);
	hwritesym(sy, xtmp + xlen * ifudgex, ytmp, size, color, vector);
    }
    setcolor(scol);
    setlinestyle(slin);
    (*devwritestr) (xtmp + (xlen + 1) * ifudgex, ytmp, 0, s);
}

initgraphics(device)
    int device;
{
    switch (device) {
    case 0:
	sviewinitgraphics(1);
	break;
    case 1:
	hpinitgraphics(1);
	break;
    case 2:
	hpinitgraphics(3);
	break;
    case 3:
	hpinitgraphics(9);
	break;
    case 4:
	hpinitgraphics(11);
	break;
    case 5:
	geninitgraphics(1);
	break;
    case 6:
	psinitgraphics(1);
	break;
    case 7:
	psinitgraphics(3);
	break;
    case 8:
	hpinitgraphics(5);
	break;
    case 9:
	hpinitgraphics(7);
	break;
    case 10:
	rasterinitgraphics(1);
	break;
    case 11:
	rasterinitgraphics(2);
	break;
    default:
	fprintf(stderr, "No such device\n");
	exit(1);
    }
    devtype = device;
}

leavegraphics()
{
    extern int deviceglob;

    (*devleavegraphics) ();
}
