/* $Id: plotone.c,v 1.32 92/08/15 15:55:08 pturner Exp Locker: pturner $
 *
 * plotone.c - entry for graphics
 *
 */

#include <stdio.h>
#include <math.h>
#include <time.h>
#include <setjmp.h>
#include "globals.h"
#include "symdefs.h"

void plotone();
void draw_ref_point();
void draw_annotation();
void dolegend();
void boxplot();
void draw_string();
void draw_box();
void draw_line();
void drawsetfill();
void drawsetxy();
void drawsethilo();
void drawval();
void drawcirclexy();
void drawsetbar();
void drawsethbar();
void drawsetstackedbar();
void drawsetstackedhbar();
void drawseterrbar();
void drawdensity();
void set_timestamp();
double setcharsize();

int density_flag;		/* temp, until interface for density plots is
				 * completed */

void plotone(gno)
    int gno;
{
    int i, initgraphics();
    world w;
    view v;
    defaults d;
    framep f;
    legend leg;
    labels lab;
    plotarr p;

    get_graph_world(gno, &w);
    get_graph_view(gno, &v);
    get_graph_defaults(gno, &d);
    get_graph_labels(gno, &lab);
    get_graph_legend(gno, &leg);
    get_graph_framep(gno, &f);
    setclipping(TRUE);
    defineworld(w.xg1, w.yg1, w.xg2, w.yg2, islogx(gno), islogy(gno));
    viewport(v.xv1, v.yv1, v.xv2, v.yv2);
    if (debuglevel == 5) {
	printf("frame\n");
    }
    if (f.active == ON) {
	if (f.fillbg == ON) {
	    setcolor(f.bgcolor);
	    fillrectcolor(w.xg1, w.yg1, w.xg2, w.yg2);
	}
	boxplot(f, w);
    }
    if (debuglevel == 5) {
	printf("axes\n");
    }
    setlinestyle(1);
    setlinewidth(1);
    drawaxes(gno);
    setlinestyle(1);
    setlinewidth(1);

    if (debuglevel == 5) {
	printf("title\n");
    }
    if (lab.title.s[0]) {
	if (debuglevel == 5) {
	    printf("draw title\n");
	}
	setlinewidth(lab.title.linew);
	setcolor(lab.title.color);
	setcharsize(lab.title.charsize);
	setfont(lab.title.font);
	drawtitle(lab.title.s, 0);
    }
    if (debuglevel == 5) {
	printf("subtitle\n");
    }
    if (lab.stitle.s[0]) {
	if (debuglevel == 5) {
	    printf("draw subtitle\n");
	}
	setlinewidth(lab.stitle.linew);
	setcolor(lab.stitle.color);
	setcharsize(lab.stitle.charsize);
	setfont(lab.stitle.font);
	drawtitle(lab.stitle.s, 1);
    }
    if (debuglevel == 5) {
	printf("fixed pt\n");
    }
    if (g[gno].pointset) {	/* mark the reference point */
	drawpolysym(&g[gno].dsx, &g[gno].dsy, 1, SYM_CIRCLE, 0, 0, 1.0);
	drawpolysym(&g[gno].dsx, &g[gno].dsy, 1, SYM_PLUS, 0, 0, 1.0);
    }
    if (debuglevel == 5) {
	printf("sets\n");
    }
    /*
     * draw sets
     */
    switch (g[gno].type) {
    case BAR:
	{
	    int tset = 0, maxn = 0;
	    double x1, y1, x2, y2, minx, miny, maxx, maxy;
	    double tdx, fmin(), fmax();

	    minx = miny = maxx = maxy = 0.0;
	    for (i = 0; i < g[gno].maxplot; i++) {
		if (isactive(gno, i)) {
		    tset++;
		    maxn += getsetlength(gno, i);
		    getsetminmax(gno, i, &x1, &x2, &y1, &y2);
		    if (tset == 1) {
			minx = x1;
			miny = y1;
			maxx = x2;
			maxy = y2;
		    } else {
			minx = fmin(x1, minx);
			miny = fmin(y1, miny);
			maxx = fmax(x2, maxx);
			maxy = fmax(y2, maxy);
		    }
		}
	    }
	    tdx = maxx - minx;
	    if (tset != 0 && maxn != 0) {
		double offsx, offsy;
		double bsize = 0.85 * tdx / maxn;
		double cset = -tset / 2.0;

		offsx = -cset * bsize - bsize / 2.0;
		offsy = 0.0;

		for (i = 0; i < g[gno].maxplot; i++) {

		    if (isactive(gno, i)) {
			drawsetbar(gno, i, cset, bsize);
			switch (dataset_type(gno, i)) {
			case XYDX:
			case XYDY:
			case XYDXDX:
			case XYDYDY:
			    drawseterrbar(gno, i, offsx, offsy);
			    break;
			case XYHILO:
			    break;
			}
			cset += 1.0;
			offsx = -cset * bsize - bsize / 2.0;
			offsy = 0.0;
		    }
		}
	    }
	}
	break;
    case STACKEDBAR:
	{
	    int tset = 0, maxn = 0;
	    double x1, y1, x2, y2, minx, miny, maxx, maxy;
	    double tdx, fmin(), fmax();

	    minx = miny = maxx = maxy = 0.0;
	    for (i = 0; i < g[gno].maxplot; i++) {
		if (isactive(gno, i)) {
		    tset++;
		    getsetminmax(gno, i, &x1, &x2, &y1, &y2);
		    if (tset == 1) {
			maxn = getsetlength(gno, i);
			minx = x1;
			miny = y1;
			maxx = x2;
			maxy = y2;
		    } else {
			maxn = (maxn < getsetlength(gno, i)) ? getsetlength(gno, i) : maxn;
			minx = fmin(x1, minx);
			miny = fmin(y1, miny);
			maxx = fmax(x2, maxx);
			maxy = fmax(y2, maxy);
		    }
		}
	    }
	    tdx = maxx - minx;
	    if (tset != 0 && maxn != 0) {
		double bsize = 0.75 * tdx / maxn;

		drawsetstackedbar(gno, maxn, bsize);
	    }
	}
	break;
    case HBAR:
	{
	    int tset = 0, maxn = 0;
	    double x1, y1, x2, y2, minx, miny, maxx, maxy;
	    double tdy, fmin(), fmax();

	    minx = miny = maxx = maxy = 0.0;
	    for (i = 0; i < g[gno].maxplot; i++) {
		if (isactive(gno, i)) {
		    tset++;
		    maxn += getsetlength(gno, i);
		    getsetminmax(gno, i, &x1, &x2, &y1, &y2);
		    if (tset == 1) {
			minx = x1;
			miny = y1;
			maxx = x2;
			maxy = y2;
		    } else {
			minx = fmin(x1, minx);
			miny = fmin(y1, miny);
			maxx = fmax(x2, maxx);
			maxy = fmax(y2, maxy);
		    }
		}
	    }
	    tdy = maxy - miny;
	    if (tset != 0 && maxn != 0) {
		double offsx, offsy;
		double bsize = 0.85 * tdy / maxn;
		double cset = -tset / 2.0;

		offsy = -cset * bsize - bsize / 2.0;
		offsx = 0.0;

		for (i = 0; i < g[gno].maxplot; i++) {

		    if (isactive(gno, i)) {
			drawsethbar(gno, i, cset, bsize);
			switch (dataset_type(gno, i)) {
			case XYDX:
			case XYDY:
			case XYDXDX:
			case XYDYDY:
			    drawseterrbar(gno, i, offsx, offsy);
			    break;
			case XYHILO:
			    break;
			}
			cset += 1.0;
			offsy = -cset * bsize - bsize / 2.0;
			offsx = 0.0;
		    }
		}
	    }
	}
	break;
    case STACKEDHBAR:
	{
	    int tset = 0, maxn = 0;
	    double x1, y1, x2, y2, minx, miny, maxx, maxy;
	    double tdy, fmin(), fmax();

	    minx = miny = maxx = maxy = 0.0;
	    for (i = 0; i < g[gno].maxplot; i++) {
		if (isactive(gno, i)) {
		    tset++;
		    getsetminmax(gno, i, &x1, &x2, &y1, &y2);
		    if (tset == 1) {
			maxn = getsetlength(gno, i);
			minx = x1;
			miny = y1;
			maxx = x2;
			maxy = y2;
		    } else {
			maxn = (maxn < getsetlength(gno, i)) ? getsetlength(gno, i) : maxn;
			minx = fmin(x1, minx);
			miny = fmin(y1, miny);
			maxx = fmax(x2, maxx);
			maxy = fmax(y2, maxy);
		    }
		}
	    }
	    tdy = maxy - miny;
	    if (tset != 0 && maxn != 0) {
		double bsize = 0.75 * tdy / maxn;

		drawsetstackedhbar(gno, maxn, bsize);
	    }
	}
	break;
    case POLAR:
	break;
    case PIE:
	break;
    case STACKEDLINE:
	break;
    case LOGX:
    case LOGY:
    case LOGXY:
    case XY:
	for (i = 0; i < g[gno].maxplot; i++) {
	    get_graph_plotarr(gno, i, &p);
	    if (isactive(gno, i)) {
		switch (dataset_type(gno, i)) {
		case XY:
		    if (p.fill) {
			drawsetfill(gno, p);
		    }
		    drawsetxy(g[gno].p[i], i);
		    break;
		case XYDX:
		case XYDY:
		case XYDXDX:
		case XYDYDY:
		case XYDXDY:
		    if (p.fill) {
			drawsetfill(gno, p);
		    }
		    drawseterrbar(gno, i, 0.0, 0.0);
		    drawsetxy(g[gno].p[i], i);
		    break;
		case XYXX:
		    drawsetxy(g[gno].p[i], i);
		    break;
		case XYYY:
		    drawsetxy(g[gno].p[i], i);
		    break;
		case XYZ:
		    if (!density_flag) {
			drawsetxy(g[gno].p[i], i);
			drawval(g[gno].p[i]);
		    } else {
			drawdensity(g[gno].p[i]);
		    }
		    break;
		case XYZW:
		    drawsetxy(g[gno].p[i], i);
		    break;
		case XYRT:
		    drawsetxy(g[gno].p[i], i);
		    drawcirclexy(g[gno].p[i]);
		    break;
		case XYX2Y2:
		    break;
		case XYSEG:
		    break;
		case XYBOX:
		    break;
		case XYHILO:
		    drawsethilo(g[gno].p[i]);
		    break;
		case XYSTRING:
		    break;
		}
	    }
	}
    }
    if (debuglevel == 5) {
	printf("regions\n");
    }
    setlinestyle(d.lines);
    setlinestyle(d.linew);
/*
 * draw any defined regions for this graph
 */
    for (i = 0; i < MAXREGION; i++) {
	if (rg[i].active == ON && rg[i].linkto[gno]) {
	    draw_region(i);
	}
    }

    if (debuglevel == 5) {
	printf("legend\n");
    }
    setlinestyle(d.lines);
    setlinestyle(d.linew);
    if (leg.active == ON) {
	dolegend(gno);
	setlinestyle(d.lines);
	setlinestyle(d.linew);
    }
    if (timestamp.active == ON) {
	double xtmp, ytmp;
	if (debuglevel == 5) {
	    printf("draw timestamp\n");
	}
	set_timestamp();
	setlinewidth(timestamp.linew);
	setcolor(timestamp.color);
	setcharsize(timestamp.charsize);
	setfont(timestamp.font);
	view2world(timestamp.x, timestamp.y, &xtmp, &ytmp);
	writestr(xtmp, ytmp, timestamp.rot, timestamp.just, timestamp.s);
    }
}

void draw_ref_point(gno)
{
    drawpolysym(&g[gno].dsx, &g[gno].dsy, 1, SYM_CIRCLE, 0, 0, 1.0);
    drawpolysym(&g[gno].dsx, &g[gno].dsy, 1, SYM_PLUS, 0, 0, 1.0);
    drawpolysym(&g[gno].dsx, &g[gno].dsy, 1, SYM_PLUS, 0, 0, 1.0);
}

void draw_annotation(gno)
    int gno;
{
    int i;

    setclipping(0);		/* shut down clipping for strings, boxes,
				 * lines, and legends */
    if (debuglevel == 5) {
	printf("Boxes\n");
    }
    for (i = 0; i < MAXBOXES; i++) {
	if (isactive_box(i)) {
	    draw_box(gno, i);
	}
    }
    if (debuglevel == 5) {
	printf("Lines\n");
    }
    for (i = 0; i < MAXLINES; i++) {
	if (isactive_line(i)) {
	    draw_line(gno, i);
	}
    }
    if (debuglevel == 5) {
	printf("Strings\n");
    }
    for (i = 0; i < MAXSTR; i++) {
	if (isactive_string(i)) {
	    if (debuglevel == 5) {
		printf("String %d\n", i);
	    }
	    draw_string(gno, i);
	}
    }
    setclipping(1);
}

/*
 * draw the legend at (legx, legy)
 * ib = 1 in loop means accumulate info for box
 * ib = -1 after loop means draw box
 */
void dolegend(gno)
    int gno;
{

    int i, j = 0, sy = 0, cy = 0, ly = 0, wy = 0, ib = 0;
    double tmpx, tmpy;
    legend l;
    plotarr p;

    get_graph_legend(gno, &l);

    if (l.loctype == VIEW) {
	view2world(l.legx, l.legy, &tmpx, &tmpy);
    } else {
	tmpx = l.legx;
	tmpy = l.legy;
    }
    j = 0;
    setcharsize(l.charsize);
    setfont(l.font);
    if (l.box == ON) {		/* compute a bounding box for the legend */
	for (i = 0; i < MAXPLOT; i++) {
	    if (isactive(gno, i) && (l.str[i].s[0])) {
		get_graph_plotarr(gno, i, &p);
		putlegend(j, 1, l.len, l.vgap, p.symsize, tmpx, tmpy, sy, ly, cy, wy, l.str[i].s, 0, p.symfill, -1, -1, -1);
		j++;
	    }
	}
	putlegendrect(l.boxfill == ON,
		      l.boxfillusing == COLOR,
		      l.boxfillcolor,
		      l.boxfillpat,
		      l.boxlcolor,
		      l.boxlinew,
		      l.boxlines);
    }
    j = 0;
    for (i = 0; i < MAXPLOT; i++) {
	if (isactive(gno, i) && (l.str[i].s[0])) {
	    setcolor(l.color);
	    setcharsize(l.charsize);
	    setfont(l.font);
	    get_graph_plotarr(gno, i, &p);
	    sy = p.sym;
	    ly = p.lines;
	    wy = p.linew;
	    cy = p.color;
	    switch (g[gno].type) {
	    case XY:
		putlegend(j, 0, l.len, l.vgap, p.symsize, tmpx, tmpy, sy, ly, cy, wy, l.str[i].s, 0, p.symfill, -1, -1, -1);
		break;
	    case BAR:
	    case STACKEDBAR:
		putlegend(j, 0, l.len, l.vgap, p.symsize, tmpx, tmpy, sy, ly, cy, wy, l.str[i].s, 1, p.symfill, p.fillusing == PATTERN, p.fillcolor, p.fillpattern);
		break;
	    default:
		putlegend(j, 0, l.len, l.vgap, p.symsize, tmpx, tmpy, sy, ly, cy, wy, l.str[i].s, 0, p.symfill, -1, -1, -1);
		break;
	    }
	    j++;
	}
    }
}

/*
 * draw the graph frame
 */
void boxplot(f, w)
    framep f;
    world w;
{
    int c, s, wi;

    c = setcolor(f.color);
    s = setlinestyle(f.lines);
    wi = setlinewidth(f.linew);
    if (f.type == 0) {
	rect(w.xg1, w.yg1, w.xg2, w.yg2);
    } else {
	my_move2(w.xg1, w.yg1);
	my_draw2(w.xg2, w.yg1);
	my_move2(w.xg1, w.yg1);
	my_draw2(w.xg1, w.yg2);
    }
    setcolor(c);
    setlinestyle(s);
    setlinewidth(wi);
}

/*
 * draw annotative text
 */
void draw_string(gno, i)
    int gno, i;
{
    double xtmp1, ytmp1;
    int f, c, w;
    double s;
    plotstr pstr;

    get_graph_string(i, &pstr);
    if (debuglevel == 5) {
	printf("String %d %s\n", i, pstr.s);
    }
    if (gno != -2) {
	if (pstr.loctype == WORLD && pstr.gno != gno) {
	    return;
	}
	if (pstr.loctype == VIEW && gno != -1) {
	    return;
	}
    }
    if (strlen(pstr.s) && (pstr.charsize > 0.0) && (pstr.active == ON)) {
	c = setcolor(pstr.color);
	w = setlinewidth(pstr.linew);
	s = setcharsize(pstr.charsize);
	f = setfont(pstr.font);
	if (pstr.loctype == WORLD) {
	    writestr(pstr.x, pstr.y, pstr.rot, pstr.just, pstr.s);
	} else {
	    view2world(pstr.x, pstr.y, &xtmp1, &ytmp1);
	    writestr(xtmp1, ytmp1, pstr.rot, pstr.just, pstr.s);
	}
	(void) setcolor(c);
	(void) setlinewidth(w);
	(void) setcharsize(s);
	(void) setfont(f);
    }
}

/*
 * draw annotative boxes
 */
void draw_box(gno, i)
    int gno, i;
{
    double xtmp1, ytmp1;
    double xtmp2, ytmp2;
    int c, l, w;
    boxtype b;

    get_graph_box(i, &b);
    if (gno != -2) {
	if (b.loctype == WORLD && b.gno != gno) {
	    return;
	}
	if (b.loctype == VIEW && gno != -1) {
	    return;
	}
    }
    if (b.active == ON) {
	setclipping(0);

	if (b.fill == COLOR) {
	    c = setcolor(b.fillcolor);
	    if (b.loctype == WORLD) {
		fillrectcolor(b.x1, b.y1, b.x2, b.y2);
	    } else {
		view2world(b.x1, b.y1, &xtmp1, &ytmp1);
		view2world(b.x2, b.y2, &xtmp2, &ytmp2);
		fillrectcolor(xtmp1, ytmp1, xtmp2, ytmp2);
	    }
	    setcolor(c);
	} else if (b.fill == PATTERN) {
	    c = setpattern(b.fillpattern);
	    if (b.loctype == WORLD) {
		fillrectpat(b.x1, b.y1, b.x2, b.y2);
	    } else {
		view2world(b.x1, b.y1, &xtmp1, &ytmp1);
		view2world(b.x2, b.y2, &xtmp2, &ytmp2);
		fillrectpat(xtmp1, ytmp1, xtmp2, ytmp2);
	    }
	}
	c = setcolor(b.color);
	l = setlinestyle(b.lines);
	w = setlinewidth(b.linew);
	if (b.loctype == WORLD) {
	    rect(b.x1, b.y1, b.x2, b.y2);
	} else {
	    view2world(b.x1, b.y1, &xtmp1, &ytmp1);
	    view2world(b.x2, b.y2, &xtmp2, &ytmp2);
	    rect(xtmp1, ytmp1, xtmp2, ytmp2);
	}
	setclipping(1);
	setcolor(c);
	setlinewidth(w);
	setlinestyle(l);
    }
}

/*
 * draw annotative lines
 */
void draw_line(gno, i)
    int gno, i;
{
    double xtmp1, ytmp1;
    double xtmp2, ytmp2;
    int c, ll, w;
    linetype l;

    get_graph_line(i, &l);
    if (gno != -2) {
	if (l.loctype == WORLD && l.gno != gno) {
	    return;
	}
	if (l.loctype == VIEW && gno != -1) {
	    return;
	}
    }
    if (l.active == ON) {
	setclipping(0);
	c = setcolor(l.color);
	ll = setlinestyle(l.lines);
	w = setlinewidth(l.linew);
	if (l.loctype == WORLD) {
	    draw_arrow(l.x1, l.y1, l.x2, l.y2, l.arrow, l.asize, l.atype);
	} else {
	    view2world(l.x1, l.y1, &xtmp1, &ytmp1);
	    view2world(l.x2, l.y2, &xtmp2, &ytmp2);
	    draw_arrow(xtmp1, ytmp1, xtmp2, ytmp2, l.arrow, l.asize, l.atype);
	}
	setclipping(1);
	setcolor(c);
	setlinewidth(w);
	setlinestyle(ll);
    }
}

/*
 * draw a set with a fill
 */
void drawsetfill(gno, p)
    int gno;
    plotarr p;
{
    int i, c, len;
    double *x = p.ex[0], *y = p.ex[1], *xtmp, *ytmp, xb[4], yb[4];

    len = p.len + 2;
    xtmp = (double *) calloc(len, sizeof(double));
    ytmp = (double *) calloc(len, sizeof(double));
    if (xtmp == NULL || ytmp == NULL) {
	errwin("Can't malloc for fills in plotone");
	cxfree(xtmp);
	cxfree(ytmp);
	return;
    }
    for (i = 0; i < p.len; i++) {
	xtmp[i] = x[i];
	ytmp[i] = y[i];
    }
    switch (p.fill) {
    case 1:
	len = p.len;
	break;
    case 2:
	xtmp[p.len] = (p.xmax > g[gno].w.xg2) ? g[gno].w.xg2 : p.xmax;
	ytmp[p.len] = 0.0;
	xtmp[p.len + 1] = (p.xmin < g[gno].w.xg1) ? g[gno].w.xg1 : p.xmin;
	ytmp[p.len + 1] = 0.0;
	break;
    case 3:
	xtmp[p.len] = 0.0;
	ytmp[p.len] = (p.ymax > g[gno].w.yg2) ? g[gno].w.yg2 : p.ymax;
	xtmp[p.len + 1] = 0.0;
	ytmp[p.len + 1] = (p.ymin < g[gno].w.yg1) ? g[gno].w.yg1 : p.ymin;
	break;
    case 4:
	xtmp[p.len] = g[gno].w.xg1;
	ytmp[p.len] = (p.ymax > g[gno].w.yg2) ? g[gno].w.yg2 : p.ymax;
	xtmp[p.len + 1] = g[gno].w.xg1;
	ytmp[p.len + 1] = (p.ymin < g[gno].w.yg1) ? g[gno].w.yg1 : p.ymin;
	break;
    case 5:
	xtmp[p.len] = g[gno].w.xg2;
	ytmp[p.len] = (p.ymax > g[gno].w.yg2) ? g[gno].w.yg2 : p.ymax;
	xtmp[p.len + 1] = g[gno].w.xg2;
	ytmp[p.len + 1] = (p.ymin < g[gno].w.yg1) ? g[gno].w.yg1 : p.ymin;
	break;
    case 6:			/* fill to ymin */
	xtmp[p.len] = (p.xmax > g[gno].w.xg2) ? g[gno].w.xg2 : p.xmax;
	ytmp[p.len] = g[gno].w.yg1;
	xtmp[p.len + 1] = (p.xmin < g[gno].w.xg1) ? g[gno].w.xg1 : p.xmin;
	ytmp[p.len + 1] = g[gno].w.yg1;
	break;
    case 7:
	xtmp[p.len] = (p.xmax > g[gno].w.xg2) ? g[gno].w.xg2 : p.xmax;
	ytmp[p.len] = g[gno].w.yg2;
	xtmp[p.len + 1] = (p.xmin < g[gno].w.xg1) ? g[gno].w.xg1 : p.xmin;
	ytmp[p.len + 1] = g[gno].w.yg2;
	break;
    }
    if (p.fillusing == COLOR) {
	c = setcolor(p.fillcolor);
	fillcolor(len, xtmp, ytmp);
	setcolor(c);
    } else if (p.fillusing == PATTERN) {
	setpattern(p.fillpattern);
	fillpattern(len, xtmp, ytmp);
    }
    cxfree(xtmp);
    cxfree(ytmp);
}

/*
 * draw a standard set with symbols and lines
 */
void drawsetxy(p, i)
    plotarr p;
    int i;
{
    int j, c, l, w, cy = p.color, sy = p.sym, ly = p.lines, wy = p.linew;
    double xbar, sd, *x = p.ex[0], *y = p.ex[1];
    char s[256];

    c = setcolor(cy);
    w = setlinewidth(wy);
    if (wy && ly) {
	l = setlinestyle(ly);
	setlinewidth(wy);
	drawpoly(x, y, p.len);
	setlinestyle(l);
    }
    if (sy) {
	switch (sy) {
	case SYM_SEG:		/* draw segments */
	    drawpolyseg(x, y, p.len);
	    break;
	case SYM_CHAR:		/* draw character */
	    setfont(p.font);
	    if (p.symchar > ' ') {
		int skip = p.symskip + 1;

		s[0] = p.symchar;
		s[1] = 0;
		for (j = 0; j < p.len; j += skip) {
		    if (symok(x[j], y[j])) {
			writestr(x[j], y[j], 0, 2, s);
		    }
		}
	    }
	    break;
	case SYM_HILOX:	/* draw hilo along X */
	    for (j = 0; j < p.len; j++) {
		my_move2(p.ex[0][j], y[j]);
		my_draw2(p.ex[2][j], y[j]);
	    }
	    break;
	case SYM_HILOY:	/* draw hilo along Y */
	    for (j = 0; j < p.len; j++) {
		my_move2(x[j], y[j]);
		my_draw2(x[j], p.ex[2][j]);
	    }
	    break;
	case SYM_OPENCLOSEX:	/* draw open/close along X */
	    for (j = 0; j < p.len; j++) {
		openclose(y[j], x[j], p.ex[2][j], 1.0, 0);
	    }
	    break;
	case SYM_OPENCLOSEY:	/* draw open/close along Y */
	    for (j = 0; j < p.len; j++) {
		openclose(x[j], y[j], p.ex[2][j], 1.0, 1);
	    }
	    break;
	case SYM_RANGE:	/* draw bounding box */
	    rect(p.xmin, p.ymin, p.xmax, p.ymax);
	    stasum(y, p.len, &xbar, &sd, 0);
	    my_move2(p.xmin, xbar);
	    my_draw2(p.xmax, xbar);
	    stasum(x, p.len, &xbar, &sd, 0);
	    my_move2(xbar, p.ymin);
	    my_draw2(xbar, p.ymax);
	    break;
	case SYM_TAG_FIRST:	/* tag the first point in a set */
	    if (symok(x[0], y[0])) {
		sprintf(s, "S%1d:1", i);
		sd = setcharsize(0.8);
		writestr(x[0], y[0], 0, 2, s);
		(void) setcharsize(sd);
	    }
	    break;
	case SYM_TAG_LAST:	/* tag the last point in a set */
	    if (symok(x[p.len - 1], y[p.len - 1])) {
		sprintf(s, "S%1d:%1d", i, p.len);
		sd = setcharsize(0.8);
		writestr(x[p.len - 1], y[p.len - 1], 0, 2, s);
		(void) setcharsize(sd);
	    }
	    break;
	case SYM_TAG_CENTER:	/* tag the middle point in a set */
	    if (symok(x[p.len / 2], y[p.len / 2])) {
		sprintf(s, "S%1d:%1d", i, p.len / 2);
		sd = setcharsize(0.8);
		writestr(x[p.len / 2], y[p.len / 2], 0, 2, s);
		(void) setcharsize(sd);
	    }
	    break;
	case SYM_STRING:	/* string at plot */
	    /* drawpolystring(x, y, p.len, sy, 0); */
	    break;
	case SYM_SETNO_LOC:	/* set number and location */
	case SYM_SETNO:	/* set number */
	    for (j = 0; j < p.len; j++) {
		if (symok(x[j], y[j])) {
		    if (sy == SYM_SETNO) {
			sprintf(s, "S%d", i);
		    } else {
			sprintf(s, "S%1d:%1d)", i, j + 1);
		    }
		    writestr(x[j], y[j], 0, 0, s);
		}
	    }
	    break;
	default:
	    drawpolysym(x, y, p.len, sy, p.symskip, p.symfill, p.symsize);
	    break;
	}
    }
    setlinewidth(w);
    setcolor(c);
}

/*
 * draw hi/lo-open/close
 */
void drawsethilo(p)
    plotarr p;
{
    int i, c, w, cy = p.color, wy = p.linew;
    double *x = p.ex[0], *y = p.ex[1], *dx = p.ex[2], *dy = p.ex[3], *dz = p.ex[4];

    c = setcolor(cy);
    w = setlinewidth(wy);
    for (i = 0; i < p.len; i++) {
	my_move2(x[i], y[i]);
	my_draw2(x[i], dx[i]);
	openclose(x[i], dy[i], dz[i], 1.0, 1);
    }
    setlinewidth(w);
    setcolor(c);
}

/*
 * draw a value as a string at x, y
 */
void drawval(p)
    plotarr p;
{
    int j;
    double *x = p.ex[0], *y = p.ex[1], *z = p.ex[2], loc;
    char *s1, s2[256];

    s2[0] = ' ';
    s1 = &s2[1];
    setfont(p.font);
    for (j = 0; j < p.len; j++) {
	if (symok(x[j], y[j])) {
	    loc = z[j];
	    create_ticklabel(p.format, p.prec, loc, s1);
	    if (p.sym) {
		writestr(x[j], y[j], 0, 0, s1 - 1);
	    } else {
		writestr(x[j], y[j], 0, 2, s1);
	    }
	}
    }
}

/*
 * draw a density plot
 */
void drawdensity(p)
    plotarr p;
{
    int j, c, w;
    double *x = p.ex[0], *y = p.ex[1], *z = p.ex[2], loc;
    char *s1, s2[256];

    c = setcolor(p.color);
    w = setlinewidth(p.linew);
    for (j = 0; j < p.len; j++) {
	if (symok(x[j], y[j])) {
	    loc = z[j];
	    if ((p.zmin == p.zmax) || (loc >= p.zmin && loc <= p.zmax)) {
		drawpolysym(&x[j], &y[j], 1, p.sym, p.symskip, p.symfill, z[j]);
	    }
	}
    }
    setcolor(c);
    setlinewidth(w);
}


/*
 * draw a circle centered at x, y with radius z
 */
void drawcirclexy(p)
    plotarr p;
{
    int j;
    int cy = p.color, wy = p.linew;
    double *x = p.ex[0], *y = p.ex[1], *z = p.ex[2];

    cy = setcolor(cy);
    wy = setlinewidth(wy);
    for (j = 0; j < p.len; j++) {
	if (symok(x[j], y[j])) {
	    drawcircle(x[j], y[j], z[j], 0);
	}
    }
    setcolor(cy);
    setlinewidth(wy);
}

/*
 * draw a set in a bar chart
 */
void drawsetbar(gno, setno, cset, bsize)
    int gno, setno;
    double cset, bsize;
{
    int i, j;
    int c, l, w, p, ls;
    int cc = g[gno].p[setno].color;
    int cy = g[gno].p[setno].fillcolor;
    int py = g[gno].p[setno].fillpattern;
    int ly = g[gno].p[setno].lines, wy = g[gno].p[setno].linew;
    double *x = getx(gno, setno), *y = gety(gno, setno);
    double tmpx[4];
    double tmpy[4];

    c = setcolor(cy);
    p = setpattern(py);
    l = setlinestyle(ly);
    w = setlinewidth(wy);
    if (g[gno].p[setno].fill) {
	for (i = 0; i < g[gno].p[setno].len; i++) {
	    tmpx[0] = x[i] + cset * bsize;
	    tmpy[0] = 0.0;
	    tmpx[1] = x[i] + cset * bsize;
	    tmpy[1] = y[i];
	    tmpx[2] = x[i] + (cset + 1.0) * bsize;
	    tmpy[2] = y[i];
	    tmpx[3] = x[i] + (cset + 1.0) * bsize;
	    tmpy[3] = 0.0;
	    if (tmpx[0] > g[gno].w.xg2) {
		continue;
	    }
	    if (tmpx[2] < g[gno].w.xg1) {
		continue;
	    }
	    for (j = 0; j < 4; j++) {
		if (tmpx[j] < g[gno].w.xg1)
		    tmpx[j] = g[gno].w.xg1;
		else if (tmpx[j] > g[gno].w.xg2)
		    tmpx[j] = g[gno].w.xg2;
		if (tmpy[j] < g[gno].w.yg1)
		    tmpy[j] = g[gno].w.yg1;
		else if (tmpy[j] > g[gno].w.yg2)
		    tmpy[j] = g[gno].w.yg2;
	    }
	    if (g[gno].p[setno].fillusing == PATTERN) {
		fillrectpat(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
	    } else if (g[gno].p[setno].fillusing == COLOR) {
		fillrectcolor(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
	    }
	}
    }
    if (ly && wy) {
	for (i = 0; i < g[gno].p[setno].len; i++) {
	    tmpx[0] = x[i] + cset * bsize;
	    tmpy[0] = 0.0;
	    tmpx[1] = x[i] + cset * bsize;
	    tmpy[1] = y[i];
	    tmpx[2] = x[i] + (cset + 1.0) * bsize;
	    tmpy[2] = y[i];
	    tmpx[3] = x[i] + (cset + 1.0) * bsize;
	    tmpy[3] = 0.0;
	    setcolor(cc);
	    setlinestyle(ly);
	    setlinewidth(wy);
	    my_move2(tmpx[0], tmpy[0]);
	    for (j = 0; j < 4; j++) {
		my_draw2(tmpx[(j + 1) % 4], tmpy[(j + 1) % 4]);
	    }
	}
    }
    setlinestyle(l);
    setlinewidth(w);
    setcolor(c);
    setpattern(p);
}

/*
 * draw a set in a horizontal bar chart
 */
void drawsethbar(gno, setno, cset, bsize)
    int gno, setno;
    double cset, bsize;
{
    int i, j;
    int c, l, w, p, ls;
    int cc = g[gno].p[setno].color;
    int cy = g[gno].p[setno].fillcolor;
    int py = g[gno].p[setno].fillpattern;
    int ly = g[gno].p[setno].lines, wy = g[gno].p[setno].linew;
    double *x = getx(gno, setno), *y = gety(gno, setno);
    double tmpx[4];
    double tmpy[4];

    c = setcolor(cy);
    p = setpattern(py);
    l = setlinestyle(ly);
    w = setlinewidth(wy);
    if (g[gno].p[setno].fill) {
	for (i = 0; i < g[gno].p[setno].len; i++) {
	    tmpy[0] = y[i] + cset * bsize;
	    tmpx[0] = 0.0;
	    tmpy[1] = y[i] + cset * bsize;
	    tmpx[1] = x[i];
	    tmpy[2] = y[i] + (cset + 1.0) * bsize;
	    tmpx[2] = x[i];
	    tmpy[3] = y[i] + (cset + 1.0) * bsize;
	    tmpx[3] = 0.0;
	    if (tmpy[0] > g[gno].w.yg2) {
		continue;
	    }
	    if (tmpy[2] < g[gno].w.yg1) {
		continue;
	    }
	    for (j = 0; j < 4; j++) {
		if (tmpy[j] < g[gno].w.yg1)
		    tmpy[j] = g[gno].w.yg1;
		else if (tmpy[j] > g[gno].w.yg2)
		    tmpy[j] = g[gno].w.yg2;
		if (tmpx[j] < g[gno].w.xg1)
		    tmpx[j] = g[gno].w.xg1;
		else if (tmpx[j] > g[gno].w.xg2)
		    tmpx[j] = g[gno].w.xg2;
	    }
	    if (g[gno].p[setno].fillusing == PATTERN) {
		fillrectpat(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
	    } else if (g[gno].p[setno].fillusing == COLOR) {
		fillrectcolor(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
	    }
	}
    }
    if (ly && wy) {
	for (i = 0; i < g[gno].p[setno].len; i++) {
	    tmpy[0] = y[i] + cset * bsize;
	    tmpx[0] = 0.0;
	    tmpy[1] = y[i] + cset * bsize;
	    tmpx[1] = x[i];
	    tmpy[2] = y[i] + (cset + 1.0) * bsize;
	    tmpx[2] = x[i];
	    tmpy[3] = y[i] + (cset + 1.0) * bsize;
	    tmpx[3] = 0.0;
	    setcolor(cc);
	    setlinestyle(ly);
	    setlinewidth(wy);
	    my_move2(tmpx[0], tmpy[0]);
	    for (j = 0; j < 4; j++) {
		my_draw2(tmpx[(j + 1) % 4], tmpy[(j + 1) % 4]);
	    }
	}
    }
    setlinestyle(l);
    setlinewidth(w);
    setcolor(c);
    setpattern(p);
}

/*
 * draw a set in a stacked bar chart
 */
void drawsetstackedbar(gno, maxn, bsize)
    int gno, maxn;
    double bsize;
{
    int i, j, k;
    int c, l, w, p, ls;
    int cc, cy, py, ly, wy;
    double *x, *y;
    double tmpx[4];
    double tmpy[4];
    double *sum = (double *) calloc(maxn, sizeof(double));

    if (sum == NULL) {
	errwin("Can't calloc in drawsetstackedbar\n");
	return;
    }
    for (i = 0; i < g[gno].maxplot; i++) {
	if (isactive(gno, i)) {
	    x = getx(gno, i);
	    y = gety(gno, i);
	    cc = g[gno].p[i].color;
	    cy = g[gno].p[i].fillcolor;
	    py = g[gno].p[i].fillpattern;
	    ly = g[gno].p[i].lines;
	    wy = g[gno].p[i].linew;
	    c = setcolor(cy);
	    p = setpattern(py);
	    l = setlinestyle(ly);
	    w = setlinewidth(wy);
	    for (j = 0; j < maxn; j++) {
		if (j < getsetlength(gno, i)) {
		    if (g[gno].p[i].fill) {
			c = setcolor(cy);
			tmpx[0] = x[j] - bsize * 0.5;
			tmpy[0] = sum[j];
			tmpx[1] = x[j] - bsize * 0.5;
			tmpy[1] = sum[j] + y[j];
			tmpx[2] = x[j] + bsize * 0.5;
			tmpy[2] = sum[j] + y[j];
			tmpx[3] = x[j] + bsize * 0.5;
			tmpy[3] = sum[j];
			if (tmpx[0] > g[gno].w.xg2) {
			    continue;
			}
			if (tmpx[2] < g[gno].w.xg1) {
			    continue;
			}
			for (k = 0; k < 4; k++) {
			    if (tmpx[k] < g[gno].w.xg1)
				tmpx[k] = g[gno].w.xg1;
			    else if (tmpx[k] > g[gno].w.xg2)
				tmpx[k] = g[gno].w.xg2;
			    if (tmpy[k] < g[gno].w.yg1)
				tmpy[k] = g[gno].w.yg1;
			    else if (tmpy[k] > g[gno].w.yg2)
				tmpy[k] = g[gno].w.yg2;
			}
			if (g[gno].p[i].fillusing == PATTERN) {
			    fillrectpat(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
			} else if (g[gno].p[i].fillusing == COLOR) {
			    fillrectcolor(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
			}
		    }
		    if (ly && wy) {
			tmpx[0] = x[j] - bsize * 0.5;
			tmpy[0] = sum[j];
			tmpx[1] = x[j] - bsize * 0.5;
			tmpy[1] = sum[j] + y[j];
			tmpx[2] = x[j] + bsize * 0.5;
			tmpy[2] = sum[j] + y[j];
			tmpx[3] = x[j] + bsize * 0.5;
			tmpy[3] = sum[j];
			setcolor(cc);
			my_move2(tmpx[0], tmpy[0]);
			for (k = 0; k < 4; k++) {
			    my_draw2(tmpx[(k + 1) % 4], tmpy[(k + 1) % 4]);
			}
		    }
		    sum[j] += y[j];
		}
	    }
	}
    }
    cfree(sum);
}

/*
 * draw a set in a horizontal stacked bar chart
 */
void drawsetstackedhbar(gno, maxn, bsize)
    int gno, maxn;
    double bsize;
{
    int i, j, k;
    int c, l, w, p, ls;
    int cc, cy, py, ly, wy;
    double *x, *y;
    double tmpx[4];
    double tmpy[4];
    double *sum = (double *) calloc(maxn, sizeof(double));

    if (sum == NULL) {
	errwin("Can't calloc in drawsetstackedbar\n");
	return;
    }
    for (i = 0; i < g[gno].maxplot; i++) {
	if (isactive(gno, i)) {
	    x = getx(gno, i);
	    y = gety(gno, i);
	    cc = g[gno].p[i].color;
	    cy = g[gno].p[i].fillcolor;
	    py = g[gno].p[i].fillpattern;
	    ly = g[gno].p[i].lines;
	    wy = g[gno].p[i].linew;
	    c = setcolor(cy);
	    p = setpattern(py);
	    l = setlinestyle(ly);
	    w = setlinewidth(wy);
	    for (j = 0; j < maxn; j++) {
		if (j < getsetlength(gno, i)) {
		    if (g[gno].p[i].fill) {
			c = setcolor(cy);
			tmpy[0] = y[j] - bsize * 0.5;
			tmpx[0] = sum[j];
			tmpy[1] = y[j] - bsize * 0.5;
			tmpx[1] = sum[j] + x[j];
			tmpy[2] = y[j] + bsize * 0.5;
			tmpx[2] = sum[j] + x[j];
			tmpy[3] = y[j] + bsize * 0.5;
			tmpx[3] = sum[j];
			if (tmpy[0] > g[gno].w.yg2) {
			    continue;
			}
			if (tmpy[2] < g[gno].w.yg1) {
			    continue;
			}
			for (k = 0; k < 4; k++) {
			    if (tmpy[k] < g[gno].w.yg1)
				tmpy[k] = g[gno].w.yg1;
			    else if (tmpy[k] > g[gno].w.yg2)
				tmpy[k] = g[gno].w.yg2;
			    if (tmpx[k] < g[gno].w.xg1)
				tmpx[k] = g[gno].w.xg1;
			    else if (tmpx[k] > g[gno].w.xg2)
				tmpx[k] = g[gno].w.xg2;
			}
			if (g[gno].p[i].fillusing == PATTERN) {
			    fillrectpat(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
			} else if (g[gno].p[i].fillusing == COLOR) {
			    fillrectcolor(tmpx[0], tmpy[0], tmpx[2], tmpy[2]);
			}
		    }
		    if (ly && wy) {
			tmpy[0] = y[j] - bsize * 0.5;
			tmpx[0] = sum[j];
			tmpy[1] = y[j] - bsize * 0.5;
			tmpx[1] = sum[j] + x[j];
			tmpy[2] = y[j] + bsize * 0.5;
			tmpx[2] = sum[j] + x[j];
			tmpy[3] = y[j] + bsize * 0.5;
			tmpx[3] = sum[j];
			setcolor(cc);
			my_move2(tmpx[0], tmpy[0]);
			for (k = 0; k < 4; k++) {
			    my_draw2(tmpx[(k + 1) % 4], tmpy[(k + 1) % 4]);
			}
		    }
		    sum[j] += x[j];
		}
	    }
	}
    }
    cfree(sum);
}

/*
 * draw a set with error bars
 */
void drawseterrbar(gno, setno, offsx, offsy)
    int gno, setno;
    double offsx, offsy;

{
    int i, n = getsetlength(gno, setno);
    double *x = getx(gno, setno), *y = gety(gno, setno);
    double *dx, *dy;
    double ebarlen = g[gno].p[setno].errbarper;
    int etype = g[gno].p[setno].errbarxy;
    int c, w, l;
    int cy = g[gno].p[setno].color;
    int wy = g[gno].p[setno].linew;
    int ly = g[gno].p[setno].lines;

    c = setcolor(cy);
    l = setlinestyle(ly);
    w = setlinewidth(wy);

    switch (dataset_type(gno, setno)) {
    case XYDX:
    case XYDY:
	dx = getcol(gno, setno, 2);
	dy = getcol(gno, setno, 2);
	break;
    case XYDXDX:
    case XYDYDY:
    case XYDXDY:
	dx = getcol(gno, setno, 2);
	dy = getcol(gno, setno, 3);
	break;
    }

/*
 * draw the riser
 */
    if (g[gno].p[setno].errbar_riser == ON) {
	setlinestyle(g[gno].p[setno].errbar_riser_lines);
	setlinewidth(g[gno].p[setno].errbar_riser_linew);
	for (i = 0; i < n; i++) {
	    switch (dataset_type(gno, setno)) {
	    case XYDY:
	    case XYDYDY:
		switch (etype) {
		case BOTH:
		    my_move2(x[i] - offsx, y[i] - dy[i]);
		    my_draw2(x[i] - offsx, y[i] + dx[i]);
		    break;
		case TOP:
		    my_move2(x[i] - offsx, y[i]);
		    my_draw2(x[i] - offsx, y[i] + dx[i]);
		    break;
		case BOTTOM:
		    my_move2(x[i] - offsx, y[i] - dy[i]);
		    my_draw2(x[i] - offsx, y[i]);
		    break;
		}
		break;
	    case XYDX:
	    case XYDXDX:
		switch (etype) {
		case BOTH:
		    my_move2(x[i] - dy[i], y[i] - offsy);
		    my_draw2(x[i] + dx[i], y[i] - offsy);
		    break;
		case LEFT:
		    my_move2(x[i] - dy[i], y[i] - offsy);
		    my_draw2(x[i], y[i] - offsy);
		    break;
		case RIGHT:
		    my_move2(x[i], y[i] - offsy);
		    my_draw2(x[i] + dx[i], y[i] - offsy);
		    break;
		}
		break;
	    case XYDXDY:
		switch (etype) {
		case BOTH:
		    my_move2(x[i] - dx[i], y[i] - offsy);
		    my_draw2(x[i] + dx[i], y[i] - offsy);
		    my_move2(x[i] - offsx, y[i] - dy[i]);
		    my_draw2(x[i] - offsx, y[i] + dy[i]);
		    break;
		case LEFT:
		    my_move2(x[i] - dx[i], y[i] - offsy);
		    my_draw2(x[i], y[i] - offsy);
		    my_move2(x[i] - offsx, y[i] - dy[i]);
		    my_draw2(x[i] - offsx, y[i]);
		    break;
		case RIGHT:
		    my_move2(x[i] + dx[i], y[i] - offsy);
		    my_draw2(x[i], y[i] - offsy);
		    my_move2(x[i] - offsx, y[i] + dy[i]);
		    my_draw2(x[i] - offsx, y[i]);
		    break;
		}
		break;
	    }
	}
    }
/*
 * draw the bar
 */
    setlinestyle(g[gno].p[setno].errbar_lines);
    setlinewidth(g[gno].p[setno].errbar_linew);
    for (i = 0; i < n; i++) {
	switch (dataset_type(gno, setno)) {
	case XYDY:
	case XYDYDY:
	    switch (etype) {
	    case BOTH:
		errorbar(x[i] - offsx, y[i] - dy[i], ebarlen, 1);
		errorbar(x[i] - offsx, y[i] + dx[i], ebarlen, 1);
		break;
	    case TOP:
		errorbar(x[i] - offsx, y[i] + dx[i], ebarlen, 1);
		break;
	    case BOTTOM:
		errorbar(x[i] - offsx, y[i] - dy[i], ebarlen, 1);
		break;
	    }
	    break;
	case XYDX:
	case XYDXDX:
	    switch (etype) {
	    case BOTH:
		errorbar(x[i] - dy[i], y[i] - offsy, ebarlen, 0);
		errorbar(x[i] + dx[i], y[i] - offsy, ebarlen, 0);
		break;
	    case LEFT:
		errorbar(x[i] - dy[i], y[i] - offsy, ebarlen, 0);
		break;
	    case RIGHT:
		errorbar(x[i] + dx[i], y[i] - offsy, ebarlen, 0);
		break;
	    }
	    break;
	case XYDXDY:
	    switch (etype) {
	    case BOTH:
		errorbar(x[i] + dx[i], y[i] - offsy, ebarlen, 0);
		errorbar(x[i] - dx[i], y[i] - offsy, ebarlen, 0);
		errorbar(x[i] - offsx, y[i] - dy[i], ebarlen, 1);
		errorbar(x[i] - offsx, y[i] + dy[i], ebarlen, 1);
		break;
	    case LEFT:
		errorbar(x[i] - dx[i], y[i] - offsy, ebarlen, 0);
		errorbar(x[i] - offsx, y[i] - dy[i], ebarlen, 1);
		break;
	    case RIGHT:
		errorbar(x[i] + dx[i], y[i] - offsy, ebarlen, 0);
		errorbar(x[i] - offsx, y[i] + dy[i], ebarlen, 1);
		break;
	    }
	    break;
	}
    }
    setlinewidth(w);
    setlinestyle(l);
    setcolor(c);
}

void set_timestamp()
{
    struct tm tm, *localtime();
    long time_value;
    char *str;

    (void) time(&time_value);
    tm = *localtime(&time_value);
    str = asctime(&tm);
    strcpy(timestamp.s, str);
    timestamp.s[strlen(timestamp.s) - 1] = 0;
}
