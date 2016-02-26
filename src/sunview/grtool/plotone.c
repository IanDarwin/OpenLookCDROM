/*
	plotone.c - entry for graphics

	$Header: plotone.c,v 1.11 89/09/02 10:07:48 pturner Locked $
*/

#include <stdio.h>
#include "defines.h"
#include "globals.h"
#include "objdefs.h"

plotstr legstr[MAXPLOT];

extern double errbarper;

extern plotstr pstr[];

int ylabdis;

plotone()
{
    int i, sy, ly, cy, onintr();
    double xtmp1, xtmp2, ytmp1, ytmp2;
    double *getx(), *gety();

    setclipping(TRUE);
    ylabdis = 0;
    if (hardcopyflag)
	device = hdevice;
    else
	device = tdevice;
    initgraphics(device);
    defineworld(xg1, yg1, xg2, yg2);
    viewport(xv1, yv1, xv2, yv2);
    setlinestyle(defline);
    setclipping(0);
    for (i = 0; i < MAXLINES; i++) {
	if (isactive_line(i)) {
	    setcolor(lines[i].color);
	    draw_arrow(lines[i].x1, lines[i].y1, lines[i].x2, lines[i].y2, lines[i].arrow);
	}
    }
    for (i = 0; i < MAXBOXES; i++) {
	if (isactive_box(i)) {
	    setcolor(boxes[i].color);
	    move2(boxes[i].x1, boxes[i].y1);
	    draw2(boxes[i].x1, boxes[i].y2);
	    draw2(boxes[i].x2, boxes[i].y2);
	    draw2(boxes[i].x2, boxes[i].y1);
	    draw2(boxes[i].x1, boxes[i].y1);
	}
    }
    setcolor(1);
    setclipping(1);
    if (boxon)
	boxplot();
    setfont(curfont);
    drawtics();
    drawlabels(xlabel, ylabel, title, stitle);
    for (i = 0; i < MAXSTR; i++) {
	if (strlen(pstr[i].s)) {
	    setfont(pstr[i].font);
	    if (pstr[i].size > 0) {
		setcharsize(pstr[i].size);
		writestr(pstr[i].x, pstr[i].y, pstr[i].rot, pstr[i].s);
		setcharsize(1.0 / pstr[i].size);
	    }
	}
    }
    setfont(curfont);
    for (i = 0; i < maxplot; i++) {
	int j;
	double *x, *y;
	char s[20];

	if (isactive(i)) {
	    if (iserrbar(i) < 0) {
		sy = getsetplotsym(i);
		ly = getsetlinesym(i);
		cy = getsetcolor(i);
		/*
		 * if (sy == MAXSYM || cy == 0) continue;
		 */
		setcolor(cy);
		if (ly) {
		    setlinestyle(ly);
		    drawpoly(getx(i), gety(i), getsetlength(i));
		    setlinestyle(defline);
		}
		if (sy < MAXSYM) {
		    drawpolysym(getx(i), gety(i), getsetlength(i), sy);
		} else {
		    switch (sy) {
		    case MAXSYM + 2:
			getsetminmax(i, &xtmp1, &xtmp2, &ytmp1, &ytmp2);
			rect(xtmp1, ytmp1, xtmp2, ytmp2);
			break;
		    case MAXSYM + 1:
		    case MAXSYM:
			x = getx(i);
			y = gety(i);
			for (j = 0; j < getsetlength(i); j++) {
			    if (symok(x[j], y[j])) {
				if (sy == MAXSYM) {
				    sprintf(s, "%d", i);
				} else {
				    sprintf(s, "(%d,%d)", i, j + 1);
				}
				writestr(x[j], y[j], 0, s);
			    }
			}
			break;
		    }
		}
	    } else {
		int setn;

		setn = iserrbar(i);
		if (isactive(setn)) {
		    cy = getsetcolor(setn);
		    setcolor(cy);
		    drawerrbar(getx(setn), gety(setn), getsetlength(setn), getx(i), gety(i),
			       getsetlength(i), getseterrbarxy(i), errbarper);
		}
	    }
	}
    }
    setcolor(1);
    setlinestyle(defline);
    if (legendflag)
	dolegend();
    leavegraphics();
    device = tdevice;
}

dolegend()
{

    int i, j = 0, sy, cy, ly;
    for (i = 0; i < maxplot; i++) {
	if (isactive(i) && (legstr[i].s[0]) && (iserrbar(i) < 0)) {
	    sy = getsetplotsym(i);
	    ly = getsetlinesym(i);
	    cy = getsetcolor(i);
	    putlegend(j, llen, lgap, 1.0, legx, legy, sy, ly, cy, legstr[i].s);
	    j++;
	}
    }
}

drawtics()
{
    if (xticflag) {
	if (xgridflag)
	    drawxgrid(xg1, xg2, yg1, yg2, xt1);
	else
	    xmajor(xg1, xg2, yg1, yg2, xt1, (boxflag & xticopflag), xticinoutflag);
	if (xticslog)
	    xminorlog(xg1, xg2, yg1, yg2, xt2, (boxflag & xticopflag), xticinoutflag);
	else
	    xminor(xg1, xg2, yg1, yg2, xt2, (boxflag & xticopflag), xticinoutflag);
    }
    if (xticlflag) {
	xlabels(xg1, xg2, xtopflag ? yg2 : yg1, xt1, xform, fformx, xticslog, xabsflag, xticangle, xtopflag);
    }
    if (yticflag) {
	if (ygridflag)
	    drawygrid(yg1, yg2, xg1, xg2, yt1);
	else
	    ymajor(yg1, yg2, xg1, xg2, yt1, (boxflag & yticopflag), yticinoutflag);
	if (yticslog)
	    yminorlog(yg1, yg2, xg1, xg2, yt2, (boxflag & yticopflag), yticinoutflag);
	else
	    yminor(yg1, yg2, xg1, xg2, yt2, (boxflag & yticopflag), yticinoutflag);
    }
    if (yticlflag)
	ylabels(yg1, yg2, ytopflag ? xg2 : xg1, yt1, yform, fformy, yticslog, yabsflag, ytopflag);
    if ((yg1 < 0.0) && (yg2 > 0.0)) {
	if (xzflag) {
	    drawzerox();
	    if (xztflag) {
		drawxzerotics(xt1, xt2);
	    }
	}
    }
    if ((xg1 < 0.0) && (xg2 > 0.0)) {
	if (yzflag) {
	    drawzeroy();
	    if (yztflag) {
		drawyzerotics(yt1, yt2);
	    }
	}
    }
}

boxplot()
{
    if (boxflag) {
	rect(xg1, yg1, xg2, yg2);
    } else {
	move2(xg1, yg1);
	draw2(xg2, yg1);
	move2(xg1, yg1);
	draw2(xg1, yg2);
    }
}
