/*
  driver for sunview

	$Header: sunview.c,v 1.5 89/09/02 10:08:09 pturner Locked $

*/

#include <stdio.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <pixrect/pixrect.h>
#include <pixrect/pr_line.h>
#include "externs.h"

double devtoworldx(), devtoworldy();

#define MINCOLOR 0
#define MAXCOLOR 9

static int sviewcolor = 1;
static int sviewdmode;
static int sviewfont = 0;
static int sviewlinestyle=1;

extern double devcharsize;
extern double charsize;

double sviewcharsize = 0.8;

double xconv(), yconv();

void drawsview();

int win_h, win_w;

static Pr_brush b = {1};
static short int mypat1[2] = {1,0};
static short int mypat2[3] = {2,10,0};
static short int mypat3[3] = {10,10,0};
static short int mypat4[3] = {15,15,0};
static short int mypat5[5] = {5,2,15,2,0};
static short int mypat6[3] = {20,20,0};
static short int mypat7[3] = {25,25,0};
static short int *patterns[] = {mypat1,mypat2,mypat3,mypat4,mypat5,mypat6,mypat7};
static Pr_texture tex;


#define PIX_XOR PIX_SRC^PIX_DST

extern Canvas canvas;
extern Pixwin *pw;

static void sunviewinit()
{
    win_h = (int) window_get(canvas, CANVAS_HEIGHT);
    win_w = (int) window_get(canvas, CANVAS_WIDTH);
    pw_writebackground(pw, 0, 0, win_w, win_h, 0);
}

void putstrsview(s)
    char *s;
{
}

void sviewsetmode(mode)
    int mode;
{
    switch (mode) {
    case 1:
	sunviewinit();
	break;
    case 2:
	break;
    }
}

static int x1, y1;

void drawsview(x, y, mode)
    int x, y, mode;
{
    if (mode){
	if (sviewcolor==1&&sviewlinestyle==1) 
	    pw_vector(pw, x1, win_h - y1, x, win_h - y, PIX_SRC, 1);
	else 
	    pw_line(pw, x1, win_h - y1, x, win_h- y, &b, &tex, PIX_SET);
    }
    x1 = x;
    y1 = y;
}

int xconvsview(x)
    double x;
{
    return ((int) (win_w * xconv(x)));
}

int yconvsview(y)
    double y;
{
    return ((int) (win_h * yconv(y)));
}

void sviewsetfont(n)
    int n;
{
    hselectfont(sviewfont = n);
}

int sviewsetcolor(c)
    int c;
{
    if (c) {
	c = c % MAXCOLOR;
    }
    b.width = c;
    sviewcolor = c;

    return c;
}

void sviewdrawtic(x, y, dir, updown)
    int x, y, dir, updown;
{
    switch (dir) {
    case 0:
	switch (updown) {
	case 0:
	    drawsview(x, y, 0);
	    drawsview(x, y + devxticl, 1);
	    break;
	case 1:
	    drawsview(x, y, 0);
	    drawsview(x, y - devxticl, 1);
	    break;
	}
	break;
    case 1:
	switch (updown) {
	case 0:
	    drawsview(x, y, 0);
	    drawsview(x + devyticl, y, 1);
	    break;
	case 1:
	    drawsview(x, y, 0);
	    drawsview(x - devyticl, y, 1);
	    break;
	}
	break;
    }
}

void dispstrsview(x, y, rot, s)
    int x, y, rot;
    char *s;
{
    puthersh(x, y, sviewcharsize * charsize, rot, sviewcolor, vector, s);
}

int sviewsetlinestyle(style)
    int style;
{
    if (style)
	tex.pattern = patterns[style-1];	/* Which pattern to use */
    return (sviewlinestyle = style);
}

void sviewleavegraphics()
{
    sviewsetmode(2);
}

void sviewinitgraphics(dmode)
{
int i=0;
    sviewdmode = dmode;
    sviewsetmode(1);
    devconvx = xconvsview;
    devconvy = yconvsview;
    vector = drawsview;
    devwritestr = dispstrsview;
    devsetcolor = sviewsetcolor;
    devsetfont = sviewsetfont;
    devsetline = sviewsetlinestyle;
    devdrawtic = sviewdrawtic;
    devleavegraphics = sviewleavegraphics;
    devxticl = 15;
    devyticl = 15;
    devcharsize = sviewcharsize;
    sviewsetcolor(1);
    sviewsetlinestyle(1);
}
