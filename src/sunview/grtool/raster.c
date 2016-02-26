/*
  driver for raster files

	$Header: raster.c,v 1.2 89/09/12 05:10:55 pturner Locked $

*/

#include <stdio.h>
#include <pixrect/pixrect_hs.h>
#include <pixrect/pixrect.h>
#include <pixrect/pr_line.h>
#include "externs.h"

double devtoworldx(), devtoworldy();
#define LJXMIN 0
#define LJXMAX 2400
#define LJYMIN 0
#define LJYMAX 3300
#define DXLJ 2400
#define DYLJ 3300

static int rasterxmin = LJXMIN;
static int rasterxmax = LJXMAX;
static int rasterymin = LJYMIN;
static int rasterymax = LJYMAX;
static int rasterdx = DXLJ;
static int rasterdy = DYLJ;

static Pixrect *mpr;
struct rasterfile ras;
static FILE *ljout;

struct {
	int width, height, depth;
	short *bits;
}raster;

struct {
	int type, nbytes;
	char *data;
}map;


#define MINCOLOR 0
#define MAXCOLOR 9


static int rastercolor = 1;
static int rasterdmode;
static int rasterfont = 0;
static int rasterlinestyle=1;
static int orientflag = 0;

extern double devcharsize;
extern double charsize;

static double rastercharsize = 2.0;

double xconv(), yconv();

void drawraster();

static int win_h, win_w;

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

void putstrraster(s)
    char *s;
{
}

void rastersetmode(mode)
    int mode;
{
int but,len;
char segfile[128];
FILE *fp;
colormap_t *colormap=NULL;
int type=RT_BYTE_ENCODED;
int copy_flag=1;
    switch (mode) {
	case 1:
		mpr=mem_create(LJXMAX,LJYMAX,1);
		win_h=LJXMAX;
		win_w=LJYMAX;
		orientflag = 1;
		break;
	case 2:
		mpr=mem_create(LJXMAX,LJYMAX,1);
		win_w=LJXMAX;
		win_h=LJYMAX;
		orientflag = 0;
		break;
	case 3:
		printf("Enter file name to write: ");
		gets(segfile);
		fp=fopen(segfile,"w");
		pr_dump(mpr,fp,colormap,type,copy_flag);
		pr_close(mpr);
		fclose(fp);
		orientflag = 0;
		break;
	}
}

static int x1, y1;

void drawraster(x, y, mode)
    int x, y, mode;
{
    if (mode){
	if (rastercolor==1&&rasterlinestyle==1) 
	    if(!orientflag)
		pr_vector(mpr, x1, win_h - y1, x, win_h - y, PIX_SRC, 1);
	    else {
		pr_vector(mpr, y1,  x1, y, x, PIX_SRC, 1);
	    }
	else 
	    if(!orientflag)
	    	pr_line(mpr, x1, win_h - y1, x, win_h- y, &b, &tex, PIX_SET);
	    else {
	    	pr_line(mpr, y1, x1, y, x, &b, &tex, PIX_SET);
	    }
    }
    x1 = x;
    y1 = y;
}

int xconvraster(x)
    double x;
{
    return ((int) (win_w * xconv(x)));
}

int yconvraster(y)
    double y;
{
    return ((int) (win_h * yconv(y)));
}

void rastersetfont(n)
    int n;
{
    hselectfont(rasterfont = n);
}

int rastersetcolor(c)
    int c;
{
    if (c) {
	c = c % MAXCOLOR;
    }
    b.width = c;
    rastercolor = c;

    return c;
}

void rasterdrawtic(x, y, dir, updown)
    int x, y, dir, updown;
{
    switch (dir) {
    case 0:
	switch (updown) {
	case 0:
	    drawraster(x, y, 0);
	    drawraster(x, y + devxticl, 1);
	    break;
	case 1:
	    drawraster(x, y, 0);
	    drawraster(x, y - devxticl, 1);
	    break;
	}
	break;
    case 1:
	switch (updown) {
	case 0:
	    drawraster(x, y, 0);
	    drawraster(x + devyticl, y, 1);
	    break;
	case 1:
	    drawraster(x, y, 0);
	    drawraster(x - devyticl, y, 1);
	    break;
	}
	break;
    }
}

void dispstrraster(x, y, rot, s)
    int x, y, rot;
    char *s;
{
    puthersh(x, y, rastercharsize * charsize, rot, rastercolor, vector, s);
}

int rastersetlinestyle(style)
    int style;
{
    if (style)
	tex.pattern = patterns[style-1];	/* Which pattern to use */
    return (rasterlinestyle = style);
}

void rasterleavegraphics()
{
    rastersetmode(3);
}

void rasterinitgraphics(dmode)
{
    rasterdmode = dmode;
    rastersetmode(dmode);
    devconvx = xconvraster;
    devconvy = yconvraster;
    vector = drawraster;
    devwritestr = dispstrraster;
    devsetcolor = rastersetcolor;
    devsetfont = rastersetfont;
    devsetline = rastersetlinestyle;
    devdrawtic = rasterdrawtic;
    devleavegraphics = rasterleavegraphics;
    devxticl = 35;
    devyticl = 35;
    devcharsize = rastercharsize;
    rastersetcolor(1);
    rastersetlinestyle(1);
}
