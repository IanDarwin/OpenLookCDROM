/*
  driver for postscript printer

courtesy of:

Jim Hudgens
hudgens@nu.cs.fsu.edu
hudgens@ray.met.fsu.edu

	$Header: ps.c,v 1.2 89/08/19 09:23:57 pturner Locked $

*/

#include <stdio.h>
#include "externs.h"

extern double charsize;
extern double devcharsize;

/* postscript page at scale = 0.25 */

#define PSXMIN 150
#define PSXMAX 2200
#define PSYMIN 150
#define PSYMAX 3200
#define DXPS 2050
#define DYPS 3050
#define CHARS 1.5

#define PSXMINP 150
#define PSXMAXP 2200
#define PSYMINP 550
#define PSYMAXP 2700
#define DXPSP 2050
#define DYPSP 2050
#define CHARSP 1.25

#define MINCOLOR 0
#define MAXCOLOR 9

#define MAXLINESTYLE 14

static int psxmin=PSXMIN;
static int psxmax=PSXMAX;
static int psymin=PSYMIN;
static int psymax=PSYMAX;
static int psdx=DXPS;
static int psdy=DYPS;

static int pscolor;
static int psdmode;
static int psfont=0;
static double pscharsize=1.5;
static int pslinestyle;

double xconv(),yconv();

static FILE *psout;

void putstrps(s)
char *s;
{
	fprintf(psout,s);
}

static char *fname;

static int orientflag=0;

void pssetmode(mode)
int mode;
{
	char tbuf[128];
	char *mktemp();

	if (mode%2) {
		fname=mktemp("/usr/tmp/XXXXXX");
		psout=fopen(fname,"w");
	}
	switch (mode) {
	case 1 : /* PS landscape */
		orientflag=1;
		pscharsize=CHARS;		
		psxmin=PSXMIN;
		psxmax=PSXMAX;
		psymin=PSYMIN;
		psymax=PSYMAX;
		psdx=DXPS;
		psdy=DYPS;
		break;
	case 3 : /* PS portrait */
                orientflag = 0;
		pscharsize=CHARSP;		
		psxmin = PSXMINP;
		psxmax = PSXMAXP;
		psymin = PSYMINP;
		psymax = PSYMAXP;
		psdx   = DXPSP;
		psdy   = DYPSP;
		break;
	case 2 :	
	case 4 :
                putstrps("stroke\n");
		putstrps("showpage\n");	
		putstrps("restore\n");	
		fclose(psout);
		sprintf(tbuf,"cp %s plot.ps",fname);
		system(tbuf);
		unlink(fname);
		orientflag=0;
		break;
	}
}

static int x1=99999,y1=99999;

#define MAXPATHLEN 500    /* 500 points in a postscript path */
                          /* between strokes */
/* note that this is an very conservative estimate.  datasets
   of the form: newpath x0 y0 moveto x1 y1 lineto .... xN yN lineto 
   are observed to overflow around N=2000 to 3000.  Solved by using
       .... xM yM lineto stroke xM yM moveto xM1 yM1 lineto ....
   for M of 500.  
 */
/* MUST make sure that the path is stroked, though... */
/* stroke when: 
      finishing
      changing line styles
      changing line colors
   as well as when going pen up.
 */

void drawps(x2,y2,mode)
int x2,y2,mode;
{
   static int pathlength=0;
   char stmp[30];
   int xtmp,ytmp;
   
   
   if (orientflag) {
       xtmp=y2;
       ytmp=(-x2)+psymax;
       
       if (mode) {
	   if (pathlength > MAXPATHLEN )
	     /* stroke, moveto, lineto */
	       {
		  sprintf(stmp,"k %d %d m %d %d l\n",
			  x1,y1, xtmp,ytmp);
		  pathlength = 0;
	       }
	   else       /* lineto */
	       {
		  
		  sprintf(stmp,"  %d %d l\n",  xtmp,ytmp);
		  pathlength ++;
	       }
		    
	   putstrps(stmp);
	   
	}
       else {
	   if (!(x1==xtmp&&y1==ytmp)) {
	       /* stroke,  moveto */
	       sprintf(stmp,"k %d %d m\n",xtmp,ytmp);
	       putstrps(stmp);
	       pathlength = 0;
	       
	    }
	}
       x1=xtmp;
       y1=ytmp;
    }
   else {
       if (mode) {
	   if (pathlength > MAXPATHLEN)
	       {
		  /* stroke, moveto, lineto */
		  sprintf(stmp," k %d %d m %d %d l\n",
			  x1,y1, x2,y2);
		  pathlength = 0;
	       }
	   else       /* lineto */
	       {
		  pathlength++;
		  sprintf(stmp," %d %d l\n",x2,y2);
	       }
	   putstrps(stmp);
	}
       else {
	   if (!(x1==x2&&y1==y2)) {
	       /* stroke, moveto */
	       sprintf(stmp," k %d %d m\n",x2,y2);
	       putstrps(stmp);
	       pathlength = 0;
	    }
	}
       x1=x2;
       y1=y2;
    }
}

int xconvps(x)
double x;
{
	if (orientflag) {
		return((int)(psymin + psdy*xconv(x)));
	}
	else {
		return((int)(psxmin + psdx*xconv(x)));
	}
}

int yconvps(y)
double y;
{
	if (orientflag) {
		return((int)(psxmin + psdx*yconv(y)));
	}
	else {
		return((int)(psymin + psdy*yconv(y)));
	}
}

void pssetfont(n)
int n;
{
	hselectfont(psfont=n);
}

int pssetcolor(c)
int c;
{
   char stmp[50];
   
   if (c) {
       c = c % MAXCOLOR;
       sprintf(stmp,"k %d w\n",2*(c-1)+1);
       putstrps(stmp);
    }
   pscolor = c;
   return c;
}

void psdrawtic(x,y,dir,updown)
int x,y,dir,updown;
{
	switch (dir) {
	case 0:
		switch (updown) {
		case 0:
			drawps(x,y,0);
			drawps(x,y+devxticl,1);
			break;
		case 1:
			drawps(x,y,0);
			drawps(x,y-devxticl,1);
			break;
		}
		break;
	case 1:
		switch (updown) {
		case 0:
			drawps(x,y,0);
			drawps(x+devyticl,y,1);
			break;
		case 1:
			drawps(x,y,0);
			drawps(x-devyticl,y,1);
			break;
		}
		break;
	}
}

int pssetlinestyle(style)
int style;
{
	char stmp[50];


	switch (style) {
		case 1: /* solid */
			strcpy(stmp,"k [] 0 d \n");
			break;
		case 2: /* dotted */
			strcpy(stmp,"k [3 10] 0 d\n");
			break;
		case 3: /* long dash */
			strcpy(stmp,"k [10 5] 0 d \n");
			break;
		case 4: /* short dash */
			strcpy(stmp,"k [5 5] 0 d \n");
			break;
		case 5: /* dot-dashed */
			strcpy(stmp,"k [10 5 3 5] 0 d\n");
			break;

	}
     	putstrps(stmp);
	return (pslinestyle = style);
}

void dispstrps(x,y,rot,s)
int x,y,rot;
char *s;
{
	puthersh(x,y,pscharsize*charsize,rot,pscolor,vector,s);
}

void psleavegraphics()
{
	pssetmode(psdmode+1);
}


/*           postscript initialization routine  */
psinitgraphics(dmode)
int dmode;
{
	psdmode=dmode;
	pssetmode(psdmode);
	devconvx=xconvps;
	devconvy=yconvps;
	vector=drawps;
	devwritestr=dispstrps;
	devsetcolor=pssetcolor;
	devsetfont=pssetfont;
	devsetline=pssetlinestyle;
	devdrawtic=psdrawtic;
	devleavegraphics=psleavegraphics;
	devcharsize=pscharsize;
	devxticl=20;
	devyticl=20;

	putstrps("%%!PS\n");
	putstrps("/w{setlinewidth}bind def\n");
	putstrps("/m{moveto} bind def\n");
	putstrps("/l{lineto}bind def\n");	
	putstrps("/k{stroke} bind def\n");
	putstrps("/d{setdash} bind def\n");

	putstrps("save\n");	
	putstrps(".25 .25 scale\n");	
	putstrps("1 setlinecap\n");	
	putstrps("1 w\n");
	setfont(2);
	setcolor(1);
	setlinestyle(0);
       
 
}
