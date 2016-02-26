
#include <stdio.h>
/* Global variables and constants for SURFMODL */

/* CONST */ 
#define DRAWING_STYLE_VECTORS 1
#define DRAWING_STYLE_HIDDEN 2
#define DRAWING_STYLE_SURFACE 3
#define QUIT 6

#define 	TRUE 1
#define 	FALSE 0
#define  	MAXNODES    1024    /* maximum # of nodes in the entire solid */
#define  	MAXCONNECT  4096 /* maximum # of connections in entire solid */
#define  	MAXSURF     1365 /* maximum # of surfaces in entire solid */
                            /* (MAXSURF = MAXCONNECT / 3) */
#define      MAXMATL  30         /* maximum # of materials in entire solid */
#define      MAXPTS   1600        /* maximum # of line points (in fillsurf) */
#define      MAXVAR   20         /* maximum # of numeric inputs on a line */
#define      MAXLITE  20         /* maximum # of light sources */

#define boolean int

double rint();
double floor();

#define round(x) (int)(rint(x))
#define trunc(x) (int)(floor(x))
#define sqr(x) ((x)*(x))

typedef int points;		/*[MAXPTS+1];*/
typedef double realpts;/* [MAXPTS+1];*/
typedef char text80;	/* [80]; */
typedef double vartype;	/* [MAXVAR]; */
typedef double surfaces;	/* [MAXSURF]; */
typedef double vector;	/* [3]; */
typedef double nodearray;  /*[MAXNODES] */

      nodearray *Xworld;
      nodearray *Yworld;
      nodearray *Zworld;
	 /* world coordinates of each node */
      nodearray *Xtran;
      nodearray *Ytran;
      nodearray *Ztran;
        /* transformed coordinates of each node */
      int **Connect;
        /* surface connectivity data */
      int *Nvert;
        /* # vertices per surface */
      int *Matl;
        /* material number of each surface */
      double R1[MAXMATL];
      double R2[MAXMATL];
      double R3[MAXMATL];
        /* material reflectivity constants */
      double Color[MAXMATL];
        /* material color 0 = red .33 = blue .66 = green */
      double Saturation[MAXMATL];
	/* color saturation: 0 = grey, 1= full color */
      double Ambient[MAXMATL];
        /* ambient light intensity for each material */
      double Xlite[MAXLITE];
      double Ylite[MAXLITE];
      double Zlite[MAXLITE];
        /* coords of light sources */
      double Intensity[MAXLITE];
        /* light source intensities */

      surfaces *Surfmin, *Surfmax;
      char *Flpurpose;              /* title for plot */
      double Xeye;              /* coords of eye */
      double Yeye;              /* coords of eye */
      double Zeye;              /* coords of eye */
      double Zphi;
      double Ztheta;
      double Xfocal;        /* coords of focal point */
      double Yfocal;        /* coords of focal point */
      double Zfocal;        /* coords of focal point */
      int Maxvert;                    /* max # vertices per surface */
      int Nsurf;                      /* # surfaces */
      int Nnodes;                     /* # nodes */
      int Nlite;                      /* # light sources */
      double Magnify;                       /* magnification factor */
      int Viewtype;                   /* code for viewing type: */
                                           /* 0=perspective, 1=XY, 2=XZ, 3=YZ */
      boolean Fileread;                   /* flag first file read */
      int Nmatl;                      /* number of materials */
      int Gxmin; /* graphics screen limits */
      int Gxmax; /* graphics screen limits */
      int Gymin; /* graphics screen limits */
      int Gymax; /* graphics screen limits */
      int System;                     /* computer being used (usually 1) */
      int Nsides;                     /* #sides of surface used (1 or 2)*/
      boolean Interpolate;                /* flag for Gouraud interpolation */
      double Epsilon;                       /* Gouraud interpolation range */
      boolean Shadowing;                  /* flag shadowing option */
      text80 *Inifile;                     /* name of INI file */
      double XYadjust;                      /* factor for screen width */
      int Ngraphchar;                 /* #chars across graphics screen*/
      int Showaxes;                   /* code to show (0) no axes; (1) */
                                           /* axis directions; (2) full axes */
      double Xaxislen;    /* lengths of axes */
      double Yaxislen;    /* lengths of axes */
      double Zaxislen;    /* lengths of axes */
      double Axiscolor;                  /* color to draw axes */
      int Nwindow;                    /* # graphics windows on screen */
      int Ncolors;                    /* #colors supported on computer */
      boolean Mono;                       /* Is picture to be displayed on */
                                           /* monochrome monitor? */
      int paramcount;

      double Xelast, Yelast,Zelast ;        /* Last eye coords */
      double Xflast, Yflast, Zflast;        /* Last focal point coords */
      double Xfotran, Yfotran, Zfotran;     /* Transformed focal point */
      double XYmax;			    /* Max coordinate */
      int Vtlast;                           /* last view type */
      double Maglast;                       /* last magnification */
      boolean Sorted;                       /* Is it still sorted ? */
      FILE *Infile;
      int display;
      int drawing_style;
      double Zcutfar, Zcutnear;	/* Min and Max clip plane  */
      double Ztranfar, Ztrannear;	/* Transformed clip planes */
      double Zfarlast, Znearlast;	/* last Zcut params	*/

      int Xmin, Ymin, Xmax, Ymax;
