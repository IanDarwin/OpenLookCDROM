#include "surfmodl.h"

void initial(argc,argv)
int *argc;
char **argv;
{
      FILE * Infile;                 /* name of data input file */
      int i;


  Xeye = 100.0;
  Yeye = -70.0;
  Zeye = 75.0;
  Xfocal = 0.0;
  Yfocal = 0.0;
  Zfocal = 0.0;
  Nlite = 1;
  Xlite[0] = 100.0;
  Ylite[0] = 30.0;
  Zlite[0] = 0.0;
  Intensity[0] = 0.7;
  Magnify = 1.0;
  Viewtype = 0;
  Interpolate = FALSE;
  Epsilon = 0.3;
  Shadowing = FALSE;
  XYadjust = 1.0;
  Showaxes = 0;
  Xaxislen = 0.0;
  Yaxislen = 0.0;
  Zaxislen = 0.0;
  Axiscolor = 1;
  Nwindow = 1;
  Mono = TRUE;
  Gxmax = 1000;
  Gymax = 1000;
  display = TRUE;
  drawing_style = DRAWING_STYLE_HIDDEN;
  Zcutfar = 1.e6;
  System = 1;
#ifdef STDCGA
  System = 1;              /* IBM PC */
  Ngraphchar = 40;
  Gxmin = 5;
  Gxmax = 315;
  Gymin = 5;
  Gymax = 195;
  Ncolors = 3;
#endif
#ifdef SANYOIBM
  System = 2;              /* Sanyo MBC-555 Hi-Res */
  Ngraphchar = 80;
  if ( (System == 1) || (System == 3) ) {
    Gxmin  = 5;
    Gxmax  = 315;
    Gxmin  = 5;
    Gxmax  = 195;
    Ncolors = 3;
  }  else {
    Gxmin = 10;
    Gxmax = 630;
    Gymin = 5;
    Gymax = 195;
    Ncolors = 7;
  }
#endif
#ifdef TOOLBOX
/* Initialize the graphics first */
  initgraphic();
  leavegraphic(); 
  System = 1;
  Ngraphchar = 0;
  Gxmin = 10;
  Gymin = 5;
  Gxmax = Xscreenmaxglb - 10;
  Gymax = Ymaxglb - 5;
/* Assume only one available color, unless otherwise noted */
  Ncolors = 1;
#endif
#ifdef TBCGA
  Ncolors = 3;
#endif
#ifdef HERCULES
  Ncolors = 1;
#endif
#ifdef EGA
  Ncolors = 15;
#endif
#ifdef Z100
  Ncolors = 7;
#endif
#ifdef IBM3270
  Ncolors = 15;
#endif
#ifdef ATT
  Ncolors = 15;
#endif
  initialize();

  /*Inifile[0] = ' '; */

/* Set the 'last' values */
  Xelast = 0.0;
  Yelast = 0.0;
  Zelast = 0.0;
  Xflast = 0.0;
  Yflast = 0.0;
  Zflast = 0.0;
  Vtlast = 0;
  Maglast = 0.0;
  Sorted = FALSE;

  Fileread = FALSE;
  switch (*argc) {
    case 2:
      readfile (argv[1]);
      break;
    case 1:
      break;
    default: 
      printf ("usage: SURFMODL [filename]\n");
      exit(1);
      break; 
  }  /*case paramcount*/
  Zcutnear = 0;
  /* move cut plane to eyepoint */

/*  clrscr(); */
}  /* procedure Initial */

