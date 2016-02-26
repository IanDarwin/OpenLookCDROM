
/*  @(#)scantool.c 1.4 90/04/04
 *
 *  Main routine for scantool, which will read a scanned image
 *  from a Microtek MS-300A scanner and convert it to a Sun rasterfile.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me, then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <strings.h>
#include "scantool.h"

char *br_strs[] = {     /* Brightness menu item strings. */
       "1   -24% Darker",
       "2   -20% Darker",
       "3   -16% Darker",
       "4   -12% Darker",
       "5   -8%  Darker",
       "6   -4%  Darker",
       "7   No adjustment",
       "8   +4%  Lighter",
       "9   +8%  Lighter",
       "10  +12% Lighter",
       "11  +16% Lighter",
       "12  +20% Lighter",
       "13  +24% Lighter",
       "14  +28% Lighter",
       NULL
} ;

char *con_strs[] = {    /* Contrast menu item strings. */
       "1   -24% Lower",
       "2   -20% Lower",
       "3   -16% Lower",
       "4   -12% Lower",
       "5   -8%  Lower",
       "6   -4%  Lower",
       "7   No adjustment",
       "8   +4%  Higher",
       "9   +8%  Higher",
       "10  +12% Higher",
       "11  +16% Higher",
       "12  +20% Higher",
       "13  +24% Higher",
       "14  +28% Higher",
       NULL
} ;

char *gr_strs[] = {      /* Grain menu item strings. */
       "0   Grain Size: 8x8   Gray Levels: 33",
       "1   Grain Size: 8x8   Gray Levels: 33",
       "2   Grain Size: 8x8   Gray Levels: 33",
       "3   Grain Size: 8x8   Gray Levels: 33",
       "4   Grain Size: 6x6   Gray Levels: 37",
       "5   Grain Size: 5x5   Gray Levels: 26",
       "6   Grain Size: 5x5   Gray Levels: 18",
       "7   Grain Size: 4x4   Gray Levels: 17",
       "8   Grain Size: 4x4   Gray Levels: 17",
       "9   Grain Size: 4x4   Gray Levels: 17",
       "10  Grain Size: 3x3   Gray Levels: 10",
       "11  Grain Size: 2x2   Gray Levels: 5",
       NULL
} ;

char *help_strs[] = {   /* Help menu item strings. */
       "Brightness",
       "Contrast",
       "Frame",
       "Grain",
       "Mode",
       "Resolution",
       "Scan",
       "Compress",
       "Port",
       "Speed",
       "Picture",
       NULL
} ;

char *res_strs[] = {     /* Resolution menu item strings. */
       "0   300 dpi  Scale: Full size",
       "1   285 dpi  Scale: 95%",
       "2   270 dpi  Scale: 90%",
       "3   255 dpi  Scale: 85%",
       "4   240 dpi  Scale: 80%",
       "5   225 dpi  Scale: 75%",
       "6   210 dpi  Scale: 70%",
       "7   200 dpi  Scale: 66%",
       "8   180 dpi  Scale: 60%",
       "9   165 dpi  Scale: 55%",
       "10  150 dpi  Scale: 50%",
       "11  135 dpi  Scale: 45%",
       "12  120 dpi  Scale: 40%",
       "13  100 dpi  Scale: 33%",
       "14   90 dpi  Scale: 30%",
       "15   75 dpi  Scale: 25%",
       NULL
} ;

char *set_strs[] = {      /* Set menu item strings. */
       "Picture Name",
       "Default Settings",
       "Clear Scanning Frame",
       "New Scanning Frame",
       "Show Current Settings",
       NULL
} ;

char helpname[MAXLINE] ;      /* Current help file to display. */
char last_message[MAXLINE] ;  /* Last message that was make_displayed. */
char old_picname[MAXLINE] ;   /* Previous picture name. */
char output[MAXLINE] ;        /* Panel_set string to be output. */
char picname[MAXLINE] ;       /* Name of file for raster image. */
char progname[MAXLINE] ;      /* Name of this program. */

int ops[MAXOPS] ;             /* Rasterop functions. */

int switches[4] =
      {
        0,                    /* Mode (Line Art). */
        1,                    /* Data Transfer (Compressed). */
        0,                    /* Serial Port (A). */
        1                     /* Baud Rate (19200). */
      } ;

int framevals[4] =            /* Initial frame in 1/8th inch intervals. */
      {
        16,                   /* X1. */
        16,                   /* Y1. */
        48,                   /* X2. */
        48,                   /* Y2. */
     } ;

int butx ;                    /* Top left X position of popup button. */
int buty ;                    /* Top left Y position of popup button. */
int cur_ch ;                  /* Last keyboard character pressed. */
int curx ;                    /* Current X position of the mouse. */
int cury ;                    /* Current Y position of the mouse. */
int font_width ;              /* Width of a single character. */
int nohelp = 0 ;              /* Indicates if a help file was found. */
int pid ;                     /* Process id of the child scan process. */
int scanning = 0 ;            /* Set if we are in the middle of a scan. */
int showing = 0 ;             /* Set if we are in the middle of a show. */
int type ;                    /* Current pseudo event type. */
int width ;                   /* Width in pixels of scantool window. */

/* Cyclic switches. */
int brightness ;              /* Brightness value. */
int contrast ;                /* Contrast value. */
int grain ;                   /* Grain value. */
int resolution ;              /* Resolution value. */

enum draw_type drawstate ;    /* Current action if a redraw is needed. */

FILE *hfd ;                   /* File descriptor for help file. */


main(argc, argv)
int argc ;
char *argv[] ;
{
  STRCPY(progname, argv[0]) ;      /* Save program name. */
  get_options(argc, argv) ;        /* Extract command line options. */
  init_ws_type() ;                 /* Setup pseudo rasterop functions. */
  drawstate = DO_NOTHING ;         /* No popups currently displayed. */
  if ((hfd = fopen(HELPNAME, "r")) == NULL)
    {
      FPRINTF(stderr, "Help file %s not found\r\n", HELPNAME) ;
      nohelp = 1 ;
    }

  STRCPY(picname, "/home1/falcon/gunnar/scan.rf") ;
  set_defaults() ;                /* Set scanner settings to default. */
  make_frames(argc, argv) ;  /* Create main scantool frame and show window. */
  make_subframes() ;         /* Create canvases for each window. */
  init_fonts() ;             /* Open fonts used by scantool. */
  make_menus() ;             /* Create scantool menubar pulldown menus. */
  start_tool() ;             /* Fit window and wait for events. */
}
