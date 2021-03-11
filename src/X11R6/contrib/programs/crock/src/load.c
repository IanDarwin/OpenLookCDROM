/*
     Copyright (c) 1994    Frank Adelstein

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

#include "types.h"

/* grey stipple bitmaps */
#include "../frames/grey/grey4.xbm"
#include "../frames/grey/grey8.xbm"
#include "../frames/grey/grey16.xbm"
#include "../frames/grey/grey32.xbm"
#include "../frames/grey/grey64.xbm"
#include "../frames/grey/grey128.xbm"

/* number (0-9) bitmaps */
#include "../frames/numbers/zero.xbm"
#include "../frames/numbers/one.xbm"
#include "../frames/numbers/two.xbm"
#include "../frames/numbers/three.xbm"
#include "../frames/numbers/four.xbm"
#include "../frames/numbers/five.xbm"
#include "../frames/numbers/six.xbm"
#include "../frames/numbers/seven.xbm"
#include "../frames/numbers/eight.xbm"
#include "../frames/numbers/nine.xbm"

/* hand holding perfect score */
#include "../frames/ctest/hand.xbm"
#include "../frames/ctest/hand.m"

/*
 * Set up the X widgets and screen stuff.
 */
void XSetUp(glob, filename, playerfile, maxplayer)
Glob      *glob;
char      *filename;
char     **playerfile;
int       *maxplayer;
{
  /* widget variables */
  int cnt;
  Arg args[15];
  Display *display1, *display2;
  Widget frame1, frame2, control1, control2, thing, thing1;
  unsigned long myforeground, mybackground;

  /* config file variables */
  FILE  *fp;
  char  variable[20],     value[80];
  char  line[100], localfile[100];

  /* XGetGeometry() variables */
  Window root;
  unsigned int wwidth, wheight, wboarder;
  int wx, wy;

  /* color variables */
  XColor red, blue, exact;
  Colormap cmap;

  /* font variables */
  XFontStruct *fontstruct;

  /* translation/action routine variables */
  static XtActionsRec     actionsTable[] = {
    {"handlemove", HandleMove},
  };

  display1 = XtDisplay(glob->Toplevel1);

  /* default pixel values */
  mybackground = WhitePixel (display1, DefaultScreen(display1));
  myforeground = BlackPixel (display1, DefaultScreen(display1));

  /* create a graphics context for later use */
  glob->blackgc1 = XCreateGC (display1, XDefaultRootWindow(display1), 0, 0);
  XSetFunction(display1, glob->blackgc1, GXcopy);
  XSetGraphicsExposures(display1, glob->blackgc1, False);
  XSetForeground (display1, glob->blackgc1, myforeground);
  XSetBackground (display1, glob->blackgc1, mybackground);

  /* create another graphics context for later use */
  glob->whitegc1 = XCreateGC (display1, XDefaultRootWindow(display1), 0, 0);
  XSetFunction(display1, glob->whitegc1, GXcopy);
  XSetGraphicsExposures(display1, glob->whitegc1, False);
  XSetForeground (display1, glob->whitegc1, mybackground);
  XSetBackground (display1, glob->whitegc1, myforeground);

  /* create another graphics context for small text */
  glob->smallgc1 = XCreateGC (display1, XDefaultRootWindow(display1), 0, 0);
  XSetFunction(display1, glob->smallgc1, GXcopy);
  XSetGraphicsExposures(display1, glob->smallgc1, False);
  XSetForeground (display1, glob->smallgc1, mybackground);
  XSetBackground (display1, glob->smallgc1, myforeground);

  if (glob->numscreens == 2) {
    /* if we have a second screen */
    display2 = XtDisplay(glob->Toplevel2);

    /* default pixel values */
    mybackground = WhitePixel (display2, DefaultScreen(display2));
    myforeground = BlackPixel (display2, DefaultScreen(display2));

    /* create a graphics context for later use */
    glob->blackgc2 = XCreateGC (display2, XDefaultRootWindow(display2), 0, 0);
    XSetFunction(display2, glob->blackgc2, GXcopy);
    XSetGraphicsExposures(display2, glob->blackgc2, False);
    XSetForeground (display2, glob->blackgc2, myforeground);
    XSetBackground (display2, glob->blackgc2, mybackground);
    
    /* create another graphics context for later use */
    glob->whitegc2 = XCreateGC (display2, XDefaultRootWindow(display2), 0, 0);
    XSetFunction(display2, glob->whitegc2, GXcopy);
    XSetGraphicsExposures(display2, glob->whitegc2, False);
    XSetForeground (display2, glob->whitegc2, mybackground);
    XSetBackground (display2, glob->whitegc2, myforeground);

    /* create another graphics context for small text */
    glob->smallgc2 = XCreateGC (display2, XDefaultRootWindow(display2), 0, 0);
    XSetFunction(display2, glob->smallgc2, GXcopy);
    XSetGraphicsExposures(display2, glob->smallgc2, False);
    XSetForeground (display2, glob->smallgc2, mybackground);
    XSetBackground (display2, glob->smallgc2, myforeground);
 
  }	/* end if (numscreens == 2) */
  
  /*
   * create a frame widget to hold things 
   */
  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 2);                   cnt++;
  frame1 = XtCreateManagedWidget ("Frame", formWidgetClass, 
				 glob->Toplevel1, args, cnt);

  control1 = makecontrolpanel(glob, frame1, &glob->continue1);
  cnt = 0;
  XtSetArg (args[cnt], XtNwidth,  100);              cnt++; 
  XtSetArg (args[cnt], XtNheight, 100);              cnt++; 
  XtSetArg (args[cnt], XtNtop, XawChainTop);         cnt++; 
  XtSetArg (args[cnt], XtNbottom, XawChainTop);      cnt++; 
  (void) XtCreateWidget ("display", formWidgetClass, frame1, args, cnt);

  if (glob->numscreens == 2) {
    cnt = 0;
    XtSetArg (args[cnt], XtNborderWidth, 2);                   cnt++;
    frame2 = XtCreateManagedWidget ("Frame", formWidgetClass, 
				   glob->Toplevel2, args, cnt);
    control2 = makecontrolpanel(glob,frame2, &glob->continue2);

    XtSetArg (args[cnt], XtNwidth,  100);            cnt++; 
    XtSetArg (args[cnt], XtNheight, 100);            cnt++; 
    XtSetArg (args[cnt], XtNtop, XawChainTop);       cnt++; 
    XtSetArg (args[cnt], XtNbottom, XawChainTop);    cnt++; 
    (void) XtCreateWidget ("display", formWidgetClass, frame2, args, cnt);

  }

  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, control1);               cnt++;
  XtSetArg (args[cnt], XtNborderWidth, 2);                   cnt++;
  XtSetArg (args[cnt], XtNright, XtChainLeft);               cnt++;
  XtSetArg (args[cnt], XtNlabel, "Loading Bitmaps ...");     cnt++;
  XtSetArg (args[cnt], XtNwidth, 400);                       cnt++;
  thing = XtCreateManagedWidget ("label", labelWidgetClass, frame1, 
				 args, cnt);

  thing1 = XtCreateManagedWidget("donetherm", formWidgetClass, frame1, 
				NULL, 0);
  XtVaSetValues(thing1, XtNfromVert, thing, XtNborderWidth, 2,
		XtNright, XtChainLeft,  
		XtNwidth, 400, XtNheight, 10, NULL);

  /* 
   * realize widget and add the actions
   */
  XtRealizeWidget (glob->Toplevel1);
  XtAppAddActions(glob->Appcon1, actionsTable, XtNumber(actionsTable));

  if (glob->numscreens == 2) {
    XtSetArg (args[0], XtNfromVert, control2);
    thing = XtCreateManagedWidget ("label", labelWidgetClass, frame2, 
				   args, cnt);
    thing1 = XtCreateManagedWidget("donetherm", formWidgetClass, frame2, 
				   NULL, 0);
    XtVaSetValues(thing1, XtNfromVert, thing, XtNborderWidth, 2,
		  XtNright, XtChainLeft, 
		  XtNwidth, 400, XtNheight, 10, NULL);
    XtRealizeWidget (glob->Toplevel2);
    /* we don't need to add the actions for the second screen */
  }

  Update(glob);

  /* we'll need to know how many bitplanes we're dealing with later on */
  XGetGeometry (display1, XtWindow(XtNameToWidget(glob->Toplevel1,"*Frame")),
 	    &root, &wx, &wy, &wwidth, &wheight, &wboarder, &glob->wdepth1);

  if (glob->wdepth1 > 1) {
    /* make a red color */
    /* create a graphics context for later use */
    cmap = DefaultColormap(display1, DefaultScreen(display1));
    if (XAllocNamedColor(display1, cmap, "red", &exact, &red) == 0) {
      (void) fprintf(stderr, "Eeek, can't alloc a red color!\n");
      exit (1);
    }
    glob->redgc1 = XCreateGC (display1, XDefaultRootWindow(display1), 0, 0);
    XSetFunction(display1, glob->redgc1, GXcopy);
    XSetGraphicsExposures(display1, glob->redgc1, False);
    XSetForeground (display1, glob->redgc1, red.pixel);
    XSetBackground (display1, glob->redgc1,     
		    BlackPixel (display1, DefaultScreen(display1)));
    /* make a blue color */
    if (XAllocNamedColor(display1, cmap, "cyan", &exact, &blue) == 0) {
      (void) fprintf(stderr, "Eeek, can't alloc a blue color!\n");
      exit (1);
    }
    glob->bluegc1 = XCreateGC (display1, XDefaultRootWindow(display1), 0, 0);
    XSetFunction(display1, glob->bluegc1, GXcopy);
    XSetGraphicsExposures(display1, glob->bluegc1, False);
    XSetForeground (display1, glob->bluegc1, blue.pixel);
    XSetBackground (display1, glob->bluegc1,     
		    BlackPixel (display1, DefaultScreen(display1)));
  } else {
    /* red is the same as white */
    glob->redgc1 = glob->whitegc1;
    glob->bluegc1 = glob->whitegc1;
  }

  /* load the CROCK font */
  fontstruct = XLoadQueryFont(display1, "12x24");
  if (fontstruct == NULL) {
    (void) fprintf(stderr, "Geeze, no 12x24 either!  You're on your own man, using the default.\n");
  }
    
  if (fontstruct != NULL) {
    
    XSetFont(display1, glob->whitegc1, fontstruct->fid);
    XSetFont(display1, glob->blackgc1, fontstruct->fid);
    XSetFont(display1, glob->redgc1, fontstruct->fid);
  }

  /* load the smaller font */
  fontstruct = XLoadQueryFont(display1, "6x13");
  if (fontstruct == NULL) {
    (void) fprintf(stderr, "Yowee, no 6x13 font!  Whatever.\n");
  } else {
    XSetFont(display1, glob->smallgc1, fontstruct->fid);
  }

  if (glob->numscreens == 2) {
    /* wdepth2 is the only thing we're really interested in */
    XGetGeometry (display2, XtWindow(XtNameToWidget(glob->Toplevel2,"*Frame")),
		  &root, &wx, &wy, &wwidth, &wheight, &wboarder, 
		  &glob->wdepth2);

    if (glob->wdepth2 > 1) {
      /* make a red color */
      cmap = DefaultColormap(display2, DefaultScreen(display2));
      if (XAllocNamedColor(display2, cmap, "red", &exact, &red) == 0) {
	(void) fprintf(stderr, "Eeek, can't alloc a red color!\n");
	exit (1); 
      }
      glob->redgc2 = XCreateGC (display2, XDefaultRootWindow(display2), 0, 0);
      XSetFunction(display2, glob->redgc2, GXcopy);
      XSetGraphicsExposures(display2, glob->redgc2, False);
      XSetForeground (display2, glob->redgc2, red.pixel);
      XSetBackground (display2, glob->redgc2, 
		      BlackPixel (display2, DefaultScreen(display2)));

      /* make a blue color */
      if (XAllocNamedColor(display2, cmap, "cyan", &exact, &blue) == 0) {
	(void) fprintf(stderr, "Eeek, can't alloc a blue color!\n");
	exit (1); 
      }
      glob->bluegc2 = XCreateGC (display2, XDefaultRootWindow(display2), 0, 0);
      XSetFunction(display2, glob->bluegc2, GXcopy);
      XSetGraphicsExposures(display2, glob->bluegc2, False);
      XSetForeground (display2, glob->bluegc2, blue.pixel);
      XSetBackground (display2, glob->bluegc2, 
		      BlackPixel (display2, DefaultScreen(display2)));
    } else {
      /* red is the same as white */
      glob->redgc2 = glob->whitegc2;
      glob->bluegc2 = glob->whitegc2;
    }
    /* load the CROCK font */
    fontstruct = XLoadQueryFont(display2, FONTNAME);
    if (fontstruct == NULL) {
      (void) fprintf (stderr,  "Can't load font %s, trying 12x24\n", FONTNAME);
      fontstruct = XLoadQueryFont(display2, "12x24");
      if (fontstruct == NULL) {
	(void) fprintf(stderr, "Geeze, no 12x24 either!  You're on your own, man, using the default.\n");
      }
/*      exit(-1); */
    }
    if (fontstruct != NULL) {
      XSetFont(display2, glob->whitegc2, fontstruct->fid);
      XSetFont(display2, glob->blackgc2, fontstruct->fid);
      XSetFont(display2, glob->redgc2, fontstruct->fid);
    }

    /* load the smaller font */
    fontstruct = XLoadQueryFont(display2, "6x13");
    if (fontstruct == NULL) {
      (void) fprintf(stderr, "Yowee, no 6x13 font!  Whatever.\n");
    } else {
      XSetFont(display2, glob->smallgc2, fontstruct->fid);
    }
  }

  /* get pixmaps for the grey maps */
  MakeGreyMaps(glob);

  /* get pixmaps for the numeric digits */
  MakeNumberMaps(glob);

  /* get pixmaps for hand holding perfect score */
  MakeHandMaps(glob);

  /* Read in the master config file */

  /* open the config file */
  if ((fp = fopen(filename, "r")) == NULL) {
    (void) fprintf (stderr, "Can't open %s for input\n", filename);
    exit (-1);
  }

  /* read in the config file */
  while (fgets(line, 100, fp) != NULL ) {
    if (*line == '#'|| *line == '\n') {
      /* skip comments and blank lines */
      ;
    } else if (strncmp(line, "background ", 11) == 0) {
      /*******************************/
      /* cope with a background line */
      /*******************************/
      Backnode temp;

      if (sscanf(line, "%*s %s %d %d %[^\n]", 
		 localfile, &temp.xoffset, &temp.yoffset, value) != 4) {
	(void) fprintf (stderr, "Can't parse line in input file:\n%s", line);
	continue;
      }	/* end if sscanf */

      /* add the background to our list, we'll load it later */
      temp.prev = NULL;
      temp.next = NULL;
      temp.isloaded = 0;
      if (getenv(CROCKROOTENV)) {
	(void) strcpy(temp.path, getenv(CROCKROOTENV));
        (void) strcat(temp.path, "/");
	(void) strcat(temp.path, localfile);
      } else {
	(void) strcpy(temp.path, CROCKROOT);
        (void) strcat(temp.path, "/");
	(void) strcat(temp.path, localfile);
      }
      (void) strcpy(temp.name, value);
      
      AddNode(&glob->backgrounds, &temp, sizeof(Backnode));
    } 		/* end if background */
#ifdef HASNETAUDIO
    else if (strncmp(line, "fight ", 6) == 0) {
      /*******************************/
      /* cope with a fight      line */
      /*******************************/

      LoadGlobSound(glob, line, &glob->fight1, &glob->fight2);

    } 		/* end if fight */
    else if (strncmp(line, "finish ", 7) == 0) {
      /*******************************/
      /* cope with a finish     line */
      /*******************************/

      LoadGlobSound(glob, line, &glob->finish1, &glob->finish2);

    } 		/* end if finish */
    else if (strncmp(line, "flawless ", 9) == 0) {
      /*******************************/
      /* cope with a flawless   line */
      /*******************************/

      LoadGlobSound(glob, line, &glob->flawless1, &glob->flawless2);

    } 		/* end if flawless */
    else if (strncmp(line, "fatality ", 9) == 0) {
      /*******************************/
      /* cope with a fatality   line */
      /*******************************/

      LoadGlobSound(glob, line, &glob->fatality1, &glob->fatality2);

    } 		/* end if fatality */
    else if (strncmp(line, "startfatal ", 11) == 0) {
      /*******************************/
      /* cope with a startfatal line */
      /*******************************/

      LoadGlobSound(glob, line, &glob->startfatal1, &glob->startfatal2);

    } 		/* end if fatality */
    else if (strncmp(line, "player1wins ", 12) == 0) {
      /********************************/
      /* cope with a player1wins line */
      /********************************/

      LoadGlobSound(glob, line, &glob->p1wins1, &glob->p1wins2);

    } 		/* end if player1wins */
    else if (strncmp(line, "player2wins ", 12) == 0) {
      /********************************/
      /* cope with a player2wins line */
      /********************************/

      LoadGlobSound(glob, line, &glob->p2wins1, &glob->p2wins2);

    } 		/* end if player2wins */
#endif
    else if (strncmp(line, "set ", 4) == 0) {
      /*******************************/
      /* cope with a set line        */
      /*******************************/
      if (sscanf(line, "%*s %s %s", variable, value) != 2) {
	(void) fprintf (stderr, "Can't parse line in input file: \n%s", line);
	continue;
      } /* end if sscanf */

      if (strcmp (variable, "width") == 0) {
	glob->playwidth = atoi(value);
      } else if (strcmp (variable, "height") == 0) {
	glob->playheight = atoi(value);
      } else {
	(void) fprintf (stderr, "Illegal variable %s\n", variable);
	continue;
      }	/* end if strcmp */
    }	/* end if set */
    else if (strncmp(line, "player ", 7) == 0) {
      /*******************************/
      /* cope with a player line     */
      /*******************************/
      int extra = 0;

      if (sscanf(line, "%*s %s", value) != 1) {
	(void) fprintf (stderr, "Can't parse line in input file: \n%s", line);
	continue;
      } /* end if sscanf */
      if (getenv(CROCKROOTENV)) {
	extra = strlen(getenv(CROCKROOTENV)) + 2;
      } else {
	extra = strlen(CROCKROOT) + 2;
      }
      if ((playerfile[*maxplayer] = (char *) malloc (strlen(value) + extra)) 
	  == NULL) {
	(void) fprintf (stderr, "Can't malloc space for playerfile name!\n");
	exit(-1);
      }
      if (getenv(CROCKROOTENV)) {
	(void) strcpy(playerfile[*maxplayer], getenv(CROCKROOTENV));
        (void) strcat(playerfile[*maxplayer], "/");
	(void) strcat(playerfile[*maxplayer], value);
      } else {
	(void) strcpy(playerfile[*maxplayer], CROCKROOT);
        (void) strcat(playerfile[*maxplayer], "/");
	(void) strcat(playerfile[*maxplayer], value);
      }
      (*maxplayer)++;
    }	/* end if player */
  }	/* end while */
  (void) fclose (fp);
}

#ifdef HASNETAUDIO
void
LoadGlobSound(glob, line, sound1, sound2)
Glob *glob;
char *line;
AuBucketID *sound1, *sound2;
{
  AuStatus ret_status;
  char tempfile[100], localfile[100];

  if (sscanf(line, "%*s %s", tempfile) != 1) {
    (void) fprintf (stderr, "Can't parse line in input file:\n%s", line);
    return;
  }	/* end if sscanf */

  if (getenv(CROCKROOTENV)) {
    (void) strcpy(localfile, getenv(CROCKROOTENV));
    (void) strcat(localfile, "/");
    (void) strcat(localfile, tempfile);
  } else {
    (void) strcpy(localfile, CROCKROOT);
    (void) strcat(localfile, "/");
    (void) strcat(localfile, tempfile);
  }

  if (glob->aud1) {
    *sound1 = AuSoundCreateBucketFromFile (glob->aud1, localfile, 
					   AuAccessAllMasks, 
					   NULL, &ret_status);
    
    if (ret_status != AuSuccess || *sound1 == AuNone) {
      (void) fprintf(stderr, "Hey, couldn't read in soundfile %s!\n", 
		     localfile);
    }	/* endif ret_status */
  }		/* end if glob->aud1 */

  if (glob->numscreens == 2) {
    if (glob->aud2) {
      *sound2 = AuSoundCreateBucketFromFile (glob->aud2, localfile, 
					     AuAccessAllMasks, 
					     NULL, &ret_status);
      
      if (ret_status != AuSuccess || *sound2 == AuNone) {
	(void) fprintf(stderr, "Hey, couldn't read in soundfile %s!\n", 
		       localfile);
      }	/* end if ret_status */
    }	/* end if glob->aud2 */
  }	/* end if numscreens == 2 */
}
#else
void
LoadGlobSound(glob, line, sound1, sound2)
Glob *glob;
char *line, *sound1, *sound2;
{
  return;
}
#endif

void
LoadBackground(glob) 
Glob *glob;
{

  /* widget variables */
  Display *display1, *display2;
  unsigned int uwidth, uheight;
  int status, xhot, yhot;

  display1 = XtDisplay(glob->Toplevel1);
  
  /* read in background bitmap file */
  status = XReadBitmapFile (display1, XDefaultRootWindow(display1),
			    glob->currentbg->path, &uwidth, &uheight, 
			    &glob->currentbg->backg1, &xhot, &yhot);
  if (status) {
    (void) fprintf(stderr, "Hey, couldn't read in bitmap %s!\n",
		   glob->currentbg->path);
  }
  
  if (glob->numscreens == 2) {
    /* read in background bitmap file */
    display2 = XtDisplay(glob->Toplevel2);
    
    status = XReadBitmapFile (display2, XDefaultRootWindow(display2),
			      glob->currentbg->path, &uwidth, &uheight, 
			      &glob->currentbg->backg2, &xhot, &yhot);
    if (status) {
      (void) fprintf(stderr, "Hey, couldn't read in bitmap %s!\n", 
		     glob->currentbg->path);
    }	/* end if status */
  }		/* end if numscreens == 2 */
  glob->currentbg->isloaded = 1;
  glob->currentbg->width    = uwidth;
  glob->currentbg->height   = uheight;
}

void
MakeHandMaps(glob)
Glob *glob;

{
  Display *display1, *display2;
  Window   window1,   window2;

  display1 = XtDisplay(glob->Toplevel1);
  window1  = XDefaultRootWindow(display1);

  glob->hand1 = XCreateBitmapFromData(display1, window1, hand_bits, 
				      hand_width, hand_height);
  glob->handm1 = XCreateBitmapFromData(display1, window1, hand_mask_bits, 
				       hand_mask_width, hand_mask_height);
  if (glob->numscreens == 2) {
    display2 = XtDisplay(glob->Toplevel2);
    window2  = XDefaultRootWindow(display2);
    
    glob->hand1 = XCreateBitmapFromData(display2, window2, hand_bits, 
					hand_width, hand_height);
    glob->handm2 = XCreateBitmapFromData(display2, window2, hand_mask_bits, 
					 hand_mask_width, hand_mask_height);
  }
}

void
MakeGreyMaps(glob)
Glob *glob;

{
  Display *display1, *display2;
  Window   window1,   window2;

  display1 = XtDisplay(glob->Toplevel1);
  window1  = XDefaultRootWindow(display1);

  glob->grey4a = XCreateBitmapFromData(display1, window1, grey4_bits, 
				       grey4_width, grey4_height);

  glob->grey8a = XCreateBitmapFromData(display1, window1, grey8_bits, 
				       grey8_width, grey8_height);
  
  glob->grey16a = XCreateBitmapFromData(display1, window1, grey16_bits, 
					grey16_width, grey16_height);

  glob->grey32a = XCreateBitmapFromData(display1, window1, grey32_bits, 
					grey32_width, grey32_height);

  glob->grey64a = XCreateBitmapFromData(display1, window1, grey64_bits, 
					grey64_width, grey64_height);

  glob->grey128a = XCreateBitmapFromData(display1, window1, grey128_bits, 
					 grey128_width, grey128_height);
  
  if (glob->numscreens == 2) {
    display2 = XtDisplay(glob->Toplevel2);
    window2  = XDefaultRootWindow(display2);
    
    glob->grey4b = XCreateBitmapFromData(display2, window2, grey4_bits, 
					 grey4_width, grey4_height);

    glob->grey8b = XCreateBitmapFromData(display2, window2, grey8_bits, 
					 grey8_width, grey8_height);
  
    glob->grey16b = XCreateBitmapFromData(display2, window2, grey16_bits, 
					  grey16_width, grey16_height);

    glob->grey32b = XCreateBitmapFromData(display2, window2, grey32_bits, 
					  grey32_width, grey32_height);

    glob->grey64b = XCreateBitmapFromData(display2, window2, grey64_bits, 
					  grey64_width, grey64_height);

    glob->grey128b = XCreateBitmapFromData(display2, window2, grey128_bits, 
					   grey128_width, grey128_height);
  }
}

void
MakeNumberMaps(glob)
Glob *glob;

{
  Display *display1, *display2;
  Window   window1,   window2;

  display1 = XtDisplay(glob->Toplevel1);
  window1  = XDefaultRootWindow(display1);

  glob->zero1 = XCreateBitmapFromData(display1, window1, zero_bits, 
				      zero_width, zero_height);

  glob->one1 = XCreateBitmapFromData(display1, window1, one_bits, 
				     one_width, one_height);
  
  glob->two1 = XCreateBitmapFromData(display1, window1, two_bits, 
				     two_width, two_height);
  
  glob->three1 = XCreateBitmapFromData(display1, window1, three_bits, 
				       three_width, three_height);
  
  glob->four1 = XCreateBitmapFromData(display1, window1, four_bits, 
				      four_width, four_height);
  
  glob->five1 = XCreateBitmapFromData(display1, window1, five_bits, 
				      five_width, five_height);

  glob->six1 = XCreateBitmapFromData(display1, window1, six_bits, 
				     six_width, six_height);

  glob->seven1 = XCreateBitmapFromData(display1, window1, seven_bits, 
				     seven_width, seven_height);

  glob->eight1 = XCreateBitmapFromData(display1, window1, eight_bits, 
				     eight_width, eight_height);

  glob->nine1 = XCreateBitmapFromData(display1, window1, nine_bits, 
				      nine_width, nine_height);
  
  if (glob->numscreens == 2) {
    display2 = XtDisplay(glob->Toplevel2);
    window2  = XDefaultRootWindow(display2);

    glob->zero2 = XCreateBitmapFromData(display2, window2, zero_bits, 
					zero_width, zero_height);
    
    glob->one2 = XCreateBitmapFromData(display2, window2, one_bits, 
				       one_width, one_height);
    
    glob->two2 = XCreateBitmapFromData(display2, window2, two_bits, 
				       two_width, two_height);
    
    glob->three2 = XCreateBitmapFromData(display2, window2, three_bits, 
					 three_width, three_height);
    
    glob->four2 = XCreateBitmapFromData(display2, window2, four_bits, 
					four_width, four_height);
    
    glob->five2 = XCreateBitmapFromData(display2, window2, five_bits, 
					five_width, five_height);
    
    glob->six2 = XCreateBitmapFromData(display2, window2, six_bits, 
				       six_width, six_height);
    
    glob->seven2 = XCreateBitmapFromData(display2, window2, seven_bits, 
					 seven_width, seven_height);
    
    glob->eight2 = XCreateBitmapFromData(display2, window2, eight_bits, 
					 eight_width, eight_height);
    
    glob->nine2 = XCreateBitmapFromData(display2, window2, nine_bits, 
					nine_width, nine_height);
  }
}

/*
 * get rid of the label widget and resize the frame to hold the background 
 */
void XSetUp2(glob)
Glob  *glob;

{
  int        cnt;
  Dimension  width, height;
  Arg        args[15];
  Display   *display1, *display2;
  
  display1 = XtDisplay(glob->Toplevel1);

  /* get rid of the label and resize the frame to the right size */
  XtUnmanageChild(XtNameToWidget(glob->Toplevel1, "*label"));
  XtDestroyWidget(XtNameToWidget(glob->Toplevel1, "*label"));
  XtUnmanageChild(XtNameToWidget(glob->Toplevel1, "*donetherm"));
  XtDestroyWidget(XtNameToWidget(glob->Toplevel1, "*donetherm"));

  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, 
	    XtNameToWidget(glob->Toplevel1,"*controlframe")); cnt++;
  XtSetArg (args[cnt], XtNborderWidth, 2);                    cnt++;
  XtSetArg (args[cnt], XtNright, XtChainLeft);                cnt++;
  XtSetArg (args[cnt], XtNwidth,  glob->playwidth);           cnt++; 
  XtSetArg (args[cnt], XtNheight, glob->playheight);          cnt++;

  XtSetValues(XtNameToWidget(glob->Toplevel1, "*display"), args, cnt);
  XtManageChild(XtNameToWidget(glob->Toplevel1, "*display"));

  cnt = 0;
  XtSetArg (args[cnt], XtNheight, &height);          cnt++; 
  XtGetValues(XtNameToWidget(glob->Toplevel1, "*controlframe"), args, cnt);
  height = 150;
  cnt = 0;
  XtSetArg (args[cnt], XtNwidth,  glob->playwidth + 10);   cnt++; 
  XtSetArg (args[cnt], XtNheight, glob->playheight + 120);  cnt++; 
  XtSetValues(glob->Toplevel1, args, cnt);
  XtSetValues(XtNameToWidget(glob->Toplevel1, "*Frame"), args, cnt);

  XtUnmanageChild(XtNameToWidget(glob->Toplevel1, "*Frame"));
  XtManageChild(XtNameToWidget(glob->Toplevel1, "*Frame"));

  /* create temp pixmap where all screen updates are drawn */
  glob->temp1 = XCreatePixmap (display1, XDefaultRootWindow(display1),
			       glob->playwidth, glob->playheight, 
			       glob->wdepth1);
  if (glob->temp1 == (Pixmap) NULL) {
    (void) fprintf (stderr, "Can't create temp pixmap!\n");
    exit (-1);
  }

  if (glob->numscreens == 2) {
    /* if we have a second screen */
    display2 = XtDisplay(glob->Toplevel2);

    cnt = 0;
    XtSetArg (args[cnt], XtNfromVert, 
	      XtNameToWidget(glob->Toplevel2,"*controlframe")); cnt++;
    XtSetArg (args[cnt], XtNborderWidth, 2);                    cnt++;
    XtSetArg (args[cnt], XtNright, XtChainLeft);                cnt++;
    XtSetArg (args[cnt], XtNwidth,  glob->playwidth);           cnt++; 
    XtSetArg (args[cnt], XtNheight, glob->playheight);          cnt++; 

    XtSetValues(XtNameToWidget(glob->Toplevel2, "*display"), args, cnt);
    XtManageChild(XtNameToWidget(glob->Toplevel2, "*display"));

    XtUnmanageChild(XtNameToWidget(glob->Toplevel2, "*label"));
    XtDestroyWidget(XtNameToWidget(glob->Toplevel2, "*label"));
    XtUnmanageChild(XtNameToWidget(glob->Toplevel2, "*donetherm"));
    XtDestroyWidget(XtNameToWidget(glob->Toplevel2, "*donetherm"));

    cnt = 0;
    XtSetArg (args[cnt], XtNwidth,  glob->playwidth + 10);      cnt++; 
    XtSetArg (args[cnt], XtNheight, glob->playheight + 120);     cnt++; 
    XtSetValues(glob->Toplevel2, args, cnt);
    XtSetValues(XtNameToWidget(glob->Toplevel2, "*Frame"), args, cnt);
  
    /* create temp pixmap where all screen updates are drawn */
    glob->temp2 = XCreatePixmap (display2, XDefaultRootWindow(display2),
				 glob->playwidth, glob->playheight, 
				 glob->wdepth2);
    if (glob->temp2 == (Pixmap) NULL) {
      (void) fprintf (stderr, "Can't create temp pixmap!\n");
      exit (-1);
    }
  }
}

void ReadInBitmaps(glob, player, configfile)
Glob     *glob;
Player   *player;
char     *configfile;

{

  Display *display1, *display2;
  unsigned int uwidth, uheight;
  int status, xhot, yhot;

  FILE   *fp;
  char    variable[20],    value[80];
  char    line[100],       bmfile[50];
  char    movename[50],    filename[200];
  char    tag[25],         binding[80];
  int     i, delay, damage, xoff, yoff, flags;

  /* counter for how many load lines there are */
  int     loadcount, numloaded;

  /* set-able variables */
  char pathprefix[100], bmsuffix[20], bmmsuffix[20];
  char sndsuffix[20], version[20];

  /* translation variables */
  Keynode *newnode;

  display1 = XtDisplay(glob->Toplevel1);
  if (glob->numscreens == 2) {
    display2 = XtDisplay(glob->Toplevel2);
  }

  /* 
   * We'll read the file in a two pass method.  The first is just to
   * count how many "load" lines there are.  A little wasteful, but...
   */

  if ((fp = fopen(configfile, "r")) == NULL) {
    (void) fprintf (stderr, "Can't open %s for input\n", configfile);
    exit (-1);
  }
  loadcount = 0;
  numloaded = 0;

  while (fgets(line, 100, fp) != NULL) {
    if (strncmp(line, "load", 4) == 0) loadcount++;
    if (strncmp(line, "soundload", 9) == 0) loadcount++;
  }
  fclose(fp);


  /* open the config file */
  if ((fp = fopen(configfile, "r")) == NULL) {
    (void) fprintf (stderr, "Can't open %s for input\n", configfile);
    exit (-1);
  }

  /* read in the config file */
  while (fgets(line, 100, fp) != NULL ) {
    if (*line == '#'|| *line == '\n') {
      /* skip comments and blank lines */
      ;
    } else if (strncmp(line, "load ", 5) == 0) {
      /*******************************/
      /* cope with a load line       */
      /*******************************/

      /* extra parameters are x and y offsets for image */
      if (sscanf(line, "%*s %s %s %d %d %d %d %d %d, %d %d %d %d, %d %d %d %d, %d %d %d %d", 
		 player->Images1[player->Maxindex].tag, bmfile,
		 &player->Images1[player->Maxindex].offsetx,
		 &player->Images1[player->Maxindex].offsety,
		 &player->Images1[player->Maxindex].attack.x1,
		 &player->Images1[player->Maxindex].attack.y1,
		 &player->Images1[player->Maxindex].attack.x2,
		 &player->Images1[player->Maxindex].attack.y2,
		 &player->Images1[player->Maxindex].high.x1,
		 &player->Images1[player->Maxindex].high.y1,
		 &player->Images1[player->Maxindex].high.x2,
		 &player->Images1[player->Maxindex].high.y2,
		 &player->Images1[player->Maxindex].middle.x1,
		 &player->Images1[player->Maxindex].middle.y1,
		 &player->Images1[player->Maxindex].middle.x2,
		 &player->Images1[player->Maxindex].middle.y2,
		 &player->Images1[player->Maxindex].low.x1,
		 &player->Images1[player->Maxindex].low.y1,
		 &player->Images1[player->Maxindex].low.x2,
		 &player->Images1[player->Maxindex].low.y2) 
	  != 20) {
	
	(void) fprintf (stderr, "Can't parse line in input file:\n%s", line);
	continue;
      }	/* end if sscanf */
      
#define PIM1 player->Images1[player->Maxindex]
#define PIM2 player->Images2[player->Maxindex]
      
      /* copy all Images1 parameters to Images2 */
      PIM2.offsetx   = PIM1.offsetx;     PIM2.offsety = PIM1.offsety;
      PIM2.attack.x1 = PIM1.attack.x1; PIM2.attack.y1 = PIM1.attack.y1;
      PIM2.attack.x2 = PIM1.attack.x2; PIM2.attack.y2 = PIM1.attack.y2;
      PIM2.high.x1   = PIM1.high.x1;     PIM2.high.y1 = PIM1.high.y1;
      PIM2.high.x2   = PIM1.high.x2;     PIM2.high.y2 = PIM1.high.y2;
      PIM2.middle.x1 = PIM1.middle.x1; PIM2.middle.y1 = PIM1.middle.y1;
      PIM2.middle.x2 = PIM1.middle.x2; PIM2.middle.y2 = PIM1.middle.y2;
      PIM2.low.x1    = PIM1.low.x1;       PIM2.low.y1 = PIM1.low.y1;
      PIM2.low.x2    = PIM1.low.x2;       PIM2.low.y2 = PIM1.low.y2;

#undef PIM1
#undef PIM2

      /* load in the bitmap file once we've built up the names */
      if (getenv(CROCKROOTENV)) {
	(void) strcpy(filename, getenv(CROCKROOTENV));
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      } else {
	(void) strcpy(filename, CROCKROOT);
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      }
      (void) strcat (filename, bmfile);
      (void) strcat (filename, bmsuffix);

      updatemessage(filename, XtNameToWidget(glob->Toplevel1, "*label"), 
		    glob, loadcount, numloaded);
      
      status = XReadBitmapFile (display1, XDefaultRootWindow(display1),
				filename, 
				/* note: we only need to save the width
				         and height for the bitmap, the
					 bitmask will have the same values */
				&player->Images1[player->Maxindex].width,
				&player->Images1[player->Maxindex].height,
				&player->Images1[player->Maxindex].pm,
				&xhot, &yhot);

      if (status) {
	(void) fprintf(stderr, "Hey, couldn't read in bitmap %s!\n", filename);
      }
      
      if (glob->numscreens == 2) {
	updatemessage(filename, XtNameToWidget(glob->Toplevel2, "*label"), 
		      glob, loadcount, numloaded);
      
	status = XReadBitmapFile (display2, XDefaultRootWindow(display2),
				  filename, 
				  /* note: we only need to save the width
				           and height for the bitmap, the
					   bitmask will have the same values */
				  &player->Images2[player->Maxindex].width,
				  &player->Images2[player->Maxindex].height,
				  &player->Images2[player->Maxindex].pm,
				  &xhot, &yhot);

	if (status) {
	  (void) fprintf(stderr, "Hey, couldn't read in bitmap %s!\n", 
			 filename);
	}
      }		/* end if numscreens == 2 */

      /* load in the bitmap file mask once we've built up the names */
      if (getenv(CROCKROOTENV)) {
	(void) strcpy(filename, getenv(CROCKROOTENV));
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      } else {      
	(void) strcpy(filename, CROCKROOT);
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      }
      (void) strcat (filename, bmfile);
      (void) strcat (filename, bmmsuffix);

      updatemessage(filename, XtNameToWidget(glob->Toplevel1, "*label"), 
		    glob, loadcount, numloaded);

      status = XReadBitmapFile (display1, XDefaultRootWindow(display1),
				filename, &uwidth, &uheight, 
				&player->Images1[player->Maxindex].mask,
				&xhot, &yhot);
      if (status) {
	(void) fprintf (stderr, "Hey, couldn't read in mask %s!\n", filename);
      }

      if (glob->numscreens == 2) {
	updatemessage(filename, XtNameToWidget(glob->Toplevel2, "*label"), 
		      glob, loadcount, numloaded);
	
	status = XReadBitmapFile (display2, XDefaultRootWindow(display2),
				  filename, &uwidth, &uheight, 
				  &player->Images2[player->Maxindex].mask,
				  &xhot, &yhot);
	if (status) {
	  (void) fprintf (stderr, "Hey, couldn't read in mask %s!\n", 
			  filename);
	}
      }		/* end if numscreens == 2 */

      numloaded++;
      if (++player->Maxindex == MAXINDEX) {
	(void) fprintf (stderr, "Hey dude, time to bump up MAXINDEX!!!\n");
	exit (0);
      }	/* end if ++ */
    }	/* end if load */
    else if (strncmp(line, "loadmask ", 9) == 0) {
      /*******************************/
      /* cope with a loadmask line   */
      /*******************************/

      /* loads in mask only for images, duplicates already loaded 
	 bitmap based on first tag */
      if (sscanf(line, "%*s %s %s %s %d %d %d %d %d %d, %d %d %d %d, %d %d %d %d, %d %d %d %d", 
		 tag,
		 player->Images1[player->Maxindex].tag, bmfile,
		 &player->Images1[player->Maxindex].offsetx,
		 &player->Images1[player->Maxindex].offsety,
		 &player->Images1[player->Maxindex].attack.x1,
		 &player->Images1[player->Maxindex].attack.y1,
		 &player->Images1[player->Maxindex].attack.x2,
		 &player->Images1[player->Maxindex].attack.y2,
		 &player->Images1[player->Maxindex].high.x1,
		 &player->Images1[player->Maxindex].high.y1,
		 &player->Images1[player->Maxindex].high.x2,
		 &player->Images1[player->Maxindex].high.y2,
		 &player->Images1[player->Maxindex].middle.x1,
		 &player->Images1[player->Maxindex].middle.y1,
		 &player->Images1[player->Maxindex].middle.x2,
		 &player->Images1[player->Maxindex].middle.y2,
		 &player->Images1[player->Maxindex].low.x1,
		 &player->Images1[player->Maxindex].low.y1,
		 &player->Images1[player->Maxindex].low.x2,
		 &player->Images1[player->Maxindex].low.y2) 
	  != 21) {
	
	(void) fprintf (stderr, "Can't parse line in input file:\n%s", line);
	continue;
      }	/* end if sscanf */
      
#define PIM1 player->Images1[player->Maxindex]
#define PIM2 player->Images2[player->Maxindex]
      
      /* copy all Images1 parameters to Images2 */
      PIM2.offsetx   = PIM1.offsetx;     PIM2.offsety = PIM1.offsety;
      PIM2.attack.x1 = PIM1.attack.x1; PIM2.attack.y1 = PIM1.attack.y1;
      PIM2.attack.x2 = PIM1.attack.x2; PIM2.attack.y2 = PIM1.attack.y2;
      PIM2.high.x1   = PIM1.high.x1;     PIM2.high.y1 = PIM1.high.y1;
      PIM2.high.x2   = PIM1.high.x2;     PIM2.high.y2 = PIM1.high.y2;
      PIM2.middle.x1 = PIM1.middle.x1; PIM2.middle.y1 = PIM1.middle.y1;
      PIM2.middle.x2 = PIM1.middle.x2; PIM2.middle.y2 = PIM1.middle.y2;
      PIM2.low.x1    = PIM1.low.x1;       PIM2.low.y1 = PIM1.low.y1;
      PIM2.low.x2    = PIM1.low.x2;       PIM2.low.y2 = PIM1.low.y2;

#undef PIM1
#undef PIM2

      /* match the tag to an existing bitmap */
      if ((i = Findtag(tag, player)) == -1) {
	(void) fprintf(stderr, "tag: %s not loaded for line:\n%s\n",
		       tag, line);
	continue;
      }
      player->Images1[player->Maxindex].pm = player->Images1[i].pm;
      if (glob->numscreens == 2) {
	player->Images2[player->Maxindex].pm = player->Images2[i].pm;
      }
      
      /* load in the bitmap file mask once we've built up the names */
      if (getenv(CROCKROOTENV)) {
	(void) strcpy(filename, getenv(CROCKROOTENV));
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      } else {
	(void) strcpy(filename, CROCKROOT);
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      }
      (void) strcat (filename, bmfile);
      (void) strcat (filename, bmmsuffix);

      updatemessage(filename, XtNameToWidget(glob->Toplevel1, "*label"), 
		    glob, loadcount, numloaded);

      status = XReadBitmapFile (display1, XDefaultRootWindow(display1),
				filename, 
				&player->Images1[player->Maxindex].width,
				&player->Images1[player->Maxindex].height,
				&player->Images1[player->Maxindex].mask,
				&xhot, &yhot);
      if (status) {
	(void) fprintf (stderr, "Hey, couldn't read in mask %s!\n", filename);
      }

      if (glob->numscreens == 2) {
	updatemessage(filename, XtNameToWidget(glob->Toplevel2, "*label"), 
		      glob, loadcount, numloaded);
	
	status = XReadBitmapFile (display2, XDefaultRootWindow(display2),
				  filename, 
				  &player->Images2[player->Maxindex].width,
				  &player->Images2[player->Maxindex].height,
				  &player->Images2[player->Maxindex].mask,
				  &xhot, &yhot);
	if (status) {
	  (void) fprintf (stderr, "Hey, couldn't read in mask %s!\n", 
			  filename);
	}
      }		/* end if numscreens == 2 */

      numloaded++;
      if (++player->Maxindex == MAXINDEX) {
	(void) fprintf (stderr, "Hey dude, time to bump up MAXINDEX!!!\n");
	exit (0);
      }	/* end if ++ */
    }	/* end if loadmask */

#ifdef HASNETAUDIO
    else if (strncmp(line, "soundload ", 5) == 0) {
      /*******************************/
      /* cope with a soundload line  */
      /*******************************/

      AuStatus ret_status;

      if (*version == '1' || *version == '2') {
	(void) fprintf (stderr, 
			"version must be 3 or higher to support sound\n");
	continue;
      }

      /* extra parameters are x and y offsets for image */
      if (sscanf(line, "%*s %s %s", 
		 player->bucket1[player->maxbuckets].tag, bmfile) != 2) {
	
	(void) fprintf (stderr, "Can't parse line in input file:\n%s", line);
	continue;
      }	/* end if sscanf */

      /* load in the sound file once we've built up the names */
      if (getenv(CROCKROOTENV)) {
	(void) strcpy(filename, getenv(CROCKROOTENV));
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      } else {
	(void) strcpy(filename, CROCKROOT);
        (void) strcat(filename, "/");
	(void) strcat (filename, pathprefix);
      }
      (void) strcat (filename, bmfile);
      (void) strcat (filename, sndsuffix);

      updatemessage(filename, XtNameToWidget(glob->Toplevel1, "*label"), 
		    glob, loadcount, numloaded);

      if (glob->aud1) {
	player->bucket1[player->maxbuckets].sound = 
	  AuSoundCreateBucketFromFile (glob->aud1, filename, AuAccessAllMasks,
				       NULL, &ret_status);

	if (ret_status != AuSuccess || 
	    player->bucket1[player->maxbuckets].sound == AuNone) {
	  (void) fprintf(stderr, "Hey, couldn't read in soundfile %s!\n", 
			 filename);
	}	/* endif ret_status */
      }		/* end if glob->aud1 */

      if (glob->numscreens == 2) {
	updatemessage(filename, XtNameToWidget(glob->Toplevel2, "*label"), 
		      glob, loadcount, numloaded);

	if (glob->aud2) {
	  player->bucket2[player->maxbuckets].sound = 
	    AuSoundCreateBucketFromFile (glob->aud2, filename, 
					 AuAccessAllMasks, NULL, &ret_status);

	  if (ret_status != AuSuccess || 
	      player->bucket2[player->maxbuckets].sound == AuNone) {
	    (void) fprintf(stderr, "Hey, couldn't read in soundfile %s!\n", 
			   filename);
	  }	/* end if ret_status */
	}	/* end if glob->aud2 */
      }		/* end if numscreens == 2 */

      numloaded++;
      if (++player->maxbuckets == MAXINDEX) {
	(void) fprintf (stderr, "Hey dude, time to bump up MAXINDEX!!!\n");
	exit (0);
      }	/* end if ++ */
    }	/* end if soundload */
#endif	/* HASNETAUDIO */
    else if (strncmp(line, "define ", 7) == 0) {
      /**************************************/
      /* cope with a define line            */
      /**************************************/
      if (sscanf(line, "%*s %s %d %d %d %d", 
		 movename, &damage, &xoff, &yoff, &flags) != 5) {
	(void) fprintf(stderr, "Can't parse line in input file: \n%s", line);
	continue;
      }	/* end if sscanf */

      /* add the move's name to the players struct */
      strncpy(player->moves[player->maxmoves].movename, movename, 25);

      /* set the damage the move does */
      player->moves[player->maxmoves].damage   = damage;
      player->moves[player->maxmoves].xoff     = xoff;
      player->moves[player->maxmoves].yoff     = yoff;
      player->moves[player->maxmoves].flags    = flags;
      player->moves[player->maxmoves].maxindex = 0;
      
      /* read in a new line (first time thing) */
      if (fgets(line, 100, fp) == NULL) {
	(void) fprintf(stderr, "Premature end of file!\n");
	break;
      }

      if (*version != '1' && *version != '2') {
	if (strncmp(line, "soundtag ", 9) == 0) {
	  if (sscanf(line, "%*s %s", movename) != 1) {
	    (void) fprintf(stderr, 
			   "Can't parse line in input file: \n%s", line);
	    continue;
	  }	/* end if sscanf */
	  
	  /* add the sound tag's name to players struct */
	  strncpy(player->moves[player->maxmoves].soundname, movename, 25);
	  
	  /* read in a new line (first time thing, once again) */
	  if (fgets(line, 100, fp) == NULL) {
	    (void) fprintf(stderr, "Premature end of file!\n");
	    break;
	  }	/* endif fgets */
	}	/* endif strncmp */
      }		/* endif version */
	
      /* read the moves and add them to the list until we get to the enddef */
      while (strncmp(line, "enddef", 6) != 0) {

	/* skip comments */
	if (*line == '#' || *line == '\n') continue;

	/* add the RImage index to the move struct.  Note that */
	/* the bitmap and tag must already be loaded or we won't */
	/* find the tag. */
	if (sscanf(line, "%s %d", tag, &delay) != 2) {
	  (void) fprintf(stderr, "Can't parse line in input file: \n%s", line);
	  continue;
	}
	if ((player->moves[player->maxmoves].
	     frameindex[player->moves[player->maxmoves].maxindex] 
	     = Findtag(tag, player)) == -1) {
	  (void) fprintf(stderr, "Tag %s not found while defining move %s\n",
			 tag, movename);
	  /* get out of this loop, back to main while loop */
	  break;
	}
	player->moves[player->maxmoves].
	  framedelay[player->moves[player->maxmoves].maxindex] = delay;
	player->moves[player->maxmoves].maxindex++;

	/* get the next line */
	if (fgets(line, 100, fp) == NULL) {
	  (void) fprintf(stderr, "Premature end of file!\n");
	  break;
	}	 /* end if fgets */
      }		/* end while !enddef */
      player->maxmoves++;
    }	/* end if define */
    else if (strncmp(line, "bind ", 5) == 0) {
      /*******************************/
      /* cope with a key bind line   */
      /*******************************/
      if (sscanf(line, "%*s %s %s", movename, binding) != 2) {
	(void) fprintf (stderr, "Can't parse line in input file: \n%s", line);
	continue;
      }	/* end if sscanf */
      
      /* make new node for linked list */
      if ((newnode = (Keynode *) malloc (sizeof (Keynode))) == NULL) {
	(void) fprintf(stderr, "Eeek, can't malloc space for new node.\n");
	exit(-1);
      }
      newnode->prev = NULL;
      newnode->next = NULL;
      /* store the binding into the linked list */
      if ((newnode->binding = (char *) malloc (strlen (line) + 1)) == NULL) {
	(void) fprintf(stderr, "Eeek, can't malloc string for new node.\n");
	exit(-1);
      }
      (void) strcpy(newnode->binding, line);
      AddNode(&player->keybindings,  newnode, sizeof (Keynode));

      /* add moves into table for computer to use */
      if ((player->movetable[player->maxmovetable] = 
	   (char *) malloc(strlen(movename) + 1)) == NULL ) {
	(void) fprintf(stderr, "Can't malloc movetable!\n");
	exit(1);
      }
      (void) strcpy(player->movetable[player->maxmovetable++], movename);
    }	/* end if bind */
    else if (strncmp(line, "set ", 4) == 0) {
      /*******************************/
      /* cope with a set line        */
      /*******************************/
      if (sscanf(line, "%*s %s %s", variable, value) != 2) {
	(void) fprintf (stderr, "Can't parse line in input file: \n%s", line);
	continue;
      } /* end if sscanf */

      if (strcmp(variable, "pathprefix") == 0) {
	(void) strncpy (pathprefix, value, 100);
      } else if (strcmp (variable, "bmsuffix") == 0) {
	(void) strncpy (bmsuffix, value, 20);
      } else if (strcmp (variable, "bmmsuffix") == 0) {
	(void) strncpy (bmmsuffix, value, 20);
      } else if (strcmp (variable, "sndsuffix") == 0) {
	(void) strncpy (sndsuffix, value, 20);
      } else if (strcmp (variable, "version") == 0) {
	(void) strncpy (version, value, 20);
      } else if (strcmp (variable, "startattribute") == 0) {
	player->attrib = atoi(value);
      } else if (strcmp (variable, "maxattribute") == 0) {
	player->maxattrib = atoi(value);
      } else if (strcmp (variable, "width") == 0) {
	player->width = atoi(value);
      } else if (strcmp (variable, "stepsize") == 0) {
	player->stepsize = atoi(value);
      } else if (strcmp (variable, "face") == 0) {
	if (strncmp(value, "left", 4) == 0) {
	  player->facing = LEFT;
	} else if (strncmp(value, "right", 5) == 0) {
	  player->facing = RIGHT;
	} else {
	  (void) fprintf(stderr, "Illegal value '%s' for variable %s\n",
			 value, variable);
	  continue;
	}
      } else {
	(void) fprintf (stderr, "Illegal variable %s\n", variable);
	continue;
      }	/* end if strcmp */
    }	/* end if set */
  Update(glob);
  }	/* end while */
  (void) fclose (fp);
}

/* 
 * quick and dirty routine to return the basename
 * of a file (i.e., skip the path component)
 */
char *basename(filename)
char *filename;
{
  register char *p1, *p2;
  for (p2 = p1 = filename; *p1 != '\0'; p1++)
    if (*p1 == '/') p2 = p1;
  return ++p2;
}

void updatemessage(msg, w, glob, total, sofar)
char     *msg;
Widget    w;
Glob     *glob;
int       total, sofar;
{
  int cnt = 0;
  Arg args[15];
  char thing[100];
  float percent;

  percent = (float) ((float)sofar / (float)total);

  if (glob->debugflag & 1) {
    /* print verbose bitmap load info */
    (void) sprintf(thing, "Loading %s ... ", basename(msg));
  } else {
    /* otherwise only report on percentage done */
    (void) sprintf(thing, "Loading player bitmap/sound data: %d%% done.",
		   (int) (percent * 100));
  }
  XtSetArg (args[cnt], XtNlabel,  thing);                       cnt++; 
  XtSetValues(w, args, cnt);

  XFillRectangle(XtDisplay(glob->Toplevel1), 
                 XtWindow(XtNameToWidget(glob->Toplevel1, "*donetherm")),
		 glob->whitegc1, 0, 0, 400, 10);

  XFillRectangle(XtDisplay(glob->Toplevel1), 
                 XtWindow(XtNameToWidget(glob->Toplevel1, "*donetherm")),
		 glob->blackgc1, 0, 0, (int) (400 * percent), 10);
			  
  if (glob->numscreens == 2) {
    XFillRectangle(XtDisplay(glob->Toplevel2), 
		   XtWindow(XtNameToWidget(glob->Toplevel2,"*donetherm")),
		   glob->whitegc2, 0, 0, 400, 10);
    
    XFillRectangle(XtDisplay(glob->Toplevel2), 
		   XtWindow(XtNameToWidget(glob->Toplevel2,"*donetherm")),
		   glob->blackgc2, 0, 0, (int) (400 * percent), 10);
    
  }
  Update(glob);
}
