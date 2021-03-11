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

/*
 * speed test to do rock in C and see
 * how much faster it is than tcl/tk.
 * 
 * --FNA (9/22/93).
 *
 * ok, it's grown to be a little more than that...
 * 
 * --FNA (5/18/94).
 */

#include "types.h"
#include "version.h"
#include "patchlevel.h"

int delay = 120000;
Glob *Gglob;

void
main (argc, argv)
int argc;
char **argv;

{
  Glob glob;
  register Glob *gptr = &glob;
  char *configfile = CONFIGFILE;
  char *Datafile[10];
  int  maxplayer = 0;

  Gglob = gptr;
  glob.numscreens           = 1;
  glob.currentbg            = NULL;
  glob.debugflag            = 0;
  glob.gamemode             = EVIL;
  glob.volume1              = DEFAULT_VOLUME;
  glob.volume2              = DEFAULT_VOLUME;
  glob.screen1              = LEFT;
  glob.screen2              = RIGHT;
  glob.startsoundfile       = NULL;
  glob.shape1               = NULL;
  glob.shape2               = NULL;
  glob.player1.keybindings  = NULL;
  glob.player2.keybindings  = NULL;
  glob.player1.projectiles  = NULL;
  glob.player2.projectiles  = NULL;
  glob.player1.Maxindex     = 0;
  glob.player2.Maxindex     = 0;
#ifdef HASNETAUDIO
  glob.player1.maxbuckets   = 0;
  glob.player2.maxbuckets   = 0;
#endif
  glob.player1.maxmovetable = 0;
  glob.player2.maxmovetable = 0;
  glob.player1.playertype   = COMPUTER;
  glob.player2.playertype   = COMPUTER;
  
  /* initialize X toolkit */
  glob.Toplevel1 = XtAppInitialize (&glob.Appcon1, TITLE, NULL, 0, &argc, argv,
			      NULL, NULL, 0);

#ifdef HASNETAUDIO
  /* initialize connection to sound server (NULL means no server) */
  /* we're relying on DISPLAY and AUDIOSERVER env. vars, not -display */
  glob.aud1 = AuOpenServer (NULL, 0, NULL, 0, NULL, NULL);
  if (glob.aud1) {
    (void) AuXtAppAddAudioHandler (glob.Appcon1, glob.aud1);
  }
#endif

  ParseArgs(argc, argv, gptr, &configfile); 

  /* --------------------- */
  /* SelectCharacter()     */

  /* set up X stuff */
  XSetUp(gptr, configfile, Datafile, &maxplayer);

  /* set up player 1 and 2*/
  ReadInBitmaps(gptr, &glob.player1, Datafile[0]);
  ReadInBitmaps(gptr, &glob.player2, Datafile[1]);

  /* more X stuff */
  XSetUp2(gptr);


  for (; ; ) {     /* forever */
    /* reset variables and play a match */
    glob.round = 0;
    do  {
      glob.round++;
      Initstuff(gptr);
      mainloop(gptr);
    } while (glob.round);
    glob.continue1 = 0;
    glob.continue2 = 0;
    XtSetSensitive(XtNameToWidget(glob.Toplevel1, "*Play again?"), 1);
    if (glob.numscreens == 2) {
      XtSetSensitive(XtNameToWidget(glob.Toplevel2, "*Play again?"), 1);
    }
    while (glob.continue1 == 0 && glob.continue2 == 0) {
      /* wait until the user presses continue or quit */
      Update(glob);
    }
    XtSetSensitive(XtNameToWidget(glob.Toplevel1, "*Play again?"), 0);
    if (glob.numscreens == 2) {
      XtSetSensitive(XtNameToWidget(glob.Toplevel2, "*Play again?"), 0);
    }
  } 
/*  Quit(gptr); */
}

void ParseArgs(argc, argv, glob, configfile)
int    argc;
char **argv;
Glob  *glob;
char **configfile;
{
  Display *display;
  char **saveargv = argv;

  argv++;
  while (--argc) {
    if (strcmp(*argv, "-disney") == 0) {
      glob->gamemode = GOOD;
    } else if (strcmp(*argv, "-h") == 0) {
      (void) fprintf(stderr, "usage: %s [-v|-h|-f file|-debug NNN|-disney|Xdisplay2name]\n\
		where -v prints the version and -h prints this message\n\
		and -f file uses a master config file other than the default.\n\
		and -debug NNN turns on debug flag number NNN\n\
		and -disney puts it in rated G mode",
		     saveargv[0]);
    } else if (strcmp(*argv, "-v") == 0) {
      (void) fprintf(stderr, "%s, Version: %s, patchlevel: %d\n",
                     VERSIONNAME, VERSIONNUMB, PATCHLEVEL);
    } else if (strcmp(*argv, "-f") == 0) {
      *configfile = argv[1];
      argv++; argc--;
    } else if (strcmp (*argv, "-debug") == 0) {
      glob->debugflag |= atoi(argv[1]);
      argv++; argc--;
    } else {
      glob->numscreens = 2;
      /* open display to second screen */
      display = XtOpenDisplay(glob->Appcon1, *argv, TITLE, CLASS, 
			      NULL, 0, &argc, argv);
      
      /* pop up top level widget for second screen */
      glob->Toplevel2 = XtAppCreateShell (TITLE, CLASS,
					  applicationShellWidgetClass, 
					  display, NULL, 0);
      glob->player1.playertype = HUMAN;
#ifdef HASNETAUDIO
      /* try to open up connection to second audio server */
      glob->aud2 = AuOpenServer(*argv, 0, NULL, 0, NULL, NULL);
      if (glob->aud2) {
	(void) AuXtAppAddAudioHandler (glob->Appcon1, glob->aud2);
      }
#endif
    }
    argv++;
  }
}

static int TICK=100000;
/*
 * Note, the main loop will execute once every 100,000 microseconds
 * (every 1/10 of a second).  The delays specified in the mail loop 
 * are in terms of "ticks", each one representing one iteration 
 * through this loop.
 */
void mainloop(glob)
Glob *glob;
{
  Player   *player;
  Player   *selection[2];
  Window    window1, window2;
  Display  *display1, *display2;
  int       localx, localy, x, sleeptime = TICK;

  window1 =  XtWindow(XtNameToWidget(glob->Toplevel1,"*display"));
  display1 = XtDisplay (glob->Toplevel1);

  if (glob->numscreens == 2) {
    window2 =  XtWindow(XtNameToWidget(glob->Toplevel2,"*display"));
    display2 = XtDisplay (glob->Toplevel2);
  }
  selection[0] = &glob->player1; 
  selection[1] = &glob->player2;

  glob->player1.locked = 1;
  glob->player2.locked = 1;
  glob->gamestate      = BEFORE;
  glob->gametimer      = BEFORETIME;
  while (glob->gamestate == BEFORE ||
	 glob->gamestate == FIGHT ||
	 glob->gamestate == DURING ||
	 glob->gamestate == END ||
	 glob->gamestate == POSE) {

    /* background */
    DrawBack(glob, glob->Toplevel1, glob->blackgc1, glob->temp1, 
	     glob->currentbg->backg1, None, 
	     0 + glob->currentbg->xoffset, 
	     glob->currentbg->height - glob->playheight + 
             glob->currentbg->yoffset,
	     glob->playwidth, glob->playheight);
    if (glob->numscreens == 2) {
      DrawBack(glob, glob->Toplevel2, glob->blackgc2, glob->temp2, 
	       glob->currentbg->backg2, None, 
	       0 + glob->currentbg->xoffset, 
	       glob->currentbg->height - glob->playheight + 
	       glob->currentbg->yoffset,
	       glob->playwidth, glob->playheight);
    }
    
    /* grey out the background if needed */
    if (glob->dimbackground) {
      DimIt(glob);
    }

    /* draw time if it's during a match */
    if (glob->gamestate == DURING) {
      DrawTime(glob);
    }

    /* deal with global objects */
    UpdateObjects(glob, &glob->objectlist, BACK);

    /* foreach player */
    for (x = 0; x < 2; x++) {
      player = selection[x];

      /* handle details about updating the player's state */
      UpdatePlayerState(glob, player, selection[1-x]);

      /* calculate player position (position + offset of image) */
      localx = player->x + player->Images1[player->moves[player->sequence].
					   frameindex[player->seqnum]].offsetx;
      localy = player->y + player->Images1[player->moves[player->sequence].
					   frameindex[player->seqnum]].offsety;


      /* draw that character */
      if (player->frozen == 2) {
	/* swap foreground and background colors if freeze rayed */
	DrawIt(glob, glob->Toplevel1, glob->bluegc1, glob->temp1, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].pm, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].mask, 
	       localx, localy, 	       
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].width, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].height,
	       0, 0);
      } else {
	/* draw normal version */
	DrawIt(glob, glob->Toplevel1, glob->blackgc1, glob->temp1, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].pm, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].mask, 
	       localx, localy, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].width, 
	       player->Images1[player->moves[player->sequence].
			       frameindex[player->seqnum]].height,
	       0, 0); 
      } 	/* end else if frozen */

      if (glob->numscreens == 2) {
	if (player->frozen == 2) {
	  /* swap foreground and background colors if freeze rayed */
	  DrawIt(glob, glob->Toplevel2, glob->bluegc2, glob->temp2, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].pm, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].mask, 
		 localx, localy, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].width, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].height, 
		 0, 0);
	} else {
	  DrawIt(glob, glob->Toplevel2, glob->blackgc2, glob->temp2, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].pm, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].mask, 
		 localx, localy, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].width, 
		 player->Images2[player->moves[player->sequence].
				 frameindex[player->seqnum]].height, 
		 0, 0);
	}	/* end else if frozen */
      }

      /* deal with teleporting */
      if (player->teleport) {
	DoTeleport(glob, player);
      }

      /* deal with projectiles */
      if (player->projectiles)
	UpdateObjects(glob, &player->projectiles, ANYWHERE);

      DrawPowerBar(glob, glob->Toplevel1, glob->blackgc1, glob->whitegc1, 
		   glob->temp1, player->strength, x, player->wins);
      if (glob->numscreens == 2) {
	DrawPowerBar(glob, glob->Toplevel2, glob->blackgc2, glob->whitegc2,
		     glob->temp2, player->strength, x, player->wins);
      }

      /* check for intersections */
      PlayerIntersection (glob, player);

      /* give the computer a chance to do something */
      ComputerPlay(glob, player);
    }	/* end for player */

    /* deal with global objects */
    UpdateObjects(glob, &glob->objectlist, FRONT);

    UpdateGameState(glob);

    /* draw temp pixmap to screen window */
    XCopyArea(XtDisplay(glob->Toplevel1), glob->temp1, window1,
	      glob->blackgc1, 0, 0, 
	      glob->playwidth, glob->playheight + ShakeIt(glob), 
	      0, 0 - ShakeIt(glob));

    if (glob->numscreens == 2) {
      XCopyArea(XtDisplay(glob->Toplevel2), glob->temp2, window2,
		glob->blackgc2, 0, 0, 
		glob->playwidth, glob->playheight + ShakeIt(glob), 
		0, 0 - ShakeIt(glob));
      XFlush(display2);
    }

    XFlush(display1);
    Update(glob);

    usleep(sleeptime);

    glob->player1.sleep--;
    glob->player2.sleep--;
  }	/* end while () */
}
