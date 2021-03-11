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

static void
playagainCB(w, doagain, calldata)
Widget   w;
int     *doagain;
caddr_t  calldata;
{
  *doagain = 1;
  return;
}

static void
quitCB(w, glob, calldata)
Widget   w;
Glob     *glob;
caddr_t  calldata;
{
  Quit(glob);
}

static void
leftplayerhumanCB(w, glob, calldata)
Widget   w;
Glob     *glob;
caddr_t  calldata;
{
  setplayertype(glob, LEFT, HUMAN, w);
}

static void
rightplayerhumanCB(w, glob, calldata)
Widget   w;
Glob     *glob;
caddr_t  calldata;
{
  setplayertype(glob, RIGHT, HUMAN, w);
}

static void
leftplayercompCB(w, glob, calldata)
Widget   w;
Glob     *glob;
caddr_t  calldata;
{
  setplayertype(glob, LEFT, COMPUTER, w);
}

static void
rightplayercompCB(w, glob, calldata)
Widget   w;
Glob     *glob;
caddr_t  calldata;
{
  setplayertype(glob, RIGHT, COMPUTER, w);
}

setplayertype(glob, player, type, w)
Glob   *glob;
int     player;
int     type;
Widget  w;
{
  int screennumber;

  if (glob->numscreens == 2) {
    /* we're in a two screen, two player mode */
    /*find out which screen we are */
    screennumber = (XtDisplay(w) == XtDisplay(glob->Toplevel1))?1:2;

    if (type == COMPUTER) {
      if (player == LEFT) {
	if ((screennumber == 1 && glob->screen1 == LEFT) || 
	    (screennumber == 2 && glob->screen2 == LEFT)) {
	  /* let them change their player to be the computer */
	  RemoveBinding(glob, screennumber);
	  glob->player1.playertype = COMPUTER;
	}
      } else if (player == RIGHT) {
	if ((screennumber == 1 && glob->screen1 == RIGHT) || 
	    (screennumber == 2 && glob->screen2 == RIGHT)) {
	  /* let them change their player to be the computer */
	  RemoveBinding(glob, screennumber);
	  glob->player2.playertype = COMPUTER;
	}
      }
    } else if (type == HUMAN) {
      /* make sure selected character is currently the computer */
      if (player == LEFT && glob->player1.playertype == COMPUTER) {
	RemoveBinding(glob, screennumber);
	glob->player1.playertype = HUMAN;
	AddBinding(glob, &glob->player1, screennumber, LEFT);
      } else if (player == RIGHT && glob->player2.playertype == COMPUTER) {
	RemoveBinding(glob,screennumber);
	glob->player2.playertype = HUMAN;
	AddBinding(glob, &glob->player2, screennumber, RIGHT);
      }
    }
  } else {
    /* only one screen, so it's just us vs. the computer */
    if (type == COMPUTER) {
      RemoveBinding(glob, 1);
      if (player == LEFT) {
	glob->player1.playertype = COMPUTER;
      } else {
	glob->player2.playertype = COMPUTER;
      }
    } else {
      if (player == LEFT) {
	/* left wants to be human, force right to be computer */
	RemoveBinding(glob, 1);
	glob->player2.playertype = COMPUTER;
	glob->player1.playertype = HUMAN;
	AddBinding(glob, &glob->player1, 1, LEFT);
      } else {
	/* right wants to be human, force left to be computer */
	RemoveBinding(glob, 1);
	glob->player1.playertype = COMPUTER;
	glob->player2.playertype = HUMAN;
	AddBinding(glob, &glob->player2, 1, RIGHT);
      }
    }
  }
}

static void 
volumejumpCB(w, glob, percent)
Widget   w;
Glob    *glob;
float   *percent;
{
  Arg     args[15];
  int     newgain;
  Widget  wid;
  char    buf[50];
  int    *volume;

  newgain = *percent * MAX_VOLUME;

  if (w == XtNameToWidget(glob->Toplevel1,"*volume")) {
    volume = &glob->volume1;
    wid = glob->Toplevel1;
  } else {
    volume = &glob->volume2;
    wid = glob->Toplevel2;
  }

  if (newgain < MIN_VOLUME) newgain = MIN_VOLUME;
  
  if (newgain != *volume) {
    *volume = newgain;
    sprintf(buf, VOLUME_FORMAT, *volume);
    XtVaSetValues(XtNameToWidget(wid, "*scrolllabel"), XtNlabel, buf, NULL);
    /* adjustvolume(glob); */
  }
}

static void
volumescrollCB(w, glob, position)
Widget   w;
Glob    *glob;
int      position;
{

  int     newgain;
  char    buf[50];
  int    *volume;
  Widget  wid;

  if (w == XtNameToWidget(glob->Toplevel1,"*volume")) {
    volume = &glob->volume1;
    wid = glob->Toplevel1;
  } else {
    volume = &glob->volume2;
    wid = glob->Toplevel2;
  }

  newgain = *volume + (position > 0 ? -1 : 1);

  if (newgain < MIN_VOLUME) 
    newgain = MIN_VOLUME;
  else if (newgain > MAX_VOLUME)
    newgain = MAX_VOLUME;

  if (newgain != *volume) {
    *volume = newgain;
    sprintf(buf, VOLUME_FORMAT, *volume);
    XtVaSetValues(XtNameToWidget(wid, "*scrolllabel"), XtNlabel, buf, NULL);
    
    XawScrollbarSetThumb(XtNameToWidget(wid, "*volume"),
			 (float) *volume / MAX_VOLUME, -1.0);
    /* adjustvolume(glob); */

  }
}


Widget makecontrolpanel(glob, parent, pleasecontinue)
Glob *glob;
Widget parent;
int *pleasecontinue;
{

  /* widget variables */
  int cnt, maxx, x;
  Arg args[15];
  Display *display1, *display2;
  Widget label, frame, button1, button2, scroll;
  Widget frame1, label1, button3, button4;
  Widget leftframe, rightframe, label2, button5, button6;
  char buf[50];

  /* make a frame to hold it all */
  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 2);                   cnt++;
  XtSetArg (args[cnt], XtNbottom, XawChainTop);              cnt++;
  frame = XtCreateManagedWidget ("controlframe", formWidgetClass, 
				 parent, args, cnt);
  /* make a scrollbar */
  cnt = 0;
  label = XtCreateManagedWidget ("scrolllabel", labelWidgetClass, frame,
				  args, cnt);
  sprintf(buf, VOLUME_FORMAT, DEFAULT_VOLUME);
  XtVaSetValues(label, XtNlabel, buf, NULL);

  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, label);                   cnt++;
  XtSetArg (args[cnt], XtNorientation, XtorientHorizontal);   cnt++;
  scroll = XtCreateManagedWidget ("volume", scrollbarWidgetClass, frame,
				  args, cnt);

  XawScrollbarSetThumb(scroll, ((float) DEFAULT_VOLUME) / MAX_VOLUME, -1.0);
  XtAddCallback(scroll, XtNscrollProc, volumescrollCB, glob);
  XtAddCallback(scroll, XtNjumpProc, volumejumpCB, glob);
  
  cnt = 0;
  XtSetArg (args[cnt], XtNfromHoriz, scroll);                 cnt++;
  button1 = XtCreateManagedWidget ("quit", commandWidgetClass, frame,
				  args, cnt);
  XtAddCallback(button1, XtNcallback, quitCB, glob);

  cnt = 0;
  XtSetArg (args[cnt], XtNfromHoriz, scroll);                 cnt++;
  XtSetArg (args[cnt], XtNfromVert, button1);                 cnt++;
  XtSetArg (args[cnt], XtNsensitive, 0);                      cnt++;
  button2 = XtCreateManagedWidget ("Play again?", commandWidgetClass, frame,
				  args, cnt);
  XtAddCallback(button2, XtNcallback, playagainCB, pleasecontinue);


  /* make a frame to hold toggle frames */
  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 2);                   cnt++;
  XtSetArg (args[cnt], XtNbottom, XawChainTop);              cnt++;
  XtSetArg (args[cnt], XtNfromHoriz, button2);               cnt++;
  frame1 = XtCreateManagedWidget ("typeframes", formWidgetClass, 
				 frame, args, cnt);

  /* make a frame to hold toggle buttons */
  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 0);                   cnt++;
  XtSetArg (args[cnt], XtNbottom, XawChainTop);              cnt++;
  leftframe = XtCreateManagedWidget ("player1typeframe", formWidgetClass, 
				 frame1, args, cnt);

  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 0);                   cnt++;
  label1 = XtCreateManagedWidget ("Left player type:", labelWidgetClass, 
				  leftframe, args, cnt);
  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, label1);                 cnt++; 
  button3 = XtCreateManagedWidget ("Computer ", commandWidgetClass, 
				   leftframe, args, cnt);
  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, button3);                 cnt++;
  XtSetArg (args[cnt], XtNradioGroup, button3);               cnt++;
  button4 = XtCreateManagedWidget ("Human    ", commandWidgetClass, 
				   leftframe, args, cnt);

  XtAddCallback(button3, XtNcallback, leftplayercompCB, glob);
  XtAddCallback(button4, XtNcallback, leftplayerhumanCB, glob);

  /* make a frame to hold toggle buttons */
  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 0);                   cnt++;
  XtSetArg (args[cnt], XtNbottom, XawChainTop);              cnt++;
  XtSetArg (args[cnt], XtNfromHoriz, leftframe);             cnt++;
  rightframe = XtCreateManagedWidget ("typeframe", formWidgetClass, 
				 frame1, args, cnt);
  cnt = 0;
  XtSetArg (args[cnt], XtNborderWidth, 0);                   cnt++;
  label1 = XtCreateManagedWidget ("Right player type:", labelWidgetClass, 
				  rightframe, args, cnt);
  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, label1);                 cnt++; 
  button3 = XtCreateManagedWidget ("Computer ", commandWidgetClass, 
				   rightframe, args, cnt);
  cnt = 0;
  XtSetArg (args[cnt], XtNfromVert, button3);                cnt++;
  XtSetArg (args[cnt], XtNradioGroup, button4);              cnt++;
  button4 = XtCreateManagedWidget ("Human    ", commandWidgetClass, 
				   rightframe, args, cnt);

  XtAddCallback(button3, XtNcallback, rightplayercompCB, glob);
  XtAddCallback(button4, XtNcallback, rightplayerhumanCB, glob);

/*
put in callbacks
 */
  

  XtVaSetValues(label, XtNwidth, 100, NULL);
  XtVaSetValues(scroll, XtNwidth, 100, NULL);
  XtVaSetValues(scroll, XtNheight, 20, NULL);
  XtVaSetValues(scroll, XtNy, 50, NULL);

  return frame;
}
