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

/*
 * dim the background by drawing a stippled rectangle over it all
 */

void DimIt(glob) 
Glob *glob;

{
  Pixmap temp1, temp2;
  Display *display1, *display2;

  switch (glob->dimbackground) {
  case 4:   temp1 = glob->grey4a; temp2 = glob->grey4b;
    break;
  case 8:   temp1 = glob->grey8a; temp2 = glob->grey8b;
    break;
  case 16:  temp1 = glob->grey16a; temp2 = glob->grey16b;
    break;
  case 32:  temp1 = glob->grey32a; temp2 = glob->grey32b;
    break;
  case 64:  temp1 = glob->grey64a; temp2 = glob->grey64b;
    break;
  case 128: temp1 = glob->grey128a; temp2 = glob->grey128b;
    break;
  default: (void) fprintf(stderr, "Got a bad value for dimbackground (%d)\n",
			  glob->dimbackground);
    temp1 = 0; temp2 = 0;
    break;
  }

  display1 = XtDisplay(glob->Toplevel1);
  XSetFillStyle(display1, glob->blackgc1, FillStippled);
  XSetStipple (display1, glob->blackgc1, temp1);
  XFillRectangle (display1, glob->temp1, glob->blackgc1,
		  0, 0, glob->playwidth, glob->playheight);
  XSetFillStyle(display1, glob->blackgc1, FillSolid);

  if (glob->numscreens == 2) {
    display2 = XtDisplay(glob->Toplevel2);
    XSetFillStyle(display2, glob->blackgc2, FillStippled);
    XSetStipple (display2, glob->blackgc2, temp2);
    XFillRectangle (display2, glob->temp2, glob->blackgc2,
		    0, 0, glob->playwidth, glob->playheight);
    XSetFillStyle(display2, glob->blackgc2, FillSolid);
  }
}

void DoTeleport(glob, player)
Glob   *glob;
Player *player;
{
  Pixmap temp1, temp2;
  Display *display1, *display2;
  Player *otherplayer;
  int localx, localy;

  /* otherplayer will be whatever "player" is not (i.e. player1 or player2) */
  otherplayer = (player == &glob->player1)?&glob->player2:&glob->player1;

  switch (player->teleport) {
  case 1:  temp1 = glob->grey4a; temp2 = glob->grey4b;
    break;
  case 2:  temp1 = glob->grey8a; temp2 = glob->grey8b;
    break;
  case 3:  temp1 = glob->grey16a; temp2 = glob->grey16b;
    break;
  case 4:  temp1 = glob->grey32a; temp2 = glob->grey32b;
    break;
  case 5:  temp1 = glob->grey64a; temp2 = glob->grey64b;
    break;
  case 6: temp1 = glob->grey128a; temp2 = glob->grey128b;
    break;
  case 7:  /* move the player */
    if (player->facing == RIGHT) {
      player->x = otherplayer->x - player->width/2;
    } else {
      player->x = otherplayer->x + player->width/2;
    }
    /* drop through to the next part (i.e. no break) */
  case 8:   temp1 = glob->grey128a; temp2 = glob->grey128b;
    break;
  case 9:   temp1 = glob->grey64a; temp2 = glob->grey64b;
    break;
  case 10:  temp1 = glob->grey32a; temp2 = glob->grey32b;
    break;
  case 11:  temp1 = glob->grey16a; temp2 = glob->grey16b;
    break;
  case 12:  temp1 = glob->grey8a; temp2 = glob->grey8b;
    break;
  case 13:  temp1 = glob->grey4a; temp2 = glob->grey4b;
    break;
  default: (void) fprintf(stderr, "Got a bad value for teleport (%d)\n",
			  player->teleport);
    temp1 = 0; temp2 = 0;
    break;
  }	/* end switch */

  localx = player->x + player->Images1[player->moves[player->sequence].
				       frameindex[player->seqnum]].offsetx;
  localy = player->y + player->Images1[player->moves[player->sequence].
				       frameindex[player->seqnum]].offsety;

  /* draw a stipple over the image */
  display1 = XtDisplay(glob->Toplevel1);
  XSetFillStyle(display1, glob->blackgc1, FillStippled);
  XSetStipple (display1, glob->blackgc1, temp1);
  XSetClipMask (display1, glob->blackgc1, 
		player->Images1[player->moves[player->sequence].
				frameindex[player->seqnum]].mask); 
  XSetClipOrigin(display1, glob->blackgc1, localx, localy);

  XFillRectangle (display1, glob->temp1, glob->blackgc1,
		  localx, localy, 
		  player->Images1[player->moves[player->sequence].
				  frameindex[player->seqnum]].width,
		  player->Images1[player->moves[player->sequence].
				  frameindex[player->seqnum]].height);
  XSetFillStyle(display1, glob->blackgc1, FillSolid);

  if (glob->numscreens == 2) {
    display2 = XtDisplay(glob->Toplevel2);
    XSetFillStyle(display2, glob->blackgc2, FillStippled);
    XSetStipple (display2, glob->blackgc2, temp2);
    XSetClipMask (display2, glob->blackgc2, 
		player->Images2[player->moves[player->sequence].
				frameindex[player->seqnum]].mask); 
    XSetClipOrigin(display2, glob->blackgc2, localx, localy);

    XFillRectangle (display2, glob->temp2, glob->blackgc2,
		    localx, localy, 
		    player->Images2[player->moves[player->sequence].
				    frameindex[player->seqnum]].width,
		    player->Images2[player->moves[player->sequence].
				    frameindex[player->seqnum]].height);
    XSetFillStyle(display2, glob->blackgc2, FillSolid);
  }

  /* bump up the value by one */
  player->teleport++;
  if (player->teleport == 14) 
    player->teleport = 0;

}
