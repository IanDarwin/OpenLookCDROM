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
 * routine to determine how the screen should shake
 */
int
ShakeIt(glob) 
Glob *glob;
{
  int offset = 0;
  if (glob->shake > 0 && glob->shake < 20) {
    switch (glob->shake % 4) {
    case 1: offset = 2;
      if (glob->shake == 1) offset = 6;
      break;
    case 2: offset = 1;
      break;
    case 3: offset = -1;
      break;
    }
    if (glob->shake == 16) 
      glob->shake = 0;
  }
  return offset;
}
/*
 * routine to determine how the screen should be offset for the 
 * DrawIt() routines.
 */
int
DrawItOffset(glob) 
Glob *glob;
{
  int offset = 0;
  if (glob->shake) {
    switch (glob->shake) {
    case 100: offset = 30;
      break;
    case 101: offset = 60;
      break;
    case 102: offset = 90;
      break;
    case 103: offset = 110;
      break;
    case 104: offset = 120;
      break;
    case 105: offset = 123;
      break;
    case 106: 
    case 107:
    case 108: 
    case 109: 
    case 110: 
    case 111:
    case 112:
    case 113:
    case 114:
    case 115:
    case 116:
    case 117:
    case 118:
    case 119:
    case 120:
      offset = 125;
      break;
    }
    if (glob->shake == 120) 
      glob->shake = 0;
  }
  return offset;
}


/*
 * draw a pixmap (with mask) on the appropriate destination pm 
 */
/* make lint happy */
/*ARGSUSED*/
void DrawIt(glob, Toplevel, Mygc, dest, pm, mask, x, y, 
	    width, height, startoffset, endoffset)
Glob *glob;
Widget Toplevel;
GC Mygc;
Pixmap dest, pm, mask;
int x, y, width, height, startoffset, endoffset;
{
  register Display *display;
  
  display = XtDisplay (Toplevel);

  XSetClipMask (display, Mygc, mask); 
  XSetClipOrigin(display, Mygc, x, y + DrawItOffset(glob));
  XCopyPlane (display, pm, dest, Mygc,
	      0, startoffset, 
	      width,  height, 
	      x, y + DrawItOffset(glob) - startoffset - endoffset, 
	      1);
  XSetClipMask (display, Mygc, None); 
}

/*
 * draw a background (with mask) on the appropriate destination pm 
 *
 * Note: the only difference between this and DrawIt() is that the
 * x and y offsets are for the SOURCE image, as opposed to the destination.
 * i.e., this is for backgrounds which are bigger than the output window
 * as opposed to the characters which are smaller and need to be positioned
 * within the output window.  No clipping is done either.
 */
/*ARGSUSED*/
void DrawBack(glob, Toplevel, Mygc, dest, pm, mask, x, y, width, height)
Glob *glob;
Widget Toplevel;
GC Mygc;
Pixmap dest, pm, mask;
int x, y, width, height;
{
  register Display *display;
  
  display = XtDisplay (Toplevel);

  XSetClipMask (display, Mygc, None); 
  XCopyPlane (display, pm, dest, Mygc,
	      x,  y - DrawItOffset(glob), width, height, 0, 0, 1);
}

/*
 * draw the strength indicator (power bar) on the side of the screen
 */
void DrawPowerBar(glob, Toplevel, Mygc, gc2, pix, power, side, wins)
Glob   *glob;
Widget  Toplevel;
GC      Mygc, gc2;
Pixmap  pix;
int     power;
int     side;
int     wins;
{

  register Display *display;

  display = XtDisplay (Toplevel);
  XSetClipMask (display, Mygc, None); 
  XSetClipMask (display, gc2, None);  

  if (side == 0) {
    /* left player */
    XFillRectangle(display, pix, Mygc, 3, 3, 108, 18);
    XFillRectangle(display, pix, gc2, 5, 5, 104, 14);
    XFillRectangle(display, pix, Mygc, 7, 7, 100, 10);
    XFillRectangle(display, pix, gc2, 7, 7, power, 10);
    /* draw a little circle to indicate a win */
    if (wins) {
      XFillArc(display, pix, gc2, 8, 25, 
	       10, 10, 0, 64*360);
    }
  } else {
    /* right player */
    XFillRectangle(display, pix, Mygc, glob->playwidth - 117, 3, 108, 18);
    XFillRectangle(display, pix, gc2, glob->playwidth - 115, 5, 104, 14);
    XFillRectangle(display, pix, Mygc, glob->playwidth - 113, 7, 100, 10);
    XFillRectangle(display, pix, gc2, glob->playwidth - 113, 7, power, 10);
    /* draw a little circle to indicate a win */
    if (wins) {
      XFillArc(display, pix, gc2, glob->playwidth - 109, 25, 
	       10, 10, 0, 64*360);
    }
  }

}

void finddigits(glob, digit, screen1, screen2)
Glob    *glob;
int     digit;
Pixmap  *screen1, *screen2;
{
  static firsttime = 1;
  static Pixmap digita[10];
  static Pixmap digitb[10];
  if (firsttime) {
    digita[0] = glob->zero1; digitb[0] = glob->zero2;
    digita[1] = glob->one1; digitb[1] = glob->one2;
    digita[2] = glob->two1; digitb[2] = glob->two2;
    digita[3] = glob->three1; digitb[3] = glob->three2;
    digita[4] = glob->four1; digitb[4] = glob->four2;
    digita[5] = glob->five1; digitb[5] = glob->five2;
    digita[6] = glob->six1; digitb[6] = glob->six2;
    digita[7] = glob->seven1; digitb[7] = glob->seven2;
    digita[8] = glob->eight1; digitb[8] = glob->eight2;
    digita[9] = glob->nine1; digitb[9] = glob->nine2;
    firsttime = 0;
  }

  *screen1 = digita[digit];
  *screen2 = digitb[digit];
  return;
}

void DrawTime(glob)
Glob    *glob;
{
  int x, posx, posy;
  Pixmap  screen1, screen2;
  int needdigit = 0;

  /* 
   * Make timer a bit closer to seconds.  If we
   * wanted to be really correct we could do the
   * conversion based on the varaible "sleeptime"
   * in mainloop() but obviously we don't.
   */
  x = glob->gametimer / 5;
  posy = 5;
  /* center 3 digits, start at hundreds place (32 bits wide, 3*32/2 = 48) */
  posx = glob->playwidth / 2 - (3 * NUMBERW / 2);
  if (x >99) {
    needdigit = 1;
    finddigits (glob, x/100, &screen1, &screen2);
    DrawIt(glob, glob->Toplevel1, glob->whitegc1, glob->temp1, 
	   screen1, screen1, posx, posy, NUMBERW, NUMBERH, 0, 0);
    if (glob->numscreens == 2) {
      DrawIt(glob, glob->Toplevel2, glob->whitegc2, glob->temp2, 
	     screen2, screen2, posx, posy, NUMBERW, NUMBERH, 0, 0);
    }
  }
  x %=  100;
  posx += NUMBERW;
  /* tens place */
  if (x > 9 || needdigit) {
    finddigits (glob, x/10, &screen1, &screen2);
    DrawIt(glob, glob->Toplevel1, glob->whitegc1, glob->temp1, 
	   screen1, screen1, posx, posy, NUMBERW, NUMBERH, 0, 0);
    if (glob->numscreens == 2) {
      DrawIt(glob, glob->Toplevel2, glob->whitegc2, glob->temp2, 
	     screen2, screen2, posx, posy, NUMBERW, NUMBERH, 0, 0);
    }
  }
  x %=  10;
  posx += NUMBERW;
  /* ones place */
  finddigits (glob, x, &screen1, &screen2);
  DrawIt(glob, glob->Toplevel1, glob->whitegc1, glob->temp1, 
	 screen1, screen1, posx, posy, NUMBERW, NUMBERH, 0, 0);
  if (glob->numscreens == 2) {
    DrawIt(glob, glob->Toplevel2, glob->whitegc2, glob->temp2, 
	   screen2, screen2, posx, posy, NUMBERW, NUMBERH, 0, 0);
  }
}

/*
 * draw a flashing message on the screen 
 */
void 
DrawText(glob, x, y, message, attribute)
Glob    *glob;
int      x;
int      y;
char    *message;
int      attribute;	/* attr = 0->normal, 1->red, 2->small */
{

  GC gc1, gc2, gcother1, gcother2;

  switch (attribute) {
  case 0:
    gc1      = glob->blackgc1;
    gc2      = glob->blackgc2;
    gcother1 = glob->whitegc1;
    gcother2 = glob->whitegc2;
    break;
  case 1:
    gc1      = glob->redgc1;
    gc2      = glob->redgc2;
    gcother1 = glob->whitegc1;
    gcother2 = glob->whitegc2;
    break;
  case 2:
    gc1      = glob->smallgc1;
    gc2      = glob->smallgc2;
    gcother1 = glob->smallgc1;
    gcother2 = glob->smallgc1;
    break;
  }

  if (glob->gametimer % 2) {
    XDrawString(XtDisplay(glob->Toplevel1), glob->temp1, gc1,
		x, y, message, strlen(message));
  } else {
    XDrawString(XtDisplay(glob->Toplevel1), glob->temp1, gcother1,
		x, y, message, strlen(message));
  }		/* end if gametimer % 2 */

  /* handle second screen if present */
  if (glob->numscreens == 2) {
    if (glob->gametimer % 2) {
      XDrawString(XtDisplay(glob->Toplevel2), glob->temp2, gc2,
		  x, y, message, strlen(message));
    } else {
      XDrawString(XtDisplay(glob->Toplevel2), glob->temp2, gcother2,
		  x, y, message, strlen(message));
    }	/* end if gametimer % 2 */
  }	/* end numscreens */
}

