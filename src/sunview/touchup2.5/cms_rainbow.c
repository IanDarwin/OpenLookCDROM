
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb
	   (If nobody is home at any of the above addresses try:
		S72QKRE@TOWSONVX.BITNET			        )

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/
/**************************************************************************
	file:  cms_rainbow.c
	purpose: this file contains that funciton that initalizes the
		the default color table for color sun usage

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Sat Jun  4 13:22:45 EST 1988
		author:	rayk
		changes:fixed color pallet so that it was continious
**************************************************************************/

extern unsigned char red[256],green[256],blue[256];

set_colorentry(i, r, g, b)
{
  red[i] =r;
  green[i] =g;
  blue[i]=b;
}



/*
 * set a rainbow color map with most all of the colors
 */
init_colortable()
{
    int i, red, green, blue;
    set_colorentry(0, 255, 255, 255);	/* white */
    set_colorentry(1, 0, 0, 0);		/* black */
    red = blue = green = 0;

    for (i = 2; i < 30; i++) {
        red += 9;
        set_colorentry(i, red, green, blue);
    }
    set_colorentry(30, 255, 0, 0);      /* red */
    red = 255; blue = green = 0;
    for (i = 30; i < 62; i++) {
        green += 6;
        set_colorentry(i, red, green, blue);
    }
    set_colorentry(62, 255, 188, 0);    /* orange */ /* note diff = 33 */
    red = 255; blue = 0; green = 188;
    for (i = 63; i < 96; i++) {
        green += 2;
        set_colorentry(i, red, green, blue);
    }
    set_colorentry(96, 255, 255, 0);    /* yellow */
    red = 255; blue = 0; green = 255;
    for (i = 97; i < 128; i++) {
        red -= 7;
        set_colorentry(i, red, green, blue);
    }
    green = blue = 255; red=0; 
    for (i = 128; i < 136; i++) {
        blue -= 12;
        set_colorentry(i, red, green, blue);
    }
    red = blue = 16; green = 239;
    for (i = 136; i < 163; i++) {
        green -= 8;
        blue += 8;
        set_colorentry(i, red, green, blue);
    }
    red = green = 50; blue = 255;
    for (i = 163; i < 176; i++) {
       green += 10;
       red += 10;
        set_colorentry(i, red, green, blue);
    }
    red = 70; green = 0; blue = 255;
    for (i = 176; i < 200; i++) {
        red += 7;
        set_colorentry(i, red, green, blue);
    }

    red = blue = 255; green = 80;
    for (i = 200; i < 208; i++) {
        blue -= 13;
        set_colorentry(i, red, green, blue);
    }

    red = blue = 112; green = 64;
    for (i = 208; i < 218; i++) {
        blue += 13;
        red += 13;
        set_colorentry(i, red, green, blue);
    }
    red = blue = 255; green = 105;
    for (i = 218; i < 239; i++) {
        green += 7;
        set_colorentry(i, red, green, blue);
    }
    set_colorentry(239, 255, 255, 255); /* white */
    red = blue = green = 255;
    for (i = 239; i <= 255; i++) {
        green -= 9;
        red -= 9;
        blue -= 9;
        set_colorentry(i, red, green, blue);
    }
    set_colorentry(254, 255, 255, 255);	/* white for passive foreground */
}
