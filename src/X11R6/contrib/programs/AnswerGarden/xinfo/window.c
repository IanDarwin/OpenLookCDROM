/*************************************************************
**
**   WINDOW.C
**       for X version 11
**
**       The most basic X program you can have.  This program
**       opens up a single blank window, pauses, and exits.
**
**       Mark Ackerman
**       MIT/Project Athena
**       June 22, 1987
**
*************************************************************/

#include <stdio.h>
#include <X11/Xlib.h>           /* the X library */

   /* a few arbitary constants */
#define START_X         10
#define START_Y         20
#define WINDOW_WIDTH   225   
#define WINDOW_HEIGHT  400
#define BORDER_WIDTH     1

Display *the_display;           /* the display for the program */
Window the_window;              /* the window that will be opened */
int the_screen;                 /* the screen on the display that will be 
				   used */
Window root_window;             /* the root window on the display */
XSizeHints size_hints;          /* size hints/properties for the window
				   manager */

main(argc,argv)
     int argc;
     char *argv[];
{

     /* Set the display to be the default display (ie, your
	display as given in the environment variable DISPLAY).  */

  if ((the_display = XOpenDisplay("")) == NULL)
     {
        printf("in %s:  can't open display\n",argv[0]);
	return(-1);
     }

     /* Get a few useful values.  */

  the_screen = DefaultScreen(the_display);

  root_window = DefaultRootWindow(the_display);      

    /* Set the size hints for the window manager.  These will
       be used below.  */

  size_hints.x = START_X;
  size_hints.y = START_Y;
  size_hints.width = WINDOW_WIDTH;
  size_hints.height = WINDOW_HEIGHT;
  size_hints.flags = PSize|PPosition;

     /* Create a window of fixed size, origin, and borderwidth.
	The window will have a black border and white background. */

  the_window = XCreateSimpleWindow(the_display,root_window,
		     size_hints.x,size_hints.y,size_hints.width,
		     size_hints.height,BORDER_WIDTH,
		     BlackPixel(the_display,the_screen),
		     WhitePixel(the_display,the_screen));

     /* Set the usual properties including the window manager size
	hints.  */

  XSetStandardProperties(the_display,the_window,
			 "My Window","My Icon",
			 None,argv,argc,&size_hints);

     /* Tell the server to make the window visible. */

  XMapWindow(the_display,the_window); 

  XFlush(the_display);   /* Don't forget this!  You won't see anything
			    without it. */

  sleep(30);             /* Pause for 30 seconds.  You can replace
			    this with a large number and control-C 
			    out.  */

}




