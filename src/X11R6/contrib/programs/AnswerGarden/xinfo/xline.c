/*************************************************************
**
**   LINE.C
**       for X version 11
**
**       A very simple (and stupid) X GRAPHICS program.  This 
**       opens up a single blank window, puts up a line starting
**       in the upper left corner of the window, and waits until
**       of the screen, and hangs out until button 2 is clicked
**       in the window.  
**
**       Mark Ackerman
**       MIT/Project Athena
**       January 4, 1989
**       Amended October 10, 1989
**
*************************************************************/

#include <stdio.h>
#include <X11/Xlib.h>           /* the X library */
#include <X11/Xutil.h>

   /* a few arbitary constants */
#define START_X         10
#define START_Y         20
#define WINDOW_WIDTH   225   
#define WINDOW_HEIGHT  400
#define BORDER_WIDTH     1

   /* the same ol' variables */

Display *the_display;
Window the_window;
int the_screen;
Window root_window;
XSizeHints size_hints;
XEvent the_event;


GC the_GC;                         /* the graphics context */
XGCValues the_GC_values;           /* for handling the GC */

main(argc,argv)
     int argc;
     char *argv[];
{

  int i;

     /* Set the display to be the default display (ie, your
	display as given in the environment variable DISPLAY). */

  if ((the_display = XOpenDisplay("")) == NULL)
     {
        printf("in %s:  can't open display\n",argv[0]);
	return(-1);
     }

     /* A few useful values. */
  the_screen = DefaultScreen(the_display);

  root_window = DefaultRootWindow(the_display);

     /* Same as always: set up the window manager size hints. */
  size_hints.x = START_X;
  size_hints.y = START_Y;
  size_hints.width = WINDOW_WIDTH;
  size_hints.height = WINDOW_HEIGHT;
  size_hints.flags = PSize|PPosition;

     /* Create a window of fixed size, origin, and borderwidth.
	The window will have a black border and white background. */

  the_window = XCreateSimpleWindow(the_display,
		     root_window,size_hints.x,size_hints.y,
		     size_hints.width,size_hints.height,BORDER_WIDTH,
		     BlackPixel(the_display,the_screen),
		     WhitePixel(the_display,the_screen));


    /* Set the usual properties. */
  XSetStandardProperties(the_display,the_window,"My Window","My Icon",
			 None,argv,argc,&size_hints);

    /* Select exposure events and button press events. */
  XSelectInput(the_display,the_window,ExposureMask|ButtonPressMask|
	       ButtonReleaseMask);

    /* Tell the server to make the window visible. */
  XMapWindow(the_display,the_window);

    /* Use only the default GC settings for now. */

  the_GC = XCreateGC(the_display,the_window,None,&the_GC_values);
  XSetForeground(the_display,the_GC,BlackPixel(the_display,the_screen));
  XSetBackground(the_display,the_GC,WhitePixel(the_display,the_screen));


    /* The main event loop.  Note that the first event will be an
       exposure event which will cause the first graphics operation.
       On any subsequent exposure events, too, redraw the line.
       Otherwise, exit on a button up. */


  while (True)
    {
      XNextEvent(the_display,&the_event);

      switch (the_event.type)
	  {
	    case ButtonRelease:
	      if (the_event.xbutton.button == Button3)
		  exit();
	      break;
	    case Expose:
	      /* On expose events, redraw. */
	      XDrawLine(the_display,the_window,the_GC,5,5,100,100);
	      break;
	    default:
	      /* Ignore everything else. */
	      break;  
	  }
  }
}



