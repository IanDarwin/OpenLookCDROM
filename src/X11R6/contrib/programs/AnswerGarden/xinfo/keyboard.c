/*************************************************************
**
**   KEYBOARD.C
**       for X version 11
**
**       A basic X program to show keyboard input events.
**
**       Mark Ackerman
**       MIT/Project Athena
**       June 22, 1987
**       Amended October 11, 1989
**
*************************************************************/

#include <stdio.h>
#include <X11/Xlib.h>           /* the X library */
#include <X11/Xutil.h>          /* the X library */

   /* a few arbitary constants */
#define START_X         10
#define START_Y         20
#define WINDOW_WIDTH   225   
#define WINDOW_HEIGHT  400
#define BORDER_WIDTH     1
#define KEY_STR_LENGTH  20


Display *the_display;           /* the display that will be used */
Window the_window;              /* the window that will be opened */
int the_screen;                 /* the screen that will be used */
Window root_window;             /* the root window on the screen */
XSizeHints size_hints;          /* size hints for the window manager */
XEvent the_event;               /* the structure for the input event */

XComposeStatus compose_status;  /* a required variable for a call */
char str[KEY_STR_LENGTH];       /* a required variable for a call */

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

  root_window = RootWindow(the_display,the_screen);

     /* Set the size hints for the window manager. */

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

    /* Round up the usual suspects for the window manager. */

  XSetStandardProperties(the_display,the_window,"My Window","My Icon",
			 None,argv,argc,&size_hints);

    /* Indicate what events you want for the window. */

  XSelectInput(the_display,the_window,ExposureMask|KeyPressMask);

    /* Tell the server to make the window visible. */

  XMapWindow(the_display,the_window); 

    /* Enter the main input event loop. */

  while (True)
    {
        /* Get the next event.  Note that the first event will be
	   an exposure event. */
      XNextEvent(the_display,&the_event);

        /* If the event is a key press event, remap the event to
	   the correct ASCII sequence.  Print the result.  Note
	   that the returned string is not null-terminated.  */

      switch (the_event.type)
	  {
	    case KeyPress:
	      i = XLookupString(&the_event, str, KEY_STR_LENGTH, NULL, 
				&compose_status);
	      str[i] = (char)0;
	      printf("from program: found %s\n",str);
	      break;
	    case Expose:
	      /* Ignore exposure events for now since nothing is displayed
		 on the screen. */
	      break;
	    default:
	      /* Ignore any other type of event */
	      break;  
	  }
  }
}

