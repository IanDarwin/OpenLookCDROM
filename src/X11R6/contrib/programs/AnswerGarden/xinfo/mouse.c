/*************************************************************
**
**   MOUSE.C
**       for X version 11
**
**       A basic X program to show mouse button input events.
**
**       Mark Ackerman
**       MIT/Project Athena
**       January 4, 1989
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


Display *the_display;           /* the display that will be used */
Window the_window;              /* the window that will be opened */
int the_screen;                 /* the screen that will be used */
Window root_window;             /* the root window on the screen */
XSizeHints size_hints;          /* size hints for the window manager */
XEvent the_event;               /* the structure for the input event */

Window the_subwindow;           /* the window that will be a child */

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

     /* Set the size hints for the window manager. */

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

    /* Round up the usual suspects for the window manager. */

  XSetStandardProperties(the_display,the_window,
			 "My Window","My Icon",
			 None,argv,argc,&size_hints);

    /* Create a child window inside the_window.  Note that no
       properties need be set for it. */

  the_subwindow = XCreateSimpleWindow(the_display,
		     the_window,10,10,60,60,BORDER_WIDTH,
		     BlackPixel(the_display,the_screen),
		     WhitePixel(the_display,the_screen));

    /* Indicate what events you want for the windows. */

  XSelectInput(the_display,the_window,
	       ExposureMask|ButtonPressMask|ButtonReleaseMask);
  XSelectInput(the_display,the_subwindow,
	       ExposureMask|ButtonPressMask|ButtonReleaseMask);

    /* Tell the server to make the windows visible. */

  XMapWindow(the_display,the_window); 
  XMapSubwindows(the_display,the_window); 

    /* Enter the main input event loop. */

  while (True)
    {
        /* Get the next event.  Note that the first event will be
	   an exposure event. */
      XNextEvent(the_display,&the_event);

        /* All of the event handling code is in a separate routine. */
      event_handler(the_display,the_subwindow,&the_event);
    }
}

event_handler(the_display,the_subwindow,the_event)
     Display *the_display;
     Window the_subwindow;
     XEvent *the_event;
{
  XButtonEvent *button_event;     /* to avoid casts */

    /* The outer if-else clauses check for the type of input event.
       The inner processing for a button down mouse event is dependent
       on the window in which the event occurred. */

  if (the_event->type == ButtonRelease)
    {
      button_event = (XButtonEvent *)the_event;
      printf("window is %ld, state is %d, button is %d, x is %d, y is %d\n",
	     button_event->window,button_event->state, button_event->button,
	     button_event->x, button_event->y);
  
        /* If the event was in the parent (outer) window, ignore unless
	   it's the middle button. */

      if (button_event->window != the_subwindow)
	switch(button_event->button)
	  {
	  case Button3:
	    exit();
	  default:
	    return;
	  }
       else
	  /* Else it's the subwindow, and send it to a routine to
	     handle this. */
	handle_subwindow(the_display,the_subwindow,button_event);

    }
  else if (the_event->type == Expose) 
    ;	/* Ignore exposure events for now since there isn't anything
      	    showing on the screen.  */
  else /* anything else */
    ;
}

handle_subwindow(the_display,the_subwindow,button_event)
	Display *the_display;
	Window the_subwindow;
	XButtonEvent *button_event;
{
     Drawable drawable_root;         /* a few required variables for a call */
     int border_width, drawable_depth;
     int x, y, height, width;


        /* Else if it's the subwindow, then play if it's the left
	   or the middle mouse button.  If it's the left, put a thick
	   (and ugly) border around the subwindow.  If it's the middle,
	   change the size of the subwindow.  You should see how the
	   subwindow grows or shrinks depending on the mouse location. */

      switch (button_event->button)
	{
	case Button1:
	  /* Change the border width to 5 pixels. */
	  XSetWindowBorderWidth(the_display,the_subwindow,5);
	  break;
	case Button2:
	  /* Get the current size of the window. */
	  XGetGeometry(the_display,the_subwindow,&drawable_root,
		       &x,&y,&width,&height,&border_width, &drawable_depth);
	  /* Change the size of the window depending on
	     the pointer location. */
	  if (button_event->x < 100 && button_event->y < 100)
	    {
	      width += 40;
	      height += 40;
	    }
	  else
	    {
	      width -= 40;
	      height -= 40;
	    }
	  /* Resize the subwindow.  No need to change the window manager
	     size hints. */
	  XResizeWindow(the_display,the_subwindow,width,height);
	  break;
	case Button3:
	  exit();
	default:
	  break;
	}
}

