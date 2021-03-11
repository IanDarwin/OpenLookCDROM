/***********************************************************************

  PIGGIE.C

  A brief example of X Toolkit usage.  This demonstrates
  the use of callbacks, arg lists, and some important
  widgets.

  This used to be va_piggie.c.

  Mark Ackerman
  Last Amended November, 1990

*************************************************************************/

#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

/*  The callback for the exit button. */

void bye_bye(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  printf("This little piggie said \"gotta go....\"\n");
  exit();
}

/* The callback for the other buttons. */

void everyone_else(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  printf("This little piggie went to market.\n");
}

/* This is the arglist for the box.  This just changes a few
   values to change the values.  */


main(argc,argv)
     int argc;
     char **argv;
{
  Widget shell;
  Widget box;
  Widget command[9];
  int i;
  char str[10];
  XtAppContext app_context;
  
    /* Set up the shell widget. */
  
  shell = XtAppInitialize(&app_context,"XToes",NULL,0,&argc,argv,
			  NULL,NULL,0);

    /* Set up the composite widget, box */

  box = XtVaCreateManagedWidget("foot",boxWidgetClass, shell, 
				XtNhSpace, (XtArgVal)1,
				XtNvSpace, (XtArgVal)1,
				NULL);

    /* Set up the first 8 command buttons.  Label them with
       "button1" through "button8".  Set their callbacks to
       everyone_else(). */

  for (i=0;i<8;i++)
    {
      sprintf(str,"button%d",i+1);
      command[i] = XtVaCreateManagedWidget(str,commandWidgetClass,box,
					   NULL);
      XtAddCallback(command[i],XtNcallback,everyone_else,NULL);
    }

    /* Set the last command button to be the exit button */

  command[8] = XtVaCreateManagedWidget("exit",commandWidgetClass,box,
					 NULL);
  XtAddCallback(command[8],XtNcallback,bye_bye,NULL);

    /* Make the widgets appear on the screen. */

  XtRealizeWidget(shell);

    /* Wait for i/o and dispatch. */
  XtAppMainLoop(app_context);
}



