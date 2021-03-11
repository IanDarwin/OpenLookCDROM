@begin(header)
author: Mark S. Ackerman
show_author: ShowNone
author_organization: MIT Center for Coordination Science
node_expert: ackerman=ag@ics.uci.edu
expiration_date: 12/31/93
last_modifier: ackerman=ag@ics.uci.edu
last_mod_date: 07/21/93
mod_num: 3
@end(header)
/*****************************************************
  
  PIGGIE.C
  
  A brief example of X Toolkit usage.  This demonstrates
  the use of callbacks, arg lists, and some important
  widgets.
  
  Mark Ackerman
  Amended April, 1990
  
*****************************************************/

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

#define NChildren 8

/*  The callback for the exit button. */

void bye_bye(widget, client_data, call_data)
     Widget widget;
     caddr_t client_data;
     caddr_t call_data;
{
    printf("This little piggie said \"gotta go....\"\n");
    exit();
}

/* The callback for the other buttons. */

void everyone_else(widget, client_data, call_data)
     Widget widget;
     caddr_t client_data;
     caddr_t call_data;
{
    printf("This little piggie went to market.\n");
}


main(argc,argv)
     int argc;
     char **argv;
{
    Widget shell;
    Widget rowcol;
    Widget button[NChildren+1];
    int i;
    char str[10];
    
    Arg arglist[5];
    XmString my_string;
    

    /* Set up the shell widget. */
    shell = XtInitialize(argv[0],"XToes",NULL,0,&argc,argv);
    

    /* Set up the composite widget, row column manager */
    
    rowcol = XmCreateRowColumn(shell, "foot", NULL, 0);
    
    /* Set up the first 8 push buttons.  Label them with
       "button1" through "button8".  Set their callbacks to
       everyone_else(). */
    
    for (i=0;i<NChildren;i++)
	{
	    sprintf(str,"button%d",i+1);
	    my_string = XmStringCreateLtoR(str,XmSTRING_DEFAULT_CHARSET);
	    XtSetArg(arglist[0],XmNlabelString,(XtArgVal)my_string);
	    button[i] = XmCreatePushButton(rowcol, str, arglist, 1);
	    
	    XtAddCallback(button[i],XmNactivateCallback,everyone_else,NULL);
	    XmStringFree(my_string);
	}
    
    /* Set the last push button to be the exit button */
    
    
    my_string = XmStringCreateLtoR("exit",XmSTRING_DEFAULT_CHARSET);
    XtSetArg(arglist[0],XmNlabelString,(XtArgVal)my_string);
    button[NChildren] = XmCreatePushButton(rowcol, "exit", arglist, 1);
    
    XtAddCallback(button[NChildren],XmNactivateCallback,bye_bye,NULL);
    XmStringFree(my_string);
    
    /* Make the widgets appear on the screen. */
    XtManageChildren(button,NChildren+1);
    XtManageChild(rowcol);
    XtRealizeWidget(shell);
    
    /* Wait for i/o and dispatch. */
    XtMainLoop();
}



