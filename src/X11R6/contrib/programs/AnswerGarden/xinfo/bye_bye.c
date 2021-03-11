#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>

void bye_bye(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
  exit();
}

main(argc,argv)
     int argc;
     char **argv;
{
  Widget shell;
  Widget command;
  XtAppContext app_context;
  
  shell = XtAppInitialize(&app_context,"SmallButBoring",NULL,0,&argc,argv,
			  NULL,NULL,0);

  command = XtVaCreateManagedWidget("quit", commandWidgetClass, shell, NULL);
  XtAddCallback(command,XtNcallback,bye_bye,NULL);

  XtRealizeWidget(shell);
  XtAppMainLoop(app_context);
}



