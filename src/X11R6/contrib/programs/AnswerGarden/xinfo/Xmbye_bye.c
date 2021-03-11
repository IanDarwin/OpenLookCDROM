#include <Xm/Xm.h>
#include <Xm/PushB.h>

void bye_bye(widget, client_data, call_data)
     Widget widget;
     caddr_t client_data;
     caddr_t call_data;
{
  exit();
}

main(argc,argv)
     int argc;
     char **argv;
{
  Widget shell;
  Widget button;

  shell = XtInitialize(argv[0],"SmallButBoring",NULL,0,&argc,argv);

  button = XmCreatePushButton(shell, "quit", NULL, 0);

  XtAddCallback(button,XmNactivateCallback,bye_bye,NULL);

  XtManageChild(button);

  XtRealizeWidget(shell);
  XtMainLoop();
}



