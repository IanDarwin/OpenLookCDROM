/*
 * Copyright 1989, 1992 O'Reilly and Associates, Inc.

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

/*
 * xuildemo - simple uil example
 */

#include <stdio.h>
/*

 * Declare our callback functions.
 */
static void button_selected();

/* 
 * Miscellaneous UIL boilerplate: next 8 lines 
 */
#include <Xm/Xm.h>                  /* Motif Toolkit */
#include <Mrm/MrmPublic.h>          /* Motif Resource Manager */

static MrmHierarchy    s_MrmHierarchy; /* MRM database hierarchy id */
static char    *vec[]={"xuildemo.uid"}; /* MRM database file list */
static MrmCode        class ;
static MrmCount        regnum = 1 ;
static MrmRegisterArg  regvec[] = {
    {"button_selected",(XtPointer) button_selected }
};

/*
 * Define our callback functions.
 */
static void button_selected(widget, call_data, client_data)
    Widget    widget;
    XtPointer client_data;
    XtPointer call_data;
{
    XmAnyCallbackStruct *data = (XmAnyCallbackStruct *) call_data;
    char *tag = (char *) client_data;

    printf("button selected\n");
}

int main(argc, argv)
int argc;
char **argv;
{
    XtAppContext      app_context;
    Widget topLevel, appMain;

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    /*
     *  Initialize the MRM
     */
    MrmInitialize ();

    topLevel = XtVaAppInitialize(
           &app_context,       /* Application context */
           "XUilDemo",         /* application class name */
           NULL, 0,            /* command line option list */
           &argc, argv,        /* command line args */
           NULL,               /* for missing app-defaults file */
           NULL);              /* terminate varargs list */

    /*
     *  Define the Mrm.hierarchy (only 1 file)
     */

    if (MrmOpenHierarchyPerDisplay(XtDisplay(topLevel),
            1,             /* number of files */
            vec,                                   /* files */
            NULL,
            &s_MrmHierarchy)          /* ptr to returned id */
            != MrmSUCCESS) {
        printf ("Mrm can't open hierarchy\n");
    }

    /*
     *     Register our callback routines so that the resource  
     *     manager can resolve them at widget-creation time.
     */

    if (MrmRegisterNames (regvec, regnum)
            != MrmSUCCESS)
         printf("Mrm can't register names\n");

    /*
     *  Call MRM to fetch and create the pushbutton and its container
     */

    if (MrmFetchWidget (s_MrmHierarchy,
            "helloworld_main",
            topLevel,
            &appMain,
            &class) != MrmSUCCESS)
        printf("Mrm can't fetch interface\n");

    /*  Manage the main window of the hierarchy created by Mrm. */

    XtManageChild(appMain);
    
    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
