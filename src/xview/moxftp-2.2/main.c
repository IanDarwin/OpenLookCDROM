/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */


#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: main.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/main.c,v $\n";
#endif

/* $Log: main.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "popen.h"
#include "patchlevel.h" 
#include "class.h"
#include "wc.h"

#include <sys/stat.h>
#include <unistd.h>
#include <signal.h>


int Debug = 0;

XtAppContext App_context;
Widget       Top;
char *Ftpname = NULL;
char *ver;
char *gateway = NULL;
int  use_gateway = 0;

static  void WM_DELETE_WINDOW();

extern char *Version;
extern String fallback_resources[];
extern do_animate;

#if defined(SIGPIPE)
SIG_TYPE     no_sigpipe();
#endif

main ( argc, argv )
    int   argc;
    char* argv[];
{   
    Display      *dpy;
    char	 *resources;
    Arg          arg[1];
    String       *fb;
    int          i;
    int		 use_fallback = 0;
    extern  void reapem();

    Ftpname =    XFTPNAME;
    argv[0] =    XFTPCLASS;
    resources =  XFTPRESOURCE;


#if defined(DESPERATE)
    malloc_debug(2);
#endif

#if defined(SIGCHLD)
    signal(SIGCHLD, reapem);
#endif

#if defined(SIGPIPE)
    signal(SIGPIPE, no_sigpipe);
#endif

    for (i=0; i<argc; i++) {
	if (strcmp(argv[i], "-debug") == 0) {
    	    resources =  XFTPRESOURCE_DEBUG;
	    Debug++;
 	} else if (strcmp(argv[i], "-ufb") == 0) {
	    use_fallback++;
 	} else if (strcmp(argv[i], "-gateway") == 0 &&
		   (i+1)<argc) {
	    i++;
	    gateway = argv[i];
 	} else if (strcmp(argv[i], "-animate") == 0) {
	    do_animate++;
	}
    }
	  
    /*
     * See if we have an archie c client.
     */
    Check_Archie();

    /*
     * Initialize local storage.
     */
    Initialize();
 
    /*
     * Check and see if fallback resources are needed.
     */
    fb = fallback_resources;
    if (checkdir(getenv("XAPPLRESDIR"), resources)) {
	fb = NULL;
    } else if(!Debug && access(APP_RES, R_OK) == 0) {
	fb = NULL;
    }
    if (use_fallback) fb = fallback_resources;

    /*
     * Initialize Open LOOK tool kit.
     */
#if defined(OPENWINDOW)
    OlToolkitInitialize((XtPointer)NULL);
#endif
    /*
     * Initialize Xt.
     */
    Top = XtAppInitialize (&App_context, 
			   resources,  
			   NULL, 
			   0, 
			   &argc, 
			   argv, 
			   fb, 	
			   NULL, 
			   0);

    /*
     * Get Application resources.
     */
/*
    XtGetApplicationResources(App_context, (char *)&args, arg_resources,
                              XtNumber(arg_resources),
                              (ArgList)NULL, (Cardinal)0);
*/

    /*
     * Register my widgets.
     */
 
    Register(App_context);

    /*
     * Register GUI widgets.
     */
    RegisterWidgets(App_context);

    /*
     * Add call backs.
     */
    Register_App_CallBacks_and_Actions();

    /*
     * Allow dialog to add its callbacks.
     */
    Register_dialog_c_a_a();

    /*
     * Create the childern.
     */
    if ( WcWidgetCreation (Top)  != 0 ) exit (1);
    /*
     * Clear list.
     */
    Clear_List();

    /*
     * So that noop will work on the command shell.
     */
    XtRealizeWidget(WcFullNameToWidget(Top, "*Shellcommand"));

    /*
     * Make visable.
     */
    XtRealizeWidget ( Top );

     /*
     * Set icon.
     */
    set_icon(ICON_DISCONNECT);

    /*
     * Allow for delete function.
     */
    WM_DELETE(Top, WM_DELETE_WINDOW, NULL);
    /*
     * Just for fun.
     */
    ver = (char *)XtMalloc(sizeof(Version)+sizeof(XFTPNAME)+20);
    sprintf(ver, Version, XFTPNAME, PATCHLEVEL);
    Set_Status(ver);
    /*
     * Bug in OPENWINDOW library or ld
     */
#if (defined(OPENWINDOW_XAW)||defined(OPENWINDOW))&&!defined(SVR4)
     force_open(1);
#endif

    /*
     * Initialize ftp.
     */
    Init_Ftp();

    /*
     * Start the main loop.
     */
    XtAppMainLoop(App_context);
}

set_icon(t)
int t;
{
    char *type = NULL;
    
    switch (t) {
	case ICON_CONNECTING:
	    type = "connecting";
	    break;
	case ICON_RECONNECT:
	    type = "reconnect";
	    break;
	case ICON_DISCONNECT:
	    type = "disconnect";
	    if (retry) type = "retry";
	    break;
	case ICON_CONNECT:
	    type = "connected";
	    break;
	case ICON_BUSY1:
	    type = "busy1";
	    break;
	case ICON_BUSY2:
	    type = "busy2";
	    break;
	case ICON_BUSY3:
	    type = "busy3";
	    break;
	case ICON_BUSY4:
	    type = "busy4";
	    break;
	break;
        default: 
	    fprintf(stderr, "Unkown icon\n");
	    return;
    }
   
    Set_Icon(Top, type);
}


static  void
WM_DELETE_WINDOW()
{
#if defined(XAW)
    exit(1);
#endif
}

checkdir(dir, file)
char *dir;
char *file;
{
    char *filename;
    int   good = 0;
 
    if (dir == NULL) return 0;
    if (*dir == '\0') return 0;
    filename = concat(NULL    , dir);
    filename = concat(filename, "/");
    filename = concat(filename, file);
    if (access(filename, R_OK) == 0) good++;
    XtFree(filename);
    return good;
}

#if (defined(OPENWINDOW_XAW)||defined(OPENWINDOW))&&!defined(SVR4)
force_open(i) 
int i;
{
 
    if (i) return;
    get_wmShellWidgetClass() ;
    get_applicationShellWidgetClass () ;
}
#endif

#if defined(SIGPIPE)
SIG_TYPE
no_sigpipe(sig)
int sig;
{
    signal(SIGPIPE, no_sigpipe);
}
#endif
