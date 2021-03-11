/* -*-C-*-
********************************************************************************
*
* File:         winterp.c
* RCS:          $Header: winterp.c,v 1.10 91/03/25 03:59:22 mayer Exp $
* Description:  WINTERP main() file.
* Author:       Niels Mayer, HPLabs
* Created:      Sat Jun 10 02:15:35 1989
* Modified:     Mon Mar 25 03:59:07 1991 (Niels Mayer) mayer@hplnpm
* Language:     C
* Package:      N/A
* Status:       X11r4 contrib tape release
*
* WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Hewlett-Packard and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Hewlett-Packard and David Betz
* make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL HEWLETT-PACKARD NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
* See ./winterp/COPYRIGHT for information on contacting the authors.
* 
* Please send modifications, improvements and bugfixes to mayer@hplabs.hp.com
* Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
**
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: winterp.c,v 1.10 91/03/25 03:59:22 mayer Exp $";

#include <stdio.h>
#include <ctype.h>

#include "../src-server/config.h" /* define DEFAULT_UNIX_SOCKET_FILEPATH DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR, etc */

#if (defined(WINTERP_WANT_INET_SERVER) || defined(WINTERP_WANT_UNIX_SERVER))
#include <sys/types.h>
#if defined(TLI)
#include <tiuser.h>
extern int t_errno;
#else
#include <sys/socket.h>
#endif /* defined(TLI) */
#endif				/* (defined(WINTERP_WANT_INET_SERVER) || defined(WINTERP_WANT_UNIX_SERVER)) */

#ifdef WINTERP_WANT_INET_SERVER
#include <netinet/in.h>
#include <netdb.h>
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
#include <stdlib.h> /* for unlink() */
#include <sys/un.h> /* for AF_UNIX sockets */
#endif				/* WINTERP_WANT_UNIX_SERVER */

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#ifdef WINTERP_MOTIF_11
#include <Xm/Protocols.h>	/* <Xm/Protocols.h> location seems to have moved in 1.1 */
#else
#include <X11/Protocols.h>
#endif				/* WINTERP_MOTIF_11 */

#include "winterp.h"
#include "user_prefs.h"
#include "xlisp/xlisp.h"


/* forward declarations */
static void Read_Eval_Print();
static int  Read_From_Stream_Eval_And_Print();
#ifdef WINTERP_WANT_INET_SERVER
static void AF_INET_Read_Eval_Print();
static int  Initialize_AF_INET_Server_Socket();
#endif				/* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
static void AF_UNIX_Read_Eval_Print();
static int  Initialize_AF_UNIX_Server_Socket();
#endif				/* WINTERP_WANT_UNIX_SERVER */
static void Winterp_Xtoolkit_Error_Handler();
static void Winterp_Xtoolkit_Warning_Handler();
static int  Winterp_Xlib_Error_Handler();
void	    Winterp_Application_Shell_WMDelete_Callback();

/* global variables */
jmp_buf		top_level;
CONTEXT		cntxt;
int		read_eval_print_just_called;
int		lisp_reader_hit_eof;
char*		app_name = NULL;
char*		app_class = NULL;
#ifdef WINTERP_WANT_INET_SERVER
static int	client_AF_INET_listen_socket = NULL;
#endif				/* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
static int	client_AF_UNIX_listen_socket = NULL;
#endif				/* WINTERP_WANT_UNIX_SERVER */
Widget		toplevel_Wgt = NULL;
XtAppContext	app_context = NULL;
Display*	display;
Window		root_win;
Screen*		screen;
Colormap	colormap;
Atom		wm_delete_atom;
Pixel		default_foreground_pixel, default_background_pixel;
USER_PREFS_DATA user_prefs;	/* extern declared in user_prefs.h, really here */
char		temptext[BUFSIZ]; /* a temporary text buffer, for sprintf() */
Arg		_args[10];	/* for XtSetArg() macros in winterp.h */
int		_num_args;	/* for XtSetArg() macros in winterp.h */

/* 
 * Data on how user-customization resources are interpreted:
 * this must be kept up to date with data structure USER_PREFS_DATA_PTR 
 * in user_prefs.h
 */
static XtResource resources[] = {
  /*
   * The name of the file to load to initialize xlisp.
   */
  {"lispInitFile", "LispInitFile",
     XmRString, sizeof(String),
     XtOffset(USER_PREFS_DATA_PTR, lisp_init_file),
     XmRString, (XtPointer) DEFAULT_LISP_INIT_FILE},

  /*
   * The name of the file to output lisp transactions.
   */
  {"lispTranscriptFile", "LispTranscriptFile",
     XmRString, sizeof(String),
     XtOffset(USER_PREFS_DATA_PTR, lisp_transcript_file),
     XmRString, (XtPointer) DEFAULT_LISP_TRANSCRIPT_FILE},

  /*
   * The name of the default directory for 'load'. This is only
   * used in cases where 'load' wasn't supplied a full
   * filepath (i.e. a path beginning with '/' or '.').
   *
   * Note that "lispLibDir" should be the path to an existing directory with
   * a trailing '/', e.g. "/usr/local/winterp/lisp-lib/". The default is
   * "./" so as to simulate Xlisp's default load behavior.
   * (See also w_utils.c:Wut_Prim_LOAD()).
   */
  {"lispLibDir", "LisplibDir",
     XmRString, sizeof(String),
     XtOffset(USER_PREFS_DATA_PTR, lisp_lib_dir),
     XmRString, (XtPointer) DEFAULT_LISP_LIB_DIR},
  
  /*
   * Setting this boolean to FALSE will allow WINTERP to startup
   * without printing lots of output.
   */
  {"enableInitMsgs", "EnableInitMsgs",
     XmRBoolean, sizeof(Boolean),
     XtOffset(USER_PREFS_DATA_PTR, enable_init_msgs),
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_INIT_MSGS},

#ifdef WINTERP_WANT_INET_SERVER
  /*
   * The port number of the widget interpreter lisp server.
   */
  {"servicePort", "ServicePort",
     XmRInt, sizeof(int),
     XtOffset(USER_PREFS_DATA_PTR, service_port),
     XmRImmediate, (XtPointer) DEFAULT_INET_SERVICE_PORT},

  /*
   * The service name of the widget interpreter lisp server.
   */
  {"serviceName", "ServiceName",
     XmRString, sizeof(String),
     XtOffset(USER_PREFS_DATA_PTR, service_name),
     XmRString, (XtPointer) DEFAULT_INET_SERVICE_NAME},

  /*
   * Setting this boolean to TRUE will start up WINTERP so that
   * it will accept input from its INET Domain Server. Those worried about
   * security when running winterp-based applications will want to
   * set this to FALSE in the application defaults file for the application.
   */
  {"enableInetServer", "enableInetServer",
     XmRBoolean, sizeof(Boolean),
     XtOffset(USER_PREFS_DATA_PTR, enable_AF_INET_server),
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_INET_SERVER},
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
  /*
   * Setting this boolean to FALSE will start up WINTERP without
   * it's Unix Domain server. Those worried about security when running
   * winterp-based applications on a multi-user machine will want
   * to set this in the  application defaults file for the application.
   */
  {"enableUnixServer", "enableUnixServer",
     XmRBoolean, sizeof(Boolean),
     XtOffset(USER_PREFS_DATA_PTR, enable_AF_UNIX_server),
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_UNIX_SERVER},

  /*
   * This is set to the full pathname for the AF_UNIX domain socket-file
   */
  {"unixSocketFilepath", "UnixSocketFilepath",
     XmRString, sizeof(String),
     XtOffset(USER_PREFS_DATA_PTR, unix_socket_filepath),
     XmRString, (XtPointer) DEFAULT_UNIX_SOCKET_FILEPATH},
#endif				/* WINTERP_WANT_UNIX_SERVER */

  /*
   * Setting this boolean to FALSE will start up WINTERP
   * with the Xtoolkit's default XtError handler -- any XtErrors
   * will cause WINTERP to exit. By default, this is TRUE,
   * which means that a lisp error will be signalled, and the
   * call-sequence (or callback) that caused the error will
   * terminate, however WINTERP will be able to execute other callbacks,
   * input from the XLISP eval-server, etc. For interactive
   * use, I suggest leaving this resource at the default TRUE;
   * for delivered applications, you probably want to set this to
   * FALSE.
   */
  {"enableXtErrorBreak", "EnableXtErrorBreak",
     XmRBoolean, sizeof(Boolean),
     XtOffset(USER_PREFS_DATA_PTR, enable_XtError_break),
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_XT_ERROR_BREAK},

  /*
   * Setting this boolean to FALSE will start up WINTERP
   * with the Xtoolkit's default XtWarning handler -- any XtWarnings
   * will just cause a message to be printed, execution will continue.
   * By default, this is TRUE which means that a lisp error will be
   * signalled, and the call-sequence (or callback)  that caused the error
   * will terminate,  however WINTERP will be able to execute other
   * callbacks, input from the XLISP eval-server, etc. For interactive
   * use, I suggest leaving this resource at the default TRUE;
   * for delivered applications, you probably want to set this to
   * FALSE.
   */
  {"enableXtWarningBreak", "EnableXtWarningBreak",
     XmRBoolean, sizeof(Boolean),
     XtOffset(USER_PREFS_DATA_PTR, enable_XtWarning_break),
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_XT_WARNING_BREAK},

  /*
   * Setting this boolean to FALSE will start up WINTERP
   * with the Xlib's default Error handler -- any XErrors
   * will cause WINTERP to exit. By default, this is TRUE,
   * which means that a lisp error will be signalled, and the
   * call-sequence (or callback) that caused the error will
   * terminate, however WINTERP will be able to execute other callbacks,
   * input from the XLISP eval-server, etc. For interactive
   * use, I suggest leaving this resource at the default TRUE;
   * for delivered applications, you probably want to set this to
   * FALSE.
   */
  {"enableXErrorBreak", "EnableXErrorBreak",
     XmRBoolean, sizeof(Boolean),
     XtOffset(USER_PREFS_DATA_PTR, enable_XError_break),
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_X_ERROR_BREAK}
};

/*
 * Table indicating how to set-from-the-command-line the application-specific
 * resources specified in resources[] above.
 */
static XrmOptionDescRec commandline_options_table[] = {
  {"-init_file",	".lispInitFile",	XrmoptionSepArg, NULL},
  {"-transcript_file",	".lispTranscriptFile",	XrmoptionSepArg, NULL},
  {"-lib_dir",		".lispLibDir",		XrmoptionSepArg, NULL},
  {"-no_init_msgs",	".enableInitMsgs",	XrmoptionNoArg, "false"},
  {"-enable_init_msgs",	".enableInitMsgs",	XrmoptionNoArg, "true"},
#ifdef WINTERP_WANT_INET_SERVER
  {"-serv_port",	".servicePort",		XrmoptionSepArg, NULL},
  {"-serv_name",	".serviceName",		XrmoptionSepArg, NULL},
  {"-no_inet_server",	".enableInetServer",	XrmoptionNoArg, "false"},
  {"-enable_inet_server",".enableInetServer",	XrmoptionNoArg, "true"},
#endif				/* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
  {"-no_unix_server",	".enableUnixServer",	XrmoptionNoArg, "false"},
  {"-enable_unix_server",".enableUnixServer",	XrmoptionNoArg, "true"},
  {"-unix_socket_file",	".unixSocketFilepath",	XrmoptionSepArg, NULL},
#endif				/* WINTERP_WANT_UNIX_SERVER */
  {"-no_xterr_brk",	".enableXtErrorBreak",	XrmoptionNoArg, "false"},
  {"-enable_xterr_brk",	".enableXtErrorBreak",	XrmoptionNoArg, "true"},
  {"-no_xtwarn_brk",	".enableXtWarningBreak",XrmoptionNoArg, "false"},
  {"-enable_xtwarn_brk",".enableXtWarningBreak",XrmoptionNoArg, "true"},
  {"-no_xerr_brk",	".enableXErrorBreak",	XrmoptionNoArg, "false"},
  {"-enable_xerr_brk",	".enableXErrorBreak",	XrmoptionNoArg, "true"}
};

/*
 * Setup an action table for winterp. Note that action procedure "Lisp"
 * is a special action procedure that calls the lisp evaluator on the
 * parameters of the action. A translation like
 * "Ctrl<Key>K: Lisp(quack 1 2 3)" will evaluate '(quack 1 2 3)'
 */
extern void Wtx_Winterp_Lisp_Action_Proc(); /* w_txlations.c */
static XtActionsRec winterp_action_table[] = {
  {"Lisp", Wtx_Winterp_Lisp_Action_Proc}
};


/*******************************************************************************
 * main - the main routine
 ******************************************************************************/
main(argc,argv)
  int argc; char *argv[];
{
  extern LVAL true;
  extern LVAL s_evalhook,s_applyhook;
  extern FILE* osaopen();
  extern FILE *tfp;
  extern int xldebug;
  extern int xltrcindent;
  extern LVAL Wshl_WidgetID_To_WIDGETOBJ(); /* wc_SHELL.c */
  extern void Wfu_Sanity_Check(); /* w_funtab.c */
  char** original_argv;
  int    original_argc;
  XEvent event;


  /*
   * Trim directory path off of program name.
   */
  if ((app_name = rindex(argv[0], '/')) == NULL)
    app_name = argv[0];
  else
    app_name++;

  /*
   * Trim "Login Shell" from the program name
   */
  if (*app_name == '-')
    app_name++;

  /*
   * sanity check to ensure that the number of pointers to funtab entries in
   * w_funtab.h correspond to the number of entries in w_funtab.c:funtab[].
   */
  Wfu_Funtab_Sanity_Check();

  /* 
   * Make a copy of argv,argc to pass into
   * 'toplevel_Wgt = XtAppCreateShell(...applicationShellWidgetClass...)'
   * This is used by session managers so as to provide arguments to restart
   * the application with the same arguments as the current invocation.
   * We must make a copy here because XtOpenDisplay() modifies argv and argc
   * and we twiddle argc/argv below.
   */
  original_argv = (char**) XtMalloc((unsigned) (argc + 1) * sizeof(char*));
  for (original_argc = 0 ; original_argc < argc ; original_argc++)
    original_argv[original_argc] = argv[original_argc];
  original_argv[original_argc] = NULL;
  
  /*
   * Special case the first argument on the command line... 
   * If it is "-class <classname>", then use the next argument <classname> as the
   * application class.  This kludge allows us to run winterp using a variable
   * application class name, thus allowing us to use specify variable APP-DEFAULT
   * files. (Hack submitted by Eric Blossom of HP Western Response Center Labs.)
   */
  app_class = "Winterp";
  if ((argc >= 3) && (strcmp(argv[1], "-class") == 0)) {
    app_class = argv[2];
    argv[2] = argv[0];
    argv += 2;
    argc -= 2;
  }

  /* 
   * Initialize the toolkit
   */
  XtToolkitInitialize();

  /* 
   * Initialize Resource converters: normally, these functions are called 
   * from XtCreateWidget(), XtCreateManagedWidget(),  XtCreatePopupShell(), and
   * XtAppCreateShell(); they only get called the first time you create a
   * widget of class Primitive or Manager because they're called from the 
   * ClassInitialize() procedure. With the way WINTERP's automatic resource
   * converters work, you can end up asking for a resource conversion to occur
   * before any ClassInitialize() procs are called, and that would cause errors
   * like "X Toolkit Warning: No type converter registered for 'String' to ..."
   */
  XmRegisterConverters();	/* from Xm/ResConvert.c -- used in Manager, Primitive and Vendor ClassInitialize() */
  _XmRegisterPixmapConverters(); /* from Xm/Visual.c -- used in Manager, Primitive and Vendor ClassInitialize() */

  /*
   * Sanity check to ensure that the version of the Motif toolkit libraries
   * used correspond to the Motif toolkit header <Xm/Xm.h>. This test is only valid
   * after XmRegisterConverters() has been called.
   */
  if (xmUseVersion != XmVersion) { /* XmVersion def'd and xmUseVersion externed in <Xm/Xm.h> */
    (void) fprintf(stderr, "%s: Fatal error: application must be recompiled with <Xm/Xm.h> matching libXm.a\n", app_name);
    (void) fprintf(stderr, "\t\t(header version == %d, library version == %d)\n", XmVersion, xmUseVersion);
    exit(1);
  }

  app_context = XtCreateApplicationContext();
  display = XtOpenDisplay(app_context, (String) NULL, app_name, app_class,
			  commandline_options_table, XtNumber(commandline_options_table),
			  &argc, argv);
  if (!display)
    xlfatal("Can't open display -- XtOpenDisplay() failed.");

  if (argc > 1) {		/* if argc!=0, then there are invalid arguments that didn't get parsed by XtOpenDisplay() */
    (void) fprintf (stderr, "usage: %s [-class <classname>] [-init_file <file.lsp>]\n", app_name);
    (void) fprintf (stderr, "\t[-transcript_file <file.out>] [-lib_dir <path-to-load-dir>]\n");
    (void) fprintf (stderr, "\t[-no_init_msgs] [-enable_init_msgs]\n");
#ifdef WINTERP_WANT_INET_SERVER
    (void) fprintf (stderr, "\t[-serv_port <portnum>] [-serv_name <servname>]\n");
    (void) fprintf (stderr, "\t[-no_inet_server] [-enable_inet_server]\n");
#endif				/* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
    (void) fprintf (stderr, "\t[-no_unix_server] [-enable_unix_server]\n");
    (void) fprintf (stderr, "\t[-unix_socket_file <socket-filepath>]\n");
#endif				/* WINTERP_WANT_UNIX_SERVER */
    (void) fprintf (stderr, "\t[-no_xterr_brk] [-enable_xterr_brk]\n");
    (void) fprintf (stderr, "\t[-no_xtwarn_brk] [-enable_xtwarn_brk]\n");
    (void) fprintf (stderr, "\t[-no_xerr_brk] [-enable_xerr_brk]\n");
    (void) fprintf (stderr, "\t[... Xtoolkit options ...]\n");
    (void) fprintf (stderr, "\tNote: if you wish to use the -class option it must be the\n");
    (void) fprintf (stderr, "\tfirst argument following %s.\n", app_name);
    xlfatal("Invalid command-line arguments.");
  }

  /* 
   * Set close-on-exec on file descriptor of display connection. Otherwise, any
   * child processes started up by WINTERP will inherit the file-descriptor, and
   * windows will not disappear after WINTERP is killed while child processes remain.
   */
  fcntl(ConnectionNumber(display), F_SETFD, 1);

  /*
   * initialize some global variables used throughout this program.
   * NOTE: if winterp ever gets changed to use application contexts enabling
   * multiple displays, screens, etc, then we'll have to make some major changes
   * here, and to any primitives that use these values.
   */
  root_win = DefaultRootWindow(display);
  screen = DefaultScreenOfDisplay(display);
  colormap = XDefaultColormapOfScreen(screen);
  wm_delete_atom = XmInternAtom(display, "WM_DELETE_WINDOW", TRUE);

  /*
   * Setup action table for accelerators and translations.
   */
  XtAppAddActions(app_context, winterp_action_table, XtNumber(winterp_action_table));
  
  /* 
   * We need toplevel_Wgt so that we can have around a "default" set of X
   * structures (colors, graphics contexts etc) for use by XtConvert()...
   * this is a kludge. We also need this widget around in order to set
   * Winterp-specific application resources in structure user_prefs.
   *
   * So as not to bother people with an uneccesary window, we create the
   * window at location +1+1, then unmap it.
   */
  ARGLIST_RESET();
  ARGLIST_ADD(XmNdeleteResponse, XmDO_NOTHING);	/* we handle wm deletion (f.kill) w/ XmAddWMProtocolCallback() below. */
  ARGLIST_ADD(XmNscreen, (XtArgVal) screen);
  ARGLIST_ADD(XmNargc, (XtArgVal) original_argc);
  ARGLIST_ADD(XmNargv, (XtArgVal) original_argv);
  ARGLIST_ADD(XmNgeometry, (XtArgVal) "10x10+1+1"); /* we don't want user to have to place this window, so give it a location; giving size prevents "Error: Shell widget winterp has zero width and/or height" */
  toplevel_Wgt = XtAppCreateShell(app_name, app_class, applicationShellWidgetClass, display, ARGLIST());
  XmAddWMProtocolCallback(toplevel_Wgt, wm_delete_atom, Winterp_Application_Shell_WMDelete_Callback, NULL);
  XtGetApplicationResources(toplevel_Wgt, &user_prefs, resources, XtNumber(resources), NULL, 0); /* place application resources in user_prefs global struct. */
  XtRealizeWidget(toplevel_Wgt); /* give the order to create the windows, etc. */
  XmUpdateDisplay(toplevel_Wgt); /* after this executes, the widget will get realized, windows created, etc. */
  XtUnmapWidget(toplevel_Wgt);	/* once the windows are created by XtRealizeWidget()/XmUpdateDisplay(), we may hide the window by unmapping */
  XtFree((char*) original_argv); /* Motif makes a copy of this upon setting XmNargv resource however, if this is placed after XtAppCreateShell() call, you get a coredump... */

  /*
   * Get Xtoolkit's default foreground and background Pixels, set globals
   * to these values.
   */
  {
    XrmValue from, to;

    from.size = (unsigned int) strlen(XtDefaultForeground) + 1;
    from.addr = (caddr_t) XtDefaultForeground;
    to.size = (unsigned int) sizeof(Pixel);
    to.addr = (caddr_t) &default_foreground_pixel;
    XtConvert(toplevel_Wgt, XmRString, &from, XmRPixel, &to);
    if (to.addr == NULL)	/* error if conversion failed */
      xlfatal("XtConvert() couldn't convert XtDefaultForeground to XmRPixel.");

    from.size = (unsigned int) strlen(XtDefaultBackground) + 1;
    from.addr = (caddr_t) XtDefaultBackground;
    to.size = (unsigned int) sizeof(Pixel);
    to.addr = (caddr_t) &default_background_pixel;
    XtConvert(toplevel_Wgt, XmRString, &from, XmRPixel, &to);
    if (to.addr == NULL)	/* error if conversion failed */
      xlfatal("XtConvert() couldn't convert XtDefaultBackground to XmRPixel.");
  }

#ifdef WINTERP_WANT_INET_SERVER
  if (user_prefs.enable_AF_INET_server) {
    /*
     * get a socket to listen on. when it's selected, call AF_INET_Read_Eval_Print()
     * to open a connection socket, process the client request, and close the socket
     */
    client_AF_INET_listen_socket = Initialize_AF_INET_Server_Socket();
    (void) XtAppAddInput(app_context, client_AF_INET_listen_socket, XtInputReadMask,
			 AF_INET_Read_Eval_Print, NULL);
  }
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
  if (user_prefs.enable_AF_UNIX_server) {
    /*
     * get a socket to listen on. when it's selected, call AF_UNIX_Read_Eval_Print()
     * to open a connection socket, process the client request, and close the socket
     */
    client_AF_UNIX_listen_socket = Initialize_AF_UNIX_Server_Socket();
    (void) XtAppAddInput(app_context, client_AF_UNIX_listen_socket, XtInputReadMask,
			 AF_UNIX_Read_Eval_Print, NULL);
  }
#endif				/* WINTERP_WANT_UNIX_SERVER */


  /*
   * Setup Xlib and Xtoolkit warning and error handlers so that errors inside
   * the Xtoolkit will call xlerror().
   */
  if (user_prefs.enable_XtError_break)
    (void) XtAppSetErrorHandler(app_context, Winterp_Xtoolkit_Error_Handler);
  if (user_prefs.enable_XtWarning_break)
    (void) XtAppSetWarningHandler(app_context, Winterp_Xtoolkit_Warning_Handler);
  if (user_prefs.enable_XError_break)
    XSetErrorHandler(Winterp_Xlib_Error_Handler);

  if (user_prefs.enable_init_msgs) {
    (void) printf("================================================================================\n");
#ifdef WINTERP_MOTIF_111
    (void) printf("WINTERP -- Motif 1.1.1 "); /* no way to tell 1.1.1 from 1.1, but the user may want to know... */
#else				/* Plain old 1.0 or 1.1 */
    (void) printf("WINTERP -- Motif %d.%d ", XmVERSION, XmREVISION); /* from <Xm/Xm.h> */
#endif				/* WINTERP_MOTIF_111 */
    (void) printf("Widget INTERPreter by Niels P. Mayer (mayer@hplabs.hp.com).\n");
    (void) printf("	WINTERP version %d.%d, Copyright (c) 1989, 1990, 1991 Hewlett-Packard Company\n",
		  WINTERP_VERSION_INT, WINTERP_REVISION_INT); /* from winterp.h */
    (void) printf("	XLISP version %d.%d, Copyright (c) 1989, by David Betz\n\n",
		  XLISP_VERSION_INT, XLISP_REVISION_INT); /* from xlisp/xlisp.h */
  }

  /* 
   * Startup XLISP
   */
  if (user_prefs.enable_init_msgs)
    osinit("Initializing ...\n");
  else 
    osinit("");

  /* setup initialization error handler */
  xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, (LVAL)1);
  if (setjmp(cntxt.c_jmpbuf))
    xlfatal("Fatal XLISP initialization error.");
  if (setjmp(top_level))
    xlfatal("XLISP RESTORE not allowed during initialization.");

  /* initialize xlisp */
  xlinit();			/* xlisp/xlinit.c */

  /* initialize WINTERP modules */
  Wso_Init();			/* w_savedobjs.c */
  Wres_Init();			/* w_resources.c */
  Wxms_Init();			/* w_XmString.c */
  Wcb_Init();			/* w_callbacks.c */
  Wto_Init();			/* w_timeouts.c */
  Wtx_Init();			/* w_txlations.c */
  Weh_Init();			/* w_evnthndlr.c */
  Wxm_Init();			/* w_libXm.c */

  /* 
   * The following create interfaces to all the motif widget-classes via
   * xlisp classes, by calling Wcls_Create_Subclass_Of_WIDGET_CLASS()
   * with o_WIDGET_CLASS (def'd in Wc_WIDGET_Init()) as their superclass.
   * Methods on the specific widget classes correspond to 
   * special operations pertaining to that class, and not to others. 
   * These derived classes may override the 'Widget_Class' :isnew method 
   * for cases where motif "convenience" functions are used to create the 
   * widget. Additionally, since different classes generate different callback
   * structures, certain widgetclasses may override the metaclass' :set_callback
   * and :add_callback methods so as to allow dereferencing of the appropriate
   * callback structure elements.
   */
  Wc_WIDGET_Init();		/* WIDGET_CLASS metaclass */
  Wc_SHELL_Init();		/* SHELL and POPUP_SHELL metaclasses */
  Wc_ArrowB_Init();
  Wc_BulletinB_Init();
  Wc_CascadeB_Init();
  Wc_Command_Init();
  Wc_DrawingA_Init();
  Wc_DrawnB_Init();
  Wc_FileSB_Init();
  Wc_Form_Init();
  Wc_Frame_Init();
  Wc_Label_Init();
  Wc_List_Init();
  Wc_MainW_Init();
  Wc_MessageB_Init();
  Wc_PanewW_Init();
  Wc_PushB_Init();
  Wc_RowColumn_Init();
  Wc_Scale_Init();
  Wc_ScrollBar_Init();
  Wc_ScrolledW_Init();
  Wc_SelectioB_Init();
  Wc_Separator_Init();
  Wc_Text_Init();
  Wc_ToggleB_Init();
#ifdef HP_GRAPH_WIDGET
  Wc_XmGraph_Init();
#endif				/* HP_GRAPH_WIDGET */

  /*
   * Make the toplevel_Wgt accessible from lisp as global *TOPLEVEL_WIDGET*.
   * This code must occur after calling Wc_SHELL_Init(), and preferably after
   * every WINTERP widget class initializer is called.
   */
  setvalue(xlenter("*TOPLEVEL_WIDGET*"), Wshl_WidgetID_To_WIDGETOBJ(toplevel_Wgt));

  /*
   * Make XLISP, WINTERP, and MOTIF version info available within interpreter.
   */
  setvalue(xlenter("*XLISP_VERSION*"),
	   cvfixnum((FIXTYPE) XLISP_VERSION_INT)); /* XLISP_VERSION_INT from xlisp/xlisp.h */
  setvalue(xlenter("*XLISP_REVISION*"),
	   cvfixnum((FIXTYPE) XLISP_REVISION_INT)); /* XLISP_REVISION_INT from xlisp/xlisp.h */
  setvalue(xlenter("*MOTIF_VERSION*"),
	   cvfixnum((FIXTYPE) XmVERSION)); /* XmVERSION from <Xm/Xm.h> */
  setvalue(xlenter("*MOTIF_REVISION*"),
	   cvfixnum((FIXTYPE) XmREVISION)); /* XmREVISION from <Xm/Xm.h> */
  setvalue(xlenter("*WINTERP_VERSION*"),
	   cvfixnum((FIXTYPE) WINTERP_VERSION_INT)); /* WINTERP_VERSION_INT from winterp.h */
  setvalue(xlenter("*WINTERP_REVISION*"),
	   cvfixnum((FIXTYPE) WINTERP_REVISION_INT)); /* WINTERP_REVISION_INT from winterp.h  */

  xlend(&cntxt);

  /* reset the error handler */
  xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, true);

  /* open the transcript file */
  if (user_prefs.lisp_transcript_file && (tfp = osaopen(user_prefs.lisp_transcript_file, "w")) == NULL) {
    (void) sprintf(temptext, "error: can't open transcript file: \"%s\"",
		   user_prefs.lisp_transcript_file);
    stdputstr(temptext);
  }

  /* load file specified by resource "lispInitFile" (defaults to "init.lsp") */
  if (setjmp(cntxt.c_jmpbuf) == 0) {
    if (!xlload(user_prefs.lisp_init_file, user_prefs.enable_init_msgs, FALSE)) {
      (void) sprintf(temptext,
		     "WINTERP warning -- couldn't load initialization file: \"%s\"\n\t\
Check command-line argument \"-init_file\" or Xresource \".lispInitFile\"\n",
		     user_prefs.lisp_init_file);
      stdputstr(temptext);
    }
  }

  if (user_prefs.enable_init_msgs) {

#ifdef WINTERP_WANT_INET_SERVER
    if (user_prefs.enable_AF_INET_server) {
      (void) printf("\nXLisp INET Domain eval-server ready for input");
      if (user_prefs.service_port)
	(void) printf(" on port %d .\n", user_prefs.service_port);
      else
	(void) printf(" using service=%s .\n", user_prefs.service_name);
    }
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
    if (user_prefs.enable_AF_UNIX_server)
      (void) printf("\nXLisp Unix Domain eval-server ready for input on socket %s .\n",
		    user_prefs.unix_socket_filepath);
#endif				/* WINTERP_WANT_UNIX_SERVER */

#if (defined(WINTERP_WANT_INET_SERVER) && !defined(WINTERP_WANT_UNIX_SERVER))
    if (user_prefs.enable_AF_INET_server)
      (void) printf("Note: INPUT TO XLISP EVALUATOR CANNOT BE ENTERED HERE!! (see winterp.doc)\n");
#endif
#if (!defined(WINTERP_WANT_INET_SERVER) && defined(WINTERP_WANT_UNIX_SERVER))
    if (user_prefs.enable_AF_UNIX_server)
      (void) printf("Note: INPUT TO XLISP EVALUATOR CANNOT BE ENTERED HERE!! (see winterp.doc)\n");
#endif
#if (defined(WINTERP_WANT_INET_SERVER) && defined(WINTERP_WANT_UNIX_SERVER))
    if ((user_prefs.enable_AF_INET_server) || (user_prefs.enable_AF_UNIX_server))
      (void) printf("Note: INPUT TO XLISP EVALUATOR CANNOT BE ENTERED HERE!! (see winterp.doc)\n");
#endif

    (void) printf("================================================================================\n");
  }
  
  /* setup longjmp target for restore */
  if (setjmp(top_level))
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, true);

  read_eval_print_just_called = TRUE; /* special initial cond */
  lisp_reader_hit_eof = FALSE;

  /* Process X Events and Lisp client requests forever */
  for (;;) {
    /* 
     * We need to setup a new error return only after each time that an XLISP 
     * evaluation occurs. Therefore, we check for read_eval_print_just_called 
     * (which is set by Read_Eval_Print()) and then clear it once the setjmp() 
     * has been done. This avoids setting up an error return for each X event
     * being processed in this loop. 
     */
    if (read_eval_print_just_called) {
      read_eval_print_just_called = FALSE;
      if (lisp_reader_hit_eof) 
	break;
      if (setjmp(cntxt.c_jmpbuf)) { /* longjmp target for error return */
	setvalue(s_evalhook, NIL);
	setvalue(s_applyhook, NIL);
	xltrcindent = 0;
	xldebug = 0;
        xlflush();		/* needed if using (read)/(read-line) from stdin */ 
      }
      if (user_prefs.enable_init_msgs)
	stdputstr("Xlisp-Eval-Result: "); /* use this to separate results of different evaluations */
      fflush(stdout); fflush(stderr); /* otherwise output won't happen while blocked in XtAppNextEvent() */
    }

    /*
     * XtAppNextEvent() waits for Xevents, and while it is waiting, it will
     * process inputs added via AtAppAddInput() or XtAppAddWorkProc(). Lisp 
     * server input will cause Read_Eval_Print() to get called, and that
     * procedure sets the globals lisp_reader_hit_eof and 
     * read_eval_print_just_called. Read_Eval_Print() sends a bogus 
     * XAnyEvent (event.type == 0) so as to force XtAppNextEvent() to return; 
     * otherwise it would only return if a lisp evaluation caused X events 
     * to be generated, which means that XLISP error returns for non-X 
     * evaluations wouldn't get set up properly.
     *
     * XtDispatchEvent() will dispatch the actions from the events gathered
     * by XtAppNextEvent(). Note that XtDispatchEvent() ignores the aforementioned
     * bogus events: "if (event->type == 0) return;"
     */
    XtAppNextEvent(app_context, &event);
    XtDispatchEvent(&event);
  }
  wrapup();			/* this is also called if we eval expr (quit) */
}


#ifdef WINTERP_WANT_INET_SERVER
/******************************************************************************
 * initialize AF_INET server, returning a socket that can be listened on.
 ******************************************************************************/
#if !defined(TLI)
static int Initialize_AF_INET_Server_Socket()
{
  int                ls;	/* socket descriptor */
  struct servent    *sp;	/* pointer to service information */
  struct sockaddr_in myaddr_in;	/* for local socket address */
  char* portenv;

  /* clear out address structure */
  memset ((char *)&myaddr_in, 0, sizeof(struct sockaddr_in));
  
  /* Set up address structure for the listen socket. */
  myaddr_in.sin_family = AF_INET;
  myaddr_in.sin_addr.s_addr = INADDR_ANY;
  
  /* Find the information for the server to get the needed port number. */
  if (portenv = getenv(DEFAULT_INET_PORT_ENVVAR)) { /* env var for port specification */
    user_prefs.service_port = (int) strtol(portenv, (char **) NULL, 0);	/* environment var overrides Xresource setting */
    myaddr_in.sin_port = htons((u_short) user_prefs.service_port);
  }
  else if (user_prefs.service_port != NULL)
    myaddr_in.sin_port = htons((u_short) user_prefs.service_port);
  else {
    if ((sp = getservbyname(user_prefs.service_name, "tcp")) == NULL)
      xlfatal("Unable to getservbyname() for INET Domain Socket.");
    myaddr_in.sin_port = sp->s_port;
  }
  
  /* Create the listen socket. */
  if ((ls = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    perror(app_name);
    xlfatal("Unable to create INET Domain Socket().");
  }
  
  /* Bind the listen address to the socket. */
  if (bind(ls, &myaddr_in, sizeof(struct sockaddr_in)) == -1) {
    perror(app_name);
    xlfatal("Unable to bind() INET Domain Socket.");
  }

  /* Initiate the listen on the socket so remote users
   * can connect.  The listen backlog is set to 5, which
   * is the largest currently supported.
   */
  if (listen(ls,5) == -1) {
    perror(app_name);
    xlfatal("Unable to listen() on INET Domain Socket.");
  }
  
  setpgrp();

  fcntl(ls, F_SETFD, 1);	/* set close-on-exec for the client listener socket */
  
  return(ls);
}
#else
static int Initialize_AF_INET_Server_Socket()
{
  int listen_fd;
  struct t_bind *bindreq, *bindret;
  struct sockaddr_in *server, raddr;

  if ((listen_fd = t_open(TLI_TCP, O_RDWR, NULL)) < 0) {
    t_error("t_open failed for listen_fd");
    exit(1);
  }

  if ((bindreq = (struct t_bind *)t_alloc(listen_fd, T_BIND, T_ALL)) == NULL) {
    t_error("t_alloc of t_bind req structure failed");
    exit(2);
  }

  if ((bindret = (struct t_bind *)t_alloc(listen_fd, T_BIND, T_ALL)) == NULL) {
    t_error("t_alloc of t_bind ret structure failed");
    exit(2);
  }

  server = (struct sockaddr_in *)bindreq->addr.buf;
  server->sin_family = AF_INET;
  server->sin_addr.s_addr = INADDR_ANY;
  if (user_prefs.service_port != NULL)
    server->sin_port = htons(user_prefs.service_port);
  else
    server->sin_port = htons(3914);
  bindreq->qlen = 16;
  bindreq->addr.len = sizeof(struct sockaddr_in);
  bindret->addr.maxlen = sizeof(struct sockaddr_in);
  
  if (t_bind(listen_fd, bindreq, bindret) < 0) {
    t_error("t_bind failed for listen_fd");
    exit(3);
  }
  
  setpgrp();
  
  fcntl(listen_fd, F_SETFD, 1);	/* set close-on-exec for the client listener socket */
  
  return(listen_fd);
}
#endif				/* !defined(TLI) */
#endif				/* WINTERP_WANT_INET_SERVER */


#ifdef WINTERP_WANT_UNIX_SERVER
/******************************************************************************
 * initialize AF_UNIX server, returning a socket that can be listened on.
 * This code contributed by Victor Kan <kan@DG-RTP.DG.COM> and modified by 
 * Niels Mayer.
 ******************************************************************************/
static int Initialize_AF_UNIX_Server_Socket()
{
  int ls;			/* socket descriptor */
  struct sockaddr_un myaddr_un;
  char* socket_path;

  memset((char *) &myaddr_un, 0, sizeof(struct sockaddr_un));
  myaddr_un.sun_family = AF_UNIX;

  if (socket_path = getenv(DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR)) /* env var for port specification */
    user_prefs.unix_socket_filepath = socket_path;
  /* else user_prefs.unix_socket_filepath is set to DEFAULT_UNIX_SOCKET_FILEPATH value above */

#ifndef SOCKADDR_UN_MAXLEN
#define SOCKADDR_UN_MAXLEN 108	/* can't find SOCKADDR_UN_MAXLEN on hpux 7.0, however "char sun_path[108];" */ 
#endif
  if (strlen(user_prefs.unix_socket_filepath) > (SOCKADDR_UN_MAXLEN - 1)) {
    (void) fprintf(stderr, "%s: Error -- socket path %s must be shorter than %d bytes.\n",
		   app_name,
		   user_prefs.unix_socket_filepath,
		   SOCKADDR_UN_MAXLEN - 1);
    exit(1);
  }
  else
    strcpy(myaddr_un.sun_path, user_prefs.unix_socket_filepath);
  
  /*
   * Create the listen socket.
   */
  if ((ls = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
    perror(app_name);
    (void) sprintf(temptext, "socket() failed to create Unix Domain socket %s .\n",
		   user_prefs.unix_socket_filepath);
    xlfatal(temptext);
  }

  /*
   * Bind the listen address to the socket.
   */
  if (bind(ls, &myaddr_un, sizeof(myaddr_un.sun_family) + strlen(myaddr_un.sun_path)) == -1) {
    perror(app_name);
    (void) sprintf(temptext,
		   "Unable to bind() Unix Domain socket \"%s\".\n\t\
Note: you may need to do \"rm %s\" if last execution\n\t\
of %s terminated incorrectly. Alternately, another\n\t\
invocation of %smay be running, in which case you need\n\t\
to specify a different UnixDomain Socket file by setting\n\t\
environment variable %s, or by setting\n\t\
resource %s.unixSocketFilepath .\n",
		   user_prefs.unix_socket_filepath,
		   user_prefs.unix_socket_filepath,
		   app_name,
		   app_name,
		   DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR,
		   app_name);
    xlfatal(temptext);
  }

  /*
   * Initiate the listen on the socket so remote users
   * can connect.  The listen backlog is set to 5, which
   * is the largest currently supported.
   */
  if (listen(ls,5) == -1) {
    perror(app_name);
    (void) sprintf(temptext, "Unable to listen() on Unix Domain socket %s .",
		   user_prefs.unix_socket_filepath);
    xlfatal(temptext);
  }
  
  setpgrp();

  fcntl(ls, F_SETFD, 1);	/* set close-on-exec for the client listener socket */

  return(ls);
}
#endif				/* WINTERP_WANT_UNIX_SERVER */


#ifdef WINTERP_WANT_INET_SERVER
/******************************************************************************
 * Accept the request on client_AF_INET_listen_socket, and open a socket for
 * reading, rdsock. rdsock will be closed by Read_Eval_Print().
 ******************************************************************************/
#if !defined(TLI)
static int Accept_AF_INET_Server_Request(client_listen_socket)
     int client_listen_socket;
{ 
  int rdsock;
  int addrlen = sizeof(struct sockaddr_in);
  struct sockaddr_in peeraddr_in; /* for peer socket address */
#ifdef hpux			
  long lingerOpt = 1L;		/* NOTE: necessary while hpux-version < 8.0 (???) */
#else
  struct linger lingerOpt;
  lingerOpt.l_onoff  = 1;
  lingerOpt.l_linger = 10000;
#endif

  memset((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
  if ((rdsock = accept(client_listen_socket, &peeraddr_in, &addrlen)) == -1) {
    perror(app_name);
    xlfatal("Unable to accept() on INET Domain Socket."); /* CLEANUP & EXIT */
  }
  if (setsockopt(rdsock, SOL_SOCKET, SO_LINGER, (char *) &lingerOpt,
#ifdef hpux
		 sizeof(long)	/* NOTE: necessary while hpux-version < 8.0 (???) */
#else
		 sizeof(struct linger)
#endif
		 ) == -1) {
    perror(app_name);
    xlfatal("Unable to setsockopt() on INET Domain Socket."); /* CLEANUP & EXIT */
  }

  fcntl(rdsock, F_SETFD, 1);	/* set close-on-exec for the client read socket */

  return (rdsock);
}
#else
static int Accept_AF_INET_Server_Request(client_listen_socket)
     int client_listen_socket;
{
  int resfd;
  int flags=0;
  struct t_call *call;

  if ((resfd = t_open(TLI_TCP, O_RDWR, NULL)) < 0) {
    t_error("t_open for responding fd failed");
    exit(2);
  }

  if ((call = (struct t_call *)t_alloc(resfd, T_CALL, T_ALL)) == NULL) {
    t_error("t_alloc of t_call structure failed");
    exit(1);
  }
  
  if (t_listen(client_listen_socket, call) < 0) {
    t_error("t_listen failed for listen_fd");
    exit(1);
  }

  if (t_bind(resfd, NULL, NULL) < 0) {
    t_error("t_bind failed for responding fd failed");
    exit(3);
  }

  if (t_accept(client_listen_socket, resfd, call) < 0) {
    if (t_errno == TLOOK) {
      if (t_rcvdis(client_listen_socket, NULL) < 0) {
	t_error("t_rcvdis failed for listen_fd");
	exit(4);
      }
      if (t_close(resfd) < 0) {
	t_error("t_close failed for responding fd");
	exit(5);
      }
      xlfatal("INET connection dropped before we could accept\n");
      exit(6);
    }
    t_error("t_accept failed");
    exit(7);
  }

  t_free(call, T_CALL);
  return(resfd);
  
}
#endif				/* !defined(TLI) */
#endif				/* WINTERP_WANT_INET_SERVER */


#ifdef WINTERP_WANT_UNIX_SERVER
/******************************************************************************
 * Accept the request on client_AF_UNIX_listen_socket, and open a socket for
 * reading, rdsock. rdsock will be closed by Read_Eval_Print().
 * This code contributed by Victor Kan <kan@DG-RTP.DG.COM> and modified by 
 * Niels Mayer.
 ******************************************************************************/
static int Accept_AF_UNIX_Server_Request(client_listen_socket)
     int client_listen_socket;
{ 
  int rdsock;
  struct sockaddr_un peeraddr_un;
  int addrlen = sizeof (struct sockaddr_un);
  memset ((char *) &peeraddr_un, 0, sizeof (struct sockaddr_un));

  if ((rdsock = accept(client_listen_socket, &peeraddr_un, &addrlen)) == -1) {
    perror(app_name);
    xlfatal("Unable to accept() on Unix Domain socket."); /* cleanup and exit */
  }

  fcntl(rdsock, F_SETFD, 1);	/* set close-on-exec for the client read socket */

  return (rdsock);
}
#endif				/* WINTERP_WANT_UNIX_SERVER */


#ifdef WINTERP_WANT_INET_SERVER
/******************************************************************************
 * This procedure is called (indirectly, via AtAppAddInput() callback) from 
 * XtAppNextEvent() in main() and from XtAppNextEvent() in 
 * xldbug.c:breakloop(). This callback will be called whenever new input 
 * appears on client_AF_INET_listen_socket indicating that a new connection has been 
 * requested and that another s-expression is ready to be evaluated by Xlisp. 
 * This procedure will accept that connection and read all the data from the 
 * client and send it off to the XLisp reader, and the Xlisp evaluator. 
 * The results of the evaluation are printed.
 ******************************************************************************/
static void AF_INET_Read_Eval_Print(client_data, source_fildes, id)
     caddr_t    client_data;
     int*       source_fildes;
     XtInputId* id;
{
  Read_Eval_Print(Accept_AF_INET_Server_Request(client_AF_INET_listen_socket));
}
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
/******************************************************************************
 * This procedure is called (indirectly, via AtAppAddInput() callback) from 
 * XtAppNextEvent() in main() and from XtAppNextEvent() in 
 * xldbug.c:breakloop(). This callback will be called whenever new input 
 * appears on client_AF_UNIX_listen_socket indicating that a new connection has been 
 * requested and that another s-expression is ready to be evaluated by Xlisp. 
 * This procedure will accept that connection and read all the data from the 
 * client and send it off to the XLisp reader, and the Xlisp evaluator. 
 * The results of the evaluation are printed.
 ******************************************************************************/
static void AF_UNIX_Read_Eval_Print(client_data, source_fildes, id)
     caddr_t    client_data;
     int*       source_fildes;
     XtInputId* id;
{
  Read_Eval_Print(Accept_AF_UNIX_Server_Request(client_AF_UNIX_listen_socket));
}
#endif				/* WINTERP_WANT_UNIX_SERVER */


/******************************************************************************
 * This procedure is called from AF_UNIX_Read_Eval_Print() or
 * AF_INET_Read_Eval_Print(). Those procedures will accept the connections
 * requested on client_AF_UNIX_listen_socket or client_AF_INET_listen_socket
 * and return a read-socket <rdsock> from which this procedure will
 * read all the data from the client and send it off to the XLisp reader,
 * and the Xlisp evaluator.  The results of the evaluation are printed.
 ******************************************************************************/
static void Read_Eval_Print(rdsock)
     int rdsock;
{
  static char rdbuf[BUFSIZ];
  int len, i;
  LVAL sexp_stream, new_elt, last_elt = NIL;
#if defined(TLI)
  int flags=0;
#endif

  /* 
   * set this global flag so that main() and breakloop() will set up an error 
   * handler for the next call to the lisp evaluator.
   */
  read_eval_print_just_called = TRUE; 

  /*
   * Read the sexpression from the socket -- note assumption that entire
   * sexpression is sent in one "packet" and then the socket is closed.
   */

  xlsave1(sexp_stream);		/* protect from gc */
  sexp_stream = newustream();	/* note - stream obj has ptrs for head and tail*/

#if !defined(TLI)  
  while (len = recv(rdsock, rdbuf, BUFSIZ, 0)) { /* read len characters into rdbuf */
    if (len < 0) {
      perror(app_name);
      xlfatal("Unable to recv() on read socket."); /* CLEANUP & EXIT */
    }
#else
  for (;;) {
    len = t_rcv(rdsock, rdbuf, BUFSIZ, &flags);
    if (len < 0) {
      if (t_errno == TLOOK)
	if (t_look(rdsock) == T_ORDREL)
	  break;
      t_error(app_name);
      xlfatal("Unable to recv() on read socket."); /* CLEANUP & EXIT */
    }
#endif	/* !defined(TLI) */

    /* foreach character received, stuff it into an xlisp unnamed stream */
    for (i = 0; i < len; i++) {
      new_elt = cons(cvchar(rdbuf[i]), NIL);
      if (last_elt) {		/* if we've already created the head of the stream */
	rplacd(last_elt, new_elt); /* add new_elt to the tail of the list */
	last_elt = new_elt;	/* increment last_elt pointer */
      }
      else {			/* else create the head of the stream */
	sethead(sexp_stream, new_elt);
	last_elt = new_elt;
      }
    }
  }
#if defined(TLI)
    t_close(rdsock);		/* we've finished reading from the socket */
#else
    close(rdsock);
#endif /* TLI */
    
  if (last_elt)
    settail(sexp_stream, last_elt); /* streams are cdr-coded -- give ptr to tail */
  else			
    sexp_stream = NIL;		/* loop never executed, no characters read. */
  lisp_reader_hit_eof = !(Read_From_Stream_Eval_And_Print(sexp_stream));
  xlpop();			/*sexp_stream*/


  /* TODO -- 
     (1) make the client program, wl, wait until the evaluation is done. This will
     ensure that we don't get into a "race condition" with gnumeacs' winterp-mode --
     It is possible that winterp will still be reading winterp-mode's tempfile
     as gnuemacs writes another copy of this file. This can happen when a user
     is giving the gnuemacs winterp-send-defun command faster than winterp can
     read the files being sent to it.
     
     (2) send the results of the evaluation back to the client program wl, 
     have it print the results on stdout. Furthermore, if the form sent to
     winterp by wl results in a lisp error, wl should return a nonzero exitstatus. 
     
     This would be trivial, except that we'd want to send stdout and stderr
     back as well. If we were to use only the xlisp xlio.c routiunes for printing
     We could conceivably set the lisp symbols *standard-output* *debug-output*
     and *trace-output* so that they print to a stream, and just shove these
     streams back at the client.
     */

  /*
   * HACK CAUSED BY LAME IMPLEMENTATION OF XtMainLoop/XtAppNextEvent:
   * This creates a bogus event so as to force XtAppNextEvent to return, even if
   * the lisp evaluation didn't result in any new events being generated. 
   * The problem was that AtAppAddInput callbacks were being handled entirely 
   * within XtAppNextEvent(). Thus, once this procedure exited, XtAppNextEvent() 
   * would block waiting for a "real event", and never exit until an XEvent 
   * occured. XLISP requires that a new setjmp/longjmp error return be setup 
   * before each new lisp evaluation, and that couldn't happen unless 
   * XtAppNextEvent exited and allowed a new execution context to be created.
   *
   * Although I could do a call to XEventsQueued(display, QueuedAfterFlush)
   * in order to determine whether a bogus event needs to be sent, my hunch
   * is that the extra XFlush() caused by that operation would be more 
   * inefficient than processing/discarding the extra bogus event each time
   * a sexp is sent to the lisp server.
   */
  {
    XEvent bogus_event;
    bogus_event.type = 0;	/* XAnyEvent type --> ignored by XtDispatchEvent() */
    bogus_event.xany.display = display;
    bogus_event.xany.window  = XtWindow(toplevel_Wgt);;
    XPutBackEvent(display, &bogus_event);
  }
}


/*******************************************************************************
 * This fn reads from its input, which is assumed to be a xlisp stream.
 * returns false if EOF hit during read.
 ******************************************************************************/
static int Read_From_Stream_Eval_And_Print(sexp_stream)
     LVAL sexp_stream;		/* make sure this is a stream, and not other LVAL */
{
  extern int xldebug;
  extern LVAL s_1plus,s_2plus,s_3plus,s_1star,s_2star,s_3star,s_minus;
  LVAL rep_expr;
  int read_result;

  xlprot1(sexp_stream);		/* protect against GC */
    
  /* Read Evaluate and Print the expression in sexp_stream */
  if ((read_result = xlread(sexp_stream, &rep_expr, FALSE))) {

    /* save the last expression returned by the reader */
    setvalue(s_3plus, getvalue(s_2plus));
    setvalue(s_2plus, getvalue(s_1plus));
    setvalue(s_1plus, getvalue(s_minus));
    setvalue(s_minus, rep_expr);

    /* evaluate the expression returned by the reader */
    rep_expr = xleval(rep_expr);

    /* save the last expression returned by the evaluator */
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,rep_expr);

    if (xldebug)		/* print eval results */
      dbgprint(rep_expr);
    else
      stdprint(rep_expr);
  }

  else {			/* if reader hit EOF, just print a new line */
    if (xldebug)
      dbgputstr("\n");   
    else
      stdputstr("\n");
  }
  xlpop(/*sexp_stream*/);
  return (read_result);		/* return FALSE if hit EOF */
}


/*******************************************************************************
 * xlfatal - print a fatal error message and exit
 ******************************************************************************/
xlfatal(msg)
  char *msg;
{
  extern FILE *tfp;

  (void) fprintf(stderr, "%s -- error: %s\n", app_name, msg);

#ifdef WINTERP_WANT_INET_SERVER
  if (client_AF_INET_listen_socket)
    close(client_AF_INET_listen_socket);
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
  if (client_AF_UNIX_listen_socket) {
    close(client_AF_UNIX_listen_socket);
    unlink(user_prefs.unix_socket_filepath);
  }
#endif				/* WINTERP_WANT_UNIX_SERVER */

  if (tfp)
    fclose(tfp);

  if (app_context)
    XtDestroyApplicationContext(app_context);

  exit(1);
}


/*******************************************************************************
 * wrapup - clean up and exit to the operating system. 
 * This is also called in xlsys.c:xexit().
 ******************************************************************************/
wrapup()
{
  extern FILE *tfp;

  stdputstr("\n");

#ifdef WINTERP_WANT_INET_SERVER
  if (client_AF_INET_listen_socket)
    close(client_AF_INET_listen_socket);
#endif				/* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
  if (client_AF_UNIX_listen_socket) {
    close(client_AF_UNIX_listen_socket);
    unlink(user_prefs.unix_socket_filepath);
  }
#endif				/* WINTERP_WANT_UNIX_SERVER */

  if (tfp)
    fclose(tfp);

  if (app_context)
    XtDestroyApplicationContext(app_context);

  exit(0);
}

/*******************************************************************************
 * This is the protocol callback for application shells created in WINTERP.
 * see toplevel_Wgt above, and also APPLICATION_SHELL_WIDGET_CLASS in
 * wc_SHELL.c.
 ******************************************************************************/
void Winterp_Application_Shell_WMDelete_Callback(shell, closure, call_data)
     Widget shell;
     XtPointer closure;
     XtPointer call_data;
{
  wrapup();
}

/*******************************************************************************
 * This handles fatal errors from the Xtoolkit. According to the Xtoolkit
 * docs, such a handler should terminate the application. In this case,
 * however, we suggest to the user that the application be terminated, but
 * don't actually do it. This may allow the user to figure out what went 
 * wrong by poking around inside the lisp environment.
 *
 * This is set up in main() via XtAppSetErrorHandler(). Note that the default
 * error handler is _XtDefaultError().
 ******************************************************************************/
static void Winterp_Xtoolkit_Error_Handler(message)
     String message;
{
  (void) sprintf(temptext,
		 "X Toolkit Fatal Error -- PLEASE QUIT AND RESTART THIS APPLICATION:\n\t%s\n",
		 message);
  xlfail(temptext);
}


/*******************************************************************************
 * This handles nonfatal errors from the Xtoolkit.
 *
 * This is set up in main() via XtAppSetWarningHandler(). Note that the default
 * error handler is _XtDefaultWarning().
 ******************************************************************************/
static void Winterp_Xtoolkit_Warning_Handler(message)
     String message;
{
  (void) sprintf(temptext,
		 "X Toolkit Warning:\n\t%s\n",
		 message);
  xlfail(temptext);
}


/*******************************************************************************
 * The following code is from X11r4:mit/lib/X/XlibInt.c.
 * Copyright    Massachusetts Institute of Technology    1985, 1986, 1987.
 ******************************************************************************/
static int Winterp_XPrintDefaultError (dpy, event, fp)
    Display *dpy;
    XErrorEvent *event;
    FILE *fp;
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "XlibMessage";
    register _XExtension *ext = (_XExtension *)NULL;
    XGetErrorText(dpy, event->error_code, buffer, BUFSIZ);
    XGetErrorDatabaseText(dpy, mtype, "XError", "X Error", mesg, BUFSIZ);
    (void) fprintf(fp, "%s:  %s\n  ", mesg, buffer);
    XGetErrorDatabaseText(dpy, mtype, "MajorCode", "Request Major code %d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->request_code);
    if (event->request_code < 128) {
	sprintf(number, "%d", event->request_code);
	XGetErrorDatabaseText(dpy, "XRequest", number, "", buffer, BUFSIZ);
    } else {
	for (ext = dpy->ext_procs;
	     ext && (ext->codes.major_opcode != event->request_code);
	     ext = ext->next)
	  ;
	if (ext)
	    strcpy(buffer, ext->name);
	else
	    buffer[0] = '\0';
    }
    (void) fprintf(fp, " (%s)\n  ", buffer);
    XGetErrorDatabaseText(dpy, mtype, "MinorCode", "Request Minor code %d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->minor_code);
    if (ext) {
	sprintf(mesg, "%s.%d", ext->name, event->minor_code);
	XGetErrorDatabaseText(dpy, "XRequest", mesg, "", buffer, BUFSIZ);
	(void) fprintf(fp, " (%s)", buffer);
    }
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "ResourceID", "ResourceID 0x%x",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->resourceid);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "ErrorSerial", "Error Serial #%d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->serial);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "CurrentSerial", "Current Serial #%d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, dpy->request);
    fputs("\n", fp);
    if (event->error_code == BadImplementation) return 0;
    return 1;
}


/*******************************************************************************
 * This handles errors from Xlib. It is set up in main() via XSetErrorHandler().
 *
 * By default, the Xlib error handler is:
 *
 * int _XDefaultError(dpy, event)
 * 	Display *dpy;
 * 	XErrorEvent *event;
 * {
 *     if (_XPrintDefaultError (dpy, event, stderr) == 0) return 0;
 *     exit(1);
 * }
 *
 * However for WINTERP, we don't want to have exit() called on such errors,
 * rather we call xlfail() to indicate an error occured and to throw us into
 * the debug loop.
 ******************************************************************************/
static int Winterp_Xlib_Error_Handler(dpy, event)
     Display*     dpy;
     XErrorEvent* event;
{

  (void) Winterp_XPrintDefaultError (dpy, event, stderr);
  xlfail("Xlib error detected.");
  return (0);
}
