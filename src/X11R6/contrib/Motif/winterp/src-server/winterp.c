/* -*-C-*-
********************************************************************************
*
* File:         winterp.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/winterp.c,v 2.9 1994/06/13 09:02:01 npm Exp $
* Description:  WINTERP main() file.
* Author:       Niels Mayer
* Created:      Sat Jun 10 02:15:35 1989
* Modified:     Sun Jun 12 01:19:57 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/winterp.c,v 2.9 1994/06/13 09:02:01 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>		/* for unlink(), getenv(), etc */
#include <fcntl.h>		/* needed for fcntl(2) calls */
#include <ctype.h>
#include <signal.h>		/* for signal() handling */

#include "../src-server/config.h" /* define DEFAULT_UNIX_SOCKET_FILEPATH DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR, etc */

#if (defined(WINTERP_WANT_INET_SERVER) || defined(WINTERP_WANT_UNIX_SERVER))
#include <sys/types.h>
#include <sys/socket.h>
#endif /* (defined(WINTERP_WANT_INET_SERVER) || defined(WINTERP_WANT_UNIX_SERVER)) */

#ifdef WINTERP_WANT_INET_SERVER
#include <netinet/in.h>
#include <netdb.h>
#endif /* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
#include <sys/un.h> /* for AF_UNIX sockets */
#endif /* WINTERP_WANT_UNIX_SERVER */

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>

#include "winterp.h"

/* this must come after winterp.h since WINTERP_MOTIF_11 may be def'd there */
#ifdef WINTERP_MOTIF_12		/* Motif >= 1.2 */
#include <X11/Xlibint.h>	/* unfortunately, this is needed for Winterp_XPrintDefaultError().
				   I tried putting this just ahead of that procedure at the end of
				   this file (to minimize side-effects of this include) but SunOS 4.1.3
				   compilation with X11r6 causes compilation error -- something about
				   redefining memset(), perhaps from Xfuncs.h */
#endif /* WINTERP_MOTIF_12 */

#ifdef WINTERP_MOTIF_11
#include <Xm/Protocols.h>	/* <Xm/Protocols.h> location seems to have moved in 1.1 */
#else
#include <X11/Protocols.h>
#endif /* WINTERP_MOTIF_11 */

#ifdef WINTERP_MOTIF_12		/* Motif >= 1.2 */
#include <Xm/RepType.h>		/* define XmRepTypeInstallTearOffModelConverter() */
#endif /* WINTERP_MOTIF_12 */

#ifdef WINTERP_XWEB		/* stuff for CYY's X Widget Extensible Builder */
#include  "/usr/local/xweb/XWEB/XWeb.appDefs"
#endif /* WINTERP_XWEB */

/* forward declarations */
static void Read_Eval_Print();
static void Winterp_Xtoolkit_Error_Handler();
static void Winterp_Xtoolkit_Warning_Handler();
static int  Winterp_Xlib_Error_Handler();
static void Winterp_Signal_Handler();
#ifdef WINTERP_WANT_INET_SERVER
static void AF_INET_Read_Eval_Print();
static int  Initialize_AF_INET_Server_Socket();
#endif /* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
static void AF_UNIX_Read_Eval_Print();
static int  Initialize_AF_UNIX_Server_Socket();
#endif /* WINTERP_WANT_UNIX_SERVER */
#ifdef WINTERP_WANT_STDIN_SERVER
static void STDIN_Read_Eval_Print();
static LVAL s_stdin_ustream = (LVAL) NULL;
#endif /* WINTERP_WANT_STDIN_SERVER */

/* global variables */
#ifdef SAVERESTORE
jmp_buf		top_level;
#endif /* SAVERESTORE */
CONTEXT		cntxt;
char*		app_name = NULL;
char*		app_class = NULL;
static Boolean	lisp_reader_hit_eof = FALSE;
       int	winterp_caught_signal = FALSE;
static int	client_AF_INET_listen_socket = 0;
static int	client_AF_UNIX_listen_socket = 0;
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

#ifdef hpux
char Error[128];		/* ??? defining this prevents weird linker problem caused by -lPW on hpux 8.0 ??? -- see /usr/include/macros.h */
#endif /* hpux */




/****************************************************************************** 
 * Data on how user-customization resources are interpreted:
 * this must be kept up to date with data structure USER_PREFS_DATA_PTR 
 * in user_prefs.h
 ******************************************************************************/
static XtResource user_prefs_resources[] = {
  /*
   * The name of the file to load to initialize xlisp.
   */
  {"lispInitFile", "LispInitFile",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, lisp_init_file),
#else
     XtOffsetOf(USER_PREFS_DATA, lisp_init_file),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_LISP_INIT_FILE},

  /*
   * The name of the file to output lisp transactions.
   */
  {"lispTranscriptFile", "LispTranscriptFile",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, lisp_transcript_file),
#else
     XtOffsetOf(USER_PREFS_DATA, lisp_transcript_file),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_LISP_TRANSCRIPT_FILE},

  /*
   * The name of the default directory for 'load'. This is only
   * used in cases where 'load' wasn't supplied a full
   * filepath (i.e. a path beginning with '/' or '.').
   *
   * Note that "lispLibDir" should be the path to an existing directory with
   * a trailing '/', e.g. "/usr/local/winterp/lisp-lib/".
   */
  {"lispLibDir", "LisplibDir",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, lisp_lib_dir),
#else
     XtOffsetOf(USER_PREFS_DATA, lisp_lib_dir),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_LISP_LIB_DIR},

  /*
   * The ':' separated list of paths searched by LOAD and RESTORE. This is only
   * used in cases where 'load' wasn't supplied a full
   * filepath (i.e. a path beginning with '/' or '.'). 
   *
   * If there exists an environment variable XLPATH, it's value will override
   * this resource. When searching for a file that is not fully qualified
   * (not beginning w '/' or '.'), the path will be searched, then 
   * <lispLibDir>/<file>, then ./<file>.
   */
  {"lispLoadPath", "LispLoadPath",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, lisp_load_path),
#else
     XtOffsetOf(USER_PREFS_DATA, lisp_load_path),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_LISP_LOAD_PATH},
  
  /*
   * Setting this boolean to FALSE will allow WINTERP to startup
   * without printing lots of output.
   */
  {"enableInitMsgs", "EnableInitMsgs",
     XmRBoolean, sizeof(Boolean),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_init_msgs),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_init_msgs),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_INIT_MSGS},

  /*
   * The port number of the widget interpreter lisp server.
   */
  {"servicePort", "ServicePort",
     XmRInt, sizeof(int),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, service_port),
#else
     XtOffsetOf(USER_PREFS_DATA, service_port),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_INET_SERVICE_PORT},

  /*
   * The service name of the widget interpreter lisp server.
   */
  {"serviceName", "ServiceName",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, service_name),
#else
     XtOffsetOf(USER_PREFS_DATA, service_name),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_INET_SERVICE_NAME},

  /*
   * Setting this boolean to TRUE will start up WINTERP so that
   * it will accept input from its INET Domain Server. Those worried about
   * security when running winterp-based applications will want to
   * set this to FALSE in the application defaults file for the application.
   */
  {"enableInetServer", "enableInetServer",
     XmRBoolean, sizeof(Boolean),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_AF_INET_server),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_AF_INET_server),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_INET_SERVER},

  /*
   * Setting this boolean to FALSE will start up WINTERP without
   * it's Unix Domain server. Those worried about security when running
   * winterp-based applications on a multi-user machine will want
   * to set this in the  application defaults file for the application.
   */
  {"enableUnixServer", "enableUnixServer",
     XmRBoolean, sizeof(Boolean),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_AF_UNIX_server),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_AF_UNIX_server),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_UNIX_SERVER},

  /*
   * This is set to the full pathname for the AF_UNIX domain socket-file
   */
  {"unixSocketFilepath", "UnixSocketFilepath",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, unix_socket_filepath),
#else
     XtOffsetOf(USER_PREFS_DATA, unix_socket_filepath),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_UNIX_SOCKET_FILEPATH},

#ifdef SAVERESTORE
  /*
   * The name of the file to load to 'restore' a saved xlisp image (*.wks)
   */
  {"lispRestoreFile", "LispRestoreFile",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, lisp_restore_file),
#else
     XtOffsetOf(USER_PREFS_DATA, lisp_restore_file),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_LISP_RESTORE_FILE},

  /*
   * Setting this boolean to FALSE will allow WINTERP to startup
   * without 'restore'ing a previously 'saved' image
   */
  {"enableLispRestore", "EnableLispRestore",
     XmRBoolean, sizeof(Boolean),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_lisp_restore),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_lisp_restore),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_LISP_RESTORE},
#endif /* SAVERESTORE */

  /*
   * The string displayed in the WINTERP "default window" at initialization time.
   * Note: display of initialization message-box is is not disabled by
   * "enableInitMsgs: FALSE"
   */
  {"initMessageString", "InitMessageString",
     XmRString, sizeof(String),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, init_message_string),
#else
     XtOffsetOf(USER_PREFS_DATA, init_message_string),
#endif /* WINTERP_MOTIF_11 */
     XmRString, (XtPointer) DEFAULT_INIT_MESSAGE_STRING},

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
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_XtError_break),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_XtError_break),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_XT_ERROR_BREAK},

  /*
   * Setting this boolean to FALSE will start up WINTERP
   * with the Xtoolkit's default XtWarning handler -- any XtWarnings
   * will just cause a message to be printed, execution will continue.
   * By default, this is FALSE which means that a warning message will get
   * printed, but Lisp will not break. This is set to FALSE by default
   * because some XtWarnings were not meant to be broken out of and can
   * leave Motif in a weird state, causing possible subsequent core-dumps.
   * If you know don't know what you're doing I suggest leaving this
   * resource at the default FALSE value.
   */
  {"enableXtWarningBreak", "EnableXtWarningBreak",
     XmRBoolean, sizeof(Boolean),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_XtWarning_break),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_XtWarning_break),
#endif /* WINTERP_MOTIF_11 */
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
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_XError_break),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_XError_break),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_X_ERROR_BREAK},

  /*
   * Setting this boolean to TRUE will cause read/eval/print to occur
   * thourgh the terminal emulator running WINTERP -- enabling this
   * makes using WINTERP seem more like using XLISP.
   */
  {"enableStdinServer", "EnableStdinServer",
     XmRBoolean, sizeof(Boolean),
#ifndef WINTERP_MOTIF_11	/* for Motif1.0/X11r3 back-compatibility */
     XtOffset(USER_PREFS_DATA_PTR, enable_STDIN_server),
#else
     XtOffsetOf(USER_PREFS_DATA, enable_STDIN_server),
#endif /* WINTERP_MOTIF_11 */
     XmRImmediate, (XtPointer) DEFAULT_ENABLE_STDIN_SERVER}
};


/******************************************************************************
 * Table indicating how to set-from-the-command-line the application-specific
 * resources specified in user_prefs_resources[] above.
 ******************************************************************************/
static XrmOptionDescRec user_prefs_cmdline_opts[] = {
  {"-init_file",	".lispInitFile",	XrmoptionSepArg, NULL},
  {"-transcript_file",	".lispTranscriptFile",	XrmoptionSepArg, NULL},
  {"-lib_dir",		".lispLibDir",		XrmoptionSepArg, NULL},
  {"-load_path",	".lispLoadPath",	XrmoptionSepArg, NULL},
  {"-init_message",	".initMessageString",	XrmoptionSepArg, NULL},
  {"-no_init_msgs",	".enableInitMsgs",	XrmoptionNoArg, "false"},
  {"-enable_init_msgs",	".enableInitMsgs",	XrmoptionNoArg, "true"},
  {"-serv_port",	".servicePort",		XrmoptionSepArg, NULL},
  {"-serv_name",	".serviceName",		XrmoptionSepArg, NULL},
  {"-no_inet_server",	".enableInetServer",	XrmoptionNoArg, "false"},
  {"-enable_inet_server",".enableInetServer",	XrmoptionNoArg, "true"},
  {"-no_unix_server",	".enableUnixServer",	XrmoptionNoArg, "false"},
  {"-enable_unix_server",".enableUnixServer",	XrmoptionNoArg, "true"},
  {"-unix_socket_file",	".unixSocketFilepath",	XrmoptionSepArg, NULL},
#ifdef SAVERESTORE
  {"-lisp_restore_file",".lispRestoreFile",	XrmoptionSepArg, NULL},
  {"-no_lisp_restore",	".enableLispRestore",	XrmoptionNoArg, "false"},
  {"-enable_lisp_restore",".enableLispRestore",	XrmoptionNoArg, "true"},
#endif /* SAVERESTORE */
  {"-no_xterr_brk",	".enableXtErrorBreak",	XrmoptionNoArg, "false"},
  {"-enable_xterr_brk",	".enableXtErrorBreak",	XrmoptionNoArg, "true"},
  {"-no_xtwarn_brk",	".enableXtWarningBreak",XrmoptionNoArg, "false"},
  {"-enable_xtwarn_brk",".enableXtWarningBreak",XrmoptionNoArg, "true"},
  {"-no_xerr_brk",	".enableXErrorBreak",	XrmoptionNoArg, "false"},
  {"-enable_xerr_brk",	".enableXErrorBreak",	XrmoptionNoArg, "true"},
  {"-no_stdin_server",	".enableStdinServer",	XrmoptionNoArg, "false"},
  {"-enable_stdin_server",".enableStdinServer",	XrmoptionNoArg, "true"}
};


/******************************************************************************
 * Setup an action table for winterp. Note that action procedure "Lisp"
 * is a special action procedure that calls the lisp evaluator on the
 * parameters of the action. A translation like
 * "Ctrl<Key>K: Lisp(quack 1 2 3)" will evaluate '(quack 1 2 3)'
 ******************************************************************************/
static XtActionsRec winterp_action_table[] = {
  {"Lisp", Wtx_Winterp_Lisp_Action_Proc}
};


/*******************************************************************************
 * main - the main routine
 ******************************************************************************/
#ifdef WINTERP_EMBEDDED
winterp_embedded_main(argc, argv)
#else /* standard winterp */
main(argc, argv)
#endif /* WINTERP_EMBEDDED */
  int argc; char *argv[];
{
  extern LVAL true;		/* from xlisp/xlglob.c */
  extern FILE *tfp;		/* from xlisp/xlglob.c */
  char** original_argv;
  int    original_argc;
  int	 image_not_restored_p;	/* TRUE if load of lib-utils/initialize.lsp needed */

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

#ifdef WINTERP_XWEB	/* stuff for CYY's X Widget Extensible Builder */
   xwebInit(argc,argv);  
#endif /* WINTERP_XWEB */

  /* 
   * Initialize the toolkit
   */
  XtToolkitInitialize();

  app_context = XtCreateApplicationContext();
  display = XtOpenDisplay(app_context, (String) NULL, app_name, app_class,
			  user_prefs_cmdline_opts, XtNumber(user_prefs_cmdline_opts),
			  &argc, argv);
  if (!display)
    xlfatal("Can't open display -- XtOpenDisplay() failed.");

  if (argc > 1) {		/* if argc!=0, then there are invalid arguments that didn't get parsed by XtOpenDisplay() */
    (void) fprintf (stderr, "usage: %s [-class <classname>] [-init_file <file.lsp>]\n", app_name);
    (void) fprintf (stderr, "\t[-transcript_file <file.out>] [-lib_dir <path-to-lib-dir>]\n");
    (void) fprintf (stderr, "\t[-load_path <:-separated-list-of-paths-to-load-dir>]\n");
#ifdef SAVERESTORE
    (void) fprintf (stderr, "\t[-lisp_restore_file <file.wks>]\n");
    (void) fprintf (stderr, "\t[-no_lisp_restore] [-enable_lisp_restore]\n");
#endif /* SAVERESTORE */
    (void) fprintf (stderr, "\t[-no_init_msgs] [-enable_init_msgs]\n");
    (void) fprintf (stderr, "\t[-init_message <string>]\n");
#ifdef WINTERP_WANT_INET_SERVER
    (void) fprintf (stderr, "\t[-serv_port <portnum>] [-serv_name <servname>]\n");
    (void) fprintf (stderr, "\t[-no_inet_server] [-enable_inet_server]\n");
#endif /* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
    (void) fprintf (stderr, "\t[-no_unix_server] [-enable_unix_server]\n");
    (void) fprintf (stderr, "\t[-unix_socket_file <socket-filepath>]\n");
#endif /* WINTERP_WANT_UNIX_SERVER */
#ifdef WINTERP_WANT_STDIN_SERVER
    (void) fprintf (stderr, "\t[-no_stdin_server] [-enable_stdin_server]\n");
#endif /* WINTERP_WANT_STDIN_SERVER */
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
  wm_delete_atom = XmInternAtom(display, "WM_DELETE_WINDOW", FALSE);

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
  ARGLIST_ADD(XmNgeometry, (XtArgVal) "+1+1"); /* we don't want user to have to place this window, so give it a location */
  toplevel_Wgt = XtAppCreateShell(app_name, app_class, applicationShellWidgetClass, display, ARGLIST());
  XmAddWMProtocolCallback(toplevel_Wgt, wm_delete_atom, Winterp_Application_Shell_WMDelete_Callback, NULL);
  XtGetApplicationResources(toplevel_Wgt, &user_prefs, user_prefs_resources, XtNumber(user_prefs_resources), NULL, 0); /* place application resources in user_prefs global struct. */

#ifdef WINTERP_XWEB	/* stuff for CYY's X Widget Extensible Builder */
  XmuRegisterApplicationDefaults(display, myAppDefaults,XtNumber(myAppDefaults));
#endif /* WINTERP_XWEB */

  {
    XmString xmstr;
    Widget   msg_w;

    if (user_prefs.init_message_string)
      xmstr = XmStringCreateLtoR(user_prefs.init_message_string,
#ifdef WINTERP_MOTIF_12
				 XmFONTLIST_DEFAULT_TAG
#else /* Motif 1.1 or 1.0 */
				 XmSTRING_DEFAULT_CHARSET
#endif /* WINTERP_MOTIF_12 */
				 );
    else {
      (void) sprintf(temptext,
		     "Initializing... Please Wait.\n   app-name: '%s'\n   app-class: '%s'",
		     app_name, app_class
		     );
      xmstr = XmStringCreateLtoR(temptext,
#ifdef WINTERP_MOTIF_12
				 XmFONTLIST_DEFAULT_TAG
#else /* Motif 1.1 or 1.0 */
				 XmSTRING_DEFAULT_CHARSET
#endif /* WINTERP_MOTIF_12 */
				 );
    }
    ARGLIST_RESET();
    ARGLIST_ADD(XmNdialogType,    (XtArgVal) XmDIALOG_WORKING);
    ARGLIST_ADD(XmNmessageString, (XtArgVal) xmstr);
    msg_w = XmCreateMessageBox(toplevel_Wgt, "appInitMsg", ARGLIST());
    XtUnmanageChild(XmMessageBoxGetChild(msg_w, XmDIALOG_OK_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(msg_w, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(msg_w, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(msg_w, XmDIALOG_SEPARATOR));
    XtManageChild(msg_w);
    XmStringFree(xmstr);

    XtRealizeWidget(toplevel_Wgt); /* give the order to create the windows, etc. */
    Wut_Wait_Till_Expose(msg_w);
    XtDestroyWidget(msg_w);
  }

  XtUnmapWidget(toplevel_Wgt);	/* once the windows are created by XtRealizeWidget()/XmUpdateDisplay(), we may hide the window by unmapping */
  XtFree((char*) original_argv); /* Motif makes a copy of this upon setting XmNargv resource however, if this is placed after XtAppCreateShell() call, you get a coredump... */

#ifdef WINTERP_MOTIF_12		/* Motif >= 1.2 */
  XmRepTypeInstallTearOffModelConverter(); /* this installs resource converters for XmNtearOffModel, not installed by default */
#endif /* WINTERP_MOTIF_12 */

  /*
   * Sanity check to ensure that the version of the Motif toolkit libraries
   * used correspond to the Motif toolkit header <Xm/Xm.h>.
   * This test is only valid after XmRegisterConverters(), has been called;
   * the call occurs in Manager/Primitive/Vendor ClassInitialize() which
   * occurs implicitly above in XmCreateMessageBox()...
   */
  if (xmUseVersion != XmVersion) { /* XmVersion def'd and xmUseVersion externed in <Xm/Xm.h> */
    (void) sprintf(temptext, "application must be recompiled with <Xm/Xm.h> matching libXm.a\n\t\t\
(header version == %d, library version == %d).",
		   XmVersion, xmUseVersion);
    xlfatal(temptext);
  }

  /*
   * Get Xtoolkit's default foreground and background Pixels, set globals
   * to these values.
   */
  {
    XrmValue from, to;

    from.size = (unsigned int) strlen(XtDefaultForeground) + 1;
    from.addr = (XtPointer) XtDefaultForeground;
    to.size = (unsigned int) sizeof(Pixel);
    to.addr = (XtPointer) &default_foreground_pixel;
    XtConvert(toplevel_Wgt, XmRString, &from, XmRPixel, &to);
    if (to.addr == NULL)	/* error if conversion failed */
      xlfatal("XtConvert() couldn't convert XtDefaultForeground to XmRPixel.");

    from.size = (unsigned int) strlen(XtDefaultBackground) + 1;
    from.addr = (XtPointer) XtDefaultBackground;
    to.size = (unsigned int) sizeof(Pixel);
    to.addr = (XtPointer) &default_background_pixel;
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
    (void) XtAppAddInput(app_context, client_AF_INET_listen_socket, (XtPointer) XtInputReadMask,
			 AF_INET_Read_Eval_Print, NULL);
  }
#endif /* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
  if (user_prefs.enable_AF_UNIX_server) {
    /*
     * get a socket to listen on. when it's selected, call AF_UNIX_Read_Eval_Print()
     * to open a connection socket, process the client request, and close the socket
     */
    client_AF_UNIX_listen_socket = Initialize_AF_UNIX_Server_Socket();
    (void) XtAppAddInput(app_context, client_AF_UNIX_listen_socket, (XtPointer) XtInputReadMask,
			 AF_UNIX_Read_Eval_Print, NULL);
  }
#endif /* WINTERP_WANT_UNIX_SERVER */


#ifdef WINTERP_WANT_STDIN_SERVER
  if (user_prefs.enable_STDIN_server) {
    /*
     * On input on stdin, call STDIN_Read_Eval_Print(), this will read all characters
     * currently on stdin. If enough chars have been read to read a s-expression, the
     * xlisp evaluator will be called on the text read so far.
     */
    (void) XtAppAddInput(app_context, fileno(stdin), (XtPointer) XtInputReadMask,
			 STDIN_Read_Eval_Print, NULL);
    /* setbuf(stdin, NULL); */
    /* (void) fcntl(fileno(stdin), F_SETFL, O_NDELAY); */
  }
#endif /* WINTERP_WANT_STDIN_SERVER */

  /* 
   * Setup Unix signal handlers to clean up gracefully when WINTERP gets killed
   */
#ifdef WINTERP_HANDLE_NASTY_SIGNALS
  /* WINTERP_HANDLE_NASTY_SIGNALS -- setting this might enable WINTERP to clean
     up after itself (e.g. delete unix domain sockets, etc) prior to crashing
     but it's not clear that this is portable, or for which cases it would work... */
  if (signal(SIGABRT, SIG_IGN) != SIG_IGN)
    signal(SIGABRT, Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGABRT ignored\n");
  if (signal(SIGILL,  SIG_IGN) != SIG_IGN)
    signal(SIGILL,  Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGILL ignored\n");
  if (signal(SIGSEGV, SIG_IGN) != SIG_IGN)
    signal(SIGSEGV, Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGSEGV ignored\n");
  if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
    signal(SIGQUIT, Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGQUIT ignored\n");
#endif /* WINTERP_HANDLE_NASTY_SIGNALS */
  if (signal(SIGHUP,  SIG_IGN) != SIG_IGN)
    signal(SIGHUP,  Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGHUP ignored\n");
  if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
    signal(SIGTERM, Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGTERM ignored\n");
  if (signal(SIGINT,  SIG_IGN) != SIG_IGN)
    signal(SIGINT,  Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGINT ignored\n");
  if (signal(SIGFPE,  SIG_IGN) != SIG_IGN)
    signal(SIGFPE,  Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGFPE ignored\n");
  if (signal(SIGPIPE, SIG_IGN) != SIG_IGN)
    signal(SIGPIPE, Winterp_Signal_Handler);
  else if (user_prefs.enable_init_msgs)
    (void) fprintf(stderr, "Warning: SIGPIPE ignored\n");

  /*
   * Setup Xlib and Xtoolkit warning and error handlers so that errors inside
   * X or Xtoolkit will call xlerror() when
   * user_prefs.{enable_xerr_brk,enable_xterr_brk} (respectively) are TRUE.
   * they call xlfatal() to cleanup and exit if FALSE...
   */
  if (user_prefs.enable_XtWarning_break)
    (void) XtAppSetWarningHandler(app_context, Winterp_Xtoolkit_Warning_Handler);
  (void) XtAppSetErrorHandler(app_context, Winterp_Xtoolkit_Error_Handler);
  XSetErrorHandler(Winterp_Xlib_Error_Handler);

  if (user_prefs.enable_init_msgs) {
    (void) printf("================================================================================\n");
#if (defined(WINTERP_MOTIF_113) && !defined(WINTERP_MOTIF_12))
    (void) printf("WINTERP -- Motif 1.1.3 Widget INTERPreter\n");
#else				/* !defined(WINTERP_MOTIF_113) */
#if (defined(WINTERP_MOTIF_111) && !defined(WINTERP_MOTIF_12))
    (void) printf("WINTERP -- Motif 1.1.1 Widget INTERPreter\n");
#else				/* !defined(WINTERP_MOTIF_111) --> Plain old 1.0 or 1.1 */
    (void) printf("WINTERP -- Motif %d.%d Widget INTERPreter\n", XmVERSION, XmREVISION); /* from <Xm/Xm.h> */
#endif /* WINTERP_MOTIF_111 */
#endif /* WINTERP_MOTIF_113 */
    (void) printf("    by Niels P. Mayer (mayer@netcom.com).\n");
    (void) printf("    WINTERP %d.%d, Copyright (c) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.\n",
		  WINTERP_VERSION_INT, WINTERP_REVISION_INT); /* from winterp.h */
    (void) printf("    WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.\n");
    (void) printf("    WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.\n");
    (void) printf("    XLISP %d.%d (XLISP-PLUS), Copyright (c) 1985-1989, David Betz.\n",
		  XLISP_VERSION_INT, XLISP_REVISION_INT); /* from xlisp/xlisp.h */
    (void) printf("    Portions of XLISP-PLUS from XLISP-STAT Copyright (c) 1988, Luke Tierney.\n");
#ifdef WINTERP_XTANGO_WIDGET
    (void) printf("    Xtango by John T. Stasko and Doug Hayes;\n        Copyright (c) 1990 Georgia Institute of Technology.\n");
#endif /* WINTERP_XTANGO_WIDGET */
#ifdef HP_GRAPH_WIDGET
    (void) printf("    XmGraph and XmArc widgets by Doug Young and Luis Miguel;\n        Copyright (c) 1988 Hewlett-Packard Co.\n");
#endif /* HP_GRAPH_WIDGET */
#ifdef WINTERP_TABLE_WIDGET
    (void) printf("    Table widget by David Harrison, Martin Brunecky, Kee Hinckley, etc;\n        (Public Domain).\n");
#endif /* WINTERP_TABLE_WIDGET */
#ifdef WINTERP_EXPECT_SUBPROCESS
    (void) printf("    Expect subprocess package by Don Libes;\n        (Public domain).\n");
#endif /* WINTERP_EXPECT_SUBPROCESS */
    (void) printf("    Portions of WINTERP from Dan Heller's \"Motif Programming Manual\";\n        Copyright 1991, O'Reilly && Associates.\n");
    (void) printf("    XLISP-PLUS contains contributed code by: Tom Almy, Johnny Greenblatt,\n        Neal Holtz, Niels Mayer, Blake McBride, Mikael Pettersson, Luke Tierney,\n        Ken Whedbee, and Pete Yadlowsky.\n\n");

    (void) printf("Note: optional compiled-in features:\n    { ");
#ifdef WINTERP_TABLE_WIDGET
    (void) printf("TABLE_WIDGET_CLASS ");
#endif /* WINTERP_TABLE_WIDGET */
#ifdef WINTERP_TREE_WIDGET
    (void) printf("TREE_WIDGET_CLASS ");
#endif /* WINTERP_TREE_WIDGET */
#ifdef WINTERP_XTANGO_WIDGET
    (void) printf("TANGO:WIDGET_CLASS ");
#endif /* WINTERP_XTANGO_WIDGET */
#ifdef HP_GRAPH_WIDGET
    (void) printf("XM_GRAPH_WIDGET_CLASS XM_ARC_WIDGET_CLASS ");
#endif /* HP_GRAPH_WIDGET */
#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.X and IndigoMagic desktop */
    (void) printf("SG_DROP_POCKET_WIDGET_CLASS SG_FINDER_WIDGET_CLASS ");
#endif /* SGI_DROP_POCKET_WIDGET */
#ifdef WINTERP_EXPECT_SUBPROCESS
    (void) printf("EXPECT-SUBPROCESSES ");
#endif /* WINTERP_EXPECT_SUBPROCESS */
#ifdef WINTERP_WANT_INET_SERVER
    (void) printf("INET-READ-EVAL-PRINT ");
#endif /* WINTERP_WANT_INET_SERVER */
#ifdef WINTERP_WANT_UNIX_SERVER
    (void) printf("UNIX-DOMAIN-READ-EVAL-PRINT ");
#endif /* WINTERP_WANT_UNIX_SERVER */
#ifdef WINTERP_WANT_STDIN_SERVER
    (void) printf("STDIN-READ-EVAL-PRINT ");
#endif /* WINTERP_WANT_STDIN_SERVER */
    (void) printf("}.\n\n");
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
#ifdef SAVERESTORE
  if (setjmp(top_level))
    xlfatal("XLISP RESTORE not allowed during initialization.");
#endif /* SAVERESTORE */

  /* initialize xlisp */
#ifdef SAVERESTORE
  image_not_restored_p = xlinit(user_prefs.lisp_restore_file); /* xlisp/xlinit.c */
#else /* !defined(SAVERESTORE) */
  image_not_restored_p = xlinit(NULL); /* xlisp/xlinit.c */
#endif /* SAVERESTORE */

  /* initialize WINTERP modules */
  Wso_Init();			/* w_savedobjs.c */
  Wres_Init();			/* w_resources.c */
  Wxms_Init();			/* w_XmString.c */
  Wcb_Init();			/* w_callbacks.c */
  Wto_Init();			/* w_timeouts.c */
  Wicb_Init();			/* w_inputCB.c */
  Wtx_Init();			/* w_txlations.c */
  Weh_Init();			/* w_evnthndlr.c */
  Wxm_Init();			/* w_libXm.c */
  Wutils_Init();		/* utils.c */

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

#ifdef WINTERP_TABLE_WIDGET
  Wc_Table_Init();
#endif /* WINTERP_TABLE_WIDGET */

#ifdef WINTERP_TREE_WIDGET
  Wc_Tree_Init();
#endif /* WINTERP_TREE_WIDGET */

#ifdef WINTERP_XTANGO_WIDGET
  Tic_IMAGE_Init();  /* -- must occur before other Tic_*_Init() --  */
  Tic_Bitmap_Init();
  Tic_Circle_Init();
  Tic_Composite_Init();
  Tic_Ellipse_Init();
  /* Tic_IMAGE_Init();  -- must occur before other Tic_*_Init() --  */
  Tic_GIF_Init();
  Tic_Line_Init();
  Tic_Polygon_Init();
  Tic_Polyline_Init();
  Tic_Rect_Init();
  Tic_Spline_Init();
  Tic_Text_Init();
  Wc_Xtango_Init();
  T_Utils_Init();
#endif /* WINTERP_XTANGO_WIDGET */

#ifdef HP_GRAPH_WIDGET
  Wc_XmGraph_Init();
#endif /* HP_GRAPH_WIDGET */

#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.X and IndigoMagic desktop */
  Wc_DropPocket_Init();
#endif /* SGI_DROP_POCKET_WIDGET */

  {
    LVAL sym;

    /*
     * Make the toplevel_Wgt accessible from lisp as global *TOPLEVEL_WIDGET*.
     * This code must occur after calling Wc_SHELL_Init(), and preferably after
     * every WINTERP widget class initializer is called.
     */
    sym = xlenter("*TOPLEVEL_WIDGET*");
    defconstant(sym, Wshl_WidgetID_To_WIDGETOBJ(toplevel_Wgt));

    /*
     * Make XLISP, WINTERP, and MOTIF version info available within interpreter.
     */
    sym = xlenter("*XLISP_VERSION*");
    defconstant(sym, cvfixnum((FIXTYPE) XLISP_VERSION_INT)); /* XLISP_VERSION_INT from xlisp/xlisp.h */
    sym = xlenter("*XLISP_REVISION*");
    defconstant(sym, cvfixnum((FIXTYPE) XLISP_REVISION_INT)); /* XLISP_REVISION_INT from xlisp/xlisp.h */
    sym = xlenter("*MOTIF_VERSION*");
    defconstant(sym, cvfixnum((FIXTYPE) XmVERSION)); /* XmVERSION from <Xm/Xm.h> */
    sym = xlenter("*MOTIF_REVISION*");
    defconstant(sym, cvfixnum((FIXTYPE) XmREVISION)); /* XmREVISION from <Xm/Xm.h> */

    /* stupid kludge allowing workarounds to bugs in various versions of 1.1 */
    sym = xlenter("*MOTIF_SUBREVISION*");
#if (defined(WINTERP_MOTIF_113) && !defined(WINTERP_MOTIF_12))	/* also valid for Motif 1.1.4, but hey, this is lame, right? */
    defconstant(sym, cvfixnum((FIXTYPE) 3));
#else				/* !defined(WINTERP_MOTIF_113) */
#if (defined(WINTERP_MOTIF_111) && !defined(WINTERP_MOTIF_12))
    defconstant(sym, cvfixnum((FIXTYPE) 1));
#else				/* !defined(WINTERP_MOTIF_111) --> Plain old 1.0 or 1.1 */
#ifdef WINTERP_MOTIF_12
    defconstant(sym, cvfixnum((FIXTYPE) XmUPDATE_LEVEL));
#else
    defconstant(sym, cvfixnum((FIXTYPE) 0));
#endif /* WINTERP_MOTIF_12 */
#endif /* WINTERP_MOTIF_111 */
#endif /* WINTERP_MOTIF_113 */

    sym = xlenter("*WINTERP_VERSION*");
    defconstant(sym, cvfixnum((FIXTYPE) WINTERP_VERSION_INT)); /* WINTERP_VERSION_INT from winterp.h */
    sym = xlenter("*WINTERP_REVISION*");
    defconstant(sym, cvfixnum((FIXTYPE) WINTERP_REVISION_INT));	/* WINTERP_REVISION_INT from winterp.h  */
  }

  /* reset the error handler, since we know what "true" is */
  xlend(&cntxt);
  xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, true);

  /* open the transcript file */
  if ((user_prefs.lisp_transcript_file != NULL) &&
      ((tfp = OSAOPEN(user_prefs.lisp_transcript_file, CREATE_WR)) == CLOSED)) {
      (void) sprintf(temptext, "error: can't open transcript file: \"%s\"",
		     user_prefs.lisp_transcript_file);
      stdputstr(temptext);
    }

  /*
   * Load user initialization file ~/.winterp, if it's not there, give warning,
   * load default system initialization file "lib-utils/initialize.lsp" (in
   * "winterp/examples/" directory -- if it's not there give another warning.
   * If SAVERESTORE enabled, and a saved image has already been restored,
   * then don't bother loading the initialization files.
   */
  if (image_not_restored_p && (setjmp(cntxt.c_jmpbuf) == 0)) {
    strcpy(temptext, getenv("HOME"));
    strcat(temptext, "/.winterp");
    if (!xlload(temptext, user_prefs.enable_init_msgs, FALSE)) {
      stdputstr("WINTERP warning -- couldn't load initialization file: \"");
      stdputstr(temptext);
      stdputstr("\".\n");
      if (!xlload("lib-utils/initialize.lsp", user_prefs.enable_init_msgs, FALSE)) {
	stdputstr("WINTERP warning -- couldn't load default initialization file: \"lib-utils/initialize.lsp\".\n\t(Did you forget to set resource 'lispLibDir' or 'lispLoadPath'?)\n");
      }
    }
  }

  /*
   * Load application file specified by resource "lispInitFile";
   * If set to NULL (default), then attempt to load from ~/.winterpapp.
   */
  if ((user_prefs.lisp_init_file == NULL) || (*(user_prefs.lisp_init_file) == '\0')) {
    strcpy(temptext, getenv("HOME"));
    strcat(temptext, "/.winterpapp");
    user_prefs.lisp_init_file = XtNewString(temptext);
  }
  if (setjmp(cntxt.c_jmpbuf) == 0) {
    if (!xlload(user_prefs.lisp_init_file, user_prefs.enable_init_msgs, FALSE)) {
      (void) sprintf(temptext,
		     "WINTERP warning -- couldn't load application file: \"%s\"\n\t\
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
#endif /* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
    if (user_prefs.enable_AF_UNIX_server)
      (void) printf("\nXLisp Unix Domain eval-server ready for input on socket %s .\n",
		    user_prefs.unix_socket_filepath);
#endif /* WINTERP_WANT_UNIX_SERVER */

#if (defined(WINTERP_WANT_INET_SERVER) && !defined(WINTERP_WANT_UNIX_SERVER))
    if (user_prefs.enable_AF_INET_server
#ifdef WINTERP_WANT_STDIN_SERVER
        && !user_prefs.enable_STDIN_server
#endif /* WINTERP_WANT_STDIN_SERVER */
	)
      (void) printf("Note: INPUT TO XLISP EVALUATOR CANNOT BE ENTERED HERE!! (see winterp.doc)\n");
#endif /* (defined(WINTERP_WANT_INET_SERVER) && !defined(WINTERP_WANT_UNIX_SERVER)) */

#if (!defined(WINTERP_WANT_INET_SERVER) && defined(WINTERP_WANT_UNIX_SERVER))
    if (user_prefs.enable_AF_UNIX_server
#ifdef WINTERP_WANT_STDIN_SERVER
        && !user_prefs.enable_STDIN_server
#endif /* WINTERP_WANT_STDIN_SERVER */
	)
      (void) printf("Note: INPUT TO XLISP EVALUATOR CANNOT BE ENTERED HERE!! (see winterp.doc)\n");
#endif /* (!defined(WINTERP_WANT_INET_SERVER) && defined(WINTERP_WANT_UNIX_SERVER)) */

#if (defined(WINTERP_WANT_INET_SERVER) && defined(WINTERP_WANT_UNIX_SERVER))
    if ((user_prefs.enable_AF_INET_server || user_prefs.enable_AF_UNIX_server)
#ifdef WINTERP_WANT_STDIN_SERVER
        && !user_prefs.enable_STDIN_server
#endif /* WINTERP_WANT_STDIN_SERVER */
	)
      (void) printf("Note: INPUT TO XLISP EVALUATOR CANNOT BE ENTERED HERE!! (see winterp.doc)\n");
#endif /* (defined(WINTERP_WANT_INET_SERVER) && defined(WINTERP_WANT_UNIX_SERVER)) */

#ifdef WINTERP_WANT_STDIN_SERVER
    if (user_prefs.enable_STDIN_server)
      (void) printf("\nXLISP EVALUATOR WAITING FOR INPUT ON TERMINAL (STDIN)!\n");
#endif /* WINTERP_WANT_STDIN_SERVER */

    (void) printf("================================================================================\n");

  }

#ifdef WINTERP_WANT_STDIN_SERVER
  if (user_prefs.enable_STDIN_server) {
    s_stdin_ustream = xlenter("*STDIN-USTREAM*");
    setvalue(s_stdin_ustream, NIL); 
  }
#endif /* WINTERP_WANT_STDIN_SERVER */

#ifdef SAVERESTORE
  /* setup longjmp target for restore */
  if (setjmp(top_level))
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, true);
#endif /* SAVERESTORE */

  lisp_reader_hit_eof = FALSE;
  winterp_caught_signal = FALSE;

  while (lisp_reader_hit_eof == FALSE) { /* !!! */
    Winterp_Print_Prompt(FALSE); /* print initial "prompt" ... */

    if (setjmp(cntxt.c_jmpbuf) == 0)
      XtAppMainLoop(app_context); /* Process X Events and Lisp client requests forever */

    /*
     * XtAppMainLoop() call above will exit upon
     * (1) closing an application-shell widget,
     * (2) Calling Lisp primitive '(exit)',
     * (3) xlbrklevel() caused by EOF in Read_Eval_Print()
     *
     * Note that the while() loop above will restart XtAppMainLoop()
     * when a global Xt or Xlib error occurs thereby calling 
     * Winterp_Xlib_Error_Handler() or Winterp_Xtoolkit_Error_Handler().
     * When either user_prefs.{enable_xerr_brk,enable_xterr_brk} are TRUE, an
     * xlisp error will get signalled which will longjmp() out of XtAppMainLoop()
     * via xlsignal() signalling CF_ERROR...
     *
     * Non-reentrancy problems may arise in doing so, which is one of the reasons
     * why the options user_prefs.{enable_xerr_brk,enable_xterr_brk} exist...
     */
  }

  xlend(&cntxt);
  wrapup();
}


#ifdef WINTERP_WANT_INET_SERVER
/******************************************************************************
 * initialize AF_INET server, returning a socket that can be listened on.
 ******************************************************************************/
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
  else if (user_prefs.service_port != 0)
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
  if (bind(ls, (struct sockaddr *) &myaddr_in, sizeof(struct sockaddr_in)) == -1) {
    perror(app_name);
    xlfatal("Unable to bind() INET Domain Socket.");
  }

  /* Initiate the listen on the socket so remote users
   * can connect.  The listen backlog is set to 5, which
   * is the largest currently supported.
   */
  if (listen(ls, 5) == -1) {
    perror(app_name);
    xlfatal("Unable to listen() on INET Domain Socket.");
  }
  
  fcntl(ls, F_SETFD, 1);	/* set close-on-exec for the client listener socket */
  
  return (ls);
}
#endif /* WINTERP_WANT_INET_SERVER */


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
#endif /* SOCKADDR_UN_MAXLEN */
  if (strlen(user_prefs.unix_socket_filepath) > (SOCKADDR_UN_MAXLEN - 1)) {
    (void) sprintf(temptext, "socket path %s must be shorter than %d bytes.",
		   user_prefs.unix_socket_filepath,
		   SOCKADDR_UN_MAXLEN - 1);
    xlfatal(temptext);
  }
  else
    strcpy(myaddr_un.sun_path, user_prefs.unix_socket_filepath);
  
  /*
   * Create the listen socket.
   */
  if ((ls = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
    perror(app_name);
    (void) sprintf(temptext, "socket() failed to create Unix Domain socket '%s'.",
		   user_prefs.unix_socket_filepath);
    xlfatal(temptext);
  }

  /*
   * Bind the listen address to the socket.
   */
  if (bind(ls, (struct sockaddr *) &myaddr_un, sizeof(myaddr_un.sun_family) + strlen(myaddr_un.sun_path)) == -1) {
    perror(app_name);
    (void) sprintf(temptext,
		   "Unable to bind() Unix Domain socket \"%s\".\n\t\
Note: you may need to do \"rm %s\" if a previous\n\t\
'%s' terminated incorrectly. Alternately, another\n\t\
invocation of '%s' may be running, in which case you need\n\t\
to specify a different UnixDomain Socket file by setting\n\t\
environment variable '%s', or by setting\n\t\
resource '%s.unixSocketFilepath'.",
		   user_prefs.unix_socket_filepath,
		   user_prefs.unix_socket_filepath,
		   app_name,
		   app_name,
		   DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR,
		   app_name);
    xlfatal(temptext);
  }

  /* Only allow the user to send WINTERP commands over unix domain socket */
  chmod(user_prefs.unix_socket_filepath, 0700);

  /*
   * Initiate the listen on the socket so remote users
   * can connect.  The listen backlog is set to 5, which
   * is the largest currently supported.
   */
  if (listen(ls, 5) == -1) {
    perror(app_name);
    (void) sprintf(temptext, "Unable to listen() on Unix Domain socket '%s'.",
		   user_prefs.unix_socket_filepath);
    xlfatal(temptext);
  }

  fcntl(ls, F_SETFD, 1);	/* set close-on-exec for the client listener socket */

  return (ls);
}
#endif /* WINTERP_WANT_UNIX_SERVER */


#ifdef WINTERP_WANT_INET_SERVER
/******************************************************************************
 * Accept the request on client_AF_INET_listen_socket, and open a socket for
 * reading, rdsock. rdsock will be closed by Read_Eval_Print().
 ******************************************************************************/
static int Accept_AF_INET_Server_Request(client_listen_socket)
     int client_listen_socket;
{ 
  int rdsock;
  int addrlen = sizeof(struct sockaddr_in);
  struct sockaddr_in peeraddr_in; /* for peer socket address */
#ifdef WINTERP_BSD_4_2
  long lingerOpt = 1L;		/* NOTE: necessary while hpux-version < 8.0 (???) */
#else /* !defined(WINTERP_BSD_4_2) --> 4.3bsd networking */
  struct linger lingerOpt;
  lingerOpt.l_onoff  = 1;
  lingerOpt.l_linger = 10000;
#endif /* WINTERP_BSD_4_2 */

  memset((char *)&peeraddr_in, 0, sizeof(struct sockaddr_in));
  if ((rdsock = accept(client_listen_socket, (struct sockaddr *) &peeraddr_in, &addrlen)) == -1) {
    perror(app_name);
    xlfatal("Unable to accept() on INET Domain Socket."); /* CLEANUP & EXIT */
  }
  if (setsockopt(rdsock, SOL_SOCKET, SO_LINGER, (char *) &lingerOpt,
#ifdef WINTERP_BSD_4_2
		 sizeof(long)	/* NOTE: necessary while hpux-version < 8.0 (???) */
#else /* !defined(WINTERP_BSD_4_2) --> 4.3bsd networking */
		 sizeof(struct linger)
#endif /* WINTERP_BSD_4_2 */
		 ) == -1) {
    perror(app_name);
    xlfatal("Unable to setsockopt() on INET Domain Socket."); /* CLEANUP & EXIT */
  }

  fcntl(rdsock, F_SETFD, 1);	/* set close-on-exec for the client read socket */

  return (rdsock);
}
#endif /* WINTERP_WANT_INET_SERVER */


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

  if ((rdsock = accept(client_listen_socket, (struct sockaddr *) &peeraddr_un, &addrlen)) == -1) {
    perror(app_name);
    xlfatal("Unable to accept() on Unix Domain socket."); /* cleanup and exit */
  }

  fcntl(rdsock, F_SETFD, 1);	/* set close-on-exec for the client read socket */

  return (rdsock);
}
#endif /* WINTERP_WANT_UNIX_SERVER */


#ifdef WINTERP_WANT_INET_SERVER
/******************************************************************************
 * This procedure is called (indirectly, via XtAppAddInput() callback) from 
 * XtAppNextEvent() in main() and from XtAppNextEvent() in 
 * xldbug.c:breakloop(). This callback will be called whenever new input 
 * appears on client_AF_INET_listen_socket indicating that a new connection has been 
 * requested and that another s-expression is ready to be evaluated by Xlisp. 
 * This procedure will accept that connection and read all the data from the 
 * client and send it off to the XLisp reader, and the Xlisp evaluator. 
 * The results of the evaluation are printed.
 ******************************************************************************/
static void AF_INET_Read_Eval_Print(client_data, source_fildes, id)
     XtPointer  client_data;	/* not used */
     int*       source_fildes;	/* not used */
     XtInputId* id;		/* not used */
{
  Read_Eval_Print(Accept_AF_INET_Server_Request(client_AF_INET_listen_socket));
}
#endif /* WINTERP_WANT_INET_SERVER */

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
     XtPointer  client_data;	/* not used */
     int*       source_fildes;	/* not used */
     XtInputId* id;		/* not used */
{
  Read_Eval_Print(Accept_AF_UNIX_Server_Request(client_AF_UNIX_listen_socket));
}
#endif /* WINTERP_WANT_UNIX_SERVER */


/******************************************************************************
 * Used by Read_Eval_Print() below.
 ******************************************************************************/
static void Server_Send_Reply(rdsock, str, len)
     int rdsock;
     char* str;
     int len;
{
  if (send(rdsock, str, len, 0) < 0) {
    perror(app_name);
    if (winterp_caught_signal == SIGPIPE) /* ignore SIGPIPE -- see Winterp_Signal_Handler() */
      winterp_caught_signal = FALSE;
    else
      xlfatal("Unable to send() server reply on read socket."); /* CLEANUP & EXIT */
  }
  close(rdsock);
}


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
  extern LVAL s_1star;
  static char rdbuf[BUFSIZ];
  int len, i, read_result;
  LVAL sexp_stream, new_elt, last_elt = NIL;
  int result_len;
  char* result_str;
  Boolean not_read_special_eof_p;

  /*
   * Read the sexpression from the socket -- note assumption that entire
   * sexpression is sent in one "packet" and then the socket is closed.
   */

  xlsave1(sexp_stream);		/* protect from gc */
  sexp_stream = newustream();	/* note - stream obj has ptrs for head and tail*/

  not_read_special_eof_p = TRUE;
  while (not_read_special_eof_p &&
	 ((len = recv(rdsock, rdbuf, BUFSIZ, 0)) > 0)) { /* read len characters into rdbuf */
    /* !!!hack!!! --
       'wl's final send prior to shutdown() is 'Server_Send(s, WINTERP_CLIENT_SPECIAL_EOF_STRING)'
       -- see ../src-client/wl.c:Server_Disconnect() and ./config.h */
    if (rdbuf[len - 1] == WINTERP_CLIENT_SPECIAL_EOF_CHAR) {
      --len;			/* ignore the WINTERP_CLIENT_SPECIAL_EOF_CHAR */
      rdbuf[len] = '\000';	/* replace WINTERP_CLIENT_SPECIAL_EOF_CHAR with a NULL,  */
      not_read_special_eof_p = FALSE;
    }

    /* foreach character received, stuff it into an xlisp unnamed stream */
    for (i = 0; i < len; i++) {
      new_elt = cons(cvchar(rdbuf[i]), NIL);
      if (last_elt != NIL) {	/* if we've already created the head of the stream */
	rplacd(last_elt, new_elt); /* add new_elt to the tail of the list */
	last_elt = new_elt;	/* increment last_elt pointer */
      }
      else {			/* else create the head of the stream */
	sethead(sexp_stream, new_elt);
	last_elt = new_elt;
      }
    }
  }

  /* check for error condition in recv() from Winterp-Lisp client*/
  if (len < 0) {
    perror(app_name);
    if (winterp_caught_signal == SIGPIPE) { /* ignore SIGPIPE -- see Winterp_Signal_Handler() */
      winterp_caught_signal = FALSE;
      lisp_reader_hit_eof = FALSE;
      close(rdsock);		/* but since we got a SIGPIPE socket is history */
      xlpop();			/* !!! -- sexp_stream */
      return;			/* !!! -- error abort... */
    }
    else
      xlfatal("Unable to recv() on read socket."); /* CLEANUP & EXIT */
  }

  if (last_elt != NIL)
    settail(sexp_stream, last_elt); /* streams are cdr-coded -- give ptr to tail */
  else			
    sexp_stream = NIL;		/* loop never executed, no characters read. */

  read_result = Read_From_Stream_Eval_And_Print(sexp_stream); /* returns FALSE if hit EOF,
								 TRUE if success,
								 or > 1 if error */
  xlpop();			/* sexp_stream */

  /* When evaluation finished, send results back to the client 'wl' or
     'wl-tcpip'. The 'wl' client will return an exit status of 0 on successful
     evaluation, an exit status of 128 will be returned if an evaluation or read
     error occured Read_From_Stream_Eval_And_Print(); an exit status of 1 indicates
     that some other error has occured (e.g. system or network error). */

  if (read_result == FALSE)  {	/* FALSE ==> hit EOF */
    Server_Send_Reply(rdsock, "EOF", 3);
    lisp_reader_hit_eof = TRUE; /* !!! */
    xlbrklevel();		/* !!! longjmp around XtAppMainLoop() and  exit */
  }
  else if (read_result == TRUE) { /* TRUE ==> evaluation ok */
    /* latest evaluator output is in "getvalue(s_1star)"
       retrieve s-expr value as string... */
    result_str = Wut_Sexp_To_String(getvalue(s_1star), &result_len);
    Server_Send_Reply(rdsock, result_str, result_len);
    XtFree(result_str);
    lisp_reader_hit_eof = FALSE;
  }
  else {			/* >1 ==> evaluation error */
    Server_Send_Reply(rdsock, "", 0); /* 0-length string signals eval error to 'wl' client */
    lisp_reader_hit_eof = FALSE;
  }
}


#ifdef WINTERP_WANT_STDIN_SERVER

static void STDIN_Protect_Sexp_Proc(ustream)
     LVAL ustream;
{
  setvalue(s_stdin_ustream, ustream);	/* protect it from GC across calls to STDIN_Read_Eval_Print() -- xlsave/xlpop won't work...*/
}

static void STDIN_Eval_Sexp_Proc(ustream)
     LVAL ustream;
{
  int read_result;

  read_result = Read_From_Stream_Eval_And_Print(ustream);	
  setvalue(s_stdin_ustream, NIL);

#if 0
  /*
   * Disabled by NPM because :
   * (1) this doesn't work anyways -- stdin reader doesn't notice ^D's
   * (2) this code caused inputs of form #| ... |# at the top-level of
   *     parenthisation to generate an EOF in the reader, which would
   *     exit WINTERP. COmmenting this out prevents this problem.
   */
  if (read_result == FALSE) {
    lisp_reader_hit_eof = TRUE;	/* !!! */
    xlbrklevel();		/* !!! longjmp around XtAppMainLoop() and  exit */
  }
#endif /* #if 0 */
}

/******************************************************************************
 * 
 ******************************************************************************/
static void STDIN_Read_Eval_Print(client_data, source_fildes, id)
     XtPointer  client_data;	/* not used */
     int*       source_fildes;
     XtInputId* id;
{
  int len;
  LVAL sexp_stream;
  static char rdbuf[BUFSIZ];
  static LVAL last_elt;
  static int paren_count, comment_count, read_state;

  sexp_stream = getvalue(s_stdin_ustream);

  len = read(*source_fildes, rdbuf, BUFSIZ); /* read len characters into rdbuf */
  if (len < 0) {
    perror(app_name);
    xlfatal("Unable to read() from stdin."); /* CLEANUP & EXIT */
  }
  if (len == 0) {
    (void) fprintf(stderr, "%s warning: stdin reader hit EOF -- disabling stdin reader.\n", app_name);
    fflush(stderr);
    XtRemoveInput(*id);
    setvalue(s_stdin_ustream, NIL);
  }
  else {
#if 0 /* COMMENTOUT */
    rdbuf[len] = '\000';
    fprintf(stdout, "stdin callback = '%s'\n", rdbuf);
    fflush(stdout);		/* otherwise output won't happen while blocked in XtAppNextEvent() */
#endif /* COMMENTOUT */
    Wicb_Read_Sexp_Proc(NULL,
			rdbuf, len,
			&paren_count, &comment_count,
			&read_state,
			&sexp_stream, &last_elt,
			STDIN_Protect_Sexp_Proc,
			STDIN_Eval_Sexp_Proc);
  }
}
#endif /* WINTERP_WANT_STDIN_SERVER */


/*******************************************************************************
 * This fn reads from its input, which is assumed to be a xlisp stream.
 * returns false if EOF hit during read.
 ******************************************************************************/
int Read_From_Stream_Eval_And_Print(sexp_stream)
     LVAL sexp_stream;		/* make sure this is a stream, and not other LVAL */
{
  extern LVAL true;		/* from xlisp/xlglob.c */
  extern LVAL xlenv, xlfenv, xldenv;
  CONTEXT cntxt;
  LVAL rep_expr;
  int read_result;
  LVAL		oldenv, oldfenv;
#ifdef SPECIALS
  LVAL		olddenv=xldenv;
#endif /* SPECIALS */

  /*
   * Reset winterp_caught_signal so that winterp.c:oscheck() doesn't trigger
   * off of a ^C typed in to terminal prior to inputting the current s-exp.
   * In other words, ^C should trigger an abort only once an XLISP evaluation
   * has started. See also w_txlations.c:Wtx_Winterp_Lisp_Action_Proc(), and
   * w_callbacks.c:Wcb_Meta_Callbackproc().
   */
  winterp_caught_signal = FALSE;
  lisp_reader_hit_eof = FALSE;

  xlstkcheck(4);
  xlprotect(sexp_stream);	/* protect against GC */
  xlsave(rep_expr);
  xlsave(oldenv);
  xlsave(oldfenv);

  /* establish a global environment -- stolen from xlsys.c:xload(). */
  oldenv = xlenv;
  oldfenv = xlfenv;
  xlenv = xlfenv = NIL;

  xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, true);

  if ((read_result = setjmp(cntxt.c_jmpbuf)) != 0) { /* errors in read or eval get caught here */
    extern LVAL s_evalhook, s_applyhook; /* from xlisp/xlglob.c */
    extern int xltrcindent, xldebug; /* from xlisp/xlglob.c */
    setvalue(s_evalhook, NIL);
    setvalue(s_applyhook, NIL);
    xltrcindent = 0;
    xldebug = 0;
    Winterp_Print_Prompt(TRUE);	/* xlflush() must occur after this in order to get new-line (as spec'd by arg 'TRUE') to occur */
    xlflush();			/* needed if using (read)/(read-line) from stdin */ 
  }
  else {
    /* Read Evaluate and Print the expression in sexp_stream */
    if ((read_result = xlread(sexp_stream, &rep_expr))) {

      /* save the last expression returned by the reader */
      xlrdsave(rep_expr);	/* from "xlisp/xlisp.h", def'd in utils.c (xlisp.c not in libXlisp.a) */

      /* evaluate the expression returned by the reader */
      rep_expr = xleval(rep_expr);

      /* save the last expression returned by the evaluator */
      xlevsave(rep_expr);	/* from "xlisp/xlisp.h", def'd in utils.c  (xlisp.c not in libXlisp.a) */

      stdprint(rep_expr);

      Winterp_Print_Prompt(FALSE);      
    }
    else			/* else reader hit EOF, so just print a new line */
      Winterp_Print_Newline();
  }

  xlend(&cntxt);

  /* from xleval.c:evfun(): restore the environment */
  xlenv = oldenv;
  xlfenv = oldfenv;
#ifdef SPECIALS
  xlunbind(olddenv);
#endif /* SPECIALS */

  xlpopn(4);			/* restore the stack */

  return (read_result);		/* return FALSE if hit EOF, TRUE if success,
				   and <mask> value (logior of CF_*'s) from setjmp */
}


/*******************************************************************************
 * xlfatal - print a fatal error message and exit.  originally from xlisp.c.
 ******************************************************************************/
void xlfatal(msg)
  char *msg;
{
  extern FILE *tfp;

  (void) fprintf(stderr, "%s -- error: %s\n", app_name, msg);
  fflush(stderr);

#ifdef WINTERP_WANT_INET_SERVER
  if (client_AF_INET_listen_socket)
    close(client_AF_INET_listen_socket);
#endif /* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
  if (client_AF_UNIX_listen_socket) {
    close(client_AF_UNIX_listen_socket);
    unlink(user_prefs.unix_socket_filepath);
  }
#endif /* WINTERP_WANT_UNIX_SERVER */

  if (tfp != CLOSED)
    OSCLOSE(tfp);

#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT /* Commented out because XtDestroyApplicationContext() crashes in a weird way on s300 HPUX 8.0, and this call isn't really needed since we're just going to exit */
  if (app_context)
    XtDestroyApplicationContext(app_context);
#endif /* THE_FOLLOWING_CODE_IS_COMMENTED_OUT */

  if (winterp_caught_signal != FALSE)
    exit(winterp_caught_signal);
  else
    exit(128);
}


/*******************************************************************************
 * wrapup - clean up and exit to the operating system. originally from xlisp.c.
 * This is also called in xlsys.c:xexit().
 ******************************************************************************/
void wrapup()
{
#ifdef WINTERP_EMBEDDED
  extern int winterp_embedded_app_wrapup();
#endif /* WINTERP_EMBEDDED */
  extern FILE *tfp;

  if (
#ifdef WINTERP_EMBEDDED
      winterp_embedded_app_wrapup() == 0
#else /* standard winterp */
      TRUE
#endif /* WINTERP_EMBEDDED */
      ) {
    Winterp_Print_Newline();

#ifdef WINTERP_WANT_INET_SERVER
    if (client_AF_INET_listen_socket)
      close(client_AF_INET_listen_socket);
#endif /* WINTERP_WANT_INET_SERVER */

#ifdef WINTERP_WANT_UNIX_SERVER
    if (client_AF_UNIX_listen_socket) {
      close(client_AF_UNIX_listen_socket);
      unlink(user_prefs.unix_socket_filepath);
    }
#endif /* WINTERP_WANT_UNIX_SERVER */

    if (tfp != CLOSED)
      OSCLOSE(tfp);

#ifdef THE_FOLLOWING_CODE_IS_COMMENTED_OUT /* Commented out because XtDestroyApplicationContext() crashes in a weird way on s300 HPUX 8.0, and this call isn't really needed since we're just going to exit */
    if (app_context)
      XtDestroyApplicationContext(app_context);
#endif /* THE_FOLLOWING_CODE_IS_COMMENTED_OUT */

    exit(0);
  }
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
 * docs, such a handler should terminate the application. When 
 * user_prefs.enable_XtError_break is TRUE however, we suggest to the user
 * that the application be terminated, but don't actually do it. This may
 * allow the user to figure out what went wrong by poking around inside the lisp
 * environment. When user_prefs.enable_XtError_break is FALSE, we force a cleanup
 * and do an exit(1).
 *
 * This is set up in main() via XtAppSetErrorHandler(). Note that the default
 * error handler is _XtDefaultError().
 ******************************************************************************/
static void Winterp_Xtoolkit_Error_Handler(message)
     String message;
{
  char msgbuf[2*BUFSIZ];

  if (user_prefs.enable_XtError_break) {
    (void) sprintf(msgbuf,
		   "X Toolkit Fatal Error -- PLEASE QUIT AND RESTART THIS APPLICATION:\n    %s\n",
		   message);
    xlfail(msgbuf);
  }
  else {
    (void) sprintf(msgbuf,
		   "X Toolkit Fatal Error -- Application terminated due to:\n    %s\n",
		   message);
    xlfatal(msgbuf);
  }
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
  char msgbuf[2*BUFSIZ];

  (void) sprintf(msgbuf,
		 "X Toolkit Warning:\n    %s\n",
		 message);
  xlfail(msgbuf);
}


/*******************************************************************************
 * The following code is from X11r4:mit/lib/X/XlibInt.c.
 * Copyright    Massachusetts Institute of Technology    1985, 1986, 1987.
 ******************************************************************************/
static int Winterp_XPrintDefaultError(dpy, event, fp)
    Display *dpy;
    XErrorEvent *event;
    FILE *fp;
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "XlibMessage";
    register _XExtension *ext = (_XExtension *)NULL;
#ifdef WINTERP_MOTIF_12
    _XExtension *bext = (_XExtension *)NULL;
#endif /* WINTERP_MOTIF_12 */    
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
#ifdef WINTERP_MOTIF_12
    (void) fprintf(fp, " (%s)\n", buffer);
    if (event->request_code >= 128) {
	XGetErrorDatabaseText(dpy, mtype, "MinorCode", "Request Minor code %d",
			      mesg, BUFSIZ);
	fputs("  ", fp);
	(void) fprintf(fp, mesg, event->minor_code);
	if (ext) {
	    sprintf(mesg, "%s.%d", ext->name, event->minor_code);
	    XGetErrorDatabaseText(dpy, "XRequest", mesg, "", buffer, BUFSIZ);
	    (void) fprintf(fp, " (%s)", buffer);
	}
	fputs("\n", fp);
    }
    if (event->error_code >= 128) {
	/* kludge, try to find the extension that caused it */
	buffer[0] = '\0';
	for (ext = dpy->ext_procs; ext; ext = ext->next) {
	    if (ext->error_string) 
		(*ext->error_string)(dpy, event->error_code, &ext->codes,
				     buffer, BUFSIZ);
	    if (buffer[0]) {
		bext = ext;
		break;
	    }
	    if (ext->codes.first_error &&
		ext->codes.first_error < event->error_code &&
		(!bext || ext->codes.first_error > bext->codes.first_error))
		bext = ext;
	}    
	if (bext)
	    sprintf(buffer, "%s.%d", bext->name,
		    event->error_code - bext->codes.first_error);
	else
	    strcpy(buffer, "Value");
	XGetErrorDatabaseText(dpy, mtype, buffer, "", mesg, BUFSIZ);
	if (mesg[0]) {
	    fputs("  ", fp);
	    (void) fprintf(fp, mesg, event->resourceid);
	    fputs("\n", fp);
	}
	/* let extensions try to print the values */
	for (ext = dpy->ext_procs; ext; ext = ext->next) {
	    if (ext->error_values)
		(*ext->error_values)(dpy, event, fp);
	}
    } else if ((event->error_code == BadWindow) ||
	       (event->error_code == BadPixmap) ||
	       (event->error_code == BadCursor) ||
	       (event->error_code == BadFont) ||
	       (event->error_code == BadDrawable) ||
	       (event->error_code == BadColor) ||
	       (event->error_code == BadGC) ||
	       (event->error_code == BadIDChoice) ||
	       (event->error_code == BadValue) ||
	       (event->error_code == BadAtom)) {
	if (event->error_code == BadValue)
	    XGetErrorDatabaseText(dpy, mtype, "Value", "Value 0x%x",
				  mesg, BUFSIZ);
	else if (event->error_code == BadAtom)
	    XGetErrorDatabaseText(dpy, mtype, "AtomID", "AtomID 0x%x",
				  mesg, BUFSIZ);
	else
	    XGetErrorDatabaseText(dpy, mtype, "ResourceID", "ResourceID 0x%x",
				  mesg, BUFSIZ);
	fputs("  ", fp);
	(void) fprintf(fp, mesg, event->resourceid);
	fputs("\n", fp);
    }
    XGetErrorDatabaseText(dpy, mtype, "ErrorSerial", "Error Serial #%d", 
			  mesg, BUFSIZ);
    fputs("  ", fp);
    (void) fprintf(fp, mesg, event->serial);
    XGetErrorDatabaseText(dpy, mtype, "CurrentSerial", "Current Serial #%d",
			  mesg, BUFSIZ);
    fputs("\n  ", fp);
    (void) fprintf(fp, mesg, dpy->request);
    fputs("\n", fp);
    if (event->error_code == BadImplementation) return 0;
    return 1;
#else /* X11r4 and Motif 1.1 or 1.0 */
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
#endif /* WINTERP_MOTIF_12 */
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
  if (user_prefs.enable_XError_break) {
    (void) Winterp_XPrintDefaultError(dpy, event, stderr);
    xlfail("Xlib error detected.");
  }
  else {
    (void) Winterp_XPrintDefaultError(dpy, event, stderr);
    xlfatal("Xlib error detected -- Application terminated (see above).");
  }
  return (0);			/* doesn't really return since xlfail() longjmp()s out and xlfatal() exit()s */
}


/*******************************************************************************
 * Attempts to Handle interrupts gracefully... despite X and Motif.
 * Basically, just set a flag indicating a signal was received, then
 * wait till oscheck() below gets called from the XLISP evaluator.
 ******************************************************************************/
static void Winterp_Signal_Handler(sig)
     int sig;
{
  winterp_caught_signal = sig;
  switch (sig) {

#ifdef WINTERP_HANDLE_NASTY_SIGNALS
  case SIGABRT:
    xlfatal("Caught SIGABRT -- abnormal termination signal.");
    break;
  case SIGILL:
    xlfatal("Caught SIGILL -- illegal instruction.");
    break;
  case SIGSEGV:
    xlfatal("Caught SIGSEGV -- invalid memory reference.");
    break;
  case SIGQUIT:
    xlfatal("Caught SIGQUIT -- quit signal.");
    break;
#endif /* WINTERP_HANDLE_NASTY_SIGNALS */

  case SIGHUP:
    xlfatal("Caught SIGHUP -- controlling terminal or process group terminated.");
    break;
  case SIGTERM:
    xlfatal("Caught SIGTERM -- program termination signal.");
    break;
  case SIGINT:
  case SIGFPE:
  case SIGPIPE:
    /* other than setting winterp_caught_signal above, we basically ignore
       these symbols until evaluator calls oscheck() */
    signal(sig, Winterp_Signal_Handler);
    break;

  default:
    /* any other signals caught by this handler are a programmer error */
    (void) sprintf(temptext, "Caught unexpected signal -- '%d'.", sig);
    xlfatal(temptext);
  }
}


/*******************************************************************************
 * oscheck -- called from xlisp/xleval.c:xleval() and xlisp/xleval.c:evfun(),
 * this routine gets called approximately once per hundred XLISP evaluations
 * (see xlisp.h:#define SAMPLE) to check whether any asynchronous signals
 * have occured, as set by Winterp_Signal_Handler() above.
 * If a fatal error has occured, this causes xlfatal() to get called,
 * printing a message and exiting WINTERP cleanly (e.g. cleans up unix domain
 * socket). If the user interrupted the system with a SIGINT
 * then the current evaluation is aborted and the system returns to the
 * "top level" of the evaluator. If a floating point exception occurs, then
 * a recoverable error is signalled.
 ******************************************************************************/
void oscheck()
{
  if (winterp_caught_signal != FALSE) {
    switch (winterp_caught_signal) {
    case SIGINT:		/* interrupt special char typed on controlling tty --> oscheck() calls xltoplevel() */
      winterp_caught_signal = FALSE;
      lisp_reader_hit_eof = FALSE;
      Winterp_Print_Prompt(TRUE); /* xlflush() must occur after this in order to get new-line (as spec'd by arg 'TRUE') to occur */
      xlflush();		/* needed if using (read)/(read-line) from stdin */ 
      xltoplevel();
      break;
    case SIGFPE:		/* arithmetic exception --> oscheck() calls xlfail() */
      winterp_caught_signal = FALSE;
      lisp_reader_hit_eof = FALSE;
      xlfail("Caught SIGFPE -- arithmetic exception.");
      break;
    case SIGPIPE:
      winterp_caught_signal = FALSE;
      lisp_reader_hit_eof = FALSE;
      xlfail("Caught SIGPIPE -- broken pipe or socket.");
      break;
    default:
      /* any other signals caught by this handler are a programmer error */
      (void) sprintf(temptext, "Caught unexpected signal -- '%d'.",
		     winterp_caught_signal);
      xlfatal(temptext);
      break;
    }
  }
}
