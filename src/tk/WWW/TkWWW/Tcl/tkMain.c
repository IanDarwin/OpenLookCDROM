/*
 * Removed the interactive stuff for tkWWW
 */

/* 
 * main.c --
 *
 *	This file contains the main program for "wish", a windowing
 *	shell based on Tk and Tcl.  It also provides a template that
 *	can be used as the basis for main programs for other Tk
 *	applications.
 *
 * Copyright (c) 1990-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#ifndef lint
static char rcsid[] = "/mit/tkwww/CVS/WWW/TkWWW/Tcl/tkMain.c,v 1.1 1994/04/01 08:36:23 joe Exp SPRITE (Berkeley)";
#endif

#include <stdio.h>
#include <tcl.h>
#include <tk.h>

/*
 * Declarations for various library procedures and variables (don't want
 * to include tkInt.h or tkConfig.h here, because people might copy this
 * file out of the Tk source directory to make their own modified versions).
 */

extern void		exit _ANSI_ARGS_((int status));
extern int		isatty _ANSI_ARGS_((int fd));
extern int		read _ANSI_ARGS_((int fd, char *buf, size_t size));
extern char *		strrchr _ANSI_ARGS_((CONST char *string, int c));

/*
 * Global variables used by the main program:
 */

static Tk_Window mainWindow;	/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */
static Tcl_Interp *interp;	/* Interpreter for this application. */
char *tcl_RcFileName = NULL;	/* Name of a user-specific startup script
				 * to source if the application is being run
				 * interactively (e.g. "~/.wishrc").  Set
				 * by Tcl_AppInit.  NULL means don't source
				 * anything ever. */
static Tcl_DString command;	/* Used to assemble lines of terminal input
				 * into Tcl commands. */
static int tty;			/* Non-zero means standard input is a
				 * terminal-like device.  Zero means it's
				 * a file. */
static char errorExitCmd[] = "exit 1";

/*
 * Command-line options:
 */

static int synchronize = 0;
static char *fileName = NULL;
static char *name = NULL;
static char *display = NULL;
static char *geometry = NULL;

static Tk_ArgvInfo argTable[] = {
    {"-file", TK_ARGV_STRING, (char *) NULL, (char *) &fileName,
	"File from which to read commands"},
    {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
	"Initial geometry for window"},
    {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &display,
	"Display to use"},
    {"-name", TK_ARGV_STRING, (char *) NULL, (char *) &name,
	"Name to use for application"},
    {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
	"Use synchronous mode for display server"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	Main program for Wish.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done
 *
 * Side effects:
 *	This procedure initializes the wish world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;				/* Number of arguments. */
    char **argv;			/* Array of argument strings. */
{
    char *args, *p, *msg;
    char buf[20];
    int code;

    interp = Tcl_CreateInterp();
#ifdef TCL_MEM_DEBUG
    Tcl_InitMemory(interp);
#endif

    /*
     * Parse command-line arguments.
     */

    if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 0)
	    != TCL_OK) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    /*
     * If a display was specified, put it into the DISPLAY
     * environment variable so that it will be available for
     * any sub-processes created by us.
     */

    if (display != NULL) {
	Tcl_SetVar2(interp, "env", "DISPLAY", display, TCL_GLOBAL_ONLY);
    }

    /*
     * Initialize the Tk application.
     */

    mainWindow = Tk_CreateMainWindow(interp, display, "tkWWW", "Tk");
    if (mainWindow == NULL) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    if (synchronize) {
	XSynchronize(Tk_Display(mainWindow), True);
    }
    Tk_GeometryRequest(mainWindow, 200, 200);

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".  Also set the "geometry" variable from the geometry
     * specified on the command line.
     */

    args = Tcl_Merge(argc-1, argv+1);
    Tcl_SetVar(interp, "argv", args, TCL_GLOBAL_ONLY);
    ckfree(args);
    sprintf(buf, "%d", argc-1);
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv0", (fileName != NULL) ? fileName : argv[0],
	    TCL_GLOBAL_ONLY);
    if (geometry != NULL) {
	Tcl_SetVar(interp, "geometry", geometry, TCL_GLOBAL_ONLY);
    }

    /*
     * Set the "tcl_interactive" variable.
     */

    tty = isatty(0);
    Tcl_SetVar(interp, "tcl_interactive",
	    ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);

    /*
     * Add a few application-specific commands to the application's
     * interpreter.
     */

    /*
     * Invoke application-specific initialization.
     */

    if (Tcl_AppInit(interp) != TCL_OK) {
	fprintf(stderr, "Tcl_AppInit failed: %s\n", interp->result);
    }

    /*
     * Set the geometry of the main window, if requested.
     */

    if (geometry != NULL) {
	code = Tcl_VarEval(interp, "wm geometry . ", geometry, (char *) NULL);
	if (code != TCL_OK) {
	    fprintf(stderr, "%s\n", interp->result);
	}
    }

    Tcl_DStringInit(&command);

    /*
     * Loop infinitely, waiting for commands to execute.  When there
     * are no windows left, Tk_MainLoop returns and we exit.
     */

    Tk_MainLoop();

    /*
     * Don't exit directly, but rather invoke the Tcl "exit" command.
     * This gives the application the opportunity to redefine "exit"
     * to do additional cleanup.
     */

    Tcl_Eval(interp, "exit");
    exit(1);

error:
    msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
    if (msg == NULL) {
	msg = interp->result;
    }
    fprintf(stderr, "%s\n", msg);
    Tcl_Eval(interp, errorExitCmd);
    return 1;			/* Needed only to prevent compiler warnings. */
}
