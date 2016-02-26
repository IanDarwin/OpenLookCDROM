#ifdef IDENT
#ident "@(#)olwm.c	1.5 olvwm version 07 Jan 1994"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

/*
 * Based on
#ident	"@(#)olwm.c	26.66	93/06/28 SMI"
 */

#include <errno.h>
#include <memory.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>

#include <sys/time.h>
#include <sys/types.h>

#include <sys/param.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#include "i18n.h"
#include "ollocale.h"
#include "events.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "defaults.h"
#include "resources.h"
#include "globals.h"
#include "group.h"
#include "selection.h"
#include "slots.h"
#include "error.h"
#include "dsdm.h"

#include "patchlevel.h"

typedef	void	(*VoidFunc)();


/*
 * Globals
 */

char		*ProgramName;		/* pointer to original argv[0] */
char		*AppName;		/* last component of ProgramName */
GlobalResourceVariables GRV;		/* variables settable by rsrcs */
XrmDatabase	OlwmDB;			/* the main resource database */
Display		*DefDpy;		/* the display connection */


#ifdef DEBUG

unsigned char	ScratchBuffer[1024];	/* for use in the debugger */

#endif /* DEBUG */


/*
 * Global Quarks.  "Top" refers to the root of the resource name/instance 
 * hierarchy.
 */

XrmQuark TopClassQ;
XrmQuark TopInstanceQ;
XrmQuark OpenWinQ;

/*
 * Quark for olwm instance -- we read resources with an instance name of
 * olwm for backwards compatibility
 */
XrmQuark OlwmQ;


/*
 * Forward declarations.
 */

static void	usage();
static Display *openDisplay();
static void	parseCommandline();
static void	sendSyncSignal();
static void	initWinClasses();


/*
 * Command-line option table.  Resources named here must be kept in sync with 
 * the resources probed for in resources.c.
 */
static	XrmOptionDescRec	optionTable[] = {
	/*
	 * Standard Options
	 */
	{ "-display",	".display",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-name",	".name",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-xrm",	NULL,		XrmoptionResArg, (caddr_t)NULL },
	{ "-2d", 	".use3D",	XrmoptionNoArg,  (caddr_t)"False" },
	{ "-3d", 	".use3D",	XrmoptionNoArg,  (caddr_t)"True" },
	{ "-bd",	"*BorderColor",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-bordercolor","*BorderColor",XrmoptionSepArg, (caddr_t)NULL },
	{ "-bg",	"*Background",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-background","*Background",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-fg",	"*Foreground",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-foreground","*Foreground",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-c", 	".setInput",	XrmoptionNoArg,  (caddr_t)"select" },
	{ "-click", 	".setInput",	XrmoptionNoArg,  (caddr_t)"select" },
	{ "-f",		".setInput",	XrmoptionNoArg,  (caddr_t)"follow" },
	{ "-follow",	".setInput",	XrmoptionNoArg,  (caddr_t)"follow" },
	{ "-fn",	"*TitleFont",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-font",	"*TitleFont",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-single",	".singleScreen",XrmoptionNoArg,  (caddr_t)"True" },
	{ "-multi",	".singleScreen",XrmoptionNoArg,  (caddr_t)"False" },
	{ "-syncpid",   ".syncPid",     XrmoptionSepArg, (caddr_t)NULL },
	{ "-syncsignal",".syncSignal",  XrmoptionSepArg, (caddr_t)NULL },
	{ "-depth",	"*depth",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-visual",	"*visual",	XrmoptionSepArg, (caddr_t)NULL },
	{ "-dsdm",	".startDSDM",	XrmoptionNoArg,	 (caddr_t)"True" },
	{ "-nodsdm",	".startDSDM",	XrmoptionNoArg,	 (caddr_t)"False" },
	/*
	 * Debugging Options
	 */
	{ "-all", 	 ".printAll",	 XrmoptionNoArg, (caddr_t)"True" },
	{ "-debug",	 ".printOrphans",XrmoptionNoArg, (caddr_t)"True" },
	{ "-orphans", 	 ".printOrphans",XrmoptionNoArg, (caddr_t)"True" },
	{ "-synchronize",".synchronize", XrmoptionNoArg, (caddr_t)"True" },
#ifdef OW_I18N_L3
	/* 
	 * Internationalization Options
	 */
        { "-basiclocale", "*basicLocale", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-displaylang", "*displayLang", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-inputlang",   "*inputLang", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-numeric",     "*numeric", XrmoptionSepArg, (caddr_t) NULL },
       	{ "-dateformat",  "*dateFormat", XrmoptionSepArg, (caddr_t) NULL },
#endif /* OW_I18N_L3 */
};
#define OPTION_TABLE_ENTRIES (sizeof(optionTable)/sizeof(XrmOptionDescRec))

/* Child Process Handling */

static Bool deadChildren = False;
static void handleChildSignal();
static int slavePid;

void ReapChildren();		/* public -- called from events.c */

#ifdef ALLPLANES
Bool AllPlanesExists;		/* server supports the ALLPLANES extension */
#endif

#ifdef SHAPE
Bool ShapeSupported;		/* server supports the SHAPE extension */
int  ShapeEventBase;
int  ShapeErrorBase;
int  ShapeRequestBase;
#endif

int	numbuttons;		/* number of buttons on the pointer */
				/*   REMIND: this shouldn't be global */

static char	**argVec;

/*
 * main	-- parse arguments, perform initialization, call event-loop
 */
void
main(argc, argv)
	int argc;
	char **argv;
{
	int			ExitOLWM(), RestartOLWM();
	XrmDatabase		commandlineDB = NULL;
	char			*dpystr;

#ifdef OW_I18N_L3
	char			*OpenWinHome;
	char			locale_dir[MAXPATHLEN+1];
	extern char		*getenv();
#endif /* OW_I18N_L3 */

#ifdef MALLOCDEBUG
	malloc_debug(MALLOCDEBUG);
#endif /* MALLOCDEBUG */
#ifdef GPROF_HOOKS
	moncontrol(0);
#endif /* GPROF_HOOKS */

#ifdef OW_I18N_L3
       	/*
       	 * Even in the SUNDAE1.0 (first release) we might need the
       	 * dynamic locale change for window manager, since window
       	 * manager is usually never re-start again in one sesstion.
       	 * But leave static for now.
       	 */
       	/*
       	 * We are setting the locale (issuing the setlocale) by
       	 * EffectOLLC() function, but we need to call setlocale here
       	 * to handle command line argument with certain locale.
       	 * FIX_ME! But may not work well, because we did not touch the
       	 * Xlib function XrmParseCommand().
       	 */
       	if (setlocale(LC_CTYPE, "") == NULL) {
		char		*locale;

		locale = (locale = getenv("LC_CTYPE")) != NULL ? locale
			: (locale = getenv("LANG")) != NULL ? locale
			: "Unknown";
		/* 
		 * In the following fprintf, it is point less to use
		 * GetString, since we are saying using "C" locale instead.
		 */
		fprintf(stderr, "\
olvwm: Warning: '%s' is invalid locale for the LC_CTYPE category,\n\
               using 'C' locale for the command line parsing.\n",
				locale);
		(void)setlocale(LC_CTYPE,"C");
	}
	if ((OpenWinHome = getenv("OPENWINHOME")) != 0)
		(void)strcpy(locale_dir,OpenWinHome);
	else
		(void)strcpy(locale_dir,"/usr");
	(void)strcat(locale_dir,"/lib/locale");
#ifdef SVR4
	bindtextdomain("olwm_messages",locale_dir);
       	textdomain("olwm_messages");
#endif
#endif /* OW_I18N_L3 */

	ProgramName = argv[0];
	argVec = argv;

	/*
	 * Set up signal handlers.  Clean up and exit on SIGHUP, SIGINT, and 
	 * SIGTERM; note child process changes on SIGCHLD.
	 */
#ifdef SYSV
	sigset(SIGHUP, (VoidFunc)ExitOLWM);
	sigset(SIGINT, (VoidFunc)ExitOLWM);
	sigset(SIGTERM, (VoidFunc)ExitOLWM);
	sigset(SIGCHLD, handleChildSignal);
	sigset(SIGUSR1, (VoidFunc)RestartOLWM);
#else
	signal(SIGHUP, (VoidFunc)ExitOLWM);
	signal(SIGINT, (VoidFunc)ExitOLWM);
	signal(SIGTERM, (VoidFunc)ExitOLWM);
	signal(SIGCHLD, handleChildSignal);
	signal(SIGUSR1, (VoidFunc)RestartOLWM);
#endif

	XrmInitialize();

	/* parse the command line arguments into local tmp DB */
	parseCommandline(&argc, argv, &commandlineDB);

	DefDpy = openDisplay(commandlineDB);

#ifdef ALLPLANES
	{
	    int tmp;
	    AllPlanesExists = XAllPlanesQueryExtension(DefDpy, &tmp, &tmp);
	}
#endif /* ALLPLANES */


#ifdef SHAPE
	ShapeSupported = XQueryExtension(DefDpy, "SHAPE",
	    &ShapeRequestBase, &ShapeEventBase, &ShapeErrorBase);
#endif /* SHAPE */


	/*
	 * Determine the number of buttons on the pointer.  Use 3 by default.
	 */
	numbuttons = XGetPointerMapping (DefDpy, (unsigned char *)0, 0);
	if (numbuttons < 1)
		numbuttons = 3;

	/* put all resources into global OlwmDB and set olwm variables */
	GetDefaults(DefDpy, commandlineDB);

	/* Initialize the event handling system. */
	InitEvents(DefDpy);
	InitBindings(DefDpy);
	XSetErrorHandler(ErrorHandler);
	if (GRV.Synchronize)
		XSynchronize(DefDpy, True);

	/* Initialize a variety of olwm subsystems. */
	InitAtoms(DefDpy);
	WIInit(DefDpy);
	initWinClasses(DefDpy);
	InitClients(DefDpy);
	GroupInit();

	/*
	 * Ensure that the X display connection is closed when we exec a 
	 * program.
	 */
	if (fcntl(ConnectionNumber(DefDpy), F_SETFD, 1) == -1) {
		perror(GetString("olvwm: child cannot disinherit TCP fd"));
		exit(1);
	}

	/* Init the global menus  -- No more global menus; done in InitScreens
	InitMenus(DefDpy);
	*/

	/* init region handling code */
	InitRegions();

	/* Init screen */
	InitScreens(DefDpy);
	InitOlvwmRC(DefDpy, NULL);
	GrabKeys(DefDpy, True);
	GrabButtons(DefDpy, True);
	ReparentScreens(DefDpy);
	if (!GRV.FocusFollowsMouse)
	    ClientFocusTopmost(DefDpy, GetFirstScrInfo(), CurrentTime);
	CreateAutoRootMenu(DefDpy);

	/* Initialize selections. */
	SelectionInit();

	/* Initialize (and then start, if desired) the DSDM function. */
	DragDropInit();
	if (GRV.StartDSDM)
	    DragDropStartDSDM(DefDpy);

 	/* Start olwmslave - using the same args we got. */
	if (GRV.RunSlaveProcess)
	    slavePid = SlaveStart(argVec);

	/* Beep to indicate that we're ready. */
	if (GRV.Beep != BeepNever)
	    XBell(DefDpy, 100);

	/* Inform anyone who's waiting that we're ready. */
	sendSyncSignal();

	EventLoop(DefDpy);

	/*NOTREACHED*/
}


/* 
 * parseCommandline - parse the command line arguments into a resource
 *	database
 */
static void
parseCommandline( argc, argv, tmpDB )
int		*argc;
char		*argv[];
XrmDatabase	*tmpDB;
{
	char	instName[MAX_NAME];
	char	namestr[MAX_NAME];
	char	*type, *p;
	XrmValue val;

	/* Extract trailing pathname component of argv[0] into AppName. */

	AppName = strrchr(argv[0], '/');
	if (AppName == NULL)
	    AppName = argv[0];
	else
	    ++AppName;

	XrmParseCommand(tmpDB, optionTable, OPTION_TABLE_ENTRIES,
			AppName, argc, argv );

	/*
	 * Initialize root instance and class quarks.  Create the instance
	 * name by first looking up the "name" resource in the command line
	 * database (for the -name option).  If it's not present, use AppName
	 * (the trailing pathname component of argv[0]).  Then, scan it and
	 * replace all illegal characters with underscores.  Note: we don't
	 * use the ctype functions here, because they are internationalized.
	 * In some locales, isalpha() will return true for characters that are
	 * not valid in resource component names.  Thus, we must fall back to
	 * standard character comparisions.
	 *
	 * REMIND: specifying the -name option changes the name with which 
	 * resources are looked up.  But the command line options were put 
	 * into the database using AppName, which is based on argv[0].  Thus, 
	 * specifying -name causes all command-line args to be ignored, which 
	 * is wrong.
	 */

	(void) strcpy(namestr, AppName);
	(void) strcat(namestr, ".name");
	if (XrmGetResource(*tmpDB, namestr, namestr, &type, &val)) {
	    (void) strncpy(instName, (char *)val.addr, MAX_NAME);
	} else {
	    (void) strncpy(instName, AppName, MAX_NAME);
	}

	instName[MAX_NAME-1] = '\0';
	for (p=instName; *p != '\0'; ++p) {
	    if (!(*p >= 'a' && *p <= 'z' ||
		  *p >= 'A' && *p <= 'Z' ||
		  *p >= '0' && *p <= '9' ||
		  *p == '_' || *p == '-')) {
		*p = '_';
	    }
	}
	TopInstanceQ = XrmStringToQuark(instName);
	TopClassQ = XrmStringToQuark("Olwm");
	OpenWinQ = XrmStringToQuark("OpenWindows");
	OlwmQ = XrmStringToQuark("olwm");

	/* check to see if there are any arguments left unparsed */
	if ( *argc != 1 )
	{
	    if (!strcmp(argv[1], "-allowSynthetic"))
		GRV.AllowSyntheticEvents = True;
	    else {
		GRV.AllowSyntheticEvents = False;
		/* check to see if it's -help */
		if ( argv[1][0] == '-' && argv[1][1] == 'h' ) {
			usage(  GetString("Command line arguments accepted"),
				GetString("are:"));
		} else {
			usage(	GetString("Unknown argument(s)"), 
				GetString("encountered"));
		}
	    }
	}
}


/*
 * openDisplay - open the connection to the X display.  A probe is done into
 * the command-line resource database in order to pick up the '-display'
 * command-line argument.  If it is found, its value is put into the
 * environment.
 */
static Display *
openDisplay(rdb)
    XrmDatabase rdb;
{
    char namebuf[MAX_NAME];
    char *type;
    XrmValue value;
    char *dpystr = NULL;
    char *envstr;
    Display *dpy;

    (void) strcpy(namebuf, AppName);
    (void) strcat(namebuf, ".display");

    if (XrmGetResource(rdb, namebuf, namebuf, &type, &value)) {
	dpystr = (char *)value.addr;
	envstr = (char *)MemAlloc(8+strlen(dpystr)+1);
	sprintf(envstr, "DISPLAY=%s", dpystr);
	putenv(envstr);
    }

    dpy = XOpenDisplay(dpystr);
    if (dpy == NULL) {
	if (dpystr == NULL)
	    dpystr = GetString("(NULL DISPLAY)");
	fprintf(stderr, GetString("%s: cannot connect to %s\n"),
		ProgramName, dpystr);
	exit(1);
    }
    return dpy;
}


/*
 * sendSyncSignal
 *
 * Send a signal to the process named on the command line (if any).  Values
 * for the process id and signal to send are looked up in the resource 
 * database; they are settable with command-line options.  The resources are 
 * looked up with the names
 * 
 *	<appname>.syncPid		process id
 *	<appname>.syncSignal		signal to send (integer)
 *
 * where <appname> is the trailing pathname component of argv[0].
 */
static void
sendSyncSignal()
{
    char *type;
    XrmValue value;
    int pid;
    int sig = SIGALRM;
    int tmp;
    char namebuf[100];

    (void) strcpy(namebuf, AppName);
    (void) strcat(namebuf, ".syncPid");
    if (!XrmGetResource(OlwmDB, namebuf, namebuf, &type, &value))
	return;
    pid = atoi((char *)value.addr);
    if (pid <= 0 || pid > MAXPID)
	return;

    (void) strcpy(namebuf, AppName);
    (void) strcat(namebuf, ".syncSignal");
    if (XrmGetResource(OlwmDB, namebuf, namebuf, &type, &value)) {
	tmp = atoi((char *)value.addr);
	if (tmp > 0 && tmp <= SIGUSR2)
	    sig = tmp;
    }
    (void) kill(pid, sig);
}
 

/*
 * initWinClasses -- initialize all of olwm's class structures.
 */
static void
initWinClasses(dpy)
Display *dpy;
{
	FrameInit(dpy);
	IconInit(dpy);
	ResizeInit(dpy);
	ColormapInit(dpy);
	ButtonInit(dpy);
	BusyInit(dpy);
	MenuInit(dpy);
	PinMenuInit(dpy);
	RootInit(dpy);
	NoFocusInit(dpy);
	PushPinInit(dpy);
	PaneInit(dpy);
	IconPaneInit(dpy);
	VirtualInit(dpy);
}


/*
 * Exit -- kill the slave process, kill all running applications, then exit.
 */
void
Exit(dpy)
Display	*dpy;
{
	extern void *ClientShutdown();
	
	SlaveStop();
	ListApply(ActiveClientList, ClientShutdown, (void *)0);
	XSync(dpy, True);
	exit(0);
	/*NOTREACHED*/
}


/*
 * cleanup -- kill the slave process, destroy pinned menus, and restore all 
 * client windows to the screen.  Does not exit.
 */
static void
cleanup()
{
	extern void *UnparentClient();

	/*
 	 * If DefDpy is NULL then we didn't get to the XOpenDisplay()
	 * so basically there is nothing to clean up so return.
	 */
	if (DefDpy == NULL)
		return;

	/*
	 * Stop olwmslave
 	 */
	SlaveStop();

	/*
	 * destroy all pinned menus
	 */
	DestroyPinnedMenuClients();

	 /*
	  * Clean up the virtual desktop -- if we aren't left in the upper
	  * left corner, nothing will start in the right place
	  */
	VirtualCleanup(DefDpy);

	/*
	 * Go through the list of windows.  Unmap all icons that are on the
	 * screen.  Reparent all windows back to the root, suitably offset
	 * according to their window-gravities.  Also remap all non-withdrawn
	 * windows, and remove all Withdrawn windows from the save-set (so
	 * they don't get remapped.  REMIND: We have to do this because
	 * Withdrawn windows are still left reparented inside the frame; this
	 * shouldn't be the case.
	 */
	ListApply(ActiveClientList,UnparentClient,NULL);

	/* Destroy the screens - which will restore input focus, colormap,
	 * and background, etc.
	 */
	DestroyScreens(DefDpy);

	XSync(DefDpy, True);
}


/* RestartOLWM -- clean up and then re-exec argv. */
int
RestartOLWM()
{
#ifndef SYSV
int     mask;
    mask = sigblock(SIGUSR1);
    sigsetmask(mask & ~(sigmask(SIGUSR1)));
#else
    sigrelse(SIGUSR1);
#endif
    cleanup();
    execvp(argVec[0], argVec);
    ErrorGeneral("cannot restart");
    /*NOTREACHED*/
    return 1;
}


/* Clean up and then exit. */
int
ExitOLWM()
{
    cleanup();
    exit(0);
    return 1;
}


/*
 * handleChildSignal - keep track of children that have died
 */
static void
handleChildSignal()
{
#ifdef SYSV
#ifndef SVR4
	signal(SIGCHLD, handleChildSignal);
#endif
#endif
	deadChildren = True;
}


/*
 * ReapChildren - wait() for all dead child processes.  Blocks SIGCHLD, reaps 
 * children until there aren't any more that have died, then unblock SIGCHLD.
 */
void
ReapChildren()
{
#ifdef SYSV
        pid_t pid;
        int status;
#else
	int oldmask;
	int pid;
	union wait status;
#endif

	if (!deadChildren)
		return;

#ifdef SYSV
	sighold(SIGCHLD);
#else
	oldmask = sigblock(sigmask(SIGCHLD));
#endif

	/* clean up children until there are no more to be cleaned up */

	while (1) {

#ifdef SYSV
                pid = waitpid(-1, &status, WNOHANG);
#else
                pid = wait3(&status, WNOHANG, (struct rusage *)0);
#endif

		if (pid == 0)
			break;

		if (pid == -1) {
		    if (errno == EINTR)
			continue;
		    if (errno != ECHILD)
			perror("olwm -- wait");
		    break;
		}

		/* if it's the slave process then stop its use */
		if (pid == slavePid) 
			SlaveStopped();

		if (WIFSTOPPED(status))
			kill(pid, SIGKILL);
	}

	deadChildren = False;

#ifdef SYSV
	sigrelse(SIGCHLD);
#else
        (void) sigsetmask(oldmask);
#endif
}


/*
 * usage(s1, s2)	-- print informative message regarding usage
 */
static void
usage(s1, s2)
char	*s1, *s2;
{
	fprintf(stderr, "%s %s\n", s1, s2);
	fprintf(stderr,GetString("usage: %s [options]\n"),ProgramName);

/* STRING_EXTRACTION - do not translate the option (ie -2d, -display)
 *	because those are the actual string names of the command line
 *	option.  Translate the option argument (ie <color>) and
 *	the descriptive text.
 */

#define USAGE(msg)	(void) fprintf(stderr,"%s\n",GetString(msg))

USAGE("Standard Options:");

USAGE(" -2d                         Use two-dimensional look");
USAGE(" -3d                         Use three-dimensional look");
USAGE(" -bd, -bordercolor <color>   Specify the border color");
USAGE(" -bg, -background <color>    Specify the background color");
USAGE(" -c, -click                  Use click-to-focus mode");
USAGE(" -depth <depth>              Specify the depth of the visual to use");
USAGE(" -display <display-string>   Specify the display to manage");
USAGE(" -f, -follow                 Use focus-follows-mouse mode");
USAGE(" -fn, -font <font-name>      Set the font for window titles");
USAGE(" -fg, -foreground <color>    Specify the foreground color");
USAGE(" -multi                      Manage windows on all screens");
USAGE(" -name <resource-name>       Specify resource name for resource db");
USAGE(" -single                     Manage windows for a single screen only");
USAGE(" -syncpid <process-id>       Synchronize with process-id");
USAGE(" -syncsignal <signal>        Signal to send to syncpid");
USAGE(" -xrm <resource-string>      Specify resources on commandline");

USAGE("Debugging Options:");

USAGE(" -all                        Print a message for all events received");
USAGE(" -debug                      Turn on all debugging options");
USAGE(" -orphans                    Print orphaned events");
USAGE(" -synchronize                Run in synchronous mode");
;
USAGE("Internationalization Options:");

USAGE(" -basiclocale <locale-name>  Specify the basic locale for all categories");
USAGE(" -displaylang <locale-name>  Specify the language used for displaying text");
USAGE(" -numeric <locale-name>      Specify the numeric format");

#undef USAGE

	exit(1);
}
