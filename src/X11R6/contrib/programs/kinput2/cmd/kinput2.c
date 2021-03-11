/*
 *	kinput2
 */

/*
 * Copyright (C) 1991 by Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *		ishisone@sra.co.jp
 */

#ifndef lint
static char	*rcsid = "$Id: kinput2.c,v 1.34 1994/06/06 02:22:08 ishisone Rel $";
#endif

#include <stdio.h>
#include <signal.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include "patchlevel.h"
#include "AsyncErr.h"
#include "MyDispatch.h"
#include "ConvMgr.h"
#include "KIProto.h"
#include "XimpProto.h"
#include "IMProto.h"

#if !defined(USE_WNN) && !defined(USE_CANNA) && !defined(USE_SJ3)
#define USE_WNN			/* default */
#endif

#ifdef USE_WNN
#include "CcWnn.h"
#include "WcharDisp.h"
#endif
#ifdef USE_CANNA
#include "Canna.h"
#include "WcharDisp.h"
#endif
#ifdef USE_SJ3
#include "Sj3.h"
#include "WcharDisp.h"
#endif

#include "DebugPrint.h"

int	debug_all;

/* application resource */
typedef struct {
    char *conversionEngine;
    int debugLevel;
    Boolean useKinputProtocol;
    Boolean useXimpProtocol;
    Boolean useXIMProtocol;
} AppResRec, *AppResP;

static AppResRec appres;

static XtResource app_resources[] = {
    { "conversionEngine", "ConversionEngine", XtRString, sizeof(String),
      XtOffset(AppResP, conversionEngine), XtRString, (XtPointer)"" },
    { "debugLevel", "DebugLevel", XtRInt, sizeof(int),
      XtOffset(AppResP, debugLevel), XtRImmediate, (XtPointer)0 },
    { "useKinputProtocol", "UseKinputProtocol", XtRBoolean, sizeof(Boolean),
      XtOffset(AppResP, useKinputProtocol), XtRImmediate, (XtPointer)True },
    { "useXimpProtocol", "UseXimpProtocol", XtRBoolean, sizeof(Boolean),
      XtOffset(AppResP, useXimpProtocol), XtRImmediate, (XtPointer)True },
    { "useXIMProtocol", "UseXIMProtocol", XtRBoolean, sizeof(Boolean),
      XtOffset(AppResP, useXIMProtocol), XtRImmediate, (XtPointer)True },
};

static XrmOptionDescRec	options[] = {
    {"-bc",		"*KinputProtocol.backwardCompatible", XrmoptionNoArg,	"True"},
    {"-font",		"*JpWcharDisplay.font",	XrmoptionSepArg,	NULL},
    {"-kanjifont",	"*JpWcharDisplay.kanjiFont", XrmoptionSepArg,	NULL},
    {"-kanafont",	"*JpWcharDisplay.kanaFont", XrmoptionSepArg,	NULL},
    {"-kinput",		".useKinputProtocol",	XrmoptionNoArg,		"on"},
    {"+kinput",		".useKinputProtocol",	XrmoptionNoArg,		"off"},
    {"-ximp",		".useXimpProtocol",	XrmoptionNoArg,		"on"},
    {"+ximp",		".useXimpProtocol",	XrmoptionNoArg,		"off"},
    {"-xim",		".useXIMProtocol",	XrmoptionNoArg,		"on"},
    {"+xim",		".useXIMProtocol",	XrmoptionNoArg,		"off"},
#ifdef DEBUG
    {"-debug",		".debugLevel",		XrmoptionNoArg,		"1"},
    {"-trace",		".debugLevel",		XrmoptionNoArg,		"10"},
    {"-debuglevel",	".debugLevel",		XrmoptionSepArg,	NULL},
#endif
#ifdef USE_WNN
    {"-wnn",		".conversionEngine",	XrmoptionNoArg,		"wnn"},
    {"-jserver",	"*CcWnn.jserver",	XrmoptionSepArg,	NULL},
    {"-ccdef",		"*CcWnn.ccdef",		XrmoptionSepArg,	NULL},
    {"-wnnenvname",	"*CcWnn.wnnEnvname",	XrmoptionSepArg,	NULL},
    {"-wnnenvrc",	"*CcWnn.wnnEnvrc",	XrmoptionSepArg,	NULL},
#endif
#ifdef USE_CANNA
    {"-canna",		".conversionEngine",	XrmoptionNoArg,		"canna"},
    {"-cannaserver",	"*Canna.cannahost",	XrmoptionSepArg,	NULL},
    {"-cs",		"*Canna.cannahost",	XrmoptionSepArg,	NULL},
    {"-cannafile",	"*Canna.cannafile",	XrmoptionSepArg,	NULL},
#endif
#ifdef USE_SJ3
    {"-sj3",        ".conversionEngine",    XrmoptionNoArg,     "sj3"},
    {"-sj3serv",    "*Sj3.sj3serv",     XrmoptionSepArg,    NULL},
    {"-sj3serv2",   "*Sj3.sj3serv2",    XrmoptionSepArg,    NULL},
    {"-sj3user",    "*Sj3.sj3user",     XrmoptionSepArg,    NULL},
    {"-rcfile",     "*Sj3.rcfile",      XrmoptionSepArg,    NULL},
    {"-sbfile",     "*Sj3.sbfile",      XrmoptionSepArg,    NULL},
    {"-rkfile",     "*Sj3.rkfile",      XrmoptionSepArg,    NULL},
    {"-hkfile",     "*Sj3.hkfile",  XrmoptionSepArg,    NULL},
    {"-zhfile",     "*Sj3.zhfile",  XrmoptionSepArg,    NULL},
    {"-sjrc",       "*Sj3.rcfile",      XrmoptionSepArg,    NULL},
    {"-sjsb",       "*Sj3.sbfile",      XrmoptionSepArg,    NULL},
    {"-sjrk",       "*Sj3.rkfile",      XrmoptionSepArg,    NULL},
    {"-sjhk",       "*Sj3.hkfile",      XrmoptionSepArg,    NULL},
    {"-sjzh",       "*Sj3.zhfile",      XrmoptionSepArg,    NULL},
#endif
};

XtAppContext	apc;
Widget		toplevel;

static int	numProtocols;
static int	(*DefaultErrorHandler)();

static WidgetClass	getInputObjClass();
static int	IgnoreBadWindow();
#ifdef SIGNALRETURNSINT
static int	scheduleExit();
#else
static void	scheduleExit();
#endif
static void	exitTimer();
static void	Destroyed();
static void	Exit();
static void	realExit();
static void	usage();
static void	print_version();

void
main(ac, av)
int ac;
char **av;
{
    Widget manager, protocol;
    int i;
    WidgetClass inputobjclass, displayobjclass;

    toplevel = XtAppInitialize(&apc, "Kinput2",
			       options, XtNumber(options),
			       &ac, av,
			       (String *)NULL, (ArgList)NULL, 0);

    /* check invalid argument */ 
    if (ac > 1) {
	int do_usage = 0;
	for (i = 1; i < ac; i++) {
	    if (!strcmp(av[i], "-version")) {
		print_version();
	    } else {
		fprintf(stderr, "unknown argument: %s\n", av[i]);
		do_usage = 1;
	    }
	}
	if (do_usage) usage();
    }

    /* initialize asynchronous error handler */
    XAEInit();

    XtGetApplicationResources(toplevel, &appres,
			      app_resources, XtNumber(app_resources),
			      NULL, 0);

    /* set debug level */
    debug_all = appres.debugLevel;

#ifdef RANDOM_ID
    /*
     * one nasty hack here:
     *
     * kinput clients often use server's window ID for the only key
     * value to identify their conversion server (kinput), and they
     * think it is dead and take appropriate action (eg connecting to
     * the new server) when they notice the ID has changed.
     *
     * but it is likely that another kinput has the same resource ID
     * base (because X servers always choose the smallest unused ID
     * base for new clients). and if it is the same, so is the owner
     * window ID, and the clients don't notice the change.
     *
     * to get rid of the problem, we add some small random offset to
     * the resource ID so that every time we get different owner ID
     * even if the resource ID base is the same.
     *
     * of course it heavily depends on the current implementaion of
     * the resource ID allocation in Xlib, so I call it 'nasty'.
     */
    XtDisplay(toplevel)->resource_id += getpid() % 1024;
#endif

    inputobjclass = getInputObjClass();
    displayobjclass = jpWcharDisplayObjectClass;

    manager = XtVaCreateManagedWidget("convmanager",
				      conversionManagerWidgetClass,
				      toplevel,
				      XtNwidth, 1,
				      XtNheight, 1,
				      NULL);

    numProtocols = 0;

    if (appres.useKinputProtocol) {
	protocol = XtVaCreateWidget("kinputprotocol",
				    kinputProtocolWidgetClass,
				    manager,
				    XtNlanguage, "JAPANESE",
				    XtNinputObjectClass, inputobjclass,
				    XtNdisplayObjectClass, displayobjclass,
				    XtNwidth, 1,
				    XtNheight, 1,
				    NULL);
	XtAddCallback(protocol, XtNdestroyCallback,
		      Destroyed, (XtPointer)NULL);
	numProtocols++;
    }

    if (appres.useXimpProtocol) {
	protocol = XtVaCreateWidget("ximpprotocol",
				    ximpProtocolWidgetClass,
				    manager,
				    XtNlocaleName, "ja_JP",
				    XtNinputObjectClass, inputobjclass,
				    XtNdisplayObjectClass, displayobjclass,
				    XtNwidth, 1,
				    XtNheight, 1,
				    NULL);
	XtAddCallback(protocol, XtNdestroyCallback,
		      Destroyed, (XtPointer)NULL);
	numProtocols++;
    }

    if (appres.useXIMProtocol) {
	protocol = XtVaCreateWidget("improtocol",
				    imProtocolWidgetClass,
				    manager,
				    XtNlanguage, "ja_JP",
				    XtNinputObjectClass, inputobjclass,
				    XtNdisplayObjectClass, displayobjclass,
				    XtNwidth, 1,
				    XtNheight, 1,
				    NULL);
	XtAddCallback(protocol, XtNdestroyCallback,
		      Destroyed, (XtPointer)NULL);
	numProtocols++;
    }

    if (numProtocols == 0) {
	fprintf(stderr, "no protocols activated\n");
	exit(1);
    }

    /* set signal handler */
    if (signal(SIGINT, SIG_IGN) != SIG_IGN) signal(SIGINT, scheduleExit);
    signal(SIGTERM, scheduleExit);
#ifdef USE_WNN
    signal(SIGPIPE, SIG_IGN);
#endif

    /* set my error handler */
    DefaultErrorHandler = XAESetErrorHandler(IgnoreBadWindow);

    XtRealizeWidget(toplevel);

    for (;;) {
	XEvent event;

	XtAppNextEvent(apc, &event);
	XtDispatchEvent(&event);
	MyDispatchEvent(&event); /* additional dispatcher */
    }
    /* NOTREACHED */
}

static WidgetClass
getInputObjClass()
{
    WidgetClass class;

#ifdef USE_WNN
    if (!strcmp(appres.conversionEngine, "wnn")) {
	return ccWnnObjectClass;
    }
#endif
#ifdef USE_CANNA
    if (!strcmp(appres.conversionEngine, "canna") ||
	!strcmp(appres.conversionEngine, "iroha")) {
	return cannaObjectClass;
    }
#endif
#ifdef USE_SJ3
    if (!strcmp(appres.conversionEngine, "sj3")) {
	return sj3ObjectClass;
    }
#endif

    /* set default input object */
#ifdef USE_SJ3
    class = sj3ObjectClass;
#endif
#ifdef USE_CANNA
    class = cannaObjectClass;
#endif
#ifdef USE_WNN
    class = ccWnnObjectClass;
#endif

    return class;
}

static int
IgnoreBadWindow(dpy, error)
Display *dpy;
XErrorEvent *error;
{
    /*
     * BadWindow events will be sent if any of the active clients die
     * during conversion.  Although I select DestroyNotify event on the
     * client windows to detect their destruction and take appropriate
     * actions, and I'm careful when doing critical operations, but still
     * there is a chance of getting unexpecting BadWindow error caused by
     * client death.
     * There are also chances of getting BadDrawable as well.
     * So I set the error handler to ignore BadWindow/BadDrawable errors.
     * Of course I'd better check if the resourceid field of the error
     * event is the window ID of a client, but I'm too lazy to do that...
     */
    if (error->error_code != BadWindow && error->error_code != BadDrawable) {
	/* invoke default error handler */
	(*DefaultErrorHandler)(dpy, error);
    }
    return 0;
}

#ifdef SIGNALRETURNSINT
static int
#else
static void
#endif
scheduleExit()
{
    /*
     * It is unwise to do complex operation (in this case,
     * XtDestroyWidget) in a signal handler.
     * So postpone the real work...
     */
    XtAppAddTimeOut(apc, 1L, exitTimer, (XtPointer)NULL);
}

/* ARGSUSED */
static void
exitTimer(cldata, timerp)
XtPointer cldata;
XtIntervalId *timerp;
{
    Exit();
}

/* ARGSUSED */
static void
Destroyed(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    static int cnt = 0;

    /*
     * if all the protocol widgets are dead, kill myself.
     */
    if (++cnt >= numProtocols) Exit();
}

static void
Exit()
{
    static int exiting = 0;

    if (exiting) return;

    exiting = 1;

    /*
     * Destroy all widgets.
     */
    XtDestroyWidget(toplevel);

    /*
     * Postpone calling exit() until next XtNextEvent(),
     * in order to give each widget time to execute their
     * destroy procedure.
     */
    XtAppAddTimeOut(apc, 1L, realExit, (XtPointer)NULL);
}

/* ARGSUSED */
static void
realExit(cldata, timerp)
XtPointer cldata;
XtIntervalId *timerp;
{
    exit(0);
}

static void
usage()
{
    char **p;
    static char *syntaxtable[] = {
#ifdef USE_WNN
	"-wnn",			"use Wnn as the conversion engine",
	"-jserver <hostname>",	"specify jserver host",
	"-ccdef <ccdeffile>",	"specify character conversion def. file",
	"-wnnenvname <name>",	"specify Wnn environment name",
	"-wnnenvrc <wnnenvrcfile>",	"specify Wnn environment file",
#endif
#ifdef USE_CANNA
	"-canna",		"use Canna (Iroha) as the conversion engine",
	"{-cannaserver|-cs} <hostname>[:n]", "specify cannaserver host",
	"-cannafile <cannafile>", "specify canna customize file",
#endif
#ifdef USE_SJ3
	"-sj3",         	"use SJ3 as the conversion engine",
	"-sj3serv <hostname>",  "specify first sj3serv host",
	"-sj3serv2 <hostname>", "specify second sj3serv host",
	"-sj3user <user>",      "specify user name connect to sj3serv",
	"{-rcfile|-sjrc} <file>",       "specify resource definition file",
	"{-sbfile|-sjsb} <file>",       "specify symbol table file",
	"{-rkfile|-sjrk} <file>",       "specify roma-kana coversion definition file",
	"{-hkfile|-sjhk} <file>",       "specify hira-kata coversion definition file",
	"{-zhfile|-sjzh} <file>",       "specify zen/han coversion definition file",
#endif
	"-bc",			"backward compatible mode",
	"-font <font>",		"ASCII font to be used",
	"-kanjifont <font>",	"KANJI font to be used",
	"-kanafont <font>",	"KANA font to be used",
	"-/+kinput",		"activate/deactivate kinput protocol family",
	"-/+ximp",		"activate/deactivate Ximp protocol",
	"-/+xim",		"activate/deactivate X Input Method protocol",
	"-background <color>",	"background color",
	"-foreground <color>",	"foreground color",
	"-rv",			"reverse video mode",
	"-display <display>",	"specify display",
	"-version",		"print version information and exit",
#ifdef DEBUG
	"-debug",		"print debug messages (debug level 1)",
	"-trace",		"print trace messages (debug level 10)",
	"-debuglevel <level>",	"set debug level",
#endif
	NULL, NULL,
    };

    fprintf(stderr, "options are:\n");
    for (p = syntaxtable; *p != NULL; p += 2) {
	fprintf(stderr, "    %-30s %s\n", *p, *(p + 1));
    }
    exit(1);
}

static void
print_version()
{
    char *p;

    printf("kinput2 %s ", KINPUT2_VERSION);
    if (PATCHLEVEL > 0) printf("fix %d ", PATCHLEVEL);
    printf(" (");
    p = DATE + 7;				/* skip '$Date: ' */
    while (*p != '\0' && *p != ' ') {
	putchar(*p);	/* print date */
	p++;
    }
    printf(")\n");

    printf("\toptions: ");
#ifdef USE_WNN
    printf("[Wnn] ");
#endif
#ifdef USE_CANNA
    printf("[Canna2] ");
#endif
#ifdef USE_SJ3
    printf("[Sj3] ");
#endif
#ifdef DEBUG
    printf("[DEBUG] ");
#endif
    printf("\n");
    exit(0);
}

#if defined(USE_WNN) && defined(NEED_Strlen)
/*
 * Wnn/jlib/js.c should have this function...
 */
int
Strlen(s)
unsigned short *s;
{
    int n = 0;

    while (*s++) n++;
    return n;
}
#endif
