#ifdef IDENT
#ident  "@(#)defaults.c	26.22    93/06/28 SMI"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifdef SYSV
#include <sys/types.h>
#ifdef SVR4
#include <sys/systeminfo.h>
#endif
#endif

#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/file.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>
#ifdef OW_I18N_L4
#include <sys/param.h>
#endif

#include "i18n.h"
#include "ollocale.h"
#include "olwm.h"
#include "defaults.h"
#include "globals.h"
#include "resources.h"


/*
 * GetUserDefaults
 *
 * Get RESOURCE_MANAGER string from server; if none, then load from
 * $HOME/.Xdefaults.  If XENVIRONMENT names a file, load and merge it.
 * Otherwise, load $HOME/.Xdefaults-hostname and merge it.  See
 * Xlib/XGetDflt.c.  We could use that code if it weren't Xlib-private.
 */
XrmDatabase
GetUserDefaults(dpy)
    Display	*dpy;
{
    XrmDatabase serverDB = NULL;
    XrmDatabase fileDB = NULL;
    char filename[1024];
    unsigned long nitems, remain;
    char *rsrcstr;
    char *homedir = getenv("HOME");
    char *envfile = getenv("XENVIRONMENT");
    char hostname[100];
#ifndef SVR4
    int namelen;
#endif

    rsrcstr = GetWindowProperty(dpy, RootWindow(dpy, 0), XA_RESOURCE_MANAGER,
	0L, 100000000L, /* REMIND: use ENTIRE_CONTENTS */
	XA_STRING, 0L, &nitems, &remain);

    if (rsrcstr == NULL) {
	if (homedir != NULL) {
	    (void) strcpy(filename, homedir);
	    (void) strcat(filename, "/.Xdefaults");
	    serverDB = XrmGetFileDatabase(filename);
	}
    } else {
	serverDB = XrmGetStringDatabase(rsrcstr);
	XFree(rsrcstr);
    }

    /* Now try XENVIRONMENT or $HOME/.Xdefaults-hostname. */

    if (envfile == NULL) {
	if (homedir != NULL) {
	    (void) strcpy(filename, homedir);
	    (void) strcat(filename, "/.Xdefaults-");
#ifdef SVR4
	    if (sysinfo(SI_HOSTNAME, hostname, sizeof(hostname)) != -1) {
#else
	    if (0 == gethostname(hostname, sizeof(hostname), &namelen)) {
#endif
		(void) strcat(filename, hostname);
		fileDB = XrmGetFileDatabase(filename);
	    }
	}
    } else {
	fileDB = XrmGetFileDatabase(envfile);
    }

    if (fileDB != NULL)
	XrmMergeDatabases(fileDB, &serverDB);

    return serverDB;
}


/* 
 * GetAppDefaults
 *
 * Gets the app-defaults file and return a database of its contents.  If we 
 * are running internationalized, looks in the following places
 *
 *	$OPENWINHOME/lib/locale/<locale>/app-defaults
 *	/usr/lib/X11/app-defaults/<locale>
 *	$OPENWINHOME/lib/app-defaults
 *	/usr/lib/X11/app-defaults
 *
 * If we are not running internationalized, the entries with <locale> are 
 * ignored.  Returns NULL if no app-defaults file is found.
 *
 * REMIND: this should use XFILESEARCHPATH.
 */
 
XrmDatabase
GetAppDefaults()
{
    XrmDatabase appDB = NULL;
    char filename[1024];
    char *openwinhome = getenv("OPENWINHOME");

#ifdef OW_I18N_L3
    char *locale;

    locale = GRV.lc_basic.locale;
    if (locale != NULL) {
	if (openwinhome != NULL) {
	    (void) sprintf(filename, "%s/lib/locale/%s/app-defaults/Olwm",
			   openwinhome, locale);
	    appDB = XrmGetFileDatabase(filename);
	    if (appDB != NULL)
		return appDB;
	}

	(void) sprintf(filename, "/usr/lib/X11/app-defaults/%s/Olwm", locale);
	appDB = XrmGetFileDatabase(filename);
	if (appDB != NULL)
	    return appDB;
    }
#endif

    if (openwinhome != NULL) {
	(void) strcpy(filename, openwinhome);
	(void) strcat(filename, "/lib/app-defaults/Olwm");
	appDB = XrmGetFileDatabase(filename);
	if (appDB != NULL)
	    return appDB;
    }

    appDB = XrmGetFileDatabase("/usr/lib/X11/app-defaults/Olwm");
    return appDB;
}


/* ===== global functions ================================================= */

/* 
 * GetDefaults
 *
 * XXX - this has been turned into just a call to InitGlobals().  Does it 
 * still need to exist?
 */
void
GetDefaults(dpy, commandlineDB)
    Display *dpy;
    XrmDatabase commandlineDB;
{
    InitGlobals(dpy, commandlineDB);
}
