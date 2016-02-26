#ifdef IDENT
#ident	"@(#)info.c	26.10	93/06/28 SMI"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */


#include <errno.h>
#include <stdio.h>
#include <memory.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "olwm.h"
#include "win.h"
#include "st.h"
#include "mem.h"
#include "error.h"


st_table	*wiHashTable;
st_table	*vHashTable;

static int
wiCompare( w1, w2 )
register char	*w1, *w2;
{
	return ((Window)w1) - ((Window)w2);
}

static int
wiHash( w1, modulus )
	register char	*w1;
	register int	modulus;
{
	return ((Window)w1) % modulus;
}

/*
 * initialize the hash tables
 * returns: True	- success
 *	    False	- failure
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
void
WIInit( dpy )
Display	*dpy;
{
	wiHashTable = st_init_table(wiCompare, wiHash);
	vHashTable = st_init_table(wiCompare, wiHash);
}

/*
 * Save window information with the associated window, for later dispatch
 */
void
WIInstallInfo(info)
WinGeneric *info;
{
	if (WIGetInfo(info->core.self) != NULL)
	{
		ErrorGeneral(
		  GetString("Tried to duplicate-register a window -- bailing"));
	}
	st_insert(wiHashTable, (int)info->core.self, (char *)info);
}

/*
 * delete storage for window information
 * returns: True	- window entry deleted
 *          False	- window entry not found
 */
Bool
WIUninstallInfo(win)
Window	win;
{
	WinGeneric *oldInfo;
	Window	tmpWin = win;
	Window  *tmpWinPtr = &tmpWin;
	Bool	ret;

	ret = st_delete(wiHashTable, (char *)tmpWinPtr, (char *)&oldInfo);
	if (oldInfo)
	    VUninstallInfo(oldInfo->core.virtual);
	return ret;
}

/*
 * retrieve information associated with a window
 * returns: Pointer to WinGeneric struct if window is found
 *	    NULL if not found
 */
WinGeneric *
WIGetInfo(win)
Window	win;
{
	WinGeneric *winInfo = NULL;

	st_lookup(wiHashTable, win, &winInfo);
	return winInfo;
}

/*
 * apply a function to all windows
 */
void
WIApply(f,c)
enum st_retval (*f)();
void *c;
{
	st_foreach(wiHashTable, f, c);
}

void
VInstallInfo(info)
WinGeneric *info;
{
	if (VGetInfo(info->core.virtual) != NULL)
	    ErrorGeneral(gettext("Tried to duplicate register a virtual window"));
	st_insert(vHashTable, (int) info->core.virtual, (char *) info);
}

/*
 * delete storage for window information
 * returns: True	- window entry deleted
 *          False	- window entry not found
 */
Bool
VUninstallInfo(win)
Window	win;
{
	WinGeneric *oldInfo;
	Window	tmpWin = win;
	Window  *tmpWinPtr = &tmpWin;

	return st_delete(vHashTable, (char *)tmpWinPtr, (char *)&oldInfo);
}

WinGeneric *
VGetInfo(win)
Window	win;
{
    WinGeneric	*winInfo = NULL;

    st_lookup(vHashTable, win, &winInfo);
    return winInfo;
}
