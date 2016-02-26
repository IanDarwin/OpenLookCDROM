/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/apex_options.c,v
 * 1.13 93/01/06 00:54:28 gounares Exp Locker: gounares $
 * 
 * $Log:	apex_options.c,v $
 * Revision 1.1  93/01/06  03:27:18  gounares
 * Initial revision
 * 
 */

/*
 * apex_options.c
 * 
 * Handle various user-defined options.
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#include <xview/xview.h>
#include <xview/defaults.h>
#include <string.h>

/*
 * get_tabsize -- width of the tab stop for textsw's
 */
int
get_tabsize()
{
	static int      size;

	if (!size)
		size = defaults_get_integer("apex.tabwidth", "apeX.tabwidth", 8);

	return size;
}

/*
 * get_filter -- the default file filter for the file_browser
 */
char           *
get_filter()
{
	static char    *szFilter;

	if (!szFilter)
		szFilter = strdup(defaults_get_string("apex.fileFilter", "apeX.fileFilter",
				"*.apex"));

	return szFilter;
}

/*
 * get_pattern -- the default pattern for files to be checked in/out
 */
char           *
get_pattern()
{
	static char    *szPattern;

	if (!szPattern)
		szPattern = strdup(defaults_get_string("apex.rcsPattern", "apeX.rcsPattern",
				"*.[chly] [Mm]akefile"));

	return szPattern;
}

/*
 * get_checkin -- the default program to run for rcs check in
 */
char           *
get_checkin()
{
	static char    *szCI;

	if (!szCI)
		szCI = strdup(defaults_get_string("apex.checkin", "apeX.checkin",
				"apex_ci"));

	return szCI;
}

/*
 * get_checkout -- the default program to run for rcs check out
 */
char           *
get_checkout()
{
	static char    *szCO;

	if (!szCO)
		szCO = strdup(defaults_get_string("apex.checkout", "apeX.checkout",
				"apex_co"));

	return szCO;
}

/*
 * get_datafile -- the default file for the navigator database
 */
char           *
get_datafile()
{
	static char    *szDatafile;

	if (!szDatafile)
		szDatafile = strdup(defaults_get_string("apex.datafile", "apeX.datafile",
				"apex.parse"));

	return szDatafile;
}

/*
 * get_nav_start -- the default function to start the call tree with
 */
char           *
get_nav_start()
{
	static char    *szStart;

	if (!szStart)
		szStart = strdup(defaults_get_string("apex.navStart", "apeX.navStart",
				"main"));

	return szStart;
}

/*
 * get_nav_3d -- whether or not to display the cool-o 3-d grid on the
 * Navigator
 */
int
get_nav_3d()
{
	static int      f = -1;

	if (f == -1)
		f = defaults_get_boolean("apex.nav3d", "apeX.nav3d", 1);

	return f;
}
