/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <X11/Xos.h>
#include <stdio.h>
#include <unistd.h>
#include <locale.h>
#include <xview/xview.h>
#include <xview/cursor.h>
#include <xview/panel.h>
#include <gfm.h>
#include <gfm_ui.h>
#include <gio.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "patchlevel.h"

Attr_attribute	INSTANCE;
Attr_attribute  POST_PARENT;
Attr_attribute  PROPS_POPUP;
Attr_attribute  SAVE_POPUP;
Attr_attribute  POST_POPUP;
Attr_attribute  POST_TYPE;
Attr_attribute  SEARCH_POPUP;
Attr_attribute  KILL_POPUP;
Attr_attribute  UNDELETE_POPUP;
Attr_attribute  LOAD_POPUP;

struct globals	*Global;

STATIC_FUNCTION(void xvnews_usage, ());

void main(argc, argv)
int		argc;
char		**argv;
{
  extern char *optarg;
  
	xvnews_xvnews_window_objects	*ip;
	xvnews_props_objects		*prp;
	xvnews_search_popup_objects     *srp;
	xvnews_undelete_popup_objects     *up;
	kill_popup_objects	*kp;
	gfm_popup_objects      		*gfm;
	int	c;

	setlocale(LC_CTYPE, DEFAULT_LOCALE);
	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);
	INSTANCE = xv_unique_key();

	/* Initialize the instance pointers. These are defined in xvnews_ui.h */
	ip = xvnews_xvnews_window_objects_initialize(NULL, NULL);
	prp = xvnews_props_objects_initialize(NULL, ip->xvnews_window);
	gfm = gfm_initialize(NULL, ip->xvnews_window, "File Saver");
	srp = xvnews_search_popup_objects_initialize(NULL, ip->xvnews_window);
	kp = kill_popup_objects_initialize(NULL, ip->xvnews_window);
	up = xvnews_undelete_popup_objects_initialize(NULL, ip->xvnews_window);

	init_globals(ip);

	/* Get the command line args */
        while ((c = getopt(argc, argv, "hus:")) != -1)
                switch(c) {
		case 'h':
		  xvnews_usage();
		  break;
                case 's':
		  Global->single = strdup(optarg);
		  break;
		case 'u':
		  Global->undelete_popup = TRUE;
		  break;
                }

	/* Malloc and zero out the structure that contains our globals. */
	initGlobalValues();

	/* Connect to the news server. */
	connect_server(get_nntp_server());

	/* Set up a signal handler for a stale socket connection. */
	notify_set_signal_func(ip->xvnews_window, signal_handler, SIGPIPE, NOTIFY_ASYNC);

	/* Set the scrolling list to use a fixed width font. */
	init_list_font(ip);

	/* Parse the .newsrc file. */
	check_newsrc(ip);
	
	/* Give us a way to get to other frames. */
	POST_PARENT = xv_unique_key();
	PROPS_POPUP = xv_unique_key();
	SAVE_POPUP = xv_unique_key();
	POST_POPUP = xv_unique_key();
	POST_TYPE = xv_unique_key();
	SEARCH_POPUP = xv_unique_key();
	KILL_POPUP = xv_unique_key();
	UNDELETE_POPUP = xv_unique_key();
	LOAD_POPUP = xv_unique_key();

	xv_set(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, prp, NULL);
	xv_set(ip->xvnews_window, XV_KEY_DATA, SAVE_POPUP, gfm, NULL);
	xv_set(ip->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, srp, NULL);
	xv_set(ip->xvnews_window, XV_KEY_DATA, KILL_POPUP, kp, NULL);
	xv_set(ip->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, up, NULL);

	/* Set up the tool by checking the users defaults. */
	apply_defaults(ip);

	/* Set up the auto rescan timer for new articles */
	init_rescan_timer(ip);

	/* Setup the copy cursor */
	init_cursor();

	/* Check to see if we have new groups. */ 
	if (Global->mode == GROUP_MODE) {
		get_groups(ip, 1);
		groups_set(ip);
		if (Global->single) {
		  /* Go directly to articles */
		  next_group(ip,1);
		}
	} else {
		new_group_set(ip);
		describe_group(ip);
		xvnews_err(ip, "New groups available\n");
	}

	/* Set up the window labels and the text defaults. */
	init_labels(ip);

	/* Set up a destroy func */
	init_destroy_func(ip);
	
	initIcon(ip);

	xv_main_loop(ip->xvnews_window);
}


STATIC void
xvnews_usage() 
{
#ifdef BETA
  printf("xvnews BETA version %-1.1f", VERSION);
#else
  printf("xvnews version %-1.1f", VERSION);
#endif
  printf("\n\nCommand-line options:\n\n");
  printf("-h           This help text\n");
  printf("-u           Display Undelete popup window when available\n");
  printf("-s newsgroup Read only this newsgroup\n\n");
  xv_usage();
}

  
