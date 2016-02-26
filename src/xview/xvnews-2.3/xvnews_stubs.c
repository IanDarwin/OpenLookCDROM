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
#include <ctype.h>
#include <sys/stat.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>
#define _OTHER_TEXTSW_FUNCTIONS /* For functions in textsw ... */
#include <xview/textsw.h>
#include <xview/window.h>
#include <xview/xv_xrect.h>
#include <xview/cursor.h>
#include <xview/notify.h>
#include <xview/font.h>
#include <gdd.h>
#include <gfm_ui.h>
#include <gfm.h>
#include <gio.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"

extern struct globals *Global;

STATIC_FUNCTION( int double_click, (Xv_opaque, struct timeval *, Xv_opaque,
				    struct timeval *));
STATIC_FUNCTION( int newGroups, (xvnews_xvnews_window_objects *));
STATIC_FUNCTION( int set_list_size, (Frame, Panel_item));

void
done_menu_proc(menu, item)
Menu            menu;
Menu_item       item;
{
        xvnews_xvnews_window_objects * ip = 
	  (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *select = (char *)xv_get(item, MENU_STRING);

	xvnews_err(ip, "Quitting %s...\n", Global->group);
	if (!strcmp(select, "update newsrc")) {
		if (Global->mode == ARTICLE_MODE) 
			update_newsrc(ip, 0);
	} else {
		int row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED);
	
		if (Global->mode == ARTICLE_MODE) {
			undeleteAll(ip);
			if (++row < xv_get(ip->groups_list,PANEL_LIST_NROWS, NULL))
				xv_set(ip->groups_list, PANEL_LIST_SELECT, row, TRUE, NULL);
		}
	}
	groups_set(ip);
}

void
kill_notify(menu, item)
Menu            menu;
Menu_item       item;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	kill_popup_objects *kp =
	  (kill_popup_objects *) xv_get(ip->xvnews_window,
					XV_KEY_DATA, KILL_POPUP, NULL);
        char *select;

        select = (char *)xv_get(item, MENU_STRING);

	if (!strcmp(select, "subject"))
		kill_subject(ip, 0);
	if (!strcmp(select, "author"))
		kill_subject(ip, 1);
	if (!strcmp(select, "Kill file...")) {
		xv_set(kp->kill_text, PANEL_CLIENT_DATA, 0, NULL);
		xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, "", NULL);
		xv_set(kp->save_butt, PANEL_CLIENT_DATA, NULL, NULL);
		xv_set(kp->file_list, PANEL_LIST_DELETE_ROWS, 0,
			xv_get(kp->file_list, PANEL_LIST_NROWS, NULL), NULL);
		xv_set(kp->kill_popup, XV_SHOW, TRUE, NULL);	
		xv_set(kp->kill_popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		fillKillText(ip, kp);
	}
}

void
next_menu_proc(menu, item)
Menu            menu;
Menu_item       item;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *select = (char *)xv_get(item, MENU_STRING);

	if (Global->mode != ALL_ARTICLES && Global->mode != ARTICLE_MODE)
		return;

	if (!strcmp(select, "next article")) {
		next_article_proc(ip, 1);
		return;
	}
	if (!strcmp(select, "next unread")) {
		int art = Global->article;

		Global->article = next_unread_art(ip);
		if (art == Global->article) {
			if (Global->mode == ARTICLE_MODE)
				update_newsrc(ip, 0);
			groups_set(ip);
		} else
			retrieve_article(ip, Global->article);
		return;
	}
	if (!strcmp(select, "next subject")) {
		int row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED);

		subject_search(ip, 1);
		if (row == xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED)) {
			if (Global->mode == ARTICLE_MODE)
				update_newsrc(ip, 0);
			groups_set(ip);
		}
		return;
	}
	if (!strcmp(select, "next author")) 
		author_search(ip, 1);
}

void
prev_menu_proc(menu, item)
Menu            menu;
Menu_item       item;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *select = (char *)xv_get(item, MENU_STRING);

	if (Global->mode != ALL_ARTICLES && Global->mode != ARTICLE_MODE)
		return;

	if (!strcmp(select, "prev article")) {
		next_article_proc(ip, 0);
		return;
	}
	if (!strcmp(select, "prev subject")) {
		subject_search(ip, 0);
		return;
	}
	if (!strcmp(select, "prev author")) {
		author_search(ip, 0);
	}
}

void
view_notify(menu, item)
Menu		menu;
Menu_item	item;
{
	xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *select;
		
	select = (char *)xv_get(item, MENU_STRING);

	if (!strcmp(select, "all groups"))
		getactive(ip, 0);
	if (!strcmp(select, "all subscribed groups"))
		getactive(ip, 1);
	if (!strcmp(select, "all unsubscribed groups"))
		getactive(ip, 2);
	if (!strcmp(select, "All matching groups...")) 
		init_groups_search(ip);
		
}

void
dismiss_view(menu, result)
Menu	menu;
Xv_opaque result;
{
	xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(menu, XV_KEY_DATA, INSTANCE);

	xv_set(menu, XV_SHOW, FALSE, NULL);
	if (Global->mode == ALL_GROUPS || Global->mode == NEW_GROUPS) {
		xv_set(ip->all_groups, XV_SHOW, FALSE, NULL);
		panel_paint(ip->controls1, PANEL_CLEAR);
	}
}

/*
 * Menu handler for `save_menu (save)'.
 */
Menu_item
save_article(item, op)
        Menu_item       item;
        Menu_generate   op;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	gfm_popup_objects               *gfm = (gfm_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SAVE_POPUP, NULL);
	char	file[128];
	char	*direct, *fil;

	struct stat statbuf;

        switch (op) {
        case MENU_DISPLAY:
                break;

        case MENU_DISPLAY_DONE:
                break;

        case MENU_NOTIFY:
		if (xv_get(gfm->popup, XV_SHOW)) {
			direct = (char *)xv_get(gfm->directory, PANEL_VALUE);
			fil = (char *)xv_get(gfm->file, PANEL_VALUE);
			sprintf(file, "%s/%s", direct, fil);
		} else {
		  sprintf(file, "%s", Global->newsdir);
		  if (stat(file, &statbuf) != 0) {
		    mkdir(file, 00755);
		  }
		  sprintf(file, "%s/%c%s", file,
			  toupper(Global->group->newsgroup[0]),
			  &Global->group->newsgroup[1]);
		}
		save_article_proc(gfm, file);
                break;

        case MENU_NOTIFY_DONE:
                break;
        }
        return item;
}

/*
 * Menu handler for `save_menu (File...)'.
 */
Menu_item
save_news(item, op)
        Menu_item       item;
        Menu_generate   op;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	gfm_popup_objects               *gfm = (gfm_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SAVE_POPUP, NULL);
static	int	up = 0;

        switch (op) {
        case MENU_DISPLAY:
                break;

        case MENU_DISPLAY_DONE:
                break;

        case MENU_NOTIFY:
		if (!up) {
		  gfm_activate(gfm, Global->newsdir, NULL, NULL,
			       save_file, (Xv_opaque)NULL, GFM_SAVE);
		  up = 1;
		} else
		  gfm_activate(gfm, NULL, NULL, NULL, save_file,
			       (Xv_opaque)NULL,	GFM_SAVE);
		xv_set(gfm->popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
                break;

        case MENU_NOTIFY_DONE:
                break;
        }
        return item;
}

/*
 * Menu handler for `save_menu (filter)'.
 */
Menu_item
filter_news(item, op)
        Menu_item       item;
        Menu_generate   op;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_props_objects *pp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);

        switch (op) {
        case MENU_DISPLAY:
		if (!strlen((char *)xv_get(pp->filter_text, PANEL_VALUE)))
			xv_set(item, MENU_INACTIVE, TRUE, NULL);
		else
			xv_set(item, MENU_INACTIVE, FALSE, NULL);
                break;

        case MENU_DISPLAY_DONE:
                break;

        case MENU_NOTIFY:
		print_article(ip, 0);
                break;
 
        case MENU_NOTIFY_DONE:
                break;
        }
        return item;
}

/*
 * Menu handler for `search_menu (subject next)'.
 */
Menu_item
subject_next_proc(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		subject_search(ip, 1);
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `search_menu (subject prev)'.
 */
Menu_item
subject_prev_proc(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		subject_search(ip, 0);
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `search_menu (author next)'.
 */
Menu_item
author_next(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		author_search(ip, 1);
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `search_menu (author prev)'.
 */
Menu_item
author_prev_proc(item, op)
	Menu_item	item;
	Menu_generate	op;
{
	xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch (op) {
	case MENU_DISPLAY:
		break;

	case MENU_DISPLAY_DONE:
		break;

	case MENU_NOTIFY:
		author_search(ip, 0);
		break;

	case MENU_NOTIFY_DONE:
		break;
	}
	return item;
}

/*
 * Menu handler for `search_menu (Search...)'.
 */
Menu_item
show_search_popup(item, op)
        Menu_item       item;
        Menu_generate   op;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_search_popup_objects * srp = (xvnews_search_popup_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, NULL);

        switch (op) {
        case MENU_DISPLAY:
                break;

        case MENU_DISPLAY_DONE:
                break;

        case MENU_NOTIFY:
		xv_set(srp->header_choice, XV_SHOW, TRUE, NULL);
        	xv_set(srp->header_text, XV_SHOW, TRUE, NULL);
        	xv_set(srp->srch_prev_butt, XV_SHOW, TRUE, NULL);
        	xv_set(srp->srch_next_button, XV_SHOW, TRUE, NULL);
        	xv_set(srp->show_groups_butt, XV_SHOW, FALSE, NULL);
		xv_set(srp->search_text, PANEL_VALUE, "", NULL);
        	xv_set(srp->search_popup, XV_LABEL, "Expression search", NULL);
		xv_set(srp->search_popup, XV_SHOW, TRUE, NULL);	
		xv_set(srp->search_popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		xv_set(srp->search_popup, FRAME_LEFT_FOOTER, "\n", NULL);
                break;

        case MENU_NOTIFY_DONE:
                break;
        }
        return item;
}

/*
 * Menu handler for `post_menu'.
 */
void
post_menu_proc(menu, item)
        Menu            menu;
        Menu_item   	item;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_post_popup_objects       *pp, *init_post_win();
	char	*select =	(char *)xv_get(item, MENU_STRING);

	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (!strncmp(select, "Post", 4)) {
		int flg = strcmp(select, "Post an article") ? NEWS_REPLY:NEWS_POST;

		if ((pp = init_post_win(ip, flg)) == NULL)
			return;
		get_news_header(pp, flg);
	} else {
		int flg = strcmp(select, "Forward article") ? MAIL_REPLY:MAIL_FORWARD;

		if ((pp = init_post_win(ip, flg)) == NULL)
			return;
		get_mail_header(pp, flg);
	}
	xv_set(pp->post_popup, FRAME_LABEL, select, NULL);
	xv_set(pp->post_popup, XV_SHOW, TRUE, NULL);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

void
done_groups_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int	nrows, i = 0;
	char	*old, group[96];
	
	xvnews_err(ip, "Retrieving subscribed groups...\n");
	get_groups(ip, Global->mode == NEW_GROUPS ? 1:0);
	if (strlen(Global->old_group)) {
		nrows = xv_get(ip->groups_list, PANEL_LIST_NROWS);
		for (; i < nrows; i++) {
			old=(char *)xv_get(ip->groups_list, PANEL_LIST_STRING, i);
			sscanf(old, "%s", group);
			if (!strcmp(group, Global->old_group))
				break;
		}
		xv_set(ip->groups_list, PANEL_LIST_SELECT, 0, FALSE, NULL);
		xv_set(ip->groups_list, PANEL_LIST_SELECT, i, TRUE, NULL);
	}
	groups_set(ip);
}

/*
 * Notify callback function for `quit_butt'.
 */
void
quit_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	xv_destroy_safe(ip->xvnews_window);
}

/*
 * Notify callback function for `rescan_butt'.
 */
void
rescan_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects *ip = 
	  (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int nrows = xv_get(ip->groups_list, PANEL_LIST_NROWS);	

	xvnews_err(ip, "Rescan in progress...\n");
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (save_newsrc())
		xvnews_err(ip, "Could not update %s!\n", Global->newsrc);
	if (!Global->nnrp || !Global->connected) {
		if (Global->connected)
			close_server();
		reconnect_server();
	}
	memset(Global->old_group, '\0', 64);
	if (Global->single == NULL && newGroups(ip)) {
		new_group_set(ip);
		Global->mode = NEW_GROUPS;
		xvnews_err(ip, "New group(s) found during rescan!\n");
	} else {
		get_groups(ip, 1);
		if (!nrows && !xv_get(ip->groups_list, PANEL_LIST_NROWS))
			freeMessages();
		xvnews_err(ip, "Rescan complete\n");
	}
	initIcon(ip);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

/*
 * Notify callback function for `update_butt'.
 */
void
w_newsrc_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	xvnews_err(ip, "Updating %s...\n", Global->newsrc);
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (save_newsrc())
		xvnews_err(ip,
			 "An error occured updating %s!\n", Global->newsrc);
	else
		xvnews_err(ip, "%s updated.\n", Global->newsrc);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

/*
 * Notify callback function for 'done', update .newsrc, iconify.
 */
void
update_close_proc(menu, item)
  Menu menu;
  Menu_item item;
{
  xvnews_xvnews_window_objects *ip = 
    (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  char *select = (char *)xv_get(item, MENU_STRING);

  w_newsrc_proc(item, (Event *)NULL);

  if (!strncmp(select, "quit", 4)) {
    exit(0);
  }
  else{
    xv_set(ip->xvnews_window, FRAME_CLOSED, TRUE, NULL);
  }
  
}

/*
 * Notify callback function for `articles_list'.
 */
int
articles_sel(item, string, client_data, op, event)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	switch(op) {
	case PANEL_LIST_OP_DESELECT:
		break;

	case PANEL_LIST_OP_SELECT:
		sscanf(string, "%d", &Global->article);
		retrieve_article(ip, Global->article);
		break;

	case PANEL_LIST_OP_VALIDATE:
		break;

	case PANEL_LIST_OP_DELETE:
		break;
#ifdef DOESNT_WORK
	case PANEL_LIST_OP_DBL_CLICK:
	  break;
#endif		

	}
	return XV_OK;
}
/*
 * Notify callback function for `groups_list'.
 */
int
group_sel(item, string, client_data, op, event)
	Panel_item	item;
	char		*string;
	Xv_opaque	client_data;
	Panel_list_op	op;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	static Xv_opaque        last_sel = (Xv_opaque)NULL;
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};
	char groupname[BUFFERLEN];
	struct newsrc_node *curr = Global->head;

	sscanf(string, "%s", groupname);

	switch(op) {
	case PANEL_LIST_OP_DESELECT:
	  if (Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES) {
	    break;
	  }
	  
	  if (Global->mode != GROUP_MODE) {
	    for (curr = Global->head; curr; curr = curr->nextgroup) {
	      if (!strcmp(groupname, curr->newsgroup)) {
		break;
	      }
	    }
	    assert( curr );
	    Global->group = curr;
	    undescribe_group(ip);
	  }
	  break;

	case PANEL_LIST_OP_SELECT:
	  if (Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES) {
	    break;
	  }
	  
	  for (curr = Global->head; curr; curr = curr->nextgroup) {
	    if (!strcmp(groupname, curr->newsgroup)) {
	      break;
	    }
	  }

	  if (!curr) {
	    xvnews_err(ip, "Group cannot be found!\n");
	    return XV_OK;
	  }
	  
	  Global->group = curr;
	  
	  if (Global->mode != GROUP_MODE) {
	    describe_group(ip);
	    break;
	  }
	  xv_set(ip->groups_list, PANEL_CLIENT_DATA, client_data,
		 PANEL_PAINT, PANEL_NONE, NULL);
	  now = event_time(event);
	  if (double_click(last_sel, &then, client_data, &now)) {
	    xvnews_err(ip, "Retrieving unread articles from %s...\n",
		       Global->group->newsgroup);
	    xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	    switch (read_article(ip, 1)) {
	    case 0:
	      catchup_group(ip);
	      groups_set(ip);
	      xvnews_err(ip, "All articles killed!\n");
	      break;
	    case 1:
	      article_set(ip);
	      break;
	    case -1:
	      xv_set(ip->groups_list, PANEL_LIST_DELETE,
		     xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED),
		     NULL);
	      groups_set(ip);
	      xvnews_err(ip, "No articles available!\n");
	      break;
	    }
	  }
	  last_sel = client_data;
	  then = now;
	  break;

	case PANEL_LIST_OP_VALIDATE:
		break;

	case PANEL_LIST_OP_DELETE:
		break;
#ifdef DOESNT_WORK
	case PANEL_LIST_OP_DBL_CLICK:
	  break;
#endif	  
	}
	return XV_OK;
}

/*
 * Notify callback function for `next_butt'.
 */
void
next_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	next_article_proc(ip, 1);
}

/*
 * Notify callback function for `unsub_butt'.
 */
void
unsub_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	unsub_group(ip);
}

/*
 * Notify callback function for `all_butt'.
 */
void
all_articles_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	if (Global->mode != ALL_ARTICLES)
		read_article(ip, 0);
	else
		xvnews_err(ip, "All articles displayed.\n");
}

/*
 * Notify callback function for `print_butt'.
 */
void
print_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	print_article(ip, 1);
}

/*
 * Notify callback function for `mark_unread_butt'.
 */
void
mark_unread_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	unread_proc(ip);
}

Notify_value
rescan_timer(frame, which)
Frame	frame;
int which;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(frame, XV_KEY_DATA, INSTANCE);
	xvnews_props_objects *xp = (xvnews_props_objects *)xv_get(frame, XV_KEY_DATA, PROPS_POPUP, NULL);
	int nrows = xv_get(ip->groups_list, PANEL_LIST_NROWS);	
	int row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED);
	char	*old;

	if (Global->mode != GROUP_MODE ||
		 (Global->post_popups) || xv_get(xp->props, XV_SHOW, TRUE) ||
			!Global->connected)
		return NOTIFY_DONE;

	/* Only scan again when no news at all is available */
	if (nrows) {
		close_server();
		return NOTIFY_DONE;
	}

	xvnews_err(ip, "Automatic rescan in progress...\n");
	if (save_newsrc())
		printf("Could not update %s!\n", Global->newsrc);
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (row != -1) {
		old = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row);
		sscanf(old, "%s", Global->old_group);
	} else
		memset(Global->old_group, '\0', 64);
	if (!Global->nnrp) {
		close_server();
		reconnect_server();
	}
	if (Global->single == NULL && newGroups(ip)) {
		new_group_set(ip);
		Global->mode = NEW_GROUPS;
		xvnews_err(ip, "New group(s) found during rescan!\n");
	} else {
		get_groups(ip, 1);
		groups_set(ip);
		if (!nrows && !xv_get(ip->groups_list, PANEL_LIST_NROWS))
			freeMessages();
		xvnews_err(ip, "Rescan complete\n");
	}

	xv_set(ip->groups_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
	initIcon(ip);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);

	return NOTIFY_DONE;
}

STATIC int newGroups(ip)
xvnews_xvnews_window_objects    *ip;
{
	int i = 0, status, count = 0, groups = 0, match;
	char	message[MAX_MESSAGE_LEN], group[96], avail;
	struct newsrc_node *new = NULL, *head = Global->head, *last = NULL;

	put_server("LIST");
	status = get_server(message, sizeof(message));
	if (status != OK_GROUPS) {
		reconnect_server();
		put_server("LIST");
		status = get_server(message, sizeof(message));
		if (status != OK_GROUPS) 
			return 0;
	}
	while(*message != '.') {
		get_server(message, sizeof(message));
		if (*message != '.') {
#ifdef CNEWS_DONT_SHOW
			sscanf(message, "%*s%*d%*d%*c%c", &avail);
			if (avail != '=' && avail != 'x')
#endif
			++count;
		}
	}	
	while (head != NULL) {
		++groups;
		head = head->nextgroup;
	}
	if (groups == count)
		return 0;

	xv_set(ip->groups_list, XV_SHOW, FALSE, NULL);
	
	put_server("LIST");
	get_server(message, sizeof(message));
	while(*message != '.') {
		get_server(message, sizeof(message));
		sscanf(message, "%s%*d%*d%*c%c", group, &avail);
#ifdef CNEWS_DONT_SHOW
		if (avail == '=' || avail == 'x')
			continue;
#endif
		head = Global->head;
		match = 0;
		while (head != NULL) {
			if (!strcmp(group, head->newsgroup) || *message == '.') {
				match = 1;
				break;
			}
			head = head->nextgroup;
		}
		if (match)
			continue;
		if (last == NULL) {
			last = Global->head;
			while(last->nextgroup != NULL)
				last = last->nextgroup;
			xv_set(ip->groups_list, PANEL_LIST_DELETE_ROWS, 0,
				xv_get(ip->groups_list, PANEL_LIST_NROWS, NULL),
				NULL);
		}
		new = (struct newsrc_node *) malloc(sizeof(struct newsrc_node));
                new->nextgroup = NULL;
                new->artlist= NULL;
		new->newsgroup = strdup(group);
                new->subscribed = UNSUBSCRIBED;
                new->kill = NULL;
                new->description = NULL;
                new->articles = 0;
		last->nextgroup = new;
		last = new;
		sprintf(message, "%-64.64sunsubscribed", new->newsgroup);
		xv_set(ip->groups_list, PANEL_LIST_INSERT, i,
			PANEL_LIST_STRING, i++, message, NULL);
	}
	if (i) 
	{
	   xv_set(ip->groups_list, PANEL_LIST_FONTS, Global->listfont, NULL,
		  NULL);
	}
	
	xv_set(ip->groups_list, XV_SHOW, TRUE, NULL);
	
	return i;
}

/*
 * Notify callback function for `next_group_butt'.
 */
void
next_group_proc(item, event)
	Panel_item	item;
	Event		*event;
{
  xvnews_xvnews_window_objects	*ip =
    (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
  if (Global->single) {
    strcpy(Global->group->newsgroup, Global->single);
    xvnews_err(ip, "Retrieving all available articles in %s...\n",
	       Global->group->newsgroup);
    xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
    
    if (Global->mode == NEW_GROUPS) {
      get_groups(ip, 1);
    }
    else {
      get_groups(ip, 0);
    }
        
    if (read_article(ip,0) > 0) {
      article_set(ip);
      Global->mode = ALL_ARTICLES;
    }
    else {
      groups_set(ip);
      xvnews_err(ip, "No articles available!\n");
    }
  }
  else {
    next_group(ip, 1);
  }
    
}

extern Notify_value undelete_popup_event(win, event, arg, type)
        Xv_window       win;
        Event           *event;
        Notify_arg      arg;
        Notify_event_type type;
{
	xvnews_undelete_popup_objects *up = (xvnews_undelete_popup_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);

	if (event_id(event) == WIN_RESIZE) {
		xv_set(up->undelete_list, PANEL_LIST_DISPLAY_ROWS,
			 set_list_size(up->undelete_popup, up->undelete_list),
			 NULL);
		xv_set(up->undelete_butt, XV_Y,
			 xv_get(up->controls3, XV_HEIGHT) - 30,
			 XV_X, (xv_get(up->controls3, XV_WIDTH) / 2) - 20, NULL);
		panel_paint(up->controls3, PANEL_NO_CLEAR);
	}

	return notify_next_event_func(win, (Notify_event) event, arg, type);
}

static int set_list_size(frame, list)
Frame	frame;
Panel_item	list;
{
        int             height_diff, height;
	int		win_height = xv_get(frame, XV_HEIGHT);
	int             row_height = xv_get(list, PANEL_LIST_ROW_HEIGHT);
	static	int		list_height = 0, list_excess = 0, win_init_height = 0;

	if (!list_height)
		list_height = xv_get(list, XV_HEIGHT);

	if (!list_excess)
		list_excess = list_height - (xv_get(list, PANEL_LIST_ROW_HEIGHT) *
			xv_get(list, PANEL_LIST_DISPLAY_ROWS)); 

	if (!win_init_height)
		win_init_height = xv_get(frame, XV_HEIGHT);

        height_diff = win_height - win_init_height;
        height =  list_height + height_diff;

	return (height - list_excess) / row_height;
}

/*
 * Event callback function for `xvnews_window'.
 */
Notify_value
main_win_proc(win, event, arg, type)
	Xv_window	win;
	Event		*event;
	Notify_arg	arg;
	Notify_event_type type;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);

	if (event_id(event) == WIN_RESIZE) {
		xv_set(ip->groups_list, PANEL_LIST_WIDTH, -1, NULL);
		xv_set(ip->articles_list, PANEL_LIST_WIDTH, -1, NULL);
		xv_set(ip->article_window, XV_HEIGHT, WIN_EXTEND_TO_EDGE, NULL);
	}

	if (event_id(event) == WIN_UNMAP_NOTIFY) {
		save_newsrc();
		initIcon(ip);
		xv_set(ip->rescan_butt, PANEL_CLIENT_DATA, FALSE, NULL);
	}

	if (event_id(event) == WIN_MAP_NOTIFY) {
		if (!Global->connected)
			rescan_proc(ip->rescan_butt, NULL);
		xv_set(ip->rescan_butt, PANEL_CLIENT_DATA, FALSE, NULL);
	}

	return notify_next_event_func(win, (Notify_event) event, arg, type);
}

/*
 * Notify callback function for `deliver_butt'.
 */
void
deliver_article(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_post_popup_objects	*ip = (xvnews_post_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	deliver_message(ip);
}

/*
 * Notify callback function for `cancel_butt'.
 */
void
cancel_post(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_post_popup_objects	*ip = (xvnews_post_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	cancel_message(ip);
}

void
sort_newsrc_proc(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	xvnews_err(ip, "Sorting newsrc...\n");
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	sort_newsrc();
	switch (Global->mode) {
        case ALL_GROUPS:
                getactive(ip, 0);
                break;
        case SUB_GROUPS:
                getactive(ip, 1);
                break;
        case UNSUB_GROUPS:
                getactive(ip, 2);
                break;
        case MATCH_GROUPS:
                break;
        }
	xvnews_err(ip, "Newsrc file sorted in alphabetical order.\n");
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

/*
 * Notify callback function for `post_button'.
 */
void
post_article(item, event)
	Panel_item	item;
	Event		*event;
{
	xvnews_xvnews_window_objects	*ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_post_popup_objects       *pp, *init_post_win();

	if ((pp = init_post_win(ip, NEWS_POST)) == NULL)
		return;
	get_news_header(pp, 0);
	xv_set(pp->post_popup, FRAME_LABEL, "Post an article", NULL);
	xv_set(pp->post_popup, XV_SHOW, TRUE, NULL);
}

/*
 * Event callback function for `post_window'.
 */
Notify_value
xvnews_text_event(win, event, arg, type)
	Xv_window	win;
	Event		*event;
	Notify_arg	arg;
	Notify_event_type type;
{
	if (event_action(event) == ACTION_DRAG_LOAD) {
		char	name[256];
		if (gdd_get_drag_name(win, name) != -1) 
			includeFile(win, name);
		return NOTIFY_DONE;
	}
	return notify_next_event_func(win, (Notify_event) event, arg, type);
}

Notify_value
news_drag(win, event, arg, type)
        Xv_window       win;
        Event           *event;
        Notify_arg      arg;
        Notify_event_type type;
{
        xvnews_xvnews_window_objects    *ip = (xvnews_xvnews_window_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);

        if (Global->mode != ARTICLE_MODE)
                return notify_next_event_func(win, (Notify_event) event, arg, type);
        if (event_is_up(event)) {
                Global->dragging = 0;
                Global->check_drag = 0;
        }

        if (event_id(event) == LOC_DRAG) {
                if (Global->dragging)
                        return NOTIFY_DONE;
                if (check_drag(ip, event)) {
                        xvnews_err(ip, "Dragging...\n");
                        if (getenv("NOSUNVIEW") == NULL)
                                drag_feedback(ip->xvnews_window, ip);
                        else
                                drag_dnd_drop(ip->xvnews_window, ip);
                }
        }
        return notify_next_event_func(win, (Notify_event) event, arg, type);
}

Notify_value
control_feedback(win, event, arg, type)
        Xv_window       win;
        Event           *event;
        Notify_arg      arg;
        Notify_event_type type;
{
        xvnews_xvnews_window_objects    *ip = (xvnews_xvnews_window_objects *) xv_get(win, XV_KEY_DATA, INSTANCE);
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	gfm_popup_objects               *gfm = (gfm_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SAVE_POPUP, NULL);

	if ((event_action(event) == ACTION_PROPS) && event_is_up(event)) {
		xv_set(prp->props, XV_SHOW, TRUE, NULL);
		xv_set(prp->props, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		return(NOTIFY_DONE);
	}
	if ((event_action(event)==ACTION_FIND_FORWARD) && event_is_up(event) && 
		(Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES)) {
			subject_search(ip, 1);
			return(NOTIFY_DONE);
	}
	if ((event_action(event)==ACTION_FIND_BACKWARD) && event_is_up(event) &&
		(Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES)) {
			subject_search(ip, 0);
			return(NOTIFY_DONE);
	}
	if (event_is_up(event)) {
		switch (event_id(event)) {
			case 'f': case ' ':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					int top, bottom, lines, total;
					
					total = textsw_file_line_count(ip->article_window, NULL);
					lines = textsw_screen_line_count(ip->article_window);
					textsw_file_lines_visible(ip->article_window, &top, &bottom);
					if (total > (bottom + 1)) {
						textsw_normalize_view(ip->article_window, textsw_index_for_file_line(ip->article_window, (top + lines) + 1));
						return(NOTIFY_DONE);
					}
				}
				/*
				} else 
					return(NOTIFY_DONE);
				*/
			case 'n':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					next_article_proc(ip, 1);
					return(NOTIFY_DONE);
				}
				if (Global->mode == GROUP_MODE) {
					next_group(ip, 1);
					return(NOTIFY_DONE);
				}
				break;
			case 'p':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
                                        next_article_proc(ip, 0);
                                        return(NOTIFY_DONE);
				}
				if (Global->mode == GROUP_MODE) {
					next_group(ip, 0);
					return(NOTIFY_DONE);
				}
				break;
			case 'b':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					int top, bottom, lines;
					
					lines = textsw_screen_line_count(ip->article_window);
					textsw_file_lines_visible(ip->article_window, &top, &bottom);
					if (top != 0) 
						textsw_normalize_view(ip->article_window, textsw_index_for_file_line(ip->article_window, (top - lines > 0 ? (top - lines) + 3:0) ));
					return(NOTIFY_DONE);
				}
				break;
			case 'q':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					xvnews_err(ip, "Quitting %s...\n",
						 Global->group->newsgroup);
					update_newsrc(ip, 0);
					groups_set(ip);
					return(NOTIFY_DONE);
				}
				break;
			case 's':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					char    file[128];
        				char    *direct, *fil;
        				struct stat statbuf;

					if (xv_get(gfm->popup, XV_SHOW)) {
                        			direct = (char *)xv_get(gfm->directory, PANEL_VALUE);
                        			fil = (char *)xv_get(gfm->file, PANEL_VALUE);
                        			sprintf(file, "%s/%s", direct, fil);
                			} else {
					  sprintf(file, "%s", Global->newsdir);
					  if (stat(file, &statbuf) != 0) {
					    mkdir(file, 00755);
					  }
					  sprintf(file, "%s/%c%s", file,
						  toupper(Global->group->newsgroup[0]),
						  &Global->group->newsgroup[1]);
                			}
                			save_article_proc(gfm, file);
                                        return(NOTIFY_DONE);
				}
                		break;
			case 'k':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					kill_subject(ip, 0);
                                        return(NOTIFY_DONE);
				}
				break;
			case 'S':
			  if (Global->mode == ARTICLE_MODE ||
			      Global->mode == ALL_ARTICLES) {
			    int choice = 
			      xv_get(prp->sort, PANEL_TOGGLE_VALUE, 0);
			    int art;
			    char *old;
					
			    xv_set(prp->sort, PANEL_VALUE, choice ? 0:1, NULL);
			    xvnews_err(ip, "Sorting Subjects...\n");
			    xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
			    if (choice) {
			      sortSubjects(ip, XV_SORT_SUBJECT);
			    }
			    else {
			      sortSubjects(ip, XV_SORT_ARTNUM);
			    }
			    old = (char *)xv_get(ip->articles_list,
						 PANEL_LIST_STRING, 0, NULL);
			    sscanf(old, "%d", &art);	
			    Global->article = art;
			    retrieve_article(ip, art);
			    xv_set(prp->sort, PANEL_VALUE, choice ? 1:0, NULL);
			  }
				break;
			case 'h':
				if (Global->mode == ARTICLE_MODE ||
					Global->mode == ALL_ARTICLES) {
					int art = xv_get(ip->articles_list,
						PANEL_LIST_FIRST_SELECTED);
					int choice = xv_get(prp->header_select,
						PANEL_TOGGLE_VALUE, 0);
					char *old;

					xv_set(prp->header_select, PANEL_VALUE,
						choice ? 0:1, NULL);
					old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, art, NULL);
					sscanf(old, "%d", &art);	
					retrieve_article(ip, art);
					xv_set(prp->header_select, PANEL_VALUE,
						choice ? 1:0, NULL);
                                        return(NOTIFY_DONE);
				}
				break;
				
		}
	}
	return notify_next_event_func(win, (Notify_event) event, arg, type);
}

/*
 * Notify callback function for `goto_button'.
 */
void
goto_group_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_xvnews_window_objects    *ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char groupname[BUFFERLEN];
	char *old;	
	struct newsrc_node *curr = Global->head;

	if (xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL) == -1)
		return;
	old = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING,
		xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL), NULL);
	sscanf(old, "%s", groupname);
	for (curr = Global->head; curr; curr = curr->nextgroup) {
	  if (!strcmp(groupname, curr->newsgroup))
	    break;
	}
	
	assert( curr );
	
	Global->group = curr;
	
	xvnews_err(ip, "Retrieving all available articles in %s...\n",
		 Global->group->newsgroup);
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (Global->mode == NEW_GROUPS)
                get_groups(ip, 1);
        else
                get_groups(ip, 0);
        if (read_article(ip, 0) > 0) {
                article_set(ip);
                Global->mode = ALL_ARTICLES;
        } else {
                groups_set(ip);
                xvnews_err(ip, "No articles available!\n");
        }
}

/*
 * Notify callback function for `subscribe_button'.
 */
void
subscribe_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_xvnews_window_objects    *ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	group_change(ip, 1);
}

/*
 * Notify callback function for `unsubscribe_button'.
 */
void
unsubscribe_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_xvnews_window_objects    *ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
 
        group_change(ip, 0);
}

/*
 * Notify callback function for `srch_prev_butt'.
 */
void
srch_prev_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_search_popup_objects     *ip = (xvnews_search_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

		exp_search(ip, 0);
}
 
/*
 * Notify callback function for `srch_next_button'.
 */
void
srch_next_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_search_popup_objects     *ip = (xvnews_search_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

		exp_search(ip, 1);
}

/*
 * Notify callback function for `display_text'.
 */
Panel_setting
change_display(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_props_objects    *ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *bp = (xvnews_xvnews_window_objects *) xv_get(xv_get(ip->props, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
        int     value = (int) xv_get(item, PANEL_VALUE);

	resize_panel(bp, value);
        return panel_text_notify(item, event);
}

Panel_setting
search_text_proc(item, event)
        Panel_item      item;
        Event           *event;
{
	xvnews_search_popup_objects     *srp = (xvnews_search_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *) xv_get(xv_get(srp->search_popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);

	if (Global->mode != ALL_ARTICLES && Global->mode != ARTICLE_MODE)
		getactive(ip, 3);

        return panel_text_notify(item, event);
}

/*
 * Event callback function for `search_text'.
 */
void
search_text_event(item, event)
        Panel_item      item;
        Event           *event;
{
	static	int	once = 0;
	char		*match, *find_approx_match();
	xvnews_search_popup_objects     *ip = (xvnews_search_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
        
	if (Global->mode != ALL_ARTICLES && Global->mode != ARTICLE_MODE &&
	   event_id(event) == 27 ) {
		if (!once) {
			once = 1;
        		panel_default_handle_event(item, event);
			return;
		}
		match = find_approx_match((char*)xv_get(item, PANEL_VALUE));
		once = 0;
		if (strlen(match))
			xv_set(item, PANEL_VALUE, match, NULL);
		else
			window_bell(ip->search_popup);
	}
		
        panel_default_handle_event(item, event);
}

/*
 * Notify callback function for `sort'. 
 */
void
sort_notify(item, value, event)
        Panel_item      item;
        int             value;
        Event           *event;
{
        xvnews_props_objects    *ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *bp = (xvnews_xvnews_window_objects *) xv_get(xv_get(ip->props, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	int art;
	char	*old;

        if (Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES) { 
		xvnews_err(bp, "Sorting Subjects...\n");
		xv_set(bp->xvnews_window, FRAME_BUSY, TRUE, NULL);
		if (value)
		  sortSubjects(bp, XV_SORT_SUBJECT);
		else
		  sortSubjects(bp, XV_SORT_ARTNUM);
                old = (char *)xv_get(bp->articles_list, PANEL_LIST_STRING, 0);
                sscanf(old, "%d", &art); 
		Global->article = art;
                retrieve_article(bp, art); 
        } 

}
        
/*
 * Notify callback function for `header_select'.
 */
void
change_header(item, value, event)
        Panel_item      item;
        int             value;
        Event           *event;
{
        xvnews_props_objects    *ip = (xvnews_props_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *bp = (xvnews_xvnews_window_objects *) xv_get(xv_get(ip->props, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	int art;
	char	*old;
        
        if (Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES) { 
                old = (char *)xv_get(bp->articles_list, PANEL_LIST_STRING, 
                 xv_get(bp->articles_list,PANEL_LIST_FIRST_SELECTED,NULL),NULL);
                sscanf(old, "%d", &art); 
                retrieve_article(bp, art); 
        } 
}

void
header_notify(menu, item)
Menu		menu;
Menu_item	item;
{
	xvnews_search_popup_objects * ip = (xvnews_search_popup_objects *)xv_get(item, XV_KEY_DATA, INSTANCE);
	char *select;
		
	select = (char *)xv_get(item, MENU_STRING);

	xv_set(ip->header_text, PANEL_VALUE, select, NULL);
}

void
dist_notify(menu, item)
Menu            menu;
Menu_item       item;
{
	xvnews_post_popup_objects       *ip = (xvnews_post_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
        char	*select;
	char	line[80], dist[16];
	Textsw_index	pos, first = 0, last = TEXTSW_INFINITY, off;

        select = (char *)xv_get(item, MENU_STRING);
	sscanf(select, "%s", dist);
	sprintf(line, "Distribution: %s", dist);

	pos = xv_get(ip->post_window, TEXTSW_INSERTION_POINT, NULL);
	if (textsw_find_bytes(ip->post_window, &first, &last, "Distribution:", 13, 0) != -1) {
		xv_set(ip->post_window, TEXTSW_INSERTION_POINT, first, NULL);
                off = textsw_edit(ip->post_window, TEXTSW_UNIT_IS_LINE, 1, 0);
		textsw_insert(ip->post_window, line, strlen(line));
		if (strlen(line) != off)
			(int)strlen(line) - off > 0 ? (pos += (strlen(line) - off)):
				(pos -= (off - strlen(line))); 
		xv_set(ip->post_window, TEXTSW_INSERTION_POINT, pos, NULL);
	}
}

/*
 * Notify callback function for `show_groups_butt'.
 */
void
show_groups_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_search_popup_objects     *srp = (xvnews_search_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *) xv_get(xv_get(srp->search_popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);

	getactive(ip, 3);
}

/*
 * double_click --check for double click
 */
static int
double_click(last_sel, then, this_sel, now)
        Xv_opaque       last_sel;
        struct timeval  *then;
        Xv_opaque       this_sel;
        struct timeval  *now;
{
        struct timeval  delta;
 
        if (this_sel != last_sel)
                return 0;
 
        delta.tv_sec = now->tv_sec - then->tv_sec;
        if ((delta.tv_usec = now->tv_usec - then->tv_usec) < 0) {
                delta.tv_usec += 1000000;
                delta.tv_sec -= 1;
        }
 
        /*
         * Compare delta against multiclick timeout.
         */
        return (delta.tv_sec*10 + delta.tv_usec/100000) <= Global->multiclick;
}

extern int load_file(gfm, direct, file)
gfm_popup_objects       *gfm;
char    *direct, *file;
{
  xvnews_post_popup_objects *pp = (xvnews_post_popup_objects *)
    xv_get(xv_get(gfm->popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
  char	name[256];

  sprintf(name, "%s/%s", direct, file);
  includeFile(pp->post_window, name);

  return GFM_OK;
}

extern int save_file(gfm, direct, file)
gfm_popup_objects       *gfm;
char    *direct, *file;
{
	char	path[80];
	struct stat statbuf;

	sprintf(path, "%s/%s", direct, file);

	if (stat(path, &statbuf) == 0)
		chdir(path);

	save_article_proc(gfm, path);
	return GFM_OK;
}

/*
 * Notify callback function for `prev_butt'.
 */
void
prev_proc(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_xvnews_window_objects    *ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

        next_article_proc(ip, 0);
}

void
include_art(menu, item)
Menu            menu;
Menu_item       item;
{
        xvnews_post_popup_objects * ip = (xvnews_post_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	gfm_popup_objects               *gfm = (gfm_popup_objects *) xv_get(ip->post_popup, XV_KEY_DATA, LOAD_POPUP, NULL);
	xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(ip->post_popup, XV_KEY_DATA, POST_PARENT, NULL);
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(bp->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
        char *select, *indent = (char *)xv_get(prp->indent_text, PANEL_VALUE, NULL);
	char *old, *err, buff[256];
	int	art;

        select = (char *)xv_get(item, MENU_STRING);
		
	if (!strncmp(select, "article", 7)) {
		if (Global->mode != ARTICLE_MODE && Global->mode != ALL_ARTICLES)
			return;

		old = (char *)xv_get(bp->articles_list, PANEL_LIST_STRING,
			xv_get(bp->articles_list, PANEL_LIST_FIRST_SELECTED), NULL);
		sscanf(old, "%d", &art);
		sprintf(buff, "In article <%s>, %s writes:\n", get_messageid(0),
			currentAuthor(0));
		textsw_insert(ip->post_window, buff, strlen(buff));
		if ((err = include_news(art, "/tmp/.xvnews.include",
		     strcmp(select, "article") ? indent:"")) != NULL)
			xv_set(ip->post_popup, FRAME_LEFT_FOOTER, err, NULL);
		else {
			xv_set(ip->post_window, TEXTSW_LOWER_CONTEXT, -1, NULL);
                        xv_set(ip->post_window,
                                TEXTSW_INSERT_FROM_FILE,    "/tmp/.xvnews.include",
                                NULL);
                        xv_set(ip->post_window, TEXTSW_LOWER_CONTEXT, 0, NULL);
                        unlink("/tmp/.xvnews.include");
		}
	} else {
		gfm_activate(gfm, Global->home, NULL, NULL, load_file,
			     (Xv_opaque)NULL, GFM_LOAD);
		xv_set(gfm->popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
	}
}

/*
 * Menu handler for `catchup_menu (up to selected article)'.
 */
Menu_item
all_articles_menu(item, op)
        Menu_item       item;
        Menu_generate   op;
{
  char str[160], *ptr, *lastchar;
  xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) 
    xv_get(item, XV_KEY_DATA, INSTANCE);
  int row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL); 
  struct newsrc_node *curr = Global->head;


  switch (op) {
  case MENU_DISPLAY:
    if (Global->mode == ALL_ARTICLES)
      xv_set(item, MENU_INACTIVE, TRUE, NULL);
    else
      xv_set(item, MENU_INACTIVE, FALSE, NULL);
    break;

  case MENU_DISPLAY_DONE:
    break;

  case MENU_NOTIFY:
    if (row == -1)
      break;
    /* Copy the current row into str, and cut it off so that only the
       group name is there. We need this to compare with the group names */
    ptr = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row, NULL);
    strcpy(str, ptr);
    lastchar = strchr(str, ' ');
    if (lastchar)
      *lastchar = '\0';

    if (Global->mode == GROUP_MODE) {
      for (curr = Global->head; curr; curr = curr->nextgroup) {
	if (curr->subscribed == SUBSCRIBED) {
	  if (!strcmp(str, curr->newsgroup)) {
	    break;
	  }
	}
      }
      assert( curr );
      Global->group = curr;

      if (notice_prompt(ip->controls1, NULL,
			NOTICE_MESSAGE_STRINGS, "Confirm catchup to ",
			Global->group->newsgroup, NULL,
			NOTICE_BUTTON_YES,      "catchup",
			NOTICE_BUTTON_NO,       "cancel",
			NULL))
	catchup_group(ip);
      Global->group = NULL;
      return item;
    }
    update_newsrc(ip, 1);
    if (save_newsrc())
      xvnews_err(ip, "Could not update .newsrc file!");
    groups_set(ip);
    break;

  case MENU_NOTIFY_DONE:
    break;
  }
  
  return item;
}

/*
 * Menu handler for `catchup_menu (up to selected article)'.
 */
Menu_item
selected_article_menu(item, op)
        Menu_item       item;
        Menu_generate   op;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char	*old;
	int	art;

        switch (op) {
        case MENU_DISPLAY:
		switch(Global->mode) {
			case ALL_ARTICLES:
				xv_set(item, MENU_INACTIVE, TRUE, NULL);
				break;
			case ARTICLE_MODE:
				xv_set(item, MENU_INACTIVE, FALSE, NULL);
				break;
			case GROUP_MODE:
				xv_set(item, MENU_INACTIVE, TRUE, NULL);
				break;
		}
                break;

        case MENU_DISPLAY_DONE:
                break;

        case MENU_NOTIFY:
		old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
		   xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL),
			 NULL);
		if (old == NULL)
			return (Menu_item)NULL;
		sscanf(old, "%d", &art);
		update_newsrc(ip, art);
		groups_set(ip);
                break;

        case MENU_NOTIFY_DONE:
                break;
        }
        return item;
}

extern int check_drag(ip, event)
xvnews_xvnews_window_objects    *ip;
Event   *event;
{
        static int      x_down, y_down;
        int     wd, newx, newy;

        newx = event_x(event);
        newy = event_y(event);

        if (!Global->check_drag) {
                x_down = newx;
                y_down = newy;
                Global->check_drag = 1;
                return 0;
        } else {
                wd = xv_get(ip->articles_list,XV_WIDTH);
		/* Check to see if we have moved more than 20 pixels.
		   Also check to see we just didn't move the scrollbar.
		 */
                if (((abs(newx - x_down) > 20) || (abs(newy - y_down) > 20)) &&
                        (newx > 40 && newx < (wd - 40))) {
                                Global->check_drag = 0;
                                return 1;
                } else
                                return 0;
        }
}

/*
 * Notify callback function for `add_local_butt'.
 */
void
add_local_func(item, event)
        Panel_item      item;
        Event           *event;
{
        kill_popup_objects *kp = (kill_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *) xv_get(xv_get(kp->kill_popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	char	*err, *exp = (char *)xv_get(kp->kill_text, PANEL_VALUE, NULL);
	int	mod = xv_get(kp->mod_choice, PANEL_VALUE, NULL);
	int 	command = xv_get(kp->command_choice, PANEL_VALUE, NULL);
	struct kill_node	*kill = NULL;

	err = xvnews_comp(exp);
	
	if (err != NULL) {
		xvnews_err(ip, "Expression error: %s for \"%s\"!\n", err, exp);
		return;
	}

	addKill(kp, exp, Global->group->newsgroup, mod, command);

	if (!command || !xv_get(kp->changes_choice, PANEL_VALUE, NULL))
		return;

	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	kill = (struct kill_node *)malloc(sizeof(struct kill_node));
	kill->string = strdup(exp);
	kill->subject = mod;
	kill->junk = 1;
	kill->place = 0;
	kill->next = NULL;

	if (mod)
		sessionSubjectKill(ip, kp, kill);	
	else
		sessionHeaderKill(ip, kp, kill);

	free(kill->string);
	free(kill);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

/*
 * Notify callback function for `add_global_butt'.
 */
void
add_global_func(item, event)
        Panel_item      item;
        Event           *event;
{
        kill_popup_objects *kp = (kill_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *) xv_get(xv_get(kp->kill_popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	char    *err, *exp = (char *)xv_get(kp->kill_text, PANEL_VALUE, NULL);
        int     mod = xv_get(kp->mod_choice, PANEL_VALUE, NULL);
        int     command = xv_get(kp->command_choice, PANEL_VALUE, NULL);
	struct kill_node	*kill = NULL;

	err = xvnews_comp(exp);
	if (err != NULL) {
		xvnews_err(ip, "Expression error: %s for \"%s\"!\n", err, exp);
		return;
	}

	addKill(kp, exp, "", mod, command);
	if (!command || !xv_get(kp->changes_choice, PANEL_VALUE, NULL))
		return;

	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	kill = (struct kill_node *)malloc(sizeof(struct kill_node));
	kill->string = strdup(exp);
	kill->subject = mod;
	kill->junk = 1;
	kill->place = 0;
	kill->next = NULL;

	if (mod)
		sessionSubjectKill(ip, kp, kill);	
	else
		sessionHeaderKill(ip, kp, kill);

	free(kill->string);
	free(kill);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

/*
 * Notify callback function for `kill_choice'.
 */
void
kill_string_func(menu, item)
Menu		menu;
Menu_item	item;
{
        kill_popup_objects *kp = (kill_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *) xv_get(xv_get(kp->kill_popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	char *select = (char *)xv_get(item, MENU_STRING);

	if (!strcmp(select, "subject")) 
		xv_set(kp->kill_text, PANEL_CLIENT_DATA, 0, NULL);
	if (!strcmp(select, "author")) 
		xv_set(kp->kill_text, PANEL_CLIENT_DATA, 1, NULL);

	fillKillText(ip, kp);
}

/*
 * Notify callback function for `undelete_butt'.
 */
void
undelete_func(item, event)
        Panel_item      item;
        Event           *event;
{
        xvnews_undelete_popup_objects *up = 
	  (xvnews_undelete_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_xvnews_window_objects *ip =
	  (xvnews_xvnews_window_objects *)
	  xv_get(xv_get(up->undelete_popup, XV_OWNER, NULL),
		 XV_KEY_DATA, INSTANCE, NULL);
	xvnews_props_objects *prp =
	  (xvnews_props_objects *) xv_get(ip->xvnews_window,
					  XV_KEY_DATA, PROPS_POPUP, NULL);
	int row = xv_get(up->undelete_list, PANEL_LIST_FIRST_SELECTED, NULL);
	char *str;
	int num, new = 0;
	struct newsrc_node *curr = Global->head;
	
	if (row == -1)
		return;

	curr = Global->group;
	
	xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
	while (row != -1) {
		str = (char *)xv_get(up->undelete_list, PANEL_LIST_STRING, row, NULL);
		sscanf(str, "%d", &num);
		add_unread(curr, num);
		new = undeleteKill(ip, prp, str, num);
		xv_set(up->undelete_list, PANEL_LIST_DELETE, row, NULL);
		row = xv_get(up->undelete_list,PANEL_LIST_FIRST_SELECTED,NULL);
	}
	if (xv_get(prp->sort, PANEL_TOGGLE_VALUE, 0)) {
		int nrows = xv_get(ip->articles_list, PANEL_LIST_NROWS), art;

		sortSubjects(ip, XV_SORT_SUBJECT);
		for (new = 0; new < nrows; new++) {
			str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
				new, NULL);
			sscanf(str, "%d", &art);
			if (num == art)
				break;
		}
	}
	xv_set(ip->articles_list, XV_SHOW, TRUE, NULL);
	xv_set(ip->articles_list, PANEL_LIST_SELECT, new, TRUE, NULL);
	Global->article = num;
	retrieve_article(ip, num);
}

/*
 * Menu handler for `kill_menu (Undelete...)'.
 */
Menu_item
undelete_menu_proc(item, op)
        Menu_item       item;
        Menu_generate   op;
{
        xvnews_xvnews_window_objects * ip = (xvnews_xvnews_window_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	xvnews_undelete_popup_objects               *up = (xvnews_undelete_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, NULL);

        switch (op) {
        case MENU_DISPLAY:
		if (xv_get(up->undelete_list, PANEL_LIST_NROWS, NULL))
			xv_set(item, MENU_INACTIVE, FALSE, NULL);
		else
			xv_set(item, MENU_INACTIVE, TRUE, NULL);
                break;

        case MENU_DISPLAY_DONE:
                break;

        case MENU_NOTIFY:
	  /* This needs to be done in case we are forced a non-show
	     of this window by leaving the list SHOW FALSE
	     (see xv_kill.c, func: articleKill()) */
	  xv_set(up->undelete_list, XV_SHOW, TRUE, NULL);

		xv_set(up->undelete_popup, PANEL_LIST_SELECT, 0, TRUE, NULL);
		xv_set(up->undelete_popup, XV_SHOW, TRUE, NULL);
		xv_set(up->undelete_popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		xv_set(up->undelete_popup, PANEL_LIST_SELECT, 0, FALSE, NULL);
                break;

        case MENU_NOTIFY_DONE:
                break;
        }
        return item;
}

/*
 * Notify callback function for `delete_kill_butt'.
 */
void
delete_kill_func(item, event)
        Panel_item      item;
        Event           *event;
{
        kill_popup_objects *kp = (kill_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	int row = xv_get(kp->file_list, PANEL_LIST_FIRST_SELECTED, NULL);

	while (row != -1) {
		xv_set(kp->file_list, PANEL_LIST_DELETE, row, NULL);
		row = xv_get(kp->file_list, PANEL_LIST_FIRST_SELECTED, NULL);
		xv_set(kp->delete_kill_butt, PANEL_CLIENT_DATA, 1, NULL);
	}
}

/*
 * Notify callback function for `save_butt'.
 */
void
save_kill_func(item, event)
        Panel_item      item;
        Event           *event;
{
        kill_popup_objects *kp = (kill_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	saveKillFile(kp);
}

/*
 * Notify callback function for `kill_file_menu'.
 */
void
kill_file_func(menu, item)
Menu		menu;
Menu_item	item;
{
        kill_popup_objects *kp = (kill_popup_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *select = (char *)xv_get(item, MENU_STRING);

	if (!strcmp(select, "group"))
		loadKill(kp, Global->group->newsgroup);
	else
		loadKill(kp, "");
}
