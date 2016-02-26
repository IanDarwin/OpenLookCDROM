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

#include <stdio.h>
#include <unistd.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/xv_xrect.h>
#include <xview/notify.h>
#include <xview/text.h>
#include <gfm_ui.h>
#include <gfm.h>
#include <gio.h>

#include "xvnews_ui.h"
#include "xvnews.h"

STATIC_FUNCTION( void initSingleArticle, (xvnews_xvnews_window_objects *));
STATIC_FUNCTION( void initSingleGroup, (xvnews_xvnews_window_objects *));
STATIC_FUNCTION( void mult_group, (xvnews_xvnews_window_objects *));

extern void init_list_font(ip)
	xvnews_xvnews_window_objects	*ip;
{
	Global->listfont = (Xv_font)xv_find(ip->xvnews_window, FONT,
		FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
		NULL);
	xv_set(ip->controls2, WIN_FONT, Global->listfont, NULL);
}

extern void article_set(ip)
xvnews_xvnews_window_objects    *ip;
{
	xvnews_search_popup_objects     *srp = (xvnews_search_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, NULL);
	xvnews_undelete_popup_objects     *up = (xvnews_undelete_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, NULL);

	xv_set(srp->search_popup, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(srp->search_popup, XV_SHOW, FALSE, NULL);
	xv_set(ip->next_group_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->unsubscribe_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->quit_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->sort_newsrc_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->post_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->update_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->all_groups, XV_SHOW, FALSE, NULL);
	xv_set(ip->subscribe_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->done_groups_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->goto_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->properties_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->rescan_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->catchup_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->unsub_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->print_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->mark_unread_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->all_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->post_men_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->save_article_button, XV_SHOW, TRUE, NULL);
	xv_set(ip->kill_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->prev_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->next_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->search_butt, XV_SHOW, TRUE, NULL);
	Global->mode = ARTICLE_MODE;
	xv_set(ip->articles_list, XV_SHOW, TRUE, NULL);
	xv_set(ip->groups_list, XV_SHOW, FALSE, NULL);
	xv_set(ip->done_butt, XV_SHOW, TRUE, NULL);
	if (xv_get(up->undelete_list, PANEL_LIST_NROWS, NULL)
	    && (Global->undelete_popup)) {
		xv_set(up->undelete_popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
		xv_set(up->undelete_popup, XV_SHOW, TRUE, NULL);
		xv_set(up->undelete_popup, PANEL_LIST_SELECT, 0, TRUE, NULL);
		xv_set(up->undelete_popup, PANEL_LIST_SELECT, 0, FALSE, NULL);
	}
	if (Global->single != NULL)
		initSingleArticle(ip);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

extern void groups_set(ip)
xvnews_xvnews_window_objects    *ip;
{
	char	file[80];
	xvnews_search_popup_objects     *srp = (xvnews_search_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, NULL);
	kill_popup_objects     *kp = (kill_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, KILL_POPUP, NULL);
	xvnews_undelete_popup_objects     *up = (xvnews_undelete_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, NULL);
	gfm_popup_objects               *gfm = (gfm_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SAVE_POPUP, NULL);

	if (xv_get(kp->save_butt, PANEL_CLIENT_DATA) != (Xv_opaque)NULL &&
                xv_get(kp->delete_kill_butt, PANEL_CLIENT_DATA))
                        confirmKill(kp);
	xv_set(ip->search_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->next_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->unsubscribe_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->sort_newsrc_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->prev_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->post_men_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->kill_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->done_groups_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->done_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->subscribe_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->goto_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->save_article_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->print_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->mark_unread_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->all_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->rescan_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->properties_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->unsub_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->update_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->post_button, XV_SHOW, TRUE, NULL);
	xv_set(ip->catchup_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->next_group_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->all_groups, XV_SHOW, TRUE, NULL);
	xv_set(ip->post_button, XV_SHOW, TRUE, NULL);
	xv_set(ip->groups_list, PANEL_CHOOSE_ONE, TRUE, NULL);
	xv_set(ip->groups_list, PANEL_CHOOSE_NONE, FALSE, NULL);
	xv_set(ip->quit_butt, XV_SHOW, TRUE, NULL);
	textsw_reset(ip->article_window, 0, 0);
	Global->group = NULL;
	sprintf(file, "/tmp/.xvnews.file.%d", getpid());
	unlink(file);
	if (Global->mode == ALL_ARTICLES && xv_get(ip->mark_unread_butt, PANEL_CLIENT_DATA)) {
		get_groups(ip, 0);
		xv_set(ip->mark_unread_butt, PANEL_CLIENT_DATA, 0, NULL);
	} else
		xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
	Global->mode = GROUP_MODE;
	xv_set(ip->articles_list, PANEL_LIST_DELETE_ROWS, 0,
		xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL), NULL);
	xv_set(up->undelete_list, PANEL_LIST_DELETE_ROWS, 0,
		xv_get(up->undelete_list, PANEL_LIST_NROWS, NULL), NULL);
	if (xv_get(up->undelete_popup, XV_SHOW, NULL)) {
		xv_set(up->undelete_popup, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
		xv_set(up->undelete_popup, XV_SHOW, FALSE, NULL);
	}
	if (xv_get(kp->kill_popup, XV_SHOW, NULL)) {
		xv_set(kp->kill_popup, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
		xv_set(kp->kill_popup, XV_SHOW, FALSE, NULL);
	}
	if (xv_get(gfm->popup, XV_SHOW, NULL)) {
		xv_set(gfm->popup, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
		xv_set(gfm->popup, XV_SHOW, FALSE, NULL);
	}
	if (xv_get(srp->search_popup, XV_SHOW, NULL)) {
		xv_set(srp->search_popup, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
		xv_set(srp->search_popup, XV_SHOW, FALSE, NULL);
	}
	xv_set(ip->groups_list, XV_SHOW, TRUE, NULL);
	if (Global->single != NULL)
		initSingleGroup(ip);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
	if (xv_get(ip->groups_list, PANEL_LIST_NROWS))
	  if (Global->single) {
	    xvnews_err(ip, "Reading articles in group '%s'\n", Global->single);
	  }
	  else {
	    xvnews_err(ip, "Subscribed groups with unread articles\n");
	  }
	else {
	  xvnews_err(ip, "No groups with unread articles\n");
	}
	init_labels(ip);

}

STATIC void initSingleGroup(ip)
xvnews_xvnews_window_objects    *ip;
{
	xv_set(ip->all_groups, XV_SHOW, FALSE, NULL);
	xv_set(ip->catchup_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->post_button, XV_SHOW, TRUE, NULL);
	xv_set(ip->unsub_butt, XV_SHOW, FALSE, NULL);
}

STATIC void initSingleArticle(ip)
xvnews_xvnews_window_objects    *ip;
{
	xv_set(ip->post_men_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->kill_butt, XV_SHOW, TRUE, NULL);
	xv_set((Menu)xv_get(ip->save_article_button, PANEL_ITEM_MENU, NULL), MENU_DEFAULT, 4, NULL);
	xv_set(ip->unsub_butt, XV_SHOW, FALSE, NULL);
}

extern void new_group_set(ip)
xvnews_xvnews_window_objects    *ip;
{
	char	line[120];

	xv_set(ip->next_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->save_article_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->quit_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->done_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->catchup_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->prev_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->unsub_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->print_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->kill_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->search_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->mark_unread_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->all_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->update_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->rescan_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->next_group_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->post_men_butt, XV_SHOW, FALSE, NULL);
	xv_set(ip->post_button, XV_SHOW, FALSE, NULL);
	xv_set(ip->all_groups, XV_SHOW, FALSE, NULL);
	xv_set(ip->properties_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->unsubscribe_button, XV_SHOW, TRUE, NULL);
	xv_set(ip->goto_button, XV_SHOW, TRUE, PANEL_INACTIVE, FALSE, NULL);
	xv_set(ip->groups_list, PANEL_CHOOSE_ONE, FALSE, NULL);
	xv_set(ip->done_groups_butt, XV_SHOW, TRUE, NULL);
	xv_set(ip->subscribe_button, XV_SHOW, TRUE, NULL);
	xv_set(ip->sort_newsrc_butt, XV_SHOW, TRUE, NULL);
	textsw_reset(ip->article_window, 0, 0);
	sprintf(line,
	 "Group\t\t\tArticles   Description\n-----\t\t\t--------   -----------\n\n");
	textsw_insert(ip->article_window, line, strlen(line));
	xv_set(ip->groups_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
	init_labels(ip);
}

extern void resize_panel(ip, num)
xvnews_xvnews_window_objects    *ip;
int num;
{
	int	listsize, controlsize;

	if (num > 24 || num < 1)
		return;
	xv_set(ip->articles_list, PANEL_LIST_DISPLAY_ROWS, num, NULL);
	xv_set(ip->groups_list, PANEL_LIST_DISPLAY_ROWS, num, NULL);
	
	listsize = xv_get(ip->articles_list, XV_HEIGHT, NULL);
	xv_set(ip->controls2, XV_HEIGHT, listsize, NULL);
	xv_set(ip->controls1, XV_Y, listsize, NULL);
	controlsize = xv_get(ip->controls1, XV_HEIGHT, NULL); 
	xv_set(ip->article_window, XV_Y, listsize + controlsize + 1, NULL);
	xv_set(ip->article_window, XV_HEIGHT, xv_get(ip->xvnews_window, XV_HEIGHT, NULL) - (listsize + controlsize), NULL);
}

static	int	Height = 400;

Notify_value
destroy_post(frame, status)
Frame   frame;
Destroy_status  status;
{
        if (status == DESTROY_CHECKING) {
                gfm_popup_objects               *gfm = (gfm_popup_objects *) xv_get(frame, XV_KEY_DATA, LOAD_POPUP, NULL);
                xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(frame, XV_KEY_DATA, POST_PARENT, NULL);

                xv_destroy(gfm->popup);
                xv_set(bp->xvnews_window, XV_KEY_DATA, POST_POPUP, NULL, NULL);
                Height = xv_get(frame, XV_HEIGHT); 
                Global->post_popups--;
        }

        return notify_next_destroy_func(frame, status);
}

xvnews_post_popup_objects *
init_post_win(ip, post)
xvnews_xvnews_window_objects    *ip;
int	post;
{
	Notify_value    destroy_post();
	xvnews_post_popup_objects       *pp;
	gfm_popup_objects               *gfm;
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	char	*editor = (char *)xv_get(prp->editor_text, PANEL_VALUE);

	if (strlen(editor)) {
		initPostEditor(ip, post);
		xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
		return NULL;
	}

	pp = xvnews_post_popup_objects_initialize(NULL, ip->xvnews_window);
	xv_set(pp->post_popup, XV_KEY_DATA, POST_TYPE, post, NULL);
	xv_set(ip->xvnews_window, XV_KEY_DATA, POST_POPUP, pp, NULL);
	xv_set(pp->post_popup, XV_KEY_DATA, POST_PARENT,ip, NULL);
        xv_set(pp->post_popup, XV_HEIGHT, Height,  XV_WIDTH,
		 xv_get(ip->xvnews_window, XV_WIDTH, NULL), NULL);
	Global->post_popups++;

	if (post == NEWS_POST || post == NEWS_REPLY) {
		if (strlen((char *)xv_get(prp->log_text, PANEL_VALUE)))
			xv_set(pp->log_choice, PANEL_VALUE, 1, NULL);
		xv_set(pp->dist_butt, XV_SHOW, TRUE, NULL);
		init_dist_menu(pp);
	} else {
		xv_set(pp->dist_butt, XV_SHOW, FALSE, NULL);
		xv_set(pp->log_choice, XV_SHOW, FALSE, NULL);
	}

	gfm = gfm_initialize(NULL, pp->post_popup, "Include File");
	xv_set(pp->post_popup, XV_KEY_DATA, LOAD_POPUP, gfm, NULL);
	apply_post_defaults(pp, gfm);

	notify_interpose_destroy_func(pp->post_popup, destroy_post);

	return pp;
}

extern void describe_group(ip)
xvnews_xvnews_window_objects	*ip;
{
  struct newsrc_node *curr = Global->group;	
  char line[256];

  assert(curr);

  xv_set(ip->article_window, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, NULL);
  if (!curr->description) {
    curr->description = nntp_gtitle(curr->newsgroup);
  }
  sprintf(line, "%-24.24s %-4d      %-48.48s\n", curr->newsgroup,
	  curr->articles, curr->description);
  mult_group(ip);
  textsw_insert(ip->article_window, line, strlen(line));
}

static void mult_group(ip)
xvnews_xvnews_window_objects	*ip;
{
  int first = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);
  int second = xv_get(ip->groups_list, PANEL_LIST_NEXT_SELECTED, first, NULL);

  if (first != -1 && second == -1)
    xv_set(ip->goto_button, PANEL_INACTIVE, FALSE, NULL);
  else
    xv_set(ip->goto_button, PANEL_INACTIVE, TRUE, NULL);
}

extern void undescribe_group(ip)
xvnews_xvnews_window_objects    *ip;
{
  char	group[26];
  Textsw_index	first = 0, last = TEXTSW_INFINITY;

  strncpy(group, Global->group->newsgroup, 24);
  strcat(group, " "); 
  group[25] = '\0';
  if (textsw_find_bytes(ip->article_window, &first, &last, group,
			strlen(group), 1)!= -1) {
    xv_set(ip->article_window, TEXTSW_READ_ONLY, FALSE, NULL);
    xv_set(ip->article_window, TEXTSW_INSERTION_POINT, first, NULL);
    textsw_edit(ip->article_window, TEXTSW_UNIT_IS_LINE, 1, 0);
    textsw_erase(ip->article_window, first, first + 1);
    xv_set(ip->article_window, TEXTSW_READ_ONLY, TRUE, NULL);
  }
  mult_group(ip);
}
