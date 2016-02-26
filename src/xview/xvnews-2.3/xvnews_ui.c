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
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/cms.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <xview/font.h>
#include <group.h>

#include "xvnews_ui.h"
#include "xvnews.h"

/*
 * Create object `done_menu' in the specified instance.
 */
Xv_opaque
xvnews_done_menu_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;
        void            done_menu_proc();

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "update newsrc",
                        MENU_NOTIFY_PROC, done_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "leave all unread",
                        MENU_NOTIFY_PROC, done_menu_proc,
                        NULL,
                MENU_GEN_PIN_WINDOW, owner, "",
                NULL);
        return obj;
}

/*
 * Create object `quit_menu' in the specified instance.
 */
Xv_opaque
xvnews_quit_menu_create(ip, owner)
  caddr_t         ip;
  Xv_opaque       owner;
{
  Xv_opaque       obj;

  obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
		  XV_KEY_DATA, INSTANCE, ip,
		  MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "iconize window",
                        MENU_NOTIFY_PROC, update_close_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "quit xvnews",
                        MENU_NOTIFY_PROC, update_close_proc,
                        NULL,
                MENU_GEN_PIN_WINDOW, owner, "",
                NULL);
        return obj;
}

/*
 * Create object `prev_menu' in the specified instance.
 */
Xv_opaque
xvnews_prev_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
	extern void             prev_menu_proc();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "prev article",
			MENU_NOTIFY_PROC,	prev_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "prev subject",
			MENU_NOTIFY_PROC,	prev_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "prev author",
			MENU_NOTIFY_PROC,	prev_menu_proc,
                        NULL,
		MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Prev",
                NULL);
        return obj;
}

/*
 * Create object `next_menu' in the specified instance.
 */
Xv_opaque
xvnews_next_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
	extern void             next_menu_proc();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "next article",
			MENU_NOTIFY_PROC,	next_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "next unread",
			MENU_NOTIFY_PROC,	next_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "next subject",
			MENU_NOTIFY_PROC,	next_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "next author",
			MENU_NOTIFY_PROC,	next_menu_proc,
                        NULL,
		MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Next",
		NULL);
        return obj;
}

/*
 * Create object `kill_string_menu' in the specified instance.
 */
Xv_opaque
xvnews_kill_string_menu_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;
	extern void		kill_string_func();

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_TITLE_ITEM, "Kill",
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "subject",
			MENU_NOTIFY_PROC,	kill_string_func,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "author",
			MENU_NOTIFY_PROC,	kill_string_func,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "crossposts",
			MENU_NOTIFY_PROC,	kill_string_func,
                        NULL,
                MENU_GEN_PIN_WINDOW, owner, "Kill",
                NULL);
        return obj;
}

/*
 * Create object `catchup_menu' in the specified instance.

 */
Xv_opaque
xvnews_catchup_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
        extern Menu_item        all_articles_menu();
        extern Menu_item        selected_article_menu();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "all articles",
                        MENU_GEN_PROC, all_articles_menu,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "up to selected article",
                        MENU_GEN_PROC, selected_article_menu,
                        NULL,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Catchup",
                NULL);
        return obj;
}

/*
 * Create object `include_menu' in the specified instance.

 */
Xv_opaque
xvnews_include_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;
	void		include_art();

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "article indented",
			MENU_NOTIFY_PROC, include_art,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "article",
			MENU_NOTIFY_PROC, include_art,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "Include file...",
			MENU_NOTIFY_PROC, include_art,
                        NULL,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Include",
                NULL);
        return obj;
}

/*
 * Create object `kill_menu' in the specified instance.

 */
Xv_opaque
xvnews_kill_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;
        extern Menu_item        undelete_menu_proc();

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "subject",
			MENU_NOTIFY_PROC, kill_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "author",
			MENU_NOTIFY_PROC, kill_notify,
                        NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Kill file...",
			MENU_NOTIFY_PROC, kill_notify,
                        NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Undelete...",
                        MENU_GEN_PROC, undelete_menu_proc,
                        NULL,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Kill",
                NULL);
        return obj;
}

/*
 * Create object `dist_menu' in the specified instance.

 */
Xv_opaque
xvnews_dist_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
	void		dist_notify();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Distributions",
                NULL);
        return obj;
}

/*
 * Create object `view_groups' in the specified instance.

 */
Xv_opaque
xvnews_view_groups_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
	extern void		view_notify();
	extern void		dismiss_view();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
		MENU_DONE_PROC,	dismiss_view,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "all groups",
			MENU_NOTIFY_PROC, view_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "all subscribed groups",
			MENU_NOTIFY_PROC, view_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "all unsubscribed groups",
			MENU_NOTIFY_PROC, view_notify,
			 NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "All matching groups...",
			MENU_NOTIFY_PROC, view_notify,
			 NULL,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Groups",
                NULL);
        return obj;
}

/*
 * Create object `header_menu' in the specified instance.

 */
Xv_opaque
xvnews_header_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
	extern void		header_notify();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "subject",
			MENU_NOTIFY_PROC,	header_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "from",
			MENU_NOTIFY_PROC,	header_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "keywords",
			MENU_NOTIFY_PROC,	header_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "entire header",
			MENU_NOTIFY_PROC,	header_notify,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "article text",
			MENU_NOTIFY_PROC,	header_notify,
                        NULL,
			NULL,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Header field",
                NULL);
        return obj;
}


/*
 * Create object `save_menu' in the specified instance.

 */
Xv_opaque
xvnews_save_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
        extern Menu_item        save_article();
        extern Menu_item        save_news();
        extern Menu_item        filter_news();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "save",
                        MENU_GEN_PROC, save_article,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "filter",
                        MENU_GEN_PROC, filter_news,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "File...",
                        MENU_GEN_PROC, save_news,
                        NULL,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Save",
                NULL);
        return obj;
}

/*
 * Create object `search_menu' in the specified instance.

 */
Xv_opaque
xvnews_search_menu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	extern Menu_item	subject_next_proc();
	extern Menu_item	subject_prev_proc();
	extern Menu_item	author_next();
	extern Menu_item	author_prev_proc();
	extern Menu_item	show_search_popup();
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
		XV_KEY_DATA, INSTANCE, ip,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "subject next",
			MENU_GEN_PROC, subject_next_proc,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "subject prev",
			MENU_GEN_PROC, subject_prev_proc,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "author next",
			MENU_GEN_PROC, author_next,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "author prev",
			MENU_GEN_PROC, author_prev_proc,
			NULL,
		MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "Search...",
                        MENU_GEN_PROC, show_search_popup,
                        NULL,
		MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Search",
		NULL);
	return obj;
}

/*
 * Create object `post_menu' in the specified instance.

 */
Xv_opaque
xvnews_post_menu_create(ip, owner)
        caddr_t         *ip;
        Xv_opaque       owner;
{
        extern void        	post_menu_proc();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "Post an article",
                        MENU_NOTIFY_PROC, post_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "Post followup",
                        MENU_NOTIFY_PROC, post_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "Reply to sender",
                        MENU_NOTIFY_PROC, post_menu_proc,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "Forward article",
                        MENU_NOTIFY_PROC, post_menu_proc,
                        NULL,
		MENU_DEFAULT, 3,
                MENU_GEN_PIN_WINDOW, (Xv_opaque) ip[0], "Options",
                NULL);
        return obj;
}

/*
 * Initialize an instance of object `xvnews_window'.
 */
xvnews_xvnews_window_objects *
xvnews_xvnews_window_objects_initialize(ip, owner)
	xvnews_xvnews_window_objects	*ip;
	Xv_opaque	owner;
{
	Xv_font		listfont;

	if (!ip && !(ip = (xvnews_xvnews_window_objects *) calloc(1, sizeof (xvnews_xvnews_window_objects))))
		return (xvnews_xvnews_window_objects *) NULL;
	if (!ip->xvnews_window)
		ip->xvnews_window = xvnews_xvnews_window_xvnews_window_create(ip, owner);
	if (!ip->controls2)
		ip->controls2 = xvnews_xvnews_window_controls2_create(ip, ip->xvnews_window);
	listfont = (Xv_font)xv_find(ip->xvnews_window, FONT,
			    FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
			    NULL);
	xv_set(ip->controls2, WIN_FONT, listfont, NULL);

	if (!ip->groups_list)
		ip->groups_list = xvnews_xvnews_window_groups_list_create(ip, ip->controls2);
	if (!ip->articles_list)
		ip->articles_list = xvnews_xvnews_window_articles_list_create(ip, ip->controls2);
	if (!ip->controls1)
		ip->controls1 = xvnews_xvnews_window_controls1_create(ip, ip->xvnews_window);
	if (!ip->prev_butt)
                ip->prev_butt = xvnews_xvnews_window_prev_butt_create(ip, ip->controls1);
	if (!ip->quit_butt)
		ip->quit_butt = xvnews_xvnews_window_quit_butt_create(ip, ip->controls1);
	if (!ip->done_butt)
                ip->done_butt = xvnews_xvnews_window_done_butt_create(ip, ip->controls1);
        if (!ip->done_groups_butt)
                ip->done_groups_butt = xvnews_xvnews_window_done_groups_butt_create(ip, ip->controls1);
	if (!ip->sort_newsrc_butt)
                ip->sort_newsrc_butt = xvnews_xvnews_window_sort_newsrc_butt_create(ip, ip->controls1);
	if (!ip->unsubscribe_button)
                ip->unsubscribe_button = xvnews_xvnews_window_unsubscribe_button_create(ip, ip->controls1);
	if (!ip->rescan_butt)
		ip->rescan_butt = xvnews_xvnews_window_rescan_butt_create(ip, ip->controls1);
	if (!ip->catchup_butt)
		ip->catchup_butt = xvnews_xvnews_window_catchup_butt_create(ip, ip->controls1);
	if (!ip->unsub_butt)
		ip->unsub_butt = xvnews_xvnews_window_unsub_butt_create(ip, ip->controls1);
	if (!ip->mark_unread_butt)
		ip->mark_unread_butt = xvnews_xvnews_window_mark_unread_butt_create(ip, ip->controls1);
	if (!ip->search_butt)
                ip->search_butt = xvnews_xvnews_window_search_butt_create(ip, ip->controls1);
	if (!ip->save_article_button)
		ip->save_article_button = xvnews_xvnews_window_save_article_button_create(ip, ip->controls1);
	if (!ip->update_butt)
		ip->update_butt = xvnews_xvnews_window_update_butt_create(ip, ip->controls1);
	if (!ip->next_butt)
		ip->next_butt = xvnews_xvnews_window_next_butt_create(ip, ip->controls1);
	if (!ip->goto_button)
                ip->goto_button = xvnews_xvnews_window_goto_button_create(ip, ip->controls1);
	if (!ip->next_group_butt)
		ip->next_group_butt = xvnews_xvnews_window_next_group_butt_create(ip, ip->controls1);
	if (!ip->all_groups)
		ip->all_groups = xvnews_xvnews_window_all_groups_create(ip, ip->controls1);
	if (!ip->all_butt)
		ip->all_butt = xvnews_xvnews_window_all_butt_create(ip, ip->controls1);
	if (!ip->subscribe_button)
                ip->subscribe_button = xvnews_xvnews_window_subscribe_button_create(ip, ip->controls1);
	if (!ip->print_butt)
		ip->print_butt = xvnews_xvnews_window_print_butt_create(ip, ip->controls1);
	if (!ip->properties_butt)
		ip->properties_butt = xvnews_xvnews_window_properties_butt_create(ip, ip->controls1);
	if (!ip->kill_butt)
                ip->kill_butt = xvnews_xvnews_window_kill_butt_create(ip, ip->controls1);
	if (!ip->post_button)
		ip->post_button = xvnews_xvnews_window_post_button_create(ip, ip->controls1);
	if (!ip->post_men_butt)
                ip->post_men_butt = xvnews_xvnews_window_post_men_butt_create(ip, ip->controls1);
	if (!ip->article_window)
		ip->article_window = xvnews_xvnews_window_article_window_create(ip, ip->xvnews_window);
	return ip;
}

/*
 * Create object `xvnews_window' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_xvnews_window_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Notify_value	main_win_proc();
	Xv_opaque	obj;
	Xv_opaque		xvnews_window_image;
	static unsigned short	xvnews_window_bits[] = {
#include "news.icon"
	};
	
	xvnews_window_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_BITS, xvnews_window_bits,
		SERVER_IMAGE_DEPTH, 1,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);
	obj = xv_create(owner, FRAME,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 640,
		XV_HEIGHT, 600,
		XV_LABEL, "xvnews",
		FRAME_CLOSED, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_ICON, xv_create(XV_NULL, ICON,
			ICON_IMAGE, xvnews_window_image,
			NULL),
		NULL);
	xv_set(obj, WIN_CONSUME_EVENTS,
		NULL, NULL);
	notify_interpose_event_func(obj,
		(Notify_func) main_win_proc, NOTIFY_SAFE);
	return obj;
}

/*
 * Create object `controls2' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_controls2_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern Notify_value     news_drag();
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, 179,
		PANEL_ACCEPT_KEYSTROKE, FALSE,
		WIN_BORDER, FALSE,
		NULL);
	xv_set(obj, WIN_CONSUME_EVENTS,
                LOC_DRAG,
                NULL, NULL);
        notify_interpose_event_func(obj,
                (Notify_func) news_drag, NOTIFY_SAFE);
	return obj;
}


/*
 * Create object `articles_list' in the specified instance.
 */
Xv_opaque
xvnews_xvnews_window_articles_list_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		articles_sel();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		PANEL_LIST_WIDTH, 608,
		XV_HEIGHT, 182,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_ACCEPT_KEYSTROKE, FALSE,
		PANEL_READ_ONLY, TRUE,
		PANEL_LIST_DISPLAY_ROWS, 9,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, articles_sel,
		XV_SHOW,	FALSE,
		NULL);
	return obj;
}

/*
 * Create object `groups_list' in the specified instance.
 */
Xv_opaque
xvnews_xvnews_window_groups_list_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		group_sel();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		PANEL_LIST_WIDTH, 608,
		XV_HEIGHT, 182,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_ACCEPT_KEYSTROKE, FALSE,
		PANEL_LIST_DISPLAY_ROWS, 9,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, TRUE,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_NOTIFY_PROC, group_sel,
		NULL);
	return obj;
}

/*
 * Create object `controls1' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_controls1_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern Notify_value	control_feedback();
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 180,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		PANEL_ACCEPT_KEYSTROKE, TRUE,
		XV_HEIGHT, 67,
		WIN_BORDER, TRUE,
		NULL);
		xv_set(obj, WIN_CONSUME_EVENTS,
                LOC_DRAG,
                NULL, NULL);
        notify_interpose_event_func(obj,
                (Notify_func)control_feedback , NOTIFY_SAFE);
	return obj;
}

/*
 * Create object `done_butt' in the specified instance.
 */
Xv_opaque
xvnews_xvnews_window_done_butt_create(ip, owner)
        xvnews_xvnews_window_objects    *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 8,
                XV_WIDTH, 45,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "done",
                PANEL_ITEM_MENU, xvnews_done_menu_create((caddr_t) ip, ip->xvnews_window),
                NULL);
        return obj;
}

/*
 * Create object `done_groups_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_done_groups_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             done_groups_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 8,
                PANEL_LABEL_WIDTH, 45,
                XV_WIDTH, 45,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "done",
                PANEL_NOTIFY_PROC, done_groups_proc,
                NULL);
        return obj;
}

/*
 * Create object `quit_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_quit_butt_create(ip, owner)
  xvnews_xvnews_window_objects *ip;
  Xv_opaque owner;
{
  Xv_opaque	obj;
	
  obj = xv_create(owner, PANEL_BUTTON,
		  XV_KEY_DATA, INSTANCE, ip,
		  XV_X, 16,
		  XV_Y, 8,
		  XV_WIDTH, 45,
		  XV_HEIGHT, 19,
		  PANEL_LABEL_STRING, "done",
		  PANEL_ITEM_MENU, 
		  xvnews_quit_menu_create((caddr_t) ip, ip->xvnews_window),
		  /* PANEL_NOTIFY_PROC, update_close_proc, */
		  NULL);
  return obj;
}

/*
 * Create object `subscribe_button' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_subscribe_button_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             subscribe_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 208,
                XV_Y, 40,
                XV_WIDTH, 99,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "subscribe",
                PANEL_NOTIFY_PROC, subscribe_proc,
                NULL);
        return obj;
}

/*
 * Create object `prev_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_prev_butt_create(ip, owner)
        xvnews_xvnews_window_objects         *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 96,
                XV_Y, 8,
		/*
                PANEL_LABEL_WIDTH, 85,
		*/
                XV_WIDTH, 85,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "prev art",
		PANEL_ITEM_MENU, xvnews_prev_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `unsubscribe_button' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_unsubscribe_button_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             unsubscribe_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 208,
                XV_Y, 8,
                XV_WIDTH, 102,
                XV_HEIGHT, 21,
                PANEL_LABEL_STRING, "unsubscribe",
                PANEL_NOTIFY_PROC, unsubscribe_proc,
                NULL);
        return obj;
}

/*
 * Create object `rescan_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_rescan_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		rescan_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 88,
		XV_Y, 8,
		PANEL_LABEL_WIDTH, 48,
		XV_WIDTH, 48,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "rescan",
		PANEL_NOTIFY_PROC, rescan_proc,
		NULL);
	return obj;
}

/*
 * Create object `catchup_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_catchup_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		catchup_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 208,
		XV_Y, 8,
		PANEL_LABEL_WIDTH, 62,
		XV_WIDTH, 62,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "catchup",
		PANEL_ITEM_MENU, xvnews_catchup_menu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `unsub_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_unsub_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		unsub_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 315,
		XV_Y, 8,
		XV_WIDTH, 92,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "unsubscribe",
		PANEL_NOTIFY_PROC, unsub_proc,
		NULL);
	return obj;
}

/*
 * Create object `mark_unread_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_mark_unread_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		mark_unread_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 376,
		XV_Y, 40,
		XV_WIDTH, 90, /* 96 */
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "mark unread",
		PANEL_NOTIFY_PROC, mark_unread_proc,
		NULL);
	return obj;
}

/*
 * Create object `search_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_search_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 500,
                XV_Y, 8,
                XV_WIDTH, 90,
                XV_HEIGHT, 21,
                PANEL_LABEL_STRING, "search",
                PANEL_ITEM_MENU, xvnews_search_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `save_article_button' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_save_article_button_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 40,
                XV_WIDTH, 75,
                XV_HEIGHT, 21,
                PANEL_LABEL_STRING, "save",
                PANEL_ITEM_MENU, xvnews_save_menu_create((caddr_t *) ip, NULL),
                NULL);
	return obj;
}

/*
 * Create object `update_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_update_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		w_newsrc_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 40,
		XV_WIDTH, 61,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "update",
		PANEL_NOTIFY_PROC, w_newsrc_proc,
		NULL);
	return obj;
}

/*
 * Create object `next_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_next_butt_create(ip, owner)
	xvnews_xvnews_window_objects		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 96,
		XV_Y, 40,
		/*
		PANEL_LABEL_WIDTH, 85,
		*/
		XV_WIDTH, 85,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "next art",
		PANEL_ITEM_MENU, xvnews_next_menu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `goto_button' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_goto_button_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             goto_group_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 88,
                XV_Y, 40,
                XV_WIDTH, 99,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "goto group",
                PANEL_NOTIFY_PROC, goto_group_proc,
                NULL);
        return obj;
}

/*
 * Create object `next_group_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_next_group_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		next_group_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 88,
		XV_Y, 40,
		PANEL_LABEL_WIDTH, 83,
		XV_WIDTH, 83,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "read group",
		PANEL_NOTIFY_PROC, next_group_proc,
		NULL);
	return obj;
}

/*
 * Create object `all_groups' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_all_groups_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 208,
		XV_Y, 40,
		XV_WIDTH, 78,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "view groups",
		PANEL_ITEM_MENU, xvnews_view_groups_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `all_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_all_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		all_articles_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 208,
		XV_Y, 40,
		XV_WIDTH, 81,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "all articles",
		PANEL_NOTIFY_PROC, all_articles_proc,
		NULL);
	return obj;
}

/*
 * Create object `print_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_print_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		print_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 308,
		XV_Y, 40,
		PANEL_LABEL_WIDTH, 40,
		XV_WIDTH, 40,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "print",
		PANEL_NOTIFY_PROC, print_proc,
		NULL);
	return obj;
}

/*
 * Create object `properties_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_properties_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		display_props();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 376,
		XV_Y, 40,
		XV_WIDTH, 89,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Properties...",
		PANEL_NOTIFY_PROC, display_props,
		NULL);
	return obj;
}

/*
 * Create object `kill_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_kill_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             kill_subject_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 420,
                XV_Y, 8,
                XV_WIDTH, 51,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "kill",
		PANEL_ITEM_MENU, xvnews_kill_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `post_button' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_post_button_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		post_article();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 484,
		XV_Y, 40,
		XV_WIDTH, 53,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Post...",
		PANEL_NOTIFY_PROC, post_article,
		NULL);
	return obj;
}

/*
 * Create object `post_men_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_post_men_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 492,
                XV_Y, 40,
                XV_WIDTH, 44,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "Post/E-mail",
                PANEL_ITEM_MENU, xvnews_post_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `article_window' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_article_window_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, TEXTSW,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 248,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		TEXTSW_LINE_BREAK_ACTION, TEXTSW_WRAP_AT_WORD,
        TEXTSW_INSERT_MAKES_VISIBLE, TEXTSW_IF_AUTO_SCROLL,
        TEXTSW_LOWER_CONTEXT,   -1,
                TEXTSW_AGAIN_RECORDING, FALSE,
                TEXTSW_IGNORE_LIMIT,    TEXTSW_INFINITY,
		TEXTSW_DISABLE_LOAD, TRUE,
		TEXTSW_READ_ONLY, TRUE,
		OPENWIN_SHOW_BORDERS, TRUE,
		NULL);
	return obj;
}

/*
 * Initialize an instance of object `props'.
 */
xvnews_props_objects *
xvnews_props_objects_initialize(ip, owner)
	xvnews_props_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (xvnews_props_objects *) calloc(1, sizeof (xvnews_props_objects))))
		return (xvnews_props_objects *) NULL;
	if (!ip->props)
		ip->props = xvnews_props_props_create(ip, owner);
	if (!ip->props_controls)
		ip->props_controls = xvnews_props_props_controls_create(ip, ip->props);
	if (!ip->props_color_class)
		ip->props_color_class = xvnews_props_props_color_class_create(ip, ip->props_controls);
	if (!ip->color_menu_button)
		ip->color_menu_button = xvnews_props_color_menu_button_create(ip, ip->props_controls);
	if (!ip->props_color)
		ip->props_color = xvnews_props_props_color_create(ip, ip->props_controls);
	if (!ip->props_font_class)
		ip->props_font_class = xvnews_props_props_font_class_create(ip, ip->props_controls);
	if (!ip->props_font)
		ip->props_font = xvnews_props_props_font_create(ip, ip->props_controls);
	if (!ip->display_text)
                ip->display_text = xvnews_props_display_text_create(ip, ip->props_controls);
	if (!ip->rescan_text)
                ip->rescan_text = xvnews_props_rescan_text_create(ip, ip->props_controls);
	if (!ip->header_select)
                ip->header_select = xvnews_props_header_select_create(ip, ip->props_controls);
        if (!ip->indent_text)
                ip->indent_text = xvnews_props_indent_text_create(ip, ip->props_controls);
	if (!ip->sort)
                ip->sort = xvnews_props_sort_create(ip, ip->props_controls);
	if (!ip->editor_text)
                ip->editor_text = xvnews_props_editor_text_create(ip, ip->props_controls);
        if (!ip->print_text)
                ip->print_text = xvnews_props_print_text_create(ip, ip->props_controls);
	if (!ip->filter_text)
                ip->filter_text = xvnews_props_filter_text_create(ip, ip->props_controls);
	if (!ip->log_text)
                ip->log_text = xvnews_props_log_text_create(ip, ip->props_controls);
	if (!ip->props_fontsize)
		ip->props_fontsize = xvnews_props_props_fontsize_create(ip, ip->props_controls);
	if (!ip->props_apply)
		ip->props_apply = xvnews_props_props_apply_create(ip, ip->props_controls);
	if (!ip->props_reset)
		ip->props_reset = xvnews_props_props_reset_create(ip, ip->props_controls);
	if (!ip->props_done)
		ip->props_done = xvnews_props_props_done_create(ip, ip->props_controls);
	return ip;
}

/*
 * Create object `props' in the specified instance.

 */
Xv_opaque
xvnews_props_props_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	props_done();
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 450,
		XV_HEIGHT, 410,
		XV_LABEL, "xvnews Properties",
		XV_SHOW, TRUE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		XV_SHOW,	FALSE,
		FRAME_DONE_PROC, props_done,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `props_controls' in the specified instance.

 */
Xv_opaque
xvnews_props_props_controls_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `props_color_class' in the specified instance.

 */
Xv_opaque
xvnews_props_props_color_class_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		change_color_class();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 10,
		XV_Y, 16,
		XV_WIDTH, 166,
		XV_HEIGHT, 23,
	/*
		PANEL_VALUE_X, 56,
		PANEL_VALUE_Y, 16,
	*/
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Color:",
		PANEL_NOTIFY_PROC, change_color_class,
		PANEL_CHOICE_STRINGS,
			"background",
			"highlight",
			"text",
			"text background",
			0,
		NULL);
	return obj;
}

/*
 * Create object `color_menu_button' in the specified instance.

 */
Xv_opaque
xvnews_props_color_menu_button_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		call_color_menu();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 56,
		XV_Y, 48,
		XV_WIDTH, 73,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "chooser...",
		PANEL_NOTIFY_PROC, call_color_menu,
		NULL);
	return obj;
}

/*
 * Create object `props_color' in the specified instance.

 */
Xv_opaque
xvnews_props_props_color_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 148,
		XV_Y, 48,
		XV_WIDTH, 200,
		XV_HEIGHT, 15,
		PANEL_VALUE_X, 148,
		PANEL_VALUE_Y, 48,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 25,
		PANEL_VALUE_STORED_LENGTH, 25,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `props_font_class' in the specified instance.

 */
Xv_opaque
xvnews_props_props_font_class_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		change_font_class();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 10,
		XV_Y, 80,
		XV_WIDTH, 167,
		XV_HEIGHT, 23,
	/*
		PANEL_VALUE_X, 53,
		PANEL_VALUE_Y, 80,
	*/
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Font:",
		PANEL_NOTIFY_PROC, change_font_class,
		PANEL_CHOICE_STRINGS,
			"article window",
			"other text",
			0,
		NULL);
	return obj;
}

/*
 * Create object `props_font' in the specified instance.

 */
Xv_opaque
xvnews_props_props_font_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		change_font();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 52,
		XV_Y, 112,
		XV_WIDTH, 163,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 135,
		PANEL_VALUE_Y, 112,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Font name:",
		PANEL_NOTIFY_PROC, change_font,
		PANEL_CHOICE_STRINGS,
			"default",
			"Lucida",
			"Times",
			"Courier",
			0,
		NULL);
	return obj;
}

/*
 * Create object `props_fontsize' in the specified instance.

 */
Xv_opaque
xvnews_props_props_fontsize_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		change_fontsize();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 228,
		XV_Y, 112,
		XV_WIDTH, 91,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 268,
		PANEL_VALUE_Y, 112,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Size:",
		PANEL_NOTIFY_PROC, change_fontsize,
		PANEL_CHOICE_STRINGS,
			"8",
			"10",
			"12",
			"14",
			"16",
			"18",
			"24",
			0,
		NULL);
	return obj;
}

/*
 * Create object `display_text' in the specified instance.

 */
Xv_opaque
xvnews_props_display_text_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
	extern Panel_setting    change_display();
        Xv_opaque       obj;

	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 10,
                XV_Y, 152,
                XV_WIDTH, 203,
                XV_HEIGHT, 15,
                PANEL_LABEL_STRING, "Display Rows:",
	/*
                PANEL_VALUE_X, 116,
                PANEL_VALUE_Y, 152,
	*/
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_VALUE_DISPLAY_LENGTH, 2,
                PANEL_VALUE_STORED_LENGTH, 80,
                PANEL_MAX_VALUE, 24,
		PANEL_NOTIFY_PROC, change_display,
                PANEL_MIN_VALUE, 1,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `rescan_text' in the specified instance.

 */
Xv_opaque
xvnews_props_rescan_text_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_NUMERIC_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 208,
                XV_Y, 152,
                XV_WIDTH, 228,
                XV_HEIGHT, 15,
                PANEL_LABEL_STRING, "Rescan Time(min):",
	/*
                PANEL_VALUE_X, 357,
                PANEL_VALUE_Y, 152,
	*/
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_VALUE_DISPLAY_LENGTH, 3,
                PANEL_VALUE_STORED_LENGTH, 80,
                PANEL_MAX_VALUE, 60 * 24,
                PANEL_MIN_VALUE, 5,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `header_select' in the specified instance.

 */
Xv_opaque
xvnews_props_header_select_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
	extern int              change_header();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TOGGLE, PANEL_FEEDBACK, PANEL_MARKED,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 272,
                XV_Y, 216,
		/*
                XV_WIDTH, 116,
                XV_HEIGHT, 23,
                PANEL_VALUE_X, 323,
                PANEL_VALUE_Y, 180,
		*/
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_CHOICE_NROWS, 1,
                PANEL_LABEL_STRING, "Header:",
		PANEL_NOTIFY_PROC, change_header,
                PANEL_CHOICE_STRING, 0, "Full",
                NULL);
        return obj;
}

/*
 * Create object `indent_text' in the specified instance.

 */
Xv_opaque
xvnews_props_indent_text_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 10,
                XV_Y, 184,
                XV_WIDTH, 269,
                XV_HEIGHT, 15,
                PANEL_LABEL_STRING, "Include indent prefix:",
	/*
                PANEL_VALUE_X, 163,
                PANEL_VALUE_Y, 184,
	*/
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_VALUE_DISPLAY_LENGTH, 9,
                PANEL_VALUE_STORED_LENGTH, 80,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `sort' in the specified instance.
 */
Xv_opaque
xvnews_props_sort_create(ip, owner)
        xvnews_props_objects    *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TOGGLE, PANEL_FEEDBACK, PANEL_MARKED,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 272,
                XV_Y, 176,
                PANEL_CHOICE_NROWS, 1,
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_LABEL_STRING, "Sort Subjects:",
                PANEL_CHOICE_STRING, 0, "",
                PANEL_VALUE, 0,
                NULL);
        return obj;
}

/*
 * Create object `editor_text' in the specified instance.
 */
Xv_opaque
xvnews_props_editor_text_create(ip, owner)
        xvnews_props_objects    *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 10,
                XV_Y, 264,
                PANEL_VALUE_DISPLAY_LENGTH, 32,
                PANEL_VALUE_STORED_LENGTH, 256,
                PANEL_LABEL_STRING, "Editor for Post/Mail:",
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `print_text' in the specified instance.

 */
Xv_opaque
xvnews_props_print_text_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 10,
                XV_Y, 224,
                XV_WIDTH, 356,
                XV_HEIGHT, 15,
                PANEL_LABEL_STRING, "Print:",
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_VALUE_DISPLAY_LENGTH, 25,
                PANEL_VALUE_STORED_LENGTH, 256,
                PANEL_READ_ONLY, FALSE,
                NULL);
	return obj;
}

/*
 * Create object `filter_text' in the specified instance.
 */
Xv_opaque
xvnews_props_filter_text_create(ip, owner)
        xvnews_props_objects    *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 8,
                XV_Y, 296,
                PANEL_VALUE_DISPLAY_LENGTH, 40,
                PANEL_VALUE_STORED_LENGTH, 256,
                PANEL_LABEL_STRING, "Save filter:",
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `log_text' in the specified instance.
 */
Xv_opaque
xvnews_props_log_text_create(ip, owner)
        xvnews_props_objects    *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 8,
                XV_Y, 336,
                PANEL_VALUE_DISPLAY_LENGTH, 34,
                PANEL_VALUE_STORED_LENGTH, 256,
                PANEL_LABEL_STRING, "Outgoing post log:",
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `props_apply' in the specified instance.

 */
Xv_opaque
xvnews_props_props_apply_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 88,
		XV_Y, 368,
		XV_WIDTH, 52,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "apply",
		PANEL_NOTIFY_PROC, apply_props,
		NULL);
	return obj;
}

/*
 * Create object `props_reset' in the specified instance.

 */
Xv_opaque
xvnews_props_props_reset_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		reset_props();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 168,
		XV_Y, 368,
		XV_WIDTH, 48,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "reset",
		PANEL_NOTIFY_PROC, reset_props,
		NULL);
	return obj;
}

/*
 * Create object `props_done' in the specified instance.

 */
Xv_opaque
xvnews_props_props_done_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		props_done_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 244,
		XV_Y, 368,
		XV_WIDTH, 48,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "done",
		PANEL_NOTIFY_PROC, props_done_notify,
		NULL);
	return obj;
}

/*
 * Initialize an instance of object `post_popup'.
 */
xvnews_post_popup_objects *
xvnews_post_popup_objects_initialize(ip, owner)
	xvnews_post_popup_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (xvnews_post_popup_objects *) calloc(1, sizeof (xvnews_post_popup_objects))))
		return (xvnews_post_popup_objects *) NULL;
	if (!ip->post_popup)
		ip->post_popup = xvnews_post_popup_post_popup_create(ip, owner);
	if (!ip->post_controls)
		ip->post_controls = xvnews_post_popup_post_controls_create(ip, ip->post_popup);
	if (!ip->deliver_butt)
		ip->deliver_butt = xvnews_post_popup_deliver_butt_create(ip, ip->post_controls);
	if (!ip->cancel_butt)
		ip->cancel_butt = xvnews_post_popup_cancel_butt_create(ip, ip->post_controls);
	if (!ip->include_butt)
                ip->include_butt = xvnews_post_popup_include_butt_create(ip, ip->post_controls);
	if (!ip->dist_butt)
                ip->dist_butt = xvnews_post_popup_dist_butt_create(ip, ip->post_controls);
	if (!ip->log_choice)
                ip->log_choice = xvnews_post_popup_log_choice_create(ip, ip->post_controls);
	if (!ip->post_group)
                ip->post_group = xvnews_post_popup_post_group_create(ip, ip->post_controls);
        window_fit(ip->post_controls);
	if (!ip->post_window)
		ip->post_window = xvnews_post_popup_post_window_create(ip, ip->post_popup);
	xv_set(ip->post_controls, XV_WIDTH, WIN_EXTEND_TO_EDGE, NULL);
        xv_set(ip->post_window,
                XV_WIDTH, WIN_EXTEND_TO_EDGE,
                XV_HEIGHT, WIN_EXTEND_TO_EDGE,
                NULL);
	return ip;
}

/*
 * Create object `post_popup' in the specified instance.

 */
Xv_opaque
xvnews_post_popup_post_popup_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	Xv_opaque		post_popup_image;
	static unsigned short	post_popup_bits[] = {
#include "compose.icon"
	};
	
	post_popup_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_BITS, post_popup_bits,
		SERVER_IMAGE_DEPTH, 1,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);
	obj = xv_create(XV_NULL, FRAME_BASE,
		WIN_IS_CLIENT_PANE,
		WIN_CMS,        (Cms)xv_get(owner, WIN_CMS),
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 619,
		XV_HEIGHT, 380,
		FRAME_CLOSED, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_ICON, xv_create(XV_NULL, ICON,
			ICON_IMAGE, post_popup_image,
			NULL),
		NULL);
	return obj;
}

/*
 * Create object `post_group' in the specified instance.
 */
Xv_opaque
xvnews_post_popup_post_group_create(ip, owner)
        xvnews_post_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, GROUP,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 32,
                XV_Y, 8,
                GROUP_TYPE, GROUP_ROW,
                GROUP_MEMBERS,
                        ip->deliver_butt,
                        ip->cancel_butt,
                        ip->include_butt,
                        ip->dist_butt,
                        ip->log_choice,
                        NULL,
                GROUP_ROW_ALIGNMENT, GROUP_TOP_EDGES,
                GROUP_HORIZONTAL_SPACING, 30,
                NULL);
        return obj;
}


/*
 * Create object `log_choice' in the specified instance.
 */
Xv_opaque
xvnews_post_popup_log_choice_create(ip, owner)
        xvnews_post_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TOGGLE, PANEL_FEEDBACK, PANEL_MARKED,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 520,
                XV_Y, 4,
                PANEL_CHOICE_NCOLS, 1,
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_LABEL_STRING, "Log:",
                NULL);
        return obj;
}

/*
 * Create object `dist_butt' in the specified instance.

 */
Xv_opaque
xvnews_post_popup_dist_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_ABBREV_MENU_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 356,
                XV_Y, 10,
                XV_WIDTH, 115,
                XV_HEIGHT, 15,
                PANEL_LABEL_STRING, "Distribution:",
                PANEL_ITEM_MENU, xvnews_dist_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `post_controls' in the specified instance.

 */
Xv_opaque
xvnews_post_popup_post_controls_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, 40,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `deliver_butt' in the specified instance.

 */
Xv_opaque
xvnews_post_popup_deliver_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		deliver_article();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 32,
		XV_Y, 8,
		XV_WIDTH, 60,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "deliver",
		PANEL_NOTIFY_PROC, deliver_article,
		NULL);
	return obj;
}

/*
 * Create object `cancel_butt' in the specified instance.

 */
Xv_opaque
xvnews_post_popup_cancel_butt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		cancel_post();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 128,
		XV_Y, 8,
		XV_WIDTH, 56,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "cancel",
		PANEL_NOTIFY_PROC, cancel_post,
		NULL);
	return obj;
}

/*
 * Create object `include_butt' in the specified instance.
 
 */
Xv_opaque
xvnews_post_popup_include_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 228,
                XV_Y, 8,
                XV_WIDTH, 94,
                XV_HEIGHT, 21,
                PANEL_LABEL_STRING, "include",
                PANEL_ITEM_MENU, xvnews_include_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `post_window' in the specified instance.

 */
Xv_opaque
xvnews_post_popup_post_window_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Notify_value	xvnews_text_event();
	Xv_opaque	obj;
	
	obj = xv_create(owner, TEXTSW,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 41,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		OPENWIN_SHOW_BORDERS, TRUE,
		TEXTSW_IGNORE_LIMIT,    TEXTSW_INFINITY,
		TEXTSW_MEMORY_MAXIMUM, TEXTSW_INFINITY,
        	TEXTSW_LOWER_CONTEXT,   -1,
		NULL);
	xv_set(textsw_first(obj), WIN_CONSUME_EVENTS,
		NULL, NULL);
	notify_interpose_event_func(textsw_first(obj),
		(Notify_func) xvnews_text_event, NOTIFY_SAFE);
	return obj;
}

/*
 * Create object `sort_newsrc_butt' in the specified instance.

 */
Xv_opaque
xvnews_xvnews_window_sort_newsrc_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             sort_newsrc_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 88,
                XV_Y, 8,
                XV_WIDTH, 88,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "sort newsrc",
                PANEL_NOTIFY_PROC, sort_newsrc_proc,
                NULL);
        return obj;
}

/*
 * Initialize an instance of object `search_popup'.
 */
xvnews_search_popup_objects *
xvnews_search_popup_objects_initialize(ip, owner)
	xvnews_search_popup_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (xvnews_search_popup_objects *) calloc(1, sizeof (xvnews_search_popup_objects))))
		return (xvnews_search_popup_objects *) NULL;
	if (!ip->search_popup)
		ip->search_popup = xvnews_search_popup_search_popup_create(ip, owner);
	if (!ip->controls4)
		ip->controls4 = xvnews_search_popup_controls4_create(ip, ip->search_popup);
	if (!ip->search_text)
		ip->search_text = xvnews_search_popup_search_text_create(ip, ip->controls4);
	if (!ip->header_choice)
                ip->header_choice = xvnews_search_popup_header_choice_create(ip, ip->controls4);
	if (!ip->header_text)
		ip->header_text = xvnews_search_popup_header_text_create(ip, ip->controls4);
	if (!ip->show_groups_butt)
                ip->show_groups_butt = xvnews_search_popup_show_groups_butt_create(ip, ip->controls4);
	if (!ip->srch_prev_butt)
                ip->srch_prev_butt = xvnews_search_popup_srch_prev_butt_create(ip, ip->controls4);
        if (!ip->srch_next_button)
                ip->srch_next_button = xvnews_search_popup_srch_next_button_create(ip, ip->controls4);
        window_fit(ip->controls4);
        window_fit(ip->search_popup);
	xv_set(ip->controls4,
                XV_WIDTH, WIN_EXTEND_TO_EDGE,
                XV_HEIGHT, WIN_EXTEND_TO_EDGE,
                NULL);
	return ip;
}

/*
 * Create object `search_popup' in the specified instance.

 */
Xv_opaque
xvnews_search_popup_search_popup_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 291,
		XV_HEIGHT, 124,
		XV_LABEL, "Expression search",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `show_groups_butt' in the specified instance.
 
 */
Xv_opaque
xvnews_search_popup_show_groups_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             show_groups_proc();
        Xv_opaque       obj;
        
        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 112,
                XV_Y, 68,
                XV_WIDTH, 92,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "view groups",
                PANEL_NOTIFY_PROC, show_groups_proc,
                NULL);
        return obj;
}

/*
 * Create object `controls4' in the specified instance.

 */
Xv_opaque
xvnews_search_popup_controls4_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `search_text' in the specified instance.

 */
Xv_opaque
xvnews_search_popup_search_text_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern Panel_setting    search_text_proc(), search_text_event();
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 20,
		XV_WIDTH, 246,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Search string:",
                PANEL_NOTIFY_PROC, search_text_proc,
		PANEL_EVENT_PROC, search_text_event,
	/*
		PANEL_VALUE_X, 118,
		PANEL_VALUE_Y, 20,
	*/
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 18,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `header_choice' in the specified instance.

 */
Xv_opaque
xvnews_search_popup_header_choice_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_ABBREV_MENU_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 56,
                XV_WIDTH, 115,
                XV_HEIGHT, 15,
                PANEL_LABEL_STRING, "Header field:",
                PANEL_ITEM_MENU, xvnews_header_menu_create((caddr_t *) ip, NULL),
                NULL);
        return obj;
}

/*
 * Create object `header_text' in the specified instance.

 */
Xv_opaque
xvnews_search_popup_header_text_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 164,
		XV_Y, 56,
		/*
		XV_WIDTH, 120,
		XV_HEIGHT, 17,
		PANEL_VALUE_X, 152,
		PANEL_VALUE_Y, 56,
		*/
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `srch_prev_butt' in the specified instance.

 */
Xv_opaque
xvnews_search_popup_srch_prev_butt_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             srch_prev_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 64,
                XV_Y, 88,
                XV_WIDTH, 45,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "prev",
                PANEL_NOTIFY_PROC, srch_prev_proc,
                NULL);
        return obj;
}

/*
 * Create object `srch_next_button' in the specified instance.
 
 */
Xv_opaque
xvnews_search_popup_srch_next_button_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
        extern void             srch_next_proc();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 184,
                XV_Y, 88,
                XV_WIDTH, 45,
                XV_HEIGHT, 19,
                PANEL_LABEL_STRING, "next",
                PANEL_NOTIFY_PROC, srch_next_proc,
                NULL);
        return obj;
}

/*
 * Initialize an instance of object `kill_popup'.
 */
kill_popup_objects *
kill_popup_objects_initialize(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_font		listfont;

	if (!ip && !(ip = (kill_popup_objects *) calloc(1, sizeof (kill_popup_objects))))
                return (kill_popup_objects *) NULL;
        if (!ip->kill_popup)
                ip->kill_popup = kill_popup_kill_popup_create(ip, owner); 
	if (!ip->kill_controls)
                ip->kill_controls = kill_popup_kill_controls_create(ip, ip->kill_popup);
	listfont = (Xv_font)xv_find(ip->kill_popup, FONT,
		    FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
		    NULL);
	xv_set(ip->kill_controls, WIN_FONT, listfont, NULL);

        if (!ip->file_list)
                ip->file_list = kill_popup_file_list_create(ip, ip->kill_controls);
        if (!ip->load_kill_butt)
                ip->load_kill_butt = kill_popup_load_kill_butt_create(ip, ip->kill_controls);
        if (!ip->delete_kill_butt)
                ip->delete_kill_butt = kill_popup_delete_kill_butt_create(ip, ip->kill_controls);
        if (!ip->save_butt)
                ip->save_butt = kill_popup_save_butt_create(ip, ip->kill_controls);
        if (!ip->kill_choice)
                ip->kill_choice = kill_popup_kill_choice_create(ip, ip->kill_controls);
        if (!ip->kill_text)
                ip->kill_text = kill_popup_kill_text_create(ip, ip->kill_controls);
        if (!ip->mod_choice)
                ip->mod_choice = kill_popup_mod_choice_create(ip, ip->kill_controls);
        if (!ip->command_choice)
		ip->command_choice = kill_popup_command_choice_create(ip, ip->kill_controls);
	if (!ip->changes_choice)
                ip->changes_choice = kill_popup_changes_choice_create(ip, ip->kill_controls);
        if (!ip->add_local_butt)
                ip->add_local_butt = kill_popup_add_local_butt_create(ip, ip->kill_controls);
        if (!ip->add_global_butt)
                ip->add_global_butt = kill_popup_add_global_butt_create(ip, ip->kill_controls);
	return ip;
}

/*
 * Create object `kill_popup' in the specified instance.
 */
Xv_opaque
kill_popup_kill_popup_create(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 400,
		XV_HEIGHT, 285,
		XV_LABEL, "Kill File Editor",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `kill_choice' in the specified instance.
 */
Xv_opaque
kill_popup_kill_choice_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_ABBREV_MENU_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 128,
                PANEL_LABEL_STRING, "Kill String:",
                PANEL_ITEM_MENU, xvnews_kill_string_menu_create((caddr_t) ip, ip->kill_popup),
                NULL);
        return obj;
}

/*
 * Create object `kill_controls' in the specified instance.
 */
Xv_opaque
kill_popup_kill_controls_create(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `kill_text' in the specified instance.
 */
Xv_opaque
kill_popup_kill_text_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TEXT,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 136,
                XV_Y, 128,
                PANEL_VALUE_DISPLAY_LENGTH, 25,
                PANEL_VALUE_STORED_LENGTH, 80,
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_READ_ONLY, FALSE,
                NULL);
        return obj;
}

/*
 * Create object `mod_choice' in the specified instance.
 */
Xv_opaque
kill_popup_mod_choice_create(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 160,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Modifier:",
		PANEL_CHOICE_STRINGS,
			"header",
			"subject",
			NULL,
		PANEL_DEFAULT_VALUE, 1,
		PANEL_VALUE, 1,
		NULL);
	return obj;
}

/*
 * Create object `changes_choice' in the specified instance.
 */
Xv_opaque
kill_popup_changes_choice_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_TOGGLE, PANEL_FEEDBACK, PANEL_MARKED,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 208,
                PANEL_CHOICE_NCOLS, 1,
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_LABEL_STRING, "Process kill entry for this session:",
                PANEL_CHOICE_STRING, 0, "",
                PANEL_VALUE, 0,
                NULL);
        return obj;
}

/*
 * Create object `command_choice' in the specified instance.
 */
Xv_opaque
kill_popup_command_choice_create(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 200,
		XV_Y, 160,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Command:",
		PANEL_CHOICE_STRINGS,
			"mark unread",
			"junk",
			NULL,
		PANEL_DEFAULT_VALUE, 1,
		PANEL_VALUE, 1,
		NULL);
	return obj;
}

/*
 * Create object `add_local_butt' in the specified instance.
 */
Xv_opaque
kill_popup_add_local_butt_create(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern void             add_local_func();
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 32,
		XV_Y, 248,
		PANEL_LABEL_STRING, "add group kill",
		PANEL_NOTIFY_PROC, add_local_func,
		NULL);
	return obj;
}

/*
 * Create object `add_global_butt' in the specified instance.
 */
Xv_opaque
kill_popup_add_global_butt_create(ip, owner)
	kill_popup_objects	*ip;
	Xv_opaque	owner;
{
	extern void             add_global_func();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 256,
		XV_Y, 248,
		PANEL_LABEL_STRING, "add global kill",
		PANEL_NOTIFY_PROC, add_global_func,
		NULL);
	return obj;
}

/*
 * Create object `file_list' in the specified instance.
 */
Xv_opaque
kill_popup_file_list_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_LIST,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 8,
                XV_Y, 8,
                PANEL_LIST_WIDTH, -1,
                PANEL_LIST_DISPLAY_ROWS, 3,
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_READ_ONLY, FALSE,
                PANEL_CHOOSE_ONE, FALSE,
                PANEL_CHOOSE_NONE, TRUE,
                NULL);
        return obj;
}

/*
 * Create object `load_kill_butt' in the specified instance.
 */
Xv_opaque
kill_popup_load_kill_butt_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 16,
                XV_Y, 96,
                PANEL_LABEL_STRING, "Edit...",
                PANEL_ITEM_MENU, xvnews_kill_file_menu_create((caddr_t) ip, ip->kill_popup),
                NULL);
        return obj;
}

/*
 * Create object `kill_file_menu' in the specified instance.
 */
Xv_opaque
xvnews_kill_file_menu_create(ip, owner)
        caddr_t         ip;
        Xv_opaque       owner;
{
	extern void	kill_file_func();
        Xv_opaque       obj;

        obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
                XV_KEY_DATA, INSTANCE, ip,
                MENU_TITLE_ITEM, "Kill",
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "group",
			MENU_NOTIFY_PROC,       kill_file_func,
                        NULL,
                MENU_ITEM,
                        XV_KEY_DATA, INSTANCE, ip,
                        MENU_STRING, "global",
			MENU_NOTIFY_PROC,       kill_file_func,
                        NULL,
                MENU_GEN_PIN_WINDOW, owner, "Kill",
                NULL);
        return obj;
}

/*
 * Create object `delete_kill_butt' in the specified instance.
 */
Xv_opaque
kill_popup_delete_kill_butt_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        extern void             delete_kill_func();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 168,
                XV_Y, 96,
                PANEL_LABEL_STRING, "delete",
                PANEL_NOTIFY_PROC, delete_kill_func,
                NULL);
        return obj;
}

/*
 * Create object `save_butt' in the specified instance.
 */
Xv_opaque
kill_popup_save_butt_create(ip, owner)
        kill_popup_objects       *ip;
        Xv_opaque       owner;
{
        extern void             save_kill_func();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_BUTTON,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 288,
                XV_Y, 96,
                PANEL_LABEL_STRING, "save file",
                PANEL_NOTIFY_PROC, save_kill_func,
                NULL);
        return obj;
}


/*
 * Initialize an instance of object `undelete_popup'.
 */
xvnews_undelete_popup_objects *
xvnews_undelete_popup_objects_initialize(ip, owner)
	xvnews_undelete_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_font		listfont;

	if (!ip && !(ip = (xvnews_undelete_popup_objects *) calloc(1, sizeof (xvnews_undelete_popup_objects))))
		return (xvnews_undelete_popup_objects *) NULL;
	if (!ip->undelete_popup)
		ip->undelete_popup = xvnews_undelete_popup_undelete_popup_create(ip, owner);
	if (!ip->controls3)
		ip->controls3 = xvnews_undelete_popup_controls3_create(ip, ip->undelete_popup);
	listfont = (Xv_font)xv_find(ip->undelete_popup, FONT,
			    FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH,
			    NULL);
	xv_set(ip->controls3, WIN_FONT, listfont, NULL);

	if (!ip->undelete_list)
		ip->undelete_list = xvnews_undelete_popup_undelete_list_create(ip, ip->controls3);
	if (!ip->undelete_butt)
		ip->undelete_butt = xvnews_undelete_popup_undelete_butt_create(ip, ip->controls3);
	window_fit(ip->controls3);
	window_fit(ip->undelete_popup);
	xv_set(ip->controls3,
                XV_WIDTH, WIN_EXTEND_TO_EDGE,
                XV_HEIGHT, WIN_EXTEND_TO_EDGE,
                NULL);
	return ip;
}

/*
 * Create object `undelete_popup' in the specified instance.
 */
Xv_opaque
xvnews_undelete_popup_undelete_popup_create(ip, owner)
	xvnews_undelete_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 530,
		XV_HEIGHT, 150,
		XV_LABEL, "Undelete Articles",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		FRAME_LEFT_FOOTER, "Articles killed this session",
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	xv_set(obj, WIN_CONSUME_EVENTS,
		NULL, NULL);
	notify_interpose_event_func(obj,
		(Notify_func) undelete_popup_event, NOTIFY_SAFE);
	return obj;
}

/*
 * Create object `controls3' in the specified instance.
 */
Xv_opaque
xvnews_undelete_popup_controls3_create(ip, owner)
	xvnews_undelete_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `undelete_list' in the specified instance.
 */
Xv_opaque
xvnews_undelete_popup_undelete_list_create(ip, owner)
	xvnews_undelete_popup_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 8,
		PANEL_LIST_WIDTH, -1,
		PANEL_LIST_DISPLAY_ROWS, 4,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
		PANEL_LIST_INSERT_DUPLICATE, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `undelete_butt' in the specified instance.
 */
Xv_opaque
xvnews_undelete_popup_undelete_butt_create(ip, owner)
	xvnews_undelete_popup_objects	*ip;
	Xv_opaque	owner;
{
	extern void		undelete_func();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 160,
		XV_Y, 112,
		PANEL_LABEL_STRING, "undelete",
		PANEL_NOTIFY_PROC, undelete_func,
		NULL);
	return obj;
}
