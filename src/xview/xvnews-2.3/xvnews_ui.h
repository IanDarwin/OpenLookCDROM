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
#ifndef	xvnews_HEADER
#define	xvnews_HEADER

extern Attr_attribute	INSTANCE;

extern Xv_opaque        xvnews_post_menu_create();
extern Xv_opaque        xvnews_search_menu_create();
extern Xv_opaque        xvnews_header_menu_create();
extern Xv_opaque        xvnews_save_menu_create();
extern Xv_opaque        xvnews_view_groups_create();
extern Xv_opaque        xvnews_dist_menu_create();
extern Xv_opaque        xvnews_kill_menu_create();
extern Xv_opaque        xvnews_include_menu_create();
extern Xv_opaque        xvnews_catchup_menu_create();
extern Xv_opaque        xvnews_kill_string_menu_create();
extern Xv_opaque        xvnews_kill_file_menu_create();
extern Xv_opaque        xvnews_prev_menu_create();
extern Xv_opaque        xvnews_next_menu_create();
extern Xv_opaque        xvnews_done_menu_create();

typedef struct {
	Xv_opaque	xvnews_window;
	Xv_opaque	controls2;
	Xv_opaque	groups_list;
	Xv_opaque	articles_list;
	Xv_opaque	controls1;
	Xv_opaque       done_butt;
	Xv_opaque       done_groups_butt;
	Xv_opaque	quit_butt;
	Xv_opaque       sort_newsrc_butt;
	Xv_opaque       prev_butt;
	Xv_opaque       unsubscribe_button;
	Xv_opaque	rescan_butt;
	Xv_opaque	catchup_butt;
	Xv_opaque	unsub_butt;
	Xv_opaque	mark_unread_butt;
	Xv_opaque       search_butt;
	Xv_opaque	save_article_button;
	Xv_opaque	update_butt;
	Xv_opaque	next_butt;
	Xv_opaque       goto_button;
	Xv_opaque	next_group_butt;
	Xv_opaque	all_butt;
	Xv_opaque       subscribe_button;
	Xv_opaque	all_groups;
	Xv_opaque	print_butt;
	Xv_opaque	properties_butt;
	Xv_opaque       kill_butt;
	Xv_opaque	post_button;
	Xv_opaque       post_men_butt;
	Xv_opaque	article_window;
} xvnews_xvnews_window_objects;

extern xvnews_xvnews_window_objects	*xvnews_xvnews_window_objects_initialize();

extern Xv_opaque	xvnews_xvnews_window_xvnews_window_create();
extern Xv_opaque	xvnews_xvnews_window_controls2_create();
extern Xv_opaque	xvnews_xvnews_window_groups_list_create();
extern Xv_opaque	xvnews_xvnews_window_articles_list_create();
extern Xv_opaque	xvnews_xvnews_window_controls1_create();
extern Xv_opaque	xvnews_xvnews_window_quit_butt_create();
extern Xv_opaque	xvnews_xvnews_window_done_butt_create();
extern Xv_opaque	xvnews_xvnews_window_done_groups_butt_create();
extern Xv_opaque        xvnews_xvnews_window_sort_newsrc_butt_create();
extern Xv_opaque        xvnews_xvnews_window_prev_butt_create();
extern Xv_opaque	xvnews_xvnews_window_rescan_butt_create();
extern Xv_opaque	xvnews_xvnews_window_catchup_butt_create();
extern Xv_opaque	xvnews_xvnews_window_unsub_butt_create();
extern Xv_opaque	xvnews_xvnews_window_mark_unread_butt_create();
extern Xv_opaque        xvnews_xvnews_window_search_butt_create();
extern Xv_opaque	xvnews_xvnews_window_save_article_button_create();
extern Xv_opaque	xvnews_xvnews_window_update_butt_create();
extern Xv_opaque	xvnews_xvnews_window_next_butt_create();
extern Xv_opaque        xvnews_xvnews_window_goto_button_create();
extern Xv_opaque	xvnews_xvnews_window_next_group_butt_create();
extern Xv_opaque	xvnews_xvnews_window_all_butt_create();
extern Xv_opaque        xvnews_xvnews_window_unsubscribe_button_create();
extern Xv_opaque        xvnews_xvnews_window_subscribe_button_create();
extern Xv_opaque	xvnews_xvnews_window_all_groups_create();
extern Xv_opaque	xvnews_xvnews_window_print_butt_create();
extern Xv_opaque	xvnews_xvnews_window_properties_butt_create();
extern Xv_opaque        xvnews_xvnews_window_kill_butt_create();
extern Xv_opaque	xvnews_xvnews_window_post_button_create();
extern Xv_opaque        xvnews_xvnews_window_post_men_butt_create();
extern Xv_opaque	xvnews_xvnews_window_article_window_create();

typedef struct {
	Xv_opaque	props;
	Xv_opaque	props_controls;
	Xv_opaque	props_color_class;
	Xv_opaque	color_menu_button;
	Xv_opaque	props_color;
	Xv_opaque	props_font_class;
	Xv_opaque	props_font;
	Xv_opaque	props_fontsize;
	Xv_opaque       display_text;
	Xv_opaque       rescan_text;
	Xv_opaque       header_select;
	Xv_opaque       sort;
        Xv_opaque       indent_text;
	Xv_opaque       filter_text;
	Xv_opaque       log_text;
        Xv_opaque       print_text;
	Xv_opaque       editor_text;
	Xv_opaque	props_apply;
	Xv_opaque	props_reset;
	Xv_opaque	props_done;
} xvnews_props_objects;

extern xvnews_props_objects	*xvnews_props_objects_initialize();

extern Xv_opaque	xvnews_props_props_create();
extern Xv_opaque	xvnews_props_props_controls_create();
extern Xv_opaque	xvnews_props_props_color_class_create();
extern Xv_opaque	xvnews_props_color_menu_button_create();
extern Xv_opaque	xvnews_props_props_color_create();
extern Xv_opaque	xvnews_props_props_font_class_create();
extern Xv_opaque	xvnews_props_props_font_create();
extern Xv_opaque	xvnews_props_props_fontsize_create();
extern Xv_opaque        xvnews_props_display_text_create();
extern Xv_opaque        xvnews_props_rescan_text_create();
extern Xv_opaque        xvnews_props_sort_create();
extern Xv_opaque        xvnews_props_header_select_create();
extern Xv_opaque        xvnews_props_indent_text_create();
extern Xv_opaque        xvnews_props_print_text_create();
extern Xv_opaque        xvnews_props_editor_text_create();
extern Xv_opaque        xvnews_props_filter_text_create();
extern Xv_opaque        xvnews_props_log_text_create();
extern Xv_opaque	xvnews_props_props_apply_create();
extern Xv_opaque	xvnews_props_props_reset_create();
extern Xv_opaque	xvnews_props_props_done_create();

typedef struct {
	Xv_opaque	post_popup;
	Xv_opaque	post_controls;
	Xv_opaque	deliver_butt;
	Xv_opaque	cancel_butt;
	Xv_opaque       include_butt;
	Xv_opaque       dist_butt;
	Xv_opaque       log_choice;
	Xv_opaque       post_group;
	Xv_opaque	post_window;
} xvnews_post_popup_objects;

extern xvnews_post_popup_objects	*xvnews_post_popup_objects_initialize();

extern Xv_opaque	xvnews_post_popup_post_popup_create();
extern Xv_opaque	xvnews_post_popup_post_controls_create();
extern Xv_opaque	xvnews_post_popup_deliver_butt_create();
extern Xv_opaque	xvnews_post_popup_cancel_butt_create();
extern Xv_opaque        xvnews_post_popup_dist_butt_create();
extern Xv_opaque        xvnews_post_popup_log_choice_create();
extern Xv_opaque        xvnews_post_popup_include_butt_create();
extern Xv_opaque	xvnews_post_popup_post_window_create();
extern Xv_opaque        xvnews_post_popup_post_group_create();

typedef struct {
        Xv_opaque       search_popup;
        Xv_opaque       controls4;
        Xv_opaque       search_text;
        Xv_opaque       header_text;
	Xv_opaque       show_groups_butt;
        Xv_opaque       header_choice;
	Xv_opaque       srch_prev_butt;
        Xv_opaque       srch_next_button;
	Xv_opaque       search_group1;
	Xv_opaque       search_group2;
	Xv_opaque       search_group3;
} xvnews_search_popup_objects;

extern xvnews_search_popup_objects      *xvnews_search_popup_objects_initialize();

extern Xv_opaque        xvnews_search_popup_search_group1_create();
extern Xv_opaque        xvnews_search_popup_search_group2_create();
extern Xv_opaque        xvnews_search_popup_search_group3_create();
extern Xv_opaque        xvnews_search_popup_search_popup_create();
extern Xv_opaque        xvnews_search_popup_controls4_create();
extern Xv_opaque        xvnews_search_popup_search_text_create();
extern Xv_opaque        xvnews_search_popup_show_groups_butt_create();
extern Xv_opaque        xvnews_search_popup_header_choice_create();
extern Xv_opaque        xvnews_search_popup_header_text_create();
extern Xv_opaque        xvnews_search_popup_srch_prev_butt_create();
extern Xv_opaque        xvnews_search_popup_srch_next_button_create();

typedef struct {
        Xv_opaque       kill_popup;
        Xv_opaque       kill_controls;
	Xv_opaque       file_list;
        Xv_opaque       load_kill_butt;
        Xv_opaque       delete_kill_butt;
        Xv_opaque       save_butt;
	Xv_opaque       kill_choice;
        Xv_opaque       kill_text;
        Xv_opaque       mod_choice;
        Xv_opaque       command_choice;
	Xv_opaque       changes_choice;
        Xv_opaque       add_local_butt;
        Xv_opaque       add_global_butt;
} kill_popup_objects;

extern kill_popup_objects        *kill_popup_objects_initialize();
extern Xv_opaque        kill_popup_kill_popup_create();
extern Xv_opaque        kill_popup_kill_controls_create();
extern Xv_opaque        kill_popup_kill_text_create();
extern Xv_opaque        kill_popup_mod_choice_create();
extern Xv_opaque        kill_popup_command_choice_create();
extern Xv_opaque        kill_popup_add_local_butt_create();
extern Xv_opaque        kill_popup_add_global_butt_create();
extern Xv_opaque        kill_popup_kill_choice_create();
extern Xv_opaque        kill_popup_file_list_create();
extern Xv_opaque        kill_popup_load_kill_butt_create();
extern Xv_opaque        kill_popup_delete_kill_butt_create();
extern Xv_opaque        kill_popup_save_butt_create();
extern Xv_opaque        kill_popup_changes_choice_create();

typedef struct {
        Xv_opaque       undelete_popup;
        Xv_opaque       controls3;
        Xv_opaque       undelete_list;
        Xv_opaque       undelete_butt;
} xvnews_undelete_popup_objects;

extern xvnews_undelete_popup_objects    *xvnews_undelete_popup_objects_initialize();

extern Xv_opaque        xvnews_undelete_popup_undelete_popup_create();
extern Xv_opaque        xvnews_undelete_popup_controls3_create();
extern Xv_opaque        xvnews_undelete_popup_undelete_list_create();
extern Xv_opaque        xvnews_undelete_popup_undelete_butt_create();
#endif
