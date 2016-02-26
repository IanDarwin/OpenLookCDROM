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
 * part of Resumix, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * Resumix, Inc. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Resumix, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Resumix has been advised of the possibility of such damages.
 *
 * Resumix, Inc.
 */

#ifndef _XVNEWS_H
#define _XVNEWS_H

#include <xview/xview.h>
#include <xview/base.h>
#include <xview/window.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/font.h>
#include <xview/textsw.h>
#include "gfm.h"
#include "xvnews_ui.h"
#include "config.h"

/* Turn assertions on when explicitly debugging, or when in
   beta testing */
/* Hmm, might not work, and it might be better to leave the
   assertions in anyway. Comment it out for now */
/*
#if !defined(DEBUG) && !defined(BETA)
#define NDEBUG
#endif
*/
#include <assert.h>

extern Attr_attribute POST_PARENT;
extern Attr_attribute PROPS_POPUP;
extern Attr_attribute SAVE_POPUP;
extern Attr_attribute POST_POPUP;
extern Attr_attribute POST_TYPE;
extern Attr_attribute SEARCH_POPUP;
extern Attr_attribute KILL_POPUP;
extern Attr_attribute UNDELETE_POPUP;
extern Attr_attribute LOAD_POPUP;

#define BUFFERLEN       8192
#define GROUP_MODE 1
#define SUBSCRIBED 1
#define UNSUBSCRIBED 0
#define NO_GROUP 411
#define SUBJECT_LENGTH 48
#define AUTHOR_LENGTH 23
#define ARTICLE_MODE 0
#define NEW_GROUPS 2
#define ALL_GROUPS 3
#define SUB_GROUPS 5
#define UNSUB_GROUPS 6
#define MATCH_GROUPS 7
#define ALL_ARTICLES 4
#define MAX_COMMAND_LEN 128
#define MAX_MESSAGE_LEN 512
#define MAX_ARTICLE_LEN 1024
#define POSTED_OK 240
#define NEWS_REPLY 1
#define NEWS_POST 4
#define MAIL_REPLY 2
#define MAIL_FORWARD 3
#define MAX_NEWS_GROUPS 1000
#define MAX_ARTICLES    5000
#define MAX_SUBSCRIBED_GROUPS   100

#define DEFAULT_SIGNATURE_SEPARATOR "\n-- \n"

typedef struct {
   char *string;
   char *xref;
   char *mesgid;
   char *subject;
   int len;
} s_node;

struct	post_node {
	int	post;
	xvnews_xvnews_window_objects	*ip;
} ;

struct kill_node {
	char	*string;
	int subject;
        int mixedcase;
	int junk;
	int place;
	struct kill_node *next;
} ;

struct message {
	char *message;
	struct message *next;
} ;

struct kill_list {
	int article;
	int global;
	int junk;
	int place;
	struct kill_list *next;
} ;

/* The a_node is a struct which contains a part of the complete
   article list of a certain newsgroup. The a_node's are linked
   together, and he newsgroup uses artlist to point to the head of the
   list. The following concentions are used with a_node's: 1) If there
   are no articles read in a group, there should be no a_node at
   all. artlist will be NULL in this case. This is represented by a
   single ':' in the .newsrc file. 2) If the range denotes a single
   article, the first should equal last. If last equals 0, then this
   is supported also. */

struct article_node_struct {
  int first;
  int last;
  struct article_node_struct *nextart;
};

typedef struct article_node_struct a_node;


struct newsrc_node {
  char    *newsgroup;
  char    *description;
  int     subscribed;
  int     articles;
  int     last_article;
  struct	kill_node *kill;
  struct newsrc_node      *nextgroup;
  a_node *artlist;
} ;



struct globals {
        int     mode;
        int     post_popups;
        int     unread;
        int     dragging;
        int     check_drag;
        int     total;
        int     list;
        int     multiclick;
        int     article;
	int	nnrp;
	int	connected;
  struct newsrc_node *group;
        char    old_group[64];
        char    next_group[64];
	char	newsdir[256];
	char	newsrc[256];
        char    *home;
	char	*single;
        Xv_Cursor       copyso_cursor;
        Xv_font  listfont;
	Icon	news_icon;
        struct newsrc_node *head;
	struct kill_node *kill;
	struct message *messages;
	int subs_read;
	int num_subs;
	char **subslist;
  int undelete_popup;
} ;


/* Some additional defines help to compile everything */
#if defined( __STDC__ )
#define STATIC_FUNCTION( proc, arg ) static proc arg
#define STATIC static
#else
#define STATIC_FUNCTION( proc, arg ) proc ()
#define STATIC 
#endif

/* getdate.y */
EXTERN_FUNCTION( time_t get_date, (char *, time_t, long));

/* main.c */
extern struct globals *Global;

/* server.c */
EXTERN_FUNCTION( void put_server, (char *, DOTDOTDOT));
EXTERN_FUNCTION( int get_server, (char *, int));
EXTERN_FUNCTION( void close_server, ());
EXTERN_FUNCTION( void connect_server, (char *));
EXTERN_FUNCTION( char *get_nntp_server, ());
EXTERN_FUNCTION( void reconnect_server, ());
EXTERN_FUNCTION( Notify_value signal_handler, ());
EXTERN_FUNCTION( char *nntp_gtitle, (char *));

/* textsw_regexp.c */
EXTERN_FUNCTION( int textsw_file_line_count, (Textsw, char *));
EXTERN_FUNCTION( int textsw_find_exp, (Textsw, char *,
				       Textsw_index *, Textsw_index*));

/* xvnews_stubs.c */
EXTERN_FUNCTION( int check_drag, (xvnews_xvnews_window_objects *, Event *));
EXTERN_FUNCTION( void update_close_proc, (Menu, Menu_item));
EXTERN_FUNCTION( int load_file, (gfm_popup_objects *, char *, char *));
EXTERN_FUNCTION( void kill_notify, (Menu, Menu_item));
EXTERN_FUNCTION( int save_file, (gfm_popup_objects *, char *, char *));
EXTERN_FUNCTION( Notify_value undelete_popup_event,
		 (Xv_window, Event *, Notify_arg, Notify_event_type));

/* xv_articles.c */
EXTERN_FUNCTION( void get_groups, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int get_prev, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void next_article_proc,
		 (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int next_group, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void print_article, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int read_article, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void retrieve_article, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void save_article_proc, (gfm_popup_objects *, char *));
EXTERN_FUNCTION( void unread_proc, (xvnews_xvnews_window_objects *));

/* xv_drag.c */
EXTERN_FUNCTION( void drag_dnd_drop, (Frame, xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void drag_feedback, (Frame, xvnews_xvnews_window_objects *));

/* xv_err.c */
EXTERN_FUNCTION( void xvnews_err,
		 (xvnews_xvnews_window_objects *, char *, DOTDOTDOT));

/* xv_getauthor.c */
EXTERN_FUNCTION( void getauthorlist, (char **, int, int, int));

/* xv_init.c */
EXTERN_FUNCTION( void init_cursor, ());
EXTERN_FUNCTION( void init_destroy_func, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void init_dist_menu, ());
EXTERN_FUNCTION( void init_groups_search, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void init_rescan_timer, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void init_labels, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void init_globals, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void initGlobalValues, ());
EXTERN_FUNCTION( void initIcon, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void initPostEditor, (xvnews_xvnews_window_objects *, int));

/* xv_init_newsrc.c */
EXTERN_FUNCTION( void check_newsrc, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( int sort_newsrc, ());

/* xv_kill.c */
EXTERN_FUNCTION( int addKill,
		 (kill_popup_objects *, char *, char *, int, int));
EXTERN_FUNCTION( int articleKill,
		 (xvnews_xvnews_window_objects *, struct kill_list *));
EXTERN_FUNCTION( void clearKills, (struct kill_node *));
EXTERN_FUNCTION( void confirmKill, (kill_popup_objects *));
EXTERN_FUNCTION( char *currentAuthor, (int));
EXTERN_FUNCTION( char *currentSubject, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void fillKillText, 
		 (xvnews_xvnews_window_objects *, kill_popup_objects *));
EXTERN_FUNCTION( void generateKillList, (struct newsrc_node *));
EXTERN_FUNCTION( void genMessage, (int));
EXTERN_FUNCTION( char *groupDir, (char *));
EXTERN_FUNCTION( void freeMessages, ());
EXTERN_FUNCTION( int headerKill, (struct kill_node *));
EXTERN_FUNCTION( struct kill_list *killHeaders,
		 (struct kill_node *, struct kill_node *, int, int,
		  struct kill_list *));
EXTERN_FUNCTION( int loadKill, (kill_popup_objects *, char *));
EXTERN_FUNCTION( struct kill_list *messageKill,
		 (int, int, struct kill_list *));
EXTERN_FUNCTION( void removeMessage, (int));
EXTERN_FUNCTION( int saveKillFile, (kill_popup_objects *));
EXTERN_FUNCTION( void sessionHeaderKill, (xvnews_xvnews_window_objects *,
					  kill_popup_objects *,
					  struct kill_node *));
EXTERN_FUNCTION( void sessionSubjectKill, (xvnews_xvnews_window_objects *, 
					   kill_popup_objects *,
					   struct kill_node *));
EXTERN_FUNCTION( struct kill_list *subjectKill,
		 (char *, struct kill_node *, struct kill_list *));
EXTERN_FUNCTION( void undeleteAll, (xvnews_xvnews_window_objects *));

/* xv_newsrc.c */
EXTERN_FUNCTION( int getactive, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void group_change, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int save_newsrc, ());
EXTERN_FUNCTION( void unsub_group, ());
EXTERN_FUNCTION( void update_newsrc, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void catchup_group, (xvnews_xvnews_window_objects *));

/* xv_parse_newsrc.c */
EXTERN_FUNCTION( void add_articles, (struct newsrc_node *, int, int));
EXTERN_FUNCTION( void add_unread, (struct newsrc_node *, int));
EXTERN_FUNCTION( void clear_artlist, (struct newsrc_node *));
EXTERN_FUNCTION( void delete_articles,
		 (xvnews_xvnews_window_objects *, struct newsrc_node *));
EXTERN_FUNCTION( void getReadArticles,
		 (xvnews_xvnews_window_objects *, struct newsrc_node *));
EXTERN_FUNCTION( int is_unread, (struct newsrc_node *, int));
EXTERN_FUNCTION( struct newsrc_node *parse_line, (char *, char *));
EXTERN_FUNCTION( int unread_articles, (struct newsrc_node *));

/* xv_post.c */
EXTERN_FUNCTION( char *addSignature, (char *, char *));
EXTERN_FUNCTION( void cancel_message, (xvnews_post_popup_objects *));
EXTERN_FUNCTION( char *checkPeriods, (char *, int));
EXTERN_FUNCTION( void deliver_message, (xvnews_post_popup_objects *));
EXTERN_FUNCTION( int filePost,
		 (xvnews_post_popup_objects *, xvnews_xvnews_window_objects *, 
		  char *));
EXTERN_FUNCTION( void get_news_header, (xvnews_post_popup_objects *, int));
EXTERN_FUNCTION( void get_mail_header, (xvnews_post_popup_objects *, int));
EXTERN_FUNCTION( char *get_messageid, ());
EXTERN_FUNCTION( int headToFile, (char *, char *));
EXTERN_FUNCTION( void includeFile, (Xv_window, char *));
EXTERN_FUNCTION( char *mailHead, (xvnews_xvnews_window_objects *, int, int *));
EXTERN_FUNCTION( char *newsHead,
		 (xvnews_xvnews_window_objects *, int, Textsw_index *));

/* xv_props.c */
EXTERN_FUNCTION( void apply_defaults, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void apply_post_defaults, 
		 (xvnews_post_popup_objects *, gfm_popup_objects *));
EXTERN_FUNCTION( void apply_props, (Panel_item, Event *));
EXTERN_FUNCTION( char *include_news, (int, char *, char *));

/* xv_regex.c */
EXTERN_FUNCTION( char *xvnews_comp, (char *));
EXTERN_FUNCTION( int xvnews_exec, (char *));
EXTERN_FUNCTION( void xvnews_free, ());

/* xv_search.c */
EXTERN_FUNCTION( int author_search, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int exp_search, (xvnews_search_popup_objects *, int));
EXTERN_FUNCTION( int isRead, (char *));
EXTERN_FUNCTION( void kill_subject, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int next_unread_art, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( int subject_search, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( int undeleteKill, (xvnews_xvnews_window_objects *,
				    xvnews_props_objects *, char *, int));

/* xv_stuff.c */
EXTERN_FUNCTION( void article_set, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void describe_group, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void groups_set, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void init_list_font, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void new_group_set, (xvnews_xvnews_window_objects *));
EXTERN_FUNCTION( void resize_panel, (xvnews_xvnews_window_objects *, int));
EXTERN_FUNCTION( void undescribe_group, (xvnews_xvnews_window_objects *));

/* xv_sort.c */
#define XV_SORT_ARTNUM 1
#define XV_SORT_SUBJECT 2
EXTERN_FUNCTION( void sortSubjects, (xvnews_xvnews_window_objects *, int));


#endif /* _XVNEWS_H */
