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
#include <ctype.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include <xview/cursor.h>
#include <xview/notice.h>
#include <gdd.h>
#include <gfm_ui.h>
#include <gfm.h>
#include <gio.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"
#include "patchlevel.h"

extern struct globals *Global;

static unsigned short copyso_image[] = {
#include "copyso.cursor"
	};
mpr_static(copyso_pr, ICON_DEFAULT_WIDTH, ICON_DEFAULT_HEIGHT, 1, copyso_image);


#ifdef SVR4
STATIC_FUNCTION( Notify_value postDone, (struct post_node *, int,
					 int *,	void *));
#else
STATIC_FUNCTION( Notify_value postDone, (struct post_node *, int,
					 union wait *, struct rusage *));
#endif
STATIC_FUNCTION( int confirmDelivery, (xvnews_xvnews_window_objects *, int));
STATIC_FUNCTION( void editArticle, (xvnews_xvnews_window_objects *, char *,
				    char *, char *, int, int));
STATIC_FUNCTION( char *deliverNews, (xvnews_xvnews_window_objects *, char *,
				     struct stat *));

extern void init_cursor()
{
	int	xhot, yhot;

	xhot = ICON_DEFAULT_WIDTH / 2 + 11;
        yhot = ICON_DEFAULT_WIDTH / 2 + 1;
        Global->copyso_cursor= xv_create(0, CURSOR,
            CURSOR_IMAGE,   &copyso_pr,
            CURSOR_XHOT,    xhot,
            CURSOR_YHOT,    yhot,
            CURSOR_OP,      PIX_SRC ^ PIX_DST,
            0);
}

extern void init_rescan_timer(ip)
xvnews_xvnews_window_objects    *ip;
{
        int             i;
	struct itimerval    timer;
	Notify_value        rescan_timer();
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);

	i = xv_get(prp->rescan_text, PANEL_VALUE, NULL);
                i *= 60;
                if (i < 300)
                        i = 300;
                if (i > 3600)
                        i = 3600;
                timer.it_interval.tv_usec = 0;
                timer.it_interval.tv_sec = i;
                timer.it_value.tv_usec = 0;
                timer.it_value.tv_sec = i;

                notify_set_itimer_func(ip->xvnews_window, rescan_timer, ITIMER_REAL,
			 &timer, NULL);

} 

extern void init_labels(ip)
xvnews_xvnews_window_objects    *ip;
{
	char	label[120];
	char *ptr;

#ifdef BETA
	sprintf(label, "xvnews Beta version %-1.1f", VERSION);
#else	
	sprintf(label, "xvnews version %-1.1f", VERSION);
#endif
	ptr = (char *)&label + strlen(label);

	if (PATCHLEVEL) {
	  sprintf(ptr, " (PL %d)", PATCHLEVEL);
	  ptr = (char *)&label + strlen(label);
	}

	sprintf(ptr, " connected to %s server %s\n", 
	Global->nnrp ? "INN":"NNTP", get_nntp_server());

	xv_set(ip->xvnews_window, FRAME_LABEL, label, NULL);
}

extern void init_globals(ip)
xvnews_xvnews_window_objects    *ip;
{
	Global = (struct globals *)malloc(sizeof(struct globals));
	if (Global == NULL) {
		printf("Malloc of struct global failed!\n");
		exit(-1);
	}
	Global->dragging = 0;
	Global->check_drag = 0;
	Global->head = NULL;
	Global->mode = GROUP_MODE;
	Global->list = 1;
	Global->post_popups = 0;
	Global->kill = NULL;
	Global->nnrp = 0;
	Global->news_icon = (Icon)xv_get(ip->xvnews_window, FRAME_ICON);
	Global->home = NULL;
	Global->single = NULL;
	Global->messages = NULL;
	memset(Global->old_group, '\0', 64);
	Global->subs_read = 0;
	Global->num_subs = 0;
	Global->subslist = NULL;
	Global->undelete_popup = FALSE;
}

extern void initGlobalValues()
{
	struct stat	statbuf;

	if (getenv("HOME"))
	  Global->home = strdup(getenv("HOME"));
	else {
	  printf("xvnews: unable to find your HOME, exiting.\n");
	  exit(1);
	}

	if (getenv("SAVEDIR") == NULL)
		sprintf(Global->newsdir, "%s/News", Global->home);
	else
		strcpy(Global->newsdir, getenv("SAVEDIR"));

	if (Global->single != NULL) {
		sprintf(Global->newsrc, "%s/.%s", Global->home, Global->single);
		return;
	}

	if (getenv("DOTDIR") == NULL) {
		sprintf(Global->newsrc, "%s/.newsrc-%s", Global->home, get_nntp_server());
                if (stat(Global->newsrc, &statbuf))
                        sprintf(Global->newsrc, "%s/.newsrc", Global->home);
        } else {
                sprintf(Global->newsrc, "%s/.newsrc-%s", getenv("DOTDIR"), get_nntp_server());
                if (stat(Global->newsrc, &statbuf))
                        sprintf(Global->newsrc, "%s/.newsrc", getenv("DOTDIR"));
        }
}

extern void initIcon(ip)
xvnews_xvnews_window_objects    *ip;
{
	static unsigned short   nonews_icon[] = {
#include "nonews.icon"
	};
	static unsigned short   news_icon[] = {
#include "news.icon"
	};
	Server_image	xvnews_icon;
	
	if (Global->mode == GROUP_MODE &&
		!xv_get(ip->groups_list, PANEL_LIST_NROWS)) 
		xvnews_icon = xv_create(XV_NULL, SERVER_IMAGE,
			SERVER_IMAGE_BITS, nonews_icon,
			SERVER_IMAGE_DEPTH, 1,
			XV_WIDTH, 64,
			XV_HEIGHT, 64,
			NULL);
	else 
		xvnews_icon = xv_create(XV_NULL, SERVER_IMAGE,
			SERVER_IMAGE_BITS, news_icon,
			SERVER_IMAGE_DEPTH, 1,
			XV_WIDTH, 64,
			XV_HEIGHT, 64,
			NULL);

	xv_set(Global->news_icon,
		ICON_IMAGE, xvnews_icon,
		WIN_RETAINED, TRUE,
		NULL);

	xv_set(ip->xvnews_window, FRAME_ICON, Global->news_icon, NULL);
}
						
extern void init_dist_menu(ip)
xvnews_post_popup_objects       *ip;
{
	char		message[MAX_MESSAGE_LEN], *dist;
	int		status;
	char		*c;
	Menu_item	mi;
	void            dist_notify();

	if (!Global->list)
		return;
	put_server("LIST DISTRIBUTIONS");
	status = get_server(message, sizeof(message));
	if (status != OK_GROUPS) {
		if (strstr(message, "Timeout") != NULL) {
			reconnect_server();
			put_server("LIST DISTRIBUTIONS");
			status = get_server(message, sizeof(message));
		}
		if (status != OK_GROUPS) {
			mi = (Menu_item) xv_create((Xv_opaque)NULL, MENUITEM,
				XV_KEY_DATA, INSTANCE, ip,
				MENU_STRING,	"world",
				MENU_NOTIFY_PROC, dist_notify,
				MENU_RELEASE,
				NULL);
			xv_set((Menu)xv_get(ip->dist_butt, PANEL_ITEM_MENU,
				NULL), MENU_APPEND_ITEM, mi, NULL);
			return;
		}
	}
	while (*message != '.') {
		get_server(message, sizeof(message));
		if (*message != '.') {
			dist = (char *) malloc(80);
			if (dist == NULL) {
				printf("Malloc failed\n");
				return;
			}
			sscanf(message, "%s", dist);
			c = (message + strlen(dist));
			while (*c++ == '	');
			sprintf(dist, "%s -- %s", dist, --c);
			mi = (Menu_item)xv_create((Xv_opaque)NULL, MENUITEM,
				XV_KEY_DATA, INSTANCE, ip,
				MENU_STRING,    dist,
				MENU_NOTIFY_PROC, dist_notify,
				MENU_RELEASE,
				NULL);
			xv_set((Menu)xv_get(ip->dist_butt, PANEL_ITEM_MENU,
				NULL), MENU_APPEND_ITEM, mi, NULL);
		}
	}
}

extern void init_destroy_func(ip)
xvnews_xvnews_window_objects    *ip;
{
	Notify_value	destroy_main(), cleanUp();

	notify_interpose_destroy_func(ip->xvnews_window, destroy_main);
	notify_set_destroy_func(ip->xvnews_window, cleanUp);
}


Notify_value
destroy_main(frame, status)
Frame	frame;
Destroy_status	status;
{
	Notify_value	cleanUp();

	switch(status) {
		case DESTROY_CHECKING:
			cleanUp(0);
			return NOTIFY_DONE;
			break;
		case DESTROY_PROCESS_DEATH:
			cleanUp(1);
			break;
		case DESTROY_CLEANUP:
			cleanUp(0);
			break;
		case DESTROY_SAVE_YOURSELF:
			save_newsrc();
			return NOTIFY_DONE;
			break;
	}

	return notify_next_destroy_func(frame, status);

}

Notify_value
cleanUp(flg)
int	flg;
{
	char    file[256];
 
        sprintf(file, "/tmp/.xvnews.file.%d", getpid());
        unlink(file);
        if (save_newsrc())
               printf("An error occured while saving your .newsrc!\n");
        close_server();
	if (flg)
		exit(0);

	return NOTIFY_DONE;
}

extern void init_groups_search(ip)
xvnews_xvnews_window_objects    *ip;
{
	xvnews_search_popup_objects * srp = (xvnews_search_popup_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, NULL);

	xv_set(srp->header_choice, XV_SHOW, FALSE, NULL); 
	xv_set(srp->header_text, XV_SHOW, FALSE, NULL); 
	xv_set(srp->srch_prev_butt, XV_SHOW, FALSE, NULL); 
	xv_set(srp->srch_next_button, XV_SHOW, FALSE, NULL); 
	xv_set(srp->show_groups_butt, XV_SHOW, TRUE, NULL);
	xv_set(srp->search_text, PANEL_VALUE, "", NULL);
	xv_set(srp->search_popup, XV_LABEL, "Groups matching string", NULL);
	xv_set(srp->search_popup, FRAME_LEFT_FOOTER, "\n", NULL);
	xv_set(srp->search_popup, XV_SHOW, TRUE, NULL);
	xv_set(srp->search_popup, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
}

extern void initPostEditor(ip, post)
xvnews_xvnews_window_objects    *ip;
int	post;
{
	int	art, point;
	char	file1[128], *head, *old, *indent=NULL;
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	char	*sig;

	if (Global->mode == ARTICLE_MODE || Global->mode == ALL_ARTICLES) {
		old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
			xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED));
		sscanf(old, "%d", &art);
		indent = (char *)xv_get(prp->indent_text, PANEL_VALUE, NULL);
	}

#ifdef ADD_SIGNATURE
	sprintf(file1, "%s/.signature", Global->home);
#endif
	if (post == MAIL_FORWARD || post == MAIL_REPLY) {
		head = mailHead(ip, post, &point);
#ifdef ADD_SIGNATURE
		sig = addSignature(file1, NULL);
#endif
	} else {
		char	file2[128];

		head = newsHead(ip, post, (Textsw_index *)&point);
#ifdef ADD_SIGNATURE
		if (Global->group) {
			sprintf(file2, "%s/%s/.signature", 
				Global->newsdir,
				groupDir(Global->group->newsgroup));
			sig = addSignature(file1, file2);
		} else
			sig = addSignature(file1, NULL);
#endif
	}

	editArticle(ip, head, indent, sig, art, post);
}

static void editArticle(ip, buff, indent, sig, art, post)
xvnews_xvnews_window_objects    *ip;
char	*buff, *indent, *sig;
int 	art, post;
{
	char	file[64], *command;
	int	pid;
	struct	post_node	*post_type;
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	char	*editor = (char *)xv_get(prp->editor_text, PANEL_VALUE);

	post_type = (struct post_node *)malloc(sizeof(struct post_node));
	post_type->post = post;
	post_type->ip = ip;
	switch (pid = fork()) {
		case -1:
			xvnews_err(ip, "fork failed for %s!\n", editor);
			return;
		case  0:
			sprintf(file, "/tmp/.xvnews.post%d", getpid());
			headToFile(buff, file);
			if ((art && post != NEWS_POST) &&
			    (Global->mode == ARTICLE_MODE ||
			     Global->mode == ALL_ARTICLES)) {
			  sprintf(buff, "In article <%s>, %s writes:\n",
				  get_messageid(), currentAuthor(0));
			  headToFile(buff, file);
			  include_news(art, file, indent);
			}
			if (sig != NULL) {
			  headToFile(sig, file);
			}
			command = (char*)malloc(strlen(editor) + strlen(file) + 2);
			sprintf(command, "%s %s", editor, file);
			execl("/bin/csh", "csh", "-f", "-c", command, (char *)NULL);
			printf("execl of %s failed!\n", editor);
			_exit(127);
		default:
			if (sig != NULL)
				free(sig);
			notify_set_wait3_func((Frame)post_type, postDone, pid);
			sleep(2);
	}
}

static Notify_value
postDone(post_type, pid, status, rusage)
struct	post_node	*post_type;
int	pid;
#ifdef SVR4
int             *status;
void *rusage;
#else
union wait	*status;
struct rusage *rusage;
#endif
{
	char	file[64];
	struct	stat	statbuf;

	sprintf(file, "/tmp/.xvnews.post%d", pid);
	if (post_type->post == MAIL_REPLY || post_type->post == MAIL_FORWARD) {
		char	command[128];

		sprintf(command, "%s < %s", MAIL_PROGRAM, file);
		if (!stat(file, &statbuf) &&
			confirmDelivery(post_type->ip, post_type->post)) {
			system(command);
			xvnews_err(post_type->ip, "Mail message sent\n");
		} else
			xvnews_err(post_type->ip, "Mail message aborted\n");
	} else {
		char	*err;

		if (stat(file, &statbuf) ||
		    !confirmDelivery(post_type->ip, post_type->post))
			xvnews_err(post_type->ip, "News article aborted\n");
		else
			if ((err = deliverNews(post_type->ip, file, &statbuf)) != NULL)
				editArticle(post_type->ip, err, NULL, NULL, 0, post_type->post);
	}

	unlink(file);
	free(post_type);

	return NOTIFY_DONE;
}

STATIC int confirmDelivery(ip, post)
xvnews_xvnews_window_objects	*ip;
int	post;
{
	char	message[64];

	if (post == MAIL_FORWARD || post == MAIL_REPLY)
		strcpy(message, "Deliver mail message?");
	else
		strcpy(message, "Post news article?");

	return notice_prompt(ip->controls1, NULL,
				NOTICE_MESSAGE_STRINGS, message,
				NULL,
				NOTICE_BUTTON_YES, "Deliver",
				NOTICE_BUTTON_NO,       "Cancel",
				NULL);
}

char	*
deliverNews(ip, file, statbuf)
xvnews_xvnews_window_objects    *ip;
char	*file;
struct	stat	*statbuf;
{
  char	aline[1024], *article;
  FILE	*ifp_in;
#ifdef USE_INEWS_TO_POST
  FILE *pipe = NULL;
#else
  char message[MAX_MESSAGE_LEN];
  int status;
#endif

  article = (char *)malloc(statbuf->st_size + 1);	
	
  if (statbuf->st_size < 2 || (ifp_in = fopen(file, "r")) == NULL) {
    xvnews_err(ip, "Unable to open %s!\n", file);
    return article;
  }

  article[0] = '\0';
  while(fgets(aline, 1023, ifp_in) != NULL) {
    aline[1023] = '\0';
    sprintf(article, "%s%s", article, aline);
  }
  fclose(ifp_in);
  article = checkPeriods(article, 100); /* TODO: This is very yucky... */
#ifdef USE_INEWS_TO_POST
	if ((pipe = popen(INEWS_PROGRAM, "w")) == NULL) {
	  xvnews_err(ip, "Couldn't open a pipe to inews!\n");
	  return article;
	}
	fprintf(pipe, "%s\n", article);
	pclose(pipe);
			  
	xvnews_err(ip, "Article passed to Inews for posting ...\n");
#else
	put_server("POST");
	status = get_server(message, sizeof(message));
	if (status != CONT_POST) {
		reconnect_server();
		put_server("POST");
		status = get_server(message, sizeof(message));
		if (status != CONT_POST)
			xvnews_err(ip, "Posting not allowed!\n");
		return article;
	}
  put_server("%s", article); /* To avoid % expansion */
	put_server("\n.\n");
	status = get_server(message, sizeof(message));
	if (status != POSTED_OK) {
		char	*c = message;

		while(isdigit(*c++)); 
		xvnews_err(ip, "Server rejected article! Error: %s\n", c);
		return article;
	}
	xvnews_err(ip, "Article Posted.\n");
#endif /* USE_INEWS_TO_POST */
	filePost(NULL, ip, article);
	free(article);

	return NULL;
}
