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
#include <xview/font.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#define _OTHER_TEXTSW_FUNCTIONS /* For additional prototypes... */
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/notice.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"

extern struct globals	*Global;

STATIC_FUNCTION( int body_search, (int, char *, char *));
STATIC_FUNCTION( int get_sub_prev, (xvnews_xvnews_window_objects *,
				    char *, char *));
STATIC_FUNCTION( int nnrp_get_sub_prev, (xvnews_xvnews_window_objects *,
					 char *, char *));

extern int subject_search(ip, next)
xvnews_xvnews_window_objects    *ip;
int	next;
{
	int	row, num = 0, art;
	char	old[SUBJECT_LENGTH + 1];
	char	*str;

	assert(ip);
	assert(next == 0 || next == 1);

	row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
	str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);

	while (isdigit(*str) || *str == ' ') {
                ++str;
                ++num;
        }
 
        memset(old, '\0', SUBJECT_LENGTH);
        strncpy(old, str, SUBJECT_LENGTH - num);

	next ? row++:row--;
	while (row < xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL)) {
		str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
		sscanf(str, "%d", &art);
		str += num;
		if ((!strncmp(old, str, SUBJECT_LENGTH - num) ||
			!strncmp(old, &str[4], SUBJECT_LENGTH - num - 4) ||
			 !strncmp(&old[4], str, strlen(old) - 4)) && !isRead(str)) {
			xv_set(ip->articles_list,
				 PANEL_LIST_SELECT, row, TRUE, NULL);
			retrieve_article(ip, art);
			return 1;
		}
	next ? row++:row--;
	}
	if (next) {
	  art = Global->article;
	  Global->article = next_unread_art(ip);
	  if (art == Global->article) {
	    if (Global->mode == ARTICLE_MODE) {
	      update_newsrc(ip, 0);
	    }
	    groups_set(ip);
	  }
	  else {
	    retrieve_article(ip, Global->article);
	    xvnews_err(ip, "No subject matches found, went to next unread article\n");
	  }
	  return -1;
	}
	xvnews_err(ip, "Searching archives for previous subject...\n");
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (toupper(old[0]) == 'R' && toupper(old[1]) == 'E') {
		if (Global->nnrp)
			art = nnrp_get_sub_prev(ip, "subject", &old[3]);
		else
			art = get_sub_prev(ip, "subject", &old[3]);
	} else {
		if (Global->nnrp)
			art = nnrp_get_sub_prev(ip, "subject", old);
		else
			art = get_sub_prev(ip, "subject", old);
	}
	if (art == -1) {
		xvnews_err(ip, "No matches found!\n");
		xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
		return -1;
	}
	xv_set(ip->articles_list, PANEL_LIST_SELECT, art, TRUE, NULL);
	retrieve_article(ip, xv_get(ip->articles_list, PANEL_LIST_CLIENT_DATA,
		art, NULL));
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
	return 1;
}

static int get_sub_prev(ip, comm, str)
xvnews_xvnews_window_objects    *ip;
char	*comm;
char	*str;
{
	int 	last = -1, art, num = 0, status;
	char	*old;
	char	string[49], message[MAX_MESSAGE_LEN];

	if (!notice_prompt(ip->controls1, NULL, NOTICE_MESSAGE_STRINGS, 
			"No match was found in unread articles.",
			"Search previously read articles?",
		 	NULL,
		NOTICE_BUTTON_YES, "Search",
		NOTICE_BUTTON_NO, "Abort",
		NULL))
			return -1;
	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
		xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL),
			 NULL);
	sscanf(old, "%d", &art);
	put_server("XHDR %s 0-%d\n", comm, --art);
	memset(string, '\0', 49);
	num = strlen(str) - 1;
	while (str[num--] == ' ');
	strncpy(string, str, num+2);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		reconnect_server();
		put_server("XHDR %s 0-%d\n", comm, --art);
		status = get_server(message, sizeof(message));
		if (status != OK_HEAD) 
			return -1;
	}
	while (*message != '.') {
		get_server(message, sizeof(message));
		if (strstr(message, string) != NULL) {
			sscanf(message, "%d", &last);
		}
	}
	if (last != -1)
		return get_prev(ip, last);

	return -1;
}

extern int author_search(ip, next)
xvnews_xvnews_window_objects    *ip;
int     next;
{
	int     row, art;
        char    old[AUTHOR_LENGTH + 1];
        char    *str;

        row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
        str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);

	str += SUBJECT_LENGTH + 1;

	memset(old, '\0', AUTHOR_LENGTH + 1);
	strncpy(old, str, AUTHOR_LENGTH);

	next ? row++:row--;
	while (row < xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL)) {
		str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row,
NULL);
		sscanf(str, "%d", &art);
		str += SUBJECT_LENGTH + 1;
		if (strncmp(old, str, AUTHOR_LENGTH - 1) == 0) {
			xv_set(ip->articles_list,
                                 PANEL_LIST_SELECT, row, TRUE, NULL);
                        retrieve_article(ip, art);
                        return 1;
                }
	next ? row++:row--;
        }
	if (next) {
		Global->article = next_unread_art(ip);
		retrieve_article(ip, Global->article);
        	xvnews_err(ip, "No author matches found, went to next unread article\n");
		return -1;
	}
	xvnews_err(ip, "Searching archives for previous author...\n");
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	if (Global->nnrp)
		art = nnrp_get_sub_prev(ip, "from", old);
	else
		art = get_sub_prev(ip, "from", old);
	if (art == -1) {
		xvnews_err(ip, "No matches found!\n");
		xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
		return -1;
	}
	xv_set(ip->articles_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
	retrieve_article(ip, xv_get(ip->articles_list, PANEL_LIST_CLIENT_DATA,
		NULL));
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
	return 1;
}

extern int exp_search(srp, next)
xvnews_search_popup_objects	*srp;
int next;
{
	xvnews_xvnews_window_objects *ip = (xvnews_xvnews_window_objects *) xv_get(xv_get(srp->search_popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	int	first, last, match = -1, row, status;
	char	*exp, *str, *old, *err = NULL, *artlast, *c;
	char	command[MAX_COMMAND_LEN], message[MAX_MESSAGE_LEN];
	
	exp = (char *)xv_get(srp->search_text, PANEL_VALUE, NULL);
	str = (char *)xv_get(srp->header_text, PANEL_VALUE, NULL);

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
		xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL), NULL);
	sscanf(old, "%d", &first);

	if (next) {
		artlast = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, 
			xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL) - 1,
				 NULL);
		sscanf(artlast, "%d", &last);
		sprintf(command, "XHDR %s %d-%d", str, first + 1, last);
	} else
		sprintf(command, "XHDR %s 0-%d", str, first - 1);
	err = xvnews_comp(exp);
	if (err != NULL) {
		xv_set(srp->search_popup, FRAME_LEFT_FOOTER, err, NULL);
		xvnews_err(ip, "Expression error: %s!\n", err);
		return -1;
	}
	xv_set(srp->search_popup, FRAME_LEFT_FOOTER, "Searching...\n", NULL);
	xvnews_err(ip, "Searching...\n");
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	xv_set(srp->search_popup, FRAME_BUSY, TRUE, NULL);
	if (!strcmp(str, "article text") || !strcmp(str,"entire header")) 
		match = body_search(next, old, str);
	else {
		put_server("%s", command);
		status = get_server(message,sizeof(message));
		if (status != OK_HEAD) {
			reconnect_server();
			put_server("%s", command);
			status = get_server(message,sizeof(message));
			if (status != OK_HEAD) 
				return -1;
		}
		while(*message != '.') {
			get_server(message,sizeof(message));
			c = message;
			while(isdigit(*c++));
			if (xvnews_exec(c)) {
				if (next && match == -1) 
					sscanf(message, "%d", &match);
				if (!next)
					sscanf(message, "%d", &match);
			}
		}
	}
	if (match == -1) {
		xvnews_err(ip, "No matches found for \"%s\"!\n", exp);
		xv_set(srp->search_popup, FRAME_LEFT_FOOTER, "No matches.\n",
			 NULL);
		xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
		xv_set(srp->search_popup, FRAME_BUSY, FALSE, NULL);
		return -1;
	}
	row = get_prev(ip, match);
	xv_set(ip->articles_list, PANEL_LIST_SELECT, row, TRUE, NULL);
	retrieve_article(ip, match);
	if (!strcmp(str, "article text")) {
		Textsw_index text_first = 0, text_last = TEXTSW_INFINITY;

		if (textsw_find_exp(ip->article_window, exp,
				    &text_first, &text_last) != -1) {
		  textsw_normalize_view(ip->article_window, text_first - 5);
		  textsw_set_selection(ip->article_window,
				       text_first, text_last, 1);
		}
	}
	xv_set(srp->search_popup, FRAME_BUSY, FALSE, NULL);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
	xv_set(srp->search_popup, FRAME_LEFT_FOOTER, "\n", NULL);
	return 1;
}

extern void kill_subject(ip, flg)
xvnews_xvnews_window_objects	*ip;
int flg;
{
  char	*str;
  char	old[SUBJECT_LENGTH + 1], undelete[SUBJECT_LENGTH + AUTHOR_LENGTH + 2];
  int 	num = 0, new = 0, artnum = 0;
  int	nrows = xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);
  int	row = xv_get(ip->articles_list,PANEL_LIST_FIRST_SELECTED, NULL);
  xvnews_undelete_popup_objects *up = (xvnews_undelete_popup_objects *)
    xv_get(ip->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, NULL);

  str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
	
  memset(old, '\0', SUBJECT_LENGTH);

  if (!flg) {
    /* Read past the article number */
    while (isdigit(*str)) {
      ++str;
      ++num;
    }
    
    /* Read past the spaces between the article number
       and the subject */
    while (*str == ' ') {
      ++str;
      ++num;
    }	
	
    if (strstr(str, "Re:") == NULL)
      strncpy(old, str, SUBJECT_LENGTH - num);
    else {
      str += 4;
      strncpy(old, str, SUBJECT_LENGTH - (num + 4));
    }
  } else {
    str += SUBJECT_LENGTH + 1;
    strncpy(old, str, AUTHOR_LENGTH - 1);
  }

  str = old + strlen(old) - 1;
  while (*str == ' ') {
    --str;
  }
  ++str;
  *str = '\0';
		
  num = 0;
  xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
  xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
  xv_set(up->undelete_list, XV_SHOW, FALSE, NULL);
  while(--nrows >= 0) {
    str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, nrows, NULL);
    if (strstr(str, old) != NULL) {
      sscanf(str, "%d", &artnum);
      strncpy(undelete, str, SUBJECT_LENGTH + AUTHOR_LENGTH);
      undelete[SUBJECT_LENGTH + AUTHOR_LENGTH] = '\0';
      xv_set(up->undelete_list,
	     PANEL_LIST_INSERT, new,
	     PANEL_LIST_STRING, new, undelete,
	     NULL);
      if ((char *)xv_get(up->undelete_list, PANEL_LIST_STRING, new, NULL)
	  == NULL)
	xv_set(up->undelete_list, PANEL_LIST_DELETE, new, NULL);
      else
	new++;
      ++num;
      add_articles(Global->group, artnum, artnum);
      if (!isRead(str)) {
	Global->unread--;
      }
      xv_set(ip->articles_list,PANEL_LIST_DELETE, nrows,NULL);
    }
  }
  Global->unread++;
  xv_set(up->undelete_list, XV_SHOW, TRUE, NULL);
	
  if (num) {
    xv_set(up->controls3, WIN_FONT, Global->listfont, NULL);
    xv_set(up->undelete_list, PANEL_LIST_FONTS, Global->listfont, NULL, NULL);
  }
  if (!xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL)) {
    if (Global->mode == ARTICLE_MODE)
      update_newsrc(ip, 1);
    groups_set(ip);
    xvnews_err(ip, "All articles killed\n");
    return;
  }
  xv_set(ip->articles_list, XV_SHOW, TRUE, NULL);
  xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
  Global->article = next_unread_art(ip);
  retrieve_article(ip, Global->article);
  xvnews_err(ip, "%d %s killed matching \"%s\"\n", num,
	     num > 1 ? "articles":"article", old);
}

STATIC int body_search(next, old, str)
int	next;
char	*old, *str;
{
	char article[MAX_ARTICLE_LEN], search[16];
	int	art, artorig, match = -1, last, response;
	struct	newsrc_node	*curr = NULL;

	sscanf(old, "%d", &artorig);

	curr = Global->group;
	
	if (next) {
		art = artorig + 1;
		last = curr->last_article + 1;
	} else {
		last = curr->last_article - curr->articles - 1;
		art = artorig - 1;
	}

	if (!strcmp(str, "article text"))
		strcpy(search, "BODY");
	else
		strcpy(search, "HEAD");

	while(match == -1) {
		put_server("%s %d", search, art);
		response = get_server(article, sizeof(article));
		if (response > 229)  {
			if (response == ERR_FAULT)
				reconnect_server();
			if ((next && art >= last) || (!next && art <= last))
				break;
			next ? art++:art--;
			continue;
		}
		for (;;) {
			get_server(article, sizeof(article));
			if (article[0] == '.' && article[1] == '\0')
                        	break;
			if (xvnews_exec(article))
				match = art;
		}
		next ? art++:art--;
		if (art == last)
			break;
	}
	return match;
}

extern int next_unread_art(ip)
xvnews_xvnews_window_objects	*ip;
{
	int	nrows = xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);
	int	row = 0, art;
	char	*old;

	for (row = 0; row < nrows; row++) {
		old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row,
			NULL);
		sscanf(old, "%d", &art);
		if (art == Global->article)
			break;
	}
	/* If the article is not found at all, reset row */
	if (row == nrows) {
	  row = 0;
	}
	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, 
		xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL), NULL);
	sscanf(old, "%d", &art);
	for (; row < nrows; row++) {
		old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row,
			NULL);
		if ((int)strlen(old) < SUBJECT_LENGTH + AUTHOR_LENGTH + 5) {
		  sscanf(old, "%d", &art);
		  xv_set(ip->articles_list, PANEL_LIST_SELECT, row, TRUE,
			   NULL);
		  return art;
		}
	}
	xv_set(ip->articles_list, PANEL_LIST_SELECT,
	       xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL), 
	       TRUE, NULL);
	return art;
}

extern int undeleteKill(ip, prp, str, num)
xvnews_xvnews_window_objects	*ip;
xvnews_props_objects *prp;
char	*str;
int num;
{
	int	art, i = 0, rows = xv_get(ip->articles_list,PANEL_LIST_NROWS,NULL);
	char	*old;

	if (!xv_get(prp->sort, PANEL_TOGGLE_VALUE, 0)) {
		for (; i<rows; i++) {
			old = (char *)xv_get(ip->articles_list,PANEL_LIST_STRING,i);
			sscanf(old, "%d", &art);
			if (art == num)
				return i;
			if (num < art) 
				break;
		}
	}
	xv_set(ip->articles_list, PANEL_LIST_INSERT, i,
			PANEL_LIST_STRING, i, str,
			PANEL_LIST_FONT, i, Global->listfont,
			NULL);
	Global->unread++;

	return i;
}


STATIC int nnrp_get_sub_prev(ip, comm, str)
xvnews_xvnews_window_objects    *ip;
char	*comm;
char	*str;
{
	int 	last = -1, art, num = 0, status;
	char	*old, *c;
	char	string[50], message[MAX_MESSAGE_LEN];

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, 
		xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL)
			, NULL);
	sscanf(old, "%d", &art);
	memset(string, '\0', 49);
	num = strlen(str) - 1;
	while (str[num] == ' ' || str[num] == '*')
		num--;
	while (*str == '*' || *str == ' ') {
		str++;
		num--;
	}
	strncpy(string, str, num+1);
	c = string;
	while (*c++ != '\0')
		if (*c == ' ')
			*c = '?';
	put_server("XPAT %s 0-%d *%s*\n", comm, --art, string);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		reconnect_server();
		put_server("XPAT %s 0-%d *%s*\n", comm, --art, string);
		status = get_server(message, sizeof(message));
		if (status != OK_HEAD) 
			return -1;
	}
	while (*message != '.') {
		get_server(message, sizeof(message));
		if (message[0] != '.')
			sscanf(message, "%d", &last);
	}
	if (last != -1)
		return get_prev(ip, last);

	return -1;
}

extern int isRead(str)
char *str;
{
	if (!strncmp(&str[strlen(str) - 6], "[read]", 6) ||
		  !strncmp(&str[strlen(str) - 6], "[save]", 6) ||
		    !strncmp(&str[strlen(str) - 7], "[print]", 7) ||
		       !strncmp(&str[strlen(str) - 6], "[filt]", 6))
		return 1;
	
	return 0;
}
			
