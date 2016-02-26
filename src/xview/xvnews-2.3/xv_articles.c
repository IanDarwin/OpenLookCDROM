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
#include <sys/timeb.h>
#include <sys/stat.h>
#include <xview/xview.h>
#include <xview/font.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/notice.h>
#include <gfm_ui.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"

extern struct globals	*Global;

STATIC_FUNCTION(write_detail_info, (Textsw, FILE *));

/* Get the subjects for a selected group. */
extern int read_article(ip, only_unread)
xvnews_xvnews_window_objects    *ip;
int only_unread;
{
  /* num is a numeral which contains one of the following values:
     0  - read all articles in this group
     !0 - read all unread articles in this group
     */
  static char message[MAX_MESSAGE_LEN];
  static char **list = (char **)NULL;
  static int l_max = 0;
   
  char	*new;
  int	first,last,total,count = 0,artfirst,artlast,killed=0;
  int	status, artnum;
  struct	newsrc_node	*curr = NULL;
  struct kill_list	*kills = NULL;
  xvnews_props_objects *prp =
    (xvnews_props_objects *)xv_get(ip->xvnews_window,	 XV_KEY_DATA,
				   PROPS_POPUP, NULL);

  /* Initialize the static lists using the default sizes */
  if (!l_max) {
    list = (char **)malloc(INIT_NUM_ARTICLES * sizeof(char *));
    l_max = INIT_NUM_ARTICLES;
    memset(list, '\0', INIT_NUM_ARTICLES * sizeof(char *));
  }
   
  /* Get us to the selected group and find the available articles. */
  put_server("GROUP %s", Global->group->newsgroup);
  total = get_server(message, sizeof(message));
  if (total != OK_GROUP) {
    reconnect_server();
    put_server("GROUP %s", Global->group->newsgroup);
    total = get_server(message, sizeof(message));
    if (total != OK_GROUP) {
      xvnews_err(ip,"Unable to retrieve %s!\n",Global->group->newsgroup);
      return -1;
    }
  }
  sscanf(message, "%*d%d%d%d", &total, &first, &last);

  /* Find the current group */
  curr = Global->group;
  curr->last_article = last;

  /* See if the user wants all articles or not. */
  if (!only_unread) {
    /* We need to show all articles. */
    if (last != 0) {
      curr->articles = total;
      artfirst = first;
      artlast = last;
      xv_set(ip->articles_list, PANEL_LIST_DELETE_ROWS, 0,
	     xv_get(ip->articles_list, PANEL_LIST_NROWS,NULL), NULL);
    } else {
      /* No articles in selected group. */
      xvnews_err(ip, "No articles available in %s!\n",
		 Global->group->newsgroup);
      return (-1);
    }
    xvnews_err(ip, "Retrieving all available articles in %s...\n",
	       Global->group->newsgroup);
  } else {
    /* Just retrieve all unread articles. */
    if (curr->artlist == NULL) {
      artfirst = curr->last_article - curr->articles + 1;
    }
    else {
      if (curr->artlist->first != 1) {
	artfirst = 1;
      }
      else {
	artfirst = curr->artlist->last + 1;
      }
    }
    
    artlast = last;
    if (xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL))
      xv_set(ip->articles_list, PANEL_LIST_DELETE_ROWS, 0,
	     xv_get(ip->articles_list, PANEL_LIST_NROWS,NULL), NULL);
    generateKillList(curr);
  }

  /* Issue the command and get the text from the news server. */
  put_server("XHDR subject %d-%d", artfirst, artlast);
  xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
  status = get_server(message, sizeof(message));
  if (status != OK_HEAD) {
    if (status == ERR_COMMAND) {
      printf("XHDR support not available from NNTP server!\n");
      return -1;
    }
    put_server("XHDR subject %d-%d", artfirst, artlast);
    status = get_server(message, sizeof(message));
    if (status != OK_HEAD)
      return -1;
  }
  while (*message != '.') {
    get_server(message, sizeof(message));
    /* If the string is not '.' malloc some space and copy the
       article number and subject into the array.
       */
    if ((strlen(message) != 1) && (message[0] != '.')) {
      /* If this article has already been read, skip it */
      sscanf(message, "%d", &artnum);
      if (!is_unread(curr, artnum) && only_unread)
	continue;
      
      if (Global->single == NULL && (Global->kill != NULL ||
				     curr->kill != NULL)) {
	kills = subjectKill(message, curr->kill, kills);
      }
      if (!list[count]) {
	list[count] = (char *)malloc(SUBJECT_LENGTH + AUTHOR_LENGTH + 3);
      }
      strncpy(list[count], message, SUBJECT_LENGTH + AUTHOR_LENGTH);
      list[count][SUBJECT_LENGTH + AUTHOR_LENGTH] = '\0';
      if (++count >= l_max) {
	list = (char **)realloc(list, 2 * l_max * sizeof(char *));
	memset(&(list[count]), '\0', l_max * sizeof(char *));
	l_max *= 2;
      }
    }
  }

  /* Set the last element of the array to NULL, just to be sure */
  if (list[count]) {
    free(list[count]);
  }
  list[count] = NULL;

  /* Get the authors to these articles. */
  getauthorlist(list, artfirst, artlast, only_unread);

  /* Insert the subjects into the scrolling list */
  xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
  xv_set(ip->articles_list, PANEL_LIST_INSERT_STRINGS, 0, list, NULL);

  if (!Global->single && (headerKill(Global->kill) ||
			  headerKill(curr->kill))) 
    kills = killHeaders(Global->kill, curr->kill, artfirst, artlast, kills);

  if (only_unread && !Global->single)
    kills = messageKill(artfirst, artlast, kills);

  /* Delete any articles already read. */
  /* This doesn't need to be done anymore, as those articles
     are now skipped beforehand.*/
#ifdef OBSOLETE
  if (only_unread && curr->artlist != NULL)
    delete_articles(ip, curr);
#endif

  /* Process the kill list */
  if (kills != NULL)
    killed = articleKill(ip, kills);

  clearKills(curr->kill);
  clearKills(Global->kill);
  curr->kill = NULL;
  Global->kill = NULL;

  /* Set up the unread article counter. */
  if (!only_unread && xv_get(ip->articles_list, XV_SHOW, NULL))
    Global->unread += artlast - artfirst;
  else
    Global->unread = xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);
  
  if (only_unread && !Global->unread) 
    return 0;

  if (xv_get(prp->sort, PANEL_TOGGLE_VALUE, 0, NULL))
    sortSubjects(ip, XV_SORT_SUBJECT);

  /* read the first article. */
  new = (char *) xv_get(ip->articles_list, PANEL_LIST_STRING, 0, NULL);
  sscanf(new, "%d", &Global->article);
  xv_set(ip->articles_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
  if (only_unread)
    Global->mode = ARTICLE_MODE;
  retrieve_article(ip, Global->article);

  /* Show the scrolling list and reset the frame busy cursor. */
  if (xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL))
    xv_set(ip->articles_list, PANEL_LIST_FONTS, Global->listfont, NULL, NULL);
  xv_set(ip->groups_list, XV_SHOW, FALSE, NULL);
  xv_set(ip->articles_list, XV_SHOW, TRUE, NULL);
  xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
  xv_set(ip->articles_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
  if (killed)
    xvnews_err(ip, 
	       "%d %s killed in %s! (Use 'Undelete' under the 'Kill' button to view)\n",
	       killed, killed == 1 ? "article":"articles",
	       Global->group->newsgroup);
  xv_set(ip->xvnews_window, XV_LABEL, Global->group->newsgroup, NULL);
  return (1);
}

/* Read an article */
extern void retrieve_article(ip, num)
xvnews_xvnews_window_objects    *ip;
int	num;
{
  static	char article[MAX_ARTICLE_LEN];
  char	new[120], file[80], *header = NULL;
  char	*old;
  int	row, response, head = 0, dup = 0, from = 0;
  char print_date[80];
  time_t new_date;
  int got_date;
#ifdef USE_FTIME
  struct timeb ftnow;
#endif
  /* The following variable indicates that we are certain to
     be in the body, even though the server indicates that we
     still are in the header */
  int in_body;
  FILE	*ifp_out;
  static	char	tfile[]="/tmp/.xvnews.file";
  xvnews_props_objects *prp = (xvnews_props_objects *)
    xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
  kill_popup_objects *kp = (kill_popup_objects *)
    xv_get(ip->xvnews_window, XV_KEY_DATA, KILL_POPUP, NULL);

  sprintf(file, "%s.%d", tfile, getpid());
  /* Open the temp file. */
  if ((ifp_out = fopen(file, "w")) == NULL) {
    printf("Unable to open temp file %s!\n", file);
    textsw_reset(ip->article_window, 0, 0);
    return;
  }

  memset(new, '\0', 120);
  row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
  old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
  if (old && !isRead(old)) {
    /* We haven't read this, mark it and decrement the counter. */
    sprintf(new, "%-76.76s [read]", old);
    Global->unread--;
    xv_set(ip->articles_list, PANEL_LIST_STRING, row, new, NULL);
  }
  xvnews_err(ip, "Reading in article %d...\n", num);
  xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
  put_server("HEAD %d", num);
  response = get_server(article, sizeof(article));
  if (response != OK_HEAD) {
    xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
    
    xvnews_err(ip, "Lost connection to NNTP server! Restoring connection...\n");
    xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
    reconnect_server();
    put_server("HEAD %d", num);
    response = get_server(article, sizeof(article));
    if (response != OK_HEAD) {
      xvnews_err(ip, "Article requested not available!\n");
      textsw_reset(ip->article_window, 0, 0);
      xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
      xv_set(ip->articles_list,
	     PANEL_LIST_DELETE, row, NULL);
      fclose(ifp_out);
      return;
    }
  }
#ifdef USE_FTIME
  ftime(&ftnow);
#else
  tzset();
#endif
  head = xv_get(prp->header_select, PANEL_TOGGLE_VALUE, 0, NULL);
  in_body = FALSE;
  get_server(article, sizeof(article));
  while(*article != '.') {
    if (!dup && !strncmp(article, "Newsgroups:", 11) &&
	index(article, ',') != NULL)
      dup = 1;	
    if (!head) {
      /* We don't want to display full headers, but if we are
	 certain we actually are reading the body, display it */
      if (in_body) {
	fprintf(ifp_out, "%s\n", article);
      }
      else {
	if (!from && !strncmp(article, "From:", 5)) {
	  if (header != NULL) {
	    fprintf(ifp_out, "From %s\n%s", &article[6], header);
	    free(header);
	  } else
	    fprintf(ifp_out, "From %s\n", &article[6]);
	  from = 1;
	}
	if ((got_date = !strncmp(article, "Date:", 5)) || 
	    !strncmp(article, "Subject:", 8) ||
	    !strncmp(article, "Newsgroups:", 11) ||
	    !strncmp(article, "Organization:", 13)) {
	  if (got_date) {
	    /* This is the date, reformat for local time-zone */
#ifdef USE_FTIME
	    new_date = get_date(article+5, ftnow.time, ftnow.timezone);
#else
	    new_date = get_date(article+5, time(NULL), timezone);
#endif
	    strftime(print_date, 80, "%a %b %d %X %Z %Y",
		     localtime(&new_date));
	  }
	  if (from)
	    if (got_date) {
	      fprintf(ifp_out, "Date: %s\n", print_date);
	    }
	    else{
	      fprintf(ifp_out, "%s\n", article);
	    }
	  else {
	    if (header == NULL) {
	      header = (char *)malloc(1024);
	      header[0] = '\0';
	    }
	    if (got_date) {
	      sprintf(header, "%sDate: %s\n", header, print_date);
	    }
	    else{
	      sprintf(header, "%s%s\n", header, article);
	    }
	  }
	}

	/* Check to see whether we are really past the header
	   and in the body. If so, set the flag accordingly */
	if (((*article >= 'A') && (*article <= 'Z')) &&
	    (*(article+strcspn(article, ": \t")) == ':')) {
	  /* Line starts with a capitalized word which ends with a
	     semi-colon and a tab. Looks like header to me */
	} 
	else {
	  if (from) {
	    in_body = TRUE;
	    fprintf(ifp_out, "%s\n", article);
	  }
	  /* If we are here, and from is not true, then weird things
	     are happening. We think we're out of the body, but
	     we haven't even seen the From: line yet. Ignore... */
	}
      }
    }
    else {
      /* Full headers were selected. Formerly, this meant no
	 processing at all. But now we need to process the time
	 into local time zone... */
      if (!strncmp(article, "Date:", 5)) {
#ifdef USE_FTIME
	new_date = get_date(article+5, ftnow.time, ftnow.timezone);
#else
	new_date = get_date(article+5, time(NULL), timezone);
#endif
	strftime(print_date, 80, "%a %b %d %X %Z %Y", localtime(&new_date));
	fprintf(ifp_out, "Date: %s\n", print_date);
      }
      else{
	fprintf(ifp_out, "%s\n", article);
      }
    }
    get_server(article, sizeof(article));
  }
  fprintf(ifp_out, "\n");
    put_server("BODY");
    response = get_server(article, sizeof(article));
    if (response != OK_BODY) {
      xvnews_err(ip, "Article requested not available\n");

		textsw_reset(ip->article_window, 0, 0);
		xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
		xv_set(ip->articles_list,
			PANEL_LIST_DELETE, row, NULL);
		return;
	}
	for(;;) {
		get_server(article, sizeof(article));
		if (article[0] == '.' && article[1] == '\0')
			break;
		if (!strncmp(article, "From", 4))
			fprintf(ifp_out, ">%s\n", article);
		else 
			fprintf(ifp_out, "%s\n", *article == '.' ? &article[1]:article);
	}
	fprintf(ifp_out, "\n");
	fclose(ifp_out);
	xv_set(ip->article_window,
		 TEXTSW_READ_ONLY, FALSE,
		 TEXTSW_MEMORY_MAXIMUM, TEXTSW_INFINITY,
		 NULL);
	textsw_reset(ip->article_window, 0, 0);
        xv_set(ip->article_window,
	       TEXTSW_FILE_CONTENTS, file,
	       TEXTSW_FIRST, 0,
	       NULL);
#ifndef KEEP_ARTICLE_EDITABLE
        xv_set(ip->article_window, TEXTSW_READ_ONLY, TRUE, NULL);
#endif
	fillKillText(ip, kp);
	if (Global->mode == ARTICLE_MODE && dup)
		genMessage(num);
	xvnews_err(ip, "Currently in %s, %d unread %s\n",
		   Global->group->newsgroup, Global->unread,
		   Global->unread == 1 ? "article":"articles");
    xv_set(ip->xvnews_window, XV_LABEL, Global->group->newsgroup, NULL);
    
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

extern void next_article_proc(ip, status)
xvnews_xvnews_window_objects    *ip;
int status;
{
	char	*item;
	int	num = 0, row = 0;
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);

	row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
	xv_set(ip->articles_list, PANEL_LIST_SELECT, row, FALSE, NULL);
	status ? ++row:--row;
	if (row == -1) {
		if (xv_get(prp->sort, PANEL_TOGGLE_VALUE, 0) && xv_get(ip->articles_list, PANEL_LIST_NROWS) > 1) {
			xvnews_err(ip, "Unable to use this feature with sorted subjects!\n");
			return;
		}
		item = (char *) xv_get(ip->articles_list, PANEL_LIST_STRING, 0, NULL);
		sscanf(item, "%d", &num);
		if (get_prev(ip, --num) == -1) {
			xvnews_err(ip, "No more articles available!\n");
			return;
		}
		row = 0;
	}

	if (row >= xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL)) {
		if (Global->mode == ARTICLE_MODE)
			update_newsrc(ip, 0);
		groups_set(ip);
	} else {
		xv_set(ip->articles_list, PANEL_LIST_SELECT, row, TRUE, NULL);
		item = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
			 row, NULL);
		sscanf(item, "%d", &num);
		Global->article = num;
		retrieve_article(ip, num);
	}
	return;
}

extern int get_prev(ip, num)
xvnews_xvnews_window_objects    *ip;
int num;
{
	int art, status, row;
	int nrow = xv_get(ip->articles_list,PANEL_LIST_NROWS,NULL);
	char message[MAX_MESSAGE_LEN];
	char	*line[1], *old;

	line[0] = (char *)malloc(SUBJECT_LENGTH + AUTHOR_LENGTH + 2);
	memset(line[0], '\0', SUBJECT_LENGTH + AUTHOR_LENGTH + 2);
	put_server("XHDR subject %d-%d", num, num);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		reconnect_server();
		put_server("XHDR subject %d-%d", num, num);
		status = get_server(message, sizeof(message));
		if (status != OK_HEAD) 
			return -1;
	}
	while (*message != '.') {
		get_server(message, sizeof(message));
		if (message[0] != '.') 
			strncpy(line[0], message, SUBJECT_LENGTH + AUTHOR_LENGTH);
	}

	if ((int)strlen(line[0]) < 2) 
	{
	   free(line[0]);
	   return -1;
	}

	getauthorlist(line, num, num, FALSE);
	for (row = 0; row < nrow; row++) {
		old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
		sscanf(old, "%d", &art);
		if (art == num) {
			xv_set(ip->articles_list, PANEL_LIST_CLIENT_DATA,
				 row, num, NULL);
			return row;
		}
		if (num < art)
			break;
	}
	xv_set(ip->articles_list,
		 PANEL_LIST_INSERT, row,
		 PANEL_LIST_STRING, row, line[0], 
		 PANEL_LIST_CLIENT_DATA, row, num,
		 PANEL_LIST_FONT, row, Global->listfont,
		 NULL);
	Global->unread++;
	free(line[0]);
	return row;
}

extern void get_groups(ip, flg)
xvnews_xvnews_window_objects    *ip;
int flg;
{

static	char	*list[MAX_SUBSCRIBED_GROUPS + 1], line[MAX_SUBSCRIBED_GROUPS][80];
static	char message[MAX_MESSAGE_LEN];
	int	total, first, last, unread, row = 0, count = 0;
	struct newsrc_node	*curr = Global->head;

	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	xv_set(ip->groups_list, PANEL_LIST_DELETE_ROWS, 0,
                xv_get(ip->groups_list, PANEL_LIST_NROWS, NULL), NULL);

	for (curr = Global->head; curr != NULL; curr = curr->nextgroup) {
		if (curr->subscribed == SUBSCRIBED) {
			if (flg) {
				put_server("GROUP %s", curr->newsgroup);
				total = get_server(message, sizeof(message));
				if (total != OK_GROUP) {
					reconnect_server();
					put_server("GROUP %s", curr->newsgroup);
					total = get_server(message,
							   sizeof(message));
					if (total != OK_GROUP)
						continue;
				}
				total = curr->articles;
				last = curr->last_article;
				sscanf(message, "%*d%d%d%d", &total, &first, &last);
				curr->last_article = last;
				curr->articles = total;
			}
			unread = unread_articles(curr);
			if (unread > 0) {
				if (row == MAX_SUBSCRIBED_GROUPS) {
					list[row] = NULL;
					xv_set(ip->groups_list,
						PANEL_LIST_INSERT_STRINGS,
						count * MAX_SUBSCRIBED_GROUPS,
						list, NULL);
					row = 0;
					count++;
				}
				sprintf(line[row],
					"%-48.48s%-5d total unread article%c",
				 	    curr->newsgroup, unread,
						unread == 1 ? ' ':'s');
				list[row] = line[row];
				++row;
			}
		}
	}
	list[row] = NULL;
	xv_set(ip->groups_list,
	 PANEL_LIST_INSERT_STRINGS, MAX_SUBSCRIBED_GROUPS * count, list,
	 NULL);
	xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
	xv_set(ip->groups_list, XV_SHOW, TRUE, NULL);
	if (xv_get(ip->groups_list, PANEL_LIST_NROWS, NULL))
		xv_set(ip->groups_list,
		 	PANEL_LIST_FONTS, Global->listfont, NULL, NULL);
	xv_set(ip->groups_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

extern void unread_proc(ip)
xvnews_xvnews_window_objects    *ip;
{
	char	*old;
	char	line[120];
	int	row, art;

	row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
	old = (char *) xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
	sscanf(old, "%d", &art);

	if (isRead(old)) {
	  sprintf(line, "%-76.76s [unread]", old);
	  xv_set(ip->articles_list, PANEL_LIST_STRING, row, line,
		 PANEL_LIST_SELECT, row, TRUE, NULL);
	  Global->unread++;
	  xvnews_err(ip, "Currently in %s, %d unread %s\n",
		     Global->group->newsgroup, Global->unread,
		     Global->unread == 1 ? "article":"articles");
	  if (Global->mode == ALL_ARTICLES &&
	      !xv_get(ip->articles_list,PANEL_LIST_CLIENT_DATA,row)) {
	    if (Global->group->subscribed == UNSUBSCRIBED) {
	      return;
	    }
		  
	    add_unread(Global->group, art);
	    xv_set(ip->mark_unread_butt,PANEL_CLIENT_DATA,1,NULL);
	    xv_set(ip->articles_list, PANEL_LIST_CLIENT_DATA, row, 1, NULL);
	  }
	  removeMessage(art);
	}
}

extern void print_article(ip, print)
xvnews_xvnews_window_objects  *ip;
int print;
{
	char	printcommand[256], new[96];
	char	*str;
	FILE	*lpr_pipe;
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);

	xvnews_err(ip, "%sing article...\n", print ? "Print":"Filter");
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	
	if (print) {
		strcpy(printcommand,(char *)xv_get(prp->print_text, PANEL_VALUE, NULL));
		if (!strlen(printcommand)) {
			if (getenv("OPENWINHOME") == NULL)
				sprintf(printcommand, "/usr/ucb/lpr");
			else 
				sprintf(printcommand, "%s/bin/xview/mp|/usr/ucb/lpr",
			 		getenv("OPENWINHOME"));
		}
	} else {
		strcpy(printcommand,(char *)xv_get(prp->filter_text, PANEL_VALUE, NULL));
		if (!strlen(printcommand)) {
			xvnews_err(ip, "No save filter specified!\n");
			xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
			return;
		}
	}
	if ((lpr_pipe = popen(printcommand, "w")) == NULL) {
		xvnews_err(ip, "Failed to open printer!\n");
		xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
		return;
	}

	if (write_detail_info((Textsw)ip->article_window, lpr_pipe) != 1) {
		xvnews_err(ip, "Printer error!\n");
	}
	pclose(lpr_pipe);
	str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING,
		xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED));
	sprintf(new, "%-76.76s [%s]", str, print ? "print":"filt");
	xv_set(ip->articles_list, PANEL_LIST_STRING, 
		xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED),
		new, NULL);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
	xvnews_err(ip, "Article %s through \"%s\"\n",
		 print ? "printed":"filtered", printcommand);
}

STATIC int
write_detail_info(txt, fptr)
Textsw  txt;
FILE    *fptr;
{

	int             textsize, next_pos;
	char            *textbuffer;

	xv_set((Textsw)txt, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, 0);
	textsize= (Textsw_index)xv_get((Textsw)txt, TEXTSW_INSERTION_POINT);
	textbuffer = (char *) malloc(textsize + 1);
	if (textbuffer == NULL) {
		printf("Malloc failed!\n");
		return(-1);
	}
	memset(textbuffer, '\0', textsize);
	next_pos = (Textsw_index) xv_get((Textsw)txt, TEXTSW_CONTENTS, 0,
	    textbuffer, textsize);
	if (next_pos < 1) {
		free(textbuffer);
		return(-1);
	}
	fwrite(textbuffer, textsize, 1, fptr);
	free(textbuffer);
	return(1);
}

extern int
next_group(ip, forward)
xvnews_xvnews_window_objects    *ip;
int forward;
{
  char groupname[BUFFERLEN];
  struct newsrc_node *curr;
  char	*new;
  int	row, end, nrows;

  row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);
  nrows = xv_get(ip->groups_list, PANEL_LIST_NROWS, NULL);

  if (!forward) {
    if (row)
      --row;
    else
      return -1;
  }

  if (row < 0 || row >= nrows)
    return(-1);
  else {
    xv_set(ip->groups_list, PANEL_LIST_SELECT, row, TRUE, NULL);
    new = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row, NULL);
    sscanf(new, "%s%d", groupname, &end);
    for (curr = Global->head; curr; curr = curr->nextgroup) {
      if (!strcmp(groupname, curr->newsgroup))
	break;
    }
    assert( curr );
    Global->group = curr;
    
    if (end) {
      xvnews_err(ip, "Retrieving %d unread articles from %s...\n",
		 end, Global->group->newsgroup);
      switch (read_article(ip, end)) {
      case 0:
	catchup_group(ip);
	groups_set(ip);
	xvnews_err(ip, "All articles killed!\n");
	break;
      case 1:
	article_set(ip);
	break;
      case -1:
	xv_set(ip->groups_list, PANEL_LIST_DELETE, row, NULL);
	groups_set(ip);
	xvnews_err(ip, "No articles available!\n");
	break;
      }
      return(row);
    } else
      return(-1);
  }
}

extern void save_article_proc(gfm, file)
gfm_popup_objects       *gfm;
char *file;
{

	char	*old;
	char 	err[160], line[90];
	int	row, new = 1;
	FILE	*ifp_out;
	struct stat statbuf;
	xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(xv_get(gfm->popup, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);

	if (!stat(file, &statbuf)) {
		sprintf(err, "Appended article to %s\n", file);
		new = 0;
	} else
		sprintf(err, "Saved article to %s\n", file);

	if ((ifp_out = fopen(file, "a")) == NULL) {
		sprintf(err, "Unable to save article to %s!\n", file);
		xvnews_err(bp, err);
		return;
	}

	if (strstr(file, Global->newsdir) != NULL && !new)
		fprintf(ifp_out, "\n");

	if (write_detail_info((Textsw)bp->article_window, ifp_out) != 1) {
		sprintf(err, "Error saving article to %s!\n", file);
		xvnews_err(bp, err);
		return;
	}

	if (strstr(file, Global->newsdir) != NULL)
		fprintf(ifp_out, "\n");

	fclose(ifp_out);
	row = xv_get(bp->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
	old = (char *)xv_get(bp->articles_list, PANEL_LIST_STRING, row, NULL);
	sprintf(line, "%-76.76s [save]", old);
	xv_set(bp->articles_list, PANEL_LIST_STRING, row, line, NULL);
	xvnews_err(bp, err);
}
