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
#ifdef SVR4
#include <sys/param.h>
#endif
#include <xview/xview.h>
#include <xview/font.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/termsw.h>
#include <xview/text.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"

extern struct globals *Global;

STATIC_FUNCTION(struct newsrc_node *find_new, (char *));

extern void update_newsrc(ip, catch)
xvnews_xvnews_window_objects    *ip;
int catch;
{

	char 	line[120], *old;
	int     nrows = (int)xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);
	int 	row=0, grow, unread;
	struct newsrc_node	*curr = Global->head;
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(ip->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);

	grow = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);

	/* Maybe we couldn't find a group. Just return */
	if (!Global->group)
	  return;
	
	curr = Global->group;
	
	if (xv_get(prp->sort, PANEL_TOGGLE_VALUE, 0))
		sortSubjects(ip, XV_SORT_ARTNUM);

	if (catch) {
		add_articles(curr, 1, catch == 1?curr->last_article:catch);
		for (row = 0; row < nrows; row++) {
			old = (char *)xv_get(ip->articles_list,
					PANEL_LIST_STRING, row, NULL);
			if (!strncmp(&old[strlen(old) - 8], "[unread]", 8)) {
				sscanf(old, "%d", &unread);
				if (catch != 1 && unread > catch)
					break;
				add_unread(curr, unread);
			}
		}
	} else 
		getReadArticles(ip, curr);
	unread = unread_articles(curr);

	if (unread > 0) {
		sprintf(line, "%-48.48s%-5d total unread article%c",
			curr->newsgroup, unread, unread == 1 ? ' ':'s');
		xv_set(ip->groups_list,
			   PANEL_LIST_STRING, grow++, line,
			   NULL);
	} else
		xv_set(ip->groups_list, PANEL_LIST_DELETE, grow, NULL);

	if ((grow) < xv_get(ip->groups_list,PANEL_LIST_NROWS, NULL))
		xv_set(ip->groups_list, PANEL_LIST_SELECT, grow, TRUE, NULL);
}

extern void catchup_group(ip)
xvnews_xvnews_window_objects    *ip;
{
	struct newsrc_node	*curr;
	char	group[80];
	char	*old;
	int 	row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);

	if (row == -1)
		return;
	old = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row, NULL);
	sscanf(old, "%s", group);

	for(curr = Global->head; curr != NULL; curr = curr->nextgroup) {
		if (curr->subscribed == SUBSCRIBED) {
			if (!strcmp(curr->newsgroup, group))
				break;
		}
	}
	if (curr) {
	  clear_artlist(curr);
	  add_articles(curr, 1, curr->last_article);
	  xv_set(ip->groups_list, PANEL_LIST_SELECT, row+1, TRUE, NULL);
	  xv_set(ip->groups_list, PANEL_LIST_DELETE, row, NULL);
	  xvnews_err(ip, "All articles in %s marked read\n", curr->newsgroup);
	}
	else {
	  xvnews_err(ip, "This does not appear to be a valid newsgroup?!");
	}
	
}

extern int
save_newsrc()
{
	FILE	*ifp_out;
	struct newsrc_node	*curr = Global->head;
	a_node *art = NULL;
	
	if ((ifp_out = fopen(Global->newsrc, "w")) == NULL) 
		return(-1);

	for (curr = Global->head; curr != NULL; curr = curr->nextgroup) {
		art = curr->artlist;
		if (art == NULL) {
			fprintf(ifp_out, "%s%c\n", curr->newsgroup,
				curr->subscribed == SUBSCRIBED ? ':':'!');
			continue;
		} else {
		  fprintf(ifp_out, "%s%c %d-%d", curr->newsgroup,
			  curr->subscribed == SUBSCRIBED ? ':' : '!',
			  art->first, art->last);
		}
		art = art->nextart;
		while (art != NULL) {
		  if (art->last && art->first != art->last)
				fprintf(ifp_out, ",%d-%d",art->first, art->last);
			else
				fprintf(ifp_out, ",%d", art->first);
			art = art->nextart;
		}
		fprintf(ifp_out, "\n");
	}
	fclose(ifp_out);
	return(0);
}

extern int getactive(ip, flg)
xvnews_xvnews_window_objects    *ip;
int flg;
{
	char	mode[32], *str, *err;
static  char	*list[5000];
	int	count=0;
	int 	row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED);
	struct newsrc_node	*curr;
	xvnews_search_popup_objects     *srp = (xvnews_search_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, SEARCH_POPUP, NULL);

	if (row != -1) {
		str = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row);
		sscanf(str, "%s", Global->old_group);
	} else
		memset(Global->old_group, '\0', 64);

	switch (flg) {
		case 0: 
			strcpy(mode, "");
			Global->mode = ALL_GROUPS;
			break;
		case 1:
			strcpy(mode, "subscribed");
			Global->mode = SUB_GROUPS;
			break;
		case 2:
			strcpy(mode, "unsubscribed");
			Global->mode = UNSUB_GROUPS;
			break;
		case 3:
			str =(char *)xv_get(srp->search_text,PANEL_VALUE,NULL);
			err = xvnews_comp(str);
			if (err != NULL) {
				xv_set(srp->search_popup, FRAME_LEFT_FOOTER,
					 err, NULL);
                		xvnews_err(ip, "Expression error: %s!\n", err);
                		return -1;
        		}
			strcpy(mode, "matching");
			Global->mode = MATCH_GROUPS;
			break;
	}
	xvnews_err(ip, "Retrieving all %s groups...\n", mode); 
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	for (curr = Global->head; curr != NULL; curr = curr->nextgroup) {
		switch (flg) {
			case 0:
				list[count] = (char *)malloc(80);
				sprintf(list[count],
		  		 curr->subscribed == SUBSCRIBED ?
				 "%-64.64ssubscribed":"%-64.64sunsubscribed",
				 curr->newsgroup);
				count++;
				break;
			case 1:
				if (curr->subscribed == SUBSCRIBED) {
			            list[count] = (char *)malloc(80);
				    sprintf(list[count],"%-64.64ssubscribed",
					 curr->newsgroup);
				count++;
				}
				break;
			case 2:
				if (curr->subscribed == UNSUBSCRIBED) {
			           list[count] = (char *)malloc(80);
				   sprintf(list[count], "%-64.64sunsubscribed",
					curr->newsgroup);
				count++;
				}
				break;	
			case 3:
			        if (xvnews_exec(curr->newsgroup)) {
				  list[count] = (char *)malloc(80);
				  sprintf(list[count],
					  (curr->subscribed == SUBSCRIBED) ? "%-64.64ssubscribed":"%-64.64sunsubscribed",
					  curr->newsgroup);
				  count++;
				}
				break;
			}
	}
	list[count] = NULL;
	xv_set(ip->groups_list, PANEL_LIST_DELETE_ROWS, 0,
		xv_get(ip->groups_list, PANEL_LIST_NROWS, NULL), NULL);
	xv_set(ip->groups_list,
		 PANEL_LIST_INSERT_STRINGS, 0, list, NULL);
	if (xv_get(ip->groups_list, PANEL_LIST_NROWS, NULL))
		xv_set(ip->groups_list, PANEL_LIST_FONTS,
			 Global->listfont, NULL, NULL);
	while(count)
		free(list[--count]);
	new_group_set(ip);
	if (xv_get(ip->groups_list, PANEL_LIST_NROWS) > 1)
		xv_set(ip->groups_list, PANEL_LIST_SELECT, 0, FALSE, NULL);
	if (Global->mode == MATCH_GROUPS)
		xv_set(ip->sort_newsrc_butt, XV_SHOW, FALSE, NULL);
	xvnews_err(ip, "All %s groups displayed\n", mode);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
	return 1;
}

extern void group_change(ip, flg)
xvnews_xvnews_window_objects    *ip;
int	flg;
{
	char	group[80], line[120], message[MAX_MESSAGE_LEN];
	char	*old;
	int	row, status;
	struct newsrc_node	*curr;

	row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);

	if (row == -1)
		return;
	xv_set(ip->xvnews_window, FRAME_BUSY, TRUE, NULL);
	xv_set(ip->groups_list, XV_SHOW, FALSE, NULL);
	while (row != -1) {
		old =(char *)xv_get(ip->groups_list,PANEL_LIST_STRING,row,NULL);
		sscanf(old, "%s", group);		
		curr = find_new(group);
		if (!curr)
		  break;
		if (flg) {
                	sprintf(line, "%-64.64ssubscribed", group);
			curr->subscribed = SUBSCRIBED;
                        put_server("GROUP %s", group);
                        status = get_server(message, sizeof(message));
			if (status != OK_GROUP) {
				reconnect_server();
                        	put_server("GROUP %s", group);
                        	status = get_server(message, sizeof(message));
				if (status != OK_GROUP)
					return;
			}
                        sscanf(message, "%*d%d%*d%d",
                                 &curr->articles, &curr->last_article);
        	} else {
                	sprintf(line, "%-64.64sunsubscribed", group);
			curr->subscribed = UNSUBSCRIBED;
        	}
        	xv_set(ip->groups_list,
            		PANEL_LIST_STRING, row, line,
            		PANEL_LIST_SELECT, row, FALSE,
            		NULL);
		row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);
	}
	textsw_reset(ip->article_window, 0, 0); 
        sprintf(line,
         "Group\t\t\tArticles   Description\n-----\t\t\t--------   -----------\n\n");
        textsw_insert(ip->article_window, line, strlen(line));
	xv_set(ip->goto_button, PANEL_INACTIVE, FALSE, NULL);
	xv_set(ip->groups_list, XV_SHOW, TRUE, NULL);
	xv_set(ip->xvnews_window, FRAME_BUSY, FALSE, NULL);
}

STATIC struct newsrc_node *
find_new(group)
char	*group;
{
	struct newsrc_node	*curr;

	for (curr = Global->head; curr != NULL; curr = curr->nextgroup) {
		if (!strcmp(group, curr->newsgroup)) 
			return curr;
	}
	return NULL;
}

extern void unsub_group(ip)
xvnews_xvnews_window_objects    *ip;
{
	char	group[80], *old;
	int	row = xv_get(ip->groups_list, PANEL_LIST_FIRST_SELECTED, NULL);
	struct newsrc_node	*curr = Global->head;

	if (Global->mode == ALL_ARTICLES) {
		xvnews_err(ip, "You cannot unsubscribe when viewing all articles!\n");
		return;
	}
	if (row == -1)
		return;
	if (Global->mode == GROUP_MODE) {
		old = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row, NULL);
		sscanf(old, "%s", group);
		xvnews_err(ip, "Unsubscribed to %s\n", group);
	} else {
		xvnews_err(ip, "Unsubscribing to %s...\n",
			   Global->group->newsgroup);
		update_newsrc(ip, 0);
		textsw_reset(ip->article_window, 0, 0);
		strcpy(group, Global->group->newsgroup);
	}

	for(curr = Global->head; curr != NULL; curr = curr->nextgroup) {
		if (curr->subscribed == SUBSCRIBED) {
			if (!strcmp(group, curr->newsgroup))
				break;
		}	
	}
	curr->subscribed = UNSUBSCRIBED;

	/* We only need to remove the group when there are
	   articles left. So check the name of the group first,
	   and if it matches, remove it. Otherwise it was removed
	   already, and we don't need to bother */
	old = (char *)xv_get(ip->groups_list, PANEL_LIST_STRING, row, NULL);
	if (!strncmp(group, old, strlen(group))) {
	  xv_set(ip->groups_list, PANEL_LIST_DELETE, row, NULL);
	  xv_set(ip->groups_list, PANEL_LIST_SELECT, row, TRUE, NULL);
	}
	
	if (Global->mode == GROUP_MODE)
		return;
	groups_set(ip);
}

char	*
find_approx_match(string)
char	*string;
{
	static	char	match[96];
	struct newsrc_node      *curr = Global->head;
	int	len = strlen(string);

	memset(match, '\0', 96);
	for ( ; curr != NULL; curr = curr->nextgroup) {	
		if (!strncmp(curr->newsgroup, string, len)) {
			if (!strlen(match))
				strcpy(match, curr->newsgroup);
			else {
				int i = 0;
				while (!strncmp(curr->newsgroup, match, ++i));
				match[--i] = '\0';
			}
		}
	}

	return match;
}
