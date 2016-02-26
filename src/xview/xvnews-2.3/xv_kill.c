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
#include <xview/notice.h>
#include <xview/termsw.h>
#include <xview/text.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"

extern struct globals *Global;

STATIC_FUNCTION( int confirmKillAll, (kill_popup_objects *));
STATIC_FUNCTION( int countKills, (struct kill_list *));
STATIC_FUNCTION( int getHeadList, (struct kill_node *, char *, int *));
STATIC_FUNCTION( void makeKillFile, (char *));
STATIC_FUNCTION( void parseKillFiles, (struct newsrc_node *));
STATIC_FUNCTION( int processKill, (char *, struct kill_node *, int *));
STATIC_FUNCTION( char *strlower, (char *));
STATIC_FUNCTION( int confirmKillAll, (kill_popup_objects *));
STATIC_FUNCTION( int countKills, (struct kill_list *));
STATIC_FUNCTION( char *escapeMetas, (char *));

extern char *
groupDir(group)
char	*group;
{

static	char	group_dir[128];
	char	*c;

	strcpy(group_dir, group);
	c = strchr(group_dir, '.');
	while (c != NULL) {
		*c = '/';
		c = strchr(group_dir, '.');
	}
	return group_dir;
}

static int processKill(subject, kill, place)
char	*subject;
struct kill_node *kill;
int	*place;
{
	int	dead = -1;
	char	*c;

	c = subject;
	while (isdigit(*c) || *c == ' ')
                 c++;
	while (kill != NULL) {
	        char *newc = NULL;

		if (!kill->subject) {
			kill = kill->next;
			continue;
		}
		if (!kill->mixedcase) {
		  newc = strlower(c);
		  c = newc;
		}
		xvnews_comp(kill->string);
		if (xvnews_exec(c)) {
			dead = kill->junk;
			*place = kill->place;
		}
		kill = kill->next;

		if (newc) {
		  free(newc);
		}
	}
	return dead;
}

static void parseKillFiles(curr)
struct newsrc_node *curr;
{
        static	char	file[256], aline[160];
	char	*c, *a, *err;
	int	place = 1;
	FILE	*ifp_in;
	struct kill_node	*kill = NULL, *prev = NULL;

	if (curr) {
	  sprintf(file, "%s/%s/KILL", Global->newsdir,
		  groupDir(curr->newsgroup));
	}
	else {
	  sprintf(file, "%s/KILL", Global->newsdir);
	}

	if ((ifp_in = fopen(file, "r")) == NULL) {
		if (curr)
		  curr->kill = NULL;
		else
		  Global->kill = NULL;
		return;
	}

	while(fgets(aline, 160, ifp_in) != NULL) {
		if (aline[0] == '#' || aline[0] == '\n')
			continue;
		if (strstr(aline, "THRU") != NULL)
			continue;
		c = strstr(aline, "/");
		if (c == NULL) {
			printf("Malformed entry %s in kill file %s!\n",
				 aline, file);
			continue;
		}
		if (*(++c) == ':')
			c++;
		if (*c == ' ')
			c++;
		if (*c == '*')
			c++;
		a = strchr(c, '/');
		if (a == NULL) {
			printf("Malformed entry %s in kill file %s!\n",
				 aline, file);
			continue;
		}
		while (*(a - 1) == '\\')
			a = strchr((a + 1), '/');
		*a = '\0';
		err = xvnews_comp(c);
		if (err != NULL) {
			printf("\nExpression error: %s!\nLine \"%s\" in file %s\n",
					err, aline, file);
			continue;
		}
		kill = (struct kill_node *)malloc(sizeof(struct kill_node));
		kill->string = strdup(c);
		/*  By default, kill by subject, unless header is
		    explicitly mentioned, see below. */
		kill->subject = 1;
		kill->mixedcase = 1;
		kill->junk = 0;
		kill->place = place++;
		kill->next = NULL;

		*a = '/';
		for (++a; *a && *a != ':'; ++a) {
		  switch (*a) {
		  case 'h':
		    kill->subject = 0;
		    break;
		  case 'c': {
		    char *string = kill->string;
		    kill->string = strlower(string);
		    free(string);
		  }
		    kill->mixedcase = 0;
		    break;
		  }
		}

		/* If not at the end of string, then skip ':' */
		if (*a)
		  ++a;

		if (*a == 'j')
			kill->junk = 1;
		if (prev == NULL)
			if (curr)
			  curr->kill = kill;
			else
			  Global->kill = kill;
		else 
			prev->next = kill;
		prev = kill;
	}
	fclose(ifp_in);
}	

extern void clearKills(first)
struct kill_node *first;
{
	struct kill_node *next = NULL;

	while (first != NULL) {
		free(first->string);
		next = first->next;
		free(first);
		first = next;
	}
	first = NULL;
}

extern int addKill(kp, exp, group, subject, junk)
kill_popup_objects *kp;
char *exp, *group;
int subject, junk;
{
static	char	file[256], buff[256], error[512];
	char	 *edit;
	int row = xv_get(kp->file_list, PANEL_LIST_NROWS, NULL);
	FILE *ifp_out;
	struct stat statbuf;

	if (strlen(group))
		sprintf(file,"%s/%s",Global->newsdir,groupDir(group));
	else
		strcpy(file, Global->newsdir);

	if (stat(file, &statbuf) != 0) 
		makeKillFile(groupDir(group));

	sprintf(file, "%s/KILL", file);

	if ((ifp_out = fopen(file, "a")) == NULL) {
		sprintf(error, "Unable to open %s!\n", file);
		xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, error, NULL);
		return -1;
	}

	if (subject)
		sprintf(buff, "/%s/:%c", exp, junk ? 'j':'m');
	else
		sprintf(buff, "/%s/h:%c", exp, junk ? 'j':'m');
	fprintf(ifp_out, "%s\n", buff);
	fclose(ifp_out);

	edit = (char *)xv_get(kp->save_butt, PANEL_CLIENT_DATA, NULL);
	if (edit != NULL && !strcmp(edit, file))
		xv_set(kp->file_list, PANEL_LIST_INSERT, row,
				PANEL_LIST_STRING, row, buff,
				NULL);

	sprintf(error, "Added \"%s\" to %s\n", exp, file);
	xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, error, NULL);
	return 1;
}

STATIC void makeKillFile(file)
char	*file;
{
	char	home[256], *c;
	struct stat statbuf;

	strcpy(home, Global->newsdir);
	
	if (stat(home, &statbuf) != 0)
                mkdir(home, 00755);

	c = home;
	c += strlen(home) + 1;
	sprintf(home, "%s/%s", home, file);
	c = strchr(c, '/');
	while (c != NULL) {
		*c = '\0';
		if (stat(home, &statbuf) != 0)
                	mkdir(home, 00755);
		*c++ = '/';
		c = strchr(c, '/');
	}
	if (stat(home, &statbuf) != 0)
                mkdir(home, 00755);
}

extern void generateKillList(curr)
struct newsrc_node *curr;
{
	clearKills(Global->kill);
	Global->kill = NULL;
	parseKillFiles(NULL);

	clearKills(curr->kill);
	curr->kill = NULL;
	parseKillFiles(curr);
}

extern int articleKill(ip, list)
xvnews_xvnews_window_objects	*ip;
struct kill_list	*list;
{
	char    *old;
        int     rows = xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);
        int     row, num, kills = 0;
	struct kill_list	*kill = list, *prev = NULL;
	xvnews_undelete_popup_objects *up = (xvnews_undelete_popup_objects *) xv_get(ip->xvnews_window, XV_KEY_DATA, UNDELETE_POPUP, NULL);
	

	xv_set(up->undelete_list, XV_SHOW, FALSE, NULL);
	for (row = 0; row < rows; row++) {
                    old = (char *)xv_get(ip->articles_list,
                            PANEL_LIST_STRING, row, NULL);
                    sscanf(old, "%d", &num);
		    while (list != NULL) {
			if (!list->junk) {
				if (prev != NULL) {
					prev->next = list->next;
					free(list);
					list = prev->next;
				} else {	
					kill = list->next;
					free(list);
					list = kill;
				}
				continue;
		    	}
			if (list->article == num) {
				char delete[SUBJECT_LENGTH + AUTHOR_LENGTH + 2];
				add_articles(Global->group, num, num);
				strncpy(delete, old,
					SUBJECT_LENGTH + AUTHOR_LENGTH);
				delete[SUBJECT_LENGTH + AUTHOR_LENGTH] = '\0';
				xv_set(up->undelete_list,
					PANEL_LIST_INSERT, kills,
					PANEL_LIST_STRING, kills, delete,
					NULL);
				if (!xv_get(up->undelete_list,PANEL_LIST_STRING,kills))
					xv_set(up->undelete_list, PANEL_LIST_DELETE, kills, NULL);
				else
					kills++;
				xv_set(ip->articles_list, PANEL_LIST_DELETE, row--, NULL);
                                        rows--;
					if (prev != NULL) 
						prev->next = list->next;
					else	
						kill = list->next;
					free(list);
                                        break;
			}
			prev = list;
			list = list->next;
		    }
		    prev = NULL;
		    list = kill;
	}
	if (Global->undelete_popup) {
	  xv_set(up->undelete_list, XV_SHOW, TRUE, NULL);
	}
	if (kills) {
		xv_set(up->controls3, WIN_FONT, Global->listfont, NULL);
		xv_set(up->undelete_list, PANEL_LIST_FONTS, Global->listfont, NULL, NULL);
	}
	while (list != NULL) {
		prev = list->next;
		free(list);
		list = prev;
	}

	return kills;
}

struct kill_list *
generateKill(list, article, junk, place, global)
struct kill_list *list;
int	article, junk, place, global;
{
	struct kill_list *new = NULL, *head = list, *prev = NULL;

	new = (struct kill_list *)malloc(sizeof(struct kill_list));
	new->article = article;
	new->junk = junk;
	new->global = global;
	new->place = place;
	new->next = NULL;

	if (list == NULL)
		return new;

	while (list != NULL) {
		if (list->article == article) {
			if (!list->global && global) {
				free(new);
				return head; 
			}
			if ((place < list->place) && (global == list->global)) {
				free(new);
				return head; 
			}
			if ((list->global == global && place > list->place) ||
				(!global && list->global)) {
				if (prev != NULL)
					prev->next = new;
				else
					head = new;
				new->next = list->next;
				free(list);
				return head;
			}
			free(new);
			return head;
		}
		prev = list;
		list = list->next;
	}

	prev->next = new;
	return head;
}

extern int headerKill(kill)
struct kill_node *kill;
{
	while (kill != NULL) {
		if (!kill->subject)
			return 1;
		kill = kill->next;
	}
	return 0;
}

struct kill_list *
killHeaders(global, local, first, last, list)
struct kill_node *global, *local;
int first, last;
struct kill_list *list;
{
	int status, place, g_status = -1, l_status = -1, l_dead = -1, g_dead = -1;
static	char article[MAX_ARTICLE_LEN];

	while (first <= last) {
		put_server("HEAD %d", first);
		status = get_server(article, sizeof(article));
		if (status != OK_HEAD) {
			if (status == ERR_FAULT)
				reconnect_server();
			first++;
			continue;
		}
		while(*article!= '.') {
			get_server(article, sizeof(article));
			if (global != NULL)
				g_status = getHeadList(global, article, &place);
			if (local != NULL)
				l_status = getHeadList(local, article, &place); 
			if (g_status != -1)
				g_dead = g_status;
			if (l_status != -1)
				l_dead = l_status;
		}
		if (l_dead == 1 || !l_dead) 
			list = generateKill(list, first, l_dead, place, 0);
		else
			if (g_dead == 1 || !g_dead) 
				list = generateKill(list, first, g_dead, place, 1);
		g_dead = -1;
		l_dead = -1;
		first++;
	}
	return list;
}

STATIC int getHeadList(kill, string, place)
struct kill_node *kill;
char *string;
int	*place;
{
	int dead = -1;

	while(kill != NULL) {
		if (kill->subject) {
			kill = kill->next;
			continue;
		}
		xvnews_comp(kill->string);
		if (xvnews_exec(string)) {
			dead = kill->junk;
			*place = kill->place;
		}
		kill = kill->next;
	}
	return dead;
}

struct kill_list *
subjectKill(subject, kill, kills)
char	*subject;
struct kill_node *kill;
struct kill_list *kills;
{
	int global = -1, local = -1, art, place = 1;

	if (Global->kill != NULL) 
		global = processKill(subject, Global->kill, &place);
	
	if (kill != NULL)
		local = processKill(subject, kill, &place);

	sscanf(subject, "%d", &art);
	if (local == 1 || !local) {
		kills = generateKill(kills, art, local, place, 0);
		return kills;
	}

	if (global == 1 || !global) {
		kills = generateKill(kills, art, global, place, 1);
		return kills;
	}

	return kills;
}

extern char *
currentSubject(ip, row)
xvnews_xvnews_window_objects *ip;
int	row;
{
	char *a, *exp;
static  char    string[SUBJECT_LENGTH + 1], *c;

	if (row == -1)
		return NULL;

	exp = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);

	if (exp == NULL)
		return NULL;

        memset(string, '\0', SUBJECT_LENGTH + 1);
        strncpy(string, exp, SUBJECT_LENGTH);
        c = string;
        while (isdigit(*c++));
        a = c;
        a += (strlen(c) - 1);
        while(*a == ' ')
                   *a-- = '\0';
	return c;
}

extern char *currentAuthor(flg)
  int flg;
{
  static char author[128];
  char *c, *result, line[1024];
  int status;

  put_server("HEAD");
  status = get_server(line, sizeof(line));
  if (status != OK_HEAD) {
    reconnect_server();
    put_server("HEAD %d", Global->article);
    status = get_server(line, sizeof(line));
    if (status != OK_HEAD)
      return NULL;
  }
  
  get_server(line, sizeof(line));
  while(line[0] != '.') {
    if (!strncmp(line, "From", 4))
      strcpy(author, &line[5]);
    get_server(line, sizeof(line));
  }
  
  result = author;
  if (*result == ' ')
    result++;
  
  c = strchr(result, '\n');
  if (c)
    *c = '\0';
  
  if (flg) {
    c = strchr(result, '(');
    if (c) {
      --c;
      *c = '\0';
    }
  }

  return result;
}

extern void fillKillText(ip, kp)
xvnews_xvnews_window_objects *ip;
kill_popup_objects *kp;
{
	int state = xv_get(kp->kill_text, PANEL_CLIENT_DATA, NULL);

	assert( state == 0 || state == 1 );

	if (!xv_get(kp->kill_popup, XV_SHOW, NULL))
		return;

	switch (state) {
	case 0:
	{
		int row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED);
		char *str = escapeMetas(currentSubject(ip, row));

		xv_set(kp->kill_text, PANEL_VALUE, str, NULL);
                xv_set(kp->mod_choice, PANEL_VALUE, 1, NULL);
                xv_set(kp->command_choice, PANEL_VALUE, 1, NULL);
		free(str);
		break;
	}
	case 1:
		xv_set(kp->kill_text, PANEL_VALUE, (char *)currentAuthor(1), NULL);
                xv_set(kp->mod_choice, PANEL_VALUE, 0, NULL);
                xv_set(kp->command_choice, PANEL_VALUE, 1, NULL);
		break;
	}
}

extern int loadKill(kp, group)
kill_popup_objects	*kp;
char	*group;
{

	int	row = 0;
	char	aline[160], *a;
static	char	file[256];
	FILE *ifp_in;

	if (xv_get(kp->save_butt, PANEL_CLIENT_DATA) != (Xv_opaque)NULL &&
		xv_get(kp->delete_kill_butt, PANEL_CLIENT_DATA))
			confirmKill(kp);

	xv_set(kp->delete_kill_butt, PANEL_CLIENT_DATA, 0, NULL);
	xv_set(kp->file_list, PANEL_LIST_DELETE_ROWS, 0,
		xv_get(kp->file_list, PANEL_LIST_NROWS, NULL), NULL);

	if (strlen(group))
		sprintf(file, "%s/%s/KILL",Global->newsdir,groupDir(group));
	else
		sprintf(file, "%s/KILL",Global->newsdir);

	if ((ifp_in = fopen(file, "r")) == NULL) {
			xv_set(kp->save_butt, PANEL_CLIENT_DATA, NULL, NULL);
			sprintf(file, "%s not found!\n", file);
			xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, file, NULL);
			return -1;
	}

	while(fgets(aline, 160, ifp_in) != NULL) {
		if ((a = strchr(aline, '\n')) != NULL)
			*a = '\0';
		if (strlen(aline)) {
			xv_set(kp->file_list,
				PANEL_LIST_INSERT, row,
				PANEL_LIST_STRING, row, aline,
				NULL);
			row++;
		}
	}

	fclose(ifp_in);
	if (row) {
		xv_set(kp->save_butt, PANEL_CLIENT_DATA, file, NULL);
		xv_set(kp->kill_controls, WIN_FONT, Global->listfont, NULL);
		xv_set(kp->file_list,
			 PANEL_LIST_FONTS, Global->listfont, NULL,
			 NULL);
		sprintf(aline, "Kill file %s loaded\n", file);
	} else
		sprintf(aline, "No lines in %s!\n", file);

	xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, aline, NULL);

	return row;
}	

extern int saveKillFile(kp)
kill_popup_objects	*kp;
{
	char	*file = (char *)xv_get(kp->save_butt, PANEL_CLIENT_DATA, NULL);
	char	*string, error[256];
	int 	row, rows = xv_get(kp->file_list, PANEL_LIST_NROWS, NULL);
	FILE *ifp_out;

	xv_set(kp->delete_kill_butt, PANEL_CLIENT_DATA, 0, NULL);
	if (file == NULL) {
		sprintf(error, "No KILL file loaded!\n");
		xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, error, NULL);
		return -1;
	}

	if (!rows) {
		if (!unlink(file)) {
			sprintf(file, "%s removed!\n", file);
			xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, file, NULL);
			xv_set(kp->save_butt, PANEL_CLIENT_DATA, NULL, NULL);
			return 1;
		} else {
			sprintf(error, "Unable to delete %s!\n", file);
			xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, error, NULL);
			return -1;
		}
	}

	if ((ifp_out = fopen(file, "w")) == NULL) {
		sprintf(error, "Unable to save kill file %s!\n", file);
		xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, error, NULL);
		return -1;
	}

	for (row = 0; row < rows; row++) {
		string = (char *)xv_get(kp->file_list, PANEL_LIST_STRING, row, NULL);
		fprintf(ifp_out, "%s\n", string);
	}
	
	fclose(ifp_out);

	sprintf(error, "Kill file %s updated\n", file);
	xv_set(kp->save_butt, PANEL_CLIENT_DATA, NULL, NULL);
	xv_set(kp->file_list, PANEL_LIST_DELETE_ROWS, 0, rows, NULL);
	xv_set(kp->kill_popup, FRAME_LEFT_FOOTER, error, NULL);
	
	return 1;
}

extern void sessionSubjectKill(ip, kp, kill)
xvnews_xvnews_window_objects	*ip;
kill_popup_objects	*kp;
struct kill_node *kill;
{
	int killed, rows = xv_get(ip->articles_list, PANEL_LIST_NROWS), row = 0;
	int	orig, now;
	char	*old, subject[SUBJECT_LENGTH + 2];
	struct kill_list	*kills = NULL;

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, xv_get(ip->articles_list,
		PANEL_LIST_FIRST_SELECTED));
	sscanf(old, "%d", &orig);	
	for (; row < rows; row++) {
		old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
		strncpy(subject, old, SUBJECT_LENGTH);
		kills = subjectKill(subject, kill, kills);
	}

	if (countKills(kills) == xv_get(ip->articles_list, PANEL_LIST_NROWS)) {
		if (!confirmKillAll(kp))
			return;
		if (Global->mode == ARTICLE_MODE)
			update_newsrc(ip, 1);
		groups_set(ip);
		xvnews_err(ip, "All articles killed\n");
		return;
	}

	xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
	killed = articleKill(ip, kills);
	xv_set(ip->articles_list, XV_SHOW, TRUE, NULL);

	if (!killed)
		return;

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, xv_get(ip->articles_list,
		PANEL_LIST_FIRST_SELECTED));
	sscanf(old, "%d", &now);	
	if (orig != now) {
		Global->article = next_unread_art(ip);
		retrieve_article(ip, Global->article);
	}
	xvnews_err(ip, "%d %s killed matching \"%s\"!\n", killed,
		 killed == 1 ? "article":"articles", kill->string);
}

extern void sessionHeaderKill(ip, kp, kill)
xvnews_xvnews_window_objects	*ip;
kill_popup_objects	*kp;
struct kill_node *kill;
{
	char	*old;
	int orig, now, first, last, killed;
	struct kill_list *kills = NULL;	

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, xv_get(ip->articles_list,
		PANEL_LIST_FIRST_SELECTED));
	sscanf(old, "%d", &orig);	
	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, 0, NULL);
	if (old == NULL)
		return;
	sscanf(old, "%d", &first);
	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, 
		xv_get(ip->articles_list, PANEL_LIST_NROWS) - 1, NULL);
	sscanf(old, "%d", &last);

	kills = killHeaders(NULL, kill, first, last, kills);

	if (countKills(kills) == xv_get(ip->articles_list, PANEL_LIST_NROWS)) {
		if (!confirmKillAll(kp))
			return;
		if (Global->mode == ARTICLE_MODE)
			update_newsrc(ip, 1);
		groups_set(ip);
		xvnews_err(ip, "All articles killed\n");
		return;
	}
	

	xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
	killed = articleKill(ip, kills);
	xv_set(ip->articles_list, XV_SHOW, TRUE, NULL);

	if (!killed)
		return;

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, xv_get(ip->articles_list,
		PANEL_LIST_FIRST_SELECTED));
	sscanf(old, "%d", &now);	
	if (orig != now) {
		Global->article = next_unread_art(ip);
		retrieve_article(ip, Global->article);
	}
	xvnews_err(ip, "%d %s killed matching \"%s\"!\n", killed,
		 killed == 1 ? "article":"articles", kill->string);
}

extern void confirmKill(kp)
kill_popup_objects	*kp;
{

	char	*file = (char *)xv_get(kp->save_butt, PANEL_CLIENT_DATA, NULL);

	if (notice_prompt(kp->kill_controls, NULL,
                NOTICE_MESSAGE_STRINGS, "Modifications have been made to",
                	file, "Save edits?",
			NULL,
                 NOTICE_BUTTON_YES, "save",
                 NOTICE_BUTTON_NO, "cancel",
                 NULL))
		saveKillFile(kp);

	xv_set(kp->delete_kill_butt, PANEL_CLIENT_DATA, 0, NULL);
}

static char *
escapeMetas(exp)
char	*exp;
{
	char	*c, *new, *a;

	new = (char *)malloc((strlen(exp) * 2) + 1);
	
	memset(new, '\0', strlen(exp) * 2);
	c = exp;
	a = new;

	while (*c != '\0') {
		if (*c == '[' || *c == ']' || *c == '<' || *c == '>' || *c == '+' ||
		 *c == '/' || *c == '?' || *c == '*' || *c == '{' || *c == '}')
			*a++ = '\\';
		*a++ = *c++;
	}

	return new;
}

STATIC int countKills(list)
struct kill_list	*list;
{
	struct kill_list *next = list;
	int count = 0;

	while (next != NULL) {
		++count;
		next = next->next;
	}	

	return count;
}

STATIC int confirmKillAll(kp)
kill_popup_objects	*kp;
{
	return notice_prompt(kp->kill_controls, NULL,
                NOTICE_MESSAGE_STRINGS, "Confirm killing of all articles in",
                	Global->group->newsgroup, NULL,
                 NOTICE_BUTTON_YES, "kill",
                 NOTICE_BUTTON_NO, "cancel",
                 NULL);
}

struct kill_list *
messageKill(first, last, kill)
int first, last;
struct kill_list *kill;
{
static		char message[MAX_MESSAGE_LEN];
		char	*c;
		int	status, art;
		struct message	*list = Global->messages;

	if (list == NULL || Global->single != NULL)
		return kill;

	put_server("XHDR message-id %d-%d", first, last);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		reconnect_server();
		put_server("XHDR message-id %d-%d", first, last);
		status = get_server(message, sizeof(message));
		if (status != OK_HEAD)
			return kill;
	}
	get_server(message, sizeof(message));
	while (*message != '.') {
	  if ((c = strchr(message, '<'))) {
	    list = Global->messages;
	    while(list != NULL) {
	      if (!strcmp(list->message, c)) {
		sscanf(message, "%d", &art);
		kill = generateKill(kill, art, 1, 1, 0);
		break;
	      }
	      list = list->next;
	    }
	  }
	  get_server(message, sizeof(message));
	}

	return kill;
}

extern void genMessage(art)
int art;
{
	char	*c, *messageId();
	struct message *list = Global->messages, *new = NULL;

	if (Global->mode != ARTICLE_MODE || Global->single != NULL)
		return;

	c = messageId(art);
	while (list != NULL) {
		if (!strcmp(list->message, c))
			return;
		list = list->next;
	}
	new = (struct message *)malloc(sizeof(struct message));
	new->next = NULL;
	new->message = strdup(c);
	new->next = Global->messages;
	Global->messages = new;	
}

extern void removeMessage(art)
int art;
{
	char *c, *messageId();
	struct message *list = Global->messages, *prev = NULL;

	if (Global->mode != ARTICLE_MODE)
		return;

	c = messageId(art);
	while (list != NULL) {
		if (!strcmp(list->message, c)) {
			free(list->message);
			if (prev != NULL)
				prev->next = list->next;
			else
				Global->messages = list->next;
			free(list);
			return;
		}
		prev = list;
		list = list->next;
	}
}

char *
messageId(art)
int art;
{
static	char	command[MAX_COMMAND_LEN], message[MAX_MESSAGE_LEN];
	int status;
	char *c;

	put_server("XHDR message-id %d", art);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		put_server("XHDR message-id %d", art);
		status = get_server(message, sizeof(message));
		if (status != OK_HEAD)
			return NULL;
	}
	get_server(message, sizeof(message));
	if ((c = strchr(message, '<')) == NULL)
		c = message;
	get_server(command, sizeof(command));

	return c;
}

extern void freeMessages()
{
	struct message *list = Global->messages, *next = NULL;

	while(list != NULL) {
		free(list->message);
		next = list->next;
		free(list);
		list = next;
	}
	Global->messages = NULL;
}

extern void undeleteAll(ip)
xvnews_xvnews_window_objects *ip;
{
	int nrows = xv_get(ip->articles_list, PANEL_LIST_NROWS), row, art;
	char	*str;

	for (row = 0; row < nrows; row++) {
		str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row);
		if (strlen(str) < (SUBJECT_LENGTH + AUTHOR_LENGTH + 4))
			continue;
		sscanf(str, "%d", &art);
		removeMessage(art);
	}
}

static char *strlower(string)
  char *string;
{
  char *newstring = strdup(string);
  char *n = newstring;

  while ((*n = *string)) {
    if (isupper(*n)) {
      *n = tolower(*n);
    }
    ++n;
    ++string;
  }

  return(newstring);
}
