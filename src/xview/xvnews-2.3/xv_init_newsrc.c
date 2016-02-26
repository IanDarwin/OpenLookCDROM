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
#include <sys/types.h>
#include <sys/stat.h>
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

STATIC_FUNCTION( int check_subs, (char *));
STATIC_FUNCTION( struct newsrc_node *createSingleNewsrc, ());
STATIC_FUNCTION( struct newsrc_node *create_newsrc, (char *[], int, int*));
STATIC_FUNCTION( int get_descriptions, ());
STATIC_FUNCTION( void parseSingleNewsrc, (FILE *));


/*
 *Check the .newsrc file. If it is not there create one from the
 *server.  If there is one, parse it and check for bogus entries, old
 *newsgroups and new newsgroups available to the user. Returns the
 *head of a linked list.
 */

extern void check_newsrc(ip)
xvnews_xvnews_window_objects    *ip;
{
   char aline[BUFFERLEN + 1], sgroup[BUFFERLEN + 1], group[BUFFERLEN + 1];
   char	line[80], **newslist;
   char	status;
   int *articles, l_max = INIT_NUM_GROUPS;
   int active=0, lines=0, done=0, row=0, i, first, last, count=0;
   FILE	*ifp_in;
   struct newsrc_node *new, *prev = NULL, *curr;

   newslist = (char **)malloc(INIT_NUM_GROUPS * sizeof(char *));
   articles = (int *)malloc(INIT_NUM_GROUPS * sizeof(int));
   
   if (Global->single == NULL) {
      put_server("LIST");
      get_server(aline, sizeof(aline));
      get_server(aline, sizeof(aline));
      while (*aline != '.') {
	 sscanf(aline, "%s%d%d%*c%c", group, &first, &last, &status);
#ifdef CNEWS_DONT_SHOW
	 if (status == '=' || status == 'x') {
	    get_server(aline, sizeof(aline));
	    continue;
	 }
#endif
	 articles[lines] = first - last;
	 newslist[lines] = strdup(group);
	 if (++lines >=l_max) 
	 {
	    l_max += INIT_NUM_GROUPS;
	    newslist = (char **)realloc(newslist, l_max * sizeof(char *));
	    articles = (int *)realloc(articles, l_max * sizeof(int));
	 }	    
	 get_server(aline, sizeof(aline));
      }
   }

   if ((ifp_in = fopen(Global->newsrc, "r")) == NULL) {
     printf("No %s found, creating a new one...\n", Global->newsrc);
     if (Global->single != NULL)
       Global->head = createSingleNewsrc();
     else {
       Global->head = create_newsrc(newslist, lines, articles);
       get_descriptions();
       sort_newsrc();
     }
     Global->mode = GROUP_MODE;
     get_groups(ip, 1);
     return;
   }

   if (Global->single != NULL) {
     parseSingleNewsrc(ifp_in);
     fclose(ifp_in);
     return;
   }

   while(fgets(aline, BUFFERLEN, ifp_in) != NULL) {
     aline[BUFFERLEN] = '\0';
     ++count;
     if ((strchr(aline, '!') == NULL) && (strchr(aline, ':') == NULL)) {
       printf("Bogus entry %s on line %d of %s\n",
	      aline, count, Global->newsrc);
       continue;
     }
     sscanf(aline, "%[^:!]s", sgroup);
     sgroup[BUFFERLEN] = '\0';
     done = 0;
     for (i = 0; i < lines; ++i) {
       if (newslist[i] == NULL)
	 continue;
       if (!strcmp(sgroup, newslist[i])) {
	 new = parse_line(aline, newslist[i]);
	 if (articles[i] < 0)
	   new->articles = 0;
	 else
	   new->articles = articles[i];
	 done = 1;
	 newslist[i] = NULL;
	 break;
       }
     }
     if (done == 0)
       printf("Invalid newsgroup %s, line %d of %s\n",
	      sgroup, count, Global->newsrc);
     else
       ++active;
   }
   fclose(ifp_in);

	if (active != lines) {
		printf("Checking for new newsgroups...\n");
		for (i = 0; i < lines; i++) {
			if (newslist[i] == NULL)
				continue;
			done = 0;
			for (curr = Global->head;curr != NULL;curr = curr->nextgroup) {
				if (!strcmp(curr->newsgroup, newslist[i])) {
					done = 1;
					break;
				} else
					prev = curr;
			}
			if (done == 0) {
				Global->mode = NEW_GROUPS;
				new = (struct newsrc_node *)
				  malloc(sizeof(struct newsrc_node));
				assert( new );
				
				if (prev == NULL)
					Global->head = new;
				else
					prev->nextgroup = new;
				new->nextgroup = NULL;
				new->artlist = NULL;
				new->newsgroup = newslist[i];
				new->subscribed = UNSUBSCRIBED;
				new->kill = NULL;
				new->description = NULL;
				new->last_article = 0;
				if (articles[i] < 0)
					new->articles = 0;
				else
					new->articles = articles[i];
				sprintf(line, "%-64.64sunsubscribed", newslist[i]);
				xv_set(ip->groups_list,
				       PANEL_LIST_INSERT, row,
				       PANEL_LIST_STRING, row, line,
				       PANEL_LIST_FONT, row, Global->listfont,
				       NULL);
/* This might help against the odd problems with empty lines,
   but why? */
				if (row == 0) {
				  xv_set(ip->groups_list,
					 PANEL_LIST_SELECT, 0, TRUE, NULL);
				  Global->group = new;
				}
				/*	*/
				++row;
				++active;
			}
			if (active == lines) {
				get_descriptions();
				return;
			}
		}
	}
	get_descriptions();
   free(newslist);
   free(articles);
}

struct newsrc_node *
create_newsrc(newslist, lines, articles)
char *newslist[];
int lines;
int *articles;
{
  struct newsrc_node	*head = NULL, *new, *prev = NULL;
  char aline[BUFFERLEN + 1], group[BUFFERLEN];
  int i, result, l_max;

  /* First, create entries for the default subscription */
  if (!Global->subs_read) {
    Global->subslist = (char **)malloc(INIT_NUM_GROUPS * sizeof(char *));
    l_max = INIT_NUM_GROUPS;

    put_server("LIST SUBSCRIPTIONS");
    result = get_server(aline, sizeof(aline));
    if (result == OK_GROUPS) {
      while (*aline != '.') {
	get_server(aline, sizeof(aline));
	if ((strlen(aline) != 1) && (aline[0] != '.')) {
	  sscanf(aline, "%s", group);
	  Global->subslist[Global->num_subs] = strdup(group);
	  if (++Global->num_subs >= l_max) {
	    Global->subslist = (char **)realloc(Global->subslist, l_max * sizeof(char *));
	  }					    
	}
      }
    }
    else {
      Global->num_subs = 2;
      Global->subslist[0] = strdup("news.announce.newusers");
      Global->subslist[1] = strdup("news.announce.questions");
    }
    Global->subs_read = 1;
  }

  /* Now get all the groups, and see if they should be subscribed */
  for (i = 0; i < lines; ++i) {
    new = (struct newsrc_node *) malloc(sizeof(struct newsrc_node));
    assert( new );
    head == NULL ? head = new:NULL;
    new->artlist = NULL;
    new->last_article = 0;
    new->description = NULL;
    new->kill = NULL;
    new->newsgroup = newslist[i];
    if (articles[i] > 0)
      new->articles = articles[i];
    else
      new->articles = 0;
    if (check_subs(newslist[i])) {
      new->subscribed = SUBSCRIBED;
    }
    else {
      new->subscribed = UNSUBSCRIBED;
    }
    prev != NULL ? prev->nextgroup = new:NULL;
    new->nextgroup = NULL;
    prev = new;
  }
  return head;
}

struct newsrc_node *
createSingleNewsrc()
{
	char message[MAX_MESSAGE_LEN];
	int status;
	struct newsrc_node	*new = NULL;

	new = (struct newsrc_node *) malloc(sizeof(struct newsrc_node));
	if (new == NULL) {
		printf("Malloc failed!\n");
		exit(-1);
	}
	new->artlist = NULL;
	new->subscribed = SUBSCRIBED;
	new->newsgroup = Global->single;
	new->nextgroup = NULL;
	new->description = NULL;
	new->kill = NULL;
	new->last_article = 0;
	new->articles = 0;
	put_server("GROUP %s", new->newsgroup);
	status = get_server(message, sizeof(message));
	if (status != OK_GROUP) {
		printf("Invalid group %s, exiting...\n", new->newsgroup);
		close_server();
		exit(-1);
	}
	return new;
}

extern int sort_newsrc()
{
	struct newsrc_node	*new_head;
	struct newsrc_node	*curr;
	struct newsrc_node	*new;
	struct newsrc_node	*prev_new;
	struct newsrc_node	*prev;

	curr = Global->head;
	new = NULL;
	prev_new = NULL;
	prev = NULL;

	new_head = curr;
	curr = curr->nextgroup;

	new_head->nextgroup = NULL;

	while(curr != NULL) {
			new = new_head;
			prev_new = NULL;
			while(new != NULL) {
				if (strcmp(curr->newsgroup,new->newsgroup) < 0) {
					prev = curr->nextgroup;
					curr->nextgroup = new;
					if (prev_new == NULL)
						new_head = curr;
					else
						prev_new->nextgroup = curr;
					curr = prev;
					break;
				} else {
					prev_new = new;
					new = new->nextgroup;
				}
				if (new == NULL) {
					prev_new->nextgroup = curr;
					curr = curr->nextgroup;
					prev_new->nextgroup->nextgroup = NULL;
				}
			}
	}
	Global->head = new_head;
	return 1;
}

STATIC int get_descriptions()
{
	char	aline[256], group[BUFFERLEN + 1];
	char	*c, *p;
	int	status;
	struct newsrc_node	*curr;

#ifdef _DEBUG_MALLOC_INC
	union dbmalloptarg m;
	m.i = 0;
	dbmallopt(MALLOC_CKDATA,m);
#endif

	if (!Global->list || Global->nnrp)
		return -1;

	put_server("LIST NEWSGROUPS");
	status = get_server(aline, sizeof(aline));

	if (status != OK_GROUPS)
		return -1;

	while(*aline != '.') {
		get_server(aline, sizeof(aline));
		sscanf(aline, "%s", group);
		for (curr = Global->head; curr != NULL; curr = curr->nextgroup) {
			if (curr->description == NULL && !strcmp(curr->newsgroup, group)) {
				c = &aline[strlen(group)+1];
				while(*c == ' ' || *c == '	' || *c == '\n')
					++c;
				if ((p = strchr(c, '\n')) != NULL)
					*p = '\0';
				curr->description = strdup(c);
				break;
			}
		}
	}
#ifdef _DEBUG_MALLOC_INC
	m.i = 1;
	dbmallopt(MALLOC_CKDATA,m);
#endif

	return 1;
}

static void parseSingleNewsrc(ifp_in)
FILE	*ifp_in;
{
	int count = 0, status;
	char		aline[BUFFERLEN], ngroup[BUFFERLEN + 1], sgroup[BUFFERLEN + 1];
	char message[MAX_MESSAGE_LEN];
	struct		newsrc_node	*new = NULL;

	while(fgets(aline, BUFFERLEN, ifp_in) != NULL) {
		count++;
		sscanf(aline, "%s", ngroup);
		if ((strchr(ngroup, '!') == NULL) &&
			 (strchr(ngroup, ':') == NULL)) {
			printf("Bogus entry %s on line %d of %s\n",
				 ngroup, count, Global->newsrc);
			continue;
		}
		strncpy(sgroup, ngroup, strlen(ngroup) - 1);
                sgroup[strlen(ngroup) - 1] = '\0';
		if (strcmp(sgroup, Global->single))
			continue;
		new = (struct newsrc_node *)parse_line(aline, sgroup);
		new->articles = 0;
		put_server("GROUP %s", sgroup);
		status = get_server(message, sizeof(message));
		if (status != OK_GROUP) {
			printf("Invalid group %s, exiting...\n", sgroup);
			close_server();
			exit(-1);
		}
		return;
	}
}

static int check_subs(newsgroup)
char *newsgroup;
{
  int i;

  for (i=0; i < Global->num_subs; ++i) {
    if (strcmp(Global->subslist[i], newsgroup) == 0)
      return(1);
  }
  return(0);
}				
