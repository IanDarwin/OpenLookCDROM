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
#include <xview/panel.h>
#include "xvnews.h"
#include "xvnews_ui.h"


extern struct globals	*Global;

/* Tack two adjacent nodes together, if possible */
STATIC void adjacent_nodes(artnode)
  a_node *artnode;
{
  a_node *nextartnode;

  assert( artnode );
  nextartnode = artnode->nextart;

  if (nextartnode) {
    /* Does this artnode end after the start of the next? */
    if (artnode->last >= nextartnode->first-1) {
      /* Does this artnode end before the next? */
      if (artnode->last < nextartnode->last) {
	artnode->last = nextartnode->last;
      }
      artnode->nextart = nextartnode->nextart;
      free(nextartnode);
    }
  }
}

extern void add_articles(new, first, last)
  struct newsrc_node *new;
  int first;
  int last;
{
  a_node *newarticles = NULL, *art = NULL, *prev = NULL;

  assert( new );

  if (!first)
    return;

  if (!last)
    last = first;
	
  if (first > last && (first != 1 && last != 0)) {
    printf("article order error\n");
    return;
  }
	
  newarticles =(a_node *)malloc(sizeof(a_node));
  assert( newarticles );
	
  newarticles->first = first;
  newarticles->last = last;
  newarticles->nextart = NULL;
	
  if (new->artlist == NULL) {
    new->artlist = newarticles;
    return;
  };
	
  art = new->artlist;
  if (!art->last) {
    free(art);
    new->artlist = newarticles;
    return;
  };

  if (first < art->first) {
    if (last > art->last) {
      art->first = first;
      art->last = last;
      adjacent_nodes(art);
      return;
    }
    else {
      if (last == art->first -1) {
	art->first = first;
	free(newarticles);
      }
      else{
	newarticles->nextart = new->artlist;
	new->artlist = newarticles;
      }
      return;
    }
  }
  
  while (art != NULL) {
    prev = art;
    art = art->nextart;
    
    if (first >= prev->first && first <= prev->last) {
      if (last > prev->last) {
	prev->last = last;
	adjacent_nodes(prev);
      };
      free(newarticles);
      return;
    };
	  
    /* Tack the article number to the back of the existing node */
    if (first == prev->last + 1) {
      prev->last = last;
      adjacent_nodes(prev);
      free(newarticles);
      return;
    };
    
    if (art == NULL)
      break;
	  
    /* Cover case where last article number overlaps */
    if (last >= art->first && last <= art->last) {
      if (first < art->first) {
	art->first = first;
	adjacent_nodes(prev);
      };
      free(newarticles);
      return;
    };
    
    /* Tack the article number to the front, not adjacent */
    if (last == art->first - 1) {
      art->first = first;
      free(newarticles);
      return;
    };
	  
    /* No adjacent node, insert new node after prev */
    if (first < art->first) {
      newarticles->nextart = prev->nextart;
      prev->nextart = newarticles;
      return;
    };
    
  };

  prev->nextart = newarticles;
}

	
#define RNUM 1
#define COMMA 2
#define DASH 3

extern struct newsrc_node *
parse_line(line, newsgroup)
	char	*line, *newsgroup;
{
	char	*c;
	char	*num;
	char	numstr1[10], numstr2[10];
	int	i;
	int	state;
	struct newsrc_node	*new = NULL;
static  struct newsrc_node	*prev = NULL;

	if ((c = strchr(line, '\n')) != NULL)
		*c = '\0';
	for (i=0, c=line; i<BUFFERLEN; i++, c++) 
		if ((*c == ':') || (*c == '!')) 
			break;
	if (i == BUFFERLEN) {
		printf("Bogus newsgroup %s\n", line);
		return NULL;
	}

	new= (struct newsrc_node *)malloc(sizeof(struct newsrc_node));
	assert( new );

	if (!Global->head) {
	  Global->head = new;
	}

	if (prev) {
	  prev->nextgroup = new;
	}
	
	new->newsgroup = newsgroup;
	if (strchr(line, ':') != NULL)
                  new->subscribed = SUBSCRIBED;
        else
                  new->subscribed = UNSUBSCRIBED;
	new->artlist = NULL;
	new->nextgroup = NULL;
	new->description = NULL;
	new->articles = 0;
	new->kill = NULL;
	prev = new;

	c++;
	state = RNUM;
	memset(numstr1, '\0', 10);
	memset(numstr2, '\0', 10);
	num = numstr1;
	while (*c != '\0') {
		switch(*c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			state = RNUM;
			*num++ = *c++;
			break;
		case '-':
			if (state != RNUM) {
				printf("Bogus dash in newsrc for group %s!\n",new->newsgroup);
				state = COMMA;
			} else {
				state = DASH;
				num = numstr2;
			}
			c++;
			break;
		case ',':
			if (state != RNUM) 
				printf("Bogus comma in newsrc for group %s!\n", new->newsgroup);
			else {
			  add_articles(new, atoi(numstr1), atoi(numstr2));
			}
			
			state = COMMA;
			memset(numstr1, '\0', 10);
			memset(numstr2, '\0', 10);
			num = numstr1;
			c++;
			break;
		case ' ':
			c++;
			break;
		default : printf("Bad character in newsrc: %c\n", *c);
			c++;
		}
	}
	if (state == RNUM) {
	  add_articles(new, atoi(numstr1), atoi(numstr2));
	}
	
	return new;
}

extern void clear_artlist(curr)
struct newsrc_node *curr;
{
  a_node *art = NULL, *next = NULL;

  assert( curr );

  art = curr->artlist;
  while (art) {
    next = art->nextart;
    free(art);
    art = next;
  }

  curr->artlist = NULL;
}

extern int unread_articles(curr)
struct newsrc_node *curr;
{
  int unread = 0;
  a_node *art = NULL;

  assert( curr );

  unread = curr->last_article;
  art = curr->artlist;
  
  /* If no article data is available, it is most accurate to
     display the amount of articles available from the server */
  if (art == NULL)
    return curr->articles;
  
  unread = (unread - art->last) + (art->first - 1);
  art = art->nextart;
  
  while (art != NULL) {
    unread -= (art->last - art->first) + 1;
    art = art->nextart;
  }
  
  /* A quick check: if the article range indicates articles which
     are not available anymore, then the unread count can end up
     higher than the number of available articles. Check this,
     and take measure if needed. */
  if (unread > curr->articles){
    unread = curr->articles;
  }

  return unread;
}

/* is_unread returns TRUE or FALSE, depending whether 
   the article is marked as read in the newsgroup's artlist. */
extern int is_unread(newsgroup, artnum)
  struct newsrc_node *newsgroup;
  int artnum;
{
  a_node *art = NULL;
  
  assert( newsgroup );

  if (!(art = newsgroup->artlist)) {
    return TRUE;
  }

  while (art) {
    if ( (art->last == 0 && art->first == artnum) ||
	 (artnum >= art->first && artnum <= art->last))
      return FALSE;
    art = art->nextart;
  }

  return TRUE;
}

extern void getReadArticles(ip, curr)
xvnews_xvnews_window_objects    *ip;
struct newsrc_node *curr;
{
	char	*old;
	int	first = -1, last = -1, row, art;
	int rows = xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);

	for (row = 0; row < rows; row++) {
		old = (char *)xv_get(ip->articles_list,
                                 PANEL_LIST_STRING, row, NULL);
		sscanf(old, "%d", &art);
		/* When writing the first chain make sure there is a correct start */
		if (row == 0) {
		  add_articles(curr, 1, art - 1);
		}

		if (isRead(old)) {
		  if (first == -1) {
		    first = art;
		    last = art;
		    continue;
		  }
		  if (art == last + 1) {
		    last = art;
		  }
		  else {
		    add_articles(curr, first, last);
		    first = art;
		    last = art;
		    continue;
		  }
		}
		
		if (first != -1) {
		    add_articles(curr, first, last);
		    first = -1;
		    last = -1;
		}
	}
	
	if (first != -1) {
	  add_articles(curr, first, last);
	}

}

extern void delete_articles(ip, curr)
xvnews_xvnews_window_objects    *ip;
struct newsrc_node *curr;
{
  char	*old;
  int	rows = xv_get(ip->articles_list, PANEL_LIST_NROWS, NULL);
  int	row, num;
  a_node *art = curr->artlist->nextart;
  
  while (art != NULL) {
    for (row = 0; row < rows; row++) {
      old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
      sscanf(old, "%d", &num);
      if (art->last == 0) {
	if (art->first == num) {
	  xv_set(ip->articles_list, PANEL_LIST_DELETE, row--, NULL);
	  rows--;
	  break;
	}
      } else
	if (num >= art->first && num <= art->last) {
	  xv_set(ip->articles_list, PANEL_LIST_DELETE, row--, NULL);
	  rows--;
	}
    }
    art = art->nextart;
  }
}
	
extern void add_unread(curr, unread)
struct newsrc_node *curr;
int unread;
{
  a_node *art = NULL, *new = NULL, *prev = NULL;

  assert( curr );
  art = curr->artlist;

  if (art == NULL)
    return;

  if (unread == curr->artlist->last) {
    curr->artlist->last--;
    return;
  }
  while(art->nextart != NULL) {
    if (unread <= art->last || unread <= art->first)
      break;
    prev = art;
    art = art->nextart;
  }
  if (unread == art->first && unread != 1) {
    if (!art->last) {
      prev != NULL ? prev->nextart = art->nextart:NULL;
      free(art);
      return;
    }
    art->first++;
    if (art->first == art->last)
      art->last = 0;
    if (art->first > curr->last_article) {
      prev != NULL ? prev->nextart = NULL:NULL;
      free(art);
    }
    return;
  }
  if (unread == curr->last_article) {
    art->last--;
    if (art->last == art->first)
      art->last = 0;
    return;
  }
  new = (a_node *)malloc(sizeof(a_node));
  if (new == NULL) {
    printf("Malloc failed!\n");
    return;
  }
  new->first = unread + 1;
  new->nextart = art->nextart;
  art->nextart = new;
  new->last = new->first == art->last ? 0:art->last;
  if ((unread - 1) == 1)
    art->last = 1;
  else
    art->last = unread - 1 == art->first ? 0:unread - 1;
  if (unread == 1)
    art->first = 0;
}
