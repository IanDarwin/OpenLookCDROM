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

#include <ctype.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "xvnews_ui.h"
#include "xvnews.h"

STATIC_FUNCTION( int compareFieldArtnum, (void *, void *));
STATIC_FUNCTION( int compareFieldSubject, (void *, void *));
STATIC_FUNCTION( void createSortList, ());
STATIC_FUNCTION( void loadList, ());     
STATIC_FUNCTION( char *getSubject, ());

/*
 * sortSubjects sorts the subjects according to flg
 */

extern void sortSubjects(ip, flg)
     xvnews_xvnews_window_objects *ip;
     int flg;
{
   static char **list;
   static s_node **sort_list;
   static l_max = 0;
   char	*old = NULL;
   int	i=0, nrows = xv_get(ip->articles_list, PANEL_LIST_NROWS), nrow = nrows;

   assert( flg == XV_SORT_ARTNUM || flg == XV_SORT_SUBJECT );

   /* Return of only one article, no need to sort */
   if (nrows == 1)
      return;

   /* If there are less or equal (because we need one field more)
      entries allocated (l_max) than rows we need (nrows), then
      allocate additonal (or new) memory */
   if (l_max <= nrows) 
   {
      if (!l_max) 
      {
	 list = (char **)malloc((nrows + 1) * sizeof(char *));
	 sort_list = (s_node **)malloc((nrows + 1) * sizeof(s_node *));
      }
      else 
      {
	 list = (char **)realloc(list, (nrows + 1) * sizeof(char *));
	 sort_list = (s_node **)realloc(sort_list, (nrows + 1) *
					sizeof(s_node *));
      }
      assert( list && sort_list );
      memset(&(list[l_max]), '\0', (nrows - l_max + 1) * sizeof(char *));
      memset(&(sort_list[l_max]), '\0', (nrows - l_max + 1) * sizeof(s_node *));
      l_max = nrows + 1;
   }
   
   /* Add all rows to the sortlist */
   while (nrow) {
      old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, --nrow);
      createSortList(sort_list, old, flg, i++);
   }	
   /* Make sure the last + 1 item is cleared */
   if (sort_list[i]) 
   {
      free(sort_list[i]->string);
      free(sort_list[i]);
   }
   sort_list[i] = NULL;

   switch(flg) {
   case XV_SORT_ARTNUM:
     qsort(sort_list, nrows, sizeof(s_node *), compareFieldArtnum);
     break;
   case XV_SORT_SUBJECT:
     qsort(sort_list, nrows, sizeof(s_node *), compareFieldSubject);
     break;
   }
   
   loadList(list, sort_list);

/*
   if (flg == 2) 
      xv_set(ip->articles_list, XV_SHOW, FALSE, NULL);
      */

   xv_set(ip->articles_list, PANEL_LIST_DELETE_ROWS, 0, nrows, NULL);
   xv_set(ip->articles_list, PANEL_LIST_INSERT_STRINGS, 0, list, NULL);

}

static void loadList(list, sort_list)
     char *list[];
     s_node *sort_list[];
{
   int i = 0;

   while (sort_list[i] != NULL) {
      list[i] = (sort_list[i])->string;
      i++;
   }
   
   if (sort_list[i]) 
   {
      free(sort_list[i]->string);
      free(sort_list[i]);
   }
   list[i] = NULL;
}

static void createSortList(sort_list, old, flg, i)
     s_node *sort_list[];
     char *old;
     int flg, i;
{
   if (!sort_list[i]) 
   {
      sort_list[i] = (s_node *)malloc(sizeof(s_node));
      sort_list[i]->string =
	(char *)malloc(SUBJECT_LENGTH + AUTHOR_LENGTH + 20);
   }
   strcpy(sort_list[i]->string, old);
   sort_list[i]->len = SUBJECT_LENGTH;

   switch(flg) {
   case XV_SORT_ARTNUM:
     sort_list[i]->subject = sort_list[i]->string;
     break;
   case XV_SORT_SUBJECT:
     sort_list[i]->subject = 
     getSubject(sort_list[i]->string, &(sort_list[i]->len));
     break;
   }
}

static char *
getSubject(string, i)
     char *string;
     int *i;
{
   char *c = string;

   while (isdigit(*c)) {
     c++, (*i)--;
   }
   
   while (*c == ' ') {
     c++, (*i)--;
   }
   
   if (!strncasecmp(c, "re:", 3)) {
      c += 3, (*i) -= 3;
      while (!isalpha(*c) && *i)
	 c++, (*i)--;
   }
   
   return c;
}

/*
 *  Compare Subjects for subject-sorted order. If the subjects
 *  are identical, compare again for article numver.
 *  TODO: should eally be done using References header...
 */

static int compareFieldSubject(a, b)
     void *a, *b;
{
  int result;
  s_node *aa = *((s_node **)a);
  s_node *bb = *((s_node **)b);

  result = strncasecmp(aa->subject, bb->subject,
		       aa->len < bb->len ? aa->len:bb->len);

  if (result == 0) {
    result = compareFieldArtnum(a, b);
  }
  
  return result;
}

static int compareFieldArtnum(a, b)
  void *a, *b;
{
  s_node *aa = *((s_node **)a);
  s_node *bb = *((s_node **)b);
  int num_a, num_b;
  int res_a, res_b;

  res_a = sscanf(aa->string, "%d", &num_a);
  res_b = sscanf(bb->string, "%d", &num_b);

  /* These sscanf's have to give proper results */
  assert(res_a && res_b);
  
  return (num_a-num_b);
}

