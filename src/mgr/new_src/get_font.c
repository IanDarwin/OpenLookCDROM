/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/get_font.c,v 1.3 91/03/01 11:05:48 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/get_font.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/get_font.c,v $$Revision: 1.3 $";

/* LRU routines for font management */

#include <stdio.h>
#include "bitmap.h"
#include "font.h"

#define MAX_FONT		20
#ifndef NULL
#define NULL		((char *) 0)
#endif
#define NULL_LIST	((struct list *) 0)

static struct list {
   struct list *next, *prev;		/* next or previous list item */
   struct font *font;			/* pointer to font structure */
   char *name;				/* font name */
   };

struct list *list_top = NULL_LIST;

/* insert an element at the top of the list */

insert(ptr)
register struct list *ptr;
   {
   ptr->next = list_top;
   ptr->prev = NULL_LIST;
   if (ptr->next != NULL_LIST)
      ptr->next->prev = ptr;
   list_top = ptr;
   }

/* unlink an element from the list */

unlink(ptr)
register struct list *ptr;
   {
   if (ptr->next != NULL_LIST)
      ptr->next->prev = ptr->prev;
   if (ptr->prev != NULL_LIST)
      ptr->prev->next = ptr->next;
   }

/* get font, name for an element */

create(name,ptr)
char *name;
register struct list *ptr;
   {
   char *save_line();

   if (ptr->name != NULL)
      free(ptr->name);
   if (ptr->font != (struct font *) 0) 
      free_font(ptr->font);
   ptr->name = save_line(name);
   ptr->font = open_font(name);
   }

/* manage the list of fonts (using LRU) */

struct font *
get_font(name)
char *name;
   {
   static count = 0;
   register struct list *ptr;
   register int found=0;
   char *alloc();
   
   if (name == (char *) 0 || *name == '\0')
      return((struct font *) 0);

   if (count>0 && strcmp(name,list_top->name)==0) {
      return(list_top->font);
      }
   
   for(ptr=list_top;count>0;ptr=ptr->next) {
      if (found=(ptr->name && strcmp(name,ptr->name)==0)) {
         unlink(ptr);
         insert(ptr);
         break;
         }
      if (ptr->next == NULL_LIST)
         break;
      }
   if (!found && count<MAX_FONT) {
      ptr = (struct list *) alloc(sizeof(struct list));
      ptr->name = NULL;
      ptr->font = (struct font *) NULL;
      create(name,ptr);
      insert(ptr);
      count++; 
      }
   else if (!found) {
      unlink(ptr);
      create(name,ptr);
      insert(ptr);
      }
   return(list_top->font);
   }

/* misc stuff */

char *
alloc(size)
int size;
   {
   char *malloc();
   char *temp;

   if ((temp=malloc(size)) == NULL)
      perror("malloc failed - called from alloc()\n");
   return(temp);
   }

char *
save_line(s)
char *s;
   {
   char *strcpy(), *alloc();

   if (s==NULL) return(s);
   return(strcpy(alloc(strlen(s)+1),s));
   }
