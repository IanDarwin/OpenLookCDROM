/* FILE pagelist.c ****************************************
 *
 * xplan - project planning tool
 * Copyright (C) 1992 Brian Gaubert, Mark M. Lacey, Richard Malingkas,
 * and Mike Marlow.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License (distributed with this program in the file
 * COPYING) for more details.
 * 
 * If you did not received a copy of the GNU General Public License
 * along with this program, write to the Free Software Foundation,
 * Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Since this was a project for a one semester software engineering
 * course, the authors will not be offering support for the product
 * after its release.
 *
 * DESCRIPTION OF CONTENTS
 *  
 * Data structure creation and manipulation for LaTeX PERT chart
 * generator.
 *
 */   

#include <stdio.h>
#include "pagelist.h"

/* FUNCTION create_page_list ****************************************

   PURPOSE

   Creates a page list.  A page list is a list of page information.
   Page information is a list of objects that go on a page.

   SAMPLE CALL

   newlist = create_page_list(across, down);

   INPUTS

   across --- number of pages across

   down --- number of pages down

   OUTPUTS

   newlist --- A pointer to the list that was created.  This will
   be set up to include the information given in the call.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
struct page_list *create_page_list(int pages_across, int pages_down)
{
   int indx, total_pages;
   struct page_list *newlist;

   total_pages = pages_across * pages_down;

   newlist = (struct page_list *) malloc(sizeof(struct page_list));

   newlist->pages = (struct object_node **) 
     malloc(total_pages * sizeof(struct object_node *));
   
   newlist->pages_across = pages_across;
   newlist->pages_down = pages_down;

   /* initialize all the pointers to NULL */
   for (indx = 0; indx < total_pages; ++indx) {
      newlist->pages[indx] = NULL;
   }

   return newlist;
}

/* FUNCTION get_page_from_list ****************************************

   PURPOSE

   Get a page's worth of information from the list of pages.

   SAMPLE CALL

   objlist = get_page_from_list(list, across, down);

   INPUTS

   list --- the page list

   across --- page across that is wanted

   down --- page down that is wanted

   OUTPUTS

   objlist --- the list of objects on that page

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
struct object_node *get_page_from_list(struct page_list *list,
				       int across, int down)
{
   return list->pages[list->pages_across * (down-1) + across-1];
}

/* FUNCTION add_object_to_page ****************************************

   PURPOSE

   Add an object to a page.

   SAMPLE CALL

   add_object_to_page(list, across, down, object);

   INPUTS

   list --- list to be added to

   across --- page across to be added to

   down --- page down to be added to

   object --- object to be added to list

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
void add_object_to_page(struct page_list *list, int across, int down,
			struct object_node *object)
{
   /* add object at head */
   
   object->next = list->pages[list->pages_across * (down - 1) +
				 across-1];
   list->pages[list->pages_across * (down - 1) + across-1] =
     object;
}

/* FUNCTION create_object node ****************************************

   PURPOSE

   Creates object node given correct information

   SAMPLE CALL

   newobj = create_object_node(objtype, x, y, dx, dy, contfrom,
   conton, critical, name, next);

   INPUTS

   objtype --- is it a box? line? bar?

   x --- x position

   y --- y position

   dx --- for lines, the difference in beg/end x

   dy --- for lines, the difference in beg/end y

   contfrom --- is this a Gantt bar continued from a previous page?

   contto --- is this a Gantt bar continued on the next page?

   name --- name of the task

   next --- next object in this list
   
   OUTPUTS

   newobj --- the newly created object

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Tester.... Mark M. Lacey, 7 Nov 1992

   MODIFICATIONS (most recent to least)

*/
struct object_node *create_object_node(int object_type, int x, int y,
				       int length,
				       int dx, int dy,
				       int continued_from_previous,
				       int continued_on_next,
				       int critical,
				       char *name,
				       struct object_node *next)
{
   struct object_node *newobject;

   newobject = (struct object_node *) malloc(sizeof(struct
						    object_node));
   newobject->object_type = object_type;
   newobject->x = x;
   newobject->y = y;
   newobject->name = name;
   newobject->length = length;
   newobject->dx = dx;
   newobject->dy = dy;
   newobject->continued_from_previous = continued_from_previous;
   newobject->continued_on_next = continued_on_next;
   newobject->next = next;

   return newobject;
}
