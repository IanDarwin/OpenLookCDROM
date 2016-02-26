/* FILE calc.c *********************************************
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
 *  DESCRIPTION OF CONTENTS
 *
 *  Breadth first search algorithm for calculating the y-coordinate
 *  of each box in the PERT chart.
 *
 ************************************************************/

#include <stdio.h>
#include "db.h"
#include "calc.h"

static struct task_node **queue;  /* array of pointer to task list */
static int 
  head,         /* head of the queue */
  tail;         /* tail of the queue */
static int database_size;   /* number of tasks in the database */


/* FUNCTION init_queue **************************************

   PURPOSE

   Initialize the queue that is going to be used by the
   breadth first search algorithm.

   SAMPLE CALL

   init_queue();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 10 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 10 Dec 1992 

   MODIFICATIONS (most recent to least)

 ********************************************************************/

init_queue() {

  struct task_list *t1;  /* pointer to the main task list */
  
  t1 = get_main_task_list();
  /*--------------------------------------------------------------
    If the task list exist, allocate space for the queue array.
    -------------------------------------------------------------*/
  if (t1 != NULL) {  
    queue = (struct task_node **)calloc(t1->size, sizeof(struct task_list *));
    database_size = t1->size;
  }
  head = tail = 0;

}  /* end of init_queue() */


/* FUNCTION insert_queue **************************************

   PURPOSE

   Act as a main routine for traversing the task list.
   Insert the 1st task along with its child tasks into the queue,
   and process the recursively by calling visit_queue().

   SAMPLE CALL

   insert_queue();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 10 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 10 Dec 1992 

   MODIFICATIONS (most recent to least)

 ********************************************************************/

insert_queue() {

  struct task_list *t1;  /* temporary pointers */
  struct task_node *t2;

  init_queue();        /* initialize the queue */

  t1 = get_main_task_list();
  if (t1 != NULL) {        /* process only if the main task list exist */
    init_marker(t1->head);
    t2 = t1->head;
    if (t2) queue[0] = t2;         /* insert the 1st task to the queue */
    else return;
    
    /* for each dependent of the 1st task ... */
    t1 = t2->data->dependents;
    for(t2 = t1->head; t2 != NULL; t2 = t2->next) 
      if (t2->data->number_of_visits == 0) {
	queue[++head] = t2;             /* insert task into the queue */
	t2->data->number_of_visits++;
      }
    visit_queue(queue[++tail]);    /* visit each of task in the queue */
  }
}  /* end of insert_queue() */ 


/* FUNCTION visit_queue **************************************

   PURPOSE

   For each task in the queue, generate all the child tasks, and
   insert those child tasks into te queue, until there is no more
   task left (end of task list is reached).

   SAMPLE CALL

   visit_queue(pointer);

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 10 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 10 Dec 1992 

   MODIFICATIONS (most recent to least)

 ********************************************************************/

visit_queue(struct task_node *pointer) {

  struct task_list *t1;  /* temporary pointers */
  struct task_node *t2;

  if (pointer == NULL) return;         /* reached end of task list ? */
  
  /* if all the tasks in the queue have been processed .. */
  if (head >= database_size - 1) return;  
  
  /* for each dependents of current task ... */
  t1 = pointer->data->dependents;
  if(t1 != NULL) {
    for(t2 = t1->head; t2 != NULL; t2 = t2->next)
      if (t2->data->number_of_visits == 0) {
	queue[++head] = t2;    /* insert the child tasks into the queue */
	t2->data->number_of_visits++;
      }
    visit_queue(queue[++tail]); /* process next task in the queue */
  }
}  /* end of visit_queue() */


/* FUNCTION calc_y_coord **************************************

   PURPOSE

   Calculate the y-coordinate of each PERT box.
   Boxes with the same x-coordinate will be aligned sequentially
   along the y-coordinate.

   SAMPLE CALL

   calc_y_coord();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 10 Dec 1992
   Auditor... 
   Tester.... Mark M. Lacey (mml), 10 Dec 1992 

   MODIFICATIONS (most recent to least)

 ********************************************************************/

calc_y_coord() {
  
  int i, 
  x=1,    /* start with x-coord = 1 */
  y=-1;   /* so the first y is 0 */
  struct task_list *t1;   /* temporary pointers */
  struct task_node *t2;
  
  insert_queue();   /* insert tasks into the queue */
  
  for(i=0; i < database_size; i++) {   /* for each task ... */
    /* fix so that it will only continue if queue[i] != NULL) */
    /* Mark 12-10 9:20 */
    if (queue[i] != NULL) {
      /*----------------------------------------------------------- 
	if this task has the same x-coord with the prev one,
	move it down one y-coord, else (meaning different x-coord),
	move it up to y=0 coordinate, and update x-coordinate.
	-----------------------------------------------------*/
      if(x == queue[i]->data->x_pert) 
	y++;
      else {
	y = 0;
	x = queue[i]->data->x_pert;
      }
      queue[i]->data->y_pert = y;   /* store the value in the database */
    }
  }
}  /* end of calc_y_coord() */
