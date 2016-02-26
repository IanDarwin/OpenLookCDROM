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
 *  General calculation functions
 *  1. Check for dependency loop.
 *  2. Calculate the relative position of each PERT box.
 *
 ************************************************************/

#include <stdio.h>
#include "db.h"
#include "calc.h"

int 
  x_coord,             /* current screen coordinate where the next */
  y_coord;             /* PERT box will be placed.                 */

enum boolean loop_found;               /* true if a loop is found. */
struct task_node *loop_pointer; /* points to the task that caused  */
                                /* the dependency loop. */

/* FUNCTION traverse_task_list **************************************

   PURPOSE

   Get the pointer to the beginning of the task list and pass the
   beginning of the task list to the recursive function visit_task().

   SAMPLE CALL

   traverse_task_list();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 2 Nov 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 2 Dec 1992 

   MODIFICATIONS (most recent to least)

   9 Dec 1992  (rcm)
   Allow the program to use 2 different algorithm for calculation of
   PERT's y-coordinate. OLD_PERT is used to toggle between the 2
   algorithm.

   6 Dec 1992  (rcm)
   Fixed bug so that the y-coordinate can only be updated once

   2 Dec 1992  (rcm)
   Set the marker to zero before calling visit_task()

********************************************************************/

traverse_task_list() {

  struct task_list *pointer;   /* pointer to the beginning of task list */
  struct task_node *temp;       

  pointer = get_main_task_list();   
  if ((pointer == NULL) || (pointer->head == NULL)) {
    /* main task list does not exist! */
    return;
  }
  loop_found = false;   /* Assume that there is no loop in the task list */

  x_coord = y_coord = 0;       /* initialize */

  /* for every task in the task list ... */
  for(temp = pointer->head; temp != NULL; temp = temp->next) { 
    temp->data->y_pert = temp->data->x_pert = 0; /* initialize  coordinates */
  }
  init_marker(pointer->head);  /* initialize the marker to zero */
  visit_task(pointer->head);   /* calc the x coordinate */
  init_marker(pointer->head);  /* Initialize the marker back to zero */

  /*------------------------------------------------------------------ 
    If the programmer prefer the 2nd algorithm (breadth first search),
    set OLD_PERT to 0 in calc.h
    ------------------------------------------------------------------*/
  if (!OLD_PERT) calc_y_coord();  /* use breadth-first search */

}  /* end of traverse_task_list() */


/* FUNCTION visit_task *************************************************

   PURPOSE

   1. Check for dependency loop
   2. Calculate the relative position of each PERT box.
        Note that this routine will only calculate the grid position of
	each box. In order to get the real screen/LaTex position, the width
	and length of each box must be taken into account.

   SAMPLE CALL

   visit_task(pointer);

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
    
   Author.... Richard C. Malingkas (rcm), 2 Nov 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 2 Dec 1992 

   MODIFICATIONS (most recent to least)
   
   2 Dec 1992 (rcm)
   Check whether the task list exist before doing any processing

   21 Nov 1992 (rcm)
   Add dependency loop checking capability

   19 Nov 1992 (rcm)
   Remove the nested if-statements
   Comment out the dependency loop checking functionality
   
****************************************************************/

visit_task(struct task_node *pointer) {

  struct task_node *temp;
  int i;

  if (loop_found == true)  /* Don't process if a loop has been found */
    return;
  
  /* ... to denote that this node/task has been visited */
  pointer->data->number_of_visits++; 

  /*------------------------------------------------------------------------- 
    A node cannot exist twice in a given path. Therefore, if this node has 
    been visited before, that means there is a loop in the task list.
  -------------------------------------------------------------------------*/
  if (pointer->data->number_of_visits > 1) {  /* ERROR : a loop is found ! */
    loop_found = true;
    /* loop_pointer will be used by GUI to show where the loop is found */
    loop_pointer = pointer;  
    return;
  }
  x_coord++;  /* this task must appear to the right of its parent */

  /* If the programmer wants to use the 2nd algorithm, skip this part. */
  if (OLD_PERT) { 
    /* so y_pert is modified only once .... */
    if ((pointer->data->y_pert == 0) && (pointer->data->x_pert == 0)) 
      pointer->data->y_pert = y_coord;
  }
  /*-------------------------------------------------------------------------
    If this path produce an x-coordinate which is greater than the coordinate
    stored in the database, assign a new x-coordinate to this path.
   -------------------------------------------------------------------------*/
  if (x_coord > pointer->data->x_pert) {
    pointer->data->x_pert = x_coord;
  }
  else {         /* If this path is shorter, do not update the x-coordinate */
    x_coord--;   /* Move back to the parent node's coordinate */
    pointer->data->number_of_visits--;  /* finish visiting this task ... */
    return;
  }
  if (pointer->data->dependents != NULL) { /* process only if the list exist */
    temp = pointer->data->dependents->head;     /* temp = NULL for leaf node */
    for (i=0; temp != NULL ;temp = temp->next, i++){ /* for each dependent.. */
      visit_task(temp);                    /* proccess each of the dependent */
      if (loop_found == true)      /* Don't process if a loop has been found */
	return;
      y_coord++;                       /* so the next path is one level down */
    }
  }

  if (i)         /* If this node has no dependent (leaf node) ... */
    y_coord--;   /* Move back up one level; to cancel out y_coord++ above */  

  x_coord--;             /* move back to the parent node's coordinate */
  pointer->data->number_of_visits--;

} /* end visit_task() */


/* FUNCTION init_marker ***********************************

   PURPOSE

   Initialize the number_of_visits to zero.

   SAMPLE CALL

   init_marker(pointer)

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 19 Nov 1992
   Auditor... 
   Tester.... Mark M. Lacey (mml), 2 Dec 1992

   MODIFICATIONS (most recent to least)

**********************************************************/

init_marker(struct task_node *pointer) {
  
  while (pointer) {               /* while not end of task list (NULL)... */
    pointer->data->number_of_visits = 0;
    pointer = pointer->next;      /* go to the next task */
  }
} /* end of init_marker() */

