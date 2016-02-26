/* FILE date-calc.c ****************************************
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
 * Date arithmetic functions
 * 1. Calculate earliest/latest start dates
 * 2. Calculate earliest/latest end dates
 *
 ************************************************************/   
#include <stdio.h>
#include <limits.h>
#include <xview/notice.h>
#include "db.h"
#include "xplan.h"


/* FUNCTION init_dates *********************************

   PURPOSE

   Initialize the earliest/latest start/end dates before
   doing any processing.

   SAMPLE CALL

   init_dates(dates);

   INPUT

   dates -- 0 for earliest start/end dates
            1 for latest start/end dates
	    2 for critical dates

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 5 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 5 Dec 1992

   MODIFICATIONS (most recent to least)
****************************************************************/

init_dates(int dates) {
 
  struct task_list   /* temporary pointers */
    *temp_task_list,
    *temp;
  struct task_node 
    *temp_task_node, 
    *node;

  temp_task_list = get_main_task_list();  
  temp_task_node = node = temp_task_list->head;

  switch (dates) {    /* which data are to be initialized ? */
  case 0:    /* earliest start/end dates */
    for(; node != NULL; node = node->next) 
      /* set the dates to minimum values */
      node->data->earliest_start_date = node->data->earliest_end_date = 0;
    break;
  case 1:    /* latest start/end dates */
    for(; node != NULL; node = node->next) 
      /* set the dates to maximum values */
      node->data->latest_start_date = node->data->latest_end_date = INT_MAX;
    break;
  case 2:   /* critical dates */
    for(; node != NULL; node = node->next) {
      /* set the dates to maximum values */
      node->data->critical_date = INT_MAX;
      /* assume that this task does not belong to the critical path */
      node->data->critical_path = false; 
    }
    break;
  default:   /* error -- this line should never be executed! */
    /*---- FOR FUTURE EXTENSION ----*/
  }
}  /* end init_dates() */
   

static unsigned
  curr_early_start;  /* current value of the earliest start date */

/* FUNCTION calc_early_date ******************************

   PURPOSE

   Calculate the earliest start/end dates for the first task, then
   call visit_early_date() to calculate the dates for the rest of the
   task list.

   SAMPLE CALL

   calc_early_date();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 2 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 10 Dec 1992

   MODIFICATIONS (most recent to least)

   11 Dec 1992 (mml)
   Add the display window for error messages.

   10 Dec 1992 (rcm)
   Fill in the planned/end dates automatically.

   5 Dec 1992 (rcm)
   Change the function name to calc_early_date() to reflect the
   functionality performed by this function (now it'll calculate
   both earliest start & end dates).
****************************************************************/

calc_early_date() {
  
  struct task_list  /* temporary pointers */
    *temp_task_list,
    *temp;
  struct task_node 
    *temp_task_node,
    *child;
  unsigned prev_early_start;
  Xv_notice notice;

  curr_early_start = prev_early_start = 0;   /* initialize */
  init_dates(0);

  temp_task_list = get_main_task_list();
  if (temp_task_list != NULL) {    /* process only if the task list exist */
    temp_task_node = temp_task_list->head;
    if (temp_task_node != NULL) {
       
       /* make sure that planned start date for the first task is filled in */
       if (temp_task_node->data->planned_start_date == 0) {
	  notice = xv_create(Main_mainWindow->mainWindow, NOTICE,
			     NOTICE_MESSAGE_STRINGS, 
			     "The planned start date for "
			     "the first task must be filled "
			     "in", 
			     NULL,
			     NOTICE_BUTTON_YES, "Continue",
			     XV_SHOW, TRUE,
			     NULL);
	  xv_destroy_safe(notice);
	  return;
       }
       /* store initial values for earliest start/end dates */
       temp_task_node->data->earliest_start_date = curr_early_start =
	 prev_early_start = temp_task_node->data->planned_start_date;
       temp_task_node->data->earliest_end_date = curr_early_start +
	 temp_task_node->data->duration;
       
       /* update the next earliest start dates */
       curr_early_start += temp_task_node->data->duration;

       /* for each dependent ... */
       temp = temp_task_node->data->dependents;
       if (temp != NULL) 
	 for(child = temp->head; child != NULL; child = child->next)
	   visit_early_date(child);  /* process the child tasks/nodes */
    }
    /* ERROR : TASK LIST IS EMPTY */
  }   
  /* ERROR : TASK LIST IS NOT EXIST */

}  /* end calc_early_date() */


/* FUNCTION visit_early_date ******************************

   PURPOSE

   Recursive function that will calculate the earliest start/end
   dates for each of the task in the task list. 

   SAMPLE CALL

   visit_early_date(pointer);

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 30 Nov 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 9 Dec 1992

   MODIFICATIONS (most recent to least)
   
   5 Dec 1992
   Change the function from visit_early_start_date().
****************************************************************/

visit_early_date(struct task_node *pointer) {

  struct task_list *temp;
  struct task_node *child;
  int i;
  unsigned prev_early_start;

  /*--------------------------------------------------------------------- 
    Calculate a new earliest start date only if the earliest start date
    value stored in the database is less than the one calculated from
    this path.
  ----------------------------------------------------------------------*/
  if (curr_early_start > pointer->data->earliest_start_date) { 
    /*-----------------------------------------------------------------------
      The curr_early_start variable will contain the earliest start dates
      for child nodes of a given parent node. Suppose that the earliest start 
      date of a node is X, and the duration is D. Then, the earliest start 
      date of any of its children would be X+D.
    ------------------------------------------------------------------------*/ 
    pointer->data->earliest_start_date = prev_early_start = curr_early_start;
    pointer->data->earliest_end_date = pointer->data->earliest_start_date +
      pointer->data->duration;

    /* fill the planned start/end dates (unconditionally) - 10 Dec 1992(rcm) */
    pointer->data->planned_start_date = pointer->data->earliest_start_date;
    pointer->data->planned_end_date = pointer->data->earliest_end_date;

    /* update the earliest start date */ 
    curr_early_start += pointer->data->duration;
    temp = pointer->data->dependents;
    if (temp != NULL)   /* check whether there is any dependents */
      /* for each dependents ... */
      for(child = temp->head; child != NULL; child = child->next) 
	visit_early_date(child);
  }
  /*------------------------------------------------------------------------- 
    Before exiting the function, set curr_early_start to its previous value,
    meaning that curr_early_start now contains the earliest start date of
    the parent node.
  ---------------------------------------------------------------------------*/
  curr_early_start = prev_early_start;

}  /* end visit_early_date() */


static int
  curr_late_end;  /* current value of the latest end date */

/* FUNCTION calc_late_date ******************************

   PURPOSE

   Calculate the latest start/end dates for the first task, then
   call visit_late_date() to calculate the rest of the
   task list.

   SAMPLE CALL

   calc_late_date();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 5 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 9 Dec 1992

   MODIFICATIONS (most recent to least)

****************************************************************/

calc_late_date() {
  
  struct task_list   /* temporary pointers */
    *temp_task_list,
    *temp;
  struct task_node 
    *temp_task_node,
    *parent;
  unsigned prev_late_end;

  curr_late_end = prev_late_end = 0;   /* initialize */
  init_dates(1);

  temp_task_list = get_main_task_list();
  if (temp_task_list != NULL) {  /* process only if the task list exist */
    temp_task_node = temp_task_list->tail;  /* start from the last task */
    if (temp_task_node != NULL) {

      /* make sure that planned end date for the last task is filled in */
      if (temp_task_node->data->planned_end_date == 0) {
	
	/* ERROR -- planned end date for last task is not filled in! */
	/* An error window should probably be displayed here */
	for(temp_task_node = temp_task_list->head; temp_task_node != NULL;
	    temp_task_node = temp_task_node->next) /* clear the latest dates */
	  temp_task_node->data->latest_start_date = 
	    temp_task_node->data->latest_end_date = 0;
	return;
      }
      /* store initial values for latest start/end dates */
      temp_task_node->data->latest_end_date = curr_late_end =
	prev_late_end = temp_task_node->data->planned_end_date;
      temp_task_node->data->latest_start_date = curr_late_end - 
	temp_task_node->data->duration;

      curr_late_end -= temp_task_node->data->duration; /* updating ... */
      temp = temp_task_node->data->dependencies;
      if (temp != NULL)  /* for every parent node ... */
	for(parent = temp->head; parent != NULL; parent = parent->next)
	  visit_late_date(parent);    /* process each parent node/task */
    }
    /* ERROR : TASK LIST IS EMPTY */
  }   
  /* ERROR : TASK LIST IS NOT EXIST */

}  /* end calc_late_date() */
 

/* FUNCTION visit_late_date ******************************

   PURPOSE

   Recursive function that will calculate the latest start/end
   dates for each of the task in the task list. 

   SAMPLE CALL

   visit_late_date(pointer);

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 5 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 9 Dec 1992

   MODIFICATIONS (most recent to least)
****************************************************************/

visit_late_date(struct task_node *pointer) {

  struct task_list *temp;
  struct task_node *parent;
  int i;
  unsigned prev_late_end;

  /*--------------------------------------------------------------------- 
    Calculate a new latest end date only if the latest end date
    value stored in the database is greater than the one calculated from
    this path.
  ----------------------------------------------------------------------*/
  if (curr_late_end < pointer->data->latest_end_date) { 
    /*--------------------------------------------------------------------
      The curr_late_end variable will contain the latest end dates
      for parent nodes of a given child node. Suppose that the latest end 
      date of a node is X, and the duration is D. Then, the latest end 
      date of any of its parents would be X-D.
    ----------------------------------------------------------------------*/ 
    pointer->data->latest_end_date = prev_late_end = curr_late_end;
    pointer->data->latest_start_date = pointer->data->latest_end_date -
      pointer->data->duration;
    /* update the latest end date */ 
    curr_late_end -= pointer->data->duration;
    temp = pointer->data->dependencies;
    if (temp != NULL)   /* check whether there is any dependencies */
      /* for each parent node ... */
      for(parent = temp->head; parent != NULL; parent = parent->next) 
	visit_late_date(parent);
  }
  /*-----------------------------------------------------------------------
    Before exiting the function, set curr_late_end to its previous value,
    meaning that curr_late_end now contains the latest end date of
    the parent node.
  -------------------------------------------------------------------------*/
  curr_late_end = prev_late_end;

}  /* end visit_late_date() */


/* FUNCTION calc_float ******************************

   PURPOSE

   Calculate the float time for each task.

   SAMPLE CALL

   calc_float();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 5 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 7 Dec 1992

   MODIFICATIONS (most recent to least)
   
   8 Dec 1992 (rcm)
   Subtract the duration from the float time.

****************************************************************/

calc_float() {

  struct task_list *pointer;  /* temporary pointers */
  struct task_node *temp;

  pointer = get_main_task_list();
  if (pointer == NULL) {
    /* ERROR : MAIN TASK LIST IS NOT EXIST */
    return;
  }
  /* For each task in the database ... */
  for(temp = pointer->head; temp != NULL; temp = temp->next)
    temp->data->float_time = temp->data->latest_end_date - 
      temp->data->earliest_start_date - temp->data->duration;
  
}   /* end calc_float */


/* FUNCTION calc_dates ******************************

   PURPOSE

   Invokes the neccessary functions to calculate:
   1. Earliest/latest start/end dates.
   2. Float time.

   SAMPLE CALL

   calc_dates();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 5 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 5 Dec 1992 

   MODIFICATIONS (most recent to least)
****************************************************************/

calc_dates() {

  calc_early_date();  /* earliest start/end dates */
  calc_late_date();   /* latest start/end dates */
  calc_float();       /* float time */

}   /* end calc_dates() */
