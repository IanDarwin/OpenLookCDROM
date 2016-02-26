/* FILE path.c *********************************************

   DESCRIPTION OF CONTENTS

   Critical path function

************************************************************/

#include <stdio.h>
#include "db.h"

/*--------------------------------------------------------------- 
  the use this variable is the same as the use of curr_late_date
  in date-calc.c 
  --------------------------------------------------------------*/
static unsigned
  curr_critical_date; 


/* FUNCTION print_critical ***********************************

   PURPOSE

   Print all tasks that belong to the critical path.

   SAMPLE CALL

   print_critical();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 8 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 8 Dec 1992

   MODIFICATIONS (most recent to least)

**********************************************************/

print_critical() {

  struct task_list *t1;  /* temporary pointers */
  struct task_node *t2;

  t1 = get_main_task_list();
  if (t1 != NULL) {   /* process only if the task list exist */
    printf("Printing critical path .....\n");
    for(t2 = t1->head; t2 !=NULL; t2 = t2->next) {
      /* if this task belongs to the critical path, print it to the screen */
      if (t2->data->critical_path == true) 
	printf("critical task : %s\n", t2->data->name);
    }
    printf("done! \n");
  }
}  /* end of print_critical() */
    
  
/* FUNCTION calc_critical ***********************************

   PURPOSE

   Mark all nodes/tasks that belong to the critical path

   SAMPLE CALL

   calc_critical();

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 8 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 8 Dec 1992 

   MODIFICATIONS (most recent to least)

**********************************************************/

calc_critical() {
  
  struct task_list       /* temporary pointers */
    *temp_task_list,
    *temp;
  struct task_node 
    *temp_task_node,
    *parent;
  unsigned prev_critical_date;

  curr_critical_date = prev_critical_date = 0;   /* initialize */
  init_dates(2);
  
  calc_early_date(); /* calculate the earliest start date for the last task */
  
  temp_task_list = get_main_task_list();
  if (temp_task_list != NULL) {  /* process only if the task list exist */
    temp_task_node = temp_task_list->tail;  /* start from the last task */
    if (temp_task_node != NULL) {

      /* store initial values for critical date of the last task */
      temp_task_node->data->critical_date = curr_critical_date = 
	prev_critical_date = temp_task_node->data->earliest_end_date;
      
      curr_critical_date -= temp_task_node->data->duration; /* updating ... */
      temp = temp_task_node->data->dependencies;
      if (temp != NULL)  /* for every parent node ... */
	for(parent = temp->head; parent != NULL; parent = parent->next)
	  visit_critical(parent);   /* process each parent task/node */
    }
    else {
      printf("Task node is not exist\n") ;
      return;
    }
  }
  else {
    printf("Task list is not exist\n");
    return;
  }
  /*-------------------------------------------------------------------
    Definition : Critical path is the longest path in the task list.
    A task belongs to a critical path if its earliest end date coincides
    with the critical date. 
    -------------------------------------------------------------------*/ 
  for(parent = temp_task_list->head; parent !=NULL; parent = parent->next) {
    if(parent->data->critical_date == parent->data->earliest_end_date)
      parent->data->critical_path = true;
  }
}  /* end calc_critical() */


/* FUNCTION visit_critical ***********************************

   PURPOSE

   Recursive function that will traverse the task list
   and calculate the latest start dates, with the following
   condition applied to the last task:
   critical_date = latest_start_date, 

   SAMPLE CALL

   visit_critical(pointer);

   INPUTS

   pointer --- A pointer to a task node

   AUTHOR/AUDITOR/TESTER
   
   Author.... Richard C. Malingkas (rcm), 8 Dec 1992
   Auditor... 
   Tester.... Richard C. Malingkas (rcm), 8 Dec 1992

   MODIFICATIONS (most recent to least)

**********************************************************/

visit_critical(struct task_node *pointer) {

  struct task_list *temp;   /* temporary variables */
  struct task_node *parent;
  int i;
  unsigned prev_critical_date;

  /*--------------------------------------------------------------------- 
    Calculate a new critical date only if the caritical date
    stored in the database is greater than the one calculated from
    this path.
  ----------------------------------------------------------------------*/
  if (curr_critical_date < pointer->data->critical_date) { 
    /*-----------------------------------------------------------------------
      The curr_critical_date variable will contain the latest end dates
      for parent nodes of a given child node. Suppose that the latest end 
      date of a node is X, and the duration is D. Then, the latest end 
      dates of any of its parents would be X-D.
    ----------------------------------------------------------------------*/ 
    pointer->data->critical_date = prev_critical_date = curr_critical_date;
  
    curr_critical_date -= pointer->data->duration; /* update critical date */ 

    temp = pointer->data->dependencies;
    if (temp != NULL)   /* check whether there is any dependencies */
      /* for each parent node ... */
      for(parent = temp->head; parent != NULL; parent = parent->next) 
	visit_critical(parent);   /* process each parent node/task */
  }
  /*----------------------------------------------------------------------- 
    Before exiting the function, set curr_critical_date to its previous value,
    meaning that curr_critical_date now contains the latest end date of
    the parent node.
  -------------------------------------------------------------------------*/
  curr_critical_date = prev_critical_date;

}  /* end visit_critical() */


