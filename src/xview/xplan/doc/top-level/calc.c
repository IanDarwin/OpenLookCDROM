/* Top Level Code: Group 7 calc.c */

/* FILE calc.c ****************************************************************
 *
 * Description of contents
 *
 *    The functions in this file are used to calculate the PERT and Gantt
 *    charts formatting info and checking for dependency loop.
 *    
 * Header files referenced
 *
 *    <none>
 *
 * Author.... Richard Malingkas (rcm)
 * Date...... 1992 Nov 2
 *
 * Auditor... <none>
 * Date......
 *
 * Modifications (most recent to least)
 *
 *    <none>
 *
 * FILE calc.c */

int x-coord,
    y-coord,
    counter = 0;
boolean  loop_found;

traverse_task_list() {

  struct task_node *pointer, *temp;

  pointer = get_first_task();
  loop_found = false;
  visit_task(pointer);
}


/* FUNCTION visit_task ********************************************************
 *
 * Purpose
 *
 *   Given a pointer to the beginning of a task list, this routine will
 *   traverse the list recursively, visit each node in the task list using 
 *   a modified version of depth-first-search algorithm. This routine will :
 *   a. Calculate the relative positions of PERT boxes.
 *   b. Mark a node if it's part of the critical path.
 *   c. Check for dependency loop in the task list.
 *
 * Sample call
 *
 *    visit_task(task_node_pointer);
 *
 * Inputs
 *
 *    task_node_pointer  pointer to the beginning of a task list
 *
 * Outputs
 *
 *    <none>
 *
 * Pertinent information
 *
 *    <none>
 *
 * Functions referenced
 *
 * Author.... Richard Malingkas (rcm)
 * Date...... 1992 Nov 2
 *
 * Auditor... <none>
 * Date...... 
 *
 * Modifications (most recent to least)
 *
 *    <none>
 *
 * FUNCTION visit_task ********************************************************

visit_task(struct task_node *pointer) {

  int i;

  if (loop_found == false) {
    if (pointer->data->number_of_visit > 0) {
      y_coord++;
      return;
    }
    else {
      counter++;
      x_coord++;
      pointer->data->y_pert = y_coord;
      
      if (counter > pointer->data->number_of_dependents) {
	loop_found = true;
	printf("**** found a loop *******\n"); /* modify later; send to GUI */
	return;
      }
      if (x_coord > pointer->data->xpert)
	pointer->data->x_pert = x_coord;
    } /* else */

    temp = pointer->data->dependents->head;
    for (i=0; temp != NULL ;temp = temp->next, i++)
      visit_task(temp);

    if (i == 0) /* this node has no dependent */
      y_coord++;

    counter--;
    x_coord--;
    pointer->data->number_of_visit = 1;
  } /* endif (loop_found == false) */
} /* end visit_task() */

int date_length,
    reference_point, 
    first_date, 
    y_down;

calculate_gantt() {

  struct task_node *pointer;

  pointer = get_first_task();
  gantt_visit(pointer);
}

gantt_visit(struct task_node pointer) {

  struct task_node *pointer, *temp;

  pointer->data->number_of_visits++;

  if (pointer->data->number_of_visits > pointer->data->number_of_dependents) {
    pointer->data->x_gantt = reference_point + 
      pointer->data->planned_start_date - first_date;
    y_down++;
    pointer->data->y_gantt = y_down;
    pointer->data->length = date_length * pointer->data->duration;

    temp = pointer->data->dependents->head;
    for (i=0; temp != NULL ;temp = temp->next, i++)
      visit_gantt(temp);
  }
} /* end gantt_visit() */
