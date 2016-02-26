/* Top Level Code: Group 7 ltpert.c */

/* FILE ltgantt.c *************************************************************
 *
 * Description of contents
 *
 *    The functions in this file are used to generate a LaTeX file
 *    with a \picture environment in it, and \put commands within that
 *    to draw out the Gantt chart for the project tasks.
 *
 * Header files referenced
 *
 *    <none>
 *
 * Author.... Mark M. Lacey (mml)
 * Date...... 1992 Nov 2
 *
 * Auditor... <none>
 * Date...... 
 *
 * Modifications (most recent to least)
 *
 *    <none>
 *
 * FILE ltgantt.c */

/* FUNCTION generate_gantt_chart *********************************************
 *
 * Purpose
 *
 *    Given a file name, this routine generates a Gantt chart for the
 *    main list of tasks and seperate charts for each of the
 *    sub-tasks.
 *
 * Sample call
 *
 *    generate_gantt_chart(filename);
 *
 * Inputs
 *
 *    filename      This is a pointer to a character string which
 *                  contains the name of the file to be generated.
 *
 * Outputs
 *
 *    Returns 0 if successful, or an error code otherwise.
 *
 * Pertinent information
 *
 *    This function uses the function in the database module to get
 *    the pointer to the main task list.  It might be a good idea to
 *    modify it to take a task list as a pointer, so that it would be
 *    possible to generate Gantt charts for only partial task lists, or
 *    generate a Gantt chart for only a single sub-task list of a task.
 *
 * Functions referenced
 *
 *    get_main_task_list
 *    split_gantt_chart_into_pages
 *    get_gantt_formatting_info
 *    generate_gantt_page
 *
 * Author.... Mark M. Lacey (mml)
 * Date...... 1992 Nov 2
 *
 * Auditor... <none>
 * Date...... 
 *
 * Modifications (most recent to least)
 *
 *    <none>
 *
 * FUNCTION generate_gantt_chart */
int generate_gantt_chart(char *filename)
{
   FILE *fp;
   int page_down_ndx, page_across_ndx, pages_across, pages_down;
   struct task_list *main_task_list;
   struct task_list **matrix_of_task_lists;
   
   main_task_list = get_main_task_list();
   get_gantt_formatting_info(main_task_list);
   split_gantt_chart_into_pages(main_task_list, &matrix_of_task_lists,
			       &pages_across, &pages_down);

   fp = fopen(filename, "w");
   if (fp == NULL) {
      /* return error code for not being able to open file */
   } 
   
   for (page_across_ndx = 0; page_across_ndx < pages_across;
	++page_across_indx) {
      for (page_down_ndx = 0; page_down_ndx < pages_down;
	   ++page_down_ndx) {
	 generate_gantt_page(fp, page_down_ndx, page_across_ndx,
			    matrix_of_task_lists,
			    matrix_of_task_lists[page_down_ndx][page_across_ndx]);
      }
   }

   fclose(fp);
}

/* FUNCTION split_gantt_chart_into_pages **************************************
 *
 * Purpose
 *
 *    Given a list of tasks, and a matrix of task lists to store
 *    results into, this routine finds the number of pages that will
 *    be needed for the Gantt chart and then places each Gantt box into
 *    the appropriate list in the matrix of lists.
 *
 * Sample call
 *
 *    split_gantt_chart_into_pages(main_task_list, matrix_of_task_lists,
 *	  		          &pages_across, &pages_down);
 *
 * Inputs
 *
 *    main_task_list   Main list of tasks to be placed on a Gantt
 *                     chart.
 *
 * Outputs
 *
 *    matrix_of_task_lists   A matrix of task lists, with each
 *                           component of the matrix being the list
 *                           of tasks that should show up on that page
 *                           (i.e. mat[0][2] is the list of things
 *                           that should be on the first page down
 *                           (0+1) and the third page over (1+2).
 *
 *    pages_across           The number of pages across that the
 *                           entire chart will take.
 *
 *    pages_down             The number of pages down that the entire
 *                           chart will take.
 *
 * Pertinent information
 *
 *    <none>
 *
 * Functions referenced
 *
 * Author.... Mark M. Lacey (mml)
 * Date...... 1992 Nov 2
 *
 * Auditor... <none>
 * Date...... 
 *
 * Modifications (most recent to least)
 *
 *    <none>
 *
 * FUNCTION split_gantt_chart_into_pages */
void split_gantt_chart_into_pages(struct task_list *task_list,
				 struct task_list
				 ***matrix_of_task_lists_ptr,
				 int *pages_across_ptr,
				 int *pages_down_ptr)
{
   int maximum_x, maximum_y;
   struct task_list *traverse;
   int pd_ndx;
   int pa_ndx;

   maximum_x = 0;
   maximum_y = 0;
   
   traverse = task_list;
   while (traverse) {
      if (traverse->x > maximum_x)
	maximum_x = traverse->x;
      if (traverse->y > maximum_y)
	maximum_y = traverse->y;
      
      traverse = traverse->next;
   }
   
   *pages_across_ptr = maximum_x % PAGE_WIDTH;
   *pages_down_ptr = maximum_y % PAGE_HEIGHT;

   /* allocate space for matrix of tasks */

   *matrix_of_task_lists_ptr =
     (struct task_list **) malloc(*pages_down_ptr *
				  sizeof(struct task_list *));
   for (pd_ndx = 0; pd_ndx < *pages_down_ptr; ++pd_ndx) {
      matrix_of_task_lists_ptr[pd_ndx] =
	(struct task_list *) malloc(*pages_across_ptr *
				    sizeof(struct task_list));
   }

   /* Add tasks to the appropriate task list */
   traverse = task_list;
   while (traverse) {
      pa_ndx = traverse->x % PAGE_WIDTH;
      pd_ndx = traverse->y % PAGE_HEIGHT;

      add_task_to_end(*matrix_of_task_lists_ptr[pd_ndx][pa_ndx],
		      traverse);
   }
}
