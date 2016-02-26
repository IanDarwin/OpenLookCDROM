/* FILE writefile.c **********************************************************
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
 *----------------------------------------------------------------------
 *
 * Description of contents
 *   
 *      This file will contian the function writefile. The function writefile
 *    will be in charge of writing the information to the file from the
 *    database.
 *    
 * Header files referenced
 *
 *   There are none at this time.
 *
 * Author.... Brian Gaubert
 *
 * FILE prototype.c */

#include <stdio.h>
#include <string.h>
#include "string_table.h"
#include "db.h"

static FILE *fp;

/* FUNCTION writefile.c *******************************************************
 *
 * Purpose
 *
 *    The purpose of this function is to write the information from the
 *   database to the file. This will be done by using the pointers from the
 *   database structures.
 * 
 *
 * Sample call
 *
 *    retval = writefile(tasklist *data, FILE *fp)
 *
 * Inputs
 *
 *    data      This is the pointer of the first task in the database
 *    fp       This is the pointer of the file to add the task to.
 *
 * Outputs
 *
 *    retval   This is the value that will be returned if the write funnction 
 *              fails;
 *
 *
 * Author.... Brian Gaubert
 *
 *  Processing narrative:
 *
 *	 This module is used to save the task information into a file of the
 *	users choice. The parameters of this routine will include the pointer
 *	to the outputfile and the task-list pointer of the first task in
 *	the list. Within the module, there will be two pointers for the
 *	traversal of the lists. The first pointer tem-task-ptr is the pointer
 *	that will move along the main task list. The second pointer is the
 *	pointer that will go through the subtask lists. This routine will run
 *	recursively so that each task list will have a pointer that will go
 *	through its current main task list and a pointer to run through the
 *	subtask lists. When the function returns to the previous call of the
 *	function ( a recursion has been done ) the tem-task-list will go to the
 *	next node in the list, and the sub-task-list will be set to the
 *	tem-task-list which in tern sets both of the pointers to the top of the
 *	next list in the list.
 *
 *	The database will be accessed from this routine by the pointer value
 *	of task-list. All of the variables within the struct of the task-list
 *	pointer coming in from the call of the function will be places in the
 *	print format and written directly to the file. The resources and
 *	dependencies have a list of their own for each task. These lists will
 *	be outputted to the file in the order that they were put into the
 *	database.
 *
 * NOTE: Writefile_init has been added that calls the function writefile
 *       so that error checking can be done by these lower modules instead
 *       of the GUI.
 *
 * 
 *  PDL for writefile:
 * PROCEDURE write-file
 * INTERFACE ptr(file pointer),task-list;
 * ptr IS FILE PTR;
 * tem-task-list IS PTR OF TASK-LIST
 * sub-task-list IS PTR OF TASK-LIST
 *  CALL get-main-task-list PROCEDURE WITH sub-task-list;
 *  WRITE (``task begin\n'');
 *
 *  DO WHILE tem-task-list != NULL
 *    WRITE (ptr,``task = {\n'');
 *    WRITE (ptr,''\t name = %s;\n'', sub-task-list->name);
 *    WRITE (ptr,''\t description = %s;\n'',sub-task-list->desc);
 *   WRITE (ptr.''\t planned-start = %d;\n",sub-task-list->planned-start-date);
 *    WRITE (ptr.''\t planned-end = %d;\n'',sub-task-list->planned-end-date);
 *    WRITE (ptr.''\t actual-start = %d;\n'',sub-task-list->actual-start-date);
 *    WRITE (ptr.''\t actual-end = %d;\n'',sub-task-list->actual-end-date);
 *   WRITE(ptr."\t forecast-start = %d;\n",sub-task-list->forecast-start-date);
 *    WRITE (ptr.''\t forecast-end = %d;\n'',sub-task-list->forecast-end-date);
 *   WRITE (ptr"\t earliest-start = %d;\n",sub-task-list->earliest-start-date);
 *    WRITE (ptr.''\t earliest-end = %d;\n'',sub-task-list->earliest-end-date);
 *    WRITE (ptr.''\t latest-start = %d;\n'',sub-task-list->latest-start-date);
 *    WRITE (ptr.''\t latest-end = %d;\n'',sub-task-list->latest-end-date);
 *    WRITE (ptr,''\t float = %d;\n'',sub-task-list->fload-time);
 *    WRITE (ptr,''\t milestone = %s;\n'',sub-task-list->milestone);
 *    WRITE (ptr,''\t deliverable = %s;\n'',sub-task-list->deliverable);
 *    IF sub-task-list->resource-list != NULL
 *      WRITE (ptr,''\t\tbegin resources;\n'')
 *      DO WHILE sub-task-list->resource-list != NULL
 *        WRITE (ptr,''\t\t\t \"%s\"\n'',sub-task-list->resource-list);
 *        sub-task-list->resource-list = sub-task-list->resource-list->next;
 *      END WHILE
 *      WRITE (ptr,''\t\tend resources;\n'');
 *    ENDIF
 *    IF sub-task-list->dependancy-list != NULL THEN
 *       WRITE (ptr,''\t\tbegin dependencies;\n'')
 *      DO WHILE sub-task-list->dependency-list != NULL
 *        WRITE (ptr,''\t\t\t \"%s\"\n'',sub-task-list->dependency-list);
 *        task-list->dependency-list = sub-task-list->dependency-list->next;
 *      END WHILE
 *      WRITE (ptr,''\t\tend dependencies;\n'');
 *    ENDIF
 *    WRITE (ptr,''\tparent = %s;\n'',sub-task-list->parent);
 *     DO WHILE sub-task-list->subtask != NULL
 *
 *
 * FUNCTION writefile and writefile_init */
int writefile_init(char *filename)
{
  /*initialize the error condition */
  int error;

  /* open the file specified by the user */
  fp = fopen(filename,"w");

  /* Check the file pointer if bad return 1 else call writefile */
  /* function */    
  if (fp == NULL)
   {
      return 1;
   }
  else
   {
     writefile();
     fclose(fp);
  }
}

/* FUNCTION writefile()***************************************************
 *
 *  PURPOSE
 *
 *   This function will write the contents of the database to a file
 *   in the format that can be read from the scanner and
 *    parser from Flex and Bison 
 *
 *  SAMPLE CALL
 *
 *    writefile();
 *   
 *  INPUTS
 *
 *   none.
 *
 *  OUTPUTS
 *
 *   A file that contains all of the information that the user typed
 *   into the database in the form of tasks.
 *
 *  AUTHOR/AUDITOR/TESTER
 *
 *  Author.... Brian Gaubert Dec, 2, 1992
 *             Mark Lacey Dec. 2, 1992
 *  Tester.... Brian Gaubert Dec, 2, 1992
 *             Mark Lacey Dec. 2, 1992
 *
 *  MODIFICATIONS (most recent to least)
 *
 * FUNCTION generate_gantt_charts() */

int writefile()
{

 /* Declare and initialize the variables used in this function */
 struct task_node *current;
 struct task_node *trav;
 struct resource_node *res;

 /* Get the first task of the list and assign that to be the current */
 /* task of the list */    
 current = get_main_task_list()->head;

 /* Start out printing the first thing in the file , begin tasks */
 fprintf(fp,"begin tasks\n\n");

 while( current != NULL )
  {
   /* task = etc. will be done for every task in the list */  
   fprintf(fp,"task = {\n");

   /* Print out the name of the task */
   if ((current->data->name != NULL)&&\
                                (strlen(current->data->name) != 0))
     fprintf(fp,"\t name = \"%s\";\n", current->data->name);

   /* print out the description */
   if ((current->data->desc != NULL)&&\
                                (strlen(current->data->desc) != 0))
     fprintf(fp,"\t description = \"%s\";\n",current->data->desc);

   /* Print out the duration */
   if (current->data->duration != 0)
     fprintf(fp,"\t duration = %u;\n",\
	   current->data->duration);

   /* Print out the planned_start_date */
   if (current->data->planned_start_date != 0)
     fprintf(fp,"\t planned_start = %u;\n",\
	   current->data->planned_start_date);

   /* Print out the planned_end_date */
   if (current->data->planned_end_date != 0)
     fprintf(fp,"\t planned_end = %u;\n",\
	   current->data->planned_end_date);

   /* Print out the actual_start_date */
   if (current->data->actual_start_date != 0)
     fprintf(fp,"\t actual_start = %u;\n",\
	   current->data->actual_start_date);

   /* Print out the actual_end_date */
   if (current->data->actual_end_date != 0)
     fprintf(fp,"\t actual_end = %u;\n",\
	   current->data->actual_end_date);

   /* Print out the forecast_start_date */
   if (current->data->forecast_start_date != 0)
     fprintf(fp,"\t forecast_start = %u;\n",\
	   current->data->forecast_start_date);

   /* Print out the forecast_end_date */
   if (current->data->forecast_end_date != 0)
     fprintf(fp,"\t forecast_end = %u;\n",\
	   current->data->forecast_end_date);

   /* Print out the earliest_start_date */
   if (current->data->earliest_start_date != 0)
     fprintf(fp,"\t earliest_start = %u;\n",\
	   current->data->earliest_start_date);

   /* Print out the earliest_end_date */
   if (current->data->earliest_end_date != 0)
     fprintf(fp,"\t earliest_end = %u;\n",\
	   current->data->earliest_end_date);

   /* Print out the latest_start_date */
   if (current->data->latest_start_date != 0)
     fprintf(fp,"\t latest_start = %u;\n",\
	   current->data->latest_start_date);

   /* Print out the latest_end_date */
   if (current->data->latest_end_date != 0)
     fprintf(fp,"\t latest_end = %u;\n",\
	   current->data->latest_end_date);

   /* Print out the float time */
   if (current->data->float_time != 0)
     fprintf(fp,"\t float = %u;\n",\
	   current->data->float_time);

   /* Print out the milestone */
   if (current->data->milestone != 0)
     fprintf(fp,"\t milestone = true;\n");
   else
     fprintf(fp,"\t milestone = false;\n");

   /* Print out the deliverable */
   if (current->data->deliverable != 0)
     fprintf(fp,"\t deliverable = true;\n");
   else
     fprintf(fp,"\t deliverable = false;\n");

   /* Print out the dependencies */
   trav = current->data->dependencies->head;
   if (trav) {
      fprintf(fp, "\t begin dependencies\n");
      while (trav) {
	 fprintf(fp, "\t\t \"%s\"", trav->data->name);
	 if (trav->next) fprintf(fp, ",\n");
	 else fprintf(fp, "\n");
	 
	 trav = trav->next;
      }
      fprintf(fp, "\t end dependencies;\n");
   }

   /* Print out the resources */
   res = current->data->resources->head;
   if (res) {
      fprintf(fp, "\t begin resources\n");
      while (res) {
	 fprintf(fp, "\t\t \"%s\"", res->data->resource->name);
	 if (res->next) fprintf(fp, ",\n");
	 else fprintf(fp, "\n");

	 res = res->next;
      }
      fprintf(fp, "\t end resources;\n");
   }

/* for debugging only 
   trav = current->data->dependents->head;
   if (trav) {
      fprintf(fp, "\t begin dependents\n");
      while (trav) {
	 fprintf(fp, "\t\t \"%s\"", trav->data->name);
	 if (trav->next) fprintf(fp, ",\n");
	 else fprintf(fp, "\n");
	 
	 trav = trav->next;
      }
      fprintf(fp, "\t end dependents;\n");
   }
*/

   /* Go on to the next task if there is one */
   fprintf(fp,"}\n\n");
   current = current->next;

   }
   fprintf(fp,"end tasks.\n");
} /* End FUNCTION writefile */
      
