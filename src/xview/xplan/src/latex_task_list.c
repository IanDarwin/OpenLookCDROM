/* xplan - project planning tool
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
 *--------------------------------------------------------------------------
 * FILE latex_task_list.c *************************************************
 *
 *  CONTENTS OF THE FILE
 *
 *    This file contains the function latex_task_list,
 *
 * FILE latex_task_list.c *************************************************/
      
#include <stdio.h>
#include <string.h>
#include "db.h"
#include "julian.h"
#include <stdlib.h>
#include <ctype.h>

static FILE *tl;
static FILE *out;

extern char export_filename[];

/* FUNCTION latex_task_list() **********************************
 *
 *   DESCRIPTION OF FUNCTION
 *    This function will create two pages of task sheet information in latex
 *    format so the user can incorperate the information into documentation
 *
 *
 *  SAMPLE CALL
 *
 *    vara = latex_task_list();
 *
 *  OUTPUTS
 *
 *   Latex output of 2 task sheets
 *
 *  AUTHOR/TESTER
 *
 *  Author... Brian Gaubert Dec, 2 1992
 *  Tester... Brian Gaubert Dec, 2 1992
 *
 *  LAST MODIFIED    
 *
 *   Date....  December 7, 1992
 *
 * FUNCTION latex_task_list() */

int latex_task_list()
{

   /* Declaring and initializing all variables usen in the function */
   int i, j, k;
   struct task_list *temp;
   struct task_list *temp2;
   struct task_node *node;
   struct task_node *node2;
   char taskl[256];
   char task[4];
   char c;
   int pages = 45;
   char b;
   int count = 1;


   /* Open the file used to create the latex document */
   tl = fopen(export_filename,"w");

   /* If it is a vbad file then return 1 */
   if (tl == NULL) {
      return 1;
   }

   /* Get the main task list so the information can be used to store */
   /* in the latex document */
   temp = get_main_task_list();
   strcpy(task,"Task");
   
   /* This will set up the document for the latex output of
      the task list. */
   fprintf(tl,"\\documentstyle[11pt,fullpage]{article}\n");
   fprintf(tl,"\\begin{document}\n");
   fprintf(tl,"\\centering{{\\Large\\bf Task Sheet}} \\\\\n");
   fprintf(tl,"\\begin{tabular}{||l|l|l|c|c||} \\hline\n");
   fprintf(tl,"TASK  & NAME & DUR & PLANNED START DATE & PLANNED END DATE \\\\ \\hline\\hline\n");
   
   /* Set the head of the top of the list to the current task in the 
      list */
   node = temp->head;
   if(node == NULL) {
      return 1;
   }

   /* This will start the formation of the first page in the task list */
   /* and go until the list has been exhausted, and print the */
   /* pertinant information to the file in latex format */
   while( node != NULL ) {
      strcpy(taskl,node->data->name);
      strfix(taskl);
      fprintf(tl,"%4.4s %3.3d & %25.25s & %3.3d & %8.8s & %8.8s \\\\ \\hline\n",\
	      task, count,taskl, node->data->duration,\
	      julian_to_str_date(node->data->planned_start_date),\
	      julian_to_str_date(node->data->planned_end_date));
      count++;
      node = node->next;

      /* If there is not any more room on the page, then start a new */
      /* page */
      if (count > pages) {
	 fprintf(tl,"\\end{tabular}\n");
	 fprintf(tl,"\\clearpage\n");
	 fprintf(tl,"\\begin{tabular}{||l|l|l|c|c||} \\hline\n");
	 fprintf(tl,"TASK  & NAME & DUR & PLANNED START DATE & PLANNED END DATE \\\\ \\hline\\hline\n");
	 pages = pages + 45;
      }
      
   }
   /* Print out the next page which contains the description and the */
   /* task number, and this will be in the latex description */
   /* environment */
   fprintf(tl,"\\end{tabular}\n");
   fprintf(tl,"\\clearpage\n");
   
   count = 1;
   fprintf(tl,"\\begin{description}\n");

   /* Get the head of the lits as a different node  and check its */
   /* pointer */
   temp2 = get_main_task_list();   
   node2 = temp2->head;
   if(node2 == NULL) {
      return 1;
   }

   /* Go until the list is exhausted */
   while( node2 != NULL ) {
      fprintf(tl,"\\item[%s %d]%s\n",task,count,node2->data->desc);
      count++;
      node2 = node2->next;
   }

   /* End the document */
   fprintf(tl,"\\end{description}\n");
   fprintf(tl,"\\end{document}\n");
   fclose(tl);   
}

