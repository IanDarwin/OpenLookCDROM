/* xplan - project planning tool
 *
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
 *-----------------------------------------------------------------------------
 * FILE gantt.c ***************************************************************
 *  
 *  DESCRIPTION OF CONTENTS
 *  
 *     This file contains the function generate_gantt_chart(), This
 *  function will be described in the header for the function under
 *  the FUNCTION heading.  
 *  
 * FILE gantt.c */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "db.h"
#include "julian.h"
#define TASKS_ON_SHEET 25
#define SUNDAY 6;
static FILE *fp;
extern char export_filename[];

/* FUNCTION generate_gantt_charts()********************************************
 *
 *  PURPOSE
 *
 *  This function will generate latex output of gent charts as shone
 *  in the gantt charts from within the project. In order to generate the
 *  gantt charts, the user must, after the file has been created, type
 *  the command latex <filename>. Then to print the user must type in
 *  dvips <filename> | lpr -P<printer>.
 *
 *  SAMPLE CALL
 *
 *  gantt = generate_gantt_charts();
 *   
 *  INPUTS
 *
 *   none.
 *
 *  OUTPUTS
 *
 *  <filename> - this is the filename of the users choice as he/she
 *               has selected from the user interface.
 *
 *  AUTHOR/AUDITOR/TESTER
 *
 *  Author.... Brian Gaubert Dec, 2, 1992
 *  Auditor... Brian Gaubert Dec, 2, 1992
 *  Tester.... Brian Gaubert Dec, 2, 1992
 *
 *  MODIFICATIONS (most recent to least)
 *
 * FUNCTION generate_gantt_charts() */


int generate_gantt_charts()
{


   /* Initializing all of the variables for the generation of the
      gantt charts */
   int cur_count = 1;
   int begin_count = 1;
   int end_count = 25;
   int first_prj_date = 0;
   int pages_accross = 0;
   int pages_down = 0;
   int task_length = 0;
   int start_of_task = 0;
   int first_date = 0;
   int last_date = 0;
   int number_of_task = 0;
   int total_length = 0;   
   int i = 0, j= 0, k = 0, ii, jj, kk;
   int task_count = 1;
   int top_page = 0;
   int box_height = 10;
   int end_of_task = 0;

   /* This initialization of the two dem. array will be used in printing
      the days of the week to a page in the charts. The reason for the
      same data twice in a row is because when the modulus command is used
      on the julian date, the the range of values is from 0 to 6, and
      if the number that you get when you do the modulus is six then
      six + 1 etc. will give the next date in the week */      
   char weekday[14][4] = {"Mon","Tue","Wed","Thr","Fri","Sat","Sun",\
			    "Mon","Tue","Wed","Thr","Fri","Sat","Sun"};
   int pages_down_count = 1;
   int dummy = 0;   
   struct task_list *temp;
   struct task_node *node;
   struct task_list *temp2;
   struct task_node *node2;
   char c;

   /* Use this task list to get hte number of pages accross and down */
   temp = get_main_task_list();
   node = temp->head;

   /* If the task list is null return 1 */
   if (node == NULL) {
      return 1;
   }

   /* Open the file and setup the filepointer */
   fp = fopen(export_filename,"w");

   /* If the file pointer is NULL, error in the file, then return 1 */
   if (fp == NULL) {
      return 1; 
   }

   /* Get the first date, the last date and the total days of the */
   /* project */
   first_date = node->data->planned_start_date;
   while( node != NULL ) {
	last_date = node->data->planned_end_date;
	number_of_task++;
        node = node->next;
     }
   total_length = abs(last_date - first_date);
  

   /* Calculate the pages accross */
      pages_accross = total_length/14 + 1;
      
   /* Calcualte the number of pages down */
      pages_down = number_of_task/25 + 1;

   /* Use this task list to run throught he list to find out what */
   /* information goes on what page of the gantt charts */
   temp2 = get_main_task_list();
   node2 = temp2->head;
   
   /* Calculate the first prj date of the chart */
   first_prj_date = first_date;

   /* Set uf the critical path of the project in the database */
   calc_critical();
   
   /* Print out the beginning of the latex file */
   fprintf(fp,"\\documentstyle[11pt,fullpage]{article}\n");
   fprintf(fp,"\\begin{document}\n");

   /* Loop until the pages down variable is exhausted */
   for(j=1;j<= pages_down;j++) {

      /* Loop until the pages accross variable is exhausted */
      for(i=1;i <= pages_accross;i++) {

	 /* Set up the top of each page with the days of the week and */
	 /* the start date of the project and the date that follows */
	 /* that date weekly */
	 task_count = 1; 
	 top_page = 600;
	 dummy = first_date % 7;
	 fprintf(fp,"\\begin{picture}(400,600)\n");
	 fprintf(fp,"\\put(50,648){%s}\n",\
		 julian_to_str_date(first_prj_date));
	 fprintf(fp,"\\put(260,648){%s}\n",\
		 julian_to_str_date(first_prj_date + 7));
	 fprintf(fp,"\\multiput(50,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy]);
	 fprintf(fp,"\\multiput(80,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy+1]);
	 fprintf(fp,"\\multiput(110,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy+2]);
	 fprintf(fp,"\\multiput(140,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy+3]);
	 fprintf(fp,"\\multiput(170,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy+4]);
	 fprintf(fp,"\\multiput(200,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy+5]);
	 fprintf(fp,"\\multiput(230,620)(210,0){2}{\\framebox(30,20){%s}}\n",\
		 weekday[dummy+6]);

	 /* Use this task list as the temp pointer for each page */
	 temp = get_main_task_list();
	 node2 = temp->head;

	 /* Go until the list is exhausted */
	 while(node2 != NULL) {

	    /* Set up the variable for the critical path */
	    if (node2->data->critical_path) {
	       c = 'C';
	    }
	    else {
	       c = ' ';
	    }

	    /* This section of code will determine what information */
	    /* goes on what screen and will write that information to */
	    /* the file specified by the user. If the */
	    /* start and end dates of the task are equal then ist is */
	    /* assumed to be a milestone and a diamond will be written */
	    /* to the file. A diamond will also be written to the file */
	    /* if the milestone was selected by the user. but all */
	    /* other information will be latex format to generate bars */
	    /* on the page */
	    if ((task_count >= begin_count)&&(task_count <= end_count)) {
	       
	       start_of_task = node2->data->planned_start_date - first_prj_date;
	       end_of_task = node2->data->planned_end_date - first_prj_date;
	       
	       
	       if ((end_of_task - start_of_task) == 0) {
		  fprintf(fp,"\\put(%d,%d){$\\Diamond$}\n",\
			  (end_of_task*30)+50,top_page);
	       }
	       else {
		  if ((end_of_task <= 14) && (start_of_task >= 1)) {
		     fprintf(fp,"\\put(%d,%d){\\framebox(%d,%d)[rr]{%c  }}\n",\
			     (start_of_task * 30)+50, top_page,\
			     (end_of_task - start_of_task)*30,\
			     box_height,c);       
		     if (node2->data->milestone == 1) {
			fprintf(fp,"\\put(%d,%d){$\\Diamond$}\n",\
				(end_of_task*30)+50,top_page);
		     }
		  }
		  else if ((start_of_task <= 1) && (end_of_task <= 14) \
			   && (end_of_task >= 1)) {
		     fprintf(fp,"\\put(50,%d){\\framebox(%d,%d)[rr]{%c  }}\n",\
			     top_page, end_of_task*30,\
			     box_height,c);
		     if (node2->data->milestone == 1) {
			fprintf(fp,"\\put(%d,%d){$\\Diamond$}\n",\
				(end_of_task*30)+50,top_page);
		     }
		  }		  
		  else if ((end_of_task > 14) && (start_of_task <= 1)) {       
		     fprintf(fp,"\\put(50,%d){\\framebox(420,%d)[rr]{%c  *}}\n",\
			     top_page, box_height,c);         
		  }
		  else if ((end_of_task > 14) && (start_of_task >= 1)\
			   && (start_of_task <= 14)) {
		     if ((14 - start_of_task) == 0 ) {
			fprintf(fp,"\\put(%d,%d){\\framebox(30,%d)[rr]{%c  *}}\n",\
				(start_of_task*30)+50,top_page,box_height,c);
		     }
		     else {
			fprintf(fp,"\\put(%d,%d){\\framebox(%d,%d)[rr]{%c  *}}\n",\
				(start_of_task*30)+50, top_page,(14 - start_of_task)*30,\
				box_height,c);
		     }
		  }
		  else if ((end_of_task == 14) && (start_of_task <= 1)) {       
		     if ((end_of_task - start_of_task) == 0) {
			fprintf(fp,"\\put(50,%d){\\framebox(420,%d)[rr]{%c  }}\n",\
				top_page, box_height,c);         
		     }
		  }
		  else if ((end_of_task == 14) && (start_of_task >= 1)\
			   && (start_of_task <= 14)) {       
		     fprintf(fp,"\\put(%d,%d){\\framebox(%d,%d)[rr]{%c  }}\n",\
			     (start_of_task*30)+50, top_page,(14 - start_of_task)*30,\
			     box_height,c);       
		     
		  }
	       }

	       /* This will print the task number to the left hand */
	       /* side of each and every page of the latex output */
	       fprintf(fp,"\\put(0,%d){\\framebox(50,20){Task %d}}\n",\
		       top_page, task_count);
	       top_page = top_page - 20;
	    }
	    task_count++;
	    node2 = node2->next;
	 }

	 /* This will end this pages information and go on to the next */
	 /* page */
	 fprintf(fp,"\\end{picture}\n");
	 fprintf(fp,"\\clearpage\n");

	 /* The first project date will be updated so that the dates */
	 /* can be printed to the top of the page */
	 first_prj_date = first_prj_date + 14;
	 
      }

      /* We must go until all of the pages down are done also */
      first_prj_date = first_date;
      begin_count = begin_count + 25;
      end_count = end_count + 25;
      
   }

   /* Finaly donw with the pages and we must end the document and */
   /* close the file */
   fprintf(fp,"\\end{document}\n");
   fclose(fp);
}



