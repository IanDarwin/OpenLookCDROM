/* FILE parser.y *******************************************************
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
 *  This file contains the format for the yyparse routine used
 *  in parsing a file.
 *  
 * AUTHOR/AUDITOR/TESTER
 *  
 * Authors & Testers
 *    Mark M. Lacey, ? Nov 1992 --- grammar and some productions
 *    Brian Gaubert, ? Nov 1992 --- remaining productions
 *
 * Undocumented productions with no actions specified are for
 * structure only and cannot be removed.
 *
 * FILE parser.y */

%{
#include <stdio.h>
#include "db.h"
#include "semantic_stack.h"
#include "string_table.h"

char *name;        /* name of the task */
char *desc;        /* task description */
unsigned duration;            /* the duration, in days */
unsigned planned_start_date;  /* All of the following dates are */
                              /* stored in Julian format */
unsigned planned_end_date;
unsigned actual_start_date;
unsigned actual_end_date;
unsigned forecast_start_date;
unsigned forecast_end_date;
unsigned earliest_start_date;
unsigned earliest_end_date;
unsigned latest_start_date;
unsigned latest_end_date;

unsigned float_time;              /* the float time, in days */

enum boolean milestone;           /* does this completion of this task */
      			          /* mean we have hit a milstone? */
enum boolean deliverable;         /* does the completion of this task */
                                  /* yield a deliverable? */

struct resource_list *resources; /* list of resources that are to */
                                 /* be employed in completing this */
				 /* task */

struct resource_node *resnode;
struct resource_info *resinfo;

struct task_list *subtasks;     /* list of sub-tasks that are a */
				/* part of this task */

struct task_node *parent;       /* if this task is a subtask, then */
				/* this points to the task that */
				/* this is a subtask of --- */
				/* otherwise this is NULL */

struct task_list *dependencies; /* those tasks that need to be */
				/* complete before this task can be */
			        /* started */

struct task_list *dependents;   /* those tasks that depend on this */
				/* tasks being complete before they */
                                /* can be started */
 
unsigned number_of_dependents;  /* used in checking for dependency */
                                /* loops */



static struct task_info *new_task;
static struct task_node *node, *new_node;

static unsigned x_pert = 0;
static unsigned y_pert = 0;

static unsigned x_gantt = 0;
static unsigned y_gantt = 0;

static unsigned length = 0;

%}
%token TASK
%token NAME
%token DESCRIPTION
%token PARENT
%token DEPENDENCIES
%token RESOURCES
%token STRING
%token DURATION
%token FLOAT
%token PLANNEDSTART
%token ACTUALSTART
%token FORECASTSTART
%token EARLIESTSTART
%token LATESTSTART
%token PLANNEDEND
%token ACTUALEND
%token FORECASTEND
%token EARLIESTEND
%token LATESTEND
%token MILESTONE
%token DELIVERABLE
%token UNSIGNED
%token TRUE
%token FALSE
%token BEG
%token END
%token TASKS
%%
project 
	: tasklist		    
{ 
   YYACCEPT;
}
	;

tasklist
	: taskblock '.'
        ;

taskblock
        : taskblockhead taskblocktail
        ;

taskblockhead
        : BEG TASKS { clear_temps(); }
        ;

taskblocktail
        : tasksublist END TASKS
        | END TASKS
	;


tasksublist
	: atask tasksublist			
        | atask
	;

atask 
	: TASK '=' '{' alist '}'		
{
   /* search for node in task list */
   node = find_task(get_main_task_list(), name);

   /* if it doesn't exist, add it */
   if (!node) {
      new_task = create_task_info(name, desc,
				  duration,
				  planned_start_date, planned_end_date,
				  actual_start_date, actual_end_date,
				  forecast_start_date, forecast_end_date,
				  earliest_start_date, earliest_end_date,
				  latest_start_date, latest_end_date,
				  float_time, milestone, deliverable,
				  resources, subtasks, parent,
				  dependencies, dependents,
				  number_of_dependents, x_pert, y_pert,
				  x_gantt, y_gantt, length);
      
      new_node = create_task_node(new_task, NULL, NULL);
      add_task_to_end(get_main_task_list(), new_node);
   } else {
      /* make a copy of the data and disconnect it from the list */
      /* temporarily */
      new_node = create_task_node(node->data, NULL, NULL);
      remove_task_node(get_main_task_list(), node);

      /* change the information */
      change_task_info(new_node->data, name, desc,
		       duration,
		       planned_start_date, planned_end_date,
		       actual_start_date, actual_end_date,
		       forecast_start_date, forecast_end_date,
		       earliest_start_date, earliest_end_date,
		       latest_start_date, latest_end_date,
		       float_time, milestone, deliverable,
		       resources, subtasks, parent,
		       dependencies, dependents,
		       number_of_dependents, x_pert, y_pert,
		       x_gantt, y_gantt, length);

      /* add it back to the list, at the end */
      add_task_to_end(get_main_task_list(), new_node);
   }
   
   /* clear our temporaries */
   clear_temps();
}
        ;

alist
	: assignment ';' alist
	| assignment ';'			
	;

assignment
	: stringassign				
	| dependencylist			
	| resourcelist				
	| numberassign				
	| booleanassign				
	| taskblock				
{
   fprintf(stderr, "Nested subtasks not implemented!\n");
}
	;

stringassign 
	: NAME '=' STRING			
{
   name = $3.str;
}
        | DESCRIPTION '=' STRING		
{
   desc = $3.str;
}

	| PARENT '=' STRING			
{
   parent = NULL;
   subtasks = NULL;
}
	;


dependencylist
	: dependhead dependencytail		
	;

dependhead
	: BEG DEPENDENCIES			
	;

dependencytail
	: dependency END DEPENDENCIES 		{
	   /* create a dependency list for this task */
	   dependencies = create_task_list(NULL, NULL, NULL);
	   node = find_task(get_main_task_list(), $1.str);
	   /* if the dependency is not already in the task list, */
	   /* create it */
	   if (!node) {
	      new_task = create_task_info($1.str, NULL, 0,
					  0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0,
					  NULL, NULL, NULL,
					  NULL, NULL,
					  0,
					  x_pert, y_pert, x_gantt,
					  y_gantt, length);

	      node = create_task_node(new_task, NULL, NULL);
	      add_task_to_beginning(get_main_task_list(), node);
	   }
	   new_node = create_task_node(node->data, NULL, NULL);
	   /* always add to the beginning since this is a */
	   /* right-recursive production */
	   add_task_to_beginning(dependencies, new_node);
	}
	| dependency ',' dependencytail		{
	   /* same as above, but don't create the list */
	   node = find_task(get_main_task_list(), $1.str);
	   if (!node) {
	      new_task = create_task_info($1.str, NULL, 0,
					  0, 0, 0, 0, 0, 0, 0, 0,
					  0, 0, 0, 0, 0,
					  NULL, NULL, NULL,
					  NULL, NULL,
					  0,
					  x_pert, y_pert, x_gantt,
					  y_gantt, length);

	      node = create_task_node(new_task, NULL, NULL);
	      add_task_to_beginning(get_main_task_list(), node);
	   }
	   new_node = create_task_node(node->data, NULL, NULL);
	   add_task_to_beginning(dependencies, new_node);
	}
        ;

dependency
	: STRING				
	;

resourcelist
	: resourcehead resourcetail		
	;

resourcehead
	: BEG RESOURCES				{
	   
	}
	;

resourcetail
	: resource END RESOURCES 		{
	   /* create a new resource list and add resources to it */
	   resources = create_resource_list(NULL, NULL, NULL);
	   resinfo = create_resource_info($1.str);
	   resnode = create_resource_node(resinfo, NULL, NULL);
	   add_resource_to_beginning(resources, resnode);
	}
	| resource ',' resourcetail		{
	   /* add resource to our list */
	   resinfo = create_resource_info($1.str);
	   resnode = create_resource_node(resinfo, NULL, NULL);
	   add_resource_to_beginning(resources, resnode);
	}
	;

resource
	: STRING				
	;

numberassign 
	: DURATION '=' UNSIGNED			
{ 
   duration = $3.uint;
}
	| FLOAT '=' UNSIGNED			
{ 
   float_time = $3.uint;
}
	| PLANNEDSTART '=' UNSIGNED		
{
   planned_start_date = $3.uint;
}
	| ACTUALSTART '=' UNSIGNED		
{
   actual_start_date = $3.uint;
}
	| FORECASTSTART '=' UNSIGNED		
{
   forecast_start_date = $3.uint;
}
	| EARLIESTSTART '=' UNSIGNED		
{
   earliest_start_date = $3.uint;
}
	| LATESTSTART '=' UNSIGNED		
{
   latest_start_date = $3.uint;
}
	| PLANNEDEND '=' UNSIGNED		
{
   planned_end_date = $3.uint;
}
	| ACTUALEND '=' UNSIGNED		
{
   actual_end_date = $3.uint;
}
	| FORECASTEND '=' UNSIGNED		
{
   forecast_end_date = $3.uint;
}
	| EARLIESTEND '=' UNSIGNED		
{
   earliest_end_date = $3.uint;
}
	| LATESTEND '=' UNSIGNED        	
{
   latest_end_date = $3.uint;
}
	;

booleanassign 
	: MILESTONE '=' boolean			
{
   milestone = $3.uint;
}
        | DELIVERABLE '=' boolean		
{
   deliverable = $3.uint;
}
	;

boolean
	: TRUE                                  
{ 
   $$.uint = true;
}
	| FALSE					
{ 
   $$.uint = false;
}
	;
%%
int yyerror(char *s)
{
}

clear_temps()
{
   name = NULL;
   desc = NULL;
   duration = 0;
   float_time = 0;
   planned_start_date = 0;
   planned_end_date = 0;
   actual_start_date = 0;
   actual_end_date = 0;
   forecast_start_date = 0;
   forecast_end_date = 0;
   earliest_start_date = 0;
   earliest_end_date = 0;
   latest_start_date = 0;
   latest_end_date = 0;
   milestone = 0;
   deliverable = 0; 
   resources = NULL;
   subtasks = NULL;
   parent = NULL;
   dependencies = NULL;
   dependents = NULL;
   number_of_dependents = 0;
 }
