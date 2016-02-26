/* FILE db.c ****************************************
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
 * General database functions for adding tasks and resources to the
 * database, search for them, and remove them.
 *  
 */   

#include <stdio.h>
#include "db.h"
#include "string_table.h"

static struct task_list *main_task_list;
static struct string_table *resource_table;

/* FUNCTION get_main_task_list ****************************************

   PURPOSE

   Returns a pointer to the main task list.  Written so we don't have
   to use it as a global pointer, and can always track the accesses to
   it.

   SAMPLE CALL

   list = get_main_task_list();

   OUTPUTS

   list --- The main list of tasks in the project that is currently
   loaded.

   AUTHOR/AUDITOR/TESTER
   
   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor... 
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct task_list *get_main_task_list()
{
   return main_task_list;
}

/* FUNCTION create_main_task_list ****************************************

   PURPOSE

   Returns a pointer to the main task list.  Written so we don't have
   to use it as a global pointer, and can always track the accesses to
   it.

   SAMPLE CALL

   create_main_task_list();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor... 
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
void create_main_task_list()
{
   main_task_list = create_task_list(NULL, NULL, NULL);
}

/* FUNCTION create_resource_table ****************************************

   PURPOSE

   Returns a pointer to the main task list.  Written so we don't have
   to use it as a global pointer, and can always track the accesses to
   it.

   SAMPLE CALL

   create_main_task_list();

   AUTHOR/AUDITOR/TESTER
   
   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor... 
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
void create_resource_table()
{
   resource_table = string_table_create(151);
}

/* FUNCTION set_main_task_list ****************************************

*/
void set_main_task_list(struct task_list *list)
{
   main_task_list = list;
}

/* FUNCTION create_task_info ****************************************

   PURPOSE

   Creates a task_info data structure given all of the parameters
   needed for it.

   SAMPLE CALL

   new_task = create_task_info(name, desc, duration,
                               planned_start_date,
                               planned_end_date,
                               actual_start_date,
                               actual_end_date,
                               forecast_start_date,
                               forecast_end_date,
                               earliest_start_date,
                               earliest_end_date,
                               latest_start_date,
                               latest_end_date,
                               float_time, 
                               milestone, deliverable, 
                               resources, 
                               subtasks, parent,
                               dependencies, dependents,
                               number_of_dependents,
                               x_pert, y_pert, 
			       x_gantt, y_gantt,
			       length);

   INPUTS

   Each argument corresponds to an element of the task_info
   structure.  See the database header file along with the detailed
   design document for a description of each.

   OUTPUTS

   A pointer to an allocated and filled in task_info structure.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

   12 Nov 92 Mark M. Lacey
   Changed the names of a few parameters.

*/
struct task_info *create_task_info(char *name, 
				   char *desc, 
				   unsigned duration, 
				   unsigned planned_start_date, 
				   unsigned planned_end_date, 
				   unsigned actual_start_date, 
				   unsigned actual_end_date, 
				   unsigned forecast_start_date, 
				   unsigned forecast_end_date, 
				   unsigned earliest_start_date, 
				   unsigned earliest_end_date, 
				   unsigned latest_start_date, 
				   unsigned latest_end_date, 
				   unsigned float_time, 
				   enum boolean milestone, 
				   enum boolean deliverable, 
				   struct resource_list *resources, 
				   struct task_list *subtasks, 
				   struct task_node *parent,
				   struct task_list *dependencies,
				   struct task_list *dependents,
				   unsigned number_of_dependents,
				   unsigned x_pert,   
				   unsigned y_pert,   
				   unsigned x_gantt,   
				   unsigned y_gantt,   
				   unsigned length)
				                  
{
   struct task_info *new_info;

   new_info = (struct task_info *) malloc(sizeof(struct task_info));

   if (name != NULL) {
      new_info->name = (char *) malloc(strlen(name)+1);
      strcpy(new_info->name, name);
   } else {
      new_info->name = NULL;
   }

   if (desc != NULL) {
      new_info->desc = (char *) malloc(strlen(desc)+1);
      strcpy(new_info->desc, desc);
   } else {
      new_info->desc = NULL;
   }

   new_info->duration = duration;
   new_info->planned_start_date = planned_start_date;
   new_info->planned_end_date = planned_end_date;
   new_info->actual_start_date = actual_start_date;
   new_info->actual_end_date = actual_end_date;
   new_info->forecast_start_date = forecast_start_date;
   new_info->forecast_end_date = forecast_end_date;
   new_info->earliest_start_date = earliest_start_date;
   new_info->earliest_end_date = earliest_end_date;
   new_info->latest_start_date = latest_start_date;
   new_info->latest_end_date = latest_end_date;
   new_info->float_time = float_time;
   new_info->milestone = milestone;
   new_info->deliverable = deliverable;
   new_info->resources = resources;
   new_info->subtasks = subtasks;
   new_info->parent = parent;
   new_info->dependencies = dependencies;
   new_info->dependents = dependents;
   new_info->x_pert = x_pert;   
   new_info->y_pert = y_pert;   
   new_info->x_gantt = x_gantt; 
   new_info->y_gantt = y_gantt; 
   new_info->length = length;

   return new_info;
}

/* FUNCTION change_task_info ****************************************

*/
void change_task_info(struct task_info *info,
		      char *name, 
		      char *desc, 
		      unsigned duration, 
		      unsigned planned_start_date, 
		      unsigned planned_end_date, 
		      unsigned actual_start_date, 
		      unsigned actual_end_date, 
		      unsigned forecast_start_date, 
		      unsigned forecast_end_date, 
		      unsigned earliest_start_date, 
		      unsigned earliest_end_date, 
		      unsigned latest_start_date, 
		      unsigned latest_end_date, 
		      unsigned float_time, 
		      enum boolean milestone, 
		      enum boolean deliverable, 
		      struct resource_list *resources, 
		      struct task_list *subtasks, 
		      struct task_node *parent,
		      struct task_list *dependencies,
		      struct task_list *dependents,
		      unsigned number_of_dependents,
		      unsigned x_pert,   
		      unsigned y_pert,   
		      unsigned x_gantt,   
		      unsigned y_gantt,   
		      unsigned length)
     
{
   if (info->name != NULL) free(info->name);
   if (name != NULL) {
      info->name = (char *) malloc(strlen(name)+1);
      strcpy(info->name, name);
   } else info->name = NULL;

   if (info->desc != NULL) free(info->desc);
   if (desc != NULL) {
      info->desc = (char *) malloc(strlen(desc)+1);
      strcpy(info->desc, desc);
   } else info->desc = NULL;

   info->duration = duration;
   info->planned_start_date = planned_start_date;
   info->planned_end_date = planned_end_date;
   info->actual_start_date = actual_start_date;
   info->actual_end_date = actual_end_date;
   info->forecast_start_date = forecast_start_date;
   info->forecast_end_date = forecast_end_date;
   info->earliest_start_date = earliest_start_date;
   info->earliest_end_date = earliest_end_date;
   info->latest_start_date = latest_start_date;
   info->latest_end_date = latest_end_date;
   info->float_time = float_time;
   info->milestone = milestone;
   info->deliverable = deliverable;
   info->resources = resources;
   info->subtasks = subtasks;
   info->parent = parent;
   info->dependencies = dependencies;
   info->dependents = dependents;
   info->x_pert = x_pert;   
   info->y_pert = y_pert;   
   info->x_gantt = x_gantt; 
   info->y_gantt = y_gantt; 
   info->length = length;
}

struct task_node *create_default_task()
{
   struct task_node *new_task_node;
   struct task_info *new_task_info;

   new_task_info = create_task_info("New Task",
				    "",
				    0, 
				    0, 0, 0, 
				    0, 0, 0, 
				    0, 0, 0, 0,
				    0, 0, 0, 
				    create_resource_list(NULL, NULL, NULL), 
				    create_task_list(NULL, NULL,
						     NULL),
				    NULL, 
				    create_task_list(NULL, NULL,
						     NULL),
				    create_task_list(NULL, NULL,
						     NULL), 
				    0,
				    0, 0, 0, 0, 0);
   
   new_task_node = create_task_node(new_task_info,
				    NULL,
				    NULL);

   return new_task_node;
}   

/* FUNCTION destroy_task_info ****************************************

   PURPOSE

   Destroys a task_info data structure, along with structures that are
   contained within it.

   SAMPLE CALL

   destroy_task_info(task);

   INPUTS

   task --- A pointer to the task_info structure to be destroyed.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92
   
   MODIFICATIONS (most recent to least)

*/
void destroy_task_info(struct task_info *taskinfo)
{
   free(taskinfo->name);
   free(taskinfo->desc);
   free(taskinfo);
}

/* FUNCTION create_task_node ****************************************

   PURPOSE

   Creates a task_node data structure given all of the parameters
   needed for it.

   SAMPLE CALL

   newnode = create_task_node(data, prev, next);

   INPUTS

   data --- A pointer to a task_info node, most likely created with
   create_task_info.

   prev --- A pointer to the previous node on the list.  

   data --- A pointer to the next node on the list. 

   OUTPUTS

   newnode --- A pointer to the task_node that was created.  This will
   be set up to include the information given in the call.

   PERTINENT INFORMATION

   Often called with prev and next equal to NULL when creating a new
   node that isn't yet a part of a list.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct task_node *create_task_node(struct task_info *data,
				   struct task_node *prev,
				   struct task_node *next)
{
   struct task_node *new_node;

   new_node = (struct task_node *) malloc(sizeof(struct task_node));

   new_node->data = data;
   new_node->prev = prev;
   new_node->next = next;

   return new_node;
}

/* FUNCTION destroy_task_node ****************************************

   PURPOSE

   Destroys a task_node data structure and removes it from the list
   it is a part of, if it is in one.  Destroys all components of the
   task_node structure, too.

   SAMPLE CALL

   destroy_task_node(tasknode);

   INPUTS

   tasknode --- A pointer to the task_node structure to be deleted.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

*/
void destroy_task_node(struct task_list *list,
		       struct task_node *tasknode)
{
   /* decrement the size of the list */
   --list->size;

   /* if there is a next node, make it point to the previous of this */
   if (tasknode->next != NULL) {
      list->current = tasknode->next;
      tasknode->next->prev = tasknode->prev;
   } else {
      list->tail = tasknode->prev;
      list->current = tasknode->prev;
   }

   /* if there is a previous node, make it point to the next of this */
   if (tasknode->prev != NULL)
     tasknode->prev->next = tasknode->next;
   else
     list->head = tasknode->next;

   /* destroy the information contained in the node */
   destroy_task_info(tasknode->data);
   free(tasknode);
}

/* FUNCTION remove_task_node ****************************************

   PURPOSE

   Destroys a task_node data structure and removes it from the list
   it is a part of, if it is in one.  Destroys all components of the
   task_node structure, too.

   SAMPLE CALL

   destroy_task_node(tasklist, tasknode);

   INPUTS

   tasklist ---
   tasknode --- A pointer to the task_node structure to be deleted.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

*/
void remove_task_node(struct task_list *list,
		       struct task_node *tasknode)
{
   /* decrement the size of the list */
   --list->size;

   /* if there is a next node, make it point to the previous of this */
   if (tasknode->next != NULL) {
      list->current = tasknode->next;
      tasknode->next->prev = tasknode->prev;
   } else {
      list->tail = tasknode->prev;
      list->current = tasknode->prev;
   }

   /* if there is a previous node, make it point to the next of this */
   if (tasknode->prev != NULL)
     tasknode->prev->next = tasknode->next;
   else
     list->head = tasknode->next;

   free(tasknode);
}

void destroy_task_list(struct task_list *list)
{
   struct task_node *next;

   list->current = list->head;
   while (list->current) {
      next = list->current->next;
      destroy_task_node(list, list->current);
      list->current = next;
   }
   list->head = list->tail = NULL;
}

/* FUNCTION create_task_list ****************************************

   PURPOSE

   Creates a task_list data structure given all of the parameters
   needed for it.

   SAMPLE CALL

   tasklist = create_task_list(head, tail, current);

   INPUTS

   head --- A pointer to the head of the task list to be created.

   tail --- A pointer to the tail of the task list to be created.

   current --- A pointer to the current node of the task list to be
   created.

   OUTPUTS

   tasklist --- A pointer to the newly created task_list structure.

   PERTINENT INFORMATION

   Often called with head, tail, and current all equal to NULL, to
   create a new and empty list.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct task_list *create_task_list(struct task_node *head,
				   struct task_node *tail,
				   struct task_node *current)
{
   struct task_list *new_list;

   new_list = (struct task_list *) malloc(sizeof(struct task_list));

   new_list->head = head;
   new_list->tail = tail;
   new_list->current = current;
   new_list->size = 0;

   return new_list;
}

/* FUNCTION add_task_to_beginning ****************************************

   PURPOSE

   Adds a task to the beginning of a task list.
   
   SAMPLE CALL

   add_task_to_beginning(list, task);

   INPUTS

   list --- The list that the task will be added to.

   task --- The task to be added to the beginning of the list.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lactey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
void add_task_to_beginning(struct task_list *list, 
			   struct task_node *task)
{
   add_task_before(list, list->head, task);
}

/* FUNCTION add_task_to_end *****************************************

   PURPOSE

   Adds a task to the end of a task list.

   SAMPLE CALL

   add_task_to_end(struct task_list *list, struct task_node *task);

   INPUTS

   list --- The list that the task will be added to.

   task --- The task to be added to the end of the list.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester ... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
void add_task_to_end(struct task_list *list, struct task_node *task)
{
   add_task_after(list, list->tail, task);
}

/* FUNCTION add_task_before ****************************************

   PURPOSE

   Adds a task before another task in a list.

   SAMPLE CALL

   add_task_before(list, ref_task, new_task);

   INPUTS

   list --- The list to be added to.

   ref_task --- The reference task that we are adding before.

   new_task --- The task to be added before ref_task.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

   12 Nov 92 Mark M. Lacey
   Bug fix.  Previously if the new task was added to the head of the
   list, the list head pointer was not updated --- now it is.

*/
void add_task_before(struct task_list *list, struct task_node
		     *ref_task, struct task_node *new_task)
{
   /* make the newest task the current task */
   list->current = new_task;
   
   /* make the task after the new one be the reference task that we */
   /* are adding the new task before */
   new_task->next = ref_task;

   if ( ref_task ) {
      new_task->prev = ref_task->prev;

      /* if the reference task doesn't have a predecessor, then it is */
      /* the head of the list, and therefore our new node will be the */
      /* new head */
      if ( ref_task->prev == NULL ) {
	 list->head = new_task;
      } else {
	 ref_task->prev->next = new_task;
      }
      ref_task->prev = new_task;
   } else {
      /* if the task sent to us is NULL, then we must be starting a */
      /* new list, so set the head and tail to this new node, and set */
      /* the previous pointer to NULL */
      list->head = new_task;
      list->tail = new_task;
      new_task->prev = NULL;
   }
   ++list->size;
}

/* FUNCTION add_task_after ****************************************

   PURPOSE

   Adds a task after another task in a list.

   SAMPLE CALL

   add_task_after(list, ref_task, new_task);

   INPUTS

   list --- The list to be added to.

   ref_task --- The reference task that we are adding after.

   new_task --- The task to be added after ref_task.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

   12 Nov 92 Mark M. Lacey
   Bug fix.  Previously if the new task was added to the tail of the
   list, the list tail pointer was not updated --- now it is.

*/
void add_task_after(struct task_list *list, struct task_node *ref_task,
		    struct task_node *new_task)
{
   /* make the newest task the current task */
   list->current = new_task;

   /* make the task before the new one be the reference task that we */
   /* are adding the new task after */
   new_task->prev = ref_task;

   if ( ref_task ) {
      new_task->next = ref_task->next;

      /* if the reference task is the tail of the list, make our new */
      /* node the new tail of the list */
      if ( ref_task->next == NULL ) {
	 list->tail = new_task;
      } else {
	 ref_task->next->prev = new_task;
      }
      ref_task->next = new_task;
   } else {
      /* if the task sent to us is NULL, then we must be starting a */
      /* new list, so set the head and tail to this new node, and set */
      /* the previous pointer to NULL */
      list->head = new_task;
      list->tail = new_task;
      new_task->next = NULL;
   }
   ++list->size;
}

/* FUNCTION find_task ****************************************

   PURPOSE

   Given a task list to search through, and a task name to find, this
   function finds it.

   SAMPLE CALL

   tasknode = find_task(list, name);

   INPUTS

   list --- The list to be searched.

   name --- The task name we are searching for.

   OUTPUTS

   tasknode --- A pointer to the task in the list with the name given,
   or NULL if it is not found.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct task_node *find_task(struct task_list *list, char *name)
{
   list->current = list->head;

   /* Search through the entire list, updating list->current each time */
   while (list->current) {
      if (strcasecmp(list->current->data->name, name) == 0) {
	 return list->current;
      }
      list->current = list->current->next;
   }

   /* If we didn't find it in the list, return NULL */
   return NULL;
}

/* FUNCTION list_current ****************************************

   PURPOSE

   Returns a pointer to the last node in the list that was referenced
   (i.e. the current node).

   SAMPLE CALL

   current = list_current(list);

   INPUTS

   list --- The list that we want to find the current node for.

   OUTPUTS

   current --- The current node of the given list.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor... 
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct task_node *list_current(struct task_list *list)
{
   return list->current;
}

/* FUNCTION list_next ****************************************

   PURPOSE

   Returns a pointer to the node after the current one in the list,
   and sets the current one to this next node.

   SAMPLE CALL

   next = list_next(list);

   INPUTS

   list --- The list that we want the next node of.

   OUTPUTS

   next --- The next node of the list.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct task_node *list_next(struct task_list *list)
{
   /* advance to the next node unless we are already at the end */
   if ( list->current )
     list->current = list->current->next;

   /* return where we are at now */
   return list->current;
}

/* FUNCTION create_resource_info ****************************************

   PURPOSE

   Creates a resource information structure given the name of the
   resource to be added.

   SAMPLE CALL

   resinfo = create_resource_info(resname);

   INPUTS

   resname --- The name of the resource to add.

   OUTPUTS

   resinfo --- A pointer to a resource info structure with the added
   information.
   
   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor... 
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
struct resource_info *create_resource_info(char *name)
{
   struct resource_info *new_info;

   new_info = (struct resource_info *) malloc(sizeof(struct resource_info));

   new_info->resource = string_table_insert(resource_table, name,
					    strlen(name));

   return new_info;
}

/* FUNCTION destroy_resource_info ********************************************

   PURPOSE

   Destroys a resource_info structure.

   SAMPLE CALL

   destroy_resource_info(resinfo);

   INPUTS

   resinfo --- A pointer to the resource info structure to be
   destroyed.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 7 Nov 1992
   Auditor...
   Tester.... Michael Marlow 11-16-92

   MODIFICATIONS (most recent to least)

*/
void destroy_resource_info(struct resource_info *resinfo)
{
   free(resinfo);
}

/***************************************************************************/
/********************** NEW RESOURCE FUNCTIONS *****************************/
/***************************************************************************/


/* FUNCTION create_resource_node *******************************************
*
*   PURPOSE
*
*   Creates a resource_node data structure given all of the parameters
*   needed for it.
*
*   SAMPLE CALL
*
*   newnode = create_resource_node(data, prev, next);
*
*   INPUTS
*
*   data --- A pointer to a resource_info node, most likely created with
*   create_resource_info.
*
*   prev --- A pointer to the previous node on the list.  
*
*   data --- A pointer to the next node on the list. 
*
*   OUTPUTS
*
*   newnode --- A pointer to the resource_node that was created.  This will
*   be set up to include the information given in the call.
*
*   PERTINENT INFORMATION
*
*   Often called with prev and next equal to NULL when creating a new
*   node that isn't yet a part of a list.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow 11-16-92
*   Auditor...
*   Tester.... Michael Marlow
*
*   MODIFICATIONS (most recent to least)
*
*/

struct resource_node *create_resource_node(struct resource_info *data,
                                   struct resource_node *prev,
                                   struct resource_node *next)
{
   struct resource_node *new_node;
   new_node = (struct resource_node *) malloc(sizeof(struct resource_node));
   new_node->data = data;
   new_node->prev = prev;
   new_node->next = next;
   return new_node;
}

/* FUNCTION destroy_resource_node ****************************************
*
*   PURPOSE
*
*   Destroys a task_node data structure and removes it from the list
*   it is a part of, if it is in one.  Destroys all components of the
*   task_node structure, too.
*
*   SAMPLE CALL
*
*   destroy_task_node(tasknode);
*
*   INPUTS
*
*   tasknode --- A pointer to the task_node structure to be deleted.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow 11-16-92
*   Auditor...
*   Tester.... Michael Marlow
*
*/

void destroy_resource_node(struct resource_node *resourcenode)
{
   /* if there is a next node, make it point to the previous of this */
   if (resourcenode->next != NULL)
     resourcenode->next->prev = resourcenode->prev;

   /* if there is a previous node, make it point to the next of this */
   if (resourcenode->prev != NULL)
     resourcenode->prev->next = resourcenode->next;

   /* destroy the information contained in the node */
   destroy_resource_info(resourcenode->data);
   free(resourcenode);
}

/* FUNCTION create_resource_list ****************************************
*
*   PURPOSE
*
*   Creates a resource_list data structure given all of the parameters
*   needed for it.
*
*   SAMPLE CALL
*
*   resourcelist = create_resource_list(head, tail, current);
*
*   INPUTS
*
*   head --- A pointer to the head of the resource list to be created.
*
*   tail --- A pointer to the tail of the resource list to be created.
*
*   current --- A pointer to the current node of the resource list to be
*   created.
*
*   OUTPUTS
*
*   resourcelist --- A pointer to the newly created resource_list structure.
*
*   PERTINENT INFORMATION
*
*   Often called with head, tail, and current all equal to NULL, to
*   create a new and empty list.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow  11-16-92
*   Auditor...
*   Tester.... Michael Marlow
*
*   MODIFICATIONS (most recent to least)
*/
struct resource_list *create_resource_list(struct resource_node *head,
                                   struct resource_node *tail,
                                   struct resource_node *current)
{
   struct resource_list *new_list;
   new_list = (struct resource_list *) malloc(sizeof(struct resource_list));
   new_list->head = head;
   new_list->tail = tail;
   new_list->current = current;
   return new_list;
}

/* FUNCTION add_resource_to_beginning ****************************************
*
*   PURPOSE
*
*   Adds a resource to the beginning of a resource list.
*   
*   SAMPLE CALL
*
*   add_resource_to_beginning(list, resource);
*
*   INPUTS
*
*   list --- The list that the resource will be added to.
*
*   resource --- The resource to be added to the beginning of the list.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow 11-16-92
*   Auditor...
*   Tester.... Michael Marlow
*
*   MODIFICATIONS (most recent to least)
*/

void add_resource_to_beginning(struct resource_list *list, 
                           struct resource_node *resource)
{
   add_resource_before(list, list->head, resource);
}

/* FUNCTION add_resource_to_end *****************************************
*
*   PURPOSE
*
*   Adds a resource to the end of a resource list.
*
*   SAMPLE CALL
*
*   add_resource_to_end(struct resource_list *list, struct resource_node *resource);
*
*   INPUTS
*
*   list --- The list that the resource will be added to.
*
*   resource --- The resource to be added to the end of the list.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow 11-16-92
*   Auditor...
*   Tester ... Michael Marlow
*
*   MODIFICATIONS (most recent to least)
*/

void add_resource_to_end(struct resource_list *list, struct resource_node *resource)
{
   add_resource_after(list, list->tail, resource);
}

/* FUNCTION add_resource_before ****************************************
*
*   PURPOSE
*
*   Adds a resource before another resource in a list.
*
*   SAMPLE CALL
*
*   add_resource_before(list, ref_resource, new_resource);
*
*   INPUTS
*
*   list --- The list to be added to.
*
*   ref_resource --- The reference resource that we are adding before.
*
*   new_resource --- The resource to be added before ref_resource.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow 11-16-92
*   Auditor...
*   Tester.... Michael Marlow
*
*   MODIFICATIONS (most recent to least)
*/

void add_resource_before(struct resource_list *list, struct resource_node
                     *ref_resource, struct resource_node *new_resource)
{
   /* make the newest resource the current resource */
   list->current = new_resource;
   /* make the resource after the new one be the reference resource that we */
   /* are adding the new resource before */
   new_resource->next = ref_resource;
   if ( ref_resource ) {
      new_resource->prev = ref_resource->prev;
      /* if the reference resource doesn't have a predecessor, then it is */
      /* the head of the list, and therefore our new node will be the */
      /* new head */
      if ( ref_resource->prev == NULL ) {
         list->head = new_resource;
      }
      ref_resource->prev = new_resource;
   } else {
      /* if the resource sent to us is NULL, then we must be starting a */
      /* new list, so set the head and tail to this new node, and set */
      /* the previous pointer to NULL */
      list->head = new_resource;
      list->tail = new_resource;
      new_resource->prev = NULL;
   }
}

/* FUNCTION add_resource_after ****************************************
*
*   PURPOSE
*
*   Adds a resource after another resource in a list.
*
*   SAMPLE CALL
*
*   add_resource_after(list, ref_resource, new_resource);
*
*   INPUTS
*
*   list --- The list to be added to.
*
*   ref_resource --- The reference resource that we are adding after.
*
*   new_resource --- The resource to be added after ref_resource.
*
*   AUTHOR/AUDITOR/TESTER
*
*   Author.... Michael Marlow 11-16-92
*   Auditor...
*   Tester.... Michael Marlow
*
*   MODIFICATIONS (most recent to least)
*
*/

void add_resource_after(struct resource_list *list, struct resource_node *ref_resource,
                    struct resource_node *new_resource)
{
   /* make the newest resource the current resource */
   list->current = new_resource;
   /* make the resource before the new one be the reference resource that we */
   /* are adding the new resource after */
   new_resource->prev = ref_resource;
   if ( ref_resource ) {
      new_resource->next = ref_resource->next;
      /* if the reference resource is the tail of the list, make our new */
      /* node the new tail of the list */
      if ( ref_resource->next == NULL ) {
         list->tail = new_resource;
      }
      ref_resource->next = new_resource;
   } else {
      /* if the resource sent to us is NULL, then we must be starting a */
      /* new list, so set the head and tail to this new node, and set */
      /* the previous pointer to NULL */
      list->head = new_resource;
      list->tail = new_resource;
      new_resource->next = NULL;
   }
}



