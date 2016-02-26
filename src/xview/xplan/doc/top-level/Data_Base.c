/* Top Level Code: Group 7 Data_Base.c */

/*   All of the functions in this file are used for creating, editing,
*  managing, and searching the xplan database for tasks, and task realted
*  information and relationships. All of the task lists are doubly linked
*  lists of task_nodes.  A task node contains pointers to the tasks position
*  in the list, and the structure storing the task information.  Thus, any
*  task knows its exact position in its particular list.  
*    Since many different tasks may use the same resource, a hash table is
*  created and maintained for all of the resources.  If a particular resource
*  is searched for and not found in the resources hash table, then it will
*  be added to it.  The collision technique of linear chaining is used for
*  maintaining the hash table, so an unknown number of PERT/Gantt project
*  resources may be dealt with.
*     Header files which are needed for the database functions managing the
*  doubly linked task list are "Data_Base.c".  Header files needed for 
*  managing the hashing functions for the task resources are "Resource_Hash_Table.h"
*     The data base functions will be called by the user's interaction with
*  the xplan Graphical User Interface (GUI).  The user will give the command
*  to create and edit tasks, and task lists with associated information such
*  as resources.  Also, the ASCII text file input module will call the functions
*  to create and add to the linked list.  The Calculations module will call
*  funtions to search for tasks and task lists.  Only the user's interaction
*  with the GUI may delete task and task list related information.
*
*/

#include <stdio.h>
#include "db.h"

static struct task_list *main_task_list;
static struct string_hash_table *resources_hash_table;

/* FUNCTION get_main_task_list ***********************************************/

struct task_list *get_main_task_list()
{
   return main_task_list;
}

/* FUNCTION create_task_info *************************************************/

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
				   unsigned x,
				   unsigned y,
				   unsigned length)
{
   struct task_info *new_info;

   new_info = (struct task_info *) malloc(sizeof(struct task_info));

   new_info->name = (char *) malloc(strlen(name)+1);
   strcpy(new_info->name, name);

   new_info->desc = (char *) malloc(strlen(desc)+1);
   strcpy(new_info->desc, desc);

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
   new_info->x = x;
   new_info->y = y;
   new_info->length = length;

   return new_info;
}

/* FUNCTION destroy_task_info *************************************************/

void destroy_task_info(struct task_info *old)
{
   free(old->name);
   free(old->desc);
   free(old);
}

/* FUNCTION create_task_node *************************************************/

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

/* FUNCTION destroy_task_node ************************************************/
 
void destroy_task_node(struct task_node *old)
{
   free(old);
}

/* FUNCTION create_task_list *************************************************/

struct task_list *create_task_list(struct task_node *head,
				   struct task_node *tail,
				   struct task_node *current)
{
   struct task_list *new_list;

   new_list = (struct task_list *) malloc(sizeof(struct task_list));

   new_list->head = head;
   new_list->tail = tail;
   new_list->current = current;

   return new_list;
}

/* FUNCTION add_task_to_beginning ********************************************/

void add_task_to_beginning(struct task_list *list, 
			   struct task_node *task)
{
   add_task_before(list, list->head, task);
}

/* FUNCTION add_task_to_end **************************************************/

void add_task_to_end(struct task_list *list, struct task_node *task)
{
   add_task_after(list, list->tail, task);
}

/* FUNCTION add_task_before **************************************************/

void add_task_before(struct task_list *list, struct task_node
		     *old_task, struct task_node *new_task)
{
   /* make the newest task the current task */
   list->current = new_task;

   new_task->next = old_task;
   if ( old_task ) {
      new_task->prev = old_task->prev;
      old_task->prev = new_task;
   } else {
      /* if the task sent to us is NULL, then we must be starting a */
      /* new list, so set the head and tail to this new node, and set */
      /* the previous pointer to NULL */
      list->head = new_task;
      list->tail = new_task;
      new_task->prev = NULL;
   }
}

/* FUNCTION add_task_after **************************************************/

void add_task_after(struct task_list *list, struct task_node *old_task,
		    struct task_node *new_task)
{
   /* make the newest task the current task */
   list->current = new_task;

   new_task->prev = old_task;
   if ( old_task ) {
      new_task->next = old_task->next;
      old_task->next = new_task;
   } else {
      /* if the task sent to us is NULL, then we must be starting a */
      /* new list, so set the head and tail to this new node, and set */
      /* the previous pointer to NULL */
      list->head = new_task;
      list->tail = new_task;
      new_task->next = NULL;
   }
}

/* FUNCTION find_task ********************************************************/

struct task_node *find_task(struct task_list *list, char *name)
{
   list->current = list->head;

   /* Search through the entire list, updating list->current each time */
   while ( list->current ) {
      if ( strcasecmp(list->current->data->name, name) == 0 ) {
	 return list->current;
      }
      list->current = list->current->next;
   }

   /* If we didn't find it in the list, return NULL */
   return NULL;
}

/* FUNCTION remove_task *****************************************************/
/*    All tasks are represented in a doubly linked list. Each task knows the
*  node before it, and the node after it.  Thus, to delete a task from a linked
*  all you need is a pointer to that task, and this function will adjust for
*  the new pointer values when the task is removed.  This function is called
*  to delete a task from any doubly linked task list.
*/

void remove_task (struct task_node *task )
{
   task->prev->next = task->next; /* remove tasks backwards link */
   task->next->prev = task->prev; /* remove tasks forwards link  */

/* FUNCTION list_current ****************************************************/

struct task_node *list_current(struct task_list *list)
{
   return list->current;
}

/* FUNCTION list_next *******************************************************/

struct task_node *list_next(struct task_list *list)
{
   /* advance to the next node unless we are already at the end */
   if ( list->current )
     list->current = list->current->next;

   /* return where we are at now */
   return list->current;
}

/* FUNCTION create_resource_info *********************************************/

struct resource_info *create_resource_info(char *name)
{
   struct resource_info *new_info;

   new_info = (struct resource_info *) malloc(sizeof(struct resource_info));

   new_info->name = string_hash_search(resources_hash_table, name);
   if (new_info->name == NULL) {
      new_info->name = string_hash_insert(resources_hash_table, name);
   }

   return new_info;
}

/* FUNCTION destroy_resource_info ********************************************/

void destroy_resource_info(struct resource_info *old)
{
   free(old);
}

/* Hash table package for string tables 
*
* All of the resources for the PERT/Gantt chart project will be stored
* in a hash table to save space.  For example, a single task may have 
* many resources, but those same resources may be used by other tasks.
* A resource is stored in memory as a character string. Linear Chaining
* is the collision resolution technique use for the Hash table.
*/

#include <stdio.h>
#include "string_table.h"

/*
 * Create a string table of the given size "nelms".
 */
struct string_table *string_table_create(unsigned nelms)
{
   struct string_table *new_table;

   new_table = (struct string_table *) malloc(sizeof(struct string_table));

   new_table->nelms = nelms;
   new_table->nodes = (struct string_node **) malloc(nelms * sizeof(struct
                                                                  string_node *));

   return new_table;
}

/*
 * Initialize the table by setting all the list heads to NULL.
 */
void string_table_initialize(struct string_table *tableptr)
{
   unsigned index;

   for (index = 0; index < tableptr->nelms; ++index) {
      tableptr->nodes[index] = NULL;
   }
}

/*
 * Free up the space used by a string table.
 */
void string_table_destroy(struct string_table *tableptr)
{
   /* Free the node pointers */
   free(tableptr->nodes);
   /* Free the table struct itself */
   free(tableptr);
}

/*
 * Empty out a string table previously created
 */
void string_table_empty(struct string_table *tableptr)
{
   unsigned index;
   struct string_node *node, *next;

   /* For each list in the table, clear the list */
   for(index = 0; index < tableptr->nelms; ++index) {
      node = tableptr->nodes[index];
      /* For each node in the list, free memory for the string and */
      /* then the node itself */
      while (node) {
       next = node->next;
       free(node->name);
       free(node);
       node = next;
      }
   }
}

/*
 * Find the list that a string should appear in
 */
unsigned string_table_hash(char *name, unsigned namelen, unsigned nelms)
{
   if (namelen == 0) {
      return 0;
   } else {
      return ((name[0]<<8)+name[namelen-1]) % nelms;
   }
}


/*
 * Search for an entry in the string table.  If it is found,
 * return a pointer to it.  Otherwise, add it to the table and return
 * a pointer to the new entry.
 */
struct string_node *string_table_insert(struct string_table *tableptr, char *name,
                                      unsigned namelen)
{
   unsigned index;
   struct string_node *traverse, *current;

   index = string_table_hash(name, namelen, tableptr->nelms);

   /* If this is not the head of this list... */
   if (tableptr->nodes[index]) {
      traverse = tableptr->nodes[index];

      while (traverse) {
       current = traverse;
       if (namelen == traverse->namelen) {
          if (strncmp(name, traverse->name, namelen) == 0) {
             return traverse;
          }
       }
       traverse = traverse->next;
      }

      /* Since it wasn't found, insert it */
      current->next = (struct string_node *) malloc(sizeof(struct string_node
                                                         ));
      current = current->next;

      current->namelen = namelen;
      current->name = (char *) malloc(namelen+1);
      if (namelen != 0) {
       strncpy(current->name, name, namelen);
      }
      current->name[namelen] = '\0';
      current->name = NULL;
   } else {
      tableptr->nodes[index] = (struct string_node *) malloc(sizeof(struct
                                                                  string_node));
      tableptr->nodes[index]->namelen = namelen;
      tableptr->nodes[index]->name = (char *) malloc(namelen+1);
      if (namelen != 0) {
       strncpy(tableptr->nodes[index]->name, name, namelen);
      }
      tableptr->nodes[index]->name[namelen] = '\0';
      tableptr->nodes[index]->next = NULL;

      return tableptr->nodes[index];
   }
}

/*
 * Search for an entry in the string table.  If it is found,
 * return a pointer to it.  Otherwise, return NULL.
 */
struct string_node *string_table_search(struct string_table *tableptr, char *name,
                                      unsigned namelen)
{
   unsigned index;
   struct string_node *traverse;

   index = string_table_hash(name, namelen, tableptr->nelms);

   /* If this is not the head of this list... */
   if (tableptr->nodes[index]) {
      traverse = tableptr->nodes[index];

      while (traverse) {
       if (namelen == traverse->namelen) {
          if (strncmp(name, traverse->name, namelen) == 0) {
             return traverse;
          }
       }
       traverse = traverse->next;
      }

      return NULL;
   } else {
      return NULL;
   }
}
