/* Top Level Code: Group 7 Data_Base.h */

/*   This is the header file declaring the data structures and functions
*  used by the database functions in the "Data_Base.c" file.  For a description
*  of how the xplan database is created, managed, and used, see the comments 
*  at the beginning of the file "Data_Base.c".
*/


#ifndef _db_h_
#define _db_h_

/* #define's and enumerated types */

#define MAXNAMELEN 60
#define MAXDESCLEN 800

enum boolean {false, true};

/* the information for each task */
struct task_info {
   char *name;                   /* pointer to the name of the task */
   char *desc;                   /* pointer to task description */
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

   unsigned x-pert, y-pert;        /* starting position of PERT box */
   unsigned x-gantt, y-gantt;      /* starting position of Gantt bar */
   unsigned length;                /* Length of a Gantt bar */
   
   unsigned number_of_visits;      /* */
   enum boolean visit;             /* determine whether a node has been */
                                   /* visited by visit-task() procedure */

}; /* end of struct task_info */

/* a node in our task list */
struct task_node {
   struct task_info *data;            /* pointer to the task */
				      /* information for this node */
   struct task_node *prev;            /* pointer to the previous node */
				      /* in the list */
   struct task_node *next;            /* pointer to the next node in */
				      /* the list */
};

/* a task list */
struct task_list {
   struct task_node *head;            /* the head of the task list */
   struct task_node *tail;            /* the tail of the task list */
   struct task_node *current;         /* the node last accessed */
};

struct string_node {                  /* for the hashing */
   char *str;
   unsigned length;
};

struct string_table {
   struct string_node *table;
   unsigned size;
};

struct resource_info {
   struct string_node *name;
};

struct resource_node {
   struct resource_info *data;        /* pointer to the resource */
				      /* information for this node */
   struct resource_node *prev;        /* pointer to the previous node */
				      /* in the list */
   struct resource_node *next;        /* pointer to the next node in */
				      /* the list */
};

struct resource_list {
   struct resource_node *head;        /* head of the resource list */
   struct resource_node *tail;        /* talk of the resource list */
   struct resource_node *current;     /* the node last accessed */
};

struct date {
  short year;
  short month;
  short day;
};

/*
 * Function prototypes for all the functions in the database library.
 *
 */
struct task_info *create_task_info(char *, 
				   char *, 
				   unsigned, 
				   unsigned, 
				   unsigned, 
				   unsigned, 
				   unsigned, 
				   unsigned, 
				   unsigned,
				   unsigned, 
				   unsigned, 
				   unsigned, 
				   unsigned, 
				   unsigned,
				   enum boolean, 
				   enum boolean, 
				   struct resource_list *, 
				   struct task_list *, 
				   struct task_node *,
				   struct task_list *,
				   struct task_list *,
				   unsigned,
				   unsigned,
				   unsigned,
				   unsigned);
void destroy_task_info(struct task_info *);
struct task_node *create_task_node(struct task_info *,
				   struct task_node *,
				   struct task_node *);
void destroy_task_node(struct task_node *);
struct task_list *create_task_list(struct task_node *,
				   struct task_node *,
				   struct task_node *);
void add_task_to_beginning(struct task_list *, 
			   struct task_node *);
void add_task_to_end(struct task_list *,
		     struct task_node *);
void add_task_before(struct task_list *, 
		     struct task_node *, 
		     struct task_node *);
void add_task_after(struct task_list *,
		    struct task_node *,
		    struct task_node *);
struct task_node *find_task(struct task_list *,
			    char *);
struct task_node *list_current(struct task_list *);
struct task_node *list_next(struct task_list *);
struct resource_info *create_resource_info(char *);
void destroy_resource_info(struct resource_info *);
struct string_hash_node *string_hash_search(struct string_hash_table
					    *, char *);
struct string_hash_node *string_hash_insert(struct string_hash_table
					    *, char *);
struct task_list *get_main_task_list();

#endif _db_h_




