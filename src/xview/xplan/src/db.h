/* FILE db.h **********************************************************
 *
 * Author.... Mark M. Lacey (mml)
 * Date......
 *
 * Auditor... 
 * Date...... 
 *
 * FILE db.h */

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
   unsigned critical_date;         /* used in calculating critical path  */

   unsigned float_time;              /* the float time, in days */

   enum boolean milestone;           /* does this completion of this task */
				     /* mean we have hit a milstone? */
   enum boolean deliverable;         /* does the completion of this task */
                                     /* yield a deliverable? */
   enum boolean critical_path;       /* does this task belong to the */
                                     /* critical path ? */

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

   unsigned x_pert, y_pert;        /* starting position of PERT box */
   unsigned x_gantt, y_gantt;      /* starting position of Gantt bar */
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
   unsigned size;
};

struct resource_info {
   struct string_node *resource;
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
   unsigned size;                     /* number of elements in the */
				      /* list */
};

/*
*  Function prototypes for all the functions in the database library.
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
				    unsigned, 
				    unsigned,
				    unsigned);
void change_task_info(struct task_info *,
		      char *, 
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
		      unsigned,
		      unsigned,
		      unsigned);
void destroy_task_info(struct task_info *);
struct task_node * create_task_node(struct task_info *,
				   struct task_node *,
				   struct task_node *);
void destroy_task_node(struct task_list *, struct task_node *);
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
struct task_node *create_default_task(void);
struct task_node *list_current(struct task_list *);
struct task_node *list_next(struct task_list *);
struct resource_info *create_resource_info(char *);
void destroy_resource_info(struct resource_info *);
struct task_list *get_main_task_list();
void create_main_task_list();
void set_main_task_list(struct task_list *);


/***************************************************************************/
/********************** NEW RESOURCE FUNCTIONS *****************************/
/***************************************************************************/

struct resource_node * create_resource_node(struct resource_info *,
                                   struct resource_node *,
                                   struct resource_node *);
void destroy_resource_node(struct resource_node *);
struct resource_list *create_resource_list(struct resource_node *,
                                   struct resource_node *,
                                   struct resource_node *);
void add_resource_to_beginning(struct resource_list *, 
                           struct resource_node *);
void add_resource_to_end(struct resource_list *,
                     struct resource_node *);
void add_resource_before(struct resource_list *, 
                     struct resource_node *, 
                     struct resource_node *);
void add_resource_after(struct resource_list *,
                    struct resource_node *,
                    struct resource_node *);




#endif

