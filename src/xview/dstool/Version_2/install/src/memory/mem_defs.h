/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
/*
 * paw  11/9/90
 *
 * dstool
 *
 * typedefs and declarations for memory data object
 *

   Memory data objects are specialized storage objects for working with
   "trajectory type" data.

 Concept:

 Each physical trajectory is stored as a linked list of TRAJ_ITEMs.
	A TRAJ_ITEM points to a fixed size array of points, and
	another TRAJ_ITEM if the array does not contain the full
	trajectory.

 A physical flow is a set of trajectories (ie multiple orbits commencing
	simultaneously).  A flow is represented as a FLOW_ITEM which
	contains a pointer to an array of TRAJ_ITEMs.  A FLOW_ITEM also
	contains information on the color coding scheme and the parameter
	values for the flow.

 FLOW_ITEMs are strung together as a linked list to create a numerical 
	description of a phase space.

 */

#define TOO_SMALL_MEM 0
#define MINIMUM_MEMORY 2000

typedef struct traj_list_item {
  double	*dpoints;		/* ptr to the double data arrays */
  int		*ipoints;		/* ptr to the integer data arrays */
  int		ddim,idim;		/* num of doubles and integers per point */
  int		num_points;		/* # of points in array */
  int		cur_point;		/* current # used */
  struct traj_list_item *next_seg;	/* ptr to the continuation of this data */
} TRAJ_ITEM;

typedef struct flow_list_item {
  int		num_trajs,		/* num of trajectories in the flow */
                requested_length,	/* expected length of traj */
		num_dparams,		/* length of the double param array */
		num_iparams;		/* length of the integer param array */
  double	*dparams;		/* array of double params for flow item */
  int		*iparams;		/* array of integer params for flow item */
  TRAJ_ITEM	**trajs;		/* pointer to the array of traj_items */
  struct flow_list_item	*next_flow;	/* pointer to the array of traj_items */
} FLOW_ITEM;

typedef struct mem_list_item {
  void 		*ptr;		/* address of memory */
  int		num_bytes;        /* size of space */
  struct mem_list_item *next; /* next empty space */
} MEM_ITEM;

typedef struct memory_obj {
  int		mem_type;
  FLOW_ITEM	*flow_list;
  FLOW_ITEM	*cur_flow;
  TRAJ_ITEM	*cur_traj;
  int		cur_traj_num;
  MEM_ITEM	*free_mem_list;
  MEM_ITEM	*total_mem_list;
  int		default_traj_length;
  int		too_small_mem;
  int           chunk_size;
  int           varb_dim;
  int           param_dim;
  int           color_dim;
  FLOW_ITEM	*read_flow;
  int		read_traj_num;
  TRAJ_ITEM	*read_traj_seg;
  int           read_point;
  int           stored_points;
} MEMORY_OBJ , *Memory;


/* definitions for memory routines */
#define NOWHERE 0
#define HEADER 1
#define BODY 2


