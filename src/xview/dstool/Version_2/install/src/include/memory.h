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
 * modified: 12/14/92
 *
 * dstool
 *
 * declarations for memory data object
 *

   Memory data objects are specialized storage objects for working with
   "trajectory type" data.

   TO USE THE MEMORY OBJECT, INCLUDE THE FILE:  memory.h

 *
 */

#ifndef memory_DEFINED
#define memory_DEFINED

/* hide the real definition of Memory , it's just a pointer */
typedef void *memory;

extern 
memory memory_create();
/*
  memory
  memory_create(type, size, def_traj_length,  varb_dim, param_dim, color_dim)
  int type, size, def_traj_length, varb_dim, param_dim, color_dim;

  This procedure creates a new memory object.

    type specifies the type of memory object to be created
    size gives the size of the requested heap allocations
    def_traj_length is the default length for a trajectory
    varb_dim is the dimension of the phase space
    param_dim is the dimension of the parameter space
    color_dim is the dimension of the color vector

  The new memory object is returned if succesful,
  else NULL is returned.
 */

extern
int memory_destroy();
/*
  int
  memory_destroy(m)
  memory m;

  This routine accepts a memory object and frees all memory associated with it.
  The memory object should then be discarded.
*/


extern
int memory_start_new_flow();
/*
  int
  memory_start_new_flow(m, n_trajs, n_extra_dvars, n_extra_ivars, n_points,
                            n_extra_dparams,n_extra_iparams)
  memory m;
  int n_trajs, n_extra_dvars, n_extra_ivars, n_points,
      n_extra_dparams, n_extra_iparams;

  This routine is called to allocate space for a new flow inside a memory object.
  It must be called before adding any new points.
  
    n_trajs specifies how many trajectories will be in the flow object
    n_extra_dvars and n_extra_ivars specify whether a point in a trajectory
        will have any additional doubles or integers associated to it
	(in addition to possible variable, parameter, and color data).
    n_points specifies the expected number of points per trajectory
        (if 0, then the default trajectory length is assumed)
    n_extra_dparams and n_extra_iparams specifies whether the flow
        object will have any additional doubles or integers associated to it
	(in addition to possible variable, parameter, and color data).
  
  On failure a non-zero integer is returned.
*/


extern
int memory_vanilla_start_new_flow();
/*
  int
  memory_vanilla_start_new_flow(m, n_trajs, n_dvars, n_ivars, n_points,
                            n_dparams,n_iparams)
  memory m;
  int n_trajs, n_dvars, n_ivars, n_points,
      n_dparams, n_iparams;

  This routine is called to allocate space for a new flow inside a memory object.
  It must be called before adding any new points.
  
    n_trajs specifies how many trajectories will be in the flow object
    n_dvars and n_ivars specify the dimensions of a point in a trajectory.
    n_points specifies the expected number of points per trajectory
        (if 0, then the default trajectory length is assumed)
    n_dparams and n_iparams specify the dimension of the parameters in
         the header of the trajectories (the flow object).
  
  On failure a non-zero integer is returned.
*/


extern
int memory_end_current_flow();
/*
  int
  memory_end_current_flow(m)
  memory m;

  This routine frees up any extra memory which might have been allocated
  for the trajectories in the current flow. 
  There is usually no need to call this since memory_start_new_flow makes
  a call to this routine.
*/

extern
int memory_exists_current_flow();
/*
  int 
  memory_exists_current_flow();
  memory m;

  This routine returns 1 if there is a current flow, and 0 if
there is not a current flow.
*/


extern
  int memory_clear_flows();
/*
  int
  memory_clear_flows(m)
  memory m;

  This routine takes a memory object and clears out all the flows.
  In a sense  the object is left in the state it was in when created originally.
  However, if at some time additional heap memory was allocated to the memory
  object, then this memory is not released to the system.
  The  current method to release heap memory is to destroy the object and recreate it.

  On error, a non-zero integer is returned.  The memory object should still exist,
  but its heap memory may have been reduced.
*/


extern
  void memory_clear_selected_flows();
/*
  void
  memory_clear_selected_flows(m,flow_test);
  memory m;
  int (*flow_test)();

  This routines takes a memory object and clears some of the flows.
  The flows cleared are the ones for which flow_test returns TRUE.
  flow_test is a routine which takes extra integer parameters from
  a flow object and returns a decision.  For example:
  
          flow_test(p,n)
	  double *p;               <-- the integer parameters 
	  int n;                   <-- the number of integer parameters
	  {
	      if ( ... )  return(TRUE);
	      return(FALSE);
	  }
	  
*/


extern
int memory_dump();
/*
  int
  memory_dump(fp,m,dump_level)
  FILE *fp;
  memory m;
  int dump_level;

  This routine writes out the memory structure to the file fp.
  dump_level specifies the detail of the dump (0 top level , ... , 3 every point)
*/


extern int
memory_add_point();
/*
  int
  memory_add_point(m, p_varb, p_param, p_color, p_extra_doubles, p_extra_ints, 
                   p_extra_dparams, p_extra_iparams)
  memory m;
  double *p_varb, *p_param, *p_extra_doubles, *p_extra_dparams;
  int *p_color, *p_extra_ints, *p_extra_iparams;

  This procedure is used to add points into the memory object.
  It assumes that all the pointers are for arrays of the previously 
  specified dimensions.
  Any one of the pointers may be NULL, in which case that field is not touched.
  If any one of the pieces of data needs to be written into a trajectory, then
  a new point is created.  The alternative is that all the data belongs in 
  the header for the flow object as a whole.  In this case, no new point is
  allocated.

  It returns 0 if succesful; else 1 if it were unable to store the point due to 
  memory allocation problems.
*/


extern int
memory_vanilla_add_point();
/*
  int
  memory_vanilla_add_point(m, p_doubles, p_ints, 
                   p_dparams, p_iparams)
  memory m;
  double *p_doubles, *p_dparams;
  int *p_ints, *p_iparams;

  This procedure is used to add points into the memory object.
  It assumes that all the pointers are for arrays of the previously 
  specified dimensions.
  Any one of the pointers may be NULL, in which case that field is not touched.
  If any one of the pieces of data needs to be written into a trajectory, then
  a new point is created.  The alternative is that all the data belongs in 
  the header for the flow object as a whole.  In this case, no new point is
  allocated.

  It returns 0 if succesful; else 1 if it were unable to store the point due to 
  memory allocation problems.
*/



extern int
memory_add_points();
/*
  int
  memory_add_points(m, n, p_varb_seg, p_param_seg, p_color_seg, 
	  	    p_extra_doubles_seg, p_extra_ints_seg);
  memory m;
  int n, **p_color_seg, **p_extra_ints_seg;
  double **p_varb_seg, **p_param_seg, **p_extra_doubles_seg

  This procedure adds an array of points into the memory object.
  Any of the pointers to segments may be NULL if these fields are
  not to be written into or are nonexistent.

  The procedure assumes that the array of doubles has the
  correct dimension.  If not, look for trouble!

  It returns 0 if succesful; else 1 if it were unable
  to store all the points due to memory allocation problems.
*/


extern int
memory_next_traj();
/*
  int
  memory_next_traj(m);
  memory m;

  This procedure tells the memory object that the next
  add points calls are intended for next trajectory in
  the current flow object.
*/


extern int
memory_set_cur_traj();
/*
  int
  memory_set_traj(m,i);
  memory m;
  int i;

  This procedure sets the current trajectory in the current flow.
  The current range is 1 <= i <= memory_ntrajs(). 
  It returns 0 if succesful and 1 if there is no such trajectory!
*/


extern int
memory_get_first_point();
/*
  int
  memory_get_first_point(m, pp_varb, pp_param, pp_color, pp_extra_doubles, pp_extra_ints,
                         pp_extra_dparams, pp_extra_iparams);
  memory m;
  double **pp_varb, **pp_param, **pp_extra_doubles, **pp_extra_dparams;
  int **pp_color, **pp_extra_ints, **pp_extra_iparams;

  Returns pointers to the elements of the first point in memory.
  If any of the arguments are NULL, then this info is not returned.

  The returned value is 0 if succesful, and 1 if there are no points in 
  the memory object.
*/


extern int
memory_get_last_point();
/*
  int
  memory_get_last_point(m, pp_varb, pp_param, pp_color, pp_extra_doubles, pp_extra_ints,
                         pp_extra_dparams, pp_extra_iparams);
  memory m;
  double **pp_varb, **pp_param, **pp_extra_doubles, **pp_extra_dparams;
  int **pp_color, **pp_extra_ints, **pp_extra_iparams;

  Returns pointers to the elements of the last point in memory.
  If any of the arguments are NULL, then this info is not returned.

  The returned value is 0 if succesful, and 1 if there are no points in 
  the memory object.
*/


extern int
memory_reset_read();
/*
  int
  memory_reset_read(m);
  memory m;

  This call is made to reset the memory so that all data may be read.
  This returns 0 is succesful and 1 if there are no flows to read.

*/


extern int
memory_read_next_flow();
/*
  int
  memory_read_next_flow(m, pn_trajs, p_extra_dparams, pn_extra_dparams, 
		        p_extra_iparams, pn_extra_iparams)
  memory m;
  double **p_extra_dparams;
  int *pn_extra_dparams,**p_extra_iparams, *pn_extra_iparams, *pn_trajs;

  This procedure advances the reading of memory to the next flow in
  the memory object.  It supplies the number of trajectories, the extra
  double parameters and how many, and the extra integer parameters and how many.
  If the pointers passed in are NULL, then this info is not returned.

  This procedure returns 0 if there was another flow, else 1.

*/


extern int
memory_read_next_traj();
/*
  int
  memory_read_next_traj(m, pn_points, pn_extra_doubles, pn_extra_ints)
  memory m;  
  int *pn_points, *pn_extra_doubles, *pn_extra_ints;

  This procedure advances the reading of memory to the next trajectory
  in the flow object being read.  It supplies the number of points in 
  the trajectory plus the dimension of the arrays of extra doubles and integers.
	
  It returns 0 if there was another trajectory, else returns 1.
*/


extern int
  memory_read_next_point();
/*
  int
  memory_read_next_point(m, pp_varb, pp_param, pp_color, 
		       pp_extra_doubles, pp_extra_ints)
  memory m;
  double **pp_varb, **pp_param, **pp_extra_doubles;
  int **pp_color, **pp_extra_ints;

  This procedure supplies the next point in the current trajectory being
  read from the memory object.  If any of the pointers are NULL then that
  information is not supplied.

  It returns 0 if there was a next point, else it returns 1.


  Here is an example of how to read points from the memory:

	if ( memory_reset_read(m) == 0 ) 
	  while ( memory_read_next_flow(m, &n_trajs, &dparams, &n_dparams, 
                                        &iparams, &n_iparams) == 0)
	    while ( memory_read_next_traj(m, &n_points, 
	                                  &n_extra_doubles, &n_extra_ints) == 0)
		    while ( memory_read_next_point(m, &varb, &param, &color, 
		                                   &extra_doubles, &extra_ints) == 0)
		      {
		    
		        DO SOMETHING WITH ALL THIS POINT DATA !

		      }

*/


extern int
memory_vanilla_read_next_flow();
/*
  int
  memory_vanilla_read_next_flow(m, pn_trajs, p_dparams, pn_dparams, 
		        p_iparams, pn_iparams)
  memory m;
  double **p_dparams;
  int *pn_dparams,**p_iparams, *pn_iparams, *pn_trajs;

  This procedure advances the reading of memory to the next flow in
  the memory object.  It supplies the number of trajectories, the
  double parameters and how many, and the integer parameters and how many.
  If the pointers passed in are NULL, then this info is not returned.

  This procedure returns 0 if there was another flow, else 1.

*/


extern int
memory_vanilla_read_next_traj();
/*
  int
  memory_vanilla_read_next_traj(m, pn_points, pn_doubles, pn_ints)
  memory m;  
  int *pn_points, *pn_doubles, *pn_ints;

  This procedure advances the reading of memory to the next trajectory
  in the flow object being read.  It supplies the number of points in 
  the trajectory plus the dimension of the arrays of extra doubles and integers.
	
  It returns 0 if there was another trajectory, else returns 1.
*/


extern int
  memory_vanilla_read_next_point();
/*
  int
  memory_vanilla_read_next_point(m, pp_doubles, pp_ints)
  memory m;
  double **pp_doubles;
  int **pp_ints;

  This procedure supplies the next point in the current trajectory being
  read from the memory object.  If any of the pointers are NULL then that
  information is not supplied.

  It returns 0 if there was a next point, else it returns 1.


  Here is an example of how to read points from the memory:

	if ( memory_reset_read(m) == 0 ) 
	  while ( memory_vanilla_read_next_flow(m, &n_trajs, &dparams, &n_dparams, 
                                        &iparams, &n_iparams) == 0)
	    while ( memory_vanilla_read_next_traj(m, &n_points, 
	                                  &n_doubles, &n_ints) == 0)
		    while ( memory_vanilla_read_next_point(m, &p_doubles, &p_ints) == 0)
		      {
		    
		        DO SOMETHING WITH ALL THIS POINT DATA !

		      }

*/


extern int
memory_set_read();
/*
  int
  memory_set_read(m,nflow,ntraj,npoint,
                    p_extra_dparams, pn_extra_dparams, 
		    p_extra_iparams, pn_extra_iparams, 
                    pn_extra_doubles, pn_extra_ints);
  memory m;
  int nflow,ntraj,npoint,*pn_extra_dparams,**p_extra_iparams,*pn_extra_iparams,
      *pn_extra_doubles,*pn_extra_ints;
  double **p_extra_dparams;

  This call is made to set the memory so that the next point to be
  read is point npoint of trajectory ntraj of flow nflow in memory object m.
  This returns 0 is succesful and 1 if there are no flows to read.
  If unsuccessful, then the current read pointers are left unchanged.
  Ranges for n* are 1 <= n* <= memory_n*s()  (where * is flow,traj or point).
  The remaining variables in the function call are used as in the 
  memory_read_next_flow and memory_read_next_traj routines.

*/

extern int
memory_get_type();
/* 
  int
  memory_get_type(m)
  memory m;
	
  Takes a memory objects and returns its type
	(see constants file).

*/


extern double
memory_closest_distance();
/*
  double
  memory_closest_distance(m,p,distance_f,work,manifold,flow_test)
  memory m;
  double *p, (*distance_f)(), *work;
  Manifold *manifold;
  int (*flow_test)();

  Procedure to determine the closest distance between p, and all the
  points in the memory object.  Only certain flows are checked; those
  for which flow_test returns non a non zero integer.  This procedure
  is passes in the extra_iparams from the flow header to allow it
  to make a decision.  If there are no points in valid flows, then
  the distance returned is negative, else the distance_f is used
  to determine a distance between p and the varb component of a point.

*/


extern int
memory_stored_points();
/*
  int
  memory_stored_points(m)
  memory m;

  returns the number of points stored in the memory object

*/


extern int
memory_delete_flow();
/*
  int
  memory_delete_flow(m, n)
  memory m;
  int n;

  Deletes the n'th flow from the memory object m.  
  If the selected flow is the current flow then it will also
  do a memory_end_current_flow and if the selected flow
  is the current flow for reading then memory_reset_read is called.
  The range of the flow numbers is 1<= flow_num <= memory_nflows().
*/

extern int
memory_delete_last_flow();
/*
  int
  memory_delete_last_flow(m)
  memory m;

  Deletes the last flow from the memory object m. 
  If the last flow is the current flow then it will also
  do a memory_end_current_flow and if the last flow
  is the current flow for reading then memory_reset_read is called.
*/

extern int
memory_nflows();
/*
  int
  memory_nflows(m)
  memory m;

  Returns the number of flows in the memory object m
*/

extern int
memory_ntrajs();
/*
  int
  memory_ntrajs(m,nflow)
  memory m;
  int nflow;

  Returns the number of trajectories in flow nflow in the memory object m
*/

extern int
memory_npoints();
/*
  int
  memory_npoints(m,nflow,ntraj)
  memory m;
  int nflow,ntraj;

  Returns the number of points in the trajectory ntraj
  in the flow nflow in the memory object m.
*/


#endif memory_DEFINED
