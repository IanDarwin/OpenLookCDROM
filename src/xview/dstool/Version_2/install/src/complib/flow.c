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
 * flow.c
 *
 * Procedures:
 *   flow_forwards();
 *   flow_backwards();
 *   flow_continue();
 *   clear_last_trajectory();
 *   clear_all_trajs();
 *
 */
#include <stdio.h>
#include <pm.h>
#include <constants.h>
#include <memory.h>

void
  flow_forwards()
{
  pm(PUT, "Flow.Direction", FORWARD, NULL);
  traj_mem_orbit();
}

void
  flow_backwards()
{
  pm(PUT, "Flow.Direction", BACKWARD, NULL);
  traj_mem_orbit();
}

void
  flow_continue()
{
  traj_mem_orbit();
}


/*
 * routine to clear the last trajectory in the trajectory memory object
 */
void
clear_last_trajectory()
{
  memory m;

  m = (memory) pm(GET, "Memory.Traj", NULL);
  memory_delete_last_flow(m);
}


/*
 * clear_all_trajs()  clears the trajectory memory object
 *                     the viewing windows
 */
void
  clear_all_trajs()
{
  pm(INIT, "Memory.Traj", TRAJ_MEMORY, 
     INIT, "Memory.Mult", MULT_MEMORY,
     NULL);
}
