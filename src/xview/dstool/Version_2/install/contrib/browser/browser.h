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


#define INIT_FLOW_NUM 0
#define INIT_TRAJ_NUM 0
#define INIT_POINT_NUM 0

typedef struct {
    int pm_mem_type;
    int flow_num;
    int new_flow_num;
    int n_flows;
    int flow_read_status;
    int traj_num;
    int new_traj_num;
    int n_trajs;
    int traj_read_status;
    int point_num;
    int new_point_num;
    int n_points;
    int point_read_status;
    int mem_read_status;
} Mem_Indices;

extern Mem_Indices
    mem_indices;
