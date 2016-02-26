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
 * Create FLOW postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>
#include <prop.h>

static char *FLOW_OBJ_NAME = "Flow";

static char *FLOW[] = {
  "Flow.Stepsize", "Flow.Start_Save_Points",
  "Flow.Total_Iterates", "Flow.Skip_Size",
  "Flow.Diverg_Cutoff", "Flow.Direction",
  "Flow.Stopping_Condition", 
  "Flow.Varb_Events", "Flow.Funct_Events",
  "Flow.Varb_Event_Values", "Flow.Funct_Event_Values",
  "Flow.Final_Time",
  "Flow.Forwards", "Flow.Backwards", "Flow.Continue",
  "Flow.Clear_Last", "Flow.Clear_All",
  "Flow.Load_Int", "Flow.Int_Num",
  "Flow.Load_Iter", "Flow.Plot_Function",
  };

typedef enum {
  STEPSZ=0, STARTSAVE_POINTS, TOT_ITERATES,
  SKIPSZ, DIVERG, DIRECTION, STOPPING_CONDITION,
  VARB_EVENTS, FUNCT_EVENTS,
  VARB_EVENT_VALUES, FUNCT_EVENT_VALUES,
  FINAL_TIME,
  GO_FORWARDS, GO_BACKWARDS, GO_CONTINUE,
  CLEAR_LAST, CLEAR_ALL,
  LOAD_INT, INT_NUM, LOAD_ITER,
  PLOT_FUNCTION
  } FLOW_t;


void
  flow_install()
{
  void flow_forwards(), flow_backwards(), flow_continue(),
  load_int(), clear_last_trajectory(), clear_all_trajs(), map_init(),
  traj_plot();

  pm(CREATE_OBJ, FLOW_OBJ_NAME,
     CREATE_ELEM, FLOW[STEPSZ], DBL,
     CREATE_ELEM, FLOW[STARTSAVE_POINTS], INT,
     CREATE_ELEM, FLOW[TOT_ITERATES], INT,
     CREATE_ELEM, FLOW[SKIPSZ], INT,
     CREATE_ELEM, FLOW[DIVERG], DBL,
     CREATE_ELEM, FLOW[DIRECTION], INT,
     CREATE_ELEM, FLOW[STOPPING_CONDITION], INT,
     CREATE_ELEM, FLOW[VARB_EVENTS], INT_LIST,
     CREATE_ELEM, FLOW[FUNCT_EVENTS], INT_LIST,
     CREATE_ELEM, FLOW[VARB_EVENT_VALUES], DBL_LIST,
     CREATE_ELEM, FLOW[FUNCT_EVENT_VALUES], DBL_LIST,
     CREATE_ELEM, FLOW[FINAL_TIME], DBL,
     CREATE_ELEM, FLOW[GO_FORWARDS], FNCT,
     CREATE_ELEM, FLOW[GO_BACKWARDS], FNCT,
     CREATE_ELEM, FLOW[GO_CONTINUE], FNCT,
     CREATE_ELEM, FLOW[CLEAR_LAST], FNCT,
     CREATE_ELEM, FLOW[CLEAR_ALL], FNCT,
     CREATE_ELEM, FLOW[LOAD_INT], FNCT,
     CREATE_ELEM, FLOW[INT_NUM], INT,
     CREATE_ELEM, FLOW[LOAD_ITER], FNCT,
     CREATE_ELEM, FLOW[PLOT_FUNCTION], ADDRS,
     NULL);

  pm(CREATE_ELEM, "Memory.Traj", MEMRY, NULL);

  pm(INIT, FLOW[GO_FORWARDS],
     INIT, FLOW[GO_BACKWARDS],
     INIT, FLOW[GO_CONTINUE],
     INIT, FLOW[CLEAR_LAST],
     INIT, FLOW[CLEAR_ALL],
     PUT, FLOW[GO_FORWARDS], flow_forwards,
     PUT, FLOW[GO_BACKWARDS], flow_backwards,
     PUT, FLOW[GO_CONTINUE], flow_continue,
     PUT, FLOW[CLEAR_LAST], clear_last_trajectory,
     PUT, FLOW[CLEAR_ALL], clear_all_trajs,
     INIT, FLOW[LOAD_INT],
     PUT, FLOW[LOAD_INT], load_int,
     PUT, FLOW[INT_NUM], DEFAULT_INTEGRATOR,
     INIT, FLOW[LOAD_ITER],
     PUT, FLOW[LOAD_ITER], map_init,
     INIT, FLOW[PLOT_FUNCTION],
     PUT, FLOW[PLOT_FUNCTION], traj_plot,
     NULL);

}


void
  flow_reset()
{
  int n_varb, n_funct;

  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_funct = *((int *) pm(GET, "Model.Funct_Dim", NULL));

  pm(INIT, "Memory.Traj", TRAJ_MEMORY,
     INIT, "Flow.Varb_Events", n_varb, 
     INIT, "Flow.Funct_Events", n_funct, 
     INIT, "Flow.Varb_Event_Values", n_varb,
     INIT, "Flow.Funct_Event_Values", n_funct,
     NULL);

  pm(PUT, FLOW[TOT_ITERATES], TOTAL_ITERATES,
     PUT, FLOW[STARTSAVE_POINTS], START_SAVE_POINTS,
     PUT, FLOW[SKIPSZ], SKIP_SIZE,
     PUT, FLOW[STEPSZ], STEPSIZE,
     PUT, FLOW[DIVERG], DIVERG_CUTOFF,
     PUT, FLOW[DIRECTION], FORWARD,
     PUT, FLOW[STOPPING_CONDITION], PROP_NSTEP,
     NULL);

  if( *((int *) pm( GET, "Model.Mapping_Flag", NULL )) ) 
    pm(EXEC, FLOW[LOAD_ITER], NULL);
  else
    pm(EXEC, FLOW[LOAD_INT], NULL);
}
