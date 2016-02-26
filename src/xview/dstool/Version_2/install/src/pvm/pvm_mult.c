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
 * pvm_mult.c
 *
 * Procedures:
 *   pvm_mult_forwards() 
 *   pvm_mult_backwards() 
 *   pvm_mult_continue()
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <memory.h>
#include <complib.h>
#include <defaults.h>
#include <pm.h>
#include "pvm_messages.h"
#include "pvmuser.h"

void
  pvm_mult_forwards()
{
  int nslaves, ntrajs, vars, params, funcs, n, dim, done=0, cpu=0,
    ddone=0, tot, bytes, msgtype, instance, worksize, color[3];
  char slave_prog[MAX_CMD_LINE_ARG_LEN], *get_the_time();
  static struct Prop_DataS prop_cntl;
  double **dmatrix(), *dvector(), *pts;
  static int responses[] = {pvm_TRAJ, pvm_EXEC_REPLY};
  int op=PUT;
  memory mem;

  nslaves = *((int *) pm(GET, "Pvm.Nslaves", NULL));
  ntrajs = *((int *) pm(GET, "Mult.Total", NULL));
  get_n_all_types(&vars, &params, &funcs);
  worksize = *((int *) pm(GET, "Defaults.Disp_Points", NULL));
  prop_cntl.ph_space_dim = vars;
  prop_cntl.parameter_dim = params;
  prop_cntl.function_dim = funcs;
  prop_cntl.traj_segment = dmatrix(0, worksize-1, 0, vars+params-1);
  prop_cntl.parameters = dvector(0, params-1);
  prop_cntl.workspace = dvector(0, funcs-1);
  prop_cntl.function =  (void *) pm( GET, "Model.DS_Def", NULL );
  prop_cntl.aux_function =  (void *) pm( GET, "Model.Aux_Function", NULL );
  prop_cntl.plot_traj = (void *) pm(GET, "Flow.Plot_Function", NULL);
  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);
  bump_color();
  color[0] = prop_cntl.table_color = get_alt_color();
  color[1] = prop_cntl.sys_color = 
    *((int *) pm(GET, "Color.Pick_Color_Choice", NULL));
  color[2] = prop_cntl.symbol = 
    get_symbol_code( *((int *) pm(GET, "Defaults.Symbol_Index", NULL)), 
		    *((int *) pm(GET, "Defaults.Size_Index", NULL)));
  mem = (memory) pm(GET, "Memory.Mult", NULL);

  fprintf(stderr, "Setting up slaves at %s.\n", get_the_time());
  pvm_mult_setup();

  /* split mult into nslaves sections ! */
  n = ntrajs / nslaves;
  if (n*nslaves < ntrajs) n++;
  dim = vars+params;
  pts = (double *) calloc(n*dim, sizeof(double));
  while (n>0 && done < ntrajs)
    {
      if (done+n > ntrajs) n = ntrajs-done;
      /* send n initial conditions */
      initsend();
      putnint(&op,1);
      putstring("Mult.Total");
      putnint(&n,1);
      snd(slave_prog, *((int *) pm(GET, "Pvm.Slave_Instances", cpu, NULL)),
	  pvm_POSTMASTER);
      tot = n*dim;
      pvm_pm_init("Mult.Fc", tot, cpu);
      initsend();
      putnint(&op,1);
      putstring("Mult.Ic");
      putnint(&tot,1);
      pm(GET_LIST, "Mult.Ic", ddone, ddone+tot-1, pts, NULL);
      putndfloat(pts, tot);
      snd(slave_prog, *((int *) pm(GET, "Pvm.Slave_Instances", cpu, NULL)),
	  pvm_POSTMASTER);
      pvm_pm_exec_reply("Mult.Forwards", cpu);
      cpu++;
      done += n;
      ddone += tot;
    }
  fprintf(stderr, "Done setting up slaves at %s.\n", get_the_time());
  cfree(pts);
  
  /* now wait to get the results back! */
  
  memory_start_new_flow(mem, cpu,
			0, 0, 
			1+(*(int *) pm(GET, "Flow.Start_Save_Points", NULL)) +
			  (*(int *) pm(GET, "Flow.Total_Iterates", NULL)),
			0,0);
  while (cpu>0)
    {
      rcvmulti(2, responses);
      rcvinfo(&bytes, &msgtype, slave_prog, &instance);
      if (msgtype == pvm_EXEC_REPLY)
	cpu--;
      else
	{
	  /* read and display data */
	  pvm_mult_unload_traj(instance, &prop_cntl, mem, color);
	}
    }

  /* now query if there is more data to read */
  while (probe(pvm_TRAJ) == pvm_TRAJ)
    {
      rcv(pvm_TRAJ);
      pvm_mult_unload_traj(instance, &prop_cntl, mem, color);
    }

  free_dmatrix(prop_cntl.traj_segment, 0, worksize-1, 0, vars+params-1);
  free_dvector(prop_cntl.parameters, 0, params-1);
  free_dvector(prop_cntl.workspace, 0, funcs-1);
}


int
  pvm_mult_unload_traj(instance, prop_cntl, m, color)
int instance, *color;
struct Prop_DataS *prop_cntl;
memory m;
{
  int i, n;

  /* set color ? */
  color[1] = prop_cntl->sys_color = instance;

  /* copy out parameters */
  getndfloat(prop_cntl->parameters, prop_cntl->parameter_dim);
  
  /* copy out traj_segment */
  getnint(&n, 1);
  for (i=0; i<n; i++)
    {
      getndfloat(prop_cntl->traj_segment[i], prop_cntl->ph_space_dim);
    }

  prop_cntl->plot_traj(prop_cntl, 0, n-1, prop_cntl->workspace);
  
  /* save the points to a memory object ?!? */
  if (m != NULL)
    {
      memory_set_cur_traj(m, instance);
      /* memory_add_point(m, NULL, prop_cntl->parameters, NULL,
	NULL, NULL, NULL, NULL); */
      for (i=0; i<n; i++)
	memory_add_point(m, prop_cntl->traj_segment[i],
			 prop_cntl->parameters,
			 color, NULL, NULL, NULL, NULL);
    }


}



void
  pvm_mult_backwards()
{
  pvm_mult_setup();

  system_mess_proc(0,"pvm_mult_backwards");
}


void
  pvm_mult_continue()
{
  pvm_mult_setup();

  system_mess_proc(0,"pvm_mult_continue");
}

int
pvm_mult_setup()
{ int record;

  pvm_pm_put_all("Flow.Stepsize");
  pvm_pm_put_all("Flow.Start_Save_Points");
  pvm_pm_put_all("Flow.Total_Iterates");
  pvm_pm_put_all("Flow.Skip_Size");
  pvm_pm_put_all("Flow.Direction");
  pvm_pm_put_all("Flow.Stopping_Condition");
  pvm_pm_put_all("Flow.Varb_Events");
  pvm_pm_put_all("Flow.Funct_Events");
  pvm_pm_put_all("Flow.Varb_Event_Values");
  pvm_pm_put_all("Flow.Funct_Event_Values");
  pvm_pm_put_all("Flow.Final_Time");
  pvm_pm_put_all("Mult.Images");
  pvm_pm_put_all("Defaults.Disp_Points");
  pvm_pm_put_all("Defaults.Disp_Points");
  
  /* turn off recording on slaves */
  record = *((int *) pm(GET, "Defaults.Recording", NULL));
  pm(PUT, "Defaults.Recording", 1, NULL);
  pvm_pm_put_all("Defaults.Recording");
  pm(PUT, "Defaults.Recording", record, NULL);
}
