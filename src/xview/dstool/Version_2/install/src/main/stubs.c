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
#include <stdio.h>
#include <defaults.h>
#include <pm.h>
#include <complib.h>

extern void start_xv_loop();
extern void rebuild_windows();
extern void user_comp_install();
extern int parserwin_open();
extern void traj_plot();
extern int save_config();
extern int load_config();
extern int windows_install_init();

/* this struct is here is force these routines to be linked */
void *STUBS[] = {
  start_xv_loop,
  rebuild_windows,
  user_comp_install,
  parserwin_open,
  traj_plot,
  save_config,
  load_config,
  windows_install_init
  };

void
  start_xv_loop(argc,argv)
int argc;
char **argv;
{
  system_mess_proc(1,"start_xv_loop stub");
}


void
  rebuild_windows()
{
  
}

void
  user_comp_install()
{
  system_mess_proc(1,"user_comp_install stub");
}

int
  parserwin_open()
{
  system_mess_proc(1,"parserwin_open stub");
}

void
  traj_plot(integ_cntl, start, stop, w)
struct Prop_DataS *integ_cntl;
int start, stop;
double *w;
{
  return;
}


int save_config(fp)
FILE *fp;
{
  return;
}

int load_config(fp)
FILE *fp;
{
  return;
}


int windows_install_init()

{
}
