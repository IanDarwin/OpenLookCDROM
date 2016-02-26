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
 * sys_install.c
 *
 * Procedures:
 *   sys_install()
 *   sys_reset()
 *
 * Here we install the postmaster objects
 * which will always exist.
 *
 */
#include <stdio.h>
#include <pm.h>

typedef void (*PFV)();

extern void
  selected_install(),
  defaults_install(),
  model_install(),
  manifold_install(),
  memory_install(),
  color_install(),
  save_install(),
  load_install(),
#ifdef USING_TCL
  tcl_install(),
#endif
  print_install();

static PFV sys_install_procs[] = {
  selected_install,
  defaults_install,
  model_install,
  manifold_install,
  memory_install,
  color_install,
  save_install,
  load_install,
#ifdef USING_TCL
  tcl_install,
#endif
  print_install
  };

#define N_SYS_INSTALL sizeof(sys_install_procs)/sizeof(PFV)

void
  sys_install()
{
  int i;

  for (i=0; i<N_SYS_INSTALL; i++)
    if (sys_install_procs[i] != (PFV) NULL)
      sys_install_procs[i]();
}



extern void
  selected_reset(),
  defaults_reset(),
  model_reset(),
  manifold_reset(),
  memory_reset(),
  color_reset(),
  save_reset(),
  load_reset(),
#ifdef USING_TCL
  tcl_reset(),
#endif
  print_reset();


static PFV sys_reset_procs[] = {
  selected_reset,
  defaults_reset,
  model_reset,
  manifold_reset,
  memory_reset,
  color_reset,
  save_reset,
  load_reset,
#ifdef USING_TCL
  tcl_reset,
#endif
  print_reset
  };

#define N_SYS_RESET sizeof(sys_reset_procs)/sizeof(PFV)

void
  sys_reset()
{
  int i;

  for (i=0; i<N_SYS_RESET; i++)
    if (sys_reset_procs[i] != (PFV) NULL)
      sys_reset_procs[i]();
}

