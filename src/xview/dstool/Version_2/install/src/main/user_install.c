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
 * user_install.c
 *
 * Procedures:
 *   user_install()
 *   user_reset()
 *
 * Here we install the postmaster objects
 * which the user specifies.
 *
 */
#include <stdio.h>
#include <pm.h>
typedef void (*PFV)();



extern void
  flow_install(),
  mult_install(),
#ifdef USING_PVM
   pvm_install(),
#endif
  fixed_install();
/*  oned_install(), */
/*  cont_install(), */
/*  lb_install(); */

static PFV user_install_procs[] = {
  flow_install,
  mult_install,
#ifdef USING_PVM
   pvm_install, 
#endif
  fixed_install
/*  oned_install, */
/*  cont_install, */
/*  lb_install */
  };

#define N_USER_INSTALL sizeof(user_install_procs)/sizeof(PFV)

void
  user_install()
{
  int i;

  for (i=0; i<N_USER_INSTALL; i++)
    if (user_install_procs[i] != (PFV) NULL)
      user_install_procs[i]();
}



extern void 
  flow_reset(),
  mult_reset(),
  fixed_reset();
/*   pvm_reset(), 
  oned_reset(),
  cont_reset(),
  lb_reset();
*/

static PFV user_reset_procs[] = {
  flow_reset,
  mult_reset,
  fixed_reset
/*   pvm_reset, */
/*  oned_reset, */
/*  cont_reset, */
/*   lb_reset */
  };

#define N_USER_RESET sizeof(user_reset_procs)/sizeof(PFV)

void
  user_reset()
{
  int i;

  for (i=0; i<N_USER_RESET; i++)
    if (user_reset_procs[i] != (PFV) NULL)
      user_reset_procs[i]();
}


