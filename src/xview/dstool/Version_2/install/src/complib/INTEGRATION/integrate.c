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
#include <constants.h>
#include <defaults.h>
#include <prop.h>
#include <complib.h>

int
integrate(integ_cntl)
struct  Prop_DataS     *integ_cntl;
{
  int	status=0;
  
  switch(integ_cntl->prop_mode)
    {case PROP_NSTEP:
       status = ode_nsteps(integ_cntl);
       break;
     case PROP_FSTOP:
       status = ode_stop(integ_cntl); 
       break;
     case PROP_TF:
       status = ode_time(integ_cntl); 
       break;
     case PROP_POINCARE:				/* paw  4/14/92 */
       status = ode_poincare(integ_cntl);
       break;
     default:
       return(MAJOR_ERROR);
     }

  return(status);
}
