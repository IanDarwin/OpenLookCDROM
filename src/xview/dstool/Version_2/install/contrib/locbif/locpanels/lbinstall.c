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
 *
 * lb_install.c
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *LB_OBJ_NAME = "Lb_Control";

static char *LB[] = {
  "Lb_Control.Lb_Fc",
  "Lb_Control.Lb_Param_Fc",
};

void
  lb_install()
{

  pm(CREATE_OBJ, LB_OBJ_NAME,
     CREATE_ELEM, LB[0], DBL_LIST, 
     CREATE_ELEM, LB[1], DBL_LIST, 
     NULL);

  pm(INIT, LB[0], *((int *) pm(GET, "Model.Varb_Dim", NULL)),
     INIT, LB[1], *((int *) pm(GET, "Model.Param_Dim", NULL)),
     NULL);

}

void
  lb_reset()
{

}



