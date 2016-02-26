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
 * Create MANIFOLD postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *MANIFOLD_OBJ_NAME = "Manifold";

static char *MANIFOLD[] = {
  "Manifold.Type", "Manifold.Periodic_Varb", 
  "Manifold.Period_Start", "Manifold.Period_End",
  };

typedef enum {
  TYPE=0, PERIODIC_VARB, PERIOD_START, PERIOD_END
  } MANIFOLD_t;


void
  manifold_install()
{
  pm(CREATE_OBJ, MANIFOLD_OBJ_NAME,
     CREATE_ELEM, MANIFOLD[TYPE], INT,
     CREATE_ELEM, MANIFOLD[PERIODIC_VARB], INT_LIST,
     CREATE_ELEM, MANIFOLD[PERIOD_START], DBL_LIST,
     CREATE_ELEM, MANIFOLD[PERIOD_END], DBL_LIST,
     NULL);
}

void
  manifold_reset()
{
}

