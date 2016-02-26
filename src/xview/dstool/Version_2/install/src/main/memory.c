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
 * Create MEMORY postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *MEMORY_OBJ_NAME = "Memory";

static char *MEMORY[] = {
  "Memory.Sel_Pt", "Memory.Param", "Memory.Cont"
  };

typedef enum {
  SEL_PT=0, PARAM, CONT
  } MEMORY_t;


void
memory_install()
{
  pm(CREATE_OBJ, MEMORY_OBJ_NAME,
     CREATE_ELEM, MEMORY[SEL_PT], MEMRY,
     CREATE_ELEM, MEMORY[PARAM], MEMRY,
     CREATE_ELEM, MEMORY[CONT], MEMRY,
     NULL);
}

void
memory_reset()
{
  pm(INIT, MEMORY[SEL_PT], SEL_PT_MEMORY,
     INIT, MEMORY[PARAM], PARAM_MEMORY,
     INIT, MEMORY[CONT], CONT_MEMORY,
     NULL);
}

