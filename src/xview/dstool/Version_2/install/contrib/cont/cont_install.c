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
 * cont_install.c
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>
#include "cont_defaults.h"
static char *CONT_OBJ_NAME = "Cont";


static char *CONT[] = {	
"Cont.Stpsize" ,"Cont.Mode" ,"Cont.Iters" ,"Cont.Direction" ,
"Cont.Param" ,"Cont.Vary_Switch" ,"Cont.Jac_Update" ,"Cont.Abserr" ,
"Cont.Relerr" ,"Cont.Maxstp" ,"Cont.Minstp" ,"Cont.Target" ,
"Cont.Param_Fc" ,"Cont.Fc" ,"Cont.Search" ,"Cont.Plot_Type" 
};

typedef enum {
STPSIZE=0, MODE, ITERS, DIRECTION, PARAM, VARY_SWITCH, 
JAC_UPDATE, ABSERR, RELERR, MAXSTEP, MINSTEP, TARGET, PARAM_FC,
FC, SEARCH, PLOT_TYPE
} CONT_t;

void cont_install()
{

pm(CREATE_OBJ, CONT_OBJ_NAME,
     CREATE_ELEM, CONT[STPSIZE], DBL,
     CREATE_ELEM, CONT[MODE], INT,
     CREATE_ELEM, CONT[ITERS], INT,
     CREATE_ELEM, CONT[DIRECTION], INT,
     CREATE_ELEM, CONT[PARAM], INT,
     CREATE_ELEM, CONT[VARY_SWITCH], INT ,
     CREATE_ELEM, CONT[JAC_UPDATE], INT,
     CREATE_ELEM, CONT[ABSERR], DBL,
     CREATE_ELEM, CONT[RELERR], DBL,
     CREATE_ELEM, CONT[MAXSTEP], DBL,
     CREATE_ELEM, CONT[MINSTEP], DBL,
     CREATE_ELEM, CONT[TARGET], DBL,
     CREATE_ELEM, CONT[PARAM_FC], DBL_LIST,
     CREATE_ELEM, CONT[FC], DBL_LIST,
     CREATE_ELEM, CONT[SEARCH], INT,
     CREATE_ELEM, CONT[PLOT_TYPE], INT,
     NULL);

	pm(CREATE_ELEM, "Memory.Cont", MEMRY, NULL);

	pm(INIT, CONT[FC], *((int *) pm(GET, "Model.Varb_Dim", NULL)),
	   CONT[PARAM_FC], *((int *) pm(GET, "Model.Param_Dim", NULL)),
     NULL);

}
void cont_reset()
{
  pm(PUT, CONT[STPSIZE], CONT_STPSIZE,
     PUT, CONT[MODE], CONT_MODE,
     PUT, CONT[ITERS], CONT_MODE,
     PUT, CONT[DIRECTION], CONT_DIRECTION,
     PUT, CONT[PARAM], CONT_PARAM,
     PUT, CONT[VARY_SWITCH], CONT_VARY_SWITCH ,
     PUT, CONT[JAC_UPDATE], CONT_JAC_UPDATE,
     PUT, CONT[ABSERR], CONT_ABSERR,
     PUT, CONT[RELERR], CONT_RELERR,
     PUT, CONT[MAXSTEP], CONT_MAXSTEP,
     PUT, CONT[MINSTEP], CONT_MINSTEP,
     PUT, CONT[TARGET], CONT_TARGET,
/*     PUT, CONT[PARAM_FC], CONT_PARAM_FC,
     PUT, CONT[FC], CONT_FC,			*/
     PUT, CONT[SEARCH], CONT_SEARCH,
     PUT, CONT[PLOT_TYPE], CONT_PLOT_TYPE,
     NULL);

  pm(INIT, "Memory.Cont", CONT_MEMORY, NULL);

}


