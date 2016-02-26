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
 * Create PVM postmaster object
 */
#include <stdio.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *PVM_OBJ_NAME = "Pvm";

static char *PVM[] = {
  "Pvm.Start",
  "Pvm.Finish",
  "Pvm.Ncpu",
  "Pvm.Status",
  "Pvm.Slave_Name",
  "Pvm.Instance",
  "Pvm.Nslaves",
  "Pvm.Slave_Instances",
  "Pvm.Mult_Forwards",
  "Pvm.Mult_Backwards",
  "Pvm.Mult_Continue"
  };

typedef enum {
  START=0, FINISH, NCPUS, STATUS, SLAVE_NAME, INSTANCE, 
  NSLAVES, SLAVE_INSTANCES, MULT_FORWARDS, MULT_BACKWARDS,
  MULT_CONTINUE
  } PVM_t;

void
  pvm_install()
{
  void pvm_start(), pvm_finish(), pvm_status(),
  pvm_mult_forwards(), pvm_mult_backwards(), pvm_mult_continue();

  pm(CREATE_OBJ, PVM_OBJ_NAME,
     CREATE_ELEM, PVM[START], FNCT,
     CREATE_ELEM, PVM[FINISH], FNCT,
     CREATE_ELEM, PVM[NCPUS], INT,
     CREATE_ELEM, PVM[STATUS], FNCT,
     CREATE_ELEM, PVM[SLAVE_NAME], STRNG,
     CREATE_ELEM, PVM[INSTANCE], INT,
     CREATE_ELEM, PVM[NSLAVES], INT,
     CREATE_ELEM, PVM[SLAVE_INSTANCES], INT_LIST,
     CREATE_ELEM, PVM[MULT_FORWARDS], FNCT,
     CREATE_ELEM, PVM[MULT_BACKWARDS], FNCT,
     CREATE_ELEM, PVM[MULT_CONTINUE], FNCT,
     NULL);

  pm(INIT, PVM[START],
     INIT, PVM[FINISH],
     INIT, PVM[STATUS],
     INIT, PVM[MULT_FORWARDS],
     INIT, PVM[MULT_BACKWARDS],
     INIT, PVM[MULT_CONTINUE],
     PUT, PVM[START], pvm_start,
     PUT, PVM[FINISH], pvm_finish,
     PUT, PVM[STATUS], pvm_status,
     PUT, PVM[MULT_FORWARDS], pvm_mult_forwards,
     PUT, PVM[MULT_BACKWARDS], pvm_mult_backwards,
     PUT, PVM[MULT_CONTINUE], pvm_mult_continue,
     PUT, PVM[NCPUS], 0,
     INIT, PVM[SLAVE_NAME], strlen(DSTOOL_PVM_SLAVE)+1,
     PUT, PVM[SLAVE_NAME], DSTOOL_PVM_SLAVE,
     NULL);

  pm(CREATE_ELEM, "Mult.Pvm_Forwards", FNCT,
     CREATE_ELEM, "Mult.Pvm_Backwards", FNCT,
     CREATE_ELEM, "Mult.Pvm_Continue", FNCT,
     NULL);

  pm(INIT, "Mult.Pvm_Forwards",
     INIT, "Mult.Pvm_Backwards",
     INIT, "Mult.Pvm_Continue",
     PUT, "Mult.Pvm_Forwards", pvm_mult_forwards,
     PUT, "Mult.Pvm_Backwards", pvm_mult_backwards,
     PUT, "Mult.Pvm_Continue", pvm_mult_continue,
     NULL);
}

void
  pvm_reset()
{
  /* make sure correct model gets loaded on slaves ! */
  if (*(int *) pm(GET, "Control.Pvm_Host", NULL) == TRUE)
    {
      pvm_pm_put_all("Model.Load_Number");
      pvm_pm_exec_all("Model.Load");
      pvm_pm_put_all("Flow.Int_Num");
      pvm_pm_exec_all("Flow.Load_Int");
    }

}
