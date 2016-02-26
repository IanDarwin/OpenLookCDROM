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
/* code to setup a new integrator */

#include <stdio.h>
#include <prop.h>
#include <pm.h>

void
load_int()
{
  int n = *((int *) pm(GET, "Flow.Int_Num", NULL));
  
  Int_Algol = Int_Sel[n];
  Int_Algol.Int_Init();
  Int_Algol.Panel_Sel_Value = Int_Cur_Choice = n;
  
  /* make sure correct integrator gets loaded on slaves ! */
  if (*(int *) pm(GET, "Control.Pvm_Host", NULL) == TRUE)
    {
#ifdef USING_PVM
      pvm_pm_put_all("Flow.Int_Num");
      pvm_pm_exec_all("Flow.Load_Int");
#endif
     n = 1;			/* kludge to avoid empty body */
    }


}

