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
### assign color and symbol type to a given periodic orbit or equilibria ###
*/

#include <stdio.h>

#include <constants.h>
#include <defaults.h>
#include <fixptlib.h>

fp_get_attributes(fp)
     struct Fixpt_DataS *fp;
{
  switch (fp->fptype)
    {
    case SADDLE:
      sprintf(fp->name,"SADDLE");
      fp->prop_cntl.sys_color = SADDLE_COLOR;
      fp->prop_cntl.symbol = SADDLE_SYMBOL;
      break;
    case SOURCE:
      sprintf(fp->name,"SOURCE");
      fp->prop_cntl.sys_color = SOURCE_COLOR;
      fp->prop_cntl.symbol = SOURCE_SYMBOL;
      break;
    case SINK:
      sprintf(fp->name,"SINK");
      fp->prop_cntl.sys_color = SINK_COLOR;
      fp->prop_cntl.symbol = SINK_SYMBOL;
      break;
    case SPIRAL_SOURCE:
      sprintf(fp->name,"SPIRAL SOURCE");
      fp->prop_cntl.sys_color = SOURCE_COLOR;
      fp->prop_cntl.symbol = SOURCE_SYMBOL;
      break;
    case SPIRAL_SINK:
      sprintf(fp->name,"SPIRAL SINK");
      fp->prop_cntl.sys_color = SINK_COLOR;
      fp->prop_cntl.symbol = SINK_SYMBOL;
      break;
    case INDETERMINATE:
    default:
      sprintf(fp->name,"INDETERMINATE");
      fp->prop_cntl.sys_color = DEFAULT_FP_COLOR;
      fp->prop_cntl.symbol = DEFAULT_FP_SYMBOL;
      break;
    }
}
