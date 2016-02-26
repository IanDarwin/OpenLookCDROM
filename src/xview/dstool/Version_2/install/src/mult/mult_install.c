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
 * Create MULT postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *MULT_OBJ_NAME = "Mult";

static char *MULT[] = {
  "Mult.Load_Choice", "Mult.Transformation",
  "Mult.Trans_Param", "Mult.Radius",
  "Mult.Points", "Mult.Total",
  "Mult.Ic", "Mult.Fc",
  "Mult.Images", "Mult.Center",
  "Mult.Forwards", "Mult.Backwards", "Mult.Continue",
  "Mult.Load", "Mult.Copy"
  };

typedef enum {
  LOAD_CHOICE=0, TRANSFORMATION, TRANS_PARAM,
  RADIUS, POINTS, TOTAL, IC, FC, IMAGES, CENTER,
  GO_FORWARDS, GO_BACKWARDS, GO_CONTINUE, LOAD, COPY
  } MULT_t;

void
  mult_install()
{
  void mult_forwards(), mult_backwards(), mult_continue(),
  mult_load(), mult_copy();

  pm(CREATE_OBJ, MULT_OBJ_NAME,
     CREATE_ELEM, MULT[LOAD_CHOICE], INT,
     CREATE_ELEM, MULT[TRANSFORMATION], INT,
     CREATE_ELEM, MULT[TRANS_PARAM], DBL,
     CREATE_ELEM, MULT[RADIUS], DBL_LIST,
     CREATE_ELEM, MULT[POINTS], INT_LIST,
     CREATE_ELEM, MULT[TOTAL], INT,
     CREATE_ELEM, MULT[IC], DBL_LIST,
     CREATE_ELEM, MULT[FC], DBL_LIST,
     CREATE_ELEM, MULT[IMAGES], INT,
     CREATE_ELEM, MULT[CENTER], DBL_LIST,
     CREATE_ELEM, MULT[GO_FORWARDS], FNCT,
     CREATE_ELEM, MULT[GO_BACKWARDS], FNCT,
     CREATE_ELEM, MULT[GO_CONTINUE], FNCT,
     CREATE_ELEM, MULT[LOAD], FNCT,
     CREATE_ELEM, MULT[COPY], FNCT,
     NULL);

  pm(CREATE_ELEM, "Memory.Mult", MEMRY, NULL);
  
  pm(INIT, MULT[GO_FORWARDS],
     INIT, MULT[GO_BACKWARDS],
     INIT, MULT[GO_CONTINUE],
     INIT, MULT[LOAD],
     INIT, MULT[COPY],
     PUT, MULT[GO_FORWARDS], mult_forwards,
     PUT, MULT[GO_BACKWARDS], mult_backwards,
     PUT, MULT[GO_CONTINUE], mult_continue,
     PUT, MULT[LOAD], mult_load,
     PUT, MULT[COPY], mult_copy,
     NULL);
}

void
  mult_reset()
{
  int i, n_varb, n_param, n_funct;
  double max, min;

  get_n_all_types(&n_varb,&n_param,&n_funct);

  pm(INIT, "Memory.Mult", MULT_MEMORY,
     NULL);

  pm(PUT, MULT[IMAGES], 1,
     PUT, MULT[TOTAL], 0,
     PUT, MULT[LOAD_CHOICE], 0,
     PUT, MULT[TRANSFORMATION], 0,
     PUT, MULT[TRANS_PARAM], 0.0,
     INIT, MULT[RADIUS], n_varb+n_param,
     INIT, MULT[POINTS], n_varb+n_param,
     INIT, MULT[CENTER], n_varb+n_param,
     CLEAR, MULT[IC],
     CLEAR, MULT[FC],
     NULL);

  for (i=0; i<n_varb; i++) 
    {
      pm(PUT, "Mult.Points", i, 1, NULL);
      max = *((double *) pm(GET, "Model.Varb_Max", i, NULL));
      min = *((double *) pm(GET, "Model.Varb_Min", i, NULL));
      pm(PUT, "Mult.Radius", i, (max - min)*MULT_RADIUS_FACTOR, NULL);
    }	      
  for (i=0; i<n_param; i++) 
    {
      pm(PUT, "Mult.Points", n_varb+i, 1, NULL);
      max = *((double *) pm(GET, "Model.Param_Max", i, NULL));
      min = *((double *) pm(GET, "Model.Param_Min", i, NULL));
      pm(PUT, "Mult.Radius", n_varb+i, (max - min)*MULT_RADIUS_FACTOR, NULL);
    }	      

}
