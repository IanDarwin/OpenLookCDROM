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
 * Create MODEL postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *MODEL_OBJ_NAME = "Model";

static char *MODEL[] = { 
  "Model.Name", "Model.DS_Def", "Model.Initialization", "Model.DfDx",
  "Model.DfDt", "Model.DfDparam", "Model.Inverse", "Model.Aux_Function",
  "Model.Mapping_Flag", "Model.Inverse_Flag", "Model.Jacobian_Flag",
  "Model.Varb_Ic", "Model.Param_Ic", 
  "Model.Varb_Dim", "Model.Param_Dim", "Model.Funct_Dim", 
  "Model.Varb_Names", "Model.Param_Names", "Model.Funct_Names",
  "Model.Varb_Min", "Model.Varb_Max",
  "Model.Param_Min", "Model.Param_Max",
  "Model.Funct_Min", "Model.Funct_Max",
  "Model.Load", "Model.Load_Number"
};

typedef enum {
  NAME=0, DS_DEF, INITIALIZATION, DFDX, DFDT, DFDPARAM, INVERSE, AUX_FUNCTION,
  MAPPING_FLAG, INVERSE_FLAG, JACOBIAN_FLAG,
  VARB_IC, PARAM_IC, VARB_DIM, PARAM_DIM, FUNCT_DIM,
  VARB_NAMES, PARAM_NAMES, FUNCT_NAMES,
  VARB_MIN, VARB_MAX, PARAM_MIN, PARAM_MAX, FUNCT_MIN, FUNCT_MAX, LOAD, LOAD_NUMBER
  } MODEL_t;

void
  model_install()
{
  extern void load_model();

  /* create MODEL object and elements */
  pm(CREATE_OBJ, MODEL_OBJ_NAME,
     CREATE_ELEM, MODEL[NAME], STRNG,
     CREATE_ELEM, MODEL[DS_DEF], ADDRS,
     CREATE_ELEM, MODEL[INITIALIZATION], ADDRS,
     CREATE_ELEM, MODEL[DFDX], ADDRS,
     CREATE_ELEM, MODEL[DFDT], ADDRS,
     CREATE_ELEM, MODEL[DFDPARAM], ADDRS,
     CREATE_ELEM, MODEL[INVERSE], ADDRS,
     CREATE_ELEM, MODEL[AUX_FUNCTION], ADDRS,
     CREATE_ELEM, MODEL[MAPPING_FLAG], INT,
     CREATE_ELEM, MODEL[INVERSE_FLAG], INT,
     CREATE_ELEM, MODEL[JACOBIAN_FLAG], INT,
     CREATE_ELEM, MODEL[VARB_IC], DBL_LIST,
     CREATE_ELEM, MODEL[PARAM_IC], DBL_LIST,
     CREATE_ELEM, MODEL[VARB_DIM], INT,
     CREATE_ELEM, MODEL[PARAM_DIM], INT,
     CREATE_ELEM, MODEL[FUNCT_DIM], INT,
     CREATE_ELEM, MODEL[VARB_NAMES], STRNG_LIST,
     CREATE_ELEM, MODEL[PARAM_NAMES], STRNG_LIST,
     CREATE_ELEM, MODEL[FUNCT_NAMES], STRNG_LIST,
     CREATE_ELEM, MODEL[VARB_MIN], DBL_LIST,
     CREATE_ELEM, MODEL[VARB_MAX], DBL_LIST,
     CREATE_ELEM, MODEL[PARAM_MIN], DBL_LIST,
     CREATE_ELEM, MODEL[PARAM_MAX], DBL_LIST,
     CREATE_ELEM, MODEL[FUNCT_MIN], DBL_LIST,
     CREATE_ELEM, MODEL[FUNCT_MAX], DBL_LIST,
     CREATE_ELEM, MODEL[LOAD], FNCT,
     CREATE_ELEM, MODEL[LOAD_NUMBER], INT,
     NULL);

  /* Default model is model number 0 */
  pm(INIT, MODEL[NAME], 0,
     INIT, MODEL[DS_DEF],
     INIT, MODEL[INITIALIZATION],
     INIT, MODEL[DFDX], INIT, MODEL[DFDT], INIT, MODEL[DFDPARAM],
     INIT, MODEL[INVERSE], INIT, MODEL[AUX_FUNCTION],
     PUT, MODEL[LOAD_NUMBER], 0,
     INIT, MODEL[LOAD],
     PUT_SAVABLE, MODEL_OBJ_NAME, SAVE_NONE,
     PUT, MODEL[LOAD], load_model, NULL);
}

void
  model_reset()
{
}
