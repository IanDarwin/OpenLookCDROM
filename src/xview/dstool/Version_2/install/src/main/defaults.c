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
 * Create DEFAULTS postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>


static char *DEFAULTS_OBJ_NAME  = "Defaults";

static char *DEFAULTS[] = {
  "Defaults.Disp_Points",
  "Defaults.Symbol_Index",
  "Defaults.Size_Index",
  "Defaults.Precision",
  "Defaults.Clipping",
  "Defaults.Recording",
  "Defaults.Varb_Min",
  "Defaults.Varb_Max",
  "Defaults.Param_Min",
  "Defaults.Param_Max",
  "Defaults.Funct_Min",
  "Defaults.Funct_Max",
  "Defaults.Ctable_File",
  "Defaults.Show_Color",
  "Defaults.Depth_Coord",
  "Defaults.Cmap_Type",
  "Defaults.Bgnd_Color"
  };

typedef enum {
  DISP_POINTS=0, SYMBOL_INDEX, SIZE_INDEX, PRECISION,
  CLIPPING, RECORDING, VARB_MIN, VARB_MAX, PARAM_MIN, PARAM_MAX,
  FUNCT_MIN, FUNCT_MAX, CTABLE_FILE, SHOW_COLOR, DEPTH_COORD,
  CMAP_TYPE, BGND_COLOR
  } DEFAULTS_t;


void
  defaults_install()
{
  pm(CREATE_OBJ, DEFAULTS_OBJ_NAME,
     CREATE_ELEM, DEFAULTS[DISP_POINTS], INT,
     CREATE_ELEM, DEFAULTS[SYMBOL_INDEX], INT,
     CREATE_ELEM, DEFAULTS[SIZE_INDEX], INT,
     CREATE_ELEM, DEFAULTS[PRECISION], INT,
     CREATE_ELEM, DEFAULTS[CLIPPING], INT,
     CREATE_ELEM, DEFAULTS[RECORDING], INT,
     CREATE_ELEM, DEFAULTS[VARB_MIN], DBL_LIST,
     CREATE_ELEM, DEFAULTS[VARB_MAX], DBL_LIST,
     CREATE_ELEM, DEFAULTS[PARAM_MIN], DBL_LIST,
     CREATE_ELEM, DEFAULTS[PARAM_MAX], DBL_LIST,
     CREATE_ELEM, DEFAULTS[FUNCT_MIN], DBL_LIST,
     CREATE_ELEM, DEFAULTS[FUNCT_MAX], DBL_LIST,
     CREATE_ELEM, DEFAULTS[CTABLE_FILE], STRNG,
     INIT, DEFAULTS[CTABLE_FILE], SIZE_OF_FNAME,
     CREATE_ELEM, DEFAULTS[SHOW_COLOR], INT,
     CREATE_ELEM, DEFAULTS[DEPTH_COORD], INT,
     CREATE_ELEM, DEFAULTS[CMAP_TYPE], INT,
     CREATE_ELEM, DEFAULTS[BGND_COLOR], INT,
     NULL);

  pm(INIT, DEFAULTS[CTABLE_FILE], SIZE_OF_FNAME, 
     NULL);

  pm(PUT, DEFAULTS[DISP_POINTS], DISP_TRAJ_ITERS,
     PUT, DEFAULTS[SYMBOL_INDEX], DEF_SYMBOL_INDEX,
     PUT, DEFAULTS[SIZE_INDEX], DEF_SYMBOL_SIZE_INDEX,
     PUT, DEFAULTS[PRECISION], DEF_PRECISION,
     PUT, DEFAULTS[RECORDING], 0,  /* yes */
     PUT, DEFAULTS[CLIPPING], 1,   /* no  */
     PUT, DEFAULTS[CTABLE_FILE], DEF_COLORTABLE,
     PUT, DEFAULTS[SHOW_COLOR], DEF_SHOWCOLOR,
     PUT, DEFAULTS[DEPTH_COORD], DEF_DEPTHCOORD,
     PUT, DEFAULTS[CMAP_TYPE], DEF_CMAPTYPE,
     PUT, DEFAULTS[BGND_COLOR], DEF_BGND_COLOR,
     NULL);
}


void
  defaults_reset()
{
  int v,p,f,i;

  get_n_all_types(&v,&p,&f);

  pm(INIT, DEFAULTS[VARB_MIN], v,
     INIT, DEFAULTS[VARB_MAX], v,
     INIT, DEFAULTS[PARAM_MIN], p,
     INIT, DEFAULTS[PARAM_MAX], p,
     INIT, DEFAULTS[FUNCT_MIN], f,
     INIT, DEFAULTS[FUNCT_MAX], f,
     NULL);

  for (i=0; i<v; i++)
    {
      pm(PUT, "Defaults.Varb_Min", i, 
	 *((double *) pm(GET, "Model.Varb_Min", i, NULL)), NULL);
      pm(PUT, "Defaults.Varb_Max", i, 
	 *((double *) pm(GET, "Model.Varb_Max", i, NULL)), NULL);
    }
  for (i=0; i<p; i++)
    {
      pm(PUT, "Defaults.Param_Min", i, 
	 *((double *) pm(GET, "Model.Param_Min", i, NULL)), NULL);
      pm(PUT, "Defaults.Param_Max", i, 
	 *((double *) pm(GET, "Model.Param_Max", i, NULL)), NULL);
    }
  for (i=0; i<f; i++)
    {
      pm(PUT, "Defaults.Funct_Min", i, 
	 *((double *) pm(GET, "Model.Funct_Min", i, NULL)), NULL);
      pm(PUT, "Defaults.Funct_Max", i, 
	 *((double *) pm(GET, "Model.Funct_Max", i, NULL)), NULL);
    }
}
