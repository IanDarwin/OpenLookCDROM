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
 * Create LOAD postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *LOAD_OBJ_NAME = "Load";

static char *LOAD[] = {
  "Load.Directory",
  "Load.Filename",
  "Load.Data_Type",
  "Load.Color_Flag",
  "Load.Symbol_Flag",
  "Load.Varb_Index",
  "Load.Param_Index",
  "Load.Format_Flag",
  "Load.Driver","Load.Model_Name",
  "Load.If_New_Model_Fnct"
  };

typedef enum {
  DIRECTORY=0, FILENAME, 
  DATATYPE, COLORFLAG, SYMBOLFLAG,
  VARBINDEX, PARAMINDEX, FORMATFLAG, DRIVER,
  MODELNAME, IFNEWMODELFNCT
  } LOAD_t;


void
load_install()
{
  char  dirname[SIZE_OF_DIR_PLUS_FNAME];
  int   get_cur_dir(), load_go(), load_model_if_new();

  pm(CREATE_OBJ, LOAD_OBJ_NAME,
     CREATE_ELEM, LOAD[DIRECTORY], STRNG,
     CREATE_ELEM, LOAD[FILENAME], STRNG,
     CREATE_ELEM, LOAD[DATATYPE], STRNG,
     CREATE_ELEM, LOAD[COLORFLAG], INT,
     CREATE_ELEM, LOAD[SYMBOLFLAG], INT,
     CREATE_ELEM, LOAD[VARBINDEX], INT_LIST,
     CREATE_ELEM, LOAD[PARAMINDEX], INT_LIST,
     CREATE_ELEM, LOAD[FORMATFLAG], INT,
     CREATE_ELEM, LOAD[DRIVER], ADDRS,
     CREATE_ELEM, LOAD[MODELNAME], STRNG,
     CREATE_ELEM, LOAD[IFNEWMODELFNCT], FNCT,
     NULL);
  
  pm(INIT, LOAD[DIRECTORY], SIZE_OF_DIR_PLUS_FNAME,
     INIT, LOAD[FILENAME], SIZE_OF_FNAME,
     INIT, LOAD[DATATYPE], MAX_LABEL_LEN,
     INIT, LOAD[DRIVER],
     INIT, LOAD[MODELNAME], MAX_LEN_DS_TITLE,
     INIT, LOAD[IFNEWMODELFNCT],
     NULL);

  get_cur_dir(dirname);		/* get defaults from system */
  pm(PUT, LOAD[DIRECTORY], dirname,
     PUT_SAVABLE, LOAD[DIRECTORY], SAVE_NONE,
     /* leave Filename empty */
     PUT, LOAD[FORMATFLAG], 0,
     PUT, LOAD[DATATYPE], "Memory.Traj",
     PUT, LOAD[COLORFLAG],    FALSE,
     PUT, LOAD[SYMBOLFLAG],   FALSE,
     PUT, LOAD[DRIVER], load_go,
     PUT, LOAD[IFNEWMODELFNCT], load_model_if_new,
     NULL);
}

void
  load_reset()
{
  int i, v_dim, p_dim;

  v_dim = *((int *)pm(GET, "Model.Varb_Dim", NULL));
  p_dim = *((int *)pm(GET, "Model.Param_Dim", NULL));

  pm(INIT, LOAD[VARBINDEX], v_dim,
     INIT, LOAD[PARAMINDEX], p_dim,
     NULL);

  for( i=0; i<v_dim; i++)
    pm(PUT, LOAD[VARBINDEX], i, FALSE, NULL);
  for( i=0; i<p_dim; i++)
    pm(PUT, LOAD[PARAMINDEX], i, FALSE, NULL);

}
