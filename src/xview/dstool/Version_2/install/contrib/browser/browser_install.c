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
 * browser_install.c
 *
 * Procedures:
 *   browser_install()
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *BROWSER_OBJ_NAME = "Browser";

static char *BROWSER[] = {
  "Browser.Memory",
  "Browser.Flow_Num", "Browser.Traj_Num", "Browser.Point_Num",
  "Browser.Highlight",
  "Browser.Vars", "Browser.Params", "Browser.Funcs",
  "Browser.Color", "Browser.Set_Code",
  "Browser.Copy", "Browser.Output", "Browser.Delete"
};

typedef enum {
  MEMORY=0,
  FLOW_NUM, TRAJ_NUM, POINT_NUM, 
  HIGHLIGHT, VARS, PARAMS, FUNCS, COLOR, SET_CODE,
  COPY, OUTPUT, DELETE
} BROWSER_t;


void
  browser_install()
{
  void browser_copy_go(), browser_output_go(),
    browser_delete_go();

  pm(CREATE_OBJ, BROWSER_OBJ_NAME,
     CREATE_ELEM, BROWSER[MEMORY], ADDRS,
     CREATE_ELEM, BROWSER[FLOW_NUM], INT,
     CREATE_ELEM, BROWSER[TRAJ_NUM], INT,
     CREATE_ELEM, BROWSER[POINT_NUM], INT,
     CREATE_ELEM, BROWSER[HIGHLIGHT], INT,
     CREATE_ELEM, BROWSER[VARS], DBL_LIST,
     CREATE_ELEM, BROWSER[PARAMS], DBL_LIST,
     CREATE_ELEM, BROWSER[FUNCS], DBL_LIST,
     CREATE_ELEM, BROWSER[COLOR], INT_LIST,
     CREATE_ELEM, BROWSER[SET_CODE], INT,
     CREATE_ELEM, BROWSER[COPY], FNCT,
     CREATE_ELEM, BROWSER[OUTPUT], FNCT,
     CREATE_ELEM, BROWSER[DELETE], FNCT,
     NULL);

  pm(INIT, BROWSER[COLOR], 3,
     INIT, BROWSER[COPY],
     INIT, BROWSER[OUTPUT],
     INIT, BROWSER[DELETE],
     PUT, BROWSER[COPY], browser_copy_go,
     PUT, BROWSER[OUTPUT], browser_output_go,
     PUT, BROWSER[DELETE], browser_delete_go,
     NULL);
}

void
  browser_reset()
{
  int v,p,f;
  
  get_n_all_types(&v, &p, &f);
  pm(INIT, BROWSER[VARS], v,
     INIT, BROWSER[PARAMS], p,
     INIT, BROWSER[FUNCS], f,
     INIT, BROWSER[MEMORY],
     NULL);

  pm(PUT, BROWSER[FLOW_NUM], 1,
     PUT, BROWSER[TRAJ_NUM], 1,
     PUT, BROWSER[POINT_NUM], 1,
     PUT, BROWSER[MEMORY], (void *) pm(GET, "Memory.Traj", NULL),
     PUT, BROWSER[HIGHLIGHT], 0,
     PUT, BROWSER[SET_CODE], 0,
     NULL);
}
