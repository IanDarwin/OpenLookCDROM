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
 * Create COLOR postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *COLOR_OBJ_NAME = "Color";

static char *COLOR[] = {
  "Color.Pick_Color_Choice",
  "Color.Traj_Colormap_Size",
  "Color.Sys_Colormap_Size",
  "Color.Red_Table", "Color.Green_Table", "Color.Blue_Table"
  };

typedef enum {
  PICK_COLOR_CHOICE, TRAJ_COLORMAP_SIZE, SYS_COLORMAP_SIZE,
  RED_TABLE, GREEN_TABLE, BLUE_TABLE
  } COLOR_t;


void
  color_install()
{
  pm(CREATE_OBJ, COLOR_OBJ_NAME,
     CREATE_ELEM, COLOR[PICK_COLOR_CHOICE], INT,
     CREATE_ELEM, COLOR[TRAJ_COLORMAP_SIZE], INT,
     CREATE_ELEM, COLOR[SYS_COLORMAP_SIZE], INT,
     CREATE_ELEM, COLOR[RED_TABLE], INT_LIST,
     CREATE_ELEM, COLOR[GREEN_TABLE], INT_LIST,
     CREATE_ELEM, COLOR[BLUE_TABLE], INT_LIST,
     NULL);

  pm(PUT_SAVABLE, COLOR_OBJ_NAME, SAVE_NONE,NULL);
}

void
  color_reset()
{
}
