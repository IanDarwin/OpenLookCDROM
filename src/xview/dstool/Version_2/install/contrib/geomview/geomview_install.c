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
 * geomview_install.c
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *GEOMVIEW_OBJ_NAME = "Geomview";

static char *GEOMVIEW[] = {
  "Geomview.Window_Number",
  "Geomview.Destination",
  "Geomview.Hor",
  "Geomview.Active_Hor_Type",
  "Geomview.Active_Hor_Index",
  "Geomview.Ver",
  "Geomview.Active_Ver_Type",
  "Geomview.Active_Ver_Index",
  "Geomview.Depth",
  "Geomview.Active_Depth_Type",
  "Geomview.Active_Depth_Index",
  "Geomview.Directory",
  "Geomview.Filename",
  "Geomview.View"
};

typedef enum {
  GV_WINDOW_NUMBER=0, GV_DESTINATION,
  GV_HOR,GV_ACTIVE_HOR_TYPE,GV_ACTIVE_HOR_INDEX,
  GV_VER,GV_ACTIVE_VER_TYPE,GV_ACTIVE_VER_INDEX,
  GV_DEPTH,GV_ACTIVE_DEPTH_TYPE,GV_ACTIVE_DEPTH_INDEX,
  GV_DIRECTORY,GV_FILENAME,GV_VIEW
} GEOMVIEW_t;

void
  geomview_install()
{
  int geomview_view_go();

  pm(CREATE_OBJ, GEOMVIEW_OBJ_NAME,
     CREATE_ELEM, GEOMVIEW[GV_WINDOW_NUMBER], INT,
     CREATE_ELEM, GEOMVIEW[GV_DESTINATION], INT,
     CREATE_ELEM, GEOMVIEW[GV_HOR], INT,
     CREATE_ELEM, GEOMVIEW[GV_ACTIVE_HOR_TYPE], INT,
     CREATE_ELEM, GEOMVIEW[GV_ACTIVE_HOR_INDEX], INT,
     CREATE_ELEM, GEOMVIEW[GV_VER], INT,
     CREATE_ELEM, GEOMVIEW[GV_ACTIVE_VER_TYPE], INT,
     CREATE_ELEM, GEOMVIEW[GV_ACTIVE_VER_INDEX], INT,
     CREATE_ELEM, GEOMVIEW[GV_DEPTH], INT,
     CREATE_ELEM, GEOMVIEW[GV_ACTIVE_DEPTH_TYPE], INT,
     CREATE_ELEM, GEOMVIEW[GV_ACTIVE_DEPTH_INDEX], INT,
     CREATE_ELEM, GEOMVIEW[GV_DIRECTORY], STRNG,
     CREATE_ELEM, GEOMVIEW[GV_FILENAME], STRNG,
     CREATE_ELEM, GEOMVIEW[GV_VIEW], FNCT,
     NULL);

  pm(INIT, GEOMVIEW[GV_DIRECTORY], SIZE_OF_DIR_PLUS_FNAME,
     INIT, GEOMVIEW[GV_FILENAME], SIZE_OF_FNAME,
     PUT,  GEOMVIEW[GV_DESTINATION], GEOMVIEW,
     PUT,  GEOMVIEW[GV_VIEW], geomview_view_go,
     NULL);
}

void
  geomview_reset()
{

}
