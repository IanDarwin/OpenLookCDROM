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
 * Create PRINT postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>


static char *PRINT_OBJ_NAME  = "Print";

static char *PRINT[] = {
  "Print.Owner",
  "Print.Hor_Label",
  "Print.Ver_Label",
  "Print.Title",
  "Print.Color_Flag",
  "Print.File_Flag",
  "Print.Directory",
  "Print.Filename",
  "Print.Printer_Name",
  "Print.Show_Settings",
  "Print.Show_Info",
  "Print.Font",
  "Print.Title_Pt",
  "Print.Label_Pt",
  "Print.Landscape",
  "Print.BBox_Ver_Length",
  "Print.BBox_Hor_Length",
  "Print.BBox_Ver_Offset",
  "Print.BBox_Hor_Offset",
  "Print.Num_Hor_Ticks",
  "Print.Num_Ver_Ticks",
  "Print.Connect",
  "Print.Label_Range",
  "Print.Show_BBox",
  "Print.HorMin",
  "Print.HorMax",
  "Print.VerMin",
  "Print.VerMax",
};

typedef enum {
  OWNER=0, HOR_LABEL, VER_LABEL, TITLE, COLOR_FLAG, FILE_FLAG,
  DIRECTORY, FILENAME, PRINT_NAME, SHOW_SETTINGS, SHOW_INFO,
  FONT, TITLE_PT, LABEL_PT, LANDSCAPE, BBOX_VER_LENGTH,
  BBOX_HOR_LENGTH, BBOX_VER_OFFSET, BBOX_HOR_OFFSET,
  NUM_HOR_TICKS, NUM_VER_TICKS, CONNECT,
  LABEL_RANGE, SHOW_BBOX, HORMIN, HORMAX, VERMIN, VERMAX
  } PRINT_t;

void 
  print_install()
{
  pm(CREATE_OBJ, PRINT_OBJ_NAME,
     CREATE_ELEM, PRINT[OWNER], INT,
     CREATE_ELEM, PRINT[HOR_LABEL], STRNG,
     CREATE_ELEM, PRINT[VER_LABEL], STRNG,
     CREATE_ELEM, PRINT[TITLE], STRNG,
     CREATE_ELEM, PRINT[COLOR_FLAG], INT,
     CREATE_ELEM, PRINT[FILE_FLAG], INT,
     CREATE_ELEM, PRINT[DIRECTORY], STRNG,
     CREATE_ELEM, PRINT[FILENAME], STRNG,
     CREATE_ELEM, PRINT[PRINT_NAME], STRNG,
     CREATE_ELEM, PRINT[SHOW_SETTINGS], INT,
     CREATE_ELEM, PRINT[SHOW_INFO], INT,
     CREATE_ELEM, PRINT[FONT], INT,
     CREATE_ELEM, PRINT[TITLE_PT], INT,
     CREATE_ELEM, PRINT[LABEL_PT], INT,
     CREATE_ELEM, PRINT[LANDSCAPE], INT,
     CREATE_ELEM, PRINT[BBOX_VER_LENGTH], INT,
     CREATE_ELEM, PRINT[BBOX_HOR_LENGTH], INT,
     CREATE_ELEM, PRINT[BBOX_VER_OFFSET], INT,
     CREATE_ELEM, PRINT[BBOX_HOR_OFFSET], INT,
     CREATE_ELEM, PRINT[NUM_HOR_TICKS], INT,
     CREATE_ELEM, PRINT[NUM_VER_TICKS], INT,
     CREATE_ELEM, PRINT[CONNECT], INT,
     CREATE_ELEM, PRINT[LABEL_RANGE], INT,
     CREATE_ELEM, PRINT[SHOW_BBOX], INT,
     CREATE_ELEM, PRINT[HORMIN], DBL,
     CREATE_ELEM, PRINT[HORMAX], DBL,
     CREATE_ELEM, PRINT[VERMIN], DBL,
     CREATE_ELEM, PRINT[VERMAX], DBL,
     NULL);

  pm(INIT, "Print.Title",     MAX_LEN_DS_TITLE,
     INIT, "Print.Hor_Label", MAX_LEN_VARB_NAME,
     INIT, "Print.Ver_Label", MAX_LEN_VARB_NAME,
     INIT, "Print.Directory", ( SIZE_OF_DIR_PLUS_FNAME - SIZE_OF_FNAME ),
     INIT, "Print.Filename",  SIZE_OF_FNAME,
     INIT, "Print.Printer_Name", SIZE_OF_FNAME,
     NULL);

}


void 
  print_reset()
{
  char dirname[SIZE_OF_DIR_PLUS_FNAME];
  char  *get_ds_name(), *get_hor_label(), *get_ver_label(), *getenv();

  if ( get_dstool_path(dirname,LPDEST) || get_dstool_path(dirname,PRINTER) ) 
    /* use environmental variables ; LPDEST or PRINTER if defined */
    pm(PUT, "Print.Printer_Name", dirname, NULL); 
  else							
    /* (see "man lpstat" and dir_util.c) use PRINTER_NAME from defaults.h */
    pm(PUT, "Print.Printer_Name", PRINTER_NAME, NULL); 

  get_cur_dir(dirname);			/* get defaults from system */

  pm(PUT, "Print.Title", get_ds_name(),
/*     PUT, "Print.Owner", -1, 
       PUT, "Print.Hor_Label", get_hor_label(win_num),
     PUT, "Print.Ver_Label", get_ver_label(win_num), */
     PUT, "Print.File_Flag", TRUE, 
     PUT, "Print.Color_Flag", FALSE,
     PUT, "Print.Directory", dirname,
     /* leave Filename empty */
     PUT, "Print.Show_Settings", FALSE,
     PUT, "Print.Show_Info", TRUE,
     PUT, "Print.Font", 0,
     PUT, "Print.Title_Pt", TITLE_PT_SIZE,
     PUT, "Print.Label_Pt", LABEL_PT_SIZE,
     PUT, "Print.Landscape", FALSE,
     PUT, "Print.Show_BBox", TRUE,
     PUT, "Print.Num_Hor_Ticks", NUM_X_TICKS,
     PUT, "Print.Num_Ver_Ticks", NUM_Y_TICKS,
     PUT, "Print.BBox_Hor_Length", BBOX_HOR_LEN,
     PUT, "Print.BBox_Ver_Length", BBOX_VER_LEN,
     PUT, "Print.BBox_Hor_Offset", BBOX_HOR_OFF,
     PUT, "Print.BBox_Ver_Offset", BBOX_VER_OFF,
     PUT, "Print.Connect", FALSE,
     PUT, "Print.Label_Range", TRUE,
     NULL);

}

