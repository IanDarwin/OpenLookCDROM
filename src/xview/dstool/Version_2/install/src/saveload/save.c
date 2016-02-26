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
 * Create SAVE postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

/* temporary - will move to constants.h */
#define 	MAX_SHORT_STR 		31

static char *SAVE_OBJ_NAME = "Save";

static char *CUR_MEMORY_OBJ_NAME = "Cur_Memory";

static char *SAVE[] = {
  "Save.Settings", "Save.Config", "Save.Traj", "Save.Fixpt",
  "Save.Cont", "Save.Param", "Save.Select", "Save.Funct",
  "Save.Directory", "Save.Filename", "Save.Go"
  };

typedef enum {
  SETTINGS=0, CONFIG, TRAJ, FIXPT, CONT, PARAM, SELECT, FUNCT,
  DIRECTORY, FILENAME, GO
  } SAVE_t;

static char *CUR_MEMORY[] = {
    "Cur_Memory.Mem_Type",
    "Cur_Memory.Num_Objects" ,
    "Cur_Memory.Num_Header_Doubles",
    "Cur_Memory.Num_Header_Integers",
    "Cur_Memory.Init_Header_Fcn",
    "Cur_Memory.Header_Doubles",
    "Cur_Memory.Header_Integers",
    "Cur_Memory.Num_Body_Doubles",
    "Cur_Memory.Num_Body_Integers",
    "Cur_Memory.Object_Num_Points",
    "Cur_Memory.Load_Object_Fcn",
    "Cur_Memory.Fcn_Data_Type"
};

typedef enum {
    MEM_TYPE=0, NUM_OBJECTS,
    NUM_HEADER_DOUBLES, NUM_HEADER_INTEGERS,
    INIT_HEADER_FCN,
    HEADER_DOUBLES, HEADER_INTEGERS,
    NUM_BODY_DOUBLES, NUM_BODY_INTEGERS,
    OBJECT_NUM_POINTS,
    LOAD_OBJECT_FCN,
    FCN_DATA_TYPE
};

void
save_install()
{
  char  dirname[SIZE_OF_DIR_PLUS_FNAME];
  int   get_cur_dir(), save_go();

  pm(CREATE_OBJ, SAVE_OBJ_NAME,
     CREATE_ELEM, SAVE[SETTINGS], INT,
     CREATE_ELEM, SAVE[CONFIG], INT,
     CREATE_ELEM, SAVE[TRAJ], INT,
     CREATE_ELEM, SAVE[FIXPT], INT,
     CREATE_ELEM, SAVE[CONT], INT,
     CREATE_ELEM, SAVE[PARAM], INT,
     CREATE_ELEM, SAVE[SELECT], INT,
     CREATE_ELEM, SAVE[FUNCT], INT,
     CREATE_ELEM, SAVE[DIRECTORY], STRNG,
     CREATE_ELEM, SAVE[FILENAME], STRNG,
     CREATE_ELEM, SAVE[GO], ADDRS,
     NULL);

  pm(INIT, SAVE[DIRECTORY],  ( SIZE_OF_DIR_PLUS_FNAME - SIZE_OF_FNAME ),
     INIT, SAVE[FILENAME], SIZE_OF_FNAME,
     INIT, SAVE[GO],
     NULL);

  get_cur_dir(dirname);		/* get defaults from system */
    
  pm(PUT, "Save.Directory", dirname,
     PUT_SAVABLE, SAVE[DIRECTORY], SAVE_NONE,
     /* leave Filename empty */
     PUT, SAVE[SETTINGS], TRUE,
     PUT, SAVE[CONFIG],   FALSE,
     PUT, SAVE[TRAJ],     FALSE,
     PUT, SAVE[FIXPT],    FALSE,
     PUT, SAVE[CONT],     FALSE,
     PUT, SAVE[PARAM],    FALSE,
     PUT, SAVE[SELECT],   FALSE,
     PUT, SAVE[FUNCT],     FALSE,
     PUT, SAVE[GO], save_go,
     NULL);

  pm(CREATE_OBJ, CUR_MEMORY_OBJ_NAME,
     CREATE_ELEM, CUR_MEMORY[MEM_TYPE], STRNG,
     INIT, CUR_MEMORY[MEM_TYPE], MAX_SHORT_STR,
     CREATE_ELEM, CUR_MEMORY[NUM_OBJECTS], INT,
     CREATE_ELEM, CUR_MEMORY[NUM_HEADER_DOUBLES], INT,
     CREATE_ELEM, CUR_MEMORY[NUM_HEADER_INTEGERS], INT,
     CREATE_ELEM, CUR_MEMORY[INIT_HEADER_FCN], FNCT,
     CREATE_ELEM, CUR_MEMORY[HEADER_DOUBLES], DBL_LIST,
     CREATE_ELEM, CUR_MEMORY[HEADER_INTEGERS], INT_LIST,
     CREATE_ELEM, CUR_MEMORY[NUM_BODY_DOUBLES], INT,
     CREATE_ELEM, CUR_MEMORY[NUM_BODY_INTEGERS], INT,
     CREATE_ELEM, CUR_MEMORY[OBJECT_NUM_POINTS], INT,
     CREATE_ELEM, CUR_MEMORY[LOAD_OBJECT_FCN], FNCT,
     CREATE_ELEM, CUR_MEMORY[FCN_DATA_TYPE], STRNG,
     INIT, CUR_MEMORY[FCN_DATA_TYPE], MAX_SHORT_STR,
     NULL);
  
}

void
  save_reset()
{
}
