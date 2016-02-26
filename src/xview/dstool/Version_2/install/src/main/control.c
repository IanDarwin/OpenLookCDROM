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
 * control.c
 *
 * Procedures:
 *   control_install_init()
 *
 * Create and initialize CONTROL postmaster object
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *CONTROL_OBJ_NAME = "Control";

static char *CONTROL[] = { 
  "Control.Mode", "Control.Argc", "Control.Argv",
  "Control.Program_Name",
  "Control.Infile", "Control.Outfile", "Control.Debug",
  "Control.Dump_PM", "Control.Dump_PMHash",
  "Control.Sys_Reset", "Control.User_Reset",
  "Control.Pvm_Host"};

typedef enum {
  MODE=0, ARGC, ARGV, PROGRAM_NAME, INFILE, OUTFILE, DEBUG,
  DUMP_PM, DUMP_PMHASH,
  SYS_RESET, USER_RESET,
  PVM_HOST
  } CONTROL_t;

void
  control_install_init()
{
  extern void 
    pm_dump(), pm_hash_dump(), sys_reset(), user_reset();

  /* create CONTROL object and elements */
  pm(CREATE_OBJ, CONTROL_OBJ_NAME,
     CREATE_ELEM, CONTROL[MODE], INT,
     CREATE_ELEM, CONTROL[ARGC], INT,
     CREATE_ELEM, CONTROL[ARGV], STRNG_LIST,
     CREATE_ELEM, CONTROL[PROGRAM_NAME], STRNG,
     CREATE_ELEM, CONTROL[INFILE], STRNG,
     CREATE_ELEM, CONTROL[OUTFILE], STRNG,
     CREATE_ELEM, CONTROL[DEBUG], INT, 
     CREATE_ELEM, CONTROL[DUMP_PM], FNCT,
     CREATE_ELEM, CONTROL[DUMP_PMHASH], FNCT, 
     CREATE_ELEM, CONTROL[SYS_RESET], FNCT,
     CREATE_ELEM, CONTROL[USER_RESET], FNCT,
     CREATE_ELEM, CONTROL[PVM_HOST], INT,
     NULL);

  /* initialize elements */
  pm(PUT, CONTROL[MODE], DEFAULT_MODE,
     PUT, CONTROL[DEBUG], DEBUG_MODE,
     INIT, CONTROL[DUMP_PM],
     INIT, CONTROL[DUMP_PMHASH],
     INIT, CONTROL[SYS_RESET],
     INIT, CONTROL[USER_RESET],
     INIT, CONTROL[INFILE],1,
     INIT, CONTROL[OUTFILE],1,
     PUT, CONTROL[DUMP_PM], pm_dump,
     PUT, CONTROL[DUMP_PMHASH], pm_hash_dump, 
     PUT, CONTROL[SYS_RESET], sys_reset,
     PUT, CONTROL[USER_RESET], user_reset,
     PUT, CONTROL[PVM_HOST], FALSE,
     NULL);
}
