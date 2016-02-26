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

static char *TCL_OBJ_NAME = "Tcl";

static char *TCL[] = {
  "Tcl.Verbose",
  "Tcl.Echo",
  "Tcl.Directory",
  "Tcl.Filename"
};

typedef enum {
  TCL_VERBOSE=0, TCL_ECHO,
  TCL_DIRECTORY,TCL_FILENAME
} TCL_t;

void
  tcl_install()
{

  pm(CREATE_OBJ, TCL_OBJ_NAME,
     CREATE_ELEM, TCL[TCL_VERBOSE], INT,
     CREATE_ELEM, TCL[TCL_ECHO], INT,
     CREATE_ELEM, TCL[TCL_DIRECTORY], STRNG,
     CREATE_ELEM, TCL[TCL_FILENAME], STRNG,
     NULL);

  pm(INIT, TCL[TCL_DIRECTORY], SIZE_OF_DIR_PLUS_FNAME,
     INIT, TCL[TCL_FILENAME], SIZE_OF_FNAME,
     PUT, TCL[TCL_VERBOSE], 1,
     PUT, TCL[TCL_ECHO], 0,
     NULL);
}

void
  tcl_reset()
{

}
