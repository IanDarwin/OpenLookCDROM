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
 * oned_install.c
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *ONED_OBJ_NAME = "OneD";

static char *ONED[] = {
  "OneD.Points",
  "OneD.Iter",
  "OneD.Sketch",
  "OneD.Diagonal",
  "OneD.Clear",
  "OneD.Forward",
  "OneD.Backward",
  "OneD.Continue"
};

typedef enum {
  POINTS=0, ITER, SKETCH, DIAGONAL, ONED_CLEAR, 
  ONED_FORWARD, ONED_BACKWARD, ONED_CONTINUE
} ONED_t;

void
  oned_install()
{
  void oned_sketch(), oned_diagonal(), 
  oned_clear(), oned_forward(), oned_backward(), oned_continue();

  pm(CREATE_OBJ, ONED_OBJ_NAME,
     CREATE_ELEM, ONED[POINTS], INT,
     CREATE_ELEM, ONED[ITER], INT,
     CREATE_ELEM, ONED[SKETCH], FNCT,
     CREATE_ELEM, ONED[DIAGONAL], FNCT,
     CREATE_ELEM, ONED[ONED_CLEAR], FNCT,
     CREATE_ELEM, ONED[ONED_FORWARD], FNCT,
     CREATE_ELEM, ONED[ONED_BACKWARD], FNCT,
     CREATE_ELEM, ONED[ONED_CONTINUE], FNCT,
     NULL);

  pm(PUT, ONED[POINTS], 1,
     PUT, ONED[ITER], 1,
     PUT, ONED[DIAGONAL], oned_diagonal,
     PUT, ONED[SKETCH], oned_sketch,
     PUT, ONED[ONED_CLEAR], oned_clear,
     PUT, ONED[ONED_FORWARD], oned_forward,
     PUT, ONED[ONED_BACKWARD], oned_backward,
     PUT, ONED[ONED_CONTINUE], oned_continue,
     NULL);
}

void
  oned_reset()
{

}



