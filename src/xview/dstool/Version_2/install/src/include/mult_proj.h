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
#ifndef MULT_PROJ_HEADER
#define MULT_PROJ_HEADER

/*
 * data structure for hooking in projection functions
 * for multiple trajectories
 *
 */

struct Mult_Proj {
  char* name;			/* the name to appear on the menu */
  int (*proj_fn)();		/* the function */
} Mult_Proj;

extern struct Mult_Proj MULT_PROJS[];
extern int NUM_MULT_PROJS;

#endif
