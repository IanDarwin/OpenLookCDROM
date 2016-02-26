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
/* defaults values for the continuation algorithms */
#define        CONT_STPSIZE	0.01
#define        CONT_MODE	0
#define        CONT_ITERS	500
#define        CONT_DIRECTION	FORWARD
#define        CONT_PARAM	0
#define        CONT_VARY_SWITCH	0
#define        CONT_JAC_UPDATE	0
#define        CONT_ABSERR	1.0e-5
#define        CONT_RELERR	1.0e-5
#define        CONT_MAXSTEP	0.1
#define        CONT_MINSTEP	0.001
#define        CONT_TARGET	0.0
#define        CONT_PARAM_FC	NULL
#define        CONT_PARAM_FC_SIZE	0
#define        CONT_FC		NULL
#define        CONT_SEARCH	FALSE
#define        CONT_PLOT_TYPE	-1

