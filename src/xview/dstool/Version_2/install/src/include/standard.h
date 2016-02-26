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
#ifndef STANDARD_HEADER
#define STANDARD_HEADER


extern double
    *dvector();			/* ARGS: (nl,nh) */

extern int
    free_dvector();		/* ARGS: (v,nl,nh) */


extern int
    *ivector();			/* ARGS: (nl,nh) */

extern int
    free_ivector();		/* ARGS: (v,nl,nh) */


extern double
    **dmatrix();		/* ARGS: (nrl,nrh,ncl,nch) */

extern int
    free_dmatrix();		/* ARGS: (m, nrl,nrh,ncl,nch)*/

#endif
