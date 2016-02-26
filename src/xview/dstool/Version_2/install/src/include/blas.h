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
extern int
    print_vector();		/* ARGS: (dim,v) */

extern double
    dot();			/* ARGS: (dim,u,v) */

extern double
    L2_norm();			/* ARGS: (dim,v) */

extern double
    L2_norm_sqr();		/* ARGS: (dim,v) */

extern double
    euclid_dist();		/* ARGS: (dim,u,v) */

extern double
    euclid_dist_sqr();		/* ARGS: (dim,u,v) */

extern double
    *normalize();		/* v set to be a unit vector
				  ARGS: (dim,v) */

extern double
    *proj_vector();		/* w = proj of u onto v
				   assumes w allocated
				  ARGS: (dim,u,v,w) */

extern double
    *orthog_vector();		/* w = proj of v orthogonal to u
				   assumes w allocated
				   ARGS: (dim,u,v,w) */

extern double
    cos_between();		/* ARGS: (dim,u,v) */

extern int
    same_side();		/* CHECK LATER
				  ARGS: (dim,c,v,u) */
