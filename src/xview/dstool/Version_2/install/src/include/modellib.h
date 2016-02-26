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
#ifndef MODELLIB_HEADER
#define MODELLIB_HEADER

/* include file for accessing a dynamical system model */
/* dynamical system data structure definition */

struct	DS_DataS	{
		int	Category;
  		char	*DS_Name;	  /* name of dynamical system */
		int	(*ds_init)();	  /* initialization routine which sets postmaster variables */
	      };

extern	struct	DS_DataS DS_Sel[], DS_model;

extern  char	*DS_Category[];
extern  int	N_DS, N_DS_Category;

#endif


