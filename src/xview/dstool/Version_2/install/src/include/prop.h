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
#ifndef PROP_HEADER
#define PROP_HEADER

/* include file for defining the integration routines attached to dstool                               
   last change: 7 November 1990                        */

#include <constants.h>

struct	Int_DataS{
		int	Category;      /* 0,1,or 2 as Standard, Symplectic, or
					  Differential Algebraic */
  		char	*Int_Name;     /* name of integrator for menu */
		int	(*Int_Init)(); /* function pointer to routine specifying
					  initializations of propagation panel for 
					  this integrator */
		int	(*Int_Driver)(); /* function pointer to integrator */
		int	Enable_Nsteps; /* can the selected integ algorithm prop a fixed # of steps? */
		int	Enable_Fstop;  /* can the selected integ algorithm prop to a function value? */
		int	Enable_Tstop; /* can the selected integ algorithm prop to a final time? */
		int	Fixed_Step_Flag; /* does the selected integ routine use fixed steps? */
		int	Panel_Sel_Value; /*  */
		int	Num_Sel_Items; /*  */
		int	*Num_Sel_Choices; /*  */
		int	*Sel_Values; /*  */
		char	**Sel_Labels; /*  */
		char	***Sel_Choices;	/*  */
		int     Num_Dfields;    /* number of custom double fields */
		int     Num_Ifields;    /* number of custom integer fields */
		char    **Dfield_Names; /* 0th field assumed to be finite difference step.
					   1st field assumed to be error desired in event
					   stopping.
					   2nd field assumed to be min_dt in ode_stop */
		char    **Ifield_Names;	/* 0th fiedl assumed to be maximum number of
					   newton iterations in event stopping. */
		double  *Dfields; /* values for custom double fields */
		int     *Ifields; /*  values for custom integer fields */
	    };

extern	struct	Int_DataS Int_Sel[], Int_Algol;

extern  char	*Int_Category[];
extern int	N_Int, N_Int_Category;
extern int	Int_Cur_Choice;

#endif

