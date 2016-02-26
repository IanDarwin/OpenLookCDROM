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
#include <stdio.h>
#include <prop.h>

struct  Int_DataS	Int_Algol; /* structure to hold information about
				    one integrator */

char	*Int_Category[] = {"Standard",
			"Symplectic"};

/* driver routines for each installed integrator */
extern	int	rk4_driver(), euler_driver(), rkqc_driver(), bs_driver(), ab4_driver();

/* panel customization routines for each installed integrator */		
extern	int	rk4_init(), euler_init(), rkqc_init(), bs_init(), ab4_init();
		

struct	Int_DataS	Int_Sel[]={
				{0,"Runge-Kutta 4",rk4_init,rk4_driver,TRUE,TRUE,TRUE,TRUE},
				{0,"Euler",euler_init,euler_driver,TRUE,TRUE,TRUE,TRUE},
				{0,"Runge-Kutta 4QC",rkqc_init,rkqc_driver,TRUE,TRUE,TRUE,FALSE}, 	
				{0,"Bulirsch-Stoer",bs_init,bs_driver,TRUE,TRUE,TRUE,FALSE},
                                {0,"Adams-Bashforth 4",ab4_init,ab4_driver,TRUE,TRUE,TRUE,TRUE}	
				  };

int	N_Int = sizeof(Int_Sel) / sizeof(struct Int_DataS);  
int	N_Int_Category = sizeof(Int_Category) / sizeof(char *);

int	Int_Cur_Choice=0;	/* index of current integrator in Int_Sel[] */

