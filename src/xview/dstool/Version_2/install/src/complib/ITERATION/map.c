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
 * map.c
 *
 * map_init()
 *
 */
#include <stdio.h>

#include <pm.h>
#include <prop.h>
#include <constants.h>
#include <defaults.h>

int
map_init()
{
   static int		ifields[]={10,15};
   static double	dfields[]={1.e-5,1.0e-8,1.0e-8};
   static int		sel_values[] = {0};
   static char		*ifield_names[]={"#MC: ","Newton Iter"};
   static char		*dfield_names[]={FINITE_DIFFERENCE_STEP_STRING,
					     "Min Step: ","Conv Crit: "};
   static char		*usr_sel_labels[] = {"Initial Guess: "};
   static int		num_choices[] = {2};
   static char		*choice0[] = {"Approx Inv","Monte Carlo"}, **choice_array[2];

 
   if( *((int *) pm( GET, "Model.Inverse_Flag", NULL )) != EXPLICIT_INV ) 
      {
       Int_Algol.Ifield_Names = ifield_names;
       Int_Algol.Dfield_Names = dfield_names;
       Int_Algol.Num_Ifields = 2;
       Int_Algol.Num_Dfields = 3;
       Int_Algol.Ifields = ifields;
       Int_Algol.Dfields = dfields;
       if( *((int *) pm( GET, "Model.Inverse_Flag", NULL )) != FALSE )
	  { /* APPROX_INV case */
           Int_Algol.Num_Sel_Items = 1;
           Int_Algol.Num_Sel_Choices = num_choices;
           Int_Algol.Sel_Labels = usr_sel_labels;
           Int_Algol.Sel_Values = sel_values;
	   choice_array[0] = choice0;
           Int_Algol.Sel_Choices = choice_array;
	   Int_Algol.Ifields[0] = 1; /* just one Monte Carlo choice chosen by
				        default since approx inverse is 
					available */
          }
       else
	   Int_Algol.Num_Sel_Items = 0;
      }
    else
      {
       Int_Algol.Num_Ifields = 0;
       Int_Algol.Num_Dfields = 0;
       Int_Algol.Num_Sel_Items = 0;
      }
}
