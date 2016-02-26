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
#ifndef SYMBOLS_DEF_HEADER
#define SYMBOLS_DEF_HEADER

#include <constants.h>
/* symbol and size definitions used by the print library */

int	Symbol_Codes[]={ SMALL_POINT,MED_POINT,LARGE_POINT,HUGE_POINT,
			 SMALL_TRI,MED_TRI,LARGE_TRI,HUGE_TRI,
			 SMALL_CROSS,MED_CROSS,LARGE_CROSS,HUGE_CROSS,
			 SMALL_BOX,MED_BOX,LARGE_BOX,HUGE_BOX};
/*char	*Symbol_Labels[]={"Small Dot","Medium Dot","Large Dot","Huge Dot",
			"Small Triangle","Medium Triangle","Large Triangle","Huge Triangle",
			"Small Cross","Medium Cross","Large Cross","Huge Cross",
			"Small Box","Medium Box","Large Box","Huge Box"};*/

char	*Symbol_Labels[]={"Sm Dot","Md Dot","Lg Dot","XL Dot",
			"Sm Triangle","Md Triangle","Lg Triangle","XL Triangle",
			"Sm Cross","Md Cross","Lg Cross","XL Cross",
			"Sm Box","Md Box","Lg Box","XL Box"};

int	Num_Symbols = sizeof(Symbol_Codes)/sizeof(int);

/* symbol and size definitions used by the 2D graphics library */

char	*Panel_Sym_Names[]={"Dot","Cross","Triangle","Box"};
char	*Panel_Sym_Sizes[]={"Small","Medium","Large","Huge"};
int     Panel_Sym_Codes[NUM_SYM_TYPES][NUM_SYM_SIZES]=
				{ {SMALL_POINT,MED_POINT,LARGE_POINT,HUGE_POINT},
				{SMALL_CROSS,MED_CROSS,LARGE_CROSS,HUGE_CROSS},
				{SMALL_TRI,MED_TRI,LARGE_TRI,HUGE_TRI},
				{SMALL_BOX,MED_BOX,LARGE_BOX,HUGE_BOX}}; 

#endif
