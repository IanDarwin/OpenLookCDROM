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
#ifndef SYMBOLS_HEADER
#define SYMBOLS_HEADER

#include <constants.h>

/* symbol and size definitions used by the print library */

extern int	Symbol_Codes[]; 
extern char	*Symbol_Labels[];
extern int	Num_Symbols; 

/* symbol and size definitions used by the 2D graphics library */

extern char	*Panel_Sym_Names[]; 
extern char	*Panel_Sym_Sizes[]; 
extern int      Panel_Sym_Codes[NUM_SYM_TYPES][NUM_SYM_SIZES];

#endif
