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
#include <math.h>
#include <constants.h>
/*
 * NOTE( fjw 8/19/92): at some point, these functions ought to be rewritten in terms
 * of bitwise operations instead of using pow() which could result in an overflow 
 * if the number of options gets too large (greater than 15 options).  
 * For example, unmask could be rewritten as
 *	unmask(box,choice)
 *	int             *box;           
 *	int             choice;         
 *	{
 *	  while(choice != 0)
 *	   {
 *	     choice >>= 1;		/(* lop off highest bit *)/
 *	     *(++box) = ((choice & 01)? 1: 0); /(* is the lowest bit turned on? *)/
 *	   }
 *	}
 * which has the advantage of not requiring n_opt and dispensing with pow()
 */

/*
unmask()  decodes an option mask and therefore determines what the user chose as options.
This function is used when the user is allowed to choose between options on a non-exclusive
option setting.  XView returns a binary mask as a panel value for the setting.
This function inserts this mask into a properly dimensioned array.

Ex: If user has five options and has chosen the first, second, and fourth options,
then the window returns choice=11 since in binary, eleven is 1011 which has the first, 
second, and fourth bits turned on and the rest turned off.
In this case, unmask() returns 01011 in box.
*/
int 
unmask(n_opt,box,choice)
int             n_opt;          /* number of options on exclusive setting panel */
int             *box;           /* array of n_opt elements */
int             choice;         /* binary mask of user selection */
{
 
  for(n_opt -= 1;n_opt>=0; n_opt--)
    if (choice >= (int) pow(2., (double)n_opt ))
      {
	choice -= (int) pow(2., (double)n_opt );
	box[n_opt] = TRUE;
      }
    else 
      box[n_opt] = FALSE;
}

/*
 * mask() reverses the process taking an integer array and transforming it into 
 * an integer binary mask;
 * 
 * Ex: if box = {1, 0, 1, 1, 0} (ie, the first, third and forth bits are on)
 * then mask returns 13 which is 01101 in binary.
 * written 8/9/92 fjw
 */
int
mask(n_opt, box, choice)
int n_opt, *box, *choice;
{
  *choice = 0;
  for(n_opt -= 1; n_opt>=0; n_opt--)
    if( box[n_opt] )
      *choice += (int) pow(2., (double) n_opt);
  return( *choice );
}
