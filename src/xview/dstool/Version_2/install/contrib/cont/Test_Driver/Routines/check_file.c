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
 * check_file_to_read()    determines if file exists and is readable.  
 *    	Return TRUE if file is to be read; FALSE otherwise
 */

#include <stdio.h>
#include <constants.h>
 

/* 
 * check_file_to_read()    determines if file exists and is readable.  
 *    	Return TRUE if file is to be read; FALSE otherwise
 */

int
check_file_to_read(fname)
char	*fname;
{
FILE 		*fp,*fopen();

if ( !(fp=fopen(fname,"r"))){   /* file does not exist or cannot be read */
   fclose(fp);
   return(FALSE);
   }
else{
   fclose(fp);
   return(TRUE);
   }
}

system_mess_proc( flag, strng)
int	flag;
char	*strng;
{
   fprintf(stderr,"%s \n",strng);
}

mem_all_win()
{
}

cont_data_refresh()
{
}

fixpt_test()
{
}

new_fixpt_test()
{
}

manifold_test()
{
}

new_manifold_test()
{
}
