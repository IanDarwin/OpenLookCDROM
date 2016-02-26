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
 * file_util.c - macros for checking the existence of files and whether 
 *	they are readable/writeable.
 * CONTENTS:
 * check_file_to_write_notice()    determines whether file exists.  If so, notify.
 *    	Return TRUE if file is to be written; FALSE otherwise
 * check_file_to_read()    determines if file exists and is readable.  
 *    	Return TRUE if file is to be read; FALSE otherwise
 */

#include <stdio.h>
#include <constants.h>

 
/* 
 * check_file_to_write()    
 * 	determines whether the file may be written to
 *      by opening in append mode.
 *    	Return TRUE if open is successful; FALSE otherwise
 * 
 * To see if the file exists already, use check file to read in
 * conjunction with this routine.
 */
int
  check_file_to_write(fname)
char	*fname;
{
  FILE *fp;
  int status = TRUE;

  if ( !(fp=fopen(fname,"a")) )
    status = FALSE;
  fclose(fp);

  return status;
}    
 
/* 
 * check_file_to_read()    
 *	determines if file exists and is readable.  
 *    	Return TRUE if file is to be read; FALSE otherwise
 */
int
  check_file_to_read(fname)
char	*fname;
{
  FILE *fp;
  int status = TRUE;
  
  if ( !(fp=fopen(fname,"r")))
    status = FALSE;
  fclose(fp);

  return status;
}    
