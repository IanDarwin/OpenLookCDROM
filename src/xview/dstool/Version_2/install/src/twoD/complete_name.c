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
#include <constants.h>
#include <pm.h>

/* 
 * complete_filename() recieves a postmaster object and uses file
 * completion routines to expand the Filename and Directory entries
 * of that pm object.
 */
int
complete_filename( object )
char *object;
{
  int   k;
 /* char	*dir, file[SIZE_OF_FNAME], pmstr[SIZE_OF_DIR_PLUS_FNAME];*/
  char	dir[SIZE_OF_DIR_PLUS_FNAME], file[SIZE_OF_FNAME], pmstr[SIZE_OF_DIR_PLUS_FNAME];

  sprintf(pmstr, "%s.Directory", object);
  pm(GET, pmstr, dir, NULL);
  
  sub_in_filename(dir);		/* expand all environmental variables */
  k = separate_filename(file, dir); 
  dir[k] = 0;			/* dir is now the "pure" directory part */
  k = filename_completion(file, dir);
	
  if (k>=0)			/* full completion of parent */
    strcat(filename_as_directory(dir,dir), file);

  pm(PUT, pmstr, dir, NULL); 

  if (k>=0)			/* no error; return as much completion as we can */
    {
      sprintf(pmstr, "%s.Filename", object);
      pm(GET, pmstr, file, NULL);
      k = filename_completion(file, dir); 
      if (k >= 0) pm(PUT, pmstr, file, NULL);
    }
  return ( k );
}
