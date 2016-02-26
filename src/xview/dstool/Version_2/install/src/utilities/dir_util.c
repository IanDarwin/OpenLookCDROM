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

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <stdlib.h>
#include <constants.h>


/*
 * dir_util.c - utilities for checking to see if files exist and	
 *	can be read or written.
 *	CONTENTS:
 * get_cur_dir()	fetches the current working directory
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>

#include <saveload.h>
#include <constants.h>

/* 
 * get_cur_dir()	fetches the current working directory
 * WARNING:  This function may be system dependent
 */

int
 get_cur_dir(dirname)
 char 	*dirname;
 {
   int   get_dstool_path();
   char *getcwd();

   if( get_dstool_path( dirname, DSTOOL_DATA_DIR ) == 0) 
     /* getwd(dirname);*/
     getcwd(dirname, SIZE_OF_DIR_PLUS_FNAME);

   return(0);
 }


/*
* this proc fetches a filesystem path based on an 
* environment variable.  This proc is architecture-specific!
*/

int 
get_dstool_path( path, type )
int	type;
char	*path;
{
  char	*getenv(), *env_value;

  switch (type) 
    {
     case DSTOOL_DIR:
       if( (env_value = getenv("DSTOOL")) == NULL ) return(FALSE);
       path = strcpy( path, env_value );
       break;
     case DSTOOL_COLOR_DIR:
       if( (env_value = getenv("DSTOOL_COLOR_DIR")) != NULL ) 
	   path = strcpy( path, env_value);
       else if ( (env_value = getenv("DSTOOL")) != NULL ) {
	   path = strcpy(path,env_value);
	   strcat(path,"/colormaps");
       }
       else 
	   return (FALSE);
       break;
     case DSTOOL_DATA_DIR:
       if( (env_value = getenv("DSTOOL_DATA_DIR")) == NULL ) return(FALSE);
       path = strcpy( path, env_value);
       break;
     case LPDEST:
       if( getenv("LPDEST") == NULL ) return( FALSE);
       path = strcpy( path, getenv("LPDEST") );
       break;
     case PRINTER:
       if( getenv("PRINTER") == NULL ) return( FALSE);
       path = strcpy( path, getenv("PRINTER") );
       break;
     case DSTOOL_PS_PROLOG:
       if( getenv("DSTOOL_PS_PROLOG") == NULL ) return( FALSE);
       path = strcpy( path, getenv("DSTOOL_PS_PROLOG") );
       break;
    }

  return( TRUE );
}
