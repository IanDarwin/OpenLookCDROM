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
 * config.c
 *
 * Procedures:
 *   configure_dstool()
 *   load_startup_data()
 *
 */
#include <stdio.h>
#include <defaults.h>
#include <constants.h>
#include <pm.h>

/*
 * configure_dstool()
 *
 * sets up the basic configuration of dstool
 * so that control may be passed either to
 * the windowing system or the batch processor
 *
 */
void
  configure_dstool()
{
  pm(EXEC, "Model.Load", NULL);
}


/*
 * load_startup_data()
 *
 * loads in configuration/input user files
 *
 */
void
  load_startup_data() 

{
  char *longname =  (char *) calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));
  char *str, *getenv();
  FILE *fp, *fopen();
  int status, forcing = TRUE;

  /* look for file pointed to by DSTOOL_CONFIG environment variable
     and load it if it exists */
  if ( (str = getenv("DSTOOL_CONFIG")) != NULL)
    {
      if (check_file_to_read(str))
	{
	  fp = fopen(str, "r");
	  status = load_form_data(fp, &forcing, &forcing);
	  fclose(fp);
	}
    }

  /* look for file in current directory named "dstool.config" and load */
  if (check_file_to_read(CONFIG_FILENAME))
    {
      fp = fopen(CONFIG_FILENAME, "r");
      status = load_form_data(fp, &forcing, &forcing);
      fclose(fp);
    }

  /* load the infile, it it exists */
  pm(GET, "Control.Infile", longname, NULL);
  if (check_file_to_read(longname))
    {
      fp = fopen(longname, "r");
      status = load_form_data(fp, &forcing, &forcing);
      fclose(fp);
    }
  free(longname);

}

