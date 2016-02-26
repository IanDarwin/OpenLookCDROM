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
 * tcl.c
 *
 * Procedures:
 *   start_tcl_loop()
 *   tcl_sigint_handler()
 *
 */
#include <stdio.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <signal.h>

#include <pm.h>
#include <constants.h>
#include <memory.h>


/*
 *
 * start_tcl_loop()
 * This procedure is a list of tcl commands
 *
 */
void
  start_tcl_loop()
{
  FILE *fp;
  char filename[SIZE_OF_DIR_PLUS_FNAME];
  extern char *get_the_time(), *get_user_info(); 
      

  fprintf(stdout,"dstool: tcl mode BEGIN\n");

  /* test the output file */
  pm(GET, "Control.Outfile", filename, NULL);
  if ( !(fp = fopen(filename,"w")) )	/* cannot write */
    {
      fprintf(stdout,"dstool: start_tcl_loop: cannot open output file %s\n", 
	      filename);
      return;
    }
  fprintf(stdout,"dstool: tcl output in %s\n", filename);
  
  /* HERE ARE THE COMPUTATIONS */
  tcl_go();

  /* write the memory objects to the output file */
  fprintf(stdout,"dstool: tcl mode END.  Writing data to file.\n");
  fprintf(fp,"#  This data file is read and written by dstool\n");
  fprintf(fp, "#  Saved By: %s\n#  %s\n#\n", get_user_info(), get_the_time());
  fprintf(fp,"pm PUT Load.Model_Name %s\npm EXEC Load.If_New_Model_Fnct\n\n", get_ds_name());
  if ( *((int *)pm(GET, "Save.Settings", NULL)))  /* save the settings */
    save_settings(fp);
  write_data_obj(fp,(memory) pm(GET, "Memory.Traj", NULL), "Traj");
  write_data_obj(fp,(memory) pm(GET, "Memory.Mult", NULL), "Mult");
  write_data_obj(fp,(memory) pm(GET, "Memory.Fixed", NULL), "Fxpt");
  write_data_obj(fp,(memory) pm(GET, "Memory.Cont", NULL), "Cont"); 
  write_data_obj(fp,(memory) pm(GET, "Memory.Param", NULL), "Param");
  write_data_obj(fp,(memory) pm(GET, "Memory.Sel_Pt", NULL), "Sel_Pt");
  fclose(fp);
  
}


/*
 * This is the SIGINT handler used for tcl mode processing
 */
void 
  tcl_sigint_handler(sig, code, scp, addr)
int sig,code;
struct sigcontext *scp;
char *addr;
{
  if (sig == SIGINT)
    set_interrupt();
}
