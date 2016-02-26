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
 * procedure to send file through tcl
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <tcl.h>

#include <constants.h>
#include <pm.h>

extern int tcl_to_pm();
extern int tcl_to_exec();
extern int tcl_to_open();
extern int init_tcl();


int
  tcl_readfile(filename)
char *filename;
{
  Tcl_Interp *interp;
  int status,verbose;
  FILE *in_fp;

  /* create interpreter */
  if ( (interp = Tcl_CreateInterp()) == NULL)
    {
      system_mess_proc(1,"Cannot create tcl interpreter");
      return -2;
    }
 
  /* setup tcl to recognize dstool commands */
  init_tcl(interp);

  /* read tcl input file */
  verbose = * ((int *) pm(GET,"Tcl.Verbose",NULL));
  if (verbose) {
      in_fp = fopen(filename, "r"); /* already checked to be readable */
      status = tcl_go_from_src(in_fp, stderr);
  }
  else
      status = Tcl_EvalFile(interp,filename); 
  if (status == TCL_ERROR) {
      fprintf(stderr,"dstool: tcl: %s\n",interp->result);
  }
  else if (strlen(interp->result))
      fprintf(stderr,"%s\n",interp->result);

  Tcl_DeleteInterp(interp);
  return 0;
}



int
  tcl_to_open(clientdata, interp, argc, argv)
ClientData clientdata;
Tcl_Interp interp;
int argc;
char *argv[];
{
  int i;

  if ( *((int *) pm(GET, "Control.Mode", NULL)) != WINDOWS_MODE ) 
    return TCL_OK;

  for (i=1; i<argc; i++)
    {
      if (strcmp(argv[i],"twoD") == 0)
	twoD_open(-1,DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"fixedpoint") == 0)
	periodic_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"orbit") == 0)
	orbit_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"multiple") == 0)
	mult_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"selected") == 0)
	sel_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"function") == 0)
	funct_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"defaults") == 0)
	def_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else if  (strcmp(argv[i],"browser") == 0)
	browser_open(DEFAULT_WIN_CONFIG,0,0,0,0);
      else
	fprintf(stdout,"dstool: tcl_to_open: INVALID WINDOW NAME %s\n",argv[i]);
    }

  return TCL_OK;
}

