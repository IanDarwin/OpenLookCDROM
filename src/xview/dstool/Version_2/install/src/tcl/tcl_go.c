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
 * tcl_go.c
 *
 * Procedures:
 *   tcl_go()
 *
 */
#include <stdio.h>

#include <defaults.h>
#include <constants.h>
#include <pm.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif


#include <tcl.h>

void
  init_tcl();

/*
 *
 * tcl_go()
 * This procedure uses tcl to interpret the stdin and control dstool
 *
 */
int
  tcl_go()
{
    tcl_go_from_src(stdin,stderr);
}

int 
tcl_go_from_src(fp_in,fp_out)

FILE
	*fp_in, *fp_out;


{

  char input[256], *cmd;
  Tcl_Interp *interp;
  int echo_status = *((int *) pm(GET,"Tcl.Echo",NULL));
/*  Tcl_CmdBuf buffer; */
  Tcl_DString *buffer_ptr;	/* For tcl 3.6 */
  Tcl_DString buffer;	/* For tcl 3.6 */

  /* create interpreter */
  if ( (interp = Tcl_CreateInterp()) == NULL)
    {
      system_mess_proc(1,"Cannot create tcl interpreter");
      return 0;
    }

  Tcl_DStringInit(&buffer);
  /* create cmd buffer */
/*  if ( (buffer = Tcl_CreateCmdBuf()) == NULL)
    {
      system_mess_proc(1,"Cannot create tcl command buffer");
      return 0;
    }*/

  /* setup tcl to recognize dstool commands */
  init_tcl(interp);

  /* execute commands from standard in */
  while (fgets(input,256,fp_in) != NULL)
    {
	if (echo_status)
	    fprintf(fp_out,"%s",input);
	cmd = Tcl_DStringAppend(&buffer,input,-1);

/*      cmd = Tcl_AssembleCmd(buffer,input); */
/*      if (cmd != NULL) */
      if ( Tcl_CommandComplete(cmd) )
	{
	  if (Tcl_Eval(interp,cmd) == TCL_ERROR) {
	      fprintf(fp_out,"dstool: TCL_ERROR on: %s\n",cmd);
	      fprintf(fp_out,"dstool: tcl: %s\n",interp->result);
	  }
	  else if (strlen(interp->result))
	      fprintf(fp_out,"%s\n",interp->result);
	  Tcl_DStringFree(&buffer);
	}
    }

  Tcl_DeleteInterp(interp);
  Tcl_DStringFree(&buffer);
/*  Tcl_DeleteCmdBuf(buffer); */
  return 0;
}

