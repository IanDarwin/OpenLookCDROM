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
 * pvm.c
 *
 * Procedures:
 *   start_pvm_loop()
 *   rcv_tcl()
 *   rcv_memory()
 *
 *
 * This executable will be a slave pvm process.
 *
 */

#include <stdio.h>
#include <defaults.h>
#include <pm.h>

#include "pvmuser.h"
#include "pvm_messages.h"

void
  start_pvm_slave_loop()
{
  int mynum, msgtype, bytes, instance;
  char slave_prog[MAX_CMD_LINE_ARG_LEN], component[MAX_CMD_LINE_ARG_LEN];
  void pvm_rcv_tcl(), pvm_rcv_pm(), pvm_rcv_memory(), pvm_send_traj();

  /* change plotting routine so that points are returned */
  pm(INIT, "Flow.Plot_Function",
     PUT, "Flow.Plot_Function", pvm_send_traj,
     NULL);

  pm(GET, "Control.Program_Name", slave_prog, NULL);
  mynum = enroll(slave_prog);

  if (mynum < 0)
    {
      stop_execution("Cannot enroll in pvm.  Check pvmd.");
    }

  /* process messages from hostnode */
  msgtype = pvm_NONE;
  while (msgtype != pvm_LEAVE)
    {
      rcv(pvm_ALL);
      rcvinfo(&bytes, &msgtype, component, &instance);
      switch (msgtype) 
	{
	case pvm_TCL:
	  pvm_rcv_tcl();
	  break;
	case pvm_POSTMASTER:
	  pvm_rcv_pm();
	  break;
	case pvm_MEMORY:
	  pvm_rcv_memory();
	  break;
	case pvm_LEAVE:
	default:
	  break;
	}
    }

  system_mess_proc(1,"DsTool slave exiting from pvm.");
  leave();
}


void
  pvm_rcv_tcl()
{
  system_mess_proc(0,"pvm receiving tcl commands.");
}

void
  pvm_rcv_memory()
{
  system_mess_proc(0,"pvm receiving memory commands.");
}


