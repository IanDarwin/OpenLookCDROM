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
 * pvm_host.c
 *
 * Procedures:
 *   pvm_start()
 *   pvm_finish()
 *   pvm_status()
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

#include <constants.h>
#include <defaults.h>
#include <pm.h>

#include "pvm_messages.h"
#include "pvmuser.h"

/*
 * pvm_start
 *
 * Enroll as host into pvm
 *
 */
void
  pvm_start()
{
  int mynum, ncpu, nformats, status, i, ntot;
  char host_prog[MAX_CMD_LINE_ARG_LEN], slave_prog[MAX_CMD_LINE_ARG_LEN];
  void pvm_reset();

  /* enroll in pvm as host program */
  pm(GET, "Control.Program_Name", host_prog, NULL);
  if ( (mynum = enroll(host_prog)) < 0)
    {
      system_mess_proc(0,
		"pvm_start: Cannot enroll in pvm as host.  Check pvmd.");
      pm(PUT, "Control.Pvm_Host", FALSE, NULL);
      return;
    }
  pm(PUT, "Pvm.Instance", mynum, NULL);

  /* find out how many cpus are available */
  if (pstatus(&ncpu,&nformats) < 0)
    {
      system_mess_proc(0,"pvm_start: Cannot get status of pvmd.");
      leave();
      pm(PUT, "Control.Pvm_Host", FALSE, NULL);
      return;
    }
  pm(PUT, "Pvm.Ncpu", ncpu, NULL);
  fprintf(stdout, "DsTool: There are %d cpus, %d formats, for pvm.\n",
	  ncpu, nformats);

  /* initiate processes */
  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);
  pm(INIT, "Pvm.Slave_Instances", ncpu-1, NULL);
  ntot = 0;
  for (i=0; i<ncpu-1; i++)
    {
      status = initiate(slave_prog, (char *) NULL);
      if (status >= 0) 
	{
	  pm(PUT, "Pvm.Slave_Instances", ntot, status, NULL);
	  ntot++;
	}
    }
  pm(PUT, "Pvm.Nslaves", ntot, NULL);
  fprintf(stdout, "DsTool: Started %d pvm slaves.\n", ntot);

  pm(PUT, "Control.Pvm_Host", TRUE, NULL);
  pvm_reset();

}



void
  pvm_finish()
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];

  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);

  initsend();
  snd(slave_prog, pvm_ALL, pvm_LEAVE);
  leave();

  pm(PUT, "Control.Pvm_Host", FALSE, NULL);
}


void
  pvm_status()
{
  system_mess_proc(0,"pvm_status: NO IDEA !");
}

