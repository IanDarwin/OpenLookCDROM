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
 * pvm_pm.c
 *
 * Procedures:
 *   pvm_pm_exec_all()
 *   pvm_pm_exec()
 *   pvm_pm_put_all()
 *   pvm_pm_put()
 *   load_pvm_from_pm()
 *   pvm_rcv_pm()
 *
 */
#include <stdio.h>
#include <stdlib.h>

#include <defaults.h>
#include <pm.h>
#include "pvm_messages.h"
#include "pvmuser.h"

/*
 * send a postmaster EXEC command to all slaves.
 *
 */
pvm_pm_exec_all(cmd)
char *cmd;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];
  int op = EXEC;

  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);

  initsend();
  putnint(&op,1);
  putstring(cmd);
  snd(slave_prog, pvm_ALL, pvm_POSTMASTER);
}


/*
 * send a postmaster EXEC command to a specific slave.
 *
 */
pvm_pm_exec(cmd, cpu)
char *cmd;
int cpu;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];
  int op = EXEC;

  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);

  initsend();
  putnint(&op,1);
  putstring(cmd);
  snd(slave_prog, *((int *) pm(GET, "Pvm.Slave_Instances", cpu, NULL)),
      pvm_POSTMASTER);
}


/*
 * send a postmaster EXEC command to a specific slave,
 * and tell it to reply when it is done
 */
pvm_pm_exec_reply(cmd, cpu)
char *cmd;
int cpu;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];
  int op = -EXEC;

  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);

  initsend();
  putnint(&op,1);
  putstring(cmd);
  snd(slave_prog, *((int *) pm(GET, "Pvm.Slave_Instances", cpu, NULL)),
      pvm_POSTMASTER);
}


/*
 * send a postmaster INIT command to all slaves.
 *
 */
pvm_pm_init_all(cmd, n)
char *cmd;
int n;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];
  int op = INIT;

  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);

  initsend();
  putnint(&op,1);
  putstring(cmd);
  putnint(&n,1);
  snd(slave_prog, pvm_ALL, pvm_POSTMASTER);
}


/*
 * send a postmaster INIT command to a specific slave.
 *
 */
pvm_pm_init(cmd, n, cpu)
char *cmd;
int n, cpu;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];
  int op = INIT;

  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);

  initsend();
  putnint(&op,1);
  putstring(cmd);
  putnint(&n,1);
  snd(slave_prog, *((int *) pm(GET, "Pvm.Slave_Instances", cpu, NULL)),
      pvm_POSTMASTER);
}


/*
 * Copy a current postmaster element to all slaves.
 * 
 */
pvm_pm_put_all(cmd)
char *cmd;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];

  load_pvm_from_pm(cmd);
  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);
  snd(slave_prog, pvm_ALL, pvm_POSTMASTER);
}


/*
 * Copy a current postmaster element to a specific slave.
 * 
 */
pvm_pm_put(cmd,cpu)
char *cmd;
int cpu;
{
  char slave_prog[MAX_CMD_LINE_ARG_LEN];

  load_pvm_from_pm(cmd);
  pm(GET, "Pvm.Slave_Name", slave_prog, NULL);
  snd(slave_prog, *((int *) pm(GET, "Pvm.Slave_Instances", cpu, NULL)),
      pvm_POSTMASTER);
}


load_pvm_from_pm(cmd)
char *cmd;
{
  char *str;
  int pmtype, op = PUT, n, len, i;

  initsend();
  putnint(&op,1);
  putstring(cmd);

  pmtype = pm_type(cmd, &n, &len);
  switch (pmtype)
    {
    case INT:
      putnint((int *) pm(GET, cmd, NULL), 1);
      break;
    case DBL:
      putndfloat((double *) pm(GET, cmd, NULL), 1);
      break;
    case STRNG:
      str = (char *) calloc(++len, sizeof(char));
      pm(GET, cmd, str, NULL);
      putnint(&len,1);
      putstring(str);
      cfree(str);
      break;
    case INT_LIST:
      putnint(&n,1);
      for (i=0; i<n; i++)
	putnint((int *) pm(GET, cmd, i, NULL), 1);
      break;
    case DBL_LIST:
      putnint(&n,1);
      for (i=0; i<n; i++)
	putndfloat((double *) pm(GET, cmd, i, NULL), 1);
      break;
    case STRNG_LIST:
      system_mess_proc(0,"pm_put_all: this pm type not yet able to put.");
      break;
    case ADDRS:
    case MEMRY:
    case FNCT:
      system_mess_proc(0,"pm_put_all: cannot transfer this type of data!");
      break;
    case 0:
      break;
    default:
      system_mess_proc(0,"pm_put_all: this pm type is not recognized.");
      break;
    }
}


void
  pvm_rcv_pm()
{
  int status, op, pmtype, idata, n, len, *ildata, bytes, msgtype, instance;
  double ddata, *dldata;
  char str[MAX_LABEL_LEN], sdata;
  char component[MAX_CMD_LINE_ARG_LEN];

  
  status = getnint(&op, 1);
  status = getstring(str);
  pmtype = pm_type(str, NULL, NULL);
  
  if (pmtype == 0) return;

  switch(op)
    {
    case EXEC:
      if (pmtype == FNCT) pm(EXEC, str, NULL);
      else
	system_mess_proc(0,"rcv_pm: Can only execute functions!");
      /* fprintf(stderr, "pm(EXEC, %s, NULL)\n",str); */
      break;
    case -EXEC:
      if (pmtype == FNCT) pm(EXEC, str, NULL);
      else
	system_mess_proc(0,"rcv_pm: Can only execute functions!");
      /* now reply! */
      rcvinfo(&bytes, &msgtype, component, &instance);
      initsend();
      snd(component, instance, pvm_EXEC_REPLY);
      break;
    case PUT:
      switch (pmtype)
	{
	case INT:
	  status = getnint(&idata, 1);
	  /* fprintf(stderr, "pm(PUT, %s, %d, NULL)\n", str, idata); */
	  pm(PUT, str, idata, NULL);
	  break;
	case DBL:
	  status = getndfloat(&ddata, 1);
	  /* fprintf(stderr, "pm(PUT, %s, %lf, NULL)\n", str, ddata); */
	  pm(PUT, str, ddata, NULL);
	  break;
	case STRNG:
	  status = getnint(&len, 1);
	  status = getstring(&sdata);
	  /* fprintf(stderr, "pm(PUT, %s, %d, NULL)\n", str, idata); */
	  pm(INIT, str, len, 
	     PUT, str, sdata, NULL);
	  break;
	case INT_LIST:
	  status = getnint(&n, 1);
	  ildata = (int *) malloc(n*sizeof(int));
	  status = getnint(ildata, n);
	  /* fprintf(stderr, "pm(PUT_LIST, %s, %d, [ ",str,n);
	     for(i=0; i<n; i++) fprintf(stderr,"%d ", ildata[i]);
	     fprintf(stderr, "], NULL)\n"); */
	  pm(INIT, str, n,
	     PUT_LIST, str, 0, n-1, ildata, NULL);
	  free(ildata);
	  break;
	case DBL_LIST:
	  status = getnint(&n, 1);
	  dldata = (double *) malloc(n*sizeof(double));
	  status = getndfloat(dldata, n);
	  /* fprintf(stderr, "pm(PUT_LIST, %s, %d,[ ",str,n);
	     for(i=0; i<n; i++) fprintf(stderr,"%lf ", dldata[i]);
	     fprintf(stderr, "], NULL)\n"); */
	  pm(INIT, str, n,
	     PUT_LIST, str, 0, n-1, dldata, NULL);
	  free(dldata);
	  break;
	case STRNG_LIST:
	  system_mess_proc(0,"rcv_pm: this pm type not yet able to rcv.");
	  break;
	case ADDRS:
	case MEMRY:
	case FNCT:
	  system_mess_proc(0,"rcv_pm: cannot transfer this type of data!");
	  break;
	case 0:
	  system_mess_proc(0,"rcv_pm: received an unknown postmaster oject.");
	  break;
	default:
	  system_mess_proc(0,"rcv_pm: this pm type is not recognized.");
	  break;
	}
      break;
    case INIT:
      status = getnint(&n,1);
      if (n<0)
	pm(INIT, str, NULL);
      else
	pm(INIT, str, n, NULL);
      break;
    default:
      fprintf(stderr,"DsTool_pvm: INVALID pm op received: %d %s.\n", op, str);
      break;
    }
  
}
