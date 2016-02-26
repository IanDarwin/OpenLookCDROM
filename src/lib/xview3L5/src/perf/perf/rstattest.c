/* Copyright (c) 1993 by Rudolf Koenig */
#include <rpc/rpc.h>
#include <rpcsvc/rstat.h>
#include <stdio.h>

main(ac, av)
  int ac;
  char *av[];
{
  struct statstime sx;

  if(ac != 2)
    exit(0);
  for(;;)
    {
      if(rstat(av[1], &sx))
	{
	  printf("No rstat...\n");
	  exit(0);
	}

      printf("cp_time: %d %d %d %d\n",
	sx.cp_time[0], sx.cp_time[1], sx.cp_time[2], sx.cp_time[3]);
      printf("dk_xfer: %d %d %d %d\n",
	sx.dk_xfer[0], sx.dk_xfer[1], sx.dk_xfer[2], sx.dk_xfer[3]);
      printf("pgin: %u, pgout %u, pswpin %u pswpout %u, intr %u\n",
	sx.v_pgpgin,sx.v_pgpgout,
	sx.v_pswpin,sx.v_pswpout,
	sx.v_intr);
      printf("ipack: %d, opack %d, ierr %d oerr %d, collis %d\n",
	sx.if_ipackets,sx.if_opackets,
	sx.if_ierrors,sx.if_oerrors,
	sx.if_collisions);
      printf("avg: %g %g %g\n", 
	(double)sx.avenrun[0] / 256,
	(double)sx.avenrun[1] / 256,
	(double)sx.avenrun[2] / 256);
      printf("context: %d\n", sx.v_swtch);
      fflush(stdout);
      sleep(1);
    }
}
