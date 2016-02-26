#include<stdio.h>
#include<sys/types.h>
#include<sys/timeb.h>

main(argc,argv)
  int argc;
  char *argv[];
  {
  char blurb[1024];
  time_t t;

  t = time(0);
  sprintf(blurb,"cp schedule schedule.%d",t);
  system(blurb);
  }
