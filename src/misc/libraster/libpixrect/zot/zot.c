/********************************************************************
 **                                                                **
 **  ZOT! - a Pixrect-based screen manipulation package            **
 **         written for SUN workstations                           **
 **                                                                **
 **  Version 1.1 written by:                                       **
 **    Mark "Crimson" Friedman  (friedman@cis.ohio-state.edu)      **
 **    and John "Rawhide" Wile  (wile@cis.ohio-state.edu)          **
 **                                                                **
 **  Zot allows you to manipulate the pixels of your (or another   **
 **  user's)  screen in  several interesting, clever, and highly   **
 **  annoying ways.  Amaze your friends!  Bother your neighbors!   **
 **  Enrage your sysadmins!  All this in one neat little package.  **
 **                                                                **
 **                                           Enjoy!               **
 **                                           - Mark Friedman      **
 **                                                                **
 **  COMPILING:                                                    **
 **                                                                **
 **  To compile, type (or cut-n-paste) one of the following lines: **
 **                                                                **
       cc zot.c -o zot -lpixrect
       gcc -O -fstrength-reduce -fcombine-regs zot.c -lpixrect -o zot
 **                                                                **
 **  Please  note that Zot! was  written on a monochrome Sun 4     **
 **  SPARCstation  SLC.  It may not  work on other Suns (read:     **
 **  DISCLAIMER!), though the  standard Pixrect library should     **
 **  eliminate this  possibility.  Zot! has not been tested on     **
 **  any other system, and it is unknown how Zot! will perform     **
 **  on a color monitor.  If  you get Zot! working  on another     **
 **  platform, drop us a line  and tell us  about it (well, if     **
 **  you feel like it...).                                         **
 **                                                                **
 **  And yes, we know it's messy, but it's just for fun, right?    **
 **                                                                **
 **  COMMAND LINE OPTIONS:                                         **
 **                                                                **
 **  Standard modes are invoked by typing:                         **
 **                                                                **
 **    zot -<mode>                                                 **
 **                                                                **
 **  where <mode> is one of the following:                         **
 **                                                                **
 **    block       swaps blocks of pixels around the screen        **
 **    flip        flip and mirror your screen in half             **
 **    melt        slides the screen off in a melting fashion      **
 **    negative    gives a photo-negative of the screen            **
 **    slide       creates a worm that wanders the screen          **
 **    snow        snow falls down from the top of the screen      **
 **    static      another type of snow, like on your TV           **
 **    vh          vertical hold problems (like on your TV)        **
 **    weasel      weasely critters burrow across the screen       **
 **    zigzag      draws random connected vectors                  **
 **                                                                **
 **  For a list of modes, try typing 'zot' (or 'zot -help').       **
 **                                                                **
 **  Options for each mode are specified with:                     **
 **                                                                **
 **    zot -<mode> [options]                                       **
 **                                                                **
 **  Also, 'zot -<mode> -help' will give you the options for       **
 **  that particular mode.                                         **
 **                                                                **
 ********************************************************************/

#include <pixrect/pixrect_hs.h>
#include <stdio.h>
#include <signal.h>

/* define your favorite number generator here */
#define RANDOM  rand
#define SRANDOM srand

/* this is the flag to keeps track of interrupt signals for forever loops */
  static int cont = 1;

/********************************************************************
 **  signal routines                                               **
 ********************************************************************/

/* upon an interrupt signal, this procedure will be called */
void bailout()
{
  cont = 0;  /* signal an exit in a forever loop */
}

/* catch interrupt signals for a clean get-away */
void set_signals()
{
  signal(SIGINT,  bailout);
  signal(SIGTERM, bailout);
  signal(SIGQUIT, bailout);
}

/********************************************************************
 **  block-mode procedure                                          **
 ********************************************************************/

block_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     x, y, max_x, max_y, xx, yy, count, bsize = 50;

  /* parse arguments */
  if(argc > 1)
    for(count = 0; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
	case 'h':  /* help option */
	  printf("Usage: zot -block [-z <size>]\n");
	  printf("<size> is between 1 and 200 (inclusive) ");
	  printf("and defaults to 50.\n");
	  exit(0);
	  break;
	case 'z':  /* block size option */
	  if(count<argc-1)
	    bsize = atol(argv[++count]);
	  if(bsize < 1)
	    bsize = 1;
	  if(bsize > 200)
	    bsize = 200;
	  break;
        default:
	  break;
	}

  /* open the frame buffer and set signals */
  screen = pr_open("/dev/fb");
  set_signals();

  /* get screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* until an interrupt... */
  while(cont)
    {
      /* initial coordinates */
      x =  RANDOM() % (max_x / bsize) * bsize;
      y =  RANDOM() % (max_y / bsize) * bsize;
      /* new coordinates */
      xx = RANDOM() % (max_x / bsize) * bsize;
      yy = RANDOM() % (max_y / bsize) * bsize;
      /* copy block */
      pr_rop(screen, x, y, bsize, bsize, PIX_SRC, screen, xx, yy);
    }

  /* close the buffer */
  pr_close(screen);
}

/********************************************************************
 **  flip-mode procedure                                           **
 ********************************************************************/

void flip_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     max_x, max_y;
  int     count, high, low, flipmode = 1, mirror = 0;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
        case 'b':  /* both option */
	  flipmode = 2;
	  break;
        case 'h':
	  switch (*(++argv[count])) {
	  case 'o':  /* horizontal option */
	    flipmode = 3;
	    break;
	  default:  /* help option */
	    printf("Usage: zot -flip [flipmode]\n");
	    printf("flipmodes: -horizontal, -vertical (default), ");
	    printf("-both, -mirror\n");
	    exit (0);
	    break;
	  }
	  break;
        case 'm':  /* mirror option */
	  mirror = 1;
	  break;
	case 'v':  /* vertical option */
	  flipmode = 1;
	  break;
        default:
	  break;
	}

  /* open frame buffer */
  screen = pr_open("/dev/fb");

  /* get screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* decide flipmode */
  switch(flipmode) {
  case 3:  /* horizontal mode */
    for(low = 0, high = max_x - 2; low < high; low++, high--) {
      if(!mirror)
	pr_rop(screen, max_x - 1, 0, 1, max_y, PIX_SRC,
	       screen, low, 0);
      pr_rop(screen, low, 0, 1, max_y, PIX_SRC,
	     screen, high, 0);
      if(!mirror)
	pr_rop(screen, high, 0, 1, max_y, PIX_SRC,
	       screen, max_x - 1, 0);
    }
    break;
  case 2:  /* both mode */
    for(low = 0, high = max_x - 2; low < high; low++, high--) {
      if(!mirror)
	pr_rop(screen, max_x - 1, 0, 1, max_y, PIX_SRC,
	       screen, low, 0);
      pr_rop(screen, low, 0, 1, max_y, PIX_SRC,
	     screen, high, 0);
      if(!mirror)
	pr_rop(screen, high, 0, 1, max_y, PIX_SRC,
	       screen, max_x - 1, 0);
    }
    /* fall through to next case! */
  default:  /* vertical mode */
    for(low = 0, high = max_y - 2; low < high; low++, high--) {
      if(!mirror)
	pr_rop(screen, 0, max_y - 1, max_x, 1, PIX_SRC,
	       screen, 0, low);
      pr_rop(screen, 0, low, max_x, 1, PIX_SRC,
	     screen, 0, high);
      if(!mirror)
	pr_rop(screen, 0, high, max_x, 1, PIX_SRC,
	       screen, 0, max_y - 1);
    }
    break;
  }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  melt-mode procedure                                           **
 ********************************************************************/

/* random function according to gaussian distribution */
int randx(max, glevel)
     int  max, glevel;
{
  double rnum;
  int    ret;

  rnum = 0;
  for(ret = 0;ret<glevel;ret++)
    rnum+=((double) (RANDOM()%10000000))/10000000.0/((double)glevel);
  return((int) (rnum*((double) max)));
}

/* the melt procedure */
melt_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     x, y, max_x, max_y, ylim[1192];
  int     count, gauss = 3, size = 5, color = PIX_SET;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
        case 'b':  /* black option */
	  color = PIX_SET;
	  break;
	case 'h':  /* help option */
	  printf("Usage: zot -melt [options]\n");
	  printf(" Option:                range:   default:\n");
	  printf(" -gaussian_dist level   [1..>]      3\n");
	  printf(" -s size               [1..%d]     5\n", max_x/2);
	  printf(" -black/-white          -b/-w    -black\n");
	  exit(0);
	  break;
	case 'g':  /* gaussian dist option */
	  if(count < argc - 1)
	    gauss = atol(argv[++count]);
	  if(gauss < 1) gauss = 1;
	  break;
        case 's':  /* size option */
	  if(count < argc - 1)
	    size = atol(argv[++count]);
	  if(size < 1) size = 1;
	  break;
        case 'w':  /* white option */
	  color = PIX_CLR;
	  break;
        default:
	  break;
	}

  /* open frame buffer and set signals */
  screen = pr_open("/dev/fb");
  set_signals();

  /* find screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* set random seed */
  SRANDOM(time(0));

  /* initialize ylim */
  for(x = 0;x<max_x;x++)
    ylim[x] = 0;

  /* until an interrupt... */
  while(cont)
    {
      x = randx(max_x-size+1,gauss);
      y = ylim[x];
      ylim[x]++;
      if(ylim[x]>=max_y)
	break;
      pr_rop(screen,x,y+1,size,(max_y-y-2),PIX_SRC,screen,x,y);
      pr_vector(screen,x,y,x+size-1,y,color,1);
    }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  negative-mode procedure                                       **
 ********************************************************************/

void negative_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     max_x, max_y;
  int     count;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
        case 'h':  /* help option */
	  printf("Usage: zot -negative\n");
	  exit (0);
	  break;
        default:
	  break;
	}

  /* open frame buffer */
  screen = pr_open("/dev/fb");

  /* get screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* flip it */
  for(count = 0; count < max_y; ++count)
      pr_vector(screen, 0, count, max_x, count, PIX_NOT(PIX_DST), 1);

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  slide-mode procedure                                          **
 ********************************************************************/

void slide_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     max_x, max_y;
  int     x[50], y[50];
  int     count, speed = 10, move = 5, segments = 10, trail = 0;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
	case 'h':  /* help option */
	  printf("Usage: zot -slide [options...]\n");
	  printf(" Option:    range:   default:\n");
	  printf("-velocity  [1..100]     10\n");
	  printf("-length    [5...50]      5\n");
	  printf("-segments  [5...50]     10\n");
	  printf("-trail      on/off     off\n");
	  printf("-help    what? are you kidding?\n");
	  exit (0);
	  break;
        case 'l':  /* length option */
	  if(count < argc - 1)
	    move = atol(argv[++count]);
	  if(move < 5) move = 5;
	  if(move > 50) move = 50;
	  break;
        case 's':  /* segments option */
	  if(count < argc - 1)
	    segments = atol(argv[++count]);
	  if(segments < 5) segments = 5;
	  if(segments > 50) segments = 50;
	  break;
        case 't':
	  trail = 1;
	  break;
	case 'v':  /* trail option */
	  if(count < argc - 1)
	    speed = atol(argv[++count]);
	  if(speed < 1) speed = 1;
	  if(speed > 100) speed = 100;
	  break;
        default:
	  break;
	}

  /* open frame buffer and set signals */
  screen = pr_open("/dev/fb");
  set_signals();

  /* find screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* set random seed */
  SRANDOM(time(0));

  /* set initial speed */
  speed = 100000 / speed;

  /* initialize x[] and y[] */
  for(count = 0; count < segments; count++) {
    x[count] = max_x /2;
    y[count] = max_y /2;
  }

  /* until an interrupt... */
  while (cont) {
    x[segments - 1] = x[segments - 2] - move + RANDOM() % (move * 2 + 1);
    y[segments - 1] = y[segments - 2] - move + RANDOM() % (move * 2 + 1);

    /* set boundaries */
    if(x[segments - 1] > max_x) x[segments - 1] = max_x;
    if(x[segments - 1] < 0)     x[segments - 1] = 0;
    if(y[segments - 1] > max_y) y[segments - 1] = max_y;
    if(y[segments - 1] < 0)     y[segments - 1] = 0;

    pr_vector(screen, x[segments - 2], y[segments - 2],
	      x[segments - 1], y[segments - 1], PIX_NOT(PIX_DST), 1);
    if(!trail)
      pr_vector(screen, x[0], y[0], x[1], y[1], PIX_NOT(PIX_DST), 1);

    for(count = 1; count < speed; count++) ;

    for(count = 0; count < segments - 1; count++) {
      x[count] = x[count + 1];
      y[count] = y[count + 1];
    }
  }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  snow-mode procedure                                           **
 ********************************************************************/

void snow_main(argc, argv)
     int argc;
     char *argv[];
{
  Pixrect *screen;
  int     count, x,*y, max_x, max_y;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
        case 'h':  /* help option */
	  printf("Usage: zot -snow\n");
	  exit (0);
	  break;
        default:
	  break;
	}

  /* open frame buffer and set signals */
  screen =  pr_open("/dev/fb");
  set_signals();

  /* find screen dimensions */
  max_x = (screen->pr_size.x);
  max_y = (screen->pr_size.y);

  y = (int *) (malloc(max_x * sizeof(int)));

  for(x = 0; x < max_x; x++)
    {
      y[x] = RANDOM() % max_y;
      pr_vector(screen, x, y[x], x, y[x], PIX_NOT(PIX_DST), 0);
    }

  /* until an interrupt... */
  while(cont)
    for(x = 0; x < max_x; x++)
      {
	pr_vector(screen, x, y[x], x, y[x], PIX_NOT(PIX_DST), 0);
	y[x] = (y[x] + (RANDOM() % 5 +1)) % max_y;
	pr_vector(screen, x, y[x], x, y[x], PIX_NOT(PIX_DST), 0);
      }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  static-mode procedure                                         **
 ********************************************************************/

#define CYCLES 10
void static_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect        *screen, *savebuf, *temp[CYCLES];
  int            count, numtimes = 20, dub = 0, max_x, max_y, linebytes;
  int            cnt, i, j;
  long           *addr[CYCLES], *a, *b;
  unsigned long  r;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
	case 'n':  /* number of times */
	  if(count < argc - 1)
	    numtimes = atol(argv[++count]);
	  if(numtimes < 1) numtimes = 1;
	  break;
	case 'd':  /* double-size pixels */
	  dub = 1;
	  break;
        default: /* help option */
	  printf("Usage: zot -static [-numtimes <numtimes>] [-double]\n");
	  exit (0);
	  break;
	}

  /* open frame buffer */
  screen = pr_open("/dev/fb");

  /* find screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* set random seed */
  SRANDOM(time(0));

  /* save the screen for restoring */
  savebuf = mem_create(max_x, max_y, screen->pr_depth);

  /* initialize temp[] */
  for(i = 0; i < CYCLES; i++)
    temp[i] = mem_create(max_x, max_y, 1);

  pr_rop(savebuf,0,0,max_x,max_y,PIX_SRC,screen,0,0);
  linebytes = mpr_prlinebytes(temp[0]);
  for(i = 0;i<CYCLES;i++)
    addr[i] = (long*) mpr_primage(temp[i]);

  for(i = 0;i<CYCLES;i++)
    {
      a = addr[i];
      if ( dub )
	{
	  for(j = max_y;j>0;j-=2,a+=linebytes/sizeof(long)*2)
	    {
	      b = a;
	      for(cnt = linebytes/sizeof(long);cnt>0;--cnt,++b)
		{
		  r = RANDOM();
		  r = r & 0x55555555;
		  *b = r | ( r << 1 );
		}
	    memcpy(b, a, linebytes);
	    }
	}
      else
	{
	  for(j = max_y;j>0;--j,a+=linebytes/sizeof(long))
	    {
	      b = a;
	      for(cnt = linebytes/sizeof(long);cnt>0;--cnt,++b)
		{
		  r = RANDOM();
		  *b = r ^ ( r << 1 );
		}
	    }
	}
    }

  for(j = 0;j<numtimes;j++)
    for(i = 0;i<CYCLES;i++)
      pr_rop(screen, 0, 0, max_x, max_y, PIX_SRC, temp[i], 0, 0);

  pr_rop(screen, 0, 0, max_x, max_y, PIX_SRC, savebuf, 0, 0);

  /* close frame buffers */
  pr_close(savebuf);
  for(i = 0; i<CYCLES; i++)
    pr_close(temp[i]);
  pr_close(screen);
}

/********************************************************************
 **  vh-mode procedure                                             **
 ********************************************************************/

void vh_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     max_x, max_y, scroll_rate = 1, vert = 1, count;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
        case 'h':
	  switch (*(++argv[count])) {
	  case 'o':  /* horizontal mode */
	    vert = 0;
	    break;
	  default:  /* help mode */
	    printf("Usage: zot -vh [-speed <int>] [scrollmode]\n");
	    printf("scrollmodes: -vertical (default) -horizontal\n");
	    exit (0);
	    break;
	  }
	  break;
	case 's':  /* speed mode */
	  if(count < argc - 1)
	    scroll_rate = atol(argv[++count]);
	  if(scroll_rate < 1) scroll_rate = 1;
	  if(scroll_rate > 100) scroll_rate = 100;
	  break;
        case 'v':  /* vertical mode */
	  vert = 1;
	  break;
        default:
	  break;
	}

  /* open frame buffer and set signals */
  screen = pr_open("/dev/fb");
  set_signals();

  /* find screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  if(scroll_rate > 0 && vert == 1)
    while(cont)
      {
	pr_rop(screen, 0, scroll_rate, max_x, max_y - scroll_rate,
	       PIX_SRC, screen, 0, 0);
	pr_rop(screen, 0, 0, max_x, scroll_rate,
	       PIX_SRC, screen, 0, max_y - scroll_rate);
      }
  else if(scroll_rate < 0 && vert == 1)
    {
      scroll_rate = -scroll_rate;
      while(cont)
	{
	  pr_rop(screen, 0, max_y - scroll_rate, max_x, scroll_rate,
		 PIX_SRC, screen, 0, 0);
	  pr_rop(screen, 0, 0, max_x, max_y - scroll_rate,
		 PIX_SRC, screen, 0, scroll_rate);
	}
    }
  else if(scroll_rate > 0)
    while(cont)
      {
	pr_rop(screen, scroll_rate, 0, max_x - scroll_rate, max_y,
	       PIX_SRC, screen, 0, 0);
	pr_rop(screen, 0, 0, scroll_rate, max_y,
	       PIX_SRC, screen, max_x - scroll_rate, 0);
      }
  else if(scroll_rate < 0)
    {
      scroll_rate = -scroll_rate;
      while(cont)
	{
	  pr_rop(screen, max_x - scroll_rate, 0, scroll_rate, max_y,
		 PIX_SRC, screen, 0, 0);
	  pr_rop(screen, 0, 0, max_x - scroll_rate, max_y,
		 PIX_SRC, screen, scroll_rate, 0);
	}
    }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  weasel-mode procedure                                         **
 ********************************************************************/

#define MAXBITS 1024
void weasel_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     count, max_x, max_y, x, y, new_x, new_y, lim_y, lim_x, a, counter;
  double  bitx[MAXBITS], bity[MAXBITS], speed[MAXBITS];

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
        case 'h':  /* help option */
	  printf("Usage: zot -weasel\n");
	  exit (0);
	  break;
        default:
	  break;
	}

  /* open frame buffer */
  screen = pr_open("/dev/fb");

  /* find screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* set random seed */
  SRANDOM(time(0));

  /* set limits */
  lim_y = max_y * 3 / 4;
  lim_x = max_x * 3 / 4;

  /* initialize arrays */
  /* note that the speed array stays constant once intialized */
  /* (this speeds weasel up) */
  for(x = 0; x < MAXBITS; x++)
    {
      speed[x] = ((double) (RANDOM() % 100)) / 50.0 + 1.0;
      bitx[x] = lim_x;
      bity[x] = max_y;
    } /* for */
  x = lim_x;
  y = lim_y;
  counter = 0;

  /* weasel away until weasel falls off the screen */
  while(1)
    {
      new_x = x + (RANDOM() % 10 - 5);
      new_y = y + (RANDOM() % 10 - 5);
      if((new_x >= max_x) || (new_x < 0) || (new_y >= max_y) || (new_y < 0))
	break;
      pr_vector(screen, x, y, new_x, new_y, PIX_NOT(PIX_DST), 1);

      bitx[counter] = (double) (new_x);
      bity[counter] = (double) (new_y);
      pr_vector(screen, bitx[counter], bity[counter],
		bitx[counter], bity[counter], PIX_NOT(PIX_DST), 1);
      
      for(a = 0; a < MAXBITS; a++)
	{
	  if(bity[a] < (double) max_y)
	    {
	      pr_vector(screen, (int) bitx[a], (int) bity[a],
			(int) bitx[a], (int) bity[a], PIX_NOT(PIX_DST), 1);
	      bity[a] += speed[a];
	    }
	  if(bity[a] < (double) max_y)
	    pr_vector(screen, (int) bitx[a], (int) bity[a],
		      (int) bitx[a], (int) bity[a], PIX_NOT(PIX_DST), 1);
	}
      counter = (counter + 1) % MAXBITS;
      x = new_x;
      y = new_y;

    }

  /* finish letting the bits fall to the bottom of the screen */
  x = 1;
  while(x)
    {
      x = 0;
      for(a = 0; a < MAXBITS; a++)
	{
	  if(bity[a] < (double) max_y)
	    {
	      pr_vector(screen, (int) bitx[a], (int) bity[a],
			(int) bitx[a], (int) bity[a], PIX_NOT(PIX_DST), 1);
	      bity[a] += speed[a];
	    }
	  if(bity[a] < (double) max_y)
	    {
	      pr_vector(screen, (int) bitx[a], (int) bity[a],
			(int) bitx[a], (int) bity[a], PIX_NOT(PIX_DST), 1);
	      x = 1;
	    }
	}
    }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  zigzag-mode procedure                                         **
 ********************************************************************/

void zigzag_main(argc, argv)
     int   argc;
     char *argv[];
{
  Pixrect *screen;
  int     x, y, max_x, max_y, new_x, new_y;
  int     count, setmode = -2;

  /* parse arguments */
  if(argc > 1)
    for(count = 1; count < argc; count++)
      if(*argv[count] == '-')
	switch (*(++argv[count])) {
	case 'b':  /* black option */
	  setmode = 1;
	  break;
	case 'h':  /* help option */
	  printf("Usage: zot -zigzag [setmode]\n");
	  printf("setmodes: -black, -white, -fuzzy, ");
	  printf("-invert/-reverse (default)\n");
	  exit (0);
	  break;
	case 'f':  /* fuzzy option */
	  setmode = -1;
	  break;
        case 'i':  /* invert option */
        case 'r':
	  setmode = -2;
	  break;
        case 'w':  /* white option */
	  setmode = 0;
	  break;
        default:
	  break;
	}

  /* open frame buffer and set signals */
  screen = pr_open("/dev/fb");
  set_signals();

  /* find screen dimensions */
  max_x = screen->pr_size.x;
  max_y = screen->pr_size.y;

  /* set random seed */
  SRANDOM(time(0));

  x = max_x / 2;
  y = max_y / 2;

  /* until an interrupt... */
  while (cont) {
    new_x = RANDOM() % max_x;
    new_y = RANDOM() % max_y;

    switch(setmode) {
    case 1:  /* black mode */
      pr_vector(screen, x, y, new_x, new_y, PIX_SET, 1);
      break;
    case 0:  /* white mode */
      pr_vector(screen, x, y, new_x, new_y, PIX_CLR, 1);
      break;
    case -1:  /* fuzzy mode */
      pr_vector(screen, x, y, new_x, new_y, PIX_NOT(PIX_DST), 1);
      /* fall through to next case! */
    default:  /* invert mode */
      pr_vector(screen, x, y, new_x, new_y, PIX_NOT(PIX_DST), 1);
      break;
    }

    x = new_x;
    y = new_y;
  }

  /* close frame buffer */
  pr_close(screen);
}

/********************************************************************
 **  the main procedure                                            **
 ********************************************************************/

void main(argc, argv)
     int   argc;
     char *argv[];
{
  int help = 0;

  /* parse arguments */
  if(argc > 1) {
    if(*argv[1] == '-') {
      switch (*(++argv[1])) {
      case 'b':  /* block option */
	block_main((--argc), &argv[1]);
	break;
      case 'f':  /* flip option */
	flip_main((--argc), &argv[1]);
	break;
      case 'h':  /* help option */
	help = 1;
	break;
      case 'm':  /* melt option */
	melt_main((--argc), &argv[1]);
	break;
      case 'n':  /* negative option */
	negative_main((--argc), &argv[1]);
	break;
      case 's':
	switch (*(++argv[1])) {
	case 'l':  /* slide option */
	  slide_main((--argc), &argv[1]);
	  break;
	case 'n':  /* snow option */
	  snow_main((--argc), &argv[1]);
	  break;
	case 't':  /* static option */
	  static_main((--argc), &argv[1]);
	  break;
	default:  /* help option */
	  help = 1;
	  break;
	}
	break;
      case 'v':  /* vh option */
	vh_main((--argc), &argv[1]);
	break;
      case 'w':  /* weasel option */
	weasel_main((--argc), &argv[1]);
	break;
      case 'z':  /* zigzagoption */
	zigzag_main((--argc), &argv[1]);
	break;
      default:
	help = 1;
	break;
      }
    } else {
      help = 1;
    }
  } else {
    help = 1;
  }

  /* print help */
  if(help) {
    printf("Usage: zot <mode [options]>\n");
    printf("  modes: -block -flip -melt -negative -slide -snow -static");
    printf(" -vh -weasel -zigzag\n");
    printf("  note: ` zot -<mode> -help ' will list options for that mode.\n");
    printf("ZOT! was created by Mark \"Crimson\" Friedman ");
    printf("and John \"Rawhide\" Wile.\n");
  }
}
