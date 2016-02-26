/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/kbd.c,v 1.3 91/03/01 11:05:53 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/kbd.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/kbd.c,v $$Revision: 1.3 $";

#include <stdio.h>
#include <sys/signal.h>
#include "bitmap.h"
#include "defs.h"
#ifdef sun
#include <sys/time.h> 
#ifdef KBD
# include <sys/types.h>
# include <sundev/kbio.h>
# include <sys/ioctl.h>
#endif KBD
#endif

static int	bell_fd;

/* initialize bell, for Suns */
int
initbell()
   {
#ifdef BELL
   int i;
   if ( !debug  &&  (i=open("/dev/bell",1))>=0 )
      bell_fd = i;
   else
      bell_fd = -1;
#endif
   }



static int ring = 0;

/* turn on the bell */

void
bell_on()
   {
#ifdef BELL
   int	bell_off();

   if( bell_fd >= 0 ) {
      if (ring==0)
         write( bell_fd, "\002", 1 );
      signal( SIGALRM, bell_off );
      set_timer(15);		/* set alarm for 100'th seconds */
      ring++;
      return;
      }
#endif
/*    We are looking for some way to make a noise when ever possible.
      This line breaks when the console is redirected to an mgr window and
      mgr's stderr (fd 2) is the console.

   write( 2, "\007", 1 );
*/
   }

#ifdef BELL
/* turn off the bell */

int
bell_off(n)
int n;			/* signal #, ignored */
   {
   write(bell_fd,"\003",1);	/* turns the bell off */
   set_timer(0);		/* insure timer is off */
   ring = 0;
   }


/* reset the keyboard */

int
kbd_reset()
   {
   if( bell_fd >= 0 )
      write(bell_fd,"\001",1);	/* this resets the kbd and turns bell off */
   set_timer(0);		/* insure timer is off */
   ring = 0;
   }


int
set_timer(time)
int time;		/* time in 100'th of seconds */
   {
   struct	itimerval new,old;

   new.it_interval.tv_sec = 0L;
   new.it_interval.tv_usec = 0L;
   new.it_value.tv_sec = time/100;
   new.it_value.tv_usec = (time%100) * 10000;

   setitimer(ITIMER_REAL,&new,&old);
   }

#else
int
kbd_reset() {}
#endif

static int kbd_fd = -1;

/* initialize the keyboard, especially when it is a separate device */

initkbd()
{
#ifdef KBD
   int fd;

   if ((fd = set_kbd(1)) > 0) {
	kbd_fd = 0; 
   	close(kbd_fd);
	dup(fd);
	close(fd);
	}
   else if( fd == 0 )
	return;
   else  {
	if( debug )
	   fprintf(stderr,"Can't find keyboard, using stdin\n");
	kbd_fd = -1;
	}
#endif
}

/* set/reset direct mode
   When setting direct mode, returns file descriptor of keyboard */

int
set_kbd(how)
int how;			/* 1=direct, 0=no direct */
   {
#ifdef KBD
   int one = 1;
   int zero = 0;
   int cons;

#ifndef  KBD_CMD_RESET
#define  KBD_CMD_RESET	0x01	/* Should be in a header file, but .. */
#endif
   if (how == 0) {		/* make sure kbd is released */
      if (kbd_fd == -1)
	 return(-1);
      ioctl(kbd_fd, KIOCSDIRECT, &zero);	/* turn off direct mode */
      ioctl(kbd_fd, KIOCCMD, KBD_CMD_RESET);	/* reset the keyboard */
      close(kbd_fd);				/* close the keyboard */
      cons = open("/dev/console", 0);	/* put console messages back */
      ioctl(cons, TIOCCONS, &one);
      close(cons);
      return(0);
      }
   else {		/* open the kbd for input */
      kbd_fd = open("/dev/kbd", 0);
      ioctl(kbd_fd, KIOCSDIRECT, &one);		/* turn on direct mode */
      ioctl(kbd_fd, KIOCCMD, KBD_CMD_RESET);	/* reset the keyboard */
      }
   return(kbd_fd);
#else
	return(0);
#endif
   }

/* keep the kernels idea of the window size up to date.  This is the
   wrong mechanism for doing this, but enough stuff uses it so it
   probably won't hurt
 */

int
set_winsize(fd, rows, cols)
int fd;		/* fd of master size of terminal */
int rows, cols;		/* terminal size */
	{
#ifdef TIOCSWINSZ
	struct winsize size;
	size.ws_row = rows;
	size.ws_col = cols;
	return(ioctl(fd,TIOCSWINSZ,&size));
#endif
	}
