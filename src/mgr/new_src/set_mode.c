/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/set_mode.c,v 1.3 91/03/01 11:05:58 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/set_mode.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/set_mode.c,v $$Revision: 1.3 $";

/* muck with tty modes */

#include <fcntl.h>
#include <sgtty.h>

/* set up tty input modes */

int set_tty(file)
int file;				/* file descriptor */
   {
   set_mode(file,RAW,ECHO,0);
   }

/* setup mouse input modes */

int set_mouseio(file)
int file;				/* file descriptor */
   {
   set_mode(file,RAW,ECHO,B1200);
   fcntl(file,F_SETFL,fcntl(file,F_GETFL,0)|FNDELAY); /* for 4.1 */
   return(ioctl(file,TIOCEXCL,0));
   }

/* reset input tty modes */

int reset_tty(file)
int file;				/* file descriptor */
   {
   set_mode(file,ECHO,RAW,0);
   }

/*
 *******************************************************************************
 *
 *	Set the terminal mode 
 */

static set_mode(file,on,off,speed)
int file;		/* file pointer */
int on;			/* flags to turn on */
int off;		/* flags to turn off */
{
	struct sgttyb buff;

	gtty(file,&buff);
	buff.sg_flags |= on;
	buff.sg_flags &= ~off;
	if (speed) 
	   buff.sg_ispeed = buff.sg_ospeed = speed;
	stty(file,&buff);
        return(0);
}

/* void tty association */

void_tty()
   {
   int tty;

   tty = open("/dev/tty",2);
   ioctl(tty,TIOCNOTTY,0);
   close(tty);
   }

/*********************************************************************/

/* save tty modes for getshell */

/* place to save tty modes */

static int t_ldisc;
static struct sgttyb t_sgttyb;
static struct tchars t_tchars;
static struct ltchars t_ltchars;
static int t_lflags;

save_modes(fd)
int fd;			/* fd to save tty modes from */
	{
   ioctl(fd,TIOCGETD,&t_ldisc);
   ioctl(fd,TIOCGETP,&t_sgttyb);
   ioctl(fd,TIOCGETC,&t_tchars);
   ioctl(fd,TIOCGLTC,&t_ltchars);
   ioctl(fd,TIOCLGET,&t_lflags);
	}

restore_modes(fd)
int fd;
	{
   ioctl(fd,TIOCSETD,&t_ldisc);
   ioctl(fd,TIOCSETP,&t_sgttyb);
   ioctl(fd,TIOCSETC,&t_tchars);
   ioctl(fd,TIOCSLTC,&t_ltchars);
   ioctl(fd,TIOCLSET,&t_lflags);
	}

adjust_mode(disc,flags)
int flags;		/* flags */
int disc;		/* line disc */
	{
   t_ldisc=disc;
   t_sgttyb.sg_flags = flags;
	}
