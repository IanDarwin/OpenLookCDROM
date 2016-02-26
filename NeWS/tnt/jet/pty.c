#ident "@(#)pty.c	1.13 91/09/14"

/*
 *
 * Copyright (c) 1991 by Sun Microsystems, Inc.
 *
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <fcntl.h>
#include <utmp.h>
#include <pwd.h>
#include <sys/termios.h>
#include "term.h"
#include <signal.h>

/* This is the default configuration for the pty port */

static struct termios term_default = {
  (BRKINT | IGNPAR | ICRNL | IXON | IMAXBEL) ,05,0655,
  (ISIG | ICANON | ECHO | ECHOE | ECHOK | IEXTEN | ECHOCTL | ECHOKE)
#ifndef SUN4SVR4
  ,0
#endif
   ,{03,034,0177,025,04,033,00,00,021,023,032,031,022,017,027,026,00}
};


char dev_path[] = {"/dev/ptyp1"};
static char dev_major[] = {"pqrs"};
static char dev_minor[] = {"0123456789abcdef"};

int get_ptys()
{
  int fd,i,j;

  for(i=0;i<sizeof(dev_major);i++) {
    dev_path[8] = dev_major[i];
    for(j=0;j<sizeof(dev_minor); j++) {
      dev_path[9] = dev_minor[j];
      fd = open(dev_path, O_RDWR);
      if (fd>=0) {
	dev_path[5] = 't';
	return(fd);
      }
    }
  }
  return(-1);
}

static  struct termios tio;
void load_pty()
{
  int fd;

  fd = open("/dev/tty", O_RDONLY);

  if(ioctl(fd, TCGETS, &tio)<0) {
    tio = term_default;
  }

  close(fd);
}

int
spawn_shell( cmd, is_console, is_login)
char *cmd;
int is_console, is_login;
{
  int width,i, tty, pid;
  int child_pid;
  char buff[64];
  char *shell, *shcmd, *p;

  child_pid = vfork();

  if (child_pid == 0) {
    struct rlimit rlp;

    tty = open("/dev/tty", 2);
    ioctl(tty, TIOCNOTTY, (char *) NULL);

    if (getrlimit(RLIMIT_NOFILE,&rlp) < 0) {
      perror("getrlimit");
      exit(-1);
    }

    width = rlp.rlim_cur - 1;

    for(i=0; i < width; i++)
      close(i);

    open(dev_path, O_RDWR); /* fd 0 */
    open(dev_path, O_RDWR); /* fd 1 */
    open(dev_path, O_RDWR); /* fd 2 */

    for(i=0;i < NCCS; i++)
      term_default.c_cc[i] = tio.c_cc[i];

    term_default.c_cflag &= ~(CS8);
    term_default.c_cflag |= (CSIZE|PARENB);

    if(ioctl(0, TCSETS, & term_default) < 0) {
      perror("ioctl");
      exit(0);
    }

    pid = getpid();

#ifdef TIOCSCTTY
    setsid();
    ioctl(0, TIOCSCTTY, 0);
#endif
    ioctl(0, TIOCSPGRP, (char *)&pid);

#ifdef SUN4SVR4
    setpgrp();
#else
    setpgrp(0,0);
#endif

    close(open(dev_path, O_WRONLY, 0));

#ifdef TIOCCONS
    if (is_console) 
      ioctl(0, TIOCCONS, 1);
#endif


#if !SVR4 && !SUN4SVR4
    sigsetmask(0);
#endif

    putenv("TERM=vt100");
    putenv("EMULATOR=jet");

    shell = (char *) getenv("SHELL");

    if (shell==NULL) 
      shell = "/bin/sh";

    shcmd = p = shell;
    while (*p) {
      if (*p == '/')
	shcmd = p + 1;
      p++;
    }

    if (is_login) {
      sprintf(buff, "-%s", shcmd);
      shcmd = buff;
    }

    if (cmd != NULL)
      execl(shell, shcmd, "-ce", cmd, NULL);
    else
      execl(shell, shcmd, NULL);

    exit(1);
  }

  return (child_pid);
}

static int utmp_slot;

void
fill_out_utmp()
{
#ifndef SUN4SVR4
  int fd;
  struct utmp utmp;
  struct passwd *p;

  fd = open("/etc/utmp", O_WRONLY);
  if (fd < 0)
    return;

  utmp_slot = ttyslot();
  p = getpwuid(getuid());

  strcpy(utmp.ut_line, dev_path + 5);
  strcpy(utmp.ut_name,p->pw_name);
  utmp.ut_host[0] = '\0';
  utmp.ut_time = time(0);

  if (lseek(fd, utmp_slot * sizeof (struct utmp), SEEK_SET) == -1) {
    close(fd);
    return;
  }

  write(fd, (char *) &utmp, sizeof(utmp));
  close(fd);
#endif SUN4SVR4
}

void
clear_utmp()
{
#ifndef SUN4SVR4
  struct utmp utmp;
  int fd;

  fd = open("/etc/utmp", O_WRONLY);
  if (fd < 0)
    return;

  bzero((char *) &utmp, sizeof(utmp));

  if (lseek(fd, utmp_slot * sizeof (struct utmp), SEEK_SET) == -1) {
    close(fd);
    return;
  }
  write(fd, (char *) &utmp, sizeof(utmp));
  close(fd);
#endif
}

pty_set_size(fd, x, y)
int fd, x, y;
{
  struct winsize w;

  w.ws_row = y;
  w.ws_col = x;
  (void) ioctl(fd, TIOCSWINSZ, &w);


#if SVR4 || SUN4SVR4
  kill (0 - tcgetpgrp(fd), SIGWINCH);
#else
  killpg (tcgetpgrp(fd), SIGWINCH);
#endif
}
