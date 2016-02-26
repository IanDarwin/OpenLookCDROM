/* Function for calling the select(2) system call in linux.
 * Linux doesn't have a syscall() function, this replaces it
 * for select(), fcntl() and read().
 * Kudos to Rick Sladkey (jrs@world.std.com) for suggesting
 * this method. */

#include <sys/time.h>

/* #define __LIBRARY__ */
#include <syscall.h>
/* #undef __LIBRARY__ */

#ifdef DEBUG
#include <stdio.h>
#endif

#define SYS_sys_select SYS_select
#define SYS_sys_fcntl  SYS_fcntl
#define SYS_sys_read   SYS_read

/* Create function sys_select(), which can be called instead of
 * syscall(SYS_select, args...) */

_syscall1(int,sys_select, unsigned long *, buffer);

int linux_select(int width, fd_set *readfds, fd_set *writefds,
                 fd_set *exceptfds, struct timeval *timeout) {
  unsigned long selargs[5];
  static struct timeval tout_copy;

#ifdef DEBUG
  fprintf(stderr, "linux_select(%ld, %ld, %ld, %ld, %ld)...",
          (unsigned long)width, (unsigned long)readfds,
          (unsigned long)writefds, (unsigned long)exceptfds,
          (unsigned long)timeout);
#endif
  selargs[0] = (unsigned long)width;
  selargs[1] = (unsigned long)readfds;
  selargs[2] = (unsigned long)writefds;
  selargs[3] = (unsigned long)exceptfds;
  if (timeout != NULL) {
    tout_copy = *timeout;
    selargs[4] = (unsigned long)&tout_copy;
  }
  else
    selargs[4] = (unsigned long)timeout;
#ifndef DEBUG
  return sys_select(selargs);
#else
  {
    int res = sys_select(selargs);
    fprintf(stderr, "returned %d\n", res);
    return res;
  }
#endif
}

/* Replacement for syscall(SYS_fcntl,...) */
_syscall3(int, sys_fcntl, int, fildes, int, cmd, int, arg);

/* Replacement for syscall(SYS_read,...) */
_syscall3(int, sys_read, int, fildes, char *, buf, off_t, cnt);

