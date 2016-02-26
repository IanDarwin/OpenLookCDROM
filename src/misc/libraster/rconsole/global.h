/*
 * rconsole
 * Copyright 1990 Tom Lawrence, Brown University
 * Last Modification 6/14/90
 */

#include <raster.h>

#ifdef __sys_types_h
#define SUNOS4
#endif

#ifdef SUNOS4
#include <sys/termios.h>
#else
#include <sys/ioctl.h>
#endif

typedef struct tty_pair {
    int tty, pty;
} tty_pair;

char **EXEC;	/* executable to run */
char *FB;	/* name of frame buffer */
int debug;

#ifndef	FD_SETSIZE
/*
 * The following is defined just in case someone should want to run
 * this on a 4.2 system.
 */
#define	FD_SETSIZE	32
#define	FD_SET(n, p)	((p)->fds_bits[0] |= (1<<(n)))
#define	FD_CLR(n, p)	((p)->fds_bits[0] &= ~(1<<(n)))
#define	FD_ISSET(n, p)	((p)->fds_bits[0] & (1<<(n)))
#define FD_ZERO(p)	((p)->fds_bits[0] = 0)
#endif
