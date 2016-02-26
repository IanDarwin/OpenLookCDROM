/*
 * rconsole
 * Copyright 1990 Tom Lawrence, Brown University
 * Last Modification 6/14/90
 */

#include <sys/types.h>
#include <stdio.h>

#include "global.h"

#include <stdio.h>
#ifdef SUNOS4
#include <sys/filio.h>
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#include <sundev/kbio.h>
#include <ctype.h>
#include <errno.h>

#include "ascii.h"
#include "get_char.h"

char kbd[] = "/dev/kbd";

int errno;

/*****************************************************************************
Attach stdin to /dev/kbd, and arrange so that keystrokes go to
/dev/kbd instead of /dev/console, which is the default.
*****************************************************************************/
void
kbd_mode()
{
	int i;

	close(0);
	if (open(kbd, O_RDWR) < 0) {	/* takes fd 0 */
		fputs("rconsole: open: ", stderr);
		perror(kbd);
		exit(1);
	}

	/*
	 * Set non blocking mode. it would be better to use blocking io for kbd
	 * input, but this only works in non-blocking mode under SunOS 4.1 for
	 * some reason
	 */
	i = 1;
	if (ioctl(0, FIONBIO, &i) < 0) {
		perror("rconsole: kbd_mode: FIONBIO");
		exit(1);
	}

	/* send keystrokes to /dev/kbd */
	i = 1;
	if (ioctl(0, KIOCSDIRECT, &i) < 0) {
		perror("rconsole: KIOCSDIRECT");
		exit(1);
	}
}


/*****************************************************************************
This routines provides flexible buffered input from a single file
descriptor as well as counting of the number of NL chars in the buffer
for jump scrolling.

The modes parameter is made up of optional flags OR'd together. The
flags are READ, indicating that the routine should attempt to read
more data from the file descriptor if the input buffer is empty; NBIO,
indicating that this read should be done in non-blocking io mode (if
no more data can be read, -1 is returned); RET_DATA, indicating that
the first unread byte in the buffer should be returned and the
internal pointer updated; and RET_NL, indicating that the number of NL
characters in the buffer from the current position to the first
non-printable character other than NL and CR should be returned. This
is used for jump scrolling. RET_DATA and RET_NL are mutually
exclusive.
*****************************************************************************/
int
get_char(fd, modes, maxrow, maxcol)
	int fd, modes, maxrow, maxcol;
{
	static char buf[4096];
	static int bufsize, offset, nbio;	/* 0 initially (bss) */
	int num_NL, x;

	/* if entire buffer has been read by caller */
	if (offset >= bufsize) {

		/* if we are not supposed to read anything */
		if (!(modes & READ))
			return (-1);

		/* if current mode is not same as requested mode */
		if ((modes & NBIO) != nbio) {

			/* set new mode */
			nbio = modes & NBIO;
			if (ioctl(fd, FIONBIO, &nbio) < 0) {
				perror("rconsole: get_char: FIONBIO");
				exit(1);
			}
		}
		/* get some data. If non blocking and nothing more to read,
		 * return -1. If read successful, set offset to beginning of
		 * buffer. */

		if ((bufsize = read(fd, buf, sizeof(buf))) < 1) {
			if (errno != EWOULDBLOCK) {
				perror("rconsole: get_char: read");
				exit(1);
			} else {
				return (-1);
			}
		}
		offset = 0;
	}

	/* if RET_NL specified, return number of NL chars up to first non
	 * printable other than NL or CR */
	if (modes & RET_NL) {
		num_NL = 0;

		for (x = offset; x < bufsize; x++) {
			if (!isprint(buf[x]) && buf[x] != NL && buf[x] != CR)
				break;
			if (buf[x] == NL)
				num_NL++;
			if (num_NL > maxrow - 2)
				break;
		}
		return (num_NL);
	}

	/* if RET_DATA specified, return first unread byte in buffer and update
	 * internal pointer */
	if (modes & RET_DATA)
		return (buf[offset++]);

	/* else caller has not requested any useful info */
	return (0);
}
