/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/signal.h>
#include <server/include/misc.h>
#include <server/include/input.h>
#include <stropts.h>

#include "physdevice.h"
#include "bba.h"
#include "bba_reg.h"
#include "ringbuffers.h"
#include "max_io.h"
#include "codec.h"
#include "pscodec.h"

/*
 * Externs.
 */
extern int  errno;
extern char *getenv();

/*
 * Performs the raw hardware initialization.
 * Opens the device, maps device into user space, 
 * puts control bits in known state, and finally
 * loads (not enables) the dsp uKernel.
 */
struct bba_info my_bba_info;
#include <sys/filio.h>
#include <sys/stropts.h>
int sunfd = 0, sunfd1 = 0, sunctl = 0;

void 
initMAX(devName, pDev)
char *devName;
maxPhysDevice *pDev;
{
	pDev->state = STATE_INIT;
	if ((pDev->fd = open(devName, O_WRONLY | O_NDELAY, 0)) < 0) {
	    ErrorF("dda can't open %s, errno message \" %d \"\n", devName, 
		errno);
	    exit(1);
	}
	bzero(&my_bba_info, sizeof(struct bba_info));
	pDev->bba = &my_bba_info;
	sunfd = pDev->fd;
	if ((sunfd1 = open(devName, O_RDONLY | O_NDELAY, 0)) < 0) {
	    ErrorF("dda can't open %s, errno message \" %d \"\n", devName, 
		errno);
	    exit(1);
	}
	ioctl(sunfd1, I_FLUSH, FLUSHR); 
	ioctl(sunfd, I_FLUSH, FLUSHW); 
	sunctl = open("/dev/audioctl", O_RDWR);
	pDev->state = STATE_OPEN;
}

void 
closeMAX(pDev)
maxPhysDevice *pDev;
{
	if (close(pDev->fd)) 
	    ErrorF("dda can't close device. errno message \" %d \"\n",
		errno);
	pDev->state = STATE_CLOSED;
}

