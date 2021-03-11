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
#if !defined(lint) && !defined(SABER)
static char lofiio_c_rcsid[]="$Header: /crl/audio/AF/server/RCS/max_io.c,v 1.12 1993/11/17 20:09:35 tml Exp $";
#endif

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
#include <stropts.h>
#include <server/include/misc.h>
#include <server/include/input.h>
#include <audiodev.h>

#include "physdevice.h"
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

int axpfd = 0;

axpIoctl (fd, cmd, data, len)
        int fd, cmd, len;
        char *data;
{
        struct strioctl ioc;

        ioc.ic_cmd = cmd;
        ioc.ic_timout = 10;
        ioc.ic_len = len;
        ioc.ic_dp = data;

        return ioctl (fd, I_STR, &ioc);
}

void 
initMAX(devName, pDev)
char *devName;
maxPhysDevice *pDev;
{
	int data [2];
	pDev->state = STATE_INIT;
	if ((pDev->fd = open(devName, O_RDWR | O_NDELAY, 0)) < 0) {
	    ErrorF("dda can't open %s, errno message \" %d \"\n", devName, 
		errno);
	    exit(1);
	}
	axpfd = pDev->fd;
	data [0] = 1; /* Enable */
        if (axpIoctl (pDev->fd, ISDNIOC_MIC_ENABLE, data, sizeof (int)) < 0) {
                FatalError("dda can't enable mic on %s, errno message \" %d \"\n", devName, errno);
        }
	if (axpIoctl (pDev->fd, ISDNIOC_MIC_INPUT, (int *)data, sizeof (int)) < 0) {
                FatalError("dda can't enable mic input on %s, errno message \" %d \"\n", devName, errno);
        }
        data [0] = 1; /* Speaker 1 */
        data [1] = 1; /* Enable */
        if (axpIoctl (pDev->fd, ISDNIOC_SPEAKER_ENABLE, data, sizeof(int)*2) < 0) {
                FatalError("dda can't enable speaker on %s, errno message \" %d \"\n", devName, errno);
        }
        if (axpIoctl (pDev->fd, ISDNIOC_SPEAKER_OUTPUT, &data [0], sizeof (int)*2) < 0) {
                FatalError("dda can't enable speaker output on %s, errno message \" %d \"\n", devName, errno);
        }
	axp_disable_stg(pDev->fd);

/*	sleep(5);    give circuit time for setup */
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

