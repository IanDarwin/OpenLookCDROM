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

#include <sys/file.h>
#include <sys/ioctl.h>
#include <server/include/misc.h>
#include <server/include/input.h>
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
void 
initMAX(char *devName, maxPhysDevice *pDev)
{
	pDev->state = STATE_INIT;
	if ((pDev->fd = open(devName, O_RDWR, 0)) < 0) {
	    ErrorF("dda can't open %s, errno message \" %s \"\n", devName, 
		strerror(errno));
	    exit(1);
	}
	if (ioctl(pDev->fd, QIOBBAINFO, &pDev->bba) < 0) {
	    ErrorF("dda QIOBBAINFO failed, errno message %s\n", 
		strerror(errno));
	    exit(1);
	}
	pDev->state = STATE_OPEN;
}

void 
closeMAX(maxPhysDevice *pDev)
{
	if (close(pDev->fd)) 
	    ErrorF("dda can't close device. errno message \" %s \"\n",
		strerror(errno));
	pDev->state = STATE_CLOSED;
}
