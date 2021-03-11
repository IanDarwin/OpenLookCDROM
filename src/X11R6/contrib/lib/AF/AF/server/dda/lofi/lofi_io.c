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
#include "lofi_io.h"
#include "codec.h"
#include "dsp.h"
#include "dspio.h"
#include "dsp_func.h"

/*
 * Externs.
 */
extern int  errno;
extern char *getenv();


/*  These parameters are set by command line option:  see 'dda.c' */

extern int hifi_enable;
extern int daa_gain_control;
extern int dsp_portc_bits;
extern int dsp_cra;
extern int dsp_crb;

/*
 * Performs the raw hardware initialization.
 * Opens the device, maps device into user space, 
 * puts control bits in known state, and finally
 * loads (not enables) the dsp uKernel.
 */
void 
initLoFi(char *devName, char *kernel, lofiPhysDevice *pDev)
{
	CARD32 devTimeOffs;

	pDev->state = STATE_INIT;
	if ((pDev->fd = open(devName, O_RDWR, 0)) < 0) {
	    ErrorF("dda can't open %s, errno message \" %s \"\n", devName, 
		strerror(errno));
	    exit(1);
	}
	if (ioctl(pDev->fd, QIOLOFIINFO, &pDev->lofi) < 0) {
	    ErrorF("dda QIOLOFIINFO failed, errno message %s\n", 
		strerror(errno));
	    exit(1);
	}
	LoFiSetCSR(pDev->lofi->us_reg, FHS, 0);
	LoFiSetCSR(pDev->lofi->us_reg, FEA, 0);
	LoFiSetCSR(pDev->lofi->us_reg, FED, 0);
	LoFiSetCSR(pDev->lofi->us_reg, FGC, daa_gain_control);

	/* XXX eventually want to maintain a memory allocation list. */
	if ( !dspLoad(pDev->lofi->us_reg, kernel)) {
		ErrorF("dda could not load dsp\n");
		exit(1);
	}

	/* Query for the time offset and tell the driver where to look. */
	devTimeOffs = CODEC_TIME;
	if (ioctl(pDev->fd, QIOLOFITOFFS, &devTimeOffs) < 0) {
	    ErrorF("dda QIOLOFITOFFS failed, errno message %s\n", 
		strerror(errno));
	   exit(1);
	}
	pDev->mPtr = &pDev->lofi->us_reg->io_sram.srcopy[0].wd[0];

	dspWriteRAM(pDev, HIFI_TIME, 0);
	dspWriteRAM(pDev, CODEC_TIME, 0);

	/* set the DSP port */
	dspWriteRAM(pDev, PORTC_BITS, dsp_portc_bits);
	dspWriteRAM(pDev, DSP_CRA, dsp_cra);
	dspWriteRAM(pDev, DSP_CRB, dsp_crb);

	/* set up stuff needed for Teleport kernel */
	dspWriteRAM(pDev, DSP_LCTL, (TELELEFTHOOK | TELERIGHTHOOK) << 8);
	dspWriteRAM(pDev, DSP_RCTL, (TELELINEMASK | TELELOCALMASK) << 8);
	dspWriteRAM(pDev, DSP_MASKI, TELEMASKI << 8);
	dspWriteRAM(pDev, DSP_MASKO, (~TELEMASKO) << 8);

	/* configure the sizes of the play and record buffers */
	dspSetBufs(pDev, LENHWBUF, HIFI_HWBUF);

	/* Start up the codecs. */
	LoFiSetCSR(pDev->lofi->us_reg, FEA, 1);

	/* enable the DSP later! */
	pDev->state = STATE_OPEN;
}

void 
closeLoFi(lofiPhysDevice *pDev)
{
	LoFiSetCSR(pDev->lofi->us_reg, FHS, 0);
	LoFiSetCSR(pDev->lofi->us_reg, FEA, 0);
	LoFiSetCSR(pDev->lofi->us_reg, FED, 0);

	if (close(pDev->fd)) 
	    ErrorF("dda can't close device. errno message \" %s \"\n",
		strerror(errno));
	pDev->state = STATE_CLOSED;
}

void LoFiSetCSR(struct lofi_reg *lp, int field, int val)
{
    INT32    image;

    if ( (field < FCA) || (field > FGC) ) {
	FatalError("dda:LoFiSetCSR unknown csr field\n"); 
    }
    image = (field << 28) | (val << 24);
    lp->wr_csr = image;	MB();
}

void
lofiTliEnable(lofiPhysDevice *pDev)
{
	CARD32 image;

	image = pDev->lofi->us_reg->rd_csr;
	/* Disable Frame interrupt and enable Telephone interrupt. */
	LoFiSetCSR(pDev->lofi->us_reg, FIE, 0x2);
}
void
lofiTliDisable(lofiPhysDevice *pDev)
{
	CARD32 image;

	image = pDev->lofi->us_reg->rd_csr;
	LoFiSetCSR(pDev->lofi->us_reg, FIE, 0);
}



int lofiReadHostPort(struct lofi_info *lofi, int reg)
{
	return (lofi->us_reg->rd_hostmap(reg) & 0x0FF) ;
}

void lofiWriteHostPort(struct lofi_info *lofi, int reg, int d)
{
	lofi->us_reg->wr_hostmap(reg) = d;	MB();
}

/*
  Enable the DSP processor.
*/
void
lofiDSPEnable(lofiPhysDevice *pDev)
{
	CARD32	*tp;
	CARD32	start;

	LoFiSetCSR(pDev->lofi->us_reg, FED, 1);

	while(!(lofiReadHostPort(pDev->lofi, HOST_ISR) & HOST_ISR_HF2))
		;

/*
 *   Make sure the Hifi clocks are running (the server will hang if they are
 *   not.
 */
	if(hifi_enable) {
		tp = dspGetHifiTimePtr(pDev);
		start = *tp;
		msleep(NULL, 10);
		if (*tp == start) {
			ErrorF("Hifi DSP clock does not appear to be running:\n \
     	Is the DSP/Stereo switch set properly?\n");
		}
	}
}

void
lofiDSPDisable(lofiPhysDevice *pDev)
{
	LoFiSetCSR(pDev->lofi->us_reg, FED, 0);
}

void
lofiCodecEnable(lofiPhysDevice *pDev)
{
	LoFiSetCSR(pDev->lofi->us_reg, FEA, 1);
}
void
lofiCodecDisable(lofiPhysDevice *pDev)
{
	LoFiSetCSR(pDev->lofi->us_reg, FEA, 0);
}

#include <sys/time.h>
void msleep(struct lofi_reg *lp, int msecs)
{
  struct timeval timeout;

  timeout.tv_sec = msecs / 1000;
  timeout.tv_usec = msecs * 1000;
  select(0, NULL, NULL, NULL, &timeout);
}
