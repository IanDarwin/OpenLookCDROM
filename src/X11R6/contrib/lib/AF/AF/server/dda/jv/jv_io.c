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
#include <sys/file.h>
#include <sys/ioctl.h>
#include <server/include/misc.h>
#include <server/include/input.h>
#include "physdevice.h"
#include "jv_io.h"
#include "dsp.h"
#include "dspio.h"

extern int dsp_portc_bits;
extern int dsp_cra;
extern int dsp_crb;

JvRegs		jv_regs;

extern JvRegs	_jvrgs;
#define	JVR	jv_regs

/*
 * Externs.
 */
extern int  errno;
extern char *getenv();
extern int JvInit(char *devName, JvRegs *jv);
extern int hifi_rate;
extern int hifi_rate_cr;

/*
 * Performs the raw hardware initialization.
 * Opens the device, maps device into user space, 
 * puts control bits in known state, and finally
 * loads (not enables) the dsp uKernel.
 */
void 
initJv(char *devName, char *kernel, jvPhysDevice *pDev)
{
	pDev->state = STATE_INIT;
	pDev->jv = &jv_regs;

	/*
	 * Does open, ioctls, etc.	
	 */
	if((pDev->fd  = JvInit(devName, pDev->jv) ) < 0) {
		ErrorF("could not open jvideo.");
		exit(1);
	}

	*JVR.audio.csr = hifi_rate_cr;		MB();
	*JVR.audio.csr = DSP_ENABLE_AUDIO;	MB();
        *JVR.audio.csr = DSP_DISABLE_CHIP;	MB();
        *JVR.audio.csr = DSP_NORMAL_MODE;	MB();

	/* XXX eventually want to maintain a memory allocation list. */
	if ( !dspLoad(pDev->jv, kernel))
		FatalError("dda could not load dsp\n");

	pDev->mPtr = (volatile CARD32 *) *JVR.audio.ram;

	dspWriteRAM(pDev, HIFI_TIME, 0);

	/* set the DSP port */
	dspWriteRAM(pDev, PORTC_BITS, dsp_portc_bits);
	dspWriteRAM(pDev, DSP_CRA, dsp_cra);
	dspWriteRAM(pDev, DSP_CRB, dsp_crb);

	/* configure the sizes of the play and record buffers */
	dspSetBufs(pDev, HIFI_HWBUF);

	/* enable the DSP later! */
	pDev->state = STATE_OPEN;
}

void 
closeJv(jvPhysDevice *pDev)
{
	*JVR.audio.csr = DSP_DISABLE_AUDIO;	MB();
	*JVR.audio.csr = DSP_DISABLE_CHIP;	MB();

	if (close(pDev->fd)) 
	    ErrorF("dda can't close device. errno message \" %s \"\n",
		strerror(errno));
	pDev->state = STATE_CLOSED;
}


void
jvDSPEnable(jvPhysDevice *pDev)
{
	*JVR.audio.csr = DSP_ENABLE_CHIP;	MB();


	while(!(*(pDev->jv->audio.isr_read)  & HOST_ISR_HF2))
		;
}

void
jvDSPDisable(jvPhysDevice *pDev)
{
	*JVR.audio.csr = DSP_DISABLE_CHIP;	MB();
}

void
jvEnableAudio(jvPhysDevice *pDev)
{
	*JVR.audio.csr = DSP_ENABLE_AUDIO;	MB();
}

void
jvDisableAudio(jvPhysDevice *pDev)
{
	*JVR.audio.csr = DSP_DISABLE_AUDIO;	MB();
}
