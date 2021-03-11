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

#include <server/include/misc.h>
#include <audioproto.h>
#include <audio.h>
#include "dda.h"
#include "lofi_io.h"
#include "physdevice.h"
#include "dsp.h"
#include "dsp_func.h"
#include "pscodec.h"
#include "codec.h"
#include <stdio.h>
#define	CBUF_OFFS	((MBX_OFFS-(4*LENHWBUF)) & 0x0FFF0)

/*
 * Reads a 24-bit value from the DSP's shared RAM
 */
int
dspReadRAM(lofiPhysDevice *pDev, int addr)
{
	return ((int)(pDev->mPtr[addr] >> 8));
}

/*
 * Writes a 24-bit value to the DSP's shared RAM
 */
void dspWriteRAM(lofiPhysDevice *pDev, int addr, int val)
{
	pDev->mPtr[addr] = val << 8;	MB();
}

/*
 * Convert a shared RAM address into a mapped address
 */

#define RAMPtr(dev,addr) ((dev)->mPtr + (addr))


/*
 * Returns a pointer to the Codec timer on the DSP.
 */
CARD32 *
dspGetCodecTimePtr(lofiPhysDevice *pDev)
{
	return (CARD32 *)RAMPtr(pDev,CODEC_TIME);
}


/*
 * Returns a pointer to the HIFI timer on the DSP.
 */
CARD32 *
dspGetHifiTimePtr(lofiPhysDevice *pDev)
{
	return (CARD32 *)RAMPtr(pDev,HIFI_TIME);
}


/*
 * Returns the mapped address of a codec's record buffer
 */
CARD32 *dspGetCodecRecBuf(lofiPhysDevice *pDev, int flag)
{
	int	addr;

	addr = flag ? CODEC_START1 : CODEC_START0;
	return (CARD32 *)RAMPtr(pDev, dspReadRAM(pDev, addr));
}


/*
 * Returns the mapped address of the code's play buffer
 */
CARD32 *dspGetCodecPlayBuf(lofiPhysDevice *pDev, int flag)
{
	int	ofs;

	ofs = dspReadRAM(pDev, flag ? CODEC_OFS1 : CODEC_OFS0);
	return dspGetCodecRecBuf(pDev, flag) + ofs;
}


/*
 * Returns the mapped address of a hifi record buffer
 */
CARD32 *dspGetHifiRecBuf(lofiPhysDevice *pDev, int flag)
{
	int	addr;

	addr = flag ? HIFI_START1 : HIFI_START0;
	return (CARD32 *)RAMPtr(pDev, dspReadRAM(pDev, addr));
}


/*
 * Returns the mapped address of the hifi play buffer
 */
CARD32 *dspGetHifiPlayBuf(lofiPhysDevice *pDev, int flag)
{
	int	ofs;

	ofs = dspReadRAM(pDev, flag ? HIFI_OFS1 : HIFI_OFS0);
	return dspGetHifiRecBuf(pDev, flag) + ofs;
}


/*
 *  Sets up the base address and length of the play and record buffers on 
 *  the DSP.  This routine must be called before the DSP is let out of
 *  reset! 'codeclen' and 'hifilen' are the lengths of the codec and 
 *  hifi buffers (both play and record), respectively.
 */
void dspSetBufs(lofiPhysDevice *pDev, int codeclen, int hifilen)
{
	int	addr;

 	addr = BUFFER_START;

/* allocate Lofi bufers */
	dspWriteRAM(pDev, CODEC_START0, addr);
	dspWriteRAM(pDev, CODEC_LEN0, codeclen-1);
	dspWriteRAM(pDev, CODEC_OFS0, codeclen);
	addr += codeclen + codeclen;
	dspWriteRAM(pDev, CODEC_START1, addr);
	dspWriteRAM(pDev, CODEC_LEN1, codeclen-1);
	dspWriteRAM(pDev, CODEC_OFS1, codeclen);
	addr += codeclen + codeclen;

/* Allocate Hifi buffers */
	dspWriteRAM(pDev, HIFI_START0, addr);
	dspWriteRAM(pDev, HIFI_LEN0, hifilen-1);
	dspWriteRAM(pDev, HIFI_OFS0, hifilen);
	addr += hifilen + hifilen;
	dspWriteRAM(pDev, HIFI_START1, addr);
	dspWriteRAM(pDev, HIFI_LEN1, hifilen-1);
	dspWriteRAM(pDev, HIFI_OFS1, hifilen);
}


/* XXX */
ABool
dspCraftEvent(ATime etime, int devNum, struct interrupt_event *ep, aEvent *aep)
{

	/* Fill in system time of event. */
	aep->u.u.type = ADSPEvent; 
	aep->u.DSP.sec = ep->time.tv_sec;
	aep->u.DSP.usec = ep->time.tv_usec;
	aep->u.DSP.time = etime;
	aep->u.DSP.device = 0; /* XXX ?*/
	aep->u.DSP.hd0 = ep->data.dsp_data[0];
	aep->u.DSP.hd1 = ep->data.dsp_data[1];
	aep->u.DSP.hd2 = ep->data.dsp_data[2];
	aep->u.DSP.hd3 = ep->data.dsp_data[3];
	aep->u.DSP.hd4 = ep->data.dsp_data[4];
	aep->u.DSP.hd5 = ep->data.dsp_data[5];
	aep->u.DSP.hd6 = ep->data.dsp_data[6];
	aep->u.DSP.hd7 = ep->data.dsp_data[7];

/*		printf("DSP I %02x %02x %02x %02x", aep->u.DSP.hd0 & 0xff,
			aep->u.DSP.hd0 & 0xff,
			aep->u.DSP.hd1 & 0xff,
			aep->u.DSP.hd2 & 0xff);
		printf(" %02x %02x %02x %02x\n", aep->u.DSP.hd4 & 0xff,
			aep->u.DSP.hd5 & 0xff,
			aep->u.DSP.hd6 & 0xff,
			aep->u.DSP.hd7 & 0xff);
	fflush(stdout);
*/
	return 0;	/* XXX base device */
}
