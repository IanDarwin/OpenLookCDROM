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
#include "jv_io.h"
#include "physdevice.h"
#include "dsp.h"
#include "dsp_func.h"

/*
 * Reads a 24-bit value from the DSP's shared RAM
 */
int
dspReadRAM(jvPhysDevice *pDev, int addr)
{
	return ((int)(pDev->mPtr[addr] >> 8));
}

/*
 * Writes a 24-bit value to the DSP's shared RAM
 */
void dspWriteRAM(jvPhysDevice *pDev, int addr, int val)
{
	pDev->mPtr[addr] = val << 8;	MB();
}

/*
 * Convert a shared RAM address into a mapped address
 */

#define RAMPtr(dev,addr) ((dev)->mPtr + (addr))


/*
 * Returns a pointer to the HIFI timer on the DSP.
 */
CARD32 *
dspGetHifiTimePtr(jvPhysDevice *pDev)
{
	return (CARD32 *)RAMPtr(pDev,HIFI_TIME);
}


/*
 * Returns the mapped address of a hifi record buffer
 */
CARD32 *dspGetHifiRecBuf(jvPhysDevice *pDev, int flag)
{
	int	addr;

	addr = flag ? HIFI_START1 : HIFI_START0;
	return (CARD32 *)RAMPtr(pDev, dspReadRAM(pDev, addr));
}


/*
 * Returns the mapped address of the hifi play buffer
 */
CARD32 *dspGetHifiPlayBuf(jvPhysDevice *pDev, int flag)
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
void dspSetBufs(jvPhysDevice *pDev, int hifilen)
{
	int	addr;

 	addr = BUFFER_START;

/* Allocate Hifi buffers */
	dspWriteRAM(pDev, HIFI_START0, addr);
	dspWriteRAM(pDev, HIFI_LEN0, hifilen-1);
	dspWriteRAM(pDev, HIFI_OFS0, hifilen);
	addr += hifilen + hifilen;
	dspWriteRAM(pDev, HIFI_START1, addr);
	dspWriteRAM(pDev, HIFI_LEN1, hifilen-1);
	dspWriteRAM(pDev, HIFI_OFS1, hifilen);
}
