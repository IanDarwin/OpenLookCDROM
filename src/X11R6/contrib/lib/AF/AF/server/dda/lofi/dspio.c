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
#include	<stdio.h>
#include	<io/tc/lofi_reg.h>
#include	<io/tc/lofi.h>
#include	"lofi_io.h"
#include	"dspio.h"

void enableHostRxInt(struct lofi_info *lofi)
{
	int r0;

	r0 = lofiReadHostPort(lofi, HOST_ICR);
	lofiWriteHostPort(lofi, HOST_ICR, r0 | HOST_ICR_RREQ);
}

void hostCommand(struct lofi_info *lofi, int vector)
{
	while((lofiReadHostPort(lofi, HOST_CVR) & HOST_CVR_HC))
		;
	lofiWriteHostPort(lofi, HOST_CVR, (vector>>1) | HOST_CVR_HC);
}

void writeHostData(struct lofi_info *lofi, int d)
{
	while(! (lofiReadHostPort(lofi, HOST_ISR) & HOST_ISR_TXDE) )
		;
	
	lofiWriteHostPort(lofi, HOST_TXH, d>>16);	MB();
	lofiWriteHostPort(lofi, HOST_TXM, d>>8);	MB();
	lofiWriteHostPort(lofi, HOST_TXL, d>>0);	MB();
}

int readHostData(struct lofi_info *lofi)
{
	int d0,d1,d2;

	while(! (lofiReadHostPort(lofi, HOST_ISR) & HOST_ISR_RXDF) )
		;
	
	d2 = lofiReadHostPort(lofi, HOST_RXH);
	d1 = lofiReadHostPort(lofi, HOST_RXM);
	d0 = lofiReadHostPort(lofi, HOST_RXL);

	return (d0 | (d1<<8) | (d2<<16));
}
