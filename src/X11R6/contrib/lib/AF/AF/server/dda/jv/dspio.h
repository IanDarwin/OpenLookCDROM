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

/* 
 * Definitions of host port registers.
 */
#define	HOST_ICR	0
#define	HOST_CVR	1
#define	HOST_ISR	2
#define	HOST_IVR	3
#define	HOST_RXH	5
#define	HOST_RXM	6
#define	HOST_RXL	7
#define	HOST_TXH	5
#define	HOST_TXM	6
#define	HOST_TXL	7

/* 
 * Definitions of host port register fields.
 */
#define	HOST_ICR_RREQ	0x01
#define	HOST_ICR_TREQ	0x02
#define	HOST_ICR_HF0	0x08
#define	HOST_ICR_HF1	0x10
#define	HOST_ICR_HM0	0x20
#define	HOST_ICR_HM1	0x40
#define	HOST_ICR_INIT	0x80

#define	HOST_ICR_HFLAGS	0x18

#define	HOST_CVR_HVEC	0x1f
#define	HOST_CVR_HC	0x80

#define	HOST_ISR_RXDF	0x01
#define	HOST_ISR_TXDE	0x02
#define	HOST_ISR_TRDY	0x04
#define	HOST_ISR_HF2	0x08
#define	HOST_ISR_HF3	0x10
#define	HOST_ISR_DMA	0x40
#define	HOST_ISR_HREQ	0x80

