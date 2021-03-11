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
#ifndef LOFI_H
#define LOFI_H
/************************************************************************
 * lofi.h
 *
 *
 * Modification History
 *	Last modified by Mon Sep 28 09:10:05 1992 by tml
 *		begin port to Alpha.
 *	7/17/90 
 *	   prototype lofi driver hacked by Rich Hyde
 *
 ************************************************************************/
#include <sys/time.h>

/*
 *							
 *  These need to be mapped into user space.  
 *							
 */

typedef  enum {
	  DSP = 1, 
	  FRAME_CLK = 2,
	  TLI_RING = 3, 
	  TLI_DTMF = 4, 
	  TLI_LOOP = 5,
	  OPTION_INTR = 6,
	  UNKNOWN = 7
    } EventType;

struct	interrupt_event{
	EventType type;		/*  event type */
	int status;		/* value of the status register */
	struct timeval time;	/* Systems notion of time when it occured */
	unsigned int dsptime;	/* DSP's notion of time when it occurred */
	int seq;		/* sequence number of the event */
	union {			/* Additional data by type. */
		unsigned char dsp_data[8];
		struct codec_intr {
			char master_ir;
			char master_rx;
			char slave_ir;
			char slave_rx;
		} codec_data;
	} data;
};

struct lofi_info {
   int flag;			/* Is open boolean */
   int event_size;		/* size of the events themselves */
   int event_list_size;	/* Size of the event queue */
   struct interrupt_event *ks_start; /* Head of the queue in kernel space */
   struct interrupt_event *us_start; /* Head of the queue in user space */
   volatile int head;		/* head of circular list	*/
   volatile int tail;		/* tail of circular list	*/
   int last_seq;		/* last sequence number used	*/
   struct lofi_reg *ks_reg;	/* addresses of the lofi option space in ks */
   struct lofi_reg *us_reg;	/*  addresses of the lofi option space in us */
#ifndef __alpha
   void *rsel;			/* select address */
#else
#if	0
   sel_queue_t rsel;            /* select queue    */
#else
   void	*rsel;
#endif
#endif
   int old_rd_csr;		/* old csr */
};
#define LOFI_DEBUG( who, level) ((lofi_debug & who) && ((0xff&lofi_debug) > level))
#define LOFI_INTR	(0x100)
#define LOFI_CONF	(0x1000)
#define LOFI_BUF	(0x2000)
#define LOFI_OTHER	(0x4000)

#define DEV_LOFI_STR "LoFi  "

#endif	/* LOFI_H */
