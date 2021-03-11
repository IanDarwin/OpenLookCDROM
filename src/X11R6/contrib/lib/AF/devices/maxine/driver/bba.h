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
#ifndef BBA_H
#define BBA_H
/************************************************************************
 * Modification History
 *
 *
 *	7/17/90 
 *	   prototype bba driver hacked by Rich Hyde
 *	
 *
 ************************************************************************/
#include <sys/time.h>

#define CODEC_READ 0
#define CODEC_WRITE 1

#define BYTESHIFT 0
#define BYTEMASK (0xff << BYTESHIFT)

#define TRUE 1
#define FALSE 0

typedef  enum {
	  C_IR = 1, 
	  C_ERR = 2, 
	  UNKNOWN = 3
    } EventType;

/*
 *							
 *  These need to be mapped into user space.  
 *							
 */

struct	interrupt_event{
	EventType type;		/*  event type */
	struct timeval time;	/* Systems notion of time when it occured */
	long	clkhand;	/* clock hand when it occurred */
	long seq;		/* sequence number of the event */
#define c_ir codec_data.codec_ir
#define c_dsr1 codec_data.codec_dsr1
#define c_derr codec_data.codec_derr
#define c_dsr2 codec_data.codec_dsr2
#define c_lsr codec_data.codec_lsr
#define c_mfsb codec_data.codec_mfsb
#define c_ppsr codec_data.codec_ppsr
	struct codec_intr {
		char codec_ir;
		char codec_derr;
		char codec_dsr1;
		char codec_dsr2;
		char codec_lsr;
		char codec_mfsb;
		char codec_ppsr;
	}codec_data;
};

#define DMASIZE 256	/* number of bytes transfered between DMA interrupts
					 * for each bchan (must be less that NBPG/sizeof(long)
					 */
#define BCHANBUFFSIZE (32 * 1024)	/* size of b channel buffer ~4sec */
/* The following constants are hardware dependent and should not be changed */
#define NDMABUFF 2		/* Number of dma buffers to toggle between */
#define NBCHAN 3	/* Number of B channels to dma to */
#define BD 0	/* identify B channel buffers */
#define BE 1
#define BF 2

#define MAXBBA (1) /* Max number of audio devices on a MAXine */

#define EVENTQUEUESIZE 200    /* no more than 200 events outstanding */
/* bits 6,7 and 8 of the address bus go to the chip.  So register 1
 * is at address BASE + 1 << 6 etc.  We have to fake
 * things out so that we get the correct byte.
 */
struct codec_reg {
	unsigned char reg_data;
	unsigned char reg_fill[(1<<6) - 1];
};

/* flag defines */
#define SIGNAL_OK 0x80000000
#define CODEC_OPEN 0x1

struct bba_info {
   int flag;			/* Is open boolean */
   void *rsel;			/* select address */
   int indir; /* signals indirect access by interrupt handler */
   int codec_time;			/* number of times the B chan has interrupted */
   /* Interrupt Event Handling */
   struct interrupt_event    queue[EVENTQUEUESIZE];
   int event_size;		/* size of the events themselves */
   int event_list_size;	/* Size of the event queue */
   struct interrupt_event *ks_start; /* Head of the queue in kernel space */
   struct interrupt_event *us_start; /* Head of the queue in user space */
   volatile int head;		/* head of circular list	*/
   volatile int tail;		/* tail of circular list	*/
   long last_seq;		/* last sequence number used	*/
   struct proc *owner; /* Where to send signals */
   /* addresses of chip registers */
   volatile struct codec_reg *ks_reg;  /* address of the codec regs in ks */
   volatile struct codec_reg *us_reg;  /* address of the codec regs in us */
   /* DMA buffer handling */
   unsigned char rcv_buff[NBCHAN][BCHANBUFFSIZE];
   int rcv_current;	/* Current offset into rcv_buff */
   unsigned char *rcv_dma_buff[NDMABUFF];
   unsigned long rcv_dma_addr[NDMABUFF];
   int rcv_toggle;
   unsigned char xmt_buff[NBCHAN][BCHANBUFFSIZE];
   int xmt_current;	/* Current offset into xmt_buff */
   unsigned char *xmt_dma_buff[NDMABUFF];
   unsigned long xmt_dma_addr[NDMABUFF];
   int xmt_toggle;
};
#define BBA_DEBUG( who, level) ((bba_debug & who) && ((0xff&bba_debug) > level))
#define BBA_CONF	(0x1000)
#define BBA_BUF		(0x2000)
#define BBA_OTHER	(0x4000)

#define DEV_BBA_STR "BBA  "
#define BBA_TIMEOUT     (hz/16)

#endif	/* BBA_H */

