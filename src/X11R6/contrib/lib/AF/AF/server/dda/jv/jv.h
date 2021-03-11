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

/*----------------------------------------------------------------------------
 * AUTHOR:        Victor Bahl
 * CREATION DATE: 01-Nov-1991
 *
 * Modification History: jv.h 
 *
 *	20-Feb-1992  VB         added shmid to struct JvMemAddr
 *--------------------------------------------------------------------------*/

/* @(#) jv.h     v1.8  */

#ifndef  _JV_H_
#define  _JV_H_

#include <sys/types.h>

#define JV_REG_OFFSET	0	/* offset for mapping the device regs */
#define TOT_BUF_SIZE	(1<<20) /* 1 MegaByte maximum for locked mem  */
#define EV_SIZE		128	/* Size of the event queue */

/*
** The type of interrupt and the time at which it was issued is detected and
** logged in a circular queue. 
*/
typedef enum
        {
	   ERROR       = 0,
	   JPEG_CMP    = 1,
	   JPEG_DEC    = 2,
	   DMA         = 4,
	   VIN_VS      = 5,
	   VOUT_VS     = 6,
	   DSP         = 7,
	   TIMER_ROLL  = 8,
	   TIMER_MATCH = 9 
        } JvIntr;

typedef struct jv_event
        {
	   JvIntr intr;		/* J-Video interrupt type */
	   u_short   timestamp;	        /* 90 KHz time stamp	 */
        } JvEvent;

typedef struct JvEqueue 
        { 
	  int get;               /* location of where to 'get' new data from */
	  int put;               /* location of where to 'put' new data      */
	  JvEvent queue[EV_SIZE];/* data, see above */
        } JvEqueue;
 
/*
** The virtual and the corresponding physical addresses of the locked down
** areas in memory are passed back to user space
*/ 
struct JvMemAddr
       {
          caddr_t Vaddr;                /* Virtual address  */
          paddr_t Paddr;                /* Physical Address */
	  int 	  shmid;		/* Shared memory id */
       };

typedef struct JvBuf 
       { 
	  u_long Bytes;                 /* Num of bytes to lock */ 
	  struct JvMemAddr *addr;	/* addresses */
       } JvBuf;

struct JvPages
       {
          u_long NumPages;               /* Num of pages to lock */
          struct JvMemAddr *Pages;       /* addresses */
       };

/*
** The J-Video i/o controls
*/

   /* Map J-Video device registers to user space */
#define JVGETMAP    _IOR('j',  0, char *)

   /* Map the event queue to user space */
/*#define JVGETQUE    _IOWR('j', 2, struct JvEqueue) --wrongo dude*/
#define JVGETQUE    _IOR('j', 2, struct JvEqueue *)

   /* Allocate wired down memory and map it to user space */
#define	JVGETBUF    _IOWR('j', 1, struct JvBuf)

   /* Deallocate wired down memory previously allocacted via JVGETBUF */
/*#define JVDELBUF    _IOR( 'j', 3, struct JvBuf) --wrongo dude*/
#define JVDELBUF    _IOW( 'j', 3, struct JvBuf) 

   /* Lock down user allocated memory */
#define JVLOCK      _IOWR( 'j', 4, struct JvPages)

   /* Unlock memory locked by JVLOCK  */
/*#define JVUNLOCK    _IOR(  'j', 5, struct JvPages) -- wrongo dude*/

#define JVUNLOCK    _IOW(  'j', 5, struct JvPages) 

   /* Enable/disable interrupts */
#define JVSETINTR   _IOW( 'j', 6, int)

   /* Query interrupts */
#define JVGETINTR   _IOR( 'j', 7, int)

   /* Panic the kernel.  A minor security hole */
#define JVPANIC   _IOW( 'j', 8, int)

#endif
