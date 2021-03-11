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
/* definitions of register for AMD79C30AGETREG/AMD79C30ASETREG ioctl's */

/* MUX register */
#define AMD79C30A_MUX_CR

/* MAP registers */
#define AMD79C30A_MAP_X	(1)
#define AMD79C30A_MAP_R	(2)
#define AMD79C30A_MAP_GX	(3)
#define AMD79C30A_MAP_GR	(4)
#define AMD79C30A_MAP_GER	(5)
#define AMD79C30A_MAP_SGCR	(6)
#define AMD79C30A_MAP_FTGR	(7)
#define AMD79C30A_MAP_ATGR	(8)
#define AMD79C30A_MAP_MMR1	(9)
#define AMD79C30A_MAP_MMR2	(10)
#define AMD79C30A_MAP_ALL	(11)
#define AMD79C30A_MUX_MCR1	(12)
#define AMD79C30A_MUX_MCR2	(13)
#define AMD79C30A_MUX_MCR3	(14)
#define AMD79C30A_MUX_MCR4	(15)
#define AMD79C30A_MUX_ALL	(16)
#define AMD79C30A_INIT_INIT	(17)

#define OCF_INIT_INIT	(1)

#define	OCF_MAP_X	(1)
#define	OCF_MAP_R	(2)
#define	OCF_MAP_GX	(3)
#define	OCF_MAP_GR	(4)
#define	OCF_MAP_GER	(5)
#define	OCF_MAP_SGCR	(6)
#define	OCF_MAP_FTGR	(7)
#define	OCF_MAP_ATGR	(8)
#define	OCF_MAP_MMR1	(9)
#define	OCF_MAP_MMR2	(10)
#define	OCF_MAP_PERFORM	(11)

#define OCF_MUX_MCR1	(1)
#define OCF_MUX_MCR2	(2)
#define OCF_MUX_MCR3	(3)
#define OCF_MUX_MCR4	(4)
#define OCF_MUX_PERFORM	(5)
/* chip definitions */
/* INIT register */

/* IR (Interrupt Reister) RO */

/* DCF bits */
#define	DCF_INIT	(0x1)
#define	DCF_MUX		(0x2)
#define	DCF_MAP		(0x3)


/* constants for individual registers */

	/* INIT register */
/* Power Mode Selection */
#define INIT_PM_MASK	(0x3)
#define INIT_PM_IDLE	(0x0)
#define INIT_PM_ACTIVE_VOICE_DATA	(0x1)
#define	INIT_PM_ACTIVE_DATA	(0x2)

/* Interrupt Selection */
#define INIT_IR_MASK	(0x4)
#define	INIT_IR_ENABLE	(0x0)
#define INIT_IR_DISABLE	(0x4)

/* Clock Divider Selection */
#define INIT_CDS_MASK	(0x38)
#define INIT_CDS_BY2	(0x00)
#define INIT_CDS_BY1	(0x08)
#define	INIT_CDS_BY4	(0x10)

#define INIT_CDS_BY3	(0x20)

/* Abort Selection */
#define INIT_AS_MASK	(0xc0)
#define INIT_AS_RA	(0x40)
#define INIT_AS_NRA	(0x00)
#define INIT_AS_TA	(0x80)
#define INIT_AS_NTA	(0x00)

/* Interrupt Register */

/* Microprocessor Interface */


/* MUX constants */
#define MUX_PORT_NONE	(0x0)
#define MUX_PORT_B1	(0x01)
#define MUX_PORT_B2	(0x02)
#define MUX_PORT_BA	(0x03)
#define MUX_PORT_BB	(0x04)
#define MUX_PORT_BC	(0x05)
#define MUX_PORT_BD	(0x06)
#define MUX_PORT_BE	(0x07)
#define MUX_PORT_BF	(0x08)
#define MUX_MCR4_ENABLE	(0x08)
#define MUX_MCR4_BB_REVERSE	(0x10)
#define MUX_MCR4_BC_REVERSE	(0x20)


/* MAP MMR constants */
#define MAP_MMR1_BITS_A-law	(0x1)
#define MAP_MMR1_BITS_GX	(0x2)
#define MAP_MMR1_BITS_GR	(0x4)
#define MAP_MMR1_BITS_GER	(0x8)
#define MAP_MMR1_BITS_X		(0x10)
#define MAP_MMR1_BITS_R		(0x20)
#define MAP_MMR1_BITS_STG	(0x40)
#define MAP_MMR1_BITS_DLOOP	(0x80)


#define MAP_MMR2_BITS_AINB	(0x1)
#define MAP_MMR2_BITS_LS	(0x2)
#define	MAP_MMR2_BITS_DTMF	(0x4)
#define MAP_MMR2_BITS_TGEN	(0x8)
#define MAP_MMR2_BITS_TRING	(0x10)
#define MAP_MMR2_BITS_HIGHPASS	(0x20)
#define MAP_MMR2_BITS_ADC	(0x40)
