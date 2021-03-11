/*
 * @DEC_COPYRIGHT@
 */
/*
 * HISTORY
 * $Log: amd79c30_reg.h,v $
 * Revision 1.1.2.2  1993/03/15  17:40:10  Craig_Peterson
 * 	Changes to support additional ioctls.
 * 	[1993/03/12  21:51:22  Craig_Peterson]
 *
 * $EndLog$
 */
/*
 * @(#)$RCSfile: amd79c30_reg.h,v $ $Revision: 1.1.2.2 $ (DEC) $Date: 1993/03/15 17:40:10 $
 */

/*
 * OLD HISTORY
 * Revision 1.1.2.4  92/08/28  11:01:42  Ron_Bhanukitsiri
 * 	Modified driver for Alpha BL8
 * 	[92/08/28  11:00:57  Ron_Bhanukitsiri]
 * 
 * Revision 1.1.2.3  92/08/19  16:09:53  Narayan_Mohanram
 * 	Updated for alpha changes
 * 	[92/08/19  15:57:57  Narayan_Mohanram]
 * 
 * Revision 1.1.2.2  92/04/19  17:09:28  Ron_Bhanukitsiri
 * 	"BL2 - Bellcore Certification"
 * 	[92/04/19  17:08:26  Ron_Bhanukitsiri]
 * 
 */

#ifndef BBA_REG_H
#define BBA_REG_H

#if defined(mips)
/* MAXine I/O ASIC hardware defines */
#define MACH_SSR_ADDR		KN02CA_SSR_ADDR /* SSR address		*/
#define MACH_SIR_ADDR		KN02CA_SIR_ADDR /* SSR address		*/
#define CODEC_REG_ADDR 		0x1c240000 /* CODEC registers */
#define DMA_CUR_XMT 		0x1c040080 /* Current transmit pointer */
#define DMA_NEXT_XMT 		0x1c040090 /* Next transmit pointer */
#define DMA_CUR_RCV 		0x1c0400a0 /* Current receive pointer */
#define DMA_NEXT_RCV 		0x1c0400b0 /* Next receive pointer */
#define DMA_XMT_DATA		0x1c040140 /* Current data being shifted out */
#define DMA_RCV_DATA 		0x1c040150 /* Current data being shifted in */

#elif defined(__alpha)
#define CODEC_REG_ADDR 		0x01f0240000l /* CODEC registers(sparse)*/
/*
 * The following are defined in the dense address space
 */
#define MACH_SSR_ADDR		0x1e0040100l /* SSR address		*/
#define MACH_SIR_ADDR		0x1e0040110l /* SIR registers		*/
#define CODEC_REG_ADDR_DENSE	PHYS_TO_KSEG (0x1e0240000l) /* CODEC registers(sparse)	*/
#define DMA_CUR_XMT 		PHYS_TO_KSEG (bba_tc_reg_p + (0x80<<1)) /* Current transmit pointer*/
#define DMA_NEXT_XMT 		PHYS_TO_KSEG (bba_tc_reg_p + (0x90<<1)) /* Next transmit pointer	*/
#define DMA_CUR_RCV 		PHYS_TO_KSEG (bba_tc_reg_p + (0xa0<<1)) /* Current receive pointer */
#define DMA_NEXT_RCV 		PHYS_TO_KSEG (bba_tc_reg_p + (0xb0<<1)) /* Next receive pointer	*/

#define ISDN_INT		SIR_ISDN
#define ISDN_DMA_ERROR		SIR_ISDN_MINT
#define ISDN_DMA_RXINT		SIR_ISDN_RINT
#define ISDN_DMA_TXINT		SIR_ISDN_XINT
#endif	/* mips or alpha */

/* System Support Register defines */
#define SSR_XMT_EN 		(1 << 20)	/* Transmit enable	*/
#define SSR_RCV_EN 		(1 << 19)	/* Receive enable	*/
#define SSR_CODEC_RESET 	(1 << 12)	/* Not reset		*/

#define QIOBBAINFO	_IOR('a', 1, struct bba_info *)

/* chip definitions */

/*
 * Codec register offsets
 */
#define	CR_OFFS		0		/* Write only */
#define IR_OFFS		CR_OFFS		/* Read only */
#define	DR_OFFS		1		/* 2 register one RO one WO */
#define	DSR1_OFFS	2		/* readonly */
#define	DER_OFFS	3		/* readonly 2 byte fifo */
#define	DCTB_OFFS	4		/* writeonly 8 byte fifo */
#define DCRB_OFFS	DCTB_OFFS	/* Readonly 8 byte fifo */
#define	BBTB_OFFS	5		/* writeonly */
#define	BBRB_OFFS	BBTB_OFFS	/* readonly */
#define	BCTB_OFFS	6		/* writeonly */
#define	BCRB_OFFS	BCTB_OFFS	/* readonly */
#define	DSR2_OFFS	7		/* readonly */

#define CODECNREGS 8

#include <io/dec/tc/amd79c30_codec.h>

/* Interrupt Register */
#define IR_DTTHRSH (unsigned char) 1     /*  D-Ch Transmit Buffer Empty    */
#define IR_DRTHRSH (unsigned char) 2     /*  D-Ch Receive Buffer Available */
#define IR_DSRI    (unsigned char) 4     /*  D-Ch Packet Status Interrupt  */
#define IR_DERI    (unsigned char) 8     /*  D-Ch Error Interrupt          */
#define IR_BBUFF   (unsigned char) 0x10  /*  Bb or Bc Byte Avail/Empty     */
#define IR_LSRI    (unsigned char) 0x20  /*  LIU Status Interrupt          */
#define IR_DSR2I   (unsigned char) 0x40  /*  D-Ch Buffer Status Interrupt  */
#define IR_PPMF    (unsigned char) 0x80  /*  Multiframe or PP interrupt */

/* DCF bits */
#define	DCF_INIT	(0x1)
#define	DCF_MUX		(0x2)
#define	DCF_MAP		(0x3)
#endif 	/* BBA_REG_H */
