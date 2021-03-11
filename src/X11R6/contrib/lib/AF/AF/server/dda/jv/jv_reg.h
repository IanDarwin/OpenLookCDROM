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

#ifndef _JV_REG_
#define _JV_REG_

/* 
** J-Video register offsets, organized by ascending address
*/
#define	EEPROM			0x000000 
#define	ICR_WRITE		0x040000 
#define	CVR_WRITE		0x040200 
#define	ISR_WRITE		0x040400 
#define	RXTXH_WRITE		0x040A00 
#define	RXTXM_WRITE		0x040C00 
#define	RXTXL_WRITE		0x040E00 
#define	ICR_READ		0x041000 
#define	CVR_READ		0x041200 
#define	ISR_READ		0x041400 
#define	RXTXH_READ		0x041A00 
#define	RXTXM_READ		0x041C00 
#define	RXTXL_READ		0x041E00 
#define	DSP_RAM			0x080000 
#define	AUDIO_CSR		0x0FA000 
#define	JPEG_COMP		0x100000 
#define	JPEG_DCMP		0x140000 
#define	DMA_CSR			0x200000 
#define	FIFO_STATUS  		0x200005 
#define	DMA_ABURSTS  		0x200003 
#define	DMA_CMD			0x200004 
#define	DMA_SCATTER		0x200008 
#define	FIFO_JPEG_FLAGS 	0x280004 
#define	FIFO_JPEG		0x28000C 
#define	FIFO_DITHER		0x280020 
#define	INT_STATUS		0x300000 
#define	INT_MASK		0x300004 
#define	FIFO_CSR		0x300008 
#define AUDIO_INT_MASK		0x30000C
#define DITHER_CSR              0x300080
#define	DITHER_R1		0x300084 
#define	DITHER_R2		0x300088 
#define	DITHER_R3		0x30008C 
#define	DITHER_R4		0x300090 
#define	BREZ_CSR		0x300094 
#define	DMA_MODE		0x3000B0
#define	BREZ_XDELTA0		0x300098 
#define	BREZ_XDELTA1 		0x30009C 
#define	BREZ_YDELTA0		0x3000A0 
#define	BREZ_YDELTA1		0x3000A4 
#define	VIN_START		0x3000A8 
#define	VIN_END			0x3000AC 
#define VIN_CAPTURE             0x3000B4
#define	FIFO_DITHER_FLAGS	0x3000C0 
#define	VOUT_CSR1		0x300100 
#define	VOUT_CSR2		0x300104 
#define	IIC_DATA		0x300180 
#define	IIC_STATUS		0x300184 
#define VIN_MISC                0x30019C
#define	DECIM_MODE		0x300190 
#define	DECIM_FACTOR		0x300198 
#define	DECIM_SKIP		0x300194 
#define	TIMER_CSR		0x3001E0 
#define	TIMER_VAL_LSB 		0x3001F0 
#define	TIMER_VAL_MSB 		0x3001F4 
#define	TIMER_LATCH_LSB 	0x3001F8 
#define	TIMER_LATCH_MSB 	0x3001FC 
#define	TIMER_COMPARATOR 	0x3001FC 

#define int32 int
#define int16 short
#define int8  char
#define uint32 unsigned int
#define uint16 unsigned short
#define uint8 unsigned char
/* ################################################################ */
/* ####		        J-Video Interrupts                     #### */
/* ################################################################ */
/*
** specifics of interrupt register (assumes low to high bit ordering) 
*/
typedef	union {
		volatile uint32 *reg;
	        struct {
		    volatile uint32 jpeg_cmp     : 1;
		    volatile uint32 jpeg_dec     : 1;
		    volatile uint32 timer        : 1;
	   	    volatile uint32 dma          : 1;
	   	    volatile uint32 vin_vs       : 1;
	   	    volatile uint32 vout_vs      : 1;
	   	    volatile uint32 unused       : 2;
		    volatile uint32 pad		 : 24; /*so its 32 bits*/
	        } *flags;
	   } JvIntrReg; 

typedef union {
		 volatile uint32 *reg;
		 struct {
		    volatile uint32 roll_mask    : 1;
		    volatile uint32 match_mask   : 1;
		    volatile uint32 unused       : 3;
		    volatile uint32 iic          : 1;
		    volatile uint32 roll_stat    : 1;
		    volatile uint32 match_stat   : 1;
		    volatile uint32 pad		 : 24; /*so its 32 bits*/
		 } *flags;
	       } JvTimerCsr;
		    


/* ################################################################ */
/* ####    Data structures with J-Video register pointers,     #### */
/* ####               arranged by functionality                #### */
/* ################################################################ */
/*
** Audio Subsystem
*/
#define		DSP_RAM_SIZE		0x8000
typedef volatile unsigned char JvEeprom[0x8000];
typedef volatile uint32  JvDspRam[DSP_RAM_SIZE];
typedef struct JvAudioRegs
	{
	   JvEeprom 	*eeprom;
	   volatile uint32  *icr_write;
	   volatile uint32  *cvr_write;
	   volatile uint32  *isr_write;
	   volatile uint32  *rxtxh_write;
	   volatile uint32  *rxtxm_write;
	   volatile uint32  *rxtxl_write;
	   volatile uint32  *icr_read;
	   volatile uint32  *cvr_read;
	   volatile uint32  *isr_read;
	   volatile uint32  *rxtxh_read;
	   volatile uint32  *rxtxm_read;
	   volatile uint32  *rxtxl_read;
	   JvDspRam	*ram;
	   volatile uint32   *csr;
	} JvAudioRegs;


/*
** Timer
*/
typedef struct JvTimerRegs
	{
	   JvTimerCsr             csr;
	   volatile uint32 *comparator;
	   volatile uint32 *val_lsb;
	   volatile uint32 *val_msb;
	   volatile uint32 *latch_lsb;
	   volatile uint32 *latch_msb;
	} JvTimerRegs;


/*
** Interrupts
*/
typedef struct JvIntRegs
	{
	   JvIntrReg status;
	   JvIntrReg mask;
	   JvIntrReg audiomask;
	} JvIntRegs;


/*
** device register structure
**
*/
typedef struct JvRegs
	{
	   JvAudioRegs	audio;
	   JvTimerRegs	timer;
	   JvIntRegs	intr;
	} JvRegs;



/* ################################################################ */
/* ####		J-Video bit position flags		       #### */	
/* ################################################################ */
/*
** Audio CSR
**
**  Note that the audio CSR has an address and data field (for writes)
**  The address is in bit <31:28>, and the data is in <27:24>
*/
		/* Write addresses */
#define		DSP_CSR_EA_ADDR		((unsigned int) 0x20000000)
#define		DSP_CSR_ED_ADDR		((unsigned int) 0x30000000)
#define		DSP_CSR_DM_ADDR		((unsigned int) 0x40000000)
#define		DSP_CSR_RATE_ADDR	((unsigned int) 0x70000000)

		/* Write values (contain address and data) */
#define		DSP_ENABLE_AUDIO	((unsigned int) 0x21000000)
#define         DSP_DISABLE_AUDIO       ((unsigned int) 0x20000000)
#define         DSP_DISABLE_CHIP        ((unsigned int) 0x30000000)
#define		DSP_ENABLE_CHIP		((unsigned int) 0x31000000)
#define		DSP_SINGLE_CHIP_MODE	((unsigned int) 0x40000000)
#define		DSP_NORMAL_MODE		((unsigned int) 0x41000000)
#define		DSP_8KHZ_RATE		((unsigned int) 0x70000000)
#define		DSP_16KHZ_RATE		((unsigned int) 0x71000000)
#define		DSP_32KHZ_RATE		((unsigned int) 0x72000000)
#define		DSP_48KHZ_RATE		((unsigned int) 0x73000000)
#define		DSP_22KHZ_RATE		((unsigned int) 0x76000000)
#define		DSP_44KHZ_RATE		((unsigned int) 0x77000000)

		/* Read flags */
#define		DSP_CSR_S1		((unsigned int) 0x800)
#define		DSP_CSR_S2		((unsigned int) 0x1000)
#define		DSP_CSR_S3		((unsigned int) 0x2000)
#define		DSP_CSR_ED		((unsigned int) 0x4000)
#define		DSP_CSR_EA		((unsigned int) 0x10000)
#define		DSP_CSR_DM		((unsigned int) 0x20000)
#define		DSP_CSR_HOST_STAT	((unsigned int) 0x80000)

/*
** Timer
*/
#define 	TIMER_ROLL_MSK		(unsigned char) 0x1
#define 	TIMER_COMP_MSK  	(unsigned char) 0x2
#define 	TIMER_TIME_ROLL 	(unsigned char) 0x40
#define 	TIMER_COMP_EQL		(unsigned char) 0x80
#define         TIMER_ROLL_INT_CLR      (unsigned char) 0x40
#define         TIMER_COMP_INT_CLR      (unsigned char) 0x80 

/*
** Interrupts
*/
		/* Clear/status/mask */
#define		INTR_COMP		((unsigned char) 0x1)
#define		INTR_DCMP		((unsigned char) 0x2)
#define		INTR_TIMER		((unsigned char) 0x4)
#define		INTR_DMA		((unsigned char) 0x8)
#define		INTR_VIN_VS		((unsigned char) 0x10)
#define		INTR_VOUT_VS		((unsigned char) 0x20)

#endif
