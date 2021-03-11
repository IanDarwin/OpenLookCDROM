#ifndef LOFI_REG_H
#define LOFI_REG_H
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

/************************************************************************
 * lofi_reg.h
 * 	Contains definitions of the LoFi/DECaudio module's I/O space
 *	and useful constants shared between driver and application.
 *
 *
 * Modification History
 * Last modified by tml on Mon Sep 28 09:07:18 1992
 *	Begin alpha port.  Change type long to type int since both
 *	32 bits on MIPS and ALPHA.  I personally prefer CARD32 style 
 *	typedefs, but wish to rely on something more systematic...
 *
 ************************************************************************/

#define	NBREGION	(256*1024)

#define	NWROM		(32*1024)
#define	NRCOPIES 	(NBREGION/NWROM/4)

typedef struct ROM {
    struct {
	volatile unsigned char romd;
	volatile unsigned char u0;
	volatile unsigned char u1;
	volatile unsigned char u2;
    } rcopy[NRCOPIES][NWROM];
} ROM;

#define	RD_HDBASE	0x00400
#define	WR_HDBASE	0x00000

typedef struct HOST {
	volatile unsigned int hd[NBREGION/4];
} HOST;

#define	NWSRAM		(32*1024)
#define	NSRCOPIES 	(NBREGION/NWSRAM/4)

typedef struct SRAM {
    union {
	volatile unsigned int	wd[NWSRAM];
	struct {
	    volatile unsigned char   u0;
	    volatile unsigned char   bd0;
	    volatile unsigned char   bd1;
	    volatile unsigned char   bd2;
	} bd[NWSRAM];	
    } srcopy[NSRCOPIES];
} SRAM;

#define	NWIO	    	(256*1024)
#define	NIO	 	(NBREGION/NWIO/4)
#define	NUSRAM		((32+26)*1024)
#define	NCSR		(2*1024)
#define	NOPTION		(2*1024)

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

typedef struct IO {
    volatile unsigned int    usram[NUSRAM];
    volatile unsigned int    csr[NCSR];
    volatile unsigned int    option[NOPTION];
    volatile unsigned int    codecs[1];
} IO;

/*
 * This structure overlays the lowest 1 MB of Lo-Fi option memory.
 */
struct lofi_reg {
    ROM		io_rom;
    HOST	io_host;
    SRAM	io_sram;
    IO		io_io;
} ;

#define	rom_map(x)	io_rom.rcopy[0][(x)].romd

#define	rd_host(rx)	(io_host.hd[RD_HDBASE + ((rx)<<7)]&0xFF)
#define	wr_host(rx)	(io_host.hd[WR_HDBASE + ((rx)<<7)]&0xFF)
#define	rd_hostmap(rx)	io_host.hd[RD_HDBASE + ((rx)<<7)]
#define	wr_hostmap(rx)	io_host.hd[WR_HDBASE + ((rx)<<7)]

#define	sram_map(x)	io_sram.srcopy[0].wd[(x)]

#define	rd_csr		io_io.csr[0]
#define	wr_csr		io_io.csr[0]

#define	C0BASE		0x0000
#define	C1BASE		0x0008

#define	codec0(rx)	io_io.codecs[C0BASE + ((rx) << 0)]
#define	codec1(rx)	io_io.codecs[C1BASE + ((rx) << 0)]

#define	woption(x)	io_io.option[(x)]

/* 
 * Rich's bit defs.
 */
#define FRAME_INTR_ENABLE_DEF	(1<<29)
#define TLI_INTR_ENABLE_DEF	(1<<28)

#define CODEC1_INTR_NOT_DEF	(1<<31)
#define CODEC0_INTR_NOT_DEF	(1<<30)

#define RING_STATUS_DEF		(1<<27)
#define LOOP_CURRENT_DEF	(1<<26)
#define DTMF_STATUS_DEF		(1<<25)
#define DTMF_VALID_DEF		(1<<20)

#define DSP_HOST_INTR_NOT_DEF	(1<<19)
#define OPTION_STATUS_DEF	(1<<18)
#if	0
#define BASIC_INTR_MASK	(DTMF_STATUS_DEF)
#else
#define BASIC_INTR_MASK	(0)
#endif
#define NOT_INTR_MASK (DSP_HOST_INTR_NOT_DEF|OPTION_STATUS_DEF|CODEC1_INTR_NOT_DEF|CODEC0_INTR_NOT_DEF)
#define CHANGE_INTR_MASK (RING_STATUS_DEF|LOOP_CURRENT_DEF|DTMF_STATUS_DEF)

#define FRAME_INTR_MASK (CODEC1_INTR_NOT_DEF|CODEC0_INTR_NOT_DEF)
#define TLI_INTR_MASK (RING_STATUS_DEF|LOOP_CURRENT_DEF|DTMF_STATUS_DEF)

#define DTMF_KEY(s)		(((s)>>21)&0x0f)

#define DSP_HOST_INTR(s)	(((s) & DSP_HOST_INTR_NOT_DEF)== 0)
#define FRAME_INTR(s)	(((s) & CODEC1_INTR_NOT_DEF & CODEC0_INTR_NOT_DEF)==0)
#define RING_INTR(s)		((s) & RING_STATUS_DEF)
#define DTMF_INTR(s)		((s) & DTMF_STATUS_DEF)
#define CURRENT_LOOP_INTR(s)	((s) &LOOP_CURRENT_DEF)
#define OPTIONS_INTR(s)		((s) & OPTION_STATUS_DEF)

#define QIOLOFIINFO	_IOR('a', 1, struct lofi_info *)
#define QIOLOFITOFFS	_IOW('a', 2, int)

#define LOFI_REG_OFFSET	(0)

/* assumes low to hi bit order */
struct lofi_status_reg {
	unsigned int s_u0 : 14;
	unsigned int s_ed : 1;
	unsigned int s_hs : 1;
	unsigned int s_ea : 1;
	unsigned int s_dm : 1;
	unsigned int s_optstat_l : 1;
	unsigned int s_hoststat_l : 1;
	unsigned int s_dtmfvalid : 1;
	unsigned int s_dtmf : 4;
	unsigned int s_dtmfstat : 1;
	unsigned int s_lcdstat : 1;
	unsigned int s_ringstat : 1;
	unsigned int s_et : 1;
	unsigned int s_ef : 1;
	unsigned int s_codec0stat_l : 1;
	unsigned int s_codec1stat_l : 1;
} ;

#endif 	/* LOFI_REG_H */
