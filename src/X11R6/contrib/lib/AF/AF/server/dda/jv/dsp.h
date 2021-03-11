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

#ifndef DSP_H
#define DSP_H

/* 
 *  This defines the start address (in the DSP shared memory) of the
 *  mailbox.  0x200 is just beyond the end of the on-chip memories.
 */
#if defined(LANGUAGE_C) || defined(__LANGUAGE_C__)
#define MBX_START	0x200
#else
#define MBX_START	$200
#endif

/* Unused MBX 
	CODEC_TIME	MBX_START+1
	CODEC_START0	MBX_START+2
	CODEC_LEN0	MBX_START+3
	CODEC_OFS0	MBX_START+4
	CODEC_START1	MBX_START+5
	CODEC_LEN1	MBX_START+6
	CODEC_OFS1	MBX_START+7
 */

/*
 * These are the default buffer sizes for the codec and hifi buffers (in
 * samples).
 */
#define HIFI_HWBUF      4096

/*
 *  This is the starting address of the circular buffers.  See
 *  dspSetBufs() for buffer allocation.
 */
#define BUFFER_START	0x1000

/*
 * "START" defines the start address for the buffers.  "LEN" defines the
 * length of the buffer.  "OFS" defines the offset from input to output.
 * Like this:
 *
 *	+--------------------------+	<--- START
 *      |                          |
 *      |       Input              |
 *      |                          |
 *      |                          |
 *	+--------------------------+	<--- START + LEN
 *
 *	+--------------------------+	<--- START + OFS
 *      |                          |
 *      |       Output             |
 *      |                          |
 *      |                          |
 *	+--------------------------+	<---- START + LEN + OFS
 */
 

/* 
 * A 16 bit timer specifying the Hifi time (for both channels)
 */
#define HIFI_TIME	MBX_START+8

/*
 * The HiFi buffers are arranged identically to the codec buffers
 */
#define HIFI_START0	MBX_START+9		/* hifi 0 */
#define HIFI_LEN0	MBX_START+10
#define HIFI_OFS0	MBX_START+11

#define HIFI_START1	MBX_START+12		/* hifi 1 */
#define HIFI_LEN1	MBX_START+13
#define HIFI_OFS1	MBX_START+14

/*
 * This value gets written to the DSP's parallel port C at startup.
 * For most DSP Port devices, this 3-bit value sets the sample rate.
 */
#define PORTC_BITS	MBX_START+15

/*
 * These values get written to the DSP's port control registers to
 * configure the DSP port.  For more information, see the DSP56000
 * documentation.
 */

#define DSP_CRA		MBX_START+16
#define DSP_CRB		MBX_START+17

/*
 * Build definitions for the assembler
 */
#if !defined(LANGUAGE_C) && !defined(__LANGUAGE_C__)

mbx_start	equ	MBX_START
hifi_time	equ	HIFI_TIME

hifi_start0	equ	HIFI_START0
hifi_len0	equ	HIFI_LEN0
hifi_ofs0	equ	HIFI_OFS0

hifi_start1	equ	HIFI_START1
hifi_len1	equ	HIFI_LEN1
hifi_ofs1	equ	HIFI_OFS1

portc_bits	equ	PORTC_BITS

dsp_cra		equ 	DSP_CRA
dsp_crb		equ	DSP_CRB

#else

/*
 * C Specific definitions go here
 */
typedef unsigned int DSPTime;

#endif

#endif
