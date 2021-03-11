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

/* 
 *  A 16-bit timer specifying the codec time (for both codecs)
 */
#define CODEC_TIME	MBX_START+1

/*
 * These are the default buffer sizes for the codec and hifi buffers (in
 * samples).
 */
#define HIFI_HWBUF      4096
#define LENHWBUF        1024

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
 
#define CODEC_START0	MBX_START+2		/* codec 0 */
#define CODEC_LEN0	MBX_START+3
#define CODEC_OFS0	MBX_START+4

#define CODEC_START1	MBX_START+5		/* codec 1 */
#define CODEC_LEN1	MBX_START+6
#define CODEC_OFS1	MBX_START+7

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

/* extra fields for the Teleport support
 *
 */

#define DSP_LCTL        MBX_START+18
#define DSP_RCTL        MBX_START+19
#define DSP_MASKI       MBX_START+20
#define DSP_MASKO       MBX_START+21
#define DSP_LSTAT       MBX_START+22
#define DSP_RSTAT       MBX_START+23
/*
 * Build definitions for the assembler
 */
#if !defined(LANGUAGE_C) && !defined(__LANGUAGE_C__)

mbx_start	equ	MBX_START
codec_time	equ	CODEC_TIME

codec_start0	equ	CODEC_START0
codec_len0	equ	CODEC_LEN0
codec_ofs0	equ	CODEC_OFS0

codec_start1	equ	CODEC_START1
codec_len1	equ	CODEC_LEN1
codec_ofs1	equ	CODEC_OFS1

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

dsp_lctl	equ	DSP_LCTL
dsp_rctl	equ	DSP_RCTL
dsp_maski	equ	DSP_MASKI
dsp_masko	equ	DSP_MASKO
dsp_lstat	equ	DSP_LSTAT
dsp_rstat	equ	DSP_RSTAT

#else

/*
 * C Specific definitions go here
 */
typedef unsigned int DSPTime;

#endif

#endif
