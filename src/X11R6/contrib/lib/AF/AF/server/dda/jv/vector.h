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
 * DSP56K Vector Locations
 */
#define	CV_I_RESET	0
#define	CV_I_STACK	2
#define	CV_I_TRACE	4
#define	CV_I_SWI	6
#define	CV_I_IRQA	8	/* Codec */
#define	CV_I_IRQB	10	/* TLI, etc. */
#define	CV_I_SSIRX	12
#define	CV_I_SSIRX_EXC	14
#define	CV_I_SSITX	16
#define	CV_I_SSITX_EXC	18
#define	CV_I_SCIRX	20
#define	CV_I_SCIRX_EXC	22
#define	CV_I_SCITX	24
#define	CV_I_SCI_IDLE	26
#define	CV_I_SCI_TIMER	28
#define	CV_I_NMI	30
#define	CV_I_HOST_RX	32
#define	CV_I_HOST_TX	34
#define	CV_I_HOST_CMD	36
#define	CV_I_HOST_0	38	/* SIGABRT */
#define	CV_I_HOST_1	40	/* SIGFPE */
#define	CV_I_HOST_2	42	/* SIGINT */
#define	CV_I_HOST_3	44	/* SIGSEGV */
#define	CV_I_HOST_4	46	/* SIGTERM */
#define	CV_I_HOST_5	48
#define	CV_I_HOST_6	50
#define	CV_I_HOST_7	52
#define	CV_I_HOST_8	54
#define	CV_I_HOST_9	56	/* temp */
#define	CV_I_HOST_10	58	/* MS Timer interrupt.		*/
#define	CV_I_HOST_11	60	/* Host to DSP doorbell.	*/
#define	CV_I_ILL	62


#define	CV_I_MSTIMER	CV_I_HOST_10
#define	CV_I_DSPALERT	CV_I_HOST_11
