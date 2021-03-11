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

#ifndef	LOFI_TLI_H
#define	LOFI_TLI_H

#include <server/include/misc.h>
/*
 *
 * Fri Oct 12 16:53:06 EDT 1990 tml
 *	Certified Lo-Fi to pt. 68 and DOC at DS&G. 
 *******************************************************************
 *****  WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****
 *******************************************************************
 * These parameters have been set during testing for compliance with 
 * DOC and FCC Pt. 68.  These parameters should not be changed 
 * without awareness of the pertinent regulatory issues.
 */

/* Dialing */
#define DTMF_ID		50		/* Certified in range [50,1000] ms */
#define	DTMF_ON		60		/* Certified in range [60,1000] ms */
#define	DTMF_ON_MAX	1000
#define	DTMF_ON_MIN	60
#define	DTMF_ID_MAX	1000
#define	DTMF_ID_MIN	50

#define	DTMF_STG	(0.0)		/* */
#define	DTMF_ATGLO	(-4.0)		/* Twist */
#define	DTMF_ATGHI	(-2.0)		/* Twist */
#define	DTMF_GER	(0.0)		/* */

#define	PULSE_ID	750		/* ms. */
#define	PULSE_MAKE	38		/* ms. Roughly 10 PPS */
#define	PULSE_BREAK	58		/* ms. */

#define DIAL_TONE	0		/* Selectively dial tone. */
#define DIAL_PULSE	1		/* Selectively dial pulse. */

extern void lofi_tli_flash();
extern int  lofi_tli_pulse();
extern void lofi_tli_onhook();
extern void lofi_tli_offhook();
extern int lofi_dial(struct lofi_info *lofi, char *number);

#endif	/* LOFI_TLI_H */
