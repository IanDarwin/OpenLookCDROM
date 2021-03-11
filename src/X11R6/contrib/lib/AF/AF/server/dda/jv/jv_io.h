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

#ifndef	JVIO_H
#define	JVIO_H

#include "jv.h"
#include "jv_reg.h"
#include "physdevice.h"

#define	PRIMARY	0
#define	SECONDARY 1

#define	FEA	2
#define	FED	3
#define	FMD	4
#define	FSR	7

#define TO_RAM(x)		((x)<<8)
#define FROM_RAM(x)		((x)>>8)

void	initJv(char *devName, char *kernel,  jvPhysDevice *pDev);
void	closeJv( jvPhysDevice *pDev);

void	jvDSPDisable( jvPhysDevice *pDev);
void	jvDSPEnable( jvPhysDevice *pDev);

#endif	/* JVIO_H */
