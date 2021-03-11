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

#ifndef	LOFIIO_H
#define	LOFIIO_H

#include <io/tc/lofi.h>
#include <io/tc/lofi_reg.h>
#include "physdevice.h"

#define	RPHYSICAL	0
#define	RRAM	1
#define	RROM	2
#define	RCSR	3
#define	RCODEC0	4
#define	RCODEC1	5
#define	ROPTION	6
#define	RHOST	7

#define	FCA	0
#define	FHS	1
#define	FEA	2
#define	FED	3
#define	FMD	4
#define	FIE	5
#define	FGC	6

#define TO_RAM(x)		((x)<<8)
#define FROM_RAM(x)		((x)>>8)

#define LoFiReadCSR(reg)	((reg)->rd_csr)

void	msleep(struct lofi_reg *lp, int ms);
void	LoFiSetCSR(struct lofi_reg *lp, int field, int val);
void	initLoFi(char *devName, char *kernel,  lofiPhysDevice *pDev);
void	closeLoFi( lofiPhysDevice *pDev);

void	lofiTliEnable( lofiPhysDevice *pDev);
void	lofiTliDisable( lofiPhysDevice *pDev);
void	lofiDSPDisable( lofiPhysDevice *pDev);
void	lofiDSPEnable( lofiPhysDevice *pDev);
void	lofiCodecEnable( lofiPhysDevice *pDev);
void	lofiCodecDisable( lofiPhysDevice *pDev);

int lofiReadHostPort(struct lofi_info *lofi, int reg);
void lofiWriteHostPort(struct lofi_info *lofi, int reg, int d);

#endif	/* LOFIIO_H */
