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
/* $Header: /crl/audio/AF/server/include/RCS/acstruct.h,v 1.26 1994/02/22 17:19:51 stewart Exp $ */

#ifndef ACSTRUCT_H
#define ACSTRUCT_H

#include <ac.h>
#include <audiodev.h>
#include <AFConvert.h>

/*
 * functions which modify the state of the AC
 */

typedef struct _ACFuncs {
    void	(* ChangeAC)(void *, struct _AC *, int);	 
    void	(* DestroyAC)(struct _AC *); 
} ACFuncs;

/*
 * Operations invoked through AC.
 */

typedef struct _ACOps {
    int	(* ConvertPlay)(ATime, unsigned char *, int , struct _AC *);
    int	(* ConvertRec)(ATime, unsigned char *, int , struct _AC *);
} ACOps;

typedef struct _AC {
    AudioDevicePtr	aDev;		
    AEncodeType		playType;
    AEncodeType		recType;
    int			playGain;
    int			recordGain;
    int			startTimeout; /* start silence timeout, def. infinity*/
    int			endSilence;   /* end silence timout, def. inifinity */
    ABool		preempt;      /* whether it should preempt */
    unsigned long	stateChanges;	/* masked with AC_<kind> */
    unsigned long	serialNumber;
    ACFuncs		*funcs;
    ACOps		*ops;
    int			recRef;		/* record reference count */
    float               playGainMul;    /* multiplier corresp. playGain */
    float               recGainMul;    /* multiplier corresp. recGain */
    pointer             privPtr;       /* ac private structure, if any */
    AFConvert_Data      uncompressState;
    AFConvert_Data      compressState;
} AC;

#endif
