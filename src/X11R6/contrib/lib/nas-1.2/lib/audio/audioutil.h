/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)audioutil.h,v 1.20 1994/02/02 18:35:46 greg Exp $
 * 
 * <audio/audioutil.h>
 * 
 * This file contains utilities for using the audio library.
 */

#ifndef AUDIOUTIL_H_
#define AUDIOUTIL_H_

_AUFUNCPROTOBEGIN

/*****************************************************************************
 *				 FLOW UTILITIES				     *
 *****************************************************************************/

extern          AuFlowID
AuGetScratchFlow(
#if NeedFunctionPrototypes
		 AuServer *,			/* server */
		 AuStatus *			/* RETURN_status */
#endif
);

extern void
AuReleaseScratchFlow(
#if NeedFunctionPrototypes
		     AuServer *,		/* server */
		     AuFlowID,			/* flow */
		     AuStatus *			/* RETURN_status */
#endif
);

extern          AuFlowID
AuGetScratchFlowToBucket(
#if NeedFunctionPrototypes
			 AuServer *,		/* server */
			 AuBucketID,		/* bucket */
			 int *,			/* RETURN_import */
			 AuStatus *		/* RETURN_status */
#endif
);

extern          AuFlowID
AuGetScratchFlowFromBucket(
#if NeedFunctionPrototypes
			   AuServer *,		/* server */
			   AuBucketID,		/* bucket */
			   int *,		/* RETURN_export */
			   AuStatus *		/* RETURN_status */
#endif
);

extern void
AuStartFlow(
#if NeedFunctionPrototypes
	   AuServer *,				/* server */
	   AuFlowID,				/* flow */
	   AuStatus *				/* RETURN_status */
#endif
);

extern void
AuStopFlow(
#if NeedFunctionPrototypes
	   AuServer *,				/* server */
	   AuFlowID,				/* flow */
	   AuStatus *				/* RETURN_status */
#endif
);

extern void
AuPauseFlow(
#if NeedFunctionPrototypes
	   AuServer *,				/* server */
	   AuFlowID,				/* flow */
	   AuStatus *				/* RETURN_status */
#endif
);

/*****************************************************************************
 *			       EVENT DISPATCHING			     *
 *****************************************************************************/

extern void
AuHandleEvents(
#if NeedFunctionPrototypes
	       AuServer *			/* server */
#endif
);

extern          AuBool
AuDispatchEvent(
#if NeedFunctionPrototypes
		AuServer *,			/* server */
		AuEvent *
#endif
);

extern AuEventHandlerRec *
AuRegisterEventHandler(
#if NeedFunctionPrototypes
		       AuServer *,		/* server */
		       AuMask,			/* value_mask */
		       int,			/* type */
		       AuID,			/* id */
		       AuEventHandlerCallback,	/* callback */
		       AuPointer		/* data */
#endif
);

extern void
AuUnregisterEventHandler(
#if NeedFunctionPrototypes
			 AuServer *,		/* server */
			 AuEventHandlerRec *	/* handler */
#endif
);

extern AuEventHandlerRec *
AuLookupEventHandler(
#if NeedFunctionPrototypes
		     AuServer *,		/* server */
		     AuEvent *,			/* event */
		     AuEventHandlerRec *	/* startwith */
#endif
);

/*****************************************************************************
 *			       STRING UTILITIES				     *
 *****************************************************************************/

_AuConst char  *
AuFormatToString(
#if NeedFunctionPrototypes
		 unsigned int			/* format */
#endif
);

int
AuStringToFormat(
#if NeedFunctionPrototypes
		 _AuConst char *		/* string */
#endif
);

_AuConst char  *
AuFormatToDefine(
#if NeedFunctionPrototypes
		 unsigned int			/* format */
#endif
);

int
AuDefineToFormat(
#if NeedFunctionPrototypes
		 _AuConst char *		/* define */
#endif
);

_AuConst char  *
AuWaveFormToString(
#if NeedFunctionPrototypes
		 unsigned int			/* waveform */
#endif
);

int
AuStringToWaveForm(
#if NeedFunctionPrototypes
		 _AuConst char *		/* string */
#endif
);

/*****************************************************************************
 *			     DATA CONVERSION UTILITIES			     *
 *****************************************************************************/

int
AuConvertDataToShort(
#if NeedFunctionPrototypes
		     int,			/* data format */
		     int,			/* num bytes */
		     AuPointer			/* data */
#endif
);

int
AuConvertShortToData(
#if NeedFunctionPrototypes
		     int,			/* data format */
		     int,			/* num bytes */
		     AuPointer			/* data */
#endif
);

/*****************************************************************************
 *			       	     MISC				     *
 *****************************************************************************/

AuEventHandlerRec *
AuMonitorDevice(
#if NeedFunctionPrototypes
		AuServer *,			/* server */
		int,				/* sample rate */
		AuDeviceID,			/* input device */
		AuDeviceID,			/* output device */
		AuFixedPoint,			/* volume */
		void (*) (			/* done_callback */
#if NeedNestedPrototypes
			  AuServer *,		/* server */
			  AuEventHandlerRec *,	/* which */
			  AuEvent *,		/* event */
			  AuPointer		/* callback data */
#endif
			  ),
		AuPointer,			/* callback data */
		AuFlowID *,			/* RETURN_flow */
		int *,				/* RETURN_volume_mult_element */
		int *,				/* RETURN_monitor_element */
		AuStatus *			/* RETURN_status */
#endif
);

_AUFUNCPROTOEND

#endif						/* AUDIOUTIL_H_ */
