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
 * $NCDId: @(#)soundlib.h,v 1.21 1994/04/07 20:33:34 greg Exp $
 */

#ifndef _SOUNDLIB_H_
#define _SOUNDLIB_H_

#include <stdio.h>
#include <audio/sound.h>

#define SOUNDLIB_VERSION	2

#ifndef _SOUNDLIB_C_
/*****************************************************************************
 * 			       PUBLIC VARIABLES			     	     *
 *****************************************************************************/
extern unsigned int
                AuSoundFileChunkSize,		/* size of chunks for
						 * reading/writing files */
                AuSoundPortDuration,		/* duration for
						 * imports/exports */
                AuSoundPortLowWaterMark,	/* low water mark for
						 * imports/exports (in
						 * percent of duration) */
                AuSoundPortHighWaterMark;	/* high water mark for
						 * imports/exports (in
						 * percent of duration) */
extern AuBool   AuSoundRestartHardwarePauses;	/* re-start a flow if the
						 * hardware can't keep up */
#endif						/* !_SOUNDLIB_C_ */

/*****************************************************************************
 * 			       PUBLIC INTERFACES			     *
 *****************************************************************************/

extern          AuBucketID
AuSoundCreateBucketFromFile(
#if NeedFunctionPrototypes
			    AuServer *,		/* server */
			    _AuConst char *,	/* filename */
			    AuUint32,	/* access */
			    AuBucketAttributes **,	/* RETURN_attributes */
			    AuStatus *		/* RETURN_status */
#endif
);

extern          AuBool
AuSoundCreateFileFromBucket(
#if NeedFunctionPrototypes
			    AuServer *,		/* server */
			    _AuConst char *,	/* filename */
			    int,		/* file format */
			    AuBucketID,		/* bucket */
			    AuStatus *		/* RETURN_status */
#endif
);

extern          AuBucketID
AuSoundCreateBucketFromData(
#if NeedFunctionPrototypes
			    AuServer *,		/* server */
			    Sound,		/* sound */
			    AuPointer,		/* data */
			    AuUint32,	/* access */
			    AuBucketAttributes **,	/* RETURN_attributes */
			    AuStatus *		/* RETURN_status */
#endif
);

extern AuPointer
AuSoundCreateDataFromBucket(
#if NeedFunctionPrototypes
			    AuServer *,		/* server */
			    AuBucketID,		/* bucket */
			    Sound *,		/* RETURN_sound */
			    AuStatus *		/* RETURN_status */
#endif
);

extern AuEventHandlerRec *
AuSoundPlayFromFile(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    _AuConst char *,		/* filename */
		    AuDeviceID,			/* destination */
		    AuFixedPoint,		/* volume */
		    void (*) (			/* done_callback */
#if NeedNestedPrototypes
			      AuServer *,	/* server */
			      AuEventHandlerRec *,	/* which */
			      AuEvent *,	/* event */
			      AuPointer		/* callback data */
#endif
			      ),
		    AuPointer,			/* callback data */
		    AuFlowID *,			/* RETURN_flow */
		    int *,			/* RETURN_volume_mult_elem */
		    int *,			/* RETURN_monitor_element */
		    AuStatus *			/* RETURN_status */
#endif
);

extern AuEventHandlerRec *
AuSoundPlayFromData(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    Sound,			/* sound */
		    AuPointer,			/* data */
		    AuDeviceID,			/* destination */
		    AuFixedPoint,		/* volume */
		    void (*) (			/* done_callback */
#if NeedNestedPrototypes
			      AuServer *,	/* server */
			      AuEventHandlerRec *,	/* which */
			      AuEvent *,	/* event */
			      AuPointer		/* callback data */
#endif
			      ),
		    AuPointer,			/* callback data */
		    AuFlowID *,			/* RETURN_flow */
		    int *,			/* RETURN_volume_mult_elem */
		    int *,			/* RETURN_monitor_element */
		    AuStatus *			/* RETURN_status */
#endif
);

extern AuEventHandlerRec *
AuSoundRecordToData(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    Sound,			/* sound */
		    AuPointer,			/* data */
		    AuDeviceID,			/* destination */
		    AuFixedPoint,		/* volume */
		    void (*) (			/* done_callback */
#if NeedNestedPrototypes
			      AuServer *,	/* server */
			      AuEventHandlerRec *,	/* which */
			      AuEvent *,	/* event */
			      AuPointer		/* callback data */
#endif
			      ),
		    AuPointer,			/* callback data */
		    int,			/* line mode */
		    AuFlowID *,			/* RETURN_flow */
		    int *,			/* RETURN_volume_mult_elem */
		    AuStatus *			/* RETURN_status */
#endif
);

extern AuEventHandlerRec *
AuSoundRecordToFile(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    _AuConst char *,		/* filename */
		    AuDeviceID,			/* source */
		    AuFixedPoint,		/* gain */
		    void (*) (			/* done_callback */
#if NeedNestedPrototypes
			      AuServer *,	/* server */
			      AuEventHandlerRec *,	/* which */
			      AuEvent *,	/* event */
			      AuPointer		/* callback data */
#endif
			      ),
		    AuPointer,			/* callback data */
		    int,			/* mode */
		    int,			/* file format */
		    char *,			/* comment */
		    AuUint32,		/* rate */
		    int,			/* data format */
		    AuFlowID *,			/* RETURN_flow */
		    int *,			/* RETURN_volume_mult_elem */
		    AuStatus *			/* RETURN status */
#endif
);

extern AuEventHandlerRec *
AuSoundRecordToFileN(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    _AuConst char *,		/* filename */
		    AuDeviceID,			/* source */
		    AuFixedPoint,		/* gain */
		    AuUint32,		/* num samples */
		    void (*) (			/* done_callback */
#if NeedNestedPrototypes
			      AuServer *,	/* server */
			      AuEventHandlerRec *,	/* which */
			      AuEvent *,	/* event */
			      AuPointer		/* callback data */
#endif
			      ),
		    AuPointer,			/* callback data */
		    int,			/* mode */
		    int,			/* file format */
		    char *,			/* comment */
		    AuUint32,		/* rate */
		    int,			/* data format */
		    AuFlowID *,			/* RETURN_flow */
		    int *,			/* RETURN_volume_mult_elem */
		    AuStatus *			/* RETURN status */
#endif
);

extern          AuBool
AuSoundPlaySynchronousFromFile(
#if NeedFunctionPrototypes
			       AuServer *,	/* server */
			       _AuConst char *,	/* filename */
			       int		/* volume */
#endif
);

extern AuEventHandlerRec *
AuSoundRecordToBucket(
#if NeedFunctionPrototypes
		      AuServer *,		/* server */
		      AuBucketID,		/* destination */
		      AuDeviceID,		/* source */
		      AuFixedPoint,		/* gain */
		      void (*) (		/* done_callback */
#if NeedNestedPrototypes
				AuServer *,	/* server */
				AuEventHandlerRec *,	/* which */
				AuEvent *,	/* event */
				AuPointer	/* callback data */
#endif
				),
		      AuPointer,		/* callback data */
		      int,			/* mode */
		      AuFlowID *,		/* RETURN_flow */
		      int *,			/* RETURN_volume_mult_elem */
		      AuStatus *		/* RETURN status */
#endif
);

extern AuEventHandlerRec *
AuSoundPlayFromBucket(
#if NeedFunctionPrototypes
		      AuServer *,		/* server */
		      AuBucketID,		/* source */
		      AuDeviceID,		/* destination */
		      AuFixedPoint,		/* volume */
		      void (*) (		/* done_callback */
#if NeedNestedPrototypes
				AuServer *,	/* server */
				AuEventHandlerRec *,	/* which */
				AuEvent *,	/* event */
				AuPointer	/* callback data */
#endif
				),
		      AuPointer,		/* callback data */
		      int,			/* loop count */
		      AuFlowID *,		/* RETURN_flow */
		      int *,			/* RETURN_volume_mult_elem */
		      int *,			/* RETURN_monitor_element */
		      AuStatus *		/* RETURN_status */
#endif
);
#endif						/* !_SOUNDLIB_H_ */
