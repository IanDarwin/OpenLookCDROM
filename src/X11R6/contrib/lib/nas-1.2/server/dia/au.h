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
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)au.h,v 1.19 1994/06/01 01:19:12 greg Exp $
 */

#ifndef _AU_H_
#define _AU_H_

#ifdef sun
#include "../dda/sun/ausun.h"
#endif						/* sun */

#ifdef sgi
#include "../dda/sgi/ausgi.h"
#endif						/* sgi */

#if defined(SVR4) && defined(SYSV386)
#include "../dda/voxware/auvoxsvr4.h"
#endif

#if defined(__FreeBSD__) || defined(linux)
#include "../dda/voxware/auvoxware.h"
#endif

typedef AuUint32 AuPointer;

#define auMaxTracks			32

#define	AuElementNotifyKindSpecial	0xff

#if (defined(SYSV) && !defined(ISC)) || defined(SVR4)
#ifndef bcopy
#define bcopy(a,b,c)			memmove((b),(a),(c))
#endif

#ifndef bzero
#define bzero(a,b)			memset((a),0,(b))
#endif
#endif						/* SYSV && !ISC || SVR4 */

#define aumin(x, y)			((x) < (y) ? (x) : (y))
#define aumax(x, y)			((x) > (y) ? (x) : (y))
#define aucopy(_src, _dst, _size)	bcopy(_src, _dst, _size)
#define auset(_dst, _val, _size)	memset(_dst, _val, _size)
#define auclr(_dst, _size)		bzero(_dst, _size)
#define aualloc				auProtectedAlloc
#define aufree				auProtectedFree
#define aurealloc			auProtectedRealloc

AuPointer       auProtectedAlloc(), auProtectedRealloc(), aucalloc();
void            auProtectedFree();

#define NUMARRAY(x)			(sizeof(x) / sizeof((x)[0]))
#define PAD4(x)				(((x) + 3) & ~3)

#define	NotAPhysicalDevice		0
#define PhysicalOutputMono		(1L << 0)
#define PhysicalOutputLeft		(1L << 1)
#define PhysicalOutputRight		(1L << 2)
#define PhysicalOutputStereo		(1L << 3)
#define AllPhysicalOutputs	       ((1L << 4) - 1)

#define PhysicalInputMono		(1L << 4)
#define PhysicalInputStereo		(1L << 5)
#define AllPhysicalInputs	      (((1L << 6) - 1) ^ AllPhysicalOutputs)


#define AuValidState(s)							       \
    ((s) == AuStateStop || (s) == AuStatePause || (s) == AuStateStart)

#define AuValidTriggerState(s)						       \
    ((s) == AuStateAny || AuValidState(s))

#define AuValidTriggerReason(r)						       \
    (((r) == AuReasonAny) ||						       \
     ((r) == AuReasonUser) ||						       \
     ((r) == AuReasonUnderrun) ||					       \
     ((r) == AuReasonOverrun) ||					       \
     ((r) == AuReasonEOF) ||						       \
     ((r) == AuReasonWatermark) ||					       \
     ((r) == AuReasonHardware))

#define AddToLinkedList(head, item)					       \
{									       \
    (item)->prev = NULL;						       \
    (item)->next = (head);						       \
    if (head)								       \
	(head)->prev = (item);						       \
    head = (item);							       \
}

#define RemoveFromLinkedList(head, item)				       \
{									       \
    if ((item)->next)							       \
	(item)->next->prev = (item)->prev;				       \
									       \
    if ((item)->prev)							       \
	(item)->prev->next = (item)->next;				       \
    else								       \
	(head) = (item)->next;						       \
}


#define AU_ERROR(e, v)							       \
{									       \
    client->errorValue = (v);						       \
    return (e);								       \
}

#define sizeofFormat(f)		auBytesPerSample[f]

typedef struct
{
    AuUint8         type;			/* AuStringLatin1, ... */
    AuUint32        len;			/* length in bytes */
    AuInt8         *string;			/* the data */
}               StringRec, *StringPtr;

typedef struct _ComponentRec
{
    /* external common stuff */
    AuID            id;				/* who am I? */
    AuUint32        numSamples,			/* for ports and buckets */
                    changableMask,		/* what can be changed */
                    valueMask;			/* what is present */
    AuUint8         kind,			/* what type of component is
						 * it */
                    use,			/* importable, exportable,
						 * etc. */
                    access,			/* who can access */
                    format,			/* data format */
                    numTracks;			/* 1 for mono, 2 for stereo,
						 * etc. */
    StringRec       description;		/* a text description */

    /* external physical stuff */
    AuUint32        minSampleRate,
                    maxSampleRate,
                    location;			/* mask of bits */
    AuFixedPoint    gain;			/* input gain (16.16%) */
    AuUint8         lineMode,			/* mic/line */
                    numChildren;		/* number of subcomponents,
						 * if any */
    AuID           *children;			/* subcomponents, if any */
    AuInt8         *childSwap;			/* swap space for children */

    /* external port stuff */
    AuUint32        lowWaterMark,
                    highWaterMark;

#ifdef notyet
    /* external virtual stuff */
    AuUint32        station;			/* for radios */
#endif						/* notyet */

    /* internal common stuff */
    AuUint32        refCnt,			/* reference count */
                    readTag,
                    sampleRate,
                    physicalDeviceMask,
                    minibufSize,		/* mini-buffer size */
		    bytesPerSample,
		    minibufSamples,
                    dataSize;			/* size of data in bytes */
    AuUint8        *minibuf,			/* chunk storage */
                   *data,			/* start of data */
                   *dataEnd;			/* end of data */

    /* internal port stuff */
    AuUint8        *write,			/* write pointer */
                   *read;			/* read pointer */
    AuUint32        incoming,			/* how much data are we
						 * expecting */
                    outgoing,			/* how much data have we sent */
                    currentSize;		/* how much data is in the
						 * port */
    AuBool          discard,			/* true if we're to clear a
						 * port when starting it */
                    eof,			/* true if we've received an
						 * EOF on this port */
                    processed;			/* used during processing */

    /* internal bucket stuff */
    AuBool          destroyed,			/* true if the bucket has
						 * been destroyed */
                    list;			/* used in ListBuckets */

    /* internal wave form stuff */
    AuUint32        frequency,
                    waveSamples;

    struct _ComponentRec *prev,			/* linked list pointers */
                   *next;
}               ComponentRec, *ComponentPtr;


typedef struct _FlowRec FlowRec,
               *FlowPtr;

typedef struct
{
    FlowPtr         flow;
    AuUint8         triggerState,
                    triggerPrevState,
                    triggerReason,
                    action,
                    elementNum,
                    newState;
}               ActionRec, *ActionPtr;

typedef struct
{
    AuPointer       client;			/* client that owns this
						 * element */
    AuUint32        state,
                    prevState,
                    elementNum,
                    numBytes,
                    refCnt,
                    varLen,
                    numActions,
		    currentSample,
		    nextSample,
                    minibufSamples,
		    minibufBytes;
    FlowPtr         flow;
    AuBool          compiled,			/* true if we've compiled
						 * this element */
                    setup,			/* true if we've setup this
						 * element */
                    countSamples,
                    stateChange,		/* true if there's been a
						 * state change */
                    isComponent,		/* true if this is a
						 * component element */
                    parmsChanged;		/* true if the element
						 * parameters changed */
    AuUint8        *minibuf;			/* pointer to chunk storage */
    auElement      *raw;			/* the raw element */
    ActionPtr       actions;			/* the actions */
    ComponentPtr    component;

    /* instance data for buffers */
    AuUint8        *read,			/* read pointer */
                   *write;			/* write pointer */
    AuUint32        sampleRate,
                    readTag;

    /* stuff used in processing the flow */
    void            (*toNativeFormat) (),
                    (*fromNativeFormat) ();
    AuInt32         minibufChunk;		/* number of bytes to read to
						 * fill up minibuffer after
						 * format conversion */
}               FlowElementRec, *FlowElementPtr;

typedef struct
{
    ComponentPtr    component;			/* the input component */
    FlowElementPtr  flowEl;			/* the flow element */
    AuFixedPoint    multiplyConstant,		/* (16.16) */
                    addConstant;		/* (16.16) */
    AuUint8         numTracks,
                    inTrack[auMaxTracks],
                    outTrack[auMaxTracks];
    AuUint32        (*rateConvert) (),
                   *preadTag;
    AuBool          (*readInput) ();
    void           *devPrivate;			/* device specific data */
    void            (*freeDevPrivate) ();	/* free function for device
						 * specific data */
}               CompiledFlowInputRec, *CompiledFlowInputPtr;

typedef struct
{
    ComponentPtr    component;			/* the output component */
    FlowElementPtr  flowEl;			/* the flow element */
    AuUint32        numInputs,			/* how many inputs */
                    firstInput;			/* head of the input chain */
    CompiledFlowInputPtr inputs;		/* the inputs */
    AuUint32        (*rateConvert) ();
    AuBool          (*writeOutput) ();
    void           *devPrivate;			/* device specific data */
    void            (*freeDevPrivate) ();	/* free function for device
						 * specific data */
}               CompiledFlowOutputRec, *CompiledFlowOutputPtr;

typedef struct
{
    AuUint32        numOutputs,			/* how many outputs */
                    physicalDeviceMask;		/* mask of physical devices
						 * used */
    AuBool          freeComponents;
    CompiledFlowOutputPtr outputs;		/* the outputs */
    void            (*accumulateOutput) ();
    void           *devPrivate;			/* device specific data */
    void            (*freeDevPrivate) ();	/* free function for device
						 * specific data */
}               CompiledFlowRec, *CompiledFlowPtr;

struct _FlowRec
{
    AuFlowID        flowId;			/* flow id */
    AuUint32        numElements,		/* how many elements */
                    count,			/* used in trivial flows */
                    state,			/* current state */
                    varLen,			/* total size of variable
						 * data */
                    pendingState;		/* new state */
    AuBool          clocked,			/* true if flow is clocked */
                    trivial,			/* true if flow is just an
						 * import or export and a
						 * bucket */
                    needsRecompile;		/* true if flow needs to be
						 * recompiled */
    CompiledFlowRec compiled;			/* compiled flow */
    FlowElementPtr  elements;			/* flow elements */
    auElement      *raw;			/* the raw elements off the
						 * wire */
    struct _FlowRec *prev,			/* linked list pointers */
                   *next;
};

AuFixedPoint    AuFixedPointMultiply();

/* configuration stuff */

#define AU_ALLOC_DEVICE(d, numTracks, numChildren)			      \
{									      \
    int extra = (numChildren) * sizeof(AuID) * 2;			      \
									      \
    if (!((d) = (ComponentPtr) aualloc(PAD4(sizeof(ComponentRec)) + (extra))))\
	return AuBadAlloc;						      \
}

#define AU_ADD_DEVICE(d)						      \
{									      \
    d->bytesPerSample = d->numTracks * sizeofFormat(d->format);		      \
    d->refCnt = 0;							      \
									      \
    AddToLinkedList(auDevices, d);					      \
									      \
    if (!AddResource(d->id, auComponentType, d))			      \
	return AuBadAlloc;						      \
									      \
    auNumServerDevices++;						      \
    *auServerDeviceListSize += sizeof(auDeviceAttributes) +		      \
	PAD4(d->description.len) + PAD4(d->numChildren * sizeof(AuDeviceID)); \
}

#define AU_ALLOC_BUCKET(b)						      \
{									      \
    if (!((b) = (ComponentPtr) aualloc(sizeof(ComponentRec))))		      \
	return AuBadAlloc;						      \
}

#define AU_ADD_BUCKET(b)						      \
{									      \
    b->refCnt = 0;							      \
									      \
    AddToLinkedList(auBuckets, b);					      \
									      \
    if (!AddResource(b->id, auComponentType, b))			      \
	return AuBadAlloc;						      \
									      \
    auNumServerBuckets++;						      \
    *auServerBucketListSize += sizeof(auBucketAttributes) +		      \
	PAD4(b->description.len);					      \
}

enum _auCallbackTypes
{
    /* required */
    AuCreateServerComponentsCB,
    AuSetPhysicalOutputGainCB,
    AuGetPhysicalOutputGainCB,
    AuSetPhysicalInputGainAndLineModeCB,
    AuEnableProcessFlowCB,
    AuDisableProcessFlowCB,
    AuReadPhysicalInputsCB,
    AuSetWritePhysicalOutputFunctionCB,

    /* optional */
    AuSetSampleRateCB,
    AuEventPostedCB,

    AuMaxCB
};

typedef AuUint32 (*_pFunc) ();

#ifndef _AUUTIL_C_
extern AuUint32 auBytesPerSample[];
extern
#endif						/* !_AUUTIL_C_ */
_pFunc          AuCallbacks[AuMaxCB];

#define AuRegisterCallback(_n, _f) 	AuCallbacks[_n] = (_pFunc) (_f)
#define CallbackExists(_n)		AuCallbacks[_n]

#define AuCallbackIf(_n, _args)						      \
{									      \
    if (CallbackExists(_n))						      \
	(*AuCallbacks[_n]) _args;					      \
}

#define AuCallback(_n, _args)						      \
	(*AuCallbacks[_n]) _args

#ifndef WAKEUP_SERVER
#define WAKEUP_SERVER()		kill(getpid(), SIGUSR1)
#endif						/* !WAKEUP_SERVER */

#ifndef _AUPROCESS_C_
extern void     AuULAW8ToNative(), AuNativeToULAW8(),
                AuUnsigned8ToNative(), AuNativeToUnsigned8(),
                AuSigned8ToNative(), AuNativeToSigned8(),
                changeSign(), byteSwap(),
                AuNeg16LSBTo16MSB(), AuNeg16MSBto16LSB();
#endif /* !_AUPROCESS_C_ */

#if (auNativeFormat == AuFormatLinearSigned16MSB)
#define AuSigned16MSBToNative		(void (*) ()) 0
#define AuNativeToSigned16MSB		(void (*) ()) 0
#define AuUnsigned16MSBToNative		changeSign
#define AuNativeToUnsigned16MSB		changeSign
#define AuSigned16LSBToNative		byteSwap
#define AuNativeToSigned16LSB		byteSwap
#define AuUnsigned16LSBToNative		AuNeg16LSBTo16MSB
#define AuNativeToUnsigned16LSB		AuNeg16MSBto16LSB
#endif

#if (auNativeFormat == AuFormatLinearSigned16LSB)
#define AuSigned16MSBToNative		byteSwap
#define AuNativeToSigned16MSB		byteSwap
#define AuUnsigned16MSBToNative		AuNeg16MSBto16LSB
#define AuNativeToUnsigned16MSB		AuNeg16LSBTo16MSB
#define AuSigned16LSBToNative		(void (*) ()) 0
#define AuNativeToSigned16LSB		(void (*) ()) 0
#define AuUnsigned16LSBToNative		changeSign
#define AuNativeToUnsigned16LSB		changeSign
#endif

#endif						/* _AU_H_ */
