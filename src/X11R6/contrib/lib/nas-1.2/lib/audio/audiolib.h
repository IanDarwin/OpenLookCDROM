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
 * $NCDId: @(#)audiolib.h,v 1.61 1994/04/07 20:40:13 greg Exp $
 * 
 * <audio/audiolib.h>
 * 
 * This file contains the low-level application programming interface for the
 * NCD-AUDIO service.
 */

#ifndef _NCD_AUDIOLIB_H_
#define _NCD_AUDIOLIB_H_

/*****************************************************************************
 *				    VERSION				     *
 *****************************************************************************/

#define AudioLibraryVersion	2



/*****************************************************************************
 *			    GET NEEDED HEADER FILES			     *
 *****************************************************************************/

#ifdef USG
#ifndef __TYPES__
#include <sys/types.h>				/* forgot to protect it... */
#define __TYPES__
#endif						/* __TYPES__ */
#else
#if defined(_POSIX_SOURCE) && defined(MOTOROLA)
#undef _POSIX_SOURCE
#include <sys/types.h>
#define _POSIX_SOURCE
#else
#include <sys/types.h>
#endif
#endif						/* USG */

#include <audio/audio.h>

/* applications should not depend on these two headers being included! */
#include <audio/Afuncproto.h>
#include <audio/Aosdefs.h>


#if 0
#define AU_MACRO_PREFIX do {
#define AU_MACRO_SUFFIX } while (0)
#else
#define AU_MACRO_PREFIX {
#define AU_MACRO_SUFFIX }
#endif

/*****************************************************************************
 *				  DATA TYPES				     *
 *****************************************************************************/

#define AuEventsQueuedAlready		0
#define AuEventsQueuedAfterReading	1
#define AuEventsQueuedAfterFlush	2

#define AU_MAX_SCRATCH_FLOWS		3

#ifdef AU_NOT_STDC_ENV
typedef char   *AuPointer;
#else
typedef void   *AuPointer;
#endif

#if NeedFunctionPrototypes
/* struct pre-declarations */
struct _AuErrorEvent;
struct _AuServer;
struct _AuEventHandlerRec;
struct _AuEventEnqHandlerRec;
struct _AuSyncHandlerRec;
union _AuEvent;
#endif

typedef struct _AuString
{
    int             type;			/* AuStringLatin1, ... */
    int             len;			/* length in bytes */
    char           *data;
}               AuString;

#define AuSetString(_s, _type, _len, _data)				      \
AU_MACRO_PREFIX								      \
    (_s)->type = _type;							      \
    (_s)->len = _len;							      \
    (_s)->data = _data;							      \
AU_MACRO_SUFFIX

typedef
AuBool(*AuErrorHandler) (
#if NeedFunctionPrototypes
			 struct _AuServer *,	/* server */
			 struct _AuErrorEvent *	/* error_event */
#endif
);

typedef
AuBool(*AuIOErrorHandler) (
#if NeedFunctionPrototypes
			   struct _AuServer *	/* server */
#endif
);

typedef
AuBool(*AuEventHandlerCallback) (
#if NeedFunctionPrototypes
				 struct _AuServer *,	/* server */
				 union _AuEvent *,	/* event */
				 struct _AuEventHandlerRec *	/* who invoked */
#endif
);

#define AuEventHandlerTypeMask		(1L << 0)
#define AuEventHandlerIDMask		(1L << 1)
#define AuEventHandlerAllMasks	       ((1L << 2) - 1)

typedef struct _AuEventHandlerRec
{
    struct _AuServer *aud;			/* server */
    AuMask          mask;			/* what to compare for match */
    int             type;			/* AuEventHandler*Mask */
    AuID            id;				/* id of event */
    AuEventHandlerCallback callback;		/* whom to call */
    AuPointer       data;			/* data registered with id */
    struct _AuEventHandlerRec *next,
                   *prev;
}               AuEventHandlerRec;


typedef void
(*AuSyncHandlerCallback) (
#if NeedFunctionPrototypes
			  struct _AuServer *,		/* server */
			  struct _AuSyncHandlerRec *,	/* who called me */
			  AuPointer			/* private data */
#endif
			  );

typedef struct _AuSyncHandlerRec
{
    AuSyncHandlerCallback callback;		/* whom to call */
    AuPointer       data;			/* data registered with id */
    struct _AuSyncHandlerRec *next,
                   *prev;
}               AuSyncHandlerRec;

#define AuEventEnqueuedByAny		0
#define AuEventEnqueuedByUnknown	1
#define AuEventEnqueuedByReply		2

typedef void
(*AuEventEnqHandlerCallback)
(
#if NeedFunctionPrototypes
    struct _AuServer *,			/* server */
    struct _AuEventEnqHandlerRec *,	/* who called me */
    union _AuEvent *,			/* event */
    AuPointer				/* private data */
#endif
);

typedef struct _AuEventEnqHandlerRec
{
    AuEventEnqHandlerCallback callback;		/* whom to call */
    int             who;			/* who enqueued the event */
    AuPointer       data;			/* data registered with id */
    struct _AuEventEnqHandlerRec *next,
                   *prev;
}               AuEventEnqHandlerRec;

/*
 * Extensions need a way to hang private data on some structures.
 */
typedef struct _AuExtData
{
    int             number;			/* number returned by
						 * XRegisterExtension */
    struct _AuExtData *next;			/* next item on list of data
						 * for structure */
    int             (*free_private) ();		/* called to free private
						 * storage */
    AuPointer       private_data;		/* data private to this
						 * extension. */
}               AuExtData;

typedef struct _AuExtCodes			/* public to extension,
						 * cannot be changed */
{
    int             extension;			/* extension number */
    int             major_opcode;		/* major op-code assigned by
						 * server */
    int             first_event;		/* first event number for the
						 * extension */
    int             first_error;		/* first error number for the
						 * extension */
}               AuExtCodes;


typedef struct _AuScratchFlow
{
    AuFlowID        flow;
    AuBool          inuse;
}               AuScratchFlow;


/*
 * AudioServer - this contains all of the information needed to send data to
 * and receive data from the audio server.  Applications should not deref
 * this structure; they should use the macros instead.
 */
typedef struct _AuServer
{
    AuPointer      *client_data;		/* client can stick stuff
						 * here */
    AuExtData      *ext_data;			/* hook for extension to hang
						 * data */
    int             fd;				/* Network socket. */
    int             lock;			/* is someone in critical
						 * section? */
    int             proto_major_version;	/* maj. version of server's
						 * protocol */
    int             proto_minor_version;	/* minor version of servers
						 * protocol */
    char           *vendor;			/* vendor of the server
						 * hardware */
    AuID            resource_base;		/* resource ID base */
    AuID            resource_mask;		/* resource ID mask bits */
    AuID            resource_id;		/* allocator current ID */
    int             resource_shift;		/* allocator shift to correct
						 * bits */
                    AuID(*resource_alloc) ();	/* allocator function */
    int             vnumber;			/* audiolib's protocol
						 * version number. */
    int             release;			/* release of the server */
    struct _AuSQEvent *head,
                   *tail;			/* Input event queue. */
    struct _AuSQEvent *qfree;
    int             qlen;			/* Length of input event
						 * queue */
    AuUint32   last_request_read;		/* seq number of last event
						 * read */
    AuUint32   request;			/* sequence number of last
						 * request. */
    char           *last_req;			/* beginning of last request,
						 * or dummy */
    char           *buffer;			/* Output buffer starting
						 * address. */
    char           *bufptr;			/* Output buffer index
						 * pointer. */
    char           *bufmax;			/* Output buffer maximum+1
						 * address. */
    unsigned        max_request_size;		/* maximum number 32 bit
						 * words in request */
    struct _AuSyncHandlerRec *synchandler;	/* Synchronization handler */
    char           *server_name;		/* "host:port" string used on
						 * this connect */
    char           *scratch_buffer;		/* place to hang scratch
						 * buffer */
    AuUint32   scratch_length;		/* length of scratch buffer */
    int             ext_number;			/* extension number on this
						 * server */
    struct _AuExten *ext_procs;			/* extensions initialized on
						 * this aud */
    /*
     * the following can be fixed size, as the protocol defines how much
     * address space is available. While this could be done using the
     * extension vector, there may be MANY events processed, so a search
     * through the extension list to find the right procedure for each event
     * might be expensive if many extensions are being used.
     */
                    AuBool(*event_vec[128]) ();	/* vector for wire to event */
                    AuStatus(*wire_vec[128]) ();/* vector for event to wire */
    struct _AuInternalAsync *async_handlers;	/* for internal async */
    AuUint32   flags;			/* internal connection flags */
                    AuBool(**error_vec) ();	/* vector for wire to error */
    int             conn_checker;		/* ugly thing used by
						 * _AuEventsQueued */
    AuEventHandlerRec *eventhandlerq;
    AuEventEnqHandlerRec *eventenqhandlerq;
    struct
    {
	AuErrorHandler  error_handler;
	AuIOErrorHandler ioerror_handler;
    }               funcs;
    struct
    {
	int             min_sample_rate;
	int             max_sample_rate;
	int             max_tracks;
	int             num_formats;
	int             num_element_types;
	int             num_wave_forms;
	int             num_actions;
	int             num_devices;
	int             num_buckets;
#ifdef NOTYET
	int             num_radios;
#endif						/* NOTYET */
	int            *formats;
	int            *element_types;
	int            *wave_forms;
	int            *actions;
	struct _AuDeviceAttributes *devices;
	struct _AuBucketAttributes *buckets;
#ifdef NOTYET
	AuRadioAttributes *radios;
#endif						/* NOTYET */
    }               connsetup;
    struct
    {						/* tmp flows to keep around */
	int             total,
	                num_inuse;
	AuScratchFlow   flows[AU_MAX_SCRATCH_FLOWS];
    }               scratch_flows;
}               AuServer;

/* USE THESE MACROS INSTEAD OF DEREFERENCING AUSERVER STRUCTURE! */
#define AuServerConnectionNumber(aud)		((aud)->fd)
#define AuServerQLength(aud) 			((aud)->qlen)
#define AuServerVendor(aud) 			((aud)->vendor)
#define AuServerProtocolMajorVersion(aud)	((aud)->proto_major_version)
#define AuServerProtocolMinorVersion(aud) 	((aud)->proto_minor_version)
#define AuServerVendorRelease(aud) 		((aud)->release)
#define AuServerString(aud) 			((aud)->server_name)
#define AuServerNextRequest(aud)		((aud)->request + 1)
#define AuServerLastKnownRequestProcessed(aud)	((aud)->last_request_read)
#define AuServerResourceBase(aud)		((aud)->resource_base)
#define AuServerResourceMask(aud)		((aud)->resource_mask)
#define AuClientOfID(aud,xxx) \
    ((xxx) & ~(AuServerResourceMask(aud) | 0xe0000000))

#define AuServerMinSampleRate(aud)	((aud)->connsetup.min_sample_rate)
#define AuServerMaxSampleRate(aud)	((aud)->connsetup.max_sample_rate)
#define AuServerMaxTracks(aud)		((aud)->connsetup.max_tracks)
#define AuServerNumFormats(aud)		((aud)->connsetup.num_formats)
#define AuServerNumElementTypes(aud)	((aud)->connsetup.num_element_types)
#define AuServerNumWaveForms(aud)	((aud)->connsetup.num_wave_forms)
#define AuServerNumActions(aud)		((aud)->connsetup.num_actions)
#define AuServerNumDevices(aud)		((aud)->connsetup.num_devices)
#define AuServerNumBuckets(aud)		((aud)->connsetup.num_buckets)
#ifdef NOTYET
#define AuServerNumRadios(aud)		((aud)->connsetup.num_radios)
#endif						/* NOTYET */
#define AuServerFormat(aud, num)		((aud)->connsetup.formats[(num)])
#define AuServerElementType(aud, num)	((aud)->connsetup.element_types[(num)])
#define AuServerWaveForm(aud, num)	((aud)->connsetup.wave_forms[(num)])
#define AuServerAction(aud, num)	((aud)->connsetup.actions[(num)])
#define AuServerDevice(aud, num)      (&((aud)->connsetup.devices[(num)]))
#define AuServerBucket(aud, num)      (&((aud)->connsetup.buckets[(num)]))
#ifdef NOTYET
#define AuServerRadio(aud, num)	      (&((aud)->connsetup.radios[(num)]))
#endif						/* NOTYET */

#define AuAllocID(aud) 			((*(aud)->resource_alloc)((aud)))

/*
 * Attributes used for Devices, Buckets, and Radios
 */
typedef struct _AuCommonPart
{
    AuMask          value_mask;			/* what is present */
    AuMask          changable_mask;		/* what can be changed */
    AuID            id;				/* used to reference in flows */
    unsigned int    kind;			/* what type of object is it */
    AuMask          use;			/* importable, exportable,
						 * etc. */
    int             format;			/* current value */
    int             num_tracks;			/* 1 for mono, 2 for stereo,
						 * etc. */
    AuMask          access;			/* who can access */
    AuString        description;		/* description of device */
}               AuCommonPart;

#define AuCommonValueMask(c)		((c)->value_mask)
#define AuCommonChangableMask(c) 	((c)->changable_mask)
#define AuCommonIdentifier(c)		((c)->id)
#define AuCommonKind(c)			((c)->kind)
#define AuCommonUse(c)			((c)->use)
#define AuCommonFormat(c)		((c)->format)
#define AuCommonNumTracks(c)		((c)->num_tracks)
#define AuCommonAccess(c)		((c)->access)
#define AuCommonDescription(c) 		(&((c)->description))

/*
 * Device attributes - these are physical devices attached to the server.
 */
typedef struct _AuDevicePart
{
    unsigned int    min_sample_rate;
    unsigned int    max_sample_rate;
    AuMask          location;			/* mask of bits */
    AuFixedPoint    gain;			/* built into the hardware */
    AuInt32            line_mode;			/* high vs. low amp circuit */
    int             num_children;		/* number of subdevices, if
						 * any */
    AuDeviceID     *children;			/* subdevices */
}               AuDevicePart;
typedef struct _AuDeviceAttributes
{
    AuCommonPart    common;
    AuDevicePart    device;
}               AuDeviceAttributes;

#define AuDeviceValueMask(d)		AuCommonValueMask(&(d)->common)
#define AuDeviceChangableMask(d)	AuCommonChangableMask(&(d)->common)
#define AuDeviceIdentifier(d)		AuCommonIdentifier(&(d)->common)
#define AuDeviceKind(d)			AuCommonKind(&(d)->common)
#define AuDeviceUse(d)			AuCommonUse(&(d)->common)
#define AuDeviceFormat(d)		AuCommonFormat(&(d)->common)
#define AuDeviceNumTracks(d)		AuCommonNumTracks(&(d)->common)
#define AuDeviceAccess(d)		AuCommonAccess(&(d)->common)
#define AuDeviceDescription(d)		AuCommonDescription(&(d)->common)
#define AuDeviceMinSampleRate(d)	((d)->device.min_sample_rate)
#define AuDeviceMaxSampleRate(d)	((d)->device.max_sample_rate)
#define AuDeviceLocation(d)		((d)->device.location)
#define AuDeviceGain(d)			((d)->device.gain)
#define AuDeviceLineMode(d)		((d)->device.line_mode)
#define AuDeviceNumChildren(d)		((d)->device.num_children)
#define AuDeviceChildren(d)		((d)->device.children)

/*
 * Bucket attributes - these are server storage for sound data.
 */
typedef struct _AuBucketPart
{
    unsigned int    sample_rate;		/* current value */
    AuUint32   num_samples;		/* max number in this device */
}               AuBucketPart;
typedef struct _AuBucketAttributes
{
    AuCommonPart    common;
    AuBucketPart    bucket;
}               AuBucketAttributes;

#define AuBucketValueMask(b)		AuCommonValueMask(&(b)->common)
#define AuBucketChangableMask(b)	AuCommonChangableMask(&(b)->common)
#define AuBucketIdentifier(b)		AuCommonIdentifier(&(b)->common)
#define AuBucketKind(b)			AuCommonKind(&(b)->common)
#define AuBucketUse(b)			AuCommonUse(&(b)->common)
#define AuBucketFormat(b)		AuCommonFormat(&(b)->common)
#define AuBucketNumTracks(b)		AuCommonNumTracks(&(b)->common)
#define AuBucketAccess(b)		AuCommonAccess(&(b)->common)
#define AuBucketDescription(b)		AuCommonDescription(&(b)->common)
#define AuBucketSampleRate(b)		((b)->bucket.sample_rate)
#define AuBucketNumSamples(b)		((b)->bucket.num_samples)

#ifdef NOTYET
/*
 * Radio attributes - these are special devices for LAN broadcast audio data
 */
typedef struct _AuRadioPart
{
    int             station;			/* which one you are
						 * listening to */
}               AuRadioPart;
typedef struct _AuRadioAttributes
{
    AuCommonPart    common;
    AuRadioPart     radio;
}               AuRadioAttributes;

#define AuRadioValueMask(r)		AuCommonValueMask(&(r)->common)
#define AuRadioChangableMask(r)		AuCommonChangableMask(&(r)->common)
#define AuRadioIdentifier(r)		AuCommonIdentifier(&(r)->common)
#define AuRadioKind(r)			AuCommonKind(&(r)->common)
#define AuRadioUse(r)			AuCommonUse(&(r)->common)
#define AuRadioFormat(r)		AuCommonFormat(&(r)->common)
#define AuRadioNumTracks(r)		AuCommonNumTracks(&(r)->common)
#define AuRadioAccess(r)		AuCommonAccess(&(r)->common)
#define AuRadioDescription(r)		AuCommonDescription(&(r)->common)
#define AuRadioStation(r)		((r)->radio.station)
#endif						/* NOTYET */




/*
 * Flows - These are the instructions (called Elements) for how to move audio
 * data from one place to another.
 */
typedef struct _AuFlowAttributes
{
    unsigned char   state;			/* Stop, Start, Pause */
    unsigned char   pad1;
    unsigned short  pad2;
    AuBool          clocked;			/* instantantious or not */
}               AuFlowAttributes;


typedef struct _AuElementAction
{
    AuFlowID        flow;			/* target flow to affect */
    unsigned char   element_num;		/* target element to affect */
    unsigned char   trigger_state;		/* state to trigger action */
    unsigned char   trigger_prev_state;		/* previous state to trigger */
    unsigned char   trigger_reason;		/* reason to trigger action */
    unsigned char   action;			/* what to do */
    unsigned char   new_state;			/* new state for target */
    unsigned short  pad;
}               AuElementAction;

#define AuMakeChangeStateAction(A, _trigger_state, _trigger_prev_state, _trigger_reason, _flow, _element_num, _new_state)\
AU_MACRO_PREFIX								       \
    (A)->trigger_state = (_trigger_state);				       \
    (A)->trigger_prev_state = (_trigger_prev_state);			       \
    (A)->trigger_reason = (_trigger_reason);				       \
    (A)->action = AuElementActionChangeState;				       \
    (A)->flow = (_flow);						       \
    (A)->element_num = (_element_num);					       \
    (A)->new_state = (_new_state);					       \
AU_MACRO_SUFFIX

#define AuMakeSendNotifyAction(A, _trigger_state, _trigger_prev_state, _trigger_reason)\
AU_MACRO_PREFIX								       \
    (A)->trigger_state = (_trigger_state);				       \
    (A)->trigger_prev_state = (_trigger_prev_state);			       \
    (A)->trigger_reason = (_trigger_reason);				       \
    (A)->action = AuElementActionSendNotify;				       \
AU_MACRO_SUFFIX

#define AuMakeNoopAction(A, _trigger_state, _trigger_prev_state, _trigger_reason)\
AU_MACRO_PREFIX								       \
    (A)->trigger_state = (_trigger_state);				       \
    (A)->trigger_prev_state = (_trigger_prev_state);			       \
    (A)->trigger_reason = (_trigger_reason);				       \
    (A)->action = AuElementActionNoop;					       \
AU_MACRO_SUFFIX


typedef struct _AuElementActionList
{
    int             num_actions;
    AuElementAction *actions;
}               AuElementActionList;

/* IMPORTS */

typedef struct _AuElementImportClient
{
    unsigned short  type;			/* AuElementTypeImportClient */
    unsigned short  sample_rate;
    unsigned char   format;
    unsigned char   num_tracks;
    unsigned char   discard;
    unsigned char   pad;
    AuUint32   max_samples;
    AuUint32   low_water_mark;
    AuElementActionList actions;
}               AuElementImportClient;

#define AuMakeElementImportClient(E, _sample_rate, _format, _num_tracks, _discard, _max_samples, _low_water_mark, _num_actions, _actions)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeImportClient;				       \
    (E)->importclient.sample_rate = (_sample_rate);			       \
    (E)->importclient.format = (_format);				       \
    (E)->importclient.num_tracks = (_num_tracks);			       \
    (E)->importclient.discard = (_discard);				       \
    (E)->importclient.max_samples = (_max_samples);			       \
    (E)->importclient.low_water_mark = (_low_water_mark);		       \
    (E)->importclient.actions.num_actions = (_num_actions);		       \
    (E)->importclient.actions.actions = (_actions);			       \
AU_MACRO_SUFFIX


typedef struct _AuElementImportDevice
{
    unsigned short  type;			/* AuElementTypeImportDevice */
    unsigned short  sample_rate;
    AuDeviceID      device;
    AuUint32   num_samples;
    AuElementActionList actions;
}               AuElementImportDevice;

#define AuMakeElementImportDevice(E, _sample_rate, _device, _num_samples, _num_actions, _actions)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeImportDevice;				       \
    (E)->importdevice.sample_rate = (_sample_rate);			       \
    (E)->importdevice.device = (_device);				       \
    (E)->importdevice.num_samples = (_num_samples);			       \
    (E)->importdevice.actions.num_actions = (_num_actions);		       \
    (E)->importdevice.actions.actions = (_actions);			       \
AU_MACRO_SUFFIX

typedef struct _AuElementImportBucket
{
    unsigned short  type;			/* AuElementTypeImportBucket */
    unsigned short  sample_rate;
    AuBucketID      bucket;
    AuUint32   num_samples;
    AuInt32         parms[AuParmsImportBucket];	/* offset */
    AuElementActionList actions;
}               AuElementImportBucket;

#define AuMakeElementImportBucket(E, _sample_rate, _bucket, _num_samples, _offset, _num_actions, _actions)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeImportBucket;				       \
    (E)->importbucket.sample_rate = (_sample_rate);			       \
    (E)->importbucket.bucket = (_bucket);				       \
    (E)->importbucket.num_samples = (_num_samples);			       \
    (E)->importbucket.parms[AuParmsImportBucketOffset] = (_offset);	       \
    (E)->importbucket.actions.num_actions = (_num_actions);		       \
    (E)->importbucket.actions.actions = (_actions);			       \
AU_MACRO_SUFFIX


typedef struct _AuElementImportWaveForm
{
    unsigned short  type;		       /* AuElementTypeImportWaveForm */
    unsigned short  sample_rate;
    int             wave_form;
    AuInt32         parms[AuParmsImportWaveForm];	/* freq, num samples */
    AuElementActionList actions;
}               AuElementImportWaveForm;

#define AuMakeElementImportWaveForm(E, _sample_rate, _wave_form, _num_samples, _freq, _num_actions, _actions)\
AU_MACRO_PREFIX								      \
    (E)->type = AuElementTypeImportWaveForm;				      \
    (E)->importwaveform.sample_rate = (_sample_rate);			      \
    (E)->importwaveform.wave_form = (_wave_form);			      \
    (E)->importwaveform.parms[AuParmsImportWaveFormFrequency] = (_freq);      \
    (E)->importwaveform.parms[AuParmsImportWaveFormNumSamples] =	      \
        (_num_samples);							      \
    (E)->importwaveform.actions.num_actions = (_num_actions);		      \
    (E)->importwaveform.actions.actions = (_actions);			      \
AU_MACRO_SUFFIX


#ifdef NOTYET
typedef struct _AuElementImportRadio
{
    unsigned short  type;			/* AuElementTypeImportRadio */
    unsigned short  sample_rate;
    AuRadioID       radio;
    AuUint32   num_samples;
    AuElementActionList actions;
}               AuElementImportRadio;

#define AuMakeElementImportRadio(E,asample_rate,aradio,anum_samples,aactions)  \
{									       \
    (E)->type = AuElementTypeImportDevice;				       \
    (E)->sample_rate = (asample_rate);					       \
    (E)->radio = (aradio);						       \
    (E)->num_samples = (anum_samples);					       \
    (E)->actions = (aactions);						       \
}
#endif /* NOTYET */


/* OPS */

typedef struct _AuInputTrack
{
    unsigned char   element_num;
    unsigned char   track;
}               AuInputTrack;

#define AuMakeInputTrack(T, _element_num, _track)			       \
AU_MACRO_PREFIX								       \
    (T)->element_num = (_element_num);					       \
    (T)->track = (_track);						       \
AU_MACRO_SUFFIX								       \

typedef struct _AuElementBundle
{
    unsigned short  type;			/* AuElementTypeBundle */
    unsigned short  num_inputs;
    AuInputTrack   *inputs;
}               AuElementBundle;

#define AuMakeElementBundle(E, _num_inputs, _inputs)			       \
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeBundle;					       \
    (E)->bundle.num_inputs = (_num_inputs);				       \
    (E)->bundle.inputs = (_inputs);					       \
AU_MACRO_SUFFIX


typedef struct _AuElementMultiplyConstant
{
    unsigned short  type;			/* AuElementTypeMultiplyConsta
						 * nt */
    unsigned short  input;
    AuInt32            parms[AuParmsMultiplyConstant];	/* constant */
}               AuElementMultiplyConstant;

#define AuMakeElementMultiplyConstant(E, _input, _constant)		       \
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeMultiplyConstant;				       \
    (E)->multiplyconstant.input = (_input);				       \
    (E)->multiplyconstant.parms[AuParmsMultiplyConstantConstant] = (_constant);\
AU_MACRO_SUFFIX


typedef struct _AuElementAddConstant
{
    unsigned short  type;			/* AuElementTypeAddConstant */
    unsigned short  input;
    AuInt32            parms[AuParmsAddConstant];
}               AuElementAddConstant;

#define AuMakeElementAddConstant(E, _input, _constant)			       \
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeAddConstant;				       \
    (E)->addconstant.input = (_input);					       \
    (E)->addconstant.parms[AuParmsAddConstantConstant] = (_constant);	       \
AU_MACRO_SUFFIX


typedef struct _AuElementSum
{
    unsigned short  type;			/* AuElementTypeSum */
    unsigned short  num_inputs;
    unsigned short  *inputs;
}               AuElementSum;

#define AuMakeElementSum(E, _num_inputs, _inputs)			       \
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeSum;					       \
    (E)->sum.num_inputs = (_num_inputs);				       \
    (E)->sum.inputs = (_inputs);					       \
AU_MACRO_SUFFIX



/* EXPORTS */

typedef struct _AuElementExportClient
{
    unsigned short  type;			/* AuElementTypeExportClient */
    unsigned short  sample_rate;
    unsigned short  input;
    unsigned char   format;
    unsigned char   num_tracks;
    unsigned char   discard;
    unsigned char   pad;
    AuUint32   max_samples;
    AuUint32   high_water_mark;
    AuElementActionList actions;
}               AuElementExportClient;

#define AuMakeElementExportClient(E, _input, _sample_rate, _format, _num_tracks, _discard, _max_samples, _high_water_mark, _num_actions, _actions)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeExportClient;				       \
    (E)->exportclient.input = (_input);					       \
    (E)->exportclient.sample_rate = (_sample_rate);			       \
    (E)->exportclient.format = (_format);				       \
    (E)->exportclient.num_tracks = (_num_tracks);			       \
    (E)->exportclient.discard = (_discard);				       \
    (E)->exportclient.max_samples = (_max_samples);			       \
    (E)->exportclient.high_water_mark = (_high_water_mark);		       \
    (E)->exportclient.actions.num_actions = (_num_actions);		       \
    (E)->exportclient.actions.actions = (_actions);			       \
AU_MACRO_SUFFIX

typedef struct _AuElementExportDevice
{
    unsigned short  type;			/* AuElementTypeExportDevice */
    unsigned short  sample_rate;
    unsigned short  input;
    AuDeviceID      device;
    AuUint32   num_samples;
    AuElementActionList actions;
}               AuElementExportDevice;

#define AuMakeElementExportDevice(E, _input, _device, _sample_rate, _num_samples, _num_actions, _actions)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeExportDevice;				       \
    (E)->exportdevice.sample_rate = (_sample_rate);			       \
    (E)->exportdevice.input = (_input);					       \
    (E)->exportdevice.device = (_device);				       \
    (E)->exportdevice.num_samples = (_num_samples);			       \
    (E)->exportdevice.actions.num_actions = (_num_actions);		       \
    (E)->exportdevice.actions.actions = (_actions);			       \
AU_MACRO_SUFFIX

typedef struct _AuElementExportBucket
{
    unsigned short  type;			/* AuElementTypeExportBucket */
    unsigned short  input;
    AuBucketID      bucket;
    AuUint32   num_samples;
    AuInt32            parms[AuParmsExportBucket];	/* offset */
    AuElementActionList actions;
}               AuElementExportBucket;

#define AuMakeElementExportBucket(E, _input, _bucket, _num_samples, _offset, _num_actions, _actions)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeExportBucket;				       \
    (E)->exportbucket.input = (_input);					       \
    (E)->exportbucket.bucket = (_bucket);				       \
    (E)->exportbucket.num_samples = (_num_samples);			       \
    (E)->exportbucket.parms[AuParmsExportBucketOffset] = (_offset);	       \
    (E)->exportbucket.actions.num_actions = (_num_actions);		       \
    (E)->exportbucket.actions.actions = (_actions);			       \
AU_MACRO_SUFFIX

typedef struct _AuElementExportRadio
{
    unsigned short  type;			/* AuElementTypeExportRadio */
    unsigned short  sample_rate;
    AuRadioID       radio;
    AuUint32   num_samples;
    AuElementActionList actions;
}               AuElementExportRadio;

#define AuMakeElementExportRadio(E,asample_rate,aradio,anum_samples,aactions)  \
{									       \
    (E)->type = AuElementTypeExportDevice;				       \
    (E)->sample_rate = (asample_rate);					       \
    (E)->radio = (aradio);						       \
    (E)->num_samples = (anum_samples);					       \
    (E)->actions = (aactions);						       \
}

typedef struct _AuElementExportMonitor
{
    unsigned short  type;			/* AuElementTypeExportMonitor */
    unsigned short  event_rate;
    unsigned short  input;
    unsigned short  pad;
    unsigned char   format;
    unsigned char   num_tracks;
    unsigned short  pad1;
}               AuElementExportMonitor;

#define AuMakeElementExportMonitor(E, _input, _event_rate, _format, _num_tracks)\
AU_MACRO_PREFIX								       \
    (E)->type = AuElementTypeExportMonitor;				       \
    (E)->exportmonitor.input = (_input);				       \
    (E)->exportmonitor.event_rate = (_event_rate);			       \
    (E)->exportmonitor.format = (_format);				       \
    (E)->exportmonitor.num_tracks = (_num_tracks);			       \
AU_MACRO_SUFFIX

/* UNION OF ELEMENTS */

typedef union _AuElement			/* generic element */
{
    unsigned short  type;
    AuElementImportClient importclient;
    AuElementImportDevice importdevice;
    AuElementImportBucket importbucket;
    AuElementImportWaveForm importwaveform;
#ifdef NOTYET
    AuElementImportRadio importradio;
#endif /* NOTYET */
    AuElementBundle bundle;
    AuElementMultiplyConstant multiplyconstant;
    AuElementAddConstant addconstant;
    AuElementSum    sum;
    AuElementExportClient exportclient;
    AuElementExportDevice exportdevice;
    AuElementExportBucket exportbucket;
#ifdef NOTYET
    AuElementExportRadio exportradio;
#endif /* NOTYET */
    AuElementExportMonitor exportmonitor;
}               AuElement;

typedef struct _AuElementParameters
{
    AuFlowID        flow;
    unsigned char   element_num;
    unsigned char   num_parameters;
    unsigned short  pad;
    AuInt32            parameters[AU_MAX_PARMS];
}               AuElementParameters;

typedef struct _AuElementState
{
    AuFlowID        flow;
    unsigned char   element_num;
    unsigned char   state;
    unsigned short  pad;
}               AuElementState;

#define AuMakeElementState(S, _flow, _element_num, _state)		       \
{									       \
    (S)->flow = (_flow);						       \
    (S)->element_num = (_element_num);					       \
    (S)->state = (_state);						       \
}

/*****************************************************************************
 *				    EVENTS				     *
 *****************************************************************************/

typedef struct _AuAnyEvent			/* common to all events */
{
    int             type;
    AuUint32   serial;
    AuBool          send_event;
    AuServer       *server;
    AuTime          time;
    AuID            id;				/* primary id */
}               AuAnyEvent;


typedef struct _AuElementNotifyEvent
{
    int             type;			/* AuEventTypeElementNotify */
    AuUint32   serial;
    AuBool          send_event;
    AuServer       *server;
    AuTime          time;
    AuFlowID        flow;
    unsigned char   element_num;
    unsigned char   kind;			/* AuElementNotify* */
    unsigned char   prev_state;			/* of element */
    unsigned char   cur_state;			/* of element */
    unsigned char   reason;			/* for event */
    AuUint32   num_bytes;			/* unread bytes, etc. */
}               AuElementNotifyEvent;


typedef struct _AuMonitorNotifyEvent
{
    int             type;			/* AuEventTypeElementNotify */
    AuUint32   serial;
    AuBool          send_event;
    AuServer       *server;
    AuTime          time;
    AuFlowID        flow;
    unsigned char   element_num;
    unsigned char   format;
    unsigned char   num_tracks;
    unsigned short  count;
    unsigned short  num_fields;
    AuUint32   data;
    AuUint32   data1;
    AuUint32   data2;
}               AuMonitorNotifyEvent;

typedef struct _AuGrabNotifyEvent
{
    int             type;			/* AuEventTypeElementNotify */
    AuUint32   serial;
    AuBool          send_event;
    AuServer       *server;
    AuTime          time;
    AuID            id;
    unsigned char   kind;			/* AuGrabNotifyKind* */
    unsigned char   pad1;
    unsigned short  pad2;
    AuID            client;			/* requestor, prev owner */
}               AuGrabNotifyEvent;



typedef struct _AuErrorEvent
{
    int             type;
    AuUint32   serial;			/* of failed request */
    AuBool          send_event;
    AuServer       *server;
    AuTime          time;
    AuID            resourceid;
    unsigned char   error_code;			/* of failed request */
    unsigned char   request_code;		/* Major op-code */
    unsigned char   minor_code;			/* Minor op-code */
    union
    {
	AuUint32   l[4];
    }               data;			/* per-error type data */
}               AuErrorEvent;


typedef union _AuEvent
{
    int             type;
    AuAnyEvent      auany;
    AuElementNotifyEvent auelementnotify;
    AuMonitorNotifyEvent aumonitornotify;
    AuGrabNotifyEvent augrabnotify;
    AuErrorEvent    auerror;
}               AuEvent;




/*****************************************************************************
 *		      APPLICATION PROGRAMMING INTERFACES		     *
 *****************************************************************************/


_AUFUNCPROTOBEGIN


/*
 * AuServerName - this routine returns the that the library will use to
 * locate the audio server.
 */
extern _AuConst char *
AuServerName(
#if NeedFunctionPrototypes
	     _AuConst char *			/* servername */
#endif
);

/*
 * AuOpenServer - this routine is used to open a connection to an audio
 * server.
 */
extern AuServer *
AuOpenServer(
#if NeedFunctionPrototypes
	     _AuConst char *,			/* servername */
	     int,				/* num_authproto */
	     _AuConst char *,			/* authproto */
	     int,				/* num_authdata */
	     _AuConst char *,			/* authdata */
	     char **				/* server_message */
#endif
);


/*
 * AuCloseServer - this routine is used to close a connection to an audio
 * server when it is no AuInt32er needed.
 */
extern void
AuCloseServer(
#if NeedFunctionPrototypes
	      AuServer *			/* server */
#endif
);


/*
 * AuSetErrorHandler - this routine is used to set the procedure that should
 * be called when this connection sees an error that is not being intercepted
 * by the library.
 */
extern          AuErrorHandler
AuSetErrorHandler(
#if NeedFunctionPrototypes
		  AuServer *,			/* server */
		  AuErrorHandler		/* handler */
#endif
);


/*
 * AuSetIOErrorHandler - this routine is used to set the procedure that
 * should be called when the connection to the server has unexpectedly been
 * broken.
 */
extern          AuIOErrorHandler
AuSetIOErrorHandler(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    AuIOErrorHandler		/* handler */
#endif
);


/*
 * AuFlush - this routine flushes any queued packets down to the server.
 */
extern void
AuFlush(
#if NeedFunctionPrototypes
	AuServer *				/* server */
#endif
);


/*
 * AuSync - this routine forces a round trip to the server
 */
extern void
AuSync(
#if NeedFunctionPrototypes
       AuServer *,				/* server */
       AuBool					/* discard_queued_events */
#endif
);


/*
 * AuSynchronize - this routine instructs the library to operate
 * synchronously, which may be easier for some applications to deal with.
 */
extern          AuBool
AuSynchronize(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuBool				/* enablesync */
#endif
);


/*
 * AuGetErrorText - this routine is used to translate error codes into
 * English strings for printing.
 */
extern void
AuGetErrorText(
#if NeedFunctionPrototypes
	       AuServer *,			/* server */
	       int,				/* code */
	       char *,				/* buffer_return */
	       int				/* length */
#endif
);


/*
 * AuGetErrorDatabaseText - this routine is used to lookup strings for error
 * messages.
 */
extern void
AuGetErrorDatabaseText(
#if NeedFunctionPrototypes
		       AuServer *,		/* server */
		       _AuConst char *,		/* name */
		       _AuConst char *,		/* message */
		       _AuConst char *,		/* default_string */
		       char *,			/* buffer_return */
		       int			/* length */
#endif
);



/*
 * AuSetCloseDownMode - this routine instructs the server to save any objects
 * created by this client even if it shuts down.
 */
extern void
AuSetCloseDownMode(
#if NeedFunctionPrototypes
		   AuServer *,			/* server */
		   int,				/* mode */
		   AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuGetCloseDownMode - this routine returns the current close-down mode for
 * this client.
 */
extern int
AuGetCloseDownMode(
#if NeedFunctionPrototypes
		   AuServer *,			/* server */
		   AuStatus *			/* RETURN_status */
#endif
);

/*
 * AuGetServerTime - this routine returns the current server time
 */
extern AuTime
AuGetServerTime(
#if NeedFunctionPrototypes
		   AuServer *,			/* server */
		   AuStatus *			/* RETURN_status */
#endif
);

/*
 * AuKillClient - this routine forces the client that created the specified
 * resource to close down, regardless of its close down mode.
 */
extern void
AuKillClient(
#if NeedFunctionPrototypes
	     AuServer *,			/* server */
	     AuID,				/* resource */
	     AuStatus *				/* RETURN_status */
#endif
);



/*
 * AuSetDeviceAttributes - this routine is used to change device attributes
 * for the fields in changable_mask returned by AuGetDeviceAttributes or the
 * setup information.
 */
extern void
AuSetDeviceAttributes(
#if NeedFunctionPrototypes
		      AuServer *,		/* server */
		      AuDeviceID,		/* resource */
		      AuMask,			/* value_mask */
		      AuDeviceAttributes *,	/* attr */
		      AuStatus *		/* RETURN_status */
#endif
);


/*
 * AuGetDeviceAttributes - this routine is used to get device attributes
 * (which are also provided by the server at connection setup).
 */
extern AuDeviceAttributes *
AuGetDeviceAttributes(
#if NeedFunctionPrototypes
		      AuServer *,		/* server */
		      AuDeviceID,		/* resource */
		      AuStatus *		/* RETURN_status */
#endif
);


/*
 * AuListDevices - this routine is used to query the devices
 */
extern AuDeviceAttributes *
AuListDevices(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuMask,				/* value_mask */
	      AuDeviceAttributes *,		/* attr */
	      int *,				/* RETURN_ndevices */
	      AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuFreeDeviceAttributes - this routine is used to free storage returned by
 * calls to AuGetDeviceAttributes.
 */
extern void
AuFreeDeviceAttributes(
#if NeedFunctionPrototypes
		       AuServer *,		/* server */
		       int,			/* num_attr */
		       AuDeviceAttributes *	/* attr */
#endif
);


/*
 * AuSetBucketAttributes - this routine is used to change bucket attributes
 * for the fields in changable_mask returned by AuGetBucketAttributes or the
 * setup information.
 */
extern void
AuSetBucketAttributes(
#if NeedFunctionPrototypes
		      AuServer *,		/* server */
		      AuBucketID,		/* resource */
		      AuMask,			/* value_mask */
		      AuBucketAttributes *,	/* attr */
		      AuStatus *		/* RETURN_status */
#endif
);


/*
 * AuGetBucketAttributes - this routine is used to get bucket attributes
 * (which are also provided by the server at connection setup).
 */
extern AuBucketAttributes *
AuGetBucketAttributes(
#if NeedFunctionPrototypes
		      AuServer *,		/* server */
		      AuBucketID,		/* resource */
		      AuStatus *		/* RETURN_status */
#endif
);


/*
 * AuListBuckets - this routine is used to query the buffers for a given
 * client, all retained clients, or all known buffers.
 */
extern AuBucketAttributes *
AuListBuckets(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuMask,				/* value_mask */
	      AuBucketAttributes *,		/* attr */
	      int *,				/* RETURN_nbuckets */
	      AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuFreeBucketAttributes - this routine is used to free storage returned by
 * calls to AuGetBucketAttributes.
 */
extern void
AuFreeBucketAttributes(
#if NeedFunctionPrototypes
		       AuServer *,		/* server */
		       int,			/* num_attr */
		       AuBucketAttributes *	/* attr */
#endif
);

/*
 * AuCreateBucket - this routine is used to create new buckets (buffers,
 * radios, wave form generators)
 */
extern          AuBucketID
AuCreateBucket(
#if NeedFunctionPrototypes
	       AuServer *,			/* server */
	       AuUint32,			/* format */
	       AuUint32,			/* num tracks */
	       AuUint32,			/* access */
	       AuUint32,			/* sample_rate */
	       AuUint32,			/* num_samples */
	       AuString *,			/* description */
	       AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuDestroyBucket - this routine is used to destroy a bucket once it is no
 * AuInt32er being used.
 */
extern void
AuDestroyBucket(
#if NeedFunctionPrototypes
		AuServer *,			/* server */
		AuBucketID,			/* bucket */
		AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuCreateFlow - this routine is used to create a Flow object which
 * describes how audio data gets from one or more inputs to one or more
 * outputs.
 */
extern          AuFlowID
AuCreateFlow(
#if NeedFunctionPrototypes
	     AuServer *,			/* server */
	     AuStatus *				/* RETURN_status */
#endif
);


/*
 * AuDestroyFlow - this routine is used to destroy a Flow object once it is
 * no AuInt32er needed.
 */
extern void
AuDestroyFlow(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuFlowID,				/* flow */
	      AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuGetFlowAttributes - this routine is used to find out what the state of
 * the flow is at any given time.
 */
extern          AuBool
AuGetFlowAttributes(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    int,			/* num_attrs */
		    AuFlowAttributes *,		/* attrs */
		    AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuSetElements - this routine is used to set the flow topology; it is
 * equivalent to destroying the flow and recreating it using the same id. The
 * API will keep a list of scratch flows to use in sending audio data over
 * the wire.
 */
extern void
AuSetElements(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuFlowID,				/* flow id */
	      AuBool,				/* clocked */
	      int,				/* num_elements */
	      AuElement *,			/* elements */
	      AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuGetElements - this routine is used to return the element topology that
 * was set in the AuCreateElements.
 */
extern AuElement *
AuGetElements(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuFlowID,				/* flow id */
	      AuBool *,				/* RETURN_clocked */
	      int *,				/* RETURN_num_elements */
	      AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuFreeElements - this routine is used to free the results returned from a
 * call to AuGetElements.
 */
extern void
AuFreeElements(
#if NeedFunctionPrototypes
	       AuServer *,			/* server */
	       int,				/* num_elements */
	       AuElement *			/* listofelements */
#endif
);


/*
 * AuGetElementStates - this routine is used to get the state of various
 * elements within a flow.
 */
extern AuElementState *
AuGetElementStates(
#if NeedFunctionPrototypes
		   AuServer *,			/* server */
		   int *,			/* INOUT_num_states */
		   AuElementState *,		/* states */
		   AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuFreeElementStates - this routine is used to free the results returned
 * from a call to AuGetElementStates.
 */
extern void
AuFreeElementStates(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    int,			/* num_elements */
		    AuElementState *		/* list of element states */
#endif
);


/*
 * AuSetElementStates - this routine is used to set the state of various
 * elements within a flow.
 */
extern void
AuSetElementStates(
#if NeedFunctionPrototypes
		   AuServer *,			/* server */
		   int,				/* num_states */
		   AuElementState *,		/* states */
		   AuStatus *			/* RETURN_status */
#endif
);



/*
 * AuGetElementParameters - this routine is used to get parameters of
 * elements within a flow.
 */
extern          AuBool
AuGetElementParameters(
#if NeedFunctionPrototypes
		       AuServer *,		/* server */
		       int,			/* num_changes */
		       AuElementParameters *,	/* parms */
		       AuStatus *		/* RETURN_status */
#endif
);


/*
 * AuSetElementParameters - this routine is used to change parameters of
 * elements within a flow.  Changes in element types is not permitted.
 */
extern void
AuSetElementParameters(
#if NeedFunctionPrototypes
		       AuServer *,		/* server */
		       int,			/* num_changes */
		       AuElementParameters *,	/* changes */
		       AuStatus *		/* RETURN_status */
#endif
);



/*
 * AuWriteElement - this routine is used to send audio data to an
 * ImportClient element in a flow.
 */
extern void
AuWriteElement(
#if NeedFunctionPrototypes
	       AuServer *,			/* server */
	       AuFlowID,			/* flow */
	       int,				/* element */
	       AuUint32,			/* num_bytes */
	       AuPointer,			/* data */
	       AuBool,				/* end of data flag */
	       AuStatus *			/* RETURN_status */
#endif
);


/*
 * AuReadElement - this routine is used to get audio data from an
 * ExportClient element in a flow.
 */
extern AuUint32
AuReadElement(
#if NeedFunctionPrototypes
	      AuServer *,			/* server */
	      AuFlowID,				/* flow */
	      int,				/* element */
	      AuUint32,			/* num_bytes */
	      AuPointer,			/* data */
	      int *				/* RETURN_status */
#endif
);



/*
 * AuEventsQueued - this routine is used to see if there are any events
 * waiting to be read.
 */
extern int
AuEventsQueued(
#if NeedFunctionPrototypes
	       AuServer *,			/* server */
	       int				/* AuEventsQueued_mode */
#endif
);


/*
 * AuScanEvents - this routine is used to walk the event queue looking for
 * events that match a client-supplied predicate.  If the predicate returns
 * AuTrue for an event, that event is returned to the caller.
 */
extern          AuBool
AuScanEvents(
#if NeedFunctionPrototypes
	     AuServer *,			/* server */
	     int,				/* AuEventsQueued_mode */
	     AuBool,				/* dequeue_match */
	     AuBool(*) (			/* clientpredicate */
#if NeedNestedPrototypes
			AuServer *,		/* server */
			AuEvent *,		/* eventtocheck */
			AuPointer		/* clientdata */
#endif
			),
	     AuPointer,				/* clientdata */
	     AuEvent *				/* RETURN_event */
#endif
);


/*
 * AuScanForTypedEvent - this is a convenience routine that calls
 * AuScanEvents with a predicate that looks for the specified event type.
 */
extern          AuBool
AuScanForTypedEvent(
#if NeedFunctionPrototypes
		    AuServer *,			/* server */
		    int,			/* AuEventsQueued_mode */
		    AuBool,			/* dequeue */
		    int,			/* eventtype */
		    AuEvent *			/* RETURN_event */
#endif
);


/*
 * AuNextEvent - this routine is used to read a queued event.  If there is no
 * event available, it will block until one arrives.
 */
extern void
AuNextEvent(
#if NeedFunctionPrototypes
	    AuServer *,				/* server */
	    AuBool,				/* dequeue */
	    AuEvent *				/* RETURN_event */
#endif
);




/*
 * AuRequeueEvent - this routine puts an event back on the front of the event
 * queue.  The skip argument specifies the number of events in the queue to
 * skip (0 means the head of the queue, AuServerQLength() means the end of
 * the queue).
 */
extern          AuBool
AuRequeueEvent(
#if NeedFunctionPrototypes
	       AuServer *,			/* server */
	       AuEvent *,			/* event */
	       int				/* skip */
#endif
);


/*
 * AuIDOfEvent - this routine returns the Flow, Bucket, or Device id from an
 * event so that it can be used to dispatch the event to the proper handler.
 */
extern          AuID
AuIDOfEvent(
#if NeedFunctionPrototypes
	    AuEvent *				/* RETURN_event */
#endif
);


extern void AuFree (
#if NeedFunctionPrototypes
		    AuPointer
#endif
);

/*****************************************************************************
 *				    MACROS				     *
 *****************************************************************************/

/**
 * The following macros are provided to make initialization of various
 * structures easier:
 *
 *     AuMakeElementImportClient()
 *     AuMakeElementImportDevice()
 *     AuMakeElementImportBucket()
 *     AuMakeElementImportWaveForm()
 *     AuMakeElementImportRadio()
 *     AuMakeElementBundle()
 *     AuMakeElementMultipleConstant()
 *     AuMakeElementAddConstant()
 *     AuMakeElementSum()
 *     AuMakeElementExportClient()
 *     AuMakeElementExportDevice()
 *     AuMakeElementExportBucket()
 *     AuMakeElementExportRadio()
 *
 *     AuMakeInputTrack()
 *
 */

_AUFUNCPROTOEND

#include <audio/audioutil.h>			/* get rest of prototypes */

#endif						/* _NCD_AUDIOLIB_H_ */
