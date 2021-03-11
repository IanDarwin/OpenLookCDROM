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
/* $Header: /crl/audio/AF/lib/AF/RCS/AFlib.h,v 1.58 1994/04/06 00:34:26 tml Exp $ */
/* 
 * Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology
 *
 *
 */


/*
 *	AFlib.h - Header definition and support file for the C subroutine
 *	interface library (AFlib) to the AudioFile system.
 *	Structures and symbols starting with "_" are private to the library.
 */
#ifndef _AFLIB_H_
#define _AFLIB_H_

#ifndef __cplusplus				/* DBW: added this */
#ifdef mips
#ifndef sgi
#define const
#endif
#endif
#endif						/* DBW: added this */

#if !defined(__cplusplus) && !defined(__GNUC__)
#ifdef	sparc
#define	const					/* rsp noted broken. */
#endif
#endif

#ifdef USG
#ifndef __TYPES__
#include <sys/types.h>			/* forgot to protect it... */
#define __TYPES__
#endif
#else
#include <sys/types.h>
#endif

#include <AF/audio.h>

#ifndef NeedWidePrototypes
#if defined(NARROWPROTO)
#define NeedWidePrototypes 0
#else
#define NeedWidePrototypes 1		/* default to make interropt. easier */
#endif
#endif

#ifdef __cplusplus			/* do not leave open across includes */
extern "C" {					/* for C++ V2.0 */
#endif

#define ABool int
#define AStatus int
#define ATrue 1
#define AFalse 0

#define AQueuedAlready 0
#define AQueuedAfterReading 1
#define AQueuedAfterFlush 2

#define AConnectionNumber(aud) 	((aud)->fd)
#define AQLength(aud) 		((aud)->qlen)
#define AServerVendor(aud) 	((aud)->vendor)
#define AProtocolVersion(aud) 	((aud)->proto_major_version)
#define AProtocolRevision(aud) 	((aud)->proto_minor_version)
#define AVendorRelease(aud) 	((aud)->release)
#define AudioConnString(aud) 	((aud)->audioconn_name)
#define ANextRequest(aud)	((aud)->request + 1)
#define ALastKnownRequestProcessed(aud)	((aud)->last_request_read)
#define ANumberOfAudioDevices(aud) ((aud)->ndevices)
#define AAudioDeviceDescriptor(aud, dev) (&((aud)->devices[dev]))

/*
 * Extensions need a way to hang private data on some structures.
 */
typedef struct _AFExtData {
	int number;		/* number returned by XRegisterExtension */
	struct _AFExtData *next;/* next item on list of data for structure */
	int (*free_private)(struct _AFExtData *); /* called to free private storage */
	char *private_data;	/* data private to this extension. */
} AFExtData;

/*
 * This file contains structures used by the extension mechanism.
 */
typedef struct {		/* public to extension, cannot be changed */
	int extension;		/* extension number */
	int major_opcode;	/* major op-code assigned by server */
	int first_event;	/* first event number for the extension */
	int first_error;	/* first error number for the extension */
} AFExtCodes;

/*
 * This structure is private to the library.
 */
typedef struct _AC *ACp;		/* Forward */
typedef struct _AFAudioConn *AUDp;	/* Forward */

/* Typedef to get mips cc to swallow them. */
typedef	int (*FuncPtr_freeAC) (AUDp, ACp, AFExtCodes *);
typedef	int (*FuncPtr_closeConn) (AUDp, AFExtCodes *);
typedef char *(*FuncPtr_errrorString) (AUDp, int, AFExtCodes *, char *, int);

typedef struct _AFExten {	/* private to extension mechanism */
	struct _AFExten *next;	/* next in list */
	AFExtCodes codes;	/* public information, all extension told */
	FuncPtr_freeAC		free_AC;
	FuncPtr_closeConn	close_audioconn;
	FuncPtr_errrorString	error_string;
	int (*error)();	/* who to call when an error occurs */
	char *name;		/* name of this extension */
} _AFExtension;


/*
 * Data structure for host setting; getting routines.
 *
 */

typedef struct {
	int family;		/* for example AF_DNET */
	int length;		/* length of address, in bytes */
	char *address;		/* pointer to where to find the bytes */
} AFHostAddress;

/*
 * data structure for AC routines.
 */
typedef struct {
	int play_gain;		/* play gain */
	int rec_gain;		/* record gain */
	ATime start_timeout;	/* start silence timeout, def. infinity */
	ATime end_silence;	/* end slince timeout, def. inifinity */
	ABool preempt;		/* whether it should preempt */
	AEncodeType type;	/* sample type */
	int channels;		/* number of channels */
	ABool endian;		/* endian-ness */
} AFSetACAttributes;

/*
 * Device description
 */
typedef struct {
	/* describe input/output capabilities */
	int numberOfInputs;		/* number of input sources */
	int numberOfOutputs;		/* number of output destinations */
	unsigned int inputsFromPhone; 
		/* mask of inputs connected to phone line */
	unsigned int outputsToPhone;  
		/* maks of outputs connected to phone line */

	/* Describe the play buffer type and size. */
	unsigned int playSampleFreq;	/* Sampling frequency, in 
					   samples per second.*/
	AEncodeType	 playBufType;	/* Data type supported.		    */
 	unsigned int playNchannels;	/* number of channels, 
					   e.g. 2 for stereo L&R.*/
	unsigned int playNSamplesBuf;/* Length in samples of play buffer.*/

	/* Describe the record buffer type and size. */
	unsigned int recSampleFreq; 	/* Sampling frequency, 
					   in samples per second.*/
	unsigned int recNchannels;	/* number of channels, 
					   e.g. 2 for stereo L&R.*/
	AEncodeType	 recBufType;	/* Data type supported.		    */
	unsigned int recNSamplesBuf;	/* Length in samples of record buffer*/
} AFDeviceDescriptor;

/*
 * AudioConn datatype maintaining display specific data.
 */
struct _AFSQEvent;
struct _ADisplayAtoms;
struct _AFreeFuncs;

typedef struct _AFAudioConn {
	struct _AFAudioConn *connection;
	AFExtData *ext_data;	/* hook for extension to hang data */
	struct _AFAudioConn *next; /* next open AudioConn on list */
	int fd;			/* Network socket. */
	int lock;		/* is someone in critical section? */
	int proto_major_version;/* maj. version of server's protocol */
	int proto_minor_version;/* minor version of servers protocol */
	char *vendor;		/* vendor of the server hardware */
        long resource_base;	/* resource ID base */
	long resource_mask;	/* resource ID mask bits */
	long resource_id;	/* allocator current ID */
	int resource_shift;	/* allocator shift to correct bits */
	AID (*resource_alloc)(struct _AFAudioConn *); /* allocator function */
	int vnumber;		/* AFlib's protocol version number. */
	int release;		/* release of the server */
	struct _AFSQEvent *head, *tail;	/* Input event queue. */
	int qlen;		/* Length of input event queue */
	unsigned long last_request_read; /* seq number of last event read */
	unsigned long request;	/* sequence number of last request. */
	char *last_req;		/* beginning of last request, or dummy */
	char *buffer;		/* Output buffer starting address. */
	char *bufptr;		/* Output buffer index pointer. */
	char *bufmax;		/* Output buffer maximum+1 address. */
	unsigned max_request_size; /* maximum number 32 bit words in request*/
	int (*synchandler)(struct _AFAudioConn *);	/* Synchronization handler */
	char *audioconn_name;	/* "host:server" string used on this connect*/
	char *scratch_buffer;	/* place to hang scratch buffer */
	unsigned long scratch_length;	/* length of scratch buffer */
	int ndevices;		/* number of audio devices */
	AFDeviceDescriptor *devices;
	int ext_number;		/* extension number on this display */
	_AFExtension *ext_procs;/* extensions initialized on this display */
	/*
	 * the following can be fixed size, as the protocol defines how
	 * much address space is available. 
	 * While this could be done using the extension vector, there
	 * may be MANY events processed, so a search through the extension
	 * list to find the right procedure for each event might be
	 * expensive if many extensions are being used.
	 */
	ABool (*event_vec[128])();  /* vector for wire to event */
	AStatus (*wire_vec[128])(); /* vector for event to wire */
	unsigned long flags;	   /* internal connection flags */
	struct _ADisplayAtoms *atoms;	/* for AFInternAtom */
	struct _AFreeFuncs *free_funcs;	/* internal free functions */
} AFAudioConn;

/*
 * Audio context.  All Alib routines deal in these rather than in
 * AContext ID's. This means it can be passed around anywhere
 * a connection might be necessary, to simplify the programming interface.
 */

typedef struct _AC {
	AFAudioConn *connection;	/* which connection */
	AFExtData *ext_data;		/* hook for connection */
	AFDeviceDescriptor *device;	/* which device */
	AContext acontext;		/* which ac */
	AFSetACAttributes attributes;	/* current attributes */
} *AC;


#undef _XEVENT_

#ifndef _XEVENT_
/*
 * A "AFEvent" structure always  has type as the first entry.  This 
 * uniquely identifies what  kind of event it is.  The second entry
 * is always a pointer to the display the event was read from.
 * The third entry is always a window of one type or another,
 * carefully selected to be useful to toolkit dispatchers.  (Except
 * for keymap events, which have no window.) You
 * must not change the order of the three elements or toolkits will
 * break! The pointer to the generic event must be cast before use to 
 * access any other information in the structure.
 */

/*
 * Definitions of specific events.
 */

#include <sys/time.h>

typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	ABool send_event;      /* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
	struct timeval stime;
	ATime time;
	ADevice device;
	ABool state;
} AFPhoneRingEvent;

typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	ABool send_event;      /* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
	struct timeval stime;
	ATime time;
	ADevice device;
	ABool state;
	int digit;
} AFPhoneDTMFEvent;

typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	ABool send_event;      /* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
	struct timeval stime;
	ATime time;
	ADevice device;
	ABool state;
} AFPhoneLoopEvent;

typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	ABool send_event;      /* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
	struct timeval stime;
	ATime time;
	ADevice device;
	ABool state;
} AFPhoneHookSwitchEvent;

typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	ABool send_event;      /* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
	struct timeval stime;
	ATime time;
	ADevice device;
	unsigned char hd[8];
} AFDSPEvent;

typedef struct {
        int type;
        unsigned long serial;    /* # of last request processed by server */
        ABool send_event;      /* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
	struct timeval stime;
        ADevice device;
        AAtom atom;
        ATime time;
        int state;              /* NewValue, Deleted */
} AFPropertyEvent;

typedef struct {
	int type;
	AFAudioConn *audioconn;	/* Display the event was read from */
	AID resourceid;		/* resource id */
	unsigned long serial;	/* serial number of failed request */
	unsigned char error_code;	/* error code of failed request */
	unsigned char request_code;	/* Major op-code of failed request */
	unsigned char minor_code;	/* Minor op-code of failed request */
} AFErrorEvent;

typedef struct {
	int type;
	unsigned long serial;	/* # of last request processed by server */
	ABool send_event;	/* true if this came from a SendEvent request */
	AFAudioConn *audioconn;  /* Display the event was read from */
} AFAnyEvent;

/*
 * this union is defined so Xlib can always use the same sized
 * event structure internally, to avoid memory fragmentation.
 */
typedef union _AFEvent {
        int type;		/* must not be changed; first element */
	AFAnyEvent aany;
	AFPhoneRingEvent aring;
	AFPhoneDTMFEvent adtmf;
	AFPhoneLoopEvent aloop;
	AFPhoneHookSwitchEvent ahook;
	AFDSPEvent adsp;
        AFPropertyEvent aproperty;
} AFEvent;
/*
 * _QEvent datatype for use in input queueing.
 */
typedef struct _AFSQEvent {
	struct _AFSQEvent *next;
	AFEvent event;
} _AFQEvent;	
#endif

#define AAllocID(aud) ((*(aud)->resource_alloc)((aud)))

/* Access.c */
void AFEnableAccessControl(register AFAudioConn *aud);
void AFDisableAccessControl(register AFAudioConn *aud);
void AFSetAccessControl(register AFAudioConn *aud, int mode);

/* OpenConn.c */

AFAudioConn *AFOpenAudioConn(
	/*const*/ char * /*server*/
	);

/* CloseConn.c */

void AFCloseAudioConn(
	AFAudioConn * /*aud*/
	);

/*ErrDes.c*/

void AFGetErrorText(
	register AFAudioConn * /*aud*/,
	register int /*code*/,
	char * /*buffer*/,
	int /*nbytes*/
	);

void AFGetErrorDatabaseText(
	AFAudioConn * /*aud*/,
	register const char * /*name*/,
	register const char * /*type*/,
	const char * /*defaultp*/,
	char * /*buffer*/,
	int /*nbytes*/
	);

/* ChACAttrs.c */
void AFChangeACAttributes (
       AC /*ac*/,
       unsigned long /*valuemask*/,
       AFSetACAttributes * /*attributes*/
       );

/* CreateAC.c */
AC AFCreateAC (
	register AFAudioConn * /*aud*/,
	register ADevice /*device*/,
	unsigned long /*valuemask*/,
	register AFSetACAttributes * /*attributes*/
	);	

/* CreatePAC.c */
AC AFCreatePhoneAC (
	register AFAudioConn * /*aud*/,
	unsigned long /*valuemask*/,
	register AFSetACAttributes * /*attributes*/
	);

/* FreeAC.c */
void AFFreeAC (
	AC /*ac*/
	);

/* SelEvents.c */
void AFSelectEvents (
	AC /*ac*/,
	long /*mask*/
	);	

/* HookSwitch.c */
void AFHookSwitch(
	AC /*ac*/,
	int /*onoff*/
	);

void AFFlashHook(
	AC /*ac*/,
	int /*duration in ms*/
	);

ATime AFGetTime(
	AC /*ac*/
	);


ATime AFPlaySamples(
	AC ac,
	ATime startTime,
	int nbytes,
	unsigned char *buf
	);

ATime AFRecordSamples(
	AC ac,
	ATime startTime,
	int nbytes,
	unsigned char *buf,
	ABool block
	);

void  AFNextEvent (
	AFAudioConn *aud,
	AFEvent *event
	);

void AFQueryPhone(
	AC ac,
	int *hsstate,
	int *lcstate
	);

void AFSetInputGain(
	AC ac,
	int gain
	);

void AFSetOutputGain(
	AC ac, 
	int gain
	);

int AFQueryOutputGain(
	AC ac,
	int *min_output_gain,
	int *max_output_gain
	);

int AFQueryInputGain(
	AC ac,
	int *min_input_gain,
	int *max_input_gain
	);

void AFEnableInput(
	AC ac,
	AMask mask,
	AMask *old_state,
	AMask *new_state
	);
	
void AFEnableOutput(
	AC ac,
	AMask mask,
	AMask *old_state,
	AMask *new_state
	);
	
void AFDisableOutput(
	AC ac,
	AMask mask,
	AMask *old_state,
	AMask *new_state
	);
	
void AFDisableInput(
	AC ac,
	AMask mask,
	AMask *old_state,
	AMask *new_state
	);
	
void AFEnablePassThrough(
	AC ac,
	ABool change,
	ABool *old_state,
	ABool *new_state
	);
	
void AFDisablePassThrough(
	AC ac,
	ABool change,
	ABool *old_state,
	ABool *new_state
	);
	
AFHostAddress *AFListHosts(
	register AFAudioConn *aud,
	int *nhosts_return, /* OUT */
	ABool *state_return /* OUT */
	);
	
void AFAddHost(
	AFAudioConn *aud,
	AFHostAddress *host
	);
	
	
void AFAddHosts(
	AFAudioConn *aud,
	AFHostAddress *hostlist,
	int count
	);
	
void AFRemoveHost(
	AFAudioConn *aud,
	AFHostAddress *host
	);
	
void AFRemoveHosts(
	AFAudioConn *aud,
	AFHostAddress *hostlist,
	int count
	);
	
void AFSync (
	register AFAudioConn *aud,
	ABool discard
	);
	

/* IfEvent.c */
void AFIfEvent (
	AFAudioConn *aud,
	AFEvent *event,
	ABool (*predicate)(),		/* function to call */
	char *arg
	);
	
/* PeekIfEvent.c */
void AFPeekIfEvent (
	AFAudioConn *aud,
	AFEvent *event,
	ABool (*predicate)(),
	char *arg
	);
	
/* ChkIfEvent.c */
ABool AFCheckIfEvent (
	AFAudioConn *aud,
	AFEvent *event,		/* XEvent to be filled in. */
	ABool (*predicate)(),	/* function to call */
	char *arg
	);
	
/* Pending.c */
int AFEventsQueued (
	AFAudioConn *aud,
	int mode
	);
	
int AFPending (
	AFAudioConn *aud
	);
	
/* AlibInt.c */
void AFFlush(
	AFAudioConn *aud
	);
	
/* ChangeProp.c */
void AFChangeProperty(
	AC ac,
	AAtom Property,
	AAtom type,	
	int format,
	int mode,
	unsigned char *data,
	int nelements
	);

/* ConnName.c */	
char *AFAudioConnName(char *);

/* DeleteProp.c */
void AFDeleteProperty(
	AC ac,
	AAtom property
	);
	
/* GetProperty.c */
int AFGetProperty(
	AC ac,
	AAtom property,
	long offset,
	long length,
	ABool del_prop,
	AAtom req_type,
	AAtom *actual_type,
	int *actual_format,
	unsigned long *nitems,
	unsigned long *bytesafter,
	unsigned char **prop
	);
	
/* InternAtom.c */
AAtom AFInternAtom(
	AFAudioConn *aud,
	const char *name,
	ABool onlyIfExists
	);
	
/* GetAtomName.c */
char *AFGetAtomName(
	AFAudioConn *aud,
	AAtom atom
	);
	
/* ListProps.c */
AAtom *AFListProperties(
	AC ac, 
	int *n_props
	);

/* ErrHndlr.c */
int (*AFSetErrorHandler(
	int (*handler)(AFAudioConn *, AFErrorEvent *)
	))(
#ifndef	mips
	AFAudioConn *, AFErrorEvent *
#endif
	);
	
int (*AFSetIOErrorHandler(
	int (*handler)(AFAudioConn *)
	))(
#ifndef	mips
	AFAudioConn *
#endif
	);

/* InitExt.c */
AFExtCodes *AFInitExtension(
	AFAudioConn*,	/* audio connection */
	const char*		/* name */
	);
	
AFExtCodes *AFAddExtension(
	AFAudioConn*        /* audio connection */
	);
	
AFExtData *AFFindOnExtensionList(
	 AFExtData**,		/* structure */
	 int			/* number */
	 );
/* QueryExt.c */
ABool AFQueryExtension(
	AFAudioConn *, 
	const char *,
	int *,
	int *,
	int *
	);
	

/* AFDialPhone.c */
extern int AFDialPhone(AC ac, char *number);
/* Dial the telephone using synthesized DTMF sounds, inband */


/* Synchro.c */
int (*AFSynchronize(
	register AFAudioConn *aud,
	int onoff
	))(
#ifndef	mips
	AFAudioConn *
#endif
	);

int (*AFSetAfterFunction(
	register AFAudioConn *aud,
	int (*func)(AFAudioConn *)
	))(
#ifndef	mips
	AFAudioConn *
#endif
	);

#define AFree(ptr)	(free((ptr)))

#ifdef __cplusplus			/* Close the C++ V2.0 brace. */
}
#endif

#endif


