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
/* Definitions for the server and c bindings */

/*
 * This packet-construction scheme makes the following assumptions:
 *
 * 1. The compiler is able
 * to generate code which addresses one- and two-byte quantities.
 * In the worst case, this would be done with bit-fields.  If bit-fields
 * are used it may be necessary to reorder the request fields in this file,
 * depending on the order in which the machine assigns bit fields to
 * machine words.  There may also be a problem with sign extension,
 * as K+R specify that bitfields are always unsigned.
 *
 * 2. 2- and 4-byte fields in packet structures must be ordered by hand
 * such that they are naturally-aligned, so that no compiler will ever
 * insert padding bytes.
 *
 * 3. All packets are hand-padded to a multiple of 4 bytes, for
 * the same reason.
 */

#ifndef APROTO_H
#define APROTO_H

/*
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved


DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/


/*
 * Define constants for the sizes of the network packets.  The sz_ prefix is
 * used instead of something more descriptive so that the symbols are no more
 * than 32 characters in length (which causes problems for some compilers).
 */
#define sz_aConnClientPrefix 12
#define sz_aConnSetupPrefix 8
#define sz_aConnSetup 20
#define sz_aDevice 44
#define sz_aGenericReply 32
#define sz_aError 32
#define sz_aEvent 32
#define sz_aReq 4
#define sz_aSyncConnectionReq 4
#define sz_aResourceReq 8
#define sz_aReply 32
#define sz_aHostEntry 4
#define sz_aDialPhoneReq 12
#define sz_aCreateACReq 16
#define sz_aChangeACAttributesReq 16
#define sz_aHookSwitchReq 12
#define sz_aFlashHookReq 12
#define sz_aSelectEventsReq 12
#define sz_aPlaySamplesReq 20
#define sz_aRecordSamplesReq 20
#define sz_aGetTimeReq 8
#define sz_aGetTimeReply 32
#define sz_aQueryExtensionReq 8
#define sz_aQueryExtensionReply 32
#define sz_aListExtensionsReply 32
#define sz_aSetAccessControlReq 8
#define sz_aListHostsReq 4
#define sz_aListHostsReply 32
#define sz_aHostEntry 4
#define sz_aChangeHostsReq 8
#define sz_aSetInputGainReq 12
#define sz_aSetOutputGainReq 12
#define sz_aQueryInputGainReq 8
#define sz_aQueryOutputGainReq 8
#define sz_aEnableInputReq 	sizeof(aEnableInputReq)
#define sz_aEnableOutputReq	sizeof(aEnableOutputReq)
#define sz_aDisableInputReq	sizeof(aDisableInputReq)
#define sz_aDisableOutputReq 	sizeof(aDisableOutputReq)
#define sz_aEnablePassThroughReq sizeof(aEnablePassThroughReq)
#define sz_aDisablePassThroughReq sizeof(aDisablePassThroughReq)
#define sz_aEnableGainControlReq sizeof(aEnableGainControlReq)
#define sz_aDisableGainControlReq sizeof(aDisableGainControlReq)
#define	sz_aQueryPhoneReq 	sizeof(aQueryPhoneReq)
#define sz_aChangePropertyReq 	sizeof(aChangePropertyReq)
#define sz_aDeletePropertyReq 	sizeof(aDeletePropertyReq)
#define sz_aGetPropertyReq 	sizeof(aGetPropertyReq)
#define sz_aGetPropertyReply 	sizeof(aGetPropertyReply)
#define sz_aInternAtomReq 	sizeof(aInternAtomReq)
#define sz_aAtomNameReply 	sizeof(aAtomNameReply)
#define sz_aListPropertiesReply sizeof(aListPropertiesReply)


#include <AF/Amd.h>

/* 
 * For the purpose of the structure definitions in this file,
 * we must redefine the following types in terms of audiomd.h's types, 
 * which may include bit fields.  All of these are #undef'd at the end of 
 * this file, restoring the definitions in audio.h.  
 */

#define AContext CARD32
#define AAtom CARD32
#define ATime CARD32
#define ADevice CARD32

#ifndef	UseOldTCPPort
#define A_TCP_PORT 1411 		/* add server number */
#else
#define A_TCP_PORT 7000 		/* add server number */
#endif

#define aTrue        1
#define aFalse       0

/* Reply codes */
#define A_Reply		1		/* Normal reply*/
#define A_Error		0		/* Error */

/* Request codes */

#define A_SelectEvents			2
#define A_DialPhone			3
#define A_CreateAC			4
#define A_ChangeACAttributes		5
#define A_FreeAC			6
#define A_PlaySamples			7
#define A_RecordSamples			8
#define A_GetTime			9
					/* 10 and 11 currently unused. */
#define A_QueryExtension		12
#define A_ListExtensions		13          
#define A_KillClient			14 
#define A_HookSwitch			15
#define	A_SetAccessControl		16
#define A_ChangeHosts			17
#define A_ListHosts			18
#define A_SetInputGain			19
#define A_SetOutputGain			20
#define A_QueryInputGain		21
#define A_QueryOutputGain		22
#define A_EnableInput			23
#define A_EnableOutput			24
#define A_DisableInput			25
#define A_DisableOutput			26
#define A_SyncConnection		27
#define	A_QueryPhone			28
#define A_EnablePassThrough		30
#define A_DisablePassThrough		31
#define	A_FlashHook			32
#define A_ChangeProperty		33
#define A_DeleteProperty		34
#define A_GetProperty			35
#define A_ListProperties		36
#define A_InternAtom			37
#define A_GetAtomName			38
#define A_EnableGainControl		39
#define A_DisableGainControl		40
#define A_NoOperation			127

/* masks for play and record requests */
#define ABigEndianMask			(1L << 0)
#define ALittleEndianMask		0
#define ABlockMask			(1L << 1)
#define ANoTimeReplyMask		(1L << 2)

/*
 * Initial connection handshake.
 */
typedef struct {
    CARD8	byteOrder;
    BYTE	pad;
    CARD16	majorVersion B16, minorVersion B16;
    CARD16	nbytesAuthProto B16;	/* Authorization protocol */
    CARD16	nbytesAuthString B16;	/* Authorization string */
    CARD16	pad2;
} aConnClientPrefix;


typedef struct {
    BOOL           success;
    BYTE           lengthReason; /*num bytes in string following if failure */
    CARD16         majorVersion B16, 
                   minorVersion B16;
    CARD16         length B16;  /* 1/4 additional bytes in setup info */
} aConnSetupPrefix;

/*
 *  connection setup structure.  This is followed by
 *  numDevices aDevice structs.
 */

typedef struct {
    CARD32         release B32;
    CARD32         ridBase B32, 
                   ridMask B32;
    CARD16         nbytesVendor B16;	/* number of bytes in vendor string */
    CARD16         maxRequestSize B16;
    CARD8          numDevices;          /* number of dev. structs to follow */
    CARD8	   pad0;
    CARD16	   pad1 B16;
} aConnSetup;

typedef struct {
    CARD8	type;			/* Codec, Hi-Fi */
    CARD8	playNchannels;
    CARD8	recordNchannels;
    CARD8	pad;
    
    CARD32	numberOfInputs;
    CARD32	numberOfOutputs;
    CARD32	inputsFromPhone;	/* bitmask of inputs from phone */
    CARD32	outputsToPhone;		/* bitmask of outputs to phone */

    CARD32	playSampleFreq B32;	/* sampling frequecy, in samples/sec*/
    CARD32	playBufType B32;	/* data type supported */
    CARD32	playNSamplesBuf B32;	/* length of samples of play buffer */

    CARD32	recordSampleFreq B32;	/* sampling frequecy, in samples/sec*/
    CARD32	recordBufType B32;	/* data type supported */
    CARD32	recordNSamplesBuf B32;	/* length of samples of play buffer */
	/* XXX other stuff may need to go here */
} aDevice;

typedef struct {
    CARD8 family;
    BYTE pad;
    CARD16 length B16;
} aHostEntry;

/*
 * Aerror
 *    All errors  are 32 bytes 
 */

typedef struct {
    BYTE type;                  /* A_Error */
    BYTE errorCode;
    CARD16 sequenceNumber B16;       /* the nth request from this client */
    CARD32 resourceID B32;
    CARD16 minorCode B16;
    CARD8 majorCode;
    BYTE pad1;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
} aError;


/*
 * XRep:
 *    meant to be 32 byte quantity 
 */


/*
 * the protocol requests themselves.
 */


/* Request structure */

typedef struct {
	CARD8 reqType;
	CARD8 data;            /* meaning depends on request type */
	CARD16 length B16;     /* length in 4 bytes quantities 
				  of whole request, including this header */
} aReq;

typedef aReq aSyncConnectionReq;

/*
 * ResourceReq is used for any request which has a resource ID 
 * (or AAtom or ATime) as its one and only argument.  
 */

typedef struct {
    CARD8 reqType;
    BYTE pad;
    CARD16 length B16;
    CARD32 id B32;  /* a ADevice, AContext.... */
} aResourceReq;

typedef struct {
	CARD8 reqType;
	BYTE pad;
	CARD16 length B16;
	AContext ac B32;
} aGetTimeReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	AContext ac B32;
	CARD32	onoff B32;
} aHookSwitchReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	AContext ac B32;
	CARD32	duration B32;
} aFlashHookReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	CARD32 mode B32;
} aSetAccessControlReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	CARD16 nbytes B16;  /* number of string bytes following structure */
	BYTE pad1, pad2;
	AContext ac B32;
} aDialPhoneReq;		/* string follows on word boundary */

typedef struct {
	CARD8	reqType;
	BYTE	pad;
        CARD16 length B16;
	AContext ac B32;
} aQueryPhoneReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	AContext ac B32;
	CARD32	mask B32;
} aSelectEventsReq;

/* Create and Change AC must be the same size... Helps library out... */
typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	AContext ac B32;
	CARD32	mask B32;
	CARD32  pad0 B32;
} aChangeACAttributesReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	AContext ac B32;
	CARD32	mask B32;
	CARD32 device B32;
} aCreateACReq;

/*
 * data follows, described by all the arguments
 */
typedef struct {
	CARD8	reqType;
	BYTE	mask;			/* byteOrder, block/noblock */
	CARD16	length B16;
	AContext ac B32;
	ATime	startTime B32;
	CARD32	nbytes B32;
	CARD16	nchannels B16;
	CARD16	sampleType B16;
} aPlaySamplesReq;

/*
 * returns a reply with the time.
 */
typedef struct {
	CARD8	reqType;
	BYTE	mask;			/* byteOrder, block/noblock */
	CARD16	length B16;
	AContext ac B32;
	ATime	startTime B32;
	CARD32	nbytes B32;
	CARD16	nchannels B16;
	CARD16	sampleType B16;	
} aRecordSamplesReq;

/*
 * Takes a structure, and mask, so that it might be extended without
 * a protocol break.
 */

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16	length B16;
	AContext	ac B32;
	CARD32 mask B32;
} aSetDeviceReq;
/*
 * I/O control requests (gain...)
 */

typedef struct {
	CARD8	reqType;
	BYTE	pad;
        CARD16 length B16;
	AContext ac B32;
	INT32  gain B32;
} aSetGainReq;
typedef aSetGainReq aSetInputGainReq;
typedef aSetGainReq aSetOutputGainReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
        CARD16 length B16;
	AContext ac B32;
} aQueryGainReq;
typedef aQueryGainReq aQueryInputGainReq;
typedef aQueryGainReq aQueryOutputGainReq;

typedef struct {
	CARD8	reqType;
	BYTE	pad;
	CARD16 length B16;
	AContext ac B32;
	CARD32  mask;
} aControlIOReq;
typedef aControlIOReq aEnableOutputReq;
typedef aControlIOReq aEnableInputReq;
typedef aControlIOReq aDisableInputReq;
typedef aControlIOReq aDisableOutputReq;
typedef aControlIOReq aEnablePassThroughReq;
typedef aControlIOReq aDisablePassThroughReq;
typedef aControlIOReq aEnableGainControlReq;
typedef aControlIOReq aDisableGainControlReq;

/*
 * Extension related requests.
 */

typedef struct {
    CARD8 reqType;
    BYTE pad;
    CARD16 length B16;
    CARD16 nbytes B16;  /* number of string bytes following structure */
    BYTE pad1, pad2;
} aQueryExtensionReq;

typedef struct {
    CARD8 reqType;
    BYTE mode;
    CARD16 length B16;
    CARD8 hostFamily;
    BYTE pad;
    CARD16 hostLength B16;
} aChangeHostsReq;    

typedef struct {
    CARD8 reqType;
    BYTE pad;
    CARD16 length B16;
    } aListHostsReq;

typedef struct {
    CARD8 reqType;
    CARD8 mode;
    CARD16 length B16;
    AContext ac B32;
    AAtom property B32, type B32;
    CARD8 format;
    BYTE pad[3];
    CARD32 nUnits B32;     /* length of stuff following, depends on format */
} aChangePropertyReq;

typedef struct {
    CARD8 reqType;
    BYTE pad;
    CARD16 length B16;
    AContext ac B32;
    AAtom property B32;
} aDeletePropertyReq;

typedef struct {
    CARD8 reqType;
    BOOL c_delete;
    CARD16 length B16;
    AContext ac B32;
    AAtom property B32, type B32;
    CARD32 longOffset B32;
    CARD32 longLength B32;
} aGetPropertyReq;
 
typedef struct {    /* followed by padded string */
    CARD8 reqType;
    BOOL onlyIfExists;
    CARD16 length B16;
    CARD16 nbytes  B16;    /* number of bytes in string */
    CARD16 pad B16;
} aInternAtomReq;


/*
 * replies.
 */

/* 
 * GenericReply is the common format of all replies.  The "data" items
 * are specific to each individual reply type. 
 */


typedef struct {	
    BYTE type;              /* Reply */
    BYTE data1;             /* depends on reply type */
    CARD16 sequenceNumber B16;  /* of last request received by server */
    CARD32 length B32;      /* 4 byte quantities beyond size of GenericReply */
    CARD32 data00 B32;
    CARD32 data01 B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
} aGenericReply;

typedef struct {	
    BYTE type;              /* A_Reply */
    BYTE data1;             /* depends on reply type */
    CARD16 sequenceNumber B16;  /* of last request received by server */
    CARD32 length B32;      /* 4 byte quantities beyond size of GenericReply */
    CARD32 currentTime B32;    /* which record buffer in stream */
    CARD32 nbytes B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
} aRecordSamplesReply;

typedef struct {
    BYTE type;	/* A_Reply */
    BYTE pad1;
    CARD16 sequenceNumber;
    CARD32 length B32; /* always 0 */
    CARD32 time;
    CARD32 pad2;
    CARD32 pad3;
    CARD32 pad4;
    CARD32 pad5;
    CARD32 pad6;
} aGetTimeReply;
    
typedef struct {	
    BYTE type;              /* Reply */
    BYTE data1;             /* depends on reply type */
    CARD16 sequenceNumber B16;  /* of last request received by server */
    CARD32 length B32; /* always 0 */
    BOOL hs_state;
    BOOL loop_state;
    CARD16 pad0 B16;		/* 3 */
    CARD32 pad1 B32;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
} aQueryPhoneReply;

typedef struct {	
    BYTE type;              /* Reply */
    BYTE data1;             /* depends on reply type */
    CARD16 sequenceNumber B16;  /* of last request received by server */
    CARD32 length B32; /* always 0 */
    INT32 gain B32;      /* 4 byte quantities beyond size of GenericReply */
    INT32 minGain B32;
    INT32 maxGain B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
} aQueryGainReply;

typedef struct {	
    BYTE type;              /* Reply */
    BYTE data1;             /* depends on reply type */
    CARD16 sequenceNumber B16;  /* of last request received by server */
    CARD32 length B32; /* 0 */
    CARD32 oldState B32;    /* 4 byte quantities beyond size of GenericReply */
    CARD32 newState B32;
    CARD32 data02 B32;
    CARD32 data03 B32;
    CARD32 data04 B32;
    CARD32 data05 B32;
} aControlIOReply;

typedef struct {
    BYTE type;  /* A_Reply */
    BYTE pad1;
    CARD16 sequenceNumber B16; 
    CARD32 length B32;
    BOOL  present;
    CARD8 major_opcode;
    CARD8 first_event;
    CARD8 first_error;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
} aQueryExtensionReply;

typedef struct {
    BYTE type;  /* A_Reply */
    CARD8 nExtensions;
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
} aListExtensionsReply;

typedef struct {
    BYTE type;  /* A_Reply */
    BOOL enabled;
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD16 nHosts B16;
    CARD16 pad1 B16;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
    } aListHostsReply;

typedef struct {
    BYTE type;  /* A_Reply */
    BYTE pad1;
    CARD16 sequenceNumber B16;
    CARD32 length B32; /* 0 */
    AAtom atom B32;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    } aInternAtomReply;

typedef struct {
    BYTE type;  /* A_Reply */
    BYTE pad1;
    CARD16 sequenceNumber B16;
    CARD32 length B32;  /* of additional bytes */
    CARD16 nameLength B16;  /* # of characters in name */
    CARD16 pad2 B16;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
    } aGetAtomNameReply;

typedef struct {
    BYTE type;  /* A_Reply */
    CARD8 format;
    CARD16 sequenceNumber B16;
    CARD32 length B32; /* of additional bytes */
    AAtom propertyType B32;
    CARD32 bytesAfter B32;
    CARD32 nItems B32; /* # of 8, 16, or 32-bit entities in reply */
    CARD32 pad1 B32;
    CARD32 pad2 B32;
    CARD32 pad3 B32;
    } aGetPropertyReply;

typedef struct {
    BYTE type;  /* A_Reply */
    BYTE pad1;
    CARD16 sequenceNumber B16;
    CARD32 length B32;
    CARD16 nProperties B16;
    CARD16 pad2 B16;
    CARD32 pad3 B32;
    CARD32 pad4 B32;
    CARD32 pad5 B32;
    CARD32 pad6 B32;
    CARD32 pad7 B32;
    } aListPropertiesReply;

/*
 * definitions of events from the server.  Gets added to as needed.
 */

typedef struct {
    union {
	struct {
	    BYTE type;
	    BYTE detail;
	    CARD16 sequenceNumber B16;
	    } u;
	struct {
            CARD32 pad00 B32;
	    CARD32 sec B32;
	    CARD32 usec B32;
	    ATime time B32;
	    ADevice device B32;
	    BOOL state;
	    CARD8 digit;
	} PhoneDTMF;
	struct {
            CARD32 pad00 B32;
	    CARD32 sec B32;
	    CARD32 usec B32;
	    ATime time B32;
	    ADevice device B32;
	    BOOL state;
	} PhoneLoop;
	struct {
            CARD32 pad00 B32;
	    CARD32 sec B32;
	    CARD32 usec B32;
	    ATime time B32;
	    ADevice device B32;
	    BOOL state;
	} PhoneHookSwitch;
	struct {
            CARD32 pad00 B32;
	    CARD32 sec B32;
	    CARD32 usec B32;
	    ATime time B32;
	    ADevice device B32;
	    BOOL state;
	} PhoneRing;
	struct {
            CARD32 pad00 B32;
	    CARD32 sec B32;
	    CARD32 usec B32;
	    ATime time B32;
	    ADevice device B32;
	    CARD8 hd0;
	    CARD8 hd1;
	    CARD8 hd2;
	    CARD8 hd3;
	    CARD8 hd4;
	    CARD8 hd5;
	    CARD8 hd6;
	    CARD8 hd7;
	} DSP;
	struct {
            CARD32 pad00 B32;
	    ADevice device B32;
	    AAtom atom B32;
	    ATime time B32;
	    BYTE state;			/* NewValue or Deleted */
	    BYTE pad1;
	    CARD16 pad2 B16;
	} property;
	struct {
	    CARD32 pad00 B32;
	    CARD32 pad01 B32;
	    CARD32 pad02 B32;
	    CARD32 pad03 B32;
	    CARD32 pad04 B32;
	    CARD32 pad05 B32;
	    CARD32 pad06 B32;
	    CARD32 pad07 B32;
	} Dummy;
    } u;
} aEvent;

typedef union {
    aGenericReply generic;
    aGetTimeReply time;
    aListHostsReply hosts;
    aError error;
    aEvent event;
    aGetAtomNameReply atom_reply;
    aGetPropertyReply property_reply;
    aListPropertiesReply list_property_reply;
} aReply;



#undef AContext
#undef AAtom
#undef ATime
#undef ADevice

#endif
