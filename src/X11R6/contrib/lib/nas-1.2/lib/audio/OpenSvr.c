/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)OpenSvr.c,v 1.22 1994/04/07 20:26:20 greg Exp $
 */

/* Portions derived from */
/*
 * $XConsortium: XOpenDis.c,v 11.126 92/11/03 18:42:50 rws Exp $
 */

/* Copyright    Massachusetts Institute of Technology    1985, 1986	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include <audio/Alibint.h>
#include <audio/Aos.h>
#include <stdio.h>

#if !defined(lint) && !defined(SABER)
static int lock;	/* get rid of ifdefs when locking implemented */
#endif

static auReq _dummy_request = {
	0, 0, 0
};

static void _AuOCOutOfMemory();

extern AuBool _AuWireToEvent();
extern AuStatus _AuUnknownNativeEvent();
extern AuBool _AuUnknownWireEvent();

static int xferConnectionSetup();

/*****************************************************************************
 *				 AuOpenServer				     *
 *****************************************************************************/

/* 
 * Connects to a server, creates a AuServer object and returns a pointer to
 * the newly created AuServer back to the caller.
 */
#if NeedFunctionPrototypes
AuServer *AuOpenServer (
	register _AuConst char *server,
	int num_authproto,
	_AuConst char *authproto,
	int num_authdata,
	_AuConst char *authdata,
	char **ret_svrmsg)
#else
AuServer *AuOpenServer (server, 
			num_authproto, authproto,
			num_authdata, authdata, ret_svrmsg)
	register char *server;
	int num_authproto;
	char *authproto;
	int num_authdata;
	char *authdata;
	char **ret_svrmsg;
#endif
{
	register AuServer *aud;		/* New AuServer object being created */
	register int i;
	_AuConst char *server_name;	/* pointer to server name */
	int endian;			/* to determine which endian. */
	auConnClientPrefix client;	/* client information */
	auConnSetupPrefix prefix;	/* prefix information */
	int vendorlen;			/* length of vendor string */
	char *setup = NULL;		/* memory allocated at startup */
	char *fullname = NULL;		/* expanded name of server */
	int iserver;			/* server number */
	union {
		auConnSetup *setup;
		char *failure;
	} u;				/* proto data returned from server */
	AuInt32 setuplength;		/* number of bytes in setup message */
	unsigned char *varData;		/* pointer to variable length data that
					 * follows the connection setup data */

	int conn_auth_namelen = num_authproto;	  /* init the auth info */
	char *conn_auth_name = (char *) authproto;
	int conn_auth_datalen = num_authdata;
	char *conn_auth_data = (char *) authdata;

	AuUint32 mask;
	extern AuBool _AuSendClientPrefix();
	extern int _AuConnectAuServer();
	extern AuID _AuAllocID();

	/*
	 * Initialize the return message to a known good value
	 */
	if (ret_svrmsg)
	    *ret_svrmsg = NULL;

	/*
	 * If the server specifier string supplied as an argument to this 
	 * routine is NULL or a pointer to NULL, read the DISPLAY variable.
	 */
	server_name = AuServerName (server);
	if (!server_name || !server_name[0])
	    return NULL;

/*
 * Lock against other threads trying to access global data (like the error
 * handlers and server list).
 */
	_AuLockMutex(&lock);

/*
 * Attempt to allocate a server structure. Return NULL if allocation fails.
 */
	if ((aud = (AuServer *)Aucalloc(1, sizeof(AuServer))) == NULL) {
		_AuUnlockMutex(&lock);
		return(NULL);
	}

/*
 * Set the default error handlers.
 */
	aud->funcs.error_handler = (AuErrorHandler) NULL;
	aud->funcs.ioerror_handler = (AuIOErrorHandler) NULL;


/*
 * Call the Connect routine to get the network socket. If -1 is returned, the
 * connection failed. The connect routine will set fullname to point to the
 * expanded name.
 */

	if ((aud->fd = _AuConnectServer (server_name, &fullname, &iserver,
					 &conn_auth_name,
					 &conn_auth_namelen, &conn_auth_data,
					 &conn_auth_datalen)) < 0) {
		Aufree ((char *) aud);
		_AuUnlockMutex(&lock);
		return(NULL);
	}

	/* Initialize as much of the server structure as we can.
	 * Initialize pointers to NULL so that AuFreeServerStructure will
	 * work if we run out of memory before we finish initializing.
	 */
	aud->server_name	= fullname;
	aud->scratch_length	= 0L;
	aud->scratch_buffer	= NULL;
	aud->ext_procs		= (_AuExtension *)NULL;
	aud->ext_data		= (AuExtData *)NULL;
	aud->ext_number 	= 0;
	aud->event_vec[Au_Error] = _AuUnknownWireEvent;
	aud->event_vec[Au_Reply] = _AuUnknownWireEvent;
	aud->wire_vec[Au_Error]  = _AuUnknownNativeEvent;
	aud->wire_vec[Au_Reply]  = _AuUnknownNativeEvent;
	for (i = AuFirstEventType; i <= AuLastEventType; i++) {
	    aud->event_vec[i] 	= _AuWireToEvent;
	    aud->wire_vec[i] 	= NULL;
	}
	for (i = AuLastEventType + 1; i < 128; i++) {
	    aud->event_vec[i] 	= _AuUnknownWireEvent;
	    aud->wire_vec[i] 	= _AuUnknownNativeEvent;
	}
	aud->resource_id	= 0;
	aud->flags		= 0;
	aud->async_handlers	= NULL;
	aud->vendor		= NULL;
	aud->buffer		= NULL;
	aud->error_vec		= NULL;

/*
 * Setup other information in this server structure.
 */
	aud->vnumber = AuProtocolMajorVersion;
	aud->resource_alloc = _AuAllocID;
	aud->synchandler = NULL;
	aud->request = 0;
	aud->last_request_read = 0;
	aud->last_req = (char *)&_dummy_request;

	/* Set up the output buffers. */
	if ((aud->bufptr = aud->buffer = Aumalloc(BUFSIZE)) == NULL) {
	        _AuOCOutOfMemory (aud, setup);
		_AuUnlockMutex(&lock);
		return(NULL);
	}
	aud->bufmax = aud->buffer + BUFSIZE;
 
	/* Set up the input event queue and input event queue parameters. */
	aud->head = aud->tail = NULL;
	aud->qlen = 0;

	/* Set up the scratch flow info */
	aud->scratch_flows.total = aud->scratch_flows.num_inuse = 0;

/*
 * The auConnClientPrefix describes the initial connection setup information
 * and is followed by the authorization information.  Sites that are interested
 * in security are strongly encouraged to use an authentication and 
 * authorization system such as Kerberos.
 */
	endian = 1;
	if (*(char *) &endian)
	    client.byteOrder = '\154'; /* 'l' */
	else
	    client.byteOrder = '\102'; /* 'B' */
	client.majorVersion = AuProtocolMajorVersion;
	client.minorVersion = AuProtocolMinorVersion;
	client.nbytesAuthProto = conn_auth_namelen;
	client.nbytesAuthString = conn_auth_datalen;
	if (!_AuSendClientPrefix(aud, &client, conn_auth_name, conn_auth_data))
	{
	    _AuDisconnectServer (aud->fd);
	    Aufree ((char *)aud);
	    _AuUnlockMutex(&lock);
	    return(NULL);
	}	    
	/* see if we had changed the auth info and release it */
	if (conn_auth_name && conn_auth_name != authproto)
	    Aufree(conn_auth_name);
	if (conn_auth_data && conn_auth_data != authdata)
	    Aufree(conn_auth_data);
/*
 * Now see if connection was accepted...
 */
	_AuRead (aud, (char *)&prefix,(AuInt32)SIZEOF(auConnSetupPrefix));

	if (prefix.majorVersion < AuProtocolMajorVersion ||
	    prefix.minorVersion != AuProtocolMinorVersion)
	    fprintf (stderr, "audiolib: warning, client is protocol rev %d.%d \
server is %d.%d!\r\n",
		     AuProtocolMajorVersion, AuProtocolMinorVersion,
		     prefix.majorVersion, prefix.minorVersion);

	setuplength = prefix.length << 2;
	if ( (u.setup = (auConnSetup *)
	      (setup =  Aumalloc ((unsigned) setuplength))) == NULL) {
		_AuDisconnectServer (aud->fd);
		Aufree ((char *)aud);
		_AuUnlockMutex(&lock);
		return(NULL);
	}
	_AuRead (aud, (char *)u.setup, setuplength);
/*
 * If the connection was not accepted by the server due to problems,
 * give error message to the user....
 */
	if (prefix.success != auTrue) {
	    int len = (int) prefix.lengthReason;
	    if (ret_svrmsg) {
		*ret_svrmsg = (char *) Aumalloc (len + 1);
		if (*ret_svrmsg) {
		    (void) strncpy (*ret_svrmsg, u.failure, len);
		    (*ret_svrmsg)[len] = '\0';
		}
	    } else {
		/* client is really out of memory, fallback to printing */
		fprintf (stderr, 
			 "%s:  connection to \"%s\" refused by server\r\n%s:  ",
			 "audiolib", fullname, "audiolib");
		(void) fwrite (u.failure, sizeof(char),
			(int)prefix.lengthReason, stderr);
		(void) fwrite ("\r\n", sizeof(char), 2, stderr);
	    }

	    _AuOCOutOfMemory(aud, setup);
	    _AuUnlockMutex(&lock);
	    return (NULL);
	}

/*
 * We succeeded at authorization, so let us move the data into
 * the server structure.
 */
	aud->proto_major_version= prefix.majorVersion;
	aud->proto_minor_version= prefix.minorVersion;
	aud->release 		= u.setup->release;
	aud->resource_base	= u.setup->ridBase;
	aud->resource_mask	= u.setup->ridMask;
	aud->max_request_size	= u.setup->maxRequestSize;
	mask = aud->resource_mask;
	aud->resource_shift	= 0;
	while (!(mask & 1)) {
	    aud->resource_shift++;
	    mask = mask >> 1;
	}
/*
 * now extract the vendor string...  String must be null terminated,
 * padded to multiple of 4 bytes.
 */
	aud->vendor = (char *) Aumalloc((unsigned) (u.setup->nbytesVendor + 1));
	if (aud->vendor == NULL) {
	    _AuOCOutOfMemory(aud, setup);
	    _AuUnlockMutex(&lock);
	    return (NULL);
	}

	varData = ((unsigned char *) u.setup) + sz_auConnSetup;
	vendorlen = u.setup->nbytesVendor;
  	(void) strncpy(aud->vendor, (char *)varData, vendorlen);
	aud->vendor[vendorlen] = '\0';
 	vendorlen = PAD4(vendorlen);
	varData += vendorlen;
/*
 * Now iterate down setup information.....
 */
	if (!xferConnectionSetup(u.setup, aud, varData))
	    return NULL;

/*
 * Now start talking to the server to setup all other information...
 */

	Aufree (setup);	/* all finished with setup information */

/*
 * and done mucking with the server
 */
	_AuUnlockServer(aud);		/* didn't exist, so didn't lock */
	_AuUnlockMutex(&lock);

/*
 * and return successfully
 */
 	return(aud);
}


/* _AuOCOutOfMemory is called if malloc fails.  AuOpenServer returns NULL
   after this returns. */

static void
_AuOCOutOfMemory (aud, setup)
    AuServer *aud;
    char *setup;
{
    _AuDisconnectServer (aud->fd);
    _AuFreeServerStructure (aud);
    if (setup) Aufree (setup);
}


/*
 * AuFreeServerStructure frees all the storage associated with a AuServer.
 * It is used by AuOpenServer if it runs out of memory, and also by
 * AuCloseServer.   It needs to check whether all pointers are non-NULL
 * before dereferencing them, since it may be called by AuOpenServer before
 * the AuServer structure is fully formed. AuOpenServer must be sure to
 * initialize all the pointers to NULL before the first possible call on
 * this.
 */

#define AufreeIf(x)							       \
{									       \
    if (x)								       \
	Aufree(x);							       \
}

void
_AuFreeServerStructure(aud)
register AuServer  *aud;
{
    int                 i;

    while (aud->ext_procs)
    {
	_AuExtension       *ext = aud->ext_procs;
	aud->ext_procs = ext->next;
	AufreeIf(ext->name);
	Aufree((char *) ext);
    }

    AufreeIf(aud->server_name);
    AufreeIf(aud->vendor);

    AufreeIf(aud->connsetup.formats);
    AufreeIf(aud->connsetup.element_types);
    AufreeIf(aud->connsetup.wave_forms);
    AufreeIf(aud->connsetup.actions);

    for (i = 0; i < aud->connsetup.num_devices; i++)
    {
	AufreeIf(aud->connsetup.devices[i].common.description.data);
	AufreeIf(aud->connsetup.devices[i].device.children);
    }
    AufreeIf(aud->connsetup.devices);

    for (i = 0; i < aud->connsetup.num_buckets; i++)
	AufreeIf(aud->connsetup.buckets[i].common.description.data);
    AufreeIf(aud->connsetup.buckets);

    AufreeIf(aud->buffer);
    AufreeIf((char *) aud->error_vec);

    _AuFreeExtData(aud->ext_data);
    AufreeIf(aud->scratch_buffer);

    _AuFreeQ(aud);

    Aufree((char *) aud);
}

#define dst     aud->connsetup

#define xferFail()							       \
{									       \
    _AuOCOutOfMemory(aud, (char *) src);				       \
    _AuUnlockMutex(&lock);						       \
    return (NULL);							       \
}

#define xferAlloc(_dst, _type, _size)					      \
{									      \
    if (_size)								      \
	if (!((_dst) = (_type *) Aumalloc((_size) * sizeof(_type))))	      \
	    xferFail();							      \
}

static int
xferConnectionSetup(src, aud, varData)
auConnSetup        *src;
AuServer           *aud;
unsigned char *varData;
{
    int i;

    dst.min_sample_rate = src->minSampleRate;
    dst.max_sample_rate = src->maxSampleRate;
    dst.max_tracks = src->maxTracks;
    dst.num_formats = src->numFormats;
    dst.num_element_types = src->numElementTypes;
    dst.num_wave_forms = src->numWaveForms;
    dst.num_actions = src->numActions;
    dst.num_devices = src->numDevices;
    dst.num_buckets = src->numBuckets;
#ifdef NOTYET
    dst.num_radios = src->numRadios;
#endif				/* NOTYET */

    /* transfer formats */
    xferAlloc(dst.formats, int, dst.num_formats)
    for (i = 0; i < dst.num_formats; i++)
	dst.formats[i] = varData[i];
    varData += PAD4(dst.num_formats);

    /* transfer element types */
    xferAlloc(dst.element_types, int, dst.num_element_types)
    for (i = 0; i < dst.num_element_types; i++)
	dst.element_types[i] = varData[i];
    varData += PAD4(dst.num_element_types);

    /* transfer wave forms */
    xferAlloc(dst.wave_forms, int, dst.num_wave_forms)
    for (i = 0; i < dst.num_wave_forms; i++)
	dst.wave_forms[i] = varData[i];
    varData += PAD4(dst.num_wave_forms);

    /* transfer actions */
    xferAlloc(dst.actions, int, dst.num_actions)
    for (i = 0; i < dst.num_actions; i++)
	dst.actions[i] = varData[i];
    varData += PAD4(dst.num_actions);

    /* transfer devices */
    if (!(dst.devices = (AuDeviceAttributes *)
	  Aucalloc(1, sizeof(AuDeviceAttributes) * dst.num_devices)))
	xferFail();

    for (i = 0; i < dst.num_devices; i++)
    {
	_xferDeviceAttributes((auDeviceAttributes *) varData, dst.devices[i]);
	varData += sz_auDeviceAttributes;

	if (dst.devices[i].common.description.len)
	{
	    int len = dst.devices[i].common.description.len;
	    char *s;

	    xferAlloc(s, char, len + 1);
	    bcopy(varData, s, len);
	    s[len] = 0;
	    dst.devices[i].common.description.data = s;
	    varData += PAD4(len);
	}

	if (dst.devices[i].device.num_children)
	{
	    int len = dst.devices[i].device.num_children * sizeof(AuDeviceID);

	    xferAlloc(dst.devices[i].device.children, AuDeviceID, len);
	    bcopy(varData, dst.devices[i].device.children, len);
	    varData += len;
	}
    }

    /* transfer buckets */
    if (dst.num_buckets)
	if (!(dst.buckets = (AuBucketAttributes *)
	      Aucalloc(1, sizeof(AuBucketAttributes) * dst.num_buckets)))
	    xferFail();

    for (i = 0; i < dst.num_buckets; i++)
    {
	_xferBucketAttributes((auBucketAttributes *) varData, dst.buckets[i]);
	varData += sz_auBucketAttributes;

	if (dst.buckets[i].common.description.len)
	{
	    int len = dst.buckets[i].common.description.len;
	    char *s;

	    xferAlloc(s, char, len + 1);
	    bcopy(varData, s, len);
	    s[len] = 0;
	    dst.buckets[i].common.description.data = s;
	    varData += PAD4(len);
	}
    }

    return 1;
}
