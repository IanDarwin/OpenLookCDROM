/*
 * $Header: /crl/audio/AF/lib/AF/RCS/OpenConn.c,v 1.23 1994/02/22 18:23:12 tml Exp $
 */

/* Copyright    Massachusetts Institute of Technology    1985, 1986	*/
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

#include <stdio.h>
#include "Alibint.h"
#include <AF/Aos.h>
#include <AF/AFauth.h>

extern int _Adebug;
extern AFAudioConn *_AHeadOfAudioConnList;

#ifndef lint
static int lock;	/* get rid of ifdefs when locking implemented */
#endif

static aReq _dummy_request = {
	0, 0, 0
};

/*
 * First, a routine for setting authorization data
 */
static int aauth_namelen = 0;
static char *aauth_name = NULL;	 /* NULL means use default mechanism */
static int aauth_datalen = 0;
static char *aauth_data = NULL;	 /* NULL means get default data */

static OutOfMemory (AFAudioConn *, char *);

void ASetAuthorization (char *name, int namelen, char *data, int datalen)
{
    char *tmpname, *tmpdata;

    if (aauth_name) Xfree (aauth_name);	 /* free any existing data */
    if (aauth_data) Xfree (aauth_data);

    aauth_name = aauth_data = NULL;	/* mark it no longer valid */
    aauth_namelen = aauth_datalen = 0;

    if (namelen < 0) namelen = 0;	/* check for bogus inputs */
    if (datalen < 0) datalen = 0;	/* maybe should return? */

    if (namelen > 0)  {			/* try to allocate space */
	tmpname = Xmalloc ((unsigned) namelen);
	if (!tmpname) return;
	bcopy (name, tmpname, namelen);
    } else {
	tmpname = NULL;
    }

    if (datalen > 0)  {
	tmpdata = Xmalloc ((unsigned) datalen);
	if (!tmpdata) {
	    if (tmpname) (void) Xfree (tmpname);
	    return;
	}
	bcopy (data, tmpdata, datalen);
    } else {
	tmpdata = NULL;
    }

    aauth_name = tmpname;		/* and store the suckers */
    aauth_namelen = namelen;
    aauth_data = tmpdata;
    aauth_datalen = datalen;
    return;
}

/* 
 * Connects to a server, creates a AFAudioConn object and returns a pointer to
 * the newly created AFAudioConn back to the caller.
 */
AFAudioConn *AFOpenAudioConn (register /* const */ char *server)
{
	register AFAudioConn *aud;	/* New AFAudioConn object being created*/
	register int i;
	char *connection_name;		/* pointer to display name */
	int indian;			/* to determine which indian. */
	aConnClientPrefix client;	/* client information */
	aConnSetupPrefix prefix;	/* prefix information */
	int vendorlen;			/* length of vendor string */
	char *setup;			/* memory allocated at startup */
	char *fullname = NULL;		/* expanded name of server */
	int iserver;			/* server number */
	union {
		aConnSetup *setup;
		char *failure;
		char *vendor;
		aDevice *device;
	} u;				/* proto data returned from server */
	long setuplength;	/* number of bytes in setup message */
	AFauth *authptr = NULL;
	char *server_addr = NULL;
	int iscreen;			/* screen number */
	int server_addrlen = 0;
	char *conn_auth_name, *conn_auth_data;
	int conn_auth_namelen, conn_auth_datalen;
	int conn_family;

	/*
	 * If the server specifier string supplied as an argument to this 
	 * routine is NULL or a pointer to NULL, read the AUDIOFILE variable.
	 */
	if (server == NULL || *server == '\0') {
		if ((connection_name = getenv("AUDIOFILE")) == NULL) {
			if ((connection_name = getenv("DISPLAY")) == NULL) {
			/* Oops! No AUDIOFILE environment variable - error. */
				return(NULL);
			}
		}
	}
	else {
		/* AFAudioConn is non-NULL, copy the pointer */
		connection_name = (char *)server;
	}

/*
 * Lock against other threads trying to access global data (like the error
 * handlers and audioconn list).
 */
	LockMutex(&lock);

/*
 * Set the default error handlers.  This allows the global variables to
 * default to NULL for use with shared libraries.
 */
	if (_AErrorFunction == NULL) (void) AFSetErrorHandler (NULL);
	if (_AIOErrorFunction == NULL) (void) AFSetIOErrorHandler (NULL);

/*
 * Attempt to allocate a AFAudioConn structure. Return NULL if allocation fails.
 */
	if ((aud = (AFAudioConn *)Xcalloc(1, sizeof(AFAudioConn))) == NULL) {
		UnlockMutex(&lock);
		return(NULL);
	}
/*
 * needs to be setup immediately, or we'll lose elsewhere...
 */
	aud->connection		= aud;

/*
 * Call the Connect routine to get the network socket. If -1 is returned, the
 * connection failed. The connect routine will set fullname to point to the
 * expanded name.
 */

	if ((aud->fd = _AConnectAudioConn (connection_name, &fullname, &iserver,
					 &iscreen, &conn_family,
					 &server_addrlen, &server_addr)) < 0) {
		if (fullname) Xfree(fullname);
		if (aud) Xfree ((char *) aud);
		UnlockMutex(&lock);
		return(NULL);
	}

/*
 * Look up the authorization protocol name and data if necessary.
 */
	if (aauth_name && aauth_data) {
	    conn_auth_namelen = aauth_namelen;
	    conn_auth_name = aauth_name;
	    conn_auth_datalen = aauth_datalen;
	    conn_auth_data = aauth_data;
	} else {
	    char audnumbuf[40];		/* big enough to hold 2^64 and more */
	    (void) sprintf (audnumbuf, "%d", iserver);

	    authptr = AFauGetAuthByAddr ((unsigned short) conn_family,
					(unsigned short) server_addrlen,
					server_addr,
					(unsigned short) strlen (audnumbuf),
					audnumbuf,
					(unsigned short) aauth_namelen,
					aauth_name);
	    if (authptr) {
		conn_auth_namelen = authptr->name_length;
		conn_auth_name = (char *)authptr->name;
		conn_auth_datalen = authptr->data_length;
		conn_auth_data = (char *)authptr->data;
	    } else {
		conn_auth_namelen = 0;
		conn_auth_name = NULL;
		conn_auth_datalen = 0;
		conn_auth_data = NULL;
	    }
	}
#ifdef HASDES
	/*
	 * build XDM-AUTHORIZATION-1 data
	 */
	if (conn_auth_namelen == 19 &&
	    !strncmp (conn_auth_name, "XDM-AUTHORIZATION-1", 19))
	{
	    static char    encrypted_data[192/8];
	    int	    i, j;
	    struct sockaddr_in	in_addr;
	    int	    addrlen;
	    long    now;

	    j = 0;
	    for (i = 0; i < 8; i++)
	    {
		encrypted_data[j] = conn_auth_data[i];
		j++;
	    }
	    addrlen = sizeof (in_addr);
	    getsockname (aud->fd, (struct sockaddr *) &in_addr, &addrlen);
	    if (in_addr.sin_family == 2)
	    {
		encrypted_data[j] = in_addr.sin_addr.s_net; j++;
		encrypted_data[j] = in_addr.sin_addr.s_host; j++;
		encrypted_data[j] = in_addr.sin_addr.s_lh; j++;
		encrypted_data[j] = in_addr.sin_addr.s_impno; j++;
		encrypted_data[j] = (in_addr.sin_port >> 8) & 0xff; j++;
		encrypted_data[j] = (in_addr.sin_port) & 0xff; j++;
	    }
	    else
	    {
		encrypted_data[j] = 0xff; j++;
		encrypted_data[j] = 0xff; j++;
		encrypted_data[j] = 0xff; j++;
		encrypted_data[j] = 0xff; j++;
		i = getpid ();
		encrypted_data[j] = (i >> 8) & 0xff; j++;
		encrypted_data[j] = (i) & 0xff; j++;
	    }
	    time (&now);
	    for (i = 3; i >= 0; i--)
	    {
		encrypted_data[j] = (now >> (i * 8)) & 0xff;
		j++;
	    }
	    AdmcpEncrypt (encrypted_data, conn_auth_data + 8,
			  encrypted_data, 192/8);
	    conn_auth_data = encrypted_data;
	    conn_auth_datalen = 192 / 8;
	}
#endif
	if (server_addr) (void) Xfree (server_addr);
/*
 * The aConnClientPrefix describes the initial connection setup information
 * and is followed by the authorization information.  Sites that are interested
 * in security are strongly encouraged to use an authentication and 
 * authorization system such as Kerberos.
 */
	indian = 1;
	if (*(char *) &indian)
	    client.byteOrder = 'l';
	else
	    client.byteOrder = 'B';
	client.majorVersion = A_PROTOCOL;
	client.minorVersion = A_PROTOCOL_REVISION;
	client.nbytesAuthProto = conn_auth_namelen;
	client.nbytesAuthString = conn_auth_datalen;
	_ASendClientPrefix (aud, &client, conn_auth_name, conn_auth_data);
	if (authptr) AFauDisposeAuth (authptr);
/*
 * Now see if connection was accepted...
 */
	_ARead (aud, (char *)&prefix,(long)SIZEOF(aConnSetupPrefix));

	if (prefix.majorVersion < A_PROTOCOL) {
	    fprintf (stderr,
       "Alib:  warning, client built for newer rev (%d) than server (%d)!\r\n",
		     A_PROTOCOL, prefix.majorVersion);
	}
	if (prefix.minorVersion != A_PROTOCOL_REVISION) {
	    fprintf (stderr,
     "Alib:  warning, client is protocol rev %d, server is rev %d!\r\n",
		     A_PROTOCOL_REVISION, prefix.minorVersion);
	}

	setuplength = prefix.length << 2;
	if ( (u.setup = (aConnSetup *)
	      (setup =  Xmalloc ((unsigned) setuplength))) == NULL) {
		Xfree(fullname);
		Xfree ((char *)aud);
		UnlockMutex(&lock);
		return(NULL);
	}
	_ARead (aud, (char *)u.setup, setuplength);
/*
 * If the connection was not accepted by the server due to problems,
 * give error message to the user....
 */
	if (prefix.success != aTrue) {
		/* XXX - printing messages marks a bad programming interface */
		fprintf (stderr, 
			 "%s:  connection to \"%s\" refused by server\r\n%s:  ",
			 "Alib", fullname, "Alib");
		(void) fwrite (u.failure, sizeof(char),
			(int)prefix.lengthReason, stderr);
		(void) fwrite ("\r\n", sizeof(char), 2, stderr);
		Xfree(fullname);
		Xfree ((char *)aud);
		Xfree (setup);
		UnlockMutex(&lock);
		return (NULL);
	}

/*
 * We succeeded at authorization, so let us move the data into
 * the server structure.
 */
	aud->next		= (AFAudioConn *) NULL;
	aud->proto_major_version= prefix.majorVersion;
	aud->proto_minor_version= prefix.minorVersion;
	aud->release 		= u.setup->release;
	aud->resource_base	= u.setup->ridBase;
	aud->resource_mask	= u.setup->ridMask;
	aud->ndevices		= u.setup->numDevices;
        aud->max_request_size   = u.setup->maxRequestSize;
	aud->scratch_length	= 0L;
	aud->scratch_buffer	= NULL;
	aud->ext_procs		= (_AFExtension *)NULL;
	aud->ext_data		= (AFExtData *)NULL;
	aud->ext_number 	= 0;
	aud->event_vec[A_Error] = _AUnknownWireEvent;
	aud->event_vec[A_Reply] = _AUnknownWireEvent;
	aud->wire_vec[A_Error]  = _AUnknownNativeEvent;
	aud->wire_vec[A_Reply]  = _AUnknownNativeEvent;
	for (i = AReply + 1; i < ALASTEvent; i++) {
	    aud->event_vec[i] 	= _AWireToEvent;
	    aud->wire_vec[i] 	= NULL;
	}
	for (i = ALASTEvent; i < 128; i++) {
	    aud->event_vec[i] 	= _AUnknownWireEvent;
	    aud->wire_vec[i] 	= _AUnknownNativeEvent;
	}
	aud->resource_id	= 0;
	aud->resource_shift	= ffs(aud->resource_mask) - 1;
	aud->flags		= 0;
/* 
 * Initialize pointers to NULL so that AFreeAudioConnStructure will
 * work if we run out of memory
 */

	aud->vendor = NULL;
	aud->buffer = NULL;
	aud->atoms = NULL;
	aud->devices = NULL;
	aud->free_funcs = NULL;

/*
 * Salt away the host:server string for later use.
 * Needs to be assigned here, so that it can be freed if necessary.
 */
	aud->audioconn_name = fullname;

/*
 * now extract the vendor string...  String must be null terminated,
 * padded to multiple of 4 bytes.
 */
	aud->vendor = (char *) Xmalloc((unsigned) (u.setup->nbytesVendor + 1));
	if (aud->vendor == NULL) {
	    OutOfMemory(aud, setup);
	    UnlockMutex(&lock);
	    return (NULL);
	}
	vendorlen = u.setup->nbytesVendor;
 	u.setup = (aConnSetup *) (((char *) u.setup) + sz_aConnSetup);
  	(void) strncpy(aud->vendor, u.vendor, vendorlen);
	aud->vendor[vendorlen] = '\0';
 	vendorlen = (vendorlen + 3) & ~3;	/* round up */
	bcopy (u.vendor + vendorlen, setup,
	       (int) setuplength - sz_aConnSetup - vendorlen);
 	u.vendor = setup;
/*
 * Now iterate down setup information.....
 */
	aud->devices =
		(AFDeviceDescriptor *) Xmalloc(
		    (unsigned) (aud->ndevices * sizeof (AFDeviceDescriptor)));
	if (aud->devices == NULL) {
		OutOfMemory (aud, setup);
		UnlockMutex(&lock);
		return(NULL);
	}
/*
 * decode device information
 */
	for (i = 0; i < aud->ndevices; i++) {
		register AFDeviceDescriptor *d = &aud->devices[i];
		d->numberOfInputs	= u.device->numberOfInputs;
		d->numberOfOutputs	= u.device->numberOfOutputs;
		d->inputsFromPhone	= u.device->inputsFromPhone;
		d->outputsToPhone	= u.device->outputsToPhone;
		d->playSampleFreq	= u.device->playSampleFreq;
		d->playBufType		= u.device->playBufType;
		d->playNchannels	= u.device->playNchannels;
		d->playNSamplesBuf	= u.device->playNSamplesBuf;
		d->recSampleFreq	= u.device->recordSampleFreq;
		d->recNchannels		= u.device->recordNchannels;
		d->recBufType		= u.device->recordBufType;
		d->recNSamplesBuf	= u.device->recordNSamplesBuf;
		u.device = (aDevice *) (((char *) u.device) + sz_aDevice);
	}
	

/*
 * Setup other information in this server structure.
 */
	aud->vnumber = A_PROTOCOL;
	aud->resource_alloc = _AAllocID;
	aud->synchandler = NULL;
	aud->request = 0;
	aud->last_request_read = 0;
	aud->last_req = (char *)&_dummy_request;

	/* Set up the output buffers. */
	if ((aud->bufptr = aud->buffer = Xmalloc(BUFSIZE)) == NULL) {
	        OutOfMemory (aud, setup);
		UnlockMutex(&lock);
		return(NULL);
	}
	aud->bufmax = aud->buffer + BUFSIZE;
 
	/* Set up the input event queue and input event queue parameters. */
	aud->head = aud->tail = NULL;
	aud->qlen = 0;

        
        /* Set up free-function record */
        if ((aud->free_funcs = (_AFreeFuncRec *)Xcalloc(1,
                                                        sizeof(_AFreeFuncRec)))
            == NULL) {
            OutOfMemory (aud, setup);
            UnlockMutex(&lock); 
            return(NULL);
        }

/*
 * Now start talking to the server to setup all other information...
 */

	Xfree (setup);	/* all finished with setup information */

/*
 * call into synchronization routine so that all programs can be
 * forced synchronize
 */
	(void) AFSynchronize(aud, _Adebug);
/*
 * chain this stucture onto global list.
 */
	aud->next = _AHeadOfAudioConnList;
	_AHeadOfAudioConnList = aud;


/*
 * and done mucking with the server
 */
	UnlockConnection(aud);		/* didn't exist, so didn't lock */
	UnlockMutex(&lock);


/*
 * and return successfully
 */
 	return(aud);
}


/* OutOfMemory is called if malloc fails.  AOpenAudioConn returns NULL
   after this returns. */

static OutOfMemory (AFAudioConn *aud, char *setup)
{
    _ADisconnectAudioConn (aud->fd);
    _AFreeAudioConnStructure (aud);
    if (setup) Xfree (setup);
}


/* AFreeAudioConnStructure frees all the storage associated with a 
 * AFAudioConn.  It is used by AOpenAudioConn if it runs out of memory,
 * and also by ACloseAudioConn.   It needs to check whether all pointers
 * are non-NULL before dereferencing them, since it may be called
 * by AOpenAudioConn before the AFAudioConn structure is fully formed.
 * AOpenAudioConn must be sure to initialize all the pointers to NULL
 * before the first possible call on this.
 */

void
_AFreeAudioConnStructure(register AFAudioConn *aud)
{
	if (aud->audioconn_name)
	   Xfree (aud->audioconn_name);
	if (aud->vendor)
	   Xfree (aud->vendor);
        if (aud->buffer)
	   Xfree (aud->buffer);
        if (aud->devices)
	   Xfree (aud->devices);
        if (aud->free_funcs)
	   Xfree (aud->free_funcs);

	while (aud->ext_procs) {
	    _AFExtension *ext = aud->ext_procs;
	    aud->ext_procs = ext->next;
	    if (ext->name)
		Xfree (ext->name);
	    Xfree ((char *)ext);
	}

	_AFreeExtData (aud->ext_data);
        
	Xfree ((char *)aud);
}


