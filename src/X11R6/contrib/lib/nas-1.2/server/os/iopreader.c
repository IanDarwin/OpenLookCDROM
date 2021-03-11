#ifdef AMOEBA
/*
 * iopreader.c
 *
 * $XFree86: mit/server/os/iopreader.c,v 1.1 1993/03/20 04:31:22 dawes Exp $
 *
 */
#include <amoeba.h>
#include <ampolicy.h>
#include <cmdreg.h>
#include <stdcom.h>
#include <stderr.h>
#include <server/iop/iop.h>

#include "osdep.h"

#define	MAXEVENTQUEUE	32

capability iopcap;

static mutex lock;
static semaphore empty, filled;

static IOPEvent event_queue[MAXEVENTQUEUE];
static int event_qin, event_qout;

void IOPCleanUp();
static void IOPServerReader();

/*
 * Initialize the IOP server
 */
void
InitializeIOPServerReader()
{
    char		host[100];
    errstat		err;

    /*
     * Initialize event queue
     */
    event_qin = event_qout = 0;
    sema_init(&empty, MAXEVENTQUEUE);
    sema_init(&filled, 0);
    mu_init(&lock);

    /* 
     * Get IOP capability, and enable the server
     */
    if (AuServerHostName == NULL)
	FatalError("No hostname, no screen\n");
    sprintf(host, "%s/%s/%s", HOST_DIR, AuServerHostName, DEF_IOPSVRNAME);
    if ((err = name_lookup(host, &iopcap)) != STD_OK)
	FatalError("Cannot find IOP server %s: %s\n", host, err_why(err));

    /*
     * Enable IOP server
     */
    if ((err = iop_enable(&iopcap)) != STD_OK)
	FatalError("iop_enable failed (%s)\n", err_why(err));

    /*
     * Start IOP reader thread
     */
    atexit(IOPCleanUp);
    if (thread_newthread(IOPServerReader, DEVREADER_STACK, 0, 0) <= 0)
	FatalError("Cannot start IOP reader thread\n");
}

/*
 * IOP clean up, actuall disable the IOP server. Its the IOP's own choice
 * what do do (perhaps restore the screen?).
 */
void
IOPCleanUp()
{
    errstat err;

    if ((err = iop_disable(&iopcap)) != STD_OK)
	ErrorF("iop_disable failed (%s)\n", err_why(err));
}

/*
 * This threads polls the IOP server for events. Once an event (or a
 * number of events) are read, they are queued up using a traditional
 * producer/consumer approach.
 */
static void
IOPServerReader()
{
    IOPEvent		queue[MAXEVENTQUEUE-1];
    int			nevents, i;
    errstat		err;

    WaitForInitialization();

#ifdef XDEBUG
    if (amDebug) ErrorF("IOPServerReader() running ...\n");
#endif

    for (;;) {
	do {
	    nevents = MAXEVENTQUEUE - 1;
	    err = iop_getevents(&iopcap, queue, &nevents);
	    if (err != STD_OK) {
	        if (err != RPC_FAILURE) {
		    ErrorF("iop_getevents failed (%s)\n", err_why(err));
		}
		nevents = 0;
	    }
	} while (nevents <= 0);

	/* store event(s) in the global event queue */
	sema_mdown(&empty, nevents);
	mu_lock(&lock);
	for (i = 0; i < nevents; i++) {
	    event_queue[event_qin] = queue[i];
	    event_qin = (event_qin + 1) % MAXEVENTQUEUE;
	}
	mu_unlock(&lock);
	sema_mup(&filled, nevents);
	WakeUpMainThread();
    }
}

/*
 * Return the number of IOP events waiting
 */
int
AmoebaEventsAvailable()
{
    return sema_level(&filled);
}

/*
 * Get the IOP events from the queue. ``size'' is the maximum the
 * requestor cares to handle, the actual size read is returned as
 * result.
 */
int
AmoebaGetEvents(queue, size)
    IOPEvent	*queue;
    int		size;
{
    int		nevents, i;

    if (sema_level(&filled) <= 0) return 0;
    if ((nevents = sema_level(&filled)) > size)
	nevents = size;
    sema_mdown(&filled, nevents);
    mu_lock(&lock);
    for (i = 0; i < nevents; i++) {
	queue[i] = event_queue[event_qout];
	event_qout = (event_qout + 1) % MAXEVENTQUEUE;
    }
    mu_unlock(&lock);
    sema_mup(&empty, nevents);
    return nevents;
}
#endif /* AMOEBA */

