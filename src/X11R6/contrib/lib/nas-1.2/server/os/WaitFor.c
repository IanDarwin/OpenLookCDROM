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
 * $NCDId: @(#)WaitFor.c,v 1.6 1994/05/27 03:10:34 greg Exp $
 */
/***********************************************************
Some portions derived from:

Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*****************************************************************
 * OS Depedent input routines:
 *
 *  WaitForSomething,  GetEvent
 *
 *****************************************************************/

#include <audio/audio.h>
#include <audio/Aproto.h>
#include <audio/Aos.h>

#include <errno.h>
#include <stdio.h>
#include "misc.h"

#ifndef _MINIX
#include <sys/param.h>
#endif /* !_MINIX */
#include <signal.h>

#ifdef _MINIX
#include <assert.h>
#endif /* _MINIX */

#include "osdep.h"
#include "dixstruct.h"
#include "opaque.h"

#ifndef _MINIX
extern long AllSockets[];
extern long AllClients[];
extern long LastSelectMask[];
extern long WellKnownConnections;
extern long ClientsWithInput[];
extern long ClientsWriteBlocked[];
extern long OutputPending[];
extern int ConnectionTranslation[];
#else /* _MINIX */
extern asio_fd_set_t InprogressFdSet;
extern asio_fd_set_t ListenFdSet;
extern asio_fd_set_t ClientFdSet;
extern asio_fd_set_t CompletedFdSet;
extern asio_fd_set_t IgnoreFdSet;
extern asio_fd_set_t GrabFdSet;

extern Bool AnyClientsWithInput;
extern int GrabInProgress;

void EnqueueNewConnection();
void UpdateClientIOStatus();
#endif /* _MINIX */

extern Bool NewOutputPending;
#ifndef _MINIX
extern Bool AnyClientsWriteBlocked;
#endif

extern WorkQueuePtr workQueue;

extern void CheckConnections();
extern Bool EstablishNewConnections();
extern void ResetOsBuffers();

extern int errno;

#ifdef apollo
extern long apInputMask[];

static long LastWriteMask[mskcnt];
#endif

#ifdef XTESTEXT1
/*
 * defined in xtestext1dd.c
 */
extern int playback_on;
#endif /* XTESTEXT1 */

/*****************
 * WaitForSomething:
 *     Make the server suspend until there is
 *	1. data from clients or
 *	2. clients that have buffered replies/events are ready
 *
 *****************/

#if !defined(AMOEBA) || !defined(_MINIX)
int
WaitForSomething(pClientsReady)
    int *pClientsReady;
{
    int i;
    struct timeval waittime, *wt;
    long timeout;
    long clientsReadable[mskcnt];
    long clientsWritable[mskcnt];
    long curclient;
    int selecterr;
    int nready;

    CLEARBITS(clientsReadable);

    /* We need a while loop here to handle 
       crashed connections and the screen saver timeout */
    while (1)
    {
	/* deal with any blocked jobs */
	if (workQueue)
	    ProcessWorkQueue();

	if (ANYSET(ClientsWithInput))
	{
	    COPYBITS(ClientsWithInput, clientsReadable);
	    break;
	}
	wt = NULL;
/* XXX need to have ResetOsBuffers() somewhere */
	COPYBITS(AllSockets, LastSelectMask);
#ifdef apollo
        COPYBITS(apInputMask, LastWriteMask);
#endif
	if (NewOutputPending)
	    FlushAllOutput();
	/* keep this check close to select() call to minimize race */
	if (dispatchException)
	    i = -1;
	else if (AnyClientsWriteBlocked)
	{
	    COPYBITS(ClientsWriteBlocked, clientsWritable);
	    i = select (MAXSOCKS, (fd_set *)LastSelectMask,
			(fd_set *)clientsWritable, (fd_set *) NULL, wt);
	}
	else
#ifdef apollo
	    i = select (MAXSOCKS, (int *)LastSelectMask,
			(fd_set *)LastWriteMask, (fd_set *) NULL, wt);
#else
	    i = select (MAXSOCKS, (fd_set *)LastSelectMask,
			(fd_set *) NULL, (fd_set *) NULL, wt);
#endif
	selecterr = errno;
	if (i <= 0) /* An error or timeout occurred */
	{

	    if (dispatchException)
		return 0;
	    CLEARBITS(clientsWritable);
	    if (i < 0) 
		if (selecterr == EBADF)    /* Some client disconnected */
		{
		    CheckConnections ();
		    if (! ANYSET (AllClients))
			return 0;
		}
		else if (selecterr != EINTR)
		    ErrorF("WaitForSomething(): select: errno=%d\n",
			selecterr);

	    {
		extern pointer AuEventQueue;

		if (AuEventQueue)
		    return 0;
	    }
	}
	else
	{
	    if (AnyClientsWriteBlocked && ANYSET (clientsWritable))
	    {
		NewOutputPending = TRUE;
		ORBITS(OutputPending, clientsWritable, OutputPending);
		UNSETBITS(ClientsWriteBlocked, clientsWritable);
		if (! ANYSET(ClientsWriteBlocked))
		    AnyClientsWriteBlocked = FALSE;
	    }

	    MASKANDSETBITS(clientsReadable, LastSelectMask, AllClients); 
	    if (LastSelectMask[0] & WellKnownConnections) 
		QueueWorkProc(EstablishNewConnections, NULL,
			      (pointer)LastSelectMask[0]);
	    if (ANYSET (clientsReadable))
		break;
	}
    }

    nready = 0;
    if (ANYSET(clientsReadable))
    {
	for (i=0; i<mskcnt; i++)
	{
	    while (clientsReadable[i])
	    {
		curclient = ffs (clientsReadable[i]) - 1;
		pClientsReady[nready++] = 
			ConnectionTranslation[curclient + (i << 5)];
		clientsReadable[i] &= ~(1 << curclient);
	    }
	}	
    }
    return nready;
}

#ifndef ANYSET
/*
 * This is not always a macro.
 */
ANYSET(src)
    long	*src;
{
    int i;

    for (i=0; i<mskcnt; i++)
	if (src[ i ])
	    return (TRUE);
    return (FALSE);
}
#endif

#else /* AMOEBA || _MINIX */
#ifdef AMOEBA
int init_waiters = 0;
semaphore init_sema;

/*
 * Force caller thread to wait until main has
 * finished the initialization
 */
void
WaitForInitialization()
{
    init_waiters++;
    sema_down(&init_sema);
}

static semaphore main_sema;

/*
 * The audio-server consists of one main thread, running the non re-entrant
 * audio code, and a number of auxilary threads that take care of reading
 * the input streams, and input devices. The following set of routines
 * wake up the main thread when it has something to do.
 */
void
InitMainThread()
{
    sema_init(&main_sema, 0);
}

void
WakeUpMainThread()
{
    sema_up(&main_sema);
}

static int
SleepMainThread(timo)
    interval timo;
{

    return (sema_trydown(&main_sema, timo) == 0) ? 0 : -1;
}

int
WaitForSomething(pClientsReady)
    int		*pClientsReady;
{
    register int	i, wt, nt;
    struct timeval	*wtp;
    long        	alwaysCheckForInput[2];
    int 		nready;

    /*
     * First, wakeup threads in initial sleep
     */
    if (init_waiters > 0) {
	while (init_waiters-- > 0)
	    sema_up(&init_sema);
    }

    /*
     * Be sure to check for input on every sweep in the dispatcher.
     * This routine should be in InitInput, but since this is more
     * or less a device dependent routine, and the semantics of it
     * are device independent I decided to put it here.
     */
    alwaysCheckForInput[0] = 0;
    alwaysCheckForInput[1] = 1;
    SetInputCheck(&alwaysCheckForInput[0], &alwaysCheckForInput[1]);

    while (TRUE) {
	/*
	 * Deal with any blocked jobs
	 */
	if (workQueue) ProcessWorkQueue();

	wt = -1;

	/*
	 * Check for new clients. We do this here and not in the
	 * listener() threads because we cannot be sure that dix
	 * is re-entrant, and we need to call some dix routines
	 * during startup.
	 */
	if (nNewConns) EstablishNewConnections();

	/*
	 * Every device driver can install handlers which are called after
	 * a certain amount of time. These handlers implement, for example,
	 * key repeat on a Sun. The following construction will determine
	 * the correct time-out value, given the available handlers. For
	 * compatibility the timeout is a timeval structure.
	 */
	wtp = (struct timeval *) NULL;
	BlockHandler((pointer)&wtp, (pointer)NULL);
	if (wtp) wt = (wtp->tv_sec * 1000) + (wtp->tv_usec / 1000);

	/*
	 * Check for clients needing attention. They might want to
	 * die or they might have input. In the second case, only
	 * accept from the grabClient (if there is one).
	 */
	for (i = 0, nready = 0; i < maxClient; i++) {
	    if (Clients[i] && Clients[i]->osPrivate) {
		register OsCommPtr oc = (OsCommPtr)Clients[i]->osPrivate;
		register int n;

		if (oc->status & CONN_KILLED) {
		    CloseDownClient(Clients[i]);
		    if (maxClient == i) maxClient--;
		    continue;
		}
		if ((n = am_avail(oc, VC_IN)) < 0) {
		    CloseDownClient(Clients[i]);
		    if (maxClient == i) maxClient--;
		    continue;
		}
		if (n > 0 || oc->status & REQ_PUSHBACK) {
		    if (grabClient == NULL || grabClient == Clients[i]) {
			*pClientsReady++ = Clients[i]->index;
			nready++;
		    }
		}
	    }
	}

	/*
	 * Well, if we found some work, or the hardware has
	 * events available, we return.
	 */
	if (nready || AmoebaEventsAvailable()) break;

	/*
	 * Nothing interesting is available. Go to sleep with a
	 * timeout and the other threads will wake us when needed.
	 */
	if (dispatchException) return 0;
	i = SleepMainThread(wt);

	/*
	 * Wake up any of the sleeping handlers
	 */
	WakeupHandler((unsigned long)0, (pointer)NULL);
	if (dispatchException) return 0;

	/*
	 * An error or timeout occurred
	 */
	if (i == -1) return 0;
    }
    return nready;
}
#endif /* AMOEBA */

#ifdef _MINIX
static int timed_fwait();

int
WaitForSomething(pClientsReady)
    int *pClientsReady;
{
    long timeout;
    struct timeval waittime[2];
    int nready;
    struct fwait fw;
    asio_fd_set_t wait_fd_set;
    int i, r;
    int fw_fd, fw_operation, fw_result, fw_errno;

    /* We need a while loop here to handle 
       crashed connections and the screen saver timeout */
    nready= 0;
    while (1)
    {
	/* deal with any blocked jobs */
	if (workQueue)
	    ProcessWorkQueue();

	wait_fd_set= InprogressFdSet;

	/* Let's fill in the fwait structure */
	fw.fw_flags= FWF_MORE;	/* Ignored by the kernel, but makes loop 
				 * termination easier
				 */

	fw.fw_bits= wait_fd_set.afds_bits;
	fw.fw_maxfd= ASIO_FD_SETSIZE;

	if (AnyClientsWithInput)
		fw.fw_flags |= FWF_NONBLOCK;

	BlockHandler((pointer)waittime, (pointer)&wait_fd_set);
	if (NewOutputPending)
	    FlushAllOutput();
	for(;fw.fw_flags & FWF_MORE; fw.fw_flags |= FWF_NONBLOCK)
	{
	    /* keep this check close to select() call to minimize race */
	    if (dispatchException)
	    {
		r = -1;
		errno= EAGAIN;
	    }
	    else if (waittime[0].tv_sec != 0)
	    {
	    	assert(waittime[0].tv_sec >= waittime[1].tv_sec);
		r= timed_fwait(&fw, &waittime[0]);
	    }
	    else
		r= fwait(&fw);
	    if (r == -1)
	    	break;
	    fw_fd= fw.fw_fd;
	    fw_operation= fw.fw_operation;
	    fw_result= fw.fw_result;
	    fw_errno= fw.fw_errno;
	    if (ASIO_FD_ISSET(fw_fd, fw_operation, &ListenFdSet))
	    {
	    	/* Got a new connection */
	    	EnqueueNewConnection(fw_fd, fw_operation, fw_result, fw_errno);
		fw.fw_fd= -1;
	    	continue;
	    }
	    if (ASIO_FD_ISSET(fw_fd, fw_operation, &ClientFdSet))
	    {
	    	/* The read or write of a client is done */
	    	UpdateClientIOStatus(fw_fd, fw_operation, fw_result, fw_errno);
		fw.fw_fd= -1;

		/* Store the client in the ready set if the operation was a
		 * read
		 */
		if (fw_operation == ASIO_READ)
		{
		    pClientsReady[nready++] = ConnectionTranslation[fw_fd];
		}
		continue;
	    }

	    /* Only thing left is a device, let's break out of this loop */
	    break;
	}
	if (r == -1)
	{
	    if (errno != EINTR && errno != EAGAIN)
		    FatalError("WaitForSomething(): fwait error: %s\n",
			strerror(errno));
	    WakeupHandler(0, NULL);	/* Nothing happend to a device */
	}
	else
	{
	    WakeupHandler(0, (pointer)&fw);
	    if (fw.fw_fd != -1)
	    	FatalError("Unable to locate module for completed I/O\n");
	}
	if (r < 0) /* An error or timeout occurred */
	{

	    if (dispatchException)
		return 0;
	    if (*checkForInput[0] != *checkForInput[1])
		return 0;
	}

	break;
    }

    if (AnyClientsWithInput)
    {
    	/* These were left over from the previous round */
    	AnyClientsWithInput= FALSE;
    	for (i= 0; i<ASIO_FD_SETSIZE; i++)
    	{
    		if (ASIO_FD_ISSET(i, ASIO_READ, &CompletedFdSet))
    		{
		    if (GrabInProgress && !ASIO_FD_ISSET(i, ASIO_READ,
								&GrabFdSet))
		    {
		    	continue;
		    }
		    if (ASIO_FD_ISSET(i, ASIO_READ, &IgnoreFdSet))
		    	continue;
		    pClientsReady[nready++] = ConnectionTranslation[i];
		}
	}
    }
    return nready;
}

static int timer_running= FALSE;
static struct timeval timer_value;
static int tf_critical= FALSE;
static int tf_expired= FALSE;

void tf_handler(sig)
int sig;
{
	struct timeval tv;

	tf_expired= TRUE;
	if (!tf_critical)
	{
		timer_running= FALSE;
		return;
	}
	signal(SIGALRM, tf_handler);
	sysutime(UTIME_TIMEOFDAY, &tv);
	tv.tv_sec++;
	/* Fake a high value to force to timer to be reloaded */
	timer_value.tv_sec= tv.tv_sec + 1000000;
	sysutime(UTIME_SETALARM, &tv);
}

static int
timed_fwait(fwp, tvp)
struct fwait *fwp;
struct timeval *tvp;
{
	struct timeval tv;
	int r;

#if DEBUG & 256
 { sysutime(UTIME_TIMEOFDAY, &tv); tv.tv_sec= tvp->tv_sec - tv.tv_sec;
 	tv.tv_usec= tvp->tv_usec - tv.tv_usec; if (tv.tv_usec < 0)
 	{ tv.tv_usec += 1000000; tv.tv_sec--; }
 	ErrorF("timed_fwait: timeout at now+%d.%06d\n", tv.tv_sec, 
 	tv.tv_usec); }
#endif

	/* Assume that only timed_fwait is allowed to let the timer running
	 * between calls.
	 */
	tv= *tvp;
	if (!timer_running)
	{
		signal(SIGALRM, tf_handler);
		sysutime(UTIME_SETALARM, &tv);
		assert(tv.tv_sec == 0);
		timer_value= *tvp;
		timer_running= TRUE;
	}
	else if (tv.tv_sec < timer_value.tv_sec || 
		(tv.tv_sec == timer_value.tv_sec && 
		tv.tv_usec < timer_value.tv_usec))
	{
		/* If the previous timer expires just before we set the 
		 * new timer, the handler is lost unless tf_critical is set,
		 * then the handler will set a new new timer 1 second later.
		 */
		tf_critical= TRUE;
		sysutime(UTIME_SETALARM, &tv);
		timer_value= *tvp;
		tf_critical= FALSE;
	}
	tf_critical= TRUE;
	if (tf_expired)
	{
		tf_critical= FALSE;
		tf_expired= FALSE;
		errno= EINTR;
		return -1;
	}
	r= fwait(fwp);
	tf_critical= FALSE;
	return r;
}
#endif /* _MINIX */
#endif /* AMOEBA || _MINIX */
