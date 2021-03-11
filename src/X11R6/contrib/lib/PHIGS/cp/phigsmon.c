/* $XConsortium: phigsmon.c,v 5.10 94/04/17 20:41:29 eswu Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include "sin.h"
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#ifndef PEX_API_SOCKET_IPC
#include <sys/ipc.h>
#include <sys/shm.h>
#endif
#include <sys/socket.h>
#include <fcntl.h>
#include <signal.h>
#ifdef AIXV3
#include <sys/ioctl.h>
#endif

#define CPR_CMD_TIMEOUT		250000

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char* getenv();
#endif

/* The CP handle for this invocation. */
static Cp_handle	phigs_cph;

/* Interval values are {interval, initial} */
static struct itimerval	check_parent_interval = { 2, 0, 2, 0};
static struct timeval	cmd_check_interval = { 0, 100 };

#ifndef PEX_API_SOCKET_IPC
static int
attach_shared_mem( cph )
    Cp_handle	cph;
{
    /* Take pains not to leave shared memory segments lying around:
     * The shared memory segment is "removed" by the parent after it
     * attaches to it.  Once "removed," the segment will go away when all
     * processes attached to it terminate.  Until then, processes attached
     * to it can use it even though it's been "removed."  If not removed,
     * it exists forever, even after all the processes using it die.
     *
     * The segment is created here because getting as far as this in the
     * program indicates that successful startup is likely.  We want to
     * create the segment when we know it's likely we can destroy it
     * normally.  Otherwise it might be left around.
     */

    int		shmid, status = 0;
    extern int	errno;	/* kernel reports here why system call failed */

    shmid = shmget( IPC_PRIVATE, sizeof(Cp_shm_buf), 0666 );
    if ( shmid < 0 ) {
	switch (errno) {
	case EINVAL:
	    ERR_BUF( cph->erh, ERRN57);
	    break;
	case ENOSPC:
	    /* IPCSHMEM facility is configured into the kernel,
	     * but shared memory resources are exhaused.
	     * This error is unlikely, so a specific error message
	     * is overkill.
	     *
	     * The parent process will raise the error -1:
	     * "cannot create communication channel".
	     */
	    break;
	default:
#ifdef	DEBUG
	    fprintf(stderr, "PHIGSMON: errno is %d after shmget\n", errno);
#endif	/* DEBUG */
	    ERR_BUF( cph->erh, ERRN50);
	    break;
	}
    } else {
	extern int errno;	/* kernel reports here why system call failed */
	assure(shmid >= 0)
	cph->shm_buf = (Cp_shm_buf *)shmat( shmid, (char *)NULL, 0 );
	if ( cph->shm_buf == (Cp_shm_buf *)-1 ) {
#ifdef	DEBUG
	    fprintf(stderr, "PHIGSMON: errno is %d after shmat\n", errno);
#endif	/* DEBUG */
	    /* Assign a more specific error number to this error,
	     * if it is ever observed.
	     */
	    ERR_BUF( cph->erh, ERRN50);
	    (void)shmctl( shmid, IPC_RMID, (struct shmid_ds *)NULL );
	    cph->shm_buf = (Cp_shm_buf *)NULL;
	} else {
	    cph->shm_buf->shmid = shmid;
	    cph->shm_buf->args.lock = 0;
	    cph->shm_buf->args.head = 0;
	    cph->shm_buf->args.tail = 0;
	    cph->shm_buf->args.buf_size = CP_SHM_ARGS_BUF_SIZE;

	    cph->shm_buf->data.lock = 0;
	    cph->shm_buf->data.head = 0;
	    cph->shm_buf->data.tail = 0;
	    cph->shm_buf->data.buf_size = sizeof(cph->shm_buf->data.buf);

	    cph->shm_buf->ret.buf_size = sizeof(cph->shm_buf->ret.buf);
	    status = 1;
#ifdef	DEBUG
	    CP_CHECK_SHM_LOCK(cph->shm_buf,args)
	    if (getenv("PHIGSMON_WAIT") != NULL)
		fprintf(stderr, "PHIGSMON: shmid is %d; shm_buf is %#x\n",
					shmid, cph->shm_buf);
#endif	/* DEBUG */
	}
    }

    return status;
}


#else /* PEX_API_SOCKET_IPC defined */
static Cp_shm_buf	dummy_shm_buf;

static int
dummy_attach( cph )
    Cp_handle	cph;
{
    /* Shared memory not used, just attach a small fake buffer with control
     * values set to force all data to go through the socket.
     */
    cph->shm_buf = &dummy_shm_buf;
    cph->data.monitor.arg_buf = dummy_shm_buf.args.buf;
    cph->data.monitor.ret_buf = &dummy_shm_buf.ret.data;
    /* Zero buffer sizes force the data to be sent through the socket. */
    cph->shm_buf->args.buf_size = 0;
    cph->shm_buf->data.buf_size = 0;
    cph->shm_buf->ret.buf_size = 0;
    return 1;
}
#endif /* ! PEX_API_SOCKET_IPC */

static Cp_handle
set_up( s, es)
    int		s, es;
{
    register Cp_handle	cph = NULL, tcph;
    Cpx_css_srvr	*css_srvr;

    /* Don't bother cleaning up on failure, the process will just exit. */
    if ( tcph = (Cp_handle)calloc( 1, sizeof(Cp_struct))) {
	tcph->configuration = CP_REMOTE;
	if ( !(tcph->erh = phg_err_init_remote( es))) {
	    ;

	} else if ( !(tcph->data.monitor.infile = phg_cpr_fdopen( s))) {
	    ERR_BUF( tcph->erh, ERR900);

	} else if ( !(css_srvr = phg_cpb_pm_init( tcph )) ) {
	    ; /* Any errors will have been reported already. */

	} else if ( !(tcph->input_q = phg_sin_q_create( tcph->erh)) ) {
	    ;

#ifndef PEX_API_SOCKET_IPC
	} else if ( !attach_shared_mem( tcph ) ) {
#else
	} else if ( !dummy_attach( tcph ) ) {
#endif
	    ;

	} else {
	    tcph->scratch.buf = NULL;
	    tcph->scratch.size = 0;

	    tcph->flags.in_monitor = 1;
	    tcph->data.monitor.sfd = s;
	    tcph->data.monitor.cmd_state = CP_READING_ARGS;
	    tcph->data.monitor.cur_cnt = 0;
	    tcph->data.monitor.parent_pid = -1;
	    tcph->data.monitor.cmd_timeout.it_value.tv_sec = 0;
	    tcph->data.monitor.cmd_timeout.it_value.tv_usec = CPR_CMD_TIMEOUT;
	    tcph->data.monitor.cmd_timeout.it_interval.tv_sec = 0;
	    tcph->data.monitor.cmd_timeout.it_interval.tv_usec =
		CPR_CMD_TIMEOUT;

#ifdef AIXV3
	    /* 
	     * AIX has a different Non-blocking IO call
	     */	
            {
                int fdflags = 1;
                ioctl (s, FIONBIO, &fdflags);
            }
#else /* AIXV3 */
	    /* ultrix reads hang on Unix sockets, hpux reads fail */
#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux))
	    (void)fcntl(s, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
	    {
		int fdflags = 1;
		ioctl (s, FIOSNBIO, &fdflags);
	    }
#else
	    (void)fcntl(s, F_SETFL, FNDELAY);
#endif
#endif
#endif /* AIXV3 */
	    if ( getenv( "PEX_SI_API_SYNC" ) )
		tcph->flags.err_sync = 1;

	    phg_cpm_load_monitor_funcs( tcph );
	    phg_cpx_link_css_srvr( tcph, css_srvr );
	    cph = tcph;
	}
    }

    return cph;
}


static void
check_parent( cphp, which )
Cp_handle	*cphp;
int		which;
{
    if ( kill( (*cphp)->data.monitor.parent_pid, 0) < 0 && errno == ESRCH) {
#ifndef PEX_API_SOCKET_IPC
	/* Make sure the shared  memory segment is removed.  This is only
	 * necessary when the parent dies before it removes the segment,
	 * but do it all the time since we can't catch that case and the
	 * extra call doesn't hurt anything.
	 */
	(void)shmctl( (*cphp)->shm_buf->shmid, IPC_RMID,
	    (struct shmid_ds *)NULL );
#endif
	exit(1);
    }
    return;
}


static Cp_handle
start_socketpair()
{
    char		msg = 'p';

    if ( (phigs_cph = set_up( fileno(stdin), fileno(stdout)))) {
	phigs_cph->data.monitor.parent_pid = getppid();

	/* Let the parent know we started okay and send the shared mem id. */
	send( phigs_cph->data.monitor.sfd, &msg, sizeof(msg), 0);
#ifndef PEX_API_SOCKET_IPC
	send( phigs_cph->data.monitor.sfd, (char *)&phigs_cph->shm_buf->shmid,
	    sizeof(phigs_cph->shm_buf->shmid), 0 );
#endif

	/* Register miscellaneous notify procs. */
	phg_register_timer_func((unsigned long)&phigs_cph, check_parent,
	    ITIMER_REAL,
	    (unsigned long)(check_parent_interval.it_interval.tv_sec
		* 1000000 + check_parent_interval.it_interval.tv_usec));
    } else {
	msg = 'e';
	send( fileno(stdin), &msg, sizeof(msg), 0);
	exit(2);
    }

    return phigs_cph;
}


#ifdef DIAGNOSTIC
#define PRINT( _s, _w ) fprintf( stderr, "%s, window %d\n", _s, _w );

static void
print_X_event( event )
    XEvent	*event;
{
    fprintf( stderr, "event type %d\n", event->type );
    switch ( event->type ) {
	case MapNotify:
	    PRINT( "MapNotify", event->xmap.window );
	    break;
	case ConfigureNotify:
	    PRINT( "ConfigureNotify", event->xconfigure.window );
	    break;
	case ReparentNotify:
	    PRINT( "ReparentNotify", event->xreparent.window );
	    break;
	case Expose:
	    PRINT( "Expose", event->xexpose.window );
	    break;
	case MappingNotify:
	    PRINT( "MappingNotify", event->xmapping.window );
	    break;
    }
}
#endif /* DIAGNOSTIC */

void
phg_check_and_dispatch_event( cph )
    Cp_handle	cph;
{
    XEvent	event;

    register Cp_display_connection	*dcon;
    
    CPX_FOR_ALL_DISPLAYS(cph, dcon) {
	if ( dcon->display ) {
	    while ( XPending(dcon->display) ) {
		XNextEvent( dcon->display, &event );
#ifdef DIAGNOSTIC
		print_X_event( &event );
#endif /* DIAGNOSTIC */
		XtDispatchEvent( &event );
		phg_ntfy_dispatch_event( dcon->display, &event );
	    }
	}
    }
}


#ifdef DEBUG
		int		phigsmon_wait = 1;
    extern	int		cpr_print_trace;


#define CP_SET_MALLOC_DEBUG_LEVEL \
    { \
	char	*debug_level; \
	if ( debug_level = getenv("PHIGSMON_MALLOC_LEVEL") ) \
	    malloc_debug( atoi(debug_level) ); \
    }
#else
#define CP_SET_MALLOC_DEBUG_LEVEL	{}
#endif /* DEBUG */


extern XtAppContext	phg_cpm_init_toolkit();

#ifdef DEBUG
#define DEBUG_PHIGSMON_SOCKET_COMMAND "/tmp/phigs_command"
#define DEBUG_PHIGSMON_SOCKET_ERROR "/tmp/phigs_error"
#include <sys/un.h>
#endif

main(argc, argv)
    int		argc;
    char	*argv[];
{
    Cp_handle		cph;
    char		*env;
    int			(*rcv_cmd)();
    XtAppContext	app_con;
    

#ifdef DEBUG
#ifdef BSD44SOCKETS
#define Make(rendezvous,addr) {\
	struct sockaddr_un	un_addr; \
 \
	unlink (addr); \
	strcpy (un_addr.sun_path, addr); \
	un_addr.sun_len = strlen(un_addr.sun_path); \
	un_addr.sun_family = AF_UNIX; \
	rendezvous = socket (AF_UNIX, SOCK_STREAM, 0); \
	if (bind (rendezvous, &un_addr, SUN_LEN(&un_addr)) == -1) \
	{ \
	    perror ("phigs debug bind"); \
	    abort (); \
	} \
	listen (rendezvous, 5); \
}
#else
#define Make(rendezvous,addr) {\
	struct sockaddr_un	un_addr; \
 \
	unlink (addr); \
	strcpy (un_addr.sun_path, addr); \
	un_addr.sun_family = AF_UNIX; \
	rendezvous = socket (AF_UNIX, SOCK_STREAM, 0); \
	if (bind (rendezvous, &un_addr, sizeof (short) + strlen (addr)) == -1) \
	{ \
	    perror ("phigs debug bind"); \
	    abort (); \
	} \
	listen (rendezvous, 5); \
}
#endif

#define Get(fd,rendezvous) { \
	struct sockaddr_un	un_addr; \
	int			addr_len; \
	\
	addr_len = sizeof (un_addr); \
	fd = accept (rendezvous, &un_addr, &addr_len); \
	if (fd == -1) {\
	    perror ("phigs debug accept"); \
	    abort (); \
	}\
}
    {
	int	r_command, s_command;
	int	r_error, s_error;
	int	pid;

	Make (r_command, DEBUG_PHIGSMON_SOCKET_COMMAND);
	Make (r_error, DEBUG_PHIGSMON_SOCKET_ERROR);
	Get (s_command, r_command);
	Get (s_error, r_error);
	pid = getpid ();
	write (s_command, &pid, sizeof (pid));
	if (dup2 (s_command, 0) < 0) {
	    perror ("phigs debug dup command");
	    abort ();
	}
	if (dup2 (s_error, 1) < 0) {
	    perror ("phigs debug dup error");
	    abort ();
	}
    }
#endif
#ifdef SUN_DEBUG
    {
	char	*trace_val;

	if ( (env = getenv("PHIGSMON_CMD_INTERVAL")) != NULL) {
	    cmd_check_interval.tv_usec = atoi(env);
	}

	if ( (trace_val = (char*)getenv("PHIGSMON_CP_TRACE")) != NULL ) {
	    cpr_print_trace = atoi( trace_val );
	}

	/* Wait for debugger to attach to child. */
	if (getenv("PHIGSMON_WAIT") != NULL) {
	    while (phigsmon_wait)
	    /* Attach a debugger, and then
	       set phigsmon_wait=0
	     * to continue.
	     */
		if (kill( getppid(), 0) < 0 && errno == ESRCH) {
		    fprintf(stderr,
			"PHIGSMON_WAIT: parent (%d) of phigsmon %d died\n",
			getppid(), getpid());
		    exit(3);
		} else
		    CP_MICROSLEEP( 100 ); /* sleep(1) won't return on sun4 */
	}
    }
#endif /* DEBUG */

    if ( !(app_con = phg_cpm_init_toolkit( argc, argv )) )
	exit(4);

    /* Initialize everything and link to parent. */
    cph = start_socketpair();
    BITSET(cph->fd_masks, cph->data.monitor.sfd);
    cph->max_fd = cph->data.monitor.sfd;
    cph->data.monitor.app_con = app_con;
    cph->data.monitor.name = argv[1];
    cph->data.monitor.classname = argv[2];
    cph->data.monitor.argc = argc - 2;
    cph->data.monitor.argv = &argv[3];

#ifdef DEBUG
    if ( (env = getenv("PHIGSMON_CMD_TIMEOUT")) != NULL) {
	cph->data.monitor.cmd_timeout.it_value.tv_usec = atoi(env);
	cph->data.monitor.cmd_timeout.it_interval.tv_usec = atoi(env);
    }
#endif

#ifndef PEX_API_SOCKET_IPC
    rcv_cmd = phg_cpr_rcv_cmd_shm;
#else
    rcv_cmd = phg_cpr_rcv_cmd_socket;
#endif
    while (1) {
	unsigned long	rmask[MSKCNT];

	check_parent( &cph, 0 );
	phg_check_and_dispatch_event(cph);

	if ((*rcv_cmd)(cph) == 0) {	    
	    /* No commands pending from client.  Check for events then
	     * sleep a while.
	     */
	    check_parent( &cph, 0 );
	    phg_check_and_dispatch_event(cph);

	    /* Block until interesting input available. */
	    COPYBITS(cph->fd_masks,rmask);
#ifndef PEX_API_SOCKET_IPC
	    /* Need to wake up frequently to check the shared memory queue
	     * for commands from the parent.
	     */
	    select( cph->max_fd+1, rmask, NULL, NULL, &cmd_check_interval );
#else
	    select( cph->max_fd+1, rmask, NULL, NULL, NULL );
#endif
	}
    }
}
