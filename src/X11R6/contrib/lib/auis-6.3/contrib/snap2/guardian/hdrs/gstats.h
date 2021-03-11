
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


/*
 * gstats.h - monitored statistics for guardian.  Packet formats and
 * guardian data structures are defined here.
 *
 */

/*
 * The current (trivial) statistics mechanism must be distinguished by a
 * packet length of 4 bytes
 */
#define GSTAT_VER_1	    1L

#if 0
#define MAX_CHILD_STATUS    32	/* Maximum number of exit status returned */
#define MAX_SIGNALS	    32  /* NSIG in <signal.h> */

typedef struct error {
    unsigned long	index;		/* which number error is this */
    unsigned long	timestamp;	/* time that error occured */
    unsigned long	error_code;	/* numeric error code (described below) */
    char		string[64];	/* english text description */
} error_t;
#endif /* 0 */

/*
 * statistics packet, encapsulated in a UDP datagram.
 *
 * Assume:
 *  sizeof (unsigned char)   == 8 bits
 *  sizeof (unsigned short)  == 16 bits
 *  sizeof (unsigned long)   == 32 bits
 * All in network byte order.
 */

typedef struct stat_packet {
    /* stats */
    unsigned long	gstat_version;
    unsigned long	timestamp;	    /* time packet was transmitted */
    unsigned long	ipAddress;	    /* ip address of server */
    unsigned long	time_up;	    /* uptime of guardian process */
    unsigned long	servers;	    /* number of child servers */
    unsigned long	max_servers;	    /* max number of child servers */
    unsigned long	users;		    /* number of users */
    unsigned long	max_users;	    /* max number of users */
    unsigned long	load_avg[3];	    /* Load avg: 1, 5, 10 min avg */

    /* guardian state */
    char		g_version[32];	    /* Guardian version as a string */
    unsigned char	vice_auth;	    /* boolean: use vice authentication? */
    unsigned char	accept_remote_req;  /* boolean */
    unsigned char	permits_file;	    /* boolean: using a user.permits file */
    unsigned char	run_as_user;	    /* booolean: run as a user or root */
    unsigned char	verbose;	    /* boolean: verbose error messages? */
    unsigned char	unused[3];	    /* pad to a 32 bit boundary */
    unsigned long	debug_flags;	    /* debug level */
    unsigned long	reboot_time;	    /* time to (until ?) reboot */
    
#if 0
    /* error logging */
    unsigned long	child_status[MAX_CHILD_STATUS]; /* # of returns with this status */
    unsigned long	child_term_status[MAX_SIGNALS];
    unsigned long	error_count;
    error_t		last_errors[10];
#endif /* 0 */
    
} stat_packet_t;

/* Error codes */
