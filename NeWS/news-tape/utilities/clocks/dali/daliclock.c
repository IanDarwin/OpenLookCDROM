/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */


#ifndef lint
static char sccsid[] = "@(#)roundclock.c 9.5 88/02/10 Copyright 1987 Sun Micro";
#endif
/*
 * NeWS clock program 
 */

#include <stdio.h>
#ifdef SYSVREF
#include <stropts.h>
#include <poll.h>
#include <sys/types.h>
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <sys/ioctl.h>
#include "daliclock.h"
#include <signal.h>
#include "psio.h"

int    fancy_clock = 0;

main (argc, argv)
    char  **argv;
{
    int     show_seconds = 0;
    int	    damaged;
    int	    second_hand_length = 38;
    int	    minute_hand_length = 35;
    int	    hour_hand_length = 20;

    int     lmin = -1,
            lhour = -1,
            lsec = -1;
    while (--argc > 0) {
	if ((++argv)[0][0] == '-')
	    switch (argv[0][1]) {
	      case 'f':
		fancy_clock = 1;
		break;
	      case 's':
		show_seconds = 1;
		break;
	      default:
		fprintf (stderr, "roundclock: illegal option: %s\n", argv[0]);
		exit (-1);
	    }
	else {
	    fprintf (stderr, "roundclock: illegal option: %s\n", argv[0]);
	    exit (-1);
	}
    }
    if (ps_open_PostScript () == 0) {
	fprintf (stderr, "No NeWS server\n");
	exit (-1);
    }
    if (fancy_clock) {
	second_hand_length = 10;
	minute_hand_length = 35;
	hour_hand_length = 20;
	ps_fancy_initializeclock();
    } else {
	ps_initializeclock();
    }
    ps_createclock ();	/* initialize round clock window */
    while (1) {
	register struct tm *tm;
#ifdef SYSVREF
	time_t	now;
#else
	long    now;
#endif

	now  = time (0);
	if (damaged)	/* Redraw the clock face if necessary */
	    ps_redrawclock ();
	tm = localtime (&now);
	ps_white ();	/* Clear out the old hands */
	tm->tm_hour = tm->tm_hour * 5 + tm->tm_min / 12;
	if (lmin >= 0) {
	    if (tm->tm_min != lmin)
		hand (lmin, minute_hand_length);
	    if (show_seconds && tm->tm_sec != lsec)
		hand (lsec, second_hand_length);
	    if (tm->tm_hour != lhour)
		hand (lhour, hour_hand_length);
	}
	ps_black ();	/* draw the new hands */
	if (show_seconds)
	    hand (tm->tm_sec, second_hand_length);
	hand (tm->tm_min, minute_hand_length);
	hand (tm->tm_hour, hour_hand_length);
	lsec = tm->tm_sec;
	lmin = tm->tm_min;
	lhour = tm->tm_hour;
	if (damaged) {	/* Redraw the focus ring if necessary */
	    ps_redrawfocus ();
	    damaged = 0;
	}
	ps_flush_PostScript ();
	{	/* Wait for either the next clock tick or a window damage
		 * repair request */
	    int     n;
#ifdef SYSVREF
	    struct pollfd	msk[1];
	    static int			t = 0;
#else
	    int     msk = 1 << psio_fileno (PostScriptInput);
	    static struct timeval t;
#endif

#ifdef SYSVREF
	    msk[0].fd = psio_fileno (PostScriptInput);
	    msk[0].events = POLLIN;
#endif

#ifdef SYSVREF
	    now = (show_seconds ? 1 : 60 - tm->tm_sec) * 1000;
	    t = now > 60000 ? 60000 : now;
	    if (poll(msk, 1, t) > 0)
#else
	    now = show_seconds ? 1 : 60 - tm->tm_sec;
	    t.tv_sec = now > 60 ? 60 : now;
	    if (select (32, &msk, 0, 0, &t) > 0)
#endif
	    {
		char    buf[1000];
		n = read (psio_fileno (PostScriptInput), buf, sizeof buf);
		if (n > 0)	/* The only input the clock ever gets is a
				 * damage repair request */
		    damaged++;
		else if (n == 0)
		    exit (0);
		else
		    perror ("read");
	    }
	}
    }
}

hand (angle,radius)
    int angle, radius;
{
    if (fancy_clock) 
	ps_fancy_hand(-angle*6, radius);
    else
	ps_hand(-angle*6, radius);
}
