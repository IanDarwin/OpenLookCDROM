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
#include "glass.h"
#include <signal.h>
#include "psio.h"


main (argc, argv)
    char  **argv;
{
    int     show_seconds = 0;

    int     lmin = -1,
            lhour = -1,
            lsec = -1;
    while (--argc > 0) {
	if ((++argv)[0][0] == '-')
	    switch (argv[0][1]) {
	      case 's':
		show_seconds = 1;
		break;
	      default:
		fprintf (stderr, "glassclock: illegal option: %s\n", argv[0]);
		exit (-1);
	    }
	else {
	    fprintf (stderr, "glassclock: illegal option: %s\n", argv[0]);
	    exit (-1);
	}
    }
    if (ps_open_PostScript () == 0) {
	fprintf (stderr, "No NeWS server\n");
	exit (-1);
    }
    ps_createclock ();	/* initialize glass clock window */
    while (1) {
	register struct tm *tm;
#ifdef SYSVREF
	time_t	now;
#else
	long    now;
#endif

	now  = time (0);
	tm = localtime (&now);
	tm->tm_hour = tm->tm_hour * 5 + tm->tm_min / 12;
	    if (tm->tm_min != lmin ||
                (show_seconds && tm->tm_sec != lsec) ||
                tm->tm_hour != lhour)
               ps_update (tm->tm_hour,tm->tm_min,tm->tm_sec,show_seconds);
	lsec = tm->tm_sec;
	lmin = tm->tm_min;
	lhour = tm->tm_hour;
	ps_flush_PostScript ();
	{
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
		if (n == 0)
		    exit (0);
		else
		    perror ("read");
	    }
	}
    }
}
