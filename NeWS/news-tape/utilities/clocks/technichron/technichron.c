/*
 * technichron.c
 * David A. LaVallee Sun Microsystems, Inc.
 * cc technichron.c -o technichron -I$NEWSHOME/include -L$NEWSHOME/lib -lcps
 * cc technichron.c -o technichron.4 -I$NEWSHOME/include -L$NEWSHOME/lib-sparc -lcps
 */

#include	<sys/time.h>
#include	"technichron.h"

struct timeval tv;
struct timezone tz;
struct tm *t;

main() {
	ps_open_PostScript();
	ps_initialize();
	ps_flush_PostScript();
	while (!psio_eof(PostScript)) {
		gettimeofday(&tv, &tz);
		t = gmtime(&tv.tv_sec);
		ps_setgmt(t->tm_year, t->tm_yday, t->tm_hour, t->tm_min);
		ps_flush_PostScript();
		sleep(240);
	};
	exit(0);
}

