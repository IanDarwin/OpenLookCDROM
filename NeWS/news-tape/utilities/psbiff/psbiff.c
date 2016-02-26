#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <time.h>
#include "psbiff.h"

char mailbox[80];
struct passwd *pw;
struct stat st;

main(argc, argv)
int argc;
char **argv;
{
    long t;
    int x, y;

    if (argc < 3) {
	fprintf(stderr, "usage: psbiff x y");
	exit(1);
    }
    x = atoi(argv[1]);
    y = atoi(argv[2]);

    ps_open_PostScript();
    initialize(x, y);
    ps_flush_PostScript();
    pw = getpwuid(getuid());
    sprintf(mailbox, "/usr/spool/mail/%s", pw->pw_name);

    for (;;) {
	time(&t);
	sleep(10);
	if (stat(mailbox, &st)) {
	    nomail();
	} else if (t <= st.st_mtime) {
	    newmail();
	} else if (st.st_size > 0) oldmail();
	else nomail();
	ps_flush_PostScript();
    }
    ps_close_PostScript();
}
