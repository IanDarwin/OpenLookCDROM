/*
 * svfacsw -- System V Facility Switch
 * Non-X Version.
 * Turns a process started from /etc/init.d on or off
 */

#include <stdio.h>
#define MAIN
#include "svfacsw.h"
 

static int
run_ui()
{
	char buf[1024];

	printf("%s [on,off] ", facility[0].name);
	fgets(buf, sizeof buf, stdin);
	if (strncmp(buf, "on", 2) == 0)
		return start(&facility[0]);
	else if (strncmp(buf, "off", 3) == 0)
		return stop(&facility[0]);
	else if (strncmp(buf, "quit", 4) == 0)
		return -1;
	else {
		fprintf(stderr, "Response %s not understood\n", buf);
		return -1;
	}
}

main(int argc, char **argv)
{
	int done;

	if (chdir("/etc")) {
		perror("omigod: /etc");
		return -42;	/* /etc gone, give back meaning of UNIXverse */
	}

	do {
		done = run_ui();
	} while (!done);
}
