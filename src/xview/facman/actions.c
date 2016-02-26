/*
 * svfacsw -- System V Facility Switch
 * Turns a process started from /etc/init.d on or off
 */

#include <stdio.h>

#include "svfacsw.h"
 
int start(struct Fac *f)
{
	char buf[1024];
	sprintf(buf, "init.d/%s", f->file);
	link(buf, f->rc23name);
	sprintf(buf, "%s start", f->rc23name);
	printf("Now running command: %s\n", buf);
	return 0;
}

int stop(struct Fac *f)
{
	char buf[1024];
	unlink(f->rc23name);
	sprintf(buf, "%s stop", f->rc23name);
	printf("Now running command: %s\n", buf);
	return 0;
}
