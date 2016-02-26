/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

void
ice_err(char *msg, int severity)
{
	int val;

	if (init_complete) {
		if (alert_prompt(progname, dpy, &val,
				LXA_LABEL, msg,
				LXA_BUTTON, "Continue", 0,
				LXA_NULL) == LX_ERROR) {
			(void) fprintf(stderr, "%s: %s\n", progname, msg);
			(void) fflush(stderr);
			return;
		}
	}
	else {
		(void) fprintf(stderr, "%s: %s\n", progname, msg);
		(void) fflush(stderr);
	}

	switch (severity) {
	case FATAL:
		exit(-1);
		break;
	case NONFATAL:
	default:
		break;
	}

	return;
}

void
ice_usage()
{
	(void) fprintf(stderr, "usage: %s [ -display display ] [ -w width ] [ -h height ] [ -dpi resolution ] [ -v level ] [ icefile ] [ -H ] [ -ps < psfile ]\n", progname);
	(void) fflush(stderr);
	exit(-1);
}
