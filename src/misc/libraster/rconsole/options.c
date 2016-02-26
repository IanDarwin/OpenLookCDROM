/*
 * rconsole
 * Copyright 1990 Tom Lawrence, Brown University
 * Last Modification 6/14/90
 */

#include <sys/types.h>
#include <stdio.h>

#include "global.h"

/*****************************************************************
*****************************************************************/
void
usage()
{
	printf("\nOptions:\n\n");
	printf("-e <executable [options]>  Run executable in rconsole.\n");
	printf("    This must be the last option on the line.\n");
	printf("-f <fontname>  Use given font. Default is gallant19.\n");
	printf("-fb <framebuffer>  Use the given frame buffer.\n");
}

/*****************************************************************
parse command line options.
*****************************************************************/
void
options(argc, argv)
	int argc;
	char **argv;
{

	EXEC = 0;
	FB = "/dev/fb";
	debug = 0;

	while (--argc) {
		argv++;
		if (!strcmp(*argv, "-e")) {
			if (argc > 1) {
				EXEC = &(argv[1]);
				break;
			} else {
				usage();
				exit(1);
			}
		} else if (!strcmp(*argv, "-fb")) {
			if (argc > 1) {
				FB = argv[1];
			} else {
				usage();
				exit(1);
			}
			argc--;
			argv++;
		} else if (!strcmp(*argv, "-d")) {
			++debug;
		} else {
			usage();
			exit(1);
		}
	}
}
