/*
 * @(#)ftoa.c 1.2 87/11/10 SMI
 *
 * Floating-point to ASCII conversion program
 */

#include <stdio.h>

static char	*prog;		/* program name from argv[0] */

extern int optind;

usage()
{
    fprintf(stderr,"Convert floating-point to ascii -- usage:\n");
    fprintf(stderr,"\t%s [filelist]\nwhere:\n", prog);
    fprintf(stderr,"\tfilelist\tlist of input files (if NULL, read stdin)\n");
    exit(1);
}

/*
 * ftoa [files or stdin]
 * Convert double to ascii
 *
 * Writes ascii data "%.8G\n" to stdout.
 */
main(argc, argv)
	int	argc;
	char	*argv[];
{
	register int c;
	FILE	*f;
	int	error;

	prog = argv[0];
	while ((c = getopt(argc, argv, "")) != EOF) {
		switch (c) {
		case '?':
		default :
			usage();
		}
	}

	argc -= optind;
	argv += optind;
	error = 0;

	if (argc == 0) {
		error = ftoa(stdin, stdout, "stdin");
	}
	else for (c = 0; c < argc; c++) {
		f = fopen(argv[c], "r");
		if (f != NULL) {
			error += ftoa(f, stdout, argv[c]);
			fclose(f);
		} else {
			fprintf(stderr, "%s: cannot open ", prog);
			perror(argv[c]);
			error++;
		}
	}
	exit(error);
}

int
ftoa(from, to, file)
	FILE	*from;
	FILE	*to;
	char	*file;
{
	double	x;

	while (fread(&x, sizeof (x), 1, from) == 1)
		fprintf(to, "%s%.8G\n", (x >= 0.) ? " " : "", x);

	if (ferror(from)) {
		fprintf(stderr, "%s: read error in ", prog);
		perror(file);
		return (1);
	} else
		return (0);
}
