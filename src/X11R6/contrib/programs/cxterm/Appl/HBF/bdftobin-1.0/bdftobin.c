/*
 *
 * File:         bdftobin.c
 * Description:  Convert an X11 BDF font to a raw bitmap font
 * Author:       mleisher@nmsu.edu (Mark Leisher)
 * Created:      Tue Feb 26 20:35:20 1991
 * Modified:     
 *
 */

/*
 * Copyright 1990, 1994 by Mark Leisher
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.  I make no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 */

#include <stdio.h>
#include "patchlevel.h"

/*
 * BUFSIZ is defined in stdio.h to be 1024.
 * This program uses this, so if it isn't set,
 * the next pre-processor commands define it.
 */

#ifndef BUFSIZ
#define BUFSIZ 1024
#endif /* BUFSIZ */

char *program;
char buf[BUFSIZ];
int line_no = 0;

void
next_bitmap(in)
FILE *in;
{
    fgets(buf, BUFSIZ, in);
    line_no++;
    while(!feof(in) && strncmp(buf, "BITMAP", 6) != 0) {
        fgets(buf, BUFSIZ, in);
        line_no++;
    }
}

void
genbinfont(in, out)
FILE *in, *out;
{
    char c, *ptr;
    int i;

    next_bitmap(in);
    if (feof(in))
      return;

    while(!feof(in)) {
        fgets(buf, BUFSIZ, in);
        line_no++;
        while(strncmp(buf, "ENDCHAR", 7) != 0) {
            ptr = buf;
            while(ptr != NULL && *ptr != '\0' && *ptr >= 0x30) {
                c = '\0';
                for (i = 0; i < 2; i++) {
                    c *= 16;
                    if (*ptr >= '0' && *ptr <= '9')
                      c += *ptr - '0';
                    else if (*ptr >= 'a' && *ptr <= 'f')
                      c += *ptr - 'a' + 10;
                    else if (*ptr >= 'A' && *ptr <= 'F')
                      c += *ptr - 'A' + 10;
                    else {
                        fprintf(stderr, "%s: bad hex value on line %d\n",
                                program, line_no);
                        if (in != stdin)
                          fclose(in);
                        if (out != stdout)
                          fclose(out);
                        exit(-1);
                    }
                    ptr++;
                }
                fprintf(out, "%c", c);
            }
            fgets(buf, BUFSIZ, in);
            line_no++;
        }
        next_bitmap(in);
    }
}


void
usage()
{
    fprintf(stderr, "usage  %s [bdf_font] [-o output_file_name]\n", program);
    exit(1);
}

main(argc, argv)
int argc;
char **argv;
{
    FILE *infile = stdin;
    FILE *outfile = stdout;
    char *infont = NULL, *outfont = NULL;

    program = argv[0];

    argc--;
    *argv++;
    while(argc) {
        if (argv[0][0] == '-') {
            /*
             * The -o option is for systems that have a limit
             * on the length of file names.  Some fonts have
             * a long name in the BDF file that can't be used
             * on such systems.
             */
            if (argv[0][1] == 'o') {
                argc--;
                *argv++;
                outfont = argv[0];
                if (!(outfile = fopen(outfont, "w+b"))) {
                    fprintf(stderr, "%s: problem with output font %s\n",
                            program, outfont);
                    exit(-1);
                }
            } else
              usage();
        } else {
            infont = argv[0];
            if (!(infile = fopen(infont, "r"))) {
                fprintf(stderr, "%s: problem with input font %s\n", program,
                        infont);
                exit(-1);
            }
        }
        argc--;
        *argv++;
    }
    genbinfont(infile, outfile);
    if (infile != stdin)
      fclose(infile);
    if (outfile != stdout)
      fclose(outfile);
    exit(0);
}
