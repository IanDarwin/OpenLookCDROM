/*
 * et2hku -- convert between ETen BIG5 coding and HKU BIG5 coding (defined by hku-ch16)
 *
 * Author:	Yongguang Zhang (ygz@cs.purdue.edu)
 *
 * Modified:	Mark Leisher (mleisher@nmsu.edu)
 *		Thu May 23 02:58:22 1991
 *
 *		Added ability to specify input and/or output file.
 *		Added MSDOS macros.
 *
 * Usage:
 *		input from stdin, output to stdout.
 *		ET  Big5 => HKU Big5:	et2hku -h [-o output_file] [input_file]
 *		HKU Big5 => ET  Big5:	et2hku -e [-o output_file] [input_file]
 *
 * Usage summary:
 *		-h   convert from ETEN->HKU format (default).
 *
 *		-r   convert from HKU->ETEN format (reverse).
 *
 *		-e   convert from HKU->ETEN format
 *
 *		-o   send output to ``output_file''
 *
 *		If ``input_file'' is not specified, then input will be
 *		taken from standard input (stdin).
 *
 *		If ``-o output_file'' is not specified, then output will
 *		be to standard output (stdout).
 *
 */

#include <stdio.h>

/* follow the convention of b2g/HCu, unconverted hanzi is replaced by a box */

#define UNCONV_H	0xa1
#define UNCONV_L	0xbc

#ifdef __MSDOS__
#define IN_MODE "rb"
#define OUT_MODE "wb"
#define PATH_SEP '\\'
#else /* !__MSDOS__ */
#define IN_MODE "r"
#define OUT_MODE "w"
#define PATH_SEP '/'
#endif /* !__MSDOS__ */

#define E2B 0
#define B2E 1

#ifndef DEFAULT_CONV
#define DEFAULT_CONV E2B
#endif

char *program;

/*
 * Forward declare the conversion functions.
 */
int et2big();
int big2et();

void usage()
{
    fprintf(stderr, "usage  %s [-h|-e|-r] [-o output_file] [input_file]\n",
            program);
    exit(1);
}

main(argc, argv)
int argc;
char **argv;
{
    FILE *in = stdin, *out = stdout;
    int uncv = 0, direction = DEFAULT_CONV;

    program = (char *)strrchr(argv[0], PATH_SEP);
    if (program == NULL)
      program = argv[0];
    else
      program++;

    argc--;
    *argv++;
    while(argc > 0) {
        if (argv[0][0] == '-') {
            switch(argv[0][1]) {
              case 'e': case 'E': case 'r': case 'R':
                direction = B2E;
                break;
              case 'h': case 'H': case 'b': case 'B':
                direction = E2B;
                break;
              case 'o': case 'O':
                argc--;
                *argv++;
                out = fopen(argv[0], OUT_MODE);
                if (out == NULL) {
                    fprintf(stderr, "%s: problem with output file \"%s\"\n",
                            program, argv[0]);
                    /*
                     * Make sure ``in'' is closed if it is an open file.
                     */
                    if (in != NULL && in != stdin)
                      fclose(in);
                    exit(-1);
                }
                break;
              default:
                /*
                 * Check to see if we opened input and output files and
                 * close them if we did.
                 */
                if (in != NULL && in != stdin)
                  fclose(in);

                if (out != NULL && out != stdout)
                  fclose(out);
                usage();
                break;
            }
        } else {
            in = fopen(argv[0], IN_MODE);
            if (in == NULL) {
                fprintf(stderr, "%s: problem with input file \"%s\"\n",
                        program, argv[0]);
                /*
                 * Make sure ``out'' is closed if it is an open file.
                 */
                if (out != NULL && out != stdout)
                  fclose(out);
                exit(-1);
            }
        }
        argc--;
        *argv++;
    }

    /*
     * Call the appropriate conversion routine.  ``et2big'' is the
     * default conversion routine.
     */
    if (direction == E2B)
      uncv = et2big(in, out);
    else
      uncv = big2et(in, out);

    /*
     * Check to see if we opened input and output files and close them
     * if we did.
     */
    if (in != NULL && in != stdin)
      fclose(in);

    if (out != NULL && out != stdout)
      fclose(out);

    if (uncv != 0)
      fprintf (stderr, "Number of non-standard Big5 (unconverted) = %d\n",
               uncv);

    /*
     * Exit with status 0 to indicate success.  If no exit() is done here,
     * then the exit status of a program will be some unknown value.
     * Shell scripts in Unix sometimes depend on exit status values.
     */
    exit(0);
}

int et2big(in, out)
FILE *in, *out;
{
    int c;
    unsigned char h, l;
    int uncv = 0;

    c = getc(in);
    while (c != EOF) {
        if (! (c & 0x80)) {
            putc(c, out);
            c = getc(in);
            continue;
        }

        /* begin of hanzi */
        h = c & 0xff;
        c = getc(in);
        if (c == EOF) {
            putc(h, out);
            continue;
        }
        l = c & 0xff;

        if ((h < 0xc6) || ((h == 0xc6) && (l <= 0x7e))) {
            /* Frequent BIG5 */
            putc(h, out);
            putc(l, out);
            c = getc(in);
            continue;
        }
        if ((h <= 0xc8) || (h > 0xf9) || ((h == 0xf9) && (l > 0xd5))) {
            /* ET added on non-standard codes */
            putc(UNCONV_H, out);
            putc(UNCONV_L, out);
            uncv++;
            c = getc(in);
            continue;
        }

        /* Non-frequent BIG5 */
        if ((l >= 0xc0) && (l <= 0xfe)) {
            putc((h - 2), out);
            putc((l - 0xc0 + 0x40), out);
        } else if ((l >= 0xa1) && (l <= 0xbf)) {
            putc((h - 3), out);
            putc((l - 0xa1 + 0xe0), out);
        } else if ((l >= 0x40) && (l <= 0x7e)) {
            putc((h - 3), out);
            putc((l - 0x40 + 0xa1), out);
        } else {
            putc(UNCONV_H, out);
            putc(UNCONV_L, out);
            uncv++;
        }
        c = getc(in);
    }
    return (uncv);
}

int big2et(in, out)
FILE *in, *out;
{
    int c;
    unsigned char h, l;
    int uncv = 0;

    c = getc(in);
    while (c != EOF) {
        if (! (c & 0x80)) {
            putc(c, out);
            c = getc(in);
            continue;
        }

        /* begin of hanzi */
        h = c & 0xff;
        c = getc(in);
        if (c == EOF) {
            putc(h, out);
            continue;
        }
        l = c & 0xff;

        if ((h < 0xc6) || ((h == 0xc6) && (l <= 0x7e))) {
            /* Frequent BIG5 */
            putc(h, out);
            putc(l, out);
            c = getc(in);
            continue;
        }
        if ((h > 0xf7) || ((h == 0xf7) && (l > 0x55))) {
            /* non-standard BIG5 */
            putc(UNCONV_H, out);
            putc(UNCONV_L, out);
            uncv++;
            c = getc(in);
            continue;
        }

        /* Non-frequnt BIG5 */
        if ((l >= 0xe0) && (l <= 0xfe)) {
            putc((h + 3), out);
            putc((l - 0xe0 + 0xa1), out);
        } else if ((l >= 0xa1) && (l <= 0xdf)) {
            putc((h + 3), out);
            putc((l - 0xa1 + 0x40), out);
        } else if ((l >= 0x40) && (l <= 0x7e)) {
            putc((h + 2), out);
            putc((l - 0x40 + 0xc0), out);
        } else {
            putc(UNCONV_H, out);
            putc(UNCONV_L, out);
            uncv++;
        }
        c = getc(in);
    }
    return (uncv);
}

