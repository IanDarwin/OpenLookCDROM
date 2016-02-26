/*
 * Icon-uncompact.c - used to extract Sun icon format files from the
 *			compact file format produced by Icon-compress[81]
 *			and Icon-compact.
 *
 * Written 9/12/91 by S. K. Tazuma, for use with Mahjongg color icons.
 *
 * $Header$
 *
 */
#include <stdio.h>

#define AVOID_LAST_COMMA	/* if defined, enables code which prevents
				 * the last comma being output; minor
				 * cosmetic issue, since C doesn't mind the
				 * trailing comma.  But in the interests
				 * of creating an exact duplicate, might
				 * as well go all the way. */
#define TYPE_64x64x8	'%'
#define TYPE_64x64x1	'!'
#define TYPE_16x16x1	'^'

#define DO_REST_OF_BUF	-1

char *hdr_strings[] = {
"/* Format_version=1, Width=64, Height=64, Depth=8, Valid_bits_per_item=16",
"/* Format_version=1, Width=64, Height=64, Depth=1, Valid_bits_per_item=16",
"/* Format_version=1, Width=16, Height=16, Depth=1, Valid_bits_per_item=16",
" *	Copyright 1988, Mark Holm",
" *			Exceptions",
" *",
" *	Permission is given to copy and distribute for non-profit purposes.",
" *",
" */",
};

#ifdef AVOID_LAST_COMMA
int First_line;		/* used as boolean */
#endif

main()
{
    char buf[256];
    int count;
/*
#define INITIAL_FP	NULL
*/
#define INITIAL_FP	stdout
    FILE *fp = INITIAL_FP;
    int compression_type = TYPE_64x64x8;
		/* if we get fed an input file which doesn't have a magic
		 * line, then we uncompact the input to stdout, and
		 * assume that the input is in an 8 bit color type icon.
		 */
    int i;
    char *line1;

    while (fgets(buf, sizeof(buf), stdin)) {
	buf[strlen(buf) - 1] = '\0';	/* need null for the fopen() */

	if (
		buf[0] == TYPE_64x64x8 ||
		buf[0] == TYPE_64x64x1 ||
		buf[0] == TYPE_16x16x1	) {

	    compression_type = buf[0];
	    if (fp != INITIAL_FP) {
#ifdef AVOID_LAST_COMMA
		fputc('\n', fp);
#endif
		fclose(fp);
	    }
	    fp = fopen(&buf[2], "w+");
	    if (fp == NULL) {
		fprintf(stderr, "Error, couldn't create '%s'\n", &buf[2]);
		exit(1);
	    }
	    if (compression_type == TYPE_64x64x8)
		line1 = hdr_strings[0];
	    else if (compression_type == TYPE_64x64x1)
		line1 = hdr_strings[1];
	    if (compression_type == TYPE_16x16x1)
		line1 = hdr_strings[2];

	    fprintf(fp, "%s\n", line1);
	    for (i = 3; i <= 8; i++)
		fprintf(fp, "%s\n", hdr_strings[i]);
#ifdef AVOID_LAST_COMMA
	    First_line = 1;
#endif

	    continue;
	}

	if (compression_type == TYPE_64x64x1 ||
			compression_type == TYPE_16x16x1)
	    print_line_1bit(fp, buf);
	else
	    print_line_8bit(fp, buf);

    }
    if (fp != NULL) {
	if (compression_type == TYPE_64x64x8) {
	    print_line(fp, DO_REST_OF_BUF, 0);
	}
#ifdef AVOID_LAST_COMMA
	fputc('\n', fp);
#endif
	fclose(fp);
    }
    exit(0);
}

print_line_8bit(fp, p)
FILE *fp;
char *p;
{
    int count;

    while (*p != '\0') {
	if (*p >= '0' && *p <= '7') {
	    print_line(fp, *p, 1);
	    ++p;
	}
	else {
	    if (*p >= 'a' && *p <= 'z')
		count = *p - 'a' + 1;
	    else if (*p >= 'A' && *p <= 'Z')
		count = *p - 'A' + 27;
	    else {
		fprintf(stderr, "Error, bad char '%c' in input file\n", *p);
		exit(1);
	    }

	    while (count > 0)
		count = print_line(fp, *(p+1), count);

	    p += 2;
	}
    }
}

#define HOLD_BUF_LEN		16
char hold_buf[HOLD_BUF_LEN + 1];
int hold_buf_len = 0;

char tmp_buf[] = "0x0-0-,";


print_line(fp, code, count)
FILE *fp;
char code;
int count;
{
    int new_str_len;
    char output_buf[128];
    int i;

    while (count > 0 && hold_buf_len < HOLD_BUF_LEN) {
	hold_buf[hold_buf_len] = code;
	++hold_buf_len;
	--count;
    }

    if (hold_buf_len == HOLD_BUF_LEN || code == DO_REST_OF_BUF) {
	output_buf[0] = '\0';
	for (i = 0; i < hold_buf_len; i += 2) {
	    tmp_buf[3] = hold_buf[i];
	    tmp_buf[5] = hold_buf[i+1];
	    strcat(output_buf, tmp_buf);
	}
	if (output_buf[0] != '\0') {
#ifdef AVOID_LAST_COMMA
	    output_buf[strlen(output_buf) - 1] = '\0';	/* delete comma, will
							 * get it next time
							 * around */
	    if (First_line) {
		fprintf(fp, "\t%s", output_buf);
		First_line = 0;
	    }
	    else
		fprintf(fp, ",\n\t%s", output_buf);
#else
	    fprintf(fp, "\t%s\n", output_buf);
#endif
	}
	hold_buf_len = 0;
    }

    return(count);
}

char tmp_buf2[] = "0x----,";

print_line_1bit(fp, buf)
FILE *fp;
char *buf;
{
    int new_str_len;
    char output_buf[128];
    int i;

    output_buf[0] = '\0';

    for (i = 0; i < 8; i++) {
	tmp_buf2[2] = buf[ i * 4 + 0 ];
	tmp_buf2[3] = buf[ i * 4 + 1 ];
	tmp_buf2[4] = buf[ i * 4 + 2 ];
	tmp_buf2[5] = buf[ i * 4 + 3 ];
	strcat(output_buf, tmp_buf2);
    }
#ifdef AVOID_LAST_COMMA
    output_buf[strlen(output_buf) - 1] = '\0'; 	/* delete comma, will
						 * get it next time
						 * around */
    if (First_line) {
	fprintf(fp, "\t%s", output_buf);
	First_line = 0;
    }
    else
	fprintf(fp, ",\n\t%s", output_buf);
#else
    fprintf(fp, "\t%s\n", output_buf);
#endif
}
