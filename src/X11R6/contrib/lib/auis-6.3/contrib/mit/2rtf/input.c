/* input.c: Implementation of multiple ungetc. */

/* This file provides the routines input_getc, input_getchar, input_ungetc,
 * input_fgetc, and input_fgets.  The header file input.h redefines getc,
 * getchar, ungetc, fgetc, and fgets to point to these routines.  Note that
 * we do NOT include input.h here, since we want access to the real stdio
 * routines. */

/* The input_* routines are distinguished from the regular stdio routines in
 * that they provide up to 1024 characters of ungetc.  This is not the right
 * solution to 2rtf's problems, but I don't feel like a complete rewrite. */

/* Performance is not a concern.  Also, this routine never hears about
 * fclose(), so it will leak memory like a sieve if the number of files used
 * by the program is not bounded.  This is not a concern for the 2rtf
 * translator.) */

/* I keep state in a File_entry structure.  buf holds the pushed-back
 * characters, which are written in from right to left.  buf_pos has the
 * position of the first character in the buffer; buf_pos == UNGETC_BUF_SIZE
 * indicates that no characters are in the buffer. */

/* Oh, and before you read any further, let me just say that I'm very,
 * very, sorry. */

#include <stdio.h>
#include <ctype.h>

char *memchr();

#define UNGETC_BUF_SIZE 1024

typedef struct file_entry File_entry;

struct file_entry {
    FILE *fp;
    char buf[UNGETC_BUF_SIZE];
    int buf_pos;
    File_entry *next;
};

File_entry *head;

static File_entry *get_file_entry();

int input_getc(fp)
FILE *fp;
{
    File_entry *entry = get_file_entry(fp);

    if (entry->buf_pos < UNGETC_BUF_SIZE)
	return entry->buf[entry->buf_pos++];
    return getc(fp);
}

int input_getchar()
{
    return input_getc(stdin);
}

void input_ungetc(c, fp)
int c;
FILE *fp;
{
    File_entry *entry = get_file_entry(fp);

    entry->buf[--entry->buf_pos] = c;
}

int input_fgetc(fp)
FILE *fp;
{
    return input_getc(fp);
}

char *input_fgets(str, n, fp)
char *str;
int n;
FILE *fp;
{
    File_entry *entry = get_file_entry(fp);
    int num_chars = UNGETC_BUF_SIZE - entry->buf_pos;
    char *p, *start = entry->buf + entry->buf_pos;

    /* The easy, and common, case: no characters in pushback. */
    if (!num_chars)
	return fgets(str, n, fp);

    /* Look for a newline in the pushback buffer. */
    p = memchr(start, '\n', num_chars);

    /* See if we have more characters to copy than there is room in str. */
    if ((p && p + 1 - start > n - 1) || (!p && num_chars > n - 1)) {
	memcpy(str, start, n - 1);
	str[n - 1] = 0;
	entry->buf_pos += n - 1;
	return str;
    }

    /* Okay, there's room in str (the usual case).  Did we find a newline? */
    if (p) {
	num_chars = p + 1 - str;
	memcpy(str, start, num_chars);
	str[num_chars] = 0;
	entry->buf_pos += num_chars;
	return str;
    }

    /* We didn't find a newline.  Copy the pushback characters and call fgets()
     * to get the rest. */
    memcpy(str, start, num_chars);
    entry->buf_pos = UNGETC_BUF_SIZE;
    fgets(str + num_chars, n - num_chars, fp);
    return str;
}

static File_entry *get_file_entry(fp)
FILE *fp;
{
    File_entry *entry;

    for (entry = head; entry; entry = entry->next) {
	if (entry->fp == fp)
	    return entry;
    }

    entry = (File_entry *)malloc(sizeof(File_entry));
    if (!entry) {
	fputs("malloc failed, aborting.", stderr);
	exit(1);
    }

    entry->fp = fp;
    entry->buf_pos = UNGETC_BUF_SIZE;
    entry->next = head;
    head = entry;

    return entry;
}

long input_number(fp)
FILE *fp;
{
    long num = 0;
    int c, neg = 0;

    c = input_getc(fp);
    if (c == '-') {
	neg = 1;
	c = input_getc(fp);
    }
    while (c != EOF && isdigit(c)) {
	num = num * 10 + c - '0';
	c = input_getc(fp);
    }
    if (c != EOF)
	input_ungetc(c, fp);
    return (neg) ? -num : num;
}

void input_skip_whitespace(fp)
FILE *fp;
{
    int c;

    c = input_getc(fp);
    while (c != EOF && isspace(c))
	c = input_getc(fp);
    if (c != EOF)
	input_ungetc(c, fp);
}

void input_match(s, fp)
char *s;
FILE *fp;
{
    int c;

    c = input_getc(fp);
    while (c != EOF && *s && c == *s) {
	s++;
	c = input_getc(fp);
    }
    if (c != EOF)
	input_ungetc(c, fp);
}

int input_read_up_to(s, n, marker, fp)
char *s;
int n;
int marker;
FILE *fp;
{
    int c;

    c = input_getc(fp);
    if (c == EOF)
	return EOF;

    while (c != EOF && n-- > 1 && c != marker) {
	*s++ = c;
	c = input_getc(fp);
    }
    *s = 0;

    if (c != EOF && c != marker)
	input_ungetc(c, fp);

    return 0;
}

int input_read_up_to_whitespace(s, n, fp)
char *s;
int n;
FILE *fp;
{
    int c;

    c = input_getc(fp);
    if (c == EOF)
	return EOF;

    while (c != EOF && n-- >= 1 && !isspace(c)) {
	*s++ = c;
	c = input_getc(fp);
    }
    *s = 0;

    if (c != EOF)
	input_ungetc(c, fp);

    return 0;
}

int input_read_up_to_one_of(s, n, markers, fp)
char *s;
int n;
char *markers;
FILE *fp;
{
    int c;

    c = input_getc(fp);
    if (c == EOF)
	return EOF;

    while (c != EOF && n-- >= 1 && !strchr(markers, c)) {
	*s++ = c;
	c = input_getc(fp);
    }
    *s = 0;

    if (c != EOF)
	input_ungetc(c, fp);

    return 0;
}

