/* mufstr.c -- master update string encoding/decoding
 *
 *	(C) Copyright 1991 by Carnegie Mellon University
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose and without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in 
 * supporting documentation, and that the name of CMU not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  
 * 
 * CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * Author: Chris Newman
 * Start Date: 9/18/91
 */

#include <andrewos.h>
#include <ctype.h>

#define NULL 0

/* from asetup.c: */
extern char *treeroots[];

static char treeroot[] = ".MESSAGES/";

/* encode a master update line into a mufstr
 */
int mufstr_encode(dst, src)
    char *dst, *src;
{
    char *base = dst;
    char *root = src;
    int i, len;

    while (*src && strncmp(src, treeroot, sizeof (treeroot) - 1)) {
	while (*++src && *src != *treeroot);
    }
    if (!*src) return (0);
    len = src - root;
    src += sizeof (treeroot) - 1;
    while (*src && !isspace(*src)) {
	*dst++ = *src == '/' ? '.' : *src;
	++src;
    }
    *dst++ = ' ';
    for (i = 0; treeroots[i] != (char *) NULL; ++i) {
	if (!strncmp(root, treeroots[i], len)) {
	    *dst++ = 'A' + i;
	    goto DO_DATE;
	}
    }
    strncpy(dst, root, len);
    dst += len;
    *dst++ = ' ';
 DO_DATE:
    while (isspace(*src)) ++src;
    while (*src && !isspace(*src)) *dst++ = *src++;
    *dst = '\0';

    return (dst - base);
}

#if 0
/* decode a mufstr into a path and a date
 * returns pointer to date.
 */
char *mufstr_decode(dst, src)
    char *dst, *src;
{
    char *root = src;
    char *tmp;
    int namlen, len;

    while (*src && !isspace(*src)) ++src;
    namlen = src - root;
    while (isspace(*src)) ++src;
    if (*src == '/') {
	while (*src && !isspace(*src)) *dst++ = *src++;
	strcpy(dst, treeroot);
	dst += sizeof (treeroot) - 1;
    } else {
	for (tmp = treeroots[*src - 'A']; *tmp; *dst++ = *tmp++);
    }
    while (namlen--) {
	*dst++ = *root == '.' ? '/' : *root;
	++root;
    }
    tmp = dst;
    while (*src && !isspace(*src)) *dst++ = *src++;
    *dst = '\0';

    return (tmp);
}
#endif
