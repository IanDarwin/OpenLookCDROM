/* mufbuf.h -- master update file buffers
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
#include "mufbuf.h"

#define NULL 0

#define MUFTCHUNK 1024
#define MUFCHUNK  8192

/* initialize a mufbuf. returns -1 on failure.
 */
int mufbuf_init(mbuf)
    mufbuf *mbuf;
{
    mbuf->entries = 0;
    mbuf->sused = 0;
    mbuf->ssize = MUFCHUNK;
    mbuf->tabsize = MUFTCHUNK;
    mbuf->table = (char **) malloc(MUFTCHUNK * sizeof (char *));
    mbuf->storage = (char *) malloc(MUFCHUNK);
    if (mbuf->table == (char **) NULL || mbuf->storage == (char *) NULL) {
	return (-1);
    }

    return (0);
}

/* clear entries from a mufbuf
 */
void mufbuf_clear(mbuf)
    mufbuf *mbuf;
{
    mbuf->entries = 0;
    mbuf->sused = 0;
}

/* re-calculate the table pointers
 */
void mufbuf_table(mbuf, size)
    mufbuf *mbuf;
    int size;
{
    char **tab = mbuf->table;
    char **tend = tab + mbuf->tabsize;
    char *ptr, *base;

    base = ptr = mbuf->storage;
    mbuf->entries = 0;
    mbuf->sused = 0;
    do {
	while (size && *ptr != '\n' && *ptr) ++ptr, --size;
	if (!size--) break;
	*ptr++ = '\0';
	if (tab == tend) {
	    mufbuf_grow(mbuf);
	    tab = mbuf->table;
	    tend = tab + mbuf->tabsize;
	    tab += mbuf->entries;
	}
	*tab++ = base;
	++mbuf->entries;
	base = ptr;
    } while (size);
    mbuf->sused = base - mbuf->storage;
}

/* returns pointer to a new mufbuf entry with space for MAXMUFSTR chars
 * returns NULL on failure
 */
char *mufbuf_grow(mbuf)
    mufbuf *mbuf;
{
    int i;
    char **tab, *ptr;
    
    if (mbuf->entries == mbuf->tabsize) {
	mbuf->tabsize += MUFTCHUNK;
	mbuf->table = (char **)
	    realloc((char *) mbuf->table, mbuf->tabsize * sizeof (char *));
	if (mbuf->table == (char **) NULL) return ((char *) NULL);
    }
    if (mbuf->ssize - mbuf->sused < MAXMUFSTR) {
	mbuf->storage = realloc(mbuf->storage, mbuf->ssize += MUFCHUNK);
	if (mbuf->storage == (char *) NULL) return ((char *) NULL);
	i = mbuf->entries;
	tab = mbuf->table;
	ptr = mbuf->storage;
	do {
	    *tab++ = ptr;
	    while (*ptr++);
	} while (--i);
    }
    
    return (mbuf->storage + mbuf->sused);
}

/* add a string to a mufbuf.  strlen must be < MAXMUFSTR
 */
int mufbuf_add(mbuf, str)
    mufbuf *mbuf;
    char *str;
{
    char *dst;
    int len = strlen(str);

    if (len >= MAXMUFSTR || (dst = mufbuf_grow(mbuf)) == (char *) NULL) {
	return (-1);
    }
    ++mbuf->entries;
    mbuf->sused += len + 1;
    if (str != dst) strcpy(dst, str);

    return (0);
}

/* set the data size for a mufbuf
 */
char *mufbuf_datasize(mbuf, size)
    mufbuf *mbuf;
    int size;
{
    if (mbuf->ssize < size) {
	mbuf->storage = realloc(mbuf->storage, mbuf->ssize = size);
    }

    return (mbuf->storage);
}

/* copy one mufbuf into another.  Returns -1 on failure, 0 on success
 * mufbuf *dst, *src;
 * int append;
 */
int mufbuf_copy(dst, src, append)
    mufbuf *dst, *src;
    int append;
{
    int i, old, new;
    char **tab, *ptr;

    if (!src->entries) {
	if (!append) {
	    dst->sused = 0;
	    dst->entries = 0;
	}
	return (0);
    }
    old = dst->ssize;
    new = (append ? dst->sused : 0) + src->sused;
    if (old < new) {
	dst->storage = realloc(dst->storage, dst->ssize = new);
	if (dst->storage == (char *) NULL) return (-1);
    }
    old = dst->tabsize;
    new = (append ? dst->entries : 0) + src->entries;
    if (old < new) {
	dst->tabsize = new;
	dst->table = (char **)
	    realloc((char *) dst->table, new * sizeof (char *));
	if (dst->table == (char **) NULL) return (-1);
    }
    old = append ? dst->sused : 0;
    dst->sused = src->sused + old;
    bcopy(src->storage, dst->storage + old, src->sused);
    old = append ? dst->entries : 0;
    i = dst->entries = src->entries + old;
    tab = dst->table;
    ptr = dst->storage;
    do {
	*tab++ = ptr;
	while (*ptr++);
    } while (--i);

    return (0);
}

/* qsort comparison function
 */
static int cmp(pone, ptwo)
    char **pone, **ptwo;
{
    unsigned char *one = (unsigned char *) *pone;
    unsigned char *two = (unsigned char *) *ptwo;
    
    while (*one == *two && *one != ' ') {
	++one, ++two;
    }

    return (*one - *two);
}


/* qsort line pointers in a mufbuf
 */
void mufbuf_qsort(mbuf)
    mufbuf *mbuf;
{
    if (mbuf->entries) {
	qsort((char *) mbuf->table, mbuf->entries, sizeof (char *), cmp);
    }
}

/* lookup a bboard (by binary search), returns NULL if not found
 */
char *mufbuf_lookup(mbuf, str)
    mufbuf *mbuf;
    char *str;
{
    int bot = 0, top = mbuf->entries - 1, mid;
    char *scan, *ptr;

    while (bot <= top) {
	ptr = mbuf->table[mid = (bot + top) >> 1];
	for (scan = str; *ptr != ' ' && *scan == *ptr; ++ptr, ++scan);
	if (*ptr == ' ' && *scan == '\0') {
	    return (mbuf->table[mid]);
	}
	if (*scan < *ptr) {
	    top = mid - 1;
	} else {
	    bot = mid + 1;
	}
    }

    return ((char *) NULL);
}
