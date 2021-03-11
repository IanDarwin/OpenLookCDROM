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

typedef struct mufbuf {
    int entries;
    char **table;
    char *storage;
    int tabsize;
    int sused, ssize;
} mufbuf;

#define MAXMUFSTR 1024

#ifdef __STDC__
/* initialize a mufbuf. returns -1 on failure.
 */
int mufbuf_init(mufbuf *);

/* clear entries from a mufbuf
 */
void mufbuf_clear(mufbuf *);

/* re-calculate the table pointers
 */
void mufbuf_table(mufbuf *, int);

/* returns pointer to a new mufbuf entry with space for MAXMUFSTR chars
 * returns NULL on failure
 */
char *mufbuf_grow(mufbuf *mbuf);

/* add a string to a mufbuf.  strlen must be < MAXMUFSTR
 */
int mufbuf_add(mufbuf *, char *);

/* set the data size for a mufbuf
 */
char *mufbuf_datasize(mufbuf *mbuf, int size);

/* copy one mufbuf into another.  Returns -1 on failure, 0 on success
 * mufbuf *dest, *src;
 * int append;
 */
int mufbuf_copy(mufbuf *, mufbuf *, int);

/* qsort line pointers in a mufbuf
 */
void mufbuf_qsort(mufbuf *mbuf);

/* lookup a bboard (by binary search), returns NULL if not found
 */
char *mufbuf_lookup(mufbuf *mbuf, char *str);
#else
int mufbuf_init(), mufbuf_add(), mufbuf_copy();
void mufbuf_clear(), mufbuf_table(), mufbuf_qsort();
char *mufbuf_grow(), *mufbuf_lookup(), *mufbuf_datasize();
#endif
