#include <stdio.h>

/*
 * The following coded was derived from the bsd fgets.c.
 * I could no longer depend on fgets to work with nonblocking io.
 * WLJ
 */

/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "machine.h"
#include "myfgets.h"

int
myfgets(buf, n, source, buffs)
char *buf;
int n;
int source;
struct _buffs *buffs;
{
    int len;
    char *s;
    char *p, *t;
    int  nn;

    s = buf;
    n--;			/* leave space for NUL */
    do {
	/*
	 * If the buffer is empty, refill it.
	 */
	if ((len = buffs->count) <= 0) {
	    buffs->count = read(source, buffs->buff, BUFFSIZE);
	    if (buffs->count <= 0) {
		nn =  buffs->count;
		buffs->count = 0;
		if (s == buf) return nn;
		break;
	    }
	    buffs->ptr = buffs->buff;
	    len = buffs->count;
	    buffs->ptr[buffs->count] = '\0';
  
	}
	p = buffs->ptr;
	/*
         * Scan through at most n bytes of the current buffer,
	 * looking for '\n'.  If found, copy up to and including
	 * newline, and stop.  Otherwise, copy entire chunk
	 * and loop.
	 */
	if (len > n) len = n;
	t = INDEX(p, '\n');
	if (t != NULL) {
	    t++;
	    len = t - p;
	    buffs->count -= len;
	    buffs->ptr = t;
	    (void)COPY((void *)p, (void *)s, len);
	    s[len] = 0;
	    return 1;
    	}
    	buffs->count -= len;
	buffs->ptr   += len;
	(void) COPY((void *)p, (void *)s, len);
	s += len;
    } while ((n -= len) != 0);
    *s = 0;
    return 1;
}


#define IAC     255 
#define DONT    254       
#define DO      253  
#define WONT    252 
#define WILL    251

int
telnetfgets(buf, n, source, buffs)
char *buf;
int n;
int source;
struct _buffs *buffs;
{
    int  nn;
    int  len; 
#if defined(__STDC__)||(defined(sun)&&!defined(SVR4))
    unsigned char c;
#else
    char c;
#endif

    n--;			/* leave space for NUL */

    switch (buffs->state) {
	case 1:
	     if ((nn = IACSUB(buffs,source)) <= 0) return nn;
	case 2:
	     if ((nn = DOSUB(buffs, source)) <= 0) return nn;
     }

    buffs->state = 0;
    while ((buffs->count < n) &&
	   ((nn = read(source, &c, 1)) == 1) && 
	   (c != '\n') ) {
	if (c == IAC) {
	    buffs->state = 1;
	    if ((nn = IACSUB(buffs, source)) <= 0) return nn;
	    if ((nn = DOSUB(buffs, source)) <= 0) return nn;
            buffs->state = 0;
	    c = 0;
	    continue;
	}
	buffs->buff[buffs->count] = c;
	buffs->count++;
        c = 0;
    }

    if (buffs->count == 0) return nn;

    if (c == '\n') {
	buffs->buff[buffs->count] = '\n';
	buffs->count++;
    }

    buffs->buff[buffs->count] = '\0';
    buffs->count++;
    len = buffs->count;
    (void) COPY(buffs->buff, buf, len);
    buffs->count = 0;
    return len;
}

int
DOSUB(buffs, source)
struct _buffs *buffs;
int   source;
{
    int n;
    char c;
    char cout[5];

    switch(buffs->func) {
	case WILL:
	case WONT:
     	    n = read(source, &c, 1);
     	    if (n!=1) return n;
	    sprintf(cout, "%c%c%c", IAC, DONT, c);
	    write(source, cout, strlen(cout));
	    return n;
	case DO:
	case DONT:
     	    n = read(source, &c, 1);
     	    if (n!=1) return n;
	    sprintf(cout, "%c%c%c", IAC, WONT, c);
	    write(source, cout, strlen(cout));
	    return n;
	default:
	    return 1;
    }
}

int
IACSUB(buffs, source)
struct _buffs *buffs;
int   source;
{
     int n;
     char c;

     buffs->state = 1;
     n = read(source, &c, 1);
     if (n == 1)  {
         buffs->state = 2;
         buffs->func = c;
     }
     return n;
}
