#ident "@(#)term.h 1.18 91/09/14"

/*
 *
 * Copyright (c) 1991 by Sun Microsystems, Inc.
 *
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <stdio.h>

#ifndef assert
#ifdef DEBUG
#ifdef __STDC__
#include <assert.h>
#else
#define assert(x) (!(x) ? fprintf(stderr,"Assertion failed \"x\" [file %s, line %d]\n", __FILE__, __LINE__),fflush(stderr),abort() : 0)
#endif
#else
#define assert(x)
#endif /* DEBUG */
#endif /* assert */


extern int link_count;
extern int selection;
extern int master_fd;
extern int scrolling;
extern FILE *master_fp;

typedef unsigned char char_type;
typedef unsigned char attribute_type;

typedef struct screen_line_struct {
  struct screen_line_struct *next, *prev;
  char_type	   *line;
  attribute_type   *ats;
  short    len, rsize;
  unsigned char dirty, trimmed, wrapped;
}	       SCREENLINE;

struct term_flags {
  char deccolm, insert, decscnm, decom, decawm, decarm, lnm;
  /* Liam: colour */ unsigned char r, g, b; /* Liam */
};

/* 
 *  deccolm - TRUE = 132 col mode
 *  insert  - FALSE - insert mode
 *  decscnm - FALSE = white on black
 *  decom   - FALSE = origin is upper left
 *  decawm  - TRUE  = auto-wrap mode
 *  decarm  - TRUE = auto-repeat mode
 *  lnm     = FALSE = only vert movement when I get a LF
 */

extern struct term_flags flags;

extern SCREENLINE    *tail;
extern SCREENLINE    **screen;
extern int scrolling_method;

extern int x_size, y_size, x_loc, y_loc;

#include <sys/types.h> 

#include "ansi.h"

#ifdef __STDC__
#define bzero(_x, _y) memset((_x), 0, (_y))
#else
extern void bzero();
#define memmove(_x,_y,_z) bcopy(_y,_x,_z)
#endif
