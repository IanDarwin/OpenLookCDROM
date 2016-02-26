#ifndef _SAFE_MALLOC_H
#define _SAFE_MALLOC_H

/*
 * safe_malloc & friends
 *
 * Copyright (c) 1994-95 by Martin Buck
 */

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <sys/types.h>		/* for size_t */


/* The error-handler pointed to by smalloc_callback can return one of the
 * following values to tell smalloc & friends what to do next.
 */
#define SMALLOC_GIVEUP 0
#define SMALLOC_RETRY 1

/* The following values are used for the type-argument of the error-handler
 * to tell it, which action went wrong.
 */
#define SMALLOC_MALLOC  1
#define SMALLOC_CALLOC  2
#define SMALLOC_REALLOC 3
#define SMALLOC_STRDUP  4
#define SMALLOC_STRAPP  5


/* Define your own error-handler by letting smalloc_callback point to your
 * own function. type tells you which action went wrong, size is the number
 * of bytes that were requested, request is the serial number of the request.
 */
extern int (*smalloc_callback)(int type, size_t size, int request);

/* These are the error-messages which are printed if the are != NULL. ': '
 * will be appended to smalloc_progname; after smalloc_errmsg a newline will
 * be printed.
 */
extern char *smalloc_progname, *smalloc_errmsg;

/* This is the exit-status which will be returned if the default error-handler
 * terminates you program. It is unused if you defined your own error-handler.
 */
extern int smalloc_exitstatus;



/* This one corresponds directly to the libc malloc
 */
void* smalloc(size_t size);

/* This one corresponds directly to the libc calloc
 */
void* scalloc(size_t count, size_t eltsize);

/* srealloc has extended functionality: if you pass a NULL-ptr in oldptr, malloc gets called
 * instead of realloc.
 */
void* srealloc(void *oldptr, size_t newsize);

/* This one corresponds directly to the libc strdup
 */
char* sstrdup(const char *str);

/* sstrapp appends string from to string to; it reallocs enough memory to fit both strings.
 * If to is NULL, this function acts like sstrdup, otherwise to must have been allocated with
 * (s)malloc!
 */
char* sstrapp(char *to, const char *from);

#endif /* _SAFE_MALLOC_H */
