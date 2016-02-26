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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "safe_malloc.h"


int (*smalloc_callback)(int type, size_t size, int request) = NULL;
char *smalloc_progname = NULL, *smalloc_errmsg = NULL;
int smalloc_exitstatus = 1;
static int request = 0;



static int
smalloc_error(int type, size_t size, int request) {
  if (smalloc_callback) {
    return smalloc_callback(type, size, request);
  } else {
    if (smalloc_progname)
      fprintf(stderr, "%s: ", smalloc_progname);
    if (smalloc_errmsg) {
      fprintf(stderr, "%s\n", smalloc_errmsg);
    } else {
      fprintf(stderr, "Out of memory.\n");
    }
    exit(smalloc_exitstatus);
  }
}



void*
smalloc(size_t size) {
  void *ptr;

  request++;
  do {
    ptr = malloc(size);
  } while (!ptr && smalloc_error(SMALLOC_MALLOC, size, request) == SMALLOC_RETRY);
  return ptr;
}


void*
scalloc(size_t count, size_t eltsize) {
  void *ptr;

  request++;
  do {
    ptr = calloc(count, eltsize);
  } while (!ptr && smalloc_error(SMALLOC_CALLOC, count * eltsize, request) == SMALLOC_RETRY);
  return ptr;
}


void*
srealloc(void *oldptr, size_t newsize) {
  void *ptr;

  request++;
  do {
    if (oldptr) {
      ptr = realloc(oldptr, newsize);
    } else {
      ptr = malloc(newsize);
    }
  } while (!ptr && smalloc_error(SMALLOC_REALLOC, newsize, request) == SMALLOC_RETRY);
  return ptr;
}


char*
sstrdup(const char *str) {
  char *ptr;

  request++;
  do {
    ptr = strdup(str);
  } while (!ptr && smalloc_error(SMALLOC_STRDUP, strlen(str) + 1, request) == SMALLOC_RETRY);
  return ptr;
}


char*
sstrapp(char *to, const char *from) {
  int newlen;
  char *ptr;
  
  newlen = strlen(from) + 1;
  if (to) {
    newlen += strlen(to);
  }
  request++;
  do {
    if (to) {
      ptr = realloc(to, newlen);
    } else {
      ptr = malloc(newlen);
      ptr[0] = '\0';
    }
  } while (!ptr && smalloc_error(SMALLOC_STRAPP, newlen, request) == SMALLOC_RETRY);
  if (ptr) {
    strcat(ptr, from);
  }
  return ptr;
}
