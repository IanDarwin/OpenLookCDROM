/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: sort.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/sort.c,v $\n";
#endif

/* $Log: sort.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/sort.h"

static int reverse = 0;
static int by_type = 0;

static struct {
	int mode;
	int weight;
} weights[] = {
    {MODE_TYPE_DIR ,     0},
    {MODE_TYPE_LINK,     1},
    {MODE_TYPE_FILE,     2}, 
    {MODE_TYPE_BLOCK,    3},
    {MODE_TYPE_CHAR,     4},
    {MODE_TYPE_BLOCK,    5},
    {MODE_TYPE_SOCK,     6}, 
    {0		   ,     7},
};


int
Sort_Files(dir, t, r, bt)
struct _dirs *dir;
int t;
int r;
int bt;
{
    int width = sizeof(struct _files);
    char *highname = NULL;
    char *pos[LIST_MAX];
    int i,j;

    if (!dir || dir->n <=0) return 0;

    if (dir->sort_type == t && dir->reverse == r && 
	dir->sort_by_type == bt) return 0;

    reverse = r;
    by_type = bt;

    for (i=0; i<dir->n; i++)  dir->files[i].temp = 0;

    if (dir->highlight > -1) highname = dir->files[dir->highlight].name;

    for(i=0; i<LIST_MAX; i++) {
        if (dir->position[i] >= 0) {
            dir->files[dir->position[i]].temp = 1;
            pos[i] = dir->files[dir->position[i]].name;
        } else {
	    pos[i] = NULL;
	}
    }


    switch (t) {
	case SORT_BY_NAME:
	    qsort((char *)dir->files, dir->n, width, sort_by_name);
	    break;
	case SORT_BY_SIZE:
	    qsort((char *)dir->files, dir->n, width, sort_by_size);
	    break;
	case SORT_BY_AGE:
	    qsort((char *)dir->files, dir->n, width, sort_by_age);
	     break; 
	default:
	    qsort((char *)dir->files, dir->n, width, sort_by_name);
	    t = SORT_BY_NAME;
	    break;
    }

    dir->highlight = -1;
    if (highname != NULL) {
	for (i=0; i<dir->n; i++) {
            if (strcmp(highname, dir->files[i].name) == 0) {
    	 	dir->highlight = i;
   	        for(j=0; j<LIST_MAX; j++) dir->position[j] = i;
		break;
	    }
	}
    } else {
        for (i=0; i<dir->n; i++) {
 	    if (dir->files[i].temp) {
   	        for(j=0; j<LIST_MAX; j++) {
		    if (pos[j] && (strcmp(pos[j], dir->files[i].name) == 0)) {
                        dir->position[j] = i;
		    }
	        }
	   }
	}
    }

    dir->sort_type = t;
    dir->reverse = r;
    dir->sort_by_type = bt;
    return 1;
}

static int
sort_by_age(a, b)
char *a;
char *b;
{
     struct _files *x = (struct _files *)a;
     struct _files *y = (struct _files *)b;
     int value = 0;

     value = sort_by_type(a, b);

     if (value == 0) value = x->time - y->time;

     if (reverse) {
	value = -value;
     }
     return value;
}

static int
sort_by_name(a,b)
char *a;
char *b;
{
     struct _files *x = (struct _files *)a;
     struct _files *y = (struct _files *)b;
     int value = 0;

     value = sort_by_type(a, b);
     if (value == 0) value = strcmp(x->name, y->name);

     if (reverse) {
	value = -value;
     }
     return value;
}

static int
sort_by_size(a,b)
char *a;
char *b;
{
     struct _files *x = (struct _files *)a;
     struct _files *y = (struct _files *)b;
     int value = 0;

     value = sort_by_type(a, b);

     if (value == 0)  value = x->size - y->size;

     if (reverse) {
	value = -value;
     }
     return value;
}

static int
sort_by_type(a, b)
char *a;
char *b;
{
     struct _files *x = (struct _files *)a;
     struct _files *y = (struct _files *)b;
     int value;

     if (!by_type) return 0;
     value = weight_type(x) - weight_type(y);
     return value;
}

static int
weight_type(x)
struct _files *x;
{
    int i;

    for (i=0; weights[i].mode; i++) {
	if (weights[i].mode & x->mode) break;
    }
    return weights[i].weight;
}
