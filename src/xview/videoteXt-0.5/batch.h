#ifndef _VTX_ASCII_H
#define _VTX_ASCII_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */

enum { GET_ALLNORM = 1, GET_ALLSUB, GET_ALL };

extern int interleave;


void ascii_insert_pagelist(int page, int subpage);
int ascii_get_pages(int getall);
int display_file(const char *fname);

#endif /* _VTX_ASCII_H */
