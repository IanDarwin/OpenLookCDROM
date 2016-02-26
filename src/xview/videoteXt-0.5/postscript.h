#ifndef _POSTSCRIPT_H
#define _POSTSCRIPT_H

/* Copyright (c) 1995 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */

#include <stdio.h>
#include "vtxdecode.h"


void write_postscript(FILE *file, const vtxpage_t *page, const char *station, int color,
    int invert, int show_hidden);

#endif /* _POSTSCIPT_H */
