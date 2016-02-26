#ifndef _FILEIO_H
#define _FILEIO_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include <stdio.h>


typedef int (*filefunc)();


int load_vtx(FILE *file);
void save_vtx(FILE *file);
void export_ascii(FILE *file);
void export_gif(FILE *file);
void export_ppm(FILE *file);

#endif /* _FILEIO_H */
