#ifndef _PR_IO_H_
#define _PR_IO_H_

#include <stdio.h>

/* Definitions. */

/* Colormap type. */
typedef struct
    {
    int type;
#define RMT_NONE	0
#define RMT_EQUAL_RGB	1
#define RMT_RAW		2
    int length;
    unsigned char* map[3];
    } colormap_t;


/* Routines. */

extern struct pixrect* pr_load ARGS(( FILE* f, colormap_t* c ));
extern int pr_dump ARGS(( struct pixrect* pr, FILE* f, colormap_t* cm, int type, int copy ));

#endif /*_PR_IO_H_*/
