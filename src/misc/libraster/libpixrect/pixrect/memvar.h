#ifndef _MEMVAR_H_
#define _MEMVAR_H_

/* Routines. */

extern struct pixrect* mem_create ARGS(( int w, int h, int depth ));
extern struct pixrect* mem_point ARGS(( int w, int h, int depth, short* data ));

extern int mpr_prlinebytes ARGS(( struct pixrect* pr ));
extern short* mpr_primage ARGS(( struct pixrect* pr ));

#endif /*_MEMVAR_H_*/
