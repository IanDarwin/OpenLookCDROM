/*
 * Copyright 1989, 1992 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

/* 
 * BitmapEditP.h - Private definitions for BitmapEdit widget
 */

#ifndef _ORABitmapEditP_h
#define _ORABitmapEditP_h

/*
 * This include not needed unless the .c file includes IntrinsicP.h
 * after this file.   Anyway, it doesn't hurt.
 */
#include <X11/CoreP.h>

/*
 * This one is always needed!
 */
#include "BitmapEdit.h"

/* New fields for the BitmapEdit widget class record */

typedef struct {
	int make_compiler_happy;	/* keep compiler happy */
} BitmapEditClassPart;

/* Full class record declaration */
typedef struct _BitmapEditClassRec {
    CoreClassPart	core_class;
    BitmapEditClassPart	bitmapEdit_class;
} BitmapEditClassRec;

extern BitmapEditClassRec bitmapEditClassRec;

/* New fields for the BitmapEdit widget record */
typedef struct {
    /* resources */
    Pixel	foreground;
    XtCallbackList callback;	/* application installed callback function(s) */
    Dimension	pixmap_width_in_cells;
    Dimension	pixmap_height_in_cells;
    int cell_size_in_pixels;
    char *cell;	/* array for keeping track of array of bits */
	Boolean showAll;  /* whether bitmap should display entire bitmap */

    /* private state */
    int cur_x, cur_y;  /* position of visible corner in big pixmap */
    Dimension	pixmap_width_in_pixels;
    Dimension	pixmap_height_in_pixels;
    Pixmap big_picture;
    GC		draw_gc;	/* one plane, for drawing into pixmap */
    GC		undraw_gc;	/* one plane, for drawing into pixmap */
    GC		copy_gc;	/* defaultdepthofscreen, for copying pixmap into window */
} BitmapEditPart;

/*
 * Full instance record declaration
 */
typedef struct _BitmapEditRec {
    CorePart		core;
    BitmapEditPart	bitmapEdit;
} BitmapEditRec;

#endif /* _ORABitmapEditP_h */
