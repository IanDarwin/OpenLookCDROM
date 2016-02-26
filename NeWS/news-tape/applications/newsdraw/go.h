/* $Header: go.h,v 1.6 88/12/02 10:43:22 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#ifndef GO_H
#define GO_H

#include "draw.h"
#include "psint.h"

typedef struct {
	void *(*holder)();			/* item pointer  */
	void *(*new)();				/* new item      */
	void *(*move)();			/* move item     */
	void *(*rotate)();			/* rotate item   */
	void *(*scale)();			/* scale item    */
	void *(*setpath)();			/* set path for int draw */
	void *(*draw)();			/* draw on screen */
	void *(*print)();		    /* write postscript file  */
	void *(*write)();		    /* write draw file */
	void *(*read)();		    /* read draw file  */
} GOPROCS;

GOPROCS goprocs[MODEMAX];

typedef struct item ITEM;
typedef struct font FONT;
typedef struct prop PROP;

#define GONewLine(x0, y0, x1, y1) (ITEM *)goprocs[LINE].new(x0, y0, x1, y1)
#define GONewRect(x0, y0, x1, y1) (ITEM *)goprocs[RECT].new(x0, y0, x1, y1)
#define GONewOval(x0, y0, x1, y1) (ITEM *)goprocs[OVAL].new(x0, y0, x1, y1)
#define GONewCirc(x0, y0, x1, y1) (ITEM *)goprocs[CIRC].new(x0, y0, x1, y1)
#define GONewText(x0, y0,pc, pfont) (ITEM *)goprocs[TEXT].new(x0, y0, pc, pfont)
#define GONewPencil(x0, y0, apoint, length) \
	(ITEM *)goprocs[PENCIL].new(apoint, length)

#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define ABS(x)   (((x) > 0) ? (x) : (-(x)))
#define POS(x)   (((x) >= 0) ? 1 : 0)
#define NEG(x)   (((x) >= 0) ? 0 : 1)

extern float GOItemGetRotation();
extern ITEM *GOAddItem();
extern ITEM *GODrawItem();

extern FONT *FontDefault();
float PropStrokeColor();
float PropFillColor();
float PropTextColor();
FONT *PropFont();
PROP *PropCopyOf();
PROP *PropCurrent();
PROP *PropRead();

#endif
