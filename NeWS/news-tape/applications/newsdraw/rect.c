/* $Header: rect.c,v 1.4 88/12/02 10:43:24 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include "go.h"
#include "gopvt.h"

/* rect section */
typedef struct {
	ITEMHEADER;
} IRECT;

static void *RectHolder()
  {
	return((IRECT *)malloc(sizeof(IRECT)));
  }

static void *RectNew(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	IRECT *pitem	= (IRECT *)RectHolder();
	pitem->type		= RECT;
	pitem->x0		= MIN(x0, x1);
	pitem->y0		= MIN(y0, y1);
	pitem->x1		= MAX(x0, x1);
	pitem->y1		= MAX(y0, y1);
	pitem->rotation	= 0;
	pitem->xscale	= 1;
	pitem->yscale	= 1;
	pitem->prop		= PropCopyOf(PropCurrent());
	return(pitem);
  }

static void *RectDraw(pitem)
IRECT *pitem;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	ps_gsave();
	if(fill >= 0.0)
	  {
		ps_setgray(fill);
		ps_drawrect(X0, Y0, X1, Y1);
		ps_fill();
	  }
	if(stroke >= 0.0)
	  {
		ps_setlinewidth(width);
		ps_setgray(stroke);
		ps_drawrect(X0, Y0, X1, Y1);
		ps_stroke();
	  }
	ps_grestore();
	return(pitem);
  }

static void *RectPrint(pitem, pfile)
IRECT *pitem;
FILE *pfile;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	fprintf(pfile, "%% rectangle\n");
	if(fill >= 0.0)
	  {
		fprintf(pfile, "%f setgray ", fill);
		fprintf(pfile, "%d %d %d %d rectpath ", X0, Y0, W, H);
		fprintf(pfile, "fill\n");
	  }
	if(stroke >= 0.0)
	  {
		fprintf(pfile, "%d setlinewidth ", width);
		fprintf(pfile, "%f setgray ", stroke);
		fprintf(pfile, "%d %d %d %d rectpath ", X0, Y0, W, H);
		fprintf(pfile, "stroke\n");
	  }
	return(pitem);
  }

GORectInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	pgoprocs->holder= RectHolder;
	pgoprocs->new	= RectNew;
	pgoprocs->draw	= RectDraw;
	pgoprocs->print	= RectPrint;
  }

/************* event handlers *************/

static void *RectSelect(x, y)
int x,y;
  {
	ps_rectcreateinteractive(x, y);
  }
static void *RectAdjust(x, y)
int x,y;
  {
	fprintf(stderr, "RectAdjust not implemented\n");
  }
static void *RectReply(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	ITEM *pitem;
	if((x0 == x1) && (y0 == y1)) return;
	pitem = GONewRect(x0, y0, x1, y1);
	GOItemAddToTop(pitem);
	GODrawItem(pitem);
	return(pitem);
  }
static void *RectBegin()
  {
	ps_xcursor();
  }
EHRectInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= RectSelect;
	pprocs->handleadjust	= RectAdjust;
	pprocs->handlereply		= RectReply;
	pprocs->handlebegin		= RectBegin;
  }

