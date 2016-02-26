/* $Header: circ.c,v 1.3 88/12/02 10:43:36 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include "go.h"
#include "gopvt.h"

/* circ section */
typedef struct {
	ITEMHEADER;
} ICIRC;

static void *CircHolder()
  {
	return((ICIRC *)malloc(sizeof(ICIRC)));
  }

extern double sqrt();
static void *CircNew(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	ICIRC *pitem	= (ICIRC *)malloc(sizeof(ICIRC));
	double	xlen	 = (double)((x0 - x1)/2);
	double	ylen	 = (double)((y0 - y1)/2);
	double xcenter	 = (double)((x0 + x1)/2);
	double ycenter	 = (double)((y0 + y1)/2);
	double radius	= (int)sqrt((double)(xlen*xlen + ylen*ylen));
	pitem->type		= CIRC;
	pitem->x0		= xcenter - radius;
	pitem->y0		= ycenter - radius;
	pitem->x1		= xcenter + radius;
	pitem->y1		= ycenter + radius;
	pitem->rotation	= 0;
	pitem->xscale	= 1;
	pitem->yscale	= 1;
	pitem->prop		= PropCopyOf(PropCurrent());
	return(pitem);
  }

static void *CircDraw(pitem)
ICIRC *pitem;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	ps_gsave();
	if(fill >= 0.0)
	  {
		ps_setgray(fill);
		ps_drawcirc((X0 + X1)/2, (Y0 + Y1)/2, (X1 - X0)/2);
		ps_fill();
	  }
	if(stroke >= 0.0)
	  {
		ps_setlinewidth(width);
		ps_setgray(stroke);
		ps_drawcirc((X0 + X1)/2, (Y0 + Y1)/2, (X1 - X0)/2);
		ps_stroke();
	  }
	ps_grestore();
	return(pitem);
  }

static void *CircPrint(pitem, pfile)
ICIRC *pitem;
FILE *pfile;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	fprintf(pfile, "%% circle\n");
	if(fill >= 0.0)
	  {
		fprintf(pfile, "%f setgray ", fill);
		fprintf(pfile, "%d %d %d circpath ",
			(X0 + X1)/2, (Y0 + Y1)/2, (X1 - X0)/2);
		fprintf(pfile, "fill\n");
	  }
	if(stroke >= 0.0)
	  {
		fprintf(pfile, "%d setlinewidth ", width);
		fprintf(pfile, "%f setgray ", stroke);
		fprintf(pfile, "%d %d %d circpath ",
			(X0 + X1)/2, (Y0 + Y1)/2, (X1 - X0)/2);
		fprintf(pfile, "stroke\n");
	  }
	return(pitem);
  }

GOCircInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	pgoprocs->holder= CircHolder;
	pgoprocs->new	= CircNew;
	pgoprocs->draw	= CircDraw;
	pgoprocs->print	= CircPrint;
  }

/************* event handlers *************/

static void *CircSelect(x, y)
int x,y;
  {
	ps_circcreateinteractive(x, y);
  }
static void *CircAdjust(x, y)
int x,y;
  {
	fprintf(stderr, "CircAdjust not implemented\n");
  }
static void *CircReply(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	/* maybe we should just make this an oval! */
	ITEM *pitem;
	if((x0 == x1) && (y0 == y1)) return;
	pitem = GONewCirc(x0, y0, x1, y1);
	GOItemAddToTop(pitem);
	GODrawItem(pitem);
	return(pitem);
  }
static void *CircBegin()
  {
	ps_xcursor();
  }
EHCircInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= CircSelect;
	pprocs->handleadjust	= CircAdjust;
	pprocs->handlereply		= CircReply;
	pprocs->handlebegin		= CircBegin;
  }
