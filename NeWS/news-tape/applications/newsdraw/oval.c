/* $Header: oval.c,v 1.3 88/12/02 10:43:39 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include "go.h"
#include "gopvt.h"

/* oval section */
typedef struct {
	ITEMHEADER;
} IOVAL;

static void *OvalHolder()
  {
	return((IOVAL *)malloc(sizeof(IOVAL)));
  }


static void *OvalNew(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	IOVAL *pitem	= (IOVAL *)malloc(sizeof(IOVAL));
	pitem->type		= OVAL;
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

static void *OvalDraw(pitem)
IOVAL *pitem;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	ps_gsave();
	if(fill >= 0.0)
	  {
		ps_setgray(fill);
		ps_drawoval(X0, Y0, X1, Y1);
		ps_fill();
	  }
	if(stroke >= 0.0)
	  {
		ps_setlinewidth(width);
		ps_setgray(stroke);
		ps_drawoval(X0, Y0, X1, Y1);
		ps_stroke();
	  }
	ps_grestore();
	return(pitem);
  }

static void *OvalPrint(pitem, pfile)
IOVAL *pitem;
FILE *pfile;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	fprintf(pfile, "%% oval\n");
	if(fill >= 0.0)
	  {
		fprintf(pfile, "%f setgray ", fill);
		fprintf(pfile, "%d %d %d %d ovalpath ", X0, Y0, W, H);
		fprintf(pfile, "fill\n");
	  }
	if(stroke >= 0.0)
	  {
		fprintf(pfile, "%d setlinewidth ", width);
		fprintf(pfile, "%f setgray ", stroke);
		fprintf(pfile, "%d %d %d %d ovalpath ", X0, Y0, W, H);
		fprintf(pfile, "stroke\n");
	  }
	return(pitem);
  }

GOOvalInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	pgoprocs->holder= OvalHolder;
	pgoprocs->new	= OvalNew;
	pgoprocs->draw	= OvalDraw;
	pgoprocs->print	= OvalPrint;
  }

/************* event handlers *************/

static void *OvalSelect(x, y)
int x,y;
  {
	ps_ovalcreateinteractive(x, y);
  }
static void *OvalAdjust(x, y)
int x,y;
  {
	fprintf(stderr, "OvalAdjust not implemented\n");
  }
static void *OvalReply(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	ITEM *pitem;
	if((x0 == x1) && (y0 == y1)) return;
	pitem = GONewOval(x0, y0, x1, y1);
	GOItemAddToTop(pitem);
	GODrawItem(pitem);
	return(pitem);
  }
static void *OvalBegin()
  {
	ps_xcursor();
  }
EHOvalInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= OvalSelect;
	pprocs->handleadjust	= OvalAdjust;
	pprocs->handlereply		= OvalReply;
	pprocs->handlebegin		= OvalBegin;
  }

