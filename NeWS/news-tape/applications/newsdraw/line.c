/* $Header: line.c,v 1.3 88/12/02 10:43:37 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include <math.h>
#include "go.h"
#include "gopvt.h"

/* line section */
typedef struct {
	ITEMHEADER;
} ILINE;

static void *LineHolder()
  {
	return((ILINE *)malloc(sizeof(ILINE)));
  }

#define SQUARE(x) ((x)*(x))

static void *LineNew(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	double	length  =  sqrt((double)(SQUARE(x0 - x1) + SQUARE(y0 - y1)));

	ILINE *pitem	= (ILINE *)malloc(sizeof(ILINE));
	pitem->type		= LINE;
	pitem->rotation	=
		atan2((double)(y1 - y0), (double)(x1 - x0)) * 180.0 / M_PI;
	pitem->x0		= (int)(((double)x0 + (double)x1 - length) / 2.0);
	pitem->y0		= (y0 + y1) / 2;
	pitem->x1		= pitem->x0 + length;
	pitem->y1		= pitem->y0;
	pitem->xscale	= 1;
	pitem->yscale	= 1;
	pitem->prop		= PropCopyOf(PropCurrent());
	return(pitem);
  }

static void *LineDraw(pitem)
ILINE *pitem;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	ps_gsave();
	if(stroke >= 0.0)
	  {
		ps_setlinewidth(width);
		ps_setgray(stroke);
		ps_drawline(X0, Y0, X1, Y1);
		ps_stroke();
	  }
	ps_grestore();
	return(pitem);
  }

static void *LinePrint(pitem, pfile)
ILINE *pitem;
FILE *pfile;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);

	fprintf(pfile, "%% line\n");
	if(stroke >= 0.0)
	  {
		fprintf(pfile, "%d setlinewidth ", width);
		fprintf(pfile, "%f setgray ", stroke);
		fprintf(pfile, "%d %d %d %d linepath ", X0, Y0, X1, Y1);
		fprintf(pfile, "stroke\n");
	  }
	return(pitem);
  }

GOLineInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	pgoprocs->holder= LineHolder;
	pgoprocs->new	= LineNew;
	pgoprocs->draw	= LineDraw;
	pgoprocs->print	= LinePrint;
  }

/************* event handlers *************/

static void *LineSelect(x, y)
int x,y;
  {
	ps_linecreateinteractive(x, y);
  }
static void *LineAdjust(x, y)
int x,y;
  {
	fprintf(stderr, "LineAdjust not implemented\n");
  }
static void *LineReply(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	ITEM *pitem;
	if((x0 == x1) && (y0 == y1)) return;
	pitem = GONewLine(x0, y0, x1, y1);
	GOItemAddToTop(pitem);
	GODrawItem(pitem);
	return(pitem);
  }
static void *LineBegin()
  {
	ps_xcursor();
  }
EHLineInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= LineSelect;
	pprocs->handleadjust	= LineAdjust;
	pprocs->handlereply		= LineReply;
	pprocs->handlebegin		= LineBegin;
  }

