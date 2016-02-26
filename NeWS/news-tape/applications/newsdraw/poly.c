/* $Header: poly.c,v 1.2 88/12/02 10:43:49 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include <math.h>
#include "go.h"
#include "gopvt.h"

/* poly section */
typedef struct {
	int x;
	int y;
} POINT;

typedef struct {
	ITEMHEADER;
	POINT *apoint;
	int length;
	int closepath;
} IPOLY;

static void *PolyHolder()
  {
	return((IPOLY *)malloc(sizeof(IPOLY)));
  }

static PolyMinMax(pitem, apoint, length)
IPOLY *pitem;
POINT apoint[];
int length;
  {
	int i;
	int x0 = apoint[0].x;
	int y0 = apoint[0].y;
	int x1 = x0;
	int y1 = y0;

	/* find bounds */
	for(i = 1; i < length; i++)
	  {
		x0 = MIN(x0, apoint[i].x);
		y0 = MIN(y0, apoint[i].y);
		x1 = MAX(x1, apoint[i].x);
		y1 = MAX(y1, apoint[i].y);
	  }

	/* make relative */
	for(i = length - 1; i > 0; i--)
	  {
		apoint[i].x -= apoint[i - 1].x;
		apoint[i].y -= apoint[i - 1].y;
	  }
	apoint[0].x -= x0;
	apoint[0].y -= y0;

	pitem->x0 = x0;
	pitem->y0 = y0;
	pitem->x1 = x1;
	pitem->y1 = y1;
	pitem->apoint = apoint;
	pitem->length = length;
  }

static void *PolyNew(apoint, length, closepath)
POINT apoint[];	/* apoint will not be copied by this proc! */
int length;
int closepath;
  {
	IPOLY *pitem	= (IPOLY *)PolyHolder();
	pitem->type		= POLY;
	pitem->rotation	= 0;
	pitem->closepath= closepath;

	/* take care of x0,y0,x1,y1 */
	PolyMinMax(pitem, apoint, length);
	pitem->xscale	= 1;
	pitem->yscale	= 1;
	pitem->prop		= PropCopyOf(PropCurrent());
	return(pitem);
  }

static void *PolyDraw(pitem)
IPOLY *pitem;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);
	int	i;

	ps_gsave();
	if(fill >= 0.0)
	  {
		ps_setgray(fill);
		ps_moveto(pitem->x0, pitem->y0);
		ps_rmoveto(pitem->apoint[0].x, pitem->apoint[0].y);
		for(i = 1; i < pitem->length; i++)
			ps_rlineto(pitem->apoint[i].x, pitem->apoint[i].y);
		ps_closepath();
		ps_fill();
	  }
	if(stroke >= 0.0)
	  {
		ps_setlinewidth(width);
		ps_setgray(stroke);
		ps_moveto(pitem->x0, pitem->y0);
		ps_rmoveto(pitem->apoint[0].x, pitem->apoint[0].y);
		for(i = 1; i < pitem->length; i++)
			ps_rlineto(pitem->apoint[i].x, pitem->apoint[i].y);
		if(pitem->closepath) ps_closepath();
		ps_stroke();
	  }
	ps_grestore();
	return(pitem);
  }

static void *PolyPrint(pitem, pfile)
IPOLY *pitem;
FILE *pfile;
  {
	float stroke	= PropStrokeColor(pitem->prop);
	float fill		= PropFillColor(pitem->prop);
	int   width		= PropLineWidth(pitem->prop);
	int   i;

	fprintf(pfile, "%% poly\n");
	if(fill >= 0.0)
	  {
		fprintf(pfile, "%f setgray ", fill);
		fprintf(pfile, "%d %d moveto ", pitem->x0, pitem->y0);
		fprintf(pfile, "%d %d rmoveto ",
			pitem->apoint[0].x, pitem->apoint[0].y);
		fprintf(pfile, "\n");

		for(i = 1; i < pitem->length; i++)
		  {
			fprintf(pfile, "%d %d rlineto \n",
				pitem->apoint[i].x, pitem->apoint[i].y);
		  }
		fprintf(pfile, "closepath fill\n");
	  }
	if(stroke >= 0.0)
	  {
		fprintf(pfile, "%d setlinewidth ", width);
		fprintf(pfile, "%f setgray ", stroke);
		fprintf(pfile, "%d %d moveto ", pitem->x0, pitem->y0);
		fprintf(pfile, "%d %d rmoveto ",
			pitem->apoint[0].x, pitem->apoint[0].y);
		fprintf(pfile, "\n");

		for(i = 1; i < pitem->length; i++)
		  {
			fprintf(pfile, "%d %d rlineto ",
				pitem->apoint[i].x, pitem->apoint[i].y);
		  }
		if(pitem->closepath) fprintf(pfile, "closepath ");
		fprintf(pfile, "stroke\n");
	  }
	return(pitem);
  }

static void *PolyWrite(pitem, pfile)
IPOLY *pitem;
FILE *pfile;
  {
	int i;
	fprintf(pfile, "%d ", pitem->closepath);
	fprintf(pfile, "%d\n", pitem->length);
	for(i = 0; i < pitem->length; i++)
		fprintf(pfile, "%d %d\n", pitem->apoint[i].x, pitem->apoint[i].y);
  }

static void *PolyRead(pitem, pfile)
IPOLY *pitem;
FILE *pfile;
  {
	int i;
	if(1 != fscanf(pfile, "%d", &pitem->closepath))
	  {
		fprintf(stderr, "Error reading poly...exiting");
		exit(1);
	  }
	if(1 != fscanf(pfile, "%d", &pitem->length))
	  {
		fprintf(stderr, "Error reading poly...exiting");
		exit(1);
	  }
	pitem->apoint = (POINT *)malloc(pitem->length * sizeof(POINT));
	for(i = 0; i < pitem->length; i++)
	  {
		if(2 != fscanf(pfile, "%d %d",
			&pitem->apoint[i].x, &pitem->apoint[i].y))
		  {
			fprintf(stderr, "Error reading poly...exiting");
			exit(1);
		  }
	  }

	while(getc(pfile) != '\n') ;
  }


GOPolyInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	pgoprocs->holder= PolyHolder;
	pgoprocs->new	= PolyNew;
	pgoprocs->draw	= PolyDraw;
	pgoprocs->print	= PolyPrint;
	pgoprocs->read	= PolyRead;
	pgoprocs->write	= PolyWrite;
  }

/* Brushes and polygons draw the same! */
GOBrushInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	GOPolyInit(pgoprocs);
  }

/************* event handlers *************/

static POINT *pbuf = (POINT *)0;
static POINT *pcurrent;
static int lcurrent;
#define MAXPOINTS 100
static polyBeginCurrent(x, y)
int x, y;
  {
	lcurrent = 1;
	pbuf = pcurrent = (POINT *)malloc(MAXPOINTS * sizeof(POINT));
	pcurrent->x = x;
	pcurrent->y = y;
  }
static polyAddPoint(x, y)
int x, y;
  {
	lcurrent++;
	if(lcurrent >= MAXPOINTS)
	  {
		fprintf(stderr, "too many points in polygon\n");
		return;
	  }
	ps_drawline(pcurrent->x, pcurrent->y, x, y);
	ps_stroke();

	pcurrent++;
	pcurrent->x = x;
	pcurrent->y = y;
  }
static polyEndCurrent()
  {
	ITEM *pitem;
	POINT *ppoint;

	if(lcurrent == 1)
	  {
		free(pbuf);
		return;
	  }

	/* polyAddPoint(pbuf->x, pbuf->y); */
	ppoint = (POINT *)malloc(lcurrent * sizeof(POINT));
	bcopy(pbuf, ppoint, lcurrent * sizeof(POINT));
	pitem = PolyNew(ppoint, lcurrent, 1);
	GOItemAddToTop(pitem);
	GODrawItem(pitem);

	free(pbuf);
	pbuf = 0;
	lcurrent = 1;
  }
static void *PolySelect(x, y)
int x,y;
  {
	if(pbuf)
		return;

	polyBeginCurrent(x, y);
	ps_linecreateinteractive(x, y);
  }
static void *PolyAdjust(x, y)
int x,y;
  {
  }
static void *PolyReply(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	if((lcurrent != 1) && (pcurrent->x == x1) && (pcurrent->y == y1))
	  {
		polyAddPoint(x1, y1);
		polyEndCurrent();
		return;
	  }
	polyAddPoint(x1, y1);
	ps_linecreateinteractive(x1, y1);
  }
static void *PolyEnd()
  {
	if(pbuf)
	  {
		polyEndCurrent();
	  }
  }
static void *PolyBegin()
  {
	ps_xcursor();
  }
EHPolyInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= PolySelect;
	pprocs->handleadjust	= PolyAdjust;
	pprocs->handlereply		= PolyReply;
	pprocs->handlebegin		= PolyBegin;
	pprocs->handlebegin		= PolyEnd;
  }

/************* event handlers *************/

static void *BrushSelect(x, y)
int x,y;
  {
	ps_pencilCreateinteractive(x, y);
  }
static void *BrushAdjust(x, y)
int x,y;
  {
  }
static void *BrushReply(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	ITEM *pitem;
	int length;
	int i;
	POINT *ppoint;

	ps_getint(&length);
	ppoint = (POINT *)malloc(length * sizeof(POINT));
	for(i = 0; i < length; i++)
	  {
		ps_getint(&ppoint[i].x);
		ps_getint(&ppoint[i].y);
	  }

	pitem = PolyNew(ppoint, length, 0);
	GOItemAddToTop(pitem);
	GODrawItem(pitem);
	return(pitem);
  }
static void *BrushEnd()
  {
  }
static void *BrushBegin()
  {
	ps_xcursor();
  }
EHBrushInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= BrushSelect;
	pprocs->handleadjust	= BrushAdjust;
	pprocs->handlereply		= BrushReply;
	pprocs->handlebegin		= BrushBegin;
	pprocs->handlebegin		= BrushEnd;
  }

