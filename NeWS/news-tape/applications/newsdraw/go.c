/* $Header: go.c,v 1.3 88/12/02 10:43:21 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include "draw.h"
#include "go.h"
#include "psio.h"
#include "psint.h"

static void *nullreadwrite(pitem, pfile)
ITEM *pitem;
FILE *pfile;
  {
  }
static void *nullproc()
  {
  }
static void *errorproc()
  {
	fprintf(stderr, "No object/method for current mode\n");
  }

GOInit()
  {
	int k;
	for(k=0; k < MODEMAX; k++)
	  {
		goprocs[k].holder	= errorproc;
		goprocs[k].new		= errorproc;
		goprocs[k].move		= errorproc;
		goprocs[k].rotate	= errorproc;
		goprocs[k].scale	= errorproc;
		goprocs[k].setpath	= errorproc;
		goprocs[k].draw		= errorproc;
		goprocs[k].print	= errorproc;
		goprocs[k].write	= nullreadwrite;
		goprocs[k].read		= nullreadwrite;
	  }
	GOItemInit();
	GOLineInit(&goprocs[LINE]);
	GORectInit(&goprocs[RECT]);
	GOOvalInit(&goprocs[OVAL]);
	GOCircInit(&goprocs[CIRC]);
	GOTextInit(&goprocs[TEXT]);
	GOPolyInit(&goprocs[POLY]);
	GOBrushInit(&goprocs[BRUSH]);
  }
