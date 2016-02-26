/* $Header: eh.c,v 1.5 88/12/02 10:43:19 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include "draw.h"
#include "go.h"
#include "psio.h"
#include "psint.h"

static void *nullproc()
  {
  }
static void *nullselect(x, y)
int x, y;
  {
	fprintf(stderr, "Select not implemented for current mode\n");
  }
static void *nulladjust(x, y)
int x, y;
  {
	fprintf(stderr, "Adjust not implemented for current mode\n");
  }
static void *nullkey(c)
int c;
  {
	fprintf(stderr, "Keystroke not implemented for current mode\n");
  }
static void *nullreply(c)
int c;
  {
	fprintf(stderr, "Reply not implemented for current mode\n");
  }
static void *handleprop(c)
int c;
  {
	if(PropHandle(c) == FALSE)
		fprintf(stderr, "Unable to handle action/property %d\n", c);
  }
static void *handledamage(x0, y0, x1, y1)
int x0, y0, x1, y1;
  {
	GORepairItems(x0, y0, x1, y1);
  }
static void *nullbegin() { }
static void *nullend() { }

EHInit()
  {
	int k;
	for(k=0; k < MODEMAX; k++)
	  {
		ehprocs[k].handleselect	= nullselect;
		ehprocs[k].handleadjust	= nulladjust;
		ehprocs[k].handlekey	= nullkey;
		ehprocs[k].handlereply	= nullreply;
		ehprocs[k].handledamage = handledamage;
		ehprocs[k].handlebegin	= nullbegin;
		ehprocs[k].handleend	= nullend;
		ehprocs[k].handleprop	= handleprop;
	  }
	EHLineInit(&ehprocs[LINE]);
	EHPolyInit(&ehprocs[POLY]);
	EHBrushInit(&ehprocs[BRUSH]);
	EHRectInit(&ehprocs[RECT]);
	EHOvalInit(&ehprocs[OVAL]);
	EHCircInit(&ehprocs[CIRC]);
	EHTextInit(&ehprocs[TEXT]);
	EHStretchInit(&ehprocs[STRETCH]);
	EHRotateInit(&ehprocs[ROTATE]);
  }
