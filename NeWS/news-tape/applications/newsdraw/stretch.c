/* $Header: stretch.c,v 1.5 88/12/02 10:43:46 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */

#include <stdio.h>
#include "draw.h"
#include "go.h"
#include "psio.h"
#include "psint.h"
#include "comm.h"

static ITEM  *pcurrent = (ITEM *)0;

static ITEM *SRSelectItem();
static ITEM *SRDeselectItem();
static void *StretchMoveReply();

#define B 3		/* selection border */
#define B1 2	/* selection border inner */

/* shared stretch/rotate stuff */
static void *SRBegin()
  {
	ps_stdcursor();
  }
static void *SREnd()
  {
	if(pcurrent)
		SRDeselectItem(pcurrent);
	pcurrent = (ITEM *)0;
  }
static void *SRProp(c)
int c;
  {
	ITEM *pitem = pcurrent;
	if(pitem)
		SRDeselectItem(pitem);

	PropHandle(c);

	/* this should be in action handle! */
	if(pitem)
	  {
		if(c == TOTOP_KEY)
		  {
			GOItemDelete(pitem);
			GOItemAddToTop(pitem);
		  }
		if(c == TOBOT_KEY)
		  {
			GOItemDelete(pitem);
			GOItemAddToBottom(pitem);
		  }
	  }

	/* set property on selected item */
	if(pitem)
	  {
		int x0, x1, y0, y1;
		GOItemSetProp(pitem, PropCopyOf(PropCurrent()));
		GOItemRotatedBounds(pitem, &x0, &y0, &x1, &y1);
		GORepairItems(x0 - 10, y0 - 10, x1 + 10, y1 + 10);	/* 10 hack */
		SRSelectItem(pitem);
	  }
  }
static void *SRAdjust(x, y)
int x,y;
  {
  }
static void *SRKey(key)
int key;
  {
	int x0, x1, y0, y1;

	if(pcurrent)
	  {
		if((key == 0177) || (key = 10)) /* delete or BS key */
		  {
			GOItemDelete(pcurrent);
			GOItemRotatedBounds(pcurrent, &x0, &y0, &x1, &y1);
			GORepairItems(x0 - B, y0 - B, x1 + B, y1 + B);
			/* GOFreeItem(pcurrent); */
			pcurrent = (ITEM *)0;
		  }
	  }
  }

/* stretch selection */
static void *StretchSelect(x, y)
int x,y;
  {
	ITEM *pfound = (ITEM *)GOFindItem(x, y);
	if((pcurrent) && (pcurrent == pfound))
	  {
		/* move or stretch */
		int x0, y0, x1, y1;
		int bL, bT, bB, bR;
		int type = GOItemType(pcurrent);
		GOItemUnrotatedBounds(pcurrent, &x0, &y0, &x1, &y1);

		bT = bB = bL = bR = 0;
		if(x < (x0 + B)) bL = 1;
		if(y < (y0 + B)) bB = 1;
		if(x > (x1 - B)) bR = 1;
		if(y > (y1 - B)) bT = 1;

		if     (bL && bT)
			ehprocs[type].handleselect(x1, y0);
		else if(bL && bB)
			ehprocs[type].handleselect(x1, y1);
		else if(bR && bT)
			ehprocs[type].handleselect(x0, y0);
		else if(bR && bB)
			ehprocs[type].handleselect(x0, y1);
		else
		  {
			ehprocs[STRETCH].handlereply	= StretchMoveReply;
			ps_moveinteractive(x, y,
				GOItemGetRotation(pcurrent), x0, y0, x1, y1);
		  }
	  }
	else
	  {
		if(pcurrent)
			SRDeselectItem(pcurrent);

		pcurrent = pfound;
		if(pcurrent)
			SRSelectItem(pcurrent);
	  }
  }
static void *StretchMoveReply(xstart, ystart, xend, yend)
int xstart, ystart, xend, yend;
  {
	int x0, y0, x1, y1;
	int s0, t0, s1, t1;
	GOItemRotatedBounds(pcurrent, &x0, &y0, &x1, &y1);
	GOItemMove(pcurrent, xend - xstart, yend - ystart);
	GOItemRotatedBounds(pcurrent, &s0, &t0, &s1, &t1);

	if((x0 > s1) || (x1 < s0) || (y0 > t1) || (y1 < t0))
	  {
		/* no intersection */
		/* printf("no intersect\n"); */
		GORepairItems(x0 - B, y0 - B, x1 + B, y1 + B);
		GORepairItems(s0, t0, s1, t1);
	  }
	else
	  {
		/* the regions intersect */
		/* printf("intersection\n"); */
		GORepairItems(  MIN(s0, x0 - B), MIN(t0, y0 - B),
						MAX(s1, x1 + B), MAX(t1, y1 + B)	);
	  }

	saveareaunderitem(pcurrent);
	GOItemUnrotatedBounds(pcurrent, &x0, &y0, &x1, &y1);
	drawselection(GOItemGetRotation(pcurrent), x0, y0, x1, y1);
  }
EHStretchInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handlebegin		= SRBegin;
	pprocs->handleend		= SREnd;
	pprocs->handleprop		= SRProp;
	pprocs->handleselect	= StretchSelect;
	pprocs->handleadjust	= SRAdjust;
	pprocs->handlekey		= SRKey;
  }

/* rotate selection */
static void *RotateSelect(x, y)
int x,y;
  {
	ITEM *pfound = (ITEM *)GOFindItem(x, y);
	if(pcurrent && pcurrent == pfound)
	  {
		/* rotate */
		int x0, y0, x1, y1;
		GOItemUnrotatedBounds(pcurrent, &x0, &y0, &x1, &y1);
		ps_rotateInteractive(
			GOItemGetRotation(pcurrent),
			x, y, (x0 + x1)/2, (y0 + y1)/2, x0, y0, x1, y1);
	  }
	else
	  {
		if(pcurrent)
			SRDeselectItem(pcurrent);

		pcurrent = pfound;
		if(pcurrent)
			SRSelectItem(pcurrent);
	  }
  }
static void *RotateReply(angle)
float angle;
  {
	int x0, y0, x1, y1;
	int s0, t0, s1, t1;
	GOItemRotatedBounds(pcurrent, &x0, &y0, &x1, &y1);
	GOItemSetRotation(pcurrent, angle);
	GOItemRotatedBounds(pcurrent, &s0, &t0, &s1, &t1);
	GORepairItems(
		MIN(x0, s0) - B, MIN(y0, t0) - B,
	 	MAX(x1, s1) + B, MAX(y1, t1) + B);
	saveareaunderitem(pcurrent);
	GOItemUnrotatedBounds(pcurrent, &x0, &y0, &x1, &y1);
	drawselection(angle, x0, y0, x1, y1);
  }
EHRotateInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handlebegin		= SRBegin;
	pprocs->handleend		= SREnd;
	pprocs->handleprop		= SRProp;
	pprocs->handlekey		= SRKey;
	pprocs->handleselect	= RotateSelect;
	pprocs->handleadjust	= SRAdjust;
	pprocs->handlereply		= RotateReply;
  }

/* private stuff */
static ITEM *SRSelectItem(pitem)
ITEM *pitem;
  {
	if(pcurrent)
	  {
		fprintf("GOSelectItem:  Internal error\n");
	  }
	if(pitem)
	  {
		int x0, y0, x1, y1;
		float angle;

		pcurrent = pitem;
		saveareaunderitem(pitem);

		angle = GOItemGetRotation(pitem);
		GOItemUnrotatedBounds(pitem, &x0, &y0, &x1, &y1);
		drawselection(angle, x0, y0, x1, y1);

		PropSetCurrent(GOItemProp(pitem));
	  }
	return(pitem);
  }

static ITEM *SRDeselectItem(pitem)
ITEM *pitem;
  {
	if(!pcurrent)
	  {
		fprintf(stderr, "SRDeselectItem: Internal error\n");
		return(pitem);
	  }
	else
	  {
		ps_restorearea();
	  }
	pcurrent = (ITEM *)0;
	return(pitem);
  }

static drawselection(angle, x0, y0, x1, y1)
float angle;
int x0, y0, x1, y1;
  {
	int x2 = (x0 + x1)/2;
	int y2 = (y0 + y1)/2;

	ps_flush_PostScript();
	ps_dorotate(x2, y2, angle);

	ps_setgray(1.0);
	ps_drawrect(x0 - B, y0 - B, x0 + B, y0 + B);
	ps_drawrect(x1 - B, y0 - B, x1 + B, y0 + B);
	ps_drawrect(x1 - B, y1 - B, x1 + B, y1 + B);
	ps_drawrect(x0 - B, y1 - B, x0 + B, y1 + B);
	ps_drawrect(x0 - B, y2 - B, x0 + B, y2 + B);
	ps_drawrect(x1 - B, y2 - B, x1 + B, y2 + B);
	ps_drawrect(x2 - B, y0 - B, x2 + B, y0 + B);
	ps_drawrect(x2 - B, y1 - B, x2 + B, y1 + B);
	ps_drawrect(x2 - B, y2 - B, x2 + B, y2 + B);
	ps_fill();

	ps_setgray(0.0);
	ps_drawrect(x0 - B1, y0 - B1, x0 + B1, y0 + B1);
	ps_drawrect(x1 - B1, y0 - B1, x1 + B1, y0 + B1);
	ps_drawrect(x1 - B1, y1 - B1, x1 + B1, y1 + B1);
	ps_drawrect(x0 - B1, y1 - B1, x0 + B1, y1 + B1);
	ps_drawrect(x0 - B1, y2 - B1, x0 + B1, y2 + B1);
	ps_drawrect(x1 - B1, y2 - B1, x1 + B1, y2 + B1);
	ps_drawrect(x2 - B1, y0 - B1, x2 + B1, y0 + B1);
	ps_drawrect(x2 - B1, y1 - B1, x2 + B1, y1 + B1);
	ps_drawrect(x2 - B1, y2 - B1, x2 + B1, y2 + B1);
	ps_fill();

	ps_unrotate();
	ps_flush_PostScript();
  }

saveareaunderitem(pitem)
ITEM *pitem;
  {
	int x0, y0, x1, y1;
	GOItemRotatedBounds(pitem, &x0, &y0, &x1, &y1);
	ps_savearea(x0 - B, y0 - B, x1 + B, y1 + B);
	/*printf("save area (%d,%d), (%d,%d)\n", x0 - B, y0 - B, x1 + B, y1 + B);*/
  }
