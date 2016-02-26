/* $Header: text.c,v 1.3 88/12/02 10:43:41 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */

#include <stdio.h>
#include "go.h"
#include "gopvt.h"

/* text section */
typedef struct {
	ITEMHEADER;
	char *ptext;
} ITEXT;

static void *TextHolder()
  {
	return((ITEXT *)malloc(sizeof(ITEXT)));
  }


extern double sqrt();
static void *TextNew(x0, y0, ptext)
int x0, y0;
char *ptext;
  {
	ITEXT *pitem	= (ITEXT *)malloc(sizeof(ITEXT));

	pitem->prop		= PropCopyOf(PropCurrent());

	pitem->type		= TEXT;
	pitem->rotation	= 0;
	pitem->xscale	= 1;
	pitem->yscale	= 1;
	pitem->ptext	= (char *)malloc(strlen(ptext));
	strcpy(pitem->ptext, ptext);

	pitem->y0		= y0 - FontDescent(PropFont(pitem->prop));
	pitem->y1		= y0 + FontAscent(PropFont(pitem->prop));
	pitem->x0		= x0;
	pitem->x1		= x0 + FontTextWidth(PropFont(pitem->prop), ptext);
	return(pitem);
  }

static void *TextDraw(pitem)
ITEXT *pitem;
  {
	float text		= PropTextColor(pitem->prop);
	FONT *pfont		= PropFont(pitem->prop);

	ps_gsave();
	if(text >= 0.0)
	  {
		ps_setgray(text);
		ps_moveto(pitem->x0, pitem->y0 + FontDescent(pfont));
		ps_show(pitem->ptext);
	  }
	ps_grestore();
	return(pitem);
  }

static void *TextPrint(pitem, pfile)
ITEXT *pitem;
FILE *pfile;
  {
	float text		= PropTextColor(pitem->prop);
	FONT *pfont		= PropFont(pitem->prop);
	if(text >= 0.0)
	  {
		fprintf(pfile, "%f setgray ", text);
		fprintf(pfile, "%d %d moveto ",
			pitem->x0, pitem->y0 + FontDescent(pfont));
		fprintf(pfile, "(%s) show\n", pitem->ptext);
	  }
	return(pitem);
  }

static void *TextWrite(pitem, pfile)
ITEXT *pitem;
FILE *pfile;
  {
	char *pc;
	pc = pitem->ptext;
	while((*pc != 0) && (*pc != '\n') && (*pc != 10))
	  {
		fputc(*pc, pfile);
		pc++;
	  }
	fputc('\n', pfile);
	/* fprintf(pfile, "%s\n", pitem->ptext); */
  }

static void *TextRead(pitem, pfile)
ITEXT *pitem;
FILE *pfile;
  {
	char buf[256];
	fgets(buf, 256, pfile);
	pitem->ptext	= (char *)malloc(strlen(buf));
	strcpy(pitem->ptext, buf);
  }

GOTextInit(pgoprocs)
GOPROCS *pgoprocs;
  {
	pgoprocs->holder= TextHolder;
	pgoprocs->new	= TextNew;
	pgoprocs->draw	= TextDraw;
	pgoprocs->print	= TextPrint;
	pgoprocs->write	= TextWrite;
	pgoprocs->read	= TextRead;
  }

/************* event handlers *************/

static int iText = 0;
static char aText[255];
static int textx, texty;
static int bSaved = 0;
static void *TextBegin()
  {
	ps_xcursor();
  }
static void *TextSelect(x, y)
int x,y;
  {
	int ascent = FontAscent(FontDefault());
	int descent = FontDescent(FontDefault());
	if(iText != 0)
	  {
		TextComplete();
	  }
	iText = 0;
	aText[0] = 0;
	textx = x;
	texty = y;

	if(bSaved) ps_restorearea();
	ps_savearea(textx - 6, texty - descent - 1, 1000, texty + ascent + 1);
	/* ps_savearea(0, 0, 1000, 1000); */
	bSaved = 1;
	ps_setgray(1.0);
	ps_drawrect(textx - 4, texty - descent - 1, textx + 4, texty + ascent + 1);
	ps_fill();
	ps_setgray(0.0);
	ps_drawline(textx, texty - descent, textx, texty + ascent);
	ps_drawline(textx - 3, texty, textx + 3, texty);
	ps_stroke();
  }
static void *TextAdjust(x, y)
int x,y;
  {
	fprintf(stderr, "TextAdjust not implemented\n");
  }
static void *TextKey(key)
int key;
  {
	int ascent = FontAscent(FontDefault());
	int descent = FontDescent(FontDefault());
	/* fprintf(stderr, "TextKey %c (0x%x)\n", key, key); */
	switch(key)
	  {
		default:
			aText[iText] = key;
			iText++;
			aText[iText] = 0;
			break;
	    case 0177:
	    case '\b':
			if(iText)
			  {
				iText--;
				aText[iText] = 0;
			  }
			break;
	  }
	ps_restorearea();

	ps_setgray(PropTextColor(PropCurrent()));
	ps_moveto(textx, texty);
	ps_show(aText);
	ps_rlineto(0, ascent);
	ps_rlineto(0, -ascent - descent);
	ps_rmoveto(0, descent);
	ps_rmoveto(-3, 0);
	ps_rlineto(6, 0);
	ps_stroke();
  }
static TextComplete()
  {
	ITEM *pitem;

	ps_restorearea();
	bSaved = 0;

	pitem = GONewText(textx, texty, aText, FontDefault());
	GOItemAddToTop(pitem);
	GODrawItem(pitem);
	/*
	ps_moveto(textx, texty);
	ps_show(aText);
	*/

	iText = 0;
	aText[0] = 0;
  }
static void *TextEnd()
  {
	if(iText)
		TextComplete();
	else if(bSaved)
		ps_restorearea();
  }

static void *TextProp(c)
int c;
  {
	PropHandle(c);
	if(iText != 0)
	  {
		ps_setgray(PropTextColor(PropCurrent()));
		ps_moveto(textx, texty);
		ps_show(aText);
	  }
  }
EHTextInit(pprocs)
EHPROCS *pprocs;
  {
	pprocs->handleselect	= TextSelect;
	pprocs->handleadjust	= TextAdjust;
	pprocs->handlekey		= TextKey;
	pprocs->handlebegin		= TextBegin;
	pprocs->handleend		= TextEnd;
	pprocs->handleprop		= TextProp;
  }

