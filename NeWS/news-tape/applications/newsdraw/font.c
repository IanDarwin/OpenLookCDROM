/* $Header: font.c,v 1.2 88/12/02 10:43:43 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>
#include "psint.h"

typedef struct {
	char *name;		/* family name */
	int id;			/* id token */
	int size;		/* point size  */
	int height;		/* heighest ascender to lowest descender */
	int descent;	/* lowest descender to baseline */
	int count;		/* chars in font */
	int *awidths;	/* array of char width info */
} FONT;

FONT *SetFont();

static FONT font;

FontInit()
  {
	SetFont("Times-Roman", 48);
  }

FONT *FontDefault() { return(&font); }
FONT *SetFont(name, size)
char *name;
int	size;
{
	int count, height, descent;
    int index = ps_next_user_token++;
    int i;
	FONT *pf = &font;
    ps_fontsetup(name, size, index, &count, &height, &descent);

    pf->size = size;
    pf->id = index;
    pf->count = count >> 1;
    pf->height = height;
    pf->descent = descent < 0 ? -descent : descent;
    pf->name = (char *)malloc(strlen(name));
    strcpy(pf->name, name);

	printf("font %s: id %d, count %d, height %d, descent %d\n",
		pf->name, pf->id, pf->count, pf->height, pf->descent);
	pf->awidths = (int *)malloc(pf->count * sizeof(int));
    for(i = pf->count - 1; i >= 0; --i)
	  {
		ps_getint(&pf->awidths[i]);
		/* printf("char %c (%x), width %d\n", i, i, pf->awidths[i]); */
	  }
    return pf;
}

FontDescent(pfont) FONT *pfont; { return(pfont->descent); }
FontHeight(pfont) FONT *pfont; { return(pfont->height); }
FontAscent(pfont) FONT *pfont; { return(pfont->height - pfont->descent); }

int FontTextWidth(pfont, ptext)
FONT *pfont;
char *ptext;
  {
	int w = 0;
	char *pc;
	for(pc = ptext; *pc; pc++)
	  {
		w += pfont->awidths[*pc];
	  }
	return(w);
  }
