/* $Header: prop.c,v 1.4 88/12/02 10:43:45 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */

#include <stdio.h>
#include "psint.h"
#include "go.h"
#include "comm.h"

typedef struct prop {
	int		linewidth;
	float	strokecolor;
	float	fillcolor;
	float	textcolor;
	FONT	*pfont;
};

static PROP *propcurrent;
static PROP *proplast;

PROP *PropNew();

PropInit()
  {
	proplast	= (PROP *)0;
	propcurrent	= PropNew();
	propcurrent->linewidth		= 1;
	propcurrent->strokecolor	= 0.0;
	propcurrent->fillcolor		= .75;
	propcurrent->textcolor		= 0.0;
	propcurrent->pfont			= FontDefault();
  }

PROP *PropCopyOf(prop)
PROP *prop;
  {
	PROP *propNew = PropNew();
	*propNew = *prop;
	return(propNew);
  }

PROP *PropNew()
  {
	return((PROP *)malloc(sizeof(PROP)));
  }

PROP *PropSetCurrent(prop)
PROP *prop;
  {
	*propcurrent = *prop;
	return(prop);
  }

PROP *PropCurrent()
  {
	return(propcurrent);
  }

int PropHandle(id) /* actually returns boolean */
int id;
  {
	int handled = TRUE;
	switch(id)
	  {
		default:
			handled = FALSE;
			break;

		/* properties */
		case FILL_KEY:
			ps_getfloat(&propcurrent->fillcolor);
			break;
		case STROKE_KEY:
			ps_getfloat(&propcurrent->strokecolor);
			break;
		case TEXT_KEY:
			ps_getfloat(&propcurrent->textcolor);
			break;
		case WIDTH_KEY:
			ps_getint(&propcurrent->linewidth);
			break;

		case PRINT_KEY:
			Print();
			break;
		case WRITE_KEY:
			Write();
			break;
		case READ_KEY:
			Read();
			break;
		case REDRAW_KEY:
			GODrawItems();
			break;
	  }
	return(handled);
  }

float PropStrokeColor(prop) PROP *prop; { return(prop->strokecolor); }
float PropFillColor(prop) PROP *prop; { return(prop->fillcolor); }
float PropTextColor(prop) PROP *prop; { return(prop->textcolor); }
int   PropLineWidth(prop) PROP *prop; { return(prop->linewidth); }
FONT *PropFont(prop) PROP *prop; { return(prop->pfont); }

PropWrite(prop, pfile)
PROP *prop;
FILE *pfile;
  {
	fprintf(pfile, "PROPS %d %f %f %f\n",
		prop->linewidth,
		prop->strokecolor,
		prop->fillcolor,
		prop->textcolor);
  }

PROP *PropRead(pfile)
FILE *pfile;
  {
	PROP *prop = PropNew();
	int ret = fscanf(pfile, "PROPS %d %f %f %f\n",
		&prop->linewidth,
		&prop->strokecolor,
		&prop->fillcolor,
		&prop->textcolor);
	prop->pfont			= FontDefault();
	if(ret != 4)
	  {
		fprintf("bad prop read...\n");
	  }
	return(prop);
  }

