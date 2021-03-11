/* $XConsortium: bldgeneric.c,v 5.2 94/04/17 20:44:26 rws Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/
/***********************************************************
Copyright (c) 1989,1990, 1991 by Sun Microsystems, Inc.

						All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that Sun Microsystems
not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*--------------------------------------------------------------------*\
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	John M. Zulauf
|
| File          :	bldgeneric.c
| Date          :	Mon Jun 26 00:53:40 PDT 1989
| Project       :	PLB
| Description   :	The generic build routines. Used to simplify
|					bld_attr.c
| Status        :	Version 1.0
|
| Revisions     :
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	int bif_truecolor(float, float, float, int, int,
|				(*int)(), (*int)() )
|		:	Build and Store/Execute any true color entity.
|	int bif_colorindex(int, int, int (*int)(), (*int)() )
|		:	Receive any of the colorindex entities from 
|	int bif_2_index(int, int, int, (*int)(), (*int)(), (*int)())
|		:	Receive any of the index entities from a 
|	int bif_index(int, int, int, (*int)(), (*int)())
|		:	Receive any of the index entities from a 
|	int bif_size(float, int, int, (*int)())
|		:	Build, Store/Execute any float size definition
|
\*--------------------------------------------------------------------*/

/*---------------------------------------------------------------------*\
|	Include files 
\*--------------------------------------------------------------------- */
#include <stdio.h>
#include "biftypes.h"
#include "bifbuild.h"
#include "new_ents.h"
#include "bifparse.h"
#include "db_tools.h"
#include "doentity.h"
#include "bifmacro.h"
#include "globals.h"
#include "ph_map.h"
#include "macfunct.h"

/*--------------------------------------------------------------------*\
| Local global variables
\*--------------------------------------------------------------------*/
/* Useful statics */
/* Temporary entity storage */
static BIF_All temp_ent;

/*--------------------------------------------------------------------*\
|	Generic True color, index color and index anything routines
\*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*\
| Procedure	:	int bif_truecolor(float, float, float, int, int,
|				(*int)(), (*int)() )
|---------------------------------------------------------------------
| Description	:	Build and Store/Execute any true color entity.
|
|			NOTE: bif_truecolor is referenced by the
|			MF_TRUE_COLOR macro.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_truecolor(c1, c2, c3, entSize, entType, entHandler, phigsFunc)
float c1, c2, c3;
int entSize;
int entType;
int (*entHandler)();
void (*phigsFunc)();

{
    char *find_keyword_token();
    BIF_All *ent;
#ifdef USING_PHIGS
    Pgcolr gcolor;
#endif /* USING_PHIGS */
#ifdef TEST_PRINT
    printf("%s: Set to %f %f %f\n",find_keyword_token((BIF_INT)entType),
	   c1 , c2, c3);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	/*------------------------------------------------------------*\
	|	All true color entities use the same structure
	|	therefore we can use a common routine.
	\*------------------------------------------------------------*/
/* Allocate the entity */
    temp_ent.truecolor.color_model = wk_info.color_model;
    temp_ent.truecolor.color[0] = c1;
    temp_ent.truecolor.color[1] = c2;
    temp_ent.truecolor.color[2] = c3;

    ent = new_generic(&temp_ent,entSize,entType,entHandler);

/* Error check for ent == NULL ( FATAL ) */
    ENT_ERROR(ent);

#ifdef TEST_PRINT
    printf("push_level %d  \n", traverser_state->push_level);
#endif /* TEST_PRINT */
/* Build or Execute */
    Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entities in PHIGS */
    gcolor.type = ent->truecolor.color_model;
    gcolor.val.general.x = (Pfloat)ent->truecolor.color[0];
    gcolor.val.general.y = (Pfloat)ent->truecolor.color[1];
    gcolor.val.general.z = (Pfloat)ent->truecolor.color[2];
    (*phigsFunc)(&gcolor);
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
  Free_NRE(traverser_state, ent);

#endif /*  PRINT_ONLY */
} /* End procedure bif_truecolor */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_colorindex(int, int, int
|						(*int)(), (*int)() )
|---------------------------------------------------------------------
| Description	:	Receive any of the colorindex entities from 
|			MF_MAP_INDEX.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_colorindex(indx, entSize, entType, entHandler, phigsFunc)
int indx;
int entSize;
int entType;
int (*entHandler)();
void (*phigsFunc)();

{

	char *find_keyword_token();
	BIF_All *ent;
#ifdef USING_PHIGS
	Pgcolr gcolor;
#endif /* USING_PHIGS */
#ifdef TEST_PRINT
	printf("%s: Set to %d \n",find_keyword_token((BIF_INT)entType),
		indx);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Allocate the entity */
#ifdef EXTERNALNOTE
	/* indx is incremented by one so that color '0' does
	not step on the default BG color. */
#endif

	temp_ent.ind.ind = indx + 1;
	ent = new_generic(&temp_ent, entSize, entType, entHandler);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

#ifdef TEST_PRINT
	printf("push_level %d  \n", traverser_state->push_level);
#endif /* TEST_PRINT */
/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entities in PHIGS */
	gcolor.type = PINDIRECT;
	gcolor.val.ind = (Pint)temp_ent.ind.ind;
	(*phigsFunc)(&gcolor);
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /*  PRINT_ONLY */
} /* End procedure bif_colorindex */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_2_index(int, int, int, (*int)(), (*int)(),
|                                       (*int)())
|---------------------------------------------------------------------
| Description	:	Receive any of the index entities from a 
|			macro-function of parser receiver reference
|                       and output same value to two functions.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_2_index(indx, entSize, entType, entHandler, phigsFunc1, phigsFunc2 )
int indx;
int entSize;
int entType;
int (*entHandler)();
void (*phigsFunc1)(),(*phigsFunc2)();
{

	char *find_keyword_token();
	BIF_All *ent;
#ifdef TEST_PRINT
	printf("%s: Set to %d \n",find_keyword_token((BIF_INT)entType),
		indx);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Allocate the entity */
	temp_ent.ind.ind = indx;
	ent = new_generic(&temp_ent, entSize, entType, entHandler);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

#ifdef TEST_PRINT
	printf("push_level %d  \n", traverser_state->push_level);
#endif /* TEST_PRINT */
/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entity in PHIGS */
	(*phigsFunc1)((Pint)temp_ent.ind.ind);
	(*phigsFunc2)((Pint)temp_ent.ind.ind);
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /*  PRINT_ONLY */
} /* End procedure bif_2_index */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_index(int, int, int, (*int)(), (*int)())
|---------------------------------------------------------------------
| Description	:	Receive any of the index entities from a 
|			macro-function of parser receiver reference.
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_index(indx, entSize, entType, entHandler, phigsFunc)
int indx;
int entSize;
int entType;
int (*entHandler)();
void (*phigsFunc)();
{

	char *find_keyword_token();
	BIF_All *ent;
#ifdef TEST_PRINT
	printf("%s: Set to %d \n",find_keyword_token((BIF_INT)entType),
		indx);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Allocate the entity */
	temp_ent.ind.ind = indx;
	ent = new_generic(&temp_ent, entSize, entType, entHandler);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

#ifdef TEST_PRINT
	printf("push_level %d  \n", traverser_state->push_level);
#endif /* TEST_PRINT */
/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entity in PHIGS */
	(*phigsFunc)((Pint)temp_ent.ind.ind);
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /*  PRINT_ONLY */
} /* End procedure bif_index */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_size(float, int, int, (*int)())
|--------------------------------------------------------------------|
| Description	:	Build, Store/Execute any float size definition
|				entity (indirectly) from the parser.
|--------------------------------------------------------------------|
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_size(scaleFactor,entSize,entType,entHandler,phigsFunc)
float scaleFactor;
int entSize;
int entType;
int (*entHandler)();
void (*phigsFunc)();

{
	char *find_keyword_token();
	BIF_All *ent;
#ifdef TEST_PRINT
	printf("%s: Set to %f \n",find_keyword_token((BIF_INT)entType),
		scaleFactor);
	printf("MARKER_SIZE: Set to %f\n",scaleFactor);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Allocate the entity */
	temp_ent.size.size = scaleFactor;
	ent = new_generic(&temp_ent,entSize,entType,entHandler);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

#ifdef TEST_PRINT
	printf("push_level %d  \n", traverser_state->push_level);
#endif /* TEST_PRINT */
/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entity in PHIGS */
	(*phigsFunc)((Pfloat)temp_ent.size.size);
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /*  PRINT_ONLY */
} /* End bif_size() */

