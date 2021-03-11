/* $XConsortium: pixbuf.c,v 5.2 94/04/17 20:44:43 rws Exp $ */
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
Copyright(c) 1989,1990, 1991 by Sun Microsystems, Inc.

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
| File          :	pixbuf.c
| Date          :	Thu Feb  8 17:50:14 PST 1990
| Project       :	GPC
| Description   :	Manager the pixel input buffer
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	bif_initpixelbuffer(int, int, int, *unsigned char, *unsigned char, *unsigned char )
|		:	Initialize the pixmap input buffer
|	bif_fillpixelbuffer(BIF_INT)
|		:	Recieve pixel value from the parser, add to list
|	bif_endpixelbuffer(int, * Real_int_union)
|		:	Finish the pixmap, error test, and pseudo map.
|	keepPixel()
|		:	Keep the currently stored pixel.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include Files
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "biftypes.h"
#include "bifmacro.h"
#include "bifparse.h"
#include "globals.h"

/*--------------------------------------------------------------------*\
|	Local Defines
\*--------------------------------------------------------------------*/
#define RUN_LENGTH(buf,run,val)\
{\
	int MACi_index;\
	if ( buf != NULL )\
		for ( MACi_index = 0; MACi_index < run ; MACi_index++ )\
			*(buf++) = val;\
}

#define INIT_PIXEL_STORE(type,store)\
{\
	valType = type;\
	limStore = store;\
	if ( wk_info.color_mode == BIF_TRUE_COLOR )\
		limKeep  = KEEP_RGB;\
	else\
		limKeep  = KEEP_RED;\
}

#define STORE_RED	1
#define STORE_RGB	3

#define KEEP_RED	1
#define KEEP_RGB	3

#define PIXEL_VALUE	1
#define SEG_HEADER	2
#define BUFFER_FULL	3

static unsigned char	*redBase	= NULL;
static unsigned char	*greenBase	= NULL;
static unsigned char	*blueBase	= NULL;
static unsigned char	*buffer[3]	= { NULL, NULL , NULL };
static int		*colorIndex	= NULL;
static int		*colorIndexBase	= NULL;
static int		numRows;
static int		numColumns;
static int		mapType;

static int		mapSize;	/* Total number of pixels    */
static int		overCount;	/* Did we get too many?      */

static int		intStore[3];
static unsigned char	pixStore[3];
static int		numKept;	/* The number pixels stored  */
static int		valType;	/* Expect what next???       */
static int		runLength;	/* Pixel repeat count	     */
static int		endOfSeg;	/* Where does the seg end?   */
static int		limStore;	/* Store how many values?    */
static int		limKeep;	/* Of these keep which ones? */
static int		numStored;	/* The number of stored vals */

/*--------------------------------------------------------------------*\
| Procedure     :	bif_initpixelbuffer(int, int, int,
|				*unsigned char, *unsigned char,
|				*unsigned char )
|---------------------------------------------------------------------
| Description   :	Initialize the pixmap input buffer
|---------------------------------------------------------------------
| Return        :	Error Code: (NI:)
\*--------------------------------------------------------------------*/
bif_initpixelbuffer(inNumRows, inNumColumns, inMapType,
			inRed, inGreen, inBlue)
int		inNumRows;
int		inNumColumns;
int		inMapType;
unsigned char	*inRed;
unsigned char	*inGreen;
unsigned char	*inBlue;

{/* initPixBuf */
	int retCode;

	/*------------------------------------------------------------*\
	|	Save the size, type, and buffer locations
	\*------------------------------------------------------------*/
	numRows    = inNumRows;
	numColumns = inNumColumns;
	mapType    = inMapType;
	redBase    = inRed;
	greenBase  = inGreen;
	blueBase   = inBlue;

	/*------------------------------------------------------------*\
	|	We have to save the RED values as integers just in case
	|	the user decides to do this as a pseudo color thang...
	\*------------------------------------------------------------*/
	if ( colorIndexBase != NULL )
		free(colorIndexBase);
	mapSize	   = numRows * numColumns;
	colorIndexBase = (int *)malloc(sizeof(int)*mapSize);

	if ( colorIndexBase == NULL )
		ERROR("FATAL: Out of memory. (malloc failed)");

	/*------------------------------------------------------------*\
	|	Set up some Useful additional information
	\*------------------------------------------------------------*/
	buffer[0]  = redBase;
	buffer[1]  = greenBase;
	buffer[2]  = blueBase;
	colorIndex = colorIndexBase;

	numKept    = 0;
	endOfSeg   = mapSize + 1;
	numStored  = 0;
	runLength  = 1;
	overCount  = 0;

	switch ( mapType )
	{
	case PIXEL_VALUES:
		INIT_PIXEL_STORE(PIXEL_VALUE,STORE_RGB);
		break;
	case INTENSITY_VALUES:
		INIT_PIXEL_STORE(PIXEL_VALUE,STORE_RED);
		break;
	case PIXEL_VALUE_SEGMENTS:
		INIT_PIXEL_STORE(SEG_HEADER,STORE_RGB);
		break;
	case INTENSITY_VALUE_SEGMENTS:
		INIT_PIXEL_STORE(SEG_HEADER,STORE_RED);
		break;
	}

	retCode   = 0; /* We Be Okey Dokey */
	return ( retCode );

}/* initPixBuf */


/*--------------------------------------------------------------------*\
| Procedure     :	bif_fillpixelbuffer(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Recieve pixel value from the parser, add to list
|---------------------------------------------------------------------
| Return        :	Error Code (NI:)
\*--------------------------------------------------------------------*/
bif_fillpixelbuffer(pix)
BIF_INT pix;

{/* bif_fillpixelbuffer */
	switch (valType)
	{
	case PIXEL_VALUE:
		intStore[numStored]   = (int)pix;
		pixStore[numStored++] = (unsigned char)pix;
		if ( numStored >= limStore )
			storePixel();
		break;
	case SEG_HEADER:
		if ( pix > 0 )
		{/* Run length segment */
			runLength = pix;
			endOfSeg  = numKept + runLength;
		}/* Run length segment */
		else
		{/* Pixel Values Segment */
			runLength = 1;
			endOfSeg  = numKept + -(pix);
		}/* Pixel Values Segment */
		valType = PIXEL_VALUE;
		break;
	case BUFFER_FULL:
		overCount++;
		break;
	}
		
}/* bif_fillpixelbuffer */


/*--------------------------------------------------------------------*\
| Procedure     :	bif_endpixelbuffer(int, * Real_int_union)
|---------------------------------------------------------------------
| Description   :	Finish the pixmap, error test, and pseudo map.
|---------------------------------------------------------------------
| Return        :	Error Code (NI:)
\*--------------------------------------------------------------------*/
bif_endpixelbuffer(pseudoColorSize, pseudoColorMap)
int pseudoColorSize;
Real_int_union *pseudoColorMap;

{/* bif_endpixelbuffer */
	int i, j, maxIndex, indx;
	char buffy[255];

	/*------------------------------------------------------------*\
	|	Make sure the buffer is full
	\*------------------------------------------------------------*/
	if ( valType != BUFFER_FULL )
	{/* Oops, we didn't fill the buffer all the way */
		/* Report! */
		sprintf(buffy,
			"In PIXEL_MAP3: Map size error (%d vs. %d).\n",
			numKept, mapSize);
		yyerror(buffy);

		/*----------------------------------------------------*\
		|	Fill the rest of the map with BLACK (0)
		\*----------------------------------------------------*/
		intStore[0] = 0;
		for( i = 0; i < limStore; i++ )
			pixStore[i] = (unsigned char)0;
		runLength = mapSize - numKept;
		storePixel();

	}/* end Oops, we didn't fill the buffer all the way */
	else if ( overCount > 0 )
	{/* Too many pixels */
		/* Report! */
		sprintf(buffy,
			"In PIXEL_MAP3: Map size error (%d vs. %d).\n",
			mapSize+overCount, mapSize);
		yyerror(buffy);
	}/* end Too many pixels */


	/*------------------------------------------------------------*\
	|	If there is a pseudo color mapping (just) do it.
	|	The save color index value remaps the RGB values
	\*------------------------------------------------------------*/
	if ( pseudoColorSize > 0 )
	{/* if pseudo coloring should be done */
		buffer[0]  = redBase;
		buffer[1]  = greenBase;
		buffer[2]  = blueBase;
		colorIndex = colorIndexBase;
		maxIndex = pseudoColorSize-1;
		for ( i = 0;  i < mapSize; i++ )
		{/* for all pixels */
			/*--------------------------------------------*\
			|	Error Checking
			\*--------------------------------------------*/
			*colorIndex = MAX_VAL(*colorIndex,0);
			*colorIndex = MIN_VAL(*colorIndex,maxIndex);
		
			/*--------------------------------------------*\
			|	Remap the rgb based on the color index
			\*--------------------------------------------*/
			indx = 3 * (int)*colorIndex;
			for( j = 0; j < limKeep; j++ )
				*(buffer[j]++) = pseudoColorMap[indx+j].Int;
			
			/*--------------------------------------------*\
			|	Next index please...
			\*--------------------------------------------*/
			colorIndex++;
		}/* end for all pixels */

	}/* end if pseudo coloring should be done */


	/*------------------------------------------------------------*\
	|	Free the temp space.
	\*------------------------------------------------------------*/
	free((char *)colorIndexBase);
	colorIndexBase = NULL;

#define GX4000
#ifdef GX4000
	/*------------------------------------------------------------*\
	|	The GX4000 FORTRAN pixmap call can't handle 0's
	\*------------------------------------------------------------*/
	buffer[0]  = redBase;
	buffer[1]  = greenBase;
	buffer[2]  = blueBase;
	for ( i = 0;  i < mapSize; i++ )
	{
		for( j = 0; j < limKeep; j++ )
		{
			if ( *buffer[j] == 0 )
				*buffer[j] = 1;
			buffer[j]++;
		}
	}
#endif /* GX4000 */
}/* bif_endpixelbuffer */

/*--------------------------------------------------------------------*\
| Procedure     :	keepPixel()
|---------------------------------------------------------------------
| Description   :	Keep the currently stored pixel.
|---------------------------------------------------------------------
| Return        :	None.
\*--------------------------------------------------------------------*/
storePixel()

{/* storePixel */
	int i, repli;

	/*------------------------------------------------------------*\
	|	Make sure the pixStore is full (by replication)
	\*------------------------------------------------------------*/
	repli = numStored - 1;
	for( i = numStored; i < limKeep; i++ )
		pixStore[i] = pixStore[repli];

	/*------------------------------------------------------------*\
	|	Keep only the data we want too keep
	\*------------------------------------------------------------*/
	RUN_LENGTH(colorIndex,runLength,intStore[0]);
	for( i = 0; i < limKeep; i++ )
		RUN_LENGTH(buffer[i],runLength,pixStore[i]);

	numStored = 0;
	numKept  += runLength;

	if ( numKept  >= mapSize )
		valType = BUFFER_FULL;
	else if ( numKept  >= endOfSeg )
		valType = SEG_HEADER;

}/* storePixel */
