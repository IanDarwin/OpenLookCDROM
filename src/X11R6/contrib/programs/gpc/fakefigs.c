/* $XConsortium: fakefigs.c,v 5.3 94/04/17 20:44:35 rws Exp $ */
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
|
| File          :	fakefigs.c
| Date          :	Sun Jun 25 15:50:46 PDT 1989
| Project       :	PLB
| Description   :	Routines that emulate missing phigs+ calls.
|					Specifically, trimesh, quadmesh
|					--- And MUCH, MUCH More!!!!!!
| Status        :	Version 1.0
|
|
|	11/21/89	Paul Chek DEC:
| 	                  - in fxrect change call to pfa to ppl 
|	
|	2/89		JMZ SimGEC: added pixmap functions
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	int fxtrd3( int *, int *, int *, float *, int *, float *, float *, float *, float *)
|		:	Generate a polyhedron call for the given fxtrd3
|	int fxqmd3( int *, int *, int *, float *, int *, int *, float *, float *, float *, float *)
|		:	Generate a polyhedron call for the given fxqmd3
|	int fxsattxal(int *, int *)
|		:	A PHIGs like call that calls both psatal and pstxal
|	int fxsattxp(int *, int *)
|		:	A PHIGs like call that calls both psatp and pstxp
|	int fxopns()
|		:	Open a resevered structure to recieve the
|	int fxclns()
|		:	Closes the resevered structure that recieves the
|	int fxsbfio(cmodel, c1, c2, c3)
|		:	noop for BACKFACE_COLOR
|	int fxsbfci(index)
|		:	noop for BACKFACE_COLOR_INDEX
|	int fxbfpr(&identify_flag, &cull_flag)
|		:	Noop for back face processing
|	fxbfp(&ambient,&diffuse,&specular,&color_model, specular_color, &highlight, &transparency);
|		:	Noop for back face properties
|	int fxsipi(index)
|		:	noop for PATTERN_INDEX
|	int fxnoop(size)
|		:	Insert a PHIGS element that does nothing...
|	int fxrect(xll, yll, xsize, ysize )
|		:	Convert a FIGARO pxrect call into a pfa call
|	int fxsbci(wk,index)
|		:	Set the background to the color value at the
|	int fxrfst(str_id)
|		:	CALL a structure without doing a save/restore 
|	int fxpaus()
|		:	pause the workstation
|	fxpixelup(int)
|		:	Set the pixel update function to the given value
|	fxpixmap3( vector3, int, int, int, *unsigned char, int, *char)
|		:	Generate a one channel pixelmap PHIGS+ element
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include Files
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include <X11/Xosdefs.h>
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc();
#endif /* macII */
#ifdef HAVE_PIXELS
#include "fig_enums.h"
#endif
#include "ph_map.h"
#include "biftypes.h"
#include "globals.h"
#include "bifmacro.h"


/*--------------------------------------------------------------------*\
| Procedure     :	int fxsattxal(Ptxalign *)
|---------------------------------------------------------------------
| Description   :	A PHIGs like call that calls both psatal and pstxal
|			NOTE: fxsattxal generates two PHIGs elements.
|---------------------------------------------------------------------
| Return        :	The return of the last PHIGs calls.
\*--------------------------------------------------------------------*/
int fxsattxal(align)
Ptext_align *align;

{
	/* NOTE: fxsattxal generates two PHIGs elements */
#ifdef USING_PHIGS
	pset_anno_align((Ptext_align *)align);
	pset_text_align((Ptext_align *)align);
#endif /* USING_PHIGS */
} /* End fxsattxal() */


/*--------------------------------------------------------------------*\
| Procedure     :	void fxsattxp(int )
|---------------------------------------------------------------------
| Description   :	A PHIGs like call that calls both psatp and pstxp
|			NOTE: fxsattxp generates two PHIGs elements.
\*--------------------------------------------------------------------*/
void fxsattxp(path)
int path;

{
	/* NOTE: fxsattxp generates two PHIGs elements */
#ifdef USING_PHIGS
	pset_anno_path((Ptext_path)path);
	pset_text_path((Ptext_path)path);
#endif /* USING_PHIGS */
} /* End fxsattxp() */


/*--------------------------------------------------------------------*\
| Procedure     :	int fxopns()
|---------------------------------------------------------------------
| Description   :	Open a resevered structure to recieve the
|			"immediate" mode BIF entities.
|---------------------------------------------------------------------
| Return        :	Error Code
\*--------------------------------------------------------------------*/
int fxopns()

{
#ifdef USING_PHIGS
	popen_struct((Pint)bench_setup.nrs_stid);
#endif /* USING_PHIGS */
} /* fxopns() */

/*--------------------------------------------------------------------*\
| Procedure     :	int fxclns()
|---------------------------------------------------------------------
| Description   :	Closes the resevered structure that recieves the
|			"immediate" mode BIF entities.
|---------------------------------------------------------------------
| Return        :	Error Code
\*--------------------------------------------------------------------*/
int fxclns()

{
#ifdef USING_PHIGS
	pclose_struct();
#endif /* USING_PHIGS */
} /* End fxclns() */

/*--------------------------------------------------------------------*\
| Procedure     :	void fxsbfci(index)
|---------------------------------------------------------------------
| Description   :	noop for BACKFACE_COLOR_INDEX
\*--------------------------------------------------------------------*/
void fxsbfci(index)
int index;

{
} /* End fpsbfci() */

/*--------------------------------------------------------------------*\
| Procedure     :	int fxsipi(index)
|---------------------------------------------------------------------
| Description   :	noop for PATTERN_INDEX
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int fxsipi(index)
int index;

{
	return(0);
} /* End fpsipi() */


/*--------------------------------------------------------------------*\
| Procedure     :	int fxnoop(size)
|---------------------------------------------------------------------
| Description   :	Insert a PHIGS element that does nothing...
|		EXCEPTION: NOOP elements Don't exist on the GX4000
|---------------------------------------------------------------------
| Return        :	Error Code ( Not implemented )
\*--------------------------------------------------------------------*/
/*ARGSUSED*/
fxnoop(size)
int size;
{
} /* End fxnoop();

/*--------------------------------------------------------------------*\
| Procedure     :	int fxrect(xll, yll, xsize, ysize )
|---------------------------------------------------------------------
| Description   :	Convert a FIGARO pxrect call into a pfa call
|----------------------------------------------------------------------
| Return        :	Error Code ( Not implemented )
\*--------------------------------------------------------------------*/
int fxrect(xll, yll, xsize, ysize )
float xll, yll, xsize, ysize;

{
	int numCoords;
	Ppoint points[4];

	numCoords = 4;
	points[0].x = (Pfloat)xll;
	points[1].x = (Pfloat)(xll + xsize);
	points[2].x = (Pfloat)(xll + xsize);
	points[3].x = (Pfloat)xll;

	points[0].y = (Pfloat)yll;
	points[1].y = (Pfloat)yll;
	points[2].y = (Pfloat)(yll + ysize);
	points[3].y = (Pfloat)(yll + ysize);

#ifdef USING_PHIGS
	{
	    Ppoint_list fpoints;

	    fpoints.num_points = (Pint)numCoords;
	    fpoints.points = (Ppoint *)points;
	    pfill_area(&fpoints);
	}
#endif

} /* End fxrect();

/*--------------------------------------------------------------------*\
| Procedure     :	int fxsbci(wk,index)
|---------------------------------------------------------------------
| Description   :	Set the background to the color value at the
|			given index.
|---------------------------------------------------------------------
| Return        :	Error Code ( Not implemented )
\*--------------------------------------------------------------------*/
/*ARGSUSED*/
int fxsbci(wk,index)
int *wk, *index;
{
	/*------------------------------------------------------------*\
	|	WORKING: Need to do an inquire color rep and then
	|	set color rep zero.
	\*------------------------------------------------------------*/
} /* End fxsbci();

/*--------------------------------------------------------------------*\
| Procedure     :	int fxrfst(str_id)
|---------------------------------------------------------------------
| Description   :	CALL a structure without doing a save/restore 
|			of the traverser state (obsolete)
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int fxrfst(str_id)
int str_id;

{
	/*------------------------------------------------------------*\
	|	PHIGs can't do this, so we'll approximate using
	|	pexst.... but there is a better way.... 
	\*------------------------------------------------------------*/
#ifdef TEST_PRINT
fflush(stderr);
printf("fxrfst: executing %d\n",str_id);
fflush(stdout);
#endif /* TEST_PRINT */

#ifdef USING_PHIGS
	pexec_struct((Pint)str_id);
#endif
}

/*--------------------------------------------------------------------*\
| Procedure     :	int fxpaus()
|---------------------------------------------------------------------
| Description   :	pause the workstation
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
/*ARGSUSED*/
int fxpaus(wk)
int *wk;

{
	/* WORKING: There should be a better form of pause avail. */
	char buffy[255];
	fflush(stdout);
	fprintf(stderr,"Paused -- Press <Return> to continue.");
	fflush(stderr);
	gets(buffy);
} /* End fxpaus() */


/*--------------------------------------------------------------------*\
| Procedure     :	fxpixelup(int)
|---------------------------------------------------------------------
| Description   :	Set the pixel update function to the given value
|---------------------------------------------------------------------
| Return        :	None:
\*--------------------------------------------------------------------*/
fxpixelup(pixelUpdate)
int pixelUpdate;

{/* fxpixelup */
	/* Pack data record parameters */
	int	il;
	int	ia[3];
	int	rl;
	float	ra[3];
	int	sl;
	int	lstr[1];  
	char	str[80];
	int	ldr;
	int	ldrPixUp;
	char	toPixUp[80];
	int	mldr;
	int	ierr;

	/*----------------------------------------------------*\
	|	Pixel Update Function Data 
	\*----------------------------------------------------*/
	il      = 2;
	ia[0]   = pixelUpdate;
	ia[1]   = BIF_OFF;  /* 0 */
	rl      = 0;
	ra[0]   = 0.;
	sl      = 0;
	lstr[0] = 0; 
	mldr    = 1;

	/* Build the data record to set the pixel function*/
#ifdef USING_PHIGS
#ifdef HAVEPIXELS
	pprec(	&il, ia, &rl, ra,
		&sl, lstr, str, &mldr, &ierr,
		&ldrPixUp, toPixUp, 80, 80);

#ifdef PPRECCHECK
	switch(ierr)
	{
	case 2001:
		ERROR("fxpixelup: insufficient memory for pprec.");
		break;
	case 2003:
		ERROR("fxpixelup: error encoding data record during pprec.");
		break;
	case 0:
		break; /* no reported error */
	default:
		ERROR("fxpixelup: unknown phigs error in pprec.");
		exit(-1);
		break;
	} /* end switch */
#endif

	pgse(FIG_PIXFUNC,&ldrPixUp,toPixUp,80);
#endif /* HAVEPIXELS */
#endif

	return;
}/* fxpixelup */

#ifdef EXTERNALNOTE
Alliant Users!
There is a problem with passing defined strings of characters from C to
Fortran in that pprec uses the C strlen. strlen will assume a char 0 
(NULL) to be the end of the string and terminate the data transfer. This
means that the length of the data read in doees not match the length
specified in br_mldr, and the pack operation is aborted. This problem is
dealt with in version 2.0 and later. 
#endif
/*--------------------------------------------------------------------*\
| Procedure     :	fxpixmap3( vector3, int, int, int,
|					*unsigned char, int, *char)
|---------------------------------------------------------------------
| Description   :	Generate a one channel pixelmap PHIGS+ element
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
fxpixmap3(upperLeft, numCol, numRow, channelID, pixelData,
		wkSize,workSpace)
vector3	upperLeft;
int	numCol;
int	numRow;
int	channelID;
unsigned char *pixelData;
int	wkSize;		/* Size of workSpace in 80 byte records*/
char	*workSpace;

{/* fxpixmap3 */
	/* Pack data record parameters */
	int	il;
	int	ia[3];
	int	rl;
	float	ra[1];
	int	sl;
	int	lstr[1];  
	int	ldr;
	int	ldrPixUp;
	char	*str;
	int	mldr;
	int	ierr;

	/*------------------------------------------------------------*\
	|	Fill the  Data to pack the pixel map
	\*------------------------------------------------------------*/
	il      = 3;
	ia[0]   = channelID;
	ia[1]   = numCol;
	ia[2]   = numRow;
	ldr     = 0;
	rl      = 0;
	ra[0]   = 0.;
	sl      = 1;
	lstr[0] = numCol * numRow; 
	str     = (char *)pixelData;
	mldr    = wkSize;

	/*------------------------------------------------------------*\
	|	Pack the pixmap data for the PHIGS+ call
	\*------------------------------------------------------------*/
#ifdef USING_PHIGS
#ifdef HAVEPIXELS
	pprec(	&il, ia, &rl, ra,
		&sl, lstr, str, &mldr, &ierr,
		&ldr, workSpace, 80, 80);

#ifdef PPRECCHECK
	switch(ierr)
	{
	case 2001:
		ERROR("fxpixmap3: insufficient memory for pprec.");
		break;
	case 2003:
		ERROR("fxpixmap3: error encoding data record during pprec.");
		break;
	case 0:
		break; /* no reported error */
	default:
		ERROR("fxpixmap3: unknown phigs error in pprec.");
		exit(-1);
		break;
	} /* end switch */
#endif

	/*------------------------------------------------------------*\
	|	Load the pixmap into the open PHIGS+ structure
	\*------------------------------------------------------------*/
	ierr = pgdp3(Fi1, Fpass3f(upperLeft),FIG_WRITEIMAGE,
		     &ldr, workSpace, 80);
#endif /* HAVEPIXELS */
#endif

}/* fxpixmap3 */
