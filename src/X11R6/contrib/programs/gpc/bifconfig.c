/* $XConsortium: bifconfig.c,v 5.2 94/04/17 20:44:22 rws Exp $ */
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
| Author        :	John M. Zulauf / SimGraphics Engineering Corp.
|
| File          :	bifconfig.c
| Date          :	Sat Jun 24 15:10:09 PDT 1989
| Project       :	PLB
| Description   :	System Configuration and Initialization Calls
| Status        :	Version 1.0
|
| Revisions     :	
|	2/89		MFR SimGEC: Added full configuration capabilities
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	int bif_defaultconfig()
|		:	set the BIF defaultconfig configuration value
|	int bif_colormodel(BIF_INT)
|		:	set the BIF color_model configuration value
|	int bif_colormode(BIF_INT)
|		:	set the BIF color_mode configuration value
|	int bif_buffermode(BIF_INT)
|		:	set the BIF buffer_mode configuration value
|
\*--------------------------------------------------------------------*/

/*---------------------------------------------------------------------*\
|	Include files 
\*--------------------------------------------------------------------- */
#include <stdio.h>
#include "biftypes.h"
#include "bifparse.h"
#include "bifmacro.h"
#include "globals.h"
#include "ph_map.h"
#include "brfexption.h"
#define EXCEPTION_HANDLER 1

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_defaultconfig()
|------------------------------------------------------------------------|
| Description	:	 set the BIF defaultconfig configuration value
|
|		This call is a configuration parameter setting routine.
|		The system dependant calls are made in bif_openwk();
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_defaultconfig()
{

#ifdef TEST_PRINT
	printf("DEFAULT CONFIGURATION : \n");
#endif /* TEST_PRINT */
	bif_colormodel(RGB);
	bif_colormode(TRUE_COLOR);
	bif_buffermode(DOUBLE_BUFFER);

} /* End procedure bif_defaultconfig */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_colormodel(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	set the BIF color_model configuration value
|		color_model is one of: < RGB | CIE | HSV | HLS >
|
|		This call is a configuration parameter setting routine.
|		The system dependant calls are made in bif_openwk();
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_colormodel(color_model)
BIF_INT color_model;
{

#ifdef TEST_PRINT
	printf("COLOR_MODEL : Set to %d\n",color_model);
#endif /* TEST_PRINT */

	wk_info.color_model = REMAP_CMODEL(color_model);
#ifdef USING_PHIGS
	/* this should work correctly, but on our box does not seem to
	work. */
/* PEX Note:
        This does work, the only problem exists when the first default
        configuration is defined, PHIGS is not open. The correct thing
        to do is test if the system is initialized before makeing calls
        to PHIGS functions.
#ifdef EXCEPTION_HANDLER
	PLB_EXCEPTION(BIF_EX_NOCOLORMODL);
#endif
*/

	if (wk_info.phigs_open) 
	    pset_colr_model((Pint)bench_setup.workid,
			    (Pint)wk_info.color_model );
#endif
} /* End procedure bif_colormodel */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_colormode(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	set the BIF color_mode configuration value
|		color_mode is one of: < PSEUDO_COLOR | TRUE_COLOR >
|
|		This call is a configuration parameter setting routine.
|		The system dependant calls are made in bif_openwk();
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_colormode(color_mode)
BIF_INT color_mode;
{

#ifdef TEST_PRINT
	printf("COLOR_MODE : Set to %d\n",color_mode);
#endif /* TEST_PRINT */
	wk_info.color_mode = color_mode;
} /* End procedure bif_colormode */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_buffermode(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	set the BIF buffer_mode configuration value
|		buffer_mode is one of: < SINGLE_BUFFER | DOUBLE_BUFFER >
|
|		This call is a configuration parameter setting routine.
|		The system dependant calls are made in bif_openwk();
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_buffermode(buffer_mode)
BIF_INT buffer_mode;
{

#ifdef TEST_PRINT
	printf("BUFFER_MODE : Set to %d\n",buffer_mode);
#endif /* TEST_PRINT */
	wk_info.buffer_mode = buffer_mode;
} /* End procedure bif_buffermode */

int bif_window(x, y)
BIF_INT x, y;
{
#ifdef TEST_PRINT
    printf("WINDOW_SIZE : Set to %d %d\n",x,y);
#endif /* TEST_PRINT */
    	wk_info.x = x;
    	wk_info.y = y;
}
