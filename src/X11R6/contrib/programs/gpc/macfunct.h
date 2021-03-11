/* $XConsortium: macfunct.h,v 5.3 94/04/17 20:44:39 rws Exp $ */
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
Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.

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
|  Copyright (C) 1989, 1990, 1991, National Computer Graphics Association
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
| File          :	macfunct.h
| Date          :	Sun Jun 25 22:41:36 PDT 1989
| Project       :	PLB
| Description   :	Macro-Functions... macros the generate
|			functions.  For those who don't like to
|			type, or yank-put (ad infinitum).
| Status        :	Version 1.0
|
| Revisions     :
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	define MF_TRUE_COLOR(MV_routine,MV_token,MV_handle,MV_phigs)
|	define MF_MAP_INDEX(MV_routine,MV_token,MV_handle,MV_phigs)
|	define MF_BIF_SIZE(MV_routine,MV_token,MV_handle,MV_phigs)
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Use a Macro to define a whole class of function:
|
|	the bif_<prim>color functions.
\*--------------------------------------------------------------------*/
#define MF_TRUE_COLOR(MV_routine,MV_token,MV_handle,MV_phigs) \
	/*----------------------------------------------------------*\
	| Procedure: int MV_routine(c1, c2, c3)			     \
	|----------------------------------------------------------- \
	| Description: Receive a MV_TOKEN entity from the parser     \
	|----------------------------------------------------------- \
	| Return: Error Code (Not Implemented)			     \
	\*---------------------------------------------------------*/\
	int MV_routine(c1, c2,c3)				     \
	BIF_REAL c1, c2,c3;					     \
	{							     \
		static int entSize        = sizeof(BIF_True_color)   \
		                            + sizeof(Pgcolr);        \
								     \
		/*--------------------------------------------------*\
		|	Since true color entities have the same	     \
		|	structure we can use a common routine.	     \
		\*-------------------------------------------------*/\
		bif_truecolor((float)c1,(float)c2,(float)c3,	     \
			entSize, (int)MV_token, MV_handle, MV_phigs);\
								     \
	} /* End MV_routine() */


/*--------------------------------------------------------------------*\
|	Use a Macro to define a whole class of function:
|
|	the bif_<prim>colorindex functions.
\*--------------------------------------------------------------------*/

#define MF_MAP_INDEX(MV_routine,MV_token,MV_handle,MV_phigs)	     \
	/*----------------------------------------------------------*\
	| Procedure: int MV_routine(index)			     \
	|----------------------------------------------------------- \
	| Description: Receive a MV_token entity from the parser     \
	|----------------------------------------------------------- \
	| Return: Error Code (Not Implemented)			     \
	\*---------------------------------------------------------*/\
	int MV_routine(index)					     \
	BIF_INT index;						     \
	{							     \
		static int entSize        = sizeof(BIF_Index);   \
								     \
		/*--------------------------------------------------*\
		|	Since true color entities have the same	     \
		|	structure we can use a common routine.	     \
		\*-------------------------------------------------*/\
		bif_colorindex((int)index, entSize,		     \
			(int)MV_token, MV_handle, MV_phigs);	     \
								     \
	} /* End MV_routine() */


#define MF_BIF_SIZE(MV_routine,MV_token,MV_handle,MV_phigs) \
	/*----------------------------------------------------------*\
	| Procedure: int MV_routine(size)			     \
	|----------------------------------------------------------- \
	| Description: Receive a MV_token entity from the parser     \
	| 		Many _size and _scalefactor routines	     \
	|		use this as a shorthand form.		     \
	|----------------------------------------------------------- \
	| Return: Error Code (Not Implemented)			     \
	\*---------------------------------------------------------*/\
	int MV_routine(size)					     \
	BIF_REAL size;						     \
	{							     \
		static int entSize = sizeof(BIF_Size);		     \
		bif_size((float)size,entSize,MV_token,		     \
			MV_handle,MV_phigs);			     \
								     \
	} /* End procedure MV_routine */

