/* $XConsortium: brf_except.c,v 5.2 94/04/17 20:44:28 rws Exp $ */
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
| Author        :	Michael Rivero
|
| File          :	brf_except.c
| Date          :	Sat Feb 10 10:28:21 PST 1990
| Project       :	BIF Benchmark Reporting System
| Description   :	Exception Handler Routines
|			Reference port contains exception definitions
|			for Alliant GX4000 PHIGS+ v1.4.2
| Status        :	Version 1.0
|
| Revisions     :	
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	char BRF_define_exceptions()
|		:	Load the definitions for the known exceptions
|	int BRF_do_exceptionsearch(*BIF_All,*BRF_state)
|		:	Test for exceptions
|	int BRF_do_optionaltest( *BIF_All, int)
|		:	Test for exceptions in optional data groups
|	int BRF_realrange( *BIF_All, int)
|		:	Test if real values inside bif entity
|	int BRF_intrange( *BIF_All, int)
|		:	Test if int values inside bif entity
|	int BRF_truecolor( *BIF_All, int)
|		:	Test if is in true color mode during true color
|	int BRF_pseudocolor( *BIF_All, int)
|		:	Test if is in psuedo color mode during true color
|
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "biftypes.h"
#include "globals.h"
#include "bifparse.h"
#include "brfexption.h"
#include "brftypes.h"
#include "brfexmacro.h"

/* where we keep the exception table info. Needs to be
declared external in brf_trv.c */

BRF_exception brf_exception[MAX_EXCEPTION];

static char tmpstring[150];
int charcount;
int num_frames;
/*--------------------------------------------------------------------*\
| Procedure     :	char BRF_define_exceptions()
|---------------------------------------------------------------------
| Description   :	Load the definitions for the known exceptions
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_define_exceptions()

{/* BRF_define_exceptions */
register int i;
int listptr;
#ifdef EXTERNALNOTE
        /*  Any special exception handlers written must be declared
	in this routine in order to have a function pointer inside the
	exceptions table. */
#endif

extern int BRF_do_optiontest();
extern int BRF_realrange();
extern int BRF_intrange();
extern int BRF_truecolor();
extern int BRF_pseudocolor();

	listptr = 0;

        /*----------------------------------------------------------------*\
        |       Set the definitions for the exception handler
        \*----------------------------------------------------------------*/

#ifdef EXTERNALNOTE
	/* This is where all known exceptions are stated. The only
	portion of PLB not accessable to the exception tests at the top of the
	BRF traversal tree are those connected with configuration issues. It
	should (in theory) be possible to describe all possible exceptions
	here and have them test for at the top of the BRF traversal tree.
	Configuration exception code must be 'hard wired' at the point
	at which configuration is set (such as init_bench.c). */
#endif
	
	/* The format for an exception entry is as follows
	brf_ex_type		The entity type integer.
	brf_ex_flag		This flag is set to a degree of failure value.
				The degree codes are defined in brfexception.h.
				This value modifies the output of the exception.
	brf_ex_optional[7]	Markers for optional data groups,
				used to mark the individual exceptions.
	brf_ex_range[2]		Holds range values in integer form.
	brf_ex_rrange[2]	Holds range values in float form.
	brf_ex_custom_handler	Generic pointer to optional custom 
				exception tester code.
	brf_ex_message		The actual exception message text to be
				output if this exception occurs.
	*/


        /*----------------------------------------------------------------*\
        |       List 1: Non-Supported Features
        \*----------------------------------------------------------------*/
#ifdef EXTERNALNOTE
        /* These are the feature not supported in any PEX implementation
           all implementation dependent exceptions are in init_bench.c */

	/* THE DEFINES FOR THESE MACROS ARE IN BRF_EX_MACRO.H */
#endif

        LIST_NONSUP(PIXEL_MAP3,BRF_ERROR,"PIXEL_MAP3 Not Supported.\n");
	LIST_NONSUP(TEXT_FONT,BRF_ERROR,"TEXT_FONT: Only one font is supported.\n");

        /*----------------------------------------------------------------*\
        |       List 2: Optional Data Group Exceptions
        \*----------------------------------------------------------------*/


        /*----------------------------------------------------------------*\
        |       List 3: Range Exceptions
        \*----------------------------------------------------------------*/


        /*----------------------------------------------------------------*\
        |       List 4: True Color in Pseudo Mode
        \*----------------------------------------------------------------*/

	LIST_COLORMODE(LINE_COLOR,BRF_ERROR,BRF_truecolor,"True LINE_COLOR called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(INTERIOR_COLOR,BRF_ERROR,BRF_truecolor,"True INTERIOR_COLOR called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(BACKFACE_INTERIOR_COLOR,BRF_ERROR,BRF_truecolor,"True BACKFACE_INTERIOR_COLOR called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(EDGE_COLOR,BRF_ERROR,BRF_truecolor,"True EDGE_COLOR called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(TEXT_COLOR,BRF_ERROR,BRF_truecolor,"True TEXT_COLOR called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(BACKGROUND_COLOR,BRF_ERROR,BRF_truecolor,"True BACKGROUND_COLOR called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(VERTEX_COLORS,BRF_ERROR,BRF_truecolor,"True VERTEX_COLORS called while in Pseudo Color Mode.\n");
	LIST_COLORMODE(FACET_COLORS,BRF_ERROR,BRF_truecolor,"True FACET_COLORS called while in Pseudo Color Mode.\n");

	brf_num_ex_tests = listptr; /* save the number of messages */
	return(TRUE);

}/* BRF_define_exceptions */


/*--------------------------------------------------------------------*\
| Procedure     :	int BRF_do_exceptionsearch(*BIF_All,*BRF_state)
|---------------------------------------------------------------------
| Description   :	Test for exceptions
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_do_exceptionsearch(bif_entity,brf_state)
BIF_All *bif_entity;
BRF_state *brf_state;
{/* BRF_do_exceptionsearch */
register int i, j, k;
int (*handlerptr)();

	charcount = 0;
	num_frames = brf_state->brf_num_frames;

        /*----------------------------------------------------------------*\
        |       Apply the current entity against the known exceptions.
        \*----------------------------------------------------------------*/

#ifdef EXTERNALNOTE
	/* This routine is called at the top of the brf traversal tree.
	It takes as input the current bif_entity. The table
	of known exceptions is searched, and if necassary, seperate routines
	pointed to by custom_handler are executed. If an axception has
	occurred, increment the exception counter in the table.
	IMPORTANT NOTE. For all nested custom handlers, the BOTTOM MOST
	layer of the test is the one which should increment the exception
	counter. This keeps the exception output
	at the ends of the branches of the decision tree.
	The important points to remember are that the argument list to
	the special handlers must always be identical, so if a special
	handler requires more than the bif_entity pointer and the index
	to the current entry on the exception table, then ALL
	the handler routines will have to be changed, or you will
	get a segmentation fault. */

#endif

        /*----------------------------------------------------------------*\
        |       Loop through the entries in the exception table
        \*----------------------------------------------------------------*/

	for(i=0;i<brf_num_ex_tests;i++)
	{
        /*----------------------------------------------------------------*\
        |       Does the type of entity match the type of the current 
	|	table entry?
        \*----------------------------------------------------------------*/
		if(bif_entity->entity_type==brf_exception[i].brf_ex_type)
		{

        /*----------------------------------------------------------------*\
        |       Is there a custom exceptions handler?
        \*----------------------------------------------------------------*/
			if(brf_exception[i].brf_ex_custom_handler != NULL)
			{
				handlerptr = brf_exception[i].brf_ex_custom_handler;
				(*handlerptr)(bif_entity,i);

			}
			else
			{
			/* increment this exceptions counter */
				brf_exception[i].brf_ex_numhits += num_frames;
			}	/* end if custom handler */
        /*----------------------------------------------------------------*\
        |       Copy tmpstring to spooler.
        \*----------------------------------------------------------------*/
		} 	/* end if entity match */
	}	/* end for all table entries */

return(TRUE);

}/* BRF_do_exceptionsearch */



/*--------------------------------------------------------------------*\
| Procedure     :	int BRF_do_optionaltest( *BIF_All, int)
|---------------------------------------------------------------------
| Description   :	Test for exceptions in optional data groups
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_do_optiontest(bif_entity,i)
BIF_All *bif_entity;
int i;
{/* BRF_do_optiontest*/
register int j, k;
int type;
BIF_Withdata3 *bif_withdata3;

        /*----------------------------------------------------------------*\
        |       Any optional group flags set?
        \*----------------------------------------------------------------*/

	bif_withdata3 = (BIF_Withdata3 *)bif_entity;
        for(j=0;j<7;j++)        /* for all optional data groups */
        {
           if((bif_withdata3->with_data_flag[j] > 0 ) &&  /* if optional flag */
                        brf_exception[i].brf_ex_optional[j] == TRUE)
                {
		brf_exception[i].brf_ex_numhits += num_frames;
	        } /* end if optional flag */
        } /* end for all optional data groups */
return(TRUE);
}/* BRF_do_optiontest*/



/*--------------------------------------------------------------------*\
| Procedure     :	int BRF_realrange( *BIF_All, int)
|---------------------------------------------------------------------
| Description   :	Test if real values inside bif entity
|			exceed range of values in brf_exception[i].
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_realrange(bif_entity,i)
BIF_All *bif_entity;
int i;
{/* BRF_realrange */
register int j, k;
int type;
BIF_Size *bif_size;
int logic;

        /*----------------------------------------------------------------*\
        |       Is the test value in size within exception range
        \*----------------------------------------------------------------*/

        bif_size = (BIF_Size*)bif_entity;
	logic = brf_exception[i].brf_ex_logic;

	switch(logic)
	{
	case TRUE:
	if(brf_exception[i].brf_ex_rrange[RANGESTART] <= bif_size->size &&
	   brf_exception[i].brf_ex_rrange[RANGESTOP] >= bif_size->size)
	{
		return(TRUE);
	}
	else
	{
		brf_exception[i].brf_ex_numhits += num_frames;
        } /* end if in range */
	break;
        case FALSE:
        if(brf_exception[i].brf_ex_rrange[RANGESTART] <= bif_size->size &&
           brf_exception[i].brf_ex_rrange[RANGESTOP] >= bif_size->size)
        {
                brf_exception[i].brf_ex_numhits += num_frames;
        }
        else
        {
                return(TRUE);
        } /* end if in range */
        break;
	} /* end switch */

return(TRUE);
}/* BRF_realrange */




/*--------------------------------------------------------------------*\
| Procedure     :	int BRF_intrange( *BIF_All, int)
|---------------------------------------------------------------------
| Description   :	Test if int values inside bif entity
|                       exceed range of values in brf_exception[i].
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_intrange(bif_entity,i)
BIF_All *bif_entity;
int i;
{/* BRF_intrange */
register int j, k;
int type;
BIF_Index *bif_index;
int logic;

        /*----------------------------------------------------------------*\
        |       Is the test value in index within exception range
        \*----------------------------------------------------------------*/

        bif_index = (BIF_Index*)bif_entity;
	logic = brf_exception[i].brf_ex_logic;
#ifdef EXTERNALNOTE
	/* here is an example of the logic field of the brf_exception
	structure in use. When set to TRUE, the range function performs
	as expected, issuing an exception if the value passed in from the
	bif_entity structure is outside the range brf_ex_rrange[RANGESTART] - 
	brf_ex_rrange[RANGESTOP]. if logic is set FALSE, the function inverts,
	issuing the exception if the testvalue lies INSIDE the range. Thus,
	it can be used as an exclusion test. */
#endif

        switch(logic)
        {
        case TRUE:

        if(brf_exception[i].brf_ex_range[RANGESTART] <= bif_index->ind &&
           brf_exception[i].brf_ex_range[RANGESTOP] >= bif_index->ind)
        {
                return(TRUE);
        }
        else
        {
                brf_exception[i].brf_ex_numhits += num_frames;
        } /* end if in range */
	break;
        case FALSE:

        if(brf_exception[i].brf_ex_range[RANGESTART] <= bif_index->ind &&
           brf_exception[i].brf_ex_range[RANGESTOP] >= bif_index->ind)
        {
                brf_exception[i].brf_ex_numhits += num_frames;
        }
        else
        {
                return(TRUE);
        } /* end if in range */
        break;
        } /* end switch */


return(TRUE);
}/* BRF_intrange */



/*--------------------------------------------------------------------*\
| Procedure     :	int BRF_truecolor( *BIF_All, int)
|---------------------------------------------------------------------
| Description   :	Test if is in true color mode during true color
|			calls
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_truecolor(bif_entity,i)
BIF_All *bif_entity;
int i;
{/* BRF_truecolor */
register int j, k;
int type;
BIF_Index *bif_index;
int logic;

        /*----------------------------------------------------------------*\
        |       Is in true color mode during true color calls?
        \*----------------------------------------------------------------*/

        bif_index = (BIF_Index*)bif_entity;
        logic = brf_exception[i].brf_ex_logic;

        if(wk_info.color_mode != TRUE_COLOR)
        {
                brf_exception[i].brf_ex_numhits += num_frames;
        }
        else
        {
                return(TRUE);
        } 

return(TRUE);
}/* BRF_truecolor */



/*--------------------------------------------------------------------*\
| Procedure     :	int BRF_pseudocolor( *BIF_All, int)
|---------------------------------------------------------------------
| Description   :	Test if is in psuedo color mode during true color
|                       calls
|
|---------------------------------------------------------------------
| Revisions     :
|
|---------------------------------------------------------------------
| Return        :	
\*--------------------------------------------------------------------*/
int BRF_pseudocolor(bif_entity,i)
BIF_All *bif_entity;
int i;
{/* BRF_pseudocolor */
register int j, k;
int type;
BIF_Index *bif_index;
int logic;

        /*----------------------------------------------------------------*\
        |       Is in pseudo color mode during true color calls?
        \*----------------------------------------------------------------*/

        bif_index = (BIF_Index*)bif_entity;
        logic = brf_exception[i].brf_ex_logic;

        if(wk_info.color_mode == TRUE_COLOR)
        {
                brf_exception[i].brf_ex_numhits += num_frames;
        }
        else
        {
                return(TRUE);
        }

return(TRUE);
}/* BRF_pseudocolor */





