/* $XConsortium: bld_str.c,v 5.2 94/04/17 20:44:25 rws Exp $ */
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
supporting documentation, and Sun Microsystems
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
| File          :	bif_str.c
| Date          :	Tue Jun 20 11:10:12 PDT 1989
| Project       :	PLB
| Description   :	Functions that build structure entities
| Status        :	Version 1.0
|
| Revisions     :
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
|	int bif_begstr(BIF_INT)
|		:	Receive a BEGIN_STRUCTURE entity from the
|	int bif_endstr()
|		:	Receive an END_STRUCTURE entity from the parser
|	int bif_label(BIF_INT)
|		:	In the current release of PLB, labels are not
|	int bif_execstr(BIF_INT)
|		:	Receive an EXECUTE_STRUCTURE entity from the parser
|	int bif_callstr(BIF_INT)
|		:	Receive a CALL_STRUCTURE entity from the parser
|	int bif_invokeatframe(BIF_INT, BIF_INT, BIF_INT, BIF_INT)
|		:	Receive an invoke at frame entity from the 
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files 
\*--------------------------------------------------------------------*/
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

/*--------------------------------------------------------------------*\
|Local #define
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|Local Globals
\*--------------------------------------------------------------------*/
BIF_All temp_ent;


/*--------------------------------------------------------------------*\
|	Structure Entities
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_begstr(BIF_INT)
|---------------------------------------------------------------------
| Description	:	Receive a BEGIN_STRUCTURE entity from the
|			parser.
|
|			NOTE: This function is an exception in that it
|			does NOT call the traverser but executes the
|			action function directly.  ( Which is required
|			unless you set the build-traverser to recogize
|			this entity type or some other trailer )
|---------------------------------------------------------------------
| Return	:	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_begstr(structure_id)
BIF_INT structure_id;
{
	BIF_Beginstructure *ent;
#ifdef TEST_PRINT
	printf("Begin structure %d\n",structure_id);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Get the pointer to the structure ( allocated if new ) */
	ent = db_get_str(structure_id);
	
/*  Check if we ran out of memory */
	ENT_ERROR(ent);

/* Open the Structure in the BIF Database and in PHIGS */
	do_beginstructure(traverser_state, ent);

#ifdef USING_PHIGS
/* Close the Non-Retained Structure */
	fxclns();
	traverser_state->nrs_state = 0;

/* Open the PHIGS Structure to build */
	popen_struct((Pint)structure_id);
#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_begstr */

/*--------------------------------------------------------------------*\
| Procedure	:	int bif_endstr()
|---------------------------------------------------------------------
| Description	:	Receive an END_STRUCTURE entity from the parser
|
|			NOTE: This function is an exception in that it
|			does NOT call the traverser but executes the
|			action function directly.  ( Which is required
|			unless you set the build-traverser to recogize
|			this entity type or some other trailer )
|---------------------------------------------------------------------
| Return	:	Error Code
\*--------------------------------------------------------------------*/
int bif_endstr()
{
#ifdef TEST_PRINT
	printf("Ending current structure\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Close the Structure in the BIF Database and in PHIGS */
	do_endstructure(traverser_state,NULL);

#ifdef USING_PHIGS
/* Close the PHIGS Retained structure */
	pclose_struct();

/* Open the Non-Reatained Structure */
	fxopns();
	traverser_state->nrs_state = 1;

#endif /* USING_PHIGS */
#endif /* PRINT_ONLY */
} /* End procedure bif_endstr */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_label(BIF_INT)
|---------------------------------------------------------------------
| Description   :	In the current release of PLB, labels are not
|			used.  This is a dummy routine to catch the 
|			parser call.
|---------------------------------------------------------------------
| Return        :	Error Code. (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_label(label_id)
BIF_INT label_id;

{

} /* End bif_label */


/*--------------------------------------------------------------------*\
|	* ************************************** *
|		Structure Hierarchy
|	* ************************************** *
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	* ************************************** *
|		Invoking other structures
|	* ************************************** *
\*--------------------------------------------------------------------*/

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_execstr(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive an EXECUTE_STRUCTURE entity from the parser
|
|	structure_id	ID of structure to execute
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_execstr(structure_id)
BIF_INT structure_id;

{
	static int ent_size = sizeof(BIF_Executestructure);
	BIF_Beginstructure *str;
	BIF_All *ent;

#ifdef TEST_PRINT
	printf("EXECUTE_STRUCTURE : execute structure %d \n",structure_id);
	fflush(stdout);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Allocate the entity */
/* Get the address of the specified structure */
	str = db_get_str(structure_id);
/*  Check if we ran out of memory */
	ENT_ERROR(str);


/* Allocate the entity */
	temp_ent.executestructure.structure_id  = structure_id;
	temp_ent.executestructure.structure_ptr = str;

	ent = new_generic(&temp_ent, ent_size, 
			EXECUTE_STRUCTURE, do_executestructure);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entity in PHIGS */
	pexec_struct((Pint)temp_ent.executestructure.structure_id);
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /* PRINT_ONLY */
} /* End procedure bif_execstr */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_callstr(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Receive a CALL_STRUCTURE entity from the parser
|
|	structure_id	ID of structure to call
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_callstr(structure_id)
BIF_INT structure_id;
{
	static int ent_size = sizeof(BIF_Callstructure);
	BIF_All *ent;
	BIF_Beginstructure *str;
	
#ifdef TEST_PRINT
	printf("CALL_STRUCTURE : call structure %d \n",structure_id);
#endif /* TEST_PRINT */

#ifndef PRINT_ONLY
/* Allocate the entity */
/* Get the address of the specified structure */
	str = db_get_str(structure_id);
/*  Check if we ran out of memory */
	ENT_ERROR(str);


/* Allocate the entity */
	temp_ent.callstructure.structure_id  = structure_id;
	temp_ent.callstructure.structure_ptr = str;
	temp_ent.callstructure.startLabel  = getNextLabel();
	temp_ent.callstructure.endLabel    = getNextLabel();

	ent = new_generic(&temp_ent, ent_size, 
			CALL_STRUCTURE, do_callstructure);

/* Error check for ent == NULL ( FATAL ) */
	ENT_ERROR(ent);

/* Build or Execute */
	Traverse(traverser_state, ent);

#ifdef USING_PHIGS
/* Call the entity in PHIGS */
#ifdef REFER_STRUCTURE_EXISTS
	fxrfst(temp_ent.callstructure.structure_id);
#else /* REFER_STRUCTURE_IS_EXISTS */
	if (traverser_state->nrs_state == 0)
	{
		/*----------------------------------------------------*\
		|	Insert two reference labels for later expansion
		|	and editing.
		\*----------------------------------------------------*/
#ifdef TEST_PRINT
printf("startLabel %d\n", ent->callstructure.startLabel);
printf("endLabel %d\n", ent->callstructure.endLabel);
#endif /* TEST_PRINT */
		plabel((Pint)ent->callstructure.startLabel);
		plabel((Pint)ent->callstructure.endLabel);
	}
	else
		/* Instant expansion into the NRS */
		pcopy_all_elems_struct((Pint)ent->callstructure.structure_id);
#endif /* REFER_STRUCTURE_EXISTS */
#endif /* USING_PHIGS */

/* Release Non-Retained Entities */
	Free_NRE(traverser_state, ent);

#endif /* PRINT_ONLY */
} /* End procedure bif_callstr */


/* Verb File */

/*--------------------------------------------------------------------*\
| Procedure     :	int bif_invokeatframe(BIF_INT, BIF_INT,
|				BIF_INT, BIF_INT)
|---------------------------------------------------------------------
| Description   :	Receive an invoke at frame entity from the 
|			parser, build and store it in the test-loop
|			structure.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int bif_invokeatframe(str_id, invoke_style, start_frame, end_frame)
BIF_INT str_id;
BIF_INT invoke_style;
BIF_INT start_frame;
BIF_INT end_frame;

{
	static int ent_size = sizeof(BIF_InvokeAtFrame);
	BIF_All *ent;
	BIF_InvokeAtFrame *atFrame;
	BIF_Executestructure *invoke_me;
	char *find_keyword_token();

#ifdef TEST_PRINT
	printf("INVOKE_AT_FRAME : %s Structure %d from frame %d to %d\n",
		find_keyword_token(invoke_style),str_id,
		start_frame,end_frame);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	/*------------------------------------------------------------*\
	|	This entity must be encountered while in Build mode.
	|	Specifically the parser ONLY allow this entity while
	|	the test-loop structure is being built
	|
	|	NOTE: With little additional effort/logic the invoke at
	|	frame entity could be allowed to be store within
	|	structures.( But the BIF spec currently does not
	|	require/support/allow this.
	\*------------------------------------------------------------*/
	if ( traverser_state->open_structure != NULL )
	{
		/*----------------------------------------------------*\
		|	Fill the temp_ent then allocate the entity
		\*----------------------------------------------------*/
		atFrame = &temp_ent.invokeatframe;

		/* The basic facts about the invocation */
		atFrame->structureID = (int)str_id;
		atFrame->invokeStyle = REMAP_INVOKE(invoke_style);
		atFrame->startFrame  = (int)start_frame;
		atFrame->endFrame    = (int)end_frame;

		/* Using these labels for editing control */
		atFrame->startLabel  = getNextLabel();
		atFrame->endLabel    = getNextLabel();

		/*----------------------------------------------------*\
		|	Set up the Invocation Sub-entity
		|
		|	Get the address of the referenced structure
		|	Check if we ran out of memory.
		\*----------------------------------------------------*/
		invoke_me               = &atFrame->invoke;
		if ( invoke_style == CALL )
		{
			HEADER(invoke_me, CALL_STRUCTURE,
				do_callstructure, NULL);
			invoke_me->startLabel = atFrame->startLabel;
			invoke_me->endLabel   = atFrame->endLabel;
		}
		else
		{
			HEADER(invoke_me, EXECUTE_STRUCTURE,
				do_executestructure, NULL);
		}
		
		invoke_me->structure_id = (int)str_id;
		invoke_me->structure_ptr =
			db_get_str((BIF_INT)invoke_me->structure_id);
		ENT_ERROR(invoke_me->structure_ptr );

		/* Alloc and copy to a stored entity */
		ent = new_generic(&temp_ent,ent_size,INVOKE_AT_FRAME,
					do_invokeatframe);

		/* Error check for ent == NULL ( FATAL ) */
		ENT_ERROR(ent);

		/* Add to the open structure */
		Traverse(traverser_state, ent);

#ifdef USING_PHIGS
		/* Call the entity in PHIGS */
		/* Insert the Edit Labels here  */
		plabel((Pint)temp_ent.invokeatframe.startLabel);
		plabel((Pint)temp_ent.invokeatframe.endLabel);
#endif /* USING_PHIGS */
	}
	else
	{
		/* There is no immediate mode meaning to the invoke
			at frame */
	}


#endif /* PRINT_ONLY */
} /* End bif_invokeatframe() */

