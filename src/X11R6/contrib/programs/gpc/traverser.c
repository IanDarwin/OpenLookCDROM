/* $XConsortium: traverser.c,v 5.2 94/04/17 20:44:45 rws Exp $ */
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
|
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
| Author        :	nde / SimGraphics Engineering Corportation
|
| File          :	traverser.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
|
| Description	:	The traverser handling functions for build
|			and execute mode.
|
| Status	:  	Version 1.0
|
|			Non-bullet proof.  Executute will branch to NULL
|			if given the value.  Could use this level of
|			inteligence, but... the traversers are DUMB by
|			design, it's up to the handling function to
|			figure out what an entity dones.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	void build_traverser( *BIF_Traverser_state, *BIF_All )
|		:	Inserts the NULL terminated linked list of
|	void execute_traverser(*BIF_Traverser_state, *BIF_All )
|		:	The BIF execute traverser: FULLY DUMB.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "bifbuild.h"
#include "biftypes.h"
#include "bifparse.h"
#include "db_tools.h"
#include "ph_map.h"

/*--------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE                                                
\*--------------------------------------------------------------------*/

/*----------------------------------------------------------------------*\
| Procedure	:	void build_traverser( *BIF_Traverser_state,
|						*BIF_All )
|------------------------------------------------------------------------
| Description	:	Inserts the NULL terminated linked list of
|			entities into the open structure between
|			insert_after and insert_after.next.  Update the
|			insert after point to the end of the given list.
|			Typically, lists are single entities with
|			NULL nexts.
|
|			If the insert_after point is NULL insert at
|			top_of_list.
|------------------------------------------------------------------------
| Return	:	
\*----------------------------------------------------------------------*/
build_traverser( traverser_state, bif_entity )
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;

{

	BIF_All *next, *end_list;
	if ( traverser_state->insert_after != NULL )
	{
	/* Save the old next */
		next = traverser_state->insert_after->any.next;

	/* Make the top of the list the next of the insert_after entity */
		traverser_state->insert_after->any.next = bif_entity;
	}
	else
	{
	/* Insert at top of list*/
	/* The old next in the old top_of_list ( or empty structure ) */
		next = traverser_state->open_structure->top_of_list;
	/* Insert the new entity */
		traverser_state->open_structure->top_of_list = bif_entity;
	}

/* Find the end of list */
	end_list = end_of_list(bif_entity);

/* Link the end of list to the old next */
	end_list->any.next = next;

/* Update the insert after point to the end of list */
	traverser_state->insert_after = end_list;

}/* End BIF_build_traverser */

/*----------------------------------------------------------------------*\
| Procedure	:	void execute_traverser(*BIF_Traverser_state,
|						*BIF_All )
|------------------------------------------------------------------------
| Description	:	The BIF execute traverser: FULLY DUMB.
|			Invoke the handling function of each entity
|			in the NULL terminate list.
|------------------------------------------------------------------------
| Return	: 
\*----------------------------------------------------------------------*/
execute_traverser( traverser_state, bif_entity )
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;

{
	while ( bif_entity != NULL )
	{
#ifdef TEST_PRINT
/* #define TRACE_TRAV */
#ifdef TRACE_TRAV
{
char *find_keyword_token();
printf("Ex:%s\n",find_keyword_token((BIF_INT)bif_entity->any.entity_type));
fflush(stdout);
}
#endif /* TRACE_TRAV */
#endif /* TEST_PRINT */
		bif_entity->any.handler( traverser_state, bif_entity );
		bif_entity = bif_entity->any.next;
	}

} /* End BIF_execute_traverser */
