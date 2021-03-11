/* $XConsortium: listbufs.c,v 5.3 94/04/17 20:44:38 rws Exp $ */
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
| Author        :	jmz / SimGraphics Engineering Corportation
|
| File          :	listbufs.c
| Date          :	3/15/89
| Project       :	PLB
| Description   :	Contains the functions to control data
|			accumulation from the parser.
| Status        :	Version 1.0
|
| Revisions     :	
|	2/90		Staff SimGEC:
|				BUFFER_OVERFLOW testing added.
|				Repeated groups allowed.
|				NULL current group identified.
|				bif_real_list added.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	int bif_intlist(BIF_INT)
|		:	Recieve an int value from the parser, add to list
|	int bif_real_list(BIF_REAL)
|		:	Recieve a real value from the parser, add to list
|	int bif_pair(BIF_REAL, BIF_REAL )
|		:	Recieve a real pair from the parser, add to list
|	int bif_triplet(BIF_REAL, BIF_REAL, BIF_REAL )
|		:	Recieve a real triplet from the parser, add to list
|	int init_triplets()
|		:	Initialize the triplet list for point-accumulation
|	int end_triplets(**Real_int_union)
|		:	Return the results of the simple point-accumulation
|	int init_groups()
|		:	Initialize group accumulation headers.
|	int end_groups( **Group_description )
|		:	Return the number of accumulated groups and the 
|	int bif_group( BIF_INT )
|		:	Set the current group to the id spec. by the parser
|	int init_contours( BIF_INT )
|		:	Initialize the accumalator for simple contours
|	int end_contours( **Real_int_union )
|		:	Return the number of accumulated contours and the 
|	int bif_contour( BIF_INT )
|		:	Begin / end a contour (faset3 and fasetdata3 only)
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include "biftypes.h"
#include "bifbuild.h"
#include "bifmacro.h"
#include "ph_map.h"

/*--------------------------------------------------------------------*\
|	Local #define
\*--------------------------------------------------------------------*/
#ifdef EXTERNAL_NOTE
	Buffer overflow testing has been added.
#endif /* EXTERNAL_NOTE */

#define INPUT_BUFFER_SIZE 125000
#define MAX_GROUPS_DEFINE 500

/*--------------------------------------------------------------------*\
|	Local MACROS  
\*--------------------------------------------------------------------*/
#define BUFFER_OVF(n)\
{\
	if ( (list_current + n - input_buffer) > INPUT_BUFFER_SIZE )\
		ERROR("FATAL: Input Buffer Overflow.");\
}

#define PRINT_MATRIX44(mat) \
{\
	printf("Row1: %lf %lf %lf %lf\n",mat[0][0],mat[0][1],mat[0][2],mat[0][3]);\
	printf("Row2: %lf %lf %lf %lf\n",mat[1][0],mat[1][1],mat[1][2],mat[1][3]);\
	printf("Row3: %lf %lf %lf %lf\n",mat[2][0],mat[2][1],mat[2][2],mat[2][3]);\
	printf("Row4: %lf %lf %lf %lf\n",mat[3][0],mat[3][1],mat[3][2],mat[3][3]);\
} /* End macro PRINT_MATRIX44 */

/*--------------------------------------------------------------------*\
| Local global variables
\*--------------------------------------------------------------------*/

static Real_int_union input_buffer[INPUT_BUFFER_SIZE];
static Real_int_union *list_current, *contour_base;
static int list_counter;
static int number_of_groups;
static int number_of_contours;
static Group_description group_header[MAX_GROUPS_DEFINE];
static Group_description *current_group;

/*--------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE
\*--------------------------------------------------------------------*/

/* ****************************************
* Value List Accumulation Utilities
* ************************************** */
/*----------------------------------------------------------------------*\
| Procedure	:	int bif_intlist(BIF_INT)
|------------------------------------------------------------------------|
| Description	:	Recieve an int value from the parser, add to list
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_intlist(int_value)
BIF_INT int_value;
{
#ifdef TEST_PRINT_2
	printf("Adding %d to list\n",int_value);
#endif /* TEST_PRINT_2 */
#ifndef PRINT_ONLY
	list_counter++;
	BUFFER_OVF(1);
	(list_current++)->Int = int_value;
#endif /* PRINT_ONLY */
} /* End procedure bif_intlist */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_real_list(BIF_REAL)
|------------------------------------------------------------------------|
| Description	:	Recieve a real value from the parser, add to list
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_real_list(real_value) /* ver 1.0 */
BIF_REAL real_value;

{
#ifdef TEST_PRINT_2
        printf("Adding %d to list\n",real_value);
	/*--------------*/
#endif /* TEST_PRINT_2 */
      /*--------------*/

#ifndef PRINT_ONLY
        list_counter++;
	BUFFER_OVF(1);
        (list_current++)->Float = real_value;
	/*------------*/
#endif /* PRINT_ONLY */
      /*------------*/
} /* End procedure bif_real_list */


/*----------------------------------------------------------------------*\
| Procedure	:	int bif_pair(BIF_REAL, BIF_REAL )
|------------------------------------------------------------------------|
| Description	:	Recieve a real pair from the parser, add to list
|			as a triplet
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_pair(x, y )
BIF_REAL x, y;
{
	bif_triplet(x, y, (BIF_REAL)0.0 );
}

/*----------------------------------------------------------------------* 
| Procedure	:	int bif_triplet(BIF_REAL, BIF_REAL, BIF_REAL )
|------------------------------------------------------------------------|
| Description	:	Recieve a real triplet from the parser, add to list
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_triplet(x, y, z )
BIF_REAL x, y, z;
{
#ifdef TEST_PRINT_2
	printf("Adding %f %f %f to list\n",x, y, z);
#endif /* TEST_PRINT_2 */
#ifndef PRINT_ONLY
	list_counter++;
	BUFFER_OVF(3);
	(list_current++)->Float = x;
	(list_current++)->Float = y;
	(list_current++)->Float = z;
#endif /* PRINT_ONLY */
} /* End procedure bif_triplet */

/*----------------------------------------------------------------------*\
| Procedure: int init_triplets()
|------------------------------------------------------------------------|
| Description: Initialize the triplet list for simple point-accumulation
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/
int init_triplets()
{
#ifdef TEST_PRINT
	printf("Initializing triplet list\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	list_counter = 0;
	list_current = input_buffer;
#endif /* PRINT_ONLY */
} /* End procedure init_triplets */

/*----------------------------------------------------------------------*\
| Procedure: int end_triplets(list_ptr)
|------------------------------------------------------------------------|
| Description: Return the results of the simple point-accumulation
|------------------------------------------------------------------------|
| Return: Number of triplets accumulated
\*----------------------------------------------------------------------*/
int end_triplets(list_ptr)
Real_int_union *(*list_ptr);
{
#ifdef TEST_PRINT
	printf("Ending triplet list\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
	*list_ptr = list_current - (list_counter * 3);
	return(list_counter);
#endif /* PRINT_ONLY */
} /* End procedure end_triplets */

/*----------------------------------------------------------------------*\
| Procedure: int init_groups()
|------------------------------------------------------------------------|
| Description: Initialize the defined accumulation groups.
|		Set the current group to NULL.
|------------------------------------------------------------------------|
| Return: None
\*----------------------------------------------------------------------*/
void init_groups()
{
#ifdef TEST_PRINT
	printf("Initializing groups\n");
#endif /* TEST_PRINT */
	number_of_groups = 0;
	current_group    = NULL;
	list_current     = input_buffer;
} /* End procedure int init_groups */

/*----------------------------------------------------------------------*\
| Procedure: int end_groups( group_ptr )
|------------------------------------------------------------------------|
| Description: Return the number of accumulated groups and the 
|		pointer to the group description array
|------------------------------------------------------------------------|
| Return: The number of groups accumulated
\*----------------------------------------------------------------------*/
int end_groups(group_ptr)
Group_description *(*group_ptr);
{
#ifdef TEST_PRINT
	printf("Ending groups\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Store the address of the group header array */
	*group_ptr = group_header;

/* Return the number of groups */
	return(number_of_groups);

#endif /* PRINT_ONLY */
} /* End end_groups */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_group( BIF_INT )
|------------------------------------------------------------------------|
| Description	:	Set the current group to the id spec. by the parser
|------------------------------------------------------------------------|
| Return	:	Error Code
|	0	Success
|	-1	Failure --- Out of groups (FATAL)
\*----------------------------------------------------------------------*/
int bif_group( group_id )
BIF_INT group_id;
{
	int ret_code, found;
	int group;
	/* Check for BIF_END_OF_GROUP */
	if ( (group_id == BIF_END_OF_GROUP) && (current_group != NULL) )
	{
#ifdef TEST_PRINT
		printf("Ending current group\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
		/* Close out the current group. 
			Get the size of the current group */
		current_group->number = list_counter;
		ret_code =  0; /* Success */
#endif /* PRINT_ONLY */
	}
	else
	{
#ifdef TEST_PRINT
		printf("Setting current group to %d\n",group_id);
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
		/* Check if we can add another group... */
		if ( number_of_groups < MAX_GROUPS_DEFINE )
		{
			ret_code = 0; /* Success, new group */

			/* Set up the new group */
			current_group = &group_header[number_of_groups++];
			current_group->list   = list_current;
			current_group->number = 0;
			current_group->name   = group_id;

			/* Reset the size counter */
			list_counter = 0;

		}
		else
		/* Failure. Out of groups */
			ERROR("FATAL: Out of groups.");
#endif /* PRINT_ONLY */
	}
	
#ifndef PRINT_ONLY
	return( ret_code );
#endif /* PRINT_ONLY */
} /* End procedure bif_group */

/*----------------------------------------------------------------------*\
| Procedure	:	int init_contours( BIF_INT )
|------------------------------------------------------------------------|
| Description	:	Initialize the accumalator for simple contours
|
|			Contours are used to avoid the implementation of
|			nested groups.  They are used only for the
|			fill_area_set (2D & 3D) entities.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
void init_contours( begin_or_end )
BIF_INT begin_or_end ;
{
#ifdef TEST_PRINT
	printf("Initializing contours\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Clear the contour counter */
	list_counter = 0;
/* Initialize the list accumulation buffer */
	list_current = input_buffer;
#endif /* PRINT_ONLY */
} /* End init_contours */

/*----------------------------------------------------------------------*\
| Procedure	:	int end_contours( **Real_int_union )
|------------------------------------------------------------------------|
| Description	:	Return the number of accumulated contours and the 
|			pointer to the contour list buffer.
|
|			Contours are used to avoid the implementation of
|			nested groups.  They are used only for the
|			fill_area_set (2D & 3D) entities.
|------------------------------------------------------------------------|
| Return	:	The number of contours accumulated.
\*----------------------------------------------------------------------*/
int end_contours(list_ptr)
Real_int_union *(*list_ptr);
{
#ifdef TEST_PRINT
	printf("Ending contours\n");
#endif /* TEST_PRINT */
#ifndef PRINT_ONLY
/* Store the address of the list accumulation buffer */
	*list_ptr = input_buffer;

/* Return the number of contours */
	return(list_counter);

#endif /* PRINT_ONLY */
} /* End end_contours */

/*----------------------------------------------------------------------*\
| Procedure	:	int bif_contour( BIF_INT )
|------------------------------------------------------------------------|
| Description	:	Begin / end a contour (faset3 and fasetdata3 only)
|
|	BIF_P_BEGIN 	begin contour
|	BIF_P_END	end contour
|
|			Contours are used to avoid the implementation of
|			nested groups.  They are used only for the
|			fill_area_set (2D & 3D) entities.
|------------------------------------------------------------------------|
| Return	:	Error Code
\*----------------------------------------------------------------------*/
int bif_contour( begin_or_end )
BIF_INT begin_or_end ;
{
#ifdef TEST_PRINT_2
	BEGEND(contour);
#endif /* TEST_PRINT_2 */
#ifndef PRINT_ONLY
	switch ( begin_or_end )
	{
	case BIF_P_BEGIN:
	/* Start a new contour */
	/* Save the contour counter */
		number_of_contours = ++list_counter;
	/* Save the contour size location */
		contour_base = list_current;
	/* Set the contour size to zero,  Just to be safe */
		BUFFER_OVF(1);
		(list_current++)->Int = 0;
	/* Set the size counter to zero */
		list_counter = 0;
		break;

	case BIF_P_END:
	/* End the current contour */
	/* Save the contour size */
		contour_base->Int = list_counter;
	/* Restore the contour counter */
		list_counter = number_of_contours;
		break;
	}
#endif /* PRINT_ONLY */
} /* End procedure bif_contour */
