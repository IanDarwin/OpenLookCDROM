/* $XConsortium: db_tools.c,v 5.3 94/04/17 20:44:33 rws Exp $ */
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
| File          :	db_tools.c
| Date          :	3/18/89
| Project       :	PLB
|
| Description   :	Routines that maintain the BIF strutures list,
|			find the end of lists, free data (wholesale),
|			and match str_id's with the pointers.
|
| Status	:	Version 1.0
|
| Revisions	:
|
|	11/22/89	Paul Chek DEC:
|			in expand and collapse structure, before doing
|			the actual collapse or expand, close and save the
|			current open structure if any.  After the collapse
|			or expand reopen the structure.
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
|	contour_counter(int, *Real_int_union, int)
|		:	Count the Contents of the Current Contour Clearly, 
|	contour_sizes(int, int, *Real_int_union, *int, int)
|		:	Find the size of each countour of list provided.
|	BIF_All *end_of_list(*BIF_All)
|		:	Find the end of the NULL terminated linked list
|	void free_all_list(*BIF_All)
|		:	Delete the NULL terminated linked list of entities
|	BIF_Beginstructure *db_inq_str(int)
|		:	Check for the existance of a given structure
|	void db_add_str(*BIF_Beginstructure)
|		:	Add the given structure to the data base
|	BIF_Beginstructure *db_get_str(int)
|		:	Get the pointer to a given structure. If the
|	void db_empty_str(int)
|		:	Empty the given structure (if it exsists).
|	void db_clear_all()
|		:	Clear all structures from memory.
|	int setNextLabel(int)
|		:	Set the base of the unique label range.
|	int getNextLabel()
|		:	Get the next unique label value.
|	int addToList(int, *int, *int, int)
|		:	Add the value "new" at the end of the array
|	int delFromList(int, *int, *int, int)
|		:	Delete the value "old" from the array
|	int expandAllStructures();
|		:	Expand all structures to include the "called"
|	expandStructure(* BIF_Beginstructure)
|		:	Expand the given structure and any descendants
|	int collapseAllStructures();
|		:	Collapse all structures that include the
|	int collapseStructure(*BIF_Beginstructure)
|		:	Collapse out the copied "called" elements from
|	int openUsingLinkList(* slList);
|		:	For PHIGS systems that do not support the 
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Include files
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
#include "biftypes.h"
#include "bifparse.h"
#include "new_ents.h"
#include "ph_map.h"


/*--------------------------------------------------------------------*\
| Local global variables
\*--------------------------------------------------------------------*/
/* The root of the structures database */
static BIF_Beginstructure *root_of_str_list = NULL;
static int nextLabel = 1;

/*--------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE 
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
| Procedure     :	contour_counter(int, *Real_int_union, int)
|-----------------------------------------------------------------------
| Description   :	Count the Contents of the Current Contour Clearly, 
|			Concisely, Clerically, Cleanly, Cleverly, and
|			Carefully.
|-----------------------------------------------------------------------
| Return        :	The total number of records in the contour list
\*--------------------------------------------------------------------*/
contour_counter(number,list,record_size)
int number, record_size;
Real_int_union *list;
{
	int i, cont_size, total;
	total = 0;
	for ( i = 0; i < number; i++ )
	{
		cont_size = list->Int;
		total += cont_size;
		list += record_size * cont_size + 1;
#ifdef TEST_PRINT
		fprintf(stderr,"contour_counter list->Int = %d\n",list->Int);
		fflush(stderr);
#endif
	}
	return(total);
} /* End procedure contour_counter */

/*--------------------------------------------------------------------*\
| Procedure     :	contour_sizes(int, int, *Real_int_union,
|					*int, int)
|-----------------------------------------------------------------------
| Description   :	Find the size of each countour of list provided.
|
|	if opFlag = 0 --> fill in the sizeList 
|	else test against the  sizeList.
|-----------------------------------------------------------------------
| Return        :	Completion status
|
|	if return > 0 Number of records in the contour list
|	if return < 0 Number (1 base) of contour on which their
|			 is a size mismatch (opFlag != 0 only)
\*--------------------------------------------------------------------*/
contour_sizes(number,list,record_size,sizeList,opFlag)
int number, record_size;
Real_int_union *list;
int		*sizeList;
int		opFlag;

{
	int i, cont_size, total;
	/*------------------------------------------------------------*\
	|	Find/Check/Save the sizes of each of the contours.
	\*------------------------------------------------------------*/
	total = 0;
	for ( i = 0; (i < number) && (total > -1 ); i++ )
	{
		/*----------------------------------------------------*\
		|	Find the size of the contour
		\*----------------------------------------------------*/
		cont_size = list->Int;
		total += cont_size;
		list += record_size * cont_size + 1;
		/*----------------------------------------------------*\
		|	Test or save the sizeList information
		\*----------------------------------------------------*/
		if ( opFlag == 0 )
		{
			sizeList[i] = cont_size;
		}
		else
		{
			if (cont_size != sizeList[i])
				total = -(i + 1);
		}
	}
	return(total);
} /* End procedure contour_sizes */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_All *end_of_list(*BIF_All)
|------------------------------------------------------------------------|
| Description   :	Find the end of the NULL terminated linked list
|			of entities
|------------------------------------------------------------------------|
| Return        :	The pointer to the last entity in the list -or-
|			NULL if ent is NULL.
\*----------------------------------------------------------------------*/
BIF_All *end_of_list(ent)
BIF_All *ent;
{
	BIF_All *previous;
/* Walk all entities to the end of the list */
	previous = ent;
	while ( ent != NULL )
	{
		previous = ent;
		ent = (ent->any).next;
	}


/* The end of the list, which is the entity "previous" to NULL */
	return(previous);
} /* End procedure free_all_list */ 

/*----------------------------------------------------------------------*\
| Procedure     :	void free_all_list(*BIF_All)
|------------------------------------------------------------------------|
| Description   :	Delete the NULL terminated linked list of entities
|			Assumes all entities are singly malloc'd 
|------------------------------------------------------------------------|
| Return        :	None
\*----------------------------------------------------------------------*/
void free_all_list(ent)
BIF_All *ent;
{
	BIF_All *e_next;
/* Free all entities to the end of the list */
	while ( ent != NULL )
	{
		e_next = (ent->any).next;
		free((char *)ent);
		ent = e_next;
	}
} /* End procedure free_all_list */ 

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Beginstructure *db_inq_str(int)
|------------------------------------------------------------------------|
| Description   :	Check for the existance of a given structure
|------------------------------------------------------------------------|
| Return        :	The address of the requested structure or NULL
\*----------------------------------------------------------------------*/
BIF_Beginstructure *db_inq_str(target_id)
BIF_INT target_id;
{
	BIF_Beginstructure *str, *found;
	found = NULL;
	str = root_of_str_list;
	
/* Simple linear search */
	found = NULL;
	while( str != NULL && found == NULL )
	{
		if ( str->structure_id == target_id )
			found = str;
		else
			str = &str->next->beginstructure;
	}
/* Tell the Calling routine what we found */
	return(found);
} /* End procedure db_inq_str */

/*----------------------------------------------------------------------*\
| Procedure     :	void db_add_str(*BIF_Beginstructure)
|------------------------------------------------------------------------|
| Description   :	Add the given structure to the data base
|------------------------------------------------------------------------|
| Return        :	None
\*----------------------------------------------------------------------*/
void db_add_str(structure)
BIF_Beginstructure *structure;

{
/* Check for valid data */
	if ( structure != NULL )
	{
	/* Insert at the top (order is not important */
		structure->next = (BIF_All *)root_of_str_list;
		root_of_str_list   = structure;
	}
	
} /* End procedure db_add_str */

/*----------------------------------------------------------------------*\
| Procedure     :	BIF_Beginstructure *db_get_str(int)
|------------------------------------------------------------------------|
| Description   :	Get the pointer to a given structure. If the
|			structure does not exist, it is created and add
|			to the structure list.
|------------------------------------------------------------------------|
| Return        :	The address of the requested structure
\*----------------------------------------------------------------------*/
BIF_Beginstructure *db_get_str(structure_id)
BIF_INT structure_id;
{
	BIF_Beginstructure *str;
/* Find out in the structure exists in the database */	
	if ( ( str = db_inq_str(structure_id) ) == NULL )
	{
	/* It doesn't. Allocate a new BEGIN_STRUCTURE entity */
		str = new_beginstructure((int)structure_id);
	/* Add it to the data base */
		db_add_str(str);
	}
/* Return the address of the structure */
	return(str);
} /* End procedure db_get_str */

/*----------------------------------------------------------------------*\
| Procedure     :	void db_empty_str(int)
|------------------------------------------------------------------------|
| Description   :	Empty the given structure (if it exsists).
|------------------------------------------------------------------------|
| Return        :	None
\*----------------------------------------------------------------------*/
void db_empty_str(structure_id)
int structure_id;

{
	BIF_Beginstructure *str;

/* Find the structure */
	if ( ( str = db_inq_str((BIF_INT)structure_id) ) != NULL )
	{
	/* Delete the contents */
		free_all_list(str->top_of_list);
		str->top_of_list = NULL;
	}
		
} /* End procedure db_empty_str */

/*----------------------------------------------------------------------*\
| Procedure     :	void db_clear_all()
|------------------------------------------------------------------------|
| Description   :	Clear all structures from memory.
|------------------------------------------------------------------------|
| Return        :	None
\*----------------------------------------------------------------------*/
void db_clear_all()
{
	BIF_Beginstructure *str;
	str = root_of_str_list;
	
/* Clear each of the structures contents */
	while( str != NULL  )
	{
	/* Free the contents of the BIF Structure */
		free_all_list(str->top_of_list);
#ifdef USING_PHIGS
	/* Free the contents of the PHIGS Structure */
		pempty_struct((Pint)str->structure_id);
#endif /* USING_PHIGS */
		str = &str->next->beginstructure;
	}
		
/* Clear Structure Headers */
	free_all_list((BIF_All *)root_of_str_list);
	root_of_str_list = NULL;
		
} /* End procedure db_clear_all */

/*--------------------------------------------------------------------*\
| Procedure     :	int setNextLabel(int)
|---------------------------------------------------------------------
| Description   :	Set the base of the unique label range.
|			They are kept unique by allocating them sequentially
|			using the getNextLabel() call. This function allows
|			the base of the label range to be set.
|
|			CALL_STRUCTURE in PHIGs requires unique labels. 
|---------------------------------------------------------------------
| Return        :	Error Code ( N I )
\*--------------------------------------------------------------------*/
int setNextLabel(labelBase)
int labelBase;

{
	nextLabel = labelBase;
} /* setNextLabel() */

/*--------------------------------------------------------------------*\
| Procedure     :	int getNextLabel()
|---------------------------------------------------------------------
| Description   :	Get the next unique label value.
|			They are kept unique by allocating them sequentially
|			using the getNextLabel() call. This function returns
|			the next label of range.
|
|			CALL_STRUCTURE in PHIGs requires unique labels. 
|---------------------------------------------------------------------
| Return        :	The next unique label
\*--------------------------------------------------------------------*/
int getNextLabel()

{
	return( nextLabel++ );
} /* getNextLabel() */


/*--------------------------------------------------------------------*\
| Procedure     :	int addToList(int, *int, *int, int)
|---------------------------------------------------------------------
| Description   :	Add the value "new" at the end of the array
|			if it is not already there.
|---------------------------------------------------------------------
| Return        :	The new length of the list
\*--------------------------------------------------------------------*/
int addToList(new,listSize,list,listMax)
int new, *listSize, *list, listMax;

{
	int i, found;
	/*------------------------------------------------------------*\
	|	Check to see if the list is full 
	\*------------------------------------------------------------*/
	if ( *listSize < listMax )
	{
		/*----------------------------------------------------*\
		|	Check if "new" already exists
		\*----------------------------------------------------*/
		found = 0;
		for  ( i = 0 ; i < *listSize && !found ; i++ )
			found = (new == list[i]);
		
		if ( !found )
		{
			/*--------------------------------------------*\
			|	Add "new" to the list
			\*--------------------------------------------*/
			list[*listSize] = new;
			*listSize += 1;
		}
	}
	return(*listSize);
} /* End addToList */

/*--------------------------------------------------------------------*\
| Procedure     :	int delFromList(int, *int, *int, int)
|---------------------------------------------------------------------
| Description   :	Delete the value "old" from the array
|			if it is there.
|---------------------------------------------------------------------
| Return        :	The new length of the list
\*--------------------------------------------------------------------*/
int delFromList(old,listSize,list)
int old, *listSize, *list;

{
	int i, shift;
	/*----------------------------------------------------*\
	|	Check if "old" exists
	\*----------------------------------------------------*/
	shift = 0;
	for  ( i = 0 ; i < *listSize ; i++ )
	{
		if ( list[i] != old )
		{
			/* copy */
			list[i-shift] = list[i];
		}
		else
		{
			/* Skip */
			shift++;
			*listSize -= 1;
		}
	}

	return(*listSize);
} /* End delFromList */


/*--------------------------------------------------------------------*\
| Procedure     :	int expandAllStructures();
|---------------------------------------------------------------------
| Description   :	Expand all structures to include the "called"
|			structure data.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int expandAllStructures()

{
	BIF_Beginstructure *str;

#ifndef REFER_STRUCTURE_EXISTS
	/*------------------------------------------------------------*\
	|	For all structures...
	|	expandStructure insures that children are ALWAYS 
	|	exapanded before their parents.
	\*------------------------------------------------------------*/
	str = root_of_str_list;
	while (str != NULL )
	{
		expandStructure(str);
		str = &str->next->beginstructure;
	}
#endif /* REFER_STRUCTURE_EXISTS */
}

/*--------------------------------------------------------------------*\
| Procedure     :	expandStructure(* BIF_Beginstructure)
|---------------------------------------------------------------------
| Description   :	Expand the given structure and any descendants
|			it may have.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
expandStructure(str)
BIF_Beginstructure *str;

{
	BIF_All *ent;
	BIF_Callstructure *cent;
	int found, top;

#ifdef TEST_PRINT
printf("expand str 0x%x \n", str);
#endif /* TEST_PRINT */
#ifndef REFER_STRUCTURE_EXISTS
	/*------------------------------------------------------------*\
	|	Initialize the list checking and see if we have been
	|	here as well.
	\*------------------------------------------------------------*/
	if ( str == NULL )
		ent = NULL;
	else if ( !str->expanded )
		ent = str->top_of_list;
	else
		ent = NULL;

	/*------------------------------------------------------------*\
	|	Look for CALL_STRUCTURE entities
	\*------------------------------------------------------------*/
	while ( ent != NULL )
	{
		/* Scan the entities in the structure */
		found = 0;
		while ( ent != NULL && !found )
		{
#ifdef TEST_PRINT2
printf("entity_type %d \n", ent->any.entity_type);
fflush(stdout);
#endif /* TEST_PRINT2 */
			if ( ent->any.entity_type == CALL_STRUCTURE )
				found = 1;
			else
				ent = ent->any.next;
		}
		
		if ( ent != NULL )
		{
			/* We found a callstructure entity */
			cent = &ent->callstructure;

			/* Expand the child (and descendants) */
#ifdef TEST_PRINT
printf("expanding child \n");
#endif /* TEST_PRINT */
			expandStructure(cent->structure_ptr);
#ifdef TEST_PRINT
printf("done expanding child \n");
#endif /* TEST_PRINT */

#ifdef USING_PHIGS

			top = 0;
#ifdef TEST_PRINT
printf("popst (%d)\n",str->structure_id);
fflush(stdout);
#endif /* TEST_PRINT */

			{
			    /* But FIRST, close and save the ID of any
			       open structures */
			    Pint errind, stid, ep;
			    Popen_struct_status stype;
			    
			    pinq_open_struct(&errind, &stype, &stid);
			    if (stype == PSTRUCT_OPEN) {
				pinq_elem_ptr(&errind, &ep);
				pclose_struct();
			    }

			    popen_struct((Pint)str->structure_id);
#ifdef TEST_PRINT
printf("psep(%d)\n",top);
fflush(stdout);
#endif /* TEST_PRINT */
			    pset_elem_ptr((Pint)top);
#ifdef TEST_PRINT
printf("pseplb(%d)\n",cent->startLabel);
fflush(stdout);
#endif /* TEST_PRINT */
			    pset_elem_ptr_label((Pint)cent->startLabel);
#ifdef TEST_PRINT
printf("pcelst(%d)\n",cent->structure_id);
fflush(stdout);
#endif /* TEST_PRINT */
			    pcopy_all_elems_struct((Pint)cent->structure_id);
#ifdef TEST_PRINT
printf("pclst()\n");
fflush(stdout);
#endif /* TEST_PRINT */
			    pclose_struct();
#ifdef TEST_PRINT
printf("done.\n");
fflush(stdout);
#endif /* TEST_PRINT */


			    /* If a structure was open the reopen it and
			       set the element pointer */
			    if (stype == PSTRUCT_OPEN) {
				popen_struct(stid);
				pset_elem_ptr(ep);
			    }
			}
#endif /* USING_PHIGS */
			ent = ent->any.next;
		    }
	    }
#endif /* REFER_STRUCTURE_EXISTS */

	/* Mark the Structure as E X P A N D E D */
	if ( str != NULL )
	    str->expanded = TRUE;

} /* End expandStructure */


/*--------------------------------------------------------------------*\
| Procedure     :	int collapseAllStructures();
|---------------------------------------------------------------------
| Description   :	Collapse all structures that include the
|			copied "called" structure data.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int collapseAllStructures()

{
	BIF_Beginstructure *str;
#ifndef REFER_STRUCTURE_EXISTS

	/*------------------------------------------------------------*\
	|	For all structures...
	|	collapse them.
	|	exapaneded before their parents.
	\*------------------------------------------------------------*/
	str = root_of_str_list;
	while (str != NULL )
	{
		collapseStructure(str);
		str = &str->next->beginstructure;
	}
#endif /* REFER_STRUCTURE_EXISTS */
}

/*--------------------------------------------------------------------*\
| Procedure     :	int collapseStructure(*BIF_Beginstructure)
|---------------------------------------------------------------------
| Description   :	Collapse out the copied "called" elements from
|			the PHIGS structure.
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
collapseStructure(str)
BIF_Beginstructure *str;

{
	BIF_All *ent;
	BIF_Callstructure *cent;
	int found, top;


#ifndef REFER_STRUCTURE_EXISTS
	/*------------------------------------------------------------*\
	|	Look for CALL_STRUCTURE entities
	\*------------------------------------------------------------*/
	if ( str != NULL )
	    ent = str->top_of_list;
	else
	    ent = NULL;

	while ( ent != NULL )
	{
	    /* Scan the entities in the structure */
	    found = 0;
	    while ( ent != NULL && !found )
	    {
#ifdef TEST_PRINT
		printf("entity_type %d \n", ent->any.entity_type);
#endif /* TEST_PRINT */
		if ( ent->any.entity_type == CALL_STRUCTURE )
		    found = 1;
		else
		    ent = ent->any.next;
	    }
		
	    if ( ent != NULL )
	    {
		/* We found a callstructure entity */
		cent = &ent->callstructure;
#ifdef USING_PHIGS


		{
		    /* But FIRST, close and save the ID of any open
		       structures */
		    Pint errind, stid, ep;
		    Popen_struct_status stype;
			    
		    pinq_open_struct(&errind, &stype, &stid);
		    if (stype == PSTRUCT_OPEN) {
			pinq_elem_ptr(&errind, &ep);
			pclose_struct();
		    }

		    /* Delete the Copied data (if any) */
		    popen_struct((Pint)str->structure_id);
		    top = 0;
		    pset_elem_ptr((Pint)top);
		    pdel_elems_labels((Pint)cent->startLabel,
				    (Pint)cent->endLabel);
		    pcopy_all_elems_struct((Pint)cent->structure_id);
		    pclose_struct();

		    /* If a structure was open the reopen it and
		       set the element pointer */
		    if (stype == PSTRUCT_OPEN) {
			popen_struct(stid);
			pset_elem_ptr(ep);
		    }
		}

#endif /* USING_PHIGS */
		ent = ent->any.next;
	    }
	}
#endif /* REFER_STRUCTURE_EXISTS */

	if ( str != NULL )
	    str->expanded = FALSE;
} /* End collapseStructure */


/*--------------------------------------------------------------------*\
| Procedure     :	int openUsingLinkList(* slList);
|---------------------------------------------------------------------
| Description   :	For PHIGS systems that do not support the 
|			refer structure element (extension).  A given
|			expanded structure instance is referenced by
|			the PHIGs structure ID containing the instance
|			and a list of CALL_STRUCTURE starting labels.
|
|			This information is contained in the singly
|			linked list passed, the data field of the
|			head of the list contains the PHIGs structure id 
|			with the data fields of the remainder the label
|			of the invocation path.
|
|---------------------------------------------------------------------
| Return        :	Error Code (Not Implemented)
\*--------------------------------------------------------------------*/
int openUsingLinkList(lnList)
slList *lnList;

{
#ifdef USING_PHIGS
	int top = 0;

	if ( lnList != NULL )
	{
		/* Open the Structure */
#ifdef TEST_PRINT
printf("openUsingLinkList: popst(%d)\n",lnList->data);
fflush(stderr);
#endif /* TEST_PRINT */
		popen_struct((Pint)(lnList->data));

#ifdef TEST_PRINT
printf("openUsingLinkList: psep(%d)\n",top);
fflush(stderr);
#endif /* TEST_PRINT */
		pset_elem_ptr((Pint)top);

		/* Follow the CALL invocation path */
		lnList = lnList->next;
		while ( lnList != NULL )
		{
#ifdef TEST_PRINT
printf("openUsingLinkList: pseplb(%d)\n",lnList->data);
fflush(stderr);
#endif /* TEST_PRINT */
			pset_elem_ptr_label((Pint)(lnList->data));
			lnList = lnList->next;
		}
#ifdef TEST_PRINT
printf("done.\n");
fflush(stderr);
#endif /* TEST_PRINT */
		return(0);
	}
	return(-1);

#endif /* USING_PHIGS */
} /* openUsingLinkList() */

