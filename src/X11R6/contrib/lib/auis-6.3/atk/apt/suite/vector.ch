/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


/* $Header $ */
/* $Source $ */





/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Vector-object

MODULE	vector.ch

VERSION	0.0

AUTHOR	GW Keim
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Vector-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  04/10/89	Created (GW Keim)

END-SPECIFICATION  ************************************************************/

#define vector_status_no_error			    0
#define vector_status_not_in_vector		    1
#define vector_status_end_of_vector		    3
#define vector_status_root_of_vector		    4
#define vector_status_empty			    5
#define vector_status_no_sort_routine		    6
#define vector_status_null_element		    7

class vector {
    classprocedures:
	InitializeObject( struct vector *self ) returns boolean;
	FinalizeObject( struct vector *self ) returns void;
	Create(long init_data_count, long reallocfactor ) returns struct vector*;
    macromethods:
	SetDestroyRoutine(routine) (self->destroyer=(long(*)())routine)
	SetSortRoutine(routine) (self->sorter=(long(*)())routine)
	Item(i) (self->data[i])
	Count() (self->current_used_count)
	SetIncrement(inc) (self->reallocation_factor = inc)
    methods:
	AddItem( long item ) returns long;
	RemoveItem( long item ) returns long;
	ItemExists( long item ) returns boolean;
	Sort() returns long;
	Subscript( long item ) returns long;
	Apply( long (*proc)() ) returns void;
    data:
	long		*data,
			 initial_vector_count,
			 current_vector_count,
			 current_used_count,
			 reallocation_factor,
			 debug,
		       (*apply)(),
		       (*sorter)(), 
		       (*destroyer)();
};



/*
    $Log: vector.ch,v $
*Revision 1.5  1993/05/04  01:06:17  susan
*RCS Tree Split
*
*Revision 1.4.1.1  1993/02/02  00:45:59  rr2b
*new R6tape branch
*
*Revision 1.4  1992/12/14  23:20:33  rr2b
*add $Logs back after disclaimerization took them out
*
Revision 1.2  1991/09/12  19:20:39  bobg
Update copyright notice

Revision 1.1  1989/04/28  20:26:46  tom
Initial revision

*/
