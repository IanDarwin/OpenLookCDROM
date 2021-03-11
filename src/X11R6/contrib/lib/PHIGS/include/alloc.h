/* $XConsortium: alloc.h,v 5.2 94/04/17 20:41:36 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

/* Assures that allocated memory is freed when *alloc is unsuccessful */

#ifndef ALLOC_INCLUDED
#define ALLOC_INCLUDED

/* Use this macro to declare as many holders for malloc'd pointers as needed. */
/* There is no error checking for exceeding the array size, so be generous. */
#define	ALLOC_DECLARE(max)					\
		char	* _allocated[(max)];			\
		char	**_alloc_ptr = _allocated	/* caller will ; */
/* _alloc_ptr points AFTER the last allocated (successful) pointer.	*/

/* After each call to malloc/realloc, merely:	ALLOCATED(var); 	*/
#define	ALLOCATED(ptr)		(*_alloc_ptr++ = (char*)(ptr))

/* To test the 1 most recent ALLOCATED failed:	if (ALLOC_FAILED)	*/
#define	ALLOC_FAILED		( *(_alloc_ptr - 1) == NULL )

/* In case of failure, merely:			ALLOC_FREE;		*/
#define	ALLOC_FREE	while (--_alloc_ptr >= _allocated)	\
			    if (*_alloc_ptr)			\
				free(*_alloc_ptr)	/* caller will ; */
/* **RESTRICTION** if realloc moves the data & returns a new pointer,
 *		   then this new pointer must be freed, and the old pointer
 *		   must NOT be freed.  This means that ALLOC_FREE can't
 *		   be used for this pointer.
 */

#define	ALLOC_RESET		(_alloc_ptr = _allocated)

#ifdef EXAMPLE_USES
/* The following shows several different ways to use these macros.  */

#define SUCCESS		1
#define FAILURE		0
int
routine()
{
    struct  s1	{
	int	a, b;
    }		*ptr1;
    double	*ptr2;
    int 	*ptr3;
    ALLOC_DECLARE(10);

    ptr1 = (struct s1 *) malloc((unsigned)100 * sizeof(struct s1));
    if (! ALLOCATED(ptr1)) {
	ALLOC_FREE;
	return FAILURE;
    }

    ALLOCATED( ptr2 = (double *) malloc((unsigned) 200) );
    if (ALLOC_FAILED) {
	ALLOC_FREE;
	return FAILURE;
    }

    if (! ALLOCATED(ptr3 = (int *) malloc((unsigned) 300))) {
	ALLOC_FREE;
	return FAILURE;
    }

    /* . . . Use ptr1 and ptr2 and ptr3 . . . */

    /* Free the variables that are no longer needed */
    free( (char*)ptr2);
    free( (char*)ptr3 );
    return SUCCESS;
}
#endif /* EXAMPLE_USES */

#endif /* ALLOC_INCLUDED */
