/* $XConsortium: cp_util.c,v 5.2 94/04/17 20:41:13 rws Exp $ */

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

/* Miscellaneous CP utility functions */

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include "version.h"

int
phg_cp_add_wst( cph, wst)
    register Cp_handle		cph;
    register Wst_handle		wst;
{
    int				status = 0;
    Cp_wst_list_entry		*node;

    if ( node = (Cp_wst_list_entry*)malloc( sizeof(*node)) ) {
	status = 1;
	node->wst = wst;
	node->next = cph->wst_list;
	cph->wst_list = node;
    }
    return status;
}


void
phg_cp_rmv_wst( cph, wst)
    register Cp_handle		cph;
    register Wst_handle		wst;
{
    register Cp_wst_list_entry		**node;
    Cp_wst_list_entry			*t;

    node = &cph->wst_list;
    while ( *node) {
	if ( (*node)->wst == wst) {
	    t = *node;
	    *node = (*node)->next;
	    free( (char *)t );
	    break;
	}
	node = &(*node)->next;
    }
}

void
phg_cp_destroy_wst_list( cph)
    register Cp_handle		cph;
{
    register Cp_wst_list_entry		*node, *next;

    node = cph->wst_list;
    while ( node) {
	next = node->next;
	phg_wst_destroy( node->wst );
	free( (char *)node );
	node = next;
    }
}
