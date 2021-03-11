/* $XConsortium: cpx_css.c,v 5.2 94/04/17 20:41:26 rws Exp $ */

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

/* CPX CSS functions used in both client and monitor processes. */

#include "phg.h"
#include "cp.h"
#include "cp_priv.h"
#include "PEX.h"
#include "PEXmacs.h"
#include "PEXfuncs.h"
#include "PEXproto.h"
#include "phigspex.h"


void
phg_cpx_inq_el_type_size( cph, cp_args, ret )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Phg_ret			*ret;
{
    Cpx_css_srvr	*css_srvr;

    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inq_el_type_size)( cph, cp_args, ret, css_srvr );

}


void
phg_cpx_inq_el_content( cph, cp_args, ret )
    register Cp_handle		cph;
    Phg_args			*cp_args;
    Phg_ret			*ret;
{
    Cpx_css_srvr	*css_srvr;

    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inq_el_content)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_inq_el_ptr( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* should be NULL */
    Phg_ret		*ret;
{
    Cpx_css_srvr		*css_srvr;

    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inq_el_ptr)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_inq_struct_status( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Cpx_css_srvr		*css_srvr;

    ret->data.idata = (int)PSTRUCT_STATUS_NON_EXISTENT;
    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inq_struct_status)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_inq_struct_ids( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;	/* should be NULL */
    Phg_ret		*ret;
{
    Cpx_css_srvr	*css_srvr;

    ret->err = 0;
    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inq_struct_ids)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_el_search( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Cpx_css_srvr	*css_srvr;

    ret->err = 0;
    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->el_search)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_inq_hierarchy( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Cpx_css_srvr	*css_srvr;

    ret->err = 0;
    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inq_hierarchy)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_inc_spa_search( cph, cp_args, ret )
    Cp_handle		cph;
    Phg_args		*cp_args;
    Phg_ret		*ret;
{
    Cpx_css_srvr	*css_srvr;

    ret->err = 0;
    CPX_MASTER_SERVER(cph,css_srvr)
    (*css_srvr->inc_spa_search)( cph, cp_args, ret, css_srvr );
}


void
phg_cpx_change_struct_refs( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;

    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->change_struct_refs)( cph, cp_args, css_srvr );
}


void
phg_cpx_change_struct_idrefs( cph, cp_args )
    Cp_handle		cph;
    Phg_args		*cp_args;
{
    Cpx_css_srvr	*css_srvr;
    Phg_args		d_cp_args;
    Phg_ret		ret;

    CPX_FOR_ALL_SERVERS(cph,css_srvr)
	(*css_srvr->change_struct_idrefs)( cph, cp_args, css_srvr );
}

