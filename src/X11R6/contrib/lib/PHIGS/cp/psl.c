/* $XConsortium: psl.c,v 5.2 94/04/17 20:41:30 rws Exp $ */

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

/* Initialise phigs state list */

#include "phg.h"

Psl_handle
phg_psl_init()
{
    register int i;
    register Psl_handle psl;
   
    if ( !(psl = (Psl_handle) malloc(sizeof(Phg_state_list))) )
        return(NULL);						/* RETURN */
    psl->phg_sys_state = PSYS_ST_PHCL;
    psl->phg_ws_state = PWS_ST_WSCL;
    psl->phg_struct_state = PSTRUCT_ST_STCL;
    psl->phg_ar_state = PST_ARCL;
    for ( i = 0; i < MAX_NO_OPEN_WS; i++)
	psl->open_ws[i].used = 0;
    for ( i = 0; i < MAX_NO_OPEN_ARFILES; i++) {
        psl->ar_files[i].used = 0;
    }
    psl->ar_res = PRES_UPD;
    psl->retr_res = PRES_ABANDON;
    psl->more_simultev = PSIMULT_NO_MORE;
    psl->edit_mode = PEDIT_INSERT;
    return(psl);   
}

void
phg_psl_destroy( psl )
    Psl_handle	psl;
{
    if ( psl ) {
	PSL_CLEAR_CUR_EVENT(psl)
	free( (char *)psl);
    }
}

int
phg_psl_inq_ws_open( psl, wsid)
    register Psl_handle		psl;
    register Pint		wsid;
    /* return 1 if wsid is in psl's open workstation list, else 0 */
{
    register int	i;
    int			status = 0;

    for ( i = 0; i < MAX_NO_OPEN_WS; i++) {
	if ( psl->open_ws[i].used && psl->open_ws[i].wsid == wsid) {
	    status = 1;
	    break;
	}
    }
    return status;
}

int
phg_psl_inq_wsids( psl, wsids)
    register Psl_handle		psl;
    register Pint		*wsids;
{
    register int	i;
    int			cnt = 0;

    for ( i = 0; i < MAX_NO_OPEN_WS; i++) {
	if ( psl->open_ws[i].used) {
	    *wsids++ = psl->open_ws[i].wsid;
	    ++cnt;
	}
    }
    return cnt;
}

Psl_ws_info*
phg_psl_get_ws_info( psl, wsid)
    register Psl_handle		psl;
    register Pint		wsid;
{
    register int	i;
    Psl_ws_info		*ws = NULL;

    for ( i = 0; i < MAX_NO_OPEN_WS; i++) {
	if ( psl->open_ws[i].used && psl->open_ws[i].wsid == wsid) {
	    ws = &psl->open_ws[i];
	    break;
	}
    }
    return ws;
}

int
phg_psl_ws_free_slot( psl)
    register Psl_handle		psl;
    /* return 1 if free slot exists, else 0 */
{
    register int 	i;
    int			status = 0;

    for ( i = 0; i < MAX_NO_OPEN_WS; i++) {
	if ( !psl->open_ws[i].used) {
	    status = 1;
	    break;
	}
    }
    return status;
}

int
phg_psl_add_ws( psl, wsid, connid, type) /* add ws to list of those open */
    register Psl_handle		psl;
    Pint			wsid;
    char			*connid;
    Wst				*type;
    /* return 1 if wsid could be added, else 0 */
{
    register int 	i;
    int			status = 0;

    for ( i = 0; i < MAX_NO_OPEN_WS; i++) {
	if ( !psl->open_ws[i].used) {
	    psl->open_ws[i].used = 1;
	    psl->open_ws[i].wsid = wsid;
	    psl->open_ws[i].connid = connid;
	    psl->open_ws[i].wstype = type;
	    status = 1;
	    break;
	}
    }
    return status;
}

void
phg_psl_rem_ws( psl, wsid)	/* remove the wsid from the open ws list */
    register Psl_handle		psl;
    register Pint		wsid;
{
    register int 	i,
			cnt;	/* number of open workstations left */

    for ( i = 0, cnt = 0; i < MAX_NO_OPEN_WS; i++) {
	if ( psl->open_ws[i].used) {
	    if ( psl->open_ws[i].wsid == wsid)
		psl->open_ws[i].used = 0;
	    else
		++cnt;
	}
    }

    /* set the ws state if no more workstations open */
    if (!cnt)
	PSL_WS_STATE( psl) = PWS_ST_WSCL;
}

/* Archive Functions */

int
phg_psl_inq_ar_open( psl, arid)
    register Psl_handle		psl;
    register Pint		arid;
    /* return 1 if arid is in psl's open archive list, else 0 */
{
    register int	i;
    int			status = 0;

    if ( psl ) {
	for ( i = 0; i < MAX_NO_OPEN_ARFILES; i++) {
	    if ( psl->ar_files[i].used && psl->ar_files[i].arid == arid) {
		status = 1;
		break;
	    }
	}
    }
    return status;
}

Psl_ar_info*
phg_psl_get_ar_info( psl, arid)
    register Psl_handle		psl;
    register Pint		arid;
{
    register int	i;
    Psl_ar_info		*ar = NULL;

    if ( psl ) {
	for ( i = 0; i < MAX_NO_OPEN_ARFILES; i++) {
	    if ( psl->ar_files[i].used && psl->ar_files[i].arid == arid) {
		ar = &psl->ar_files[i];
		break;
	    }
	}
    }
    return ar;
}

int
phg_psl_ar_free_slot( psl)
    register Psl_handle		psl;
    /* return 1 if free slot exists, else 0 */
{
    register int 	i;
    int			status = 0;

    for ( i = 0; i < MAX_NO_OPEN_ARFILES; i++) {
	if ( !psl->ar_files[i].used) {
	    status = 1;
	    break;
	}
    }
    return status;
}

int
phg_psl_add_ar( psl, arid, fname) /* add ar to list of those open */
    register Psl_handle		psl;
    Pint			arid;
    char			*fname;
    /* return 1 if arid could be added, else 0 */
{
    register int 	i;
    int			status = 0;

    for ( i = 0; i < MAX_NO_OPEN_ARFILES; i++) {
	if ( !psl->ar_files[i].used) {
	    psl->ar_files[i].used = 1;
	    psl->ar_files[i].arid = arid;
	    psl->ar_files[i].fname = fname;
	    status = 1;
	    break;
	}
    }
    return status;
}

void
phg_psl_rem_ar( psl, arid)	/* remove the arid from the open ar list */
    register Psl_handle		psl;
    register Pint		arid;
{
    register int 	i,
			cnt;	/* number of open archives left */

    for ( i = 0, cnt = 0; i < MAX_NO_OPEN_ARFILES; i++) {
	if ( psl->ar_files[i].used) {
	    if ( psl->ar_files[i].arid == arid) {
		psl->ar_files[i].used = 0;
		free( (char *)psl->ar_files[i].fname);
	    } else {
		++cnt;
	    }
	}
    }

    /* set the ar state if no more archives open */
    if (!cnt)
	PSL_AR_STATE( psl) = PST_ARCL;
}
