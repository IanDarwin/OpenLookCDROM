/* $XConsortium: cp_ops.h,v 5.2 94/04/17 20:41:39 rws Exp $ */

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

#ifndef PHG_CP_OPS_H_INCLUDED
#define PHG_CP_OPS_H_INCLUDED

typedef enum {
    CPX_SRVR_SS,	/* model A */
    CPX_CLNT_SS		/* model B */
} Cpx_css_srvr_type;

typedef enum {
    CP_FUNC_OP_NONE = 0,
    CP_FUNC_OP_OPEN_WS,
    CP_FUNC_OP_CLOSE_WS,
    CP_FUNC_OP_WS_REDRAW_ALL,
    CP_FUNC_OP_WS_UPDATE,
    CP_FUNC_OP_SET_DISP_STATE,
    CP_FUNC_OP_MESSAGE,
    CP_FUNC_OP_SET_REP,
    CP_FUNC_OP_SET_FILTER,
    CP_FUNC_OP_SET_COLOUR_MODEL,
    CP_FUNC_OP_SET_HLHSR_MODE,
    CP_FUNC_OP_SET_VIEW_INPUT_PRIO,
    CP_FUNC_OP_SET_WS_WIN,
    CP_FUNC_OP_SET_WS_VP,
    CP_FUNC_OP_ADD_EL,
    CP_FUNC_OP_COPY_ALL_ELS,
    CP_FUNC_OP_OPEN_STRUCT,
    CP_FUNC_OP_CLOSE_STRUCT,
    CP_FUNC_OP_SET_EL_PTR,
    CP_FUNC_OP_SET_EDIT_MODE,
    CP_FUNC_OP_DELETE_EL,
    CP_FUNC_OP_DELETE_STRUCT,
    CP_FUNC_OP_DELETE_STRUCT_NET,
    CP_FUNC_OP_DELETE_ALL_STRUCTS,
    CP_FUNC_OP_CHANGE_STRUCT_ID,
    CP_FUNC_OP_CHANGE_STRUCT_REFS,
    CP_FUNC_OP_CHANGE_STRUCT_IDREFS,
    CP_FUNC_OP_POST_STRUCT,
    CP_FUNC_OP_UNPOST_STRUCT,
    CP_FUNC_OP_UNPOST_ALL,
    CP_FUNC_OP_AR_OPEN,
    CP_FUNC_OP_AR_CLOSE,
    CP_FUNC_OP_AR_ARCHIVE,
    CP_FUNC_OP_AR_RETRIEVE,
    CP_FUNC_OP_AR_DELETE,
    CP_FUNC_OP_AR_GET_NAMES,
    CP_FUNC_OP_AR_GET_HIERARCHY,	/* ancestors and descendents */
    CP_FUNC_OP_INP_INIT_DEV,
    CP_FUNC_OP_INP_SET_MODE,
    CP_FUNC_OP_INP_REQUEST,
    CP_FUNC_OP_INP_SAMPLE,
    CP_FUNC_OP_INP_AWAIT,
    CP_FUNC_OP_FLUSH_DEV,
    CP_FUNC_OP_SET_ERR_HAND_MODE,
    CP_FUNC_OP_EL_SEARCH,
    CP_FUNC_OP_INC_SPA_SEARCH,

    /* inquiry functions */
    CP_FUNC_OP_INQ_STRUCT_STATUS,
    CP_FUNC_OP_INQ_STRUCT_IDS,
    CP_FUNC_OP_INQ_EL_PTR,
    CP_FUNC_OP_INQ_EL_TYPE_SIZE,
    CP_FUNC_OP_INQ_EL_CONTENT,
    CP_FUNC_OP_INQ_CONFLICTING,
    CP_FUNC_OP_INQ_INDICES,
    CP_FUNC_OP_INQ_FILTER,
    CP_FUNC_OP_INQ_POSTED,
    CP_FUNC_OP_INQ_INP_DEV_STATE,
    CP_FUNC_OP_INQ_WSS_POSTED_TO,
    CP_FUNC_OP_INQ_HIERARCHY,
    CP_FUNC_OP_INQ_REPRESENTATION,
    CP_FUNC_OP_INQ_VIEW_REP,
    CP_FUNC_OP_INQ_HLHSR_MODE,
    CP_FUNC_OP_INQ_DISP_UPDATE_STATE,
    CP_FUNC_OP_INQ_COLOUR_MODEL,
    CP_FUNC_OP_INQ_WS_XFORM,
    CP_FUNC_OP_INQ_INP_OVERFLOW,
    CP_FUNC_OP_INQ_TEXT_EXTENT,
    CP_FUNC_OP_INQ_MORE_EVENTS,

    CP_FUNC_OP_EMERG_CLOSE,
    CP_FUNC_OP_CLOSE_PHIGS,

    /* Escapes */
    CP_FUNC_OP_ERROR_SYNC,
    CP_FUNC_OP_XX_INQ_DPY_AND_DRAWABLE,
    CP_FUNC_OP_DRAWABLE_PICK,
    CP_FUNC_OP_MAP_POINTS,
    CP_FUNC_OP_REDRAW_REGIONS,
    CP_FUNC_OP_WS_SYNCH,

    CP_FUNC_OP_INQ_WIN_INFO,
    CP_FUNC_OP_INQ_COLR_MAP_METH_ST,

    NUM_CP_FUNCS	/* need this to get the number of funcs */
} Cp_func_op;

#endif
