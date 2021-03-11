/* $XConsortium: psl.h,v 5.2 94/04/17 20:41:55 rws Exp $ */

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

#ifndef PHG_PSL_H_INCLUDED
#define PHG_PSL_H_INCLUDED

/* phigs state list */

#ifndef MAXNAMLEN
#define MAXNAMLEN		255
#endif

#  define MAX_NO_OPEN_WS	PDT_MAX_SIM_OPEN_WS

#define MAX_NO_OPEN_ARFILES	PDT_MAX_SIM_OPEN_ARFILES

#define PSL_SYS_STATE( psl) \
    ((psl)->phg_sys_state)

#define PSL_WS_STATE( psl) \
    ((psl)->phg_ws_state)

#define PSL_STRUCT_STATE( psl) \
    ((psl)->phg_struct_state)

#define PSL_AR_STATE( psl) \
    ((psl)->phg_ar_state)

#define PSL_ARCHIVE_CONFLICT( psl) \
    ((psl)->ar_res)

#define PSL_RETRIEVE_CONFLICT( psl) \
    ((psl)->retr_res)

#define PSL_MORE_SIMULTEV( psl) \
    ((psl)->more_simultev)

#define PSL_SET_CUR_EVENT_ID( _psl, _id) \
    ((_psl)->cur_event.id = (_id))

#define PSL_SET_CUR_EVENT_DATA( _psl, _data) \
    ((_psl)->cur_event.data = (_data))

#define PSL_CUR_EVENT_CLASS( _psl) \
    ((_psl)->cur_event.id.class)

#define PSL_CUR_EVENT_DATA( _psl, _class) \
    ((_psl)->cur_event.data._class)

#define PSL_CLEAR_CUR_EVENT( _psl) \
    {   switch ((_psl)->cur_event.id.class) { \
	  case PIN_STROKE: \
	    if ( (_psl)->cur_event.data.stk.num_points > 0) \
		free((char*)((_psl)->cur_event.data.stk.points)); \
	    break; \
	  case PIN_PICK: \
	    if ( (_psl)->cur_event.data.pik.status == PIN_STATUS_OK \
		&& (_psl)->cur_event.data.pik.pick_path.depth > 0) \
		free((char*)((_psl)->cur_event.data.pik.pick_path.path_list)); \
	    break; \
	  case PIN_STRING: \
	    if ( (_psl)->cur_event.data.str.length > 0) \
		free((char*)((_psl)->cur_event.data.str.string)); \
	    break; \
	} \
	(_psl)->cur_event.id.class = PIN_NONE; \
    }

typedef struct {
    Pint	used;
    Pint	wsid;
    char	*connid;
    Wst		*wstype;
    int		lun;	/* logical unit number for FORTRAN (only). */
} Psl_ws_info;

typedef struct {
    Pint	used;
    char	*fname;
    Pint	arid;
    int		lun;	/* logical unit number for FORTRAN (only). */
} Psl_ar_info;

typedef struct {
    Pevent		id;
    Phg_inp_event_data	data;
} Psl_inp_event;

typedef struct _Phg_state_list {
    Psys_st	phg_sys_state;		/* system state */
    Pws_st		phg_ws_state;		/* workstation state */
    Pstruct_st	phg_struct_state;	/* structure state */
    Par_st	phg_ar_state;		/* archive state */
    Psl_ws_info		open_ws[MAX_NO_OPEN_WS]; /* open workstations */
    Psl_ar_info		ar_files[MAX_NO_OPEN_ARFILES]; /* open archives */ 
    Pconf_res		ar_res;		/* archival conflict resolution */
    Pconf_res		retr_res;	/* retrieval conflict resolution */
    Pmore_simult_events	more_simultev;	/* more simultaneous events */
    Psl_inp_event	cur_event;
    Pedit_mode		edit_mode;
    Pint		open_struct;
} Phg_state_list;

typedef Phg_state_list	*Psl_handle;

extern Psl_handle	phg_psl_init();
extern void		phg_psl_destroy();
extern int		phg_psl_ws_free_slot();
extern int		phg_psl_add_ws();
extern void		phg_psl_rem_ws();
extern int		phg_psl_inq_ws_open();
extern int		phg_psl_inq_wsids();
extern Psl_ws_info*	phg_psl_get_ws_info();

extern Psl_ar_info*	phg_psl_get_ar_info();
extern int		phg_psl_ar_free_slot();
extern int		phg_psl_add_ar();
extern void		phg_psl_rem_ar();
extern int		phg_psl_inq_ar_open();

#endif
