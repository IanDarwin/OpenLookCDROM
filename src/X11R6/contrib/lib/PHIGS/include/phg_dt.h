/* $XConsortium: phg_dt.h,v 5.2 94/04/17 20:41:47 rws Exp $ */

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

#ifndef PHG_DT_H_INCLUDED
#define PHG_DT_H_INCLUDED

#undef BITSPERBYTE

/* phigs description table */

/* NPC Limits */
#define PDT_NPC_XMIN	0.0
#define PDT_NPC_YMIN	0.0
#define PDT_NPC_ZMIN	0.0
#define PDT_NPC_XMAX	1.0
#define PDT_NPC_YMAX	1.0
#define PDT_NPC_ZMAX	1.0

/* Maximum number of simultaneously open workstations and archive files */
#define PDT_MAX_SIM_OPEN_WS		20
#define PDT_MAX_SIM_OPEN_ARFILES	25
/* No hard limit on these, so pick something high */
#define PDT_MAX_ISS_FILTERS		100

/* Number of available character sets */
#define PDT_NUM_CHAR_SETS		 1
/* List of CHAR_SETS would, in general, be present in PDT */
/* Character sets are listed using the PCS_* constants in phigs.h */

typedef struct {
    Pint		max_num_open_workstations;
    Pint		max_num_open_archives;
    Pint_list		char_sets;
    Pint		max_num_names_for_nameset;
    Pint		max_length_normal_iss_filter;
    Pint		max_length_inverted_iss_filter;
    Pgse_id_dep_list	gses;
    Pint		max_num_model_clip_volumes;
    Pint_list		model_clip_ops;
    /* list and number of workstation types are kept in the CP structure. */
} Phg_desc_tbl;

extern int	phg_pdt_init();
extern void	phg_pdt_clear();

#endif
