/* $XConsortium: util.h,v 5.2 94/04/17 20:41:56 rws Exp $ */

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

#ifndef PHG_UTIL_H_INCLUDED
#define PHG_UTIL_H_INCLUDED

/* Also see mat_utils.h for more utility functions. */
/* TODO: get rid of mat_utils.h, it's contents have already been moved to
 * here.
 */

extern char
    *phg_path(),
    *phg_grow_scratch();

extern int
    phg_tranpt3(),
    phg_tranpts3(),
    phg_mat3_equal();

extern float
    phg_vector_dot_prod(),
    phg_vector_length();

extern	void
    phg_mat_scale(),
    phg_mat_trans(),
    phg_mat_shear_z(),
    phg_mat_rot_x(),
    phg_mat_rot_y(),
    phg_mat_rot_z(),
    phg_mat_mul_pt(),
    phg_mat_copy(),
    phg_mat_mul(),
    phg_mat_mul_3x3(),
    phg_mat_identity (),
    phg_mat_inv(),	/* invert a 4 x 4 matrix */
    phg_vector_cross_prod(),
    phg_vector_normalize(),
    phg_mat_print(),
    phg_vector_print();

extern caddr_t
    phg_utx_interior_bndl_from_pex(),
    phg_utx_interior_bndl_to_pex(),
    phg_utx_line_bndl_from_pex(),
    phg_utx_line_bndl_to_pex(),
    phg_utx_marker_bndl_from_pex(),
    phg_utx_marker_bndl_to_pex(),
    phg_utx_text_bndl_from_pex(),
    phg_utx_text_bndl_to_pex(),
    phg_utx_edge_bndl_from_pex(),
    phg_utx_edge_bndl_to_pex(),
    phg_utx_pattern_entry_from_pex(),
    phg_utx_pattern_entry_to_pex(),
    phg_utx_dcue_entry_from_pex(),
    phg_utx_dcue_entry_to_pex(),
    phg_utx_light_entry_from_pex(),
    phg_utx_light_entry_to_pex(),
    phg_utx_view_entry_from_pex(),
    phg_utx_view_entry_to_pex(),
    phg_utx_colour_entry_from_pex(),
    phg_utx_colour_entry_to_pex(),
    phg_utx_colrmap_entry_from_pex(),
    phg_utx_colrmap_entry_to_pex(),
    phg_utx_ws_xform_from_pex();

extern int
    phg_utx_pex_supported();

/* Conversion utilities */
extern int		phg_utx_ptlst4_to_pex();
extern int		phg_utx_ptlst3_to_pex();
extern int		phg_utx_ptlst_to_pex();
extern int		phg_utx_ptlst4_from_pex();
extern int		phg_utx_ptlst3_from_pex();
extern int		phg_utx_ptlst_from_pex();
extern int		phg_utx_vdata_size();
extern int		phg_utx_vdata_to_pex();
extern int		phg_utx_fdata_to_pex();
extern Pelem_type	phg_utx_pex_eltype_to_phigs();
extern int		phg_utx_bitmask_to_intlst();
extern int		phg_utx_count_bits();
extern char*		phg_utx_vdata_from_pex();
extern char*		phg_utx_fdata_from_pex();
extern int		phg_utx_convert_colour();
extern int		phg_utx_map_update_state();
extern int		pgh_utx_compute_el_size();
extern void		phg_utx_el_data_from_pex();
extern int		phg_utx_build_pex_oc();
extern void		phg_register_timer_func();
extern void		phg_register_signal_func();
extern int		phg_utx_encode_text();

extern Display*		phg_utx_open_pex_display();

#define PHG_MAT_COPY(to, from) \
	bcopy((char *)(from), (char *)(to), sizeof(Pmatrix3)); 

/* Hash table utility stuff. */
typedef struct _Htab	*Hash_table;

extern Hash_table	phg_ut_htab_create();
extern void		phg_ut_htab_destroy();
extern int		phg_ut_htab_add_entry();
extern caddr_t		phg_ut_htab_delete_entry();
extern int		phg_ut_htab_get_entry();
extern void		phg_ut_htab_change_data();
extern int		phg_ut_htab_num_entries();
extern int		phg_ut_htab_first_entry();
extern int		phg_ut_htab_next_entry();

#endif
