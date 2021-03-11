/* $XConsortium: cb_ar.c,v 5.4 94/04/17 20:40:41 mor Exp $ */

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

/* Archive functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

static int
valid_ar_fname( fname )
    char	*fname;
{
    int		status = 0;

    if ( fname && strlen(fname) <= MAXNAMLEN)
	status = 1;

    return status;
}

void
popen_ar_file( archive_id, archive_file)
    Pint	archive_id;	/* archive identifier	*/
    char	*archive_file;	/* archive file name	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_open	*args = &cp_args.data.ar_open;
    Phg_ret			ret;
    char			*fname;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_open_ar_file)) {
	if ( phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR402);

	} else if ( !valid_ar_fname( archive_file )) {
	    /* need a better error code for this, use 190 for now */
	    ERR_REPORT( phg_cur_cph->erh, ERR400);

	} else if ( !phg_psl_ar_free_slot( phg_cur_cph->psl)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR401);

	} else if ( !(fname = malloc((unsigned)strlen(archive_file) + 1))) {
	    ERR_REPORT( phg_cur_cph->erh, ERR900);

	} else {
	    args->arid = archive_id;
	    args->fname = archive_file;
	    args->name_length = strlen(archive_file) + 1;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_OPEN, &cp_args, &ret);
	    if ( !ret.err) {
		strcpy( fname, archive_file);
		(void)phg_psl_add_ar( phg_cur_cph->psl, archive_id, fname);
		PSL_AR_STATE( phg_cur_cph->psl) = PST_AROP;
	    } else {
		free( fname);
	    }
	    ERR_FLUSH( phg_cur_cph->erh);
	}
    }
}

void
pclose_ar_file( archive_id)
    Pint	archive_id;	/* archive identifier	*/
{
    Phg_args			cp_args;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_close_ar_file)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    cp_args.data.idata = archive_id;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_CLOSE, &cp_args, NULL);
	    /* psl_rem_ar() sets the ar state to ARCL if no more ar's open */
	    phg_psl_rem_ar( phg_cur_cph->psl, archive_id);
	}
    }
}

void
par_structs( archive_id, struct_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint_list	*struct_ids;	/* list of structure identifiers	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ar_structs)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->data = *struct_ids;
	    args->op = PHG_ARGS_AR_STRUCTS;
	    args->resflag = PSL_ARCHIVE_CONFLICT( phg_cur_cph->psl);
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_ARCHIVE, &cp_args, NULL);
	}
    }
}

void
par_struct_nets( archive_id, struct_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint_list	*struct_ids;	/* list of structure identifiers	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ar_struct_nets)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->data = *struct_ids;
	    args->op = PHG_ARGS_AR_NETWORKS;
	    args->resflag = PSL_ARCHIVE_CONFLICT( phg_cur_cph->psl);
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_ARCHIVE, &cp_args, NULL);
	}
    }
}

void
par_all_structs( archive_id)
    Pint	archive_id;	/* archive identifier	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ar_all_structs)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->op = PHG_ARGS_AR_ALL;
	    args->resflag = PSL_ARCHIVE_CONFLICT( phg_cur_cph->psl);
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_ARCHIVE, &cp_args, NULL);
	}
    }
}

void
pset_conf_res( archival_resolution, retrieval_resolution)
    Pconf_res	archival_resolution; /*archival conflict resolution*/
    Pconf_res	retrieval_resolution; /*retieval conflict resolution*/
{
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_set_conf_res)) {
	PSL_ARCHIVE_CONFLICT( phg_cur_cph->psl) = archival_resolution;
	PSL_RETRIEVE_CONFLICT( phg_cur_cph->psl) = retrieval_resolution;
    }
}

void
pret_struct_ids( archive_id, max_ids, start, ids, actual_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint	max_ids;	/* size of appl. archive id list	*/
    Pint	start;		/* start position of ids	*/
    Pint_list	*ids;		/* OUT list of structure ids	*/
    Pint	*actual_ids;	/* OUT actual number of ids in PHIGS	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ret_struct_ids)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);

	} else {
	    cp_args.data.idata = archive_id;
	    ret.err = 0;
	    ids->num_ints = 0;
	    *actual_ids = 0;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_GET_NAMES, &cp_args, &ret);
	    if ( !ret.err) {
		ids->num_ints = 0;
		if ( (*actual_ids = ret.data.int_list.num_ints) > 0) {
		    if (start < 0 || start >= ret.data.int_list.num_ints) {
			ERR_REPORT( phg_cur_cph->erh, ERR2201);
		    } else if (max_ids > 0) {
			ids->num_ints = MIN( max_ids,
					    ret.data.int_list.num_ints - start);
			bcopy( (char*)&ret.data.int_list.ints[start],
				(char*)ids->ints,
				ids->num_ints * sizeof(Pint));
		    } else if (max_ids < 0) {
			ERR_REPORT( phg_cur_cph->erh, ERRN153);
		    }
		}
	    }
	    ERR_FLUSH( phg_cur_cph->erh);
	}
    }
}

void
pret_structs( archive_id, struct_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint_list	*struct_ids;	/* list of structure identifiers	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ret_structs)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->data = *struct_ids;
	    args->op = PHG_ARGS_AR_STRUCTS;
	    args->resflag = PSL_RETRIEVE_CONFLICT( phg_cur_cph->psl);
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_RETRIEVE, &cp_args, NULL);
	}
    }
}

void
pret_struct_nets( archive_id, struct_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint_list	*struct_ids;	/* list of structure identifiers	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ret_struct_nets)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->data = *struct_ids;
	    args->op = PHG_ARGS_AR_NETWORKS;
	    args->resflag = PSL_RETRIEVE_CONFLICT( phg_cur_cph->psl);
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_RETRIEVE, &cp_args, NULL);
	}
    }
}

void
pret_all_structs( archive_id)
    Pint	archive_id;	/* archive identifier	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ret_all_structs)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->op = PHG_ARGS_AR_ALL;
	    args->resflag = PSL_RETRIEVE_CONFLICT( phg_cur_cph->psl);
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_RETRIEVE, &cp_args, NULL);
	}
    }
}

void
pdel_structs_ar( archive_id, struct_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint_list	*struct_ids;	/* list of structure identifiers	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_del_structs_ar)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->data = *struct_ids;
	    args->op = PHG_ARGS_AR_STRUCTS;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_DELETE, &cp_args, NULL);
	}
    }
}

void
pdel_struct_nets_ar( archive_id, struct_ids)
    Pint	archive_id;	/* archive identifier	*/
    Pint_list	*struct_ids;	/* list of structure identifiers	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_del_struct_nets_ar)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->data = *struct_ids;
	    args->op = PHG_ARGS_AR_NETWORKS;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_DELETE, &cp_args, NULL);
	}
    }
}

void
pdel_all_structs_ar( archive_id)
    Pint	archive_id;	/* archive identifier	*/
{
    Phg_args			cp_args;
    register Phg_args_ar_info	*args = &cp_args.data.ar_info;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_del_all_structs_ar)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, archive_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);
	
	} else {
	    args->arid = archive_id;
	    args->op = PHG_ARGS_AR_ALL;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_DELETE, &cp_args, NULL);
	}
    }
}

/* Inquiry Functions */

void
pinq_ar_st( archive_state)
    Par_st	*archive_state;	/* OUT archive state	*/
{
    if ( CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY))
	 *archive_state = PSL_AR_STATE( phg_cur_cph->psl);
    else
	 *archive_state = PST_ARCL;
}

void
pinq_ar_files( store, error_ind, ar_files)
Pstore              store;          /* handle to Store object */
Pint		*error_ind;	/* OUT error indicator	*/
Par_file_list	**ar_files;     /* OUT list of archive file */
{
    register	Phg_state_list	*psl;
    register	int 		i, size;
    register	char		*name_buf;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = 2;

    } else {
	psl = phg_cur_cph->psl;
	*error_ind = 0;
	*ar_files = &((_Pstore *)store)->data.ar_files;
	(*ar_files)->num_ar_files = 0;
        if ( PSL_AR_STATE( phg_cur_cph->psl) == PST_AROP) {
	    for ( i = 0, size = 0; i < MAX_NO_OPEN_ARFILES; i++ ) {
		if ( psl->ar_files[i].used) {
		    (*ar_files)->num_ar_files++;
		    size += strlen(psl->ar_files[i].fname) + 1;
		}
	    }
	    size += (*ar_files)->num_ar_files * sizeof(Par_file);
	    if ( CB_STORE_SPACE( ((_Pstore *)store), size, error_ind ) ) {
		register int j = 0;
		(*ar_files)->ar_files = (Par_file *)((_Pstore *)store)->buf;
		name_buf = (char *)
		    ((*ar_files)->ar_files + (*ar_files)->num_ar_files);
		for ( i = 0; i < MAX_NO_OPEN_ARFILES; i++ ) {
		    if ( psl->ar_files[i].used ) {
			(*ar_files)->ar_files[j].id = psl->ar_files[i].arid;
			(*ar_files)->ar_files[j].name = name_buf;
			strcpy( name_buf, psl->ar_files[i].fname );
			name_buf += strlen( psl->ar_files[i].fname ) + 1;
			j++;
		    }
		}
	    }
	}
    }
}

void
pinq_conf_res( error_ind, archive, retrieval)
    Pint	*error_ind;	/* OUT error indicator	*/
    Pconf_res	*archive;	/* OUT archvival resolution	*/
    Pconf_res	*retrieval;	/* OUT retrieval resolution	*/
{
    if (CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = 0;
	*archive = PSL_ARCHIVE_CONFLICT( phg_cur_cph->psl);
	*retrieval = PSL_RETRIEVE_CONFLICT( phg_cur_cph->psl);
    } else
	*error_ind = 2;
}

void
pinq_all_conf_structs(ar_id, length, start, error_ind, ids, total_length)
Pint	ar_id;		/* archive identifier	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list	*ids;	/* OUT list of conflicting structure ids	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR7;

    } else if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	*error_ind = ERR7;

    } else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, ar_id)) {
	*error_ind = ERR404;

    } else {
	cp_args.data.q_conflicting.op = PHG_ARGS_CONF_ALL;
	cp_args.data.q_conflicting.arid = ar_id;
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_CONFLICTING, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    ids->num_ints = 0;
	    if ( (*total_length = ret.data.int_list.num_ints) > 0)
		if (start < 0 || start >= ret.data.int_list.num_ints)
		    *error_ind = ERR2201;
		else if (length > 0) {
		    ids->num_ints = MIN(length, ret.data.int_list.num_ints - start);
		    bcopy( (char*) &ret.data.int_list.ints[start],
			    (char*) ids->ints,
			    ids->num_ints * sizeof(Pint));
		} else if (length < 0)
		    *error_ind = ERRN153;
	}
    }
}

void
pinq_conf_structs_net(ar_id, struct_id, src, length, start, error_ind, ids, total_length)
Pint	ar_id;		/* archive identifier	*/
Pint	struct_id;	/* structure identifier	*/
Pstruct_net_source	src;	/* structure network source	*/
Pint	length;		/* length of application list	*/
Pint	start;		/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint_list	*ids;		/* OUT conflicting struct id list	*/
Pint	*total_length;	/* OUT length of list in PHIGS	*/
{
    Phg_args		cp_args;
    Phg_ret		ret;

    if ( !CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = ERR7;

    } else if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	*error_ind = ERR7;

    } else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, ar_id)) {
	*error_ind = ERR404;

    } else {
	cp_args.data.q_conflicting.op = PHG_ARGS_CONF_NET;
	cp_args.data.q_conflicting.arid = ar_id;
	cp_args.data.q_conflicting.struct_id = struct_id;
	cp_args.data.q_conflicting.src = src;
	ret.err = 0;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_INQ_CONFLICTING, &cp_args, &ret);
	if ( ret.err) {
	    *error_ind = ret.err;
	} else {
	    *error_ind = 0;
	    ids->num_ints = 0;
	    if ( (*total_length = ret.data.int_list.num_ints) > 0)
		if (start < 0 || start >= ret.data.int_list.num_ints)
		    *error_ind = ERR2201;
		else if (length > 0) {
		    ids->num_ints = MIN(length, ret.data.int_list.num_ints - start);
		    bcopy( (char*) &ret.data.int_list.ints[start],
			    (char*) ids->ints,
			    ids->num_ints * sizeof(Pint));
		} else if (length < 0)
		    *error_ind = ERRN153;
	}
    }
}

void
pret_paths_descs(ar_id, struct_id, po, pd, store, paths, status)
    Pint            		ar_id;		/* archive identifier */
    Pint	                struct_id;	/* structure identifier	*/
    Ppath_order     		po;     	/* path order */
    Pint            		pd;		/* path depth */
    Pstore              	store;          /* handle to Store object */
    Pelem_ref_list_list		**paths;	/* OUT path list */
    Pint			*status;	/* OUT status of retrieval */
{
    Phg_args		cp_args;
    Phg_ret		ret;

    *status = 1;	/* assume failure */
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ret_paths_descs)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( pd < 0 ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR207);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, ar_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);

	} else {
	    cp_args.data.q_ar_hierarchy.arid = ar_id;
	    cp_args.data.q_ar_hierarchy.hier.dir = PHG_ARGS_HIER_DESCENDANTS;
	    cp_args.data.q_ar_hierarchy.hier.struct_id = struct_id;
	    cp_args.data.q_ar_hierarchy.hier.order = po;
	    cp_args.data.q_ar_hierarchy.hier.depth = pd;
	    ret.err = 0;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_GET_HIERARCHY, &cp_args, &ret);
	    *paths = &((_Pstore *)store)->data.struct_paths;
	    (*paths)->num_elem_ref_lists = 0;
	    if ( !ret.err ) {
		phg_cb_copy_hierarchy( &ret.data.hierarchy, ((_Pstore *)store), status,
		    *paths );
	    }
	    ERR_FLUSH( phg_cur_cph->erh);
	}
    }
}

void
pret_paths_ances(ar_id, struct_id, po, pd, store, paths, status)
Pint             	ar_id;		/* archive identifier	*/
Pint            	struct_id;	/* structure identifier	*/
Ppath_order      	po;      	/* path order	*/
Pint            	pd;		/* path depth	*/
Pstore          	store;    	/* OUT store handle */
Pelem_ref_list_list	**paths;	/* OUT structure path list	*/
Pint                    *status;        /* OUT status of retrieval      */
{
    Phg_args		cp_args;
    Phg_ret		ret;

    *status = 1;	/* assume failure */
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR7, Pfn_ret_paths_ancest)) {
        if ( PSL_AR_STATE( phg_cur_cph->psl) != PST_AROP) {
	    ERR_REPORT( phg_cur_cph->erh, ERR7);

	} else if ( pd < 0 ) {
	    ERR_REPORT( phg_cur_cph->erh, ERR207);

	} else if ( !phg_psl_inq_ar_open( phg_cur_cph->psl, ar_id)) {
	    ERR_REPORT( phg_cur_cph->erh, ERR404);

	} else {
	    cp_args.data.q_ar_hierarchy.arid = ar_id;
	    cp_args.data.q_ar_hierarchy.hier.dir = PHG_ARGS_HIER_ANCESTORS;
	    cp_args.data.q_ar_hierarchy.hier.struct_id = struct_id;
	    cp_args.data.q_ar_hierarchy.hier.order = po;
	    cp_args.data.q_ar_hierarchy.hier.depth = pd;
	    ret.err = 0;
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_AR_GET_HIERARCHY, &cp_args, &ret);
	    *paths = &((_Pstore *)store)->data.struct_paths;
	    (*paths)->num_elem_ref_lists = 0;
	    if ( !ret.err ) {
		phg_cb_copy_hierarchy( &ret.data.hierarchy, ((_Pstore *)store), status,
		    *paths );
	    }
	    ERR_FLUSH( phg_cur_cph->erh);
	}
    }
}
