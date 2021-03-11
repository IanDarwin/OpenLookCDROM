/* $XConsortium: cpx_ar.c,v 5.9 94/04/17 20:41:23 hersh Exp $ */

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

#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "cp_priv.h"
#include "ar.h"
#include "PEXprotost.h"
#include "PEXproto.h"
#include "PEX.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifdef X_NOT_POSIX
off_t lseek();
#endif

#ifndef L_XTND
#define  L_XTND      2  /* relative to end of file */
#endif

static int	get_ar_structure_network_ids();
static int	get_css_struct_ids();
static int	get_css_network_sids();
static int	compile_network_sids();

#define GET_ARH(cph, id, arh) \
    for (arh = cph->ar_list; arh && arh->arid != id; arh = arh->next);


/* Perform a binary search on a sorted list of ints.  Return 1 if found,
 * 0 if not found */
static int
search_integer_list(target, intlist, num)
register int	target;
int	       *intlist;
int		num;
{
    register int *base = intlist;
    register int *last = &intlist[num-1];
    
    while (last >= base) {
    
	register int *ptr = base + ((last - base) / 2);
	
	if (target == *ptr)
	    return 1;
	else if (target < *ptr)
	    last = ptr - 1;
	else
	    base = ptr + 1;
	    
    }
    
    return 0;
	
}
		
#if __STDC__
static int
intcompare(a, b)
const void *a, *b;
{
    return(*((int *)a) - *((int *)b));
}
#else
static int
intcompare(a, b)
int *a, *b;
{
    return(*a - *b);
}
#endif

void
phg_cpx_ar_open( cph, cp_args, ret )
Cp_handle cph;
Phg_args *cp_args;
Phg_ret *ret;
{
    int		      err;
    Ar_handle	      arh;
    Phg_args_ar_open *args = &(cp_args->data.ar_open);
    struct stat	      finfo;
    FILE	     *fp = NULL;
    
    ret->err = !0;
    
    if ( (err = stat(args->fname, &finfo)) && errno != ENOENT) {
	ERR_BUF(cph->erh, ERR400);		    /* can't open file */
    } else if (!err && !(fp = fopen(args->fname, "r"))) {
	ERR_BUF(cph->erh, ERR400);		    /* can't open file */
    } else if (!(arh = (Ar_handle)calloc((unsigned)1,sizeof(Ar_struct)))) {
	ERR_BUF(cph->erh, ERR900);		    /* out of memory */
    } else {
	/* fill in archive structure and put on beginning of ar_list */
	arh->arid = args->arid;
	*arh->fname = '\0';
	strncat(arh->fname, args->fname, MAXNAMLEN);
	arh->toc = NULL;
	arh->next = cph->ar_list;
	cph->ar_list = arh;
	fclose(fp);
	if (!err && finfo.st_size) {
	    /* file exists and is not empty, so read it */
	    if ((arh->fd = open(arh->fname, O_RDWR)) == -1)
		arh->fd = open(arh->fname, O_RDONLY);
	    if (    phg_ar_read_baf(arh) ||
		    phg_ar_read_afd(arh) ||
		    phg_ar_read_toc(arh) ||
		    phg_ar_read_eoa(arh)) {
		ERR_BUF(cph->erh, ERR403);	    /* bad archive file */
	    } else {
		ret->err = 0;
	    }
	} else {
	    /* File couldn't be opened, attempt to create new archive */
	    if ((arh->fd = open(arh->fname, O_RDWR | O_CREAT, 0644)) == -1) {
		ERR_BUF(cph->erh, ERR400);
	    } else {
		arh->format = PHG_AR_HOST_BYTE_ORDER | 
			      PHG_AR_HOST_FLOAT_FORMAT;
		arh->toc = NULL;
		if (	(!phg_ar_init_toc(arh)) ||
			(phg_ar_write_baf(arh)) ||
			(phg_ar_write_afd(arh))) {
		    ERR_BUF(cph->erh, ERR400);
		} else {
		    arh->afiOffset = (CARD32)lseek(arh->fd, 0L, L_XTND);
		    if (phg_ar_write_toc(arh)) {
			ERR_BUF(cph->erh, ERR400);
		    } else {
			ret->err = 0;
		    }
		}
	    }
	}
    }
}

void
phg_cpx_ar_close( cph, cp_args )
Cp_handle cph;
Phg_args *cp_args;
{
    Pint	ar_id = cp_args->data.idata;
    Ar_handle	arh, arp, tmp_arp = NULL;

    GET_ARH(cph, ar_id, arh);
    if ( (phg_ar_write_toc(arh)) ||
	 (phg_ar_write_eoa(arh->fd))) {
	ERR_BUF(cph->erh, ERR406);  /* archive file is full */
    }

    close(arh->fd);
    phg_ar_free_toc(arh);
    
    for (arp = cph->ar_list; arp; arp = arp->next) {
	if (arp == arh) {
	    if (tmp_arp) 
		tmp_arp = arp->next;
	    else
		cph->ar_list = arh->next;
	    break;
	}
	tmp_arp = arp;
    }
    free((char *)arh);
}

void
phg_cpx_ar_archive( cph, cp_args )
Cp_handle cph;
Phg_args *cp_args;
{
    Phg_args_ar_info	*args = &(cp_args->data.ar_info);
    Pint_list		 sidlist, arids;
    Ar_handle		 arh;
    Phg_ar_index_entry	*entry;
    int			 i;
    Cpx_css_srvr	*css_srvr;

    GET_ARH(cph, args->arid, arh);
    
    /** Get sids of all the structures we want to archive **/
    switch (args->op) {
    
	case PHG_ARGS_AR_STRUCTS :
	    sidlist.num_ints = args->data.num_ints;
	    sidlist.ints = args->data.ints;
	    break;
	    
	case PHG_ARGS_AR_NETWORKS :
	    if (compile_network_sids(cph, (Ar_handle)NULL, &(args->data),
					   PNET_CSS, &(args->data))) {
		return;			       
	    }
	    break;
	    
	case PHG_ARGS_AR_ALL :
	    if (get_css_struct_ids(cph, &(args->data))) {
		return;
	    }
	    break;
	    
	default :
	    return;
	    break;
    }
    
    /** Now all of the css ids we're going to archive are in args->data **/
    
    /** Now put all of the archive ids into a list **/
    arids.num_ints = 0;
    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
	arids.num_ints++;
    PHG_AR_END_FOR_ALL_TOC_ENTRIES
    
    if ( arids.num_ints > 0 && !(arids.ints = (Pint *)
	    malloc((unsigned)(arids.num_ints * sizeof(Pint))))) {
	ERR_BUF(cph->erh, ERR900);
	if (args->op != PHG_ARGS_AR_STRUCTS)
	    free((char *)(args->data.ints));
	return;
    }
    
    i = 0;
    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
	arids.ints[i++] = entry->str;
    PHG_AR_END_FOR_ALL_TOC_ENTRIES
    
    if (args->resflag == PRES_ABANDON && arids.num_ints > 0) {
	for (i = 0; i < args->data.num_ints; i++) {
	    if (search_integer_list(args->data.ints[i],
				    arids.ints, arids.num_ints)) {
		ERR_BUF(cph->erh, ERR405);	    
		return;
	    }
	}
    }
    
    CPX_MASTER_SERVER(cph, css_srvr);
    (*css_srvr->ar_archive)(cph, arh, args, css_srvr);
}


void
phg_cpx_ar_retrieve( cph, cp_args)
Cp_handle cph;
Phg_args *cp_args;
{

    Phg_args_ar_info	*args = &(cp_args->data.ar_info);
    Pint_list		 ar_structs, css_ids;
    Ar_handle		 arh;
    register int	 i, eln;
    Phg_args		 args2, el_args;
    Phg_ret		 ret2;
    Phg_ar_index_entry	*entry;
    caddr_t		 buffer;
    Cpx_css_srvr	*css_srvr;
    pexElementInfo	*pex_el;
    Pedit_mode           cur_edit_mode;
    Pint                 cur_open_struct, cur_elem_ptr, cur_struct_state;

    GET_ARH(cph, args->arid, arh);
    
    switch (args->op) {
    
	case PHG_ARGS_AR_STRUCTS :
	    ar_structs.num_ints = args->data.num_ints;
	    ar_structs.ints = args->data.ints;
	    break;
	    
	case PHG_ARGS_AR_NETWORKS :
	    if (compile_network_sids(cph, arh, &(args->data), PNET_AR,
					   &(args->data)))
		return;
	    ar_structs.num_ints = args->data.num_ints;
	    ar_structs.ints = args->data.ints;
	    break;
	    
	case PHG_ARGS_AR_ALL :
	    i = 0;
	    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
		i++;
	    PHG_AR_END_FOR_ALL_TOC_ENTRIES
	    
	    ar_structs.num_ints = i;
	    if (!(ar_structs.ints = (Pint *)malloc((unsigned)(i * sizeof(Pint))))) {
		ERR_BUF(cph->erh, ERR900);
		return;
	    }
	    
	    i = 0;
	    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
		ar_structs.ints[i++] = entry->str;
	    PHG_AR_END_FOR_ALL_TOC_ENTRIES

	    break;
	    
	default :
	    return;
	    break;
    }
    
    /* Now we know which structures to retrieve */
    
    if (get_css_struct_ids(cph, &css_ids)) {
	free((char *)ar_structs.ints);
        return;
    }
    
    /* if resolution flag is abandon, and there are conflicts, give up */
    if (args->resflag == PRES_ABANDON && css_ids.num_ints > 0) {
	for (i = 0; i < ar_structs.num_ints; i++) {
	    if (search_integer_list(ar_structs.ints[i],
				    css_ids.ints, css_ids.num_ints)) {
		ERR_BUF(cph->erh, ERR405);
		free((char *)css_ids.ints);
		if (args->op != PHG_ARGS_AR_STRUCTS)
		    free((char *)ar_structs.ints);
		return;	    
	    }
	}
    }
    
    for (i = 0; i < ar_structs.num_ints; i++) {
	if ( !(entry = phg_ar_get_entry_from_archive(arh,
		ar_structs.ints[i])) ) {
	   ERR_BUF(cph->erh, ERR408);
           /* Structure not in archive, create an empty one in CSS if */
           /* it isn't already there */
           if (args->resflag != PRES_MAINTAIN || !(css_ids.num_ints > 0 &&
               search_integer_list(ar_structs.ints[i],
                                   css_ids.ints, css_ids.num_ints))) {
               args2.data.del_el.op = PHG_ARGS_EMPTY_STRUCT;
               args2.data.del_el.data.struct_id = ar_structs.ints[i];
               CP_FUNC(cph, CP_FUNC_OP_DELETE_EL, &args2, NULL);
           }
           continue;
	} else if (!(buffer = malloc((unsigned)entry->length))) {
	    ERR_BUF(cph->erh, ERR900);
	    free((char *)css_ids.ints);
	    if (args->op != PHG_ARGS_AR_STRUCTS)
		free((char *)ar_structs.ints);
	    return;
	}
	
	if (phg_ar_read_struct_from_archive(arh, entry, buffer)) {
	    ERR_BUF(cph->erh, ERR403);	/* bad archive file */
	    free((char *)css_ids.ints);
	    if (args->op != PHG_ARGS_AR_STRUCTS)
		free((char *)ar_structs.ints);
	    free((char *)buffer);
	    return;
	}
	
	args2.data.idata = ar_structs.ints[i];
	if (args->resflag != PRES_ABANDON && css_ids.num_ints > 0 &&
		search_integer_list(ar_structs.ints[i],
				    css_ids.ints, css_ids.num_ints)) {

	    if (args->resflag == PRES_MAINTAIN)
		continue;
	    else {
		/* remove structure from CSS */
		args2.data.del_el.op = PHG_ARGS_EMPTY_STRUCT;
		args2.data.del_el.data.struct_id = ar_structs.ints[i];
		CP_FUNC(cph, CP_FUNC_OP_DELETE_EL, &args2, NULL);
		args2.data.idata = ar_structs.ints[i];
	    }
	}

        /* To do this right the id of the current open structure must */
        /* be retrieved along with the element pointer and edit mode. */
        /* The edit mode needs to be set to insert and everything */
        /* restored after the close. Also after the close, if the */
        /* structure being opened is the one just retrieved, the */
        /* element pointer should not be reset. */
        cur_struct_state = PSL_STRUCT_STATE( cph->psl);
        if (cur_struct_state == PSTRUCT_ST_STOP)
        {
            cur_open_struct = cph->psl->open_struct;
            CP_FUNC(cph, CP_FUNC_OP_INQ_EL_PTR, &args2, &ret2);
            cur_elem_ptr = ret2.data.idata;
        }
        cur_edit_mode = cph->psl->edit_mode;
        cph->psl->edit_mode = PEDIT_INSERT;

	/* Create new structure and add the elements. */
	CP_FUNC(cph, CP_FUNC_OP_OPEN_STRUCT, &args2, &ret2);
	if (ret2.err) {
	    free((char *)css_ids.ints);
	    if (args->op != PHG_ARGS_AR_STRUCTS)
		free((char *)ar_structs.ints);
	    free((char *)buffer);
	    return;
	}

	pex_el = (pexElementInfo *)buffer;
	for (eln = 0; eln < entry->nelts; eln++) {
	    el_args.data.add_el.el_type =
		phg_utx_pex_eltype_to_phigs(pex_el->elementType);
	    el_args.data.add_el.pex_oc.oc = pex_el;
	    el_args.data.add_el.pex_oc.size = pex_el->length * sizeof(CARD32);
	    CP_FUNC(cph, CP_FUNC_OP_ADD_EL, &el_args, (Phg_ret *)NULL);
	    pex_el += pex_el->length;
	}

	/* close the structure */
	CP_FUNC(cph, CP_FUNC_OP_CLOSE_STRUCT, &args2, (Phg_ret *)NULL);

        /* Restore things */
        cph->psl->edit_mode = cur_edit_mode;
        if (cur_struct_state == PSTRUCT_ST_STOP)
        {
            args2.data.idata = cur_open_struct;
            CP_FUNC(cph, CP_FUNC_OP_OPEN_STRUCT, &args2, &ret2);
            if (cur_open_struct != ar_structs.ints[i])
            {
                args2.data.set_el_ptr.op = PHG_ARGS_SETEP_ABS;
                args2.data.set_el_ptr.data = cur_elem_ptr;
                CP_FUNC( cph, CP_FUNC_OP_SET_EL_PTR, &args2, &ret2);
            }
        }

	free((char *)buffer);
    }
    
    free((char *)css_ids.ints);
    if (args->op != PHG_ARGS_AR_STRUCTS)
	free((char *)ar_structs.ints);
}

/** Merge nl sorted lists, removing duplicates **/
static void
merge_and_remove_duplicates(nl, lsts, result)
int		 nl;			/* number of lists */
register Pint_list lsts[];		/* array of intlsts */
Pint_list		*result;		/* pointer to resulting list */
{
    register int i, j;
    int	     done = 0;
    int	    *inds = (int *)malloc((unsigned)(nl * sizeof(int)));

    for (i = 0; i < nl; i++)
	inds[i] = 0;
	
    i = 0;
    
    while (!done) {
    
	int min_lst = -1;
	Pint min_num = 9999999;
	
	/* find smallest number in first elements of all lists */
	for (j = 0; j < nl; j++) {
	    if (inds[j] < lsts[j].num_ints && 
		    ((lsts[j].ints[inds[j]] < min_num) ||
		    (min_lst == -1))) {
		min_num = lsts[j].ints[inds[j]];
		min_lst = j;
	    }
	}
	
	/* put onto main list */
	if (min_lst == -1) {
	    done = 1;
	} else {
	    if (i == 0 || min_num != result->ints[i - 1]) {
		/* not a duplicate */
		result->ints[i++] = min_num;
	    }
	    inds[min_lst]++;
	}
    }
    result->num_ints = i;
    free((char *)inds);
}

void
phg_cpx_ar_delete( cph, cp_args )
Cp_handle cph;
Phg_args *cp_args;
{
    Ar_handle		 arh;
    Pint		 id;
    Phg_args_ar_info	*args = &(cp_args->data.ar_info);
    Phg_args		 op_and_cl_args;
    Phg_ret		 dummy_ret;
    char		*arname;
    int			 already_warned = 0;
    int			 i, j;
    Phg_ar_index_entry	*entry;

    GET_ARH(cph, args->arid, arh);
    
    switch (args->op) {
    
	case PHG_ARGS_AR_STRUCTS :
	    for (i = 0; i < args->data.num_ints; i++) {
		if (!(entry = phg_ar_get_entry_from_archive(arh, 
				    args->data.ints[i]))) {
		    if (!already_warned) {
			ERR_BUF(cph->erh, ERR407);  /* just warn once */
			already_warned = 1;
		    }
		} else {
		    phg_ar_free_entry(arh, entry);
		}
	    }
	    break;
	    
	case PHG_ARGS_AR_NETWORKS :
	    {
		int	 nl = args->data.num_ints;
		Pint_list *lsts = (Pint_list *)malloc((unsigned)(nl * sizeof(Pint_list)));
		int	 upper_bound = 0;
		Pint_list	 ids;

		/* create list of all sids under specified structures */
		for (i = 0; i < nl; i++) {
		    if (!phg_ar_get_entry_from_archive(arh,
				    args->data.ints[i])) {
			if (!already_warned) {
			    ERR_BUF(cph->erh, ERR407);	/* just warn once */
			    already_warned = 1;
			}
			lsts[i].num_ints = 0;
		    } else {
			if (get_ar_structure_network_ids(cph, arh, 
				args->data.ints[i], &lsts[i])) {
			    ERR_BUF(cph->erh, ERR900);
			    for (j = 0; j < i; j++)
				free((char *)lsts[j].ints);
			    free((char *)lsts);
			    return;
			}
		    }

		    qsort((char *)lsts[i].ints, lsts[i].num_ints, 
			    sizeof(Pint), intcompare);
	
		    upper_bound += lsts[i].num_ints;
		}
		
		ids.ints = (Pint *)malloc((unsigned)(upper_bound * sizeof(Pint)));
		
		merge_and_remove_duplicates(nl, lsts, &ids);
		
		for (i = 0; i < ids.num_ints; i++) {
		    phg_ar_free_entry(arh, 
			phg_ar_get_entry_from_archive(arh, ids.ints[i]));
		}
		
		for (i = 0; i < nl; i++) {
		    if (lsts[i].num_ints) 
			free((char *)lsts[i].ints);
		}
		free((char *)lsts);
		free((char *)ids.ints);
		
	    }
	    break;

	case PHG_ARGS_AR_ALL :
	    /* delete all structs by removing the file and reopening anew */
	    id = arh->arid;
	    if (!(arname = malloc((unsigned)(strlen(arh->fname) + 1)))) {
		ERR_BUF(cph->erh, ERR900);
		return;
	    }
	    strcpy(arname, arh->fname);
	    op_and_cl_args.data.idata = id;
	    CP_FUNC(cph, CP_FUNC_OP_AR_CLOSE, &op_and_cl_args, &dummy_ret);
	    unlink(arname);
	    op_and_cl_args.data.ar_open.fname = arname;
	    op_and_cl_args.data.ar_open.name_length = sizeof(arname);
	    op_and_cl_args.data.ar_open.arid = id;
	    CP_FUNC(cph, CP_FUNC_OP_AR_OPEN, &op_and_cl_args, &dummy_ret);
	    free((char *)arname);
	    break;
	default :
	    return;
    }
}

void
phg_cpx_ar_get_names( cph, cp_args, ret )
Cp_handle cph;
Phg_args *cp_args;
Phg_ret *ret;
{
    Pint	    ar_id = cp_args->data.idata;
    Ar_handle	    arh;
    int		    i;
    Phg_ar_index_entry   *entry;
    
    ret->err = !0;
    
    GET_ARH(cph, ar_id, arh);
    
    /* count the structures */
    ret->data.int_list.num_ints = 0;
    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
	ret->data.int_list.num_ints++;
    PHG_AR_END_FOR_ALL_TOC_ENTRIES
    
    if (!(ret->data.int_list.ints = (int *)PHG_SCRATCH_SPACE(&cph->scratch, 
		ret->data.int_list.num_ints * sizeof(Pint)))) {
	ERR_BUF(cph->erh, ERR900);
    } else {
        /* catalog the ids */
	i = 0;
	PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
	    ret->data.int_list.ints[i++] = entry->str;
	PHG_AR_END_FOR_ALL_TOC_ENTRIES
	ret->err = 0;
    }
}


void
phg_cpx_ar_get_hierarchy( cph, cp_args, ret )
Cp_handle cph;
Phg_args *cp_args;
Phg_ret *ret;
{
    Phg_args_q_hierarchy    *args = &(cp_args->data.q_ar_hierarchy.hier);
    Pelem_ref_list	     allpaths, curpath;
    Pint_list		     counts;
    int			     retval = TRUE;
    Phg_args_hierarchy_dir   dir = args->dir;
    Pint		     struct_id = args->struct_id;
    Ppath_order		     order = args->order;
    Pint		     depth = args->depth;
    Ar_handle		     arh;
    
    ret->err = !0;
    
    GET_ARH(cph, cp_args->data.q_ar_hierarchy.arid, arh);

    if (!phg_ar_get_entry_from_archive(arh, struct_id)) {
	ERR_BUF(cph->erh, ERR201);
	return;
    }

    /* alloc 1024 cells, use realloc later if it's not big enough */
    if (!(allpaths.elem_refs = 
		(Pelem_ref *)PHG_SCRATCH_SPACE(&cph->scratch,
		    1024 * sizeof(Pelem_ref)))) {
        ERR_BUF(cph->erh, ERR900);
        return;                                         /* out of memory */
    }
    
    /* start out with room for a current path depth of 
     * PHG_AR_TMPMEM_BLOCKSIZE and a total of PHG_AR_TMPMEM_BLOCKSIZE paths*/
    curpath.elem_refs = (Pelem_ref *) malloc(PHG_AR_TMPMEM_BLOCKSIZE * 
				      sizeof(Pelem_ref));
    if (!(curpath.elem_refs)) {
        ERR_BUF(cph->erh, ERR900);
        return;                                         /* out of memory */
    }
    counts.ints = (Pint *) malloc(PHG_AR_TMPMEM_BLOCKSIZE * sizeof(Pint));
    if (!(counts.ints)) {
	free((char *)curpath.elem_refs);
        ERR_BUF(cph->erh, ERR900);
        return;                                         /* out of memory */
    }

    allpaths.num_elem_refs = curpath.num_elem_refs = counts.num_ints = 0;
    if (dir == PHG_ARGS_HIER_DESCENDANTS)
        retval = phg_ar_inq_descendants(cph, arh, struct_id, &allpaths, 
			&curpath, &counts, order, depth);
    else 
        retval = phg_ar_inq_ancestors(cph, arh, struct_id, &allpaths, 
			&curpath, &counts, order, depth);
				   
    if (!retval) {
        ERR_BUF(cph->erh, ERR900);                     /* out of memory */
        goto free_and_return;
    }
    
    /* make sure cph->scratch has enough space before copying counts array */
    if (allpaths.num_elem_refs*sizeof(Pelem_ref) + counts.num_ints*sizeof(Pint) >
            cph->scratch.size) {
	    
        int increm = counts.num_ints * sizeof(Pint);
 
        cph->scratch.buf = realloc(cph->scratch.buf, cph->scratch.size+increm);
        if (cph->scratch.buf) {
            cph->scratch.size += increm;
            allpaths.elem_refs = (Pelem_ref *)cph->scratch.buf;
        } else {
            ERR_BUF(cph->erh, ERR900);                 /* out of memory */
            goto free_and_return;
        }
    }

    ret->data.hierarchy.paths = allpaths.elem_refs;
    ret->data.hierarchy.num_pairs = allpaths.num_elem_refs;
    ret->data.hierarchy.counts.num_ints = counts.num_ints;

    ret->data.hierarchy.counts.ints =
        (Pint *)(allpaths.elem_refs + allpaths.num_elem_refs);
	
    while (counts.num_ints--)
        ret->data.hierarchy.counts.ints[counts.num_ints] =
            counts.ints[counts.num_ints];

    ret->err = 0;

free_and_return:
    free((char*)curpath.elem_refs);
    free((char*)counts.ints);
}

void
phg_cpx_inq_ar_conflicting( cph, cp_args, ret )
Cp_handle cph;
Phg_args *cp_args;
Phg_ret *ret;
{
    Phg_args_q_conflicting  *args = &(cp_args->data.q_conflicting);
    Pint_list		     css_ids, ar_net_ids;
    register int	     i;
    Phg_ar_index_entry	    *entry;
    Ar_handle		     arh;
    
    ret->err = !0;
    
    GET_ARH(cph, args->arid, arh);
    
    /* get the CSS structure ids */
    if (args->op == PHG_ARGS_CONF_NET && args->src == PNET_CSS) {
	if (get_css_network_sids(cph, args->struct_id, &css_ids))
	    return;
    } else {
	if (get_css_struct_ids(cph, &css_ids))
	    return;
    }

    qsort((char *)css_ids.ints, css_ids.num_ints, sizeof(Pint), 
		    intcompare);

    /* we know that cph->scratch is at least as big as sizeof(Pint)*(#sids)
     * because of previous calls, so we don't need to count the conflicting
     * ones before using cph->scratch. */
    ret->data.int_list.ints = (Pint *)cph->scratch.buf;
    ret->data.int_list.num_ints = 0;
    
    if (args->op == PHG_ARGS_CONF_NET && args->src == PNET_AR) {
    
	if (get_ar_structure_network_ids(cph, arh,
					 args->struct_id, &ar_net_ids)) {
	    free((char *)css_ids.ints);
	    return;
	}

	/* now figure out which are in both css and archive net */
	if (css_ids.num_ints > 0)
	    for (i = 0; i < ar_net_ids.num_ints; i++) {
		if (search_integer_list(ar_net_ids.ints[i],
					css_ids.ints, css_ids.num_ints)) {
		    ret->data.int_list.ints[ret->data.int_list.num_ints++]
			    = ar_net_ids.ints[i];
		}
	    }
		
	free((char *)ar_net_ids.ints);

    } else {

	/* check each entry to see if it's in CSS id list.  If it is,
	 * then add to list of conflicting structures */
	if (css_ids.num_ints > 0)
	    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
		if (search_integer_list((int)entry->str, css_ids.ints, 
				                    css_ids.num_ints)) {
		    ret->data.int_list.ints[ret->data.int_list.num_ints++] =
		    entry->str;
		}
	    PHG_AR_END_FOR_ALL_TOC_ENTRIES
    }

    free((char *)css_ids.ints);
    
    ret->err = 0;
}



/* return an ordered, duplicates removed, list of structure ids in a 
 * structure network rooted at the specified struct_id.  IT IS THE
 * CALLERS RESPONSIBILITY TO FREE THE lst->ints STRUCTURE WHEN
 * IT'S NOT NEEDED */
static int
get_ar_structure_network_ids(cph, arh, struct_id, lst)
Cp_handle   cph;
Ar_handle   arh;
Pint	    struct_id;
Pint_list	   *lst;
{
    Phg_args		     args;
    Phg_ret		     ret;
    Phg_args_q_hierarchy    *hier = &args.data.q_ar_hierarchy.hier;
    register int	     i, j;
    
    args.data.q_ar_hierarchy.arid = arh->arid;
    hier->dir	= PHG_ARGS_HIER_DESCENDANTS;
    hier->depth	= 0;
    hier->order	= PORDER_TOP_FIRST;
    hier->struct_id = struct_id;
    CP_FUNC(cph, CP_FUNC_OP_AR_GET_HIERARCHY, &args, &ret);
	
    /** this won't put on the root structure, so explicitly add it in **/
    if (ret.err)
        return(1);
    else {
        lst->num_ints = ret.data.hierarchy.num_pairs + 1;
        if (!(lst->ints = 
		    (Pint *)malloc((unsigned)(lst->num_ints * sizeof(Pint))))) {
	    ERR_BUF(cph->erh, ERR900);
	    return(1);
        } else {
	    /* copy list */
	    for (i = 0; i < lst->num_ints - 1; i++) {
	        lst->ints[i] = ret.data.hierarchy.paths[i].struct_id;
	    }
	    lst->ints[lst->num_ints - 1] = struct_id;

	    /* sort and remove duplicates */
	    qsort((char *)lst->ints, lst->num_ints,
			sizeof(Pint), intcompare);
	    for (i = j = 0; j < lst->num_ints; j++) {
	        while ((j + 1 < lst->num_ints) &&
		       (lst->ints[j] == lst->ints[j+1]))
		    j++;
	        lst->ints[i++] = lst->ints[j];
	    }
	    lst->num_ints = i;
	}
	return(0);
    }
}



/*** CALLER RESPONSIBLE FOR FREEING ints FIELD ***/
static int
get_css_struct_ids(cph, lst)
Cp_handle   cph;
Pint_list	   *lst;
{
    Phg_args	args;
    Phg_ret	ret;
    
    CP_FUNC(cph, CP_FUNC_OP_INQ_STRUCT_IDS, &args, &ret);
    if (ret.err)
        return(1);
    else {
        lst->num_ints = ret.data.int_list.num_ints;
	if (lst->num_ints == 0) {
	    lst->ints = NULL;
	} else if (!(lst->ints = (Pint *)malloc((unsigned)(lst->num_ints * 
						    sizeof(Pint))))) {
	    ERR_BUF(cph->erh, ERR900);
	    return(1);
        } else {
	    bcopy((char *)ret.data.int_list.ints, 
		(char *)lst->ints, lst->num_ints * sizeof(Pint));
        }
    }

    return(0);
}


/*** CALLERS RESPONSIBILE FOR FREEING lst->ints FIELD ***/
static int
get_css_network_sids(cph, sid, lst)
Cp_handle	     cph;
Pint		     sid;
register Pint_list    *lst;
{
    Phg_args		     args;
    Phg_ret		     ret;
    Phg_args_q_hierarchy    *hier = &(args.data.q_hierarchy);
    register int	     i;

    hier->dir = PHG_ARGS_HIER_DESCENDANTS;
    hier->struct_id = sid;
    hier->order = PORDER_TOP_FIRST;
    hier->depth = 0;
    CP_FUNC(cph, CP_FUNC_OP_INQ_HIERARCHY, &args, &ret);

    if (ret.err)
	return(1);
    else if (ret.data.hierarchy.num_pairs == 0) {
	lst->num_ints = 1;
	if (!(lst->ints = (Pint *)malloc(sizeof(Pint)))) {
	    ERR_BUF(cph->erh, ERR900);
	    return(1);
	} else {
	    lst->ints[0] = sid;
	}
    } else {
	lst->num_ints = ret.data.hierarchy.num_pairs;
	if (!(lst->ints = (Pint *)malloc((unsigned)(lst->num_ints * sizeof(Pint))))) {
	    ERR_BUF(cph->erh, ERR900);
	    return(1);
	} else {
	    for (i = 0; i < lst->num_ints; i++) {
		lst->ints[i] = ret.data.hierarchy.paths[i].struct_id;
	    }
	}
    }
    
    return(0);
}



/** takes in a list of structure ids, which are interpreted as a list of
 ** structure network roots.  Returns a sorted, duplicates removed, list
 ** of all of the structures in those networks.  Can come from either the
 ** CSS or the archive **/
/** CALLER RESPONSIBLE FOR FREEING out->ints **/
static int
compile_network_sids(cph, arh, in, where, out)
Cp_handle	 cph;
Ar_handle	 arh;		/* ignore if where == PNET_CSS */
Pint_list		*in;		/* list of networks (by root sid) */
Pstruct_net_source	 where;		/* get from CSS or archive? */
Pint_list		*out;		/* list of sids in network */
{
    int	      nl = in->num_ints;
    Pint_list  *lsts = (Pint_list *)malloc((unsigned)(nl * sizeof(Pint_list)));
    int	      i, j, upper_bound = 0;

    if (!lsts) {
	ERR_BUF(cph->erh, ERR900);
	return(1);
    }

    for (i = 0; i < nl; i++) {
    
	if (where == PNET_AR) {
	    if (!phg_ar_get_entry_from_archive(arh, in->ints[i])) {
		lsts[i].num_ints = 1;
		lsts[i].ints = (Pint *)malloc(sizeof(Pint));
		lsts[i].ints[0] = in->ints[i];
	    } else {
		if (get_ar_structure_network_ids(cph, arh, in->ints[i],
				    &lsts[i])) {
		    ERR_BUF(cph->erh, ERR900);
		    for (j = 0; j < i; j++)
			free((char *)lsts[j].ints);
		    free((char *)lsts);
		    return(1);
		}
	    }
	    
	} else { /* where == PNET_CSS */
	    Cpx_css_srvr   *css_srvr;
	    CPX_MASTER_SERVER(cph, css_srvr)
	    if (!(*css_srvr->struct_exists)(cph, css_srvr, in->ints[i])) {
		ERR_BUF(cph->erh, ERR200);
		lsts[i].num_ints = 0;
	    } else {
		if (get_css_network_sids(cph, in->ints[i], &lsts[i])) {
		    for (j = 0; j < i; j++)
			free((char *)lsts[j].ints);
		    free((char *)lsts);
		    return(1);
		}
	    }
	}
	
	qsort((char *)lsts[i].ints, lsts[i].num_ints, sizeof(Pint), 
		    intcompare);

	upper_bound += lsts[i].num_ints;
    }

    out->ints = (Pint *)malloc((unsigned)(upper_bound * sizeof(Pint)));

    merge_and_remove_duplicates(nl, lsts, out);

    for (i = 0; i < nl; i++) {
	if (lsts[i].num_ints)
	    free((char *)lsts[i].ints);
    }
	
    free((char *)lsts);
    
    return(0);
}

