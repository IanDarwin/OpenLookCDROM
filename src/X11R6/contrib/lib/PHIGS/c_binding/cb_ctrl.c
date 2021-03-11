/* $XConsortium: cb_ctrl.c,v 5.8 94/04/17 20:40:42 mor Exp $ */

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

/* Control functions for the PHIGS C binding */

#include "phg.h"
#include "phg_dt.h"
#include "cp.h"
#include "cb_priv.h"

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif

/* The global CP handle for the binding. */
Cp_handle	phg_cur_cph = NULL;

static int		xinfo_used;
static Pxphigs_info	xphigs_info;

static XrmOptionDescRec	options[] = {
    {"-display",	".display",	XrmoptionSepArg,	(caddr_t) NULL},
    {"-bufmode",	".bufMode",	XrmoptionSepArg,	(caddr_t) NULL},
    {"=",		"*geometry",	XrmoptionIsArg,		(caddr_t) NULL},
    {"-label",		".label",	XrmoptionSepArg,	(caddr_t) NULL},
    {"-iconlabel",	".iconLabel",	XrmoptionSepArg,	(caddr_t) NULL}
};

#define NUM_OPTIONS	(sizeof(options)/sizeof(options[0]))

merge_args( xinfo )
    Pxphigs_info	*xinfo;
{
    XrmDatabase		db = NULL;

    if ( !xinfo->rmdb ) {
	XrmInitialize();
    }

    if ( xinfo->args.argc_p ) {
	XrmParseCommand( &db, options, NUM_OPTIONS,
	    xinfo->appl_id.name ? xinfo->appl_id.name : "phigs",
	    xinfo->args.argc_p, xinfo->args.argv );
    }

    if ( xinfo->rmdb )
	XrmMergeDatabases( db, &xinfo->rmdb );
    else
	xinfo->rmdb = db;
}

void
popen_xphigs( error_file, memory, xinfo_mask, xinfo )
    char		*error_file;	/* error file name */
    size_t		memory;
    unsigned long	xinfo_mask;
    Pxphigs_info	*xinfo;
{
    xinfo_used = 1;
    bzero( (char *)&xphigs_info, (int)sizeof(Pxphigs_info) );

    /* Control stuff. */
    if ( xinfo_mask & PXPHIGS_INFO_DISPLAY )
	xphigs_info.display = xinfo->display;
    if ( xinfo_mask & PXPHIGS_INFO_FLAGS_NO_MON )
	xphigs_info.flags.no_monitor = xinfo->flags.no_monitor;
    if ( xinfo_mask & PXPHIGS_INFO_FLAGS_CLIENT_SS )
	xphigs_info.flags.force_client_SS = xinfo->flags.force_client_SS;

    /* RMDB stuff. */
    if ( xinfo_mask & PXPHIGS_INFO_APPL_ID ) {
	xphigs_info.appl_id.name = xinfo->appl_id.name;
	xphigs_info.appl_id.class_name = xinfo->appl_id.class_name;
    }
    if ( xinfo_mask & PXPHIGS_INFO_RMDB )
	/* Must assign rmdb before the args. */
	xphigs_info.rmdb = xinfo->rmdb;
    if ( xinfo_mask & PXPHIGS_INFO_ARGS ) {
	xphigs_info.args.argc_p = xinfo->args.argc_p;
	xphigs_info.args.argv = xinfo->args.argv;
	merge_args( &xphigs_info );
    }

    popen_phigs( error_file, memory );
    return;
}

/* Need to get W_OK for SYSV */
#ifdef SYSV
#include <unistd.h>
#endif

static int
check_errfile( error_file)
    char	*error_file;
{
    int		status = 0;
    FILE	*erf;

    /* see if error file can be opened for writing */
    if ( error_file) {
	if ( access( error_file, W_OK) == 0) {
	    status = 1;

	/* file might not exist, see if it can be created */
	} else if ( errno == ENOENT && (erf = fopen( error_file, "a")) ) {
	    status = 1;
	    (void) fclose( erf);
	    (void) unlink( error_file);
	}
    } else {	/* NULL means use stderr */
	status = 1;
    }
    return status;
}

static void
check_env_controls()
{
    char		*str;

    /* If the application already set something we'll override it. */
    if ( str = getenv( "PEX_SI_API_NO_PM" ) ) {
	xinfo_used = 1;
	xphigs_info.flags.no_monitor = atoi( str ) ? 1 : 0;
    }
    if ( str = getenv( "PEX_SI_API_CLIENT_SS" ) ) {
	xinfo_used = 1;
	xphigs_info.flags.force_client_SS = atoi( str ) ? 1 : 0;
    }
}

void
popen_phigs( error_file, memory )
    char	*error_file;	/* error file name */
    size_t	memory;		/* NOT USED */
{
    if (CB_PHIGS_OPEN( phg_cur_cph)) {
	ERR_SET_CUR_FUNC( phg_cur_cph->erh, Pfn_open_phigs);
	ERR_REPORT( phg_cur_cph->erh, ERR1);

    } else if ( !check_errfile( error_file)) {
	/* have to bypass the error reporting code: */
	ERR_HANDLE( ERR450, Pfn_open_phigs, error_file);

    } else {
	check_env_controls();
	if ( phg_cur_cph = phg_cpxc_open( error_file,
		xinfo_used ? &xphigs_info : (Pxphigs_info *)NULL ) ) {
	    phg_cur_cph->psl->phg_sys_state = PSYS_ST_PHOP;
	}
    }

    /* Clear any controls so they don't carry over to next OPEN. */
    if ( xinfo_used ) {
	xinfo_used = 0;
	bzero( (char *)&xphigs_info, (int)sizeof(Pxphigs_info) );
    }
}

void
pclose_phigs()
{
    Phg_args	cp_args;

    if (CB_ENTRY_CHECK( phg_cur_cph, ERR4, Pfn_close_phigs)) {
	if ( phg_cur_cph->psl->phg_ws_state == PWS_ST_WSCL
	    && phg_cur_cph->psl->phg_struct_state == PSTRUCT_ST_STCL
	    && phg_cur_cph->psl->phg_ar_state == PST_ARCL)
	{
	    CP_FUNC( phg_cur_cph, CP_FUNC_OP_CLOSE_PHIGS, &cp_args, NULL);
	    phg_cur_cph = NULL;
	} else {
	    ERR_REPORT( phg_cur_cph->erh, ERR4);
	}
    }
}

void
pinq_sys_st(sys_state)
    Psys_st	*sys_state;	/* OUT the system state	*/
{
    if ( CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY))
	 *sys_state = PSYS_ST_PHOP;
    else
	 *sys_state = PSYS_ST_PHCL;
}


void
pinq_phigs_facs(length, start, error_ind, open_ws, open_ar, num_names,
    char_sets, length_list, norm_filt, inv_filt)
Pint	length;	/* length of application list	*/
Pint	start;	/* starting position	*/
Pint	*error_ind;	/* OUT error indicator	*/
Pint	*open_ws;	/* OUT max. num. simultaneously open ws	*/
Pint	*open_ar;	/* OUT max. num. simultaneously open archive files */
Pint	*num_names;	/* OUT number of available names for name sets */
Pint_list *char_sets;	/* OUT list of character sets	*/
Pint    *length_list;   /* OUT length of list in PHIGS  */
Pint	*norm_filt;	/* OUT maximum length of normal filter list for ISS */
Pint	*inv_filt;	/* OUT maximum length of inverted filter list for ISS */
{
    if (! CB_PHIGS_OPEN( phg_cur_cph))
	    *error_ind = ERR2;
    else {
	*open_ws = phg_cur_cph->pdt.max_num_open_workstations;
	*open_ar = phg_cur_cph->pdt.max_num_open_archives;
	*num_names = phg_cur_cph->pdt.max_num_names_for_nameset;
	*norm_filt = phg_cur_cph->pdt.max_length_normal_iss_filter;
	*inv_filt = phg_cur_cph->pdt.max_length_inverted_iss_filter;
	*error_ind = ERR0;
	char_sets->num_ints = 0;
	if ( (*length_list = phg_cur_cph->pdt.char_sets.num_ints) > 0) {
	    if (start < 0 || start >= *length_list)
		*error_ind = ERR2201;	/* char_sets->num_ints "unreliable" */
	    else if (length > 0) {
		register int	i;
		char_sets->num_ints = MIN( length, *length_list - start );
 		for(i=0; i < char_sets->num_ints; i++) {
		    char_sets->ints[i] =
			phg_cur_cph->pdt.char_sets.ints[start + i];
	        }
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}

void
pinq_gse_facs(length, start, error_ind, gse, total_length)
Pint    	length; 	/* length of application list	*/
Pint    	start;  	/* starting position	*/
Pint     	*error_ind;	/* OUT error indicator	*/
Pgse_id_dep_list *gse;  	/* OUT list of GSE ids and their dependencies	*/
Pint    	*total_length;	/* OUT length of list in PHIGS	*/
{
    register	int	i;

    if (! CB_PHIGS_OPEN( phg_cur_cph))
	    *error_ind = ERR2;
    else {
	*error_ind = ERR0;
	gse->num_id_facs = 0;
	if ( (*total_length = phg_cur_cph->pdt.gses.num_id_facs) > 0) {
	    if (start < 0 || start >= *total_length)
		*error_ind = ERR2201;
	    else if (length > 0) {
		gse->num_id_facs = MIN( length, *total_length - start );
 		for(i=0; i < gse->num_id_facs; i++) {
		    gse->id_facs[i] =
			phg_cur_cph->pdt.gses.id_facs[start + i];
	        }
	    } else if (length < 0)
		*error_ind = ERRN153;
	}
    }
}

void
pinq_model_clip_facs(length, start, error_ind, num, ops, total_length)
Pint    	length; 	/* length of application list	*/
Pint    	start;   	/* starting position	*/
Pint    	*error_ind;	/* OUT error indicator	*/
Pint    	*num;   	/* OUT number of distinct planes in modelling clipping volume */
Pint_list	*ops;   	/* OUT list of operators	*/
Pint    	*total_length;	/* OUT length of list in PHIGS	*/
{
    register	int	i;

    if (! CB_PHIGS_OPEN( phg_cur_cph))
	    *error_ind = ERR2;
    else {
	*num = phg_cur_cph->pdt.max_num_model_clip_volumes;
	*error_ind = ERR0;
	ops->num_ints = 0;
	if ( (*total_length = phg_cur_cph->pdt.model_clip_ops.num_ints) > 0) {
	    if (start < 0 || start >= *total_length)
		*error_ind = ERR2201;
	    else if (length > 0) {
		ops->num_ints = MIN(length, *total_length - start);
 		for(i=0; i < ops->num_ints; i++) {
		    ops->ints[i] =
			phg_cur_cph->pdt.model_clip_ops.ints[start + i];
	        }
	    }
        } else if (length < 0)
		*error_ind = ERRN153;
    }
}

void
pemergency_close_phigs()
{
    Phg_args    cp_args;

    if (CB_PHIGS_OPEN( phg_cur_cph)) {
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_EMERG_CLOSE, &cp_args, NULL);
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_CLOSE_PHIGS, &cp_args, NULL);
	phg_cur_cph = NULL;
    }
}
