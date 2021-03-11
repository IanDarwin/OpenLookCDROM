/* $XConsortium: cb_err.c,v 5.4 94/04/17 20:40:45 eswu Exp $ */

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

/* Error functions for the PHIGS C binding */

#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

void
pset_err_hand_mode( mode)
    Perr_mode	mode;	/* error handling mode	*/
{
    Phg_args			cp_args;

    if ( CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_set_err_hand_mode)) {
	cp_args.data.idata = (Pint)mode;
	CP_FUNC( phg_cur_cph, CP_FUNC_OP_SET_ERR_HAND_MODE, &cp_args, NULL);
    }
}

void
pinq_err_hand_mode( error_ind, mode)
    Pint	*error_ind;	/* OUT error indicator	*/
    Perr_mode	*mode;		/* OUT error mode	*/
{
    if ( CB_ENTRY_CHECK( phg_cur_cph, 0, Pfn_INQUIRY)) {
	*error_ind = 0;
	*mode = phg_cur_cph->erh->mode;
    } else
	*error_ind = ERR2;
}

void
perr_log( errnum, funcnum, fname)
    Pint	errnum;
    Pint	funcnum;
    char	*fname;
{
    FILE	*erf;
    char	msg_buf[CB_MAX_ERR_MSG_LENGTH];

    phg_cb_format_err_msg( PHG_C_BINDING, errnum, funcnum, msg_buf);
    if ( fname && (erf = fopen( fname, "a"))) {
	(void) fprintf( erf, "%s", msg_buf);
	(void) fclose( erf);
    } else {
	(void) fprintf( stderr, "%s", msg_buf);
    }
}

void
pset_err_hand( new_err_hand, old_err_hand )
    void	(*new_err_hand)();
    void	(**old_err_hand)();
{
    *old_err_hand = phg_errhandle;
    phg_errhandle = new_err_hand;
}

extern char *phg_path();


static	int  err_punt_msg();
static char  err_msg_format[] = "PHIGS error %d in %s: %s\n";

static FILE *errmsgfile, *funcmsgfile;

#define MSG_BUF_SIZE	1024

static int
err_lookup_string (fn, num, msgbuf, msgbufsize)
    FILE *fn;
    int num;
    char *msgbuf;
    int msgbufsize;
{
    int curnum = -9999;

    /* look for the message with num at it's start */
    do { 
	if (fgets (msgbuf, msgbufsize, fn) == NULL) {
		return FALSE;
	}
	(void) sscanf (msgbuf, "%d", &curnum);
    } while (curnum != num);
    return TRUE;
}

static void
err_add_nl (buf, linelen)	/* add new lines to long message */
    char	*buf;
    int		linelen;
{
    char *p;

    if (strlen (buf) > linelen) {
	p = buf + linelen;
	while (--p != buf)
            /* replace blank with new line, check for more. */
            if (*p == ' ') {
                *p = '\n';
		err_add_nl (p+1, linelen);
		break;
        }
    }
}

/* Create the PHIGS error message and place it in "buf."  The message
 * contains the new line char.
 */

void
phg_cb_format_err_msg( binding, errnum, funcnum, buf )
    int		binding;	/* IN: binding that's calling */
    int		errnum;		/* IN: error number */
    int		funcnum;	/* IN: function id */
    char	*buf;		/* OUT: error message */
{
    char msgbuf [MSG_BUF_SIZE];
    char funcbuf [MSG_BUF_SIZE];
    char *msgtext, *funcname = (char*) NULL, *msgnl;

    /* if the Function Name file isn't open, open it */
    if (funcmsgfile == NULL) {
	char *funcfname;

        if( (!(funcfname = phg_path( FUNCNAME_FILE_NAME, (Err_handle)NULL, 0)))
	    || ((funcmsgfile = fopen (funcfname, "r")) == NULL) ) {
	    (void) err_punt_msg( errnum, funcnum, funcname, buf );
	    return;
	}
    } else
	rewind (funcmsgfile);	/* start search at start of file */

    /* if the errmessage file isn't open, open it */
    if (errmsgfile == NULL) {
	char *errfname;

        if( (!(errfname = phg_path( ERRMSG_FILE_NAME, (Err_handle)NULL, 0)))
	    || ((errmsgfile = fopen (errfname, "r")) == NULL) ) {
	    (void) err_punt_msg( errnum, funcnum, funcname, buf );
	    return;
	}
    } else
	rewind (errmsgfile);	/* start search at start of file */

    /* 
     * look for function name.
     */
    if (err_lookup_string (funcmsgfile, funcnum, funcbuf, MSG_BUF_SIZE)) {
	funcname = strchr (funcbuf, ':')+1;
	/* null out newline in function name string */
	msgnl = strrchr (funcname, '\n');
	if (msgnl)
	    *msgnl = '\0';
    }

    /* 
     * look for error message. 
     */
    if (err_lookup_string (errmsgfile, errnum, msgbuf, MSG_BUF_SIZE)) {
	msgtext = strchr (msgbuf, ':'); /* find delimiter */
	*msgtext = '\0'; 		/* end msgname string */
	++msgtext;			/* advance to message text */

	/* null out newline in message string */
	msgnl = strrchr (msgtext, '\n');
	if (msgnl)
	    *msgnl = '\0';

	/*
         * at last, format the message.
         */
	(void) sprintf (buf, err_msg_format, errnum, funcname, msgtext);
	err_add_nl (buf, 80);
    } else { 
	/* failed to find error message, use old format */
	(void) err_punt_msg( errnum, funcnum, funcname, buf );
    }
}

/* Special case certain errors, and do our best with others,
 * so the message can be printed without run-time files.
 */
static
int	/* TRUE if message is known; FALSE (and numbers formatted) otherwise */
err_punt_msg( errnum, funcnum, funcname, buf )
    int		errnum;		/* IN: error number */
    int		funcnum;	/* IN: function id */
    char	*funcname;	/* IN: function name string */
    char	*buf;		/* OUT: error message */
{
    char msgbuf [MSG_BUF_SIZE];
    char funcbuf [MSG_BUF_SIZE];
    int		known = TRUE;
    /* Could build a list of (msg number, msg pointer) pairs & search in loop */
    /* Further compact these fallback messages using FIXED %s escapes */
    static	char ignoring_function[] = "Ignoring function";
    static	char cannot_open_phigs[] = "cannot open PHIGS";
    static	char msg_for_ERRN51[] =
		    "%s, %s, cannot locate PEX API file \"phigsmon\"";
    static	char msg_for_ERRN54[] =
		    "%s, cannot locate PHIGS API support file";
    static	char msg_for_ERRN55[] =
		    "%s, %s, cannot open font files";
    static	char function_0[] = "OPEN PHIGS";

    if (funcname == (char*)NULL) {
	/* failed to find function name, use old format */
	funcname = funcbuf;
	(void) sprintf (funcbuf, "function number %d", funcnum);
    }

    /* Could separate finding function number from finding msg number in list */
    if (known = (funcnum == 0)) {  /* only 1 known function name at this time */
	switch (errnum) {
	case ERRN51:
	    (void) sprintf(msgbuf, err_msg_format, errnum, function_0,
				msg_for_ERRN51);
	    break;
	case ERRN54:
	    (void) sprintf(msgbuf, err_msg_format, errnum, function_0,
				msg_for_ERRN54);
	    break;
	case ERRN55:
	    (void) sprintf(msgbuf, err_msg_format, errnum, function_0,
				msg_for_ERRN55);
	    break;
	default:
	    known = FALSE;
	    /* Override input, possibly "function number 0" */
	    funcname = function_0;
	    break;
	}
    }
    if (known) {
	(void) sprintf (buf, msgbuf, ignoring_function, cannot_open_phigs);
	err_add_nl (buf, 80);
    } else
	(void) sprintf (buf, "PHIGS error %d in %s\n", errnum, funcname);
    return known;
}
