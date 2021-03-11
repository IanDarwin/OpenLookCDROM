/* $XConsortium: err.h,v 5.2 94/04/17 20:41:45 rws Exp $ */

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

#ifndef PHG_ERR_H_INCLUDED
#define PHG_ERR_H_INCLUDED

#define ERR_SET_CUR_FUNC( erh, funcnum) \
    ((erh)->cur_func_num = (funcnum))

#define ERR_BUF( erh, errnum) \
    if ( (erh)->mode == PERR_ON) (*(erh)->buf_func)((erh), (errnum))

#define ERR_FLUSH( erh) \
    if ( (erh)->flush_func) (*(erh)->flush_func)( erh)

/* ERR_HANDLE calls the global error handler function */
#define ERR_HANDLE( errnum, funcnum, erf) \
    (*phg_errhandle)((errnum), (funcnum), (erf))

/*** ERR_REPORT called by binding only ***/
#define ERR_REPORT( erh, errnum) \
    if ( (erh)->mode == PERR_ON && (erh)->report_func) \
	(*(erh)->report_func)((erh), (errnum))

/*** ERR_DESTROY called by error code only ***/
#define ERR_DESTROY( erh) \
    if ( (erh)->destroy_func) (*(erh)->destroy_func)(erh)

typedef struct {
    Pint	errnum;
    Pint	funcnum;
} Err_msg;

typedef struct {
    char	*fname;	/* error file name */
} Err_local_data;

typedef struct {
    int		sfd;	/* socket fd for errors from server */
    char	*fname;	/* error file name */
} Err_client_data;

typedef struct {
    int		sfd;		/* fd to write to client */
} Err_remote_data;

typedef struct {
    Perr_mode	mode;
    Perr_mode	err_state;	/* NOT maintained in server */
    int		cur_func_num;
    int		errno;
    void	(*buf_func)();
    void	(*flush_func)();
    void	(*report_func)();
    void	(*destroy_func)();
    union {
	Err_local_data		local;
	Err_client_data		client;
	Err_remote_data		remote;
    }		data;
} Err_struct;

typedef Err_struct	*Err_handle;

extern Err_handle	phg_err_init_local();
extern Err_handle	phg_err_init_client();
extern Err_handle	phg_err_init_remote();

/* declare the global error handler function pointer. */
extern void		(*phg_errhandle)();

#endif
