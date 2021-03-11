/* $XConsortium: cp.h,v 5.7 94/04/17 20:41:39 rws Exp $ */

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

#ifndef PHG_CP_H_INCLUDED
#define PHG_CP_H_INCLUDED

typedef void	(*Cp_func)();

#include "cpx.h"
#include "X11/Xos.h"

/* The maximum number of indirect data blocks ("pointed to" data) that
 * the CP will send across the communication channel for any single CP
 * command.
 */
#define CP_MAX_COM_PTRS		50

#ifndef PEX_API_SOCKET_IPC
#define CP_SHM_DATA_BUF_SIZE	20000	/* Pfloats, for correct alignment */
#define CP_SHM_ARGS_BUF_SIZE	1000	/* args elements */
#define CP_SHM_RET_BUF_SIZE	8000	/* Pfloats, for correct alignment */
#else
/* These buffers aren't used. */
#define CP_SHM_DATA_BUF_SIZE	1
#define CP_SHM_ARGS_BUF_SIZE	1
#define CP_SHM_RET_BUF_SIZE	1
#endif /* !PEX_API_SOCKET_IPC */

typedef enum {
    CP_LOCAL,
    CP_REMOTE
} Cp_configuration;


#define CP_FUNC( cph, _op, args, ret) \
    ((args)->op = (unsigned)(_op), (*(cph)->funcs[(int)(_op)])((cph), (args), (ret)))

/* Struct to use for internal file buffering, similar to type FILE. */
typedef struct {
    int		cnt;		/* current number of bytes in buffer */
    int		bufsize;	/* max size of buffer */
    int		fd;
    char	*ptr;		/* ptr to next byte to read */
    char	*base;		/* ptr to buffer */
} Cp_file;

typedef struct _Cp_display_connection {
    Display	*display;
    int		instance_count;
    struct {
	unsigned opened_by_api: 1;
    }		flags;
    struct _Cp_display_connection	*next;
} Cp_display_connection;

/* Data specific to the configuration (local, monitor or client) */

typedef struct {
    void	(*set_input_notify)();
} Cp_data_local;

typedef struct {
    int		child_pid;
    int		sfd;		/* socket fd for communication with monitor */
} Cp_data_client;

typedef enum {
    CP_READING_ARGS,
    CP_EXECUTING_CMD,
    CP_CMD_DONE
} Cp_cmd_state;

typedef struct {
    int			parent_pid;
    Cp_cmd_state	cmd_state;
    int			cur_cnt;	/* number of bytes read so far */
    Cp_file		*infile;
    int			sfd;		/* communication with client fd */
    Phg_args		*arg_buf;	/* not used when shared memory used */
    Phg_ret		*ret_buf;	/* not used when shared memory used */
    struct itimerval	cmd_timeout;
    XtAppContext	app_con;
    char		*name;		/* of application */
    char		*classname;	/* of application */
    int			argc;
    char		**argv;
} Cp_data_monitor;

typedef union {
    Cp_data_local	local;
    Cp_data_monitor	monitor;
    Cp_data_client	client;
} Cp_data;

typedef struct _Cp_wst_list_entry {
    Wst_handle			wst;
    struct _Cp_wst_list_entry	*next;
} Cp_wst_list_entry;

typedef enum {
    CP_RET_DATA_UNLOCKED,
    CP_RET_DATA_LOCKED,
    CP_RET_DATA_READY
} Cp_ret_data_state;

/* Shared memory buffer. */
typedef struct {
    int		shmid;		/* id from shmget(2) call */
    struct {
	int		buf_size;
	int		lock;
	int		head;
	int		tail;
	Phg_args	buf[CP_SHM_ARGS_BUF_SIZE];
    }		args;			/* args queue */
    struct {
	int		buf_size;
	int		lock;
	int		head;
	int		tail;
	Pfloat		buf[CP_SHM_DATA_BUF_SIZE];
    }		data;			/* pre-data queue */
    struct {
	int			buf_size;
	Cp_ret_data_state	state;
	Phg_ret			data;
	Pfloat			buf[CP_SHM_RET_BUF_SIZE];
    }		ret;			/* returned data buffers */
} Cp_shm_buf;

typedef struct _Cp_struct {
    Cp_func		funcs[NUM_CP_FUNCS];
    Psl_handle		psl;	/* NULL in monitor */
    Err_handle		erh;
    Cp_configuration	configuration;
    Cp_data		data;
    Ar_handle		arhl[MAX_NO_OPEN_ARFILES];
    Input_q_handle	input_q;   /* only in monitor */
    Cp_wst_list_entry	*wst_list; /* head of ws type linked list, not used
				      in monitor */
    Cpx_css_srvr	*css_srvr_list;
    Ws_handle		ws_list;
    Ar_handle		ar_list;
    struct {
	unsigned monitor_active: 1;
	unsigned force_client_SS: 1;
	unsigned in_monitor: 1;
	unsigned err_sync: 1;
	unsigned ignore_DC_errors: 1;
    }			flags;
    Phg_scratch		scratch;
    Cp_shm_buf		*shm_buf;
    int			max_fd;
    /* Use a generous size for fd_masks, to avoid including <sys/param.h> */
    unsigned long	fd_masks[64];
    Phg_desc_tbl	pdt;
    Cp_display_connection	*displays;
} Cp_struct;

extern Cp_handle	phg_cp_open();
extern int		phg_cp_add_wst();
extern void		phg_cp_rmv_wst();
extern void		phg_cp_send_request();
extern int		phg_cp_any_inp_device_active();

/* Notify utilities */
extern void		phg_ntfy_unregister_event();
extern void		phg_ntfy_unregister_window();
extern void		phg_ntfy_unregister_display();

#endif

