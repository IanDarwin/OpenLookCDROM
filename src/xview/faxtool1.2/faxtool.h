/*
 * Copyright (c) 1993 Thomas W. Corson
 * Copyright (c) 1993 VetMark Systems, Inc. d.b.a Information Dynamics
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Thomas W. Corson, VetMark Systems, or Information Dynamics may not be used 
 * in any advertising or publicity relating to the software without the 
 * specific, prior written permission of Thomas W. Corson and VetMark Systems.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL THOMAS W. CORSON OR VETMARK SYSTEMS, INC. BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include "faxtool_ui.h"

#define PATHLEN	128
#define COMLEN	512
#define TXTLEN	80
#define NUMLEN	25
#define MAXFILE	25
#define MAXARGS	25

#define	MAX_FRAMES		50
#define	MAX_POPUPS		15
#define	MAX_CHOICES		25
#define	MAX_LISTS_PER_PANEL	5
#define	MAX_GROUPS_PER_PANEL	30

#define	ORIG_HEIGHT	1015
#define	ORIG_WIDTH	1016
#define	LAST_HEIGHT	1017
#define	LAST_WIDTH	1018
#define	PANELS_LIST	1024
#define	GROUPS_LIST	1025
#define	LISTS_LIST	1026

#undef	FALSE
#undef	TRUE

typedef	enum {
	FALSE,
	TRUE
}		boolean;

typedef enum
{
	SCALE_RELATIVE,
	SCALE_ABSOLUTE
}               SCALE_TYPE;

typedef struct {
	char	server[NUMLEN + 1];
	char	spool_dir[PATHLEN + 1];
	char	view_cmd[TXTLEN + 1];
	char	print_cmd[TXTLEN + 1];
	char	preview_cmd[TXTLEN + 1];
} Defaults;

typedef struct {
	char	fax_number[NUMLEN + 1];
	char	name[TXTLEN + 1];
	char	company[TXTLEN + 1];
	char	re[TXTLEN + 1];
	char	comments[COMLEN + 1];
} Cover;

int
add_file_to_send(gfm_popup_objects	*ip,
		 char			*directory,
		 char			*file);

int 
validate_fax(faxtool_window1_objects 	*ip,
	     Cover			*cover);

void
do_command(char		*argv[],
	   Notify_value	(*notify_fn) (Notify_client, register int));

Notify_value
read_pipe_to_footer(Notify_client	client,
	 	    register int	fd);

Notify_value
read_pipe_to_list(Notify_client	client,
	 	  register int	fd);

Notify_value
sig_child_notify(Notify_client	client,
		 int		pid,
		 union wait	*status,
		 struct rusage	*rusage);

void
load_recv_queue(faxtool_window1_objects	*ip);

void
layout_objects(Frame frame,
	       Panel controls,
	       Xv_opaque target_objects,
	       Xv_opaque all_objects);

Group
which_group(Panel_item	list,
	    Group	top_group);

boolean
scale_list(Panel owner,
	   Panel_item list,
	   SCALE_TYPE type);

int
scale_list_height(Panel owner,
		  Panel_item list,
		  SCALE_TYPE type);

int
scale_list_width(Panel owner,
		 Panel_item list,
		 SCALE_TYPE type);

void
panel_resize(Panel	panel,
	     Group	top_group);

void
load_defaults(faxtool_prop_popup_objects	*ip,
	      FILE				*fd);

void
store_defaults(faxtool_prop_popup_objects	*ip,
	       FILE				*fd);

int
load_directory(Panel_item	list,
	       FILE		*fd);

int
store_directory(Panel_item	list,
	        FILE		*fd);

void
save_rc(void);

void
trim_string(char	*string);
