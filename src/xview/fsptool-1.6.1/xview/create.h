/********************************************************************************/
/* create.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtool_CREATE_H_
#define _FSPtool_CREATE_H_ 1

/********************************************************************************/

#define XV_SET_TRANSIENT(a) 							\
	    XSetTransientForHint((Display*)xv_get(baseframe.frame,XV_DISPLAY),	\
				xv_get(a,XV_XID),				\
				xv_get(baseframe.frame,XV_XID));

/********************************************************************************/

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	dir_list, dir_text;
    Panel_item	file_button, view_button, properties_button,
		sethost_button, abort_button, cache_button;

    Panel_drop_target_item	drop_target;
    Drag_drop			dnd;
    Selection_requestor		sel_req;
    }
  Base_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	status, size, gauge, full_gauge;
    }
  Transfer_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	localdir, free, dir_list;
    Panel_item	file_button, view_button;
    }
  Local_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	host_list;
    Panel_item	add_button, delete_button, clear_button, load_button, save_button;
    Panel_item	alias, host, port, description, ldir, rdir;
    }
  Hostlist_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	filter, message, checkboxes;
    }
  Filter_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	new_button, hostlist_button, hosts_button;
    Panel_item	host, port;
    }
  Sethost_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	fileactions[NO_FILETYPES+1];
    }
  Action_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	bufsize, localport, timeout, delaysize;
    }
  Fsp_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	openlook, cancelclose, menuclose, hostread;
    }
  Tool_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	sorttype, alphasort, timesort, sizesort;
    Panel_item	cachesize, cachetimeout;
    }
  Dirlist_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	fver, fls, fget, fput;
    }
  Clients_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item  transfer_button, write_button, options_button;
    Panel_item	batch_list;
    }
  Batch_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    }
  Generic_frame;

typedef struct
    {
    Frame	frame;
    Panel	panel;
    Panel_item	dirname, filename;
    Panel_item	apply_button, cancel_button;
    }
  Filer_frame;

/********************************************************************************/

extern void make_baseframe();
extern void make_child_frames();
extern void make_aboutfspframe(int);

#endif

/********************************************************************************/

