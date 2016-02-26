/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/seln.h>
#include <sys/param.h>
#include <ctype.h>
 
#include <sys/wait.h>
#include <sys/stat.h>

#include "defs.h"

#include "openp.h"
/*
	local	vars
*/
extern	struct	card	*current, *first ,*last ;
extern	struct	card	*make_card() ,*insert_card();
extern	struct	card	*undelete_card();
extern	int	load_file_action;
extern	int	need_save;

dbl_clck_list_proc(item, string, client_data,op,event)
Panel_item item;
char	*string;
caddr_t *client_data;
Panel_list_op op;
Event	*event;
{
static	struct timeval last_click=0;
struct  timeval click_time;
static	caddr_t	*last_choice=0;
long     dclick_usec;

	if (last_choice != client_data)
	{
		last_choice = client_data;
		last_click = event->ie_time;
		return;
	}
	if (( op == PANEL_LIST_OP_SELECT)
		|| (op == PANEL_LIST_OP_DESELECT))
	{
	       /* if the click was within a 
		   certain time of the last 
		   left click, it is a shorthand 
		   for show.
		   */
		dclick_usec = 100000 * (int) defaults_get_integer("MultiClickTimeout", "MultiClickTimeout", 4);
		click_time = event->ie_time;
		if (click_time.tv_sec == last_click.tv_sec)
		{
			if ((click_time.tv_usec - last_click.tv_usec) < dclick_usec)
			{
				do_show((struct  card *)client_data);
				deselect_all_but_one(item,current->c_num);
			}
		}
			else if (click_time.tv_sec == (last_click.tv_sec + 1))
		{
			if (((1000000 - last_click.tv_usec) + click_time.tv_usec) < dclick_usec)
			{
				do_show((struct  card *)client_data);
				deselect_all_but_one(item,current->c_num);
			}
		}
	}
	last_choice = client_data;
	last_click = event->ie_time;
}

do_show(p)
struct  card *p;
{
	save_card(current);
	if (p != NULL_CARD)	 show_card(p);
}

deselect_all_but_one(item,n)
Panel_item item;
{
	int i,j = xv_get(item,PANEL_LIST_NROWS,0);
	n--;
	for(i=0;i<j;i++)
		if ((i != n)&&xv_get(item,PANEL_LIST_SELECTED,i,0))
			xv_set(item,PANEL_LIST_SELECT,i,FALSE,0);
}

/*
 *	The following code deals with drag n drop
 */
Notify_value	drop_text_eventproc();
Notify_value	drop_file_eventproc();
Xv_window       pop_file_paintwin; /*saved to avoid confusion later */

/*
	Init drag n drop events for the application.
*/
init_selections( panel)
Panel	panel;
{
		/* drag and drop events for the rolodex */
	notify_interpose_event_func( (Xv_Window)
		xv_get(panel, CANVAS_NTH_PAINT_WINDOW, 0),
		drop_file_eventproc, NOTIFY_SAFE); 
	pop_file_paintwin =
		xv_get((Panel)
			xv_get( pop_file_frame , FRAME_CMD_PANEL, 0 ),
			CANVAS_NTH_PAINT_WINDOW, 0);
	notify_interpose_event_func( pop_file_paintwin,
		drop_file_eventproc, NOTIFY_SAFE); 

		/* drag and drop events for the rolocard */
	notify_interpose_event_func(textsw_first(rolocard),
		drop_text_eventproc, NOTIFY_SAFE); 
}

/*
	Dragged and dropped a file in the rolo card window
	fill the card with contents of the file.
*/
Notify_value
drop_text_eventproc(win, evnt, arg, type)
Xv_window       win;
Event           *evnt;
Notify_arg      arg;
Notify_event_type       type;
{
int     action;
char	sel_import[500];
        switch (action = event_action(evnt))
        {
		case ACTION_DRAG_LOAD:	/* load file */
			get_seln(sel_import);
			show_file(sel_import);
			break;
		default:		/* let them thru to be
						processed */
			return (notify_next_event_func(win, evnt, arg, type));
	}
	return(0);	/* don't do anything else */
}

/*
	user dragged and dropped a file into the panel or the
	file pop frame.  Load the rolofile into the the rolodex.
*/
Notify_value
drop_file_eventproc( win, evnt, arg, type)
Xv_window       win;
Event           *evnt;
Notify_arg      arg;
Notify_event_type       type;
{
int     action;
char	sel_import[300];
char	*get_path_n_name(), *name, path[300];

        switch (action = event_action(evnt))
        {
		case ACTION_DRAG_LOAD:	/* load file */
			get_seln(sel_import);
			break;
		default:		/* let them thru to be
						processed */
			return (notify_next_event_func(win, evnt, arg, type));
	}
	if ( win == pop_file_paintwin && ! load_file_action)
		return(0);	/* dumped into a save file window */
	if (!((need_save == TRUE) && (verify_no_save () == FALSE)))
	{
		save_card(current);
		xv_set(rolo_frame, FRAME_BUSY, TRUE, 0);
		name = get_path_n_name(sel_import,path);
		xv_set( pop_file_path_item, PANEL_VALUE, path,0);
		xv_set( pop_file_name_item, PANEL_VALUE, name, 0);
		read_rolo( sel_import);	/* full name */
		xv_set(rolo_frame, FRAME_BUSY, FALSE, 0);
	}
}

/*
	Read the current primary selection
*/
get_seln(data)
char	*data;
{
	Seln_holder	holder;
	Seln_request	*buffer;
	
	holder = seln_inquire(SELN_PRIMARY);
	buffer = seln_ask( &holder, SELN_REQ_CONTENTS_ASCII, 0, 0);
	strcpy( data, ((buffer->data) + sizeof(Seln_attribute))  );
}

/*
	fill the current rolofile with the contents of a file
*/
show_file(filename)
char	*filename;
{
FILE	*fp, *fopen();
char	tmp[1000];
int	size;
	textsw_reset( rolocard, 0 , 0 );
	if ( fp = fopen( filename,"r"))
	{
		while ( size = fread(tmp, 1, 1000,fp))
			textsw_insert(rolocard, tmp,size);
		fclose(fp);
	}
	else
		fprintf(stderr,"cannot read %s\n",filename);
}

/* 
	Separate the filename and the path name from a name.
*/
char	*
get_path_n_name(text,path)
char	*text,*path;
{
char	*t1;
	strcpy(path,text);
	t1 = path + strlen(path) - 1;	/* last char */
	for (; t1 != path; t1 -- )
		if ( *t1 == '/')
		{
			*t1 = '\0';   /* null terminate the path */
			return(t1+1);	/* name */
		}
	return(text);	/* the whole name */
}


/*
 *	Panel list maintanance routines
 *	for undel_list_item and list_item
 */
/*
	Remove all selected card from delete list and
		re-insert it into the active list
*/
undel_card(before)
{
struct  card    *p;
int     i, total;
	total = (int)xv_get( undel_list_item, PANEL_LIST_NROWS, 0);
	save_card(current);
	for (i = 0; i < total ; i++)
	{
		if (!(int)xv_get(undel_list_item,PANEL_LIST_SELECTED,i,0))
			continue;
		p = (struct card *)xv_get(undel_list_item, PANEL_LIST_CLIENT_DATA,i,0);
		if (p == NULL_CARD )    continue;
		unstack_card(p); 
		(void) insert_card (p, (before == TRUE) ? current->c_prev : current); 
		need_save = TRUE; 
		set_slider_max (renumber (first)); 
		undel_insert_list(p,i);
		show_card (p); 
		--i; 		/* look at the next card, renumbered */
		--total ;	/* one fewer in the list */
	}
}
/*
	Insert an entry in the active card list
	call only when the cards are numbered correctly(renumber()).
*/
insert_card_list(p)
struct card *p;
{
char    line_buf [MAX_INDEX_LINE + 2], *nl;

	(void) strncpy(line_buf, first_char (p->c_text),MAX_INDEX_LINE);
	line_buf [MAX_INDEX_LINE] = 0;  /* make sure it's terminated */
	(void) strcat (line_buf, "\n"); /* make sure of newline */
	nl = index (line_buf, '\n');
	*++nl = '\0';                   /* chop at first line break */
	xv_set( list_item, PANEL_LIST_INSERT, p->c_num-1 , 0);
	xv_set( list_item, PANEL_LIST_STRING, p->c_num-1, line_buf, 0 );
	xv_set( list_item, PANEL_LIST_CLIENT_DATA, p->c_num-1 ,p,0);/* for later */
}

/*
	Entry already created there, label the new slot.
	call only when the cards are numbered correctly(renumber()).
*/
update_card_list(p)
struct card *p;
{
char    line_buf [MAX_INDEX_LINE + 2], *nl;
	(void) strncpy(line_buf, first_char (p->c_text),MAX_INDEX_LINE);
	line_buf [MAX_INDEX_LINE] = 0;  /* make sure it's terminated */
	(void) strcat (line_buf, "\n"); /* make sure of newline */
	nl = index (line_buf, '\n');
	*++nl = '\0';                   /* chop at first line break */
	xv_set( list_item, PANEL_LIST_STRING, p->c_num-1 ,line_buf, 0 );
	xv_set( list_item, PANEL_LIST_CLIENT_DATA, p->c_num-1 ,p,0);
}

/*
	Insert card from list of active cards into list of deleted cards
	call after renumber()	by delete_card()
*/
extern	struct	card	*dead;
del_insert_list(p, blank)
struct card *p;
int	blank;
{
	char  	*line_buf; /*  line_buf [MAX_INDEX_LINE + 2]; */

	if (! blank )
	{
		line_buf = (char *)
			xv_get(list_item, PANEL_LIST_STRING, p->c_num - 1 , 0);
		/* enter in list of deleted cards */
		/*  at the top of the list */
		xv_set( undel_list_item, PANEL_LIST_INSERT,0, 0);
		xv_set( undel_list_item, PANEL_LIST_STRING,0,line_buf, 0 );
		xv_set( undel_list_item, PANEL_LIST_CLIENT_DATA,0,p,0);
	}
		/* delete from active list */
	xv_set( list_item, PANEL_LIST_DELETE, p->c_num -1 , 0);
}

/*
	Insert card from list of deleted cards into active list
	call after renumber() by all undelete routines
				after calling undelete_card()
	p->c_num are assumed to be correct addresses.
	addr is the actual address of the card in the delete list
*/
undel_insert_list(p, addr)
struct card *p;
int	addr;
{
char    line_buf [MAX_INDEX_LINE + 2], *nl;
	nl = (char *) xv_get( undel_list_item, PANEL_LIST_STRING, addr );
		/* insert into active list */
	xv_set( list_item, PANEL_LIST_INSERT,p->c_num-1, 0);
	xv_set( list_item, PANEL_LIST_STRING,p->c_num-1,nl, 0 );
	xv_set( list_item, PANEL_LIST_CLIENT_DATA,p->c_num-1,p,0);
		/* delete from undelete list */
	xv_set( undel_list_item, PANEL_LIST_DELETE,addr ,0 );
}

remove_top_card_from_list()
{
	xv_set( list_item, PANEL_LIST_DELETE,0 ,0 );
}

/*
	deletes all the selected cards in the active list
*/
struct card *
delete_from_active_list()
{
	struct	card	*p = NULL ,
			*next = (struct card *)NO_ACTION;
	int	i, deleted_current = 0;
	int total = last->c_num;
	for (i = 0; i < total ; i++)
	{
		if (!(int)xv_get(list_item,PANEL_LIST_SELECTED,i,0))
			continue;	/* if not selected */
		p = (struct card *)xv_get(list_item, PANEL_LIST_CLIENT_DATA,i,0);
		if (p == NULL_CARD )    continue; /* bad error */
		next = (p == last)?p->c_prev : p->c_next;
		if (p == current) /* if current card is deleted */
			deleted_current = 1;
		delete_card (p);
		set_slider_max (renumber (first)); 
		--i; 		/* look at the next card, renumbered */
		--total ;	/* one fewer in the list */
	}
	if ( deleted_current)
		return(next);	/* card next to the last selected card*/
	else
		return(current);/* show the same card as before */
}
/*
	shows the active list : for debugging only
*/
debug_list(item , from_where)
Panel_item	item;
char		*from_where;
{
int	i,n;
char	*c;
	n = (int)xv_get(item, PANEL_LIST_NROWS,0);
	for(i=0;i<n;i++)
	{
		c = (char *)xv_get( item, PANEL_LIST_STRING,i ,0 );
		(void)fprintf(stderr,"debug-%s>%d: %s\n",from_where,i,c);
	}
}

