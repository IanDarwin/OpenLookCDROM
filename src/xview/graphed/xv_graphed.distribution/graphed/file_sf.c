/* (C) Universitaet Passau 1986-1991 */
#include "misc.h"
#include "graph.h"
#include "graphed_subwindows.h"

#include "load.h"
#include "store.h"

#include "user.h"

#include "fileselector/fileselect.h"

typedef	enum {
	LOAD_STORE_VIA_STDINOUT = 0, /* Abhaengig von der Reihenfolge	*/
	LOAD_STORE_VIA_FILE     = 1  /* in file_selection_mode_cycle !	*/
}
	Load_store_mode;

static	Load_store_mode	saved_mode_selection = LOAD_STORE_VIA_FILE;

typedef	enum	{
	STORE_LAST_GRAPH,
	STORE_ALL_GRAPHS,
	
	NUMBER_OF_STORE_WHATS,	/* Dummy	*/
}
	Store_what;

static	char	*store_what_strings [(int)NUMBER_OF_STORE_WHATS] = {
		"Store current Graph ",
		"Store all Graphs "
};

static	Panel		file_selection_panel;
static	Panel_item	file_selection_mode_cycle;
static	Panel_item	file_selection_store_what_cycle;
static	Fs_item	fileselector = (Fs_item)NULL;
static	Load_or_store	what_are_we_doing;

static void	local_load();
static void	local_store();

static void	nf_stdinout( item, event )
Panel_item	item;
Event		*event;
{
	saved_mode_selection = LOAD_STORE_VIA_STDINOUT;
	fls_close( fileselector );
	if( what_are_we_doing == STORE ) {
		local_store( " ","" );
	} else {
		local_load( " ","" );
	}
	saved_mode_selection = LOAD_STORE_VIA_FILE;
}

static void	create_graphed_fileselect_panel( panel )
Panel	panel;
{	
	file_selection_panel = panel;
	
	if( what_are_we_doing == STORE ) {
		file_selection_mode_cycle = xv_create(file_selection_panel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC,	nf_stdinout,
			PANEL_LABEL_STRING,	"Store to stdout",
			PANEL_LABEL_X,		xv_col(file_selection_panel,5),
			PANEL_LABEL_Y,		xv_row(file_selection_panel,0),
			NULL);
		file_selection_store_what_cycle = xv_create(file_selection_panel, PANEL_CHOICE,
			PANEL_LABEL_STRING,	"Mode      : ",
			PANEL_LABEL_X,		xv_col(file_selection_panel,5),
			PANEL_LABEL_Y,		xv_row(file_selection_panel,1),
/*			PANEL_LAYOUT,		PANEL_VERTICAL,
			PANEL_LABEL_BOLD,	TRUE,
			PANEL_CHOICES_BOLD,	TRUE,
*/			PANEL_CHOICE_STRINGS,	store_what_strings [(int)STORE_LAST_GRAPH],
						store_what_strings [(int)STORE_ALL_GRAPHS],
						NULL,
			NULL);
			
		if (buffers[wac_buffer].graphs != empty_graph &&
		    buffers[wac_buffer].graphs->suc != buffers[wac_buffer].graphs) {
		 	
			xv_set(file_selection_store_what_cycle, PANEL_VALUE, STORE_ALL_GRAPHS, NULL);
		} else {
			xv_set(file_selection_store_what_cycle, PANEL_VALUE, STORE_LAST_GRAPH, NULL);
		}
	} else { /* we're loading */
		file_selection_mode_cycle = xv_create(file_selection_panel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC,	nf_stdinout,
			PANEL_LABEL_STRING,	"Load from stdin",
			PANEL_LABEL_X,		xv_col(file_selection_panel,5),
			PANEL_LABEL_Y,		xv_row(file_selection_panel,0),
			NULL);
/*		file_selection_mode_cycle = xv_create(file_selection_panel, PANEL_CHOICE,
			PANEL_LABEL_STRING,	"Load from : ",
			PANEL_LABEL_X,		xv_col(file_selection_panel,5),
			PANEL_LAYOUT,		PANEL_VERTICAL,
			PANEL_CHOICE_STRINGS,	"stdin               ", "selected file ", 0,
			PANEL_VALUE,		saved_mode_selection,
			NULL);
*/	}
	window_fit_height(file_selection_panel);
}




static void	local_load( dir, file)
char	*dir, *file;
{
	char		name[WDLEN+FNLEN+1];
	Load_store_mode	mode;
	
	if (!strcmp( dir, "" ) && !strcmp( file, "NOTHING SELECTED" )){	/* Abort has been selected */
		unlock_user_interface();
		return;
	}
	
	if (get_currently_edited_node() != empty_node ||
	    get_currently_edited_edge() != empty_edge) {
		bell ();
		unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
		return;
	}
	
	mode = saved_mode_selection;
	if ( mode == LOAD_STORE_VIA_STDINOUT ) {
		strcpy( name, "" );
	} else {
		if (!strcmp( dir, "" )) {
			if( !strcmp( file, "" ) ) {
				unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
				return;
			} else {
				strcpy( name, file );
			}
		} else {
			if (!strcmp( dir, "/" )) {
				sprintf( name, "/%s", file );
			} else {
				sprintf(name, "%s/%s", dir, file);
			}
		}
	}
	load(wac_buffer, name );
	
	unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
}

static void	local_store( dir, file)
char	*dir, *file;
{
	char		name[WDLEN+FNLEN+1];
	Load_store_mode	mode;
	
	if (!strcmp( dir, "" ) && !strcmp( file, "NOTHING SELECTED" )){	/* Abort has been selected */
		unlock_user_interface();
		return;
	}
	
	if (get_currently_edited_node() != empty_node ||
	    get_currently_edited_edge() != empty_edge) {
		bell ();
		unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
		return;
	}
	
	mode = saved_mode_selection;
	if ( mode == LOAD_STORE_VIA_STDINOUT ) {
		strcpy( name, "" );
	} else {
		if (!strcmp( dir, "" )) {
			if( !strcmp( file, "" ) ) {
				unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
				return;
			} else {
				strcpy( name, file );
			}
		} else {
			if (!strcmp( dir, "/" )) {
				sprintf( name, "/%s", file );
			} else {
				sprintf(name, "%s/%s", dir, file);
			}
		}
	}
	
	if ((Store_what)xv_get(file_selection_store_what_cycle, PANEL_VALUE) == STORE_LAST_GRAPH) {
		if (get_last_graph() == empty_graph &&
		    buffers[wac_buffer].graphs != empty_graph &&
		    buffers[wac_buffer].graphs != buffers[wac_buffer].graphs->suc) {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Please klick a node or a edge belonging to the desired graph.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
			unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
			dispatch_user_action (SELECT_MODE);
			return;
		}
	}
	
	if ( 	strcmp( name, "")  &&
		file_exists(name)  && 
		NOTICE_NO == notice_prompt (base_frame, NULL,		/*fisprompt*/
			NOTICE_MESSAGE_STRINGS,	"Please klick a node or a edge belonging to the desired graph.", NULL,
			NOTICE_BUTTON_YES,	"Ok",
			NOTICE_BUTTON_NO,	"Cancel",
			NULL)
	   ) {
		unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
		return;
	}
	
	if ((Store_what)xv_get(file_selection_store_what_cycle, PANEL_VALUE) == STORE_LAST_GRAPH){
		if (get_last_graph() != empty_graph){
			store_graph (get_last_graph(), name);
		} else {
			/* buffers[wac_buffer] ist nach obiger Abfrage einelementig !	*/
			store_graph (buffers[wac_buffer].graphs, name);
		}
	} else {
		store_graphs (name);
	}
	
	unlock_user_interface(); /* nur, wenn fileselector-box verschwindet ! */
}


static	int	created = FALSE;


void	init_file_fileselector ()
{
	extern	char	*get_existing_fileselector_startup_filename();
	
	if( !created ) {
		created = TRUE;
		fileselector = fls_create();
		fls_set_user_panel_items_create_proc( fileselector, create_graphed_fileselect_panel );
		fls_setup_from_file( fileselector, get_existing_fileselector_startup_filename(), "load_store" );
	}

}

void		show_file_selection_subframe (load_or_store)
Load_or_store	load_or_store;
{
	extern	char	*get_existing_fileselector_startup_filename();
	
	init_file_fileselector ();
	
	lock_user_interface ();
	what_are_we_doing = load_or_store;
	
	if (load_or_store == LOAD) {
		fls_set_info( fileselector, " << LOAD GRAPH : >>" );
		fileselect( fileselector, base_frame, local_load );
	} else {
		fls_set_info( fileselector, " << STORE GRAPH : >>");
		fileselect( fileselector, base_frame, local_store );
	}
}

void	write_load_store_fileselector( file )
FILE	*file;
{
	init_file_fileselector ();
	fls_write_to_file( fileselector, file, "load_store" );
	fls_close (fileselector);
}
