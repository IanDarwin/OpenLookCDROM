/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				type_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des type_edit_subframe,	*/
/*	mit dem die Liste der Knoten- und Kantentypen erweitert bzw.	*/
/*	gekuerzt werden kann.						*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_svi.h"

#include "type.h"
#include "fileselector/fileselect.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_type_edit_subframe (node_or_edge)			*/
/*	void	write_type_fileselector ( file )			*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


Frame		type_edit_subframe;


/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/

static	void delete_current_type();
static	void close_type_subframe();
static 	void	local_insert_type();
static	void make_type_panel_items();

/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/

static	Node_or_edge	what_type;
static	Fs_item		type_fileselector = (Fs_item)NULL;

/************************************************************************/
/*									*/
/*		TYPE_EDIT_SUBFRAME ERZEUGEN UND VERWALTEN		*/
/*									*/
/*	Dieser Subframe wird nur bei Bedarf erzeugt, um			*/
/*	Filedeskriptoren zu sparen.					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_type_edit_subframe (node_or_edge)			*/
/*									*/
/*	Laesst den type_edit_subframe erzeugen und setzt diverse	*/
/*	Parameter entsprechend node_or_edge.				*/
/*	node_or_edge wird in type_edit_panel in PANEL_CLIENT_DATA	*/
/*	festgehalten.							*/
/*									*/
/************************************************************************/


static	void delete_current_type()
{
	if ( what_type == NODE ) {
		delete_nodetype (current_nodetype_index);
	} else {
		delete_edgetype (current_edgetype_index);
	}
}

static	void close_type_subframe()
{
	fls_close( type_fileselector );
	unlock_user_interface();
}

static 	void	local_insert_type( dir, file )
char *dir, *file;
{
	char	filename [FILENAMESIZE],
		type_id  [FONT_ID_SIZE];

	if (!strcmp( dir, "" ) && !strcmp( file, "NOTHING SELECTED" )){	/* Abort has been selected */
		unlock_user_interface();
		return;
	}
	strcpy (filename, dir);
	if (strcmp(filename, "") && strcmp(filename, "/")){
		strcat (filename, "/");
	}
	
	if (!strcmp (file, "")) {
		error ("Please give a filename !\n");
		return;
	} else {
		if( file[0] == SYSTEM_TYPES_IDENTIFICATION_CHARACTER ) {
			filename[0] = '\0';
		}
	}
	strcat(filename, file);
	
	if (!check_file_is_single (filename)){
		return;
	}
			
	if (  (what_type == NODE && find_nodetype(filename) != -1) ||
	      (what_type == EDGE && find_edgetype(filename) != -1) )  {
	
		error ("This type is already loaded !");
		if (what_type == NODE)
			set_current_nodetype (find_nodetype(filename));
		if (what_type == EDGE)
			set_current_edgetype (find_edgetype(filename));
			
	} else {

		if (what_type == NODE) {
			add_nodetype (filename);
		} else {
			add_edgetype (filename);
		}
	}
	
	unlock_user_interface();
}

static	void make_type_panel_items( panel )
Panel	panel;
{
	
	(void)xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_X,		xv_col(panel,2),
		PANEL_LABEL_Y,		xv_row(panel,0),
		PANEL_LABEL_STRING,	"delete current type",
		PANEL_NOTIFY_PROC,	delete_current_type,
		NULL);

	(void)xv_create( panel, PANEL_BUTTON,
		PANEL_LABEL_X,		xv_col(panel,48),
		PANEL_LABEL_Y,		xv_row(panel,0),
		PANEL_LABEL_STRING,	"done",
		PANEL_NOTIFY_PROC,	close_type_subframe,
		NULL);
				
	window_fit_height( panel );
}


static int	first = TRUE;


void		init_type_fileselector()
{
	extern	char	*get_existing_fileselector_startup_filename ();
	
	if( first ) {
		first = FALSE;
		type_fileselector = fls_create();
		if( type_fileselector != NULL ) {
			fls_setup_from_file( type_fileselector, get_existing_fileselector_startup_filename(), "edit_types" );
			type_fileselector->ok_implies_close = FALSE;
			fls_set_user_panel_items_create_proc( type_fileselector, make_type_panel_items );
		}
	}
}


void		show_type_edit_subframe (node_or_edge)
Node_or_edge	node_or_edge;
{
	extern	char	*get_existing_fileselector_startup_filename ();
	
	lock_user_interface ();
	what_type = node_or_edge;
	init_type_fileselector ();
	if( what_type == NODE ) {
		fls_set_info( type_fileselector, " << ADD NODETYPES >>" );
	} else {
		fls_set_info( type_fileselector, " << ADD EDGETYPES >>" );
	}
	fileselect( type_fileselector, base_frame, local_insert_type );
}

void	write_type_fileselector( file )
FILE	*file;
{
	init_type_fileselector ();
	if (type_fileselector != NULL) {
		fls_write_to_file( type_fileselector, file, "edit_types" );
		fls_close( type_fileselector );
	}
}
