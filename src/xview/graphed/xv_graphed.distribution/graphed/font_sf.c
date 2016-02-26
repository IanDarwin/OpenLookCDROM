/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				font_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des font_edit_subframe,	*/
/*	mit dem die Liste der Zeichensaetze erweitert bzw. gekuerzt	*/
/*	werden kann.							*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_svi.h"

#include "font.h"
#include "fileselector/fileselect.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_font_edit_subframe (node_or_edge)			*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/

Frame		font_edit_subframe;


/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/


static	void	create_font_edit_subframe     ();
static		notify_font_edit_buttons      ();
static		notify_font_id_selection      ();
static	char	*derive_font_id_from_filename ();


/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/

static	Node_or_edge	what_font;
static	Fs_item		font_fileselector = (Fs_item)NULL;

/************************************************************************/
/*									*/
/*			FONT_EDIT_SUBFRAMEVERWALTEN			*/
/*									*/
/*	Dieser Subframe wird nur bei Bedarf erzeugt, um			*/
/*	Filedeskriptoren zu sparen.					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_font_edit_subframe (node_or_edge)			*/
/*									*/
/*	Laesst den font_edit_subframe erzeugen und bereitet die		*/
/*	Anzeigen entsprechend node_or_edge auf.				*/
/*	node_or_edge wird in font_selection_panel in PANEL_CLIENT_DATA	*/
/*	festgehalten.							*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	char	*derive_font_id_from_filename (filename)	*/
/*									*/
/*	Spezialprozedur fuer notify_font_id_selection_item : aus	*/
/*	filename wird der Dateiname ohne Pfad herausgesucht.		*/
/*	ACHTUNG : Rueckgabe ist ein Zeiger auf einen Teilstring von	*/
/*	Filename !							*/
/*									*/
/************************************************************************/

static	char	*derive_font_id_from_filename (filename)
char		*filename;
{
	char	*possible_id;
	
	possible_id = strrchr (filename, '/');
		
	return iif (possible_id != NULL, possible_id+1, filename);
}

static	void delete_current_font()
{
	if ( what_font == NODE ) {
		delete_font (current_nodefont_index);
	} else {
		delete_font (current_edgefont_index);
	}
}

static	void close_font_subframe()
{
	fls_close( font_fileselector );
	unlock_user_interface();
}

static 	void	local_insert_font( dir, file )
char *dir, *file;
{
	char	filename [FILENAMESIZE],
		font_id  [FONT_ID_SIZE];

	if (!strcmp( dir, "" ) && !strcmp( file, "NOTHING SELECTED" )){	/* Abort has been selected */
		unlock_user_interface();
		return;
	}
	strcpy (filename, dir);
	if (strcmp (filename, "")){
		strcat (filename, "/");
	}
	strcat(filename, file);
	
	strcpy (font_id, derive_font_id_from_filename (filename));
	if (!strcmp (filename, "")) {
		error ("Please give a filename !\n");
		return;
	}
	if (!check_file_is_single (filename)){
		return;
	}
			
	if ( (find_font(filename, font_id) != -1) ) {
	
		int	set_nodefont_and_edgefont_indices =
			current_nodefont_index == current_edgefont_index;
		
		error ("This font is already loaded !\n");
		if (what_font == NODE || set_nodefont_and_edgefont_indices)
			set_current_nodefont (find_font(filename, font_id));
		if (what_font == EDGE || set_nodefont_and_edgefont_indices)
			set_current_edgefont (find_font(filename, font_id));
			
	} else {

		add_font (filename, font_id);

	}

	unlock_user_interface();
}

static	void make_font_panel_items( panel )
Panel	panel;
{
	
	(void)xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_X,		xv_col(panel,2),
		PANEL_LABEL_Y,		xv_row(panel,0),
		PANEL_LABEL_STRING,	"delete current font",
		PANEL_NOTIFY_PROC,	delete_current_font,
		NULL);

	(void)xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_X,		xv_col(panel,48),
		PANEL_LABEL_Y,		xv_row(panel,0),
		PANEL_LABEL_STRING,	"done",
		PANEL_NOTIFY_PROC,	close_font_subframe,
		NULL);
				
	window_fit_height(panel);
}

static int	first = TRUE;

void	init_font_fileselector ()
{
	extern	char	*get_existing_fileselector_startup_filename();
	
	if( first ) {
		first = FALSE;
		font_fileselector = fls_create();
		if( font_fileselector != NULL ) {
			fls_setup_from_file( font_fileselector, get_existing_fileselector_startup_filename(), "edit_fonts" );
			font_fileselector->ok_implies_close = FALSE;
			fls_set_user_panel_items_create_proc( font_fileselector, make_font_panel_items );
		}
	}
}

void		show_font_edit_subframe (node_or_edge)
Node_or_edge	node_or_edge;
{
	extern	char	*get_existing_fileselector_startup_filename();
	
	
	lock_user_interface ();
	what_font = node_or_edge;
	init_font_fileselector ();
	if( what_font == NODE ) {
		fls_set_info( font_fileselector, " << ADD NODEFONTS >>" );
	} else {
		fls_set_info( font_fileselector, " << ADD EDGEFONTS >>" );
	}
	fileselect( font_fileselector, base_frame, local_insert_font );
}

void	write_font_fileselector( file )
FILE	*file;
{
	init_font_fileselector ();
	if( font_fileselector != NULL ) {
		fls_write_to_file( font_fileselector, file, "edit_fonts" );
		fls_close (font_fileselector);
	}
}
