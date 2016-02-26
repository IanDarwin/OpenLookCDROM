/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				load.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Prozedur load zum Laden von Graphen.	*/
/*	WICHTIG : Das eigentliche Laden wird im Parser yyparse ()	*/
/*	(-> scanner.l, parser.y) beschrieben.				*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "load.h"
#include "draw.h"

#include "graphed_subwindows.h"
#include "menu.h"
#include "user.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	load          (buffer, filename)			*/
/*	void	set_lex_input (file)					*/
/*	void	set_filename  (filename)				*/
/*	char	*get_filename ()					*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			GLOBALE VARIABLE				*/
/*									*/
/************************************************************************/


int	lex_input_file_linenumber    = 0;
int	overwrite_state = TRUE;

int	load_buffer;

/************************************************************************/
/*									*/
/*			LADEN VON GRAPHEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	load (buffer, filename)					*/
/*									*/
/*	Laedt einen neuen Graphen (mittels yyparse) aus filename.	*/
/*	in <buffer>.							*/
/*	Ist filename == "", so wird von stdin eingelesen.		*/
/*	Rueckmeldung ist TRUE, falls das Laden erfolgreich war.		*/
/*	WICHTIG : Ein Fehler beim Einlesen hat zur Folge, dass der	*/
/*	ganze bis dahin eingelesene Graph (im Parser) geloescht wird.	*/
/*									*/
/*	load aktiviert bzw. deaktiviert im Menue die Punkte		*/
/*	LOAD_AGAIN und STORE_TO_SAME_FILE, falls das			*/
/*	Laden erfolgreich bzw. erfolglos verlaufen ist.			*/
/*	Der bisherige Graph wird geloescht und (bei erfolgreichem	*/
/*	Laden) im Label des base_frame der neue Filename eingetragen.	*/
/*	Load_graph oeffnet und schliesst die Datei selbststaendig	*/
/*	Wichtige Ausnahme : stdin).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_lex_input (file)					*/
/*									*/
/*	Setzt die Eingabe von yylex() (= yyin) auf file. Die		*/
/*	Zeilennummer lex_input_file_linenumber wird auf 1		*/
/*	zurueckgesetzt.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_filename (filename)					*/
/*									*/
/*	Setzt den Filenamen, im aktuellen buffer und im Label des	*/
/*	frame.								*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*get_filename ()					*/
/*									*/
/*	Gibt einen Zeiger auf den Filenamen aus dem aktuellen buffer	*/
/*	zurueck.							*/
/*									*/
/************************************************************************/


int	load (buffer, filename)
int	buffer;
char	*filename;
{	
	FILE	*file;
	char	*fname, fn[FILENAMESIZE];
	int	loading_successful;
	
	if (!strcmp(filename, "")) {
		message ("Loading graph from stdin\n");
		file = stdin;
	} else {
		if (filename == NULL) {
			error ("Can't find file %s anywhere\n", filename);
			return FALSE;
		} else {
			if ((fname = file_exists_somewhere (filename, getenv ("GRAPHED_INPUTS"))) != NULL) {
				strcpy (fn, fname);
			} else {
				error ("Can't get file %s\nFile does not exist or too many files match pattern\n", filename);
				return NULL;
			}
			if ( (file = fopen (fn, "r")) == (FILE *)NULL) {
				error ("Can't open file %s\n", fn);
				sys_error (errno);
				inactivate_menu_item (LOAD_AGAIN);
				inactivate_menu_item (STORE_TO_SAME_FILE);
				return FALSE;
			} else
				message ("Loading graph from file %s\n", fn);
		}
	}
	
	load_buffer = buffer;
	graphed_state.loading = TRUE;
	set_last_graph (empty_graph);
	delete_graphs_in_buffer (wac_buffer);
	
	set_lex_input (file);
	
	loading_successful = (yyparse() == 0);	/* jetzt wird geladen	*/
	
	redraw_all       (); /* Jetzt zeichnen. Damit erspart man sich	*/
	                     /* eine grosse global_repaint_rectlist	*/
	force_repainting ();
	
	if (file != stdin) fclose (file);
	
	if (loading_successful) {
		set_filename (fn);
		activate_menu_item (LOAD_AGAIN);
		activate_menu_item (STORE_TO_SAME_FILE);
	} else {
		set_filename ("");
		inactivate_menu_item (LOAD_AGAIN);
		inactivate_menu_item (STORE_TO_SAME_FILE);
	}
	
	if (buffers[wac_buffer].graphs != empty_graph &&
	    buffers[wac_buffer].graphs == buffers[wac_buffer].graphs->suc) {
		set_last_graph (buffers[wac_buffer].graphs->suc);
	}
	
	graphed_state.loading = FALSE;
	return loading_successful;
}



void	set_lex_input (file)
FILE	*file;
{
	extern	FILE	*yyin;	/* Aus der Datei scanner.c, die von lex	*/
				/* erzeugt wird				*/
	
	yyin = file;
	lex_input_file_linenumber    = 1;
}


void	set_filename (filename)
char	*filename;
{
	if (buffers[wac_buffer].filename != NULL)
		myfree (buffers[wac_buffer].filename);
	
	buffers[wac_buffer].filename = strsave (filename);
	
	set_canvas_frame_label (wac_buffer);
}


char	*get_filename ()
{
	return buffers[wac_buffer].filename;
}
