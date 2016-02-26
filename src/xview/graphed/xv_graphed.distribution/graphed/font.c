/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				font.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul verwaltet die Zeichensaetze fuer Knoten- und	*/
/*	Kantenlabel.							*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include <pixrect/pixrect_hs.h>
#include <xview/panel.h>

#include "install.h"
#include "load.h"
#include "font.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	insert_font (filename, font_id, insert_position)	*/
/*	int	add_font    (filename, font_id)				*/
/*	int	delete_font (delete_position)				*/
/*									*/
/*	Graphed_font	get_current_nodefont ()				*/
/*	Graphed_font	get_current_edgefont ()				*/
/*									*/
/*	Graphed_font	use_font       (font_index)			*/
/*	void		unuse_font     (font)				*/
/*	int		get_font_index (font)				*/
/*	int		find_font      (filename)			*/
/*									*/
/*	void		install_current_nodefont ();			*/
/*	void		install_current_edgefont ();			*/
/*									*/
/*	void		init_fonts  ()					*/
/*	void		write_fonts (file)				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		Lokale Funktionen					*/
/*									*/
/************************************************************************/


static	void	install_fontlist_for_cycle    ();



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	struct
{
	int		number_of_fonts;
	Graphed_font	list            [MAX_FONTS];
	char		*list_for_cycle [(MAX_FONTS+2)*2+1];
}
	fonts;



/************************************************************************/
/*									*/
/*		ZEICHENSAETZE - DATENSTRUKTUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Die Liste der Zeichensaetze hat folgende Datenstruktur		*/
/*									*/
/*	static	struct {						*/
/*									*/
/*		int		number_of_fonts;			*/
/*		Graphed_font	list            [MAX_FONTS];		*/
/*		char		*list_for_cycle [(MAX_FONTS+2)*2+1];	*/
/*									*/
/*	}								*/
/*		fonts;							*/
/*									*/
/*									*/
/*	MAX_FONTS	Maximalzahl an moeglichen Zeichensaetzen	*/
/*	number_of_fonts	Anzahl der vorhandenen Fonts, (nur) durch	*/
/*			MAX_FONTS beschraenkt.				*/
/*	list		Liste der Zeichensaetze.			*/
/*	list_for_cycle	Liste, mit denen die Zeichensatzliste in	*/
/*			Panel_cycle's abgebildet werden kann. Details	*/
/*			siehe unten.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	ACHTUNG : Diese Tabelle ist ausserhalb von font.c nicht		*/
/*	sichtbar. Der Zugriff erfolgt von aussen ueber die Indices	*/
/*									*/
/*	current_nodefont_index	Index des aktuellen Zeichensatzes fuer	*/
/*				Knoten(label). MAKRO !			*/
/*	current_edgefont_index	Index des aktuellen Zeichensatzes fuer	*/
/*				Kanten(label). MAKRO !			*/
/*									*/
/*	Die Indices fuer Zeichensaetze liegen in dem Intervall		*/
/*	[0..fonts.number_of_fonts-1].					*/
/*	Die "Variablen" liegen in Wirklichkeit in graph_state.		*/
/*									*/
/*	CURRENT_NODEFONT_INDEX BZW. CURRENT_EDGEFONT_INDEX KANN SICH	*/
/*	NACH INSERT_FONT ODER DELETE_FONT AENDERN !			*/
/*									*/
/*======================================================================*/
/*									*/
/*	fonts.list_for_cycle hat folgende Struktur :			*/
/*	PANEL_CHOICE_STRINGS						*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		(id'ds)							*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		NULL  (Ende Liste mit den Id's)				*/
/*	PANEL_CHOICE_FONTS						*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		(font's)						*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		NULL (Ende Liste mit den Fonts)				*/
/*	0   (Ende Liste)						*/
/*									*/
/*	Diese Liste kann an ein PANEL_CYCLE als ATTR_LIST uebergeben	*/
/*	werden. Dann erscheinen die Zeichensaetze im cycle mit den id's	*/
/*	als Beispielstrings, jeder im dazugehoerigen Font.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Ein einzelner Zeichensatz (fonts.list[?]) hat folgende		*/
/*	Struktur :							*/
/*									*/
/*	typedef	struct	graphed_font {					*/
/*									*/
/*		struct	pixfont	*font;					*/
/*		char		id      [FONT_ID_SIZE];			*/
/*		char		filename[FILENAMESIZE];			*/
/*		int		used;					*/
/*									*/
/*	}								*/
/*		*Graphed_font;						*/
/*									*/
/*	font		Zeiger auf den eigentlichen Zeichensatz		*/
/*	id		Beispielstring (fuer Darstellung in		*
/*			PANEL_CYCLE					*/
/*	filename	Hier liegt der Zeichensatz als Datei.		*/
/*	used		Gibt an, wie oft (in wie vielen Knoten/Kanten)	*/
/*			der Zeichensatz verwendet wird. Nur wenn	*/
/*			used == 0, darf der Zeichensatz geloescht	*/
/*			werden.						*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*		ZEICHENSAETZE EINFUEGEN UND LOESCHEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	int	insert_font (filename, font_id, insert_position)	*/
/*									*/
/*	Fuegt einen Zeichensatz aus den File filename mit font_id	*/
/*	in die Liste der Zeichensaetze ein. Rueckgabe ist der Index, an	*/
/*	dem der Zeichensatz tatsaechlich eingefuegt wurde.		*/
/*	Strategie :							*/
/*	- gibt es den Zeichensatz (font, font_id) schon, so melde nur	*/
/*	  seinen Index (insert_position wird in diesem Fall ausser	*/
/*	  Acht gelassen.). Sonst :					*/
/*	- ist insert_position < fonts.number_of_fonts, so fuege vor	*/
/*	  insert_position ein						*/
/*	- ist insert_position = fonts.number_of_fonts, so haenge an	*/
/*	  die Liste an							*/
/*									*/
/*	Kann die Datei unter filename nicht gefunden werden und		*/
/*	ist filename kein absoluter Pfad, so wird in den im		*/
/*	Environment unter GRAPHED_INPUTS angebenenen Directories	*/
/*	nachgesehen.							*/
/*									*/
/*	Falls schon zu viele Zeichensaetze da sind oder das File nicht	*/
/*	geoeffnet werden kann, wird eine Fehlermeldung ausgegeben und	*/
/*	anstatt eines Index -1 zurueckgegeben.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	add_font (filename, font_id)				*/
/*									*/
/*	= insert_font (filename, font_id, fonts.number_of_fonts), d.h.	*/
/*	Anhangen an die Liste, wenn nicht schon vorhanden.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	delete_font (delete_position)				*/
/*									*/
/*	Loescht den Zeichensatz mit Index delete_position.		*/
/*	Strategie :							*/
/*	- delete_position == current_...font_index > 0 :		*/
/*	  neuer current_....font_index wird der Zeichensatz, der vor	*/
/*	  delete_position liegt						*/
/*	- delete_position == current_...font_index = 0 :		*/
/*	  neuer current_....font_index wird der neue erste Zeichensatz	*/
/*	- sonst : "normales" Loeschen aus der Liste.			*/
/*									*/
/*	Ist nur noch ein Font uebrig, so darf dieser sinnvollerweise	*/
/*	nicht auch noch geloescht werden. Dann wird eine Fehlermeldung	*/
/*	ausgeben und FALSE zurueckgegeben, ansonsten natuerlich TRUE.	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	ACHTUNG * WICHTIG * ACHTUNG * WICHTIG * ACHTUNG * WICHTIG	*/
/*									*/
/*	Alle Prozeduren lassen den current_nodefont bzw.		*/
/*	current_edgefont unveraendert. Dazu ist es notwendig, bei	*/
/*	insert_font bzw. delete_font ggf. den current_nodefont_index	*/
/*	bzw. current_edgefont_index aendern.				*/
/*									*/
/************************************************************************/



int	insert_font (filename, font_id, insert_position)
char	*filename;
char	*font_id;
int	insert_position;
{
	extern	Pixfont	*pf_open();

	Graphed_font	new_font;
	char		*extended_filename;
	int		i;


	if ((i = find_font (filename, font_id)) != -1)
		return i;
	if (fonts.number_of_fonts == MAX_FONTS) {
		error ("Can't insert a new font - too many fonts\n");
		return (-1);
	}

	/* Erzeuge und besetze zunaechst den Zeichensatz selbst	*/
	new_font = (Graphed_font) mymalloc(sizeof(struct graphed_font));
	extended_filename = file_exists_somewhere (filename,
		getenv ("GRAPHED_INPUTS"));
	if (extended_filename == NULL) {
		myfree (new_font);
		error ("Cannot find file %s anywhere\n", filename);
		return (-1);
	} else if ( (new_font->font = 
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_open instead Remember to extern it
#endif
pf_open(extended_filename)) ==
	            (struct pixfont *)NULL ) {
		error ("Can't read font file %s\n", extended_filename);
		return (-1);
	}
	new_font->used = 0;
	strcpy (new_font->id       = mymalloc (strlen(font_id)+1),  font_id);
	strcpy (new_font->filename = mymalloc (strlen(filename)+1), filename);

	/* Verschiebe fonts.list und fonts.list_for_cycle, um Platz zu	*/
	/* machen							*/
	/* Pity poor dog who has to change the next lines ...		*/
	for (i = fonts.number_of_fonts; i > insert_position; i--)
		fonts.list[i] = fonts.list[i-1];
	fonts.list[insert_position] = new_font;
	for (i = (fonts.number_of_fonts+2)*2+1                  +1;
	     i > (fonts.number_of_fonts+2) + insert_position+1  +1;
	     i--)
		fonts.list_for_cycle[i] = fonts.list_for_cycle[i-2];
	for (i = (fonts.number_of_fonts+2) + insert_position+1;
	     i > insert_position+1;
	     i--)
		fonts.list_for_cycle[i] = fonts.list_for_cycle[i-1];
	
	/* Trage neuen Zeichensatz in fonts ein	*/
	fonts.list_for_cycle [insert_position+1] =
		new_font->id;
	fonts.list_for_cycle
		[(fonts.number_of_fonts+2) + insert_position+1 + 1] =
		(char *)new_font->font;
	fonts.number_of_fonts ++;

	/* Installiere die neuen Listen und modifiziere evtl.	*/
	/* current_nodefont_index bzw. current_edgefont_index	*/
	install_fontlist_for_cycle ();
	if (current_nodefont_index >= insert_position)
		set_current_nodefont (current_nodefont_index + 1);
	if (current_edgefont_index >= insert_position)
		set_current_edgefont (current_edgefont_index + 1);
	for (i=0; i<NUMBER_OF_NODE_STYLES; i++) {
		Node_attributes node_attr;
		node_attr = get_node_style(i);
		if (node_attr.font_index >= insert_position) {
			node_attr.font_index++;
			set_node_style (i, node_attr);
		}
	}
	for (i=0; i<NUMBER_OF_EDGE_STYLES; i++) {
		Edge_attributes	edge_attr;
		edge_attr = get_edge_style (i);
		if (edge_attr.font_index >= insert_position) {
			edge_attr.font_index++;
			set_edge_style (i, edge_attr);
		}
	}
	
	return(insert_position);
}



int	add_font (filename, font_id)
char	*filename;
char	*font_id;
{
	return	insert_font (filename, font_id, fonts.number_of_fonts);
}



int	delete_font (delete_position)
int	delete_position;
{
	Graphed_font	old_font;
	int		i;

	if (fonts.number_of_fonts == 1) {
		error ("Only one nodefont left - Can't delete it\n");
		return (FALSE);
	}
	if (fonts.list[delete_position]->used != 0) {
		error ("This nodefont is used - Can't delete it\n");
		return (FALSE);
	}

	old_font = fonts.list[delete_position];
	
	/* Schiebe fonts.list und fonts.list_for_cycle wieder zusammen	*/
	/* Pity ...							*/
	for (i = delete_position+1; i < fonts.number_of_fonts; i++)
		fonts.list[i-1] = fonts.list[i];
	for (i = delete_position+1 +1;
	     i < (fonts.number_of_fonts+2) + delete_position+1;
	     i++)
	 	fonts.list_for_cycle[i-1] = fonts.list_for_cycle[i];
	for (i = (fonts.number_of_fonts+2) + delete_position+1 +1;
	     i < (fonts.number_of_fonts+2)*2+1;
	     i++)
	 	fonts.list_for_cycle[i-2] = fonts.list_for_cycle[i];
	fonts.number_of_fonts --;

	/* Installiere die neuen Listen und modifiziere evtl.	*/
	/* current_nodefont_index bzw. current_edgefont_index	*/
	install_fontlist_for_cycle ();
	if (current_nodefont_index >= delete_position)
		set_current_nodefont (maximum (current_nodefont_index-1,0));
	if (current_edgefont_index >= delete_position)
		set_current_edgefont (maximum (current_edgefont_index-1,0));
	for (i=0; i<NUMBER_OF_NODE_STYLES; i++) {
		Node_attributes	node_attr;
		node_attr = get_node_style(i);
		if (node_attr.font_index == delete_position) {
			node_attr.font_index = current_nodefont_index;
			set_node_style (i, node_attr);
		} else if (node_attr.font_index > delete_position) {
			node_attr.font_index = maximum (node_attr.font_index-1,0);
			set_node_style (i, node_attr);
		}
	}
	for (i=0; i<NUMBER_OF_EDGE_STYLES; i++) {
		Edge_attributes	edge_attr;
		edge_attr = get_edge_style (i);
		if (edge_attr.font_index == delete_position) {
			edge_attr.font_index = current_edgefont_index;
			set_edge_style (i, edge_attr);
		} else if (edge_attr.font_index > delete_position) {
			edge_attr.font_index = maximum (edge_attr.font_index-1,0);
			set_edge_style (i, edge_attr);
		}
	}

	/* Gib den belegten Speicher wieder frei	*/
	
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_close instead, remember to extern it
#endif
pf_close (old_font->font);
	myfree     (old_font->id);
	myfree     (old_font->filename);
	myfree     (old_font);
	
	return (TRUE);
}/************************************************************************/
/*									*/
/*		AKTUELLER KNOTEN- BZW. KANTENZEICHENSATZ		*/
/*									*/
/************************************************************************/
/*									*/
/*	Nodefont	current_used_nodefont ()			*/
/*	Edgefont	current_used_edgefont ()			*/
/*									*/
/*	Direct access to the current node- resp. edgefont		*/
/*									*/
/************************************************************************/

Graphed_font	get_current_nodefont ()
{
	return	fonts.list [current_nodefont_index];
}

Graphed_font	get_current_edgefont ()
{
	return	fonts.list [current_edgefont_index];
}


/************************************************************************/
/*									*/
/*		FUNKTIONEN FUER ZUGRIFF AUF ZEICHENSAETZE		*/
/*									*/
/************************************************************************/
/*									*/
/*	Graphed_font	use_font   (font_index)				*/
/*	void		unuse_font (font)				*/
/*									*/
/*	Melden die Benutzung eines Zeichensatzes an oder ab.		*/
/*	Der von use_font zureuckgegebene Font muss bei unuse_font als	*/
/*	Argument uebergeben werden.					*/
/*	use_font   zaehlt font.list[font_index]->used hoch,		*/
/*	unuse_font zaehlt font.list[font_index]->used herunter.		*/
/*	unuse_font ist gegen NULL als Argument und font->used == 0	*/
/*	unempfindlich.							*/
/*									*/
/*	ACHTUNG * WICHTIG * ACHTUNG * WICHTIG * ACHTUNG * WICHTIG	*/
/*									*/
/*	Soll ein Zeichensatz in einem Knoten bzw. einer Kante verwendet	*/
/*	werden, muss er unbedingt mit use_font an- und unuse_font ab-	*/
/*	gemeldet werden, da sonst eine versehentliche Loeschung und	*/
/*	damit Zugriff ueber einen "dangling pointer" im Knoten		*/
/*	mit all seinen Konsequenzen ("core dumped") moeglich ist !	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	get_font_index (font)					*/
/*									*/
/*	Bestimmt den Index des angegebenen Zeichensatzes.		*/
/*	DER INDEX KANN NACH INSERT_FONT BZW. DELETE_FONT UNGUELTIG	*/
/*	WERDEN.								*/
/*	Wenn nicht gefunden, Rueckgabe -1.				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	find_font (filename, font_id)				*/
/*									*/
/*	Sucht Font mit dem filename und font_id.			*/
/*	Rueckgabe Index oder -1, falls nicht gefunden.			*/
/*									*/
/************************************************************************/



Graphed_font	use_font (font_index)
int		font_index;
{
	fonts.list[font_index]->used ++;
	return fonts.list[font_index];
}


void		unuse_font (font)
Graphed_font	font;
{
	if ((font != (Graphed_font)NULL) && (font->used > 0))
		font->used --;
}


int		get_font_index (font)
Graphed_font	font;
{
	int	i;
	
	for (i = 0; font != fonts.list[i] && i < fonts.number_of_fonts; i++);
	if (i == fonts.number_of_fonts)
		return -1;
	else
		return i;
}


int	find_font (filename, font_id)
char	*filename;
char	*font_id;
{
	int	i;
	
	for (i=0; i < fonts.number_of_fonts; i++)
	    if (!strcmp (fonts.list[i]->filename, filename) &&
	        !strcmp (fonts.list[i]->id,       font_id) )
		return i;

	return -1;
}
/************************************************************************/
/*									*/
/*	ZEICHENSAETZE IN DER BENUTZEROBERFLAECHE INSTALLIEREN		*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	install_fontlist_for_cycle ()			*/
/*	void		install_current_nodefont   ()			*/
/*	void		install_current_edgefont   ()			*/
/*									*/
/*	Die install_... - Prozeduren setzen fonts.list_for_cycle bzw.	*/
/*	current_nodefont_index bzw. current_edgefont_index ueber	*/
/*	weitere install_... - Prozeduren aus anderen Modulen an den	*/
/*	benoetigten Stellen der graphischen Oberflaeche ein.		*/
/*									*/
/************************************************************************/



static	void	install_fontlist_for_cycle ()
{	
	install_fontlist_in_nodefont_selection (fonts.list_for_cycle);
	install_fontlist_in_edgefont_selection (fonts.list_for_cycle);
	install_fontlist_in_node_subframe  (fonts.list_for_cycle);
	install_fontlist_in_edge_subframe  (fonts.list_for_cycle);
	install_fontlist_in_group_subframe (fonts.list_for_cycle);
	install_fontlist_in_node_defaults_subframe (fonts.list_for_cycle);
	install_fontlist_in_edge_defaults_subframe (fonts.list_for_cycle);
}


void	install_current_nodefont ()
{	
	install_current_nodefont_in_nodefont_selection     ();
	install_current_nodefont_in_node_subframe          ();
}


void	install_current_edgefont ()
{	
	install_current_edgefont_in_edgefont_selection     ();
	install_current_edgefont_in_edge_subframe          ();
}
/************************************************************************/
/*									*/
/*	ZEICHENSAETZE INITIALISIEREN, KONFIGURATION ABSPEICHERN		*/
/*									*/
/************************************************************************/
/*									*/
/*	void		init_fonts ()					*/
/*									*/
/*	Initialisiert die Struktur fonts.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void		write_fonts (file)				*/
/*									*/
/*	Schreibt die Liste der Zeichensaetze auf file. Die Datei muss	*/
/*	offen sein und wird nicht geschlossen.				*/
/*	Das Format ist (natuerlich) voll kompatibel zu dem		*/
/*	entsprechenden Teil der Grammatik in parser.y.			*/
/*									*/
/************************************************************************/



void	init_fonts()
{
	fonts.number_of_fonts = 0;
	fonts.list_for_cycle [0] = (char *)PANEL_CHOICE_STRINGS;
	fonts.list_for_cycle [1] = (char *)0;
	fonts.list_for_cycle [2] = (char *)PANEL_CHOICE_FONTS;
	fonts.list_for_cycle [3] = (char *)0;
	fonts.list_for_cycle [4] = (char *)0;
}



void	write_fonts (file)
FILE	*file;
{
	int	i;
	
	for (i=0; i < fonts.number_of_fonts; i++) {
		write_quoted_text (file, fonts.list[i]->filename);
		fprintf (file, "\t");
		write_quoted_text (file, fonts.list[i]->id);
		fprintf (file, "\n");
	}
}
